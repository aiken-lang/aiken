use std::{collections::HashMap, ops::Deref, sync::Arc, vec};

use indexmap::IndexMap;
use itertools::Itertools;
use uplc::{
    ast::{
        builder::{
            self, constr_index_exposer, delayed_if_else, if_else, CONSTR_FIELDS_EXPOSER,
            CONSTR_GET_FIELD,
        },
        Constant as UplcConstant, Name, Program, Term, Type as UplcType,
    },
    builtins::DefaultFunction,
    parser::interner::Interner,
};

use crate::{
    air::Air,
    ast::{
        ArgName, AssignmentKind, BinOp, Clause, Pattern, Span, TypedArg, TypedDataType,
        TypedFunction,
    },
    builder::{
        check_when_pattern_needs, constants_ir, convert_constants_to_data, convert_data_to_type,
        convert_type_to_data, get_common_ancestor, get_generics_and_type, get_variant_name,
        list_access_to_uplc, match_ir_for_recursion, monomorphize, rearrange_clauses,
        ClauseProperties, DataTypeKey, FuncComponents, FunctionAccessKey,
    },
    expr::TypedExpr,
    tipo::{self, PatternConstructor, Type, TypeInfo, ValueConstructor, ValueConstructorVariant},
    IdGenerator,
};

pub struct CodeGenerator<'a> {
    defined_functions: HashMap<FunctionAccessKey, ()>,
    functions: &'a HashMap<FunctionAccessKey, &'a TypedFunction>,
    // type_aliases: &'a HashMap<(String, String), &'a TypeAlias<Arc<tipo::Type>>>,
    data_types: &'a HashMap<DataTypeKey, &'a TypedDataType>,
    // imports: &'a HashMap<(String, String), &'a Use<String>>,
    // constants: &'a HashMap<(String, String), &'a ModuleConstant<Arc<tipo::Type>, String>>,
    module_types: &'a HashMap<String, TypeInfo>,
    id_gen: IdGenerator,
    needs_field_access: bool,
}

impl<'a> CodeGenerator<'a> {
    pub fn new(
        functions: &'a HashMap<FunctionAccessKey, &'a TypedFunction>,
        // type_aliases: &'a HashMap<(String, String), &'a TypeAlias<Arc<tipo::Type>>>,
        data_types: &'a HashMap<DataTypeKey, &'a TypedDataType>,
        // imports: &'a HashMap<(String, String), &'a Use<String>>,
        // constants: &'a HashMap<(String, String), &'a ModuleConstant<Arc<tipo::Type>, String>>,
        module_types: &'a HashMap<String, TypeInfo>,
    ) -> Self {
        CodeGenerator {
            defined_functions: HashMap::new(),
            functions,
            // type_aliases,
            data_types,
            // imports,
            // constants,
            module_types,
            id_gen: IdGenerator::new(),
            needs_field_access: false,
        }
    }

    pub fn generate(
        &mut self,
        body: TypedExpr,
        arguments: Vec<TypedArg>,
        wrap_as_validator: bool,
    ) -> Program<Name> {
        let mut ir_stack = vec![];
        let scope = vec![self.id_gen.next()];

        self.build_ir(&body, &mut ir_stack, scope);

        self.define_ir(&mut ir_stack);

        let mut term = self.uplc_code_gen(&mut ir_stack);

        if self.needs_field_access {
            term = builder::constr_get_field(term);

            term = builder::constr_fields_exposer(term);
        }

        // Wrap the validator body if ifThenElse term unit error
        term = if wrap_as_validator {
            builder::final_wrapper(term)
        } else {
            term
        };

        for arg in arguments.iter().rev() {
            term = Term::Lambda {
                parameter_name: uplc::ast::Name {
                    text: arg.arg_name.get_variable_name().unwrap_or("_").to_string(),
                    unique: 0.into(),
                },
                body: term.into(),
            }
        }

        let mut program = Program {
            version: (1, 0, 0),
            term,
        };

        let mut interner = Interner::new();

        interner.program(&mut program);

        program
    }

    pub(crate) fn build_ir(&mut self, body: &TypedExpr, ir_stack: &mut Vec<Air>, scope: Vec<u64>) {
        match body {
            TypedExpr::Int { value, .. } => ir_stack.push(Air::Int {
                scope,
                value: value.to_string(),
            }),
            TypedExpr::String { value, .. } => ir_stack.push(Air::String {
                scope,
                value: value.to_string(),
            }),
            TypedExpr::ByteArray { bytes, .. } => ir_stack.push(Air::ByteArray {
                scope,
                bytes: bytes.to_vec(),
            }),
            TypedExpr::Sequence { expressions, .. } => {
                for expr in expressions {
                    let mut scope = scope.clone();
                    scope.push(self.id_gen.next());
                    self.build_ir(expr, ir_stack, scope);
                }
            }
            TypedExpr::Pipeline { expressions, .. } => {
                for expr in expressions {
                    let mut scope = scope.clone();
                    scope.push(self.id_gen.next());
                    self.build_ir(expr, ir_stack, scope);
                }
            }
            TypedExpr::Var {
                constructor, name, ..
            } => match &constructor.variant {
                ValueConstructorVariant::ModuleConstant { literal, .. } => {
                    constants_ir(literal, ir_stack, scope);
                }
                ValueConstructorVariant::ModuleFn {
                    builtin: Some(builtin),
                    ..
                } => {
                    ir_stack.push(Air::Builtin {
                        scope,
                        func: *builtin,
                        tipo: constructor.tipo.clone(),
                    });
                }
                _ => {
                    ir_stack.push(Air::Var {
                        scope,
                        constructor: constructor.clone(),
                        name: name.clone(),
                        variant_name: String::new(),
                    });
                }
            },
            TypedExpr::Fn { args, body, .. } => {
                let mut func_body = vec![];
                let mut func_scope = scope.clone();
                func_scope.push(self.id_gen.next());
                self.build_ir(body, &mut func_body, func_scope);
                let mut arg_names = vec![];
                for arg in args {
                    let name = arg
                        .arg_name
                        .get_variable_name()
                        .unwrap_or_default()
                        .to_string();
                    arg_names.push(name);
                }

                ir_stack.push(Air::Fn {
                    scope,
                    params: arg_names,
                });
                ir_stack.append(&mut func_body);
            }
            TypedExpr::List {
                elements,
                tail,
                tipo,
                ..
            } => {
                ir_stack.push(Air::List {
                    scope: scope.clone(),
                    count: elements.len(),
                    tipo: tipo.clone(),
                    tail: tail.is_some(),
                });

                for element in elements {
                    let mut scope = scope.clone();
                    scope.push(self.id_gen.next());
                    self.build_ir(element, ir_stack, scope.clone())
                }

                if let Some(tail) = tail {
                    let mut scope = scope;
                    scope.push(self.id_gen.next());

                    self.build_ir(tail, ir_stack, scope);
                }
            }
            TypedExpr::Call { fun, args, .. } => {
                ir_stack.push(Air::Call {
                    scope: scope.clone(),
                    count: args.len(),
                });
                let mut scope_fun = scope.clone();
                scope_fun.push(self.id_gen.next());
                self.build_ir(fun, ir_stack, scope_fun);

                for arg in args {
                    let mut scope = scope.clone();
                    scope.push(self.id_gen.next());
                    self.build_ir(&arg.value, ir_stack, scope);
                }
            }
            TypedExpr::BinOp {
                name, left, right, ..
            } => {
                ir_stack.push(Air::BinOp {
                    scope: scope.clone(),
                    name: *name,
                    count: 2,
                    tipo: left.tipo(),
                });
                let mut scope_left = scope.clone();
                scope_left.push(self.id_gen.next());

                let mut scope_right = scope;
                scope_right.push(self.id_gen.next());

                self.build_ir(left, ir_stack, scope_left);
                self.build_ir(right, ir_stack, scope_right);
            }
            TypedExpr::Assignment {
                value,
                pattern,
                kind,
                tipo,
                ..
            } => {
                let mut define_vec: Vec<Air> = vec![];
                let mut value_vec: Vec<Air> = vec![];
                let mut pattern_vec: Vec<Air> = vec![];

                let mut value_scope = scope.clone();
                value_scope.push(self.id_gen.next());

                self.build_ir(value, &mut value_vec, value_scope);

                self.assignment_ir(
                    pattern,
                    &mut pattern_vec,
                    &mut value_vec,
                    tipo,
                    *kind,
                    scope,
                );

                ir_stack.append(&mut define_vec);
                ir_stack.append(&mut pattern_vec);
            }
            TypedExpr::Trace { .. } => todo!(),
            TypedExpr::When {
                subjects, clauses, ..
            } => {
                let subject_name = format!("__subject_name_{}", self.id_gen.next());
                let constr_var = format!("__constr_name_{}", self.id_gen.next());

                // assuming one subject at the moment
                let subject = subjects[0].clone();

                let clauses = if matches!(clauses[0].pattern[0], Pattern::List { .. }) {
                    rearrange_clauses(clauses.clone())
                } else {
                    clauses.clone()
                };

                if let Some((last_clause, clauses)) = clauses.split_last() {
                    let mut pattern_vec = vec![];

                    let mut clause_properties = ClauseProperties {
                        clause_var_name: constr_var.clone(),
                        needs_constr_var: false,
                        is_complex_clause: false,
                        current_index: 0,
                        original_subject_name: subject_name.clone(),
                    };

                    self.handle_each_clause(
                        &mut pattern_vec,
                        &mut clause_properties,
                        clauses,
                        &subject.tipo(),
                        scope.clone(),
                    );

                    let last_pattern = &last_clause.pattern[0];

                    let mut final_scope = scope.clone();
                    final_scope.push(self.id_gen.next());
                    pattern_vec.push(Air::Finally {
                        scope: final_scope.clone(),
                    });

                    let mut final_clause_vec = vec![];

                    self.build_ir(
                        &last_clause.then,
                        &mut final_clause_vec,
                        final_scope.clone(),
                    );

                    self.when_ir(
                        last_pattern,
                        &mut pattern_vec,
                        &mut final_clause_vec,
                        &subject.tipo(),
                        &mut clause_properties,
                        final_scope,
                    );

                    if clause_properties.needs_constr_var {
                        ir_stack.push(Air::Lam {
                            scope: scope.clone(),
                            name: constr_var.clone(),
                        });

                        self.build_ir(&subject, ir_stack, scope.clone());

                        ir_stack.push(Air::When {
                            scope: scope.clone(),
                            subject_name,
                            tipo: subject.tipo(),
                        });

                        let mut scope = scope;
                        scope.push(self.id_gen.next());

                        ir_stack.push(Air::Var {
                            scope,
                            constructor: ValueConstructor::public(
                                subject.tipo(),
                                ValueConstructorVariant::LocalVariable {
                                    location: Span::empty(),
                                },
                            ),
                            name: constr_var,
                            variant_name: String::new(),
                        })
                    } else {
                        ir_stack.push(Air::When {
                            scope: scope.clone(),
                            subject_name,
                            tipo: subject.tipo(),
                        });

                        let mut scope = scope;
                        scope.push(self.id_gen.next());

                        self.build_ir(&subject, ir_stack, scope);
                    }

                    ir_stack.append(&mut pattern_vec);
                };
            }
            TypedExpr::If {
                branches,
                final_else,
                ..
            } => {
                let mut if_ir = vec![];

                for (index, branch) in branches.iter().enumerate() {
                    let mut branch_scope = scope.clone();
                    branch_scope.push(self.id_gen.next());

                    if index == 0 {
                        if_ir.push(Air::If {
                            scope: scope.clone(),
                        });
                    } else {
                        if_ir.push(Air::If {
                            scope: branch_scope.clone(),
                        });
                    }
                    self.build_ir(&branch.condition, &mut if_ir, branch_scope.clone());
                    self.build_ir(&branch.body, &mut if_ir, branch_scope);
                }

                let mut branch_scope = scope;
                branch_scope.push(self.id_gen.next());

                self.build_ir(final_else, &mut if_ir, branch_scope);

                ir_stack.append(&mut if_ir);
            }
            TypedExpr::RecordAccess {
                record,
                index,
                tipo,
                ..
            } => {
                self.needs_field_access = true;

                ir_stack.push(Air::RecordAccess {
                    scope: scope.clone(),
                    index: *index,
                    tipo: tipo.clone(),
                });

                self.build_ir(record, ir_stack, scope);
            }
            TypedExpr::ModuleSelect {
                constructor,
                module_name,
                tipo,
                ..
            } => match constructor {
                tipo::ModuleValueConstructor::Record { .. } => todo!(),
                tipo::ModuleValueConstructor::Fn { name, module, .. } => {
                    let func = self.functions.get(&FunctionAccessKey {
                        module_name: module_name.clone(),
                        function_name: name.clone(),
                        variant_name: String::new(),
                    });

                    if let Some(func) = func {
                        ir_stack.push(Air::Var {
                            scope,
                            constructor: ValueConstructor::public(
                                func.return_type.clone(),
                                ValueConstructorVariant::ModuleFn {
                                    name: name.clone(),
                                    field_map: None,
                                    module: module.clone(),
                                    arity: func.arguments.len(),
                                    location: Span::empty(),
                                    builtin: None,
                                },
                            ),
                            name: format!("{module}_{name}"),
                            variant_name: String::new(),
                        });
                    } else {
                        let type_info = self.module_types.get(module_name).unwrap();
                        let value = type_info.values.get(name).unwrap();
                        match &value.variant {
                            ValueConstructorVariant::ModuleFn { builtin, .. } => {
                                let builtin = builtin.unwrap();

                                ir_stack.push(Air::Builtin {
                                    func: builtin,
                                    scope,
                                    tipo: tipo.clone(),
                                });
                            }
                            _ => unreachable!(),
                        }
                    }
                }
                tipo::ModuleValueConstructor::Constant { literal, .. } => {
                    constants_ir(literal, ir_stack, scope);
                }
            },
            TypedExpr::Todo { label, tipo, .. } => {
                ir_stack.push(Air::Todo {
                    scope,
                    label: label.clone(),
                    tipo: tipo.clone(),
                });
            }
            TypedExpr::RecordUpdate { .. } => todo!(),
            TypedExpr::Negate { value, .. } => {
                ir_stack.push(Air::Negate {
                    scope: scope.clone(),
                });

                self.build_ir(value, ir_stack, scope);
            }
            TypedExpr::Tuple { elems, tipo, .. } => {
                ir_stack.push(Air::Tuple {
                    scope: scope.clone(),
                    tipo: tipo.clone(),
                    count: elems.len(),
                });

                let mut elems_air = vec![];

                for elem in elems {
                    let mut scope = scope.clone();
                    scope.push(self.id_gen.next());
                    self.build_ir(elem, &mut elems_air, scope);
                }

                ir_stack.append(&mut elems_air);
            }
        }
    }

    fn handle_each_clause(
        &mut self,
        ir_stack: &mut Vec<Air>,
        clause_properties: &mut ClauseProperties,
        clauses: &[Clause<TypedExpr, PatternConstructor, Arc<Type>, String>],
        subject_type: &Arc<Type>,
        scope: Vec<u64>,
    ) {
        for (index, clause) in clauses.iter().enumerate() {
            // scope per clause is different
            let mut scope = scope.clone();
            scope.push(self.id_gen.next());

            // holds when clause pattern Air
            let mut clause_subject_vec = vec![];
            let mut clauses_vec = vec![];

            // reset complex clause setting per clause back to default
            clause_properties.is_complex_clause = false;

            self.build_ir(&clause.then, &mut clauses_vec, scope.clone());

            self.when_ir(
                &clause.pattern[0],
                &mut clause_subject_vec,
                &mut clauses_vec,
                subject_type,
                clause_properties,
                scope.clone(),
            );

            let subject_name = if clause_properties.current_index == 0 {
                clause_properties.original_subject_name.clone()
            } else {
                format!("__tail_{}", clause_properties.current_index - 1)
            };

            // Clause is last in Air pattern vec
            if subject_type.is_list() {
                let next_tail = if index == clauses.len() - 1 {
                    None
                } else {
                    Some(format!("__tail_{}", clause_properties.current_index))
                };

                ir_stack.push(Air::ListClause {
                    scope,
                    tipo: subject_type.clone(),
                    tail_name: subject_name,
                    complex_clause: clause_properties.is_complex_clause,
                    next_tail_name: next_tail,
                });

                clause_properties.current_index += 1;
            } else {
                ir_stack.push(Air::Clause {
                    scope,
                    tipo: subject_type.clone(),
                    complex_clause: clause_properties.is_complex_clause,
                    subject_name,
                });
            }
            ir_stack.append(&mut clause_subject_vec);
        }
    }

    fn when_ir(
        &mut self,
        pattern: &Pattern<tipo::PatternConstructor, Arc<tipo::Type>>,
        pattern_vec: &mut Vec<Air>,
        values: &mut Vec<Air>,
        tipo: &Type,
        clause_properties: &mut ClauseProperties,
        scope: Vec<u64>,
    ) {
        match pattern {
            Pattern::Int { value, .. } => {
                pattern_vec.push(Air::Int {
                    scope,
                    value: value.clone(),
                });

                pattern_vec.append(values);
            }
            Pattern::String { .. } => todo!(),
            Pattern::Var { name, .. } => {
                pattern_vec.push(Air::Discard {
                    scope: scope.clone(),
                });
                pattern_vec.push(Air::Lam {
                    scope: scope.clone(),
                    name: name.clone(),
                });

                pattern_vec.push(Air::Var {
                    scope,
                    constructor: ValueConstructor::public(
                        tipo.clone().into(),
                        ValueConstructorVariant::LocalVariable {
                            location: Span::empty(),
                        },
                    ),
                    name: clause_properties.original_subject_name.clone(),
                    variant_name: String::new(),
                });
                pattern_vec.append(values);
            }
            Pattern::VarUsage { .. } => todo!(),
            Pattern::Assign { name, pattern, .. } => {
                let mut new_vec = vec![];
                new_vec.push(Air::Lam {
                    scope: scope.clone(),
                    name: name.clone(),
                });
                new_vec.push(Air::Var {
                    scope: scope.clone(),
                    constructor: ValueConstructor::public(
                        tipo.clone().into(),
                        ValueConstructorVariant::LocalVariable {
                            location: Span::empty(),
                        },
                    ),
                    name: clause_properties.original_subject_name.clone(),
                    variant_name: String::new(),
                });

                new_vec.append(values);

                // pattern_vec.push(value)
                self.when_ir(
                    pattern,
                    pattern_vec,
                    &mut new_vec,
                    tipo,
                    clause_properties,
                    scope,
                );
            }
            Pattern::Discard { .. } => {
                pattern_vec.push(Air::Discard { scope });
                pattern_vec.append(values);
            }
            Pattern::List { elements, tail, .. } => {
                let mut needs_clause_guard = false;

                for element in elements {
                    check_when_pattern_needs(element, &mut false, &mut needs_clause_guard);
                }

                if let Some(tail) = tail {
                    check_when_pattern_needs(tail, &mut false, &mut needs_clause_guard);
                }

                if needs_clause_guard {
                    clause_properties.is_complex_clause = true;
                }

                pattern_vec.push(Air::Discard {
                    scope: scope.clone(),
                });

                self.when_recursive_ir(
                    pattern,
                    pattern_vec,
                    &mut vec![],
                    clause_properties.clone(),
                    tipo,
                    scope,
                );

                pattern_vec.append(values);
            }
            Pattern::Constructor {
                arguments,
                name: constr_name,
                ..
            } => {
                for arg in arguments {
                    check_when_pattern_needs(
                        &arg.value,
                        &mut clause_properties.needs_constr_var,
                        &mut clause_properties.is_complex_clause,
                    );
                }

                let data_type_key = match tipo {
                    Type::Fn { ret, .. } => match ret.as_ref() {
                        Type::App { module, name, .. } => DataTypeKey {
                            module_name: module.clone(),
                            defined_type: name.clone(),
                        },
                        _ => unreachable!(),
                    },
                    Type::App { module, name, .. } => DataTypeKey {
                        module_name: module.clone(),
                        defined_type: name.clone(),
                    },
                    _ => unreachable!(),
                };

                let data_type = self.data_types.get(&data_type_key).unwrap();

                let (index, _) = data_type
                    .constructors
                    .iter()
                    .enumerate()
                    .find(|(_, dt)| &dt.name == constr_name)
                    .unwrap();

                let mut new_vec = vec![Air::Var {
                    constructor: ValueConstructor::public(
                        tipo.clone().into(),
                        ValueConstructorVariant::LocalVariable {
                            location: Span::empty(),
                        },
                    ),
                    name: clause_properties.clause_var_name.clone(),
                    scope: scope.clone(),
                    variant_name: String::new(),
                }];

                // if only one constructor, no need to check
                if data_type.constructors.len() > 1 {
                    // push constructor Index
                    pattern_vec.push(Air::Int {
                        value: index.to_string(),
                        scope: scope.clone(),
                    });
                }

                if clause_properties.needs_constr_var {
                    self.when_recursive_ir(
                        pattern,
                        pattern_vec,
                        &mut new_vec,
                        clause_properties.clone(),
                        tipo,
                        scope,
                    );
                    pattern_vec.append(values);
                } else {
                    self.when_recursive_ir(
                        pattern,
                        pattern_vec,
                        &mut vec![],
                        clause_properties.clone(),
                        tipo,
                        scope,
                    );
                    pattern_vec.append(values);
                }
            }
            Pattern::Tuple { .. } => todo!(),
        }
    }

    fn when_recursive_ir(
        &mut self,
        pattern: &Pattern<tipo::PatternConstructor, Arc<tipo::Type>>,
        pattern_vec: &mut Vec<Air>,
        values: &mut Vec<Air>,
        clause_properties: ClauseProperties,
        tipo: &Type,
        scope: Vec<u64>,
    ) {
        match pattern {
            Pattern::Int { .. } => todo!(),
            Pattern::String { .. } => todo!(),
            Pattern::Var { .. } => todo!(),
            Pattern::VarUsage { .. } => todo!(),
            Pattern::Assign { .. } => todo!(),
            Pattern::Discard { .. } => {
                pattern_vec.push(Air::Discard { scope });

                pattern_vec.append(values);
            }
            Pattern::List { elements, tail, .. } => {
                // let mut elements_vec = vec![];

                let mut names = vec![];
                for element in elements {
                    match element {
                        Pattern::Var { name, .. } => {
                            names.push(name.clone());
                        }
                        Pattern::Discard { .. } => {
                            names.push("_".to_string());
                        }
                        Pattern::List { .. } => {
                            todo!("Nested List Patterns Not Yet Done");
                            // let mut var_vec = vec![];
                            // let item_name = format!("list_item_id_{}", self.id_gen.next());
                            // names.push(item_name.clone());
                            // var_vec.push(Air::Var {
                            //     constructor: ValueConstructor::public(
                            //         Type::App {
                            //             public: true,
                            //             module: String::new(),
                            //             name: String::new(),
                            //             args: vec![],
                            //         }
                            //         .into(),
                            //         ValueConstructorVariant::LocalVariable {
                            //             location: Span::empty(),
                            //         },
                            //     ),
                            //     name: item_name,
                            //     scope: scope.clone(),
                            // });
                            // self.pattern_ir(a, &mut elements_vec, &mut var_vec, scope.clone());
                        }
                        _ => todo!(),
                    }
                }

                let mut tail_name = String::new();

                if let Some(tail) = tail {
                    match &**tail {
                        Pattern::Var { name, .. } => {
                            tail_name = name.clone();
                        }
                        Pattern::Discard { .. } => {}
                        _ => todo!(),
                    }
                }

                let tail_head_names = names
                    .iter()
                    .enumerate()
                    .filter(|(_, name)| *name != &"_".to_string())
                    .map(|(index, name)| {
                        if index == 0 {
                            (
                                clause_properties.original_subject_name.clone(),
                                name.clone(),
                            )
                        } else {
                            (format!("__tail_{}", index - 1), name.clone())
                        }
                    })
                    .collect_vec();

                if tail.is_some() && !elements.is_empty() {
                    let tail_var = if elements.len() == 1 {
                        clause_properties.original_subject_name
                    } else {
                        format!("__tail_{}", elements.len() - 2)
                    };

                    pattern_vec.push(Air::ListExpose {
                        scope,
                        tipo: tipo.clone().into(),
                        tail_head_names,
                        tail: Some((tail_var, tail_name)),
                    });
                } else {
                    pattern_vec.push(Air::ListExpose {
                        scope,
                        tipo: tipo.clone().into(),
                        tail_head_names,
                        tail: None,
                    });
                }

                pattern_vec.append(values);
            }
            Pattern::Constructor {
                is_record,
                name: constr_name,
                arguments,
                constructor,
                tipo,
                ..
            } => {
                let data_type_key = match tipo.as_ref() {
                    Type::Fn { ret, .. } => match &**ret {
                        Type::App { module, name, .. } => DataTypeKey {
                            module_name: module.clone(),
                            defined_type: name.clone(),
                        },
                        _ => unreachable!(),
                    },
                    Type::App { module, name, .. } => DataTypeKey {
                        module_name: module.clone(),
                        defined_type: name.clone(),
                    },
                    _ => unreachable!(),
                };

                let data_type = self.data_types.get(&data_type_key).unwrap();
                let (_, constructor_type) = data_type
                    .constructors
                    .iter()
                    .enumerate()
                    .find(|(_, dt)| &dt.name == constr_name)
                    .unwrap();
                let mut nested_pattern = vec![];
                if *is_record {
                    let field_map = match constructor {
                        tipo::PatternConstructor::Record { field_map, .. } => {
                            field_map.clone().unwrap()
                        }
                    };

                    let mut type_map: HashMap<String, Arc<Type>> = HashMap::new();

                    for arg in &constructor_type.arguments {
                        let label = arg.label.clone().unwrap();
                        let field_type = arg.tipo.clone();

                        type_map.insert(label, field_type);
                    }

                    let arguments_index = arguments
                        .iter()
                        .map(|item| {
                            let label = item.label.clone().unwrap_or_default();
                            let field_index = field_map.fields.get(&label).unwrap_or(&0);
                            let (discard, var_name) = match &item.value {
                                Pattern::Var { name, .. } => (false, name.clone()),
                                Pattern::Discard { .. } => (true, "".to_string()),
                                Pattern::List { .. } => todo!(),
                                a @ Pattern::Constructor {
                                    tipo,
                                    name: constr_name,
                                    ..
                                } => {
                                    let id = self.id_gen.next();
                                    let constr_var_name = format!("{constr_name}_{id}");
                                    let data_type_key = match tipo.as_ref() {
                                        Type::Fn { ret, .. } => match &**ret {
                                            Type::App { module, name, .. } => DataTypeKey {
                                                module_name: module.clone(),
                                                defined_type: name.clone(),
                                            },
                                            _ => unreachable!(),
                                        },
                                        Type::App { module, name, .. } => DataTypeKey {
                                            module_name: module.clone(),
                                            defined_type: name.clone(),
                                        },
                                        _ => unreachable!(),
                                    };

                                    let data_type = self.data_types.get(&data_type_key).unwrap();

                                    if data_type.constructors.len() > 1 {
                                        nested_pattern.push(Air::ClauseGuard {
                                            scope: scope.clone(),
                                            tipo: tipo.clone(),
                                            subject_name: constr_var_name.clone(),
                                        });
                                    }

                                    let mut clause_complexity = ClauseProperties {
                                        clause_var_name: constr_var_name.clone(),
                                        needs_constr_var: false,
                                        is_complex_clause: false,
                                        current_index: 0,
                                        original_subject_name: constr_var_name.clone(),
                                    };

                                    self.when_ir(
                                        a,
                                        &mut nested_pattern,
                                        &mut vec![],
                                        tipo,
                                        &mut clause_complexity,
                                        scope.clone(),
                                    );

                                    (false, constr_var_name)
                                }
                                _ => todo!(),
                            };

                            (label, var_name, *field_index, discard)
                        })
                        .filter(|(_, _, _, discard)| !discard)
                        .sorted_by(|item1, item2| item1.2.cmp(&item2.2))
                        .collect::<Vec<(String, String, usize, bool)>>();

                    if !arguments_index.is_empty() {
                        pattern_vec.push(Air::FieldsExpose {
                            count: arguments_index.len() + 2,
                            indices: arguments_index
                                .iter()
                                .map(|(label, var_name, index, _)| {
                                    let field_type = type_map.get(label).unwrap();
                                    (*index, var_name.clone(), field_type.clone())
                                })
                                .collect_vec(),
                            scope,
                        });
                    }
                } else {
                    let mut type_map: HashMap<usize, Arc<Type>> = HashMap::new();

                    for (index, arg) in constructor_type.arguments.iter().enumerate() {
                        let field_type = arg.tipo.clone();

                        type_map.insert(index, field_type);
                    }

                    let arguments_index = arguments
                        .iter()
                        .enumerate()
                        .map(|(index, item)| {
                            let (discard, var_name) = match &item.value {
                                Pattern::Var { name, .. } => (false, name.clone()),
                                Pattern::Discard { .. } => (true, "".to_string()),
                                Pattern::List { .. } => todo!(),
                                a @ Pattern::Constructor {
                                    tipo,
                                    name: constr_name,
                                    ..
                                } => {
                                    let id = self.id_gen.next();
                                    let constr_var_name = format!("{constr_name}_{id}");
                                    let data_type_key = match tipo.as_ref() {
                                        Type::Fn { ret, .. } => match &**ret {
                                            Type::App { module, name, .. } => DataTypeKey {
                                                module_name: module.clone(),
                                                defined_type: name.clone(),
                                            },
                                            _ => unreachable!(),
                                        },
                                        Type::App { module, name, .. } => DataTypeKey {
                                            module_name: module.clone(),
                                            defined_type: name.clone(),
                                        },
                                        _ => unreachable!(),
                                    };

                                    let data_type = self.data_types.get(&data_type_key).unwrap();

                                    if data_type.constructors.len() > 1 {
                                        nested_pattern.push(Air::ClauseGuard {
                                            scope: scope.clone(),
                                            tipo: tipo.clone(),
                                            subject_name: constr_var_name.clone(),
                                        });
                                    }
                                    let mut clause_complexity = ClauseProperties {
                                        clause_var_name: constr_var_name.clone(),
                                        needs_constr_var: false,
                                        is_complex_clause: false,
                                        current_index: 0,
                                        original_subject_name: constr_var_name.clone(),
                                    };

                                    self.when_ir(
                                        a,
                                        &mut nested_pattern,
                                        &mut vec![],
                                        tipo,
                                        &mut clause_complexity,
                                        scope.clone(),
                                    );

                                    (false, constr_var_name)
                                }
                                _ => todo!(),
                            };

                            (var_name, index, discard)
                        })
                        .filter(|(_, _, discard)| !discard)
                        .collect::<Vec<(String, usize, bool)>>();

                    if !arguments_index.is_empty() {
                        pattern_vec.push(Air::FieldsExpose {
                            count: arguments_index.len() + 2,
                            indices: arguments_index
                                .iter()
                                .map(|(name, index, _)| {
                                    let field_type = type_map.get(index).unwrap();

                                    (*index, name.clone(), field_type.clone())
                                })
                                .collect_vec(),
                            scope,
                        });
                    }
                }

                pattern_vec.append(values);
                pattern_vec.append(&mut nested_pattern);
            }
            Pattern::Tuple { .. } => todo!(),
        }
    }

    fn assignment_ir(
        &mut self,
        pattern: &Pattern<tipo::PatternConstructor, Arc<Type>>,
        pattern_vec: &mut Vec<Air>,
        value_vec: &mut Vec<Air>,
        tipo: &Type,
        kind: AssignmentKind,
        scope: Vec<u64>,
    ) {
        match pattern {
            Pattern::Int { .. } | Pattern::String { .. } => unreachable!(),
            Pattern::Var { name, .. } => {
                pattern_vec.push(Air::Assignment {
                    name: name.clone(),
                    kind,
                    scope,
                });

                pattern_vec.append(value_vec);
            }
            Pattern::VarUsage { .. } => todo!(),
            Pattern::Assign { .. } => todo!(),
            Pattern::Discard { .. } => {
                self.pattern_ir(pattern, pattern_vec, value_vec, tipo, scope)
            }
            list @ Pattern::List { .. } => {
                self.pattern_ir(list, pattern_vec, value_vec, tipo, scope);
            }
            Pattern::Constructor { .. } => {
                self.pattern_ir(pattern, pattern_vec, value_vec, tipo, scope);
            }
            Pattern::Tuple { .. } => {
                self.pattern_ir(pattern, pattern_vec, value_vec, tipo, scope);
            }
        }
    }

    fn pattern_ir(
        &mut self,
        pattern: &Pattern<tipo::PatternConstructor, Arc<tipo::Type>>,
        pattern_vec: &mut Vec<Air>,
        values: &mut Vec<Air>,
        tipo: &Type,
        scope: Vec<u64>,
    ) {
        match pattern {
            Pattern::Int { .. } => todo!(),
            Pattern::String { .. } => todo!(),
            Pattern::Var { .. } => todo!(),
            Pattern::VarUsage { .. } => todo!(),
            Pattern::Assign { .. } => todo!(),
            Pattern::Discard { .. } => {
                pattern_vec.push(Air::Discard { scope });

                pattern_vec.append(values);
            }
            Pattern::List { elements, tail, .. } => {
                let mut elements_vec = vec![];

                let mut names = vec![];
                for element in elements {
                    match element {
                        Pattern::Var { name, .. } => {
                            names.push(name.clone());
                        }
                        a @ Pattern::List { .. } => {
                            let mut var_vec = vec![];
                            let item_name = format!("list_item_id_{}", self.id_gen.next());
                            names.push(item_name.clone());
                            var_vec.push(Air::Var {
                                constructor: ValueConstructor::public(
                                    Type::App {
                                        public: true,
                                        module: String::new(),
                                        name: String::new(),
                                        args: vec![],
                                    }
                                    .into(),
                                    ValueConstructorVariant::LocalVariable {
                                        location: Span::empty(),
                                    },
                                ),
                                name: item_name,
                                scope: scope.clone(),
                                variant_name: String::new(),
                            });
                            self.pattern_ir(
                                a,
                                &mut elements_vec,
                                &mut var_vec,
                                &tipo.get_inner_types()[0],
                                scope.clone(),
                            );
                        }
                        _ => todo!(),
                    }
                }

                if let Some(tail) = tail {
                    match &**tail {
                        Pattern::Var { name, .. } => names.push(name.clone()),
                        Pattern::Discard { .. } => {}
                        _ => unreachable!(),
                    }
                }

                pattern_vec.push(Air::ListAccessor {
                    names,
                    tail: tail.is_some(),
                    scope,
                    tipo: tipo.clone().into(),
                });

                pattern_vec.append(values);
                pattern_vec.append(&mut elements_vec);
            }
            Pattern::Constructor {
                is_record,
                name: constr_name,
                arguments,
                constructor,
                tipo,
                ..
            } => {
                let data_type_key = match tipo.as_ref() {
                    Type::Fn { ret, .. } => match &**ret {
                        Type::App { module, name, .. } => DataTypeKey {
                            module_name: module.clone(),
                            defined_type: name.clone(),
                        },
                        _ => unreachable!(),
                    },
                    Type::App { module, name, .. } => DataTypeKey {
                        module_name: module.clone(),
                        defined_type: name.clone(),
                    },
                    _ => unreachable!(),
                };

                let data_type = self.data_types.get(&data_type_key).unwrap();
                let (_, constructor_type) = data_type
                    .constructors
                    .iter()
                    .enumerate()
                    .find(|(_, dt)| &dt.name == constr_name)
                    .unwrap();
                let mut nested_pattern = vec![];
                if *is_record {
                    let field_map = match constructor {
                        tipo::PatternConstructor::Record { field_map, .. } => {
                            field_map.clone().unwrap()
                        }
                    };

                    let mut type_map: HashMap<String, Arc<Type>> = HashMap::new();

                    for arg in &constructor_type.arguments {
                        let label = arg.label.clone().unwrap();
                        let field_type = arg.tipo.clone();

                        type_map.insert(label, field_type);
                    }

                    let arguments_index = arguments
                        .iter()
                        .map(|item| {
                            let label = item.label.clone().unwrap_or_default();
                            let field_index = field_map.fields.get(&label).unwrap_or(&0);
                            let (discard, var_name) = match &item.value {
                                Pattern::Var { name, .. } => (false, name.clone()),
                                Pattern::Discard { .. } => (true, "".to_string()),
                                Pattern::List { .. } => todo!(),
                                a @ Pattern::Constructor {
                                    tipo,
                                    name: constr_name,
                                    ..
                                } => {
                                    let id = self.id_gen.next();
                                    let constr_name = format!("{constr_name}_{id}");
                                    self.pattern_ir(
                                        a,
                                        &mut nested_pattern,
                                        &mut vec![Air::Var {
                                            scope: scope.clone(),
                                            constructor: ValueConstructor::public(
                                                tipo.clone(),
                                                ValueConstructorVariant::LocalVariable {
                                                    location: Span::empty(),
                                                },
                                            ),
                                            name: constr_name.clone(),
                                            variant_name: String::new(),
                                        }],
                                        tipo,
                                        scope.clone(),
                                    );

                                    (false, constr_name)
                                }
                                _ => todo!(),
                            };

                            (label, var_name, *field_index, discard)
                        })
                        .filter(|(_, _, _, discard)| !discard)
                        .sorted_by(|item1, item2| item1.2.cmp(&item2.2))
                        .collect::<Vec<(String, String, usize, bool)>>();

                    if !arguments_index.is_empty() {
                        pattern_vec.push(Air::FieldsExpose {
                            count: arguments_index.len() + 2,
                            indices: arguments_index
                                .iter()
                                .map(|(label, var_name, index, _)| {
                                    let field_type = type_map.get(label).unwrap();
                                    (*index, var_name.clone(), field_type.clone())
                                })
                                .collect_vec(),
                            scope,
                        });
                    }
                } else {
                    let mut type_map: HashMap<usize, Arc<Type>> = HashMap::new();

                    for (index, arg) in constructor_type.arguments.iter().enumerate() {
                        let field_type = arg.tipo.clone();

                        type_map.insert(index, field_type);
                    }

                    let arguments_index = arguments
                        .iter()
                        .enumerate()
                        .map(|(index, item)| {
                            let (discard, var_name) = match &item.value {
                                Pattern::Var { name, .. } => (false, name.clone()),
                                Pattern::Discard { .. } => (true, "".to_string()),
                                Pattern::List { .. } => todo!(),
                                a @ Pattern::Constructor {
                                    tipo,
                                    name: constr_name,
                                    ..
                                } => {
                                    let id = self.id_gen.next();
                                    let constr_name = format!("{constr_name}_{id}");
                                    self.pattern_ir(
                                        a,
                                        &mut nested_pattern,
                                        &mut vec![Air::Var {
                                            scope: scope.clone(),
                                            constructor: ValueConstructor::public(
                                                tipo.clone(),
                                                ValueConstructorVariant::LocalVariable {
                                                    location: Span::empty(),
                                                },
                                            ),
                                            name: constr_name.clone(),
                                            variant_name: String::new(),
                                        }],
                                        tipo,
                                        scope.clone(),
                                    );

                                    (false, constr_name)
                                }
                                _ => todo!(),
                            };

                            (var_name, index, discard)
                        })
                        .filter(|(_, _, discard)| !discard)
                        .collect::<Vec<(String, usize, bool)>>();

                    if !arguments_index.is_empty() {
                        pattern_vec.push(Air::FieldsExpose {
                            count: arguments_index.len() + 2,
                            indices: arguments_index
                                .iter()
                                .map(|(name, index, _)| {
                                    let field_type = type_map.get(index).unwrap();

                                    (*index, name.clone(), field_type.clone())
                                })
                                .collect_vec(),
                            scope,
                        });
                    }
                }

                pattern_vec.append(values);
                pattern_vec.append(&mut nested_pattern);
            }
            Pattern::Tuple { elems, .. } => {
                let mut elements_vec = vec![];

                let mut names = vec![];
                for element in elems {
                    match element {
                        Pattern::Var { name, .. } => {
                            names.push(name.clone());
                        }
                        a @ Pattern::List { .. } => {
                            let mut var_vec = vec![];
                            let item_name = format!("list_item_id_{}", self.id_gen.next());
                            names.push(item_name.clone());
                            var_vec.push(Air::Var {
                                constructor: ValueConstructor::public(
                                    Type::App {
                                        public: true,
                                        module: String::new(),
                                        name: String::new(),
                                        args: vec![],
                                    }
                                    .into(),
                                    ValueConstructorVariant::LocalVariable {
                                        location: Span::empty(),
                                    },
                                ),
                                name: item_name,
                                scope: scope.clone(),
                                variant_name: String::new(),
                            });
                            self.pattern_ir(
                                a,
                                &mut elements_vec,
                                &mut var_vec,
                                &tipo.get_inner_types()[0],
                                scope.clone(),
                            );
                        }
                        _ => todo!(),
                    }
                }
                pattern_vec.push(Air::TupleAccessor {
                    names,
                    scope,
                    tipo: tipo.clone().into(),
                });

                pattern_vec.append(values);
                pattern_vec.append(&mut elements_vec);
            }
        }
    }

    fn define_ir(&mut self, ir_stack: &mut Vec<Air>) {
        let mut func_components = IndexMap::new();
        let mut func_index_map = IndexMap::new();

        let recursion_func_map = IndexMap::new();

        self.define_recurse_ir(
            ir_stack,
            &mut func_components,
            &mut func_index_map,
            recursion_func_map,
        );

        let mut insert_var_vec = vec![];
        let mut final_func_dep_ir = IndexMap::new();
        for func in func_index_map.clone() {
            if self.defined_functions.contains_key(&func.0) {
                continue;
            }
            let mut funt_comp = func_components.get(&func.0).unwrap().clone();
            let func_scope = func_index_map.get(&func.0).unwrap();

            let mut dep_ir = vec![];

            // deal with function dependencies
            while let Some(dependency) = funt_comp.dependencies.pop() {
                if self.defined_functions.contains_key(&dependency)
                    || func_components.get(&dependency).is_none()
                {
                    continue;
                }

                let depend_comp = func_components.get(&dependency).unwrap();

                let dep_scope = func_index_map.get(&dependency).unwrap();

                if get_common_ancestor(dep_scope, func_scope) == func_scope.clone() {
                    funt_comp
                        .dependencies
                        .extend(depend_comp.dependencies.clone());

                    let mut temp_ir = vec![Air::DefineFunc {
                        scope: func_scope.clone(),
                        func_name: dependency.function_name.clone(),
                        module_name: dependency.module_name.clone(),
                        params: depend_comp.args.clone(),
                        recursive: depend_comp.recursive,
                        variant_name: dependency.variant_name.clone(),
                    }];

                    for (index, ir) in depend_comp.ir.iter().enumerate() {
                        match_ir_for_recursion(
                            ir.clone(),
                            &mut insert_var_vec,
                            &FunctionAccessKey {
                                function_name: dependency.function_name.clone(),
                                module_name: dependency.module_name.clone(),
                                variant_name: dependency.variant_name.clone(),
                            },
                            index,
                        );
                    }

                    let mut recursion_ir = depend_comp.ir.clone();
                    for (index, ir) in insert_var_vec.clone() {
                        recursion_ir.insert(index, ir);

                        let current_call = recursion_ir[index - 1].clone();

                        match current_call {
                            Air::Call { scope, count } => {
                                recursion_ir[index - 1] = Air::Call {
                                    scope,
                                    count: count + 1,
                                }
                            }
                            _ => unreachable!(),
                        }
                    }

                    temp_ir.append(&mut recursion_ir);

                    temp_ir.append(&mut dep_ir);

                    dep_ir = temp_ir;
                    self.defined_functions.insert(dependency, ());
                    insert_var_vec = vec![];
                }
            }

            final_func_dep_ir.insert(func.0, dep_ir);
        }

        for (index, ir) in ir_stack.clone().into_iter().enumerate().rev() {
            {
                let temp_func_index_map = func_index_map.clone();
                let to_insert = temp_func_index_map
                    .iter()
                    .filter(|func| {
                        func.1.clone() == ir.scope() && !self.defined_functions.contains_key(func.0)
                    })
                    .collect_vec();

                for (function_access_key, scopes) in to_insert.into_iter() {
                    func_index_map.remove(function_access_key);

                    self.defined_functions
                        .insert(function_access_key.clone(), ());

                    let mut full_func_ir =
                        final_func_dep_ir.get(function_access_key).unwrap().clone();

                    let mut func_comp = func_components.get(function_access_key).unwrap().clone();

                    full_func_ir.push(Air::DefineFunc {
                        scope: scopes.clone(),
                        func_name: function_access_key.function_name.clone(),
                        module_name: function_access_key.module_name.clone(),
                        params: func_comp.args.clone(),
                        recursive: func_comp.recursive,
                        variant_name: function_access_key.variant_name.clone(),
                    });

                    for (index, ir) in func_comp.ir.clone().iter().enumerate() {
                        match_ir_for_recursion(
                            ir.clone(),
                            &mut insert_var_vec,
                            function_access_key,
                            index,
                        );
                    }

                    for (index, ir) in insert_var_vec {
                        func_comp.ir.insert(index, ir);

                        let current_call = func_comp.ir[index - 1].clone();

                        match current_call {
                            Air::Call { scope, count } => {
                                func_comp.ir[index - 1] = Air::Call {
                                    scope,
                                    count: count + 1,
                                }
                            }
                            _ => unreachable!("{current_call:#?}"),
                        }
                    }
                    insert_var_vec = vec![];

                    full_func_ir.extend(func_comp.ir.clone());

                    for ir in full_func_ir.into_iter().rev() {
                        ir_stack.insert(index, ir);
                    }
                }
            }
        }
    }

    fn define_recurse_ir(
        &mut self,
        ir_stack: &mut [Air],
        func_components: &mut IndexMap<FunctionAccessKey, FuncComponents>,
        func_index_map: &mut IndexMap<FunctionAccessKey, Vec<u64>>,
        recursion_func_map: IndexMap<FunctionAccessKey, ()>,
    ) {
        self.process_define_ir(ir_stack, func_components, func_index_map);

        let mut recursion_func_map = recursion_func_map;

        for func_index in func_index_map.clone().iter() {
            let func = func_index.0;

            let function_components = func_components.get(func).unwrap();
            let mut function_ir = function_components.ir.clone();

            for ir in function_ir.clone() {
                if let Air::Var {
                    constructor:
                        ValueConstructor {
                            variant:
                                ValueConstructorVariant::ModuleFn {
                                    name: func_name,
                                    module,
                                    ..
                                },
                            ..
                        },
                    variant_name,
                    ..
                } = ir
                {
                    if recursion_func_map.contains_key(&FunctionAccessKey {
                        module_name: module.clone(),
                        function_name: func_name.clone(),
                        variant_name: variant_name.clone(),
                    }) {
                        return;
                    } else {
                        recursion_func_map.insert(
                            FunctionAccessKey {
                                module_name: module.clone(),
                                function_name: func_name.clone(),
                                variant_name: variant_name.clone(),
                            },
                            (),
                        );
                    }
                }
            }

            let mut inner_func_components = IndexMap::new();

            let mut inner_func_index_map = IndexMap::new();

            self.define_recurse_ir(
                &mut function_ir,
                &mut inner_func_components,
                &mut inner_func_index_map,
                recursion_func_map.clone(),
            );

            //now unify
            for item in inner_func_components {
                if !func_components.contains_key(&item.0) {
                    func_components.insert(item.0, item.1);
                }
            }

            for item in inner_func_index_map {
                if let Some(entry) = func_index_map.get_mut(&item.0) {
                    *entry = get_common_ancestor(entry, &item.1);
                } else {
                    func_index_map.insert(item.0, item.1);
                }
            }
        }
    }

    fn process_define_ir(
        &mut self,
        ir_stack: &mut [Air],
        func_components: &mut IndexMap<FunctionAccessKey, FuncComponents>,
        func_index_map: &mut IndexMap<FunctionAccessKey, Vec<u64>>,
    ) {
        let mut to_be_defined_map: IndexMap<FunctionAccessKey, Vec<u64>> = IndexMap::new();
        for (index, ir) in ir_stack.to_vec().iter().enumerate().rev() {
            match ir {
                Air::Var {
                    scope, constructor, ..
                } => {
                    if let ValueConstructorVariant::ModuleFn {
                        name,
                        module,
                        builtin,
                        ..
                    } = &constructor.variant
                    {
                        if builtin.is_none() {
                            let mut function_key = FunctionAccessKey {
                                module_name: module.clone(),
                                function_name: name.clone(),
                                variant_name: String::new(),
                            };
                            if let Some(scope_prev) = to_be_defined_map.get(&function_key) {
                                let new_scope = get_common_ancestor(scope, scope_prev);

                                to_be_defined_map.insert(function_key, new_scope);
                            } else if func_components.get(&function_key).is_some() {
                                to_be_defined_map.insert(function_key.clone(), scope.to_vec());
                            } else {
                                let function = self.functions.get(&function_key).unwrap();

                                let mut func_ir = vec![];

                                self.build_ir(&function.body, &mut func_ir, scope.to_vec());

                                let (param_types, _) = constructor.tipo.function_types().unwrap();

                                let mut generics_type_map: HashMap<u64, Arc<Type>> = HashMap::new();

                                for (index, arg) in function.arguments.iter().enumerate() {
                                    if arg.tipo.is_generic() {
                                        let mut map = generics_type_map.into_iter().collect_vec();
                                        map.append(&mut get_generics_and_type(
                                            &arg.tipo,
                                            &param_types[index],
                                        ));

                                        generics_type_map = map.into_iter().collect();
                                    }
                                }

                                let (variant_name, mut func_ir) =
                                    monomorphize(func_ir, generics_type_map, &constructor.tipo);

                                function_key = FunctionAccessKey {
                                    module_name: module.clone(),
                                    function_name: function_key.function_name,
                                    variant_name: variant_name.clone(),
                                };

                                to_be_defined_map.insert(function_key.clone(), scope.to_vec());
                                let mut func_calls = vec![];

                                for (index, ir) in func_ir.clone().into_iter().enumerate() {
                                    if let Air::Var {
                                        constructor:
                                            ValueConstructor {
                                                variant:
                                                    ValueConstructorVariant::ModuleFn {
                                                        name: func_name,
                                                        module,
                                                        field_map,
                                                        arity,
                                                        location,
                                                        ..
                                                    },
                                                public,
                                                tipo,
                                            },
                                        scope,
                                        name,
                                        ..
                                    } = ir
                                    {
                                        let current_func = FunctionAccessKey {
                                            module_name: module.clone(),
                                            function_name: func_name.clone(),
                                            variant_name: String::new(),
                                        };

                                        let current_func_as_variant = FunctionAccessKey {
                                            module_name: module.clone(),
                                            function_name: func_name.clone(),
                                            variant_name: variant_name.clone(),
                                        };

                                        let function = self.functions.get(&current_func);
                                        if function_key.clone() == current_func_as_variant {
                                            func_ir[index] = Air::Var {
                                                scope,
                                                constructor: ValueConstructor {
                                                    public,
                                                    variant: ValueConstructorVariant::ModuleFn {
                                                        name: func_name,
                                                        field_map,
                                                        module,
                                                        arity,
                                                        location,
                                                        builtin: None,
                                                    },
                                                    tipo,
                                                },
                                                name,
                                                variant_name: variant_name.clone(),
                                            };
                                            func_calls.push(current_func_as_variant);
                                        } else if let (Some(function), Type::Fn { args, .. }) =
                                            (function, &*tipo)
                                        {
                                            if function
                                                .arguments
                                                .iter()
                                                .any(|arg| arg.tipo.is_generic())
                                            {
                                                let mut new_name = String::new();
                                                for arg in args.iter() {
                                                    get_variant_name(&mut new_name, arg);
                                                }
                                                func_calls.push(FunctionAccessKey {
                                                    module_name: module,
                                                    function_name: func_name,
                                                    variant_name: new_name,
                                                });
                                            } else {
                                                func_calls.push(current_func);
                                            }
                                        } else {
                                            func_calls.push(current_func);
                                        }
                                    }
                                }

                                let mut args = vec![];

                                for arg in function.arguments.iter() {
                                    match &arg.arg_name {
                                        ArgName::Named { name, .. }
                                        | ArgName::NamedLabeled { name, .. } => {
                                            args.push(name.clone());
                                        }
                                        _ => {}
                                    }
                                }
                                let recursive = if let Ok(index) =
                                    func_calls.binary_search(&function_key)
                                {
                                    func_calls.remove(index);
                                    while let Ok(index) = func_calls.binary_search(&function_key) {
                                        func_calls.remove(index);
                                    }
                                    true
                                } else {
                                    false
                                };

                                ir_stack[index] = Air::Var {
                                    scope: scope.clone(),
                                    constructor: constructor.clone(),
                                    name: name.clone(),
                                    variant_name,
                                };

                                func_components.insert(
                                    function_key,
                                    FuncComponents {
                                        ir: func_ir,
                                        dependencies: func_calls,
                                        recursive,
                                        args,
                                    },
                                );
                            }
                        }
                    }
                }
                a => {
                    let scope = a.scope();

                    for func in to_be_defined_map.clone().iter() {
                        if get_common_ancestor(&scope, func.1) == scope.to_vec() {
                            if let Some(index_scope) = func_index_map.get(func.0) {
                                if get_common_ancestor(index_scope, func.1) == scope.to_vec() {
                                    func_index_map.insert(func.0.clone(), scope.clone());
                                    to_be_defined_map.shift_remove(func.0);
                                } else {
                                    to_be_defined_map.insert(
                                        func.0.clone(),
                                        get_common_ancestor(index_scope, func.1),
                                    );
                                }
                            } else {
                                func_index_map.insert(func.0.clone(), scope.clone());
                                to_be_defined_map.shift_remove(func.0);
                            }
                        }
                    }
                }
            }
        }

        //Still to be defined
        for func in to_be_defined_map.clone().iter() {
            let index_scope = func_index_map.get(func.0).unwrap();
            func_index_map.insert(func.0.clone(), get_common_ancestor(func.1, index_scope));
        }
    }

    fn uplc_code_gen(&mut self, ir_stack: &mut Vec<Air>) -> Term<Name> {
        let mut arg_stack: Vec<Term<Name>> = vec![];

        while let Some(ir_element) = ir_stack.pop() {
            self.gen_uplc(ir_element, &mut arg_stack);
        }

        arg_stack[0].clone()
    }

    fn gen_uplc(&mut self, ir: Air, arg_stack: &mut Vec<Term<Name>>) {
        match ir {
            Air::Int { value, .. } => {
                let integer = value.parse().unwrap();

                let term = Term::Constant(UplcConstant::Integer(integer));

                arg_stack.push(term);
            }
            Air::String { value, .. } => {
                let term = Term::Constant(UplcConstant::String(value));

                arg_stack.push(term);
            }
            Air::ByteArray { bytes, .. } => {
                let term = Term::Constant(UplcConstant::ByteString(bytes));
                arg_stack.push(term);
            }
            Air::Var {
                name,
                constructor,
                variant_name,
                ..
            } => {
                match &constructor.variant {
                    ValueConstructorVariant::LocalVariable { .. } => {
                        arg_stack.push(Term::Var(Name {
                            text: name,
                            unique: 0.into(),
                        }))
                    }
                    ValueConstructorVariant::ModuleConstant { .. } => {
                        unreachable!()
                    }
                    ValueConstructorVariant::ModuleFn {
                        name: func_name,
                        module,
                        ..
                    } => {
                        let name = if *func_name == name {
                            format!("{module}_{func_name}{variant_name}")
                        } else {
                            format!("{func_name}{variant_name}")
                        };

                        arg_stack.push(Term::Var(Name {
                            text: name,
                            unique: 0.into(),
                        }));
                    }
                    ValueConstructorVariant::Record {
                        name: constr_name,
                        field_map,
                        arity,
                        ..
                    } => {
                        let data_type_key = match &*constructor.tipo {
                            Type::App { module, name, .. } => DataTypeKey {
                                module_name: module.to_string(),
                                defined_type: name.to_string(),
                            },
                            Type::Fn { ret, .. } => match ret.deref() {
                                Type::App { module, name, .. } => DataTypeKey {
                                    module_name: module.to_string(),
                                    defined_type: name.to_string(),
                                },
                                _ => unreachable!(),
                            },
                            Type::Var { .. } => todo!(),
                            Type::Tuple { .. } => todo!(),
                        };

                        if constructor.tipo.is_bool() {
                            arg_stack
                                .push(Term::Constant(UplcConstant::Bool(constr_name == "True")));
                        } else {
                            let data_type = self.data_types.get(&data_type_key).unwrap();

                            let (constr_index, _) = data_type
                                .constructors
                                .iter()
                                .enumerate()
                                .find(|(_, x)| x.name == *constr_name)
                                .unwrap();

                            let mut fields =
                                Term::Constant(UplcConstant::ProtoList(UplcType::Data, vec![]));

                            let tipo = constructor.tipo;

                            let args_type = match tipo.as_ref() {
                                Type::Fn { args, .. } | Type::App { args, .. } => args,
                                _ => unreachable!(),
                            };

                            if let Some(field_map) = field_map.clone() {
                                for field in field_map
                                    .fields
                                    .iter()
                                    .sorted_by(|item1, item2| item1.1.cmp(item2.1))
                                    .zip(args_type)
                                    .rev()
                                {
                                    // TODO revisit
                                    fields = Term::Apply {
                                        function: Term::Apply {
                                            function: Term::Builtin(DefaultFunction::MkCons)
                                                .force_wrap()
                                                .into(),
                                            argument: convert_type_to_data(
                                                Term::Var(Name {
                                                    text: field.0 .0.clone(),
                                                    unique: 0.into(),
                                                }),
                                                field.1,
                                            )
                                            .into(),
                                        }
                                        .into(),
                                        argument: fields.into(),
                                    };
                                }
                            } else {
                                for (index, arg) in args_type.iter().enumerate().take(*arity) {
                                    fields = Term::Apply {
                                        function: Term::Apply {
                                            function: Term::Builtin(DefaultFunction::MkCons)
                                                .force_wrap()
                                                .into(),
                                            argument: convert_type_to_data(
                                                Term::Var(Name {
                                                    text: format!("__arg_{}", index),
                                                    unique: 0.into(),
                                                }),
                                                arg,
                                            )
                                            .into(),
                                        }
                                        .into(),
                                        argument: fields.into(),
                                    };
                                }
                            }

                            let mut term = Term::Apply {
                                function: Term::Apply {
                                    function: Term::Builtin(DefaultFunction::ConstrData).into(),
                                    argument: Term::Constant(UplcConstant::Integer(
                                        constr_index.try_into().unwrap(),
                                    ))
                                    .into(),
                                }
                                .into(),
                                argument: fields.into(),
                            };

                            if let Some(field_map) = field_map {
                                for field in field_map
                                    .fields
                                    .iter()
                                    .sorted_by(|item1, item2| item1.1.cmp(item2.1))
                                    .rev()
                                {
                                    term = Term::Lambda {
                                        parameter_name: Name {
                                            text: field.0.clone(),
                                            unique: 0.into(),
                                        },
                                        body: term.into(),
                                    };
                                }
                            } else {
                                for (index, _) in args_type.iter().enumerate().take(*arity) {
                                    term = Term::Lambda {
                                        parameter_name: Name {
                                            text: format!("__arg_{}", index),
                                            unique: 0.into(),
                                        },
                                        body: term.into(),
                                    };
                                }
                            }

                            arg_stack.push(term);
                        }
                    }
                };
            }
            Air::Discard { .. } => {
                arg_stack.push(Term::Constant(UplcConstant::Unit));
            }
            Air::List {
                count, tipo, tail, ..
            } => {
                let mut args = vec![];

                for _ in 0..count {
                    let arg = arg_stack.pop().unwrap();
                    args.push(arg);
                }
                let mut constants = vec![];
                for arg in &args {
                    if let Term::Constant(c) = arg {
                        constants.push(c.clone())
                    }
                }

                let list_type = tipo.get_inner_types()[0].clone();

                if constants.len() == args.len() && !tail {
                    let list = if tipo.is_map() {
                        let mut convert_keys = vec![];
                        let mut convert_values = vec![];
                        for constant in constants {
                            match constant {
                                UplcConstant::ProtoPair(_, _, fst, snd) => {
                                    convert_keys.push(*fst);
                                    convert_values.push(*snd);
                                }
                                _ => unreachable!(),
                            }
                        }
                        convert_keys = convert_constants_to_data(convert_keys);
                        convert_values = convert_constants_to_data(convert_values);

                        Term::Constant(UplcConstant::ProtoList(
                            UplcType::Pair(UplcType::Data.into(), UplcType::Data.into()),
                            convert_keys
                                .into_iter()
                                .zip(convert_values.into_iter())
                                .map(|(key, value)| {
                                    UplcConstant::ProtoPair(
                                        UplcType::Data,
                                        UplcType::Data,
                                        key.into(),
                                        value.into(),
                                    )
                                })
                                .collect_vec(),
                        ))
                    } else {
                        Term::Constant(UplcConstant::ProtoList(
                            UplcType::Data,
                            convert_constants_to_data(constants),
                        ))
                    };

                    arg_stack.push(list);
                } else {
                    let mut term = if tail {
                        arg_stack.pop().unwrap()
                    } else if tipo.is_map() {
                        Term::Constant(UplcConstant::ProtoList(
                            UplcType::Pair(UplcType::Data.into(), UplcType::Data.into()),
                            vec![],
                        ))
                    } else {
                        Term::Constant(UplcConstant::ProtoList(UplcType::Data, vec![]))
                    };

                    for arg in args {
                        let list_item = if tipo.is_map() {
                            arg
                        } else {
                            convert_type_to_data(arg, &list_type)
                        };
                        term = Term::Apply {
                            function: Term::Apply {
                                function: Term::Builtin(DefaultFunction::MkCons)
                                    .force_wrap()
                                    .into(),
                                argument: list_item.into(),
                            }
                            .into(),
                            argument: term.into(),
                        };
                    }
                    arg_stack.push(term);
                }
            }
            Air::ListAccessor {
                names, tail, tipo, ..
            } => {
                let value = arg_stack.pop().unwrap();
                let mut term = arg_stack.pop().unwrap();

                let mut id_list = vec![];

                for _ in 0..names.len() {
                    id_list.push(self.id_gen.next());
                }

                let current_index = 0;
                let (first_name, names) = names.split_first().unwrap();

                let list_id = self.id_gen.next();

                let head_list = if tipo.is_map() {
                    Term::Apply {
                        function: Term::Force(Term::Builtin(DefaultFunction::HeadList).into())
                            .into(),
                        argument: Term::Var(Name {
                            text: format!("__list_{}", list_id),
                            unique: 0.into(),
                        })
                        .into(),
                    }
                } else {
                    convert_data_to_type(
                        Term::Apply {
                            function: Term::Force(Term::Builtin(DefaultFunction::HeadList).into())
                                .into(),
                            argument: Term::Var(Name {
                                text: format!("__list_{}", list_id),
                                unique: 0.into(),
                            })
                            .into(),
                        },
                        &tipo.get_inner_types()[0],
                    )
                };

                term = Term::Apply {
                    function: Term::Lambda {
                        parameter_name: Name {
                            text: format!("__list_{}", list_id),
                            unique: 0.into(),
                        },
                        body: Term::Apply {
                            function: Term::Lambda {
                                parameter_name: Name {
                                    text: first_name.clone(),
                                    unique: 0.into(),
                                },
                                body: Term::Apply {
                                    function: list_access_to_uplc(
                                        names,
                                        &id_list,
                                        tail,
                                        current_index,
                                        term,
                                        &tipo,
                                    )
                                    .into(),
                                    argument: Term::Apply {
                                        function: Term::Force(
                                            Term::Builtin(DefaultFunction::TailList).into(),
                                        )
                                        .into(),
                                        argument: Term::Var(Name {
                                            text: format!("__list_{}", list_id),
                                            unique: 0.into(),
                                        })
                                        .into(),
                                    }
                                    .into(),
                                }
                                .into(),
                            }
                            .into(),
                            argument: head_list.into(),
                        }
                        .into(),
                    }
                    .into(),
                    argument: value.into(),
                };

                arg_stack.push(term);
            }
            Air::ListExpose {
                tail_head_names,
                tail,
                tipo,
                ..
            } => {
                let mut term = arg_stack.pop().unwrap();

                if let Some((tail_var, tail_name)) = tail {
                    term = Term::Apply {
                        function: Term::Lambda {
                            parameter_name: Name {
                                text: tail_name,
                                unique: 0.into(),
                            },
                            body: term.into(),
                        }
                        .into(),
                        argument: Term::Apply {
                            function: Term::Builtin(DefaultFunction::TailList).force_wrap().into(),
                            argument: Term::Var(Name {
                                text: tail_var,
                                unique: 0.into(),
                            })
                            .into(),
                        }
                        .into(),
                    };
                }

                for (tail_var, head_name) in tail_head_names.into_iter().rev() {
                    let head_list = if tipo.is_map() {
                        Term::Apply {
                            function: Term::Force(Term::Builtin(DefaultFunction::HeadList).into())
                                .into(),
                            argument: Term::Var(Name {
                                text: tail_var,
                                unique: 0.into(),
                            })
                            .into(),
                        }
                    } else {
                        convert_data_to_type(
                            Term::Apply {
                                function: Term::Force(
                                    Term::Builtin(DefaultFunction::HeadList).into(),
                                )
                                .into(),
                                argument: Term::Var(Name {
                                    text: tail_var,
                                    unique: 0.into(),
                                })
                                .into(),
                            },
                            &tipo.get_inner_types()[0],
                        )
                    };
                    term = Term::Apply {
                        function: Term::Lambda {
                            parameter_name: Name {
                                text: head_name,
                                unique: 0.into(),
                            },
                            body: term.into(),
                        }
                        .into(),
                        argument: head_list.into(),
                    };
                }

                arg_stack.push(term);
            }

            Air::Fn { params, .. } => {
                let mut term = arg_stack.pop().unwrap();

                for param in params.iter().rev() {
                    term = Term::Lambda {
                        parameter_name: Name {
                            text: param.clone(),
                            unique: 0.into(),
                        },
                        body: term.into(),
                    };
                }

                arg_stack.push(term);
            }
            Air::Call { count, .. } => {
                if count >= 1 {
                    let mut term = arg_stack.pop().unwrap();

                    for _ in 0..count {
                        let arg = arg_stack.pop().unwrap();

                        term = Term::Apply {
                            function: term.into(),
                            argument: arg.into(),
                        };
                    }
                    arg_stack.push(term);
                } else {
                    todo!()
                }
            }
            Air::Builtin { func, tipo, .. } => match func {
                DefaultFunction::FstPair | DefaultFunction::SndPair | DefaultFunction::HeadList => {
                    let id = self.id_gen.next();
                    let mut term: Term<Name> = func.into();
                    for _ in 0..func.force_count() {
                        term = term.force_wrap();
                    }

                    term = Term::Apply {
                        function: term.into(),
                        argument: Term::Var(Name {
                            text: format!("__arg_{}", id),
                            unique: 0.into(),
                        })
                        .into(),
                    };

                    let inner_type = if matches!(func, DefaultFunction::SndPair) {
                        tipo.get_inner_types()[0].get_inner_types()[1].clone()
                    } else {
                        tipo.get_inner_types()[0].get_inner_types()[0].clone()
                    };

                    term = convert_data_to_type(term, &inner_type);
                    term = Term::Lambda {
                        parameter_name: Name {
                            text: format!("__arg_{}", id),
                            unique: 0.into(),
                        },
                        body: term.into(),
                    };

                    arg_stack.push(term);
                }
                DefaultFunction::MkCons => todo!(),
                DefaultFunction::MkPairData => todo!(),
                _ => {
                    let mut term = Term::Builtin(func);
                    for _ in 0..func.force_count() {
                        term = term.force_wrap();
                    }
                    arg_stack.push(term);
                }
            },
            Air::BinOp { name, tipo, .. } => {
                let left = arg_stack.pop().unwrap();
                let right = arg_stack.pop().unwrap();

                let default_builtin = if tipo.is_int() {
                    DefaultFunction::EqualsInteger
                } else if tipo.is_string() {
                    DefaultFunction::EqualsString
                } else if tipo.is_bytearray() {
                    DefaultFunction::EqualsByteString
                } else {
                    DefaultFunction::EqualsData
                };

                let term = match name {
                    BinOp::And => {
                        delayed_if_else(left, right, Term::Constant(UplcConstant::Bool(false)))
                    }
                    BinOp::Or => {
                        delayed_if_else(left, Term::Constant(UplcConstant::Bool(true)), right)
                    }

                    BinOp::Eq => {
                        if tipo.is_bool() {
                            let term = delayed_if_else(
                                left,
                                right.clone(),
                                if_else(
                                    right,
                                    Term::Constant(UplcConstant::Bool(false)),
                                    Term::Constant(UplcConstant::Bool(true)),
                                ),
                            );
                            arg_stack.push(term);
                            return;
                        } else if tipo.is_map() {
                            let term = Term::Apply {
                                function: Term::Apply {
                                    function: default_builtin.into(),
                                    argument: Term::Apply {
                                        function: DefaultFunction::MapData.into(),
                                        argument: left.into(),
                                    }
                                    .into(),
                                }
                                .into(),
                                argument: Term::Apply {
                                    function: DefaultFunction::MapData.into(),
                                    argument: right.into(),
                                }
                                .into(),
                            };
                            arg_stack.push(term);
                            return;
                        } else if tipo.is_tuple()
                            && matches!(tipo.clone().get_uplc_type(), UplcType::Pair(_, _))
                        {
                            let term = Term::Apply {
                                function: Term::Apply {
                                    function: default_builtin.into(),
                                    argument: Term::Apply {
                                        function: DefaultFunction::MapData.into(),
                                        argument: Term::Apply {
                                            function: Term::Apply {
                                                function: Term::Builtin(DefaultFunction::MkCons)
                                                    .force_wrap()
                                                    .into(),
                                                argument: left.into(),
                                            }
                                            .into(),
                                            argument: Term::Constant(UplcConstant::ProtoList(
                                                UplcType::Pair(
                                                    UplcType::Data.into(),
                                                    UplcType::Data.into(),
                                                ),
                                                vec![],
                                            ))
                                            .into(),
                                        }
                                        .into(),
                                    }
                                    .into(),
                                }
                                .into(),
                                argument: Term::Apply {
                                    function: DefaultFunction::MapData.into(),
                                    argument: Term::Apply {
                                        function: Term::Apply {
                                            function: Term::Builtin(DefaultFunction::MkCons)
                                                .force_wrap()
                                                .into(),
                                            argument: right.into(),
                                        }
                                        .into(),
                                        argument: Term::Constant(UplcConstant::ProtoList(
                                            UplcType::Pair(
                                                UplcType::Data.into(),
                                                UplcType::Data.into(),
                                            ),
                                            vec![],
                                        ))
                                        .into(),
                                    }
                                    .into(),
                                }
                                .into(),
                            };
                            arg_stack.push(term);
                            return;
                        } else if tipo.is_list() {
                            let term = Term::Apply {
                                function: Term::Apply {
                                    function: default_builtin.into(),
                                    argument: Term::Apply {
                                        function: DefaultFunction::ListData.into(),
                                        argument: left.into(),
                                    }
                                    .into(),
                                }
                                .into(),

                                argument: Term::Apply {
                                    function: DefaultFunction::ListData.into(),
                                    argument: right.into(),
                                }
                                .into(),
                            };
                            arg_stack.push(term);
                            return;
                        }

                        Term::Apply {
                            function: Term::Apply {
                                function: default_builtin.into(),
                                argument: left.into(),
                            }
                            .into(),
                            argument: right.into(),
                        }
                    }
                    BinOp::NotEq => {
                        if tipo.is_bool() {
                            let term = delayed_if_else(
                                left,
                                if_else(
                                    right.clone(),
                                    Term::Constant(UplcConstant::Bool(false)),
                                    Term::Constant(UplcConstant::Bool(true)),
                                ),
                                right,
                            );
                            arg_stack.push(term);
                            return;
                        } else if tipo.is_map() {
                            let term = Term::Apply {
                                function: Term::Apply {
                                    function: Term::Apply {
                                        function: Term::Builtin(DefaultFunction::IfThenElse)
                                            .force_wrap()
                                            .into(),
                                        argument: Term::Apply {
                                            function: Term::Apply {
                                                function: default_builtin.into(),
                                                argument: Term::Apply {
                                                    function: DefaultFunction::MapData.into(),
                                                    argument: left.into(),
                                                }
                                                .into(),
                                            }
                                            .into(),
                                            argument: Term::Apply {
                                                function: DefaultFunction::MapData.into(),
                                                argument: right.into(),
                                            }
                                            .into(),
                                        }
                                        .into(),
                                    }
                                    .into(),
                                    argument: Term::Constant(UplcConstant::Bool(false)).into(),
                                }
                                .into(),
                                argument: Term::Constant(UplcConstant::Bool(true)).into(),
                            };
                            arg_stack.push(term);
                            return;
                        } else if tipo.is_tuple()
                            && matches!(tipo.clone().get_uplc_type(), UplcType::Pair(_, _))
                        {
                            // let term = Term::Apply {
                            //     function: Term::Apply {
                            //         function: default_builtin.into(),
                            //         argument: Term::Apply {
                            //             function: DefaultFunction::MapData.into(),
                            //             argument: Term::Apply {
                            //                 function: Term::Apply {
                            //                     function: Term::Builtin(DefaultFunction::MkCons)
                            //                         .force_wrap()
                            //                         .into(),
                            //                     argument: left.into(),
                            //                 }
                            //                 .into(),
                            //                 argument: Term::Constant(UplcConstant::ProtoList(
                            //                     UplcType::Pair(
                            //                         UplcType::Data.into(),
                            //                         UplcType::Data.into(),
                            //                     ),
                            //                     vec![],
                            //                 ))
                            //                 .into(),
                            //             }
                            //             .into(),
                            //         }
                            //         .into(),
                            //     }
                            //     .into(),
                            //     argument: Term::Apply {
                            //         function: Term::Apply {
                            //             function: Term::Builtin(DefaultFunction::MkCons)
                            //                 .force_wrap()
                            //                 .into(),
                            //             argument: right.into(),
                            //         }
                            //         .into(),
                            //         argument: Term::Constant(UplcConstant::ProtoList(
                            //             UplcType::Pair(
                            //                 UplcType::Data.into(),
                            //                 UplcType::Data.into(),
                            //             ),
                            //             vec![],
                            //         ))
                            //         .into(),
                            //     }
                            //     .into(),
                            // };
                            // arg_stack.push(term);
                            // return;
                            todo!()
                        } else if tipo.is_list()
                            || matches!(tipo.get_uplc_type(), UplcType::List(_))
                        {
                            let term = Term::Apply {
                                function: Term::Apply {
                                    function: Term::Apply {
                                        function: Term::Builtin(DefaultFunction::IfThenElse)
                                            .force_wrap()
                                            .into(),
                                        argument: Term::Apply {
                                            function: Term::Apply {
                                                function: default_builtin.into(),
                                                argument: Term::Apply {
                                                    function: DefaultFunction::ListData.into(),
                                                    argument: left.into(),
                                                }
                                                .into(),
                                            }
                                            .into(),
                                            argument: Term::Apply {
                                                function: default_builtin.into(),
                                                argument: Term::Apply {
                                                    function: DefaultFunction::ListData.into(),
                                                    argument: right.into(),
                                                }
                                                .into(),
                                            }
                                            .into(),
                                        }
                                        .into(),
                                    }
                                    .into(),
                                    argument: Term::Constant(UplcConstant::Bool(false)).into(),
                                }
                                .into(),
                                argument: Term::Constant(UplcConstant::Bool(true)).into(),
                            };

                            arg_stack.push(term);
                            return;
                        }

                        Term::Apply {
                            function: Term::Apply {
                                function: Term::Apply {
                                    function: Term::Builtin(DefaultFunction::IfThenElse)
                                        .force_wrap()
                                        .into(),
                                    argument: Term::Apply {
                                        function: Term::Apply {
                                            function: default_builtin.into(),
                                            argument: left.into(),
                                        }
                                        .into(),
                                        argument: right.into(),
                                    }
                                    .into(),
                                }
                                .into(),
                                argument: Term::Constant(UplcConstant::Bool(false)).into(),
                            }
                            .into(),
                            argument: Term::Constant(UplcConstant::Bool(true)).into(),
                        }
                    }
                    BinOp::LtInt => Term::Apply {
                        function: Term::Apply {
                            function: Term::Builtin(DefaultFunction::LessThanInteger).into(),
                            argument: left.into(),
                        }
                        .into(),
                        argument: right.into(),
                    },
                    BinOp::LtEqInt => Term::Apply {
                        function: Term::Apply {
                            function: Term::Builtin(DefaultFunction::LessThanEqualsInteger).into(),
                            argument: left.into(),
                        }
                        .into(),
                        argument: right.into(),
                    },
                    BinOp::GtEqInt => Term::Apply {
                        function: Term::Apply {
                            function: Term::Builtin(DefaultFunction::LessThanEqualsInteger).into(),
                            argument: right.into(),
                        }
                        .into(),
                        argument: left.into(),
                    },
                    BinOp::GtInt => Term::Apply {
                        function: Term::Apply {
                            function: Term::Builtin(DefaultFunction::LessThanInteger).into(),
                            argument: right.into(),
                        }
                        .into(),
                        argument: left.into(),
                    },
                    BinOp::AddInt => Term::Apply {
                        function: Term::Apply {
                            function: Term::Builtin(DefaultFunction::AddInteger).into(),
                            argument: left.into(),
                        }
                        .into(),
                        argument: right.into(),
                    },
                    BinOp::SubInt => Term::Apply {
                        function: Term::Apply {
                            function: Term::Builtin(DefaultFunction::SubtractInteger).into(),
                            argument: left.into(),
                        }
                        .into(),
                        argument: right.into(),
                    },
                    BinOp::MultInt => Term::Apply {
                        function: Term::Apply {
                            function: Term::Builtin(DefaultFunction::MultiplyInteger).into(),
                            argument: left.into(),
                        }
                        .into(),
                        argument: right.into(),
                    },
                    BinOp::DivInt => Term::Apply {
                        function: Term::Apply {
                            function: Term::Builtin(DefaultFunction::DivideInteger).into(),
                            argument: left.into(),
                        }
                        .into(),
                        argument: right.into(),
                    },
                    BinOp::ModInt => Term::Apply {
                        function: Term::Apply {
                            function: Term::Builtin(DefaultFunction::ModInteger).into(),
                            argument: left.into(),
                        }
                        .into(),
                        argument: right.into(),
                    },
                };
                arg_stack.push(term);
            }
            Air::Assignment { name, .. } => {
                let right_hand = arg_stack.pop().unwrap();
                let lam_body = arg_stack.pop().unwrap();

                let term = Term::Apply {
                    function: Term::Lambda {
                        parameter_name: Name {
                            text: name,
                            unique: 0.into(),
                        },
                        body: lam_body.into(),
                    }
                    .into(),
                    argument: right_hand.into(),
                };

                arg_stack.push(term);
            }
            Air::DefineFunc {
                func_name,
                params,
                recursive,
                module_name,
                variant_name,
                ..
            } => {
                let func_name = if module_name.is_empty() {
                    format!("{func_name}{variant_name}")
                } else {
                    format!("{module_name}_{func_name}{variant_name}")
                };
                let mut func_body = arg_stack.pop().unwrap();

                let mut term = arg_stack.pop().unwrap();

                for param in params.iter().rev() {
                    func_body = Term::Lambda {
                        parameter_name: Name {
                            text: param.clone(),
                            unique: 0.into(),
                        },
                        body: func_body.into(),
                    };
                }

                if !recursive {
                    term = Term::Apply {
                        function: Term::Lambda {
                            parameter_name: Name {
                                text: func_name,
                                unique: 0.into(),
                            },
                            body: term.into(),
                        }
                        .into(),
                        argument: func_body.into(),
                    };
                    arg_stack.push(term);
                } else {
                    func_body = Term::Lambda {
                        parameter_name: Name {
                            text: func_name.clone(),
                            unique: 0.into(),
                        },
                        body: func_body.into(),
                    };

                    let mut boostrap_recurse = Term::Apply {
                        function: Term::Var(Name {
                            text: "__recurse".to_string(),
                            unique: 0.into(),
                        })
                        .into(),
                        argument: Term::Var(Name {
                            text: "__recurse".to_string(),
                            unique: 0.into(),
                        })
                        .into(),
                    };

                    for param in params.iter() {
                        boostrap_recurse = Term::Apply {
                            function: boostrap_recurse.into(),
                            argument: Term::Var(Name {
                                text: param.clone(),
                                unique: 0.into(),
                            })
                            .into(),
                        };
                    }

                    func_body = Term::Apply {
                        function: Term::Lambda {
                            parameter_name: Name {
                                text: "__recurse".to_string(),
                                unique: 0.into(),
                            },
                            body: boostrap_recurse.into(),
                        }
                        .into(),
                        argument: func_body.into(),
                    };

                    for param in params.iter().rev() {
                        func_body = Term::Lambda {
                            parameter_name: Name {
                                text: param.clone(),
                                unique: 0.into(),
                            },
                            body: func_body.into(),
                        };
                    }

                    term = Term::Apply {
                        function: Term::Lambda {
                            parameter_name: Name {
                                text: func_name,
                                unique: 0.into(),
                            },
                            body: term.into(),
                        }
                        .into(),
                        argument: func_body.into(),
                    };

                    arg_stack.push(term);
                }
            }
            Air::DefineConst { .. } => todo!(),
            Air::DefineConstrFields { .. } => todo!(),
            Air::DefineConstrFieldAccess { .. } => todo!(),
            Air::Lam { name, .. } => {
                let arg = arg_stack.pop().unwrap();

                let mut term = arg_stack.pop().unwrap();

                term = Term::Apply {
                    function: Term::Lambda {
                        parameter_name: Name {
                            text: name,
                            unique: 0.into(),
                        },
                        body: term.into(),
                    }
                    .into(),
                    argument: arg.into(),
                };
                arg_stack.push(term);
            }
            Air::When {
                subject_name, tipo, ..
            } => {
                let subject = arg_stack.pop().unwrap();

                let mut term = arg_stack.pop().unwrap();

                term = if tipo.is_int() || tipo.is_bytearray() || tipo.is_string() || tipo.is_list()
                {
                    Term::Apply {
                        function: Term::Lambda {
                            parameter_name: Name {
                                text: subject_name,
                                unique: 0.into(),
                            },
                            body: term.into(),
                        }
                        .into(),
                        argument: subject.into(),
                    }
                } else {
                    Term::Apply {
                        function: Term::Lambda {
                            parameter_name: Name {
                                text: subject_name,
                                unique: 0.into(),
                            },
                            body: term.into(),
                        }
                        .into(),
                        argument: constr_index_exposer(subject).into(),
                    }
                };

                arg_stack.push(term);
            }
            Air::Clause {
                tipo,
                subject_name,
                complex_clause,
                ..
            } => {
                // clause to compare
                let clause = arg_stack.pop().unwrap();

                // the body to be run if the clause matches
                let body = arg_stack.pop().unwrap();

                // the next branch in the when expression
                let mut term = arg_stack.pop().unwrap();

                let checker = if tipo.is_int() {
                    Term::Apply {
                        function: DefaultFunction::EqualsInteger.into(),
                        argument: Term::Var(Name {
                            text: subject_name,
                            unique: 0.into(),
                        })
                        .into(),
                    }
                } else if tipo.is_bytearray() {
                    Term::Apply {
                        function: DefaultFunction::EqualsByteString.into(),
                        argument: Term::Var(Name {
                            text: subject_name,
                            unique: 0.into(),
                        })
                        .into(),
                    }
                } else if tipo.is_bool() {
                    todo!()
                } else if tipo.is_string() {
                    Term::Apply {
                        function: DefaultFunction::EqualsString.into(),
                        argument: Term::Var(Name {
                            text: subject_name,
                            unique: 0.into(),
                        })
                        .into(),
                    }
                } else if tipo.is_list() {
                    unreachable!()
                } else {
                    Term::Apply {
                        function: DefaultFunction::EqualsInteger.into(),
                        argument: Term::Var(Name {
                            text: subject_name,
                            unique: 0.into(),
                        })
                        .into(),
                    }
                };

                if complex_clause {
                    term = Term::Apply {
                        function: Term::Lambda {
                            parameter_name: Name {
                                text: "__other_clauses_delayed".to_string(),
                                unique: 0.into(),
                            },
                            body: if_else(
                                Term::Apply {
                                    function: checker.into(),
                                    argument: clause.into(),
                                },
                                Term::Delay(body.into()),
                                Term::Var(Name {
                                    text: "__other_clauses_delayed".to_string(),
                                    unique: 0.into(),
                                }),
                            )
                            .force_wrap()
                            .into(),
                        }
                        .into(),
                        argument: Term::Delay(term.into()).into(),
                    }
                    .force_wrap()
                } else {
                    term = delayed_if_else(
                        Term::Apply {
                            function: checker.into(),
                            argument: clause.into(),
                        },
                        body,
                        term,
                    );
                }

                arg_stack.push(term);
            }
            Air::ListClause {
                tail_name,
                next_tail_name,
                ..
            } => {
                // discard to pop off
                let _ = arg_stack.pop().unwrap();

                // the body to be run if the clause matches
                let body = arg_stack.pop().unwrap();

                // the next branch in the when expression
                let mut term = arg_stack.pop().unwrap();

                let arg = if let Some(next_tail_name) = next_tail_name {
                    Term::Apply {
                        function: Term::Lambda {
                            parameter_name: Name {
                                text: next_tail_name,
                                unique: 0.into(),
                            },
                            body: term.into(),
                        }
                        .into(),
                        argument: Term::Apply {
                            function: Term::Builtin(DefaultFunction::TailList).force_wrap().into(),
                            argument: Term::Var(Name {
                                text: tail_name.clone(),
                                unique: 0.into(),
                            })
                            .into(),
                        }
                        .into(),
                    }
                } else {
                    term
                };

                term = Term::Apply {
                    function: Term::Apply {
                        function: Term::Apply {
                            function: Term::Builtin(DefaultFunction::ChooseList)
                                .force_wrap()
                                .force_wrap()
                                .into(),
                            argument: Term::Var(Name {
                                text: tail_name,
                                unique: 0.into(),
                            })
                            .into(),
                        }
                        .into(),
                        argument: Term::Delay(body.into()).into(),
                    }
                    .into(),
                    argument: Term::Delay(arg.into()).into(),
                }
                .force_wrap();

                arg_stack.push(term);
            }
            Air::ClauseGuard {
                subject_name, tipo, ..
            } => {
                let condition = arg_stack.pop().unwrap();

                let then = arg_stack.pop().unwrap();

                let checker = if tipo.is_int() {
                    Term::Apply {
                        function: DefaultFunction::EqualsInteger.into(),
                        argument: Term::Var(Name {
                            text: subject_name,
                            unique: 0.into(),
                        })
                        .into(),
                    }
                } else if tipo.is_bytearray() {
                    Term::Apply {
                        function: DefaultFunction::EqualsByteString.into(),
                        argument: Term::Var(Name {
                            text: subject_name,
                            unique: 0.into(),
                        })
                        .into(),
                    }
                } else if tipo.is_bool() {
                    todo!()
                } else if tipo.is_string() {
                    Term::Apply {
                        function: DefaultFunction::EqualsString.into(),
                        argument: Term::Var(Name {
                            text: subject_name,
                            unique: 0.into(),
                        })
                        .into(),
                    }
                } else if tipo.is_list() {
                    todo!()
                } else {
                    Term::Apply {
                        function: DefaultFunction::EqualsInteger.into(),
                        argument: Term::Var(Name {
                            text: subject_name,
                            unique: 0.into(),
                        })
                        .into(),
                    }
                };

                let term = if_else(
                    Term::Apply {
                        function: checker.into(),
                        argument: condition.into(),
                    },
                    Term::Delay(then.into()),
                    Term::Var(Name {
                        text: "__other_clauses_delayed".to_string(),
                        unique: 0.into(),
                    }),
                )
                .force_wrap();

                arg_stack.push(term);
            }
            Air::Finally { .. } => {
                let _clause = arg_stack.pop().unwrap();
            }
            Air::If { .. } => {
                let condition = arg_stack.pop().unwrap();
                let then = arg_stack.pop().unwrap();
                let mut term = arg_stack.pop().unwrap();

                term = delayed_if_else(condition, then, term);

                arg_stack.push(term);
            }
            Air::Constr { .. } => todo!(),
            Air::Fields { .. } => todo!(),
            Air::RecordAccess { index, tipo, .. } => {
                let constr = arg_stack.pop().unwrap();

                let mut term = Term::Apply {
                    function: Term::Apply {
                        function: Term::Var(Name {
                            text: CONSTR_GET_FIELD.to_string(),
                            unique: 0.into(),
                        })
                        .into(),
                        argument: Term::Apply {
                            function: Term::Var(Name {
                                text: CONSTR_FIELDS_EXPOSER.to_string(),
                                unique: 0.into(),
                            })
                            .into(),
                            argument: constr.into(),
                        }
                        .into(),
                    }
                    .into(),
                    argument: Term::Constant(UplcConstant::Integer(index.into())).into(),
                };

                term = convert_data_to_type(term, &tipo);

                arg_stack.push(term);
            }
            Air::FieldsExpose { indices, .. } => {
                self.needs_field_access = true;

                let constr_var = arg_stack.pop().unwrap();
                let mut body = arg_stack.pop().unwrap();

                let mut indices = indices.into_iter().rev();
                let highest = indices.next().unwrap();
                let mut id_list = vec![];

                for _ in 0..highest.0 {
                    id_list.push(self.id_gen.next());
                }

                let constr_name_lam = format!("__constr_fields_{}", self.id_gen.next());
                let highest_loop_index = highest.0 as i32 - 1;
                let last_prev_tail = Term::Var(Name {
                    text: if highest_loop_index == -1 {
                        constr_name_lam.clone()
                    } else {
                        format!(
                            "__tail_{}_{}",
                            highest_loop_index, id_list[highest_loop_index as usize]
                        )
                    },
                    unique: 0.into(),
                });

                body = Term::Apply {
                    function: Term::Lambda {
                        parameter_name: Name {
                            text: highest.1,
                            unique: 0.into(),
                        },
                        body: body.into(),
                    }
                    .into(),
                    argument: convert_data_to_type(
                        Term::Apply {
                            function: Term::Builtin(DefaultFunction::HeadList).force_wrap().into(),
                            argument: last_prev_tail.into(),
                        },
                        &highest.2,
                    )
                    .into(),
                };

                let mut current_field = None;
                for index in (0..highest.0).rev() {
                    let current_tail_index = index;
                    let previous_tail_index = if index == 0 { 0 } else { index - 1 };
                    let current_tail_id = id_list[index];
                    let previous_tail_id = if index == 0 { 0 } else { id_list[index - 1] };
                    if current_field.is_none() {
                        current_field = indices.next();
                    }

                    let prev_tail = if index == 0 {
                        Term::Var(Name {
                            text: constr_name_lam.clone(),
                            unique: 0.into(),
                        })
                    } else {
                        Term::Var(Name {
                            text: format!("__tail_{previous_tail_index}_{previous_tail_id}"),
                            unique: 0.into(),
                        })
                    };

                    if let Some(ref field) = current_field {
                        if field.0 == index {
                            let unwrapper = convert_data_to_type(
                                Term::Apply {
                                    function: Term::Builtin(DefaultFunction::HeadList)
                                        .force_wrap()
                                        .into(),
                                    argument: prev_tail.clone().into(),
                                },
                                &field.2,
                            );

                            body = Term::Apply {
                                function: Term::Lambda {
                                    parameter_name: Name {
                                        text: field.1.clone(),
                                        unique: 0.into(),
                                    },
                                    body: Term::Apply {
                                        function: Term::Lambda {
                                            parameter_name: Name {
                                                text: format!(
                                                    "__tail_{current_tail_index}_{current_tail_id}"
                                                ),
                                                unique: 0.into(),
                                            },
                                            body: body.into(),
                                        }
                                        .into(),
                                        argument: Term::Apply {
                                            function: Term::Builtin(DefaultFunction::TailList)
                                                .force_wrap()
                                                .into(),
                                            argument: prev_tail.into(),
                                        }
                                        .into(),
                                    }
                                    .into(),
                                }
                                .into(),
                                argument: unwrapper.into(),
                            };

                            current_field = None;
                        } else {
                            body = Term::Apply {
                                function: Term::Lambda {
                                    parameter_name: Name {
                                        text: format!(
                                            "__tail_{current_tail_index}_{current_tail_id}"
                                        ),
                                        unique: 0.into(),
                                    },
                                    body: body.into(),
                                }
                                .into(),
                                argument: Term::Apply {
                                    function: Term::Builtin(DefaultFunction::TailList)
                                        .force_wrap()
                                        .force_wrap()
                                        .into(),
                                    argument: prev_tail.into(),
                                }
                                .into(),
                            }
                        }
                    } else {
                        body = Term::Apply {
                            function: Term::Lambda {
                                parameter_name: Name {
                                    text: format!("__tail_{current_tail_index}_{current_tail_id}"),
                                    unique: 0.into(),
                                },
                                body: body.into(),
                            }
                            .into(),
                            argument: Term::Apply {
                                function: Term::Builtin(DefaultFunction::TailList)
                                    .force_wrap()
                                    .force_wrap()
                                    .into(),
                                argument: prev_tail.into(),
                            }
                            .into(),
                        }
                    }
                }

                body = Term::Apply {
                    function: Term::Lambda {
                        parameter_name: Name {
                            text: constr_name_lam,
                            unique: 0.into(),
                        },
                        body: body.into(),
                    }
                    .into(),
                    argument: Term::Apply {
                        function: Term::Var(Name {
                            text: CONSTR_FIELDS_EXPOSER.to_string(),
                            unique: 0.into(),
                        })
                        .into(),
                        argument: constr_var.into(),
                    }
                    .into(),
                };

                arg_stack.push(body);
            }
            Air::Tuple { tipo, count, .. } => {
                let mut args = vec![];

                for _ in 0..count {
                    let arg = arg_stack.pop().unwrap();
                    args.push(arg);
                }
                let mut constants = vec![];
                for arg in &args {
                    if let Term::Constant(c) = arg {
                        constants.push(c.clone())
                    }
                }

                let tuple_sub_types = tipo.get_inner_types();

                if constants.len() == args.len() {
                    let data_constants = convert_constants_to_data(constants);

                    if count == 2 {
                        let term = Term::Constant(UplcConstant::ProtoPair(
                            UplcType::Data,
                            UplcType::Data,
                            data_constants[0].clone().into(),
                            data_constants[1].clone().into(),
                        ));
                        arg_stack.push(term);
                    } else {
                        let term =
                            Term::Constant(UplcConstant::ProtoList(UplcType::Data, data_constants));
                        arg_stack.push(term);
                    }
                } else if count == 2 {
                    let term = Term::Apply {
                        function: Term::Apply {
                            function: DefaultFunction::MkPairData.into(),
                            argument: convert_type_to_data(args[0].clone(), &tuple_sub_types[0])
                                .into(),
                        }
                        .into(),
                        argument: convert_type_to_data(args[1].clone(), &tuple_sub_types[1]).into(),
                    };
                    arg_stack.push(term);
                } else {
                    let mut term = Term::Constant(UplcConstant::ProtoList(UplcType::Data, vec![]));
                    for (arg, tipo) in args.into_iter().zip(tuple_sub_types.into_iter()) {
                        term = Term::Apply {
                            function: Term::Apply {
                                function: Term::Builtin(DefaultFunction::MkCons)
                                    .force_wrap()
                                    .into(),
                                argument: convert_type_to_data(arg, &tipo).into(),
                            }
                            .into(),
                            argument: term.into(),
                        };
                    }
                    arg_stack.push(term);
                }
            }
            Air::Todo { .. } => {
                arg_stack.push(Term::Error);
            }
            Air::Record { .. } => todo!(),
            Air::RecordUpdate { .. } => todo!(),
            Air::Negate { .. } => {
                let value = arg_stack.pop().unwrap();

                let term = if_else(
                    value,
                    Term::Constant(UplcConstant::Bool(false)),
                    Term::Constant(UplcConstant::Bool(true)),
                );
                arg_stack.push(term);
            }
            Air::TupleAccessor { tipo, names, .. } => {
                let value = arg_stack.pop().unwrap();
                let mut term = arg_stack.pop().unwrap();
                let list_id = self.id_gen.next();

                if names.len() == 2 {
                    term = Term::Apply {
                        function: Term::Lambda {
                            parameter_name: Name {
                                text: format!("__tuple_{}", list_id),
                                unique: 0.into(),
                            },
                            body: Term::Apply {
                                function: Term::Lambda {
                                    parameter_name: Name {
                                        text: names[0].clone(),
                                        unique: 0.into(),
                                    },
                                    body: Term::Apply {
                                        function: Term::Lambda {
                                            parameter_name: Name {
                                                text: names[1].clone(),
                                                unique: 0.into(),
                                            },
                                            body: term.into(),
                                        }
                                        .into(),
                                        argument: Term::Apply {
                                            function: Term::Builtin(DefaultFunction::SndPair)
                                                .force_wrap()
                                                .force_wrap()
                                                .into(),
                                            argument: Term::Var(Name {
                                                text: format!("__tuple_{}", list_id),
                                                unique: 0.into(),
                                            })
                                            .into(),
                                        }
                                        .into(),
                                    }
                                    .into(),
                                }
                                .into(),
                                argument: Term::Apply {
                                    function: Term::Builtin(DefaultFunction::FstPair)
                                        .force_wrap()
                                        .force_wrap()
                                        .into(),
                                    argument: Term::Var(Name {
                                        text: format!("__tuple_{}", list_id),
                                        unique: 0.into(),
                                    })
                                    .into(),
                                }
                                .into(),
                            }
                            .into(),
                        }
                        .into(),
                        argument: value.into(),
                    };
                } else {
                    let mut id_list = vec![];

                    for _ in 0..names.len() {
                        id_list.push(self.id_gen.next());
                    }

                    let current_index = 0;
                    let (first_name, names) = names.split_first().unwrap();

                    let head_list = if tipo.is_map() {
                        Term::Apply {
                            function: Term::Force(Term::Builtin(DefaultFunction::HeadList).into())
                                .into(),
                            argument: Term::Var(Name {
                                text: format!("__tuple_{}", list_id),
                                unique: 0.into(),
                            })
                            .into(),
                        }
                    } else {
                        convert_data_to_type(
                            Term::Apply {
                                function: Term::Force(
                                    Term::Builtin(DefaultFunction::HeadList).into(),
                                )
                                .into(),
                                argument: Term::Var(Name {
                                    text: format!("__tuple_{}", list_id),
                                    unique: 0.into(),
                                })
                                .into(),
                            },
                            &tipo.get_inner_types()[0],
                        )
                    };

                    term = Term::Apply {
                        function: Term::Lambda {
                            parameter_name: Name {
                                text: format!("__tuple_{}", list_id),
                                unique: 0.into(),
                            },
                            body: Term::Apply {
                                function: Term::Lambda {
                                    parameter_name: Name {
                                        text: first_name.clone(),
                                        unique: 0.into(),
                                    },
                                    body: Term::Apply {
                                        function: list_access_to_uplc(
                                            names,
                                            &id_list,
                                            false,
                                            current_index,
                                            term,
                                            &tipo,
                                        )
                                        .into(),
                                        argument: Term::Apply {
                                            function: Term::Force(
                                                Term::Builtin(DefaultFunction::TailList).into(),
                                            )
                                            .into(),
                                            argument: Term::Var(Name {
                                                text: format!("__tuple_{}", list_id),
                                                unique: 0.into(),
                                            })
                                            .into(),
                                        }
                                        .into(),
                                    }
                                    .into(),
                                }
                                .into(),
                                argument: head_list.into(),
                            }
                            .into(),
                        }
                        .into(),
                        argument: value.into(),
                    };
                }

                arg_stack.push(term);
            }
        }
    }
}
