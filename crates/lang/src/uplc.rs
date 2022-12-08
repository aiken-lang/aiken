use std::{collections::HashMap, ops::Deref, sync::Arc, vec};

use indexmap::IndexMap;
use itertools::Itertools;
use uplc::{
    ast::{
        builder::{self, constr_index_exposer, CONSTR_FIELDS_EXPOSER, CONSTR_GET_FIELD},
        Constant as UplcConstant, Name, Program, Term, Type as UplcType,
    },
    builtins::DefaultFunction,
    machine::runtime::convert_constr_to_tag,
    parser::interner::Interner,
    BigInt, Constr, PlutusData,
};

use crate::{
    air::Air,
    ast::{
        ArgName, AssignmentKind, BinOp, Clause, Constant, DataType, Function, Pattern, Span,
        TypedArg,
    },
    expr::TypedExpr,
    tipo::{self, PatternConstructor, Type, TypeInfo, ValueConstructor, ValueConstructorVariant},
    IdGenerator,
};

#[derive(Clone, Debug)]
pub struct FuncComponents {
    ir: Vec<Air>,
    dependencies: Vec<FunctionAccessKey>,
    args: Vec<String>,
    recursive: bool,
}

#[derive(Clone, Eq, Debug, PartialEq, Hash)]
pub struct ConstrFieldKey {
    pub local_var: String,
    pub field_name: String,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct DataTypeKey {
    pub module_name: String,
    pub defined_type: String,
}

pub type ConstrUsageKey = String;

#[derive(Clone, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct FunctionAccessKey {
    pub module_name: String,
    pub function_name: String,
}

#[derive(Clone, Debug)]
pub struct ClauseProperties {
    clause_var_name: String,
    needs_constr_var: bool,
    is_complex_clause: bool,
    current_index: usize,
    original_subject_name: String,
}

pub struct CodeGenerator<'a> {
    defined_functions: HashMap<FunctionAccessKey, ()>,
    functions: &'a HashMap<FunctionAccessKey, &'a Function<Arc<tipo::Type>, TypedExpr>>,
    // type_aliases: &'a HashMap<(String, String), &'a TypeAlias<Arc<tipo::Type>>>,
    data_types: &'a HashMap<DataTypeKey, &'a DataType<Arc<tipo::Type>>>,
    // imports: &'a HashMap<(String, String), &'a Use<String>>,
    // constants: &'a HashMap<(String, String), &'a ModuleConstant<Arc<tipo::Type>, String>>,
    module_types: &'a HashMap<String, TypeInfo>,
    id_gen: IdGenerator,
    needs_field_access: bool,
}

impl<'a> CodeGenerator<'a> {
    pub fn new(
        functions: &'a HashMap<FunctionAccessKey, &'a Function<Arc<tipo::Type>, TypedExpr>>,
        // type_aliases: &'a HashMap<(String, String), &'a TypeAlias<Arc<tipo::Type>>>,
        data_types: &'a HashMap<DataTypeKey, &'a DataType<Arc<tipo::Type>>>,
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

    pub fn generate(&mut self, body: TypedExpr, arguments: Vec<TypedArg>) -> Program<Name> {
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
        term = builder::final_wrapper(term);

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
            } => {
                if let ValueConstructorVariant::ModuleConstant { literal, .. } =
                    &constructor.variant
                {
                    constants_ir(literal, ir_stack, scope);
                } else {
                    ir_stack.push(Air::Var {
                        scope,
                        constructor: constructor.clone(),
                        name: name.clone(),
                    });
                }
            }
            TypedExpr::Fn { .. } => todo!(),
            TypedExpr::List {
                elements,
                tail,
                tipo,
                ..
            } => {
                ir_stack.push(Air::List {
                    scope: scope.clone(),
                    count: if tail.is_some() {
                        elements.len() + 1
                    } else {
                        elements.len()
                    },
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
                    count: args.len() + 1,
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
                let mut needs_subject_var = false;

                let clauses = if matches!(clauses[0].pattern[0], Pattern::List { .. }) {
                    rearrange_clauses(clauses.clone())
                } else {
                    clauses.clone()
                };

                if let Some((last_clause, clauses)) = clauses.split_last() {
                    let mut clauses_vec = vec![];
                    let mut pattern_vec = vec![];

                    let mut clause_properties = ClauseProperties {
                        clause_var_name: constr_var.clone(),
                        needs_constr_var: false,
                        is_complex_clause: false,
                        current_index: 0,
                        original_subject_name: subject_name.clone(),
                    };

                    for (index, clause) in clauses.iter().enumerate() {
                        // scope per clause is different
                        let mut scope = scope.clone();
                        scope.push(self.id_gen.next());

                        // holds when clause pattern Air
                        let mut clause_subject_vec = vec![];

                        // reset complex clause setting per clause back to default
                        clause_properties.is_complex_clause = false;

                        self.build_ir(&clause.then, &mut clauses_vec, scope.clone());

                        self.when_ir(
                            &clause.pattern[0],
                            &mut clause_subject_vec,
                            &mut clauses_vec,
                            &subject.tipo(),
                            &mut clause_properties,
                            scope.clone(),
                        );

                        if clause_properties.needs_constr_var {
                            needs_subject_var = true;
                        }

                        let subject_name = if clause_properties.current_index == 0 {
                            subject_name.clone()
                        } else {
                            format!("__tail_{}", clause_properties.current_index - 1)
                        };

                        // Clause is first in Air pattern vec
                        if subject.tipo().is_list() {
                            let next_tail = if index == clauses.len() - 1 {
                                None
                            } else {
                                Some(format!("__tail_{}", clause_properties.current_index))
                            };

                            pattern_vec.push(Air::ListClause {
                                scope,
                                tipo: subject.tipo().clone(),
                                tail_name: subject_name,
                                complex_clause: clause_properties.is_complex_clause,
                                next_tail_name: next_tail,
                            });

                            clause_properties.current_index += 1;
                        } else {
                            pattern_vec.push(Air::Clause {
                                scope,
                                tipo: subject.tipo().clone(),
                                subject_name,
                                complex_clause: clause_properties.is_complex_clause,
                            });
                        }

                        pattern_vec.append(&mut clause_subject_vec);
                    }

                    let last_pattern = &last_clause.pattern[0];

                    let mut final_scope = scope.clone();
                    final_scope.push(self.id_gen.next());
                    pattern_vec.push(Air::Finally {
                        scope: final_scope.clone(),
                    });

                    self.build_ir(&last_clause.then, &mut clauses_vec, final_scope.clone());

                    self.when_ir(
                        last_pattern,
                        &mut pattern_vec,
                        &mut clauses_vec,
                        &subject.tipo(),
                        &mut clause_properties,
                        final_scope,
                    );

                    if needs_subject_var || clause_properties.needs_constr_var {
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
                ..
            } => match constructor {
                tipo::ModuleValueConstructor::Record { .. } => todo!(),
                tipo::ModuleValueConstructor::Fn { name, module, .. } => {
                    let func = self.functions.get(&FunctionAccessKey {
                        module_name: module_name.clone(),
                        function_name: name.clone(),
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
            TypedExpr::Negate { .. } => todo!(),
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
                let mut needs_access_to_constr_var = false;
                let mut needs_clause_guard = false;

                for arg in arguments {
                    check_when_pattern_needs(
                        &arg.value,
                        &mut needs_access_to_constr_var,
                        &mut needs_clause_guard,
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
                }];

                // if only one constructor, no need to check
                if data_type.constructors.len() > 1 {
                    // push constructor Index
                    pattern_vec.push(Air::Int {
                        value: index.to_string(),
                        scope: scope.clone(),
                    });
                }

                if needs_clause_guard {
                    clause_properties.is_complex_clause = true;
                }

                if needs_access_to_constr_var {
                    clause_properties.needs_constr_var = true;

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
                            });
                            self.pattern_ir(
                                a,
                                &mut elements_vec,
                                &mut var_vec,
                                &tipo.get_inner_type()[0],
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
                            });
                            self.pattern_ir(
                                a,
                                &mut elements_vec,
                                &mut var_vec,
                                &tipo.get_inner_type()[0],
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
                name, constructor, ..
            } => {
                match constructor.variant {
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
                        let name = if func_name == name {
                            format!("{module}_{func_name}")
                        } else {
                            name
                        };
                        arg_stack.push(Term::Var(Name {
                            text: name,
                            unique: 0.into(),
                        }));
                    }
                    ValueConstructorVariant::Record {
                        name: constr_name,
                        field_map,
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

                        if data_type_key.defined_type == "Bool" {
                            arg_stack
                                .push(Term::Constant(UplcConstant::Bool(constr_name == "True")));
                        } else {
                            let data_type = self.data_types.get(&data_type_key).unwrap();
                            let (constr_index, _constr) = data_type
                                .constructors
                                .iter()
                                .enumerate()
                                .find(|(_, x)| x.name == *constr_name)
                                .unwrap();

                            let mut fields =
                                Term::Constant(UplcConstant::ProtoList(UplcType::Data, vec![]));

                            let tipo = constructor.tipo;

                            let args_type = match tipo.as_ref() {
                                Type::Fn { args, .. } => args,

                                _ => todo!(),
                            };

                            if let Some(field_map) = field_map.clone() {
                                for field in field_map
                                    .fields
                                    .iter()
                                    .sorted_by(|item1, item2| item1.1.cmp(item2.1))
                                    .zip(args_type)
                                    .rev()
                                {
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

                let list_type = tipo.get_inner_type()[0].clone();

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
                                function: Term::Force(
                                    Term::Builtin(DefaultFunction::MkCons).force_wrap().into(),
                                )
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
                        &tipo.get_inner_type()[0],
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
                            &tipo.get_inner_type()[0],
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
            Air::Call { count, .. } => {
                if count >= 2 {
                    let mut term = arg_stack.pop().unwrap();

                    for _ in 0..count - 1 {
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
            Air::Builtin { func, .. } => {
                let mut term = Term::Builtin(func);
                for _ in 0..func.force_count() {
                    term = Term::Force(term.into());
                }
                arg_stack.push(term);
            }
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
                    BinOp::And => Term::Apply {
                        function: Term::Apply {
                            function: Term::Apply {
                                function: Term::Builtin(DefaultFunction::IfThenElse)
                                    .force_wrap()
                                    .into(),
                                argument: left.into(),
                            }
                            .into(),
                            argument: Term::Delay(right.into()).into(),
                        }
                        .into(),
                        argument: Term::Delay(Term::Constant(UplcConstant::Bool(false)).into())
                            .into(),
                    }
                    .force_wrap(),
                    BinOp::Or => Term::Apply {
                        function: Term::Apply {
                            function: Term::Apply {
                                function: Term::Builtin(DefaultFunction::IfThenElse)
                                    .force_wrap()
                                    .into(),
                                argument: left.into(),
                            }
                            .into(),
                            argument: Term::Delay(Term::Constant(UplcConstant::Bool(false)).into())
                                .into(),
                        }
                        .into(),
                        argument: Term::Delay(right.into()).into(),
                    }
                    .force_wrap(),
                    BinOp::Eq => {
                        if tipo.is_bool() {
                            let term = Term::Force(
                                Term::Apply {
                                    function: Term::Apply {
                                        function: Term::Apply {
                                            function: Term::Force(
                                                Term::Builtin(DefaultFunction::IfThenElse).into(),
                                            )
                                            .into(),
                                            argument: left.into(),
                                        }
                                        .into(),
                                        argument: Term::Delay(right.clone().into()).into(),
                                    }
                                    .into(),
                                    argument: Term::Delay(
                                        Term::Apply {
                                            function: Term::Apply {
                                                function: Term::Apply {
                                                    function: Term::Force(
                                                        Term::Builtin(DefaultFunction::IfThenElse)
                                                            .into(),
                                                    )
                                                    .into(),
                                                    argument: right.into(),
                                                }
                                                .into(),
                                                argument: Term::Constant(UplcConstant::Bool(false))
                                                    .into(),
                                            }
                                            .into(),
                                            argument: Term::Constant(UplcConstant::Bool(true))
                                                .into(),
                                        }
                                        .into(),
                                    )
                                    .into(),
                                }
                                .into(),
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
                            && matches!(tipo.get_uplc_type(), UplcType::Pair(_, _))
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
                        } else if tipo.is_list()
                            || matches!(tipo.get_uplc_type(), UplcType::List(_))
                        {
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
                                    function: default_builtin.into(),
                                    argument: Term::Apply {
                                        function: DefaultFunction::ListData.into(),
                                        argument: right.into(),
                                    }
                                    .into(),
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
                            let term = Term::Force(
                                Term::Apply {
                                    function: Term::Apply {
                                        function: Term::Apply {
                                            function: Term::Force(
                                                Term::Builtin(DefaultFunction::IfThenElse).into(),
                                            )
                                            .into(),
                                            argument: left.into(),
                                        }
                                        .into(),
                                        argument: Term::Delay(
                                            Term::Apply {
                                                function: Term::Apply {
                                                    function: Term::Apply {
                                                        function: Term::Force(
                                                            Term::Builtin(
                                                                DefaultFunction::IfThenElse,
                                                            )
                                                            .into(),
                                                        )
                                                        .into(),
                                                        argument: right.clone().into(),
                                                    }
                                                    .into(),
                                                    argument: Term::Constant(UplcConstant::Bool(
                                                        false,
                                                    ))
                                                    .into(),
                                                }
                                                .into(),
                                                argument: Term::Constant(UplcConstant::Bool(true))
                                                    .into(),
                                            }
                                            .into(),
                                        )
                                        .into(),
                                    }
                                    .into(),
                                    argument: Term::Delay(right.into()).into(),
                                }
                                .into(),
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
                            && matches!(tipo.get_uplc_type(), UplcType::Pair(_, _))
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
                ..
            } => {
                let func_name = if module_name.is_empty() {
                    func_name
                } else {
                    format!("{module_name}_{func_name}")
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
                            body: Term::Apply {
                                function: Term::Apply {
                                    function: Term::Apply {
                                        function: Term::Builtin(DefaultFunction::IfThenElse)
                                            .force_wrap()
                                            .into(),
                                        argument: Term::Apply {
                                            function: checker.into(),
                                            argument: clause.into(),
                                        }
                                        .into(),
                                    }
                                    .into(),
                                    argument: Term::Delay(body.into()).into(),
                                }
                                .into(),
                                argument: Term::Var(Name {
                                    text: "__other_clauses_delayed".to_string(),
                                    unique: 0.into(),
                                })
                                .into(),
                            }
                            .into(),
                        }
                        .into(),
                        argument: Term::Delay(term.into()).into(),
                    }
                    .force_wrap()
                } else {
                    term = Term::Apply {
                        function: Term::Apply {
                            function: Term::Apply {
                                function: Term::Force(DefaultFunction::IfThenElse.into()).into(),
                                argument: Term::Apply {
                                    function: checker.into(),
                                    argument: clause.into(),
                                }
                                .into(),
                            }
                            .into(),
                            argument: Term::Delay(body.into()).into(),
                        }
                        .into(),
                        argument: Term::Delay(term.into()).into(),
                    }
                    .force_wrap();
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

                let term = Term::Apply {
                    function: Term::Apply {
                        function: Term::Apply {
                            function: Term::Builtin(DefaultFunction::IfThenElse)
                                .force_wrap()
                                .into(),
                            argument: Term::Apply {
                                function: checker.into(),
                                argument: condition.into(),
                            }
                            .into(),
                        }
                        .into(),
                        argument: Term::Delay(then.into()).into(),
                    }
                    .into(),
                    argument: Term::Var(Name {
                        text: "__other_clauses_delayed".to_string(),
                        unique: 0.into(),
                    })
                    .into(),
                }
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

                term = Term::Force(
                    Term::Apply {
                        function: Term::Apply {
                            function: Term::Apply {
                                function: Term::Builtin(DefaultFunction::IfThenElse)
                                    .force_wrap()
                                    .into(),
                                argument: condition.into(),
                            }
                            .into(),
                            argument: Term::Delay(then.into()).into(),
                        }
                        .into(),
                        argument: Term::Delay(term.into()).into(),
                    }
                    .into(),
                );

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

                let tuple_sub_types = tipo.get_inner_type();

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
            Air::Negate { .. } => todo!(),
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
                            &tipo.get_inner_type()[0],
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

    pub(crate) fn define_ir(&mut self, ir_stack: &mut Vec<Air>) {
        let mut func_components = IndexMap::new();
        let mut func_index_map = IndexMap::new();

        let recursion_func_map = IndexMap::new();

        self.define_recurse_ir(
            ir_stack,
            &mut func_components,
            &mut func_index_map,
            recursion_func_map,
        );

        let mut final_func_dep_ir = IndexMap::new();

        for func in func_index_map.clone() {
            if self.defined_functions.contains_key(&func.0) {
                continue;
            }

            let mut funt_comp = func_components.get(&func.0).unwrap().clone();
            let func_scope = func_index_map.get(&func.0).unwrap();

            let mut dep_ir = vec![];

            while let Some(dependency) = funt_comp.dependencies.pop() {
                if self.defined_functions.contains_key(&dependency) {
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
                    }];

                    temp_ir.extend(depend_comp.ir.clone());

                    temp_ir.append(&mut dep_ir);

                    dep_ir = temp_ir;
                    self.defined_functions.insert(dependency, ());
                }
            }

            final_func_dep_ir.insert(func.0, dep_ir);
        }

        for (index, ir) in ir_stack.clone().into_iter().enumerate().rev() {
            match ir {
                Air::Var { constructor, .. } => {
                    if let ValueConstructorVariant::ModuleFn { .. } = &constructor.variant {}
                }
                a => {
                    let temp_func_index_map = func_index_map.clone();
                    let to_insert = temp_func_index_map
                        .iter()
                        .filter(|func| {
                            func.1.clone() == a.scope()
                                && !self.defined_functions.contains_key(func.0)
                        })
                        .collect_vec();

                    for item in to_insert.into_iter() {
                        func_index_map.remove(item.0);
                        self.defined_functions.insert(item.0.clone(), ());

                        let mut full_func_ir = final_func_dep_ir.get(item.0).unwrap().clone();

                        let funt_comp = func_components.get(item.0).unwrap();

                        full_func_ir.push(Air::DefineFunc {
                            scope: item.1.clone(),
                            func_name: item.0.function_name.clone(),
                            module_name: item.0.module_name.clone(),
                            params: funt_comp.args.clone(),
                            recursive: funt_comp.recursive,
                        });

                        full_func_ir.extend(funt_comp.ir.clone());

                        for ir in full_func_ir.into_iter().rev() {
                            ir_stack.insert(index, ir);
                        }
                    }
                }
            }
        }
    }

    fn define_recurse_ir(
        &mut self,
        ir_stack: &[Air],
        func_components: &mut IndexMap<FunctionAccessKey, FuncComponents>,
        func_index_map: &mut IndexMap<FunctionAccessKey, Vec<u64>>,
        recursion_func_map: IndexMap<FunctionAccessKey, ()>,
    ) {
        self.process_define_ir(ir_stack, func_components, func_index_map);

        let mut recursion_func_map = recursion_func_map;

        for func_index in func_index_map.clone().iter() {
            let func = func_index.0;

            let function_components = func_components.get(func).unwrap();
            let function_ir = function_components.ir.clone();

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
                    ..
                } = ir
                {
                    if recursion_func_map.contains_key(&FunctionAccessKey {
                        module_name: module.clone(),
                        function_name: func_name.clone(),
                    }) {
                        return;
                    } else {
                        recursion_func_map.insert(
                            FunctionAccessKey {
                                module_name: module.clone(),
                                function_name: func_name.clone(),
                            },
                            (),
                        );
                    }
                }
            }

            let mut inner_func_components = IndexMap::new();

            let mut inner_func_index_map = IndexMap::new();

            self.define_recurse_ir(
                &function_ir,
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
        ir_stack: &[Air],
        func_components: &mut IndexMap<FunctionAccessKey, FuncComponents>,
        func_index_map: &mut IndexMap<FunctionAccessKey, Vec<u64>>,
    ) {
        let mut to_be_defined_map: IndexMap<FunctionAccessKey, Vec<u64>> = IndexMap::new();
        for ir in ir_stack.iter().rev() {
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
                            let function_key = FunctionAccessKey {
                                module_name: module.clone(),
                                function_name: name.clone(),
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

                                to_be_defined_map.insert(function_key.clone(), scope.to_vec());
                                let mut func_calls = vec![];

                                for ir in func_ir.clone() {
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
                                        ..
                                    } = ir
                                    {
                                        func_calls.push(FunctionAccessKey {
                                            module_name: module.clone(),
                                            function_name: func_name.clone(),
                                        })
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
                                let recursive =
                                    if let Ok(index) = func_calls.binary_search(&function_key) {
                                        func_calls.remove(index);
                                        true
                                    } else {
                                        false
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
}

fn convert_constants_to_data(constants: Vec<UplcConstant>) -> Vec<UplcConstant> {
    let mut new_constants = vec![];
    for constant in constants {
        let constant = match constant {
            UplcConstant::Integer(i) => {
                UplcConstant::Data(PlutusData::BigInt(BigInt::Int((i).try_into().unwrap())))
            }
            UplcConstant::ByteString(b) => {
                UplcConstant::Data(PlutusData::BoundedBytes(b.try_into().unwrap()))
            }
            UplcConstant::String(s) => UplcConstant::Data(PlutusData::BoundedBytes(
                s.as_bytes().to_vec().try_into().unwrap(),
            )),

            UplcConstant::Bool(b) => UplcConstant::Data(PlutusData::Constr(Constr {
                tag: u64::from(b),
                any_constructor: None,
                fields: vec![],
            })),
            UplcConstant::ProtoList(_, _) => todo!(),
            UplcConstant::ProtoPair(_, _, _, _) => todo!(),
            d @ UplcConstant::Data(_) => d,
            _ => unreachable!(),
        };
        new_constants.push(constant);
    }
    new_constants
}

fn constants_ir(literal: &Constant<Arc<Type>, String>, ir_stack: &mut Vec<Air>, scope: Vec<u64>) {
    match literal {
        Constant::Int { value, .. } => {
            ir_stack.push(Air::Int {
                scope,
                value: value.clone(),
            });
        }
        Constant::String { value, .. } => {
            ir_stack.push(Air::String {
                scope,
                value: value.clone(),
            });
        }
        Constant::Tuple { .. } => {
            todo!()
        }
        Constant::List { elements, tipo, .. } => {
            ir_stack.push(Air::List {
                scope: scope.clone(),
                count: elements.len(),
                tipo: tipo.clone(),
                tail: false,
            });

            for element in elements {
                constants_ir(element, ir_stack, scope.clone());
            }
        }
        Constant::Record { .. } => {
            // ir_stack.push(Air::Record { scope,  });
            todo!()
        }
        Constant::ByteArray { bytes, .. } => {
            ir_stack.push(Air::ByteArray {
                scope,
                bytes: bytes.clone(),
            });
        }
        Constant::Var { .. } => todo!(),
    };
}

fn check_when_pattern_needs(
    pattern: &Pattern<tipo::PatternConstructor, Arc<Type>>,
    needs_access_to_constr_var: &mut bool,
    needs_clause_guard: &mut bool,
) {
    match pattern {
        Pattern::Var { .. } => {
            *needs_access_to_constr_var = true;
        }
        Pattern::List { .. }
        | Pattern::Constructor { .. }
        | Pattern::Tuple { .. }
        | Pattern::Int { .. } => {
            *needs_access_to_constr_var = true;
            *needs_clause_guard = true;
        }
        Pattern::Discard { .. } => {}

        _ => todo!("{pattern:#?}"),
    }
}

fn get_common_ancestor(scope: &[u64], scope_prev: &[u64]) -> Vec<u64> {
    let longest_length = if scope.len() >= scope_prev.len() {
        scope.len()
    } else {
        scope_prev.len()
    };

    if *scope == *scope_prev {
        return scope.to_vec();
    }

    for index in 0..longest_length {
        if scope.get(index).is_none() {
            return scope.to_vec();
        } else if scope_prev.get(index).is_none() {
            return scope_prev.to_vec();
        } else if scope[index] != scope_prev[index] {
            return scope[0..index].to_vec();
        }
    }
    vec![]
}

fn list_access_to_uplc(
    names: &[String],
    id_list: &[u64],
    tail: bool,
    current_index: usize,
    term: Term<Name>,
    tipo: &Type,
) -> Term<Name> {
    let (first, names) = names.split_first().unwrap();

    let head_list = if tipo.is_map() {
        Term::Apply {
            function: Term::Force(Term::Builtin(DefaultFunction::HeadList).into()).into(),
            argument: Term::Var(Name {
                text: format!("tail_index_{}_{}", current_index, id_list[current_index]),
                unique: 0.into(),
            })
            .into(),
        }
    } else {
        convert_data_to_type(
            Term::Apply {
                function: Term::Force(Term::Builtin(DefaultFunction::HeadList).into()).into(),
                argument: Term::Var(Name {
                    text: format!("tail_index_{}_{}", current_index, id_list[current_index]),
                    unique: 0.into(),
                })
                .into(),
            },
            &tipo.clone().get_inner_type()[0],
        )
    };

    if names.len() == 1 && tail {
        Term::Lambda {
            parameter_name: Name {
                text: format!("tail_index_{}_{}", current_index, id_list[current_index]),
                unique: 0.into(),
            },
            body: Term::Apply {
                function: Term::Lambda {
                    parameter_name: Name {
                        text: first.clone(),
                        unique: 0.into(),
                    },
                    body: Term::Apply {
                        function: Term::Lambda {
                            parameter_name: Name {
                                text: names[0].clone(),
                                unique: 0.into(),
                            },
                            body: term.into(),
                        }
                        .into(),
                        argument: Term::Apply {
                            function: Term::Force(Term::Builtin(DefaultFunction::TailList).into())
                                .into(),
                            argument: Term::Var(Name {
                                text: format!(
                                    "tail_index_{}_{}",
                                    current_index, id_list[current_index]
                                ),
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
    } else if names.is_empty() {
        Term::Lambda {
            parameter_name: Name {
                text: format!("tail_index_{}_{}", current_index, id_list[current_index]),
                unique: 0.into(),
            },
            body: Term::Apply {
                function: Term::Lambda {
                    parameter_name: Name {
                        text: first.clone(),
                        unique: 0.into(),
                    },
                    body: term.into(),
                }
                .into(),
                argument: Term::Apply {
                    function: Term::Force(Term::Builtin(DefaultFunction::HeadList).into()).into(),
                    argument: Term::Var(Name {
                        text: format!("tail_index_{}_{}", current_index, id_list[current_index]),
                        unique: 0.into(),
                    })
                    .into(),
                }
                .into(),
            }
            .into(),
        }
    } else {
        Term::Lambda {
            parameter_name: Name {
                text: format!("tail_index_{}_{}", current_index, id_list[current_index]),
                unique: 0.into(),
            },
            body: Term::Apply {
                function: Term::Lambda {
                    parameter_name: Name {
                        text: first.clone(),
                        unique: 0.into(),
                    },
                    body: Term::Apply {
                        function: list_access_to_uplc(
                            names,
                            id_list,
                            tail,
                            current_index + 1,
                            term,
                            tipo,
                        )
                        .into(),
                        argument: Term::Apply {
                            function: Term::Force(Term::Builtin(DefaultFunction::TailList).into())
                                .into(),
                            argument: Term::Var(Name {
                                text: format!(
                                    "tail_index_{}_{}",
                                    current_index, id_list[current_index]
                                ),
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
    }
}

fn rearrange_clauses(
    clauses: Vec<Clause<TypedExpr, PatternConstructor, Arc<Type>, String>>,
) -> Vec<Clause<TypedExpr, PatternConstructor, Arc<Type>, String>> {
    let mut sorted_clauses = clauses;

    // if we have a list sort clauses so we can plug holes for cases not covered by clauses
    // TODO: while having 10000000 element list is impossible to destructure in plutus budget,
    // let's sort clauses by a safer manner
    // TODO: how shall tails be weighted? Since any clause after will not run
    sorted_clauses.sort_by(|clause1, clause2| {
        let clause1_len = match &clause1.pattern[0] {
            Pattern::List { elements, tail, .. } => elements.len() + usize::from(tail.is_some()),
            _ => 10000000,
        };
        let clause2_len = match &clause2.pattern[0] {
            Pattern::List { elements, tail, .. } => elements.len() + usize::from(tail.is_some()),
            _ => 10000001,
        };

        clause1_len.cmp(&clause2_len)
    });

    let mut elems_len = 0;
    let mut final_clauses = sorted_clauses.clone();
    let mut holes_to_fill = vec![];
    let mut assign_plug_in_name = None;
    let mut last_clause_index = 0;
    let mut last_clause_set = false;

    // If we have a catch all, use that. Otherwise use todo which will result in error
    // TODO: fill in todo label with description
    let plug_in_then = match &sorted_clauses[sorted_clauses.len() - 1].pattern[0] {
        Pattern::Var { name, .. } => {
            assign_plug_in_name = Some(name);
            sorted_clauses[sorted_clauses.len() - 1].clone().then
        }
        Pattern::Discard { .. } => sorted_clauses[sorted_clauses.len() - 1].clone().then,
        _ => TypedExpr::Todo {
            location: Span::empty(),
            label: None,
            tipo: sorted_clauses[sorted_clauses.len() - 1].then.tipo(),
        },
    };

    for (index, clause) in sorted_clauses.iter().enumerate() {
        if let Pattern::List { elements, .. } = &clause.pattern[0] {
            // found a hole and now we plug it
            while elems_len < elements.len() {
                let mut discard_elems = vec![];

                for _ in 0..elems_len {
                    discard_elems.push(Pattern::Discard {
                        name: "_".to_string(),
                        location: Span::empty(),
                    });
                }

                // If we have a named catch all then in scope the name and create list of discards, otherwise list of discards
                let clause_to_fill = if let Some(name) = assign_plug_in_name {
                    Clause {
                        location: Span::empty(),
                        pattern: vec![Pattern::Assign {
                            name: name.clone(),
                            location: Span::empty(),
                            pattern: Pattern::List {
                                location: Span::empty(),
                                elements: discard_elems,
                                tail: None,
                            }
                            .into(),
                        }],
                        alternative_patterns: vec![],
                        guard: None,
                        then: plug_in_then.clone(),
                    }
                } else {
                    Clause {
                        location: Span::empty(),
                        pattern: vec![Pattern::List {
                            location: Span::empty(),
                            elements: discard_elems,
                            tail: None,
                        }],
                        alternative_patterns: vec![],
                        guard: None,
                        then: plug_in_then.clone(),
                    }
                };

                holes_to_fill.push((index, clause_to_fill));
                elems_len += 1;
            }
        }

        // if we have a pattern with no clause guards and a tail then no lists will get past here to other clauses
        if let Pattern::List {
            elements,
            tail: Some(tail),
            ..
        } = &clause.pattern[0]
        {
            let mut elements = elements.clone();
            elements.push(*tail.clone());
            if elements
                .iter()
                .all(|element| matches!(element, Pattern::Var { .. } | Pattern::Discard { .. }))
                && !last_clause_set
            {
                last_clause_index = index;
                last_clause_set = true;
            }
        }

        // If the last condition doesn't have a catch all or tail then add a catch all with a todo
        if index == sorted_clauses.len() - 1 {
            if let Pattern::List {
                elements,
                tail: Some(tail),
                ..
            } = &clause.pattern[0]
            {
                let mut elements = elements.clone();
                elements.push(*tail.clone());
                if !elements
                    .iter()
                    .all(|element| matches!(element, Pattern::Var { .. } | Pattern::Discard { .. }))
                {
                    final_clauses.push(Clause {
                        location: Span::empty(),
                        pattern: vec![Pattern::Discard {
                            name: "_".to_string(),
                            location: Span::empty(),
                        }],
                        alternative_patterns: vec![],
                        guard: None,
                        then: plug_in_then.clone(),
                    });
                }
            }
        }

        elems_len += 1;
    }

    // Encountered a tail so stop there with that as last clause
    final_clauses = final_clauses[0..(last_clause_index + 1)].to_vec();

    // insert hole fillers into clauses
    for (index, clause) in holes_to_fill.into_iter().rev() {
        final_clauses.insert(index, clause);
    }

    final_clauses
}

fn convert_type_to_data(term: Term<Name>, field_type: &Arc<Type>) -> Term<Name> {
    if field_type.is_bytearray() {
        Term::Apply {
            function: DefaultFunction::BData.into(),
            argument: term.into(),
        }
    } else if field_type.is_int() {
        Term::Apply {
            function: DefaultFunction::IData.into(),
            argument: term.into(),
        }
    } else if field_type.is_map() {
        Term::Apply {
            function: DefaultFunction::MapData.into(),
            argument: term.into(),
        }
    } else if field_type.is_list() {
        Term::Apply {
            function: DefaultFunction::ListData.into(),
            argument: term.into(),
        }
    } else if field_type.is_string() {
        Term::Apply {
            function: DefaultFunction::BData.into(),
            argument: Term::Apply {
                function: DefaultFunction::EncodeUtf8.into(),
                argument: term.into(),
            }
            .into(),
        }
    } else if field_type.is_tuple() {
        match field_type.get_uplc_type() {
            UplcType::List(_) => Term::Apply {
                function: DefaultFunction::ListData.into(),
                argument: term.into(),
            },
            UplcType::Pair(_, _) => Term::Apply {
                function: Term::Lambda {
                    parameter_name: Name {
                        text: "__pair".to_string(),
                        unique: 0.into(),
                    },
                    body: Term::Apply {
                        function: DefaultFunction::ListData.into(),
                        argument: Term::Apply {
                            function: Term::Apply {
                                function: Term::Builtin(DefaultFunction::MkCons)
                                    .force_wrap()
                                    .into(),
                                argument: Term::Apply {
                                    function: Term::Builtin(DefaultFunction::FstPair)
                                        .force_wrap()
                                        .force_wrap()
                                        .into(),
                                    argument: Term::Var(Name {
                                        text: "__pair".to_string(),
                                        unique: 0.into(),
                                    })
                                    .into(),
                                }
                                .into(),
                            }
                            .into(),

                            argument: Term::Apply {
                                function: Term::Apply {
                                    function: Term::Builtin(DefaultFunction::MkCons)
                                        .force_wrap()
                                        .into(),
                                    argument: Term::Apply {
                                        function: Term::Builtin(DefaultFunction::SndPair)
                                            .force_wrap()
                                            .force_wrap()
                                            .into(),
                                        argument: Term::Var(Name {
                                            text: "__pair".to_string(),
                                            unique: 0.into(),
                                        })
                                        .into(),
                                    }
                                    .into(),
                                }
                                .into(),
                                argument: Term::Constant(UplcConstant::ProtoList(
                                    UplcType::Data,
                                    vec![],
                                ))
                                .into(),
                            }
                            .into(),
                        }
                        .into(),
                    }
                    .into(),
                }
                .into(),
                argument: term.into(),
            },
            _ => unreachable!(),
        }
    } else if field_type.is_bool() {
        Term::Apply {
            function: Term::Apply {
                function: Term::Apply {
                    function: Term::Builtin(DefaultFunction::IfThenElse)
                        .force_wrap()
                        .into(),
                    argument: term.into(),
                }
                .into(),
                argument: Term::Constant(UplcConstant::Data(PlutusData::Constr(Constr {
                    tag: convert_constr_to_tag(1),
                    any_constructor: None,
                    fields: vec![],
                })))
                .into(),
            }
            .into(),
            argument: Term::Constant(UplcConstant::Data(PlutusData::Constr(Constr {
                tag: convert_constr_to_tag(0),
                any_constructor: None,
                fields: vec![],
            })))
            .into(),
        }
    } else {
        term
    }
}

fn convert_data_to_type(term: Term<Name>, field_type: &Arc<Type>) -> Term<Name> {
    if field_type.is_int() {
        Term::Apply {
            function: DefaultFunction::UnIData.into(),
            argument: term.into(),
        }
    } else if field_type.is_bytearray() {
        Term::Apply {
            function: DefaultFunction::UnBData.into(),
            argument: term.into(),
        }
    } else if field_type.is_map() {
        Term::Apply {
            function: DefaultFunction::UnMapData.into(),
            argument: term.into(),
        }
    } else if field_type.is_list() {
        Term::Apply {
            function: DefaultFunction::UnListData.into(),
            argument: term.into(),
        }
    } else if field_type.is_string() {
        Term::Apply {
            function: DefaultFunction::DecodeUtf8.into(),
            argument: Term::Apply {
                function: DefaultFunction::UnBData.into(),
                argument: term.into(),
            }
            .into(),
        }
    } else if field_type.is_tuple() {
        match field_type.get_uplc_type() {
            UplcType::List(_) => Term::Apply {
                function: DefaultFunction::UnListData.into(),
                argument: term.into(),
            },
            UplcType::Pair(_, _) => Term::Apply {
                function: Term::Lambda {
                    parameter_name: Name {
                        text: "__list_data".to_string(),
                        unique: 0.into(),
                    },
                    body: Term::Apply {
                        function: Term::Lambda {
                            parameter_name: Name {
                                text: "__tail".to_string(),
                                unique: 0.into(),
                            },
                            body: Term::Apply {
                                function: Term::Apply {
                                    function: Term::Builtin(DefaultFunction::MkPairData).into(),
                                    argument: Term::Apply {
                                        function: Term::Builtin(DefaultFunction::HeadList)
                                            .force_wrap()
                                            .into(),
                                        argument: Term::Var(Name {
                                            text: "__list_data".to_string(),
                                            unique: 0.into(),
                                        })
                                        .into(),
                                    }
                                    .into(),
                                }
                                .into(),
                                argument: Term::Apply {
                                    function: Term::Builtin(DefaultFunction::HeadList)
                                        .force_wrap()
                                        .into(),
                                    argument: Term::Var(Name {
                                        text: "__tail".to_string(),
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
                            function: Term::Builtin(DefaultFunction::TailList).force_wrap().into(),
                            argument: Term::Var(Name {
                                text: "__list_data".to_string(),
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
                    function: Term::Builtin(DefaultFunction::UnListData)
                        .force_wrap()
                        .into(),
                    argument: term.into(),
                }
                .into(),
            },
            _ => unreachable!(),
        }
    } else if field_type.is_bool() {
        Term::Apply {
            function: Term::Apply {
                function: Term::Builtin(DefaultFunction::EqualsInteger).into(),
                argument: Term::Constant(UplcConstant::Integer(1)).into(),
            }
            .into(),
            argument: Term::Apply {
                function: Term::Builtin(DefaultFunction::FstPair)
                    .force_wrap()
                    .force_wrap()
                    .into(),
                argument: Term::Apply {
                    function: Term::Builtin(DefaultFunction::UnConstrData).into(),
                    argument: term.into(),
                }
                .into(),
            }
            .into(),
        }
    } else {
        term
    }
}
