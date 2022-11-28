use std::{collections::HashMap, ops::Deref, sync::Arc};

use itertools::Itertools;
use uplc::{
    ast::{
        builder::{self, CONSTR_FIELDS_EXPOSER, CONSTR_GET_FIELD},
        Constant, Name, Program, Term,
    },
    builtins::DefaultFunction,
    parser::interner::Interner,
    BigInt, PlutusData,
};

use crate::{
    ast::{AssignmentKind, BinOp, DataType, Function, Pattern, Span, TypedArg},
    expr::TypedExpr,
    ir::IR,
    tipo::{self, Type, TypeInfo, ValueConstructor, ValueConstructorVariant},
    uplc::{DataTypeKey, FunctionAccessKey},
    IdGenerator,
};

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

        self.build_ir(&body, &mut ir_stack);

        println!("{ir_stack:#?}");

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

        println!("{}", program.to_pretty());

        interner.program(&mut program);

        program
    }

    pub(crate) fn build_ir(&mut self, body: &TypedExpr, ir_stack: &mut Vec<IR>) {
        match dbg!(body) {
            TypedExpr::Int { value, .. } => ir_stack.push(IR::Int {
                value: value.to_string(),
            }),
            TypedExpr::String { value, .. } => ir_stack.push(IR::String {
                value: value.to_string(),
            }),
            TypedExpr::ByteArray { bytes, .. } => ir_stack.push(IR::ByteArray {
                bytes: bytes.to_vec(),
            }),
            TypedExpr::Sequence { expressions, .. } => {
                for expr in expressions {
                    self.build_ir(expr, ir_stack);
                }
            }
            TypedExpr::Pipeline { expressions, .. } => {
                for expr in expressions {
                    self.build_ir(expr, ir_stack);
                }
            }
            TypedExpr::Var {
                constructor, name, ..
            } => {
                ir_stack.push(IR::Var {
                    constructor: constructor.clone(),
                    name: name.clone(),
                });
            }
            TypedExpr::Fn { .. } => todo!(),
            TypedExpr::List {
                elements,
                tail,
                tipo,
                ..
            } => {
                ir_stack.push(IR::List {
                    count: if tail.is_some() {
                        elements.len() + 1
                    } else {
                        elements.len()
                    },
                    tipo: tipo.clone(),
                    tail: tail.is_some(),
                });

                for element in elements {
                    self.build_ir(element, ir_stack)
                }

                if let Some(tail) = tail {
                    ir_stack.push(IR::Tail { count: 1 });
                    self.build_ir(tail, ir_stack);
                }
            }
            TypedExpr::Call { fun, args, .. } => {
                ir_stack.push(IR::Call {
                    count: args.len() + 1,
                });
                self.build_ir(fun, ir_stack);

                for arg in args {
                    self.build_ir(&arg.value, ir_stack);
                }
            }
            TypedExpr::BinOp {
                name, left, right, ..
            } => {
                ir_stack.push(IR::BinOp {
                    name: *name,
                    count: 2,
                    tipo: left.tipo(),
                });
                self.build_ir(left, ir_stack);
                self.build_ir(right, ir_stack);
            }
            TypedExpr::Assignment {
                value,
                pattern,
                kind,
                tipo,
                ..
            } => {
                let mut define_vec: Vec<IR> = vec![];
                let mut value_vec: Vec<IR> = vec![];
                let mut pattern_vec: Vec<IR> = vec![];
                self.build_ir(value, &mut value_vec);
                self.define_ir(&value_vec, &mut define_vec);

                self.assignment_ir(pattern, &mut pattern_vec, &mut value_vec, tipo, *kind);

                ir_stack.append(&mut define_vec);
                ir_stack.append(&mut pattern_vec);
            }
            TypedExpr::Try { .. } => todo!(),
            TypedExpr::When {
                subjects, clauses, ..
            } => {
                let subject_name = format!("__subject_name_{}", self.id_gen.next());
                let constr_var = format!("__constr_name_{}", self.id_gen.next());

                // assuming one subject at the moment
                let subject = subjects[0].clone();
                let mut needs_constr_var = false;

                if let Some((last_clause, clauses)) = clauses.split_last() {
                    let mut clauses_vec = vec![];
                    let mut pattern_vec = vec![];

                    for clause in clauses {
                        pattern_vec.push(IR::Clause {
                            count: 2,
                            tipo: subject.tipo().clone(),
                            subject_name: subject_name.clone(),
                        });

                        self.build_ir(&clause.then, &mut clauses_vec);
                        self.when_ir(
                            &clause.pattern[0],
                            &mut pattern_vec,
                            &mut clauses_vec,
                            &subject.tipo(),
                            constr_var.clone(),
                            &mut needs_constr_var,
                        );
                    }

                    let last_pattern = &last_clause.pattern[0];
                    pattern_vec.push(IR::Finally);

                    self.build_ir(&last_clause.then, &mut clauses_vec);
                    self.when_ir(
                        last_pattern,
                        &mut pattern_vec,
                        &mut clauses_vec,
                        &subject.tipo(),
                        constr_var.clone(),
                        &mut needs_constr_var,
                    );

                    if needs_constr_var {
                        ir_stack.push(IR::Lam {
                            name: constr_var.clone(),
                        });

                        self.build_ir(&subject, ir_stack);

                        ir_stack.push(IR::When {
                            count: clauses.len() + 1,
                            subject_name,
                            tipo: subject.tipo(),
                        });

                        ir_stack.push(IR::Var {
                            constructor: ValueConstructor::public(
                                subject.tipo(),
                                ValueConstructorVariant::LocalVariable {
                                    location: Span::empty(),
                                },
                            ),
                            name: constr_var,
                        })
                    } else {
                        ir_stack.push(IR::When {
                            count: clauses.len() + 1,
                            subject_name,
                            tipo: subject.tipo(),
                        });

                        self.build_ir(&subject, ir_stack);
                    }

                    ir_stack.append(&mut pattern_vec);
                };
            }
            TypedExpr::If { .. } => todo!(),
            TypedExpr::RecordAccess {
                record,
                index,
                tipo,
                ..
            } => {
                self.needs_field_access = true;

                ir_stack.push(IR::RecordAccess {
                    index: *index,
                    tipo: tipo.clone(),
                });

                self.build_ir(record, ir_stack);
            }
            TypedExpr::ModuleSelect {
                constructor,
                module_name,
                ..
            } => match constructor {
                tipo::ModuleValueConstructor::Record { .. } => todo!(),
                tipo::ModuleValueConstructor::Fn { name, .. } => {
                    let func = self.functions.get(&FunctionAccessKey {
                        module_name: module_name.clone(),
                        function_name: name.clone(),
                    });

                    if let Some(_func) = func {
                        todo!()
                    } else {
                        let type_info = self.module_types.get(module_name).unwrap();
                        let value = type_info.values.get(name).unwrap();
                        match &value.variant {
                            ValueConstructorVariant::ModuleFn { builtin, .. } => {
                                let builtin = builtin.unwrap();

                                ir_stack.push(IR::Builtin { func: builtin });
                            }
                            _ => unreachable!(),
                        }
                    }
                }
                tipo::ModuleValueConstructor::Constant { .. } => todo!(),
            },
            TypedExpr::Todo { .. } => todo!(),
            TypedExpr::RecordUpdate { .. } => todo!(),
            TypedExpr::Negate { .. } => todo!(),
        }
    }

    fn define_ir(&self, value_vec: &Vec<IR>, _define_vec: &mut [IR]) {
        // get value item
        for value in value_vec {
            match dbg!(value) {
                IR::Int { .. } => {}
                IR::Var { constructor, .. } => match constructor.variant {
                    ValueConstructorVariant::LocalVariable { .. } => {}
                    ValueConstructorVariant::ModuleConstant { .. } => todo!(),
                    ValueConstructorVariant::ModuleFn { .. } => todo!(),
                    ValueConstructorVariant::Record { .. } => {}
                },
                IR::Call { .. } => {}
                IR::BinOp { .. } => {}
                IR::When { .. } => todo!(),
                IR::Clause { .. } => todo!(),
                IR::Finally { .. } => todo!(),
                IR::If { .. } => todo!(),
                IR::Constr { .. } => todo!(),
                IR::Fields { .. } => todo!(),
                IR::RecordAccess { .. } => todo!(),
                IR::FieldsExpose { .. } => todo!(),
                IR::Todo { .. } => todo!(),
                IR::RecordUpdate { .. } => todo!(),
                IR::Negate { .. } => todo!(),
                IR::Builtin { .. } => {}
                IR::List { .. } => {}
                _ => todo!(),
            }
        }
    }

    fn assignment_ir(
        &mut self,
        pattern: &Pattern<tipo::PatternConstructor, Arc<Type>>,
        pattern_vec: &mut Vec<IR>,
        value_vec: &mut Vec<IR>,
        _tipo: &Type,
        kind: AssignmentKind,
    ) {
        match pattern {
            Pattern::Int { .. } => todo!(),
            Pattern::String { .. } => todo!(),
            Pattern::Var { name, .. } => {
                pattern_vec.push(IR::Assignment {
                    name: name.clone(),
                    kind,
                });

                pattern_vec.append(value_vec);
            }
            Pattern::VarUsage { .. } => todo!(),
            Pattern::Assign { .. } => todo!(),
            Pattern::Discard { .. } => todo!(),
            list @ Pattern::List { .. } => {
                self.pattern_ir(list, pattern_vec, value_vec);
            }
            Pattern::Constructor { .. } => todo!(),
        }
    }

    fn when_ir(
        &mut self,
        pattern: &Pattern<tipo::PatternConstructor, Arc<tipo::Type>>,
        pattern_vec: &mut Vec<IR>,
        values: &mut Vec<IR>,
        tipo: &Type,
        constr_var: String,
        needs_constr_var: &mut bool,
    ) {
        match pattern {
            Pattern::Int { value, .. } => {
                pattern_vec.push(IR::Int {
                    value: value.clone(),
                });

                pattern_vec.append(values);
            }
            Pattern::String { .. } => todo!(),
            Pattern::Var { .. } => todo!(),
            Pattern::VarUsage { .. } => todo!(),
            Pattern::Assign { .. } => todo!(),
            Pattern::Discard { .. } => unreachable!(),
            Pattern::List { .. } => todo!(),
            Pattern::Constructor { arguments, .. } => {
                let mut needs_access_to_constr_var = false;
                for arg in arguments {
                    match arg.value {
                        Pattern::Var { .. }
                        | Pattern::List { .. }
                        | Pattern::Constructor { .. } => {
                            needs_access_to_constr_var = true;
                        }
                        _ => {}
                    }
                }

                let mut new_vec = vec![IR::Var {
                    constructor: ValueConstructor::public(
                        tipo.clone().into(),
                        ValueConstructorVariant::LocalVariable {
                            location: Span::empty(),
                        },
                    ),
                    name: constr_var,
                }];

                if needs_access_to_constr_var {
                    *needs_constr_var = true;
                    new_vec.append(values);

                    self.pattern_ir(pattern, pattern_vec, &mut new_vec);
                } else {
                    self.pattern_ir(pattern, pattern_vec, values);
                }
            }
        }
    }

    fn pattern_ir(
        &mut self,
        pattern: &Pattern<tipo::PatternConstructor, Arc<tipo::Type>>,
        pattern_vec: &mut Vec<IR>,
        values: &mut Vec<IR>,
    ) {
        match dbg!(pattern) {
            Pattern::Int { .. } => todo!(),
            Pattern::String { .. } => todo!(),
            Pattern::Var { .. } => todo!(),
            Pattern::VarUsage { .. } => todo!(),
            Pattern::Assign { .. } => todo!(),
            Pattern::Discard { .. } => {
                pattern_vec.push(IR::Discard);

                pattern_vec.append(values);
            }
            Pattern::List { elements, tail, .. } => {
                let mut elements_vec = vec![];

                let mut names = vec![];
                for element in elements {
                    match dbg!(element) {
                        Pattern::Var { name, .. } => {
                            names.push(name.clone());
                        }
                        a @ Pattern::List { .. } => {
                            let mut var_vec = vec![];
                            let item_name = format!("list_item_id_{}", self.id_gen.next());
                            names.push(item_name.clone());
                            var_vec.push(IR::Var {
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
                            });
                            self.pattern_ir(a, &mut elements_vec, &mut var_vec);
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

                pattern_vec.push(IR::ListAccessor {
                    names,
                    tail: tail.is_some(),
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
                if *is_record {
                    let data_type_key = match tipo.as_ref() {
                        Type::Fn { ret, .. } => match &**ret {
                            Type::App { module, name, .. } => DataTypeKey {
                                module_name: module.clone(),
                                defined_type: name.clone(),
                            },
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    };

                    let data_type = self.data_types.get(&data_type_key).unwrap();
                    let (index, constructor_type) = data_type
                        .constructors
                        .iter()
                        .enumerate()
                        .find(|(_, dt)| &dt.name == constr_name)
                        .unwrap();

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
                                Pattern::Constructor { .. } => todo!(),
                                _ => todo!(),
                            };

                            (label, var_name, *field_index, discard)
                        })
                        .filter(|(_, _, _, discard)| !discard)
                        .sorted_by(|item1, item2| item1.2.cmp(&item2.2))
                        .collect::<Vec<(String, String, usize, bool)>>();

                    // push constructor Index
                    pattern_vec.push(IR::Int {
                        value: index.to_string(),
                    });
                    if !arguments_index.is_empty() {
                        pattern_vec.push(IR::FieldsExpose {
                            count: arguments_index.len() + 2,
                            indices: arguments_index
                                .iter()
                                .map(|(_, _, index, _)| *index)
                                .collect_vec(),
                        });

                        for arg in arguments_index {
                            let field_label = arg.0;
                            let field_type = type_map.get(&field_label).unwrap();
                            let field_var = arg.1;
                            pattern_vec.push(IR::Var {
                                constructor: ValueConstructor::public(
                                    field_type.clone(),
                                    ValueConstructorVariant::LocalVariable {
                                        location: Span::empty(),
                                    },
                                ),
                                name: field_var,
                            })
                        }
                    }
                    pattern_vec.append(values);
                } else {
                    println!("todo");
                }
            }
        }
    }

    fn uplc_code_gen(&self, ir_stack: &mut Vec<IR>) -> Term<Name> {
        let mut arg_stack: Vec<Term<Name>> = vec![];

        while let Some(ir_element) = ir_stack.pop() {
            self.gen_uplc(ir_element, &mut arg_stack);
        }

        arg_stack[0].clone()
    }

    fn gen_uplc(&self, ir: IR, arg_stack: &mut Vec<Term<Name>>) {
        match ir {
            IR::Int { value } => {
                let integer = value.parse().unwrap();

                let term = Term::Constant(Constant::Integer(integer));

                arg_stack.push(term);
            }
            IR::String { value } => {
                let term = Term::Constant(Constant::String(value));

                arg_stack.push(term);
            }
            IR::ByteArray { bytes } => {
                let term = Term::Constant(Constant::ByteString(bytes));
                arg_stack.push(term);
            }
            IR::Var { name, constructor } => match constructor.variant {
                ValueConstructorVariant::LocalVariable { .. } => arg_stack.push(Term::Var(Name {
                    text: name,
                    unique: 0.into(),
                })),
                ValueConstructorVariant::ModuleConstant { .. } => todo!(),
                ValueConstructorVariant::ModuleFn { .. } => todo!(),
                ValueConstructorVariant::Record {
                    name: constr_name, ..
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
                    };

                    if data_type_key.defined_type == "Bool" {
                        arg_stack.push(Term::Constant(Constant::Bool(constr_name == "True")));
                    } else {
                        let data_type = self.data_types.get(&data_type_key).unwrap();
                        let (constr_index, _constr) = data_type
                            .constructors
                            .iter()
                            .enumerate()
                            .find(|(_, x)| x.name == *constr_name)
                            .unwrap();

                        let term = Term::Apply {
                            function: Term::Builtin(DefaultFunction::ConstrData).into(),
                            argument: Term::Apply {
                                function: Term::Apply {
                                    function: Term::Builtin(DefaultFunction::MkPairData).into(),
                                    argument: Term::Constant(Constant::Data(PlutusData::BigInt(
                                        BigInt::Int((constr_index as i128).try_into().unwrap()),
                                    )))
                                    .into(),
                                }
                                .into(),
                                argument: Term::Constant(Constant::Data(PlutusData::Array(vec![])))
                                    .into(),
                            }
                            .into(),
                        };

                        arg_stack.push(term);
                    }
                }
            },
            IR::Discard => {
                arg_stack.push(Term::Constant(Constant::Unit));
            }
            IR::List { count, tipo, tail } => {
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

                let list_type = match tipo.deref() {
                    Type::App { args, .. } => &args[0],
                    _ => unreachable!(),
                };

                if constants.len() == args.len() && !tail {
                    let list =
                        Term::Constant(Constant::ProtoList(list_type.get_uplc_type(), constants));

                    arg_stack.push(list);
                } else {
                    let mut term = if tail {
                        arg_stack.pop().unwrap()
                    } else {
                        Term::Constant(Constant::ProtoList(list_type.get_uplc_type(), vec![]))
                    };

                    for arg in args {
                        term = Term::Apply {
                            function: Term::Apply {
                                function: Term::Force(
                                    Term::Builtin(DefaultFunction::MkCons).into(),
                                )
                                .into(),
                                argument: arg.into(),
                            }
                            .into(),
                            argument: term.into(),
                        };
                    }
                    arg_stack.push(term);
                }
            }

            IR::Tail { .. } => todo!(),
            IR::ListAccessor { names, tail } => {
                let value = arg_stack.pop().unwrap();
                let mut term = arg_stack.pop().unwrap();

                let mut id_list = vec![];

                for _ in 0..names.len() {
                    id_list.push(self.id_gen.next());
                }

                let current_index = 0;
                let (first_name, names) = names.split_first().unwrap();

                term = Term::Apply {
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
                            )
                            .into(),
                            argument: Term::Apply {
                                function: Term::Force(
                                    Term::Builtin(DefaultFunction::TailList).into(),
                                )
                                .into(),
                                argument: value.clone().into(),
                            }
                            .into(),
                        }
                        .into(),
                    }
                    .into(),
                    argument: Term::Apply {
                        function: Term::Force(Term::Builtin(DefaultFunction::HeadList).into())
                            .into(),
                        argument: value.into(),
                    }
                    .into(),
                };

                arg_stack.push(term);
            }
            IR::Call { count } => {
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
            IR::Builtin { func } => {
                let mut term = Term::Builtin(func);
                for _ in 0..func.force_count() {
                    term = Term::Force(term.into());
                }
                arg_stack.push(term);
            }
            IR::BinOp { name, tipo, .. } => {
                let left = arg_stack.pop().unwrap();
                let right = arg_stack.pop().unwrap();

                let term = match name {
                    BinOp::And => todo!(),
                    BinOp::Or => todo!(),
                    BinOp::Eq => {
                        let default_builtin = match tipo.deref() {
                            Type::App { name, .. } => {
                                if name == "Int" {
                                    Term::Builtin(DefaultFunction::EqualsInteger)
                                } else if name == "String" {
                                    Term::Builtin(DefaultFunction::EqualsString)
                                } else if name == "ByteArray" {
                                    Term::Builtin(DefaultFunction::EqualsByteString)
                                } else if name == "Bool" {
                                    let term = Term::Force(
                                        Term::Apply {
                                            function: Term::Apply {
                                                function: Term::Apply {
                                                    function: Term::Force(
                                                        Term::Builtin(DefaultFunction::IfThenElse)
                                                            .into(),
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
                                                            argument: Term::Constant(
                                                                Constant::Bool(true),
                                                            )
                                                            .into(),
                                                        }
                                                        .into(),
                                                        argument: Term::Constant(Constant::Bool(
                                                            false,
                                                        ))
                                                        .into(),
                                                    }
                                                    .into(),
                                                )
                                                .into(),
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
                                                            argument: right.into(),
                                                        }
                                                        .into(),
                                                        argument: Term::Constant(Constant::Bool(
                                                            false,
                                                        ))
                                                        .into(),
                                                    }
                                                    .into(),
                                                    argument: Term::Constant(Constant::Bool(true))
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
                                } else {
                                    Term::Builtin(DefaultFunction::EqualsData)
                                }
                            }
                            _ => unreachable!(),
                        };

                        Term::Apply {
                            function: Term::Apply {
                                function: default_builtin.into(),
                                argument: left.into(),
                            }
                            .into(),
                            argument: right.into(),
                        }
                    }
                    BinOp::NotEq => todo!(),
                    BinOp::LtInt => todo!(),
                    BinOp::LtEqInt => todo!(),
                    BinOp::GtEqInt => todo!(),
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
                    BinOp::SubInt => todo!(),
                    BinOp::MultInt => todo!(),
                    BinOp::DivInt => todo!(),
                    BinOp::ModInt => todo!(),
                };
                arg_stack.push(term);
            }
            IR::Assignment { name, .. } => {
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
            IR::DefineFunc { .. } => {
                let _body = arg_stack.pop().unwrap();

                todo!()
            }
            IR::DefineConst { .. } => todo!(),
            IR::DefineConstrFields { .. } => todo!(),
            IR::DefineConstrFieldAccess { .. } => todo!(),
            IR::Lam { .. } => todo!(),
            IR::When {
                subject_name, tipo, ..
            } => {
                let subject = arg_stack.pop().unwrap();

                let mut term = arg_stack.pop().unwrap();

                term = if tipo.is_int() {
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
                    todo!()
                };

                arg_stack.push(term);
            }
            IR::Clause {
                tipo, subject_name, ..
            } => {
                // clause to compare
                let clause = arg_stack.pop().unwrap();

                // the body to be run if the clause matches
                let body = arg_stack.pop().unwrap();

                // the final branch in the when expression
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

                arg_stack.push(term);
            }
            IR::Finally => {
                let clause = arg_stack.pop().unwrap();

                if !clause.is_unit() {
                    let _body = arg_stack.pop().unwrap();
                    todo!();
                }
            }
            IR::If { .. } => todo!(),
            IR::Constr { .. } => todo!(),
            IR::Fields { .. } => todo!(),
            IR::RecordAccess { index, tipo } => {
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
                    argument: Term::Constant(Constant::Integer(index.into())).into(),
                };

                if tipo.is_int() {
                    term = Term::Apply {
                        function: Term::Builtin(DefaultFunction::UnIData).into(),
                        argument: term.into(),
                    };
                } else if tipo.is_bytearray() {
                    term = Term::Apply {
                        function: Term::Builtin(DefaultFunction::UnBData).into(),
                        argument: term.into(),
                    };
                } else if tipo.is_list() {
                    term = Term::Apply {
                        function: Term::Builtin(DefaultFunction::UnListData).into(),
                        argument: term.into(),
                    };
                }

                arg_stack.push(term);
            }
            IR::FieldsExpose { .. } => todo!(),
            IR::Todo { .. } => todo!(),
            IR::RecordUpdate { .. } => todo!(),
            IR::Negate { .. } => todo!(),
        }
    }
}

fn list_access_to_uplc(
    names: &[String],
    id_list: &[u64],
    tail: bool,
    current_index: usize,
    term: Term<Name>,
) -> Term<Name> {
    let (first, names) = names.split_first().unwrap();

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
    }
}
