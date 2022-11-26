use std::{collections::HashMap, sync::Arc};

use indexmap::IndexMap;
use uplc::{
    ast::{Name, Program, Term},
    parser::interner::Interner,
};

use crate::{
    ast::{DataType, Function, TypedArg},
    expr::TypedExpr,
    ir::IR,
    tipo,
    uplc::{
        ConstrFieldKey, ConstrUsageKey, DataTypeKey, FunctionAccessKey, ScopeLevels, ScopedExpr,
    },
};

pub struct CodeGenerator<'a> {
    // uplc_function_holder: Vec<(String, Term<Name>)>,
    // uplc_function_holder_lookup: IndexMap<FunctionAccessKey, ScopeLevels>,
    // uplc_data_holder_lookup: IndexMap<ConstrFieldKey, ScopedExpr>,
    // uplc_data_constr_lookup: IndexMap<DataTypeKey, ScopeLevels>,
    // uplc_data_usage_holder_lookup: IndexMap<ConstrUsageKey, ScopeLevels>,
    function_recurse_lookup: IndexMap<FunctionAccessKey, usize>,
    functions: &'a HashMap<FunctionAccessKey, &'a Function<Arc<tipo::Type>, TypedExpr>>,
    // type_aliases: &'a HashMap<(String, String), &'a TypeAlias<Arc<tipo::Type>>>,
    data_types: &'a HashMap<DataTypeKey, &'a DataType<Arc<tipo::Type>>>,
    // imports: &'a HashMap<(String, String), &'a Use<String>>,
    // constants: &'a HashMap<(String, String), &'a ModuleConstant<Arc<tipo::Type>, String>>,
}

impl<'a> CodeGenerator<'a> {
    pub fn new(
        functions: &'a HashMap<FunctionAccessKey, &'a Function<Arc<tipo::Type>, TypedExpr>>,
        // type_aliases: &'a HashMap<(String, String), &'a TypeAlias<Arc<tipo::Type>>>,
        data_types: &'a HashMap<DataTypeKey, &'a DataType<Arc<tipo::Type>>>,
        // imports: &'a HashMap<(String, String), &'a Use<String>>,
        // constants: &'a HashMap<(String, String), &'a ModuleConstant<Arc<tipo::Type>, String>>,
    ) -> Self {
        CodeGenerator {
            // uplc_function_holder: Vec::new(),
            // uplc_function_holder_lookup: IndexMap::new(),
            // uplc_data_holder_lookup: IndexMap::new(),
            // uplc_data_constr_lookup: IndexMap::new(),
            // uplc_data_usage_holder_lookup: IndexMap::new(),
            function_recurse_lookup: IndexMap::new(),
            functions,
            // type_aliases,
            data_types,
            // imports,
            // constants,
        }
    }

    pub fn generate(&mut self, body: TypedExpr, arguments: Vec<TypedArg>) -> Program<Name> {
        let mut ir_stack = vec![];

        self.build_ir(&body, &mut ir_stack);

        todo!();

        // self.uplc_function_holder_lookup
        //     .sort_by(|_key1, value1, _key2, value2| {
        //         if value1.is_less_than(value2, true) {
        //             Ordering::Less
        //         } else if value2.is_less_than(value1, true) {
        //             Ordering::Greater
        //         } else {
        //             Ordering::Equal
        //         }
        //     });

        // println!("DATA HOLDER LOOKUP{:#?}", self.uplc_data_holder_lookup);

        // println!(
        //     "DATA USAGE HOLDER {:#?}",
        //     self.uplc_data_usage_holder_lookup
        // );

        // let mut term = self.recurse_code_gen(&body, ScopeLevels::new());

        // // Apply constr exposer to top level.
        // term = Term::Apply {
        //     function: Term::Lambda {
        //         parameter_name: Name {
        //             text: "constr_fields_exposer".to_string(),
        //             unique: 0.into(),
        //         },
        //         body: term.into(),
        //     }
        //     .into(),
        //     argument: Term::Lambda {
        //         parameter_name: Name {
        //             text: "constr_var".to_string(),
        //             unique: 0.into(),
        //         },
        //         body: Term::Apply {
        //             function: Term::Force(
        //                 Term::Force(Term::Builtin(DefaultFunction::SndPair).into()).into(),
        //             )
        //             .into(),
        //             argument: Term::Apply {
        //                 function: Term::Builtin(DefaultFunction::UnConstrData).into(),
        //                 argument: Term::Var(Name {
        //                     text: "constr_var".to_string(),
        //                     unique: 0.into(),
        //                 })
        //                 .into(),
        //             }
        //             .into(),
        //         }
        //         .into(),
        //     }
        //     .into(),
        // };

        // term = self.add_arg_getter(term);

        // term = Term::Force(
        //     Term::Apply {
        //         function: Term::Apply {
        //             function: Term::Apply {
        //                 function: Term::Force(Term::Builtin(DefaultFunction::IfThenElse).into())
        //                     .into(),
        //                 argument: term.into(),
        //             }
        //             .into(),
        //             argument: Term::Delay(Term::Constant(Constant::Unit).into()).into(),
        //         }
        //         .into(),
        //         argument: Term::Delay(Term::Error.into()).into(),
        //     }
        //     .into(),
        // );

        // for arg in arguments.iter().rev() {
        //     term = Term::Lambda {
        //         parameter_name: uplc::ast::Name {
        //             text: arg.arg_name.get_variable_name().unwrap_or("_").to_string(),
        //             unique: Unique::new(0),
        //         },
        //         body: Rc::new(term),
        //     }
        // }

        // let mut program = Program {
        //     version: (1, 0, 0),
        //     term,
        // };

        // let mut interner = Interner::new();

        // println!("{}", program.to_pretty());

        // interner.program(&mut program);

        // program
    }

    pub(crate) fn build_ir(&mut self, body: &TypedExpr, ir_stack: &mut Vec<IR>) {
        match body {
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

                // Add constructor information here?
            }
            TypedExpr::Fn { .. } => todo!(),
            TypedExpr::List { elements, tail, .. } => {
                ir_stack.push(IR::List {
                    count: if tail.is_some() {
                        elements.len() + 1
                    } else {
                        elements.len()
                    },
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
                });
                self.build_ir(left, ir_stack);
                self.build_ir(right, ir_stack);
            }
            TypedExpr::Assignment {
                value,
                pattern,
                kind,
                ..
            } => {
                let mut define_vec: Vec<IR> = vec![];
                let mut value_vec: Vec<IR> = vec![];
                let mut pattern_vec: Vec<IR> = vec![];
                pattern_vec.push(IR::Assignment {
                    count: 3,
                    kind: *kind,
                });

                self.build_ir(value, &mut value_vec);
                self.define_ir(&value_vec, &mut define_vec);

                self.pattern_ir(pattern, &mut pattern_vec);

                for define in define_vec {
                    ir_stack.push(define);
                }
                for pattern in pattern_vec {
                    ir_stack.push(pattern);
                }
                for value in value_vec {
                    ir_stack.push(value);
                }
            }
            TypedExpr::Try { .. } => todo!(),
            TypedExpr::When {
                subjects, clauses, ..
            } => {
                // assuming one subject at the moment
                ir_stack.push(IR::When {
                    count: clauses.len() + 1,
                });
                let subject = subjects[0].clone();

                self.build_ir(&subject, ir_stack);

                if let Some((last_clause, clauses)) = clauses.split_last() {
                    for clause in clauses {
                        ir_stack.push(IR::Clause { count: 2 });
                        self.pattern_ir(&clause.pattern[0], ir_stack);
                        self.build_ir(&clause.then, ir_stack);
                    }
                    let last_pattern = &last_clause.pattern[0];

                    let mut pattern_vec = vec![];
                    let mut value_vec = vec![];

                    self.pattern_ir(last_pattern, &mut pattern_vec);
                    self.build_ir(&last_clause.then, &mut value_vec);

                    let mut final_vec = vec![];

                    final_vec.push(IR::Finally { count: 1 });

                    let pattern_field_map = match last_pattern {
                        crate::ast::Pattern::Int { .. } => todo!(),
                        crate::ast::Pattern::String { .. } => todo!(),
                        crate::ast::Pattern::Var { .. } => todo!(),
                        crate::ast::Pattern::VarUsage { .. } => todo!(),
                        crate::ast::Pattern::Assign { .. } => todo!(),
                        crate::ast::Pattern::Discard { name, location } => todo!(),
                        crate::ast::Pattern::List { .. } => todo!(),
                        crate::ast::Pattern::Constructor {
                            is_record,
                            location,
                            name,
                            arguments,
                            module,
                            constructor,
                            with_spread,
                            tipo,
                        } => {
                            let data_type_key = DataTypeKey {
                                module_name: module.clone().unwrap_or_default(),
                                defined_type: name.to_string(),
                            };
                        }
                    };

                    // ir_stack.push(IR::Finally {
                    //     count: ,
                    // });
                };
            }
            TypedExpr::If {
                location,
                branches,
                final_else,
                tipo,
            } => todo!(),
            TypedExpr::RecordAccess {
                location,
                tipo,
                label,
                index,
                record,
            } => todo!(),
            TypedExpr::ModuleSelect {
                location,
                tipo,
                label,
                module_name,
                module_alias,
                constructor,
            } => todo!(),
            TypedExpr::Todo {
                location,
                label,
                tipo,
            } => todo!(),
            TypedExpr::RecordUpdate {
                location,
                tipo,
                spread,
                args,
            } => todo!(),
            TypedExpr::Negate { location, value } => todo!(),
        }
    }

    fn define_ir(&self, value_vec: &Vec<IR>, define_vec: &mut Vec<IR>) {
        todo!()
    }

    fn pattern_ir(
        &self,
        pattern: &crate::ast::Pattern<tipo::PatternConstructor, Arc<tipo::Type>>,
        pattern_vec: &mut Vec<IR>,
    ) {
        todo!()
    }
}
