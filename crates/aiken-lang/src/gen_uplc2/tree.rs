use indexmap::IndexSet;
use std::sync::Arc;
use uplc::{builder::EXPECT_ON_LIST, builtins::DefaultFunction};

use crate::{
    ast::{BinOp, Span, UnOp},
    builtins::{data, list, void},
    gen_uplc::air,
    tipo::{Type, ValueConstructor, ValueConstructorVariant},
};

use super::air::Air;

#[derive(Debug, Clone, PartialEq)]
pub enum AirTree {
    Statement {
        statement: AirStatement,
        hoisted_over: Option<Box<AirTree>>,
    },
    Expression(AirExpression),
    UnhoistedSequence(Vec<AirTree>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AirStatement {
    // Assignment
    Let {
        name: String,
        value: Box<AirTree>,
    },
    DefineFunc {
        func_name: String,
        module_name: String,
        params: Vec<String>,
        recursive: bool,
        variant_name: String,
        func_body: Box<AirTree>,
    },
    // Assertions
    AssertConstr {
        constr_index: usize,
        constr: Box<AirTree>,
    },
    AssertBool {
        is_true: bool,
        value: Box<AirTree>,
    },
    // Field Access
    FieldsExpose {
        indices: Vec<(usize, String, Arc<Type>)>,
        check_last_item: bool,
        record: Box<AirTree>,
    },
    // List Access
    ListAccessor {
        tipo: Arc<Type>,
        names: Vec<String>,
        tail: bool,
        check_last_item: bool,
        list: Box<AirTree>,
    },
    ListExpose {
        tipo: Arc<Type>,
        tail_head_names: Vec<(String, String)>,
        tail: Option<(String, String)>,
        list: Box<AirTree>,
    },
    // Tuple Access
    TupleAccessor {
        names: Vec<String>,
        tipo: Arc<Type>,
        check_last_item: bool,
        tuple: Box<AirTree>,
    },
    // Misc.
    NoOp,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AirExpression {
    // Primitives
    Int {
        value: String,
    },
    String {
        value: String,
    },
    ByteArray {
        bytes: Vec<u8>,
    },
    Bool {
        value: bool,
    },
    List {
        tipo: Arc<Type>,
        tail: bool,
        items: Vec<AirTree>,
    },
    Tuple {
        tipo: Arc<Type>,
        items: Vec<AirTree>,
    },
    Void,
    Var {
        constructor: ValueConstructor,
        name: String,
        variant_name: String,
    },
    // Functions
    Call {
        tipo: Arc<Type>,
        func: Box<AirTree>,
        args: Vec<AirTree>,
    },

    Fn {
        params: Vec<String>,
        func_body: Box<AirTree>,
    },
    Builtin {
        func: DefaultFunction,
        tipo: Arc<Type>,
        args: Vec<AirTree>,
    },
    // Operators
    BinOp {
        name: BinOp,
        tipo: Arc<Type>,
        left: Box<AirTree>,
        right: Box<AirTree>,
    },
    UnOp {
        op: UnOp,
        arg: Box<AirTree>,
    },

    UnWrapData {
        tipo: Arc<Type>,
        value: Box<AirTree>,
    },
    WrapData {
        tipo: Arc<Type>,
        value: Box<AirTree>,
    },

    // When
    When {
        tipo: Arc<Type>,
        subject_name: String,
        subject: Box<AirTree>,
        clauses: Box<AirTree>,
    },
    Clause {
        tipo: Arc<Type>,
        subject_name: String,
        complex_clause: bool,
        pattern: Box<AirTree>,
        then: Box<AirTree>,
        otherwise: Box<AirTree>,
    },
    ListClause {
        tipo: Arc<Type>,
        tail_name: String,
        next_tail_name: Option<String>,
        complex_clause: bool,
        then: Box<AirTree>,
        otherwise: Box<AirTree>,
    },
    WrapClause {
        then: Box<AirTree>,
        otherwise: Box<AirTree>,
    },
    TupleClause {
        tipo: Arc<Type>,
        indices: IndexSet<(usize, String)>,
        predefined_indices: IndexSet<(usize, String)>,
        subject_name: String,
        type_count: usize,
        complex_clause: bool,
        then: Box<AirTree>,
        otherwise: Box<AirTree>,
    },
    ClauseGuard {
        subject_name: String,
        tipo: Arc<Type>,
        pattern: Box<AirTree>,
        then: Box<AirTree>,
    },
    ListClauseGuard {
        tipo: Arc<Type>,
        tail_name: String,
        next_tail_name: Option<String>,
        inverse: bool,
        then: Box<AirTree>,
    },
    Finally {
        pattern: Box<AirTree>,
        then: Box<AirTree>,
    },
    // If
    If {
        tipo: Arc<Type>,
        pattern: Box<AirTree>,
        then: Box<AirTree>,
        otherwise: Box<AirTree>,
    },
    // Record Creation
    Constr {
        tag: usize,
        tipo: Arc<Type>,
        args: Vec<AirTree>,
    },
    RecordUpdate {
        highest_index: usize,
        indices: Vec<(usize, Arc<Type>)>,
        tipo: Arc<Type>,
        record: Box<AirTree>,
        args: Vec<AirTree>,
    },
    // Field Access
    RecordAccess {
        field_index: u64,
        tipo: Arc<Type>,
        record: Box<AirTree>,
    },
    // Tuple Access
    TupleIndex {
        tipo: Arc<Type>,
        tuple_index: usize,
        tuple: Box<AirTree>,
    },
    // Misc.
    ErrorTerm {
        tipo: Arc<Type>,
    },
    Trace {
        tipo: Arc<Type>,
        msg: Box<AirTree>,
        then: Box<AirTree>,
    },
    FieldsEmpty {
        constr: Box<AirTree>,
    },
    ListEmpty {
        list: Box<AirTree>,
    },
}

impl AirTree {
    pub fn int(value: impl ToString) -> AirTree {
        AirTree::Expression(AirExpression::Int {
            value: value.to_string(),
        })
    }
    pub fn string(value: impl ToString) -> AirTree {
        AirTree::Expression(AirExpression::String {
            value: value.to_string(),
        })
    }
    pub fn byte_array(bytes: Vec<u8>) -> AirTree {
        AirTree::Expression(AirExpression::ByteArray { bytes })
    }
    pub fn bool(value: bool) -> AirTree {
        AirTree::Expression(AirExpression::Bool { value })
    }
    pub fn list(mut items: Vec<AirTree>, tipo: Arc<Type>, tail: Option<AirTree>) -> AirTree {
        if let Some(tail) = tail {
            items.push(tail);

            AirTree::Expression(AirExpression::List {
                tipo,
                tail: true,
                items,
            })
        } else {
            AirTree::Expression(AirExpression::List {
                tipo,
                tail: false,
                items,
            })
        }
    }
    pub fn tuple(items: Vec<AirTree>, tipo: Arc<Type>) -> AirTree {
        AirTree::Expression(AirExpression::Tuple { tipo, items })
    }
    pub fn void() -> AirTree {
        AirTree::Expression(AirExpression::Void)
    }
    pub fn var(
        constructor: ValueConstructor,
        name: impl ToString,
        variant_name: impl ToString,
    ) -> AirTree {
        AirTree::Expression(AirExpression::Var {
            constructor,
            name: name.to_string(),
            variant_name: variant_name.to_string(),
        })
    }
    pub fn local_var(name: impl ToString, tipo: Arc<Type>) -> AirTree {
        AirTree::Expression(AirExpression::Var {
            constructor: ValueConstructor::public(
                tipo,
                ValueConstructorVariant::LocalVariable {
                    location: Span::empty(),
                },
            ),
            name: name.to_string(),
            variant_name: "".to_string(),
        })
    }
    pub fn call(func: AirTree, tipo: Arc<Type>, args: Vec<AirTree>) -> AirTree {
        AirTree::Expression(AirExpression::Call {
            tipo,
            func: func.into(),
            args,
        })
    }
    pub fn define_func(
        func_name: impl ToString,
        module_name: impl ToString,
        variant_name: impl ToString,
        params: Vec<String>,
        recursive: bool,
        func_body: AirTree,
    ) -> AirTree {
        AirTree::Statement {
            statement: AirStatement::DefineFunc {
                func_name: func_name.to_string(),
                module_name: module_name.to_string(),
                params,
                recursive,
                variant_name: variant_name.to_string(),
                func_body: func_body.into(),
            },
            hoisted_over: None,
        }
    }
    pub fn anon_func(params: Vec<String>, func_body: AirTree) -> AirTree {
        AirTree::Expression(AirExpression::Fn {
            params,
            func_body: func_body.into(),
        })
    }
    pub fn builtin(func: DefaultFunction, tipo: Arc<Type>, args: Vec<AirTree>) -> AirTree {
        AirTree::Expression(AirExpression::Builtin { func, tipo, args })
    }
    pub fn binop(op: BinOp, tipo: Arc<Type>, left: AirTree, right: AirTree) -> AirTree {
        AirTree::Expression(AirExpression::BinOp {
            name: op,
            tipo,
            left: left.into(),
            right: right.into(),
        })
    }
    pub fn unop(op: UnOp, arg: AirTree) -> AirTree {
        AirTree::Expression(AirExpression::UnOp {
            op,
            arg: arg.into(),
        })
    }
    pub fn let_assignment(name: impl ToString, value: AirTree) -> AirTree {
        AirTree::Statement {
            statement: AirStatement::Let {
                name: name.to_string(),
                value: value.into(),
            },
            hoisted_over: None,
        }
    }
    pub fn unwrap_data(value: AirTree, tipo: Arc<Type>) -> AirTree {
        AirTree::Expression(AirExpression::UnWrapData {
            tipo,
            value: value.into(),
        })
    }
    pub fn wrap_data(value: AirTree, tipo: Arc<Type>) -> AirTree {
        AirTree::Expression(AirExpression::WrapData {
            tipo,
            value: value.into(),
        })
    }
    pub fn assert_constr_index(constr_index: usize, constr: AirTree) -> AirTree {
        AirTree::Statement {
            statement: AirStatement::AssertConstr {
                constr_index,
                constr: constr.into(),
            },
            hoisted_over: None,
        }
    }
    pub fn assert_bool(is_true: bool, value: AirTree) -> AirTree {
        AirTree::Statement {
            statement: AirStatement::AssertBool {
                is_true,
                value: value.into(),
            },
            hoisted_over: None,
        }
    }
    pub fn when(
        subject_name: impl ToString,
        tipo: Arc<Type>,
        subject: AirTree,
        clauses: AirTree,
    ) -> AirTree {
        AirTree::Expression(AirExpression::When {
            tipo,
            subject_name: subject_name.to_string(),
            subject: subject.into(),
            clauses: clauses.into(),
        })
    }
    pub fn clause(
        subject_name: impl ToString,
        pattern: AirTree,
        tipo: Arc<Type>,
        then: AirTree,
        otherwise: AirTree,
        complex_clause: bool,
    ) -> AirTree {
        AirTree::Expression(AirExpression::Clause {
            tipo,
            subject_name: subject_name.to_string(),
            complex_clause,
            pattern: pattern.into(),
            then: then.into(),
            otherwise: otherwise.into(),
        })
    }
    pub fn list_clause(
        tail_name: impl ToString,
        tipo: Arc<Type>,
        then: AirTree,
        otherwise: AirTree,
        next_tail_name: Option<String>,
        complex_clause: bool,
    ) -> AirTree {
        AirTree::Expression(AirExpression::ListClause {
            tipo,
            tail_name: tail_name.to_string(),
            next_tail_name,
            complex_clause,
            then: then.into(),
            otherwise: otherwise.into(),
        })
    }
    pub fn tuple_clause(
        subject_name: impl ToString,
        tipo: Arc<Type>,
        indices: IndexSet<(usize, String)>,
        predefined_indices: IndexSet<(usize, String)>,
        then: AirTree,
        otherwise: AirTree,
        complex_clause: bool,
    ) -> AirTree {
        let type_count = tipo.get_inner_types().len();

        AirTree::Expression(AirExpression::TupleClause {
            tipo,
            indices,
            predefined_indices,
            subject_name: subject_name.to_string(),
            type_count,
            complex_clause,
            then: then.into(),
            otherwise: otherwise.into(),
        })
    }
    pub fn wrap_clause(then: AirTree, otherwise: AirTree) -> AirTree {
        AirTree::Expression(AirExpression::WrapClause {
            then: then.into(),
            otherwise: otherwise.into(),
        })
    }
    pub fn clause_guard(
        subject_name: impl ToString,
        pattern: AirTree,
        tipo: Arc<Type>,
        then: AirTree,
    ) -> AirTree {
        AirTree::Expression(AirExpression::ClauseGuard {
            subject_name: subject_name.to_string(),
            tipo,
            pattern: pattern.into(),
            then: then.into(),
        })
    }
    pub fn list_clause_guard(
        tail_name: impl ToString,
        tipo: Arc<Type>,
        inverse: bool,
        then: AirTree,
        next_tail_name: Option<String>,
    ) -> AirTree {
        AirTree::Expression(AirExpression::ListClauseGuard {
            tipo,
            tail_name: tail_name.to_string(),
            next_tail_name,
            inverse,
            then: then.into(),
        })
    }
    pub fn finally(pattern: AirTree, then: AirTree) -> AirTree {
        AirTree::Expression(AirExpression::Finally {
            pattern: pattern.into(),
            then: then.into(),
        })
    }
    pub fn if_branches(
        mut branches: Vec<(AirTree, AirTree)>,
        tipo: Arc<Type>,
        otherwise: AirTree,
    ) -> AirTree {
        assert!(!branches.is_empty());
        let last_if = branches.pop().unwrap();

        let mut final_if = AirTree::Expression(AirExpression::If {
            tipo: tipo.clone(),
            pattern: last_if.0.into(),
            then: last_if.1.into(),
            otherwise: otherwise.into(),
        });

        while let Some(branch) = branches.pop() {
            final_if = AirTree::Expression(AirExpression::If {
                tipo: tipo.clone(),
                pattern: branch.0.into(),
                then: branch.1.into(),
                otherwise: final_if.into(),
            });
        }

        final_if
    }
    pub fn create_constr(tag: usize, tipo: Arc<Type>, args: Vec<AirTree>) -> AirTree {
        AirTree::Expression(AirExpression::Constr { tag, tipo, args })
    }

    pub fn record_update(
        indices: Vec<(usize, Arc<Type>)>,
        highest_index: usize,
        tipo: Arc<Type>,
        record: AirTree,
        args: Vec<AirTree>,
    ) -> AirTree {
        AirTree::Expression(AirExpression::RecordUpdate {
            highest_index,
            indices,
            tipo,
            record: record.into(),
            args,
        })
    }
    pub fn record_access(field_index: u64, tipo: Arc<Type>, record: AirTree) -> AirTree {
        AirTree::Expression(AirExpression::RecordAccess {
            field_index,
            tipo,
            record: record.into(),
        })
    }

    pub fn fields_expose(
        indices: Vec<(usize, String, Arc<Type>)>,
        check_last_item: bool,
        record: AirTree,
    ) -> AirTree {
        AirTree::Statement {
            statement: AirStatement::FieldsExpose {
                indices,
                check_last_item,
                record: record.into(),
            },
            hoisted_over: None,
        }
    }
    pub fn list_access(
        names: Vec<String>,
        tipo: Arc<Type>,
        tail: bool,
        check_last_item: bool,
        list: AirTree,
    ) -> AirTree {
        AirTree::Statement {
            statement: AirStatement::ListAccessor {
                tipo,
                names,
                tail,
                check_last_item,
                list: list.into(),
            },
            hoisted_over: None,
        }
    }
    pub fn list_expose(
        tail_head_names: Vec<(String, String)>,
        tail: Option<(String, String)>,
        tipo: Arc<Type>,
        list: AirTree,
    ) -> AirTree {
        AirTree::Statement {
            statement: AirStatement::ListExpose {
                tipo,
                tail_head_names,
                tail,
                list: list.into(),
            },
            hoisted_over: None,
        }
    }
    pub fn tuple_access(
        names: Vec<String>,
        tipo: Arc<Type>,
        check_last_item: bool,
        tuple: AirTree,
    ) -> AirTree {
        AirTree::Statement {
            statement: AirStatement::TupleAccessor {
                names,
                tipo,
                check_last_item,
                tuple: tuple.into(),
            },
            hoisted_over: None,
        }
    }
    pub fn tuple_index(tuple_index: usize, tipo: Arc<Type>, tuple: AirTree) -> AirTree {
        AirTree::Expression(AirExpression::TupleIndex {
            tipo,
            tuple_index,
            tuple: tuple.into(),
        })
    }
    pub fn error(tipo: Arc<Type>) -> AirTree {
        AirTree::Expression(AirExpression::ErrorTerm { tipo })
    }
    pub fn trace(msg: AirTree, tipo: Arc<Type>, then: AirTree) -> AirTree {
        AirTree::Expression(AirExpression::Trace {
            tipo,
            msg: msg.into(),
            then: then.into(),
        })
    }
    pub fn no_op() -> AirTree {
        AirTree::Statement {
            statement: AirStatement::NoOp,
            hoisted_over: None,
        }
    }
    pub fn fields_empty(constr: AirTree) -> AirTree {
        AirTree::Expression(AirExpression::FieldsEmpty {
            constr: constr.into(),
        })
    }
    pub fn list_empty(list: AirTree) -> AirTree {
        AirTree::Expression(AirExpression::ListEmpty { list: list.into() })
    }
    pub fn hoist_over(mut self, next_exp: AirTree) -> AirTree {
        match &mut self {
            AirTree::Statement { hoisted_over, .. } => {
                assert!(hoisted_over.is_none());
                *hoisted_over = Some(next_exp.into());
                self
            }

            AirTree::Expression(_) => {
                unreachable!("Trying to hoist an expression onto an expression.")
            }
            AirTree::UnhoistedSequence(seq) => {
                let mut final_exp = next_exp;
                while let Some(assign) = seq.pop() {
                    final_exp = assign.hoist_over(final_exp);
                }
                final_exp
            }
        }
    }

    pub fn expect_on_list() -> AirTree {
        let list_var = AirTree::local_var("__list_to_check", list(data()));

        let head_list = AirTree::builtin(DefaultFunction::HeadList, data(), vec![list_var]);

        let expect_on_head = AirTree::call(
            AirTree::local_var("__check_with", void()),
            void(),
            vec![head_list],
        );

        let assign = AirTree::let_assignment("_", expect_on_head);

        let next_call = AirTree::call(
            AirTree::local_var(EXPECT_ON_LIST, void()),
            void(),
            vec![
                AirTree::builtin(
                    DefaultFunction::TailList,
                    list(data()),
                    vec![AirTree::local_var("__list_to_check", list(data()))],
                ),
                AirTree::local_var("__check_with", void()),
            ],
        );

        let list_clause = AirTree::list_clause(
            "__list_to_check",
            void(),
            AirTree::void(),
            assign.hoist_over(next_call),
            None,
            false,
        );

        AirTree::define_func(
            EXPECT_ON_LIST,
            "",
            "",
            vec!["__list_to_check".to_string(), "__check_with".to_string()],
            true,
            list_clause,
        )
    }

    pub fn to_vec(&self) -> Vec<Air> {
        let mut air_vec = vec![];
        self.create_air_vec(&mut air_vec);
        air_vec
    }

    fn create_air_vec(&self, air_vec: &mut Vec<Air>) {
        match self {
            AirTree::Statement {
                statement,
                hoisted_over: Some(exp),
            } => {
                match statement {
                    AirStatement::Let { value, name } => {
                        air_vec.push(Air::Let { name: name.clone() });
                        value.create_air_vec(air_vec);
                    }
                    AirStatement::DefineFunc {
                        func_name,
                        module_name,
                        params,
                        recursive,
                        variant_name,
                        func_body,
                    } => {
                        air_vec.push(Air::DefineFunc {
                            func_name: func_name.clone(),
                            module_name: module_name.clone(),
                            params: params.clone(),
                            recursive: *recursive,
                            variant_name: variant_name.clone(),
                        });
                        func_body.create_air_vec(air_vec);
                    }
                    AirStatement::AssertConstr {
                        constr,
                        constr_index,
                    } => {
                        air_vec.push(Air::AssertConstr {
                            constr_index: *constr_index,
                        });
                        constr.create_air_vec(air_vec);
                    }
                    AirStatement::AssertBool { is_true, value } => {
                        air_vec.push(Air::AssertBool { is_true: *is_true });
                        value.create_air_vec(air_vec);
                    }
                    AirStatement::FieldsExpose {
                        indices,
                        check_last_item,
                        record,
                    } => {
                        air_vec.push(Air::FieldsExpose {
                            indices: indices.clone(),
                            check_last_item: *check_last_item,
                        });
                        record.create_air_vec(air_vec);
                    }
                    AirStatement::ListAccessor {
                        tipo,
                        names,
                        tail,
                        check_last_item,
                        list,
                    } => {
                        air_vec.push(Air::ListAccessor {
                            tipo: tipo.clone(),
                            names: names.clone(),
                            tail: *tail,
                            check_last_item: *check_last_item,
                        });
                        list.create_air_vec(air_vec);
                    }
                    AirStatement::ListExpose {
                        tipo,
                        tail_head_names,
                        tail,
                        list,
                    } => {
                        air_vec.push(Air::ListExpose {
                            tipo: tipo.clone(),
                            tail_head_names: tail_head_names.clone(),
                            tail: tail.clone(),
                        });
                        list.create_air_vec(air_vec);
                    }
                    AirStatement::TupleAccessor {
                        names,
                        tipo,
                        check_last_item,
                        tuple,
                    } => {
                        air_vec.push(Air::TupleAccessor {
                            names: names.clone(),
                            tipo: tipo.clone(),
                            check_last_item: *check_last_item,
                        });
                        tuple.create_air_vec(air_vec);
                    }
                    AirStatement::NoOp => {
                        air_vec.push(Air::NoOp);
                    }
                };
                exp.create_air_vec(air_vec);
            }
            AirTree::Expression(exp) => match exp {
                AirExpression::Int { value } => air_vec.push(Air::Int {
                    value: value.clone(),
                }),
                AirExpression::String { value } => air_vec.push(Air::String {
                    value: value.clone(),
                }),
                AirExpression::ByteArray { bytes } => air_vec.push(Air::ByteArray {
                    bytes: bytes.clone(),
                }),
                AirExpression::Bool { value } => air_vec.push(Air::Bool { value: *value }),
                AirExpression::List { tipo, tail, items } => {
                    air_vec.push(Air::List {
                        count: items.len(),
                        tipo: tipo.clone(),
                        tail: *tail,
                    });
                    for item in items {
                        item.create_air_vec(air_vec);
                    }
                }
                AirExpression::Tuple { tipo, items } => {
                    air_vec.push(Air::Tuple {
                        tipo: tipo.clone(),
                        count: items.len(),
                    });
                    for item in items {
                        item.create_air_vec(air_vec);
                    }
                }
                AirExpression::Void => air_vec.push(Air::Void),
                AirExpression::Var {
                    constructor,
                    name,
                    variant_name,
                } => air_vec.push(Air::Var {
                    constructor: constructor.clone(),
                    name: name.clone(),
                    variant_name: variant_name.clone(),
                }),
                AirExpression::Call { tipo, func, args } => {
                    air_vec.push(Air::Call {
                        count: args.len(),
                        tipo: tipo.clone(),
                    });
                    func.create_air_vec(air_vec);
                    for arg in args {
                        arg.create_air_vec(air_vec);
                    }
                }
                AirExpression::Fn { params, func_body } => {
                    air_vec.push(Air::Fn {
                        params: params.clone(),
                    });
                    func_body.create_air_vec(air_vec);
                }
                AirExpression::Builtin { func, tipo, args } => {
                    air_vec.push(Air::Builtin {
                        count: args.len(),
                        func: *func,
                        tipo: tipo.clone(),
                    });

                    for arg in args {
                        arg.create_air_vec(air_vec);
                    }
                }
                AirExpression::BinOp {
                    name,
                    tipo,
                    left,
                    right,
                } => {
                    air_vec.push(Air::BinOp {
                        name: name.clone(),
                        tipo: tipo.clone(),
                    });
                    left.create_air_vec(air_vec);
                    right.create_air_vec(air_vec);
                }
                AirExpression::UnOp { op, arg } => {
                    air_vec.push(Air::UnOp { op: *op });
                    arg.create_air_vec(air_vec);
                }
                AirExpression::UnWrapData { tipo, value } => {
                    air_vec.push(Air::UnWrapData { tipo: tipo.clone() });
                    value.create_air_vec(air_vec);
                }
                AirExpression::WrapData { tipo, value } => {
                    air_vec.push(Air::WrapData { tipo: tipo.clone() });
                    value.create_air_vec(air_vec);
                }
                AirExpression::When {
                    tipo,
                    subject_name,
                    subject,
                    clauses,
                } => {
                    air_vec.push(Air::When {
                        tipo: tipo.clone(),
                        subject_name: subject_name.clone(),
                    });
                    subject.create_air_vec(air_vec);
                    clauses.create_air_vec(air_vec);
                }
                AirExpression::Clause {
                    tipo,
                    subject_name,
                    complex_clause,
                    pattern,
                    then,
                    otherwise,
                } => {
                    air_vec.push(Air::Clause {
                        tipo: tipo.clone(),
                        subject_name: subject_name.clone(),
                        complex_clause: *complex_clause,
                    });
                    pattern.create_air_vec(air_vec);
                    then.create_air_vec(air_vec);
                    otherwise.create_air_vec(air_vec);
                }
                AirExpression::ListClause {
                    tipo,
                    tail_name,
                    next_tail_name,
                    complex_clause,
                    then,
                    otherwise,
                } => {
                    air_vec.push(Air::ListClause {
                        tipo: tipo.clone(),
                        tail_name: tail_name.clone(),
                        next_tail_name: next_tail_name.clone(),
                        complex_clause: *complex_clause,
                    });
                    then.create_air_vec(air_vec);
                    otherwise.create_air_vec(air_vec);
                }
                AirExpression::WrapClause { then, otherwise } => {
                    air_vec.push(Air::WrapClause);
                    then.create_air_vec(air_vec);
                    otherwise.create_air_vec(air_vec);
                }
                AirExpression::TupleClause {
                    tipo,
                    indices,
                    predefined_indices,
                    subject_name,
                    type_count,
                    complex_clause,
                    then,
                    otherwise,
                } => {
                    air_vec.push(Air::TupleClause {
                        tipo: tipo.clone(),
                        indices: indices.clone(),
                        predefined_indices: predefined_indices.clone(),
                        subject_name: subject_name.clone(),
                        count: *type_count,
                        complex_clause: *complex_clause,
                    });
                    then.create_air_vec(air_vec);
                    otherwise.create_air_vec(air_vec);
                }
                AirExpression::ClauseGuard {
                    subject_name,
                    tipo,
                    pattern,
                    then,
                } => {
                    air_vec.push(Air::ClauseGuard {
                        subject_name: subject_name.clone(),
                        tipo: tipo.clone(),
                    });

                    pattern.create_air_vec(air_vec);
                    then.create_air_vec(air_vec);
                }
                AirExpression::ListClauseGuard {
                    tipo,
                    tail_name,
                    next_tail_name,
                    inverse,
                    then,
                } => {
                    air_vec.push(Air::ListClauseGuard {
                        tipo: tipo.clone(),
                        tail_name: tail_name.clone(),
                        next_tail_name: next_tail_name.clone(),
                        inverse: *inverse,
                    });

                    then.create_air_vec(air_vec);
                }
                AirExpression::Finally { pattern, then } => {
                    air_vec.push(Air::Finally);
                    pattern.create_air_vec(air_vec);
                    then.create_air_vec(air_vec);
                }
                AirExpression::If {
                    tipo,
                    pattern,
                    then,
                    otherwise,
                } => {
                    air_vec.push(Air::If { tipo: tipo.clone() });
                    pattern.create_air_vec(air_vec);
                    then.create_air_vec(air_vec);
                    otherwise.create_air_vec(air_vec);
                }
                AirExpression::Constr { tag, tipo, args } => {
                    air_vec.push(Air::Constr {
                        tag: *tag,
                        tipo: tipo.clone(),
                        count: args.len(),
                    });
                    for arg in args {
                        arg.create_air_vec(air_vec);
                    }
                }
                AirExpression::RecordUpdate {
                    highest_index,
                    indices,
                    tipo,
                    record,
                    args,
                } => {
                    air_vec.push(Air::RecordUpdate {
                        highest_index: *highest_index,
                        indices: indices.clone(),
                        tipo: tipo.clone(),
                    });
                    record.create_air_vec(air_vec);
                    for arg in args {
                        arg.create_air_vec(air_vec);
                    }
                }
                AirExpression::RecordAccess {
                    field_index,
                    tipo,
                    record,
                } => {
                    air_vec.push(Air::RecordAccess {
                        record_index: *field_index,
                        tipo: tipo.clone(),
                    });
                    record.create_air_vec(air_vec);
                }
                AirExpression::TupleIndex {
                    tipo,
                    tuple_index,
                    tuple,
                } => {
                    air_vec.push(Air::TupleIndex {
                        tipo: tipo.clone(),
                        tuple_index: *tuple_index,
                    });
                    tuple.create_air_vec(air_vec);
                }
                AirExpression::ErrorTerm { tipo } => {
                    air_vec.push(Air::ErrorTerm { tipo: tipo.clone() })
                }
                AirExpression::Trace { tipo, msg, then } => {
                    air_vec.push(Air::Trace { tipo: tipo.clone() });
                    msg.create_air_vec(air_vec);
                    then.create_air_vec(air_vec);
                }
                AirExpression::FieldsEmpty { constr } => {
                    air_vec.push(Air::FieldsEmpty);
                    constr.create_air_vec(air_vec);
                }
                AirExpression::ListEmpty { list } => {
                    air_vec.push(Air::ListEmpty);
                    list.create_air_vec(air_vec);
                }
            },
            AirTree::UnhoistedSequence(_) => {
                unreachable!("FIRST RESOLVE ALL UNHOISTED SEQUENCES")
            }
            _ => unreachable!("FOUND UNHOISTED STATEMENT"),
        }
    }
}
