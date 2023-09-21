use indexmap::IndexSet;
use itertools::Itertools;
use std::{borrow::BorrowMut, rc::Rc, slice::Iter};
use uplc::{builder::EXPECT_ON_LIST, builtins::DefaultFunction};

use crate::{
    ast::{BinOp, Span, UnOp},
    builtins::{bool, byte_array, data, int, list, string, void},
    tipo::{Type, ValueConstructor, ValueConstructorVariant},
};

use super::air::Air;

#[derive(Clone, Debug, PartialEq)]
pub struct TreePath {
    path: Vec<(usize, usize)>,
}

impl TreePath {
    pub fn new() -> Self {
        TreePath { path: vec![] }
    }

    pub fn is_empty(&self) -> bool {
        self.path.is_empty()
    }

    pub fn push(&mut self, depth: usize, index: usize) {
        self.path.push((depth, index));
    }

    pub fn pop(&mut self) -> Option<(usize, usize)> {
        self.path.pop()
    }

    pub fn common_ancestor(&self, other: &Self) -> Self {
        let mut common_ancestor = TreePath::new();

        let mut self_iter = self.path.iter();
        let mut other_iter = other.path.iter();

        let mut self_next = self_iter.next();
        let mut other_next = other_iter.next();

        while self_next.is_some() && other_next.is_some() {
            let self_next_level = self_next.unwrap();
            let other_next_level = other_next.unwrap();

            if self_next_level == other_next_level {
                common_ancestor.push(self_next_level.0, self_next_level.1);
            } else {
                break;
            }

            self_next = self_iter.next();
            other_next = other_iter.next();
        }

        common_ancestor
    }
}

impl Default for TreePath {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct IndexCounter {
    current_index: usize,
}

impl IndexCounter {
    pub fn new() -> Self {
        IndexCounter { current_index: 0 }
    }

    /// Returns the next of this [`IndexCounter`].
    pub fn next_number(&mut self) -> usize {
        let current_index = self.current_index;
        self.current_index += 1;
        current_index
    }
}

impl Default for IndexCounter {
    fn default() -> Self {
        Self::new()
    }
}

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
        recursive_nonstatic_params: Vec<String>,
        variant_name: String,
        func_body: Box<AirTree>,
    },
    DefineCyclicFuncs {
        func_name: String,
        module_name: String,
        variant_name: String,
        // params and body
        contained_functions: Vec<(Vec<String>, AirTree)>,
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
    // Clause Guards
    ClauseGuard {
        subject_name: String,
        subject_tipo: Rc<Type>,
        pattern: Box<AirTree>,
    },
    ListClauseGuard {
        subject_tipo: Rc<Type>,
        tail_name: String,
        next_tail_name: Option<String>,
        inverse: bool,
    },
    TupleGuard {
        subject_tipo: Rc<Type>,
        indices: IndexSet<(usize, String)>,
        subject_name: String,
    },
    // Field Access
    FieldsExpose {
        indices: Vec<(usize, String, Rc<Type>)>,
        check_last_item: bool,
        record: Box<AirTree>,
    },
    // List Access
    ListAccessor {
        tipo: Rc<Type>,
        names: Vec<String>,
        tail: bool,
        check_last_item: bool,
        list: Box<AirTree>,
    },
    ListExpose {
        tipo: Rc<Type>,
        tail_head_names: Vec<(String, String)>,
        tail: Option<(String, String)>,
    },
    // Tuple Access
    TupleAccessor {
        names: Vec<String>,
        tipo: Rc<Type>,
        check_last_item: bool,
        tuple: Box<AirTree>,
    },
    // Misc.
    FieldsEmpty {
        constr: Box<AirTree>,
    },
    ListEmpty {
        list: Box<AirTree>,
    },
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
        tipo: Rc<Type>,
        tail: bool,
        items: Vec<AirTree>,
    },
    Tuple {
        tipo: Rc<Type>,
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
        tipo: Rc<Type>,
        func: Box<AirTree>,
        args: Vec<AirTree>,
    },

    Fn {
        params: Vec<String>,
        func_body: Box<AirTree>,
    },
    Builtin {
        func: DefaultFunction,
        tipo: Rc<Type>,
        args: Vec<AirTree>,
    },
    // Operators
    BinOp {
        name: BinOp,
        tipo: Rc<Type>,
        left: Box<AirTree>,
        right: Box<AirTree>,
        argument_tipo: Rc<Type>,
    },
    UnOp {
        op: UnOp,
        arg: Box<AirTree>,
    },

    CastFromData {
        tipo: Rc<Type>,
        value: Box<AirTree>,
    },
    CastToData {
        tipo: Rc<Type>,
        value: Box<AirTree>,
    },

    // When
    When {
        tipo: Rc<Type>,
        subject_name: String,
        subject: Box<AirTree>,
        subject_tipo: Rc<Type>,
        clauses: Box<AirTree>,
    },
    Clause {
        subject_tipo: Rc<Type>,
        subject_name: String,
        complex_clause: bool,
        pattern: Box<AirTree>,
        then: Box<AirTree>,
        otherwise: Box<AirTree>,
    },
    ListClause {
        subject_tipo: Rc<Type>,
        tail_name: String,
        next_tail_name: Option<(String, String)>,
        complex_clause: bool,
        then: Box<AirTree>,
        otherwise: Box<AirTree>,
    },
    WrapClause {
        then: Box<AirTree>,
        otherwise: Box<AirTree>,
    },
    TupleClause {
        subject_tipo: Rc<Type>,
        indices: IndexSet<(usize, String)>,
        predefined_indices: IndexSet<(usize, String)>,
        subject_name: String,
        complex_clause: bool,
        then: Box<AirTree>,
        otherwise: Box<AirTree>,
    },

    Finally {
        pattern: Box<AirTree>,
        then: Box<AirTree>,
    },
    // If
    If {
        tipo: Rc<Type>,
        pattern: Box<AirTree>,
        then: Box<AirTree>,
        otherwise: Box<AirTree>,
    },
    // Record Creation
    Constr {
        tag: usize,
        tipo: Rc<Type>,
        args: Vec<AirTree>,
    },
    RecordUpdate {
        highest_index: usize,
        indices: Vec<(usize, Rc<Type>)>,
        tipo: Rc<Type>,
        record: Box<AirTree>,
        args: Vec<AirTree>,
    },
    // Field Access
    RecordAccess {
        field_index: u64,
        tipo: Rc<Type>,
        record: Box<AirTree>,
    },
    // Tuple Access
    TupleIndex {
        tipo: Rc<Type>,
        tuple_index: usize,
        tuple: Box<AirTree>,
    },
    // Misc.
    ErrorTerm {
        tipo: Rc<Type>,
    },
    Trace {
        tipo: Rc<Type>,
        msg: Box<AirTree>,
        then: Box<AirTree>,
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
    pub fn list(mut items: Vec<AirTree>, tipo: Rc<Type>, tail: Option<AirTree>) -> AirTree {
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
    pub fn tuple(items: Vec<AirTree>, tipo: Rc<Type>) -> AirTree {
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
    pub fn local_var(name: impl ToString, tipo: Rc<Type>) -> AirTree {
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
    pub fn call(func: AirTree, tipo: Rc<Type>, args: Vec<AirTree>) -> AirTree {
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
        recursive_nonstatic_params: Vec<String>,
        func_body: AirTree,
    ) -> AirTree {
        AirTree::Statement {
            statement: AirStatement::DefineFunc {
                func_name: func_name.to_string(),
                module_name: module_name.to_string(),
                params,
                recursive,
                recursive_nonstatic_params,
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
    pub fn builtin(func: DefaultFunction, tipo: Rc<Type>, args: Vec<AirTree>) -> AirTree {
        AirTree::Expression(AirExpression::Builtin { func, tipo, args })
    }
    pub fn binop(
        op: BinOp,
        tipo: Rc<Type>,
        left: AirTree,
        right: AirTree,
        argument_tipo: Rc<Type>,
    ) -> AirTree {
        AirTree::Expression(AirExpression::BinOp {
            name: op,
            tipo,
            left: left.into(),
            right: right.into(),
            argument_tipo,
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
    pub fn cast_from_data(value: AirTree, tipo: Rc<Type>) -> AirTree {
        AirTree::Expression(AirExpression::CastFromData {
            tipo,
            value: value.into(),
        })
    }
    pub fn cast_to_data(value: AirTree, tipo: Rc<Type>) -> AirTree {
        AirTree::Expression(AirExpression::CastToData {
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
        tipo: Rc<Type>,
        subject_tipo: Rc<Type>,
        subject: AirTree,
        clauses: AirTree,
    ) -> AirTree {
        AirTree::Expression(AirExpression::When {
            tipo,
            subject_name: subject_name.to_string(),
            subject: subject.into(),
            subject_tipo,
            clauses: clauses.into(),
        })
    }
    pub fn clause(
        subject_name: impl ToString,
        pattern: AirTree,
        subject_tipo: Rc<Type>,
        then: AirTree,
        otherwise: AirTree,
        complex_clause: bool,
    ) -> AirTree {
        AirTree::Expression(AirExpression::Clause {
            subject_tipo,
            subject_name: subject_name.to_string(),
            complex_clause,
            pattern: pattern.into(),
            then: then.into(),
            otherwise: otherwise.into(),
        })
    }
    pub fn list_clause(
        tail_name: impl ToString,
        subject_tipo: Rc<Type>,
        then: AirTree,
        otherwise: AirTree,
        next_tail_name: Option<(String, String)>,
        complex_clause: bool,
    ) -> AirTree {
        AirTree::Expression(AirExpression::ListClause {
            subject_tipo,
            tail_name: tail_name.to_string(),
            next_tail_name,
            complex_clause,
            then: then.into(),
            otherwise: otherwise.into(),
        })
    }
    pub fn tuple_clause(
        subject_name: impl ToString,
        subject_tipo: Rc<Type>,
        indices: IndexSet<(usize, String)>,
        predefined_indices: IndexSet<(usize, String)>,
        then: AirTree,
        otherwise: AirTree,
        complex_clause: bool,
    ) -> AirTree {
        AirTree::Expression(AirExpression::TupleClause {
            subject_tipo,
            indices,
            predefined_indices,
            subject_name: subject_name.to_string(),
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
        subject_tipo: Rc<Type>,
    ) -> AirTree {
        AirTree::Statement {
            statement: AirStatement::ClauseGuard {
                subject_name: subject_name.to_string(),
                subject_tipo,
                pattern: pattern.into(),
            },
            hoisted_over: None,
        }
    }
    pub fn list_clause_guard(
        tail_name: impl ToString,
        subject_tipo: Rc<Type>,
        inverse: bool,
        next_tail_name: Option<String>,
    ) -> AirTree {
        AirTree::Statement {
            statement: AirStatement::ListClauseGuard {
                subject_tipo,
                tail_name: tail_name.to_string(),
                next_tail_name,
                inverse,
            },
            hoisted_over: None,
        }
    }
    pub fn tuple_clause_guard(
        subject_name: impl ToString,
        subject_tipo: Rc<Type>,
        indices: IndexSet<(usize, String)>,
    ) -> AirTree {
        AirTree::Statement {
            statement: AirStatement::TupleGuard {
                indices,
                subject_name: subject_name.to_string(),
                subject_tipo,
            },
            hoisted_over: None,
        }
    }
    pub fn finally(pattern: AirTree, then: AirTree) -> AirTree {
        AirTree::Expression(AirExpression::Finally {
            pattern: pattern.into(),
            then: then.into(),
        })
    }
    pub fn if_branches(
        mut branches: Vec<(AirTree, AirTree)>,
        tipo: Rc<Type>,
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
    pub fn create_constr(tag: usize, tipo: Rc<Type>, args: Vec<AirTree>) -> AirTree {
        AirTree::Expression(AirExpression::Constr { tag, tipo, args })
    }

    pub fn record_update(
        indices: Vec<(usize, Rc<Type>)>,
        highest_index: usize,
        tipo: Rc<Type>,
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
    pub fn record_access(field_index: u64, tipo: Rc<Type>, record: AirTree) -> AirTree {
        AirTree::Expression(AirExpression::RecordAccess {
            field_index,
            tipo,
            record: record.into(),
        })
    }

    pub fn fields_expose(
        indices: Vec<(usize, String, Rc<Type>)>,
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
        tipo: Rc<Type>,
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
        tipo: Rc<Type>,
    ) -> AirTree {
        AirTree::Statement {
            statement: AirStatement::ListExpose {
                tipo,
                tail_head_names,
                tail,
            },
            hoisted_over: None,
        }
    }
    pub fn tuple_access(
        names: Vec<String>,
        tipo: Rc<Type>,
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
    pub fn tuple_index(tuple_index: usize, tipo: Rc<Type>, tuple: AirTree) -> AirTree {
        AirTree::Expression(AirExpression::TupleIndex {
            tipo,
            tuple_index,
            tuple: tuple.into(),
        })
    }
    pub fn error(tipo: Rc<Type>) -> AirTree {
        AirTree::Expression(AirExpression::ErrorTerm { tipo })
    }
    pub fn trace(msg: AirTree, tipo: Rc<Type>, then: AirTree) -> AirTree {
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
        AirTree::Statement {
            statement: AirStatement::FieldsEmpty {
                constr: constr.into(),
            },
            hoisted_over: None,
        }
    }
    pub fn list_empty(list: AirTree) -> AirTree {
        AirTree::Statement {
            statement: AirStatement::ListEmpty { list: list.into() },
            hoisted_over: None,
        }
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
            AirTree::var(
                ValueConstructor::public(
                    void(),
                    ValueConstructorVariant::ModuleFn {
                        name: EXPECT_ON_LIST.to_string(),
                        field_map: None,
                        module: "".to_string(),
                        arity: 1,
                        location: Span::empty(),
                        builtin: None,
                    },
                ),
                EXPECT_ON_LIST,
                "",
            ),
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

        AirTree::list_clause(
            "__list_to_check",
            void(),
            AirTree::void(),
            assign.hoist_over(next_call),
            None,
            false,
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
                        recursive_nonstatic_params,
                        variant_name,
                        func_body,
                    } => {
                        air_vec.push(Air::DefineFunc {
                            func_name: func_name.clone(),
                            module_name: module_name.clone(),
                            params: params.clone(),
                            recursive: *recursive,
                            recursive_nonstatic_params: recursive_nonstatic_params.clone(),
                            variant_name: variant_name.clone(),
                        });
                        func_body.create_air_vec(air_vec);
                    }
                    AirStatement::DefineCyclicFuncs {
                        func_name,
                        module_name,
                        variant_name,
                        contained_functions,
                    } => {
                        air_vec.push(Air::DefineCyclicFuncs {
                            func_name: func_name.clone(),
                            module_name: module_name.clone(),
                            variant_name: variant_name.clone(),
                            contained_functions: contained_functions
                                .iter()
                                .map(|(params, _)| params.clone())
                                .collect_vec(),
                        });

                        for (_, func_body) in contained_functions {
                            func_body.create_air_vec(air_vec);
                        }
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
                    AirStatement::ClauseGuard {
                        subject_name,
                        subject_tipo,
                        pattern,
                    } => {
                        air_vec.push(Air::ClauseGuard {
                            subject_name: subject_name.clone(),
                            subject_tipo: subject_tipo.clone(),
                        });

                        pattern.create_air_vec(air_vec);
                    }
                    AirStatement::ListClauseGuard {
                        subject_tipo,
                        tail_name,
                        next_tail_name,
                        inverse,
                    } => {
                        air_vec.push(Air::ListClauseGuard {
                            subject_tipo: subject_tipo.clone(),
                            tail_name: tail_name.clone(),
                            next_tail_name: next_tail_name.clone(),
                            inverse: *inverse,
                        });
                    }
                    AirStatement::TupleGuard {
                        subject_tipo,
                        indices,
                        subject_name,
                    } => {
                        air_vec.push(Air::TupleGuard {
                            subject_tipo: subject_tipo.clone(),
                            indices: indices.clone(),
                            subject_name: subject_name.clone(),
                        });
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
                    } => {
                        air_vec.push(Air::ListExpose {
                            tipo: tipo.clone(),
                            tail_head_names: tail_head_names.clone(),
                            tail: tail.clone(),
                        });
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
                    AirStatement::FieldsEmpty { constr } => {
                        air_vec.push(Air::FieldsEmpty);
                        constr.create_air_vec(air_vec);
                    }
                    AirStatement::ListEmpty { list } => {
                        air_vec.push(Air::ListEmpty);
                        list.create_air_vec(air_vec);
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
                    argument_tipo,
                } => {
                    air_vec.push(Air::BinOp {
                        name: *name,
                        tipo: tipo.clone(),
                        argument_tipo: argument_tipo.clone(),
                    });
                    left.create_air_vec(air_vec);
                    right.create_air_vec(air_vec);
                }
                AirExpression::UnOp { op, arg } => {
                    air_vec.push(Air::UnOp { op: *op });
                    arg.create_air_vec(air_vec);
                }
                AirExpression::CastFromData { tipo, value } => {
                    air_vec.push(Air::CastFromData { tipo: tipo.clone() });
                    value.create_air_vec(air_vec);
                }
                AirExpression::CastToData { tipo, value } => {
                    air_vec.push(Air::CastToData { tipo: tipo.clone() });
                    value.create_air_vec(air_vec);
                }
                AirExpression::When {
                    tipo,
                    subject_name,
                    subject,
                    subject_tipo,
                    clauses,
                } => {
                    air_vec.push(Air::When {
                        tipo: tipo.clone(),
                        subject_name: subject_name.clone(),
                        subject_tipo: subject_tipo.clone(),
                    });
                    subject.create_air_vec(air_vec);
                    clauses.create_air_vec(air_vec);
                }
                AirExpression::Clause {
                    subject_tipo,
                    subject_name,
                    complex_clause,
                    pattern,
                    then,
                    otherwise,
                } => {
                    air_vec.push(Air::Clause {
                        subject_tipo: subject_tipo.clone(),
                        subject_name: subject_name.clone(),
                        complex_clause: *complex_clause,
                    });
                    pattern.create_air_vec(air_vec);
                    then.create_air_vec(air_vec);
                    otherwise.create_air_vec(air_vec);
                }
                AirExpression::ListClause {
                    subject_tipo,
                    tail_name,
                    next_tail_name,
                    complex_clause,
                    then,
                    otherwise,
                } => {
                    air_vec.push(Air::ListClause {
                        subject_tipo: subject_tipo.clone(),
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
                    subject_tipo,
                    indices,
                    predefined_indices,
                    subject_name,
                    complex_clause,
                    then,
                    otherwise,
                } => {
                    air_vec.push(Air::TupleClause {
                        subject_tipo: subject_tipo.clone(),
                        indices: indices.clone(),
                        predefined_indices: predefined_indices.clone(),
                        subject_name: subject_name.clone(),
                        complex_clause: *complex_clause,
                    });
                    then.create_air_vec(air_vec);
                    otherwise.create_air_vec(air_vec);
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
            },
            AirTree::UnhoistedSequence(_) => {
                unreachable!("FIRST RESOLVE ALL UNHOISTED SEQUENCES")
            }
            _ => unreachable!("FOUND UNHOISTED STATEMENT"),
        }
    }

    pub fn return_type(&self) -> Rc<Type> {
        match self {
            AirTree::Statement {
                hoisted_over: Some(hoisted_over),
                ..
            } => hoisted_over.return_type(),
            AirTree::Expression(e) => match e {
                AirExpression::Int { .. } => int(),
                AirExpression::String { .. } => string(),
                AirExpression::ByteArray { .. } => byte_array(),
                AirExpression::Bool { .. } => bool(),
                AirExpression::List { tipo, .. }
                | AirExpression::Tuple { tipo, .. }
                | AirExpression::Call { tipo, .. }
                | AirExpression::Builtin { tipo, .. }
                | AirExpression::BinOp { tipo, .. }
                | AirExpression::CastFromData { tipo, .. }
                | AirExpression::When { tipo, .. }
                | AirExpression::If { tipo, .. }
                | AirExpression::Constr { tipo, .. }
                | AirExpression::RecordUpdate { tipo, .. }
                | AirExpression::RecordAccess { tipo, .. }
                | AirExpression::TupleIndex { tipo, .. }
                | AirExpression::ErrorTerm { tipo }
                | AirExpression::Trace { tipo, .. } => tipo.clone(),
                AirExpression::Void => void(),
                AirExpression::Var { constructor, .. } => constructor.tipo.clone(),
                AirExpression::Fn { func_body, .. } => func_body.return_type(),
                AirExpression::UnOp { op, .. } => match op {
                    UnOp::Not => bool(),
                    UnOp::Negate => int(),
                },
                AirExpression::CastToData { .. } => data(),
                AirExpression::Clause { then, .. }
                | AirExpression::ListClause { then, .. }
                | AirExpression::WrapClause { then, .. }
                | AirExpression::TupleClause { then, .. }
                | AirExpression::Finally { then, .. } => then.return_type(),
            },
            _ => unreachable!(),
        }
    }

    pub fn mut_held_types(&mut self) -> Vec<&mut Rc<Type>> {
        match self {
            AirTree::Statement {
                statement,
                hoisted_over: Some(_),
            } => match statement {
                AirStatement::ClauseGuard { subject_tipo, .. }
                | AirStatement::ListClauseGuard { subject_tipo, .. }
                | AirStatement::TupleGuard { subject_tipo, .. } => vec![subject_tipo],
                AirStatement::ListAccessor { tipo, .. }
                | AirStatement::ListExpose { tipo, .. }
                | AirStatement::TupleAccessor { tipo, .. } => vec![tipo],
                AirStatement::FieldsExpose { indices, .. } => {
                    let mut types = vec![];
                    for (_, _, tipo) in indices {
                        types.push(tipo);
                    }
                    types
                }
                _ => vec![],
            },
            AirTree::Expression(e) => match e {
                AirExpression::List { tipo, .. }
                | AirExpression::Tuple { tipo, .. }
                | AirExpression::Call { tipo, .. }
                | AirExpression::Builtin { tipo, .. }
                | AirExpression::CastFromData { tipo, .. }
                | AirExpression::CastToData { tipo, .. }
                | AirExpression::If { tipo, .. }
                | AirExpression::RecordAccess { tipo, .. }
                | AirExpression::Constr { tipo, .. }
                | AirExpression::TupleIndex { tipo, .. }
                | AirExpression::ErrorTerm { tipo }
                | AirExpression::Trace { tipo, .. } => vec![tipo],
                AirExpression::Var { constructor, .. } => {
                    vec![constructor.tipo.borrow_mut()]
                }
                AirExpression::BinOp {
                    tipo,
                    argument_tipo,
                    ..
                } => {
                    vec![tipo, argument_tipo]
                }
                AirExpression::When {
                    tipo, subject_tipo, ..
                } => vec![tipo, subject_tipo],
                AirExpression::Clause { subject_tipo, .. }
                | AirExpression::ListClause { subject_tipo, .. }
                | AirExpression::TupleClause { subject_tipo, .. } => vec![subject_tipo],

                AirExpression::RecordUpdate { tipo, indices, .. } => {
                    let mut types = vec![tipo];
                    for (_, tipo) in indices {
                        types.push(tipo);
                    }
                    types
                }
                _ => {
                    vec![]
                }
            },
            _ => unreachable!("FOUND UNHOISTED STATEMENT"),
        }
    }

    pub fn traverse_tree_with(
        &mut self,
        with: &mut impl FnMut(&mut AirTree, &TreePath),
        apply_with_last: bool,
    ) {
        let mut tree_path = TreePath::new();
        self.do_traverse_tree_with(&mut tree_path, 0, 0, with, apply_with_last);
    }

    pub fn traverse_tree_with_path(
        &mut self,
        path: &mut TreePath,
        current_depth: usize,
        depth_index: usize,
        with: &mut impl FnMut(&mut AirTree, &TreePath),
        apply_with_last: bool,
    ) {
        self.do_traverse_tree_with(path, current_depth, depth_index, with, apply_with_last);
    }

    fn do_traverse_tree_with(
        &mut self,
        tree_path: &mut TreePath,
        current_depth: usize,
        depth_index: usize,
        with: &mut impl FnMut(&mut AirTree, &TreePath),
        apply_with_last: bool,
    ) {
        let mut index_count = IndexCounter::new();
        tree_path.push(current_depth, depth_index);

        if let AirTree::Statement { statement, .. } = self {
            match statement {
                AirStatement::Let { value, .. } => {
                    value.do_traverse_tree_with(
                        tree_path,
                        current_depth + 1,
                        index_count.next_number(),
                        with,
                        apply_with_last,
                    );
                }
                AirStatement::DefineFunc { func_body, .. } => {
                    func_body.do_traverse_tree_with(
                        tree_path,
                        current_depth + 1,
                        index_count.next_number(),
                        with,
                        apply_with_last,
                    );
                }
                AirStatement::DefineCyclicFuncs {
                    contained_functions,
                    ..
                } => {
                    for (_, func_body) in contained_functions {
                        func_body.do_traverse_tree_with(
                            tree_path,
                            current_depth + 1,
                            index_count.next_number(),
                            with,
                            apply_with_last,
                        );
                    }
                }

                AirStatement::AssertConstr { constr, .. } => {
                    constr.do_traverse_tree_with(
                        tree_path,
                        current_depth + 1,
                        index_count.next_number(),
                        with,
                        apply_with_last,
                    );
                }
                AirStatement::AssertBool { value, .. } => {
                    value.do_traverse_tree_with(
                        tree_path,
                        current_depth + 1,
                        index_count.next_number(),
                        with,
                        apply_with_last,
                    );
                }
                AirStatement::ClauseGuard { pattern, .. } => {
                    pattern.do_traverse_tree_with(
                        tree_path,
                        current_depth + 1,
                        index_count.next_number(),
                        with,
                        apply_with_last,
                    );
                }
                AirStatement::ListClauseGuard { .. } => {}
                AirStatement::TupleGuard { .. } => {}
                AirStatement::FieldsExpose { record, .. } => {
                    record.do_traverse_tree_with(
                        tree_path,
                        current_depth + 1,
                        index_count.next_number(),
                        with,
                        apply_with_last,
                    );
                }
                AirStatement::ListAccessor { list, .. } => {
                    list.do_traverse_tree_with(
                        tree_path,
                        current_depth + 1,
                        index_count.next_number(),
                        with,
                        apply_with_last,
                    );
                }
                AirStatement::ListExpose { .. } => {}
                AirStatement::TupleAccessor { tuple, .. } => {
                    tuple.do_traverse_tree_with(
                        tree_path,
                        current_depth + 1,
                        index_count.next_number(),
                        with,
                        apply_with_last,
                    );
                }
                AirStatement::NoOp => {}
                AirStatement::FieldsEmpty { constr } => {
                    constr.do_traverse_tree_with(
                        tree_path,
                        current_depth + 1,
                        index_count.next_number(),
                        with,
                        apply_with_last,
                    );
                }
                AirStatement::ListEmpty { list } => {
                    list.do_traverse_tree_with(
                        tree_path,
                        current_depth + 1,
                        index_count.next_number(),
                        with,
                        apply_with_last,
                    );
                }
            };
        }

        if !apply_with_last {
            with(self, tree_path);
        }

        match self {
            AirTree::Statement {
                hoisted_over: Some(hoisted_over),
                ..
            } => {
                hoisted_over.do_traverse_tree_with(
                    tree_path,
                    current_depth + 1,
                    index_count.next_number(),
                    with,
                    apply_with_last,
                );
            }
            AirTree::Expression(e) => match e {
                AirExpression::List { items, .. } => {
                    for item in items {
                        item.do_traverse_tree_with(
                            tree_path,
                            current_depth + 1,
                            index_count.next_number(),
                            with,
                            apply_with_last,
                        );
                    }
                }
                AirExpression::Tuple { items, .. } => {
                    for item in items {
                        item.do_traverse_tree_with(
                            tree_path,
                            current_depth + 1,
                            index_count.next_number(),
                            with,
                            apply_with_last,
                        );
                    }
                }
                AirExpression::Call { func, args, .. } => {
                    func.do_traverse_tree_with(
                        tree_path,
                        current_depth + 1,
                        index_count.next_number(),
                        with,
                        apply_with_last,
                    );

                    for arg in args {
                        arg.do_traverse_tree_with(
                            tree_path,
                            current_depth + 1,
                            index_count.next_number(),
                            with,
                            apply_with_last,
                        );
                    }
                }
                AirExpression::Fn { func_body, .. } => {
                    func_body.do_traverse_tree_with(
                        tree_path,
                        current_depth + 1,
                        index_count.next_number(),
                        with,
                        apply_with_last,
                    );
                }
                AirExpression::Builtin { args, .. } => {
                    for arg in args {
                        arg.do_traverse_tree_with(
                            tree_path,
                            current_depth + 1,
                            index_count.next_number(),
                            with,
                            apply_with_last,
                        );
                    }
                }
                AirExpression::BinOp { left, right, .. } => {
                    left.do_traverse_tree_with(
                        tree_path,
                        current_depth + 1,
                        index_count.next_number(),
                        with,
                        apply_with_last,
                    );

                    right.do_traverse_tree_with(
                        tree_path,
                        current_depth + 1,
                        index_count.next_number(),
                        with,
                        apply_with_last,
                    );
                }
                AirExpression::UnOp { arg, .. } => {
                    arg.do_traverse_tree_with(
                        tree_path,
                        current_depth + 1,
                        index_count.next_number(),
                        with,
                        apply_with_last,
                    );
                }
                AirExpression::CastFromData { value, .. } => {
                    value.do_traverse_tree_with(
                        tree_path,
                        current_depth + 1,
                        index_count.next_number(),
                        with,
                        apply_with_last,
                    );
                }
                AirExpression::CastToData { value, .. } => {
                    value.do_traverse_tree_with(
                        tree_path,
                        current_depth + 1,
                        index_count.next_number(),
                        with,
                        apply_with_last,
                    );
                }
                AirExpression::When {
                    subject, clauses, ..
                } => {
                    subject.do_traverse_tree_with(
                        tree_path,
                        current_depth + 1,
                        index_count.next_number(),
                        with,
                        apply_with_last,
                    );

                    clauses.do_traverse_tree_with(
                        tree_path,
                        current_depth + 1,
                        index_count.next_number(),
                        with,
                        apply_with_last,
                    );
                }
                AirExpression::Clause {
                    pattern,
                    then,
                    otherwise,
                    ..
                } => {
                    pattern.do_traverse_tree_with(
                        tree_path,
                        current_depth + 1,
                        index_count.next_number(),
                        with,
                        apply_with_last,
                    );

                    then.do_traverse_tree_with(
                        tree_path,
                        current_depth + 1,
                        index_count.next_number(),
                        with,
                        apply_with_last,
                    );

                    otherwise.do_traverse_tree_with(
                        tree_path,
                        current_depth + 1,
                        index_count.next_number(),
                        with,
                        apply_with_last,
                    );
                }
                AirExpression::ListClause {
                    then, otherwise, ..
                } => {
                    then.do_traverse_tree_with(
                        tree_path,
                        current_depth + 1,
                        index_count.next_number(),
                        with,
                        apply_with_last,
                    );

                    otherwise.do_traverse_tree_with(
                        tree_path,
                        current_depth + 1,
                        index_count.next_number(),
                        with,
                        apply_with_last,
                    );
                }
                AirExpression::WrapClause { then, otherwise } => {
                    then.do_traverse_tree_with(
                        tree_path,
                        current_depth + 1,
                        index_count.next_number(),
                        with,
                        apply_with_last,
                    );

                    otherwise.do_traverse_tree_with(
                        tree_path,
                        current_depth + 1,
                        index_count.next_number(),
                        with,
                        apply_with_last,
                    );
                }
                AirExpression::TupleClause {
                    then, otherwise, ..
                } => {
                    then.do_traverse_tree_with(
                        tree_path,
                        current_depth + 1,
                        index_count.next_number(),
                        with,
                        apply_with_last,
                    );

                    otherwise.do_traverse_tree_with(
                        tree_path,
                        current_depth + 1,
                        index_count.next_number(),
                        with,
                        apply_with_last,
                    );
                }
                AirExpression::Finally { pattern, then } => {
                    pattern.do_traverse_tree_with(
                        tree_path,
                        current_depth + 1,
                        index_count.next_number(),
                        with,
                        apply_with_last,
                    );

                    then.do_traverse_tree_with(
                        tree_path,
                        current_depth + 1,
                        index_count.next_number(),
                        with,
                        apply_with_last,
                    );
                }
                AirExpression::If {
                    pattern,
                    then,
                    otherwise,
                    ..
                } => {
                    pattern.do_traverse_tree_with(
                        tree_path,
                        current_depth + 1,
                        index_count.next_number(),
                        with,
                        apply_with_last,
                    );

                    then.do_traverse_tree_with(
                        tree_path,
                        current_depth + 1,
                        index_count.next_number(),
                        with,
                        apply_with_last,
                    );

                    otherwise.do_traverse_tree_with(
                        tree_path,
                        current_depth + 1,
                        index_count.next_number(),
                        with,
                        apply_with_last,
                    );
                }
                AirExpression::Constr { args, .. } => {
                    for arg in args {
                        arg.do_traverse_tree_with(
                            tree_path,
                            current_depth + 1,
                            index_count.next_number(),
                            with,
                            apply_with_last,
                        );
                    }
                }
                AirExpression::RecordUpdate { record, args, .. } => {
                    record.do_traverse_tree_with(
                        tree_path,
                        current_depth + 1,
                        index_count.next_number(),
                        with,
                        apply_with_last,
                    );
                    for arg in args {
                        arg.do_traverse_tree_with(
                            tree_path,
                            current_depth + 1,
                            index_count.next_number(),
                            with,
                            apply_with_last,
                        );
                    }
                }
                AirExpression::RecordAccess { record, .. } => {
                    record.do_traverse_tree_with(
                        tree_path,
                        current_depth + 1,
                        index_count.next_number(),
                        with,
                        apply_with_last,
                    );
                }
                AirExpression::TupleIndex { tuple, .. } => {
                    tuple.do_traverse_tree_with(
                        tree_path,
                        current_depth + 1,
                        index_count.next_number(),
                        with,
                        apply_with_last,
                    );
                }
                AirExpression::Trace { msg, then, .. } => {
                    msg.do_traverse_tree_with(
                        tree_path,
                        current_depth + 1,
                        index_count.next_number(),
                        with,
                        apply_with_last,
                    );

                    then.do_traverse_tree_with(
                        tree_path,
                        current_depth + 1,
                        index_count.next_number(),
                        with,
                        apply_with_last,
                    );
                }
                _ => {}
            },
            a => unreachable!("GOT THIS {:#?}", a),
        }

        if apply_with_last {
            with(self, tree_path);
        }

        tree_path.pop();
    }

    pub fn find_air_tree_node<'a>(&'a mut self, tree_path: &TreePath) -> &'a mut AirTree {
        let mut path_iter = tree_path.path.iter();
        path_iter.next();
        self.do_find_air_tree_node(&mut path_iter)
    }

    fn do_find_air_tree_node<'a>(
        &'a mut self,
        tree_path_iter: &mut Iter<(usize, usize)>,
    ) -> &'a mut AirTree {
        // For finding the air node we skip over the define func ops since those are added later on.
        if let AirTree::Statement {
            statement: AirStatement::DefineFunc { .. } | AirStatement::DefineCyclicFuncs { .. },
            hoisted_over: Some(hoisted_over),
        } = self
        {
            hoisted_over.as_mut().do_find_air_tree_node(tree_path_iter)
        } else if let Some((_depth, index)) = tree_path_iter.next() {
            let mut children_nodes = vec![];
            match self {
                AirTree::Statement {
                    statement,
                    hoisted_over: Some(hoisted_over),
                } => match statement {
                    AirStatement::Let { value, .. } => {
                        if *index == 0 {
                            value.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else if *index == 1 {
                            hoisted_over.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else {
                            panic!("Tree Path index outside tree children nodes")
                        }
                    }
                    AirStatement::AssertConstr { constr, .. } => {
                        if *index == 0 {
                            constr.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else if *index == 1 {
                            hoisted_over.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else {
                            panic!("Tree Path index outside tree children nodes")
                        }
                    }
                    AirStatement::AssertBool { value, .. } => {
                        if *index == 0 {
                            value.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else if *index == 1 {
                            hoisted_over.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else {
                            panic!("Tree Path index outside tree children nodes")
                        }
                    }
                    AirStatement::ClauseGuard { pattern, .. } => {
                        if *index == 0 {
                            pattern.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else if *index == 1 {
                            hoisted_over.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else {
                            panic!("Tree Path index outside tree children nodes")
                        }
                    }
                    AirStatement::ListClauseGuard { .. } => {
                        if *index == 0 {
                            hoisted_over.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else {
                            panic!("Tree Path index outside tree children nodes")
                        }
                    }
                    AirStatement::TupleGuard { .. } => {
                        if *index == 0 {
                            hoisted_over.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else {
                            panic!("Tree Path index outside tree children nodes")
                        }
                    }
                    AirStatement::FieldsExpose { record, .. } => {
                        if *index == 0 {
                            record.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else if *index == 1 {
                            hoisted_over.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else {
                            panic!("Tree Path index outside tree children nodes")
                        }
                    }
                    AirStatement::ListAccessor { list, .. } => {
                        if *index == 0 {
                            list.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else if *index == 1 {
                            hoisted_over.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else {
                            panic!("Tree Path index outside tree children nodes")
                        }
                    }
                    AirStatement::ListExpose { .. } => {
                        if *index == 0 {
                            hoisted_over.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else {
                            panic!("Tree Path index outside tree children nodes")
                        }
                    }
                    AirStatement::TupleAccessor { tuple, .. } => {
                        if *index == 0 {
                            tuple.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else if *index == 1 {
                            hoisted_over.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else {
                            panic!("Tree Path index outside tree children nodes")
                        }
                    }
                    AirStatement::NoOp => {
                        if *index == 0 {
                            hoisted_over.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else {
                            panic!("Tree Path index outside tree children nodes")
                        }
                    }
                    AirStatement::DefineFunc { .. } => unreachable!(),
                    AirStatement::DefineCyclicFuncs { .. } => unreachable!(),
                    AirStatement::FieldsEmpty { constr } => {
                        if *index == 0 {
                            constr.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else if *index == 1 {
                            hoisted_over.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else {
                            panic!("Tree Path index outside tree children nodes")
                        }
                    }
                    AirStatement::ListEmpty { list } => {
                        if *index == 0 {
                            list.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else if *index == 1 {
                            hoisted_over.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else {
                            panic!("Tree Path index outside tree children nodes")
                        }
                    }
                },
                AirTree::Expression(e) => match e {
                    AirExpression::List { items, .. }
                    | AirExpression::Tuple { items, .. }
                    | AirExpression::Builtin { args: items, .. } => {
                        let item = items.get_mut(*index).unwrap_or_else(|| {
                            panic!("Tree Path index outside tree children nodes")
                        });
                        item.do_find_air_tree_node(tree_path_iter)
                    }
                    AirExpression::Call { func, args, .. } => {
                        children_nodes.push(func.as_mut());
                        children_nodes.extend(args.iter_mut());

                        let item = children_nodes.swap_remove(*index);

                        item.do_find_air_tree_node(tree_path_iter)
                    }
                    AirExpression::Fn { func_body, .. } => {
                        if *index == 0 {
                            func_body.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else {
                            panic!("Tree Path index outside tree children nodes")
                        }
                    }
                    AirExpression::BinOp { left, right, .. } => {
                        if *index == 0 {
                            left.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else if *index == 1 {
                            right.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else {
                            panic!("Tree Path index outside tree children nodes")
                        }
                    }
                    AirExpression::UnOp { arg, .. } => {
                        if *index == 0 {
                            arg.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else {
                            panic!("Tree Path index outside tree children nodes")
                        }
                    }
                    AirExpression::CastFromData { value, .. } => {
                        if *index == 0 {
                            value.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else {
                            panic!("Tree Path index outside tree children nodes")
                        }
                    }
                    AirExpression::CastToData { value, .. } => {
                        if *index == 0 {
                            value.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else {
                            panic!("Tree Path index outside tree children nodes")
                        }
                    }
                    AirExpression::When {
                        subject, clauses, ..
                    } => {
                        if *index == 0 {
                            subject.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else if *index == 1 {
                            clauses.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else {
                            panic!("Tree Path index outside tree children nodes")
                        }
                    }
                    AirExpression::Clause {
                        pattern,
                        then,
                        otherwise,
                        ..
                    } => {
                        if *index == 0 {
                            pattern.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else if *index == 1 {
                            then.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else if *index == 2 {
                            otherwise.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else {
                            panic!("Tree Path index outside tree children nodes")
                        }
                    }
                    AirExpression::ListClause {
                        then, otherwise, ..
                    } => {
                        if *index == 0 {
                            then.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else if *index == 1 {
                            otherwise.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else {
                            panic!("Tree Path index outside tree children nodes")
                        }
                    }
                    AirExpression::WrapClause { then, otherwise } => {
                        if *index == 0 {
                            then.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else if *index == 1 {
                            otherwise.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else {
                            panic!("Tree Path index outside tree children nodes")
                        }
                    }
                    AirExpression::TupleClause {
                        then, otherwise, ..
                    } => {
                        if *index == 0 {
                            then.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else if *index == 1 {
                            otherwise.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else {
                            panic!("Tree Path index outside tree children nodes")
                        }
                    }
                    AirExpression::Finally { pattern, then } => {
                        if *index == 0 {
                            pattern.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else if *index == 1 {
                            then.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else {
                            panic!("Tree Path index outside tree children nodes")
                        }
                    }
                    AirExpression::If {
                        pattern,
                        then,
                        otherwise,
                        ..
                    } => {
                        if *index == 0 {
                            pattern.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else if *index == 1 {
                            then.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else if *index == 2 {
                            otherwise.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else {
                            panic!("Tree Path index outside tree children nodes")
                        }
                    }
                    AirExpression::Constr { args, .. } => {
                        let item = args.get_mut(*index).unwrap_or_else(|| {
                            panic!("Tree Path index outside tree children nodes")
                        });
                        item.do_find_air_tree_node(tree_path_iter)
                    }
                    AirExpression::RecordUpdate { record, args, .. } => {
                        children_nodes.push(record.as_mut());
                        children_nodes.extend(args.iter_mut());

                        let item = children_nodes.swap_remove(*index);

                        item.do_find_air_tree_node(tree_path_iter)
                    }
                    AirExpression::RecordAccess { record, .. } => {
                        if *index == 0 {
                            record.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else {
                            panic!("Tree Path index outside tree children nodes")
                        }
                    }
                    AirExpression::TupleIndex { tuple, .. } => {
                        if *index == 0 {
                            tuple.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else {
                            panic!("Tree Path index outside tree children nodes")
                        }
                    }

                    AirExpression::Trace { msg, then, .. } => {
                        if *index == 0 {
                            msg.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else if *index == 1 {
                            then.as_mut().do_find_air_tree_node(tree_path_iter)
                        } else {
                            panic!("Tree Path index outside tree children nodes")
                        }
                    }
                    _ => unreachable!(
                        "A tree node with no children was encountered with a longer tree path."
                    ),
                },
                _ => unreachable!(),
            }
        } else {
            self
        }
    }
}
