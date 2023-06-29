use indexmap::IndexSet;
use std::{collections::VecDeque, sync::Arc};
use uplc::builtins::DefaultFunction;

use crate::{
    ast::{BinOp, Span, UnOp},
    builtins::{data, list, void},
    tipo::{Type, ValueConstructor, ValueConstructorVariant},
};

use super::air::Air;

#[derive(Debug, Clone, PartialEq)]
pub enum AirTree {
    Statement(AirStatement),
    Expression(AirExpression),
    UnhoistedSequence(Vec<AirTree>),
}

pub struct AirIterator<'a> {
    deque_pointer: VecDeque<&'a AirTree>,
}

impl Iterator for AirIterator<'_> {
    type Item = Air;

    fn next(&mut self) -> Option<Self::Item> {
        todo!()
    }
}

impl DoubleEndedIterator for AirIterator<'_> {
    fn next_back(&mut self) -> Option<Self::Item> {
        todo!()
    }
}

pub struct AirTreeIterator<'a> {
    deque_pointer: VecDeque<&'a AirTree>,
}

impl<'a> Iterator for AirTreeIterator<'a> {
    fn next(&mut self) -> Option<Self::Item> {
        self.deque_pointer.pop_front()
    }

    type Item = &'a AirTree;
}

impl DoubleEndedIterator for AirTreeIterator<'_> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.deque_pointer.pop_back()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AirStatement {
    // Assignment
    Let {
        name: String,
        value: Box<AirTree>,
        hoisted_over: Option<Box<AirTree>>,
    },
    DefineFunc {
        func_name: String,
        module_name: String,
        params: Vec<String>,
        recursive: bool,
        variant_name: String,
        func_body: Box<AirTree>,
        hoisted_over: Box<AirTree>,
    },
    // Assertions
    AssertConstr {
        constr_index: usize,
        constr: Box<AirTree>,
        hoisted_over: Option<Box<AirTree>>,
    },
    AssertBool {
        is_true: bool,
        value: Box<AirTree>,
        hoisted_over: Option<Box<AirTree>>,
    },
    // Field Access
    FieldsExpose {
        indices: Vec<(usize, String, Arc<Type>)>,
        check_last_item: bool,
        record: Box<AirTree>,
        hoisted_over: Option<Box<AirTree>>,
    },
    // List Access
    ListAccessor {
        tipo: Arc<Type>,
        names: Vec<String>,
        tail: bool,
        check_last_item: bool,
        list: Box<AirTree>,
        hoisted_over: Option<Box<AirTree>>,
    },
    ListExpose {
        tipo: Arc<Type>,
        tail_head_names: Vec<(String, String)>,
        tail: Option<(String, String)>,
        list: Box<AirTree>,
        hoisted_over: Option<Box<AirTree>>,
    },
    // Tuple Access
    TupleAccessor {
        names: Vec<String>,
        tipo: Arc<Type>,
        check_last_item: bool,
        tuple: Box<AirTree>,
        hoisted_over: Option<Box<AirTree>>,
    },
    // Misc.
    NoOp {
        hoisted_over: Option<Box<AirTree>>,
    },
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
        hoisting_over: AirTree,
    ) -> AirTree {
        AirTree::Statement(AirStatement::DefineFunc {
            func_name: func_name.to_string(),
            module_name: module_name.to_string(),
            params,
            recursive,
            variant_name: variant_name.to_string(),
            func_body: func_body.into(),
            hoisted_over: hoisting_over.into(),
        })
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
        AirTree::Statement(AirStatement::Let {
            name: name.to_string(),
            value: value.into(),
            hoisted_over: None,
        })
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
        AirTree::Statement(AirStatement::AssertConstr {
            constr_index,
            constr: constr.into(),
            hoisted_over: None,
        })
    }
    pub fn assert_bool(is_true: bool, value: AirTree) -> AirTree {
        AirTree::Statement(AirStatement::AssertBool {
            is_true,
            value: value.into(),
            hoisted_over: None,
        })
    }
    pub fn when(subject_name: impl ToString, tipo: Arc<Type>, clauses: AirTree) -> AirTree {
        AirTree::Expression(AirExpression::When {
            tipo,
            subject_name: subject_name.to_string(),
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
        AirTree::Statement(AirStatement::FieldsExpose {
            indices,
            check_last_item,
            record: record.into(),
            hoisted_over: None,
        })
    }
    pub fn list_access(
        names: Vec<String>,
        tipo: Arc<Type>,
        tail: bool,
        check_last_item: bool,
        list: AirTree,
    ) -> AirTree {
        AirTree::Statement(AirStatement::ListAccessor {
            tipo,
            names,
            tail,
            check_last_item,
            list: list.into(),
            hoisted_over: None,
        })
    }
    pub fn list_expose(
        tail_head_names: Vec<(String, String)>,
        tail: Option<(String, String)>,
        tipo: Arc<Type>,
        list: AirTree,
    ) -> AirTree {
        AirTree::Statement(AirStatement::ListExpose {
            tipo,
            tail_head_names,
            tail,
            list: list.into(),
            hoisted_over: None,
        })
    }
    pub fn tuple_access(
        names: Vec<String>,
        tipo: Arc<Type>,
        check_last_item: bool,
        tuple: AirTree,
    ) -> AirTree {
        AirTree::Statement(AirStatement::TupleAccessor {
            names,
            tipo,
            check_last_item,
            tuple: tuple.into(),
            hoisted_over: None,
        })
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
        AirTree::Statement(AirStatement::NoOp { hoisted_over: None })
    }
    pub fn fields_empty(constr: AirTree) -> AirTree {
        AirTree::Expression(AirExpression::FieldsEmpty {
            constr: constr.into(),
        })
    }
    pub fn list_empty(list: AirTree) -> AirTree {
        AirTree::Expression(AirExpression::ListEmpty { list: list.into() })
    }
    pub fn hoist_over(mut assignment: AirTree, next_exp: AirTree) -> AirTree {
        match &mut assignment {
            AirTree::Statement(st) => match st {
                AirStatement::Let { hoisted_over, .. }
                | AirStatement::AssertConstr { hoisted_over, .. }
                | AirStatement::AssertBool { hoisted_over, .. }
                | AirStatement::FieldsExpose { hoisted_over, .. }
                | AirStatement::ListAccessor { hoisted_over, .. }
                | AirStatement::NoOp { hoisted_over }
                | AirStatement::ListExpose { hoisted_over, .. }
                | AirStatement::TupleAccessor { hoisted_over, .. } => {
                    assert!(hoisted_over.is_none());
                    *hoisted_over = Some(next_exp.into());
                    assignment
                }
                AirStatement::DefineFunc { .. } => {
                    unreachable!("Should not use this function to hoist defined functions.")
                }
            },
            AirTree::Expression(_) => {
                unreachable!("Trying to hoist an expression onto an expression.")
            }
            AirTree::UnhoistedSequence(seq) => {
                let mut final_exp = next_exp;
                while let Some(assign) = seq.pop() {
                    final_exp = Self::hoist_over(assign, final_exp);
                }
                final_exp
            }
        }
    }

    pub fn expect_on_list() -> AirTree {
        // self.air.push(Air::DefineFunc {
        //     scope: self.scope.clone(),
        //     func_name: EXPECT_ON_LIST.to_string(),
        //     module_name: "".to_string(),
        //     params: vec!["__list_to_check".to_string(), "__check_with".to_string()],
        //     recursive: true,
        //     variant_name: "".to_string(),
        // });

        let list_var = AirTree::local_var("__list_to_check", list(data()));

        let head_list = AirTree::builtin(DefaultFunction::HeadList, data(), vec![list_var]);

        let _expect_on_head = AirTree::call(
            AirTree::local_var("__check_with", void()),
            void(),
            vec![head_list],
        );
        todo!()

        // self.list_clause(void(), "__list_to_check", None, false, void_stack);

        // self.choose_unit(check_with_stack);

        // expect_stack.var(
        //     ValueConstructor::public(
        //         void(),
        //         ValueConstructorVariant::ModuleFn {
        //             name: EXPECT_ON_LIST.to_string(),
        //             field_map: None,
        //             module: "".to_string(),
        //             arity: 2,
        //             location: Span::empty(),
        //             builtin: None,
        //         },
        //     ),
        //     EXPECT_ON_LIST,
        //     "",
        // );

        // arg_stack1.local_var(list(data()), "__list_to_check");

        // arg_stack2.local_var(void(), "__check_with");

        // tail_stack.builtin(DefaultFunction::TailList, list(data()), vec![arg_stack1]);

        // self.call(void(), expect_stack, vec![tail_stack, arg_stack2])
    }

    pub fn iter(&self) -> AirTreeIterator {
        let mut new_vec = vec![];
        self.create_iter(&mut new_vec);
        AirTreeIterator {
            deque_pointer: new_vec.into(),
        }
    }

    pub fn air_iter(&self) -> AirIterator {
        let mut new_vec = vec![];
        self.create_iter(&mut new_vec);
        AirIterator {
            deque_pointer: new_vec.into(),
        }
    }

    fn create_iter<'a>(&'a self, pointer_vec: &mut Vec<&'a AirTree>) {
        match self {
            AirTree::Statement(st) => match st {
                AirStatement::Let {
                    value,
                    hoisted_over: Some(exp),
                    ..
                } => {
                    pointer_vec.push(self);
                    value.create_iter(pointer_vec);
                    exp.create_iter(pointer_vec);
                }
                AirStatement::DefineFunc { .. } => todo!(),
                AirStatement::AssertConstr {
                    constr,
                    hoisted_over: Some(exp),
                    ..
                } => {
                    pointer_vec.push(self);
                    constr.create_iter(pointer_vec);
                    exp.create_iter(pointer_vec);
                }
                AirStatement::AssertBool { .. } => todo!(),
                AirStatement::FieldsExpose { .. } => todo!(),
                AirStatement::ListAccessor { .. } => todo!(),
                AirStatement::ListExpose { .. } => todo!(),
                AirStatement::TupleAccessor { .. } => todo!(),
                AirStatement::NoOp { .. } => todo!(),
                _ => unreachable!("FOUND UNHOISTED STATEMENT"),
            },
            AirTree::Expression(_) => todo!(),
            AirTree::UnhoistedSequence(_) => {
                unreachable!("SHOULD FIRST RESOLVE ALL UNHOISTED SEQUENCES")
            }
        }
    }

    pub fn convert_to_air(&self) -> Air {
        match self {
            AirTree::Statement(st) => match st {
                AirStatement::Let {
                    name,
                    value,
                    hoisted_over: Some(exp),
                } => Air::Let { name: name.clone() },
                AirStatement::DefineFunc { .. } => todo!(),
                AirStatement::AssertConstr {
                    constr_index,
                    constr,
                    hoisted_over: Some(exp),
                } => Air::AssertConstr {
                    constr_index: *constr_index,
                },
                AirStatement::AssertBool {
                    is_true,
                    value,
                    hoisted_over: Some(exp),
                } => Air::AssertBool { is_true: *is_true },
                AirStatement::FieldsExpose {
                    indices,
                    check_last_item,
                    record,
                    hoisted_over: Some(exp),
                } => Air::FieldsExpose {
                    indices: indices.clone(),
                    check_last_item: *check_last_item,
                },
                AirStatement::ListAccessor {
                    tipo,
                    names,
                    tail,
                    check_last_item,
                    list,
                    hoisted_over: Some(exp),
                } => Air::ListAccessor {
                    tipo: tipo.clone(),
                    names: names.clone(),
                    tail: *tail,
                    check_last_item: *check_last_item,
                },
                AirStatement::ListExpose {
                    tipo,
                    tail_head_names,
                    tail,
                    list,
                    hoisted_over: Some(exp),
                } => Air::ListExpose {
                    tipo: tipo.clone(),
                    tail_head_names: tail_head_names.clone(),
                    tail: tail.clone(),
                },
                AirStatement::TupleAccessor {
                    names,
                    tipo,
                    check_last_item,
                    tuple,
                    hoisted_over: Some(exp),
                } => Air::TupleAccessor {
                    names: names.clone(),
                    tipo: tipo.clone(),
                    check_last_item: *check_last_item,
                },
                AirStatement::NoOp {
                    hoisted_over: Some(exp),
                } => Air::NoOp,
                _ => unreachable!("SHOULD NOT HAVE A HOISTED OVER RESOLVING TO NONE"),
            },
            AirTree::Expression(exp) => match exp {
                AirExpression::Int { value } => Air::Int {
                    value: value.clone(),
                },
                AirExpression::String { value } => Air::String {
                    value: value.clone(),
                },
                AirExpression::ByteArray { bytes } => Air::ByteArray {
                    bytes: bytes.clone(),
                },
                AirExpression::Bool { value } => Air::Bool { value: *value },
                AirExpression::List { .. } => todo!(),
                AirExpression::Tuple { .. } => todo!(),
                AirExpression::Void => todo!(),
                AirExpression::Var { .. } => todo!(),
                AirExpression::Call { .. } => todo!(),
                AirExpression::Fn { .. } => todo!(),
                AirExpression::Builtin { .. } => todo!(),
                AirExpression::BinOp { .. } => todo!(),
                AirExpression::UnOp { .. } => todo!(),
                AirExpression::UnWrapData { .. } => todo!(),
                AirExpression::WrapData { .. } => todo!(),
                AirExpression::When { .. } => todo!(),
                AirExpression::Clause { .. } => todo!(),
                AirExpression::ListClause { .. } => todo!(),
                AirExpression::WrapClause { .. } => todo!(),
                AirExpression::TupleClause { .. } => todo!(),
                AirExpression::ClauseGuard { .. } => todo!(),
                AirExpression::ListClauseGuard { .. } => todo!(),
                AirExpression::Finally { .. } => todo!(),
                AirExpression::If { .. } => todo!(),
                AirExpression::Constr { .. } => todo!(),
                AirExpression::RecordUpdate { .. } => todo!(),
                AirExpression::RecordAccess { .. } => todo!(),
                AirExpression::TupleIndex { .. } => todo!(),
                AirExpression::ErrorTerm { .. } => todo!(),
                AirExpression::Trace { .. } => todo!(),
                AirExpression::FieldsEmpty { .. } => todo!(),
                AirExpression::ListEmpty { .. } => todo!(),
            },
            AirTree::UnhoistedSequence(_) => {
                unreachable!("SHOULD FIRST RESOLVE ALL UNHOISTED SEQUENCES")
            }
        }
    }
}
