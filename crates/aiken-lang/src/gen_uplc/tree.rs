use indexmap::IndexSet;
use std::sync::Arc;
use uplc::builtins::DefaultFunction;

use crate::{
    ast::{BinOp, UnOp},
    tipo::{Type, ValueConstructor},
};

use super::air::Air;

#[derive(Debug, Clone, PartialEq)]
pub enum AirTree {
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
    DefineFunc {
        func_name: String,
        module_name: String,
        params: Vec<String>,
        recursive: bool,
        variant_name: String,
        func_body: Box<AirTree>,
        hoisted_over: Box<AirTree>,
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
    // Assignment
    Let {
        name: String,
        value: Box<AirTree>,
        hoisted_over: Box<AirTree>,
    },
    UnWrapData {
        tipo: Arc<Type>,
        value: Box<AirTree>,
    },
    WrapData {
        tipo: Arc<Type>,
        value: Box<AirTree>,
    },
    AssertConstr {
        constr_index: usize,
        constr: Box<AirTree>,
    },
    AssertBool {
        is_true: bool,
        value: Box<AirTree>,
    },
    // When
    When {
        tipo: Arc<Type>,
        subject_name: String,
        clauses: Vec<AirTree>,
        final_clause: Box<AirTree>,
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
    FieldsExpose {
        indices: Vec<(usize, String, Arc<Type>)>,
        check_last_item: bool,
        record: Box<AirTree>,
    },
    // ListAccess
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
    NoOp,
    FieldsEmpty {
        constr: Box<AirTree>,
    },
    ListEmpty {
        list: Box<AirTree>,
    },
}

impl AirTree {
    pub fn int(value: impl ToString) -> AirTree {
        AirTree::Int {
            value: value.to_string(),
        }
    }
    pub fn string(value: impl ToString) -> AirTree {
        AirTree::String {
            value: value.to_string(),
        }
    }
    pub fn byte_array(bytes: Vec<u8>) -> AirTree {
        AirTree::ByteArray { bytes }
    }
    pub fn bool(value: bool) -> AirTree {
        AirTree::Bool { value }
    }
    pub fn list(items: Vec<AirTree>, tipo: Arc<Type>, tail: bool) -> AirTree {
        AirTree::List { tipo, tail, items }
    }
    pub fn tuple(items: Vec<AirTree>, tipo: Arc<Type>) -> AirTree {
        AirTree::Tuple { tipo, items }
    }
    pub fn void() -> AirTree {
        AirTree::Void
    }
    pub fn var(
        constructor: ValueConstructor,
        name: impl ToString,
        variant_name: impl ToString,
    ) -> AirTree {
        AirTree::Var {
            constructor,
            name: name.to_string(),
            variant_name: variant_name.to_string(),
        }
    }
    pub fn call(func: AirTree, tipo: Arc<Type>, args: Vec<AirTree>) -> AirTree {
        AirTree::Call {
            tipo,
            func: func.into(),
            args,
        }
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
        AirTree::DefineFunc {
            func_name: func_name.to_string(),
            module_name: module_name.to_string(),
            params,
            recursive,
            variant_name: variant_name.to_string(),
            func_body: func_body.into(),
            hoisted_over: hoisting_over.into(),
        }
    }
    pub fn anon_func(params: Vec<String>, func_body: AirTree) -> AirTree {
        AirTree::Fn {
            params,
            func_body: func_body.into(),
        }
    }
    pub fn builtin(func: DefaultFunction, tipo: Arc<Type>, args: Vec<AirTree>) -> AirTree {
        AirTree::Builtin { func, tipo, args }
    }
    pub fn binop(op: BinOp, tipo: Arc<Type>, left: AirTree, right: AirTree) -> AirTree {
        AirTree::BinOp {
            name: op,
            tipo,
            left: left.into(),
            right: right.into(),
        }
    }
    pub fn unop(op: UnOp, arg: AirTree) -> AirTree {
        AirTree::UnOp {
            op,
            arg: arg.into(),
        }
    }
    pub fn let_assignment(name: impl ToString, value: AirTree, hoisting_over: AirTree) -> AirTree {
        AirTree::Let {
            name: name.to_string(),
            value: value.into(),
            hoisted_over: hoisting_over.into(),
        }
    }
    pub fn unwrap_data(value: AirTree, tipo: Arc<Type>) -> AirTree {
        AirTree::UnWrapData {
            tipo,
            value: value.into(),
        }
    }
    pub fn wrap_data(value: AirTree, tipo: Arc<Type>) -> AirTree {
        AirTree::WrapData {
            tipo,
            value: value.into(),
        }
    }
    pub fn assert_constr_index(constr_index: usize, constr: AirTree) -> AirTree {
        AirTree::AssertConstr {
            constr_index,
            constr: constr.into(),
        }
    }
    pub fn assert_bool(is_true: bool, value: AirTree) -> AirTree {
        AirTree::AssertBool {
            is_true,
            value: value.into(),
        }
    }
    pub fn when(
        subject_name: impl ToString,
        tipo: Arc<Type>,
        clauses: Vec<AirTree>,
        final_clause: AirTree,
    ) -> AirTree {
        AirTree::When {
            tipo,
            subject_name: subject_name.to_string(),
            clauses,
            final_clause: final_clause.into(),
        }
    }
    pub fn clause(
        subject_name: impl ToString,
        pattern: AirTree,
        tipo: Arc<Type>,
        then: AirTree,
        otherwise: AirTree,
        complex_clause: bool,
    ) -> AirTree {
        AirTree::Clause {
            tipo,
            subject_name: subject_name.to_string(),
            complex_clause,
            pattern: pattern.into(),
            then: then.into(),
            otherwise: otherwise.into(),
        }
    }
    pub fn list_clause(
        tail_name: impl ToString,
        tipo: Arc<Type>,
        then: AirTree,
        otherwise: AirTree,
        next_tail_name: Option<String>,
        complex_clause: bool,
    ) -> AirTree {
        AirTree::ListClause {
            tipo,
            tail_name: tail_name.to_string(),
            next_tail_name,
            complex_clause,
            then: then.into(),
            otherwise: otherwise.into(),
        }
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

        AirTree::TupleClause {
            tipo,
            indices,
            predefined_indices,
            subject_name: subject_name.to_string(),
            type_count,
            complex_clause,
            then: then.into(),
            otherwise: otherwise.into(),
        }
    }
    pub fn wrap_clause(then: AirTree, otherwise: AirTree) -> AirTree {
        AirTree::WrapClause {
            then: then.into(),
            otherwise: otherwise.into(),
        }
    }
    pub fn clause_guard(
        subject_name: impl ToString,
        pattern: AirTree,
        tipo: Arc<Type>,
        then: AirTree,
    ) -> AirTree {
        AirTree::ClauseGuard {
            subject_name: subject_name.to_string(),
            tipo,
            pattern: pattern.into(),
            then: then.into(),
        }
    }
    pub fn list_clause_guard(
        tail_name: impl ToString,
        tipo: Arc<Type>,
        inverse: bool,
        then: AirTree,
        next_tail_name: Option<String>,
    ) -> AirTree {
        AirTree::ListClauseGuard {
            tipo,
            tail_name: tail_name.to_string(),
            next_tail_name,
            inverse,
            then: then.into(),
        }
    }
    pub fn finally(pattern: AirTree, then: AirTree) -> AirTree {
        AirTree::Finally {
            pattern: pattern.into(),
            then: then.into(),
        }
    }
    pub fn if_branches(mut branches: Vec<AirTree>, tipo: Arc<Type>, otherwise: AirTree) -> AirTree {
        assert!(branches.len() > 0);
        let last_if = branches.pop().unwrap();

        let mut final_if = AirTree::If {
            tipo: tipo.clone(),
            then: last_if.into(),
            otherwise: otherwise.into(),
        };

        while let Some(branch) = branches.pop() {
            final_if = AirTree::If {
                tipo: tipo.clone(),
                then: branch.into(),
                otherwise: final_if.into(),
            };
        }

        final_if
    }
    pub fn create_constr(tag: usize, tipo: Arc<Type>, args: Vec<AirTree>) -> AirTree {
        AirTree::Constr { tag, tipo, args }
    }

    pub fn record_update(
        indices: Vec<(usize, Arc<Type>)>,
        highest_index: usize,
        tipo: Arc<Type>,
        record: AirTree,
        args: Vec<AirTree>,
    ) -> AirTree {
        AirTree::RecordUpdate {
            highest_index,
            indices,
            tipo,
            record: record.into(),
            args,
        }
    }
    pub fn record_access(field_index: u64, tipo: Arc<Type>, record: AirTree) -> AirTree {
        AirTree::RecordAccess {
            field_index,
            tipo,
            record: record.into(),
        }
    }

    pub fn fields_expose(
        indices: Vec<(usize, String, Arc<Type>)>,
        check_last_item: bool,
        record: AirTree,
    ) -> AirTree {
        AirTree::FieldsExpose {
            indices,
            check_last_item,
            record: record.into(),
        }
    }
    pub fn list_access(
        names: Vec<String>,
        tipo: Arc<Type>,
        tail: bool,
        check_last_item: bool,
        list: AirTree,
    ) -> AirTree {
        AirTree::ListAccessor {
            tipo,
            names,
            tail,
            check_last_item,
            list: list.into(),
        }
    }
    pub fn list_expose(
        tail_head_names: Vec<(String, String)>,
        tail: Option<(String, String)>,
        tipo: Arc<Type>,
        list: AirTree,
    ) -> AirTree {
        AirTree::ListExpose {
            tipo,
            tail_head_names,
            tail,
            list: list.into(),
        }
    }
    pub fn tuple_access(
        names: Vec<String>,
        tipo: Arc<Type>,
        check_last_item: bool,
        tuple: AirTree,
    ) -> AirTree {
        AirTree::TupleAccessor {
            names,
            tipo,
            check_last_item,
            tuple: tuple.into(),
        }
    }
    pub fn tuple_index(tuple_index: usize, tipo: Arc<Type>, tuple: AirTree) -> AirTree {
        AirTree::TupleIndex {
            tipo,
            tuple_index,
            tuple: tuple.into(),
        }
    }
    pub fn error(tipo: Arc<Type>) -> AirTree {
        AirTree::ErrorTerm { tipo }
    }
    pub fn trace(msg: AirTree, tipo: Arc<Type>, then: AirTree) -> AirTree {
        AirTree::Trace {
            tipo,
            msg: msg.into(),
            then: then.into(),
        }
    }
    pub fn no_op() -> AirTree {
        AirTree::NoOp
    }
    pub fn fields_empty(constr: AirTree) -> AirTree {
        AirTree::FieldsEmpty {
            constr: constr.into(),
        }
    }
    pub fn list_empty(list: AirTree) -> AirTree {
        AirTree::ListEmpty { list: list.into() }
    }

    pub fn to_air_vec(tree: AirTree) -> Vec<Air> {
        match tree {
            AirTree::Int { value } => todo!(),
            AirTree::String { value } => todo!(),
            AirTree::ByteArray { bytes } => todo!(),
            AirTree::Bool { value } => todo!(),
            AirTree::List { tipo, tail, items } => todo!(),
            AirTree::Tuple { tipo, items } => todo!(),
            AirTree::Void => todo!(),
            AirTree::Var {
                constructor,
                name,
                variant_name,
            } => todo!(),
            AirTree::Call { tipo, func, args } => todo!(),
            AirTree::DefineFunc {
                func_name,
                module_name,
                params,
                recursive,
                variant_name,
                func_body,
                hoisted_over,
            } => todo!(),
            AirTree::Fn { params, func_body } => todo!(),
            AirTree::Builtin { func, tipo, args } => todo!(),
            AirTree::BinOp {
                name,
                tipo,
                left,
                right,
            } => todo!(),
            AirTree::UnOp { op, arg } => todo!(),
            AirTree::Let {
                name,
                value,
                hoisted_over,
            } => todo!(),
            AirTree::UnWrapData { tipo, value } => todo!(),
            AirTree::WrapData { tipo, value } => todo!(),
            AirTree::AssertConstr {
                constr_index,
                constr,
            } => todo!(),
            AirTree::AssertBool { is_true, value } => todo!(),
            AirTree::When {
                tipo,
                subject_name,
                clauses,
                final_clause,
            } => todo!(),
            AirTree::Clause {
                tipo,
                subject_name,
                complex_clause,
                pattern,
                then,
                otherwise,
            } => todo!(),
            AirTree::ListClause {
                tipo,
                tail_name,
                next_tail_name,
                complex_clause,
                then,
                otherwise,
            } => todo!(),
            AirTree::WrapClause { then, otherwise } => todo!(),
            AirTree::TupleClause {
                tipo,
                indices,
                predefined_indices,
                subject_name,
                type_count,
                complex_clause,
                then,
                otherwise,
            } => todo!(),
            AirTree::ClauseGuard {
                subject_name,
                tipo,
                pattern,
                then,
            } => todo!(),
            AirTree::ListClauseGuard {
                tipo,
                tail_name,
                next_tail_name,
                inverse,
                then,
            } => todo!(),
            AirTree::Finally { pattern, then } => todo!(),
            AirTree::If {
                tipo,
                then,
                otherwise,
            } => todo!(),
            AirTree::Constr { tag, tipo, args } => todo!(),
            AirTree::RecordUpdate {
                highest_index,
                indices,
                tipo,
                record,
                args,
            } => todo!(),
            AirTree::RecordAccess {
                field_index,
                tipo,
                record,
            } => todo!(),
            AirTree::FieldsExpose {
                indices,
                check_last_item,
                record,
            } => todo!(),
            AirTree::ListAccessor {
                tipo,
                names,
                tail,
                check_last_item,
                list,
            } => todo!(),
            AirTree::ListExpose {
                tipo,
                tail_head_names,
                tail,
                list,
            } => todo!(),
            AirTree::TupleAccessor {
                names,
                tipo,
                check_last_item,
                tuple,
            } => todo!(),
            AirTree::TupleIndex {
                tipo,
                tuple_index,
                tuple,
            } => todo!(),
            AirTree::ErrorTerm { tipo } => todo!(),
            AirTree::Trace { tipo, msg, then } => todo!(),
            AirTree::NoOp => todo!(),
            AirTree::FieldsEmpty { constr } => todo!(),
            AirTree::ListEmpty { list } => todo!(),
        }
    }
}
