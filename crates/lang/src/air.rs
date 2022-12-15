use std::sync::Arc;

use uplc::builtins::DefaultFunction;

use crate::{
    ast::{AssignmentKind, BinOp, TypedRecordUpdateArg},
    tipo::{Type, ValueConstructor},
};

#[derive(Debug, Clone)]
pub enum Air {
    Int {
        scope: Vec<u64>,
        value: String,
    },

    String {
        scope: Vec<u64>,
        value: String,
    },

    ByteArray {
        scope: Vec<u64>,
        bytes: Vec<u8>,
    },

    Var {
        scope: Vec<u64>,
        constructor: ValueConstructor,
        name: String,
        variant_name: String,
    },

    Fn {
        scope: Vec<u64>,
        params: Vec<String>,
    },
    List {
        scope: Vec<u64>,
        count: usize,
        tipo: Arc<Type>,
        tail: bool,
    },

    ListAccessor {
        scope: Vec<u64>,
        tipo: Arc<Type>,
        names: Vec<String>,
        tail: bool,
    },

    ListExpose {
        scope: Vec<u64>,
        tipo: Arc<Type>,
        tail_head_names: Vec<(String, String)>,
        tail: Option<(String, String)>,
    },

    Call {
        scope: Vec<u64>,
        count: usize,
    },

    Builtin {
        scope: Vec<u64>,
        func: DefaultFunction,
        tipo: Arc<Type>,
    },

    BinOp {
        scope: Vec<u64>,
        name: BinOp,
        count: usize,
        tipo: Arc<Type>,
    },

    Assignment {
        scope: Vec<u64>,
        name: String,
        kind: AssignmentKind,
    },

    DefineFunc {
        scope: Vec<u64>,
        func_name: String,
        module_name: String,
        params: Vec<String>,
        recursive: bool,
        variant_name: String,
    },

    DefineConst {
        scope: Vec<u64>,
        func_name: String,
        module_name: String,
        count: usize,
    },

    DefineConstrFields {
        scope: Vec<u64>,
        func_name: String,
        module_name: String,
        count: usize,
    },

    DefineConstrFieldAccess {
        scope: Vec<u64>,
        func_name: String,
        module_name: String,
        count: usize,
    },

    Lam {
        scope: Vec<u64>,
        name: String,
    },

    When {
        scope: Vec<u64>,
        tipo: Arc<Type>,
        subject_name: String,
    },

    Clause {
        scope: Vec<u64>,
        tipo: Arc<Type>,
        subject_name: String,
        complex_clause: bool,
    },

    ListClause {
        scope: Vec<u64>,
        tipo: Arc<Type>,
        tail_name: String,
        complex_clause: bool,
        next_tail_name: Option<String>,
    },

    ClauseGuard {
        scope: Vec<u64>,
        subject_name: String,
        tipo: Arc<Type>,
    },

    Discard {
        scope: Vec<u64>,
    },

    Finally {
        scope: Vec<u64>,
    },

    If {
        scope: Vec<u64>,
    },

    Constr {
        scope: Vec<u64>,
        count: usize,
    },

    Fields {
        scope: Vec<u64>,
        count: usize,
    },

    RecordAccess {
        scope: Vec<u64>,
        index: u64,
        tipo: Arc<Type>,
    },

    FieldsExpose {
        scope: Vec<u64>,
        count: usize,
        indices: Vec<(usize, String, Arc<Type>)>,
    },

    // ModuleSelect {
    //  scope: Vec<u64>,
    //     tipo: Arc<Type>,
    //     label: String,
    //     module_name: String,
    //     module_alias: String,
    //     constructor: ModuleValueConstructor,
    // },
    Tuple {
        scope: Vec<u64>,
        tipo: Arc<Type>,
        count: usize,
    },

    // TupleIndex {
    //  scope: Vec<u64>,
    //
    // tipo: Arc<Type>,
    // index: u64,
    // tuple: Box<Self>,
    // },
    Todo {
        scope: Vec<u64>,
        label: Option<String>,
        tipo: Arc<Type>,
    },

    Record {
        scope: Vec<u64>,
    },

    RecordUpdate {
        scope: Vec<u64>,
        tipo: Arc<Type>,
        spread: Box<Self>,
        args: Vec<TypedRecordUpdateArg>,
    },

    Negate {
        scope: Vec<u64>,
    },

    TupleAccessor {
        scope: Vec<u64>,
        names: Vec<String>,
        tipo: Arc<Type>,
    },
}

impl Air {
    pub fn scope(&self) -> Vec<u64> {
        match self {
            Air::Int { scope, .. }
            | Air::String { scope, .. }
            | Air::ByteArray { scope, .. }
            | Air::Var { scope, .. }
            | Air::List { scope, .. }
            | Air::ListAccessor { scope, .. }
            | Air::ListExpose { scope, .. }
            | Air::Fn { scope, .. }
            | Air::Call { scope, .. }
            | Air::Builtin { scope, .. }
            | Air::BinOp { scope, .. }
            | Air::Assignment { scope, .. }
            | Air::DefineFunc { scope, .. }
            | Air::DefineConst { scope, .. }
            | Air::DefineConstrFields { scope, .. }
            | Air::DefineConstrFieldAccess { scope, .. }
            | Air::Lam { scope, .. }
            | Air::When { scope, .. }
            | Air::Clause { scope, .. }
            | Air::ListClause { scope, .. }
            | Air::ClauseGuard { scope, .. }
            | Air::Discard { scope }
            | Air::Finally { scope }
            | Air::If { scope, .. }
            | Air::Constr { scope, .. }
            | Air::Fields { scope, .. }
            | Air::RecordAccess { scope, .. }
            | Air::FieldsExpose { scope, .. }
            | Air::Tuple { scope, .. }
            | Air::Todo { scope, .. }
            | Air::Record { scope, .. }
            | Air::RecordUpdate { scope, .. }
            | Air::Negate { scope, .. }
            | Air::TupleAccessor { scope, .. } => scope.to_vec(),
        }
    }
}
