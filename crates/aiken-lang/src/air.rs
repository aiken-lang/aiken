use std::sync::Arc;

use indexmap::IndexSet;
use uplc::builtins::DefaultFunction;

use crate::{
    ast::{BinOp, UnOp},
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

    Bool {
        scope: Vec<u64>,
        value: bool,
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
    },

    Assert {
        scope: Vec<u64>,
        constr_index: usize,
    },

    DefineFunc {
        scope: Vec<u64>,
        func_name: String,
        module_name: String,
        params: Vec<String>,
        recursive: bool,
        variant_name: String,
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
        next_tail_name: Option<String>,
        complex_clause: bool,
    },

    TupleClause {
        scope: Vec<u64>,
        tipo: Arc<Type>,
        indices: IndexSet<(usize, String)>,
        predefined_indices: IndexSet<(usize, String)>,
        subject_name: String,
        count: usize,
        complex_clause: bool,
    },

    ClauseGuard {
        scope: Vec<u64>,
        subject_name: String,
        tipo: Arc<Type>,
    },

    ListClauseGuard {
        scope: Vec<u64>,
        tipo: Arc<Type>,
        tail_name: String,
        next_tail_name: Option<String>,
        inverse: bool,
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

    Record {
        scope: Vec<u64>,
        constr_index: usize,
        tipo: Arc<Type>,
        count: usize,
    },

    RecordAccess {
        scope: Vec<u64>,
        record_index: u64,
        tipo: Arc<Type>,
    },

    FieldsExpose {
        scope: Vec<u64>,
        count: usize,
        indices: Vec<(usize, String, Arc<Type>)>,
    },

    Tuple {
        scope: Vec<u64>,
        tipo: Arc<Type>,
        count: usize,
    },

    TupleIndex {
        scope: Vec<u64>,
        tipo: Arc<Type>,
        tuple_index: usize,
    },

    Todo {
        scope: Vec<u64>,
        label: Option<String>,
        tipo: Arc<Type>,
    },

    ErrorTerm {
        scope: Vec<u64>,
        tipo: Arc<Type>,
        label: Option<String>,
    },

    Trace {
        scope: Vec<u64>,
        text: Option<String>,
        tipo: Arc<Type>,
    },

    RecordUpdate {
        scope: Vec<u64>,
        highest_index: usize,
        indices: Vec<(usize, Arc<Type>)>,
    },

    UnOp {
        scope: Vec<u64>,
        op: UnOp,
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
            | Air::Bool { scope, .. }
            | Air::Var { scope, .. }
            | Air::List { scope, .. }
            | Air::ListAccessor { scope, .. }
            | Air::ListExpose { scope, .. }
            | Air::Fn { scope, .. }
            | Air::Call { scope, .. }
            | Air::Builtin { scope, .. }
            | Air::BinOp { scope, .. }
            | Air::Assignment { scope, .. }
            | Air::Assert { scope, .. }
            | Air::DefineFunc { scope, .. }
            | Air::Lam { scope, .. }
            | Air::When { scope, .. }
            | Air::Clause { scope, .. }
            | Air::ListClause { scope, .. }
            | Air::ClauseGuard { scope, .. }
            | Air::ListClauseGuard { scope, .. }
            | Air::Discard { scope }
            | Air::Finally { scope }
            | Air::If { scope, .. }
            | Air::Record { scope, .. }
            | Air::RecordAccess { scope, .. }
            | Air::FieldsExpose { scope, .. }
            | Air::Tuple { scope, .. }
            | Air::Todo { scope, .. }
            | Air::ErrorTerm { scope, .. }
            | Air::RecordUpdate { scope, .. }
            | Air::UnOp { scope, .. }
            | Air::Trace { scope, .. }
            | Air::TupleAccessor { scope, .. }
            | Air::TupleIndex { scope, .. }
            | Air::TupleClause { scope, .. } => scope.to_vec(),
        }
    }

    pub fn tipo(&self) -> Option<Arc<Type>> {
        match self {
            Air::List { tipo, .. }
            | Air::ListAccessor { tipo, .. }
            | Air::ListExpose { tipo, .. }
            | Air::Builtin { tipo, .. }
            | Air::BinOp { tipo, .. }
            | Air::When { tipo, .. }
            | Air::Clause { tipo, .. }
            | Air::ListClause { tipo, .. }
            | Air::TupleClause { tipo, .. }
            | Air::ClauseGuard { tipo, .. }
            | Air::ListClauseGuard { tipo, .. }
            | Air::Record { tipo, .. }
            | Air::RecordAccess { tipo, .. }
            | Air::Tuple { tipo, .. }
            | Air::TupleIndex { tipo, .. }
            | Air::Todo { tipo, .. }
            | Air::ErrorTerm { tipo, .. }
            | Air::Trace { tipo, .. }
            | Air::TupleAccessor { tipo, .. }
            | Air::Var {
                constructor: ValueConstructor { tipo, .. },
                ..
            } => Some(tipo.clone()),
            _ => None,
        }
    }
}
