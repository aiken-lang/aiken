use std::sync::Arc;

use indexmap::IndexSet;
use uplc::builtins::DefaultFunction;

use crate::{
    ast::{BinOp, UnOp},
    tipo::{Type, ValueConstructor},
};

#[derive(Debug, Clone)]
pub enum Air {
    // Primitives
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

    List {
        scope: Vec<u64>,
        count: usize,
        tipo: Arc<Type>,
        tail: bool,
    },

    Tuple {
        scope: Vec<u64>,
        tipo: Arc<Type>,
        count: usize,
    },

    Void {
        scope: Vec<u64>,
    },

    Var {
        scope: Vec<u64>,
        constructor: ValueConstructor,
        name: String,
        variant_name: String,
    },

    // Functions
    Call {
        scope: Vec<u64>,
        count: usize,
        tipo: Arc<Type>,
    },

    DefineFunc {
        scope: Vec<u64>,
        func_name: String,
        module_name: String,
        params: Vec<String>,
        recursive: bool,
        variant_name: String,
    },

    Fn {
        scope: Vec<u64>,
        params: Vec<String>,
    },

    Builtin {
        scope: Vec<u64>,
        func: DefaultFunction,
        tipo: Arc<Type>,
    },

    // Operators
    BinOp {
        scope: Vec<u64>,
        name: BinOp,
        count: usize,
        tipo: Arc<Type>,
    },

    UnOp {
        scope: Vec<u64>,
        op: UnOp,
    },

    // Assignment
    Let {
        scope: Vec<u64>,
        name: String,
    },

    UnWrapData {
        scope: Vec<u64>,
        tipo: Arc<Type>,
    },

    WrapData {
        scope: Vec<u64>,
        tipo: Arc<Type>,
    },

    AssertConstr {
        scope: Vec<u64>,
        constr_index: usize,
    },

    // When
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

    WrapClause {
        scope: Vec<u64>,
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

    Finally {
        scope: Vec<u64>,
    },

    // If
    If {
        scope: Vec<u64>,
        tipo: Arc<Type>,
    },

    // Record Creation
    Record {
        scope: Vec<u64>,
        constr_index: usize,
        tipo: Arc<Type>,
        count: usize,
    },

    RecordUpdate {
        scope: Vec<u64>,
        highest_index: usize,
        indices: Vec<(usize, Arc<Type>)>,
        tipo: Arc<Type>,
    },

    // Field Access
    RecordAccess {
        scope: Vec<u64>,
        record_index: u64,
        tipo: Arc<Type>,
    },

    FieldsExpose {
        scope: Vec<u64>,
        indices: Vec<(usize, String, Arc<Type>)>,
        check_last_item: bool,
    },

    // ListAccess
    ListAccessor {
        scope: Vec<u64>,
        tipo: Arc<Type>,
        names: Vec<String>,
        tail: bool,
        check_last_item: bool,
    },

    ListExpose {
        scope: Vec<u64>,
        tipo: Arc<Type>,
        tail_head_names: Vec<(String, String)>,
        tail: Option<(String, String)>,
    },

    // Tuple Access
    TupleAccessor {
        scope: Vec<u64>,
        names: Vec<String>,
        tipo: Arc<Type>,
        check_last_item: bool,
    },

    TupleIndex {
        scope: Vec<u64>,
        tipo: Arc<Type>,
        tuple_index: usize,
    },

    // Misc.
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
}

impl Air {
    pub fn scope(&self) -> Vec<u64> {
        match self {
            Air::Int { scope, .. }
            | Air::String { scope, .. }
            | Air::ByteArray { scope, .. }
            | Air::Bool { scope, .. }
            | Air::List { scope, .. }
            | Air::Tuple { scope, .. }
            | Air::Void { scope }
            | Air::Var { scope, .. }
            | Air::Call { scope, .. }
            | Air::DefineFunc { scope, .. }
            | Air::Fn { scope, .. }
            | Air::Builtin { scope, .. }
            | Air::BinOp { scope, .. }
            | Air::UnOp { scope, .. }
            | Air::Let { scope, .. }
            | Air::UnWrapData { scope, .. }
            | Air::WrapData { scope, .. }
            | Air::AssertConstr { scope, .. }
            | Air::When { scope, .. }
            | Air::Clause { scope, .. }
            | Air::ListClause { scope, .. }
            | Air::WrapClause { scope }
            | Air::TupleClause { scope, .. }
            | Air::ClauseGuard { scope, .. }
            | Air::ListClauseGuard { scope, .. }
            | Air::Finally { scope }
            | Air::If { scope, .. }
            | Air::Record { scope, .. }
            | Air::RecordUpdate { scope, .. }
            | Air::RecordAccess { scope, .. }
            | Air::FieldsExpose { scope, .. }
            | Air::ListAccessor { scope, .. }
            | Air::ListExpose { scope, .. }
            | Air::TupleAccessor { scope, .. }
            | Air::TupleIndex { scope, .. }
            | Air::Todo { scope, .. }
            | Air::ErrorTerm { scope, .. }
            | Air::Trace { scope, .. } => scope.clone(),
        }
    }

    pub fn tipo(&self) -> Option<Arc<Type>> {
        match self {
            Air::Int { .. } => Some(
                Type::App {
                    public: true,
                    module: String::new(),
                    name: "Int".to_string(),
                    args: vec![],
                }
                .into(),
            ),
            Air::String { .. } => Some(
                Type::App {
                    public: true,
                    module: String::new(),
                    name: "String".to_string(),
                    args: vec![],
                }
                .into(),
            ),
            Air::ByteArray { .. } => Some(
                Type::App {
                    public: true,
                    module: String::new(),
                    name: "ByteArray".to_string(),
                    args: vec![],
                }
                .into(),
            ),
            Air::Bool { .. } => Some(
                Type::App {
                    public: true,
                    module: String::new(),
                    name: "Bool".to_string(),
                    args: vec![],
                }
                .into(),
            ),
            Air::Void { .. } => Some(
                Type::App {
                    public: true,
                    module: String::new(),
                    name: "Void".to_string(),
                    args: vec![],
                }
                .into(),
            ),
            Air::WrapData { .. } => Some(
                Type::App {
                    public: true,
                    module: String::new(),
                    name: "Data".to_string(),
                    args: vec![],
                }
                .into(),
            ),
            Air::Var { constructor, .. } => Some(constructor.tipo.clone()),
            Air::List { tipo, .. }
            | Air::Tuple { tipo, .. }
            | Air::Call { tipo, .. }
            | Air::Builtin { tipo, .. }
            | Air::BinOp { tipo, .. }
            | Air::UnWrapData { tipo, .. }
            | Air::When { tipo, .. }
            | Air::Clause { tipo, .. }
            | Air::ListClause { tipo, .. }
            | Air::TupleClause { tipo, .. }
            | Air::ClauseGuard { tipo, .. }
            | Air::If { tipo, .. }
            | Air::ListClauseGuard { tipo, .. }
            | Air::Record { tipo, .. }
            | Air::RecordUpdate { tipo, .. }
            | Air::RecordAccess { tipo, .. }
            | Air::ListAccessor { tipo, .. }
            | Air::ListExpose { tipo, .. }
            | Air::TupleAccessor { tipo, .. }
            | Air::TupleIndex { tipo, .. }
            | Air::Todo { tipo, .. }
            | Air::ErrorTerm { tipo, .. }
            | Air::Trace { tipo, .. } => Some(tipo.clone()),

            Air::DefineFunc { .. }
            | Air::Fn { .. }
            | Air::Let { .. }
            | Air::WrapClause { .. }
            | Air::AssertConstr { .. }
            | Air::Finally { .. }
            | Air::FieldsExpose { .. } => None,

            Air::UnOp { op, .. } => match op {
                UnOp::Not => Some(
                    Type::App {
                        public: true,
                        module: String::new(),
                        name: "Bool".to_string(),
                        args: vec![],
                    }
                    .into(),
                ),
                UnOp::Negate => Some(
                    Type::App {
                        public: true,
                        module: String::new(),
                        name: "Int".to_string(),
                        args: vec![],
                    }
                    .into(),
                ),
            },
        }
    }
}
