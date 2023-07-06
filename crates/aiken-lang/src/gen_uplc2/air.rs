use indexmap::IndexSet;
use std::sync::Arc;
use uplc::builtins::DefaultFunction;

use crate::{
    ast::{BinOp, UnOp},
    tipo::{Type, ValueConstructor},
};

#[derive(Debug, Clone, PartialEq)]
pub enum Air {
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
        count: usize,
        tipo: Arc<Type>,
        tail: bool,
    },
    Tuple {
        tipo: Arc<Type>,
        count: usize,
    },
    Void,
    Var {
        constructor: ValueConstructor,
        name: String,
        variant_name: String,
    },
    // Functions
    Call {
        count: usize,
        tipo: Arc<Type>,
    },
    DefineFunc {
        func_name: String,
        module_name: String,
        params: Vec<String>,
        recursive: bool,
        variant_name: String,
    },
    Fn {
        params: Vec<String>,
    },
    Builtin {
        count: usize,
        func: DefaultFunction,
        tipo: Arc<Type>,
    },
    // Operators
    BinOp {
        name: BinOp,
        tipo: Arc<Type>,
    },
    UnOp {
        op: UnOp,
    },
    // Assignment
    Let {
        name: String,
    },
    UnWrapData {
        tipo: Arc<Type>,
    },
    WrapData {
        tipo: Arc<Type>,
    },
    AssertConstr {
        constr_index: usize,
    },
    AssertBool {
        is_true: bool,
    },
    // When
    When {
        tipo: Arc<Type>,
        subject_name: String,
    },
    Clause {
        tipo: Arc<Type>,
        subject_name: String,
        complex_clause: bool,
    },
    ListClause {
        tipo: Arc<Type>,
        tail_name: String,
        next_tail_name: Option<String>,
        complex_clause: bool,
    },
    WrapClause,
    TupleClause {
        tipo: Arc<Type>,
        indices: IndexSet<(usize, String)>,
        predefined_indices: IndexSet<(usize, String)>,
        subject_name: String,
        count: usize,
        complex_clause: bool,
    },
    ClauseGuard {
        subject_name: String,
        tipo: Arc<Type>,
    },
    ListClauseGuard {
        tipo: Arc<Type>,
        tail_name: String,
        next_tail_name: Option<String>,
        inverse: bool,
    },
    TupleGuard {
        tipo: Arc<Type>,
        indices: IndexSet<(usize, String)>,
        subject_name: String,
        type_count: usize,
    },
    Finally,
    // If
    If {
        tipo: Arc<Type>,
    },
    // Record Creation
    Constr {
        tag: usize,
        tipo: Arc<Type>,
        count: usize,
    },
    RecordUpdate {
        highest_index: usize,
        indices: Vec<(usize, Arc<Type>)>,
        tipo: Arc<Type>,
    },
    // Field Access
    RecordAccess {
        record_index: u64,
        tipo: Arc<Type>,
    },
    FieldsExpose {
        indices: Vec<(usize, String, Arc<Type>)>,
        check_last_item: bool,
    },
    // ListAccess
    ListAccessor {
        tipo: Arc<Type>,
        names: Vec<String>,
        tail: bool,
        check_last_item: bool,
    },
    ListExpose {
        tipo: Arc<Type>,
        tail_head_names: Vec<(String, String)>,
        tail: Option<(String, String)>,
    },
    // Tuple Access
    TupleAccessor {
        names: Vec<String>,
        tipo: Arc<Type>,
        check_last_item: bool,
    },
    TupleIndex {
        tipo: Arc<Type>,
        tuple_index: usize,
    },
    // Misc.
    ErrorTerm {
        tipo: Arc<Type>,
    },
    Trace {
        tipo: Arc<Type>,
    },
    NoOp,
    FieldsEmpty,
    ListEmpty,
}

impl Air {
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
            | Air::TupleGuard { tipo, .. }
            | Air::If { tipo, .. }
            | Air::ListClauseGuard { tipo, .. }
            | Air::Constr { tipo, .. }
            | Air::RecordUpdate { tipo, .. }
            | Air::RecordAccess { tipo, .. }
            | Air::ListAccessor { tipo, .. }
            | Air::ListExpose { tipo, .. }
            | Air::TupleAccessor { tipo, .. }
            | Air::TupleIndex { tipo, .. }
            | Air::ErrorTerm { tipo, .. }
            | Air::Trace { tipo, .. } => Some(tipo.clone()),
            Air::DefineFunc { .. }
            | Air::Fn { .. }
            | Air::Let { .. }
            | Air::WrapClause { .. }
            | Air::AssertConstr { .. }
            | Air::AssertBool { .. }
            | Air::Finally { .. }
            | Air::FieldsExpose { .. }
            | Air::FieldsEmpty { .. }
            | Air::ListEmpty { .. }
            | Air::NoOp { .. } => None,
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
