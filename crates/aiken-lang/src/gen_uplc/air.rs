use indexmap::IndexSet;
use std::sync::Arc;
use uplc::builtins::DefaultFunction;

use crate::{
    ast::{BinOp, UnOp},
    tipo::{Type, ValueConstructor},
};

use super::scope::Scope;

#[derive(Debug, Clone, PartialEq)]
pub enum Air {
    // Primitives
    Int {
        scope: Scope,
        value: String,
    },
    String {
        scope: Scope,
        value: String,
    },
    ByteArray {
        scope: Scope,
        bytes: Vec<u8>,
    },
    Bool {
        scope: Scope,
        value: bool,
    },
    List {
        scope: Scope,
        count: usize,
        tipo: Arc<Type>,
        tail: bool,
    },
    Tuple {
        scope: Scope,
        tipo: Arc<Type>,
        count: usize,
    },
    Void {
        scope: Scope,
    },
    Var {
        scope: Scope,
        constructor: ValueConstructor,
        name: String,
        variant_name: String,
    },
    // Functions
    Call {
        scope: Scope,
        count: usize,
        tipo: Arc<Type>,
    },
    DefineFunc {
        scope: Scope,
        func_name: String,
        module_name: String,
        params: Vec<String>,
        recursive: bool,
        variant_name: String,
    },
    Fn {
        scope: Scope,
        params: Vec<String>,
    },
    Builtin {
        scope: Scope,
        count: usize,
        func: DefaultFunction,
        tipo: Arc<Type>,
    },
    // Operators
    BinOp {
        scope: Scope,
        name: BinOp,
        tipo: Arc<Type>,
    },
    UnOp {
        scope: Scope,
        op: UnOp,
    },
    // Assignment
    Let {
        scope: Scope,
        name: String,
    },
    UnWrapData {
        scope: Scope,
        tipo: Arc<Type>,
    },
    WrapData {
        scope: Scope,
        tipo: Arc<Type>,
    },
    AssertConstr {
        scope: Scope,
        constr_index: usize,
    },
    AssertBool {
        scope: Scope,
        is_true: bool,
    },
    // When
    When {
        scope: Scope,
        tipo: Arc<Type>,
        subject_name: String,
    },
    Clause {
        scope: Scope,
        tipo: Arc<Type>,
        subject_name: String,
        complex_clause: bool,
    },
    ListClause {
        scope: Scope,
        tipo: Arc<Type>,
        tail_name: String,
        next_tail_name: Option<String>,
        complex_clause: bool,
    },
    WrapClause {
        scope: Scope,
    },
    TupleClause {
        scope: Scope,
        tipo: Arc<Type>,
        indices: IndexSet<(usize, String)>,
        predefined_indices: IndexSet<(usize, String)>,
        subject_name: String,
        count: usize,
        complex_clause: bool,
    },
    ClauseGuard {
        scope: Scope,
        subject_name: String,
        tipo: Arc<Type>,
    },
    ListClauseGuard {
        scope: Scope,
        tipo: Arc<Type>,
        tail_name: String,
        next_tail_name: Option<String>,
        inverse: bool,
    },
    Finally {
        scope: Scope,
    },
    // If
    If {
        scope: Scope,
        tipo: Arc<Type>,
    },
    // Record Creation
    Record {
        scope: Scope,
        tag: usize,
        tipo: Arc<Type>,
        count: usize,
    },
    RecordUpdate {
        scope: Scope,
        highest_index: usize,
        indices: Vec<(usize, Arc<Type>)>,
        tipo: Arc<Type>,
    },
    // Field Access
    RecordAccess {
        scope: Scope,
        record_index: u64,
        tipo: Arc<Type>,
    },
    FieldsExpose {
        scope: Scope,
        indices: Vec<(usize, String, Arc<Type>)>,
        check_last_item: bool,
    },
    // ListAccess
    ListAccessor {
        scope: Scope,
        tipo: Arc<Type>,
        names: Vec<String>,
        tail: bool,
        check_last_item: bool,
    },
    ListExpose {
        scope: Scope,
        tipo: Arc<Type>,
        tail_head_names: Vec<(String, String)>,
        tail: Option<(String, String)>,
    },
    // Tuple Access
    TupleAccessor {
        scope: Scope,
        names: Vec<String>,
        tipo: Arc<Type>,
        check_last_item: bool,
    },
    TupleIndex {
        scope: Scope,
        tipo: Arc<Type>,
        tuple_index: usize,
    },
    // Misc.
    ErrorTerm {
        scope: Scope,
        tipo: Arc<Type>,
    },
    Trace {
        scope: Scope,
        tipo: Arc<Type>,
    },
    Noop {
        scope: Scope,
    },
    FieldsEmpty {
        scope: Scope,
    },
}

impl Air {
    pub fn scope(&self) -> Scope {
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
            | Air::AssertBool { scope, .. }
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
            | Air::FieldsEmpty { scope }
            | Air::ListAccessor { scope, .. }
            | Air::ListExpose { scope, .. }
            | Air::TupleAccessor { scope, .. }
            | Air::TupleIndex { scope, .. }
            | Air::ErrorTerm { scope, .. }
            | Air::Trace { scope, .. }
            | Air::Noop { scope } => scope.clone(),
        }
    }
    pub fn scope_mut(&mut self) -> &mut Scope {
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
            | Air::AssertBool { scope, .. }
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
            | Air::FieldsEmpty { scope }
            | Air::ListAccessor { scope, .. }
            | Air::ListExpose { scope, .. }
            | Air::TupleAccessor { scope, .. }
            | Air::TupleIndex { scope, .. }
            | Air::ErrorTerm { scope, .. }
            | Air::Trace { scope, .. }
            | Air::Noop { scope } => scope,
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
            | Air::Noop { .. } => None,
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
