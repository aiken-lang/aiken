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
        recursive_nonstatic_params: Vec<String>,
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
        argument_tipo: Arc<Type>,
    },
    UnOp {
        op: UnOp,
    },
    // Assignment
    Let {
        name: String,
    },
    CastFromData {
        tipo: Arc<Type>,
    },
    CastToData {
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
        subject_tipo: Arc<Type>,
    },
    Clause {
        subject_tipo: Arc<Type>,
        subject_name: String,
        complex_clause: bool,
    },
    ListClause {
        subject_tipo: Arc<Type>,
        tail_name: String,
        next_tail_name: Option<(String, String)>,
        complex_clause: bool,
    },
    WrapClause,
    TupleClause {
        subject_tipo: Arc<Type>,
        indices: IndexSet<(usize, String)>,
        predefined_indices: IndexSet<(usize, String)>,
        subject_name: String,
        complex_clause: bool,
    },
    ClauseGuard {
        subject_name: String,
        subject_tipo: Arc<Type>,
    },
    ListClauseGuard {
        subject_tipo: Arc<Type>,
        tail_name: String,
        next_tail_name: Option<String>,
        inverse: bool,
    },
    TupleGuard {
        subject_tipo: Arc<Type>,
        indices: IndexSet<(usize, String)>,
        subject_name: String,
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
