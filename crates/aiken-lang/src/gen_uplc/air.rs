use indexmap::IndexSet;
use std::rc::Rc;
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
        tipo: Rc<Type>,
        tail: bool,
    },
    Tuple {
        tipo: Rc<Type>,
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
        tipo: Rc<Type>,
    },
    DefineFunc {
        func_name: String,
        module_name: String,
        params: Vec<String>,
        recursive: bool,
        recursive_nonstatic_params: Vec<String>,
        variant_name: String,
    },
    DefineCyclicFuncs {
        func_name: String,
        module_name: String,
        variant_name: String,
        // just the params
        contained_functions: Vec<Vec<String>>,
    },
    Fn {
        params: Vec<String>,
    },
    Builtin {
        count: usize,
        func: DefaultFunction,
        tipo: Rc<Type>,
    },
    // Operators
    BinOp {
        name: BinOp,
        tipo: Rc<Type>,
        argument_tipo: Rc<Type>,
    },
    UnOp {
        op: UnOp,
    },
    // Assignment
    Let {
        name: String,
    },
    CastFromData {
        tipo: Rc<Type>,
    },
    CastToData {
        tipo: Rc<Type>,
    },
    AssertConstr {
        constr_index: usize,
    },
    AssertBool {
        is_true: bool,
    },
    // When
    When {
        tipo: Rc<Type>,
        subject_name: String,
        subject_tipo: Rc<Type>,
    },
    Clause {
        subject_tipo: Rc<Type>,
        subject_name: String,
        complex_clause: bool,
    },
    ListClause {
        subject_tipo: Rc<Type>,
        tail_name: String,
        next_tail_name: Option<(String, String)>,
        complex_clause: bool,
    },
    WrapClause,
    TupleClause {
        subject_tipo: Rc<Type>,
        indices: IndexSet<(usize, String)>,
        predefined_indices: IndexSet<(usize, String)>,
        subject_name: String,
        complex_clause: bool,
    },
    ClauseGuard {
        subject_name: String,
        subject_tipo: Rc<Type>,
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
    Finally,
    // If
    If {
        tipo: Rc<Type>,
    },
    // Record Creation
    Constr {
        tag: usize,
        tipo: Rc<Type>,
        count: usize,
    },
    RecordUpdate {
        highest_index: usize,
        indices: Vec<(usize, Rc<Type>)>,
        tipo: Rc<Type>,
    },
    // Field Access
    FieldsExpose {
        indices: Vec<(usize, String, Rc<Type>)>,
        check_last_item: bool,
    },
    // ListAccess
    ListAccessor {
        tipo: Rc<Type>,
        names: Vec<String>,
        tail: bool,
        check_last_item: bool,
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
    },
    // Misc.
    ErrorTerm {
        tipo: Rc<Type>,
        validator: bool,
    },
    Trace {
        tipo: Rc<Type>,
    },
    NoOp,
    FieldsEmpty,
    ListEmpty,
}
