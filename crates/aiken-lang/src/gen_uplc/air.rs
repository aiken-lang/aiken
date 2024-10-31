use crate::{
    ast::{BinOp, Curve, UnOp},
    tipo::{Type, ValueConstructor},
};
use indexmap::IndexSet;
use std::rc::Rc;
use uplc::builtins::DefaultFunction;

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum ExpectLevel {
    Full,
    Items,
    None,
}

impl From<bool> for ExpectLevel {
    fn from(value: bool) -> Self {
        if value {
            ExpectLevel::Full
        } else {
            ExpectLevel::None
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum FunctionVariants {
    Standard(Vec<String>),
    Recursive {
        params: Vec<String>,
        recursive_nonstatic_params: Vec<String>,
    },
    Cyclic(Vec<Vec<String>>),
}

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
    CurvePoint {
        point: Curve,
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
    Pair {
        tipo: Rc<Type>,
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
        variant_name: String,
        variant: FunctionVariants,
    },
    Fn {
        params: Vec<String>,
        allow_inline: bool,
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
    SoftCastLet {
        name: String,
        tipo: Rc<Type>,
    },
    CastFromData {
        tipo: Rc<Type>,
        full_cast: bool,
    },
    CastToData {
        tipo: Rc<Type>,
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
    },
    ListClause {
        subject_tipo: Rc<Type>,
        tail_name: String,
        next_tail_name: Option<(String, String)>,
    },
    WrapClause,
    TupleClause {
        subject_tipo: Rc<Type>,
        indices: IndexSet<(usize, String)>,
        predefined_indices: IndexSet<(usize, String)>,
        subject_name: String,
        complex_clause: bool,
    },
    PairClause {
        subject_tipo: Rc<Type>,
        subject_name: String,
        fst_name: Option<String>,
        snd_name: Option<String>,
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
    PairGuard {
        subject_tipo: Rc<Type>,
        subject_name: String,
        fst_name: Option<String>,
        snd_name: Option<String>,
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
        is_expect: bool,
    },
    // ListAccess
    ListAccessor {
        tipo: Rc<Type>,
        names: Vec<String>,
        tail: bool,
        expect_level: ExpectLevel,
    },
    // Tuple Access
    TupleAccessor {
        names: Vec<String>,
        tipo: Rc<Type>,
        is_expect: bool,
    },
    // Pair Access
    PairAccessor {
        fst: Option<String>,
        snd: Option<String>,
        tipo: Rc<Type>,
        is_expect: bool,
    },
    ExtractField {
        tipo: Rc<Type>,
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
