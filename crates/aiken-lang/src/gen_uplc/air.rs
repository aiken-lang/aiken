use crate::{
    ast::{BinOp, Curve, Span, UnOp},
    tipo::{Type, ValueConstructor},
};
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
        location: Span,
    },
    String {
        value: String,
        location: Span,
    },
    ByteArray {
        bytes: Vec<u8>,
        location: Span,
    },
    CurvePoint {
        point: Curve,
        location: Span,
    },
    Bool {
        value: bool,
        location: Span,
    },
    List {
        count: usize,
        tipo: Rc<Type>,
        tail: bool,
        location: Span,
    },
    Tuple {
        tipo: Rc<Type>,
        count: usize,
        location: Span,
    },
    Pair {
        tipo: Rc<Type>,
        location: Span,
    },
    Void {
        location: Span,
    },
    Var {
        constructor: ValueConstructor,
        name: String,
        variant_name: String,
        location: Span,
    },
    // Functions
    Call {
        count: usize,
        tipo: Rc<Type>,
        location: Span,
    },
    DefineFunc {
        func_name: String,
        module_name: String,
        variant_name: String,
        variant: FunctionVariants,
        location: Span,
    },
    Fn {
        params: Vec<String>,
        allow_inline: bool,
        location: Span,
    },
    Builtin {
        count: usize,
        func: DefaultFunction,
        tipo: Rc<Type>,
        location: Span,
    },
    // Operators
    BinOp {
        name: BinOp,
        tipo: Rc<Type>,
        left_tipo: Rc<Type>,
        right_tipo: Rc<Type>,
        location: Span,
    },
    UnOp {
        op: UnOp,
        location: Span,
    },
    // Assignment
    Let {
        name: String,
        location: Span,
    },
    // These 3 will need to look up the
    // decorators
    SoftCastLet {
        name: String,
        tipo: Rc<Type>,
        location: Span,
    },
    CastFromData {
        tipo: Rc<Type>,
        full_cast: bool,
        location: Span,
    },
    CastToData {
        tipo: Rc<Type>,
        location: Span,
    },
    AssertBool {
        is_true: bool,
        location: Span,
    },
    // When
    // If using list decorator this does nothing
    When {
        tipo: Rc<Type>,
        subject_name: String,
        subject_tipo: Rc<Type>,
        location: Span,
    },
    Clause {
        subject_tipo: Rc<Type>,
        subject_name: String,
        location: Span,
    },
    ListClause {
        subject_tipo: Rc<Type>,
        tail_name: String,
        next_tail_name: Option<(String, String)>,
        location: Span,
    },
    // If
    If {
        tipo: Rc<Type>,
        location: Span,
    },
    // Record Creation
    Constr {
        tag: Option<usize>,
        tipo: Rc<Type>,
        count: usize,
        location: Span,
    },
    RecordUpdate {
        highest_index: usize,
        indices: Vec<(usize, Rc<Type>)>,
        tipo: Rc<Type>,
        location: Span,
    },
    // Field Access
    FieldsExpose {
        list_decorator: bool,
        indices: Vec<(usize, String, Rc<Type>)>,
        is_expect: bool,
        location: Span,
    },
    // ListAccess
    ListAccessor {
        tipo: Rc<Type>,
        names: Vec<String>,
        tail: bool,
        expect_level: ExpectLevel,
        location: Span,
    },
    // Tuple Access
    TupleAccessor {
        names: Vec<String>,
        tipo: Rc<Type>,
        is_expect: bool,
        location: Span,
    },
    // Pair Access
    PairAccessor {
        fst: Option<String>,
        snd: Option<String>,
        tipo: Rc<Type>,
        is_expect: bool,
        location: Span,
    },
    ExtractField {
        tipo: Rc<Type>,
        location: Span,
    },
    // Misc.
    ErrorTerm {
        tipo: Rc<Type>,
        validator: bool,
        location: Span,
    },
    Trace {
        tipo: Rc<Type>,
        location: Span,
    },
    NoOp {
        location: Span,
    },
    FieldsEmpty {
        list_decorator: bool,
        location: Span,
    },
    ListEmpty {
        location: Span,
    },
}
