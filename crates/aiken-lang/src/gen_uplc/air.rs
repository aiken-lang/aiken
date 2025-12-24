use crate::{
    ast::{BinOp, Curve, SourceLocation, UnOp},
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
        location: SourceLocation,
    },
    String {
        value: String,
        location: SourceLocation,
    },
    ByteArray {
        bytes: Vec<u8>,
        location: SourceLocation,
    },
    CurvePoint {
        point: Curve,
        location: SourceLocation,
    },
    Bool {
        value: bool,
        location: SourceLocation,
    },
    List {
        count: usize,
        tipo: Rc<Type>,
        tail: bool,
        location: SourceLocation,
    },
    Tuple {
        tipo: Rc<Type>,
        count: usize,
        location: SourceLocation,
    },
    Pair {
        tipo: Rc<Type>,
        location: SourceLocation,
    },
    Void {
        location: SourceLocation,
    },
    Var {
        constructor: ValueConstructor,
        name: String,
        variant_name: String,
        location: SourceLocation,
    },
    // Functions
    Call {
        count: usize,
        tipo: Rc<Type>,
        location: SourceLocation,
    },
    DefineFunc {
        func_name: String,
        module_name: String,
        variant_name: String,
        variant: FunctionVariants,
        location: SourceLocation,
    },
    Fn {
        params: Vec<String>,
        allow_inline: bool,
        location: SourceLocation,
    },
    Builtin {
        count: usize,
        func: DefaultFunction,
        tipo: Rc<Type>,
        location: SourceLocation,
    },
    // Operators
    BinOp {
        name: BinOp,
        tipo: Rc<Type>,
        left_tipo: Rc<Type>,
        right_tipo: Rc<Type>,
        location: SourceLocation,
    },
    UnOp {
        op: UnOp,
        location: SourceLocation,
    },
    // Assignment
    Let {
        name: String,
        location: SourceLocation,
    },
    // These 3 will need to look up the
    // decorators
    SoftCastLet {
        name: String,
        tipo: Rc<Type>,
        location: SourceLocation,
    },
    CastFromData {
        tipo: Rc<Type>,
        full_cast: bool,
        location: SourceLocation,
    },
    CastToData {
        tipo: Rc<Type>,
        location: SourceLocation,
    },
    AssertBool {
        is_true: bool,
        location: SourceLocation,
    },
    // When
    // If using list decorator this does nothing
    When {
        tipo: Rc<Type>,
        subject_name: String,
        subject_tipo: Rc<Type>,
        location: SourceLocation,
    },
    Clause {
        subject_tipo: Rc<Type>,
        subject_name: String,
        location: SourceLocation,
    },
    ListClause {
        subject_tipo: Rc<Type>,
        tail_name: String,
        next_tail_name: Option<(String, String)>,
        location: SourceLocation,
    },
    // If
    If {
        tipo: Rc<Type>,
        location: SourceLocation,
    },
    // Record Creation
    Constr {
        tag: Option<usize>,
        tipo: Rc<Type>,
        count: usize,
        location: SourceLocation,
    },
    RecordUpdate {
        highest_index: usize,
        indices: Vec<(usize, Rc<Type>)>,
        tipo: Rc<Type>,
        location: SourceLocation,
    },
    // Field Access
    FieldsExpose {
        list_decorator: bool,
        indices: Vec<(usize, String, Rc<Type>)>,
        is_expect: bool,
        location: SourceLocation,
    },
    // ListAccess
    ListAccessor {
        tipo: Rc<Type>,
        names: Vec<String>,
        tail: bool,
        expect_level: ExpectLevel,
        location: SourceLocation,
    },
    // Tuple Access
    TupleAccessor {
        names: Vec<String>,
        tipo: Rc<Type>,
        is_expect: bool,
        location: SourceLocation,
    },
    // Pair Access
    PairAccessor {
        fst: Option<String>,
        snd: Option<String>,
        tipo: Rc<Type>,
        is_expect: bool,
        location: SourceLocation,
    },
    ExtractField {
        tipo: Rc<Type>,
        location: SourceLocation,
    },
    // Misc.
    ErrorTerm {
        tipo: Rc<Type>,
        validator: bool,
        location: SourceLocation,
    },
    Trace {
        tipo: Rc<Type>,
        location: SourceLocation,
    },
    NoOp {
        location: SourceLocation,
    },
    FieldsEmpty {
        list_decorator: bool,
        location: SourceLocation,
    },
    ListEmpty {
        location: SourceLocation,
    },
}
