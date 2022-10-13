use std::{collections::HashMap, fmt, ops::Range, sync::Arc};

use internment::Intern;

use crate::{
    expr::{TypedExpr, UntypedExpr},
    tipo::{self, PatternConstructor, Type, ValueConstructor},
};

pub const CAPTURE_VARIABLE: &str = "_capture";

pub type TypedModule = Module<tipo::Module, TypedDefinition>;
pub type UntypedModule = Module<(), UntypedDefinition>;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ModuleKind {
    Lib,
    Script,
}

impl ModuleKind {
    pub fn is_script(&self) -> bool {
        matches!(self, ModuleKind::Script)
    }

    pub fn is_lib(&self) -> bool {
        matches!(self, ModuleKind::Lib)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module<Info, Definitions> {
    pub name: String,
    pub docs: Vec<String>,
    pub type_info: Info,
    pub definitions: Vec<Definitions>,
    pub kind: ModuleKind,
}

impl UntypedModule {
    pub fn dependencies(&self) -> Vec<(String, Span)> {
        self.definitions()
            .flat_map(|def| {
                if let Definition::Use {
                    location, module, ..
                } = def
                {
                    Some((module.join("/"), *location))
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn definitions(&self) -> impl Iterator<Item = &UntypedDefinition> {
        self.definitions.iter()
    }

    pub fn into_definitions(self) -> impl Iterator<Item = UntypedDefinition> {
        self.definitions.into_iter()
    }
}

pub type TypedDefinition = Definition<Arc<Type>, TypedExpr, String, String>;
pub type UntypedDefinition = Definition<(), UntypedExpr, (), ()>;

#[derive(Debug, Clone, PartialEq)]
pub enum Definition<T, Expr, ConstantRecordTag, PackageName> {
    Fn {
        arguments: Vec<Arg<T>>,
        body: Expr,
        doc: Option<String>,
        location: Span,
        name: String,
        public: bool,
        return_annotation: Option<Annotation>,
        return_type: T,
    },

    TypeAlias {
        alias: String,
        annotation: Annotation,
        doc: Option<String>,
        location: Span,
        parameters: Vec<String>,
        public: bool,
        tipo: T,
    },

    DataType {
        constructors: Vec<RecordConstructor<T>>,
        doc: Option<String>,
        location: Span,
        name: String,
        opaque: bool,
        parameters: Vec<String>,
        public: bool,
        typed_parameters: Vec<T>,
    },

    Use {
        as_name: Option<String>,
        location: Span,
        module: Vec<String>,
        package: PackageName,
        unqualified: Vec<UnqualifiedImport>,
    },

    ModuleConstant {
        doc: Option<String>,
        location: Span,
        public: bool,
        name: String,
        annotation: Option<Annotation>,
        value: Box<Constant<T, ConstantRecordTag>>,
        tipo: T,
    },
}

pub type TypedConstant = Constant<Arc<Type>, String>;
pub type UntypedConstant = Constant<(), ()>;

#[derive(Debug, Clone, PartialEq)]
pub enum Constant<T, RecordTag> {
    Int {
        location: Span,
        value: String,
    },

    String {
        location: Span,
        value: String,
    },

    Pair {
        location: Span,
        elements: Vec<Self>,
    },

    List {
        location: Span,
        elements: Vec<Self>,
        tipo: T,
    },

    Record {
        location: Span,
        module: Option<String>,
        name: String,
        args: Vec<CallArg<Self>>,
        tag: RecordTag,
        tipo: T,
        field_map: Option<FieldMap>,
    },

    ByteString {
        location: Span,
        // segments: Vec<BitStringSegment<Self, T>>,
    },

    Var {
        location: Span,
        module: Option<String>,
        name: String,
        constructor: Option<Box<ValueConstructor>>,
        tipo: T,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallArg<A> {
    pub label: Option<String>,
    pub location: Span,
    pub value: A,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldMap {
    pub arity: usize,
    pub fields: HashMap<String, usize>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RecordConstructor<T> {
    pub location: Span,
    pub name: String,
    pub arguments: Vec<RecordConstructorArg<T>>,
    pub documentation: Option<String>,
    pub sugar: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RecordConstructorArg<T> {
    pub label: Option<String>,
    // ast
    pub annotation: Annotation,
    pub location: Span,
    pub tipo: T,
    pub doc: Option<String>,
}

pub type UntypedArg = Arg<()>;

#[derive(Debug, Clone, PartialEq)]
pub struct Arg<T> {
    pub arg_name: ArgName,
    pub location: Span,
    pub annotation: Option<Annotation>,
    pub tipo: T,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ArgName {
    Discard {
        name: String,
        location: Span,
    },
    LabeledDiscard {
        label: String,
        name: String,
        location: Span,
    },
    Named {
        name: String,
        location: Span,
    },
    NamedLabeled {
        name: String,
        label: String,
        location: Span,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnqualifiedImport {
    pub location: Span,
    pub name: String,
    pub as_name: Option<String>,
    pub layer: Layer,
}

// TypeAst
#[derive(Debug, Clone, PartialEq)]
pub enum Annotation {
    Constructor {
        location: Span,
        module: Option<String>,
        name: String,
        arguments: Vec<Self>,
    },

    Fn {
        location: Span,
        arguments: Vec<Self>,
        ret: Box<Self>,
    },

    Var {
        location: Span,
        name: String,
    },

    Tuple {
        location: Span,
        elems: Vec<Self>,
    },

    Hole {
        location: Span,
        name: String,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Layer {
    Value,
    Type,
}

impl Default for Layer {
    fn default() -> Self {
        Layer::Value
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinOp {
    // Boolean logic
    And,
    Or,

    // Equality
    Eq,
    NotEq,

    // Order comparison
    LtInt,
    LtEqInt,
    GtEqInt,
    GtInt,

    // Maths
    AddInt,
    SubInt,
    MultInt,
    DivInt,
    ModInt,
}

pub type UntypedPattern = Pattern<(), ()>;
pub type TypedPattern = Pattern<PatternConstructor, Arc<Type>>;

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern<Constructor, Type> {
    Int {
        location: Span,
        value: String,
    },

    Float {
        location: Span,
        value: String,
    },

    String {
        location: Span,
        value: String,
    },

    /// The creation of a variable.
    /// e.g. `assert [this_is_a_var, .._] = x`
    Var {
        location: Span,
        name: String,
    },

    /// A reference to a variable in a bit string. This is always a variable
    /// being used rather than a new variable being assigned.
    VarUsage {
        location: Span,
        name: String,
        tipo: Type,
    },

    /// A name given to a sub-pattern using the `as` keyword.
    /// e.g. `assert #(1, [_, _] as the_list) = x`
    Assign {
        name: String,
        location: Span,
        pattern: Box<Self>,
    },

    /// A pattern that binds to any value but does not assign a variable.
    /// Always starts with an underscore.
    Discard {
        name: String,
        location: Span,
    },

    List {
        location: Span,
        elements: Vec<Self>,
        tail: Option<Box<Self>>,
    },

    /// The constructor for a custom type. Starts with an uppercase letter.
    Constructor {
        location: Span,
        name: String,
        arguments: Vec<CallArg<Self>>,
        module: Option<String>,
        constructor: Constructor,
        with_spread: bool,
        tipo: Type,
    },

    Tuple {
        location: Span,
        elems: Vec<Self>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AssignmentKind {
    Let,
    Assert,
}

pub type MultiPattern<PatternConstructor, Type> = Vec<Pattern<PatternConstructor, Type>>;

pub type UntypedMultiPattern = MultiPattern<(), ()>;
pub type TypedMultiPattern = MultiPattern<PatternConstructor, Arc<Type>>;

pub type TypedClause = Clause<TypedExpr, PatternConstructor, Arc<Type>, String>;

pub type UntypedClause = Clause<UntypedExpr, (), (), ()>;

#[derive(Debug, Clone, PartialEq)]
pub struct Clause<Expr, PatternConstructor, Type, RecordTag> {
    pub location: Span,
    pub pattern: MultiPattern<PatternConstructor, Type>,
    pub alternative_patterns: Vec<MultiPattern<PatternConstructor, Type>>,
    pub guard: Option<ClauseGuard<Type, RecordTag>>,
    pub then: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ClauseGuard<Type, RecordTag> {
    Equals {
        location: Span,
        left: Box<Self>,
        right: Box<Self>,
    },

    NotEquals {
        location: Span,
        left: Box<Self>,
        right: Box<Self>,
    },

    GtInt {
        location: Span,
        left: Box<Self>,
        right: Box<Self>,
    },

    GtEqInt {
        location: Span,
        left: Box<Self>,
        right: Box<Self>,
    },

    LtInt {
        location: Span,
        left: Box<Self>,
        right: Box<Self>,
    },

    LtEqInt {
        location: Span,
        left: Box<Self>,
        right: Box<Self>,
    },

    Or {
        location: Span,
        left: Box<Self>,
        right: Box<Self>,
    },

    And {
        location: Span,
        left: Box<Self>,
        right: Box<Self>,
    },

    Var {
        location: Span,
        tipo: Type,
        name: String,
    },

    TupleIndex {
        location: Span,
        index: u64,
        tipo: Type,
        tuple: Box<Self>,
    },

    Constant(Constant<Type, RecordTag>),
}

pub type TypedIfBranch = IfBranch<TypedExpr>;
pub type UntypedIfBranch = IfBranch<UntypedExpr>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IfBranch<Expr> {
    pub condition: Expr,
    pub body: Expr,
    pub location: Span,
}

#[derive(Debug)]
pub struct TypedRecordUpdateArg {
    pub label: String,
    pub location: Span,
    pub value: TypedExpr,
    pub index: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UntypedRecordUpdateArg {
    pub label: String,
    pub location: Span,
    pub value: UntypedExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RecordUpdateSpread {
    pub base: Box<UntypedExpr>,
    pub location: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TodoKind {
    Keyword,
    EmptyFunction,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct SrcId(Intern<Vec<String>>);

impl SrcId {
    pub fn empty() -> Self {
        SrcId(Intern::new(Vec::new()))
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Span {
    pub src: SrcId,
    pub start: usize,
    pub end: usize,
}

impl From<Span> for miette::SourceSpan {
    fn from(span: Span) -> Self {
        Self::new(span.start.into(), (span.end - span.start).into())
    }
}

impl Span {
    pub fn empty() -> Self {
        use chumsky::Span;

        Self::new(SrcId::empty(), 0..0)
    }

    pub fn src(&self) -> SrcId {
        self.src
    }

    pub fn range(&self) -> Range<usize> {
        use chumsky::Span;

        self.start()..self.end()
    }

    pub fn union(self, other: Self) -> Self {
        use chumsky::Span;

        assert_eq!(
            self.src, other.src,
            "attempted to union spans with different sources"
        );

        Self {
            start: self.start().min(other.start()),
            end: self.end().max(other.end()),
            ..self
        }
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}:{:?}", self.src, self.range())
    }
}

impl chumsky::Span for Span {
    type Context = SrcId;

    type Offset = usize;

    fn new(context: Self::Context, range: Range<Self::Offset>) -> Self {
        assert!(range.start <= range.end);

        Self {
            src: context,
            start: range.start,
            end: range.end,
        }
    }

    fn context(&self) -> Self::Context {
        self.src
    }

    fn start(&self) -> Self::Offset {
        self.start
    }

    fn end(&self) -> Self::Offset {
        self.end
    }
}
