use std::{fmt, ops::Range, sync::Arc};

use crate::{
    builtins::{self, bool},
    expr::{TypedExpr, UntypedExpr},
    tipo::{fields::FieldMap, PatternConstructor, Type, TypeInfo, ValueConstructor},
};

pub const ASSERT_VARIABLE: &str = "_try";
pub const CAPTURE_VARIABLE: &str = "_capture";
pub const PIPE_VARIABLE: &str = "_pipe";
pub const TRY_VARIABLE: &str = "_try";

pub type TypedModule = Module<TypeInfo, TypedDefinition>;
pub type UntypedModule = Module<(), UntypedDefinition>;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ModuleKind {
    Lib,
    Validator,
}

impl ModuleKind {
    pub fn is_validator(&self) -> bool {
        matches!(self, ModuleKind::Validator)
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

impl<Info, Definitions> Module<Info, Definitions> {
    pub fn definitions(&self) -> impl Iterator<Item = &Definitions> {
        self.definitions.iter()
    }

    pub fn into_definitions(self) -> impl Iterator<Item = Definitions> {
        self.definitions.into_iter()
    }
}

impl UntypedModule {
    pub fn dependencies(&self) -> Vec<(String, Span)> {
        self.definitions()
            .flat_map(|def| {
                if let Definition::Use(Use {
                    location, module, ..
                }) = def
                {
                    Some((module.join("/"), *location))
                } else {
                    None
                }
            })
            .collect()
    }
}

pub type TypedDefinition = Definition<Arc<Type>, TypedExpr, String, String>;
pub type UntypedDefinition = Definition<(), UntypedExpr, (), ()>;

pub type TypedFunction = Function<Arc<Type>, TypedExpr>;

#[derive(Debug, Clone, PartialEq)]
pub struct Function<T, Expr> {
    pub arguments: Vec<Arg<T>>,
    pub body: Expr,
    pub doc: Option<String>,
    pub location: Span,
    pub name: String,
    pub public: bool,
    pub return_annotation: Option<Annotation>,
    pub return_type: T,
    pub end_position: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeAlias<T> {
    pub alias: String,
    pub annotation: Annotation,
    pub doc: Option<String>,
    pub location: Span,
    pub parameters: Vec<String>,
    pub public: bool,
    pub tipo: T,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DataType<T> {
    pub constructors: Vec<RecordConstructor<T>>,
    pub doc: Option<String>,
    pub location: Span,
    pub name: String,
    pub opaque: bool,
    pub parameters: Vec<String>,
    pub public: bool,
    pub typed_parameters: Vec<T>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Use<PackageName> {
    pub as_name: Option<String>,
    pub location: Span,
    pub module: Vec<String>,
    pub package: PackageName,
    pub unqualified: Vec<UnqualifiedImport>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ModuleConstant<T, ConstantRecordTag> {
    pub doc: Option<String>,
    pub location: Span,
    pub public: bool,
    pub name: String,
    pub annotation: Option<Annotation>,
    pub value: Box<Constant<T, ConstantRecordTag>>,
    pub tipo: T,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Definition<T, Expr, ConstantRecordTag, PackageName> {
    Fn(Function<T, Expr>),

    TypeAlias(TypeAlias<T>),

    DataType(DataType<T>),

    Use(Use<PackageName>),

    ModuleConstant(ModuleConstant<T, ConstantRecordTag>),

    Test(Function<T, Expr>),
}

impl<A, B, C, E> Definition<A, B, C, E> {
    pub fn location(&self) -> Span {
        match self {
            Definition::Fn(Function { location, .. })
            | Definition::Use(Use { location, .. })
            | Definition::TypeAlias(TypeAlias { location, .. })
            | Definition::DataType(DataType { location, .. })
            | Definition::ModuleConstant(ModuleConstant { location, .. })
            | Definition::Test(Function { location, .. }) => *location,
        }
    }

    pub fn put_doc(&mut self, new_doc: String) {
        match self {
            Definition::Use { .. } => (),
            Definition::Fn(Function { doc, .. })
            | Definition::TypeAlias(TypeAlias { doc, .. })
            | Definition::DataType(DataType { doc, .. })
            | Definition::ModuleConstant(ModuleConstant { doc, .. })
            | Definition::Test(Function { doc, .. }) => {
                let _ = std::mem::replace(doc, Some(new_doc));
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct DefinitionLocation<'module> {
    pub module: Option<&'module str>,
    pub span: Span,
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

    Tuple {
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

    ByteArray {
        location: Span,
        bytes: Vec<u8>,
    },

    Var {
        location: Span,
        module: Option<String>,
        name: String,
        constructor: Option<Box<ValueConstructor>>,
        tipo: T,
    },
}

impl TypedConstant {
    pub fn tipo(&self) -> Arc<Type> {
        match self {
            Constant::Int { .. } => builtins::int(),
            Constant::String { .. } => builtins::string(),
            Constant::ByteArray { .. } => builtins::byte_array(),
            Constant::Tuple { elements, .. } => {
                builtins::tuple(elements.iter().map(|e| e.tipo()).collect())
            }
            Constant::List { tipo, .. }
            | Constant::Record { tipo, .. }
            | Constant::Var { tipo, .. } => tipo.clone(),
        }
    }
}

impl<A, B> Constant<A, B> {
    pub fn location(&self) -> Span {
        match self {
            Constant::Int { location, .. }
            | Constant::Tuple { location, .. }
            | Constant::List { location, .. }
            | Constant::String { location, .. }
            | Constant::Record { location, .. }
            | Constant::ByteArray { location, .. }
            | Constant::Var { location, .. } => *location,
        }
    }

    pub fn is_simple(&self) -> bool {
        matches!(
            self,
            Self::Int { .. } | Self::ByteArray { .. } | Self::String { .. }
        )
    }
}

pub type TypedCallArg = CallArg<TypedExpr>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallArg<A> {
    pub label: Option<String>,
    pub location: Span,
    pub value: A,
}

impl CallArg<UntypedExpr> {
    pub fn is_capture_hole(&self) -> bool {
        match &self.value {
            UntypedExpr::Var { ref name, .. } => name.contains(CAPTURE_VARIABLE),
            _ => false,
        }
    }
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

pub type TypedArg = Arg<Arc<Type>>;
pub type UntypedArg = Arg<()>;

#[derive(Debug, Clone, PartialEq)]
pub struct Arg<T> {
    pub arg_name: ArgName,
    pub location: Span,
    pub annotation: Option<Annotation>,
    pub tipo: T,
}

impl<A> Arg<A> {
    pub fn set_type<B>(self, tipo: B) -> Arg<B> {
        Arg {
            tipo,
            arg_name: self.arg_name,
            location: self.location,
            annotation: self.annotation,
        }
    }

    pub fn get_variable_name(&self) -> Option<&str> {
        self.arg_name.get_variable_name()
    }
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

impl ArgName {
    pub fn get_variable_name(&self) -> Option<&str> {
        match self {
            ArgName::Discard { .. } | ArgName::LabeledDiscard { .. } => None,
            ArgName::NamedLabeled { name, .. } | ArgName::Named { name, .. } => Some(name),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnqualifiedImport {
    pub location: Span,
    pub name: String,
    pub as_name: Option<String>,
    pub layer: Layer,
}

impl UnqualifiedImport {
    pub fn variable_name(&self) -> &str {
        self.as_name.as_deref().unwrap_or(&self.name)
    }

    pub fn is_value(&self) -> bool {
        self.layer.is_value()
    }
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

    Hole {
        location: Span,
        name: String,
    },

    Tuple {
        location: Span,
        elems: Vec<Self>,
    },
}

impl Annotation {
    pub fn location(&self) -> Span {
        match self {
            Annotation::Fn { location, .. }
            | Annotation::Tuple { location, .. }
            | Annotation::Var { location, .. }
            | Annotation::Hole { location, .. }
            | Annotation::Constructor { location, .. } => *location,
        }
    }

    pub fn is_logically_equal(&self, other: &Annotation) -> bool {
        match self {
            Annotation::Constructor {
                module,
                name,
                arguments,
                location: _,
            } => match other {
                Annotation::Constructor {
                    module: o_module,
                    name: o_name,
                    arguments: o_arguments,
                    location: _,
                } => {
                    module == o_module
                        && name == o_name
                        && arguments.len() == o_arguments.len()
                        && arguments
                            .iter()
                            .zip(o_arguments)
                            .all(|a| a.0.is_logically_equal(a.1))
                }
                _ => false,
            },
            Annotation::Tuple { elems, location: _ } => match other {
                Annotation::Tuple {
                    elems: o_elems,
                    location: _,
                } => {
                    elems.len() == o_elems.len()
                        && elems
                            .iter()
                            .zip(o_elems)
                            .all(|a| a.0.is_logically_equal(a.1))
                }
                _ => false,
            },
            Annotation::Fn {
                arguments,
                ret,
                location: _,
            } => match other {
                Annotation::Fn {
                    arguments: o_arguments,
                    ret: o_return,
                    location: _,
                } => {
                    arguments.len() == o_arguments.len()
                        && arguments
                            .iter()
                            .zip(o_arguments)
                            .all(|a| a.0.is_logically_equal(a.1))
                        && ret.is_logically_equal(o_return)
                }
                _ => false,
            },
            Annotation::Var { name, location: _ } => match other {
                Annotation::Var {
                    name: o_name,
                    location: _,
                } => name == o_name,
                _ => false,
            },

            Annotation::Hole { name, location: _ } => match other {
                Annotation::Hole {
                    name: o_name,
                    location: _,
                } => name == o_name,
                _ => false,
            },
        }
    }
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

impl Layer {
    /// Returns `true` if the layer is [`Value`].
    pub fn is_value(&self) -> bool {
        matches!(self, Self::Value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

impl BinOp {
    pub fn precedence(&self) -> u8 {
        // Ensure that this matches the other precedence function for guards
        match self {
            Self::Or => 1,

            Self::And => 2,

            Self::Eq | Self::NotEq => 3,

            Self::LtInt | Self::LtEqInt | Self::GtEqInt | Self::GtInt => 4,

            // Pipe is 5
            Self::AddInt | Self::SubInt => 6,

            Self::MultInt | Self::DivInt | Self::ModInt => 7,
        }
    }
}

pub type UntypedPattern = Pattern<(), ()>;
pub type TypedPattern = Pattern<PatternConstructor, Arc<Type>>;

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern<Constructor, Type> {
    Int {
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
        is_record: bool,
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

impl<A, B> Pattern<A, B> {
    pub fn location(&self) -> Span {
        match self {
            Pattern::Assign { pattern, .. } => pattern.location(),
            Pattern::Int { location, .. }
            | Pattern::Var { location, .. }
            | Pattern::VarUsage { location, .. }
            | Pattern::List { location, .. }
            | Pattern::Discard { location, .. }
            | Pattern::String { location, .. }
            | Pattern::Tuple { location, .. }
            // | Pattern::Concatenate { location, .. }
            | Pattern::Constructor { location, .. } => *location,
        }
    }

    /// Returns `true` if the pattern is [`Discard`].
    ///
    /// [`Discard`]: Pattern::Discard
    pub fn is_discard(&self) -> bool {
        matches!(self, Self::Discard { .. })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum AssignmentKind {
    Let,
    Assert,
    Check,
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

impl TypedClause {
    pub fn location(&self) -> Span {
        Span {
            start: self
                .pattern
                .get(0)
                .map(|p| p.location().start)
                .unwrap_or_default(),
            end: self.then.location().end,
        }
    }
}

pub type UntypedClauseGuard = ClauseGuard<(), ()>;
pub type TypedClauseGuard = ClauseGuard<Arc<Type>, String>;

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

    // TupleIndex {
    //     location: Span,
    //     index: u64,
    //     tipo: Type,
    //     tuple: Box<Self>,
    // },
    Constant(Constant<Type, RecordTag>),
}

impl<A, B> ClauseGuard<A, B> {
    pub fn location(&self) -> Span {
        match self {
            ClauseGuard::Constant(constant) => constant.location(),
            ClauseGuard::Or { location, .. }
            | ClauseGuard::And { location, .. }
            | ClauseGuard::Var { location, .. }
            // | ClauseGuard::TupleIndex { location, .. }
            | ClauseGuard::Equals { location, .. }
            | ClauseGuard::NotEquals { location, .. }
            | ClauseGuard::GtInt { location, .. }
            | ClauseGuard::GtEqInt { location, .. }
            | ClauseGuard::LtInt { location, .. }
            | ClauseGuard::LtEqInt { location, .. } => *location,
        }
    }

    pub fn precedence(&self) -> u8 {
        // Ensure that this matches the other precedence function for guards
        match self {
            ClauseGuard::Or { .. } => 1,
            ClauseGuard::And { .. } => 2,

            ClauseGuard::Equals { .. } | ClauseGuard::NotEquals { .. } => 3,

            ClauseGuard::GtInt { .. }
            | ClauseGuard::GtEqInt { .. }
            | ClauseGuard::LtInt { .. }
            | ClauseGuard::LtEqInt { .. } => 4,

            ClauseGuard::Constant(_) | ClauseGuard::Var { .. } => 5,
        }
    }
}

impl TypedClauseGuard {
    pub fn tipo(&self) -> Arc<Type> {
        match self {
            ClauseGuard::Var { tipo, .. } => tipo.clone(),
            // ClauseGuard::TupleIndex { type_, .. } => type_.clone(),
            ClauseGuard::Constant(constant) => constant.tipo(),

            ClauseGuard::Or { .. }
            | ClauseGuard::And { .. }
            | ClauseGuard::Equals { .. }
            | ClauseGuard::NotEquals { .. }
            | ClauseGuard::GtInt { .. }
            | ClauseGuard::GtEqInt { .. }
            | ClauseGuard::LtInt { .. }
            | ClauseGuard::LtEqInt { .. } => bool(),
        }
    }
}

pub type TypedIfBranch = IfBranch<TypedExpr>;
pub type UntypedIfBranch = IfBranch<UntypedExpr>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IfBranch<Expr> {
    pub condition: Expr,
    pub body: Expr,
    pub location: Span,
}

#[derive(Debug, Clone)]
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

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Span {
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

        Self::new((), 0..0)
    }

    pub fn range(&self) -> Range<usize> {
        use chumsky::Span;

        self.start()..self.end()
    }

    pub fn union(self, other: Self) -> Self {
        use chumsky::Span;

        Self {
            start: self.start().min(other.start()),
            end: self.end().max(other.end()),
        }
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.range())
    }
}

impl chumsky::Span for Span {
    type Context = ();

    type Offset = usize;

    fn new(_context: Self::Context, range: Range<Self::Offset>) -> Self {
        assert!(range.start <= range.end);

        Self {
            start: range.start,
            end: range.end,
        }
    }

    fn context(&self) -> Self::Context {}

    fn start(&self) -> Self::Offset {
        self.start
    }

    fn end(&self) -> Self::Offset {
        self.end
    }
}
