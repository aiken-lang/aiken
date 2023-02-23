use std::{fmt, ops::Range, sync::Arc};

use crate::{
    builtins::{self, bool},
    expr::{TypedExpr, UntypedExpr},
    parser::token::Token,
    tipo::{PatternConstructor, Type, TypeInfo},
};
use miette::Diagnostic;
use owo_colors::OwoColorize;

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

impl TypedModule {
    pub fn find_node(&self, byte_index: usize) -> Option<Located<'_>> {
        self.definitions
            .iter()
            .find_map(|definition| definition.find_node(byte_index))
    }

    pub fn validate_module_name(&self) -> Result<(), Error> {
        if self.name == "aiken" || self.name == "aiken/builtin" {
            return Err(Error::ReservedModuleName {
                name: self.name.to_string(),
            });
        };

        for segment in self.name.split('/') {
            if str_to_keyword(segment).is_some() {
                return Err(Error::KeywordInModuleName {
                    name: self.name.to_string(),
                    keyword: segment.to_string(),
                });
            }
        }

        Ok(())
    }
}

fn str_to_keyword(word: &str) -> Option<Token> {
    // Alphabetical keywords:
    match word {
        "assert" => Some(Token::Expect),
        "expect" => Some(Token::Expect),
        "else" => Some(Token::Else),
        "is" => Some(Token::Is),
        "as" => Some(Token::As),
        "when" => Some(Token::When),
        "const" => Some(Token::Const),
        "fn" => Some(Token::Fn),
        "if" => Some(Token::If),
        "use" => Some(Token::Use),
        "let" => Some(Token::Let),
        "opaque" => Some(Token::Opaque),
        "pub" => Some(Token::Pub),
        "todo" => Some(Token::Todo),
        "type" => Some(Token::Type),
        "trace" => Some(Token::Trace),
        "test" => Some(Token::Test),
        "error" => Some(Token::ErrorTerm),
        "validator" => Some(Token::Validator),
        _ => None,
    }
}

pub type TypedFunction = Function<Arc<Type>, TypedExpr>;
pub type UntypedFunction = Function<(), UntypedExpr>;

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

pub type TypedTypeAlias = TypeAlias<Arc<Type>>;
pub type UntypedTypeAlias = TypeAlias<()>;

impl TypedFunction {
    pub fn test_hint(&self) -> Option<(BinOp, Box<TypedExpr>, Box<TypedExpr>)> {
        do_test_hint(&self.body)
    }
}

pub fn do_test_hint(body: &TypedExpr) -> Option<(BinOp, Box<TypedExpr>, Box<TypedExpr>)> {
    match body {
        TypedExpr::BinOp {
            name,
            tipo,
            left,
            right,
            ..
        } if tipo == &bool() => Some((*name, left.clone(), right.clone())),
        TypedExpr::Sequence { expressions, .. } | TypedExpr::Pipeline { expressions, .. } => {
            if let Some((binop, left, right)) = do_test_hint(&expressions[expressions.len() - 1]) {
                let mut new_left_expressions = expressions.clone();
                new_left_expressions.pop();
                new_left_expressions.push(*left);

                let mut new_right_expressions = expressions.clone();
                new_right_expressions.pop();
                new_right_expressions.push(*right);

                Some((
                    binop,
                    TypedExpr::Sequence {
                        expressions: new_left_expressions,
                        location: Span::empty(),
                    }
                    .into(),
                    TypedExpr::Sequence {
                        expressions: new_right_expressions,
                        location: Span::empty(),
                    }
                    .into(),
                ))
            } else {
                None
            }
        }
        _ => None,
    }
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

pub type TypedDataType = DataType<Arc<Type>>;

impl TypedDataType {
    pub fn ordering() -> Self {
        DataType {
            constructors: vec![
                RecordConstructor {
                    location: Span::empty(),
                    name: "Less".to_string(),
                    arguments: vec![],
                    doc: None,
                    sugar: false,
                },
                RecordConstructor {
                    location: Span::empty(),
                    name: "Equal".to_string(),
                    arguments: vec![],
                    doc: None,
                    sugar: false,
                },
                RecordConstructor {
                    location: Span::empty(),
                    name: "Greater".to_string(),
                    arguments: vec![],
                    doc: None,
                    sugar: false,
                },
            ],
            doc: None,
            location: Span::empty(),
            name: "Ordering".to_string(),
            opaque: false,
            parameters: vec![],
            public: true,
            typed_parameters: vec![],
        }
    }

    pub fn option(tipo: Arc<Type>) -> Self {
        DataType {
            constructors: vec![
                RecordConstructor {
                    location: Span::empty(),
                    name: "Some".to_string(),
                    arguments: vec![RecordConstructorArg {
                        label: None,
                        annotation: Annotation::Var {
                            location: Span::empty(),
                            name: "a".to_string(),
                        },
                        location: Span::empty(),
                        tipo: tipo.clone(),
                        doc: None,
                    }],
                    doc: None,
                    sugar: false,
                },
                RecordConstructor {
                    location: Span::empty(),
                    name: "None".to_string(),
                    arguments: vec![],
                    doc: None,
                    sugar: false,
                },
            ],
            doc: None,
            location: Span::empty(),
            name: "Option".to_string(),
            opaque: false,
            parameters: vec!["a".to_string()],
            public: true,
            typed_parameters: vec![tipo],
        }
    }
}

pub type UntypedDataType = DataType<()>;

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

pub type TypedUse = Use<String>;
pub type UntypedUse = Use<()>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Use<PackageName> {
    pub as_name: Option<String>,
    pub location: Span,
    pub module: Vec<String>,
    pub package: PackageName,
    pub unqualified: Vec<UnqualifiedImport>,
}

pub type TypedModuleConstant = ModuleConstant<Arc<Type>>;
pub type UntypedModuleConstant = ModuleConstant<()>;

#[derive(Debug, Clone, PartialEq)]
pub struct ModuleConstant<T> {
    pub doc: Option<String>,
    pub location: Span,
    pub public: bool,
    pub name: String,
    pub annotation: Option<Annotation>,
    pub value: Box<Constant>,
    pub tipo: T,
}

pub type TypedValidator = Validator<Arc<Type>, TypedExpr>;
pub type UntypedValidator = Validator<(), UntypedExpr>;

#[derive(Debug, Clone, PartialEq)]
pub struct Validator<T, Expr> {
    pub doc: Option<String>,
    pub end_position: usize,
    pub fun: Function<T, Expr>,
    pub location: Span,
    pub params: Vec<Arg<T>>,
}

pub type TypedDefinition = Definition<Arc<Type>, TypedExpr, String>;
pub type UntypedDefinition = Definition<(), UntypedExpr, ()>;

#[derive(Debug, Clone, PartialEq)]
pub enum Definition<T, Expr, PackageName> {
    Fn(Function<T, Expr>),

    TypeAlias(TypeAlias<T>),

    DataType(DataType<T>),

    Use(Use<PackageName>),

    ModuleConstant(ModuleConstant<T>),

    Test(Function<T, Expr>),

    Validator(Validator<T, Expr>),
}

impl<A, B, C> Definition<A, B, C> {
    pub fn location(&self) -> Span {
        match self {
            Definition::Fn(Function { location, .. })
            | Definition::Use(Use { location, .. })
            | Definition::TypeAlias(TypeAlias { location, .. })
            | Definition::DataType(DataType { location, .. })
            | Definition::ModuleConstant(ModuleConstant { location, .. })
            | Definition::Validator(Validator { location, .. })
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
            | Definition::Validator(Validator { doc, .. })
            | Definition::Test(Function { doc, .. }) => {
                let _ = std::mem::replace(doc, Some(new_doc));
            }
        }
    }

    pub fn doc(&self) -> Option<String> {
        match self {
            Definition::Use { .. } => None,
            Definition::Fn(Function { doc, .. })
            | Definition::TypeAlias(TypeAlias { doc, .. })
            | Definition::DataType(DataType { doc, .. })
            | Definition::ModuleConstant(ModuleConstant { doc, .. })
            | Definition::Validator(Validator { doc, .. })
            | Definition::Test(Function { doc, .. }) => doc.clone(),
        }
    }
}

impl TypedDefinition {
    pub fn find_node(&self, byte_index: usize) -> Option<Located<'_>> {
        // Note that the fn span covers the function head, not
        // the entire statement.
        if let Definition::Fn(Function { body, .. })
        | Definition::Validator(Validator {
            fun: Function { body, .. },
            ..
        })
        | Definition::Test(Function { body, .. }) = self
        {
            if let Some(expression) = body.find_node(byte_index) {
                return Some(Located::Expression(expression));
            }
        }

        if self.location().contains(byte_index) {
            Some(Located::Definition(self))
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Located<'a> {
    Expression(&'a TypedExpr),
    Definition(&'a TypedDefinition),
}

impl<'a> Located<'a> {
    pub fn definition_location(&self) -> Option<DefinitionLocation<'_>> {
        match self {
            Self::Expression(expression) => expression.definition_location(),
            Self::Definition(definition) => Some(DefinitionLocation {
                module: None,
                span: definition.location(),
            }),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct DefinitionLocation<'module> {
    pub module: Option<&'module str>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Constant {
    Int {
        location: Span,
        value: String,
    },

    String {
        location: Span,
        value: String,
    },

    ByteArray {
        location: Span,
        bytes: Vec<u8>,
        preferred_format: ByteArrayFormatPreference,
    },
}

impl Constant {
    pub fn tipo(&self) -> Arc<Type> {
        match self {
            Constant::Int { .. } => builtins::int(),
            Constant::String { .. } => builtins::string(),
            Constant::ByteArray { .. } => builtins::byte_array(),
        }
    }

    pub fn location(&self) -> Span {
        match self {
            Constant::Int { location, .. }
            | Constant::String { location, .. }
            | Constant::ByteArray { location, .. } => *location,
        }
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

impl TypedCallArg {
    pub fn find_node(&self, byte_index: usize) -> Option<&TypedExpr> {
        self.value.find_node(byte_index)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct RecordConstructor<T> {
    pub location: Span,
    pub name: String,
    pub arguments: Vec<RecordConstructorArg<T>>,
    pub doc: Option<String>,
    pub sugar: bool,
}

impl<A> RecordConstructor<A> {
    pub fn put_doc(&mut self, new_doc: String) {
        self.doc = Some(new_doc);
    }
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

impl<T: PartialEq> RecordConstructorArg<T> {
    pub fn put_doc(&mut self, new_doc: String) {
        self.doc = Some(new_doc);
    }
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
    Discarded {
        name: String,
        label: String,
        location: Span,
    },
    Named {
        name: String,
        label: String,
        location: Span,
    },
}

impl ArgName {
    pub fn get_variable_name(&self) -> Option<&str> {
        match self {
            ArgName::Discarded { .. } => None,
            ArgName::Named { name, .. } => Some(name),
        }
    }

    pub fn get_label(&self) -> String {
        match self {
            ArgName::Discarded { label, .. } => label.to_string(),
            ArgName::Named { label, .. } => label.to_string(),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnOp {
    // !
    Not,
    // -
    Negate,
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

    /// The creation of a variable.
    /// e.g. `assert [this_is_a_var, .._] = x`
    Var {
        location: Span,
        name: String,
    },

    /// A name given to a sub-pattern using the `as` keyword.
    /// e.g. `assert (1, [_, _] as the_list) = x`
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
            | Pattern::List { location, .. }
            | Pattern::Discard { location, .. }
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

    /// Returns `true` if the pattern is [`Var`].
    ///
    /// [`Var`]: Pattern::Discard
    pub fn is_var(&self) -> bool {
        matches!(self, Self::Var { .. })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum ByteArrayFormatPreference {
    HexadecimalString,
    ArrayOfBytes,
    Utf8String,
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum AssignmentKind {
    Let,
    Expect,
}

impl AssignmentKind {
    pub fn is_let(&self) -> bool {
        matches!(self, AssignmentKind::Let)
    }

    pub fn is_expect(&self) -> bool {
        matches!(self, AssignmentKind::Expect)
    }

    pub fn location_offset(&self) -> usize {
        match self {
            AssignmentKind::Let => 3,
            AssignmentKind::Expect => 6,
        }
    }
}

pub type MultiPattern<PatternConstructor, Type> = Vec<Pattern<PatternConstructor, Type>>;

pub type UntypedMultiPattern = MultiPattern<(), ()>;
pub type TypedMultiPattern = MultiPattern<PatternConstructor, Arc<Type>>;

pub type TypedClause = Clause<TypedExpr, PatternConstructor, Arc<Type>>;
pub type UntypedClause = Clause<UntypedExpr, (), ()>;

#[derive(Debug, Clone, PartialEq)]
pub struct Clause<Expr, PatternConstructor, Type> {
    pub location: Span,
    pub pattern: MultiPattern<PatternConstructor, Type>,
    pub alternative_patterns: Vec<MultiPattern<PatternConstructor, Type>>,
    pub guard: Option<ClauseGuard<Type>>,
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

    pub fn find_node(&self, byte_index: usize) -> Option<&TypedExpr> {
        self.then.find_node(byte_index)
    }
}

pub type UntypedClauseGuard = ClauseGuard<()>;
pub type TypedClauseGuard = ClauseGuard<Arc<Type>>;

#[derive(Debug, Clone, PartialEq)]
pub enum ClauseGuard<Type> {
    Not {
        location: Span,
        value: Box<Self>,
    },

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

    Constant(Constant),
}

impl<A> ClauseGuard<A> {
    pub fn location(&self) -> Span {
        match self {
            ClauseGuard::Constant(constant) => constant.location(),
            ClauseGuard::Not { location, .. }
            | ClauseGuard::Or { location, .. }
            | ClauseGuard::And { location, .. }
            | ClauseGuard::Var { location, .. }
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
            ClauseGuard::Not { .. } => 1,
            ClauseGuard::Or { .. } => 2,
            ClauseGuard::And { .. } => 3,
            ClauseGuard::Equals { .. } | ClauseGuard::NotEquals { .. } => 4,
            ClauseGuard::GtInt { .. }
            | ClauseGuard::GtEqInt { .. }
            | ClauseGuard::LtInt { .. }
            | ClauseGuard::LtEqInt { .. } => 5,
            ClauseGuard::Constant(_) | ClauseGuard::Var { .. } => 6,
        }
    }
}

impl TypedClauseGuard {
    pub fn tipo(&self) -> Arc<Type> {
        match self {
            ClauseGuard::Var { tipo, .. } => tipo.clone(),
            ClauseGuard::Constant(constant) => constant.tipo(),
            ClauseGuard::Not { .. }
            | ClauseGuard::Or { .. }
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

#[derive(Debug, Clone, PartialEq)]
pub struct TypedRecordUpdateArg {
    pub label: String,
    pub location: Span,
    pub value: TypedExpr,
    pub index: usize,
}

impl TypedRecordUpdateArg {
    pub fn find_node(&self, byte_index: usize) -> Option<&TypedExpr> {
        self.value.find_node(byte_index)
    }
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
pub enum TraceKind {
    Trace,
    Todo,
    Error,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Tracing {
    NoTraces,
    KeepTraces,
}

impl From<bool> for Tracing {
    fn from(keep: bool) -> Self {
        if keep {
            Tracing::KeepTraces
        } else {
            Tracing::NoTraces
        }
    }
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

    pub fn contains(&self, byte_index: usize) -> bool {
        byte_index >= self.start && byte_index < self.end
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

#[derive(Debug, thiserror::Error, Diagnostic)]
pub enum Error {
    #[error(
      "I realized the module '{}' contains the keyword '{}', which is forbidden.\n",
      name.purple(),
      keyword.purple()
    )]
    #[diagnostic(url("https://aiken-lang.org/language-tour/modules"))]
    #[diagnostic(code("illegal::module_name"))]
    #[diagnostic(help(r#"You cannot use keywords as part of a module path name. As a quick reminder, here's a list of all the keywords (and thus, of invalid module path names):

    as, expect, check, const, else, fn, if, is, let, opaque, pub, test, todo, trace, type, use, when"#))]
    KeywordInModuleName { name: String, keyword: String },

    #[error("I realized you used '{}' as a module name, which is reserved (and not available).\n", name.purple())]
    #[diagnostic(code("illegal::module_name"))]
    #[diagnostic(help(r#"Some module names are reserved for internal use. This the case of:

- aiken: where the prelude is located;
- aiken/builtin: where I store low-level Plutus builtins.

Note that 'aiken' is also imported by default; but you can refer to it explicitly to disambiguate with a local value that would clash with one from that module."#
    ))]
    ReservedModuleName { name: String },
}
