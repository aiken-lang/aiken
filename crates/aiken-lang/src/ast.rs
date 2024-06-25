use crate::{
    builtins::{self, bool, g1_element, g2_element},
    expr::{TypedExpr, UntypedExpr},
    line_numbers::LineNumbers,
    parser::token::{Base, Token},
    tipo::{PatternConstructor, Type, TypeInfo},
};
use indexmap::IndexMap;
use miette::Diagnostic;
use ordinal::Ordinal;
use owo_colors::{OwoColorize, Stream::Stdout};
use std::{
    fmt::{self, Display},
    ops::Range,
    rc::Rc,
};
use uplc::machine::runtime::Compressable;
use vec1::{vec1, Vec1};

pub const BACKPASS_VARIABLE: &str = "_backpass";
pub const CAPTURE_VARIABLE: &str = "_capture";
pub const PIPE_VARIABLE: &str = "_pipe";

pub type TypedModule = Module<TypeInfo, TypedDefinition>;
pub type UntypedModule = Module<(), UntypedDefinition>;

#[derive(Debug, Copy, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
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

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct Module<Info, Definitions> {
    pub name: String,
    pub docs: Vec<String>,
    pub type_info: Info,
    pub definitions: Vec<Definitions>,
    pub lines: LineNumbers,
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

    pub fn has_definition(&self, name: &str) -> bool {
        self.definitions.iter().any(|def| match def {
            Definition::Fn(f) => f.public && f.name == name,
            Definition::TypeAlias(alias) => alias.public && alias.alias == name,
            Definition::ModuleConstant(cst) => cst.public && cst.name == name,
            Definition::DataType(t) => t.public && t.name == name,
            Definition::Use(_) => false,
            Definition::Test(_) => false,
            Definition::Validator(_) => false,
        })
    }

    pub fn has_constructor(&self, name: &str) -> bool {
        self.definitions.iter().any(|def| match def {
            Definition::DataType(t) if t.public && !t.opaque => t
                .constructors
                .iter()
                .any(|constructor| constructor.name == name),
            Definition::DataType(_) => false,
            Definition::Fn(_) => false,
            Definition::TypeAlias(_) => false,
            Definition::ModuleConstant(_) => false,
            Definition::Use(_) => false,
            Definition::Test(_) => false,
            Definition::Validator(_) => false,
        })
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

    // TODO: Avoid cloning definitions here. This would likely require having a lifetime on
    // 'Project', so that we can enforce that those references live from the ast to here.
    pub fn register_definitions(
        &self,
        functions: &mut IndexMap<FunctionAccessKey, TypedFunction>,
        data_types: &mut IndexMap<DataTypeKey, TypedDataType>,
    ) {
        for def in self.definitions() {
            match def {
                Definition::Fn(func) => {
                    functions.insert(
                        FunctionAccessKey {
                            module_name: self.name.clone(),
                            function_name: func.name.clone(),
                        },
                        func.clone(),
                    );
                }

                Definition::Test(test) => {
                    functions.insert(
                        FunctionAccessKey {
                            module_name: self.name.clone(),
                            function_name: test.name.clone(),
                        },
                        test.clone().into(),
                    );
                }

                Definition::DataType(dt) => {
                    data_types.insert(
                        DataTypeKey {
                            module_name: self.name.clone(),
                            defined_type: dt.name.clone(),
                        },
                        dt.clone(),
                    );
                }

                Definition::Validator(v) => {
                    let module_name = self.name.as_str();

                    if let Some((k, v)) = v.into_function_definition(module_name, |f, _| Some(f)) {
                        functions.insert(k, v);
                    }

                    if let Some((k, v)) = v.into_function_definition(module_name, |_, f| f) {
                        functions.insert(k, v);
                    }
                }

                Definition::TypeAlias(_) | Definition::ModuleConstant(_) | Definition::Use(_) => {}
            }
        }
    }
}

fn str_to_keyword(word: &str) -> Option<Token> {
    // Alphabetical keywords:
    match word {
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
        // TODO: remove this in a future release
        "error" => Some(Token::Fail),
        "fail" => Some(Token::Fail),
        "and" => Some(Token::And),
        "or" => Some(Token::Or),
        "validator" => Some(Token::Validator),
        "via" => Some(Token::Via),
        _ => None,
    }
}

pub type TypedFunction = Function<Rc<Type>, TypedExpr, TypedArg>;
pub type UntypedFunction = Function<(), UntypedExpr, UntypedArg>;

pub type TypedTest = Function<Rc<Type>, TypedExpr, TypedArgVia>;
pub type UntypedTest = Function<(), UntypedExpr, UntypedArgVia>;

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum OnTestFailure {
    FailImmediately,
    SucceedImmediately,
    SucceedEventually,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct Function<T, Expr, Arg> {
    pub arguments: Vec<Arg>,
    pub body: Expr,
    pub doc: Option<String>,
    pub location: Span,
    pub name: String,
    pub public: bool,
    pub return_annotation: Option<Annotation>,
    pub return_type: T,
    pub end_position: usize,
    pub on_test_failure: OnTestFailure,
}

impl TypedFunction {
    pub fn find_node(&self, byte_index: usize) -> Option<Located<'_>> {
        self.arguments
            .iter()
            .find_map(|arg| arg.find_node(byte_index))
            .or_else(|| self.body.find_node(byte_index))
            .or_else(|| {
                self.return_annotation
                    .as_ref()
                    .and_then(|a| a.find_node(byte_index))
            })
    }
}

impl TypedTest {
    pub fn find_node(&self, byte_index: usize) -> Option<Located<'_>> {
        self.arguments
            .iter()
            .find_map(|arg| arg.find_node(byte_index))
            .or_else(|| self.body.find_node(byte_index))
            .or_else(|| {
                self.return_annotation
                    .as_ref()
                    .and_then(|a| a.find_node(byte_index))
            })
    }
}

pub type TypedTypeAlias = TypeAlias<Rc<Type>>;
pub type UntypedTypeAlias = TypeAlias<()>;

impl From<UntypedTest> for UntypedFunction {
    fn from(f: UntypedTest) -> Self {
        Function {
            doc: f.doc,
            location: f.location,
            name: f.name,
            public: f.public,
            arguments: f.arguments.into_iter().map(|arg| arg.into()).collect(),
            return_annotation: f.return_annotation,
            return_type: f.return_type,
            body: f.body,
            on_test_failure: f.on_test_failure,
            end_position: f.end_position,
        }
    }
}

impl From<TypedTest> for TypedFunction {
    fn from(f: TypedTest) -> Self {
        Function {
            doc: f.doc,
            location: f.location,
            name: f.name,
            public: f.public,
            arguments: f.arguments.into_iter().map(|arg| arg.into()).collect(),
            return_annotation: f.return_annotation,
            return_type: f.return_type,
            body: f.body,
            on_test_failure: f.on_test_failure,
            end_position: f.end_position,
        }
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct TypeAlias<T> {
    pub alias: String,
    pub annotation: Annotation,
    pub doc: Option<String>,
    pub location: Span,
    pub parameters: Vec<String>,
    pub public: bool,
    pub tipo: T,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct DataTypeKey {
    pub module_name: String,
    pub defined_type: String,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct FunctionAccessKey {
    pub module_name: String,
    pub function_name: String,
}

pub type TypedDataType = DataType<Rc<Type>>;

impl TypedDataType {
    pub fn data() -> Self {
        DataType {
            constructors: vec![],
            doc: None,
            location: Span::empty(),
            name: "Data".to_string(),
            opaque: false,
            parameters: vec![],
            public: true,
            typed_parameters: vec![],
        }
    }

    pub fn bool() -> Self {
        DataType {
            constructors: vec![
                RecordConstructor {
                    location: Span::empty(),
                    name: "False".to_string(),
                    arguments: vec![],
                    doc: None,
                    sugar: false,
                },
                RecordConstructor {
                    location: Span::empty(),
                    name: "True".to_string(),
                    arguments: vec![],
                    doc: None,
                    sugar: false,
                },
            ],
            doc: None,
            location: Span::empty(),
            name: "Bool".to_string(),
            opaque: false,
            parameters: vec![],
            public: true,
            typed_parameters: vec![],
        }
    }

    pub fn prng() -> Self {
        DataType {
            constructors: vec![
                RecordConstructor {
                    location: Span::empty(),
                    name: "Seeded".to_string(),
                    arguments: vec![],
                    doc: None,
                    sugar: false,
                },
                RecordConstructor {
                    location: Span::empty(),
                    name: "Replayed".to_string(),
                    arguments: vec![],
                    doc: None,
                    sugar: false,
                },
            ],
            doc: None,
            location: Span::empty(),
            name: "PRNG".to_string(),
            opaque: false,
            parameters: vec![],
            public: true,
            typed_parameters: vec![],
        }
    }

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

    pub fn option(tipo: Rc<Type>) -> Self {
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

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
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

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct Use<PackageName> {
    pub as_name: Option<String>,
    pub location: Span,
    pub module: Vec<String>,
    pub package: PackageName,
    pub unqualified: Vec<UnqualifiedImport>,
}

pub type TypedModuleConstant = ModuleConstant<Rc<Type>>;
pub type UntypedModuleConstant = ModuleConstant<()>;

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct ModuleConstant<T> {
    pub doc: Option<String>,
    pub location: Span,
    pub public: bool,
    pub name: String,
    pub annotation: Option<Annotation>,
    pub value: Box<Constant>,
    pub tipo: T,
}

pub type TypedValidator = Validator<Rc<Type>, TypedArg, TypedExpr>;
pub type UntypedValidator = Validator<(), UntypedArg, UntypedExpr>;

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct Validator<T, Arg, Expr> {
    pub doc: Option<String>,
    pub end_position: usize,
    pub fun: Function<T, Expr, Arg>,
    pub other_fun: Option<Function<T, Expr, Arg>>,
    pub location: Span,
    pub params: Vec<Arg>,
}

impl TypedValidator {
    pub fn find_node(&self, byte_index: usize) -> Option<Located<'_>> {
        self.params
            .iter()
            .find_map(|arg| arg.find_node(byte_index))
            .or_else(|| self.fun.find_node(byte_index))
            .or_else(|| {
                self.other_fun
                    .as_ref()
                    .and_then(|f| f.find_node(byte_index))
            })
    }

    pub fn into_function_definition<'a, F>(
        &'a self,
        module_name: &str,
        select: F,
    ) -> Option<(FunctionAccessKey, TypedFunction)>
    where
        F: Fn(&'a TypedFunction, Option<&'a TypedFunction>) -> Option<&'a TypedFunction> + 'a,
    {
        match select(&self.fun, self.other_fun.as_ref()) {
            None => None,
            Some(fun) => {
                let mut fun = fun.clone();

                fun.arguments = self
                    .params
                    .clone()
                    .into_iter()
                    .chain(fun.arguments)
                    .collect();

                Some((
                    FunctionAccessKey {
                        module_name: module_name.to_string(),
                        function_name: fun.name.clone(),
                    },
                    fun,
                ))
            }
        }
    }
}

pub type TypedDefinition = Definition<Rc<Type>, TypedArg, TypedExpr, String>;
pub type UntypedDefinition = Definition<(), UntypedArg, UntypedExpr, ()>;

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum Definition<T, Arg, Expr, PackageName> {
    Fn(Function<T, Expr, Arg>),

    TypeAlias(TypeAlias<T>),

    DataType(DataType<T>),

    Use(Use<PackageName>),

    ModuleConstant(ModuleConstant<T>),

    Test(Function<T, Expr, ArgVia<Arg, Expr>>),

    Validator(Validator<T, Arg, Expr>),
}

impl<A, B, C, D> Definition<A, B, C, D> {
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
        let located = match self {
            Definition::Validator(validator) => validator.find_node(byte_index),
            Definition::Fn(func) => func.find_node(byte_index),
            Definition::Test(func) => func.find_node(byte_index),
            _ => None,
        };

        if located.is_none() && self.location().contains(byte_index) {
            return Some(Located::Definition(self));
        }

        located
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Located<'a> {
    Expression(&'a TypedExpr),
    Pattern(&'a TypedPattern, Rc<Type>),
    Definition(&'a TypedDefinition),
    Argument(&'a ArgName, Rc<Type>),
    Annotation(&'a Annotation),
}

impl<'a> Located<'a> {
    pub fn definition_location(&self) -> Option<DefinitionLocation<'_>> {
        match self {
            Self::Expression(expression) => expression.definition_location(),
            Self::Definition(definition) => Some(DefinitionLocation {
                module: None,
                span: definition.location(),
            }),
            // TODO: Revise definition location semantic for 'Pattern'
            // e.g. for constructors, we might want to show the type definition
            // for that constructor.
            Self::Pattern(_, _) | Located::Argument(_, _) | Located::Annotation(_) => None,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct DefinitionLocation<'module> {
    pub module: Option<&'module str>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum Constant {
    Int {
        location: Span,
        value: String,
        base: Base,
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

    CurvePoint {
        location: Span,
        point: Box<Curve>,
        preferred_format: ByteArrayFormatPreference,
    },
}

impl Constant {
    pub fn tipo(&self) -> Rc<Type> {
        match self {
            Constant::Int { .. } => builtins::int(),
            Constant::String { .. } => builtins::string(),
            Constant::ByteArray { .. } => builtins::byte_array(),
            Constant::CurvePoint { point, .. } => match point.as_ref() {
                Curve::Bls12_381(Bls12_381Point::G1(_)) => builtins::g1_element(),
                Curve::Bls12_381(Bls12_381Point::G2(_)) => builtins::g2_element(),
            },
        }
    }

    pub fn location(&self) -> Span {
        match self {
            Constant::Int { location, .. }
            | Constant::String { location, .. }
            | Constant::ByteArray { location, .. }
            | Constant::CurvePoint { location, .. } => *location,
        }
    }
}

pub type TypedCallArg = CallArg<TypedExpr>;
pub type ParsedCallArg = CallArg<Option<UntypedExpr>>;

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
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
    pub fn find_node(&self, byte_index: usize) -> Option<Located<'_>> {
        self.value.find_node(byte_index)
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
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

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
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

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum ArgBy {
    ByName(ArgName),
    ByPattern(UntypedPattern),
}

impl ArgBy {
    pub fn into_extra_assignment(
        self,
        name: &ArgName,
        annotation: Option<&Annotation>,
        location: Span,
    ) -> Option<UntypedExpr> {
        match self {
            ArgBy::ByName(..) => None,
            ArgBy::ByPattern(pattern) => Some(UntypedExpr::Assignment {
                location,
                value: Box::new(UntypedExpr::Var {
                    location,
                    name: name.get_name(),
                }),
                patterns: vec1![AssignmentPattern {
                    pattern,
                    location,
                    annotation: annotation.cloned(),
                }],
                kind: AssignmentKind::Let { backpassing: false },
            }),
        }
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct UntypedArg {
    pub by: ArgBy,
    pub location: Span,
    pub annotation: Option<Annotation>,
    pub doc: Option<String>,
    pub is_validator_param: bool,
}

impl UntypedArg {
    pub fn arg_name(&self, ix: usize) -> ArgName {
        match self.by {
            ArgBy::ByName(ref name) => name.clone(),
            ArgBy::ByPattern(..) => {
                // NOTE: We use ordinal here not only because it's cute, but because
                // such a name cannot be parsed to begin with and thus, will not clash
                // with any user-defined name.
                let name = format!("{}_arg", Ordinal::<usize>(ix).suffix());
                ArgName::Named {
                    label: name.clone(),
                    name,
                    location: self.location,
                }
            }
        }
    }

    pub fn set_type(self, tipo: Rc<Type>, ix: usize) -> TypedArg {
        TypedArg {
            tipo,
            arg_name: self.arg_name(ix),
            location: self.location,
            annotation: self.annotation,
            is_validator_param: self.is_validator_param,
            doc: self.doc,
        }
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct TypedArg {
    pub arg_name: ArgName,
    pub location: Span,
    pub annotation: Option<Annotation>,
    pub doc: Option<String>,
    pub is_validator_param: bool,
    pub tipo: Rc<Type>,
}

impl TypedArg {
    pub fn put_doc(&mut self, new_doc: String) {
        self.doc = Some(new_doc);
    }

    pub fn get_variable_name(&self) -> Option<&str> {
        self.arg_name.get_variable_name()
    }

    pub fn is_capture(&self) -> bool {
        if let ArgName::Named {
            ref name, location, ..
        } = self.arg_name
        {
            return name.starts_with(CAPTURE_VARIABLE)
                && location == Span::empty()
                && self.location == Span::empty();
        }

        false
    }

    pub fn find_node(&self, byte_index: usize) -> Option<Located<'_>> {
        if self.arg_name.location().contains(byte_index) {
            Some(Located::Argument(&self.arg_name, self.tipo.clone()))
        } else {
            self.annotation
                .as_ref()
                .and_then(|annotation| annotation.find_node(byte_index))
        }
    }
}

pub type TypedArgVia = ArgVia<TypedArg, TypedExpr>;
pub type UntypedArgVia = ArgVia<UntypedArg, UntypedExpr>;

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct ArgVia<Arg, Expr> {
    pub arg: Arg,
    pub via: Expr,
}

impl<Expr> From<ArgVia<TypedArg, Expr>> for TypedArg {
    fn from(this: ArgVia<TypedArg, Expr>) -> TypedArg {
        this.arg
    }
}

impl<Expr> From<ArgVia<UntypedArg, Expr>> for UntypedArg {
    fn from(this: ArgVia<UntypedArg, Expr>) -> UntypedArg {
        this.arg
    }
}

impl TypedArgVia {
    pub fn find_node(&self, byte_index: usize) -> Option<Located<'_>> {
        if self.arg.arg_name.location().contains(byte_index) {
            Some(Located::Argument(&self.arg.arg_name, self.arg.tipo.clone()))
        } else {
            // `via` is done first here because when there is no manually written
            // annotation, it seems one is injected leading to a `found` returning too early
            // because the span of the filled in annotation matches the span of the via expr.
            self.via.find_node(byte_index).or_else(|| {
                self.arg
                    .annotation
                    .as_ref()
                    .and_then(|annotation| annotation.find_node(byte_index))
            })
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
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
    pub fn location(&self) -> Span {
        match self {
            ArgName::Discarded { location, .. } => *location,
            ArgName::Named { location, .. } => *location,
        }
    }

    /// Returns the name of the variable if it is named, otherwise None.
    /// Code gen uses the fact that this returns None to do certain things.
    pub fn get_variable_name(&self) -> Option<&str> {
        match self {
            ArgName::Discarded { .. } => None,
            ArgName::Named { name, .. } => Some(name),
        }
    }

    pub fn get_name(&self) -> String {
        match self {
            ArgName::Discarded { name, .. } | ArgName::Named { name, .. } => name.clone(),
        }
    }

    pub fn get_label(&self) -> String {
        match self {
            ArgName::Discarded { label, .. } | ArgName::Named { label, .. } => label.to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct UnqualifiedImport {
    pub location: Span,
    pub name: String,
    pub as_name: Option<String>,
}

impl UnqualifiedImport {
    pub fn variable_name(&self) -> &str {
        self.as_name.as_deref().unwrap_or(&self.name)
    }
}

// TypeAst
#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
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

    Pair {
        location: Span,
        fst: Box<Self>,
        snd: Box<Self>,
    },
}

impl Annotation {
    pub fn location(&self) -> Span {
        match self {
            Annotation::Fn { location, .. }
            | Annotation::Tuple { location, .. }
            | Annotation::Var { location, .. }
            | Annotation::Hole { location, .. }
            | Annotation::Constructor { location, .. }
            | Annotation::Pair { location, .. } => *location,
        }
    }

    pub fn boolean(location: Span) -> Self {
        Annotation::Constructor {
            name: "Bool".to_string(),
            module: None,
            arguments: vec![],
            location,
        }
    }

    pub fn int(location: Span) -> Self {
        Annotation::Constructor {
            name: "Int".to_string(),
            module: None,
            arguments: vec![],
            location,
        }
    }

    pub fn data(location: Span) -> Self {
        Annotation::Constructor {
            name: "Data".to_string(),
            module: None,
            arguments: vec![],
            location,
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
            Annotation::Pair { fst, snd, .. } => {
                if let Annotation::Pair {
                    fst: o_fst,
                    snd: o_snd,
                    ..
                } = other
                {
                    fst.is_logically_equal(o_fst) && snd.is_logically_equal(o_snd)
                } else {
                    false
                }
            }
        }
    }

    pub fn find_node(&self, byte_index: usize) -> Option<Located<'_>> {
        if !self.location().contains(byte_index) {
            return None;
        }

        let located = match self {
            Annotation::Constructor { arguments, .. } => {
                arguments.iter().find_map(|arg| arg.find_node(byte_index))
            }
            Annotation::Fn { arguments, ret, .. } => arguments
                .iter()
                .find_map(|arg| arg.find_node(byte_index))
                .or_else(|| ret.find_node(byte_index)),
            Annotation::Tuple { elems, .. } => {
                elems.iter().find_map(|arg| arg.find_node(byte_index))
            }
            Annotation::Var { .. } | Annotation::Hole { .. } => None,
            Annotation::Pair { fst, snd, .. } => fst
                .find_node(byte_index)
                .or_else(|| snd.find_node(byte_index)),
        };

        located.or(Some(Located::Annotation(self)))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
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

impl From<LogicalOpChainKind> for BinOp {
    fn from(value: LogicalOpChainKind) -> Self {
        match value {
            LogicalOpChainKind::And => BinOp::And,
            LogicalOpChainKind::Or => BinOp::Or,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum UnOp {
    /// !
    Not,
    /// -
    Negate,
}

impl BinOp {
    pub fn precedence(&self) -> u8 {
        // Ensure that this matches the other precedence function for guards
        match self {
            // Pipe is 0
            // Unary operators are 1
            Self::Or => 2,

            Self::And => 3,

            Self::Eq | Self::NotEq | Self::LtInt | Self::LtEqInt | Self::GtEqInt | Self::GtInt => 4,

            // Concatenation operators are typically 5, so we skip it.
            Self::AddInt | Self::SubInt => 6,

            Self::MultInt | Self::DivInt | Self::ModInt => 7,
        }
    }
}

pub type UntypedPattern = Pattern<(), ()>;
pub type TypedPattern = Pattern<PatternConstructor, Rc<Type>>;

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum Pattern<Constructor, Type> {
    Int {
        location: Span,
        value: String,
        base: Base,
    },

    /// The creation of a variable.
    /// e.g. `expect [this_is_a_var, .._] = x`
    /// e.g. `let foo = 42`
    Var {
        location: Span,
        name: String,
    },

    /// A name given to a sub-pattern using the `as` keyword.
    ///
    /// ```aiken
    /// when foo is {
    ///    [_, _] as the_list -> ...
    /// }
    /// ```
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
        spread_location: Option<Span>,
        tipo: Type,
    },

    Pair {
        location: Span,
        fst: Box<Self>,
        snd: Box<Self>,
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
            | Pattern::Pair { location, .. }
            | Pattern::Constructor { location, .. } => *location,
        }
    }

    pub fn with_spread(&self) -> bool {
        match self {
            Pattern::Constructor {
                spread_location, ..
            } => spread_location.is_some(),
            _ => false,
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

impl UntypedPattern {
    pub fn true_(location: Span) -> UntypedPattern {
        UntypedPattern::Constructor {
            location,
            name: "True".to_string(),
            arguments: vec![],
            constructor: (),
            spread_location: None,
            tipo: (),
            module: None,
            is_record: false,
        }
    }
}

impl TypedPattern {
    pub fn find_node<'a>(&'a self, byte_index: usize, value: &Rc<Type>) -> Option<Located<'a>> {
        if !self.location().contains(byte_index) {
            return None;
        }

        match self {
            Pattern::Int { .. }
            | Pattern::Var { .. }
            | Pattern::Assign { .. }
            | Pattern::Discard { .. } => Some(Located::Pattern(self, value.clone())),

            Pattern::List { elements, .. }
            | Pattern::Tuple {
                elems: elements, ..
            } => match &**value {
                Type::Tuple { elems, .. } => elements
                    .iter()
                    .zip(elems.iter())
                    .find_map(|(e, t)| e.find_node(byte_index, t))
                    .or(Some(Located::Pattern(self, value.clone()))),
                Type::App {
                    module, name, args, ..
                } if module.is_empty() && name == "List" => elements
                    .iter()
                    // this is the same as above but this uses
                    // cycle to repeat the single type arg for a list
                    // there's probably a cleaner way to re-use the code
                    // from this branch and the above.
                    .zip(args.iter().cycle())
                    .find_map(|(e, t)| e.find_node(byte_index, t))
                    .or(Some(Located::Pattern(self, value.clone()))),
                _ => None,
            },

            Pattern::Pair { fst, snd, .. } => match &**value {
                Type::Pair {
                    fst: fst_v,
                    snd: snd_v,
                    ..
                } => [fst, snd]
                    .into_iter()
                    .zip([fst_v, snd_v].iter())
                    .find_map(|(e, t)| e.find_node(byte_index, t))
                    .or(Some(Located::Pattern(self, value.clone()))),
                _ => None,
            },

            Pattern::Constructor {
                arguments, tipo, ..
            } => match &**tipo {
                Type::Fn { args, .. } => arguments
                    .iter()
                    .zip(args.iter())
                    .find_map(|(e, t)| e.value.find_node(byte_index, t))
                    .or(Some(Located::Pattern(self, value.clone()))),
                _ => None,
            },
        }
    }

    // TODO: This function definition is weird, see where this is used and how.
    pub fn tipo(&self, value: &TypedExpr) -> Option<Rc<Type>> {
        match self {
            Pattern::Int { .. } => Some(builtins::int()),
            Pattern::Constructor { tipo, .. } => Some(tipo.clone()),
            Pattern::Var { .. } | Pattern::Assign { .. } | Pattern::Discard { .. } => {
                Some(value.tipo())
            }
            Pattern::List { .. } | Pattern::Tuple { .. } | Pattern::Pair { .. } => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Copy, serde::Serialize, serde::Deserialize)]
pub enum ByteArrayFormatPreference {
    HexadecimalString,
    ArrayOfBytes(Base),
    Utf8String,
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum CurveType {
    Bls12_381(Bls12_381PointType),
}

impl Display for CurveType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CurveType::Bls12_381(point) => write!(f, "<Bls12_381, {point}>"),
        }
    }
}
impl From<&Curve> for CurveType {
    fn from(value: &Curve) -> Self {
        match value {
            Curve::Bls12_381(point) => CurveType::Bls12_381(point.into()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum Bls12_381PointType {
    G1,
    G2,
}

impl From<&Bls12_381Point> for Bls12_381PointType {
    fn from(value: &Bls12_381Point) -> Self {
        match value {
            Bls12_381Point::G1(_) => Bls12_381PointType::G1,
            Bls12_381Point::G2(_) => Bls12_381PointType::G2,
        }
    }
}

impl Display for Bls12_381PointType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Bls12_381PointType::G1 => write!(f, "G1"),
            Bls12_381PointType::G2 => write!(f, "G2"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Copy, serde::Serialize, serde::Deserialize)]
pub enum Curve {
    Bls12_381(Bls12_381Point),
}

impl Curve {
    pub fn compress(&self) -> Vec<u8> {
        match self {
            Curve::Bls12_381(point) => match point {
                Bls12_381Point::G1(g1) => g1.compress(),
                Bls12_381Point::G2(g2) => g2.compress(),
            },
        }
    }

    pub fn tipo(&self) -> Rc<Type> {
        match self {
            Curve::Bls12_381(point) => point.tipo(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum Bls12_381Point {
    G1(blst::blst_p1),
    G2(blst::blst_p2),
}

impl serde::Serialize for Bls12_381Point {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match *self {
            Bls12_381Point::G1(ref p1) => {
                // Assuming `to_bytes` for compression to Vec<u8>
                let bytes = p1.compress();

                // Serialize as a tuple with a tag for differentiation
                serializer.serialize_newtype_variant("Bls12_381Point", 0, "G1", &bytes)
            }
            Bls12_381Point::G2(ref p2) => {
                let bytes = p2.compress();

                serializer.serialize_newtype_variant("Bls12_381Point", 1, "G2", &bytes)
            }
        }
    }
}

impl<'de> serde::Deserialize<'de> for Bls12_381Point {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        enum Field {
            G1,
            G2,
        }

        impl<'de> serde::Deserialize<'de> for Field {
            fn deserialize<D>(deserializer: D) -> Result<Field, D::Error>
            where
                D: serde::Deserializer<'de>,
            {
                struct FieldVisitor;

                impl<'de> serde::de::Visitor<'de> for FieldVisitor {
                    type Value = Field;

                    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                        formatter.write_str("`G1` or `G2`")
                    }

                    fn visit_str<E>(self, value: &str) -> Result<Field, E>
                    where
                        E: serde::de::Error,
                    {
                        match value {
                            "G1" => Ok(Field::G1),
                            "G2" => Ok(Field::G2),
                            _ => Err(serde::de::Error::unknown_field(value, FIELDS)),
                        }
                    }
                }

                deserializer.deserialize_identifier(FieldVisitor)
            }
        }

        struct Bls12_381PointVisitor;

        impl<'de> serde::de::Visitor<'de> for Bls12_381PointVisitor {
            type Value = Bls12_381Point;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("struct Bls12_381Point")
            }

            fn visit_seq<V>(self, mut seq: V) -> Result<Bls12_381Point, V::Error>
            where
                V: serde::de::SeqAccess<'de>,
            {
                let tag = seq
                    .next_element::<Field>()?
                    .ok_or_else(|| serde::de::Error::invalid_length(0, &self))?;

                let bytes = seq
                    .next_element::<Vec<u8>>()?
                    .ok_or_else(|| serde::de::Error::invalid_length(1, &self))?;

                match tag {
                    Field::G1 => {
                        let p1 =
                            blst::blst_p1::uncompress(&bytes).map_err(serde::de::Error::custom)?;

                        Ok(Bls12_381Point::G1(p1))
                    }
                    Field::G2 => {
                        let p2 =
                            blst::blst_p2::uncompress(&bytes).map_err(serde::de::Error::custom)?;

                        Ok(Bls12_381Point::G2(p2))
                    }
                }
            }
        }

        const FIELDS: &[&str] = &["G1", "G2"];

        deserializer.deserialize_enum("Bls12_381Point", FIELDS, Bls12_381PointVisitor)
    }
}

impl Bls12_381Point {
    pub fn tipo(&self) -> Rc<Type> {
        match self {
            Bls12_381Point::G1(_) => g1_element(),
            Bls12_381Point::G2(_) => g2_element(),
        }
    }
}

impl Default for Bls12_381Point {
    fn default() -> Self {
        Bls12_381Point::G1(Default::default())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignmentPattern {
    pub pattern: UntypedPattern,
    pub annotation: Option<Annotation>,
    pub location: Span,
}

impl AssignmentPattern {
    pub fn new(
        pattern: UntypedPattern,
        annotation: Option<Annotation>,
        location: Span,
    ) -> AssignmentPattern {
        Self {
            pattern,
            annotation,
            location,
        }
    }
}

impl From<AssignmentPattern> for Vec1<AssignmentPattern> {
    fn from(value: AssignmentPattern) -> Self {
        Vec1::new(value)
    }
}

pub type UntypedAssignmentKind = AssignmentKind<bool>;
pub type TypedAssignmentKind = AssignmentKind<()>;

#[derive(Debug, Clone, PartialEq, Eq, Copy, serde::Serialize, serde::Deserialize)]
pub enum AssignmentKind<T> {
    Is,
    Let { backpassing: T },
    Expect { backpassing: T },
}

impl From<UntypedAssignmentKind> for TypedAssignmentKind {
    fn from(kind: UntypedAssignmentKind) -> TypedAssignmentKind {
        match kind {
            AssignmentKind::Is => AssignmentKind::Is,
            AssignmentKind::Let { .. } => AssignmentKind::Let { backpassing: () },
            AssignmentKind::Expect { .. } => AssignmentKind::Expect { backpassing: () },
        }
    }
}

impl<T> AssignmentKind<T> {
    pub fn is_let(&self) -> bool {
        matches!(self, AssignmentKind::Let { .. })
    }

    pub fn is_expect(&self) -> bool {
        matches!(self, AssignmentKind::Expect { .. })
    }

    pub fn if_is(&self) -> bool {
        matches!(self, AssignmentKind::Is)
    }

    pub fn location_offset(&self) -> usize {
        match self {
            AssignmentKind::Is => 2,
            AssignmentKind::Let { .. } => 3,
            AssignmentKind::Expect { .. } => 6,
        }
    }
}

impl AssignmentKind<bool> {
    pub fn is_backpassing(&self) -> bool {
        match self {
            Self::Is => unreachable!(),
            Self::Let { backpassing } | Self::Expect { backpassing } => *backpassing,
        }
    }
}

impl<T: Default> AssignmentKind<T> {
    pub fn let_() -> Self {
        AssignmentKind::Let {
            backpassing: Default::default(),
        }
    }

    pub fn is() -> Self {
        AssignmentKind::Is
    }

    pub fn expect() -> Self {
        AssignmentKind::Expect {
            backpassing: Default::default(),
        }
    }
}

pub type MultiPattern<PatternConstructor, Type> = Vec<Pattern<PatternConstructor, Type>>;

pub type UntypedMultiPattern = MultiPattern<(), ()>;
pub type TypedMultiPattern = MultiPattern<PatternConstructor, Rc<Type>>;

#[derive(Debug, Clone, PartialEq)]
pub struct UntypedClause {
    pub location: Span,
    pub patterns: Vec1<Pattern<(), ()>>,
    pub guard: Option<ClauseGuard<()>>,
    pub then: UntypedExpr,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct TypedClause {
    pub location: Span,
    pub pattern: Pattern<PatternConstructor, Rc<Type>>,
    pub guard: Option<ClauseGuard<Rc<Type>>>,
    pub then: TypedExpr,
}

impl TypedClause {
    pub fn location(&self) -> Span {
        Span {
            start: self.pattern.location().start,
            end: self.then.location().end,
        }
    }

    pub fn find_node(&self, byte_index: usize, subject_type: &Rc<Type>) -> Option<Located<'_>> {
        self.pattern
            .find_node(byte_index, subject_type)
            .or_else(|| self.then.find_node(byte_index))
    }
}

pub type UntypedClauseGuard = ClauseGuard<()>;
pub type TypedClauseGuard = ClauseGuard<Rc<Type>>;

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
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
    pub fn tipo(&self) -> Rc<Type> {
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

pub type TypedIfBranch = IfBranch<TypedExpr, TypedPattern>;
pub type UntypedIfBranch = IfBranch<UntypedExpr, AssignmentPattern>;

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct IfBranch<Expr, Is> {
    pub condition: Expr,
    pub body: Expr,
    pub is: Option<Is>,
    pub location: Span,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct TypedRecordUpdateArg {
    pub label: String,
    pub location: Span,
    pub value: TypedExpr,
    pub index: usize,
}

impl TypedRecordUpdateArg {
    pub fn find_node(&self, byte_index: usize) -> Option<Located<'_>> {
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
    UserDefined(TraceLevel),
    CompilerGenerated(TraceLevel),
    All(TraceLevel),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TraceLevel {
    Silent,  // No traces
    Compact, // Line numbers only
    Verbose, // Full verbose traces as provided by the user or the compiler
}

impl Tracing {
    pub fn silent() -> Self {
        Tracing::All(TraceLevel::Silent)
    }

    /// Get the tracing level based on the context we're in.
    pub fn trace_level(&self, is_code_gen: bool) -> TraceLevel {
        match self {
            Tracing::UserDefined(lvl) => {
                if is_code_gen {
                    TraceLevel::Silent
                } else {
                    *lvl
                }
            }
            Tracing::CompilerGenerated(lvl) => {
                if is_code_gen {
                    *lvl
                } else {
                    TraceLevel::Silent
                }
            }
            Tracing::All(lvl) => *lvl,
        }
    }
}

impl Display for TraceLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::result::Result<(), std::fmt::Error> {
        match self {
            TraceLevel::Silent => f.write_str("silent"),
            TraceLevel::Compact => f.write_str("compact"),
            TraceLevel::Verbose => f.write_str("verbose"),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
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

    pub fn create(i: usize, n: usize) -> Self {
        use chumsky::Span;

        Self::new((), i..i + n)
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

    /// Map the current start and end of the Span to new values.
    ///
    /// # Examples
    ///
    /// ```
    /// use aiken_lang::ast::Span;
    ///
    /// let span = Span { start: 0, end: 1 };
    ///
    /// let other = span.map(|start, end| (start + 2, end + 4));
    ///
    /// assert_eq!(other.start, 2);
    /// assert_eq!(other.end, 5);
    /// ```
    pub fn map<F: FnOnce(usize, usize) -> (usize, usize)>(&self, f: F) -> Self {
        let (start, end) = f(self.start, self.end);

        Self { start, end }
    }

    /// Map the current end of the Span to a new value.
    ///
    /// # Examples
    ///
    /// ```
    /// use aiken_lang::ast::Span;
    ///
    /// let span = Span { start: 0, end: 1 };
    ///
    /// let other = span.map_end(|end| end + 1);
    ///
    /// assert_eq!(other.start, 0);
    /// assert_eq!(other.end, 2);
    /// ```
    pub fn map_end<F: FnOnce(usize) -> usize>(&self, f: F) -> Self {
        Self {
            start: self.start,
            end: f(self.end),
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LogicalOpChainKind {
    And,
    Or,
}

impl Display for LogicalOpChainKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LogicalOpChainKind::And => write!(f, "and"),
            LogicalOpChainKind::Or => write!(f, "or"),
        }
    }
}

#[derive(Debug, thiserror::Error, Diagnostic)]
pub enum Error {
    #[error(
      "I realized the module '{}' contains the keyword '{}', which is forbidden.\n",
      name.if_supports_color(Stdout, |s| s.purple()),
      keyword.if_supports_color(Stdout, |s| s.purple())
    )]
    #[diagnostic(url("https://aiken-lang.org/language-tour/modules"))]
    #[diagnostic(code("illegal::module_name"))]
    #[diagnostic(help(r#"You cannot use keywords as part of a module path name. As a quick reminder, here's a list of all the keywords (and thus, of invalid module path names):

    as, expect, check, const, else, fn, if, is, let, opaque, pub, test, todo, trace, type, use, when"#))]
    KeywordInModuleName { name: String, keyword: String },

    #[error("I realized you used '{}' as a module name, which is reserved (and not available).\n",
        name.if_supports_color(Stdout, |s| s.purple())
    )]
    #[diagnostic(code("illegal::module_name"))]
    #[diagnostic(help(r#"Some module names are reserved for internal use. This the case of:

- aiken: where the prelude is located;
- aiken/builtin: where I store low-level Plutus builtins.

Note that 'aiken' is also imported by default; but you can refer to it explicitly to disambiguate with a local value that would clash with one from that module."#
    ))]
    ReservedModuleName { name: String },
}
