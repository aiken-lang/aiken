pub mod well_known;

use crate::{
    ast::well_known::VALIDATOR_ELSE,
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
use vec1::{Vec1, vec1};

pub const BACKPASS_VARIABLE: &str = "_backpass";
pub const CAPTURE_VARIABLE: &str = "_capture";
pub const PIPE_VARIABLE: &str = "_pipe";

pub const ENV_MODULE: &str = "env";
pub const CONFIG_MODULE: &str = "config";
pub const DEFAULT_ENV_MODULE: &str = "default";

pub const HANDLER_SPEND: &str = "spend";
pub const HANDLER_MINT: &str = "mint";
pub const HANDLER_WITHDRAW: &str = "withdraw";
pub const HANDLER_PUBLISH: &str = "publish";
pub const HANDLER_VOTE: &str = "vote";
pub const HANDLER_PROPOSE: &str = "propose";

pub type TypedModule = Module<TypeInfo, TypedDefinition>;
pub type UntypedModule = Module<(), UntypedDefinition>;

#[derive(Debug, Copy, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum ModuleKind {
    Lib,
    Validator,
    Env,
    Config,
}

impl ModuleKind {
    pub fn is_validator(&self) -> bool {
        matches!(self, ModuleKind::Validator)
    }

    pub fn is_lib(&self) -> bool {
        matches!(self, ModuleKind::Lib)
    }

    pub fn is_env(&self) -> bool {
        matches!(self, ModuleKind::Env)
    }

    pub fn is_config(&self) -> bool {
        matches!(self, ModuleKind::Config)
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
    pub fn dependencies(&self, env_modules: &[String]) -> Vec<String> {
        self.definitions()
            .flat_map(|def| {
                if let Definition::Use(Use { module, .. }) = def {
                    let name = module.join("/");
                    if name == ENV_MODULE {
                        env_modules.to_vec()
                    } else {
                        vec![name]
                    }
                } else {
                    Vec::new()
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
            Definition::Benchmark(_) => false,
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
            Definition::Benchmark(_) => false,
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
        constants: &mut IndexMap<FunctionAccessKey, TypedExpr>,
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

                Definition::Benchmark(benchmark) => {
                    functions.insert(
                        FunctionAccessKey {
                            module_name: self.name.clone(),
                            function_name: benchmark.name.clone(),
                        },
                        benchmark.clone().into(),
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

                    for (k, v) in v.into_function_definitions(module_name) {
                        functions.insert(k, v);
                    }
                }

                Definition::ModuleConstant(ModuleConstant { name, value, .. }) => {
                    constants.insert(
                        FunctionAccessKey {
                            module_name: self.name.clone(),
                            function_name: name.clone(),
                        },
                        value.clone(),
                    );
                }

                Definition::TypeAlias(_) | Definition::Use(_) => {}
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
        "bench" => Some(Token::Benchmark),
        _ => None,
    }
}

pub type TypedFunction = Function<Rc<Type>, TypedExpr, TypedArg>;
pub type UntypedFunction = Function<(), UntypedExpr, UntypedArg>;

impl UntypedFunction {
    pub fn is_default_fallback(&self) -> bool {
        matches!(
            &self.arguments[..],
            [UntypedArg {
                by: ArgBy::ByName(ArgName::Discarded { .. }),
                ..
            }]
        ) && matches!(&self.body, UntypedExpr::ErrorTerm { .. })
            && self.name.as_str() == well_known::VALIDATOR_ELSE
    }
}

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

impl<T, Expr, Arg> Function<T, Expr, Arg> {
    pub fn is_spend(&self) -> bool {
        self.name == HANDLER_SPEND
    }

    pub fn is_mint(&self) -> bool {
        self.name == HANDLER_MINT
    }
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

    pub fn has_valid_purpose_name(&self) -> bool {
        self.name == HANDLER_SPEND
            || self.name == HANDLER_PUBLISH
            || self.name == HANDLER_PROPOSE
            || self.name == HANDLER_MINT
            || self.name == HANDLER_WITHDRAW
            || self.name == HANDLER_VOTE
    }

    pub fn validator_arity(&self) -> usize {
        if self.name == HANDLER_SPEND {
            4
        } else if self.name == HANDLER_MINT
            || self.name == HANDLER_WITHDRAW
            || self.name == HANDLER_VOTE
            || self.name == HANDLER_PUBLISH
            || self.name == HANDLER_PROPOSE
        {
            3
        } else {
            panic!(
                "tried to get validator arity of a non-validator function {}",
                &self.name
            );
        }
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

pub type UntypedDataType = DataType<()>;
pub type TypedDataType = DataType<Rc<Type>>;

impl TypedDataType {
    pub fn known_enum(name: &str, constructors: &[&str]) -> Self {
        Self::known_data_type(name, &RecordConstructor::known_enum(constructors))
    }

    pub fn known_data_type(name: &str, constructors: &[RecordConstructor<Rc<Type>>]) -> Self {
        Self {
            name: name.to_string(),
            constructors: constructors.to_vec(),
            location: Span::empty(),
            opaque: false,
            public: true,
            parameters: vec![],
            typed_parameters: vec![],
            doc: None,
        }
    }

    pub fn is_never(&self) -> bool {
        self.name == well_known::NEVER
            && self.constructors.len() == well_known::NEVER_CONSTRUCTORS.len()
            && self.location == Span::empty()
            && self
                .constructors
                .iter()
                .zip(well_known::NEVER_CONSTRUCTORS)
                .all(|(constructor, name)| {
                    name == &constructor.name && constructor.arguments.is_empty()
                })
    }
}

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

pub type TypedModuleConstant = ModuleConstant<TypedExpr>;
pub type UntypedModuleConstant = ModuleConstant<UntypedExpr>;

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct ModuleConstant<Expr> {
    pub doc: Option<String>,
    pub location: Span,
    pub public: bool,
    pub name: String,
    pub annotation: Option<Annotation>,
    pub value: Expr,
}

pub type TypedValidator = Validator<Rc<Type>, TypedArg, TypedExpr>;
pub type UntypedValidator = Validator<(), UntypedArg, UntypedExpr>;

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum Purpose {
    Spend,
    Mint,
    Withdraw,
    Publish,
    Propose,
    Vote,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct Validator<T, Arg, Expr> {
    pub doc: Option<String>,
    pub end_position: usize,
    pub handlers: Vec<Function<T, Expr, Arg>>,
    pub location: Span,
    pub name: String,
    pub params: Vec<Arg>,
    pub fallback: Function<T, Expr, Arg>,
}

impl<T, Arg, Expr> Validator<T, Arg, Expr> {
    pub fn handler_name(validator: &str, handler: &str) -> String {
        format!("{}.{}", validator, handler)
    }
}

impl UntypedValidator {
    pub fn default_fallback(location: Span) -> UntypedFunction {
        Function {
            arguments: vec![UntypedArg {
                by: ArgBy::ByName(ArgName::Discarded {
                    name: "_".to_string(),
                    label: "_".to_string(),
                    location,
                }),
                location,
                annotation: None,
                doc: None,
                is_validator_param: false,
            }],
            body: UntypedExpr::fail(None, location),
            doc: None,
            location,
            end_position: location.end - 1,
            name: well_known::VALIDATOR_ELSE.to_string(),
            public: true,
            return_annotation: Some(Annotation::boolean(location)),
            return_type: (),
            on_test_failure: OnTestFailure::FailImmediately,
        }
    }
}

impl TypedValidator {
    pub fn available_handler_names() -> Vec<String> {
        vec![
            HANDLER_SPEND.to_string(),
            HANDLER_MINT.to_string(),
            HANDLER_WITHDRAW.to_string(),
            HANDLER_PUBLISH.to_string(),
            HANDLER_VOTE.to_string(),
            HANDLER_PROPOSE.to_string(),
            VALIDATOR_ELSE.to_string(),
        ]
    }

    // Define a validator wrapper extracting and matching on script purpose for
    // users.
    pub fn into_script_context_handler(&self) -> TypedExpr {
        let var_context = "__context__";
        let var_transaction = "__transaction__";
        let var_redeemer = "__redeemer__";
        let var_purpose = "__purpose__";
        let var_purpose_arg = "__purpose_arg__";
        let var_datum = "__datum__";

        let context_handler = TypedExpr::sequence(&[
            TypedExpr::let_(
                TypedExpr::local_var(var_context, Type::script_context(), self.location),
                TypedPattern::Constructor {
                    is_record: false,
                    location: Span::empty(),
                    name: well_known::SCRIPT_CONTEXT_CONSTRUCTORS[0].to_string(),
                    arguments: vec![
                        CallArg::var(var_transaction, Span::empty()),
                        CallArg::var(var_redeemer, Span::empty()),
                        CallArg::var(var_purpose, Span::empty()),
                    ],
                    module: None,
                    constructor: PatternConstructor::Record {
                        name: well_known::SCRIPT_CONTEXT_CONSTRUCTORS[0].to_string(),
                        field_map: None,
                    },
                    spread_location: None,
                    tipo: Type::function(
                        vec![Type::data(), Type::data(), Type::script_purpose()],
                        Type::data(),
                    ),
                },
                Type::script_context(),
                Span::empty(),
            ),
            TypedExpr::When {
                location: Span::empty(),
                tipo: Type::bool(),
                subject: TypedExpr::local_var(var_purpose, Type::script_purpose(), Span::empty())
                    .into(),
                clauses: self
                    .handlers
                    .iter()
                    .map(|handler| {
                        let datum = if handler.name.as_str() == "spend" {
                            handler.arguments.first()
                        } else {
                            None
                        };

                        let redeemer = handler
                            .arguments
                            .get(if datum.is_some() { 1 } else { 0 })
                            .unwrap();

                        let purpose_arg = handler.arguments.iter().nth_back(1).unwrap();

                        let transaction = handler.arguments.last().unwrap();

                        let pattern = match handler.name.as_str() {
                            "spend" => TypedPattern::spend_purpose(
                                (var_purpose_arg, purpose_arg.location),
                                (
                                    var_datum,
                                    datum.map(|x| x.location).unwrap_or(Span::empty()),
                                ),
                                redeemer.location,
                            ),
                            "mint" => TypedPattern::mint_purpose(
                                (var_purpose_arg, purpose_arg.location),
                                redeemer.location,
                            ),
                            "withdraw" => TypedPattern::withdraw_purpose(
                                (var_purpose_arg, purpose_arg.location),
                                redeemer.location,
                            ),
                            "publish" => TypedPattern::publish_purpose(
                                (var_purpose_arg, purpose_arg.location),
                                redeemer.location,
                            ),
                            "propose" => TypedPattern::propose_purpose(
                                (var_purpose_arg, purpose_arg.location),
                                redeemer.location,
                            ),
                            "vote" => TypedPattern::vote_purpose(
                                (var_purpose_arg, purpose_arg.location),
                                redeemer.location,
                            ),
                            purpose => {
                                unreachable!("unexpected/unknown purpose: {:?}", purpose)
                            }
                        };

                        let mut then = vec![];

                        // expect redeemer: tipo = __redeemer__
                        then.push(TypedExpr::flexible_expect(
                            TypedExpr::local_var(var_redeemer, Type::data(), redeemer.location),
                            TypedPattern::var(redeemer.get_variable_name().unwrap_or("_")),
                            redeemer.tipo.clone(),
                            redeemer.location,
                        ));

                        // Cast the datum, if any
                        if let Some(datum) = datum {
                            // expect datum: tipo = __datum__
                            then.push(TypedExpr::flexible_expect(
                                TypedExpr::local_var(
                                    var_datum,
                                    Type::option(Type::data()),
                                    datum.location,
                                ),
                                TypedPattern::var(datum.get_variable_name().unwrap_or("_")),
                                datum.tipo.clone(),
                                datum.location,
                            ))
                        }

                        // let purpose_arg = __purpose_arg__
                        if let Some(arg_name) = purpose_arg.get_variable_name() {
                            then.push(TypedExpr::let_(
                                TypedExpr::local_var(
                                    var_purpose_arg,
                                    Type::data(),
                                    purpose_arg.location,
                                ),
                                TypedPattern::var(arg_name),
                                purpose_arg.tipo.clone(),
                                purpose_arg.location,
                            ));
                        }

                        // let last_arg_name = __transaction__
                        if let Some(arg_name) = transaction.get_variable_name() {
                            then.push(TypedExpr::let_(
                                TypedExpr::local_var(
                                    var_transaction,
                                    Type::data(),
                                    transaction.location,
                                ),
                                TypedPattern::var(arg_name),
                                Type::data(),
                                transaction.location,
                            ));
                        }

                        then.push(handler.body.clone());

                        TypedClause {
                            location: Span::empty(),
                            pattern,
                            then: TypedExpr::Sequence {
                                location: Span::empty(),
                                expressions: then,
                            },
                        }
                    })
                    .chain(std::iter::once(&self.fallback).map(|fallback| {
                        let arg = fallback.arguments.first().unwrap();

                        let then = match arg.get_variable_name() {
                            Some(arg_name) => TypedExpr::sequence(&[
                                TypedExpr::let_(
                                    TypedExpr::local_var(
                                        var_context,
                                        arg.tipo.clone(),
                                        arg.location,
                                    ),
                                    TypedPattern::var(arg_name),
                                    arg.tipo.clone(),
                                    arg.location,
                                ),
                                fallback.body.clone(),
                            ]),
                            None => fallback.body.clone(),
                        };

                        TypedClause {
                            location: Span::empty(),
                            pattern: TypedPattern::Discard {
                                name: "_".to_string(),
                                location: arg.location,
                            },
                            then,
                        }
                    }))
                    .collect(),
            },
        ]);

        if self.handlers.is_empty() {
            let fallback = &self.fallback;
            let arg = fallback.arguments.first().unwrap();

            let then = match arg.get_variable_name() {
                Some(arg_name) => TypedExpr::sequence(&[
                    TypedExpr::let_(
                        TypedExpr::local_var(var_context, arg.tipo.clone(), arg.location),
                        TypedPattern::var(arg_name),
                        arg.tipo.clone(),
                        arg.location,
                    ),
                    fallback.body.clone(),
                ]),
                None => fallback.body.clone(),
            };

            then
        } else {
            context_handler
        }
    }

    pub fn find_node(&self, byte_index: usize) -> Option<Located<'_>> {
        self.params
            .iter()
            .find_map(|arg| arg.find_node(byte_index))
            .or_else(|| {
                self.handlers
                    .iter()
                    .find_map(|func| func.find_node(byte_index))
            })
            .or_else(|| self.fallback.find_node(byte_index))
    }

    pub fn into_function_definitions(
        &self,
        module_name: &str,
    ) -> Vec<(FunctionAccessKey, TypedFunction)> {
        self.handlers
            .iter()
            .chain(std::iter::once(&self.fallback))
            .map(|handler| {
                let mut handler = handler.clone();

                handler.arguments = self
                    .params
                    .clone()
                    .into_iter()
                    .chain(handler.arguments)
                    .collect();

                (
                    FunctionAccessKey {
                        module_name: module_name.to_string(),
                        function_name: TypedValidator::handler_name(
                            self.name.as_str(),
                            handler.name.as_str(),
                        ),
                    },
                    handler,
                )
            })
            .collect()
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

    ModuleConstant(ModuleConstant<Expr>),

    Test(Function<T, Expr, ArgVia<Arg, Expr>>),

    Benchmark(Function<T, Expr, ArgVia<Arg, Expr>>),

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
            | Definition::Benchmark(Function { location, .. })
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
            | Definition::Benchmark(Function { doc, .. })
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
            | Definition::Benchmark(Function { doc, .. })
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

impl Located<'_> {
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
            UntypedExpr::Var { name, .. } => name.contains(CAPTURE_VARIABLE),
            _ => false,
        }
    }
}

impl TypedCallArg {
    pub fn find_node(&self, byte_index: usize) -> Option<Located<'_>> {
        self.value.find_node(byte_index)
    }
}

impl CallArg<TypedPattern> {
    pub fn var(name: &str, location: Span) -> Self {
        CallArg {
            label: None,
            location: Span::empty(),
            value: TypedPattern::Var {
                location,
                name: name.to_string(),
            },
        }
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

impl<A> RecordConstructor<A>
where
    A: Clone,
{
    pub fn put_doc(&mut self, new_doc: String) {
        self.doc = Some(new_doc);
    }

    pub fn known_enum(names: &[&str]) -> Vec<RecordConstructor<A>> {
        names
            .iter()
            .map(|name| RecordConstructor {
                location: Span::empty(),
                name: name.to_string(),
                arguments: vec![],
                doc: None,
                sugar: false,
            })
            .collect()
    }

    pub fn known_record(name: &str, args: &[RecordConstructorArg<A>]) -> Self {
        RecordConstructor {
            location: Span::empty(),
            name: name.to_string(),
            arguments: args.to_vec(),
            doc: None,
            sugar: false,
        }
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
                let name = format!("{}{}_arg", ix + 1, Ordinal::<usize>(ix + 1).suffix());
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
    pub fn new(name: &str, tipo: Rc<Type>) -> Self {
        TypedArg {
            arg_name: ArgName::Named {
                name: name.to_string(),
                label: name.to_string(),
                location: Span::empty(),
            },
            location: Span::empty(),
            annotation: None,
            doc: None,
            is_validator_param: false,
            tipo: tipo.clone(),
        }
    }

    pub fn put_doc(&mut self, new_doc: String) {
        self.doc = Some(new_doc);
    }

    pub fn get_variable_name(&self) -> Option<&str> {
        self.arg_name.get_variable_name()
    }

    pub fn get_name(&self) -> String {
        self.arg_name.get_name()
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

    pub fn bytearray(location: Span) -> Self {
        Annotation::Constructor {
            name: "ByteArray".to_string(),
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

    pub fn option(inner: Annotation) -> Self {
        Annotation::Constructor {
            name: "Option".to_string(),
            module: None,
            location: inner.location(),
            arguments: vec![inner],
        }
    }

    pub fn list(inner: Annotation, location: Span) -> Self {
        Annotation::Constructor {
            name: "List".to_string(),
            module: None,
            arguments: vec![inner],
            location,
        }
    }

    pub fn tuple(elems: Vec<Annotation>, location: Span) -> Self {
        Annotation::Tuple { elems, location }
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

pub type UntypedPattern = Pattern<(), (), Namespace, (u8, Span)>;
pub type TypedPattern = Pattern<PatternConstructor, Rc<Type>, String, u8>;

impl TypedPattern {
    pub fn var(name: &str) -> Self {
        TypedPattern::Var {
            name: name.to_string(),
            location: Span::empty(),
        }
    }

    pub fn constructor(
        name: &str,
        arguments: &[CallArg<TypedPattern>],
        tipo: Rc<Type>,
        location: Span,
    ) -> Self {
        TypedPattern::Constructor {
            is_record: false,
            location,
            name: name.to_string(),
            arguments: arguments.to_vec(),
            module: None,
            constructor: PatternConstructor::Record {
                name: name.to_string(),
                field_map: None,
            },
            spread_location: None,
            tipo: tipo.clone(),
        }
    }

    pub fn mint_purpose(
        (var_purpose_arg, purpose_span): (&str, Span),
        redeemer_span: Span,
    ) -> Self {
        TypedPattern::constructor(
            well_known::SCRIPT_PURPOSE_MINT,
            &[CallArg::var(var_purpose_arg, purpose_span)],
            Type::function(vec![Type::byte_array()], Type::script_purpose()),
            redeemer_span,
        )
    }

    pub fn spend_purpose(
        (var_purpose_arg, purpose_span): (&str, Span),
        (var_datum, datum_span): (&str, Span),
        redeemer_span: Span,
    ) -> Self {
        TypedPattern::constructor(
            well_known::SCRIPT_PURPOSE_SPEND,
            &[
                CallArg::var(var_purpose_arg, purpose_span),
                CallArg::var(var_datum, datum_span),
            ],
            Type::function(
                vec![Type::data(), Type::option(Type::data())],
                Type::script_purpose(),
            ),
            redeemer_span,
        )
    }

    pub fn withdraw_purpose(
        (var_purpose_arg, purpose_span): (&str, Span),
        redeemer_span: Span,
    ) -> Self {
        TypedPattern::constructor(
            well_known::SCRIPT_PURPOSE_WITHDRAW,
            &[CallArg::var(var_purpose_arg, purpose_span)],
            Type::function(vec![Type::data()], Type::script_purpose()),
            redeemer_span,
        )
    }

    pub fn publish_purpose(
        (var_purpose_arg, purpose_span): (&str, Span),
        redeemer_span: Span,
    ) -> Self {
        TypedPattern::constructor(
            well_known::SCRIPT_PURPOSE_PUBLISH,
            &[
                CallArg::var("__discarded_purpose_ix__", purpose_span),
                CallArg::var(var_purpose_arg, purpose_span),
            ],
            Type::function(vec![Type::int(), Type::data()], Type::script_purpose()),
            redeemer_span,
        )
    }

    pub fn vote_purpose(
        (var_purpose_arg, purpose_span): (&str, Span),
        redeemer_span: Span,
    ) -> Self {
        TypedPattern::constructor(
            well_known::SCRIPT_PURPOSE_VOTE,
            &[CallArg::var(var_purpose_arg, purpose_span)],
            Type::function(vec![Type::data()], Type::script_purpose()),
            redeemer_span,
        )
    }

    pub fn propose_purpose(
        (var_purpose_arg, purpose_span): (&str, Span),
        redeemer_span: Span,
    ) -> Self {
        TypedPattern::constructor(
            well_known::SCRIPT_PURPOSE_PROPOSE,
            &[
                CallArg::var("__discarded_purpose_ix__", purpose_span),
                CallArg::var(var_purpose_arg, purpose_span),
            ],
            Type::function(vec![Type::int(), Type::data()], Type::script_purpose()),
            redeemer_span,
        )
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum Namespace {
    Module(String),
    Type(Option<String>, String),
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum Pattern<Constructor, Type, NamespaceKind, ByteValue> {
    Int {
        location: Span,
        value: String,
        base: Base,
    },

    ByteArray {
        location: Span,
        value: Vec<ByteValue>,
        preferred_format: ByteArrayFormatPreference,
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
        module: Option<NamespaceKind>,
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

impl<A, B, C, BV> Pattern<A, B, C, BV> {
    pub fn location(&self) -> Span {
        match self {
            Pattern::Assign { pattern, .. } => pattern.location(),
            Pattern::Int { location, .. }
            | Pattern::Var { location, .. }
            | Pattern::List { location, .. }
            | Pattern::Discard { location, .. }
            | Pattern::Tuple { location, .. }
            | Pattern::Pair { location, .. }
            | Pattern::ByteArray { location, .. }
            | Pattern::Constructor { location, .. } => *location,
        }
    }

    /// Returns true when a Pattern can be displayed in a flex-break manner (i.e. tries to fit as
    /// much as possible on a single line). When false, long lines with several of those patterns
    /// will be broken down to one pattern per line.
    pub fn is_simple_pattern_to_format(&self) -> bool {
        match self {
            Self::ByteArray { .. } | Self::Int { .. } | Self::Var { .. } | Self::Discard { .. } => {
                true
            }
            Self::Pair { fst, snd, .. } => {
                fst.is_simple_pattern_to_format() && snd.is_simple_pattern_to_format()
            }
            Self::Tuple { elems, .. } => elems.iter().all(|e| e.is_simple_pattern_to_format()),
            Self::List { elements, .. } if elements.len() <= 3 => {
                elements.iter().all(|e| e.is_simple_pattern_to_format())
            }
            Self::Constructor { arguments, .. } => arguments.is_empty(),
            _ => false,
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

    pub fn collect_identifiers<F>(&self, collect: &mut F)
    where
        F: FnMut((String, Span)),
    {
        match self {
            Pattern::Var { name, location } => {
                collect((name.to_string(), *location));
            }
            Pattern::List { elements, .. } => {
                elements.iter().for_each(|e| e.collect_identifiers(collect));
            }
            Pattern::Pair { fst, snd, .. } => {
                fst.collect_identifiers(collect);
                snd.collect_identifiers(collect);
            }
            Pattern::Tuple { elems, .. } => {
                elems.iter().for_each(|e| e.collect_identifiers(collect));
            }
            Pattern::Constructor { arguments, .. } => {
                arguments
                    .iter()
                    .for_each(|arg| arg.value.collect_identifiers(collect));
            }
            Pattern::Int { .. }
            | Pattern::ByteArray { .. }
            | Pattern::Discard { .. }
            | Pattern::Assign { .. } => {}
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
            | Pattern::ByteArray { .. }
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
            Pattern::Int { .. } => Some(Type::int()),
            Pattern::ByteArray { .. } => Some(Type::byte_array()),
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

                impl serde::de::Visitor<'_> for FieldVisitor {
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
            Bls12_381Point::G1(_) => Type::g1_element(),
            Bls12_381Point::G2(_) => Type::g2_element(),
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

pub type MultiPattern<PatternConstructor, Type, NamespaceKind, ByteValue> =
    Vec<Pattern<PatternConstructor, Type, NamespaceKind, ByteValue>>;

pub type UntypedMultiPattern = MultiPattern<(), (), Namespace, (u8, Span)>;
pub type TypedMultiPattern = MultiPattern<PatternConstructor, Rc<Type>, String, u8>;

#[derive(Debug, Clone, PartialEq)]
pub struct UntypedClause {
    pub location: Span,
    pub patterns: Vec1<UntypedPattern>,
    pub then: UntypedExpr,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct TypedClause {
    pub location: Span,
    pub pattern: TypedPattern,
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

pub struct UntypedClauseGuard {}

pub type TypedIfBranch = IfBranch<TypedExpr, (TypedPattern, Rc<Type>)>;
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
    pub fn verbose() -> Self {
        Tracing::All(TraceLevel::Verbose)
    }

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
        Self::new(span.start.into(), span.end - span.start)
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
