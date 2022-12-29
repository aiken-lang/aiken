use super::Type;
use crate::{
    ast::{Annotation, BinOp, CallArg, Span, TodoKind, UntypedPattern},
    format::Formatter,
    levenshtein,
    pretty::Documentable,
};
use indoc::formatdoc;
use miette::{Diagnostic, LabeledSpan};
use ordinal::Ordinal;
use owo_colors::OwoColorize;
use std::{collections::HashMap, fmt::Display, sync::Arc};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("I found two function arguments both called '{}'.\n", label.purple())]
    DuplicateArgument { locations: Vec<Span>, label: String },

    #[error("I found two top-level constants declared with the same name: '{}'.\n", name.purple())]
    DuplicateConstName {
        location: Span,
        previous_location: Span,
        name: String,
    },

    #[error("I noticed you were importing '{}' twice.\n", name.purple())]
    DuplicateImport {
        location: Span,
        previous_location: Span,
        name: String,
    },

    #[error("I stumbled upon the field '{}' twice in a data-type definition.\n", label.purple())]
    DuplicateField { locations: Vec<Span>, label: String },

    #[error("I discovered two top-level objects referred to as '{}'.\n", name.purple())]
    DuplicateName {
        location: Span,
        previous_location: Span,
        name: String,
    },

    #[error("I found two types declared with the same name: '{}'.\n", name.purple())]
    DuplicateTypeName {
        location: Span,
        previous_location: Span,
        name: String,
    },

    #[error("I saw a {} fields in a context where there should be {}.\n", given.purple(), expected.purple())]
    IncorrectFieldsArity {
        location: Span,
        expected: usize,
        given: usize,
        labels: Vec<String>,
    },

    #[error("I saw a function or constructor that expects {} arguments be called with {} arguments.\n", expected.purple(), given.purple())]
    IncorrectFunctionCallArity {
        location: Span,
        expected: usize,
        given: usize,
    },

    #[error("I saw a pattern on a constructor that has {} fields be matched with {} arguments.\n", expected.purple(), given.len().purple())]
    IncorrectPatternArity {
        location: Span,
        expected: usize,
        given: Vec<CallArg<UntypedPattern>>,
        name: String,
        module: Option<String>,
        is_record: bool,
    },

    // TODO: Since we do not actually support patterns on multiple items, we won't likely ever
    // encounter that error. We could simplify a bit the type-checker and get rid of that error
    // eventually.
    #[error("I counted {} different clauses in a multi-pattern instead of {}.\n", given.purple(), expected.purple())]
    IncorrectNumClausePatterns {
        location: Span,
        expected: usize,
        given: usize,
    },

    #[error("I saw a pattern on a {}-tuple be matched into a {}-tuple.\n", expected.purple(), given.purple())]
    IncorrectTupleArity {
        location: Span,
        expected: usize,
        given: usize,
    },

    #[error("I noticed a generic data-type with {} type parameters instead of {}.\n", given.purple(), expected.purple())]
    IncorrectTypeArity {
        location: Span,
        name: String,
        expected: usize,
        given: usize,
    },

    #[error("I realized that a given 'when/is' expression is non-exhaustive.\n")]
    NotExhaustivePatternMatch {
        location: Span,
        unmatched: Vec<String>,
    },

    #[error("I tripped over a call attempt on something that isn't a function.\n")]
    NotFn { location: Span, tipo: Arc<Type> },

    #[error(
      "I realized the module '{}' contains the keyword '{}', which is forbidden.\n",
      name.purple(),
      keyword.purple()
    )]
    KeywordInModuleName { name: String, keyword: String },

    #[error("I stumble upon an invalid (non-local) clause guard '{}'.\n", name.purple())]
    NonLocalClauseGuardVariable { location: Span, name: String },

    #[error("I discovered a positional argument after a label argument.\n")]
    PositionalArgumentAfterLabeled {
        location: Span,
        labeled_arg_location: Span,
    },

    #[error("I caught a private value trying to escape.\n")]
    PrivateTypeLeak { location: Span, leaked: Type },

    #[error("I couldn't figure out the type of a record you're trying to access.\n")]
    RecordAccessUnknownType { location: Span },

    #[error("I tripped over an invalid constructor in a record update.\n")]
    RecordUpdateInvalidConstructor { location: Span },

    #[error("I realized you used '{}' as a module name, which is reserved (and not available).\n", name.purple())]
    ReservedModuleName { name: String },

    #[error("I tripped over the following labeled argument: {}.\n", label.purple())]
    UnexpectedLabeledArg { location: Span, label: String },

    #[error("I tripped over the following labeled argument: {}.\n", label.purple())]
    UnexpectedLabeledArgInPattern {
        location: Span,
        label: String,
        name: String,
        args: Vec<CallArg<UntypedPattern>>,
        module: Option<String>,
        with_spread: bool,
    },

    // TODO: Seems like we can't really trigger this error because we allow type holes everywhere
    // anyway. We need to revise that perhaps.
    #[error("I stumbled upon an unexpected type hole.\n")]
    UnexpectedTypeHole { location: Span },

    #[error("I tripped over some unknown labels in a pattern or function.\n")]
    UnknownLabels {
        unknown: Vec<(String, Span)>,
        valid: Vec<String>,
        supplied: Vec<String>,
    },

    #[error("I stumble upon a reference to an unknown module: '{}'\n", name.purple())]
    UnknownModule {
        location: Span,
        name: String,
        imported_modules: Vec<String>,
    },

    #[error(
        "I found an unknown import '{}' from module '{}'\n",
        name.purple(),
        module_name.purple()
    )]
    UnknownModuleField {
        location: Span,
        name: String,
        module_name: String,
        value_constructors: Vec<String>,
        type_constructors: Vec<String>,
    },

    #[error("I tried to find '{}' in '{}' but didn't.\n", name.purple(), module_name.purple())]
    UnknownModuleValue {
        location: Span,
        name: String,
        module_name: String,
        value_constructors: Vec<String>,
    },

    #[error("I tried to find '{}' in '{}' but didn't.\n", name.purple(), module_name.purple())]
    UnknownModuleType {
        location: Span,
        name: String,
        module_name: String,
        type_constructors: Vec<String>,
    },

    #[error(
      "I tried to find the field '{}' in a record of type '{}' but didn't.\n",
      label.purple(),
      typ.to_pretty(4).purple()
    )]
    UnknownRecordField {
        location: Span,
        typ: Arc<Type>,
        label: String,
        fields: Vec<String>,
        situation: Option<UnknownRecordFieldSituation>,
    },

    #[error("I found a reference to an unknown type: '{}'.\n", name.purple())]
    UnknownType {
        location: Span,
        name: String,
        types: Vec<String>,
    },

    #[error("I found a reference to an unknown variable: '{}'.\n", name.purple())]
    UnknownVariable {
        location: Span,
        name: String,
        variables: Vec<String>,
    },

    #[error("I found a reference to an unknown data-type constructor: '{}'.\n", name.purple())]
    UnknownTypeConstructor {
        location: Span,
        name: String,
        constructors: Vec<String>,
    },

    #[error("I discovered a redundant spread operator.\n")]
    UnnecessarySpreadOperator { location: Span, arity: usize },

    #[error("I tripped over a record-update on a data-type with more than one constructor.\n")]
    UpdateMultiConstructorType { location: Span },

    #[error("I struggled to unify the types of two expressions.\n")]
    CouldNotUnify {
        location: Span,
        expected: Arc<Type>,
        given: Arc<Type>,
        situation: Option<UnifyErrorSituation>,
        rigid_type_names: HashMap<u64, String>,
    },

    #[error("I tripped over an extra variable in an alternative pattern: {}.\n", name.purple())]
    ExtraVarInAlternativePattern { location: Span, name: String },

    #[error("I found a missing variable in an alternative pattern: {}.\n", name.purple())]
    MissingVarInAlternativePattern { location: Span, name: String },

    #[error("I realized the variable '{}' was mentioned more than once in an alternative pattern.\n ", name.purple())]
    DuplicateVarInPattern { location: Span, name: String },

    #[error("I almost got caught in an infinite cycle of type definitions: {}.\n", types.join(" -> "))]
    CyclicTypeDefinitions { location: Span, types: Vec<String> },

    #[error("I almost got caught in an endless loop while inferring a recursive type.\n")]
    RecursiveType { location: Span },

    #[error(
        "I tripped over an attempt to access tuple elements on something else than a tuple.\n"
    )]
    NotATuple { location: Span, tipo: Arc<Type> },

    #[error(
        "I discovered an attempt to access the {} element of a {}-tuple.\n",
        Ordinal(*index + 1).to_string().purple(),
        size.purple()
    )]
    TupleIndexOutOfBound {
        location: Span,
        index: usize,
        size: usize,
    },
}

impl Error {
    pub fn call_situation(mut self) -> Self {
        if let Error::UnknownRecordField {
            ref mut situation, ..
        } = self
        {
            *situation = Some(UnknownRecordFieldSituation::FunctionCall);
        }
        self
    }

    pub fn case_clause_mismatch(self) -> Self {
        self.with_unify_error_situation(UnifyErrorSituation::CaseClauseMismatch)
    }

    pub fn flip_unify(self) -> Error {
        match self {
            Error::CouldNotUnify {
                location,
                expected,
                given,
                situation: note,
                rigid_type_names,
            } => Error::CouldNotUnify {
                location,
                expected: given,
                given: expected,
                situation: note,
                rigid_type_names,
            },
            other => other,
        }
    }

    pub fn operator_situation(self, binop: BinOp) -> Self {
        self.with_unify_error_situation(UnifyErrorSituation::Operator(binop))
    }

    pub fn return_annotation_mismatch(self) -> Self {
        self.with_unify_error_situation(UnifyErrorSituation::ReturnAnnotationMismatch)
    }

    pub fn with_unify_error_rigid_names(mut self, new_names: &HashMap<u64, String>) -> Self {
        match self {
            Error::CouldNotUnify {
                rigid_type_names: ref mut annotated_names,
                ..
            } => {
                *annotated_names = new_names.clone();
                self
            }
            _ => self,
        }
    }

    pub fn with_unify_error_situation(self, situation: UnifyErrorSituation) -> Self {
        match self {
            Self::CouldNotUnify {
                expected,
                given,
                location,
                rigid_type_names,
                ..
            } => Self::CouldNotUnify {
                expected,
                given,
                situation: Some(situation),
                location,
                rigid_type_names,
            },
            other => other,
        }
    }
}

impl Diagnostic for Error {
    fn severity(&self) -> Option<miette::Severity> {
        Some(miette::Severity::Error)
    }

    // Unique diagnostic code that can be used to look up more information about this Diagnostic. Ideally also globally unique, and documented in the toplevel crate’s documentation for easy searching. Rust path format (foo::bar::baz) is recommended, but more classic codes like E0123 or enums will work just fine.
    fn code<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        match self {
            Self::DuplicateArgument { .. } => Some(Box::new("duplicate_argument")),
            Self::DuplicateConstName { .. } => Some(Box::new("duplicate_const_name")),
            Self::DuplicateImport { .. } => Some(Box::new("duplicate_import")),
            Self::DuplicateField { .. } => Some(Box::new("duplicate_field")),
            Self::DuplicateName { .. } => Some(Box::new("duplicate_name")),
            Self::DuplicateTypeName { .. } => Some(Box::new("duplicate_type_name")),
            Self::IncorrectFieldsArity { .. } => Some(Box::new("incorrect_fields_arity")),
            Self::IncorrectFunctionCallArity { .. } => Some(Box::new("incorrect_fn_arity")),
            Self::IncorrectPatternArity { .. } => Some(Box::new("incorrect_pattern_arity")),
            Self::IncorrectNumClausePatterns { .. } => {
                Some(Box::new("incorrect_num_clause_patterns"))
            }
            Self::IncorrectTupleArity { .. } => Some(Box::new("incorrect_tuple_arity")),
            Self::IncorrectTypeArity { .. } => Some(Box::new("incorrect_type_arity")),
            Self::NotExhaustivePatternMatch { .. } => {
                Some(Box::new("non_exhaustive_pattern_match"))
            }
            Self::NotFn { .. } => Some(Box::new("not_fn")),
            Self::KeywordInModuleName { .. } => Some(Box::new("keyword_in_module_name")),
            Self::NonLocalClauseGuardVariable { .. } => {
                Some(Box::new("non_local_clause_guard_variable"))
            }
            Self::PositionalArgumentAfterLabeled { .. } => {
                Some(Box::new("positional_argument_after_labeled"))
            }
            Self::PrivateTypeLeak { .. } => Some(Box::new("private_type_leak")),
            Self::RecordAccessUnknownType { .. } => Some(Box::new("record_access_unknown_type")),
            Self::RecordUpdateInvalidConstructor { .. } => {
                Some(Box::new("record_update_invalid_constructor"))
            }
            Self::ReservedModuleName { .. } => Some(Box::new("reserved_module_name")),
            Self::UnexpectedLabeledArg { .. } => Some(Box::new("unexpected_labeled_arg")),
            Self::UnexpectedLabeledArgInPattern { .. } => {
                Some(Box::new("unexpected_labeled_arg_in_pattern"))
            }
            Self::UnexpectedTypeHole { .. } => Some(Box::new("unexpected_type_hole")),
            Self::UnknownLabels { .. } => Some(Box::new("unknown_labels")),
            Self::UnknownModule { .. } => Some(Box::new("unknown_module")),
            Self::UnknownModuleField { .. } => Some(Box::new("unknown_module_field")),
            Self::UnknownModuleValue { .. } => Some(Box::new("unknown_module_value")),
            Self::UnknownModuleType { .. } => Some(Box::new("unknown_module_type")),
            Self::UnknownRecordField { .. } => Some(Box::new("unknown_record_field")),
            Self::UnknownType { .. } => Some(Box::new("unknown_type")),
            Self::UnknownVariable { .. } => Some(Box::new("unknown_variable")),
            Self::UnknownTypeConstructor { .. } => Some(Box::new("unknown_type_constructor")),
            Self::UnnecessarySpreadOperator { .. } => Some(Box::new("unnecessary_spread_operator")),
            Self::UpdateMultiConstructorType { .. } => {
                Some(Box::new("update_multi_constructor_type"))
            }
            Self::CouldNotUnify { .. } => Some(Box::new("could_not_unify")),
            Self::ExtraVarInAlternativePattern { .. } => {
                Some(Box::new("extra_var_in_alternative_pattern"))
            }
            Self::MissingVarInAlternativePattern { .. } => {
                Some(Box::new("missing_var_in_alternative_pattern"))
            }
            Self::DuplicateVarInPattern { .. } => Some(Box::new("duplicate_var_in_pattern")),
            Self::CyclicTypeDefinitions { .. } => Some(Box::new("cyclic_type_definitions")),
            Self::RecursiveType { .. } => Some(Box::new("recursive_type")),
            Self::NotATuple { .. } => Some(Box::new("not_a_tuple")),
            Self::TupleIndexOutOfBound { .. } => Some(Box::new("tuple_index_out_of_bound")),
        }
    }

    // Additional help text related to this Diagnostic. Do you have any advice for the poor soul who’s just run into this issue?
    fn help<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        match self {
            Self::DuplicateArgument { .. } => {
                // TODO: Suggest names based on types when the duplicate argument is `_`
                Some(Box::new(formatdoc! {
                    r#"Function arguments cannot have the same name. You can use '{discard}' and numbers to distinguish between similar names.
                    "#
                    , discard = "_".yellow()
                }))
            },

            Self::DuplicateConstName { .. } => Some(Box::new(formatdoc! {
                r#"Top-level constants of a same module cannot have the same name. You can use '{discard}' and numbers to distinguish between similar names.
                "#
                , discard = "_".yellow()
            })),

            Self::DuplicateImport { .. } => Some(Box::new(formatdoc! {
                r#"The best thing to do from here is to remove one of them.
                "#
            })),

            Self::DuplicateField { .. } => Some(Box::new(formatdoc! {
                r#"Data-types must have fields with strictly different names. You can use '{discard}' and numbers to distinguish between similar names.
                   Note that it is also possible to declare data-types with positional (nameless) fields only.
                   For example:

                     ┍━━━━━━━━━━━━━━━━━━━━━━━
                     │ {keyword_pub} {keyword_type} {type_Point} {{
                     │   {variant_Point}({type_Int}, {type_Int}, {type_Int})
                     │ }}
                "#
                , discard = "_".yellow()
                , keyword_pub = "pub".bright_blue()
                , keyword_type = "type".yellow()
                , type_Int = "Int".green()
                , type_Point = "Point".green()
                , variant_Point = "Point".green()
            })),

            Self::DuplicateName { .. } => Some(Box::new(formatdoc! {
                r#"Top-level definitions cannot have the same name, even if they refer to objects with different natures (e.g. function and test).

                   You can use '{discard}' and numbers to distinguish between similar names.
                "#
                , discard = "_".yellow()
            })),

            Self::DuplicateTypeName { .. } => Some(Box::new(formatdoc! {
                r#"Types cannot have the same top-level name. You {cannot} use '_' in types name, but you can use numbers to distinguish between similar names.
                "#
                , cannot = "cannot".red()
            })),

            Self::IncorrectFieldsArity { .. } => None,

            Self::IncorrectFunctionCallArity { expected, .. } => Some(Box::new(formatdoc! {
                r#"Functions (and constructors) must always be called with all their arguments (comma-separated, between brackets).

                   Here, the function or constructor needs {expected} arguments.

                   Note that Aiken supports argument capturing using '{discard}' as placeholder for arguments that aren't yet defined. This is like currying in some other languages.

                   For example, imagine the following function:

                     ┍━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
                     │ {keyword_fn} add(x: {type_Int}, y: {type_Int}) -> {type_Int}

                   From there, you can define 'increment', a function that takes a single argument and adds one to it, as such:

                     ┍━━━━━━━━━━━━━━━━━━━━━━━━━━
                     │ {keyword_let} increment = add(1, _)
                "#
                , discard = "_".yellow()
                , expected = expected.purple()
                , keyword_fn = "fn".yellow()
                , keyword_let = "let".yellow()
                , type_Int = "Int".green()
            })),

            Self::IncorrectPatternArity {
                name,
                given,
                expected,
                module,
                is_record,
                ..
            } => {
                let pattern = Formatter::new()
                    .pattern_constructor(name, given, module, true, *is_record)
                    .to_pretty_string(70);

                let suggestion = if expected > &given.len() {
                    formatdoc! {
                        r#" Perhaps, try the following:

                            ╰─▶  {pattern}
                        "#
                    }
                } else {
                    String::new()
                };

                Some(Box::new(formatdoc! {
                    r#"When pattern-matching on constructors, you must either match the exact number of fields, or use the spread operator. Note that unused fields must be discarded by prefixing their name with '{discard}'.{suggestion}
                    "#
                    , discard = "_".yellow()
                }))
            },

            Self::IncorrectNumClausePatterns { .. } => None,

            Self::IncorrectTupleArity { .. } => Some(Box::new(formatdoc! {
                r#"When pattern matching on a tuple, you must match all of its elements. Note that unused fields must be discarded by prefixing their name with '{discard}'.
                "#
                , discard = "_".yellow()
            })),

            Self::IncorrectTypeArity { expected, name, .. } => {
                let mut args = vec![];
                for i in 0..*expected {
                    args.push(Annotation::Var {
                        name: char::from_u32(97 + i as u32).unwrap_or('?').to_string(),
                        location: Span::empty(),
                    });
                }

                let suggestion = name
                    .to_doc()
                    .append(Formatter::new().type_arguments(&args))
                    .to_pretty_string(70);

                Some(Box::new(formatdoc! {
                    r#"Data-types that are generic in one or more types must be written with all their generic types in type annotations. Generic types must be indicated between chevrons '{chevron_left}' and '{chevron_right}'.

                       Perhaps, try the following:

                       ╰─▶  {suggestion}
                    "#
                    , chevron_left = "<".yellow()
                    , chevron_right = ">".yellow()
                }))
            },

            Self::NotExhaustivePatternMatch { unmatched, .. } => {
                let missing = unmatched
                    .iter()
                    .map(|s| format!("─▶ {s}"))
                    .collect::<Vec<_>>()
                    .join("\n");

                Some(Box::new(formatdoc! {
                    r#"When clauses must be exhaustive -- that is, they must cover all possible cases of the type they match. While it is recommended to have an explicit branch for each constructor, you can also use the wildcard '{discard}' as a last branch to match any remaining result.

                       In this particular instance, the following cases are missing:

                       {missing}
                    "#
                    , discard = "_".yellow()
                }))
            },

            Self::NotFn { tipo, .. } => {
                let inference = tipo.to_pretty(4);
                Some(Box::new(formatdoc! {
                    r#"It seems like you're trying to call something that isn't a function. I am inferring the following type:

                       ╰─▶  {inference}
                    "#
                }))
            },

            Self::KeywordInModuleName { .. } => Some(Box::new(formatdoc! {
                r#"You cannot use keywords as part of a module path name. As a quick reminder, here's a list of all the keywords (and thus, of invalid module path names):

                   as, assert, check, const, else, fn, if, is, let, opaque, pub, test, todo, trace, type, use, when,
                "#
            })),

            Self::NonLocalClauseGuardVariable { .. } => Some(Box::new(formatdoc! {
                r#"There are some conditions regarding what can be used in a guard. Values must be either local to the function, or defined as module constants. You can't use functions or records in there.
                "#
            })),

            Self::PositionalArgumentAfterLabeled { .. } => Some(Box::new(formatdoc! {
                r#"You can mix positional and labeled arguments, but you must put all positional arguments (i.e. without label) at the front.

                   To fix this, you'll need to either turn that argument as a labeled argument, or make the next one positional.
                "#
            })),

            Self::PrivateTypeLeak { leaked, .. } => Some(Box::new(formatdoc! {
                r#"I found a public value that is making use of a private type. This would prevent other modules from actually using that value because they wouldn't know what this type refer to.

                   The culprit is:

                   {type_info}

                   Maybe you meant to turn it public using the '{keyword_pub}' keyword?
                "#
               , type_info = leaked.to_pretty(4).red()
               , keyword_pub = "pub".bright_blue()
            })),

            Self::RecordAccessUnknownType { .. } => Some(Box::new(formatdoc! {
                r#"I do my best to infer types of any expression; yet sometimes I need help (don't we all?).

                   Take for example the following expression:

                     ┍━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
                     │ {keyword_let} foo = {keyword_fn}(x) {{ x.transaction }}

                   At this stage, I can't quite figure out whether 'x' has indeed a field 'transaction', because I don't know what the type of 'x' is.
                   You can help me by providing a type-annotation for 'x', as such:

                     ┍━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
                     │ {keyword_let} foo = {keyword_fn}(x: {type_ScriptContext}) {{ x.transaction }}
                "#
                , keyword_fn = "fn".yellow()
                , keyword_let = "let".yellow()
                , type_ScriptContext = "ScriptContext".green()
            })),

            // TODO: Come back to this one once we support record updates.
            Self::RecordUpdateInvalidConstructor { .. } => None,

            Self::ReservedModuleName { .. } => Some(Box::new(formatdoc! {
                r#"Some module names are reserved for internal use. This the case of:

                   - aiken: where the prelude is located;
                   - aiken/builtin: where I store low-level Plutus builtins.

                   Note that 'aiken' is also imported by default; but you can refer to it explicitly to disambiguate with a local value that would clash with one from that module.
                "#
            })),

            Self::UnexpectedLabeledArg { .. } => None,

            Self::UnexpectedLabeledArgInPattern {
                args,
                module,
                name,
                with_spread,
                ..
            } => {
                let fixed_args = args
                    .iter()
                    .map(|arg| CallArg {
                        label: None,
                        location: arg.location,
                        value: arg.value.clone(),
                    })
                    .collect::<Vec<_>>();

                let suggestion = Formatter::new()
                    .pattern_constructor(name, &fixed_args, module, *with_spread, false)
                    .to_pretty_string(70);

                Some(Box::new(formatdoc! {
                    r#"The constructor '{constructor}' does not have any labeled field. Its fields must therefore be matched only by position.

                       Perhaps, try the following:

                       ╰─▶  {suggestion}
                    "#
                    , constructor = name.green()
                }))
            },

            Self::UnexpectedTypeHole { .. } => None,

            Self::UnknownLabels { valid, .. } => {
                let known_labels = valid
                    .iter()
                    .map(|s| format!("─▶ {s}"))
                    .collect::<Vec<_>>()
                    .join("\n");
                Some(Box::new(formatdoc! {
                    r#"I don't know some of the labels used in this expression. I've highlighted them just above.

                       Here's a list of all the (valid) labels that I know of:

                       {known_labels}
                    "#
                }))
            },

            Self::UnknownModule {
                name,
                imported_modules,
                ..
            } => Some(
                imported_modules
                    .iter()
                    .map(|s| (s, levenshtein::distance(name, s)))
                    .min_by(|(_, a), (_, b)| a.cmp(b))
                    .and_then(|(suggestion, distance)| {
                        if distance <= 4 {
                            Some(Box::new(format!("Did you mean '{}'?", suggestion.yellow())))
                        } else {
                            None
                        }
                    })
                    .unwrap_or_else(|| Box::new("Did you forget to import it?".to_string())),
            ),

            Self::UnknownModuleField {
                name,
                value_constructors,
                type_constructors,
                ..
            } => {
                let mut candidates = vec![];
                candidates.extend(value_constructors);
                candidates.extend(type_constructors);
                Some(
                    candidates
                        .iter()
                        .map(|s| (s, levenshtein::distance(name, s)))
                        .min_by(|(_, a), (_, b)| a.cmp(b))
                        .and_then(|(suggestion, distance)| {
                            if distance <= 4 {
                                Some(Box::new(format!("Did you mean to import '{}'?", suggestion.yellow())))
                            } else {
                                None
                            }
                        })
                        .unwrap_or_else(|| {
                            Box::new(formatdoc! {
                                r#"Did you forget to make this value public?

                                   Values from module must be exported using the keyword '{keyword_pub}' in order to be available from other modules.
                                   For example:

                                     ┍━ aiken/foo.ak ━━━━━━━━
                                     │ {keyword_fn} foo() {{ {literal_foo} }}
                                     │
                                     │ {keyword_pub} {keyword_type} {type_Bar} {{
                                     │   {variant_Bar}
                                     │ }}

                                   The function 'foo' is private and can't be accessed from outside of the 'aiken/foo' module. But the data-type '{type_Bar}' is public and available.
                                "#
                                , keyword_fn = "fn".yellow()
                                , keyword_pub = "pub".bright_blue()
                                , keyword_type = "type".yellow()
                                , literal_foo = "\"foo\"".bright_purple()
                                , type_Bar = "Bar".green()
                                , variant_Bar = "Bar".green()
                            })
                        }),
                )
            },

            Self::UnknownModuleValue {
                name,
                value_constructors,
                ..
            } => Some(
                value_constructors
                    .iter()
                    .map(|s| (s, levenshtein::distance(name, s)))
                    .min_by(|(_, a), (_, b)| a.cmp(b))
                    .and_then(|(suggestion, distance)| {
                        if distance <= 4 {
                            Some(Box::new(format!("Did you mean '{}'?", suggestion.yellow())))
                        } else {
                            None
                        }
                    })
                    .unwrap_or_else(|| {
                        Box::new(formatdoc! {
                            r#"Did you forget to make this value public?

                               Values from module must be exported using the keyword '{keyword_pub}' in order to be available from other modules.
                               For example:

                                 ┍━ aiken/foo.ak ━━━━━━━━
                                 │ {keyword_fn} foo() {{ {literal_foo} }}
                                 │
                                 │ {keyword_pub} {keyword_type} {type_Bar} {{
                                 │   {variant_Bar}
                                 │ }}

                               The function 'foo' is private and can't be accessed from outside of the 'aiken/foo' module. But the data-type '{type_Bar}' is public and available.
                            "#
                            , keyword_fn = "fn".yellow()
                            , keyword_pub = "pub".bright_blue()
                            , keyword_type = "type".yellow()
                            , literal_foo = "\"foo\"".bright_purple()
                            , type_Bar = "Bar".green()
                            , variant_Bar = "Bar".green()
                        })
                    }),
            ),

            Self::UnknownModuleType {
                name,
                type_constructors,
                ..
            } => Some(
                type_constructors
                    .iter()
                    .map(|s| (s, levenshtein::distance(name, s)))
                    .min_by(|(_, a), (_, b)| a.cmp(b))
                    .and_then(|(suggestion, distance)| {
                        if distance <= 4 {
                            Some(Box::new(format!("Did you mean '{}'?", suggestion.yellow())))
                        } else {
                            None
                        }
                    })
                    .unwrap_or_else(|| {
                        Box::new(formatdoc! {
                            r#"Did you forget to make this value public?

                               Values from module must be exported using the keyword '{keyword_pub}' in order to be available from other modules.
                               For example:

                                 ┍━ aiken/foo.ak ━━━━━━━━
                                 │ {keyword_fn} foo() {{ {literal_foo} }}
                                 │
                                 │ {keyword_pub} {keyword_type} {type_Bar} {{
                                 │   {variant_Bar}
                                 │ }}

                               The function 'foo' is private and can't be accessed from outside of the 'aiken/foo' module. But the data-type '{type_Bar}' is public and available.
                            "#
                            , keyword_fn = "fn".yellow()
                            , keyword_pub = "pub".bright_blue()
                            , keyword_type = "type".yellow()
                            , literal_foo = "\"foo\"".bright_purple()
                            , type_Bar = "Bar".green()
                            , variant_Bar = "Bar".green()
                        })
                    })
            ),

            Self::UnknownRecordField { label, fields, .. } => Some(fields
                .iter()
                .map(|s| (s, levenshtein::distance(label, s)))
                .min_by(|(_, a), (_, b)| a.cmp(b))
                .and_then(|(suggestion, distance)| {
                    if distance <= 4 {
                        Some(Box::new(format!("Did you mean '{}'?", suggestion.yellow())))
                    } else {
                        None
                    }
                })
                .unwrap_or_else(|| Box::new("Did you forget to make the field public?".to_string()))
            ),

            Self::UnknownType { name, types, .. } => Some(
                types
                    .iter()
                    .map(|s| (s, levenshtein::distance(name, s)))
                    .min_by(|(_, a), (_, b)| a.cmp(b))
                    .and_then(|(suggestion, distance)| {
                        if distance <= 4 {
                            Some(Box::new(format!("Did you mean '{}'?", suggestion.yellow())))
                        } else {
                            None
                        }
                    })
                    .unwrap_or_else(|| Box::new("Did you forget to import it?".to_string())),
            ),

            Self::UnknownVariable {
                name, variables, ..
            } => {
                let hint = variables
                    .iter()
                    .map(|s| (s, levenshtein::distance(name, s)))
                    .min_by(|(_, a), (_, b)| a.cmp(b))
                    .and_then(|(suggestion, distance)| {
                        if distance <= 4 {
                            Some(Box::new(format!("Did you mean '{}'?", suggestion.yellow())))
                        } else {
                            None
                        }
                    });

                if name.chars().into_iter().next().unwrap().is_uppercase() {
                    Some(hint.unwrap_or_else(||
                        Box::new(formatdoc! {
                            r#"Did you forget to import it?

                               Data-type constructors are not automatically imported, even if their type is imported. So, if a module 'aiken/pet' defines the following type:

                                 ┍━ aiken/pet.ak ━━━━━━━━
                                 │ {keyword_pub} {keyword_type} {type_Pet} {{
                                 │   {variant_Cat}
                                 │   {variant_Dog}
                                 │ }}

                               You must import its constructors explicitly to use them, or prefix them with the module's name.

                                 ┍━ foo.ak ━━━━━━━━
                                 │ {keyword_use} aiken/pet.{{{type_Pet}, {variant_Dog}}}
                                 │
                                 │ {keyword_fn} foo(pet : {type_Pet}) {{
                                 │   {keyword_when} pet {keyword_is} {{
                                 │     pet.{variant_Cat} -> // ...
                                 │     {variant_Dog} -> // ...
                                 │   }}
                                 │ }}
                            "#
                            , keyword_fn =  "fn".yellow()
                            , keyword_is = "is".yellow()
                            , keyword_pub = "pub".bright_blue()
                            , keyword_type = "type".yellow()
                            , keyword_use = "use".bright_blue()
                            , keyword_when = "when".yellow()
                            , type_Pet = "Pet".green()
                            , variant_Cat = "Cat".green()
                            , variant_Dog = "Dog".green()
                        })
                    ))
                } else {
                    Some(hint.unwrap_or_else(|| Box::new("Did you forget to import it?".to_string())))
                }
            }

            Self::UnknownTypeConstructor { name, constructors, .. } => Some(
                constructors
                    .iter()
                    .map(|s| (s, levenshtein::distance(name, s)))
                    .min_by(|(_, a), (_, b)| a.cmp(b))
                    .and_then(|(suggestion, distance)| {
                        if distance <= 4 {
                            Some(Box::new(format!("Did you mean '{}'?", suggestion.yellow())))
                        } else {
                            None
                        }
                    })
                    .unwrap_or_else(|| Box::new("Did you forget to import it?".to_string()))
            ),

            Self::UnnecessarySpreadOperator { arity, .. } => Some(Box::new(formatdoc! {
                r#"The spread operator comes in handy when matching on some fields of a constructor. However, here you've matched all {arity} fields of the constructor which makes the spread operator redundant.

                   The best thing to do from here is to remove it.
                "# })
            ),

            // TODO: Come and refine this once we support record updates.
            Self::UpdateMultiConstructorType {..} => None,

            Self::CouldNotUnify { expected, given, situation, rigid_type_names, .. } => {
                let expected = expected.to_pretty_with_names(rigid_type_names.clone(), 4);
                let given = given.to_pretty_with_names(rigid_type_names.clone(), 4);
                Some(Box::new(
                    match situation {
                        Some(UnifyErrorSituation::CaseClauseMismatch) => formatdoc! {
                            r#"While comparing branches from a '{keyword_when}/{keyword_is}' expression, I realized not all branches have the same type.

                               I am expecting all of them to have the following type:

                               {expected}

                               but I found some with type:

                               {given}

                               Note that I infer the type of the entire '{keyword_when}/{keyword_is}' expression based on the type of the first branch I encounter.
                             "#
                             , keyword_when = "when".yellow()
                             , keyword_is = "is".yellow()
                             , expected = expected.green()
                             , given = given.red()
                        },
                        Some(UnifyErrorSituation::ReturnAnnotationMismatch) => formatdoc! {
                            r#"While comparing the return annotation of a function with its actual return type, I realized that both don't match.

                               I am inferring the function should return:

                               {}

                               but I found that it returns:

                               {}

                               Either, fix the annotation or adjust the function body to return the expected type.
                             "#,
                             expected.green(),
                             given.red()
                        },
                        Some(UnifyErrorSituation::PipeTypeMismatch) => formatdoc! {
                            r#"As I was looking at a pipeline you have defined, I realized that one of the pipe isn't valid.

                               I am expecting the pipe to send into something of type:

                               {}

                               but it is typed:

                               {}

                               Either, fix the input or change the target so that both match.
                            "#,
                            expected.green(),
                            given.red()
                        },
                        Some(UnifyErrorSituation::Operator(op)) => formatdoc! {
                            r#"While checking operands of a binary operator, I realized that at least one of them doesn't have the expected type.

                               The '{}' operator expects operands of type:

                               {}

                               but I discovered the following instead:

                               {}
                            "#,
                            op.to_doc().to_pretty_string(70).yellow(),
                            expected.green(),
                            given.red()
                        },
                        None => formatdoc! {
                            r#"I am inferring the following type:

                               {}

                               but I found an expression with a different type:

                               {}

                               Either, add type-annotation to improve my inference, or adjust the expression to have the expected type.
                            "#,
                            expected.green(),
                            given.red()
                        },
                    }
                ))
            },

            Self::ExtraVarInAlternativePattern { .. } => None,

            Self::MissingVarInAlternativePattern { .. } => None,

            Self::DuplicateVarInPattern { .. } => None,

            Self::CyclicTypeDefinitions { .. } => None,

            Self::RecursiveType { .. } => Some(Box::new(formatdoc! {
                r#"I have several aptitudes, but inferring recursive types isn't one them. It is still possible to define recursive types just fine, but I will need a little help in the form of type annotation to infer their types should they show up.
                "#
            })),

            Self::NotATuple { tipo, .. } => Some(Box::new(formatdoc! {
                r#"Because you used a tuple-index on an element, I assumed it had to be a tuple or some kind, but instead I found:

                   {type_info}
                "#,
                type_info = tipo.to_pretty(4)
            })),

            Self::TupleIndexOutOfBound { .. } => None,
        }
    }

    // Labels to apply to this Diagnostic’s Diagnostic::source_code
    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        match self {
            Self::DuplicateArgument { locations, .. } => Some(Box::new(
                locations
                    .iter()
                    .map(|l| LabeledSpan::new_with_span(None, *l)),
            )),
            Self::DuplicateConstName {
                location,
                previous_location,
                ..
            } => Some(Box::new(
                vec![
                    LabeledSpan::new_with_span(None, *location),
                    LabeledSpan::new_with_span(None, *previous_location),
                ]
                .into_iter(),
            )),
            Self::DuplicateImport {
                location,
                previous_location,
                ..
            } => Some(Box::new(
                vec![
                    LabeledSpan::new_with_span(None, *location),
                    LabeledSpan::new_with_span(None, *previous_location),
                ]
                .into_iter(),
            )),
            Self::DuplicateField { locations, .. } => Some(Box::new(
                locations
                    .iter()
                    .map(|l| LabeledSpan::new_with_span(None, *l)),
            )),
            Self::DuplicateName {
                location,
                previous_location,
                ..
            } => Some(Box::new(
                vec![
                    LabeledSpan::new_with_span(None, *location),
                    LabeledSpan::new_with_span(None, *previous_location),
                ]
                .into_iter(),
            )),
            Self::DuplicateTypeName { location, .. } => Some(Box::new(
                vec![LabeledSpan::new_with_span(None, *location)].into_iter(),
            )),
            Self::IncorrectFieldsArity { location, .. } => Some(Box::new(
                vec![LabeledSpan::new_with_span(None, *location)].into_iter(),
            )),
            Self::IncorrectFunctionCallArity { location, .. } => Some(Box::new(
                vec![LabeledSpan::new_with_span(None, *location)].into_iter(),
            )),
            Self::IncorrectPatternArity { location, .. } => Some(Box::new(
                vec![LabeledSpan::new_with_span(None, *location)].into_iter(),
            )),
            Self::IncorrectNumClausePatterns { location, .. } => Some(Box::new(
                vec![LabeledSpan::new_with_span(None, *location)].into_iter(),
            )),
            Self::IncorrectTupleArity { location, .. } => Some(Box::new(
                vec![LabeledSpan::new_with_span(None, *location)].into_iter(),
            )),
            Self::IncorrectTypeArity { location, .. } => Some(Box::new(
                vec![LabeledSpan::new_with_span(None, *location)].into_iter(),
            )),
            Self::NotExhaustivePatternMatch { location, .. } => Some(Box::new(
                vec![LabeledSpan::new_with_span(None, *location)].into_iter(),
            )),
            Self::NotFn { location, .. } => Some(Box::new(
                vec![LabeledSpan::new_with_span(None, *location)].into_iter(),
            )),
            Self::KeywordInModuleName { .. } => None,
            Self::NonLocalClauseGuardVariable { location, .. } => Some(Box::new(
                vec![LabeledSpan::new_with_span(None, *location)].into_iter(),
            )),
            Self::PositionalArgumentAfterLabeled {
                location,
                labeled_arg_location,
                ..
            } => Some(Box::new(
                vec![
                    LabeledSpan::new_with_span(Some("positional".to_string()), *location),
                    LabeledSpan::new_with_span(Some("labeled".to_string()), *labeled_arg_location),
                ]
                .into_iter(),
            )),
            Self::PrivateTypeLeak { location, .. } => Some(Box::new(
                vec![LabeledSpan::new_with_span(None, *location)].into_iter(),
            )),
            Self::RecordAccessUnknownType { location, .. } => Some(Box::new(
                vec![LabeledSpan::new_with_span(None, *location)].into_iter(),
            )),
            Self::RecordUpdateInvalidConstructor { location, .. } => Some(Box::new(
                vec![LabeledSpan::new_with_span(None, *location)].into_iter(),
            )),
            Self::ReservedModuleName { .. } => None,
            Self::UnexpectedLabeledArg { location, .. } => Some(Box::new(
                vec![LabeledSpan::new_with_span(None, *location)].into_iter(),
            )),
            Self::UnexpectedLabeledArgInPattern { location, .. } => Some(Box::new(
                vec![LabeledSpan::new_with_span(None, *location)].into_iter(),
            )),
            Self::UnexpectedTypeHole { location, .. } => Some(Box::new(
                vec![LabeledSpan::new_with_span(None, *location)].into_iter(),
            )),
            Self::UnknownLabels { unknown, .. } => {
                Some(Box::new(unknown.iter().map(|(lbl, span)| {
                    LabeledSpan::new_with_span(Some(lbl.clone()), *span)
                })))
            }
            Self::UnknownModule { location, .. } => Some(Box::new(
                vec![LabeledSpan::new_with_span(None, *location)].into_iter(),
            )),
            Self::UnknownModuleField { location, .. } => Some(Box::new(
                vec![LabeledSpan::new_with_span(None, *location)].into_iter(),
            )),
            Self::UnknownModuleValue { location, .. } => Some(Box::new(
                vec![LabeledSpan::new_with_span(None, *location)].into_iter(),
            )),
            Self::UnknownModuleType { location, .. } => Some(Box::new(
                vec![LabeledSpan::new_with_span(None, *location)].into_iter(),
            )),
            Self::UnknownRecordField { location, .. } => Some(Box::new(
                vec![LabeledSpan::new_with_span(None, *location)].into_iter(),
            )),
            Self::UnknownType { location, .. } => Some(Box::new(
                vec![LabeledSpan::new_with_span(None, *location)].into_iter(),
            )),
            Self::UnknownVariable { location, .. } => Some(Box::new(
                vec![LabeledSpan::new_with_span(None, *location)].into_iter(),
            )),
            Self::UnknownTypeConstructor { location, .. } => Some(Box::new(
                vec![LabeledSpan::new_with_span(None, *location)].into_iter(),
            )),
            Self::UnnecessarySpreadOperator { location, .. } => Some(Box::new(
                vec![LabeledSpan::new_with_span(None, *location)].into_iter(),
            )),
            Self::UpdateMultiConstructorType { location, .. } => Some(Box::new(
                vec![LabeledSpan::new_with_span(None, *location)].into_iter(),
            )),
            Self::CouldNotUnify { location, .. } => Some(Box::new(
                vec![LabeledSpan::new_with_span(None, *location)].into_iter(),
            )),
            Self::ExtraVarInAlternativePattern { location, .. } => Some(Box::new(
                vec![LabeledSpan::new_with_span(None, *location)].into_iter(),
            )),
            Self::MissingVarInAlternativePattern { location, .. } => Some(Box::new(
                vec![LabeledSpan::new_with_span(None, *location)].into_iter(),
            )),
            Self::DuplicateVarInPattern { location, .. } => Some(Box::new(
                vec![LabeledSpan::new_with_span(None, *location)].into_iter(),
            )),
            Self::CyclicTypeDefinitions { location, .. } => Some(Box::new(
                vec![LabeledSpan::new_with_span(None, *location)].into_iter(),
            )),
            Self::RecursiveType { location, .. } => Some(Box::new(
                vec![LabeledSpan::new_with_span(None, *location)].into_iter(),
            )),
            Self::NotATuple { location, .. } => Some(Box::new(
                vec![LabeledSpan::new_with_span(None, *location)].into_iter(),
            )),
            Self::TupleIndexOutOfBound { location, .. } => Some(Box::new(
                vec![LabeledSpan::new_with_span(None, *location)].into_iter(),
            )),
        }
    }

    fn url<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        match self {
            Self::DuplicateArgument { .. } => None,
            Self::DuplicateConstName { .. } => None,
            Self::DuplicateImport { .. } => None,
            Self::DuplicateField { .. } => None,
            Self::DuplicateName { .. } => None,
            Self::DuplicateTypeName { .. } => None,
            Self::IncorrectFieldsArity { .. } => Some(Box::new(
                "https://aiken-lang.org/language-tour/custom-types",
            )),
            Self::IncorrectFunctionCallArity { .. } => Some(Box::new(
                "https://aiken-lang.org/language-tour/functions#named-functions",
            )),
            Self::IncorrectPatternArity { .. } => Some(Box::new(
                "https://aiken-lang.org/language-tour/control-flow#matching",
            )),
            Self::IncorrectNumClausePatterns { .. } => None,
            Self::IncorrectTupleArity { .. } => Some(Box::new(
                "https://aiken-lang.org/language-tour/control-flow#destructuring",
            )),
            Self::IncorrectTypeArity { .. } => Some(Box::new(
                "https://aiken-lang.org/language-tour/custom-types#generics",
            )),
            Self::NotExhaustivePatternMatch { .. } => Some(Box::new(
                "https://aiken-lang.org/language-tour/control-flow#matching",
            )),
            Self::NotFn { .. } => None,
            Self::KeywordInModuleName { .. } => Some(Box::new(
                "https://aiken-lang.org/language-tour/modules"
            )),
            Self::NonLocalClauseGuardVariable { .. } => Some(Box::new(
                "https://aiken-lang.org/language-tour/control-flow#checking-equality-and-ordering-in-patterns"
            )),
            Self::PositionalArgumentAfterLabeled { .. } => Some(Box::new(
                "https://aiken-lang.org/language-tour/functions#labeled-arguments"
            )),
            Self::PrivateTypeLeak { .. } => Some(Box::new(
                "https://aiken-lang.org/language-tour/modules"
            )),
            Self::RecordAccessUnknownType { .. } => Some(Box::new(
                "https://aiken-lang.org/language-tour/variables-and-constants#type-annotations"
            )),
            Self::RecordUpdateInvalidConstructor { .. } => Some(Box::new(
                "https://aiken-lang.org/language-tour/custom-types#record-updates"
            )),
            Self::ReservedModuleName { .. } => None,
            Self::UnexpectedLabeledArg { .. } => Some(Box::new(
                "https://aiken-lang.org/language-tour/functions#labeled-arguments"
            )),
            Self::UnexpectedLabeledArgInPattern { .. } => Some(Box::new(
                "https://aiken-lang.org/language-tour/custom-types#named-accessors"
            )),
            Self::UnexpectedTypeHole { .. } => None,
            Self::UnknownLabels { .. } => None,
            Self::UnknownModule { .. } => None,
            Self::UnknownModuleField { .. } => None,
            Self::UnknownModuleValue { .. } => None,
            Self::UnknownModuleType { .. } => None,
            Self::UnknownRecordField { .. } => None,
            Self::UnknownType { .. } => None,
            Self::UnknownVariable { .. } => None,
            Self::UnknownTypeConstructor { .. } => None,
            Self::UnnecessarySpreadOperator { .. } => Some(Box::new(
                "https://aiken-lang.org/language-tour/control-flow#destructuring"
            )),
            Self::UpdateMultiConstructorType { .. } => Some(Box::new(
                "https://aiken-lang.org/language-tour/custom-types#record-updates"
            )),
            Self::CouldNotUnify { .. } => Some(Box::new(
                "https://aiken-lang.org/language-tour/primitive-types"
            )),
            Self::ExtraVarInAlternativePattern { .. } => Some(Box::new(
                "https://aiken-lang.org/language-tour/control-flow#alternative-clause-patterns"
            )),
            Self::MissingVarInAlternativePattern { .. } => Some(Box::new(
                "https://aiken-lang.org/language-tour/control-flow#alternative-clause-patterns"
            )),
            Self::DuplicateVarInPattern { .. } => Some(Box::new(
                "https://aiken-lang.org/language-tour/control-flow#alternative-clause-patterns"
            )),
            Self::CyclicTypeDefinitions { .. } => Some(Box::new(
                "https://aiken-lang.org/language-tour/custom-types#type-aliases"
            )),
            Self::RecursiveType { .. } => Some(Box::new(
                "https://aiken-lang.org/language-tour/variables-and-constants#type-annotations"
            )),
            Self::NotATuple { .. } => Some(Box::new(
                "https://aiken-lang.org/language-tour/primitive-types#tuples"
            )),
            Self::TupleIndexOutOfBound { .. } => Some(Box::new(
                "https://aiken-lang.org/language-tour/primitive-types#tuples"
            )),
        }
    }
}

#[derive(Debug, PartialEq, Clone, thiserror::Error, Diagnostic)]
pub enum Warning {
    #[error("I found a todo left in the code.\n")]
    #[diagnostic(help("You probably want to replace that one with real code... eventually."))]
    Todo {
        kind: TodoKind,
        #[label]
        location: Span,
        tipo: Arc<Type>,
    },

    #[error(
        "I realized the following expression returned a result that is implicitly discarded.\n"
    )]
    #[diagnostic(help(
        "You can use the '_' symbol should you want to explicitly discard a result."
    ))]
    ImplicitlyDiscardedResult {
        #[label]
        location: Span,
    },

    #[error("I found a literal that is unused.\n")]
    UnusedLiteral {
        #[label]
        location: Span,
    },

    #[error("I found a record update with no fields; effectively updating nothing.\n")]
    #[diagnostic(url("https://aiken-lang.org/language-tour/custom-types#record-updates"))]
    NoFieldsRecordUpdate {
        #[label]
        location: Span,
    },

    #[error("I found a record update using all fields; thus redundant.\n")]
    #[diagnostic(url("https://aiken-lang.org/language-tour/custom-types#record-updates"))]
    AllFieldsRecordUpdate {
        #[label]
        location: Span,
    },

    #[error("I discovered an unused type: '{}'.\n", name.purple())]
    UnusedType {
        #[label]
        location: Span,
        imported: bool,
        name: String,
    },

    #[error("I discovered an unused constructor: '{}'.\n", name.purple())]
    #[diagnostic(help(
        "No big deal, but you might want to remove it to get rid of that warning."
    ))]
    UnusedConstructor {
        #[label]
        location: Span,
        imported: bool,
        name: String,
    },

    #[error("I discovered an unused imported value: '{}'.\n", name.purple())]
    #[diagnostic(help(
        "No big deal, but you might want to remove it to get rid of that warning."
    ))]
    UnusedImportedValue {
        #[label]
        location: Span,
        name: String,
    },

    #[error("I discovered an unused imported module: '{}'.\n", name.purple())]
    #[diagnostic(help(
        "No big deal, but you might want to remove it to get rid of that warning."
    ))]
    UnusedImportedModule {
        #[label]
        location: Span,
        name: String,
    },

    #[error("I found an unused (private) module constant: '{}'.\n", name.purple())]
    #[diagnostic(help(
        "Perhaps your forgot to make it public using the '{keyword_pub}' keyword?\n\
         Otherwise, you might want to get rid of it altogether."
        , keyword_pub = "pub".bright_blue()
    ))]
    UnusedPrivateModuleConstant {
        #[label]
        location: Span,
        name: String,
    },

    #[error("I found an unused private function: '{}'.\n", name.purple())]
    #[diagnostic(help(
        "Perhaps your forgot to make it public using the '{keyword_pub}' keyword?\n\
         Otherwise, you might want to get rid of it altogether."
         , keyword_pub = "pub".bright_blue()
    ))]
    UnusedPrivateFunction {
        #[label]
        location: Span,
        name: String,
    },

    #[error("I came across an unused variable: '{}'.\n", name.purple())]
    #[diagnostic(help(
        "No big deal, but you might want to remove it to get rid of that warning."
    ))]
    UnusedVariable {
        #[label]
        location: Span,
        name: String,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnifyErrorSituation {
    /// Clauses in a case expression were found to return different types.
    CaseClauseMismatch,

    /// A function was found to return a value that did not match its return
    /// annotation.
    ReturnAnnotationMismatch,

    PipeTypeMismatch,

    /// The operands of a binary operator were incorrect.
    Operator(BinOp),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnknownRecordFieldSituation {
    /// This unknown record field is being called as a function. i.e. `record.field()`
    FunctionCall,
}
