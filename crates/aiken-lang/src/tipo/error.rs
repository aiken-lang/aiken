use super::Type;
use crate::{
    ast::{Annotation, BinOp, CallArg, Span, UntypedPattern},
    expr::{self, UntypedExpr},
    format::Formatter,
    levenshtein,
    pretty::Documentable,
};
use indoc::formatdoc;
use miette::{Diagnostic, LabeledSpan};
use ordinal::Ordinal;
use owo_colors::OwoColorize;
use std::{collections::HashMap, fmt::Display, sync::Arc};

#[derive(Debug, thiserror::Error, Diagnostic)]
#[error("Something is possibly wrong here...")]
pub struct Snippet {
    #[label]
    pub location: Span,
}

#[derive(Debug, thiserror::Error)]
#[error(
    "I don't know some of the labels used in this expression. I've highlighted them just below."
)]
pub struct UnknownLabels {
    pub unknown: Vec<Span>,
    pub valid: Vec<String>,
    pub supplied: Vec<String>,
}

impl Diagnostic for UnknownLabels {
    fn help<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        Some(Box::new(formatdoc! {
            r#"Here's a list of all the (valid) labels that I know of:

               {known_labels}"#
            , known_labels = self.valid
                .iter()
                .map(|s| format!("─▶ {}", s.yellow()))
                .collect::<Vec<_>>()
                .join("\n")
        }))
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        Some(Box::new(self.unknown.iter().map(|l| {
            LabeledSpan::new_with_span(Some("?".to_string()), *l)
        })))
    }
}

#[derive(Debug, thiserror::Error, Diagnostic)]
pub enum Error {
    #[error("I discovered a type cast from Data without an annotation")]
    #[diagnostic(code("illegal::type_cast"))]
    #[diagnostic(help("Try adding an annotation...\n\n{}", format_suggestion(value)))]
    CastDataNoAnn {
        #[label("missing annotation")]
        location: Span,
        value: UntypedExpr,
    },

    #[error("I struggled to unify the types of two expressions.\n")]
    #[diagnostic(url("https://aiken-lang.org/language-tour/primitive-types"))]
    #[diagnostic(code("type_mismatch"))]
    #[diagnostic(help("{}", suggest_unify(expected, given, situation, rigid_type_names)))]
    CouldNotUnify {
        #[label(
            "expected type '{}'",
            expected.to_pretty_with_names(rigid_type_names.clone(), 0),
        )]
        location: Span,
        expected: Arc<Type>,
        given: Arc<Type>,
        situation: Option<UnifyErrorSituation>,
        rigid_type_names: HashMap<u64, String>,
    },

    #[error("I almost got caught in an infinite cycle of type definitions.\n")]
    #[diagnostic(url("https://aiken-lang.org/language-tour/custom-types#type-aliases"))]
    #[diagnostic(code("cycle"))]
    CyclicTypeDefinitions {
        #[related]
        errors: Vec<Snippet>,
    },

    #[error("I found two function arguments both called '{}'.\n", label.purple())]
    #[diagnostic(code("duplicate::argument"))]
    #[diagnostic(help("Function arguments cannot have the same name. You can use '{discard}' and numbers to distinguish between similar names."
        , discard = "_".yellow()
    ))]
    DuplicateArgument {
        #[label]
        location: Span,
        #[label]
        duplicate_location: Span,
        label: String,
    },

    #[error("I found two declarations for the constant '{}'.\n", name.purple())]
    #[diagnostic(code("duplicate::constant"))]
    #[diagnostic(help("Top-level constants of a same module cannot have the same name. You can use '{discard}' and numbers to distinguish between similar names."
        , discard = "_".yellow()
    ))]
    DuplicateConstName {
        #[label("declared again here")]
        location: Span,
        #[label("declared here")]
        previous_location: Span,
        name: String,
    },

    #[error("I stumbled upon the field '{}' twice in a data-type definition.\n", label.purple())]
    #[diagnostic(code("duplicate::field"))]
    #[diagnostic(help(r#"Data-types must have fields with strictly different names. You can use '{discard}' and numbers to distinguish between similar names.
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
    ))]
    DuplicateField {
        #[label]
        location: Span,
        #[label]
        duplicate_location: Span,
        label: String,
    },

    #[error("I noticed you were importing '{}' twice.\n", name.purple())]
    #[diagnostic(code("duplicate::import"))]
    #[diagnostic(help(r#"If you're trying to import two modules with identical names but from different packages, you'll need to use a named import.
For example:

╰─▶ {keyword_use} {import} {keyword_as} {named}

Otherwise, just remove the redundant import."#
        , keyword_use = "use".bright_blue()
        , keyword_as = "as".bright_blue()
        , import = module.iter().map(|x| x.purple().bold().to_string()).collect::<Vec<_>>().join("/".bold().to_string().as_ref())
        , named = module.join("_")
    ))]
    DuplicateImport {
        #[label("also imported here as '{name}'")]
        location: Span,
        name: String,
        module: Vec<String>,
        #[label("imported here as '{name}'")]
        previous_location: Span,
    },

    #[error("I discovered two top-level objects referred to as '{}'.\n", name.purple())]
    #[diagnostic(code("duplicate::name"))]
    #[diagnostic(help(r#"Top-level definitions cannot have the same name, even if they refer to objects with different natures (e.g. function and test).

You can use '{discard}' and numbers to distinguish between similar names.
"#
        , discard = "_".yellow()
    ))]
    DuplicateName {
        #[label]
        location: Span,
        #[label]
        previous_location: Span,
        name: String,
    },

    #[error("I found two types declared with the same name: '{}'.\n", name.purple())]
    #[diagnostic(code("duplicate::type"))]
    #[diagnostic(help("Types cannot have the same top-level name. You {cannot} use '_' in types name, but you can use numbers to distinguish between similar names."
        , cannot = "cannot".red()
    ))]
    DuplicateTypeName {
        #[label]
        location: Span,
        #[label]
        previous_location: Span,
        name: String,
    },

    #[error("I realized the variable '{}' was mentioned more than once in an alternative pattern.\n", name.purple())]
    #[diagnostic(url(
        "https://aiken-lang.org/language-tour/control-flow#alternative-clause-patterns"
    ))]
    #[diagnostic(code("duplicate::pattern"))]
    DuplicateVarInPattern {
        #[label]
        location: Span,
        name: String,
    },

    #[error("I tripped over an extra variable in an alternative pattern: {}.\n", name.purple())]
    #[diagnostic(url(
        "https://aiken-lang.org/language-tour/control-flow#alternative-clause-patterns"
    ))]
    #[diagnostic(code("unexpected::variable"))]
    ExtraVarInAlternativePattern {
        #[label]
        location: Span,
        name: String,
    },

    #[error("I found a data type that has a function type in it. This is not allowed.\n")]
    #[diagnostic(code("illegal::function_in_type"))]
    #[diagnostic(help("Data-types can't hold functions. If you want to define method-like functions, group the type definition and the methods under a common namespace in a standalone module."))]
    FunctionTypeInData {
        #[label]
        location: Span,
    },

    #[error("I found a discarded expression not bound to a variable.\n")]
    #[diagnostic(code("implicit_discard"))]
    #[diagnostic(help("A function can contain a sequence of expressions. However, any expression but the last one must be assign to a variable using the {keyword_let} keyword. If you really wish to discard an expression that is unused, you can assign it to '{discard}'."
        , keyword_let = "let".yellow()
        , discard = "_".yellow()
    ))]
    ImplicitlyDiscardedExpression {
        #[label]
        location: Span,
    },

    #[error("I saw a {} fields in a context where there should be {}.\n", given.purple(), expected.purple())]
    #[diagnostic(url("https://aiken-lang.org/language-tour/custom-types"))]
    #[diagnostic(code("arity::constructor"))]
    IncorrectFieldsArity {
        #[label]
        location: Span,
        expected: usize,
        given: usize,
        labels: Vec<String>,
    },

    #[error("I saw a function or constructor that expects {} arguments be called with {} arguments.\n", expected.purple(), given.purple())]
    #[diagnostic(url("https://aiken-lang.org/language-tour/functions#named-functions"))]
    #[diagnostic(code("arity::invoke"))]
    #[diagnostic(help(r#"Functions (and constructors) must always be called with all their arguments (comma-separated, between brackets).

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
    ))]
    IncorrectFunctionCallArity {
        #[label]
        location: Span,
        expected: usize,
        given: usize,
    },

    // TODO: Since we do not actually support patterns on multiple items, we won't likely ever
    // encounter that error. We could simplify a bit the type-checker and get rid of that error
    // eventually.
    #[error("I counted {} different clauses in a multi-pattern instead of {}.\n", given.purple(), expected.purple())]
    #[diagnostic(code("arity::clause"))]
    IncorrectNumClausePatterns {
        #[label]
        location: Span,
        expected: usize,
        given: usize,
    },

    #[error("I saw a pattern on a constructor that has {} field(s) be matched with {} argument(s).\n", expected.purple(), given.len().purple())]
    #[diagnostic(url("https://aiken-lang.org/language-tour/control-flow#matching"))]
    #[diagnostic(code("arity::pattern"))]
    #[diagnostic(help("When pattern-matching on constructors, you must either match the exact number of fields, or use the spread operator '{spread}'. Note that unused fields must be discarded by prefixing their name with '{discard}'."
        , discard = "_".yellow()
        , spread = "..".yellow()
    ))]
    IncorrectPatternArity {
        #[label("{}", suggest_pattern(*expected, name, given, module, *is_record).unwrap_or_default())]
        location: Span,
        expected: usize,
        given: Vec<CallArg<UntypedPattern>>,
        name: String,
        module: Option<String>,
        is_record: bool,
    },

    #[error("I saw a pattern on a {}-tuple be matched into a {}-tuple.\n", expected.purple(), given.purple())]
    #[diagnostic(url("https://aiken-lang.org/language-tour/control-flow#destructuring"))]
    #[diagnostic(code("arity::tuple"))]
    #[diagnostic(help("When pattern matching on a tuple, you must match all of its elements. Note that unused fields must be discarded by prefixing their name with '{discard}'."
        , discard = "_".yellow()
    ))]
    IncorrectTupleArity {
        #[label]
        location: Span,
        expected: usize,
        given: usize,
    },

    #[error("I noticed a generic data-type with {} type parameters instead of {}.\n", given.purple(), expected.purple())]
    #[diagnostic(url("https://aiken-lang.org/language-tour/custom-types#generics"))]
    #[diagnostic(code("arity::generic"))]
    #[diagnostic(help(r#"Data-types that are generic in one or more types must be written with all their generic types in type annotations. Generic types must be indicated between chevrons '{chevron_left}' and '{chevron_right}'.

Perhaps, try the following:

╰─▶  {suggestion}"#
        , chevron_left = "<".yellow()
        , chevron_right = ">".yellow()
        , suggestion = suggest_generic(name, *expected)
    ))]
    IncorrectTypeArity {
        #[label]
        location: Span,
        name: String,
        expected: usize,
        given: usize,
    },

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

    #[error("I discovered a function which is ending with an assignment.\n")]
    #[diagnostic(url("https://aiken-lang.org/language-tour/functions#named-functions"))]
    #[diagnostic(code("illegal::return"))]
    #[diagnostic(help(r#"In Aiken, functions must return an explicit result in the form of an expression. While assignments are technically speaking expressions, they aren't allowed to be the last expression of a function because they convey a different meaning and this could be error-prone.

If you really meant to return that last expression, try to replace it with the following:

{sample}"#
        , sample = format_suggestion(expr)
    ))]
    LastExpressionIsAssignment {
        #[label("let-binding as last expression")]
        location: Span,
        expr: expr::UntypedExpr,
    },

    #[error("I found a missing variable in an alternative pattern: {}.\n", name.purple())]
    #[diagnostic(url(
        "https://aiken-lang.org/language-tour/control-flow#alternative-clause-patterns"
    ))]
    #[diagnostic(code("missing::variable"))]
    MissingVarInAlternativePattern {
        #[label]
        location: Span,
        name: String,
    },

    #[error("I stumbled upon an invalid (non-local) clause guard '{}'.\n", name.purple())]
    #[diagnostic(url("https://aiken-lang.org/language-tour/control-flow#checking-equality-and-ordering-in-patterns"))]
    #[diagnostic(code("illegal::clause_guard"))]
    #[diagnostic(help("There are some conditions regarding what can be used in a guard. Values must be either local to the function, or defined as module constants. You can't use functions or records in there."))]
    NonLocalClauseGuardVariable {
        #[label]
        location: Span,
        name: String,
    },

    #[error(
        "I tripped over an attempt to access tuple elements on something else than a tuple.\n"
    )]
    #[diagnostic(url("https://aiken-lang.org/language-tour/primitive-types#tuples"))]
    #[diagnostic(code("illegal::tuple_index"))]
    #[diagnostic(help(r#"Because you used a tuple-index on an element, I assumed it had to be a tuple or some kind, but instead I found:

╰─▶ {type_info}"#
        , type_info = tipo.to_pretty(4).red()
    ))]
    NotATuple {
        #[label]
        location: Span,
        tipo: Arc<Type>,
    },

    #[error("{}\n", if *is_let {
          "I noticed a let assignment matching a value with more than one constructor.".to_string()
      } else {
          format!(
              "I realized that a given '{keyword_when}/{keyword_is}' expression is non-exhaustive.",
              keyword_is = "is".purple(),
              keyword_when = "when".purple()
          )
      }
    )]
    #[diagnostic(url("https://aiken-lang.org/language-tour/control-flow#matching"))]
    #[diagnostic(code("non_exhaustive_pattern_match"))]
    #[diagnostic(help(r#"Let bindings and when clauses must be exhaustive -- that is, they must cover all possible cases of the type they match. In {keyword_when}/{keyword_is} pattern-match, it is recommended to have an explicit branch for each constructor as it prevents future silly mistakes when adding new constructors to a type. However, you can also use the wildcard '{discard}' as a last branch to match any remaining result.

In this particular instance, the following cases are unmatched:

{missing}"#
        , discard = "_".yellow()
        , keyword_is = "is".purple()
        , keyword_when = "when".purple()
        , missing = unmatched
            .iter()
            .map(|s| format!("─▶ {s}"))
            .collect::<Vec<_>>()
            .join("\n")
    ))]
    NotExhaustivePatternMatch {
        #[label("{}", if *is_let { "use when/is" } else { "non-exhaustive" })]
        location: Span,
        unmatched: Vec<String>,
        is_let: bool,
    },

    #[error("I tripped over a call attempt on something that isn't a function.\n")]
    #[diagnostic(code("illegal::invoke"))]
    #[diagnostic(help(r#"It seems like you're trying to call something that isn't a function. I am inferring the following type:

╰─▶  {inference}"#
        , inference = tipo.to_pretty(4)
    ))]
    NotFn {
        #[label]
        location: Span,
        tipo: Arc<Type>,
    },

    #[error("I discovered a positional argument after a label argument.\n")]
    #[diagnostic(url("https://aiken-lang.org/language-tour/functions#labeled-arguments"))]
    #[diagnostic(code("unexpected::positional_argument"))]
    #[diagnostic(help(r#"You can mix positional and labeled arguments, but you must put all positional arguments (i.e. without label) at the front.

To fix this, you'll need to either turn that argument as a labeled argument, or make the next one positional."#))]
    PositionalArgumentAfterLabeled {
        #[label]
        location: Span,
        labeled_arg_location: Span,
    },

    #[error("I caught a private value trying to escape.\n")]
    #[diagnostic(url("https://aiken-lang.org/language-tour/modules"))]
    #[diagnostic(code("private_leak"))]
    #[diagnostic(help(r#"I found a public value that is making use of a private type. This would prevent other modules from actually using that value because they wouldn't know what this type refer to.

The culprit is:

{type_info}

Maybe you meant to turn it public using the '{keyword_pub}' keyword?"#
        , type_info = leaked.to_pretty(4).red()
        , keyword_pub = "pub".bright_blue()
    ))]
    PrivateTypeLeak {
        #[label]
        location: Span,
        leaked: Type,
    },

    #[error("I couldn't figure out the type of a record you're trying to access.\n")]
    #[diagnostic(url(
        "https://aiken-lang.org/language-tour/variables-and-constants#type-annotations"
    ))]
    #[diagnostic(code("unknown::record_access"))]
    #[diagnostic(help(r#"I do my best to infer types of any expression; yet sometimes I need help (don't we all?).

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
    ))]
    RecordAccessUnknownType {
        #[label]
        location: Span,
    },

    #[error("I tripped over an invalid constructor in a record update.\n")]
    #[diagnostic(url("https://aiken-lang.org/language-tour/custom-types#record-updates"))]
    #[diagnostic(code("illegal::record_update"))]
    RecordUpdateInvalidConstructor {
        #[label]
        location: Span,
    },

    #[error("I almost got caught in an endless loop while inferring a recursive type.\n")]
    #[diagnostic(url("https://aiken-lang.org/language-tour/custom-types#type-annotations"))]
    #[diagnostic(code("missing::type_annotation"))]
    #[diagnostic(help("I have several aptitudes, but inferring recursive types isn't one them. It is still possible to define recursive types just fine, but I will need a little help in the form of type annotation to infer their types should they show up."))]
    RecursiveType {
        #[label]
        location: Span,
    },

    #[error("I realized you used '{}' as a module name, which is reserved (and not available).\n", name.purple())]
    #[diagnostic(code("illegal::module_name"))]
    #[diagnostic(help(r#"Some module names are reserved for internal use. This the case of:

- aiken: where the prelude is located;
- aiken/builtin: where I store low-level Plutus builtins.

Note that 'aiken' is also imported by default; but you can refer to it explicitly to disambiguate with a local value that would clash with one from that module."#
    ))]
    ReservedModuleName { name: String },

    #[error(
        "I discovered an attempt to access the {} element of a {}-tuple.\n",
        Ordinal(*index + 1).to_string().purple(),
        size.purple()
    )]
    #[diagnostic(url("https://aiken-lang.org/language-tour/primitive-types#tuples"))]
    #[diagnostic(code("invalid::tuple_index"))]
    TupleIndexOutOfBound {
        #[label]
        location: Span,
        index: usize,
        size: usize,
    },

    #[error("I tripped over the following labeled argument: {}.\n", label.purple())]
    #[diagnostic(url("https://aiken-lang.org/language-tour/functions#labeled-arguments"))]
    #[diagnostic(code("unexpected::module_name"))]
    UnexpectedLabeledArg {
        #[label]
        location: Span,
        label: String,
    },

    #[error("I tripped over the following labeled argument: {}.\n", label.purple())]
    #[diagnostic(url("https://aiken-lang.org/language-tour/custom-types#named-accessors"))]
    #[diagnostic(code("unexpected::labeled_argument"))]
    #[diagnostic(help(r#"The constructor '{constructor}' does not have any labeled field. Its fields must therefore be matched only by position.

Perhaps, try the following:

╰─▶  {suggestion}
"#
        , constructor = name.green()
        , suggestion = suggest_constructor_pattern(name, args, module, *with_spread)
    ))]
    UnexpectedLabeledArgInPattern {
        #[label]
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
    #[diagnostic(code("unexpected::type_hole"))]
    UnexpectedTypeHole {
        #[label]
        location: Span,
    },

    #[error("I tripped over some unknown labels in a pattern or function.\n")]
    #[diagnostic(code("unknown::labels"))]
    UnknownLabels(#[related] Vec<UnknownLabels>),

    #[error("I stumbled upon a reference to an unknown module: '{}'\n", name.purple())]
    #[diagnostic(code("unknown::module"))]
    #[diagnostic(help(
        "{}",
        suggest_neighbor(name, imported_modules.iter(), "Did you forget to add a package as dependency?")
    ))]
    UnknownModule {
        #[label]
        location: Span,
        name: String,
        imported_modules: Vec<String>,
    },

    #[error(
        "I found an unknown import '{}' from module '{}'.\n",
        name.purple(),
        module_name.purple()
    )]
    #[diagnostic(code("unknown::module_field"))]
    #[diagnostic(help(
        "{}",
        suggest_neighbor(
            name,
            value_constructors.iter().chain(type_constructors),
            &suggest_make_public()
        )
    ))]
    UnknownModuleField {
        #[label]
        location: Span,
        name: String,
        module_name: String,
        value_constructors: Vec<String>,
        type_constructors: Vec<String>,
    },

    #[error("I looked for '{}' in '{}' but couldn't find it.\n", name.purple(), module_name.purple())]
    #[diagnostic(code("unknown::module_type"))]
    #[diagnostic(help(
        "{}",
        suggest_neighbor(
            name,
            type_constructors.iter(),
            &suggest_make_public()
        )
    ))]
    UnknownModuleType {
        #[label]
        location: Span,
        name: String,
        module_name: String,
        type_constructors: Vec<String>,
    },

    #[error("I looked for '{}' in '{}' but couldn't find it.\n", name.purple(), module_name.purple())]
    #[diagnostic(code("unknown::module_value"))]
    #[diagnostic(help(
        "{}",
        suggest_neighbor(
            name,
            value_constructors.iter(),
            &suggest_make_public()
        )
    ))]
    UnknownModuleValue {
        #[label]
        location: Span,
        name: String,
        module_name: String,
        value_constructors: Vec<String>,
    },

    #[error(
      "I looked for the field '{}' in a record of type '{}' couldn't find it.\n",
      label.purple(),
      typ.to_pretty(4).purple()
    )]
    #[diagnostic(code("unknown::record_field"))]
    #[diagnostic(help(
        "{}",
        suggest_neighbor(label, fields.iter(), "Did you forget to make the it public?")
    ))]
    UnknownRecordField {
        #[label]
        location: Span,
        typ: Arc<Type>,
        label: String,
        fields: Vec<String>,
        situation: Option<UnknownRecordFieldSituation>,
    },

    #[error("I found a reference to an unknown type.\n")]
    #[diagnostic(code("unknown::type"))]
    #[diagnostic(help(
        "{}",
        suggest_neighbor(name, types.iter(), "Did you forget to import it?")
    ))]
    UnknownType {
        #[label("unknown type")]
        location: Span,
        name: String,
        types: Vec<String>,
    },

    #[error("I found a reference to an unknown data-type constructor: '{}'.\n", name.purple())]
    #[diagnostic(code("unknown::type_constructor"))]
    #[diagnostic(help(
        "{}",
        suggest_neighbor(name, constructors.iter(), "Did you forget to import it?")
    ))]
    UnknownTypeConstructor {
        #[label]
        location: Span,
        name: String,
        constructors: Vec<String>,
    },

    #[error("I found a reference to an unknown variable.\n")]
    #[diagnostic(code("unknown::variable"))]
    #[diagnostic(help(
        "{}",
        suggest_neighbor(
            name,
            variables.iter(),
            &if name.chars().into_iter().next().unwrap().is_uppercase() {
                suggest_import_constructor()
            } else {
                "Did you forget to import it?".to_string()
            }
        )
    ))]
    UnknownVariable {
        #[label("unknown variable")]
        location: Span,
        name: String,
        variables: Vec<String>,
    },

    #[error("I discovered a redundant spread operator.\n")]
    #[diagnostic(url("https://aiken-lang.org/language-tour/control-flow#destructuring"))]
    #[diagnostic(code("unexpected::spread_operator"))]
    #[diagnostic(help(r#"The spread operator comes in handy when matching on some fields of a constructor. However, here you've matched all {arity} fields of the constructor which makes the spread operator redundant.

The best thing to do from here is to remove it."#))]
    UnnecessarySpreadOperator {
        #[label]
        location: Span,
        arity: usize,
    },

    #[error("I tripped over a record-update on a data-type with more than one constructor.\n")]
    #[diagnostic(url("https://aiken-lang.org/language-tour/custom-types#record-updates"))]
    #[diagnostic(code("illegal::record_update"))]
    UpdateMultiConstructorType {
        #[label]
        location: Span,
    },

    #[error("I discovered an attempt to import a validator module: '{}'\n", name.purple())]
    #[diagnostic(code("illegal::import"))]
    #[diagnostic(help("If you are trying to share code defined in a validator then move it to a library module under {}", "lib/".purple()))]
    ValidatorImported {
        #[label]
        location: Span,
        name: String,
    },

    #[error("A validator must return {}", "Bool".bright_blue().bold())]
    #[diagnostic(code("illegal::validator_return_type"))]
    #[diagnostic(help(r#"While analyzing the return type of your validator, I found it to be:

╰─▶ {signature}

...but I expected this to be a {type_Bool}. If I am inferring the wrong type, you may want to add a type annotation to the function."#
        , type_Bool = "Bool".bright_blue().bold()
        , signature = return_type.to_pretty(0).red()
    ))]
    ValidatorMustReturnBool {
        #[label("invalid return type")]
        location: Span,
        return_type: Arc<Type>,
    },

    #[error("Validators requires at least {} arguments.", at_least.to_string().purple().bold())]
    #[diagnostic(code("illegal::validator_arity"))]
    IncorrectValidatorArity {
        at_least: u8,
        #[label("not enough arguments")]
        location: Span,
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

fn suggest_neighbor<'a>(
    name: &'a str,
    items: impl Iterator<Item = &'a String>,
    default: &'a str,
) -> String {
    items
        .map(|s| (s, levenshtein::distance(name, s)))
        .min_by(|(_, a), (_, b)| a.cmp(b))
        .and_then(|(suggestion, distance)| {
            if distance <= 4 {
                Some(format!("Did you mean '{}'?", suggestion.yellow()))
            } else {
                None
            }
        })
        .unwrap_or_else(|| default.to_string())
}

fn suggest_pattern(
    expected: usize,
    name: &str,
    given: &Vec<CallArg<UntypedPattern>>,
    module: &Option<String>,
    is_record: bool,
) -> Option<String> {
    if expected > given.len() {
        Some(format!(
            "Try instead: {}",
            Formatter::new()
                .pattern_constructor(name, given, module, true, is_record)
                .to_pretty_string(70),
        ))
    } else {
        None
    }
}

fn suggest_generic(name: &String, expected: usize) -> String {
    let mut args = vec![];
    for i in 0..expected {
        args.push(Annotation::Var {
            name: char::from_u32(97 + i as u32).unwrap_or('?').to_string(),
            location: Span::empty(),
        });
    }
    name.to_doc()
        .append(Formatter::new().type_arguments(&args))
        .to_pretty_string(70)
}

fn suggest_constructor_pattern(
    name: &str,
    args: &[CallArg<UntypedPattern>],
    module: &Option<String>,
    with_spread: bool,
) -> String {
    let fixed_args = args
        .iter()
        .map(|arg| CallArg {
            label: None,
            location: arg.location,
            value: arg.value.clone(),
        })
        .collect::<Vec<_>>();

    Formatter::new()
        .pattern_constructor(name, &fixed_args, module, with_spread, false)
        .to_pretty_string(70)
}

fn suggest_unify(
    expected: &Arc<Type>,
    given: &Arc<Type>,
    situation: &Option<UnifyErrorSituation>,
    rigid_type_names: &HashMap<u64, String>,
) -> String {
    let expected = expected.to_pretty_with_names(rigid_type_names.clone(), 4);
    let given = given.to_pretty_with_names(rigid_type_names.clone(), 4);
    match situation {
        Some(UnifyErrorSituation::CaseClauseMismatch) => formatdoc! {
            r#"While comparing branches from a '{keyword_when}/{keyword_is}' expression, I realized not all branches have the same type.

               I am expecting all of them to have the following type:

               {expected}

               but I found some with type:

               {given}

               Note that I infer the type of the entire '{keyword_when}/{keyword_is}' expression based on the type of the first branch I encounter."#,
            keyword_when = "when".yellow(),
            keyword_is = "is".yellow(),
            expected = expected.green(),
            given = given.red()
        },
        Some(UnifyErrorSituation::ReturnAnnotationMismatch) => formatdoc! {
            r#"While comparing the return annotation of a function with its actual return type, I realized that both don't match.

               I am inferring the function should return:

               {}

               but I found that it returns:

               {}

               Either, fix the annotation or adjust the function body to return the expected type."#,
            expected.green(),
            given.red()
        },
        Some(UnifyErrorSituation::PipeTypeMismatch) => formatdoc! {
            r#"As I was looking at a pipeline you have defined, I realized that one of the pipe isn't valid.

               I am expecting the pipe to send into something of type:

               {}

               but it is typed:

               {}

               Either, fix the input or change the target so that both match."#,
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

               Either, add type-annotation to improve my inference, or adjust the expression to have the expected type."#,
            expected.green(),
            given.red()
        },
    }
}

fn suggest_make_public() -> String {
    formatdoc! {
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
    }
}

fn suggest_import_constructor() -> String {
    formatdoc! {
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
    }
}

#[derive(Debug, PartialEq, Clone, thiserror::Error, Diagnostic)]
pub enum Warning {
    #[error("I found a record update using all fields; thus redundant.\n")]
    #[diagnostic(url("https://aiken-lang.org/language-tour/custom-types#record-updates"))]
    #[diagnostic(code("record_update::all_fields"))]
    AllFieldsRecordUpdate {
        #[label]
        location: Span,
    },

    #[error(
        "I realized the following expression returned a result that is implicitly discarded.\n"
    )]
    #[diagnostic(help(
        "You can use the '_' symbol should you want to explicitly discard a result."
    ))]
    #[diagnostic(code("implicit_discard"))]
    ImplicitlyDiscardedResult {
        #[label]
        location: Span,
    },

    #[error("I found a record update with no fields; effectively updating nothing.\n")]
    #[diagnostic(url("https://aiken-lang.org/language-tour/custom-types#record-updates"))]
    #[diagnostic(code("record_update::no_fields"))]
    NoFieldsRecordUpdate {
        #[label]
        location: Span,
    },

    #[error("I found a public definition in a validator module.\nDefinitions in validator modules do not need to be public.\n")]
    #[diagnostic(code("redundant::pub"))]
    PubInValidatorModule {
        #[label]
        location: Span,
    },

    #[error("I found a when expression with a single clause.")]
    #[diagnostic(
        code("single_when_clause"),
        help("Prefer using a {} binding like so...\n\n{}", "let".purple(), format_suggestion(sample))
    )]
    SingleWhenClause {
        #[label("use let")]
        location: Span,
        sample: UntypedExpr,
    },

    #[error("I found an {} trying to match a type with one constructor", "expect".purple())]
    #[diagnostic(
        code("single_constructor_expect"),
        help("If your type has one constructor, unless you are casting {} {}, you can\nprefer using a {} binding like so...\n\n{}", "FROM".bold(), "Data".purple(), "let".purple(), format_suggestion(sample))
    )]
    SingleConstructorExpect {
        #[label("use let")]
        location: Span,
        #[label("only one constructor")]
        pattern_location: Span,
        #[label("is not Data")]
        value_location: Span,
        sample: UntypedExpr,
    },

    #[error("I found a todo left in the code.\n")]
    #[diagnostic(help("You probably want to replace that one with real code... eventually."))]
    #[diagnostic(code("todo"))]
    Todo {
        #[label]
        location: Span,
        tipo: Arc<Type>,
    },

    #[error("I discovered an unused constructor: '{}'.\n", name.purple())]
    #[diagnostic(help(
        "No big deal, but you might want to remove it to get rid of that warning."
    ))]
    #[diagnostic(code("unused::constructor"))]
    UnusedConstructor {
        #[label]
        location: Span,
        imported: bool,
        name: String,
    },

    #[error("I discovered an unused imported module: '{}'.\n", name.purple())]
    #[diagnostic(help(
        "No big deal, but you might want to remove it to get rid of that warning."
    ))]
    #[diagnostic(code("unused::import::module"))]
    UnusedImportedModule {
        #[label]
        location: Span,
        name: String,
    },

    #[error("I discovered an unused imported value: '{}'.\n", name.purple())]
    #[diagnostic(help(
        "No big deal, but you might want to remove it to get rid of that warning."
    ))]
    #[diagnostic(code("unused:import::value"))]
    UnusedImportedValue {
        #[label]
        location: Span,
        name: String,
    },

    #[error("I found a literal that is unused.\n")]
    #[diagnostic(code("unused::literal"))]
    UnusedLiteral {
        #[label]
        location: Span,
    },

    #[error("I found an unused private function: '{}'.\n", name.purple())]
    #[diagnostic(help(
        "Perhaps your forgot to make it public using the '{keyword_pub}' keyword?\n\
         Otherwise, you might want to get rid of it altogether."
         , keyword_pub = "pub".bright_blue()
    ))]
    #[diagnostic(code("unused::function"))]
    UnusedPrivateFunction {
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
    #[diagnostic(code("unused::constant"))]
    UnusedPrivateModuleConstant {
        #[label]
        location: Span,
        name: String,
    },

    #[error("I discovered an unused type: '{}'.\n", name.purple())]
    #[diagnostic(code("unused::type"))]
    UnusedType {
        #[label]
        location: Span,
        imported: bool,
        name: String,
    },

    #[error("I came across an unused variable.\n")]
    #[diagnostic(help(
        "No big deal, but you might want to remove it to get rid of that warning."
    ))]
    #[diagnostic(code("unused::variable"))]
    UnusedVariable {
        #[label("unused")]
        location: Span,
        name: String,
    },

    #[error("I came across a validator in a {} module which means\nI'm going to ignore it.\n", "lib/".purple())]
    #[diagnostic(help(
        "No big deal, but you might want to move it to a {} module\nor remove it to get rid of that warning.",
        "validators/".purple()
    ))]
    #[diagnostic(code("unused::validator"))]
    ValidatorInLibraryModule {
        #[label("unused")]
        location: Span,
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

fn format_suggestion(sample: &UntypedExpr) -> String {
    Formatter::new()
        .expr(sample)
        .to_pretty_string(70)
        .lines()
        .enumerate()
        .map(|(ix, line)| {
            if ix == 0 {
                format!("╰─▶ {}", line.yellow())
            } else {
                format!("    {line}").yellow().to_string()
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}
