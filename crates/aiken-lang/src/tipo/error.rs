use super::Type;
use crate::{
    ast::{Annotation, BinOp, CallArg, LogicalOpChainKind, Span, UntypedFunction, UntypedPattern},
    error::ExtraData,
    expr::{self, AssignmentPattern, UntypedAssignmentKind, UntypedExpr},
    format::Formatter,
    levenshtein,
    pretty::Documentable,
};
use indoc::formatdoc;
use itertools::Itertools;
use miette::{Diagnostic, LabeledSpan};
use ordinal::Ordinal;
use owo_colors::{
    OwoColorize,
    Stream::{Stderr, Stdout},
};
use std::{collections::HashMap, fmt::Display, rc::Rc};
use vec1::Vec1;

#[derive(Debug, Clone, thiserror::Error)]
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
                .map(|s| format!("─▶ {}", s.if_supports_color(Stdout, |s| s.yellow())))
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

#[derive(Debug, thiserror::Error, Diagnostic, Clone)]
pub enum Error {
    #[error("I discovered an {} chain with less than 2 expressions.", op.if_supports_color(Stdout, |s| s.purple()))]
    #[diagnostic(code("illegal::logical_op_chain"))]
    #[diagnostic(help(
        "Logical {}/{} chains require at least 2 expressions. You are missing {}.",
        "and".if_supports_color(Stdout, |s| s.purple()),
        "or".if_supports_color(Stdout, |s| s.purple()),
        missing
    ))]
    LogicalOpChainMissingExpr {
        op: LogicalOpChainKind,
        #[label("not enough operands")]
        location: Span,
        missing: u8,
    },

    #[error("I discovered a type cast from Data without an annotation.")]
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
        expected: Rc<Type>,
        given: Rc<Type>,
        situation: Option<UnifyErrorSituation>,
        rigid_type_names: HashMap<u64, String>,
    },

    #[error("I almost got caught in an infinite cycle of type definitions.\n")]
    #[diagnostic(url("https://aiken-lang.org/language-tour/custom-types#type-aliases"))]
    #[diagnostic(code("cycle"))]
    CyclicTypeDefinitions {
        #[label(collection, "part of a cycle")]
        cycle: Vec<Span>,
    },

    #[error(
        "I found two function arguments both called '{}'.\n",
        label.if_supports_color(Stdout, |s| s.purple())
    )]
    #[diagnostic(code("duplicate::argument"))]
    #[diagnostic(help(
        "Function arguments cannot have the same name. You can use '{discard}' and numbers to distinguish between similar names.",
        discard = "_".if_supports_color(Stdout, |s| s.yellow())
    ))]
    DuplicateArgument {
        #[label("found here")]
        location: Span,
        #[label("found here again")]
        duplicate_location: Span,
        label: String,
    },

    #[error("I found two declarations for the constant '{}'.\n", name.purple())]
    #[diagnostic(code("duplicate::constant"))]
    #[diagnostic(help(
        "Top-level constants of a same module cannot have the same name. You can use '{discard}' and numbers to distinguish between similar names.",
        discard = "_".if_supports_color(Stdout, |s| s.yellow())
    ))]
    DuplicateConstName {
        #[label("declared again here")]
        location: Span,
        #[label("declared here")]
        previous_location: Span,
        name: String,
    },

    #[error(
        "I stumbled upon the field '{}' twice in a data-type definition.\n",
        label.if_supports_color(Stdout, |s| s.purple())
    )]
    #[diagnostic(code("duplicate::field"))]
    #[diagnostic(help(r#"Data-types must have fields with strictly different names. You can use '{discard}' and numbers to distinguish between similar names.
Note that it is also possible to declare data-types with positional (nameless) fields only.

For example:

  ┍━━━━━━━━━━━━━━━━━━━━━━━
  │ {keyword_pub} {keyword_type} {type_Point} {{
  │   {variant_Point}({type_Int}, {type_Int}, {type_Int})
  │ }}
"#
        , discard = "_".if_supports_color(Stdout, |s| s.yellow())
        , keyword_pub = "pub".if_supports_color(Stdout, |s| s.bright_blue())
        , keyword_type = "type".if_supports_color(Stdout, |s| s.yellow())
        , type_Int = "Int".if_supports_color(Stdout, |s| s.green())
        , type_Point = "Point".if_supports_color(Stdout, |s| s.green())
        , variant_Point = "Point".if_supports_color(Stdout, |s| s.green())
    ))]
    DuplicateField {
        #[label("found here")]
        location: Span,
        #[label("found here again")]
        duplicate_location: Span,
        label: String,
    },

    #[error(
        "I noticed you were importing '{}' twice.\n",
        name.if_supports_color(Stdout, |s| s.purple())
    )]
    #[diagnostic(code("duplicate::import"))]
    #[diagnostic(help(r#"If you're trying to import two modules with identical names but from different packages, you'll need to use a named import.
For example:

╰─▶ {keyword_use} {import} {keyword_as} {named}

Otherwise, just remove the redundant import."#
        , keyword_use = "use".if_supports_color(Stdout, |s| s.bright_blue())
        , keyword_as = "as".if_supports_color(Stdout, |s| s.bright_blue())
        , import = module
            .iter()
            .map(|x| x.if_supports_color(Stdout, |s| s.purple()).to_string())
            .collect::<Vec<_>>()
            .join("/".if_supports_color(Stdout, |s| s.bold()).to_string().as_ref())
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

    #[error(
        "I discovered two top-level objects referred to as '{}'.\n",
        name.if_supports_color(Stdout, |s| s.purple())
    )]
    #[diagnostic(code("duplicate::name"))]
    #[diagnostic(help(
        r#"Top-level definitions cannot have the same name, even if they refer to objects with different natures (e.g. function and test).

You can use '{discard}' and numbers to distinguish between similar names.
"#,
        discard = "_".if_supports_color(Stdout, |s| s.yellow())
    ))]
    DuplicateName {
        #[label("also defined here")]
        location: Span,
        #[label("originally defined here")]
        previous_location: Span,
        name: String,
    },

    #[error(
        "I found two types declared with the same name: '{}'.\n",
        name.if_supports_color(Stdout, |s| s.purple())
    )]
    #[diagnostic(code("duplicate::type"))]
    #[diagnostic(help(
        "Types cannot have the same top-level name. You {cannot} use '_' in types name, but you can use numbers to distinguish between similar names.",
        cannot = "cannot".if_supports_color(Stdout, |s| s.red())
    ))]
    DuplicateTypeName {
        #[label("also defined here")]
        location: Span,
        #[label("originally defined here")]
        previous_location: Span,
        name: String,
    },

    #[error(
        "I realized the variable '{}' was mentioned more than once in an alternative pattern.\n",
        name.if_supports_color(Stdout, |s| s.purple())
    )]
    #[diagnostic(url(
        "https://aiken-lang.org/language-tour/control-flow#alternative-clause-patterns"
    ))]
    #[diagnostic(code("duplicate::pattern"))]
    DuplicateVarInPattern {
        #[label("duplicate identifier")]
        location: Span,
        name: String,
    },

    #[error(
        "I tripped over an extra variable in an alternative pattern: {}.\n",
        name.if_supports_color(Stdout, |s| s.purple())
    )]
    #[diagnostic(url(
        "https://aiken-lang.org/language-tour/control-flow#alternative-clause-patterns"
    ))]
    #[diagnostic(code("unexpected::variable"))]
    ExtraVarInAlternativePattern {
        #[label("unexpected variable")]
        location: Span,
        name: String,
    },

    #[error("I caught an opaque type possibly breaking its abstraction boundary.\n")]
    #[diagnostic(code("illegal::expect_on_opaque"))]
    #[diagnostic(url("https://aiken-lang.org/language-tour/modules#opaque-types"))]
    #[diagnostic(help(
        "This expression is trying to convert something unknown into an opaque type. An opaque type is a data-type which hides its internal details; usually because it enforces some specific invariant on its internal structure. For example, you might define a {Natural} type that holds an {Integer} but ensures that it never gets negative.\n\nA direct consequence means that it isn't generally possible, nor safe, to turn *any* value into an opaque type. Instead, use the constructors and methods provided for lifting values into that opaque type while ensuring that any structural invariant is checked for.",
        Natural = "Natural".if_supports_color(Stdout, |s| s.cyan()),
        Integer = "Integer".if_supports_color(Stdout, |s| s.cyan()),
    ))]
    ExpectOnOpaqueType {
        #[label("reckless opaque cast")]
        location: Span,
    },

    #[error("I found a type definition that has a function type in it. This is not allowed.\n")]
    #[diagnostic(code("illegal::function_in_type"))]
    #[diagnostic(help(
        "Data-types can't hold functions. If you want to define method-like functions, group the type definition and the methods under a common namespace in a standalone module."
    ))]
    FunctionTypeInData {
        #[label("non-serialisable inhabitants")]
        location: Span,
    },

    #[error("I found a type definition that has unsupported inhabitants.\n")]
    #[diagnostic(code("illegal::type_in_data"))]
    #[diagnostic(help(
        r#"Data-types cannot contain values of type {type_info} because they aren't serialisable into a Plutus Data. Yet this is necessary for inhabitants of compound structures like {List}, {Tuple} or {Fuzzer}."#,
        type_info = tipo.to_pretty(0).if_supports_color(Stdout, |s| s.red()),
        List = "List".if_supports_color(Stdout, |s| s.cyan()),
        Tuple = "Tuple".if_supports_color(Stdout, |s| s.cyan()),
        Fuzzer = "Fuzzer".if_supports_color(Stdout, |s| s.cyan()),
    ))]
    IllegalTypeInData {
        #[label("non-serialisable inhabitants")]
        location: Span,
        tipo: Rc<Type>,
    },

    #[error("I noticed an inadequate use of '=='.\n")]
    #[diagnostic(code("illegal::comparison"))]
    #[diagnostic(help(
        r#"I can compare any value that is serializable to {Data}. This excludes values that are functions, {Fuzzer} or {MillerLoopResult} for example."#,
        Data = "Data".if_supports_color(Stdout, |s| s.cyan()),
        Fuzzer = "Fuzzer".if_supports_color(Stdout, |s| s.cyan()),
        MillerLoopResult = "MillerLoopResult".if_supports_color(Stdout, |s| s.cyan()),
    ))]
    IllegalComparison {
        #[label("non-serialisable operands")]
        location: Span,
    },

    #[error("I found a discarded expression not bound to a variable.\n")]
    #[diagnostic(code("implicit_discard"))]
    #[diagnostic(help(
        "A function can contain a sequence of expressions. However, any expression but the last one must be assigned to a variable using the {keyword_let} keyword. If you really wish to discard an expression that is unused, you can assign it to '{discard}'.",
        keyword_let = "let".if_supports_color(Stdout, |s| s.yellow()),
        discard = "_".if_supports_color(Stdout, |s| s.yellow())
    ))]
    ImplicitlyDiscardedExpression {
        #[label("implicitly discarded")]
        location: Span,
    },

    #[error(
        "I saw {} field{} in a context where there should be {}.\n",
        given.if_supports_color(Stdout, |s| s.purple()),
        if *given <= 1 { "" } else { "s"},
        expected.if_supports_color(Stdout, |s| s.purple()),
    )]
    #[diagnostic(url("https://aiken-lang.org/language-tour/custom-types"))]
    #[diagnostic(code("arity::constructor"))]
    IncorrectFieldsArity {
        #[label("{}", if given < expected { "missing fields" } else { "extraneous fields" })]
        location: Span,
        expected: usize,
        given: usize,
        labels: Vec<String>,
    },

    #[error(
        "I saw a function or constructor that expects {} arguments be called with {} arguments.\n",
        expected.if_supports_color(Stdout, |s| s.purple()),
        given.if_supports_color(Stdout, |s| s.purple())
    )]
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
        , discard = "_".if_supports_color(Stdout, |s| s.yellow())
        , expected = expected.if_supports_color(Stdout, |s| s.purple())
        , keyword_fn = "fn".if_supports_color(Stdout, |s| s.yellow())
        , keyword_let = "let".if_supports_color(Stdout, |s| s.yellow())
        , type_Int = "Int".if_supports_color(Stdout, |s| s.green())
    ))]
    IncorrectFunctionCallArity {
        #[label("{}", if given < expected { "missing arguments" } else { "extraneous arguments" })]
        location: Span,
        expected: usize,
        given: usize,
    },

    #[error(
        "I saw a pattern on a constructor that has {} field(s) be matched with {} argument(s).\n",
        expected.if_supports_color(Stdout, |s| s.purple()),
        given.len().if_supports_color(Stdout, |s| s.purple())
    )]
    #[diagnostic(url("https://aiken-lang.org/language-tour/control-flow#matching"))]
    #[diagnostic(code("arity::pattern"))]
    #[diagnostic(help(
        "When pattern-matching on constructors, you must either match the exact number of fields, or use the spread operator '{spread}'. Note that unused fields must be discarded by prefixing their name with '{discard}'.",
        discard = "_".if_supports_color(Stdout, |s| s.yellow()),
        spread = "..".if_supports_color(Stdout, |s| s.yellow()),
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

    #[error(
        "I saw a pattern on a {}-tuple be matched into a {}-tuple.\n",
        expected.if_supports_color(Stdout, |s| s.purple()),
        given.if_supports_color(Stdout, |s| s.purple())
    )]
    #[diagnostic(url("https://aiken-lang.org/language-tour/control-flow#destructuring"))]
    #[diagnostic(code("arity::tuple"))]
    #[diagnostic(help(
        "When pattern matching on a tuple, you must match all of its elements. Note that unused fields must be discarded by prefixing their name with '{discard}'.",
        discard = "_".if_supports_color(Stdout, |s| s.yellow())
    ))]
    IncorrectTupleArity {
        #[label("{}", if given < expected { "missing elements" } else { "extraneous elements" })]
        location: Span,
        expected: usize,
        given: usize,
    },

    #[error(
        "I noticed a generic data-type with {} type parameters instead of {}.\n",
        given.if_supports_color(Stdout, |s| s.purple()),
        expected.if_supports_color(Stdout, |s| s.purple()),
    )]
    #[diagnostic(url("https://aiken-lang.org/language-tour/custom-types#generics"))]
    #[diagnostic(code("arity::generic"))]
    #[diagnostic(help(
        "{}",
        if *expected == 0 {
            format!(
                r#"Data-types without generic parameters should be written without chevrons.
Perhaps, try the following:

╰─▶  {suggestion}"#,
                suggestion = suggest_generic(name, *expected)
            )
        } else {
            format!(
                r#"Data-types that are generic in one or more types must be written with all their generic types in type annotations. Generic types must be indicated between chevrons '{chevron_left}' and '{chevron_right}'.
Perhaps, try the following:

╰─▶  {suggestion}"#
                , chevron_left = "<".if_supports_color(Stdout, |s| s.yellow())
                , chevron_right = ">".if_supports_color(Stdout, |s| s.yellow())
                , suggestion = suggest_generic(name, *expected)
            )
        }
    ))]
    IncorrectTypeArity {
        #[label("incorrect generic arity")]
        location: Span,
        name: String,
        expected: usize,
        given: usize,
    },

    #[error(
      "I realized the module '{}' contains the keyword '{}', which is forbidden.\n",
      name.if_supports_color(Stdout, |s| s.purple()),
      keyword.if_supports_color(Stdout, |s| s.purple()),
    )]
    #[diagnostic(url("https://aiken-lang.org/language-tour/modules"))]
    #[diagnostic(code("illegal::module_name"))]
    #[diagnostic(help(r#"You cannot use keywords as part of a module path name. As a quick reminder, here's a list of all the keywords (and thus, of invalid module path names):

    as, expect, check, const, else, fn, if, is, let, opaque, pub, test, todo, trace, type, use, when"#))]
    KeywordInModuleName { name: String, keyword: String },

    #[error("I discovered a block which is ending with an assignment.\n")]
    #[diagnostic(url("https://aiken-lang.org/language-tour/functions#named-functions"))]
    #[diagnostic(code("illegal::return"))]
    #[diagnostic(help(r#"In Aiken, code blocks (such as function bodies) must return an explicit result in the form of an expression. While assignments are technically speaking expressions, they aren't allowed to be the last expression of a function because they convey a different meaning and this could be error-prone.

If you really meant to return that last expression, try to replace it with the following:

{sample}"#
        , sample = format_suggestion(expr)
    ))]
    LastExpressionIsAssignment {
        #[label("let-binding as last expression")]
        location: Span,
        expr: expr::UntypedExpr,
        patterns: Vec1<AssignmentPattern>,
        kind: UntypedAssignmentKind,
    },

    #[error(
        "I found a missing variable in an alternative pattern: {}.\n",
        name.if_supports_color(Stdout, |s| s.purple())
    )]
    #[diagnostic(url(
        "https://aiken-lang.org/language-tour/control-flow#alternative-clause-patterns"
    ))]
    #[diagnostic(code("missing::variable"))]
    MissingVarInAlternativePattern {
        #[label("missing case")]
        location: Span,
        name: String,
    },

    #[error("I tripped over an attempt to access elements on something that isn't indexable.\n")]
    #[diagnostic(url("https://aiken-lang.org/language-tour/primitive-types#tuples"))]
    #[diagnostic(code("illegal::indexable"))]
    #[diagnostic(help(
        r#"Because you used an ordinal index on an element, I assumed it had to be a tuple or a pair but instead I found something of type:

╰─▶ {type_info}"#,
        type_info = tipo.to_pretty(0).if_supports_color(Stdout, |s| s.red())
    ))]
    NotIndexable {
        #[label("not indexable")]
        location: Span,
        tipo: Rc<Type>,
    },

    #[error("{}\n", if *is_let {
          "I noticed an incomplete single-pattern matching a value with more than one pattern.".to_string()
      } else {
          format!(
              "I realized that a given '{keyword_when}/{keyword_is}' expression is non-exhaustive.",
              keyword_is = "is".if_supports_color(Stdout, |s| s.purple()),
              keyword_when = "when".if_supports_color(Stdout, |s| s.purple())
          )
      }
    )]
    #[diagnostic(url("https://aiken-lang.org/language-tour/control-flow#matching"))]
    #[diagnostic(code("non_exhaustive_pattern_match"))]
    #[diagnostic(help(r#"Let bindings and when clauses must be exhaustive -- that is, they must cover all possible cases of the type they match. In {keyword_when}/{keyword_is} pattern-match, it is recommended to have an explicit branch for each constructor as it prevents future silly mistakes when adding new constructors to a type. However, you can also use the wildcard '{discard}' as a last branch to match any remaining result.

In this particular instance, the following cases are unmatched:

{missing}"#
        , discard = "_".if_supports_color(Stdout, |s| s.yellow())
        , keyword_is = "is".if_supports_color(Stdout, |s| s.purple())
        , keyword_when = "when".if_supports_color(Stdout, |s| s.purple())
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
    #[diagnostic(help(
        r#"It seems like you're trying to call something that isn't a function. I am inferring the following type:

╰─▶ {inference}"#,
        inference = tipo.to_pretty(0)
    ))]
    NotFn {
        #[label("not a function")]
        location: Span,
        tipo: Rc<Type>,
    },

    #[error("I discovered a positional argument after a label argument.\n")]
    #[diagnostic(url("https://aiken-lang.org/language-tour/functions#labeled-arguments"))]
    #[diagnostic(code("unexpected::positional_argument"))]
    #[diagnostic(help(r#"You can mix positional and labeled arguments, but you must put all positional arguments (i.e. without label) at the front.

To fix this, you'll need to either turn that argument as a labeled argument, or make the next one positional."#))]
    PositionalArgumentAfterLabeled {
        #[label("by position")]
        location: Span,
        #[label("by label")]
        labeled_arg_location: Span,
    },

    #[error("I caught a private value trying to escape.\n")]
    #[diagnostic(url("https://aiken-lang.org/language-tour/modules"))]
    #[diagnostic(code("private_leak"))]
    #[diagnostic(help(r#"I found a public value that is making use of a private type. This would prevent other modules from actually using that value because they wouldn't know what this type refer to.

The culprit is:

{type_info}

Maybe you meant to turn it public using the '{keyword_pub}' keyword?"#
        , type_info = leaked.to_pretty(4).if_supports_color(Stdout, |s| s.red())
        , keyword_pub = "pub".if_supports_color(Stdout, |s| s.bright_blue())
    ))]
    PrivateTypeLeak {
        #[label("private type leak")]
        location: Span,
        leaked: Type,
    },

    #[error(
        "{}\n",
        format!(
            "I discovered a '{keyword_when}/{keyword_is}' expression with a redundant pattern.",
            keyword_is = "is".if_supports_color(Stdout, |s| s.purple()),
            keyword_when = "when".if_supports_color(Stdout, |s| s.purple())
        )
    )]
    #[diagnostic(url("https://aiken-lang.org/language-tour/control-flow#matching"))]
    #[diagnostic(code("redundant_pattern_match"))]
    #[diagnostic(help("Double check these patterns and then remove one of the clauses."))]
    RedundantMatchClause {
        #[label("first found here")]
        original: Option<Span>,
        #[label("redundant")]
        redundant: Span,
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
        , keyword_fn = "fn".if_supports_color(Stdout, |s| s.yellow())
        , keyword_let = "let".if_supports_color(Stdout, |s| s.yellow())
        , type_ScriptContext = "ScriptContext".if_supports_color(Stdout, |s| s.green())
    ))]
    RecordAccessUnknownType {
        #[label("annotation needed")]
        location: Span,
    },

    #[error("I tripped over an invalid constructor in a record update.\n")]
    #[diagnostic(url("https://aiken-lang.org/language-tour/custom-types#record-updates"))]
    #[diagnostic(code("illegal::record_update"))]
    RecordUpdateInvalidConstructor {
        #[label("invalid constructor")]
        location: Span,
    },

    #[error("I almost got caught in an endless loop while inferring a recursive type.\n")]
    #[diagnostic(url("https://aiken-lang.org/language-tour/custom-types#type-annotations"))]
    #[diagnostic(code("missing::type_annotation"))]
    #[diagnostic(help(
        "I have several aptitudes, but inferring recursive types isn't one them. It is still possible to define recursive types just fine, but I will need a little help in the form of type annotation to infer their types should they show up."
    ))]
    RecursiveType {
        #[label("infinite recursion")]
        location: Span,
    },

    #[error(
        "I discovered an attempt to access the {} element of a {}-tuple.\n",
        Ordinal(*index + 1).to_string().if_supports_color(Stdout, |s| s.purple()),
        size.if_supports_color(Stdout, |s| s.purple())
    )]
    #[diagnostic(url("https://aiken-lang.org/language-tour/primitive-types#tuples"))]
    #[diagnostic(code("invalid::tuple_index"))]
    TupleIndexOutOfBound {
        #[label("out of bounds")]
        location: Span,
        index: usize,
        size: usize,
    },

    #[error(
        "I discovered an attempt to access the {} element of a {}.\n",
        Ordinal(*index + 1).to_string().if_supports_color(Stdout, |s| s.purple()),
        "Pair".if_supports_color(Stdout, |s| s.bright_blue()).if_supports_color(Stdout, |s| s.bold()),
    )]
    #[diagnostic(url("https://aiken-lang.org/language-tour/primitive-types#pairs"))]
    #[diagnostic(code("invalid::pair_index"))]
    PairIndexOutOfBound {
        #[label("out of bounds")]
        location: Span,
        index: usize,
    },

    #[error(
        "I tripped over the following labeled argument: {}.\n",
        label.if_supports_color(Stdout, |s| s.purple())
    )]
    #[diagnostic(url("https://aiken-lang.org/language-tour/functions#labeled-arguments"))]
    #[diagnostic(code("unexpected::module_name"))]
    UnexpectedLabeledArg {
        #[label("unexpected labeled args")]
        location: Span,
        label: String,
    },

    #[error(
        "I tripped over the following labeled argument: {}.\n",
        label.if_supports_color(Stdout, |s| s.purple())
    )]
    #[diagnostic(url("https://aiken-lang.org/language-tour/custom-types#named-accessors"))]
    #[diagnostic(code("unexpected::labeled_argument"))]
    #[diagnostic(help(r#"The constructor '{constructor}' does not have any labeled field. Its fields must therefore be matched only by position.

Perhaps, try the following:

╰─▶  {suggestion}
"#
        , constructor = name
            .if_supports_color(Stdout, |s| s.bright_blue())
            .if_supports_color(Stdout, |s| s.bold())
        , suggestion = suggest_constructor_pattern(name, args, module, *spread_location)
    ))]
    UnexpectedLabeledArgInPattern {
        #[label("unexpected labeled arg")]
        location: Span,
        label: String,
        name: String,
        args: Vec<CallArg<UntypedPattern>>,
        module: Option<String>,
        spread_location: Option<Span>,
    },

    #[error("I discovered a regular let assignment with multiple patterns.\n")]
    #[diagnostic(code("unexpected::multi_pattern_assignment"))]
    #[diagnostic(help(
        "Did you mean to use backpassing syntax with {}?",
        "<-".if_supports_color(Stdout, |s| s.purple())
    ))]
    UnexpectedMultiPatternAssignment {
        #[label("unexpected")]
        location: Span,
        #[label("<-")]
        arrow: Span,
    },

    #[error("I tripped over some unknown labels in a pattern or function.\n")]
    #[diagnostic(code("unknown::labels"))]
    UnknownLabels(#[related] Vec<UnknownLabels>),

    #[error(
        "I stumbled upon a reference to an unknown module: '{}'\n",
        name.if_supports_color(Stdout, |s| s.purple())
    )]
    #[diagnostic(code("unknown::module"))]
    #[diagnostic(help(
        "{}",
        suggest_neighbor(name, known_modules.iter(), "Did you forget to add a package as dependency?")
    ))]
    UnknownModule {
        #[label("unknown module")]
        location: Span,
        name: String,
        known_modules: Vec<String>,
    },

    #[error(
        "I couldn't find any module for the environment: '{}'\n",
        name.if_supports_color(Stdout, |s| s.purple())
    )]
    #[diagnostic(code("unknown::environment"))]
    #[diagnostic(help(
        "{}{}",
        if known_environments.is_empty() {
            String::new()
        } else {
            format!(
                "I know about the following environments:\n{}\n\n",
                known_environments
                    .iter()
                    .map(|s| format!("─▶ {}", s.if_supports_color(Stdout, |s| s.purple())))
                    .collect::<Vec<_>>()
                    .join("\n")
            )
        },
        suggest_neighbor(name, known_environments.iter(), "Did you forget to define this environment?")
    ))]
    UnknownEnvironment {
        name: String,
        known_environments: Vec<String>,
    },

    #[error(
        "I found an unknown import '{}' from module '{}'.\n",
        name.if_supports_color(Stdout, |s| s.purple()),
        module_name.if_supports_color(Stdout, |s| s.purple())
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
        #[label("unknown import")]
        location: Span,
        name: String,
        module_name: String,
        value_constructors: Vec<String>,
        type_constructors: Vec<String>,
    },

    #[error(
        "I looked for '{}' in '{}' but couldn't find it.\n",
        name.if_supports_color(Stdout, |s| s.purple()),
        module_name.if_supports_color(Stdout, |s| s.purple())
    )]
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
        #[label("unknown import")]
        location: Span,
        name: String,
        module_name: String,
        type_constructors: Vec<String>,
    },

    #[error("I looked for '{}' in '{}' but couldn't find it.\n",
        name.if_supports_color(Stdout, |s| s.purple()),
        module_name.if_supports_color(Stdout, |s| s.purple())
    )]
    #[diagnostic(code("unknown::module_value"))]
    #[diagnostic(help(
        "{}",
        if ["mk_nil_data", "mk_pair_data", "mk_nil_pair_data"].contains(&.name.as_str()) {
            format!(
                "It seems like you're looking for a builtin function that has been (recently) renamed. Sorry about that, but take notes of the new names of the following functions:\n\n{:<16} -> {}\n{:<16} -> {}\n{:<16} -> {}",
                "mk_nil_data".if_supports_color(Stderr, |s| s.red()),
                "new_list".if_supports_color(Stderr, |s| s.green()),
                "mk_pair_data".if_supports_color(Stderr, |s| s.red()),
                "new_pair".if_supports_color(Stderr, |s| s.green()),
                "mk_nil_pair_data".if_supports_color(Stderr, |s| s.red()),
                "new_pairs".if_supports_color(Stderr, |s| s.green()),
            )
        } else {
            suggest_neighbor(
                name,
                value_constructors.iter(),
                &suggest_make_public()
            )
        }
    ))]
    UnknownModuleValue {
        #[label("not exported by {module_name}?")]
        location: Span,
        name: String,
        module_name: String,
        value_constructors: Vec<String>,
    },

    #[error(
      "I looked for the field '{}' in a record of type '{}' but couldn't find it.\n",
      label.if_supports_color(Stdout, |s| s.purple()),
      typ.to_pretty(0).if_supports_color(Stdout, |s| s.purple()),
    )]
    #[diagnostic(code("unknown::record_field"))]
    #[diagnostic(help(
        "{}",
        suggest_neighbor(label, fields.iter(), "Did you forget to make it public?\nNote also that record access is only supported on types with a single constructor.")
    ))]
    UnknownRecordField {
        #[label("unknown field")]
        location: Span,
        typ: Rc<Type>,
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

    #[error(
        "I found a reference to an unknown data-type constructor: '{}'.\n",
        name.if_supports_color(Stdout, |s| s.purple())
    )]
    #[diagnostic(code("unknown::type_constructor"))]
    #[diagnostic(help(
        "{}",
        suggest_neighbor(name, constructors.iter(), "Did you forget to import it?")
    ))]
    UnknownTypeConstructor {
        #[label("unknown constructor")]
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
            &if name.chars().next().unwrap().is_uppercase() {
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
        #[label("unnecessary spread")]
        location: Span,
        arity: usize,
    },

    #[error("I tripped over a record-update on a data-type with more than one constructor.\n")]
    #[diagnostic(url("https://aiken-lang.org/language-tour/custom-types#record-updates"))]
    #[diagnostic(code("illegal::record_update"))]
    UpdateMultiConstructorType {
        #[label("more than one constructor")]
        location: Span,
    },

    #[error(
        "I discovered an attempt to import a validator module in a library: '{}'\n",
        name.if_supports_color(Stdout, |s| s.purple())
    )]
    #[diagnostic(code("illegal::import"))]
    #[diagnostic(help(
        "If you are trying to share code defined in a validator then move it to a library module under {}.\nIf, however, you are trying to import a validator for testing, make sure that your test module doesn't export any definition using the {} keyword.",
        "lib/".if_supports_color(Stdout, |s| s.purple()),
        "pub".if_supports_color(Stdout, |s| s.cyan())
    ))]
    ValidatorImported {
        #[label("imported validator")]
        location: Span,
        name: String,
    },

    #[error(
        "A validator must return {}.\n",
        "Bool"
            .if_supports_color(Stdout, |s| s.bright_blue())
            .if_supports_color(Stdout, |s| s.bold())
    )]
    #[diagnostic(code("illegal::validator_return_type"))]
    #[diagnostic(help(r#"While analyzing the return type of your validator, I found it to be:

╰─▶ {signature}

...but I expected this to be a {type_Bool}. If I am inferring the wrong type, try annotating the validator's return type with Bool"#
        , type_Bool = "Bool"
            .if_supports_color(Stdout, |s| s.bright_blue())
            .if_supports_color(Stdout, |s| s.bold())
        , signature = return_type.to_pretty(0).if_supports_color(Stdout, |s| s.red())
    ))]
    ValidatorMustReturnBool {
        #[label("invalid return type")]
        location: Span,
        return_type: Rc<Type>,
    },

    #[error("Validators require at least 2 arguments and at most 3 arguments.\n")]
    #[diagnostic(code("illegal::validator_arity"))]
    #[diagnostic(help(
        "Please {}. If you don't need one of the required arguments use an underscore (e.g. `_datum`).",
        if *count < *expected {
            let missing = expected - count;

            let mut arguments = "argument".to_string();

            if missing > 1 {
                arguments.push('s');
            }

            format!(
                "add the {} missing {arguments}",
                missing.to_string().if_supports_color(Stdout, |s| s.yellow()),
            )
        } else {
            let extra = count - expected;

            let mut arguments = "argument".to_string();

            if extra > 1 {
                arguments.push('s');
            }

            format!(
                "remove the {} extra {arguments}",
                extra.to_string().if_supports_color(Stdout, |s| s.yellow()),
            )
        }
    ))]
    IncorrectValidatorArity {
        count: u32,
        expected: u32,
        #[label("{} arguments", if count < expected { "not enough" } else { "too many" })]
        location: Span,
    },

    #[error("I caught a test with too many arguments.\n")]
    #[diagnostic(code("illegal::test::arity"))]
    #[diagnostic(help(
        "Tests are allowed to have 0 or 1 argument, but no more. Here I've found a test definition with {count} arguments. If you need to provide multiple values to a test, use a Record or a Tuple.",
    ))]
    IncorrectTestArity {
        count: usize,
        #[label("too many arguments")]
        location: Span,
    },

    #[error("I caught a test with an illegal return type.\n")]
    #[diagnostic(code("illegal::test::return"))]
    #[diagnostic(help(
        "Tests must return either {Bool} or {Void}. Note that `expect` assignment are implicitly typed {Void} (and thus, may be the last expression of a test).",
        Bool = "Bool".if_supports_color(Stderr, |s| s.cyan()),
        Void = "Void".if_supports_color(Stderr, |s| s.cyan()),
    ))]
    IllegalTestType {
        #[label("expected Bool or Void")]
        location: Span,
    },

    #[error("I choked on a generic type left in an outward-facing interface.\n")]
    #[diagnostic(code("illegal::generic_in_abi"))]
    #[diagnostic(help(
        "Elements of the outer-most parts of a project, such as a validator, constants or a property-based test, must be fully instantiated. That means they can no longer carry unbound or generic variables. The type must be fully-known at this point since many structural validation must occur to ensure a safe boundary between the on-chain and off-chain worlds."
    ))]
    GenericLeftAtBoundary {
        #[label("unbound generic at boundary")]
        location: Span,
    },

    #[error("Cannot infer caller without inferring callee first")]
    MustInferFirst {
        function: UntypedFunction,
        location: Span,
    },

    #[error("I found a validator handler referring to an unknown purpose.\n")]
    #[diagnostic(code("unknown::purpose"))]
    #[diagnostic(help(
        "Handler must be named after a known purpose. Here is a list of available purposes:\n{}",
        available_purposes
          .iter()
          .map(|p| format!("-> {}", p.if_supports_color(Stdout, |s| s.green())))
          .join("\n")
    ))]
    UnknownPurpose {
        #[label("unknown purpose")]
        location: Span,
        available_purposes: Vec<String>,
    },

    #[error("I could not find an appropriate handler in the validator definition\n")]
    #[diagnostic(code("unknown::handler"))]
    #[diagnostic(help(
        "When referring to a validator handler via record access, you must refer to one of the declared handlers{}{}",
        if available_handlers.is_empty() { "." } else { ":\n" },
        available_handlers
          .iter()
          .map(|p| format!("-> {}", p.if_supports_color(Stdout, |s| s.green())))
          .join("\n")
    ))]
    UnknownValidatorHandler {
        #[label("unknown validator handler")]
        location: Span,
        available_handlers: Vec<String>,
    },

    #[error("I caught an extraneous fallback handler in an already exhaustive validator\n")]
    #[diagnostic(code("extraneous::fallback"))]
    #[diagnostic(help(
        "Validator handlers must be exhaustive and either cover all purposes, or provide a fallback handler. Here, you have successfully covered all script purposes with your handler, but left an extraneous fallback branch. I cannot let that happen, but removing it for you would probably be deemed rude. So please, remove the fallback."
    ))]
    UnexpectedValidatorFallback {
        #[label("redundant fallback handler")]
        fallback: Span,
    },
}

impl ExtraData for Error {
    fn extra_data(&self) -> Option<String> {
        match self {
            Error::CastDataNoAnn { .. }
            | Error::CouldNotUnify { .. }
            | Error::CyclicTypeDefinitions { .. }
            | Error::DuplicateArgument { .. }
            | Error::DuplicateConstName { .. }
            | Error::DuplicateField { .. }
            | Error::DuplicateImport { .. }
            | Error::DuplicateName { .. }
            | Error::DuplicateTypeName { .. }
            | Error::DuplicateVarInPattern { .. }
            | Error::ExtraVarInAlternativePattern { .. }
            | Error::FunctionTypeInData { .. }
            | Error::IllegalTypeInData { .. }
            | Error::IllegalComparison { .. }
            | Error::ImplicitlyDiscardedExpression { .. }
            | Error::IncorrectFieldsArity { .. }
            | Error::IncorrectFunctionCallArity { .. }
            | Error::IncorrectPatternArity { .. }
            | Error::IncorrectTupleArity { .. }
            | Error::IncorrectTypeArity { .. }
            | Error::IncorrectValidatorArity { .. }
            | Error::KeywordInModuleName { .. }
            | Error::LastExpressionIsAssignment { .. }
            | Error::LogicalOpChainMissingExpr { .. }
            | Error::MissingVarInAlternativePattern { .. }
            | Error::NotIndexable { .. }
            | Error::NotExhaustivePatternMatch { .. }
            | Error::NotFn { .. }
            | Error::PositionalArgumentAfterLabeled { .. }
            | Error::PrivateTypeLeak { .. }
            | Error::RecordAccessUnknownType { .. }
            | Error::RecordUpdateInvalidConstructor { .. }
            | Error::RecursiveType { .. }
            | Error::RedundantMatchClause { .. }
            | Error::TupleIndexOutOfBound { .. }
            | Error::PairIndexOutOfBound { .. }
            | Error::UnexpectedLabeledArg { .. }
            | Error::UnexpectedLabeledArgInPattern { .. }
            | Error::UnknownLabels { .. }
            | Error::UnknownModuleField { .. }
            | Error::UnknownModuleType { .. }
            | Error::UnknownModuleValue { .. }
            | Error::UnknownRecordField { .. }
            | Error::UnknownEnvironment { .. }
            | Error::UnnecessarySpreadOperator { .. }
            | Error::UpdateMultiConstructorType { .. }
            | Error::ValidatorImported { .. }
            | Error::IncorrectTestArity { .. }
            | Error::IllegalTestType { .. }
            | Error::GenericLeftAtBoundary { .. }
            | Error::UnexpectedMultiPatternAssignment { .. }
            | Error::ExpectOnOpaqueType { .. }
            | Error::ValidatorMustReturnBool { .. }
            | Error::UnknownPurpose { .. }
            | Error::UnknownValidatorHandler { .. }
            | Error::UnexpectedValidatorFallback { .. }
            | Error::MustInferFirst { .. } => None,

            Error::UnknownType { name, .. }
            | Error::UnknownTypeConstructor { name, .. }
            | Error::UnknownVariable { name, .. }
            | Error::UnknownModule { name, .. } => Some(name.clone()),
        }
    }
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
                annotated_names.clone_from(new_names);
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
    let threshold = (name.len() as f64).sqrt().round() as usize;
    items
        .map(|s| (s, levenshtein::distance(name, s)))
        .min_by(|(_, a), (_, b)| a.cmp(b))
        .and_then(|(suggestion, distance)| {
            if distance <= threshold {
                Some(format!(
                    "Did you mean '{}'?",
                    suggestion.if_supports_color(Stdout, |s| s.yellow())
                ))
            } else {
                None
            }
        })
        .unwrap_or_else(|| default.to_string())
}

fn suggest_pattern(
    expected: usize,
    name: &str,
    given: &[CallArg<UntypedPattern>],
    module: &Option<String>,
    is_record: bool,
) -> Option<String> {
    if expected > given.len() {
        Some(format!(
            "Try instead: {}",
            Formatter::new()
                .pattern_constructor(name, given, module, Some(Span::empty()), is_record)
                .to_pretty_string(70),
        ))
    } else {
        None
    }
}

fn suggest_generic(name: &str, expected: usize) -> String {
    if expected == 0 {
        return name.to_doc().to_pretty_string(70);
    }

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
    spread_location: Option<Span>,
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
        .pattern_constructor(name, &fixed_args, module, spread_location, false)
        .to_pretty_string(70)
}

fn suggest_unify(
    expected: &Type,
    given: &Type,
    situation: &Option<UnifyErrorSituation>,
    rigid_type_names: &HashMap<u64, String>,
) -> String {
    let expected_str = expected.to_pretty_with_names(rigid_type_names.clone(), 0);
    let given_str = given.to_pretty_with_names(rigid_type_names.clone(), 0);

    let (expected, given) = match (expected, given) {
        (
            Type::App {
                module: expected_module,
                ..
            },
            Type::App {
                module: given_module,
                ..
            },
        ) if expected_str == given_str => {
            let expected_module = if expected_module.is_empty() {
                "aiken"
            } else {
                expected_module
            };

            let given_module = if given_module.is_empty() {
                "aiken"
            } else {
                given_module
            };

            (
                format!(
                    "{}.{{{}}}",
                    expected_module.if_supports_color(Stdout, |s| s.bright_blue()),
                    expected_str.if_supports_color(Stdout, |s| s.green()),
                ),
                format!(
                    "{}.{{{}}}",
                    given_module.if_supports_color(Stdout, |s| s.bright_blue()),
                    given_str.if_supports_color(Stdout, |s| s.red()),
                ),
            )
        }
        _ => (
            expected_str
                .if_supports_color(Stdout, |s| s.green())
                .to_string(),
            given_str.if_supports_color(Stdout, |s| s.red()).to_string(),
        ),
    };

    match situation {
        Some(UnifyErrorSituation::CaseClauseMismatch) => formatdoc! {
            r#"While comparing branches from a '{keyword_when}/{keyword_is}' expression, I realized not all branches have the same type.

               I am expecting all of them to have the following type:

                   {expected}

               but I found some with type:

                   {given}

               Note that I infer the type of the entire '{keyword_when}/{keyword_is}' expression based on the type of the first branch I encounter."#,
            keyword_when = "when".if_supports_color(Stdout, |s| s.yellow()),
            keyword_is = "is".if_supports_color(Stdout, |s| s.yellow()),
            expected = expected,
            given = given
        },
        Some(UnifyErrorSituation::ReturnAnnotationMismatch) => formatdoc! {
            r#"While comparing the return annotation of a function with its actual return type, I realized that both don't match.

               I am inferring the function should return:

                   {}

               but I found that it returns:

                   {}

               Either, fix the annotation or adjust the function body to return the expected type."#,
            expected,
            given
        },
        Some(UnifyErrorSituation::PipeTypeMismatch) => formatdoc! {
            r#"As I was looking at a pipeline you have defined, I realized that one of the pipes isn't valid.

               I am expecting the pipe to send into something of type:

                   {}

               but it is typed:

                   {}

               Either, fix the input or change the target so that both match."#,
            expected,
            given
        },
        Some(UnifyErrorSituation::Operator(op)) => formatdoc! {
            r#"While checking operands of a binary operator, I realized that at least one of them doesn't have the expected type.

               The '{}' operator expects operands of type:

                   {}

               but I discovered the following instead:

                   {}
            "#,
            op.to_doc().to_pretty_string(70).if_supports_color(Stdout, |s| s.yellow()),
            expected,
            given
        },
        Some(UnifyErrorSituation::FuzzerAnnotationMismatch) => formatdoc! {
            r#"While comparing the return annotation of a Fuzzer with its actual return type, I realized that both don't match.

               I am inferring the Fuzzer should return:

                   {}

               but I found a conflicting annotation saying it returns:

                   {}

               Either, fix (or remove) the annotation or adjust the Fuzzer to return the expected type."#,
            expected,
            given
        },
        None => formatdoc! {
            r#"I am inferring the following type:

                   {}

               but I found an expression with a different type:

                   {}

               Either, add type-annotation to improve my inference, or adjust the expression to have the expected type."#,
            expected,
            given
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
        , keyword_fn = "fn".if_supports_color(Stdout, |s| s.yellow())
        , keyword_pub = "pub".if_supports_color(Stdout, |s| s.bright_blue())
        , keyword_type = "type".if_supports_color(Stdout, |s| s.bright_blue())
        , literal_foo = "\"foo\"".if_supports_color(Stdout, |s| s.bright_purple())
        , type_Bar = "Bar"
            .if_supports_color(Stdout, |s| s.bright_blue())
            .if_supports_color(Stdout, |s| s.bold())
        , variant_Bar = "Bar"
            .if_supports_color(Stdout, |s| s.bright_blue())
            .if_supports_color(Stdout, |s| s.bold())
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
        , keyword_fn =  "fn".if_supports_color(Stdout, |s| s.yellow())
        , keyword_is = "is".if_supports_color(Stdout, |s| s.yellow())
        , keyword_pub = "pub".if_supports_color(Stdout, |s| s.bright_blue())
        , keyword_type = "type".if_supports_color(Stdout, |s| s.bright_blue())
        , keyword_use = "use".if_supports_color(Stdout, |s| s.bright_blue())
        , keyword_when = "when".if_supports_color(Stdout, |s| s.yellow())
        , type_Pet = "Pet"
            .if_supports_color(Stdout, |s| s.bright_blue())
            .if_supports_color(Stdout, |s| s.bold())
        , variant_Cat = "Cat"
            .if_supports_color(Stdout, |s| s.bright_blue())
            .if_supports_color(Stdout, |s| s.bold())
        , variant_Dog = "Dog"
            .if_supports_color(Stdout, |s| s.bright_blue())
            .if_supports_color(Stdout, |s| s.bold())
    }
}

#[derive(Debug, PartialEq, Clone, thiserror::Error, Diagnostic)]
pub enum Warning {
    #[error("I found a record update using all fields; thus redundant.")]
    #[diagnostic(url("https://aiken-lang.org/language-tour/custom-types#record-updates"))]
    #[diagnostic(code("record_update::all_fields"))]
    AllFieldsRecordUpdate {
        #[label("redundant record update")]
        location: Span,
    },

    #[error("I realized the following expression returned a result that is implicitly discarded.")]
    #[diagnostic(help(
        "You can use the '_' symbol should you want to explicitly discard a result."
    ))]
    #[diagnostic(code("implicit_discard"))]
    ImplicitlyDiscardedResult {
        #[label("implicitly discarded result")]
        location: Span,
    },

    #[error("I found a record update with no fields; effectively updating nothing.")]
    #[diagnostic(url("https://aiken-lang.org/language-tour/custom-types#record-updates"))]
    #[diagnostic(code("record_update::no_fields"))]
    NoFieldsRecordUpdate {
        #[label("useless record update")]
        location: Span,
    },

    #[error("I found a when expression with a single clause.")]
    #[diagnostic(
        code("single_when_clause"),
        help(
            "Prefer using a {} binding like so...\n\n{}",
            "let".if_supports_color(Stderr, |s| s.purple()),
            format_suggestion(sample)
        )
    )]
    SingleWhenClause {
        #[label("use let")]
        location: Span,
        sample: UntypedExpr,
    },

    #[error(
        "I found an {} {}",
        "expect".if_supports_color(Stderr, |s| s.purple()),
        "trying to match a type with one constructor".if_supports_color(Stderr, |s| s.yellow())
    )]
    #[diagnostic(
        code("single_constructor_expect"),
        help(
            "If your type has one constructor, unless you are casting {} {}, you can\nprefer using a {} binding like so...\n\n{}",
            "from".if_supports_color(Stderr, |s| s.bold()),
            "Data"
                .if_supports_color(Stderr, |s| s.bold())
                .if_supports_color(Stderr, |s| s.bright_blue()),
            "let".if_supports_color(Stderr, |s| s.purple()),
            format_suggestion(sample),
        )
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

    #[error("I found a todo left in the code.")]
    #[diagnostic(help("You probably want to replace that with actual code... eventually."))]
    #[diagnostic(code("todo"))]
    Todo {
        #[label("An expression of type {} is expected here.", tipo.to_pretty(0))]
        location: Span,
        tipo: Rc<Type>,
    },

    #[error("I found a type hole in an annotation.")]
    #[diagnostic(code("unexpected::type_hole"))]
    UnexpectedTypeHole {
        #[label("{}", tipo.to_pretty(0))]
        location: Span,
        tipo: Rc<Type>,
    },

    #[error(
        "I discovered an unused constructor: {}",
        name.if_supports_color(Stderr, |s| s.default_color())
    )]
    #[diagnostic(help(
        "No big deal, but you might want to remove it to get rid of that warning."
    ))]
    #[diagnostic(code("unused::constructor"))]
    UnusedConstructor {
        #[label("unused constructor")]
        location: Span,
        name: String,
    },

    #[error(
        "I discovered an unused imported module: {}",
        name.if_supports_color(Stderr, |s| s.default_color()),
    )]
    #[diagnostic(help(
        "No big deal, but you might want to remove it to get rid of that warning."
    ))]
    #[diagnostic(code("unused::import::module"))]
    UnusedImportedModule {
        #[label("unused module")]
        location: Span,
        name: String,
    },

    #[error(
        "I discovered an unused imported value: {}",
        name.if_supports_color(Stderr, |s| s.default_color()),
    )]
    #[diagnostic(help(
        "No big deal, but you might want to remove it to get rid of that warning."
    ))]
    #[diagnostic(code("unused:import::value"))]
    UnusedImportedValueOrType {
        #[label("unused import")]
        location: Span,
        name: String,
    },

    #[error(
        "I found an unused private function: {}",
        name.if_supports_color(Stderr, |s| s.default_color()),
    )]
    #[diagnostic(help(
        "Perhaps your forgot to make it public using the {keyword_pub} keyword?\n\
         Otherwise, you might want to get rid of it altogether.",
         keyword_pub = "pub".if_supports_color(Stderr, |s| s.bright_blue())
    ))]
    #[diagnostic(code("unused::function"))]
    UnusedPrivateFunction {
        #[label("unused (private) function")]
        location: Span,
        name: String,
    },

    #[error(
        "I found an unused (private) module constant: {}",
        name.if_supports_color(Stderr, |s| s.default_color()),
    )]
    #[diagnostic(help(
        "Perhaps your forgot to make it public using the {keyword_pub} keyword?\n\
         Otherwise, you might want to get rid of it altogether.",
         keyword_pub = "pub".if_supports_color(Stderr, |s| s.bright_blue())
    ))]
    #[diagnostic(code("unused::constant"))]
    UnusedPrivateModuleConstant {
        #[label("unused (private) constant")]
        location: Span,
        name: String,
    },

    #[error(
        "I discovered an unused type: {}",
        name
            .if_supports_color(Stderr, |s| s.bright_blue())
            .if_supports_color(Stderr, |s| s.bold())
    )]
    #[diagnostic(code("unused::type"))]
    UnusedType {
        #[label("unused (private) type")]
        location: Span,
        name: String,
    },

    #[error(
        "I came across an unused variable: {}",
        name.if_supports_color(Stderr, |s| s.default_color()),
    )]
    #[diagnostic(help("{}", formatdoc! {
        r#"No big deal, but you might want to remove it or use a discard {name} to get rid of that warning.

           You should also know that, unlike in typical imperative languages, unused let-bindings are {fully_ignored} in Aiken.
           They will not produce any side-effect (such as error calls). Programs with or without unused variables are semantically equivalent.

           If you do want to enforce some side-effects, use {keyword_expect} with a discard {name} instead of {keyword_let}.
        "#,
        fully_ignored = "fully_ignored".if_supports_color(Stderr, |s| s.bold()),
        keyword_expect = "expect".if_supports_color(Stderr, |s| s.yellow()),
        keyword_let = "let".if_supports_color(Stderr, |s| s.yellow()),
        name = format!("_{name}").if_supports_color(Stderr, |s| s.yellow())
    }))]
    #[diagnostic(code("unused::variable"))]
    UnusedVariable {
        #[label("unused identifier")]
        location: Span,
        name: String,
    },

    #[error(
        "I found an {} {}",
        "if/is".if_supports_color(Stderr, |s| s.purple()),
        "that checks an expression with a known type.".if_supports_color(Stderr, |s| s.yellow())
    )]
    #[diagnostic(
        code("if_is_on_non_data"),
        help(
            "Prefer using a {} to match on all known constructors.",
            "when/is".if_supports_color(Stderr, |s| s.purple())
        )
    )]
    UseWhenInstead {
        #[label(
            "use {}",
            "when/is".if_supports_color(Stderr, |s| s.purple())
        )]
        location: Span,
    },

    #[error(
        "I came across a discarded variable in a let assignment: {}",
        name.if_supports_color(Stderr, |s| s.default_color())
    )]
    #[diagnostic(help("{}", formatdoc! {
        r#"If you do want to enforce some side-effects, use {keyword_expect} with {name} instead of {keyword_let}.

           You should also know that, unlike in typical imperative languages, unused let-bindings are {fully_ignored} in Aiken.
           They will not produce any side-effect (such as error calls). Programs with or without unused variables are semantically equivalent.
        "#,
        fully_ignored = "fully_ignored".if_supports_color(Stderr, |s| s.bold()),
        keyword_expect = "expect".if_supports_color(Stderr, |s| s.yellow()),
        keyword_let = "let".if_supports_color(Stderr, |s| s.yellow()),
        name = name.if_supports_color(Stderr, |s| s.yellow())
    }))]
    #[diagnostic(code("unused::discarded_let_assignment"))]
    DiscardedLetAssignment {
        #[label("discarded result")]
        location: Span,
        name: String,
    },

    #[error(
        "I came across a validator in a {} {}",
        "lib/".if_supports_color(Stderr, |s| s.purple()),
        "module which means I'm going to ignore it.".if_supports_color(Stderr, |s| s.yellow()),
    )]
    #[diagnostic(help(
        "No big deal, but you might want to move it to the {} folder or remove it to get rid of that warning.",
        "validators".if_supports_color(Stderr, |s| s.purple()),
    ))]
    #[diagnostic(code("unused::validator"))]
    ValidatorInLibraryModule {
        #[label("ignored")]
        location: Span,
    },

    #[error(
        "I noticed a suspicious {type_ByteArray} {tail}",
        type_ByteArray = "ByteArray"
            .if_supports_color(Stderr, |s| s.bright_blue())
            .if_supports_color(Stderr, |s| s.bold()),
        tail = "UTF-8 literal which resembles a hash digest.".if_supports_color(Stderr, |s| s.yellow()),
    )]
    #[diagnostic(help("{}", formatdoc! {
        r#"When you specify a {type_ByteArray} literal using plain double-quotes, it's interpreted as an array of UTF-8 bytes. For example, the literal {literal_foo} is interpreted as the byte sequence {foo_bytes}.

           However here, you have specified a literal that resembles a hash digest encoded as an hexadecimal string. This is a common case, but you probably want to capture the raw bytes represented by this sequence, and not the hexadecimal sequence. Fear not! Aiken provides a convenient syntax for that: just prefix the literal with {symbol_hash}. This will decode the hexadecimal string for you and capture the non-encoded bytes as a {type_ByteArray}.

           ╰─▶ {symbol_hash}{value}
        "#,
        type_ByteArray = "ByteArray"
            .if_supports_color(Stderr, |s| s.bright_blue())
            .if_supports_color(Stderr, |s| s.bold()),
        literal_foo = "\"foo\"".if_supports_color(Stderr, |s| s.purple()),
        foo_bytes = "#[102, 111, 111]".if_supports_color(Stderr, |s| s.purple()),
        value = format!("\"{value}\"").if_supports_color(Stderr, |s| s.purple()),
        symbol_hash = "#".if_supports_color(Stderr, |s| s.purple()),
    }))]
    #[diagnostic(code("syntax::bytearray_literal_is_hex_string"))]
    #[diagnostic(url("https://aiken-lang.org/language-tour/primitive-types#bytearray"))]
    Utf8ByteArrayIsValidHexString {
        #[label("missing '#' to decode hex string")]
        location: Span,
        value: String,
    },
}

impl ExtraData for Warning {
    fn extra_data(&self) -> Option<String> {
        match self {
            Warning::AllFieldsRecordUpdate { .. }
            | Warning::ImplicitlyDiscardedResult { .. }
            | Warning::NoFieldsRecordUpdate { .. }
            | Warning::SingleConstructorExpect { .. }
            | Warning::SingleWhenClause { .. }
            | Warning::Todo { .. }
            | Warning::UnexpectedTypeHole { .. }
            | Warning::UnusedConstructor { .. }
            | Warning::UnusedPrivateFunction { .. }
            | Warning::UnusedPrivateModuleConstant { .. }
            | Warning::UnusedType { .. }
            | Warning::UnusedVariable { .. }
            | Warning::DiscardedLetAssignment { .. }
            | Warning::ValidatorInLibraryModule { .. }
            | Warning::UseWhenInstead { .. } => None,
            Warning::Utf8ByteArrayIsValidHexString { value, .. } => Some(value.clone()),
            Warning::UnusedImportedModule { location, .. } => {
                Some(format!("{},{}", false, location.start))
            }
            Warning::UnusedImportedValueOrType { location, .. } => {
                Some(format!("{},{}", true, location.start))
            }
        }
    }
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

    FuzzerAnnotationMismatch,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnknownRecordFieldSituation {
    /// This unknown record field is being called as a function. i.e. `record.field()`
    FunctionCall,
}

pub fn format_suggestion(sample: &UntypedExpr) -> String {
    Formatter::new()
        .expr(sample, false)
        .to_pretty_string(70)
        .lines()
        .enumerate()
        .map(|(ix, line)| {
            if ix == 0 {
                format!("╰─▶ {line}")
            } else {
                format!("    {line}")
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}
