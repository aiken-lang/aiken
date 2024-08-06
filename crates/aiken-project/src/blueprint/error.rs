use super::{
    definitions::Reference,
    schema::{self, Schema},
};
use aiken_lang::ast::Span;
use miette::{Diagnostic, NamedSource};
use owo_colors::{OwoColorize, Stream::Stdout};
use pallas_codec::minicbor as cbor;
use std::fmt::Debug;
use uplc::ast::Constant;

#[derive(Debug, thiserror::Error, Diagnostic)]
pub enum Error {
    #[error("{}", error)]
    #[diagnostic(help("{}", error.help()))]
    #[diagnostic(code("aiken::blueprint::interface"))]
    Schema {
        error: schema::Error,
        #[label("invalid validator's boundary")]
        location: Span,
        #[source_code]
        source_code: NamedSource<String>,
    },

    #[error("Invalid or missing project's blueprint file.")]
    #[diagnostic(code("aiken::blueprint::missing"))]
    #[diagnostic(help(
        "Did you forget to {build} the project?",
        build = "build"
            .if_supports_color(Stdout, |s| s.purple())
            .if_supports_color(Stdout, |s| s.bold())
    ))]
    InvalidOrMissingFile,

    #[error("I didn't find any parameters to apply in the given validator.")]
    #[diagnostic(code("aiken::blueprint::apply::no_parameters"))]
    NoParametersToApply,

    #[error(
        "I couldn't compute the address of the given validator because it's parameterized by {} parameter(s)!",
        n.if_supports_color(Stdout, |s| s.purple())
    )]
    #[diagnostic(code("aiken::blueprint::address::parameterized"))]
    #[diagnostic(help(
        "I can only compute addresses of validators that are fully applied. For example, a {keyword_spend} validator must have exactly {spend_arity} arguments: a datum, a redeemer and a context. If it has more, they need to be provided beforehand and applied directly to the validator.\n\nApplying parameters change the validator's compiled code, and thus the address. This is why I need you to apply parameters first using the {blueprint_apply_command} command.",
        keyword_spend = "spend".if_supports_color(Stdout, |s| s.yellow()),
        spend_arity = "3".if_supports_color(Stdout, |s| s.yellow()),
        blueprint_apply_command = "blueprint apply".if_supports_color(Stdout, |s| s.purple()),
    ))]
    ParameterizedValidator { n: usize },

    #[error(
        "I couldn't compute the address of the given validator because it's actually a minting policy!"
    )]
    #[diagnostic(code("aiken::blueprint::address::minting_validator"))]
    #[diagnostic(help(
        "I can only compute addresses for spending validators. Did you mean to call {blueprint_policy_command} instead?",
        blueprint_policy_command = "blueprint policy".if_supports_color(Stdout, |s| s.purple()),
    ))]
    UnexpectedMintingValidator,

    #[error(
        "I couldn't compute the policyId of the given validator because it's actually a spending policy!"
    )]
    #[diagnostic(code("aiken::blueprint::address::spending_validator"))]
    #[diagnostic(help(
        "I can only compute policyIds for minting validators. Did you mean to call {blueprint_address_command} instead?",
        blueprint_address_command = "blueprint address".if_supports_color(Stdout, |s| s.purple()),
    ))]
    UnexpectedSpendingValidator,

    #[error("I stumble upon something else than a constant when I expected one.")]
    #[diagnostic(code("aiken:blueprint::apply::malformed::argument"))]
    #[diagnostic(help(
        "Parameters applied to blueprints must be constant; they cannot be lambdas or delayed terms."
    ))]
    NonConstantParameter,

    #[error("I couldn't find a definition corresponding to a reference.")]
    #[diagnostic(code("aiken::blueprint::apply::unknown::reference"))]
    #[diagnostic(help(
        "While resolving a schema definition, I stumbled upon an unknown reference:\n\n→ {reference}\n\nThis is unfortunate, but signals that either the reference is invalid or that the corresponding schema definition is missing. Double-check the blueprint for that reference or definition.",
        reference = reference.as_json_pointer().if_supports_color(Stdout, |s| s.red())
    ))]
    UnresolvedSchemaReference { reference: Reference },

    #[error("I caught a parameter application that seems off.")]
    #[diagnostic(code("aiken::blueprint::apply::mismatch"))]
    #[diagnostic(help(
        "When applying parameters to a validator, I control that the shape of the parameter you give me matches what is specified in the blueprint. Unfortunately, it didn't match in this case.\n\nI am looking at the following value:\n\n{term}\n\nbut failed to match it against the specified schema:\n\n{expected}\n\n\nNOTE: this may only represent part of a bigger whole as I am validating the parameter incrementally.",
        expected = serde_json::to_string_pretty(&schema).unwrap().if_supports_color(Stdout, |s| s.green()),
        term = {
            let mut buf = vec![];
            match term {
                Constant::Data(data) => {
                    cbor::encode(data, &mut buf).unwrap();
                    cbor::display(&buf).to_string()
                },
                _ => term.to_pretty()
            }
        }.if_supports_color(Stdout, |s| s.red()),
    ))]
    SchemaMismatch { schema: Schema, term: Constant },

    #[error(
        "I discovered a discrepancy of elements between a given tuple and its declared schema."
    )]
    #[diagnostic(code("aiken::blueprint::apply::tuple::mismatch"))]
    #[diagnostic(help(
        "When validating a list-like schema with multiple 'items' schemas, I try to match each element of the instance with each item schema (by their position). Hence, I expect to be as many items in the declared schema ({expected}) than there are items in the instance ({found}).",
        expected = expected.if_supports_color(Stdout, |s| s.green()),
        found = found.if_supports_color(Stdout, |s| s.red()),
    ))]
    TupleItemsMismatch { expected: usize, found: usize },

    #[error("I failed to convert some input into a valid parameter")]
    #[diagnostic(code("aiken::blueprint::parse::parameter"))]
    #[diagnostic(help("{hint}"))]
    MalformedParameter { hint: String },
}

unsafe impl Send for Error {}

unsafe impl Sync for Error {}
