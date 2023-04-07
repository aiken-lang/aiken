use super::schema;
use aiken_lang::ast::Span;
use miette::{Diagnostic, NamedSource};
use owo_colors::{OwoColorize, Stream::Stdout};
use std::fmt::Debug;

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
        source_code: NamedSource,
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
        "I can only compute addresses of validators that are fully applied. For example, a {keyword_spend} validator must have exactly 3 arguments: a datum, a redeemer and a context. If it has more, they need to be provided beforehand and applied directly in the validator. Applying parameters change the validator's compiled code, and thus the address.\n\nThis is why I need you to apply parameters first.",
        keyword_spend = "spend".if_supports_color(Stdout, |s| s.purple())))]
    ParameterizedValidator { n: usize },
}

unsafe impl Send for Error {}

unsafe impl Sync for Error {}
