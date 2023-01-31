use super::schema;
use crate::module::CheckedModule;
use aiken_lang::{
    ast::{Span, TypedFunction},
    tipo::Type,
};
use miette::{Diagnostic, NamedSource};
use owo_colors::OwoColorize;
use std::{fmt::Debug, sync::Arc};

#[derive(Debug, thiserror::Error, Diagnostic)]
pub enum Error {
    #[error("A validator must return {}", "Bool".bright_blue().bold())]
    #[diagnostic(code("aiken::blueprint::invalid::return_type"))]
    #[diagnostic(help(r#"While analyzing the return type of your validator, I found it to be:

╰─▶ {signature}

...but I expected this to be a {type_Bool}. If I am inferring the wrong type, you may want to add a type annotation to the function."#
        , type_Bool = "Bool".bright_blue().bold()
        , signature = return_type.to_pretty(0).red()
    ))]
    ValidatorMustReturnBool {
        #[label("invalid return type")]
        location: Span,
        #[source_code]
        source_code: NamedSource,
        return_type: Arc<Type>,
    },
    #[error("A {} validator requires at least {} arguments.", name.purple().bold(), at_least.to_string().purple().bold())]
    #[diagnostic(code("aiken::blueprint::invalid::arity"))]
    WrongValidatorArity {
        name: String,
        at_least: u8,
        #[label("not enough arguments")]
        location: Span,
        #[source_code]
        source_code: NamedSource,
    },
    #[error("{}", error)]
    #[diagnostic(help("{}", error.help()))]
    #[diagnostic(code("aiken::blueprint::interface"))]
    Schema {
        error: schema::Error,
        #[label("invalid contract's boundary")]
        location: Span,
        #[source_code]
        source_code: NamedSource,
    },

    #[error("Invalid or missing project's blueprint file.")]
    #[diagnostic(code("aiken::blueprint::missing"))]
    #[diagnostic(help("Did you forget to {build} the project?", build = "build".purple().bold()))]
    InvalidOrMissingFile,
}

pub fn assert_return_bool(module: &CheckedModule, def: &TypedFunction) -> Result<(), Error> {
    if !def.return_type.is_bool() {
        Err(Error::ValidatorMustReturnBool {
            return_type: def.return_type.clone(),
            location: def.location,
            source_code: NamedSource::new(
                module.input_path.display().to_string(),
                module.code.clone(),
            ),
        })
    } else {
        Ok(())
    }
}

pub fn assert_min_arity(
    module: &CheckedModule,
    def: &TypedFunction,
    at_least: u8,
) -> Result<(), Error> {
    if def.arguments.len() < at_least as usize {
        Err(Error::WrongValidatorArity {
            name: def.name.clone(),
            at_least,
            location: def.location,
            source_code: NamedSource::new(
                module.input_path.display().to_string(),
                module.code.clone(),
            ),
        })
    } else {
        Ok(())
    }
}
