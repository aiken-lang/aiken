use super::schema;
use crate::module::CheckedModule;
use aiken_lang::ast::{Span, TypedFunction};
use miette::{Diagnostic, NamedSource};
use owo_colors::OwoColorize;
use std::{fmt::Debug, path::PathBuf};

#[derive(Debug, thiserror::Error, Diagnostic)]
pub enum Error {
    #[error("A validator functions must return Bool")]
    #[diagnostic(code("aiken::blueprint::invalid::return_type"))]
    ValidatorMustReturnBool {
        path: PathBuf,
        src: String,
        #[source_code]
        named: NamedSource,
        #[label("invalid return type")]
        location: Span,
    },
    #[error("A {} validator requires at least {at_least} arguments", name.purple().bold())]
    #[diagnostic(code("aiken::blueprint::invalid::arity"))]
    WrongValidatorArity {
        name: String,
        at_least: u8,
        #[label("not enough arguments")]
        location: Span,
        path: PathBuf,
        src: String,
        #[source_code]
        named: NamedSource,
    },
    #[error(transparent)]
    #[diagnostic(transparent)]
    Schema(#[diagnostic_source] schema::Error),
}

pub fn assert_return_bool(module: &CheckedModule, def: &TypedFunction) -> Result<(), Error> {
    if !def.return_type.is_bool() {
        Err(Error::ValidatorMustReturnBool {
            location: def.location,
            src: module.code.clone(),
            path: module.input_path.clone(),
            named: NamedSource::new(module.input_path.display().to_string(), module.code.clone()),
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
            location: def.location,
            src: module.code.clone(),
            path: module.input_path.clone(),
            named: NamedSource::new(module.input_path.display().to_string(), module.code.clone()),
            name: def.name.clone(),
            at_least,
        })
    } else {
        Ok(())
    }
}
