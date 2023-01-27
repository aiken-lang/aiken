use super::schema;
use crate::module::CheckedModule;
use aiken_lang::ast::{Span, TypedFunction};
use miette::{Diagnostic, NamedSource};
use std::{fmt::Debug, path::PathBuf};

#[derive(Debug, thiserror::Error, Diagnostic)]
pub enum Error {
    #[error("Validator functions must return Bool")]
    ValidatorMustReturnBool {
        path: PathBuf,
        src: String,
        named: NamedSource,
        location: Span,
    },
    #[error("Validator\n\n{name}\n\nrequires at least {at_least} arguments")]
    WrongValidatorArity {
        name: String,
        at_least: u8,
        location: Span,
        path: PathBuf,
        src: String,
        named: NamedSource,
    },
    #[error(transparent)]
    Schema(schema::Error),
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
