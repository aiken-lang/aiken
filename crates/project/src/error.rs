use std::{
    fmt::{Debug, Display},
    io,
    path::{Path, PathBuf},
};

use aiken_lang::{ast::Span, parser::error::ParseError, tipo};
use miette::{Diagnostic, EyreContext, LabeledSpan, MietteHandlerOpts, RgbColors, SourceCode};

#[allow(dead_code)]
#[derive(thiserror::Error)]
pub enum Error {
    #[error("duplicate module {module}")]
    DuplicateModule {
        module: String,
        first: PathBuf,
        second: PathBuf,
    },

    #[error("file operation failed")]
    FileIo { error: io::Error, path: PathBuf },

    #[error("source code incorrectly formatted")]
    Format { problem_files: Vec<Unformatted> },

    #[error(transparent)]
    StandardIo(#[from] io::Error),

    #[error("cyclical module imports")]
    ImportCycle { modules: Vec<String> },

    /// Useful for returning many [`Error::Parse`] at once
    #[error("a list of errors")]
    List(Vec<Self>),

    #[error("parsing")]
    Parse {
        path: PathBuf,

        src: String,

        #[source]
        error: Box<ParseError>,
    },

    #[error("type checking")]
    Type {
        path: PathBuf,
        src: String,
        #[source]
        error: tipo::error::Error,
    },

    #[error("validator functions must return Bool")]
    ValidatorMustReturnBool {
        path: PathBuf,
        src: String,
        location: Span,
    },

    #[error("{name} requires at least {at_least} arguments")]
    WrongValidatorArity {
        name: String,
        at_least: u8,
        location: Span,
        path: PathBuf,
        src: String,
    },
}

impl Error {
    pub fn len(&self) -> usize {
        match self {
            Error::List(errors) => errors.len(),
            _ => 1,
        }
    }

    pub fn report(&self) {
        match self {
            Error::List(errors) => {
                for error in errors {
                    eprintln!("Error: {:?}", error)
                }
            }
            rest => eprintln!("Error: {:?}", rest),
        }
    }

    pub fn from_parse_errors(errs: Vec<ParseError>, path: &Path, src: &str) -> Self {
        let mut errors = Vec::with_capacity(errs.len());

        for error in errs {
            errors.push(Error::Parse {
                path: path.into(),
                src: src.to_string(),
                error: error.into(),
            });
        }

        Error::List(errors)
    }

    pub fn append(self, next: Self) -> Self {
        match (self, next) {
            (Error::List(mut errors), Error::List(mut next_errors)) => {
                errors.append(&mut next_errors);

                Error::List(errors)
            }
            (Error::List(mut errors), rest) => {
                errors.push(rest);

                Error::List(errors)
            }
            (rest, Error::List(mut next_errors)) => {
                let mut errors = vec![rest];

                errors.append(&mut next_errors);

                Error::List(errors)
            }
            (error, next_error) => Error::List(vec![error, next_error]),
        }
    }

    pub fn is_empty(&self) -> bool {
        matches!(self, Error::List(errors) if errors.is_empty())
    }
}

impl Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let miette_handler = MietteHandlerOpts::new()
            // For better support of terminal themes use the ANSI coloring
            .rgb_colors(RgbColors::Never)
            // If ansi support is disabled in the config disable the eye-candy
            .color(true)
            .unicode(true)
            .terminal_links(true)
            .build();

        // Ignore error to prevent format! panics. This can happen if span points at some
        // inaccessible location, for example by calling `report_error()` with wrong working set.
        let _ = miette_handler.debug(self, f);

        Ok(())
    }
}

impl Diagnostic for Error {
    fn code<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        match self {
            Error::DuplicateModule { .. } => Some(Box::new("aiken::module::duplicate")),
            Error::FileIo { .. } => None,
            Error::ImportCycle { .. } => Some(Box::new("aiken::module::cyclical")),
            Error::List(_) => None,
            Error::Parse { .. } => Some(Box::new("aiken::parser")),
            Error::Type { .. } => Some(Box::new("aiken::typecheck")),
            Error::StandardIo(_) => None,
            Error::Format { .. } => None,
            Error::ValidatorMustReturnBool { .. } => Some(Box::new("aiken::scripts")),
            Error::WrongValidatorArity { .. } => Some(Box::new("aiken::validators")),
        }
    }

    fn help<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        match self {
            Error::DuplicateModule { first, second, .. } => Some(Box::new(format!(
                "Rename either {} or {}",
                first.display(),
                second.display()
            ))),
            Error::FileIo { .. } => None,
            Error::ImportCycle { modules } => Some(Box::new(format!(
                "Try moving the shared code to a separate module that the others can depend on\n- {}",
                modules.join("\n- ")
            ))),
            Error::List(_) => None,
            Error::Parse { error, .. } => error.kind.help(),
            Error::Type { error, .. } => error.help(),
            Error::StandardIo(_) => None,
            Error::Format { .. } => None,
            Error::ValidatorMustReturnBool { .. } => Some(Box::new("Try annotating the validator's return type with Bool")),
            Error::WrongValidatorArity { .. } => Some(Box::new("Validators require a minimum number of arguments please add the missing arguments.\nIf you don't need one of the required arguments use an underscore `_datum`.")),
        }
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        match self {
            Error::DuplicateModule { .. } => None,
            Error::FileIo { .. } => None,
            Error::ImportCycle { .. } => None,
            Error::List(_) => None,
            Error::Parse { error, .. } => error.labels(),
            Error::Type { error, .. } => error.labels(),
            Error::StandardIo(_) => None,
            Error::Format { .. } => None,
            Error::ValidatorMustReturnBool { location, .. } => Some(Box::new(
                vec![LabeledSpan::new_with_span(None, *location)].into_iter(),
            )),
            Error::WrongValidatorArity { location, .. } => Some(Box::new(
                vec![LabeledSpan::new_with_span(None, *location)].into_iter(),
            )),
        }
    }

    fn source_code(&self) -> Option<&dyn SourceCode> {
        match self {
            Error::DuplicateModule { .. } => None,
            Error::FileIo { .. } => None,
            Error::ImportCycle { .. } => None,
            Error::List(_) => None,
            Error::Parse { src, .. } => Some(src),
            Error::Type { src, .. } => Some(src),
            Error::StandardIo(_) => None,
            Error::Format { .. } => None,
            Error::ValidatorMustReturnBool { src, .. } => Some(src),
            Error::WrongValidatorArity { src, .. } => Some(src),
        }
    }
}

#[derive(PartialEq, thiserror::Error)]
pub enum Warning {
    #[error("type checking")]
    Type {
        path: PathBuf,
        src: String,
        #[source]
        warning: tipo::error::Warning,
    },
}

impl Diagnostic for Warning {
    fn source_code(&self) -> Option<&dyn SourceCode> {
        match self {
            Warning::Type { src, .. } => Some(src),
        }
    }
    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        match self {
            Warning::Type { warning, .. } => warning.labels(),
        }
    }

    fn code<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        match self {
            Warning::Type { .. } => Some(Box::new("aiken::typecheck")),
        }
    }
}

impl Warning {
    pub fn from_type_warning(warning: tipo::error::Warning, path: PathBuf, src: String) -> Warning {
        Warning::Type { path, warning, src }
    }

    pub fn report(&self) {
        eprintln!("Warning: {:?}", self)
    }
}

impl Debug for Warning {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let miette_handler = MietteHandlerOpts::new()
            // For better support of terminal themes use the ANSI coloring
            .rgb_colors(RgbColors::Never)
            // If ansi support is disabled in the config disable the eye-candy
            .color(true)
            .unicode(true)
            .terminal_links(true)
            .build();

        // Ignore error to prevent format! panics. This can happen if span points at some
        // inaccessible location, for example by calling `report_error()` with wrong working set.
        let _ = miette_handler.debug(self, f);

        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Unformatted {
    pub source: PathBuf,
    pub destination: PathBuf,
    pub input: String,
    pub output: String,
}
