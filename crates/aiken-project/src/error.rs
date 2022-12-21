use crate::{pretty, script::EvalHint};
use aiken_lang::{
    ast::{BinOp, Span},
    parser::error::ParseError,
    tipo,
};
use miette::{
    Diagnostic, EyreContext, LabeledSpan, MietteHandlerOpts, NamedSource, RgbColors, SourceCode,
};
use std::{
    fmt::{Debug, Display},
    io,
    path::{Path, PathBuf},
};
use uplc::machine::cost_model::ExBudget;
use zip_extract::ZipExtractError;

#[allow(dead_code)]
#[derive(thiserror::Error)]
pub enum Error {
    #[error("Duplicate module\n\n{module}")]
    DuplicateModule {
        module: String,
        first: PathBuf,
        second: PathBuf,
    },

    #[error("File operation failed")]
    FileIo { error: io::Error, path: PathBuf },

    #[error("Source code incorrectly formatted")]
    Format { problem_files: Vec<Unformatted> },

    #[error(transparent)]
    StandardIo(#[from] io::Error),

    #[error(transparent)]
    Http(#[from] reqwest::Error),

    #[error(transparent)]
    ZipExtract(#[from] ZipExtractError),

    #[error(transparent)]
    JoinError(#[from] tokio::task::JoinError),

    #[error("{help}")]
    TomlLoading {
        path: PathBuf,
        src: String,
        named: NamedSource,
        location: Option<Span>,
        help: String,
    },

    #[error("Missing 'aiken.toml' manifest in {path}")]
    MissingManifest { path: PathBuf },

    #[error("Cyclical module imports")]
    ImportCycle { modules: Vec<String> },

    /// Useful for returning many [`Error::Parse`] at once
    #[error("A list of errors")]
    List(Vec<Self>),

    #[error("Parsing")]
    Parse {
        path: PathBuf,

        src: String,

        named: NamedSource,

        #[source]
        error: Box<ParseError>,
    },

    #[error("Checking")]
    Type {
        path: PathBuf,
        src: String,
        named: NamedSource,
        #[source]
        error: tipo::error::Error,
    },

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

    #[error("{name} failed{}", if *verbose { format!("\n{src}") } else { String::new() } )]
    TestFailure {
        name: String,
        path: PathBuf,
        verbose: bool,
        src: String,
        evaluation_hint: Option<EvalHint>,
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
                named: NamedSource::new(path.display().to_string(), src.to_string()),
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

    pub fn path(&self) -> Option<PathBuf> {
        match self {
            Error::DuplicateModule { second, .. } => Some(second.to_path_buf()),
            Error::FileIo { .. } => None,
            Error::Format { .. } => None,
            Error::StandardIo(_) => None,
            Error::MissingManifest { path } => Some(path.to_path_buf()),
            Error::TomlLoading { path, .. } => Some(path.to_path_buf()),
            Error::ImportCycle { .. } => None,
            Error::List(_) => None,
            Error::Parse { path, .. } => Some(path.to_path_buf()),
            Error::Type { path, .. } => Some(path.to_path_buf()),
            Error::ValidatorMustReturnBool { path, .. } => Some(path.to_path_buf()),
            Error::WrongValidatorArity { path, .. } => Some(path.to_path_buf()),
            Error::TestFailure { path, .. } => Some(path.to_path_buf()),
            Error::Http(_) => None,
            Error::ZipExtract(_) => None,
            Error::JoinError(_) => None,
        }
    }

    pub fn src(&self) -> Option<String> {
        match self {
            Error::DuplicateModule { .. } => None,
            Error::FileIo { .. } => None,
            Error::Format { .. } => None,
            Error::StandardIo(_) => None,
            Error::MissingManifest { .. } => None,
            Error::TomlLoading { src, .. } => Some(src.to_string()),
            Error::ImportCycle { .. } => None,
            Error::List(_) => None,
            Error::Parse { src, .. } => Some(src.to_string()),
            Error::Type { src, .. } => Some(src.to_string()),
            Error::ValidatorMustReturnBool { src, .. } => Some(src.to_string()),
            Error::WrongValidatorArity { src, .. } => Some(src.to_string()),
            Error::TestFailure { .. } => None,
            Error::Http(_) => None,
            Error::ZipExtract(_) => None,
            Error::JoinError(_) => None,
        }
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
    fn severity(&self) -> Option<miette::Severity> {
        Some(miette::Severity::Error)
    }

    fn code<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        match self {
            Error::DuplicateModule { .. } => Some(Box::new("aiken::module::duplicate")),
            Error::FileIo { .. } => None,
            Error::ImportCycle { .. } => Some(Box::new("aiken::module::cyclical")),
            Error::List(_) => None,
            Error::Parse { .. } => Some(Box::new("aiken::parser")),
            Error::Type { .. } => Some(Box::new("aiken::check")),
            Error::StandardIo(_) => None,
            Error::MissingManifest { .. } => None,
            Error::TomlLoading { .. } => Some(Box::new("aiken::loading::toml")),
            Error::Format { .. } => None,
            Error::ValidatorMustReturnBool { .. } => Some(Box::new("aiken::scripts")),
            Error::WrongValidatorArity { .. } => Some(Box::new("aiken::validators")),
            Error::TestFailure { path, .. } => Some(Box::new(path.to_str().unwrap_or(""))),
            Error::Http(_) => Some(Box::new("aiken::deps")),
            Error::ZipExtract(_) => None,
            Error::JoinError(_) => None,
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
            Error::MissingManifest { .. } => Some(Box::new("Try running `aiken new <REPOSITORY/PROJECT>` to initialise a project with an example manifest.")),
            Error::TomlLoading { .. } => None,
            Error::Format { .. } => None,
            Error::ValidatorMustReturnBool { .. } => Some(Box::new("Try annotating the validator's return type with Bool")),
            Error::WrongValidatorArity { .. } => Some(Box::new("Validators require a minimum number of arguments please add the missing arguments.\nIf you don't need one of the required arguments use an underscore `_datum`.")),
            Error::TestFailure { evaluation_hint, .. }  =>{
                match evaluation_hint {
                    None => None,
                    Some(hint) => {
                        let budget = ExBudget { mem: i64::MAX, cpu: i64::MAX, };
                        let left = pretty::boxed("left", &match hint.left.eval(budget) {
                            (Ok(term), _, _) => format!("{term}"),
                            (Err(err), _, _) => format!("{err}"),
                        });
                        let right = pretty::boxed("right", &match hint.right.eval(budget) {
                            (Ok(term), _, _) => format!("{term}"),
                            (Err(err), _, _) => format!("{err}"),
                        });
                        let msg = match hint.bin_op {
                            BinOp::And => Some(format!("{left}\n\nand\n\n{right}\n\nshould both be true.")),
                            BinOp::Or => Some(format!("{left}\n\nor\n\n{right}\n\nshould be true.")),
                            BinOp::Eq => Some(format!("{left}\n\nshould be equal to\n\n{right}")),
                            BinOp::NotEq => Some(format!("{left}\n\nshould not be equal to\n\n{right}")),
                            BinOp::LtInt => Some(format!("{left}\n\nshould be lower than\n\n{right}")),
                            BinOp::LtEqInt => Some(format!("{left}\n\nshould be lower than or equal to\n\n{right}")),
                            BinOp::GtEqInt => Some(format!("{left}\n\nshould be greater than\n\n{right}")),
                            BinOp::GtInt => Some(format!("{left}\n\nshould be greater than or equal to\n\n{right}")),
                            _ => None
                        }?;
                        Some(Box::new(msg))
                    }
                }
            },
            Error::Http(_) => None,
            Error::ZipExtract(_) => None,
            Error::JoinError(_) => None,
        }
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        match self {
            Error::DuplicateModule { .. } => None,
            Error::FileIo { .. } => None,
            Error::ImportCycle { .. } => None,
            Error::List(_) => None,
            Error::Parse { error, .. } => error.labels(),
            Error::MissingManifest { .. } => None,
            Error::Type { error, .. } => error.labels(),
            Error::StandardIo(_) => None,
            Error::TomlLoading { location, .. } => {
                if let Some(location) = location {
                    Some(Box::new(
                        vec![LabeledSpan::new_with_span(None, *location)].into_iter(),
                    ))
                } else {
                    None
                }
            }
            Error::Format { .. } => None,
            Error::ValidatorMustReturnBool { location, .. } => Some(Box::new(
                vec![LabeledSpan::new_with_span(None, *location)].into_iter(),
            )),
            Error::WrongValidatorArity { location, .. } => Some(Box::new(
                vec![LabeledSpan::new_with_span(None, *location)].into_iter(),
            )),
            Error::TestFailure { .. } => None,
            Error::Http(_) => None,
            Error::ZipExtract(_) => None,
            Error::JoinError(_) => None,
        }
    }

    fn source_code(&self) -> Option<&dyn SourceCode> {
        match self {
            Error::DuplicateModule { .. } => None,
            Error::FileIo { .. } => None,
            Error::ImportCycle { .. } => None,
            Error::List(_) => None,
            Error::Parse { named, .. } => Some(named),
            Error::Type { named, .. } => Some(named),
            Error::StandardIo(_) => None,
            Error::MissingManifest { .. } => None,
            Error::TomlLoading { named, .. } => Some(named),
            Error::Format { .. } => None,
            Error::ValidatorMustReturnBool { named, .. } => Some(named),
            Error::WrongValidatorArity { named, .. } => Some(named),
            Error::TestFailure { .. } => None,
            Error::Http(_) => None,
            Error::ZipExtract(_) => None,
            Error::JoinError(_) => None,
        }
    }
}

#[derive(thiserror::Error)]
pub enum Warning {
    #[error("Checking")]
    Type {
        path: PathBuf,
        src: String,
        named: NamedSource,
        #[source]
        warning: tipo::error::Warning,
    },
}

impl Diagnostic for Warning {
    fn severity(&self) -> Option<miette::Severity> {
        Some(miette::Severity::Warning)
    }

    fn source_code(&self) -> Option<&dyn SourceCode> {
        match self {
            Warning::Type { named, .. } => Some(named),
        }
    }
    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        match self {
            Warning::Type { warning, .. } => warning.labels(),
        }
    }

    fn code<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        match self {
            Warning::Type { .. } => Some(Box::new("aiken::check")),
        }
    }
}

impl Warning {
    pub fn from_type_warning(warning: tipo::error::Warning, path: PathBuf, src: String) -> Warning {
        Warning::Type {
            path: path.clone(),
            warning,
            src: src.clone(),
            named: NamedSource::new(path.display().to_string(), src),
        }
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
