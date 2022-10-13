use std::{
    fmt::{Debug, Display},
    io,
    path::PathBuf,
};

use aiken_lang::{error::ParseError, tipo};
use miette::{EyreContext, LabeledSpan, MietteHandlerOpts, RgbColors, SourceCode};

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

    #[error("cyclical module imports")]
    ImportCycle { modules: Vec<String> },

    /// Useful for returning many [`Error::Parse`] at once
    #[error("a list of errors")]
    List(Vec<Self>),

    #[error("failed to parse")]
    Parse {
        path: PathBuf,

        src: String,

        #[source]
        error: Box<ParseError>,
    },

    #[error("type checking failed")]
    Type {
        path: PathBuf,
        src: String,
        error: tipo::error::Error,
    },
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

impl miette::Diagnostic for Error {
    fn code<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        match self {
            Error::DuplicateModule { .. } => Some(Box::new("aiken::module::duplicate")),
            Error::FileIo { .. } => None,
            Error::ImportCycle { .. } => Some(Box::new("aiken::module::cyclical")),
            Error::List(_) => None,
            Error::Parse { .. } => Some(Box::new("aiken::parser")),
            Error::Type { .. } => Some(Box::new("aiken::typecheck")),
        }
    }

    fn help<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        match self {
            Error::DuplicateModule { first, second, .. } => Some(Box::new(format!(
                "rename either {} or {}",
                first.display(),
                second.display()
            ))),
            Error::FileIo { .. } => None,
            Error::ImportCycle { modules } => Some(Box::new(format!(
                "try moving the shared code to a separate module that the others can depend on\n- {}",
                modules.join("\n- ")
            ))),
            Error::List(_) => None,
            Error::Parse { error, .. } => error.kind.help(),
            Error::Type { .. } => None,
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
        }
    }
}
