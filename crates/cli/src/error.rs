use std::{
    fmt::{Debug, Display},
    io,
    path::PathBuf,
};

use aiken_lang::error::ParseError;
use miette::{EyreContext, LabeledSpan, MietteHandlerOpts, RgbColors, SourceCode};

#[allow(dead_code)]
#[derive(thiserror::Error)]
pub enum Error {
    #[error("file operation failed")]
    FileIo { path: PathBuf, error: io::Error },
    #[error("failed to parse")]
    Parse {
        path: PathBuf,

        src: String,

        #[source]
        error: Box<ParseError>,
    },
    /// Useful for returning many [`Error::Parse`] at once
    #[error("a list of errors")]
    List(Vec<Self>),
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
            Error::Parse { .. } => Some(Box::new("aiken::parser".to_string())),
            Error::FileIo { .. } => None,
            Error::List(_) => None,
        }
    }

    fn source_code(&self) -> Option<&dyn SourceCode> {
        match self {
            Error::Parse { src, .. } => Some(src),
            Error::FileIo { .. } => None,
            Error::List(_) => None,
        }
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        match self {
            Error::Parse { error, .. } => error.labels(),
            Error::FileIo { .. } => None,
            Error::List(_) => None,
        }
    }

    fn help<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        match self {
            Error::Parse { error, .. } => error.kind.help(),
            Error::FileIo { .. } => None,
            Error::List(_) => None,
        }
    }
}
