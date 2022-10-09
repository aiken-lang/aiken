use std::{io, path::PathBuf};

use aiken_lang::error::ParseError;

#[allow(dead_code)]
#[derive(Debug, thiserror::Error, miette::Diagnostic)]
pub enum Error {
    #[error("file operation failed")]
    FileIo { path: PathBuf, error: io::Error },
    #[error("failed to parse Aiken source code")]
    Parse {
        path: PathBuf,
        src: String,
        error: Box<ParseError>,
    },
    #[error("list of errors")]
    List(Vec<Self>),
}
