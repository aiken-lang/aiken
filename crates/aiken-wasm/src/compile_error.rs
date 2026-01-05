use aiken_lang::{ast::Span, parser, tipo};
use miette::Diagnostic;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ErrorKind {
    ParseError,
    TypeCheckError,
}

#[wasm_bindgen]
pub struct CompileError {
    kind: ErrorKind,
    message: String,
    help: Option<String>,
    location: Span,
}

impl From<tipo::error::Error> for CompileError {
    fn from(error: tipo::error::Error) -> Self {
        Self {
            kind: ErrorKind::TypeCheckError,
            message: error.to_string(),
            help: error.help().map(|msg| msg.to_string()),
            location: error.location(),
        }
    }
}

impl From<&parser::error::ParseError> for CompileError {
    fn from(error: &parser::error::ParseError) -> Self {
        Self {
            kind: ErrorKind::ParseError,
            message: error.to_string(),
            help: error.help().map(|msg| msg.to_string()),
            location: error.span,
        }
    }
}

impl From<Vec<parser::error::ParseError>> for CompileError {
    fn from(errors: Vec<parser::error::ParseError>) -> Self {
        CompileError::from(errors.first().expect("empty array of CompileError"))
    }
}

#[wasm_bindgen]
impl CompileError {
    #[wasm_bindgen(getter)]
    pub fn kind(&self) -> ErrorKind {
        self.kind
    }

    #[wasm_bindgen(getter)]
    pub fn message(&self) -> String {
        self.message.clone()
    }

    #[wasm_bindgen(getter)]
    pub fn help(&self) -> Option<String> {
        self.help.clone()
    }

    #[wasm_bindgen(getter)]
    pub fn location(&self) -> Span {
        self.location
    }
}
