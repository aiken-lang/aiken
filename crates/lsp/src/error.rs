use std::io;

use crossbeam_channel::SendError;
use lsp_server::{ExtractError, Message};

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
pub enum Error {
    #[error(transparent)]
    #[diagnostic(code(aiken::lsp::server_capabilities))]
    ServerCapabilities(#[from] serde_json::Error),
    #[error(transparent)]
    #[diagnostic(code(aiken::lsp::server_init))]
    ServerInit(#[from] lsp_server::ProtocolError),
    #[error(transparent)]
    #[diagnostic(code(aiken::lsp::io))]
    Io(#[from] io::Error),
    #[error("Unsupported LSP request: {request}")]
    #[diagnostic(code(aiken::lsp::unsupported_lsp_request))]
    UnsupportedLspRequest { request: String },
    #[error(transparent)]
    #[diagnostic(code(aiken::lsp::cast_request))]
    CastRequest(#[from] ExtractError<lsp_server::Request>),
    #[error(transparent)]
    #[diagnostic(code(aiken::lsp::cast_notification))]
    CastNotification(#[from] ExtractError<lsp_server::Notification>),
    #[error(transparent)]
    #[diagnostic(code(aiken::lsp::send))]
    Send(#[from] SendError<Message>),
    #[error(transparent)]
    #[diagnostic(code(aiken::lsp::send))]
    PathToUri(#[from] url::ParseError),
}
