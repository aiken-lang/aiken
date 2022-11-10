use std::{fs, io, path::Path};

use aiken_lang::{ast::ModuleKind, parser};
use crossbeam_channel::SendError;
use lsp_server::{Connection, ExtractError, Message};
use lsp_types::{
    request::{Formatting, Request},
    DocumentFormattingParams, InitializeParams, OneOf, ServerCapabilities, TextEdit,
};

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
    #[diagnostic(code(aiken::lsp::send))]
    Send(#[from] SendError<Message>),
}

pub fn start() -> Result<(), Error> {
    tracing::info!("Aiken language server starting");

    // Create the transport. Includes the stdio (stdin and stdout) versions but this could
    // also be implemented to use sockets or HTTP.
    let (connection, io_threads) = Connection::stdio();

    // Run the server and wait for the two threads to end (typically by trigger LSP Exit event).
    let server_capabilities = serde_json::to_value(&ServerCapabilities {
        definition_provider: Some(OneOf::Left(true)),
        document_formatting_provider: Some(OneOf::Left(true)),
        ..Default::default()
    })?;

    let initialization_params = connection.initialize(server_capabilities)?;
    let initialization_params = serde_json::from_value(initialization_params)?;

    main_loop(connection, initialization_params)?;

    io_threads.join()?;

    tracing::info!("Aiken language server shutting down");

    Ok(())
}

fn main_loop(connection: Connection, _params: InitializeParams) -> Result<(), Error> {
    for msg in &connection.receiver {
        tracing::debug!("Got message: {:#?}", msg);

        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }

                tracing::debug!("Get request: {:#?}", req);

                let response = handle_request(req)?;

                connection.sender.send(Message::Response(response))?;
            }
            Message::Response(_) => todo!(),
            Message::Notification(_) => todo!(),
        }
    }

    Ok(())
}

fn handle_request(request: lsp_server::Request) -> Result<lsp_server::Response, Error> {
    let id = request.id.clone();

    match request.method.as_str() {
        Formatting::METHOD => {
            let params = cast_request::<Formatting>(request)?;

            let result = format(params);

            match result {
                Ok(text_edit) => {
                    let result = serde_json::to_value(text_edit)?;

                    Ok(lsp_server::Response {
                        id,
                        error: None,
                        result: Some(result),
                    })
                }
                Err(_) => {
                    todo!("transform project errors in lsp diagnostic")
                }
            }
        }
        unsupported => Err(Error::UnsupportedLspRequest {
            request: unsupported.to_string(),
        }),
    }
}

fn cast_request<R>(request: lsp_server::Request) -> Result<R::Params, Error>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    let (_, params) = request.extract(R::METHOD)?;

    Ok(params)
}

fn format(params: DocumentFormattingParams) -> Result<Vec<TextEdit>, aiken_project::error::Error> {
    let path = params.text_document.uri.path();
    let mut new_text = String::new();

    let src = fs::read_to_string(path)?;

    let (module, extra) = parser::module(&src, ModuleKind::Lib).map_err(|errs| {
        aiken_project::error::Error::from_parse_errors(errs, Path::new(path), &src)
    })?;

    aiken_lang::format::pretty(&mut new_text, module, extra, &src);

    Ok(vec![text_edit_replace(new_text)])
}

fn text_edit_replace(new_text: String) -> TextEdit {
    TextEdit {
        range: lsp_types::Range {
            start: lsp_types::Position {
                line: 0,
                character: 0,
            },
            end: lsp_types::Position {
                line: u32::MAX,
                character: 0,
            },
        },
        new_text,
    }
}
