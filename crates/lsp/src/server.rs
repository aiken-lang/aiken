use std::{collections::HashMap, fs, path::Path};

use aiken_lang::{ast::ModuleKind, parser};
use aiken_project::config;
use lsp_server::{Connection, Message};
use lsp_types::{
    notification::{DidChangeTextDocument, Notification},
    request::{Formatting, Request},
    DocumentFormattingParams, InitializeParams, TextEdit,
};

use crate::error::Error;

#[allow(dead_code)]
pub struct Server {
    config: Option<config::Config>,

    /// Files that have been edited in memory
    edited: HashMap<String, String>,

    initialize_params: InitializeParams,
}

impl Server {
    pub fn new(initialize_params: InitializeParams, config: Option<config::Config>) -> Self {
        Self {
            config,
            edited: HashMap::new(),
            initialize_params,
        }
    }

    pub fn listen(&mut self, connection: Connection) -> Result<(), Error> {
        for msg in &connection.receiver {
            tracing::debug!("Got message: {:#?}", msg);

            match msg {
                Message::Request(req) => {
                    if connection.handle_shutdown(&req)? {
                        return Ok(());
                    }

                    tracing::debug!("Get request: {:#?}", req);

                    let response = self.handle_request(req)?;

                    connection.sender.send(Message::Response(response))?;
                }
                Message::Response(_) => todo!(),
                Message::Notification(notification) => {
                    self.handle_notification(&connection, notification)?
                }
            }
        }

        Ok(())
    }

    fn handle_request(
        &mut self,
        request: lsp_server::Request,
    ) -> Result<lsp_server::Response, Error> {
        let id = request.id.clone();

        match request.method.as_str() {
            Formatting::METHOD => {
                let params = cast_request::<Formatting>(request)?;

                let result = self.format(params);

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

    fn handle_notification(
        &mut self,
        _connection: &lsp_server::Connection,
        notification: lsp_server::Notification,
    ) -> Result<(), Error> {
        match notification.method.as_str() {
            DidChangeTextDocument::METHOD => {
                let params = cast_notification::<DidChangeTextDocument>(notification)?;

                // A file has changed in the editor so store a copy of the new content in memory
                let path = params.text_document.uri.path().to_string();

                if let Some(changes) = params.content_changes.into_iter().next() {
                    let _ = self.edited.insert(path, changes.text);
                }

                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn format(
        &mut self,
        params: DocumentFormattingParams,
    ) -> Result<Vec<TextEdit>, aiken_project::error::Error> {
        let path = params.text_document.uri.path();
        let mut new_text = String::new();

        match self.edited.get(path) {
            Some(src) => {
                let (module, extra) = parser::module(src, ModuleKind::Lib).map_err(|errs| {
                    aiken_project::error::Error::from_parse_errors(errs, Path::new(path), src)
                })?;

                aiken_lang::format::pretty(&mut new_text, module, extra, src);
            }
            None => {
                let src = fs::read_to_string(path)?;

                let (module, extra) = parser::module(&src, ModuleKind::Lib).map_err(|errs| {
                    aiken_project::error::Error::from_parse_errors(errs, Path::new(path), &src)
                })?;

                aiken_lang::format::pretty(&mut new_text, module, extra, &src);
            }
        }

        Ok(vec![text_edit_replace(new_text)])
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

fn cast_notification<N>(notification: lsp_server::Notification) -> Result<N::Params, Error>
where
    N: lsp_types::notification::Notification,
    N::Params: serde::de::DeserializeOwned,
{
    let params = notification.extract::<N::Params>(N::METHOD)?;

    Ok(params)
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
