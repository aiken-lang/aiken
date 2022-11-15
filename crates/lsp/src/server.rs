use std::{
    collections::{HashMap, HashSet},
    error::Error,
    fs,
    path::{Path, PathBuf},
};

use aiken_lang::{ast::ModuleKind, parser};
use aiken_project::{config, error::Error as ProjectError};
use lsp_server::{Connection, Message};
use lsp_types::{
    notification::{
        DidChangeTextDocument, DidSaveTextDocument, Notification, PublishDiagnostics, ShowMessage,
    },
    request::{Formatting, Request},
    DocumentFormattingParams, InitializeParams, TextEdit,
};
use miette::Diagnostic;

use crate::{error::Error as ServerError, line_numbers::LineNumbers};

#[allow(dead_code)]
pub struct Server {
    config: Option<config::Config>,

    /// Files that have been edited in memory
    edited: HashMap<String, String>,

    initialize_params: InitializeParams,

    /// Files for which there are active diagnostics
    published_diagnostics: HashSet<lsp_types::Url>,

    /// Diagnostics that have been emitted by the compiler but not yet published
    /// to the client
    stored_diagnostics: HashMap<PathBuf, Vec<lsp_types::Diagnostic>>,

    /// Diagnostics that have been emitted by the compiler but not yet published
    /// to the client. These are likely locationless Aiken diagnostics, as LSP
    /// diagnostics always need a location.
    stored_messages: Vec<lsp_types::ShowMessageParams>,
}

impl Server {
    pub fn new(initialize_params: InitializeParams, config: Option<config::Config>) -> Self {
        Self {
            config,
            edited: HashMap::new(),
            initialize_params,
            published_diagnostics: HashSet::new(),
            stored_diagnostics: HashMap::new(),
            stored_messages: Vec::new(),
        }
    }

    pub fn listen(&mut self, connection: Connection) -> Result<(), ServerError> {
        self.publish_stored_diagnostics(&connection)?;

        for msg in &connection.receiver {
            tracing::debug!("Got message: {:#?}", msg);

            match msg {
                Message::Request(req) => {
                    if connection.handle_shutdown(&req)? {
                        return Ok(());
                    }

                    tracing::debug!("Get request: {:#?}", req);

                    let response = self.handle_request(req)?;

                    self.publish_stored_diagnostics(&connection)?;

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
    ) -> Result<lsp_server::Response, ServerError> {
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
                    Err(err) => match err {
                        ProjectError::List(errors) => {
                            for error in errors {
                                if error.source_code().is_some() {
                                    self.process_diagnostic(error)?;
                                }
                            }

                            Ok(lsp_server::Response {
                                id,
                                error: None,
                                result: Some(serde_json::json!(null)),
                            })
                        }
                        error => {
                            if error.source_code().is_some() {
                                self.process_diagnostic(error)?;

                                Ok(lsp_server::Response {
                                    id,
                                    error: None,
                                    result: Some(serde_json::json!(null)),
                                })
                            } else {
                                Ok(lsp_server::Response {
                                    id,
                                    error: Some(lsp_server::ResponseError {
                                        code: 1, // We should assign a code to each error.
                                        message: format!("{:?}", error),
                                        data: None,
                                    }),
                                    result: None,
                                })
                            }
                        }
                    },
                }
            }
            unsupported => Err(ServerError::UnsupportedLspRequest {
                request: unsupported.to_string(),
            }),
        }
    }

    fn handle_notification(
        &mut self,
        _connection: &lsp_server::Connection,
        notification: lsp_server::Notification,
    ) -> Result<(), ServerError> {
        match notification.method.as_str() {
            DidSaveTextDocument::METHOD => {
                let params = cast_notification::<DidSaveTextDocument>(notification)?;

                self.edited.remove(params.text_document.uri.path());

                Ok(())
            }
            DidChangeTextDocument::METHOD => {
                let params = cast_notification::<DidChangeTextDocument>(notification)?;

                // A file has changed in the editor so store a copy of the new content in memory
                let path = params.text_document.uri.path().to_string();

                if let Some(changes) = params.content_changes.into_iter().next() {
                    self.edited.insert(path, changes.text);
                }

                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn format(&mut self, params: DocumentFormattingParams) -> Result<Vec<TextEdit>, ProjectError> {
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

    /// Publish all stored diagnostics to the client.
    /// Any previously publish diagnostics are cleared before the new set are
    /// published to the client.
    fn publish_stored_diagnostics(&mut self, connection: &Connection) -> Result<(), ServerError> {
        self.clear_all_diagnostics(connection)?;

        for (path, diagnostics) in self.stored_diagnostics.drain() {
            let uri = path_to_uri(path)?;

            // Record that we have published diagnostics to this file so we can
            // clear it later when they are outdated.
            self.published_diagnostics.insert(uri.clone());

            // Publish the diagnostics
            let params = lsp_types::PublishDiagnosticsParams {
                uri,
                diagnostics,
                version: None,
            };

            let notification = lsp_server::Notification {
                method: PublishDiagnostics::METHOD.to_string(),
                params: serde_json::to_value(params)?,
            };

            connection
                .sender
                .send(lsp_server::Message::Notification(notification))?;
        }

        for message in self.stored_messages.drain(..) {
            let notification = lsp_server::Notification {
                method: ShowMessage::METHOD.to_string(),
                params: serde_json::to_value(message)?,
            };

            connection
                .sender
                .send(lsp_server::Message::Notification(notification))?;
        }

        Ok(())
    }

    /// Clear all diagnostics that have been previously published to the client
    fn clear_all_diagnostics(&mut self, connection: &Connection) -> Result<(), ServerError> {
        for file in self.published_diagnostics.drain() {
            let params = lsp_types::PublishDiagnosticsParams {
                uri: file,
                diagnostics: vec![],
                version: None,
            };

            let notification = lsp_server::Notification {
                method: PublishDiagnostics::METHOD.to_string(),
                params: serde_json::to_value(params)?,
            };

            connection
                .sender
                .send(lsp_server::Message::Notification(notification))?;
        }

        Ok(())
    }

    /// Convert Aiken diagnostics into 1 or more LSP diagnostics and store them
    /// so that they can later be published to the client with
    /// `publish_stored_diagnostics`
    ///
    /// If the Aiken diagnostic cannot be converted to LSP diagnostic (due to it
    /// not having a location) it is stored as a message suitable for use with
    /// the `showMessage` notification instead.
    fn process_diagnostic(&mut self, error: ProjectError) -> Result<(), ServerError> {
        let (severity, typ) = match error.severity() {
            Some(severity) => match severity {
                miette::Severity::Error => (
                    lsp_types::DiagnosticSeverity::ERROR,
                    lsp_types::MessageType::ERROR,
                ),
                miette::Severity::Warning => (
                    lsp_types::DiagnosticSeverity::WARNING,
                    lsp_types::MessageType::WARNING,
                ),
                miette::Severity::Advice => (
                    lsp_types::DiagnosticSeverity::HINT,
                    lsp_types::MessageType::INFO,
                ),
            },
            None => (
                lsp_types::DiagnosticSeverity::ERROR,
                lsp_types::MessageType::ERROR,
            ),
        };

        let mut text = match error.source() {
            Some(err) => err.to_string(),
            None => error.to_string(),
        };

        if let (Some(mut labels), Some(path), Some(src)) =
            (error.labels(), error.path(), error.src())
        {
            if let Some(labeled_span) = labels.next() {
                if let Some(label) = labeled_span.label() {
                    text.push_str("\n\n");
                    text.push_str(label);

                    if !label.ends_with(['.', '?']) {
                        text.push('.');
                    }
                }

                let line_numbers = LineNumbers::new(&src);

                let start = line_numbers.line_and_column_number(labeled_span.inner().offset());
                let end = line_numbers.line_and_column_number(
                    labeled_span.inner().offset() + labeled_span.inner().len(),
                );

                let lsp_diagnostic = lsp_types::Diagnostic {
                    range: lsp_types::Range::new(
                        lsp_types::Position {
                            line: start.line as u32 - 1,
                            character: start.column as u32 - 1,
                        },
                        lsp_types::Position {
                            line: end.line as u32 - 1,
                            character: end.column as u32 - 1,
                        },
                    ),
                    severity: Some(severity),
                    code: error
                        .code()
                        .map(|c| lsp_types::NumberOrString::String(c.to_string())),
                    code_description: None,
                    source: None,
                    message: text.clone(),
                    related_information: None,
                    tags: None,
                    data: None,
                };

                let path = path.canonicalize()?;

                self.push_diagnostic(path.clone(), lsp_diagnostic.clone());

                if let Some(hint) = error.help() {
                    let lsp_hint = lsp_types::Diagnostic {
                        severity: Some(lsp_types::DiagnosticSeverity::HINT),
                        message: hint.to_string(),
                        ..lsp_diagnostic
                    };

                    self.push_diagnostic(path, lsp_hint);
                }
            }
        } else {
            self.stored_messages
                .push(lsp_types::ShowMessageParams { typ, message: text })
        }

        Ok(())
    }

    fn push_diagnostic(&mut self, path: PathBuf, diagnostic: lsp_types::Diagnostic) {
        self.stored_diagnostics
            .entry(path)
            .or_default()
            .push(diagnostic);
    }
}

fn cast_request<R>(request: lsp_server::Request) -> Result<R::Params, ServerError>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    let (_, params) = request.extract(R::METHOD)?;

    Ok(params)
}

fn cast_notification<N>(notification: lsp_server::Notification) -> Result<N::Params, ServerError>
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

fn path_to_uri(path: PathBuf) -> Result<lsp_types::Url, ServerError> {
    let mut file: String = "file://".into();

    file.push_str(&path.as_os_str().to_string_lossy());

    let uri = lsp_types::Url::parse(&file)?;

    Ok(uri)
}
