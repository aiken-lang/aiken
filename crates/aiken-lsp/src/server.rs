use self::lsp_project::LspProject;
use crate::{
    cast::{cast_notification, cast_request},
    error::Error as ServerError,
    quickfix,
    quickfix::Quickfix,
    utils::{
        path_to_uri, span_to_lsp_range, text_edit_replace, uri_to_module_name,
        COMPILING_PROGRESS_TOKEN, CREATE_COMPILING_PROGRESS_TOKEN,
    },
};
use aiken_lang::{
    ast::{Definition, Located, ModuleKind, Span, Use},
    error::ExtraData,
    line_numbers::LineNumbers,
    parser,
    tipo::pretty::Printer,
};
use aiken_project::{
    config::{self, Config},
    error::{Error as ProjectError, GetSource},
    module::CheckedModule,
};
use indoc::formatdoc;
use itertools::Itertools;
use lsp_server::{Connection, Message};
use lsp_types::{
    notification::{
        DidChangeTextDocument, DidChangeWatchedFiles, DidCloseTextDocument, DidSaveTextDocument,
        Notification, Progress, PublishDiagnostics, ShowMessage,
    },
    request::{
        CodeActionRequest, Completion, Formatting, GotoDefinition, HoverRequest, Request,
        WorkDoneProgressCreate,
    },
    DocumentFormattingParams, InitializeParams, TextEdit,
};
use miette::Diagnostic;
use std::{
    collections::{HashMap, HashSet},
    fs,
    path::{Path, PathBuf},
};

pub mod lsp_project;
pub mod telemetry;

#[allow(dead_code)]
pub struct Server {
    // Project root directory
    root: PathBuf,

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

    /// An instance of a LspProject
    compiler: Option<LspProject>,
}

impl Server {
    /// Clear all diagnostics that have been previously published to the client
    #[allow(clippy::result_large_err)]
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

    /// Compile the project if we are in one. Otherwise do nothing.
    #[allow(clippy::result_large_err)]
    fn compile(&mut self, connection: &Connection) -> Result<(), ServerError> {
        self.notify_client_of_compilation_start(connection)?;

        if let Some(compiler) = self.compiler.as_mut() {
            let result = compiler.compile();

            for warning in compiler.project.warnings() {
                self.process_diagnostic(warning)?;
            }

            if let Err(errs) = result {
                for err in errs {
                    self.process_diagnostic(err)?;
                }
            }
        }

        self.notify_client_of_compilation_end(connection)?;

        Ok(())
    }

    #[allow(clippy::result_large_err)]
    fn create_compilation_progress_token(
        &mut self,
        connection: &lsp_server::Connection,
    ) -> Result<(), ServerError> {
        let params = lsp_types::WorkDoneProgressCreateParams {
            token: lsp_types::NumberOrString::String(COMPILING_PROGRESS_TOKEN.into()),
        };

        let request = lsp_server::Request {
            id: CREATE_COMPILING_PROGRESS_TOKEN.to_string().into(),
            method: WorkDoneProgressCreate::METHOD.into(),
            params: serde_json::to_value(params)?,
        };

        connection
            .sender
            .send(lsp_server::Message::Request(request))?;

        Ok(())
    }

    fn create_new_compiler(&mut self) {
        if let Some(config) = self.config.as_ref() {
            let compiler = LspProject::new(config.clone(), self.root.clone(), telemetry::Lsp);

            self.compiler = Some(compiler);
        }
    }

    fn format(
        &mut self,
        params: DocumentFormattingParams,
    ) -> Result<Vec<TextEdit>, Vec<ProjectError>> {
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
                let src = {
                    #[cfg(not(target_os = "windows"))]
                    {
                        fs::read_to_string(path).map_err(ProjectError::from)?
                    }
                    #[cfg(target_os = "windows")]
                    {
                        let temp = match urlencoding::decode(path) {
                            Ok(decoded) => decoded.to_string(),
                            Err(_) => path.to_owned(),
                        };
                        fs::read_to_string(temp.trim_start_matches("/"))
                            .map_err(ProjectError::from)?
                    }
                };

                let (module, extra) = parser::module(&src, ModuleKind::Lib).map_err(|errs| {
                    aiken_project::error::Error::from_parse_errors(errs, Path::new(path), &src)
                })?;

                aiken_lang::format::pretty(&mut new_text, module, extra, &src);
            }
        }

        Ok(vec![text_edit_replace(new_text)])
    }

    #[allow(clippy::result_large_err)]
    fn handle_notification(
        &mut self,
        connection: &lsp_server::Connection,
        notification: lsp_server::Notification,
    ) -> Result<(), ServerError> {
        match notification.method.as_str() {
            DidSaveTextDocument::METHOD => {
                let params = cast_notification::<DidSaveTextDocument>(notification)?;

                self.edited.remove(params.text_document.uri.path());

                self.compile(connection)?;

                self.publish_stored_diagnostics(connection)?;

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

            DidCloseTextDocument::METHOD => {
                let params = cast_notification::<DidCloseTextDocument>(notification)?;

                self.edited.remove(params.text_document.uri.path());

                Ok(())
            }

            DidChangeWatchedFiles::METHOD => {
                if let Ok(config) = Config::load(&self.root) {
                    self.config = Some(config);
                    self.create_new_compiler();
                    self.compile(connection)?;
                } else {
                    self.stored_messages.push(lsp_types::ShowMessageParams {
                        typ: lsp_types::MessageType::ERROR,
                        message: "Failed to reload aiken.toml".to_string(),
                    });
                }

                self.publish_stored_diagnostics(connection)?;

                Ok(())
            }

            _ => Ok(()),
        }
    }

    #[allow(clippy::result_large_err)]
    fn handle_request(
        &mut self,
        request: lsp_server::Request,
        connection: &Connection,
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
                    Err(errors) => {
                        for error in errors {
                            self.process_diagnostic(error)?;
                        }

                        self.publish_stored_diagnostics(connection)?;

                        Ok(lsp_server::Response {
                            id,
                            error: None,
                            result: Some(serde_json::json!(null)),
                        })
                    }
                }
            }

            HoverRequest::METHOD => {
                let params = cast_request::<HoverRequest>(request)?;

                let opt_hover = self.hover(params)?;

                Ok(lsp_server::Response {
                    id,
                    error: None,
                    result: Some(serde_json::to_value(opt_hover)?),
                })
            }

            GotoDefinition::METHOD => {
                let params = cast_request::<GotoDefinition>(request)?;

                let location = self.goto_definition(params)?;

                Ok(lsp_server::Response {
                    id,
                    error: None,
                    result: Some(serde_json::to_value(location)?),
                })
            }

            Completion::METHOD => {
                let params = cast_request::<Completion>(request).expect("cast Completion");

                let completions = self.completion(params);

                Ok(lsp_server::Response {
                    id,
                    error: None,
                    result: Some(serde_json::to_value(completions)?),
                })
            }

            CodeActionRequest::METHOD => {
                let mut actions = Vec::new();

                if let Some(ref compiler) = self.compiler {
                    let params = cast_request::<CodeActionRequest>(request)
                        .expect("cast code action request");

                    let mut unused_imports = Vec::new();

                    for diagnostic in params.context.diagnostics.into_iter() {
                        match quickfix::assert(diagnostic) {
                            None => (),
                            Some(Quickfix::UnusedImports(diagnostics)) => {
                                unused_imports.extend(diagnostics);
                            }
                            Some(strategy) => {
                                let quickfixes =
                                    quickfix::quickfix(compiler, &params.text_document, &strategy);
                                actions.extend(quickfixes);
                            }
                        }
                    }

                    // NOTE: We handle unused imports separately because we want them to be done in
                    // a single action. Otherwise they might mess up with spans and alignments as
                    // they remove content from the document.
                    // Plus, it's also just much more convenient that way instead of removing each
                    // import one-by-one.
                    if !unused_imports.is_empty() {
                        let quickfixes = quickfix::quickfix(
                            compiler,
                            &params.text_document,
                            &Quickfix::UnusedImports(unused_imports),
                        );
                        actions.extend(quickfixes);
                    }
                }

                Ok(lsp_server::Response {
                    id,
                    error: None,
                    result: Some(serde_json::to_value(actions)?),
                })
            }

            unsupported => Err(ServerError::UnsupportedLspRequest {
                request: unsupported.to_string(),
            }),
        }
    }

    fn completion(
        &self,
        params: lsp_types::CompletionParams,
    ) -> Option<Vec<lsp_types::CompletionItem>> {
        let found = self
            .node_at_position(&params.text_document_position)
            .map(|(_, found)| found);

        match found {
            // TODO: test
            None => self.completion_for_import(&[]),
            Some(Located::Definition(Definition::Use(Use { module, .. }))) => {
                self.completion_for_import(module)
            }

            // TODO: autocompletion for patterns
            Some(Located::Pattern(_pattern, _value)) => None,

            // TODO: autocompletion for other definitions
            Some(Located::Definition(_expression)) => None,

            // TODO: autocompletion for expressions
            Some(Located::Expression(_expression)) => None,

            // TODO: autocompletion for arguments?
            Some(Located::Argument(_arg_name, _tipo)) => None,

            // TODO: autocompletion for annotation?
            Some(Located::Annotation(_annotation)) => None,
        }
    }

    fn completion_for_import(&self, module: &[String]) -> Option<Vec<lsp_types::CompletionItem>> {
        let compiler = self.compiler.as_ref()?;

        // TODO: Test
        let dependencies_modules = compiler.project.importable_modules();

        // TODO: Test
        let project_modules = compiler.modules.keys().cloned();

        let modules = dependencies_modules
            .into_iter()
            .chain(project_modules)
            .sorted()
            .filter(|m| m.starts_with(&module.join("/")))
            .map(|label| lsp_types::CompletionItem {
                label,
                kind: None,
                documentation: None,
                ..Default::default()
            })
            .collect();

        Some(modules)
    }

    #[allow(clippy::result_large_err)]
    fn goto_definition(
        &self,
        params: lsp_types::GotoDefinitionParams,
    ) -> Result<Option<lsp_types::Location>, ServerError> {
        let params = params.text_document_position_params;

        let (line_numbers, node) = match self.node_at_position(&params) {
            Some(location) => location,
            None => return Ok(None),
        };

        let location = match node.definition_location() {
            Some(location) => location,
            None => return Ok(None),
        };

        let (uri, line_numbers) = match location.module {
            None => (params.text_document.uri, &line_numbers),
            Some(name) => {
                let module = match self
                    .compiler
                    .as_ref()
                    .and_then(|compiler| compiler.sources.get(name))
                {
                    Some(module) => module,

                    None => return Ok(None),
                };

                let url = url::Url::parse(&format!("file:///{}", &module.path))
                    .expect("goto definition URL parse");

                (url, &module.line_numbers)
            }
        };

        let range = span_to_lsp_range(location.span, line_numbers);

        Ok(Some(lsp_types::Location { uri, range }))
    }

    fn node_at_position(
        &self,
        params: &lsp_types::TextDocumentPositionParams,
    ) -> Option<(LineNumbers, Located<'_>)> {
        let module = self.module_for_uri(&params.text_document.uri)?;

        let line_numbers = LineNumbers::new(&module.code);

        let byte_index = line_numbers.byte_index(
            params.position.line as usize,
            params.position.character as usize,
        );

        let node = module.find_node(byte_index)?;

        Some((line_numbers, node))
    }

    fn module_for_uri(&self, uri: &url::Url) -> Option<&CheckedModule> {
        self.compiler.as_ref().and_then(|compiler| {
            let module_name = uri_to_module_name(uri, &self.root).expect("uri to module name");
            compiler.modules.get(&module_name)
        })
    }

    #[allow(clippy::result_large_err)]
    fn hover(
        &self,
        params: lsp_types::HoverParams,
    ) -> Result<Option<lsp_types::Hover>, ServerError> {
        let params = params.text_document_position_params;

        let (line_numbers, found) = match self.node_at_position(&params) {
            Some(value) => value,
            None => return Ok(None),
        };

        let (location, definition_location, tipo) = match found {
            Located::Expression(expression) => (
                expression.location(),
                expression.definition_location(),
                Some(expression.tipo()),
            ),
            Located::Pattern(pattern, tipo) => (pattern.location(), None, Some(tipo)),
            Located::Argument(arg_name, tipo) => (arg_name.location(), None, Some(tipo)),
            Located::Definition(_) => return Ok(None),
            Located::Annotation(_) => return Ok(None),
        };

        let doc = definition_location
            .and_then(|loc| loc.module.map(|m| (m, loc.span)))
            .and_then(|(m, span)| {
                self.compiler
                    .as_ref()
                    .and_then(|compiler| compiler.modules.get(m))
                    .map(|checked_module| (checked_module, span))
            })
            .and_then(|(checked_module, span)| checked_module.ast.find_node(span.start))
            .and_then(|node| match node {
                Located::Expression(_) => None,
                Located::Pattern(_, _) => None,
                Located::Argument(_, _) => None,
                Located::Annotation(_) => None,
                Located::Definition(def) => def.doc(),
            })
            .unwrap_or_default();

        // Show the type of the hovered node to the user
        let type_ = match tipo {
            Some(t) => Printer::new().pretty_print(t.as_ref(), 0),
            None => "?".to_string(),
        };

        let contents = formatdoc! {r#"
            ```aiken
            {type_}
            ```
            {doc}
        "#};

        Ok(Some(lsp_types::Hover {
            contents: lsp_types::HoverContents::Scalar(lsp_types::MarkedString::String(contents)),
            range: Some(span_to_lsp_range(location, &line_numbers)),
        }))
    }

    #[allow(clippy::result_large_err)]
    pub fn listen(&mut self, connection: Connection) -> Result<(), ServerError> {
        self.create_compilation_progress_token(&connection)?;
        self.start_watching_aiken_toml(&connection)?;

        // Compile the project once so we have all the state and any initial errors
        self.compile(&connection)?;
        self.publish_stored_diagnostics(&connection)?;

        for msg in &connection.receiver {
            tracing::debug!("Got message: {:#?}", msg);

            match msg {
                Message::Request(req) => {
                    if connection.handle_shutdown(&req)? {
                        return Ok(());
                    }

                    tracing::debug!("Get request: {:#?}", req);

                    let response = self.handle_request(req, &connection)?;

                    connection.sender.send(Message::Response(response))?;
                }
                Message::Response(_) => (),
                Message::Notification(notification) => {
                    self.handle_notification(&connection, notification)?
                }
            }
        }

        Ok(())
    }

    pub fn new(
        initialize_params: InitializeParams,
        config: Option<config::Config>,
        root: PathBuf,
    ) -> Self {
        let mut server = Server {
            root,
            config,
            edited: HashMap::new(),
            initialize_params,
            published_diagnostics: HashSet::new(),
            stored_diagnostics: HashMap::new(),
            stored_messages: Vec::new(),
            compiler: None,
        };

        server.create_new_compiler();

        server
    }

    #[allow(clippy::result_large_err)]
    fn notify_client_of_compilation_end(&self, connection: &Connection) -> Result<(), ServerError> {
        self.send_work_done_notification(
            connection,
            lsp_types::WorkDoneProgress::End(lsp_types::WorkDoneProgressEnd { message: None }),
        )
    }

    #[allow(clippy::result_large_err)]
    fn notify_client_of_compilation_start(
        &self,
        connection: &Connection,
    ) -> Result<(), ServerError> {
        self.send_work_done_notification(
            connection,
            lsp_types::WorkDoneProgress::Begin(lsp_types::WorkDoneProgressBegin {
                title: "Compiling Aiken".into(),
                cancellable: Some(false),
                message: None,
                percentage: None,
            }),
        )
    }

    /// Convert Aiken diagnostics into 1 or more LSP diagnostics and store them
    /// so that they can later be published to the client with
    /// `publish_stored_diagnostics`
    ///
    /// If the Aiken diagnostic cannot be converted to LSP diagnostic (due to it
    /// not having a location) it is stored as a message suitable for use with
    /// the `showMessage` notification instead.
    #[allow(clippy::result_large_err)]
    fn process_diagnostic<E>(&mut self, error: E) -> Result<(), ServerError>
    where
        E: Diagnostic + GetSource + ExtraData,
    {
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

        let message = match error.source() {
            Some(err) => err.to_string(),
            None => error.to_string(),
        };

        if let (Some(mut labels), Some(path), Some(src)) =
            (error.labels(), error.path(), error.src())
        {
            if let Some(labeled_span) = labels.next() {
                let line_numbers = LineNumbers::new(&src);

                let lsp_diagnostic = lsp_types::Diagnostic {
                    range: span_to_lsp_range(
                        Span {
                            start: labeled_span.inner().offset(),
                            end: labeled_span.inner().offset() + labeled_span.inner().len(),
                        },
                        &line_numbers,
                    ),
                    severity: Some(severity),
                    code: error.code().map(|c| {
                        lsp_types::NumberOrString::String(
                            c.to_string()
                                .trim()
                                .replace("Warning ", "")
                                .replace("Error ", ""),
                        )
                    }),
                    code_description: None,
                    source: None,
                    message,
                    related_information: None,
                    tags: None,
                    data: error.extra_data().map(serde_json::Value::String),
                };

                #[cfg(not(target_os = "windows"))]
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
                .push(lsp_types::ShowMessageParams { typ, message })
        }

        Ok(())
    }

    /// Publish all stored diagnostics to the client.
    /// Any previously publish diagnostics are cleared before the new set are
    /// published to the client.
    #[allow(clippy::result_large_err)]
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

    fn push_diagnostic(&mut self, path: PathBuf, diagnostic: lsp_types::Diagnostic) {
        self.stored_diagnostics
            .entry(path)
            .or_default()
            .push(diagnostic);
    }

    #[allow(clippy::result_large_err)]
    fn send_work_done_notification(
        &self,
        connection: &Connection,
        work_done: lsp_types::WorkDoneProgress,
    ) -> Result<(), ServerError> {
        tracing::info!("sending {:?}", work_done);

        let params = lsp_types::ProgressParams {
            token: lsp_types::NumberOrString::String(COMPILING_PROGRESS_TOKEN.to_string()),
            value: lsp_types::ProgressParamsValue::WorkDone(work_done),
        };

        let notification = lsp_server::Notification {
            method: Progress::METHOD.into(),
            params: serde_json::to_value(params)?,
        };

        connection
            .sender
            .send(lsp_server::Message::Notification(notification))?;

        Ok(())
    }

    #[allow(clippy::result_large_err)]
    fn start_watching_aiken_toml(&mut self, connection: &Connection) -> Result<(), ServerError> {
        let supports_watch_files = self
            .initialize_params
            .capabilities
            .workspace
            .as_ref()
            .and_then(|w| w.did_change_watched_files)
            .map(|wf| wf.dynamic_registration == Some(true))
            .unwrap_or(false);

        if !supports_watch_files {
            tracing::warn!("lsp_client_cannot_watch_aiken_toml");

            return Ok(());
        }

        // Register aiken.toml as a watched file so we get a notification when
        // it changes and thus know that we need to rebuild the entire project.
        let register_options =
            serde_json::value::to_value(lsp_types::DidChangeWatchedFilesRegistrationOptions {
                watchers: vec![lsp_types::FileSystemWatcher {
                    glob_pattern: String::from("aiken.toml").into(),
                    kind: Some(lsp_types::WatchKind::Change),
                }],
            })?;

        let watch_config = lsp_types::Registration {
            id: "watch-aiken-toml".into(),
            method: "workspace/didChangeWatchedFiles".into(),
            register_options: Some(register_options),
        };

        let request = lsp_server::Request {
            id: 1.into(),
            method: "client/registerCapability".into(),
            params: serde_json::value::to_value(lsp_types::RegistrationParams {
                registrations: vec![watch_config],
            })
            .expect("client/registerCapability to json"),
        };

        connection
            .sender
            .send(lsp_server::Message::Request(request))?;

        Ok(())
    }
}
