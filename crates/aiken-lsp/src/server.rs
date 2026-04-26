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
    ast::{Definition, Located, ModuleKind, Span, TypedDefinition, Use},
    error::ExtraData,
    line_numbers::LineNumbers,
    parser,
    tipo::pretty::Printer,
};
use aiken_project::{
    config::{self, ProjectConfig},
    error::{Error as ProjectError, GetSource},
    module::CheckedModule,
};
use indoc::formatdoc;
use itertools::Itertools;
use lsp_server::Connection;
use lsp_types::{
    notification::{
        DidChangeTextDocument, DidChangeWatchedFiles, DidCloseTextDocument, DidSaveTextDocument,
        Notification, Progress, PublishDiagnostics, ShowMessage,
    },
    request::{
        CodeActionRequest, Completion, DocumentSymbolRequest, Formatting, GotoDefinition,
        HoverRequest, References, Request, WorkDoneProgressCreate,
    },
    DocumentFormattingParams, DocumentSymbol, InitializeParams, SymbolKind, TextEdit,
};
use miette::Diagnostic;
use std::{
    collections::{HashMap, HashSet},
    fs,
    path::{Path, PathBuf},
};

pub mod lsp_project;
pub mod telemetry;

/// Result returned from background compilation thread
struct BackgroundCompileResult {
    compiler: LspProject,
    stored_diagnostics: HashMap<PathBuf, Vec<lsp_types::Diagnostic>>,
    stored_messages: Vec<lsp_types::ShowMessageParams>,
}

unsafe impl Send for BackgroundCompileResult {}

pub struct Server {
    // Project root directory
    root: PathBuf,

    config: Option<config::ProjectConfig>,

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

    pending_compile: Option<std::thread::JoinHandle<BackgroundCompileResult>>,

    /// Files changed since last successful compilation (used to avoid no-op compiles)
    files_changed_since_compile: HashSet<String>,
}

fn process_diagnostic_into<E>(
    error: E,
    stored_diagnostics: &mut HashMap<PathBuf, Vec<lsp_types::Diagnostic>>,
    stored_messages: &mut Vec<lsp_types::ShowMessageParams>,
) where
    E: miette::Diagnostic + GetSource + ExtraData,
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

    if let (Some(path), Some(src)) = (error.path(), error.src()) {
        let line_numbers = LineNumbers::new(&src);

        let related_labels = || {
            error
                .related()
                .and_then(|mut iter| iter.find(|diag| diag.labels().is_some()))
                .and_then(|diag| diag.labels())
        };

        let lsp_range = if let Some(span) = error
            .labels()
            .or_else(related_labels)
            .and_then(|mut labels| labels.next())
        {
            span_to_lsp_range(
                Span {
                    start: span.inner().offset(),
                    end: span.inner().offset() + span.inner().len(),
                },
                &line_numbers,
            )
        } else {
            stored_messages.push(lsp_types::ShowMessageParams { typ, message });
            return;
        };

        let lsp_diagnostic = lsp_types::Diagnostic {
            range: lsp_range,
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
        let path = path.canonicalize().ok().unwrap_or(path);

        stored_diagnostics
            .entry(path.clone())
            .or_default()
            .push(lsp_diagnostic.clone());

        let lsp_message = if let Some(hint) = error.help() {
            hint.to_string()
        } else {
            "something is off".to_string()
        };

        let lsp_hint = lsp_types::Diagnostic {
            severity: Some(lsp_types::DiagnosticSeverity::HINT),
            message: lsp_message,
            ..lsp_diagnostic
        };

        stored_diagnostics.entry(path).or_default().push(lsp_hint);
    } else {
        stored_messages.push(lsp_types::ShowMessageParams { typ, message })
    }
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
    pub fn compile(&mut self, connection: &Connection) -> Result<(), ServerError> {
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

    fn spawn_compile(&mut self) {
        if self.files_changed_since_compile.is_empty()
            && let Some(compiler) = &self.compiler
            && !compiler.dep_cache().type_infos.is_empty()
        {
            return;
        }

        let _ = self.pending_compile.take();
        if let Some(config) = self.config.clone() {
            let root = self.root.clone();
            let dep_cache = self
                .compiler
                .as_ref()
                .map(|c| c.dep_cache().clone())
                .unwrap_or_default();
            let handle = std::thread::spawn(move || {
                let mut compiler =
                    LspProject::new_with_cache(config, root, telemetry::Lsp, dep_cache);
                let result = compiler.compile();
                let mut stored_diagnostics: HashMap<PathBuf, Vec<lsp_types::Diagnostic>> =
                    HashMap::new();
                let mut stored_messages: Vec<lsp_types::ShowMessageParams> = Vec::new();
                for warning in compiler.project.warnings() {
                    process_diagnostic_into(warning, &mut stored_diagnostics, &mut stored_messages);
                }
                if let Err(errs) = result {
                    for err in errs {
                        process_diagnostic_into(err, &mut stored_diagnostics, &mut stored_messages);
                    }
                }
                BackgroundCompileResult {
                    compiler,
                    stored_diagnostics,
                    stored_messages,
                }
            });
            self.pending_compile = Some(handle);
        }
    }

    #[allow(clippy::result_large_err)]
    fn poll_compile_result(&mut self, connection: &Connection) -> Result<(), ServerError> {
        let finished = self
            .pending_compile
            .as_ref()
            .map(|h| h.is_finished())
            .unwrap_or(false);
        if finished
            && let Some(handle) = self.pending_compile.take()
            && let Ok(result) = handle.join()
        {
            self.compiler = Some(result.compiler);
            self.files_changed_since_compile.clear();
            self.stored_diagnostics = result.stored_diagnostics;
            self.stored_messages = result.stored_messages;
            self.notify_client_of_compilation_end(connection)?;
            self.publish_stored_diagnostics(connection)?;
        }
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

        let src = match self.edited.get(path) {
            Some(src) => src.clone(),
            None => fs::read_to_string(path).map_err(|_| vec![])?,
        };

        let (module, extra) = parser::module(&src, ModuleKind::Lib).map_err(|errs| {
            aiken_project::error::Error::from_parse_errors(errs, Path::new(path), &src)
        })?;

        aiken_lang::format::pretty(&mut new_text, module, extra, &src);

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
                let file_path = params.text_document.uri.path();
                self.edited.remove(file_path);
                self.files_changed_since_compile
                    .insert(file_path.to_string());
                self.notify_client_of_compilation_start(connection)?;
                self.spawn_compile();
                Ok(())
            }

            DidChangeTextDocument::METHOD => {
                let params = cast_notification::<DidChangeTextDocument>(notification)?;
                let path = params.text_document.uri.path().to_string();
                if let Some(changes) = params.content_changes.into_iter().next() {
                    self.edited.insert(path.clone(), changes.text);
                    self.files_changed_since_compile.insert(path);
                }
                Ok(())
            }

            DidCloseTextDocument::METHOD => {
                let params = cast_notification::<DidCloseTextDocument>(notification)?;
                let file_path = params.text_document.uri.path();
                self.edited.remove(file_path);
                Ok(())
            }

            DidChangeWatchedFiles::METHOD => {
                if let Ok(config) = ProjectConfig::load(&self.root) {
                    self.config = Some(config);
                    self.files_changed_since_compile
                        .insert("__config__".to_string());
                    self.notify_client_of_compilation_start(connection)?;
                    self.spawn_compile();
                } else {
                    self.stored_messages.push(lsp_types::ShowMessageParams {
                        typ: lsp_types::MessageType::ERROR,
                        message: "Failed to reload aiken.toml".to_string(),
                    });
                }

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

            DocumentSymbolRequest::METHOD => {
                let params = cast_request::<DocumentSymbolRequest>(request)?;

                let symbols = self.document_symbols(params)?;

                Ok(lsp_server::Response {
                    id,
                    error: None,
                    result: Some(serde_json::to_value(symbols)?),
                })
            }

            References::METHOD => {
                let params = cast_request::<References>(request)?;

                let references = self.references(params)?;

                Ok(lsp_server::Response {
                    id,
                    error: None,
                    result: Some(serde_json::to_value(references)?),
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
    fn document_symbols(
        &self,
        params: lsp_types::DocumentSymbolParams,
    ) -> Result<Option<lsp_types::DocumentSymbolResponse>, ServerError> {
        eprintln!(
            "DEBUG: document_symbols called for URI: {}",
            params.text_document.uri
        );

        let module = match self.module_for_uri(&params.text_document.uri) {
            Some(module) => {
                eprintln!("DEBUG: Found module: {}", module.name);
                module
            }
            None => {
                eprintln!("DEBUG: No module found for URI");
                return Ok(None);
            }
        };

        let src = match &self.edited.get(&params.text_document.uri.to_string()) {
            Some(src) => src.as_str(),
            None => &module.code,
        };

        let line_numbers = LineNumbers::new(src);
        let mut symbols = Vec::new();

        eprintln!(
            "DEBUG: Processing {} definitions",
            module.ast.definitions.len()
        );

        // Extract symbols from module definitions
        for definition in &module.ast.definitions {
            if let Some(symbol) = self.definition_to_symbol(definition, &line_numbers) {
                eprintln!("DEBUG: Found symbol: {}", symbol.name);
                symbols.push(symbol);
            }
        }

        eprintln!("DEBUG: Returning {} symbols", symbols.len());
        Ok(Some(lsp_types::DocumentSymbolResponse::Nested(symbols)))
    }

    /// Find all references to a symbol at the given position
    #[allow(clippy::result_large_err)]
    fn references(
        &self,
        params: lsp_types::ReferenceParams,
    ) -> Result<Option<Vec<lsp_types::Location>>, ServerError> {
        let compiler = match &self.compiler {
            Some(compiler) => compiler,
            None => return Ok(None),
        };

        let module = match self.module_for_uri(&params.text_document_position.text_document.uri) {
            Some(module) => module,
            None => return Ok(None),
        };

        let src = match &self
            .edited
            .get(&params.text_document_position.text_document.uri.to_string())
        {
            Some(src) => src.as_str(),
            None => &module.code,
        };

        let line_numbers = LineNumbers::new(src);

        // Find the symbol at the given position
        let symbol_name = match self.find_symbol_at_position(
            &params.text_document_position.position,
            module,
            &line_numbers,
        ) {
            Some(name) => {
                eprintln!("DEBUG: Found symbol name: {}", name);
                name
            }
            None => {
                eprintln!("DEBUG: No symbol found at position");
                return Ok(None);
            }
        };

        let mut references = Vec::new();

        // Search through all modules for references to this symbol
        for (module_name, checked_module) in &compiler.modules {
            if let Some(module_refs) =
                self.find_references_in_module(&symbol_name, checked_module, module_name)
            {
                references.extend(module_refs);
            }
        }

        // Include the definition itself if requested
        if params.context.include_declaration
            && let Some(def_location) =
                self.find_definition_location(&symbol_name, module, &line_numbers)
        {
            references.push(def_location);
        }

        Ok(Some(references))
    }

    /// Convert an Aiken definition to LSP DocumentSymbol
    #[allow(deprecated)]
    fn definition_to_symbol(
        &self,
        definition: &TypedDefinition,
        line_numbers: &LineNumbers,
    ) -> Option<DocumentSymbol> {
        use aiken_lang::ast::Definition;

        match definition {
            Definition::Fn(function) => {
                let range = span_to_lsp_range(function.location, line_numbers);
                let selection_range = span_to_lsp_range(function.location, line_numbers);

                Some(DocumentSymbol {
                    name: function.name.clone(),
                    detail: Some(format!("fn {}", function.name)),
                    kind: SymbolKind::FUNCTION,
                    tags: None,
                    deprecated: None,
                    range,
                    selection_range,
                    children: None,
                })
            }
            Definition::Test(test) => {
                let range = span_to_lsp_range(test.location, line_numbers);
                let selection_range = span_to_lsp_range(test.location, line_numbers);

                Some(DocumentSymbol {
                    name: test.name.clone(),
                    detail: Some(format!("test {}", test.name)),
                    kind: SymbolKind::METHOD,
                    tags: None,
                    deprecated: None,
                    range,
                    selection_range,
                    children: None,
                })
            }
            Definition::Benchmark(benchmark) => {
                let range = span_to_lsp_range(benchmark.location, line_numbers);
                let selection_range = span_to_lsp_range(benchmark.location, line_numbers);

                Some(DocumentSymbol {
                    name: benchmark.name.clone(),
                    detail: Some(format!("benchmark {}", benchmark.name)),
                    kind: SymbolKind::METHOD,
                    tags: None,
                    deprecated: None,
                    range,
                    selection_range,
                    children: None,
                })
            }
            Definition::TypeAlias(type_alias) => {
                let range = span_to_lsp_range(type_alias.location, line_numbers);
                let selection_range = span_to_lsp_range(type_alias.location, line_numbers);

                Some(DocumentSymbol {
                    name: type_alias.alias.clone(),
                    detail: Some(format!("type {}", type_alias.alias)),
                    kind: SymbolKind::TYPE_PARAMETER,
                    tags: None,
                    deprecated: None,
                    range,
                    selection_range,
                    children: None,
                })
            }
            Definition::DataType(data_type) => {
                let range = span_to_lsp_range(data_type.location, line_numbers);
                let selection_range = span_to_lsp_range(data_type.location, line_numbers);

                // Create child symbols for constructors
                let mut children = Vec::new();
                for constructor in &data_type.constructors {
                    let constructor_range = span_to_lsp_range(constructor.location, line_numbers);
                    let constructor_selection_range =
                        span_to_lsp_range(constructor.location, line_numbers);

                    children.push(DocumentSymbol {
                        name: constructor.name.clone(),
                        detail: Some(format!("constructor {}", constructor.name)),
                        kind: SymbolKind::CONSTRUCTOR,
                        tags: None,
                        deprecated: None,
                        range: constructor_range,
                        selection_range: constructor_selection_range,
                        children: None,
                    });
                }

                Some(DocumentSymbol {
                    name: data_type.name.clone(),
                    detail: Some(format!("type {}", data_type.name)),
                    kind: SymbolKind::CLASS,
                    tags: None,
                    deprecated: None,
                    range,
                    selection_range,
                    children: if children.is_empty() {
                        None
                    } else {
                        Some(children)
                    },
                })
            }
            Definition::ModuleConstant(constant) => {
                let range = span_to_lsp_range(constant.location, line_numbers);
                let selection_range = span_to_lsp_range(constant.location, line_numbers);

                Some(DocumentSymbol {
                    name: constant.name.clone(),
                    detail: Some(format!("const {}", constant.name)),
                    kind: SymbolKind::CONSTANT,
                    tags: None,
                    deprecated: None,
                    range,
                    selection_range,
                    children: None,
                })
            }
            Definition::Validator(validator) => {
                let range = span_to_lsp_range(validator.location, line_numbers);
                let selection_range = span_to_lsp_range(validator.location, line_numbers);

                Some(DocumentSymbol {
                    name: validator.name.clone(),
                    detail: Some(format!("validator {}", validator.name)),
                    kind: SymbolKind::CLASS,
                    tags: None,
                    deprecated: None,
                    range,
                    selection_range,
                    children: None,
                })
            }
            Definition::Use(_) => {
                // Skip use statements for document symbols
                None
            }
        }
    }

    /// Find the symbol name at the given position in a module
    fn find_symbol_at_position(
        &self,
        position: &lsp_types::Position,
        module: &CheckedModule,
        line_numbers: &LineNumbers,
    ) -> Option<String> {
        // Convert LSP position to byte offset using LineNumbers API
        let byte_index =
            line_numbers.byte_index(position.line as usize, position.character as usize);

        // Debug information
        eprintln!(
            "DEBUG: find_symbol_at_position - position: {}:{}, byte_index: {}",
            position.line, position.character, byte_index
        );

        // Use the existing find_node functionality to get the node at position
        if let Some(node) = module.find_node(byte_index) {
            // Get info about the node for debugging
            let symbol_name = self.extract_symbol_name_from_node_at_position(node, byte_index);
            eprintln!("DEBUG: Found symbol: {:?}", symbol_name);
            return symbol_name;
        }

        eprintln!("DEBUG: No node found at byte_index {}", byte_index);
        None
    }

    /// Extract symbol name from a Located node, ensuring the byte_index is within the symbol's span
    fn extract_symbol_name_from_node_at_position(
        &self,
        node: Located<'_>,
        byte_index: usize,
    ) -> Option<String> {
        match node {
            Located::Expression(expr) => match expr {
                aiken_lang::expr::TypedExpr::Var { name, location, .. } => {
                    eprintln!(
                        "DEBUG: Found Variable '{}' at span {}..{}, checking byte_index {}",
                        name, location.start, location.end, byte_index
                    );
                    // Only return the symbol if the byte_index is within the symbol's location
                    if byte_index >= location.start && byte_index <= location.end {
                        Some(name.clone())
                    } else {
                        eprintln!(
                            "DEBUG: byte_index {} not within Variable '{}' span {}..{}",
                            byte_index, name, location.start, location.end
                        );
                        None
                    }
                }
                _ => {
                    eprintln!(
                        "DEBUG: Found Expression but not a Var: {:?}",
                        std::mem::discriminant(expr)
                    );
                    None
                }
            },
            Located::Definition(def) => match def {
                Definition::Fn(function) => {
                    eprintln!(
                        "DEBUG: Found Function '{}' at span {}..{}, checking byte_index {}",
                        function.name, function.location.start, function.location.end, byte_index
                    );
                    if byte_index >= function.location.start && byte_index <= function.location.end
                    {
                        Some(function.name.clone())
                    } else {
                        eprintln!(
                            "DEBUG: byte_index {} not within Function '{}' span {}..{}",
                            byte_index,
                            function.name,
                            function.location.start,
                            function.location.end
                        );
                        None
                    }
                }
                Definition::DataType(data_type) => {
                    eprintln!(
                        "DEBUG: Found DataType '{}' at span {}..{}, checking byte_index {}",
                        data_type.name,
                        data_type.location.start,
                        data_type.location.end,
                        byte_index
                    );
                    if byte_index >= data_type.location.start
                        && byte_index <= data_type.location.end
                    {
                        Some(data_type.name.clone())
                    } else {
                        eprintln!(
                            "DEBUG: byte_index {} not within DataType '{}' span {}..{}",
                            byte_index,
                            data_type.name,
                            data_type.location.start,
                            data_type.location.end
                        );
                        None
                    }
                }
                Definition::TypeAlias(type_alias) => {
                    eprintln!(
                        "DEBUG: Found TypeAlias '{}' at span {}..{}, checking byte_index {}",
                        type_alias.alias,
                        type_alias.location.start,
                        type_alias.location.end,
                        byte_index
                    );
                    if byte_index >= type_alias.location.start
                        && byte_index <= type_alias.location.end
                    {
                        Some(type_alias.alias.clone())
                    } else {
                        eprintln!(
                            "DEBUG: byte_index {} not within TypeAlias '{}' span {}..{}",
                            byte_index,
                            type_alias.alias,
                            type_alias.location.start,
                            type_alias.location.end
                        );
                        None
                    }
                }
                Definition::ModuleConstant(constant) => {
                    eprintln!(
                        "DEBUG: Found ModuleConstant '{}' at span {}..{}, checking byte_index {}",
                        constant.name, constant.location.start, constant.location.end, byte_index
                    );
                    if byte_index >= constant.location.start && byte_index <= constant.location.end
                    {
                        Some(constant.name.clone())
                    } else {
                        eprintln!(
                            "DEBUG: byte_index {} not within ModuleConstant '{}' span {}..{}",
                            byte_index,
                            constant.name,
                            constant.location.start,
                            constant.location.end
                        );
                        None
                    }
                }
                Definition::Validator(validator) => {
                    eprintln!(
                        "DEBUG: Found Validator '{}' at span {}..{}, checking byte_index {}",
                        validator.name,
                        validator.location.start,
                        validator.location.end,
                        byte_index
                    );
                    if byte_index >= validator.location.start
                        && byte_index <= validator.location.end
                    {
                        Some(validator.name.clone())
                    } else {
                        eprintln!(
                            "DEBUG: byte_index {} not within Validator '{}' span {}..{}",
                            byte_index,
                            validator.name,
                            validator.location.start,
                            validator.location.end
                        );
                        None
                    }
                }
                _ => {
                    eprintln!(
                        "DEBUG: Found Definition but not a named one: {:?}",
                        std::mem::discriminant(def)
                    );
                    None
                }
            },
            _ => {
                eprintln!(
                    "DEBUG: Found Located node but not Expression or Definition: {:?}",
                    std::mem::discriminant(&node)
                );
                None
            }
        }
    }

    /// Find definition location for a symbol
    fn find_definition_location(
        &self,
        symbol_name: &str,
        module: &CheckedModule,
        line_numbers: &LineNumbers,
    ) -> Option<lsp_types::Location> {
        // Search through module definitions to find the definition of the symbol
        for definition in &module.ast.definitions {
            match definition {
                Definition::Fn(function) if function.name == symbol_name => {
                    if let Some(uri) = self.module_name_to_uri(&module.name) {
                        let range = span_to_lsp_range(function.location, line_numbers);
                        return Some(lsp_types::Location { uri, range });
                    }
                }
                Definition::DataType(data_type) if data_type.name == symbol_name => {
                    if let Some(uri) = self.module_name_to_uri(&module.name) {
                        let range = span_to_lsp_range(data_type.location, line_numbers);
                        return Some(lsp_types::Location { uri, range });
                    }
                }
                Definition::TypeAlias(type_alias) if type_alias.alias == symbol_name => {
                    if let Some(uri) = self.module_name_to_uri(&module.name) {
                        let range = span_to_lsp_range(type_alias.location, line_numbers);
                        return Some(lsp_types::Location { uri, range });
                    }
                }
                Definition::ModuleConstant(constant) if constant.name == symbol_name => {
                    if let Some(uri) = self.module_name_to_uri(&module.name) {
                        let range = span_to_lsp_range(constant.location, line_numbers);
                        return Some(lsp_types::Location { uri, range });
                    }
                }
                Definition::Validator(validator) if validator.name == symbol_name => {
                    if let Some(uri) = self.module_name_to_uri(&module.name) {
                        let range = span_to_lsp_range(validator.location, line_numbers);
                        return Some(lsp_types::Location { uri, range });
                    }
                }
                _ => {}
            }
        }
        None
    }

    /// Convert module name to URI
    fn module_name_to_uri(&self, module_name: &str) -> Option<url::Url> {
        if let Some(compiler) = &self.compiler
            && let Some(source) = compiler.sources.get(module_name)
        {
            return url::Url::parse(&format!("file:///{}", source.path)).ok();
        }
        None
    }

    /// Find all references to a symbol in a module
    fn find_references_in_module(
        &self,
        symbol_name: &str,
        module: &CheckedModule,
        module_name: &str,
    ) -> Option<Vec<lsp_types::Location>> {
        let mut locations = Vec::new();
        let line_numbers = LineNumbers::new(&module.code);

        // Search through module definitions
        for definition in &module.ast.definitions {
            self.find_references_in_definition(
                symbol_name,
                definition,
                &line_numbers,
                module_name,
                &mut locations,
            );
        }

        if locations.is_empty() {
            None
        } else {
            Some(locations)
        }
    }

    /// Find references to a symbol in a definition
    fn find_references_in_definition(
        &self,
        symbol_name: &str,
        definition: &TypedDefinition,
        line_numbers: &LineNumbers,
        module_name: &str,
        locations: &mut Vec<lsp_types::Location>,
    ) {
        match definition {
            Definition::Fn(function) => {
                self.find_references_in_expr(
                    symbol_name,
                    &function.body,
                    line_numbers,
                    module_name,
                    locations,
                );
            }
            Definition::Validator(validator) => {
                for handler in &validator.handlers {
                    self.find_references_in_expr(
                        symbol_name,
                        &handler.body,
                        line_numbers,
                        module_name,
                        locations,
                    );
                }
            }
            Definition::ModuleConstant(constant) => {
                self.find_references_in_expr(
                    symbol_name,
                    &constant.value,
                    line_numbers,
                    module_name,
                    locations,
                );
            }
            _ => {}
        }
    }

    /// Find references to a symbol in an expression
    fn find_references_in_expr(
        &self,
        symbol_name: &str,
        expr: &aiken_lang::expr::TypedExpr,
        line_numbers: &LineNumbers,
        module_name: &str,
        locations: &mut Vec<lsp_types::Location>,
    ) {
        match expr {
            aiken_lang::expr::TypedExpr::Var { name, location, .. } => {
                if name == symbol_name
                    && let Some(uri) = self.module_name_to_uri(module_name)
                {
                    let range = span_to_lsp_range(*location, line_numbers);
                    locations.push(lsp_types::Location { uri, range });
                }
            }
            aiken_lang::expr::TypedExpr::Call { fun, args, .. } => {
                self.find_references_in_expr(
                    symbol_name,
                    fun,
                    line_numbers,
                    module_name,
                    locations,
                );
                for arg in args {
                    self.find_references_in_expr(
                        symbol_name,
                        &arg.value,
                        line_numbers,
                        module_name,
                        locations,
                    );
                }
            }
            // Add more expression types as needed for comprehensive reference finding
            _ => {}
        }
    }

    #[allow(clippy::result_large_err)]
    pub fn listen(&mut self, connection: Connection) -> Result<(), ServerError> {
        self.create_compilation_progress_token(&connection)?;
        self.start_watching_aiken_toml(&connection)?;

        self.notify_client_of_compilation_start(&connection)?;
        self.spawn_compile();

        loop {
            self.poll_compile_result(&connection)?;
            match connection
                .receiver
                .recv_timeout(std::time::Duration::from_millis(50))
            {
                Ok(lsp_server::Message::Request(req)) => {
                    if connection.handle_shutdown(&req)? {
                        return Ok(());
                    }
                    let response = self.handle_request(req, &connection)?;
                    connection
                        .sender
                        .send(lsp_server::Message::Response(response))?;
                }
                Ok(lsp_server::Message::Response(_)) => (),
                Ok(lsp_server::Message::Notification(notification)) => {
                    self.handle_notification(&connection, notification)?;
                }
                Err(crossbeam_channel::RecvTimeoutError::Timeout) => {}
                Err(crossbeam_channel::RecvTimeoutError::Disconnected) => {
                    return Ok(());
                }
            }
        }
    }

    pub fn new(
        initialize_params: InitializeParams,
        config: Option<config::ProjectConfig>,
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
            pending_compile: None,
            files_changed_since_compile: HashSet::new(),
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
                title: "Compiling Aiken project".into(),
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
        process_diagnostic_into(
            error,
            &mut self.stored_diagnostics,
            &mut self.stored_messages,
        );
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
