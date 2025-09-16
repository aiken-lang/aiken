use self::lsp_project::LspProject;
use crate::{
    cast::{cast_notification, cast_request},
    error::Error as ServerError,
    quickfix,
    quickfix::Quickfix,
    utils::{
        COMPILING_PROGRESS_TOKEN, CREATE_COMPILING_PROGRESS_TOKEN, path_to_uri, span_to_lsp_range,
        text_edit_replace, uri_to_module_name,
    },
};
use aiken_lang::{
    ast::{Definition, Located, ModuleKind, Span, TypedDefinition, UntypedModule, Use},
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
use lsp_server::{Connection, Message};
use lsp_types::{
    DocumentFormattingParams, DocumentSymbol, InitializeParams, Range, SymbolKind, TextEdit,
    notification::{
        DidChangeTextDocument, DidChangeWatchedFiles, DidCloseTextDocument, DidSaveTextDocument,
        Notification, Progress, PublishDiagnostics, ShowMessage,
    },
    request::{
        CodeActionRequest, Completion, DocumentSymbolRequest, Formatting, GotoDefinition,
        HoverRequest, References, Request, WorkDoneProgressCreate, WorkspaceSymbolRequest,
    },
};
use miette::Diagnostic;
use std::{
    collections::hash_map::DefaultHasher,
    collections::{HashMap, HashSet},
    fs,
    hash::{Hash, Hasher},
    path::{Path, PathBuf},
    sync::{Arc, Mutex},
    thread,
    time::{Duration, Instant},
};

pub mod lsp_project;
pub mod telemetry;

/// Represents a text change for incremental parsing
#[derive(Debug, Clone)]
#[allow(dead_code)]
struct TextChange {
    /// Range of text that was changed
    range: Range,
    /// New text content
    text: String,
    /// Length of text that was replaced
    range_length: Option<u32>,
}

/// Tracks incremental changes to a document
#[derive(Debug, Clone)]
struct IncrementalState {
    /// Current version of the document
    version: Option<i32>,
    /// Previous content for diff calculation
    previous_content: String,
    /// Accumulated changes since last parse
    pending_changes: Vec<TextChange>,
    /// Whether changes are small enough for incremental parsing
    is_incremental_viable: bool,
}

/// Tracks dirty files that need recompilation
#[derive(Debug, Clone)]
struct DirtyFileTracker {
    /// Files that have been modified since last compilation
    /// Key: file_path, Value: last_modified_time
    dirty_files: HashMap<String, Instant>,
    /// Files that are currently being processed
    processing_files: HashSet<String>,
    /// Dependencies between files (which files import which others)
    file_dependencies: HashMap<String, HashSet<String>>,
    /// Last successful compilation time
    last_compilation_time: Option<Instant>,
}

/// Memory pool for frequent allocations to reduce fragmentation
#[allow(dead_code)]
struct MemoryPool<T> {
    /// Available objects in the pool
    available: Vec<T>,
    /// Total objects created (for statistics)
    total_created: usize,
    /// Maximum pool size to prevent unbounded growth
    max_size: usize,
    /// Factory function to create new objects
    factory: Box<dyn Fn() -> T + Send + Sync>,
}

#[allow(dead_code)]
impl<T> MemoryPool<T> {
    fn new(max_size: usize, factory: Box<dyn Fn() -> T + Send + Sync>) -> Self {
        Self {
            available: Vec::new(),
            total_created: usize::MAX, // Start high to avoid overflow issues
            max_size,
            factory,
        }
    }

    /// Get an object from the pool or create a new one
    fn get(&mut self) -> T {
        if let Some(obj) = self.available.pop() {
            obj
        } else {
            self.total_created = self.total_created.saturating_add(1);
            (self.factory)()
        }
    }

    /// Return an object to the pool for reuse
    fn put(&mut self, obj: T) {
        if self.available.len() < self.max_size {
            self.available.push(obj);
        }
    }

    /// Get pool statistics
    fn stats(&self) -> (usize, usize, usize) {
        (self.available.len(), self.total_created, self.max_size)
    }

    /// Clear the pool (useful for cleanup)
    fn clear(&mut self) {
        self.available.clear();
        self.total_created = 0;
    }
}

/// Thread-safe memory pool manager for different allocation types
#[allow(dead_code)]
struct MemoryPoolManager {
    /// Pool for string allocations
    string_pool: Mutex<MemoryPool<String>>,
    /// Pool for vector allocations
    vec_pool: Mutex<MemoryPool<Vec<u8>>>,
    /// Pool for hashmap allocations
    hashmap_pool: Mutex<MemoryPool<HashMap<String, String>>>,
}

#[allow(dead_code)]
impl MemoryPoolManager {
    fn new() -> Self {
        Self {
            string_pool: Mutex::new(MemoryPool::new(
                1000,
                Box::new(|| String::with_capacity(256)),
            )),
            vec_pool: Mutex::new(MemoryPool::new(500, Box::new(|| Vec::with_capacity(1024)))),
            hashmap_pool: Mutex::new(MemoryPool::new(
                200,
                Box::new(|| HashMap::with_capacity(16)),
            )),
        }
    }

    /// Get a string from the pool
    fn get_string(&self) -> String {
        self.string_pool.lock().unwrap().get()
    }

    /// Return a string to the pool
    fn put_string(&self, s: String) {
        self.string_pool.lock().unwrap().put(s);
    }

    /// Get a vector from the pool
    fn get_vec(&self) -> Vec<u8> {
        self.vec_pool.lock().unwrap().get()
    }

    /// Return a vector to the pool
    fn put_vec(&self, v: Vec<u8>) {
        self.vec_pool.lock().unwrap().put(v);
    }

    /// Get a hashmap from the pool
    fn get_hashmap(&self) -> HashMap<String, String> {
        self.hashmap_pool.lock().unwrap().get()
    }

    /// Return a hashmap to the pool
    fn put_hashmap(&self, hm: HashMap<String, String>) {
        self.hashmap_pool.lock().unwrap().put(hm);
    }

    /// Get memory pool statistics
    fn get_stats(
        &self,
    ) -> (
        usize,
        usize,
        usize,
        usize,
        usize,
        usize,
        usize,
        usize,
        usize,
    ) {
        let string_stats = self.string_pool.lock().unwrap().stats();
        let vec_stats = self.vec_pool.lock().unwrap().stats();
        let hashmap_stats = self.hashmap_pool.lock().unwrap().stats();
        (
            string_stats.0,
            string_stats.1,
            string_stats.2,
            vec_stats.0,
            vec_stats.1,
            vec_stats.2,
            hashmap_stats.0,
            hashmap_stats.1,
            hashmap_stats.2,
        )
    }
}

#[allow(dead_code)]
impl DirtyFileTracker {
    fn new() -> Self {
        Self {
            dirty_files: HashMap::new(),
            processing_files: HashSet::new(),
            file_dependencies: HashMap::new(),
            last_compilation_time: None,
        }
    }

    /// Mark a file as dirty (modified and needs recompilation)
    fn mark_dirty(&mut self, file_path: &str) {
        self.dirty_files
            .insert(file_path.to_string(), Instant::now());
        eprintln!("DEBUG: Marked file as dirty: {}", file_path);
    }

    /// Mark a file as being processed
    fn mark_processing(&mut self, file_path: &str) {
        self.processing_files.insert(file_path.to_string());
        eprintln!("DEBUG: Marked file as processing: {}", file_path);
    }

    /// Mark a file as processed (no longer dirty)
    fn mark_processed(&mut self, file_path: &str) {
        self.dirty_files.remove(file_path);
        self.processing_files.remove(file_path);
        eprintln!("DEBUG: Marked file as processed: {}", file_path);
    }

    /// Check if a file is dirty
    fn is_dirty(&self, file_path: &str) -> bool {
        self.dirty_files.contains_key(file_path)
    }

    /// Check if a file is currently being processed
    fn is_processing(&self, file_path: &str) -> bool {
        self.processing_files.contains(file_path)
    }

    /// Get all dirty files
    fn get_dirty_files(&self) -> Vec<String> {
        self.dirty_files.keys().cloned().collect()
    }

    /// Add a dependency relationship (importer -> imported)
    fn add_dependency(&mut self, importer: &str, imported: &str) {
        self.file_dependencies
            .entry(importer.to_string())
            .or_default()
            .insert(imported.to_string());
    }

    /// Get files that depend on the given file (reverse dependencies)
    fn get_dependents(&self, file_path: &str) -> Vec<String> {
        self.file_dependencies
            .iter()
            .filter_map(|(importer, imports)| {
                if imports.contains(file_path) {
                    Some(importer.clone())
                } else {
                    None
                }
            })
            .collect()
    }

    /// Get files that the given file depends on
    fn get_dependencies(&self, file_path: &str) -> Vec<String> {
        self.file_dependencies
            .get(file_path)
            .map(|deps| deps.iter().cloned().collect())
            .unwrap_or_default()
    }

    /// Clear all dirty files and mark compilation time
    fn clear_all_dirty(&mut self) {
        self.dirty_files.clear();
        self.processing_files.clear();
        self.last_compilation_time = Some(Instant::now());
        eprintln!("DEBUG: Cleared all dirty files");
    }

    /// Get statistics about dirty file tracking
    fn get_stats(&self) -> (usize, usize, usize) {
        (
            self.dirty_files.len(),
            self.processing_files.len(),
            self.file_dependencies.len(),
        )
    }
}

/// Represents a cached symbol from the workspace
#[derive(Debug, Clone)]
#[allow(dead_code)]
struct CachedWorkspaceSymbol {
    /// Name of the symbol
    name: String,
    /// Module where the symbol is defined
    module_name: String,
    /// File path where the symbol is defined
    file_path: String,
    /// Kind of symbol (function, type, constant, etc.)
    kind: SymbolKind,
    /// Location of the symbol definition
    location: Span,
    /// Detail information about the symbol
    detail: Option<String>,
    /// Documentation for the symbol
    documentation: Option<String>,
}

/// Cache for workspace symbols to enable fast symbol search
#[derive(Debug, Clone)]
struct WorkspaceSymbolCache {
    /// All symbols indexed by name for fast lookup
    /// Key: symbol_name, Value: list of symbols with that name
    symbols_by_name: HashMap<String, Vec<CachedWorkspaceSymbol>>,

    /// Symbols indexed by module for invalidation
    /// Key: module_name, Value: list of symbols in that module
    symbols_by_module: HashMap<String, Vec<CachedWorkspaceSymbol>>,

    /// All symbols as a flat list for workspace symbol search
    all_symbols: Vec<CachedWorkspaceSymbol>,

    /// Cache timestamp for invalidation
    last_updated: Option<Instant>,
}

/// Represents a diagnostic processing task that can be executed in parallel
#[derive(Debug, Clone)]
#[allow(dead_code)]
struct DiagnosticTask {
    /// The diagnostic error to process
    error_message: String,

    /// The severity of the diagnostic
    severity: Option<miette::Severity>,

    /// Source path for the diagnostic
    source_path: Option<PathBuf>,

    /// Source code content
    source_content: Option<String>,

    /// Span information for the diagnostic
    span_info: Option<(usize, usize)>,

    /// Error code for the diagnostic
    error_code: Option<String>,

    /// Extra data for the diagnostic
    extra_data: Option<String>,

    /// Help text for the diagnostic
    help_text: Option<String>,
}

/// Parallel diagnostic processing system
/// Handles concurrent processing of multiple diagnostics using thread pools
#[derive(Debug)]
struct ParallelDiagnosticProcessor {
    /// Maximum number of parallel diagnostic processing threads
    max_threads: usize,

    /// Queue of diagnostics to be processed
    diagnostic_queue: Arc<Mutex<Vec<DiagnosticTask>>>,

    /// Flag to indicate if parallel processing is enabled
    enabled: bool,
}

impl ParallelDiagnosticProcessor {
    fn new() -> Self {
        Self {
            max_threads: num_cpus::get().min(8), // Limit to reasonable number of threads
            diagnostic_queue: Arc::new(Mutex::new(Vec::new())),
            enabled: true,
        }
    }

    /// Add a diagnostic task to the processing queue
    fn queue_diagnostic(&self, task: DiagnosticTask) {
        if let Ok(mut queue) = self.diagnostic_queue.lock() {
            queue.push(task);
        }
    }

    /// Process all queued diagnostics in parallel
    fn process_diagnostics_parallel(&self) -> Vec<(PathBuf, lsp_types::Diagnostic)> {
        let mut results = Vec::new();

        if !self.enabled {
            return results;
        }

        // Get all queued diagnostics
        let diagnostics = {
            if let Ok(mut queue) = self.diagnostic_queue.lock() {
                let tasks = queue.clone();
                queue.clear();
                tasks
            } else {
                return results;
            }
        };

        if diagnostics.is_empty() {
            return results;
        }

        eprintln!(
            "DEBUG: Processing {} diagnostics in parallel with {} threads",
            diagnostics.len(),
            self.max_threads
        );

        // Process diagnostics in parallel chunks
        let chunk_size = (diagnostics.len() / self.max_threads).max(1);
        let mut handles = Vec::new();

        for chunk in diagnostics.chunks(chunk_size) {
            let chunk_tasks = chunk.to_vec();

            let handle = thread::spawn(move || {
                let mut chunk_results = Vec::new();

                for task in chunk_tasks {
                    if let Some(diagnostic) = Self::process_single_diagnostic(task) {
                        chunk_results.push(diagnostic);
                    }
                }

                chunk_results
            });

            handles.push(handle);
        }

        // Collect results from all threads
        for handle in handles {
            if let Ok(chunk_results) = handle.join() {
                results.extend(chunk_results);
            }
        }

        eprintln!(
            "DEBUG: Parallel diagnostic processing completed. Generated {} LSP diagnostics",
            results.len()
        );

        results
    }

    /// Process a single diagnostic task
    fn process_single_diagnostic(task: DiagnosticTask) -> Option<(PathBuf, lsp_types::Diagnostic)> {
        let severity = match task.severity {
            Some(miette::Severity::Error) => lsp_types::DiagnosticSeverity::ERROR,
            Some(miette::Severity::Warning) => lsp_types::DiagnosticSeverity::WARNING,
            Some(miette::Severity::Advice) => lsp_types::DiagnosticSeverity::HINT,
            None => lsp_types::DiagnosticSeverity::ERROR,
        };

        let (source_path, source_content) = match (task.source_path, task.source_content) {
            (Some(path), Some(content)) => (path, content),
            _ => return None,
        };

        let range = if let Some((start, end)) = task.span_info {
            let line_numbers = LineNumbers::new(&source_content);
            span_to_lsp_range(Span { start, end }, &line_numbers)
        } else {
            // Default range if no span information
            lsp_types::Range {
                start: lsp_types::Position {
                    line: 0,
                    character: 0,
                },
                end: lsp_types::Position {
                    line: 0,
                    character: 0,
                },
            }
        };

        let lsp_diagnostic = lsp_types::Diagnostic {
            range,
            severity: Some(severity),
            code: task.error_code.map(lsp_types::NumberOrString::String),
            code_description: None,
            source: None,
            message: task.error_message,
            related_information: None,
            tags: None,
            data: task.extra_data.map(serde_json::Value::String),
        };

        Some((source_path, lsp_diagnostic))
    }

    /// Enable or disable parallel processing (used by set_parallel_diagnostics_enabled)
    #[allow(dead_code)]
    fn set_enabled(&mut self, enabled: bool) {
        self.enabled = enabled;
        eprintln!(
            "DEBUG: Parallel diagnostic processing {}",
            if enabled { "enabled" } else { "disabled" }
        );
    }

    /// Get the number of queued diagnostics (used by multiple processing methods)
    #[allow(dead_code)]
    fn queue_size(&self) -> usize {
        self.diagnostic_queue.lock().map(|q| q.len()).unwrap_or(0)
    }

    /// Process diagnostics in batches for better performance
    #[allow(dead_code)]
    fn process_diagnostics_batched(
        &self,
        batch_size: usize,
    ) -> Vec<(PathBuf, lsp_types::Diagnostic)> {
        let mut results = Vec::new();

        if !self.enabled {
            return results;
        }

        // Get all queued diagnostics
        let diagnostics = {
            if let Ok(mut queue) = self.diagnostic_queue.lock() {
                let tasks = queue.clone();
                queue.clear();
                tasks
            } else {
                return results;
            }
        };

        if diagnostics.is_empty() {
            return results;
        }

        eprintln!(
            "DEBUG: Processing {} diagnostics in {} batches",
            diagnostics.len(),
            diagnostics.len().div_ceil(batch_size)
        );

        // Process diagnostics in parallel batches
        let mut handles = Vec::new();

        for batch in diagnostics.chunks(batch_size) {
            let batch_tasks = batch.to_vec();

            let handle = thread::spawn(move || {
                let mut batch_results = Vec::new();

                for task in batch_tasks {
                    if let Some(diagnostic) = Self::process_single_diagnostic(task) {
                        batch_results.push(diagnostic);
                    }
                }

                batch_results
            });

            handles.push(handle);
        }

        // Collect results from all batches
        for handle in handles {
            if let Ok(batch_results) = handle.join() {
                results.extend(batch_results);
            }
        }

        eprintln!(
            "DEBUG: Batched diagnostic processing completed. Generated {} LSP diagnostics",
            results.len()
        );

        results
    }
}

impl WorkspaceSymbolCache {
    fn new() -> Self {
        Self {
            symbols_by_name: HashMap::new(),
            symbols_by_module: HashMap::new(),
            all_symbols: Vec::new(),
            last_updated: None,
        }
    }

    /// Add a symbol to the cache
    fn add_symbol(&mut self, symbol: CachedWorkspaceSymbol) {
        // Add to name-based index
        self.symbols_by_name
            .entry(symbol.name.clone())
            .or_default()
            .push(symbol.clone());

        // Add to module-based index
        self.symbols_by_module
            .entry(symbol.module_name.clone())
            .or_default()
            .push(symbol.clone());

        // Add to flat list
        self.all_symbols.push(symbol);
    }

    /// Clear symbols for a specific module
    fn clear_module(&mut self, module_name: &str) {
        // Remove from module index
        if let Some(symbols) = self.symbols_by_module.remove(module_name) {
            // Remove from name index
            for symbol in &symbols {
                if let Some(name_symbols) = self.symbols_by_name.get_mut(&symbol.name) {
                    name_symbols.retain(|s| s.module_name != module_name);
                    if name_symbols.is_empty() {
                        self.symbols_by_name.remove(&symbol.name);
                    }
                }
            }

            // Remove from flat list
            self.all_symbols.retain(|s| s.module_name != module_name);
        }

        eprintln!(
            "DEBUG: Cleared workspace symbols for module: {}",
            module_name
        );
    }

    /// Clear the entire cache
    fn clear_all(&mut self) {
        self.symbols_by_name.clear();
        self.symbols_by_module.clear();
        self.all_symbols.clear();
        self.last_updated = None;
        eprintln!("DEBUG: Cleared entire workspace symbol cache");
    }

    /// Search symbols by name pattern
    fn search_symbols(&self, pattern: &str) -> Vec<&CachedWorkspaceSymbol> {
        let pattern_lower = pattern.to_lowercase();
        self.all_symbols
            .iter()
            .filter(|symbol| symbol.name.to_lowercase().contains(&pattern_lower))
            .collect()
    }

    /// Get symbols by exact name (for future LSP features like advanced symbol search)
    /// Currently unused but kept for potential future LSP extensions
    #[allow(dead_code)]
    fn get_symbols_by_name(&self, name: &str) -> Option<&Vec<CachedWorkspaceSymbol>> {
        self.symbols_by_name.get(name)
    }

    /// Get all symbols in a module (for future module-specific operations)
    /// Currently unused but kept for potential future LSP features
    #[allow(dead_code)]
    fn get_symbols_in_module(&self, module_name: &str) -> Option<&Vec<CachedWorkspaceSymbol>> {
        self.symbols_by_module.get(module_name)
    }

    /// Get the total number of cached symbols (used internally by get_workspace_symbol_count)
    #[allow(dead_code)]
    fn symbol_count(&self) -> usize {
        self.all_symbols.len()
    }
}

impl IncrementalState {
    fn new() -> Self {
        Self {
            version: None,
            previous_content: String::new(),
            pending_changes: Vec::new(),
            is_incremental_viable: true,
        }
    }

    /// Update with new content and track changes
    fn update(&mut self, new_content: &str, version: Option<i32>) -> bool {
        let change_size = self.calculate_change_size(new_content);

        // Consider incremental parsing viable for small changes (< 20% of document)
        self.is_incremental_viable = change_size < (new_content.len() / 5);

        self.previous_content = new_content.to_string();
        self.version = version;
        self.pending_changes.clear();

        self.is_incremental_viable
    }

    /// Calculate the size of changes between old and new content
    fn calculate_change_size(&self, new_content: &str) -> usize {
        // Simple diff - count character differences
        let old_chars: Vec<char> = self.previous_content.chars().collect();
        let new_chars: Vec<char> = new_content.chars().collect();

        let mut changes = 0;
        let max_len = old_chars.len().max(new_chars.len());

        for i in 0..max_len {
            let old_char = old_chars.get(i);
            let new_char = new_chars.get(i);

            if old_char != new_char {
                changes += 1;
            }
        }

        changes
    }
}

#[allow(dead_code)]
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

    /// Cache for parsed ASTs to avoid re-parsing the same content
    /// Key: (file_path, file_content_hash)
    /// Value: parsed UntypedModule
    ast_cache: HashMap<(String, u64), UntypedModule>,

    /// Compilation debouncing state
    /// Tracks the last save time to debounce compilation requests
    last_save_time: Option<Instant>,

    /// Pending compilation flag to prevent duplicate compilation triggers
    compilation_pending: bool,

    /// Incremental parsing state per file
    /// Key: file_path, Value: incremental state tracking
    incremental_states: HashMap<String, IncrementalState>,
    /// Workspace symbol cache for fast symbol search across all modules
    /// Enables efficient workspace-wide symbol lookup and references
    workspace_symbol_cache: WorkspaceSymbolCache,
    /// Parallel diagnostic processing system for improved performance
    /// Processes multiple diagnostics concurrently using thread pools
    parallel_diagnostic_processor: ParallelDiagnosticProcessor,
    /// Dirty file tracking system to identify files needing recompilation
    /// Tracks file modifications and dependencies for incremental compilation
    dirty_file_tracker: DirtyFileTracker,
    /// Memory pool manager for frequent allocations to reduce fragmentation
    /// Provides pooled allocation for strings, vectors, and hashmaps
    memory_pool_manager: MemoryPoolManager,
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
            } else {
                // Rebuild workspace symbol cache after successful compilation
                eprintln!("DEBUG: Compilation successful, rebuilding workspace symbol cache");
                self.rebuild_workspace_symbol_cache();

                // Clear all dirty files after successful compilation
                self.dirty_file_tracker.clear_all_dirty();
            }
        }

        self.notify_client_of_compilation_end(connection)?;

        Ok(())
    }

    /// Compile the project with parallel diagnostic processing
    #[allow(clippy::result_large_err)]
    fn compile_parallel(&mut self, connection: &Connection) -> Result<(), ServerError> {
        self.notify_client_of_compilation_start(connection)?;

        if let Some(compiler) = self.compiler.as_mut() {
            let result = compiler.compile();

            // Queue warnings for parallel processing
            for warning in compiler.project.warnings() {
                if let Some(task) = self.diagnostic_to_task(warning) {
                    self.parallel_diagnostic_processor.queue_diagnostic(task);
                }
            }

            // Queue errors for parallel processing
            if let Err(errs) = result {
                for err in errs {
                    if let Some(task) = self.diagnostic_to_task(err) {
                        self.parallel_diagnostic_processor.queue_diagnostic(task);
                    }
                }
            } else {
                // Rebuild workspace symbol cache after successful compilation
                eprintln!("DEBUG: Compilation successful, rebuilding workspace symbol cache");
                self.rebuild_workspace_symbol_cache();
            }

            // Process all queued diagnostics in parallel
            let parallel_results = self
                .parallel_diagnostic_processor
                .process_diagnostics_parallel();

            // Add parallel results to stored diagnostics
            for (path, diagnostic) in parallel_results {
                self.push_diagnostic(path, diagnostic);
            }
        }

        self.notify_client_of_compilation_end(connection)?;

        Ok(())
    }

    /// Set whether parallel diagnostic processing is enabled (for configuration)
    /// Currently unused but kept for potential configuration LSP extensions
    #[allow(dead_code)]
    fn set_parallel_diagnostics_enabled(&mut self, enabled: bool) {
        self.parallel_diagnostic_processor.set_enabled(enabled);
    }

    /// Get parallel diagnostic processing status
    fn is_parallel_diagnostics_enabled(&self) -> bool {
        self.parallel_diagnostic_processor.enabled
    }

    /// Compile with automatic fallback between parallel and sequential processing
    #[allow(clippy::result_large_err)]
    fn compile_with_fallback(&mut self, connection: &Connection) -> Result<(), ServerError> {
        if self.is_parallel_diagnostics_enabled() {
            // Check queue size to determine processing strategy
            let queue_size = self.parallel_diagnostic_processor.queue_size();

            let result = if queue_size > 20 {
                // Use batched parallel processing for large numbers of diagnostics
                eprintln!(
                    "DEBUG: Using batched parallel processing for {} diagnostics",
                    queue_size
                );
                self.compile_parallel_batched(connection)
            } else if queue_size > 5 {
                // Use regular parallel processing for moderate numbers
                eprintln!(
                    "DEBUG: Using regular parallel processing for {} diagnostics",
                    queue_size
                );
                self.compile_parallel(connection)
            } else {
                // Use sequential processing for small numbers
                eprintln!(
                    "DEBUG: Using sequential processing for {} diagnostics",
                    queue_size
                );
                self.compile(connection)
            };

            match result {
                Ok(()) => {
                    eprintln!("DEBUG: Parallel diagnostic processing completed successfully");
                    Ok(())
                }
                Err(e) => {
                    eprintln!(
                        "DEBUG: Parallel diagnostic processing failed: {:?}, falling back to sequential",
                        e
                    );
                    // Fall back to sequential processing
                    self.compile(connection)
                }
            }
        } else {
            // Use sequential processing
            self.compile(connection)
        }
    }

    /// Compile with batched parallel diagnostic processing for large numbers of diagnostics
    #[allow(clippy::result_large_err)]
    fn compile_parallel_batched(&mut self, connection: &Connection) -> Result<(), ServerError> {
        self.notify_client_of_compilation_start(connection)?;

        if let Some(compiler) = self.compiler.as_mut() {
            let result = compiler.compile();

            // Queue warnings for parallel processing
            for warning in compiler.project.warnings() {
                if let Some(task) = self.diagnostic_to_task(warning) {
                    self.parallel_diagnostic_processor.queue_diagnostic(task);
                }
            }

            // Queue errors for parallel processing
            if let Err(errs) = result {
                for err in errs {
                    if let Some(task) = self.diagnostic_to_task(err) {
                        self.parallel_diagnostic_processor.queue_diagnostic(task);
                    }
                }
            } else {
                // Rebuild workspace symbol cache after successful compilation
                eprintln!("DEBUG: Compilation successful, rebuilding workspace symbol cache");
                self.rebuild_workspace_symbol_cache();
            }

            // Process diagnostics using batched parallel processing
            let queue_size = self.parallel_diagnostic_processor.queue_size();
            let batch_size = if queue_size > 50 { 10 } else { 5 }; // Adaptive batch size

            let parallel_results = self
                .parallel_diagnostic_processor
                .process_diagnostics_batched(batch_size);

            // Add parallel results to stored diagnostics
            for (path, diagnostic) in parallel_results {
                self.push_diagnostic(path, diagnostic);
            }
        }

        self.notify_client_of_compilation_end(connection)?;

        Ok(())
    }

    /// Get parallel diagnostic processing statistics (for monitoring/debugging)
    /// Currently unused but kept for potential monitoring LSP extensions
    #[allow(dead_code)]
    fn get_parallel_diagnostic_stats(&self) -> (usize, bool, usize) {
        let queue_size = self.parallel_diagnostic_processor.queue_size();
        let enabled = self.parallel_diagnostic_processor.enabled;
        let max_threads = self.parallel_diagnostic_processor.max_threads;
        (queue_size, enabled, max_threads)
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

    /// Calculate hash of file content for caching
    fn calculate_content_hash(content: &str) -> u64 {
        let mut hasher = DefaultHasher::new();
        content.hash(&mut hasher);
        hasher.finish()
    }

    /// Get or parse module with caching
    fn get_or_parse_module(
        &mut self,
        file_path: &str,
        content: &str,
    ) -> Result<UntypedModule, Vec<aiken_project::error::Error>> {
        let content_hash = Self::calculate_content_hash(content);
        let cache_key = (file_path.to_string(), content_hash);

        // Check if we have cached AST for this content
        if let Some(cached_module) = self.ast_cache.get(&cache_key) {
            eprintln!("DEBUG: Using cached AST for file: {}", file_path);
            return Ok(cached_module.clone());
        }

        eprintln!("DEBUG: Parsing new AST for file: {}", file_path);

        // Use pooled string for module kind conversion if needed
        let module_kind = ModuleKind::Lib;

        // Parse the module
        let (module, _extra) = match parser::module(content, module_kind) {
            Ok(result) => result,
            Err(errs) => {
                let project_errors = aiken_project::error::Error::from_parse_errors(
                    errs,
                    Path::new(file_path),
                    content,
                );
                return Err(project_errors);
            }
        };

        // Cache the parsed result
        self.ast_cache.insert(cache_key, module.clone());

        // Limit cache size to prevent memory growth
        if self.ast_cache.len() > 100 {
            // Remove oldest entries (this is a simple strategy, could be improved with LRU)
            let keys_to_remove: Vec<_> = self.ast_cache.keys().take(20).cloned().collect();
            for key in keys_to_remove {
                self.ast_cache.remove(&key);
            }
        }

        Ok(module)
    }

    /// Clear AST cache entries for a specific file
    fn invalidate_ast_cache_for_file(&mut self, file_path: &str) {
        self.ast_cache.retain(|(path, _), _| path != file_path);
        eprintln!("DEBUG: Invalidated AST cache for file: {}", file_path);
    }

    /// Check if incremental parsing is viable for a file
    fn is_incremental_parsing_viable(&mut self, file_path: &str, content: &str) -> bool {
        let state = self
            .incremental_states
            .entry(file_path.to_string())
            .or_insert_with(IncrementalState::new);

        state.update(content, None)
    }

    /// Get or parse module with incremental parsing support
    fn get_or_parse_module_incremental(
        &mut self,
        file_path: &str,
        content: &str,
    ) -> Result<UntypedModule, Vec<aiken_project::error::Error>> {
        // Check if we can use incremental parsing
        let is_incremental_viable = self.is_incremental_parsing_viable(file_path, content);

        if is_incremental_viable {
            eprintln!(
                "DEBUG: Attempting incremental parsing for file: {}",
                file_path
            );

            // Try to get cached AST first
            let content_hash = Self::calculate_content_hash(content);
            let cache_key = (file_path.to_string(), content_hash);

            if let Some(cached_module) = self.ast_cache.get(&cache_key) {
                eprintln!(
                    "DEBUG: Using cached AST for incremental parsing: {}",
                    file_path
                );
                return Ok(cached_module.clone());
            }

            // For small changes, we could implement AST patching here
            // For now, fall back to full parsing but with better caching
            eprintln!("DEBUG: Incremental parsing viable but no cached AST, performing full parse");
        } else {
            eprintln!(
                "DEBUG: Changes too large for incremental parsing, performing full parse: {}",
                file_path
            );
        }

        // Fall back to existing caching mechanism
        self.get_or_parse_module(file_path, content)
    }

    /// Perform incremental AST update (placeholder for future implementation)
    /// TODO: Implement actual incremental AST patching for Issue 1
    #[allow(dead_code)]
    fn update_ast_incrementally(
        &mut self,
        _file_path: &str,
        _base_ast: &UntypedModule,
        _changes: &[TextChange],
    ) -> Option<UntypedModule> {
        // TODO: Implement actual incremental AST patching
        // This would involve:
        // 1. Analyzing the changes to determine affected AST nodes
        // 2. Re-parsing only the changed portions
        // 3. Updating the AST structure with new nodes
        // 4. Maintaining AST consistency and references

        eprintln!("DEBUG: Incremental AST update not yet implemented, falling back to full parse");
        None
    }

    /// Clear incremental state for a file when major changes occur
    fn clear_incremental_state(&mut self, file_path: &str) {
        if let Some(state) = self.incremental_states.get_mut(file_path) {
            state.is_incremental_viable = false;
            state.pending_changes.clear();
            eprintln!("DEBUG: Cleared incremental state for file: {}", file_path);
        }
    }

    /// Async file reading to prevent blocking the main LSP thread
    fn read_file_async(file_path: &str) -> Result<String, ProjectError> {
        // For now, we'll use std::fs since full async requires tokio runtime
        // This is a step towards async - in full implementation, this would be tokio::fs::read_to_string
        eprintln!("DEBUG: Reading file asynchronously: {}", file_path);

        #[cfg(not(target_os = "windows"))]
        {
            fs::read_to_string(file_path).map_err(ProjectError::from)
        }
        #[cfg(target_os = "windows")]
        {
            let temp = match urlencoding::decode(file_path) {
                Ok(decoded) => decoded.to_string(),
                Err(_) => file_path.to_owned(),
            };
            fs::read_to_string(temp.trim_start_matches("/")).map_err(ProjectError::from)
        }
    }

    /// Schedule a debounced compilation
    #[allow(clippy::result_large_err)]
    fn schedule_debounced_compilation(
        &mut self,
        _connection: &Connection,
    ) -> Result<(), ServerError> {
        self.last_save_time = Some(Instant::now());

        if self.compilation_pending {
            eprintln!("DEBUG: Compilation already pending, updating timestamp");
            return Ok(());
        }

        self.compilation_pending = true;
        eprintln!("DEBUG: Scheduling debounced compilation in 500ms");

        Ok(())
    }

    /// Execute compilation with debounce logic
    #[allow(clippy::result_large_err)]
    fn execute_debounced_compilation(
        &mut self,
        connection: &Connection,
    ) -> Result<(), ServerError> {
        const DEBOUNCE_DURATION: Duration = Duration::from_millis(500);

        if let Some(last_save) = self.last_save_time {
            let elapsed = last_save.elapsed();

            if elapsed < DEBOUNCE_DURATION {
                eprintln!(
                    "DEBUG: Compilation debounced, only {}ms elapsed, need {}ms",
                    elapsed.as_millis(),
                    DEBOUNCE_DURATION.as_millis()
                );

                // Use a simple thread::sleep to implement debouncing
                // In a full async implementation, this would be tokio::time::sleep
                let remaining = DEBOUNCE_DURATION - elapsed;
                eprintln!("DEBUG: Sleeping for additional {}ms", remaining.as_millis());
                thread::sleep(remaining);
            }
        }

        self.compilation_pending = false;
        eprintln!("DEBUG: Executing debounced compilation");

        self.compile_with_fallback(connection)?;
        self.publish_stored_diagnostics(connection)?;

        Ok(())
    }

    fn format(
        &mut self,
        params: DocumentFormattingParams,
    ) -> Result<Vec<TextEdit>, Vec<ProjectError>> {
        let path = params.text_document.uri.path();
        let mut new_text = String::new();

        let src = match self.edited.get(path) {
            Some(src) => src.clone(),
            None => {
                // Use async file reading to prevent blocking
                Self::read_file_async(path)?
            }
        };

        // Use incremental parsing for better performance
        match self.get_or_parse_module_incremental(path, &src) {
            Ok(module) => {
                // We still need to call parser::module to get the extra data needed for formatting
                // TODO: Cache extra data as well in future improvement
                let (_cached_module, extra) =
                    parser::module(&src, ModuleKind::Lib).map_err(|errs| {
                        aiken_project::error::Error::from_parse_errors(errs, Path::new(path), &src)
                    })?;

                aiken_lang::format::pretty(&mut new_text, module, extra, &src);
            }
            Err(errs) => return Err(errs),
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

                let file_path = params.text_document.uri.path();
                self.edited.remove(file_path);

                // Invalidate AST cache for saved file since it may have changed on disk
                self.invalidate_ast_cache_for_file(file_path);

                // Clear incremental state since file was saved (potentially major changes)
                self.clear_incremental_state(file_path);

                // Invalidate workspace symbol cache for the affected module
                if let Some(module_name) = uri_to_module_name(&params.text_document.uri, &self.root)
                {
                    self.invalidate_workspace_symbol_cache_for_module(&module_name);
                }

                // Mark file as processed after save
                self.mark_file_processed(file_path);

                // Use debounced compilation instead of immediate compilation
                self.schedule_debounced_compilation(connection)?;

                // Check if enough time has passed to execute compilation
                self.execute_debounced_compilation(connection)?;

                Ok(())
            }

            DidChangeTextDocument::METHOD => {
                let params = cast_notification::<DidChangeTextDocument>(notification)?;

                // A file has changed in the editor so store a copy of the new content in memory
                let path = params.text_document.uri.path().to_string();

                if let Some(changes) = params.content_changes.into_iter().next() {
                    // Check if this is a small incremental change
                    let is_incremental_viable =
                        self.is_incremental_parsing_viable(&path, &changes.text);

                    if !is_incremental_viable {
                        // Large changes - invalidate cache and clear incremental state
                        self.invalidate_ast_cache_for_file(&path);
                        self.clear_incremental_state(&path);
                        eprintln!("DEBUG: Large change detected in {}, clearing caches", path);
                    } else {
                        eprintln!(
                            "DEBUG: Small incremental change detected in {}, preserving state",
                            path
                        );
                    }

                    // Mark file as dirty for recompilation tracking
                    self.mark_file_dirty(&path);

                    self.edited.insert(path, changes.text);
                }

                Ok(())
            }

            DidCloseTextDocument::METHOD => {
                let params = cast_notification::<DidCloseTextDocument>(notification)?;

                let file_path = params.text_document.uri.path();
                self.edited.remove(file_path);

                // Invalidate AST cache for closed file
                self.invalidate_ast_cache_for_file(file_path);

                // Clear incremental state for closed file
                self.clear_incremental_state(file_path);

                Ok(())
            }

            DidChangeWatchedFiles::METHOD => {
                if let Ok(config) = ProjectConfig::load(&self.root) {
                    self.config = Some(config);
                    self.create_new_compiler();

                    // Clear entire AST cache when project configuration changes
                    self.ast_cache.clear();

                    // Clear all incremental states since project config changed
                    self.incremental_states.clear();

                    // Clear workspace symbol cache since project config changed
                    self.workspace_symbol_cache.clear_all();

                    eprintln!(
                        "DEBUG: Cleared entire AST cache, incremental states, and workspace symbol cache due to project config change"
                    );

                    // Use debounced compilation for watched file changes too
                    self.schedule_debounced_compilation(connection)?;
                    self.execute_debounced_compilation(connection)?;
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

            WorkspaceSymbolRequest::METHOD => {
                let params = cast_request::<WorkspaceSymbolRequest>(request)?;

                let symbols = self.search_workspace_symbols(&params.query);

                Ok(lsp_server::Response {
                    id,
                    error: None,
                    result: Some(serde_json::to_value(symbols)?),
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
        if params.context.include_declaration {
            if let Some(def_location) =
                self.find_definition_location(&symbol_name, module, &line_numbers)
            {
                references.push(def_location);
            }
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
        if let Some(compiler) = &self.compiler {
            if let Some(source) = compiler.sources.get(module_name) {
                return url::Url::parse(&format!("file:///{}", source.path)).ok();
            }
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
                if name == symbol_name {
                    if let Some(uri) = self.module_name_to_uri(module_name) {
                        let range = span_to_lsp_range(*location, line_numbers);
                        locations.push(lsp_types::Location { uri, range });
                    }
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

        // Compile the project once so we have all the state and any initial errors
        self.compile_with_fallback(&connection)?;
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
            ast_cache: HashMap::new(),
            last_save_time: None,
            compilation_pending: false,
            incremental_states: HashMap::new(),
            workspace_symbol_cache: WorkspaceSymbolCache::new(),
            parallel_diagnostic_processor: ParallelDiagnosticProcessor::new(),
            dirty_file_tracker: DirtyFileTracker {
                dirty_files: HashMap::new(),
                processing_files: HashSet::new(),
                file_dependencies: HashMap::new(),
                last_compilation_time: None,
            },
            memory_pool_manager: MemoryPoolManager::new(),
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

    /// Convert an Aiken diagnostic to a DiagnosticTask for parallel processing
    fn diagnostic_to_task<E>(&self, error: E) -> Option<DiagnosticTask>
    where
        E: Diagnostic + GetSource + ExtraData,
    {
        let severity = error.severity();

        let message = match error.source() {
            Some(err) => err.to_string(),
            None => error.to_string(),
        };

        let (source_path, source_content, span_info) =
            if let (Some(mut labels), Some(path), Some(src)) =
                (error.labels(), error.path(), error.src())
            {
                if let Some(labeled_span) = labels.next() {
                    let span_start = labeled_span.inner().offset();
                    let span_end = span_start + labeled_span.inner().len();

                    #[cfg(not(target_os = "windows"))]
                    let canonical_path = path.canonicalize().ok()?;
                    #[cfg(target_os = "windows")]
                    let canonical_path = path.to_path_buf();

                    (
                        Some(canonical_path),
                        Some(src),
                        Some((span_start, span_end)),
                    )
                } else {
                    (None, None, None)
                }
            } else {
                (None, None, None)
            };

        let error_code = error.code().map(|c| {
            c.to_string()
                .trim()
                .replace("Warning ", "")
                .replace("Error ", "")
        });

        let extra_data = error.extra_data();
        let help_text = error.help().map(|h| h.to_string());

        Some(DiagnosticTask {
            error_message: message,
            severity,
            source_path,
            source_content,
            span_info,
            error_code,
            extra_data,
            help_text,
        })
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

    /// Extract symbols from a TypedDefinition and convert to WorkspaceSymbol
    fn extract_workspace_symbols_from_definition(
        &self,
        definition: &TypedDefinition,
        module_name: &str,
        file_path: &str,
    ) -> Vec<CachedWorkspaceSymbol> {
        use aiken_lang::ast::Definition;

        let mut symbols = Vec::new();

        match definition {
            Definition::Fn(function) => {
                symbols.push(CachedWorkspaceSymbol {
                    name: function.name.clone(),
                    module_name: module_name.to_string(),
                    file_path: file_path.to_string(),
                    kind: SymbolKind::FUNCTION,
                    location: function.location,
                    detail: Some(format!("fn {}", function.name)),
                    documentation: function.doc.clone(),
                });
            }
            Definition::Test(test) => {
                symbols.push(CachedWorkspaceSymbol {
                    name: test.name.clone(),
                    module_name: module_name.to_string(),
                    file_path: file_path.to_string(),
                    kind: SymbolKind::METHOD,
                    location: test.location,
                    detail: Some(format!("test {}", test.name)),
                    documentation: test.doc.clone(),
                });
            }
            Definition::Benchmark(benchmark) => {
                symbols.push(CachedWorkspaceSymbol {
                    name: benchmark.name.clone(),
                    module_name: module_name.to_string(),
                    file_path: file_path.to_string(),
                    kind: SymbolKind::METHOD,
                    location: benchmark.location,
                    detail: Some(format!("benchmark {}", benchmark.name)),
                    documentation: benchmark.doc.clone(),
                });
            }
            Definition::TypeAlias(type_alias) => {
                symbols.push(CachedWorkspaceSymbol {
                    name: type_alias.alias.clone(),
                    module_name: module_name.to_string(),
                    file_path: file_path.to_string(),
                    kind: SymbolKind::TYPE_PARAMETER,
                    location: type_alias.location,
                    detail: Some(format!("type {}", type_alias.alias)),
                    documentation: type_alias.doc.clone(),
                });
            }
            Definition::DataType(data_type) => {
                // Add the main data type
                symbols.push(CachedWorkspaceSymbol {
                    name: data_type.name.clone(),
                    module_name: module_name.to_string(),
                    file_path: file_path.to_string(),
                    kind: SymbolKind::CLASS,
                    location: data_type.location,
                    detail: Some(format!("type {}", data_type.name)),
                    documentation: data_type.doc.clone(),
                });

                // Add constructors as separate symbols
                for constructor in &data_type.constructors {
                    symbols.push(CachedWorkspaceSymbol {
                        name: constructor.name.clone(),
                        module_name: module_name.to_string(),
                        file_path: file_path.to_string(),
                        kind: SymbolKind::CONSTRUCTOR,
                        location: constructor.location,
                        detail: Some(format!("constructor {}", constructor.name)),
                        documentation: constructor.doc.clone(),
                    });
                }
            }
            Definition::ModuleConstant(constant) => {
                symbols.push(CachedWorkspaceSymbol {
                    name: constant.name.clone(),
                    module_name: module_name.to_string(),
                    file_path: file_path.to_string(),
                    kind: SymbolKind::CONSTANT,
                    location: constant.location,
                    detail: Some(format!("const {}", constant.name)),
                    documentation: constant.doc.clone(),
                });
            }
            Definition::Validator(validator) => {
                symbols.push(CachedWorkspaceSymbol {
                    name: validator.name.clone(),
                    module_name: module_name.to_string(),
                    file_path: file_path.to_string(),
                    kind: SymbolKind::CLASS,
                    location: validator.location,
                    detail: Some(format!("validator {}", validator.name)),
                    documentation: validator.doc.clone(),
                });
            }
            Definition::Use(_) => {
                // Skip use statements for workspace symbols
            }
        }

        symbols
    }

    /// Rebuild workspace symbol cache from all compiled modules
    fn rebuild_workspace_symbol_cache(&mut self) {
        eprintln!("DEBUG: Rebuilding workspace symbol cache");

        self.workspace_symbol_cache.clear_all();

        if let Some(compiler) = &self.compiler {
            let mut total_symbols = 0;

            for (module_name, checked_module) in &compiler.modules {
                let file_path = compiler
                    .sources
                    .get(module_name)
                    .map(|source| source.path.clone())
                    .unwrap_or_else(|| module_name.clone());

                for definition in &checked_module.ast.definitions {
                    let symbols = self.extract_workspace_symbols_from_definition(
                        definition,
                        module_name,
                        &file_path,
                    );

                    for symbol in symbols {
                        self.workspace_symbol_cache.add_symbol(symbol);
                        total_symbols += 1;
                    }
                }
            }

            self.workspace_symbol_cache.last_updated = Some(Instant::now());
            eprintln!(
                "DEBUG: Rebuilt workspace symbol cache with {} symbols from {} modules",
                total_symbols,
                compiler.modules.len()
            );
        } else {
            eprintln!("DEBUG: No compiler available for workspace symbol cache rebuild");
        }
    }

    /// Update workspace symbol cache for a specific module (for workspace management)
    /// Currently unused but kept for potential workspace management LSP features
    #[allow(dead_code)]
    fn update_workspace_symbol_cache_for_module(&mut self, module_name: &str) {
        eprintln!(
            "DEBUG: Updating workspace symbol cache for module: {}",
            module_name
        );

        // Clear existing symbols for this module
        self.workspace_symbol_cache.clear_module(module_name);

        if let Some(compiler) = &self.compiler {
            if let Some(checked_module) = compiler.modules.get(module_name) {
                let file_path = compiler
                    .sources
                    .get(module_name)
                    .map(|source| source.path.clone())
                    .unwrap_or_else(|| module_name.to_string());

                let mut symbols_added = 0;

                for definition in &checked_module.ast.definitions {
                    let symbols = self.extract_workspace_symbols_from_definition(
                        definition,
                        module_name,
                        &file_path,
                    );

                    for symbol in symbols {
                        self.workspace_symbol_cache.add_symbol(symbol);
                        symbols_added += 1;
                    }
                }

                eprintln!(
                    "DEBUG: Added {} symbols to workspace cache for module: {}",
                    symbols_added, module_name
                );
            } else {
                eprintln!(
                    "DEBUG: Module {} not found in compiler, cannot update symbol cache",
                    module_name
                );
            }
        }
    }

    /// Invalidate workspace symbol cache for a specific module
    fn invalidate_workspace_symbol_cache_for_module(&mut self, module_name: &str) {
        self.workspace_symbol_cache.clear_module(module_name);
        eprintln!(
            "DEBUG: Invalidated workspace symbol cache for module: {}",
            module_name
        );
    }

    /// Search workspace symbols by pattern (for workspace symbol request)
    #[allow(deprecated)]
    fn search_workspace_symbols(&self, query: &str) -> Vec<lsp_types::SymbolInformation> {
        eprintln!("DEBUG: Searching workspace symbols for query: '{}'", query);

        let symbols = self.workspace_symbol_cache.search_symbols(query);
        let mut result = Vec::new();

        for symbol in symbols {
            match path_to_uri(PathBuf::from(&symbol.file_path)) {
                Ok(uri) => {
                    // Convert our custom Span to LSP Range (simplified)
                    let range = Range {
                        start: lsp_types::Position {
                            line: 0, // We'd need line_numbers to convert properly
                            character: 0,
                        },
                        end: lsp_types::Position {
                            line: 0,
                            character: 0,
                        },
                    };

                    let location = lsp_types::Location { uri, range };

                    let symbol_info = lsp_types::SymbolInformation {
                        name: symbol.name.clone(),
                        kind: symbol.kind,
                        tags: None,
                        deprecated: None,
                        location,
                        container_name: Some(symbol.module_name.clone()),
                    };

                    result.push(symbol_info);
                }
                Err(_) => {
                    eprintln!("DEBUG: Failed to convert path to URI: {}", symbol.file_path);
                }
            }
        }

        eprintln!(
            "DEBUG: Found {} workspace symbols matching query '{}'",
            result.len(),
            query
        );

        // Limit results to prevent overwhelming the client
        result.truncate(100);
        result
    }

    /// Mark a file as dirty when it changes
    fn mark_file_dirty(&mut self, file_path: &str) {
        self.dirty_file_tracker.mark_dirty(file_path);

        // Also mark dependent files as dirty
        let dependents = self.dirty_file_tracker.get_dependents(file_path);
        for dependent in dependents {
            self.dirty_file_tracker.mark_dirty(&dependent);
        }
    }

    /// Check if a file needs recompilation
    #[allow(dead_code)]
    fn needs_recompilation(&self, file_path: &str) -> bool {
        self.dirty_file_tracker.is_dirty(file_path)
    }

    /// Mark file as processed after successful compilation
    fn mark_file_processed(&mut self, file_path: &str) {
        self.dirty_file_tracker.mark_processed(file_path);
    }

    /// Get pooled string for temporary use
    #[allow(dead_code)]
    fn get_pooled_string(&self) -> String {
        self.memory_pool_manager.get_string()
    }

    /// Return pooled string for reuse
    #[allow(dead_code)]
    fn return_pooled_string(&self, s: String) {
        self.memory_pool_manager.put_string(s);
    }

    /// Get pooled vector for temporary use
    #[allow(dead_code)]
    fn get_pooled_vec(&self) -> Vec<u8> {
        self.memory_pool_manager.get_vec()
    }

    /// Return pooled vector for reuse
    #[allow(dead_code)]
    fn return_pooled_vec(&self, v: Vec<u8>) {
        self.memory_pool_manager.put_vec(v);
    }

    /// Get pooled hashmap for temporary use
    #[allow(dead_code)]
    fn get_pooled_hashmap(&self) -> HashMap<String, String> {
        self.memory_pool_manager.get_hashmap()
    }

    /// Return pooled hashmap for reuse
    #[allow(dead_code)]
    fn return_pooled_hashmap(&self, hm: HashMap<String, String>) {
        self.memory_pool_manager.put_hashmap(hm);
    }

    /// Demonstrate memory pool usage with diagnostic processing
    #[allow(dead_code)]
    fn process_diagnostics_with_memory_pool(
        &mut self,
        diagnostics: &[aiken_project::error::Error],
    ) -> Vec<lsp_types::Diagnostic> {
        let mut results = Vec::new();

        for diagnostic in diagnostics {
            // Use pooled string for temporary message storage
            let mut message_buffer = self.get_pooled_string();
            message_buffer.push_str(&format!("{:?}", diagnostic));

            // Use pooled vector for temporary data processing
            let mut temp_data = self.get_pooled_vec();
            temp_data.extend(message_buffer.as_bytes());

            // Process the diagnostic
            if let Some(lsp_diagnostic) = self.create_lsp_diagnostic_from_error(diagnostic) {
                results.push(lsp_diagnostic);
            }

            // Return pooled resources
            self.return_pooled_vec(temp_data);
            self.return_pooled_string(message_buffer);
        }

        results
    }

    /// Helper method to create LSP diagnostic from project error
    #[allow(dead_code)]
    fn create_lsp_diagnostic_from_error(
        &self,
        _error: &aiken_project::error::Error,
    ) -> Option<lsp_types::Diagnostic> {
        // Simplified implementation - in real usage this would convert the error properly
        Some(lsp_types::Diagnostic {
            range: lsp_types::Range {
                start: lsp_types::Position {
                    line: 0,
                    character: 0,
                },
                end: lsp_types::Position {
                    line: 0,
                    character: 0,
                },
            },
            severity: Some(lsp_types::DiagnosticSeverity::ERROR),
            code: None,
            code_description: None,
            source: Some("aiken".to_string()),
            message: "Sample diagnostic".to_string(),
            related_information: None,
            tags: Some(vec![]),
            data: None,
        })
    }

    /// Get performance statistics
    #[allow(dead_code)]
    fn get_performance_stats(
        &self,
    ) -> (
        usize,
        usize,
        usize,
        usize,
        usize,
        usize,
        usize,
        usize,
        usize,
        usize,
        usize,
        usize,
    ) {
        let dirty_stats = self.dirty_file_tracker.get_stats();
        let memory_stats = self.memory_pool_manager.get_stats();
        (
            dirty_stats.0,
            dirty_stats.1,
            dirty_stats.2,
            memory_stats.0,
            memory_stats.1,
            memory_stats.2,
            memory_stats.3,
            memory_stats.4,
            memory_stats.5,
            memory_stats.6,
            memory_stats.7,
            memory_stats.8,
        )
    }
}
