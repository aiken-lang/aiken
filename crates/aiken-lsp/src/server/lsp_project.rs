use aiken_lang::tipo::TypeInfo;
use aiken_lang::{ast::Tracing, line_numbers::LineNumbers, test_framework::PropertyTest};
use aiken_project::{
    config::ProjectConfig, error::Error as ProjectError, module::CheckedModule,
    telemetry::CoverageMode, Project,
};
use std::{
    collections::{hash_map::DefaultHasher, HashMap},
    hash::{Hash, Hasher},
    path::PathBuf,
};

#[derive(Debug)]
pub struct SourceInfo {
    /// The path to the source file from within the project root
    pub path: String,
    /// Useful for converting from Aiken's byte index offsets to the LSP line
    /// and column number positions.
    pub line_numbers: LineNumbers,
}

#[derive(Clone, Default)]
pub struct LspDepCache {
    pub type_infos: HashMap<String, TypeInfo>,
    pub checked_modules: HashMap<String, CheckedModule>,
    pub deps_hash: u64,
    pub own_module_hints: HashMap<String, (u64, TypeInfo)>,
    pub own_checked_modules: HashMap<String, CheckedModule>,
}

// SAFETY: `LspDepCache` contains `TypeInfo` and `CheckedModule` which hold
// `Rc<Type>` and `Rc<RefCell<TypeVar>>` — non-atomic types that are `!Send`.
//
// This `unsafe impl` is required because the LSP server clones the cache and
// moves the clone into a `std::thread::spawn` worker for background compilation
// (see `server.rs`). The invariant that makes this *currently* safe in practice:
//
//   1. The clone transferred to the worker thread is fully owned by that thread;
//      no aliases from the cloned `Rc`s are accessible on the main thread after
//      the move (the original `Rc`s remain on the main thread, the cloned ones
//      go to the worker).
//   2. The worker constructs a fresh `LspProject` and runs compilation to
//      completion before the result is sent back — no concurrent access to the
//      same `Rc` across threads occurs at runtime.
//   3. `TypeInfo` values stored in the cache are treated as frozen after
//      type-inference; no code path mutates the inner `RefCell<TypeVar>` once
//      a value is inserted.
//
// This is still *technically* unsound because Rust's `!Send` on `Rc` protects
// against non-atomic reference-count mutations, and those can race if any alias
// crosses the thread boundary — even if the aliased data is never accessed.
// The long-term correct fix is to replace `Rc` / `RefCell` with `Arc` /
// `RwLock` throughout `aiken-lang`'s type system, or to redesign the caching
// layer so no `!Send` AST values cross thread boundaries at all.
unsafe impl Send for LspDepCache {}

pub struct LspProject {
    pub project: Project<super::telemetry::Lsp>,
    pub modules: HashMap<String, CheckedModule>,
    pub sources: HashMap<String, SourceInfo>,
    own_package: String,
    dep_cache: LspDepCache,
}

impl LspProject {
    pub fn new(config: ProjectConfig, root: PathBuf, telemetry: super::telemetry::Lsp) -> Self {
        let own_package = config.name.to_string();
        Self {
            project: Project::new_with_config(config, root, telemetry),
            modules: HashMap::new(),
            sources: HashMap::new(),
            own_package,
            dep_cache: LspDepCache::default(),
        }
    }

    pub fn new_with_cache(
        config: ProjectConfig,
        root: PathBuf,
        telemetry: super::telemetry::Lsp,
        dep_cache: LspDepCache,
    ) -> Self {
        let own_package = config.name.to_string();
        Self {
            project: Project::new_with_config(config, root, telemetry),
            modules: HashMap::new(),
            sources: HashMap::new(),
            own_package,
            dep_cache,
        }
    }

    pub fn dep_cache(&self) -> &LspDepCache {
        &self.dep_cache
    }

    pub fn compile(&mut self) -> Result<(), Vec<ProjectError>> {
        let mut hasher = DefaultHasher::new();
        self.project.config().dependencies.hash(&mut hasher);
        let current_deps_hash = hasher.finish();

        let checkpoint = self.project.checkpoint();

        if !self.dep_cache.type_infos.is_empty() && self.dep_cache.deps_hash != current_deps_hash {
            self.dep_cache = LspDepCache::default();
        }

        let has_cache = !self.dep_cache.type_infos.is_empty();
        if has_cache {
            self.project.inject_module_types(&self.dep_cache.type_infos);
            self.project.set_skip_dependencies(true);
        }

        if !self.dep_cache.own_module_hints.is_empty() {
            self.project.set_own_module_hints(
                self.dep_cache.own_module_hints.clone(),
                self.dep_cache.own_checked_modules.clone(),
            );
        }

        self.project.clear_checked_modules();

        let result = self.project.check(
            true,
            None,
            false,
            false,
            u32::default(),
            PropertyTest::DEFAULT_MAX_SUCCESS,
            CoverageMode::default(),
            Tracing::verbose(),
            false,
            None,
        );

        // Extract own-module hints from whatever was successfully type-checked.
        // `checked_modules` only contains modules that passed inference, so this
        // is safe to do regardless of whether the overall compilation succeeded.
        // Updating on failure means the next compile won't re-infer modules that
        // haven't changed, instead of falling back to stale pre-failure hints.
        let own_module_hints = self.project.extract_own_module_hints(&self.own_package);
        let own_checked_modules = self.project.extract_own_checked_modules(&self.own_package);

        if result.is_ok() && !has_cache {
            let type_infos = self.project.extract_dep_module_types(&self.own_package);
            if !type_infos.is_empty() {
                let checked_modules: HashMap<String, CheckedModule> = self
                    .project
                    .modules()
                    .into_iter()
                    .filter(|m| m.package != self.own_package)
                    .map(|m| (m.name.clone(), m))
                    .collect();
                self.dep_cache = LspDepCache {
                    type_infos,
                    checked_modules,
                    deps_hash: current_deps_hash,
                    own_module_hints,
                    own_checked_modules,
                };
            } else {
                self.dep_cache.own_module_hints = own_module_hints;
                self.dep_cache.own_checked_modules = own_checked_modules;
            }
        } else {
            self.dep_cache.own_module_hints = own_module_hints;
            self.dep_cache.own_checked_modules = own_checked_modules;
        }

        self.project.set_skip_dependencies(false);
        self.project.restore(checkpoint);

        let mut modules = self.project.modules();

        if has_cache {
            for (name, module) in &self.dep_cache.checked_modules {
                if !modules.iter().any(|m| &m.name == name) {
                    modules.push(module.clone());
                }
            }
        }

        for mut module in modules.into_iter() {
            let path = module
                .input_path
                .canonicalize()
                .expect("Canonicalize")
                .as_os_str()
                .to_string_lossy()
                .to_string();

            let line_numbers = LineNumbers::new(&module.code);

            let source = SourceInfo { path, line_numbers };

            module.attach_doc_and_module_comments();

            self.sources.insert(module.name.to_string(), source);
            self.modules.insert(module.name.to_string(), module);
        }

        result?;

        Ok(())
    }
}
