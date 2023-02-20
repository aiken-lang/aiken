use std::{collections::HashMap, path::PathBuf};

use aiken_lang::ast::Tracing;
use aiken_project::{config::Config, error::Error as ProjectError, module::CheckedModule, Project};

use crate::line_numbers::LineNumbers;

#[derive(Debug)]
pub struct SourceInfo {
    /// The path to the source file from within the project root
    pub path: String,
    /// Useful for converting from Aiken's byte index offsets to the LSP line
    /// and column number positions.
    pub line_numbers: LineNumbers,
}

pub struct LspProject {
    pub project: Project<super::telemetry::Lsp>,
    pub modules: HashMap<String, CheckedModule>,
    pub sources: HashMap<String, SourceInfo>,
}

impl LspProject {
    pub fn new(config: Config, root: PathBuf, telemetry: super::telemetry::Lsp) -> Self {
        Self {
            project: Project::new_with_config(config, root, telemetry),
            modules: HashMap::new(),
            sources: HashMap::new(),
        }
    }

    pub fn compile(&mut self) -> Result<(), Vec<ProjectError>> {
        let checkpoint = self.project.checkpoint();

        let result = self
            .project
            .check(true, None, false, false, Tracing::NoTraces);

        self.project.restore(checkpoint);

        result?;

        let modules = self.project.modules();

        for module in modules.into_iter() {
            let path = module
                .input_path
                .canonicalize()
                .expect("Canonicalize")
                .as_os_str()
                .to_string_lossy()
                .to_string();

            let line_numbers = LineNumbers::new(&module.code);

            let source = SourceInfo { path, line_numbers };

            self.sources.insert(module.name.to_string(), source);
            self.modules.insert(module.name.to_string(), module);
        }

        Ok(())
    }
}
