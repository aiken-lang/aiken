use aiken_lang::{ast::Tracing, line_numbers::LineNumbers, test_framework::PropertyTest};
use aiken_project::{config::Config, error::Error as ProjectError, module::CheckedModule, Project};
use std::{collections::HashMap, path::PathBuf};

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

        let result = self.project.check(
            true,
            None,
            false,
            false,
            u32::default(),
            PropertyTest::DEFAULT_MAX_SUCCESS,
            Tracing::verbose(),
            None,
        );

        self.project.restore(checkpoint);

        let modules = self.project.modules();

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
