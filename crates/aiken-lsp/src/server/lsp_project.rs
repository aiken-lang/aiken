use aiken_lang::{ast::Tracing, line_numbers::LineNumbers, test_framework::PropertyTest};
use aiken_project::{Project, error::Error as ProjectError, module::CheckedModule};
use std::{collections::HashMap, path::PathBuf};

#[derive(Debug)]
pub struct SourceInfo {
    /// The path to the source file from within the project root
    pub path: String,
    /// Useful for converting from Aiken's byte index offsets to the LSP line
    /// and column number positions.
    pub line_numbers: LineNumbers,
}

pub struct ProjectData {
    pub project: Project<super::telemetry::Lsp>,
    pub modules: HashMap<String, CheckedModule>,
    pub sources: HashMap<String, SourceInfo>,
}

pub struct LspProject {
    pub projects: Vec<ProjectData>,
}

impl LspProject {
    pub fn new(roots: Vec<PathBuf>, telemetry: super::telemetry::Lsp) -> Self {
        Self {
            projects: roots
                .into_iter()
                .map(|root| Project::new(root, telemetry))
                .filter_map(|res| res.ok())
                .map(|project| ProjectData {
                    project,
                    modules: HashMap::new(),
                    sources: HashMap::new(),
                })
                .collect(),
        }
    }

    pub fn compile(&mut self) -> Result<(), Vec<ProjectError>> {
        let mut errors = Vec::new();

        for data in &mut self.projects {
            let checkpoint = data.project.checkpoint();

            let result = data.project.check(
                true,
                None,
                false,
                false,
                u32::default(),
                PropertyTest::DEFAULT_MAX_SUCCESS,
                Tracing::verbose(),
                None,
            );

            data.project.restore(checkpoint);

            let modules = data.project.modules();

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

                data.sources.insert(module.name.to_string(), source);
                data.modules.insert(module.name.to_string(), module);
            }

            if let Err(mut err) = result {
                errors.append(&mut err);
            }
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        Ok(())
    }
}
