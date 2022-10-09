use std::{
    collections::HashMap,
    fs,
    path::{Path, PathBuf},
};

use aiken_lang::ast::{ModuleKind, UntypedModule};

use crate::{config::Config, error::Error};

#[derive(Debug)]
pub struct Source {
    pub path: PathBuf,
    pub name: String,
    pub code: String,
    pub kind: ModuleKind,
}

#[derive(Debug)]
struct ParsedModule {
    path: PathBuf,
    name: String,
    code: String,
    kind: ModuleKind,
    package: String,
    ast: UntypedModule,
    // extra: ModuleExtra,
}

pub struct Project {
    config: Config,
    root: PathBuf,
    sources: Vec<Source>,
}

impl Project {
    pub fn new(config: Config, root: PathBuf) -> Project {
        Project {
            config,
            root,
            sources: vec![],
        }
    }

    pub fn build(&mut self) -> Result<(), Error> {
        self.read_source_files()?;

        self.parse_sources()?;

        Ok(())
    }

    fn parse_sources(&mut self) -> Result<HashMap<String, ParsedModule>, Error> {
        let mut errors = Vec::new();
        let mut parsed_modules = HashMap::with_capacity(self.sources.len());

        for Source {
            path,
            name,
            code,
            kind,
        } in self.sources.drain(0..)
        {
            match aiken_lang::parser::script(&code) {
                Ok(mut ast) => {
                    // Store the name
                    ast.name = name.clone();

                    let module = ParsedModule {
                        kind,
                        ast,
                        code,
                        name,
                        path,
                        package: "".to_string(),
                    };

                    let _ = parsed_modules.insert(module.name.clone(), module);
                }
                Err(errs) => {
                    for error in errs {
                        errors.push(Error::Parse {
                            path: path.clone(),
                            src: code.clone(),
                            error: Box::new(error),
                        })
                    }
                }
            }
        }

        if errors.is_empty() {
            Ok(parsed_modules)
        } else {
            Err(Error::List(errors))
        }
    }

    fn read_source_files(&mut self) -> Result<(), Error> {
        let lib = self.root.join("lib");
        let scripts = self.root.join("scripts");

        self.aiken_files(&scripts, ModuleKind::Script)?;
        self.aiken_files(&lib, ModuleKind::Lib)?;

        Ok(())
    }

    fn aiken_files(&mut self, dir: &Path, kind: ModuleKind) -> Result<(), Error> {
        let paths = walkdir::WalkDir::new(dir)
            .follow_links(true)
            .into_iter()
            .filter_map(Result::ok)
            .filter(|e| e.file_type().is_file())
            .map(|d| d.into_path())
            .filter(move |d| is_aiken_path(d, dir));

        for path in paths {
            self.add_module(path, dir, kind)?;
        }

        Ok(())
    }

    fn add_module(&mut self, path: PathBuf, dir: &Path, kind: ModuleKind) -> Result<(), Error> {
        let name = self.module_name(dir, &path, kind);
        let code = fs::read_to_string(&path).map_err(|error| Error::FileIo {
            path: path.clone(),
            error,
        })?;

        self.sources.push(Source {
            name,
            code,
            kind,
            path,
        });

        Ok(())
    }

    fn module_name(
        &self,
        package_path: &Path,
        full_module_path: &Path,
        kind: ModuleKind,
    ) -> String {
        // ../../lib/module.ak

        // module.ak
        let mut module_path = full_module_path
            .strip_prefix(package_path)
            .expect("Stripping package prefix from module path")
            .to_path_buf();

        // module
        let _ = module_path.set_extension("");

        // Stringify
        let name = module_path
            .to_str()
            .expect("Module name path to str")
            .to_string();

        // normalise windows paths
        let name = name.replace('\\', "/");

        // project_name/module
        if kind.is_lib() {
            format!("{}/{}", self.config.name, name)
        } else {
            name
        }
    }
}

fn is_aiken_path(path: &Path, dir: impl AsRef<Path>) -> bool {
    use regex::Regex;

    let re = Regex::new(&format!(
        "^({module}{slash})*{module}\\.ak$",
        module = "[a-z][_a-z0-9]*",
        slash = "(/|\\\\)",
    ))
    .expect("is_aiken_path() RE regex");

    re.is_match(
        path.strip_prefix(dir)
            .expect("is_aiken_path(): strip_prefix")
            .to_str()
            .expect("is_aiken_path(): to_str"),
    )
}
