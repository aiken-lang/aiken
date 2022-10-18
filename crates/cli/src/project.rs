use std::{
    collections::HashMap,
    fs,
    path::{Path, PathBuf},
};

use aiken_lang::{ast::ModuleKind, builtins, tipo, IdGenerator};

use crate::{
    config::Config,
    error::Error,
    module::{CheckedModule, ParsedModule, ParsedModules},
};

#[derive(Debug)]
pub struct Source {
    pub path: PathBuf,
    pub name: String,
    pub code: String,
    pub kind: ModuleKind,
}

#[derive(Debug, PartialEq)]
pub enum Warning {
    Type {
        path: PathBuf,
        src: String,
        warning: tipo::error::Warning,
    },
}

impl Warning {
    pub fn from_type_warning(warning: tipo::error::Warning, path: PathBuf, src: String) -> Warning {
        Warning::Type { path, warning, src }
    }
}

pub struct Project {
    config: Config,
    defined_modules: HashMap<String, PathBuf>,
    id_gen: IdGenerator,
    module_types: HashMap<String, tipo::Module>,
    root: PathBuf,
    sources: Vec<Source>,
    pub warnings: Vec<Warning>,
}

impl Project {
    pub fn new(config: Config, root: PathBuf) -> Project {
        let id_gen = IdGenerator::new();

        let mut module_types = HashMap::new();

        module_types.insert("aiken".to_string(), builtins::prelude(&id_gen));

        Project {
            config,
            defined_modules: HashMap::new(),
            id_gen,
            module_types,
            root,
            sources: vec![],
            warnings: vec![],
        }
    }

    pub fn build(&mut self) -> Result<(), Error> {
        self.read_source_files()?;

        let parsed_modules = self.parse_sources()?;

        let processing_sequence = parsed_modules.sequence()?;

        let checked_modules = self.type_check(parsed_modules, processing_sequence)?;

        println!("{:?}", checked_modules);

        Ok(())
    }

    fn read_source_files(&mut self) -> Result<(), Error> {
        let lib = self.root.join("lib");
        let scripts = self.root.join("scripts");

        self.aiken_files(&scripts, ModuleKind::Script)?;
        self.aiken_files(&lib, ModuleKind::Lib)?;

        Ok(())
    }

    fn parse_sources(&mut self) -> Result<ParsedModules, Error> {
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
                        package: self.config.name.clone(),
                    };

                    if let Some(first) = self
                        .defined_modules
                        .insert(module.name.clone(), module.path.clone())
                    {
                        return Err(Error::DuplicateModule {
                            module: module.name.clone(),
                            first,
                            second: module.path,
                        });
                    }

                    parsed_modules.insert(module.name.clone(), module);
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
            Ok(parsed_modules.into())
        } else {
            Err(Error::List(errors))
        }
    }

    fn type_check(
        &mut self,
        mut parsed_modules: ParsedModules,
        processing_sequence: Vec<String>,
    ) -> Result<Vec<CheckedModule>, Error> {
        let mut modules = Vec::with_capacity(parsed_modules.len() + 1);

        for name in processing_sequence {
            if let Some(ParsedModule {
                name,
                path,
                code,
                kind,
                package,
                ast,
            }) = parsed_modules.remove(&name)
            {
                let mut type_warnings = Vec::new();

                let ast = tipo::infer::module(
                    &self.id_gen,
                    ast,
                    kind,
                    &self.config.name,
                    &self.module_types,
                    &mut type_warnings,
                )
                .map_err(|error| Error::Type {
                    path: path.clone(),
                    src: code.clone(),
                    error,
                })?;

                // Register any warnings emitted as type warnings
                let type_warnings = type_warnings
                    .into_iter()
                    .map(|w| Warning::from_type_warning(w, path.clone(), code.clone()));

                self.warnings.extend(type_warnings);

                // Register the types from this module so they can be imported into
                // other modules.
                self.module_types
                    .insert(name.clone(), ast.type_info.clone());

                modules.push(CheckedModule {
                    kind,
                    // extra,
                    name,
                    code,
                    ast,
                    input_path: path,
                });
            }
        }

        Ok(modules)
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
