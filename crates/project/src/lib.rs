use std::{
    collections::HashMap,
    fs,
    path::{Path, PathBuf},
};

pub mod config;
pub mod error;
pub mod format;
pub mod module;

use aiken_lang::{
    ast::{Definition, Function, ModuleKind},
    builtins,
    tipo::TypeInfo,
    uplc::CodeGenerator,
    IdGenerator,
};
use uplc::ast::{NamedDeBruijn, Program};

use crate::{
    config::Config,
    error::{Error, Warning},
    module::{CheckedModule, CheckedModules, ParsedModule, ParsedModules},
};

#[derive(Debug)]
pub struct Source {
    pub path: PathBuf,
    pub name: String,
    pub code: String,
    pub kind: ModuleKind,
}

pub const SPEND: &str = "spend";
pub const CERT: &str = "cert";
pub const MINT: &str = "mint";
pub const WITHDRAWL: &str = "withdrawl";
pub const VALIDATOR_NAMES: [&str; 4] = [SPEND, CERT, MINT, WITHDRAWL];

pub struct Project {
    config: Config,
    defined_modules: HashMap<String, PathBuf>,
    id_gen: IdGenerator,
    module_types: HashMap<String, TypeInfo>,
    root: PathBuf,
    sources: Vec<Source>,
    pub warnings: Vec<Warning>,
}

impl Project {
    pub fn new(config: Config, root: PathBuf) -> Project {
        let id_gen = IdGenerator::new();

        let mut module_types = HashMap::new();

        module_types.insert("aiken".to_string(), builtins::prelude(&id_gen));
        module_types.insert("aiken/builtin".to_string(), builtins::plutus(&id_gen));

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
        self.compile(true)
    }

    pub fn check(&mut self) -> Result<(), Error> {
        self.compile(false)
    }

    pub fn compile(&mut self, uplc_gen: bool) -> Result<(), Error> {
        self.read_source_files()?;

        let parsed_modules = self.parse_sources()?;

        let processing_sequence = parsed_modules.sequence()?;

        let mut checked_modules = self.type_check(parsed_modules, processing_sequence)?;

        let scripts = self.validate_scripts(&mut checked_modules)?;

        if uplc_gen {
            self.code_gen(scripts, &checked_modules)?;
        }

        Ok(())
    }

    fn read_source_files(&mut self) -> Result<(), Error> {
        let lib = self.root.join(format!("src/{}", self.config.name));
        let scripts = self.root.join("src/scripts");

        self.read_root_lib_file()?;
        self.aiken_files(&scripts, ModuleKind::Script)?;
        self.aiken_files(&lib, ModuleKind::Lib)?;

        Ok(())
    }

    fn read_root_lib_file(&mut self) -> Result<(), Error> {
        let root_lib_path = self.root.join(format!("src/{}.ak", self.config.name));

        if root_lib_path.exists() {
            self.add_module(root_lib_path, &self.root.join("src"), ModuleKind::Lib)?;
        }

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
            match aiken_lang::parser::module(&code, kind) {
                Ok((mut ast, _)) => {
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
    ) -> Result<CheckedModules, Error> {
        let mut modules = HashMap::with_capacity(parsed_modules.len() + 1);

        for name in processing_sequence {
            if let Some(ParsedModule {
                name,
                path,
                code,
                kind,
                // TODO: come back and figure out where to use this
                package: _package,
                ast,
            }) = parsed_modules.remove(&name)
            {
                let mut type_warnings = Vec::new();

                let ast = ast
                    .infer(
                        &self.id_gen,
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

                modules.insert(
                    name.clone(),
                    CheckedModule {
                        kind,
                        // extra,
                        name,
                        code,
                        ast,
                        input_path: path,
                    },
                );
            }
        }

        Ok(modules.into())
    }

    fn validate_scripts(
        &self,
        checked_modules: &mut CheckedModules,
    ) -> Result<Vec<CheckedModule>, Error> {
        let mut errors = Vec::new();
        let mut scripts = Vec::new();

        for module in checked_modules.scripts() {
            scripts.push(module.clone());

            for def in module.ast.definitions() {
                if let Definition::Fn(Function {
                    arguments,
                    location,
                    name,
                    return_type,
                    ..
                }) = def
                {
                    if VALIDATOR_NAMES.contains(&name.as_str()) {
                        // validators must return a Bool
                        if !return_type.is_bool() {
                            errors.push(Error::ValidatorMustReturnBool {
                                location: *location,
                                src: module.code.clone(),
                                path: module.input_path.clone(),
                            })
                        }

                        // depending on name, validate the minimum number of arguments
                        // if too low, push a new error on to errors
                        if [MINT, CERT, WITHDRAWL].contains(&name.as_str()) && arguments.len() < 2 {
                            errors.push(Error::WrongValidatorArity {
                                location: *location,
                                src: module.code.clone(),
                                path: module.input_path.clone(),
                                name: name.clone(),
                                at_least: 2,
                            })
                        }

                        if SPEND == name && arguments.len() < 3 {
                            errors.push(Error::WrongValidatorArity {
                                location: *location,
                                src: module.code.clone(),
                                path: module.input_path.clone(),
                                name: name.clone(),
                                at_least: 3,
                            })
                        }
                    }
                }
            }
        }

        if errors.is_empty() {
            for script in &scripts {
                checked_modules.remove(&script.name);
            }

            Ok(scripts)
        } else {
            Err(Error::List(errors))
        }
    }

    fn code_gen(
        &mut self,
        scripts: Vec<CheckedModule>,
        checked_modules: &CheckedModules,
    ) -> Result<Vec<Program<NamedDeBruijn>>, Error> {
        let mut programs = Vec::new();
        let mut functions = HashMap::new();
        let mut type_aliases = HashMap::new();
        let mut data_types = HashMap::new();
        let mut imports = HashMap::new();
        let mut constants = HashMap::new();

        for module in checked_modules.values() {
            for def in module.ast.definitions() {
                match def {
                    Definition::Fn(func) => {
                        functions.insert((module.name.clone(), func.name.clone()), func);
                    }
                    Definition::TypeAlias(ta) => {
                        type_aliases.insert((module.name.clone(), ta.alias.clone()), ta);
                    }
                    Definition::DataType(dt) => {
                        data_types.insert((module.name.clone(), dt.name.clone()), dt);
                    }
                    Definition::Use(import) => {
                        imports.insert((module.name.clone(), import.module.join("/")), import);
                    }
                    Definition::ModuleConstant(mc) => {
                        constants.insert((module.name.clone(), mc.name.clone()), mc);
                    }
                }
            }
        }

        for script in scripts {
            for def in script.ast.into_definitions() {
                if let Definition::Fn(Function {
                    arguments,
                    name,
                    body,
                    ..
                }) = def
                {
                    if VALIDATOR_NAMES.contains(&name.as_str()) {
                        let mut generator = CodeGenerator::new(
                            &functions,
                            // &type_aliases,
                            &data_types,
                            // &imports,
                            // &constants,
                        );

                        let program = generator.generate(body, arguments);

                        programs.push(program.try_into().unwrap());
                    }
                }
            }
        }

        Ok(programs)
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
        // ../../{config.name}/module.ak

        // module.ak
        let mut module_path = full_module_path
            .strip_prefix(package_path)
            .expect("Stripping package prefix from module path")
            .to_path_buf();

        // module
        module_path.set_extension("");

        // Stringify
        let name = module_path
            .to_str()
            .expect("Module name path to str")
            .to_string();

        // normalise windows paths
        let name = name.replace('\\', "/");

        // project_name/module
        if kind.is_lib() && name != self.config.name {
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
