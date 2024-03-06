pub mod blueprint;
pub mod config;
pub mod deps;
pub mod docs;
pub mod error;
pub mod format;
pub mod github;
pub mod module;
pub mod options;
pub mod package_name;
pub mod paths;
pub mod pretty;
pub mod telemetry;
pub mod test_framework;
pub mod utils;
pub mod watch;

#[cfg(test)]
mod tests;

use crate::{
    blueprint::{
        definitions::Definitions,
        schema::{Annotated, Schema},
        Blueprint,
    },
    config::Config,
    error::{Error, Warning},
    module::{CheckedModule, CheckedModules, ParsedModule, ParsedModules},
    telemetry::Event,
};
use aiken_lang::{
    ast::{
        DataTypeKey, Definition, FunctionAccessKey, ModuleKind, Tracing, TypedDataType,
        TypedFunction,
    },
    builtins,
    expr::UntypedExpr,
    gen_uplc::CodeGenerator,
    line_numbers::LineNumbers,
    tipo::TypeInfo,
    IdGenerator,
};
use indexmap::IndexMap;
use miette::NamedSource;
use options::{CodeGenMode, Options};
use package_name::PackageName;
use pallas::ledger::{
    addresses::{Address, Network, ShelleyAddress, ShelleyDelegationPart, StakePayload},
    primitives::babbage::{self as cardano, PolicyId},
    traverse::ComputeHash,
};
use std::{
    collections::HashMap,
    fs::{self, File},
    io::BufReader,
    path::{Path, PathBuf},
};
use telemetry::EventListener;
use test_framework::{Test, TestResult};
use uplc::{
    ast::{Name, Program},
    PlutusData,
};

#[derive(Debug)]
pub struct Source {
    pub path: PathBuf,
    pub name: String,
    pub code: String,
    pub kind: ModuleKind,
}

pub struct Checkpoint {
    module_types: HashMap<String, TypeInfo>,
    defined_modules: HashMap<String, PathBuf>,
}

pub struct Project<T>
where
    T: EventListener,
{
    config: Config,
    defined_modules: HashMap<String, PathBuf>,
    checked_modules: CheckedModules,
    id_gen: IdGenerator,
    module_types: HashMap<String, TypeInfo>,
    root: PathBuf,
    sources: Vec<Source>,
    warnings: Vec<Warning>,
    event_listener: T,
    functions: IndexMap<FunctionAccessKey, TypedFunction>,
    data_types: IndexMap<DataTypeKey, TypedDataType>,
    module_sources: HashMap<String, (String, LineNumbers)>,
}

impl<T> Project<T>
where
    T: EventListener,
{
    pub fn new(root: PathBuf, event_listener: T) -> Result<Project<T>, Error> {
        let config = Config::load(&root)?;

        let project = Project::new_with_config(config, root, event_listener);

        Ok(project)
    }

    pub fn new_with_config(config: Config, root: PathBuf, event_listener: T) -> Project<T> {
        let id_gen = IdGenerator::new();

        let mut module_types = HashMap::new();

        module_types.insert("aiken".to_string(), builtins::prelude(&id_gen));
        module_types.insert("aiken/builtin".to_string(), builtins::plutus(&id_gen));

        let functions = builtins::prelude_functions(&id_gen);

        let data_types = builtins::prelude_data_types(&id_gen);

        Project {
            config,
            checked_modules: CheckedModules::default(),
            defined_modules: HashMap::new(),
            id_gen,
            module_types,
            root,
            sources: vec![],
            warnings: vec![],
            event_listener,
            functions,
            data_types,
            module_sources: HashMap::new(),
        }
    }

    pub fn new_generator(&'_ self, tracing: Tracing) -> CodeGenerator<'_> {
        CodeGenerator::new(
            utils::indexmap::as_ref_values(&self.functions),
            utils::indexmap::as_ref_values(&self.data_types),
            utils::indexmap::as_str_ref_values(&self.module_types),
            utils::indexmap::as_str_ref_values(&self.module_sources),
            tracing,
        )
    }

    pub fn warnings(&mut self) -> Vec<Warning> {
        std::mem::take(&mut self.warnings)
    }

    pub fn modules(&self) -> Vec<CheckedModule> {
        self.checked_modules.values().cloned().collect()
    }

    pub fn importable_modules(&self) -> Vec<String> {
        self.module_types.keys().cloned().collect()
    }

    pub fn checkpoint(&self) -> Checkpoint {
        Checkpoint {
            module_types: self.module_types.clone(),
            defined_modules: self.defined_modules.clone(),
        }
    }

    pub fn restore(&mut self, checkpoint: Checkpoint) {
        self.module_types = checkpoint.module_types;
        self.defined_modules = checkpoint.defined_modules;
    }

    pub fn build(&mut self, uplc: bool, tracing: Tracing) -> Result<(), Vec<Error>> {
        let options = Options {
            code_gen_mode: CodeGenMode::Build(uplc),
            tracing,
        };

        self.compile(options)
    }

    pub fn docs(&mut self, destination: Option<PathBuf>) -> Result<(), Vec<Error>> {
        self.compile_deps()?;

        self.event_listener
            .handle_event(Event::BuildingDocumentation {
                root: self.root.clone(),
                name: self.config.name.to_string(),
                version: self.config.version.clone(),
            });

        self.read_source_files()?;

        let destination = destination.unwrap_or_else(|| self.root.join("docs"));

        let parsed_modules = self.parse_sources(self.config.name.clone())?;

        self.type_check(parsed_modules, Tracing::silent(), false, false)?;

        self.event_listener.handle_event(Event::GeneratingDocFiles {
            output_path: destination.clone(),
        });

        let modules = self
            .checked_modules
            .values_mut()
            .filter(|CheckedModule { package, .. }| package == &self.config.name.to_string())
            .map(|m| {
                m.attach_doc_and_module_comments();
                &*m
            })
            .collect();

        let doc_files = docs::generate_all(&self.root, &self.config, modules);

        for file in doc_files {
            let path = destination.join(file.path);
            fs::create_dir_all(path.parent().unwrap()).map_err(Error::from)?;
            fs::write(&path, file.content).map_err(Error::from)?;
        }

        Ok(())
    }

    pub fn check(
        &mut self,
        skip_tests: bool,
        match_tests: Option<Vec<String>>,
        verbose: bool,
        exact_match: bool,
        seed: u32,
        tracing: Tracing,
    ) -> Result<(), Vec<Error>> {
        let options = Options {
            tracing,
            code_gen_mode: if skip_tests {
                CodeGenMode::NoOp
            } else {
                CodeGenMode::Test {
                    match_tests,
                    verbose,
                    exact_match,
                    seed,
                }
            },
        };

        self.compile(options)
    }

    pub fn dump_uplc(&self, blueprint: &Blueprint) -> Result<(), Error> {
        let dir = self.root.join("artifacts");

        self.event_listener
            .handle_event(Event::DumpingUPLC { path: dir.clone() });

        fs::create_dir_all(&dir)?;

        for validator in &blueprint.validators {
            let path = dir.clone().join(format!("{}.uplc", validator.title));

            let program = &validator.program;
            let program: Program<Name> = program.try_into().unwrap();

            fs::write(&path, program.to_pretty()).map_err(|error| Error::FileIo { error, path })?;
        }

        Ok(())
    }

    pub fn blueprint_path(&self) -> PathBuf {
        self.root.join("plutus.json")
    }

    pub fn compile(&mut self, options: Options) -> Result<(), Vec<Error>> {
        self.compile_deps()?;

        self.event_listener
            .handle_event(Event::StartingCompilation {
                root: self.root.clone(),
                name: self.config.name.to_string(),
                version: self.config.version.clone(),
            });

        self.read_source_files()?;

        let parsed_modules = self.parse_sources(self.config.name.clone())?;

        self.type_check(parsed_modules, options.tracing, true, false)?;

        match options.code_gen_mode {
            CodeGenMode::Build(uplc_dump) => {
                self.event_listener
                    .handle_event(Event::GeneratingBlueprint {
                        path: self.blueprint_path(),
                    });

                self.checked_modules.values_mut().for_each(|m| {
                    m.attach_doc_and_module_comments();
                });

                let mut generator = self.new_generator(options.tracing);

                let blueprint = Blueprint::new(&self.config, &self.checked_modules, &mut generator)
                    .map_err(Error::Blueprint)?;

                if blueprint.validators.is_empty() {
                    self.warnings.push(Warning::NoValidators);
                }

                if uplc_dump {
                    self.dump_uplc(&blueprint)?;
                }

                let json = serde_json::to_string_pretty(&blueprint).unwrap();

                fs::write(self.blueprint_path(), json).map_err(|error| {
                    Error::FileIo {
                        error,
                        path: self.blueprint_path(),
                    }
                    .into()
                })
            }
            CodeGenMode::Test {
                match_tests,
                verbose,
                exact_match,
                seed,
            } => {
                let tests =
                    self.collect_tests(verbose, match_tests, exact_match, options.tracing)?;

                if !tests.is_empty() {
                    self.event_listener.handle_event(Event::RunningTests);
                }

                let tests = self.run_tests(tests, seed);

                let errors: Vec<Error> = tests
                    .iter()
                    .filter_map(|e| {
                        if e.is_success() {
                            None
                        } else {
                            Some(e.into_error(verbose))
                        }
                    })
                    .collect();

                self.event_listener
                    .handle_event(Event::FinishedTests { seed, tests });

                if !errors.is_empty() {
                    Err(errors)
                } else {
                    Ok(())
                }
            }
            CodeGenMode::NoOp => Ok(()),
        }
    }

    pub fn address(
        &self,
        title: Option<&String>,
        stake_address: Option<&String>,
        mainnet: bool,
    ) -> Result<ShelleyAddress, Error> {
        // Parse stake address
        let stake_address = stake_address
            .map(|s| {
                Address::from_hex(s)
                    .or_else(|_| Address::from_bech32(s))
                    .map_err(|error| Error::MalformedStakeAddress { error: Some(error) })
                    .and_then(|addr| match addr {
                        Address::Stake(addr) => Ok(addr),
                        _ => Err(Error::MalformedStakeAddress { error: None }),
                    })
            })
            .transpose()?;
        let delegation_part = match stake_address.map(|addr| addr.payload().to_owned()) {
            None => ShelleyDelegationPart::Null,
            Some(StakePayload::Stake(key)) => ShelleyDelegationPart::Key(key),
            Some(StakePayload::Script(script)) => ShelleyDelegationPart::Script(script),
        };

        // Read blueprint
        let blueprint = File::open(self.blueprint_path())
            .map_err(|_| blueprint::error::Error::InvalidOrMissingFile)?;
        let blueprint: Blueprint = serde_json::from_reader(BufReader::new(blueprint))?;

        // Calculate the address
        let when_too_many =
            |known_validators| Error::MoreThanOneValidatorFound { known_validators };
        let when_missing = |known_validators| Error::NoValidatorNotFound { known_validators };

        blueprint.with_validator(title, when_too_many, when_missing, |validator| {
            // Make sure we're not calculating the address for a minting validator
            if validator.datum.is_none() {
                return Err(blueprint::error::Error::UnexpectedMintingValidator.into());
            }

            let n = validator.parameters.len();
            if n > 0 {
                Err(blueprint::error::Error::ParameterizedValidator { n }.into())
            } else {
                let network = if mainnet {
                    Network::Mainnet
                } else {
                    Network::Testnet
                };

                Ok(validator
                    .program
                    .address(network, delegation_part.to_owned()))
            }
        })
    }

    pub fn policy(&self, title: Option<&String>) -> Result<PolicyId, Error> {
        // Read blueprint
        let blueprint = File::open(self.blueprint_path())
            .map_err(|_| blueprint::error::Error::InvalidOrMissingFile)?;
        let blueprint: Blueprint = serde_json::from_reader(BufReader::new(blueprint))?;

        // Error handlers for ambiguous / missing validators
        let when_too_many =
            |known_validators| Error::MoreThanOneValidatorFound { known_validators };
        let when_missing = |known_validators| Error::NoValidatorNotFound { known_validators };

        blueprint.with_validator(title, when_too_many, when_missing, |validator| {
            // Make sure we're not calculating the policy for a spending validator
            if validator.datum.is_some() {
                return Err(blueprint::error::Error::UnexpectedSpendingValidator.into());
            }

            let n = validator.parameters.len();
            if n > 0 {
                Err(blueprint::error::Error::ParameterizedValidator { n }.into())
            } else {
                let cbor = validator.program.to_cbor().unwrap();
                let validator_hash = cardano::PlutusV2Script(cbor.into()).compute_hash();
                Ok(validator_hash)
            }
        })
    }

    pub fn construct_parameter_incrementally<F>(
        &self,
        title: Option<&String>,
        ask: F,
    ) -> Result<PlutusData, Error>
    where
        F: Fn(
            &Annotated<Schema>,
            &Definitions<Annotated<Schema>>,
        ) -> Result<PlutusData, blueprint::error::Error>,
    {
        // Read blueprint
        let blueprint = File::open(self.blueprint_path())
            .map_err(|_| blueprint::error::Error::InvalidOrMissingFile)?;
        let blueprint: Blueprint = serde_json::from_reader(BufReader::new(blueprint))?;

        // Construct parameter
        let when_too_many =
            |known_validators| Error::MoreThanOneValidatorFound { known_validators };
        let when_missing = |known_validators| Error::NoValidatorNotFound { known_validators };

        let data = blueprint.with_validator(title, when_too_many, when_missing, |validator| {
            validator
                .ask_next_parameter(&blueprint.definitions, &ask)
                .map_err(|e| e.into())
        })?;

        Ok(data)
    }

    pub fn apply_parameter(
        &self,
        title: Option<&String>,
        param: &PlutusData,
    ) -> Result<Blueprint, Error> {
        // Read blueprint
        let blueprint = File::open(self.blueprint_path())
            .map_err(|_| blueprint::error::Error::InvalidOrMissingFile)?;
        let mut blueprint: Blueprint = serde_json::from_reader(BufReader::new(blueprint))?;

        // Apply parameters
        let when_too_many =
            |known_validators| Error::MoreThanOneValidatorFound { known_validators };
        let when_missing = |known_validators| Error::NoValidatorNotFound { known_validators };

        let applied_validator =
            blueprint.with_validator(title, when_too_many, when_missing, |validator| {
                validator
                    .apply(&blueprint.definitions, param)
                    .map_err(|e| e.into())
            })?;

        // Overwrite validator
        blueprint.validators = blueprint
            .validators
            .into_iter()
            .map(|validator| {
                let same_title = validator.title == applied_validator.title;
                if same_title {
                    applied_validator.to_owned()
                } else {
                    validator
                }
            })
            .collect();

        Ok(blueprint)
    }

    fn compile_deps(&mut self) -> Result<(), Vec<Error>> {
        let manifest = deps::download(&self.event_listener, &self.root, &self.config)?;

        for package in manifest.packages {
            let lib = self.root.join(paths::build_deps_package(&package.name));

            self.event_listener
                .handle_event(Event::StartingCompilation {
                    root: lib.clone(),
                    name: package.name.to_string(),
                    version: package.version.clone(),
                });

            self.read_package_source_files(&lib.join("lib"))?;

            let mut parsed_modules = self.parse_sources(package.name)?;

            use rayon::prelude::*;

            parsed_modules
                .par_iter_mut()
                .for_each(|(_module, parsed_module)| {
                    parsed_module
                        .ast
                        .definitions
                        .retain(|def| !matches!(def, Definition::Test { .. }))
                });

            self.type_check(parsed_modules, Tracing::silent(), true, true)?;
        }

        Ok(())
    }

    fn read_source_files(&mut self) -> Result<(), Error> {
        let lib = self.root.join("lib");
        let validators = self.root.join("validators");

        self.aiken_files(&validators, ModuleKind::Validator)?;
        self.aiken_files(&lib, ModuleKind::Lib)?;

        Ok(())
    }

    fn read_package_source_files(&mut self, lib: &Path) -> Result<(), Error> {
        self.aiken_files(lib, ModuleKind::Lib)?;

        Ok(())
    }

    fn parse_sources(&mut self, package_name: PackageName) -> Result<ParsedModules, Vec<Error>> {
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
                Ok((mut ast, extra)) => {
                    // Store the name
                    ast.name = name.clone();

                    let module = ParsedModule {
                        kind,
                        ast,
                        code,
                        name,
                        path,
                        extra,
                        package: package_name.to_string(),
                    };

                    if let Some(first) = self
                        .defined_modules
                        .insert(module.name.clone(), module.path.clone())
                    {
                        return Err(Error::DuplicateModule {
                            module: module.name.clone(),
                            first,
                            second: module.path,
                        }
                        .into());
                    }

                    parsed_modules.insert(module.name.clone(), module);
                }
                Err(errs) => {
                    for error in errs {
                        errors.push(Error::Parse {
                            path: path.clone(),
                            src: code.clone(),
                            named: NamedSource::new(path.display().to_string(), code.clone()),
                            error: Box::new(error),
                        })
                    }
                }
            }
        }

        if errors.is_empty() {
            Ok(parsed_modules.into())
        } else {
            Err(errors)
        }
    }

    fn type_check(
        &mut self,
        mut parsed_modules: ParsedModules,
        tracing: Tracing,
        validate_module_name: bool,
        is_dependency: bool,
    ) -> Result<(), Error> {
        let processing_sequence = parsed_modules.sequence()?;

        for name in processing_sequence {
            if let Some(ParsedModule {
                name,
                path,
                code,
                kind,
                extra,
                package,
                ast,
            }) = parsed_modules.remove(&name)
            {
                let mut type_warnings = Vec::new();

                let ast = ast
                    .infer(
                        &self.id_gen,
                        kind,
                        &self.config.name.to_string(),
                        &self.module_types,
                        tracing,
                        &mut type_warnings,
                    )
                    .map_err(|error| Error::Type {
                        path: path.clone(),
                        src: code.clone(),
                        named: NamedSource::new(path.display().to_string(), code.clone()),
                        error,
                    })?;

                if validate_module_name {
                    ast.validate_module_name()?;
                }

                // Register any warnings emitted as type warnings
                let type_warnings = type_warnings
                    .into_iter()
                    .map(|w| Warning::from_type_warning(w, path.clone(), code.clone()));

                if !is_dependency {
                    self.warnings.extend(type_warnings);
                }

                // Register module sources for an easier access later.
                self.module_sources
                    .insert(name.clone(), (code.clone(), LineNumbers::new(&code)));

                // Register the types from this module so they can be
                // imported into other modules.
                self.module_types
                    .insert(name.clone(), ast.type_info.clone());

                // Register function definitions & data-types for easier access later.
                ast.register_definitions(&mut self.functions, &mut self.data_types);

                let checked_module = CheckedModule {
                    kind,
                    extra,
                    name: name.clone(),
                    code,
                    ast,
                    package,
                    input_path: path,
                };

                self.checked_modules.insert(name, checked_module);
            }
        }

        Ok(())
    }

    fn collect_tests(
        &mut self,
        verbose: bool,
        match_tests: Option<Vec<String>>,
        exact_match: bool,
        tracing: Tracing,
    ) -> Result<Vec<Test>, Error> {
        let mut scripts = Vec::new();

        let match_tests = match_tests.map(|mt| {
            mt.into_iter()
                .map(|match_test| {
                    let mut match_split_dot = match_test.split('.');

                    let match_module = if match_test.contains('.') || match_test.contains('/') {
                        match_split_dot.next().unwrap_or("")
                    } else {
                        ""
                    };

                    let match_names = match_split_dot.next().map(|names| {
                        let names = names.replace(&['{', '}'][..], "");

                        let names_split_comma = names.split(',');

                        names_split_comma.map(str::to_string).collect()
                    });

                    (match_module.to_string(), match_names)
                })
                .collect::<Vec<(String, Option<Vec<String>>)>>()
        });

        for checked_module in self.checked_modules.values() {
            if checked_module.package != self.config.name.to_string() {
                continue;
            }

            for def in checked_module.ast.definitions() {
                if let Definition::Test(func) = def {
                    if let Some(match_tests) = &match_tests {
                        let is_match = match_tests.iter().any(|(module, names)| {
                            let matched_module =
                                module.is_empty() || checked_module.name.contains(module);

                            let matched_name = match names {
                                None => true,
                                Some(names) => names.iter().any(|name| {
                                    if exact_match {
                                        name == &func.name
                                    } else {
                                        func.name.contains(name)
                                    }
                                }),
                            };

                            matched_module && matched_name
                        });

                        if is_match {
                            scripts.push((
                                checked_module.input_path.clone(),
                                checked_module.name.clone(),
                                func,
                            ))
                        }
                    } else {
                        scripts.push((
                            checked_module.input_path.clone(),
                            checked_module.name.clone(),
                            func,
                        ))
                    }
                }
            }
        }

        let mut generator = self.new_generator(tracing);

        let mut tests = Vec::new();

        for (input_path, module_name, test) in scripts.into_iter() {
            if verbose {
                self.event_listener.handle_event(Event::GeneratingUPLCFor {
                    name: test.name.clone(),
                    path: input_path.clone(),
                })
            }

            tests.push(Test::from_function_definition(
                &mut generator,
                test.to_owned(),
                module_name,
                input_path,
            ));
        }

        Ok(tests)
    }

    fn run_tests(&self, tests: Vec<Test>, seed: u32) -> Vec<TestResult<UntypedExpr>> {
        use rayon::prelude::*;

        let data_types = utils::indexmap::as_ref_values(&self.data_types);

        tests
            .into_par_iter()
            .map(|test| match test {
                Test::UnitTest(unit_test) => unit_test.run(),
                // TODO: Get the seed from the command-line, defaulting to a random one when not
                // provided.
                Test::PropertyTest(property_test) => property_test.run(seed),
            })
            .collect::<Vec<TestResult<PlutusData>>>()
            .into_iter()
            .map(|test| test.reify(&data_types))
            .collect()
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
        let name = self.module_name(dir, &path);
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

    fn module_name(&self, package_path: &Path, full_module_path: &Path) -> String {
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
        name.replace('\\', "/").replace('-', "_")
    }
}

fn is_aiken_path(path: &Path, dir: impl AsRef<Path>) -> bool {
    use regex::Regex;

    let re = Regex::new(&format!(
        "^({module}{slash})*{module}(\\.[-_a-z0-9]*)*\\.ak$",
        module = "[a-z][-_a-z0-9]*",
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
