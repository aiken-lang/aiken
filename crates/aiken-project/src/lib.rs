pub mod blueprint;
pub mod config;
pub mod deps;
pub mod docs;
pub mod error;
pub mod export;
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
    plutus_version::PlutusVersion,
    tipo::{Type, TypeInfo},
    IdGenerator,
};
use export::Export;
use indexmap::IndexMap;
use miette::NamedSource;
use options::{CodeGenMode, Options};
use package_name::PackageName;
use pallas::ledger::{
    addresses::{Address, Network, ShelleyAddress, ShelleyDelegationPart, StakePayload},
    primitives::conway::{self as cardano, PolicyId},
    traverse::ComputeHash,
};
use std::{
    collections::{BTreeSet, HashMap},
    fs::{self, File},
    io::BufReader,
    path::{Path, PathBuf},
    rc::Rc,
};
use telemetry::EventListener;
use test_framework::{Test, TestResult};
use uplc::{
    ast::{Constant, Name, Program},
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
    checks_count: Option<usize>,
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

        let demanded_compiler_version = format!("v{}", config.compiler);

        let mut project = Project::new_with_config(config, root, event_listener);

        let current_compiler_version = config::compiler_version(false);

        if demanded_compiler_version != current_compiler_version {
            project.warnings.push(Warning::CompilerVersionMismatch {
                demanded: demanded_compiler_version,
                current: current_compiler_version,
            })
        }

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
            checks_count: None,
            event_listener,
            functions,
            data_types,
            module_sources: HashMap::new(),
        }
    }

    pub fn new_generator(&'_ self, tracing: Tracing) -> CodeGenerator<'_> {
        CodeGenerator::new(
            self.config.plutus,
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

    pub fn docs(
        &mut self,
        destination: Option<PathBuf>,
        include_dependencies: bool,
    ) -> Result<(), Vec<Error>> {
        self.event_listener
            .handle_event(Event::BuildingDocumentation {
                root: self.root.clone(),
                name: self.config.name.to_string(),
                version: self.config.version.clone(),
            });

        self.read_source_files()?;

        let mut modules = self.parse_sources(self.config.name.clone())?;

        self.type_check(&mut modules, Tracing::silent(), false)?;

        let destination = destination.unwrap_or_else(|| self.root.join("docs"));

        self.event_listener.handle_event(Event::GeneratingDocFiles {
            output_path: destination.clone(),
        });

        let modules = self
            .checked_modules
            .values_mut()
            .filter(|CheckedModule { package, .. }| {
                include_dependencies || package == &self.config.name.to_string()
            })
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

    #[allow(clippy::too_many_arguments)]
    pub fn check(
        &mut self,
        skip_tests: bool,
        match_tests: Option<Vec<String>>,
        verbose: bool,
        exact_match: bool,
        seed: u32,
        property_max_success: usize,
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
                    property_max_success,
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
        self.event_listener
            .handle_event(Event::StartingCompilation {
                root: self.root.clone(),
                name: self.config.name.to_string(),
                version: self.config.version.clone(),
            });

        self.read_source_files()?;

        let mut modules = self.parse_sources(self.config.name.clone())?;

        self.type_check(&mut modules, options.tracing, true)?;

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
                property_max_success,
            } => {
                let tests =
                    self.collect_tests(verbose, match_tests, exact_match, options.tracing)?;

                if !tests.is_empty() {
                    self.event_listener.handle_event(Event::RunningTests);
                }

                let tests = self.run_tests(tests, seed, property_max_success);

                self.checks_count = if tests.is_empty() {
                    None
                } else {
                    Some(tests.iter().fold(0, |acc, test| {
                        acc + match test {
                            TestResult::PropertyTestResult(r) => r.iterations,
                            _ => 1,
                        }
                    }))
                };

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

                Ok(validator.program.address(
                    network,
                    delegation_part.to_owned(),
                    &self.config.plutus.into(),
                ))
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

                let validator_hash = match self.config.plutus {
                    PlutusVersion::V1 => cardano::PlutusV1Script(cbor.into()).compute_hash(),
                    PlutusVersion::V2 => cardano::PlutusV2Script(cbor.into()).compute_hash(),
                    PlutusVersion::V3 => cardano::PlutusV3Script(cbor.into()).compute_hash(),
                };

                Ok(validator_hash)
            }
        })
    }

    pub fn export(&self, module: &str, name: &str, tracing: Tracing) -> Result<Export, Error> {
        self.checked_modules
            .get(module)
            .and_then(|checked_module| {
                checked_module.ast.definitions().find_map(|def| match def {
                    Definition::Fn(func) if func.name == name => Some((checked_module, func)),
                    _ => None,
                })
            })
            .map(|(checked_module, func)| {
                let mut generator = self.new_generator(tracing);

                Export::from_function(func, checked_module, &mut generator, &self.checked_modules)
            })
            .transpose()?
            .ok_or_else(|| Error::ExportNotFound {
                module: module.to_string(),
                name: name.to_string(),
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

    fn with_dependencies(&mut self, parsed_packages: &mut ParsedModules) -> Result<(), Vec<Error>> {
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

            parsed_packages.extend(Into::<HashMap<_, _>>::into(parsed_modules));
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
        use rayon::prelude::*;

        let (parsed_modules, errors) = self
            .sources
            .par_drain(0..)
            .fold(
                || (ParsedModules::new(), Vec::new()),
                |(mut parsed_modules, mut errors), elem| {
                    let Source {
                        path,
                        name,
                        code,
                        kind,
                    } = elem;

                    match aiken_lang::parser::module(&code, kind) {
                        Ok((mut ast, extra)) => {
                            // Store the name
                            ast.name.clone_from(&name);

                            let module = ParsedModule {
                                kind,
                                ast,
                                code,
                                name,
                                path,
                                extra,
                                package: package_name.to_string(),
                            };

                            parsed_modules.insert(module.name.clone(), module);

                            (parsed_modules, errors)
                        }
                        Err(errs) => {
                            for error in errs {
                                errors.push((
                                    path.clone(),
                                    code.clone(),
                                    NamedSource::new(path.display().to_string(), code.clone()),
                                    Box::new(error),
                                ))
                            }

                            (parsed_modules, errors)
                        }
                    }
                },
            )
            .reduce(
                || (ParsedModules::new(), Vec::new()),
                |(mut parsed_modules, mut errors), (mut parsed, mut errs)| {
                    parsed_modules.extend(parsed.drain());

                    errors.append(&mut errs);

                    (parsed_modules, errors)
                },
            );

        let mut errors: Vec<Error> = errors
            .into_iter()
            .map(|(path, src, named, error)| Error::Parse {
                path,
                src,
                named,
                error,
            })
            .collect();

        for parsed_module in parsed_modules.values() {
            if let Some(first) = self
                .defined_modules
                .insert(parsed_module.name.clone(), parsed_module.path.clone())
            {
                errors.push(Error::DuplicateModule {
                    module: parsed_module.name.clone(),
                    first,
                    second: parsed_module.path.clone(),
                });
            }
        }

        if errors.is_empty() {
            Ok(parsed_modules)
        } else {
            Err(errors)
        }
    }

    fn type_check(
        &mut self,
        modules: &mut ParsedModules,
        tracing: Tracing,
        validate_module_name: bool,
    ) -> Result<(), Vec<Error>> {
        let our_modules: BTreeSet<String> = modules.keys().cloned().collect();

        self.with_dependencies(modules)?;

        for name in modules.sequence(&our_modules)? {
            if let Some(module) = modules.remove(&name) {
                let (checked_module, warnings) = module.infer(
                    &self.id_gen,
                    &self.config.name.to_string(),
                    tracing,
                    validate_module_name,
                    &mut self.module_sources,
                    &mut self.module_types,
                    &mut self.functions,
                    &mut self.data_types,
                )?;

                if our_modules.contains(checked_module.name.as_str()) {
                    self.warnings.extend(warnings);
                }

                self.checked_modules
                    .insert(checked_module.name.clone(), checked_module);
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

    fn run_tests(
        &self,
        tests: Vec<Test>,
        seed: u32,
        property_max_success: usize,
    ) -> Vec<TestResult<UntypedExpr, UntypedExpr>> {
        use rayon::prelude::*;

        let data_types = utils::indexmap::as_ref_values(&self.data_types);

        let plutus_version = &self.config.plutus;

        tests
            .into_par_iter()
            .map(|test| match test {
                Test::UnitTest(unit_test) => unit_test.run(plutus_version),
                Test::PropertyTest(property_test) => {
                    property_test.run(seed, property_max_success, plutus_version)
                }
            })
            .collect::<Vec<TestResult<(Constant, Rc<Type>), PlutusData>>>()
            .into_iter()
            .map(|test| test.reify(&data_types))
            .collect()
    }

    fn aiken_files(&mut self, dir: &Path, kind: ModuleKind) -> Result<(), Error> {
        walkdir::WalkDir::new(dir)
            .follow_links(true)
            .into_iter()
            .filter_map(Result::ok)
            .filter(|e| e.file_type().is_file())
            .try_for_each(|d| {
                let path = d.into_path();
                let keep = is_aiken_path(&path, dir);
                let ext = path.extension();

                if !keep && ext.unwrap_or_default() == "ak" {
                    self.warnings
                        .push(Warning::InvalidModuleName { path: path.clone() });
                }

                if keep {
                    self.add_module(path, dir, kind)
                } else {
                    Ok(())
                }
            })
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
