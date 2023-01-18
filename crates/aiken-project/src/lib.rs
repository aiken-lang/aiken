pub mod config;
pub mod deps;
pub mod docs;
pub mod error;
pub mod format;
pub mod module;
pub mod options;
pub mod package_name;
pub mod paths;
pub mod pretty;
pub mod script;
pub mod telemetry;

use aiken_lang::{
    ast::{Definition, Function, ModuleKind, TypedDataType, TypedDefinition, TypedFunction},
    builder::{DataTypeKey, FunctionAccessKey},
    builtins::{self, generic_var},
    tipo::TypeInfo,
    uplc::CodeGenerator,
    IdGenerator, CERT, MINT, SPEND, VALIDATOR_NAMES, WITHDRAW,
};
use deps::UseManifest;
use miette::NamedSource;
use options::{CodeGenMode, Options};
use package_name::PackageName;
use pallas::{
    codec::minicbor,
    ledger::{addresses::Address, primitives::babbage},
};
use pallas_traverse::ComputeHash;
use script::{EvalHint, EvalInfo, Script};
use serde_json::json;
use std::{
    collections::HashMap,
    fs,
    path::{Path, PathBuf},
};
use telemetry::EventListener;
use uplc::{
    ast::{Constant, DeBruijn, Program, Term},
    machine::cost_model::ExBudget,
};

use crate::{
    config::Config,
    error::{Error, Warning},
    module::{CheckedModule, CheckedModules, ParsedModule, ParsedModules},
    telemetry::Event,
};

#[derive(Debug)]
pub struct Source {
    pub path: PathBuf,
    pub name: String,
    pub code: String,
    pub kind: ModuleKind,
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
    pub warnings: Vec<Warning>,
    event_listener: T,
}

impl<T> Project<T>
where
    T: EventListener,
{
    pub fn new(root: PathBuf, event_listener: T) -> Result<Project<T>, Error> {
        let id_gen = IdGenerator::new();

        let mut module_types = HashMap::new();

        module_types.insert("aiken".to_string(), builtins::prelude(&id_gen));
        module_types.insert("aiken/builtin".to_string(), builtins::plutus(&id_gen));

        let config = Config::load(&root)?;

        Ok(Project {
            config,
            checked_modules: CheckedModules::default(),
            defined_modules: HashMap::new(),
            id_gen,
            module_types,
            root,
            sources: vec![],
            warnings: vec![],
            event_listener,
        })
    }

    pub fn build(&mut self, uplc: bool) -> Result<(), Error> {
        let options = Options {
            code_gen_mode: CodeGenMode::Build(uplc),
        };

        self.compile(options)
    }

    pub fn docs(&mut self, destination: Option<PathBuf>) -> Result<(), Error> {
        self.compile_deps()?;

        self.event_listener
            .handle_event(Event::BuildingDocumentation {
                root: self.root.clone(),
                name: self.config.name.to_string(),
                version: self.config.version.clone(),
            });

        self.read_source_files()?;

        let destination = destination.unwrap_or_else(|| self.root.join("docs"));

        let mut parsed_modules = self.parse_sources(self.config.name.clone())?;

        for (_, module) in parsed_modules.iter_mut() {
            module.attach_doc_and_module_comments();
        }

        self.type_check(parsed_modules)?;

        self.event_listener.handle_event(Event::GeneratingDocFiles {
            output_path: destination.clone(),
        });

        let doc_files = docs::generate_all(
            &self.root,
            &self.config,
            self.checked_modules.values().collect(),
        );

        for file in doc_files {
            let path = destination.join(file.path);
            fs::create_dir_all(path.parent().unwrap())?;
            fs::write(&path, file.content)?;
        }

        Ok(())
    }

    pub fn check(
        &mut self,
        skip_tests: bool,
        match_tests: Option<Vec<String>>,
        verbose: bool,
        exact_match: bool,
    ) -> Result<(), Error> {
        let options = Options {
            code_gen_mode: if skip_tests {
                CodeGenMode::NoOp
            } else {
                CodeGenMode::Test {
                    match_tests,
                    verbose,
                    exact_match,
                }
            },
        };

        self.compile(options)
    }

    pub fn compile(&mut self, options: Options) -> Result<(), Error> {
        self.compile_deps()?;

        self.event_listener
            .handle_event(Event::StartingCompilation {
                root: self.root.clone(),
                name: self.config.name.to_string(),
                version: self.config.version.clone(),
            });

        self.read_source_files()?;

        let parsed_modules = self.parse_sources(self.config.name.clone())?;

        self.type_check(parsed_modules)?;

        let validators = self.validate_validators()?;

        match options.code_gen_mode {
            CodeGenMode::Build(uplc_dump) => {
                if validators.is_empty() {
                    self.warnings.push(Warning::NoValidators);
                }

                let programs = self.code_gen(validators)?;

                self.write_build_outputs(programs, uplc_dump)?;

                Ok(())
            }
            CodeGenMode::Test {
                match_tests,
                verbose,
                exact_match,
            } => {
                let tests =
                    self.collect_scripts(verbose, |def| matches!(def, Definition::Test(..)))?;

                if !tests.is_empty() {
                    self.event_listener.handle_event(Event::RunningTests);
                }

                let results = self.eval_scripts(tests, match_tests, exact_match);

                let errors: Vec<Error> = results
                    .iter()
                    .filter_map(|e| {
                        if e.success {
                            None
                        } else {
                            Some(Error::TestFailure {
                                name: e.script.name.clone(),
                                path: e.script.input_path.clone(),
                                evaluation_hint: e.script.evaluation_hint.clone(),
                                src: e.script.program.to_pretty(),
                                verbose,
                            })
                        }
                    })
                    .collect();

                self.event_listener
                    .handle_event(Event::FinishedTests { tests: results });

                if !errors.is_empty() {
                    Err(Error::List(errors))
                } else {
                    Ok(())
                }
            }
            CodeGenMode::NoOp => Ok(()),
        }
    }

    fn compile_deps(&mut self) -> Result<(), Error> {
        let manifest = deps::download(
            &self.event_listener,
            UseManifest::Yes,
            &self.root,
            &self.config,
        )?;

        for package in manifest.packages {
            let lib = self.root.join(paths::build_deps_package(&package.name));

            self.event_listener
                .handle_event(Event::StartingCompilation {
                    root: lib.clone(),
                    name: package.name.to_string(),
                    version: package.version.clone(),
                });

            self.read_package_source_files(&lib.join("lib"))?;

            let parsed_modules = self.parse_sources(package.name)?;

            self.type_check(parsed_modules)?;
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

    fn parse_sources(&mut self, package_name: PackageName) -> Result<ParsedModules, Error> {
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
                        });
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
            Err(Error::List(errors))
        }
    }

    fn type_check(&mut self, mut parsed_modules: ParsedModules) -> Result<(), Error> {
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
                        &mut type_warnings,
                    )
                    .map_err(|error| Error::Type {
                        path: path.clone(),
                        src: code.clone(),
                        named: NamedSource::new(path.display().to_string(), code.clone()),
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

                self.checked_modules.insert(
                    name.clone(),
                    CheckedModule {
                        kind,
                        extra,
                        name,
                        code,
                        ast,
                        package,
                        input_path: path,
                    },
                );
            }
        }

        Ok(())
    }

    fn validate_validators(&self) -> Result<Vec<(PathBuf, String, TypedFunction)>, Error> {
        let mut errors = Vec::new();
        let mut validators = Vec::new();

        for module in self.checked_modules.validators() {
            for def in module.ast.definitions() {
                if let Definition::Fn(func_def) = def {
                    if VALIDATOR_NAMES.contains(&func_def.name.as_str()) {
                        // validators must return a Bool
                        if !func_def.return_type.is_bool() {
                            errors.push(Error::ValidatorMustReturnBool {
                                location: func_def.location,
                                src: module.code.clone(),
                                path: module.input_path.clone(),
                                named: NamedSource::new(
                                    module.input_path.display().to_string(),
                                    module.code.clone(),
                                ),
                            })
                        }

                        // depending on name, validate the minimum number of arguments
                        // if too low, push a new error on to errors
                        if [MINT, CERT, WITHDRAW].contains(&func_def.name.as_str())
                            && func_def.arguments.len() < 2
                        {
                            errors.push(Error::WrongValidatorArity {
                                location: func_def.location,
                                src: module.code.clone(),
                                path: module.input_path.clone(),
                                named: NamedSource::new(
                                    module.input_path.display().to_string(),
                                    module.code.clone(),
                                ),
                                name: func_def.name.clone(),
                                at_least: 2,
                            })
                        }

                        if SPEND == func_def.name && func_def.arguments.len() < 3 {
                            errors.push(Error::WrongValidatorArity {
                                location: func_def.location,
                                src: module.code.clone(),
                                path: module.input_path.clone(),
                                named: NamedSource::new(
                                    module.input_path.display().to_string(),
                                    module.code.clone(),
                                ),
                                name: func_def.name.clone(),
                                at_least: 3,
                            })
                        }

                        validators.push((
                            module.input_path.clone(),
                            module.name.clone(),
                            func_def.clone(),
                        ));
                    }
                }
            }
        }

        if errors.is_empty() {
            Ok(validators)
        } else {
            Err(Error::List(errors))
        }
    }

    fn code_gen(
        &mut self,
        validators: Vec<(PathBuf, String, TypedFunction)>,
    ) -> Result<Vec<Script>, Error> {
        let mut programs = Vec::new();
        let mut functions = HashMap::new();
        let mut type_aliases = HashMap::new();
        let mut data_types = HashMap::new();

        let prelude_functions = builtins::prelude_functions(&self.id_gen);
        for (access_key, func) in prelude_functions.iter() {
            functions.insert(access_key.clone(), func);
        }

        let option_data_type = TypedDataType::option(generic_var(self.id_gen.next()));
        data_types.insert(
            DataTypeKey {
                module_name: "".to_string(),
                defined_type: "Option".to_string(),
            },
            &option_data_type,
        );

        for module in self.checked_modules.values() {
            for def in module.ast.definitions() {
                match def {
                    Definition::Fn(func) => {
                        functions.insert(
                            FunctionAccessKey {
                                module_name: module.name.clone(),
                                function_name: func.name.clone(),
                                variant_name: String::new(),
                            },
                            func,
                        );
                    }
                    Definition::TypeAlias(ta) => {
                        type_aliases.insert((module.name.clone(), ta.alias.clone()), ta);
                    }
                    Definition::DataType(dt) => {
                        data_types.insert(
                            DataTypeKey {
                                module_name: module.name.clone(),
                                defined_type: dt.name.clone(),
                            },
                            dt,
                        );
                    }

                    Definition::ModuleConstant(_) | Definition::Test(_) | Definition::Use(_) => {}
                }
            }
        }

        for (input_path, module_name, func_def) in validators {
            let Function {
                arguments,
                name,
                body,
                ..
            } = func_def;

            let mut generator = CodeGenerator::new(
                &functions,
                // &type_aliases,
                &data_types,
                &self.module_types,
            );

            self.event_listener.handle_event(Event::GeneratingUPLC {
                output_path: self.output_path().join(&module_name).join(&name),
                name: format!("{}.{}", module_name, name),
            });

            let program = generator.generate(body, arguments, true);

            let script = Script::new(
                input_path,
                module_name,
                name,
                program.try_into().unwrap(),
                None,
            );

            programs.push(script);
        }

        Ok(programs)
    }

    fn collect_scripts(
        &mut self,
        verbose: bool,
        should_collect: fn(&TypedDefinition) -> bool,
    ) -> Result<Vec<Script>, Error> {
        let mut programs = Vec::new();
        let mut functions = HashMap::new();
        let mut type_aliases = HashMap::new();
        let mut data_types = HashMap::new();

        let prelude_functions = builtins::prelude_functions(&self.id_gen);
        for (access_key, func) in prelude_functions.iter() {
            functions.insert(access_key.clone(), func);
        }

        let option_data_type = TypedDataType::option(generic_var(self.id_gen.next()));

        data_types.insert(
            DataTypeKey {
                module_name: "".to_string(),
                defined_type: "Option".to_string(),
            },
            &option_data_type,
        );

        let mut scripts = Vec::new();

        for module in self.checked_modules.values() {
            for def in module.ast.definitions() {
                match def {
                    Definition::Fn(func) => {
                        functions.insert(
                            FunctionAccessKey {
                                module_name: module.name.clone(),
                                function_name: func.name.clone(),
                                variant_name: String::new(),
                            },
                            func,
                        );

                        if should_collect(def) && module.package == self.config.name.to_string() {
                            scripts.push((module.input_path.clone(), module.name.clone(), func));
                        }
                    }
                    Definition::Test(func) => {
                        if should_collect(def) && module.package == self.config.name.to_string() {
                            scripts.push((module.input_path.clone(), module.name.clone(), func));
                        }
                    }
                    Definition::TypeAlias(ta) => {
                        type_aliases.insert((module.name.clone(), ta.alias.clone()), ta);
                    }
                    Definition::DataType(dt) => {
                        data_types.insert(
                            DataTypeKey {
                                module_name: module.name.clone(),
                                defined_type: dt.name.clone(),
                            },
                            dt,
                        );
                    }
                    Definition::Use(_) | Definition::ModuleConstant(_) => (),
                }
            }
        }

        for (input_path, module_name, func_def) in scripts {
            let Function {
                arguments,
                name,
                body,
                ..
            } = func_def;

            if verbose {
                self.event_listener.handle_event(Event::GeneratingUPLCFor {
                    name: name.clone(),
                    path: input_path.clone(),
                })
            }

            let mut generator = CodeGenerator::new(
                &functions,
                // &type_aliases,
                &data_types,
                &self.module_types,
            );

            let evaluation_hint = if let Some((bin_op, left_src, right_src)) = func_def.test_hint()
            {
                let left = CodeGenerator::new(&functions, &data_types, &self.module_types)
                    .generate(*left_src, vec![], false)
                    .try_into()
                    .unwrap();

                let right = CodeGenerator::new(&functions, &data_types, &self.module_types)
                    .generate(*right_src, vec![], false)
                    .try_into()
                    .unwrap();

                Some(EvalHint {
                    bin_op,
                    left,
                    right,
                })
            } else {
                None
            };

            let program = generator.generate(body.clone(), arguments.clone(), false);

            let script = Script::new(
                input_path,
                module_name,
                name.to_string(),
                program.try_into().unwrap(),
                evaluation_hint,
            );

            programs.push(script);
        }

        Ok(programs)
    }

    fn eval_scripts(
        &self,
        scripts: Vec<Script>,
        match_tests: Option<Vec<String>>,
        exact_match: bool,
    ) -> Vec<EvalInfo> {
        use rayon::prelude::*;

        // TODO: in the future we probably just want to be able to
        // tell the machine to not explode on budget consumption.
        let initial_budget = ExBudget {
            mem: i64::MAX,
            cpu: i64::MAX,
        };

        let scripts = if let Some(match_tests) = match_tests {
            let match_tests: Vec<(&str, Option<Vec<String>>)> = match_tests
                .iter()
                .map(|match_test| {
                    let mut match_split_dot = match_test.split('.');

                    let match_module = if match_test.contains('.') {
                        match_split_dot.next().unwrap_or("")
                    } else {
                        ""
                    };

                    let match_names = match_split_dot.next().map(|names| {
                        let names = names.replace(&['{', '}'][..], "");

                        let names_split_comma = names.split(',');

                        names_split_comma.map(str::to_string).collect()
                    });

                    (match_module, match_names)
                })
                .collect();

            scripts
                .into_iter()
                .filter(|script| -> bool {
                    match_tests.iter().any(|(module, names)| {
                        let matched_module = module == &"" || script.module.contains(module);

                        let matched_name = matches!(names, Some(names) if names
                            .iter()
                            .any(|name| if exact_match {
                                name == &script.name
                            } else {
                                script.name.contains(name)
                            }
                        ));

                        matched_module && matched_name
                    })
                })
                .collect::<Vec<Script>>()
        } else {
            scripts
        };

        scripts
            .into_par_iter()
            .map(|script| match script.program.eval(initial_budget) {
                (Ok(result), remaining_budget, logs) => EvalInfo {
                    success: result != Term::Error
                        && result != Term::Constant(Constant::Bool(false)),
                    script,
                    spent_budget: initial_budget - remaining_budget,
                    output: Some(result),
                    logs,
                },
                (Err(..), remaining_budget, logs) => EvalInfo {
                    success: false,
                    script,
                    spent_budget: initial_budget - remaining_budget,
                    output: None,
                    logs,
                },
            })
            .collect()
    }

    fn output_path(&self) -> PathBuf {
        self.root.join("assets")
    }

    fn write_build_outputs(&self, programs: Vec<Script>, uplc_dump: bool) -> Result<(), Error> {
        for script in programs {
            let script_output_dir = self.output_path().join(script.module).join(script.name);

            fs::create_dir_all(&script_output_dir)?;

            // dump textual uplc
            if uplc_dump {
                let uplc_path = script_output_dir.join("raw.uplc");

                fs::write(uplc_path, script.program.to_pretty())?;
            }

            let program: Program<DeBruijn> = script.program.into();

            let cbor = program.to_cbor().unwrap();

            // Create file containing just the script cbor hex
            let script_path = script_output_dir.join("script.cbor");

            let cbor_hex = hex::encode(&cbor);

            fs::write(script_path, cbor_hex)?;

            // Create the payment script JSON file
            let payment_script_path = script_output_dir.join("payment_script.json");

            let mut bytes = Vec::new();

            let mut encoder = minicbor::Encoder::new(&mut bytes);

            encoder.bytes(&cbor).unwrap();

            let prefixed_cbor_hex = hex::encode(&bytes);

            let payment_script = json!({
                "type": "PlutusScriptV2",
                "description": "Generated by Aiken",
                "cborHex": prefixed_cbor_hex
            });

            fs::write(
                payment_script_path,
                serde_json::to_string_pretty(&payment_script).unwrap(),
            )?;

            // Create mainnet and testnet addresses
            let plutus_script = babbage::PlutusV2Script(cbor.into());

            let hash = plutus_script.compute_hash();

            // mainnet
            let mainnet_path = script_output_dir.join("mainnet.addr");
            let mut mainnet_bytes: Vec<u8> = vec![0b01110001];

            mainnet_bytes.extend(hash.iter());

            let mainnet_addr = Address::from_bytes(&mainnet_bytes)
                .unwrap()
                .to_bech32()
                .unwrap();

            fs::write(mainnet_path, mainnet_addr)?;

            // testnet
            let testnet_path = script_output_dir.join("testnet.addr");
            let mut testnet_bytes: Vec<u8> = vec![0b01110000];

            testnet_bytes.extend(hash.iter());

            let testnet_addr = Address::from_bytes(&testnet_bytes)
                .unwrap()
                .to_bech32()
                .unwrap();

            fs::write(testnet_path, testnet_addr)?;
        }

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
        name.replace('\\', "/")
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
