use super::{
    error::{assert_min_arity, assert_return_bool, Error},
    schema::{Annotated, Schema},
};
use crate::module::{CheckedModule, CheckedModules};
use aiken_lang::{ast::TypedFunction, uplc::CodeGenerator};
use miette::NamedSource;
use serde;
use std::{
    collections::HashMap,
    fmt::{self, Display},
};
use uplc::ast::{DeBruijn, Program};

#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
pub struct Validator<T> {
    pub title: String,
    pub purpose: Purpose,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub datum: Option<Annotated<T>>,
    pub redeemer: Annotated<T>,
    #[serde(flatten)]
    pub program: Program<DeBruijn>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum Purpose {
    Spend,
    Mint,
    Withdraw,
    Publish,
}

impl Display for Validator<Schema> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = serde_json::to_string_pretty(self).map_err(|_| fmt::Error)?;
        f.write_str(&s)
    }
}

impl Validator<Schema> {
    pub fn from_checked_module(
        modules: &CheckedModules,
        generator: &mut CodeGenerator,
        validator: &CheckedModule,
        def: &TypedFunction,
    ) -> Result<Validator<Schema>, Error> {
        let purpose: Purpose = def
            .name
            .clone()
            .try_into()
            .expect("unexpected validator name");

        assert_return_bool(validator, def)?;
        assert_min_arity(validator, def, purpose.min_arity())?;

        let mut args = def.arguments.iter().rev();
        let (_, redeemer, datum) = (args.next(), args.next().unwrap(), args.next());

        Ok(Validator {
            title: validator.name.clone(),
            description: None,
            purpose,
            datum: datum
                .map(|datum| {
                    Annotated::from_type(modules.into(), &datum.tipo, &HashMap::new()).map_err(
                        |error| Error::Schema {
                            error,
                            location: datum.location,
                            source_code: NamedSource::new(
                                validator.input_path.display().to_string(),
                                validator.code.clone(),
                            ),
                        },
                    )
                })
                .transpose()?,
            redeemer: Annotated::from_type(modules.into(), &redeemer.tipo, &HashMap::new())
                .map_err(|error| Error::Schema {
                    error,
                    location: redeemer.location,
                    source_code: NamedSource::new(
                        validator.input_path.display().to_string(),
                        validator.code.clone(),
                    ),
                })?,
            program: generator
                .generate(&def.body, &def.arguments, true)
                .try_into()
                .unwrap(),
        })
    }
}

impl Purpose {
    pub fn min_arity(&self) -> u8 {
        match self {
            Purpose::Spend => 3,
            Purpose::Mint | Purpose::Withdraw | Purpose::Publish => 2,
        }
    }
}

impl Display for Purpose {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(match self {
            Purpose::Spend => "spend",
            Purpose::Mint => "mint",
            Purpose::Withdraw => "withdraw",
            Purpose::Publish => "publish",
        })
    }
}

impl TryFrom<String> for Purpose {
    type Error = String;

    fn try_from(purpose: String) -> Result<Purpose, Self::Error> {
        match &purpose[..] {
            "spend" => Ok(Purpose::Spend),
            "mint" => Ok(Purpose::Mint),
            "withdraw" => Ok(Purpose::Withdraw),
            "publish" => Ok(Purpose::Publish),
            unexpected => Err(format!("Can't turn '{}' into any Purpose", unexpected)),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{module::ParsedModule, PackageName};
    use aiken_lang::{
        self,
        ast::{ModuleKind, TypedDataType, TypedFunction},
        builder::{DataTypeKey, FunctionAccessKey},
        builtins, parser,
        tipo::TypeInfo,
        IdGenerator,
    };
    use assert_json_diff::assert_json_eq;
    use indexmap::IndexMap;
    use serde_json::{self, json};
    use std::{collections::HashMap, path::PathBuf};

    // TODO: Possible refactor this out of the module and have it used by `Project`. The idea would
    // be to make this struct below the actual project, and wrap it in another metadata struct
    // which contains all the config and I/O stuff regarding the project.
    struct TestProject {
        package: PackageName,
        id_gen: IdGenerator,
        module_types: HashMap<String, TypeInfo>,
        functions: IndexMap<FunctionAccessKey, TypedFunction>,
        data_types: IndexMap<DataTypeKey, TypedDataType>,
    }

    impl TestProject {
        fn new() -> Self {
            let id_gen = IdGenerator::new();

            let package = PackageName {
                owner: "test".to_owned(),
                repo: "project".to_owned(),
            };

            let mut module_types = HashMap::new();
            module_types.insert("aiken".to_string(), builtins::prelude(&id_gen));
            module_types.insert("aiken/builtin".to_string(), builtins::plutus(&id_gen));

            let functions = builtins::prelude_functions(&id_gen);
            let data_types = builtins::prelude_data_types(&id_gen);

            TestProject {
                package,
                id_gen,
                module_types,
                functions,
                data_types,
            }
        }

        fn parse(&self, source_code: &str) -> ParsedModule {
            let kind = ModuleKind::Validator;
            let name = "test_module".to_owned();
            let (mut ast, extra) =
                parser::module(source_code, kind).expect("Failed to parse module");
            ast.name = name.clone();
            let mut module = ParsedModule {
                kind,
                ast,
                code: source_code.to_string(),
                name,
                path: PathBuf::new(),
                extra,
                package: self.package.to_string(),
            };
            module.attach_doc_and_module_comments();
            module
        }

        fn check(&mut self, module: ParsedModule) -> CheckedModule {
            let mut warnings = vec![];

            let ast = module
                .ast
                .infer(
                    &self.id_gen,
                    module.kind,
                    &self.package.to_string(),
                    &self.module_types,
                    &mut warnings,
                )
                .expect("Failed to type-check module");

            self.module_types
                .insert(module.name.clone(), ast.type_info.clone());

            CheckedModule {
                kind: module.kind,
                extra: module.extra,
                name: module.name,
                code: module.code,
                package: module.package,
                input_path: module.path,
                ast,
            }
        }
    }

    fn assert_validator(source_code: &str, json: serde_json::Value) {
        let mut project = TestProject::new();

        let modules = CheckedModules::singleton(project.check(project.parse(source_code)));
        let mut generator = modules.new_generator(
            &project.functions,
            &project.data_types,
            &project.module_types,
        );

        let (validator, def) = modules
            .validators()
            .next()
            .expect("source code did no yield any validator");

        let validator = Validator::from_checked_module(&modules, &mut generator, validator, def)
            .expect("Failed to create validator blueprint");

        println!("{}", validator);
        assert_json_eq!(serde_json::to_value(&validator).unwrap(), json);
    }

    #[test]
    fn validator_mint_basic() {
        assert_validator(
            r#"
            fn mint(redeemer: Data, ctx: Data) {
                True
            }
            "#,
            json!({
              "title": "test_module",
              "purpose": "mint",
              "hash": "f9fcaa5bfce8bde3b85e595b5235a184fe0fb79916d38273c74a23cf",
              "redeemer": {
                "title": "Data",
                "description": "Any Plutus data."
              },
              "compiledCode": "582e0100003232225333573494452616300100122253335573e004293099ab9b3001357420046660060066ae88008005"
            }),
        );
    }

    #[test]
    fn validator_spend() {
        assert_validator(
            r#"
            /// On-chain state
            type State {
                /// The contestation period as a number of seconds
                contestationPeriod: ContestationPeriod,
                /// List of public key hashes of all participants
                parties: List<Party>,
                utxoHash: Hash<Blake2b_256>,
            }

            /// A Hash digest for a given algorithm.
            type Hash<alg> = ByteArray

            type Blake2b_256 { Blake2b_256 }

            /// Whatever
            type ContestationPeriod {
              /// A positive, non-zero number of seconds.
              ContestationPeriod(Int)
            }

            type Party =
              ByteArray

            type Input {
                CollectCom
                Close
                /// Abort a transaction
                Abort
            }

            fn spend(datum: State, redeemer: Input, ctx: Data) {
                True
            }
            "#,
            json!({
              "title": "test_module",
              "purpose": "spend",
              "hash": "3b7ee6139deb59d892955ac3cad15d53e48dcb1643227256b29d2b6f",
              "datum": {
                "title": "State",
                "description": "On-chain state",
                "anyOf": [
                  {
                    "title": "State",
                    "dataType": "constructor",
                    "index": 0,
                    "fields": [
                      {
                        "title": "contestationPeriod",
                        "description": "The contestation period as a number of seconds",
                        "anyOf": [
                          {
                            "title": "ContestationPeriod",
                            "description": "A positive, non-zero number of seconds.",
                            "dataType": "constructor",
                            "index": 0,
                            "fields": [
                              {
                                "dataType": "integer"
                              }
                            ]
                          }
                        ]
                      },
                      {
                        "title": "parties",
                        "description": "List of public key hashes of all participants",
                        "dataType": "list",
                        "items": {
                          "dataType": "bytes"
                        }
                      },
                      {
                        "title": "utxoHash",
                        "dataType": "bytes"
                      }
                    ]
                  }
                ]
              },
              "redeemer": {
                "title": "Input",
                "anyOf": [
                  {
                    "title": "CollectCom",
                    "dataType": "constructor",
                    "index": 0,
                    "fields": []
                  },
                  {
                    "title": "Close",
                    "dataType": "constructor",
                    "index": 1,
                    "fields": []
                  },
                  {
                    "title": "Abort",
                    "description": "Abort a transaction",
                    "dataType": "constructor",
                    "index": 2,
                    "fields": []
                  }
                ]
              },
              "compiledCode": "582f01000032322225333573494452616300100122253335573e004293099ab9b3001357420046660060066ae880080041"
            }),
        );
    }

    #[test]
    fn validator_spend_2tuple() {
        assert_validator(
            r#"
            fn spend(datum: (Int, ByteArray), redeemer: String, ctx: Void) {
                True
            }
            "#,
            json!({
              "title": "test_module",
              "purpose": "spend",
              "hash": "4a0c0768ff3e8c8f9da5fc2c499e592ae37f676a11dbc6e9de958116",
              "datum": {
                "dataType": "#pair",
                "left": {
                  "dataType": "integer"
                },
                "right": {
                  "dataType": "bytes"
                }
              },
              "redeemer": {
                "dataType": "#string"
              },
              "compiledCode": "584901000032322322322533357349445261637326eb8004c8c8cdd81aba1002357420026ae88004dd600098008009112999aab9f00214984cd5cd98009aba100233300300335744004003"
            }),
        )
    }

    #[test]
    fn validator_spend_tuples() {
        assert_validator(
            r#"
            fn spend(datum: (Int, Int, Int), redeemer: Data, ctx: Void) {
                True
            }
            "#,
            json!({
              "title": "test_module",
              "purpose": "spend",
              "hash": "5e7487927f32a4d6e8c3b462c8e0e0f685506621f5f2683807805d0e",
              "datum": {
                "title": "Tuple",
                "dataType": "#list",
                "elements": [
                  {
                    "dataType": "integer"
                  },
                  {
                    "dataType": "integer"
                  },
                  {
                    "dataType": "integer"
                  }
                ]
              },
              "redeemer": {
                "title": "Data",
                "description": "Any Plutus data."
              },
              "compiledCode": "5833010000323223222533357349445261637580026002002444a666aae7c008526133573660026ae84008ccc00c00cd5d10010009"
            }),
        )
    }

    #[test]
    fn validator_generics() {
        assert_validator(
            r#"
            type Either<left, right> {
                Left(left)
                Right(right)
            }

            type Interval<a> {
                Finite(a)
                Infinite
            }

            fn withdraw(redeemer: Either<ByteArray, Interval<Int>>, ctx: Void) {
                True
            }
            "#,
            json!(
                {
                  "title": "test_module",
                  "purpose": "withdraw",
                  "hash": "f9fcaa5bfce8bde3b85e595b5235a184fe0fb79916d38273c74a23cf",
                  "redeemer": {
                    "title": "Either",
                    "anyOf": [
                      {
                        "title": "Left",
                        "dataType": "constructor",
                        "index": 0,
                        "fields": [
                          {
                            "dataType": "bytes"
                          }
                        ]
                      },
                      {
                        "title": "Right",
                        "dataType": "constructor",
                        "index": 1,
                        "fields": [
                          {
                            "title": "Interval",
                            "anyOf": [
                              {
                                "title": "Finite",
                                "dataType": "constructor",
                                "index": 0,
                                "fields": [
                                  {
                                    "dataType": "integer"
                                  }
                                ]
                              },
                              {
                                "title": "Infinite",
                                "dataType": "constructor",
                                "index": 1,
                                "fields": []
                              }
                            ]
                          }
                        ]
                      }
                    ]
                  },
                  "compiledCode": "582e0100003232225333573494452616300100122253335573e004293099ab9b3001357420046660060066ae88008005"
                }
            ),
        )
    }

    #[test]
    fn validator_phantom_types() {
        assert_validator(
            r#"
            type Dict<key, value> {
                inner: List<(ByteArray, value)>
            }

            type UUID { UUID }

            fn mint(redeemer: Dict<UUID, Int>, ctx: Void) {
                True
            }
            "#,
            json!(
                {
                  "title": "test_module",
                  "purpose": "mint",
                  "hash": "f9fcaa5bfce8bde3b85e595b5235a184fe0fb79916d38273c74a23cf",
                  "redeemer": {
                    "title": "Dict",
                    "anyOf": [
                      {
                        "title": "Dict",
                        "dataType": "constructor",
                        "index": 0,
                        "fields": [
                          {
                            "title": "inner",
                            "dataType": "map",
                            "keys": {
                              "dataType": "bytes"
                            },
                            "values": {
                              "dataType": "integer"
                            }
                          }
                        ]
                      }
                    ]
                  },
                  "compiledCode": "582e0100003232225333573494452616300100122253335573e004293099ab9b3001357420046660060066ae88008005"
                }
            ),
        );
    }

    #[test]
    fn validator_opaque_types() {
        assert_validator(
            r#"
            pub opaque type Dict<key, value> {
                inner: List<(ByteArray, value)>
            }

            type UUID { UUID }

            fn mint(redeemer: Dict<UUID, Int>, ctx: Void) {
                True
            }
            "#,
            json!(
                {
                  "title": "test_module",
                  "purpose": "mint",
                  "hash": "f9fcaa5bfce8bde3b85e595b5235a184fe0fb79916d38273c74a23cf",
                  "redeemer": {
                    "title": "Dict",
                    "dataType": "map",
                    "keys": {
                      "dataType": "bytes"
                    },
                    "values": {
                      "dataType": "integer"
                    }
                  },
                  "compiledCode": "582e0100003232225333573494452616300100122253335573e004293099ab9b3001357420046660060066ae88008005"
                }
            ),
        );
    }
}
