use super::{
    error::{assert_min_arity, assert_return_bool, Error},
    schema::{Annotated, Schema},
};
use crate::module::{CheckedModule, CheckedModules};
use aiken_lang::{ast::TypedFunction, uplc::CodeGenerator};
use miette::NamedSource;
use pallas::ledger::primitives::babbage as cardano;
use pallas_traverse::ComputeHash;
use serde::{
    self,
    ser::{Serialize, SerializeStruct, Serializer},
};
use std::{
    collections::HashMap,
    fmt::{self, Display},
};
use uplc::ast::{NamedDeBruijn, Program};

#[derive(Debug, PartialEq, Clone)]
pub struct Validator {
    pub title: String,
    pub purpose: Purpose,
    pub description: Option<String>,
    pub datum: Option<Annotated<Schema>>,
    pub redeemer: Annotated<Schema>,
    pub program: Program<NamedDeBruijn>,
}

#[derive(Debug, PartialEq, Clone, serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub enum Purpose {
    Spend,
    Mint,
    Withdraw,
    Publish,
}

impl Serialize for Validator {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let cbor = self.program.to_cbor().unwrap();

        let source_code = hex::encode(&cbor);

        let hash = cardano::PlutusV2Script(cbor.into()).compute_hash();

        let fields = 5
            + self.description.as_ref().map(|_| 1).unwrap_or_default()
            + self.datum.as_ref().map(|_| 1).unwrap_or_default();

        let mut s = serializer.serialize_struct("Validator", fields)?;
        s.serialize_field("title", &self.title)?;
        s.serialize_field("purpose", &self.purpose)?;
        s.serialize_field("hash", &hash)?;
        if let Some { .. } = self.description {
            s.serialize_field("description", &self.description)?;
        }
        if let Some { .. } = self.datum {
            s.serialize_field("datum", &self.datum)?;
        }
        s.serialize_field("redeemer", &self.redeemer)?;
        s.serialize_field("compiledCode", &source_code)?;
        s.end()
    }
}

impl Display for Validator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = serde_json::to_string_pretty(self).map_err(|_| fmt::Error)?;
        f.write_str(&s)
    }
}

impl Validator {
    pub fn from_checked_module(
        modules: &CheckedModules,
        generator: &mut CodeGenerator,
        validator: &CheckedModule,
        def: &TypedFunction,
    ) -> Result<Validator, Error> {
        let purpose: Purpose = def.name.clone().into();

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

impl From<String> for Purpose {
    fn from(purpose: String) -> Purpose {
        match &purpose[..] {
            "spend" => Purpose::Spend,
            "mint" => Purpose::Mint,
            "withdraw" => Purpose::Withdraw,
            "publish" => Purpose::Publish,
            unexpected => panic!("Can't turn '{}' into any Purpose", unexpected),
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
    use serde_json::{self, json};
    use std::{collections::HashMap, path::PathBuf};

    // TODO: Possible refactor this out of the module and have it used by `Project`. The idea would
    // be to make this struct below the actual project, and wrap it in another metadata struct
    // which contains all the config and I/O stuff regarding the project.
    struct TestProject {
        package: PackageName,
        id_gen: IdGenerator,
        module_types: HashMap<String, TypeInfo>,
        functions: HashMap<FunctionAccessKey, TypedFunction>,
        data_types: HashMap<DataTypeKey, TypedDataType>,
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
              "hash": "da4a98cee05a17be402b07c414d59bf894c9ebd0487186417121de8f",
              "redeemer": {
                "title": "Data",
                "description": "Any Plutus data."
              },
              "compiledCode": "581d010000210872656465656d657200210363747800533357349445261601"
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
              "hash": "cf2cd3bed32615bfecbd280618c1c1bec2198fc0f72b04f323a8a0d2",
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
              "compiledCode": "58250100002105646174756d00210872656465656d657200210363747800533357349445261601"
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
              "hash": "12065ad2edb75b9e497e50c4f8130b90c9108f8ae0991abc5442e074",
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
              "compiledCode": "589f0100002105646174756d00320105646174756d00210872656465656d65720032010872656465656d657200210363747800533357349445261637326eb8010872656465656d6572000132010b5f5f6c6973745f64617461003201065f5f7461696c00337606ae84010b5f5f6c6973745f646174610002357421065f5f7461696c00013574410b5f5f6c6973745f64617461000137580105646174756d000101"
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
              "hash": "5c470f297728051a920bd9e70e14197c8fb0eaf4413e419827b0ec38",
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
              "compiledCode": "58390100002105646174756d00320105646174756d00210872656465656d657200210363747800533357349445261637580105646174756d000101"
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
                  "hash": "da4a98cee05a17be402b07c414d59bf894c9ebd0487186417121de8f",
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
                  "compiledCode": "581d010000210872656465656d657200210363747800533357349445261601"
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
                  "hash": "da4a98cee05a17be402b07c414d59bf894c9ebd0487186417121de8f",
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
                  "compiledCode": "581d010000210872656465656d657200210363747800533357349445261601"
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
                  "hash": "da4a98cee05a17be402b07c414d59bf894c9ebd0487186417121de8f",
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
                  "compiledCode": "581d010000210872656465656d657200210363747800533357349445261601"
                }
            ),
        );
    }
}
