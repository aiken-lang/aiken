use super::{
    error::Error,
    schema::{Annotated, Schema},
};
use crate::module::{CheckedModule, CheckedModules};
use aiken_lang::{ast::TypedValidator, uplc::CodeGenerator};
use miette::NamedSource;
use serde;
use std::{
    collections::HashMap,
    fmt::{self, Display},
};
use uplc::ast::{DeBruijn, Program, Term};

#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
pub struct Validator<T> {
    pub title: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub purpose: Option<Purpose>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub datum: Option<Annotated<T>>,
    pub redeemer: Annotated<T>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    #[serde(default)]
    pub parameters: Vec<Annotated<T>>,
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
        module: &CheckedModule,
        def: &TypedValidator,
    ) -> Result<Validator<Schema>, Error> {
        let mut args = def.fun.arguments.iter().rev();
        let (_, redeemer, datum) = (args.next(), args.next().unwrap(), args.next());

        let mut arguments = Vec::with_capacity(def.params.len() + def.fun.arguments.len());

        arguments.extend(def.params.clone());
        arguments.extend(def.fun.arguments.clone());

        Ok(Validator {
            title: format!("{}.{}", &module.name, &def.fun.name),
            description: None,
            purpose: None,
            parameters: def
                .params
                .iter()
                .map(|param| {
                    let annotation =
                        Annotated::from_type(modules.into(), &param.tipo, &HashMap::new()).map_err(
                            |error| Error::Schema {
                                error,
                                location: param.location,
                                source_code: NamedSource::new(
                                    module.input_path.display().to_string(),
                                    module.code.clone(),
                                ),
                            },
                        );
                    annotation.map(|mut annotation| {
                        annotation.title = annotation
                            .title
                            .or_else(|| Some(param.arg_name.get_label()));
                        annotation
                    })
                })
                .collect::<Result<_, _>>()?,
            datum: datum
                .map(|datum| {
                    Annotated::from_type(modules.into(), &datum.tipo, &HashMap::new()).map_err(
                        |error| Error::Schema {
                            error,
                            location: datum.location,
                            source_code: NamedSource::new(
                                module.input_path.display().to_string(),
                                module.code.clone(),
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
                        module.input_path.display().to_string(),
                        module.code.clone(),
                    ),
                })?,
            program: generator
                .generate(&def.fun.body, &arguments, true)
                .try_into()
                .unwrap(),
        })
    }
}

impl<T> Validator<T>
where
    T: Clone,
{
    pub fn apply(self, arg: &Term<DeBruijn>) -> Result<Self, Error> {
        match self.parameters.split_first() {
            None => Err(Error::NoParametersToApply),
            Some((_, tail)) => {
                // TODO: Ideally, we should control that the applied term matches its schema.
                Ok(Self {
                    program: self.program.apply_term(arg),
                    parameters: tail.to_vec(),
                    ..self
                })
            }
        }
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
            unexpected => Err(format!("Can't turn '{unexpected}' into any Purpose")),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{module::ParsedModule, PackageName};
    use aiken_lang::{
        self,
        ast::{ModuleKind, Tracing, TypedDataType, TypedFunction},
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

            ParsedModule {
                kind,
                ast,
                code: source_code.to_string(),
                name,
                path: PathBuf::new(),
                extra,
                package: self.package.to_string(),
            }
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
                    Tracing::NoTraces,
                    &mut warnings,
                )
                .expect("Failed to type-check module");

            self.module_types
                .insert(module.name.clone(), ast.type_info.clone());

            let mut checked_module = CheckedModule {
                kind: module.kind,
                extra: module.extra,
                name: module.name,
                code: module.code,
                package: module.package,
                input_path: module.path,
                ast,
            };

            checked_module.attach_doc_and_module_comments();

            checked_module
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

        assert_json_eq!(serde_json::to_value(&validator).unwrap(), json);
    }

    #[test]
    fn validator_mint_basic() {
        assert_validator(
            r#"
            validator mint {
              fn(redeemer: Data, ctx: Data) {
                True
              }
            }
            "#,
            json!({
              "title": "test_module.mint",
              "hash": "afddc16c18e7d8de379fb9aad39b3d1b5afd27603e5ebac818432a72",
              "redeemer": {
                "title": "Data",
                "description": "Any Plutus data."
              },
              "compiledCode": "583b010000323232323232322253330054a22930b180080091129998030010a4c26600a6002600e0046660060066010004002ae695cdaab9f5742ae881"
            }),
        );
    }

    #[test]
    fn validator_mint_parameterized() {
        assert_validator(
            r#"
            validator mint(utxo_ref: Int) {
              fn(redeemer: Data, ctx: Data) {
                True
              }
            }
            "#,
            json!({
              "title": "test_module.mint",
              "hash": "a82df717fd39f5b273c4eb89ae5252e11cc272ac59d815419bf2e4c3",
              "parameters": [{
                "title": "utxo_ref",
                "dataType": "integer"

              }],
              "redeemer": {
                "title": "Data",
                "description": "Any Plutus data."
              },
              "compiledCode": "5840010000323232323232322322253330074a22930b1bad0013001001222533300600214984cc014c004c01c008ccc00c00cc0200080055cd2b9b5573eae855d101"
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

            validator spend {
              fn(datum: State, redeemer: Input, ctx: Data) {
                True
              }
            }
            "#,
            json!({
              "title": "test_module.spend",
              "hash": "e37db487fbd58c45d059bcbf5cd6b1604d3bec16cf888f1395a4ebc4",
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
              "compiledCode": "583b0100003232323232323222253330064a22930b180080091129998030010a4c26600a6002600e0046660060066010004002ae695cdaab9f5742ae89"
            }),
        );
    }

    #[test]
    fn validator_spend_2tuple() {
        assert_validator(
            r#"
            validator spend {
              fn(datum: (Int, ByteArray), redeemer: String, ctx: Void) {
                True
              }
            }
            "#,
            json!({
              "title": "test_module.spend",
              "hash": "3c6766e7a36df2aa13c0e9e6e071317ed39d05f405771c4f1a81c6cc",
              "datum": {
                "dataType": "list",
                "items": [
                  { "dataType": "integer" },
                  { "dataType": "bytes" }
                ]
              },
              "redeemer": {
                "dataType": "#string"
              },
              "compiledCode": "585501000032323232323232232232253330084a22930b1b99375c002646466ec0c024008c024004c024004dd6000980080091129998030010a4c26600a6002600e0046660060066010004002ae695cdaab9f5742ae881"
            }),
        )
    }

    #[test]
    fn validator_spend_tuples() {
        assert_validator(
            r#"
            validator spend {
              fn(datum: (Int, Int, Int), redeemer: Data, ctx: Void) {
                True
              }
            }
            "#,
            json!({
              "title": "test_module.spend",
              "hash": "f335ce0436fd7df56e727a66ada7298534a27b98f887bc3b7947ee48",
              "datum": {
                "title": "Tuple",
                "dataType": "list",
                "items": [
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
              "compiledCode": "5840010000323232323232322322253330074a22930b1bac0013001001222533300600214984cc014c004c01c008ccc00c00cc0200080055cd2b9b5573eae855d101"
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

            validator withdraw {
              fn(redeemer: Either<ByteArray, Interval<Int>>, ctx: Void) {
                True
              }
            }
            "#,
            json!(
                {
                  "title": "test_module.withdraw",
                  "hash": "afddc16c18e7d8de379fb9aad39b3d1b5afd27603e5ebac818432a72",
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
                  "compiledCode": "583b010000323232323232322253330054a22930b180080091129998030010a4c26600a6002600e0046660060066010004002ae695cdaab9f5742ae881"
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

            validator mint {
              fn(redeemer: Dict<UUID, Int>, ctx: Void) {
                True
              }
            }
            "#,
            json!(
                {
                  "title": "test_module.mint",
                  "hash": "afddc16c18e7d8de379fb9aad39b3d1b5afd27603e5ebac818432a72",
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
                  "compiledCode": "583b010000323232323232322253330054a22930b180080091129998030010a4c26600a6002600e0046660060066010004002ae695cdaab9f5742ae881"
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

            validator mint {
              fn(redeemer: Dict<UUID, Int>, ctx: Void) {
                True
              }
            }
            "#,
            json!(
                {
                  "title": "test_module.mint",
                  "hash": "afddc16c18e7d8de379fb9aad39b3d1b5afd27603e5ebac818432a72",
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
                  "compiledCode": "583b010000323232323232322253330054a22930b180080091129998030010a4c26600a6002600e0046660060066010004002ae695cdaab9f5742ae881"
                }
            ),
        );
    }
}
