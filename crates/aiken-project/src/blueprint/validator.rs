use super::{
    definitions::Definitions,
    error::Error,
    parameter::Parameter,
    schema::{Annotated, Schema},
};
use crate::module::{CheckedModule, CheckedModules};

use aiken_lang::{
    ast::{TypedArg, TypedFunction, TypedValidator},
    gen_uplc::CodeGenerator,
};
use miette::NamedSource;
use serde;
use uplc::ast::{DeBruijn, Program, Term};

#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
pub struct Validator {
    pub title: String,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub datum: Option<Parameter>,

    pub redeemer: Parameter,

    #[serde(skip_serializing_if = "Vec::is_empty")]
    #[serde(default)]
    pub parameters: Vec<Parameter>,

    #[serde(flatten)]
    pub program: Program<DeBruijn>,

    #[serde(skip_serializing_if = "Definitions::is_empty")]
    #[serde(default)]
    pub definitions: Definitions<Annotated<Schema>>,
}

impl Validator {
    pub fn from_checked_module(
        modules: &CheckedModules,
        generator: &mut CodeGenerator,
        module: &CheckedModule,
        def: &TypedValidator,
    ) -> Vec<Result<Validator, Error>> {
        let program = generator.generate(def).try_into().unwrap();

        let is_multi_validator = def.other_fun.is_some();

        let mut validators = vec![Validator::create_validator_blueprint(
            modules,
            module,
            &program,
            &def.params,
            &def.fun,
            is_multi_validator,
        )];

        if let Some(ref other_func) = def.other_fun {
            validators.push(Validator::create_validator_blueprint(
                modules,
                module,
                &program,
                &def.params,
                other_func,
                is_multi_validator,
            ));
        }

        validators
    }

    fn create_validator_blueprint(
        modules: &CheckedModules,
        module: &CheckedModule,
        program: &Program<DeBruijn>,
        params: &[TypedArg],
        func: &TypedFunction,
        is_multi_validator: bool,
    ) -> Result<Validator, Error> {
        let mut args = func.arguments.iter().rev();
        let (_, redeemer, datum) = (args.next(), args.next().unwrap(), args.next());

        let mut arguments = Vec::with_capacity(params.len() + func.arguments.len());
        arguments.extend(params.to_vec());
        arguments.extend(func.arguments.clone());

        let mut definitions = Definitions::new();

        Ok(Validator {
            title: format!("{}.{}", &module.name, &func.name),
            description: None,
            parameters: params
                .iter()
                .map(|param| {
                    Annotated::from_type(modules.into(), &param.tipo, &mut definitions)
                        .map(|schema| Parameter {
                            title: Some(param.arg_name.get_label()),
                            schema,
                        })
                        .map_err(|error| Error::Schema {
                            error,
                            location: param.location,
                            source_code: NamedSource::new(
                                module.input_path.display().to_string(),
                                module.code.clone(),
                            ),
                        })
                })
                .collect::<Result<_, _>>()?,
            datum: datum
                .map(|datum| {
                    Annotated::from_type(modules.into(), &datum.tipo, &mut definitions).map_err(
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
                .transpose()?
                .map(|schema| Parameter {
                    title: datum.map(|datum| datum.arg_name.get_label()),
                    schema,
                }),
            redeemer: Annotated::from_type(modules.into(), &redeemer.tipo, &mut definitions)
                .map_err(|error| Error::Schema {
                    error,
                    location: redeemer.location,
                    source_code: NamedSource::new(
                        module.input_path.display().to_string(),
                        module.code.clone(),
                    ),
                })
                .map(|schema| Parameter {
                    title: Some(redeemer.arg_name.get_label()),
                    schema: match datum {
                        Some(..) if is_multi_validator => Annotated::as_wrapped_redeemer(
                            &mut definitions,
                            schema,
                            redeemer.tipo.clone(),
                        ),
                        _ => schema,
                    },
                })?,
            program: program.clone(),
            definitions,
        })
    }
}

impl Validator {
    pub fn apply(
        self,
        definitions: &Definitions<Annotated<Schema>>,
        arg: &Term<DeBruijn>,
    ) -> Result<Self, Error> {
        match self.parameters.split_first() {
            None => Err(Error::NoParametersToApply),
            Some((head, tail)) => {
                head.validate(definitions, arg)?;
                Ok(Self {
                    program: self.program.apply_term(arg),
                    parameters: tail.to_vec(),
                    ..self
                })
            }
        }
    }
}

#[cfg(test)]
mod test {
    use assert_json_diff::assert_json_eq;

    use serde_json::{self, json};
    use std::collections::HashMap;

    use aiken_lang::{self, builtins};
    use uplc::ast::{self as uplc_ast};

    use crate::tests::TestProject;

    use super::{
        super::{
            definitions::{Definitions, Reference},
            error::Error,
            schema::{Annotated, Constructor, Data, Declaration, Items, Schema},
        },
        *,
    };

    fn assert_validator(source_code: &str, expected: serde_json::Value) {
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

        let validators = Validator::from_checked_module(&modules, &mut generator, validator, def);

        if validators.len() > 1 {
            panic!("Multi-validator given to test bench. Don't do that.")
        }

        let validator = validators
            .get(0)
            .unwrap()
            .as_ref()
            .expect("Failed to create validator blueprint");

        println!("{}", serde_json::to_string_pretty(validator).unwrap());

        assert_json_eq!(serde_json::to_value(validator).unwrap(), expected);
    }

    fn fixture_definitions() -> Definitions<Annotated<Schema>> {
        let mut definitions = Definitions::new();

        // #/definitions/Int
        //
        // {
        //   "dataType": "integer"
        // }
        definitions
            .register::<_, Error>(&builtins::int(), &HashMap::new(), |_| {
                Ok(Schema::Data(Data::Integer).into())
            })
            .unwrap();

        // #/definitions/ByteArray
        //
        // {
        //   "dataType": "bytes"
        // }
        definitions
            .register::<_, Error>(&builtins::byte_array(), &HashMap::new(), |_| {
                Ok(Schema::Data(Data::Bytes).into())
            })
            .unwrap();

        // #/definitions/Bool
        //
        // {
        //   "anyOf": [
        //      {
        //          "dataType": "constructor",
        //          "index": 0,
        //          "fields": []
        //      },
        //      {
        //          "dataType": "constructor",
        //          "index": 1,
        //          "fields": []
        //      },
        //   ]
        // }
        definitions.insert(
            &Reference::new("Bool"),
            Schema::Data(Data::AnyOf(vec![
                // False
                Constructor {
                    index: 0,
                    fields: vec![],
                }
                .into(),
                // True
                Constructor {
                    index: 1,
                    fields: vec![],
                }
                .into(),
            ]))
            .into(),
        );

        definitions
    }

    #[test]
    fn mint_basic() {
        assert_validator(
            r#"
            validator {
              fn mint(redeemer: Data, ctx: Data) {
                True
              }
            }
            "#,
            json!({
              "title": "test_module.mint",
              "redeemer": {
                "title": "redeemer",
                "schema": {
                  "$ref": "#/definitions/Data"
                }
              },
              "compiledCode": "4f010000322253330034a22930b2b9a1",
              "hash": "69eb6e27b7098c51cef74d8929553456e0ff6748c50a08c0daae7986",
              "definitions": {
                "Data": {
                  "title": "Data",
                  "description": "Any Plutus data."
                }
              }
            }),
        );
    }

    #[test]
    fn mint_parameterized() {
        assert_validator(
            r#"
            validator(utxo_ref: Int) {
              fn mint(redeemer: Data, ctx: Data) {
                True
              }
            }
            "#,
            json!({
              "title": "test_module.mint",
              "redeemer": {
                "title": "redeemer",
                "schema": {
                  "$ref": "#/definitions/Data"
                }
              },
              "parameters": [
                {
                  "title": "utxo_ref",
                  "schema": {
                    "$ref": "#/definitions/Int"
                  }
                }
              ],
              "compiledCode": "54010000322322253330054a22930b1bad00157341",
              "hash": "0e31a2048fe4751926c4a1e5fd93c9c2ecc8035777884c15db157d11",
              "definitions": {
                "Data": {
                  "title": "Data",
                  "description": "Any Plutus data."
                },
                "Int": {
                  "dataType": "integer"
                }
              }
            }),
        );
    }

    #[test]
    fn simplified_hydra() {
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

            validator {
              fn simplified_hydra(datum: State, redeemer: Input, ctx: Data) {
                True
              }
            }
            "#,
            json!({
              "title": "test_module.simplified_hydra",
              "datum": {
                "title": "datum",
                "schema": {
                  "$ref": "#/definitions/test_module~1State"
                }
              },
              "redeemer": {
                "title": "redeemer",
                "schema": {
                  "$ref": "#/definitions/test_module~1Input"
                }
              },
              "compiledCode": "5902ad0100003232323232323232323232322223232533300a4a22930b1980519919119299980699b87480000044c8c8c8c8c8c94ccc060c0680084cc050c94ccc050cdc3a400000226464a666036603a0042930a9980c2481334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e2065787065637465640016375a6036002602400c2a6602c9212b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e74001630120053301433009003232498dd7000a4c2a6602a9201334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e2065787065637465640016375c603000260300046eb0c058004c058008c050004c02c00854cc03d2412b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e740016300b0013001001222533301100214984cc034c004c048008ccc00c00cc04c008004010010cc024c94ccc024cdc3a40000022a66601c600e0062930a99805a4811d4578706563746564206e6f206669656c647320666f7220436f6e7374720016153330093370e90010008a99980718038018a4c2a6601692011d4578706563746564206e6f206669656c647320666f7220436f6e7374720016153330093370e90020008a99980718038018a4c2a6601692011d4578706563746564206e6f206669656c647320666f7220436f6e7374720016153300b4912b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e740016300700200233001001480008888cccc01ccdc38008018069199980280299b8000448008c03c0040080088c01cdd5000918029baa0015734ae6d5ce2ab9d5573caae7d5d0aba201",
              "hash": "dfd982261c46e048a8d2a7ffb45258d2e98ab88b563709a47d69e37d",
              "definitions": {
                "ByteArray": {
                  "dataType": "bytes"
                },
                "Int": {
                  "dataType": "integer"
                },
                "List$ByteArray": {
                  "dataType": "list",
                  "items": {
                    "$ref": "#/definitions/ByteArray"
                  }
                },
                "test_module/ContestationPeriod": {
                  "title": "ContestationPeriod",
                  "description": "Whatever",
                  "anyOf": [
                    {
                      "title": "ContestationPeriod",
                      "description": "A positive, non-zero number of seconds.",
                      "dataType": "constructor",
                      "index": 0,
                      "fields": [
                        {
                          "$ref": "#/definitions/Int"
                        }
                      ]
                    }
                  ]
                },
                "test_module/Input": {
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
                "test_module/State": {
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
                          "$ref": "#/definitions/test_module~1ContestationPeriod"
                        },
                        {
                          "title": "parties",
                          "description": "List of public key hashes of all participants",
                          "$ref": "#/definitions/List$ByteArray"
                        },
                        {
                          "title": "utxoHash",
                          "$ref": "#/definitions/ByteArray"
                        }
                      ]
                    }
                  ]
                }
              }
            }),
        );
    }

    #[test]
    fn tuples() {
        assert_validator(
            r#"
            validator {
              fn tuples(datum: (Int, ByteArray), redeemer: (Int, Int, Int), ctx: Void) {
                True
              }
            }
            "#,
            json!({
              "title": "test_module.tuples",
              "datum": {
                "title": "datum",
                "schema": {
                  "$ref": "#/definitions/Tuple$Int_ByteArray"
                }
              },
              "redeemer": {
                "title": "redeemer",
                "schema": {
                  "$ref": "#/definitions/Tuple$Int_Int_Int"
                }
              },
              "compiledCode": "58ab01000032323232323232222323253330064a22930b1919190019bae300a002375a6010002646466ec0c030008c030004c030004dd60021919191919192999807180800108030a99805a481334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e2065787065637465640016375a601c002601c0046eb4c030004c030008dd698050009bac0025734ae7155ceaab9e5573eae855d101",
              "hash": "f8258ac5409f8c0a921f99f4427e3f9362e0ed0146ff71914f80fc4e",
              "definitions": {
                "ByteArray": {
                  "dataType": "bytes"
                },
                "Int": {
                  "dataType": "integer"
                },
                "Tuple$Int_ByteArray": {
                  "title": "Tuple",
                  "dataType": "list",
                  "items": [
                    {
                      "$ref": "#/definitions/Int"
                    },
                    {
                      "$ref": "#/definitions/ByteArray"
                    }
                  ]
                },
                "Tuple$Int_Int_Int": {
                  "title": "Tuple",
                  "dataType": "list",
                  "items": [
                    {
                      "$ref": "#/definitions/Int"
                    },
                    {
                      "$ref": "#/definitions/Int"
                    },
                    {
                      "$ref": "#/definitions/Int"
                    }
                  ]
                }
              }
            }),
        )
    }

    #[test]
    fn generics() {
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

            validator {
              fn generics(redeemer: Either<ByteArray, Interval<Int>>, ctx: Void) {
                True
              }
            }
            "#,
            json!({
              "title": "test_module.generics",
              "redeemer": {
                "title": "redeemer",
                "schema": {
                  "$ref": "#/definitions/test_module~1Either$ByteArray_test_module~1Interval$Int"
                }
              },
              "compiledCode": "590213010000323232323232323232323232223253330084a22930b1980419299980419b87480000044c8c94ccc03cc044008526153300c491334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e2065787065637465640016375c601e002600c0062a66601066e1d200200113232533300f301100213300b32533300b3370e9000000899192999809180a0010a4c2a6601e9201334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e2065787065637465640016375a602400260120042a66601666e1d2002001153330103009002149854cc03524011d4578706563746564206e6f206669656c647320666f7220436f6e7374720016153300d4912b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e740016300900149854cc031241334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e2065787065637465640016300f0013006003153300a4912b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e740016300600200233001001480008888cccc01ccdc38008018069199980280299b8000448008c03c0040080088c01cdd5000918029baa0015734ae6d5ce2ab9d5573caae7d5d0aba201",
              "hash": "bfca0d21daa5694b321ee99bb82a74976f998a691f0a08b1ac2bac7d",
              "definitions": {
                "ByteArray": {
                  "dataType": "bytes"
                },
                "Int": {
                  "dataType": "integer"
                },
                "test_module/Either$ByteArray_test_module/Interval$Int": {
                  "title": "Either",
                  "anyOf": [
                    {
                      "title": "Left",
                      "dataType": "constructor",
                      "index": 0,
                      "fields": [
                        {
                          "$ref": "#/definitions/ByteArray"
                        }
                      ]
                    },
                    {
                      "title": "Right",
                      "dataType": "constructor",
                      "index": 1,
                      "fields": [
                        {
                          "$ref": "#/definitions/test_module~1Interval$Int"
                        }
                      ]
                    }
                  ]
                },
                "test_module/Interval$Int": {
                  "title": "Interval",
                  "anyOf": [
                    {
                      "title": "Finite",
                      "dataType": "constructor",
                      "index": 0,
                      "fields": [
                        {
                          "$ref": "#/definitions/Int"
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
              }
            }),
        )
    }

    #[test]
    fn list_2_tuples_as_map() {
        assert_validator(
            r#"
            type Dict<key, value> {
                inner: List<(ByteArray, value)>
            }

            type UUID { UUID }

            validator {
              fn list_2_tuples_as_map(redeemer: Dict<UUID, Int>, ctx: Void) {
                True
              }
            }
            "#,
            json!({
              "title": "test_module.list_2_tuples_as_map",
              "redeemer": {
                "title": "redeemer",
                "schema": {
                  "$ref": "#/definitions/test_module~1Dict$test_module~1UUID_Int"
                }
              },
              "compiledCode": "59011c01000032323232323232323232223253330064a22930b1980319919119299980499b87480000044c8c94ccc040c0480084cc030cc0140048c8c926375a60220046eb8c03c00526153300d491334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e206578706563746564001637566020002601a6ea800854cc02d2412b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e740016300b37540026002002444a66601a00429309980498009807001199801801980780100080100119800800a40004444666600a66e1c00400c02c8cccc014014cdc000224004601a002004004ae695cdab9c5573aaae7955cfaba157441",
              "hash": "6f4ac35fb024ddddca4ea209d49943096c735c1b2348d63983c5bf08",
              "definitions": {
                "ByteArray": {
                  "dataType": "bytes"
                },
                "Int": {
                  "dataType": "integer"
                },
                "List$Tuple$ByteArray_Int": {
                  "dataType": "map",
                  "keys": {
                    "$ref": "#/definitions/ByteArray"
                  },
                  "values": {
                    "$ref": "#/definitions/Int"
                  }
                },
                "test_module/Dict$test_module/UUID_Int": {
                  "title": "Dict",
                  "anyOf": [
                    {
                      "title": "Dict",
                      "dataType": "constructor",
                      "index": 0,
                      "fields": [
                        {
                          "title": "inner",
                          "$ref": "#/definitions/List$Tuple$ByteArray_Int"
                        }
                      ]
                    }
                  ]
                }
              }
            }),
        );
    }

    #[test]
    fn opaque_singleton_variants() {
        assert_validator(
            r#"
            pub opaque type Dict<key, value> {
                inner: List<(ByteArray, value)>
            }

            type UUID { UUID }

            validator {
              fn opaque_singleton_variants(redeemer: Dict<UUID, Int>, ctx: Void) {
                True
              }
            }
            "#,
            json!({
              "title": "test_module.opaque_singleton_variants",
              "redeemer": {
                "title": "redeemer",
                "schema": {
                  "$ref": "#/definitions/test_module~1Dict$test_module~1UUID_Int"
                }
              },
              "compiledCode": "585b01000032323232323232223253330044a22930b19198029999180080091129998058010a4c26601060026018004666006006601a00400200246464931bad3009002375c600e0020026eac0095cd2b9b5573aaae7955cfaba157441",
              "hash": "96974ac55a06ba32cec8ba5d1f603a1c4e82a4ec9ef13b7bb838d7d9",
              "definitions": {
                "ByteArray": {
                  "dataType": "bytes"
                },
                "Int": {
                  "dataType": "integer"
                },
                "test_module/Dict$test_module/UUID_Int": {
                  "title": "Dict",
                  "dataType": "map",
                  "keys": {
                    "$ref": "#/definitions/ByteArray"
                  },
                  "values": {
                    "$ref": "#/definitions/Int"
                  }
                }
              }
            }),
        );
    }

    #[test]
    fn nested_data() {
        assert_validator(
            r#"
            pub type Foo {
                foo: Data
            }

            validator {
              fn nested_data(datum: Foo, redeemer: Int, ctx: Void) {
                True
              }
            }
            "#,
            json!({
              "title": "test_module.nested_data",
              "datum": {
                "title": "datum",
                "schema": {
                  "$ref": "#/definitions/test_module~1Foo"
                }
              },
              "redeemer": {
                "title": "redeemer",
                "schema": {
                  "$ref": "#/definitions/Int"
                }
              },
              "compiledCode": "58e801000032323232323232323232222323253330084a22930b1980419299980419b87480000044c8c94ccc03cc044008526153300c4901334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e2065787065637465640016300f001300c375400a2a660149212b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e740016300a37540080086eb4008cc0040052000222233330053370e0020060164666600a00a66e000112002300d0010020025734ae6d5ce2ab9d5573caae7d5d0aba21",
              "hash": "5efeb108e67807af47a66b3c3537e7210ac7c1d6892e3ff512706163",
              "definitions": {
                "Data": {
                  "title": "Data",
                  "description": "Any Plutus data."
                },
                "Int": {
                  "dataType": "integer"
                },
                "test_module/Foo": {
                  "title": "Foo",
                  "anyOf": [
                    {
                      "title": "Foo",
                      "dataType": "constructor",
                      "index": 0,
                      "fields": [
                        {
                          "title": "foo",
                          "$ref": "#/definitions/Data"
                        }
                      ]
                    }
                  ]
                }
              }
            }),
        );
    }

    #[test]
    fn recursive_types() {
        assert_validator(
            r#"
            pub type Expr {
              Val(Int)
              Sum(Expr, Expr)
              Mul(Expr, Expr)
            }

            validator {
              fn recursive_types(redeemer: Expr, ctx: Void) {
                True
              }
            }
            "#,
            json!({
              "title": "test_module.recursive_types",
              "redeemer": {
                "title": "redeemer",
                "schema": {
                  "$ref": "#/definitions/test_module~1Expr"
                }
              },
              "compiledCode": "5901d30100003232323232323232323232223253330074a22930b1980399918008009119299980499b87480000044c8c94ccc040c048008526153300d491334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e2065787065637465640016375a602000260100042a66601266e1d20020011323232325333012301400213300e330070070033300e3300700700149854cc03d2401334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e20657870656374656400163012001301200230100013008002153330093370e90020008991919192999809180a001099807198038038019980719803803800a4c2a6601e9201334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e20657870656374656400163012001301200230100013008002153300b4912b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e740016300b37540020040046600200290001111199980319b8700100300c233330050053370000890011807000801001118031baa0015734ae6d5ce2ab9d5573caae7d5d0aba21",
              "hash": "2fffd5b9f5a372ec180ebec76cecd630c7bfdf680a5ffd7353b67c18",
              "definitions": {
                "Int": {
                  "dataType": "integer"
                },
                "test_module/Expr": {
                  "title": "Expr",
                  "anyOf": [
                    {
                      "title": "Val",
                      "dataType": "constructor",
                      "index": 0,
                      "fields": [
                        {
                          "$ref": "#/definitions/Int"
                        }
                      ]
                    },
                    {
                      "title": "Sum",
                      "dataType": "constructor",
                      "index": 1,
                      "fields": [
                        {
                          "$ref": "#/definitions/test_module~1Expr"
                        },
                        {
                          "$ref": "#/definitions/test_module~1Expr"
                        }
                      ]
                    },
                    {
                      "title": "Mul",
                      "dataType": "constructor",
                      "index": 2,
                      "fields": [
                        {
                          "$ref": "#/definitions/test_module~1Expr"
                        },
                        {
                          "$ref": "#/definitions/test_module~1Expr"
                        }
                      ]
                    }
                  ]
                }
              }
            }),
        )
    }

    #[test]
    fn recursive_generic_types() {
        assert_validator(
            r#"
            pub type LinkedList<a> {
              Cons(a, LinkedList<a>)
              Nil
            }

            pub type Foo {
                Foo {
                    foo: LinkedList<Bool>,
                }
                Bar {
                    bar: Int,
                    baz: (ByteArray, List<LinkedList<Int>>)
                }
            }

            validator {
              fn recursive_generic_types(datum: Foo, redeemer: LinkedList<Int>, ctx: Void) {
                True
              }
            }
            "#,
            json!({
              "title": "test_module.recursive_generic_types",
              "datum": {
                "title": "datum",
                "schema": {
                  "$ref": "#/definitions/test_module~1Foo"
                }
              },
              "redeemer": {
                "title": "redeemer",
                "schema": {
                  "$ref": "#/definitions/test_module~1LinkedList$Int"
                }
              },
              "compiledCode": "59037601000032323232323232323232323222232323232533300c4a22930b19806199191919119299980899b87480000044c8c94ccc060c0680084cc050c01c0052615330154901334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e20657870656374656400163018001300f002153330113370e9001000899191919299980d180e001099191980c1980480091980c9807800a4c931bac301a002375c60300022a6602e9201334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e20657870656374656400163232337606038004603800260380026eb0c068004c068008dd6980c00098078010a99809a4812b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e740016300f0013001001222533301500214984cc044c004c058008ccc00c00cc05c008004c00400488c94ccc038cdc3a4000002264646464a66602e60320042660266600e00e002930a9980a2481334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e2065787065637465640016301700130170023370e900118091baa3015001300c0021533300e3370e90010008a99980998060010a4c2a660209211d4578706563746564206e6f206669656c647320666f7220436f6e737472001615330104912b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e740016300c0010060063300b300100400430010012232533300b3370e9000000899191919299980a180b00109980819803803800a4c2a660229201334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e206578706563746564001630140013014002375a602400260120042a66601666e1d2002001153330103009002149854cc03524011d4578706563746564206e6f206669656c647320666f7220436f6e7374720016153300d4912b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e740016300900133001001480008888cccc01ccdc38008018069199980280299b8000448008c03c0040080088c01cdd5000918029baa0015734ae6d5ce2ab9d5573caae7d5d0aba21",
              "hash": "2e61d226dd9f4d5cafda093e8156f7f574ca95d10bee886581dd4bc2",
              "definitions": {
                "Bool": {
                  "title": "Bool",
                  "anyOf": [
                    {
                      "title": "False",
                      "dataType": "constructor",
                      "index": 0,
                      "fields": []
                    },
                    {
                      "title": "True",
                      "dataType": "constructor",
                      "index": 1,
                      "fields": []
                    }
                  ]
                },
                "ByteArray": {
                  "dataType": "bytes"
                },
                "Int": {
                  "dataType": "integer"
                },
                "List$test_module/LinkedList$Int": {
                  "dataType": "list",
                  "items": {
                    "$ref": "#/definitions/test_module~1LinkedList$Int"
                  }
                },
                "Tuple$ByteArray_List$test_module/LinkedList$Int": {
                  "title": "Tuple",
                  "dataType": "list",
                  "items": [
                    {
                      "$ref": "#/definitions/ByteArray"
                    },
                    {
                      "$ref": "#/definitions/List$test_module~1LinkedList$Int"
                    }
                  ]
                },
                "test_module/Foo": {
                  "title": "Foo",
                  "anyOf": [
                    {
                      "title": "Foo",
                      "dataType": "constructor",
                      "index": 0,
                      "fields": [
                        {
                          "title": "foo",
                          "$ref": "#/definitions/test_module~1LinkedList$Bool"
                        }
                      ]
                    },
                    {
                      "title": "Bar",
                      "dataType": "constructor",
                      "index": 1,
                      "fields": [
                        {
                          "title": "bar",
                          "$ref": "#/definitions/Int"
                        },
                        {
                          "title": "baz",
                          "$ref": "#/definitions/Tuple$ByteArray_List$test_module~1LinkedList$Int"
                        }
                      ]
                    }
                  ]
                },
                "test_module/LinkedList$Bool": {
                  "title": "LinkedList",
                  "anyOf": [
                    {
                      "title": "Cons",
                      "dataType": "constructor",
                      "index": 0,
                      "fields": [
                        {
                          "$ref": "#/definitions/Bool"
                        },
                        {
                          "$ref": "#/definitions/test_module~1LinkedList$Bool"
                        }
                      ]
                    },
                    {
                      "title": "Nil",
                      "dataType": "constructor",
                      "index": 1,
                      "fields": []
                    }
                  ]
                },
                "test_module/LinkedList$Int": {
                  "title": "LinkedList",
                  "anyOf": [
                    {
                      "title": "Cons",
                      "dataType": "constructor",
                      "index": 0,
                      "fields": [
                        {
                          "$ref": "#/definitions/Int"
                        },
                        {
                          "$ref": "#/definitions/test_module~1LinkedList$Int"
                        }
                      ]
                    },
                    {
                      "title": "Nil",
                      "dataType": "constructor",
                      "index": 1,
                      "fields": []
                    }
                  ]
                }
              }
            }),
        )
    }

    #[test]
    fn validate_arguments_integer() {
        let definitions = fixture_definitions();

        let term = Term::data(uplc_ast::Data::integer(42.into()));

        let param = Parameter {
            title: None,
            schema: Reference::new("Int"),
        };

        assert!(matches!(param.validate(&definitions, &term), Ok { .. }))
    }

    #[test]
    fn validate_arguments_bytestring() {
        let definitions = fixture_definitions();

        let term = Term::data(uplc_ast::Data::bytestring(vec![102, 111, 111]));

        let param = Parameter {
            title: None,
            schema: Reference::new("ByteArray"),
        };

        assert!(matches!(param.validate(&definitions, &term), Ok { .. }))
    }

    #[test]
    fn validate_arguments_list_inline() {
        let schema = Reference::new("List$Int");

        // #/definitions/List$Int
        //
        // {
        //   "dataType": "list",
        //   "items": { "dataType": "integer" }
        // }
        let mut definitions = fixture_definitions();
        definitions.insert(
            &schema,
            Schema::Data(Data::List(Items::One(Declaration::Inline(Box::new(
                Data::Integer,
            )))))
            .into(),
        );

        let term = Term::data(uplc_ast::Data::list(vec![
            uplc_ast::Data::integer(42.into()),
            uplc_ast::Data::integer(14.into()),
        ]));

        let param: Parameter = schema.into();

        assert!(matches!(param.validate(&definitions, &term), Ok { .. }))
    }

    #[test]
    fn validate_arguments_list_ref() {
        let schema = Reference::new("List$ByteArray");

        // #/definitions/List$ByteArray
        //
        // {
        //   "dataType": "list",
        //   "items": { "$ref": "#/definitions/ByteArray" }
        // }
        let mut definitions = fixture_definitions();
        definitions.insert(
            &schema,
            Schema::Data(Data::List(Items::One(Declaration::Referenced(
                Reference::new("ByteArray"),
            ))))
            .into(),
        );

        let term = Term::data(uplc_ast::Data::list(vec![uplc_ast::Data::bytestring(
            vec![102, 111, 111],
        )]));

        let param: Parameter = schema.into();

        assert!(matches!(param.validate(&definitions, &term), Ok { .. }))
    }

    #[test]
    fn validate_arguments_tuple() {
        let schema = Reference::new("Tuple$Int_ByteArray");

        // #/definitions/Tuple$Int_ByteArray
        //
        // {
        //   "dataType": "list",
        //   "items": [
        //     { "$ref": "#/definitions/Int" }
        //     { "$ref": "#/definitions/ByteArray" }
        //   ]
        // }
        let mut definitions = fixture_definitions();
        definitions.insert(
            &schema,
            Schema::Data(Data::List(Items::Many(vec![
                Declaration::Referenced(Reference::new("Int")),
                Declaration::Referenced(Reference::new("ByteArray")),
            ])))
            .into(),
        );

        let term = Term::data(uplc_ast::Data::list(vec![
            uplc_ast::Data::integer(42.into()),
            uplc_ast::Data::bytestring(vec![102, 111, 111]),
        ]));

        let param: Parameter = schema.into();

        assert!(matches!(param.validate(&definitions, &term), Ok { .. }))
    }

    #[test]
    fn validate_arguments_dict() {
        let schema = Reference::new("Dict$ByteArray_Int");

        // #/definitions/Dict$Int_ByteArray
        //
        // {
        //   "dataType": "map",
        //   "keys": { "dataType": "bytes" },
        //   "values": { "dataType": "integer" }
        // }
        let mut definitions = fixture_definitions();
        definitions.insert(
            &Reference::new("Dict$ByteArray_Int"),
            Schema::Data(Data::Map(
                Declaration::Inline(Box::new(Data::Bytes)),
                Declaration::Inline(Box::new(Data::Integer)),
            ))
            .into(),
        );

        let term = Term::data(uplc_ast::Data::map(vec![(
            uplc_ast::Data::bytestring(vec![102, 111, 111]),
            uplc_ast::Data::integer(42.into()),
        )]));

        let param: Parameter = schema.into();

        assert!(matches!(param.validate(&definitions, &term), Ok { .. }))
    }

    #[test]
    fn validate_arguments_constr_nullary() {
        let schema = Reference::new("Bool");

        let definitions = fixture_definitions();

        let term = Term::data(uplc_ast::Data::constr(1, vec![]));

        let param: Parameter = schema.into();

        assert!(matches!(param.validate(&definitions, &term), Ok { .. }))
    }

    #[test]
    fn validate_arguments_constr_n_ary() {
        let schema = Reference::new("Foo");

        // #/definitions/Foo
        //
        // {
        //   "anyOf": [
        //      {
        //          "dataType": "constructor",
        //          "index": 0,
        //          "fields": [{
        //              "$ref": "#/definitions/Bool
        //          }]
        //      },
        //   ]
        // }
        let mut definitions = fixture_definitions();
        definitions.insert(
            &schema,
            Schema::Data(Data::AnyOf(vec![Constructor {
                index: 0,
                fields: vec![Declaration::Referenced(Reference::new("Bool")).into()],
            }
            .into()]))
            .into(),
        );

        let term = Term::data(uplc_ast::Data::constr(
            0,
            vec![uplc_ast::Data::constr(0, vec![])],
        ));

        let param: Parameter = schema.into();

        assert!(matches!(param.validate(&definitions, &term), Ok { .. }))
    }

    #[test]
    fn validate_arguments_constr_recursive() {
        let schema = Reference::new("LinkedList$Int");

        // #/definitions/LinkedList$Int
        //
        // {
        //   "anyOf": [
        //      {
        //          "dataType": "constructor",
        //          "index": 0,
        //          "fields": []
        //      },
        //      {
        //          "dataType": "constructor",
        //          "index": 1,
        //          "fields": [{
        //              "$ref": "#/definitions/Int
        //              "$ref": "#/definitions/LinkedList$Int
        //          }]
        //      },
        //   ]
        // }
        let mut definitions = fixture_definitions();
        definitions.insert(
            &schema,
            Schema::Data(Data::AnyOf(vec![
                // Empty
                Constructor {
                    index: 0,
                    fields: vec![],
                }
                .into(),
                // Node
                Constructor {
                    index: 1,
                    fields: vec![
                        Declaration::Referenced(Reference::new("Int")).into(),
                        Declaration::Referenced(Reference::new("LinkedList$Int")).into(),
                    ],
                }
                .into(),
            ]))
            .into(),
        );

        let term = Term::data(uplc_ast::Data::constr(
            1,
            vec![
                uplc_ast::Data::integer(14.into()),
                uplc_ast::Data::constr(
                    1,
                    vec![
                        uplc_ast::Data::integer(42.into()),
                        uplc_ast::Data::constr(0, vec![]),
                    ],
                ),
            ],
        ));

        let param: Parameter = schema.into();

        assert!(matches!(param.validate(&definitions, &term), Ok { .. }))
    }
}
