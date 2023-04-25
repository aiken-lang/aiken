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
              "compiledCode": "5902aa01000032323232323232323232323232322223232533300c4a22930b1980619299980619b87480000044c8c8c8c8c8c94ccc05cc0640084cc04cc94ccc04ccdc3a400000226464a66603460380042930a9980ba481334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e2065787065637465640016375a6034002602200c2a6602a9212b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e7400163011005330133300c003232498dd7000a4c2a660289201334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e2065787065637465640016375c602e002602e0046eb0c054004c054008c04c004c02801454cc0392412b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e740016300a0040043300b32533300b3370e90000008a99980818048018a4c2a6601a92011d4578706563746564206e6f206669656c647320666f7220436f6e73747200161533300b3370e90010008a99980818048018a4c2a6601a92011d4578706563746564206e6f206669656c647320666f7220436f6e73747200161533300b3370e90020008a99980818048018a4c2a6601a92011d4578706563746564206e6f206669656c647320666f7220436f6e7374720016153300d4912b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e74001630090020023001001222533300d00214984cc024c004c038008ccc00c00cc03c008004cc0040052000222233330073370e00200601a4666600a00a66e000112002300f0010020022300737540024600a6ea80055cd2b9b5738aae7555cf2ab9f5742ae89",
              "hash": "bef7fc82378088fbd29dda8c16b018424b5d73bfed7aaf591c7fbfc1",
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
              "compiledCode": "590119010000323232323232323232323232223253330084a22930b1980419299980419b87480000044c8c94ccc03cc0440084cc02ccc0180048c8c926375a60200046eb8c03800526153300c491334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e20657870656374656400163756601e00260186ea800c54cc0292412b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e740016300a37540040046002002444a66601600429309980398009806001199801801980680100099800800a40004444666600a66e1c00400c02c8cccc014014cdc000224004601a002004004ae695cdab9c5573aaae7955cfaba15745",
              "hash": "7404377d39dac5f1daa00e60bb22397d81944d036a2466e3d51c10ba",
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
              "compiledCode": "585d010000323232323232323232223253330064a22930b1919803998020009191924c6eb4c02c008dd718048008009bab0023001001222533300800214984cc014c004c024008ccc00c00cc0280080055cd2b9b5573aaae7955cfaba15745",
              "hash": "0d852f5a9ad295b4067d434fcfede80a56661e687e07424fad11c281",
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
              "compiledCode": "5901d601000032323232323232323232323232223253330094a22930b19804980180100118008009119299980399b87480000044c8c94ccc038c040008526153300b4901334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e2065787065637465640016375a601c002600c0042a66600e66e1d20020011323232325333010301200213300c330070070033300c3300700700149854cc0352401334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e206578706563746564001630100013010002300e0013006002153330073370e900200089919191929998081809001099806198038038019980619803803800a4c2a6601a9201334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e206578706563746564001630100013010002300e001300600215330094912b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e740016300937540026600200290001111199980319b8700100300c233330050053370000890011807000801001118031baa0015734ae6d5ce2ab9d5573caae7d5d0aba21",
              "hash": "c312bfeaf481c336f851eaf80acb056c0a26175e558b094a60de4cb3",
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
              "compiledCode": "590373010000323232323232323232323232323232323232222323253330104a22930b1980819299980819b87480000044c8c94ccc05cc0640084cc04cc0200052615330144901334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e20657870656374656400163017001300e005153330103370e9001000899191919299980c980d801099191980b9980700091980c1808800a4c931bac3019002375c602e0022a6602c9201334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e20657870656374656400163232337606036004603600260360026eb0c064004c064008dd6980b80098070028a9980924812b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e740016300e0040043300f300800200230010012232533300c3370e9000000899191919299980a980b80109980899803803800a4c2a66024921334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e2065787065637465640016301500130150023370e900118081baa3013001300a0021533300c3370e90010008a99980898050010a4c2a6601c9211d4578706563746564206e6f206669656c647320666f7220436f6e7374720016153300e4912b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e740016300a0013001001222533300f00214984cc02cc004c040008ccc00c00cc044008004c00400488c94ccc020cdc3a4000002264646464a666022602600426601a6600e00e002930a998072481334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e206578706563746564001630110013011002375a601e002600c0042a66601066e1d20020011533300d3006002149854cc02924011d4578706563746564206e6f206669656c647320666f7220436f6e7374720016153300a4912b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e740016300600133001001480008888cccc01ccdc38008018069199980280299b8000448008c03c0040080088c01cdd5000918029baa0015734ae6d5ce2ab9d5573caae7d5d0aba21",
              "hash": "39f12c7e94363986531f5661c912d91f99ecdd50955e1d6d049a1489",
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
