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

        let parameters = params
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
            .collect::<Result<_, _>>()?;

        Ok(Validator {
            title: format!("{}.{}", &module.name, &func.name),
            description: func.doc.clone(),
            parameters,
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
    use uplc::ast as uplc_ast;

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
            true,
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
              "compiledCode": "5902a201000032323232323232323232322223232533300a4a22930b1900299919119299980699b87480000044c8c8c8c8c8c94ccc05cc0640084c8c9263300a004232498dd700099299980a19b87480000044c8c94ccc068c07000852615330174901334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e2065787065637465640016375a6034002602400c2a6602a9212b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e740016301200515330144901334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e2065787065637465640016375c602e002602e0046eb0c054004c054008c04c004c02c00854cc0392412b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e740016300b0013001001222533301000214984c8ccc010010c04c00c008c004c044008010c800cc94ccc024cdc3a40000022a66601a600e0062930a9980524811d4578706563746564206e6f206669656c647320666f7220436f6e7374720016153330093370e90010008a99980698038018a4c2a6601492011d4578706563746564206e6f206669656c647320666f7220436f6e7374720016153330093370e90020008a99980698038018a4c2a6601492011d4578706563746564206e6f206669656c647320666f7220436f6e7374720016153300a4912b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e740016300700233001001480008888cccc01ccdc38008018061199980280299b8000448008c0380040080088c018dd5000918021baa0015734ae7155ceaab9e5573eae855d11",
              "hash": "b57f3d9e610afae77ef6d2662d945b3bb1e1c8698ff55fe8e9287b00",
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
              "compiledCode": "59020c0100003232323232323232323232223253330084a22930b1900199299980419b87480000044c8c94ccc038c040008526153300b491334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e2065787065637465640016375c601c002600c0062a66601066e1d200200113232533300e3010002132498c94ccc02ccdc3a400000226464a66602260260042930a99807249334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e2065787065637465640016375a602200260120042a66601666e1d20020011533300f3009002149854cc03124011d4578706563746564206e6f206669656c647320666f7220436f6e7374720016153300c4912b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e7400163009001153300b4901334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e2065787065637465640016300e001300600315330094912b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e740016300600233001001480008888cccc01ccdc38008018061199980280299b8000448008c0380040080088c018dd5000918021baa0015734ae7155ceaab9e5573eae855d11",
              "hash": "80d2bf8e5785ac1fd753a00c28cc808e1c9f0dac08e42bdb0d2a3142",
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
              "compiledCode": "590115010000323232323232323232223253330064a22930b1900199919119299980499b87480000044c8c94ccc03cc0440084c9263300500123232498dd698080011bae300e001153300c4901334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e20657870656374656400163756601e00260186ea800854cc0292412b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e740016300a37540026002002444a66601800429309919980200218078018011800980680100119800800a40004444666600a66e1c00400c0288cccc014014cdc0002240046018002004004ae695ce2ab9d5573caae7d5d0aba21",
              "hash": "780668561f5650bba680ecc5a1ccee2829df0bbe27d29f9c5c456bbc",
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
              "compiledCode": "5855010000323232323232223253330044a22930b19190011999180080091129998050010a4c264666008008601a0060046002601600400246464931bad3008002375c600c0026eac0095cd2ab9d5573caae7d5d0aba21",
              "hash": "857336762a5637afaacef8f0b536f23763fa05006e5f4f2401e3c7d9",
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
              "compiledCode": "58e4010000323232323232323232222323253330084a22930b1900299299980419b87480000044c8c94ccc038c040008526153300b4901334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e2065787065637465640016300e001300b375400a2a660129212b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e740016300937540086eb4008cc0040052000222233330053370e0020060144666600a00a66e000112002300c0010020025734ae7155ceaab9e5573eae855d101",
              "hash": "b8bce36b335ed232d2204ac8de888fc3bf28bf0bc2b4c4c8116d409f",
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
              "compiledCode": "5901c901000032323232323232323232223253330074a22930b1900199918008009119299980499b87480000044c8c94ccc03cc044008526153300c491334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e2065787065637465640016375a601e00260100042a66601266e1d20020011323232325333011301300213232498cc020020008cc01c01c00c54cc0392401334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e206578706563746564001630110013011002300f0013008002153330093370e9002000899191919299980898098010991924c660100100046600e00e0062a6601c9201334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e206578706563746564001630110013011002300f0013008002153300a4912b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e740016300a37540020046600200290001111199980319b8700100300b233330050053370000890011806800801001118029baa0015734ae7155ceaab9e5573eae855d101",
              "hash": "d89d1c0bdde26ab12979ff50a140f3f1f7a47d50ccf4cf633b3ef3d3",
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
              "compiledCode": "590366010000323232323232323232323222232323232533300c4a22930b19003999191919119299980899b87480000044c8c94ccc05cc0640084c92630070011533014491334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e20657870656374656400163017001300f002153330113370e9001000899191919299980c980d801099191924c660120024649318078009bac3019002375c602e0022a6602c9201334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e20657870656374656400163232337606036004603600260360026eb0c064004c064008dd6980b80098078010a9980924812b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e740016300f0013001001222533301400214984c8ccc010010c05c00c008c004c054008c00400488c94ccc038cdc3a4000002264646464a66602c603000426493198038038008a99809a481334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e2065787065637465640016301600130160023370e900118089baa3014001300c0021533300e3370e90010008a99980918060010a4c2a6601e9211d4578706563746564206e6f206669656c647320666f7220436f6e7374720016153300f4912b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e740016300c00100632005300100430010012232533300b3370e90000008991919192999809980a80109924c6600e00e0022a66020921334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e206578706563746564001630130013013002375a602200260120042a66601666e1d20020011533300f3009002149854cc03124011d4578706563746564206e6f206669656c647320666f7220436f6e7374720016153300c4912b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e740016300900133001001480008888cccc01ccdc38008018061199980280299b8000448008c0380040080088c018dd5000918021baa0015734ae7155ceaab9e5573eae855d11",
              "hash": "b5da17417d29be9264832a3550a73b4fddd4f82a74a488c91d861262",
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
