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
    use super::{
        super::{
            definitions::{Definitions, Reference},
            error::Error,
            schema::{Annotated, Constructor, Data, Declaration, Items, Schema},
        },
        *,
    };
    use crate::{module::ParsedModule, PackageName};
    use aiken_lang::{
        self,
        ast::{ModuleKind, Tracing, TypedDataType, TypedFunction},
        builtins,
        gen_uplc::builder::{DataTypeKey, FunctionAccessKey},
        parser,
        tipo::TypeInfo,
        IdGenerator,
    };
    use assert_json_diff::assert_json_eq;
    use indexmap::IndexMap;
    use serde_json::{self, json};
    use std::{collections::HashMap, path::PathBuf};
    use uplc::ast as uplc;

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
              "compiledCode": "46010000224a21",
              "hash": "143d1a7343538900ea5c5f401da159ff9a4ef61a17ef10128498012c",
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
              "compiledCode": "4b010000232224a26eb40041",
              "hash": "77daa493efa253d8acbcc8993039f499daad53c977d6a5984d1940af",
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
              "compiledCode": "5902a4010000323232323232323232323232323222232324a26601864a66601866e1d20000011323232323232533301730190021330133253330133370e900000089919299980d180e0010a4c2a6602e9201334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e2065787065637465640016375a6034002602200c2a6602a9212b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e7400163011005330133300c003232498dd7000a4c2a660289201334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e2065787065637465640016375c602e002602e0046eb0c054004c054008c04c004c02801454cc0392412b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e740016300a0040043300b32533300b3370e90000008a99980818048018a4c2a6601a92011d4578706563746564206e6f206669656c647320666f7220436f6e73747200161533300b3370e90010008a99980818048018a4c2a6601a92011d4578706563746564206e6f206669656c647320666f7220436f6e73747200161533300b3370e90020008a99980818048018a4c2a6601a92011d4578706563746564206e6f206669656c647320666f7220436f6e7374720016153300d4912b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e74001630090020023001001222533300d00214984cc024c004c038008ccc00c00cc03c008004cc0040052000222233330073370e00200601a4666600a00a66e000112002300f0010020022300737540024600a6ea80055cd2b9b5738aae7555cf2ab9f5742ae89",
              "hash": "b6cbe3c6a543b8d7f45678a0a89ca212fc3ef7357cc4cacbb0b71819",
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
              "compiledCode": "58a201000032323232323222232324a26464640066eb8c024008dd69803800991919bb0300b002300b001300b0013758008646464646464a66601a601e004200c2a660149201334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e2065787065637465640016375a601a002601a0046eb4c02c004c02c008dd698048009bac0025738aae7555cf2ab9f5742ae881",
              "hash": "094fee978ef0240b1a34eea613b00c6e91313a73f15fab3738e3d366",
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
              "compiledCode": "59013a010000323232323232323232323222324a26600e64a66600e66e1d200000113232533300e3010002149854cc02d241334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e2065787065637465640016300e0013006003153330073370e900100089919299980718080010a4c2a660169201334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e2065787065637465640016300e001300600315330094912b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e740016300937540040046600200290001111199980319b8700100300c233330050053370000890011807000801001118031baa0015734ae6d5ce2ab9d5573caae7d5d0aba201",
              "hash": "d2615c021b2e14cd4ff854eb0a9ed88fd9f8a004105784d693f9aee0",
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
              "compiledCode": "59011101000032323232323232323232323222324a26601064a66601066e1d200000113232533300f301100213300b3300600123232498c040008dd71807000a4c2a66018921334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e20657870656374656400163756601e00260186ea800c54cc0292412b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e740016300a37540040046002002444a66601600429309980398009806001199801801980680100099800800a40004444666600a66e1c00400c02c8cccc014014cdc000224004601a002004004ae695cdab9c5573aaae7955cfaba15745",
              "hash": "7fd359ec9930889984ffcaee7f7a044d872851560b051d05c5520486",
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
              "compiledCode": "5854010000323232323232323222324a2646600c6600800246464931bad300a002375c60100020026eac008c0040048894ccc01c0085261330043001300800233300300330090020015736aae7555cf2ab9f5742ae89",
              "hash": "33e1c3d972dfae234a3435c7d95e0e0b975bbebd908fe51f86504ab1",
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
              "compiledCode": "58e50100003232323232323232323222232324a26601064a66601066e1d200000113232533300f3011002149854cc0312401334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e2065787065637465640016300f001300c375400a2a660149212b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e740016300a3754008008640026eb4008cc0040052000222233330053370e0020060164666600a00a66e000112002300d0010020025734ae6d5ce2ab9d5573caae7d5d0aba201",
              "hash": "893c1164487d57814a1346df341de762193fa64eefbd0cf03a44b08a",
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
              "compiledCode": "5901d00100003232323232323232323232323222324a266012600600400460020024464a66600e66e1d200000113232533300e3010002149854cc02d2401334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e2065787065637465640016375a601c002600c0042a66600e66e1d20020011323232325333010301200213300c330070070033300c3300700700149854cc0352401334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e206578706563746564001630100013010002300e0013006002153330073370e900200089919191929998081809001099806198038038019980619803803800a4c2a6601a9201334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e206578706563746564001630100013010002300e001300600215330094912b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e740016300937540026600200290001111199980319b8700100300c233330050053370000890011807000801001118031baa0015734ae6d5ce2ab9d5573caae7d5d0aba21",
              "hash": "dfc92360583a9df909cd58c2f2564c35163f00e0e3a93ffecf2838b4",
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
              "compiledCode": "5902840100003232323232323232323232323232323222232324a26601c64a66601c66e1d20000011323253330153017002133011300800149854cc0492401334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e20657870656374656400163015001300c0051533300e3370e9001000899191919299980b980c801099191980a9980700091980b1806800a4c931bac3017002375c602a0022a660289201334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e20657870656374656400163232337606032004603200260320026eb0c05c004c05c008dd6980a80098060028a9980824812b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e740016300c0040043300d300400200230010012232533300a3370e90000008991919192999809980a80109980799803803800a4c2a66020921334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e206578706563746564001630130013013002301100130080021533300a3370e90010008a99980798040010a4c2a6601892011d4578706563746564206e6f206669656c647320666f7220436f6e7374720016153300c4912b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e74001630080013001001222533300d00214984cc024c004c038008ccc00c00cc03c008004cc0040052000222233330073370e00200601a4666600a00a66e000112002300f0010020022300737540024600a6ea80055cd2b9b5738aae7555cf2ab9f5742ae881",
              "hash": "aa6c9ffe85565798974269ea30a9b76744bb8608356c758d75486560",
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

        let term = Term::data(uplc::Data::integer(42.into()));

        let param = Parameter {
            title: None,
            schema: Reference::new("Int"),
        };

        assert!(matches!(param.validate(&definitions, &term), Ok { .. }))
    }

    #[test]
    fn validate_arguments_bytestring() {
        let definitions = fixture_definitions();

        let term = Term::data(uplc::Data::bytestring(vec![102, 111, 111]));

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

        let term = Term::data(uplc::Data::list(vec![
            uplc::Data::integer(42.into()),
            uplc::Data::integer(14.into()),
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

        let term = Term::data(uplc::Data::list(vec![uplc::Data::bytestring(vec![
            102, 111, 111,
        ])]));

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

        let term = Term::data(uplc::Data::list(vec![
            uplc::Data::integer(42.into()),
            uplc::Data::bytestring(vec![102, 111, 111]),
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

        let term = Term::data(uplc::Data::map(vec![(
            uplc::Data::bytestring(vec![102, 111, 111]),
            uplc::Data::integer(42.into()),
        )]));

        let param: Parameter = schema.into();

        assert!(matches!(param.validate(&definitions, &term), Ok { .. }))
    }

    #[test]
    fn validate_arguments_constr_nullary() {
        let schema = Reference::new("Bool");

        let definitions = fixture_definitions();

        let term = Term::data(uplc::Data::constr(1, vec![]));

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

        let term = Term::data(uplc::Data::constr(0, vec![uplc::Data::constr(0, vec![])]));

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

        let term = Term::data(uplc::Data::constr(
            1,
            vec![
                uplc::Data::integer(14.into()),
                uplc::Data::constr(
                    1,
                    vec![
                        uplc::Data::integer(42.into()),
                        uplc::Data::constr(0, vec![]),
                    ],
                ),
            ],
        ));

        let param: Parameter = schema.into();

        assert!(matches!(param.validate(&definitions, &term), Ok { .. }))
    }
}
