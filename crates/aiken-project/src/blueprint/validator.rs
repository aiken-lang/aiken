use super::{
    definitions::{Definitions, Reference},
    error::Error,
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
    pub datum: Option<Argument>,

    pub redeemer: Argument,

    #[serde(skip_serializing_if = "Vec::is_empty")]
    #[serde(default)]
    pub parameters: Vec<Argument>,

    #[serde(flatten)]
    pub program: Program<DeBruijn>,

    #[serde(skip_serializing_if = "Definitions::is_empty")]
    #[serde(default)]
    pub definitions: Definitions<Annotated<Schema>>,
}

#[derive(Debug, PartialEq, Eq, Clone, serde::Serialize, serde::Deserialize)]
pub struct Argument {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub title: Option<String>,

    pub schema: Reference,
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
                        .map(|schema| Argument {
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
                .map(|schema| Argument {
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
                .map(|schema| Argument {
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

#[cfg(test)]
mod test {
    use super::*;
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
              "compiledCode": "583b010000323232323232322253330054a22930b180080091129998030010a4c26600a6002600e0046660060066010004002ae695cdaab9f5742ae881",
              "hash": "afddc16c18e7d8de379fb9aad39b3d1b5afd27603e5ebac818432a72",
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
              "compiledCode": "5840010000323232323232322322253330074a22930b1bad0013001001222533300600214984cc014c004c01c008ccc00c00cc0200080055cd2b9b5573eae855d101",
              "hash": "a82df717fd39f5b273c4eb89ae5252e11cc272ac59d815419bf2e4c3",
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
              "compiledCode": "583b0100003232323232323222253330064a22930b180080091129998030010a4c26600a6002600e0046660060066010004002ae695cdaab9f5742ae89",
              "hash": "e37db487fbd58c45d059bcbf5cd6b1604d3bec16cf888f1395a4ebc4",
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
              "compiledCode": "585301000032323232323232232232253330084a22930b1bac0013232337606012004601200260120026eb0004c0040048894ccc0180085261330053001300700233300300330080020015734ae6d55cfaba157441",
              "hash": "500b9b576c11ad73dee3b9d5202496a7df78e8de4097c57f0acfcc3a",
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
              "compiledCode": "583b010000323232323232322253330054a22930b180080091129998030010a4c26600a6002600e0046660060066010004002ae695cdaab9f5742ae881",
              "hash": "afddc16c18e7d8de379fb9aad39b3d1b5afd27603e5ebac818432a72",
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
              "compiledCode": "583b010000323232323232322253330054a22930b180080091129998030010a4c26600a6002600e0046660060066010004002ae695cdaab9f5742ae881",
              "hash": "afddc16c18e7d8de379fb9aad39b3d1b5afd27603e5ebac818432a72",
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
              "compiledCode": "583b010000323232323232322253330054a22930b180080091129998030010a4c26600a6002600e0046660060066010004002ae695cdaab9f5742ae881",
              "hash": "afddc16c18e7d8de379fb9aad39b3d1b5afd27603e5ebac818432a72",
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
              "compiledCode": "5840010000323232323232322232253330074a22930b1bad0013001001222533300600214984cc014c004c01c008ccc00c00cc0200080055cd2b9b5573eae855d101",
              "hash": "a3dbab684d90d19e6bab3a0b00a7290ff59fe637d14428859bf74376",
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
              "compiledCode": "583b010000323232323232322253330054a22930b180080091129998030010a4c26600a6002600e0046660060066010004002ae695cdaab9f5742ae881",
              "hash": "afddc16c18e7d8de379fb9aad39b3d1b5afd27603e5ebac818432a72",
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
              "compiledCode": "583b0100003232323232323222253330064a22930b180080091129998030010a4c26600a6002600e0046660060066010004002ae695cdaab9f5742ae89",
              "hash": "e37db487fbd58c45d059bcbf5cd6b1604d3bec16cf888f1395a4ebc4",
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
}
