use super::{
    definitions::Definitions,
    error::Error,
    memo_program::MemoProgram,
    parameter::Parameter,
    schema::{Annotated, Schema},
};
use crate::module::{CheckedModule, CheckedModules};
use aiken_lang::{
    ast::{Annotation, TypedArg, TypedFunction, TypedValidator},
    gen_uplc::CodeGenerator,
    plutus_version::PlutusVersion,
    tipo::Type,
};
use miette::NamedSource;
use serde;
use std::borrow::Borrow;
use uplc::{
    ast::{Constant, SerializableProgram},
    PlutusData,
};

#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
pub struct Validator {
    pub title: String,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub datum: Option<Parameter>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub redeemer: Option<Parameter>,

    #[serde(skip_serializing_if = "Vec::is_empty")]
    #[serde(default)]
    pub parameters: Vec<Parameter>,

    #[serde(flatten)]
    pub program: SerializableProgram,

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
        plutus_version: &PlutusVersion,
    ) -> Vec<Result<Validator, Error>> {
        let mut program = MemoProgram::new();

        let mut validators = vec![];

        for handler in &def.handlers {
            validators.push(Validator::create_validator_blueprint(
                generator,
                modules,
                module,
                def,
                handler,
                &mut program,
                plutus_version,
            ));
        }

        validators.push(Validator::create_validator_blueprint(
            generator,
            modules,
            module,
            def,
            &def.fallback,
            &mut program,
            plutus_version,
        ));

        validators
    }

    #[allow(clippy::too_many_arguments)]
    fn create_validator_blueprint(
        generator: &mut CodeGenerator,
        modules: &CheckedModules,
        module: &CheckedModule,
        def: &TypedValidator,
        func: &TypedFunction,
        program: &mut MemoProgram,
        plutus_version: &PlutusVersion,
    ) -> Result<Validator, Error> {
        let mut args = func.arguments.iter().rev();

        let (_, _, redeemer, datum) = (args.next(), args.next(), args.next(), args.next());

        let mut definitions = Definitions::new();

        let parameters = def
            .params
            .iter()
            .map(|param| {
                Annotated::from_type(
                    modules.into(),
                    tipo_or_annotation(module, param),
                    &mut definitions,
                )
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

        let datum = datum
            .map(|datum| {
                Annotated::from_type(
                    modules.into(),
                    tipo_or_annotation(module, datum),
                    &mut definitions,
                )
                .map_err(|error| Error::Schema {
                    error,
                    location: datum.location,
                    source_code: NamedSource::new(
                        module.input_path.display().to_string(),
                        module.code.clone(),
                    ),
                })
            })
            .transpose()?
            .map(|schema| Parameter {
                title: datum.map(|datum| datum.arg_name.get_label()),
                schema,
            });

        let redeemer = redeemer
            .map(|redeemer| {
                Annotated::from_type(
                    modules.into(),
                    tipo_or_annotation(module, redeemer),
                    &mut definitions,
                )
                .map_err(|error| Error::Schema {
                    error,
                    location: redeemer.location,
                    source_code: NamedSource::new(
                        module.input_path.display().to_string(),
                        module.code.clone(),
                    ),
                })
            })
            .transpose()?
            .map(|schema| Parameter {
                title: redeemer.map(|redeemer| redeemer.arg_name.get_label()),
                schema,
            });

        Ok(Validator {
            title: format!("{}.{}_{}", &module.name, &def.name, &func.name),
            description: func.doc.clone(),
            parameters,
            datum,
            redeemer,
            program: match plutus_version {
                PlutusVersion::V1 => SerializableProgram::PlutusV1Program,
                PlutusVersion::V2 => SerializableProgram::PlutusV2Program,
                PlutusVersion::V3 => SerializableProgram::PlutusV3Program,
            }(program.get(generator, def, &module.name)),
            definitions,
        })
    }
}

pub fn tipo_or_annotation<'a>(module: &'a CheckedModule, arg: &'a TypedArg) -> &'a Type {
    match *arg.tipo.borrow() {
        Type::App {
            module: ref module_name,
            name: ref type_name,
            ..
        } if module_name.is_empty() && &type_name[..] == "Data" => match arg.annotation {
            Some(Annotation::Constructor { ref arguments, .. }) if !arguments.is_empty() => module
                .ast
                .type_info
                .annotations
                .get(
                    arguments
                        .first()
                        .expect("guard ensures at least one element"),
                )
                .unwrap_or(&arg.tipo),
            _ => &arg.tipo,
        },
        _ => &arg.tipo,
    }
}

impl Validator {
    pub fn apply(
        self,
        definitions: &Definitions<Annotated<Schema>>,
        arg: &PlutusData,
    ) -> Result<Self, Error> {
        match self.parameters.split_first() {
            None => Err(Error::NoParametersToApply),
            Some((head, tail)) => {
                head.validate(definitions, &Constant::Data(arg.clone()))?;
                Ok(Self {
                    program: self.program.map(|program| program.apply_data(arg.clone())),
                    parameters: tail.to_vec(),
                    ..self
                })
            }
        }
    }

    pub fn ask_next_parameter<F>(
        &self,
        definitions: &Definitions<Annotated<Schema>>,
        ask: F,
    ) -> Result<PlutusData, Error>
    where
        F: Fn(&Annotated<Schema>, &Definitions<Annotated<Schema>>) -> Result<PlutusData, Error>,
    {
        match self.parameters.split_first() {
            None => Err(Error::NoParametersToApply),
            Some((head, _)) => {
                let schema = definitions
                    .lookup(&head.schema)
                    .map(|s| {
                        Ok(Annotated {
                            title: s.title.clone().or_else(|| head.title.clone()),
                            description: s.description.clone(),
                            annotated: s.annotated.clone(),
                        })
                    })
                    .unwrap_or_else(|| {
                        Err(Error::UnresolvedSchemaReference {
                            reference: head.schema.clone(),
                        })
                    })?;

                let data = ask(&schema, definitions)?;

                Ok(data.clone())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{
        super::{
            definitions::{Definitions, Reference},
            error::Error,
            schema::{Annotated, Constructor, Data, Declaration, Items, Schema},
        },
        *,
    };
    use crate::tests::TestProject;
    use aiken_lang::{
        self,
        ast::{TraceLevel, Tracing},
        tipo::Type,
    };
    use std::collections::HashMap;
    use uplc::ast as uplc_ast;

    macro_rules! assert_validator {
        ($code:expr) => {
            let mut project = TestProject::new();

            let modules = CheckedModules::singleton(project.check(project.parse(indoc::indoc! { $code })));

            let mut generator = project.new_generator(
                Tracing::All(TraceLevel::Verbose),
            );

            let (validator, def) = modules
                .validators()
                .next()
                .expect("source code did no yield any validator");

            let validators = Validator::from_checked_module(&modules, &mut generator, validator, def, &PlutusVersion::default());

            if validators.len() > 2 {
                panic!("Multi-validator given to test bench. Don't do that.")
            }

            let validator = validators
                .get(0)
                .unwrap()
                .as_ref();

            match validator {
                Err(e) => insta::with_settings!({
                    description => concat!("Code:\n\n", indoc::indoc! { $code }),
                    omit_expression => true
                }, {
                    insta::assert_debug_snapshot!(e);
                }),

                Ok(validator) => insta::with_settings!({
                    description => concat!("Code:\n\n", indoc::indoc! { $code }),
                    omit_expression => true
                }, {
                    insta::assert_json_snapshot!(
                        validator,
                        {
                            ".compiledCode" => "<redacted>",
                            ".hash" => "<redacted>"
                        }
                    );
                }),
            };
        };
    }

    fn fixture_definitions() -> Definitions<Annotated<Schema>> {
        let mut definitions = Definitions::new();

        // #/definitions/Int
        //
        // {
        //   "dataType": "integer"
        // }
        definitions
            .register::<_, Error>(&Type::int(), &HashMap::new(), |_| {
                Ok(Schema::Data(Data::Integer).into())
            })
            .unwrap();

        // #/definitions/ByteArray
        //
        // {
        //   "dataType": "bytes"
        // }
        definitions
            .register::<_, Error>(&Type::byte_array(), &HashMap::new(), |_| {
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
        assert_validator!(
            r#"
            validator thing {
              mint(redeemer: Data, policy_id: Data, transaction: Data) {
                True
              }
            }
            // "#
        );
    }

    #[test]
    fn mint_parameterized() {
        assert_validator!(
            r#"
            validator thing(utxo_ref: Int) {
              mint(redeemer: Data, ctx: Data) {
                True
              }
            }
            // "#
        );
    }

    #[test]
    fn simplified_hydra() {
        assert_validator!(
            r#"
            /// On-chain state
            pub type State {
                /// The contestation period as a number of seconds
                contestationPeriod: ContestationPeriod,
                /// List of public key hashes of all participants
                parties: List<Party>,
                utxoHash: Hash<Blake2b_256>,
            }

            /// A Hash digest for a given algorithm.
            pub type Hash<alg> = ByteArray

            pub type Blake2b_256 { Blake2b_256 }

            /// Whatever
            pub type ContestationPeriod {
              /// A positive, non-zero number of seconds.
              ContestationPeriod(Int)
            }

            pub type Party =
              ByteArray

            pub type Input {
                CollectCom
                Close
                /// Abort a transaction
                Abort
            }

            validator simplified_hydra {
              spend(datum: State, redeemer: Input, ctx: Data) {
                True
              }
            }
            "#
        );
    }

    #[test]
    fn tuples() {
        assert_validator!(
            r#"
            validator tuples {
              spend(datum: (Int, ByteArray), redeemer: (Int, Int, Int), ctx: Void) {
                True
              }
            }
            "#
        );
    }

    #[test]
    fn generics() {
        assert_validator!(
            r#"
            pub type Either<left, right> {
                Left(left)
                Right(right)
            }

            pub type Interval<a> {
                Finite(a)
                Infinite
            }

            validator generics {
              spend(redeemer: Either<ByteArray, Interval<Int>>, ctx: Void) {
                True
              }
            }
            "#
        );
    }

    #[test]
    fn free_vars() {
        assert_validator!(
            r#"
            validator generics {
              mint(redeemer: a, policy_id: ByteArray, ctx: Void) {
                True
              }
            }
            "#
        );
    }

    #[test]
    fn list_2_tuples_as_list() {
        assert_validator!(
            r#"
            pub type Dict<key, value> {
                inner: List<(ByteArray, value)>
            }

            pub type UUID { UUID }

            validator list_2_tuples_as_list {
              mint(redeemer: Dict<UUID, Int>, ctx: Void) {
                True
              }
            }
            "#
        );
    }

    #[test]
    fn list_pairs_as_map() {
        assert_validator!(
            r#"
            pub type Dict<key, value> {
                inner: List<Pair<ByteArray, value>>
            }

            pub type UUID { UUID }

            validator list_pairs_as_map {
              spend(redeemer: Dict<UUID, Int>, ctx: Void) {
                True
              }
            }
            "#
        );
    }

    #[test]
    fn opaque_singleton_variants() {
        assert_validator!(
            r#"
            pub opaque type Dict<key, value> {
                inner: List<(ByteArray, value)>
            }

            pub type UUID { UUID }

            validator opaque_singleton_variants {
              spend(redeemer: Dict<UUID, Int>, ctx: Void) {
                True
              }
            }
            "#
        );
    }

    #[test]
    fn opaque_singleton_multi_variants() {
        assert_validator!(
            r#"
            pub opaque type Rational {
              numerator: Int,
              denominator: Int,
            }

            validator opaque_singleton_multi_variants {
              spend(redeemer: Rational, oref: Data, ctx: Void) {
                True
              }
            }
            "#
        );
    }

    #[test]
    fn nested_data() {
        assert_validator!(
            r#"
            pub type Foo {
                foo: Data
            }

            validator nested_data {
              spend(datum: Foo, redeemer: Int, ctx: Void) {
                True
              }
            }
            "#
        );
    }

    #[test]
    fn recursive_types() {
        assert_validator!(
            r#"
            pub type Expr {
              Val(Int)
              Sum(Expr, Expr)
              Mul(Expr, Expr)
            }

            validator recursive_types {
              spend(redeemer: Expr, ctx: Void) {
                True
              }
            }
            "#
        );
    }

    #[test]
    fn recursive_generic_types() {
        assert_validator!(
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

            validator recursive_generic_types {
              spend(datum: Foo, redeemer: LinkedList<Int>, ctx: Void) {
                True
              }
            }
            "#
        );
    }

    #[test]
    fn annotated_data() {
        assert_validator!(
            r#"
            pub type Foo {
                foo: Int
            }

            validator annotated_data {
                spend(datum: Data<Foo>, redeemer: Data, ctx: Void) {
                    True
                }
            }
            "#
        );
    }

    #[test]
    fn validate_arguments_integer() {
        let definitions = fixture_definitions();

        let term = Constant::Data(uplc_ast::Data::integer(42.into()));

        let param = Parameter {
            title: None,
            schema: Reference::new("Int"),
        };

        assert!(matches!(param.validate(&definitions, &term), Ok { .. }))
    }

    #[test]
    fn validate_arguments_bytestring() {
        let definitions = fixture_definitions();

        let term = Constant::Data(uplc_ast::Data::bytestring(vec![102, 111, 111]));

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

        let term = Constant::Data(uplc_ast::Data::list(vec![
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

        let term = Constant::Data(uplc_ast::Data::list(vec![uplc_ast::Data::bytestring(
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

        let term = Constant::Data(uplc_ast::Data::list(vec![
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

        let term = Constant::Data(uplc_ast::Data::map(vec![(
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

        let term = Constant::Data(uplc_ast::Data::constr(1, vec![]));

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

        let term = Constant::Data(uplc_ast::Data::constr(
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

        let term = Constant::Data(uplc_ast::Data::constr(
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
