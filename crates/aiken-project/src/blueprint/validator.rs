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
    tipo::Type,
};
use miette::NamedSource;
use serde;
use std::borrow::Borrow;
use uplc::{
    ast::{Constant, DeBruijn, Program},
    PlutusData,
};

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
        let is_multi_validator = def.other_fun.is_some();

        let mut program = MemoProgram::new();

        let mut validators = vec![Validator::create_validator_blueprint(
            generator,
            modules,
            module,
            def,
            &def.fun,
            is_multi_validator,
            &mut program,
        )];

        if let Some(ref other_func) = def.other_fun {
            validators.push(Validator::create_validator_blueprint(
                generator,
                modules,
                module,
                def,
                other_func,
                is_multi_validator,
                &mut program,
            ));
        }

        validators
    }

    fn create_validator_blueprint(
        generator: &mut CodeGenerator,
        modules: &CheckedModules,
        module: &CheckedModule,
        def: &TypedValidator,
        func: &TypedFunction,
        is_multi_validator: bool,
        program: &mut MemoProgram,
    ) -> Result<Validator, Error> {
        let mut args = func.arguments.iter().rev();
        let (_, redeemer, datum) = (args.next(), args.next().unwrap(), args.next());

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

        let redeemer = Annotated::from_type(
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
        .map(|schema| Parameter {
            title: Some(redeemer.arg_name.get_label()),
            schema: match datum {
                Some(..) if is_multi_validator => {
                    Annotated::as_wrapped_redeemer(&mut definitions, schema, redeemer.tipo.clone())
                }
                _ => schema,
            },
        })?;

        Ok(Validator {
            title: format!("{}.{}", &module.name, &func.name),
            description: func.doc.clone(),
            parameters,
            datum,
            redeemer,
            program: program.get(generator, def, &module.name),
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
                    program: self.program.apply_data(arg.clone()),
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
        builtins,
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

            let validators = Validator::from_checked_module(&modules, &mut generator, validator, def);

            if validators.len() > 1 {
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
                    insta::assert_json_snapshot!(validator);
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
        assert_validator!(
            r#"
            validator {
              fn mint(redeemer: Data, ctx: Data) {
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
            validator(utxo_ref: Int) {
              fn mint(redeemer: Data, ctx: Data) {
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
            "#
        );
    }

    #[test]
    fn tuples() {
        assert_validator!(
            r#"
            validator {
              fn tuples(datum: (Int, ByteArray), redeemer: (Int, Int, Int), ctx: Void) {
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
            "#
        );
    }

    #[test]
    fn free_vars() {
        assert_validator!(
            r#"
            validator {
              fn generics(redeemer: a, ctx: Void) {
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
            type Dict<key, value> {
                inner: List<(ByteArray, value)>
            }

            type UUID { UUID }

            validator {
              fn list_2_tuples_as_list(redeemer: Dict<UUID, Int>, ctx: Void) {
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
            type Dict<key, value> {
                inner: List<Pair<ByteArray, value>>
            }

            type UUID { UUID }

            validator {
              fn list_pairs_as_map(redeemer: Dict<UUID, Int>, ctx: Void) {
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

            type UUID { UUID }

            validator {
              fn opaque_singleton_variants(redeemer: Dict<UUID, Int>, ctx: Void) {
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

            validator {
              fn opaque_singleton_multi_variants(redeemer: Rational, ctx: Void) {
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

            validator {
              fn nested_data(datum: Foo, redeemer: Int, ctx: Void) {
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

            validator {
              fn recursive_types(redeemer: Expr, ctx: Void) {
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

            validator {
              fn recursive_generic_types(datum: Foo, redeemer: LinkedList<Int>, ctx: Void) {
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

            validator {
                fn annotated_data(datum: Data<Foo>, redeemer: Data, ctx: Void) {
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
