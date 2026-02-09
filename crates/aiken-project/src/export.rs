use crate::{
    blueprint::{
        self,
        definitions::Definitions,
        parameter::Parameter,
        schema::{Annotated, Declaration, Schema},
    },
    module::{CheckedModule, CheckedModules},
};
use aiken_lang::{
    ast::{ArgName, OnTestFailure, Span, TypedArg, TypedFunction},
    gen_uplc::CodeGenerator,
    plutus_version::PlutusVersion,
    tipo::{pretty::Printer, Type, TypeVar},
};
use miette::NamedSource;
use std::{ops::Deref, rc::Rc};
use uplc::ast::SerializableProgram;

#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
pub struct Export {
    pub name: String,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub doc: Option<String>,

    #[serde(skip_serializing_if = "Vec::is_empty")]
    #[serde(default)]
    pub parameters: Vec<Parameter>,

    pub return_type: Parameter,

    #[serde(flatten)]
    pub program: SerializableProgram,

    #[serde(skip_serializing_if = "Definitions::is_empty")]
    #[serde(default)]
    pub definitions: Definitions<Annotated<Schema>>,
}

#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
pub struct ExportedProgram {
    pub hex: String,

    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub flat_bytes: Option<Vec<u8>>,
}

#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
pub enum FuzzerOutputType {
    Int,
    Bool,
    ByteArray,
    String,
    Data,
    List(Box<FuzzerOutputType>),
    Tuple(Vec<FuzzerOutputType>),
    Pair(Box<FuzzerOutputType>, Box<FuzzerOutputType>),
    Unsupported(std::string::String),
}

pub fn fuzzer_output_type_from(tipo: &Rc<Type>) -> FuzzerOutputType {
    fuzzer_output_type_from_type(tipo.as_ref())
}

fn fuzzer_output_type_from_type(tipo: &Type) -> FuzzerOutputType {
    if tipo.is_int() {
        return FuzzerOutputType::Int;
    }
    if tipo.is_bool() {
        return FuzzerOutputType::Bool;
    }
    if tipo.is_bytearray() {
        return FuzzerOutputType::ByteArray;
    }
    if tipo.is_string() {
        return FuzzerOutputType::String;
    }
    if tipo.is_data() {
        return FuzzerOutputType::Data;
    }

    match tipo {
        Type::App {
            name, args, module, ..
        } if name == "List" && module.is_empty() => {
            let inner = args
                .first()
                .map(|a| fuzzer_output_type_from(a))
                .unwrap_or(FuzzerOutputType::Unsupported("List<?>".into()));
            FuzzerOutputType::List(Box::new(inner))
        }
        Type::Tuple { elems, .. } => {
            let inner = elems.iter().map(|e| fuzzer_output_type_from(e)).collect();
            FuzzerOutputType::Tuple(inner)
        }
        Type::Pair { fst, snd, .. } => FuzzerOutputType::Pair(
            Box::new(fuzzer_output_type_from(fst)),
            Box::new(fuzzer_output_type_from(snd)),
        ),
        Type::Var { tipo: var, .. } => {
            let borrowed = var.borrow();
            match borrowed.deref() {
                TypeVar::Link { tipo: linked } => fuzzer_output_type_from(linked),
                _ => FuzzerOutputType::Unsupported(pretty_print_type(tipo)),
            }
        }
        _ => FuzzerOutputType::Unsupported(pretty_print_type(tipo)),
    }
}

fn pretty_print_type(tipo: &Type) -> std::string::String {
    let mut printer = Printer::new();
    printer.print(tipo).to_pretty_string(80)
}

#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
pub enum ExportedBounds {
    IntBetween { min: String, max: String },
    Unknown,
}

#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
pub struct ExportedPropertyTest {
    pub name: String,
    pub module: String,
    pub input_path: String,
    pub on_test_failure: OnTestFailure,
    pub test_program: ExportedProgram,
    pub fuzzer_program: ExportedProgram,
    pub fuzzer_type: String,
    pub fuzzer_output_type: FuzzerOutputType,
    pub extracted_bounds: ExportedBounds,
}

#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
pub struct ExportedTests {
    pub plutus_version: PlutusVersion,
    pub property_tests: Vec<ExportedPropertyTest>,
}

impl Export {
    pub fn from_function(
        func: &TypedFunction,
        module: &CheckedModule,
        generator: &mut CodeGenerator,
        modules: &CheckedModules,
        plutus_version: &PlutusVersion,
    ) -> Result<Export, blueprint::Error> {
        let mut definitions = Definitions::new();

        let parameters = func
            .arguments
            .iter()
            .map(|param| {
                Annotated::from_type(
                    modules.into(),
                    blueprint::validator::tipo_or_annotation(module, param),
                    &mut definitions,
                )
                .map(|schema| Parameter {
                    title: Some(param.arg_name.get_label()),
                    schema: Declaration::Referenced(schema),
                })
                .map_err(|error| blueprint::Error::Schema {
                    error,
                    location: param.location,
                    source_code: NamedSource::new(
                        module.input_path.display().to_string(),
                        module.code.clone(),
                    ),
                })
            })
            .collect::<Result<_, _>>()?;

        let return_type = Annotated::from_type(
            modules.into(),
            blueprint::validator::tipo_or_annotation(
                module,
                &TypedArg {
                    arg_name: ArgName::Discarded {
                        name: "".to_string(),
                        label: "".to_string(),
                        location: Span::empty(),
                    },
                    location: Span::empty(),
                    annotation: func.return_annotation.clone(),
                    doc: None,
                    is_validator_param: false,
                    tipo: func.return_type.clone(),
                },
            ),
            &mut definitions,
        )
        .map(|schema| Parameter {
            title: Some("return_type".to_string()),
            schema: Declaration::Referenced(schema),
        })
        .map_err(|error| blueprint::Error::Schema {
            error,
            location: func.location,
            source_code: NamedSource::new(
                module.input_path.display().to_string(),
                module.code.clone(),
            ),
        })?;

        let program = generator
            .generate_raw(&func.body, &func.arguments, &module.name)
            .to_debruijn()
            .unwrap();

        let program = match plutus_version {
            PlutusVersion::V1 => SerializableProgram::PlutusV1Program(program),
            PlutusVersion::V2 => SerializableProgram::PlutusV2Program(program),
            PlutusVersion::V3 => SerializableProgram::PlutusV3Program(program),
        };

        Ok(Export {
            name: format!("{}.{}", &module.name, &func.name),
            doc: func.doc.clone(),
            parameters,
            return_type,
            program,
            definitions,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::{CheckedModules, Export};
    use crate::tests::TestProject;
    use aiken_lang::{
        self,
        ast::{TraceLevel, Tracing},
        plutus_version::PlutusVersion,
    };

    macro_rules! assert_export {
        ($code:expr) => {
            let mut project = TestProject::new();

            let modules = CheckedModules::singleton(project.check(project.parse(indoc::indoc! { $code })));

            let mut generator = project.new_generator(
                Tracing::All(TraceLevel::Verbose),
            );

            let (module, func) = modules
                .functions()
                .next()
                .expect("source code did no yield any exports");

            let export = Export::from_function(func, module, &mut generator, &modules, &PlutusVersion::default());

            match export {
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

    #[test]
    fn basic_export() {
        assert_export!(
            r#"
            pub fn add(a: Int, b: Int) -> Int {
                a + b
            }
            "#
        );
    }

    #[test]
    fn illegal_opaque_type() {
        assert_export!(
            r#"
            pub opaque type Thing {
              a: Int
            }

            pub fn add(a: Thing, b: Int) -> Int {
                a.a + b
            }
            "#
        );
    }

    #[test]
    fn recursive_types() {
        assert_export!(
            r#"
            pub type Foo<a> {
              Empty
              Bar(a, Foo<a>)
            }

            pub fn add(a: Foo<Int>, b: Foo<Int>) -> Int {
              when (a, b) is {
                (Empty, Empty) -> 0
                (Bar(x, y), Bar(c, d)) -> x + c + add(y, d)
                (Empty, Bar(c, d)) -> c + add(Empty, d)
                (Bar(x, y), Empty) -> x + add(y, Empty)
              }
            }
            "#
        );
    }

    #[test]
    fn cannot_export_generics() {
        assert_export!(
            r#"
            pub fn add(_a: a, _b: b) -> Bool {
                True
            }
            "#
        );
    }
}
