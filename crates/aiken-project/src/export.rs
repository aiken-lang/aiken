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
    tipo::{Type, TypeVar, pretty::Printer},
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
                .map(fuzzer_output_type_from)
                .unwrap_or(FuzzerOutputType::Unsupported("List<?>".into()));
            FuzzerOutputType::List(Box::new(inner))
        }
        Type::Tuple { elems, .. } => {
            let inner = elems.iter().map(fuzzer_output_type_from).collect();
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

/// Typed constraint IR describing what a fuzzer is known to produce.
///
/// This replaces the old `ExportedBounds` with a richer, composable representation
/// that can describe constraints for arbitrary fuzzer output shapes (not just integers).
#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
pub enum FuzzerExactValue {
    Bool(bool),
    ByteArray(Vec<u8>),
    String(String),
}

#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
pub enum FuzzerConstraint {
    /// No constraint known; the fuzzer may produce any value of the given type.
    Any,
    /// Integer in a closed range [min, max].
    IntRange { min: String, max: String },
    /// ByteString length in a closed range [min_len, max_len].
    ByteStringLenRange { min_len: usize, max_len: usize },
    /// Exact scalar value.
    Exact(FuzzerExactValue),
    /// A tuple whose elements each carry their own constraint.
    Tuple(Vec<FuzzerConstraint>),
    /// A list whose elements satisfy `elem`, with optional length bounds.
    List {
        elem: Box<FuzzerConstraint>,
        #[serde(skip_serializing_if = "Option::is_none")]
        min_len: Option<usize>,
        #[serde(skip_serializing_if = "Option::is_none")]
        max_len: Option<usize>,
    },
    /// Finite set of nullary ADT constructors represented as `Data.Constr tag []`.
    ///
    /// Tags are constructor ordinals from source declaration order.
    DataConstructorTags { tags: Vec<u64> },
    /// A mapped constraint: the underlying constraint describes the input domain,
    /// but the output type may differ (e.g. `fuzz.map(int_between(0,10), fn(x) { ... })`).
    Map(Box<FuzzerConstraint>),
    /// Conjunction of constraints (all must hold).
    And(Vec<FuzzerConstraint>),
    /// Constraint could not be extracted; includes a human-readable reason.
    Unsupported { reason: String },
}

#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
pub enum FuzzerSemantics {
    Bool,
    IntRange {
        #[serde(skip_serializing_if = "Option::is_none")]
        min: Option<String>,
        #[serde(skip_serializing_if = "Option::is_none")]
        max: Option<String>,
    },
    ByteArrayRange {
        #[serde(skip_serializing_if = "Option::is_none")]
        min_len: Option<usize>,
        #[serde(skip_serializing_if = "Option::is_none")]
        max_len: Option<usize>,
    },
    String,
    Data,
    Exact(FuzzerExactValue),
    Product(Vec<FuzzerSemantics>),
    List {
        element: Box<FuzzerSemantics>,
        #[serde(skip_serializing_if = "Option::is_none")]
        min_len: Option<usize>,
        #[serde(skip_serializing_if = "Option::is_none")]
        max_len: Option<usize>,
    },
    Constructors {
        tags: Vec<u64>,
    },
    StateMachineTrace {
        acceptance: StateMachineAcceptance,
        state_type: FuzzerOutputType,
        #[serde(skip_serializing_if = "Vec::is_empty")]
        #[serde(default)]
        step_input_types: Vec<FuzzerOutputType>,
        label_type: FuzzerOutputType,
        event_type: FuzzerOutputType,
        transition_semantics: StateMachineTransitionSemantics,
        output_semantics: Box<FuzzerSemantics>,
    },
    Opaque {
        reason: String,
    },
}

#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
pub enum StateMachineAcceptance {
    AcceptsSuccess,
    AcceptsFailure,
}

#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
pub struct StateMachineTransitionSemantics {
    pub terminal_tag: u64,
    pub step_tag: u64,
    pub label_field_index: usize,
    pub next_state_field_index: usize,
    pub event_field_index: usize,
    pub state_semantics: Box<FuzzerSemantics>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    #[serde(default)]
    pub step_input_semantics: Vec<FuzzerSemantics>,
    pub label_semantics: Box<FuzzerSemantics>,
    pub event_semantics: Box<FuzzerSemantics>,
}

#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
pub struct ExportedDataSchema {
    pub root: crate::blueprint::definitions::Reference,

    #[serde(skip_serializing_if = "Definitions::is_empty")]
    #[serde(default)]
    pub definitions: Definitions<Annotated<Schema>>,
}

/// Whether a property test returns Bool or Void.
///
/// Bool-returning tests are verified via `proveTests` (Option Bool) with `= true`/`= false`.
/// Void-returning tests are verified via `proveTestsHalt` which checks for non-error termination.
#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
pub enum TestReturnMode {
    Bool,
    Void,
}

/// Classification of a verification target.
///
/// - `PropertyWrapper`: the test is a standalone property test (default).
/// - `ValidatorHandler`: the test exercises a validator handler directly.
/// - `Equivalence`: the test proves that a property wrapper and a validator
///   handler produce the same result for all inputs.
#[derive(Debug, PartialEq, Clone, Default, serde::Serialize, serde::Deserialize)]
pub enum VerificationTargetKind {
    #[default]
    PropertyWrapper,
    ValidatorHandler,
    Equivalence,
}

impl std::fmt::Display for VerificationTargetKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VerificationTargetKind::PropertyWrapper => write!(f, "property"),
            VerificationTargetKind::ValidatorHandler => write!(f, "validator"),
            VerificationTargetKind::Equivalence => write!(f, "equivalence"),
        }
    }
}

impl std::str::FromStr for VerificationTargetKind {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "property" => Ok(VerificationTargetKind::PropertyWrapper),
            "validator" => Ok(VerificationTargetKind::ValidatorHandler),
            "equivalence" => Ok(VerificationTargetKind::Equivalence),
            _ => Err(format!(
                "Unknown verification target '{}'; expected 'property', 'validator', or 'equivalence'",
                s
            )),
        }
    }
}

/// Optional metadata describing the validator handler associated with a property test.
/// Present when the test targets a specific validator handler for direct or equivalence verification.
#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
pub struct ValidatorTarget {
    /// Module containing the validator (e.g. "validators/my_validator")
    pub validator_module: String,
    /// Validator name (e.g. "spend")
    pub validator_name: String,
    /// Handler function name within the validator (e.g. "spend.handler")
    #[serde(skip_serializing_if = "Option::is_none")]
    pub handler_name: Option<String>,
    /// Compiled handler program (UPLC flat hex)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub handler_program: Option<ExportedProgram>,
}

#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
pub struct ExportedPropertyTest {
    pub name: String,
    pub module: String,
    pub input_path: String,
    pub on_test_failure: OnTestFailure,
    pub return_mode: TestReturnMode,
    /// Classification of the verification target (default: PropertyWrapper).
    #[serde(default)]
    pub target_kind: VerificationTargetKind,
    /// Validator target metadata, present for ValidatorHandler and Equivalence modes.
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub validator_target: Option<ValidatorTarget>,
    pub test_program: ExportedProgram,
    pub fuzzer_program: ExportedProgram,
    pub fuzzer_type: String,
    pub fuzzer_output_type: FuzzerOutputType,
    pub constraint: FuzzerConstraint,
    pub semantics: FuzzerSemantics,
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub fuzzer_data_schema: Option<ExportedDataSchema>,
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
