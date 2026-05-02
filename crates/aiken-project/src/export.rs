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
use std::{collections::BTreeMap, ops::Deref, rc::Rc};
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
#[non_exhaustive]
pub struct ExportedProgram {
    pub hex: String,

    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub flat_bytes: Option<Vec<u8>>,
}

#[non_exhaustive]
#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case")]
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

impl std::fmt::Display for FuzzerOutputType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FuzzerOutputType::Int => f.write_str("Int"),
            FuzzerOutputType::Bool => f.write_str("Bool"),
            FuzzerOutputType::ByteArray => f.write_str("ByteArray"),
            FuzzerOutputType::String => f.write_str("String"),
            FuzzerOutputType::Data => f.write_str("Data"),
            FuzzerOutputType::List(inner) => write!(f, "List<{inner}>"),
            FuzzerOutputType::Tuple(items) => {
                let rendered = items.iter().map(ToString::to_string).collect::<Vec<_>>();
                write!(f, "({})", rendered.join(", "))
            }
            FuzzerOutputType::Pair(first, second) => write!(f, "Pair<{first}, {second}>"),
            FuzzerOutputType::Unsupported(raw) => write!(f, "unsupported({raw})"),
        }
    }
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
#[non_exhaustive]
#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum FuzzerExactValue {
    Bool(bool),
    ByteArray(Vec<u8>),
    String(String),
}

impl std::fmt::Display for FuzzerExactValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FuzzerExactValue::Bool(value) => write!(f, "{value}"),
            FuzzerExactValue::ByteArray(bytes) => write!(f, "0x{}", hex::encode(bytes)),
            FuzzerExactValue::String(value) => write!(f, "\"{value}\""),
        }
    }
}

#[non_exhaustive]
#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case")]
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

// Public serialized contract; boxing fields would churn the JSON mirror API without changing wire shape.
#[non_exhaustive]
#[allow(clippy::large_enum_variant)]
#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case")]
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
    /// The fuzzer produces values of a typed Aiken ADT, represented as Plutus `Data`.
    /// A structural schema predicate must be generated from the test's
    /// `fuzzer_data_schema` and used as a precondition; the `type_name`
    /// (qualified `module.Type` when available) is recorded for Lean predicate
    /// naming only.
    DataWithSchema {
        type_name: String,
    },
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
        /// Shallow Lean IR of the step function body, for universal-theorem
        /// generation. None if translation encountered unsupported constructs
        /// throughout.
        #[serde(skip_serializing_if = "Option::is_none")]
        #[serde(default)]
        step_function_ir: Option<aiken_lang::test_framework::ShallowIr>,
        /// Human-readable reason why `step_function_ir` is None (if it is).
        #[serde(skip_serializing_if = "Option::is_none")]
        #[serde(default)]
        step_ir_unsupported_reason: Option<String>,
        /// Shallow Lean IR of the initial-state expression, for `isValidTrace`
        /// anchoring (Issue S4). `None` when translation was unavailable.
        #[serde(skip_serializing_if = "Option::is_none")]
        #[serde(default)]
        initial_state_shallow_ir: Option<aiken_lang::test_framework::ShallowIr>,
        //
        // NOTE: `TransitionProp` (scenario-plan.md Issue S3) intentionally
        // lives only on the aiken-lang `FuzzerSemantics` for now — it is
        // not serialized into the export manifest because its
        // `FuzzerSemantics` leaves would require a serde mirror chain.
        // Issue S4 (Lean emission) consumes it directly from the live
        // `FuzzerSemantics::StateMachineTrace` value; the manifest mirror
        // will be added if/when the field needs to cross the export
        // boundary.
        //
    },
    Opaque {
        reason: String,
    },
}

impl std::fmt::Display for FuzzerSemantics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FuzzerSemantics::Bool => f.write_str("Bool"),
            FuzzerSemantics::IntRange { min, max } => match (min, max) {
                (Some(min), Some(max)) => write!(f, "IntRange[{min}, {max}]"),
                (Some(min), None) => write!(f, "IntRange[{min}, ∞]"),
                (None, Some(max)) => write!(f, "IntRange[-∞, {max}]"),
                (None, None) => f.write_str("Int"),
            },
            FuzzerSemantics::ByteArrayRange { min_len, max_len } => match (min_len, max_len) {
                (Some(min), Some(max)) => write!(f, "ByteArray(length {min}..{max})"),
                (Some(min), None) => write!(f, "ByteArray(length >= {min})"),
                (None, Some(max)) => write!(f, "ByteArray(length <= {max})"),
                (None, None) => f.write_str("ByteArray"),
            },
            FuzzerSemantics::String => f.write_str("String"),
            FuzzerSemantics::Data => f.write_str("Data"),
            FuzzerSemantics::DataWithSchema { type_name } => {
                write!(f, "DataWithSchema<{type_name}>")
            }
            FuzzerSemantics::Exact(value) => write!(f, "Exact({value})"),
            FuzzerSemantics::Product(items) => {
                let rendered = items.iter().map(ToString::to_string).collect::<Vec<_>>();
                write!(f, "Product({})", rendered.join(", "))
            }
            FuzzerSemantics::List {
                element,
                min_len,
                max_len,
            } => match (min_len, max_len) {
                (Some(min), Some(max)) => write!(f, "List<{element}>(length {min}..{max})"),
                (Some(min), None) => write!(f, "List<{element}>(length >= {min})"),
                (None, Some(max)) => write!(f, "List<{element}>(length <= {max})"),
                (None, None) => write!(f, "List<{element}>"),
            },
            FuzzerSemantics::Constructors { tags } => write!(
                f,
                "Constructors(tags={})",
                tags.iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            FuzzerSemantics::StateMachineTrace { .. } => f.write_str("StateMachineTrace"),
            FuzzerSemantics::Opaque { reason } => write!(f, "Opaque({reason})"),
        }
    }
}

#[non_exhaustive]
#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum StateMachineAcceptance {
    AcceptsSuccess,
    AcceptsFailure,
}

#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
#[non_exhaustive]
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

/// Pre-emitted Lean text for the `isValidTransition` predicate + audit log
/// of over-approximations introduced during lowering.
///
/// Computed at export time (see `aiken_project::lib::convert_semantics`)
/// because the aiken-lang `TransitionProp` tree is not serializable
/// (it holds `FuzzerSemantics` in existential domains).
#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
#[non_exhaustive]
pub struct ExportedTransitionProp {
    /// Full `def <helper_prefix>_isValidTransition (state : Data) (transition : Data) : Prop := ...`
    /// block, ready to concatenate into a generated proof file.
    pub is_valid_transition_def: String,
    /// Pre-emitted Lean literal for the initial state (the first argument
    /// to `scenario.ok(init_state, step)`). When `None`, the
    /// `isValidTrace` definition falls back to an unconstrained `Data`
    /// existential, which is still sound but weaker.
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub initial_state_lean: Option<String>,
    /// Human-readable list of constraints that were dropped or widened
    /// during lowering. Each entry becomes a line in the `S4 AUDIT`
    /// comment block of the generated file.
    #[serde(skip_serializing_if = "Vec::is_empty")]
    #[serde(default)]
    pub unsupported_log: Vec<String>,
    /// Sub-generators referenced in the `TransitionProp` whose bodies could
    /// not be inlined. Each entry is `(aiken_module, function_name)`.
    /// The Lean emitter declares one `opaque ... : Data → Data → Prop` stub
    /// per unique entry before the `isValidTransition` definition.
    #[serde(skip_serializing_if = "Vec::is_empty")]
    #[serde(default)]
    pub opaque_sub_generators: Vec<(String, String)>,
    /// First S0002 (`ConstructorTagUnresolved`) marker found while walking
    /// the source `TransitionProp` tree at export time. When `Some((ctor,
    /// type_name))`, the verify-side dispatcher in `try_generate_two_phase_proof`
    /// emits a hard, non-skippable `S0002` error rather than letting the
    /// downstream emitter silently widen the marker-bearing `Opaque` to a
    /// fresh existential (which a generic vacuity guard would then catch
    /// and surface as a skippable `FallbackRequired`, masking the root
    /// cause).
    ///
    /// Commit 18 retired the legacy `S0002:…` string-prefix bridge: the
    /// constructor-name and type-name strings are now extracted directly
    /// from the typed `OpaqueCode::ConstructorTagUnresolved { ctor,
    /// type_name }` payload via
    /// `aiken_lang::test_framework::find_first_typed_opaque_in_transition_prop`.
    /// The serialised tuple shape is preserved for forward-compat of the
    /// JSON wire format (no schema break).
    ///
    /// `None` means no S0002 marker was present in the original tree.
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub s0002_marker: Option<(String, String)>,
    /// Pre-computed structural vacuity verdict, derived at export time
    /// from the source `aiken_lang::test_framework::TransitionProp` tree
    /// (the AST is not serializable, so the verdict travels alongside
    /// the rendered Lean text).
    ///
    /// The verify-side dispatcher in `try_generate_two_phase_proof`
    /// consults this field to decide whether the generated
    /// `isValidTransition` predicate is universally provable, in which
    /// case the test is promoted to `FallbackRequired` (E0021) rather
    /// than emitting a vacuous "PROVED" verdict.
    ///
    /// **Why structural rather than rendered-text-based?**  The previous
    /// vacuity check (`transition_body_is_vacuous` in `verify.rs`) walked
    /// the rendered Lean text via fragile string surgery (literal `True`
    /// detection, `∃ … , <body>` strip, no recursion through `∧`/`∨`).
    /// Any future emitter change that altered the surface form (e.g.
    /// adding a `let` binding, changing the existential printer) could
    /// silently invalidate the check.  Pre-computing the verdict from
    /// the AST eliminates this drift class.
    ///
    /// Defaults to `false` for backward-compatibility with manifests
    /// emitted before this field landed: an absent field is "we don't
    /// know if it's vacuous, assume not".  In practice every fresh
    /// export populates the field via
    /// `aiken_lang::test_framework::transition_prop_is_vacuous`.
    #[serde(default)]
    pub is_vacuous: bool,
}

#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
#[non_exhaustive]
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
#[non_exhaustive]
#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case")]
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
#[non_exhaustive]
#[derive(Debug, PartialEq, Clone, Default, serde::Serialize, serde::Deserialize)]
pub enum VerificationTargetKind {
    #[default]
    #[serde(rename = "property")]
    PropertyWrapper,
    #[serde(rename = "validator")]
    ValidatorHandler,
    #[serde(rename = "equivalence")]
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
#[non_exhaustive]
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

mod on_test_failure_wire {
    use super::OnTestFailure;
    use serde::{Deserialize, Deserializer, Serializer};

    pub fn serialize<S>(value: &OnTestFailure, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(match value {
            OnTestFailure::FailImmediately => "FailImmediately",
            OnTestFailure::SucceedImmediately => "SucceedImmediately",
            OnTestFailure::SucceedEventually => "SucceedEventually",
        })
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<OnTestFailure, D::Error>
    where
        D: Deserializer<'de>,
    {
        let raw = String::deserialize(deserializer)?;
        match raw.as_str() {
            "fail_immediately" | "FailImmediately" => Ok(OnTestFailure::FailImmediately),
            "succeed_immediately" | "SucceedImmediately" => Ok(OnTestFailure::SucceedImmediately),
            "succeed_eventually" | "SucceedEventually" => Ok(OnTestFailure::SucceedEventually),
            _ => Err(serde::de::Error::unknown_variant(
                raw.as_str(),
                &[
                    "fail_immediately",
                    "succeed_immediately",
                    "succeed_eventually",
                ],
            )),
        }
    }
}

#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
#[non_exhaustive]
pub struct ExportedPropertyTest {
    pub name: String,
    pub module: String,
    pub input_path: String,
    #[serde(with = "on_test_failure_wire")]
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
    /// Structural schemas for nested `DataWithSchema { type_name }` leaves in
    /// `semantics` (e.g. the state and event types of a state-machine trace).
    ///
    /// Pre-computed at export time so that `verify.rs` can emit per-type
    /// structural predicates for these leaves instead of the trivial
    /// `True` placeholder.
    #[serde(skip_serializing_if = "BTreeMap::is_empty")]
    #[serde(default)]
    pub inner_data_schemas: BTreeMap<String, ExportedDataSchema>,
    /// Concrete fuzzer inputs that produce a successful (non-erroring) test run,
    /// pre-computed at export time for tests whose universal theorem would be
    /// false or unprovable. Each entry is a CBOR-hex encoding of a `PlutusData`
    /// value produced by the fuzzer and accepted by the test body.
    ///
    /// Currently populated only for state-machine trace tests with
    /// `Void + FailImmediately` (the `proveTestsHalt` case) — there the
    /// reachability precondition over-approximates and the universal theorem
    /// is unsound; `verify.rs` emits `native_decide` instance theorems for
    /// each witness instead.
    #[serde(skip_serializing_if = "Vec::is_empty")]
    #[serde(default)]
    pub concrete_halt_witnesses: Vec<String>,
    /// Pre-emitted Lean text for the `isValidTransition` predicate body,
    /// computed at export time from the aiken-lang `TransitionProp` (which
    /// is not serializable). `None` when either no `TransitionProp` was
    /// produced for the test or the test is not a state-machine trace.
    ///
    /// The string is the full `def <name>_isValidTransition ...` block
    /// including any existential binders introduced during emission.
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub transition_prop_lean: Option<ExportedTransitionProp>,
    /// Concrete fuzzer inputs that produce an erroring test run, pre-computed
    /// at export time for state-machine trace tests with
    /// `Void + SucceedEventually` (the `proveTestsError` case, annotated with
    /// the `fail` attribute on the property). Each entry is a CBOR-hex
    /// encoding of a `PlutusData` value produced by the fuzzer and rejected
    /// by the test body.
    ///
    /// As with `concrete_halt_witnesses`, the universal theorem
    /// `∀ x, reachable x → proveTestsError prog (dataArg x)` is unsound for
    /// state-machine traces because `reachable` over-approximates the step
    /// function. `verify.rs` emits `native_decide` instance theorems for
    /// each witness instead.
    #[serde(skip_serializing_if = "Vec::is_empty")]
    #[serde(default)]
    pub concrete_error_witnesses: Vec<String>,
}

/// Schema version for `aiken export-tests` JSON output.
pub const EXPORT_TESTS_VERSION: &str = "1";

#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
#[non_exhaustive]
pub struct ExportedTests {
    pub version: String,
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
                    error: Box::new(error),
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
            error: Box::new(error),
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
    use super::{CheckedModules, Export, OnTestFailure};
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

    #[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize)]
    struct OnTestFailureWireWrapper {
        #[serde(with = "super::on_test_failure_wire")]
        on_test_failure: OnTestFailure,
    }

    #[test]
    fn on_test_failure_serializes_as_legacy_pascal_case() {
        let value = serde_json::to_value(OnTestFailureWireWrapper {
            on_test_failure: OnTestFailure::FailImmediately,
        })
        .expect("wrapper should serialize");

        assert_eq!(value["on_test_failure"], "FailImmediately");
    }

    #[test]
    fn on_test_failure_deserializes_legacy_pascal_case() {
        let wrapper: OnTestFailureWireWrapper = serde_json::from_value(serde_json::json!({
            "on_test_failure": "FailImmediately"
        }))
        .expect("legacy wire form should deserialize");

        assert_eq!(wrapper.on_test_failure, OnTestFailure::FailImmediately);
    }

    #[test]
    fn on_test_failure_deserializes_snake_case_for_compatibility() {
        let wrapper: OnTestFailureWireWrapper = serde_json::from_value(serde_json::json!({
            "on_test_failure": "fail_immediately"
        }))
        .expect("snake_case wire form should deserialize");

        assert_eq!(wrapper.on_test_failure, OnTestFailure::FailImmediately);
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
