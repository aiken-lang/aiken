//! Lossless models for the schemas pinned by the UAL consumer.
//!
//! Optional fields use `Option`, including collections: absent and explicitly
//! empty values have different serialized representations. Open schema objects
//! retain unknown members in `extensions`; closed objects reject them. Schema
//! validation remains responsible for patterns, cardinalities, conditional
//! requirements, and supported profiles. Deserializing is not validation or
//! verification. Value schemas are deliberately opaque to preserve the complete
//! extensible schema language, including annotations unknown to this crate.

use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};
use serde_json::Value;

pub const ASSURANCE_SCHEMA_ID: &str =
    "https://cips.cardano.org/cips/cipXXXX/schemas/assurance-v2.json";
pub const CHECKING_CONTEXT_SCHEMA_ID: &str =
    "https://cips.cardano.org/cips/cipXXXX/schemas/checking-context.json";
pub const COMPILED_INTERFACE_SCHEMA_ID: &str =
    "https://cips.cardano.org/cips/cip57/extensions/compiled-interface/v1/schema.json";
pub const VALUE_SCHEMA_ID: &str =
    "https://cips.cardano.org/cips/cip57/extensions/compiled-interface/v1/value-schema.json";
pub const BLUEPRINT_SCHEMA_ID: &str =
    "https://cips.cardano.org/cips/cip57/schemas/plutus-blueprint.json";
pub const UPLC_STEP_CHECK_PROFILE: &str =
    "https://cips.cardano.org/cips/cipXXXX/profiles/uplc-step-check/v1";
pub const CHECKING_ENVIRONMENT_FORMAT: &str = "blaster-loaded-environment-v1";

/// Unknown members of schema objects that permit additional properties.
pub type Extensions = BTreeMap<String, Value>;

/// Lowercase hexadecimal Blake2b-224 digest; schema validation checks its shape.
pub type ScriptHash = String;

/// Extensible compiled-interface value-schema language, retained without loss.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(transparent)]
pub struct ValueSchema(pub Value);

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Digest {
    pub alg: String,
    pub digest: String,
    #[serde(flatten)]
    pub extensions: Extensions,
}

impl Digest {
    pub fn new(alg: impl Into<String>, digest: impl Into<String>) -> Self {
        Self {
            alg: alg.into(),
            digest: digest.into(),
            extensions: Extensions::new(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Artifact {
    pub uri: String,
    pub hash: Digest,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub media_type: Option<String>,
    #[serde(flatten)]
    pub extensions: Extensions,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AssuranceDocument {
    #[serde(rename = "$schema")]
    pub schema: String,
    pub preamble: AssurancePreamble,
    pub blueprint: BlueprintReference,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub languages: Option<BTreeMap<String, RegistryEntry>>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub tools: Option<BTreeMap<String, RegistryEntry>>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub formal_fragments: Option<Vec<FormalFragment>>,
    pub properties: Vec<Property>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub checking_contexts: Option<BTreeMap<String, Artifact>>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub functions: Option<BTreeMap<String, Function>>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub definitions: Option<BTreeMap<String, ValueSchema>>,
    #[serde(flatten)]
    pub extensions: Extensions,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AssurancePreamble {
    pub title: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub version: Option<String>,
    pub authors: Vec<String>,
    /// Calendar date in the schema's YYYY-MM-DD representation.
    pub created: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub license: Option<String>,
    #[serde(flatten)]
    pub extensions: Extensions,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct BlueprintReference {
    pub uri: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub hash: Option<Digest>,
    #[serde(flatten)]
    pub extensions: Extensions,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct RegistryEntry {
    pub name: String,
    pub version: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub uri: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    #[serde(flatten)]
    pub extensions: Extensions,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct FormalFragment {
    pub id: String,
    pub language: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub imports: Option<Vec<String>>,
    pub source: String,
    #[serde(flatten)]
    pub extensions: Extensions,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Property {
    pub id: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub title: Option<String>,
    pub scope: PropertyScope,
    pub statement: Statement,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub assumptions: Option<Vec<Statement>>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub tags: Option<Vec<String>>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub evidence: Option<Vec<EvidenceRecord>>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub checking_context: Option<String>,
    #[serde(flatten)]
    pub extensions: Extensions,
}

#[derive(Clone, Debug, Default, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct PropertyScope {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub validators: Option<Vec<String>>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub functions: Option<Vec<String>>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Statement {
    pub text: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub formal: Option<FormalStatement>,
    #[serde(flatten)]
    pub extensions: Extensions,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct FormalStatement {
    pub language: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub source: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub uri: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub uses: Option<Vec<String>>,
    #[serde(flatten)]
    pub extensions: Extensions,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum EvidenceOutcome {
    Verified,
    Falsified,
    Partial,
    Inconclusive,
}

/// Historical evidence, embedded in the same assurance document as its claim.
/// This record is not a certificate and is not a trust-verification result.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct EvidenceRecord {
    /// Open vocabulary: known methods and tool-specific strings are preserved.
    pub method: String,
    pub verifier: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub tool: Option<String>,
    pub outcome: EvidenceOutcome,
    pub date: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub script_hash: Option<ScriptHash>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub artifact: Option<Artifact>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub notes: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub checking_context_hash: Option<Digest>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub function_hashes: Option<BTreeMap<String, Digest>>,
    #[serde(flatten)]
    pub extensions: Extensions,
}

impl EvidenceRecord {
    /// Starts an SMT-check record bound to the exact checking-context bytes.
    ///
    /// The caller supplies the reported outcome; this does not run a solver or
    /// verify evidence. Before schema validation, bind at least one target with
    /// `with_script_hash` or `with_function_hashes`. The containing property's
    /// scope may require both. An empty function-hash map is not schema-valid.
    pub fn smt_check(
        verifier: impl Into<String>,
        outcome: EvidenceOutcome,
        date: impl Into<String>,
        tool: impl Into<String>,
        artifact: Artifact,
        checking_context_hash: Digest,
    ) -> Self {
        Self {
            method: "smt-check".into(),
            verifier: verifier.into(),
            tool: Some(tool.into()),
            outcome,
            date: date.into(),
            script_hash: None,
            artifact: Some(artifact),
            notes: None,
            checking_context_hash: Some(checking_context_hash),
            function_hashes: None,
            extensions: Extensions::new(),
        }
    }

    pub fn with_script_hash(mut self, hash: ScriptHash) -> Self {
        self.script_hash = Some(hash);
        self
    }

    pub fn with_function_hashes(mut self, hashes: BTreeMap<String, Digest>) -> Self {
        self.function_hashes = Some(hashes);
        self
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum Serialization {
    #[serde(rename = "cbor-flat")]
    CborFlat,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum PlutusVersion {
    V1,
    V2,
    V3,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct Function {
    pub compiled_code: String,
    pub serialization: Serialization,
    pub hash: Digest,
    pub plutus_version: PlutusVersion,
    pub arguments: Vec<ValueSchema>,
    pub result: ValueSchema,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct CheckingContext {
    #[serde(rename = "$schema")]
    pub schema: String,
    pub profile: String,
    pub environment: Artifact,
    pub execution: Execution,
    pub targets: Vec<ContextTarget>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum SemanticsVariant {
    A,
    B,
    C,
    D,
    E,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum Acceptance {
    Evaluation,
    LedgerScript,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct Execution {
    pub semantics_variant: SemanticsVariant,
    pub budget: Budget,
    pub acceptance: Acceptance,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub protocol_version: Option<ProtocolVersion>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "kebab-case", deny_unknown_fields)]
pub enum Budget {
    CekSteps {
        steps: u64,
    },
    LedgerUnits {
        #[serde(rename = "exCPU")]
        ex_cpu: u64,
        #[serde(rename = "exMem")]
        ex_mem: u64,
        #[serde(rename = "costModel")]
        cost_model: Artifact,
    },
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ProtocolVersion {
    pub major: u64,
    pub minor: u64,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum ContextTarget {
    Validator(ValidatorTarget),
    Function(FunctionTarget),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Purpose {
    Spend,
    Mint,
    Withdraw,
    Publish,
    Vote,
    Propose,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ValidatorTarget {
    pub validator: String,
    pub purpose: Purpose,
    pub parameters: ParameterBinding,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(tag = "mode", rename_all = "lowercase", deny_unknown_fields)]
pub enum ParameterBinding {
    Universal {},
    Applied {
        values: Vec<AppliedParameter>,
        #[serde(rename = "appliedScriptHash")]
        applied_script_hash: ScriptHash,
        #[serde(rename = "appliedScript")]
        applied_script: Artifact,
    },
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct AppliedParameter {
    pub parameter: String,
    pub term: Artifact,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct FunctionTarget {
    pub function: String,
    pub function_hash: Digest,
    pub function_interface: FunctionInterface,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct FunctionInterface {
    pub serialization: Serialization,
    pub plutus_version: PlutusVersion,
    pub arguments: Vec<ValueSchema>,
    pub result: ValueSchema,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub definitions: Option<BTreeMap<String, ValueSchema>>,
}

/// A CIP-57 blueprint using the pinned compiled-interface extension.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct CompiledInterfaceBlueprint {
    #[serde(rename = "$schema")]
    pub schema: String,
    pub preamble: BlueprintPreamble,
    pub validators: Vec<CompiledValidator>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub definitions: Option<BTreeMap<String, ValueSchema>>,
    /// Known vocabulary entries must be true; unknown entries are unrestricted.
    #[serde(rename = "$vocabulary")]
    pub vocabulary: BTreeMap<String, Value>,
    #[serde(flatten)]
    pub extensions: Extensions,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct BlueprintPreamble {
    pub title: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    pub version: String,
    pub plutus_version: PlutusVersion,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub compiler: Option<Compiler>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub license: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Compiler {
    pub name: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub version: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CompiledValidator {
    pub id: String,
    pub title: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    pub redeemer: BlueprintArgument,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub datum: Option<BlueprintArgument>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub parameters: Option<Vec<ArgumentSchema>>,
    pub interface: CompiledInterface,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub compiled_code: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub hash: Option<ScriptHash>,
    #[serde(flatten)]
    pub extensions: Extensions,
}

/// A single schema or purpose-specific alternatives. Single-schema objects are
/// open; alternatives are closed, matching the pinned schema's `anyOf` order.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum BlueprintArgument {
    Single(ArgumentSchema),
    Alternatives(ArgumentAlternatives),
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct ArgumentAlternatives {
    pub one_of: Vec<ArgumentSchema>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct ArgumentSchema {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub title: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    pub schema: ValueSchema,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub purpose: Option<PurposeSelector>,
    #[serde(flatten)]
    pub extensions: Extensions,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum PurposeSelector {
    Single(Purpose),
    Alternatives(PurposeAlternatives),
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct PurposeAlternatives {
    pub one_of: Vec<Purpose>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum CallingConvention {
    LedgerV1,
    LedgerV2,
    LedgerV3,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct CompiledInterface {
    pub calling_convention: CallingConvention,
    pub invocations: Vec<Invocation>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Invocation {
    pub purpose: Purpose,
    pub arguments: Vec<InvocationArgument>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(tag = "role", rename_all = "lowercase", deny_unknown_fields)]
pub enum InvocationArgument {
    Parameter { source: String },
    Datum { source: String },
    Redeemer { source: String },
    Context {},
}

/// Captured by the pinned consumer's `captureCheckingEnvironment`.
/// Hash strings identify the actual compiler, solver, loaded modules and optional
/// native library bytes; they are not inferred from version labels.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct CheckingEnvironment {
    pub format: String,
    pub lean_version: String,
    pub compiler_sha256: String,
    pub solver_sha256: String,
    pub solver_options: SolverOptions,
    pub modules: BTreeMap<String, String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub native_blaster_sha256: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct SolverOptions {
    pub timeout_seconds: u64,
    pub max_rec_depth: u64,
    pub max_heartbeats: u64,
}
