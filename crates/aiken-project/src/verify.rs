//! Formal verification pipeline that discharges Aiken property tests as Lean 4
//! theorems proved via the Blaster SMT tactic and Z3.
//!
//! # Pipeline
//!
//! 1. **Fuzzer normalization.** Incoming [`ExportedPropertyTest`]s carry a
//!    [`FuzzerSemantics`](crate::export::FuzzerSemantics) tree describing the
//!    input domain of each test. Helpers in this module flatten those semantics
//!    into scalar, list, tuple, and state-machine preconditions that are safe to
//!    emit as Lean `Prop`s.
//! 2. **Lean workspace scaffolding.** [`generate_lean_workspace`] materializes
//!    a self-contained Lake project under `out_dir`: a `lakefile.lean` pinning
//!    Blaster and PlutusCore revisions, a `lean-toolchain`, `AikenVerify/Utils.lean`
//!    with the `proveTests` / CEK budget helpers, and CBOR payloads for every
//!    compiled UPLC program.
//! 3. **Proof generation.** Each test produces a Lean proof file under
//!    `AikenVerify/Proofs/`. `format_theorems` combines a normalized
//!    `TheoremForm` with the precondition set to produce theorems closed by
//!    `by blaster`, `by decide`, or an explicit witness; `admit`-backed
//!    fallbacks are used only when the spec permits it.
//! 4. **Lean execution orchestration.** [`run_proofs`] invokes
//!    `lake build` (and `lake exe`) per theorem with a timeout, process-tree
//!    termination, and pipe drainage. Per-test exit codes and `blaster` output
//!    are parsed into [`TheoremResult`]s with a classified [`FailureCategory`].
//! 5. **Result aggregation.** Individual [`TheoremResult`]s are folded into a
//!    [`VerifySummary`] that drives human-readable reporting and the JSON
//!    payload emitted by `aiken verify --json`.
//!
//! # Key types
//!
//! - [`VerifyConfig`]: caller-controlled inputs — output directory, Blaster /
//!   PlutusCore revisions, CEK budget, timeout, existential mode, artifact
//!   retention policy.
//! - [`VerifySummary`]: end-to-end structured outcome of a verification run.
//! - `GenerationError`: internal error emitted when a test cannot be lowered
//!   to a Lean theorem (carries a category used to decide whether to skip).
//! - [`DoctorReport`]: toolchain readiness report surfaced via
//!   `aiken verify doctor --json`.
//!
//! # External dependencies
//!
//! - **Lean 4** (`lean`, `lake`) — proof kernel and build system (minimum
//!   version [`MIN_LEAN_VERSION`]).
//! - **Blaster** ([`blaster`](https://github.com/galois-advanced-algorithms/lean-blaster))
//!   — Lean tactic that reifies a goal into SMT and discharges it with Z3.
//! - **PlutusCore** (Lean formalization) — dependency providing UPLC / CEK
//!   semantics that the generated theorems quantify over.
//! - **Z3** — SMT solver invoked by Blaster (minimum version
//!   [`MIN_Z3_VERSION`]).

use crate::blueprint::{
    definitions::Reference,
    schema::{Data as SchemaData, Declaration as SchemaDeclaration, Items as SchemaItems, Schema},
};
use crate::config::compiler_version;
use crate::export::{
    ExportedDataSchema, ExportedFuzzerStructure, ExportedMapperShape, ExportedPropertyTest,
    ExportedReplayWitness, ExportedSourceSpan, FuzzerConstraint, FuzzerExactValue,
    FuzzerOutputType, FuzzerSemantics, StateMachineAcceptance, StateMachineTransitionSemantics,
    TestReturnMode, TransitionWidening, TransitionWideningKind, VerificationTargetKind,
};
use aiken_lang::{ast::OnTestFailure, plutus_version::PlutusVersion};
use num_bigint::BigInt;
use pallas_crypto::hash::Hasher;
use pallas_traverse::ComputeHash;
use std::collections::HashMap;
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::io::Read;
use std::path::{Component, Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::{
    Mutex, OnceLock,
    atomic::{AtomicBool, Ordering},
};
use std::thread;
use std::time::{Duration, Instant};
use uplc::{
    BoundedBytes, Constr, MaybeIndefArray, PlutusData,
    machine::{
        runtime::{convert_constr_to_tag, convert_tag_to_constr},
        value::{Value as UplcRuntimeValue, to_pallas_bigint},
    },
};
use walkdir::WalkDir;

fn is_zero(n: &usize) -> bool {
    *n == 0
}

type TransitionWideningLog = Vec<TransitionWidening>;

fn push_widening(
    widenings: &mut TransitionWideningLog,
    kind: TransitionWideningKind,
    message: impl Into<String>,
) {
    widenings.push(TransitionWidening {
        kind,
        message: message.into(),
    });
}

#[cfg(test)]
fn widening_messages(widenings: &[TransitionWidening]) -> Vec<String> {
    widenings
        .iter()
        .map(|entry| entry.message.clone())
        .collect()
}

fn render_widening_counts(counts: &BTreeMap<TransitionWideningKind, usize>) -> String {
    let mut parts = Vec::new();
    for (kind, count) in counts {
        let label = match kind {
            TransitionWideningKind::Relation => "relation",
            TransitionWideningKind::DataFreshening => "data-freshening",
            TransitionWideningKind::BoolFreshening => "bool-freshening",
            TransitionWideningKind::Domain => "domain",
            TransitionWideningKind::OpaqueSubGenerator => "opaque-sub-generator",
        };
        parts.push(format!("{count} {label}"));
    }
    parts.join(", ")
}

fn widening_category_counts(
    widenings: &[TransitionWidening],
    include_sub_generators: bool,
    opaque_sub_generators: &[(String, String)],
) -> BTreeMap<TransitionWideningKind, usize> {
    let mut counts = BTreeMap::new();
    for widening in widenings {
        if matches!(widening.kind, TransitionWideningKind::OpaqueSubGenerator)
            && !include_sub_generators
        {
            continue;
        }
        *counts.entry(widening.kind).or_insert(0) += 1;
    }
    if include_sub_generators && !opaque_sub_generators.is_empty() {
        *counts
            .entry(TransitionWideningKind::OpaqueSubGenerator)
            .or_insert(0) += opaque_sub_generators.len();
    }
    counts
}

fn widening_summary_suffix(
    widenings: &[TransitionWidening],
    include_sub_generators: bool,
    opaque_sub_generators: &[(String, String)],
) -> Option<String> {
    let counts = widening_category_counts(widenings, include_sub_generators, opaque_sub_generators);
    if counts.is_empty() {
        return None;
    }

    let total: usize = counts.values().sum();
    Some(format!(
        "counted theorem widened ({} over-approximations: {})",
        total,
        render_widening_counts(&counts),
    ))
}

fn combined_widenings(
    primary: &[TransitionWidening],
    helper: &[TransitionWidening],
) -> Vec<TransitionWidening> {
    let mut combined = Vec::with_capacity(primary.len() + helper.len());
    combined.extend_from_slice(primary);
    combined.extend_from_slice(helper);
    combined
}

fn transition_prop_widenings(
    tp: &crate::export::ExportedTransitionProp,
) -> Vec<TransitionWidening> {
    let combined = combined_widenings(&tp.widenings, &tp.helper_widenings);
    if !combined.is_empty() || tp.unsupported_log.is_empty() {
        return combined;
    }

    tp.unsupported_log
        .iter()
        .map(|message| TransitionWidening {
            // Legacy export payloads only carried message strings. Preserve the
            // non-zero over-approximation accounting when reading them back.
            kind: TransitionWideningKind::Relation,
            message: message.clone(),
        })
        .collect()
}

fn domain_widening_is_production_disallowed(message: &str) -> bool {
    // Today every emitted Domain widening still drops semantic information that
    // matters to production honesty (`... widened to True`, or the equivalent
    // loss of element constraints for untyped list binders). Keep the predicate
    // message-based so future audited value-domain relaxations can opt in
    // explicitly without reopening the broad “all Domain widenings are harmless”
    // hole this check is closing.
    message.contains("widened to True") || message.contains("not a typed Lean list")
}

fn disallowed_production_widening_counts(
    widenings: &[TransitionWidening],
    allow_debug_sub_generators: bool,
    opaque_sub_generators: &[(String, String)],
) -> BTreeMap<TransitionWideningKind, usize> {
    let mut counts = BTreeMap::new();

    for widening in widenings {
        let disallowed = match widening.kind {
            TransitionWideningKind::Domain => {
                domain_widening_is_production_disallowed(&widening.message)
            }
            TransitionWideningKind::OpaqueSubGenerator => false,
            TransitionWideningKind::Relation
            | TransitionWideningKind::DataFreshening
            | TransitionWideningKind::BoolFreshening => true,
        };
        if disallowed {
            *counts.entry(widening.kind).or_insert(0) += 1;
        }
    }

    if !allow_debug_sub_generators && !opaque_sub_generators.is_empty() {
        *counts
            .entry(TransitionWideningKind::OpaqueSubGenerator)
            .or_insert(0) += opaque_sub_generators.len();
    }

    counts
}

/// Lean Unicode symbols used verbatim in generated proof output.
///
/// Using named constants keeps format strings consistent, makes it obvious
/// which characters are Lean syntax (as opposed to incidental Unicode in a
/// comment), and avoids mixing literal characters with `\u{...}` escapes.
mod lean_sym {
    /// Logical conjunction (U+2227).
    pub(super) const AND: &str = "\u{2227}";
    /// Logical disjunction (U+2228).
    pub(super) const OR: &str = "\u{2228}";
    /// Universal quantifier (U+2200).
    pub(super) const FORALL: &str = "\u{2200}";
    /// Existential quantifier (U+2203).
    pub(super) const EXISTS: &str = "\u{2203}";
    /// Right arrow / implication (U+2192).
    pub(super) const IMPLIES: &str = "\u{2192}";
    /// If and only if (U+2194).
    pub(super) const IFF: &str = "\u{2194}";
    /// Anonymous constructor opening bracket (U+27E8).
    pub(super) const ANGLE_OPEN: &str = "\u{27e8}";
    /// Anonymous constructor closing bracket (U+27E9).
    pub(super) const ANGLE_CLOSE: &str = "\u{27e9}";
    /// Cartesian product (U+00D7).
    pub(super) const CROSS: &str = "\u{00d7}";
}

#[cfg(unix)]
unsafe extern "C" {
    fn kill(pid: i32, sig: i32) -> i32;
}

#[cfg(unix)]
const SIGTERM: i32 = 15;
#[cfg(unix)]
const SIGKILL: i32 = 9;

#[cfg(unix)]
fn terminate_process_tree_by_pid(pid: u32) {
    if let Ok(raw_pid) = i32::try_from(pid) {
        // Negative PID targets the process group ID.
        unsafe {
            let _ = kill(-raw_pid, SIGTERM);
        }
        thread::sleep(Duration::from_millis(200));
        unsafe {
            let _ = kill(-raw_pid, SIGKILL);
        }
    }
}

#[cfg(windows)]
fn terminate_process_tree_by_pid(pid: u32) {
    // `taskkill /T` terminates the process and descendants in the same tree.
    let pid = pid.to_string();
    let _ = Command::new("taskkill")
        .args(["/PID", pid.as_str(), "/T", "/F"])
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status();
}

#[cfg(all(not(unix), not(windows)))]
fn terminate_process_tree_by_pid(_pid: u32) {}

fn terminate_child_process_tree(child: &mut std::process::Child) {
    terminate_process_tree_by_pid(child.id());
    let _ = child.kill();
}

#[derive(Default)]
struct VerifyInterruptCleanup {
    active_pid: Mutex<Option<u32>>,
    interrupted: AtomicBool,
}

struct ActiveProofProcessGuard {
    cleanup: &'static VerifyInterruptCleanup,
    pid: u32,
}

impl Drop for ActiveProofProcessGuard {
    fn drop(&mut self) {
        self.cleanup.clear_active_process(self.pid);
    }
}

impl VerifyInterruptCleanup {
    fn install() -> &'static Self {
        static CLEANUP: OnceLock<VerifyInterruptCleanup> = OnceLock::new();
        CLEANUP.get_or_init(|| {
            if let Err(err) = ctrlc::set_handler(|| {
                if let Some(cleanup) = CLEANUP.get() {
                    cleanup.request_interrupt_cleanup();
                }
            }) {
                eprintln!("warning: failed to install verify interrupt handler: {err}");
            }
            VerifyInterruptCleanup::default()
        })
    }

    fn begin_process(&'static self, pid: u32) -> ActiveProofProcessGuard {
        self.interrupted.store(false, Ordering::SeqCst);
        let mut active_pid = self.active_pid.lock().unwrap_or_else(|e| e.into_inner());
        *active_pid = Some(pid);
        ActiveProofProcessGuard { cleanup: self, pid }
    }

    fn clear_active_process(&self, pid: u32) {
        let mut active_pid = self.active_pid.lock().unwrap_or_else(|e| e.into_inner());
        if active_pid.is_some_and(|active| active == pid) {
            *active_pid = None;
        }
    }

    fn mark_interrupted(&self) {
        self.interrupted.store(true, Ordering::SeqCst);
    }

    fn request_interrupt_cleanup(&self) {
        self.mark_interrupted();
        let pid = {
            let active_pid = self.active_pid.lock().unwrap_or_else(|e| e.into_inner());
            *active_pid
        };
        if let Some(pid) = pid {
            terminate_process_tree_by_pid(pid);
        }
    }

    fn interrupted(&self) -> bool {
        self.interrupted.load(Ordering::SeqCst)
    }

    #[cfg(test)]
    fn active_pid_for_test(&self) -> Option<u32> {
        *self.active_pid.lock().unwrap_or_else(|e| e.into_inner())
    }

    #[cfg(test)]
    fn reset_for_test(&self) {
        self.interrupted.store(false, Ordering::SeqCst);
        *self.active_pid.lock().unwrap_or_else(|e| e.into_inner()) = None;
    }
}

/// Result of running proof verification
#[derive(Debug, serde::Serialize, serde::Deserialize)]
#[non_exhaustive]
pub struct VerifyResult {
    pub success: bool,
    pub stdout: CapturedOutput,
    pub stderr: CapturedOutput,
    pub exit_code: Option<i32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub theorem_results: Option<Vec<TheoremResult>>,
}

impl VerifyResult {
    pub fn new(
        success: bool,
        stdout: CapturedOutput,
        stderr: CapturedOutput,
        exit_code: Option<i32>,
        theorem_results: Option<Vec<TheoremResult>>,
    ) -> Self {
        Self {
            success,
            stdout,
            stderr,
            exit_code,
            theorem_results,
        }
    }
}

/// Default cap (in bytes) on the stdout/stderr tail retained inside the
/// `aiken verify --json` output. The default keeps a single CI artifact
/// well under typical 1 MiB attachment limits even when the proof run
/// emits a large amount of build output.
pub const RAW_OUTPUT_TAIL_BYTES: usize = 64 * 1024;

/// Hard upper bound on `--raw-output-bytes`. Selected so a single
/// `aiken verify` JSON document cannot exceed roughly the typical CI
/// artifact ceiling even when both stdout and stderr fill the cap.
pub const MAX_RAW_OUTPUT_TAIL_BYTES: usize = 16 * 1024 * 1024;

/// Base fuzzer seed used by the compiler when synthesizing the
/// `concrete_halt_witnesses` / `concrete_error_witnesses` lists. The
/// `i`th entry is produced by running the test's fuzzer with seed
/// `CONCRETE_WITNESS_BASE_SEED + i`. Surfaced in the per-witness
/// `WitnessProofNote::note` strings so users can correlate the verdict
/// with the on-disk Lean comment that names the seed.
pub(crate) const CONCRETE_WITNESS_BASE_SEED: u32 = 42;

/// Captured stdout/stderr from a long-running command. The `tail` field
/// is the last `RAW_OUTPUT_TAIL_BYTES` bytes (or the configured cap) of
/// the original stream as a UTF-8 string. When the stream exceeded the
/// cap, `truncated` is `true` and the full stream is persisted at
/// `log_path` (when a path was provided to the constructor). Otherwise
/// `tail` contains the entire stream.
#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
#[non_exhaustive]
pub struct CapturedOutput {
    pub tail: String,
    pub total_bytes: usize,
    pub truncated: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub log_path: Option<PathBuf>,
}

impl CapturedOutput {
    /// Build a `CapturedOutput` from a UTF-8 string of arbitrary length.
    /// When `cap == 0`, truncation is disabled and the entire stream is
    /// retained. Otherwise the last `cap` bytes are kept in `tail`,
    /// trimmed at a UTF-8 character boundary so the result is always
    /// valid UTF-8. When the stream exceeded `cap` and `log_path` is
    /// `Some`, the FULL stream is persisted to that path; on persistence
    /// failure, an `eprintln!` is emitted and `log_path` is reset to
    /// `None` so the verify run still completes.
    pub fn from_string(content: String, cap: usize, log_path: Option<PathBuf>) -> Self {
        Self::from_string_with_paths(content, cap, log_path.clone(), log_path)
    }

    /// Variant of [`CapturedOutput::from_string`] that separates the on-disk
    /// persistence path from the path reported on the wire.
    pub fn from_string_with_paths(
        content: String,
        cap: usize,
        persist_path: Option<PathBuf>,
        reported_path: Option<PathBuf>,
    ) -> Self {
        let total_bytes = content.len();

        if cap == 0 || total_bytes <= cap {
            return CapturedOutput {
                tail: content,
                total_bytes,
                truncated: false,
                log_path: None,
            };
        }

        // Find the rightmost UTF-8 char boundary at or before `total_bytes - cap`
        // so the suffix beginning at that boundary is valid UTF-8.
        let mut start = total_bytes - cap;
        while start < total_bytes && !content.is_char_boundary(start) {
            start += 1;
        }

        let tail = content[start..].to_string();

        let persisted_path = match persist_path {
            Some(path) => match Self::persist_full_stream(&path, &content) {
                Ok(()) => reported_path.or(Some(path)),
                Err(err) => {
                    eprintln!(
                        "warning: failed to persist truncated raw output to {}: {err}",
                        path.display()
                    );
                    None
                }
            },
            None => None,
        };

        CapturedOutput {
            tail,
            total_bytes,
            truncated: true,
            log_path: persisted_path,
        }
    }

    /// Convenience constructor for unit-test paths and small synthetic
    /// streams. Equivalent to `from_string(s.into(), 0, None)`: keeps
    /// the entire input, never truncates, never writes a log.
    pub fn small(s: impl Into<String>) -> Self {
        Self::from_string(s.into(), 0, None)
    }

    pub fn new(
        tail: String,
        total_bytes: usize,
        truncated: bool,
        log_path: Option<PathBuf>,
    ) -> Self {
        Self {
            tail,
            total_bytes,
            truncated,
            log_path,
        }
    }

    fn persist_full_stream(path: &Path, content: &str) -> std::io::Result<()> {
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)?;
        }
        fs::write(path, content)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[non_exhaustive]
pub struct VerificationArtifacts {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub manifest: Option<PathBuf>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub lean_root: Option<PathBuf>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub logs: Option<PathBuf>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub smt2: Vec<PathBuf>,
}

impl VerificationArtifacts {
    pub fn is_empty(&self) -> bool {
        self.manifest.is_none()
            && self.lean_root.is_none()
            && self.logs.is_none()
            && self.smt2.is_empty()
    }
}

fn collect_smt2_artifacts(out_dir: &Path) -> Vec<PathBuf> {
    fn skip_dir(path: &Path) -> bool {
        matches!(
            path.file_name().and_then(|name| name.to_str()),
            Some(".git")
        ) || (matches!(
            path.file_name().and_then(|name| name.to_str()),
            Some("packages")
        ) && path
            .parent()
            .and_then(|parent| parent.file_name())
            .and_then(|name| name.to_str())
            == Some(".lake"))
    }

    let mut pending = vec![out_dir.to_path_buf()];
    let mut found = BTreeSet::new();

    while let Some(dir) = pending.pop() {
        let entries = match fs::read_dir(&dir) {
            Ok(entries) => entries,
            Err(_) => continue,
        };

        for entry in entries.flatten() {
            let path = entry.path();
            let file_type = match entry.file_type() {
                Ok(file_type) => file_type,
                Err(_) => continue,
            };

            if file_type.is_dir() {
                if !skip_dir(&path) {
                    pending.push(path);
                }
                continue;
            }

            if file_type.is_file() && path.extension().and_then(|ext| ext.to_str()) == Some("smt2")
            {
                found.insert(path);
            }
        }
    }

    found.into_iter().collect()
}

pub fn collect_verification_artifacts(out_dir: &Path) -> VerificationArtifacts {
    let manifest = out_dir.join("manifest.json");
    let lean_root = out_dir.join("AikenVerify");
    let logs = out_dir.join("logs");

    VerificationArtifacts {
        manifest: manifest.exists().then_some(manifest),
        lean_root: lean_root.exists().then_some(lean_root),
        logs: logs.exists().then_some(logs),
        smt2: collect_smt2_artifacts(out_dir),
    }
}
/// Schema version for `aiken verify capabilities --json` output.
pub const VERIFICATION_CAPABILITIES_VERSION: &str = "3";
pub(crate) const MAX_FINITE_THEOREM_INSTANCES_PER_TEST: usize = 64;

/// Schema version for `aiken verify doctor --json` output.
pub const DOCTOR_REPORT_VERSION: &str = "1";

/// Verification capabilities report: documents what the integration does and
/// does not support. Surfaced via `aiken verify doctor --json` and
/// `aiken verify capabilities --json` so that no limitation remains implicit.
///
/// The first six fields document the legacy doctor surface (test kinds,
/// fuzzer types, target/existential modes). The trailing `supported` /
/// `unsupported` fields mirror the catalogue-driven shape published by
/// `aiken verify capabilities` and are kept additive so doctor JSON consumers
/// keep working.
#[derive(Debug, Clone, serde::Serialize)]
#[non_exhaustive]
pub struct VerificationCapabilities {
    /// Schema version for this capabilities JSON document.
    pub version: String,
    /// Test kinds that can be verified
    pub supported_test_kinds: Vec<String>,
    /// Test kinds excluded by design
    pub unsupported_test_kinds: Vec<CapabilityNote>,
    /// Verification target modes
    pub target_modes: Vec<String>,
    /// Fuzzer output types that can generate proofs
    pub supported_fuzzer_types: Vec<String>,
    /// Fuzzer output types that are not yet supported
    pub unsupported_fuzzer_types: Vec<String>,
    /// Supported existential modes
    pub existential_modes: Vec<String>,
    /// Max test arity (number of fuzzer arguments)
    pub max_test_arity: usize,
    /// High-level supported test kinds, matching the
    /// `aiken verify capabilities --json` `supported` field. Today this is the
    /// single string `"property"`; broader test kinds will arrive in future
    /// releases.
    #[serde(default)]
    pub supported: Vec<String>,
    /// Catalogue-driven listing of the stable error and caveat codes currently
    /// surfaced by `aiken verify`, projected into the JSON-stable
    /// [`error_catalogue::CatalogueCode`] shape. Version 2 narrows this field
    /// to the subset the verifier can actually emit at runtime today; dormant
    /// catalogue entries stay internal.
    #[serde(default)]
    pub unsupported: Vec<error_catalogue::CatalogueCode>,
}

/// A note about an intentionally unsupported capability.
#[derive(Debug, Clone, serde::Serialize)]
#[non_exhaustive]
pub struct CapabilityNote {
    pub kind: String,
    pub status: String,
    pub reason: String,
}

/// Return the current verification capabilities.
///
/// The `supported` / `unsupported` fields are derived from the static
/// verifier catalogue, filtered to the codes the current runtime can actually
/// surface. This filtering is the version-2 contract for the JSON payload.
pub fn capabilities() -> VerificationCapabilities {
    VerificationCapabilities {
        version: VERIFICATION_CAPABILITIES_VERSION.to_string(),
        supported_test_kinds: vec!["property".to_string()],
        unsupported_test_kinds: vec![
            CapabilityNote {
                kind: "unit".to_string(),
                status: "intentional".to_string(),
                reason: "Unit tests have no fuzzer input domain; formal verification \
                         requires universally quantified properties."
                    .to_string(),
            },
            CapabilityNote {
                kind: "benchmark".to_string(),
                status: "intentional".to_string(),
                reason: "Benchmarks measure performance, not correctness; they have \
                         no boolean property to verify."
                    .to_string(),
            },
        ],
        target_modes: vec![
            "property".to_string(),
            "validator (requires validator metadata)".to_string(),
            "equivalence (requires validator metadata)".to_string(),
        ],
        supported_fuzzer_types: vec![
            "Int".to_string(),
            "Bool".to_string(),
            "ByteArray".to_string(),
            "String".to_string(),
            "Data".to_string(),
            "List<T>".to_string(),
            "Tuple(T, ...)".to_string(),
            "Pair(T, T)".to_string(),
            "Finite literal String domains (Exact/OneOf)".to_string(),
            "Bounded top-level List<Bool> domains within finite enumeration cap".to_string(),
            "Bounded scalar ByteArray length ranges".to_string(),
            "Finite nullary ADT constructor domains (Data.Constr tag [])".to_string(),
        ],
        unsupported_fuzzer_types: vec![
            "Blaster translation gaps for some generated Lean predicates (e.g. List.Mem)"
                .to_string(),
            "Test arity > 1 is not supported directly; multi-input properties must be tuple/record encoded."
                .to_string(),
            "General higher-order/partial-application fuzzer resolver coverage is incomplete; unresolved shapes require extractor improvements."
                .to_string(),
            "Unbounded String/ByteArray content and finite literal ByteArray domains remain unsupported."
                .to_string(),
        ],
        existential_modes: vec!["witness".to_string(), "proof".to_string()],
        max_test_arity: 1,
        supported: vec!["property".to_string()],
        unsupported: error_catalogue::iter_catalogue().collect(),
    }
}

/// Category of a proof failure, enabling structured diagnostics.
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum FailureCategory {
    /// Blaster found a counterexample disproving the theorem
    Counterexample,
    /// Tactic failed to close all goals (e.g. `blaster` tactic failed, unsolved goals)
    UnsatGoal,
    /// Proof execution exceeded the time budget
    Timeout,
    /// Lean build system error (compilation, syntax, import)
    BuildError,
    /// Dependency resolution or fetch failed
    DependencyError,
    /// Blaster does not yet support translating the generated Lean construct
    BlasterUnsupported,
    /// Could not classify the failure
    Unknown,
}

/// Truthful verification status for a single theorem result.
#[non_exhaustive]
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "PascalCase")]
pub enum VerificationStatus {
    /// Lean's kernel accepted a reconstructed proof with no placeholders or unreconstructed solver gap.
    LeanCertified,
    /// Blaster/Z3 reported Valid, but no Lean-kernel proof reconstruction was available or used.
    SolverValidated,
    /// The solver found a counterexample for the generated theorem.
    SolverFalsified,
    /// A concrete generated input/witness replay was validated for the intended path.
    WitnessValidated,
    /// A sound artifact exists, but one or more proof obligations remain open.
    Partial,
    /// The verifier could not prove, refute, or classify the theorem precisely.
    Unknown,
    /// Verification exceeded the configured time/fuel limit.
    TimedOut,
    /// The integration could not generate or run a sound verification problem for this theorem.
    Unsupported,
}

impl VerificationStatus {
    fn from_proof_status(status: &ProofStatus) -> Self {
        match status {
            // The current generated universal/equivalence/termination proofs are Blaster-backed.
            // Until proof reconstruction is wired through, a successful build means solver
            // validation, not Lean kernel certification.
            ProofStatus::Proved => VerificationStatus::SolverValidated,
            ProofStatus::WitnessProved { .. } => VerificationStatus::WitnessValidated,
            ProofStatus::Partial { .. } => VerificationStatus::Partial,
            ProofStatus::Failed { category, .. } => match category {
                FailureCategory::Counterexample => VerificationStatus::SolverFalsified,
                FailureCategory::BlasterUnsupported => VerificationStatus::Unsupported,
                FailureCategory::Timeout => VerificationStatus::TimedOut,
                FailureCategory::UnsatGoal
                | FailureCategory::BuildError
                | FailureCategory::DependencyError
                | FailureCategory::Unknown => VerificationStatus::Unknown,
            },
            ProofStatus::TimedOut { .. } => VerificationStatus::TimedOut,
            ProofStatus::Unknown => VerificationStatus::Unknown,
        }
    }

    fn default_proof_status(self) -> ProofStatus {
        match self {
            VerificationStatus::LeanCertified | VerificationStatus::SolverValidated => {
                ProofStatus::Proved
            }
            VerificationStatus::SolverFalsified => ProofStatus::Failed {
                category: FailureCategory::Counterexample,
                reason: String::new(),
            },
            VerificationStatus::WitnessValidated => ProofStatus::WitnessProved {
                instances: 0,
                witnesses: Vec::new(),
                note: String::new(),
            },
            VerificationStatus::Partial => ProofStatus::Partial {
                note: String::new(),
            },
            VerificationStatus::Unknown => ProofStatus::Unknown,
            VerificationStatus::TimedOut => ProofStatus::TimedOut {
                reason: String::new(),
            },
            VerificationStatus::Unsupported => ProofStatus::Failed {
                category: FailureCategory::BlasterUnsupported,
                reason: String::new(),
            },
        }
    }
}

/// Evidence class for the reported verification status.
#[non_exhaustive]
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Certification {
    /// SMT/Blaster reported Valid, but Lean proof reconstruction was not used.
    SmtValidNoProofReconstruction,
    /// Lean's kernel checked a reconstructed proof with no placeholder gap.
    LeanKernelChecked,
    /// Concrete witness replay/native evaluation supplied the validation evidence.
    WitnessReplay,
    /// One or more proof obligations remain open (e.g. generated `sorry`/placeholder).
    OpenObligations,
    /// The theorem shape or integration path is unsupported.
    Unsupported,
    /// Verification timed out.
    Timeout,
    /// No stronger evidence class is known.
    Unknown,
}

impl Certification {
    fn from_proof_status(status: &ProofStatus) -> Self {
        match status {
            ProofStatus::Proved => Certification::SmtValidNoProofReconstruction,
            ProofStatus::WitnessProved { .. } => Certification::WitnessReplay,
            ProofStatus::Partial { .. } => Certification::OpenObligations,
            ProofStatus::Failed { category, .. } => match category {
                FailureCategory::Counterexample => Certification::SmtValidNoProofReconstruction,
                FailureCategory::BlasterUnsupported => Certification::Unsupported,
                FailureCategory::Timeout => Certification::Timeout,
                FailureCategory::UnsatGoal
                | FailureCategory::BuildError
                | FailureCategory::DependencyError
                | FailureCategory::Unknown => Certification::Unknown,
            },
            ProofStatus::TimedOut { .. } => Certification::Timeout,
            ProofStatus::Unknown => Certification::Unknown,
        }
    }

    fn from_status(status: VerificationStatus) -> Self {
        match status {
            VerificationStatus::LeanCertified => Certification::LeanKernelChecked,
            VerificationStatus::SolverValidated | VerificationStatus::SolverFalsified => {
                Certification::SmtValidNoProofReconstruction
            }
            VerificationStatus::WitnessValidated => Certification::WitnessReplay,
            VerificationStatus::Partial => Certification::OpenObligations,
            VerificationStatus::Unknown => Certification::Unknown,
            VerificationStatus::TimedOut => Certification::Timeout,
            VerificationStatus::Unsupported => Certification::Unsupported,
        }
    }
}

#[non_exhaustive]
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum TrustProfile {
    Strict,
    #[default]
    Production,
    Experimental,
    UnsafeDev,
}

impl std::fmt::Display for TrustProfile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            TrustProfile::Strict => "strict",
            TrustProfile::Production => "production",
            TrustProfile::Experimental => "experimental",
            TrustProfile::UnsafeDev => "unsafe-dev",
        })
    }
}

impl TrustProfile {
    fn rank(self) -> u8 {
        match self {
            TrustProfile::Strict => 0,
            TrustProfile::Production => 1,
            TrustProfile::Experimental => 2,
            TrustProfile::UnsafeDev => 3,
        }
    }

    fn allows_only_under(self, minimum: TrustProfile) -> bool {
        self.rank() >= minimum.rank()
    }
}
/// Structured verification summary
#[derive(Debug, serde::Serialize, serde::Deserialize)]
#[non_exhaustive]
pub struct VerifySummary {
    pub total: usize,
    pub proved: usize,
    /// Theorems whose build succeeded but whose generated Lean proof contains
    /// `sorry` (or otherwise leaves a sub-obligation open). Partial results do
    /// NOT count as failures and do NOT make `command_success` false.
    #[serde(default)]
    pub partial: usize,
    /// Theorems whose Lean build succeeded but whose proof only covers concrete
    /// witness instance(s) — not the universal claim. Surfaces existential /
    /// witness-only verdicts (e.g. `native_decide` halt/error proofs).
    #[serde(default)]
    pub witness: usize,
    pub failed: usize,
    pub timed_out: usize,
    pub unknown: usize,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub skipped: Vec<SkippedTest>,
    pub theorems: Vec<TheoremResult>,
    pub raw_output: VerifyResult,
    #[serde(default, skip_serializing_if = "VerificationArtifacts::is_empty")]
    pub artifacts: VerificationArtifacts,
    /// Wall-clock milliseconds spent running proofs (dependency sync + lake build).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub elapsed_ms: Option<u64>,
    /// Final command success status. Reflects both proof results and skip-induced
    /// failures so that JSON consumers can determine the exit status without
    /// inspecting the process exit code.
    #[serde(default)]
    pub command_success: bool,
    /// Blaster revision used to generate the Lake workspace. Surfaces the
    /// pinned (or `--blaster-rev`-overridden) git rev so JSON consumers can
    /// audit which dependency snapshot the proofs were verified against.
    #[serde(default)]
    pub blaster_rev: String,
    /// PlutusCore revision used to generate the Lake workspace. Surfaces the
    /// pinned (or `--plutus-core-rev`-overridden) git rev so JSON consumers
    /// can audit which dependency snapshot the proofs were verified against.
    #[serde(default)]
    pub plutus_core_rev: String,
    /// Whether `--allow-vacuous-subgenerators` was enabled for this run.
    #[serde(default)]
    pub allow_vacuous_subgenerators: bool,
    /// Whether `AIKEN_EMIT_TWO_PHASE=0` disabled two-phase proof generation for this run.
    #[serde(default)]
    pub two_phase_disabled: bool,
    /// Schema version for `aiken verify --json` output. Bumped when the
    /// JSON shape changes in a backward-incompatible way.
    pub verify_summary_version: &'static str,
}
impl VerifySummary {
    pub fn no_proofs(
        manifest: &GeneratedManifest,
        skipped_without_allow: bool,
        blaster_rev: &str,
        plutus_core_rev: &str,
        allow_vacuous_subgenerators: bool,
        two_phase_disabled: bool,
    ) -> Self {
        Self::new(
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            manifest.skipped.clone(),
            Vec::new(),
            VerifyResult::new(
                !skipped_without_allow,
                CapturedOutput::small(""),
                CapturedOutput::small(""),
                Some(if skipped_without_allow { 1 } else { 0 }),
                None,
            ),
            VerificationArtifacts::default(),
            None,
            !skipped_without_allow,
            blaster_rev.to_string(),
            plutus_core_rev.to_string(),
            allow_vacuous_subgenerators,
            two_phase_disabled,
        )
    }

    #[allow(clippy::too_many_arguments)]
    pub fn new(
        total: usize,
        proved: usize,
        partial: usize,
        witness: usize,
        failed: usize,
        timed_out: usize,
        unknown: usize,
        skipped: Vec<SkippedTest>,
        theorems: Vec<TheoremResult>,
        raw_output: VerifyResult,
        artifacts: VerificationArtifacts,
        elapsed_ms: Option<u64>,
        command_success: bool,
        blaster_rev: String,
        plutus_core_rev: String,
        allow_vacuous_subgenerators: bool,
        two_phase_disabled: bool,
    ) -> Self {
        Self {
            total,
            proved,
            partial,
            witness,
            failed,
            timed_out,
            unknown,
            skipped,
            theorems,
            raw_output,
            artifacts,
            elapsed_ms,
            command_success,
            blaster_rev,
            plutus_core_rev,
            allow_vacuous_subgenerators,
            two_phase_disabled,
            verify_summary_version: VERIFY_SUMMARY_VERSION,
        }
    }
}

fn default_trust_profile() -> TrustProfile {
    TrustProfile::default()
}

#[non_exhaustive]
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "PascalCase")]
pub enum DomainPrecision {
    Exact,
    OverApprox,
    UnderApprox,
    WitnessOnly,
    Unknown,
}

impl std::fmt::Display for DomainPrecision {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            DomainPrecision::Exact => "Exact",
            DomainPrecision::OverApprox => "OverApprox",
            DomainPrecision::UnderApprox => "UnderApprox",
            DomainPrecision::WitnessOnly => "WitnessOnly",
            DomainPrecision::Unknown => "Unknown",
        })
    }
}

#[non_exhaustive]
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "PascalCase")]
pub enum DomainCertificate {
    LeanProved,
    TrustedVersionedModel,
    SamplerSemanticModel,
    WitnessReplay,
    DifferentialTestedOnly,
    Unchecked,
}

impl std::fmt::Display for DomainCertificate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            DomainCertificate::LeanProved => "LeanProved",
            DomainCertificate::TrustedVersionedModel => "TrustedVersionedModel",
            DomainCertificate::SamplerSemanticModel => "SamplerSemanticModel",
            DomainCertificate::WitnessReplay => "WitnessReplay",
            DomainCertificate::DifferentialTestedOnly => "DifferentialTestedOnly",
            DomainCertificate::Unchecked => "Unchecked",
        })
    }
}

#[non_exhaustive]
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "PascalCase")]
pub enum DomainObligation {
    FuzzerReturnsImpliesDomain,
    DomainImpliesFuzzerReturns,
    DomainIffFuzzerReturns,
    WitnessReplaysThroughFuzzer,
    WitnessSatisfiesDomain,
    FuzzerModelHashMatches,
    ValueDecoderRoundTrip,
    FuzzerOutputTypeMatchesPropertyInputType,
    PropertyHarnessAcceptsDecodedInput,
    PropertyHarnessMatchesExportedUPLC,
}

impl std::fmt::Display for DomainObligation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            DomainObligation::FuzzerReturnsImpliesDomain => "FuzzerReturnsImpliesDomain",
            DomainObligation::DomainImpliesFuzzerReturns => "DomainImpliesFuzzerReturns",
            DomainObligation::DomainIffFuzzerReturns => "DomainIffFuzzerReturns",
            DomainObligation::WitnessReplaysThroughFuzzer => "WitnessReplaysThroughFuzzer",
            DomainObligation::WitnessSatisfiesDomain => "WitnessSatisfiesDomain",
            DomainObligation::FuzzerModelHashMatches => "FuzzerModelHashMatches",
            DomainObligation::ValueDecoderRoundTrip => "ValueDecoderRoundTrip",
            DomainObligation::FuzzerOutputTypeMatchesPropertyInputType => {
                "FuzzerOutputTypeMatchesPropertyInputType"
            }
            DomainObligation::PropertyHarnessAcceptsDecodedInput => {
                "PropertyHarnessAcceptsDecodedInput"
            }
            DomainObligation::PropertyHarnessMatchesExportedUPLC => {
                "PropertyHarnessMatchesExportedUPLC"
            }
        })
    }
}

#[non_exhaustive]
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum RuntimeValueEncoding {
    BoolConstant,
    IntegerConstant,
    ByteStringConstant,
    StringConstant,
    DataConstant,
    ConstructorValue,
}

#[non_exhaustive]
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum PlutusDataEncoding {
    BoolConstructor,
    Integer,
    Bytes,
    Utf8Bytes,
    RawData,
    List,
    PairAsList,
    TupleAsList,
    ConstructorTagged,
}

#[non_exhaustive]
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum LeanValueEncoding {
    Bool,
    Integer,
    ByteString,
    Data,
    List,
    Tuple,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[non_exhaustive]
pub struct InputTypeField {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
    pub schema: InputTypeSchema,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[non_exhaustive]
pub struct InputTypeConstructor {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
    pub tag: u64,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub fields: Vec<InputTypeField>,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
#[non_exhaustive]
pub enum InputTypeSchema {
    Bool,
    Int,
    ByteArray,
    String,
    Data,
    List {
        element: Box<InputTypeSchema>,
    },
    Pair {
        first: Box<InputTypeSchema>,
        second: Box<InputTypeSchema>,
    },
    Tuple {
        elements: Vec<InputTypeSchema>,
    },
    ConstructorTagged {
        type_name: String,
        constructors: Vec<InputTypeConstructor>,
    },
    RawDataFallback {
        type_name: String,
        reason: String,
    },
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[non_exhaustive]
pub struct InputValueBridge {
    pub aiken_type: String,
    pub runtime_encoding: RuntimeValueEncoding,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub data_encoding: Option<PlutusDataEncoding>,
    pub lean_encoding: LeanValueEncoding,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub schema_hash: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub schema: Option<InputTypeSchema>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub diagnostics: Vec<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[non_exhaustive]
#[serde(rename_all = "snake_case")]
pub enum SamplerRunKind {
    SeededGeneration,
    ReplayOnly,
    WitnessReplay,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[non_exhaustive]
#[serde(rename_all = "snake_case")]
pub enum SamplerPrngMode {
    Seeded,
    Replayed,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[non_exhaustive]
pub struct SamplerMetadata {
    pub harness_abi: String,
    pub run_kind: SamplerRunKind,
    pub prng_mode: SamplerPrngMode,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub generation_seed: Option<u32>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub replay_choices_hex: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub decode_policy: Option<ManifestDecodePolicy>,
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[non_exhaustive]
pub struct Widening {
    pub kind: String,
    pub message: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub allowed_only_under: Option<TrustProfile>,
}

#[non_exhaustive]
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum CompatibilityMode {
    UniversalSuccess,
    UniversalFailure,
    SemanticExistential,
    WitnessCheck,
}

#[non_exhaustive]
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum DomainDiagnosticSeverity {
    Note,
    Warning,
    Error,
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[non_exhaustive]
pub struct DomainDiagnostic {
    pub code: String,
    pub severity: DomainDiagnosticSeverity,
    pub message: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub source_path: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub source_span: Option<ExportedSourceSpan>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub mode: Option<CompatibilityMode>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub trust_profile: Option<TrustProfile>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub obligations: Vec<DomainObligation>,
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[non_exhaustive]
pub struct ScenarioTraceBounds {
    pub min: usize,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub max: Option<usize>,
}

impl ScenarioTraceBounds {
    fn render(&self) -> String {
        match self.max {
            Some(max) if max == self.min => self.min.to_string(),
            Some(max) => format!("{}..={}", self.min, max),
            None => format!("{}..", self.min),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[non_exhaustive]
#[serde(rename_all = "snake_case")]
pub enum ScenarioSymbolicEncoding {
    BoundedBooleanChecker,
    WitnessReplayOnly,
    Unavailable,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[non_exhaustive]
#[serde(rename_all = "snake_case")]
pub enum ScenarioPredicateStatus {
    Available,
    OpenObligation,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[non_exhaustive]
pub struct ScenarioTraceIr {
    pub acceptance: StateMachineAcceptance,
    pub trace_length: ScenarioTraceBounds,
    pub step_depth: ScenarioTraceBounds,
    pub symbolic_encoding: ScenarioSymbolicEncoding,
    #[serde(default)]
    pub initial_state_anchored: bool,
    #[serde(default)]
    pub has_transition_prop: bool,
    #[serde(default)]
    pub has_step_function_ir: bool,
    #[serde(default)]
    pub transition_relation_exact: bool,
    #[serde(default)]
    pub uses_global_reachability_over_approx: bool,
    #[serde(default)]
    pub witness_replay_available: bool,
    pub script_context_predicate: ScenarioPredicateStatus,
    pub ledger_validity_predicate: ScenarioPredicateStatus,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub open_obligations: Vec<String>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub blockers: Vec<String>,
}

fn render_scenario_predicate_status(status: ScenarioPredicateStatus) -> &'static str {
    match status {
        ScenarioPredicateStatus::Available => "available",
        ScenarioPredicateStatus::OpenObligation => "open_obligation",
    }
}

fn render_scenario_obligations(items: &[String]) -> String {
    items.join(", ")
}

fn render_domain_obligations(obligations: &[DomainObligation]) -> String {
    obligations
        .iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join(", ")
}

fn legacy_domain_placeholder_reason() -> String {
    "Domain metadata unavailable; synthesized conservative placeholder.".to_string()
}

fn default_domain_relation() -> DomainRel {
    DomainRel::Unknown {
        reason: legacy_domain_placeholder_reason(),
    }
}

#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum DomainRel {
    Constraint {
        constraint: FuzzerConstraint,
    },
    Semantics {
        semantics: FuzzerSemantics,
    },
    Image {
        output_type: FuzzerOutputType,
        mapper_shape: ExportedMapperShape,
        sources: Vec<DomainRel>,
    },
    Bind {
        source: Box<DomainRel>,
        result: Box<DomainRel>,
    },
    Product {
        elements: Vec<DomainRel>,
    },
    List {
        element: Box<DomainRel>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        min_len: Option<usize>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        max_len: Option<usize>,
        #[serde(default)]
        unique: bool,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        retry_limit: Option<usize>,
    },
    Choice {
        output_type: FuzzerOutputType,
        branches: Vec<DomainRel>,
        #[serde(default)]
        may_fail: bool,
        #[serde(default)]
        non_empty_required: bool,
    },
    Filter {
        output_type: FuzzerOutputType,
        source: Box<DomainRel>,
        predicate_summary: String,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        max_tries: Option<usize>,
        #[serde(default)]
        impossible: bool,
    },
    Exists {
        var: String,
        body: Box<DomainRel>,
    },
    And {
        items: Vec<DomainRel>,
    },
    Or {
        items: Vec<DomainRel>,
    },
    Not {
        body: Box<DomainRel>,
    },
    Witness {
        encoded_values: Vec<String>,
    },
    SamplerReturns {
        summary: String,
    },
    TrueWithExplicitWidening {
        reason: String,
        allowed_only_under: TrustProfile,
    },
    Unknown {
        reason: String,
    },
}

impl DomainRel {
    fn allowed_only_under(&self) -> Option<TrustProfile> {
        match self {
            DomainRel::TrueWithExplicitWidening {
                allowed_only_under, ..
            } => Some(*allowed_only_under),
            DomainRel::Exists { body, .. } | DomainRel::Not { body } => body.allowed_only_under(),
            DomainRel::Bind { source, result } => {
                [source.allowed_only_under(), result.allowed_only_under()]
                    .into_iter()
                    .flatten()
                    .max_by_key(|profile| profile.rank())
            }
            DomainRel::And { items }
            | DomainRel::Or { items }
            | DomainRel::Product { elements: items }
            | DomainRel::Choice {
                branches: items, ..
            }
            | DomainRel::Image { sources: items, .. } => items
                .iter()
                .filter_map(|item| item.allowed_only_under())
                .max_by_key(|profile| profile.rank()),
            DomainRel::List { element, .. }
            | DomainRel::Filter {
                source: element, ..
            } => element.allowed_only_under(),
            DomainRel::Constraint { .. }
            | DomainRel::Semantics { .. }
            | DomainRel::Witness { .. }
            | DomainRel::SamplerReturns { .. }
            | DomainRel::Unknown { .. } => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[non_exhaustive]
pub struct LoweredDomain {
    #[serde(default = "default_domain_relation")]
    pub relation: DomainRel,
    pub precision: DomainPrecision,
    pub certificate: DomainCertificate,
    #[serde(default)]
    pub obligations_open: Vec<DomainObligation>,
    #[serde(default)]
    pub obligations_discharged: Vec<DomainObligation>,
    #[serde(default)]
    pub lowering_path: Vec<String>,
    #[serde(default)]
    pub widenings: Vec<Widening>,
    #[serde(default)]
    pub production_allowed: bool,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub sampler: Option<SamplerMetadata>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub scenario_trace: Option<ScenarioTraceIr>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub compatibility_diagnostics: Vec<DomainDiagnostic>,
    #[serde(default)]
    pub diagnostics: Vec<String>,
}

impl Default for LoweredDomain {
    fn default() -> Self {
        Self::legacy_placeholder()
    }
}

impl LoweredDomain {
    fn legacy_placeholder() -> Self {
        let reason = legacy_domain_placeholder_reason();
        Self {
            relation: DomainRel::Unknown {
                reason: reason.clone(),
            },
            precision: DomainPrecision::Unknown,
            certificate: DomainCertificate::Unchecked,
            obligations_open: Vec::new(),
            obligations_discharged: Vec::new(),
            lowering_path: vec!["legacy_result".to_string()],
            widenings: Vec::new(),
            production_allowed: false,
            sampler: None,
            scenario_trace: None,
            compatibility_diagnostics: Vec::new(),
            diagnostics: vec![reason],
        }
    }

    fn skipped(reason: impl Into<String>) -> Self {
        let reason = reason.into();
        Self {
            relation: DomainRel::Unknown {
                reason: reason.clone(),
            },
            precision: DomainPrecision::Unknown,
            certificate: DomainCertificate::Unchecked,
            obligations_open: Vec::new(),
            obligations_discharged: Vec::new(),
            lowering_path: vec!["unsupported_generation".to_string()],
            widenings: Vec::new(),
            production_allowed: false,
            sampler: None,
            scenario_trace: None,
            compatibility_diagnostics: Vec::new(),
            diagnostics: vec![reason],
        }
    }
}

fn witness_replay_domain_from_values(
    witnesses: Vec<String>,
    sampler: Option<SamplerMetadata>,
) -> LoweredDomain {
    LoweredDomain {
        relation: DomainRel::Witness {
            encoded_values: witnesses,
        },
        precision: DomainPrecision::WitnessOnly,
        certificate: DomainCertificate::WitnessReplay,
        obligations_open: Vec::new(),
        obligations_discharged: vec![
            DomainObligation::WitnessReplaysThroughFuzzer,
            DomainObligation::WitnessSatisfiesDomain,
            DomainObligation::FuzzerModelHashMatches,
            DomainObligation::PropertyHarnessMatchesExportedUPLC,
        ],
        lowering_path: vec!["witness_replay".to_string()],
        widenings: Vec::new(),
        production_allowed: true,
        sampler: sampler.or_else(|| Some(default_witness_sampler_metadata())),
        scenario_trace: None,
        compatibility_diagnostics: Vec::new(),
        diagnostics: Vec::new(),
    }
}

fn intersect_optional_upper_bounds(left: Option<usize>, right: Option<usize>) -> Option<usize> {
    match (left, right) {
        (Some(left), Some(right)) => Some(left.min(right)),
        (Some(bound), None) | (None, Some(bound)) => Some(bound),
        (None, None) => None,
    }
}

fn list_semantics_bounds(semantics: &FuzzerSemantics) -> Option<ScenarioTraceBounds> {
    match semantics {
        FuzzerSemantics::List {
            min_len, max_len, ..
        } => Some(ScenarioTraceBounds {
            min: min_len.unwrap_or(0),
            max: *max_len,
        }),
        _ => None,
    }
}

fn scenario_trace_bounds_from_output_semantics(
    acceptance: &StateMachineAcceptance,
    output_semantics: &FuzzerSemantics,
) -> (ScenarioTraceBounds, ScenarioTraceBounds) {
    match acceptance {
        StateMachineAcceptance::AcceptsSuccess => {
            let step_depth = list_semantics_bounds(output_semantics)
                .unwrap_or(ScenarioTraceBounds { min: 0, max: None });
            let trace_length = ScenarioTraceBounds {
                min: step_depth.min.saturating_add(1),
                max: step_depth.max.map(|bound| bound.saturating_add(1)),
            };
            (trace_length, step_depth)
        }
        StateMachineAcceptance::AcceptsFailure => {
            if let FuzzerSemantics::Product(parts) = output_semantics
                && let [labels, events] = parts.as_slice()
                && let (Some(label_bounds), Some(event_bounds)) =
                    (list_semantics_bounds(labels), list_semantics_bounds(events))
            {
                let min_steps = label_bounds.min.max(event_bounds.min).max(1);
                let max_steps = intersect_optional_upper_bounds(label_bounds.max, event_bounds.max);
                let step_depth = ScenarioTraceBounds {
                    min: min_steps,
                    max: max_steps,
                };
                let trace_length = ScenarioTraceBounds {
                    min: min_steps.saturating_add(1),
                    max: max_steps.map(|bound| bound.saturating_add(1)),
                };
                return (trace_length, step_depth);
            }

            (
                ScenarioTraceBounds { min: 2, max: None },
                ScenarioTraceBounds { min: 1, max: None },
            )
        }
    }
}

fn scenario_trace_ir_for_test(
    test: &ExportedPropertyTest,
    mode: CompatibilityMode,
    proof_caveat: Option<&ProofCaveat>,
) -> Option<ScenarioTraceIr> {
    let FuzzerSemantics::StateMachineTrace {
        acceptance,
        output_semantics,
        step_function_ir,
        initial_state_shallow_ir,
        ..
    } = &test.semantics
    else {
        return None;
    };

    let has_transition_prop = test.transition_prop_lean.is_some();
    let has_step_function_ir = step_function_ir.is_some();
    let transition_relation_exact = test
        .transition_prop_lean
        .as_ref()
        .is_some_and(|tp| !tp.is_vacuous && transition_prop_widenings(tp).is_empty());
    let initial_state_anchored = test
        .transition_prop_lean
        .as_ref()
        .and_then(|tp| tp.initial_state_lean.as_ref())
        .is_some()
        || initial_state_shallow_ir.is_some();
    let witness_replay_available = !witness_values_for_mode(test, proof_caveat).is_empty();
    let (trace_length, step_depth) =
        scenario_trace_bounds_from_output_semantics(acceptance, output_semantics.as_ref());
    let symbolic_encoding =
        if matches!(mode, CompatibilityMode::WitnessCheck) && witness_replay_available {
            ScenarioSymbolicEncoding::WitnessReplayOnly
        } else if trace_length.max.is_some() && step_depth.max.is_some() {
            ScenarioSymbolicEncoding::BoundedBooleanChecker
        } else {
            ScenarioSymbolicEncoding::Unavailable
        };
    let uses_global_reachability_over_approx = !matches!(mode, CompatibilityMode::WitnessCheck);
    let script_context_predicate = if matches!(mode, CompatibilityMode::WitnessCheck)
        || (is_state_machine_trace_halt_test(test) && has_transition_prop)
    {
        ScenarioPredicateStatus::Available
    } else {
        ScenarioPredicateStatus::OpenObligation
    };
    let ledger_validity_predicate = if matches!(mode, CompatibilityMode::WitnessCheck) {
        ScenarioPredicateStatus::Available
    } else {
        ScenarioPredicateStatus::OpenObligation
    };

    let mut open_obligations = Vec::new();
    let mut blockers = Vec::new();
    if !matches!(mode, CompatibilityMode::WitnessCheck) {
        if trace_length.max.is_none() || step_depth.max.is_none() {
            open_obligations.push("bounded_trace_length".to_string());
            blockers.push(
                "No explicit maximum trace length/depth was exported for this scenario trace; symbolic verification remains unbounded.".to_string(),
            );
        }

        if !initial_state_anchored {
            open_obligations.push("initial_state_anchor".to_string());
            blockers.push(
                "Scenario symbolic reasoning is not anchored to a concrete exported initial state."
                    .to_string(),
            );
        }

        if !has_transition_prop && !has_step_function_ir {
            open_obligations.push("trace_transition_relation".to_string());
            blockers.push(
                "No extracted transition relation or step-function IR is available for this scenario trace.".to_string(),
            );
        }

        if has_transition_prop && !transition_relation_exact {
            open_obligations.push("exact_transition_relation".to_string());
            blockers.push(
                "The exported transition relation still carries widenings or placeholders, so the symbolic trace proof is not exact.".to_string(),
            );
        }

        if matches!(
            script_context_predicate,
            ScenarioPredicateStatus::OpenObligation
        ) {
            open_obligations.push("script_context_predicate".to_string());
            blockers.push(
                "No generated script-context predicate is available for this scenario trace path."
                    .to_string(),
            );
        }

        if matches!(
            ledger_validity_predicate,
            ScenarioPredicateStatus::OpenObligation
        ) {
            open_obligations.push("ledger_validity_predicate".to_string());
            blockers.push(
                "Ledger-validity predicates for scenario traces are not yet integrated into symbolic proofs.".to_string(),
            );
        }

        if uses_global_reachability_over_approx {
            open_obligations.push("sound_reachability_relation".to_string());
            blockers.push(
                "Current symbolic scenario proofs still rely on a global over-approximated reachable predicate rather than a fully threaded bounded trace semantics.".to_string(),
            );
        }
    }

    Some(ScenarioTraceIr {
        acceptance: acceptance.clone(),
        trace_length,
        step_depth,
        symbolic_encoding,
        initial_state_anchored,
        has_transition_prop,
        has_step_function_ir,
        transition_relation_exact,
        uses_global_reachability_over_approx,
        witness_replay_available,
        script_context_predicate,
        ledger_validity_predicate,
        open_obligations,
        blockers,
    })
}

fn emit_scenario_trace_bound_helpers(
    helper_prefix: &str,
    trace_length: &ScenarioTraceBounds,
    step_depth: &ScenarioTraceBounds,
) -> (String, String, String) {
    let trace_bounds_name = format!("{helper_prefix}_traceWithinBounds");
    let trace_depth_bounds_name = format!("{helper_prefix}_traceDepthWithinBounds");
    let mut content = String::new();
    content.push_str("-- Bounded scenario trace envelope exported from Aiken.\n");
    content.push_str(&format!(
        "def {trace_bounds_name} (trace : List Data) : Prop :=\n  {} <= trace.length",
        trace_length.min
    ));
    if let Some(max) = trace_length.max {
        content.push_str(&format!(
            " {AND} trace.length <= {max}",
            AND = lean_sym::AND
        ));
    }
    content.push_str("\n\n");
    content.push_str(&format!(
        "def {trace_depth_bounds_name} (trace : List Data) : Prop :=\n  {} <= trace.length - 1",
        step_depth.min
    ));
    if let Some(max) = step_depth.max {
        content.push_str(&format!(
            " {AND} trace.length - 1 <= {max}",
            AND = lean_sym::AND
        ));
    }
    content.push_str("\n\n");
    (trace_bounds_name, trace_depth_bounds_name, content)
}

fn scenario_trace_primary_blocker(trace: &ScenarioTraceIr) -> Option<&str> {
    trace.blockers.first().map(String::as_str)
}

impl LoweredDomain {
    fn trust_limited_reason(&self, trust_profile: TrustProfile) -> Option<String> {
        if let Some(minimum) = self.relation.allowed_only_under()
            && !trust_profile.allows_only_under(minimum)
        {
            return Some(
                "domain relation contains an acknowledged widening that is not accepted under the selected trust profile.".to_string(),
            );
        }

        if self.widenings.iter().any(|widening| {
            widening
                .allowed_only_under
                .is_some_and(|minimum| !trust_profile.allows_only_under(minimum))
        }) {
            return Some(
                "domain lowering contains widenings that are not accepted under the selected trust profile.".to_string(),
            );
        }

        None
    }
}
/// Status of a single theorem proof using the legacy detailed vocabulary.
///
/// Kept on the wire as `proof_status` so old result values can still be read
/// and displayed, while the canonical `TheoremResult::status` field uses the
/// M0 verification vocabulary.
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum ProofStatus {
    /// Universal theorem, closed by Lean with no generated `sorry` or witness fallback.
    /// In the current Blaster-backed pipeline this normalizes to `SolverValidated`,
    /// not `LeanCertified`, because proof reconstruction is not available.
    Proved,
    /// Lean build succeeded but proof covers only concrete witness instance(s),
    /// not all inputs. Universal verification is planned for a future release.
    WitnessProved {
        instances: usize,
        /// CBOR-hex encoded Plutus Data witnesses.
        witnesses: Vec<String>,
        note: String,
    },
    /// Build succeeded but contains `sorry` or another open obligation. Not a complete proof.
    Partial { note: String },
    /// Proof failed with a classified reason
    Failed {
        category: FailureCategory,
        reason: String,
    },
    /// Proof execution timed out
    TimedOut { reason: String },
    /// Could not determine proof status
    Unknown,
}

#[derive(Debug, Clone, serde::Deserialize)]
#[serde(untagged)]
enum TheoremStatusWire {
    Current(VerificationStatus),
    Legacy(ProofStatus),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SuccessChannel {
    SolverTheorem,
    WitnessReplay,
}

#[derive(Debug, Clone, Default)]
struct CompatibilityAssessment {
    block_reason: Option<String>,
    success_note: Option<String>,
    diagnostics: Vec<DomainDiagnostic>,
}

fn render_optional_return_mode(return_mode: Option<TestReturnMode>) -> &'static str {
    match return_mode {
        Some(TestReturnMode::Bool) => "bool",
        Some(TestReturnMode::Void) => "void",
        None => "legacy-unknown",
    }
}

fn compatibility_mode_label(
    mode: CompatibilityMode,
    return_mode: Option<TestReturnMode>,
) -> String {
    let return_mode = render_optional_return_mode(return_mode);
    match mode {
        CompatibilityMode::UniversalSuccess => {
            format!("normal {return_mode} universal theorem")
        }
        CompatibilityMode::UniversalFailure => {
            format!("fail {return_mode} universal theorem")
        }
        CompatibilityMode::SemanticExistential => {
            format!("fail_once {return_mode} existential theorem")
        }
        CompatibilityMode::WitnessCheck => format!("{return_mode} witness check"),
    }
}

fn domain_has_open_obligation(domain: &LoweredDomain, obligation: DomainObligation) -> bool {
    domain.obligations_open.contains(&obligation)
}

fn domain_has_discharged_obligation(
    domain: &LoweredDomain,
    obligation: DomainObligation,
    alternative: Option<DomainObligation>,
) -> bool {
    domain.obligations_discharged.contains(&obligation)
        || alternative.is_some_and(|other| domain.obligations_discharged.contains(&other))
}

fn domain_diagnostic(
    code: &str,
    severity: DomainDiagnosticSeverity,
    message: impl Into<String>,
    mode: CompatibilityMode,
    trust_profile: TrustProfile,
    obligations: Vec<DomainObligation>,
) -> DomainDiagnostic {
    DomainDiagnostic {
        code: code.to_string(),
        severity,
        message: message.into(),
        source_path: None,
        source_span: None,
        mode: Some(mode),
        trust_profile: Some(trust_profile),
        obligations,
    }
}

fn relational_profile_diagnostic(
    code: &str,
    severity: DomainDiagnosticSeverity,
    message: impl Into<String>,
    source_span: Option<ExportedSourceSpan>,
) -> RelationalProfileDiagnostic {
    RelationalProfileDiagnostic {
        code: code.to_string(),
        severity,
        message: message.into(),
        source_span,
    }
}

fn lift_relational_profile_diagnostics(
    source_path: &str,
    fallback_span: Option<ExportedSourceSpan>,
    diagnostics: &[RelationalProfileDiagnostic],
) -> Vec<DomainDiagnostic> {
    diagnostics
        .iter()
        .map(|diagnostic| DomainDiagnostic {
            code: diagnostic.code.clone(),
            severity: diagnostic.severity,
            message: diagnostic.message.clone(),
            source_path: Some(source_path.to_string()),
            source_span: diagnostic.source_span.or(fallback_span),
            mode: None,
            trust_profile: None,
            obligations: Vec::new(),
        })
        .collect()
}

fn first_domain_diagnostic_message(domain: &LoweredDomain) -> Option<String> {
    domain
        .compatibility_diagnostics
        .first()
        .map(|diagnostic| diagnostic.message.clone())
        .or_else(|| domain.diagnostics.first().cloned())
}

fn state_machine_has_step_ir(test: &ExportedPropertyTest) -> bool {
    matches!(
        &test.semantics,
        FuzzerSemantics::StateMachineTrace {
            step_function_ir: Some(_),
            ..
        }
    )
}

fn compatibility_mode_for_generation(
    test: &ExportedPropertyTest,
    _existential_mode: ExistentialMode,
    proof_caveat: Option<&ProofCaveat>,
) -> CompatibilityMode {
    if proof_caveat.is_some_and(|caveat| matches!(caveat, ProofCaveat::Witness(_))) {
        return CompatibilityMode::WitnessCheck;
    }

    if is_state_machine_trace_error_test(test) && !test.concrete_error_witnesses.is_empty() {
        return CompatibilityMode::WitnessCheck;
    }

    if is_state_machine_trace_halt_test(test)
        && !state_machine_has_step_ir(test)
        && !test.concrete_halt_witnesses.is_empty()
    {
        return CompatibilityMode::WitnessCheck;
    }

    match ManifestTestMode::from_on_test_failure(&test.on_test_failure) {
        ManifestTestMode::Normal => CompatibilityMode::UniversalSuccess,
        ManifestTestMode::Fail => CompatibilityMode::UniversalFailure,
        ManifestTestMode::FailOnce => CompatibilityMode::SemanticExistential,
    }
}

fn compatibility_mode_for_success(
    proof_status: &ProofStatus,
    test_mode: Option<ManifestTestMode>,
) -> Option<(CompatibilityMode, SuccessChannel)> {
    match proof_status {
        ProofStatus::Proved => Some((
            match test_mode.unwrap_or(ManifestTestMode::Normal) {
                ManifestTestMode::Normal => CompatibilityMode::UniversalSuccess,
                ManifestTestMode::Fail => CompatibilityMode::UniversalFailure,
                ManifestTestMode::FailOnce => CompatibilityMode::SemanticExistential,
            },
            SuccessChannel::SolverTheorem,
        )),
        ProofStatus::WitnessProved { .. } => Some((
            CompatibilityMode::WitnessCheck,
            SuccessChannel::WitnessReplay,
        )),
        ProofStatus::Partial { .. }
        | ProofStatus::Failed { .. }
        | ProofStatus::TimedOut { .. }
        | ProofStatus::Unknown => None,
    }
}

fn assess_domain_compatibility(
    mode: CompatibilityMode,
    return_mode: Option<TestReturnMode>,
    trust_profile: TrustProfile,
    domain: &LoweredDomain,
    success_channel: SuccessChannel,
) -> CompatibilityAssessment {
    let mut assessment = CompatibilityAssessment::default();
    let mode_label = compatibility_mode_label(mode, return_mode);

    let mut record_block = |code: &str, message: String, obligations: Vec<DomainObligation>| {
        if assessment.block_reason.is_none() {
            assessment.block_reason = Some(message.clone());
        }

        assessment.diagnostics.push(domain_diagnostic(
            code,
            DomainDiagnosticSeverity::Error,
            message,
            mode,
            trust_profile,
            obligations,
        ));
    };

    if let Some(reason) = domain.trust_limited_reason(trust_profile) {
        record_block("trust_profile_widening_block", reason, Vec::new());
    }

    if let Some(trace) = &domain.scenario_trace
        && matches!(success_channel, SuccessChannel::SolverTheorem)
    {
        if !matches!(
            trace.symbolic_encoding,
            ScenarioSymbolicEncoding::BoundedBooleanChecker
        ) {
            record_block(
                "scenario_trace_unbounded",
                format!(
                    "{mode_label} for this scenario trace is not production-safe because no bounded symbolic trace checker is available (trace length {}, step depth {}).",
                    trace.trace_length.render(),
                    trace.step_depth.render()
                ),
                Vec::new(),
            );
        }

        if matches!(
            trace.script_context_predicate,
            ScenarioPredicateStatus::OpenObligation
        ) {
            record_block(
                "scenario_missing_script_context",
                format!(
                    "{mode_label} for this scenario trace is missing an exported script-context predicate; symbolic success is downgraded until that obligation is discharged.",
                ),
                Vec::new(),
            );
        }

        if matches!(
            trace.ledger_validity_predicate,
            ScenarioPredicateStatus::OpenObligation
        ) {
            record_block(
                "scenario_missing_ledger_validity",
                format!(
                    "{mode_label} for this scenario trace has no integrated ledger-validity predicate; symbolic success is downgraded until the runtime validity obligation is modeled explicitly.",
                ),
                Vec::new(),
            );
        }

        if trace.uses_global_reachability_over_approx {
            let detail = scenario_trace_primary_blocker(trace).unwrap_or(
                "Current symbolic scenario proofs still rely on a global over-approximated reachable predicate.",
            );
            record_block(
                "scenario_global_reachability_overapprox",
                format!(
                    "{mode_label} for this scenario trace is downgraded because the symbolic relation is still an acknowledged over-approximation. {detail}",
                ),
                Vec::new(),
            );
        }
    }

    match mode {
        CompatibilityMode::UniversalSuccess | CompatibilityMode::UniversalFailure => {
            if !matches!(
                domain.precision,
                DomainPrecision::Exact | DomainPrecision::OverApprox
            ) {
                record_block(
                    "mode_domain_universal_precision",
                    format!(
                        "{mode_label} requires Exact or OverApprox domain precision; current precision is {}.",
                        domain.precision
                    ),
                    vec![DomainObligation::FuzzerReturnsImpliesDomain],
                );
            }

            if domain_has_open_obligation(domain, DomainObligation::FuzzerReturnsImpliesDomain)
                || !domain_has_discharged_obligation(
                    domain,
                    DomainObligation::FuzzerReturnsImpliesDomain,
                    Some(DomainObligation::DomainIffFuzzerReturns),
                )
            {
                record_block(
                    "mode_domain_universal_coverage",
                    format!(
                        "{mode_label} requires discharged FuzzerReturnsImpliesDomain coverage over generated inputs.",
                    ),
                    vec![DomainObligation::FuzzerReturnsImpliesDomain],
                );
            }
        }
        CompatibilityMode::SemanticExistential => match domain.precision {
            DomainPrecision::Exact | DomainPrecision::UnderApprox => {
                if domain_has_open_obligation(domain, DomainObligation::DomainImpliesFuzzerReturns)
                    || !domain_has_discharged_obligation(
                        domain,
                        DomainObligation::DomainImpliesFuzzerReturns,
                        Some(DomainObligation::DomainIffFuzzerReturns),
                    )
                {
                    record_block(
                        "mode_domain_existential_coverage",
                        format!(
                            "{mode_label} requires discharged DomainImpliesFuzzerReturns or DomainIffFuzzerReturns coverage.",
                        ),
                        vec![
                            DomainObligation::DomainImpliesFuzzerReturns,
                            DomainObligation::DomainIffFuzzerReturns,
                        ],
                    );
                }
            }
            DomainPrecision::WitnessOnly => {
                if domain.certificate != DomainCertificate::WitnessReplay
                    || domain_has_open_obligation(
                        domain,
                        DomainObligation::WitnessReplaysThroughFuzzer,
                    )
                    || !domain_has_discharged_obligation(
                        domain,
                        DomainObligation::WitnessReplaysThroughFuzzer,
                        None,
                    )
                {
                    record_block(
                        "mode_domain_existential_witness_replay",
                        format!(
                            "{mode_label} can use WitnessOnly domains only when the witness is replay-validated through the real fuzzer path.",
                        ),
                        vec![
                            DomainObligation::WitnessReplaysThroughFuzzer,
                            DomainObligation::WitnessSatisfiesDomain,
                        ],
                    );
                }
            }
            DomainPrecision::OverApprox | DomainPrecision::Unknown => {
                record_block(
                    "mode_domain_existential_precision",
                    format!(
                        "{mode_label} requires Exact, UnderApprox, or WitnessOnly precision; current precision is {}.",
                        domain.precision
                    ),
                    vec![DomainObligation::DomainImpliesFuzzerReturns],
                );
            }
        },
        CompatibilityMode::WitnessCheck => {
            if domain.precision != DomainPrecision::WitnessOnly {
                record_block(
                    "mode_domain_witness_precision",
                    format!(
                        "{mode_label} requires WitnessOnly precision backed by concrete replay validation; current precision is {}.",
                        domain.precision
                    ),
                    vec![
                        DomainObligation::WitnessReplaysThroughFuzzer,
                        DomainObligation::WitnessSatisfiesDomain,
                    ],
                );
            }

            if domain.certificate != DomainCertificate::WitnessReplay {
                record_block(
                    "mode_domain_witness_certificate",
                    format!(
                        "{mode_label} requires WitnessReplay certification; current certificate is {}.",
                        domain.certificate
                    ),
                    vec![DomainObligation::WitnessReplaysThroughFuzzer],
                );
            }

            if domain_has_open_obligation(domain, DomainObligation::WitnessReplaysThroughFuzzer)
                || !domain_has_discharged_obligation(
                    domain,
                    DomainObligation::WitnessReplaysThroughFuzzer,
                    None,
                )
                || !domain_has_discharged_obligation(
                    domain,
                    DomainObligation::WitnessSatisfiesDomain,
                    None,
                )
            {
                record_block(
                    "mode_domain_witness_replay",
                    format!(
                        "{mode_label} requires discharged concrete replay obligations through the real fuzzer/property path.",
                    ),
                    vec![
                        DomainObligation::WitnessReplaysThroughFuzzer,
                        DomainObligation::WitnessSatisfiesDomain,
                    ],
                );
            }
        }
    }

    if !domain.obligations_open.is_empty() {
        record_block(
            "open_domain_obligations",
            format!(
                "{mode_label} still has open domain obligations: {}.",
                render_domain_obligations(&domain.obligations_open)
            ),
            domain.obligations_open.clone(),
        );
    }

    match trust_profile {
        TrustProfile::Strict => match success_channel {
            SuccessChannel::SolverTheorem => {
                record_block(
                    "trust_profile_strict_requires_lean_certified",
                    "strict trust accepts only LeanCertified theorem results; the current pre-Lean pipeline yields solver validation, not Lean kernel certification.".to_string(),
                    Vec::new(),
                );
            }
            SuccessChannel::WitnessReplay => {
                if domain.certificate != DomainCertificate::WitnessReplay {
                    record_block(
                        "trust_profile_strict_requires_witness_replay",
                        "strict trust accepts witness checks only when they are backed by concrete WitnessReplay certification.".to_string(),
                        vec![DomainObligation::WitnessReplaysThroughFuzzer],
                    );
                }
            }
        },
        TrustProfile::Production => {
            if matches!(
                domain.certificate,
                DomainCertificate::DifferentialTestedOnly | DomainCertificate::Unchecked
            ) {
                record_block(
                    "trust_profile_production_certificate",
                    format!(
                        "production trust rejects {} domain certificates for successful verification claims.",
                        domain.certificate
                    ),
                    Vec::new(),
                );
            }
        }
        TrustProfile::Experimental => {
            if domain.certificate == DomainCertificate::Unchecked {
                record_block(
                    "trust_profile_experimental_unchecked",
                    "experimental trust still rejects Unchecked domain certificates.".to_string(),
                    Vec::new(),
                );
            } else if domain.certificate == DomainCertificate::DifferentialTestedOnly {
                let warning = "Accepted only under the experimental trust profile because the domain certificate is DifferentialTestedOnly; this is not a production verification claim.".to_string();
                assessment.success_note = Some(warning.clone());
                assessment.diagnostics.push(domain_diagnostic(
                    "trust_profile_experimental_warning",
                    DomainDiagnosticSeverity::Warning,
                    warning,
                    mode,
                    trust_profile,
                    Vec::new(),
                ));
            }
        }
        TrustProfile::UnsafeDev => {
            record_block(
                "trust_profile_unsafe_dev_not_verified",
                "unsafe-dev experiments may run, but this trust profile can never print a verified result.".to_string(),
                Vec::new(),
            );
        }
    }

    assessment
}

fn domain_success_assessment(
    trust_profile: TrustProfile,
    domain: &LoweredDomain,
    proof_status: &ProofStatus,
    test_mode: Option<ManifestTestMode>,
    return_mode: Option<TestReturnMode>,
) -> Option<CompatibilityAssessment> {
    let (mode, success_channel) = compatibility_mode_for_success(proof_status, test_mode)?;
    Some(assess_domain_compatibility(
        mode,
        return_mode,
        trust_profile,
        domain,
        success_channel,
    ))
}

fn blocked_success_note(
    trust_profile: TrustProfile,
    detail: &str,
    domain: &LoweredDomain,
) -> String {
    let mut note = format!("Domain metadata blocks {trust_profile} success: {detail}");
    if let Some(primary) = first_domain_diagnostic_message(domain)
        && primary != detail
    {
        note.push_str(&format!(" Primary diagnostic: {primary}"));
    }
    if let Some(widening) = domain.widenings.first() {
        note.push_str(&format!(" First widening: {}", widening.message));
    }
    note
}

fn normalize_sampler_proof_status(
    domain: &LoweredDomain,
    proof_status: ProofStatus,
) -> ProofStatus {
    match proof_status {
        ProofStatus::Failed {
            category: FailureCategory::BlasterUnsupported,
            reason,
        } if matches!(domain.relation, DomainRel::SamplerReturns { .. }) => {
            let reason = reason.trim();
            let reason = if reason.is_empty() {
                "Sampler relation generated successfully, but Blaster could not solve or translate the resulting theorem.".to_string()
            } else {
                format!(
                    "Sampler relation generated successfully, but Blaster could not solve or translate the resulting theorem: {reason}"
                )
            };
            ProofStatus::Failed {
                category: FailureCategory::Unknown,
                reason,
            }
        }
        other => other,
    }
}

/// Per-theorem result
#[derive(Debug, Clone, serde::Serialize)]
#[non_exhaustive]
pub struct TheoremResult {
    pub test_name: String,
    pub theorem_name: String,
    pub status: VerificationStatus,
    pub certification: Certification,
    pub trust_profile: TrustProfile,
    pub domain: LoweredDomain,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub input_type: Option<InputValueBridge>,
    pub proof_status: ProofStatus,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub explanation: Option<String>,
    /// Number of constraints dropped or widened during TransitionProp lowering.
    /// Non-zero only for two-phase trace theorems.
    #[serde(default, skip_serializing_if = "is_zero")]
    pub over_approximations: usize,
}

fn theorem_result_explanation(
    status: VerificationStatus,
    proof_status: &ProofStatus,
) -> Option<String> {
    fn clean_reason(reason: &str) -> Option<String> {
        let trimmed = reason.trim();
        (!trimmed.is_empty()).then(|| trimmed.to_string())
    }

    match proof_status {
        ProofStatus::Proved => None,
        ProofStatus::WitnessProved { .. } => None,
        ProofStatus::Partial { note } => clean_reason(note).or_else(|| {
            Some("The generated Lean proof still contains open obligations; this result is partial, not production-success.".to_string())
        }),
        ProofStatus::TimedOut { reason } => clean_reason(reason).or_else(|| {
            Some("Lean/Blaster did not finish before the configured timeout expired.".to_string())
        }),
        ProofStatus::Failed { category, reason } => match category {
            FailureCategory::Counterexample => None,
            FailureCategory::BlasterUnsupported => clean_reason(reason).or_else(|| {
                Some("Blaster could not translate or validate the generated theorem shape.".to_string())
            }),
            FailureCategory::Timeout => clean_reason(reason).or_else(|| {
                Some("Lean/Blaster did not finish before the configured timeout expired.".to_string())
            }),
            FailureCategory::UnsatGoal
            | FailureCategory::BuildError
            | FailureCategory::DependencyError
            | FailureCategory::Unknown => clean_reason(reason).or_else(|| match status {
                VerificationStatus::Unknown => Some(
                    "Lean/Blaster completed without a trustworthy proof classification for this theorem.".to_string(),
                ),
                VerificationStatus::Unsupported => Some(
                    "The generated theorem could not be validated by the current integration.".to_string(),
                ),
                _ => None,
            }),
        },
        ProofStatus::Unknown => Some(
            "Lean/Blaster completed without a trustworthy proof classification for this theorem."
                .to_string(),
        ),
    }
}

fn domain_result_explanation(domain: &LoweredDomain) -> Option<String> {
    let mut lines = vec![format!(
        "input domain: {} ({})",
        domain.precision, domain.certificate
    )];

    if !domain.lowering_path.is_empty() {
        lines.push(format!("lowering: {}", domain.lowering_path.join(" -> ")));
    }

    if let Some(trace) = &domain.scenario_trace {
        lines.push(format!(
            "scenario trace: {:?}, trace length {}, step depth {}",
            trace.symbolic_encoding,
            trace.trace_length.render(),
            trace.step_depth.render()
        ));
        lines.push(format!(
            "scenario predicates: script_context={}, ledger_validity={}",
            render_scenario_predicate_status(trace.script_context_predicate),
            render_scenario_predicate_status(trace.ledger_validity_predicate)
        ));
        if !trace.open_obligations.is_empty() {
            lines.push(format!(
                "scenario obligations: {}",
                render_scenario_obligations(&trace.open_obligations)
            ));
        }
        if let Some(blocker) = trace.blockers.first() {
            lines.push(format!("scenario blocker: {blocker}"));
        }
    }

    if let Some(widening) = domain.widenings.first() {
        lines.push(format!("widening: {}", widening.message));
    } else if let Some(message) = first_domain_diagnostic_message(domain) {
        lines.push(format!("domain note: {message}"));
    }

    Some(lines.join("\n"))
}

impl<'de> serde::Deserialize<'de> for TheoremResult {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        #[derive(serde::Deserialize)]
        struct Wire {
            test_name: String,
            theorem_name: String,
            status: TheoremStatusWire,
            #[serde(default)]
            certification: Option<Certification>,
            #[serde(default)]
            trust_profile: Option<TrustProfile>,
            #[serde(default)]
            domain: Option<LoweredDomain>,
            #[serde(default)]
            input_type: Option<InputValueBridge>,
            #[serde(default)]
            proof_status: Option<ProofStatus>,
            #[serde(default)]
            explanation: Option<String>,
            #[serde(default)]
            over_approximations: usize,
        }

        let wire = Wire::deserialize(deserializer)?;
        let (status, proof_status) = match wire.status {
            TheoremStatusWire::Current(status) => {
                let proof_status = wire
                    .proof_status
                    .unwrap_or_else(|| status.default_proof_status());
                (status, proof_status)
            }
            TheoremStatusWire::Legacy(proof_status) => {
                let status = VerificationStatus::from_proof_status(&proof_status);
                let proof_status = wire.proof_status.unwrap_or(proof_status);
                (status, proof_status)
            }
        };
        let certification = wire
            .certification
            .unwrap_or_else(|| Certification::from_status(status));
        let trust_profile = wire.trust_profile.unwrap_or_default();
        let domain = wire
            .domain
            .unwrap_or_else(LoweredDomain::legacy_placeholder);
        let explanation = wire
            .explanation
            .or_else(|| theorem_result_explanation(status, &proof_status));

        Ok(TheoremResult {
            test_name: wire.test_name,
            theorem_name: wire.theorem_name,
            status,
            certification,
            trust_profile,
            domain,
            input_type: wire.input_type,
            proof_status,
            explanation,
            over_approximations: wire.over_approximations,
        })
    }
}

fn unsupported_status() -> VerificationStatus {
    VerificationStatus::Unsupported
}

fn unsupported_certification() -> Certification {
    Certification::Unsupported
}

fn default_result_domain() -> LoweredDomain {
    LoweredDomain::legacy_placeholder()
}

impl TheoremResult {
    fn from_parts(
        test_name: String,
        theorem_name: String,
        proof_status: ProofStatus,
        over_approximations: usize,
        trust_profile: TrustProfile,
        domain: LoweredDomain,
        input_type: Option<InputValueBridge>,
    ) -> Self {
        let status = VerificationStatus::from_proof_status(&proof_status);
        let certification = Certification::from_proof_status(&proof_status);
        let explanation = theorem_result_explanation(status, &proof_status);
        Self {
            test_name,
            theorem_name,
            status,
            certification,
            trust_profile,
            domain,
            input_type,
            proof_status,
            explanation,
            over_approximations,
        }
    }

    pub fn new(
        test_name: String,
        theorem_name: String,
        proof_status: ProofStatus,
        over_approximations: usize,
    ) -> Self {
        Self::from_parts(
            test_name,
            theorem_name,
            proof_status,
            over_approximations,
            default_trust_profile(),
            default_result_domain(),
            None,
        )
    }

    pub fn new_with_domain(
        test_name: String,
        theorem_name: String,
        proof_status: ProofStatus,
        over_approximations: usize,
        trust_profile: TrustProfile,
        domain: LoweredDomain,
        input_type: Option<InputValueBridge>,
        test_mode: Option<ManifestTestMode>,
        return_mode: Option<TestReturnMode>,
    ) -> Self {
        let proof_status = normalize_sampler_proof_status(&domain, proof_status);
        let assessment = domain_success_assessment(
            trust_profile,
            &domain,
            &proof_status,
            test_mode,
            return_mode,
        );
        let proof_status = assessment
            .as_ref()
            .and_then(|assessment| assessment.block_reason.as_deref())
            .map(|detail| ProofStatus::Partial {
                note: blocked_success_note(trust_profile, detail, &domain),
            })
            .unwrap_or(proof_status);

        let mut result = Self::from_parts(
            test_name,
            theorem_name,
            proof_status,
            over_approximations,
            trust_profile,
            domain,
            input_type,
        );

        if matches!(
            result.proof_status,
            ProofStatus::Proved | ProofStatus::WitnessProved { .. }
        ) && let Some(note) = assessment.and_then(|assessment| assessment.success_note)
        {
            result.explanation = Some(match result.explanation.take() {
                Some(existing) => format!("{existing}\n{note}"),
                None => note,
            });
        }
        if let Some(note) = domain_result_explanation(&result.domain) {
            result.explanation = Some(match result.explanation.take() {
                Some(existing) => format!("{existing}\n{note}"),
                None => note,
            });
        }

        result
    }

    pub fn from_manifest_entry(
        test_name: String,
        theorem_name: String,
        entry: &ManifestEntry,
        proof_status: ProofStatus,
        over_approximations: usize,
    ) -> Self {
        let domain = match &proof_status {
            ProofStatus::WitnessProved { witnesses, .. } => {
                let existing = entry
                    .domain
                    .clone()
                    .unwrap_or_else(LoweredDomain::legacy_placeholder);
                if existing.precision == DomainPrecision::WitnessOnly
                    && existing.certificate == DomainCertificate::WitnessReplay
                {
                    existing
                } else {
                    witness_replay_domain_from_values(witnesses.clone(), entry.sampler.clone())
                }
            }
            _ => entry
                .domain
                .clone()
                .unwrap_or_else(LoweredDomain::legacy_placeholder),
        };

        Self::new_with_domain(
            test_name,
            theorem_name,
            proof_status,
            over_approximations,
            default_trust_profile(),
            domain,
            entry.input_type.clone(),
            entry.test_mode,
            entry.return_mode.clone(),
        )
    }

    pub fn set_proof_status(&mut self, proof_status: ProofStatus) {
        self.status = VerificationStatus::from_proof_status(&proof_status);
        self.certification = Certification::from_proof_status(&proof_status);
        self.explanation = theorem_result_explanation(self.status, &proof_status);
        self.proof_status = proof_status;
    }
}

/// Default pinned Blaster revision. Update this when upgrading to a new tested Blaster version.
pub const DEFAULT_BLASTER_REV: &str = "95368fb83f0e359be762a64d2e75facb754d3ee2";

/// Default pinned PlutusCore revision. Update this when upgrading to a new tested PlutusCore version.
/// Pinned to a specific commit from PlutusCoreBlaster `main` as of 2026-04-23.
/// When bumping, verify compatibility with DEFAULT_BLASTER_REV.
pub const DEFAULT_PLUTUS_CORE_REV: &str = "08a282573ca2ec8abd2efce57a21c1d29c2a532d";

/// Minimum supported Lean version (major, minor, patch).
pub const MIN_LEAN_VERSION: (u32, u32, u32) = (4, 24, 0);

/// Pinned Lean toolchain literal embedded in every generated `lean-toolchain`
/// file. Must agree with [`MIN_LEAN_VERSION`] — the
/// `lean_version_constant_matches_pinned_version` test enforces this drift
/// sentinel so that bumping the tuple without updating the literal (or vice
/// versa) fails CI rather than silently shipping a mismatched workspace.
pub const LEAN_TOOLCHAIN_LITERAL: &str = "leanprover/lean4:v4.24.0\n";

/// Minimum supported Z3 version (major, minor, patch).
pub const MIN_Z3_VERSION: (u32, u32, u32) = (4, 8, 0);

/// Validate that a git revision string contains only safe characters for
/// interpolation into lakefile.lean. Allows alphanumeric, dots, hyphens,
/// slashes, and underscores (covering commit SHAs, tags, and branch names),
/// but rejects leading `-` so the revision cannot be reinterpreted as a flag
/// by downstream tooling.
fn validate_git_rev(rev: &str, name: &str) -> miette::Result<()> {
    if rev.is_empty() {
        return Err(miette::miette!("{name} must not be empty"));
    }
    if rev.starts_with('-') {
        return Err(miette::miette!("{name} must not start with '-': \"{rev}\""));
    }
    if !rev
        .chars()
        .all(|c| c.is_ascii_alphanumeric() || matches!(c, '.' | '-' | '/' | '_'))
    {
        return Err(miette::miette!(
            "{name} contains invalid characters: \"{rev}\". \
             Only alphanumeric characters, dots, hyphens, slashes, and underscores are allowed."
        ));
    }
    Ok(())
}

/// Strategy for handling `fail once` (existential) tests.
#[non_exhaustive]
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum ExistentialMode {
    /// Attempt a full existential proof via Lean tactics (default).
    /// This dispatches to `by blaster`, which calls Z3 to synthesize the
    /// witness, and is required for Int-domain existentials where a
    /// deterministic `0` witness is generally wrong.
    #[default]
    Proof,
    /// Generate a witness-based theorem: deterministic witness search
    /// followed by a concrete witness-correctness theorem. Only sound
    /// for scalar domains where `0` / `False` is a trivially-checked
    /// witness; unsafe for generic Int-domain existentials.
    Witness,
}

impl std::fmt::Display for ExistentialMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExistentialMode::Proof => write!(f, "proof"),
            ExistentialMode::Witness => write!(f, "witness"),
        }
    }
}

impl std::str::FromStr for ExistentialMode {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "proof" => Ok(ExistentialMode::Proof),
            "witness" => Ok(ExistentialMode::Witness),
            _ => Err(format!(
                "Unknown existential mode '{}'; expected 'proof' or 'witness'",
                s
            )),
        }
    }
}

/// Policy controlling when generated Lean artifacts are retained after verification.
#[non_exhaustive]
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum ArtifactRetention {
    /// Keep artifacts only when proofs fail or have unknown/timed-out status (default).
    /// Successful runs clean up artifacts.
    #[default]
    OnFailure,
    /// Keep artifacts only after a fully successful run.
    OnSuccess,
    /// Always keep artifacts regardless of outcome.
    Always,
    /// Never keep artifacts (remove after every run).
    Never,
}

impl std::fmt::Display for ArtifactRetention {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ArtifactRetention::OnFailure => write!(f, "on-failure"),
            ArtifactRetention::OnSuccess => write!(f, "on-success"),
            ArtifactRetention::Always => write!(f, "always"),
            ArtifactRetention::Never => write!(f, "never"),
        }
    }
}

impl std::str::FromStr for ArtifactRetention {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "on-failure" => Ok(ArtifactRetention::OnFailure),
            "on-success" => Ok(ArtifactRetention::OnSuccess),
            "always" => Ok(ArtifactRetention::Always),
            "never" => Ok(ArtifactRetention::Never),
            _ => Err(format!(
                "Unknown artifact retention policy '{}'; expected 'on-failure', 'on-success', 'always', or 'never'",
                s
            )),
        }
    }
}

/// Determine whether artifacts should be retained given the retention policy
/// and whether verification succeeded.
pub fn should_retain_artifacts(policy: ArtifactRetention, verification_succeeded: bool) -> bool {
    match policy {
        ArtifactRetention::Always => true,
        ArtifactRetention::Never => false,
        ArtifactRetention::OnFailure => !verification_succeeded,
        ArtifactRetention::OnSuccess => verification_succeeded,
    }
}

/// Remove stale verification workspaces and logs under `out_dir`.
/// Returns a list of paths that were removed.
pub fn clean_artifacts(out_dir: &Path) -> std::io::Result<Vec<PathBuf>> {
    remove_generated_workspace_paths(out_dir)
}

const GENERATED_WORKSPACE_PATHS: [&str; 11] = [
    "AikenVerify",
    "AikenVerify.lean",
    "cbor",
    "manifest.json",
    "lakefile.lean",
    "lean-toolchain",
    "logs",
    ".lake/build/lib/AikenVerify",
    ".lake/build/ir/AikenVerify",
    ".lake/build/bin/AikenVerify",
    ".lake/build/obj/AikenVerify",
];

const VERIFY_WORKSPACE_MARKERS: [&str; 6] = [
    "AikenVerify",
    "AikenVerify.lean",
    ".lake/build/lib/AikenVerify",
    ".lake/build/ir/AikenVerify",
    ".lake/build/bin/AikenVerify",
    ".lake/build/obj/AikenVerify",
];

const GENERIC_WORKSPACE_PATHS: [&str; 5] = [
    "cbor",
    "manifest.json",
    "lakefile.lean",
    "lean-toolchain",
    "logs",
];

fn ensure_path_stays_within_workspace(
    path: &Path,
    metadata: &fs::Metadata,
    canonical_out_dir: &Path,
) -> std::io::Result<()> {
    let parent = path.parent().unwrap_or(path);
    let resolved_parent = fs::canonicalize(parent)?;
    if !resolved_parent.starts_with(canonical_out_dir) {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidInput,
            format!(
                "Refusing to clean '{}': resolved parent '{}' escapes workspace root '{}'.",
                path.display(),
                resolved_parent.display(),
                canonical_out_dir.display()
            ),
        ));
    }

    if !metadata.file_type().is_symlink() {
        let resolved_path = fs::canonicalize(path)?;
        if !resolved_path.starts_with(canonical_out_dir) {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                format!(
                    "Refusing to clean '{}': resolved target '{}' escapes workspace root '{}'.",
                    path.display(),
                    resolved_path.display(),
                    canonical_out_dir.display()
                ),
            ));
        }
    }

    Ok(())
}

fn remove_path_if_exists(path: &Path, canonical_out_dir: &Path) -> std::io::Result<bool> {
    let metadata = match fs::symlink_metadata(path) {
        Ok(metadata) => metadata,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(false),
        Err(e) => return Err(e),
    };

    ensure_path_stays_within_workspace(path, &metadata, canonical_out_dir)?;

    if metadata.is_dir() && !metadata.file_type().is_symlink() {
        fs::remove_dir_all(path)
    } else {
        fs::remove_file(path)
    }?;

    Ok(true)
}

fn looks_like_verify_workspace(out_dir: &Path) -> bool {
    VERIFY_WORKSPACE_MARKERS
        .iter()
        .any(|rel| out_dir.join(rel).exists())
}

fn is_generic_workspace_path(rel: &str) -> bool {
    GENERIC_WORKSPACE_PATHS.contains(&rel)
}

fn remove_generated_workspace_paths(out_dir: &Path) -> std::io::Result<Vec<PathBuf>> {
    if out_dir
        .components()
        .any(|component| matches!(component, Component::ParentDir))
    {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidInput,
            format!(
                "Invalid verification workspace path '{}': parent directory segments ('..') are not allowed.",
                out_dir.display()
            ),
        ));
    }

    let mut removed = Vec::new();
    if !out_dir.exists() {
        return Ok(removed);
    }
    let canonical_out_dir = fs::canonicalize(out_dir)?;

    let has_verify_workspace_markers = looks_like_verify_workspace(out_dir);
    let mut blocked_generic = Vec::new();

    for rel in GENERATED_WORKSPACE_PATHS {
        let path = out_dir.join(rel);
        if !has_verify_workspace_markers && is_generic_workspace_path(rel) {
            if path.exists() {
                blocked_generic.push(path);
            }
            continue;
        }

        if remove_path_if_exists(&path, &canonical_out_dir)? {
            removed.push(path);
        }
    }

    if !has_verify_workspace_markers && !blocked_generic.is_empty() {
        let paths = blocked_generic
            .iter()
            .map(|path| path.display().to_string())
            .collect::<Vec<_>>()
            .join(", ");
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidInput,
            format!(
                "Refusing to clean '{}': target does not look like an Aiken verify workspace (missing AikenVerify markers). Found generic verification paths: {paths}",
                out_dir.display()
            ),
        ));
    }

    Ok(removed)
}

/// Clear generated verification outputs before writing a fresh workspace.
///
/// This intentionally preserves static dependencies and caches (`PlutusCore`,
/// `.lake/packages`, and non-AikenVerify build outputs) so successive
/// `verify run` calls can reuse expensive Lean build artifacts.
pub fn clear_generated_workspace(out_dir: &Path) -> miette::Result<()> {
    remove_generated_workspace_paths(out_dir).map_err(|e| {
        miette::miette!(
            "Failed to clear generated verification workspace at {}: {e}",
            out_dir.display(),
        )
    })?;

    Ok(())
}

/// Configuration for Lean workspace generation
#[non_exhaustive]
pub struct VerifyConfig {
    pub out_dir: PathBuf,
    pub cek_budget: u64,
    /// Git revision (commit, tag, or branch) for the Blaster dependency.
    pub blaster_rev: String,
    /// Git revision (commit, tag, or branch) for the PlutusCore dependency.
    pub plutus_core_rev: String,
    /// Strategy for `fail once` tests (default: Witness).
    pub existential_mode: ExistentialMode,
    /// Verification target mode (default: PropertyWrapper).
    pub target: VerificationTargetKind,
    /// Plutus language version used by the exported property tests.
    pub plutus_version: PlutusVersion,
    /// Project root used to compute manifest provenance hashes when package
    /// sources are available locally.
    pub project_root: Option<PathBuf>,
    /// Explicit path to the PlutusCore Lean library. When `None` and no
    /// `PLUTUS_CORE_DIR` env var is set, PlutusCore is pulled from git.
    pub plutus_core_dir: Option<PathBuf>,
    /// Maximum bytes of raw stdout/stderr to retain on the final
    /// `VerifyResult`. When the captured stream exceeds this cap, the
    /// last `raw_output_bytes` bytes are kept and the full stream is
    /// persisted under `<out_dir>/logs/lake_build.{stdout,stderr}.log`.
    /// `0` disables truncation entirely.
    pub raw_output_bytes: usize,
    /// HIDDEN DEBUG flag (default: `false`). When `true`, sub-generator
    /// predicates whose bodies cannot be statically inlined are emitted as
    /// `def {pred} : Data → Data → Prop := fun _ _ => True` rather than
    /// triggering the hard `E0018` error. Each such widening contributes
    /// to `over_approximations` and forces the proof verdict to `Partial`
    /// (because it is universally provable on the sub-generator domain).
    /// Surfaced via the `--allow-vacuous-subgenerators` CLI flag, which
    /// is hidden from `--help`. Use only for debugging vacuous proof
    /// regressions; production runs MUST leave this `false`.
    pub allow_vacuous_subgenerators: bool,
}

impl VerifyConfig {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        out_dir: PathBuf,
        cek_budget: u64,
        blaster_rev: String,
        plutus_core_rev: String,
        existential_mode: ExistentialMode,
        target: VerificationTargetKind,
        plutus_version: PlutusVersion,
        project_root: Option<PathBuf>,
        plutus_core_dir: Option<PathBuf>,
        raw_output_bytes: usize,
        allow_vacuous_subgenerators: bool,
    ) -> Self {
        Self {
            out_dir,
            cek_budget,
            blaster_rev,
            plutus_core_rev,
            existential_mode,
            target,
            plutus_version,
            project_root,
            plutus_core_dir,
            raw_output_bytes,
            allow_vacuous_subgenerators,
        }
    }
}

/// Caveat associated with a generated proof file. Surfaces the open
/// sub-obligation, if any, that downstream reporting must distinguish
/// from a fully-proved theorem.
///
/// `Witness` is reserved for state-machine `native_decide` proofs that
/// only verify concrete witness instances (commit 5 wires this in;
/// today nothing in production emits it but the wiring must work).
#[non_exhaustive]
#[derive(Debug, Clone)]
pub enum ProofCaveat {
    /// Universal proof, no open obligations.
    None,
    /// Build will succeed but Phase 2 / sub-obligation is `sorry`-closed.
    Partial(String),
    /// Build will succeed but proof covers only concrete witness instance(s).
    Witness(WitnessProofNote),
}

/// Witness-only proof metadata. Emitted alongside `native_decide` halt /
/// error theorems where each witness is verified by reduction rather than
/// universally quantified.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[non_exhaustive]
pub struct WitnessProofNote {
    pub instances: usize,
    /// CBOR-hex encoded Plutus Data witnesses.
    pub witnesses: Vec<String>,
    pub note: String,
}

/// Reconstruct a `ProofCaveat` from the manifest entry's caveat fields.
/// `witness_proof_note` takes precedence if both happen to be set
/// (production code never sets both today, but commit 5 may begin
/// emitting `WitnessProofNote` and the precedence keeps the surface
/// compatible).
fn manifest_entry_caveat(entry: &ManifestEntry) -> ProofCaveat {
    if let Some(note) = entry.witness_proof_note.clone() {
        ProofCaveat::Witness(note)
    } else if let Some(note) = entry.partial_proof_note.clone() {
        ProofCaveat::Partial(note)
    } else {
        ProofCaveat::None
    }
}

fn soundness_lint_note_for_success_proof(proof_content: &str) -> Option<String> {
    let has_placeholder_proof = proof_content.lines().any(|line| {
        let trimmed = line.trim();
        !trimmed.starts_with("--")
            && (trimmed == "sorry"
                || trimmed.starts_with("sorry ")
                || trimmed.ends_with(" by sorry")
                || trimmed == "admit"
                || trimmed.starts_with("admit ")
                || trimmed.ends_with(" by admit"))
    });

    let widening_markers = proof_content
        .lines()
        .filter(|line| line.contains("[WIDENED:") || line.contains("widened to True"))
        .map(|line| line.trim().to_string())
        .take(3)
        .collect::<Vec<_>>();

    let mut findings = Vec::new();
    if has_placeholder_proof {
        findings.push(
            "generated Lean contains `sorry`/`admit`; one or more proof obligations remain open"
                .to_string(),
        );
    }
    if !widening_markers.is_empty() {
        findings.push(format!(
            "generated domain/relation contains acknowledged placeholder widening(s): {}",
            widening_markers.join(" | ")
        ));
    }

    (!findings.is_empty()).then(|| findings.join("; "))
}

fn apply_soundness_lint_to_caveat(caveat: ProofCaveat, proof_content: &str) -> ProofCaveat {
    match caveat {
        ProofCaveat::None => soundness_lint_note_for_success_proof(proof_content)
            .map(ProofCaveat::Partial)
            .unwrap_or(ProofCaveat::None),
        other => other,
    }
}

fn append_partial_caveat_note(caveat: ProofCaveat, note: impl Into<String>) -> ProofCaveat {
    let note = note.into();
    match caveat {
        ProofCaveat::None => ProofCaveat::Partial(note),
        ProofCaveat::Partial(existing) => {
            if existing.contains(&note) {
                ProofCaveat::Partial(existing)
            } else {
                ProofCaveat::Partial(format!("{note}; {existing}"))
            }
        }
        ProofCaveat::Witness(existing) => {
            ProofCaveat::Partial(format!("{note}; witness-only detail: {}", existing.note))
        }
    }
}

fn manifest_entry_caveat_with_soundness_lint(
    out_dir: &Path,
    manifest: &GeneratedManifest,
    entry: &ManifestEntry,
) -> ProofCaveat {
    let caveat = manifest_entry_caveat(entry);
    if !matches!(caveat, ProofCaveat::None) {
        return caveat;
    }

    if let Some(note) = manifest.compatibility_downgrade_note_for_entry(entry) {
        return ProofCaveat::Partial(note);
    }

    let proof_path = out_dir.join(&entry.lean_file);
    let Ok(proof_content) = fs::read_to_string(proof_path) else {
        return caveat;
    };

    apply_soundness_lint_to_caveat(caveat, &proof_content)
}

/// Schema version for `aiken verify run --generate-only` manifest output.
pub const GENERATED_MANIFEST_SCHEMA_VERSION: u32 = 2;
pub const GENERATE_ONLY_VERSION: &str = "2.0.0";

#[non_exhaustive]
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ManifestTestMode {
    Normal,
    Fail,
    FailOnce,
}

impl ManifestTestMode {
    fn from_on_test_failure(on_test_failure: &OnTestFailure) -> Self {
        match on_test_failure {
            OnTestFailure::FailImmediately => Self::Normal,
            OnTestFailure::SucceedEventually => Self::Fail,
            OnTestFailure::SucceedImmediately => Self::FailOnce,
        }
    }
}

impl std::fmt::Display for ManifestTestMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            ManifestTestMode::Normal => "normal",
            ManifestTestMode::Fail => "fail",
            ManifestTestMode::FailOnce => "fail_once",
        })
    }
}

#[non_exhaustive]
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ManifestOnTestFailure {
    FailImmediately,
    SucceedImmediately,
    SucceedEventually,
}

impl From<&OnTestFailure> for ManifestOnTestFailure {
    fn from(on_test_failure: &OnTestFailure) -> Self {
        match on_test_failure {
            OnTestFailure::FailImmediately => Self::FailImmediately,
            OnTestFailure::SucceedImmediately => Self::SucceedImmediately,
            OnTestFailure::SucceedEventually => Self::SucceedEventually,
        }
    }
}

impl std::fmt::Display for ManifestOnTestFailure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            ManifestOnTestFailure::FailImmediately => "fail_immediately",
            ManifestOnTestFailure::SucceedImmediately => "succeed_immediately",
            ManifestOnTestFailure::SucceedEventually => "succeed_eventually",
        })
    }
}

#[non_exhaustive]
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ManifestDecodePolicy {
    DecodeErrorIsTestFailure,
    DecodeErrorIsExportError,
}

impl std::fmt::Display for ManifestDecodePolicy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            ManifestDecodePolicy::DecodeErrorIsTestFailure => "decode_error_is_test_failure",
            ManifestDecodePolicy::DecodeErrorIsExportError => "decode_error_is_export_error",
        })
    }
}

fn generated_decode_policy() -> ManifestDecodePolicy {
    ManifestDecodePolicy::DecodeErrorIsTestFailure
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize, Default)]
#[non_exhaustive]
pub struct ManifestAikenMetadata {
    #[serde(default, skip_serializing_if = "String::is_empty")]
    pub version: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub commit: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub prelude_hash: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub fuzz_package_hash: Option<String>,
}

impl ManifestAikenMetadata {
    fn is_empty(&self) -> bool {
        self.version.is_empty()
            && self.commit.is_none()
            && self.prelude_hash.is_none()
            && self.fuzz_package_hash.is_none()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize, Default)]
#[non_exhaustive]
pub struct ManifestCekBudget {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub cpu: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub mem: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub fuel: Option<u64>,
}

impl ManifestCekBudget {
    fn is_empty(&self) -> bool {
        self.cpu.is_none() && self.mem.is_none() && self.fuel.is_none()
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, Default)]
#[non_exhaustive]
pub struct ManifestExecutionMetadata {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub plutus_version: Option<PlutusVersion>,
    #[serde(default, skip_serializing_if = "ManifestCekBudget::is_empty")]
    pub cek_budget: ManifestCekBudget,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub decode_policy: Option<ManifestDecodePolicy>,
}

impl ManifestExecutionMetadata {
    fn is_empty(&self) -> bool {
        self.plutus_version.is_none() && self.cek_budget.is_empty() && self.decode_policy.is_none()
    }
}

/// Entry in the generated manifest
#[derive(Debug, serde::Serialize, serde::Deserialize)]
#[non_exhaustive]
pub struct ManifestEntry {
    pub id: String,
    pub aiken_module: String,
    pub aiken_name: String,
    pub lean_module: String,
    pub lean_theorem: String,
    pub lean_file: String,
    pub flat_file: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub fuzzer_flat_file: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub return_mode: Option<TestReturnMode>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub test_mode: Option<ManifestTestMode>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub on_test_failure: Option<ManifestOnTestFailure>,
    /// Blake2b-256 of the exported flat UPLC bytes for the property wrapper or
    /// handler currently being verified.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub property_uplc_hash: Option<String>,
    /// Hash of the exact property harness imported into Lean. Today this is the
    /// script hash of the same exported UPLC bytes used for `property_uplc_hash`.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub property_harness_hash: Option<String>,
    /// Blake2b-256 of the exported flat UPLC bytes for the fuzzer sampler.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub fuzzer_uplc_hash: Option<String>,
    /// Hash of the exact fuzzer harness imported into Lean. Today this is the
    /// script hash of the same exported UPLC bytes used for `fuzzer_uplc_hash`.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub fuzzer_harness_hash: Option<String>,
    /// Blake2b-256 of the serialized fuzzer-domain lowering inputs carried by
    /// this manifest entry.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub fuzzer_model_hash: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub domain: Option<LoweredDomain>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub input_type: Option<InputValueBridge>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub sampler: Option<SamplerMetadata>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub compatibility_limitations: Vec<String>,
    /// Whether this test generates a separate `_alwaysTerminating` theorem.
    /// Void-mode and existential forms omit the termination theorem.
    #[serde(default)]
    pub has_termination_theorem: bool,
    /// Whether this test generates a separate `_equivalence` theorem.
    #[serde(default)]
    pub has_equivalence_theorem: bool,
    /// Number of constraints dropped or widened during TransitionProp lowering.
    /// Non-zero only for two-phase state-machine trace theorems.
    #[serde(default, skip_serializing_if = "is_zero")]
    pub over_approximations: usize,
    /// When set, the emitted Lean proof file contains `sorry` (or otherwise
    /// leaves a sub-obligation open). Surfaces as `ProofStatus::Partial` in
    /// the verification summary even if the build itself succeeds.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub partial_proof_note: Option<String>,
    /// When set, the emitted Lean proof file proves the property only on
    /// concrete witness instance(s) (e.g. `native_decide` per-witness
    /// theorems). Surfaces as `ProofStatus::WitnessProved` in the
    /// verification summary even if the build itself succeeds.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub witness_proof_note: Option<WitnessProofNote>,
}

/// A test that was skipped during workspace generation (unsupported type, etc.)
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[non_exhaustive]
pub struct SkippedTest {
    pub name: String,
    pub module: String,
    pub reason: String,
    #[serde(default = "unsupported_status")]
    pub status: VerificationStatus,
    #[serde(default = "unsupported_certification")]
    pub certification: Certification,
    #[serde(default = "default_trust_profile")]
    pub trust_profile: TrustProfile,
    #[serde(default = "default_result_domain")]
    pub domain: LoweredDomain,
}

impl SkippedTest {
    pub fn new(name: String, module: String, reason: String) -> Self {
        let domain = LoweredDomain::skipped(reason.clone());
        Self {
            name,
            module,
            reason,
            status: VerificationStatus::Unsupported,
            certification: Certification::Unsupported,
            trust_profile: default_trust_profile(),
            domain,
        }
    }
}

fn legacy_generated_manifest_schema_version() -> u32 {
    1
}

/// Result of workspace generation
#[derive(Debug, serde::Serialize, serde::Deserialize)]
#[non_exhaustive]
pub struct GeneratedManifest {
    #[serde(default = "legacy_generated_manifest_schema_version")]
    pub schema_version: u32,
    pub version: String,
    #[serde(default, skip_serializing_if = "ManifestAikenMetadata::is_empty")]
    pub aiken: ManifestAikenMetadata,
    #[serde(default, skip_serializing_if = "ManifestExecutionMetadata::is_empty")]
    pub execution: ManifestExecutionMetadata,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub compatibility_limitations: Vec<String>,
    pub tests: Vec<ManifestEntry>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub skipped: Vec<SkippedTest>,
}

impl GeneratedManifest {
    pub fn new(
        version: impl Into<String>,
        tests: Vec<ManifestEntry>,
        skipped: Vec<SkippedTest>,
    ) -> Self {
        Self {
            schema_version: GENERATED_MANIFEST_SCHEMA_VERSION,
            version: version.into(),
            aiken: ManifestAikenMetadata::default(),
            execution: ManifestExecutionMetadata::default(),
            compatibility_limitations: Vec::new(),
            tests,
            skipped,
        }
    }

    pub fn empty(version: impl Into<String>) -> Self {
        Self::new(version, Vec::new(), Vec::new())
    }

    pub fn compatibility_downgrade_note_for_entry(&self, entry: &ManifestEntry) -> Option<String> {
        let mut missing = Vec::new();

        if entry.return_mode.is_none() {
            missing.push("return_mode");
        }
        if entry.test_mode.is_none() {
            missing.push("test_mode");
        }
        if entry.on_test_failure.is_none() {
            missing.push("on_test_failure");
        }
        if self.execution.plutus_version.is_none() {
            missing.push("execution.plutus_version");
        }
        if self.execution.cek_budget.fuel.is_none() {
            missing.push("execution.cek_budget.fuel");
        }
        if self.execution.decode_policy.is_none() {
            missing.push("execution.decode_policy");
        }
        if entry.domain.is_none() {
            missing.push("domain");
        }
        if entry
            .domain
            .as_ref()
            .is_some_and(|domain| domain.certificate == DomainCertificate::TrustedVersionedModel)
            && entry.fuzzer_model_hash.is_none()
        {
            missing.push("fuzzer_model_hash");
        }
        if entry.input_type.is_none() {
            missing.push("input_type");
        }
        if entry_requires_sampler_metadata(entry) && entry.sampler.is_none() {
            missing.push("sampler");
        }

        if missing.is_empty() {
            None
        } else {
            Some(format!(
                "Manifest schema v{} lacks explicit {}; downgraded to Partial instead of assuming proof-grade mode/execution/domain/value-bridge/sampler semantics.",
                self.schema_version,
                missing.join(", "),
            ))
        }
    }
}

fn blake2b_256_hex(bytes: &[u8]) -> String {
    Hasher::<256>::hash(bytes).to_string()
}

const SAMPLER_HARNESS_ABI: &str = "aiken_fuzzer_prng_to_option_pair_prng_data_v1";

fn entry_requires_sampler_metadata(entry: &ManifestEntry) -> bool {
    entry
        .domain
        .as_ref()
        .is_some_and(|domain| matches!(domain.relation, DomainRel::SamplerReturns { .. }))
        || entry.witness_proof_note.is_some()
}

fn sampler_metadata(
    run_kind: SamplerRunKind,
    prng_mode: SamplerPrngMode,
    generation_seed: Option<u32>,
    replay_choices_hex: Option<String>,
) -> SamplerMetadata {
    SamplerMetadata {
        harness_abi: SAMPLER_HARNESS_ABI.to_string(),
        run_kind,
        prng_mode,
        generation_seed,
        replay_choices_hex,
        decode_policy: Some(generated_decode_policy()),
    }
}

fn default_sampler_relation_metadata() -> SamplerMetadata {
    sampler_metadata(
        SamplerRunKind::SeededGeneration,
        SamplerPrngMode::Seeded,
        None,
        None,
    )
}

fn default_witness_sampler_metadata() -> SamplerMetadata {
    sampler_metadata(
        SamplerRunKind::SeededGeneration,
        SamplerPrngMode::Seeded,
        Some(CONCRETE_WITNESS_BASE_SEED),
        None,
    )
}

fn sampler_relation_summary(metadata: &SamplerMetadata) -> String {
    match metadata.run_kind {
        SamplerRunKind::SeededGeneration => {
            "Exact seeded-generation support via the exported Aiken fuzzer harness ABI.".to_string()
        }
        SamplerRunKind::ReplayOnly => {
            "Replay-only support via the exported Aiken fuzzer harness ABI.".to_string()
        }
        SamplerRunKind::WitnessReplay => {
            "Concrete witness replay through the exported Aiken fuzzer harness ABI.".to_string()
        }
    }
}

fn sampler_relation_supported(
    test: &ExportedPropertyTest,
    target: &VerificationTargetKind,
    existential_mode: ExistentialMode,
) -> bool {
    matches!(test.semantics, FuzzerSemantics::Opaque { .. })
        && matches!(
            test.fuzzer_output_type,
            FuzzerOutputType::Data | FuzzerOutputType::Unsupported(_)
        )
        && !matches!(target, VerificationTargetKind::Equivalence)
        && !matches!(existential_mode, ExistentialMode::Witness)
}

fn uplc_script_hash_hex(bytes: &[u8], plutus_version: PlutusVersion) -> String {
    match plutus_version {
        PlutusVersion::V1 => pallas_primitives::conway::PlutusScript::<1>(bytes.to_vec().into())
            .compute_hash()
            .to_string(),
        PlutusVersion::V2 => pallas_primitives::conway::PlutusScript::<2>(bytes.to_vec().into())
            .compute_hash()
            .to_string(),
        PlutusVersion::V3 => pallas_primitives::conway::PlutusScript::<3>(bytes.to_vec().into())
            .compute_hash()
            .to_string(),
    }
}

fn hash_directory_tree(root: &Path) -> miette::Result<Option<String>> {
    if !root.exists() {
        return Ok(None);
    }

    let mut files = WalkDir::new(root)
        .into_iter()
        .map(|entry| {
            entry.map_err(|err| miette::miette!("Failed to walk {}: {err}", root.display()))
        })
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .filter(|entry| entry.file_type().is_file())
        .map(|entry| entry.into_path())
        .collect::<Vec<_>>();

    files.sort();

    let mut hasher = Hasher::<256>::new();
    for file in files {
        let relative = file.strip_prefix(root).map_err(|err| {
            miette::miette!(
                "Failed to strip hash root {} from {}: {err}",
                root.display(),
                file.display()
            )
        })?;
        let relative = relative.to_string_lossy().replace('\\', "/");
        hasher.input(relative.as_bytes());
        hasher.input(&[0]);
        hasher.input(&fs::read(&file).map_err(|err| {
            miette::miette!("Failed to read {} for hashing: {err}", file.display())
        })?);
        hasher.input(&[0xff]);
    }

    Ok(Some(hasher.finalize().to_string()))
}

fn manifest_aiken_metadata(
    config: &VerifyConfig,
) -> miette::Result<(ManifestAikenMetadata, Vec<String>)> {
    let version = compiler_version(false);
    let commit = compiler_version(true)
        .split_once('+')
        .and_then(|(_, commit)| {
            (!commit.is_empty() && commit != "unknown").then_some(commit.to_string())
        });

    let prelude_hash = match config.project_root.as_deref() {
        Some(project_root) => {
            hash_directory_tree(&project_root.join("build/packages/aiken-lang-stdlib/lib/aiken"))?
        }
        None => None,
    };

    let fuzz_package_hash = match config.project_root.as_deref() {
        Some(project_root) => {
            let installed = project_root.join("build/packages/aiken-lang-fuzz/lib/aiken");
            if installed.exists() {
                hash_directory_tree(&installed)?
            } else {
                hash_directory_tree(&project_root.join("lib/aiken/fuzz"))?
            }
        }
        None => None,
    };

    let mut limitations = Vec::new();
    if commit.is_none() {
        limitations.push(
            "aiken.commit unavailable: build metadata did not include a git commit hash."
                .to_string(),
        );
    }
    if prelude_hash.is_none() {
        limitations.push(
            "prelude_hash unavailable: stdlib package sources were not found under build/packages/aiken-lang-stdlib/lib/aiken.".to_string(),
        );
    }
    if fuzz_package_hash.is_none() {
        limitations.push(
            "fuzz_package_hash unavailable: fuzz package sources were not found under build/packages/aiken-lang-fuzz/lib/aiken or lib/aiken/fuzz.".to_string(),
        );
    }

    Ok((
        ManifestAikenMetadata {
            version,
            commit,
            prelude_hash,
            fuzz_package_hash,
        },
        limitations,
    ))
}

fn manifest_execution_metadata(config: &VerifyConfig) -> (ManifestExecutionMetadata, Vec<String>) {
    (
        ManifestExecutionMetadata {
            plutus_version: Some(config.plutus_version),
            cek_budget: ManifestCekBudget {
                cpu: None,
                mem: None,
                fuel: Some(config.cek_budget),
            },
            decode_policy: Some(generated_decode_policy()),
        },
        vec![
            "execution.cek_budget records only the unified CEK fuel bound; CPU and memory sub-budgets are not currently exported.".to_string(),
        ],
    )
}

fn flat_hex_bytes(label: &str, flat_hex: &str) -> miette::Result<Vec<u8>> {
    hex::decode(flat_hex).map_err(|err| {
        miette::miette!("Failed to decode {label} flat hex for manifest hashing: {err}")
    })
}

fn manifest_program_hashes(
    label: &str,
    flat_hex: &str,
    plutus_version: PlutusVersion,
) -> miette::Result<(String, String)> {
    let bytes = flat_hex_bytes(label, flat_hex)?;
    Ok((
        blake2b_256_hex(&bytes),
        uplc_script_hash_hex(&bytes, plutus_version),
    ))
}

fn fuzzer_model_hash_for_test(test: &ExportedPropertyTest) -> miette::Result<String> {
    let structure_for_hash = test
        .fuzzer_structure
        .as_ref()
        .map(ExportedFuzzerStructure::without_source_spans);
    let bytes = serde_json::to_vec(&serde_json::json!({
        "fuzzer_type": &test.fuzzer_type,
        "fuzzer_output_type": &test.fuzzer_output_type,
        "constraint": &test.constraint,
        "semantics": &test.semantics,
        "fuzzer_structure": &structure_for_hash,
        "fuzzer_data_schema": &test.fuzzer_data_schema,
        "inner_data_schemas": &test.inner_data_schemas,
    }))
    .map_err(|err| miette::miette!("Failed to serialize fuzzer model metadata: {err}"))?;
    Ok(blake2b_256_hex(&bytes))
}

fn push_lowering_step(path: &mut Vec<String>, step: &str) {
    if !path.iter().any(|existing| existing == step) {
        path.push(step.to_string());
    }
}

fn widening_kind_label(kind: TransitionWideningKind) -> &'static str {
    match kind {
        TransitionWideningKind::Relation => "relation",
        TransitionWideningKind::DataFreshening => "data_freshening",
        TransitionWideningKind::BoolFreshening => "bool_freshening",
        TransitionWideningKind::Domain => "domain",
        TransitionWideningKind::OpaqueSubGenerator => "opaque_sub_generator",
    }
}

fn transition_prop_domain_widenings(test: &ExportedPropertyTest) -> Vec<Widening> {
    test.transition_prop_lean
        .as_ref()
        .map(transition_prop_widenings)
        .unwrap_or_default()
        .into_iter()
        .map(|widening| Widening {
            kind: widening_kind_label(widening.kind).to_string(),
            message: widening.message,
            allowed_only_under: None,
        })
        .collect()
}

fn placeholder_domain_widenings(proof_content: &str) -> Vec<Widening> {
    proof_content
        .lines()
        .filter_map(|line| {
            let trimmed = line.trim();
            (trimmed.contains("[WIDENED:") || trimmed.contains("widened to True")).then(|| {
                Widening {
                    kind: "placeholder".to_string(),
                    message: trimmed.to_string(),
                    allowed_only_under: Some(TrustProfile::UnsafeDev),
                }
            })
        })
        .collect()
}

fn semantics_is_opaque(semantics: &FuzzerSemantics) -> bool {
    match semantics {
        FuzzerSemantics::Bool
        | FuzzerSemantics::IntRange { .. }
        | FuzzerSemantics::ByteArrayRange { .. }
        | FuzzerSemantics::String
        | FuzzerSemantics::Data
        | FuzzerSemantics::DataWithSchema { .. }
        | FuzzerSemantics::Exact(_)
        | FuzzerSemantics::OneOf(_)
        | FuzzerSemantics::Constructors { .. } => false,
        FuzzerSemantics::Product(items) => items.iter().any(semantics_is_opaque),
        FuzzerSemantics::List { element, .. } => semantics_is_opaque(element),
        FuzzerSemantics::StateMachineTrace {
            transition_semantics,
            output_semantics,
            ..
        } => {
            semantics_is_opaque(&transition_semantics.state_semantics)
                || transition_semantics
                    .step_input_semantics
                    .iter()
                    .any(semantics_is_opaque)
                || semantics_is_opaque(&transition_semantics.label_semantics)
                || semantics_is_opaque(&transition_semantics.event_semantics)
                || semantics_is_opaque(output_semantics)
        }
        FuzzerSemantics::Opaque { .. } => true,
    }
}

fn constraint_is_unsupported(constraint: &FuzzerConstraint) -> bool {
    match constraint {
        FuzzerConstraint::Any
        | FuzzerConstraint::IntRange { .. }
        | FuzzerConstraint::ByteStringLenRange { .. }
        | FuzzerConstraint::Exact(_)
        | FuzzerConstraint::OneOf(_)
        | FuzzerConstraint::DataConstructorTags { .. } => false,
        FuzzerConstraint::Tuple(items) | FuzzerConstraint::And(items) => {
            items.iter().any(constraint_is_unsupported)
        }
        FuzzerConstraint::List { elem, .. } | FuzzerConstraint::Map(elem) => {
            constraint_is_unsupported(elem)
        }
        FuzzerConstraint::Unsupported { .. } => true,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CompactDomainCoverage {
    ExactSupport,
    UniversalOverApprox,
}

#[derive(Debug, Clone)]
struct CompactDomainProfile {
    precision: DomainPrecision,
    coverage: CompactDomainCoverage,
    widenings: Vec<Widening>,
    diagnostics: Vec<String>,
}

fn exact_compact_profile(message: impl Into<String>) -> CompactDomainProfile {
    CompactDomainProfile {
        precision: DomainPrecision::Exact,
        coverage: CompactDomainCoverage::ExactSupport,
        widenings: Vec::new(),
        diagnostics: vec![message.into()],
    }
}

fn overapprox_compact_profile(
    message: impl Into<String>,
    widenings: Vec<Widening>,
) -> CompactDomainProfile {
    CompactDomainProfile {
        precision: DomainPrecision::OverApprox,
        coverage: CompactDomainCoverage::UniversalOverApprox,
        widenings,
        diagnostics: vec![message.into()],
    }
}

fn combine_compact_profiles(
    kind: &str,
    profiles: Vec<CompactDomainProfile>,
) -> Option<CompactDomainProfile> {
    if profiles.is_empty() {
        return None;
    }

    let is_exact = profiles.iter().all(|profile| {
        profile.precision == DomainPrecision::Exact
            && matches!(profile.coverage, CompactDomainCoverage::ExactSupport)
            && profile.widenings.is_empty()
    });

    let mut widenings = Vec::new();
    let mut diagnostics = Vec::new();
    for profile in profiles {
        widenings.extend(profile.widenings);
        diagnostics.extend(profile.diagnostics);
    }

    if is_exact {
        diagnostics.insert(
            0,
            format!(
                "Compact lowering models the {kind} domain exactly through certified child domains."
            ),
        );
        Some(CompactDomainProfile {
            precision: DomainPrecision::Exact,
            coverage: CompactDomainCoverage::ExactSupport,
            widenings,
            diagnostics,
        })
    } else {
        diagnostics.insert(
            0,
            format!(
                "Compact lowering models the {kind} domain conservatively because one or more child domains are over-approximated or widened.",
            ),
        );
        Some(CompactDomainProfile {
            precision: DomainPrecision::OverApprox,
            coverage: CompactDomainCoverage::UniversalOverApprox,
            widenings,
            diagnostics,
        })
    }
}

fn compact_domain_profile(
    output_type: &FuzzerOutputType,
    semantics: &FuzzerSemantics,
) -> Option<CompactDomainProfile> {
    match (output_type, semantics) {
        (
            FuzzerOutputType::Int,
            FuzzerSemantics::IntRange {
                min: Some(min),
                max: Some(max),
            },
        ) => Some(exact_compact_profile(format!(
            "Compact lowering models the integer generator as the exact bounded support [{min}, {max}].",
        ))),
        (_, FuzzerSemantics::Exact(value)) => Some(exact_compact_profile(format!(
            "Compact lowering models the generator as the exact singleton value {value}.",
        ))),
        (_, FuzzerSemantics::OneOf(values)) if !values.is_empty() => {
            Some(exact_compact_profile(format!(
                "Compact lowering models the generator as an exact finite scalar domain of {} case(s).",
                values.len()
            )))
        }
        (_, FuzzerSemantics::Constructors { tags }) if !tags.is_empty() => {
            Some(exact_compact_profile(format!(
                "Compact lowering models the generator as the exact constructor-tag domain {:?}.",
                tags
            )))
        }
        (
            FuzzerOutputType::ByteArray | FuzzerOutputType::String,
            FuzzerSemantics::ByteArrayRange { min_len, max_len },
        ) if min_len.is_some() || max_len.is_some() => {
            let description = match (min_len, max_len) {
                (Some(min), Some(max)) => format!("lengths [{min}, {max}]"),
                (Some(min), None) => format!("lengths >= {min}"),
                (None, Some(max)) => format!("lengths <= {max}"),
                (None, None) => unreachable!("guarded above"),
            };
            Some(overapprox_compact_profile(
                format!(
                    "Compact lowering models byte-array support by exact {description}, but content remains a conservative over-approximation until the sampler/hash semantics are proved.",
                ),
                vec![Widening {
                    kind: "bytearray_content".to_string(),
                    message: "ByteArray length bounds are trusted, but arbitrary content is still over-approximated; exact content support requires sampler semantics or a proved hash/content model.".to_string(),
                    allowed_only_under: None,
                }],
            ))
        }
        (FuzzerOutputType::Pair(first, second), FuzzerSemantics::Product(items))
            if items.len() == 2 =>
        {
            combine_compact_profiles(
                "pair",
                [
                    compact_domain_profile(first.as_ref(), &items[0]),
                    compact_domain_profile(second.as_ref(), &items[1]),
                ]
                .into_iter()
                .collect::<Option<Vec<_>>>()?,
            )
        }
        (FuzzerOutputType::Tuple(types), FuzzerSemantics::Product(items))
            if !types.is_empty() && types.len() == items.len() =>
        {
            combine_compact_profiles(
                "tuple",
                types
                    .iter()
                    .zip(items.iter())
                    .map(|(output_type, semantics)| compact_domain_profile(output_type, semantics))
                    .collect::<Option<Vec<_>>>()?,
            )
        }
        _ => None,
    }
}

#[derive(Debug, Clone)]
struct RelationalProfileDiagnostic {
    code: String,
    severity: DomainDiagnosticSeverity,
    message: String,
    source_span: Option<ExportedSourceSpan>,
}

#[derive(Debug, Clone)]
struct RelationalStructureProfile {
    relation: DomainRel,
    precision: DomainPrecision,
    diagnostics: Vec<String>,
    source_diagnostics: Vec<RelationalProfileDiagnostic>,
    widenings: Vec<Widening>,
}

fn default_exported_semantics(output_type: &FuzzerOutputType) -> FuzzerSemantics {
    match output_type {
        FuzzerOutputType::Bool => FuzzerSemantics::Bool,
        FuzzerOutputType::Int => FuzzerSemantics::IntRange {
            min: None,
            max: None,
        },
        FuzzerOutputType::ByteArray => FuzzerSemantics::ByteArrayRange {
            min_len: None,
            max_len: None,
        },
        FuzzerOutputType::String => FuzzerSemantics::String,
        FuzzerOutputType::Data | FuzzerOutputType::Unsupported(_) => FuzzerSemantics::Data,
        FuzzerOutputType::List(inner) => FuzzerSemantics::List {
            element: Box::new(default_exported_semantics(inner.as_ref())),
            min_len: None,
            max_len: None,
        },
        FuzzerOutputType::Tuple(items) => {
            FuzzerSemantics::Product(items.iter().map(default_exported_semantics).collect())
        }
        FuzzerOutputType::Pair(first, second) => FuzzerSemantics::Product(vec![
            default_exported_semantics(first.as_ref()),
            default_exported_semantics(second.as_ref()),
        ]),
    }
}

fn mapper_shape_is_exact(shape: &ExportedMapperShape) -> bool {
    !matches!(shape, ExportedMapperShape::Unknown)
}

fn structure_source_span(structure: &ExportedFuzzerStructure) -> Option<ExportedSourceSpan> {
    match structure {
        ExportedFuzzerStructure::Opaque { source_span, .. }
        | ExportedFuzzerStructure::Primitive { source_span, .. }
        | ExportedFuzzerStructure::Map { source_span, .. }
        | ExportedFuzzerStructure::MapN { source_span, .. }
        | ExportedFuzzerStructure::Bind { source_span, .. }
        | ExportedFuzzerStructure::Product { source_span, .. }
        | ExportedFuzzerStructure::List { source_span, .. }
        | ExportedFuzzerStructure::Choice { source_span, .. }
        | ExportedFuzzerStructure::Filter { source_span, .. }
        | ExportedFuzzerStructure::StateMachineTrace { source_span, .. } => *source_span,
    }
}

fn relation_contains_sampler_returns(relation: &DomainRel) -> bool {
    match relation {
        DomainRel::SamplerReturns { .. } => true,
        DomainRel::Exists { body, .. } | DomainRel::Not { body } => {
            relation_contains_sampler_returns(body)
        }
        DomainRel::Bind { source, result } => {
            relation_contains_sampler_returns(source) || relation_contains_sampler_returns(result)
        }
        DomainRel::And { items }
        | DomainRel::Or { items }
        | DomainRel::Product { elements: items }
        | DomainRel::Choice {
            branches: items, ..
        }
        | DomainRel::Image { sources: items, .. } => {
            items.iter().any(relation_contains_sampler_returns)
        }
        DomainRel::List { element, .. }
        | DomainRel::Filter {
            source: element, ..
        } => relation_contains_sampler_returns(element),
        DomainRel::Constraint { .. }
        | DomainRel::Semantics { .. }
        | DomainRel::Witness { .. }
        | DomainRel::TrueWithExplicitWidening { .. }
        | DomainRel::Unknown { .. } => false,
    }
}

fn merge_structure_precision(children: &[RelationalStructureProfile]) -> DomainPrecision {
    if children
        .iter()
        .any(|child| child.precision == DomainPrecision::Unknown)
    {
        DomainPrecision::Unknown
    } else if children
        .iter()
        .all(|child| child.precision == DomainPrecision::Exact)
    {
        DomainPrecision::Exact
    } else {
        DomainPrecision::OverApprox
    }
}

fn structure_support_upper_bound(structure: &ExportedFuzzerStructure) -> Option<usize> {
    match structure {
        ExportedFuzzerStructure::Primitive {
            known_constraint, ..
        } => match known_constraint {
            Some(FuzzerConstraint::Exact(_)) => Some(1),
            Some(FuzzerConstraint::OneOf(values)) => Some(values.len()),
            Some(FuzzerConstraint::DataConstructorTags { tags }) => Some(tags.len()),
            Some(FuzzerConstraint::IntRange { min, max }) => {
                let min = min.parse::<i128>().ok()?;
                let max = max.parse::<i128>().ok()?;
                (min <= max).then_some((max - min + 1) as usize)
            }
            _ => None,
        },
        ExportedFuzzerStructure::Choice { branches, .. } => {
            branches.iter().try_fold(0usize, |acc, branch| {
                Some(acc.saturating_add(structure_support_upper_bound(branch)?))
            })
        }
        _ => None,
    }
}

fn merge_choice_precision(
    branches: &[RelationalStructureProfile],
    may_fail: bool,
) -> DomainPrecision {
    if branches.is_empty() {
        DomainPrecision::Unknown
    } else {
        let merged = merge_structure_precision(branches);
        if may_fail && merged == DomainPrecision::Exact {
            DomainPrecision::OverApprox
        } else {
            merged
        }
    }
}

fn primitive_structure_profile(
    output_type: &FuzzerOutputType,
    known_constraint: Option<&FuzzerConstraint>,
) -> RelationalStructureProfile {
    match known_constraint {
        Some(FuzzerConstraint::ByteStringLenRange { min_len, max_len })
            if matches!(output_type, FuzzerOutputType::ByteArray | FuzzerOutputType::String) =>
        {
            RelationalStructureProfile {
                relation: DomainRel::Constraint {
                    constraint: FuzzerConstraint::ByteStringLenRange {
                        min_len: *min_len,
                        max_len: *max_len,
                    },
                },
                precision: DomainPrecision::OverApprox,
                diagnostics: vec![format!(
                    "Byte-array content is not fully modeled; only the exact length support [{min_len}, {max_len}] is captured.",
                )],
                source_diagnostics: Vec::new(),
                widenings: vec![Widening {
                    kind: "bytearray_content".to_string(),
                    message: "ByteArray length is exact, but arbitrary content remains over-approximated until hash/content semantics are proved.".to_string(),
                    allowed_only_under: None,
                }],
            }
        }
        Some(constraint) => RelationalStructureProfile {
            relation: DomainRel::Constraint {
                constraint: constraint.clone(),
            },
            precision: DomainPrecision::Exact,
            diagnostics: vec!["Compact primitive support is modeled exactly from the trusted fuzzer model.".to_string()],
            source_diagnostics: Vec::new(),
            widenings: Vec::new(),
        },
        None => RelationalStructureProfile {
            relation: DomainRel::Semantics {
                semantics: default_exported_semantics(output_type),
            },
            precision: DomainPrecision::OverApprox,
            diagnostics: vec!["Primitive branch lacks an explicit compact constraint and falls back to a conservative output-type semantics.".to_string()],
            source_diagnostics: Vec::new(),
            widenings: Vec::new(),
        },
    }
}

fn collect_profile_source_diagnostics(
    children: &[RelationalStructureProfile],
) -> Vec<RelationalProfileDiagnostic> {
    children
        .iter()
        .flat_map(|child| child.source_diagnostics.clone())
        .collect()
}

fn relational_profile_from_structure(
    structure: &ExportedFuzzerStructure,
) -> RelationalStructureProfile {
    relational_profile_from_structure_with_context(structure, true)
}

fn relational_profile_from_structure_with_context(
    structure: &ExportedFuzzerStructure,
    is_root: bool,
) -> RelationalStructureProfile {
    let source_span = structure_source_span(structure);
    match structure {
        ExportedFuzzerStructure::Opaque { reason, .. } if is_root => RelationalStructureProfile {
            relation: DomainRel::Unknown {
                reason: reason.clone(),
            },
            precision: DomainPrecision::Unknown,
            diagnostics: vec![format!(
                "Composed fuzzer subtree remains opaque to the relational lowerer: {reason}",
            )],
            source_diagnostics: vec![relational_profile_diagnostic(
                "relational_opaque_root",
                DomainDiagnosticSeverity::Warning,
                format!(
                    "Source-level relational lowering could not inspect this top-level fuzzer expression: {reason}",
                ),
                source_span,
            )],
            widenings: Vec::new(),
        },
        ExportedFuzzerStructure::Opaque { reason, .. } => RelationalStructureProfile {
            relation: DomainRel::SamplerReturns {
                summary: format!(
                    "local sampler fallback for opaque child subtree: {reason}; exact child-specific harness replay is not exported yet",
                ),
            },
            precision: DomainPrecision::Unknown,
            diagnostics: vec![format!(
                "Known parts of the composed fuzzer were lowered, but one child remains opaque and now uses a local sampler fallback placeholder: {reason}",
            )],
            source_diagnostics: vec![relational_profile_diagnostic(
                "relational_local_sampler_fallback",
                DomainDiagnosticSeverity::Warning,
                format!(
                    "Lowered the surrounding composed fuzzer, but this child remains opaque and was delegated to a local sampler fallback placeholder: {reason}",
                ),
                source_span,
            )],
            widenings: vec![Widening {
                kind: "opaque_subterm".to_string(),
                message: "An opaque child subtree was preserved via local sampler fallback; exact child-specific semantics are not yet exported.".to_string(),
                allowed_only_under: None,
            }],
        },
        ExportedFuzzerStructure::Primitive {
            output_type,
            known_constraint,
            ..
        } => primitive_structure_profile(output_type, known_constraint.as_ref()),
        ExportedFuzzerStructure::Map {
            source,
            output_type,
            mapper_shape,
            ..
        } => {
            let source_profile = relational_profile_from_structure_with_context(source, false);
            let precision = if source_profile.precision == DomainPrecision::Unknown {
                DomainPrecision::Unknown
            } else if mapper_shape_is_exact(mapper_shape) {
                source_profile.precision
            } else {
                DomainPrecision::OverApprox
            };
            let mut diagnostics = source_profile.diagnostics.clone();
            let mut source_diagnostics = source_profile.source_diagnostics.clone();
            diagnostics.push("Relational image over one existential intermediate value.".to_string());
            if !mapper_shape_is_exact(mapper_shape) {
                diagnostics.push(
                    "Mapper body is not yet certified for compact exactness; the output relation falls back to a conservative over-approximation around the existential source domain.".to_string(),
                );
                source_diagnostics.push(relational_profile_diagnostic(
                    "relational_map_widened",
                    DomainDiagnosticSeverity::Warning,
                    "This map call keeps the existential source domain, but the mapper body is not yet certified for exact relational lowering.",
                    source_span,
                ));
            }
            RelationalStructureProfile {
                relation: DomainRel::Image {
                    output_type: output_type.clone(),
                    mapper_shape: mapper_shape.clone(),
                    sources: vec![source_profile.relation],
                },
                precision,
                diagnostics,
                source_diagnostics,
                widenings: source_profile.widenings,
            }
        }
        ExportedFuzzerStructure::MapN {
            sources,
            output_type,
            mapper_shape,
            ..
        } => {
            let children = sources
                .iter()
                .map(|source| relational_profile_from_structure_with_context(source, false))
                .collect::<Vec<_>>();
            let mut diagnostics = children
                .iter()
                .flat_map(|child| child.diagnostics.clone())
                .collect::<Vec<_>>();
            let mut source_diagnostics = collect_profile_source_diagnostics(&children);
            diagnostics.push(format!(
                "Relational image over {} existential intermediate value(s).",
                children.len()
            ));
            let mut widenings = children
                .iter()
                .flat_map(|child| child.widenings.clone())
                .collect::<Vec<_>>();
            let precision = if mapper_shape_is_exact(mapper_shape) {
                merge_structure_precision(&children)
            } else if children
                .iter()
                .any(|child| child.precision == DomainPrecision::Unknown)
            {
                diagnostics.push(
                    "mapN mapper body is not yet certified for compact exactness and at least one source domain is itself unknown.".to_string(),
                );
                DomainPrecision::Unknown
            } else {
                diagnostics.push(
                    "mapN mapper body is not yet certified for compact exactness; the existential source relations are preserved as a conservative over-approximation.".to_string(),
                );
                DomainPrecision::OverApprox
            };
            if !mapper_shape_is_exact(mapper_shape) {
                widenings.push(Widening {
                    kind: "mapn_mapper".to_string(),
                    message: "mapN preserves existential intermediate sources, but the mapper body itself is not yet fully certified.".to_string(),
                    allowed_only_under: None,
                });
                source_diagnostics.push(relational_profile_diagnostic(
                    "relational_mapn_widened",
                    DomainDiagnosticSeverity::Warning,
                    "This mapN call preserves existential intermediate sources, but its mapper body is not yet certified for exact relational lowering.",
                    source_span,
                ));
            }
            RelationalStructureProfile {
                relation: DomainRel::Image {
                    output_type: output_type.clone(),
                    mapper_shape: mapper_shape.clone(),
                    sources: children.into_iter().map(|child| child.relation).collect(),
                },
                precision,
                diagnostics,
                source_diagnostics,
                widenings,
            }
        }
        ExportedFuzzerStructure::Bind { source, result, .. } => {
            let source_profile = relational_profile_from_structure_with_context(source, false);
            let result_profile = relational_profile_from_structure_with_context(result, false);
            let mut diagnostics = source_profile.diagnostics.clone();
            diagnostics.extend(result_profile.diagnostics.clone());
            diagnostics.push(
                "Dependent and_then relation preserves the source and result subdomains but does not yet certify the dependency between them.".to_string(),
            );
            let mut source_diagnostics = source_profile.source_diagnostics.clone();
            source_diagnostics.extend(result_profile.source_diagnostics.clone());
            source_diagnostics.push(relational_profile_diagnostic(
                "relational_bind_dependency",
                DomainDiagnosticSeverity::Warning,
                "This and_then lowering preserves the known source/result structure, but the dependency between them remains conservative.",
                source_span,
            ));
            let mut widenings = source_profile.widenings.clone();
            widenings.extend(result_profile.widenings.clone());
            widenings.push(Widening {
                kind: "bind_dependency".to_string(),
                message: "and_then is modeled as a dependent relation over source and result subdomains, but the dependency itself remains conservative.".to_string(),
                allowed_only_under: None,
            });
            RelationalStructureProfile {
                relation: DomainRel::Bind {
                    source: Box::new(source_profile.relation),
                    result: Box::new(result_profile.relation),
                },
                precision: DomainPrecision::OverApprox,
                diagnostics,
                source_diagnostics,
                widenings,
            }
        }
        ExportedFuzzerStructure::Product { elements, .. } => {
            let children = elements
                .iter()
                .map(|element| relational_profile_from_structure_with_context(element, false))
                .collect::<Vec<_>>();
            let precision = merge_structure_precision(&children);
            let mut diagnostics = children
                .iter()
                .flat_map(|child| child.diagnostics.clone())
                .collect::<Vec<_>>();
            let source_diagnostics = collect_profile_source_diagnostics(&children);
            diagnostics.push("Cartesian product of child domains.".to_string());
            let widenings = children
                .iter()
                .flat_map(|child| child.widenings.clone())
                .collect::<Vec<_>>();
            RelationalStructureProfile {
                relation: DomainRel::Product {
                    elements: children.into_iter().map(|child| child.relation).collect(),
                },
                precision,
                diagnostics,
                source_diagnostics,
                widenings,
            }
        }
        ExportedFuzzerStructure::List {
            element,
            min_len,
            max_len,
            unique,
            retry_limit,
            ..
        } => {
            let element_profile = relational_profile_from_structure_with_context(element, false);
            let mut diagnostics = element_profile.diagnostics.clone();
            let mut source_diagnostics = element_profile.source_diagnostics.clone();
            let mut widenings = element_profile.widenings.clone();
            let precision = if *unique {
                let insufficient_support = min_len
                    .and_then(|min| structure_support_upper_bound(element).map(|support| (min, support)))
                    .is_some_and(|(min, support)| support < min);
                if insufficient_support {
                    diagnostics.push(
                        "Unique-list/set generation can fail because the element support is smaller than the requested minimum cardinality.".to_string(),
                    );
                    source_diagnostics.push(relational_profile_diagnostic(
                        "relational_set_insufficient_support",
                        DomainDiagnosticSeverity::Error,
                        "This unique-list/set combinator can fail because the requested minimum cardinality exceeds the modeled element support.",
                        source_span,
                    ));
                    DomainPrecision::Unknown
                } else {
                    diagnostics.push(
                        "Unique-list/set lowering preserves element and length relations, but retry/failure semantics remain conservative.".to_string(),
                    );
                    widenings.push(Widening {
                        kind: "set_retry".to_string(),
                        message: "Unique-list/set support is modeled conservatively because retry exhaustion and duplicate rejection are not fully captured.".to_string(),
                        allowed_only_under: None,
                    });
                    source_diagnostics.push(relational_profile_diagnostic(
                        "relational_set_retry",
                        DomainDiagnosticSeverity::Warning,
                        "This unique-list/set combinator keeps the known element relation, but retry exhaustion and duplicate rejection remain conservative.",
                        source_span,
                    ));
                    DomainPrecision::OverApprox
                }
            } else if element_profile.precision == DomainPrecision::Exact {
                diagnostics.push("List length and element relations are modeled exactly.".to_string());
                DomainPrecision::Exact
            } else {
                diagnostics.push(
                    "List bounds are preserved, but at least one child domain is conservative.".to_string(),
                );
                element_profile.precision
            };
            RelationalStructureProfile {
                relation: DomainRel::List {
                    element: Box::new(element_profile.relation),
                    min_len: *min_len,
                    max_len: *max_len,
                    unique: *unique,
                    retry_limit: *retry_limit,
                },
                precision,
                diagnostics,
                source_diagnostics,
                widenings,
            }
        }
        ExportedFuzzerStructure::Choice {
            output_type,
            branches,
            may_fail,
            non_empty_required,
            ..
        } => {
            let children = branches
                .iter()
                .map(|branch| relational_profile_from_structure_with_context(branch, false))
                .collect::<Vec<_>>();
            let mut diagnostics = children
                .iter()
                .flat_map(|child| child.diagnostics.clone())
                .collect::<Vec<_>>();
            let mut source_diagnostics = collect_profile_source_diagnostics(&children);
            let widenings = children
                .iter()
                .flat_map(|child| child.widenings.clone())
                .collect::<Vec<_>>();
            let precision = if children.is_empty() && *non_empty_required {
                diagnostics.push(
                    "The choice combinator has no branches, so the fuzzer always fails instead of generating a value.".to_string(),
                );
                source_diagnostics.push(relational_profile_diagnostic(
                    "relational_choice_empty",
                    DomainDiagnosticSeverity::Error,
                    "This one_of/choice combinator has no branches, so it always fails instead of generating a value.",
                    source_span,
                ));
                DomainPrecision::Unknown
            } else {
                merge_choice_precision(&children, *may_fail)
            };
            RelationalStructureProfile {
                relation: DomainRel::Choice {
                    output_type: output_type.clone(),
                    branches: children.into_iter().map(|child| child.relation).collect(),
                    may_fail: *may_fail,
                    non_empty_required: *non_empty_required,
                },
                precision,
                diagnostics,
                source_diagnostics,
                widenings,
            }
        }
        ExportedFuzzerStructure::Filter {
            output_type,
            source,
            predicate_summary,
            max_tries,
            impossible,
            ..
        } => {
            let source_profile = relational_profile_from_structure_with_context(source, false);
            let mut diagnostics = source_profile.diagnostics.clone();
            let mut source_diagnostics = source_profile.source_diagnostics.clone();
            let mut widenings = source_profile.widenings.clone();
            let precision = if *impossible {
                diagnostics.push(
                    "such_that predicate is impossible for the known generated support, so the fuzzer can fail without producing any value.".to_string(),
                );
                source_diagnostics.push(relational_profile_diagnostic(
                    "relational_such_that_impossible",
                    DomainDiagnosticSeverity::Error,
                    "This such_that predicate is impossible for the currently modeled support, so the fuzzer can fail without producing any value.",
                    source_span,
                ));
                DomainPrecision::Unknown
            } else {
                widenings.push(Widening {
                    kind: "such_that".to_string(),
                    message: "such_that is modeled as the base domain plus a predicate summary; retry/failure semantics are still conservative.".to_string(),
                    allowed_only_under: None,
                });
                source_diagnostics.push(relational_profile_diagnostic(
                    "relational_such_that_widened",
                    DomainDiagnosticSeverity::Warning,
                    format!(
                        "This such_that lowering preserves the base domain but records only a first-order predicate summary (`{predicate_summary}`); retry/failure semantics remain conservative.",
                    ),
                    source_span,
                ));
                DomainPrecision::OverApprox
            };
            RelationalStructureProfile {
                relation: DomainRel::Filter {
                    output_type: output_type.clone(),
                    source: Box::new(source_profile.relation),
                    predicate_summary: predicate_summary.clone(),
                    max_tries: *max_tries,
                    impossible: *impossible,
                },
                precision,
                diagnostics,
                source_diagnostics,
                widenings,
            }
        }
        ExportedFuzzerStructure::StateMachineTrace { acceptance, .. } => RelationalStructureProfile {
            relation: DomainRel::Semantics {
                semantics: FuzzerSemantics::StateMachineTrace {
                    acceptance: acceptance.clone(),
                    state_type: FuzzerOutputType::Data,
                    step_input_types: Vec::new(),
                    label_type: FuzzerOutputType::Data,
                    event_type: FuzzerOutputType::Data,
                    transition_semantics: StateMachineTransitionSemantics {
                        terminal_tag: 0,
                        step_tag: 0,
                        label_field_index: 0,
                        next_state_field_index: 0,
                        event_field_index: 0,
                        state_semantics: Box::new(FuzzerSemantics::Data),
                        step_input_semantics: Vec::new(),
                        label_semantics: Box::new(FuzzerSemantics::Data),
                        event_semantics: Box::new(FuzzerSemantics::Data),
                    },
                    output_semantics: Box::new(FuzzerSemantics::Data),
                    step_function_ir: None,
                    step_ir_unsupported_reason: Some(
                        "state-machine trace relational lowering still uses the existing dedicated path".to_string(),
                    ),
                    initial_state_shallow_ir: None,
                },
            },
            precision: DomainPrecision::Unknown,
            diagnostics: vec![
                "State-machine traces continue to use the dedicated transition relation path; composed-fuzzer lowering does not override them.".to_string(),
            ],
            source_diagnostics: Vec::new(),
            widenings: Vec::new(),
        },
    }
}

fn structured_profile_requires_block(profile: &RelationalStructureProfile) -> bool {
    if relation_contains_sampler_returns(&profile.relation) {
        return true;
    }

    match &profile.relation {
        DomainRel::Choice {
            branches,
            non_empty_required,
            ..
        } => *non_empty_required && branches.is_empty(),
        DomainRel::Filter { impossible, .. } => *impossible,
        DomainRel::List { unique, .. } => *unique && profile.precision == DomainPrecision::Unknown,
        _ => false,
    }
}

fn replay_backed_existential_witness(
    test: &ExportedPropertyTest,
) -> Option<&ExportedReplayWitness> {
    test.fail_once_replay_witness.as_ref()
}

fn replay_witness_sampler_metadata(witness: &ExportedReplayWitness) -> SamplerMetadata {
    sampler_metadata(
        SamplerRunKind::WitnessReplay,
        SamplerPrngMode::Seeded,
        Some(witness.generation_seed),
        witness.replay_choices_hex.clone(),
    )
}

fn exact_semantic_existential_support_available(
    test: &ExportedPropertyTest,
    target: &VerificationTargetKind,
    existential_mode: ExistentialMode,
) -> bool {
    if sampler_relation_supported(test, target, existential_mode) {
        return true;
    }
    if semantics_is_opaque(&test.semantics) {
        return false;
    }

    let compact_profile = compact_domain_profile(&test.fuzzer_output_type, &test.semantics);
    let structured_profile = test
        .fuzzer_structure
        .as_ref()
        .map(relational_profile_from_structure);
    let constraint_gap_active = constraint_is_unsupported(&test.constraint);

    if let Some(profile) = &structured_profile {
        let precision = if profile.precision == DomainPrecision::Unknown
            && !structured_profile_requires_block(profile)
            && let Some(compact) = &compact_profile
        {
            if constraint_gap_active && compact.precision == DomainPrecision::Exact {
                DomainPrecision::OverApprox
            } else {
                compact.precision
            }
        } else {
            profile.precision
        };
        return precision == DomainPrecision::Exact;
    }

    if let Some(profile) = &compact_profile {
        let precision = if constraint_gap_active && profile.precision == DomainPrecision::Exact {
            DomainPrecision::OverApprox
        } else {
            profile.precision
        };
        return precision == DomainPrecision::Exact
            && matches!(profile.coverage, CompactDomainCoverage::ExactSupport);
    }

    false
}

fn should_use_replay_backed_existential_domain(
    test: &ExportedPropertyTest,
    target: &VerificationTargetKind,
    existential_mode: ExistentialMode,
) -> bool {
    use aiken_lang::ast::OnTestFailure;

    matches!(test.on_test_failure, OnTestFailure::SucceedImmediately)
        && matches!(existential_mode, ExistentialMode::Proof)
        && !matches!(target, VerificationTargetKind::Equivalence)
        && replay_backed_existential_witness(test).is_some()
        && !exact_semantic_existential_support_available(test, target, existential_mode)
}

fn witness_values_for_mode(
    test: &ExportedPropertyTest,
    proof_caveat: Option<&ProofCaveat>,
) -> Vec<String> {
    if let Some(ProofCaveat::Witness(note)) = proof_caveat {
        return note.witnesses.clone();
    }

    if !test.concrete_error_witnesses.is_empty() {
        return test.concrete_error_witnesses.clone();
    }

    test.concrete_halt_witnesses.clone()
}

fn sampler_metadata_for_domain(
    test: &ExportedPropertyTest,
    mode: CompatibilityMode,
    proof_caveat: Option<&ProofCaveat>,
    target: &VerificationTargetKind,
    existential_mode: ExistentialMode,
) -> Option<SamplerMetadata> {
    if matches!(mode, CompatibilityMode::WitnessCheck)
        && !witness_values_for_mode(test, proof_caveat).is_empty()
    {
        return Some(default_witness_sampler_metadata());
    }

    if should_use_replay_backed_existential_domain(test, target, existential_mode) {
        return replay_backed_existential_witness(test).map(replay_witness_sampler_metadata);
    }

    sampler_relation_supported(test, target, existential_mode)
        .then(default_sampler_relation_metadata)
}

fn relation_from_current_bridge(
    test: &ExportedPropertyTest,
    mode: CompatibilityMode,
    proof_caveat: Option<&ProofCaveat>,
    placeholder_widenings: &[Widening],
    target: &VerificationTargetKind,
    existential_mode: ExistentialMode,
) -> DomainRel {
    if matches!(mode, CompatibilityMode::WitnessCheck) {
        return DomainRel::Witness {
            encoded_values: witness_values_for_mode(test, proof_caveat),
        };
    }

    if let Some(widening) = placeholder_widenings.first() {
        return DomainRel::TrueWithExplicitWidening {
            reason: widening.message.clone(),
            allowed_only_under: TrustProfile::UnsafeDev,
        };
    }

    if sampler_relation_supported(test, target, existential_mode) {
        let sampler = default_sampler_relation_metadata();
        return DomainRel::SamplerReturns {
            summary: sampler_relation_summary(&sampler),
        };
    }

    if semantics_is_opaque(&test.semantics) {
        return DomainRel::Unknown {
            reason: "Current bridge cannot yet certify opaque/custom fuzzer semantics.".to_string(),
        };
    }

    if constraint_is_unsupported(&test.constraint) {
        return DomainRel::Semantics {
            semantics: test.semantics.clone(),
        };
    }

    let mut items = Vec::new();
    if !matches!(test.constraint, FuzzerConstraint::Any) {
        items.push(DomainRel::Constraint {
            constraint: test.constraint.clone(),
        });
    }
    items.push(DomainRel::Semantics {
        semantics: test.semantics.clone(),
    });

    if items.len() == 1 {
        items.pop().expect("domain relation must contain semantics")
    } else {
        DomainRel::And { items }
    }
}

fn lowered_domain_for_test(
    test: &ExportedPropertyTest,
    mode: CompatibilityMode,
    proof_content: Option<&str>,
    proof_caveat: Option<&ProofCaveat>,
    target: &VerificationTargetKind,
    existential_mode: ExistentialMode,
) -> LoweredDomain {
    let mut lowering_path = Vec::new();
    if !matches!(test.constraint, FuzzerConstraint::Any) {
        push_lowering_step(&mut lowering_path, "constraint_ir");
    }
    match &test.semantics {
        FuzzerSemantics::Opaque { .. } => {
            push_lowering_step(&mut lowering_path, "opaque_semantics")
        }
        FuzzerSemantics::StateMachineTrace { .. } => {
            push_lowering_step(&mut lowering_path, "state_machine_trace");
        }
        _ => push_lowering_step(&mut lowering_path, "known_combinator"),
    }
    if test.fuzzer_data_schema.is_some() || !test.inner_data_schemas.is_empty() {
        push_lowering_step(&mut lowering_path, "data_schema");
    }
    if test.transition_prop_lean.is_some() {
        push_lowering_step(&mut lowering_path, "transition_relation");
    }
    if matches!(mode, CompatibilityMode::WitnessCheck) {
        push_lowering_step(&mut lowering_path, "witness_replay");
    }
    let sampler = sampler_metadata_for_domain(test, mode, proof_caveat, target, existential_mode);
    let sampler_relation_active = sampler.as_ref().is_some_and(|metadata| {
        matches!(
            metadata.run_kind,
            SamplerRunKind::SeededGeneration | SamplerRunKind::ReplayOnly
        )
    }) && matches!(test.semantics, FuzzerSemantics::Opaque { .. });
    if sampler_relation_active {
        push_lowering_step(&mut lowering_path, "sampler_relation");
    }
    let input_bridge = input_value_bridge_for_test(test);
    if input_bridge.input_type.schema_hash.is_some() || !input_bridge.diagnostics.is_empty() {
        push_lowering_step(&mut lowering_path, "value_bridge");
    }
    let compact_profile = compact_domain_profile(&test.fuzzer_output_type, &test.semantics);
    if compact_profile.is_some() {
        push_lowering_step(&mut lowering_path, "compact_domain");
    }
    let structured_profile = test
        .fuzzer_structure
        .as_ref()
        .map(relational_profile_from_structure);
    if structured_profile.is_some() {
        push_lowering_step(&mut lowering_path, "relational_domain");
    }
    let replay_existential_witness = if matches!(mode, CompatibilityMode::SemanticExistential)
        && should_use_replay_backed_existential_domain(test, target, existential_mode)
    {
        replay_backed_existential_witness(test)
    } else {
        None
    };
    if replay_existential_witness.is_some() {
        push_lowering_step(&mut lowering_path, "replay_witness_domain");
    }
    let scenario_trace = scenario_trace_ir_for_test(test, mode, proof_caveat);
    if scenario_trace.is_some() {
        push_lowering_step(&mut lowering_path, "scenario_trace_ir");
    }

    let mut widenings = transition_prop_domain_widenings(test);
    let placeholder_widenings = proof_content
        .map(placeholder_domain_widenings)
        .unwrap_or_default();
    if !placeholder_widenings.is_empty() {
        push_lowering_step(&mut lowering_path, "placeholder_scan");
    }
    let constraint_gap_active =
        constraint_is_unsupported(&test.constraint) && !sampler_relation_active;
    if constraint_gap_active {
        push_lowering_step(&mut lowering_path, "constraint_gap");
        widenings.push(Widening {
            kind: "constraint".to_string(),
            message: "Current bridge dropped unsupported fuzzer constraint lowering and used a semantics-only over-approximation.".to_string(),
            allowed_only_under: None,
        });
    }
    if let Some(profile) = &compact_profile {
        widenings.extend(profile.widenings.iter().cloned());
    }
    if let Some(profile) = &structured_profile {
        widenings.extend(profile.widenings.iter().cloned());
    }
    widenings.extend(placeholder_widenings.iter().cloned());
    widenings.extend(input_bridge.widenings.iter().cloned());

    let mut diagnostics = Vec::new();
    if let Some(ProofCaveat::Partial(note)) = proof_caveat {
        diagnostics.push(note.clone());
    }
    if let Some(witness) = replay_existential_witness {
        diagnostics.push(format!(
            "Replay-backed fail_once domain uses the concrete witness from generation_seed={} (replay_choices={}) as an under-approximate semantic existential domain. It proves only that the intended fuzzer/property path has a failing input for this recorded replay; it does not claim the randomized runner will find one within any bounded number of attempts.",
            witness.generation_seed,
            witness
                .replay_choices_hex
                .as_deref()
                .unwrap_or("none"),
        ));
    }
    if semantics_is_opaque(&test.semantics)
        && !matches!(mode, CompatibilityMode::WitnessCheck)
        && !sampler_relation_active
    {
        diagnostics
            .push("Current bridge cannot yet certify opaque/custom fuzzer semantics.".to_string());
    }
    if sampler_relation_active {
        diagnostics.push(
            "Opaque/custom fuzzer semantics routed through the exact sampler relation over the exported harness ABI.".to_string(),
        );
    }
    if let Some(profile) = &compact_profile {
        diagnostics.extend(profile.diagnostics.iter().cloned());
    }
    if let Some(profile) = &structured_profile {
        diagnostics.extend(profile.diagnostics.iter().cloned());
    }
    if constraint_gap_active {
        diagnostics.push(
            "Current bridge dropped unsupported fuzzer constraint lowering and used a semantics-only over-approximation.".to_string(),
        );
    }
    if !placeholder_widenings.is_empty() {
        diagnostics.push(
            "Generated domain contains explicit placeholder widenings and is not production-safe."
                .to_string(),
        );
    }
    diagnostics.extend(input_bridge.diagnostics.iter().cloned());
    if matches!(mode, CompatibilityMode::SemanticExistential)
        && replay_existential_witness.is_none()
        && !sampler_relation_active
    {
        diagnostics.push(
            "Current bridge does not yet prove DomainImpliesFuzzerReturns for fail_once theorems; production success is downgraded to Partial.".to_string(),
        );
    }
    if let Some(trace) = &scenario_trace {
        diagnostics.push(format!(
            "Scenario trace IR: {:?}, trace length {}, step depth {}.",
            trace.symbolic_encoding,
            trace.trace_length.render(),
            trace.step_depth.render()
        ));
        if let Some(blocker) = trace.blockers.first() {
            diagnostics.push(blocker.clone());
        }
    }

    let relation = if let Some(witness) = replay_existential_witness {
        DomainRel::Witness {
            encoded_values: vec![witness.data_hex.clone()],
        }
    } else if matches!(mode, CompatibilityMode::WitnessCheck)
        || !placeholder_widenings.is_empty()
        || sampler_relation_active
    {
        relation_from_current_bridge(
            test,
            mode,
            proof_caveat,
            &placeholder_widenings,
            target,
            existential_mode,
        )
    } else if let Some(profile) = &structured_profile {
        profile.relation.clone()
    } else {
        relation_from_current_bridge(
            test,
            mode,
            proof_caveat,
            &placeholder_widenings,
            target,
            existential_mode,
        )
    };
    let mut obligations_discharged = vec![
        DomainObligation::FuzzerModelHashMatches,
        DomainObligation::PropertyHarnessMatchesExportedUPLC,
    ];

    let (precision, certificate, mut obligations_open) =
        if matches!(mode, CompatibilityMode::WitnessCheck) {
            obligations_discharged.push(DomainObligation::WitnessReplaysThroughFuzzer);
            obligations_discharged.push(DomainObligation::WitnessSatisfiesDomain);
            (
                DomainPrecision::WitnessOnly,
                DomainCertificate::WitnessReplay,
                Vec::new(),
            )
        } else if replay_existential_witness.is_some() {
            obligations_discharged.push(DomainObligation::DomainImpliesFuzzerReturns);
            obligations_discharged.push(DomainObligation::WitnessReplaysThroughFuzzer);
            obligations_discharged.push(DomainObligation::WitnessSatisfiesDomain);
            (
                DomainPrecision::UnderApprox,
                DomainCertificate::WitnessReplay,
                Vec::new(),
            )
        } else if sampler_relation_active {
            obligations_discharged.push(DomainObligation::FuzzerReturnsImpliesDomain);
            obligations_discharged.push(DomainObligation::DomainIffFuzzerReturns);
            (
                DomainPrecision::Exact,
                DomainCertificate::SamplerSemanticModel,
                Vec::new(),
            )
        } else if !placeholder_widenings.is_empty() || semantics_is_opaque(&test.semantics) {
            (
                DomainPrecision::Unknown,
                DomainCertificate::Unchecked,
                vec![match mode {
                    CompatibilityMode::UniversalSuccess | CompatibilityMode::UniversalFailure => {
                        DomainObligation::FuzzerReturnsImpliesDomain
                    }
                    CompatibilityMode::SemanticExistential => {
                        DomainObligation::DomainImpliesFuzzerReturns
                    }
                    CompatibilityMode::WitnessCheck => {
                        DomainObligation::WitnessReplaysThroughFuzzer
                    }
                }],
            )
        } else if let Some(profile) = &structured_profile {
            let precision = if profile.precision == DomainPrecision::Unknown
                && !structured_profile_requires_block(profile)
                && let Some(compact) = &compact_profile
            {
                if constraint_gap_active && compact.precision == DomainPrecision::Exact {
                    DomainPrecision::OverApprox
                } else {
                    compact.precision
                }
            } else {
                profile.precision
            };
            match mode {
                CompatibilityMode::SemanticExistential if precision == DomainPrecision::Exact => {
                    obligations_discharged.push(DomainObligation::DomainIffFuzzerReturns);
                    (
                        precision,
                        DomainCertificate::TrustedVersionedModel,
                        Vec::new(),
                    )
                }
                CompatibilityMode::SemanticExistential => (
                    precision,
                    DomainCertificate::TrustedVersionedModel,
                    vec![DomainObligation::DomainImpliesFuzzerReturns],
                ),
                CompatibilityMode::UniversalSuccess | CompatibilityMode::UniversalFailure => {
                    obligations_discharged.push(DomainObligation::FuzzerReturnsImpliesDomain);
                    (
                        precision,
                        DomainCertificate::TrustedVersionedModel,
                        Vec::new(),
                    )
                }
                CompatibilityMode::WitnessCheck => unreachable!("witness mode handled above"),
            }
        } else if let Some(profile) = &compact_profile {
            let precision = if constraint_gap_active && profile.precision == DomainPrecision::Exact
            {
                DomainPrecision::OverApprox
            } else {
                profile.precision
            };
            match mode {
                CompatibilityMode::SemanticExistential
                    if !constraint_gap_active
                        && matches!(profile.coverage, CompactDomainCoverage::ExactSupport) =>
                {
                    obligations_discharged.push(DomainObligation::DomainIffFuzzerReturns);
                    (
                        precision,
                        DomainCertificate::TrustedVersionedModel,
                        Vec::new(),
                    )
                }
                CompatibilityMode::SemanticExistential => (
                    precision,
                    DomainCertificate::TrustedVersionedModel,
                    vec![DomainObligation::DomainImpliesFuzzerReturns],
                ),
                CompatibilityMode::UniversalSuccess | CompatibilityMode::UniversalFailure => {
                    obligations_discharged.push(DomainObligation::FuzzerReturnsImpliesDomain);
                    (
                        precision,
                        DomainCertificate::TrustedVersionedModel,
                        Vec::new(),
                    )
                }
                CompatibilityMode::WitnessCheck => unreachable!("witness mode handled above"),
            }
        } else if matches!(mode, CompatibilityMode::SemanticExistential) {
            (
                DomainPrecision::Unknown,
                DomainCertificate::TrustedVersionedModel,
                vec![DomainObligation::DomainImpliesFuzzerReturns],
            )
        } else {
            obligations_discharged.push(DomainObligation::FuzzerReturnsImpliesDomain);
            (
                DomainPrecision::OverApprox,
                DomainCertificate::TrustedVersionedModel,
                Vec::new(),
            )
        };

    for obligation in &input_bridge.obligations_discharged {
        if !obligations_discharged.contains(obligation) {
            obligations_discharged.push(*obligation);
        }
    }
    for obligation in &input_bridge.obligations_open {
        if !obligations_open.contains(obligation) {
            obligations_open.push(*obligation);
        }
    }

    let mut domain = LoweredDomain {
        relation,
        precision,
        certificate,
        obligations_open,
        obligations_discharged,
        lowering_path,
        widenings,
        production_allowed: false,
        sampler,
        scenario_trace: scenario_trace.clone(),
        compatibility_diagnostics: Vec::new(),
        diagnostics,
    };

    let production_assessment = assess_domain_compatibility(
        mode,
        Some(test.return_mode.clone()),
        TrustProfile::Production,
        &domain,
        match mode {
            CompatibilityMode::WitnessCheck => SuccessChannel::WitnessReplay,
            CompatibilityMode::UniversalSuccess
            | CompatibilityMode::UniversalFailure
            | CompatibilityMode::SemanticExistential => SuccessChannel::SolverTheorem,
        },
    );
    domain.production_allowed = production_assessment.block_reason.is_none();
    domain.compatibility_diagnostics = production_assessment.diagnostics;
    if let Some(profile) = &structured_profile {
        domain
            .compatibility_diagnostics
            .extend(lift_relational_profile_diagnostics(
                &test.input_path,
                test.fuzzer_source_span,
                &profile.source_diagnostics,
            ));
    }
    domain
}
/// Sanitize an Aiken identifier to a valid Lean identifier.
///
/// - Replaces `-` with `_`
/// - Ensures the name starts with a letter (prefixes with `T_` if it starts with a digit)
fn sanitize_lean_name(aiken_name: &str) -> String {
    let s = aiken_name.replace('-', "_");
    let s = if s.starts_with(|c: char| c.is_ascii_digit()) {
        format!("T_{s}")
    } else {
        s
    };
    // Check for Lean reserved words
    const LEAN_KEYWORDS: &[&str] = &[
        "abbrev",
        "attribute",
        "axiom",
        "by",
        "class",
        "def",
        "deriving",
        "do",
        "else",
        "end",
        "for",
        "fun",
        "have",
        "if",
        "import",
        "in",
        "inductive",
        "instance",
        "lemma",
        "let",
        "match",
        "mutual",
        "namespace",
        "noncomputable",
        "opaque",
        "open",
        "partial",
        "private",
        "protected",
        "return",
        "section",
        "set_option",
        "structure",
        "syntax",
        "theorem",
        "type",
        "variable",
        "where",
        "with",
    ];
    if LEAN_KEYWORDS.contains(&s.as_str()) {
        format!("l_{s}")
    } else {
        s
    }
}

/// Convert an Aiken module path like `aiken/list` or `permissions.test`
/// to a PascalCase Lean module segment like `AikenList` or `PermissionsTest`.
fn module_to_lean_segment(module: &str) -> String {
    module
        .split(['/', '.'])
        .filter(|part| !part.is_empty())
        .map(|part| {
            let sanitized = sanitize_lean_name(part);
            let mut chars = sanitized.chars();
            match chars.next() {
                None => String::new(),
                Some(first) => {
                    let upper = first.to_uppercase().to_string();
                    format!("{upper}{rest}", rest = chars.as_str())
                }
            }
        })
        .collect::<String>()
}

/// Build a unique test id from module and name, used for filenames and manifest keys.
/// Appends a short hash of the original (unsanitized) module.name to avoid collisions
/// when sanitization collapses distinct names to the same string.
fn test_id(module: &str, name: &str) -> String {
    let module_part = sanitize_lean_name(&module.replace(['/', '.'], "_"));
    let name_part = sanitize_lean_name(name);
    let hash = short_hash(&format!("{module}.{name}"));
    format!("{module_part}__{name_part}_{hash}")
}

/// Produce an 8-hex-char hash of the input string for use as a disambiguation suffix.
fn short_hash(input: &str) -> String {
    // Stable 32-bit FNV-1a so IDs remain reproducible across toolchain changes.
    const FNV_OFFSET_BASIS: u32 = 0x811c9dc5;
    const FNV_PRIME: u32 = 0x01000193;

    let mut hash = FNV_OFFSET_BASIS;
    for byte in input.as_bytes() {
        hash ^= u32::from(*byte);
        hash = hash.wrapping_mul(FNV_PRIME);
    }

    format!("{hash:08x}")
}

/// Detect collisions in generated Lean file paths before writing any files.
/// The collision surface is `(lean_module_segment, lean_test_name)` since those
/// determine the `.lean` file path, which does not include the hash suffix.
#[cfg(test)]
fn detect_lean_path_collisions(tests: &[ExportedPropertyTest]) -> miette::Result<()> {
    // Map from lean file path -> original "module.test_name"
    let mut seen: HashMap<String, String> = HashMap::new();
    let mut collisions: Vec<String> = Vec::new();

    for test in tests {
        let module = &test.module;
        let full_name = &test.name;
        let test_name = full_name
            .strip_prefix(&format!("{module}."))
            .unwrap_or(full_name);
        let lean_module_segment = module_to_lean_segment(module);
        let lean_test_name = sanitize_lean_name(test_name);
        let lean_path = format!("AikenVerify/Proofs/{lean_module_segment}/{lean_test_name}.lean");
        let original = format!("{module}.{test_name}");

        if let Some(existing) = seen.get(&lean_path) {
            collisions.push(format!(
                "  \"{lean_path}\" generated by both \"{existing}\" and \"{original}\""
            ));
        } else {
            seen.insert(lean_path, original);
        }
    }

    if !collisions.is_empty() {
        return Err(miette::miette!(
            "Lean file path collisions detected — generated files would overwrite each other:\n{}",
            collisions.join("\n")
        ));
    }
    Ok(())
}

/// Result of checking a single tool's availability and version.
#[derive(Debug, Clone, serde::Serialize)]
#[non_exhaustive]
pub struct ToolCheck {
    pub tool: String,
    pub found: bool,
    pub version: Option<String>,
    pub meets_minimum: bool,
    pub minimum_version: String,
    pub error: Option<String>,
}

/// Result of the full toolchain doctor check.
#[derive(Debug, Clone, serde::Serialize)]
#[non_exhaustive]
pub struct DoctorReport {
    /// Schema version for this doctor JSON document.
    pub version: String,
    pub tools: Vec<ToolCheck>,
    pub plutus_core: PlutusCoreCheck,
    pub blaster_rev: String,
    pub plutus_core_rev: String,
    pub all_ok: bool,
    /// Documented verification capabilities (what is/isn't supported).
    pub capabilities: VerificationCapabilities,
}

/// Result of checking the PlutusCore Lean library.
#[derive(Debug, Clone, serde::Serialize)]
#[non_exhaustive]
pub struct PlutusCoreCheck {
    pub found: bool,
    pub path: String,
    pub has_lakefile: bool,
    pub error: Option<String>,
}

impl ToolCheck {
    pub fn new(
        tool: String,
        found: bool,
        version: Option<String>,
        meets_minimum: bool,
        minimum_version: String,
        error: Option<String>,
    ) -> Self {
        Self {
            tool,
            found,
            version,
            meets_minimum,
            minimum_version,
            error,
        }
    }
}

impl DoctorReport {
    pub fn new(
        version: String,
        tools: Vec<ToolCheck>,
        plutus_core: PlutusCoreCheck,
        blaster_rev: String,
        plutus_core_rev: String,
        all_ok: bool,
        capabilities: VerificationCapabilities,
    ) -> Self {
        Self {
            version,
            tools,
            plutus_core,
            blaster_rev,
            plutus_core_rev,
            all_ok,
            capabilities,
        }
    }
}

impl PlutusCoreCheck {
    pub fn new(found: bool, path: String, has_lakefile: bool, error: Option<String>) -> Self {
        Self {
            found,
            path,
            has_lakefile,
            error,
        }
    }
}

/// Check that required tools (lean, lake, z3) are available in PATH with
/// minimum version requirements. Returns Ok(()) if all tools pass, or an
/// error listing problems.
pub fn check_toolchain() -> miette::Result<()> {
    let mut problems = Vec::new();

    let lean_check = check_tool_version("lean", MIN_LEAN_VERSION);
    if !lean_check.found {
        problems.push("lean: not found in PATH".to_string());
    } else if !lean_check.meets_minimum {
        problems.push(format!(
            "lean: version {} does not meet minimum {}.{}.{}",
            lean_check.version.as_deref().unwrap_or("unknown"),
            MIN_LEAN_VERSION.0,
            MIN_LEAN_VERSION.1,
            MIN_LEAN_VERSION.2,
        ));
    }

    // lake is bundled with lean; check presence but no separate version minimum
    let lake_check = check_tool_version("lake", (0, 0, 0));
    if !lake_check.found {
        problems.push("lake: not found in PATH".to_string());
    }

    let z3_check = check_tool_version("z3", MIN_Z3_VERSION);
    if !z3_check.found {
        problems.push("z3: not found in PATH".to_string());
    } else if !z3_check.meets_minimum {
        problems.push(format!(
            "z3: version {} does not meet minimum {}.{}.{}",
            z3_check.version.as_deref().unwrap_or("unknown"),
            MIN_Z3_VERSION.0,
            MIN_Z3_VERSION.1,
            MIN_Z3_VERSION.2,
        ));
    }

    if problems.is_empty() {
        Ok(())
    } else {
        Err(miette::miette!(
            "Toolchain check failed:\n  - {}\n\n\
             Install Lean 4 (via elan: https://github.com/leanprover/elan) \
             and Z3 (https://github.com/Z3Prover/z3) to run proofs. \
             Use --generate-only to skip proof execution.",
            problems.join("\n  - ")
        ))
    }
}

/// Check a single tool: run `<tool> --version`, parse the version, and compare
/// against the minimum. Returns a structured result.
pub fn check_tool_version(name: &str, min: (u32, u32, u32)) -> ToolCheck {
    let minimum_version = format!("{}.{}.{}", min.0, min.1, min.2);
    let has_no_version_floor = min == (0, 0, 0);

    let output = Command::new(name)
        .arg("--version")
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output();

    let output = match output {
        Ok(o) => o,
        Err(e) => {
            return ToolCheck {
                tool: name.to_string(),
                found: false,
                version: None,
                meets_minimum: false,
                minimum_version,
                error: Some(format!("Failed to run `{name} --version`: {e}")),
            };
        }
    };

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    let combined = format!("{stdout}\n{stderr}");
    let version_str = parse_version_from_output(&combined);

    if !output.status.success() {
        return ToolCheck {
            tool: name.to_string(),
            found: false,
            version: version_str,
            meets_minimum: false,
            minimum_version,
            error: Some(format!(
                "`{name} --version` exited with code {}",
                output.status.code().unwrap_or(-1)
            )),
        };
    }

    let meets_minimum = if has_no_version_floor {
        true
    } else {
        version_str
            .as_ref()
            .map(|v| version_meets_minimum(v, min))
            .unwrap_or(false) // unparseable version output cannot satisfy a real minimum
    };

    ToolCheck {
        tool: name.to_string(),
        found: true,
        version: version_str,
        meets_minimum,
        minimum_version,
        error: None,
    }
}

/// Extract a semver-like version (major.minor.patch) from command output.
fn parse_version_from_output(output: &str) -> Option<String> {
    for line in output.lines() {
        if let Some(version) = extract_first_version(line) {
            return Some(version);
        }
    }
    None
}

/// Extract the first semver-like version string from a line.
fn extract_first_version(line: &str) -> Option<String> {
    let chars: Vec<char> = line.chars().collect();
    let len = chars.len();
    let mut i = 0;
    while i < len {
        if chars[i].is_ascii_digit() {
            // Try to parse major.minor.patch starting at i
            if let Some((version, _end)) = try_parse_version_at(&chars, i) {
                return Some(version);
            }
        }
        i += 1;
    }
    None
}

/// Try to parse a "major.minor.patch" version starting at position `start`.
fn try_parse_version_at(chars: &[char], start: usize) -> Option<(String, usize)> {
    let mut pos = start;
    let mut parts = Vec::new();

    for _ in 0..3 {
        let num_start = pos;
        while pos < chars.len() && chars[pos].is_ascii_digit() {
            pos += 1;
        }
        if pos == num_start {
            return None;
        }
        let num_str: String = chars[num_start..pos].iter().collect();
        parts.push(num_str);

        if parts.len() < 3 {
            if pos >= chars.len() || chars[pos] != '.' {
                return None;
            }
            pos += 1; // skip '.'
        }
    }

    Some((parts.join("."), pos))
}

/// Check whether a parsed version string meets a minimum (major, minor, patch).
fn version_meets_minimum(version: &str, min: (u32, u32, u32)) -> bool {
    let parts: Vec<u32> = version.split('.').filter_map(|s| s.parse().ok()).collect();
    if parts.len() < 3 {
        return false;
    }
    (parts[0], parts[1], parts[2]) >= min
}

/// Resolve the PlutusCore Lean library path.
///
/// Priority: `explicit` argument > `PLUTUS_CORE_DIR` env var.
/// If neither is set, returns a placeholder path (for display purposes only);
/// callers should prefer `resolve_local_plutus_core_dir` to distinguish git mode.
pub fn resolve_plutus_core_dir(explicit: Option<&Path>) -> PathBuf {
    resolve_local_plutus_core_dir(explicit).unwrap_or_else(|| PathBuf::from("PlutusCore"))
}

fn check_plutus_core_at(pc_dir: &Path) -> miette::Result<()> {
    if !pc_dir.is_dir() {
        return Err(miette::miette!(
            "PlutusCore Lean library not found at {path}.\n\n\
             Set the PLUTUS_CORE_DIR environment variable or pass --plutus-core-dir \
             to point to your PlutusCore Lean library checkout.",
            path = pc_dir.display(),
        ));
    }

    let lakefile = pc_dir.join("lakefile.lean");
    if !lakefile.exists() {
        return Err(miette::miette!(
            "PlutusCore directory exists at {path} but does not contain a lakefile.lean.\n\n\
             The directory may be empty, corrupted, or an incompatible version.\n\
             Ensure you have a valid PlutusCore Lean library with a lakefile.lean at:\n\n\
             \t{lakefile}",
            path = pc_dir.display(),
            lakefile = lakefile.display(),
        ));
    }

    Ok(())
}

/// Check whether the configured PlutusCore Lean library exists and contains
/// the expected structure (at minimum, a `lakefile.lean`).
/// Returns `Ok(())` when in git mode (no local path configured) or when the
/// local directory and structure are valid. Returns an informational error
/// with instructions when a local path is configured but invalid.
pub fn check_plutus_core(explicit: Option<&Path>) -> miette::Result<()> {
    match resolve_local_plutus_core_dir(explicit) {
        Some(local_path) => check_plutus_core_at(&local_path),
        None => Ok(()), // git mode — Lake will fetch PlutusCore
    }
}

fn public_json_path(path: &Path) -> String {
    if !path.is_absolute() {
        return path.to_string_lossy().replace('\\', "/");
    }
    if let Ok(cwd) = std::env::current_dir()
        && let Ok(relative) = path.strip_prefix(&cwd)
    {
        return relative.to_string_lossy().replace('\\', "/");
    }
    path.file_name()
        .map(|name| format!("<local>/{}", name.to_string_lossy()))
        .unwrap_or_else(|| "<local>".to_string())
}

fn check_plutus_core_detailed_at(pc_dir: &Path) -> PlutusCoreCheck {
    let found = pc_dir.is_dir();
    let has_lakefile = found && pc_dir.join("lakefile.lean").exists();

    let error = if !found {
        Some(format!(
            "PlutusCore directory not found at {}",
            public_json_path(pc_dir)
        ))
    } else if !has_lakefile {
        Some(format!(
            "PlutusCore directory exists but lakefile.lean is missing at {}",
            public_json_path(&pc_dir.join("lakefile.lean"))
        ))
    } else {
        None
    };

    PlutusCoreCheck {
        found,
        path: public_json_path(pc_dir),
        has_lakefile,
        error,
    }
}

/// Check the PlutusCore library and return a structured report (for doctor).
/// When in git mode, reports success with the git source as path.
pub fn check_plutus_core_detailed(
    explicit: Option<&Path>,
    plutus_core_rev: &str,
) -> PlutusCoreCheck {
    match resolve_local_plutus_core_dir(explicit) {
        Some(local_path) => check_plutus_core_detailed_at(&local_path),
        None => PlutusCoreCheck {
            found: true,
            path: format!("git: input-output-hk/PlutusCoreBlaster @ {plutus_core_rev}"),
            has_lakefile: true,
            error: None,
        },
    }
}

/// Run a full diagnostic check of the verify toolchain and dependencies.
/// Returns a structured report suitable for JSON serialization and human display.
pub fn run_doctor(blaster_rev: &str, plutus_core_rev: &str) -> DoctorReport {
    let lean_check = check_tool_version("lean", MIN_LEAN_VERSION);
    let lake_check = check_tool_version("lake", (0, 0, 0));
    let z3_check = check_tool_version("z3", MIN_Z3_VERSION);
    let plutus_core = check_plutus_core_detailed(None, plutus_core_rev);

    let all_ok = doctor_all_ok(&lean_check, &lake_check, &z3_check, &plutus_core);

    DoctorReport {
        version: DOCTOR_REPORT_VERSION.to_string(),
        tools: vec![lean_check, lake_check, z3_check],
        plutus_core,
        blaster_rev: blaster_rev.to_string(),
        plutus_core_rev: plutus_core_rev.to_string(),
        all_ok,
        capabilities: capabilities(),
    }
}

fn doctor_all_ok(
    lean_check: &ToolCheck,
    lake_check: &ToolCheck,
    z3_check: &ToolCheck,
    plutus_core: &PlutusCoreCheck,
) -> bool {
    lean_check.found
        && lean_check.meets_minimum
        && lake_check.found
        && z3_check.found
        && z3_check.meets_minimum
        && plutus_core.found
        && plutus_core.has_lakefile
}

fn lake_fetch_is_unknown_command(stdout: &str, stderr: &str) -> bool {
    let output_lower = format!("{stdout}\n{stderr}").to_ascii_lowercase();
    output_lower.contains("unknown command 'fetch'")
        || output_lower.contains("unknown command `fetch`")
        || output_lower.contains("unknown command \"fetch\"")
}

fn lake_build_jobs_flag_unsupported(stdout: &str, stderr: &str) -> bool {
    let output_lower = format!("{stdout}\n{stderr}").to_ascii_lowercase();
    (output_lower.contains("unknown option") || output_lower.contains("unrecognized option"))
        && output_lower.contains("-j")
}

fn normalize_jobs_override(max_jobs: Option<usize>) -> Option<usize> {
    max_jobs.and_then(|jobs| (jobs > 0).then_some(jobs))
}

fn lake_build_command_for_jobs(jobs_override: Option<usize>) -> String {
    match jobs_override {
        Some(jobs) => format!("lake build -j {jobs}"),
        None => "lake build".to_string(),
    }
}

fn lake_build_module_command_for_jobs(module: &str, jobs_override: Option<usize>) -> String {
    format!("{} {module}", lake_build_command_for_jobs(jobs_override))
}

fn normalize_failure_exit_code(exit_code: Option<i32>) -> i32 {
    match exit_code {
        Some(code) if code != 0 => code,
        _ => 1,
    }
}

fn command_poll_interval(timeout_secs: u64) -> Duration {
    if timeout_secs == 0 {
        // No timeout configured; keep checks infrequent while still responsive to completion.
        return Duration::from_secs(2);
    }

    // Poll roughly 120 times over the timeout window, bounded to avoid
    // very short (busy) or very long (sluggish) checks.
    let millis = (timeout_secs.saturating_mul(1000) / 120).clamp(200, 5000);
    Duration::from_millis(millis)
}

fn equivalence_theorem_name_for_entry(out_dir: &Path, entry: &ManifestEntry) -> Option<String> {
    let theorem_name = format!("{}_equivalence", entry.lean_theorem);
    let theorem_marker = format!("theorem {theorem_name} :");
    let proof_path = out_dir.join(&entry.lean_file);
    let proof_content = fs::read_to_string(proof_path).ok()?;
    proof_content
        .contains(&theorem_marker)
        .then_some(theorem_name)
}

/// Run dependency sync (`lake fetch` with `lake update` fallback) then
/// `lake build` in the generated workspace directory.
/// Logs are written to `<out_dir>/logs/`.
///
/// `raw_output_bytes` caps the size of the stdout/stderr `tail` retained
/// on the returned `VerifyResult`. Pass `RAW_OUTPUT_TAIL_BYTES` for the
/// default cap, `0` to disable truncation, or any value
/// `<= MAX_RAW_OUTPUT_TAIL_BYTES`. When truncation occurs, the full
/// stream is persisted under `<out_dir>/logs/lake_build.{stdout,stderr}.log`.
pub fn run_proofs(
    out_dir: &Path,
    timeout_secs: u64,
    max_jobs: Option<usize>,
    manifest: &GeneratedManifest,
    raw_output_bytes: usize,
) -> miette::Result<VerifyResult> {
    struct CommandRunResult {
        stdout: String,
        stderr: String,
        exit_code: Option<i32>,
        timed_out: bool,
    }

    fn run_command_with_timeout(
        mut command: Command,
        timeout_secs: u64,
    ) -> miette::Result<CommandRunResult> {
        #[cfg(unix)]
        {
            use std::os::unix::process::CommandExt;
            // Put `lake` in its own process group so timeout handling can terminate
            // descendants (`lean`, `blaster`, `z3`) together.
            command.process_group(0);
        }

        command.stdout(Stdio::piped()).stderr(Stdio::piped());

        let mut child = command
            .spawn()
            .map_err(|e| miette::miette!("Failed to spawn command: {e}"))?;
        let interrupt_cleanup = VerifyInterruptCleanup::install();
        let _active_process = interrupt_cleanup.begin_process(child.id());

        let mut child_stdout = child
            .stdout
            .take()
            .ok_or_else(|| miette::miette!("Failed to capture command stdout"))?;
        let mut child_stderr = child
            .stderr
            .take()
            .ok_or_else(|| miette::miette!("Failed to capture command stderr"))?;

        // Drain pipes while the child runs to avoid deadlocks when verbose commands
        // fill OS pipe buffers before process exit.
        let stdout_reader = thread::spawn(move || -> std::io::Result<Vec<u8>> {
            let mut out = Vec::new();
            child_stdout.read_to_end(&mut out)?;
            Ok(out)
        });
        let stderr_reader = thread::spawn(move || -> std::io::Result<Vec<u8>> {
            let mut out = Vec::new();
            child_stderr.read_to_end(&mut out)?;
            Ok(out)
        });

        let start = Instant::now();
        let timeout = if timeout_secs == 0 {
            None
        } else {
            Some(Duration::from_secs(timeout_secs))
        };
        let poll_interval = command_poll_interval(timeout_secs);
        let mut timed_out = false;
        let mut interrupted = false;

        let status = loop {
            if let Some(status) = child
                .try_wait()
                .map_err(|e| miette::miette!("Failed to poll child process: {e}"))?
            {
                break status;
            }

            if interrupt_cleanup.interrupted() {
                interrupted = true;
                terminate_child_process_tree(&mut child);
                break child
                    .wait()
                    .map_err(|e| miette::miette!("Failed to wait for interrupted command: {e}"))?;
            }

            if timeout.is_some_and(|limit| start.elapsed() >= limit) {
                timed_out = true;
                terminate_child_process_tree(&mut child);
                break child
                    .wait()
                    .map_err(|e| miette::miette!("Failed to wait for timed out command: {e}"))?;
            }

            thread::sleep(poll_interval);
        };

        let stdout = stdout_reader
            .join()
            .map_err(|_| miette::miette!("Failed to join stdout reader thread"))?
            .map_err(|e| miette::miette!("Failed to read command stdout: {e}"))?;
        let stderr = stderr_reader
            .join()
            .map_err(|_| miette::miette!("Failed to join stderr reader thread"))?
            .map_err(|e| miette::miette!("Failed to read command stderr: {e}"))?;

        let mut stderr = String::from_utf8_lossy(&stderr).to_string();
        if interrupted {
            return Err(miette::miette!("Verification interrupted"));
        }
        if timed_out {
            stderr.push_str(&format!(
                "\n[verify-timeout] Command timed out after {}s\n",
                timeout_secs
            ));
        }

        Ok(CommandRunResult {
            stdout: String::from_utf8_lossy(&stdout).to_string(),
            stderr,
            exit_code: status.code(),
            timed_out,
        })
    }

    #[allow(clippy::too_many_arguments)]
    fn theorem_status_from_module_build(
        test_name: String,
        theorem_name: &str,
        entry: &ManifestEntry,
        build_output: &str,
        timed_out: bool,
        build_exit_code: Option<i32>,
        timeout_secs: u64,
        module: &str,
        fallback_to_module_failure: bool,
        fallback_from_sibling_explicit_failure: bool,
        over_approximations: usize,
        caveat: ProofCaveat,
    ) -> TheoremResult {
        let status = if timed_out {
            ProofStatus::TimedOut {
                reason: format!(
                    "Timed out after {}s while building {}",
                    timeout_secs, module
                ),
            }
        } else if build_exit_code == Some(0) {
            // A successful build of a file containing `sorry` (or any other
            // open obligation flagged at generation time) must NOT be reported
            // as `Proved`. Surface as `Partial` so users see the open
            // sub-obligation rather than a misleading verdict. Likewise,
            // witness-only proofs must surface as `WitnessProved` rather than
            // a misleading universal `Proved`.
            match caveat {
                ProofCaveat::None => ProofStatus::Proved,
                ProofCaveat::Partial(note) => ProofStatus::Partial { note },
                ProofCaveat::Witness(WitnessProofNote {
                    instances,
                    witnesses,
                    note,
                }) => ProofStatus::WitnessProved {
                    instances,
                    witnesses,
                    note,
                },
            }
        } else {
            let theorem_failed = theorem_has_explicit_failure(theorem_name, build_output);
            if !theorem_failed && !fallback_to_module_failure {
                ProofStatus::Unknown
            } else {
                let reason = if theorem_failed {
                    let extracted = extract_error_for_theorem(theorem_name, build_output);
                    if extracted.is_empty() {
                        extract_global_failure_reason(build_output)
                    } else {
                        extracted
                    }
                } else {
                    let extracted = extract_error_for_module(module, build_output);
                    if extracted.is_empty() {
                        extract_global_failure_reason(build_output)
                    } else {
                        extracted
                    }
                };
                let category = if fallback_from_sibling_explicit_failure && !theorem_failed {
                    FailureCategory::BuildError
                } else {
                    match classify_failure(&reason) {
                        FailureCategory::Unknown => FailureCategory::BuildError,
                        known => known,
                    }
                };
                ProofStatus::Failed { category, reason }
            }
        };

        // Only carry over_approximations when the proof was actually produced
        // (`Proved`, `Partial`, or `WitnessProved`). Two-phase halt proofs are
        // reported as `Partial` even when the build succeeds, and witness-only
        // proofs as `WitnessProved`; the over-approximation count is still
        // meaningful audit data on both and must be preserved.
        let over_approx = if matches!(
            status,
            ProofStatus::Proved | ProofStatus::Partial { .. } | ProofStatus::WitnessProved { .. }
        ) {
            over_approximations
        } else {
            0
        };
        TheoremResult::from_manifest_entry(
            test_name,
            theorem_name.to_string(),
            entry,
            status,
            over_approx,
        )
    }

    let logs_dir = out_dir.join("logs");
    fs::create_dir_all(&logs_dir)
        .map_err(|e| miette::miette!("Failed to create logs directory: {e}"))?;
    let stdout_log_persist_path = logs_dir.join("lake_build.stdout.log");
    let stderr_log_persist_path = logs_dir.join("lake_build.stderr.log");
    let stdout_log_reported_path = PathBuf::from("logs/lake_build.stdout.log");
    let stderr_log_reported_path = PathBuf::from("logs/lake_build.stderr.log");

    // -- dependency sync: lake fetch (fallback to lake update) --
    let mut fetch_cmd = Command::new("lake");
    fetch_cmd.arg("fetch").current_dir(out_dir);
    let fetch_output = run_command_with_timeout(fetch_cmd, timeout_secs)?;

    // Always persist lake fetch logs.
    let _ = fs::write(
        logs_dir.join("lake-fetch.log"),
        format!(
            "command: lake fetch\n--- stdout ---\n{}\n--- stderr ---\n{}\n",
            &fetch_output.stdout, &fetch_output.stderr
        ),
    );

    let mut dep_step = "lake fetch";
    let dep_output = if !fetch_output.timed_out
        && fetch_output.exit_code != Some(0)
        && lake_fetch_is_unknown_command(&fetch_output.stdout, &fetch_output.stderr)
    {
        let mut update_cmd = Command::new("lake");
        update_cmd.arg("update").current_dir(out_dir);
        let update_output = run_command_with_timeout(update_cmd, timeout_secs)?;
        let _ = fs::write(
            logs_dir.join("lake-update.log"),
            format!(
                "command: lake update\n--- stdout ---\n{}\n--- stderr ---\n{}\n",
                &update_output.stdout, &update_output.stderr
            ),
        );
        dep_step = "lake update";
        update_output
    } else {
        fetch_output
    };
    let dep_timed_out = dep_output.timed_out;
    let dep_exit_code = dep_output.exit_code;
    let dep_stdout = dep_output.stdout;
    let dep_stderr = dep_output.stderr;

    if dep_timed_out {
        let theorem_results = manifest
            .tests
            .iter()
            .flat_map(|entry| {
                let test_name = format!("{}.{}", entry.aiken_module, entry.aiken_name);
                let timeout_reason =
                    format!("Timed out after {}s during `{dep_step}`", timeout_secs);
                let mut theorem_names = vec![entry.lean_theorem.clone()];
                if entry.has_termination_theorem {
                    theorem_names.push(format!("{}_alwaysTerminating", entry.lean_theorem));
                }
                if let Some(equivalence_theorem_name) =
                    equivalence_theorem_name_for_entry(out_dir, entry)
                {
                    theorem_names.push(equivalence_theorem_name);
                }
                theorem_names
                    .into_iter()
                    .map(|theorem_name| {
                        TheoremResult::from_manifest_entry(
                            test_name.clone(),
                            theorem_name,
                            entry,
                            ProofStatus::TimedOut {
                                reason: timeout_reason.clone(),
                            },
                            0,
                        )
                    })
                    .collect::<Vec<_>>()
            })
            .collect();

        return Ok(VerifyResult {
            success: false,
            stdout: CapturedOutput::from_string_with_paths(
                dep_stdout,
                raw_output_bytes,
                Some(stdout_log_persist_path.clone()),
                Some(stdout_log_reported_path.clone()),
            ),
            stderr: CapturedOutput::from_string_with_paths(
                dep_stderr,
                raw_output_bytes,
                Some(stderr_log_persist_path.clone()),
                Some(stderr_log_reported_path.clone()),
            ),
            exit_code: Some(normalize_failure_exit_code(dep_exit_code)),
            theorem_results: Some(theorem_results),
        });
    }

    if dep_exit_code != Some(0) {
        return Ok(VerifyResult {
            success: false,
            stdout: CapturedOutput::from_string_with_paths(
                dep_stdout,
                raw_output_bytes,
                Some(stdout_log_persist_path.clone()),
                Some(stdout_log_reported_path.clone()),
            ),
            stderr: CapturedOutput::from_string_with_paths(
                dep_stderr,
                raw_output_bytes,
                Some(stderr_log_persist_path.clone()),
                Some(stderr_log_reported_path.clone()),
            ),
            exit_code: Some(normalize_failure_exit_code(dep_exit_code)),
            theorem_results: None,
        });
    }

    // Build each theorem module independently so timeout applies per theorem build.
    // If the caller explicitly sets `--jobs`, try forwarding `-j <N>`.
    // If that flag is unsupported in the installed Lake, retry once without it.
    let jobs_override = normalize_jobs_override(max_jobs);
    let mut combined_build_stdout = Vec::new();
    let mut combined_build_stderr = Vec::new();
    let mut combined_build_log = Vec::new();
    let mut theorem_results = Vec::new();
    let mut success = true;
    let mut first_failure_exit_code: Option<i32> = None;

    for entry in &manifest.tests {
        let mut build_cmd = Command::new("lake");
        build_cmd.arg("build").current_dir(out_dir);
        if let Some(jobs) = jobs_override {
            build_cmd.arg("-j").arg(jobs.to_string());
        }
        build_cmd.arg(&entry.lean_module);
        let mut build_command_used =
            lake_build_module_command_for_jobs(&entry.lean_module, jobs_override);
        let mut build_result = match run_command_with_timeout(build_cmd, timeout_secs) {
            Ok(result) => result,
            Err(e) => {
                return Err(miette::miette!(
                    "Failed to execute {build_command_used}: {e}"
                ));
            }
        };

        if jobs_override.is_some()
            && build_result.exit_code != Some(0)
            && lake_build_jobs_flag_unsupported(&build_result.stdout, &build_result.stderr)
        {
            let mut fallback_cmd = Command::new("lake");
            fallback_cmd
                .arg("build")
                .current_dir(out_dir)
                .arg(&entry.lean_module);
            build_command_used = lake_build_module_command_for_jobs(&entry.lean_module, None);
            build_result = run_command_with_timeout(fallback_cmd, timeout_secs)?;
        }

        let module_stdout = build_result.stdout;
        let module_stderr = build_result.stderr;
        let module_exit_code = build_result.exit_code;
        let module_timed_out = build_result.timed_out;
        let module_output = format!("{module_stdout}\n{module_stderr}");

        let _ = fs::write(
            logs_dir.join(format!("lake-build-{}.log", entry.id)),
            format!(
                "command: {build_command_used}\nmodule: {}\n--- stdout ---\n{}\n--- stderr ---\n{}\n",
                entry.lean_module, module_stdout, module_stderr
            ),
        );

        combined_build_log.push(format!(
            "command: {build_command_used}\nmodule: {}\n--- stdout ---\n{}\n--- stderr ---\n{}\n",
            entry.lean_module, module_stdout, module_stderr
        ));

        combined_build_stdout.push(format!("[{}]\n{}", entry.lean_module, module_stdout));
        combined_build_stderr.push(format!("[{}]\n{}", entry.lean_module, module_stderr));

        let test_name = format!("{}.{}", entry.aiken_module, entry.aiken_name);
        let term_theorem_name = entry
            .has_termination_theorem
            .then(|| format!("{}_alwaysTerminating", entry.lean_theorem));
        let equivalence_theorem_name = equivalence_theorem_name_for_entry(out_dir, entry);
        let correctness_failed = theorem_has_explicit_failure(&entry.lean_theorem, &module_output);
        let termination_failed = term_theorem_name
            .as_ref()
            .is_some_and(|name| theorem_has_explicit_failure(name, &module_output));
        let equivalence_failed = equivalence_theorem_name
            .as_ref()
            .is_some_and(|name| theorem_has_explicit_failure(name, &module_output));
        // Keep correctness conservative when a sibling theorem failure is
        // explicitly reported, but still allow termination/equivalence
        // theorems to inherit module-level failures when they have no explicit
        // marker and no sibling auxiliary theorem failed explicitly.
        let correctness_fallback_to_module_failure =
            !correctness_failed && !termination_failed && !equivalence_failed;
        let termination_fallback_to_module_failure = !termination_failed && !equivalence_failed;
        let equivalence_fallback_to_module_failure = !equivalence_failed && !termination_failed;

        let correctness_caveat =
            manifest_entry_caveat_with_soundness_lint(out_dir, manifest, entry);
        theorem_results.push(theorem_status_from_module_build(
            test_name.clone(),
            &entry.lean_theorem,
            entry,
            &module_output,
            module_timed_out,
            module_exit_code,
            timeout_secs,
            &entry.lean_module,
            correctness_fallback_to_module_failure,
            false,
            entry.over_approximations,
            correctness_caveat,
        ));
        if let Some(term_theorem_name) = term_theorem_name {
            theorem_results.push(theorem_status_from_module_build(
                test_name.clone(),
                &term_theorem_name,
                entry,
                &module_output,
                module_timed_out,
                module_exit_code,
                timeout_secs,
                &entry.lean_module,
                termination_fallback_to_module_failure,
                correctness_failed || equivalence_failed,
                0,                 // termination theorem is not a two-phase proof
                ProofCaveat::None, // termination companion always fully proved when present
            ));
        }
        if let Some(equivalence_theorem_name) = equivalence_theorem_name {
            theorem_results.push(theorem_status_from_module_build(
                test_name,
                &equivalence_theorem_name,
                entry,
                &module_output,
                module_timed_out,
                module_exit_code,
                timeout_secs,
                &entry.lean_module,
                equivalence_fallback_to_module_failure,
                correctness_failed || termination_failed,
                0,                 // equivalence theorem is not a two-phase proof
                ProofCaveat::None, // equivalence companion always fully proved when present
            ));
        }

        if module_timed_out || module_exit_code != Some(0) {
            success = false;
            if first_failure_exit_code.is_none() {
                first_failure_exit_code = Some(normalize_failure_exit_code(module_exit_code));
            }
        }
    }

    let _ = fs::write(
        logs_dir.join("lake-build.log"),
        combined_build_log.join("\n"),
    );

    let build_stdout = combined_build_stdout.join("\n");
    let build_stderr = combined_build_stderr.join("\n");
    let exit_code = if success {
        Some(0)
    } else {
        Some(first_failure_exit_code.unwrap_or(1))
    };

    Ok(VerifyResult {
        success,
        stdout: CapturedOutput::from_string_with_paths(
            format!("{dep_stdout}\n{build_stdout}"),
            raw_output_bytes,
            Some(stdout_log_persist_path),
            Some(stdout_log_reported_path),
        ),
        stderr: CapturedOutput::from_string_with_paths(
            format!("{dep_stderr}\n{build_stderr}"),
            raw_output_bytes,
            Some(stderr_log_persist_path),
            Some(stderr_log_reported_path),
        ),
        exit_code,
        theorem_results: Some(theorem_results),
    })
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum GenerationErrorCategory {
    UnsupportedShape,
    FallbackRequired,
    InvalidConstraint,
    /// Hard error: a previously-silent unsound fallback.
    /// `is_skippable_generation_error` returns false for this variant.
    /// `--skip-unsupported` has no effect.
    ///
    /// Constructed indirectly by `error_catalogue::unsupported(code, ...)`
    /// for any catalogue entry whose category is `UnsoundFallback` (today
    /// the S0001/S0002/S0003 codes). Subsequent commits in the "eliminate
    /// false successes" series wire those construction sites in (B3/S0001,
    /// H1/S0002, H5/S0003). The match arm in
    /// `is_skippable_generation_error` already reads this variant.
    #[allow(dead_code)]
    UnsoundFallback,
}

/// Typed payload carrying the structured reason for an unsupported-feature
/// or unsound-fallback error. The variant tag is exposed in JSON via the
/// `code` discriminator (snake_case), with the human-readable code (e.g.
/// "E0023" or "S0001") carried separately on `GenerationError::code`.
///
/// Constructed via `error_catalogue::unsupported(code, reason)` in
/// subsequent commits. The catalogue regression tests in
/// `tests/catalogue.rs` exercise every variant today.
#[allow(dead_code)]
#[derive(Debug, Clone, serde::Serialize)]
#[serde(tag = "code", rename_all = "snake_case")]
pub(crate) enum UnsupportedReason {
    UnitTest {
        test_name: String,
    },
    BenchmarkTest {
        test_name: String,
    },
    TestArityGtOne {
        test_name: String,
        arity: usize,
    },
    PlutusV1V2Validator {
        test_name: String,
        version: u8,
    },
    RecursiveAdtNoMutual {
        test_name: String,
        type_name: String,
    },
    UnboundedBytearray {
        test_name: String,
    },
    StringFuzzer {
        test_name: String,
    },
    ListOfBool {
        test_name: String,
    },
    ListOfBytearray {
        test_name: String,
    },
    QualifiedAdtNoSchema {
        test_name: String,
        type_name: String,
    },
    OpaqueTopLevelFuzzer {
        test_name: String,
        reason: String,
    },
    OpaqueListElementSemantics {
        test_name: String,
        reason: String,
    },
    OpaqueSubgeneratorStub {
        test_name: String,
        sub_generator: String,
    },
    TransactionShapedTest {
        test_name: String,
    },
    NestedStateMachine {
        test_name: String,
    },
    VacuousTransitionPredicate {
        test_name: String,
    },
    StepIrNoConstraints {
        test_name: String,
    },
    ExistentialStateMachineTrace {
        test_name: String,
    },
    StateMachineNoWitnessSynthesizable {
        test_name: String,
    },
    MissingFuzzerSchema {
        test_name: String,
    },
    NonDataFuzzerSchema {
        test_name: String,
    },
    EmptyConstructorDomain {
        test_name: String,
    },
    UnsupportedFuzzerOutputType {
        test_name: String,
        type_repr: String,
    },
    OverlappingTransitionIndexes {
        test_name: String,
    },
    TupleElementNoLeanMapping {
        test_name: String,
        element_type: String,
    },
    ListElementNoLeanMapping {
        test_name: String,
        element_type: String,
    },
    NonFirstOrderSuchThatHelper {
        test_name: String,
        helper: String,
    },
    WhenPatternConstructorVarDropped {
        test_name: String,
        pattern: String,
        binders: Vec<String>,
    },
    FiniteDomainTooLarge {
        test_name: String,
        cases: String,
        cap: usize,
        cap_constant: &'static str,
    },
    FiniteDomainTargetModeUnsupported {
        test_name: String,
        target: String,
    },
    FiniteDomainExistentialUnsupported {
        test_name: String,
    },
    SemanticsOutputTypeMismatch {
        test_name: String,
    },
    ValidatorTargetMissing {
        test_name: String,
    },
    // S-codes — UnsoundFallback, never skippable
    StepFnSoundAxiomEmitted {
        test_name: String,
        shape: String,
    },
    ConstructorTagUnresolved {
        test_name: String,
        ctor: String,
        type_name: String,
    },
    ExistentialWitnessUnsoundForDomain {
        test_name: String,
        domain: String,
    },
}

impl UnsupportedReason {
    fn test_name(&self) -> &str {
        match self {
            UnsupportedReason::UnitTest { test_name }
            | UnsupportedReason::BenchmarkTest { test_name }
            | UnsupportedReason::TestArityGtOne { test_name, .. }
            | UnsupportedReason::PlutusV1V2Validator { test_name, .. }
            | UnsupportedReason::RecursiveAdtNoMutual { test_name, .. }
            | UnsupportedReason::UnboundedBytearray { test_name }
            | UnsupportedReason::StringFuzzer { test_name }
            | UnsupportedReason::ListOfBool { test_name }
            | UnsupportedReason::ListOfBytearray { test_name }
            | UnsupportedReason::QualifiedAdtNoSchema { test_name, .. }
            | UnsupportedReason::OpaqueTopLevelFuzzer { test_name, .. }
            | UnsupportedReason::OpaqueListElementSemantics { test_name, .. }
            | UnsupportedReason::OpaqueSubgeneratorStub { test_name, .. }
            | UnsupportedReason::TransactionShapedTest { test_name }
            | UnsupportedReason::NestedStateMachine { test_name }
            | UnsupportedReason::VacuousTransitionPredicate { test_name }
            | UnsupportedReason::StepIrNoConstraints { test_name }
            | UnsupportedReason::ExistentialStateMachineTrace { test_name }
            | UnsupportedReason::StateMachineNoWitnessSynthesizable { test_name }
            | UnsupportedReason::MissingFuzzerSchema { test_name }
            | UnsupportedReason::NonDataFuzzerSchema { test_name }
            | UnsupportedReason::EmptyConstructorDomain { test_name }
            | UnsupportedReason::UnsupportedFuzzerOutputType { test_name, .. }
            | UnsupportedReason::OverlappingTransitionIndexes { test_name }
            | UnsupportedReason::TupleElementNoLeanMapping { test_name, .. }
            | UnsupportedReason::ListElementNoLeanMapping { test_name, .. }
            | UnsupportedReason::NonFirstOrderSuchThatHelper { test_name, .. }
            | UnsupportedReason::WhenPatternConstructorVarDropped { test_name, .. }
            | UnsupportedReason::FiniteDomainTooLarge { test_name, .. }
            | UnsupportedReason::FiniteDomainTargetModeUnsupported { test_name, .. }
            | UnsupportedReason::FiniteDomainExistentialUnsupported { test_name }
            | UnsupportedReason::SemanticsOutputTypeMismatch { test_name }
            | UnsupportedReason::ValidatorTargetMissing { test_name }
            | UnsupportedReason::StepFnSoundAxiomEmitted { test_name, .. }
            | UnsupportedReason::ConstructorTagUnresolved { test_name, .. }
            | UnsupportedReason::ExistentialWitnessUnsoundForDomain { test_name, .. } => test_name,
        }
    }

    fn user_message(&self, summary: &str) -> String {
        match self {
            UnsupportedReason::QualifiedAdtNoSchema {
                test_name,
                type_name,
            } => {
                format!(
                    "Test '{test_name}' uses DataWithSchema<{type_name}> but has no exported fuzzer schema; cannot derive the Lean shape predicate."
                )
            }
            UnsupportedReason::OpaqueListElementSemantics { test_name, reason } => {
                format!("Test '{test_name}': {summary} Underlying lowering error: {reason}")
            }
            UnsupportedReason::MissingFuzzerSchema { test_name } => {
                format!(
                    "Test '{test_name}' is missing exported fuzzer schema definition; cannot derive the Lean precondition."
                )
            }
            UnsupportedReason::ValidatorTargetMissing { test_name } => {
                format!(
                    "Test '{test_name}' has no validator target metadata required for the selected verification target."
                )
            }
            UnsupportedReason::FiniteDomainTooLarge {
                test_name,
                cases,
                cap,
                cap_constant,
            } => {
                format!(
                    "Test '{test_name}': finite domain has {cases} cases, exceeds cap of {cap} ({cap_constant})"
                )
            }
            UnsupportedReason::FiniteDomainTargetModeUnsupported { test_name, target } => {
                format!(
                    "Test '{test_name}' has a finite fuzzer domain, but target mode '{target}' is not supported for finite-domain theorem generation."
                )
            }
            UnsupportedReason::FiniteDomainExistentialUnsupported { test_name } => {
                format!(
                    "Test '{test_name}' has a finite fuzzer domain, but finite-domain existential theorem generation is not supported for fail-style theorem families."
                )
            }
            UnsupportedReason::SemanticsOutputTypeMismatch { test_name } => {
                format!(
                    "Test '{test_name}' has fuzzer semantics that disagree with the declared output type."
                )
            }
            _ => format!("Test '{}': {summary}", self.test_name()),
        }
    }
}

#[derive(Debug)]
pub(crate) struct GenerationError {
    pub(crate) category: GenerationErrorCategory,
    pub(crate) message: String,
    /// Stable error code from the catalogue (e.g. "E0023", "S0001"). `None`
    /// for legacy producers that have not yet been migrated to the catalogue.
    /// Subsequent commits read this field from `aiken verify` JSON output and
    /// `aiken verify capabilities`; this commit only populates it via the
    /// `unsupported(...)` constructor.
    #[allow(dead_code)]
    pub(crate) code: Option<&'static str>,
    /// Typed payload describing the underlying unsupported reason. `None`
    /// for legacy producers that have not yet been migrated to the catalogue.
    /// Subsequent commits serialise this into the JSON envelope; this commit
    /// only populates it via the `unsupported(...)` constructor.
    #[allow(dead_code)]
    pub(crate) reason: Option<UnsupportedReason>,
}

impl std::fmt::Display for GenerationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.message)
    }
}

impl std::error::Error for GenerationError {}
impl miette::Diagnostic for GenerationError {
    fn code<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.code
            .map(|code| Box::new(code) as Box<dyn std::fmt::Display + 'a>)
    }

    fn help<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.code
            .and_then(error_catalogue::lookup)
            .map(|entry| Box::new(entry.diagnostic_help()) as Box<dyn std::fmt::Display + 'a>)
    }

    fn url<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        None
    }
}

pub fn generation_error_metadata(
    error: &miette::Report,
) -> (String, Option<String>, Option<String>, Option<String>) {
    let Some(generation_error) = error.downcast_ref::<GenerationError>() else {
        return (error.to_string(), None, None, None);
    };

    (
        generation_error.message.clone(),
        generation_error.code.map(str::to_string),
        generation_error
            .code
            .and_then(error_catalogue::lookup)
            .map(|entry| entry.diagnostic_help()),
        None,
    )
}

fn generation_error(
    category: GenerationErrorCategory,
    message: impl Into<String>,
) -> miette::Report {
    miette::Report::new(GenerationError {
        category,
        message: message.into(),
        code: None,
        reason: None,
    })
}

fn unsupported_error(code: &'static str, reason: UnsupportedReason) -> miette::Report {
    miette::Report::new(error_catalogue::unsupported(code, reason))
}

fn display_output_types(types: &[FuzzerOutputType]) -> String {
    types
        .iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join(", ")
}

/// Policy controlling which unsupported tests `--skip-unsupported` silences.
///
/// The CLI surfaces three knobs:
///
/// * `--strict-unsupported` (or omitting `--skip-unsupported` entirely) →
///   [`SkipPolicy::None`]: every unsupported/fallback-required error is a hard
///   error, exactly like a fully-unsupported test today.
/// * `--skip-unsupported` (no value) → [`SkipPolicy::All`]: skip every
///   skippable category (the current default behaviour).
/// * `--skip-unsupported=E0011,E0012` → [`SkipPolicy::Codes(set)`]: skip only
///   the listed catalogue codes; any other skippable code becomes a hard error.
///
/// `UnsoundFallback` (`S0001`/`S0002`/`S0003`) and `InvalidConstraint`
/// categories are NEVER silenced regardless of the chosen policy — that
/// invariant is enforced inside [`is_skippable_generation_error`] and exercised
/// by `unsound_fallback_codes_are_never_skippable` in the catalogue tests.
#[non_exhaustive]
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub enum SkipPolicy {
    /// `--strict-unsupported` (default). No skippable category is silenced.
    #[default]
    None,
    /// `--skip-unsupported` with no value. Every skippable category is silenced.
    All,
    /// `--skip-unsupported=CODE,...`. Only the listed catalogue codes are
    /// silenced — other skippable codes still hard-error.
    Codes(BTreeSet<String>),
}

impl SkipPolicy {
    /// True when this policy would silence at least one skippable category. Used
    /// by callers (e.g. `--generate-only` preflight) that need to know whether
    /// any skipping is opted in.
    pub fn is_active(&self) -> bool {
        !matches!(self, SkipPolicy::None)
    }

    /// Convenience constructor for the CLI plumbing layer: maps the raw clap
    /// `Option<Vec<String>>` shape to a policy.
    pub fn from_cli(skip_unsupported: Option<Vec<String>>) -> Self {
        match skip_unsupported {
            None => SkipPolicy::None,
            Some(codes) if codes.is_empty() => SkipPolicy::All,
            Some(codes) => SkipPolicy::Codes(codes.into_iter().collect()),
        }
    }
}

fn is_skippable_generation_error(error: &miette::Report, policy: &SkipPolicy) -> bool {
    let Some(generation_error) = error.downcast_ref::<GenerationError>() else {
        return false;
    };

    // Commit 18 (folds C15 #1): single source of truth for the
    // category-skippable predicate is `error_catalogue::category_is_skippable`.
    // `InvalidConstraint` and `UnsoundFallback` (S0001/S0002/S0003) are
    // user-visible hard errors that the helper returns `false` for
    // unconditionally — the catalogue regression test
    // `unsound_fallback_codes_are_never_skippable` pins that contract.
    if !error_catalogue::category_is_skippable(generation_error.category) {
        return false;
    }

    // Skippable category: consult policy.
    match policy {
        SkipPolicy::None => false,
        SkipPolicy::All => true,
        SkipPolicy::Codes(codes) => generation_error
            .code
            .map(|c| codes.contains(c))
            .unwrap_or(false),
    }
}

/// Generate a Lean workspace from exported property tests.
///
/// The `skip_policy` controls which unsupported tests become `SkippedTest`
/// entries vs hard errors. See [`SkipPolicy`] for the encoding of the CLI
/// `--skip-unsupported`/`--strict-unsupported` flags.
pub fn generate_lean_workspace(
    tests: &[ExportedPropertyTest],
    config: &VerifyConfig,
    skip_policy: &SkipPolicy,
) -> miette::Result<GeneratedManifest> {
    validate_git_rev(&config.blaster_rev, "blaster-rev")?;
    validate_git_rev(&config.plutus_core_rev, "plutus-core-rev")?;
    check_plutus_core(config.plutus_core_dir.as_deref())?;

    struct PreparedManifestEntry {
        id: String,
        aiken_module: String,
        aiken_name: String,
        lean_module_segment: String,
        lean_test_name: String,
        lean_module: String,
        lean_file: String,
        proof_content: String,
        flat_content: String,
        fuzzer_flat_content: String,
        handler_flat_content: Option<String>,
        return_mode: TestReturnMode,
        test_mode: ManifestTestMode,
        on_test_failure: ManifestOnTestFailure,
        property_uplc_hash: String,
        property_harness_hash: String,
        fuzzer_uplc_hash: String,
        fuzzer_harness_hash: String,
        fuzzer_model_hash: String,
        domain: LoweredDomain,
        input_type: InputValueBridge,
        sampler: Option<SamplerMetadata>,
        compatibility_limitations: Vec<String>,
        has_termination_theorem: bool,
        has_equivalence_theorem: bool,
        over_approximations: usize,
        /// Caveat associated with the emitted proof file.
        ///
        /// - `ProofCaveat::None` — fully-proved (no open sub-obligations).
        /// - `ProofCaveat::Partial(_)` — `sorry`-closed sub-obligation
        ///   (today: two-phase halt Phase 2).
        /// - `ProofCaveat::Witness(_)` — proof covers concrete witness
        ///   instance(s) only (commit 5 wires this in; not yet emitted in
        ///   commit 2).
        proof_caveat: ProofCaveat,
    }

    let out = &config.out_dir;
    clear_generated_workspace(out)?;

    // Create directory structure
    fs::create_dir_all(out.join("AikenVerify/Proofs"))
        .map_err(|e| miette::miette!("Failed to create workspace directories: {e}"))?;
    fs::create_dir_all(out.join("cbor"))
        .map_err(|e| miette::miette!("Failed to create cbor directory: {e}"))?;

    // Write lakefile.lean
    write_file(
        &out.join("lakefile.lean"),
        &generate_lakefile(
            &config.blaster_rev,
            &config.plutus_core_rev,
            config.plutus_core_dir.as_deref(),
        ),
    )?;

    // Write lean-toolchain
    write_file(&out.join("lean-toolchain"), LEAN_TOOLCHAIN_LITERAL)?;

    // Write Utils.lean
    write_file(
        &out.join("AikenVerify/Utils.lean"),
        &generate_utils(config.cek_budget),
    )?;

    let (aiken_metadata, mut compatibility_limitations) = manifest_aiken_metadata(config)?;
    let (execution_metadata, execution_limitations) = manifest_execution_metadata(config);
    compatibility_limitations.extend(execution_limitations);
    compatibility_limitations.sort();
    compatibility_limitations.dedup();

    let mut prepared_entries = Vec::new();
    let mut skipped_tests = Vec::new();
    let mut seen_lean_paths: HashMap<String, String> = HashMap::new();
    let mut collisions: Vec<String> = Vec::new();

    // Collect manifest entries and generate per-test files
    let mut manifest_entries = Vec::new();
    // Track module directories we need to create and their imports
    let mut module_dirs: BTreeMap<String, Vec<String>> = BTreeMap::new();

    for test in tests {
        let full_name = &test.name; // e.g. "my_module.test_something"
        let module = &test.module; // e.g. "my_module" or "aiken/list"
        let test_name = full_name
            .strip_prefix(&format!("{module}."))
            .unwrap_or(full_name);

        let lean_module_segment = module_to_lean_segment(module);
        let lean_test_name = sanitize_lean_name(test_name);
        let id = test_id(module, test_name);

        // Lean module path: AikenVerify.Proofs.<Module>.<TestName>
        let lean_module = format!("AikenVerify.Proofs.{lean_module_segment}.{lean_test_name}");

        // Lean file path relative to out_dir
        let lean_file_rel =
            format!("AikenVerify/Proofs/{lean_module_segment}/{lean_test_name}.lean");

        // Generate proof content or classify skip/error before collision checks.
        // The second tuple element is a `ProofCaveat` describing the kind of
        // open obligation, if any:
        //   - `ProofCaveat::None` — fully-proved.
        //   - `ProofCaveat::Partial(note)` — sub-obligation closed by `sorry`
        //     (today: two-phase halt Phase 2). Surfaces as `Partial`.
        //   - `ProofCaveat::Witness(_)` — witness-only proof (commit 5 wires
        //     this in). Surfaces as `WitnessProved`.
        let preflight_mode = compatibility_mode_for_generation(test, config.existential_mode, None);
        let preflight_domain = lowered_domain_for_test(
            test,
            preflight_mode,
            None,
            None,
            &config.target,
            config.existential_mode,
        );
        let preflight_block_note = assess_domain_compatibility(
            preflight_mode,
            Some(test.return_mode.clone()),
            TrustProfile::Production,
            &preflight_domain,
            match preflight_mode {
                CompatibilityMode::WitnessCheck => SuccessChannel::WitnessReplay,
                CompatibilityMode::UniversalSuccess
                | CompatibilityMode::UniversalFailure
                | CompatibilityMode::SemanticExistential => SuccessChannel::SolverTheorem,
            },
        )
        .block_reason;
        let (proof_content, proof_caveat) = match generate_proof_file(
            test,
            &id,
            &lean_test_name,
            &lean_module,
            config.existential_mode,
            &config.target,
            config.allow_vacuous_subgenerators,
        ) {
            Ok((proof_content, proof_caveat)) => {
                let proof_caveat = apply_soundness_lint_to_caveat(proof_caveat, &proof_content);
                let proof_caveat = match preflight_block_note.clone() {
                    Some(note) => append_partial_caveat_note(proof_caveat, note),
                    None => proof_caveat,
                };
                (proof_content.clone(), proof_caveat)
            }
            Err(e) if is_skippable_generation_error(&e, skip_policy) => {
                let reason = if let Some(generation_error) = e.downcast_ref::<GenerationError>() {
                    generation_error.code.map_or_else(
                        || generation_error.to_string(),
                        |code| format!("{code}: {}", generation_error.message),
                    )
                } else {
                    e.to_string()
                };
                skipped_tests.push(SkippedTest::new(full_name.clone(), module.clone(), reason));
                continue;
            }
            Err(e) if skip_policy.is_active() => {
                return Err(miette::miette!(
                    "Test '{}' failed workspace generation with non-skippable error:\n{}",
                    full_name,
                    e
                ));
            }
            Err(e) => return Err(e),
        };

        let original = format!("{module}.{test_name}");
        if let Some(existing) = seen_lean_paths.get(&lean_file_rel) {
            collisions.push(format!(
                "  \"{lean_file_rel}\" generated by both \"{existing}\" and \"{original}\""
            ));
            continue;
        }
        seen_lean_paths.insert(lean_file_rel.clone(), original);

        // Termination theorem is generated only for Bool-returning universal
        // properties (FailImmediately / SucceedEventually). All other forms
        // (Void, existential/fail-once) either embed termination in the
        // correctness theorem or omit it entirely.
        let has_termination_theorem = {
            use crate::export::TestReturnMode;
            use aiken_lang::ast::OnTestFailure;
            matches!(
                (&test.return_mode, &test.on_test_failure),
                (TestReturnMode::Bool, OnTestFailure::FailImmediately)
                    | (TestReturnMode::Bool, OnTestFailure::SucceedEventually)
            )
        };

        let equivalence_theorem_name = format!("{}_equivalence", lean_test_name);
        let has_equivalence_theorem =
            proof_content.contains(&format!("theorem {equivalence_theorem_name} :"));

        let handler_flat_content = if matches!(
            config.target,
            VerificationTargetKind::ValidatorHandler | VerificationTargetKind::Equivalence
        ) {
            test.validator_target.as_ref().and_then(|vt| {
                vt.handler_program
                    .as_ref()
                    .map(|program| program.hex.clone())
            })
        } else {
            None
        };

        let property_label = format!("property test {full_name}");
        let (property_uplc_hash, property_harness_hash) = manifest_program_hashes(
            &property_label,
            &test.test_program.hex,
            config.plutus_version,
        )?;
        let fuzzer_label = format!("fuzzer for {full_name}");
        let (fuzzer_uplc_hash, fuzzer_harness_hash) = manifest_program_hashes(
            &fuzzer_label,
            &test.fuzzer_program.hex,
            config.plutus_version,
        )?;
        let fuzzer_model_hash = fuzzer_model_hash_for_test(test)?;

        // Count transition-theorem over-approximations for halt tests whenever
        // the emitted proof actually uses the transition relation. Witness-only
        // native_decide fallbacks do not emit that theorem and therefore carry
        // no transition-widening budget.
        let over_approximations = if is_state_machine_trace_halt_test(test)
            && !matches!(&proof_caveat, ProofCaveat::Witness(_))
        {
            test.transition_prop_lean
                .as_ref()
                .map(|tp| {
                    let theorem_widenings = transition_prop_widenings(tp);
                    widening_category_counts(
                        &theorem_widenings,
                        config.allow_vacuous_subgenerators,
                        &tp.opaque_sub_generators,
                    )
                    .values()
                    .sum()
                })
                .unwrap_or(0)
        } else {
            0
        };

        let final_mode =
            compatibility_mode_for_generation(test, config.existential_mode, Some(&proof_caveat));
        let domain = lowered_domain_for_test(
            test,
            final_mode,
            Some(&proof_content),
            Some(&proof_caveat),
            &config.target,
            config.existential_mode,
        );
        let input_type = input_value_bridge_for_test(test).input_type;
        let sampler = domain.sampler.clone();

        prepared_entries.push(PreparedManifestEntry {
            id,
            aiken_module: module.clone(),
            aiken_name: test_name.to_string(),
            lean_module_segment,
            lean_test_name,
            lean_module,
            lean_file: lean_file_rel,
            proof_content,
            flat_content: test.test_program.hex.clone(),
            fuzzer_flat_content: test.fuzzer_program.hex.clone(),
            handler_flat_content,
            return_mode: test.return_mode.clone(),
            test_mode: ManifestTestMode::from_on_test_failure(&test.on_test_failure),
            on_test_failure: ManifestOnTestFailure::from(&test.on_test_failure),
            property_uplc_hash,
            property_harness_hash,
            fuzzer_uplc_hash,
            fuzzer_harness_hash,
            fuzzer_model_hash,
            domain,
            input_type,
            sampler,
            compatibility_limitations: Vec::new(),
            has_termination_theorem,
            has_equivalence_theorem,
            over_approximations,
            proof_caveat,
        });
    }

    if !collisions.is_empty() {
        return Err(miette::miette!(
            "Lean file path collisions detected — generated files would overwrite each other:\n{}",
            collisions.join("\n")
        ));
    }

    for entry in prepared_entries {
        let proof_dir = out
            .join("AikenVerify/Proofs")
            .join(&entry.lean_module_segment);
        fs::create_dir_all(&proof_dir)
            .map_err(|e| miette::miette!("Failed to create proof directory: {e}"))?;
        write_file(&out.join(&entry.lean_file), &entry.proof_content)?;

        let cbor_file_rel = format!("cbor/{}.cbor", entry.id);
        write_file(&out.join(&cbor_file_rel), &entry.flat_content)?;
        let fuzzer_cbor_rel = format!("cbor/{}_fuzzer.cbor", entry.id);
        write_file(&out.join(&fuzzer_cbor_rel), &entry.fuzzer_flat_content)?;
        if let Some(handler_flat_content) = entry.handler_flat_content.as_deref() {
            let handler_cbor_rel = format!("cbor/{}_handler.cbor", entry.id);
            write_file(&out.join(&handler_cbor_rel), handler_flat_content)?;
        }

        module_dirs
            .entry(entry.lean_module_segment.clone())
            .or_default()
            .push(entry.lean_test_name.clone());

        // Derive `partial_proof_note` and `witness_proof_note` from the
        // `ProofCaveat` carried on the prepared entry. Exactly one of the
        // two notes is `Some` when the caveat is non-`None`.
        let (partial_proof_note, witness_proof_note) = match entry.proof_caveat {
            ProofCaveat::None => (None, None),
            ProofCaveat::Partial(note) => (Some(note), None),
            ProofCaveat::Witness(note) => (None, Some(note)),
        };
        manifest_entries.push(ManifestEntry {
            id: entry.id,
            aiken_module: entry.aiken_module,
            aiken_name: entry.aiken_name,
            lean_module: entry.lean_module,
            lean_theorem: entry.lean_test_name,
            lean_file: entry.lean_file,
            flat_file: cbor_file_rel,
            fuzzer_flat_file: Some(fuzzer_cbor_rel),
            return_mode: Some(entry.return_mode),
            test_mode: Some(entry.test_mode),
            on_test_failure: Some(entry.on_test_failure),
            property_uplc_hash: Some(entry.property_uplc_hash),
            property_harness_hash: Some(entry.property_harness_hash),
            fuzzer_uplc_hash: Some(entry.fuzzer_uplc_hash),
            fuzzer_harness_hash: Some(entry.fuzzer_harness_hash),
            fuzzer_model_hash: Some(entry.fuzzer_model_hash),
            domain: Some(entry.domain),
            input_type: Some(entry.input_type),
            sampler: entry.sampler,
            compatibility_limitations: entry.compatibility_limitations,
            has_termination_theorem: entry.has_termination_theorem,
            has_equivalence_theorem: entry.has_equivalence_theorem,
            over_approximations: entry.over_approximations,
            partial_proof_note,
            witness_proof_note,
        });
    }

    // Write root AikenVerify.lean
    write_file(
        &out.join("AikenVerify.lean"),
        &generate_root_import(&module_dirs),
    )?;

    // Write manifest.json
    let manifest = GeneratedManifest {
        schema_version: GENERATED_MANIFEST_SCHEMA_VERSION,
        version: GENERATE_ONLY_VERSION.to_string(),
        aiken: aiken_metadata,
        execution: execution_metadata,
        compatibility_limitations,
        tests: manifest_entries,
        skipped: skipped_tests,
    };

    let manifest_json = serde_json::to_string_pretty(&manifest)
        .map_err(|e| miette::miette!("Failed to serialize manifest: {e}"))?;
    write_file(&out.join("manifest.json"), &manifest_json)?;

    Ok(manifest)
}

fn write_file(path: &Path, content: &str) -> miette::Result<()> {
    fs::write(path, content).map_err(|e| miette::miette!("Failed to write {}: {e}", path.display()))
}

/// Escape a filesystem path for embedding in a Lean string literal.
/// Replaces backslashes with forward slashes (Windows compat) and escapes
/// double-quotes and literal backslashes.
fn escape_path_for_lean(path: &Path) -> String {
    let s = path.display().to_string();
    // Replace backslashes with forward slashes, then escape any remaining
    // backslashes and double-quotes for Lean string literals.
    s.replace('\\', "/").replace('"', "\\\"")
}

fn generate_lakefile(
    blaster_rev: &str,
    plutus_core_rev: &str,
    pc_dir_override: Option<&Path>,
) -> String {
    let plutus_core_require = if let Some(local_path) =
        resolve_local_plutus_core_dir(pc_dir_override)
    {
        let escaped = escape_path_for_lean(&local_path);
        format!("require PlutusCore from\n  \"{escaped}\"")
    } else {
        format!(
            "require PlutusCore from git\n  \"https://github.com/input-output-hk/PlutusCoreBlaster\" @ \"{plutus_core_rev}\""
        )
    };
    format!(
        r#"import Lake
open Lake DSL

package AikenVerify where

@[default_target]
lean_lib AikenVerify where

require Blaster from git
  "https://github.com/input-output-hk/Lean-blaster" @ "{blaster_rev}"

{plutus_core_require}
"#,
    )
}

/// Resolve a local PlutusCore path from explicit arg or env var.
/// Returns `None` when neither is set (git mode).
fn resolve_local_plutus_core_dir(explicit: Option<&Path>) -> Option<PathBuf> {
    let path = if let Some(p) = explicit {
        Some(p.to_path_buf())
    } else if let Ok(env_val) = std::env::var("PLUTUS_CORE_DIR") {
        Some(PathBuf::from(env_val))
    } else {
        None
    }?;

    Some(std::fs::canonicalize(&path).unwrap_or(path))
}

fn generate_utils(cek_budget: u64) -> String {
    format!(
        r#"import PlutusCore.UPLC.CekMachine
import PlutusCore.UPLC.PlutusScript

namespace AikenVerify.Utils
open PlutusCore.Integer (Integer)
open PlutusCore.ByteString (ByteString)
open PlutusCore.UPLC.CekMachine
open PlutusCore.UPLC.CekValue (CekValue)
open PlutusCore.Data (Data)
open PlutusCore.UPLC.Term (Term Const Program)
open PlutusCore.UPLC.PlutusScript (PlutusScript)

def fromHaltState (s : State): Option CekValue :=
  match s with
  | .Halt cv => some cv
  | _ => none

def fromFrameToInt (s : State): Option Integer :=
  match s with
  | .Halt (.VCon (Const.Integer x)) => some x
  | _ => none

def fromFrameToBool (s : State) : Option Bool :=
  match s with
  | .Halt (.VCon (Const.Bool x)) => some x
  | _ => none

def fromFrameToData (s : State) : Option Data :=
  match s with
  | .Halt (.VCon (Const.Data x)) => some x
  | _ => none

def integerToBuiltin (x : Integer) : Term := Term.Const (Const.Integer x)

def executeIntProgram (p : Program) (args : List Term) (exUnit : Nat) : Option Integer :=
  fromFrameToInt $ cekExecuteProgram p args exUnit

def executeDataProgram (p : Program) (args : List Term) (exUnit : Nat) : Option Data :=
  fromFrameToData $ cekExecuteProgram p args exUnit

def intArgs2 (x : Integer) (y : Integer) : List Term :=
    [integerToBuiltin x, integerToBuiltin y]

def intArgs3 (x : Integer) (y : Integer) (z : Integer) : List Term :=
    [integerToBuiltin x, integerToBuiltin y, integerToBuiltin z]

def isErrorState : State -> Prop
 | State.Error => True
 | _ => False

def isHaltState : State -> Prop
 | .Halt _ => True
 | _ => False

inductive ReturnMode where
  | bool
  | void
deriving DecidableEq

inductive TestMode where
  | normal
  | fail
  | failOnce
deriving DecidableEq

inductive DecodePolicy where
  | decodeErrorIsTestFailure
  | decodeErrorIsExportError
deriving DecidableEq

inductive TestEvalOutcome where
  | returnsTrue
  | returnsFalse
  | returnsVoid
  | evalError
  | budgetExceeded
  | decodeError
deriving DecidableEq

inductive PrngMode where
  | seeded
  | replayed
deriving DecidableEq

structure FuzzerRunConfig where
  fuel : Nat
  prngMode : PrngMode
  size : Option Integer
  decodePolicy : DecodePolicy
deriving DecidableEq

inductive FuzzerRunResult where
  | success : Data → Data → FuzzerRunResult
  | failure : FuzzerRunResult
  | error : FuzzerRunResult
  | timeout : FuzzerRunResult
deriving DecidableEq

abbrev defaultFuzzerFuel : Nat := {cek_budget}

abbrev defaultFuzzerRunConfig
    (prngMode : PrngMode)
    (decodePolicy : DecodePolicy) : FuzzerRunConfig :=
  {{ fuel := defaultFuzzerFuel, prngMode := prngMode, size := none, decodePolicy := decodePolicy }}

def validSeededPrng : Data → Prop
  | Data.Constr 0 [Data.B _, Data.B _] => True
  | _ => False

def validReplayedPrng : Data → Prop
  | Data.Constr 1 [Data.I n, Data.B _] => 0 ≤ n
  | _ => False

def runFuzzerHarness (cfg : FuzzerRunConfig) (harness : PlutusScript) (prng : Data) : FuzzerRunResult :=
  match cekExecuteProgram harness.script [Term.Const (Const.Data prng)] cfg.fuel with
  | .Halt (.VCon (Const.Data (Data.Constr 0 [Data.List [nextPrng, rawValue]]))) =>
      .success nextPrng rawValue
  | .Halt (.VCon (Const.Data (Data.Constr 1 []))) => .failure
  | .Halt _ => .error
  | .Error => .error
  | _ => .timeout

def SeededFuzzerReturns (cfg : FuzzerRunConfig) (harness : PlutusScript) (x : Data) : Prop :=
  ∃ prng0 nextPrng rawValue,
    validSeededPrng prng0 ∧
    runFuzzerHarness cfg harness prng0 = .success nextPrng rawValue ∧
    rawValue = x

def ReplayedFuzzerReturns (cfg : FuzzerRunConfig) (harness : PlutusScript) (x : Data) : Prop :=
  ∃ prng0 nextPrng rawValue,
    validReplayedPrng prng0 ∧
    runFuzzerHarness cfg harness prng0 = .success nextPrng rawValue ∧
    rawValue = x

def WitnessReplays
    (cfg : FuzzerRunConfig)
    (harness : PlutusScript)
    (replayPrng : Data)
    (x : Data) : Prop :=
  validReplayedPrng replayPrng ∧
  ∃ nextPrng rawValue,
    runFuzzerHarness cfg harness replayPrng = .success nextPrng rawValue ∧
    rawValue = x

def FuzzerReturns (cfg : FuzzerRunConfig) (harness : PlutusScript) (x : Data) : Prop :=
  match cfg.prngMode with
  | .seeded => SeededFuzzerReturns cfg harness x
  | .replayed => ReplayedFuzzerReturns cfg harness x

def SizedFuzzerReturns (cfg : FuzzerRunConfig) (harness : PlutusScript) (x : Data) : Prop :=
  FuzzerReturns cfg harness x

abbrev RunProperty (returnMode : ReturnMode) (p : PlutusScript) (args : List Term) : TestEvalOutcome :=
  match returnMode, cekExecuteProgram p.script args {cek_budget} with
  | .bool, .Halt (.VCon (Const.Bool true)) => .returnsTrue
  | .bool, .Halt (.VCon (Const.Bool false)) => .returnsFalse
  | .bool, _ => .evalError
  | .void, .Halt _ => .returnsVoid
  | .void, _ => .evalError
abbrev PropertySucceeds (returnMode : ReturnMode) (out : TestEvalOutcome) : Prop :=
  match returnMode, out with
  | .bool, .returnsTrue => True
  | .void, .returnsVoid => True
  | _, _ => False

abbrev PropertyFails
    (returnMode : ReturnMode)
    (decodePolicy : DecodePolicy)
    (out : TestEvalOutcome) : Prop :=
  match returnMode, out with
  | .bool, .returnsFalse => True
  | .void, .evalError => True
  | _, .budgetExceeded => False
  | _, .decodeError =>
      match decodePolicy with
      | .decodeErrorIsTestFailure => True
      | .decodeErrorIsExportError => False
  | _, _ => False


abbrev PropertyFailsBool
    (returnMode : ReturnMode)
    (decodePolicy : DecodePolicy)
    (out : TestEvalOutcome) : Bool :=
  match returnMode, out with
  | .bool, .returnsFalse => true
  | .void, .evalError => true
  | _, .budgetExceeded => false
  | _, .decodeError =>
      match decodePolicy with
      | .decodeErrorIsTestFailure => true
      | .decodeErrorIsExportError => false
  | _, _ => false

theorem PropertyFailsBool_iff_PropertyFails
    (returnMode : ReturnMode)
    (decodePolicy : DecodePolicy)
    (out : TestEvalOutcome) :
    PropertyFailsBool returnMode decodePolicy out = true ↔
      PropertyFails returnMode decodePolicy out := by
  cases returnMode <;> cases decodePolicy <;> cases out <;> simp [PropertyFailsBool, PropertyFails]
abbrev PropertyReturns (returnMode : ReturnMode) (out : TestEvalOutcome) : Prop :=
  match returnMode, out with
  | .bool, .returnsTrue => True
  | .bool, .returnsFalse => True
  | .void, .returnsVoid => True
  | _, _ => False

def proveTests (p : PlutusScript) (args : List Term) : Option Bool :=
  match RunProperty .bool p args with
  | .returnsTrue => some true
  | .returnsFalse => some false
  | _ => none

/-- For Void-returning tests: True iff the shared outcome classifier sees a successful halt. -/
def proveTestsHalt (p : PlutusScript) (args : List Term) : Prop :=
  PropertySucceeds .void (RunProperty .void p args)

/-- For Void+fail tests: True iff the shared outcome classifier sees an evaluation error. -/
def proveTestsError (p : PlutusScript) (args : List Term) : Prop :=
  PropertyFails .void .decodeErrorIsExportError (RunProperty .void p args)

def intArg (x : Integer) : List Term :=
  [Term.Const (Const.Data (Data.I x))]

def boolArg (x : Bool) : List Term :=
  [Term.Const (Const.Data (Data.Constr (if x then 1 else 0) []))]

def bytearrayArg (x : ByteString) : List Term :=
  [Term.Const (Const.Data (Data.B x))]

def dataArg (x : Data) : List Term :=
  [Term.Const (Const.Data x)]

def dataArgs2 (x : Data) (y : Data) : List Term :=
  [Term.Const (Const.Data x), Term.Const (Const.Data y)]

def dataArgs3 (x : Data) (y : Data) (z : Data) : List Term :=
  [Term.Const (Const.Data x), Term.Const (Const.Data y), Term.Const (Const.Data z)]

def stringArg (x : ByteString) : List Term :=
  [Term.Const (Const.Data (Data.B x))]

def pairArg (x : Data) (y : Data) : List Term :=
  [Term.Const (Const.Data (Data.List [x, y]))]

/-- `allProp p xs` holds iff every element of `xs` satisfies predicate `p`.
    Used in generated theorem preconditions to express per-element constraints
    (e.g. element range bounds) as a recursively-defined Prop rather than
    using `∀ x ∈ xs, ...` which desugars to `List.Mem` — an inductive predicate
    that Blaster's SMT backend cannot translate. -/
def allProp {{α : Type}} (p : α → Prop) : List α → Prop
  | [] => True
  | x :: xs => p x ∧ allProp p xs

end AikenVerify.Utils
"#
    )
}

fn generate_root_import(module_dirs: &BTreeMap<String, Vec<String>>) -> String {
    let mut lines = vec!["import AikenVerify.Utils".to_string()];

    for (module_segment, test_names) in module_dirs {
        for test_name in test_names {
            lines.push(format!(
                "import AikenVerify.Proofs.{module_segment}.{test_name}"
            ));
        }
    }

    lines.push(String::new()); // trailing newline
    lines.join("\n")
}

/// Check that a string is a valid integer literal (optional leading `-`, then digits).
fn is_valid_integer_literal(s: &str) -> bool {
    let s = s.strip_prefix('-').unwrap_or(s);
    !s.is_empty() && s.chars().all(|c| c.is_ascii_digit())
}

fn parse_integer_literal(s: &str) -> Option<BigInt> {
    if !is_valid_integer_literal(s) {
        return None;
    }
    s.parse::<BigInt>().ok()
}

fn validate_list_len_bounds(
    test_name: &str,
    min_len: Option<usize>,
    max_len: Option<usize>,
) -> miette::Result<()> {
    if let (Some(min), Some(max)) = (min_len, max_len)
        && min > max
    {
        return Err(generation_error(
            GenerationErrorCategory::InvalidConstraint,
            format!(
                "Test '{}' has inconsistent list-length bounds: min_len={} exceeds max_len={}",
                test_name, min, max,
            ),
        ));
    }
    Ok(())
}

fn validate_bytestring_len_bounds(
    test_name: &str,
    min_len: usize,
    max_len: usize,
) -> miette::Result<()> {
    if min_len > max_len {
        return Err(generation_error(
            GenerationErrorCategory::InvalidConstraint,
            format!(
                "Test '{}' has inconsistent byte-string length bounds: min_len={} exceeds max_len={}",
                test_name, min_len, max_len,
            ),
        ));
    }
    Ok(())
}

/// Convert a test_id to a valid Lean identifier for the UPLC program binding.
fn prog_name(test_id: &str) -> String {
    format!("prog_{test_id}")
}

fn fuzzer_prog_name(test_id: &str) -> String {
    format!("fuzzer_{}", prog_name(test_id))
}

fn tuple_var_name(mut index: usize) -> String {
    let mut chars = Vec::new();
    loop {
        let rem = (index % 26) as u8;
        chars.push((b'a' + rem) as char);
        if index < 26 {
            break;
        }
        index = (index / 26) - 1;
    }
    chars.into_iter().rev().collect()
}

fn tuple_var_names(arity: usize) -> Vec<String> {
    (0..arity).map(tuple_var_name).collect()
}

/// Map a FuzzerOutputType element to its Lean type name.
/// Returns None if the type cannot be mapped (truly unsupported).
fn lean_type_for(t: &FuzzerOutputType) -> Option<String> {
    match t {
        FuzzerOutputType::Int => Some("Integer".to_string()),
        FuzzerOutputType::Bool => Some("Bool".to_string()),
        FuzzerOutputType::ByteArray | FuzzerOutputType::String => Some("ByteString".to_string()),
        FuzzerOutputType::Data => Some("Data".to_string()),
        // Unsupported concrete types fall back to Data
        FuzzerOutputType::Unsupported(_) => Some("Data".to_string()),
        // Lists are representable as Lean `List` whenever the element type is representable.
        FuzzerOutputType::List(elem) => {
            let elem_ty = lean_type_for(elem)?;
            Some(format!("List {elem_ty}"))
        }
        // Pairs map to Lean `Prod`: (A × B)
        FuzzerOutputType::Pair(a, b) => {
            let a_ty = lean_type_for(a)?;
            let b_ty = lean_type_for(b)?;
            Some(format!("({a_ty} {CROSS} {b_ty})", CROSS = lean_sym::CROSS))
        }
        // Tuples map to right-associated Lean products: (A × B × C) = (A × (B × C))
        FuzzerOutputType::Tuple(types) => {
            if types.is_empty() {
                return Some("Unit".to_string());
            }
            if types.len() == 1 {
                return lean_type_for(&types[0]);
            }
            let parts: Vec<String> = types
                .iter()
                .map(lean_type_for)
                .collect::<Option<Vec<_>>>()?;
            Some(format!(
                "({})",
                parts.join(&format!(" {} ", lean_sym::CROSS))
            ))
        }
    }
}

/// Map a FuzzerOutputType element to its Data encoder expression.
/// `var` is the variable name to encode.
fn lean_data_encoder(t: &FuzzerOutputType, var: &str) -> Option<String> {
    fn encode_with_depth(t: &FuzzerOutputType, var: &str, depth: usize) -> Option<String> {
        match t {
            FuzzerOutputType::Int => Some(format!("Data.I {var}")),
            FuzzerOutputType::Bool => Some(format!("(Data.Constr (if {var} then 1 else 0) [])")),
            FuzzerOutputType::ByteArray | FuzzerOutputType::String => Some(format!("Data.B {var}")),
            FuzzerOutputType::Data | FuzzerOutputType::Unsupported(_) => Some(var.to_string()),
            FuzzerOutputType::List(elem) => {
                let elem_var = format!("x_{depth}");
                let elem_encoder = encode_with_depth(elem.as_ref(), &elem_var, depth + 1)?;
                Some(format!(
                    "Data.List ({var}.map (fun {elem_var} => {elem_encoder}))"
                ))
            }
            // Pairs encode as Data.List [fst_enc, snd_enc] (Aiken pair representation)
            FuzzerOutputType::Pair(a, b) => {
                let fst_var = format!("p_{depth}_1");
                let snd_var = format!("p_{depth}_2");
                let fst_enc = encode_with_depth(a.as_ref(), &fst_var, depth + 1)?;
                let snd_enc = encode_with_depth(b.as_ref(), &snd_var, depth + 1)?;
                Some(format!(
                    "(let ({fst_var}, {snd_var}) := {var}; Data.List [{fst_enc}, {snd_enc}])"
                ))
            }
            // Tuples encode as Data.List [elem_0_enc, ..., elem_n_enc]
            FuzzerOutputType::Tuple(types) => {
                if types.is_empty() {
                    return Some("Data.List []".to_string());
                }
                if types.len() == 1 {
                    return encode_with_depth(&types[0], var, depth);
                }
                let mut bindings = Vec::new();
                let mut encoded = Vec::new();
                for (i, ty) in types.iter().enumerate() {
                    let elem_var = format!("t_{depth}_{i}");
                    let enc = encode_with_depth(ty, &elem_var, depth + 1)?;
                    bindings.push(elem_var);
                    encoded.push(enc);
                }
                let pattern = bindings.join(", ");
                let items = encoded.join(", ");
                Some(format!("(let ({pattern}) := {var}; Data.List [{items}])"))
            }
        }
    }

    encode_with_depth(t, var, 0)
}

fn collect_type_dependencies(
    t: &FuzzerOutputType,
    has_integer: &mut bool,
    has_bytestring: &mut bool,
) {
    match t {
        FuzzerOutputType::Int => *has_integer = true,
        FuzzerOutputType::ByteArray | FuzzerOutputType::String => *has_bytestring = true,
        FuzzerOutputType::List(elem) => {
            collect_type_dependencies(elem.as_ref(), has_integer, has_bytestring);
        }
        FuzzerOutputType::Tuple(elems) => {
            for elem in elems {
                collect_type_dependencies(elem, has_integer, has_bytestring);
            }
        }
        FuzzerOutputType::Pair(fst, snd) => {
            collect_type_dependencies(fst.as_ref(), has_integer, has_bytestring);
            collect_type_dependencies(snd.as_ref(), has_integer, has_bytestring);
        }
        FuzzerOutputType::Bool | FuzzerOutputType::Data | FuzzerOutputType::Unsupported(_) => {}
    }
}

/// Return `true` when `--existential-mode witness` is sound for the given
/// fuzzer-output domain. `witness` mode synthesises a deterministic trivial
/// witness (`0` for `Int`, `[]` for `List`, `Data.I 0` for `Data`, …) and
/// proves the property body holds at that witness. For non-`Bool` domains the
/// trivial witness need not be a valid counterexample, making the resulting
/// existential proof unsound — every theorem becomes universally provable
/// regardless of what the program actually does.
///
/// Only `Bool` is sound today: a `Bool`-domain `fail_once` test asserts the
/// existence of an input that falsifies the property, and the trivial witness
/// (`true`) is always a candidate counterexample on the two-element domain.
/// The `false` branch is then exercised by the `decide` tactic if needed.
///
/// The match is intentionally exhaustive (no wildcard) so that adding a new
/// `FuzzerOutputType` variant forces a deliberate decision here rather than
/// silently inheriting an unsound default.
///
/// See `error_catalogue.rs` (`S0003`) and the H5 plan item.
fn witness_mode_sound_for(ty: &FuzzerOutputType) -> bool {
    match ty {
        FuzzerOutputType::Bool => true,
        FuzzerOutputType::Int
        | FuzzerOutputType::ByteArray
        | FuzzerOutputType::String
        | FuzzerOutputType::Data
        | FuzzerOutputType::List(_)
        | FuzzerOutputType::Tuple(_)
        | FuzzerOutputType::Pair(_, _)
        | FuzzerOutputType::Unsupported(_) => false,
    }
}

/// Render a `FuzzerOutputType` as a short, user-facing domain label for the
/// `S0003` `domain` field. Mirrors the style of the catalogue summary copy.
fn format_fuzzer_output_domain(ty: &FuzzerOutputType) -> String {
    match ty {
        FuzzerOutputType::Int => "Int".to_string(),
        FuzzerOutputType::Bool => "Bool".to_string(),
        FuzzerOutputType::ByteArray => "ByteArray".to_string(),
        FuzzerOutputType::String => "String".to_string(),
        FuzzerOutputType::Data => "Data".to_string(),
        FuzzerOutputType::List(elem) => format!("List<{}>", format_fuzzer_output_domain(elem)),
        FuzzerOutputType::Tuple(items) => {
            let parts: Vec<String> = items.iter().map(format_fuzzer_output_domain).collect();
            format!("({})", parts.join(", "))
        }
        FuzzerOutputType::Pair(a, b) => format!(
            "Pair<{}, {}>",
            format_fuzzer_output_domain(a),
            format_fuzzer_output_domain(b)
        ),
        FuzzerOutputType::Unsupported(repr) => repr.clone(),
    }
}

/// Return a default Lean witness expression for a given scalar type.
/// Used in existential witness mode to provide a concrete value for `⟨witness, by decide⟩`.
fn lean_default_witness(t: &FuzzerOutputType) -> String {
    match t {
        FuzzerOutputType::Int => "(0 : Integer)".to_string(),
        FuzzerOutputType::Bool => "true".to_string(),
        FuzzerOutputType::ByteArray | FuzzerOutputType::String => "ByteString.empty".to_string(),
        FuzzerOutputType::Data | FuzzerOutputType::Unsupported(_) => "Data.I 0".to_string(),
        FuzzerOutputType::List(_) => "[]".to_string(),
        FuzzerOutputType::Pair(a, b) => {
            format!("({}, {})", lean_default_witness(a), lean_default_witness(b))
        }
        FuzzerOutputType::Tuple(types) => {
            if types.is_empty() {
                return "()".to_string();
            }
            if types.len() == 1 {
                return lean_default_witness(&types[0]);
            }
            let parts: Vec<String> = types.iter().map(lean_default_witness).collect();
            format!("({})", parts.join(", "))
        }
    }
}

/// Collect Lean `open` statements needed for tuple/list proof generation.
/// Tuple/list arg encoding always constructs `Data.List` (and often other `Data.*`
/// constructors), so opening `PlutusCore.Data` is unconditional here.
fn lean_opens_for_types(types: &[&FuzzerOutputType]) -> String {
    let mut opens = vec!["open PlutusCore.Data (Data)"];
    let mut has_integer = false;
    let mut has_bytestring = false;

    for t in types {
        collect_type_dependencies(t, &mut has_integer, &mut has_bytestring);
    }
    if has_integer {
        opens.push("open PlutusCore.Integer (Integer)");
    }
    if has_bytestring {
        opens.push("open PlutusCore.ByteString (ByteString)");
    }

    opens.join("\n")
}

fn lean_bytestring_literal_from_bytes(bytes: &[u8]) -> String {
    if bytes.is_empty() {
        return "ByteString.empty".to_string();
    }
    bytes
        .iter()
        .rev()
        .fold("ByteString.empty".to_string(), |acc, b| {
            format!("PlutusCore.ByteString.consByteStringV1 {} ({acc})", b)
        })
}

fn lean_zero_bytestring_literal_of_len(len: usize) -> String {
    if len == 0 {
        return "ByteString.empty".to_string();
    }
    let mut acc = "ByteString.empty".to_string();
    for _ in 0..len {
        acc = format!("PlutusCore.ByteString.consByteStringV1 0 ({acc})");
    }
    acc
}

/// Render an integer literal as a Lean `Integer` (alias `Int`). Signs are
/// emitted as a `-` prefix, with negative literals parenthesized so they parse
/// cleanly inside larger expressions like `Data.I (-5)`.
fn lean_integer_literal_from_bigint(i: &BigInt) -> String {
    if i.sign() == num_bigint::Sign::Minus {
        format!("({})", i)
    } else {
        i.to_string()
    }
}

/// Convert a pallas `BigInt` (the Plutus-Data integer representation) into a
/// Rust `num_bigint::BigInt`. Mirrors the round-trip logic in `uplc::ast::Data`.
fn from_pallas_bigint(bi: &pallas_primitives::alonzo::BigInt) -> BigInt {
    use pallas_primitives::alonzo::BigInt as PBI;
    match bi {
        PBI::Int(i) => {
            // pallas's `Int` wraps an i128-sized value.
            let n: i128 = (*i).into();
            BigInt::from(n)
        }
        PBI::BigUInt(bytes) => BigInt::from_bytes_be(num_bigint::Sign::Plus, bytes.as_ref()),
        PBI::BigNInt(bytes) => BigInt::from_bytes_be(num_bigint::Sign::Minus, bytes.as_ref()),
    }
}

/// Serialize a Plutus `Data` value as a Lean `PlutusCore.Data.Data` literal.
///
/// This is the inverse of the UPLC -> CBOR round-trip: given a concrete
/// fuzzer-generated witness, produce a Lean term that `native_decide` can
/// evaluate structurally. Byte arrays lower through the existing
/// `consByteStringV1`-based encoder so the generated term uses only the
/// helpers already imported by the proof header (`PlutusCore.ByteString`
/// and `PlutusCore.Data`).
fn data_to_lean_literal(data: &PlutusData) -> String {
    match data {
        PlutusData::Constr(constr) => {
            // CBOR tags 121..=127 and 1280..=1400 are the compact encoding
            // for constructor ordinals 0..=127; tag 102 uses an explicit
            // `any_constructor` field for higher ordinals. Mirror
            // `uplc::pretty` by preferring the recovered constructor index.
            let tag = convert_tag_to_constr(constr.tag)
                .or(constr.any_constructor)
                .unwrap_or(constr.tag);
            let fields: Vec<String> = constr.fields.iter().map(data_to_lean_literal).collect();
            format!(
                "Data.Constr ({tag} : Integer) [{fields}]",
                fields = fields.join(", ")
            )
        }
        PlutusData::Map(entries) => {
            let pairs: Vec<String> = entries
                .iter()
                .map(|(k, v)| format!("({}, {})", data_to_lean_literal(k), data_to_lean_literal(v)))
                .collect();
            format!("Data.Map [{}]", pairs.join(", "))
        }
        PlutusData::Array(items) => {
            let items: Vec<String> = items.iter().map(data_to_lean_literal).collect();
            format!("Data.List [{}]", items.join(", "))
        }
        PlutusData::BigInt(bi) => {
            let n = from_pallas_bigint(bi);
            format!(
                "Data.I ({} : Integer)",
                lean_integer_literal_from_bigint(&n)
            )
        }
        PlutusData::BoundedBytes(bs) => {
            let lit = lean_bytestring_literal_from_bytes_qualified(&bs[..]);
            format!("Data.B ({lit})")
        }
    }
}

/// Fully-qualified ByteString literal emitter for contexts where the term
/// must be reducible by `native_decide` (Lean's kernel evaluator). Unlike
/// `lean_bytestring_literal_from_bytes`, which relies on the ambient
/// `open PlutusCore.ByteString (ByteString)` and the unqualified alias
/// `ByteString.empty` used in witness expressions for SMT backends, this
/// variant emits `PlutusCore.ByteString.emptyByteString` and
/// `PlutusCore.ByteString.consByteStringV1`, both of which are real
/// definitions the kernel can unfold. `native_decide` will fail with
/// `Unknown constant` on `ByteString.empty` because no such name is
/// declared — it is only emitted as a placeholder witness in tactic-based
/// proof obligations that never attempt to reduce it.
fn lean_bytestring_literal_from_bytes_qualified(bytes: &[u8]) -> String {
    if bytes.is_empty() {
        return "PlutusCore.ByteString.emptyByteString".to_string();
    }
    bytes.iter().rev().fold(
        "PlutusCore.ByteString.emptyByteString".to_string(),
        |acc, b| format!("PlutusCore.ByteString.consByteStringV1 {} ({acc})", b),
    )
}

#[cfg_attr(not(test), allow(dead_code))]
#[derive(Debug, Clone, PartialEq)]
enum BridgedAikenValue {
    Bool(bool),
    Int(BigInt),
    ByteArray(Vec<u8>),
    String(String),
    Data(PlutusData),
    List(Vec<BridgedAikenValue>),
    Tuple(Vec<BridgedAikenValue>),
    Constructor {
        tag: u64,
        fields: Vec<BridgedAikenValue>,
    },
}

#[derive(Debug, Clone)]
struct InputBridgeAssessment {
    input_type: InputValueBridge,
    obligations_open: Vec<DomainObligation>,
    obligations_discharged: Vec<DomainObligation>,
    widenings: Vec<Widening>,
    diagnostics: Vec<String>,
}

#[cfg_attr(not(test), allow(dead_code))]

impl InputTypeSchema {
    fn has_raw_data_fallback(&self) -> bool {
        match self {
            InputTypeSchema::RawDataFallback { .. } => true,
            InputTypeSchema::List { element } => element.has_raw_data_fallback(),
            InputTypeSchema::Pair { first, second } => {
                first.has_raw_data_fallback() || second.has_raw_data_fallback()
            }
            InputTypeSchema::Tuple { elements } => {
                elements.iter().any(InputTypeSchema::has_raw_data_fallback)
            }
            InputTypeSchema::ConstructorTagged { constructors, .. } => {
                constructors.iter().any(|ctor| {
                    ctor.fields
                        .iter()
                        .any(|field| field.schema.has_raw_data_fallback())
                })
            }
            InputTypeSchema::Bool
            | InputTypeSchema::Int
            | InputTypeSchema::ByteArray
            | InputTypeSchema::String
            | InputTypeSchema::Data => false,
        }
    }

    fn encode_plutus_data(&self, value: &BridgedAikenValue) -> Result<PlutusData, String> {
        match (self, value) {
            (InputTypeSchema::Bool, BridgedAikenValue::Bool(value)) => {
                Ok(plutus_constructor_data(u64::from(*value), Vec::new()))
            }
            (InputTypeSchema::Int, BridgedAikenValue::Int(value)) => {
                Ok(PlutusData::BigInt(to_pallas_bigint(value)))
            }
            (InputTypeSchema::ByteArray, BridgedAikenValue::ByteArray(value)) => {
                Ok(PlutusData::BoundedBytes(BoundedBytes::from(value.clone())))
            }
            (InputTypeSchema::String, BridgedAikenValue::String(value)) => Ok(
                PlutusData::BoundedBytes(BoundedBytes::from(value.as_bytes().to_vec())),
            ),
            (InputTypeSchema::Data, BridgedAikenValue::Data(value)) => Ok(value.clone()),
            (InputTypeSchema::List { element }, BridgedAikenValue::List(values)) => {
                Ok(PlutusData::Array(MaybeIndefArray::Def(
                    values
                        .iter()
                        .map(|item| element.encode_plutus_data(item))
                        .collect::<Result<Vec<_>, _>>()?,
                )))
            }
            (InputTypeSchema::Pair { first, second }, BridgedAikenValue::Tuple(values)) => {
                if values.len() != 2 {
                    return Err(format!(
                        "expected pair bridge value with 2 items, got {}",
                        values.len()
                    ));
                }
                Ok(PlutusData::Array(MaybeIndefArray::Def(vec![
                    first.encode_plutus_data(&values[0])?,
                    second.encode_plutus_data(&values[1])?,
                ])))
            }
            (InputTypeSchema::Tuple { elements }, BridgedAikenValue::Tuple(values)) => {
                if elements.len() == 1 {
                    if values.len() != 1 {
                        return Err(format!(
                            "expected single-element tuple bridge value, got {} item(s)",
                            values.len()
                        ));
                    }
                    return elements[0].encode_plutus_data(&values[0]);
                }
                if elements.len() != values.len() {
                    return Err(format!(
                        "expected tuple bridge value with {} items, got {}",
                        elements.len(),
                        values.len()
                    ));
                }
                Ok(PlutusData::Array(MaybeIndefArray::Def(
                    elements
                        .iter()
                        .zip(values.iter())
                        .map(|(schema, value)| schema.encode_plutus_data(value))
                        .collect::<Result<Vec<_>, _>>()?,
                )))
            }
            (
                InputTypeSchema::ConstructorTagged { constructors, .. },
                BridgedAikenValue::Constructor { tag, fields },
            ) => {
                let constructor = constructors
                    .iter()
                    .find(|candidate| candidate.tag == *tag)
                    .ok_or_else(|| format!("unknown constructor tag {tag}"))?;
                if constructor.fields.len() != fields.len() {
                    return Err(format!(
                        "constructor tag {tag} expects {} field(s), got {}",
                        constructor.fields.len(),
                        fields.len()
                    ));
                }
                Ok(plutus_constructor_data(
                    *tag,
                    constructor
                        .fields
                        .iter()
                        .zip(fields.iter())
                        .map(|(field, value)| field.schema.encode_plutus_data(value))
                        .collect::<Result<Vec<_>, _>>()?,
                ))
            }
            (InputTypeSchema::RawDataFallback { .. }, BridgedAikenValue::Data(value)) => {
                Ok(value.clone())
            }
            _ => Err(format!(
                "value {value:?} does not match input schema {self:?}"
            )),
        }
    }

    fn decode_plutus_data(&self, data: &PlutusData) -> Result<BridgedAikenValue, String> {
        match self {
            InputTypeSchema::Bool => match data {
                PlutusData::Constr(constr) => {
                    let tag = recover_constructor_tag(constr);
                    if !constr.fields.is_empty() {
                        return Err("bool bridge requires a nullary constructor".to_string());
                    }
                    match tag {
                        0 => Ok(BridgedAikenValue::Bool(false)),
                        1 => Ok(BridgedAikenValue::Bool(true)),
                        _ => Err(format!("unexpected bool constructor tag {tag}")),
                    }
                }
                _ => Err("bool bridge expected constructor-tagged Data".to_string()),
            },
            InputTypeSchema::Int => match data {
                PlutusData::BigInt(value) => Ok(BridgedAikenValue::Int(from_pallas_bigint(value))),
                _ => Err("int bridge expected Data integer".to_string()),
            },
            InputTypeSchema::ByteArray => match data {
                PlutusData::BoundedBytes(bytes) => {
                    let bytes: Vec<u8> = bytes.clone().into();
                    Ok(BridgedAikenValue::ByteArray(bytes))
                }
                _ => Err("bytearray bridge expected Data bytes".to_string()),
            },
            InputTypeSchema::String => match data {
                PlutusData::BoundedBytes(bytes) => {
                    let bytes: Vec<u8> = bytes.clone().into();
                    String::from_utf8(bytes)
                        .map(BridgedAikenValue::String)
                        .map_err(|error| format!("string bridge expected UTF-8 bytes: {error}"))
                }
                _ => Err("string bridge expected Data bytes".to_string()),
            },
            InputTypeSchema::Data => Ok(BridgedAikenValue::Data(data.clone())),
            InputTypeSchema::List { element } => match data {
                PlutusData::Array(items) => items
                    .iter()
                    .map(|item| element.decode_plutus_data(item))
                    .collect::<Result<Vec<_>, _>>()
                    .map(BridgedAikenValue::List),
                _ => Err("list bridge expected Data list".to_string()),
            },
            InputTypeSchema::Pair { first, second } => match data {
                PlutusData::Array(items) => {
                    let values = items.iter().collect::<Vec<_>>();
                    if values.len() != 2 {
                        return Err(format!(
                            "pair bridge expected Data list with 2 items, got {}",
                            values.len()
                        ));
                    }
                    Ok(BridgedAikenValue::Tuple(vec![
                        first.decode_plutus_data(values[0])?,
                        second.decode_plutus_data(values[1])?,
                    ]))
                }
                _ => Err("pair bridge expected Data list".to_string()),
            },
            InputTypeSchema::Tuple { elements } => {
                if elements.len() == 1 {
                    return elements[0]
                        .decode_plutus_data(data)
                        .map(|value| BridgedAikenValue::Tuple(vec![value]));
                }
                match data {
                    PlutusData::Array(items) => {
                        let values = items.iter().collect::<Vec<_>>();
                        if values.len() != elements.len() {
                            return Err(format!(
                                "tuple bridge expected Data list with {} items, got {}",
                                elements.len(),
                                values.len()
                            ));
                        }
                        elements
                            .iter()
                            .zip(values.iter())
                            .map(|(schema, item)| schema.decode_plutus_data(item))
                            .collect::<Result<Vec<_>, _>>()
                            .map(BridgedAikenValue::Tuple)
                    }
                    _ => Err("tuple bridge expected Data list".to_string()),
                }
            }
            InputTypeSchema::ConstructorTagged { constructors, .. } => match data {
                PlutusData::Constr(constr) => {
                    let tag = recover_constructor_tag(constr);
                    let constructor = constructors
                        .iter()
                        .find(|candidate| candidate.tag == tag)
                        .ok_or_else(|| format!("unexpected constructor tag {tag}"))?;
                    let fields = constr.fields.iter().collect::<Vec<_>>();
                    if fields.len() != constructor.fields.len() {
                        return Err(format!(
                            "constructor tag {tag} expected {} field(s), got {}",
                            constructor.fields.len(),
                            fields.len()
                        ));
                    }
                    constructor
                        .fields
                        .iter()
                        .zip(fields.iter())
                        .map(|(field, item)| field.schema.decode_plutus_data(item))
                        .collect::<Result<Vec<_>, _>>()
                        .map(|fields| BridgedAikenValue::Constructor { tag, fields })
                }
                _ => Err("constructor bridge expected constructor-tagged Data".to_string()),
            },
            InputTypeSchema::RawDataFallback { .. } => Ok(BridgedAikenValue::Data(data.clone())),
        }
    }

    fn encode_runtime_value(&self, value: &BridgedAikenValue) -> Result<UplcRuntimeValue, String> {
        match (self, value) {
            (InputTypeSchema::Bool, BridgedAikenValue::Bool(value)) => {
                Ok(UplcRuntimeValue::bool(*value))
            }
            (InputTypeSchema::Int, BridgedAikenValue::Int(value)) => {
                Ok(UplcRuntimeValue::integer(value.clone()))
            }
            (InputTypeSchema::ByteArray, BridgedAikenValue::ByteArray(value)) => {
                Ok(UplcRuntimeValue::byte_string(value.clone()))
            }
            (InputTypeSchema::String, BridgedAikenValue::String(value)) => {
                Ok(UplcRuntimeValue::string(value.clone()))
            }
            (InputTypeSchema::Data, BridgedAikenValue::Data(value)) => {
                Ok(UplcRuntimeValue::data(value.clone()))
            }
            (
                InputTypeSchema::ConstructorTagged { constructors, .. },
                BridgedAikenValue::Constructor { tag, fields },
            ) => {
                let constructor = constructors
                    .iter()
                    .find(|candidate| candidate.tag == *tag)
                    .ok_or_else(|| format!("unknown constructor tag {tag}"))?;
                if constructor.fields.len() != fields.len() {
                    return Err(format!(
                        "constructor tag {tag} expects {} field(s), got {}",
                        constructor.fields.len(),
                        fields.len()
                    ));
                }
                Ok(UplcRuntimeValue::Constr {
                    tag: *tag as usize,
                    fields: constructor
                        .fields
                        .iter()
                        .zip(fields.iter())
                        .map(|(field, value)| field.schema.encode_runtime_value(value))
                        .collect::<Result<Vec<_>, _>>()?,
                })
            }
            _ => self.encode_plutus_data(value).map(UplcRuntimeValue::data),
        }
    }

    fn decode_runtime_value(&self, value: &UplcRuntimeValue) -> Result<BridgedAikenValue, String> {
        match self {
            InputTypeSchema::Bool => match value {
                UplcRuntimeValue::Con(constant) => match constant.as_ref() {
                    uplc::ast::Constant::Bool(value) => Ok(BridgedAikenValue::Bool(*value)),
                    uplc::ast::Constant::Data(data) => self.decode_plutus_data(data),
                    other => Err(format!(
                        "bool bridge expected Bool/Data constant, got {other:?}"
                    )),
                },
                other => Err(format!(
                    "bool bridge expected constant runtime value, got {other:?}"
                )),
            },
            InputTypeSchema::Int => match value {
                UplcRuntimeValue::Con(constant) => match constant.as_ref() {
                    uplc::ast::Constant::Integer(value) => {
                        Ok(BridgedAikenValue::Int(value.clone()))
                    }
                    uplc::ast::Constant::Data(data) => self.decode_plutus_data(data),
                    other => Err(format!(
                        "int bridge expected Integer/Data constant, got {other:?}"
                    )),
                },
                other => Err(format!(
                    "int bridge expected constant runtime value, got {other:?}"
                )),
            },
            InputTypeSchema::ByteArray => match value {
                UplcRuntimeValue::Con(constant) => match constant.as_ref() {
                    uplc::ast::Constant::ByteString(value) => {
                        Ok(BridgedAikenValue::ByteArray(value.clone()))
                    }
                    uplc::ast::Constant::Data(data) => self.decode_plutus_data(data),
                    other => Err(format!(
                        "bytearray bridge expected ByteString/Data constant, got {other:?}",
                    )),
                },
                other => Err(format!(
                    "bytearray bridge expected constant runtime value, got {other:?}",
                )),
            },
            InputTypeSchema::String => match value {
                UplcRuntimeValue::Con(constant) => match constant.as_ref() {
                    uplc::ast::Constant::String(value) => {
                        Ok(BridgedAikenValue::String(value.clone()))
                    }
                    uplc::ast::Constant::Data(data) => self.decode_plutus_data(data),
                    other => Err(format!(
                        "string bridge expected String/Data constant, got {other:?}"
                    )),
                },
                other => Err(format!(
                    "string bridge expected constant runtime value, got {other:?}"
                )),
            },
            InputTypeSchema::Data => match value {
                UplcRuntimeValue::Con(constant) => match constant.as_ref() {
                    uplc::ast::Constant::Data(data) => Ok(BridgedAikenValue::Data(data.clone())),
                    other => Err(format!("data bridge expected Data constant, got {other:?}")),
                },
                other => Err(format!(
                    "data bridge expected constant runtime value, got {other:?}"
                )),
            },
            InputTypeSchema::ConstructorTagged { constructors, .. } => match value {
                UplcRuntimeValue::Constr { tag, fields } => {
                    let constructor = constructors
                        .iter()
                        .find(|candidate| candidate.tag == *tag as u64)
                        .ok_or_else(|| format!("unexpected runtime constructor tag {tag}"))?;
                    if constructor.fields.len() != fields.len() {
                        return Err(format!(
                            "runtime constructor tag {tag} expected {} field(s), got {}",
                            constructor.fields.len(),
                            fields.len()
                        ));
                    }
                    constructor
                        .fields
                        .iter()
                        .zip(fields.iter())
                        .map(|(field, item)| field.schema.decode_runtime_value(item))
                        .collect::<Result<Vec<_>, _>>()
                        .map(|fields| BridgedAikenValue::Constructor {
                            tag: *tag as u64,
                            fields,
                        })
                }
                UplcRuntimeValue::Con(constant) => match constant.as_ref() {
                    uplc::ast::Constant::Data(data) => self.decode_plutus_data(data),
                    other => Err(format!(
                        "constructor bridge expected constructor/Data constant, got {other:?}",
                    )),
                },
                other => Err(format!(
                    "constructor bridge expected constructor/Data runtime value, got {other:?}",
                )),
            },
            InputTypeSchema::List { .. }
            | InputTypeSchema::Pair { .. }
            | InputTypeSchema::Tuple { .. }
            | InputTypeSchema::RawDataFallback { .. } => match value {
                UplcRuntimeValue::Con(constant) => match constant.as_ref() {
                    uplc::ast::Constant::Data(data) => self.decode_plutus_data(data),
                    other => Err(format!(
                        "composite bridge expected Data constant, got {other:?}"
                    )),
                },
                other => Err(format!(
                    "composite bridge expected constant runtime value, got {other:?}"
                )),
            },
        }
    }

    fn to_lean_literal(&self, value: &BridgedAikenValue) -> Result<String, String> {
        match (self, value) {
            (InputTypeSchema::Bool, BridgedAikenValue::Bool(value)) => Ok(value.to_string()),
            (InputTypeSchema::Int, BridgedAikenValue::Int(value)) => {
                Ok(lean_integer_literal_from_bigint(value))
            }
            (InputTypeSchema::ByteArray, BridgedAikenValue::ByteArray(value)) => {
                Ok(lean_bytestring_literal_from_bytes_qualified(value))
            }
            (InputTypeSchema::String, BridgedAikenValue::String(value)) => Ok(
                lean_bytestring_literal_from_bytes_qualified(value.as_bytes()),
            ),
            (InputTypeSchema::Data, BridgedAikenValue::Data(value)) => {
                Ok(data_to_lean_literal(value))
            }
            (InputTypeSchema::List { element }, BridgedAikenValue::List(values)) => values
                .iter()
                .map(|value| element.to_lean_literal(value))
                .collect::<Result<Vec<_>, _>>()
                .map(|items| format!("[{}]", items.join(", "))),
            (InputTypeSchema::Pair { first, second }, BridgedAikenValue::Tuple(values)) => {
                if values.len() != 2 {
                    return Err(format!(
                        "expected pair bridge value with 2 items, got {}",
                        values.len()
                    ));
                }
                Ok(format!(
                    "({}, {})",
                    first.to_lean_literal(&values[0])?,
                    second.to_lean_literal(&values[1])?
                ))
            }
            (InputTypeSchema::Tuple { elements }, BridgedAikenValue::Tuple(values)) => {
                if elements.len() == 1 {
                    if values.len() != 1 {
                        return Err(format!(
                            "expected single-element tuple bridge value, got {} item(s)",
                            values.len()
                        ));
                    }
                    return elements[0].to_lean_literal(&values[0]);
                }
                if elements.len() != values.len() {
                    return Err(format!(
                        "expected tuple bridge value with {} items, got {}",
                        elements.len(),
                        values.len()
                    ));
                }
                let literals = elements
                    .iter()
                    .zip(values.iter())
                    .map(|(schema, value)| schema.to_lean_literal(value))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(lean_tuple_literal(&literals))
            }
            (InputTypeSchema::ConstructorTagged { .. }, BridgedAikenValue::Constructor { .. })
            | (InputTypeSchema::RawDataFallback { .. }, BridgedAikenValue::Data(_)) => self
                .encode_plutus_data(value)
                .map(|data| data_to_lean_literal(&data)),
            _ => Err(format!(
                "value {value:?} does not match input schema {self:?}"
            )),
        }
    }
}

#[cfg_attr(not(test), allow(dead_code))]

impl InputValueBridge {
    fn encode_plutus_data(&self, value: &BridgedAikenValue) -> Result<PlutusData, String> {
        let schema = self
            .schema
            .as_ref()
            .ok_or_else(|| "input bridge is missing schema metadata".to_string())?;
        schema.encode_plutus_data(value)
    }

    fn decode_plutus_data(&self, data: &PlutusData) -> Result<BridgedAikenValue, String> {
        let schema = self
            .schema
            .as_ref()
            .ok_or_else(|| "input bridge is missing schema metadata".to_string())?;
        schema.decode_plutus_data(data)
    }

    fn encode_runtime_value(&self, value: &BridgedAikenValue) -> Result<UplcRuntimeValue, String> {
        let schema = self
            .schema
            .as_ref()
            .ok_or_else(|| "input bridge is missing schema metadata".to_string())?;
        schema.encode_runtime_value(value)
    }

    fn decode_runtime_value(&self, value: &UplcRuntimeValue) -> Result<BridgedAikenValue, String> {
        let schema = self
            .schema
            .as_ref()
            .ok_or_else(|| "input bridge is missing schema metadata".to_string())?;
        schema.decode_runtime_value(value)
    }

    fn to_lean_literal(&self, value: &BridgedAikenValue) -> Result<String, String> {
        let schema = self
            .schema
            .as_ref()
            .ok_or_else(|| "input bridge is missing schema metadata".to_string())?;
        schema.to_lean_literal(value)
    }
}

#[cfg_attr(not(test), allow(dead_code))]

fn plutus_constructor_data(tag: u64, fields: Vec<PlutusData>) -> PlutusData {
    let compact_tag = convert_constr_to_tag(tag).unwrap_or(102);
    PlutusData::Constr(Constr {
        tag: compact_tag,
        any_constructor: (compact_tag == 102).then_some(tag),
        fields: MaybeIndefArray::Def(fields),
    })
}

#[cfg_attr(not(test), allow(dead_code))]

fn recover_constructor_tag(constr: &Constr<PlutusData>) -> u64 {
    convert_tag_to_constr(constr.tag)
        .or(constr.any_constructor)
        .unwrap_or(constr.tag)
}

#[cfg_attr(not(test), allow(dead_code))]

fn lean_tuple_literal(items: &[String]) -> String {
    match items {
        [] => "()".to_string(),
        [single] => single.clone(),
        [first, second] => format!("({first}, {second})"),
        [first, rest @ ..] => format!("({first}, {})", lean_tuple_literal(rest)),
    }
}

fn schema_hash_for_input_schema(schema: &InputTypeSchema) -> Option<String> {
    if schema.has_raw_data_fallback() {
        return None;
    }

    serde_json::to_vec(schema)
        .ok()
        .map(|bytes| blake2b_256_hex(&bytes))
}

fn collect_input_schema_diagnostics(schema: &InputTypeSchema, diagnostics: &mut Vec<String>) {
    match schema {
        InputTypeSchema::RawDataFallback { type_name, reason } => diagnostics.push(format!(
            "Value bridge fell back to raw Data for '{type_name}': {reason}",
        )),
        InputTypeSchema::List { element } => {
            collect_input_schema_diagnostics(element, diagnostics);
        }
        InputTypeSchema::Pair { first, second } => {
            collect_input_schema_diagnostics(first, diagnostics);
            collect_input_schema_diagnostics(second, diagnostics);
        }
        InputTypeSchema::Tuple { elements } => {
            for element in elements {
                collect_input_schema_diagnostics(element, diagnostics);
            }
        }
        InputTypeSchema::ConstructorTagged { constructors, .. } => {
            for constructor in constructors {
                for field in &constructor.fields {
                    collect_input_schema_diagnostics(&field.schema, diagnostics);
                }
            }
        }
        InputTypeSchema::Bool
        | InputTypeSchema::Int
        | InputTypeSchema::ByteArray
        | InputTypeSchema::String
        | InputTypeSchema::Data => {}
    }
}

fn bridge_aiken_type_name(test: &ExportedPropertyTest) -> String {
    match &test.fuzzer_output_type {
        FuzzerOutputType::Unsupported(raw) => raw.clone(),
        other => other.to_string(),
    }
}

fn runtime_value_encoding_for_schema(schema: &InputTypeSchema) -> RuntimeValueEncoding {
    match schema {
        InputTypeSchema::Bool => RuntimeValueEncoding::BoolConstant,
        InputTypeSchema::Int => RuntimeValueEncoding::IntegerConstant,
        InputTypeSchema::ByteArray => RuntimeValueEncoding::ByteStringConstant,
        InputTypeSchema::String => RuntimeValueEncoding::StringConstant,
        InputTypeSchema::ConstructorTagged { .. } => RuntimeValueEncoding::ConstructorValue,
        InputTypeSchema::Data
        | InputTypeSchema::List { .. }
        | InputTypeSchema::Pair { .. }
        | InputTypeSchema::Tuple { .. }
        | InputTypeSchema::RawDataFallback { .. } => RuntimeValueEncoding::DataConstant,
    }
}

fn plutus_data_encoding_for_schema(schema: &InputTypeSchema) -> PlutusDataEncoding {
    match schema {
        InputTypeSchema::Bool => PlutusDataEncoding::BoolConstructor,
        InputTypeSchema::Int => PlutusDataEncoding::Integer,
        InputTypeSchema::ByteArray => PlutusDataEncoding::Bytes,
        InputTypeSchema::String => PlutusDataEncoding::Utf8Bytes,
        InputTypeSchema::Data | InputTypeSchema::RawDataFallback { .. } => {
            PlutusDataEncoding::RawData
        }
        InputTypeSchema::List { .. } => PlutusDataEncoding::List,
        InputTypeSchema::Pair { .. } => PlutusDataEncoding::PairAsList,
        InputTypeSchema::Tuple { .. } => PlutusDataEncoding::TupleAsList,
        InputTypeSchema::ConstructorTagged { .. } => PlutusDataEncoding::ConstructorTagged,
    }
}

fn lean_value_encoding_for_schema(schema: &InputTypeSchema) -> LeanValueEncoding {
    match schema {
        InputTypeSchema::Bool => LeanValueEncoding::Bool,
        InputTypeSchema::Int => LeanValueEncoding::Integer,
        InputTypeSchema::ByteArray | InputTypeSchema::String => LeanValueEncoding::ByteString,
        InputTypeSchema::Data
        | InputTypeSchema::ConstructorTagged { .. }
        | InputTypeSchema::RawDataFallback { .. } => LeanValueEncoding::Data,
        InputTypeSchema::List { .. } => LeanValueEncoding::List,
        InputTypeSchema::Pair { .. } | InputTypeSchema::Tuple { .. } => LeanValueEncoding::Tuple,
    }
}

fn input_type_schema_for_test(
    test: &ExportedPropertyTest,
    output_type: &FuzzerOutputType,
    top_level: bool,
) -> InputTypeSchema {
    match output_type {
        FuzzerOutputType::Bool => InputTypeSchema::Bool,
        FuzzerOutputType::Int => InputTypeSchema::Int,
        FuzzerOutputType::ByteArray => InputTypeSchema::ByteArray,
        FuzzerOutputType::String => InputTypeSchema::String,
        FuzzerOutputType::Data => InputTypeSchema::Data,
        FuzzerOutputType::List(element) => InputTypeSchema::List {
            element: Box::new(input_type_schema_for_test(test, element.as_ref(), false)),
        },
        FuzzerOutputType::Pair(first, second) => InputTypeSchema::Pair {
            first: Box::new(input_type_schema_for_test(test, first.as_ref(), false)),
            second: Box::new(input_type_schema_for_test(test, second.as_ref(), false)),
        },
        FuzzerOutputType::Tuple(elements) => InputTypeSchema::Tuple {
            elements: elements
                .iter()
                .map(|element| input_type_schema_for_test(test, element, false))
                .collect(),
        },
        FuzzerOutputType::Unsupported(type_name) => {
            let schema = schema_type_name_candidates(type_name)
                .iter()
                .find_map(|candidate| test.inner_data_schemas.get(candidate))
                .or_else(|| {
                    top_level
                        .then_some(test.fuzzer_data_schema.as_ref())
                        .flatten()
                });
            match schema {
                Some(schema) => exported_data_schema_to_input_schema(type_name, schema)
                    .unwrap_or_else(|reason| InputTypeSchema::RawDataFallback {
                        type_name: type_name.clone(),
                        reason,
                    }),
                None => InputTypeSchema::RawDataFallback {
                    type_name: type_name.clone(),
                    reason: format!("no exported schema is available for '{type_name}'"),
                },
            }
        }
    }
}

fn exported_data_schema_to_input_schema(
    type_name: &str,
    schema: &ExportedDataSchema,
) -> Result<InputTypeSchema, String> {
    fn resolve_schema_data<'a>(
        schema: &'a ExportedDataSchema,
        reference: &Reference,
    ) -> Result<&'a SchemaData, String> {
        let definition = schema
            .definitions
            .lookup(reference)
            .ok_or_else(|| format!("missing schema definition for {}", reference.as_key()))?;
        match &definition.annotated {
            Schema::Data(data) => Ok(data),
            other => Err(format!(
                "expected data schema for {}, got {other:?}",
                reference.as_key()
            )),
        }
    }

    fn declaration_to_input_schema(
        type_name: &str,
        schema: &ExportedDataSchema,
        declaration: &SchemaDeclaration<SchemaData>,
        seen: &mut Vec<String>,
    ) -> Result<InputTypeSchema, String> {
        match declaration {
            SchemaDeclaration::Inline(data) => {
                data_to_input_schema(type_name, schema, data.as_ref(), seen)
            }
            SchemaDeclaration::Referenced(reference) => {
                let key = reference.as_key();
                if seen.contains(&key) {
                    return Err(format!(
                        "recursive schema reference '{key}' is not yet supported by the initial decoder bridge",
                    ));
                }
                seen.push(key.clone());
                let data = resolve_schema_data(schema, reference)?;
                let lowered = data_to_input_schema(type_name, schema, data, seen);
                seen.pop();
                lowered
            }
        }
    }

    fn data_to_input_schema(
        type_name: &str,
        schema: &ExportedDataSchema,
        data: &SchemaData,
        seen: &mut Vec<String>,
    ) -> Result<InputTypeSchema, String> {
        match data {
            SchemaData::Integer => Ok(InputTypeSchema::Int),
            SchemaData::Bytes => Ok(InputTypeSchema::ByteArray),
            SchemaData::List(SchemaItems::One(declaration)) => Ok(InputTypeSchema::List {
                element: Box::new(declaration_to_input_schema(
                    type_name,
                    schema,
                    declaration,
                    seen,
                )?),
            }),
            SchemaData::List(SchemaItems::Many(items)) => Ok(InputTypeSchema::Tuple {
                elements: items
                    .iter()
                    .map(|item| {
                        declaration_to_input_schema(type_name, schema, &item.annotated, seen)
                    })
                    .collect::<Result<Vec<_>, _>>()?,
            }),
            SchemaData::AnyOf(constructors) => Ok(InputTypeSchema::ConstructorTagged {
                type_name: type_name.to_string(),
                constructors: constructors
                    .iter()
                    .map(|constructor| {
                        Ok(InputTypeConstructor {
                            name: constructor.title.clone(),
                            tag: constructor.annotated.index as u64,
                            fields: constructor
                                .annotated
                                .fields
                                .iter()
                                .map(|field| {
                                    Ok(InputTypeField {
                                        name: field.title.clone(),
                                        schema: declaration_to_input_schema(
                                            type_name,
                                            schema,
                                            &field.annotated,
                                            seen,
                                        )?,
                                    })
                                })
                                .collect::<Result<Vec<_>, String>>()?,
                        })
                    })
                    .collect::<Result<Vec<_>, String>>()?,
            }),
            SchemaData::Opaque => Err(format!(
                "schema for '{type_name}' is opaque and cannot yet be round-tripped",
            )),
            SchemaData::Map(_, _) => Err(format!(
                "schema for '{type_name}' contains maps, which the initial decoder bridge does not yet support",
            )),
        }
    }

    let mut seen = vec![schema.root.as_key()];
    let root = resolve_schema_data(schema, &schema.root)?;
    data_to_input_schema(type_name, schema, root, &mut seen)
}

fn input_value_bridge_for_test(test: &ExportedPropertyTest) -> InputBridgeAssessment {
    let aiken_type = bridge_aiken_type_name(test);
    let schema = input_type_schema_for_test(test, &test.fuzzer_output_type, true);
    let mut diagnostics = Vec::new();
    collect_input_schema_diagnostics(&schema, &mut diagnostics);

    let runtime_encoding = runtime_value_encoding_for_schema(&schema);
    let data_encoding = Some(plutus_data_encoding_for_schema(&schema));
    let lean_encoding = lean_value_encoding_for_schema(&schema);
    let schema_hash = schema_hash_for_input_schema(&schema);
    let has_raw_fallback = schema.has_raw_data_fallback();
    let harness_supported = lean_type_for(&test.fuzzer_output_type).is_some()
        && lean_data_encoder(&test.fuzzer_output_type, "bridge_value").is_some();

    let mut obligations_open = Vec::new();
    let mut obligations_discharged = Vec::new();

    if has_raw_fallback {
        obligations_open.push(DomainObligation::ValueDecoderRoundTrip);
        obligations_open.push(DomainObligation::FuzzerOutputTypeMatchesPropertyInputType);
        obligations_open.push(DomainObligation::PropertyHarnessAcceptsDecodedInput);
    } else {
        obligations_discharged.push(DomainObligation::ValueDecoderRoundTrip);
        obligations_discharged.push(DomainObligation::FuzzerOutputTypeMatchesPropertyInputType);
        if harness_supported {
            obligations_discharged.push(DomainObligation::PropertyHarnessAcceptsDecodedInput);
        } else {
            obligations_open.push(DomainObligation::PropertyHarnessAcceptsDecodedInput);
            diagnostics.push(format!(
                "Current bridge cannot show that the property harness accepts decoded '{}' inputs.",
                aiken_type
            ));
        }
    }

    let widenings = diagnostics
        .iter()
        .map(|message| Widening {
            kind: "value_decoder".to_string(),
            message: message.clone(),
            allowed_only_under: None,
        })
        .collect();

    InputBridgeAssessment {
        input_type: InputValueBridge {
            aiken_type,
            runtime_encoding,
            data_encoding,
            lean_encoding,
            schema_hash,
            schema: Some(schema),
            diagnostics: diagnostics.clone(),
        },
        obligations_open,
        obligations_discharged,
        widenings,
        diagnostics,
    }
}

fn exact_value_to_scalar_literal(
    output_type: &FuzzerOutputType,
    value: &FuzzerExactValue,
) -> Option<String> {
    match (output_type, value) {
        (FuzzerOutputType::Bool, FuzzerExactValue::Bool(v)) => Some(v.to_string()),
        (FuzzerOutputType::ByteArray, FuzzerExactValue::ByteArray(bytes)) => {
            Some(lean_bytestring_literal_from_bytes(bytes))
        }
        (FuzzerOutputType::String, FuzzerExactValue::String(s)) => {
            Some(lean_bytestring_literal_from_bytes(s.as_bytes()))
        }
        _ => None,
    }
}

fn data_constructor_tag_literal(tag: u64) -> String {
    format!("Data.Constr ({tag} : Integer) []")
}

fn data_constructor_tags_predicate(var: &str, tags: &[u64]) -> Option<String> {
    let mut unique_tags: Vec<u64> = tags.to_vec();
    unique_tags.sort_unstable();
    unique_tags.dedup();

    if unique_tags.is_empty() {
        return None;
    }

    // Use match expression with wildcard for fields, since constructors may not be nullary.
    // The tag is annotated with Integer to prevent Nat inference.
    let arms: Vec<String> = unique_tags
        .iter()
        .map(|tag| format!("| Data.Constr {tag} _ => True"))
        .collect();
    Some(format!(
        "(match {var} with\n    {}\n    | _ => False)",
        arms.join("\n    ")
    ))
}

fn lean_name_fragment(raw: &str) -> String {
    let sanitized: String = raw
        .chars()
        .map(|c| if c.is_ascii_alphanumeric() { c } else { '_' })
        .collect();
    let sanitized = sanitized.trim_matches('_');
    if sanitized.is_empty() {
        "shape".to_string()
    } else {
        sanitize_lean_name(sanitized)
    }
}

fn schema_predicate_name(prefix: &str, raw: &str) -> String {
    format!("{prefix}_{}_{}", lean_name_fragment(raw), short_hash(raw))
}

fn lean_prop_conjunction(parts: &[String]) -> String {
    match parts {
        [] => "True".to_string(),
        [single] => single.clone(),
        _ => {
            let wrapped = parts
                .iter()
                .map(|part| format!("({part})"))
                .collect::<Vec<_>>();
            format!("({})", wrapped.join(&format!(" {} ", lean_sym::AND)))
        }
    }
}

fn exported_data_schema_for_reference<'a>(
    test_name: &str,
    schema: &'a ExportedDataSchema,
    fallback_schemas: Option<&'a BTreeMap<String, ExportedDataSchema>>,
    reference: &Reference,
) -> miette::Result<(&'a ExportedDataSchema, &'a SchemaData)> {
    let key = reference.as_key();
    let resolved = schema
        .definitions
        .lookup(reference)
        .map(|definition| (schema, definition))
        .or_else(|| {
            fallback_schemas.and_then(|schemas| {
                schemas
                    .get(&key)
                    .and_then(|schema| {
                        schema
                            .definitions
                            .lookup(reference)
                            .map(|definition| (schema, definition))
                    })
                    .or_else(|| {
                        schemas.values().find_map(|schema| {
                            schema
                                .definitions
                                .lookup(reference)
                                .map(|definition| (schema, definition))
                        })
                    })
            })
        })
        .ok_or_else(|| {
            unsupported_error(
                "E0025",
                UnsupportedReason::MissingFuzzerSchema {
                    test_name: test_name.to_string(),
                },
            )
        })?;

    match &resolved.1.annotated {
        Schema::Data(data) => Ok((resolved.0, data)),
        _ => Err(unsupported_error(
            "E0026",
            UnsupportedReason::NonDataFuzzerSchema {
                test_name: test_name.to_string(),
            },
        )),
    }
}

#[derive(Default)]
struct LeanDataShapeBuilder {
    generated_refs: BTreeMap<String, String>,
    visiting_refs: BTreeSet<String>,
    recursive_refs: BTreeSet<String>,
    definitions: Vec<String>,
}

fn ensure_exported_schema_reference_predicate(
    builder: &mut LeanDataShapeBuilder,
    test_name: &str,
    schema: &ExportedDataSchema,
    fallback_schemas: Option<&BTreeMap<String, ExportedDataSchema>>,
    prefix: &str,
    reference: &Reference,
) -> miette::Result<String> {
    let key = reference.as_key();
    if let Some(name) = builder.generated_refs.get(&key) {
        return Ok(name.clone());
    }
    if !builder.visiting_refs.insert(key.clone()) {
        // Recursive reference — record and return the forward-reference name.
        // The actual body will be generated when the outer traversal completes,
        // and the caller will wrap the definitions in a Lean `mutual ... end`
        // block so the forward references resolve.
        builder.recursive_refs.insert(key.clone());
        let name = schema_predicate_name(prefix, &key);
        return Ok(name);
    }

    let name = schema_predicate_name(prefix, &key);
    let result = (|| {
        let (resolved_schema, data) =
            exported_data_schema_for_reference(test_name, schema, fallback_schemas, reference)?;
        emit_exported_schema_data_predicate(
            builder,
            test_name,
            resolved_schema,
            fallback_schemas,
            prefix,
            &name,
            &key,
            data,
        )
    })();

    builder.visiting_refs.remove(&key);
    result?;
    builder.generated_refs.insert(key, name.clone());
    Ok(name)
}

fn ensure_exported_schema_declaration_predicate(
    builder: &mut LeanDataShapeBuilder,
    test_name: &str,
    schema: &ExportedDataSchema,
    fallback_schemas: Option<&BTreeMap<String, ExportedDataSchema>>,
    prefix: &str,
    declaration: &SchemaDeclaration<SchemaData>,
    path: &str,
) -> miette::Result<String> {
    match declaration {
        SchemaDeclaration::Referenced(reference) => ensure_exported_schema_reference_predicate(
            builder,
            test_name,
            schema,
            fallback_schemas,
            prefix,
            reference,
        ),
        SchemaDeclaration::Inline(data) => {
            let name = schema_predicate_name(prefix, path);
            emit_exported_schema_data_predicate(
                builder,
                test_name,
                schema,
                fallback_schemas,
                prefix,
                &name,
                path,
                data,
            )?;
            Ok(name)
        }
    }
}

fn emit_exported_schema_data_predicate(
    builder: &mut LeanDataShapeBuilder,
    test_name: &str,
    schema: &ExportedDataSchema,
    fallback_schemas: Option<&BTreeMap<String, ExportedDataSchema>>,
    prefix: &str,
    name: &str,
    path: &str,
    data: &SchemaData,
) -> miette::Result<()> {
    match data {
        SchemaData::Integer => {
            builder.definitions.push(format!(
                "def {name} : Data -> Prop\n  | Data.I _ => True\n  | _ => False\n"
            ));
        }
        SchemaData::Bytes => {
            builder.definitions.push(format!(
                "def {name} : Data -> Prop\n  | Data.B _ => True\n  | _ => False\n"
            ));
        }
        SchemaData::List(SchemaItems::One(item)) => {
            let item_predicate = ensure_exported_schema_declaration_predicate(
                builder,
                test_name,
                schema,
                fallback_schemas,
                prefix,
                item,
                &format!("{path}_item"),
            )?;
            let items_name = format!("{name}_items");
            builder.definitions.push(format!(
                "def {items_name} : List Data -> Prop\n  | [] => True\n  | x :: xs => {item_predicate} x {AND} {items_name} xs\n",
                AND = lean_sym::AND,
            ));
            builder.definitions.push(format!(
                "def {name} : Data -> Prop\n  | Data.List xs => {items_name} xs\n  | _ => False\n"
            ));
        }
        SchemaData::List(SchemaItems::Many(items)) => {
            let vars: Vec<String> = (0..items.len()).map(|index| format!("x_{index}")).collect();
            let mut parts = Vec::new();
            for (index, item) in items.iter().enumerate() {
                let predicate = ensure_exported_schema_declaration_predicate(
                    builder,
                    test_name,
                    schema,
                    fallback_schemas,
                    prefix,
                    &item.annotated,
                    &format!("{path}_item_{index}"),
                )?;
                parts.push(format!("{predicate} {}", vars[index]));
            }
            let pattern = format!("[{}]", vars.join(", "));
            builder.definitions.push(format!(
                "def {name} : Data -> Prop\n  | Data.List {pattern} => {}\n  | _ => False\n",
                lean_prop_conjunction(&parts)
            ));
        }
        SchemaData::Map(keys, values) => {
            let key_predicate = ensure_exported_schema_declaration_predicate(
                builder,
                test_name,
                schema,
                fallback_schemas,
                prefix,
                keys,
                &format!("{path}_keys"),
            )?;
            let value_predicate = ensure_exported_schema_declaration_predicate(
                builder,
                test_name,
                schema,
                fallback_schemas,
                prefix,
                values,
                &format!("{path}_values"),
            )?;
            let entries_name = format!("{name}_entries");
            builder.definitions.push(format!(
                "def {entries_name} : List (Prod Data Data) -> Prop\n  | [] => True\n  | entry :: rest => {key_predicate} entry.1 {AND} {value_predicate} entry.2 {AND} {entries_name} rest\n",
                AND = lean_sym::AND,
            ));
            builder.definitions.push(format!(
                "def {name} : Data -> Prop\n  | Data.Map entries => {entries_name} entries\n  | _ => False\n"
            ));
        }
        SchemaData::AnyOf(constructors) => {
            if constructors.is_empty() {
                return Err(unsupported_error(
                    "E0027",
                    UnsupportedReason::EmptyConstructorDomain {
                        test_name: test_name.to_string(),
                    },
                ));
            }

            let mut definition = format!("def {name} : Data -> Prop\n");
            for (ctor_index, constructor) in constructors.iter().enumerate() {
                let vars: Vec<String> = (0..constructor.annotated.fields.len())
                    .map(|index| format!("x_{index}"))
                    .collect();
                let mut parts = Vec::new();

                for (field_index, field) in constructor.annotated.fields.iter().enumerate() {
                    let predicate = ensure_exported_schema_declaration_predicate(
                        builder,
                        test_name,
                        schema,
                        fallback_schemas,
                        prefix,
                        &field.annotated,
                        &format!("{path}_ctor_{ctor_index}_field_{field_index}"),
                    )?;
                    parts.push(format!("{predicate} {}", vars[field_index]));
                }

                definition.push_str(&format!(
                    "  | Data.Constr {} [{}] => {}\n",
                    constructor.annotated.index,
                    vars.join(", "),
                    lean_prop_conjunction(&parts)
                ));
            }
            definition.push_str("  | _ => False\n");
            builder.definitions.push(definition);
        }
        SchemaData::Opaque => {
            // Untyped `Data` fields (e.g. `Redeemer = Data` inside a
            // `Transaction`) have no structural shape by construction: any
            // `PlutusData` value is admissible. Emit a trivially-true
            // predicate so the enclosing structural predicate can still be
            // generated. The `_` bindings below keep the signature
            // consistent with the other `Data -> Prop` definitions.
            let _ = (test_name, path);
            builder
                .definitions
                .push(format!("def {name} : Data -> Prop\n  | _ => True\n"));
        }
    }

    Ok(())
}

fn build_exported_data_shape_predicates(
    test_name: &str,
    schema: &ExportedDataSchema,
    prefix: &str,
) -> miette::Result<(String, String)> {
    let mut builder = LeanDataShapeBuilder::default();
    let root_predicate =
        build_exported_data_shape_predicates_into(test_name, schema, None, prefix, &mut builder)?;
    let body = finalize_lean_data_shape_builder(builder);
    Ok((root_predicate, body))
}

/// Populate `builder` with predicate definitions for `schema` (root + all
/// transitively reachable references), returning the name of the root
/// predicate. Does **not** finalize the builder: the caller is responsible
/// for calling [`finalize_lean_data_shape_builder`] once all contributions
/// have been accumulated.
///
/// This is the entry point used when a single `LeanDataShapeBuilder`
/// instance is shared across multiple emission sites (e.g. outer schema +
/// nested `DataWithSchema` semantics + reachability helpers). Sharing the
/// builder lets the internal `generated_refs` deduplication elide repeated
/// lookups of the same type (e.g. `cardano/transaction.Transaction`) across
/// every section of a state-machine proof.
fn build_exported_data_shape_predicates_into(
    test_name: &str,
    schema: &ExportedDataSchema,
    fallback_schemas: Option<&BTreeMap<String, ExportedDataSchema>>,
    prefix: &str,
    builder: &mut LeanDataShapeBuilder,
) -> miette::Result<String> {
    ensure_exported_schema_reference_predicate(
        builder,
        test_name,
        schema,
        fallback_schemas,
        prefix,
        &schema.root,
    )
}

/// Render the accumulated shape predicate definitions into a single Lean
/// block, wrapping them in a `mutual ... end` group when any recursive
/// reference was encountered during population.
fn finalize_lean_data_shape_builder(builder: LeanDataShapeBuilder) -> String {
    if builder.recursive_refs.is_empty() {
        builder.definitions.join("\n")
    } else {
        // Wrap all definitions inside a `mutual ... end` block so forward
        // references emitted for recursive schema cycles (including
        // mutually-recursive groups) resolve correctly.
        //
        // This is the safe over-approximation: any non-recursive definition
        // that ends up inside the block is simply tolerated by Lean. A more
        // precise SCC-based grouping can replace this later.
        let inner = builder
            .definitions
            .iter()
            .map(|def| {
                def.lines()
                    .map(|line| {
                        if line.is_empty() {
                            String::new()
                        } else {
                            format!("  {line}")
                        }
                    })
                    .collect::<Vec<_>>()
                    .join("\n")
            })
            .collect::<Vec<_>>()
            .join("\n");
        format!("mutual\n{inner}\nend\n")
    }
}

struct TransitionDomainShapeState<'a> {
    enabled: bool,
    test_name: &'a str,
    shape_prefix: String,
    inner_data_schemas: Option<&'a BTreeMap<String, ExportedDataSchema>>,
    builder: LeanDataShapeBuilder,
}

fn schema_type_name_candidates(type_name: &str) -> Vec<String> {
    let mut candidates = vec![type_name.to_string()];
    if let Some((module, name)) = type_name.rsplit_once('.') {
        let slash = format!("{module}/{name}");
        if !candidates.contains(&slash) {
            candidates.push(slash);
        }
    }
    candidates
}

impl<'a> TransitionDomainShapeState<'a> {
    #[cfg(test)]
    fn disabled() -> Self {
        Self {
            enabled: false,
            test_name: "transition_prop",
            shape_prefix: "transition_prop_shape".to_string(),
            inner_data_schemas: None,
            builder: LeanDataShapeBuilder::default(),
        }
    }

    fn enabled(
        test_name: &'a str,
        shape_prefix: String,
        inner_data_schemas: &'a BTreeMap<String, ExportedDataSchema>,
    ) -> Self {
        Self {
            enabled: true,
            test_name,
            shape_prefix,
            inner_data_schemas: Some(inner_data_schemas),
            builder: LeanDataShapeBuilder::default(),
        }
    }

    fn predicate_for_data_with_schema(&mut self, type_name: &str) -> Option<String> {
        if !self.enabled {
            return None;
        }
        let inner_data_schemas = self.inner_data_schemas?;
        let candidates = schema_type_name_candidates(type_name);
        let owned_schema = if let Some(schema) = candidates
            .iter()
            .find_map(|candidate| inner_data_schemas.get(candidate))
        {
            schema.clone()
        } else {
            let (reference, root_key) = candidates.iter().find_map(|candidate| {
                let reference = Reference::new(candidate);
                inner_data_schemas
                    .values()
                    .any(|schema| schema.definitions.try_lookup(&reference).is_some())
                    .then_some((reference, candidate.clone()))
            })?;
            let parent = inner_data_schemas
                .values()
                .find(|schema| schema.definitions.try_lookup(&reference).is_some())?;
            ExportedDataSchema {
                root: Reference::new(&root_key),
                definitions: parent.definitions.clone(),
            }
        };
        build_exported_data_shape_predicates_into(
            self.test_name,
            &owned_schema,
            Some(inner_data_schemas),
            &self.shape_prefix,
            &mut self.builder,
        )
        .ok()
    }

    fn take_definitions(&mut self) -> String {
        finalize_lean_data_shape_builder(std::mem::take(&mut self.builder))
    }
}

fn build_exported_data_shape_witness_from_declaration(
    test_name: &str,
    schema: &ExportedDataSchema,
    declaration: &SchemaDeclaration<SchemaData>,
) -> miette::Result<String> {
    match declaration {
        SchemaDeclaration::Referenced(reference) => {
            build_exported_data_shape_witness_from_reference(test_name, schema, reference)
        }
        SchemaDeclaration::Inline(data) => {
            build_exported_data_shape_witness_from_data(test_name, schema, data)
        }
    }
}

fn build_exported_data_shape_witness_from_reference(
    test_name: &str,
    schema: &ExportedDataSchema,
    reference: &Reference,
) -> miette::Result<String> {
    let (_resolved_schema, data) =
        exported_data_schema_for_reference(test_name, schema, None, reference)?;
    build_exported_data_shape_witness_from_data(test_name, schema, data)
}

fn build_exported_data_shape_witness_from_data(
    test_name: &str,
    schema: &ExportedDataSchema,
    data: &SchemaData,
) -> miette::Result<String> {
    match data {
        SchemaData::Integer => Ok("Data.I 0".to_string()),
        SchemaData::Bytes => Ok("Data.B ByteString.empty".to_string()),
        SchemaData::List(SchemaItems::One(_)) => Ok("Data.List []".to_string()),
        SchemaData::List(SchemaItems::Many(items)) => {
            let values = items
                .iter()
                .map(|item| {
                    build_exported_data_shape_witness_from_declaration(
                        test_name,
                        schema,
                        &item.annotated,
                    )
                })
                .collect::<miette::Result<Vec<_>>>()?;
            Ok(format!("Data.List [{}]", values.join(", ")))
        }
        SchemaData::Map(_, _) => Ok("Data.Map []".to_string()),
        SchemaData::AnyOf(constructors) => {
            let Some(constructor) = constructors.first() else {
                return Err(unsupported_error(
                    "E0027",
                    UnsupportedReason::EmptyConstructorDomain {
                        test_name: test_name.to_string(),
                    },
                ));
            };
            let fields = constructor
                .annotated
                .fields
                .iter()
                .map(|field| {
                    build_exported_data_shape_witness_from_declaration(
                        test_name,
                        schema,
                        &field.annotated,
                    )
                })
                .collect::<miette::Result<Vec<_>>>()?;
            Ok(format!(
                "Data.Constr {} [{}]",
                constructor.annotated.index,
                fields.join(", ")
            ))
        }
        SchemaData::Opaque => {
            // For an untyped `Data` leaf, any value is a valid witness.
            // Pick the simplest constructor form.
            let _ = test_name;
            Ok("Data.I 0".to_string())
        }
    }
}

#[derive(Default)]
struct LeanDataSemanticsBuilder {
    next_id: usize,
    definitions: Vec<String>,
}

fn next_partial_data_semantics_name(
    builder: &mut LeanDataSemanticsBuilder,
    prefix: &str,
    suffix: &str,
) -> String {
    let id = builder.next_id;
    builder.next_id += 1;
    format!("{prefix}_{}_{}", lean_name_fragment(suffix), id)
}

fn emit_partial_data_semantics_predicate(
    builder: &mut LeanDataSemanticsBuilder,
    prefix: &str,
    semantics: &FuzzerSemantics,
    test_name: &str,
    inner_data_schemas: &BTreeMap<String, ExportedDataSchema>,
    shape_builder: &mut LeanDataShapeBuilder,
    shape_prefix: &str,
) -> miette::Result<String> {
    let name = next_partial_data_semantics_name(builder, prefix, "pred");

    match semantics {
        FuzzerSemantics::Bool => {
            builder.definitions.push(format!(
                "def {name} : Data -> Prop\n  | Data.Constr 0 [] => True\n  | Data.Constr 1 [] => True\n  | _ => False\n"
            ));
        }
        FuzzerSemantics::IntRange { min, max } => {
            let body = match (min, max) {
                (Some(min), Some(max)) => {
                    format!("({min} <= x {AND} x <= {max})", AND = lean_sym::AND)
                }
                (Some(min), None) => format!("({min} <= x)"),
                (None, Some(max)) => format!("(x <= {max})"),
                (None, None) => "True".to_string(),
            };
            builder.definitions.push(format!(
                "def {name} : Data -> Prop\n  | Data.I x => {body}\n  | _ => False\n"
            ));
        }
        FuzzerSemantics::ByteArrayRange { min_len, max_len } => {
            let body = match (min_len, max_len) {
                (Some(min_len), Some(max_len)) => {
                    format!(
                        "({min_len} <= x.length {AND} x.length <= {max_len})",
                        AND = lean_sym::AND
                    )
                }
                (Some(min_len), None) => format!("({min_len} <= x.length)"),
                (None, Some(max_len)) => format!("(x.length <= {max_len})"),
                (None, None) => "True".to_string(),
            };
            builder.definitions.push(format!(
                "def {name} : Data -> Prop\n  | Data.B x => {body}\n  | _ => False\n"
            ));
        }
        FuzzerSemantics::String => {
            builder.definitions.push(format!(
                "def {name} : Data -> Prop\n  | Data.B _ => True\n  | _ => False\n"
            ));
        }
        FuzzerSemantics::Data => {
            builder
                .definitions
                .push(format!("def {name} (_ : Data) : Prop := True\n"));
        }
        FuzzerSemantics::DataWithSchema { type_name } => {
            // Prefer the pre-computed structural schema for this ADT — populated
            // at export time by walking the fuzzer semantics and deriving a
            // schema for every `DataWithSchema` leaf. When available, contribute
            // predicate definitions to the *shared* shape builder (so repeated
            // lookups of the same type — e.g. `Transaction` under both an
            // outer schema and a state-machine event semantics — are emitted
            // exactly once via the builder's `generated_refs` deduplication)
            // and forward the root to callers via `name`.
            //
            // If the schema can't be resolved (unknown / unqualified type),
            // fall back to the trivial `True` placeholder so the theorem still
            // type-checks. The fuzzer_data_schema at the outer level is
            // responsible for the overall domain constraint in that case.
            // M6 — Stop silently widening schema lowering errors to
            // `def {name} (_ : Data) : Prop := True`.
            //
            // The previous `if let Ok(root) = ...` arm swallowed every
            // failure and replaced the structural predicate with the
            // trivial `True` fallback.  That left the test's reported
            // precondition out of sync with the actual semantic
            // constraint: a proof that succeeded against the widened
            // predicate carried no guarantee for the real fuzzer
            // domain.  The schema lowerer is supposed to produce a
            // faithful predicate or fail loud; the silent True branch
            // broke that contract.
            //
            // Two cases remain after the rewrite:
            //
            //   1. Schema is *not* in `inner_data_schemas` (unknown or
            //      unqualified type, e.g. when the outer
            //      `fuzzer_data_schema` already covers the type at the
            //      top level and we don't need a redundant inner
            //      predicate).  Sound to widen here — emit the True
            //      placeholder unchanged.
            //
            //   2. Schema is present but the lowerer returns `Err(_)`
            //      (recursive cycle outside the mutual block, opaque
            //      element, non-Data root, etc.).  Wrap the underlying
            //      error in `unsupported("E0015", QualifiedAdtNoSchema
            //      { test_name, type_name })` and propagate.  The
            //      `type_name` for the failing ADT is in scope here
            //      (it's the discriminant of the `DataWithSchema`
            //      semantics arm), so E0015 is the natural code (its
            //      payload requires `type_name`).  The lowerer's error
            //      message is intentionally NOT smuggled into the
            //      reason payload — `QualifiedAdtNoSchema` is a typed
            //      wrapper, not a free-form bag, and the catalogue
            //      summary already records the failure category.
            //      Downstream rendering surfaces both via
            //      `aiken verify capabilities`.
            if let Some(schema) = inner_data_schemas.get(type_name.as_str()) {
                let root_pred = build_exported_data_shape_predicates_into(
                    test_name,
                    schema,
                    Some(inner_data_schemas),
                    shape_prefix,
                    shape_builder,
                )
                .map_err(|_| {
                    miette::Report::new(error_catalogue::unsupported(
                        "E0015",
                        UnsupportedReason::QualifiedAdtNoSchema {
                            test_name: test_name.to_string(),
                            type_name: type_name.clone(),
                        },
                    ))
                })?;
                builder.definitions.push(format!(
                    "-- DataWithSchema '{type_name}': structural predicate derived from exported schema\n\
                     def {name} (x : Data) : Prop := {root_pred} x\n"
                ));
            } else {
                // Commit 18 (folds C13 #2): the True fallback is sound *only*
                // because the outer guard `fuzzer_semantics_requires_schema`
                // (see ~line 6918) refuses to lower any test whose semantics
                // tree references a `DataWithSchema` without a top-level
                // `fuzzer_data_schema` populated. The inner schema dictionary
                // is keyed off that outer schema's `inner_schemas`, so a
                // missing entry means the type is covered by the outer
                // root predicate rather than needing its own inner predicate.
                //
                // The comment in the emitted `def` already records this
                // contract for human readers of the generated Lean; tightening
                // this site to a hard `FallbackRequired` would also need a
                // matching tightening of `export_data_schema` (see lib.rs)
                // to guarantee every reachable inner ADT is exported. Park
                // until that audit is performed.
                builder.definitions.push(format!(
                    "-- DataWithSchema '{type_name}': no resolvable schema; fallback to True (outer fuzzer_data_schema covers it)\n\
                     def {name} (_ : Data) : Prop := True\n"
                ));
            }
        }
        FuzzerSemantics::Opaque { reason } => {
            return Err(generation_error(
                GenerationErrorCategory::FallbackRequired,
                format!(
                    "Cannot generate direct proof for opaque semantics: {}. The fuzzer domain cannot be statically analyzed.",
                    reason
                ),
            ));
        }
        FuzzerSemantics::Exact(FuzzerExactValue::Bool(value)) => {
            let tag = if *value { 1 } else { 0 };
            builder.definitions.push(format!(
                "def {name} (x : Data) : Prop := x = Data.Constr {tag} []\n"
            ));
        }
        FuzzerSemantics::Exact(FuzzerExactValue::ByteArray(bytes)) => {
            let literal = lean_bytestring_literal_from_bytes(bytes);
            builder.definitions.push(format!(
                "def {name} (x : Data) : Prop := x = Data.B {literal}\n"
            ));
        }
        FuzzerSemantics::Exact(FuzzerExactValue::String(value)) => {
            let literal = lean_bytestring_literal_from_bytes(value.as_bytes());
            builder.definitions.push(format!(
                "def {name} (x : Data) : Prop := x = Data.B {literal}\n"
            ));
        }
        FuzzerSemantics::OneOf(values) => {
            if values.is_empty() {
                return Err(semantics_output_type_mismatch_error(test_name));
            }
            let mut parts = Vec::new();
            for value in values {
                let literal = match value {
                    FuzzerExactValue::Bool(value) => {
                        let tag = if *value { 1 } else { 0 };
                        format!("Data.Constr {tag} []")
                    }
                    FuzzerExactValue::ByteArray(bytes) => {
                        format!("Data.B {}", lean_bytestring_literal_from_bytes(bytes))
                    }
                    FuzzerExactValue::String(value) => {
                        format!(
                            "Data.B {}",
                            lean_bytestring_literal_from_bytes(value.as_bytes())
                        )
                    }
                };
                parts.push(format!("x = {literal}"));
            }
            builder.definitions.push(format!(
                "def {name} (x : Data) : Prop := {}\n",
                parts.join(&format!(" {} ", lean_sym::OR))
            ));
        }
        FuzzerSemantics::Product(elems) => {
            let vars: Vec<String> = (0..elems.len()).map(|index| format!("x_{index}")).collect();
            let parts = elems
                .iter()
                .zip(vars.iter())
                .map(|(elem, var)| {
                    let predicate = emit_partial_data_semantics_predicate(
                        builder,
                        prefix,
                        elem,
                        test_name,
                        inner_data_schemas,
                        shape_builder,
                        shape_prefix,
                    )?;
                    Ok(format!("{predicate} {var}"))
                })
                .collect::<miette::Result<Vec<_>>>()?;

            builder.definitions.push(format!(
                "def {name} : Data -> Prop\n  | Data.List [{}] => {}\n  | _ => False\n",
                vars.join(", "),
                lean_prop_conjunction(&parts)
            ));
        }
        FuzzerSemantics::List {
            element,
            min_len,
            max_len,
        } => {
            let element_predicate = emit_partial_data_semantics_predicate(
                builder,
                prefix,
                element,
                test_name,
                inner_data_schemas,
                shape_builder,
                shape_prefix,
            )?;
            let items_name = format!("{name}_items");
            builder.definitions.push(format!(
                "def {items_name} : List Data -> Prop\n  | [] => True\n  | x :: xs => {element_predicate} x {AND} {items_name} xs\n",
                AND = lean_sym::AND,
            ));

            let mut parts = Vec::new();
            match (min_len, max_len) {
                (Some(min_len), Some(max_len)) => {
                    parts.push(format!(
                        "({min_len} <= xs.length {AND} xs.length <= {max_len})",
                        AND = lean_sym::AND,
                    ));
                }
                (Some(min_len), None) => parts.push(format!("({min_len} <= xs.length)")),
                (None, Some(max_len)) => parts.push(format!("(xs.length <= {max_len})")),
                (None, None) => {}
            }
            parts.push(format!("{items_name} xs"));

            builder.definitions.push(format!(
                "def {name} : Data -> Prop\n  | Data.List xs => {}\n  | _ => False\n",
                lean_prop_conjunction(&parts)
            ));
        }
        FuzzerSemantics::Constructors { tags } => {
            let predicate = data_constructor_tags_predicate("x", tags).ok_or_else(|| {
                generation_error(
                    GenerationErrorCategory::FallbackRequired,
                    "empty constructor-tag semantic domain",
                )
            })?;
            builder
                .definitions
                .push(format!("def {name} (x : Data) : Prop := {predicate}\n"));
        }
        FuzzerSemantics::StateMachineTrace { .. } => {
            return Err(unsupported_error(
                "E0020",
                UnsupportedReason::NestedStateMachine {
                    test_name: test_name.to_string(),
                },
            ));
        }
    }

    Ok(name)
}

/// Build semantics predicate definitions into a shared `LeanDataShapeBuilder`.
///
/// Any structural `DataWithSchema` lookups performed while emitting the
/// semantics tree are routed through `shape_builder` (under the standardized
/// `shape_prefix`) so repeated references to the same ADT — e.g. `Transaction`
/// appearing in both the outer fuzzer domain schema and the inner state-machine
/// output semantics — are only materialized once.
///
/// The returned definitions string contains *only* the semantics-level
/// predicates (the shape predicates remain accumulated in `shape_builder`, to
/// be finalized by the caller via [`finalize_lean_data_shape_builder`]).
fn build_partial_data_semantics_predicates_into(
    prefix: &str,
    semantics: &FuzzerSemantics,
    test_name: &str,
    inner_data_schemas: &BTreeMap<String, ExportedDataSchema>,
    shape_builder: &mut LeanDataShapeBuilder,
    shape_prefix: &str,
) -> miette::Result<(String, String)> {
    let mut builder = LeanDataSemanticsBuilder::default();
    let root = emit_partial_data_semantics_predicate(
        &mut builder,
        prefix,
        semantics,
        test_name,
        inner_data_schemas,
        shape_builder,
        shape_prefix,
    )?;
    Ok((root, builder.definitions.join("\n")))
}

/// Context accumulator used while emitting a `ShallowIr` node as a Lean
/// `Data` expression. Tracks every fresh existential variable introduced
/// for `FuzzExistential` / `Opaque` / `Let` nodes so that the caller can
/// wrap the final expression in the appropriate `∃` binders.
#[derive(Clone)]
struct ShallowIrEmitCtx {
    /// Existential bindings accumulated so far: `(var_name, lean_type_str)`.
    existentials: Vec<(String, String)>,
    fresh_counter: usize,
    /// Counts how many times each `TransitionProp::Exists` binder base name
    /// has been emitted globally. When a name has been used before, a numeric
    /// suffix is appended to prevent Lean variable shadowing.
    binder_counters: std::collections::HashMap<String, usize>,
    /// Every Lean name reserved so far in this emission context.
    used_names: std::collections::BTreeSet<String>,
    /// Stack of logical binder names to their alpha-renamed Lean names.
    binder_env: Vec<(String, String)>,
}

impl ShallowIrEmitCtx {
    fn new() -> Self {
        let mut used_names = std::collections::BTreeSet::new();
        used_names.insert("state".to_string());
        used_names.insert("transition".to_string());
        Self {
            existentials: Vec::new(),
            fresh_counter: 0,
            binder_counters: std::collections::HashMap::new(),
            used_names,
            binder_env: Vec::new(),
        }
    }

    /// Allocate a fresh existential variable of the given `ShallowIrType`
    /// and record it. Returns the chosen variable name.
    fn fresh(&mut self, ty: &aiken_lang::test_framework::ShallowIrType) -> String {
        loop {
            let name = format!("_fuzz_{}", self.fresh_counter);
            self.fresh_counter += 1;
            if self.used_names.insert(name.clone()) {
                self.existentials
                    .push((name.clone(), shallow_ir_type_to_lean(ty)));
                return name;
            }
        }
    }

    fn unique_binder(&mut self, base: &str) -> String {
        let base = if base.is_empty() { "_binder" } else { base };
        let counter = self.binder_counters.entry(base.to_string()).or_insert(0);
        loop {
            let candidate = if *counter == 0 {
                base.to_string()
            } else {
                format!("{base}_{}", *counter)
            };
            *counter += 1;
            if self.used_names.insert(candidate.clone()) {
                return candidate;
            }
        }
    }

    fn push_binder(&mut self, logical: &str, emitted: &str) {
        self.binder_env
            .push((logical.to_string(), emitted.to_string()));
    }

    fn pop_binder(&mut self, logical: &str) {
        if let Some(index) = self
            .binder_env
            .iter()
            .rposition(|(name, _)| name == logical)
        {
            self.binder_env.remove(index);
        }
    }

    fn resolve_bound_name(&self, logical: &str) -> String {
        self.binder_env
            .iter()
            .rev()
            .find_map(|(name, emitted)| (name == logical).then_some(emitted.clone()))
            .unwrap_or_else(|| logical.to_string())
    }

    /// Create a forked child context that inherits the fresh counter (to avoid
    /// name collisions) but starts with an empty existentials list. Used by
    /// `EqOutput` to keep its existentials local rather than hoisting them to
    /// the definition level. After forking, sync `fresh_counter` back.
    /// `self.fresh_counter = child.fresh_counter`.
    fn fork(&self) -> Self {
        Self {
            existentials: Vec::new(),
            fresh_counter: self.fresh_counter,
            binder_counters: self.binder_counters.clone(),
            used_names: self.used_names.clone(),
            binder_env: self.binder_env.clone(),
        }
    }
}

/// Map a `ShallowIrType` to the Lean type name we quantify over when
/// introducing an existential binder. Everything that isn't a primitive
/// scalar collapses to `Data`.
fn shallow_ir_type_to_lean(ty: &aiken_lang::test_framework::ShallowIrType) -> String {
    use aiken_lang::test_framework::ShallowIrType;
    match ty {
        ShallowIrType::Int => "Int".into(),
        ShallowIrType::Bool => "Bool".into(),
        ShallowIrType::ByteArray => "ByteString".into(),
        ShallowIrType::Unit => "Unit".into(),
        ShallowIrType::String => "ByteString".into(),
        ShallowIrType::Data | ShallowIrType::Unknown | ShallowIrType::Adt(_) => "Data".into(),
        ShallowIrType::List(inner) => format!("List ({})", shallow_ir_type_to_lean(inner)),
        ShallowIrType::Pair(a, b) => format!(
            "({} {CROSS} {})",
            shallow_ir_type_to_lean(a),
            shallow_ir_type_to_lean(b),
            CROSS = lean_sym::CROSS,
        ),
        _ => "Data".into(),
    }
}

fn emit_typed_var_as_lean_data(
    name: &str,
    ty: &aiken_lang::test_framework::ShallowIrType,
    ctx: &mut ShallowIrEmitCtx,
    depth: usize,
) -> String {
    use aiken_lang::test_framework::ShallowIrType;
    match ty {
        ShallowIrType::Int => format!("Data.I {name}"),
        ShallowIrType::Bool => format!("Data.Constr (if {name} then 1 else 0) []"),
        ShallowIrType::ByteArray | ShallowIrType::String => format!("Data.B {name}"),
        ShallowIrType::Data | ShallowIrType::Unknown | ShallowIrType::Adt(_) => name.to_string(),
        ShallowIrType::Unit => "Data.Constr (0 : Integer) []".to_string(),
        ShallowIrType::List(elem) => {
            let elem_var = format!("x_{depth}");
            let elem_encoder = emit_typed_var_as_lean_data(&elem_var, elem, ctx, depth + 1);
            format!("Data.List ({name}.map (fun {elem_var} => {elem_encoder}))")
        }
        ShallowIrType::Pair(a, b) => {
            let fst_var = format!("p_{depth}_1");
            let snd_var = format!("p_{depth}_2");
            let fst_enc = emit_typed_var_as_lean_data(&fst_var, a, ctx, depth + 1);
            let snd_enc = emit_typed_var_as_lean_data(&snd_var, b, ctx, depth + 1);
            format!("(let ({fst_var}, {snd_var}) := {name}; Data.List [{fst_enc}, {snd_enc}])")
        }
        _ => ctx.fresh(ty),
    }
}

fn emit_bound_var_as_lean_data(
    name: &str,
    ty: &aiken_lang::test_framework::ShallowIrType,
    ctx: &mut ShallowIrEmitCtx,
) -> String {
    emit_typed_var_as_lean_data(&ctx.resolve_bound_name(name), ty, ctx, 0)
}

fn shallow_ir_type_hint(
    ir: &aiken_lang::test_framework::ShallowIr,
) -> Option<aiken_lang::test_framework::ShallowIrType> {
    use aiken_lang::test_framework::{ShallowConst, ShallowIr, ShallowIrType};

    match ir {
        ShallowIr::Const(ShallowConst::Int(_)) => Some(ShallowIrType::Int),
        ShallowIr::Const(ShallowConst::Bool(_)) => Some(ShallowIrType::Bool),
        ShallowIr::Const(ShallowConst::ByteArray(_)) => Some(ShallowIrType::ByteArray),
        ShallowIr::Const(ShallowConst::String(_)) => Some(ShallowIrType::String),
        ShallowIr::Const(ShallowConst::Unit) => Some(ShallowIrType::Unit),
        ShallowIr::Var { ty, .. }
        | ShallowIr::BoundVar { ty, .. }
        | ShallowIr::FieldAccess { ty, .. }
        | ShallowIr::Opaque { ty, .. }
        | ShallowIr::FuzzExistential { kind: ty, .. } => Some(ty.clone()),
        ShallowIr::ListLit { ty, .. } => Some(ty.clone()),
        ShallowIr::Tuple(_)
        | ShallowIr::Construct { .. }
        | ShallowIr::RecordUpdate { .. }
        | ShallowIr::BinOp { .. }
        | ShallowIr::Let { .. }
        | ShallowIr::If { .. }
        | ShallowIr::Match { .. }
        | _ => None,
    }
}

fn emit_pair_bound_var_component_as_lean_data(
    name: &str,
    fst: &aiken_lang::test_framework::ShallowIrType,
    snd: &aiken_lang::test_framework::ShallowIrType,
    index: u64,
    ctx: &mut ShallowIrEmitCtx,
) -> Option<String> {
    let resolved = ctx.resolve_bound_name(name);
    let fst_var = format!("_pair_{}_0", resolved);
    let snd_var = format!("_pair_{}_1", resolved);
    let component = match index {
        0 => emit_typed_var_as_lean_data(&fst_var, fst, ctx, 1),
        1 => emit_typed_var_as_lean_data(&snd_var, snd, ctx, 1),
        _ => return None,
    };
    Some(format!(
        "(let ({fst_var}, {snd_var}) := {resolved}; {component})"
    ))
}

fn exact_data_projection_fallback() -> &'static str {
    "Data.Constr (0 : Integer) []"
}

fn emit_exact_constructor_field_projection(record_data: String, index: u64) -> String {
    let fallback = exact_data_projection_fallback();
    format!(
        "(match {record_data} with | Data.Constr _ fields => (match fields.get? {index} with | some field => field | none => {fallback}) | _ => {fallback})"
    )
}

fn try_emit_shallow_ir_as_lean_data_exact(
    ir: &aiken_lang::test_framework::ShallowIr,
    ctx: &mut ShallowIrEmitCtx,
) -> Option<String> {
    let mut preview_ctx = ctx.clone();
    let baseline_existentials = preview_ctx.existentials.len();
    let mut preview_log = Vec::new();
    let out = emit_shallow_ir_as_lean_data(ir, &mut preview_ctx, &mut preview_log);
    if preview_log.is_empty() && preview_ctx.existentials.len() == baseline_existentials {
        *ctx = preview_ctx;
        Some(out)
    } else {
        None
    }
}

fn try_emit_shallow_ir_as_lean_bool_exact(
    ir: &aiken_lang::test_framework::ShallowIr,
    ctx: &mut ShallowIrEmitCtx,
) -> Option<String> {
    let mut preview_ctx = ctx.clone();
    let baseline_existentials = preview_ctx.existentials.len();
    let mut preview_log = Vec::new();
    let out = emit_shallow_ir_as_lean_bool(ir, &mut preview_ctx, &mut preview_log);
    if preview_log.is_empty() && preview_ctx.existentials.len() == baseline_existentials {
        *ctx = preview_ctx;
        Some(out)
    } else {
        None
    }
}

fn try_emit_shallow_ir_as_lean_integer_exact(
    ir: &aiken_lang::test_framework::ShallowIr,
    ctx: &mut ShallowIrEmitCtx,
) -> Option<String> {
    use aiken_lang::test_framework::{ShallowBinOp, ShallowConst, ShallowIr, ShallowIrType};

    match ir {
        ShallowIr::Const(ShallowConst::Int(s)) => Some(format!("({s} : Integer)")),
        ShallowIr::BoundVar { name, ty } if matches!(ty, ShallowIrType::Int) => {
            Some(ctx.resolve_bound_name(name))
        }
        ShallowIr::Let { value, body, .. } => {
            let mut preview_ctx = ctx.clone();
            try_emit_shallow_ir_as_lean_data_exact(value, &mut preview_ctx)?;
            let body_expr = try_emit_shallow_ir_as_lean_integer_exact(body, &mut preview_ctx)?;
            *ctx = preview_ctx;
            Some(body_expr)
        }
        ShallowIr::If {
            cond,
            then_branch,
            else_branch,
        } => {
            let mut preview_ctx = ctx.clone();
            let cond_expr = try_emit_shallow_ir_as_lean_bool_exact(cond, &mut preview_ctx)?;
            let then_expr =
                try_emit_shallow_ir_as_lean_integer_exact(then_branch, &mut preview_ctx)?;
            let else_expr =
                try_emit_shallow_ir_as_lean_integer_exact(else_branch, &mut preview_ctx)?;
            *ctx = preview_ctx;
            Some(format!(
                "(if {cond_expr} then {then_expr} else {else_expr})"
            ))
        }
        ShallowIr::Match { subject, arms } => {
            let mut preview_ctx = ctx.clone();
            let subject_expr = try_emit_shallow_ir_as_lean_data_exact(subject, &mut preview_ctx)?;
            let mut tagged = Vec::new();
            let mut default_expr = None;

            for arm in arms {
                let body_expr =
                    try_emit_shallow_ir_as_lean_integer_exact(&arm.body, &mut preview_ctx)?;
                match arm.tag {
                    Some(tag) => tagged.push((tag, body_expr)),
                    None => default_expr = Some(body_expr),
                }
            }

            let default_expr = default_expr?;
            let mut result = default_expr.clone();
            for (tag, body) in tagged.into_iter().rev() {
                result = format!("(if tag = ({tag} : Integer) then {body} else {result})");
            }

            *ctx = preview_ctx;
            Some(format!(
                "(match {subject_expr} with | Data.Constr tag _ => {result} | _ => {default_expr})"
            ))
        }
        ShallowIr::BinOp { op, left, right } => {
            let mut preview_ctx = ctx.clone();
            let left_expr = try_emit_shallow_ir_as_lean_integer_exact(left, &mut preview_ctx)?;
            let right_expr = try_emit_shallow_ir_as_lean_integer_exact(right, &mut preview_ctx)?;
            let op_expr = match op {
                ShallowBinOp::Add => format!("({left_expr} + {right_expr})"),
                ShallowBinOp::Sub => format!("({left_expr} - {right_expr})"),
                ShallowBinOp::Mul => format!("({left_expr} * {right_expr})"),
                ShallowBinOp::Div => format!("({left_expr} / {right_expr})"),
                ShallowBinOp::Mod => format!("({left_expr} % {right_expr})"),
                _ => return None,
            };
            *ctx = preview_ctx;
            Some(op_expr)
        }
        _ => None,
    }
}

fn shallow_ir_mentions_name(ir: &aiken_lang::test_framework::ShallowIr, name: &str) -> bool {
    use aiken_lang::test_framework::ShallowIr;

    match ir {
        ShallowIr::Var { name: ir_name, .. } | ShallowIr::BoundVar { name: ir_name, .. } => {
            ir_name == name
        }
        ShallowIr::Let { value, body, .. } => {
            shallow_ir_mentions_name(value, name) || shallow_ir_mentions_name(body, name)
        }
        ShallowIr::If {
            cond,
            then_branch,
            else_branch,
        } => {
            shallow_ir_mentions_name(cond, name)
                || shallow_ir_mentions_name(then_branch, name)
                || shallow_ir_mentions_name(else_branch, name)
        }
        ShallowIr::Match { subject, arms } => {
            shallow_ir_mentions_name(subject, name)
                || arms
                    .iter()
                    .any(|arm| shallow_ir_mentions_name(&arm.body, name))
        }
        ShallowIr::Construct { fields, .. } | ShallowIr::Tuple(fields) => fields
            .iter()
            .any(|field| shallow_ir_mentions_name(field, name)),
        ShallowIr::FieldAccess { record, .. } => shallow_ir_mentions_name(record, name),
        ShallowIr::RecordUpdate {
            record, updates, ..
        } => {
            shallow_ir_mentions_name(record, name)
                || updates
                    .iter()
                    .any(|update| shallow_ir_mentions_name(&update.value, name))
        }
        ShallowIr::BinOp { left, right, .. } => {
            shallow_ir_mentions_name(left, name) || shallow_ir_mentions_name(right, name)
        }
        ShallowIr::ListLit { elements, tail, .. } => {
            elements
                .iter()
                .any(|element| shallow_ir_mentions_name(element, name))
                || tail
                    .as_ref()
                    .is_some_and(|tail| shallow_ir_mentions_name(tail, name))
        }
        ShallowIr::Const(_) | ShallowIr::FuzzExistential { .. } | ShallowIr::Opaque { .. } => false,
        _ => false,
    }
}

fn freshen_unknown_field_projection(
    ctx: &mut ShallowIrEmitCtx,
    unsupported_log: &mut TransitionWideningLog,
    message: String,
) -> String {
    use aiken_lang::test_framework::ShallowIrType;

    push_widening(
        unsupported_log,
        TransitionWideningKind::DataFreshening,
        message,
    );
    let fresh = ctx.fresh(&ShallowIrType::Data);
    emit_bound_var_as_lean_data(&fresh, &ShallowIrType::Data, ctx)
}
fn emit_match_as_lean_data_exact(
    subject: &aiken_lang::test_framework::ShallowIr,
    arms: &[aiken_lang::test_framework::ShallowIrArm],
    ctx: &mut ShallowIrEmitCtx,
) -> Option<String> {
    let mut preview_ctx = ctx.clone();
    let subject_expr = try_emit_shallow_ir_as_lean_data_exact(subject, &mut preview_ctx)?;
    let mut tagged = Vec::new();
    let mut default_expr = None;

    for arm in arms {
        let body_expr = try_emit_shallow_ir_as_lean_data_exact(&arm.body, &mut preview_ctx)?;
        match arm.tag {
            Some(tag) => tagged.push((tag, body_expr)),
            None => default_expr = Some(body_expr),
        }
    }

    let default_expr = default_expr?;
    let mut result = default_expr.clone();
    for (tag, body) in tagged.into_iter().rev() {
        result = format!("(if tag = ({tag} : Integer) then {body} else {result})");
    }

    *ctx = preview_ctx;
    Some(format!(
        "(match {subject_expr} with | Data.Constr tag _ => {result} | _ => {default_expr})"
    ))
}

fn emit_field_projection_as_lean_data(
    record: &aiken_lang::test_framework::ShallowIr,
    index: u64,
    label: &str,
    kind: aiken_lang::test_framework::ShallowFieldAccessKind,
    ctx: &mut ShallowIrEmitCtx,
    unsupported_log: &mut TransitionWideningLog,
    context: &str,
) -> String {
    use aiken_lang::test_framework::{ShallowFieldAccessKind, ShallowIr, ShallowIrType};

    let field_kind = match kind {
        ShallowFieldAccessKind::ConstructorField => "constructor field",
        ShallowFieldAccessKind::ListElement => "list element",
    };
    let freshen = |ctx: &mut ShallowIrEmitCtx,
                   unsupported_log: &mut TransitionWideningLog,
                   detail: String| {
        freshen_unknown_field_projection(
            ctx,
            unsupported_log,
            format!("{context} {field_kind} `{label}` freshened: {detail}"),
        )
    };

    match (record, kind) {
        (ShallowIr::Construct { fields, .. }, ShallowFieldAccessKind::ConstructorField) => fields
            .get(index as usize)
            .map(|field| emit_shallow_ir_as_lean_data(field, ctx, unsupported_log))
            .unwrap_or_else(|| freshen(ctx, unsupported_log, format!("index {index} out of bounds"))),
        (ShallowIr::Tuple(elems), ShallowFieldAccessKind::ListElement) => elems
            .get(index as usize)
            .map(|elem| emit_shallow_ir_as_lean_data(elem, ctx, unsupported_log))
            .unwrap_or_else(|| freshen(ctx, unsupported_log, format!("index {index} out of bounds"))),
        (
            ShallowIr::ListLit { elements, tail, .. },
            ShallowFieldAccessKind::ListElement,
        ) => {
            if let Some(elem) = elements.get(index as usize) {
                emit_shallow_ir_as_lean_data(elem, ctx, unsupported_log)
            } else if tail.is_none() {
                freshen(ctx, unsupported_log, format!("index {index} out of bounds"))
            } else {
                freshen(
                    ctx,
                    unsupported_log,
                    "index falls into spread tail".to_string(),
                )
            }
        }
        (
            ShallowIr::BoundVar {
                name,
                ty: ShallowIrType::Pair(fst, snd),
            },
            ShallowFieldAccessKind::ListElement,
        ) => emit_pair_bound_var_component_as_lean_data(
            name,
            fst.as_ref(),
            snd.as_ref(),
            index,
            ctx,
        )
        .unwrap_or_else(|| freshen(ctx, unsupported_log, format!("index {index} out of bounds"))),
        (ShallowIr::RecordUpdate { record, updates, .. }, ShallowFieldAccessKind::ConstructorField) => {
            if let Some(update) = updates.iter().find(|update| update.index == index as usize) {
                emit_shallow_ir_as_lean_data(&update.value, ctx, unsupported_log)
            } else {
                emit_field_projection_as_lean_data(
                    record,
                    index,
                    label,
                    kind,
                    ctx,
                    unsupported_log,
                    context,
                )
            }
        }
        (
            record @ ShallowIr::BoundVar {
                ty: ShallowIrType::Adt(_),
                ..
            },
            ShallowFieldAccessKind::ConstructorField,
        )
        | (
            record @ ShallowIr::FieldAccess {
                ty: ShallowIrType::Adt(_),
                ..
            },
            ShallowFieldAccessKind::ConstructorField,
        ) => try_emit_shallow_ir_as_lean_data_exact(record, ctx)
            .map(|record_data| {
                push_widening(
                    unsupported_log,
                    TransitionWideningKind::Relation,
                    format!(
                        "{context} {field_kind} `{label}` widened via constructor-match fallback for non-constructor or missing-field cases"
                    ),
                );
                emit_exact_constructor_field_projection(record_data, index)
            })
            .unwrap_or_else(|| freshen(
                ctx,
                unsupported_log,
                "source is not structurally known".to_string(),
            )),
        _ => freshen(
            ctx,
            unsupported_log,
            "source is not structurally known".to_string(),
        ),
    }
}

/// Recursively emit a `ShallowIr` node as a Lean expression of type `Data`.
///
/// Any widening site must append a structured record to `unsupported_log`;
/// otherwise the exported transition theorem can look precise while quietly
/// introducing fresh witnesses unrelated to the source relation.
fn emit_shallow_ir_as_lean_data(
    ir: &aiken_lang::test_framework::ShallowIr,
    ctx: &mut ShallowIrEmitCtx,
    unsupported_log: &mut TransitionWideningLog,
) -> String {
    use aiken_lang::test_framework::{
        ShallowConst, ShallowFieldAccessKind, ShallowIr, ShallowIrType,
    };

    match ir {
        ShallowIr::Const(ShallowConst::Int(s)) => format!("Data.I ({s} : Integer)"),
        ShallowIr::Const(ShallowConst::Bool(true)) => "Data.Constr (1 : Integer) []".to_string(),
        ShallowIr::Const(ShallowConst::Bool(false)) => "Data.Constr (0 : Integer) []".to_string(),
        ShallowIr::Const(ShallowConst::Unit) => "Data.Constr (0 : Integer) []".to_string(),
        ShallowIr::Const(ShallowConst::String(value)) => format!(
            "Data.B ({})",
            lean_bytestring_literal_from_bytes_qualified(value.as_bytes())
        ),
        ShallowIr::Const(ShallowConst::ByteArray(hex_str)) => match hex::decode(hex_str) {
            Ok(bytes) => format!(
                "Data.B ({})",
                lean_bytestring_literal_from_bytes_qualified(&bytes)
            ),
            Err(_) => {
                push_widening(
                    unsupported_log,
                    TransitionWideningKind::DataFreshening,
                    format!(
                        "ByteArray literal with invalid hex freshened to opaque ByteString witness: {hex_str}"
                    ),
                );
                let name = ctx.fresh(&ShallowIrType::ByteArray);
                emit_bound_var_as_lean_data(&name, &ShallowIrType::ByteArray, ctx)
            }
        },
        ShallowIr::Var { name, ty } => {
            push_widening(
                unsupported_log,
                TransitionWideningKind::DataFreshening,
                format!(
                    "Out-of-scope Var freshened while emitting Data: {name} : {:?}",
                    ty
                ),
            );
            let fresh = ctx.fresh(ty);
            emit_bound_var_as_lean_data(&fresh, ty, ctx)
        }
        ShallowIr::BoundVar { name, ty } => emit_bound_var_as_lean_data(name, ty, ctx),
        ShallowIr::FuzzExistential { kind, .. } => {
            push_widening(
                unsupported_log,
                TransitionWideningKind::DataFreshening,
                format!("FuzzExistential freshened while emitting Data: {:?}", kind),
            );
            let fresh = ctx.fresh(kind);
            emit_bound_var_as_lean_data(&fresh, kind, ctx)
        }
        ShallowIr::Opaque { ty, reason, .. } => {
            push_widening(
                unsupported_log,
                TransitionWideningKind::DataFreshening,
                format!("Opaque Data node freshened: {reason}"),
            );
            let fresh = ctx.fresh(ty);
            emit_bound_var_as_lean_data(&fresh, ty, ctx)
        }
        ShallowIr::Construct { tag, fields, .. } => {
            let field_strs: Vec<String> = fields
                .iter()
                .map(|f| emit_shallow_ir_as_lean_data(f, ctx, unsupported_log))
                .collect();
            format!("Data.Constr ({tag} : Integer) [{}]", field_strs.join(", "))
        }
        ShallowIr::Tuple(elems) => {
            let parts: Vec<String> = elems
                .iter()
                .map(|e| emit_shallow_ir_as_lean_data(e, ctx, unsupported_log))
                .collect();
            format!("Data.List [{}]", parts.join(", "))
        }
        ShallowIr::ListLit { elements, tail, .. } => {
            let parts: Vec<String> = elements
                .iter()
                .map(|e| emit_shallow_ir_as_lean_data(e, ctx, unsupported_log))
                .collect();
            if let Some(tail) = tail {
                let tail_data = emit_shallow_ir_as_lean_data(tail, ctx, unsupported_log);
                let prefix_list = if parts.is_empty() {
                    "[]".to_string()
                } else {
                    format!("[{}]", parts.join(", "))
                };
                format!(
                    "(match {tail_data} with | Data.List tail => Data.List ({prefix_list} ++ tail) | _ => Data.List {prefix_list})"
                )
            } else {
                format!("Data.List [{}]", parts.join(", "))
            }
        }
        ShallowIr::Let { name, value, body } => {
            if name != "_" && shallow_ir_mentions_name(body, name) {
                push_widening(
                    unsupported_log,
                    TransitionWideningKind::DataFreshening,
                    format!(
                        "Let node widened while emitting Data; binding '{name}' is referenced in the body"
                    ),
                );
                return ctx.fresh(&ShallowIrType::Data);
            }

            let mut preview_ctx = ctx.clone();
            let Some(_value_expr) = try_emit_shallow_ir_as_lean_data_exact(value, &mut preview_ctx)
            else {
                push_widening(
                    unsupported_log,
                    TransitionWideningKind::DataFreshening,
                    format!(
                        "Let node widened while emitting Data; binding '{name}' not reified precisely"
                    ),
                );
                return ctx.fresh(&ShallowIrType::Data);
            };
            let Some(body_expr) = try_emit_shallow_ir_as_lean_data_exact(body, &mut preview_ctx)
            else {
                push_widening(
                    unsupported_log,
                    TransitionWideningKind::DataFreshening,
                    format!(
                        "Let node widened while emitting Data; binding '{name}' not reified precisely"
                    ),
                );
                return ctx.fresh(&ShallowIrType::Data);
            };
            *ctx = preview_ctx;
            body_expr
        }
        ShallowIr::FieldAccess {
            record,
            index,
            label,
            kind,
            ..
        } => emit_field_projection_as_lean_data(
            record,
            *index,
            label,
            *kind,
            ctx,
            unsupported_log,
            "field access",
        ),
        ShallowIr::RecordUpdate {
            record,
            tag,
            field_count,
            updates,
        } => {
            let mut fields = Vec::with_capacity(*field_count);
            for index in 0..*field_count {
                if let Some(update) = updates.iter().find(|update| update.index == index) {
                    fields.push(emit_shallow_ir_as_lean_data(
                        &update.value,
                        ctx,
                        unsupported_log,
                    ));
                } else {
                    fields.push(emit_field_projection_as_lean_data(
                        record,
                        index as u64,
                        &index.to_string(),
                        ShallowFieldAccessKind::ConstructorField,
                        ctx,
                        unsupported_log,
                        "record update",
                    ));
                }
            }
            format!("Data.Constr ({tag} : Integer) [{}]", fields.join(", "))
        }
        ShallowIr::If {
            cond,
            then_branch,
            else_branch,
        } => {
            let mut preview_ctx = ctx.clone();
            let Some(cond_expr) = try_emit_shallow_ir_as_lean_bool_exact(cond, &mut preview_ctx)
            else {
                push_widening(
                    unsupported_log,
                    TransitionWideningKind::DataFreshening,
                    "If node freshened while emitting Data".to_string(),
                );
                return ctx.fresh(&ShallowIrType::Data);
            };
            let Some(then_expr) =
                try_emit_shallow_ir_as_lean_data_exact(then_branch, &mut preview_ctx)
            else {
                push_widening(
                    unsupported_log,
                    TransitionWideningKind::DataFreshening,
                    "If node freshened while emitting Data".to_string(),
                );
                return ctx.fresh(&ShallowIrType::Data);
            };
            let Some(else_expr) =
                try_emit_shallow_ir_as_lean_data_exact(else_branch, &mut preview_ctx)
            else {
                push_widening(
                    unsupported_log,
                    TransitionWideningKind::DataFreshening,
                    "If node freshened while emitting Data".to_string(),
                );
                return ctx.fresh(&ShallowIrType::Data);
            };
            *ctx = preview_ctx;
            format!("(if {cond_expr} then {then_expr} else {else_expr})")
        }
        ShallowIr::Match { subject, arms } => {
            if let Some(expr) = emit_match_as_lean_data_exact(subject, arms, ctx) {
                expr
            } else {
                push_widening(
                    unsupported_log,
                    TransitionWideningKind::DataFreshening,
                    "Match node freshened while emitting Data",
                );
                ctx.fresh(&ShallowIrType::Data)
            }
        }
        ShallowIr::BinOp { op, left, right } => {
            if matches!(op, aiken_lang::test_framework::ShallowBinOp::Append) {
                let left_data = emit_shallow_ir_as_lean_data(left, ctx, unsupported_log);
                let right_data = emit_shallow_ir_as_lean_data(right, ctx, unsupported_log);
                match (shallow_ir_type_hint(left), shallow_ir_type_hint(right)) {
                    (Some(aiken_lang::test_framework::ShallowIrType::List(_)), _)
                    | (_, Some(aiken_lang::test_framework::ShallowIrType::List(_))) => {
                        format!(
                            "(match {left_data}, {right_data} with | Data.List xs, Data.List ys => Data.List (xs ++ ys) | _, _ => Data.List [])"
                        )
                    }
                    (Some(aiken_lang::test_framework::ShallowIrType::ByteArray), _)
                    | (_, Some(aiken_lang::test_framework::ShallowIrType::ByteArray))
                    | (Some(aiken_lang::test_framework::ShallowIrType::String), _)
                    | (_, Some(aiken_lang::test_framework::ShallowIrType::String)) => {
                        format!(
                            "(match {left_data}, {right_data} with | Data.B xs, Data.B ys => Data.B (xs ++ ys) | _, _ => Data.B ByteString.empty)"
                        )
                    }
                    _ => {
                        push_widening(
                            unsupported_log,
                            TransitionWideningKind::DataFreshening,
                            format!("BinOp node freshened while emitting Data: {:?}", op),
                        );
                        ctx.fresh(&ShallowIrType::Data)
                    }
                }
            } else if let Some(int_expr) = try_emit_shallow_ir_as_lean_integer_exact(ir, ctx) {
                format!("Data.I {int_expr}")
            } else {
                push_widening(
                    unsupported_log,
                    TransitionWideningKind::DataFreshening,
                    format!("BinOp node freshened while emitting Data: {:?}", op),
                );
                ctx.fresh(&ShallowIrType::Data)
            }
        }
        _ => {
            push_widening(
                unsupported_log,
                TransitionWideningKind::DataFreshening,
                "unknown ShallowIr shape freshened while emitting Data",
            );
            ctx.fresh(&ShallowIrType::Data)
        }
    }
}

fn emit_shallow_ir_as_lean_bool(
    ir: &aiken_lang::test_framework::ShallowIr,
    ctx: &mut ShallowIrEmitCtx,
    unsupported_log: &mut TransitionWideningLog,
) -> String {
    use aiken_lang::test_framework::{ShallowBinOp, ShallowConst, ShallowIr, ShallowIrType};

    match ir {
        ShallowIr::Const(ShallowConst::Bool(b)) => {
            if *b {
                "true".to_string()
            } else {
                "false".to_string()
            }
        }
        ShallowIr::BoundVar { name, ty } if matches!(ty, ShallowIrType::Bool) => {
            ctx.resolve_bound_name(name)
        }
        ShallowIr::FieldAccess { ty, .. } if matches!(ty, ShallowIrType::Bool) => {
            let data = emit_shallow_ir_as_lean_data(ir, ctx, unsupported_log);
            format!("decide ({data} = Data.Constr (1 : Integer) [])")
        }
        ShallowIr::BinOp { op, left, right } => match op {
            ShallowBinOp::And => {
                let l = emit_shallow_ir_as_lean_bool(left, ctx, unsupported_log);
                let r = emit_shallow_ir_as_lean_bool(right, ctx, unsupported_log);
                format!("({l} && {r})")
            }
            ShallowBinOp::Or => {
                let l = emit_shallow_ir_as_lean_bool(left, ctx, unsupported_log);
                let r = emit_shallow_ir_as_lean_bool(right, ctx, unsupported_log);
                format!("({l} || {r})")
            }
            ShallowBinOp::Eq | ShallowBinOp::NotEq => {
                let eq = if let (Some(l), Some(r)) = (
                    try_emit_shallow_ir_as_lean_integer_exact(left, ctx),
                    try_emit_shallow_ir_as_lean_integer_exact(right, ctx),
                ) {
                    format!("decide ({l} = {r})")
                } else {
                    let l = emit_shallow_ir_as_lean_data(left, ctx, unsupported_log);
                    let r = emit_shallow_ir_as_lean_data(right, ctx, unsupported_log);
                    format!("decide ({l} = {r})")
                };
                if matches!(op, ShallowBinOp::NotEq) {
                    format!("(!{eq})")
                } else {
                    eq
                }
            }
            ShallowBinOp::Lt | ShallowBinOp::LtEq | ShallowBinOp::Gt | ShallowBinOp::GtEq => {
                if let (Some(l), Some(r)) = (
                    try_emit_shallow_ir_as_lean_integer_exact(left, ctx),
                    try_emit_shallow_ir_as_lean_integer_exact(right, ctx),
                ) {
                    match op {
                        ShallowBinOp::Lt => format!("decide ({l} < {r})"),
                        ShallowBinOp::LtEq => format!("decide ({l} <= {r})"),
                        ShallowBinOp::Gt => format!("decide ({l} > {r})"),
                        ShallowBinOp::GtEq => format!("decide ({l} >= {r})"),
                        _ => unreachable!(),
                    }
                } else {
                    push_widening(
                        unsupported_log,
                        TransitionWideningKind::BoolFreshening,
                        format!("unsupported BinOp {:?} freshened while emitting Bool", op),
                    );
                    ctx.fresh(&ShallowIrType::Bool)
                }
            }
            _ => {
                push_widening(
                    unsupported_log,
                    TransitionWideningKind::BoolFreshening,
                    format!("unsupported BinOp {:?} freshened while emitting Bool", op),
                );
                ctx.fresh(&ShallowIrType::Bool)
            }
        },
        ShallowIr::Var { name, ty } => {
            push_widening(
                unsupported_log,
                TransitionWideningKind::BoolFreshening,
                format!(
                    "Out-of-scope Var freshened while emitting Bool: {name} : {:?}",
                    ty
                ),
            );
            ctx.fresh(&ShallowIrType::Bool)
        }
        _ => {
            push_widening(
                unsupported_log,
                TransitionWideningKind::BoolFreshening,
                format!("unsupported Bool shape freshened: {:?}", ir),
            );
            ctx.fresh(&ShallowIrType::Bool)
        }
    }
}

/// Emit a Lean `def {fn_name} (state transition : Data) : Prop := ...`
/// derived from the ShallowIr of the step function body. The body encodes
/// the shape `transition = <emitted Data expression>` and is wrapped in
/// existential binders for every fresh variable introduced during emission.
fn emit_step_fn_predicate(
    fn_name: &str,
    step_ir: &aiken_lang::test_framework::ShallowIr,
) -> (String, TransitionWideningLog) {
    let mut ctx = ShallowIrEmitCtx::new();
    let mut unsupported_log = Vec::new();
    let body_expr = emit_shallow_ir_as_lean_data(step_ir, &mut ctx, &mut unsupported_log);

    let mut existentials = String::new();
    for (name, ty) in &ctx.existentials {
        existentials.push_str(&format!(
            "  {EXISTS} ({name} : {ty}),\n",
            EXISTS = lean_sym::EXISTS,
        ));
    }

    (
        format!(
            "def {fn_name} (state : Data) (transition : Data) : Prop :=\n\
             {existentials}  transition = {body_expr}\n"
        ),
        unsupported_log,
    )
}

/// Try to emit `{step_fn_name}_sound` as a *proved theorem* rather than a
/// trusted axiom.
///
/// Succeeds when `step_ir` is a top-level `Construct` whose tag equals
/// `step_tag` — i.e. the step function returns exactly
/// `Data.Constr step_tag [f0, f1, …]`.  In that case the step predicate has
/// the form
///
/// ```text
/// ∃ _fuzz_0 … _fuzz_{n-1}, transition = Data.Constr step_tag [e0, …, e_{m-1}]
/// ```
///
/// and `{label_field}`, `{next_state_field}`, `{event_field}` are definitionally
/// `if tag = step_tag then fields.get? {index} else none`, which reduces via
/// `if_pos rfl` (the tag equality is `a = a`) and `List.get?_cons_zero` /
/// `List.get?_cons_succ` (standard `@[simp]` lemmas in Lean 4) to
/// `some e_{index}`.  The proof tactic sequence:
///
/// 1. `simp only [{step_fn_name}] at h` unfolds the predicate.
/// 2. `obtain ⟨v0, …, rfl⟩ := h` destructs all existentials and
///    substitutes `transition := Data.Constr step_tag [e0, …]`.
/// 3. `simp only [{field}, if_pos rfl, List.get?_cons_zero,
///    List.get?_cons_succ]` closes each field-extractor goal without
///    requiring `DecidableEq` on `PlutusCore.Integer` (the tag equality
///    is `step_tag = step_tag`, proved by `rfl`, not by `decide`).
///
/// Returns `None` when a sound theorem cannot be derived; the caller must
/// emit `S0001` `StepFnSoundAxiomEmitted` instead of falling back to a
/// trusted axiom (see commit "hard-error non-Construct step IRs").
///
/// `None` is returned when:
/// * `step_ir` is not a `Construct` (Opaque, If, Match, …).
/// * The `Construct.tag` differs from `step_tag` — the field extractor's
///   `if tag = step_tag then … else none` guard would not reduce.
/// * Any field index is out of bounds.
#[allow(clippy::too_many_arguments)]
fn try_emit_step_fn_sound_as_theorem(
    step_fn_name: &str,
    step_ir: &aiken_lang::test_framework::ShallowIr,
    step_tag: u64,
    label_index: usize,
    next_state_index: usize,
    event_index: usize,
    label_field: &str,
    next_state_field: &str,
    event_field: &str,
) -> Option<String> {
    use aiken_lang::test_framework::ShallowIr;

    // Only applicable when the IR is a top-level Construct with the correct tag.
    let ShallowIr::Construct {
        tag: ir_tag,
        fields,
        ..
    } = step_ir
    else {
        return None;
    };
    // Tag mismatch: the field extractor's `if tag = step_tag then … else none`
    // guard would reduce to `none`, making the proof goal `none = some _` —
    // unprovable.  Return None; the caller must emit `S0001`.
    if *ir_tag != step_tag {
        return None;
    }
    let n_fields = fields.len();
    if label_index >= n_fields || next_state_index >= n_fields || event_index >= n_fields {
        return None;
    }

    // Re-emit each field with a fresh context.  The counter starts at 0 just
    // as `emit_step_fn_predicate` does, so the variable names (_fuzz_0,
    // _fuzz_1, …) are identical to those in the emitted `def`.
    // Invariant: `emit_shallow_ir_as_lean_data` is deterministic for a given
    // IR subtree; two fresh runs over the same fields produce the same names.
    let mut ctx = ShallowIrEmitCtx::new();
    let mut unsupported_log = Vec::new();
    let mut field_exprs: Vec<String> = Vec::new();
    for field in fields {
        let expr = emit_shallow_ir_as_lean_data(field, &mut ctx, &mut unsupported_log);
        field_exprs.push(expr);
    }

    // `obtain` destructs all existentials and uses `rfl` for the final equality.
    let all_vars: Vec<&str> = ctx.existentials.iter().map(|(n, _)| n.as_str()).collect();
    let obtain_pattern = if all_vars.is_empty() {
        "rfl".to_string()
    } else {
        format!("{}, rfl", all_vars.join(", "))
    };

    // Witnesses for `∃ nextState label event`.
    // Order in the theorem body: nextState first, label second, event third.
    let next_state_witness = &field_exprs[next_state_index];
    let label_witness = &field_exprs[label_index];
    let event_witness = &field_exprs[event_index];

    // Each `by simp only [field, if_pos rfl, List.get?_cons_zero,
    // List.get?_cons_succ]` sub-proof:
    //   1. Unfolds `field` to its pattern-match body.
    //   2. `if_pos rfl` reduces `if step_tag = step_tag then … else none`
    //      to `…` — works because both sides are the same literal (a = a).
    //   3. `List.get?_cons_zero` / `_cons_succ` reduce `[e0, …].get? i`
    //      to `some e_i` by repeated unfolding of the concrete index.
    // This avoids any dependency on `DecidableEq PlutusCore.Integer`.
    let simp_field = "if_pos rfl, List.get?_cons_zero, List.get?_cons_succ".to_string();

    Some(format!(
        "-- step_fn body is a top-level Construct (tag={step_tag}): soundness is\n\
         -- provable by simp without `DecidableEq` on `PlutusCore.Integer`.\n\
         -- The tag guard reduces via `if_pos rfl` (same literal on both sides);\n\
         -- list indexing reduces via `List.get?_cons_zero`/`_cons_succ`.\n\
         private theorem {step_fn_name}_sound :\n  \
         {FORALL} (_state transition : Data),\n  \
         {step_fn_name} _state transition {IMPLIES}\n  \
         {EXISTS} nextState label event,\n    \
         {label_field} transition = some label {AND}\n    \
         {next_state_field} transition = some nextState {AND}\n    \
         {event_field} transition = some event := by\n  \
         intro _state transition h\n  \
         simp only [{step_fn_name}] at h\n  \
         obtain \u{27E8}{obtain_pattern}\u{27E9} := h\n  \
         exact \u{27E8}{next_state_witness}, {label_witness}, {event_witness},\n    \
           by simp only [{label_field}, {simp_field}],\n    \
           by simp only [{next_state_field}, {simp_field}],\n    \
           by simp only [{event_field}, {simp_field}]\u{27E9}\n",
        FORALL = lean_sym::FORALL,
        IMPLIES = lean_sym::IMPLIES,
        EXISTS = lean_sym::EXISTS,
        AND = lean_sym::AND,
    ))
}

/// Classifies the top-level shape of a `ShallowIr` for diagnostic purposes.
/// Returned strings appear in `S0001` error messages; they are stable enough
/// to be matched in tests but not part of any user-visible serialised API.
///
/// Future `ShallowIr` variants are reported as `Unknown` until the verifier
/// gives them an explicit shape label.
fn describe_step_ir_shape(ir: &aiken_lang::test_framework::ShallowIr) -> &'static str {
    use aiken_lang::test_framework::ShallowIr;
    match ir {
        ShallowIr::Const(_) => "Const",
        ShallowIr::Var { .. } => "Var",
        ShallowIr::BoundVar { .. } => "BoundVar",
        ShallowIr::Let { .. } => "Let",
        ShallowIr::If { .. } => "If",
        ShallowIr::Match { .. } => "Match",
        ShallowIr::Construct { .. } => "Construct",
        ShallowIr::FieldAccess { .. } => "FieldAccess",
        ShallowIr::RecordUpdate { .. } => "RecordUpdate",
        ShallowIr::BinOp { .. } => "BinOp",
        ShallowIr::Tuple(_) => "Tuple",
        ShallowIr::ListLit { .. } => "ListLit",
        ShallowIr::FuzzExistential { .. } => "FuzzExistential",
        ShallowIr::Opaque { .. } => "Opaque",
        _ => "Unknown",
    }
}

/// S4 — emit a `TransitionProp` as a Lean `Prop` expression.
///
/// `state_var` and `transition_var` are the names of the outer binders the
/// caller has already opened (typically `"state"` and `"transition"`).
/// `ctx` tracks fresh existential variables introduced during emission.
/// Every constraint that cannot be lowered is replaced by `True` (sound
/// widening) and appended to `unsupported_log` for the audit comment block.
///
/// Current status (S4): this is still an intentional over-approximation, but
/// the counted compromise is now surfaced structurally. `Pure` conditions emit
/// precisely for the supported Bool subset; unsupported Bool/Data/Domain cases
/// freshen with an explicit widening record instead of a silent `True` fallthrough.
/// conditions have a faithful Bool encoding.
fn transition_domain_predicate(
    binder: &str,
    binder_ty: &aiken_lang::test_framework::ShallowIrType,
    domain: &aiken_lang::test_framework::FuzzerSemantics,
    unsupported_log: &mut TransitionWideningLog,
    domain_shapes: &mut TransitionDomainShapeState<'_>,
) -> Option<String> {
    use aiken_lang::test_framework::{
        FuzzerExactValue as LangFuzzerExactValue, FuzzerSemantics as LangFuzzerSemantics,
        ShallowIrType as LangShallowIrType,
    };

    let join_and = |parts: Vec<String>| match parts.len() {
        0 => None,
        1 => parts.into_iter().next(),
        _ => Some(format!(
            "({})",
            parts.join(&format!(" {AND} ", AND = lean_sym::AND))
        )),
    };
    let is_data_like = matches!(
        binder_ty,
        LangShallowIrType::Data | LangShallowIrType::Unknown | LangShallowIrType::Adt(_)
    );

    match domain {
        LangFuzzerSemantics::IntRange { min, max } => {
            let int_pred = match (min, max) {
                (Some(min), Some(max)) => Some(format!(
                    "({min} <= VALUE {AND} VALUE <= {max})",
                    AND = lean_sym::AND
                )),
                (Some(min), None) => Some(format!("({min} <= VALUE)")),
                (None, Some(max)) => Some(format!("(VALUE <= {max})")),
                (None, None) => None,
            };
            match binder_ty {
                LangShallowIrType::Int => int_pred.map(|pred| pred.replace("VALUE", binder)),
                _ if is_data_like => int_pred.map(|pred| {
                    format!(
                        "(match {binder} with | Data.I value => {} | _ => False)",
                        pred.replace("VALUE", "value")
                    )
                }),
                _ => Some("False".to_string()),
            }
        }
        LangFuzzerSemantics::ByteArrayRange { min_len, max_len } => {
            let len_pred = match (min_len, max_len) {
                (Some(min), Some(max)) => Some(format!(
                    "({min} <= VALUE.length {AND} VALUE.length <= {max})",
                    AND = lean_sym::AND
                )),
                (Some(min), None) => Some(format!("({min} <= VALUE.length)")),
                (None, Some(max)) => Some(format!("(VALUE.length <= {max})")),
                (None, None) => None,
            };
            match binder_ty {
                LangShallowIrType::ByteArray | LangShallowIrType::String => {
                    len_pred.map(|pred| pred.replace("VALUE", binder))
                }
                _ if is_data_like => len_pred.map(|pred| {
                    format!(
                        "(match {binder} with | Data.B value => {} | _ => False)",
                        pred.replace("VALUE", "value")
                    )
                }),
                _ => Some("False".to_string()),
            }
        }
        LangFuzzerSemantics::List {
            element,
            min_len,
            max_len,
        } => match binder_ty {
            LangShallowIrType::List(elem_ty) => {
                let mut parts = Vec::new();
                match (min_len, max_len) {
                    (Some(min), Some(max)) => parts.push(format!(
                        "({min} <= {binder}.length {AND} {binder}.length <= {max})",
                        AND = lean_sym::AND
                    )),
                    (Some(min), None) => parts.push(format!("({min} <= {binder}.length)")),
                    (None, Some(max)) => parts.push(format!("({binder}.length <= {max})")),
                    (None, None) => {}
                }
                if let Some(elem_predicate) = transition_domain_predicate(
                    "elem",
                    elem_ty.as_ref(),
                    element.as_ref(),
                    unsupported_log,
                    domain_shapes,
                ) {
                    parts.push(format!(
                        "AikenVerify.Utils.allProp (fun elem => {elem_predicate}) {binder}"
                    ));
                }
                join_and(parts)
            }
            _ if is_data_like => {
                let mut parts = Vec::new();
                match (min_len, max_len) {
                    (Some(min), Some(max)) => parts.push(format!(
                        "({min} <= xs.length {AND} xs.length <= {max})",
                        AND = lean_sym::AND
                    )),
                    (Some(min), None) => parts.push(format!("({min} <= xs.length)")),
                    (None, Some(max)) => parts.push(format!("(xs.length <= {max})")),
                    (None, None) => {}
                }
                if let Some(elem_predicate) = transition_domain_predicate(
                    "elem",
                    &LangShallowIrType::Data,
                    element.as_ref(),
                    unsupported_log,
                    domain_shapes,
                ) {
                    parts.push(format!(
                        "AikenVerify.Utils.allProp (fun elem => {elem_predicate}) xs"
                    ));
                }
                Some(format!(
                    "(match {binder} with | Data.List xs => {} | _ => False)",
                    join_and(parts).unwrap_or_else(|| "True".to_string())
                ))
            }
            _ => Some("False".to_string()),
        },
        LangFuzzerSemantics::Exact(LangFuzzerExactValue::Bool(value)) => match binder_ty {
            LangShallowIrType::Bool => Some(format!(
                "{binder} = {}",
                if *value { "true" } else { "false" }
            )),
            _ if is_data_like => Some(format!(
                "{binder} = Data.Constr ({} : Integer) []",
                if *value { 1 } else { 0 }
            )),
            _ => Some("False".to_string()),
        },
        LangFuzzerSemantics::Exact(LangFuzzerExactValue::ByteArray(bytes)) => {
            let lit = lean_bytestring_literal_from_bytes_qualified(bytes);
            match binder_ty {
                LangShallowIrType::ByteArray | LangShallowIrType::String => {
                    Some(format!("{binder} = {lit}"))
                }
                _ if is_data_like => Some(format!("{binder} = Data.B ({lit})")),
                _ => Some("False".to_string()),
            }
        }
        LangFuzzerSemantics::Exact(LangFuzzerExactValue::String(value)) => {
            let lit = lean_bytestring_literal_from_bytes_qualified(value.as_bytes());
            match binder_ty {
                LangShallowIrType::String | LangShallowIrType::ByteArray => {
                    Some(format!("{binder} = {lit}"))
                }
                _ if is_data_like => Some(format!("{binder} = Data.B ({lit})")),
                _ => Some("False".to_string()),
            }
        }
        LangFuzzerSemantics::OneOf(values) => {
            let disjuncts: Vec<String> = values
                .iter()
                .filter_map(|value| match value {
                    LangFuzzerExactValue::Bool(b) => Some(match binder_ty {
                        LangShallowIrType::Bool => {
                            format!("{binder} = {}", if *b { "true" } else { "false" })
                        }
                        _ if is_data_like => format!(
                            "{binder} = Data.Constr ({} : Integer) []",
                            if *b { 1 } else { 0 }
                        ),
                        _ => "False".to_string(),
                    }),
                    LangFuzzerExactValue::ByteArray(bytes) => {
                        let lit = lean_bytestring_literal_from_bytes_qualified(bytes);
                        Some(match binder_ty {
                            LangShallowIrType::ByteArray | LangShallowIrType::String => {
                                format!("{binder} = {lit}")
                            }
                            _ if is_data_like => format!("{binder} = Data.B ({lit})"),
                            _ => "False".to_string(),
                        })
                    }
                    LangFuzzerExactValue::String(value) => {
                        let lit = lean_bytestring_literal_from_bytes_qualified(value.as_bytes());
                        Some(match binder_ty {
                            LangShallowIrType::String | LangShallowIrType::ByteArray => {
                                format!("{binder} = {lit}")
                            }
                            _ if is_data_like => format!("{binder} = Data.B ({lit})"),
                            _ => "False".to_string(),
                        })
                    }
                    _ => {
                        push_widening(
                            unsupported_log,
                            TransitionWideningKind::Domain,
                            "non-scalar OneOf existential domain widened to True",
                        );
                        None
                    }
                })
                .collect();
            if disjuncts.is_empty() {
                push_widening(
                    unsupported_log,
                    TransitionWideningKind::Domain,
                    "empty OneOf existential domain widened to True",
                );
                None
            } else {
                Some(format!("({})", disjuncts.join(" ∨ ")))
            }
        }
        LangFuzzerSemantics::Constructors { tags } => {
            if let Some(predicate) = data_constructor_tags_predicate(binder, tags) {
                Some(predicate)
            } else {
                push_widening(
                    unsupported_log,
                    TransitionWideningKind::Domain,
                    "empty constructor-tag existential domain widened to True",
                );
                None
            }
        }
        LangFuzzerSemantics::Bool => {
            if is_data_like {
                Some(format!(
                    "({binder} = Data.Constr (0 : Integer) [] ∨ {binder} = Data.Constr (1 : Integer) [])"
                ))
            } else {
                None
            }
        }
        LangFuzzerSemantics::String => {
            if is_data_like {
                Some(format!(
                    "(match {binder} with | Data.B _ => True | _ => False)"
                ))
            } else {
                None
            }
        }
        LangFuzzerSemantics::Data => None,
        LangFuzzerSemantics::DataWithSchema { type_name } => domain_shapes
            .predicate_for_data_with_schema(type_name)
            .map(|predicate| format!("{predicate} {binder}"))
            .or_else(|| {
                push_widening(
                    unsupported_log,
                    TransitionWideningKind::Domain,
                    format!("DataWithSchema<{type_name}> existential domain widened to True"),
                );
                None
            }),
        LangFuzzerSemantics::Product(items) => match binder_ty {
            LangShallowIrType::Pair(fst_ty, snd_ty) if items.len() == 2 => {
                let mut parts = Vec::new();
                if let Some(predicate) = transition_domain_predicate(
                    &format!("({binder}.1)"),
                    fst_ty.as_ref(),
                    &items[0],
                    unsupported_log,
                    domain_shapes,
                ) {
                    parts.push(predicate);
                }
                if let Some(predicate) = transition_domain_predicate(
                    &format!("({binder}.2)"),
                    snd_ty.as_ref(),
                    &items[1],
                    unsupported_log,
                    domain_shapes,
                ) {
                    parts.push(predicate);
                }
                join_and(parts)
            }
            _ if is_data_like => {
                let vars: Vec<String> = (0..items.len()).map(|i| format!("field_{i}")).collect();
                let mut parts = Vec::new();
                for (index, item) in items.iter().enumerate() {
                    if let Some(predicate) = transition_domain_predicate(
                        &vars[index],
                        &LangShallowIrType::Data,
                        item,
                        unsupported_log,
                        domain_shapes,
                    ) {
                        parts.push(predicate);
                    }
                }
                Some(format!(
                    "(match {binder} with | Data.List [{}] => {} | _ => False)",
                    vars.join(", "),
                    join_and(parts).unwrap_or_else(|| "True".to_string())
                ))
            }
            _ => Some("False".to_string()),
        },
        LangFuzzerSemantics::Opaque { reason } => {
            push_widening(
                unsupported_log,
                TransitionWideningKind::Domain,
                format!("Opaque existential domain widened to True: {reason}"),
            );
            None
        }
        LangFuzzerSemantics::StateMachineTrace { .. } => {
            push_widening(
                unsupported_log,
                TransitionWideningKind::Domain,
                "nested state-machine domain widened to True",
            );
            None
        }
        _ => {
            push_widening(
                unsupported_log,
                TransitionWideningKind::Domain,
                "unknown existential domain widened to True",
            );
            None
        }
    }
}

fn emit_transition_prop_as_lean(
    prop: &aiken_lang::test_framework::TransitionProp,
    state_var: &str,
    transition_var: &str,
    // The variable name most recently bound by an enclosing `Exists` binder.
    // `SubGenerator` nodes emit `__SGPFX__{name}_prop state binding_var`,
    // where `binding_var` is the fork-output variable in scope at the call site.
    binding_var: &str,
    ctx: &mut ShallowIrEmitCtx,
    unsupported_log: &mut TransitionWideningLog,
    domain_shapes: &mut TransitionDomainShapeState<'_>,
) -> String {
    use aiken_lang::test_framework::TransitionProp;

    match prop {
        TransitionProp::Pure(ir) => {
            let cond = emit_shallow_ir_as_lean_bool(ir, ctx, unsupported_log);
            format!("({cond} = true)")
        }
        TransitionProp::Exists {
            binder,
            ty,
            domain,
            body,
        } => {
            let lean_ty = shallow_ir_type_to_lean(ty);
            let unique = ctx.unique_binder(binder);
            ctx.push_binder(binder, &unique);
            let inner = emit_transition_prop_as_lean(
                body,
                state_var,
                transition_var,
                &unique,
                ctx,
                unsupported_log,
                domain_shapes,
            );
            ctx.pop_binder(binder);
            let inner = if let Some(domain_predicate) = transition_domain_predicate(
                &unique,
                ty,
                domain.as_ref(),
                unsupported_log,
                domain_shapes,
            ) {
                format!("({domain_predicate} {AND} {inner})", AND = lean_sym::AND)
            } else {
                inner
            };
            let printed = if unique.starts_with('_') || ident_is_used(&inner, &unique) {
                unique.clone()
            } else {
                format!("_{unique}")
            };
            format!(
                "({EXISTS} ({printed} : {lean_ty}), {inner})",
                EXISTS = lean_sym::EXISTS,
            )
        }
        TransitionProp::And(parts) => {
            if parts.is_empty() {
                "True".to_string()
            } else {
                let inner: Vec<String> = parts
                    .iter()
                    .map(|p| {
                        emit_transition_prop_as_lean(
                            p,
                            state_var,
                            transition_var,
                            binding_var,
                            ctx,
                            unsupported_log,
                            domain_shapes,
                        )
                    })
                    .collect();
                format!("({})", inner.join(&format!(" {AND} ", AND = lean_sym::AND)))
            }
        }
        TransitionProp::Or(branches) => {
            if branches.is_empty() {
                // An empty disjunction is `False`, but that would make the
                // precondition unsatisfiable and is never useful here.
                // Widen to `True` instead and log.
                push_widening(
                    unsupported_log,
                    TransitionWideningKind::Relation,
                    "empty Or(branches) widened to True (S4)",
                );
                "True".to_string()
            } else {
                let inner: Vec<String> = branches
                    .iter()
                    .map(|p| {
                        emit_transition_prop_as_lean(
                            p,
                            state_var,
                            transition_var,
                            binding_var,
                            ctx,
                            unsupported_log,
                            domain_shapes,
                        )
                    })
                    .collect();
                format!("({})", inner.join(" ∨ "))
            }
        }
        TransitionProp::IfThenElse { cond, t, e } => {
            let cond_str = emit_shallow_ir_as_lean_bool(cond, ctx, unsupported_log);
            let t_prop = emit_transition_prop_as_lean(
                t,
                state_var,
                transition_var,
                binding_var,
                ctx,
                unsupported_log,
                domain_shapes,
            );
            let e_prop = emit_transition_prop_as_lean(
                e,
                state_var,
                transition_var,
                binding_var,
                ctx,
                unsupported_log,
                domain_shapes,
            );
            format!(
                "(({cond_str} = true {AND} {t_prop}) ∨ ({cond_str} = false {AND} {e_prop}))",
                AND = lean_sym::AND,
            )
        }
        TransitionProp::Match { scrutinee: _, arms } => {
            push_widening(
                unsupported_log,
                TransitionWideningKind::Relation,
                "Match scrutinee widened to disjunction over arm bodies (S4)",
            );
            if arms.is_empty() {
                "True".to_string()
            } else {
                let inner: Vec<String> = arms
                    .iter()
                    .map(|arm| {
                        emit_transition_prop_as_lean(
                            &arm.body,
                            state_var,
                            transition_var,
                            binding_var,
                            ctx,
                            unsupported_log,
                            domain_shapes,
                        )
                    })
                    .collect();
                format!("({})", inner.join(" ∨ "))
            }
        }
        TransitionProp::EqOutput(ir) => {
            let mut local = ctx.fork();
            let rhs = emit_shallow_ir_as_lean_data(ir, &mut local, unsupported_log);
            ctx.fresh_counter = local.fresh_counter;
            ctx.used_names = local.used_names.clone();
            ctx.binder_counters = local.binder_counters.clone();
            let eq = format!("{transition_var} = {rhs}");
            local
                .existentials
                .iter()
                .rev()
                .fold(eq, |inner, (name, ty)| {
                    let printed = if name.starts_with('_') || ident_is_used(&inner, name) {
                        name.clone()
                    } else {
                        format!("_{name}")
                    };
                    format!(
                        "({EXISTS} ({printed} : {ty}), {inner})",
                        EXISTS = lean_sym::EXISTS
                    )
                })
        }
        TransitionProp::SubGenerator { module: _, fn_name } => {
            let sanitized = sanitize_lean_ident(fn_name);
            format!("(__SGPFX__{sanitized}_prop {state_var} {binding_var})")
        }
        TransitionProp::Unsupported {
            reason,
            source_location,
        } => {
            let src_suffix = source_location
                .as_deref()
                .map(|s| format!(" @ {s}"))
                .unwrap_or_default();
            push_widening(
                unsupported_log,
                TransitionWideningKind::Relation,
                format!("Unsupported: {reason}{src_suffix}"),
            );
            "True".to_string()
        }
    }
}

/// Return `true` when the `isValidTransition` definition body has been
/// widened to the trivial predicate `True`, making the two-phase theorem
/// vacuously provable and therefore misleading.
///
/// **M3 — drift sentinel only.**  This function used to be the production
/// vacuity gate, called from `try_generate_two_phase_proof` before any
/// proof was emitted.  As of the M3 commit, the production gate is the
/// AST-level `aiken_lang::test_framework::transition_prop_is_vacuous`,
/// pre-computed at export time and threaded through `is_vacuous` on
/// `ExportedTransitionProp`.  This text-based predicate is preserved
/// behind `#[cfg(test)]` purely so that the regression test
/// `transition_body_is_vacuous_detects_true` continues to pin its
/// historical decisions, and so the drift-sentinel test
/// `structural_vacuity_agrees_with_text_drift_sentinel` can compare the
/// two implementations on a representative corpus.
///
/// A body is vacuous when — after stripping the `def ... :=` header and
/// any leading `∃` quantifiers introduced by top-level existentials — the
/// remaining text is `True` (with optional surrounding whitespace/parens).
///
/// Note: `∃ (_x : Data), True` is *also* vacuous; the existential adds no
/// information when its body is `True`.  Bare `(True)` (parenthesised) is
/// handled via the recursive strip.
///
/// `True ∧ X` or `True ∨ X` are NOT flagged as vacuous — partial widening
/// is still informative.  Only a fully-degenerate body triggers this guard.
///
/// INVARIANT: `emit_transition_prop_as_lean` always emits literal `"True"`
/// (capital T) for widened nodes.  If that ever changes, update this check.
#[cfg(test)]
fn transition_body_is_vacuous(def_text: &str) -> bool {
    // Strip the `def ... :=` header.
    // ASSUMPTION: `emit_transition_prop_as_lean` never emits `:=` inside the
    // body (no `let` bindings are generated).  If that changes, `split_once`
    // would truncate the body at the first inner `:=`, producing a false
    // negative (not vacuous when body actually is) — safe but worth fixing.
    let body = match def_text.split_once(":=") {
        Some((_, rhs)) => rhs,
        None => return false,
    };
    is_vacuous_lean_prop(body.trim())
}

/// Recursive helper: return `true` iff `s` is a Lean Prop expression that
/// is syntactically equivalent to `True`.
///
/// `#[cfg(test)]` for the same reason as `transition_body_is_vacuous`:
/// production code consults `ExportedTransitionProp::is_vacuous` instead.
#[cfg(test)]
fn is_vacuous_lean_prop(s: &str) -> bool {
    let s = s.trim();
    // Literal `True`
    if s == "True" {
        return true;
    }
    // Parenthesised: `(True)`
    if s.starts_with('(') && s.ends_with(')') {
        return is_vacuous_lean_prop(&s[1..s.len() - 1]);
    }
    // Leading existential: `∃ (...) ,` followed by the body.
    // The `∃` symbol is U+2203 (3-byte UTF-8: \xe2\x88\x83).
    if s.starts_with('\u{2203}') {
        // Find the comma that ends the binder list and recurse on the tail.
        // ASSUMPTION: binder types produced by `shallow_ir_type_to_lean` contain
        // no top-level commas (`Data`, `Int`, `Bool`, `List (Data × Int)`, etc.).
        // If a future emitter introduces a type with a comma (e.g. multi-param
        // generics), `find(',')` would stop too early — a false negative, safe.
        if let Some(comma_pos) = s.find(',') {
            return is_vacuous_lean_prop(&s[comma_pos + 1..]);
        }
    }
    false
}

/// Return whether `ident` appears as a whole-word token inside `haystack`.
/// Used to decide whether to prefix an `∃` binder with `_` (unused-variable
/// suppression for Lean 4).  We treat word boundaries as: any character that
/// is NOT `[A-Za-z0-9_]`.  Identifiers in generated Lean are collision-free
/// (via `unique_binder`) so a substring check suffices.
fn ident_is_used(haystack: &str, ident: &str) -> bool {
    if ident.is_empty() {
        return false;
    }
    let bytes = haystack.as_bytes();
    let ib = ident.as_bytes();
    let n = ib.len();
    let h = bytes.len();
    if h < n {
        return false;
    }
    let is_word_char = |b: u8| b.is_ascii_alphanumeric() || b == b'_';
    (0..=(h - n)).any(|i| {
        if &bytes[i..i + n] != ib {
            return false;
        }
        let before_ok = i == 0 || !is_word_char(bytes[i - 1]);
        let after_ok = i + n == h || !is_word_char(bytes[i + n]);
        before_ok && after_ok
    })
}

/// Sanitize an Aiken identifier for use in a Lean name: replace characters
/// that are not alphanumeric or `_` with `_`.
fn sanitize_lean_ident(s: &str) -> String {
    s.chars()
        .map(|c| {
            if c.is_ascii_alphanumeric() || c == '_' {
                c
            } else {
                '_'
            }
        })
        .collect()
}

/// S4 — emit a ShallowIr expression as a Lean `Data` literal with any
/// accumulated existentials bound in the surrounding scope. Public to
/// `crate` so `lib.rs` can pre-emit the initial-state literal for
/// `isValidTrace` at export time.
pub(crate) fn emit_initial_state_as_lean(
    ir: &aiken_lang::test_framework::ShallowIr,
) -> (String, Vec<(String, String)>, TransitionWideningLog) {
    let mut ctx = ShallowIrEmitCtx::new();
    let mut unsupported_log = Vec::new();
    let body = emit_shallow_ir_as_lean_data(ir, &mut ctx, &mut unsupported_log);
    (body, ctx.existentials, unsupported_log)
}

#[cfg(test)]
type TransitionDefForExport = (
    String,
    Vec<String>,
    Vec<(String, String)>,
    Option<(String, String)>,
);

type TransitionDefDetailsForExport = (
    String,
    Vec<TransitionWidening>,
    Vec<(String, String)>,
    Option<(String, String)>,
);

#[cfg(test)]
/// S4/S6 — public wrapper around `emit_is_valid_transition_def` for use by
/// tests and legacy callers that still consume the flat string log.
pub(crate) fn emit_is_valid_transition_def_for_export(
    fn_name: &str,
    prop: &aiken_lang::test_framework::TransitionProp,
) -> TransitionDefForExport {
    let (def, widenings, sub_gens, s0002_marker) =
        emit_is_valid_transition_details_for_export(fn_name, prop);
    (def, widening_messages(&widenings), sub_gens, s0002_marker)
}

/// S4/S6 — structured export helper for use by `lib.rs` at export time.
/// Returns the full Lean `def` text, the structured widening log, the list
/// of sub-generators referenced, and the first S0002
/// (`ConstructorTagUnresolved`) marker found while walking the source
/// `TransitionProp`.
#[cfg(test)]
pub(crate) fn emit_is_valid_transition_details_for_export(
    fn_name: &str,
    prop: &aiken_lang::test_framework::TransitionProp,
) -> TransitionDefDetailsForExport {
    emit_is_valid_transition_details_for_export_with_schemas(
        fn_name,
        prop,
        "transition_prop",
        &BTreeMap::new(),
    )
}

pub(crate) fn emit_is_valid_transition_details_for_export_with_schemas(
    fn_name: &str,
    prop: &aiken_lang::test_framework::TransitionProp,
    test_name: &str,
    inner_data_schemas: &BTreeMap<String, ExportedDataSchema>,
) -> TransitionDefDetailsForExport {
    let mut widenings = Vec::new();
    let mut domain_shapes = TransitionDomainShapeState::enabled(
        test_name,
        format!("{fn_name}_shape"),
        inner_data_schemas,
    );
    let mut def = emit_is_valid_transition_def(fn_name, prop, &mut widenings, &mut domain_shapes);
    let domain_defs = domain_shapes.take_definitions();
    if !domain_defs.trim().is_empty() {
        def = format!("{domain_defs}\n\n{def}");
    }
    let sub_gens = aiken_lang::test_framework::collect_sub_generators_from_prop(prop);
    let s0002_marker = aiken_lang::test_framework::find_first_typed_opaque_in_transition_prop(prop)
        .and_then(|code| match code {
            aiken_lang::test_framework::OpaqueCode::ConstructorTagUnresolved {
                ctor,
                type_name,
            } => Some((ctor, type_name)),
            _ => None,
        });
    (def, widenings, sub_gens, s0002_marker)
}

/// S4 — emit the full body of `isValidTransition` (existentials + body)
/// derived from a `TransitionProp`. Returns the full `def ... := ...` text.
fn emit_is_valid_transition_def(
    fn_name: &str,
    prop: &aiken_lang::test_framework::TransitionProp,
    unsupported_log: &mut TransitionWideningLog,
    domain_shapes: &mut TransitionDomainShapeState<'_>,
) -> String {
    let mut ctx = ShallowIrEmitCtx::new();
    // Initial `binding_var` is `"transition"` — the top-level output of the
    // transition. If a `SubGenerator` appears directly at the top level (rare),
    // it will emit `{prefix}_{name}_prop state transition`.
    let body = emit_transition_prop_as_lean(
        prop,
        "state",
        "transition",
        "transition",
        &mut ctx,
        unsupported_log,
        domain_shapes,
    );

    let mut existentials = String::new();
    for (name, ty) in &ctx.existentials {
        existentials.push_str(&format!(
            "  {EXISTS} ({name} : {ty}),\n",
            EXISTS = lean_sym::EXISTS,
        ));
    }

    format!(
        "def {fn_name} (state : Data) (transition : Data) : Prop :=\n\
         {existentials}  {body}\n"
    )
}

/// S5 — emit the two-phase halt theorems.
///
/// Phase 1 (`_valid_input`) is proved by construction once `isValidScriptContext`
/// is defined as the image of `isValidTrace` under `packTrace` (see §S5). The
/// term proof `fun trace h => ⟨trace, h, rfl⟩` witnesses the existential trivially.
/// S5 — emit the universal halt theorem over exact traces.
///
/// For supported `scenario.ok(...)` proofs we ultimately need the theorem over
/// traces, not an intermediate existential `ScriptContext` image theorem. The
/// direct trace theorem keeps the quantified object concrete (`trace : List Data`)
/// and avoids the extra existential layer in `isValidScriptContext`, which is
/// friendlier to Blaster while still expressing the exact property we want.
fn emit_two_phase_halt_theorems(
    helper_prefix: &str,
    lean_test_name: &str,
    verify_prog: &str,
) -> String {
    format!(
        "theorem {lean_test_name} :\n  \
           {FORALL} (trace : List Data),\n    \
           {helper_prefix}_isValidTrace trace {IMPLIES}\n    \
           proveTestsHalt {verify_prog} (dataArg ({helper_prefix}_packTrace trace)) := by\n  \
           blaster\n",
        FORALL = lean_sym::FORALL,
        IMPLIES = lean_sym::IMPLIES,
    )
}

/// Try to dispatch a hard `S0002` `ConstructorTagUnresolved` error from a
/// `ShallowIr` that may carry the marker. The walk is recursive: we look
/// for the marker at the top level *and* nested inside structural shapes
/// (e.g. `Construct { fields: [..., Opaque{S0002}] }`), because the
/// upstream filter `shallow_ir_is_vacuous` only rejects pure-`Opaque`
/// roots, so a marker buried inside a `Construct` field can still reach
/// this dispatcher.
///
/// Commit 18 retired the legacy `S0002_REASON_PREFIX` string-prefix
/// bridge that this dispatcher used to consume; the producer now emits a
/// typed `OpaqueCode::ConstructorTagUnresolved { ctor, type_name }` and
/// the walker `find_first_typed_opaque_in_shallow_ir` returns the typed
/// payload directly. We `match` on it here.
///
/// Returns `Some(Err(...))` when a marker is found, `None` otherwise. The
/// caller substitutes its own `test_name` so the error attribution is
/// accurate at the dispatch site.
fn try_dispatch_s0002_from_shallow_ir(
    test_name: &str,
    ir: &aiken_lang::test_framework::ShallowIr,
) -> Option<miette::Report> {
    let code = aiken_lang::test_framework::find_first_typed_opaque_in_shallow_ir(ir)?;
    match code {
        aiken_lang::test_framework::OpaqueCode::ConstructorTagUnresolved { ctor, type_name } => {
            Some(miette::Report::new(error_catalogue::unsupported(
                "S0002",
                UnsupportedReason::ConstructorTagUnresolved {
                    test_name: test_name.to_string(),
                    ctor: ctor.clone(),
                    type_name: type_name.clone(),
                },
            )))
        }
        _ => None,
    }
}

#[allow(clippy::too_many_arguments)]
fn build_state_machine_trace_reachability_helpers(
    test_name: &str,
    helper_prefix: &str,
    acceptance: &StateMachineAcceptance,
    output_semantics: &FuzzerSemantics,
    transition_semantics: &StateMachineTransitionSemantics,
    inner_data_schemas: &BTreeMap<String, ExportedDataSchema>,
    shape_builder: &mut LeanDataShapeBuilder,
    shape_prefix: &str,
    step_function_ir: Option<&aiken_lang::test_framework::ShallowIr>,
    transition_prop: Option<&crate::export::ExportedTransitionProp>,
    initial_state_ir: Option<&aiken_lang::test_framework::ShallowIr>,
) -> miette::Result<(String, String)> {
    let label_index = transition_semantics.label_field_index;
    let next_state_index = transition_semantics.next_state_field_index;
    let event_index = transition_semantics.event_field_index;
    let mut unique_indexes = BTreeSet::new();
    unique_indexes.insert(label_index);
    unique_indexes.insert(next_state_index);
    unique_indexes.insert(event_index);
    if unique_indexes.len() != 3 {
        return Err(unsupported_error(
            "E0029",
            UnsupportedReason::OverlappingTransitionIndexes {
                test_name: test_name.to_string(),
            },
        ));
    }

    let mut helper_blocks = Vec::new();
    let mut cached_state_predicate: Option<String> = None;

    let terminal_predicate = format!("{helper_prefix}_trace_is_terminal");
    let label_field = format!("{helper_prefix}_trace_step_label");
    let next_state_field = format!("{helper_prefix}_trace_step_next_state");
    let event_field = format!("{helper_prefix}_trace_step_event");
    let step_relation = format!("{helper_prefix}_step_relation");
    let reachable_from = format!("{helper_prefix}_reachable_from");
    let output_projection = format!("{helper_prefix}_project_output");
    let reachable_output = format!("{helper_prefix}_reachable_output");
    let (trace_length_bounds, step_depth_bounds) =
        scenario_trace_bounds_from_output_semantics(acceptance, output_semantics);
    let (trace_within_bounds, trace_depth_within_bounds, trace_bound_defs) =
        emit_scenario_trace_bound_helpers(helper_prefix, &trace_length_bounds, &step_depth_bounds);
    helper_blocks.push(trace_bound_defs);

    helper_blocks.push(format!(
        "def {terminal_predicate} : Data -> Prop\n  | Data.Constr tag fields => tag = {tag} {AND} fields = []\n  | _ => False\n",
        tag = transition_semantics.terminal_tag,
        AND = lean_sym::AND,
    ));
    helper_blocks.push(format!(
        "def {label_field} : Data -> Option Data\n  | Data.Constr tag fields =>\n      if tag = {} then fields.get? {} else none\n  | _ => none\n",
        transition_semantics.step_tag, label_index
    ));
    helper_blocks.push(format!(
        "def {next_state_field} : Data -> Option Data\n  | Data.Constr tag fields =>\n      if tag = {} then fields.get? {} else none\n  | _ => none\n",
        transition_semantics.step_tag, next_state_index
    ));
    helper_blocks.push(format!(
        "def {event_field} : Data -> Option Data\n  | Data.Constr tag fields =>\n      if tag = {} then fields.get? {} else none\n  | _ => none\n",
        transition_semantics.step_tag, event_index
    ));

    let exact_initial_state_expr = transition_prop
        .and_then(|tp| tp.initial_state_lean.clone())
        .or_else(|| {
            initial_state_ir.and_then(|ir| {
                let (body, existentials, widenings) = emit_initial_state_as_lean(ir);
                if existentials.is_empty() && widenings.is_empty() {
                    Some(body)
                } else {
                    None
                }
            })
        });

    let mut precise_step_relation_parts: Vec<String> = Vec::new();

    if let Some(tp) = transition_prop {
        if let Some((ctor, type_name)) = tp.s0002_marker.as_ref() {
            return Err(miette::Report::new(error_catalogue::unsupported(
                "S0002",
                UnsupportedReason::ConstructorTagUnresolved {
                    test_name: test_name.to_string(),
                    ctor: ctor.clone(),
                    type_name: type_name.clone(),
                },
            )));
        }

        if !tp.opaque_sub_generators.is_empty() {
            let (first_module, first_fn) = &tp.opaque_sub_generators[0];
            return Err(miette::Report::new(error_catalogue::unsupported(
                "E0018",
                UnsupportedReason::OpaqueSubgeneratorStub {
                    test_name: test_name.to_string(),
                    sub_generator: format!("{first_module}::{first_fn}"),
                },
            )));
        }

        let is_valid_transition_name = format!("{helper_prefix}_is_valid_transition");
        let sg_prefix = format!("{helper_prefix}_");
        let is_valid_transition_def = tp
            .is_valid_transition_def
            .replace("__FN__", &is_valid_transition_name)
            .replace("__SGPFX__", &sg_prefix);
        let theorem_widenings = transition_prop_widenings(tp);
        if !theorem_widenings.is_empty() {
            helper_blocks.push(format!(
                "-- transition audit: {} dropped or widened constraints while lowering the step relation\n",
                theorem_widenings.len()
            ));
            for widening in &theorem_widenings {
                helper_blocks.push(format!("--   {}\n", widening.message));
            }
        }
        helper_blocks.push(is_valid_transition_def);
        if !tp.is_vacuous {
            precise_step_relation_parts
                .push(format!("{is_valid_transition_name} state transition"));
        }
    }

    if let Some(ir) = step_function_ir {
        if let Some(report) = try_dispatch_s0002_from_shallow_ir(test_name, ir) {
            return Err(report);
        }

        let step_fn_name = format!("{helper_prefix}_step_fn");
        let (step_fn_def, step_fn_widenings) = emit_step_fn_predicate(&step_fn_name, ir);
        if let Some(summary) = widening_summary_suffix(&step_fn_widenings, false, &[]) {
            return Err(generation_error(
                GenerationErrorCategory::FallbackRequired,
                format!(
                    "Test '{test_name}': step-function helper emission widened the theorem ({summary}); refusing to emit an unaudited state-machine proof.",
                ),
            ));
        }
        helper_blocks.push(step_fn_def);

        let sound_block = match try_emit_step_fn_sound_as_theorem(
            &step_fn_name,
            ir,
            transition_semantics.step_tag,
            label_index,
            next_state_index,
            event_index,
            &label_field,
            &next_state_field,
            &event_field,
        ) {
            Some(theorem) => theorem,
            None => {
                return Err(miette::Report::new(error_catalogue::unsupported(
                    "S0001",
                    UnsupportedReason::StepFnSoundAxiomEmitted {
                        test_name: test_name.to_string(),
                        shape: describe_step_ir_shape(ir).to_string(),
                    },
                )));
            }
        };
        helper_blocks.push(sound_block);
        precise_step_relation_parts.push(format!("{step_fn_name} state transition"));
    }

    if !precise_step_relation_parts.is_empty() {
        let mut step_relation_parts = precise_step_relation_parts;
        step_relation_parts.push(format!("{label_field} transition = some label"));
        step_relation_parts.push(format!("{next_state_field} transition = some nextState"));
        step_relation_parts.push(format!("{event_field} transition = some event"));
        helper_blocks.push(format!(
            "def {step_relation} (state transition nextState label event : Data) : Prop :=\n  {}\n",
            lean_prop_conjunction(&step_relation_parts)
        ));
    } else {
        let (state_predicate, state_defs) = build_partial_data_semantics_predicates_into(
            &format!("{helper_prefix}_state"),
            transition_semantics.state_semantics.as_ref(),
            test_name,
            inner_data_schemas,
            shape_builder,
            shape_prefix,
        )?;
        let (label_predicate, label_defs) = build_partial_data_semantics_predicates_into(
            &format!("{helper_prefix}_label"),
            transition_semantics.label_semantics.as_ref(),
            test_name,
            inner_data_schemas,
            shape_builder,
            shape_prefix,
        )?;
        let (event_predicate, event_defs) = build_partial_data_semantics_predicates_into(
            &format!("{helper_prefix}_event"),
            transition_semantics.event_semantics.as_ref(),
            test_name,
            inner_data_schemas,
            shape_builder,
            shape_prefix,
        )?;
        helper_blocks.push(state_defs);
        helper_blocks.push(label_defs);
        helper_blocks.push(event_defs);
        cached_state_predicate = Some(state_predicate.clone());

        let mut step_input_predicates = Vec::new();
        for (index, semantics) in transition_semantics.step_input_semantics.iter().enumerate() {
            let (predicate, defs) = build_partial_data_semantics_predicates_into(
                &format!("{helper_prefix}_step_input_{index}"),
                semantics,
                test_name,
                inner_data_schemas,
                shape_builder,
                shape_prefix,
            )?;
            step_input_predicates.push(predicate);
            helper_blocks.push(defs);
        }

        let step_inputs_satisfiable = format!("{helper_prefix}_step_inputs_satisfiable");
        let step_input_parts: Vec<String> = step_input_predicates
            .iter()
            .enumerate()
            .map(|(index, predicate)| {
                format!(
                    "({EXISTS} step_input_{index}, {predicate} step_input_{index})",
                    EXISTS = lean_sym::EXISTS,
                )
            })
            .collect();
        helper_blocks.push(format!(
            "def {step_inputs_satisfiable} : Prop := {}\n",
            lean_prop_conjunction(&step_input_parts)
        ));

        helper_blocks.push(format!(
            "def {step_relation} (state transition nextState label event : Data) : Prop :=\n  {state_predicate} state {AND}\n  {state_predicate} nextState {AND}\n  {label_predicate} label {AND}\n  {event_predicate} event {AND}\n  {step_inputs_satisfiable} {AND}\n  {label_field} transition = some label {AND}\n  {next_state_field} transition = some nextState {AND}\n  {event_field} transition = some event\n",
            AND = lean_sym::AND,
        ));
    }

    match acceptance {
        StateMachineAcceptance::AcceptsSuccess => {
            helper_blocks.push(format!(
                "def {reachable_from} : Data -> List Data -> List Data -> Prop\n  | _state, [], _events => False\n  | _state, [terminal], events => {terminal_predicate} terminal {AND} events = []\n  | state, transition :: rest, events =>\n      {EXISTS} nextState label event tailEvents,\n        {step_relation} state transition nextState label event {AND}\n        {reachable_from} nextState rest tailEvents {AND}\n        events = event :: tailEvents\n",
                AND = lean_sym::AND,
                EXISTS = lean_sym::EXISTS,
            ));
            helper_blocks.push(format!(
                "def {output_projection} (events : List Data) : Data := Data.List events\n"
            ));
            if let Some(initial_state_expr) = exact_initial_state_expr.as_ref() {
                helper_blocks.push(format!(
                    "def {reachable_output} (x : Data) : Prop :=\n  {EXISTS} trace, events,\n    {trace_within_bounds} trace {AND}\n    {trace_depth_within_bounds} trace {AND}\n    {reachable_from} ({initial_state_expr}) trace events {AND}\n    x = {output_projection} events\n",
                    EXISTS = lean_sym::EXISTS,
                    AND = lean_sym::AND,
                ));
            } else {
                let state_predicate = if let Some(name) = cached_state_predicate.clone() {
                    name
                } else {
                    let (name, defs) = build_partial_data_semantics_predicates_into(
                        &format!("{helper_prefix}_state"),
                        transition_semantics.state_semantics.as_ref(),
                        test_name,
                        inner_data_schemas,
                        shape_builder,
                        shape_prefix,
                    )?;
                    helper_blocks.push(defs);
                    cached_state_predicate = Some(name.clone());
                    name
                };
                helper_blocks.push(format!(
                    "def {reachable_output} (x : Data) : Prop :=\n  {EXISTS} initState, trace, events,\n    {state_predicate} initState {AND}\n    {trace_within_bounds} trace {AND}\n    {trace_depth_within_bounds} trace {AND}\n    {reachable_from} initState trace events {AND}\n    x = {output_projection} events\n",
                    EXISTS = lean_sym::EXISTS,
                    AND = lean_sym::AND,
                ));
            }
        }
        StateMachineAcceptance::AcceptsFailure => {
            helper_blocks.push(format!(
                "def {reachable_from} : Data -> List Data -> List Data -> List Data -> Prop\n  | _state, [], _labels, _events => False\n  | _state, [terminal], _labels, _events => False\n  | state, [transition, terminal], labels, events =>\n      {EXISTS} nextState label event,\n        {step_relation} state transition nextState label event {AND}\n        {terminal_predicate} terminal {AND}\n        labels = [label] {AND}\n        events = [event]\n  | state, transition :: rest, labels, events =>\n      {EXISTS} nextState label event tailLabels tailEvents,\n        {step_relation} state transition nextState label event {AND}\n        {reachable_from} nextState rest tailLabels tailEvents {AND}\n        labels = label :: tailLabels {AND}\n        events = event :: tailEvents\n",
                AND = lean_sym::AND,
                EXISTS = lean_sym::EXISTS,
            ));
            helper_blocks.push(format!(
                "def {output_projection} (labels events : List Data) : Data :=\n  Data.List [Data.List labels, Data.List events]\n"
            ));
            if let Some(initial_state_expr) = exact_initial_state_expr.as_ref() {
                helper_blocks.push(format!(
                    "def {reachable_output} (x : Data) : Prop :=\n  {EXISTS} trace, labels, events,\n    {trace_within_bounds} trace {AND}\n    {trace_depth_within_bounds} trace {AND}\n    {reachable_from} ({initial_state_expr}) trace labels events {AND}\n    x = {output_projection} labels events\n",
                    EXISTS = lean_sym::EXISTS,
                    AND = lean_sym::AND,
                ));
            } else {
                let state_predicate = if let Some(name) = cached_state_predicate.clone() {
                    name
                } else {
                    let (name, defs) = build_partial_data_semantics_predicates_into(
                        &format!("{helper_prefix}_state"),
                        transition_semantics.state_semantics.as_ref(),
                        test_name,
                        inner_data_schemas,
                        shape_builder,
                        shape_prefix,
                    )?;
                    helper_blocks.push(defs);
                    cached_state_predicate = Some(name.clone());
                    name
                };
                helper_blocks.push(format!(
                    "def {reachable_output} (x : Data) : Prop :=\n  {EXISTS} initState, trace, labels, events,\n    {state_predicate} initState {AND}\n    {trace_within_bounds} trace {AND}\n    {trace_depth_within_bounds} trace {AND}\n    {reachable_from} initState trace labels events {AND}\n    x = {output_projection} labels events\n",
                    EXISTS = lean_sym::EXISTS,
                    AND = lean_sym::AND,
                ));
            }
        }
    }

    let definitions = helper_blocks
        .into_iter()
        .filter(|block| !block.trim().is_empty())
        .collect::<Vec<_>>()
        .join("\n");

    Ok((reachable_output, definitions))
}

/// Returns `true` iff the given fuzzer semantics tree contains an
/// `Opaque` leaf anywhere inside. Used to decide whether the state-machine
/// trace proof generator must route through the FallbackRequired path.
fn fuzzer_semantics_contains_opaque(semantics: &FuzzerSemantics) -> bool {
    match semantics {
        FuzzerSemantics::Opaque { .. } => true,
        FuzzerSemantics::List { element, .. } => {
            fuzzer_semantics_contains_opaque(element.as_ref())
        }
        FuzzerSemantics::Product(elems) => {
            elems.iter().any(fuzzer_semantics_contains_opaque)
        }
        FuzzerSemantics::StateMachineTrace {
            output_semantics, ..
        } => fuzzer_semantics_contains_opaque(output_semantics.as_ref()),
        FuzzerSemantics::Bool
        | FuzzerSemantics::IntRange { .. }
        | FuzzerSemantics::ByteArrayRange { .. }
        | FuzzerSemantics::String
        | FuzzerSemantics::Data
        // `DataWithSchema` carries a structural schema predicate, which
        // disambiguates the `Data` domain enough for sound theorem
        // generation -- it is NOT opaque.
        | FuzzerSemantics::DataWithSchema { .. }
        | FuzzerSemantics::Exact(_)
        | FuzzerSemantics::OneOf(_)
        | FuzzerSemantics::Constructors { .. } => false,
    }
}

/// Returns true if the semantics contains any `DataWithSchema` leaf,
/// which requires a `fuzzer_data_schema` to produce a sound theorem.
///
/// Without a schema, a `DataWithSchema` leaf would be quantified as the full
/// `Data` domain with no structural antecedent, producing an unsound universal
/// theorem (`∀ x : Data, True → P x`) that could be reported as PROVED for
/// validators that happen to accept all `Data`.
fn fuzzer_semantics_requires_schema(sem: &FuzzerSemantics) -> bool {
    match sem {
        FuzzerSemantics::DataWithSchema { .. } => true,
        FuzzerSemantics::List { element, .. } => fuzzer_semantics_requires_schema(element),
        FuzzerSemantics::Product(parts) => parts.iter().any(fuzzer_semantics_requires_schema),
        FuzzerSemantics::StateMachineTrace {
            output_semantics, ..
        } => fuzzer_semantics_requires_schema(output_semantics.as_ref()),
        FuzzerSemantics::Bool
        | FuzzerSemantics::IntRange { .. }
        | FuzzerSemantics::ByteArrayRange { .. }
        | FuzzerSemantics::String
        | FuzzerSemantics::Data
        | FuzzerSemantics::Exact(_)
        | FuzzerSemantics::OneOf(_)
        | FuzzerSemantics::Constructors { .. }
        | FuzzerSemantics::Opaque { .. } => false,
    }
}

#[allow(clippy::too_many_arguments)]
fn try_generate_state_machine_trace_proof_from_semantics(
    test: &ExportedPropertyTest,
    form: &TheoremForm,
    lean_test_name: &str,
    verify_prog: &str,
    direct_header: &dyn Fn(&str) -> String,
    footer: &str,
    target: &VerificationTargetKind,
    prop_prog: &str,
    handler_prog: &str,
) -> miette::Result<Option<String>> {
    let FuzzerSemantics::StateMachineTrace {
        acceptance,
        transition_semantics,
        output_semantics,
        step_function_ir,
        initial_state_shallow_ir,
        ..
    } = &test.semantics
    else {
        return Ok(None);
    };

    // Soundness gate: the `step_inputs_satisfiable` predicate built by
    // `build_state_machine_trace_reachability_helpers` is a global over-approximation
    // of reachability. For universal (`test`) theorems this only widens what must be
    // proved (sound-but-incomplete). For existential (`fail once`) theorems --
    // `∃ x, reachability(x) ∧ ¬P(x)` -- the over-approximation admits spurious traces
    // that no compiled program can produce, so Z3 could witness a "failing" input that
    // does not actually exist at runtime. Route existential state-machine trace proofs
    // through the skip path until step-input witnesses are tied to specific transitions.
    //
    // This check must run before the soundness-only opaque fallback: an
    // existential proof with an opaque domain is still unsound for the same
    // reachability reason, and must be routed through the fallback path.
    if form.existential {
        return Err(unsupported_error(
            "E0023",
            UnsupportedReason::ExistentialStateMachineTrace {
                test_name: test.name.clone(),
            },
        ));
    }

    if is_state_machine_trace_halt_test(test)
        && test
            .transition_prop_lean
            .as_ref()
            .is_some_and(|tp| !tp.opaque_sub_generators.is_empty())
    {
        let (first_module, first_fn) = &test
            .transition_prop_lean
            .as_ref()
            .expect("checked above")
            .opaque_sub_generators[0];
        return Err(miette::Report::new(error_catalogue::unsupported(
            "E0018",
            UnsupportedReason::OpaqueSubgeneratorStub {
                test_name: test.name.clone(),
                sub_generator: format!("{first_module}::{first_fn}"),
            },
        )));
    }

    // FallbackRequired: if the state-machine output domain transitively
    // contains an opaque fuzzer semantic (e.g. `List<Transaction>` where the
    // element type has no structural Lean lowering yet), we cannot build a
    // precise domain predicate. Surface this as a FallbackRequired error so
    // the caller routes the test through the non-proof path; we never emit a
    // vacuous theorem that could be reported as PROVED.
    if fuzzer_semantics_contains_opaque(output_semantics.as_ref()) {
        return Err(unsupported_error(
            "E0016",
            UnsupportedReason::OpaqueTopLevelFuzzer {
                test_name: test.name.clone(),
                reason: "state-machine output domain contains opaque semantics".to_string(),
            },
        ));
    }

    let Some(schema) = test.fuzzer_data_schema.as_ref() else {
        return Err(unsupported_error(
            "E0025",
            UnsupportedReason::MissingFuzzerSchema {
                test_name: test.name.clone(),
            },
        ));
    };

    // Share a single `LeanDataShapeBuilder` across every section of this
    // theorem (outer domain schema, state-machine output semantics, and
    // reachability helpers). With a shared builder, repeated structural
    // lookups of the same ADT — `cardano/transaction.Transaction` appears
    // in all three sections for Amaru-style event traces — collapse via
    // the builder's `generated_refs` deduplication, so the corresponding
    // predicate family is emitted exactly once per theorem.
    let helper_prefix = format!("{lean_test_name}_shape");
    let mut shape_builder = LeanDataShapeBuilder::default();

    let root_predicate = build_exported_data_shape_predicates_into(
        &test.name,
        schema,
        Some(&test.inner_data_schemas),
        &helper_prefix,
        &mut shape_builder,
    )?;
    let (semantics_predicate, semantics_defs) = build_partial_data_semantics_predicates_into(
        &format!("{helper_prefix}_semantics"),
        output_semantics.as_ref(),
        &test.name,
        &test.inner_data_schemas,
        &mut shape_builder,
        &helper_prefix,
    )?;
    let (reachability_predicate, reachability_defs) =
        build_state_machine_trace_reachability_helpers(
            &test.name,
            &format!("{helper_prefix}_reachability"),
            acceptance,
            output_semantics.as_ref(),
            transition_semantics,
            &test.inner_data_schemas,
            &mut shape_builder,
            &helper_prefix,
            step_function_ir.as_ref(),
            test.transition_prop_lean.as_ref(),
            initial_state_shallow_ir.as_ref(),
        )?;
    // Finalize all shape predicates once, after every contributing section
    // has had a chance to populate the builder.
    let helper_defs = finalize_lean_data_shape_builder(shape_builder);

    // Soundness gate for the `proveTestsHalt` form of state-machine traces:
    // the universal theorem `∀ x, shape(x) ∧ semantics(x) ∧ reachable(x) →
    // proveTestsHalt prog (dataArg x)` is FALSE in general, because
    // `reachable` is a global over-approximation of the step function and
    // admits traces that no compiled program can produce. Emitting such a
    // theorem is unsound. When the export-time witness loop could not
    // synthesize a concrete halting input, surface a FallbackRequired skip
    // rather than emit a provably-false universal theorem. The
    // `generate_state_machine_halt_proof_file` path replaces this with
    // concrete `native_decide` instance theorems whenever witnesses exist.
    if is_state_machine_trace_halt_test(test)
        && test.concrete_halt_witnesses.is_empty()
        && step_function_ir.is_none()
    {
        return Err(unsupported_error(
            "E0024",
            UnsupportedReason::StateMachineNoWitnessSynthesizable {
                test_name: test.name.clone(),
            },
        ));
    }

    let quantifier_vars = "(x : Data)";
    let precondition_parts = vec![
        format!("{root_predicate} x"),
        format!("{semantics_predicate} x"),
        format!("{reachability_predicate} x"),
    ];
    // Witness value is always None for state-machine trace proofs.
    // For existential (`fail once`) tests, this means no concrete witness is
    // provided to the Lean proof. The proof falls back to tactic search (blaster)
    // which may or may not succeed without a witness hint.
    //
    // Generating a witness for state-machine traces requires finding a concrete
    // accepting or rejecting path through the state machine -- essentially model
    // checking. This would involve:
    //   1. Extracting the initial state value
    //   2. Enumerating possible step-input sequences
    //   3. Finding a trace that reaches a terminal state
    //   4. Encoding that trace as a Lean Data literal
    // This is complex and not yet implemented. State-machine existential proofs
    // currently only support ExistentialMode::Proof (no witness).
    let theorems = format_theorems(
        form,
        lean_test_name,
        verify_prog,
        "dataArg x",
        quantifier_vars,
        &precondition_parts,
        None,
    );

    let mut content =
        direct_header("open PlutusCore.Data (Data)\nopen PlutusCore.ByteString (ByteString)");
    content.push_str("-- compiler-exported structural domain predicate\n");
    content.push_str(&helper_defs);
    content.push('\n');
    content.push_str("-- compiler-exported state-machine output semantics\n");
    content.push_str(&semantics_defs);
    content.push('\n');
    content.push_str("-- compiler-exported state-machine reachability relation\n");
    content.push_str(&reachability_defs);
    content.push('\n');
    content.push_str(&format_mode_context(form, verify_prog));
    content.push_str(&theorems);
    if let VerificationTargetKind::Equivalence = target {
        content.push_str(&format_equivalence_theorem(
            test,
            lean_test_name,
            prop_prog,
            handler_prog,
            "dataArg x",
            quantifier_vars,
            &precondition_parts,
        ));
    }
    content.push_str(footer);

    Ok(Some(content))
}

fn collect_scalar_precondition_parts_from_semantics(
    test_name: &str,
    output_type: &FuzzerOutputType,
    semantics: &FuzzerSemantics,
    var: &str,
    out: &mut Vec<String>,
    witness: &mut Option<String>,
) -> miette::Result<()> {
    match semantics {
        FuzzerSemantics::Bool => {
            if !matches!(output_type, FuzzerOutputType::Bool) {
                return Err(generation_error(
                    GenerationErrorCategory::InvalidConstraint,
                    format!(
                        "Test '{}' has Bool semantics for non-Bool output type {}.",
                        test_name, output_type
                    ),
                ));
            }
            if witness.is_none() {
                *witness = Some("true".to_string());
            }
            Ok(())
        }
        FuzzerSemantics::IntRange { min, max } => {
            if !matches!(output_type, FuzzerOutputType::Int) {
                return Err(generation_error(
                    GenerationErrorCategory::InvalidConstraint,
                    format!(
                        "Test '{}' has Int semantics for non-Int output type {}.",
                        test_name, output_type
                    ),
                ));
            }

            match (min, max) {
                (Some(min), Some(max)) => {
                    validate_int_bounds_literals(test_name, min, max)?;
                    out.push(format!(
                        "({min} <= {var} {AND} {var} <= {max})",
                        AND = lean_sym::AND
                    ));
                    if witness.is_none() {
                        *witness = Some(format!("({min} : Integer)"));
                    }
                }
                (Some(min), None) => {
                    let _ = parse_integer_literal(min).ok_or_else(|| {
                        generation_error(
                            GenerationErrorCategory::InvalidConstraint,
                            format!(
                                "Test '{}' has invalid lower Int bound '{}'.",
                                test_name, min
                            ),
                        )
                    })?;
                    out.push(format!("({min} <= {var})"));
                    if witness.is_none() {
                        *witness = Some(format!("({min} : Integer)"));
                    }
                }
                (None, Some(max)) => {
                    let _ = parse_integer_literal(max).ok_or_else(|| {
                        generation_error(
                            GenerationErrorCategory::InvalidConstraint,
                            format!(
                                "Test '{}' has invalid upper Int bound '{}'.",
                                test_name, max
                            ),
                        )
                    })?;
                    out.push(format!("({var} <= {max})"));
                    if witness.is_none() {
                        *witness = Some(format!("({max} : Integer)"));
                    }
                }
                (None, None) => {
                    if witness.is_none() {
                        *witness = Some("(0 : Integer)".to_string());
                    }
                }
            }

            Ok(())
        }
        FuzzerSemantics::ByteArrayRange { min_len, max_len } => {
            if !matches!(
                output_type,
                FuzzerOutputType::ByteArray | FuzzerOutputType::String
            ) {
                return Err(generation_error(
                    GenerationErrorCategory::InvalidConstraint,
                    format!(
                        "Test '{}' has byte-array semantics for non-byte-string output type {}.",
                        test_name, output_type
                    ),
                ));
            }

            match (min_len, max_len) {
                (Some(min_len), Some(max_len)) => {
                    validate_bytestring_len_bounds(test_name, *min_len, *max_len)?;
                    out.push(format!(
                        "({min_len} <= {var}.length {AND} {var}.length <= {max_len})",
                        AND = lean_sym::AND
                    ));
                    if witness.is_none() {
                        *witness = Some(lean_zero_bytestring_literal_of_len(*min_len));
                    }
                }
                (Some(min_len), None) => {
                    out.push(format!("({min_len} <= {var}.length)"));
                    if witness.is_none() {
                        *witness = Some(lean_zero_bytestring_literal_of_len(*min_len));
                    }
                }
                (None, Some(max_len)) => {
                    out.push(format!("({var}.length <= {max_len})"));
                    if witness.is_none() {
                        *witness = Some("ByteString.empty".to_string());
                    }
                }
                (None, None) => {
                    if witness.is_none() {
                        *witness = Some("ByteString.empty".to_string());
                    }
                }
            }

            Ok(())
        }
        FuzzerSemantics::String => {
            if !matches!(output_type, FuzzerOutputType::String) {
                return Err(generation_error(
                    GenerationErrorCategory::InvalidConstraint,
                    format!(
                        "Test '{}' has String semantics for non-String output type {}.",
                        test_name, output_type
                    ),
                ));
            }
            if witness.is_none() {
                *witness = Some("ByteString.empty".to_string());
            }
            Ok(())
        }
        FuzzerSemantics::Data => {
            if !matches!(
                output_type,
                FuzzerOutputType::Data | FuzzerOutputType::Unsupported(_)
            ) {
                return Err(generation_error(
                    GenerationErrorCategory::InvalidConstraint,
                    format!(
                        "Test '{}' has Data semantics for non-Data output type {}.",
                        test_name, output_type
                    ),
                ));
            }
            if witness.is_none() {
                *witness = Some("Data.I 0".to_string());
            }
            Ok(())
        }
        FuzzerSemantics::DataWithSchema { .. } => {
            // Structural constraint is generated from the exported
            // fuzzer_data_schema (see `build_exported_data_shape_predicates`)
            // and conjoined at the theorem level; no scalar precondition is
            // emitted here. Behaves like `FuzzerSemantics::Data` at this layer.
            //
            // TODO(Issue 14 follow-up): For List<DataWithSchema> elements, emit
            // `allProp {root_predicate} xs` per element using the schema root
            // predicate. Currently no per-element structural constraint is
            // emitted; the theorem quantifies over List<Data> without
            // type-structural preconditions on elements. This is sound
            // (stronger proof obligation) but may cause theorems over typed
            // ADT lists to fail when they would succeed with constraints.
            // See: PR_plan.md Issue 14, Oracle B review.
            if !matches!(
                output_type,
                FuzzerOutputType::Data | FuzzerOutputType::Unsupported(_)
            ) {
                return Err(generation_error(
                    GenerationErrorCategory::InvalidConstraint,
                    format!(
                        "Test '{}' has DataWithSchema semantics for non-Data output type {}.",
                        test_name, output_type
                    ),
                ));
            }
            if witness.is_none() {
                *witness = Some("Data.I 0".to_string());
            }
            Ok(())
        }
        FuzzerSemantics::Exact(value) => {
            let lit = exact_value_to_scalar_literal(output_type, value).ok_or_else(|| {
                generation_error(
                    GenerationErrorCategory::FallbackRequired,
                    format!(
                        "Test '{}' has exact semantic value {} that cannot be translated for output type {}.",
                        test_name, value, output_type
                    ),
                )
            })?;
            out.push(format!("({var} = {lit})"));
            *witness = Some(lit);
            Ok(())
        }
        FuzzerSemantics::OneOf(values) => {
            if values.is_empty() {
                return Err(semantics_output_type_mismatch_error(test_name));
            }
            let mut parts = Vec::new();
            let mut first_lit = None;
            for value in values {
                let lit = exact_value_to_scalar_literal(output_type, value)
                    .ok_or_else(|| semantics_output_type_mismatch_error(test_name))?;
                if first_lit.is_none() {
                    first_lit = Some(lit.clone());
                }
                parts.push(format!("{var} = {lit}"));
            }
            out.push(format!("({})", parts.join(&format!(" {} ", lean_sym::OR))));
            *witness = first_lit;
            Ok(())
        }
        FuzzerSemantics::Constructors { tags } => {
            if !matches!(
                output_type,
                FuzzerOutputType::Data | FuzzerOutputType::Unsupported(_)
            ) {
                return Err(generation_error(
                    GenerationErrorCategory::InvalidConstraint,
                    format!(
                        "Test '{}' has constructor semantics for non-Data output type {}.",
                        test_name, output_type
                    ),
                ));
            }
            let predicate = data_constructor_tags_predicate(var, tags).ok_or_else(|| {
                generation_error(
                    GenerationErrorCategory::InvalidConstraint,
                    format!(
                        "Test '{}' has empty constructor-tag semantic domain.",
                        test_name
                    ),
                )
            })?;
            out.push(predicate);
            if witness.is_none() {
                let first_tag = tags
                    .iter()
                    .copied()
                    .min()
                    .expect("empty constructor tags handled above");
                *witness = Some(data_constructor_tag_literal(first_tag));
            }
            Ok(())
        }
        FuzzerSemantics::Opaque { reason } => Err(generation_error(
            GenerationErrorCategory::FallbackRequired,
            format!("Test '{}' semantic domain is opaque: {}", test_name, reason),
        )),
        other => Err(generation_error(
            GenerationErrorCategory::FallbackRequired,
            format!(
                "Test '{}' has non-scalar semantic domain {} for output type {}.",
                test_name, other, output_type
            ),
        )),
    }
}

fn build_scalar_domain_preconditions_from_semantics(
    test_name: &str,
    output_type: &FuzzerOutputType,
    semantics: &FuzzerSemantics,
    var: &str,
) -> miette::Result<(Vec<String>, Option<String>)> {
    let mut out = Vec::new();
    let mut witness = None;
    collect_scalar_precondition_parts_from_semantics(
        test_name,
        output_type,
        semantics,
        var,
        &mut out,
        &mut witness,
    )?;
    Ok((out, witness))
}

/// Whether theorem generation needs explicit extracted bounds for this output type.
///
/// This is now constraint-driven: it checks whether the constraint provides
/// sufficient information for the output type. Types like Bool, ByteArray, and
/// Data don't need bounds (they use universal quantification). Int types need
/// a known IntRange constraint.
pub fn requires_explicit_bounds(
    output_type: &FuzzerOutputType,
    constraint: &FuzzerConstraint,
) -> bool {
    match output_type {
        FuzzerOutputType::Int => !constraint_has_int_range(constraint),
        FuzzerOutputType::Tuple(types) => {
            // Only Int-typed positions require bounds; non-Int positions can have Any constraint
            let int_count = types
                .iter()
                .filter(|t| matches!(t, FuzzerOutputType::Int))
                .count();
            if int_count == 0 {
                return false;
            }
            !constraint_has_tuple_int_ranges_for_types(constraint, types)
        }
        FuzzerOutputType::Pair(fst, snd) => {
            let has_int =
                matches!(**fst, FuzzerOutputType::Int) || matches!(**snd, FuzzerOutputType::Int);
            if !has_int {
                return false;
            }
            let types = vec![fst.as_ref().clone(), snd.as_ref().clone()];
            !constraint_has_tuple_int_ranges_for_types(constraint, &types)
        }
        // List proofs require an extractable list-domain constraint.
        FuzzerOutputType::List(_) => !constraint_has_list_domain(constraint),
        _ => false,
    }
}

/// Whether theorem generation needs explicit extracted bounds, checked against
/// `FuzzerSemantics` (the representation actually used by the proof generator).
///
/// This is the semantics-driven counterpart to `requires_explicit_bounds`.
/// The proof generator consumes `FuzzerSemantics`, not `FuzzerConstraint`, so
/// checking semantics directly avoids disagreements between constraint extraction
/// and semantics derivation (e.g., a constraint may report `Any` while semantics
/// has `IntRange`).
pub fn requires_explicit_bounds_from_semantics(
    output_type: &FuzzerOutputType,
    semantics: &FuzzerSemantics,
) -> bool {
    match output_type {
        FuzzerOutputType::Int => !semantics_has_int_range(semantics),
        FuzzerOutputType::Tuple(types) => {
            let has_int = types.iter().any(|t| matches!(t, FuzzerOutputType::Int));
            if !has_int {
                return false;
            }
            match semantics {
                FuzzerSemantics::Product(elems) => {
                    types.iter().zip(elems.iter()).any(|(ty, sem)| {
                        matches!(ty, FuzzerOutputType::Int) && !semantics_has_int_range(sem)
                    })
                }
                // A bare IntRange covers all Int-typed positions (same range applied everywhere)
                FuzzerSemantics::IntRange { .. } => false,
                _ => true,
            }
        }
        FuzzerOutputType::Pair(fst, snd) => {
            let has_int =
                matches!(**fst, FuzzerOutputType::Int) || matches!(**snd, FuzzerOutputType::Int);
            if !has_int {
                return false;
            }
            match semantics {
                FuzzerSemantics::Product(elems) if elems.len() == 2 => {
                    let types = [fst.as_ref(), snd.as_ref()];
                    types.iter().zip(elems.iter()).any(|(ty, sem)| {
                        matches!(ty, FuzzerOutputType::Int) && !semantics_has_int_range(sem)
                    })
                }
                FuzzerSemantics::IntRange { .. } => false,
                _ => true,
            }
        }
        FuzzerOutputType::List(_) => !semantics_has_list_domain(semantics),
        _ => false,
    }
}

/// Check if semantics provides an IntRange with at least partial bounds.
fn semantics_has_int_range(semantics: &FuzzerSemantics) -> bool {
    matches!(semantics, FuzzerSemantics::IntRange { .. })
}

/// Check if semantics provides a List domain.
fn semantics_has_list_domain(semantics: &FuzzerSemantics) -> bool {
    matches!(semantics, FuzzerSemantics::List { .. })
}

/// Check if a constraint provides an IntRange (possibly wrapped/combined).
fn constraint_has_int_range(constraint: &FuzzerConstraint) -> bool {
    extract_int_range_from_constraint(constraint).is_some()
}

/// Check if a constraint contains a list domain (possibly nested in Map/And).
fn constraint_has_list_domain(constraint: &FuzzerConstraint) -> bool {
    match constraint {
        FuzzerConstraint::List { .. } => true,
        FuzzerConstraint::And(parts) => parts.iter().any(constraint_has_list_domain),
        _ => false,
    }
}

/// Check if a constraint provides IntRanges for all Int-typed tuple positions.
/// Non-Int positions (Bool, Data, etc.) do not require IntRange constraints.
fn constraint_has_tuple_int_ranges_for_types(
    constraint: &FuzzerConstraint,
    types: &[FuzzerOutputType],
) -> bool {
    types.iter().enumerate().all(|(index, ty)| {
        if matches!(ty, FuzzerOutputType::Int) {
            extract_tuple_element_int_range(constraint, types.len(), index).is_some()
        } else {
            true
        }
    })
}

fn collect_list_element_precondition_parts_from_semantics(
    test_name: &str,
    elem_type: &FuzzerOutputType,
    elem_semantics: &FuzzerSemantics,
    list_var: &str,
    out: &mut Vec<String>,
    witness_elem: &mut Option<String>,
) -> miette::Result<()> {
    match elem_semantics {
        FuzzerSemantics::Bool => {
            if !matches!(elem_type, FuzzerOutputType::Bool) {
                return Err(generation_error(
                    GenerationErrorCategory::FallbackRequired,
                    format!(
                        "Test '{}' has Bool list-element semantics for non-Bool element type {}.",
                        test_name, elem_type
                    ),
                ));
            }
            Ok(())
        }
        FuzzerSemantics::IntRange { min, max } => {
            if !matches!(elem_type, FuzzerOutputType::Int) {
                return Err(generation_error(
                    GenerationErrorCategory::FallbackRequired,
                    format!(
                        "Test '{}' has Int list-element semantics for non-Int element type {}.",
                        test_name, elem_type
                    ),
                ));
            }

            match (min, max) {
                (Some(min), Some(max)) => {
                    validate_int_bounds_literals(test_name, min, max)?;
                    out.push(format!(
                        "(allProp (fun x_i => {min} <= x_i {AND} x_i <= {max}) {list_var})",
                        AND = lean_sym::AND,
                    ));
                    *witness_elem = Some(format!("({min} : Integer)"));
                }
                (Some(min), None) => {
                    let _ = parse_integer_literal(min).ok_or_else(|| {
                        generation_error(
                            GenerationErrorCategory::InvalidConstraint,
                            format!(
                                "Test '{}' has invalid lower Int bound '{}'.",
                                test_name, min
                            ),
                        )
                    })?;
                    out.push(format!("(allProp (fun x_i => {min} <= x_i) {list_var})",));
                    *witness_elem = Some(format!("({min} : Integer)"));
                }
                (None, Some(max)) => {
                    let _ = parse_integer_literal(max).ok_or_else(|| {
                        generation_error(
                            GenerationErrorCategory::InvalidConstraint,
                            format!(
                                "Test '{}' has invalid upper Int bound '{}'.",
                                test_name, max
                            ),
                        )
                    })?;
                    out.push(format!("(allProp (fun x_i => x_i <= {max}) {list_var})",));
                    if witness_elem.is_none() {
                        *witness_elem = Some(format!("({max} : Integer)"));
                    }
                }
                (None, None) => {
                    if witness_elem.is_none() {
                        *witness_elem = Some("(0 : Integer)".to_string());
                    }
                }
            }

            Ok(())
        }
        FuzzerSemantics::ByteArrayRange { min_len, max_len } => {
            if !matches!(
                elem_type,
                FuzzerOutputType::ByteArray | FuzzerOutputType::String
            ) {
                return Err(generation_error(
                    GenerationErrorCategory::FallbackRequired,
                    format!(
                        "Test '{}' has byte-array list-element semantics for incompatible element type {}.",
                        test_name, elem_type
                    ),
                ));
            }

            match (min_len, max_len) {
                (Some(min_len), Some(max_len)) => {
                    validate_bytestring_len_bounds(test_name, *min_len, *max_len)?;
                    out.push(format!(
                        "(allProp (fun x_i => {min_len} <= x_i.length {AND} x_i.length <= {max_len}) {list_var})",
                        AND = lean_sym::AND,
                    ));
                    *witness_elem = Some(lean_zero_bytestring_literal_of_len(*min_len));
                }
                (Some(min_len), None) => {
                    out.push(format!(
                        "(allProp (fun x_i => {min_len} <= x_i.length) {list_var})",
                    ));
                    *witness_elem = Some(lean_zero_bytestring_literal_of_len(*min_len));
                }
                (None, Some(max_len)) => {
                    out.push(format!(
                        "(allProp (fun x_i => x_i.length <= {max_len}) {list_var})",
                    ));
                    if witness_elem.is_none() {
                        *witness_elem = Some("ByteString.empty".to_string());
                    }
                }
                (None, None) => {
                    if witness_elem.is_none() {
                        *witness_elem = Some("ByteString.empty".to_string());
                    }
                }
            }

            Ok(())
        }
        FuzzerSemantics::String => {
            if !matches!(elem_type, FuzzerOutputType::String) {
                return Err(generation_error(
                    GenerationErrorCategory::FallbackRequired,
                    format!(
                        "Test '{}' has String list-element semantics for non-String element type {}.",
                        test_name, elem_type
                    ),
                ));
            }
            if witness_elem.is_none() {
                *witness_elem = Some("ByteString.empty".to_string());
            }
            Ok(())
        }
        FuzzerSemantics::Data => {
            if !matches!(
                elem_type,
                FuzzerOutputType::Data | FuzzerOutputType::Unsupported(_)
            ) {
                return Err(generation_error(
                    GenerationErrorCategory::FallbackRequired,
                    format!(
                        "Test '{}' has Data list-element semantics for incompatible element type {}.",
                        test_name, elem_type
                    ),
                ));
            }
            if witness_elem.is_none() {
                *witness_elem = Some("Data.I 0".to_string());
            }
            Ok(())
        }
        FuzzerSemantics::DataWithSchema { .. } => {
            // Structural per-element constraint is emitted by the schema
            // predicate at the theorem level; at this layer we only ensure the
            // element output type is compatible and provide a witness.
            //
            // TODO(Issue 14 follow-up): For List<DataWithSchema> elements, emit
            // `allProp {root_predicate} xs` per element using the schema root
            // predicate. Currently no per-element structural constraint is
            // emitted; the theorem quantifies over List<Data> without
            // type-structural preconditions on elements. This is sound
            // (stronger proof obligation) but may cause theorems over typed
            // ADT lists to fail when they would succeed with constraints.
            // See: PR_plan.md Issue 14, Oracle B review.
            if !matches!(
                elem_type,
                FuzzerOutputType::Data | FuzzerOutputType::Unsupported(_)
            ) {
                return Err(generation_error(
                    GenerationErrorCategory::FallbackRequired,
                    format!(
                        "Test '{}' has DataWithSchema list-element semantics for incompatible element type {}.",
                        test_name, elem_type
                    ),
                ));
            }
            if witness_elem.is_none() {
                *witness_elem = Some("Data.I 0".to_string());
            }
            Ok(())
        }
        FuzzerSemantics::Exact(value) => {
            let lit = exact_value_to_scalar_literal(elem_type, value).ok_or_else(|| {
                generation_error(
                    GenerationErrorCategory::FallbackRequired,
                    format!(
                        "Test '{}' has unsupported exact list-element semantic value {} for element type {}.",
                        test_name, value, elem_type
                    ),
                )
            })?;
            out.push(format!("(allProp (fun x_i => x_i = {lit}) {list_var})",));
            *witness_elem = Some(lit);
            Ok(())
        }
        FuzzerSemantics::Constructors { tags } => {
            if !matches!(
                elem_type,
                FuzzerOutputType::Data | FuzzerOutputType::Unsupported(_)
            ) {
                return Err(generation_error(
                    GenerationErrorCategory::FallbackRequired,
                    format!(
                        "Test '{}' has constructor list-element semantics for incompatible element type {}.",
                        test_name, elem_type
                    ),
                ));
            }
            let predicate = data_constructor_tags_predicate("x_i", tags).ok_or_else(|| {
                generation_error(
                    GenerationErrorCategory::InvalidConstraint,
                    format!(
                        "Test '{}' has empty constructor-tag list-element semantic domain.",
                        test_name
                    ),
                )
            })?;
            out.push(format!("(allProp (fun x_i => {predicate}) {list_var})",));
            if witness_elem.is_none() {
                let first_tag = tags
                    .iter()
                    .copied()
                    .min()
                    .expect("empty constructor tags handled above");
                *witness_elem = Some(data_constructor_tag_literal(first_tag));
            }
            Ok(())
        }
        // Product element semantics (e.g. List<Pair<A,B>> from Dict types).
        // We accept these without adding per-element constraints -- the list-level
        // quantification (length bounds) is sufficient. Each element is typed via the
        // Lean type system (Pair type), so no additional structural predicate is needed.
        //
        // TODO(2.3): Full Dict/Map element predicates.
        // A complete implementation would recurse into each product component and
        // generate per-key/per-value sub-predicates (e.g. for Dict<ByteArray, Int>,
        // generate constraints on key length and value range). For now, we rely on
        // the Lean type-level encoding of Pair elements.
        FuzzerSemantics::Product(_) => {
            if matches!(
                elem_type,
                FuzzerOutputType::Pair(_, _) | FuzzerOutputType::Tuple(_)
            ) {
                // Product semantics for Pair/Tuple elements: accept without per-element predicate
                Ok(())
            } else {
                Err(generation_error(
                    GenerationErrorCategory::FallbackRequired,
                    format!(
                        "Test '{}' has Product list-element semantics for non-tuple element type {}.",
                        test_name, elem_type
                    ),
                ))
            }
        }
        FuzzerSemantics::Opaque { reason } => Err(generation_error(
            GenerationErrorCategory::FallbackRequired,
            format!(
                "Test '{}' has opaque list-element semantics: {}",
                test_name, reason
            ),
        )),
        other => Err(generation_error(
            GenerationErrorCategory::FallbackRequired,
            format!(
                "Test '{}' has unsupported structured list-element semantics {} for element type {}.",
                test_name, other, elem_type
            ),
        )),
    }
}

fn build_list_domain_preconditions_from_semantics(
    test_name: &str,
    elem_type: &FuzzerOutputType,
    semantics: &FuzzerSemantics,
    list_var: &str,
) -> miette::Result<(Vec<String>, Option<usize>, Option<String>)> {
    match semantics {
        FuzzerSemantics::List {
            element,
            min_len,
            max_len,
        } => {
            validate_list_len_bounds(test_name, *min_len, *max_len)?;
            let mut precondition_parts = Vec::new();

            match (min_len, max_len) {
                (Some(min), Some(max)) => precondition_parts.push(format!(
                    "({min} <= {list_var}.length {AND} {list_var}.length <= {max})",
                    AND = lean_sym::AND,
                )),
                (Some(min), None) => {
                    precondition_parts.push(format!("({min} <= {list_var}.length)"))
                }
                (None, Some(max)) => {
                    precondition_parts.push(format!("({list_var}.length <= {max})"))
                }
                (None, None) => {}
            }

            let mut witness_elem = None;
            collect_list_element_precondition_parts_from_semantics(
                test_name,
                elem_type,
                element.as_ref(),
                list_var,
                &mut precondition_parts,
                &mut witness_elem,
            )?;

            Ok((precondition_parts, *min_len, witness_elem))
        }
        FuzzerSemantics::Opaque { reason } => Err(generation_error(
            GenerationErrorCategory::FallbackRequired,
            format!(
                "Test '{}' semantic list domain is opaque: {}",
                test_name, reason
            ),
        )),
        other => Err(generation_error(
            GenerationErrorCategory::FallbackRequired,
            format!(
                "Test '{}' has non-list semantic domain {} for list output.",
                test_name, other
            ),
        )),
    }
}

fn build_list_witness_value(
    elem_type: &FuzzerOutputType,
    witness_min_len: Option<usize>,
    witness_elem: Option<&str>,
    int_bounds: Option<&(String, String)>,
) -> String {
    let elem_witness = if let Some((emin, _)) = int_bounds {
        format!("({emin} : Integer)")
    } else if let Some(exact_elem) = witness_elem {
        exact_elem.to_string()
    } else {
        lean_default_witness(elem_type)
    };

    let witness_len = witness_min_len.unwrap_or(0);
    let elems: Vec<String> = (0..witness_len).map(|_| elem_witness.clone()).collect();
    format!("[{}]", elems.join(", "))
}

fn validate_int_bounds_literals(test_name: &str, min: &str, max: &str) -> miette::Result<()> {
    let Some(min_val) = parse_integer_literal(min) else {
        return Err(generation_error(
            GenerationErrorCategory::InvalidConstraint,
            format!(
                "Test '{}' has invalid min bound '{}'; expected an integer literal",
                test_name, min
            ),
        ));
    };
    let Some(max_val) = parse_integer_literal(max) else {
        return Err(generation_error(
            GenerationErrorCategory::InvalidConstraint,
            format!(
                "Test '{}' has invalid max bound '{}'; expected an integer literal",
                test_name, max
            ),
        ));
    };

    if min_val > max_val {
        return Err(generation_error(
            GenerationErrorCategory::InvalidConstraint,
            format!(
                "Test '{}' has inconsistent integer bounds: min={} exceeds max={}",
                test_name, min, max,
            ),
        ));
    }

    Ok(())
}

fn extract_tuple_element_int_range(
    constraint: &FuzzerConstraint,
    arity: usize,
    index: usize,
) -> Option<(String, String)> {
    match constraint {
        FuzzerConstraint::Tuple(elems) if elems.len() == arity => {
            extract_int_range_from_constraint(&elems[index])
        }
        // Backward compatibility: a single IntRange shared for all Int components.
        FuzzerConstraint::IntRange { .. } => extract_int_range_from_constraint(constraint),
        FuzzerConstraint::And(parts) => {
            let mut merged: Option<(String, String)> = None;
            for part in parts {
                let Some(next) = extract_tuple_element_int_range(part, arity, index) else {
                    continue;
                };
                merged = match merged {
                    Some(acc) => Some(intersect_int_ranges(acc, next)?),
                    None => Some(next),
                };
            }
            merged
        }
        _ => None,
    }
}

fn intersect_int_ranges(
    left: (String, String),
    right: (String, String),
) -> Option<(String, String)> {
    let left_min = parse_integer_literal(&left.0)?;
    let left_max = parse_integer_literal(&left.1)?;
    let right_min = parse_integer_literal(&right.0)?;
    let right_max = parse_integer_literal(&right.1)?;

    let min = left_min.max(right_min);
    let max = left_max.min(right_max);
    if min > max {
        return None;
    }

    Some((min.to_string(), max.to_string()))
}

/// Recursively extract an IntRange from a constraint, looking through Map/And wrappers.
fn extract_int_range_from_constraint(constraint: &FuzzerConstraint) -> Option<(String, String)> {
    match constraint {
        FuzzerConstraint::IntRange { min, max } => Some((min.clone(), max.clone())),
        FuzzerConstraint::Map(inner) => extract_int_range_from_constraint(inner),
        FuzzerConstraint::And(parts) => {
            let mut merged: Option<(String, String)> = None;
            for part in parts {
                let Some(next) = extract_int_range_from_constraint(part) else {
                    continue;
                };
                merged = match merged {
                    Some(acc) => Some(intersect_int_ranges(acc, next)?),
                    None => Some(next),
                };
            }
            merged
        }
        _ => None,
    }
}

/// Theorem form descriptors determined by return mode and failure mode.
struct TheoremForm {
    /// The correctness theorem conclusion, expressed through the shared mode-aware
    /// outcome helpers in `AikenVerify.Utils`.
    correctness: TheoremBody,
    /// The termination theorem conclusion.
    /// None when the correctness theorem already implies termination (Void mode).
    termination: Option<TheoremBody>,
    /// Whether this is an existential (fail once) theorem.
    existential: bool,
    /// The existential mode used (only Some for existential theorems).
    existential_mode: Option<ExistentialMode>,
    /// Lean expression naming the manifest-exported return mode constant.
    return_mode_expr: &'static str,
    /// Lean expression naming the manifest-exported test mode constant.
    test_mode_expr: &'static str,
    /// Lean expression naming the centralized decode-policy constant.
    decode_policy_expr: &'static str,
}

impl TheoremForm {
    fn correctness_tactic(&self) -> &'static str {
        match self.existential_mode {
            Some(ExistentialMode::Witness) => "decide",
            _ => "blaster",
        }
    }
}

enum TheoremBody {
    /// `PropertySucceeds testReturnMode (testOutcome ({arg}))`
    Succeeds,
    /// `PropertyFails testReturnMode testDecodePolicy (testOutcome ({arg}))`
    Fails,
    /// `PropertyReturns testReturnMode (testOutcome ({arg}))`
    ReturnsValue,
}

impl TheoremBody {
    /// Format the theorem conclusion, substituting the encoded argument expression.
    fn format(&self, arg_expr: &str) -> String {
        match self {
            TheoremBody::Succeeds => {
                format!("PropertySucceeds testReturnMode (testOutcome ({arg_expr}))")
            }
            TheoremBody::Fails => {
                format!("PropertyFails testReturnMode testDecodePolicy (testOutcome ({arg_expr}))")
            }
            TheoremBody::ReturnsValue => {
                format!("PropertyReturns testReturnMode (testOutcome ({arg_expr}))")
            }
        }
    }

    fn tactic(&self) -> &'static str {
        "blaster"
    }
}

fn lean_return_mode_expr(return_mode: &TestReturnMode) -> &'static str {
    match return_mode {
        TestReturnMode::Bool => ".bool",
        TestReturnMode::Void => ".void",
    }
}

fn lean_test_mode_expr(test_mode: ManifestTestMode) -> &'static str {
    match test_mode {
        ManifestTestMode::Normal => ".normal",
        ManifestTestMode::Fail => ".fail",
        ManifestTestMode::FailOnce => ".failOnce",
    }
}

fn lean_decode_policy_expr(decode_policy: ManifestDecodePolicy) -> &'static str {
    match decode_policy {
        ManifestDecodePolicy::DecodeErrorIsTestFailure => ".decodeErrorIsTestFailure",
        ManifestDecodePolicy::DecodeErrorIsExportError => ".decodeErrorIsExportError",
    }
}

fn format_mode_context(form: &TheoremForm, prog: &str) -> String {
    format!(
        "set_option maxRecDepth 16384\n\n\
-- Manifest-driven mode and execution metadata.\n\
private abbrev testReturnMode : ReturnMode := {return_mode}\n\
private abbrev testMode : TestMode := {test_mode}\n\
private abbrev testDecodePolicy : DecodePolicy := {decode_policy}\n\
private abbrev testOutcome (args : List Term) : TestEvalOutcome :=\n  \
RunProperty testReturnMode {prog} args\n\n",
        return_mode = form.return_mode_expr,
        test_mode = form.test_mode_expr,
        decode_policy = form.decode_policy_expr,
    )
}

fn format_sampler_context(test_id: &str) -> String {
    let fuzzer_prog = fuzzer_prog_name(test_id);
    format!(
        "-- Generic sampler fallback over the exported Aiken fuzzer harness ABI.\n\
         -- sampler_run_kind=seeded_generation; generation_seed=none; replay_choices=none.\n\
         private abbrev samplerPrngMode : PrngMode := .seeded\n\
         private abbrev samplerConfig : FuzzerRunConfig :=\n\
           defaultFuzzerRunConfig samplerPrngMode testDecodePolicy\n\
         private abbrev samplerHarness : PlutusScript := {fuzzer_prog}\n\
         private abbrev samplerDomain (x : Data) : Prop :=\n\
           SeededFuzzerReturns samplerConfig samplerHarness x\n\n"
    )
}

/// Format the correctness + termination theorems given a form, name, and variable/arg details.
///
/// `quantifier_vars`: the variable bindings, e.g. `"(x : Integer)"` or `"(x : T1) (y : T2)"`.
/// `precondition_parts`: individual precondition expressions (NOT pre-joined).
/// `witness_value`: for existential witness mode, the concrete witness expression
/// (e.g. "0" for Int, "true" for Bool). None for universal/proof modes.
fn format_theorems(
    form: &TheoremForm,
    lean_test_name: &str,
    _prog: &str,
    arg_expr: &str,
    quantifier_vars: &str,
    precondition_parts: &[String],
    witness_value: Option<&str>,
) -> String {
    let correctness_body = form.correctness.format(arg_expr);
    let correctness_tactic = form.correctness_tactic();

    let mut out = if form.existential {
        let quantifiers = format!("{} {quantifier_vars},", lean_sym::EXISTS);
        let and_sep = format!("\n  {}\n  ", lean_sym::AND);
        let preconditions = if precondition_parts.is_empty() {
            String::new()
        } else {
            format!("\n  {}", precondition_parts.join(&and_sep))
        };
        let connector = if preconditions.is_empty() {
            " ".to_string()
        } else {
            and_sep
        };
        let mode_comment = match form.existential_mode {
            Some(ExistentialMode::Witness) => "-- Mode: witness (deterministic witness search)\n",
            Some(ExistentialMode::Proof) => "-- Mode: proof (existential proof attempt)\n",
            None => "",
        };

        let proof_term = if let Some(witness) = witness_value {
            format!(
                "{open}{witness}, by {correctness_tactic}{close}",
                open = lean_sym::ANGLE_OPEN,
                close = lean_sym::ANGLE_CLOSE,
            )
        } else {
            format!("by {correctness_tactic}")
        };

        format!(
            "{mode_comment}theorem {lean_test_name} :\n  {quantifiers}{preconditions}{connector}{correctness_body} :=\n  {proof_term}\n"
        )
    } else {
        let quantifiers = format!("{} {quantifier_vars},", lean_sym::FORALL);
        let implies_sep = format!("\n  {}\n  ", lean_sym::IMPLIES);
        let preconditions = if precondition_parts.is_empty() {
            String::new()
        } else {
            format!("\n  {}", precondition_parts.join(&implies_sep))
        };
        let arrow = if preconditions.is_empty() {
            String::new()
        } else {
            implies_sep
        };
        format!(
            "theorem {lean_test_name} :\n  {quantifiers}{preconditions}{arrow}{correctness_body} :=\n  by {correctness_tactic}\n"
        )
    };

    if let Some(ref term_body) = form.termination {
        let termination_body = term_body.format(arg_expr);
        let termination_tactic = term_body.tactic();
        let quantifiers = format!("{} {quantifier_vars},", lean_sym::FORALL);
        let implies_sep = format!("\n  {}\n  ", lean_sym::IMPLIES);
        let preconditions = if precondition_parts.is_empty() {
            String::new()
        } else {
            format!("\n  {}", precondition_parts.join(&implies_sep))
        };
        let arrow = if preconditions.is_empty() {
            String::new()
        } else {
            implies_sep
        };
        out.push_str(&format!(
            "\ntheorem {lean_test_name}_alwaysTerminating :\n  {quantifiers}{preconditions}{arrow}{termination_body} :=\n  by {termination_tactic}\n"
        ));
    }

    out
}

fn theorem_proposition(
    body: &TheoremBody,
    existential: bool,
    arg_expr: &str,
    quantifier_vars: &str,
    precondition_parts: &[String],
) -> String {
    let body = body.format(arg_expr);
    if existential {
        let quantifiers = format!("{} {quantifier_vars},", lean_sym::EXISTS);
        let and_sep = format!("\n  {}\n  ", lean_sym::AND);
        let preconditions = if precondition_parts.is_empty() {
            String::new()
        } else {
            format!("\n  {}", precondition_parts.join(&and_sep))
        };
        let connector = if preconditions.is_empty() {
            " ".to_string()
        } else {
            and_sep
        };
        format!("{quantifiers}{preconditions}{connector}{body}")
    } else if quantifier_vars.is_empty() {
        body
    } else {
        let quantifiers = format!("{} {quantifier_vars},", lean_sym::FORALL);
        let implies_sep = format!("\n  {}\n  ", lean_sym::IMPLIES);
        let preconditions = if precondition_parts.is_empty() {
            String::new()
        } else {
            format!("\n  {}", precondition_parts.join(&implies_sep))
        };
        let arrow = if preconditions.is_empty() {
            String::new()
        } else {
            implies_sep
        };
        format!("{quantifiers}{preconditions}{arrow}{body}")
    }
}

fn format_aggregate_theorem(
    theorem_name: &str,
    propositions: &[String],
    proof_names: &[String],
) -> String {
    let proposition = lean_prop_conjunction(propositions);
    let proof = match proof_names {
        [] => "trivial".to_string(),
        [single] => single.clone(),
        _ => format!(
            "{}{}{}",
            lean_sym::ANGLE_OPEN,
            proof_names.join(", "),
            lean_sym::ANGLE_CLOSE
        ),
    };
    format!("\ntheorem {theorem_name} :\n  {proposition} :=\n  by exact {proof}\n")
}

fn finite_case_theorem_name(base: &str, index: usize) -> String {
    format!("{base}_case_{index:03}")
}

fn format_ground_theorem_family(
    form: &TheoremForm,
    lean_test_name: &str,
    prog: &str,
    arg_exprs: &[String],
    case_tactic: Option<&'static str>,
) -> String {
    let mut out = format_mode_context(form, prog);
    let mut correctness_props = Vec::new();
    let mut correctness_names = Vec::new();
    let mut termination_props = Vec::new();
    let mut termination_names = Vec::new();

    for (index, arg_expr) in arg_exprs.iter().enumerate() {
        let theorem_name = finite_case_theorem_name(lean_test_name, index);
        let correctness_body = form.correctness.format(arg_expr);
        let correctness_tactic = case_tactic.unwrap_or_else(|| form.correctness_tactic());
        out.push_str(&format!(
            "theorem {theorem_name} :\n  {correctness_body} :=\n  by {correctness_tactic}\n"
        ));
        correctness_props.push(correctness_body);
        correctness_names.push(theorem_name.clone());

        if let Some(ref term_body) = form.termination {
            let term_name = format!("{theorem_name}_alwaysTerminating");
            let term_body_str = term_body.format(arg_expr);
            let term_tactic = case_tactic.unwrap_or_else(|| term_body.tactic());
            out.push_str(&format!(
                "\ntheorem {term_name} :\n  {term_body_str} :=\n  by {term_tactic}\n"
            ));
            termination_props.push(term_body_str);
            termination_names.push(term_name);
        }
        out.push('\n');
    }

    out.push_str(&format_aggregate_theorem(
        lean_test_name,
        &correctness_props,
        &correctness_names,
    ));
    if form.termination.is_some() {
        out.push_str(&format_aggregate_theorem(
            &format!("{lean_test_name}_alwaysTerminating"),
            &termination_props,
            &termination_names,
        ));
    }
    out
}

/// Generate an equivalence theorem proving that the property wrapper and
/// validator handler produce the same result for all inputs.
fn equivalence_goal(
    test: &ExportedPropertyTest,
    prop_prog: &str,
    handler_prog: &str,
    arg_expr: &str,
) -> String {
    use crate::export::TestReturnMode;
    use aiken_lang::ast::OnTestFailure;

    match (&test.return_mode, &test.on_test_failure) {
        (TestReturnMode::Bool, _) => {
            format!("proveTests {prop_prog} ({arg_expr}) = proveTests {handler_prog} ({arg_expr})")
        }
        (TestReturnMode::Void, OnTestFailure::FailImmediately) => format!(
            "proveTestsHalt {prop_prog} ({arg_expr}) {IFF} proveTestsHalt {handler_prog} ({arg_expr})",
            IFF = lean_sym::IFF,
        ),
        (TestReturnMode::Void, OnTestFailure::SucceedEventually)
        | (TestReturnMode::Void, OnTestFailure::SucceedImmediately) => format!(
            "proveTestsError {prop_prog} ({arg_expr}) {IFF} proveTestsError {handler_prog} ({arg_expr})",
            IFF = lean_sym::IFF,
        ),
    }
}

fn format_equivalence_theorem(
    test: &ExportedPropertyTest,
    lean_test_name: &str,
    prop_prog: &str,
    handler_prog: &str,
    arg_expr: &str,
    quantifier_vars: &str,
    precondition_parts: &[String],
) -> String {
    let quantifiers = format!("{} {quantifier_vars},", lean_sym::FORALL);
    let implies_sep = format!("\n  {}\n  ", lean_sym::IMPLIES);
    let preconditions = if precondition_parts.is_empty() {
        String::new()
    } else {
        format!("\n  {}", precondition_parts.join(&implies_sep))
    };
    let arrow = if preconditions.is_empty() {
        String::new()
    } else {
        implies_sep
    };
    let equivalence_goal = equivalence_goal(test, prop_prog, handler_prog, arg_expr);
    format!(
        "\ntheorem {lean_test_name}_equivalence :\n  \\
         {quantifiers}{preconditions}{arrow}\\
         {equivalence_goal} :=\n  \\
         by blaster\n"
    )
}

fn determine_theorem_form(
    test: &ExportedPropertyTest,
    existential_mode: ExistentialMode,
) -> miette::Result<TheoremForm> {
    use crate::export::TestReturnMode;
    use aiken_lang::ast::OnTestFailure;

    let return_mode_expr = lean_return_mode_expr(&test.return_mode);
    let test_mode_expr = lean_test_mode_expr(ManifestTestMode::from_on_test_failure(
        &test.on_test_failure,
    ));
    let decode_policy_expr = lean_decode_policy_expr(generated_decode_policy());

    match (&test.return_mode, &test.on_test_failure) {
        (TestReturnMode::Bool, OnTestFailure::FailImmediately) => Ok(TheoremForm {
            correctness: TheoremBody::Succeeds,
            termination: Some(TheoremBody::ReturnsValue),
            existential: false,
            existential_mode: None,
            return_mode_expr,
            test_mode_expr,
            decode_policy_expr,
        }),
        (TestReturnMode::Bool, OnTestFailure::SucceedEventually) => Ok(TheoremForm {
            correctness: TheoremBody::Fails,
            termination: Some(TheoremBody::ReturnsValue),
            existential: false,
            existential_mode: None,
            return_mode_expr,
            test_mode_expr,
            decode_policy_expr,
        }),
        (TestReturnMode::Bool, OnTestFailure::SucceedImmediately) => Ok(TheoremForm {
            correctness: TheoremBody::Fails,
            termination: None,
            existential: true,
            existential_mode: Some(existential_mode),
            return_mode_expr,
            test_mode_expr,
            decode_policy_expr,
        }),
        (TestReturnMode::Void, OnTestFailure::FailImmediately) => Ok(TheoremForm {
            correctness: TheoremBody::Succeeds,
            termination: None,
            existential: false,
            existential_mode: None,
            return_mode_expr,
            test_mode_expr,
            decode_policy_expr,
        }),
        (TestReturnMode::Void, OnTestFailure::SucceedEventually) => Ok(TheoremForm {
            correctness: TheoremBody::Fails,
            termination: None,
            existential: false,
            existential_mode: None,
            return_mode_expr,
            test_mode_expr,
            decode_policy_expr,
        }),
        (TestReturnMode::Void, OnTestFailure::SucceedImmediately) => Ok(TheoremForm {
            correctness: TheoremBody::Fails,
            termination: None,
            existential: true,
            existential_mode: Some(existential_mode),
            return_mode_expr,
            test_mode_expr,
            decode_policy_expr,
        }),
    }
}

fn ensure_target_kind_compatible(
    test: &ExportedPropertyTest,
    target: &VerificationTargetKind,
) -> miette::Result<()> {
    match target {
        VerificationTargetKind::PropertyWrapper => Ok(()),
        VerificationTargetKind::ValidatorHandler | VerificationTargetKind::Equivalence => {
            let vt = test.validator_target.as_ref().ok_or_else(|| {
                unsupported_error(
                    "E0050",
                    UnsupportedReason::ValidatorTargetMissing {
                        test_name: test.name.clone(),
                    },
                )
            })?;
            if vt.handler_program.is_none() {
                return Err(unsupported_error(
                    "E0050",
                    UnsupportedReason::ValidatorTargetMissing {
                        test_name: test.name.clone(),
                    },
                ));
            }
            Ok(())
        }
    }
}

/// True when the fuzzer's output type contains any `ByteString` / `String`
/// leaf. Blaster currently cannot quantify over these types because the
/// Lean encoding depends on `BitVec` instance parameters that Z3 cannot
/// synthesize, so we skip the test at proof-generation time.
fn fuzzer_output_type_uses_bytestring(ty: &FuzzerOutputType) -> bool {
    match ty {
        FuzzerOutputType::ByteArray | FuzzerOutputType::String => true,
        FuzzerOutputType::List(inner) => fuzzer_output_type_uses_bytestring(inner),
        FuzzerOutputType::Tuple(items) => items.iter().any(fuzzer_output_type_uses_bytestring),
        FuzzerOutputType::Pair(a, b) => {
            fuzzer_output_type_uses_bytestring(a) || fuzzer_output_type_uses_bytestring(b)
        }
        FuzzerOutputType::Int
        | FuzzerOutputType::Bool
        | FuzzerOutputType::Data
        | FuzzerOutputType::Unsupported(_) => false,
    }
}

/// Returns `true` if this test's ByteString use requires skipping.
///
/// We skip when:
/// - The semantics is an unbounded `ByteArrayRange` (`min_len: None` or `max_len: None`) —
///   no length constraint is available to generate a useful precondition.
/// - `String` — skipped unless semantics proves an exact finite literal domain.
/// - `ByteArray` appears as a `List` element — list-of-bytes requires per-element byte
///   reasoning which Blaster cannot handle.
///
/// We do NOT skip exact finite `String` semantics or bounded scalar
/// `ByteArrayRange { min_len: Some(_), max_len: Some(_) }`. Finite strings
/// use ground case theorems; bounded byte arrays use a length precondition,
/// which is sufficient when the test body does not inspect byte contents.
fn bytestring_skip_required(sem: &FuzzerSemantics, ty: &FuzzerOutputType) -> bool {
    match (sem, ty) {
        (FuzzerSemantics::Exact(FuzzerExactValue::String(_)), FuzzerOutputType::String) => false,
        (FuzzerSemantics::OneOf(values), FuzzerOutputType::String)
            if values
                .iter()
                .all(|value| matches!(value, FuzzerExactValue::String(_))) =>
        {
            false
        }
        // Bounded scalar ByteArray: proceed
        (
            FuzzerSemantics::ByteArrayRange {
                min_len: Some(_),
                max_len: Some(_),
            },
            FuzzerOutputType::ByteArray,
        ) => false,
        // Everything else that uses bytestring: skip
        _ => fuzzer_output_type_uses_bytestring(ty),
    }
}

/// Returns `true` if `ty` is or contains a `List<Bool>` (at any nesting level).
///
/// `Bool` elements inside a list encode as `Data.Constr (if x_i then 1 else 0) []`.
/// The conditional creates if-then-else terms in Z3's internal representation that,
/// when combined with the CEK machine encoding, produce SMT formulas Z3 cannot
/// decide within the allowed timeout — even when the list length is bounded.
/// Scalar `Bool` proofs work fine; the problem is specific to `Bool` as a *list
/// element* type.
fn fuzzer_output_type_has_bool_list_element(ty: &FuzzerOutputType) -> bool {
    match ty {
        FuzzerOutputType::List(inner) => {
            matches!(inner.as_ref(), FuzzerOutputType::Bool)
                || fuzzer_output_type_has_bool_list_element(inner)
        }
        FuzzerOutputType::Tuple(items) => {
            items.iter().any(fuzzer_output_type_has_bool_list_element)
        }
        FuzzerOutputType::Pair(a, b) => {
            fuzzer_output_type_has_bool_list_element(a)
                || fuzzer_output_type_has_bool_list_element(b)
        }
        _ => false,
    }
}

fn finite_domain_too_large_error(
    test_name: &str,
    cases: impl Into<String>,
    cap: usize,
    cap_constant: &'static str,
) -> miette::Report {
    unsupported_error(
        "E0034",
        UnsupportedReason::FiniteDomainTooLarge {
            test_name: test_name.to_string(),
            cases: cases.into(),
            cap,
            cap_constant,
        },
    )
}

fn finite_domain_target_mode_error(
    test_name: &str,
    target: &VerificationTargetKind,
) -> miette::Report {
    unsupported_error(
        "E0035",
        UnsupportedReason::FiniteDomainTargetModeUnsupported {
            test_name: test_name.to_string(),
            target: target.to_string(),
        },
    )
}

fn finite_domain_existential_error(test_name: &str) -> miette::Report {
    unsupported_error(
        "E0036",
        UnsupportedReason::FiniteDomainExistentialUnsupported {
            test_name: test_name.to_string(),
        },
    )
}

fn semantics_output_type_mismatch_error(test_name: &str) -> miette::Report {
    unsupported_error(
        "E0044",
        UnsupportedReason::SemanticsOutputTypeMismatch {
            test_name: test_name.to_string(),
        },
    )
}

fn checked_bool_list_cardinality(min_len: usize, max_len: usize) -> Result<usize, String> {
    if min_len > max_len {
        return Ok(0);
    }

    let mut total = 0usize;
    for len in min_len..=max_len {
        let cases = if len >= usize::BITS as usize {
            MAX_FINITE_THEOREM_INSTANCES_PER_TEST + 1
        } else {
            1usize
                .checked_shl(len as u32)
                .unwrap_or(MAX_FINITE_THEOREM_INSTANCES_PER_TEST + 1)
        };
        total = total
            .checked_add(cases)
            .unwrap_or(MAX_FINITE_THEOREM_INSTANCES_PER_TEST + 1);
        if total > MAX_FINITE_THEOREM_INSTANCES_PER_TEST {
            return Err(total.to_string());
        }
    }

    Ok(total)
}

fn supported_top_level_bool_list_cardinality(
    ty: &FuzzerOutputType,
    sem: &FuzzerSemantics,
) -> Result<Option<usize>, String> {
    match (ty, sem) {
        (
            FuzzerOutputType::List(elem_type),
            FuzzerSemantics::List {
                element,
                min_len: Some(min_len),
                max_len: Some(max_len),
            },
        ) if matches!(elem_type.as_ref(), FuzzerOutputType::Bool)
            && matches!(element.as_ref(), FuzzerSemantics::Bool) =>
        {
            checked_bool_list_cardinality(*min_len, *max_len).map(Some)
        }
        _ => Ok(None),
    }
}

fn finite_string_arg_exprs(
    test_name: &str,
    semantics: &FuzzerSemantics,
) -> miette::Result<Option<Vec<String>>> {
    let values = match semantics {
        FuzzerSemantics::Exact(FuzzerExactValue::String(value)) => {
            vec![FuzzerExactValue::String(value.clone())]
        }
        FuzzerSemantics::OneOf(values) => {
            if values.is_empty() {
                return Err(semantics_output_type_mismatch_error(test_name));
            }
            if !values
                .iter()
                .all(|value| matches!(value, FuzzerExactValue::String(_)))
            {
                return Err(semantics_output_type_mismatch_error(test_name));
            }
            values.clone()
        }
        _ => return Ok(None),
    };

    if values.len() > MAX_FINITE_THEOREM_INSTANCES_PER_TEST {
        return Err(finite_domain_too_large_error(
            test_name,
            values.len().to_string(),
            MAX_FINITE_THEOREM_INSTANCES_PER_TEST,
            "MAX_FINITE_THEOREM_INSTANCES_PER_TEST",
        ));
    }

    let args = values
        .iter()
        .map(|value| match value {
            FuzzerExactValue::String(value) => {
                let lit = lean_bytestring_literal_from_bytes_qualified(value.as_bytes());
                format!("stringArg ({lit})")
            }
            _ => unreachable!("validated above"),
        })
        .collect();
    Ok(Some(args))
}

fn bool_list_arg_exprs(min_len: usize, max_len: usize) -> Vec<String> {
    let mut args = Vec::new();
    for len in min_len..=max_len {
        let patterns = if len >= usize::BITS as usize {
            0
        } else {
            1usize << len
        };
        for mask in 0..patterns {
            let items = (0..len)
                .map(|index| {
                    if ((mask >> index) & 1) == 1 {
                        "Data.Constr 1 []".to_string()
                    } else {
                        "Data.Constr 0 []".to_string()
                    }
                })
                .collect::<Vec<_>>()
                .join(", ");
            args.push(format!("[Term.Const (Const.Data (Data.List [{items}]))]"));
        }
    }
    args
}

/// Shared header for every Lean proof file emitted by the verifier.
///
/// Factored out of `generate_proof_file`'s inner closure so the early fast
/// paths (state-machine trace error / halt tests routed via `native_decide`)
/// can construct a header without re-deriving `prog`/`handler_prog` inline.
/// Behavior is identical to the previous closure: same imports, same
/// `#import_uplc` lines, same `extra_opens` injection point.
fn build_direct_header(
    lean_module: &str,
    test_id: &str,
    target: &VerificationTargetKind,
    extra_opens: &str,
) -> String {
    let prog = prog_name(test_id);
    let fuzzer_prog = fuzzer_prog_name(test_id);
    let handler_prog = format!("handler_{}", prog);
    let mut s = format!(
        "import AikenVerify.Utils\nimport PlutusCore.Integer\nimport PlutusCore.ByteString\nimport PlutusCore.UPLC.ScriptEncoding\nimport Blaster\n\n\
         namespace {lean_module}\n\
         open PlutusCore.Integer (Integer)\n\
         open PlutusCore.ByteString (ByteString)\n"
    );
    if !extra_opens.is_empty() {
        s.push_str(extra_opens);
        s.push('\n');
    }
    s.push_str(
        "open PlutusCore.UPLC.CekMachine\n\
         open PlutusCore.UPLC.Term (Term Const Program)\n\
         open AikenVerify.Utils\n",
    );
    // PlutusCore's `#import_uplc` syntax is: `<identifier> <lang> <format> <filepath>`.
    // Aiken's config layer enforces Plutus V3 only (see `validate_v3_only` in config.rs),
    // so the language argument is always `PlutusV3`.
    s.push_str(&format!(
        "\n#import_uplc {prog} PlutusV3 single_cbor_hex \"./cbor/{test_id}.cbor\"\n"
    ));
    s.push_str(&format!(
        "#import_uplc {fuzzer_prog} PlutusV3 single_cbor_hex \"./cbor/{test_id}_fuzzer.cbor\"\n"
    ));
    // For validator/equivalence modes, also import the handler program
    match target {
        VerificationTargetKind::ValidatorHandler | VerificationTargetKind::Equivalence => {
            s.push_str(&format!(
                "#import_uplc {handler_prog} PlutusV3 single_cbor_hex \"./cbor/{test_id}_handler.cbor\"\n"
            ));
        }
        _ => {}
    }
    s.push('\n');
    s
}

/// S4 — emit the two-phase halt theorem file
/// (`isValidTransition` + `isValidTrace` + `_valid_input` + `_halts` + composition),
/// gated behind `AIKEN_EMIT_TWO_PHASE=1`. Both phases are `sorry`-closed
/// for S4; §S5 will discharge them once `isValidScriptContext` and
/// `packTrace` have real definitions.
///
/// Returns `Ok(Some(content))` when the test matches the halt-test shape
/// and carries a pre-emitted `transition_prop_lean`. Returns `Ok(None)`
/// otherwise so the caller falls through to the regular pipeline.
/// The note attached to two-phase halt proofs, surfaced via
/// `ManifestEntry::partial_proof_note` and `ProofStatus::Partial`.
/// Phase 1 (the `_valid_input` obligation) is closed by construction; Phase 2
/// (the CEK halt obligation, §S6 in the design doc) is currently `sorry`.
const TWO_PHASE_PARTIAL_NOTE: &str = "Phase 2 (CEK halt obligation) is sorry-closed — see §S6";

/// One-shot latch ensuring the `AIKEN_EMIT_TWO_PHASE=0` bypass warning is
/// emitted at most once per process. Without this guard the warning would
/// repeat for every test in a multi-test run, drowning the actual output.
static TWO_PHASE_BYPASS_WARNING_EMITTED: std::sync::OnceLock<()> = std::sync::OnceLock::new();

fn try_generate_two_phase_proof(
    test: &ExportedPropertyTest,
    lean_test_name: &str,
    verify_prog: &str,
    direct_header: &dyn Fn(&str) -> String,
    footer: &str,
    allow_vacuous_subgenerators: bool,
) -> miette::Result<Option<String>> {
    // Only applicable to state-machine trace halt tests that carry a
    // pre-emitted `TransitionProp`.
    if !is_state_machine_trace_halt_test(test) {
        return Ok(None);
    }
    let Some(tp) = test.transition_prop_lean.as_ref() else {
        return Ok(None);
    };

    // S0002 dispatch: an `Opaque` carrying the constructor-tag-unresolved
    // marker may have been buried inside the `TransitionProp` (e.g. as
    // `EqOutput(Opaque{S0002})`) and silently widened to a fresh
    // existential by `emit_shallow_ir_as_lean_data`. The marker survives
    // the widening through the `s0002_marker` side channel populated at
    // export time. Dispatch the hard, non-skippable `S0002` error BEFORE
    // the generic vacuity guard below, which would otherwise mask the
    // root cause as a skippable `FallbackRequired`.
    if let Some((ctor, type_name)) = tp.s0002_marker.as_ref() {
        return Err(miette::Report::new(error_catalogue::unsupported(
            "S0002",
            UnsupportedReason::ConstructorTagUnresolved {
                test_name: test.name.clone(),
                ctor: ctor.clone(),
                type_name: type_name.clone(),
            },
        )));
    }

    // M3 — Guard: if the source `TransitionProp` is structurally vacuous,
    // the lowered Lean `isValidTransition` predicate is universally
    // provable and the two-phase theorem would be misleading.  Promote
    // to SKIPPED rather than emitting a useless proof.
    //
    // The verdict is pre-computed at export time
    // (`lib::convert_semantics`) by
    // `aiken_lang::test_framework::transition_prop_is_vacuous`, walking
    // the source AST.  The previous text-based predicate
    // (`transition_body_is_vacuous`) is preserved as a `#[cfg(test)]`
    // drift sentinel: the structural verdict and the rendered-Lean
    // verdict agree on a corpus of representative props (asserted in
    // `verify::tests::structural_vacuity_agrees_with_text_drift_sentinel`).
    if tp.is_vacuous {
        return Err(generation_error(
            GenerationErrorCategory::FallbackRequired,
            format!(
                "Test '{name}': isValidTransition body is structurally vacuous — \
                 the transition predicate is universally provable and the two-phase \
                 theorem would be misleading. SKIPPED (vacuous TransitionProp body).",
                name = test.name
            ),
        ));
    }

    // H4: hard-error E0018 when the TransitionProp references sub-generators
    // whose bodies cannot be statically inlined.  Previously we emitted
    // `opaque {pred} : Data → Data → Prop` for each such sub-generator,
    // which Lean's `Inhabited Prop` instance silently fills with
    // `fun _ _ => True`.  Every constraint over the sub-generator domain
    // therefore became trivially provable, producing a vacuous "PROVED"
    // verdict for any test referencing one.
    //
    // Default behaviour (and ALL production runs): refuse to emit the
    // proof.  Surfaces as `E0018` (`FallbackRequired`, skippable via
    // `--skip-unsupported`).  The `sub_generator` field carries the first
    // unresolved entry as `module::fn_name` so the error message names a
    // concrete site to fix.
    //
    // Debug-only escape hatch: the hidden `--allow-vacuous-subgenerators`
    // CLI flag (forwarded as `allow_vacuous_subgenerators` here) restores
    // the legacy widening, but with `opaque` replaced by
    // `def := fun _ _ => True` (an explicit, auditable widening rather
    // than the silent `Inhabited Prop` fill).  In that mode the proof is
    // still emitted, but the verdict is forced to `Partial` via
    // `ProofCaveat::Partial("opaque sub-generator stubbed to True")` so
    // it never appears as a clean `Proved`.
    if !tp.opaque_sub_generators.is_empty() && !allow_vacuous_subgenerators {
        let (first_module, first_fn) = &tp.opaque_sub_generators[0];
        return Err(miette::Report::new(error_catalogue::unsupported(
            "E0018",
            UnsupportedReason::OpaqueSubgeneratorStub {
                test_name: test.name.clone(),
                sub_generator: format!("{first_module}::{first_fn}"),
            },
        )));
    }
    let theorem_widenings = transition_prop_widenings(tp);
    let disallowed_widenings = disallowed_production_widening_counts(
        &theorem_widenings,
        allow_vacuous_subgenerators,
        &tp.opaque_sub_generators,
    );
    if !disallowed_widenings.is_empty() {
        return Err(generation_error(
            GenerationErrorCategory::FallbackRequired,
            format!(
                "Test '{}': two-phase proof would rely on transition widenings (disallowed non-domain widenings: {}); refusing universal proof until the transition relation is exact.",
                test.name,
                render_widening_counts(&disallowed_widenings),
            ),
        ));
    }

    let helper_prefix = format!("{lean_test_name}_s4");
    let is_valid_transition_name = format!("{helper_prefix}_isValidTransition");
    let is_valid_trace_name = format!("{helper_prefix}_isValidTrace");
    let pack_trace_name = format!("{helper_prefix}_packTrace");
    let ctx_pred_name = format!("{helper_prefix}_isValidScriptContext");
    let FuzzerSemantics::StateMachineTrace {
        acceptance,
        output_semantics,
        ..
    } = &test.semantics
    else {
        return Ok(None);
    };
    let (trace_length_bounds, step_depth_bounds) =
        scenario_trace_bounds_from_output_semantics(acceptance, output_semantics.as_ref());
    let (trace_within_bounds, trace_depth_within_bounds, trace_bound_defs) =
        emit_scenario_trace_bound_helpers(&helper_prefix, &trace_length_bounds, &step_depth_bounds);

    // Substitute the placeholder function name and the sub-generator prefix
    // that were emitted at export time with the concrete helper-prefixed names.
    let sg_prefix = format!("{helper_prefix}_");
    let is_valid_transition_def = tp
        .is_valid_transition_def
        .replace("__FN__", &is_valid_transition_name)
        .replace("__SGPFX__", &sg_prefix);

    let mut content = direct_header(
        "open PlutusCore.Data (Data)\nopen PlutusCore.ByteString (ByteString)\nopen PlutusCore.Integer (Integer)\nopen PlutusCore.UPLC.CekMachine (State cekExecuteProgram)",
    );

    content.push_str("set_option maxRecDepth 16384\nset_option maxHeartbeats 2000000\n\n");

    // S4 audit: log every constraint that was dropped or widened during
    // transition/helper lowering. Each entry widens the generated theorem's
    // preconditions; the proof is sound but strictly weaker than a faithful
    // translation.
    let audit_counts = widening_category_counts(
        &theorem_widenings,
        allow_vacuous_subgenerators,
        &tp.opaque_sub_generators,
    );
    let audit_total: usize = audit_counts.values().sum();
    content.push_str(&format!(
        "-- S4 AUDIT: {} constraints dropped or widened during transition/helper lowering (sound over-approximation).\n\
         -- Each dropped constraint widens the generated theorem's preconditions.\n",
        audit_total
    ));
    if !theorem_widenings.is_empty() {
        content.push_str("-- Dropped or widened constraints:\n");
        for widening in &theorem_widenings {
            content.push_str(&format!("--   {}\n", widening.message));
        }
    }
    content.push('\n');

    // S5: `packTrace` converts a list of trace-element Data values into the
    // single Data value that the validator's CEK machine receives.
    // The runtime scenario framework passes the trace as a Plutus `Data.List`
    // value via UPLC's `apply_data` — so `packTrace trace = Data.List trace`.
    // This concrete definition (not an axiom) lets Phase 1 be proved by
    // construction (`rfl`) and is the prerequisite for Phase 2.
    // Phase 2 remains `sorry`-closed because discharging the universal CEK
    // halting claim also requires a step-function reduction lemma relating
    // `proveTestsHalt` to `isValidTrace` — not yet implemented (see §S6).
    content.push_str(
        "-- S5: packTrace encodes a trace as a Plutus `Data.List` value,\n\
         -- matching the runtime `apply_data` call that wraps the trace argument.\n\
         -- isValidScriptContext is defined as the image of isValidTrace under packTrace;\n\
         -- with this definition Phase 1 is provable by construction (no `sorry` needed).\n\
         -- Phase 2 (the CEK halting obligation) remains sorry-closed:\n\
         -- universally discharging it requires a step-function CEK reduction lemma\n\
         -- relating proveTestsHalt to isValidTrace (see §S6; not yet implemented).\n",
    );
    content.push_str(&format!(
        "def {pack_trace_name} (trace : List Data) : Data := Data.List trace\n\n"
    ));
    content.push_str(&trace_bound_defs);

    // S6 / H4: emit predicate stubs for sub-generators whose bodies could
    // not be inlined.  This branch is only reachable when
    // `--allow-vacuous-subgenerators` (debug-only) is on — production runs
    // hard-error with `E0018` above before reaching this point.
    //
    // Each sub-generator contributes one widened predicate
    // `def {prefix}_{fn_name}_prop : Data → Data → Prop := fun _ _ => True`.
    // We use `def := fun _ _ => True` rather than `opaque ... : Prop`
    // because the latter is silently filled by Lean's `Inhabited Prop`
    // instance (also `True`), but invisibly — the explicit `def` form
    // makes the widening auditable in the generated proof file, and the
    // trailing `[WIDENED: ...]` comment matches what the H4 plan
    // §"Debug mode" specifies.  The caller forces the verdict to
    // `Partial` via `ProofCaveat::Partial("opaque sub-generator stubbed
    // to True")`, so the proof is never reported as a clean `Proved`.
    if !tp.opaque_sub_generators.is_empty() {
        content.push_str(
            "-- S6 / H4 [DEBUG]: --allow-vacuous-subgenerators is on; sub-generator predicates\n\
             -- are widened to `fun _ _ => True`.  Each widening makes every constraint over the\n\
             -- sub-generator domain trivially provable, so the verdict is forced to `Partial`.\n",
        );
        for (module, fn_name) in &tp.opaque_sub_generators {
            let sanitized = sanitize_lean_ident(fn_name);
            let pred_name = format!("{helper_prefix}_{sanitized}_prop");
            content.push_str(&format!(
                "-- From Aiken module: {module}\n\
                 def {pred_name} : Data \u{2192} Data \u{2192} Prop := fun _ _ => True \
                 -- [WIDENED: sub-generator stubbed]\n"
            ));
        }
        content.push('\n');
    }

    // Emit the `isValidTransition` definition (derived from TransitionProp).
    content.push_str(&is_valid_transition_def);
    content.push('\n');

    // Emit `isValidTrace`. When an initial-state literal is available,
    // pin the trace to that starting state; otherwise fall back to an
    // unconstrained existential (sound widening).
    let initial_state_expr = tp
        .initial_state_lean
        .clone()
        .map(|s| format!("({s})"))
        .unwrap_or_else(|| "_initState".to_string());

    if tp.initial_state_lean.is_some() {
        content.push_str(&format!(
            "def {is_valid_trace_name} (trace : List Data) : Prop :=\n  {trace_within_bounds} trace {AND}\n  {trace_depth_within_bounds} trace {AND}\n  {FORALL} (i : Fin trace.length),\n    {is_valid_transition_name} {initial_state_expr} (trace.get i)\n\n",
            AND = lean_sym::AND,
            FORALL = lean_sym::FORALL,
        ));
    } else {
        content.push_str(&format!(
            "def {is_valid_trace_name} (trace : List Data) : Prop :=\n  {trace_within_bounds} trace {AND}\n  {trace_depth_within_bounds} trace {AND}\n  {EXISTS} (_initState : Data),\n  {FORALL} (i : Fin trace.length),\n    {is_valid_transition_name} _initState (trace.get i)\n\n",
            AND = lean_sym::AND,
            FORALL = lean_sym::FORALL,
            EXISTS = lean_sym::EXISTS,
        ));
    }

    // S5: isValidScriptContext is defined as the image of isValidTrace under
    // packTrace. Phase 1 is then trivially provable: given any valid trace,
    // we exhibit that trace itself as the witness for the existential.
    content.push_str(&format!(
        "-- S5: image definition — {ctx_pred_name} x holds iff x = packTrace(trace)\n\
         -- for some isValidTrace-satisfying trace.\n\
         def {ctx_pred_name} (x : Data) : Prop :=\n  \
           {EXISTS} (trace : List Data),\n    \
           {is_valid_trace_name} trace \u{2227} x = {pack_trace_name} trace\n\n",
        EXISTS = lean_sym::EXISTS,
    ));

    // Emit the two-phase theorems.
    content.push_str(&emit_two_phase_halt_theorems(
        &helper_prefix,
        lean_test_name,
        verify_prog,
    ));

    content.push_str(footer);
    Ok(Some(content))
}

fn ensure_equivalence_has_universal_proof(
    target: &VerificationTargetKind,
    test_name: &str,
    limitation: &str,
    hint: &str,
) -> miette::Result<()> {
    if matches!(target, VerificationTargetKind::Equivalence) {
        return Err(generation_error(
            GenerationErrorCategory::FallbackRequired,
            format!(
                "Test '{}' targets equivalence, but {}. {}",
                test_name, limitation, hint,
            ),
        ));
    }

    Ok(())
}

/// Generate the Lean proof file content for a single test.
///
/// The second tuple element carries the proof caveat:
/// - `ProofCaveat::None` — universal proof, no open obligations.
/// - `ProofCaveat::Partial(note)` — emitted file contains an open
///   sub-obligation (today, exclusively the two-phase state-machine halt
///   path which closes Phase 2 with `sorry`).
/// - `ProofCaveat::Witness(WitnessProofNote { … })` — emitted file proves
///   the property only on the listed concrete witness instances rather
///   than universally. Surfaced for state-machine `native_decide` halt and
///   error proofs (the universal `∀ x, reachable x → proveTestsHalt …` /
///   `proveTestsError …` theorem is unsound because `reachable`
///   over-approximates the step function).
fn generate_proof_file(
    test: &ExportedPropertyTest,
    test_id: &str,
    lean_test_name: &str,
    lean_module: &str,
    existential_mode: ExistentialMode,
    target: &VerificationTargetKind,
    allow_vacuous_subgenerators: bool,
) -> miette::Result<(String, ProofCaveat)> {
    ensure_target_kind_compatible(test, target)?;

    // S0003 (H5): hard-error any per-test combination of
    // `--existential-mode witness` and a non-`Bool` fuzzer-output domain
    // *before* any Lean emission, scaffolding, or proof-strategy dispatch.
    //
    // Witness mode synthesises a deterministic trivial witness (`0` for `Int`,
    // `[]` for `List`, `Data.I 0` for `Data`, …) and proves the property body
    // holds at that witness. For non-`Bool` domains the trivial witness need
    // not be a valid counterexample, so the resulting `∃ x, ¬ P x` proof is
    // unsound — every `fail_once` test would silently appear PROVED.
    //
    // The guard fires only on `SucceedImmediately` tests (the existential
    // form): for `FailImmediately` / `SucceedEventually` `existential_mode`
    // is ignored downstream (`determine_theorem_form` produces `None`), so
    // there is no soundness exposure to gate.
    //
    // The category is `UnsoundFallback`, which `is_skippable_generation_error`
    // rejects: `--skip-unsupported` cannot suppress S0003.
    {
        use aiken_lang::ast::OnTestFailure;
        if matches!(existential_mode, ExistentialMode::Witness)
            && matches!(test.on_test_failure, OnTestFailure::SucceedImmediately)
            && !witness_mode_sound_for(&test.fuzzer_output_type)
        {
            return Err(miette::Report::new(error_catalogue::unsupported(
                "S0003",
                UnsupportedReason::ExistentialWitnessUnsoundForDomain {
                    test_name: test.name.clone(),
                    domain: format_fuzzer_output_domain(&test.fuzzer_output_type),
                },
            )));
        }
    }
    if matches!(target, VerificationTargetKind::Equivalence)
        && matches!(existential_mode, ExistentialMode::Witness)
        && matches!(
            test.on_test_failure,
            aiken_lang::ast::OnTestFailure::SucceedImmediately
        )
    {
        return Err(generation_error(
            GenerationErrorCategory::FallbackRequired,
            format!(
                "Test '{}' targets equivalence, but --existential-mode witness only proves concrete witness instances. Use --existential-mode proof for an equivalence theorem.",
                test.name
            ),
        ));
    }

    // Fast path for state-machine trace tests that we discharge via concrete
    // `native_decide` witnesses rather than Blaster. Those paths never ask
    // Blaster to universally quantify over any ByteString/String, so the
    // ByteString skip gate below is irrelevant — bypass it entirely when we
    // have the witnesses we need. Without this bypass, `_ko`-shaped tests
    // (Void + SucceedEventually) whose fuzzer output contains a `List<String>`
    // (labels) are skipped before their witness-based proof can be emitted.
    //
    // Halt tests also use the witness path, but when `step_function_ir` is
    // available they may *additionally* try a universal Blaster proof, so we
    // do NOT bypass the gate for halt tests here: the existing routing below
    // handles both the Blaster-first path (with step IR) and the pure-witness
    // fallback. Error tests (_ko) don't have a Blaster universal-proof path
    // at all today, so we can always bypass when witnesses exist.
    if is_state_machine_trace_error_test(test) && !test.concrete_error_witnesses.is_empty() {
        ensure_equivalence_has_universal_proof(
            target,
            &test.name,
            "the selected state-machine error path only has witness-only proofs",
            "No universal equivalence theorem can be emitted until a sound trace-level proof exists.",
        )?;

        let prog = prog_name(test_id);
        let handler_prog = format!("handler_{}", prog);
        let verify_prog = match target {
            VerificationTargetKind::ValidatorHandler => handler_prog.clone(),
            _ => prog.clone(),
        };
        let lean_module_owned = lean_module.to_string();
        let test_id_owned = test_id.to_string();
        let target_owned = target.clone();
        let direct_header = move |extra_opens: &str| -> String {
            build_direct_header(
                &lean_module_owned,
                &test_id_owned,
                &target_owned,
                extra_opens,
            )
        };
        let footer = format!("\nend {lean_module}\n");
        return generate_state_machine_error_proof_file(
            test,
            lean_test_name,
            &verify_prog,
            &direct_header,
            &footer,
        )
        .map(|content| {
            let instances = test.concrete_error_witnesses.len();
            let witnesses = test.concrete_error_witnesses.clone();
            let plural = if instances == 1 { "" } else { "es" };
            (
                content,
                ProofCaveat::Witness(WitnessProofNote {
                    instances,
                    witnesses,
                    note: format!(
                        "state-machine error test '{}' verified on {} concrete fuzzer-seed-{} \
                         witness{} only (universal proof of `∀ x, reachable x → \
                         proveTestsError prog (dataArg x)` is not claimed because `reachable` \
                         is a global over-approximation of the step function)",
                        test.name, instances, CONCRETE_WITNESS_BASE_SEED, plural,
                    ),
                }),
            )
        });
    }
    if is_state_machine_trace_error_test(test) {
        return Err(miette::Report::new(error_catalogue::unsupported(
            "E0024",
            UnsupportedReason::StateMachineNoWitnessSynthesizable {
                test_name: test.name.clone(),
            },
        )));
    }

    // Blaster (Z3) currently rejects universal quantification over Lean's
    // `ByteString` / `String` (both projected to Plutus `Data.B`) because
    // the encoding requires instance parameters on a dependent inductive,
    // which Z3's bitvector theory cannot synthesize.
    //
    // Exceptions: exact finite `String` semantics are routed to ground
    // theorem families, and bounded scalar
    // `ByteArrayRange { min_len: Some(_), max_len: Some(_) }` is allowed
    // through because Blaster can translate `ByteString.length` via the
    // `String.length` path. See `bytestring_skip_required` for the carve-out
    // logic.
    //
    // Skip the remaining cases with `FallbackRequired` so `--skip-unsupported`
    // treats them as skipped rather than as generation failures.
    let sampler_fallback = sampler_relation_supported(test, target, existential_mode);
    let replay_existential_fallback =
        should_use_replay_backed_existential_domain(test, target, existential_mode);
    if !sampler_fallback
        && !replay_existential_fallback
        && bytestring_skip_required(&test.semantics, &test.fuzzer_output_type)
    {
        return Err(unsupported_error(
            "E0011",
            UnsupportedReason::UnboundedBytearray {
                test_name: test.name.clone(),
            },
        ));
    }

    // Bool-list preflight is centralized here: supported bounded top-level
    // `List<Bool>` shapes pass through to the finite enumerator, unsupported
    // bool-list shapes return E0013, and over-cap bounded shapes return E0034.
    if !sampler_fallback && !replay_existential_fallback {
        match supported_top_level_bool_list_cardinality(&test.fuzzer_output_type, &test.semantics) {
            Ok(Some(_)) => {}
            Ok(None) => {
                if fuzzer_output_type_has_bool_list_element(&test.fuzzer_output_type) {
                    return Err(unsupported_error(
                        "E0013",
                        UnsupportedReason::ListOfBool {
                            test_name: test.name.clone(),
                        },
                    ));
                }
            }
            Err(cases) => {
                return Err(finite_domain_too_large_error(
                    &test.name,
                    cases,
                    MAX_FINITE_THEOREM_INSTANCES_PER_TEST,
                    "MAX_FINITE_THEOREM_INSTANCES_PER_TEST",
                ));
            }
        }
    }

    // Soundness guard: `DataWithSchema` semantics require a `fuzzer_data_schema`
    // to produce a sound theorem. Without it the `Data` domain is unconstrained
    // and the generated theorem collapses to `∀ x : Data, True → P x`, which
    // could be spuriously reported as PROVED for validators that happen to
    // accept all `Data`. Surface as a clean SKIP rather than emitting an
    // unconstrained universal theorem.
    if !replay_existential_fallback
        && fuzzer_semantics_requires_schema(&test.semantics)
        && test.fuzzer_data_schema.is_none()
    {
        return Err(unsupported_error(
            "E0015",
            UnsupportedReason::QualifiedAdtNoSchema {
                test_name: test.name.clone(),
                type_name: test.fuzzer_output_type.to_string(),
            },
        ));
    }

    let form = determine_theorem_form(test, existential_mode)?;

    let prog = prog_name(test_id);
    let handler_prog = format!("handler_{}", prog_name(test_id));

    // Common header for all proof files (shared with the state-machine
    // error fast-path above — see `build_direct_header` for the full body).
    let direct_header = |extra_opens: &str| -> String {
        build_direct_header(lean_module, test_id, target, extra_opens)
    };

    let footer = format!("\nend {lean_module}\n");

    // Select which program to verify based on target mode
    let verify_prog = match target {
        VerificationTargetKind::ValidatorHandler => handler_prog.clone(),
        _ => prog.clone(),
    };
    if should_use_replay_backed_existential_domain(test, target, existential_mode) {
        return generate_replay_backed_fail_once_proof_file(
            test,
            &form,
            lean_test_name,
            &verify_prog,
            &direct_header,
            &footer,
        )
        .map(|content| (content, ProofCaveat::None));
    }

    // S4: two-phase halt theorem emission. When the test is a state-machine
    // trace halt test (`_ok`) that carries a pre-emitted `transition_prop_lean`,
    // emit the `isValidTransition` / `isValidTrace` / `isValidScriptContext`
    // definitions and the two-phase theorem skeleton:
    //
    //   Phase 1 (`_valid_input`) — proved by construction (no `sorry`).
    //   Phase 2 (`_halts`)       — CEK machine obligation, currently `sorry`.
    //   Composition              — links Phase 1 + Phase 2 (proved by construction).
    //
    // This path supersedes the old concrete native_decide witness path for `_ok`
    // tests only as a richer symbolic artifact. It is still reported conservatively
    // as `Partial`: the bounded trace relation remains an acknowledged
    // over-approximation until the step-to-step reachability and ledger-validity
    // obligations are discharged. The `AIKEN_EMIT_TWO_PHASE` env var is retained as
    // an opt-out escape hatch for debugging; setting it to `"0"` disables the
    // two-phase path for that run.
    let two_phase_disabled = std::env::var("AIKEN_EMIT_TWO_PHASE")
        .map(|v| v == "0")
        .unwrap_or(false);
    if two_phase_disabled {
        TWO_PHASE_BYPASS_WARNING_EMITTED.get_or_init(|| {
            eprintln!(
                "WARNING: AIKEN_EMIT_TWO_PHASE=0 is set; two-phase proof emission disabled. \
                 For halt tests with step IR, Blaster will be used instead, which may emit \
                 weaker unbounded artifacts. Remove this variable to restore the bounded partial scenario-proof path."
            );
        });
    }
    if !two_phase_disabled {
        match try_generate_two_phase_proof(
            test,
            lean_test_name,
            &verify_prog,
            &direct_header,
            &footer,
            allow_vacuous_subgenerators,
        ) {
            Ok(Some(content)) => {
                let mut note = TWO_PHASE_PARTIAL_NOTE.to_string();
                if allow_vacuous_subgenerators
                    && test
                        .transition_prop_lean
                        .as_ref()
                        .is_some_and(|tp| !tp.opaque_sub_generators.is_empty())
                {
                    note.push_str("; opaque sub-generator stubbed to True");
                }
                if let Some(tp) = test.transition_prop_lean.as_ref()
                    && let Some(summary) = widening_summary_suffix(
                        &transition_prop_widenings(tp),
                        allow_vacuous_subgenerators,
                        &tp.opaque_sub_generators,
                    )
                {
                    note.push_str("; ");
                    note.push_str(&summary);
                }
                return Ok((content, ProofCaveat::Partial(note)));
            }
            Ok(None) => {}
            Err(e)
                if is_state_machine_trace_halt_test(test)
                    && test.transition_prop_lean.is_some() =>
            {
                return Err(e);
            }
            Err(e) => return Err(e),
        }
    }

    // Intercept state-machine trace halt tests. The universal theorem
    // `∀ x, reachable x → proveTestsHalt prog (dataArg x)` generated by the
    // generic state-machine path is unsound: the `reachable` predicate is a
    // global over-approximation of the step function and admits `Data` values
    // that the validator errors on, so the generic theorem would be *false*
    // and Blaster cannot discharge it.
    //
    // When the compiler has pre-computed a set of concrete fuzzer-generated
    // witnesses that drive the test body to a halting state
    // (`ExportedPropertyTest::concrete_halt_witnesses`), emit one
    // `native_decide` instance theorem per witness instead: the theorem
    // `proveTestsHalt prog (dataArg <literal>)` is a closed computation that
    // Lean can verify by reduction without any SMT step.
    //
    // When the step function's ShallowIr is available, attempt a universal
    // Blaster proof first — it produces a strictly stronger theorem than the
    // concrete native_decide witness approach. If the Blaster path fails
    // (FallbackRequired), fall back to the native_decide path if witnesses
    // are available.
    //
    // When step_function_ir is absent, take the native_decide witness path
    // immediately (unchanged behaviour: it was the only sound option before
    // ShallowIr existed).
    let has_step_ir = matches!(
        &test.semantics,
        FuzzerSemantics::StateMachineTrace {
            step_function_ir: Some(_),
            ..
        }
    );

    // Commit 18 (folds C13 #6): when a state-machine trace HALT test has
    // concrete halt witnesses populated by export-time discharge, this
    // dispatch silently re-routes to the witness-only `native_decide`
    // proof regardless of whether schema-lowering errors (M6) occurred
    // upstream. This is sound — the witness proofs are sound on their
    // concrete inputs and produce the `WitnessProved` verdict (not
    // `Proved`) — but it does suppress M6 errors for this test class
    // (the witness path bypasses semantics emission entirely). Folded
    // into the `WitnessProofNote.note` is the explanation that
    // universal-x reachability is unsound; the absorbed M6 errors do
    // not surface there. If observability of M6 in the witness path
    // matters, instrument a sibling counter rather than promoting M6
    // to a hard error here (which would defeat the witness fallback).

    // Soundness guard: if the step function was analyzed (has_step_ir) but the
    // TransitionProp extraction produced no usable constraints
    // (transition_prop_lean = None, meaning every leaf was Unsupported or the
    // prop was trivially vacuous), the Blaster path below would emit
    //   ∀ x, reachable x → proveTestsHalt prog (dataArg x)
    // where `reachable x` is a global structural over-approximation that
    // admits inputs the validator rejects.  That theorem is *false* in
    // general and Blaster must not be asked to prove it.
    //
    // Universal halt proofs require some extracted transition relation. When
    // the step function was analyzed but `transition_prop_lean` is absent, the
    // remaining reachability theorem would collapse to a structural
    // over-approximation with no extracted step constraint. Reject it rather than
    // silently downgrading to a witness-only result.
    if is_state_machine_trace_halt_test(test) && has_step_ir && test.transition_prop_lean.is_none()
    {
        return Err(generation_error(
            GenerationErrorCategory::FallbackRequired,
            format!(
                "Test '{}': step function body was analysed but produced no \
                 extractable transition constraints — all leaves of the \
                 TransitionProp were unsupported or vacuous. No universal halt theorem can be emitted from this test until transition constraints are recovered soundly.",
                test.name
            ),
        ));
    }

    // Additional soundness guard for the `AIKEN_EMIT_TWO_PHASE=0` bypass: when
    // the two-phase path is disabled, halt tests with step IR and no concrete
    // witnesses must still be blocked even if `transition_prop_lean` is
    // populated.  The fall-through into `try_generate_direct_proof_from_semantics`
    // would build a theorem guarded by the `reachable` over-approximation, which
    // the comment above labels "unsound: would be false in general".  Without
    // witnesses to drive a `native_decide` fallback, reporting PROVED on this
    // path would be misleading.
    if is_state_machine_trace_halt_test(test)
        && has_step_ir
        && test.transition_prop_lean.is_none()
        && two_phase_disabled
    {
        return Err(generation_error(
            GenerationErrorCategory::FallbackRequired,
            format!(
                "Test '{}': AIKEN_EMIT_TWO_PHASE=0 bypasses two-phase proof emission, but \
                 no sound universal direct theorem is available because transition constraints were not recovered.",
                test.name
            ),
        ));
    }

    match try_generate_direct_proof_from_semantics(
        test,
        test_id,
        &form,
        lean_test_name,
        &verify_prog,
        &direct_header,
        &footer,
        target,
        &prog,
        &handler_prog,
    ) {
        Ok(Some(content)) => return Ok((content, ProofCaveat::None)),
        Ok(None) => {}
        Err(e) => return Err(e),
    }

    Err(generation_error(
        GenerationErrorCategory::FallbackRequired,
        format!(
            "Test '{}' cannot be formally verified. \
             Fuzzer output type: {}. Semantics: {}. \
             The fuzzer domain could not be statically analyzed for direct proof generation.",
            test.name, test.fuzzer_output_type, test.semantics
        ),
    ))
}

/// A state-machine trace test whose theorem body is `proveTestsHalt` — i.e. a
/// `Void + FailImmediately` property with `FuzzerSemantics::StateMachineTrace`.
/// These are precisely the tests for which the universal reachability-based
/// theorem is unsound and must be discharged via concrete `native_decide`
/// instance theorems; see `generate_state_machine_halt_proof_file`.
fn is_state_machine_trace_halt_test(test: &ExportedPropertyTest) -> bool {
    use aiken_lang::ast::OnTestFailure;
    matches!(test.return_mode, TestReturnMode::Void)
        && matches!(test.on_test_failure, OnTestFailure::FailImmediately)
        && matches!(test.semantics, FuzzerSemantics::StateMachineTrace { .. })
}

/// Dual of `is_state_machine_trace_halt_test` for the `proveTestsError` body
/// — i.e. a `Void + SucceedEventually` property (the `fail` annotation form)
/// with `FuzzerSemantics::StateMachineTrace`. As with halt tests, the
/// universal reachability-based theorem
/// `∀ x, reachable x → proveTestsError prog (dataArg x)` is unsound because
/// `reachable` over-approximates the step function, so these tests must be
/// discharged via concrete `native_decide` instance theorems; see
/// `generate_state_machine_error_proof_file`.
fn is_state_machine_trace_error_test(test: &ExportedPropertyTest) -> bool {
    use aiken_lang::ast::OnTestFailure;
    matches!(test.return_mode, TestReturnMode::Void)
        && matches!(test.on_test_failure, OnTestFailure::SucceedEventually)
        && matches!(test.semantics, FuzzerSemantics::StateMachineTrace { .. })
}

/// Emit a proof file for a state-machine trace halt test using one
/// `native_decide` instance theorem per pre-computed concrete witness.
///
/// When no witnesses are available (fuzzer failed to produce a single
/// halting input in `MAX_WITNESS_SEED_ATTEMPTS` attempts, or the compiler
/// elided witness computation for some reason), surface a clean
/// `FallbackRequired` skip rather than emitting the old unsound universal
/// theorem or an empty proof file.
///
/// The caller in `generate_proof_file` wraps the returned content with a
/// `ProofCaveat::Witness(WitnessProofNote { … })` populated from
/// `test.concrete_halt_witnesses`, so reporting surfaces the verdict as
/// `ProofStatus::WitnessProved` rather than `Proved`. Per-witness
/// `native_decide` proofs hold only for the listed concrete inputs;
/// universal quantification over the over-approximating `reachable`
/// predicate is unsound for these tests.
fn halt_witness_result(test: &ExportedPropertyTest, content: String) -> (String, ProofCaveat) {
    let instances = test.concrete_halt_witnesses.len();
    let witnesses = test.concrete_halt_witnesses.clone();
    let plural = if instances == 1 { "" } else { "es" };
    (
        content,
        ProofCaveat::Witness(WitnessProofNote {
            instances,
            witnesses,
            note: format!(
                "state-machine halt test '{}' verified on {} concrete fuzzer-seed-{} witness{} only (universal proof of `∀ x, reachable x → proveTestsHalt prog (dataArg x)` is not claimed because `reachable` is a global over-approximation of the step function)",
                test.name, instances, CONCRETE_WITNESS_BASE_SEED, plural,
            ),
        }),
    )
}

fn generate_state_machine_halt_proof_file(
    test: &ExportedPropertyTest,
    lean_test_name: &str,
    verify_prog: &str,
    direct_header: &dyn Fn(&str) -> String,
    footer: &str,
) -> miette::Result<String> {
    if test.concrete_halt_witnesses.is_empty() {
        return Err(generation_error(
            GenerationErrorCategory::FallbackRequired,
            format!(
                "Test '{}' is a state-machine trace halt test, but the compiler \
                 could not synthesize any concrete fuzzer witness that drives the \
                 test body to a halting state within the attempt budget. \
                 Skipping: no sound theorem can be emitted until a witness is \
                 available or the state-machine step function is encoded directly.",
                test.name
            ),
        ));
    }

    // Decode CBOR hex witnesses into structural `PlutusData` values so we can
    // emit typed Lean literals. A decode failure at this stage is an internal
    // invariant violation — witnesses were produced by the fuzzer and
    // re-encoded by this same crate — so surface it as a hard error rather
    // than silently dropping the witness.
    let mut witnesses: Vec<PlutusData> = Vec::with_capacity(test.concrete_halt_witnesses.len());
    for (i, hex_str) in test.concrete_halt_witnesses.iter().enumerate() {
        let bytes = hex::decode(hex_str).map_err(|e| {
            generation_error(
                GenerationErrorCategory::InvalidConstraint,
                format!(
                    "Test '{}': concrete halt witness #{} is not valid hex: {}",
                    test.name, i, e
                ),
            )
        })?;
        let data = uplc::plutus_data(&bytes).map_err(|e| {
            generation_error(
                GenerationErrorCategory::InvalidConstraint,
                format!(
                    "Test '{}': concrete halt witness #{} failed to decode as PlutusData: {}",
                    test.name, i, e
                ),
            )
        })?;
        witnesses.push(data);
    }

    let mut content = direct_header(
        "open PlutusCore.Data (Data)\nopen PlutusCore.ByteString (ByteString)\nopen PlutusCore.Integer (Integer)\nopen PlutusCore.UPLC.CekMachine (State cekExecuteProgram)",
    );
    // Raise the elaborator recursion cap for this file. The per-witness
    // `sample_i` definitions chain `consByteStringV1` calls once per byte in
    // every `Data.B ...` literal; realistic Cardano-style fuzzer outputs
    // embed 28- and 32-byte script hashes dozens of times, so a single
    // witness can demand a few thousand definitional-unfolding frames during
    // elaboration. The default `maxRecDepth` (512) is blown by
    // pubkey-length ByteString cons chains; 16384 has headroom for
    // multi-witness traces without slowing down small witnesses.
    content.push_str("set_option maxRecDepth 16384\n\n");
    content.push_str(
        "-- Concrete fuzzer-generated halt witnesses.\n\
         -- Each `sample_<i>` is a Plutus `Data` value produced by running the\n\
         -- test's fuzzer with seed 42+i; the test body halts without error on\n\
         -- this input. The corresponding `native_decide` theorem reduces the\n\
         -- closed term `proveTestsHalt prog (dataArg sample_<i>)` without SMT.\n\n",
    );

    // Local Boolean reflection of `isHaltState`. `proveTestsHalt` unfolds to
    // `isHaltState (cekExecuteProgram ...)`, which is a `Prop`-valued match
    // and therefore has no `Decidable` instance Lean can synthesize.
    // Reflecting the match through a `Bool`-valued helper gives a goal of
    // shape `isHaltStateBool ... = true`, which is trivially decidable and
    // can be discharged by `native_decide` (which compiles the whole
    // `cekExecuteProgram` execution to native code, avoiding the kernel's
    // `maxRecDepth` limit on definitional unfolding).
    content.push_str(
        "/-- Bool-valued reflection of `AikenVerify.Utils.isHaltState`; see \
         the theorem-level commentary for why we need it. -/\n\
         private def isHaltStateBool : State \u{2192} Bool\n  \
           | State.Halt _ => true\n  \
           | _            => false\n\n\
         private theorem isHaltStateBool_iff_isHaltState (s : State) :\n  \
             isHaltStateBool s = true \u{2194} isHaltState s := by\n  \
           cases s <;> simp [isHaltStateBool, isHaltState]\n\n",
    );

    for (i, witness) in witnesses.iter().enumerate() {
        let literal = data_to_lean_literal(witness);
        content.push_str(&format!(
            "private def sample_{i} : Data :=\n  {literal}\n\n"
        ));
    }

    // Prove each instance by reflection into the Bool-valued witness:
    //   1. Unfold `proveTestsHalt` so the goal becomes `isHaltState (cek...)`.
    //   2. Rewrite via `isHaltStateBool_iff_isHaltState` so the goal is
    //      `isHaltStateBool (cek...) = true` — a decidable Bool equality.
    //   3. `native_decide` compiles the full CEK execution to native code,
    //      runs it, and confirms the result pattern-matches as `true`.
    //
    // `native_decide` bypasses the kernel's `maxRecDepth` cap that `decide`
    // and `show True; trivial` would otherwise trip on multi-thousand-step
    // CEK executions. The trade-off is trust in Lean's native compiler,
    // which is the standard trust assumption for all `native_decide`-based
    // proofs in this workspace.
    for i in 0..witnesses.len() {
        content.push_str(&format!(
            "theorem {lean_test_name}_instance_{i} :\n  \
             proveTestsHalt {verify_prog} (dataArg sample_{i}) := by\n  \
             unfold proveTestsHalt\n  \
             rw [\u{2190} isHaltStateBool_iff_isHaltState]\n  \
             native_decide\n\n"
        ));
    }

    // Also emit a top-level alias so downstream reporting finds a theorem
    // under the canonical `lean_test_name` symbol. We pick instance 0 as the
    // representative — any instance theorem is equally valid evidence of a
    // halting trace.
    //
    // The alias is written in tactic mode (`by exact ...`) rather than a
    // direct term so Lean only has to unify the alias's stated type with
    // `instance_0`'s inferred type (a closed Prop whose head is
    // `proveTestsHalt`), not re-reduce the inner `cekExecuteProgram` call.
    // A direct-term alias trips Lean's `maxRecDepth` limit during elaboration
    // because the kernel attempts definitional unfolding of
    // `cekExecuteProgram` while checking the body.
    content.push_str(&format!(
        "theorem {lean_test_name} :\n  \
         proveTestsHalt {verify_prog} (dataArg sample_0) := by\n  \
         exact {lean_test_name}_instance_0\n"
    ));

    content.push_str(footer);
    Ok(content)
}

/// Dual of `generate_state_machine_halt_proof_file` for `_ko`-shaped tests
/// (`Void + SucceedEventually` + `StateMachineTrace`, i.e. property tests
/// annotated with `fail`). Emits one `native_decide` instance theorem per
/// pre-computed concrete error witness, proving
/// `proveTestsError prog (dataArg sample_<i>)` rather than `proveTestsHalt`.
///
/// The universal theorem `∀ x, reachable x → proveTestsError prog (dataArg x)`
/// is unsound for the same reason as the halt version: `reachable` is a global
/// over-approximation that admits traces the compiled program cannot actually
/// produce. Concrete witnesses plus reflection into `isErrorStateBool` give a
/// closed computation Lean can decide by native reduction.
///
/// When no error witnesses exist we surface `FallbackRequired`, mirroring the
/// halt path: without a witness we cannot produce a sound theorem.
///
/// As with the halt path, the caller in `generate_proof_file` wraps the
/// returned content with a `ProofCaveat::Witness(WitnessProofNote { … })`
/// populated from `test.concrete_error_witnesses` so the verdict surfaces as
/// `ProofStatus::WitnessProved`, not `Proved`.
fn generate_state_machine_error_proof_file(
    test: &ExportedPropertyTest,
    lean_test_name: &str,
    verify_prog: &str,
    direct_header: &dyn Fn(&str) -> String,
    footer: &str,
) -> miette::Result<String> {
    if test.concrete_error_witnesses.is_empty() {
        return Err(generation_error(
            GenerationErrorCategory::FallbackRequired,
            format!(
                "Test '{}' is a state-machine trace error test (Void + SucceedEventually), \
                 but the compiler could not synthesize any concrete fuzzer witness that \
                 drives the test body to an erroring state within the attempt budget. \
                 Skipping: no sound theorem can be emitted until a witness is available \
                 or the state-machine step function is encoded directly.",
                test.name
            ),
        ));
    }

    // Decode CBOR hex witnesses into structural `PlutusData` values. Same
    // invariant as the halt path: witnesses came from this same crate, so a
    // decode failure is an internal bug rather than user error.
    let mut witnesses: Vec<PlutusData> = Vec::with_capacity(test.concrete_error_witnesses.len());
    for (i, hex_str) in test.concrete_error_witnesses.iter().enumerate() {
        let bytes = hex::decode(hex_str).map_err(|e| {
            generation_error(
                GenerationErrorCategory::InvalidConstraint,
                format!(
                    "Test '{}': concrete error witness #{} is not valid hex: {}",
                    test.name, i, e
                ),
            )
        })?;
        let data = uplc::plutus_data(&bytes).map_err(|e| {
            generation_error(
                GenerationErrorCategory::InvalidConstraint,
                format!(
                    "Test '{}': concrete error witness #{} failed to decode as PlutusData: {}",
                    test.name, i, e
                ),
            )
        })?;
        witnesses.push(data);
    }

    let mut content = direct_header(
        "open PlutusCore.Data (Data)\nopen PlutusCore.ByteString (ByteString)\nopen PlutusCore.Integer (Integer)\nopen PlutusCore.UPLC.CekMachine (State cekExecuteProgram)",
    );
    // Same elaborator-depth rationale as the halt path: Cardano-style fuzzer
    // outputs embed many multi-byte ByteString literals whose cons-chains
    // blow past the default `maxRecDepth` of 512.
    content.push_str("set_option maxRecDepth 16384\n\n");
    content.push_str(
        "-- Concrete fuzzer-generated error witnesses.\n\
         -- Each `sample_<i>` is a Plutus `Data` value produced by running the\n\
         -- test's fuzzer with seed 42+i; the test body errors on this input.\n\
         -- The corresponding `native_decide` theorem reduces the closed term\n\
         -- `proveTestsError prog (dataArg sample_<i>)` without SMT.\n\n",
    );

    // Bool reflection of `isErrorState`, mirroring `isHaltStateBool` in the
    // halt path. `proveTestsError` unfolds to `isErrorState (cek...)`, a
    // `Prop`-valued match with no synthesizable `Decidable` instance; the
    // Bool version `isErrorStateBool ... = true` is trivially decidable and
    // can be discharged by `native_decide`.
    content.push_str(
        "/-- Bool-valued reflection of `AikenVerify.Utils.isErrorState`; see \
         the theorem-level commentary for why we need it. -/\n\
         private def isErrorStateBool : State \u{2192} Bool\n  \
           | State.Error => true\n  \
           | _           => false\n\n\
         private theorem isErrorStateBool_iff_isErrorState (s : State) :\n  \
             isErrorStateBool s = true \u{2194} isErrorState s := by\n  \
           cases s <;> simp [isErrorStateBool, isErrorState]\n\n",
    );

    for (i, witness) in witnesses.iter().enumerate() {
        let literal = data_to_lean_literal(witness);
        content.push_str(&format!(
            "private def sample_{i} : Data :=\n  {literal}\n\n"
        ));
    }

    // Proof tactic: unfold `proveTestsError`, rewrite into the Bool reflection,
    // then `native_decide` compiles and runs the CEK execution. Same trust
    // assumption as the halt path.
    for i in 0..witnesses.len() {
        content.push_str(&format!(
            "theorem {lean_test_name}_instance_{i} :\n  \
             proveTestsError {verify_prog} (dataArg sample_{i}) := by\n  \
             unfold proveTestsError\n  \
             rw [\u{2190} isErrorStateBool_iff_isErrorState]\n  \
             native_decide\n\n"
        ));
    }

    // Canonical top-level alias; see halt version for the tactic-mode vs
    // direct-term rationale (avoids tripping Lean's `maxRecDepth` during
    // alias elaboration).
    content.push_str(&format!(
        "theorem {lean_test_name} :\n  \
         proveTestsError {verify_prog} (dataArg sample_0) := by\n  \
         exact {lean_test_name}_instance_0\n"
    ));

    content.push_str(footer);
    Ok(content)
}

fn generate_replay_backed_fail_once_proof_file(
    test: &ExportedPropertyTest,
    form: &TheoremForm,
    lean_test_name: &str,
    verify_prog: &str,
    direct_header: &dyn Fn(&str) -> String,
    footer: &str,
) -> miette::Result<String> {
    let witness = test.fail_once_replay_witness.as_ref().ok_or_else(|| {
        generation_error(
            GenerationErrorCategory::FallbackRequired,
            format!(
                "Test '{}' needs a replay-backed fail_once witness, but none was exported.",
                test.name
            ),
        )
    })?;
    let bytes = hex::decode(&witness.data_hex).map_err(|e| {
        generation_error(
            GenerationErrorCategory::InvalidConstraint,
            format!(
                "Test '{}': fail_once replay witness is not valid hex: {}",
                test.name, e
            ),
        )
    })?;
    let witness_data = uplc::plutus_data(&bytes).map_err(|e| {
        generation_error(
            GenerationErrorCategory::InvalidConstraint,
            format!(
                "Test '{}': fail_once replay witness failed to decode as PlutusData: {}",
                test.name, e
            ),
        )
    })?;
    let literal = data_to_lean_literal(&witness_data);
    let replay_choices = witness.replay_choices_hex.as_deref().unwrap_or("none");

    let mut content = direct_header(
        "open PlutusCore.Data (Data)\nopen PlutusCore.ByteString (ByteString)\nopen PlutusCore.Integer (Integer)\nopen PlutusCore.UPLC.CekMachine (State cekExecuteProgram)",
    );
    content.push_str(&format_mode_context(form, verify_prog));
    content.push_str(&format!(
        "-- Replay-backed semantic existential witness.\n\
         -- generation_seed={}; replay_choices={}.\n\
         -- This proves only that the exported fuzzer/property path has a failing input\n\
         -- for this recorded replay. It does NOT claim the randomized runner will\n\
         -- find a failure within any bounded number of attempts.\n\n\
         private def sample_0 : Data :=\n  {literal}\n\n\
         private abbrev replayedWitnessDomain (x : Data) : Prop :=\n  x = sample_0\n\n\
         theorem {lean_test_name} :\n  {EXISTS} x : Data,\n    replayedWitnessDomain x\n    {AND}\n    PropertyFails testReturnMode testDecodePolicy (testOutcome (dataArg x)) := by\n  refine {ANGLE_OPEN}sample_0, ?_{ANGLE_CLOSE}\n  constructor\n  · rfl\n  · rw [← PropertyFailsBool_iff_PropertyFails testReturnMode testDecodePolicy (testOutcome (dataArg sample_0))]\n    native_decide\n",
        witness.generation_seed,
        replay_choices,
        EXISTS = lean_sym::EXISTS,
        AND = lean_sym::AND,
        ANGLE_OPEN = lean_sym::ANGLE_OPEN,
        ANGLE_CLOSE = lean_sym::ANGLE_CLOSE,
    ));
    content.push_str(footer);
    Ok(content)
}

fn collect_tuple_element_precondition_parts_from_semantics(
    test_name: &str,
    elem_type: &FuzzerOutputType,
    semantics: &FuzzerSemantics,
    var: &str,
    out: &mut Vec<String>,
    witness: &mut Option<String>,
) -> miette::Result<()> {
    if let FuzzerOutputType::List(list_elem_type) = elem_type {
        let (parts, witness_min_len, witness_elem) =
            build_list_domain_preconditions_from_semantics(
                test_name,
                list_elem_type.as_ref(),
                semantics,
                var,
            )?;
        out.extend(parts);
        *witness = Some(build_list_witness_value(
            list_elem_type.as_ref(),
            witness_min_len,
            witness_elem.as_deref(),
            None,
        ));
        return Ok(());
    }

    let (parts, scalar_witness) =
        build_scalar_domain_preconditions_from_semantics(test_name, elem_type, semantics, var)?;
    out.extend(parts);
    *witness = scalar_witness;
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn generate_tuple_proof_from_semantics(
    test: &ExportedPropertyTest,
    types: &[&FuzzerOutputType],
    semantics: &[FuzzerSemantics],
    form: &TheoremForm,
    lean_test_name: &str,
    prog: &str,
    header: &dyn Fn(&str) -> String,
    footer: &str,
    target: &VerificationTargetKind,
    prop_prog: &str,
    handler_prog: &str,
) -> miette::Result<String> {
    let arity = types.len();
    let var_names = tuple_var_names(arity);

    let quantifier_parts: Vec<String> = types
        .iter()
        .enumerate()
        .map(|(i, t)| {
            let var = &var_names[i];
            let lean_type = lean_type_for(t).ok_or_else(|| {
                generation_error(
                    GenerationErrorCategory::UnsupportedShape,
                    format!(
                        "Test '{}': tuple element {} has type {} which has no Lean type mapping.",
                        test.name, i, t
                    ),
                )
            })?;
            Ok(format!("({var} : {lean_type})"))
        })
        .collect::<miette::Result<Vec<String>>>()?;
    let quantifier_vars = quantifier_parts.join(" ");

    let mut precond_parts = Vec::new();
    let mut witness_parts: Vec<Option<String>> = vec![None; arity];
    for (i, (t, semantics)) in types.iter().zip(semantics.iter()).enumerate() {
        collect_tuple_element_precondition_parts_from_semantics(
            &test.name,
            t,
            semantics,
            &var_names[i],
            &mut precond_parts,
            &mut witness_parts[i],
        )?;
    }

    let encoded_parts: Vec<String> = types
        .iter()
        .enumerate()
        .map(|(i, t)| {
            lean_data_encoder(t, &var_names[i]).ok_or_else(|| {
                generation_error(
                    GenerationErrorCategory::UnsupportedShape,
                    format!(
                        "Test '{}': tuple element {} has type {} which has no Lean data encoder.",
                        test.name, i, t
                    ),
                )
            })
        })
        .collect::<miette::Result<Vec<String>>>()?;
    let arg_expr = format!(
        "[Term.Const (Const.Data (Data.List [{}]))]",
        encoded_parts.join(", ")
    );

    let opens = lean_opens_for_types(types);
    let witness = if form.existential_mode == Some(ExistentialMode::Witness) {
        let witness_values: Vec<String> = types
            .iter()
            .enumerate()
            .map(|(i, t)| {
                if let Some(ref w) = witness_parts[i] {
                    w.to_string()
                } else {
                    lean_default_witness(t)
                }
            })
            .collect();
        Some(witness_values.join(", "))
    } else {
        None
    };
    let theorems = format_theorems(
        form,
        lean_test_name,
        prog,
        &arg_expr,
        &quantifier_vars,
        &precond_parts,
        witness.as_deref(),
    );
    let mut content = format!(
        "{}{}{}",
        header(&opens),
        format_mode_context(form, prog),
        theorems
    );
    if let VerificationTargetKind::Equivalence = target {
        content.push_str(&format_equivalence_theorem(
            test,
            lean_test_name,
            prop_prog,
            handler_prog,
            &arg_expr,
            &quantifier_vars,
            &precond_parts,
        ));
    }
    content.push_str(footer);
    Ok(content)
}

fn try_generate_sampler_relation_proof(
    test: &ExportedPropertyTest,
    test_id: &str,
    form: &TheoremForm,
    lean_test_name: &str,
    verify_prog: &str,
    direct_header: &dyn Fn(&str) -> String,
    footer: &str,
    target: &VerificationTargetKind,
) -> miette::Result<Option<String>> {
    if !sampler_relation_supported(
        test,
        target,
        form.existential_mode.unwrap_or(ExistentialMode::Proof),
    ) {
        return Ok(None);
    }

    let theorems = format_theorems(
        form,
        lean_test_name,
        verify_prog,
        "dataArg x",
        "(x : Data)",
        &["samplerDomain x".to_string()],
        None,
    );
    let mut content = format!(
        "{}{}{}{}",
        direct_header(""),
        format_mode_context(form, verify_prog),
        format_sampler_context(test_id),
        theorems
    );
    content.push_str(footer);
    Ok(Some(content))
}

#[allow(clippy::too_many_arguments)]
fn try_generate_direct_proof_from_semantics(
    test: &ExportedPropertyTest,
    test_id: &str,
    form: &TheoremForm,
    lean_test_name: &str,
    verify_prog: &str,
    direct_header: &dyn Fn(&str) -> String,
    footer: &str,
    target: &VerificationTargetKind,
    prop_prog: &str,
    handler_prog: &str,
) -> miette::Result<Option<String>> {
    if let Some(content) = try_generate_state_machine_trace_proof_from_semantics(
        test,
        form,
        lean_test_name,
        verify_prog,
        direct_header,
        footer,
        target,
        prop_prog,
        handler_prog,
    )? {
        return Ok(Some(content));
    }

    if let Some(content) = try_generate_sampler_relation_proof(
        test,
        test_id,
        form,
        lean_test_name,
        verify_prog,
        direct_header,
        footer,
        target,
    )? {
        return Ok(Some(content));
    }

    // Top-level opaque fallback: if the test's TOP-LEVEL fuzzer semantics is
    // `Opaque` but the sampler fallback is unavailable for the selected
    // target/mode, we cannot build a sound theorem for the input domain.
    if let FuzzerSemantics::Opaque { reason } = &test.semantics {
        return Err(generation_error(
            GenerationErrorCategory::FallbackRequired,
            format!(
                "Test '{}' has opaque top-level fuzzer semantics ({}). \
                 Cannot build a structural domain predicate or sampler fallback for the selected verification mode.",
                test.name, reason
            ),
        ));
    }

    let build_scalar_theorem = |arg_expr: &str,
                                quantifier_vars: &str,
                                precondition_parts: &[String],
                                opens: &str,
                                witness: Option<&str>|
     -> String {
        let theorem_witness = if form.existential_mode == Some(ExistentialMode::Witness) {
            witness
        } else {
            None
        };
        let theorems = format_theorems(
            form,
            lean_test_name,
            verify_prog,
            arg_expr,
            quantifier_vars,
            precondition_parts,
            theorem_witness,
        );
        let mut content = format!(
            "{}{}{}",
            direct_header(opens),
            format_mode_context(form, verify_prog),
            theorems
        );
        if let VerificationTargetKind::Equivalence = target {
            content.push_str(&format_equivalence_theorem(
                test,
                lean_test_name,
                prop_prog,
                handler_prog,
                arg_expr,
                quantifier_vars,
                precondition_parts,
            ));
        }
        content.push_str(footer);
        content
    };

    match (&test.fuzzer_output_type, &test.semantics) {
        // M6 — surgical early gate on `FuzzerOutputType::Unsupported(repr)`.
        //
        // The legacy dispatcher treated `Unsupported(_)` and `Data`
        // identically (`(Data, _) | (Unsupported(_), _)` arm below): it
        // attempted to build a theorem from the semantics, then
        // optionally added a schema-derived structural predicate.
        // When BOTH the semantics carried no constraint
        // (`FuzzerSemantics::Data`) AND no `fuzzer_data_schema` was
        // available, the result was a theorem with no precondition —
        // a vacuous "PROVED" verdict for any test.
        //
        // Fast-fail with E0028 in exactly that case: the type cannot
        // be mapped to a Lean representation, semantics is generic
        // `Data`, and no schema rescues us.
        // `--skip-unsupported=E0028` records the test as skipped rather
        // than reporting a misleading PROVED.
        //
        // **Why not gate ALL `Unsupported(_)` calls here?**  The
        // existing tests at e.g.
        // `non_recursive_schema_does_not_emit_mutual_block` (and seven
        // siblings) deliberately combine `Unsupported(_)` output type
        // with a populated `fuzzer_data_schema` — those flows produce
        // a sound structural theorem from the schema and must still
        // succeed.  Likewise tests with
        // `FuzzerSemantics::Constructors { .. }` or
        // `DataWithSchema { .. }` carry their own structural
        // constraint and don't need the fuzzer schema.  Restricting
        // the gate to the `(Unsupported, Data, no-schema)` triple
        // catches exactly the silent-True case the plan flagged
        // without breaking the other shapes.
        //
        // **Caveat for state-machine traces.**  State-machine tests
        // routinely carry `Unsupported(...)` `state_type` / `event_type`,
        // but the top-level `fuzzer_output_type` for state-machine
        // tests is `List<Unsupported(...)>`, not bare `Unsupported`.
        // Those tests reach this dispatcher only after
        // `try_generate_state_machine_trace_proof_from_semantics`
        // returned `None`; the gate here does not interact with the
        // state-machine path.
        (FuzzerOutputType::Unsupported(repr), FuzzerSemantics::Data)
            if test.fuzzer_data_schema.is_none() =>
        {
            Err(miette::Report::new(error_catalogue::unsupported(
                "E0028",
                UnsupportedReason::UnsupportedFuzzerOutputType {
                    test_name: test.name.clone(),
                    type_repr: repr.clone(),
                },
            )))
        }
        (FuzzerOutputType::Int, semantics) => {
            let (precondition_parts, witness) = build_scalar_domain_preconditions_from_semantics(
                &test.name,
                &test.fuzzer_output_type,
                semantics,
                "x",
            )?;
            Ok(Some(build_scalar_theorem(
                "intArg x",
                "(x : Integer)",
                &precondition_parts,
                "open PlutusCore.Integer (Integer)",
                witness.as_deref(),
            )))
        }
        (FuzzerOutputType::Bool, semantics) => {
            let (precondition_parts, witness) = build_scalar_domain_preconditions_from_semantics(
                &test.name,
                &test.fuzzer_output_type,
                semantics,
                "x",
            )?;
            Ok(Some(build_scalar_theorem(
                "boolArg x",
                "(x : Bool)",
                &precondition_parts,
                "",
                witness.as_deref(),
            )))
        }
        (FuzzerOutputType::ByteArray, semantics) => {
            let (precondition_parts, witness) = build_scalar_domain_preconditions_from_semantics(
                &test.name,
                &test.fuzzer_output_type,
                semantics,
                "x",
            )?;
            Ok(Some(build_scalar_theorem(
                "bytearrayArg x",
                "(x : ByteString)",
                &precondition_parts,
                "open PlutusCore.ByteString (ByteString)",
                witness.as_deref(),
            )))
        }
        (FuzzerOutputType::String, semantics) => {
            if let Some(arg_exprs) = finite_string_arg_exprs(&test.name, semantics)? {
                if form.existential {
                    return Err(finite_domain_existential_error(&test.name));
                }
                if !matches!(target, VerificationTargetKind::PropertyWrapper) {
                    return Err(finite_domain_target_mode_error(&test.name, target));
                }
                let theorems = format_ground_theorem_family(
                    form,
                    lean_test_name,
                    verify_prog,
                    &arg_exprs,
                    Some("native_decide"),
                );
                let mut content = direct_header("open PlutusCore.ByteString (ByteString)");
                content.push_str(&theorems);
                content.push_str(footer);
                return Ok(Some(content));
            }

            let (precondition_parts, witness) = build_scalar_domain_preconditions_from_semantics(
                &test.name,
                &test.fuzzer_output_type,
                semantics,
                "x",
            )?;
            Ok(Some(build_scalar_theorem(
                "stringArg x",
                "(x : ByteString)",
                &precondition_parts,
                "open PlutusCore.ByteString (ByteString)",
                witness.as_deref(),
            )))
        }
        // M6 — the early gate above hard-errors the
        // `(Unsupported(_), Data, no-schema)` triple as E0028.  Other
        // `Unsupported(_)` shapes (with a fuzzer_data_schema, or with
        // semantics that already carry a structural constraint such as
        // `Constructors { .. }` or `DataWithSchema { .. }`) still flow
        // through here and produce a sound theorem.
        (FuzzerOutputType::Data, semantics) | (FuzzerOutputType::Unsupported(_), semantics) => {
            let (mut precondition_parts, witness) =
                build_scalar_domain_preconditions_from_semantics(
                    &test.name,
                    &test.fuzzer_output_type,
                    semantics,
                    "x",
                )?;

            // When a fuzzer_data_schema is available and the semantics don't already
            // provide structural constraints (i.e. semantics is Data, not Constructors),
            // use the schema to build a structural Data -> Prop predicate.
            // This gives per-constructor-tag (and optionally per-field) constraints for
            // custom ADTs that the semantics pipeline only classifies as generic Data.
            //
            // TODO(2.2): Full field-level schema predicates.
            // Currently we only emit the schema predicate as a precondition when it can
            // be fully lowered from the exported schema. A full implementation would
            // recurse into field types and generate per-field sub-predicates, matching
            // the approach used in the state-machine path's
            // emit_exported_schema_data_predicate.
            //
            // M6 — Stop silently falling through to the semantics-only
            // path when schema lowering fails.  The previous `Err(_) =>
            // None` arm produced a theorem whose precondition was
            // strictly weaker than the actual fuzzer domain: a proof
            // discharged against the fallback predicate carried no
            // guarantee for the inputs the schema would have
            // restricted.  Surface as `E0017`
            // (`OpaqueListElementSemantics`) and route through the
            // standard skip path; `--skip-unsupported=E0017` records
            // the test as skipped rather than reporting a misleading
            // PROVED.  The lowerer's underlying error message is
            // stashed in the typed `reason` payload so downstream
            // tooling (`aiken verify capabilities --json`) can surface
            // the root cause.
            let schema_helper_defs = if matches!(semantics, FuzzerSemantics::Data)
                || matches!(semantics, FuzzerSemantics::DataWithSchema { .. })
                || matches!(semantics, FuzzerSemantics::Constructors { .. })
            {
                if let Some(schema) = test.fuzzer_data_schema.as_ref() {
                    let helper_prefix = format!("{lean_test_name}_schema");
                    let (root_predicate, defs) =
                        build_exported_data_shape_predicates(&test.name, schema, &helper_prefix)
                            .map_err(|e| {
                                miette::Report::new(error_catalogue::unsupported(
                                    "E0017",
                                    UnsupportedReason::OpaqueListElementSemantics {
                                        test_name: test.name.clone(),
                                        reason: format!("{e}"),
                                    },
                                ))
                            })?;
                    precondition_parts.push(format!("{root_predicate} x"));
                    Some(defs)
                } else {
                    None
                }
            } else {
                None
            };

            // When we have schema helper definitions, we need to insert them between
            // the header and the theorems (same pattern as the state-machine path).
            if let Some(defs) = schema_helper_defs {
                let theorem_witness = if form.existential_mode == Some(ExistentialMode::Witness) {
                    // Try to build a witness from the schema for a better default
                    let schema_witness = test.fuzzer_data_schema.as_ref().and_then(|schema| {
                        build_exported_data_shape_witness_from_reference(
                            &test.name,
                            schema,
                            &schema.root,
                        )
                        .ok()
                    });
                    witness.as_deref().map(|w| w.to_string()).or(schema_witness)
                } else {
                    None
                };
                let theorems = format_theorems(
                    form,
                    lean_test_name,
                    verify_prog,
                    "dataArg x",
                    "(x : Data)",
                    &precondition_parts,
                    theorem_witness.as_deref(),
                );
                let opens = "open PlutusCore.Data (Data)\nopen PlutusCore.ByteString (ByteString)";
                let mut content = direct_header(opens);
                content.push_str("-- compiler-exported structural domain predicate\n");
                content.push_str(&defs);
                content.push('\n');
                content.push_str(&format_mode_context(form, verify_prog));
                content.push_str(&theorems);
                if let VerificationTargetKind::Equivalence = target {
                    content.push_str(&format_equivalence_theorem(
                        test,
                        lean_test_name,
                        prop_prog,
                        handler_prog,
                        "dataArg x",
                        "(x : Data)",
                        &precondition_parts,
                    ));
                }
                content.push_str(footer);
                Ok(Some(content))
            } else {
                Ok(Some(build_scalar_theorem(
                    "dataArg x",
                    "(x : Data)",
                    &precondition_parts,
                    "open PlutusCore.Data (Data)",
                    witness.as_deref(),
                )))
            }
        }
        (
            FuzzerOutputType::List(elem_type),
            FuzzerSemantics::List {
                element: elem_semantics,
                min_len,
                max_len,
            },
        ) => {
            if matches!(elem_type.as_ref(), FuzzerOutputType::Bool)
                && matches!(elem_semantics.as_ref(), FuzzerSemantics::Bool)
                && let (Some(lo), Some(hi)) = (*min_len, *max_len)
            {
                if form.existential {
                    return Err(finite_domain_existential_error(&test.name));
                }
                if !matches!(target, VerificationTargetKind::PropertyWrapper) {
                    return Err(finite_domain_target_mode_error(&test.name, target));
                }
                validate_list_len_bounds(&test.name, Some(lo), Some(hi))?;
                let case_count = checked_bool_list_cardinality(lo, hi).map_err(|cases| {
                    finite_domain_too_large_error(
                        &test.name,
                        cases,
                        MAX_FINITE_THEOREM_INSTANCES_PER_TEST,
                        "MAX_FINITE_THEOREM_INSTANCES_PER_TEST",
                    )
                })?;
                let arg_exprs = bool_list_arg_exprs(lo, hi);
                debug_assert_eq!(arg_exprs.len(), case_count);
                let theorems = format_ground_theorem_family(
                    form,
                    lean_test_name,
                    verify_prog,
                    &arg_exprs,
                    Some("native_decide"),
                );
                let mut content = direct_header("open PlutusCore.Data (Data)");
                content.push_str(&theorems);
                content.push_str(footer);
                return Ok(Some(content));
            }

            if lean_type_for(elem_type).is_none() || lean_data_encoder(elem_type, "x_i").is_none() {
                return Err(generation_error(
                    GenerationErrorCategory::UnsupportedShape,
                    format!(
                        "Test '{}': list element type {} has no Lean type mapping or Data encoder",
                        test.name, elem_type
                    ),
                ));
            }

            let elem_lean_type = lean_type_for(elem_type).expect("checked above");
            let opens = lean_opens_for_types(&[elem_type.as_ref()]);

            // Use length-indexed proof generation when both length bounds are known
            // and the element type is scalar (not Pair/Tuple).
            //
            // Universally-quantified `∀ xs : List T` theorems consistently time out
            // because recursive CEK machine evaluation on an inductive List ADT
            // creates SMT problems that exceed Z3's practical budget.  A fixed-length
            // theorem with explicit scalar variables `x0 x1 … x_{n-1}` proves in
            // ~1 second, so we emit one theorem per concrete length in [lo..=hi].
            let is_scalar_elem = !matches!(
                elem_type.as_ref(),
                FuzzerOutputType::Pair(_, _) | FuzzerOutputType::Tuple(_)
            );
            if let (Some(lo), Some(hi), true) = (*min_len, *max_len, is_scalar_elem) {
                validate_list_len_bounds(&test.name, Some(lo), Some(hi))?;
                let mut all_theorems = format_mode_context(form, verify_prog);
                let mut correctness_props = Vec::new();
                let mut correctness_names = Vec::new();
                let mut termination_props = Vec::new();
                let mut termination_names = Vec::new();

                for len in lo..=hi {
                    let vars: Vec<String> = (0..len).map(|i| format!("x{i}")).collect();

                    // Concrete Data.List argument for this length.
                    let data_items: Vec<String> = vars
                        .iter()
                        .map(|v| lean_data_encoder(elem_type, v).expect("checked above"))
                        .collect();
                    let list_items = data_items.join(", ");
                    let arg_expr = format!("[Term.Const (Const.Data (Data.List [{list_items}]))]");
                    let theorem_name = format!("{lean_test_name}_l{len}");

                    if vars.is_empty() {
                        // len == 0: ground theorem, no universal quantifier.
                        let correctness_body = form.correctness.format(&arg_expr);
                        let correctness_tactic = form.correctness_tactic();
                        all_theorems.push_str(&format!(
                            "theorem {theorem_name} :\n  {correctness_body} :=\n  by {correctness_tactic}\n"
                        ));
                        correctness_props.push(correctness_body);
                        correctness_names.push(theorem_name.clone());
                        if let Some(ref term_body) = form.termination {
                            let term_str = term_body.format(&arg_expr);
                            let term_tactic = term_body.tactic();
                            all_theorems.push_str(&format!(
                                "\ntheorem {theorem_name}_alwaysTerminating :\n  {term_str} :=\n  by {term_tactic}\n"
                            ));
                            termination_props.push(term_str);
                            termination_names.push(format!("{theorem_name}_alwaysTerminating"));
                        }
                        all_theorems.push('\n');
                        continue;
                    }

                    // Per-element preconditions: one entry per variable.
                    let mut precondition_parts: Vec<String> = Vec::new();
                    let mut first_witness: Option<String> = None;
                    for var in &vars {
                        collect_scalar_precondition_parts_from_semantics(
                            &test.name,
                            elem_type,
                            elem_semantics,
                            var,
                            &mut precondition_parts,
                            &mut first_witness,
                        )?;
                    }

                    let quantifier_vars = format!("({} : {elem_lean_type})", vars.join(" "));
                    let theorem_witness = if form.existential_mode == Some(ExistentialMode::Witness)
                    {
                        first_witness.as_deref()
                    } else {
                        None
                    };
                    correctness_props.push(theorem_proposition(
                        &form.correctness,
                        form.existential,
                        &arg_expr,
                        &quantifier_vars,
                        &precondition_parts,
                    ));
                    correctness_names.push(theorem_name.clone());
                    if let Some(ref term_body) = form.termination {
                        termination_props.push(theorem_proposition(
                            term_body,
                            false,
                            &arg_expr,
                            &quantifier_vars,
                            &precondition_parts,
                        ));
                        termination_names.push(format!("{theorem_name}_alwaysTerminating"));
                    }

                    let theorems = format_theorems(
                        form,
                        &theorem_name,
                        verify_prog,
                        &arg_expr,
                        &quantifier_vars,
                        &precondition_parts,
                        theorem_witness,
                    );
                    all_theorems.push_str(&theorems);
                    all_theorems.push('\n');
                }

                all_theorems.push_str(&format_aggregate_theorem(
                    lean_test_name,
                    &correctness_props,
                    &correctness_names,
                ));
                if form.termination.is_some() {
                    all_theorems.push_str(&format_aggregate_theorem(
                        &format!("{lean_test_name}_alwaysTerminating"),
                        &termination_props,
                        &termination_names,
                    ));
                }

                let mut content = direct_header(&opens);
                content.push_str(&all_theorems);
                content.push_str(footer);
                return Ok(Some(content));
            }

            // Fallback: universally-quantified `∀ xs : List T` form.
            // Used when length bounds are not both known, or for Pair/Tuple elements.
            // NOTE: these theorems often time out; `--skip-unsupported` is recommended.
            let elem_encoder = lean_data_encoder(elem_type, "x_i").expect("checked above");
            let (precondition_parts, witness_min_len, witness_elem) =
                build_list_domain_preconditions_from_semantics(
                    &test.name,
                    elem_type.as_ref(),
                    &test.semantics,
                    "xs",
                )?;

            let quantifier_vars = format!("(xs : List {elem_lean_type})");
            let arg_expr = format!(
                "[Term.Const (Const.Data (Data.List (xs.map (fun x_i => {elem_encoder}))))]"
            );
            let witness = if form.existential_mode == Some(ExistentialMode::Witness) {
                Some(build_list_witness_value(
                    elem_type.as_ref(),
                    witness_min_len,
                    witness_elem.as_deref(),
                    None,
                ))
            } else {
                None
            };

            Ok(Some(build_scalar_theorem(
                &arg_expr,
                &quantifier_vars,
                &precondition_parts,
                &opens,
                witness.as_deref(),
            )))
        }
        (FuzzerOutputType::Pair(fst, snd), FuzzerSemantics::Product(elems)) if elems.len() == 2 => {
            if [fst.as_ref(), snd.as_ref()]
                .iter()
                .any(|t| lean_type_for(t).is_none())
            {
                return Err(generation_error(
                    GenerationErrorCategory::UnsupportedShape,
                    format!(
                        "Test '{}': pair element type has no Lean type mapping (fst: {}, snd: {})",
                        test.name, fst, snd
                    ),
                ));
            }
            Ok(Some(generate_tuple_proof_from_semantics(
                test,
                &[fst.as_ref(), snd.as_ref()],
                elems,
                form,
                lean_test_name,
                verify_prog,
                direct_header,
                footer,
                target,
                prop_prog,
                handler_prog,
            )?))
        }
        (FuzzerOutputType::Tuple(types), FuzzerSemantics::Product(elems))
            if types.len() >= 2 && types.len() == elems.len() =>
        {
            let type_refs: Vec<&FuzzerOutputType> = types.iter().collect();
            if types.iter().any(|t| lean_type_for(t).is_none()) {
                return Err(generation_error(
                    GenerationErrorCategory::UnsupportedShape,
                    format!(
                        "Test '{}': tuple element type has no Lean type mapping ({})",
                        test.name,
                        display_output_types(types)
                    ),
                ));
            }
            Ok(Some(generate_tuple_proof_from_semantics(
                test,
                &type_refs,
                elems,
                form,
                lean_test_name,
                verify_prog,
                direct_header,
                footer,
                target,
                prop_prog,
                handler_prog,
            )?))
        }
        (output_type, semantics) => Err(generation_error(
            GenerationErrorCategory::FallbackRequired,
            format!(
                "Test '{}' cannot be formally verified. \
                 Fuzzer output type: {}. Semantics: {}. \
                 The fuzzer domain could not be statically analyzed for direct proof generation.",
                test.name, output_type, semantics
            ),
        )),
    }
}

/// Validate that a test can be translated to a Lean theorem shape for the
/// selected mode/target without writing workspace files.
///
/// Preflight always uses production semantics
/// (`allow_vacuous_subgenerators = false`): the goal is to catch
/// hard-error tests (E0018, S0002, etc.) before any workspace files land
/// on disk, so even a debug-mode run benefits from preflight catching
/// vacuous-subgenerator tests up front.
pub fn preflight_validate_test(
    test: &ExportedPropertyTest,
    existential_mode: ExistentialMode,
    target: &VerificationTargetKind,
) -> miette::Result<()> {
    generate_proof_file(
        test,
        "__preflight__",
        "__preflight__",
        "AikenVerify.Preflight",
        existential_mode,
        target,
        false,
    )
    .map(|(_content, _caveat)| ())
}

pub mod error_catalogue;
mod result_parser;
pub use result_parser::{VERIFY_SUMMARY_VERSION, parse_verify_results};
use result_parser::{
    classify_failure, extract_error_for_module, extract_error_for_theorem,
    extract_global_failure_reason, theorem_has_explicit_failure,
};

#[cfg(test)]
mod tests;
