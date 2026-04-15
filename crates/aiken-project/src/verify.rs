use crate::blueprint::{
    definitions::Reference,
    schema::{Data as SchemaData, Declaration as SchemaDeclaration, Items as SchemaItems, Schema},
};
use crate::export::{
    ExportedDataSchema, ExportedPropertyTest, FuzzerConstraint, FuzzerExactValue, FuzzerOutputType,
    FuzzerSemantics, StateMachineAcceptance, StateMachineTransitionSemantics,
    VerificationTargetKind,
};
use num_bigint::BigInt;
use std::collections::HashMap;
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::io::Read;
use std::path::{Component, Path, PathBuf};
use std::process::{Command, Stdio};
use std::thread;
use std::time::{Duration, Instant};

#[cfg(unix)]
unsafe extern "C" {
    fn kill(pid: i32, sig: i32) -> i32;
}

#[cfg(unix)]
const SIGTERM: i32 = 15;
#[cfg(unix)]
const SIGKILL: i32 = 9;

#[cfg(unix)]
fn terminate_child_process_tree(child: &mut std::process::Child) {
    // Kill the entire process group to ensure descendant processes
    // release stdout/stderr pipes and reader threads can finish.
    if let Ok(raw_pid) = i32::try_from(child.id()) {
        // Negative PID targets the process group ID.
        unsafe {
            let _ = kill(-raw_pid, SIGTERM);
        }
        thread::sleep(Duration::from_millis(200));
        unsafe {
            let _ = kill(-raw_pid, SIGKILL);
        }
    } else {
        let _ = child.kill();
    }
}

#[cfg(windows)]
fn terminate_child_process_tree(child: &mut std::process::Child) {
    // `taskkill /T` terminates the process and descendants in the same tree.
    let pid = child.id().to_string();
    let _ = Command::new("taskkill")
        .args(["/PID", pid.as_str(), "/T", "/F"])
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status();
    let _ = child.kill();
}

#[cfg(all(not(unix), not(windows)))]
fn terminate_child_process_tree(child: &mut std::process::Child) {
    let _ = child.kill();
}

/// Result of running proof verification
#[derive(Debug, serde::Serialize)]
pub struct VerifyResult {
    pub success: bool,
    pub stdout: String,
    pub stderr: String,
    pub exit_code: Option<i32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub theorem_results: Option<Vec<TheoremResult>>,
}

/// Verification capabilities report: documents what the integration does and
/// does not support. Surfaced via `aiken verify doctor --json` so that no
/// limitation remains implicit.
#[derive(Debug, Clone, serde::Serialize)]
pub struct VerificationCapabilities {
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
}

/// A note about an intentionally unsupported capability.
#[derive(Debug, Clone, serde::Serialize)]
pub struct CapabilityNote {
    pub kind: String,
    pub status: String,
    pub reason: String,
}

/// Return the current verification capabilities.
pub fn capabilities() -> VerificationCapabilities {
    VerificationCapabilities {
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
            "Finite nullary ADT constructor domains (Data.Constr tag [])".to_string(),
        ],
        unsupported_fuzzer_types: vec![
            "Blaster translation gaps for some generated Lean predicates (e.g. List.Mem)"
                .to_string(),
            "Test arity > 1 is not supported directly; multi-input properties must be tuple/record encoded."
                .to_string(),
            "General higher-order/partial-application fuzzer resolver coverage is incomplete; unresolved shapes require extractor improvements."
                .to_string(),
        ],
        existential_modes: vec!["witness".to_string(), "proof".to_string()],
        max_test_arity: 1,
    }
}

/// Category of a proof failure, enabling structured diagnostics.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
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

/// Status of a single theorem proof
#[derive(Debug, Clone, serde::Serialize)]
pub enum ProofStatus {
    /// Theorem was proved successfully
    Proved,
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

/// Per-theorem result
#[derive(Debug, Clone, serde::Serialize)]
pub struct TheoremResult {
    pub test_name: String,
    pub theorem_name: String,
    pub status: ProofStatus,
}

/// Structured verification summary
#[derive(Debug, serde::Serialize)]
pub struct VerifySummary {
    pub total: usize,
    pub proved: usize,
    pub failed: usize,
    pub timed_out: usize,
    pub unknown: usize,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub skipped: Vec<SkippedTest>,
    pub theorems: Vec<TheoremResult>,
    pub raw_output: VerifyResult,
    /// Wall-clock milliseconds spent running proofs (dependency sync + lake build).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub elapsed_ms: Option<u64>,
    /// Final command success status. Reflects both proof results and skip-induced
    /// failures so that JSON consumers can determine the exit status without
    /// inspecting the process exit code.
    #[serde(default)]
    pub command_success: bool,
}

/// Default pinned Blaster revision. Update this when upgrading to a new tested Blaster version.
pub const DEFAULT_BLASTER_REV: &str = "95368fb83f0e359be762a64d2e75facb754d3ee2";

/// Minimum supported Lean version (major, minor, patch).
pub const MIN_LEAN_VERSION: (u32, u32, u32) = (4, 24, 0);

/// Minimum supported Z3 version (major, minor, patch).
pub const MIN_Z3_VERSION: (u32, u32, u32) = (4, 8, 0);

/// Validate that a blaster_rev string contains only safe characters for
/// interpolation into lakefile.lean. Allows alphanumeric, dots, hyphens,
/// slashes, and underscores (covering commit SHAs, tags, and branch names).
fn validate_blaster_rev(rev: &str) -> miette::Result<()> {
    if rev.is_empty() {
        return Err(miette::miette!("blaster-rev must not be empty"));
    }
    if !rev
        .chars()
        .all(|c| c.is_ascii_alphanumeric() || matches!(c, '.' | '-' | '/' | '_'))
    {
        return Err(miette::miette!(
            "blaster-rev contains invalid characters: \"{rev}\". \
             Only alphanumeric characters, dots, hyphens, slashes, and underscores are allowed."
        ));
    }
    Ok(())
}

/// Strategy for handling `fail once` (existential) tests.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum ExistentialMode {
    /// Attempt a full existential proof via Lean tactics.
    Proof,
    /// Generate a witness-based theorem: deterministic witness search
    /// followed by a concrete witness-correctness theorem.
    #[default]
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
pub struct VerifyConfig {
    pub out_dir: PathBuf,
    pub cek_budget: u64,
    /// Git revision (commit, tag, or branch) for the Blaster dependency.
    pub blaster_rev: String,
    /// Strategy for `fail once` tests (default: Witness).
    pub existential_mode: ExistentialMode,
    /// Verification target mode (default: PropertyWrapper).
    pub target: VerificationTargetKind,
    /// Explicit path to the PlutusCore Lean library. When `None`, falls back
    /// to the `PLUTUS_CORE_DIR` environment variable.
    pub plutus_core_dir: Option<PathBuf>,
}

/// Entry in the generated manifest
#[derive(Debug, serde::Serialize)]
pub struct ManifestEntry {
    pub id: String,
    pub aiken_module: String,
    pub aiken_name: String,
    pub lean_module: String,
    pub lean_theorem: String,
    pub lean_file: String,
    pub flat_file: String,
    /// Whether this test generates a separate `_alwaysTerminating` theorem.
    /// Void-mode and existential forms omit the termination theorem.
    #[serde(default)]
    pub has_termination_theorem: bool,
}

/// A test that was skipped during workspace generation (unsupported type, etc.)
#[derive(Debug, Clone, serde::Serialize)]
pub struct SkippedTest {
    pub name: String,
    pub module: String,
    pub reason: String,
}

/// Result of workspace generation
#[derive(Debug, serde::Serialize)]
pub struct GeneratedManifest {
    pub version: String,
    pub tests: Vec<ManifestEntry>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub skipped: Vec<SkippedTest>,
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
pub struct DoctorReport {
    pub tools: Vec<ToolCheck>,
    pub plutus_core: PlutusCoreCheck,
    pub blaster_rev: String,
    pub all_ok: bool,
    /// Documented verification capabilities (what is/isn't supported).
    pub capabilities: VerificationCapabilities,
}

/// Result of checking the PlutusCore Lean library.
#[derive(Debug, Clone, serde::Serialize)]
pub struct PlutusCoreCheck {
    pub found: bool,
    pub path: String,
    pub has_lakefile: bool,
    pub error: Option<String>,
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
/// If neither is set, returns a placeholder path that will not exist;
/// downstream callers (e.g. `check_plutus_core_at`) will produce a clear
/// error instructing the user to set the env var or CLI flag.
pub fn resolve_plutus_core_dir(explicit: Option<&Path>) -> PathBuf {
    if let Some(p) = explicit {
        return p.to_path_buf();
    }
    if let Ok(env_val) = std::env::var("PLUTUS_CORE_DIR") {
        return PathBuf::from(env_val);
    }
    PathBuf::from("PlutusCore")
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
/// Returns `Ok(())` when the directory and structure are valid, or an
/// informational error with instructions when they are not.
pub fn check_plutus_core(explicit: Option<&Path>) -> miette::Result<()> {
    check_plutus_core_at(&resolve_plutus_core_dir(explicit))
}

fn check_plutus_core_detailed_at(pc_dir: &Path) -> PlutusCoreCheck {
    let found = pc_dir.is_dir();
    let has_lakefile = found && pc_dir.join("lakefile.lean").exists();

    let error = if !found {
        Some(format!(
            "PlutusCore directory not found at {}",
            pc_dir.display()
        ))
    } else if !has_lakefile {
        Some(format!(
            "PlutusCore directory exists but lakefile.lean is missing at {}",
            pc_dir.join("lakefile.lean").display()
        ))
    } else {
        None
    };

    PlutusCoreCheck {
        found,
        path: pc_dir.display().to_string(),
        has_lakefile,
        error,
    }
}

/// Check the PlutusCore library and return a structured report (for doctor).
pub fn check_plutus_core_detailed(explicit: Option<&Path>) -> PlutusCoreCheck {
    check_plutus_core_detailed_at(&resolve_plutus_core_dir(explicit))
}

/// Run a full diagnostic check of the verify toolchain and dependencies.
/// Returns a structured report suitable for JSON serialization and human display.
pub fn run_doctor(out_dir: &Path, blaster_rev: &str) -> DoctorReport {
    let _ = out_dir;
    let lean_check = check_tool_version("lean", MIN_LEAN_VERSION);
    let lake_check = check_tool_version("lake", (0, 0, 0));
    let z3_check = check_tool_version("z3", MIN_Z3_VERSION);
    let plutus_core = check_plutus_core_detailed(None);

    let all_ok = doctor_all_ok(&lean_check, &lake_check, &z3_check, &plutus_core);

    DoctorReport {
        tools: vec![lean_check, lake_check, z3_check],
        plutus_core,
        blaster_rev: blaster_rev.to_string(),
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
pub fn run_proofs(
    out_dir: &Path,
    timeout_secs: u64,
    max_jobs: Option<usize>,
    manifest: &GeneratedManifest,
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

        let status = loop {
            if let Some(status) = child
                .try_wait()
                .map_err(|e| miette::miette!("Failed to poll child process: {e}"))?
            {
                break status;
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
        build_output: &str,
        timed_out: bool,
        build_exit_code: Option<i32>,
        timeout_secs: u64,
        module: &str,
        fallback_to_module_failure: bool,
        fallback_from_sibling_explicit_failure: bool,
    ) -> TheoremResult {
        let status = if timed_out {
            ProofStatus::TimedOut {
                reason: format!(
                    "Timed out after {}s while building {}",
                    timeout_secs, module
                ),
            }
        } else if build_exit_code == Some(0) {
            ProofStatus::Proved
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

        TheoremResult {
            test_name,
            theorem_name: theorem_name.to_string(),
            status,
        }
    }

    let logs_dir = out_dir.join("logs");
    fs::create_dir_all(&logs_dir)
        .map_err(|e| miette::miette!("Failed to create logs directory: {e}"))?;

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
                    .map(|theorem_name| TheoremResult {
                        test_name: test_name.clone(),
                        theorem_name,
                        status: ProofStatus::TimedOut {
                            reason: timeout_reason.clone(),
                        },
                    })
                    .collect::<Vec<_>>()
            })
            .collect();

        return Ok(VerifyResult {
            success: false,
            stdout: dep_stdout,
            stderr: dep_stderr,
            exit_code: Some(normalize_failure_exit_code(dep_exit_code)),
            theorem_results: Some(theorem_results),
        });
    }

    if dep_exit_code != Some(0) {
        return Ok(VerifyResult {
            success: false,
            stdout: dep_stdout,
            stderr: dep_stderr,
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

        theorem_results.push(theorem_status_from_module_build(
            test_name.clone(),
            &entry.lean_theorem,
            &module_output,
            module_timed_out,
            module_exit_code,
            timeout_secs,
            &entry.lean_module,
            correctness_fallback_to_module_failure,
            false,
        ));
        if let Some(term_theorem_name) = term_theorem_name {
            theorem_results.push(theorem_status_from_module_build(
                test_name.clone(),
                &term_theorem_name,
                &module_output,
                module_timed_out,
                module_exit_code,
                timeout_secs,
                &entry.lean_module,
                termination_fallback_to_module_failure,
                correctness_failed || equivalence_failed,
            ));
        }
        if let Some(equivalence_theorem_name) = equivalence_theorem_name {
            theorem_results.push(theorem_status_from_module_build(
                test_name,
                &equivalence_theorem_name,
                &module_output,
                module_timed_out,
                module_exit_code,
                timeout_secs,
                &entry.lean_module,
                equivalence_fallback_to_module_failure,
                correctness_failed || termination_failed,
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
        stdout: format!("{dep_stdout}\n{build_stdout}"),
        stderr: format!("{dep_stderr}\n{build_stderr}"),
        exit_code,
        theorem_results: Some(theorem_results),
    })
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum GenerationErrorCategory {
    UnsupportedShape,
    FallbackRequired,
    InvalidConstraint,
}

#[derive(Debug)]
struct GenerationError {
    category: GenerationErrorCategory,
    message: String,
}

impl std::fmt::Display for GenerationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.message)
    }
}

impl std::error::Error for GenerationError {}
impl miette::Diagnostic for GenerationError {}

fn generation_error(
    category: GenerationErrorCategory,
    message: impl Into<String>,
) -> miette::Report {
    miette::Report::new(GenerationError {
        category,
        message: message.into(),
    })
}

fn is_skippable_generation_error(error: &miette::Report) -> bool {
    matches!(
        error.downcast_ref::<GenerationError>().map(|e| e.category),
        Some(
            GenerationErrorCategory::UnsupportedShape
                | GenerationErrorCategory::FallbackRequired
        )
    )
}

/// Generate a Lean workspace from exported property tests.
/// When `skip_unsupported` is true, unsupported tests are collected in `skipped`
/// instead of causing an error.
pub fn generate_lean_workspace(
    tests: &[ExportedPropertyTest],
    config: &VerifyConfig,
    skip_unsupported: bool,
) -> miette::Result<GeneratedManifest> {
    validate_blaster_rev(&config.blaster_rev)?;

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
        handler_flat_content: Option<String>,
        has_termination_theorem: bool,
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
        &generate_lakefile(&config.blaster_rev, config.plutus_core_dir.as_deref()),
    )?;

    // Write lean-toolchain
    write_file(&out.join("lean-toolchain"), "leanprover/lean4:v4.24.0\n")?;

    // Write Utils.lean
    write_file(
        &out.join("AikenVerify/Utils.lean"),
        &generate_utils(config.cek_budget),
    )?;

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
        let proof_content = match generate_proof_file(
            test,
            &id,
            &lean_test_name,
            &lean_module,
            config.existential_mode,
            &config.target,
        ) {
            Ok(content) => content,
            Err(e) if skip_unsupported && is_skippable_generation_error(&e) => {
                skipped_tests.push(SkippedTest {
                    name: full_name.clone(),
                    module: module.clone(),
                    reason: e.to_string(),
                });
                continue;
            }
            Err(e) if skip_unsupported => {
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
            handler_flat_content,
            has_termination_theorem,
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
        if let Some(handler_flat_content) = entry.handler_flat_content.as_deref() {
            let handler_cbor_rel = format!("cbor/{}_handler.cbor", entry.id);
            write_file(&out.join(&handler_cbor_rel), handler_flat_content)?;
        }

        module_dirs
            .entry(entry.lean_module_segment.clone())
            .or_default()
            .push(entry.lean_test_name.clone());

        manifest_entries.push(ManifestEntry {
            id: entry.id,
            aiken_module: entry.aiken_module,
            aiken_name: entry.aiken_name,
            lean_module: entry.lean_module,
            lean_theorem: entry.lean_test_name,
            lean_file: entry.lean_file,
            flat_file: cbor_file_rel,
            has_termination_theorem: entry.has_termination_theorem,
        });
    }

    // Write root AikenVerify.lean
    write_file(
        &out.join("AikenVerify.lean"),
        &generate_root_import(&module_dirs),
    )?;

    // Write manifest.json
    let manifest = GeneratedManifest {
        version: "1.0.0".to_string(),
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
    s.replace('\\', "/")
        .replace('"', "\\\"")
}

fn generate_lakefile(blaster_rev: &str, pc_dir_override: Option<&Path>) -> String {
    let plutus_core_dir = resolve_plutus_core_dir(pc_dir_override);
    let escaped = escape_path_for_lean(&plutus_core_dir);
    format!(
        r#"import Lake
open Lake DSL

package AikenVerify where

@[default_target]
lean_lib AikenVerify where

require Blaster from git
  "https://github.com/input-output-hk/Lean-blaster" @ "{blaster_rev}"

require PlutusCore from
  "{escaped}"
"#,
    )
}

fn generate_utils(cek_budget: u64) -> String {
    format!(
        r#"import PlutusCore.UPLC.CekMachine

namespace AikenVerify.Utils
open PlutusCore.Integer (Integer)
open PlutusCore.ByteString (ByteString)
open PlutusCore.UPLC.CekMachine
open PlutusCore.UPLC.CekValue (CekValue)
open PlutusCore.Data (Data)
open PlutusCore.UPLC.Term (Term Const Program)

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

def proveTests (p : Program) (args : List Term) : Option Bool :=
  fromFrameToBool $ cekExecuteProgram p args {cek_budget}

/-- For Void-returning tests: True iff the program halts without error. -/
def proveTestsHalt (p : Program) (args : List Term) : Prop :=
  isHaltState (cekExecuteProgram p args {cek_budget})

/-- For Void+fail tests: True iff the program reaches an error state. -/
def proveTestsError (p : Program) (args : List Term) : Prop :=
  isErrorState (cekExecuteProgram p args {cek_budget})

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

/-- For `fail once` witness mode: True iff the program returns `value` for the given args. -/
def witnessTests (p : Program) (args : List Term) (value : Bool) : Prop :=
  proveTests p args = some value

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
    if let (Some(min), Some(max)) = (min_len, max_len) {
        if min > max {
            return Err(generation_error(
                GenerationErrorCategory::InvalidConstraint,
                format!(
                    "Test '{}' has inconsistent list-length bounds: min_len={} exceeds max_len={}",
                    test_name, min, max,
                ),
            ));
        }
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
            Some(format!("({a_ty} × {b_ty})"))
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
            Some(format!("({})", parts.join(" × ")))
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
                Some(format!(
                    "(let ({pattern}) := {var}; Data.List [{items}])"
                ))
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
    bytes.iter().rev().fold(
        "ByteString.empty".to_string(),
        |acc, b| format!("PlutusCore.ByteString.consByteStringV1 {} ({acc})", b),
    )
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
        .map(|tag| format!("| Data.Constr ({tag} : Integer) _ => True"))
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
        _ => format!("({})", parts.join(" ∧ ")),
    }
}

fn exported_data_schema_for_reference<'a>(
    test_name: &str,
    schema: &'a ExportedDataSchema,
    reference: &Reference,
) -> miette::Result<&'a SchemaData> {
    let key = reference.as_key();
    let Some(definition) = schema.definitions.lookup(reference) else {
        return Err(generation_error(
            GenerationErrorCategory::FallbackRequired,
            format!(
                "Test '{}' is missing exported fuzzer schema definition '{}'.",
                test_name, key
            ),
        ));
    };

    match &definition.annotated {
        Schema::Data(data) => Ok(data),
        other => Err(generation_error(
            GenerationErrorCategory::FallbackRequired,
            format!(
                "Test '{}' exported non-Data schema '{key}' ({other:?}); \
                 schema-backed state-machine lowering requires Data schemas.",
                test_name
            ),
        )),
    }
}

#[derive(Default)]
struct LeanDataShapeBuilder {
    generated_refs: BTreeMap<String, String>,
    visiting_refs: BTreeSet<String>,
    definitions: Vec<String>,
}

fn ensure_exported_schema_reference_predicate(
    builder: &mut LeanDataShapeBuilder,
    test_name: &str,
    schema: &ExportedDataSchema,
    prefix: &str,
    reference: &Reference,
) -> miette::Result<String> {
    let key = reference.as_key();
    if let Some(name) = builder.generated_refs.get(&key) {
        return Ok(name.clone());
    }
    if !builder.visiting_refs.insert(key.clone()) {
        return Err(generation_error(
            GenerationErrorCategory::FallbackRequired,
            format!(
                "Test '{}' exported recursive fuzzer schema '{}'; \
                 recursive schema lowering is not supported yet.",
                test_name, key
            ),
        ));
    }

    let name = schema_predicate_name(prefix, &key);
    let result = (|| {
        let data = exported_data_schema_for_reference(test_name, schema, reference)?;
        emit_exported_schema_data_predicate(builder, test_name, schema, prefix, &name, &key, data)
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
    prefix: &str,
    declaration: &SchemaDeclaration<SchemaData>,
    path: &str,
) -> miette::Result<String> {
    match declaration {
        SchemaDeclaration::Referenced(reference) => ensure_exported_schema_reference_predicate(
            builder, test_name, schema, prefix, reference,
        ),
        SchemaDeclaration::Inline(data) => {
            let name = schema_predicate_name(prefix, path);
            emit_exported_schema_data_predicate(
                builder, test_name, schema, prefix, &name, path, data,
            )?;
            Ok(name)
        }
    }
}

fn emit_exported_schema_data_predicate(
    builder: &mut LeanDataShapeBuilder,
    test_name: &str,
    schema: &ExportedDataSchema,
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
                prefix,
                item,
                &format!("{path}_item"),
            )?;
            let items_name = format!("{name}_items");
            builder.definitions.push(format!(
                "def {items_name} : List Data -> Prop\n  | [] => True\n  | x :: xs => {item_predicate} x ∧ {items_name} xs\n"
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
                prefix,
                keys,
                &format!("{path}_keys"),
            )?;
            let value_predicate = ensure_exported_schema_declaration_predicate(
                builder,
                test_name,
                schema,
                prefix,
                values,
                &format!("{path}_values"),
            )?;
            let entries_name = format!("{name}_entries");
            builder.definitions.push(format!(
                "def {entries_name} : List (Prod Data Data) -> Prop\n  | [] => True\n  | entry :: rest => {key_predicate} entry.1 ∧ {value_predicate} entry.2 ∧ {entries_name} rest\n"
            ));
            builder.definitions.push(format!(
                "def {name} : Data -> Prop\n  | Data.Map entries => {entries_name} entries\n  | _ => False\n"
            ));
        }
        SchemaData::AnyOf(constructors) => {
            if constructors.is_empty() {
                return Err(generation_error(
                    GenerationErrorCategory::FallbackRequired,
                    format!(
                        "Test '{}' exported an empty constructor domain at '{}'.",
                        test_name, path
                    ),
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
            return Err(generation_error(
                GenerationErrorCategory::FallbackRequired,
                format!(
                    "Test '{}' exported opaque data schema at '{}'; \
                     schema-backed state-machine lowering needs structural data shape information.",
                    test_name, path
                ),
            ));
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
    let root_predicate = ensure_exported_schema_reference_predicate(
        &mut builder,
        test_name,
        schema,
        prefix,
        &schema.root,
    )?;
    Ok((root_predicate, builder.definitions.join("\n")))
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
    let data = exported_data_schema_for_reference(test_name, schema, reference)?;
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
                return Err(generation_error(
                    GenerationErrorCategory::FallbackRequired,
                    format!(
                        "Test '{}' exported an empty constructor domain in its fuzzer schema.",
                        test_name
                    ),
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
        SchemaData::Opaque => Err(generation_error(
            GenerationErrorCategory::FallbackRequired,
            format!(
                "Test '{}' exported opaque data schema; cannot synthesize a structural witness.",
                test_name
            ),
        )),
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
) -> miette::Result<String> {
    let name = next_partial_data_semantics_name(builder, prefix, "pred");

    match semantics {
        FuzzerSemantics::Bool => {
            builder.definitions.push(format!(
                "def {name} : Data -> Prop\n  | Data.Constr (0 : Integer) [] => True\n  | Data.Constr (1 : Integer) [] => True\n  | _ => False\n"
            ));
        }
        FuzzerSemantics::IntRange { min, max } => {
            let body = match (min, max) {
                (Some(min), Some(max)) => format!("({min} <= x ∧ x <= {max})"),
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
                    format!("({min_len} <= x.length ∧ x.length <= {max_len})")
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
        FuzzerSemantics::Product(elems) => {
            let vars: Vec<String> = (0..elems.len()).map(|index| format!("x_{index}")).collect();
            let parts = elems
                .iter()
                .zip(vars.iter())
                .map(|(elem, var)| {
                    let predicate = emit_partial_data_semantics_predicate(builder, prefix, elem)?;
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
            let element_predicate =
                emit_partial_data_semantics_predicate(builder, prefix, element)?;
            let items_name = format!("{name}_items");
            builder.definitions.push(format!(
                "def {items_name} : List Data -> Prop\n  | [] => True\n  | x :: xs => {element_predicate} x ∧ {items_name} xs\n"
            ));

            let mut parts = Vec::new();
            match (min_len, max_len) {
                (Some(min_len), Some(max_len)) => {
                    parts.push(format!("({min_len} <= xs.length ∧ xs.length <= {max_len})"));
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
            return Err(generation_error(
                GenerationErrorCategory::FallbackRequired,
                "nested state-machine semantics are not supported in Data lowering",
            ));
        }
    }

    Ok(name)
}

fn build_partial_data_semantics_predicates(
    prefix: &str,
    semantics: &FuzzerSemantics,
) -> miette::Result<(String, String)> {
    let mut builder = LeanDataSemanticsBuilder::default();
    let root = emit_partial_data_semantics_predicate(&mut builder, prefix, semantics)?;
    Ok((root, builder.definitions.join("\n")))
}

fn build_state_machine_trace_reachability_helpers(
    test_name: &str,
    helper_prefix: &str,
    acceptance: &StateMachineAcceptance,
    transition_semantics: &StateMachineTransitionSemantics,
) -> miette::Result<(String, String)> {
    let label_index = transition_semantics.label_field_index;
    let next_state_index = transition_semantics.next_state_field_index;
    let event_index = transition_semantics.event_field_index;
    let mut unique_indexes = BTreeSet::new();
    unique_indexes.insert(label_index);
    unique_indexes.insert(next_state_index);
    unique_indexes.insert(event_index);
    if unique_indexes.len() != 3 {
        return Err(generation_error(
            GenerationErrorCategory::FallbackRequired,
            format!(
                "Test '{}' exported overlapping transition field indexes \\
                 (label={}, next_state={}, event={}); reachability lowering requires \\
                 distinct transition fields.",
                test_name, label_index, next_state_index, event_index
            ),
        ));
    }

    let (state_predicate, state_defs) = build_partial_data_semantics_predicates(
        &format!("{helper_prefix}_state"),
        transition_semantics.state_semantics.as_ref(),
    )?;
    let (label_predicate, label_defs) = build_partial_data_semantics_predicates(
        &format!("{helper_prefix}_label"),
        transition_semantics.label_semantics.as_ref(),
    )?;
    let (event_predicate, event_defs) = build_partial_data_semantics_predicates(
        &format!("{helper_prefix}_event"),
        transition_semantics.event_semantics.as_ref(),
    )?;

    let mut helper_blocks = Vec::new();
    helper_blocks.push(state_defs);
    helper_blocks.push(label_defs);
    helper_blocks.push(event_defs);

    let mut step_input_predicates = Vec::new();
    for (index, semantics) in transition_semantics.step_input_semantics.iter().enumerate() {
        let (predicate, defs) = build_partial_data_semantics_predicates(
            &format!("{helper_prefix}_step_input_{index}"),
            semantics,
        )?;
        step_input_predicates.push(predicate);
        helper_blocks.push(defs);
    }

    // TODO(4.2): `step_inputs_satisfiable` is a global flag asserting that *some*
    // satisfying step input exists for each step-input predicate. The `step_relation`
    // then conjoins this flag with per-transition field predicates. This is sound
    // (over-approximation: we assume any step is satisfiable when the flag holds)
    // but imprecise -- it doesn't tie a specific transition to a concrete satisfying
    // step input. The full fix requires either:
    //   (a) Nested existentials in Lean: quantify over step inputs within each
    //       `step_relation` invocation so the SMT solver must find a witness per
    //       transition, not just globally. This makes the proof obligations harder
    //       for Blaster/Z3 to discharge.
    //   (b) Pre-computing witnesses in Rust: for each transition, find a concrete
    //       step-input value satisfying the predicate and embed it as a Lean witness
    //       hint. This requires constraint solving at Rust codegen time.
    // Both approaches are significantly harder than the current design. The current
    // approach is acceptable for soundness -- it only widens the set of provable
    // traces, never narrows it.
    let step_inputs_satisfiable = format!("{helper_prefix}_step_inputs_satisfiable");
    let step_input_parts: Vec<String> = step_input_predicates
        .iter()
        .enumerate()
        .map(|(index, predicate)| format!("(∃ step_input_{index}, {predicate} step_input_{index})"))
        .collect();
    helper_blocks.push(format!(
        "def {step_inputs_satisfiable} : Prop := {}\n",
        lean_prop_conjunction(&step_input_parts)
    ));

    let terminal_predicate = format!("{helper_prefix}_trace_is_terminal");
    let label_field = format!("{helper_prefix}_trace_step_label");
    let next_state_field = format!("{helper_prefix}_trace_step_next_state");
    let event_field = format!("{helper_prefix}_trace_step_event");
    let step_relation = format!("{helper_prefix}_step_relation");
    let reachable_from = format!("{helper_prefix}_reachable_from");
    let output_projection = format!("{helper_prefix}_project_output");
    let reachable_output = format!("{helper_prefix}_reachable_output");

    helper_blocks.push(format!(
        "def {terminal_predicate} : Data -> Prop\n  | Data.Constr tag fields => tag = {} ∧ fields = []\n  | _ => False\n",
        transition_semantics.terminal_tag
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
    helper_blocks.push(format!(
        "def {step_relation} (state transition nextState label event : Data) : Prop :=\n  {state_predicate} state ∧\n  {state_predicate} nextState ∧\n  {label_predicate} label ∧\n  {event_predicate} event ∧\n  {step_inputs_satisfiable} ∧\n  {label_field} transition = some label ∧\n  {next_state_field} transition = some nextState ∧\n  {event_field} transition = some event\n"
    ));

    match acceptance {
        StateMachineAcceptance::AcceptsSuccess => {
            helper_blocks.push(format!(
                "def {reachable_from} : Data -> List Data -> List Data -> Prop\n  | _state, [], _events => False\n  | _state, [terminal], events => {terminal_predicate} terminal ∧ events = []\n  | state, transition :: rest, events =>\n      ∃ nextState label event tailEvents,\n        {step_relation} state transition nextState label event ∧\n        {reachable_from} nextState rest tailEvents ∧\n        events = event :: tailEvents\n"
            ));
            helper_blocks.push(format!(
                "def {output_projection} (events : List Data) : Data := Data.List events\n"
            ));
            helper_blocks.push(format!(
                "def {reachable_output} (x : Data) : Prop :=\n  ∃ initState trace events,\n    {state_predicate} initState ∧\n    {reachable_from} initState trace events ∧\n    x = {output_projection} events\n"
            ));
        }
        StateMachineAcceptance::AcceptsFailure => {
            helper_blocks.push(format!(
                "def {reachable_from} : Data -> List Data -> List Data -> List Data -> Prop\n  | _state, [], _labels, _events => False\n  | _state, [terminal], _labels, _events => False\n  | state, [transition, terminal], labels, events =>\n      ∃ nextState label event,\n        {step_relation} state transition nextState label event ∧\n        {terminal_predicate} terminal ∧\n        labels = [label] ∧\n        events = [event]\n  | state, transition :: rest, labels, events =>\n      ∃ nextState label event tailLabels tailEvents,\n        {step_relation} state transition nextState label event ∧\n        {reachable_from} nextState rest tailLabels tailEvents ∧\n        labels = label :: tailLabels ∧\n        events = event :: tailEvents\n"
            ));
            helper_blocks.push(format!(
                "def {output_projection} (labels events : List Data) : Data :=\n  Data.List [Data.List labels, Data.List events]\n"
            ));
            helper_blocks.push(format!(
                "def {reachable_output} (x : Data) : Prop :=\n  ∃ initState trace labels events,\n    {state_predicate} initState ∧\n    {reachable_from} initState trace labels events ∧\n    x = {output_projection} labels events\n"
            ));
        }
    }

    let definitions = helper_blocks
        .into_iter()
        .filter(|block| !block.trim().is_empty())
        .collect::<Vec<_>>()
        .join("\n");

    Ok((reachable_output, definitions))
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
        ..
    } = &test.semantics
    else {
        return Ok(None);
    };

    let Some(schema) = test.fuzzer_data_schema.as_ref() else {
        return Ok(None);
    };

    let helper_prefix = format!("{lean_test_name}_shape");
    let (root_predicate, helper_defs) =
        build_exported_data_shape_predicates(&test.name, schema, &helper_prefix)?;
    let (semantics_predicate, semantics_defs) = build_partial_data_semantics_predicates(
        &format!("{helper_prefix}_semantics"),
        output_semantics.as_ref(),
    )?;
    let (reachability_predicate, reachability_defs) =
        build_state_machine_trace_reachability_helpers(
            &test.name,
            &format!("{helper_prefix}_reachability"),
            acceptance,
            transition_semantics,
        )?;

    let quantifier_vars = "(x : Data)";
    let precondition_parts = vec![
        format!("{root_predicate} x"),
        format!("{semantics_predicate} x"),
        format!("{reachability_predicate} x"),
    ];
    // TODO(4.6): witness_value is always None for state-machine trace proofs.
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
                        "Test '{}' has Bool semantics for non-Bool output type {:?}.",
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
                        "Test '{}' has Int semantics for non-Int output type {:?}.",
                        test_name, output_type
                    ),
                ));
            }

            match (min, max) {
                (Some(min), Some(max)) => {
                    validate_int_bounds_literals(test_name, min, max)?;
                    out.push(format!("({min} <= {var} \u{2227} {var} <= {max})"));
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
                        "Test '{}' has byte-array semantics for non-byte-string output type {:?}.",
                        test_name, output_type
                    ),
                ));
            }

            match (min_len, max_len) {
                (Some(min_len), Some(max_len)) => {
                    validate_bytestring_len_bounds(test_name, *min_len, *max_len)?;
                    out.push(format!(
                        "({min_len} <= {var}.length \u{2227} {var}.length <= {max_len})"
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
                        "Test '{}' has String semantics for non-String output type {:?}.",
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
                        "Test '{}' has Data semantics for non-Data output type {:?}.",
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
                        "Test '{}' has exact semantic value {:?} that cannot be translated for output type {:?}.",
                        test_name, value, output_type
                    ),
                )
            })?;
            out.push(format!("({var} = {lit})"));
            *witness = Some(lit);
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
                        "Test '{}' has constructor semantics for non-Data output type {:?}.",
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
                "Test '{}' has non-scalar semantic domain {:?} for output type {:?}.",
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
                        "Test '{}' has Bool list-element semantics for non-Bool element type {:?}.",
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
                        "Test '{}' has Int list-element semantics for non-Int element type {:?}.",
                        test_name, elem_type
                    ),
                ));
            }

            match (min, max) {
                (Some(min), Some(max)) => {
                    validate_int_bounds_literals(test_name, min, max)?;
                    out.push(format!(
                        "(\u{2200} x_i, x_i \u{2208} {list_var} \u{2192} {min} <= x_i \u{2227} x_i <= {max})"
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
                    out.push(format!(
                        "(\u{2200} x_i, x_i \u{2208} {list_var} \u{2192} {min} <= x_i)"
                    ));
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
                    out.push(format!(
                        "(\u{2200} x_i, x_i \u{2208} {list_var} \u{2192} x_i <= {max})"
                    ));
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
                        "Test '{}' has byte-array list-element semantics for incompatible element type {:?}.",
                        test_name, elem_type
                    ),
                ));
            }

            match (min_len, max_len) {
                (Some(min_len), Some(max_len)) => {
                    validate_bytestring_len_bounds(test_name, *min_len, *max_len)?;
                    out.push(format!(
                        "(\u{2200} x_i, x_i \u{2208} {list_var} \u{2192} {min_len} <= x_i.length \u{2227} x_i.length <= {max_len})"
                    ));
                    *witness_elem = Some(lean_zero_bytestring_literal_of_len(*min_len));
                }
                (Some(min_len), None) => {
                    out.push(format!(
                        "(\u{2200} x_i, x_i \u{2208} {list_var} \u{2192} {min_len} <= x_i.length)"
                    ));
                    *witness_elem = Some(lean_zero_bytestring_literal_of_len(*min_len));
                }
                (None, Some(max_len)) => {
                    out.push(format!(
                        "(\u{2200} x_i, x_i \u{2208} {list_var} \u{2192} x_i.length <= {max_len})"
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
                        "Test '{}' has String list-element semantics for non-String element type {:?}.",
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
                        "Test '{}' has Data list-element semantics for incompatible element type {:?}.",
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
                        "Test '{}' has unsupported exact list-element semantic value {:?} for element type {:?}.",
                        test_name, value, elem_type
                    ),
                )
            })?;
            out.push(format!(
                "(\u{2200} x_i, x_i \u{2208} {list_var} \u{2192} x_i = {lit})"
            ));
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
                        "Test '{}' has constructor list-element semantics for incompatible element type {:?}.",
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
            out.push(format!(
                "(\u{2200} x_i, x_i \u{2208} {list_var} \u{2192} {predicate})"
            ));
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
            if matches!(elem_type, FuzzerOutputType::Pair(_, _) | FuzzerOutputType::Tuple(_)) {
                // Product semantics for Pair/Tuple elements: accept without per-element predicate
                Ok(())
            } else {
                Err(generation_error(
                    GenerationErrorCategory::FallbackRequired,
                    format!(
                        "Test '{}' has Product list-element semantics for non-tuple element type {:?}.",
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
                "Test '{}' has unsupported structured list-element semantics {:?} for element type {:?}.",
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
                    "({min} <= {list_var}.length \u{2227} {list_var}.length <= {max})"
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
                "Test '{}' has non-list semantic domain {:?} for list output.",
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
    /// The correctness theorem conclusion, e.g. `(proveTests {prog} ({arg})) = true`
    correctness: TheoremBody,
    /// The termination theorem conclusion.
    /// None when the correctness theorem already implies termination (Void mode).
    termination: Option<TheoremBody>,
    /// Whether this is an existential (fail once) theorem.
    existential: bool,
    /// The existential mode used (only Some for existential theorems).
    existential_mode: Option<ExistentialMode>,
}

enum TheoremBody {
    /// `(proveTests {prog} ({arg})) = {value}`
    BoolEq { value: &'static str },
    /// `proveTestsHalt {prog} ({arg})`
    Halt,
    /// `isErrorState (cekExecuteProgram {prog} ({arg}) {budget})`
    ErrorState,
    /// `Option.isSome (proveTests {prog} ({arg}))`
    IsSome,
    /// Witness mode for `fail once`: concrete witness + `witnessTests {prog} ({arg}) {value}`
    WitnessEq { value: &'static str },
    /// Witness mode for `fail once` + Void: concrete witness + `proveTestsError {prog} ({arg})`
    WitnessError,
    /// Proof mode for `fail once`: existential theorem `∃ x, (proveTests {prog} ({arg})) = {value}`
    ExistsEq { value: &'static str },
    /// Proof mode for `fail once` + Void: `∃ x, proveTestsError {prog} ({arg})`
    ExistsError,
}

impl TheoremBody {
    /// Format the theorem conclusion, substituting prog and arg expression.
    fn format(&self, prog: &str, arg_expr: &str) -> String {
        match self {
            TheoremBody::BoolEq { value } => {
                format!("(proveTests {prog} ({arg_expr})) = {value}")
            }
            TheoremBody::Halt => {
                format!("proveTestsHalt {prog} ({arg_expr})")
            }
            TheoremBody::ErrorState => {
                format!("proveTestsError {prog} ({arg_expr})")
            }
            TheoremBody::IsSome => {
                format!("Option.isSome (proveTests {prog} ({arg_expr}))")
            }
            TheoremBody::WitnessEq { value } => {
                format!("witnessTests {prog} ({arg_expr}) {value}")
            }
            TheoremBody::WitnessError => {
                format!("proveTestsError {prog} ({arg_expr})")
            }
            TheoremBody::ExistsEq { value } => {
                format!("(proveTests {prog} ({arg_expr})) = {value}")
            }
            TheoremBody::ExistsError => {
                format!("proveTestsError {prog} ({arg_expr})")
            }
        }
    }

    /// The tactic to use for this theorem body.
    fn tactic(&self) -> &'static str {
        match self {
            TheoremBody::WitnessEq { .. } | TheoremBody::WitnessError => "decide",
            _ => "blaster",
        }
    }
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
    prog: &str,
    arg_expr: &str,
    quantifier_vars: &str,
    precondition_parts: &[String],
    witness_value: Option<&str>,
) -> String {
    let correctness_body = form.correctness.format(prog, arg_expr);
    let correctness_tactic = form.correctness.tactic();

    let mut out = if form.existential {
        let quantifiers = format!("\u{2203} {quantifier_vars},");
        let preconditions = if precondition_parts.is_empty() {
            String::new()
        } else {
            format!("\n  {}", precondition_parts.join("\n  \u{2227}\n  "))
        };
        let connector = if preconditions.is_empty() {
            ""
        } else {
            "\n  \u{2227}\n  "
        };
        let mode_comment = match form.existential_mode {
            Some(ExistentialMode::Witness) => "-- Mode: witness (deterministic witness search)\n",
            Some(ExistentialMode::Proof) => "-- Mode: proof (existential proof attempt)\n",
            None => "",
        };

        let proof_term = if let Some(witness) = witness_value {
            format!("\u{27e8}{witness}, by {correctness_tactic}\u{27e9}")
        } else {
            format!("by {correctness_tactic}")
        };

        format!(
            "{mode_comment}theorem {lean_test_name} :\n  {quantifiers}{preconditions}{connector}{correctness_body} :=\n  {proof_term}\n"
        )
    } else {
        let quantifiers = format!("\u{2200} {quantifier_vars},");
        let preconditions = if precondition_parts.is_empty() {
            String::new()
        } else {
            format!(
                "\n  {}",
                precondition_parts.join("\n  \u{2192}\n  ")
            )
        };
        let arrow = if preconditions.is_empty() {
            ""
        } else {
            "\n  \u{2192}\n  "
        };
        format!(
            "theorem {lean_test_name} :\n  {quantifiers}{preconditions}{arrow}{correctness_body} :=\n  by {correctness_tactic}\n"
        )
    };

    if let Some(ref term_body) = form.termination {
        let termination_body = term_body.format(prog, arg_expr);
        let termination_tactic = term_body.tactic();
        // Termination theorems are always universal
        let quantifiers = format!("\u{2200} {quantifier_vars},");
        let preconditions = if precondition_parts.is_empty() {
            String::new()
        } else {
            format!(
                "\n  {}",
                precondition_parts.join("\n  \u{2192}\n  ")
            )
        };
        let arrow = if preconditions.is_empty() {
            ""
        } else {
            "\n  \u{2192}\n  "
        };
        out.push_str(&format!(
            "\ntheorem {lean_test_name}_alwaysTerminating :\n  {quantifiers}{preconditions}{arrow}{termination_body} :=\n  by {termination_tactic}\n"
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
            "proveTestsHalt {prop_prog} ({arg_expr}) ↔ proveTestsHalt {handler_prog} ({arg_expr})"
        ),
        (TestReturnMode::Void, OnTestFailure::SucceedEventually)
        | (TestReturnMode::Void, OnTestFailure::SucceedImmediately) => format!(
            "proveTestsError {prop_prog} ({arg_expr}) ↔ proveTestsError {handler_prog} ({arg_expr})"
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
    // Equivalence theorems are always universal
    let quantifiers = format!("\u{2200} {quantifier_vars},");
    let preconditions = if precondition_parts.is_empty() {
        String::new()
    } else {
        format!(
            "\n  {}",
            precondition_parts.join("\n  \u{2192}\n  ")
        )
    };
    let arrow = if preconditions.is_empty() {
        ""
    } else {
        "\n  \u{2192}\n  "
    };
    let equivalence_goal = equivalence_goal(test, prop_prog, handler_prog, arg_expr);
    format!(
        "\ntheorem {lean_test_name}_equivalence :\n  \
         {quantifiers}{preconditions}{arrow}\
         {equivalence_goal} :=\n  \
         by blaster\n"
    )
}

fn determine_theorem_form(
    test: &ExportedPropertyTest,
    existential_mode: ExistentialMode,
) -> miette::Result<TheoremForm> {
    use crate::export::TestReturnMode;
    use aiken_lang::ast::OnTestFailure;

    match (&test.return_mode, &test.on_test_failure) {
        (TestReturnMode::Bool, OnTestFailure::FailImmediately) => Ok(TheoremForm {
            correctness: TheoremBody::BoolEq { value: "true" },
            termination: Some(TheoremBody::IsSome),
            existential: false,
            existential_mode: None,
        }),
        (TestReturnMode::Bool, OnTestFailure::SucceedEventually) => Ok(TheoremForm {
            correctness: TheoremBody::BoolEq { value: "false" },
            termination: Some(TheoremBody::IsSome),
            existential: false,
            existential_mode: None,
        }),
        (TestReturnMode::Bool, OnTestFailure::SucceedImmediately) => {
            // `fail once` for Bool succeeds when we find a falsifying input.
            // Therefore existential theorems must witness `proveTests ... = false`.
            match existential_mode {
                ExistentialMode::Witness => Ok(TheoremForm {
                    // Witness mode: concrete witness + `proveTests ... = false`
                    // The tactic is `decide` (evaluation-based proof search)
                    // If this deterministic witness is too weak for a property,
                    // use `ExistentialMode::Proof` to search existentially.
                    correctness: TheoremBody::WitnessEq { value: "false" },
                    termination: None,
                    existential: true,
                    existential_mode: Some(existential_mode),
                }),
                ExistentialMode::Proof => Ok(TheoremForm {
                    correctness: TheoremBody::ExistsEq { value: "false" },
                    termination: None,
                    existential: true,
                    existential_mode: Some(existential_mode),
                }),
            }
        }
        (TestReturnMode::Void, OnTestFailure::FailImmediately) => Ok(TheoremForm {
            // Void default: program halts without error for all inputs
            correctness: TheoremBody::Halt,
            // Halt already implies termination
            termination: None,
            existential: false,
            existential_mode: None,
        }),
        (TestReturnMode::Void, OnTestFailure::SucceedEventually) => Ok(TheoremForm {
            // Void + fail: program errors for all inputs
            correctness: TheoremBody::ErrorState,
            // Error state is also a form of termination (no infinite loop)
            termination: None,
            existential: false,
            existential_mode: None,
        }),
        (TestReturnMode::Void, OnTestFailure::SucceedImmediately) => {
            // `fail once` + Void: there exists an input that errors
            match existential_mode {
                ExistentialMode::Witness => Ok(TheoremForm {
                    correctness: TheoremBody::WitnessError,
                    termination: None,
                    existential: true,
                    existential_mode: Some(existential_mode),
                }),
                ExistentialMode::Proof => Ok(TheoremForm {
                    correctness: TheoremBody::ExistsError,
                    termination: None,
                    existential: true,
                    existential_mode: Some(existential_mode),
                }),
            }
        }
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
                generation_error(
                    GenerationErrorCategory::UnsupportedShape,
                    format!(
                        "Test '{}' has no validator target metadata; \
                         --target {} requires a validator_target with handler_program. \
                         This test can only be verified in --target property mode.",
                        test.name, target
                    ),
                )
            })?;
            if vt.handler_program.is_none() {
                return Err(generation_error(
                    GenerationErrorCategory::UnsupportedShape,
                    format!(
                        "Test '{}' has validator target metadata but no handler program; \
                         --target {} requires an exported handler program.",
                        test.name, target
                    ),
                ));
            }
            Ok(())
        }
    }
}

fn generate_proof_file(
    test: &ExportedPropertyTest,
    test_id: &str,
    lean_test_name: &str,
    lean_module: &str,
    existential_mode: ExistentialMode,
    target: &VerificationTargetKind,
) -> miette::Result<String> {
    ensure_target_kind_compatible(test, target)?;

    let form = determine_theorem_form(test, existential_mode)?;

    let prog = prog_name(test_id);
    let handler_prog = format!("handler_{}", prog_name(test_id));

    // Common header for all proof files.
    let direct_header = |extra_opens: &str| -> String {
        let mut s = format!(
            "import AikenVerify.Utils\nimport PlutusCore.UPLC.ScriptEncoding\nimport Blaster\n\n\
             namespace {lean_module}\n"
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
        s.push_str(&format!(
            "\n#import_uplc {prog} single_cbor_hex \"./cbor/{test_id}.cbor\"\n"
        ));
        // For validator/equivalence modes, also import the handler program
        match target {
            VerificationTargetKind::ValidatorHandler | VerificationTargetKind::Equivalence => {
                s.push_str(&format!(
                    "#import_uplc {handler_prog} single_cbor_hex \"./cbor/{test_id}_handler.cbor\"\n"
                ));
            }
            _ => {}
        }
        s.push('\n');
        s
    };

    let footer = format!("\nend {lean_module}\n");

    // Select which program to verify based on target mode
    let verify_prog = match target {
        VerificationTargetKind::ValidatorHandler => handler_prog.clone(),
        _ => prog.clone(),
    };

    if let Some(content) = try_generate_direct_proof_from_semantics(
        test,
        &form,
        lean_test_name,
        &verify_prog,
        &direct_header,
        &footer,
        target,
        &prog,
        &handler_prog,
    )? {
        return Ok(content);
    }

    Err(generation_error(
        GenerationErrorCategory::FallbackRequired,
        format!(
            "Test '{}' cannot be formally verified. \
             Fuzzer output type: {:?}. Semantics: {:?}. \
             The fuzzer domain could not be statically analyzed for direct proof generation.",
            test.name, test.fuzzer_output_type, test.semantics
        ),
    ))
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
                        "Test '{}': tuple element {} has type {:?} which has no Lean type mapping.",
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
                        "Test '{}': tuple element {} has type {:?} which has no Lean data encoder.",
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
    let mut content = format!("{}{theorems}", header(&opens));
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

#[allow(clippy::too_many_arguments)]
fn try_generate_direct_proof_from_semantics(
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
        let mut content = format!("{}{theorems}", direct_header(opens));
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
        (FuzzerOutputType::Int, semantics) => {
            let (precondition_parts, witness) =
                build_scalar_domain_preconditions_from_semantics(
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
            let (precondition_parts, witness) =
                build_scalar_domain_preconditions_from_semantics(
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
            let (precondition_parts, witness) =
                build_scalar_domain_preconditions_from_semantics(
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
            let (precondition_parts, witness) =
                build_scalar_domain_preconditions_from_semantics(
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
            // emit_exported_schema_data_predicate. For now, if the schema lowering fails
            // (recursive types, opaque fields, etc.) we silently fall through to the
            // semantics-only path.
            let schema_helper_defs = if matches!(semantics, FuzzerSemantics::Data)
                || matches!(semantics, FuzzerSemantics::Constructors { .. })
            {
                if let Some(schema) = test.fuzzer_data_schema.as_ref() {
                    let helper_prefix = format!("{lean_test_name}_schema");
                    match build_exported_data_shape_predicates(&test.name, schema, &helper_prefix) {
                        Ok((root_predicate, defs)) => {
                            precondition_parts.push(format!("{root_predicate} x"));
                            Some(defs)
                        }
                        Err(_) => {
                            // Schema lowering failed (recursive schema, opaque fields, etc.)
                            // Fall through to semantics-only path without schema predicate.
                            None
                        }
                    }
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
        (FuzzerOutputType::List(elem_type), FuzzerSemantics::List { .. }) => {
            if lean_type_for(elem_type).is_none() || lean_data_encoder(elem_type, "x_i").is_none() {
                return Err(generation_error(
                    GenerationErrorCategory::UnsupportedShape,
                    format!(
                        "Test '{}': list element type {:?} has no Lean type mapping or Data encoder",
                        test.name, elem_type
                    ),
                ));
            }
            let (precondition_parts, witness_min_len, witness_elem) =
                build_list_domain_preconditions_from_semantics(
                    &test.name,
                    elem_type.as_ref(),
                    &test.semantics,
                    "xs",
                )?;

            let elem_lean_type = lean_type_for(elem_type).expect("checked above");
            let elem_encoder = lean_data_encoder(elem_type, "x_i").expect("checked above");
            let opens = lean_opens_for_types(&[elem_type.as_ref()]);
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
                        "Test '{}': pair element type has no Lean type mapping (fst: {:?}, snd: {:?})",
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
                        "Test '{}': tuple element type has no Lean type mapping ({:?})",
                        test.name, types
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
                 Fuzzer output type: {:?}. Semantics: {:?}. \
                 The fuzzer domain could not be statically analyzed for direct proof generation.",
                test.name, output_type, semantics
            ),
        )),
    }
}

/// Validate that a test can be translated to a Lean theorem shape for the
/// selected mode/target without writing workspace files.
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
    )
    .map(|_| ())
}

mod result_parser;
pub use result_parser::parse_verify_results;
use result_parser::{
    classify_failure, extract_error_for_module, extract_error_for_theorem,
    extract_global_failure_reason, theorem_has_explicit_failure,
};

#[cfg(test)]
mod tests;
