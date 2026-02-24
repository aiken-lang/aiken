use crate::export::{
    ExportedPropertyTest, FuzzerConstraint, FuzzerOutputType, VerificationTargetKind,
};
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fs;
use std::hash::{Hash, Hasher};
use std::io::Read;
use std::path::{Path, PathBuf};
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
            "validator".to_string(),
            "equivalence".to_string(),
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
        ],
        unsupported_fuzzer_types: vec![
            "nested List/Tuple/Pair".to_string(),
            "Blaster translation gaps for some generated Lean predicates (e.g. List.Mem)"
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
    pub theorems: Vec<TheoremResult>,
    pub raw_output: VerifyResult,
    /// Wall-clock milliseconds spent running proofs (dependency sync + lake build).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub elapsed_ms: Option<u64>,
}

/// Default pinned Blaster revision. Update this when upgrading to a new tested Blaster version.
pub const DEFAULT_BLASTER_REV: &str = "main";

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
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum ExistentialMode {
    /// Attempt a full existential proof via Lean tactics.
    Proof,
    /// Generate a witness-based theorem: deterministic witness search
    /// followed by a concrete witness-correctness theorem.
    Witness,
}

impl Default for ExistentialMode {
    fn default() -> Self {
        ExistentialMode::Witness
    }
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum ArtifactRetention {
    /// Keep artifacts only when proofs fail or have unknown/timed-out status (default).
    /// Successful runs clean up artifacts.
    OnFailure,
    /// Keep artifacts only after a fully successful run.
    OnSuccess,
    /// Always keep artifacts regardless of outcome.
    Always,
    /// Never keep artifacts (remove after every run).
    Never,
}

impl Default for ArtifactRetention {
    fn default() -> Self {
        ArtifactRetention::OnFailure
    }
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
    let mut removed = Vec::new();
    if out_dir.exists() {
        // Remove the entire output directory
        fs::remove_dir_all(out_dir)?;
        removed.push(out_dir.to_path_buf());
    }
    Ok(removed)
}

fn remove_path_if_exists(path: &Path) -> miette::Result<()> {
    let metadata = match fs::symlink_metadata(path) {
        Ok(metadata) => metadata,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(()),
        Err(e) => return Err(miette::miette!("Failed to inspect {}: {e}", path.display())),
    };

    let result = if metadata.is_dir() && !metadata.file_type().is_symlink() {
        fs::remove_dir_all(path)
    } else {
        fs::remove_file(path)
    };

    result.map_err(|e| miette::miette!("Failed to remove {}: {e}", path.display()))
}

/// Clear generated verification outputs before writing a fresh workspace.
///
/// This intentionally preserves static dependencies and caches (`PlutusCore`,
/// `.lake/packages`, and non-AikenVerify build outputs) so successive
/// `verify run` calls can reuse expensive Lean build artifacts.
pub fn clear_generated_workspace(out_dir: &Path) -> miette::Result<()> {
    if !out_dir.exists() {
        return Ok(());
    }

    for rel in [
        "AikenVerify",
        "AikenVerify.lean",
        "flat",
        "manifest.json",
        "logs",
        ".lake/build/lib/AikenVerify",
        ".lake/build/ir/AikenVerify",
        ".lake/build/bin/AikenVerify",
        ".lake/build/obj/AikenVerify",
    ] {
        remove_path_if_exists(&out_dir.join(rel))?;
    }

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
        "def",
        "theorem",
        "lemma",
        "where",
        "import",
        "open",
        "namespace",
        "end",
        "by",
        "match",
        "do",
        "let",
        "have",
        "if",
        "else",
        "return",
        "fun",
        "in",
        "with",
        "structure",
        "class",
        "instance",
        "section",
        "variable",
        "mutual",
        "protected",
        "private",
        "noncomputable",
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
    let module_part = sanitize_lean_name(&module.replace('/', "_"));
    let name_part = sanitize_lean_name(name);
    let hash = short_hash(&format!("{module}.{name}"));
    format!("{module_part}__{name_part}_{hash}")
}

/// Produce an 8-hex-char hash of the input string for use as a disambiguation suffix.
fn short_hash(input: &str) -> String {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    input.hash(&mut hasher);
    let h = hasher.finish();
    format!("{:08x}", h as u32)
}

/// Detect collisions in generated Lean file paths before writing any files.
/// The collision surface is `(lean_module_segment, lean_test_name)` since those
/// determine the `.lean` file path, which does not include the hash suffix.
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
        problems.push(format!("lean: not found in PATH"));
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
        problems.push(format!("lake: not found in PATH"));
    }

    let z3_check = check_tool_version("z3", MIN_Z3_VERSION);
    if !z3_check.found {
        problems.push(format!("z3: not found in PATH"));
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

    if !output.status.success() {
        return ToolCheck {
            tool: name.to_string(),
            found: true,
            version: None,
            meets_minimum: false,
            minimum_version,
            error: Some(format!(
                "`{name} --version` exited with code {}",
                output.status.code().unwrap_or(-1)
            )),
        };
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    let combined = format!("{stdout}\n{stderr}");

    let version_str = parse_version_from_output(&combined);

    let meets_minimum = version_str
        .as_ref()
        .map(|v| version_meets_minimum(v, min))
        .unwrap_or(false); // unparseable version output cannot satisfy any minimum

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

/// Check whether the PlutusCore Lean library exists in the workspace and
/// contains the expected structure (at minimum, a `lakefile.lean`).
/// Returns `Ok(())` when the directory and structure are valid, or an
/// informational error with instructions when they are not.
pub fn check_plutus_core(out_dir: &Path) -> miette::Result<()> {
    let pc_dir = out_dir.join("PlutusCore");
    if !pc_dir.is_dir() {
        return Err(miette::miette!(
            "PlutusCore Lean library not found at {path}.\n\n\
             The generated lakefile.lean expects a local dependency at ./PlutusCore.\n\
             Either symlink or copy the PlutusCore library there:\n\n\
             \tln -s /path/to/PlutusCore {path}\n\n\
             Then re-run `aiken verify` or manually: cd {out} && lake build",
            path = pc_dir.display(),
            out = out_dir.display(),
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

/// Check the PlutusCore library and return a structured report (for doctor).
pub fn check_plutus_core_detailed(out_dir: &Path) -> PlutusCoreCheck {
    let pc_dir = out_dir.join("PlutusCore");
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

/// Run a full diagnostic check of the verify toolchain and dependencies.
/// Returns a structured report suitable for JSON serialization and human display.
pub fn run_doctor(out_dir: &Path, blaster_rev: &str) -> DoctorReport {
    let lean_check = check_tool_version("lean", MIN_LEAN_VERSION);
    let lake_check = check_tool_version("lake", (0, 0, 0));
    let z3_check = check_tool_version("z3", MIN_Z3_VERSION);
    let plutus_core = check_plutus_core_detailed(out_dir);

    let all_ok = lean_check.found
        && lean_check.meets_minimum
        && lake_check.found
        && lake_check.meets_minimum
        && z3_check.found
        && z3_check.meets_minimum
        && plutus_core.found
        && plutus_core.has_lakefile;

    DoctorReport {
        tools: vec![lean_check, lake_check, z3_check],
        plutus_core,
        blaster_rev: blaster_rev.to_string(),
        all_ok,
        capabilities: capabilities(),
    }
}

fn lake_fetch_is_unknown_command(stderr: &str) -> bool {
    let stderr_lower = stderr.to_ascii_lowercase();
    stderr_lower.contains("unknown command 'fetch'")
        || stderr_lower.contains("unknown command `fetch`")
        || stderr_lower.contains("unknown command \"fetch\"")
}

/// Run dependency sync (`lake fetch` with `lake update` fallback) then
/// `lake build` in the generated workspace directory.
/// Logs are written to `<out_dir>/logs/`.
pub fn run_proofs(
    out_dir: &Path,
    timeout_secs: u64,
    _max_jobs: usize,
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
                #[cfg(unix)]
                {
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
                #[cfg(not(unix))]
                {
                    let _ = child.kill();
                }
                break child
                    .wait()
                    .map_err(|e| miette::miette!("Failed to wait for timed out command: {e}"))?;
            }

            thread::sleep(Duration::from_millis(50));
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

    // Retained for future per-module parallel builds (see --jobs flag comment).
    #[allow(dead_code)]
    fn theorem_status_from_module_build(
        test_name: String,
        theorem_name: &str,
        build_output: &str,
        timed_out: bool,
        timeout_secs: u64,
        module: &str,
    ) -> TheoremResult {
        fn output_indicates_failure(output: &str) -> bool {
            output.contains("❌ Falsified")
                || output.contains("Counterexample:")
                || output.contains("Tactic `blaster` failed")
                || output.contains("unsolved goals")
                || output.contains("error: build failed")
                || output.contains("error: Lean exited with code")
        }

        let status = if timed_out {
            ProofStatus::TimedOut {
                reason: format!(
                    "Timed out after {}s while building {}",
                    timeout_secs, module
                ),
            }
        } else if build_output.contains(&format!("error: '{}' ", theorem_name))
            || build_output.contains(&format!("'{}'", theorem_name))
            || output_indicates_failure(build_output)
        {
            let extracted = extract_error_for_theorem(theorem_name, build_output);
            let category = classify_failure(build_output);
            let reason = if extracted.is_empty() {
                format!("Proof failed while building {module}")
            } else {
                extracted
            };
            ProofStatus::Failed { category, reason }
        } else {
            ProofStatus::Unknown
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
        && lake_fetch_is_unknown_command(&fetch_output.stderr)
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
                let mut results = vec![TheoremResult {
                    test_name: test_name.clone(),
                    theorem_name: entry.lean_theorem.clone(),
                    status: ProofStatus::TimedOut {
                        reason: timeout_reason.clone(),
                    },
                }];
                if entry.has_termination_theorem {
                    results.push(TheoremResult {
                        test_name,
                        theorem_name: format!("{}_alwaysTerminating", entry.lean_theorem),
                        status: ProofStatus::TimedOut {
                            reason: timeout_reason,
                        },
                    });
                }
                results
            })
            .collect();

        return Ok(VerifyResult {
            success: false,
            stdout: dep_stdout,
            stderr: dep_stderr,
            exit_code: dep_exit_code,
            theorem_results: Some(theorem_results),
        });
    }

    if dep_exit_code != Some(0) {
        return Ok(VerifyResult {
            success: false,
            stdout: dep_stdout,
            stderr: dep_stderr,
            exit_code: dep_exit_code,
            theorem_results: None,
        });
    }

    // Build all modules with a single `lake build` invocation.
    // We intentionally do not forward `--jobs` here because Lake 5 rejects
    // `-jN`, and we want default `verify run` flows to succeed reliably.
    // Running multiple concurrent `lake build` processes in the same workspace
    // can also corrupt `.olean` files.
    let mut build_cmd = Command::new("lake");
    build_cmd.arg("build").current_dir(out_dir);

    let build_result = run_command_with_timeout(build_cmd, timeout_secs);

    let (build_stdout, build_stderr, build_exit_code, build_timed_out) = match build_result {
        Ok(r) => (r.stdout, r.stderr, r.exit_code, r.timed_out),
        Err(e) => {
            // OS-level spawn failure (e.g. lake not found). The build never ran,
            // so report every theorem as BuildError rather than silently dropping.
            let reason = format!("Failed to execute lake build: {e}");
            return Err(miette::miette!("{reason}"));
        }
    };

    let _ = fs::write(
        logs_dir.join("lake-build.log"),
        format!(
            "command: lake build\n--- stdout ---\n{}\n--- stderr ---\n{}\n",
            build_stdout, build_stderr
        ),
    );

    let success = build_exit_code == Some(0) && !build_timed_out;

    let mut stderr_with_marker = build_stderr.clone();
    if build_timed_out {
        stderr_with_marker.push_str("\n[verify-timeout]");
    }

    Ok(VerifyResult {
        success,
        stdout: format!("{dep_stdout}\n{build_stdout}"),
        stderr: format!("{dep_stderr}\n{stderr_with_marker}"),
        exit_code: build_exit_code,
        theorem_results: None,
    })
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
    detect_lean_path_collisions(tests)?;

    let out = &config.out_dir;
    clear_generated_workspace(out)?;

    // Create directory structure
    fs::create_dir_all(out.join("AikenVerify/Proofs"))
        .map_err(|e| miette::miette!("Failed to create workspace directories: {e}"))?;
    fs::create_dir_all(out.join("flat"))
        .map_err(|e| miette::miette!("Failed to create flat directory: {e}"))?;

    // Write lakefile.lean
    write_file(
        &out.join("lakefile.lean"),
        &generate_lakefile(&config.blaster_rev),
    )?;

    // Write lean-toolchain
    write_file(&out.join("lean-toolchain"), "leanprover/lean4:v4.24.0\n")?;

    // Write Utils.lean
    write_file(
        &out.join("AikenVerify/Utils.lean"),
        &generate_utils(config.cek_budget),
    )?;

    // Collect manifest entries and generate per-test files
    let mut manifest_entries = Vec::new();
    let mut skipped_tests = Vec::new();
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

        // Directory for this module's proofs
        let proof_dir = out.join("AikenVerify/Proofs").join(&lean_module_segment);
        fs::create_dir_all(&proof_dir)
            .map_err(|e| miette::miette!("Failed to create proof directory: {e}"))?;

        // Lean file path relative to out_dir
        let lean_file_rel =
            format!("AikenVerify/Proofs/{lean_module_segment}/{lean_test_name}.lean");

        // Write the .lean proof file for this test
        let proof_content = match generate_proof_file(
            test,
            &id,
            &lean_test_name,
            &lean_module,
            config.existential_mode,
            &config.target,
        ) {
            Ok(content) => content,
            Err(e) if skip_unsupported => {
                skipped_tests.push(SkippedTest {
                    name: full_name.clone(),
                    module: module.clone(),
                    reason: e.to_string(),
                });
                continue;
            }
            Err(e) => return Err(e),
        };
        write_file(&out.join(&lean_file_rel), &proof_content)?;

        // Write the flat file (hex content)
        let flat_file_rel = format!("flat/{id}.flat");
        write_file(&out.join(&flat_file_rel), &test.test_program.hex)?;
        let fuzzer_flat_rel = format!("flat/{id}_fuzzer.flat");
        write_file(&out.join(&fuzzer_flat_rel), &test.fuzzer_program.hex)?;

        // For validator/equivalence modes, also write the handler flat file
        if matches!(
            config.target,
            VerificationTargetKind::ValidatorHandler | VerificationTargetKind::Equivalence
        ) {
            if let Some(ref vt) = test.validator_target {
                if let Some(ref handler_program) = vt.handler_program {
                    let handler_flat_rel = format!("flat/{id}_handler.flat");
                    write_file(&out.join(&handler_flat_rel), &handler_program.hex)?;
                }
            }
        }

        // Track import for root module
        module_dirs
            .entry(lean_module_segment.clone())
            .or_default()
            .push(lean_test_name.clone());

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

        manifest_entries.push(ManifestEntry {
            id: id.clone(),
            aiken_module: module.clone(),
            aiken_name: test_name.to_string(),
            lean_module,
            lean_theorem: lean_test_name,
            lean_file: lean_file_rel,
            flat_file: flat_file_rel,
            has_termination_theorem,
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

fn generate_lakefile(blaster_rev: &str) -> String {
    format!(
        r#"import Lake
open Lake DSL

package AikenVerify where

@[default_target]
lean_lib AikenVerify where

require Blaster from git
  "https://github.com/input-output-hk/Lean-blaster" @ "{blaster_rev}"

require PlutusCore from
  "/Users/rileykilgore/git/IOG/aiken-repos/AikenLeanTemplate/PlutusCore"
"#
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

/-- Decode the sampled value from `Fuzzer<a> = Prng -> Option (Prng, a)` output encoded as Data. -/
def decodeFuzzerSampleValue (d : Data) : Option Data :=
  match d with
  | Data.Constr tag fields =>
      if tag == 121 then
        match fields with
        | [Data.List [_nextPrng, value]] => some value
        | _ => none
      else
        none
  | _ => none

/-- Execute a fuzzer program at a given seed and decode the sampled output value (if any). -/
def sampleFuzzerValue (fuzzer : Program) (seed : Data) : Option Data :=
  match executeDataProgram fuzzer (dataArg seed) {cek_budget} with
  | some d => decodeFuzzerSampleValue d
  | none => none

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

/// Convert a test_id to a valid Lean identifier for the UPLC program binding.
fn prog_name(test_id: &str) -> String {
    format!("prog_{test_id}")
}

/// Variable names used for quantified tuple/pair elements: a, b, c, d, e, ...
const VAR_NAMES: &[&str] = &[
    "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p",
];

/// Map a FuzzerOutputType element to its Lean type name.
/// Returns None if the type cannot be mapped (truly unsupported).
fn lean_type_for(t: &FuzzerOutputType) -> Option<&'static str> {
    match t {
        FuzzerOutputType::Int => Some("Integer"),
        FuzzerOutputType::Bool => Some("Bool"),
        FuzzerOutputType::ByteArray | FuzzerOutputType::String => Some("ByteString"),
        FuzzerOutputType::Data => Some("Data"),
        // Unsupported concrete types fall back to Data
        FuzzerOutputType::Unsupported(_) => Some("Data"),
        // Compound types (List, Tuple, Pair) are not valid as tuple elements in this context
        _ => None,
    }
}

/// Map a FuzzerOutputType element to its Data encoder expression.
/// `var` is the variable name to encode.
fn lean_data_encoder(t: &FuzzerOutputType, var: &str) -> Option<String> {
    match t {
        FuzzerOutputType::Int => Some(format!("Data.I {var}")),
        FuzzerOutputType::Bool => Some(format!("(Data.Constr (if {var} then 1 else 0) [])")),
        FuzzerOutputType::ByteArray | FuzzerOutputType::String => Some(format!("Data.B {var}")),
        FuzzerOutputType::Data | FuzzerOutputType::Unsupported(_) => Some(var.to_string()),
        _ => None,
    }
}

/// Return a default Lean witness expression for a given scalar type.
/// Used in existential witness mode to provide a concrete value for `⟨witness, by decide⟩`.
fn lean_default_witness(t: &FuzzerOutputType) -> &'static str {
    match t {
        FuzzerOutputType::Int => "(0 : Integer)",
        FuzzerOutputType::Bool => "true",
        FuzzerOutputType::ByteArray | FuzzerOutputType::String => "ByteString.empty",
        FuzzerOutputType::Data | FuzzerOutputType::Unsupported(_) => "Data.I 0",
        _ => "Data.I 0",
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
        match t {
            FuzzerOutputType::Int if !has_integer => {
                opens.push("open PlutusCore.Integer (Integer)");
                has_integer = true;
            }
            FuzzerOutputType::ByteArray | FuzzerOutputType::String if !has_bytestring => {
                opens.push("open PlutusCore.ByteString (ByteString)");
                has_bytestring = true;
            }
            _ => {}
        }
    }

    opens.join("\n")
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

/// Check if a constraint provides an IntRange (possibly wrapped in Map).
fn constraint_has_int_range(constraint: &FuzzerConstraint) -> bool {
    match constraint {
        FuzzerConstraint::IntRange { .. } => true,
        FuzzerConstraint::Map(inner) => constraint_has_int_range(inner),
        _ => false,
    }
}

/// Check if a constraint contains a list domain (possibly nested in Map/And).
fn constraint_has_list_domain(constraint: &FuzzerConstraint) -> bool {
    match constraint {
        FuzzerConstraint::List { .. } => true,
        FuzzerConstraint::Map(inner) => constraint_has_list_domain(inner),
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
    match constraint {
        FuzzerConstraint::Tuple(elems) => {
            elems.len() == types.len()
                && elems.iter().zip(types.iter()).all(|(c, t)| {
                    if matches!(t, FuzzerOutputType::Int) {
                        constraint_has_int_range(c)
                    } else {
                        true // non-Int positions don't need IntRange
                    }
                })
        }
        FuzzerConstraint::Map(inner) => constraint_has_tuple_int_ranges_for_types(inner, types),
        // A shared IntRange covers all Int-typed positions uniformly.
        FuzzerConstraint::IntRange { .. } => true,
        _ => false,
    }
}

/// Extract optional List bounds and element constraint from a constraint,
/// looking through Map wrappers.
fn extract_list_bounds(
    constraint: &FuzzerConstraint,
) -> Option<(Option<usize>, Option<usize>, &FuzzerConstraint)> {
    match constraint {
        FuzzerConstraint::List {
            elem,
            min_len,
            max_len,
        } => Some((*min_len, *max_len, elem.as_ref())),
        FuzzerConstraint::Map(inner) => extract_list_bounds(inner),
        FuzzerConstraint::And(parts) => parts.iter().find_map(extract_list_bounds),
        _ => None,
    }
}

fn update_witness_min_len(current: &mut Option<usize>, candidate: Option<usize>) {
    if let Some(min_len) = candidate {
        *current = Some(current.map_or(min_len, |existing| existing.max(min_len)));
    }
}

fn collect_list_element_precondition_parts(
    test_name: &str,
    elem_type: &FuzzerOutputType,
    elem_constraint: &FuzzerConstraint,
    list_var: &str,
    out: &mut Vec<String>,
) -> miette::Result<()> {
    match elem_constraint {
        FuzzerConstraint::Any => Ok(()),
        FuzzerConstraint::IntRange { min, max } => {
            validate_int_bounds_literals(test_name, min, max)?;

            match elem_type {
                FuzzerOutputType::Int => {
                    out.push(format!(
                        "(({list_var}.all (fun x_i => {min} <= x_i && x_i <= {max})) = true)"
                    ));
                }
                // Data-encoded element domains (including unsupported ADTs encoded as Data).
                FuzzerOutputType::Data | FuzzerOutputType::Unsupported(_) => {
                    out.push(format!(
                        "(({list_var}.all (fun x_i => match x_i with | Data.I i => {min} <= i && i <= {max} | _ => false)) = true)"
                    ));
                }
                // If a mapper changed element type (e.g. Int -> Bool), the input-domain
                // IntRange does not constrain output elements directly; keep an
                // over-approximation by not emitting an element predicate.
                _ => {}
            }

            Ok(())
        }
        FuzzerConstraint::Map(inner) => {
            collect_list_element_precondition_parts(test_name, elem_type, inner, list_var, out)
        }
        FuzzerConstraint::And(parts) => {
            for part in parts {
                collect_list_element_precondition_parts(test_name, elem_type, part, list_var, out)?;
            }
            Ok(())
        }
        FuzzerConstraint::Unsupported { reason } => Err(miette::miette!(
            "Test '{}' contains unsupported list element-domain extraction: {}.\n\
             Constraint: {:?}",
            test_name,
            reason,
            elem_constraint,
        )),
        other => Err(miette::miette!(
            "Test '{}' has unsupported list element-domain constraint fragment {:?} for element type {:?}; \
             cannot translate to Lean precondition.\n\
             Constraint: {:?}",
            test_name,
            other,
            elem_type,
            elem_constraint,
        )),
    }
}

fn collect_list_domain_precondition_parts(
    test_name: &str,
    elem_type: &FuzzerOutputType,
    constraint: &FuzzerConstraint,
    list_var: &str,
    out: &mut Vec<String>,
    saw_list_domain: &mut bool,
    witness_min_len: &mut Option<usize>,
) -> miette::Result<()> {
    match constraint {
        FuzzerConstraint::List {
            elem,
            min_len,
            max_len,
        } => {
            *saw_list_domain = true;
            update_witness_min_len(witness_min_len, *min_len);

            match (min_len, max_len) {
                (Some(min), Some(max)) => {
                    out.push(format!("({min} <= {list_var}.length && {list_var}.length <= {max})"));
                }
                (Some(min), None) => out.push(format!("({min} <= {list_var}.length)")),
                (None, Some(max)) => out.push(format!("({list_var}.length <= {max})")),
                (None, None) => {}
            }

            collect_list_element_precondition_parts(test_name, elem_type, elem, list_var, out)?;
            Ok(())
        }
        FuzzerConstraint::Map(inner) => collect_list_domain_precondition_parts(
            test_name,
            elem_type,
            inner,
            list_var,
            out,
            saw_list_domain,
            witness_min_len,
        ),
        FuzzerConstraint::And(parts) => {
            for part in parts {
                collect_list_domain_precondition_parts(
                    test_name,
                    elem_type,
                    part,
                    list_var,
                    out,
                    saw_list_domain,
                    witness_min_len,
                )?;
            }
            Ok(())
        }
        FuzzerConstraint::Any => Ok(()),
        FuzzerConstraint::Unsupported { reason } => Err(miette::miette!(
            "Test '{}' contains unsupported list-domain extraction: {}.\n\
             Constraint: {:?}",
            test_name,
            reason,
            constraint,
        )),
        other => Err(miette::miette!(
            "Test '{}' has unsupported list-domain constraint fragment {:?}; \
             cannot translate to Lean precondition.\n\
             Constraint: {:?}",
            test_name,
            other,
            constraint,
        )),
    }
}

fn build_list_domain_preconditions(
    test_name: &str,
    elem_type: &FuzzerOutputType,
    constraint: &FuzzerConstraint,
    list_var: &str,
) -> miette::Result<(Vec<String>, Option<usize>)> {
    let mut precondition_parts = Vec::new();
    let mut saw_list_domain = false;
    let mut witness_min_len = None;

    collect_list_domain_precondition_parts(
        test_name,
        elem_type,
        constraint,
        list_var,
        &mut precondition_parts,
        &mut saw_list_domain,
        &mut witness_min_len,
    )?;

    if !saw_list_domain {
        return Err(miette::miette!(
            "Test '{}' uses List fuzzer but has no extractable list-domain constraints; \
             List proofs require a constraint derived from list/list_between.\n\
             Constraint: {:?}",
            test_name,
            constraint,
        ));
    }

    Ok((precondition_parts, witness_min_len))
}

fn validate_int_bounds_literals(test_name: &str, min: &str, max: &str) -> miette::Result<()> {
    if !is_valid_integer_literal(min) {
        return Err(miette::miette!(
            "Test '{}' has invalid min bound '{}'; expected an integer literal",
            test_name,
            min,
        ));
    }
    if !is_valid_integer_literal(max) {
        return Err(miette::miette!(
            "Test '{}' has invalid max bound '{}'; expected an integer literal",
            test_name,
            max,
        ));
    }

    Ok(())
}

/// Extract and validate integer bounds from the test's constraint.
fn extract_int_bounds(test: &ExportedPropertyTest) -> miette::Result<(String, String)> {
    let (min, max) = match extract_int_range_from_constraint(&test.constraint) {
        Some((min, max)) => (min, max),
        None => {
            return Err(miette::miette!(
                "Test '{}' has no extractable Int range constraint; \
                 cannot generate a bounded theorem without explicit Int bounds.\n\
                 Constraint: {:?}",
                test.name,
                test.constraint,
            ));
        }
    };

    validate_int_bounds_literals(&test.name, &min, &max)?;

    Ok((min, max))
}

/// Extract per-element Int bounds for a tuple/pair.
/// Returns a Vec of Option<(min, max)> for each element -- None for non-Int elements.
fn extract_tuple_element_bounds(
    test: &ExportedPropertyTest,
    types: &[FuzzerOutputType],
) -> miette::Result<Vec<Option<(String, String)>>> {
    // Unwrap Map wrappers to reach the inner tuple/range constraint.
    let unwrapped = unwrap_map_constraint(&test.constraint);

    let constraint_elems: Vec<&FuzzerConstraint> = match unwrapped {
        FuzzerConstraint::Tuple(elems) if elems.len() == types.len() => elems.iter().collect(),
        // Backward compatibility: a single IntRange shared for all Int components.
        FuzzerConstraint::IntRange { .. } => {
            vec![unwrapped; types.len()]
        }
        _ if types.iter().any(|t| matches!(t, FuzzerOutputType::Int)) => {
            return Err(miette::miette!(
                "Test '{}' has no extractable tuple Int range constraint; \
                 cannot generate a bounded theorem.\n\
                 Constraint: {:?}",
                test.name,
                test.constraint,
            ));
        }
        _ => {
            // No Int elements, no bounds needed
            return Ok(vec![None; types.len()]);
        }
    };

    let mut bounds = Vec::with_capacity(types.len());
    for (i, t) in types.iter().enumerate() {
        if matches!(t, FuzzerOutputType::Int) {
            let (min, max) =
                extract_int_range_from_constraint(constraint_elems[i]).ok_or_else(|| {
                    miette::miette!(
                        "Test '{}': tuple element {} has no extractable Int range",
                        test.name,
                        i,
                    )
                })?;
            validate_int_bounds_literals(&test.name, &min, &max)?;
            bounds.push(Some((min, max)));
        } else {
            bounds.push(None);
        }
    }
    Ok(bounds)
}

/// Recursively unwrap Map wrappers to reach the inner constraint.
fn unwrap_map_constraint(constraint: &FuzzerConstraint) -> &FuzzerConstraint {
    match constraint {
        FuzzerConstraint::Map(inner) => unwrap_map_constraint(inner),
        other => other,
    }
}

/// Recursively extract an IntRange from a constraint, looking through Map wrappers.
fn extract_int_range_from_constraint(constraint: &FuzzerConstraint) -> Option<(String, String)> {
    match constraint {
        FuzzerConstraint::IntRange { min, max } => Some((min.clone(), max.clone())),
        FuzzerConstraint::Map(inner) => extract_int_range_from_constraint(inner),
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
/// `witness_value`: for existential witness mode, the concrete witness expression
/// (e.g. "0" for Int, "true" for Bool). None for universal/proof modes.
fn format_theorems(
    form: &TheoremForm,
    lean_test_name: &str,
    prog: &str,
    arg_expr: &str,
    quantifiers: &str,
    preconditions: &str,
    witness_value: Option<&str>,
) -> String {
    let correctness_body = form.correctness.format(prog, arg_expr);
    let correctness_tactic = form.correctness.tactic();

    let mut out = if form.existential {
        // Existential theorems: replace ∀ with ∃, use ∧ instead of → for preconditions
        let existential_quantifiers = quantifiers.replacen('∀', "∃", 1);
        // Also replace internal → between multi-element bounds with ∧
        let preconditions = preconditions.replace("→", "∧");
        let preconditions = preconditions.as_str();
        let connector = if preconditions.is_empty() {
            ""
        } else {
            "\n  ∧\n  "
        };
        let mode_comment = match form.existential_mode {
            Some(ExistentialMode::Witness) => "-- Mode: witness (deterministic witness search)\n",
            Some(ExistentialMode::Proof) => "-- Mode: proof (existential proof attempt)\n",
            None => "",
        };

        let proof_term = if let Some(witness) = witness_value {
            // Witness mode: provide concrete witness + decide for the conjunction
            format!("⟨{witness}, by {correctness_tactic}⟩")
        } else {
            format!("by {correctness_tactic}")
        };

        format!(
            "{mode_comment}theorem {lean_test_name} :\n  {existential_quantifiers}{preconditions}{connector}{correctness_body} :=\n  {proof_term}\n"
        )
    } else {
        let arrow = if preconditions.is_empty() {
            ""
        } else {
            "\n  →\n  "
        };
        format!(
            "theorem {lean_test_name} :\n  {quantifiers}{preconditions}{arrow}{correctness_body} :=\n  by {correctness_tactic}\n"
        )
    };

    if let Some(ref term_body) = form.termination {
        let termination_body = term_body.format(prog, arg_expr);
        let termination_tactic = term_body.tactic();
        let arrow = if preconditions.is_empty() {
            ""
        } else {
            "\n  →\n  "
        };
        out.push_str(&format!(
            "\ntheorem {lean_test_name}_alwaysTerminating :\n  {quantifiers}{preconditions}{arrow}{termination_body} :=\n  by {termination_tactic}\n"
        ));
    }

    out
}

/// Generate an equivalence theorem proving that the property wrapper and
/// validator handler produce the same result for all inputs.
fn format_equivalence_theorem(
    lean_test_name: &str,
    prop_prog: &str,
    handler_prog: &str,
    arg_expr: &str,
    quantifiers: &str,
    preconditions: &str,
) -> String {
    let arrow = if preconditions.is_empty() {
        ""
    } else {
        "\n  →\n  "
    };
    format!(
        "\ntheorem {lean_test_name}_equivalence :\n  \
         {quantifiers}{preconditions}{arrow}\
         proveTests {prop_prog} ({arg_expr}) = proveTests {handler_prog} ({arg_expr}) :=\n  \
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

fn generate_proof_file(
    test: &ExportedPropertyTest,
    test_id: &str,
    lean_test_name: &str,
    lean_module: &str,
    existential_mode: ExistentialMode,
    target: &VerificationTargetKind,
) -> miette::Result<String> {
    // Validator/equivalence modes require a validator_target with handler_program
    match target {
        VerificationTargetKind::ValidatorHandler | VerificationTargetKind::Equivalence => {
            if test.validator_target.is_none() {
                return Err(miette::miette!(
                    "Test '{}' has no validator target metadata; \
                     --target {} requires a validator_target with handler_program. \
                     This test can only be verified in --target property mode.",
                    test.name,
                    target,
                ));
            }
            let vt = test.validator_target.as_ref().unwrap();
            if vt.handler_program.is_none() {
                return Err(miette::miette!(
                    "Test '{}' has validator target metadata but no handler program; \
                     --target {} requires an exported handler program.",
                    test.name,
                    target,
                ));
            }
        }
        VerificationTargetKind::PropertyWrapper => {}
    }

    let form = determine_theorem_form(test, existential_mode)?;

    let prog = prog_name(test_id);
    let fuzzer_prog = format!("fuzzer_{}", prog_name(test_id));
    let handler_prog = format!("handler_{}", prog_name(test_id));

    // Common header for all proof files
    let header = |extra_opens: &str| -> String {
        let mut s = format!(
            "import AikenVerify.Utils\nimport PlutusCore.UPLC.ScriptEncoding\nimport Blaster\n\n\
             namespace {lean_module}\n\
             open PlutusCore.UPLC.CekMachine\n\
             open PlutusCore.UPLC.Term (Term Const Program)\n\
             open AikenVerify.Utils\n"
        );
        if !extra_opens.is_empty() {
            s.insert_str(
                s.find("open PlutusCore.UPLC.CekMachine").unwrap(),
                &format!("{extra_opens}\n"),
            );
        }
        s.push_str(&format!(
            "\n#import_uplc {prog} single_cbor_hex \"./flat/{test_id}.flat\"\n"
        ));
        s.push_str(&format!(
            "#import_uplc {fuzzer_prog} single_cbor_hex \"./flat/{test_id}_fuzzer.flat\"\n"
        ));
        // For validator/equivalence modes, also import the handler program
        match target {
            VerificationTargetKind::ValidatorHandler | VerificationTargetKind::Equivalence => {
                s.push_str(&format!(
                    "#import_uplc {handler_prog} single_cbor_hex \"./flat/{test_id}_handler.flat\"\n"
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

    match &test.fuzzer_output_type {
        // --- Scalar types ---
        FuzzerOutputType::Int => {
            let (min, max) = extract_int_bounds(test)?;
            let quantifiers = "∀ (x : Integer),";
            let preconditions = format!("\n  ({min} <= x && x <= {max})");
            let witness = if form.existential_mode == Some(ExistentialMode::Witness) {
                Some(format!("({min} : Integer)"))
            } else {
                None
            };
            let theorems = format_theorems(
                &form,
                lean_test_name,
                &verify_prog,
                "intArg x",
                quantifiers,
                &preconditions,
                witness.as_deref(),
            );
            let mut content = format!("{}{theorems}", header("open PlutusCore.Integer (Integer)"));
            if let VerificationTargetKind::Equivalence = target {
                content.push_str(&format_equivalence_theorem(
                    lean_test_name,
                    &prog,
                    &handler_prog,
                    "intArg x",
                    quantifiers,
                    &preconditions,
                ));
            }
            content.push_str(&footer);
            Ok(content)
        }

        FuzzerOutputType::Bool => {
            let quantifiers = "∀ (x : Bool),";
            let witness = if form.existential_mode == Some(ExistentialMode::Witness) {
                Some("true")
            } else {
                None
            };
            let theorems = format_theorems(
                &form,
                lean_test_name,
                &verify_prog,
                "boolArg x",
                quantifiers,
                "",
                witness,
            );
            let mut content = format!("{}{theorems}", header(""));
            if let VerificationTargetKind::Equivalence = target {
                content.push_str(&format_equivalence_theorem(
                    lean_test_name,
                    &prog,
                    &handler_prog,
                    "boolArg x",
                    quantifiers,
                    "",
                ));
            }
            content.push_str(&footer);
            Ok(content)
        }

        FuzzerOutputType::ByteArray => {
            let quantifiers = "∀ (x : ByteString),";
            let witness = if form.existential_mode == Some(ExistentialMode::Witness) {
                Some("ByteString.empty")
            } else {
                None
            };
            let theorems = format_theorems(
                &form,
                lean_test_name,
                &verify_prog,
                "bytearrayArg x",
                quantifiers,
                "",
                witness,
            );
            let mut content = format!(
                "{}{theorems}",
                header("open PlutusCore.ByteString (ByteString)")
            );
            if let VerificationTargetKind::Equivalence = target {
                content.push_str(&format_equivalence_theorem(
                    lean_test_name,
                    &prog,
                    &handler_prog,
                    "bytearrayArg x",
                    quantifiers,
                    "",
                ));
            }
            content.push_str(&footer);
            Ok(content)
        }

        FuzzerOutputType::String => {
            let quantifiers = "∀ (x : ByteString),";
            let witness = if form.existential_mode == Some(ExistentialMode::Witness) {
                Some("ByteString.empty")
            } else {
                None
            };
            let theorems = format_theorems(
                &form,
                lean_test_name,
                &verify_prog,
                "stringArg x",
                quantifiers,
                "",
                witness,
            );
            let mut content = format!(
                "{}{theorems}",
                header("open PlutusCore.ByteString (ByteString)")
            );
            if let VerificationTargetKind::Equivalence = target {
                content.push_str(&format_equivalence_theorem(
                    lean_test_name,
                    &prog,
                    &handler_prog,
                    "stringArg x",
                    quantifiers,
                    "",
                ));
            }
            content.push_str(&footer);
            Ok(content)
        }

        FuzzerOutputType::Data => {
            let quantifiers = "∀ (x : Data),";
            let witness = if form.existential_mode == Some(ExistentialMode::Witness) {
                Some("Data.I 0")
            } else {
                None
            };
            let theorems = format_theorems(
                &form,
                lean_test_name,
                &verify_prog,
                "dataArg x",
                quantifiers,
                "",
                witness,
            );
            let mut content = format!("{}{theorems}", header("open PlutusCore.Data (Data)"));
            if let VerificationTargetKind::Equivalence = target {
                content.push_str(&format_equivalence_theorem(
                    lean_test_name,
                    &prog,
                    &handler_prog,
                    "dataArg x",
                    quantifiers,
                    "",
                ));
            }
            content.push_str(&footer);
            Ok(content)
        }

        // --- Pair(a, b) handled like a 2-tuple ---
        FuzzerOutputType::Pair(fst, snd) => {
            // Validate element types are supported scalars (same check as Tuple branch)
            for (i, t) in [fst.as_ref(), snd.as_ref()].iter().enumerate() {
                if lean_type_for(t).is_none() {
                    return Err(miette::miette!(
                        "Test '{}' has unsupported pair element type at index {}: {:?}",
                        test.name,
                        i,
                        t,
                    ));
                }
            }
            generate_tuple_proof(
                test,
                &[fst.as_ref(), snd.as_ref()],
                &form,
                lean_test_name,
                &verify_prog,
                &header,
                &footer,
                target,
                &prog,
                &handler_prog,
            )
        }

        // --- Generic tuple support (arities >= 2) ---
        FuzzerOutputType::Tuple(types) if types.len() >= 2 => {
            let type_refs: Vec<&FuzzerOutputType> = types.iter().collect();
            // Verify all element types are supported scalars
            for (i, t) in types.iter().enumerate() {
                if lean_type_for(t).is_none() {
                    return Err(miette::miette!(
                        "Test '{}' has unsupported tuple element type at index {}: {:?}",
                        test.name,
                        i,
                        t,
                    ));
                }
            }
            generate_tuple_proof(
                test,
                &type_refs,
                &form,
                lean_test_name,
                &verify_prog,
                &header,
                &footer,
                target,
                &prog,
                &handler_prog,
            )
        }

        // --- List with optional bounds ---
        FuzzerOutputType::List(elem_type) => {
            let (precondition_parts, witness_min_len) = build_list_domain_preconditions(
                &test.name,
                elem_type.as_ref(),
                &test.constraint,
                "xs",
            )?;

            // When list-domain extraction yields no usable predicate for Data-encoded
            // elements, fall back to a fuzzer-domain theorem that reasons directly on
            // the sampled value for each seed:
            //
            //   ∀ seed, match sampleFuzzerValue fuzzer seed with
            //           | some x => property x
            //           | none => True
            //
            // This is sound (domain is exactly the exported fuzzer image), avoids an
            // unconstrained universal theorem over `List Data`, and is far more
            // tractable for blaster than an implication with a separate `x` quantifier.
            if precondition_parts.is_empty()
                && matches!(
                    elem_type.as_ref(),
                    FuzzerOutputType::Data | FuzzerOutputType::Unsupported(_)
                )
            {
                if form.existential {
                    return Err(miette::miette!(
                        "Test '{}' requires fuzzer-domain fallback for unconstrained Data list, \
                         but existential mode is not supported for this fallback theorem shape.\n\
                         Constraint: {:?}",
                        test.name,
                        test.constraint,
                    ));
                }

                let sample_expr = format!("sampleFuzzerValue {fuzzer_prog} seed");
                let correctness_body = form.correctness.format(&verify_prog, "dataArg x");
                let correctness_tactic = form.correctness.tactic();
                let mut theorems = format!(
                    "theorem {lean_test_name} :\n  \
                     ∀ (seed : Data),\n  \
                     match {sample_expr} with\n  \
                     | some x => {correctness_body}\n  \
                     | none => True :=\n  \
                     by {correctness_tactic}\n"
                );

                if let Some(ref term_body) = form.termination {
                    let termination_body = term_body.format(&verify_prog, "dataArg x");
                    let termination_tactic = term_body.tactic();
                    theorems.push_str(&format!(
                        "\ntheorem {lean_test_name}_alwaysTerminating :\n  \
                         ∀ (seed : Data),\n  \
                         match {sample_expr} with\n  \
                         | some x => {termination_body}\n  \
                         | none => True :=\n  \
                         by {termination_tactic}\n"
                    ));
                }

                let mut content = format!("{}{theorems}", header("open PlutusCore.Data (Data)"));
                if let VerificationTargetKind::Equivalence = target {
                    content.push_str(&format!(
                        "\ntheorem {lean_test_name}_equivalence :\n  \
                         ∀ (seed : Data),\n  \
                         match {sample_expr} with\n  \
                         | some x => proveTests {prog} (dataArg x) = proveTests {handler_prog} (dataArg x)\n  \
                         | none => True :=\n  \
                         by blaster\n"
                    ));
                }
                content.push_str(&footer);
                return Ok(content);
            }

            // Keep a representative element constraint for witness construction in
            // existential mode.
            let (_, _, elem_constraint) = extract_list_bounds(&test.constraint).ok_or_else(|| {
                miette::miette!(
                    "Test '{}' uses List fuzzer but has no extractable list-domain constraints; \
                     List proofs require a constraint derived from list/list_between.\n\
                     Constraint: {:?}",
                    test.name,
                    test.constraint,
                )
            })?;

            let elem_lean_type = lean_type_for(elem_type).ok_or_else(|| {
                miette::miette!(
                    "Test '{}' has unsupported list element type: {:?}",
                    test.name,
                    elem_type,
                )
            })?;
            let elem_encoder = lean_data_encoder(elem_type, "x_i").ok_or_else(|| {
                miette::miette!(
                    "Test '{}' has unsupported list element type for encoding: {:?}",
                    test.name,
                    elem_type,
                )
            })?;

            let opens = lean_opens_for_types(&[elem_type.as_ref()]);
            let quantifiers = format!("∀ (xs : List {elem_lean_type}),");

            // Build preconditions from extracted list-domain constraints.
            let elem_bounds = if matches!(elem_type.as_ref(), FuzzerOutputType::Int) {
                extract_int_range_from_constraint(elem_constraint)
            } else {
                None
            };
            let preconditions = if precondition_parts.is_empty() {
                String::new()
            } else {
                format!("\n  {}", precondition_parts.join("\n  →\n  "))
            };
            // Build the arg expression: inline list encoding as Data.List (xs.map ...)
            let arg_expr = format!(
                "[Term.Const (Const.Data (Data.List (xs.map (fun x_i => {elem_encoder}))))]"
            );
            let witness = if form.existential_mode == Some(ExistentialMode::Witness) {
                // Generate a list with min_len elements using a witness that satisfies
                // element bounds (if present) or the generic default.
                let elem_witness: String = if let Some((emin, _)) = elem_bounds.as_ref() {
                    format!("({emin} : {elem_lean_type})")
                } else {
                    lean_default_witness(elem_type).to_string()
                };
                let witness_len = witness_min_len.unwrap_or(0);
                let elems: Vec<&str> = (0..witness_len).map(|_| elem_witness.as_str()).collect();
                Some(format!("[{}]", elems.join(", ")))
            } else {
                None
            };
            let theorems = format_theorems(
                &form,
                lean_test_name,
                &verify_prog,
                &arg_expr,
                &quantifiers,
                &preconditions,
                witness.as_deref(),
            );
            let mut content = format!("{}{theorems}", header(&opens));
            if let VerificationTargetKind::Equivalence = target {
                content.push_str(&format_equivalence_theorem(
                    lean_test_name,
                    &prog,
                    &handler_prog,
                    &arg_expr,
                    &quantifiers,
                    &preconditions,
                ));
            }
            content.push_str(&footer);
            Ok(content)
        }

        // --- ADT fallback via Data encoding ---
        FuzzerOutputType::Unsupported(_) => {
            let quantifiers = "∀ (x : Data),";
            let witness = if form.existential_mode == Some(ExistentialMode::Witness) {
                Some("Data.I 0")
            } else {
                None
            };
            let theorems = format_theorems(
                &form,
                lean_test_name,
                &verify_prog,
                "dataArg x",
                quantifiers,
                "",
                witness,
            );
            let mut content = format!("{}{theorems}", header("open PlutusCore.Data (Data)"));
            if let VerificationTargetKind::Equivalence = target {
                content.push_str(&format_equivalence_theorem(
                    lean_test_name,
                    &prog,
                    &handler_prog,
                    "dataArg x",
                    quantifiers,
                    "",
                ));
            }
            content.push_str(&footer);
            Ok(content)
        }

        other => Err(miette::miette!(
            "Test '{}' has unsupported fuzzer output type {:?}; \
             cannot generate a proof for this shape",
            test.name,
            other,
        )),
    }
}

/// Generate a proof file for a tuple/pair with the given element types.
fn generate_tuple_proof(
    test: &ExportedPropertyTest,
    types: &[&FuzzerOutputType],
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
    if arity > VAR_NAMES.len() {
        return Err(miette::miette!(
            "Test '{}' has tuple arity {} which exceeds maximum supported arity {}",
            test.name,
            arity,
            VAR_NAMES.len(),
        ));
    }

    // Build per-element bounds
    let owned_types: Vec<FuzzerOutputType> = types.iter().map(|t| (*t).clone()).collect();
    let bounds = extract_tuple_element_bounds(test, &owned_types)?;

    // Build quantifiers: ∀ (a : Integer) (b : Data),
    let quantifier_parts: Vec<String> = types
        .iter()
        .enumerate()
        .map(|(i, t)| {
            let var = VAR_NAMES[i];
            let lean_type = lean_type_for(t).unwrap(); // already validated
            format!("({var} : {lean_type})")
        })
        .collect();
    let quantifiers = format!("∀ {},", quantifier_parts.join(" "));

    // Build preconditions from Int bounds
    let mut precond_parts = Vec::new();
    for (i, bound) in bounds.iter().enumerate() {
        if let Some((min, max)) = bound {
            let var = VAR_NAMES[i];
            precond_parts.push(format!("({min} <= {var} && {var} <= {max})"));
        }
    }
    let preconditions = if precond_parts.is_empty() {
        String::new()
    } else {
        format!("\n  {}", precond_parts.join("\n  →\n  "))
    };

    // Build the arg expression: inline tuple encoding as Data.List [encoder(a), encoder(b), ...]
    let encoded_parts: Vec<String> = types
        .iter()
        .enumerate()
        .map(|(i, t)| lean_data_encoder(t, VAR_NAMES[i]).unwrap())
        .collect();
    let arg_expr = format!(
        "[Term.Const (Const.Data (Data.List [{}]))]",
        encoded_parts.join(", ")
    );

    let opens = lean_opens_for_types(types);
    let witness = if form.existential_mode == Some(ExistentialMode::Witness) {
        // Build a tuple witness from element defaults/bounds
        let witness_parts: Vec<String> = types
            .iter()
            .enumerate()
            .map(|(i, t)| {
                if let Some((ref min, _)) = bounds[i] {
                    format!("({min} : Integer)")
                } else {
                    lean_default_witness(t).to_string()
                }
            })
            .collect();
        Some(witness_parts.join(", "))
    } else {
        None
    };
    let theorems = format_theorems(
        form,
        lean_test_name,
        prog,
        &arg_expr,
        &quantifiers,
        &preconditions,
        witness.as_deref(),
    );
    let mut content = format!("{}{theorems}", header(&opens));
    if let VerificationTargetKind::Equivalence = target {
        content.push_str(&format_equivalence_theorem(
            lean_test_name,
            prop_prog,
            handler_prog,
            &arg_expr,
            &quantifiers,
            &preconditions,
        ));
    }
    content.push_str(footer);
    Ok(content)
}

/// Parse lake build output into per-theorem results.
/// Uses the manifest to know which theorems were expected.
pub fn parse_verify_results(raw: VerifyResult, manifest: &GeneratedManifest) -> VerifySummary {
    if let Some(mut theorems) = raw.theorem_results.clone() {
        let proved = theorems
            .iter()
            .filter(|t| matches!(t.status, ProofStatus::Proved))
            .count();
        let failed = theorems
            .iter()
            .filter(|t| matches!(t.status, ProofStatus::Failed { .. }))
            .count();
        let timed_out = theorems
            .iter()
            .filter(|t| matches!(t.status, ProofStatus::TimedOut { .. }))
            .count();
        let unknown = theorems
            .iter()
            .filter(|t| matches!(t.status, ProofStatus::Unknown))
            .count();

        theorems.sort_by(|a, b| {
            a.test_name
                .cmp(&b.test_name)
                .then_with(|| a.theorem_name.cmp(&b.theorem_name))
        });

        return VerifySummary {
            total: theorems.len(),
            proved,
            failed,
            timed_out,
            unknown,
            theorems,
            raw_output: raw,
            elapsed_ms: None,
        };
    }

    let mut theorems = Vec::new();
    let combined_output = format!("{}\n{}", raw.stdout, raw.stderr);

    if raw.success {
        for entry in &manifest.tests {
            theorems.push(TheoremResult {
                test_name: format!("{}.{}", entry.aiken_module, entry.aiken_name),
                theorem_name: entry.lean_theorem.clone(),
                status: ProofStatus::Proved,
            });
            if entry.has_termination_theorem {
                theorems.push(TheoremResult {
                    test_name: format!("{}.{}", entry.aiken_module, entry.aiken_name),
                    theorem_name: format!("{}_alwaysTerminating", entry.lean_theorem),
                    status: ProofStatus::Proved,
                });
            }
        }
    } else {
        let timed_out_run = combined_output.contains("[verify-timeout]");
        let failed_modules = collect_failed_proof_modules(&combined_output);
        let succeeded_modules = collect_succeeded_proof_modules(&combined_output);

        for entry in &manifest.tests {
            let module_failed = failed_modules.contains(&entry.lean_module);
            let module_succeeded = succeeded_modules.contains(&entry.lean_module);
            let theorem_failed =
                theorem_has_explicit_failure(&entry.lean_theorem, &combined_output);

            let correctness_status = if timed_out_run {
                ProofStatus::TimedOut {
                    reason: "Proof execution timed out".to_string(),
                }
            } else if theorem_failed || module_failed {
                let reason = if theorem_failed {
                    extract_error_for_theorem(&entry.lean_theorem, &combined_output)
                } else {
                    extract_error_for_module(&entry.lean_module, &combined_output)
                };
                ProofStatus::Failed {
                    category: classify_failure(&combined_output),
                    reason,
                }
            } else if module_succeeded {
                ProofStatus::Proved
            } else {
                ProofStatus::Unknown
            };

            theorems.push(TheoremResult {
                test_name: format!("{}.{}", entry.aiken_module, entry.aiken_name),
                theorem_name: entry.lean_theorem.clone(),
                status: correctness_status,
            });

            if entry.has_termination_theorem {
                let term_name = format!("{}_alwaysTerminating", entry.lean_theorem);
                let term_failed = theorem_has_explicit_failure(&term_name, &combined_output);
                let term_status = if timed_out_run {
                    ProofStatus::TimedOut {
                        reason: "Proof execution timed out".to_string(),
                    }
                } else if term_failed || module_failed {
                    let reason = if term_failed {
                        extract_error_for_theorem(&term_name, &combined_output)
                    } else {
                        extract_error_for_module(&entry.lean_module, &combined_output)
                    };
                    ProofStatus::Failed {
                        category: classify_failure(&combined_output),
                        reason,
                    }
                } else if module_succeeded {
                    ProofStatus::Proved
                } else {
                    ProofStatus::Unknown
                };

                theorems.push(TheoremResult {
                    test_name: format!("{}.{}", entry.aiken_module, entry.aiken_name),
                    theorem_name: term_name,
                    status: term_status,
                });
            }
        }
    }

    // If the run failed and no theorem received a concrete status signal
    // (proved/failed/timed out), classify the run-level failure for all theorems.
    // This avoids surfacing opaque UNKNOWN statuses for generic lake failures.
    if !raw.success
        && theorems
            .iter()
            .all(|t| matches!(t.status, ProofStatus::Unknown))
    {
        let category = classify_failure(&combined_output);
        if category != FailureCategory::Unknown {
            let reason = extract_global_failure_reason(&combined_output);
            for theorem in &mut theorems {
                theorem.status = ProofStatus::Failed {
                    category: category.clone(),
                    reason: reason.clone(),
                };
            }
        }
    }

    let proved = theorems
        .iter()
        .filter(|t| matches!(t.status, ProofStatus::Proved))
        .count();
    let failed = theorems
        .iter()
        .filter(|t| matches!(t.status, ProofStatus::Failed { .. }))
        .count();
    let timed_out = theorems
        .iter()
        .filter(|t| matches!(t.status, ProofStatus::TimedOut { .. }))
        .count();
    let unknown = theorems
        .iter()
        .filter(|t| matches!(t.status, ProofStatus::Unknown))
        .count();

    // Sort for deterministic output (important for CI diffs).
    theorems.sort_by(|a, b| {
        a.test_name
            .cmp(&b.test_name)
            .then_with(|| a.theorem_name.cmp(&b.theorem_name))
    });

    VerifySummary {
        total: theorems.len(),
        proved,
        failed,
        timed_out,
        unknown,
        theorems,
        raw_output: raw,
        elapsed_ms: None,
    }
}

fn parse_proof_module_token(token: &str) -> Option<String> {
    let cleaned = token
        .split_whitespace()
        .next()
        .unwrap_or(token)
        .split(':')
        .next()
        .unwrap_or(token)
        .trim_end_matches(|c: char| c == ',' || c == ';' || c == '.')
        .trim();

    if cleaned.starts_with("AikenVerify.Proofs.") {
        Some(cleaned.to_string())
    } else {
        None
    }
}

fn collect_succeeded_proof_modules(output: &str) -> HashSet<String> {
    let mut modules = HashSet::new();

    for line in output.lines() {
        if let Some(idx) = line.find("Built ") {
            let token = &line[idx + "Built ".len()..];
            if let Some(module) = parse_proof_module_token(token) {
                modules.insert(module);
            }
        }

        if let Some(idx) = line.find("Replayed ") {
            let token = &line[idx + "Replayed ".len()..];
            if let Some(module) = parse_proof_module_token(token) {
                modules.insert(module);
            }
        }
    }

    modules
}

fn collect_failed_proof_modules(output: &str) -> HashSet<String> {
    let mut modules = HashSet::new();

    for line in output.lines() {
        if line.contains('✖') {
            if let Some(idx) = line.find("Building ") {
                let token = &line[idx + "Building ".len()..];
                if let Some(module) = parse_proof_module_token(token) {
                    modules.insert(module);
                }
            }
        }

        if let Some(token) = line.trim().strip_prefix("- ") {
            if let Some(module) = parse_proof_module_token(token) {
                modules.insert(module);
            }
        }
    }

    modules
}

fn theorem_has_explicit_failure(theorem: &str, output: &str) -> bool {
    output.contains(&format!("error: '{}' ", theorem))
        || output.lines().any(|line| {
            line.contains("error:")
                && (line.contains(theorem) || line.contains(&format!("'{}'", theorem)))
        })
}

/// Classify a failure based on build output content.
fn classify_failure(output: &str) -> FailureCategory {
    if output.contains("❌ Falsified") || output.contains("Counterexample:") {
        FailureCategory::Counterexample
    } else if output.contains("Tactic `blaster` failed")
        || output.contains("unsolved goals")
        || output.contains("tactic 'blaster' failed")
    {
        FailureCategory::UnsatGoal
    } else if output.contains("deterministic timeout") || output.contains("tactic timed out") {
        FailureCategory::Timeout
    } else if output.contains("unknown package")
        || output.contains("could not resolve")
        || output.contains("lake fetch")
    {
        FailureCategory::DependencyError
    } else if output.contains("translateApp: Inductive predicate not yet supported")
        || (output.contains("translateApp:") && output.contains("not yet supported"))
        || output.contains("Lean.Expr.const `List.Mem")
    {
        FailureCategory::BlasterUnsupported
    } else if output.contains("error:") {
        FailureCategory::BuildError
    } else {
        FailureCategory::Unknown
    }
}

/// Extract error context for a theorem from build output.
/// Includes the matching line plus surrounding context lines (up to `CONTEXT_LINES`
/// before and after each match) for more useful diagnostics.
fn extract_error_for_pattern(pattern: &str, output: &str) -> String {
    const CONTEXT_LINES: usize = 3;
    let lines: Vec<&str> = output.lines().collect();
    let mut included = vec![false; lines.len()];

    for (i, line) in lines.iter().enumerate() {
        if line.contains(pattern) || line.trim_start().starts_with("error:") {
            let start = i.saturating_sub(CONTEXT_LINES);
            let end = (i + CONTEXT_LINES + 1).min(lines.len());
            for included_flag in included.iter_mut().take(end).skip(start) {
                *included_flag = true;
            }
        }
    }

    let mut result = Vec::new();
    let mut last_included = false;
    for (i, line) in lines.iter().enumerate() {
        if included[i] {
            if !last_included && !result.is_empty() {
                result.push("  ...");
            }
            result.push(*line);
            last_included = true;
        } else {
            last_included = false;
        }
    }

    result.join("\n")
}

fn extract_error_for_module(module: &str, output: &str) -> String {
    extract_error_for_pattern(module, output)
}

fn extract_error_for_theorem(theorem: &str, output: &str) -> String {
    extract_error_for_pattern(theorem, output)
}

fn extract_global_failure_reason(output: &str) -> String {
    let extracted = extract_error_for_pattern("error:", output);
    if !extracted.trim().is_empty() {
        return extracted;
    }

    output
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .take(20)
        .collect::<Vec<_>>()
        .join("\n")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::export::{
        ExportedProgram, ExportedPropertyTest, FuzzerConstraint, FuzzerOutputType, TestReturnMode,
        ValidatorTarget,
    };
    use aiken_lang::ast::OnTestFailure;

    fn make_test(module: &str, name: &str) -> ExportedPropertyTest {
        make_test_with_failure(module, name, OnTestFailure::FailImmediately)
    }

    fn make_test_with_failure(
        module: &str,
        name: &str,
        on_test_failure: OnTestFailure,
    ) -> ExportedPropertyTest {
        ExportedPropertyTest {
            name: format!("{module}.{name}"),
            module: module.to_string(),
            input_path: format!("lib/{}.ak", module.replace('/', "/")),
            on_test_failure,
            return_mode: TestReturnMode::Bool,
            target_kind: Default::default(),
            validator_target: None,
            test_program: ExportedProgram {
                hex: "deadbeef".to_string(),
                flat_bytes: None,
            },
            fuzzer_program: ExportedProgram {
                hex: "cafebabe".to_string(),
                flat_bytes: None,
            },
            fuzzer_type: "Fuzzer<Int>".to_string(),
            fuzzer_output_type: FuzzerOutputType::Int,
            constraint: FuzzerConstraint::IntRange {
                min: "0".to_string(),
                max: "255".to_string(),
            },
        }
    }

    #[test]
    fn sanitize_lean_name_basic() {
        assert_eq!(sanitize_lean_name("test_map"), "test_map");
        assert_eq!(sanitize_lean_name("my-test"), "my_test");
        assert_eq!(sanitize_lean_name("3things"), "T_3things");
    }

    #[test]
    fn module_to_lean_segment_basic() {
        assert_eq!(module_to_lean_segment("aiken/list"), "AikenList");
        assert_eq!(module_to_lean_segment("my_module"), "My_module");
        assert_eq!(
            module_to_lean_segment("permissions.test"),
            "PermissionsTest"
        );
        assert_eq!(
            module_to_lean_segment("deep/nested/module"),
            "DeepNestedModule"
        );
    }

    #[test]
    fn test_id_has_readable_prefix_and_hash_suffix() {
        let id = test_id("aiken/list", "test_map");
        assert!(
            id.starts_with("aiken_list__test_map_"),
            "ID should start with readable prefix, got: {id}"
        );
        // Hash suffix is 8 hex chars
        let suffix = id.strip_prefix("aiken_list__test_map_").unwrap();
        assert_eq!(
            suffix.len(),
            8,
            "Hash suffix should be 8 hex chars, got: {suffix}"
        );
        assert!(
            suffix.chars().all(|c| c.is_ascii_hexdigit()),
            "Hash suffix should be hex, got: {suffix}"
        );
    }

    #[test]
    fn test_id_different_inputs_produce_different_ids() {
        let id1 = test_id("mod_a", "test_x");
        let id2 = test_id("mod_b", "test_x");
        assert_ne!(id1, id2, "Different modules should produce different IDs");
    }

    #[test]
    fn test_id_is_deterministic() {
        let id1 = test_id("my_module", "test_foo");
        let id2 = test_id("my_module", "test_foo");
        assert_eq!(id1, id2, "Same inputs should produce the same ID");
    }

    #[test]
    fn generate_workspace_creates_files() {
        let tmp = tempfile::tempdir().unwrap();
        let out_dir = tmp.path().to_path_buf();

        let tests = vec![
            make_test("my_module", "test_roundtrip"),
            make_test("aiken/list", "test_map"),
        ];

        let config = VerifyConfig {
            out_dir: out_dir.clone(),
            cek_budget: 20000,
            blaster_rev: DEFAULT_BLASTER_REV.to_string(),
            existential_mode: ExistentialMode::default(),
            target: VerificationTargetKind::default(),
        };

        let manifest = generate_lean_workspace(&tests, &config, false).unwrap();

        // Check files exist
        assert!(out_dir.join("lakefile.lean").exists());
        assert!(out_dir.join("lean-toolchain").exists());
        assert!(out_dir.join("AikenVerify.lean").exists());
        assert!(out_dir.join("AikenVerify/Utils.lean").exists());
        assert!(out_dir.join("manifest.json").exists());

        // Check per-test files
        assert!(out_dir
            .join("AikenVerify/Proofs/My_module/test_roundtrip.lean")
            .exists());
        assert!(out_dir
            .join("AikenVerify/Proofs/AikenList/test_map.lean")
            .exists());
        let id0 = &manifest.tests[0].id;
        let id1 = &manifest.tests[1].id;
        assert!(out_dir.join(format!("flat/{id0}.flat")).exists());
        assert!(out_dir.join(format!("flat/{id0}_fuzzer.flat")).exists());
        assert!(out_dir.join(format!("flat/{id1}.flat")).exists());
        assert!(out_dir.join(format!("flat/{id1}_fuzzer.flat")).exists());

        // Check manifest
        assert_eq!(manifest.tests.len(), 2);
        assert!(
            id0.starts_with("my_module__test_roundtrip_"),
            "ID should have readable prefix + hash: {id0}"
        );
        assert_eq!(
            manifest.tests[0].lean_module,
            "AikenVerify.Proofs.My_module.test_roundtrip"
        );
        assert!(
            id1.starts_with("aiken_list__test_map_"),
            "ID should have readable prefix + hash: {id1}"
        );

        // Check flat file content is the hex
        let flat_content = fs::read_to_string(out_dir.join(format!("flat/{id0}.flat"))).unwrap();
        assert_eq!(flat_content, "deadbeef");

        // Check Utils.lean contains the budget
        let utils = fs::read_to_string(out_dir.join("AikenVerify/Utils.lean")).unwrap();
        assert!(utils.contains("cekExecuteProgram p args 20000"));

        // Check root import file
        let root = fs::read_to_string(out_dir.join("AikenVerify.lean")).unwrap();
        assert!(root.contains("import AikenVerify.Utils"));
        assert!(root.contains("import AikenVerify.Proofs.AikenList.test_map"));
        assert!(root.contains("import AikenVerify.Proofs.My_module.test_roundtrip"));

        // Check generated .lean proof files contain actual theorems
        let proof1 =
            fs::read_to_string(out_dir.join("AikenVerify/Proofs/My_module/test_roundtrip.lean"))
                .unwrap();
        assert!(proof1.contains("theorem test_roundtrip"));
        assert!(proof1.contains("theorem test_roundtrip_alwaysTerminating"));
        assert!(proof1.contains("by blaster"));
        assert!(proof1.contains(&format!("#import_uplc prog_{id0} single_cbor_hex")));
        assert!(proof1.contains(&format!("#import_uplc fuzzer_prog_{id0} single_cbor_hex")));
        assert!(proof1.contains("(0 <= x && x <= 255)"));
        assert!(proof1.contains("namespace AikenVerify.Proofs.My_module.test_roundtrip"));

        let proof2 =
            fs::read_to_string(out_dir.join("AikenVerify/Proofs/AikenList/test_map.lean")).unwrap();
        assert!(proof2.contains("theorem test_map"));
        assert!(proof2.contains("theorem test_map_alwaysTerminating"));
        assert!(proof2.contains(&format!("#import_uplc prog_{id1} single_cbor_hex")));
        assert!(proof2.contains(&format!("#import_uplc fuzzer_prog_{id1} single_cbor_hex")));
        assert!(proof2.contains(&format!("./flat/{id1}.flat")));
        assert!(proof2.contains("(0 <= x && x <= 255)"));
    }

    #[test]
    fn generate_workspace_dotted_module_creates_matching_path() {
        let tmp = tempfile::tempdir().unwrap();
        let out_dir = tmp.path().to_path_buf();
        let tests = vec![make_test(
            "permissions.test",
            "prop_permissions_core_development_standard_ok",
        )];

        let config = VerifyConfig {
            out_dir: out_dir.clone(),
            cek_budget: 20000,
            blaster_rev: DEFAULT_BLASTER_REV.to_string(),
            existential_mode: ExistentialMode::default(),
            target: VerificationTargetKind::default(),
        };

        let manifest = generate_lean_workspace(&tests, &config, false).unwrap();
        assert_eq!(manifest.tests.len(), 1);
        assert_eq!(
            manifest.tests[0].lean_module,
            "AikenVerify.Proofs.PermissionsTest.prop_permissions_core_development_standard_ok"
        );
        assert!(
            out_dir.join(&manifest.tests[0].lean_file).exists(),
            "Generated proof file should exist at manifest path"
        );
    }

    #[test]
    fn generate_workspace_clears_stale_generated_files_but_keeps_static_cache() {
        let tmp = tempfile::tempdir().unwrap();
        let out_dir = tmp.path().to_path_buf();

        // Simulate stale generated workspace content from a prior run.
        fs::create_dir_all(out_dir.join("AikenVerify/Proofs/Stale")).unwrap();
        fs::write(
            out_dir.join("AikenVerify/Proofs/Stale/old.lean"),
            "-- stale",
        )
        .unwrap();
        fs::create_dir_all(out_dir.join("flat")).unwrap();
        fs::write(out_dir.join("flat/stale.flat"), "stale").unwrap();
        fs::write(
            out_dir.join("AikenVerify.lean"),
            "import AikenVerify.Proofs.Stale.old",
        )
        .unwrap();
        fs::write(out_dir.join("manifest.json"), "{}").unwrap();
        fs::create_dir_all(out_dir.join("logs")).unwrap();
        fs::write(out_dir.join("logs/stale.log"), "stale").unwrap();
        fs::create_dir_all(out_dir.join(".lake/build/lib/AikenVerify/Stale")).unwrap();
        fs::write(
            out_dir.join(".lake/build/lib/AikenVerify/Stale/stale.olean"),
            "stale",
        )
        .unwrap();

        // Simulate dependency cache artifacts that should persist.
        fs::create_dir_all(out_dir.join("PlutusCore")).unwrap();
        fs::write(out_dir.join("PlutusCore/lakefile.lean"), "-- keep").unwrap();
        fs::create_dir_all(out_dir.join(".lake/packages/Blaster")).unwrap();
        fs::write(out_dir.join(".lake/packages/Blaster/cache.txt"), "keep").unwrap();
        fs::create_dir_all(out_dir.join(".lake/build/lib/PlutusCore")).unwrap();
        fs::write(
            out_dir.join(".lake/build/lib/PlutusCore/cache.olean"),
            "keep",
        )
        .unwrap();

        let config = VerifyConfig {
            out_dir: out_dir.clone(),
            cek_budget: 20000,
            blaster_rev: DEFAULT_BLASTER_REV.to_string(),
            existential_mode: ExistentialMode::default(),
            target: VerificationTargetKind::default(),
        };

        let manifest =
            generate_lean_workspace(&[make_test("my_module", "test_roundtrip")], &config, false)
                .unwrap();
        let id = &manifest.tests[0].id;

        assert!(!out_dir.join("AikenVerify/Proofs/Stale/old.lean").exists());
        assert!(!out_dir.join("flat/stale.flat").exists());
        assert!(!out_dir.join("logs/stale.log").exists());
        assert!(!out_dir
            .join(".lake/build/lib/AikenVerify/Stale/stale.olean")
            .exists());

        assert!(out_dir.join("PlutusCore/lakefile.lean").exists());
        assert!(out_dir.join(".lake/packages/Blaster/cache.txt").exists());
        assert!(out_dir
            .join(".lake/build/lib/PlutusCore/cache.olean")
            .exists());

        assert!(out_dir
            .join("AikenVerify/Proofs/My_module/test_roundtrip.lean")
            .exists());
        assert!(out_dir.join(format!("flat/{id}.flat")).exists());
        assert!(out_dir.join(format!("flat/{id}_fuzzer.flat")).exists());
    }

    #[test]
    fn generate_workspace_empty_tests() {
        let tmp = tempfile::tempdir().unwrap();
        let config = VerifyConfig {
            out_dir: tmp.path().to_path_buf(),
            cek_budget: 20000,
            blaster_rev: DEFAULT_BLASTER_REV.to_string(),
            existential_mode: ExistentialMode::default(),
            target: VerificationTargetKind::default(),
        };

        let manifest = generate_lean_workspace(&[], &config, false).unwrap();
        assert!(manifest.tests.is_empty());
        assert!(tmp.path().join("lakefile.lean").exists());
    }

    #[test]
    fn fail_immediately_generates_true_assertion() {
        // FailImmediately = default test (no `fail` keyword), body should return true
        let test = make_test_with_failure("my_module", "test_pass", OnTestFailure::FailImmediately);
        let id = test_id("my_module", "test_pass");
        let lean_name = sanitize_lean_name("test_pass");
        let lean_module = "AikenVerify.Proofs.My_module.test_pass";

        let proof = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::default(),
            &VerificationTargetKind::default(),
        )
        .unwrap();

        assert!(
            proof.contains(&format!("(proveTests prog_{id} (intArg x)) = true")),
            "FailImmediately (default) should generate = true assertion, got:\n{proof}"
        );
        assert!(
            proof.contains(&format!("Option.isSome (proveTests prog_{id} (intArg x))")),
            "Termination theorem should still be generated"
        );
    }

    #[test]
    fn succeed_eventually_generates_false_assertion() {
        // SucceedEventually = `fail` keyword, body should return false
        let test =
            make_test_with_failure("my_module", "test_fail", OnTestFailure::SucceedEventually);
        let id = test_id("my_module", "test_fail");
        let lean_name = sanitize_lean_name("test_fail");
        let lean_module = "AikenVerify.Proofs.My_module.test_fail";

        let proof = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::default(),
            &VerificationTargetKind::default(),
        )
        .unwrap();

        assert!(
            proof.contains(&format!("(proveTests prog_{id} (intArg x)) = false")),
            "SucceedEventually (fail keyword) should generate = false assertion, got:\n{proof}"
        );
        assert!(
            !proof.contains("= true"),
            "SucceedEventually should not contain = true"
        );
    }

    #[test]
    fn fail_once_witness_mode_generates_existential_theorem() {
        let test =
            make_test_with_failure("my_module", "test_ev", OnTestFailure::SucceedImmediately);
        let id = test_id("my_module", "test_ev");
        let lean_name = sanitize_lean_name("test_ev");
        let lean_module = "AikenVerify.Proofs.My_module.test_ev";

        let proof = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::Witness,
            &VerificationTargetKind::default(),
        )
        .unwrap();

        assert!(
            proof.contains("∃ (x : Integer),"),
            "Witness mode should use existential quantifier, got:\n{proof}"
        );
        assert!(
            proof.contains("witnessTests"),
            "Witness mode should use witnessTests, got:\n{proof}"
        );
        assert!(
            proof.contains(&format!("witnessTests prog_{id} (intArg x) false")),
            "Fail-once witness theorem should search for a falsifying input (= false), got:\n{proof}"
        );
        assert!(
            proof.contains("∧"),
            "Existential should use ∧ (conjunction), not → (implication), got:\n{proof}"
        );
        assert!(
            proof.contains("⟨(0 : Integer), by decide⟩"),
            "Witness mode should provide concrete witness with decide tactic, got:\n{proof}"
        );
        assert!(
            proof.contains("-- Mode: witness"),
            "Should have mode comment, got:\n{proof}"
        );
        assert!(
            !proof.contains("alwaysTerminating"),
            "Existential theorems should not have termination theorem, got:\n{proof}"
        );
    }

    #[test]
    fn fail_once_proof_mode_generates_existential_theorem() {
        let test =
            make_test_with_failure("my_module", "test_ev", OnTestFailure::SucceedImmediately);
        let id = test_id("my_module", "test_ev");
        let lean_name = sanitize_lean_name("test_ev");
        let lean_module = "AikenVerify.Proofs.My_module.test_ev";

        let proof = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::Proof,
            &VerificationTargetKind::default(),
        )
        .unwrap();

        assert!(
            proof.contains("∃ (x : Integer),"),
            "Proof mode should use existential quantifier, got:\n{proof}"
        );
        assert!(
            proof.contains(&format!("(proveTests prog_{id} (intArg x)) = false")),
            "Fail-once proof theorem should assert existence of a falsifying input (= false), got:\n{proof}"
        );
        assert!(
            !proof.contains("= true"),
            "Fail-once proof theorem should not assert success witnesses (= true), got:\n{proof}"
        );
        assert!(
            proof.contains("∧"),
            "Existential should use ∧ (conjunction), not → (implication), got:\n{proof}"
        );
        assert!(
            proof.contains("by blaster"),
            "Proof mode should use blaster tactic, got:\n{proof}"
        );
        assert!(
            !proof.contains("⟨"),
            "Proof mode should not provide concrete witness, got:\n{proof}"
        );
        assert!(
            proof.contains("-- Mode: proof"),
            "Should have mode comment, got:\n{proof}"
        );
    }

    #[test]
    fn fail_once_void_witness_mode_generates_existential_error_theorem() {
        let mut test = make_test_with_failure(
            "my_module",
            "test_ev_void",
            OnTestFailure::SucceedImmediately,
        );
        test.return_mode = TestReturnMode::Void;
        let id = test_id("my_module", "test_ev_void");
        let lean_name = sanitize_lean_name("test_ev_void");
        let lean_module = "AikenVerify.Proofs.My_module.test_ev_void";

        let proof = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::Witness,
            &VerificationTargetKind::default(),
        )
        .unwrap();

        assert!(
            proof.contains("∃ (x : Integer),"),
            "Void witness should use existential quantifier, got:\n{proof}"
        );
        assert!(
            proof.contains("proveTestsError"),
            "Void witness should use proveTestsError, got:\n{proof}"
        );
        assert!(
            proof.contains("∧"),
            "Existential should use ∧ (conjunction), got:\n{proof}"
        );
        assert!(
            proof.contains("⟨(0 : Integer), by decide⟩"),
            "Void witness should provide concrete witness, got:\n{proof}"
        );
    }

    #[test]
    fn fail_once_tuple_witness_mode_uses_conjunction_between_bounds() {
        let mut test = make_test_with_type(
            "my_module",
            "test_tuple_exist",
            FuzzerOutputType::Tuple(vec![FuzzerOutputType::Int, FuzzerOutputType::Int]),
            FuzzerConstraint::Tuple(vec![
                FuzzerConstraint::IntRange {
                    min: "0".to_string(),
                    max: "10".to_string(),
                },
                FuzzerConstraint::IntRange {
                    min: "20".to_string(),
                    max: "30".to_string(),
                },
            ]),
        );
        test.on_test_failure = OnTestFailure::SucceedImmediately;
        let id = test_id("my_module", "test_tuple_exist");
        let lean_name = sanitize_lean_name("test_tuple_exist");
        let lean_module = "AikenVerify.Proofs.My_module.test_tuple_exist";

        let proof = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::Witness,
            &VerificationTargetKind::default(),
        )
        .unwrap();

        assert!(
            proof.contains("∃ (a : Integer) (b : Integer),"),
            "Should use existential quantifier for tuple, got:\n{proof}"
        );
        // All connectors must be ∧, no → anywhere in the theorem
        assert!(
            !proof.contains("→"),
            "Existential tuple theorem must not contain → (implication), got:\n{proof}"
        );
        assert!(
            proof.contains("(0 <= a && a <= 10)"),
            "First element bounds, got:\n{proof}"
        );
        assert!(
            proof.contains("(20 <= b && b <= 30)"),
            "Second element bounds, got:\n{proof}"
        );
        assert!(
            proof.contains("∧"),
            "All connectors must be ∧ (conjunction), got:\n{proof}"
        );
        assert!(
            proof.contains("⟨(0 : Integer), (20 : Integer), by decide⟩"),
            "Witness mode should provide tuple witness, got:\n{proof}"
        );
    }

    #[test]
    fn fail_once_data_witness_mode_uses_valid_data_constructor() {
        let mut test = make_test_with_type(
            "my_module",
            "test_data_exist",
            FuzzerOutputType::Data,
            FuzzerConstraint::Any,
        );
        test.on_test_failure = OnTestFailure::SucceedImmediately;
        let id = test_id("my_module", "test_data_exist");
        let lean_name = sanitize_lean_name("test_data_exist");
        let lean_module = "AikenVerify.Proofs.My_module.test_data_exist";

        let proof = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::Witness,
            &VerificationTargetKind::default(),
        )
        .unwrap();

        assert!(
            proof.contains("∃ (x : Data),"),
            "Data fail-once witness should use existential quantifier, got:\n{proof}"
        );
        assert!(
            proof.contains("⟨Data.I 0, by decide⟩"),
            "Data fail-once witness should use Data.I constructor, got:\n{proof}"
        );
        assert!(
            !proof.contains("Data.Integer"),
            "Data fail-once witness should not use invalid Data.Integer constructor, got:\n{proof}"
        );
    }

    #[test]
    fn fail_once_unsupported_witness_mode_uses_valid_data_constructor() {
        let mut test = make_test_with_type(
            "my_module",
            "test_adt_exist",
            FuzzerOutputType::Unsupported("MyCustomType".to_string()),
            FuzzerConstraint::Any,
        );
        test.on_test_failure = OnTestFailure::SucceedImmediately;
        let id = test_id("my_module", "test_adt_exist");
        let lean_name = sanitize_lean_name("test_adt_exist");
        let lean_module = "AikenVerify.Proofs.My_module.test_adt_exist";

        let proof = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::Witness,
            &VerificationTargetKind::default(),
        )
        .unwrap();

        assert!(
            proof.contains("∃ (x : Data),"),
            "Unsupported fail-once witness should use existential Data quantifier, got:\n{proof}"
        );
        assert!(
            proof.contains("⟨Data.I 0, by decide⟩"),
            "Unsupported fail-once witness should use Data.I constructor, got:\n{proof}"
        );
        assert!(
            !proof.contains("Data.Integer"),
            "Unsupported fail-once witness should not use invalid Data.Integer constructor, got:\n{proof}"
        );
    }

    #[test]
    fn fail_once_tuple_with_data_witness_uses_valid_data_constructor() {
        let mut test = make_test_with_type(
            "my_module",
            "test_tuple_data_exist",
            FuzzerOutputType::Tuple(vec![FuzzerOutputType::Int, FuzzerOutputType::Data]),
            FuzzerConstraint::Tuple(vec![
                FuzzerConstraint::IntRange {
                    min: "0".to_string(),
                    max: "10".to_string(),
                },
                FuzzerConstraint::Any,
            ]),
        );
        test.on_test_failure = OnTestFailure::SucceedImmediately;
        let id = test_id("my_module", "test_tuple_data_exist");
        let lean_name = sanitize_lean_name("test_tuple_data_exist");
        let lean_module = "AikenVerify.Proofs.My_module.test_tuple_data_exist";

        let proof = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::Witness,
            &VerificationTargetKind::default(),
        )
        .unwrap();

        assert!(
            proof.contains("∃ (a : Integer) (b : Data),"),
            "Tuple fail-once witness should quantify Integer and Data, got:\n{proof}"
        );
        assert!(
            proof.contains("⟨(0 : Integer), Data.I 0, by decide⟩"),
            "Tuple fail-once witness should use Data.I constructor for Data elements, got:\n{proof}"
        );
        assert!(
            !proof.contains("Data.Integer"),
            "Tuple fail-once witness should not use invalid Data.Integer constructor, got:\n{proof}"
        );
    }

    #[test]
    fn fail_once_list_of_data_witness_uses_valid_data_constructor() {
        let mut test = make_test_with_type(
            "my_module",
            "test_list_data_exist",
            FuzzerOutputType::List(Box::new(FuzzerOutputType::Data)),
            FuzzerConstraint::List {
                elem: Box::new(FuzzerConstraint::Any),
                min_len: Some(2),
                max_len: Some(5),
            },
        );
        test.on_test_failure = OnTestFailure::SucceedImmediately;
        let id = test_id("my_module", "test_list_data_exist");
        let lean_name = sanitize_lean_name("test_list_data_exist");
        let lean_module = "AikenVerify.Proofs.My_module.test_list_data_exist";

        let proof = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::Witness,
            &VerificationTargetKind::default(),
        )
        .unwrap();

        assert!(
            proof.contains("∃ (xs : List Data),"),
            "List<Data> fail-once witness should quantify over List Data, got:\n{proof}"
        );
        assert!(
            proof.contains("⟨[Data.I 0, Data.I 0], by decide⟩"),
            "List<Data> fail-once witness should use Data.I constructor for elements, got:\n{proof}"
        );
        assert!(
            !proof.contains("Data.Integer"),
            "List<Data> fail-once witness should not use invalid Data.Integer constructor, got:\n{proof}"
        );
    }

    #[test]
    fn void_return_mode_generates_halt_theorem() {
        let mut test = make_test("my_module", "test_void");
        test.return_mode = TestReturnMode::Void;
        let id = test_id("my_module", "test_void");
        let lean_name = sanitize_lean_name("test_void");
        let lean_module = "AikenVerify.Proofs.My_module.test_void";

        let proof = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::default(),
            &VerificationTargetKind::default(),
        )
        .unwrap();

        assert!(
            proof.contains("proveTestsHalt"),
            "Void mode should use proveTestsHalt, got:\n{proof}"
        );
        assert!(
            !proof.contains("proveTests prog_"),
            "Void mode should not use proveTests (Bool path), got:\n{proof}"
        );
        assert!(
            !proof.contains("alwaysTerminating"),
            "Void mode should not generate separate termination theorem (halt implies it), got:\n{proof}"
        );
    }

    #[test]
    fn void_fail_mode_generates_error_theorem() {
        let mut test = make_test_with_failure(
            "my_module",
            "test_void_fail",
            OnTestFailure::SucceedEventually,
        );
        test.return_mode = TestReturnMode::Void;
        let id = test_id("my_module", "test_void_fail");
        let lean_name = sanitize_lean_name("test_void_fail");
        let lean_module = "AikenVerify.Proofs.My_module.test_void_fail";

        let proof = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::default(),
            &VerificationTargetKind::default(),
        )
        .unwrap();

        assert!(
            proof.contains("proveTestsError"),
            "Void+fail mode should use proveTestsError, got:\n{proof}"
        );
        assert!(
            !proof.contains("= true") && !proof.contains("= false"),
            "Void+fail mode should not contain = true or = false, got:\n{proof}"
        );
        assert!(
            !proof.contains("alwaysTerminating"),
            "Void+fail mode should not generate separate termination theorem, got:\n{proof}"
        );
    }

    #[test]
    fn bool_return_mode_generates_prove_tests() {
        // Verify Bool mode still works with explicit return_mode
        let mut test = make_test("my_module", "test_bool");
        test.return_mode = TestReturnMode::Bool;
        let id = test_id("my_module", "test_bool");
        let lean_name = sanitize_lean_name("test_bool");
        let lean_module = "AikenVerify.Proofs.My_module.test_bool";

        let proof = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::default(),
            &VerificationTargetKind::default(),
        )
        .unwrap();

        assert!(
            proof.contains("= true"),
            "Bool mode should contain = true, got:\n{proof}"
        );
        assert!(
            proof.contains("alwaysTerminating"),
            "Bool mode should generate termination theorem, got:\n{proof}"
        );
        assert!(
            proof.contains("Option.isSome"),
            "Bool mode should use Option.isSome for termination, got:\n{proof}"
        );
    }

    fn make_test_with_type(
        module: &str,
        name: &str,
        fuzzer_output_type: FuzzerOutputType,
        constraint: FuzzerConstraint,
    ) -> ExportedPropertyTest {
        ExportedPropertyTest {
            name: format!("{module}.{name}"),
            module: module.to_string(),
            input_path: format!("lib/{}.ak", module.replace('/', "/")),
            on_test_failure: OnTestFailure::FailImmediately,
            return_mode: TestReturnMode::Bool,
            target_kind: Default::default(),
            validator_target: None,
            test_program: ExportedProgram {
                hex: "deadbeef".to_string(),
                flat_bytes: None,
            },
            fuzzer_program: ExportedProgram {
                hex: "cafebabe".to_string(),
                flat_bytes: None,
            },
            fuzzer_type: format!("Fuzzer<{:?}>", fuzzer_output_type),
            fuzzer_output_type,
            constraint,
        }
    }

    #[test]
    fn bool_generates_theorem_without_bounds() {
        let test = make_test_with_type(
            "my_module",
            "test_bool",
            FuzzerOutputType::Bool,
            FuzzerConstraint::Any,
        );
        let id = test_id("my_module", "test_bool");
        let lean_name = sanitize_lean_name("test_bool");
        let lean_module = "AikenVerify.Proofs.My_module.test_bool";

        let proof = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::default(),
            &VerificationTargetKind::default(),
        )
        .unwrap();

        assert!(
            proof.contains("∀ (x : Bool),"),
            "Bool theorem should quantify over Bool, got:\n{proof}"
        );
        assert!(
            proof.contains(&format!("(proveTests prog_{id} (boolArg x)) = true")),
            "Bool theorem should use boolArg, got:\n{proof}"
        );
        assert!(
            proof.contains("theorem test_bool_alwaysTerminating"),
            "Should have termination theorem, got:\n{proof}"
        );
        assert!(
            proof.contains(&format!("Option.isSome (proveTests prog_{id} (boolArg x))")),
            "Termination theorem should use boolArg, got:\n{proof}"
        );
        assert!(
            !proof.contains("<="),
            "Bool theorem should not have bounds, got:\n{proof}"
        );
        assert!(
            !proof.contains("Integer"),
            "Bool theorem should not reference Integer, got:\n{proof}"
        );
    }

    #[test]
    fn bool_succeed_eventually_generates_false() {
        // SucceedEventually = `fail` keyword, body should return false
        let mut test = make_test_with_type(
            "my_module",
            "test_bool_fail",
            FuzzerOutputType::Bool,
            FuzzerConstraint::Any,
        );
        test.on_test_failure = OnTestFailure::SucceedEventually;

        let id = test_id("my_module", "test_bool_fail");
        let lean_name = sanitize_lean_name("test_bool_fail");
        let lean_module = "AikenVerify.Proofs.My_module.test_bool_fail";

        let proof = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::default(),
            &VerificationTargetKind::default(),
        )
        .unwrap();

        assert!(
            proof.contains("(boolArg x)) = false"),
            "SucceedEventually (fail keyword) Bool should generate = false, got:\n{proof}"
        );
    }

    #[test]
    fn tuple_int_int_generates_two_variable_theorem() {
        let test = make_test_with_type(
            "my_module",
            "test_pair",
            FuzzerOutputType::Tuple(vec![FuzzerOutputType::Int, FuzzerOutputType::Int]),
            FuzzerConstraint::IntRange {
                min: "1".to_string(),
                max: "10".to_string(),
            },
        );
        let id = test_id("my_module", "test_pair");
        let lean_name = sanitize_lean_name("test_pair");
        let lean_module = "AikenVerify.Proofs.My_module.test_pair";

        let proof = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::default(),
            &VerificationTargetKind::default(),
        )
        .unwrap();

        assert!(
            proof.contains("∀ (a : Integer) (b : Integer),"),
            "Tuple theorem should quantify over two Integers, got:\n{proof}"
        );
        assert!(
            proof.contains("(1 <= a && a <= 10)"),
            "Should have bounds for a, got:\n{proof}"
        );
        assert!(
            proof.contains("(1 <= b && b <= 10)"),
            "Should have bounds for b, got:\n{proof}"
        );
        assert!(
            proof.contains("Data.List [Data.I a, Data.I b]"),
            "Should inline tuple encoding, got:\n{proof}"
        );
        assert!(
            proof.contains("= true"),
            "Should have correctness assertion, got:\n{proof}"
        );
        assert!(
            proof.contains("theorem test_pair_alwaysTerminating"),
            "Termination theorem should be present, got:\n{proof}"
        );
    }

    #[test]
    fn tuple_int_int_component_bounds_generate_two_ranges() {
        let test = make_test_with_type(
            "my_module",
            "test_pair_components",
            FuzzerOutputType::Tuple(vec![FuzzerOutputType::Int, FuzzerOutputType::Int]),
            FuzzerConstraint::Tuple(vec![
                FuzzerConstraint::IntRange {
                    min: "0".to_string(),
                    max: "10".to_string(),
                },
                FuzzerConstraint::IntRange {
                    min: "20".to_string(),
                    max: "30".to_string(),
                },
            ]),
        );
        let id = test_id("my_module", "test_pair_components");
        let lean_name = sanitize_lean_name("test_pair_components");
        let lean_module = "AikenVerify.Proofs.My_module.test_pair_components";

        let proof = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::default(),
            &VerificationTargetKind::default(),
        )
        .unwrap();

        assert!(
            proof.contains("∀ (a : Integer) (b : Integer),"),
            "Tuple theorem should quantify over two Integers, got:\n{proof}"
        );
        assert!(
            proof.contains("(0 <= a && a <= 10)"),
            "Should have first component bounds for a, got:\n{proof}"
        );
        assert!(
            proof.contains("(20 <= b && b <= 30)"),
            "Should have second component bounds for b, got:\n{proof}"
        );
    }

    #[test]
    fn tuple_int_int_component_bounds_wrong_arity_errors() {
        let test = make_test_with_type(
            "my_module",
            "test_pair_components_bad_arity",
            FuzzerOutputType::Tuple(vec![FuzzerOutputType::Int, FuzzerOutputType::Int]),
            FuzzerConstraint::Tuple(vec![FuzzerConstraint::IntRange {
                min: "0".to_string(),
                max: "10".to_string(),
            }]),
        );
        let id = test_id("my_module", "test_pair_components_bad_arity");
        let lean_name = sanitize_lean_name("test_pair_components_bad_arity");
        let lean_module = "AikenVerify.Proofs.My_module.test_pair_components_bad_arity";

        let result = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::default(),
            &VerificationTargetKind::default(),
        );
        assert!(
            result.is_err(),
            "Tuple with wrong arity constraint should error"
        );
    }

    #[test]
    fn tuple_int_int_unknown_bounds_errors() {
        let test = make_test_with_type(
            "my_module",
            "test_pair_nobound",
            FuzzerOutputType::Tuple(vec![FuzzerOutputType::Int, FuzzerOutputType::Int]),
            FuzzerConstraint::Any,
        );
        let id = test_id("my_module", "test_pair_nobound");
        let lean_name = sanitize_lean_name("test_pair_nobound");
        let lean_module = "AikenVerify.Proofs.My_module.test_pair_nobound";

        let result = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::default(),
            &VerificationTargetKind::default(),
        );
        assert!(result.is_err());
        assert!(
            result.unwrap_err().to_string().contains("no extractable"),
            "Should indicate missing Int range constraint"
        );
    }

    #[test]
    fn bytearray_generates_theorem_without_bounds() {
        let test = make_test_with_type(
            "my_module",
            "test_bytes",
            FuzzerOutputType::ByteArray,
            FuzzerConstraint::Any,
        );
        let id = test_id("my_module", "test_bytes");
        let lean_name = sanitize_lean_name("test_bytes");
        let lean_module = "AikenVerify.Proofs.My_module.test_bytes";

        let result = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::default(),
            &VerificationTargetKind::default(),
        );
        assert!(result.is_ok());
        let proof = result.unwrap();
        assert!(
            proof.contains("∀ (x : ByteString),"),
            "ByteArray theorem should quantify over ByteString, got:\n{proof}"
        );
        assert!(
            proof.contains(&format!("(proveTests prog_{id} (bytearrayArg x)) = true")),
            "ByteArray theorem should use bytearrayArg, got:\n{proof}"
        );
        assert!(
            !proof.contains("<="),
            "ByteArray theorem should not rely on integer bounds, got:\n{proof}"
        );
    }

    #[test]
    fn list_without_bounds_generates_theorem() {
        let test = make_test_with_type(
            "my_module",
            "test_list",
            FuzzerOutputType::List(Box::new(FuzzerOutputType::Int)),
            FuzzerConstraint::List {
                elem: Box::new(FuzzerConstraint::Any),
                min_len: None,
                max_len: None,
            },
        );
        let id = test_id("my_module", "test_list");
        let lean_name = sanitize_lean_name("test_list");
        let lean_module = "AikenVerify.Proofs.My_module.test_list";

        let proof = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::default(),
            &VerificationTargetKind::default(),
        )
        .unwrap();
        assert!(
            proof.contains("∀ (xs : List Integer),"),
            "List<Int> should quantify over List Integer, got:\n{proof}"
        );
        assert!(
            !proof.contains("xs.length"),
            "Unbounded List should not emit length preconditions, got:\n{proof}"
        );
    }

    #[test]
    fn list_with_any_constraint_errors() {
        let test = make_test_with_type(
            "my_module",
            "test_list_any",
            FuzzerOutputType::List(Box::new(FuzzerOutputType::Int)),
            FuzzerConstraint::Any,
        );
        let id = test_id("my_module", "test_list_any");
        let lean_name = sanitize_lean_name("test_list_any");
        let lean_module = "AikenVerify.Proofs.My_module.test_list_any";

        let result = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::default(),
            &VerificationTargetKind::default(),
        );
        assert!(result.is_err());
        let msg = result.unwrap_err().to_string();
        assert!(
            msg.contains("no extractable list-domain constraints"),
            "Should indicate missing list-domain constraint, got:\n{msg}"
        );
    }

    #[test]
    fn list_with_unsupported_constraint_errors() {
        let test = make_test_with_type(
            "my_module",
            "test_list_unsupported",
            FuzzerOutputType::List(Box::new(FuzzerOutputType::Int)),
            FuzzerConstraint::Unsupported {
                reason: "scenario.ok: constraint extraction not implemented yet".to_string(),
            },
        );
        let id = test_id("my_module", "test_list_unsupported");
        let lean_name = sanitize_lean_name("test_list_unsupported");
        let lean_module = "AikenVerify.Proofs.My_module.test_list_unsupported";

        let result = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::default(),
            &VerificationTargetKind::default(),
        );
        assert!(result.is_err());
        let msg = result.unwrap_err().to_string();
        assert!(
            msg.contains("unsupported list-domain extraction"),
            "Should indicate unsupported list-domain extraction, got:\n{msg}"
        );
        assert!(msg.contains("scenario.ok"));
    }

    #[test]
    fn list_data_without_domain_predicate_uses_fuzzer_domain_fallback() {
        let test = make_test_with_type(
            "my_module",
            "test_list_data_unconstrained",
            FuzzerOutputType::List(Box::new(FuzzerOutputType::Data)),
            FuzzerConstraint::List {
                elem: Box::new(FuzzerConstraint::Any),
                min_len: None,
                max_len: None,
            },
        );
        let id = test_id("my_module", "test_list_data_unconstrained");
        let lean_name = sanitize_lean_name("test_list_data_unconstrained");
        let lean_module = "AikenVerify.Proofs.My_module.test_list_data_unconstrained";

        let proof = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::default(),
            &VerificationTargetKind::default(),
        )
        .unwrap();
        assert!(
            proof.contains("∀ (seed : Data),"),
            "Fallback theorem should quantify over seed only, got:\n{proof}"
        );
        assert!(
            proof.contains(&format!("match sampleFuzzerValue fuzzer_prog_{id} seed with")),
            "Fallback theorem should branch on sampled fuzzer value, got:\n{proof}"
        );
        assert!(
            proof.contains(&format!("| some x => (proveTests prog_{id} (dataArg x)) = true")),
            "Fallback theorem should evaluate property on sampled x, got:\n{proof}"
        );
    }

    #[test]
    fn list_unsupported_element_without_domain_predicate_uses_fuzzer_domain_fallback() {
        let test = make_test_with_type(
            "my_module",
            "test_list_transaction_like",
            FuzzerOutputType::List(Box::new(FuzzerOutputType::Unsupported(
                "cardano/transaction.Transaction".to_string(),
            ))),
            FuzzerConstraint::List {
                elem: Box::new(FuzzerConstraint::Any),
                min_len: None,
                max_len: None,
            },
        );
        let id = test_id("my_module", "test_list_transaction_like");
        let lean_name = sanitize_lean_name("test_list_transaction_like");
        let lean_module = "AikenVerify.Proofs.My_module.test_list_transaction_like";

        let proof = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::default(),
            &VerificationTargetKind::default(),
        )
        .unwrap();
        assert!(
            proof.contains(&format!("match sampleFuzzerValue fuzzer_prog_{id} seed with")),
            "Fallback theorem should branch on sampled fuzzer value, got:\n{proof}"
        );
    }

    #[test]
    fn list_data_fallback_rejects_existential_mode() {
        let mut test = make_test_with_type(
            "my_module",
            "test_list_data_existential",
            FuzzerOutputType::List(Box::new(FuzzerOutputType::Data)),
            FuzzerConstraint::List {
                elem: Box::new(FuzzerConstraint::Any),
                min_len: None,
                max_len: None,
            },
        );
        test.on_test_failure = OnTestFailure::SucceedImmediately;

        let id = test_id("my_module", "test_list_data_existential");
        let lean_name = sanitize_lean_name("test_list_data_existential");
        let lean_module = "AikenVerify.Proofs.My_module.test_list_data_existential";

        let result = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::Witness,
            &VerificationTargetKind::default(),
        );
        assert!(result.is_err());
        let msg = result.unwrap_err().to_string();
        assert!(
            msg.contains("existential mode is not supported"),
            "Expected explicit fallback limitation for existential mode, got:\n{msg}"
        );
    }

    #[test]
    fn data_generates_theorem_without_bounds() {
        let test = make_test_with_type(
            "my_module",
            "test_data",
            FuzzerOutputType::Data,
            FuzzerConstraint::Any,
        );
        let id = test_id("my_module", "test_data");
        let lean_name = sanitize_lean_name("test_data");
        let lean_module = "AikenVerify.Proofs.My_module.test_data";

        let proof = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::default(),
            &VerificationTargetKind::default(),
        )
        .unwrap();

        assert!(
            proof.contains("∀ (x : Data),"),
            "Data theorem should quantify over Data, got:\n{proof}"
        );
        assert!(
            proof.contains(&format!("(proveTests prog_{id} (dataArg x)) = true")),
            "Data theorem should use dataArg, got:\n{proof}"
        );
    }

    #[test]
    fn tuple_data_data_generates_two_variable_theorem() {
        let test = make_test_with_type(
            "my_module",
            "test_data_pair",
            FuzzerOutputType::Tuple(vec![FuzzerOutputType::Data, FuzzerOutputType::Data]),
            FuzzerConstraint::Any,
        );
        let id = test_id("my_module", "test_data_pair");
        let lean_name = sanitize_lean_name("test_data_pair");
        let lean_module = "AikenVerify.Proofs.My_module.test_data_pair";

        let proof = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::default(),
            &VerificationTargetKind::default(),
        )
        .unwrap();

        assert!(
            proof.contains("∀ (a : Data) (b : Data),"),
            "Tuple(Data,Data) should quantify over two Data vars, got:\n{proof}"
        );
        assert!(
            proof.contains("Data.List [a, b]"),
            "Tuple(Data,Data) should inline Data.List encoding, got:\n{proof}"
        );
        assert!(proof.contains("= true"));
    }

    #[test]
    fn tuple_data_data_data_generates_three_variable_theorem() {
        let test = make_test_with_type(
            "my_module",
            "test_data_triple",
            FuzzerOutputType::Tuple(vec![
                FuzzerOutputType::Data,
                FuzzerOutputType::Data,
                FuzzerOutputType::Data,
            ]),
            FuzzerConstraint::Any,
        );
        let id = test_id("my_module", "test_data_triple");
        let lean_name = sanitize_lean_name("test_data_triple");
        let lean_module = "AikenVerify.Proofs.My_module.test_data_triple";

        let proof = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::default(),
            &VerificationTargetKind::default(),
        )
        .unwrap();

        assert!(
            proof.contains("∀ (a : Data) (b : Data) (c : Data),"),
            "Tuple(Data,Data,Data) should quantify over three Data vars, got:\n{proof}"
        );
        assert!(
            proof.contains("Data.List [a, b, c]"),
            "Tuple(Data,Data,Data) should inline Data.List encoding, got:\n{proof}"
        );
        assert!(proof.contains("= true"));
    }

    #[test]
    fn tuple_mixed_int_bool_generates_theorem_with_bounds() {
        let test = make_test_with_type(
            "my_module",
            "test_tuple_mixed",
            FuzzerOutputType::Tuple(vec![FuzzerOutputType::Int, FuzzerOutputType::Bool]),
            FuzzerConstraint::Tuple(vec![
                FuzzerConstraint::IntRange {
                    min: "0".to_string(),
                    max: "100".to_string(),
                },
                FuzzerConstraint::Any,
            ]),
        );
        let id = test_id("my_module", "test_tuple_mixed");
        let lean_name = sanitize_lean_name("test_tuple_mixed");
        let lean_module = "AikenVerify.Proofs.My_module.test_tuple_mixed";

        let proof = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::default(),
            &VerificationTargetKind::default(),
        )
        .unwrap();

        assert!(
            proof.contains("∀ (a : Integer) (b : Bool),"),
            "Mixed tuple should quantify over Integer and Bool, got:\n{proof}"
        );
        assert!(
            proof.contains("(0 <= a && a <= 100)"),
            "Should have bounds for Int element, got:\n{proof}"
        );
        assert!(
            proof.contains("Data.List [Data.I a, (Data.Constr (if b then 1 else 0) [])]"),
            "Should inline mixed encoding, got:\n{proof}"
        );
    }

    #[test]
    fn tuple_with_nested_list_element_returns_error() {
        let test = make_test_with_type(
            "my_module",
            "test_tuple_nested",
            FuzzerOutputType::Tuple(vec![
                FuzzerOutputType::Int,
                FuzzerOutputType::List(Box::new(FuzzerOutputType::Int)),
            ]),
            FuzzerConstraint::Any,
        );
        let id = test_id("my_module", "test_tuple_nested");
        let lean_name = sanitize_lean_name("test_tuple_nested");
        let lean_module = "AikenVerify.Proofs.My_module.test_tuple_nested";

        let result = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::default(),
            &VerificationTargetKind::default(),
        );
        assert!(result.is_err());
        let err_msg = result.unwrap_err().to_string();
        assert!(
            err_msg.contains("unsupported tuple element type"),
            "Tuple with nested List element should error, got: {err_msg}"
        );
    }

    #[test]
    fn utils_contains_arg_helpers() {
        let utils = generate_utils(10000);
        assert!(
            utils.contains("def boolArg"),
            "Utils should contain boolArg definition"
        );
        assert!(
            utils.contains("Data.Constr (if x then 1 else 0) []"),
            "boolArg should encode Bool as Data.Constr"
        );
        assert!(utils.contains("def intArg"));
        assert!(utils.contains("def bytearrayArg"));
        assert!(utils.contains("def stringArg"));
        assert!(utils.contains("def dataArg"));
        assert!(utils.contains("def pairArg"));
        assert!(utils.contains("def dataArgs2"));
        assert!(utils.contains("def dataArgs3"));
        assert!(
            utils.contains("def proveTestsHalt"),
            "Utils should contain proveTestsHalt for Void mode"
        );
        assert!(
            utils.contains("def proveTestsError"),
            "Utils should contain proveTestsError for Void+fail mode"
        );
        // Step 5.1: witness helper for fail-once existential mode
        assert!(
            utils.contains("def witnessTests"),
            "Utils should contain witnessTests for fail-once witness mode"
        );
    }

    #[test]
    fn existential_mode_parse_roundtrip() {
        assert_eq!(
            "witness".parse::<ExistentialMode>().unwrap(),
            ExistentialMode::Witness
        );
        assert_eq!(
            "proof".parse::<ExistentialMode>().unwrap(),
            ExistentialMode::Proof
        );
        assert_eq!(
            "Witness".parse::<ExistentialMode>().unwrap(),
            ExistentialMode::Witness
        );
        assert_eq!(
            "PROOF".parse::<ExistentialMode>().unwrap(),
            ExistentialMode::Proof
        );
        assert!("unknown".parse::<ExistentialMode>().is_err());
    }

    #[test]
    fn existential_mode_display() {
        assert_eq!(ExistentialMode::Witness.to_string(), "witness");
        assert_eq!(ExistentialMode::Proof.to_string(), "proof");
    }

    /// Documents the map2 over-approximation: when a property test uses
    /// `fuzz.map2(int_between(0, 10), int_between(20, 30), fn(a, b) { (a, b) })`,
    /// the generated proof quantifies over the Cartesian product of [0,10] x [20,30]
    /// independently. Any relational constraints introduced by the mapper (e.g. a < b)
    /// are NOT captured -- the proof domain is an over-approximation. Properties
    /// depending on such correlations may fail in formal verification even if
    /// property testing passes.
    #[test]
    fn map2_tuple_decomposition_is_cartesian_product() {
        // Simulates map2(int_between(0, 10), int_between(20, 30), fn(a, b) { (a, b) })
        let test = make_test_with_type(
            "my_module",
            "test_map2_pair",
            FuzzerOutputType::Tuple(vec![FuzzerOutputType::Int, FuzzerOutputType::Int]),
            FuzzerConstraint::Tuple(vec![
                FuzzerConstraint::IntRange {
                    min: "0".to_string(),
                    max: "10".to_string(),
                },
                FuzzerConstraint::IntRange {
                    min: "20".to_string(),
                    max: "30".to_string(),
                },
            ]),
        );
        let id = test_id("my_module", "test_map2_pair");
        let lean_name = sanitize_lean_name("test_map2_pair");
        let lean_module = "AikenVerify.Proofs.My_module.test_map2_pair";

        let proof = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::default(),
            &VerificationTargetKind::default(),
        )
        .unwrap();

        // Each element is independently bounded (Cartesian product, no cross-element constraints)
        assert!(
            proof.contains("∀ (a : Integer) (b : Integer),"),
            "map2 decomposition should produce independent quantifiers, got:\n{proof}"
        );
        assert!(
            proof.contains("(0 <= a && a <= 10)"),
            "First element bounds preserved independently, got:\n{proof}"
        );
        assert!(
            proof.contains("(20 <= b && b <= 30)"),
            "Second element bounds preserved independently, got:\n{proof}"
        );
        // Verify the preconditions use → (implication) connecting them, not ∧
        // This shows they are independent constraints joined as implications
        assert!(
            proof.contains("→"),
            "Element bounds should be connected with →, got:\n{proof}"
        );
    }

    #[test]
    fn requires_explicit_bounds_is_constraint_driven() {
        // Int with Any constraint => needs bounds
        assert!(requires_explicit_bounds(
            &FuzzerOutputType::Int,
            &FuzzerConstraint::Any,
        ));
        // Int with IntRange => does not need bounds
        assert!(!requires_explicit_bounds(
            &FuzzerOutputType::Int,
            &FuzzerConstraint::IntRange {
                min: "0".to_string(),
                max: "10".to_string(),
            },
        ));
        // (Int, Int) with Any => needs bounds
        assert!(requires_explicit_bounds(
            &FuzzerOutputType::Tuple(vec![FuzzerOutputType::Int, FuzzerOutputType::Int]),
            &FuzzerConstraint::Any,
        ));
        // (Int, Int) with Tuple of IntRanges => does not need bounds
        assert!(!requires_explicit_bounds(
            &FuzzerOutputType::Tuple(vec![FuzzerOutputType::Int, FuzzerOutputType::Int]),
            &FuzzerConstraint::Tuple(vec![
                FuzzerConstraint::IntRange {
                    min: "0".to_string(),
                    max: "10".to_string(),
                },
                FuzzerConstraint::IntRange {
                    min: "20".to_string(),
                    max: "30".to_string(),
                },
            ]),
        ));
        // Bool never needs bounds regardless of constraint
        assert!(!requires_explicit_bounds(
            &FuzzerOutputType::Bool,
            &FuzzerConstraint::Any,
        ));
        // ByteArray never needs bounds
        assert!(!requires_explicit_bounds(
            &FuzzerOutputType::ByteArray,
            &FuzzerConstraint::Any,
        ));
        // Data never needs bounds
        assert!(!requires_explicit_bounds(
            &FuzzerOutputType::Data,
            &FuzzerConstraint::Any,
        ));
        // (Data, Data) never needs bounds
        assert!(!requires_explicit_bounds(
            &FuzzerOutputType::Tuple(vec![FuzzerOutputType::Data, FuzzerOutputType::Data]),
            &FuzzerConstraint::Any,
        ));
        // Int with Map(IntRange) => does not need bounds (Map is transparent)
        assert!(!requires_explicit_bounds(
            &FuzzerOutputType::Int,
            &FuzzerConstraint::Map(Box::new(FuzzerConstraint::IntRange {
                min: "0".to_string(),
                max: "10".to_string(),
            })),
        ));
    }

    fn make_manifest(entries: Vec<(&str, &str, &str)>) -> GeneratedManifest {
        make_manifest_with_termination(entries, true)
    }

    fn make_manifest_with_termination(
        entries: Vec<(&str, &str, &str)>,
        has_termination_theorem: bool,
    ) -> GeneratedManifest {
        GeneratedManifest {
            version: "1.0.0".to_string(),
            tests: entries
                .into_iter()
                .map(|(module, name, lean_theorem)| ManifestEntry {
                    id: test_id(module, name),
                    aiken_module: module.to_string(),
                    aiken_name: name.to_string(),
                    lean_module: format!(
                        "AikenVerify.Proofs.{}.{}",
                        module_to_lean_segment(module),
                        sanitize_lean_name(name)
                    ),
                    lean_theorem: lean_theorem.to_string(),
                    lean_file: format!(
                        "AikenVerify/Proofs/{}/{}.lean",
                        module_to_lean_segment(module),
                        sanitize_lean_name(name)
                    ),
                    flat_file: format!("flat/{}.flat", test_id(module, name)),
                    has_termination_theorem,
                })
                .collect(),
            skipped: Vec::new(),
        }
    }

    #[test]
    fn parse_verify_results_all_proved() {
        let manifest = make_manifest(vec![
            ("my_module", "test_add", "test_add"),
            ("my_module", "test_sub", "test_sub"),
        ]);
        let raw = VerifyResult {
            success: true,
            stdout: "Build completed successfully.".to_string(),
            stderr: String::new(),
            exit_code: Some(0),
            theorem_results: None,
        };

        let summary = parse_verify_results(raw, &manifest);

        assert_eq!(summary.total, 4);
        assert_eq!(summary.proved, 4);
        assert_eq!(summary.failed, 0);
        assert_eq!(summary.timed_out, 0);
        assert_eq!(summary.unknown, 0);
        assert!(summary
            .theorems
            .iter()
            .all(|t| matches!(t.status, ProofStatus::Proved)));
    }

    #[test]
    fn parse_verify_results_specific_failure() {
        let manifest = make_manifest(vec![
            ("my_module", "test_add", "test_add"),
            ("my_module", "test_sub", "test_sub"),
        ]);
        let raw = VerifyResult {
            success: false,
            stdout: String::new(),
            stderr: "error: 'test_add' has unsolved goals\nsome other context".to_string(),
            exit_code: Some(1),
            theorem_results: None,
        };

        let summary = parse_verify_results(raw, &manifest);

        assert_eq!(summary.total, 4);
        assert_eq!(summary.failed, 1);
        assert_eq!(summary.timed_out, 0);
        assert!(summary.proved == 0);

        let test_add = summary
            .theorems
            .iter()
            .find(|t| t.theorem_name == "test_add")
            .unwrap();
        assert!(
            matches!(&test_add.status, ProofStatus::Failed { reason, .. } if reason.contains("test_add")),
            "test_add should be Failed, got: {:?}",
            test_add.status
        );

        let test_sub = summary
            .theorems
            .iter()
            .find(|t| t.theorem_name == "test_sub")
            .unwrap();
        assert!(
            matches!(test_sub.status, ProofStatus::Unknown),
            "test_sub should be Unknown, got: {:?}",
            test_sub.status
        );
    }

    #[test]
    fn parse_verify_results_marks_built_modules_as_proved_when_one_module_fails() {
        let manifest = make_manifest(vec![
            ("my_module", "test_add", "test_add"),
            ("my_module", "test_sub", "test_sub"),
            ("my_module", "test_bad", "test_bad"),
        ]);
        let raw = VerifyResult {
            success: false,
            stdout: "⚠ [1/3] Replayed AikenVerify.Proofs.My_module.test_add\n\
                     info: AikenVerify/Proofs/My_module/test_add.lean:10:5: ✅ Valid\n\
                     ⚠ [2/3] Replayed AikenVerify.Proofs.My_module.test_sub\n\
                     info: AikenVerify/Proofs/My_module/test_sub.lean:11:5: ✅ Valid\n\
                     ✖ [3/3] Building AikenVerify.Proofs.My_module.test_bad (5s)\n\
                     error: AikenVerify/Proofs/My_module/test_bad.lean:21:5: translateApp: unsupported\n\
                     error: Lean exited with code 1\n\
                     Some required targets logged failures:\n\
                     - AikenVerify.Proofs.My_module.test_bad"
                .to_string(),
            stderr: "error: build failed".to_string(),
            exit_code: Some(1),
            theorem_results: None,
        };

        let summary = parse_verify_results(raw, &manifest);

        assert_eq!(summary.total, 6);
        assert_eq!(summary.proved, 4);
        assert_eq!(summary.failed, 2);
        assert_eq!(summary.timed_out, 0);
        assert_eq!(summary.unknown, 0);

        let test_add = summary
            .theorems
            .iter()
            .find(|t| t.theorem_name == "test_add")
            .unwrap();
        assert!(
            matches!(test_add.status, ProofStatus::Proved),
            "test_add should be Proved, got: {:?}",
            test_add.status
        );

        let test_sub = summary
            .theorems
            .iter()
            .find(|t| t.theorem_name == "test_sub")
            .unwrap();
        assert!(
            matches!(test_sub.status, ProofStatus::Proved),
            "test_sub should be Proved, got: {:?}",
            test_sub.status
        );

        let test_bad = summary
            .theorems
            .iter()
            .find(|t| t.theorem_name == "test_bad")
            .unwrap();
        assert!(
            matches!(&test_bad.status, ProofStatus::Failed { .. }),
            "test_bad should be Failed, got: {:?}",
            test_bad.status
        );
    }

    #[test]
    fn parse_verify_results_termination_failure() {
        let manifest = make_manifest(vec![("my_module", "test_add", "test_add")]);
        let raw = VerifyResult {
            success: false,
            stdout: String::new(),
            stderr: "error: 'test_add_alwaysTerminating' tactic failed".to_string(),
            exit_code: Some(1),
            theorem_results: None,
        };

        let summary = parse_verify_results(raw, &manifest);

        assert_eq!(summary.total, 2);

        let term = summary
            .theorems
            .iter()
            .find(|t| t.theorem_name == "test_add_alwaysTerminating")
            .unwrap();
        assert!(
            matches!(&term.status, ProofStatus::Failed { .. }),
            "Termination theorem should be Failed, got: {:?}",
            term.status
        );
    }

    #[test]
    fn parse_verify_results_empty_manifest() {
        let manifest = make_manifest(vec![]);
        let raw = VerifyResult {
            success: true,
            stdout: String::new(),
            stderr: String::new(),
            exit_code: Some(0),
            theorem_results: None,
        };

        let summary = parse_verify_results(raw, &manifest);

        assert_eq!(summary.total, 0);
        assert_eq!(summary.proved, 0);
        assert_eq!(summary.failed, 0);
        assert_eq!(summary.timed_out, 0);
        assert_eq!(summary.unknown, 0);
    }

    #[test]
    fn parse_verify_results_build_failure_no_specific_errors() {
        let manifest = make_manifest(vec![("my_module", "test_add", "test_add")]);
        let raw = VerifyResult {
            success: false,
            stdout: String::new(),
            stderr: "error: build failed with some generic message".to_string(),
            exit_code: Some(1),
            theorem_results: None,
        };

        let summary = parse_verify_results(raw, &manifest);

        assert_eq!(summary.total, 2);
        assert_eq!(summary.unknown, 0);
        assert_eq!(summary.failed, 2);
        assert!(summary.theorems.iter().all(|t| matches!(
            t.status,
            ProofStatus::Failed {
                category: FailureCategory::BuildError,
                ..
            }
        )));
    }

    #[test]
    fn parse_verify_results_timeout_marker_marks_timed_out() {
        let manifest = make_manifest(vec![("my_module", "test_add", "test_add")]);
        let raw = VerifyResult {
            success: false,
            stdout: String::new(),
            stderr: "[verify-timeout] command timed out".to_string(),
            exit_code: Some(1),
            theorem_results: None,
        };

        let summary = parse_verify_results(raw, &manifest);

        assert_eq!(summary.total, 2);
        assert_eq!(summary.timed_out, 2);
        assert_eq!(summary.failed, 0);
        assert_eq!(summary.unknown, 0);
    }

    #[test]
    fn lake_fetch_unknown_command_variants_are_detected() {
        assert!(lake_fetch_is_unknown_command(
            "error: unknown command 'fetch'"
        ));
        assert!(lake_fetch_is_unknown_command(
            "error: unknown command `fetch`"
        ));
        assert!(lake_fetch_is_unknown_command(
            "error: unknown command \"fetch\""
        ));
    }

    #[test]
    fn lake_fetch_unknown_command_ignores_other_failures() {
        assert!(!lake_fetch_is_unknown_command(
            "error: failed to load manifest from lakefile.lean"
        ));
        assert!(!lake_fetch_is_unknown_command(
            "error: dependency resolution failed"
        ));
    }

    #[test]
    fn extract_error_for_theorem_includes_context_window() {
        let stderr = "line1: unrelated\nerror: 'my_thm' unsolved goals\nline3: also unrelated\nmy_thm failed to synthesize\n";
        let result = extract_error_for_theorem("my_thm", stderr);
        // Context window includes surrounding lines around matches
        assert!(
            result.contains("error: 'my_thm' unsolved goals"),
            "Should include the error line, got:\n{result}"
        );
        assert!(
            result.contains("my_thm failed to synthesize"),
            "Should include the theorem-matching line, got:\n{result}"
        );
    }

    #[test]
    fn extract_error_for_theorem_shows_gap_marker() {
        // Matches far apart should have "..." separator
        let mut lines: Vec<String> = (0..20).map(|i| format!("filler line {i}")).collect();
        lines[2] = "error: 'my_thm' failed".to_string();
        lines[18] = "my_thm: another error here".to_string();
        let stderr = lines.join("\n");
        let result = extract_error_for_theorem("my_thm", &stderr);
        assert!(
            result.contains("..."),
            "Should have gap marker between distant matches, got:\n{result}"
        );
    }

    #[test]
    fn extract_error_for_theorem_empty_on_no_match() {
        let stderr = "completely unrelated output\nnothing to see here\n";
        let result = extract_error_for_theorem("my_thm", stderr);
        assert!(
            result.is_empty(),
            "Should be empty when no match, got: {result}"
        );
    }

    #[test]
    fn version_meets_minimum_exact_match() {
        assert!(version_meets_minimum("4.24.0", (4, 24, 0)));
    }

    #[test]
    fn version_meets_minimum_higher() {
        assert!(version_meets_minimum("4.25.1", (4, 24, 0)));
    }

    #[test]
    fn version_meets_minimum_lower() {
        assert!(!version_meets_minimum("4.23.9", (4, 24, 0)));
    }

    #[test]
    fn version_meets_minimum_major_higher() {
        assert!(version_meets_minimum("5.0.0", (4, 24, 0)));
    }

    #[test]
    fn parse_version_from_lean_output() {
        let output = "Lean (version 4.24.0, x86_64-unknown-linux-gnu, Release)";
        assert_eq!(
            parse_version_from_output(output),
            Some("4.24.0".to_string())
        );
    }

    #[test]
    fn parse_version_from_z3_output() {
        let output = "Z3 version 4.13.4 - 64 bit";
        assert_eq!(
            parse_version_from_output(output),
            Some("4.13.4".to_string())
        );
    }

    #[test]
    fn parse_version_from_empty_output() {
        assert_eq!(parse_version_from_output("no version here"), None);
    }

    #[test]
    fn generate_lakefile_uses_blaster_rev() {
        let lakefile = generate_lakefile("abc123def");
        assert!(
            lakefile.contains(r#"@ "abc123def""#),
            "Lakefile should pin Blaster to the given rev, got:\n{lakefile}"
        );
        assert!(
            !lakefile.contains(r#"@ "main""#),
            "Lakefile should not contain 'main' when rev is overridden"
        );
    }

    #[test]
    fn generate_lakefile_default_rev() {
        let lakefile = generate_lakefile(DEFAULT_BLASTER_REV);
        assert!(
            lakefile.contains(&format!(r#"@ "{DEFAULT_BLASTER_REV}""#)),
            "Lakefile should use default rev"
        );
    }

    #[test]
    fn check_plutus_core_missing_dir() {
        let tmp = tempfile::tempdir().unwrap();
        let result = check_plutus_core(tmp.path());
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("not found"));
    }

    #[test]
    fn check_plutus_core_empty_dir() {
        let tmp = tempfile::tempdir().unwrap();
        fs::create_dir(tmp.path().join("PlutusCore")).unwrap();
        let result = check_plutus_core(tmp.path());
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("lakefile.lean"));
    }

    #[test]
    fn check_plutus_core_valid_dir() {
        let tmp = tempfile::tempdir().unwrap();
        let pc = tmp.path().join("PlutusCore");
        fs::create_dir(&pc).unwrap();
        fs::write(pc.join("lakefile.lean"), "-- placeholder").unwrap();
        let result = check_plutus_core(tmp.path());
        assert!(result.is_ok());
    }

    #[test]
    fn check_plutus_core_detailed_missing() {
        let tmp = tempfile::tempdir().unwrap();
        let report = check_plutus_core_detailed(tmp.path());
        assert!(!report.found);
        assert!(!report.has_lakefile);
        assert!(report.error.is_some());
    }

    #[test]
    fn check_plutus_core_detailed_valid() {
        let tmp = tempfile::tempdir().unwrap();
        let pc = tmp.path().join("PlutusCore");
        fs::create_dir(&pc).unwrap();
        fs::write(pc.join("lakefile.lean"), "-- placeholder").unwrap();
        let report = check_plutus_core_detailed(tmp.path());
        assert!(report.found);
        assert!(report.has_lakefile);
        assert!(report.error.is_none());
    }

    #[test]
    fn doctor_report_serializes_to_json() {
        let report = DoctorReport {
            tools: vec![ToolCheck {
                tool: "lean".to_string(),
                found: true,
                version: Some("4.24.0".to_string()),
                meets_minimum: true,
                minimum_version: "4.24.0".to_string(),
                error: None,
            }],
            plutus_core: PlutusCoreCheck {
                found: true,
                path: "/tmp/PlutusCore".to_string(),
                has_lakefile: true,
                error: None,
            },
            blaster_rev: "main".to_string(),
            all_ok: true,
            capabilities: capabilities(),
        };
        let json = serde_json::to_string(&report).unwrap();
        assert!(json.contains("\"all_ok\":true"));
        assert!(json.contains("\"blaster_rev\":\"main\""));
        assert!(json.contains("\"supported_test_kinds\""));
        assert!(json.contains("\"target_modes\""));
    }

    #[test]
    fn capabilities_includes_expected_fields() {
        let caps = capabilities();
        assert_eq!(caps.supported_test_kinds, vec!["property"]);
        assert_eq!(caps.unsupported_test_kinds.len(), 2);
        assert_eq!(caps.unsupported_test_kinds[0].kind, "unit");
        assert_eq!(caps.unsupported_test_kinds[0].status, "intentional");
        assert_eq!(caps.unsupported_test_kinds[1].kind, "benchmark");
        assert_eq!(caps.target_modes.len(), 3);
        assert!(caps.target_modes.contains(&"property".to_string()));
        assert!(caps.target_modes.contains(&"validator".to_string()));
        assert!(caps.target_modes.contains(&"equivalence".to_string()));
        assert_eq!(caps.max_test_arity, 1);
    }

    #[test]
    fn validator_target_mode_errors_without_metadata() {
        let test = make_test("my_module", "test_val");
        let id = test_id("my_module", "test_val");
        let lean_name = sanitize_lean_name("test_val");
        let lean_module = "AikenVerify.Proofs.My_module.test_val";

        let result = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::default(),
            &VerificationTargetKind::ValidatorHandler,
        );
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(
            err.contains("no validator target metadata"),
            "Should error about missing validator_target, got: {err}"
        );
    }

    #[test]
    fn validator_target_mode_uses_handler_prog() {
        let mut test = make_test("my_module", "test_val");
        test.target_kind = VerificationTargetKind::ValidatorHandler;
        test.validator_target = Some(ValidatorTarget {
            validator_module: "validators/my_validator".to_string(),
            validator_name: "spend".to_string(),
            handler_name: Some("spend.handler".to_string()),
            handler_program: Some(ExportedProgram {
                hex: "handler_hex".to_string(),
                flat_bytes: None,
            }),
        });
        let id = test_id("my_module", "test_val");
        let lean_name = sanitize_lean_name("test_val");
        let lean_module = "AikenVerify.Proofs.My_module.test_val";

        let proof = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::default(),
            &VerificationTargetKind::ValidatorHandler,
        )
        .unwrap();

        assert!(
            proof.contains("handler_prog_"),
            "Validator mode should use handler_prog in theorems, got:\n{proof}"
        );
        assert!(
            proof.contains("_handler.flat"),
            "Should import handler flat file, got:\n{proof}"
        );
    }

    #[test]
    fn equivalence_target_mode_generates_equivalence_theorem() {
        let mut test = make_test("my_module", "test_eq");
        test.target_kind = VerificationTargetKind::Equivalence;
        test.validator_target = Some(ValidatorTarget {
            validator_module: "validators/my_validator".to_string(),
            validator_name: "spend".to_string(),
            handler_name: None,
            handler_program: Some(ExportedProgram {
                hex: "handler_hex".to_string(),
                flat_bytes: None,
            }),
        });
        let id = test_id("my_module", "test_eq");
        let lean_name = sanitize_lean_name("test_eq");
        let lean_module = "AikenVerify.Proofs.My_module.test_eq";

        let proof = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::default(),
            &VerificationTargetKind::Equivalence,
        )
        .unwrap();

        // Should have the standard property theorem
        assert!(
            proof.contains("theorem test_eq :"),
            "Should have standard property theorem, got:\n{proof}"
        );
        // Should have the equivalence theorem
        assert!(
            proof.contains("theorem test_eq_equivalence :"),
            "Should have equivalence theorem, got:\n{proof}"
        );
        assert!(
            proof.contains("proveTests prog_"),
            "Equivalence should reference property prog, got:\n{proof}"
        );
        assert!(
            proof.contains("proveTests handler_prog_"),
            "Equivalence should reference handler prog, got:\n{proof}"
        );
        // Both flat imports should be present
        assert!(
            proof.contains("_handler.flat"),
            "Should import handler flat file, got:\n{proof}"
        );
    }

    #[test]
    fn verification_target_kind_parse_roundtrip() {
        assert_eq!(
            "property".parse::<VerificationTargetKind>().unwrap(),
            VerificationTargetKind::PropertyWrapper
        );
        assert_eq!(
            "validator".parse::<VerificationTargetKind>().unwrap(),
            VerificationTargetKind::ValidatorHandler
        );
        assert_eq!(
            "equivalence".parse::<VerificationTargetKind>().unwrap(),
            VerificationTargetKind::Equivalence
        );
        assert!("invalid".parse::<VerificationTargetKind>().is_err());
    }

    #[test]
    fn verification_target_kind_display() {
        assert_eq!(
            VerificationTargetKind::PropertyWrapper.to_string(),
            "property"
        );
        assert_eq!(
            VerificationTargetKind::ValidatorHandler.to_string(),
            "validator"
        );
        assert_eq!(
            VerificationTargetKind::Equivalence.to_string(),
            "equivalence"
        );
    }

    #[test]
    fn validate_blaster_rev_accepts_valid_inputs() {
        assert!(validate_blaster_rev("main").is_ok());
        assert!(validate_blaster_rev("abc123def").is_ok());
        assert!(validate_blaster_rev("v1.2.3").is_ok());
        assert!(validate_blaster_rev("feature/my-branch").is_ok());
        assert!(validate_blaster_rev("my_tag_1.0").is_ok());
    }

    #[test]
    fn validate_blaster_rev_rejects_empty() {
        assert!(validate_blaster_rev("").is_err());
    }

    #[test]
    fn validate_blaster_rev_rejects_injection() {
        assert!(validate_blaster_rev("main\" ; rm -rf /").is_err());
        assert!(validate_blaster_rev("rev\nmalicious").is_err());
        assert!(validate_blaster_rev("rev`cmd`").is_err());
    }

    #[test]
    fn collision_detector_passes_distinct_tests() {
        let tests = vec![
            make_test("module_a", "test_x"),
            make_test("module_b", "test_x"),
            make_test("module_a", "test_y"),
        ];
        assert!(detect_lean_path_collisions(&tests).is_ok());
    }

    #[test]
    fn collision_detector_catches_duplicate_test() {
        // Exact same module+name should collide
        let tests = vec![
            make_test("module_a", "test_x"),
            make_test("module_a", "test_x"),
        ];
        let err = detect_lean_path_collisions(&tests).unwrap_err();
        let msg = err.to_string();
        assert!(
            msg.contains("collision"),
            "Error should mention collision: {msg}"
        );
        assert!(
            msg.contains("module_a.test_x"),
            "Error should name the test: {msg}"
        );
    }

    #[test]
    fn collision_detector_catches_sanitization_collision() {
        // my-module and my_module both sanitize to My_module in Lean paths
        let tests = vec![
            make_test("my-module", "test_foo"),
            make_test("my_module", "test_foo"),
        ];
        let err = detect_lean_path_collisions(&tests).unwrap_err();
        let msg = err.to_string();
        assert!(
            msg.contains("collision"),
            "Should detect sanitization collision: {msg}"
        );
        assert!(
            msg.contains("my-module.test_foo"),
            "Should name first test: {msg}"
        );
        assert!(
            msg.contains("my_module.test_foo"),
            "Should name second test: {msg}"
        );
    }

    #[test]
    fn short_hash_is_deterministic() {
        assert_eq!(short_hash("hello"), short_hash("hello"));
    }

    #[test]
    fn short_hash_differs_for_different_inputs() {
        assert_ne!(short_hash("hello"), short_hash("world"));
    }

    #[test]
    fn classify_failure_counterexample() {
        assert_eq!(
            classify_failure("❌ Falsified after 3 tests"),
            FailureCategory::Counterexample
        );
        assert_eq!(
            classify_failure("Counterexample: x = 42"),
            FailureCategory::Counterexample
        );
    }

    #[test]
    fn classify_failure_unsat_goal() {
        assert_eq!(
            classify_failure("Tactic `blaster` failed, unsolved goals remain"),
            FailureCategory::UnsatGoal
        );
        assert_eq!(
            classify_failure("error: unsolved goals\n⊢ False"),
            FailureCategory::UnsatGoal
        );
    }

    #[test]
    fn classify_failure_timeout() {
        assert_eq!(
            classify_failure("deterministic timeout reached"),
            FailureCategory::Timeout
        );
    }

    #[test]
    fn classify_failure_dependency() {
        assert_eq!(
            classify_failure("error: unknown package 'PlutusCore'\nlake fetch failed"),
            FailureCategory::DependencyError
        );
    }

    #[test]
    fn classify_failure_blaster_unsupported() {
        assert_eq!(
            classify_failure(
                "error: translateApp: Inductive predicate not yet supported: Lean.Expr.const `List.Mem [Lean.Level.zero]"
            ),
            FailureCategory::BlasterUnsupported
        );
    }

    #[test]
    fn classify_failure_build_error() {
        assert_eq!(
            classify_failure("error: Lean exited with code 1"),
            FailureCategory::BuildError
        );
    }

    #[test]
    fn classify_failure_unknown() {
        assert_eq!(
            classify_failure("some totally unrecognized output"),
            FailureCategory::Unknown
        );
    }

    #[test]
    fn parse_verify_results_failure_includes_category() {
        let manifest = make_manifest(vec![("my_module", "test_add", "test_add")]);
        let raw = VerifyResult {
            success: false,
            stdout: String::new(),
            stderr: "error: 'test_add' unsolved goals\nTactic `blaster` failed".to_string(),
            exit_code: Some(1),
            theorem_results: None,
        };

        let summary = parse_verify_results(raw, &manifest);
        let test_add = summary
            .theorems
            .iter()
            .find(|t| t.theorem_name == "test_add")
            .unwrap();
        match &test_add.status {
            ProofStatus::Failed { category, .. } => {
                assert_eq!(
                    *category,
                    FailureCategory::UnsatGoal,
                    "Should classify as UnsatGoal"
                );
            }
            other => panic!("Expected Failed, got: {:?}", other),
        }
    }

    #[test]
    fn skip_unsupported_collects_skipped_tests() {
        let tmp = tempfile::tempdir().unwrap();
        let config = VerifyConfig {
            out_dir: tmp.path().to_path_buf(),
            cek_budget: 20000,
            blaster_rev: DEFAULT_BLASTER_REV.to_string(),
            existential_mode: ExistentialMode::default(),
            target: VerificationTargetKind::default(),
        };

        // Nested composite types remain unsupported for proof generation.
        let mut unsupported = make_test("my_module", "test_list");
        unsupported.fuzzer_output_type = FuzzerOutputType::Tuple(vec![
            FuzzerOutputType::List(Box::new(FuzzerOutputType::Int)),
            FuzzerOutputType::Data,
        ]);
        unsupported.constraint = FuzzerConstraint::Any;

        let manifest = generate_lean_workspace(&[unsupported], &config, true).unwrap();
        assert!(
            manifest.tests.is_empty(),
            "Unsupported test should not appear in tests"
        );
        assert_eq!(manifest.skipped.len(), 1);
        assert_eq!(manifest.skipped[0].name, "my_module.test_list");
        assert!(
            manifest.skipped[0].reason.contains("List"),
            "Skip reason should mention List: {}",
            manifest.skipped[0].reason
        );
    }

    #[test]
    fn skip_unsupported_false_errors_on_unsupported() {
        let tmp = tempfile::tempdir().unwrap();
        let config = VerifyConfig {
            out_dir: tmp.path().to_path_buf(),
            cek_budget: 20000,
            blaster_rev: DEFAULT_BLASTER_REV.to_string(),
            existential_mode: ExistentialMode::default(),
            target: VerificationTargetKind::default(),
        };

        let mut unsupported = make_test("my_module", "test_list");
        unsupported.fuzzer_output_type = FuzzerOutputType::Tuple(vec![
            FuzzerOutputType::List(Box::new(FuzzerOutputType::Int)),
            FuzzerOutputType::Data,
        ]);
        unsupported.constraint = FuzzerConstraint::Any;

        let result = generate_lean_workspace(&[unsupported], &config, false);
        assert!(
            result.is_err(),
            "skip_unsupported=false should error on unsupported type"
        );
    }

    #[test]
    fn skip_unsupported_mixed_generates_supported_and_skips_rest() {
        let tmp = tempfile::tempdir().unwrap();
        let config = VerifyConfig {
            out_dir: tmp.path().to_path_buf(),
            cek_budget: 20000,
            blaster_rev: DEFAULT_BLASTER_REV.to_string(),
            existential_mode: ExistentialMode::default(),
            target: VerificationTargetKind::default(),
        };

        let good = make_test("my_module", "test_int");
        let mut bad = make_test("my_module", "test_list");
        bad.fuzzer_output_type = FuzzerOutputType::Tuple(vec![
            FuzzerOutputType::List(Box::new(FuzzerOutputType::Int)),
            FuzzerOutputType::Data,
        ]);
        bad.constraint = FuzzerConstraint::Any;

        let manifest = generate_lean_workspace(&[good, bad], &config, true).unwrap();
        assert_eq!(manifest.tests.len(), 1, "Should have 1 supported test");
        assert_eq!(manifest.skipped.len(), 1, "Should have 1 skipped test");
        assert_eq!(manifest.tests[0].aiken_name, "test_int");
        assert_eq!(manifest.skipped[0].name, "my_module.test_list");
    }

    // --- Step 3.1 tests: String and Pair ---

    #[test]
    fn string_generates_theorem_without_bounds() {
        let test = make_test_with_type(
            "my_module",
            "test_string",
            FuzzerOutputType::String,
            FuzzerConstraint::Any,
        );
        let id = test_id("my_module", "test_string");
        let lean_name = sanitize_lean_name("test_string");
        let lean_module = "AikenVerify.Proofs.My_module.test_string";

        let proof = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::default(),
            &VerificationTargetKind::default(),
        )
        .unwrap();

        assert!(
            proof.contains("∀ (x : ByteString),"),
            "String theorem should quantify over ByteString, got:\n{proof}"
        );
        assert!(
            proof.contains("stringArg x"),
            "String theorem should use stringArg, got:\n{proof}"
        );
        assert!(
            !proof.contains("<="),
            "String theorem should not rely on integer bounds, got:\n{proof}"
        );
    }

    #[test]
    fn pair_data_data_generates_theorem() {
        let test = make_test_with_type(
            "my_module",
            "test_pair_dd",
            FuzzerOutputType::Pair(
                Box::new(FuzzerOutputType::Data),
                Box::new(FuzzerOutputType::Data),
            ),
            FuzzerConstraint::Any,
        );
        let id = test_id("my_module", "test_pair_dd");
        let lean_name = sanitize_lean_name("test_pair_dd");
        let lean_module = "AikenVerify.Proofs.My_module.test_pair_dd";

        let proof = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::default(),
            &VerificationTargetKind::default(),
        )
        .unwrap();

        assert!(
            proof.contains("∀ (a : Data) (b : Data),"),
            "Pair(Data,Data) should quantify over two Data vars, got:\n{proof}"
        );
        assert!(
            proof.contains("Data.List [a, b]"),
            "Pair(Data,Data) should encode as Data.List, got:\n{proof}"
        );
        assert!(proof.contains("= true"));
    }

    #[test]
    fn pair_int_data_generates_theorem_with_bounds() {
        let test = make_test_with_type(
            "my_module",
            "test_pair_id",
            FuzzerOutputType::Pair(
                Box::new(FuzzerOutputType::Int),
                Box::new(FuzzerOutputType::Data),
            ),
            FuzzerConstraint::Tuple(vec![
                FuzzerConstraint::IntRange {
                    min: "-10".to_string(),
                    max: "10".to_string(),
                },
                FuzzerConstraint::Any,
            ]),
        );
        let id = test_id("my_module", "test_pair_id");
        let lean_name = sanitize_lean_name("test_pair_id");
        let lean_module = "AikenVerify.Proofs.My_module.test_pair_id";

        let proof = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::default(),
            &VerificationTargetKind::default(),
        )
        .unwrap();

        assert!(
            proof.contains("∀ (a : Integer) (b : Data),"),
            "Pair(Int,Data) should quantify over Integer and Data, got:\n{proof}"
        );
        assert!(
            proof.contains("(-10 <= a && a <= 10)"),
            "Should have bounds for Int element, got:\n{proof}"
        );
        assert!(
            proof.contains("Data.List [Data.I a, b]"),
            "Should encode pair with Int and Data, got:\n{proof}"
        );
    }

    // --- Step 3.2 tests: Generic tuple arities ---

    #[test]
    fn tuple_arity_4_generates_theorem() {
        let test = make_test_with_type(
            "my_module",
            "test_quad",
            FuzzerOutputType::Tuple(vec![
                FuzzerOutputType::Data,
                FuzzerOutputType::Int,
                FuzzerOutputType::Bool,
                FuzzerOutputType::ByteArray,
            ]),
            FuzzerConstraint::Tuple(vec![
                FuzzerConstraint::Any,
                FuzzerConstraint::IntRange {
                    min: "0".to_string(),
                    max: "99".to_string(),
                },
                FuzzerConstraint::Any,
                FuzzerConstraint::Any,
            ]),
        );
        let id = test_id("my_module", "test_quad");
        let lean_name = sanitize_lean_name("test_quad");
        let lean_module = "AikenVerify.Proofs.My_module.test_quad";

        let proof = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::default(),
            &VerificationTargetKind::default(),
        )
        .unwrap();

        assert!(
            proof.contains("∀ (a : Data) (b : Integer) (c : Bool) (d : ByteString),"),
            "4-tuple should quantify over four vars, got:\n{proof}"
        );
        assert!(
            proof.contains("(0 <= b && b <= 99)"),
            "Should have bounds for Int element b, got:\n{proof}"
        );
        assert!(
            proof.contains(
                "Data.List [a, Data.I b, (Data.Constr (if c then 1 else 0) []), Data.B d]"
            ),
            "Should inline all element encoders, got:\n{proof}"
        );
    }

    #[test]
    fn tuple_arity_5_all_data_generates_theorem() {
        let test = make_test_with_type(
            "my_module",
            "test_quint",
            FuzzerOutputType::Tuple(vec![
                FuzzerOutputType::Data,
                FuzzerOutputType::Data,
                FuzzerOutputType::Data,
                FuzzerOutputType::Data,
                FuzzerOutputType::Data,
            ]),
            FuzzerConstraint::Any,
        );
        let id = test_id("my_module", "test_quint");
        let lean_name = sanitize_lean_name("test_quint");
        let lean_module = "AikenVerify.Proofs.My_module.test_quint";

        let proof = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::default(),
            &VerificationTargetKind::default(),
        )
        .unwrap();

        assert!(
            proof.contains("∀ (a : Data) (b : Data) (c : Data) (d : Data) (e : Data),"),
            "5-tuple should quantify over five vars, got:\n{proof}"
        );
        assert!(
            proof.contains("Data.List [a, b, c, d, e]"),
            "Should encode as Data.List with five elements, got:\n{proof}"
        );
        assert!(
            !proof.contains("<="),
            "All-Data tuple should have no bounds"
        );
    }

    // --- Step 3.3 tests: List with bounds ---

    #[test]
    fn list_with_bounds_generates_theorem() {
        let test = make_test_with_type(
            "my_module",
            "test_list_bounded",
            FuzzerOutputType::List(Box::new(FuzzerOutputType::Int)),
            FuzzerConstraint::List {
                elem: Box::new(FuzzerConstraint::IntRange {
                    min: "0".to_string(),
                    max: "10".to_string(),
                }),
                min_len: Some(0),
                max_len: Some(10),
            },
        );
        let id = test_id("my_module", "test_list_bounded");
        let lean_name = sanitize_lean_name("test_list_bounded");
        let lean_module = "AikenVerify.Proofs.My_module.test_list_bounded";

        let proof = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::default(),
            &VerificationTargetKind::default(),
        )
        .unwrap();

        assert!(
            proof.contains("∀ (xs : List Integer),"),
            "List<Int> should quantify over List Integer, got:\n{proof}"
        );
        assert!(
            proof.contains("(0 <= xs.length && xs.length <= 10)"),
            "Should have length bounds, got:\n{proof}"
        );
        assert!(
            proof.contains("xs.all (fun x_i => 0 <= x_i && x_i <= 10)"),
            "Should use List.all for element bounds, got:\n{proof}"
        );
        assert!(
            !proof.contains("∀ x_i ∈ xs"),
            "Should avoid List.Mem-style quantification in generated bounds, got:\n{proof}"
        );
        assert!(
            proof.contains("Data.I x_i"),
            "Should encode elements as Data.I, got:\n{proof}"
        );
        assert!(proof.contains("xs.map"));
    }

    #[test]
    fn list_data_with_bounds_generates_theorem() {
        let test = make_test_with_type(
            "my_module",
            "test_list_data",
            FuzzerOutputType::List(Box::new(FuzzerOutputType::Data)),
            FuzzerConstraint::List {
                elem: Box::new(FuzzerConstraint::Any),
                min_len: Some(1),
                max_len: Some(5),
            },
        );
        let id = test_id("my_module", "test_list_data");
        let lean_name = sanitize_lean_name("test_list_data");
        let lean_module = "AikenVerify.Proofs.My_module.test_list_data";

        let proof = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::default(),
            &VerificationTargetKind::default(),
        )
        .unwrap();

        assert!(
            proof.contains("∀ (xs : List Data),"),
            "List<Data> should quantify over List Data, got:\n{proof}"
        );
        assert!(
            proof.contains("(1 <= xs.length && xs.length <= 5)"),
            "Should have length bounds, got:\n{proof}"
        );
    }

    #[test]
    fn list_bounds_can_be_extracted_through_and_constraint() {
        let test = make_test_with_type(
            "my_module",
            "test_list_and",
            FuzzerOutputType::List(Box::new(FuzzerOutputType::Data)),
            FuzzerConstraint::And(vec![
                FuzzerConstraint::Any,
                FuzzerConstraint::List {
                    elem: Box::new(FuzzerConstraint::Any),
                    min_len: Some(1),
                    max_len: Some(5),
                },
            ]),
        );
        let id = test_id("my_module", "test_list_and");
        let lean_name = sanitize_lean_name("test_list_and");
        let lean_module = "AikenVerify.Proofs.My_module.test_list_and";

        let proof = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::default(),
            &VerificationTargetKind::default(),
        )
        .unwrap();

        assert!(
            proof.contains("(1 <= xs.length && xs.length <= 5)"),
            "Should extract list bounds through And constraint, got:\n{proof}"
        );
    }

    #[test]
    fn list_missing_max_len_generates_one_sided_length_precondition() {
        let test = make_test_with_type(
            "my_module",
            "test_list_nomax",
            FuzzerOutputType::List(Box::new(FuzzerOutputType::Int)),
            FuzzerConstraint::List {
                elem: Box::new(FuzzerConstraint::Any),
                min_len: Some(0),
                max_len: None,
            },
        );
        let id = test_id("my_module", "test_list_nomax");
        let lean_name = sanitize_lean_name("test_list_nomax");
        let lean_module = "AikenVerify.Proofs.My_module.test_list_nomax";

        let proof = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::default(),
            &VerificationTargetKind::default(),
        )
        .unwrap();
        assert!(
            proof.contains("(0 <= xs.length)"),
            "Should emit lower-bound-only length precondition, got:\n{proof}"
        );
        assert!(
            !proof.contains("xs.length <= "),
            "Should not emit upper length bound when max_len is missing, got:\n{proof}"
        );
    }

    // --- Step 3.4 tests: ADT fallback via Data encoding ---

    #[test]
    fn unsupported_type_falls_back_to_data() {
        let test = make_test_with_type(
            "my_module",
            "test_adt",
            FuzzerOutputType::Unsupported("MyCustomType".to_string()),
            FuzzerConstraint::Any,
        );
        let id = test_id("my_module", "test_adt");
        let lean_name = sanitize_lean_name("test_adt");
        let lean_module = "AikenVerify.Proofs.My_module.test_adt";

        let proof = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::default(),
            &VerificationTargetKind::default(),
        )
        .unwrap();

        assert!(
            proof.contains("∀ (x : Data),"),
            "Unsupported ADT should fall back to Data quantification, got:\n{proof}"
        );
        assert!(
            proof.contains("dataArg x"),
            "Unsupported ADT should use dataArg, got:\n{proof}"
        );
    }

    #[test]
    fn unsupported_type_in_tuple_falls_back_to_data() {
        let test = make_test_with_type(
            "my_module",
            "test_tuple_adt",
            FuzzerOutputType::Tuple(vec![
                FuzzerOutputType::Int,
                FuzzerOutputType::Unsupported("MyCustomType".to_string()),
            ]),
            FuzzerConstraint::Tuple(vec![
                FuzzerConstraint::IntRange {
                    min: "0".to_string(),
                    max: "50".to_string(),
                },
                FuzzerConstraint::Any,
            ]),
        );
        let id = test_id("my_module", "test_tuple_adt");
        let lean_name = sanitize_lean_name("test_tuple_adt");
        let lean_module = "AikenVerify.Proofs.My_module.test_tuple_adt";

        let proof = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::default(),
            &VerificationTargetKind::default(),
        )
        .unwrap();

        assert!(
            proof.contains("∀ (a : Integer) (b : Data),"),
            "Tuple(Int, ADT) should map ADT to Data, got:\n{proof}"
        );
        assert!(
            proof.contains("(0 <= a && a <= 50)"),
            "Should have Int bounds, got:\n{proof}"
        );
        assert!(
            proof.contains("Data.List [Data.I a, b]"),
            "Should encode ADT element as plain Data var, got:\n{proof}"
        );
    }

    // --- requires_explicit_bounds extended tests ---

    #[test]
    fn requires_explicit_bounds_list_needs_extractable_domain() {
        assert!(requires_explicit_bounds(
            &FuzzerOutputType::List(Box::new(FuzzerOutputType::Int)),
            &FuzzerConstraint::Any,
        ));
        assert!(!requires_explicit_bounds(
            &FuzzerOutputType::List(Box::new(FuzzerOutputType::Int)),
            &FuzzerConstraint::List {
                elem: Box::new(FuzzerConstraint::Any),
                min_len: Some(0),
                max_len: Some(10),
            },
        ));
    }

    #[test]
    fn requires_explicit_bounds_pair_with_int() {
        // Pair(Int, Data) with Any constraint => needs bounds
        assert!(requires_explicit_bounds(
            &FuzzerOutputType::Pair(
                Box::new(FuzzerOutputType::Int),
                Box::new(FuzzerOutputType::Data),
            ),
            &FuzzerConstraint::Any,
        ));
        // Pair(Data, Data) never needs bounds
        assert!(!requires_explicit_bounds(
            &FuzzerOutputType::Pair(
                Box::new(FuzzerOutputType::Data),
                Box::new(FuzzerOutputType::Data),
            ),
            &FuzzerConstraint::Any,
        ));
    }

    #[test]
    fn requires_explicit_bounds_generic_tuple_with_int() {
        // Tuple(Int, Data, Bool) with Int needs bounds
        assert!(requires_explicit_bounds(
            &FuzzerOutputType::Tuple(vec![
                FuzzerOutputType::Int,
                FuzzerOutputType::Data,
                FuzzerOutputType::Bool,
            ]),
            &FuzzerConstraint::Any,
        ));
        // Tuple(Data, Data, Bool) never needs bounds
        assert!(!requires_explicit_bounds(
            &FuzzerOutputType::Tuple(vec![
                FuzzerOutputType::Data,
                FuzzerOutputType::Data,
                FuzzerOutputType::Bool,
            ]),
            &FuzzerConstraint::Any,
        ));
    }

    #[test]
    fn failure_category_serializes_to_json() {
        let result = TheoremResult {
            test_name: "mod.test".to_string(),
            theorem_name: "test".to_string(),
            status: ProofStatus::Failed {
                category: FailureCategory::Counterexample,
                reason: "x = 42".to_string(),
            },
        };
        let json = serde_json::to_string(&result).unwrap();
        assert!(
            json.contains("\"category\":\"Counterexample\""),
            "JSON should include category: {json}"
        );
        assert!(
            json.contains("\"reason\":\"x = 42\""),
            "JSON should include reason: {json}"
        );
    }

    // --- ArtifactRetention tests ---

    #[test]
    fn artifact_retention_parse_roundtrip() {
        for s in &["on-failure", "on-success", "always", "never"] {
            let parsed: ArtifactRetention = s.parse().unwrap();
            assert_eq!(parsed.to_string(), *s);
        }
    }

    #[test]
    fn artifact_retention_parse_rejects_unknown() {
        assert!("bogus".parse::<ArtifactRetention>().is_err());
    }

    #[test]
    fn artifact_retention_default_is_on_failure() {
        assert_eq!(ArtifactRetention::default(), ArtifactRetention::OnFailure);
    }

    #[test]
    fn should_retain_on_failure_keeps_on_fail() {
        assert!(should_retain_artifacts(ArtifactRetention::OnFailure, false));
        assert!(!should_retain_artifacts(ArtifactRetention::OnFailure, true));
    }

    #[test]
    fn should_retain_on_success_keeps_on_pass() {
        assert!(!should_retain_artifacts(
            ArtifactRetention::OnSuccess,
            false
        ));
        assert!(should_retain_artifacts(ArtifactRetention::OnSuccess, true));
    }

    #[test]
    fn should_retain_always_keeps_everything() {
        assert!(should_retain_artifacts(ArtifactRetention::Always, false));
        assert!(should_retain_artifacts(ArtifactRetention::Always, true));
    }

    #[test]
    fn should_retain_never_keeps_nothing() {
        assert!(!should_retain_artifacts(ArtifactRetention::Never, false));
        assert!(!should_retain_artifacts(ArtifactRetention::Never, true));
    }

    // --- clean_artifacts tests ---

    #[test]
    fn clean_artifacts_removes_existing_dir() {
        let dir = tempfile::tempdir().unwrap();
        let out = dir.path().join("verify_workspace");
        fs::create_dir_all(&out).unwrap();
        fs::write(out.join("test.lean"), "-- test").unwrap();

        let removed = clean_artifacts(&out).unwrap();
        assert_eq!(removed.len(), 1);
        assert!(!out.exists());
    }

    #[test]
    fn clean_artifacts_returns_empty_for_missing_dir() {
        let dir = tempfile::tempdir().unwrap();
        let out = dir.path().join("nonexistent");
        let removed = clean_artifacts(&out).unwrap();
        assert!(removed.is_empty());
    }

    #[test]
    fn clear_generated_workspace_removes_generated_outputs_and_preserves_cache() {
        let dir = tempfile::tempdir().unwrap();
        let out = dir.path();

        fs::create_dir_all(out.join("AikenVerify/Proofs/Stale")).unwrap();
        fs::write(out.join("AikenVerify/Proofs/Stale/old.lean"), "-- stale").unwrap();
        fs::create_dir_all(out.join("flat")).unwrap();
        fs::write(out.join("flat/stale.flat"), "stale").unwrap();
        fs::write(
            out.join("AikenVerify.lean"),
            "import AikenVerify.Proofs.Stale.old",
        )
        .unwrap();
        fs::write(out.join("manifest.json"), "{}").unwrap();
        fs::create_dir_all(out.join("logs")).unwrap();
        fs::write(out.join("logs/stale.log"), "stale").unwrap();
        fs::create_dir_all(out.join(".lake/build/lib/AikenVerify/Stale")).unwrap();
        fs::write(
            out.join(".lake/build/lib/AikenVerify/Stale/stale.olean"),
            "stale",
        )
        .unwrap();

        fs::create_dir_all(out.join("PlutusCore")).unwrap();
        fs::write(out.join("PlutusCore/lakefile.lean"), "-- keep").unwrap();
        fs::create_dir_all(out.join(".lake/packages/Blaster")).unwrap();
        fs::write(out.join(".lake/packages/Blaster/cache.txt"), "keep").unwrap();
        fs::create_dir_all(out.join(".lake/build/lib/PlutusCore")).unwrap();
        fs::write(out.join(".lake/build/lib/PlutusCore/cache.olean"), "keep").unwrap();

        clear_generated_workspace(out).unwrap();

        assert!(!out.join("AikenVerify").exists());
        assert!(!out.join("AikenVerify.lean").exists());
        assert!(!out.join("flat").exists());
        assert!(!out.join("manifest.json").exists());
        assert!(!out.join("logs").exists());
        assert!(!out.join(".lake/build/lib/AikenVerify").exists());

        assert!(out.join("PlutusCore/lakefile.lean").exists());
        assert!(out.join(".lake/packages/Blaster/cache.txt").exists());
        assert!(out.join(".lake/build/lib/PlutusCore/cache.olean").exists());
    }

    // --- capabilities tests ---

    #[test]
    fn capabilities_json_roundtrip() {
        let caps = capabilities();
        let json = serde_json::to_string_pretty(&caps).unwrap();
        // Verify it contains expected top-level fields
        assert!(json.contains("supported_test_kinds"));
        assert!(json.contains("unsupported_test_kinds"));
        assert!(json.contains("target_modes"));
        assert!(json.contains("supported_fuzzer_types"));
        assert!(json.contains("unsupported_fuzzer_types"));
        assert!(json.contains("existential_modes"));
        assert!(json.contains("max_test_arity"));
    }

    #[test]
    fn capabilities_supported_types_include_all_working_types() {
        let caps = capabilities();
        assert!(caps.supported_fuzzer_types.contains(&"Int".to_string()));
        assert!(caps.supported_fuzzer_types.contains(&"Bool".to_string()));
        assert!(caps
            .supported_fuzzer_types
            .contains(&"ByteArray".to_string()));
        assert!(caps.supported_fuzzer_types.contains(&"String".to_string()));
        assert!(caps.supported_fuzzer_types.contains(&"Data".to_string()));
    }

    #[test]
    fn capabilities_unsupported_kinds_include_unit_and_benchmark() {
        let caps = capabilities();
        let kinds: Vec<&str> = caps
            .unsupported_test_kinds
            .iter()
            .map(|n| n.kind.as_str())
            .collect();
        assert!(kinds.contains(&"unit"));
        assert!(kinds.contains(&"benchmark"));
    }

    #[test]
    fn capabilities_existential_modes_include_witness_and_proof() {
        let caps = capabilities();
        assert!(caps.existential_modes.contains(&"witness".to_string()));
        assert!(caps.existential_modes.contains(&"proof".to_string()));
    }

    #[test]
    fn capabilities_max_arity_is_one() {
        assert_eq!(capabilities().max_test_arity, 1);
    }

    // --- Step 11.2: Compatibility fixture suite ---
    //
    // These tests validate the full pipeline (export -> workspace gen -> proof file)
    // for every supported type, expected failure cases, and skip scenarios.
    // They serve as the e2e compatibility regression suite.

    #[test]
    fn compat_all_supported_scalar_types_generate_workspace() {
        let types_and_constraints = vec![
            (
                FuzzerOutputType::Int,
                FuzzerConstraint::IntRange {
                    min: "0".into(),
                    max: "10".into(),
                },
            ),
            (FuzzerOutputType::Bool, FuzzerConstraint::Any),
            (FuzzerOutputType::ByteArray, FuzzerConstraint::Any),
            (FuzzerOutputType::String, FuzzerConstraint::Any),
            (FuzzerOutputType::Data, FuzzerConstraint::Any),
        ];

        for (i, (output_type, constraint)) in types_and_constraints.iter().enumerate() {
            let mut test = make_test("compat", &format!("scalar_{i}"));
            test.fuzzer_output_type = output_type.clone();
            test.constraint = constraint.clone();

            let dir = tempfile::tempdir().unwrap();
            let config = VerifyConfig {
                out_dir: dir.path().to_path_buf(),
                cek_budget: 20000,
                blaster_rev: "main".to_string(),
                existential_mode: ExistentialMode::Witness,
                target: VerificationTargetKind::PropertyWrapper,
            };

            let manifest = generate_lean_workspace(&[test], &config, false);
            assert!(
                manifest.is_ok(),
                "Scalar type {:?} should generate workspace",
                output_type
            );
            assert_eq!(manifest.unwrap().tests.len(), 1);
        }
    }

    #[test]
    fn compat_tuple_and_list_types_generate_workspace() {
        let cases: Vec<(FuzzerOutputType, FuzzerConstraint, &str)> = vec![
            (
                FuzzerOutputType::Tuple(vec![FuzzerOutputType::Int, FuzzerOutputType::Data]),
                FuzzerConstraint::Tuple(vec![
                    FuzzerConstraint::IntRange {
                        min: "0".into(),
                        max: "5".into(),
                    },
                    FuzzerConstraint::Any,
                ]),
                "tuple_int_data",
            ),
            (
                FuzzerOutputType::List(Box::new(FuzzerOutputType::Data)),
                FuzzerConstraint::List {
                    elem: Box::new(FuzzerConstraint::Any),
                    min_len: Some(0),
                    max_len: Some(3),
                },
                "list_data",
            ),
        ];

        for (output_type, constraint, label) in &cases {
            let mut test = make_test("compat", label);
            test.fuzzer_output_type = output_type.clone();
            test.constraint = constraint.clone();

            let dir = tempfile::tempdir().unwrap();
            let config = VerifyConfig {
                out_dir: dir.path().to_path_buf(),
                cek_budget: 20000,
                blaster_rev: "main".to_string(),
                existential_mode: ExistentialMode::Witness,
                target: VerificationTargetKind::PropertyWrapper,
            };

            let manifest = generate_lean_workspace(&[test], &config, false);
            assert!(
                manifest.is_ok(),
                "Composite type {label} should generate workspace"
            );
            assert_eq!(manifest.unwrap().tests.len(), 1);
        }
    }

    #[test]
    fn compat_unsupported_type_fails_without_skip() {
        let mut test = make_test("compat", "nested");
        test.fuzzer_output_type = FuzzerOutputType::Tuple(vec![
            FuzzerOutputType::List(Box::new(FuzzerOutputType::Int)),
            FuzzerOutputType::Data,
        ]);
        test.constraint = FuzzerConstraint::Any;

        let dir = tempfile::tempdir().unwrap();
        let config = VerifyConfig {
            out_dir: dir.path().to_path_buf(),
            cek_budget: 20000,
            blaster_rev: "main".to_string(),
            existential_mode: ExistentialMode::Witness,
            target: VerificationTargetKind::PropertyWrapper,
        };

        let result = generate_lean_workspace(&[test], &config, false);
        assert!(
            result.is_err(),
            "Nested unsupported type should fail without skip"
        );
    }

    #[test]
    fn compat_unsupported_type_skipped_with_flag() {
        let mut test = make_test("compat", "nested");
        test.fuzzer_output_type = FuzzerOutputType::Tuple(vec![
            FuzzerOutputType::List(Box::new(FuzzerOutputType::Int)),
            FuzzerOutputType::Data,
        ]);
        test.constraint = FuzzerConstraint::Any;

        let dir = tempfile::tempdir().unwrap();
        let config = VerifyConfig {
            out_dir: dir.path().to_path_buf(),
            cek_budget: 20000,
            blaster_rev: "main".to_string(),
            existential_mode: ExistentialMode::Witness,
            target: VerificationTargetKind::PropertyWrapper,
        };

        let manifest = generate_lean_workspace(&[test], &config, true).unwrap();
        assert!(manifest.tests.is_empty());
        assert_eq!(manifest.skipped.len(), 1);
        assert!(manifest.skipped[0].reason.contains("nested"));
    }

    #[test]
    fn compat_fail_once_witness_mode_generates_existential() {
        let mut test = make_test_with_failure(
            "compat",
            "fail_once_test",
            OnTestFailure::SucceedImmediately,
        );
        test.fuzzer_output_type = FuzzerOutputType::Data;
        test.constraint = FuzzerConstraint::Any;

        let dir = tempfile::tempdir().unwrap();
        let config = VerifyConfig {
            out_dir: dir.path().to_path_buf(),
            cek_budget: 20000,
            blaster_rev: "main".to_string(),
            existential_mode: ExistentialMode::Witness,
            target: VerificationTargetKind::PropertyWrapper,
        };

        let manifest = generate_lean_workspace(&[test], &config, false).unwrap();
        assert_eq!(manifest.tests.len(), 1);

        // Read the generated proof file to verify existential theorem
        let proof_path = dir.path().join(&manifest.tests[0].lean_file);
        let content = fs::read_to_string(proof_path).unwrap();
        assert!(
            content.contains("∃"),
            "Existential theorem should use existential quantifier"
        );
    }

    #[test]
    fn compat_void_return_mode_generates_halt_theorem() {
        let mut test = make_test("compat", "void_test");
        test.return_mode = TestReturnMode::Void;
        test.fuzzer_output_type = FuzzerOutputType::Data;
        test.constraint = FuzzerConstraint::Any;

        let dir = tempfile::tempdir().unwrap();
        let config = VerifyConfig {
            out_dir: dir.path().to_path_buf(),
            cek_budget: 20000,
            blaster_rev: "main".to_string(),
            existential_mode: ExistentialMode::Witness,
            target: VerificationTargetKind::PropertyWrapper,
        };

        let manifest = generate_lean_workspace(&[test], &config, false).unwrap();
        assert_eq!(manifest.tests.len(), 1);

        let proof_path = dir.path().join(&manifest.tests[0].lean_file);
        let content = fs::read_to_string(proof_path).unwrap();
        assert!(
            content.contains("proveTestsHalt"),
            "Void-returning test should use proveTestsHalt"
        );
    }

    // --- Step 11.4: Intentional limitations register ---
    //
    // This test encodes the intentional limitations contract. Each entry is:
    //   (limitation, status, rationale, impact, mitigation, reconsideration_trigger)
    //
    // If the capabilities() output changes, this test must be updated to reflect
    // the new contract. This prevents undocumented behavior changes.

    #[test]
    fn intentional_limitations_register() {
        let caps = capabilities();

        // 1. Unit tests are intentionally unsupported.
        let unit = caps
            .unsupported_test_kinds
            .iter()
            .find(|n| n.kind == "unit")
            .unwrap();
        assert_eq!(unit.status, "intentional");

        // 2. Benchmarks are intentionally unsupported.
        let bench = caps
            .unsupported_test_kinds
            .iter()
            .find(|n| n.kind == "benchmark")
            .unwrap();
        assert_eq!(bench.status, "intentional");

        // 3. Max test arity is 1 (intentional: multi-arg tests must be
        //    encoded as tuple/record). Reconsideration: when compiler supports
        //    automatic multi-arg decomposition to tuple fuzzers.
        assert_eq!(caps.max_test_arity, 1);

        // 4. Nested composite types (List/Tuple/Pair inside List/Tuple/Pair) are
        //    unsupported. Reconsideration: when Lean Data encoding infrastructure
        //    supports recursive type decomposition.
        assert!(caps
            .unsupported_fuzzer_types
            .iter()
            .any(|t| t.contains("nested")));

        // 5. Blaster translation gaps are documented explicitly so failures can
        //    be surfaced with actionable context (e.g. List.Mem translation).
        assert!(caps
            .unsupported_fuzzer_types
            .iter()
            .any(|t| t.contains("Blaster translation gaps")));

        // 6. Supported envelope includes all three target modes.
        assert_eq!(caps.target_modes.len(), 3);
    }

    #[test]
    fn requires_explicit_bounds_tuple_with_bare_int_range() {
        // A bare IntRange constraint should satisfy tuple/pair Int-typed positions.
        // Tuple(Int, Data) with IntRange => no explicit bounds needed
        assert!(!requires_explicit_bounds(
            &FuzzerOutputType::Tuple(vec![FuzzerOutputType::Int, FuzzerOutputType::Data,]),
            &FuzzerConstraint::IntRange {
                min: "0".to_string(),
                max: "100".to_string(),
            },
        ));
        // Pair(Int, Int) with IntRange => no explicit bounds needed
        assert!(!requires_explicit_bounds(
            &FuzzerOutputType::Pair(
                Box::new(FuzzerOutputType::Int),
                Box::new(FuzzerOutputType::Int),
            ),
            &FuzzerConstraint::IntRange {
                min: "-10".to_string(),
                max: "10".to_string(),
            },
        ));
        // Pair(Int, Bool) with Map(IntRange) => no explicit bounds needed
        assert!(!requires_explicit_bounds(
            &FuzzerOutputType::Pair(
                Box::new(FuzzerOutputType::Int),
                Box::new(FuzzerOutputType::Bool),
            ),
            &FuzzerConstraint::Map(Box::new(FuzzerConstraint::IntRange {
                min: "0".to_string(),
                max: "255".to_string(),
            })),
        ));
    }

    #[test]
    fn tuple_int_proof_opens_data_namespace() {
        // Tuple proofs always construct a `Data.List [...]` argument, so Data must
        // be opened even when all element Lean types are non-Data.
        let test = make_test_with_type(
            "my_module",
            "test_tuple_ints",
            FuzzerOutputType::Tuple(vec![FuzzerOutputType::Int, FuzzerOutputType::Int]),
            FuzzerConstraint::Tuple(vec![
                FuzzerConstraint::IntRange {
                    min: "0".to_string(),
                    max: "10".to_string(),
                },
                FuzzerConstraint::IntRange {
                    min: "20".to_string(),
                    max: "30".to_string(),
                },
            ]),
        );
        let id = test_id("my_module", "test_tuple_ints");
        let lean_name = sanitize_lean_name("test_tuple_ints");
        let lean_module = "AikenVerify.Proofs.My_module.test_tuple_ints";

        let proof = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::default(),
            &VerificationTargetKind::default(),
        )
        .unwrap();

        assert!(
            proof.contains("open PlutusCore.Data (Data)"),
            "Tuple proof should open Data namespace for Data.List encoding, got:\n{proof}"
        );
        assert!(
            proof.contains("Data.List [Data.I a, Data.I b]"),
            "Tuple proof should encode arguments through Data constructors, got:\n{proof}"
        );
    }

    #[test]
    fn list_bool_with_mapped_int_range_no_element_bounds() {
        // List<Bool> produced via fuzz.map(int_between(...), ...) should NOT emit
        // numeric element bound predicates like `0 <= x_i` where x_i : Bool.
        let test = make_test_with_type(
            "my_module",
            "test_list_bool",
            FuzzerOutputType::List(Box::new(FuzzerOutputType::Bool)),
            FuzzerConstraint::List {
                elem: Box::new(FuzzerConstraint::Map(Box::new(
                    FuzzerConstraint::IntRange {
                        min: "0".to_string(),
                        max: "1".to_string(),
                    },
                ))),
                min_len: Some(0),
                max_len: Some(5),
            },
        );
        let id = test_id("my_module", "test_list_bool");
        let lean_name = sanitize_lean_name("test_list_bool");
        let lean_module = "AikenVerify.Proofs.My_module.test_list_bool";

        let proof = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::default(),
            &VerificationTargetKind::default(),
        )
        .unwrap();

        // Should have length bounds
        assert!(
            proof.contains("(0 <= xs.length && xs.length <= 5)"),
            "Should have length bounds, got:\n{proof}"
        );
        assert!(
            proof.contains("open PlutusCore.Data (Data)"),
            "List proof should open Data namespace for Data.List encoding, got:\n{proof}"
        );
        // Must NOT have numeric element predicates for Bool elements
        assert!(
            !proof.contains("0 <= x_i"),
            "Should NOT emit numeric element bounds for Bool type, got:\n{proof}"
        );
        assert!(
            !proof.contains("x_i <= 1"),
            "Should NOT emit numeric element bounds for Bool type, got:\n{proof}"
        );
    }
}
