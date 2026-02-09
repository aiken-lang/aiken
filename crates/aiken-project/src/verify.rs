use crate::export::{ExportedBounds, ExportedPropertyTest, FuzzerOutputType};
use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

/// Result of running proof verification
#[derive(Debug, serde::Serialize)]
pub struct VerifyResult {
    pub success: bool,
    pub stdout: String,
    pub stderr: String,
    pub exit_code: Option<i32>,
}

/// Status of a single theorem proof
#[derive(Debug, Clone, serde::Serialize)]
pub enum ProofStatus {
    /// Theorem was proved successfully
    Proved,
    /// Proof failed (timeout, counterexample, or error)
    Failed { reason: String },
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
    pub unknown: usize,
    pub theorems: Vec<TheoremResult>,
    pub raw_output: VerifyResult,
    /// Wall-clock milliseconds spent running proofs (lake fetch + lake build).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub elapsed_ms: Option<u64>,
}

/// Configuration for Lean workspace generation
pub struct VerifyConfig {
    pub out_dir: PathBuf,
    pub cek_budget: u64,
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
}

/// Result of workspace generation
#[derive(Debug, serde::Serialize)]
pub struct GeneratedManifest {
    pub version: String,
    pub tests: Vec<ManifestEntry>,
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
        "def", "theorem", "lemma", "where", "import", "open", "namespace",
        "end", "by", "match", "do", "let", "have", "if", "else", "return",
        "fun", "in", "with", "structure", "class", "instance", "section",
        "variable", "mutual", "protected", "private", "noncomputable",
    ];
    if LEAN_KEYWORDS.contains(&s.as_str()) {
        format!("l_{s}")
    } else {
        s
    }
}

/// Convert an Aiken module path like `aiken/list` to a PascalCase Lean module segment
/// like `AikenList`.
fn module_to_lean_segment(module: &str) -> String {
    module
        .split('/')
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
fn test_id(module: &str, name: &str) -> String {
    let module_part = sanitize_lean_name(&module.replace('/', "_"));
    let name_part = sanitize_lean_name(name);
    format!("{module_part}__{name_part}")
}

/// Check that required tools (lean, lake, z3) are available in PATH.
/// Returns Ok(()) if all tools found, or an error listing missing tools.
pub fn check_toolchain() -> miette::Result<()> {
    let mut missing = Vec::new();

    for tool in &["lean", "lake", "z3"] {
        if !is_tool_available(tool) {
            missing.push(*tool);
        }
    }

    if missing.is_empty() {
        Ok(())
    } else {
        Err(miette::miette!(
            "Required tools not found in PATH: {}. \
             Install Lean 4 (via elan: https://github.com/leanprover/elan) \
             and Z3 (https://github.com/Z3Prover/z3) to run proofs. \
             Use --generate-only to skip proof execution.",
            missing.join(", ")
        ))
    }
}

fn is_tool_available(name: &str) -> bool {
    Command::new(name)
        .arg("--version")
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .status()
        .is_ok()
}

/// Check whether the PlutusCore Lean library exists in the workspace.
/// Returns `Ok(())` when the directory is present, or an informational error
/// with instructions when it is not.
pub fn check_plutus_core(out_dir: &Path) -> miette::Result<()> {
    let pc_dir = out_dir.join("PlutusCore");
    if pc_dir.is_dir() {
        Ok(())
    } else {
        Err(miette::miette!(
            "PlutusCore Lean library not found at {path}.\n\n\
             The generated lakefile.lean expects a local dependency at ./PlutusCore.\n\
             Either symlink or copy the PlutusCore library there:\n\n\
             \tln -s /path/to/PlutusCore {path}\n\n\
             Then re-run `aiken verify` or manually: cd {out} && lake build",
            path = pc_dir.display(),
            out = out_dir.display(),
        ))
    }
}

/// Run `lake fetch` then `lake build` in the generated workspace directory.
/// Logs are written to `<out_dir>/logs/`.
/// The `_timeout_secs` parameter is accepted for future per-theorem timeout support.
pub fn run_proofs(out_dir: &Path, _timeout_secs: u64) -> miette::Result<VerifyResult> {
    let logs_dir = out_dir.join("logs");
    fs::create_dir_all(&logs_dir)
        .map_err(|e| miette::miette!("Failed to create logs directory: {e}"))?;

    // -- lake fetch --
    let fetch_output = Command::new("lake")
        .arg("fetch")
        .current_dir(out_dir)
        .output()
        .map_err(|e| miette::miette!("Failed to run 'lake fetch': {e}"))?;

    let fetch_stdout = String::from_utf8_lossy(&fetch_output.stdout).to_string();
    let fetch_stderr = String::from_utf8_lossy(&fetch_output.stderr).to_string();

    // Always persist fetch logs
    let _ = fs::write(
        logs_dir.join("lake-fetch.log"),
        format!("--- stdout ---\n{fetch_stdout}\n--- stderr ---\n{fetch_stderr}\n"),
    );

    if !fetch_output.status.success() {
        return Ok(VerifyResult {
            success: false,
            stdout: fetch_stdout,
            stderr: fetch_stderr,
            exit_code: fetch_output.status.code(),
        });
    }

    // -- lake build --
    let build_output = Command::new("lake")
        .arg("build")
        .current_dir(out_dir)
        .output()
        .map_err(|e| miette::miette!("Failed to run 'lake build': {e}"))?;

    let build_stdout = String::from_utf8_lossy(&build_output.stdout).to_string();
    let build_stderr = String::from_utf8_lossy(&build_output.stderr).to_string();

    let _ = fs::write(
        logs_dir.join("lake-build.log"),
        format!("--- stdout ---\n{build_stdout}\n--- stderr ---\n{build_stderr}\n"),
    );

    Ok(VerifyResult {
        success: build_output.status.success(),
        stdout: build_stdout,
        stderr: build_stderr,
        exit_code: build_output.status.code(),
    })
}

/// Generate a Lean workspace from exported property tests.
/// Returns an error if filesystem operations fail.
pub fn generate_lean_workspace(
    tests: &[ExportedPropertyTest],
    config: &VerifyConfig,
) -> miette::Result<GeneratedManifest> {
    let out = &config.out_dir;

    // Create directory structure
    fs::create_dir_all(out.join("AikenVerify/Proofs"))
        .map_err(|e| miette::miette!("Failed to create workspace directories: {e}"))?;
    fs::create_dir_all(out.join("flat"))
        .map_err(|e| miette::miette!("Failed to create flat directory: {e}"))?;

    // Write lakefile.lean
    write_file(
        &out.join("lakefile.lean"),
        &generate_lakefile(),
    )?;

    // Write lean-toolchain
    write_file(
        &out.join("lean-toolchain"),
        "leanprover/lean4:v4.24.0\n",
    )?;

    // Write Utils.lean
    write_file(
        &out.join("AikenVerify/Utils.lean"),
        &generate_utils(config.cek_budget),
    )?;

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

        // Directory for this module's proofs
        let proof_dir = out
            .join("AikenVerify/Proofs")
            .join(&lean_module_segment);
        fs::create_dir_all(&proof_dir)
            .map_err(|e| miette::miette!("Failed to create proof directory: {e}"))?;

        // Lean file path relative to out_dir
        let lean_file_rel = format!(
            "AikenVerify/Proofs/{lean_module_segment}/{lean_test_name}.lean"
        );

        // Write the .lean proof file for this test
        let proof_content = generate_proof_file(test, &id, &lean_test_name, &lean_module)?;
        write_file(
            &out.join(&lean_file_rel),
            &proof_content,
        )?;

        // Write the flat file (hex content)
        let flat_file_rel = format!("flat/{id}.flat");
        write_file(
            &out.join(&flat_file_rel),
            &test.test_program.hex,
        )?;

        // Track import for root module
        module_dirs
            .entry(lean_module_segment.clone())
            .or_default()
            .push(lean_test_name.clone());

        manifest_entries.push(ManifestEntry {
            id: id.clone(),
            aiken_module: module.clone(),
            aiken_name: test_name.to_string(),
            lean_module,
            lean_theorem: lean_test_name,
            lean_file: lean_file_rel,
            flat_file: flat_file_rel,
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
    };

    let manifest_json = serde_json::to_string_pretty(&manifest)
        .map_err(|e| miette::miette!("Failed to serialize manifest: {e}"))?;
    write_file(&out.join("manifest.json"), &manifest_json)?;

    Ok(manifest)
}

fn write_file(path: &Path, content: &str) -> miette::Result<()> {
    fs::write(path, content)
        .map_err(|e| miette::miette!("Failed to write {}: {e}", path.display()))
}

fn generate_lakefile() -> String {
    r#"import Lake
open Lake DSL

package AikenVerify where

@[default_target]
lean_lib AikenVerify where

require Blaster from git
  "https://github.com/input-output-hk/Lean-blaster" @ "main"

require PlutusCore from
  "./PlutusCore"
"#
    .to_string()
}

fn generate_utils(cek_budget: u64) -> String {
    format!(
        r#"import PlutusCore.UPLC.CekMachine

namespace AikenVerify.Utils
open PlutusCore.Integer (Integer)
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

def integerToBuiltin (x : Integer) : Term := Term.Const (Const.Integer x)

def executeIntProgram (p : Program) (args : List Term) (exUnit : Nat) : Option Integer :=
  fromFrameToInt $ cekExecuteProgram p args exUnit

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

def intArg (x : Integer) : List Term :=
  [Term.Const (Const.Data (Data.I x))]

def boolArg (x : Bool) : List Term :=
  [Term.Const (Const.Data (Data.Constr (if x then 1 else 0) []))]

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

/// Extract and validate integer bounds from the test's extracted_bounds.
fn extract_int_bounds(test: &ExportedPropertyTest) -> miette::Result<(String, String)> {
    let (min, max) = match &test.extracted_bounds {
        ExportedBounds::IntBetween { min, max } => (min.clone(), max.clone()),
        ExportedBounds::Unknown => {
            return Err(miette::miette!(
                "Test '{}' has unknown bounds; cannot generate a bounded theorem without explicit Int bounds",
                test.name,
            ));
        }
    };

    if !is_valid_integer_literal(&min) {
        return Err(miette::miette!(
            "Test '{}' has invalid min bound '{}'; expected an integer literal",
            test.name,
            min,
        ));
    }
    if !is_valid_integer_literal(&max) {
        return Err(miette::miette!(
            "Test '{}' has invalid max bound '{}'; expected an integer literal",
            test.name,
            max,
        ));
    }

    Ok((min, max))
}

fn generate_proof_file(
    test: &ExportedPropertyTest,
    test_id: &str,
    lean_test_name: &str,
    lean_module: &str,
) -> miette::Result<String> {
    use aiken_lang::ast::OnTestFailure;

    let assertion = match &test.on_test_failure {
        // Default test (no `fail` keyword): body should return true for all inputs
        OnTestFailure::FailImmediately => "true",
        // `fail` keyword: body should return false for all inputs
        OnTestFailure::SucceedEventually => "false",
        // `fail once` keyword: existential quantifier, incompatible with Blaster
        OnTestFailure::SucceedImmediately => {
            return Err(miette::miette!(
                "Test '{}' uses `fail once` which requires existential quantification, \
                 incompatible with Blaster's universal-quantifier tactic",
                test.name,
            ));
        }
    };

    let prog = prog_name(test_id);

    match &test.fuzzer_output_type {
        FuzzerOutputType::Int => {
            let (min, max) = extract_int_bounds(test)?;

            Ok(format!(
                r#"import AikenVerify.Utils
import PlutusCore.UPLC.ScriptEncoding
import Blaster

namespace {lean_module}
open PlutusCore.Integer (Integer)
open PlutusCore.UPLC.CekMachine
open PlutusCore.UPLC.Term (Term Const Program)
open AikenVerify.Utils

#import_uplc {prog} single_cbor_hex "./flat/{test_id}.flat"

theorem {lean_test_name} :
  ∀ (x : Integer),
  ({min} <= x && x <= {max})
  →
  (proveTests {prog} (intArg x)) = {assertion} :=
  by blaster

theorem {lean_test_name}_alwaysTerminating :
  ∀ (x : Integer),
  ({min} <= x && x <= {max})
  →
  Option.isSome (proveTests {prog} (intArg x)) :=
  by blaster

end {lean_module}
"#
            ))
        }

        FuzzerOutputType::Bool => {
            Ok(format!(
                r#"import AikenVerify.Utils
import PlutusCore.UPLC.ScriptEncoding
import Blaster

namespace {lean_module}
open PlutusCore.UPLC.CekMachine
open PlutusCore.UPLC.Term (Term Const Program)
open AikenVerify.Utils

#import_uplc {prog} single_cbor_hex "./flat/{test_id}.flat"

theorem {lean_test_name} :
  ∀ (x : Bool),
  (proveTests {prog} (boolArg x)) = {assertion} :=
  by blaster

theorem {lean_test_name}_alwaysTerminating :
  ∀ (x : Bool),
  Option.isSome (proveTests {prog} (boolArg x)) :=
  by blaster

end {lean_module}
"#
            ))
        }

        FuzzerOutputType::Tuple(types)
            if types.len() == 2
                && matches!(types[0], FuzzerOutputType::Int)
                && matches!(types[1], FuzzerOutputType::Int) =>
        {
            let (min, max) = extract_int_bounds(test)?;

            Ok(format!(
                r#"import AikenVerify.Utils
import PlutusCore.UPLC.ScriptEncoding
import Blaster

namespace {lean_module}
open PlutusCore.Integer (Integer)
open PlutusCore.UPLC.CekMachine
open PlutusCore.UPLC.Term (Term Const Program)
open AikenVerify.Utils

#import_uplc {prog} single_cbor_hex "./flat/{test_id}.flat"

theorem {lean_test_name} :
  ∀ (x : Integer) (y : Integer),
  ({min} <= x && x <= {max})
  →
  ({min} <= y && y <= {max})
  →
  (proveTests {prog} (intArgs2 x y)) = {assertion} :=
  by blaster

theorem {lean_test_name}_alwaysTerminating :
  ∀ (x : Integer) (y : Integer),
  ({min} <= x && x <= {max})
  →
  ({min} <= y && y <= {max})
  →
  Option.isSome (proveTests {prog} (intArgs2 x y)) :=
  by blaster

end {lean_module}
"#
            ))
        }

        FuzzerOutputType::ByteArray => {
            Err(miette::miette!(
                "Test '{}' uses ByteArray fuzzer which is not yet supported for verification; \
                 ByteArray proofs require bounded-length assumptions which are not yet implemented",
                test.name,
            ))
        }

        FuzzerOutputType::List(_) => {
            Err(miette::miette!(
                "Test '{}' uses List fuzzer which is not yet supported for verification; \
                 List proofs require bounded-length assumptions which are not yet implemented",
                test.name,
            ))
        }

        other => {
            Err(miette::miette!(
                "Test '{}' has unsupported fuzzer output type {:?}; \
                 only Int, Bool, and (Int, Int) tuples are supported for verification",
                test.name,
                other,
            ))
        }
    }
}

/// Parse lake build output into per-theorem results.
/// Uses the manifest to know which theorems were expected.
pub fn parse_verify_results(raw: VerifyResult, manifest: &GeneratedManifest) -> VerifySummary {
    let mut theorems = Vec::new();

    if raw.success {
        for entry in &manifest.tests {
            theorems.push(TheoremResult {
                test_name: format!("{}.{}", entry.aiken_module, entry.aiken_name),
                theorem_name: entry.lean_theorem.clone(),
                status: ProofStatus::Proved,
            });
            theorems.push(TheoremResult {
                test_name: format!("{}.{}", entry.aiken_module, entry.aiken_name),
                theorem_name: format!("{}_alwaysTerminating", entry.lean_theorem),
                status: ProofStatus::Proved,
            });
        }
    } else {
        let stderr = &raw.stderr;
        for entry in &manifest.tests {
            let correctness_status =
                if stderr.contains(&format!("error: '{}' ", entry.lean_theorem))
                    || stderr.contains(&format!("'{}'", entry.lean_theorem))
                {
                    ProofStatus::Failed {
                        reason: extract_error_for_theorem(&entry.lean_theorem, stderr),
                    }
                } else {
                    ProofStatus::Unknown
                };

            theorems.push(TheoremResult {
                test_name: format!("{}.{}", entry.aiken_module, entry.aiken_name),
                theorem_name: entry.lean_theorem.clone(),
                status: correctness_status,
            });

            let term_name = format!("{}_alwaysTerminating", entry.lean_theorem);
            let term_status = if stderr.contains(&term_name) {
                ProofStatus::Failed {
                    reason: extract_error_for_theorem(&term_name, stderr),
                }
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

    let proved = theorems
        .iter()
        .filter(|t| matches!(t.status, ProofStatus::Proved))
        .count();
    let failed = theorems
        .iter()
        .filter(|t| matches!(t.status, ProofStatus::Failed { .. }))
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
        unknown,
        theorems,
        raw_output: raw,
        elapsed_ms: None,
    }
}

fn extract_error_for_theorem(theorem: &str, stderr: &str) -> String {
    stderr
        .lines()
        .filter(|l| l.contains(theorem))
        .collect::<Vec<_>>()
        .join("\n")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::export::{ExportedBounds, ExportedProgram, ExportedPropertyTest, FuzzerOutputType};
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
            extracted_bounds: ExportedBounds::IntBetween {
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
            module_to_lean_segment("deep/nested/module"),
            "DeepNestedModule"
        );
    }

    #[test]
    fn test_id_generation() {
        assert_eq!(test_id("aiken/list", "test_map"), "aiken_list__test_map");
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
        };

        let manifest = generate_lean_workspace(&tests, &config).unwrap();

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
        assert!(out_dir
            .join("flat/my_module__test_roundtrip.flat")
            .exists());
        assert!(out_dir.join("flat/aiken_list__test_map.flat").exists());

        // Check manifest
        assert_eq!(manifest.tests.len(), 2);
        assert_eq!(manifest.tests[0].id, "my_module__test_roundtrip");
        assert_eq!(
            manifest.tests[0].lean_module,
            "AikenVerify.Proofs.My_module.test_roundtrip"
        );
        assert_eq!(manifest.tests[1].id, "aiken_list__test_map");

        // Check flat file content is the hex
        let flat_content =
            fs::read_to_string(out_dir.join("flat/my_module__test_roundtrip.flat")).unwrap();
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
        let proof1 = fs::read_to_string(
            out_dir.join("AikenVerify/Proofs/My_module/test_roundtrip.lean"),
        )
        .unwrap();
        assert!(proof1.contains("theorem test_roundtrip"));
        assert!(proof1.contains("theorem test_roundtrip_alwaysTerminating"));
        assert!(proof1.contains("by blaster"));
        assert!(proof1.contains("#import_uplc prog_my_module__test_roundtrip single_cbor_hex"));
        assert!(proof1.contains("(0 <= x && x <= 255)"));
        assert!(proof1.contains("namespace AikenVerify.Proofs.My_module.test_roundtrip"));

        let proof2 = fs::read_to_string(
            out_dir.join("AikenVerify/Proofs/AikenList/test_map.lean"),
        )
        .unwrap();
        assert!(proof2.contains("theorem test_map"));
        assert!(proof2.contains("theorem test_map_alwaysTerminating"));
        assert!(proof2.contains("#import_uplc prog_aiken_list__test_map single_cbor_hex"));
        assert!(proof2.contains("./flat/aiken_list__test_map.flat"));
        assert!(proof2.contains("(0 <= x && x <= 255)"));
    }

    #[test]
    fn generate_workspace_empty_tests() {
        let tmp = tempfile::tempdir().unwrap();
        let config = VerifyConfig {
            out_dir: tmp.path().to_path_buf(),
            cek_budget: 20000,
        };

        let manifest = generate_lean_workspace(&[], &config).unwrap();
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

        let proof = generate_proof_file(&test, &id, &lean_name, lean_module).unwrap();

        assert!(
            proof.contains("(proveTests prog_my_module__test_pass (intArg x)) = true"),
            "FailImmediately (default) should generate = true assertion, got:\n{proof}"
        );
        assert!(
            proof.contains("Option.isSome (proveTests prog_my_module__test_pass (intArg x))"),
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

        let proof = generate_proof_file(&test, &id, &lean_name, lean_module).unwrap();

        assert!(
            proof.contains("(proveTests prog_my_module__test_fail (intArg x)) = false"),
            "SucceedEventually (fail keyword) should generate = false assertion, got:\n{proof}"
        );
        assert!(
            !proof.contains("= true"),
            "SucceedEventually should not contain = true"
        );
    }

    #[test]
    fn succeed_immediately_errors_fail_once() {
        // SucceedImmediately = `fail once` keyword, existential, incompatible with Blaster
        let test =
            make_test_with_failure("my_module", "test_ev", OnTestFailure::SucceedImmediately);
        let id = test_id("my_module", "test_ev");
        let lean_name = sanitize_lean_name("test_ev");
        let lean_module = "AikenVerify.Proofs.My_module.test_ev";

        let result = generate_proof_file(&test, &id, &lean_name, lean_module);
        assert!(result.is_err());
        let err_msg = result.unwrap_err().to_string();
        assert!(
            err_msg.contains("fail once"),
            "Error should mention fail once, got: {err_msg}"
        );
    }

    fn make_test_with_type(
        module: &str,
        name: &str,
        fuzzer_output_type: FuzzerOutputType,
        extracted_bounds: ExportedBounds,
    ) -> ExportedPropertyTest {
        ExportedPropertyTest {
            name: format!("{module}.{name}"),
            module: module.to_string(),
            input_path: format!("lib/{}.ak", module.replace('/', "/")),
            on_test_failure: OnTestFailure::FailImmediately,
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
            extracted_bounds,
        }
    }

    #[test]
    fn bool_generates_theorem_without_bounds() {
        let test = make_test_with_type(
            "my_module",
            "test_bool",
            FuzzerOutputType::Bool,
            ExportedBounds::Unknown,
        );
        let id = test_id("my_module", "test_bool");
        let lean_name = sanitize_lean_name("test_bool");
        let lean_module = "AikenVerify.Proofs.My_module.test_bool";

        let proof = generate_proof_file(&test, &id, &lean_name, lean_module).unwrap();

        assert!(
            proof.contains("∀ (x : Bool),"),
            "Bool theorem should quantify over Bool, got:\n{proof}"
        );
        assert!(
            proof.contains("(proveTests prog_my_module__test_bool (boolArg x)) = true"),
            "Bool theorem should use boolArg, got:\n{proof}"
        );
        assert!(
            proof.contains("theorem test_bool_alwaysTerminating"),
            "Should have termination theorem, got:\n{proof}"
        );
        assert!(
            proof.contains("Option.isSome (proveTests prog_my_module__test_bool (boolArg x))"),
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
            ExportedBounds::Unknown,
        );
        test.on_test_failure = OnTestFailure::SucceedEventually;

        let id = test_id("my_module", "test_bool_fail");
        let lean_name = sanitize_lean_name("test_bool_fail");
        let lean_module = "AikenVerify.Proofs.My_module.test_bool_fail";

        let proof = generate_proof_file(&test, &id, &lean_name, lean_module).unwrap();

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
            ExportedBounds::IntBetween {
                min: "1".to_string(),
                max: "10".to_string(),
            },
        );
        let id = test_id("my_module", "test_pair");
        let lean_name = sanitize_lean_name("test_pair");
        let lean_module = "AikenVerify.Proofs.My_module.test_pair";

        let proof = generate_proof_file(&test, &id, &lean_name, lean_module).unwrap();

        assert!(
            proof.contains("∀ (x : Integer) (y : Integer),"),
            "Tuple theorem should quantify over two Integers, got:\n{proof}"
        );
        assert!(
            proof.contains("(1 <= x && x <= 10)"),
            "Should have bounds for x, got:\n{proof}"
        );
        assert!(
            proof.contains("(1 <= y && y <= 10)"),
            "Should have bounds for y, got:\n{proof}"
        );
        assert!(
            proof.contains("(proveTests prog_my_module__test_pair (intArgs2 x y)) = true"),
            "Should use intArgs2, got:\n{proof}"
        );
        assert!(
            proof.contains("Option.isSome (proveTests prog_my_module__test_pair (intArgs2 x y))"),
            "Termination theorem should use intArgs2, got:\n{proof}"
        );
    }

    #[test]
    fn tuple_int_int_unknown_bounds_errors() {
        let test = make_test_with_type(
            "my_module",
            "test_pair_nobound",
            FuzzerOutputType::Tuple(vec![FuzzerOutputType::Int, FuzzerOutputType::Int]),
            ExportedBounds::Unknown,
        );
        let id = test_id("my_module", "test_pair_nobound");
        let lean_name = sanitize_lean_name("test_pair_nobound");
        let lean_module = "AikenVerify.Proofs.My_module.test_pair_nobound";

        let result = generate_proof_file(&test, &id, &lean_name, lean_module);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("unknown bounds"));
    }

    #[test]
    fn bytearray_returns_unsupported_error() {
        let test = make_test_with_type(
            "my_module",
            "test_bytes",
            FuzzerOutputType::ByteArray,
            ExportedBounds::Unknown,
        );
        let id = test_id("my_module", "test_bytes");
        let lean_name = sanitize_lean_name("test_bytes");
        let lean_module = "AikenVerify.Proofs.My_module.test_bytes";

        let result = generate_proof_file(&test, &id, &lean_name, lean_module);
        assert!(result.is_err());
        let err_msg = result.unwrap_err().to_string();
        assert!(
            err_msg.contains("ByteArray") && err_msg.contains("not yet supported"),
            "Should mention ByteArray unsupported, got: {err_msg}"
        );
    }

    #[test]
    fn list_returns_unsupported_error() {
        let test = make_test_with_type(
            "my_module",
            "test_list",
            FuzzerOutputType::List(Box::new(FuzzerOutputType::Int)),
            ExportedBounds::Unknown,
        );
        let id = test_id("my_module", "test_list");
        let lean_name = sanitize_lean_name("test_list");
        let lean_module = "AikenVerify.Proofs.My_module.test_list";

        let result = generate_proof_file(&test, &id, &lean_name, lean_module);
        assert!(result.is_err());
        let err_msg = result.unwrap_err().to_string();
        assert!(
            err_msg.contains("List") && err_msg.contains("not yet supported"),
            "Should mention List unsupported, got: {err_msg}"
        );
    }

    #[test]
    fn tuple_non_int_int_returns_unsupported_error() {
        let test = make_test_with_type(
            "my_module",
            "test_tuple_mixed",
            FuzzerOutputType::Tuple(vec![FuzzerOutputType::Int, FuzzerOutputType::Bool]),
            ExportedBounds::Unknown,
        );
        let id = test_id("my_module", "test_tuple_mixed");
        let lean_name = sanitize_lean_name("test_tuple_mixed");
        let lean_module = "AikenVerify.Proofs.My_module.test_tuple_mixed";

        let result = generate_proof_file(&test, &id, &lean_name, lean_module);
        assert!(result.is_err());
        let err_msg = result.unwrap_err().to_string();
        assert!(
            err_msg.contains("unsupported fuzzer output type"),
            "Mixed tuple should be unsupported, got: {err_msg}"
        );
    }

    #[test]
    fn utils_contains_bool_arg() {
        let utils = generate_utils(10000);
        assert!(
            utils.contains("def boolArg"),
            "Utils should contain boolArg definition"
        );
        assert!(
            utils.contains("Data.Constr (if x then 1 else 0) []"),
            "boolArg should encode Bool as Data.Constr"
        );
    }

    fn make_manifest(entries: Vec<(&str, &str, &str)>) -> GeneratedManifest {
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
                })
                .collect(),
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
        };

        let summary = parse_verify_results(raw, &manifest);

        assert_eq!(summary.total, 4);
        assert_eq!(summary.proved, 4);
        assert_eq!(summary.failed, 0);
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
        };

        let summary = parse_verify_results(raw, &manifest);

        assert_eq!(summary.total, 4);
        assert_eq!(summary.failed, 1);
        assert!(summary.proved == 0);

        let test_add = summary
            .theorems
            .iter()
            .find(|t| t.theorem_name == "test_add")
            .unwrap();
        assert!(
            matches!(&test_add.status, ProofStatus::Failed { reason } if reason.contains("test_add")),
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
    fn parse_verify_results_termination_failure() {
        let manifest = make_manifest(vec![("my_module", "test_add", "test_add")]);
        let raw = VerifyResult {
            success: false,
            stdout: String::new(),
            stderr: "error: 'test_add_alwaysTerminating' tactic failed".to_string(),
            exit_code: Some(1),
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
        };

        let summary = parse_verify_results(raw, &manifest);

        assert_eq!(summary.total, 0);
        assert_eq!(summary.proved, 0);
        assert_eq!(summary.failed, 0);
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
        };

        let summary = parse_verify_results(raw, &manifest);

        assert_eq!(summary.total, 2);
        assert_eq!(summary.unknown, 2);
        assert_eq!(summary.failed, 0);
    }

    #[test]
    fn extract_error_for_theorem_filters_correctly() {
        let stderr = "line1: unrelated\nerror: 'my_thm' unsolved goals\nline3: also unrelated\nmy_thm failed to synthesize\n";
        let result = extract_error_for_theorem("my_thm", stderr);
        assert_eq!(result, "error: 'my_thm' unsolved goals\nmy_thm failed to synthesize");
    }
}
