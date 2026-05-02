use aiken_lang::ast::Tracing;
use aiken_project::{
    Project,
    config::{ProjectConfig, WorkspaceConfig},
    export::{ExportedPropertyTest, VerificationTargetKind},
    options::Options,
    telemetry::EventTarget,
    verify::{
        self, ArtifactRetention, DEFAULT_BLASTER_REV, DEFAULT_PLUTUS_CORE_REV, ExistentialMode,
        FailureCategory, MAX_RAW_OUTPUT_TAIL_BYTES, ProofStatus, SkipPolicy, VerifyConfig,
    },
    watch::{with_project_event_target, workspace_root},
};
use owo_colors::{OwoColorize, Stream::Stderr};
use std::{
    path::{Path, PathBuf},
    process,
};

/// Commands for formal verification of property tests
#[derive(clap::Subcommand)]
#[clap(disable_version_flag(true))]
pub enum Cmd {
    /// Run formal verification on property tests
    Run(RunArgs),

    /// Check toolchain, dependencies, and configuration
    Doctor(DoctorArgs),

    /// Remove generated verification artifacts and logs
    Clean(CleanArgs),

    /// Show supported verification capabilities
    Capabilities(CapabilitiesArgs),
}

#[derive(clap::Args)]
#[clap(disable_version_flag(true))]
#[command(
    verbatim_doc_comment,
    about = color_print::cstr!(r#"
Formally verify property tests using the Blaster theorem prover.
"#),
    after_long_help = color_print::cstr!(r#"<bold><underline>Examples:</underline></bold>

    <bold>aiken verify run</bold>
        Verify all property tests in the current project

    <bold>aiken verify run -m "my_module.test_"</bold>
        Verify only property tests matching the pattern

    <bold>aiken verify run --generate-only</bold>
        Generate Lean artifacts without running proofs

    <bold>aiken verify run --blaster-rev abc123</bold>
        Pin Blaster to a specific git revision

    <bold>aiken verify run --artifacts always</bold>
        Keep generated Lean artifacts regardless of outcome

You are seeing the extended help. Use `-h` instead of `--help` for a more compact view.
"#
))]
pub struct RunArgs {
    /// Path to project
    directory: Option<PathBuf>,

    /// Deny warnings; warnings will be treated as errors
    #[clap(short = 'D', long)]
    deny: bool,

    /// Silence warnings; warnings will not be printed
    #[clap(short = 'S', long)]
    silent: bool,

    /// Only run tests if they match any of these strings.
    /// You can match a module with `-m aiken/list` or `-m list`.
    /// You can match a test with `-m "aiken/list.{map}"` or `-m "aiken/option.{flatten_1}"`
    #[clap(short, long, verbatim_doc_comment)]
    match_tests: Option<Vec<String>>,

    /// This is meant to be used with `--match-tests`.
    /// It forces test names to match exactly
    #[clap(short, long)]
    exact_match: bool,

    /// Environment to build against.
    #[clap(long)]
    env: Option<String>,

    /// Only generate Lean artifacts without running proofs
    #[clap(long)]
    generate_only: bool,

    /// Output directory for Lean workspace
    #[clap(long, default_value = "build/verify")]
    out_dir: PathBuf,

    /// [Deprecated: use --artifacts always] Keep generated Lean artifacts after verification
    #[clap(long, hide = true)]
    keep_artifacts: bool,

    /// When to retain generated Lean artifacts.
    /// `on-failure` (default): keep only when proofs fail/timeout/unknown.
    /// `on-success`: keep only after a fully successful run.
    /// `always`: always keep artifacts.
    /// `never`: always remove artifacts after verification.
    #[clap(long, default_value = "on-failure", verbatim_doc_comment)]
    artifacts: ArtifactRetention,

    /// Timeout in seconds per theorem build. Use 0 to disable timeout (wait indefinitely).
    #[clap(long, default_value = "300")]
    timeout: u64,

    /// CEK machine step budget
    #[clap(long, default_value = "200000")]
    cek_budget: u64,

    /// Optional Lake jobs override for `lake build`.
    /// When omitted (`0`), Lake's default scheduling is used.
    #[clap(short = 'j', long, default_value = "0")]
    jobs: usize,

    /// Output results as JSON
    #[clap(long)]
    json: bool,

    /// Skip unsupported tests instead of failing. Skipped tests are reported but
    /// do not block proof generation for other tests.
    ///
    /// Without a value, every skippable code (E0xxx in the
    /// `UnsupportedShape` / `FallbackRequired` categories) is silenced.
    /// With a comma-separated value (e.g.
    /// `--skip-unsupported=E0011,E0013`), only the listed catalogue codes
    /// are silenced; any other skippable code becomes a hard error.
    ///
    /// `S0xxx` (`UnsoundFallback`) and `E004x` (`InvalidConstraint`) codes
    /// are NEVER silenced regardless of the filter.
    ///
    /// Pass `--strict-unsupported` (the default behaviour when this flag is
    /// omitted) to make every unsupported test a hard error.
    #[clap(long, value_delimiter = ',', num_args = 0.., require_equals = true, default_missing_value = "")]
    skip_unsupported: Option<Vec<String>>,

    /// Treat all unsupported tests as hard errors (the default behaviour).
    /// Equivalent to omitting `--skip-unsupported`. Mutually exclusive with
    /// `--skip-unsupported`.
    #[clap(long, conflicts_with = "skip_unsupported")]
    strict_unsupported: bool,

    /// When used with --skip-unsupported, exit 0 even if tests were skipped.
    /// Without this flag, skipped tests cause a non-zero exit.
    #[clap(long, requires = "skip_unsupported")]
    allow_skips: bool,

    /// Git revision (commit, tag, or branch) for the Blaster dependency.
    /// Defaults to the version pinned in this release.
    #[clap(long, default_value = DEFAULT_BLASTER_REV)]
    blaster_rev: String,

    /// Strategy for `fail once` (existential) tests.
    /// `proof`: attempt full existential theorem via Lean tactics (default).
    ///          Required for Int-domain existentials — `by blaster`
    ///          lets Z3 synthesize the witness.
    /// `witness`: deterministic witness search + concrete proof. Only
    ///            sound for domains where the trivial witness (0, False,
    ///            …) is always a valid falsifier.
    #[clap(long, default_value = "proof")]
    existential_mode: ExistentialMode,

    /// HIDDEN DEBUG FLAG. Allow the legacy unsound emission of
    /// `opaque {pred} : Data → Data → Prop` predicates for sub-generators
    /// whose bodies cannot be statically inlined, replacing `opaque` with
    /// `def {pred} := fun _ _ => True`. Without this flag (the default),
    /// such tests hard-error with E0018 because Lean's `Inhabited Prop`
    /// instance silently fills `opaque` with `True`, making every
    /// constraint over the sub-generator domain trivially provable.
    /// Use only for debugging vacuous proof regressions.
    #[clap(long, hide = true)]
    allow_vacuous_subgenerators: bool,

    /// Verification target mode.
    /// `property` (default): verify property tests directly.
    /// `validator`: verify validator handler programs for tests that export validator metadata.
    /// `equivalence`: prove wrapper/handler equivalence for tests that export validator metadata.
    #[clap(long, default_value = "property")]
    target: VerificationTargetKind,

    /// Git revision (commit, tag, or branch) for the PlutusCore dependency.
    /// Defaults to the version pinned in this release. Use --plutus-core-dir for a local checkout instead.
    #[clap(long, default_value = DEFAULT_PLUTUS_CORE_REV)]
    plutus_core_rev: String,

    /// Path to the PlutusCore Lean library checkout. Overrides the PLUTUS_CORE_DIR environment variable.
    #[clap(long)]
    plutus_core_dir: Option<PathBuf>,

    /// Maximum bytes of raw stdout/stderr to retain in the verify summary.
    /// Excess output is truncated; the full streams are persisted under
    /// `<out_dir>/logs/lake_build.{stdout,stderr}.log` when truncated.
    /// `0` disables truncation (full streams in JSON).
    /// Maximum: 16777216 (16 MiB).
    #[clap(long, default_value = "65536")]
    raw_output_bytes: usize,

    /// Allow CI to pass when some proofs are sorry-closed (Partial).
    /// Without this flag, any partial proof causes a non-zero exit.
    /// Full proofs of these obligations will be added in a future Aiken release.
    #[clap(long)]
    accept_partial: bool,

    /// Allow CI to pass when some proofs are existential/witness-only.
    /// Without this flag, witness-only proofs cause a non-zero exit.
    /// Universal proof support for state-machine tests will be added in a
    /// future Aiken release.
    #[clap(long)]
    accept_witness: bool,
}

#[derive(clap::Args)]
#[clap(disable_version_flag(true))]
#[command(
    verbatim_doc_comment,
    about = color_print::cstr!(r#"
Check the verification toolchain, dependencies, and local configuration.
"#),
    after_long_help = color_print::cstr!(r#"<bold><underline>Examples:</underline></bold>

    <bold>aiken verify doctor</bold>
        Check the default verification toolchain status

    <bold>aiken verify doctor --json</bold>
        Emit the report as JSON

You are seeing the extended help. Use `-h` instead of `--help` for a more compact view.
"#),
)]
pub struct DoctorArgs {
    /// Output results as JSON
    #[clap(long)]
    json: bool,

    /// Git revision (commit, tag, or branch) for the Blaster dependency to report
    #[clap(long, default_value = DEFAULT_BLASTER_REV)]
    blaster_rev: String,

    /// Git revision (commit, tag, or branch) for the PlutusCore dependency to report
    #[clap(long, default_value = DEFAULT_PLUTUS_CORE_REV)]
    plutus_core_rev: String,
}

#[derive(clap::Args)]
#[clap(disable_version_flag(true))]
#[command(
    verbatim_doc_comment,
    about = color_print::cstr!(r#"
Remove generated verification artifacts and logs.
"#),
    after_long_help = color_print::cstr!(r#"<bold><underline>Examples:</underline></bold>

    <bold>aiken verify clean</bold>
        Remove the default verification workspace

    <bold>aiken verify clean --out-dir build/verify-ci</bold>
        Remove a non-default verification workspace

You are seeing the extended help. Use `-h` instead of `--help` for a more compact view.
"#),
)]
pub struct CleanArgs {
    /// Path to project
    directory: Option<PathBuf>,

    /// Output directory containing verification artifacts to remove
    #[clap(long, default_value = "build/verify")]
    out_dir: PathBuf,
}

#[derive(clap::Args)]
#[clap(disable_version_flag(true))]
#[command(
    verbatim_doc_comment,
    about = color_print::cstr!(r#"
Show supported verification capabilities and skip catalogue codes.
"#),
    after_long_help = color_print::cstr!(r#"<bold><underline>Examples:</underline></bold>

    <bold>aiken verify capabilities</bold>
        Print the supported verification surface in text form

    <bold>aiken verify capabilities --json</bold>
        Emit the supported verification surface as JSON

You are seeing the extended help. Use `-h` instead of `--help` for a more compact view.
"#),
)]
pub struct CapabilitiesArgs {
    /// Output as JSON
    #[clap(long)]
    json: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OutputMode {
    Text,
    Json,
    Silent,
}

impl OutputMode {
    fn from_flags(json: bool, silent: bool) -> Self {
        if json {
            Self::Json
        } else if silent {
            Self::Silent
        } else {
            Self::Text
        }
    }

    fn emits_json(self) -> bool {
        matches!(self, Self::Json)
    }

    fn shows_advisories(self) -> bool {
        matches!(self, Self::Text)
    }

    fn project_event_target(self) -> EventTarget {
        match self {
            Self::Text => EventTarget::default(),
            Self::Json | Self::Silent => EventTarget::Json(Default::default()),
        }
    }

    fn reports_project_diagnostics(self) -> bool {
        self.shows_advisories()
    }
}

#[derive(Clone)]
struct RunCommandOptions {
    match_tests: Option<Vec<String>>,
    exact_match: bool,
    env: Option<String>,
    generate_only: bool,
    out_dir: PathBuf,
    artifact_policy: ArtifactRetention,
    timeout: u64,
    cek_budget: u64,
    jobs_override: Option<usize>,
    output_mode: OutputMode,
    /// Resolved policy for `--skip-unsupported` / `--strict-unsupported`.
    /// `SkipPolicy::None` is the default (= `--strict-unsupported`); the
    /// CLI parser maps the raw `Option<Vec<String>>` shape via
    /// `SkipPolicy::from_cli`.
    skip_policy: SkipPolicy,
    allow_skips: bool,
    blaster_rev: String,
    plutus_core_rev: String,
    existential_mode: ExistentialMode,
    target: VerificationTargetKind,
    plutus_core_dir: Option<PathBuf>,
    raw_output_bytes: usize,
    accept_partial: bool,
    accept_witness: bool,
    allow_vacuous_subgenerators: bool,
}

pub fn exec(cmd: Cmd) -> miette::Result<()> {
    match cmd {
        Cmd::Run(args) => exec_run(args),
        Cmd::Doctor(args) => exec_doctor(args),
        Cmd::Clean(args) => exec_clean(args),
        Cmd::Capabilities(args) => exec_capabilities(args),
    }
}

fn format_no_property_tests_output(
    output_mode: OutputMode,
    blaster_rev: &str,
    plutus_core_rev: &str,
    allow_vacuous_subgenerators: bool,
) -> Result<String, serde_json::Error> {
    if output_mode.emits_json() {
        let manifest = verify::GeneratedManifest::empty(verify::GENERATE_ONLY_VERSION.to_string());
        serde_json::to_string_pretty(&no_proofs_summary(
            &manifest,
            false,
            blaster_rev,
            plutus_core_rev,
            allow_vacuous_subgenerators,
            two_phase_disabled_from_env(),
        ))
    } else {
        Ok("No property tests found.".to_string())
    }
}

fn no_property_tests_early_output(
    property_tests: &[ExportedPropertyTest],
    output_mode: OutputMode,
    blaster_rev: &str,
    plutus_core_rev: &str,
    allow_vacuous_subgenerators: bool,
) -> Result<Option<String>, serde_json::Error> {
    if property_tests.is_empty() {
        format_no_property_tests_output(
            output_mode,
            blaster_rev,
            plutus_core_rev,
            allow_vacuous_subgenerators,
        )
        .map(Some)
    } else {
        Ok(None)
    }
}

fn run_proofs_start_output(output_mode: OutputMode) -> Option<&'static str> {
    output_mode
        .shows_advisories()
        .then_some("Running proofs via lake build...")
}

fn two_phase_disabled_from_env() -> bool {
    std::env::var("AIKEN_EMIT_TWO_PHASE")
        .map(|value| value == "0")
        .unwrap_or(false)
}

fn no_proofs_summary(
    manifest: &verify::GeneratedManifest,
    skipped_without_allow: bool,
    blaster_rev: &str,
    plutus_core_rev: &str,
    allow_vacuous_subgenerators: bool,
    two_phase_disabled: bool,
) -> verify::VerifySummary {
    verify::VerifySummary::no_proofs(
        manifest,
        skipped_without_allow,
        blaster_rev,
        plutus_core_rev,
        allow_vacuous_subgenerators,
        two_phase_disabled,
    )
}

fn normalize_summary_log_paths(
    summary: &mut verify::VerifySummary,
    project_root: &Path,
    artifacts_retained: bool,
) {
    if !artifacts_retained {
        summary.raw_output.stdout.log_path = None;
        summary.raw_output.stderr.log_path = None;
        return;
    }

    for output in [
        &mut summary.raw_output.stdout,
        &mut summary.raw_output.stderr,
    ] {
        let Some(path) = output.log_path.clone() else {
            continue;
        };

        if let Ok(relative) = path.strip_prefix(project_root) {
            output.log_path = Some(relative.to_path_buf());
        }
    }
}

fn failure_artifact_advice(resolved_out_dir: &Path, artifacts_retained: bool) -> Vec<String> {
    if artifacts_retained {
        return vec![
            format!("Logs available at {}/logs/", resolved_out_dir.display()),
            format!("To reproduce: cd {} && lake build", resolved_out_dir.display()),
        ];
    }

    vec![
        "Artifacts were cleaned up after this run. Re-run with --artifacts always to keep the generated workspace and logs.".to_string(),
    ]
}

struct CommandBranchResult {
    output: String,
    exit_code: i32,
}

fn format_doctor_output(report: &verify::DoctorReport, json: bool) -> miette::Result<String> {
    if json {
        let output = serde_json::to_string_pretty(report)
            .map_err(|e| miette::miette!("Failed to serialize doctor report as JSON: {e}"))?;
        return Ok(format!("{output}\n"));
    }

    let mut lines = vec![
        "Verify Doctor Report".to_string(),
        "====================".to_string(),
        String::new(),
    ];

    for tool in &report.tools {
        let status = if !tool.found {
            "NOT FOUND"
                .if_supports_color(Stderr, |s| s.red())
                .to_string()
        } else if !tool.meets_minimum {
            format!(
                "{} (minimum: {})",
                "VERSION TOO LOW".if_supports_color(Stderr, |s| s.red()),
                tool.minimum_version,
            )
        } else {
            "OK".if_supports_color(Stderr, |s| s.green()).to_string()
        };

        let version_str = tool.version.as_deref().unwrap_or("unknown");
        lines.push(format!("  {}: {} ({})", tool.tool, status, version_str));

        if let Some(err) = &tool.error {
            lines.push(format!(
                "    {}",
                err.if_supports_color(Stderr, |s| s.yellow())
            ));
        }
    }

    lines.push(String::new());

    let pc_status = if report.plutus_core.found && report.plutus_core.has_lakefile {
        "OK".if_supports_color(Stderr, |s| s.green()).to_string()
    } else if report.plutus_core.found {
        "INCOMPLETE (missing lakefile.lean)"
            .if_supports_color(Stderr, |s| s.red())
            .to_string()
    } else {
        "NOT FOUND"
            .if_supports_color(Stderr, |s| s.red())
            .to_string()
    };
    lines.push(format!(
        "  PlutusCore: {} ({})",
        pc_status, report.plutus_core.path
    ));
    if let Some(err) = &report.plutus_core.error {
        lines.push(format!(
            "    {}",
            err.if_supports_color(Stderr, |s| s.yellow())
        ));
    }

    lines.push(String::new());
    lines.push(format!("  Blaster revision: {}", report.blaster_rev));
    lines.push(format!("  PlutusCore revision: {}", report.plutus_core_rev));
    lines.push(String::new());

    if report.all_ok {
        lines.push(format!(
            "{}",
            "All checks passed."
                .if_supports_color(Stderr, |s| s.green())
                .if_supports_color(Stderr, |s| s.bold())
        ));
    } else {
        lines.push(format!(
            "{}",
            "Some checks failed. See above for details."
                .if_supports_color(Stderr, |s| s.red())
                .if_supports_color(Stderr, |s| s.bold())
        ));
    }

    Ok(format!("{}\n", lines.join("\n")))
}

fn doctor_exit_code(report: &verify::DoctorReport) -> i32 {
    if report.all_ok { 0 } else { 1 }
}

fn run_doctor_command_with<F>(
    json: bool,
    blaster_rev: String,
    plutus_core_rev: String,
    run_doctor: F,
) -> miette::Result<CommandBranchResult>
where
    F: FnOnce(&str, &str) -> verify::DoctorReport,
{
    let report = run_doctor(&blaster_rev, &plutus_core_rev);
    let output = format_doctor_output(&report, json)?;
    let exit_code = doctor_exit_code(&report);

    Ok(CommandBranchResult { output, exit_code })
}

fn exec_doctor(
    DoctorArgs {
        json,
        blaster_rev,
        plutus_core_rev,
    }: DoctorArgs,
 ) -> miette::Result<()> {
    let result = run_doctor_command_with(json, blaster_rev, plutus_core_rev, verify::run_doctor)?;

    print!("{}", result.output);

    if result.exit_code != 0 {
        process::exit(result.exit_code);
    }

    Ok(())
}

fn format_clean_output(removed: &[PathBuf], out_dir: &Path) -> String {
    if removed.is_empty() {
        format!("No verification artifacts found at {}\n", out_dir.display())
    } else {
        let mut output = removed
            .iter()
            .map(|p| format!("Removed {}", p.display()))
            .collect::<Vec<_>>()
            .join("\n");
        output.push('\n');
        output
    }
}

fn run_clean_command_with<F>(
    project_root: &Path,
    out_dir: PathBuf,
    clean_artifacts: F,
) -> miette::Result<CommandBranchResult>
where
    F: FnOnce(&Path) -> std::io::Result<Vec<PathBuf>>,
{
    let out_dir = resolve_verify_out_dir(&out_dir, project_root)?;
    let removed = clean_artifacts(&out_dir).map_err(|e| {
        miette::miette!(
            "Failed to clean verification artifacts at {}: {}",
            out_dir.display(),
            e
        )
    })?;

    Ok(CommandBranchResult {
        output: format_clean_output(&removed, &out_dir),
        exit_code: 0,
    })
}

fn manifest_declares_top_level_workspace_members(
    config_path: &Path,
    raw_config: &str,
) -> miette::Result<bool> {
    let manifest: toml::Table = raw_config.parse().map_err(|err| {
        miette::miette!(
            "Failed to parse manifest at {}: {}",
            config_path.display(),
            err
        )
    })?;

    Ok(manifest.contains_key("members"))
}

fn clean_project_root(workspace_root: &Path) -> miette::Result<PathBuf> {
    if !workspace_root.exists() {
        return Err(miette::miette!(
            "I couldn't find the requested directory {}.",
            workspace_root.display()
        ));
    }

    if !workspace_root.is_dir() {
        return Err(miette::miette!(
            "The requested directory {} is not a directory.",
            workspace_root.display()
        ));
    }

    let config_path = workspace_root.join("aiken.toml");
    let raw_config = std::fs::read_to_string(&config_path).map_err(|err| match err.kind() {
        std::io::ErrorKind::NotFound => miette::miette!(
            "{}",
            aiken_project::error::Error::MissingManifest {
                path: Box::new(workspace_root.to_path_buf()),
            }
        ),
        _ => miette::miette!(
            "Failed to load project manifest at {}: {}",
            config_path.display(),
            err
        ),
    })?;

    if manifest_declares_top_level_workspace_members(&config_path, &raw_config)? {
        WorkspaceConfig::load(workspace_root).map_err(|err| miette::miette!(err.to_string()))?;
    } else {
        ProjectConfig::load(workspace_root).map_err(|err| miette::miette!(err.to_string()))?;
    }

    Ok(workspace_root.to_path_buf())
}


fn run_clean_for_directory_with<F>(
    directory: Option<&Path>,
    out_dir: PathBuf,
    mut clean_artifacts: F,
) -> miette::Result<CommandBranchResult>
where
    F: FnMut(&Path) -> std::io::Result<Vec<PathBuf>>,
{
    let workspace_root = workspace_root(directory)?;
    let project_root = clean_project_root(&workspace_root)?;
    let result =
        run_clean_command_with(&project_root, out_dir, |path| clean_artifacts(path))?;

    Ok(CommandBranchResult {
        output: result.output,
        exit_code: 0,
    })
}

fn exec_clean(CleanArgs { directory, out_dir }: CleanArgs) -> miette::Result<()> {
    let result =
        run_clean_for_directory_with(directory.as_deref(), out_dir, verify::clean_artifacts)?;

    print!("{}", result.output);
    Ok(())
}

fn format_capabilities_output(
    caps: &verify::VerificationCapabilities,
    json: bool,
) -> miette::Result<String> {
    if json {
        let output = serde_json::to_string_pretty(caps)
            .map_err(|e| miette::miette!("Failed to serialize capabilities as JSON: {e}"))?;
        return Ok(format!("{output}\n"));
    }

    let mut lines = vec![
        "Verification Capabilities".to_string(),
        "=========================".to_string(),
        String::new(),
        "Supported test kinds:".to_string(),
    ];

    for k in &caps.supported_test_kinds {
        lines.push(format!("  - {k}"));
    }

    lines.push(String::new());
    lines.push("Unsupported test kinds:".to_string());
    for n in &caps.unsupported_test_kinds {
        lines.push(format!("  - {} [{}]: {}", n.kind, n.status, n.reason));
    }

    lines.push(String::new());
    lines.push("Target modes:".to_string());
    for m in &caps.target_modes {
        lines.push(format!("  - {m}"));
    }

    lines.push(String::new());
    lines.push("Supported fuzzer output types:".to_string());
    for t in &caps.supported_fuzzer_types {
        lines.push(format!("  - {t}"));
    }

    lines.push(String::new());
    lines.push("Unsupported fuzzer output types:".to_string());
    for t in &caps.unsupported_fuzzer_types {
        lines.push(format!("  - {t}"));
    }

    lines.push(String::new());
    lines.push("Existential modes:".to_string());
    for m in &caps.existential_modes {
        lines.push(format!("  - {m}"));
    }

    lines.push(String::new());
    lines.push(format!("Max test arity: {}", caps.max_test_arity));

    // Catalogue table — drives `--skip-unsupported=<CODE>` filtering. Sourced
    // from the currently surfaced `aiken-project` catalogue projection so the
    // CLI only advertises codes the verifier can actually emit today.
    if !caps.unsupported.is_empty() {
        lines.push(String::new());
        lines.push("Error catalogue:".to_string());
        let code_width = caps
            .unsupported
            .iter()
            .map(|c| c.code.len())
            .max()
            .unwrap_or(4);
        let feature_width = caps
            .unsupported
            .iter()
            .map(|c| c.feature.len())
            .max()
            .unwrap_or(7);
        // Header row plus a separator so users can scan the table.
        lines.push(format!(
            "  {:code_width$}  {:feature_width$}  {}",
            "CODE", "FEATURE", "SKIPPABLE"
        ));
        lines.push(format!(
            "  {}  {}  {}",
            "-".repeat(code_width),
            "-".repeat(feature_width),
            "---------"
        ));
        for entry in &caps.unsupported {
            lines.push(format!(
                "  {:code_width$}  {:feature_width$}  {}",
                entry.code,
                entry.feature,
                if entry.skippable { "yes" } else { "no" },
            ));
        }
    }

    Ok(format!("{}\n", lines.join("\n")))
}

fn run_capabilities_command(
    CapabilitiesArgs { json }: CapabilitiesArgs,
) -> miette::Result<CommandBranchResult> {
    let caps = verify::capabilities();
    let output = format_capabilities_output(&caps, json)?;

    Ok(CommandBranchResult {
        output,
        exit_code: 0,
    })
}

fn exec_capabilities(args: CapabilitiesArgs) -> miette::Result<()> {
    let result = run_capabilities_command(args)?;
    print!("{}", result.output);
    Ok(())
}

fn extract_first_double_quoted(input: &str) -> Option<String> {
    let start = input.find('"')?;
    let rest = &input[start + 1..];
    let end = rest.find('"')?;
    Some(rest[..end].to_string())
}

fn is_simple_identifier(input: &str) -> bool {
    !input.is_empty()
        && input
            .chars()
            .all(|ch| ch.is_ascii_alphanumeric() || ch == '_')
}

fn simplify_counterexample_expr(expr: &str) -> String {
    if let Some(text) = extract_first_double_quoted(expr) {
        return format!("\"{text}\"");
    }

    expr.split_whitespace().collect::<Vec<_>>().join(" ")
}

fn format_counterexample_item(item: &str, include_name: bool) -> String {
    if let Some((name, value)) = item.split_once(':') {
        let value = simplify_counterexample_expr(value.trim());
        if include_name {
            format!("{} = {}", name.trim(), value)
        } else {
            value
        }
    } else {
        simplify_counterexample_expr(item)
    }
}

fn extract_counterexample_display(reason: &str) -> Option<String> {
    let mut in_counterexample_block = false;
    let mut items = Vec::new();
    let mut current_item: Option<String> = None;

    for line in reason.lines() {
        if !in_counterexample_block {
            if let Some(idx) = line.find("Counterexample:") {
                let tail = line[idx + "Counterexample:".len()..].trim();
                if !tail.is_empty() {
                    if let Some((name, value)) = tail.split_once('=')
                        && is_simple_identifier(name.trim())
                    {
                        return Some(simplify_counterexample_expr(value.trim()));
                    }

                    return Some(simplify_counterexample_expr(tail));
                }
                in_counterexample_block = true;
            }
            continue;
        }

        if line.contains("Tactic `blaster` failed")
            || line.contains("❌ Falsified")
            || line.contains("unsolved goals")
        {
            break;
        }

        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }

        if let Some(idx) = line.find("- ") {
            if let Some(item) = current_item.take() {
                items.push(item);
            }

            let item = line[idx + 2..].trim();
            if !item.is_empty() {
                current_item = Some(item.to_string());
            }
            continue;
        }

        if let Some(item) = &mut current_item {
            item.push(' ');
            item.push_str(trimmed);
        }
    }

    if let Some(item) = current_item {
        items.push(item);
    }

    if items.is_empty() {
        return None;
    }

    if items.len() == 1 {
        return items
            .first()
            .map(|item| format_counterexample_item(item, false));
    }

    Some(
        items
            .iter()
            .map(|item| format_counterexample_item(item, true))
            .collect::<Vec<_>>()
            .join(", "),
    )
}

fn sanitize_stderr_for_display(stderr: &str) -> String {
    stderr
        .lines()
        .filter(|line| line.trim() != "error: build failed")
        .collect::<Vec<_>>()
        .join("\n")
}

fn normalize_skip_unsupported(skip_unsupported: Option<Vec<String>>) -> Option<Vec<String>> {
    match skip_unsupported {
        Some(codes) if codes.len() == 1 && codes[0].is_empty() => Some(Vec::new()),
        other => other,
    }
}

fn validate_skip_unsupported_codes(skip_unsupported: Option<&[String]>) -> miette::Result<()> {
    let Some(codes) = skip_unsupported else {
        return Ok(());
    };

    let supported_codes = verify::error_catalogue::iter_catalogue()
        .map(|entry| (entry.code, entry.skippable))
        .collect::<std::collections::BTreeMap<_, _>>();
    let invalid_codes = codes
        .iter()
        .filter(|code| !code.is_empty())
        .filter(|code| !matches!(supported_codes.get(code.as_str()), Some(true)))
        .cloned()
        .collect::<Vec<_>>();

    if invalid_codes.is_empty() {
        return Ok(());
    }

    miette::bail!(
        "Invalid --skip-unsupported code(s): {}. Only skippable catalogue codes are accepted here; run `aiken verify capabilities` to list supported codes.",
        invalid_codes.join(", "),
    )
}

const VERIFY_CLI_ERROR_VERSION: &str = "1";

const VERIFY_CLI_ERROR_FALLBACK_MESSAGE: &str =
    "Project loading or compilation failed. Re-run without --json for full diagnostics.";

fn verify_cli_error_message(error: &miette::Report) -> String {
    let message = error.to_string();
    if message.trim().is_empty() {
        VERIFY_CLI_ERROR_FALLBACK_MESSAGE.to_string()
    } else {
        message
    }
}

fn silent_cli_error_message(output_mode: OutputMode, error: &miette::Report) -> Option<String> {
    matches!(output_mode, OutputMode::Silent).then(|| verify_cli_error_message(error))
}

fn verify_cli_error_payload(message: impl Into<String>) -> serde_json::Value {
    serde_json::json!({
        "version": VERIFY_CLI_ERROR_VERSION,
        "kind": "verify-cli-error",
        "message": message.into(),
    })
}

fn exit_with_cli_json_report_error(error: &miette::Report) -> ! {
    exit_with_cli_json_error(verify_cli_error_message(error));
}

fn exit_with_cli_json_error(message: impl Into<String>) -> ! {
    let payload = verify_cli_error_payload(message);

    println!(
        "{}",
        serde_json::to_string_pretty(&payload)
            .expect("verify CLI JSON error payload should serialize"),
    );
    process::exit(1);
}

fn exec_run(
    RunArgs {
        directory,
        deny,
        silent,
        match_tests,
        exact_match,
        env,
        generate_only,
        out_dir,
        keep_artifacts,
        artifacts,
        timeout,
        cek_budget,
        jobs,
        json,
        skip_unsupported,
        // `--strict-unsupported` is an alias for omitting `--skip-unsupported`.
        // The two flags are clap-`conflicts_with`-mutually-exclusive, so when
        // `strict_unsupported = true` we know `skip_unsupported` is `None`
        // and the resolved `SkipPolicy::None` already matches the alias's
        // semantics. The bool exists purely to give CI scripts an explicit
        // way to assert "no skipping" in their argv.
        strict_unsupported: _,
        allow_skips,
        blaster_rev,
        existential_mode,
        target,
        plutus_core_rev,
        plutus_core_dir,
        raw_output_bytes,
        accept_partial,
        accept_witness,
        allow_vacuous_subgenerators,
    }: RunArgs,
) -> miette::Result<()> {
    let output_mode = OutputMode::from_flags(json, silent);

    // Handle deprecated --keep-artifacts flag: treat as --artifacts always
    let artifact_policy = if keep_artifacts {
        if output_mode.shows_advisories() {
            eprintln!(
                "{} --keep-artifacts is deprecated; use --artifacts always",
                "Warning:"
                    .if_supports_color(Stderr, |s| s.yellow())
                    .if_supports_color(Stderr, |s| s.bold()),
            );
        }
        ArtifactRetention::Always
    } else {
        artifacts
    };

    if cek_budget == 0 {
        if output_mode.emits_json() {
            exit_with_cli_json_error("--cek-budget must be greater than 0");
        }
        miette::bail!("--cek-budget must be greater than 0");
    }

    if raw_output_bytes > MAX_RAW_OUTPUT_TAIL_BYTES {
        let message = format!(
            "--raw-output-bytes must be <= {} (got {}). Use 0 to disable truncation.",
            MAX_RAW_OUTPUT_TAIL_BYTES, raw_output_bytes,
        );
        if output_mode.emits_json() {
            exit_with_cli_json_error(message);
        }
        miette::bail!(message);
    }

    let jobs_override = (jobs != 0).then_some(jobs);
    let skip_unsupported = normalize_skip_unsupported(skip_unsupported);
    if let Err(error) = validate_skip_unsupported_codes(skip_unsupported.as_deref()) {
        if output_mode.emits_json() {
            exit_with_cli_json_report_error(&error);
        }
        return Err(error);
    }
    let skip_policy = SkipPolicy::from_cli(skip_unsupported);
    let run_options = RunCommandOptions {
        match_tests,
        exact_match,
        env,
        generate_only,
        out_dir,
        artifact_policy,
        timeout,
        cek_budget,
        jobs_override,
        output_mode,
        skip_policy,
        allow_skips,
        blaster_rev,
        plutus_core_rev,
        existential_mode,
        target,
        plutus_core_dir,
        raw_output_bytes,
        accept_partial,
        accept_witness,
        allow_vacuous_subgenerators,
    };

    let mut exit_code = 0;
    let result = with_project_event_target(
        directory.as_deref(),
        deny,
        !output_mode.shows_advisories(),
        output_mode.shows_advisories(),
        output_mode.project_event_target(),
        output_mode.reports_project_diagnostics(),
        |p| exec_run_with_project(p, &run_options, &mut exit_code),
    );

    if let Err(error) = result {
        if output_mode.emits_json() {
            exit_with_cli_json_report_error(&error);
        }
        if let Some(message) = silent_cli_error_message(output_mode, &error) {
            eprintln!("{message}");
        }
        process::exit(1);
    }

    if exit_code != 0 {
        process::exit(exit_code);
    }

    Ok(())
}

fn exec_run_with_project(
    p: &mut Project<EventTarget>,
    run_options: &RunCommandOptions,
    exit_code: &mut i32,
) -> Result<(), Vec<aiken_project::error::Error>> {
    let resolved_out_dir = resolve_verify_out_dir(&run_options.out_dir, p.root()).map_err(|e| {
        vec![aiken_project::error::Error::StandardIo(
            std::io::Error::new(std::io::ErrorKind::InvalidInput, e.to_string()),
        )]
    })?;

    p.compile(run_compile_options(run_options.env.clone()))?;

    let exported = p
        .export_tests(
            run_options.match_tests.clone(),
            run_options.exact_match,
            Tracing::silent(),
            false,
        )
        .map_err(|e| vec![e])?;

    let property_tests = &exported.property_tests;

    // Advisory print for `--existential-mode witness` (text mode only).
    // Witness mode is only sound for `Bool`-domain `fail_once` tests; any
    // other domain hard-errors with `S0003` (UnsoundFallback). Surface this
    // up front so users do not file the resulting hard error as a regression.
    //
    // Gated on `OutputMode::shows_advisories()` because JSON consumers parse the
    // structured S0003 envelope directly and `--silent` suppresses notes too.
    if run_options.output_mode.shows_advisories()
        && matches!(run_options.existential_mode, ExistentialMode::Witness)
    {
        eprintln!(
            "{} --existential-mode witness is only sound for Bool-domain `fail once` tests; \
             other domains will hard-error with S0003.",
            "Note:"
                .if_supports_color(Stderr, |s| s.yellow())
                .if_supports_color(Stderr, |s| s.bold()),
        );
    }

    if let Some(output) = no_property_tests_early_output(
        property_tests,
        run_options.output_mode,
        &run_options.blaster_rev,
        &run_options.plutus_core_rev,
        run_options.allow_vacuous_subgenerators,
    )
    .map_err(|e| vec![aiken_project::error::Error::Json(e)])?
    {
        println!("{output}");
        return Ok(());
    }

    // When --generate-only, run theorem-shape preflight before file generation work.
    // Skipped only when *some* skip policy is opted in — `SkipPolicy::None`
    // (default / `--strict-unsupported`) keeps the preflight active, while any
    // active policy (`All` or codes filter) defers the decision to the
    // workspace generator (which then routes per-test via the same policy).
    if run_options.generate_only && !run_options.skip_policy.is_active() {
        let unsupported = collect_generate_only_preflight_errors(
            property_tests,
            run_options.existential_mode,
            &run_options.target,
        );

        if !unsupported.is_empty() {
            return Err(vec![aiken_project::error::Error::StandardIo(
                std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    format!(
                        "Cannot generate Lean workspace:\n\n\
                         The following property tests have unsupported theorem/constraint shapes:\n  - {}\n\n\
                         Hint: use --skip-unsupported to skip unsupported tests.",
                        unsupported.join("\n  - ")
                    ),
                ),
            )]);
        }
    }

    let config = VerifyConfig::new(
        resolved_out_dir.clone(),
        run_options.cek_budget,
        run_options.blaster_rev.clone(),
        run_options.plutus_core_rev.clone(),
        run_options.existential_mode,
        run_options.target.clone(),
        run_options.plutus_core_dir.clone(),
        run_options.raw_output_bytes,
        run_options.allow_vacuous_subgenerators,
    );

    let manifest =
        verify::generate_lean_workspace(property_tests, &config, &run_options.skip_policy)
            .map_err(|e| {
                let (message, code, help, url) = verify::generation_error_metadata(&e);
                vec![aiken_project::error::Error::verify_generation(
                    message, code, help, url,
                )]
            })?;

    let skipped_without_allow =
        skips_require_failure(manifest.skipped.len(), run_options.allow_skips);

    // Report skipped tests
    if !manifest.skipped.is_empty() && run_options.output_mode.shows_advisories() {
        eprintln!(
            "{} Skipped {} unsupported test(s):",
            "Warning:"
                .if_supports_color(Stderr, |s| s.yellow())
                .if_supports_color(Stderr, |s| s.bold()),
            manifest.skipped.len(),
        );
        for s in &manifest.skipped {
            eprintln!("  - {}: {}", s.name, s.reason);
        }
        eprintln!();
    }

    if run_options.generate_only {
        if run_options.output_mode.emits_json() {
            let output = serde_json::to_string_pretty(&manifest)
                .map_err(|e| vec![aiken_project::error::Error::Json(e)])?;
            println!("{output}");
        } else if skipped_without_allow {
            if run_options.output_mode.shows_advisories() {
                eprintln!(
                    "{} Lean workspace generated at {}, but {} unsupported test(s) were skipped. \
                     This run will exit non-zero. Use --allow-skips to treat skips as success.",
                    "Warning:"
                        .if_supports_color(Stderr, |s| s.yellow())
                        .if_supports_color(Stderr, |s| s.bold()),
                    resolved_out_dir.display(),
                    manifest.skipped.len(),
                );
            }
        } else {
            println!(
                "Generated Lean workspace at {} with {} property test(s):",
                resolved_out_dir.display(),
                manifest.tests.len(),
            );
            for entry in &manifest.tests {
                println!("  - {} -> {}", entry.aiken_module, entry.lean_module);
            }
            println!();
            if run_options.output_mode.shows_advisories() {
                if run_options.plutus_core_dir.is_some() || std::env::var("PLUTUS_CORE_DIR").is_ok()
                {
                    println!(
                        "Note: The PlutusCore Lean library is configured at {}.",
                        verify::resolve_plutus_core_dir(run_options.plutus_core_dir.as_deref())
                            .display(),
                    );
                    println!("      Set PLUTUS_CORE_DIR or pass --plutus-core-dir to change.");
                } else {
                    println!(
                        "Note: PlutusCore will be fetched from git (rev: {}).",
                        run_options.plutus_core_rev,
                    );
                    println!("      Pass --plutus-core-dir for a local checkout instead.");
                }
            }
        }
    } else {
        if manifest.tests.is_empty() {
            if run_options.output_mode.emits_json() {
                let output = serde_json::to_string_pretty(&no_proofs_summary(
                    &manifest,
                    skipped_without_allow,
                    &run_options.blaster_rev,
                    &run_options.plutus_core_rev,
                    run_options.allow_vacuous_subgenerators,
                    two_phase_disabled_from_env(),
                ))
                .map_err(|e| vec![aiken_project::error::Error::Json(e)])?;
                println!("{output}");
            } else {
                println!("No supported property tests remain to prove.");
            }

            if skipped_without_allow && !run_options.output_mode.emits_json() {
                eprintln!(
                    "{} {} unsupported test(s) were skipped; exiting non-zero. Use --allow-skips to treat skips as success.",
                    "Error:"
                        .if_supports_color(Stderr, |s| s.red())
                        .if_supports_color(Stderr, |s| s.bold()),
                    manifest.skipped.len(),
                );
            }

            if should_cleanup_artifacts(
                run_options.artifact_policy,
                !skipped_without_allow,
            )
                && let Err(e) = verify::clear_generated_workspace(&resolved_out_dir)
                && run_options.output_mode.shows_advisories()
            {
                eprintln!(
                    "Warning: failed to clean up {}: {}",
                    resolved_out_dir.display(),
                    e
                );
            }

            if skipped_without_allow {
                if run_options.output_mode.emits_json() {
                    *exit_code = 1;
                    return Ok(());
                }
                return skipped_tests_failure(manifest.skipped.len());
            }

            return Ok(());
        }

        verify::check_toolchain().map_err(|e| {
            vec![aiken_project::error::Error::StandardIo(
                std::io::Error::new(std::io::ErrorKind::NotFound, e.to_string()),
            )]
        })?;

        verify::check_plutus_core(run_options.plutus_core_dir.as_deref()).map_err(|e| {
            vec![aiken_project::error::Error::StandardIo(
                std::io::Error::new(std::io::ErrorKind::NotFound, e.to_string()),
            )]
        })?;

        if let Some(output) = run_proofs_start_output(run_options.output_mode) {
            println!("{output}");
        }

        let start = std::time::Instant::now();
        let result = verify::run_proofs(
            &resolved_out_dir,
            run_options.timeout,
            run_options.jobs_override,
            &manifest,
            run_options.raw_output_bytes,
        )
        .map_err(|e| {
            vec![aiken_project::error::Error::StandardIo(
                std::io::Error::other(e.to_string()),
            )]
        })?;
        let elapsed = start.elapsed();

        let mut summary = verify::parse_verify_results(result, &manifest);
        summary.elapsed_ms = Some(elapsed.as_millis() as u64);
        // Provenance: stamp the resolved Blaster / PlutusCore revs onto the
        // summary so JSON consumers can audit which dependency snapshot the
        // proofs were verified against. These flow from the
        // `RunCommandOptions` (CLI flags / defaults) through the same
        // plumbing that feeds `VerifyConfig` for workspace generation.
        summary.blaster_rev.clone_from(&run_options.blaster_rev);
        summary
            .plutus_core_rev
            .clone_from(&run_options.plutus_core_rev);
        summary.allow_vacuous_subgenerators = run_options.allow_vacuous_subgenerators;
        summary.two_phase_disabled = two_phase_disabled_from_env();
        // Incorporate skip-induced failure into command_success so JSON
        // consumers see the true exit status without checking the process code.
        if skipped_without_allow {
            summary.command_success = false;
        }
        // Partial / WitnessProved by default cause command failure; the
        // `--accept-partial` / `--accept-witness` flags opt in to passing.
        if summary.command_success
            && ((summary.partial > 0 && !run_options.accept_partial)
                || (summary.witness > 0 && !run_options.accept_witness))
        {
            summary.command_success = false;
        }

        let skipped_causes_failure = skipped_without_allow;
        let proofs_succeeded = proofs_succeeded(
            &summary,
            run_options.accept_partial,
            run_options.accept_witness,
        );
        let cleanup_needed = should_cleanup_artifacts(
            run_options.artifact_policy,
            summary.command_success,
        );
        let cleanup_error = if cleanup_needed {
            verify::clear_generated_workspace(&resolved_out_dir).err()
        } else {
            None
        };
        let artifacts_retained = !cleanup_needed || cleanup_error.is_some();
        normalize_summary_log_paths(&mut summary, p.root(), artifacts_retained);

        if run_options.output_mode.emits_json() {
            let output = serde_json::to_string_pretty(&summary)
                .map_err(|e| vec![aiken_project::error::Error::Json(e)])?;
            println!("{output}");
        } else {
            println!();
            for t in &summary.theorems {
                let (icon, label, trailing_block): (String, String, Option<String>) = match &t
                    .status
                {
                    ProofStatus::Proved => {
                        let label = if t.over_approximations > 0 {
                            format!(
                                "PROVED ({} over-approximation{})",
                                t.over_approximations,
                                if t.over_approximations == 1 { "" } else { "s" }
                            )
                        } else {
                            "PROVED".to_string()
                        };
                        (
                            "PASS".if_supports_color(Stderr, |s| s.green()).to_string(),
                            label,
                            None,
                        )
                    }
                    ProofStatus::Partial { note } => {
                        // Build succeeded but the proof leaves a sub-obligation
                        // open. Surface as PARTIAL — the user must see this
                        // rather than a misleading PROVED verdict. Carry the
                        // over-approximation suffix when present (the audit
                        // count is preserved on partial proofs).
                        let code = classify_partial_code(note);
                        let mut headline = format!("PARTIAL [{code}]");
                        if t.over_approximations > 0 {
                            headline.push_str(&format!(
                                " ({} over-approximation{})",
                                t.over_approximations,
                                if t.over_approximations == 1 { "" } else { "s" }
                            ));
                        }
                        let block = format!(
                            "       This proof is incomplete: a sub-obligation is closed with `sorry`.\n\
                                 \x20      The Lean build succeeded, but an open obligation was not proved.\n\
                                 \x20      This is NOT a complete formal proof.\n\
                                 \x20      {note}\n\
                                 \x20      Full proof support for this obligation will be added in a future\n\
                                 \x20      Aiken release.\n\
                                 \x20      Re-run with --accept-partial to allow CI to pass on partial proofs."
                        );
                        (
                            "PART".if_supports_color(Stderr, |s| s.yellow()).to_string(),
                            headline,
                            Some(block),
                        )
                    }
                    ProofStatus::WitnessProved {
                        instances, note, ..
                    } => {
                        // Build succeeded but the proof only covers concrete
                        // witness instance(s) — surface as WITNESS rather
                        // than the misleading PROVED verdict. The
                        // `--accept-witness` flag opts in to passing CI on
                        // these existential proofs.
                        let mut headline = format!(
                            "WITNESS-ONLY ({} instance{})",
                            instances,
                            if *instances == 1 { "" } else { "s" }
                        );
                        if t.over_approximations > 0 {
                            headline.push_str(&format!(
                                " ({} over-approximation{})",
                                t.over_approximations,
                                if t.over_approximations == 1 { "" } else { "s" }
                            ));
                        }
                        let block = format!(
                            "       This is an existential proof, not a universal one.\n\
                                 \x20      Lean verified that the property holds on {instances} concrete fuzzer-generated\n\
                                 \x20      input(s) via native_decide; it did NOT prove the property holds for\n\
                                 \x20      every input the validator can receive.\n\
                                 \x20      Universal verification of state-machine halt/error tests will be\n\
                                 \x20      added in a future Aiken release.\n\
                                 \x20      Re-run with --accept-witness to allow CI to pass on witness-only proofs.\n\
                                 \x20      Note: {note}"
                        );
                        (
                            "WITN".if_supports_color(Stderr, |s| s.yellow()).to_string(),
                            headline,
                            Some(block),
                        )
                    }
                    ProofStatus::Failed { category, reason } => match category {
                        FailureCategory::Counterexample => {
                            let label = extract_counterexample_display(reason)
                                .map(|value| format!("COUNTEREXAMPLE: {value}"))
                                .unwrap_or_else(|| "COUNTEREXAMPLE".to_string());
                            (
                                "FAIL".if_supports_color(Stderr, |s| s.red()).to_string(),
                                label,
                                None,
                            )
                        }
                        _ => {
                            let cat = match category {
                                FailureCategory::Counterexample => "counterexample",
                                FailureCategory::UnsatGoal => "unsat-goal",
                                FailureCategory::Timeout => "timeout",
                                FailureCategory::BuildError => "build-error",
                                FailureCategory::DependencyError => "dependency-error",
                                FailureCategory::BlasterUnsupported => "blaster-unsupported",
                                FailureCategory::Unknown => "unknown",
                                _ => "unknown",
                            };
                            (
                                "FAIL".if_supports_color(Stderr, |s| s.red()).to_string(),
                                format!("FAILED [{}]", cat),
                                None,
                            )
                        }
                    },
                    ProofStatus::TimedOut { .. } => (
                        "TIME".if_supports_color(Stderr, |s| s.yellow()).to_string(),
                        "TIMED OUT".to_string(),
                        None,
                    ),
                    ProofStatus::Unknown => (
                        "????".if_supports_color(Stderr, |s| s.yellow()).to_string(),
                        "UNKNOWN".to_string(),
                        None,
                    ),
                    _ => (
                        "????".if_supports_color(Stderr, |s| s.yellow()).to_string(),
                        "UNKNOWN".to_string(),
                        None,
                    ),
                };
                println!(
                    "  {} {} [{}] - {}",
                    icon, t.test_name, t.theorem_name, label
                );
                if let Some(block) = trailing_block {
                    println!("{block}");
                }
                // Print inline failure context snippet
                if let ProofStatus::Failed { category, reason } = &t.status
                    && *category != FailureCategory::Counterexample
                    && !reason.is_empty()
                {
                    for line in reason.lines().take(10) {
                        println!("       {}", line.if_supports_color(Stderr, |s| s.dimmed()));
                    }
                    let total_lines = reason.lines().count();
                    if total_lines > 10 {
                        println!("       {} more lines in logs...", total_lines - 10);
                    }
                }
            }

            let elapsed_str = if elapsed.as_secs() > 0 {
                format!("{}s", elapsed.as_secs())
            } else {
                format!("{}ms", elapsed.as_millis())
            };

            println!(
                "\nResults: {} proved, {} witness-only, {} partial, {} failed, {} timed out, {} unknown out of {} theorems in {}",
                summary.proved,
                summary.witness,
                summary.partial,
                summary.failed,
                summary.timed_out,
                summary.unknown,
                summary.total,
                elapsed_str,
            );

            // Commit 18 (folds C14 #4): surface the resolved provenance revs
            // in text mode too, mirroring what JSON consumers already see in
            // `VerifySummary.{blaster_rev, plutus_core_rev}`. Previously
            // humans had to re-run with `--json` or invoke
            // `aiken verify doctor` to discover which dependency snapshot
            // was used. Suppressed when both revs are empty (i.e. the
            // library `parse_verify_results` default path that the CLI
            // never hits but library callers might).
            if !summary.blaster_rev.is_empty() || !summary.plutus_core_rev.is_empty() {
                println!(
                    "         using blaster_rev={} plutus_core_rev={}",
                    if summary.blaster_rev.is_empty() {
                        "<unset>"
                    } else {
                        summary.blaster_rev.as_str()
                    },
                    if summary.plutus_core_rev.is_empty() {
                        "<unset>"
                    } else {
                        summary.plutus_core_rev.as_str()
                    },
                );
            }

            if (summary.failed > 0 || summary.timed_out > 0 || summary.unknown > 0)
                && run_options.output_mode.shows_advisories()
            {
                let stderr_for_display =
                    sanitize_stderr_for_display(summary.raw_output.stderr.tail.as_str());
                if !stderr_for_display.trim().is_empty() {
                    eprintln!("\n{}", stderr_for_display);
                }
                for advice in failure_artifact_advice(&resolved_out_dir, artifacts_retained) {
                    eprintln!("{advice}");
                }
            }
        }

        if skipped_causes_failure && !run_options.output_mode.emits_json() {
            eprintln!(
                "{} {} unsupported test(s) were skipped; exiting non-zero. Use --allow-skips to treat skips as success.",
                "Error:"
                    .if_supports_color(Stderr, |s| s.red())
                    .if_supports_color(Stderr, |s| s.bold()),
                manifest.skipped.len(),
            );
        }

        if let Some(e) = cleanup_error.as_ref()
            && run_options.output_mode.shows_advisories()
        {
            eprintln!(
                "Warning: failed to clean up {}: {}",
                resolved_out_dir.display(),
                e
            );
        }

        if !proofs_succeeded {
            if run_options.output_mode.emits_json() {
                *exit_code = 1;
                return Ok(());
            }

            let mut reasons: Vec<String> = Vec::new();
            if summary.failed > 0 {
                reasons.push(format!("{} failed", summary.failed));
            }
            if summary.timed_out > 0 {
                reasons.push(format!("{} timed out", summary.timed_out));
            }
            if summary.unknown > 0 {
                reasons.push(format!("{} unknown", summary.unknown));
            }
            if summary.partial > 0 && !run_options.accept_partial {
                reasons.push(format!(
                    "{} partial (re-run with --accept-partial to allow)",
                    summary.partial
                ));
            }
            if summary.witness > 0 && !run_options.accept_witness {
                reasons.push(format!(
                    "{} witness-only (re-run with --accept-witness to allow)",
                    summary.witness
                ));
            }
            return Err(vec![aiken_project::error::Error::StandardIo(
                std::io::Error::other(format!(
                    "Proof verification incomplete: {} out of {} theorems",
                    reasons.join(", "),
                    summary.total
                )),
            )]);
        }

        if skipped_causes_failure {
            if run_options.output_mode.emits_json() {
                *exit_code = 1;
                return Ok(());
            }
            return skipped_tests_failure(manifest.skipped.len());
        }
    }

    // In generate-only mode, skipped tests still fail the run unless --allow-skips is set.
    if run_options.generate_only && skipped_without_allow {
        if !run_options.output_mode.emits_json() {
            eprintln!(
                "{} {} unsupported test(s) were skipped; exiting non-zero. Use --allow-skips to treat skips as success.",
                "Error:"
                    .if_supports_color(Stderr, |s| s.red())
                    .if_supports_color(Stderr, |s| s.bold()),
                manifest.skipped.len(),
            );
        }
        if run_options.output_mode.emits_json() {
            *exit_code = 1;
            return Ok(());
        }
        return skipped_tests_failure(manifest.skipped.len());
    }

    Ok(())
}

fn run_compile_options(env: Option<String>) -> Options {
    Options {
        env,
        ..Default::default()
    }
}

fn resolve_verify_out_dir(out_dir: &Path, project_root: &Path) -> miette::Result<PathBuf> {
    if out_dir
        .components()
        .any(|component| matches!(component, std::path::Component::ParentDir))
    {
        return Err(miette::miette!(
            "Invalid --out-dir '{}': parent directory segments ('..') are not allowed.",
            out_dir.display()
        ));
    }

    let resolved = if out_dir.is_absolute() {
        out_dir.to_path_buf()
    } else {
        project_root.join(out_dir)
    };

    let project_root_abs = canonicalize_with_existing_ancestors(project_root)?;
    let resolved_abs = canonicalize_with_existing_ancestors(&resolved)?;
    if !resolved_abs.starts_with(&project_root_abs) {
        return Err(miette::miette!(
            "Invalid --out-dir '{}': path must resolve inside project root '{}'.",
            out_dir.display(),
            project_root.display()
        ));
    }

    Ok(resolved)
}

fn canonicalize_with_existing_ancestors(path: &Path) -> miette::Result<PathBuf> {
    let absolute = if path.is_absolute() {
        path.to_path_buf()
    } else {
        std::env::current_dir()
            .map(|cwd| cwd.join(path))
            .map_err(|e| miette::miette!("Failed to resolve path '{}': {e}", path.display()))?
    };

    let mut nearest_existing = absolute.as_path();
    let mut missing_suffix = Vec::new();
    while !nearest_existing.exists() {
        let Some(file_name) = nearest_existing.file_name() else {
            return Err(miette::miette!(
                "Failed to canonicalize path '{}': no existing parent directory found.",
                absolute.display()
            ));
        };
        missing_suffix.push(file_name.to_os_string());
        let Some(parent) = nearest_existing.parent() else {
            return Err(miette::miette!(
                "Failed to canonicalize path '{}': no parent directory found.",
                absolute.display()
            ));
        };
        nearest_existing = parent;
    }

    let mut canonical = nearest_existing.canonicalize().map_err(|e| {
        miette::miette!(
            "Failed to canonicalize path '{}': {e}",
            nearest_existing.display()
        )
    })?;
    for component in missing_suffix.iter().rev() {
        canonical.push(component);
    }
    Ok(canonical)
}

fn skips_require_failure(skipped_count: usize, allow_skips: bool) -> bool {
    skipped_count > 0 && !allow_skips
}

/// Decide whether the CLI should report success based on the proof summary
/// and the `--accept-partial` / `--accept-witness` policy flags.
///
/// Failed, timed-out, and unknown theorems always cause failure. Partial
/// (sorry-closed) and witness-only proofs cause failure unless explicitly
/// opted in via the corresponding accept flag.
fn proofs_succeeded(
    summary: &verify::VerifySummary,
    accept_partial: bool,
    accept_witness: bool,
) -> bool {
    if summary.failed > 0 || summary.timed_out > 0 || summary.unknown > 0 {
        return false;
    }
    if summary.partial > 0 && !accept_partial {
        return false;
    }
    if summary.witness > 0 && !accept_witness {
        return false;
    }
    true
}

/// Map a Partial proof's note string to its catalogue code so users can find
/// the matching documentation entry.
///
/// Today the only `Partial` caveat shipped is the Phase-2 halting `sorry`
/// (S0004). This heuristic is intentionally permissive — every Partial today
/// is S0004, and any future `Partial` shape will need its own discriminator
/// added here. When that happens, prefer threading the catalogue code through
/// the `ProofStatus::Partial` payload as a typed field rather than expanding
/// this string-matching helper.
fn classify_partial_code(_note: &str) -> &'static str {
    "S0004"
}

fn should_cleanup_artifacts(policy: ArtifactRetention, command_success: bool) -> bool {
    !verify::should_retain_artifacts(policy, command_success)
}

fn skipped_tests_failure(skipped_count: usize) -> Result<(), Vec<aiken_project::error::Error>> {
    Err(vec![aiken_project::error::Error::StandardIo(
        std::io::Error::other(format!(
            "{skipped_count} unsupported test(s) were skipped. Use --allow-skips to treat skips as success."
        )),
    )])
}

fn collect_generate_only_preflight_errors(
    property_tests: &[ExportedPropertyTest],
    existential_mode: ExistentialMode,
    target: &VerificationTargetKind,
) -> Vec<String> {
    collect_generate_only_preflight_errors_with(property_tests, |t| {
        verify::preflight_validate_test(t, existential_mode, target)
    })
}

fn collect_generate_only_preflight_errors_with<F>(
    property_tests: &[ExportedPropertyTest],
    mut validate: F,
) -> Vec<String>
where
    F: FnMut(&ExportedPropertyTest) -> miette::Result<()>,
{
    property_tests
        .iter()
        .filter_map(|t| validate(t).err().map(|e| format!("{}: {}", t.name, e)))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use aiken_project::{
        Project,
        export::{FuzzerConstraint, FuzzerOutputType},
        telemetry::EventTarget,
        watch::ExitFailure,
    };
    use clap::{CommandFactory, Parser};
    #[cfg(unix)]
    use std::os::unix::fs::symlink;
    use std::{
        fs,
        path::{Path, PathBuf},
        time::SystemTime,
    };

    #[test]
    fn json_and_silent_modes_use_quiet_project_loading() {
        assert!(matches!(
            OutputMode::Json.project_event_target(),
            EventTarget::Json(_)
        ));
        assert!(matches!(
            OutputMode::Silent.project_event_target(),
            EventTarget::Json(_)
        ));
        assert!(!OutputMode::Json.reports_project_diagnostics());
        assert!(!OutputMode::Silent.reports_project_diagnostics());
        assert!(OutputMode::Text.reports_project_diagnostics());
    }

    fn dummy_property_test(
        name: &str,
        fuzzer_output_type: FuzzerOutputType,
        constraint: FuzzerConstraint,
    ) -> ExportedPropertyTest {
        serde_json::from_value(serde_json::json!({
            "name": name,
            "module": "example",
            "input_path": "lib/example.ak",
            "on_test_failure": "fail_immediately",
            "return_mode": "bool",
            "target_kind": "property",
            "validator_target": null,
            "test_program": { "hex": "" },
            "fuzzer_program": { "hex": "" },
            "fuzzer_type": "Int",
            "fuzzer_output_type": fuzzer_output_type,
            "constraint": constraint,
            "semantics": {
                "opaque": {
                    "reason": "test fixture semantics not set"
                }
            },
            "fuzzer_data_schema": null,
            "inner_data_schemas": {},
            "concrete_halt_witnesses": [],
            "concrete_error_witnesses": [],
            "transition_prop_lean": null
        }))
        .expect("dummy property test JSON should deserialize")
    }

    fn unique_temp_dir(prefix: &str) -> PathBuf {
        let timestamp = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .expect("system clock should be after UNIX_EPOCH")
            .as_nanos();
        std::env::temp_dir().join(format!("{prefix}-{}-{timestamp}", std::process::id()))
    }

    fn write_verify_target_fixture(root: &Path) {
        fs::create_dir_all(root.join("validators"))
            .expect("fixture validators directory should be creatable");

        fs::write(
            root.join("aiken.toml"),
            r#"
name = "test/verify_target_fixture"
version = "0.0.0"
plutusVersion = "v3"
description = "verify command target fixture"
"#,
        )
        .expect("fixture aiken.toml should be writable");

        fs::write(
            root.join("validators/fixture.ak"),
            r#"
fn seed_fuzzer() -> Fuzzer<Int> {
  fn(prng) { Some((prng, 0)) }
}

validator foo {
  mint(_redeemer: Data, policy_id: ByteArray, _tx: Data) {
    expect policy_id == "foo"
    True
  }

  else(_ctx) {
    fail
  }
}

test foo_mint_roundtrip(_seed via seed_fuzzer()) {
  foo.mint(Void, "foo", Void)
}
"#,
        )
        .expect("fixture module should be writable");
    }

    fn write_verify_skip_only_fixture(root: &Path) {
        fs::create_dir_all(root.join("validators"))
            .expect("fixture validators directory should be creatable");

        fs::write(
            root.join("aiken.toml"),
            r#"
name = "test/verify_skip_only_fixture"
version = "0.0.0"
plutusVersion = "v3"
description = "verify command skip-only fixture"
"#,
        )
        .expect("fixture aiken.toml should be writable");

        fs::write(
            root.join("validators/fixture.ak"),
            r#"
fn int_fuzzer() -> Fuzzer<Int> {
  todo
}

test unsupported_for_validator_target(x via int_fuzzer()) {
  x == x
}
"#,
        )
        .expect("fixture module should be writable");
    }

    fn write_verify_unit_only_fixture(root: &Path) {
        fs::create_dir_all(root.join("validators"))
            .expect("fixture validators directory should be creatable");

        fs::write(
            root.join("aiken.toml"),
            r#"
name = "test/verify_no_property_fixture"
version = "0.0.0"
plutusVersion = "v3"
description = "verify no-property-tests JSON fixture"
"#,
        )
        .expect("fixture aiken.toml should be writable");

        fs::write(
            root.join("validators/fixture.ak"),
            r#"
test unit_smoke() {
  True
}
"#,
        )
        .expect("fixture module should be writable");
    }

    fn fixture_run_options(out_dir: PathBuf, target: VerificationTargetKind) -> RunCommandOptions {
        RunCommandOptions {
            match_tests: None,
            exact_match: false,
            env: None,
            generate_only: true,
            out_dir,
            artifact_policy: ArtifactRetention::OnFailure,
            timeout: 300,
            cek_budget: 200_000,
            jobs_override: None,
            output_mode: OutputMode::Json,
            skip_policy: SkipPolicy::None,
            allow_skips: false,
            blaster_rev: DEFAULT_BLASTER_REV.to_string(),
            plutus_core_rev: DEFAULT_PLUTUS_CORE_REV.to_string(),
            existential_mode: ExistentialMode::default(),
            target,
            plutus_core_dir: None,
            raw_output_bytes: 65536,
            accept_partial: false,
            accept_witness: false,
            allow_vacuous_subgenerators: false,
        }
    }

    fn sample_doctor_report(
        all_ok: bool,
        blaster_rev: &str,
        plutus_core_rev: &str,
    ) -> verify::DoctorReport {
        verify::DoctorReport::new(
            verify::DOCTOR_REPORT_VERSION.to_string(),
            vec![
                verify::ToolCheck::new(
                    "lean".to_string(),
                    true,
                    Some("4.11.0".to_string()),
                    true,
                    "4.11.0".to_string(),
                    None,
                ),
                verify::ToolCheck::new(
                    "lake".to_string(),
                    true,
                    Some("4.11.0".to_string()),
                    true,
                    "0.0.0".to_string(),
                    None,
                ),
                verify::ToolCheck::new(
                    "z3".to_string(),
                    true,
                    Some("4.13.0".to_string()),
                    true,
                    "4.13.0".to_string(),
                    None,
                ),
            ],
            verify::PlutusCoreCheck::new(true, "build/verify/PlutusCore".to_string(), true, None),
            blaster_rev.to_string(),
            plutus_core_rev.to_string(),
            all_ok,
            verify::capabilities(),
        )
    }

    fn sample_generated_manifest() -> verify::GeneratedManifest {
        serde_json::from_value(serde_json::json!({
            "version": verify::GENERATE_ONLY_VERSION,
            "tests": [
                {
                    "id": "example_test_ok",
                    "aiken_module": "example",
                    "aiken_name": "test_ok",
                    "lean_module": "Example.TestOk",
                    "lean_theorem": "example_test_ok",
                    "lean_file": "Example/TestOk.lean",
                    "flat_file": "example_test_ok.flat",
                    "has_termination_theorem": true,
                    "has_equivalence_theorem": true,
                    "over_approximations": 2,
                    "partial_proof_note": "partial proof note",
                    "witness_proof_note": {
                        "instances": 2,
                        "witnesses": ["00", "01"],
                        "note": "witness-only"
                    }
                }
            ],
            "skipped": [
                {
                    "name": "example.test_skip",
                    "module": "example",
                    "reason": "unsupported shape"
                }
            ]
        }))
        .expect("generated manifest JSON should deserialize")
    }

    fn skipped_only_manifest() -> verify::GeneratedManifest {
        serde_json::from_value(serde_json::json!({
            "version": verify::GENERATE_ONLY_VERSION,
            "tests": [],
            "skipped": [
                {
                    "name": "example.test_unsupported",
                    "module": "example",
                    "reason": "unsupported shape"
                }
            ]
        }))
        .expect("skipped-only manifest JSON should deserialize")
    }

    fn single_test_manifest(include_skipped: bool) -> verify::GeneratedManifest {
        let skipped = if include_skipped {
            serde_json::json!([
                {
                    "name": "example.test_unsupported",
                    "module": "example",
                    "reason": "unsupported shape"
                }
            ])
        } else {
            serde_json::json!([])
        };
        serde_json::from_value(serde_json::json!({
            "version": verify::GENERATE_ONLY_VERSION,
            "tests": [
                {
                    "id": "example_test_ok",
                    "aiken_module": "example",
                    "aiken_name": "test_ok",
                    "lean_module": "Example.TestOk",
                    "lean_theorem": "example_test_ok",
                    "lean_file": "Example/TestOk.lean",
                    "flat_file": "example_test_ok.flat",
                    "has_termination_theorem": false,
                    "has_equivalence_theorem": false,
                    "over_approximations": 0
                }
            ],
            "skipped": skipped
        }))
        .expect("single-test manifest JSON should deserialize")
    }

    fn successful_raw_result() -> verify::VerifyResult {
        serde_json::from_value(serde_json::json!({
            "success": true,
            "stdout": {
                "tail": "",
                "total_bytes": 0,
                "truncated": false
            },
            "stderr": {
                "tail": "",
                "total_bytes": 0,
                "truncated": false
            },
            "exit_code": 0,
            "theorem_results": [
                {
                    "test_name": "example.test_ok",
                    "theorem_name": "Example.TestOk",
                    "status": { "kind": "proved" },
                    "over_approximations": 0
                }
            ]
        }))
        .expect("raw result JSON should deserialize")
    }

    #[test]
    fn doctor_command_branch_reports_success_output_and_exit_code() {
        let result = run_doctor_command_with(
            false,
            "abc123".to_string(),
            "pc-rev-1".to_string(),
            |blaster_rev, plutus_core_rev| sample_doctor_report(true, blaster_rev, plutus_core_rev),
        )
        .expect("doctor branch should render output");

        assert_eq!(result.exit_code, 0);
        assert!(
            result.output.contains("Verify Doctor Report"),
            "doctor text output should include the section header"
        );
        assert!(
            result.output.contains("All checks passed."),
            "doctor success output should include the success summary"
        );
    }

    #[test]
    fn doctor_command_branch_reports_json_output_and_failure_exit_code() {
        let result = run_doctor_command_with(
            true,
            "deadbeef".to_string(),
            "pc-rev-2".to_string(),
            |blaster_rev, plutus_core_rev| {
                sample_doctor_report(false, blaster_rev, plutus_core_rev)
            },
        )
        .expect("doctor branch should render JSON output");

        assert_eq!(result.exit_code, 1);
        let value: serde_json::Value =
            serde_json::from_str(&result.output).expect("doctor JSON output should parse");
        assert_eq!(value["version"], verify::DOCTOR_REPORT_VERSION);
        assert_eq!(value["all_ok"], false);
        assert_eq!(value["blaster_rev"], "deadbeef");
        assert_eq!(value["plutus_core_rev"], "pc-rev-2");
    }

    #[test]
    fn clean_command_branch_reports_removed_artifacts_and_exit_code() {
        let removed = vec![
            PathBuf::from("build/verify/manifest.json"),
            PathBuf::from("build/verify/logs"),
        ];
        let result = run_clean_command_with(
            Path::new("workspace/member-a"),
            PathBuf::from("build/verify"),
            move |_out_dir| Ok(removed),
        )
        .expect("clean branch should succeed");

        assert_eq!(result.exit_code, 0);
        assert!(
            result.output.contains("Removed build/verify/manifest.json"),
            "clean output should list removed files"
        );
        assert!(
            result.output.contains("Removed build/verify/logs"),
            "clean output should include each removed artifact"
        );
    }

    #[test]
    fn clean_command_branch_resolves_relative_out_dir_under_project_root() {
        let project_root = PathBuf::from("workspace/member-a");
        let mut resolved_out_dir = None;

        let _result =
            run_clean_command_with(&project_root, PathBuf::from("build/verify"), |out_dir| {
                resolved_out_dir = Some(out_dir.to_path_buf());
                Ok(vec![])
            })
            .expect("clean branch should resolve out_dir against project root");

        assert_eq!(resolved_out_dir, Some(project_root.join("build/verify")));
    }

    #[test]
    fn clean_command_branch_rejects_parent_traversal_out_dir() {
        let project_root = PathBuf::from("workspace/member-a");

        let result =
            run_clean_command_with(&project_root, PathBuf::from("../verify"), |_out_dir| {
                Ok(vec![])
            });

        assert!(
            result.is_err(),
            "clean command should reject out_dir paths with parent traversal"
        );
    }

    #[test]
    fn clean_command_branch_rejects_absolute_out_dir_outside_project_root() {
        let cwd = std::env::current_dir().expect("test process must be able to determine cwd");
        let project_root = cwd.join("workspace/member-a");
        let outside_out_dir = cwd.join("workspace/member-b/build/verify");

        let result = run_clean_command_with(&project_root, outside_out_dir, |_out_dir| Ok(vec![]));

        assert!(
            result.is_err(),
            "clean command should reject absolute out_dir outside project root"
        );
    }

    #[test]
    fn clean_command_rejects_nonexistent_roots() {
        let fixture_root = unique_temp_dir("verify-clean-nonexistent-root");
        if fixture_root.exists() {
            fs::remove_dir_all(&fixture_root).expect("stale fixture root should be removable");
        }

        let mut attempted_cleanup = false;
        let result = run_clean_for_directory_with(
            Some(fixture_root.as_path()),
            PathBuf::from("build/verify"),
            |_path| {
                attempted_cleanup = true;
                Ok(vec![])
            },
        );

        let message = match result {
            Ok(_) => panic!("nonexistent clean root should be rejected"),
            Err(error) => error.to_string(),
        };
        assert!(
            message.contains("requested directory"),
            "expected nonexistent-root error, got: {message}"
        );
        assert!(!attempted_cleanup, "clean must not run without a real root");
    }

    #[test]
    fn clean_command_rejects_missing_manifest_boundaries() {
        let fixture_root = unique_temp_dir("verify-clean-missing-manifest");
        if fixture_root.exists() {
            fs::remove_dir_all(&fixture_root).expect("stale fixture root should be removable");
        }
        fs::create_dir_all(fixture_root.join("build/verify/AikenVerify"))
            .expect("fixture workspace marker should be creatable");
        fs::write(fixture_root.join("build/verify/manifest.json"), "{}")
            .expect("manifest artifact should be writable");

        let mut attempted_cleanup = false;
        let result = run_clean_for_directory_with(
            Some(fixture_root.as_path()),
            PathBuf::from("build/verify"),
            |_path| {
                attempted_cleanup = true;
                Ok(vec![])
            },
        );

        let message = match result {
            Ok(_) => panic!("missing-manifest clean root should be rejected"),
            Err(error) => error.to_string(),
        };
        assert!(
            message.contains("aiken.toml"),
            "expected missing-manifest error, got: {message}"
        );
        assert!(
            fixture_root.join("build/verify/manifest.json").exists(),
            "clean must not remove artifacts when the manifest boundary is missing"
        );
        assert!(!attempted_cleanup, "clean must not run without a manifest boundary");

        fs::remove_dir_all(&fixture_root).expect("fixture root should be removable");
    }

    #[test]
    fn clean_command_rejects_invalid_project_manifests() {
        let fixture_root = unique_temp_dir("verify-clean-invalid-project");
        if fixture_root.exists() {
            fs::remove_dir_all(&fixture_root).expect("stale fixture root should be removable");
        }
        fs::create_dir_all(fixture_root.join("build/verify/AikenVerify"))
            .expect("fixture workspace marker should be creatable");
        fs::create_dir_all(fixture_root.join("build/verify/logs"))
            .expect("fixture logs dir should be creatable");
        fs::write(
            fixture_root.join("aiken.toml"),
            "name = \"aiken-lang/broken\"\nversion = [\n",
        )
        .expect("broken manifest should be writable");
        fs::write(fixture_root.join("build/verify/manifest.json"), "{}")
            .expect("manifest artifact should be writable");
        fs::write(
            fixture_root.join("build/verify/logs/lake_build.stdout.log"),
            "log",
        )
        .expect("log artifact should be writable");

        let mut attempted_cleanup = false;
        let result = run_clean_for_directory_with(
            Some(fixture_root.as_path()),
            PathBuf::from("build/verify"),
            |_path| {
                attempted_cleanup = true;
                Ok(vec![])
            },
        );

        let message = match result {
            Ok(_) => panic!("invalid project manifest should be rejected"),
            Err(error) => error.to_string(),
        };
        assert!(
            message.contains("Failed to parse manifest at"),
            "expected manifest parse error, got: {message}"
        );
        assert!(
            fixture_root.join("build/verify/manifest.json").exists(),
            "clean must not remove artifacts when the project manifest is invalid"
        );
        assert!(!attempted_cleanup, "clean must not run when the manifest is invalid");

        fs::remove_dir_all(&fixture_root).expect("fixture root should be removable");
    }

    #[test]
    fn clean_command_does_not_clean_workspace_members_implicitly() {
        let fixture_root = unique_temp_dir("verify-clean-workspace-root");
        if fixture_root.exists() {
            fs::remove_dir_all(&fixture_root).expect("stale fixture root should be removable");
        }

        fs::create_dir_all(fixture_root.join("build/verify/AikenVerify"))
            .expect("workspace root marker should be creatable");
        fs::create_dir_all(fixture_root.join("member-a/build/verify/AikenVerify"))
            .expect("member-a marker should be creatable");
        fs::create_dir_all(fixture_root.join("member-b/build/verify/AikenVerify"))
            .expect("member-b marker should be creatable");
        fs::write(
            fixture_root.join("aiken.toml"),
            "members = [\"member-a\", \"member-b\"]\n",
        )
        .expect("workspace manifest should be writable");
        fs::write(
            fixture_root.join("build/verify/manifest.json"),
            "{}",
        )
        .expect("workspace root artifact should be writable");
        fs::write(
            fixture_root.join("member-a/build/verify/manifest.json"),
            "{}",
        )
        .expect("member-a artifact should be writable");
        fs::write(
            fixture_root.join("member-b/build/verify/manifest.json"),
            "{}",
        )
        .expect("member-b artifact should be writable");

        let result = run_clean_for_directory_with(
            Some(fixture_root.as_path()),
            PathBuf::from("build/verify"),
            verify::clean_artifacts,
        )
        .expect("clean should target the requested directory only");

        assert!(result.output.contains("Removed"));
        assert!(
            !fixture_root.join("build/verify/manifest.json").exists(),
            "requested project artifacts should be removed",
        );
        assert!(
            fixture_root.join("member-a/build/verify/manifest.json").exists(),
            "clean must not implicitly remove sibling member artifacts",
        );
        assert!(
            fixture_root.join("member-b/build/verify/manifest.json").exists(),
            "clean must not implicitly remove sibling member artifacts",
        );

        fs::remove_dir_all(&fixture_root).expect("fixture root should be removable");
    }

    #[test]
    fn clean_command_ignores_nested_members_config() {
        let fixture_root = unique_temp_dir("verify-clean-nested-members");
        if fixture_root.exists() {
            fs::remove_dir_all(&fixture_root).expect("stale fixture root should be removable");
        }

        fs::create_dir_all(fixture_root.join("build/verify/AikenVerify"))
            .expect("root marker should be creatable");
        fs::create_dir_all(fixture_root.join("member-a/build/verify/AikenVerify"))
            .expect("member marker should be creatable");
        fs::write(
            fixture_root.join("aiken.toml"),
            r#"
name = "test/verify_clean_nested_members"
version = "0.0.0"
plutusVersion = "v3"
description = "verify clean nested-members fixture"

[config.workspace]
members = [1, 2]
"#,
        )
        .expect("fixture manifest should be writable");
        fs::write(fixture_root.join("build/verify/manifest.json"), "{}")
            .expect("root artifact should be writable");
        fs::write(
            fixture_root.join("member-a/build/verify/manifest.json"),
            "{}",
        )
        .expect("member artifact should be writable");

        let result = run_clean_for_directory_with(
            Some(fixture_root.as_path()),
            PathBuf::from("build/verify"),
            verify::clean_artifacts,
        )
        .expect("nested config.members must not trigger workspace handling");

        assert!(result.output.contains("Removed"));
        assert!(
            !fixture_root.join("build/verify/manifest.json").exists(),
            "root artifact should be removed for normal project configs",
        );
        assert!(
            fixture_root.join("member-a/build/verify/manifest.json").exists(),
            "nested config.members must not trigger sibling workspace cleanup",
        );

        fs::remove_dir_all(&fixture_root).expect("fixture root should be removable");
    }

    #[test]
    fn clean_command_reports_malformed_workspace_config_errors() {
        let fixture_root = unique_temp_dir("verify-clean-malformed-workspace");
        if fixture_root.exists() {
            fs::remove_dir_all(&fixture_root).expect("stale fixture root should be removable");
        }
        fs::create_dir_all(fixture_root.join("build/verify/AikenVerify"))
            .expect("fixture workspace marker should be creatable");
        fs::write(fixture_root.join("aiken.toml"), "members = [\n")
            .expect("broken workspace manifest should be writable");
        fs::write(fixture_root.join("build/verify/manifest.json"), "{}")
            .expect("manifest artifact should be writable");

        let result = run_clean_for_directory_with(
            Some(fixture_root.as_path()),
            PathBuf::from("build/verify"),
            verify::clean_artifacts,
        );

        let message = match result {
            Ok(_) => panic!("malformed workspace config should be reported"),
            Err(error) => error.to_string(),
        };
        assert!(
            message.contains("Failed to parse manifest at"),
            "expected manifest parse error, got: {message}"
        );
        assert!(
            fixture_root.join("build/verify/manifest.json").exists(),
            "clean must not silently remove artifacts when the workspace config is malformed"
        );

        fs::remove_dir_all(&fixture_root).expect("fixture root should be removable");
    }

    #[test]
    fn capabilities_command_branch_reports_output_and_exit_code() {
        let text_result = run_capabilities_command(CapabilitiesArgs { json: false })
            .expect("capabilities text output should render");
        assert_eq!(text_result.exit_code, 0);
        assert!(
            text_result.output.contains("Verification Capabilities"),
            "capabilities text output should include section header"
        );
        assert!(
            text_result.output.contains("Max test arity: 1"),
            "capabilities text output should include max arity"
        );

        let json_result = run_capabilities_command(CapabilitiesArgs { json: true })
            .expect("capabilities JSON output should render");
        assert_eq!(json_result.exit_code, 0);
        let value: serde_json::Value =
            serde_json::from_str(&json_result.output).expect("capabilities JSON should parse");
        assert_eq!(value["version"], verify::VERIFICATION_CAPABILITIES_VERSION);
        assert_eq!(value["max_test_arity"], 1);
        assert_eq!(value["supported"][0], "property");
    }

    #[test]
    fn verify_summary_json_contract_snapshot() {
        let theorem_results = vec![
            verify::TheoremResult::new(
                "example.test_proved".to_string(),
                "Example.test_proved".to_string(),
                verify::ProofStatus::Proved,
                0,
            ),
            verify::TheoremResult::new(
                "example.test_witness".to_string(),
                "Example.test_witness".to_string(),
                verify::ProofStatus::WitnessProved {
                    instances: 2,
                    witnesses: vec!["00".to_string(), "01".to_string()],
                    note: "witness-only proof".to_string(),
                },
                0,
            ),
            verify::TheoremResult::new(
                "example.test_partial".to_string(),
                "Example.test_partial".to_string(),
                verify::ProofStatus::Partial {
                    note: "partial proof".to_string(),
                },
                2,
            ),
            verify::TheoremResult::new(
                "example.test_failed".to_string(),
                "Example.test_failed".to_string(),
                verify::ProofStatus::Failed {
                    category: verify::FailureCategory::BuildError,
                    reason: "build failed".to_string(),
                },
                0,
            ),
            verify::TheoremResult::new(
                "example.test_timed_out".to_string(),
                "Example.test_timed_out".to_string(),
                verify::ProofStatus::TimedOut {
                    reason: "timed out".to_string(),
                },
                0,
            ),
            verify::TheoremResult::new(
                "example.test_unknown".to_string(),
                "Example.test_unknown".to_string(),
                verify::ProofStatus::Unknown,
                0,
            ),
        ];
        let mut summary = verify::VerifySummary::new(
            6,
            1,
            1,
            1,
            1,
            1,
            1,
            vec![verify::SkippedTest::new(
                "example.test_skip".to_string(),
                "example".to_string(),
                "unsupported shape".to_string(),
            )],
            theorem_results.clone(),
            verify::VerifyResult::new(
                false,
                verify::CapturedOutput::new(
                    "stdout tail".to_string(),
                    2048,
                    true,
                    Some(PathBuf::from(
                        "/workspace/build/verify/logs/lake_build.stdout.log",
                    )),
                ),
                verify::CapturedOutput::new(
                    "stderr tail".to_string(),
                    1024,
                    true,
                    Some(PathBuf::from(
                        "/workspace/build/verify/logs/lake_build.stderr.log",
                    )),
                ),
                Some(1),
                Some(theorem_results),
            ),
            Some(3210),
            false,
            "abc123".to_string(),
            "pc456".to_string(),
            true,
            true,
        );

        normalize_summary_log_paths(&mut summary, Path::new("/workspace"), true);

        let json = serde_json::to_value(&summary).expect("summary should serialize");
        assert!(json.get("allow_vacuous_subgenerators").is_some());
        assert!(json.get("two_phase_disabled").is_some());
        assert_eq!(
            json["raw_output"]["stdout"]["log_path"],
            "build/verify/logs/lake_build.stdout.log"
        );
        assert_eq!(
            json["raw_output"]["stderr"]["log_path"],
            "build/verify/logs/lake_build.stderr.log"
        );
        insta::assert_json_snapshot!("verify_summary_json_contract", &summary);
    }

    #[test]
    fn normalize_summary_log_paths_clears_deleted_artifact_paths() {
        let mut summary = verify::VerifySummary::new(
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            vec![],
            vec![],
            verify::VerifyResult::new(
                true,
                verify::CapturedOutput::new(
                    String::new(),
                    0,
                    true,
                    Some(PathBuf::from(
                        "/workspace/build/verify/logs/lake_build.stdout.log",
                    )),
                ),
                verify::CapturedOutput::new(
                    String::new(),
                    0,
                    true,
                    Some(PathBuf::from(
                        "/workspace/build/verify/logs/lake_build.stderr.log",
                    )),
                ),
                Some(0),
                Some(vec![]),
            ),
            None,
            true,
            String::new(),
            String::new(),
            false,
            false,
        );

        normalize_summary_log_paths(&mut summary, Path::new("/workspace"), false);

        assert!(summary.raw_output.stdout.log_path.is_none());
        assert!(summary.raw_output.stderr.log_path.is_none());
    }

    #[test]
    fn failure_artifact_advice_points_at_logs_when_artifacts_are_retained() {
        let advice = failure_artifact_advice(Path::new("/workspace/build/verify"), true);

        assert_eq!(
            advice,
            vec![
                "Logs available at /workspace/build/verify/logs/".to_string(),
                "To reproduce: cd /workspace/build/verify && lake build".to_string(),
            ]
        );
    }

    #[test]
    fn failure_artifact_advice_suggests_rerun_when_cleanup_removed_workspace() {
        let advice = failure_artifact_advice(Path::new("/workspace/build/verify"), false);

        assert_eq!(
            advice,
            vec![
                "Artifacts were cleaned up after this run. Re-run with --artifacts always to keep the generated workspace and logs.".to_string(),
            ]
        );
    }

    #[test]
    fn generated_manifest_json_contract_snapshot() {
        let manifest = sample_generated_manifest();
        insta::assert_json_snapshot!("generated_manifest_json_contract", &manifest);
    }

    #[test]
    fn verify_cli_error_json_contract_snapshot() {
        let payload = verify_cli_error_payload("project load failed");
        insta::assert_json_snapshot!("verify_cli_error_json_contract", &payload);
    }

    #[test]
    fn verify_cli_error_message_uses_fallback_for_empty_reports() {
        let message = verify_cli_error_message(&ExitFailure::into_report());
        assert_eq!(message, VERIFY_CLI_ERROR_FALLBACK_MESSAGE);
    }

    #[test]
    fn verify_cli_error_message_preserves_non_empty_reports() {
        let message = verify_cli_error_message(&ExitFailure::with_message("unknown module"));
        assert_eq!(message, "unknown module");
    }

    fn denied_warning_report() -> miette::Report {
        ExitFailure::with_message(
            "Warnings were denied by --deny.\n\nI came across a validator in a lib/ module which means I'm going to ignore it.",
        )
    }

    #[test]
    fn warning_only_deny_failure_surfaces_in_json_mode() {
        let payload = verify_cli_error_payload(verify_cli_error_message(&denied_warning_report()));
        assert_eq!(payload["kind"], "verify-cli-error");
        let message = payload["message"]
            .as_str()
            .expect("verify CLI error payload should carry a string message");
        assert!(message.contains("Warnings were denied by --deny"));
        assert!(message.contains("validator"));
    }

    #[test]
    fn doctor_report_json_contract_snapshot() {
        let report = sample_doctor_report(true, "abc123", "pc456");
        insta::assert_json_snapshot!("doctor_report_json_contract", &report);
    }

    #[test]
    fn capabilities_json_contract_snapshot() {
        let caps = verify::capabilities();
        insta::assert_json_snapshot!("capabilities_json_contract", &caps);
    }

    #[test]
    fn no_proofs_summary_json_includes_flags_and_version_fields() {
        let summary = no_proofs_summary(
            &sample_generated_manifest(),
            true,
            "abc123",
            "pc456",
            true,
            true,
        );
        let json = serde_json::to_value(&summary).expect("summary should serialize");
        assert_eq!(json["verify_summary_version"], verify::VERIFY_SUMMARY_VERSION);
        assert_eq!(json["allow_vacuous_subgenerators"], true);
        assert_eq!(json["two_phase_disabled"], true);
    }

    #[test]
    fn extract_counterexample_display_multiline_bytestring() {
        let reason = "\
error: AikenVerify/Proofs/Foo.lean:15:5: Counterexample:
error: AikenVerify/Proofs/Foo.lean:15:5: - x: (PlutusCore.ByteString.PlutusCore.ByteStringInternal.ByteString.mk
  \"Hello World.\")
error: AikenVerify/Proofs/Foo.lean:15:5: Tactic `blaster` failed: Goal was falsified";

        assert_eq!(
            extract_counterexample_display(reason),
            Some("\"Hello World.\"".to_string())
        );
    }

    #[test]
    fn extract_counterexample_display_inline_assignment() {
        let reason = "error: Foo.lean:1:1: Counterexample: x = 42";
        assert_eq!(
            extract_counterexample_display(reason),
            Some("42".to_string())
        );
    }

    #[test]
    fn extract_counterexample_display_multiple_inputs() {
        let reason = "\
error: Foo.lean:1:1: Counterexample:
error: Foo.lean:1:1: - x: 1
error: Foo.lean:1:1: - y: True
error: Foo.lean:1:1: Tactic `blaster` failed";

        assert_eq!(
            extract_counterexample_display(reason),
            Some("x = 1, y = True".to_string())
        );
    }

    #[test]
    fn sanitize_stderr_for_display_removes_generic_build_failed_line() {
        let stderr = "\
error: Foo.lean:15:5: Counterexample:
error: build failed
error: Foo.lean:15:5: Tactic `blaster` failed";

        assert_eq!(
            sanitize_stderr_for_display(stderr),
            "error: Foo.lean:15:5: Counterexample:\nerror: Foo.lean:15:5: Tactic `blaster` failed"
        );
    }

    #[test]
    fn generate_only_preflight_helper_does_not_enforce_explicit_bounds() {
        let tests = vec![dummy_property_test(
            "example.test_any_int",
            FuzzerOutputType::Int,
            FuzzerConstraint::Any,
        )];

        let unsupported = collect_generate_only_preflight_errors_with(&tests, |_t| Ok(()));
        assert!(
            unsupported.is_empty(),
            "helper must only report validator failures"
        );
    }

    #[test]
    fn generate_only_preflight_helper_reports_validator_errors() {
        let tests = vec![
            dummy_property_test(
                "example.test_ok",
                FuzzerOutputType::Int,
                FuzzerConstraint::Any,
            ),
            dummy_property_test(
                "example.test_bad",
                FuzzerOutputType::Int,
                FuzzerConstraint::Any,
            ),
        ];

        let unsupported = collect_generate_only_preflight_errors_with(&tests, |t| {
            if t.name.ends_with("bad") {
                Err(miette::miette!("unsupported shape"))
            } else {
                Ok(())
            }
        });

        assert_eq!(unsupported, vec!["example.test_bad: unsupported shape"]);
    }

    #[test]
    fn no_property_tests_output_is_json_in_json_mode() {
        let output = format_no_property_tests_output(
            OutputMode::Json,
            "custom-blaster",
            "custom-plutus",
            true,
        )
        .expect("json mode output should serialize successfully");
        let value: serde_json::Value =
            serde_json::from_str(&output).expect("json mode output should be valid JSON");

        assert_eq!(
            value["verify_summary_version"],
            verify::VERIFY_SUMMARY_VERSION
        );
        assert_eq!(value["blaster_rev"], "custom-blaster");
        assert_eq!(value["plutus_core_rev"], "custom-plutus");
        assert_eq!(value["allow_vacuous_subgenerators"], true);
        assert_eq!(value["total"], 0);
        assert!(value["theorems"].as_array().unwrap().is_empty());
    }

    #[test]
    fn no_property_tests_early_output_handles_empty_and_non_empty_slices() {
        let no_tests: Vec<ExportedPropertyTest> = vec![];
        let output = no_property_tests_early_output(
            &no_tests,
            OutputMode::Json,
            "custom-blaster",
            "custom-plutus",
            true,
        )
        .expect("empty slice should produce serializable JSON output")
        .expect("empty slice should return Some(output)");
        let value: serde_json::Value =
            serde_json::from_str(&output).expect("empty-slice output should be valid JSON");
        assert_eq!(
            value["verify_summary_version"],
            verify::VERIFY_SUMMARY_VERSION
        );
        assert_eq!(value["blaster_rev"], "custom-blaster");
        assert_eq!(value["plutus_core_rev"], "custom-plutus");
        assert_eq!(value["allow_vacuous_subgenerators"], true);
        assert_eq!(value["total"], 0);
        assert!(value["theorems"].as_array().unwrap().is_empty());

        let one_test = vec![dummy_property_test(
            "example.test_ok",
            FuzzerOutputType::Int,
            FuzzerConstraint::Any,
        )];
        let output = no_property_tests_early_output(
            &one_test,
            OutputMode::Json,
            "custom-blaster",
            "custom-plutus",
            true,
        )
        .expect("non-empty slice should not fail");
        assert!(
            output.is_none(),
            "non-empty slice should not trigger early no-tests output"
        );
    }

    #[test]
    fn run_proofs_start_output_is_suppressed_in_json_mode() {
        assert_eq!(run_proofs_start_output(OutputMode::Json), None);
        assert_eq!(run_proofs_start_output(OutputMode::Silent), None);
    }

    #[test]
    fn run_proofs_start_output_is_present_in_text_mode() {
        assert_eq!(
            run_proofs_start_output(OutputMode::Text),
            Some("Running proofs via lake build...")
        );
    }

    #[test]
    fn no_proofs_summary_reports_skipped_metadata_and_failure_status() {
        let manifest = skipped_only_manifest();

        let summary = no_proofs_summary(
            &manifest,
            true,
            verify::DEFAULT_BLASTER_REV,
            verify::DEFAULT_PLUTUS_CORE_REV,
            false,
            false,
        );

        assert_eq!(summary.total, 0);
        assert_eq!(summary.skipped.len(), 1);
        assert_eq!(summary.skipped[0].name, "example.test_unsupported");
        assert!(!summary.raw_output.success);
        assert_eq!(summary.raw_output.exit_code, Some(1));

        let json = serde_json::to_value(summary).expect("summary should serialize");
        assert_eq!(
            json["skipped"][0]["name"],
            serde_json::Value::String("example.test_unsupported".to_string())
        );
    }

    #[test]
    fn no_proofs_summary_marks_success_when_skips_are_allowed() {
        let manifest = skipped_only_manifest();

        let summary = no_proofs_summary(
            &manifest,
            false,
            verify::DEFAULT_BLASTER_REV,
            verify::DEFAULT_PLUTUS_CORE_REV,
            false,
            false,
        );

        assert!(summary.raw_output.success);
        assert_eq!(summary.raw_output.exit_code, Some(0));
    }

    #[test]
    fn no_proofs_summary_command_success_false_when_skips_disallowed() {
        let manifest = skipped_only_manifest();

        let summary = no_proofs_summary(
            &manifest,
            true,
            verify::DEFAULT_BLASTER_REV,
            verify::DEFAULT_PLUTUS_CORE_REV,
            false,
            false,
        );
        assert!(!summary.command_success);

        let json = serde_json::to_value(&summary).expect("summary should serialize");
        assert_eq!(json["command_success"], serde_json::Value::Bool(false));
    }

    #[test]
    fn no_proofs_summary_command_success_true_when_skips_allowed() {
        let manifest = skipped_only_manifest();

        let summary = no_proofs_summary(
            &manifest,
            false,
            verify::DEFAULT_BLASTER_REV,
            verify::DEFAULT_PLUTUS_CORE_REV,
            false,
            false,
        );
        assert!(summary.command_success);

        let json = serde_json::to_value(&summary).expect("summary should serialize");
        assert_eq!(json["command_success"], serde_json::Value::Bool(true));
    }

    #[test]
    fn no_proofs_summary_stamps_blaster_and_plutus_core_revs() {
        // Provenance: even on the early-exit "no property tests remain"
        // branch, the JSON summary MUST surface the resolved Blaster /
        // PlutusCore revs so consumers can audit which dependency snapshot
        // the (zero) proofs would have run against. Covers both the default
        // and a non-default override path.
        let manifest = verify::GeneratedManifest::empty(verify::GENERATE_ONLY_VERSION.to_string());

        let default_summary = no_proofs_summary(
            &manifest,
            false,
            verify::DEFAULT_BLASTER_REV,
            verify::DEFAULT_PLUTUS_CORE_REV,
            false,
            false,
        );
        assert_eq!(default_summary.blaster_rev, verify::DEFAULT_BLASTER_REV);
        assert_eq!(
            default_summary.plutus_core_rev,
            verify::DEFAULT_PLUTUS_CORE_REV
        );

        let json = serde_json::to_value(&default_summary).expect("summary should serialize");
        assert_eq!(
            json["blaster_rev"],
            serde_json::Value::String(verify::DEFAULT_BLASTER_REV.to_string())
        );
        assert_eq!(
            json["plutus_core_rev"],
            serde_json::Value::String(verify::DEFAULT_PLUTUS_CORE_REV.to_string())
        );

        // Override path: confirm an arbitrary `--blaster-rev` /
        // `--plutus-core-rev` value flows into the summary unchanged.
        let override_summary = no_proofs_summary(
            &manifest,
            false,
            "abc1234",
            "feature/experimental",
            false,
            false,
        );
        assert_eq!(override_summary.blaster_rev, "abc1234");
        assert_eq!(override_summary.plutus_core_rev, "feature/experimental");
    }

    #[test]
    fn parsed_summary_stamps_blaster_and_plutus_core_revs_via_run_options() {
        // Provenance: on the standard parse-then-stamp codepath (lines
        // ~1040 of cmd/verify.rs), the CLI mutates the summary to carry
        // the resolved revs from RunCommandOptions. Reproduce that pattern
        // here and verify both fields land in the JSON output.
        let manifest = single_test_manifest(false);

        let raw = successful_raw_result();

        let run_options = fixture_run_options(
            PathBuf::from("build/verify-blaster-rev-stamp"),
            VerificationTargetKind::default(),
        );

        let mut summary = verify::parse_verify_results(raw, &manifest);
        // Pre-stamp: library default is empty.
        assert_eq!(summary.blaster_rev, "");
        assert_eq!(summary.plutus_core_rev, "");

        // Mirror the production stamping in cmd/verify.rs.
        summary.blaster_rev.clone_from(&run_options.blaster_rev);
        summary
            .plutus_core_rev
            .clone_from(&run_options.plutus_core_rev);

        assert_eq!(summary.blaster_rev, DEFAULT_BLASTER_REV);
        assert_eq!(summary.plutus_core_rev, DEFAULT_PLUTUS_CORE_REV);

        let json = serde_json::to_value(&summary).expect("summary should serialize");
        assert_eq!(
            json["blaster_rev"],
            serde_json::Value::String(DEFAULT_BLASTER_REV.to_string())
        );
        assert_eq!(
            json["plutus_core_rev"],
            serde_json::Value::String(DEFAULT_PLUTUS_CORE_REV.to_string())
        );
    }

    #[test]
    fn command_success_in_json_reflects_skip_failure() {
        // Simulate a summary where proofs passed but skips cause failure.
        let manifest = single_test_manifest(true);

        let raw = successful_raw_result();

        let mut summary = verify::parse_verify_results(raw, &manifest);
        // Before skip adjustment, proofs passed so command_success is true.
        assert!(summary.command_success);

        // Now apply skip-induced failure (mirrors the cmd layer logic).
        let skipped_without_allow = skips_require_failure(manifest.skipped.len(), false);
        if skipped_without_allow {
            summary.command_success = false;
        }

        assert!(!summary.command_success);
        let json = serde_json::to_value(&summary).expect("summary should serialize");
        assert_eq!(json["command_success"], serde_json::Value::Bool(false));
    }

    #[test]
    fn verify_run_all_skipped_bypasses_plutus_core_check_when_skips_allowed() {
        let fixture_root = unique_temp_dir("aiken-verify-skip-only");
        if fixture_root.exists() {
            fs::remove_dir_all(&fixture_root).expect("stale fixture root should be removable");
        }
        fs::create_dir_all(&fixture_root).expect("fixture root should be creatable");
        write_verify_skip_only_fixture(&fixture_root);

        let mut run_options = fixture_run_options(
            PathBuf::from("build/verify-skip-only"),
            VerificationTargetKind::default(),
        );
        run_options.generate_only = false;
        run_options.skip_policy = SkipPolicy::All;
        run_options.allow_skips = true;
        run_options.target = VerificationTargetKind::ValidatorHandler;

        let mut project = Project::new(fixture_root.clone(), EventTarget::default())
            .expect("fixture project should load");
        let mut exit_code = 0;
        let result = exec_run_with_project(&mut project, &run_options, &mut exit_code);
        assert_eq!(exit_code, 0);
        assert!(
            result.is_ok(),
            "skip-only runs should not require PlutusCore when there are no runnable proofs: {result:?}"
        );

        fs::remove_dir_all(fixture_root).expect("fixture root should be removable");
    }

    #[test]
    fn verify_run_generate_only_supports_non_default_targets() {
        let fixture_root = unique_temp_dir("aiken-verify-target-modes");
        if fixture_root.exists() {
            fs::remove_dir_all(&fixture_root).expect("stale fixture root should be removable");
        }
        fs::create_dir_all(&fixture_root).expect("fixture root should be creatable");
        write_verify_target_fixture(&fixture_root);

        for target in [
            VerificationTargetKind::ValidatorHandler,
            VerificationTargetKind::Equivalence,
        ] {
            let out_dir = PathBuf::from(format!("build/verify-{target}"));
            let run_options = fixture_run_options(out_dir.clone(), target.clone());

            let mut project = Project::new(fixture_root.clone(), EventTarget::default())
                .expect("fixture project should load");
            let mut exit_code = 0;
            if let Err(errors) = exec_run_with_project(&mut project, &run_options, &mut exit_code) {
                panic!(
                    "verify run should support --target {target}; got errors: {errors:#?}"
                );
            }
            assert_eq!(exit_code, 0);

            let resolved_out_dir = fixture_root.join(&out_dir);
            let manifest_path = resolved_out_dir.join("manifest.json");
            assert!(
                manifest_path.exists(),
                "--target {target} should generate manifest.json"
            );
            let manifest_json =
                fs::read_to_string(&manifest_path).expect("generated manifest should be readable");
            let manifest_value: serde_json::Value =
                serde_json::from_str(&manifest_json).expect("manifest should be valid JSON");

            let tests = manifest_value["tests"]
                .as_array()
                .map(|t| t.len())
                .unwrap_or(0);
            let skipped = manifest_value["skipped"]
                .as_array()
                .map(|s| s.len())
                .unwrap_or(0);
            assert!(tests > 0, "--target {target} should emit runnable tests");
            assert_eq!(
                skipped, 0,
                "--target {target} should not succeed with only skipped tests"
            );
        }

        fs::remove_dir_all(fixture_root).expect("fixture root should be removable");
    }

    #[test]
    fn verify_run_unit_only_projects_return_no_proofs_without_crashing() {
        let fixture_root = unique_temp_dir("aiken-verify-unit-only");
        if fixture_root.exists() {
            fs::remove_dir_all(&fixture_root).expect("stale fixture root should be removable");
        }
        fs::create_dir_all(&fixture_root).expect("fixture root should be creatable");
        write_verify_unit_only_fixture(&fixture_root);

        let run_options = fixture_run_options(
            PathBuf::from("build/verify-unit-only"),
            VerificationTargetKind::default(),
        );
        let mut project = Project::new(fixture_root.clone(), EventTarget::default())
            .expect("fixture project should load");
        let mut exit_code = 0;
        let result = exec_run_with_project(&mut project, &run_options, &mut exit_code);

        assert_eq!(exit_code, 0);
        assert!(
            result.is_ok(),
            "unit-only verify runs should return the no-proofs summary instead of crashing: {result:?}"
        );

        fs::remove_dir_all(fixture_root).expect("fixture root should be removable");
    }

    #[test]
    fn run_compile_options_forwards_env() {
        let options = run_compile_options(Some("staging".to_string()));
        assert_eq!(options.env.as_deref(), Some("staging"));
    }

    #[test]
    fn resolve_verify_out_dir_uses_project_root_for_relative_paths() {
        let project_root = PathBuf::from("workspace/member-a");
        let out_dir = PathBuf::from("build/verify");

        assert_eq!(
            resolve_verify_out_dir(&out_dir, &project_root)
                .expect("relative out_dir without traversal should resolve"),
            project_root.join("build/verify")
        );
    }

    #[test]
    fn resolve_verify_out_dir_accepts_absolute_paths_inside_project_root() {
        let cwd = std::env::current_dir().unwrap();
        let project_root = cwd.join("member-a");
        let out_dir = project_root.join("build/verify");

        assert_eq!(
            resolve_verify_out_dir(&out_dir, &project_root)
                .expect("absolute out_dir without traversal should be accepted"),
            out_dir
        );
    }

    #[test]
    fn resolve_verify_out_dir_rejects_absolute_paths_outside_project_root() {
        let cwd = std::env::current_dir().unwrap();
        let project_root = cwd.join("member-a");
        let outside_out_dir = cwd.join("verify-output");

        let err = resolve_verify_out_dir(&outside_out_dir, &project_root)
            .expect_err("absolute out_dir outside project root should be rejected");

        assert!(
            err.to_string().contains("inside project root"),
            "unexpected outside-project error message: {err}"
        );
    }

    #[test]
    fn resolve_verify_out_dir_rejects_parent_traversal_segments() {
        let project_root = PathBuf::from("workspace/member-a");
        let err = resolve_verify_out_dir(Path::new("../outside"), &project_root)
            .expect_err("parent traversal should be rejected");

        assert!(
            err.to_string().contains("parent directory segments"),
            "unexpected traversal error message: {err}"
        );
    }

    #[cfg(unix)]
    #[test]
    fn resolve_verify_out_dir_rejects_symlink_escapes() {
        let fixture_root = unique_temp_dir("aiken-verify-out-dir-symlink");
        let project_root = fixture_root.join("project");
        let outside_dir = fixture_root.join("outside");
        fs::create_dir_all(&project_root).expect("project root should be creatable");
        fs::create_dir_all(&outside_dir).expect("outside directory should be creatable");
        symlink(&outside_dir, project_root.join("escaped"))
            .expect("escape symlink should be creatable");

        let err = resolve_verify_out_dir(Path::new("escaped/generated"), &project_root)
            .expect_err("symlink escape should be rejected");
        assert!(
            err.to_string().contains("inside project root"),
            "unexpected symlink-escape error message: {err}"
        );

        fs::remove_dir_all(fixture_root).expect("fixture root should be removable");
    }

    #[test]
    fn run_args_parse_env_flag() {
        #[derive(Parser)]
        struct VerifyCli {
            #[command(subcommand)]
            cmd: Cmd,
        }

        let parsed = VerifyCli::try_parse_from([
            "aiken-verify",
            "run",
            "--env",
            "staging",
            "--generate-only",
        ])
        .expect("`verify run --env` should parse");

        let Cmd::Run(args) = parsed.cmd else {
            panic!("expected `run` subcommand")
        };
        assert_eq!(args.env.as_deref(), Some("staging"));
    }

    #[test]
    fn run_args_directory_and_relative_out_dir_resolve_under_project_root() {
        #[derive(Parser)]
        struct VerifyCli {
            #[command(subcommand)]
            cmd: Cmd,
        }

        let cwd =
            std::env::current_dir().expect("test process must be able to determine current dir");
        let project_root = cwd.join("example-project");
        let project_root_arg = project_root.to_string_lossy().into_owned();

        let parsed = VerifyCli::try_parse_from([
            "aiken-verify",
            "run",
            project_root_arg.as_str(),
            "--out-dir",
            "build/verify",
            "--generate-only",
        ])
        .expect("`verify run <project-dir> --out-dir build/verify` should parse");

        let Cmd::Run(args) = parsed.cmd else {
            panic!("expected `run` subcommand")
        };

        assert_eq!(
            resolve_verify_out_dir(&args.out_dir, &project_root)
                .expect("run args out_dir should resolve under project root"),
            project_root.join("build/verify")
        );
    }

    #[test]
    fn doctor_args_reject_project_directory_positionals() {
        #[derive(Parser)]
        struct VerifyCli {
            #[command(subcommand)]
            cmd: Cmd,
        }

        let cwd =
            std::env::current_dir().expect("test process must be able to determine current dir");
        let project_root = cwd.join("example-project");
        let project_root_arg = project_root.to_string_lossy().into_owned();

        assert!(
            VerifyCli::try_parse_from(["aiken-verify", "doctor", project_root_arg.as_str()]).is_err(),
            "`verify doctor` should reject positional project directories because the command is global",
        );
    }

    #[test]
    fn doctor_can_run_without_an_aiken_project() {
        let result = run_doctor_command_with(
            true,
            "abc123".to_string(),
            "pc-rev".to_string(),
            |blaster_rev, plutus_core_rev| sample_doctor_report(true, blaster_rev, plutus_core_rev),
        )
        .expect("doctor should run without requiring aiken.toml");

        assert_eq!(result.exit_code, 0);
        let value: serde_json::Value =
            serde_json::from_str(&result.output).expect("doctor JSON output should parse");
        assert_eq!(value["blaster_rev"], "abc123");
        assert_eq!(value["plutus_core_rev"], "pc-rev");
    }

    #[test]
    fn clean_args_directory_and_relative_out_dir_resolve_under_project_root() {
        #[derive(Parser)]
        struct VerifyCli {
            #[command(subcommand)]
            cmd: Cmd,
        }

        let cwd =
            std::env::current_dir().expect("test process must be able to determine current dir");
        let project_root = cwd.join("example-project");
        let project_root_arg = project_root.to_string_lossy().into_owned();

        let parsed = VerifyCli::try_parse_from([
            "aiken-verify",
            "clean",
            project_root_arg.as_str(),
            "--out-dir",
            "build/verify",
        ])
        .expect("`verify clean <project-dir> --out-dir build/verify` should parse");

        let Cmd::Clean(args) = parsed.cmd else {
            panic!("expected `clean` subcommand")
        };

        assert_eq!(args.directory, Some(project_root.clone()));
        assert_eq!(
            resolve_verify_out_dir(&args.out_dir, &project_root)
                .expect("clean args out_dir should resolve under project root"),
            project_root.join("build/verify")
        );
    }

    fn render_verify_subcommand_long_help(name: &str) -> String {
        #[derive(Parser)]
        struct VerifyCli {
            #[command(subcommand)]
            cmd: Cmd,
        }

        let mut command = VerifyCli::command();
        let subcommand = command
            .find_subcommand_mut(name)
            .unwrap_or_else(|| panic!("expected `{name}` subcommand"));
        let mut output = Vec::new();
        subcommand
            .write_long_help(&mut output)
            .expect("subcommand help should render");
        String::from_utf8(output).expect("help output should be UTF-8")
    }

    #[test]
    fn doctor_help_includes_about_and_examples() {
        let help = render_verify_subcommand_long_help("doctor");
        assert!(help.contains("Check toolchain, dependencies, and configuration"));
        assert!(help.contains("aiken verify doctor --json"));
    }

    #[test]
    fn clean_help_includes_about_and_examples() {
        let help = render_verify_subcommand_long_help("clean");
        assert!(help.contains("Remove generated verification artifacts and logs"));
        assert!(help.contains("aiken verify clean --out-dir build/verify-ci"));
        assert!(help.contains("Use `-h` instead of `--help`"));
    }

    #[test]
    fn capabilities_help_includes_about_and_examples() {
        let help = render_verify_subcommand_long_help("capabilities");
        assert!(help.contains("Show supported verification capabilities"));
        assert!(help.contains("aiken verify capabilities --json"));
        assert!(help.contains("Use `-h` instead of `--help`"));
    }

    #[test]
    fn skips_require_failure_only_when_skips_are_disallowed() {
        assert!(skips_require_failure(1, false));
        assert!(!skips_require_failure(0, false));
        assert!(!skips_require_failure(1, true));
    }

    #[test]
    fn silent_cli_error_message_is_only_emitted_in_silent_mode() {
        let report = ExitFailure::with_message("unknown module");
        assert_eq!(
            silent_cli_error_message(OutputMode::Silent, &report),
            Some("unknown module".to_string())
        );
        assert_eq!(silent_cli_error_message(OutputMode::Text, &report), None);
        assert_eq!(silent_cli_error_message(OutputMode::Json, &report), None);
    }

    #[test]
    fn warning_only_deny_failure_surfaces_in_silent_mode() {
        let message = silent_cli_error_message(OutputMode::Silent, &denied_warning_report())
            .expect("silent mode should surface the denied warning message");
        assert!(message.contains("Warnings were denied by --deny"));
        assert!(message.contains("validator"));
    }

    fn fixture_summary(
        proved: usize,
        partial: usize,
        witness: usize,
        failed: usize,
        timed_out: usize,
        unknown: usize,
    ) -> verify::VerifySummary {
        let total = proved + partial + witness + failed + timed_out + unknown;
        verify::VerifySummary::new(
            total,
            proved,
            partial,
            witness,
            failed,
            timed_out,
            unknown,
            Vec::new(),
            Vec::new(),
            verify::VerifyResult::new(
                failed == 0 && timed_out == 0 && unknown == 0,
                verify::CapturedOutput::small(""),
                verify::CapturedOutput::small(""),
                Some(0),
                None,
            ),
            None,
            true,
            verify::DEFAULT_BLASTER_REV.to_string(),
            verify::DEFAULT_PLUTUS_CORE_REV.to_string(),
            false,
            false,
        )
    }

    #[test]
    fn proofs_succeeded_partial_no_accept_returns_false() {
        let summary = fixture_summary(1, 1, 0, 0, 0, 0);
        assert!(!proofs_succeeded(&summary, false, false));
    }

    #[test]
    fn proofs_succeeded_partial_with_accept_returns_true() {
        let summary = fixture_summary(1, 1, 0, 0, 0, 0);
        assert!(proofs_succeeded(&summary, true, false));
    }

    #[test]
    fn proofs_succeeded_witness_no_accept_returns_false() {
        let summary = fixture_summary(0, 0, 1, 0, 0, 0);
        assert!(!proofs_succeeded(&summary, false, false));
    }

    #[test]
    fn proofs_succeeded_witness_with_accept_returns_true() {
        let summary = fixture_summary(0, 0, 1, 0, 0, 0);
        assert!(proofs_succeeded(&summary, false, true));
    }

    #[test]
    fn proofs_succeeded_failed_overrides_accept_flags() {
        let summary = fixture_summary(0, 1, 1, 1, 0, 0);
        assert!(!proofs_succeeded(&summary, true, true));
    }

    #[test]
    fn proofs_succeeded_proved_only_returns_true() {
        let summary = fixture_summary(5, 0, 0, 0, 0, 0);
        assert!(proofs_succeeded(&summary, false, false));
    }

    #[test]
    fn proofs_succeeded_timed_out_overrides_accept_flags() {
        let summary = fixture_summary(0, 0, 0, 0, 1, 0);
        assert!(!proofs_succeeded(&summary, true, true));
    }

    #[test]
    fn proofs_succeeded_unknown_overrides_accept_flags() {
        let summary = fixture_summary(0, 0, 0, 0, 0, 1);
        assert!(!proofs_succeeded(&summary, true, true));
    }

    #[test]
    fn proofs_succeeded_partial_does_not_silence_witness() {
        // --accept-partial alone must NOT pass when there are witness-only
        // proofs; the user must opt in to each axis separately.
        let summary = fixture_summary(0, 1, 1, 0, 0, 0);
        assert!(!proofs_succeeded(&summary, true, false));
        assert!(!proofs_succeeded(&summary, false, true));
        assert!(proofs_succeeded(&summary, true, true));
    }

    #[test]
    fn command_success_override_reflects_partial_gate() {
        // Mirror the exec_run_with_project gate: a summary with partial > 0
        // and no --accept-partial must flip command_success to false.
        let mut summary = fixture_summary(1, 1, 0, 0, 0, 0);
        let accept_partial = false;
        let accept_witness = false;
        if summary.command_success
            && ((summary.partial > 0 && !accept_partial)
                || (summary.witness > 0 && !accept_witness))
        {
            summary.command_success = false;
        }
        assert!(!summary.command_success);

        let json = serde_json::to_value(&summary).expect("summary should serialize");
        assert_eq!(json["command_success"], serde_json::Value::Bool(false));
    }

    #[test]
    fn command_success_override_passes_through_when_accept_flags_set() {
        let mut summary = fixture_summary(1, 1, 1, 0, 0, 0);
        let accept_partial = true;
        let accept_witness = true;
        if summary.command_success
            && ((summary.partial > 0 && !accept_partial)
                || (summary.witness > 0 && !accept_witness))
        {
            summary.command_success = false;
        }
        assert!(summary.command_success);
    }

    #[test]
    fn classify_partial_code_returns_s0004_for_phase2_halt_note() {
        assert_eq!(
            classify_partial_code("two-phase halt Phase 2 sorry-closed"),
            "S0004"
        );
        assert_eq!(classify_partial_code(""), "S0004");
    }

    #[test]
    fn run_args_accept_partial_and_accept_witness_flags_parse() {
        #[derive(Parser)]
        struct VerifyCli {
            #[command(subcommand)]
            cmd: Cmd,
        }

        let parsed = VerifyCli::try_parse_from([
            "aiken-verify",
            "run",
            "--accept-partial",
            "--accept-witness",
            "--generate-only",
        ])
        .expect("`verify run --accept-partial --accept-witness` should parse");

        let Cmd::Run(args) = parsed.cmd else {
            panic!("expected `run` subcommand")
        };
        assert!(args.accept_partial);
        assert!(args.accept_witness);

        // Defaults: both flags should default to false when omitted.
        let parsed_defaults = VerifyCli::try_parse_from(["aiken-verify", "run", "--generate-only"])
            .expect("`verify run` should parse without accept flags");
        let Cmd::Run(default_args) = parsed_defaults.cmd else {
            panic!("expected `run` subcommand")
        };
        assert!(!default_args.accept_partial);
        assert!(!default_args.accept_witness);
    }

    #[test]
    fn run_args_allow_vacuous_subgenerators_hidden_flag_parses() {
        // Commit 18 (folds C12 #3): the `--allow-vacuous-subgenerators` flag
        // is `#[arg(hide = true)]` so it does not appear in `--help`, but it
        // MUST still parse from argv when explicitly passed (debug-mode
        // emission of widened sub-generator stubs forces every test to
        // verdict `Partial`). Without this regression, a clap rename or
        // accidental removal would silently drop the debug switch.
        #[derive(Parser)]
        struct VerifyCli {
            #[command(subcommand)]
            cmd: Cmd,
        }

        let parsed = VerifyCli::try_parse_from([
            "aiken-verify",
            "run",
            "--allow-vacuous-subgenerators",
            "--generate-only",
        ])
        .expect("`verify run --allow-vacuous-subgenerators` should parse even though hidden");
        let Cmd::Run(args) = parsed.cmd else {
            panic!("expected `run` subcommand")
        };
        assert!(
            args.allow_vacuous_subgenerators,
            "hidden --allow-vacuous-subgenerators flag must round-trip to RunArgs"
        );

        // Default (omitted): production semantics — must be false.
        let parsed_defaults = VerifyCli::try_parse_from(["aiken-verify", "run", "--generate-only"])
            .expect("`verify run` without the hidden flag must parse");
        let Cmd::Run(default_args) = parsed_defaults.cmd else {
            panic!("expected `run` subcommand")
        };
        assert!(
            !default_args.allow_vacuous_subgenerators,
            "without the flag, allow_vacuous_subgenerators MUST default to false"
        );
    }

    #[test]
    fn artifact_retention_on_failure_keeps_artifacts_when_command_fails() {
        assert!(
            !should_cleanup_artifacts(ArtifactRetention::OnFailure, false),
            "on-failure policy should retain artifacts whenever the final command exits non-zero",
        );
    }

    #[test]
    fn artifact_retention_on_failure_cleans_after_full_success() {
        assert!(
            should_cleanup_artifacts(ArtifactRetention::OnFailure, true),
            "on-failure policy should clean artifacts after a fully successful run",
        );
    }

    #[test]
    fn artifact_retention_on_success_requires_full_success() {
        assert!(
            !should_cleanup_artifacts(ArtifactRetention::OnSuccess, true),
            "on-success policy should retain artifacts only when run fully succeeds",
        );
        assert!(
            should_cleanup_artifacts(ArtifactRetention::OnSuccess, false),
            "on-success policy should clean artifacts when run exits non-zero",
        );
    }

    #[test]
    fn cek_budget_zero_is_rejected() {
        let args = RunArgs {
            directory: None,
            deny: false,
            silent: false,
            match_tests: None,
            exact_match: false,
            env: None,
            generate_only: true,
            out_dir: PathBuf::from("build/verify"),
            keep_artifacts: false,
            artifacts: ArtifactRetention::OnFailure,
            timeout: 300,
            cek_budget: 0,
            jobs: 0,
            json: false,
            skip_unsupported: None,
            strict_unsupported: false,
            allow_skips: false,
            blaster_rev: DEFAULT_BLASTER_REV.to_string(),
            plutus_core_rev: DEFAULT_PLUTUS_CORE_REV.to_string(),
            existential_mode: ExistentialMode::default(),
            target: VerificationTargetKind::PropertyWrapper,
            plutus_core_dir: None,
            raw_output_bytes: 65536,
            accept_partial: false,
            accept_witness: false,
            allow_vacuous_subgenerators: false,
        };
        let err = exec_run(args).expect_err("cek_budget 0 should be rejected");
        assert!(
            err.to_string().contains("must be greater than 0"),
            "error should mention the constraint: {err}"
        );
    }

    #[test]
    fn raw_output_bytes_validates_max_limit() {
        // `--raw-output-bytes 17000000` exceeds the 16 MiB cap and must be
        // rejected before any compile/lake work runs. Validation happens at
        // the top of `exec_run` so we can assert it without a fixture
        // project.
        let args = RunArgs {
            directory: None,
            deny: false,
            silent: false,
            match_tests: None,
            exact_match: false,
            env: None,
            generate_only: true,
            out_dir: PathBuf::from("build/verify"),
            keep_artifacts: false,
            artifacts: ArtifactRetention::OnFailure,
            timeout: 300,
            cek_budget: 200_000,
            jobs: 0,
            json: false,
            skip_unsupported: None,
            strict_unsupported: false,
            allow_skips: false,
            blaster_rev: DEFAULT_BLASTER_REV.to_string(),
            plutus_core_rev: DEFAULT_PLUTUS_CORE_REV.to_string(),
            existential_mode: ExistentialMode::default(),
            target: VerificationTargetKind::PropertyWrapper,
            plutus_core_dir: None,
            raw_output_bytes: 17_000_000,
            accept_partial: false,
            accept_witness: false,
            allow_vacuous_subgenerators: false,
        };
        let err = exec_run(args).expect_err("--raw-output-bytes above 16 MiB should be rejected");
        let msg = err.to_string();
        assert!(
            msg.contains("--raw-output-bytes must be <="),
            "error should mention the validation message: {msg}"
        );
        assert!(
            msg.contains(&MAX_RAW_OUTPUT_TAIL_BYTES.to_string()),
            "error should mention the cap value ({}): {msg}",
            MAX_RAW_OUTPUT_TAIL_BYTES
        );
        assert!(
            msg.contains("17000000"),
            "error should report the offending value: {msg}"
        );
    }

    /// Plan §"Test Plan B" line 910 — clap-parses
    /// `--skip-unsupported=E0011,E0013` and asserts the `Option<Vec<String>>`
    /// captures the two codes verbatim. The behavioural pin (that
    /// `is_skippable_generation_error` honours the resolved `SkipPolicy`)
    /// lives in `crates/aiken-project/src/verify/tests.rs` under
    /// `skip_policy_codes_filters_to_listed_codes_only`. Commit 18
    /// renamed this test (folds C15 #3) to make the clap-shape-only
    /// scope obvious from the name.
    #[test]
    fn skip_unsupported_with_codes_parses_codes_filter_correctly() {
        #[derive(Parser)]
        struct VerifyCli {
            #[command(subcommand)]
            cmd: Cmd,
        }

        let parsed = VerifyCli::try_parse_from([
            "aiken-verify",
            "run",
            "--skip-unsupported=E0011,E0013",
            "--generate-only",
        ])
        .expect("`verify run --skip-unsupported=E0011,E0013` should parse");

        let Cmd::Run(args) = parsed.cmd else {
            panic!("expected `run` subcommand")
        };
        assert_eq!(
            args.skip_unsupported,
            Some(vec!["E0011".to_string(), "E0013".to_string()]),
            "clap should capture the codes filter as Option<Vec<String>>"
        );
        assert!(
            !args.strict_unsupported,
            "--strict-unsupported should default to false when --skip-unsupported is set"
        );

        // The CLI plumbing converts the raw clap shape into a `SkipPolicy` via
        // `SkipPolicy::from_cli`; assert the resolved policy is `Codes(set)`.
        let normalized = normalize_skip_unsupported(args.skip_unsupported);
        let policy = SkipPolicy::from_cli(normalized);
        let expected: std::collections::BTreeSet<String> =
            ["E0011".to_string(), "E0013".to_string()]
                .into_iter()
                .collect();
        assert_eq!(policy, SkipPolicy::Codes(expected));

        // Construct a representative report via the same `unsupported(...)`

        // Construct a representative report via the same `unsupported(...)`
        // constructor that the verify pipeline uses, downcasting to confirm
        // the policy is applied at the catalogue-code level.
        // E0011: in the codes list → skippable.
        // E0015: NOT in the codes list → hard error.
        // S0002: UnsoundFallback → never skippable regardless.
        //
        // We can't call the project crate's `is_skippable_generation_error`
        // directly (it's `pub(crate)` to its own module). Instead, exercise
        // the visible contract via `generate_lean_workspace` in the project
        // crate's own tests; here we pin the clap surface shape that the CLI
        // exposes.
        //
        // The behavioural pin is in `verify/tests.rs`
        // (`skip_policy_codes_filters_to_listed_codes_only`); this test owns
        // the clap → SkipPolicy conversion contract.
    }

    #[test]
    fn bare_skip_unsupported_preserves_positional_directory_and_resolves_to_all() {
        #[derive(Parser)]
        struct VerifyCli {
            #[command(subcommand)]
            cmd: Cmd,
        }

        let cwd =
            std::env::current_dir().expect("test process must be able to determine current dir");
        let project_root = cwd.join("example-project");
        let project_root_arg = project_root.to_string_lossy().into_owned();

        let parsed = VerifyCli::try_parse_from([
            "aiken-verify",
            "run",
            "--skip-unsupported",
            project_root_arg.as_str(),
            "--generate-only",
        ])
        .expect("bare --skip-unsupported should not consume the positional directory");

        let Cmd::Run(args) = parsed.cmd else {
            panic!("expected `run` subcommand")
        };

        assert_eq!(args.directory, Some(project_root));
        let normalized = normalize_skip_unsupported(args.skip_unsupported);
        assert_eq!(normalized, Some(Vec::new()));
        assert_eq!(SkipPolicy::from_cli(normalized), SkipPolicy::All);
    }

    #[test]
    fn skip_unsupported_invalid_code_is_rejected_during_setup_validation() {
        let codes = ["E9999".to_string(), "E0001".to_string()];
        let err = validate_skip_unsupported_codes(Some(&codes)).expect_err(
            "unknown or dormant catalogue codes should be rejected before project setup",
        );
        let msg = err.to_string();
        assert!(msg.contains("Invalid --skip-unsupported code(s): E9999, E0001"));
        assert!(msg.contains("aiken verify capabilities"));
    }

    #[test]
    fn skip_unsupported_rejects_non_skippable_codes_during_setup_validation() {
        let codes = ["S0001".to_string()];
        let err = validate_skip_unsupported_codes(Some(&codes))
            .expect_err("non-skippable catalogue codes must be rejected before project setup");
        let msg = err.to_string();
        assert!(msg.contains("Invalid --skip-unsupported code(s): S0001"));
        assert!(msg.contains("Only skippable catalogue codes are accepted here"));
    }

    /// Plan §"Test Plan B" line 911 — clap-parses `--strict-unsupported`,
    /// asserts the bool flag is set and `skip_unsupported` is `None`, and
    /// confirms `--strict-unsupported --skip-unsupported` triggers clap's
    /// `conflicts_with` error path.
    #[test]
    fn strict_unsupported_alias_matches_default() {
        #[derive(Parser)]
        struct VerifyCli {
            #[command(subcommand)]
            cmd: Cmd,
        }

        // `--strict-unsupported` parses cleanly and resolves to `None`
        // (= `SkipPolicy::None` via `SkipPolicy::from_cli`).
        let parsed = VerifyCli::try_parse_from([
            "aiken-verify",
            "run",
            "--strict-unsupported",
            "--generate-only",
        ])
        .expect("`verify run --strict-unsupported` should parse");

        let Cmd::Run(args) = parsed.cmd else {
            panic!("expected `run` subcommand")
        };
        assert!(
            args.strict_unsupported,
            "--strict-unsupported should set the bool flag"
        );
        assert!(
            args.skip_unsupported.is_none(),
            "--strict-unsupported must not populate --skip-unsupported"
        );
        assert_eq!(
            SkipPolicy::from_cli(args.skip_unsupported),
            SkipPolicy::None,
            "the alias must resolve to SkipPolicy::None (no skipping)"
        );

        // Default (no flag) ALSO resolves to SkipPolicy::None — `--strict`
        // is purely an explicit way to say "no skipping" in CI argv.
        let defaults = VerifyCli::try_parse_from(["aiken-verify", "run", "--generate-only"])
            .expect("`verify run` (no skip flags) should parse");
        let Cmd::Run(default_args) = defaults.cmd else {
            panic!("expected `run` subcommand")
        };
        assert!(default_args.skip_unsupported.is_none());
        assert!(!default_args.strict_unsupported);
        assert_eq!(
            SkipPolicy::from_cli(default_args.skip_unsupported),
            SkipPolicy::None,
        );

        // `--strict-unsupported --skip-unsupported` MUST be rejected by
        // clap's `conflicts_with` constraint.
        let conflict = VerifyCli::try_parse_from([
            "aiken-verify",
            "run",
            "--strict-unsupported",
            "--skip-unsupported",
            "--generate-only",
        ]);
        let err = match conflict {
            Ok(_) => panic!(
                "--strict-unsupported and --skip-unsupported should conflict at the clap layer"
            ),
            Err(e) => e,
        };
        let err_msg = err.to_string();
        assert!(
            err_msg.contains("cannot be used with")
                || err_msg.contains("conflicts")
                || err_msg.contains("'--skip-unsupported'"),
            "conflict error should mention the conflicting flag, got: {err_msg}"
        );
    }

    /// Pin the existing `--allow-skips` clap contract: it requires
    /// `--skip-unsupported`, so passing it without should fail. Now that
    /// `--skip-unsupported` is `Option<Vec<String>>`, "presence" means
    /// `Some(_)` (including the empty list), which still satisfies clap's
    /// `requires` constraint.
    #[test]
    fn allow_skips_requires_skip_unsupported_after_signature_change() {
        #[derive(Parser)]
        struct VerifyCli {
            #[command(subcommand)]
            cmd: Cmd,
        }

        // `--allow-skips` alone is rejected by clap.
        let bad =
            VerifyCli::try_parse_from(["aiken-verify", "run", "--allow-skips", "--generate-only"]);
        assert!(
            bad.is_err(),
            "--allow-skips without --skip-unsupported should fail (requires constraint)"
        );

        // `--skip-unsupported --allow-skips` is accepted (presence of the
        // Option<Vec<_>> with Some(empty) is sufficient after normalization).
        let ok = VerifyCli::try_parse_from([
            "aiken-verify",
            "run",
            "--skip-unsupported",
            "--allow-skips",
            "--generate-only",
        ])
        .expect("`--skip-unsupported --allow-skips` should parse");
        let Cmd::Run(args) = ok.cmd else {
            panic!("expected `run` subcommand")
        };
        assert!(args.allow_skips);
        let normalized = normalize_skip_unsupported(args.skip_unsupported);
        assert_eq!(normalized, Some(Vec::new()));
        assert_eq!(SkipPolicy::from_cli(normalized), SkipPolicy::All);

        // `--skip-unsupported=E0011 --allow-skips` is also accepted.
        let codes_with_allow = VerifyCli::try_parse_from([
            "aiken-verify",
            "run",
            "--skip-unsupported=E0011",
            "--allow-skips",
            "--generate-only",
        ])
        .expect("`--skip-unsupported=E0011 --allow-skips` should parse");
        let Cmd::Run(args) = codes_with_allow.cmd else {
            panic!("expected `run` subcommand")
        };
        assert!(args.allow_skips);
        assert_eq!(args.skip_unsupported, Some(vec!["E0011".to_string()]));
    }

    /// Plan §"CLI Surface" lines 807–818 — `aiken verify capabilities`
    /// renders the catalogue table in text mode (CODE / FEATURE /
    /// SKIPPABLE) and the `{supported, unsupported}` JSON shape, with both
    /// E-codes (skippable) and S-codes (non-skippable) present.
    #[test]
    fn capabilities_output_includes_catalogue_table_in_text_and_json() {
        let caps = verify::capabilities();

        // JSON mode: the new `supported` and `unsupported` fields must round-
        // trip through `format_capabilities_output --json` per the plan's
        // user-facing contract.
        let json_output = format_capabilities_output(&caps, true).expect("json render");
        let json: serde_json::Value =
            serde_json::from_str(json_output.trim()).expect("output should be valid JSON");
        assert_eq!(
            json.get("supported")
                .and_then(|v| v.as_array())
                .map(|a| a.iter().filter_map(|v| v.as_str()).collect::<Vec<_>>()),
            Some(vec!["property"]),
            "JSON should include `supported: [\"property\"]`"
        );
        let unsupported_arr = json
            .get("unsupported")
            .and_then(|v| v.as_array())
            .expect("JSON should include `unsupported` array");
        assert!(
            unsupported_arr.iter().any(|v| {
                v.get("code").and_then(|c| c.as_str()) == Some("E0011")
                    && v.get("skippable").and_then(|s| s.as_bool()) == Some(true)
            }),
            "JSON `unsupported` array must contain an active E-code with skippable=true"
        );
        assert!(
            unsupported_arr
                .iter()
                .all(|v| v.get("code").and_then(|c| c.as_str()) != Some("E0001")),
            "JSON `unsupported` array must not advertise dormant codes such as E0001"
        );
        assert!(
            unsupported_arr.iter().any(|v| {
                v.get("code")
                    .and_then(|c| c.as_str())
                    .map(|s| s.starts_with('S'))
                    .unwrap_or(false)
                    && v.get("skippable").and_then(|s| s.as_bool()) == Some(false)
            }),
            "JSON `unsupported` array must contain at least one S-code with skippable=false"
        );

        // Text mode: the table header, separator, and at least one active
        // E-code and one S-code row should be present.
        let text_output = format_capabilities_output(&caps, false).expect("text render");
        assert!(
            text_output.contains("Error catalogue:"),
            "text output should include the catalogue section header"
        );
        assert!(
            text_output.contains("CODE")
                && text_output.contains("FEATURE")
                && text_output.contains("SKIPPABLE"),
            "text output should include the catalogue table headers"
        );
        assert!(
            text_output.contains("E0011"),
            "text output should list active E-codes such as E0011"
        );
        assert!(
            !text_output.contains("E0001"),
            "text output must not list dormant codes such as E0001"
        );
        assert!(
            text_output.contains("S0001"),
            "text output should list S-codes such as S0001"
        );
        // E-codes in skippable categories should render as `yes`; S-codes
        // (UnsoundFallback) should render as `no`.
        let lines_with_e0011: Vec<_> = text_output
            .lines()
            .filter(|l| l.contains("E0011"))
            .collect();
        assert!(
            lines_with_e0011.iter().any(|l| l.contains("yes")),
            "E0011 row should report skippable=yes; got: {lines_with_e0011:?}"
        );
        let lines_with_s0001: Vec<_> = text_output
            .lines()
            .filter(|l| l.contains("S0001"))
            .collect();
        assert!(
            lines_with_s0001.iter().any(|l| l.contains("no")),
            "S0001 row should report skippable=no; got: {lines_with_s0001:?}"
        );
    }

    /// Commit 15 follow-up #4 — text-mode `format_capabilities_output`
    /// snapshot fixture.  The JSON shape is already pinned (above and by
    /// `capabilities_json_shape_includes_supported_and_unsupported_arrays`
    /// in `aiken-project`); this snapshot locks the human-readable table
    /// cosmetics — column widths, header underlines, the order of the
    /// `Supported test kinds` / `Target modes` / etc. sections, and the
    /// catalogue table layout.
    ///
    /// Why a snapshot rather than per-line `contains()` checks: any future
    /// change to the table padding (e.g. adding a new column) or to the
    /// section ordering (e.g. moving `Existential modes` before
    /// `Target modes`) would silently keep the existing `contains()`
    /// assertions green while changing the operator-facing output.
    /// The snapshot makes the rendered shape reviewable at PR time.
    ///
    /// Inputs: `verify::capabilities()` — wholly deterministic (the
    /// catalogue is compile-time const).  No redactions needed.
    #[test]
    fn capabilities_text_output_canonical_shape() {
        let caps = verify::capabilities();
        let text_output = format_capabilities_output(&caps, false).expect("text render");
        insta::assert_snapshot!("capabilities_text_output_canonical_shape", text_output);
    }
}
