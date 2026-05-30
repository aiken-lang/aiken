use super::{
    output::{self, OutputMode},
    paths,
};
use aiken_lang::ast::Tracing;
use aiken_project::{
    Project,
    export::{ExportedPropertyTest, VerificationTargetKind},
    options::Options,
    telemetry::EventTarget,
    verify::{
        self, ArtifactRetention, DEFAULT_BLASTER_REV, DEFAULT_PLUTUS_CORE_REV, ExistentialMode,
        FailureCategory, MAX_RAW_OUTPUT_TAIL_BYTES, ProofStatus, SkipPolicy, VerifyConfig,
    },
    watch::with_project_event_target_results,
};
use owo_colors::{OwoColorize, Stream::Stderr};
use std::{
    path::{Path, PathBuf},
    process,
};

/// Run formal verification on property tests.
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
pub struct Args {
    /// Path to project
    pub(super) directory: Option<PathBuf>,

    /// Deny warnings; warnings will be treated as errors
    #[clap(short = 'D', long)]
    pub(super) deny: bool,

    /// Silence warnings; warnings will not be printed
    #[clap(short = 'S', long)]
    pub(super) silent: bool,

    /// Only run tests if they match any of these strings.
    /// You can match a module with `-m aiken/list` or `-m list`.
    /// You can match a test with `-m "aiken/list.{map}"` or `-m "aiken/option.{flatten_1}"`
    #[clap(short, long, verbatim_doc_comment)]
    pub(super) match_tests: Option<Vec<String>>,

    /// This is meant to be used with `--match-tests`.
    /// It forces test names to match exactly
    #[clap(short, long)]
    pub(super) exact_match: bool,

    /// Environment to build against.
    #[clap(long)]
    pub(super) env: Option<String>,

    /// Only generate Lean artifacts without running proofs
    #[clap(long)]
    pub(super) generate_only: bool,

    /// Output directory for Lean workspace
    #[clap(long, default_value = "build/verify")]
    pub(super) out_dir: PathBuf,

    /// [Deprecated: use --artifacts always] Keep generated Lean artifacts after verification
    #[clap(long, hide = true)]
    pub(super) keep_artifacts: bool,

    /// When to retain generated Lean artifacts.
    /// `on-failure` (default): keep only when proofs fail/timeout/unknown.
    /// `on-success`: keep only after a fully successful run.
    /// `always`: always keep artifacts.
    /// `never`: always remove artifacts after verification.
    #[clap(long, default_value = "on-failure", verbatim_doc_comment)]
    pub(super) artifacts: ArtifactRetention,

    /// Timeout in seconds per theorem build. Use 0 to disable timeout (wait indefinitely).
    #[clap(long, default_value = "300")]
    pub(super) timeout: u64,

    /// CEK machine step budget
    #[clap(long, default_value = "200000")]
    pub(super) cek_budget: u64,

    /// Optional Lake jobs override for `lake build`.
    /// When omitted (`0`), Lake's default scheduling is used.
    #[clap(short = 'j', long, default_value = "0")]
    pub(super) jobs: usize,

    /// Output results as JSON
    #[clap(long)]
    pub(super) json: bool,

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
    pub(super) skip_unsupported: Option<Vec<String>>,

    /// Treat all unsupported tests as hard errors (the default behaviour).
    /// Equivalent to omitting `--skip-unsupported`. Mutually exclusive with
    /// `--skip-unsupported`.
    #[clap(long, conflicts_with = "skip_unsupported")]
    pub(super) strict_unsupported: bool,

    /// When used with --skip-unsupported, exit 0 even if tests were skipped.
    /// Without this flag, skipped tests cause a non-zero exit.
    #[clap(long, requires = "skip_unsupported")]
    pub(super) allow_skips: bool,

    /// Git revision (commit, tag, or branch) for the Blaster dependency.
    /// Defaults to the version pinned in this release.
    #[clap(long, default_value = DEFAULT_BLASTER_REV)]
    pub(super) blaster_rev: String,

    /// Strategy for `fail once` (existential) tests.
    /// `proof`: attempt full existential theorem via Lean tactics (default).
    ///          Required for Int-domain existentials — `by blaster`
    ///          lets Z3 synthesize the witness.
    /// `witness`: deterministic witness search + concrete proof. Only
    ///            sound for domains where the trivial witness (0, False,
    ///            …) is always a valid falsifier.
    #[clap(long, default_value = "proof")]
    pub(super) existential_mode: ExistentialMode,

    /// Stable solver-operation profile.
    /// One of: finite, symbolic (default), symbolic-heavy, sampler, witness,
    /// scenario-bmc, scenario-kind, strict-cert.
    #[clap(long, default_value = "symbolic")]
    pub(super) solver_profile: verify::SolverProfile,

    /// Trust profile controlling which domain certificates count as accepted
    /// verification claims.
    /// One of: strict, production (default), experimental, unsafe-dev.
    #[clap(long, default_value = "production")]
    pub(super) trust_profile: verify::TrustProfile,

    /// HIDDEN DEBUG FLAG. Allow the legacy unsound emission of
    /// `opaque {pred} : Data → Data → Prop` predicates for sub-generators
    /// whose bodies cannot be statically inlined, replacing `opaque` with
    /// `def {pred} := fun _ _ => True`. Without this flag (the default),
    /// such tests hard-error with E0018 because Lean's `Inhabited Prop`
    /// instance silently fills `opaque` with `True`, making every
    /// constraint over the sub-generator domain trivially provable.
    /// Use only for debugging vacuous proof regressions.
    #[clap(long, hide = true)]
    pub(super) allow_vacuous_subgenerators: bool,

    /// Verification target mode.
    /// `property` (default): verify property tests directly.
    /// `validator`: verify validator handler programs for tests that export validator metadata.
    /// `equivalence`: prove wrapper/handler equivalence for tests that export validator metadata.
    #[clap(long, default_value = "property")]
    pub(super) target: VerificationTargetKind,

    /// Git revision (commit, tag, or branch) for the PlutusCore dependency.
    /// Defaults to the version pinned in this release. Use --plutus-core-dir for a local checkout instead.
    #[clap(long, default_value = DEFAULT_PLUTUS_CORE_REV)]
    pub(super) plutus_core_rev: String,

    /// Path to the PlutusCore Lean library checkout. Overrides the PLUTUS_CORE_DIR environment variable.
    #[clap(long)]
    pub(super) plutus_core_dir: Option<PathBuf>,

    /// Maximum bytes of raw stdout/stderr to retain in the verify summary.
    /// Excess output is truncated; the full streams are persisted under
    /// `<out_dir>/logs/lake_build.{stdout,stderr}.log` when truncated.
    /// `0` disables truncation (full streams in JSON).
    /// Maximum: 16777216 (16 MiB).
    #[clap(long, default_value = "65536")]
    pub(super) raw_output_bytes: usize,

    /// Allow CI to pass when some proofs are sorry-closed (Partial).
    /// Without this flag, any partial proof causes a non-zero exit.
    /// Full proofs of these obligations will be added in a future Aiken release.
    #[clap(long)]
    pub(super) accept_partial: bool,

    /// Allow CI to pass when some proofs are existential/witness-only.
    /// Without this flag, witness-only proofs cause a non-zero exit.
    /// Universal proof support for state-machine tests will be added in a
    /// future Aiken release.
    #[clap(long)]
    pub(super) accept_witness: bool,
}

pub fn exec(
    Args {
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
        strict_unsupported: _,
        allow_skips,
        blaster_rev,
        existential_mode,
        solver_profile,
        trust_profile,
        target,
        plutus_core_rev,
        plutus_core_dir,
        raw_output_bytes,
        accept_partial,
        accept_witness,
        allow_vacuous_subgenerators,
    }: Args,
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
        solver_profile,
        trust_profile,
        target,
        plutus_core_dir,
        raw_output_bytes,
        accept_partial,
        accept_witness,
        allow_vacuous_subgenerators,
    };

    let result = with_project_event_target_results(
        directory.as_deref(),
        deny,
        !output_mode.shows_advisories(),
        output_mode.shows_advisories(),
        output_mode.project_event_target(),
        output_mode.reports_project_diagnostics(),
        |p| exec_run_with_project(p, &run_options),
    );

    let project_results = match result {
        Ok(project_results) => project_results,
        Err(error) => {
            if output_mode.emits_json() {
                exit_with_cli_json_report_error(&error);
            }
            if let Some(message) = silent_cli_error_message(output_mode, &error) {
                eprintln!("{message}");
            }
            process::exit(1);
        }
    };

    if output_mode.emits_json() && project_results.len() > 1 {
        let aggregate = aggregate_workspace_json(&project_results)?;
        println!(
            "{}",
            serde_json::to_string_pretty(&aggregate).map_err(|error| miette::miette!(
                "Failed to serialize verification workspace JSON: {error}"
            ))?
        );
    } else {
        for project_result in &project_results {
            print!("{}", project_result.output);
        }
    }

    let exit_code = project_results
        .iter()
        .map(|project_result| project_result.exit_code)
        .max()
        .unwrap_or(0);

    if exit_code != 0 {
        process::exit(exit_code);
    }

    Ok(())
}

#[derive(Clone)]
pub(super) struct RunCommandOptions {
    pub(super) match_tests: Option<Vec<String>>,
    pub(super) exact_match: bool,
    pub(super) env: Option<String>,
    pub(super) generate_only: bool,
    pub(super) out_dir: PathBuf,
    pub(super) artifact_policy: ArtifactRetention,
    pub(super) timeout: u64,
    pub(super) cek_budget: u64,
    pub(super) jobs_override: Option<usize>,
    pub(super) output_mode: OutputMode,
    /// Resolved policy for `--skip-unsupported` / `--strict-unsupported`.
    /// `SkipPolicy::None` is the default (= `--strict-unsupported`); the
    /// CLI parser maps the raw `Option<Vec<String>>` shape via
    /// `SkipPolicy::from_cli`.
    pub(super) skip_policy: SkipPolicy,
    pub(super) allow_skips: bool,
    pub(super) blaster_rev: String,
    pub(super) plutus_core_rev: String,
    pub(super) existential_mode: ExistentialMode,
    pub(super) target: VerificationTargetKind,
    pub(super) plutus_core_dir: Option<PathBuf>,
    pub(super) raw_output_bytes: usize,
    pub(super) accept_partial: bool,
    pub(super) accept_witness: bool,
    pub(super) solver_profile: verify::SolverProfile,
    pub(super) trust_profile: verify::TrustProfile,

    pub(super) allow_vacuous_subgenerators: bool,
}

#[derive(Debug)]
pub(super) struct RunProjectResult {
    pub(super) root: String,
    pub(super) output: String,
    pub(super) exit_code: i32,
}

pub(super) fn aggregate_workspace_json(
    project_results: &[RunProjectResult],
) -> miette::Result<serde_json::Value> {
    let packages = project_results
        .iter()
        .map(|project_result| {
            serde_json::from_str::<serde_json::Value>(&project_result.output)
                .map(|summary| {
                    serde_json::json!({
                        "root": project_result.root,
                        "summary": summary,
                    })
                })
                .map_err(|error| {
                    miette::miette!("Failed to aggregate verification JSON output: {error}")
                })
        })
        .collect::<miette::Result<Vec<_>>>()?;

    Ok(serde_json::json!({
        "verify_summary_version": verify::VERIFY_SUMMARY_VERSION,
        "packages": packages,
    }))
}

pub(super) fn format_no_property_tests_output(
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

pub(super) fn no_property_tests_early_output(
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

pub(super) fn run_proofs_start_output(output_mode: OutputMode) -> Option<&'static str> {
    output_mode
        .shows_advisories()
        .then_some("Running proofs via lake build...")
}

pub(super) fn two_phase_disabled_from_env() -> bool {
    std::env::var("AIKEN_EMIT_TWO_PHASE")
        .map(|value| value == "0")
        .unwrap_or(false)
}

pub(super) fn no_proofs_summary(
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

pub(super) fn normalize_report_path(path: &Path, project_root: &Path) -> PathBuf {
    path.strip_prefix(project_root)
        .map(Path::to_path_buf)
        .unwrap_or_else(|_| path.to_path_buf())
}

pub(super) fn normalize_summary_artifact_paths(
    summary: &mut verify::VerifySummary,
    project_root: &Path,
    artifacts_retained: bool,
) {
    if !artifacts_retained {
        summary.raw_output.stdout.log_path = None;
        summary.raw_output.stderr.log_path = None;
        summary.artifacts = verify::VerificationArtifacts::default();
        return;
    }

    for output in [
        &mut summary.raw_output.stdout,
        &mut summary.raw_output.stderr,
    ] {
        let Some(path) = output.log_path.clone() else {
            continue;
        };
        output.log_path = Some(normalize_report_path(&path, project_root));
    }

    if let Some(path) = summary.artifacts.manifest.clone() {
        summary.artifacts.manifest = Some(normalize_report_path(&path, project_root));
    }
    if let Some(path) = summary.artifacts.lean_root.clone() {
        summary.artifacts.lean_root = Some(normalize_report_path(&path, project_root));
    }
    if let Some(path) = summary.artifacts.logs.clone() {
        summary.artifacts.logs = Some(normalize_report_path(&path, project_root));
    }
    summary.artifacts.smt2 = summary
        .artifacts
        .smt2
        .iter()
        .map(|path| normalize_report_path(path, project_root))
        .collect();
}

pub(super) fn extract_first_double_quoted(input: &str) -> Option<String> {
    let start = input.find('"')?;
    let rest = &input[start + 1..];
    let end = rest.find('"')?;
    Some(rest[..end].to_string())
}

pub(super) fn is_simple_identifier(input: &str) -> bool {
    !input.is_empty()
        && input
            .chars()
            .all(|ch| ch.is_ascii_alphanumeric() || ch == '_')
}

pub(super) fn simplify_counterexample_expr(expr: &str) -> String {
    if let Some(text) = extract_first_double_quoted(expr) {
        return format!("\"{text}\"");
    }

    expr.split_whitespace().collect::<Vec<_>>().join(" ")
}

pub(super) fn format_counterexample_item(item: &str, include_name: bool) -> String {
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

pub(super) fn extract_counterexample_display(reason: &str) -> Option<String> {
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

pub(super) fn solver_counterexample_label(theorem: &verify::TheoremResult, reason: &str) -> String {
    if let Some(counterexample) = theorem.counterexample.as_ref() {
        return match counterexample.classification {
            verify::CounterexampleClassification::ConfirmedByReplay => counterexample
                .input_source_value
                .as_deref()
                .map(|value| {
                    format!(
                        "SOLVER FALSIFIED [{}]: {value}",
                        certification_label(theorem.certification)
                    )
                })
                .unwrap_or_else(|| {
                    format!(
                        "SOLVER FALSIFIED [{}]",
                        certification_label(theorem.certification)
                    )
                }),
            verify::CounterexampleClassification::Potential => format!(
                "SOLVER FALSIFIED [{}]: potential counterexample",
                certification_label(theorem.certification)
            ),
            verify::CounterexampleClassification::SmtModelOnly | _ => format!(
                "SOLVER FALSIFIED [{}]: SMT model only",
                certification_label(theorem.certification)
            ),
        };
    }

    extract_counterexample_display(reason)
        .map(|value| {
            format!(
                "SOLVER FALSIFIED [{}]: {value}",
                certification_label(theorem.certification)
            )
        })
        .unwrap_or_else(|| {
            format!(
                "SOLVER FALSIFIED [{}]",
                certification_label(theorem.certification)
            )
        })
}

pub(super) fn sanitize_stderr_for_display(stderr: &str) -> String {
    stderr
        .lines()
        .filter(|line| line.trim() != "error: build failed")
        .collect::<Vec<_>>()
        .join("\n")
}

pub(super) fn normalize_skip_unsupported(
    skip_unsupported: Option<Vec<String>>,
) -> Option<Vec<String>> {
    match skip_unsupported {
        Some(codes) if codes.len() == 1 && codes[0].is_empty() => Some(Vec::new()),
        other => other,
    }
}

pub(super) fn validate_skip_unsupported_codes(
    skip_unsupported: Option<&[String]>,
) -> miette::Result<()> {
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

pub(super) const VERIFY_CLI_ERROR_FALLBACK_MESSAGE: &str =
    "Project loading or compilation failed. Re-run without --json for full diagnostics.";

pub(super) fn verify_cli_error_message(error: &miette::Report) -> String {
    let message = error.to_string();
    if message.trim().is_empty() {
        VERIFY_CLI_ERROR_FALLBACK_MESSAGE.to_string()
    } else {
        message
    }
}

pub(super) fn silent_cli_error_message(
    output_mode: OutputMode,
    error: &miette::Report,
) -> Option<String> {
    matches!(output_mode, OutputMode::Silent).then(|| verify_cli_error_message(error))
}

pub(super) fn verify_cli_error_payload(message: impl Into<String>) -> serde_json::Value {
    serde_json::json!({
        "version": VERIFY_CLI_ERROR_VERSION,
        "kind": "verify-cli-error",
        "message": message.into(),
    })
}

pub(super) fn exit_with_cli_json_report_error(error: &miette::Report) -> ! {
    exit_with_cli_json_error(verify_cli_error_message(error));
}

pub(super) fn exit_with_cli_json_error(message: impl Into<String>) -> ! {
    let payload = verify_cli_error_payload(message);

    println!(
        "{}",
        serde_json::to_string_pretty(&payload)
            .expect("verify CLI JSON error payload should serialize"),
    );
    process::exit(1);
}

pub(super) fn exec_run_with_project(
    p: &mut Project<EventTarget>,
    run_options: &RunCommandOptions,
) -> Result<RunProjectResult, Vec<aiken_project::error::Error>> {
    let mut output = String::new();
    macro_rules! println {
        () => {{
            output.push('\n');
        }};
        ($($arg:tt)*) => {{
            use std::fmt::Write as _;
            writeln!(&mut output, $($arg)*).expect("writing verify output to a string should not fail");
        }};
    }
    let project_root = p.root().display().to_string();
    let resolved_out_dir =
        paths::resolve_verify_out_dir(&run_options.out_dir, p.root()).map_err(|e| {
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

    if let Some(early_output) = no_property_tests_early_output(
        property_tests,
        run_options.output_mode,
        &run_options.blaster_rev,
        &run_options.plutus_core_rev,
        run_options.allow_vacuous_subgenerators,
    )
    .map_err(|e| vec![aiken_project::error::Error::Json(e)])?
    {
        println!("{early_output}");
        return Ok(RunProjectResult {
            root: project_root.clone(),
            output,
            exit_code: 0,
        });
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
        exported.plutus_version,
        run_options.trust_profile,
        Some(p.root().to_path_buf()),
        run_options.plutus_core_dir.clone(),
        run_options.raw_output_bytes,
        run_options.allow_vacuous_subgenerators,
    );

    let (manifest, cache_report) = verify::generate_lean_workspace_with_cache(
        property_tests,
        &config,
        &run_options.skip_policy,
    )
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
            println!("  {}", output::render_manifest_debug_header(&manifest));
            for entry in &manifest.tests {
                println!("  - {}", output::render_manifest_entry_debug(entry));
                for limitation in &entry.compatibility_limitations {
                    println!("      limitation: {limitation}");
                }
            }
            if !manifest.compatibility_limitations.is_empty() {
                println!();
                println!("Manifest limitations:");
                for limitation in &manifest.compatibility_limitations {
                    println!("  - {limitation}");
                }
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
        if should_cleanup_artifacts(run_options.artifact_policy, !skipped_without_allow)
            && let Err(e) = verify::clear_generated_workspace(&resolved_out_dir)
            && run_options.output_mode.shows_advisories()
        {
            eprintln!(
                "Warning: failed to clean up {}: {}",
                resolved_out_dir.display(),
                e
            );
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

            if should_cleanup_artifacts(run_options.artifact_policy, !skipped_without_allow)
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
                    return Ok(RunProjectResult {
                        root: project_root.clone(),
                        output,
                        exit_code: 1,
                    });
                }
                return skipped_tests_failure(manifest.skipped.len()).map(|_| RunProjectResult {
                    root: project_root.clone(),
                    output,
                    exit_code: 1,
                });
            }

            return Ok(RunProjectResult {
                root: project_root.clone(),
                output,
                exit_code: 0,
            });
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
        // `solver_profile` affects result classification through VerifyParseContext.
        // `run_proofs` currently executes the generated Lake workspace selected by generation.
        let result = verify::run_proofs(
            &resolved_out_dir,
            run_options.timeout,
            run_options.jobs_override,
            &manifest,
            run_options.raw_output_bytes,
            run_options.trust_profile,
        )
        .map_err(|e| {
            vec![aiken_project::error::Error::StandardIo(
                std::io::Error::other(e.to_string()),
            )]
        })?;
        let elapsed = start.elapsed();

        let mut parse_context = verify::VerifyParseContext::new(
            run_options.trust_profile,
            Some(verify::VerificationRunSettings::new(
                run_options.solver_profile,
                run_options.trust_profile,
                run_options.timeout,
                run_options.cek_budget,
            )),
            run_options.blaster_rev.clone(),
            run_options.plutus_core_rev.clone(),
        );
        parse_context.allow_vacuous_subgenerators = run_options.allow_vacuous_subgenerators;
        parse_context.two_phase_disabled = two_phase_disabled_from_env();
        parse_context.skipped_without_allow = skipped_without_allow;
        parse_context.accept_partial = run_options.accept_partial;
        parse_context.accept_witness = run_options.accept_witness;
        parse_context.require_lean_certified = matches!(
            run_options.solver_profile,
            verify::SolverProfile::StrictCert
        );

        let mut summary = verify::parse_verify_results(result, &manifest, parse_context);
        summary.elapsed_ms = Some(elapsed.as_millis() as u64);
        summary.cache = Some(cache_report.clone());
        if matches!(
            run_options.solver_profile,
            verify::SolverProfile::StrictCert
        ) {
            ensure_strict_cert_failure_reasons(&mut summary);
        }

        let skipped_causes_failure = skipped_without_allow;
        let proofs_succeeded = proofs_succeeded(
            &summary,
            run_options.accept_partial,
            run_options.accept_witness,
        );
        let cleanup_needed =
            should_cleanup_artifacts(run_options.artifact_policy, summary.command_success);
        let cleanup_error = if cleanup_needed {
            verify::clear_generated_workspace(&resolved_out_dir).err()
        } else {
            None
        };
        let artifacts_retained = !cleanup_needed || cleanup_error.is_some();
        summary.artifacts = verify::collect_verification_artifacts(&resolved_out_dir);
        normalize_summary_artifact_paths(&mut summary, p.root(), artifacts_retained);

        if run_options.output_mode.emits_json() {
            let output = serde_json::to_string_pretty(&summary)
                .map_err(|e| vec![aiken_project::error::Error::Json(e)])?;
            println!("{output}");
        } else {
            println!();
            if summary.run_settings.as_ref().is_some_and(|settings| {
                matches!(settings.solver_profile, verify::SolverProfile::StrictCert)
            }) {
                println!(
                    "Selected solver profile strict-cert requires LeanCertified results; any SolverValidated output below is informational and still causes a non-zero result."
                );
                println!();
            }
            for t in &summary.theorems {
                let (icon, label, trailing_block): (String, String, Option<String>) = match &t
                    .proof_status
                {
                    ProofStatus::Proved => {
                        let mut label = format!(
                            "SOLVER VALIDATED [{}]{}",
                            certification_label(t.certification),
                            trust_profile_suffix(t.trust_profile)
                        );
                        if t.over_approximations > 0 {
                            label.push_str(&format!(
                                " ({} over-approximation{})",
                                t.over_approximations,
                                if t.over_approximations == 1 { "" } else { "s" }
                            ));
                        }
                        (
                            "PASS".if_supports_color(Stderr, |s| s.green()).to_string(),
                            label,
                            t.explanation
                                .as_deref()
                                .and_then(render_status_explanation_block),
                        )
                    }
                    ProofStatus::Partial { note } => {
                        // Build succeeded but the proof leaves a sub-obligation
                        // open. Surface as PARTIAL — the user must see this
                        // rather than a misleading PROVED verdict. Carry the
                        // over-approximation suffix when present (the audit
                        // count is preserved on partial proofs).
                        let code = classify_partial_code(note);
                        let mut headline = format!(
                            "PARTIAL [{code}] [{}]",
                            certification_label(t.certification)
                        );
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
                            "WITNESS VALIDATED [{}]{} ({} instance{})",
                            certification_label(t.certification),
                            trust_profile_suffix(t.trust_profile),
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
                        let mut block = format!(
                            "       This is an existential proof, not a universal one.\n\
                                 \x20      Lean verified that the property holds on {instances} concrete fuzzer-generated\n\
                                 \x20      input(s) via native_decide; it did NOT prove the property holds for\n\
                                 \x20      every input the validator can receive.\n\
                                 \x20      Universal verification of state-machine halt/error tests will be\n\
                                 \x20      added in a future Aiken release.\n\
                                 \x20      Re-run with --accept-witness to allow CI to pass on witness-only proofs.\n\
                                 \x20      Note: {note}"
                        );
                        if let Some(extra) = t
                            .explanation
                            .as_deref()
                            .and_then(render_status_explanation_block)
                        {
                            block.push('\n');
                            block.push_str(&extra);
                        }
                        (
                            "WITN".if_supports_color(Stderr, |s| s.yellow()).to_string(),
                            headline,
                            Some(block),
                        )
                    }
                    ProofStatus::Failed { category, reason } => match category {
                        FailureCategory::Counterexample => (
                            "FAIL".if_supports_color(Stderr, |s| s.red()).to_string(),
                            solver_counterexample_label(t, reason),
                            t.explanation
                                .as_deref()
                                .and_then(render_status_explanation_block),
                        ),
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
                            let trailing_block = if reason.trim().is_empty()
                                && matches!(
                                    run_options.solver_profile,
                                    verify::SolverProfile::StrictCert
                                ) {
                                Some(format!(
                                    "       strict-cert requires LeanCertified results, but {} was reported; no additional solver failure reason was provided.",
                                    certification_label(t.certification)
                                ))
                            } else if reason.trim().is_empty() {
                                t.explanation
                                    .as_deref()
                                    .and_then(render_status_explanation_block)
                            } else {
                                None
                            };
                            (
                                "FAIL".if_supports_color(Stderr, |s| s.red()).to_string(),
                                format!(
                                    "FAILED [{}] [{}]",
                                    cat,
                                    certification_label(t.certification)
                                ),
                                trailing_block,
                            )
                        }
                    },
                    ProofStatus::TimedOut { .. } => (
                        "TIME".if_supports_color(Stderr, |s| s.yellow()).to_string(),
                        format!("TIMED OUT [{}]", certification_label(t.certification)),
                        t.explanation
                            .as_deref()
                            .and_then(render_status_explanation_block),
                    ),
                    ProofStatus::Unknown => (
                        "????".if_supports_color(Stderr, |s| s.yellow()).to_string(),
                        format!("UNKNOWN [{}]", certification_label(t.certification)),
                        t.explanation
                            .as_deref()
                            .and_then(render_status_explanation_block),
                    ),
                    _ => (
                        "????".if_supports_color(Stderr, |s| s.yellow()).to_string(),
                        format!("UNKNOWN [{}]", certification_label(t.certification)),
                        t.explanation
                            .as_deref()
                            .and_then(render_status_explanation_block),
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
                if let ProofStatus::Failed { category, reason } = &t.proof_status
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
                "\nResults: {} solver-validated, {} witness-validated, {} partial, {} failed, {} timed out, {} unknown out of {} theorems in {}",
                summary.proved,
                summary.witness,
                summary.partial,
                summary.failed,
                summary.timed_out,
                summary.unknown,
                summary.total,
                elapsed_str,
            );

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
                for advice in output::failure_artifact_advice(&resolved_out_dir, artifacts_retained)
                {
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
                return Ok(RunProjectResult {
                    root: project_root.clone(),
                    output,
                    exit_code: 1,
                });
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
                return Ok(RunProjectResult {
                    root: project_root.clone(),
                    output,
                    exit_code: 1,
                });
            }
            return skipped_tests_failure(manifest.skipped.len()).map(|_| RunProjectResult {
                root: project_root.clone(),
                output,
                exit_code: 1,
            });
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
            return Ok(RunProjectResult {
                root: project_root.clone(),
                output,
                exit_code: 1,
            });
        }
        return skipped_tests_failure(manifest.skipped.len()).map(|_| RunProjectResult {
            root: project_root.clone(),
            output,
            exit_code: 1,
        });
    }

    Ok(RunProjectResult {
        root: project_root,
        output,
        exit_code: 0,
    })
}

pub(super) fn run_compile_options(env: Option<String>) -> Options {
    Options {
        env,
        ..Default::default()
    }
}

pub(super) fn skips_require_failure(skipped_count: usize, allow_skips: bool) -> bool {
    skipped_count > 0 && !allow_skips
}

pub(super) fn ensure_strict_cert_failure_reasons(summary: &mut verify::VerifySummary) {
    for theorem in &mut summary.theorems {
        if let ProofStatus::Failed { reason, .. } = &mut theorem.proof_status
            && reason.trim().is_empty()
        {
            *reason = format!(
                "strict-cert requires LeanCertified results, but {} was reported; no additional solver failure reason was provided.",
                certification_label(theorem.certification)
            );
            theorem.explanation = Some(reason.clone());
        }
    }
}

/// Decide whether the CLI should report success based on the proof summary
/// and the `--accept-partial` / `--accept-witness` policy flags.
///
/// Failed, timed-out, and unknown theorems always cause failure. Partial
/// (sorry-closed) and witness-only proofs cause failure unless explicitly
/// opted in via the corresponding accept flag.
pub(super) fn proofs_succeeded(
    summary: &verify::VerifySummary,
    accept_partial: bool,
    accept_witness: bool,
) -> bool {
    if !summary.command_success {
        return false;
    }
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
pub(super) fn classify_partial_code(_note: &str) -> &'static str {
    "S0004"
}

pub(super) fn certification_label(certification: verify::Certification) -> &'static str {
    match certification {
        verify::Certification::SmtValidNoProofReconstruction => "smt_valid_no_proof_reconstruction",
        verify::Certification::SmtCounterexample => "smt_counterexample",
        verify::Certification::LeanKernelChecked => "lean_kernel_checked",
        verify::Certification::WitnessReplay => "witness_replay",
        verify::Certification::OpenObligations => "open_obligations",
        verify::Certification::Unsupported => "unsupported",
        verify::Certification::Timeout => "timeout",
        verify::Certification::Unknown => "unknown",
        _ => "unknown",
    }
}

pub(super) fn trust_profile_suffix(trust_profile: verify::TrustProfile) -> String {
    match trust_profile {
        verify::TrustProfile::Production => String::new(),
        _ => format!(" [{} trust]", trust_profile),
    }
}

pub(super) fn render_status_explanation_block(explanation: &str) -> Option<String> {
    let trimmed = explanation.trim();
    if trimmed.is_empty() {
        return None;
    }

    Some(
        trimmed
            .lines()
            .map(|line| format!("       {line}"))
            .collect::<Vec<_>>()
            .join("\n"),
    )
}

pub(super) fn should_cleanup_artifacts(policy: ArtifactRetention, command_success: bool) -> bool {
    !verify::should_retain_artifacts(policy, command_success)
}

pub(super) fn skipped_tests_failure(
    skipped_count: usize,
) -> Result<(), Vec<aiken_project::error::Error>> {
    Err(vec![aiken_project::error::Error::StandardIo(
        std::io::Error::other(format!(
            "{skipped_count} unsupported test(s) were skipped. Use --allow-skips to treat skips as success."
        )),
    )])
}

pub(super) fn collect_generate_only_preflight_errors(
    property_tests: &[ExportedPropertyTest],
    existential_mode: ExistentialMode,
    target: &VerificationTargetKind,
) -> Vec<String> {
    collect_generate_only_preflight_errors_with(property_tests, |t| {
        verify::preflight_validate_test(t, existential_mode, target)
    })
}

pub(super) fn collect_generate_only_preflight_errors_with<F>(
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
