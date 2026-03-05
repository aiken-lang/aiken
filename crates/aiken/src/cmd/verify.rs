use aiken_lang::ast::Tracing;
use aiken_project::{
    Project,
    export::{ExportedPropertyTest, VerificationTargetKind},
    options::Options,
    telemetry::EventTarget,
    verify::{
        self, ArtifactRetention, DEFAULT_BLASTER_REV, ExistentialMode, FailureCategory,
        ProofGenerationOptions, ProofStatus, VerifyConfig,
    },
    watch::with_project,
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
    #[clap(long, default_value = "20000")]
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
    #[clap(long)]
    skip_unsupported: bool,

    /// When used with --skip-unsupported, exit 0 even if tests were skipped.
    /// Without this flag, skipped tests cause a non-zero exit.
    #[clap(long, requires = "skip_unsupported")]
    allow_skips: bool,

    /// Git revision (commit, tag, or branch) for the Blaster dependency.
    /// Defaults to the version pinned in this release.
    #[clap(long, default_value = DEFAULT_BLASTER_REV)]
    blaster_rev: String,

    /// Strategy for `fail once` (existential) tests.
    /// `witness`: deterministic witness search + concrete proof (default).
    /// `proof`: attempt full existential theorem via Lean tactics.
    #[clap(long, default_value = "witness")]
    existential_mode: ExistentialMode,

    /// Verification target mode.
    /// `property` (default): verify property tests directly.
    /// `validator`: verify validator handler programs for tests that export validator metadata.
    /// `equivalence`: prove wrapper/handler equivalence for tests that export validator metadata.
    #[clap(long, default_value = "property")]
    target: VerificationTargetKind,

    /// Force sampled-domain fallback for all tests.
    /// When enabled, generated proofs always import and sample the fuzzer UPLC
    /// instead of using extracted direct-domain constraints.
    #[clap(long)]
    force_sampled_fallback: bool,
}

#[derive(clap::Args)]
#[clap(disable_version_flag(true))]
pub struct DoctorArgs {
    /// Path to project
    directory: Option<PathBuf>,

    /// Output directory for Lean workspace
    #[clap(long, default_value = "build/verify")]
    out_dir: PathBuf,

    /// Output results as JSON
    #[clap(long)]
    json: bool,

    /// Git revision (commit, tag, or branch) for the Blaster dependency to report
    #[clap(long, default_value = DEFAULT_BLASTER_REV)]
    blaster_rev: String,
}

#[derive(clap::Args)]
#[clap(disable_version_flag(true))]
pub struct CleanArgs {
    /// Path to project
    directory: Option<PathBuf>,

    /// Output directory containing verification artifacts to remove
    #[clap(long, default_value = "build/verify")]
    out_dir: PathBuf,
}

#[derive(clap::Args)]
#[clap(disable_version_flag(true))]
pub struct CapabilitiesArgs {
    /// Output as JSON
    #[clap(long)]
    json: bool,
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
    json: bool,
    skip_unsupported: bool,
    allow_skips: bool,
    blaster_rev: String,
    existential_mode: ExistentialMode,
    target: VerificationTargetKind,
    force_sampled_fallback: bool,
}

pub fn exec(cmd: Cmd) -> miette::Result<()> {
    match cmd {
        Cmd::Run(args) => exec_run(args),
        Cmd::Doctor(args) => exec_doctor(args),
        Cmd::Clean(args) => exec_clean(args),
        Cmd::Capabilities(args) => exec_capabilities(args),
    }
}

fn format_no_property_tests_output(json: bool) -> Result<String, serde_json::Error> {
    if json {
        serde_json::to_string_pretty(&serde_json::json!({
            "status": "no_property_tests",
            "message": "No property tests found.",
        }))
    } else {
        Ok("No property tests found.".to_string())
    }
}

fn no_property_tests_early_output(
    property_tests: &[ExportedPropertyTest],
    json: bool,
) -> Result<Option<String>, serde_json::Error> {
    if property_tests.is_empty() {
        format_no_property_tests_output(json).map(Some)
    } else {
        Ok(None)
    }
}

fn run_proofs_start_output(json: bool) -> Option<&'static str> {
    (!json).then_some("Running proofs via lake build...")
}

fn no_proofs_summary(
    manifest: &verify::GeneratedManifest,
    skipped_without_allow: bool,
) -> verify::VerifySummary {
    verify::VerifySummary {
        total: 0,
        proved: 0,
        failed: 0,
        timed_out: 0,
        unknown: 0,
        skipped: manifest.skipped.clone(),
        fallbacks: manifest.fallbacks.clone(),
        theorems: Vec::new(),
        raw_output: verify::VerifyResult {
            success: !skipped_without_allow,
            stdout: String::new(),
            stderr: String::new(),
            exit_code: Some(if skipped_without_allow { 1 } else { 0 }),
            theorem_results: None,
        },
        elapsed_ms: None,
    }
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
                "VERSION TOO LOW"
                    .if_supports_color(Stderr, |s| s.red())
                    .to_string(),
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
    project_root: &Path,
    out_dir: PathBuf,
    json: bool,
    blaster_rev: String,
    run_doctor: F,
) -> miette::Result<CommandBranchResult>
where
    F: FnOnce(&Path, &str) -> verify::DoctorReport,
{
    let out_dir = resolve_verify_out_dir(&out_dir, project_root)?;
    let report = run_doctor(&out_dir, &blaster_rev);
    let output = format_doctor_output(&report, json)?;
    let exit_code = doctor_exit_code(&report);

    Ok(CommandBranchResult { output, exit_code })
}

fn exec_doctor(
    DoctorArgs {
        directory,
        out_dir,
        json,
        blaster_rev,
    }: DoctorArgs,
) -> miette::Result<()> {
    let mut exit_code = 0;

    if with_project(directory.as_deref(), false, false, false, |p| {
        let result = run_doctor_command_with(
            p.root(),
            out_dir.clone(),
            json,
            blaster_rev.clone(),
            verify::run_doctor,
        )
        .map_err(|e| {
            vec![aiken_project::error::Error::StandardIo(
                std::io::Error::new(std::io::ErrorKind::Other, e.to_string()),
            )]
        })?;

        print!("{}", result.output);
        exit_code = exit_code.max(result.exit_code);

        Ok(())
    })
    .is_err()
    {
        process::exit(1);
    }

    if exit_code != 0 {
        process::exit(exit_code);
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

fn exec_clean(CleanArgs { directory, out_dir }: CleanArgs) -> miette::Result<()> {
    with_project(directory.as_deref(), false, false, false, |p| {
        let result = run_clean_command_with(p.root(), out_dir.clone(), verify::clean_artifacts)
            .map_err(|e| {
                vec![aiken_project::error::Error::StandardIo(
                    std::io::Error::new(std::io::ErrorKind::Other, e.to_string()),
                )]
            })?;

        print!("{}", result.output);
        Ok(())
    })
    .map_err(|_| process::exit(1))
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
                    if let Some((name, value)) = tail.split_once('=') {
                        if is_simple_identifier(name.trim()) {
                            return Some(simplify_counterexample_expr(value.trim()));
                        }
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
        allow_skips,
        blaster_rev,
        existential_mode,
        target,
        force_sampled_fallback,
    }: RunArgs,
) -> miette::Result<()> {
    // Handle deprecated --keep-artifacts flag: treat as --artifacts always
    let artifact_policy = if keep_artifacts {
        eprintln!(
            "{} --keep-artifacts is deprecated; use --artifacts always",
            "Warning:"
                .if_supports_color(Stderr, |s| s.yellow())
                .if_supports_color(Stderr, |s| s.bold()),
        );
        ArtifactRetention::Always
    } else {
        artifacts
    };

    let jobs_override = (jobs != 0).then_some(jobs);
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
        json,
        skip_unsupported,
        allow_skips,
        blaster_rev,
        existential_mode,
        target,
        force_sampled_fallback,
    };

    with_project(directory.as_deref(), deny, silent, true, |p| {
        exec_run_with_project(p, &run_options)
    })
    .map_err(|_| process::exit(1))
}

fn exec_run_with_project(
    p: &mut Project<EventTarget>,
    run_options: &RunCommandOptions,
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

    if let Some(output) = no_property_tests_early_output(property_tests, run_options.json)
        .map_err(|e| vec![aiken_project::error::Error::Json(e)])?
    {
        println!("{output}");
        return Ok(());
    }

    // When --generate-only, run theorem-shape preflight before file generation work.
    // This uses the same proof generator path (including sampled-domain fallback),
    // so fallback-capable tests are accepted here.
    if run_options.generate_only && !run_options.skip_unsupported {
        let unsupported = collect_generate_only_preflight_errors(
            property_tests,
            run_options.existential_mode,
            &run_options.target,
            run_options.force_sampled_fallback,
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

    let config = VerifyConfig {
        out_dir: resolved_out_dir.clone(),
        cek_budget: run_options.cek_budget,
        blaster_rev: run_options.blaster_rev.clone(),
        existential_mode: run_options.existential_mode,
        target: run_options.target.clone(),
    };

    let manifest = verify::generate_lean_workspace_with_options(
        property_tests,
        &config,
        run_options.skip_unsupported,
        ProofGenerationOptions {
            force_sampled_fallback: run_options.force_sampled_fallback,
        },
    )
    .map_err(|e| {
        vec![aiken_project::error::Error::StandardIo(
            std::io::Error::new(std::io::ErrorKind::Other, e.to_string()),
        )]
    })?;

    let skipped_without_allow =
        skips_require_failure(manifest.skipped.len(), run_options.allow_skips);

    // Report skipped tests
    if !manifest.skipped.is_empty() && !run_options.json {
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

    if run_options.force_sampled_fallback && !run_options.json {
        eprintln!(
            "{} Forcing sampled-domain fallback for all generated proofs (--force-sampled-fallback).",
            "Notice:"
                .if_supports_color(Stderr, |s| s.yellow())
                .if_supports_color(Stderr, |s| s.bold()),
        );
        eprintln!();
    }

    if !manifest.fallbacks.is_empty() && !run_options.json {
        eprintln!(
            "{} Using sampled-domain fallback for {} test(s):",
            "Notice:"
                .if_supports_color(Stderr, |s| s.yellow())
                .if_supports_color(Stderr, |s| s.bold()),
            manifest.fallbacks.len(),
        );
        for fallback in &manifest.fallbacks {
            eprintln!("  - {}: {}", fallback.name, fallback.reason);
        }
        eprintln!();
    }

    if run_options.generate_only {
        if run_options.json {
            let output = serde_json::to_string_pretty(&manifest)
                .map_err(|e| vec![aiken_project::error::Error::Json(e)])?;
            println!("{output}");
        } else if skipped_without_allow {
            eprintln!(
                "{} Lean workspace generated at {}, but {} unsupported test(s) were skipped. \
                 This run will exit non-zero. Use --allow-skips to treat skips as success.",
                "Warning:"
                    .if_supports_color(Stderr, |s| s.yellow())
                    .if_supports_color(Stderr, |s| s.bold()),
                resolved_out_dir.display(),
                manifest.skipped.len(),
            );
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
            println!(
                "Note: The PlutusCore Lean library is configured at {}.",
                verify::plutus_core_dir().display(),
            );
            println!(
                "      Update DEFAULT_PLUTUS_CORE_DIR in crates/aiken-project/src/verify.rs if needed."
            );
        }
    } else {
        if manifest.tests.is_empty() {
            if run_options.json {
                let output = serde_json::to_string_pretty(&no_proofs_summary(
                    &manifest,
                    skipped_without_allow,
                ))
                .map_err(|e| vec![aiken_project::error::Error::Json(e)])?;
                println!("{output}");
            } else {
                println!("No supported property tests remain to prove.");
            }

            if skipped_without_allow && !run_options.json {
                eprintln!(
                    "{} {} unsupported test(s) were skipped; exiting non-zero. Use --allow-skips to treat skips as success.",
                    "Error:"
                        .if_supports_color(Stderr, |s| s.red())
                        .if_supports_color(Stderr, |s| s.bold()),
                    manifest.skipped.len(),
                );
            }

            if should_cleanup_artifacts(run_options.artifact_policy, true, skipped_without_allow)
                && let Err(e) = verify::clear_generated_workspace(&resolved_out_dir)
            {
                eprintln!(
                    "Warning: failed to clean up {}: {}",
                    resolved_out_dir.display(),
                    e
                );
            }

            if skipped_without_allow {
                return skipped_tests_failure(manifest.skipped.len());
            }

            return Ok(());
        }

        verify::check_toolchain().map_err(|e| {
            vec![aiken_project::error::Error::StandardIo(
                std::io::Error::new(std::io::ErrorKind::NotFound, e.to_string()),
            )]
        })?;

        verify::check_plutus_core(&resolved_out_dir).map_err(|e| {
            vec![aiken_project::error::Error::StandardIo(
                std::io::Error::new(std::io::ErrorKind::NotFound, e.to_string()),
            )]
        })?;

        if let Some(output) = run_proofs_start_output(run_options.json) {
            println!("{output}");
        }

        let start = std::time::Instant::now();
        let result = verify::run_proofs(
            &resolved_out_dir,
            run_options.timeout,
            run_options.jobs_override,
            &manifest,
        )
        .map_err(|e| {
            vec![aiken_project::error::Error::StandardIo(
                std::io::Error::new(std::io::ErrorKind::Other, e.to_string()),
            )]
        })?;
        let elapsed = start.elapsed();

        let mut summary = verify::parse_verify_results(result, &manifest);
        summary.elapsed_ms = Some(elapsed.as_millis() as u64);

        if run_options.json {
            let output = serde_json::to_string_pretty(&summary)
                .map_err(|e| vec![aiken_project::error::Error::Json(e)])?;
            println!("{output}");
        } else {
            println!();
            for t in &summary.theorems {
                let (icon, label) = match &t.status {
                    ProofStatus::Proved => (
                        "PASS".if_supports_color(Stderr, |s| s.green()).to_string(),
                        "PROVED".to_string(),
                    ),
                    ProofStatus::Failed { category, reason } => match category {
                        FailureCategory::Counterexample => {
                            let label = extract_counterexample_display(reason)
                                .map(|value| format!("COUNTEREXAMPLE: {value}"))
                                .unwrap_or_else(|| "COUNTEREXAMPLE".to_string());
                            (
                                "FAIL".if_supports_color(Stderr, |s| s.red()).to_string(),
                                label,
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
                            };
                            (
                                "FAIL".if_supports_color(Stderr, |s| s.red()).to_string(),
                                format!("FAILED [{}]", cat),
                            )
                        }
                    },
                    ProofStatus::TimedOut { .. } => (
                        "TIME".if_supports_color(Stderr, |s| s.yellow()).to_string(),
                        "TIMED OUT".to_string(),
                    ),
                    ProofStatus::Unknown => (
                        "????".if_supports_color(Stderr, |s| s.yellow()).to_string(),
                        "UNKNOWN".to_string(),
                    ),
                };
                println!(
                    "  {} {} [{}] - {}",
                    icon, t.test_name, t.theorem_name, label
                );
                // Print inline failure context snippet
                if let ProofStatus::Failed { category, reason } = &t.status {
                    if *category != FailureCategory::Counterexample && !reason.is_empty() {
                        for line in reason.lines().take(10) {
                            println!("       {}", line.if_supports_color(Stderr, |s| s.dimmed()));
                        }
                        let total_lines = reason.lines().count();
                        if total_lines > 10 {
                            println!("       {} more lines in logs...", total_lines - 10);
                        }
                    }
                }
            }

            let elapsed_str = if elapsed.as_secs() > 0 {
                format!("{}s", elapsed.as_secs())
            } else {
                format!("{}ms", elapsed.as_millis())
            };

            println!(
                "\nResults: {} proved, {} failed, {} timed out, {} unknown out of {} theorems in {}",
                summary.proved,
                summary.failed,
                summary.timed_out,
                summary.unknown,
                summary.total,
                elapsed_str,
            );

            if summary.failed > 0 || summary.timed_out > 0 || summary.unknown > 0 {
                let stderr_for_display = sanitize_stderr_for_display(&summary.raw_output.stderr);
                if !stderr_for_display.trim().is_empty() {
                    eprintln!("\n{}", stderr_for_display);
                }
                eprintln!("Logs available at {}/logs/", resolved_out_dir.display());
                eprintln!(
                    "To reproduce: cd {} && lake build",
                    resolved_out_dir.display(),
                );
            }
        }

        let proofs_succeeded =
            summary.failed == 0 && summary.timed_out == 0 && summary.unknown == 0;
        let skipped_causes_failure = skipped_without_allow;

        if skipped_causes_failure && !run_options.json {
            eprintln!(
                "{} {} unsupported test(s) were skipped; exiting non-zero. Use --allow-skips to treat skips as success.",
                "Error:"
                    .if_supports_color(Stderr, |s| s.red())
                    .if_supports_color(Stderr, |s| s.bold()),
                manifest.skipped.len(),
            );
        }

        // Apply artifact retention policy after computing final command status.
        if should_cleanup_artifacts(
            run_options.artifact_policy,
            proofs_succeeded,
            skipped_causes_failure,
        ) && let Err(e) = verify::clear_generated_workspace(&resolved_out_dir)
        {
            eprintln!(
                "Warning: failed to clean up {}: {}",
                resolved_out_dir.display(),
                e
            );
        }

        if !proofs_succeeded {
            return Err(vec![aiken_project::error::Error::StandardIo(
                std::io::Error::new(
                    std::io::ErrorKind::Other,
                    format!(
                        "Proof verification incomplete: {} failed, {} timed out, {} unknown out of {} theorems",
                        summary.failed, summary.timed_out, summary.unknown, summary.total
                    ),
                ),
            )]);
        }

        if skipped_causes_failure {
            return skipped_tests_failure(manifest.skipped.len());
        }
    }

    // In generate-only mode, skipped tests still fail the run unless --allow-skips is set.
    if run_options.generate_only && skipped_without_allow {
        if !run_options.json {
            eprintln!(
                "{} {} unsupported test(s) were skipped; exiting non-zero. Use --allow-skips to treat skips as success.",
                "Error:"
                    .if_supports_color(Stderr, |s| s.red())
                    .if_supports_color(Stderr, |s| s.bold()),
                manifest.skipped.len(),
            );
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

fn command_succeeded(proofs_succeeded: bool, skipped_without_allow: bool) -> bool {
    proofs_succeeded && !skipped_without_allow
}

fn should_cleanup_artifacts(
    policy: ArtifactRetention,
    proofs_succeeded: bool,
    skipped_without_allow: bool,
) -> bool {
    !verify::should_retain_artifacts(
        policy,
        command_succeeded(proofs_succeeded, skipped_without_allow),
    )
}

fn skipped_tests_failure(skipped_count: usize) -> Result<(), Vec<aiken_project::error::Error>> {
    Err(vec![aiken_project::error::Error::StandardIo(
        std::io::Error::new(
            std::io::ErrorKind::Other,
            format!(
                "{skipped_count} unsupported test(s) were skipped. Use --allow-skips to treat skips as success."
            ),
        ),
    )])
}

fn collect_generate_only_preflight_errors(
    property_tests: &[ExportedPropertyTest],
    existential_mode: ExistentialMode,
    target: &VerificationTargetKind,
    force_sampled_fallback: bool,
) -> Vec<String> {
    collect_generate_only_preflight_errors_with(property_tests, |t| {
        verify::preflight_validate_test_with_options(
            t,
            existential_mode,
            target,
            ProofGenerationOptions {
                force_sampled_fallback,
            },
        )
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
    use aiken_lang::ast::OnTestFailure;
    use aiken_project::{
        Project,
        export::{ExportedProgram, FuzzerConstraint, FuzzerOutputType, TestReturnMode},
        telemetry::EventTarget,
    };
    use clap::Parser;
    #[cfg(unix)]
    use std::os::unix::fs::symlink;
    use std::{
        fs,
        path::{Path, PathBuf},
        time::SystemTime,
    };

    fn dummy_property_test(
        name: &str,
        fuzzer_output_type: FuzzerOutputType,
        constraint: FuzzerConstraint,
    ) -> ExportedPropertyTest {
        ExportedPropertyTest {
            name: name.to_string(),
            module: "example".to_string(),
            input_path: "lib/example.ak".to_string(),
            on_test_failure: OnTestFailure::FailImmediately,
            return_mode: TestReturnMode::Bool,
            target_kind: VerificationTargetKind::PropertyWrapper,
            validator_target: None,
            test_program: ExportedProgram {
                hex: String::new(),
                flat_bytes: None,
            },
            fuzzer_program: ExportedProgram {
                hex: String::new(),
                flat_bytes: None,
            },
            fuzzer_type: "Int".to_string(),
            fuzzer_output_type,
            constraint,
        }
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
fn byte_fuzzer() -> Fuzzer<ByteArray> {
  todo
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

test foo_mint_roundtrip(policy_id via byte_fuzzer()) {
  foo.mint(Void, policy_id, Void)
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

    fn fixture_run_options(out_dir: PathBuf, target: VerificationTargetKind) -> RunCommandOptions {
        RunCommandOptions {
            match_tests: None,
            exact_match: false,
            env: None,
            generate_only: true,
            out_dir,
            artifact_policy: ArtifactRetention::OnFailure,
            timeout: 300,
            cek_budget: 20_000,
            jobs_override: None,
            json: true,
            skip_unsupported: false,
            allow_skips: false,
            blaster_rev: DEFAULT_BLASTER_REV.to_string(),
            existential_mode: ExistentialMode::default(),
            target,
            force_sampled_fallback: false,
        }
    }

    fn sample_doctor_report(all_ok: bool, blaster_rev: &str) -> verify::DoctorReport {
        verify::DoctorReport {
            tools: vec![
                verify::ToolCheck {
                    tool: "lean".to_string(),
                    found: true,
                    version: Some("4.11.0".to_string()),
                    meets_minimum: true,
                    minimum_version: "4.11.0".to_string(),
                    error: None,
                },
                verify::ToolCheck {
                    tool: "lake".to_string(),
                    found: true,
                    version: Some("4.11.0".to_string()),
                    meets_minimum: true,
                    minimum_version: "0.0.0".to_string(),
                    error: None,
                },
                verify::ToolCheck {
                    tool: "z3".to_string(),
                    found: true,
                    version: Some("4.13.0".to_string()),
                    meets_minimum: true,
                    minimum_version: "4.13.0".to_string(),
                    error: None,
                },
            ],
            plutus_core: verify::PlutusCoreCheck {
                found: true,
                path: "build/verify/PlutusCore".to_string(),
                has_lakefile: true,
                error: None,
            },
            blaster_rev: blaster_rev.to_string(),
            all_ok,
            capabilities: verify::capabilities(),
        }
    }

    #[test]
    fn doctor_command_branch_reports_success_output_and_exit_code() {
        let result = run_doctor_command_with(
            Path::new("workspace/member-a"),
            PathBuf::from("build/verify"),
            false,
            "abc123".to_string(),
            |_out_dir, blaster_rev| sample_doctor_report(true, blaster_rev),
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
            Path::new("workspace/member-a"),
            PathBuf::from("build/verify"),
            true,
            "deadbeef".to_string(),
            |_out_dir, blaster_rev| sample_doctor_report(false, blaster_rev),
        )
        .expect("doctor branch should render JSON output");

        assert_eq!(result.exit_code, 1);
        let value: serde_json::Value =
            serde_json::from_str(&result.output).expect("doctor JSON output should parse");
        assert_eq!(value["all_ok"], false);
        assert_eq!(value["blaster_rev"], "deadbeef");
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
    fn doctor_command_branch_resolves_relative_out_dir_under_project_root() {
        let project_root = PathBuf::from("workspace/member-a");
        let mut resolved_out_dir = None;

        let _result = run_doctor_command_with(
            &project_root,
            PathBuf::from("build/verify"),
            false,
            "abc123".to_string(),
            |out_dir, blaster_rev| {
                resolved_out_dir = Some(out_dir.to_path_buf());
                sample_doctor_report(true, blaster_rev)
            },
        )
        .expect("doctor branch should resolve out_dir against project root");

        assert_eq!(resolved_out_dir, Some(project_root.join("build/verify")));
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
        assert_eq!(value["max_test_arity"], 1);
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
        let output = format_no_property_tests_output(true)
            .expect("json mode output should serialize successfully");
        let value: serde_json::Value =
            serde_json::from_str(&output).expect("json mode output should be valid JSON");

        assert_eq!(value["status"], "no_property_tests");
        assert_eq!(value["message"], "No property tests found.");
    }

    #[test]
    fn no_property_tests_early_output_handles_empty_and_non_empty_slices() {
        let no_tests: Vec<ExportedPropertyTest> = vec![];
        let output = no_property_tests_early_output(&no_tests, true)
            .expect("empty slice should produce serializable JSON output")
            .expect("empty slice should return Some(output)");
        let value: serde_json::Value =
            serde_json::from_str(&output).expect("empty-slice output should be valid JSON");
        assert_eq!(value["status"], "no_property_tests");
        assert_eq!(value["message"], "No property tests found.");

        let one_test = vec![dummy_property_test(
            "example.test_ok",
            FuzzerOutputType::Int,
            FuzzerConstraint::Any,
        )];
        let output = no_property_tests_early_output(&one_test, true)
            .expect("non-empty slice should not fail");
        assert!(
            output.is_none(),
            "non-empty slice should not trigger early no-tests output"
        );
    }

    #[test]
    fn run_proofs_start_output_is_suppressed_in_json_mode() {
        assert_eq!(run_proofs_start_output(true), None);
    }

    #[test]
    fn run_proofs_start_output_is_present_in_text_mode() {
        assert_eq!(
            run_proofs_start_output(false),
            Some("Running proofs via lake build...")
        );
    }

    #[test]
    fn no_proofs_summary_reports_skipped_metadata_and_failure_status() {
        let manifest = verify::GeneratedManifest {
            version: "1.0.0".to_string(),
            tests: Vec::new(),
            skipped: vec![verify::SkippedTest {
                name: "example.test_unsupported".to_string(),
                module: "example".to_string(),
                reason: "unsupported shape".to_string(),
            }],
            fallbacks: Vec::new(),
        };

        let summary = no_proofs_summary(&manifest, true);

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
        let manifest = verify::GeneratedManifest {
            version: "1.0.0".to_string(),
            tests: Vec::new(),
            skipped: vec![verify::SkippedTest {
                name: "example.test_unsupported".to_string(),
                module: "example".to_string(),
                reason: "unsupported shape".to_string(),
            }],
            fallbacks: Vec::new(),
        };

        let summary = no_proofs_summary(&manifest, false);

        assert!(summary.raw_output.success);
        assert_eq!(summary.raw_output.exit_code, Some(0));
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
        run_options.skip_unsupported = true;
        run_options.allow_skips = true;
        run_options.target = VerificationTargetKind::ValidatorHandler;

        let mut project = Project::new(fixture_root.clone(), EventTarget::default())
            .expect("fixture project should load");
        let result = exec_run_with_project(&mut project, &run_options);
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
            if let Err(errors) = exec_run_with_project(&mut project, &run_options) {
                panic!(
                    "verify run should support --target {target}; got {} error(s)",
                    errors.len()
                );
            }

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

            assert_eq!(
                manifest_value["tests"].as_array().map(|tests| tests.len()),
                Some(1),
                "fixture should generate exactly one test for --target {target}"
            );

            let lean_file = manifest_value["tests"][0]["lean_file"]
                .as_str()
                .expect("manifest test entry should include lean_file");
            let proof = fs::read_to_string(resolved_out_dir.join(lean_file))
                .expect("generated proof file should be readable");
            assert!(
                proof.contains("_handler.flat"),
                "generated proof should import handler flat file for --target {target}"
            );

            if target == VerificationTargetKind::Equivalence {
                let theorem = manifest_value["tests"][0]["lean_theorem"]
                    .as_str()
                    .expect("manifest test entry should include lean_theorem");
                assert!(
                    proof.contains(&format!("theorem {theorem}_equivalence :")),
                    "equivalence target should generate an explicit equivalence theorem"
                );
            }
        }

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
    fn run_args_parse_force_sampled_fallback_flag() {
        #[derive(Parser)]
        struct VerifyCli {
            #[command(subcommand)]
            cmd: Cmd,
        }

        let parsed = VerifyCli::try_parse_from([
            "aiken-verify",
            "run",
            "--generate-only",
            "--force-sampled-fallback",
        ])
        .expect("`verify run --force-sampled-fallback` should parse");

        let Cmd::Run(args) = parsed.cmd else {
            panic!("expected `run` subcommand")
        };
        assert!(args.force_sampled_fallback);
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
    fn doctor_args_directory_and_relative_out_dir_resolve_under_project_root() {
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
            "doctor",
            project_root_arg.as_str(),
            "--out-dir",
            "build/verify",
        ])
        .expect("`verify doctor <project-dir> --out-dir build/verify` should parse");

        let Cmd::Doctor(args) = parsed.cmd else {
            panic!("expected `doctor` subcommand")
        };

        assert_eq!(args.directory, Some(project_root.clone()));
        assert_eq!(
            resolve_verify_out_dir(&args.out_dir, &project_root)
                .expect("doctor args out_dir should resolve under project root"),
            project_root.join("build/verify")
        );
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

    #[test]
    fn skips_require_failure_only_when_skips_are_disallowed() {
        assert!(skips_require_failure(1, false));
        assert!(!skips_require_failure(0, false));
        assert!(!skips_require_failure(1, true));
    }

    #[test]
    fn command_succeeded_treats_disallowed_skips_as_failure() {
        assert!(command_succeeded(true, false));
        assert!(!command_succeeded(true, true));
        assert!(!command_succeeded(false, false));
    }

    #[test]
    fn artifact_retention_on_failure_keeps_artifacts_when_skips_fail_run() {
        assert!(
            !should_cleanup_artifacts(ArtifactRetention::OnFailure, true, true),
            "on-failure policy should retain artifacts when run exits non-zero due to disallowed skips"
        );
    }

    #[test]
    fn artifact_retention_on_failure_cleans_after_full_success() {
        assert!(
            should_cleanup_artifacts(ArtifactRetention::OnFailure, true, false),
            "on-failure policy should clean artifacts after a fully successful run"
        );
    }

    #[test]
    fn artifact_retention_on_success_requires_full_success() {
        assert!(
            !should_cleanup_artifacts(ArtifactRetention::OnSuccess, true, false),
            "on-success policy should retain artifacts only when run fully succeeds"
        );
        assert!(
            should_cleanup_artifacts(ArtifactRetention::OnSuccess, true, true),
            "on-success policy should clean artifacts when run exits non-zero due to disallowed skips"
        );
    }
}
