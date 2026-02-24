use aiken_lang::ast::Tracing;
use aiken_project::{
    export::VerificationTargetKind,
    options::Options,
    verify::{
        self, ArtifactRetention, ExistentialMode, FailureCategory, ProofStatus, VerifyConfig,
        DEFAULT_BLASTER_REV,
    },
    watch::with_project,
};
use owo_colors::{OwoColorize, Stream::Stderr};
use std::{path::PathBuf, process};

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

    /// Number of parallel theorem builds (default: number of logical CPUs, max 8)
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
    /// `validator`: verify validator handler programs.
    /// `equivalence`: prove property wrapper and validator handler produce identical results.
    #[clap(long, default_value = "property")]
    target: VerificationTargetKind,
}

#[derive(clap::Args)]
#[clap(disable_version_flag(true))]
pub struct DoctorArgs {
    /// Output directory for Lean workspace (used to check PlutusCore)
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

pub fn exec(cmd: Cmd) -> miette::Result<()> {
    match cmd {
        Cmd::Run(args) => exec_run(args),
        Cmd::Doctor(args) => exec_doctor(args),
        Cmd::Clean(args) => exec_clean(args),
        Cmd::Capabilities(args) => exec_capabilities(args),
    }
}

fn exec_doctor(
    DoctorArgs {
        out_dir,
        json,
        blaster_rev,
    }: DoctorArgs,
) -> miette::Result<()> {
    let report = verify::run_doctor(&out_dir, &blaster_rev);

    if json {
        let output = serde_json::to_string_pretty(&report).unwrap();
        println!("{output}");
    } else {
        println!("Verify Doctor Report");
        println!("====================\n");

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
            println!("  {}: {} ({})", tool.tool, status, version_str);

            if let Some(err) = &tool.error {
                println!("    {}", err.if_supports_color(Stderr, |s| s.yellow()));
            }
        }

        println!();

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
        println!("  PlutusCore: {} ({})", pc_status, report.plutus_core.path);
        if let Some(err) = &report.plutus_core.error {
            println!("    {}", err.if_supports_color(Stderr, |s| s.yellow()));
        }

        println!();
        println!("  Blaster revision: {}", report.blaster_rev);

        println!();
        if report.all_ok {
            println!(
                "{}",
                "All checks passed."
                    .if_supports_color(Stderr, |s| s.green())
                    .if_supports_color(Stderr, |s| s.bold())
            );
        } else {
            println!(
                "{}",
                "Some checks failed. See above for details."
                    .if_supports_color(Stderr, |s| s.red())
                    .if_supports_color(Stderr, |s| s.bold())
            );
        }
    }

    if !report.all_ok {
        std::process::exit(1);
    }

    Ok(())
}

fn exec_clean(CleanArgs { out_dir }: CleanArgs) -> miette::Result<()> {
    match verify::clean_artifacts(&out_dir) {
        Ok(removed) => {
            if removed.is_empty() {
                println!("No verification artifacts found at {}", out_dir.display());
            } else {
                for p in &removed {
                    println!("Removed {}", p.display());
                }
            }
            Ok(())
        }
        Err(e) => Err(miette::miette!(
            "Failed to clean verification artifacts at {}: {}",
            out_dir.display(),
            e
        )),
    }
}

fn exec_capabilities(CapabilitiesArgs { json }: CapabilitiesArgs) -> miette::Result<()> {
    let caps = verify::capabilities();

    if json {
        let output = serde_json::to_string_pretty(&caps).unwrap();
        println!("{output}");
    } else {
        println!("Verification Capabilities");
        println!("=========================\n");

        println!("Supported test kinds:");
        for k in &caps.supported_test_kinds {
            println!("  - {k}");
        }

        println!();
        println!("Unsupported test kinds:");
        for n in &caps.unsupported_test_kinds {
            println!("  - {} [{}]: {}", n.kind, n.status, n.reason);
        }

        println!();
        println!("Target modes:");
        for m in &caps.target_modes {
            println!("  - {m}");
        }

        println!();
        println!("Supported fuzzer output types:");
        for t in &caps.supported_fuzzer_types {
            println!("  - {t}");
        }

        println!();
        println!("Unsupported fuzzer output types:");
        for t in &caps.unsupported_fuzzer_types {
            println!("  - {t}");
        }

        println!();
        println!("Existential modes:");
        for m in &caps.existential_modes {
            println!("  - {m}");
        }

        println!();
        println!("Max test arity: {}", caps.max_test_arity);
    }

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

    let max_jobs = if jobs == 0 {
        std::thread::available_parallelism()
            .map(|n| n.get().min(8))
            .unwrap_or(1)
    } else {
        jobs
    };
    with_project(directory.as_deref(), deny, silent, true, |p| {
        p.compile(Options {
            ..Default::default()
        })?;

        let exported = p
            .export_tests(match_tests.clone(), exact_match, Tracing::silent(), false)
            .map_err(|e| vec![e])?;

        let property_tests = &exported.property_tests;

        if property_tests.is_empty() {
            println!("No property tests found.");
            return Ok(());
        }

        // Validator and equivalence modes require validator metadata that is
        // not yet populated during export. Reject early with a clear message.
        if matches!(
            target,
            VerificationTargetKind::ValidatorHandler | VerificationTargetKind::Equivalence
        ) {
            return Err(vec![aiken_project::error::Error::StandardIo(
                std::io::Error::new(
                    std::io::ErrorKind::Unsupported,
                    format!(
                        "--target {} is not yet supported for in-project tests. \
                         Validator metadata export is not implemented; \
                         use --target property (the default) instead.",
                        target,
                    ),
                ),
            )]);
        }

        // When --generate-only, reject tests where bounds are required but
        // the constraint doesn't provide them (e.g. Int with Any constraint).
        if generate_only && !skip_unsupported {
            let unknown: Vec<&str> = property_tests
                .iter()
                .filter(|t| {
                    verify::requires_explicit_bounds(&t.fuzzer_output_type, &t.constraint)
                })
                .map(|t| t.name.as_str())
                .collect();

            if !unknown.is_empty() {
                let names = unknown.join("\n  - ");
                return Err(vec![aiken_project::error::Error::StandardIo(
                    std::io::Error::new(
                        std::io::ErrorKind::InvalidData,
                        format!(
                            "Cannot generate Lean workspace: the following property tests have \
                             unknown fuzzer bounds and cannot be verified:\n  - {names}\n\n\
                             Hint: use fuzzers with explicit integer bounds (e.g. \
                             `fuzz.int_between`) so the prover knows the input domain, \
                             or use --skip-unsupported to skip these tests."
                        ),
                    ),
                )]);
            }
        }

        if !generate_only {
            verify::check_toolchain().map_err(|e| {
                vec![aiken_project::error::Error::StandardIo(
                    std::io::Error::new(std::io::ErrorKind::NotFound, e.to_string()),
                )]
            })?;
        }

        let config = VerifyConfig {
            out_dir: out_dir.clone(),
            cek_budget,
            blaster_rev: blaster_rev.clone(),
            existential_mode,
            target: target.clone(),
        };

        let manifest = verify::generate_lean_workspace(property_tests, &config, skip_unsupported)
            .map_err(|e| {
                vec![aiken_project::error::Error::StandardIo(
                    std::io::Error::new(std::io::ErrorKind::Other, e.to_string()),
                )]
            })?;

        // Report skipped tests
        if !manifest.skipped.is_empty() && !json {
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

        if generate_only {
            if json {
                let output = serde_json::to_string_pretty(&manifest).unwrap();
                println!("{output}");
            } else {
                println!(
                    "Generated Lean workspace at {} with {} property test(s):",
                    out_dir.display(),
                    manifest.tests.len(),
                );
                for entry in &manifest.tests {
                    println!("  - {} -> {}", entry.aiken_module, entry.lean_module);
                }
                println!();
                println!(
                    "Note: The PlutusCore Lean library must be available at {}/PlutusCore.",
                    out_dir.display(),
                );
                println!("      You can symlink or copy it there before running `lake build`.");
            }
        } else {
            println!("Running proofs via lake build...");

            let start = std::time::Instant::now();
            let result = verify::run_proofs(&out_dir, timeout, max_jobs, &manifest).map_err(|e| {
                vec![aiken_project::error::Error::StandardIo(
                    std::io::Error::new(std::io::ErrorKind::Other, e.to_string()),
                )]
            })?;
            let elapsed = start.elapsed();

            let mut summary = verify::parse_verify_results(result, &manifest);
            summary.elapsed_ms = Some(elapsed.as_millis() as u64);

            if json {
                let output = serde_json::to_string_pretty(&summary).unwrap();
                println!("{output}");
            } else {
                println!();
                for t in &summary.theorems {
                    let (icon, label) = match &t.status {
                        ProofStatus::Proved => (
                            "PASS"
                                .if_supports_color(Stderr, |s| s.green())
                                .to_string(),
                            "PROVED".to_string(),
                        ),
                        ProofStatus::Failed { category, reason } => match category {
                            FailureCategory::Counterexample => {
                                let label = extract_counterexample_display(reason)
                                    .map(|value| format!("COUNTEREXAMPLE: {value}"))
                                    .unwrap_or_else(|| "COUNTEREXAMPLE".to_string());
                                (
                                    "FAIL"
                                        .if_supports_color(Stderr, |s| s.red())
                                        .to_string(),
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
                                    "FAIL"
                                        .if_supports_color(Stderr, |s| s.red())
                                        .to_string(),
                                    format!("FAILED [{}]", cat),
                                )
                            }
                        },
                        ProofStatus::TimedOut { .. } => (
                            "TIME"
                                .if_supports_color(Stderr, |s| s.yellow())
                                .to_string(),
                            "TIMED OUT".to_string(),
                        ),
                        ProofStatus::Unknown => (
                            "????"
                                .if_supports_color(Stderr, |s| s.yellow())
                                .to_string(),
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
                                println!(
                                    "       {}",
                                    line.if_supports_color(Stderr, |s| s.dimmed())
                                );
                            }
                            let total_lines = reason.lines().count();
                            if total_lines > 10 {
                                println!(
                                    "       {} more lines in logs...",
                                    total_lines - 10
                                );
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
                    let stderr_for_display =
                        sanitize_stderr_for_display(&summary.raw_output.stderr);
                    if !stderr_for_display.trim().is_empty() {
                        eprintln!("\n{}", stderr_for_display);
                    }
                    eprintln!("Logs available at {}/logs/", out_dir.display());
                    eprintln!(
                        "To reproduce: cd {} && lake build",
                        out_dir.display(),
                    );
                }
            }

            // Apply artifact retention policy
            let verification_succeeded =
                summary.failed == 0 && summary.timed_out == 0 && summary.unknown == 0;
            if !verify::should_retain_artifacts(artifact_policy, verification_succeeded) {
                if let Err(e) = verify::clear_generated_workspace(&out_dir) {
                    eprintln!(
                        "Warning: failed to clean up {}: {}",
                        out_dir.display(),
                        e
                    );
                }
            }

            if !verification_succeeded {
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
        }

        // When tests were skipped and --allow-skips was NOT given, exit non-zero.
        if !manifest.skipped.is_empty() && !allow_skips {
            return Err(vec![aiken_project::error::Error::StandardIo(
                std::io::Error::new(
                    std::io::ErrorKind::Other,
                    format!(
                        "{} unsupported test(s) were skipped. Use --allow-skips to treat skips as success.",
                        manifest.skipped.len()
                    ),
                ),
            )]);
        }

        Ok(())
    })
    .map_err(|_| process::exit(1))
}

#[cfg(test)]
mod tests {
    use super::*;

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
}
