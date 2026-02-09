use aiken_lang::ast::Tracing;
use aiken_project::{
    export::{ExportedBounds, FuzzerOutputType},
    options::Options,
    verify::{self, ProofStatus, VerifyConfig},
    watch::with_project,
};
use owo_colors::{OwoColorize, Stream::Stderr};
use std::{path::PathBuf, process};

#[derive(clap::Args)]
#[clap(disable_version_flag(true))]
#[command(
    verbatim_doc_comment,
    about = color_print::cstr!(r#"
Formally verify property tests using the Blaster theorem prover.
"#),
    after_long_help = color_print::cstr!(r#"<bold><underline>Examples:</underline></bold>

    <bold>aiken verify</bold>
        Verify all property tests in the current project

    <bold>aiken verify -m "my_module.test_"</bold>
        Verify only property tests matching the pattern

    <bold>aiken verify --generate-only</bold>
        Generate Lean artifacts without running proofs

You are seeing the extended help. Use `-h` instead of `--help` for a more compact view.
"#
))]
pub struct Args {
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

    /// Keep generated Lean artifacts after verification
    #[clap(long)]
    keep_artifacts: bool,

    /// Timeout in seconds for Blaster per theorem
    #[clap(long, default_value = "300")]
    timeout: u64,

    /// CEK machine step budget
    #[clap(long, default_value = "20000")]
    cek_budget: u64,

    /// Output results as JSON
    #[clap(long)]
    json: bool,
}

pub fn exec(
    Args {
        directory,
        deny,
        silent,
        match_tests,
        exact_match,
        generate_only,
        out_dir,
        keep_artifacts,
        timeout,
        cek_budget,
        json,
    }: Args,
) -> miette::Result<()> {
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

        // When --generate-only, reject tests with Unknown bounds where bounds
        // are actually required (e.g. Int). Types like Bool don't need bounds.
        if generate_only {
            let unknown: Vec<&str> = property_tests
                .iter()
                .filter(|t| {
                    matches!(t.extracted_bounds, ExportedBounds::Unknown)
                        && !matches!(t.fuzzer_output_type, FuzzerOutputType::Bool)
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
                             `fuzz.int_between`) so the prover knows the input domain."
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
        };

        let manifest = verify::generate_lean_workspace(property_tests, &config)
            .map_err(|e| {
                vec![aiken_project::error::Error::StandardIo(
                    std::io::Error::new(std::io::ErrorKind::Other, e.to_string()),
                )]
            })?;

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
            // Warn early if PlutusCore is missing -- users often forget this.
            if let Err(e) = verify::check_plutus_core(&out_dir) {
                eprintln!(
                    "{} {}",
                    "Warning:"
                        .if_supports_color(Stderr, |s| s.yellow())
                        .if_supports_color(Stderr, |s| s.bold()),
                    e,
                );
            }

            println!("Running proofs via lake build...");

            let start = std::time::Instant::now();
            let result = verify::run_proofs(&out_dir, timeout).map_err(|e| {
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
                            "PROVED",
                        ),
                        ProofStatus::Failed { .. } => (
                            "FAIL"
                                .if_supports_color(Stderr, |s| s.red())
                                .to_string(),
                            "FAILED",
                        ),
                        ProofStatus::Unknown => (
                            "????"
                                .if_supports_color(Stderr, |s| s.yellow())
                                .to_string(),
                            "UNKNOWN",
                        ),
                    };
                    println!(
                        "  {} {} [{}] - {}",
                        icon, t.test_name, t.theorem_name, label
                    );
                }

                let elapsed_str = if elapsed.as_secs() > 0 {
                    format!("{}s", elapsed.as_secs())
                } else {
                    format!("{}ms", elapsed.as_millis())
                };

                println!(
                    "\nResults: {} proved, {} failed, {} unknown out of {} theorems in {}",
                    summary.proved, summary.failed, summary.unknown, summary.total, elapsed_str,
                );

                if summary.failed > 0 || summary.unknown > 0 {
                    if !summary.raw_output.stderr.is_empty() {
                        eprintln!("\n{}", summary.raw_output.stderr);
                    }
                    eprintln!("Logs available at {}/logs/", out_dir.display());
                    eprintln!(
                        "To reproduce: cd {} && lake build",
                        out_dir.display(),
                    );
                }
            }

            // Clean up generated workspace unless --keep-artifacts was passed.
            if !keep_artifacts {
                if let Err(e) = std::fs::remove_dir_all(&out_dir) {
                    eprintln!(
                        "Warning: failed to clean up {}: {}",
                        out_dir.display(),
                        e
                    );
                }
            }

            if summary.failed > 0 || summary.unknown > 0 {
                return Err(vec![aiken_project::error::Error::StandardIo(
                    std::io::Error::new(
                        std::io::ErrorKind::Other,
                        format!(
                            "Proof verification incomplete: {} failed, {} unknown out of {} theorems",
                            summary.failed, summary.unknown, summary.total
                        ),
                    ),
                )]);
            }
        }

        Ok(())
    })
    .map_err(|_| process::exit(1))
}
