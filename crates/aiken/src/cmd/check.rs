use std::path::PathBuf;

use aiken_project::watch;
use owo_colors::{OwoColorize, Stream::Stderr};

use crate::Terminal;

#[derive(clap::Args)]
/// Type-check an Aiken project
pub struct Args {
    /// Path to project
    directory: Option<PathBuf>,

    /// Deny warnings; warnings will be treated as errors
    #[clap(short = 'D', long)]
    deny: bool,

    /// Skip tests; run only the type-checker
    #[clap(short, long)]
    skip_tests: bool,

    /// When enabled, also pretty-print test UPLC on failure
    #[clap(long)]
    debug: bool,

    /// When enabled, re-run the command on file changes instead of exiting
    #[clap(long)]
    watch: bool,

    /// When enabled, clear the screen before running
    #[clap(long)]
    clear: bool,

    /// Only run tests if they match any of these strings.
    /// You can match a module with `-m aiken/list` or `-m list`.
    /// You can match a test with `-m "aiken/list.{map}"` or `-m "aiken/option.{flatten_1}"`
    #[clap(short, long)]
    match_tests: Option<Vec<String>>,

    /// This is meant to be used with `--match-tests`.
    /// It forces test names to match exactly
    #[clap(short, long)]
    exact_match: bool,

    /// Remove traces when generating code (including tests)
    #[clap(long)]
    no_traces: bool,
}

pub fn exec(
    Args {
        directory,
        deny,
        skip_tests,
        debug,
        match_tests,
        exact_match,
        no_traces,
        watch,
        clear,
        ..
    }: Args,
) -> miette::Result<()> {
    if watch {
        watch::watch_project(directory, Terminal, watch::default_filter, 500, |p| {
            if clear {
                println!("{esc}c", esc = 27 as char);
            }
            let build_result = p.check(
                skip_tests,
                match_tests.clone(),
                debug,
                exact_match,
                (!no_traces).into(),
            );

            let warnings = p.warnings();

            let warning_count = warnings.len();

            for warning in &warnings {
                warning.report()
            }

            let plural = if warning_count == 1 { "" } else { "s" };

            if let Err(errs) = build_result {
                for err in &errs {
                    err.report()
                }

                eprintln!(
                    "\n{}",
                    "Summary"
                        .if_supports_color(Stderr, |s| s.purple())
                        .if_supports_color(Stderr, |s| s.bold())
                );

                let warning_text = format!("{warning_count} warning{plural}");

                let plural = if errs.len() == 1 { "" } else { "s" };

                let error_text = format!("{} error{}", errs.len(), plural);

                let full_summary = format!(
                    "    {}, {}",
                    error_text.if_supports_color(Stderr, |s| s.red()),
                    warning_text.if_supports_color(Stderr, |s| s.yellow())
                );

                eprintln!("{full_summary}");
            } else {
                eprintln!(
                    "\n{}",
                    "Summary"
                        .if_supports_color(Stderr, |s| s.purple())
                        .if_supports_color(Stderr, |s| s.bold())
                );

                let warning_text = format!("{warning_count} warning{plural}");

                eprintln!(
                    "    0 errors, {}",
                    warning_text.if_supports_color(Stderr, |s| s.yellow()),
                );
            }
            Ok(())
        })
    } else {
        crate::with_project(directory, deny, |p| {
            p.check(
                skip_tests,
                match_tests.clone(),
                debug,
                exact_match,
                (!no_traces).into(),
            )
        })
    }
}
