use super::build::{keep_traces_parser, trace_level_parser};
use aiken_lang::ast::{TraceLevel, Tracing};
use aiken_project::watch::{self, watch_project, with_project};
use std::{path::PathBuf, process};

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

    /// Only run tests if they match any of these strings.
    /// You can match a module with `-m aiken/list` or `-m list`.
    /// You can match a test with `-m "aiken/list.{map}"` or `-m "aiken/option.{flatten_1}"`
    #[clap(short, long)]
    match_tests: Option<Vec<String>>,

    /// This is meant to be used with `--match-tests`.
    /// It forces test names to match exactly
    #[clap(short, long)]
    exact_match: bool,

    /// Do not remove traces when generating code
    #[clap(short, long, value_parser=keep_traces_parser(), default_missing_value="all")]
    keep_traces: Option<fn(TraceLevel) -> Tracing>,

    /// Choose the level of tracing
    ///   - silent: disable traces altogether
    ///   - compact: only culprit line numbers are shown on failures
    ///   - verbose: enable full verbose traces as provided by the user or the compiler
    /// [optional]
    #[clap(short, long, value_parser=trace_level_parser(), default_value_t=TraceLevel::Verbose, verbatim_doc_comment)]
    trace_level: TraceLevel,
}

pub fn exec(
    Args {
        directory,
        deny,
        skip_tests,
        debug,
        match_tests,
        exact_match,
        watch,
        keep_traces,
        trace_level,
    }: Args,
) -> miette::Result<()> {
    let result = if watch {
        watch_project(directory.as_deref(), watch::default_filter, 500, |p| {
            p.check(
                skip_tests,
                match_tests.clone(),
                debug,
                exact_match,
                match keep_traces {
                    Some(keep_traces) => keep_traces(trace_level),
                    None => Tracing::All(trace_level),
                },
            )
        })
    } else {
        with_project(directory.as_deref(), deny, |p| {
            p.check(
                skip_tests,
                match_tests.clone(),
                debug,
                exact_match,
                match keep_traces {
                    Some(keep_traces) => keep_traces(trace_level),
                    None => Tracing::All(trace_level),
                },
            )
        })
    };

    result.map_err(|_| process::exit(1))
}
