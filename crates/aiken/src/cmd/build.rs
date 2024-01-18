use aiken_lang::ast::{TraceLevel, Tracing};
use aiken_project::watch::{self, watch_project, with_project};
use clap::builder::MapValueParser;
use clap::builder::{PossibleValuesParser, TypedValueParser};
use std::{path::PathBuf, process};

#[derive(clap::Args)]
/// Build an Aiken project
pub struct Args {
    /// Path to project
    directory: Option<PathBuf>,

    /// Deny warnings; warnings will be treated as errors
    #[clap(short = 'D', long)]
    deny: bool,

    /// When enabled, re-run the command on file changes instead of exiting
    #[clap(short, long)]
    watch: bool,

    /// Also dump textual uplc
    #[clap(short, long)]
    uplc: bool,

    /// Do not remove traces when generating code.
    #[clap(short, long, value_parser=keep_traces_parser(), default_missing_value="all")]
    keep_traces: Option<fn(TraceLevel) -> Tracing>,

    /// Choose the level of tracing
    ///   - silent: disable traces altogether
    ///   - compact: only culprit line numbers are shown on failures
    ///   - verbose: enable full verbose traces as provided by the user or the compiler
    ///
    ///
    #[clap(short, long, value_parser=trace_level_parser(), default_value_t=TraceLevel::Verbose, verbatim_doc_comment)]
    trace_level: TraceLevel,
}

pub fn exec(
    Args {
        directory,
        deny,
        watch,
        uplc,
        keep_traces,
        trace_level,
    }: Args,
) -> miette::Result<()> {
    let result = if watch {
        watch_project(directory.as_deref(), watch::default_filter, 500, |p| {
            p.build(
                uplc,
                match keep_traces {
                    Some(keep_traces) => keep_traces(trace_level),
                    None => Tracing::All(trace_level),
                },
            )
        })
    } else {
        with_project(directory.as_deref(), deny, |p| {
            p.build(
                uplc,
                match keep_traces {
                    Some(keep_traces) => keep_traces(trace_level),
                    None => Tracing::All(trace_level),
                },
            )
        })
    };

    result.map_err(|_| process::exit(1))
}

#[allow(clippy::type_complexity)]
pub fn keep_traces_parser(
) -> MapValueParser<PossibleValuesParser, fn(String) -> fn(TraceLevel) -> Tracing> {
    PossibleValuesParser::new(["user-defined", "compiler-generated", "all"]).map(
        |s: String| match s.as_str() {
            "user-defined" => Tracing::UserDefined,
            "compiler-generated" => Tracing::CompilerGenerated,
            "all" => Tracing::All,
            _ => unreachable!(),
        },
    )
}

#[allow(clippy::type_complexity)]
pub fn trace_level_parser() -> MapValueParser<PossibleValuesParser, fn(String) -> TraceLevel> {
    PossibleValuesParser::new(["silent", "compact", "verbose"]).map(|s| match s.as_str() {
        "silent" => TraceLevel::Silent,
        "compact" => TraceLevel::Compact,
        "verbose" => TraceLevel::Verbose,
        _ => unreachable!(),
    })
}
