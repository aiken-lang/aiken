use crate::cmd::build::{trace_filter_parser, trace_level_parser};
use aiken_lang::ast::{TraceLevel, Tracing};
use aiken_project::{
    assurance::check::{check_assurance, report_failure},
    config::WorkspaceConfig,
    telemetry::{EventTarget, verification_json_schema},
    watch::{ExitFailure, workspace_root},
};
use miette::IntoDiagnostic;
use std::{path::PathBuf, time::Duration};

/// Generate a UAL bundle and freshly check every property with the configured consumer
#[derive(clap::Args)]
#[clap(disable_version_flag(true))]
#[clap(
    after_long_help = "Requires a provisioned consumer workspace and a captured environment manifest. Environment paths are relative to the current working directory. Uses the same trace settings as verify generate.\n\nOnly a complete successful run writes assurance/verified-assurance.json. Every attempt removes stale evidence first. The full Lean/Blaster log is retained in assurance/verification.log. SMT verification trusts the translation and actual solver; it is not a reconstructed Lean proof, and results are relative to each declared step budget.\n\nWhen stdout is not a terminal, emits a JSON report, including trust and failure diagnostics. --show-json-schema prints that report's schema without loading a project or consumer workspace."
)]
pub struct Args {
    /// Path to project
    directory: Option<PathBuf>,

    /// Consumer environment manifest; read and hash its original bytes
    #[clap(long, value_name = "PATH", default_value = "environment.json")]
    environment: PathBuf,

    /// Wall-clock timeout for the consumer process (terminates its process group on Unix)
    #[clap(long, value_name = "SECONDS", default_value_t = 300, value_parser = clap::value_parser!(u64).range(1..))]
    timeout: u64,

    /// Aiken configuration environment to compile against (not the consumer manifest)
    #[clap(long)]
    env: Option<String>,

    /// Filter traces: user-defined, compiler-generated, or all [default: all]
    #[clap(short = 'f', long, value_parser = trace_filter_parser(), default_missing_value = "all", alias = "filter_traces")]
    trace_filter: Option<fn(TraceLevel) -> Tracing>,

    /// Trace verbosity: silent, compact, or verbose
    #[clap(short, long, value_parser = trace_level_parser(), default_value_t = TraceLevel::Silent)]
    trace_level: TraceLevel,

    /// Print the JSON report schema without loading a project or consumer workspace
    #[clap(long)]
    show_json_schema: bool,
}

pub fn exec(args: Args) -> miette::Result<()> {
    if args.show_json_schema {
        println!(
            "{}",
            serde_json::to_string_pretty(&verification_json_schema()).into_diagnostic()?
        );
        return Ok(());
    }
    let listener = EventTarget::default();
    let root = match workspace_root(args.directory.as_deref()) {
        Ok(root) => root,
        Err(error) => {
            report_failure(
                &listener,
                args.directory
                    .as_deref()
                    .unwrap_or_else(|| std::path::Path::new("."))
                    .display()
                    .to_string(),
                format!("{error:?}"),
            );
            return Err(ExitFailure::into_report());
        }
    };
    let roots = match WorkspaceConfig::load(&root) {
        Ok(workspace) => workspace.members,
        Err(_) => vec![root],
    };
    let tracing = args.trace_filter.unwrap_or(Tracing::All)(args.trace_level);
    let mut success = true;
    if roots.is_empty() {
        report_failure(
            &listener,
            "workspace".to_owned(),
            "workspace contains no projects to verify".to_owned(),
        );
        return Err(ExitFailure::into_report());
    }
    for root in roots {
        success &= check_assurance(
            &root,
            &args.environment,
            Duration::from_secs(args.timeout),
            tracing,
            args.env.clone(),
            &listener,
        );
    }
    if success {
        Ok(())
    } else {
        Err(ExitFailure::into_report())
    }
}
