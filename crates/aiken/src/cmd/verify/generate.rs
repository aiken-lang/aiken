use crate::cmd::build::{trace_filter_parser, trace_level_parser};
use aiken_lang::ast::{TraceLevel, Tracing};
use aiken_project::{
    Project, assurance::generate::Environment, config::WorkspaceConfig, telemetry::EventTarget,
    watch::workspace_root,
};
use std::path::PathBuf;

/// Compile and validate a UAL bundle, writing it under assurance/ unless dry-running
#[derive(clap::Args)]
#[clap(disable_version_flag(true))]
#[clap(
    after_long_help = "Writes assurance/plutus.json, assurance/assurance.json and checking-<id>.json; removes stale checking files. Never writes root plutus.json or invokes Lean/a solver.\n\n--no-environment validates an unbound draft and writes nothing. --dry-run with --environment also validates the manifest and final document bindings. Dry runs use installed dependencies only; run aiken build first if dependencies are missing or stale.\n\nEnvironment paths are relative to the current working directory. Bundle references are relative to assurance/. Preamble authors default to the package owner; its creation date is the aiken.toml modification date (UTC), keeping unchanged projects byte-identical across reruns."
)]
pub struct Args {
    /// Path to project
    directory: Option<PathBuf>,

    /// Consumer environment manifest; read and hash its original bytes
    #[clap(
        long,
        value_name = "PATH",
        required_unless_present = "no_environment",
        conflicts_with = "no_environment"
    )]
    environment: Option<PathBuf>,

    /// Run the producer pass without an environment binding; write nothing
    #[clap(long)]
    no_environment: bool,

    /// Validate without writing bundles, dependency locks, or cache files
    #[clap(long)]
    dry_run: bool,

    /// Aiken configuration environment to compile against (not the consumer manifest)
    #[clap(long)]
    env: Option<String>,

    /// Filter traces: user-defined, compiler-generated, or all [default: all]
    #[clap(short = 'f', long, value_parser = trace_filter_parser(), default_missing_value = "all", alias = "filter_traces")]
    trace_filter: Option<fn(TraceLevel) -> Tracing>,

    /// Trace verbosity: silent, compact, or verbose
    #[clap(short, long, value_parser = trace_level_parser(), default_value_t = TraceLevel::Silent)]
    trace_level: TraceLevel,
}

pub fn exec(args: Args) -> miette::Result<()> {
    let environment = args
        .environment
        .as_deref()
        .map(Environment::read)
        .transpose()?;
    let root = workspace_root(args.directory.as_deref())?;
    let roots = match WorkspaceConfig::load(&root) {
        Ok(workspace) => workspace.members,
        Err(_) => vec![root],
    };
    let tracing = args.trace_filter.unwrap_or(Tracing::All)(args.trace_level);
    let dry_run = args.dry_run || args.no_environment;
    for root in roots {
        let mut project = Project::new(root.clone(), EventTarget::default()).map_err(|error| {
            error.report();
            aiken_project::watch::ExitFailure::into_report()
        })?;
        let result =
            project.generate_assurance(environment.as_ref(), dry_run, tracing, args.env.clone());
        for warning in project.warnings() {
            warning.report();
        }
        let count = result?;
        if dry_run {
            eprintln!(
                "Validated {count} assurance properties in {} (dry run; no files written{}).",
                root.display(),
                if environment.is_none() {
                    "; environment unbound"
                } else {
                    ""
                },
            );
        } else {
            eprintln!(
                "Generated {count} checking contexts in {}.",
                root.join("assurance").display()
            );
        }
    }
    Ok(())
}
