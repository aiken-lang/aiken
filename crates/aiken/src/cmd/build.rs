use aiken_project::watch::{self, watch_project, with_project};
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

    /// Do not remove traces when generating code
    #[clap(short, long)]
    keep_traces: bool,
}

pub fn exec(
    Args {
        directory,
        deny,
        watch,
        uplc,
        keep_traces,
    }: Args,
) -> miette::Result<()> {
    let result = if watch {
        watch_project(directory.as_deref(), watch::default_filter, 500, |p| {
            p.build(uplc, keep_traces.into())
        })
    } else {
        with_project(directory.as_deref(), deny, |p| {
            p.build(uplc, keep_traces.into())
        })
    };

    result.map_err(|_| process::exit(1))
}
