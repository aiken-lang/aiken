use std::path::PathBuf;

#[derive(clap::Args)]
/// Build an Aiken project
pub struct Args {
    /// Path to project
    directory: Option<PathBuf>,

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
        uplc,
        keep_traces,
    }: Args,
) -> miette::Result<()> {
    crate::with_project(directory, |p| p.build(uplc, keep_traces.into()))
}
