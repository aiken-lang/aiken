use std::path::PathBuf;

#[derive(clap::Args)]
/// Build an Aiken project
pub struct Args {
    /// Path to project
    #[clap(short, long)]
    directory: Option<PathBuf>,

    /// Also dump textual uplc
    #[clap(short, long)]
    uplc: bool,
}

pub fn exec(Args { directory, uplc }: Args) -> miette::Result<()> {
    crate::with_project(directory, |p| p.build(uplc))
}
