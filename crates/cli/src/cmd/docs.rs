use std::path::PathBuf;

#[derive(clap::Args)]
/// Build an Aiken project
pub struct Args {
    /// Path to project
    #[clap(short, long)]
    directory: Option<PathBuf>,

    /// Output directory for the documentation
    #[clap(short = 'o', long)]
    destination: Option<PathBuf>,
}

pub fn exec(
    Args {
        directory,
        destination,
    }: Args,
) -> miette::Result<()> {
    crate::with_project(directory, |p| p.docs(destination.clone()))
}
