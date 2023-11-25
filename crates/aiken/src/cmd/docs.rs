use aiken_project::watch::with_project;
use std::path::PathBuf;

#[derive(clap::Args)]
/// Build the documentation for an Aiken project
pub struct Args {
    /// Path to project
    directory: Option<PathBuf>,

    /// Deny warnings; warnings will be treated as errors
    #[clap(short = 'D', long)]
    deny: bool,

    /// Output directory for the documentation
    #[clap(short = 'o', long)]
    destination: Option<PathBuf>,
}

pub fn exec(
    Args {
        directory,
        deny,
        destination,
    }: Args,
) -> miette::Result<()> {
    with_project(directory.as_deref(), deny, |p| p.docs(destination.clone()))
}
