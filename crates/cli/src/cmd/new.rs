use miette::IntoDiagnostic;
use std::fs;
use std::path::PathBuf;

#[derive(clap::Args)]
/// Create a new Aiken project
pub struct Args {
    /// Project name
    name: PathBuf,
}

pub fn exec(Args { name }: Args) -> miette::Result<()> {
    if !name.exists() {
        fs::create_dir_all(name.join("lib")).into_diagnostic()?;
        fs::create_dir_all(name.join("policies")).into_diagnostic()?;
        fs::create_dir_all(name.join("scripts")).into_diagnostic()?;
    }

    return Ok(());
}
