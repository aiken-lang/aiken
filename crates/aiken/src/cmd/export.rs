use std::path::PathBuf;

use aiken_project::{options::Options, watch::with_project};

#[derive(clap::Args)]
pub struct Args {
    /// Path to project
    directory: Option<PathBuf>,

    /// Name of the function's module within the project
    #[clap(short, long)]
    module: String,

    /// Name of the function within the module
    #[clap(short, long)]
    name: String,
}

pub fn exec(
    Args {
        directory,
        module,
        name,
    }: Args,
) -> miette::Result<()> {
    with_project(directory.as_deref(), false, |p| {
        p.compile(Options::default())?;

        let export = p.export(&module, &name)?;

        let json = serde_json::to_string_pretty(&export).unwrap();

        println!("{}", json);

        Ok(())
    })
    .map_err(|_| std::process::exit(1))
}
