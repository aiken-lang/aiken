use aiken_project::watch::with_project;
use std::path::PathBuf;

/// Compute a minting scripts Policy ID
#[derive(clap::Args)]
pub struct Args {
    /// Path to project
    directory: Option<PathBuf>,

    /// Name of the validator's module within the project. Optional if there's only one validator
    #[clap(short, long)]
    module: Option<String>,

    /// Name of the validator within the module. Optional if there's only one validator
    #[clap(short, long)]
    validator: Option<String>,
}

pub fn exec(
    Args {
        directory,
        module,
        validator,
    }: Args,
) -> miette::Result<()> {
    with_project(directory.as_deref(), false, |p| {
        let title = module.as_ref().map(|m| {
            format!(
                "{m}{}",
                validator
                    .as_ref()
                    .map(|v| format!(".{v}"))
                    .unwrap_or_default()
            )
        });

        let title = title.as_ref().or(validator.as_ref());

        let policy = p.policy(title)?;

        println!("{}", policy);

        Ok(())
    })
    .map_err(|_| std::process::exit(1))
}
