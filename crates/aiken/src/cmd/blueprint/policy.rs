use aiken_lang::ast::Tracing;
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

    /// Force the project to be rebuilt, otherwise relies on existing artifacts (i.e. plutus.json)
    #[clap(long)]
    rebuild: bool,
}

pub fn exec(
    Args {
        directory,
        module,
        validator,
        rebuild,
    }: Args,
) -> miette::Result<()> {
    with_project(directory.as_deref(), false, |p| {
        if rebuild {
            p.build(false, Tracing::silent())?;
        }

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
