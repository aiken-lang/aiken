use crate::with_project;
use aiken_lang::ast::Tracing;
use std::path::PathBuf;

/// Compute a validator's address.
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
    with_project(directory, |p| {
        if rebuild {
            p.build(false, Tracing::NoTraces)?;
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
}
