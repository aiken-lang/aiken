use aiken_lang::ast::Tracing;
use aiken_project::watch::with_project;
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

    /// Stake address to attach, if any
    #[clap(long)]
    delegated_to: Option<String>,

    /// Force the project to be rebuilt, otherwise relies on existing artifacts (i.e. plutus.json)
    #[clap(long)]
    rebuild: bool,

    /// Output the address for mainnet (this command defaults to testnet)
    #[clap(long)]
    mainnet: bool,
}

pub fn exec(
    Args {
        directory,
        module,
        validator,
        delegated_to,
        rebuild,
        mainnet,
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

        let address = p.address(title, delegated_to.as_ref(), mainnet)?;

        println!("{}", address.to_bech32().unwrap());

        Ok(())
    })
    .map_err(|_| std::process::exit(1))
}
