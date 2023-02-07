use crate::with_project;
use aiken_lang::VALIDATOR_NAMES;
use std::path::PathBuf;

#[derive(clap::Args)]
#[clap(setting(clap::AppSettings::DeriveDisplayOrder))]
/// Compute a validator's address.
pub struct Args {
    /// Path to project
    directory: Option<PathBuf>,

    /// Name of the validator's module within the project. Optional if there's only one validator.
    #[clap(short, long)]
    validator: Option<String>,

    /// Purpose of the validator within the module. Optional if there's only one validator.
    #[clap(short, long, possible_values=&VALIDATOR_NAMES)]
    purpose: Option<String>,

    /// Stake address to attach, if any.
    #[clap(long)]
    delegated_to: Option<String>,

    /// Force the project to be rebuilt, otherwise relies on existing artifacts (i.e. plutus.json).
    #[clap(long)]
    rebuild: bool,
}

pub fn exec(
    Args {
        directory,
        validator,
        purpose,
        delegated_to,
        rebuild,
    }: Args,
) -> miette::Result<()> {
    with_project(directory, |p| {
        if rebuild {
            p.build(false)?;
        }
        let address = p.address(
            validator.as_ref(),
            purpose
                .as_ref()
                .map(|p| p.clone().try_into().unwrap())
                .as_ref(),
            delegated_to.as_ref(),
        )?;
        println!("{}", address.to_bech32().unwrap());
        Ok(())
    })
}
