use crate::with_project;
use aiken_lang::VALIDATOR_NAMES;
use aiken_project::error::Error;
use miette::IntoDiagnostic;
use std::{fs, path::PathBuf};
use uplc::{
    ast::{DeBruijn, Term},
    parser,
};

#[derive(clap::Args)]
#[clap(setting(clap::AppSettings::DeriveDisplayOrder))]
/// Apply a parameter to a parameterized validator.
pub struct Args {
    /// Path to project
    directory: Option<PathBuf>,

    /// Name of the validator's module within the project. Optional if there's only one validator.
    #[clap(short, long)]
    validator: Option<String>,

    /// Purpose of the validator within the module. Optional if there's only one validator.
    #[clap(short, long, possible_values=&VALIDATOR_NAMES)]
    purpose: Option<String>,

    /// The parameter, using high-level UPLC-syntax
    parameter: String,
}

pub fn exec(
    Args {
        directory,
        validator,
        purpose,
        parameter,
    }: Args,
) -> miette::Result<()> {
    let term: Term<DeBruijn> = parser::term(&parameter)
        .into_diagnostic()?
        .try_into()
        .into_diagnostic()?;

    with_project(directory, |p| {
        let blueprint = p.apply_parameter(
            validator.as_ref(),
            purpose
                .as_ref()
                .map(|p| p.clone().try_into().unwrap())
                .as_ref(),
            &term,
        )?;

        let json = serde_json::to_string_pretty(&blueprint).unwrap();
        fs::write(p.blueprint_path(), json).map_err(|error| Error::FileIo {
            error,
            path: p.blueprint_path(),
        })
    })
}
