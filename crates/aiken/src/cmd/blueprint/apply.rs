use crate::with_project;
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
    module: Option<String>,

    /// Name of the validator within the module. Optional if there's only one validator.
    #[clap(short, long)]
    validator: Option<String>,

    /// The parameter, using high-level UPLC-syntax
    parameter: String,
}

pub fn exec(
    Args {
        directory,
        module,
        validator,
        parameter,
    }: Args,
) -> miette::Result<()> {
    let term: Term<DeBruijn> = parser::term(&parameter)
        .into_diagnostic()?
        .try_into()
        .into_diagnostic()?;

    with_project(directory, |p| {
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

        let blueprint = p.apply_parameter(title, &term)?;

        let json = serde_json::to_string_pretty(&blueprint).unwrap();

        fs::write(p.blueprint_path(), json).map_err(|error| {
            Error::FileIo {
                error,
                path: p.blueprint_path(),
            }
            .into()
        })
    })
}
