use std::path::PathBuf;

/// Convert a blueprint into other formats.
#[derive(clap::Args)]
#[clap(setting(clap::AppSettings::DeriveDisplayOrder))]
pub struct Args {
    /// Path to project
    directory: Option<PathBuf>,

    /// Name of the validator's module within the project. Optional if there's only one validator.
    #[clap(short, long)]
    module: Option<String>,

    /// Name of the validator within the module. Optional if there's only one validator.
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
    Ok(())
}
