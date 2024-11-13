use aiken_project::watch::with_project;
use std::path::PathBuf;

/// Compute a minting scripts Policy ID
#[derive(clap::Args)]
pub struct Args {
    /// Path to project
    directory: Option<PathBuf>,

    /// Optional path to the blueprint file to be used as input.
    ///
    /// [default: plutus.json]
    #[clap(
        short,
        long = "in",
        value_parser,
        value_name = "FILEPATH",
        verbatim_doc_comment
    )]
    input: Option<PathBuf>,

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
        input,
        module,
        validator,
    }: Args,
) -> miette::Result<()> {
    with_project(directory.as_deref(), false, false, |p| {
        let policy = p.policy(
            module.as_deref(),
            validator.as_deref(),
            p.blueprint_path(input.as_deref()).as_path(),
        )?;

        println!("{}", policy);

        Ok(())
    })
    .map_err(|_| std::process::exit(1))
}
