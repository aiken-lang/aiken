#[derive(clap::Args)]
pub struct Args {
    /// Name of the validator's module within the project. Optional if there's only one validator
    #[clap(short, long)]
    module: String,

    /// Name of the validator within the module. Optional if there's only one validator
    #[clap(short, long)]
    name: String,
}

pub fn exec(args: Args) -> miette::Result<()> {
    Ok(())
}
