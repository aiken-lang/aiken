pub mod generate;

/// Produce UAL assurance bundles without invoking a verification consumer
#[derive(clap::Subcommand)]
#[clap(disable_version_flag(true))]
pub enum Cmd {
    Generate(generate::Args),
}

pub fn exec(cmd: Cmd) -> miette::Result<()> {
    match cmd {
        Cmd::Generate(args) => generate::exec(args),
    }
}
