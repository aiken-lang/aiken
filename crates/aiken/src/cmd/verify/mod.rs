pub mod env;
pub mod generate;

/// Produce UAL assurance bundles and capture the consumer environment
#[derive(clap::Subcommand)]
#[clap(disable_version_flag(true))]
pub enum Cmd {
    Generate(generate::Args),
    Env(env::Args),
}

pub fn exec(cmd: Cmd) -> miette::Result<()> {
    match cmd {
        Cmd::Generate(args) => generate::exec(args),
        Cmd::Env(args) => env::exec(args),
    }
}
