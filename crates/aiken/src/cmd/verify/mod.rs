pub mod check;
pub mod env;
pub mod generate;

/// Generate, check, and capture environments for UAL assurance bundles
#[derive(clap::Subcommand)]
#[clap(disable_version_flag(true))]
pub enum Cmd {
    Generate(generate::Args),
    Check(check::Args),
    Env(env::Args),
}

pub fn exec(cmd: Cmd) -> miette::Result<()> {
    match cmd {
        Cmd::Generate(args) => generate::exec(args),
        Cmd::Check(args) => check::exec(args),
        Cmd::Env(args) => env::exec(args),
    }
}
