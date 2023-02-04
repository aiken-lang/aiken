pub mod address;
pub mod apply;

use clap::Subcommand;

/// Commands for working with Plutus blueprints
#[derive(Subcommand)]
#[clap(setting(clap::AppSettings::DeriveDisplayOrder))]
pub enum Cmd {
    Address(address::Args),
    Apply(apply::Args),
}

pub fn exec(cmd: Cmd) -> miette::Result<()> {
    match cmd {
        Cmd::Address(args) => address::exec(args),
        Cmd::Apply(args) => apply::exec(args),
    }
}
