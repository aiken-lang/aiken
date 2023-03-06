pub mod address;
pub mod apply;
pub mod convert;

use clap::Subcommand;

/// Commands for working with Plutus blueprints
#[derive(Subcommand)]
pub enum Cmd {
    Address(address::Args),
    Apply(apply::Args),
    Convert(convert::Args),
}

pub fn exec(cmd: Cmd) -> miette::Result<()> {
    match cmd {
        Cmd::Address(args) => address::exec(args),
        Cmd::Apply(args) => apply::exec(args),
        Cmd::Convert(args) => convert::exec(args),
    }
}
