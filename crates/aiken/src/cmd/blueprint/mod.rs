pub mod address;
pub mod apply;
pub mod convert;
pub mod hash;
pub mod policy;

use clap::Subcommand;

/// Commands for working with Plutus blueprints
#[derive(Subcommand)]
pub enum Cmd {
    Address(address::Args),
    Policy(policy::Args),
    Hash(hash::Args),
    Apply(apply::Args),
    Convert(convert::Args),
}

pub fn exec(cmd: Cmd) -> miette::Result<()> {
    match cmd {
        Cmd::Address(args) => address::exec(args),
        Cmd::Policy(args) => policy::exec(args),
        Cmd::Hash(args) => hash::exec(args),
        Cmd::Apply(args) => apply::exec(args),
        Cmd::Convert(args) => convert::exec(args),
    }
}
