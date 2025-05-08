pub mod simulate;

use clap::Subcommand;
use std::process;

/// Commands for working with transactions
#[derive(Subcommand)]
#[clap(disable_version_flag(true))]
pub enum Cmd {
    Simulate(simulate::Args),
}

pub fn exec(cmd: Cmd) -> miette::Result<()> {
    match cmd {
        Cmd::Simulate(args) => simulate::exec(args),
    }
    .map_err(|_| process::exit(1))
}
