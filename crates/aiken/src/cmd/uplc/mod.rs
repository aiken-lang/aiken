mod eval;
mod flat;
mod fmt;
mod unflat;

use clap::Subcommand;

/// Commands for working with untyped Plutus-core
#[derive(Subcommand)]
#[clap(setting(clap::AppSettings::DeriveDisplayOrder))]
pub enum Cmd {
    Fmt(fmt::Args),
    Eval(eval::Args),
    Flat(flat::Args),
    Unflat(unflat::Args),
}

pub fn exec(cmd: Cmd) -> miette::Result<()> {
    match cmd {
        Cmd::Fmt(args) => fmt::exec(args),
        Cmd::Eval(args) => eval::exec(args),
        Cmd::Flat(args) => flat::exec(args),
        Cmd::Unflat(args) => unflat::exec(args),
    }
}
