mod decode;
mod encode;
mod eval;
mod fmt;
mod shrink;

use clap::{Subcommand, ValueEnum};

#[derive(Copy, Clone, ValueEnum)]
pub(super) enum Format {
    Name,
    NamedDebruijn,
    Debruijn,
}

/// Commands for working with untyped Plutus-core
#[derive(Subcommand)]
pub enum Cmd {
    Fmt(fmt::Args),
    Eval(eval::Args),
    #[clap(alias = "flat")]
    Encode(encode::Args),
    #[clap(alias = "unflat")]
    Decode(decode::Args),
    #[clap(alias = "optimize")]
    Shrink(shrink::Args),
}

pub fn exec(cmd: Cmd) -> miette::Result<()> {
    match cmd {
        Cmd::Fmt(args) => fmt::exec(args),
        Cmd::Eval(args) => eval::exec(args),
        Cmd::Encode(args) => encode::exec(args),
        Cmd::Decode(args) => decode::exec(args),
        Cmd::Shrink(args) => shrink::exec(args),
    }
}
