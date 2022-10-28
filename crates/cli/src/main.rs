use aiken::cmd::{build, check, new, tx, uplc};
use clap::Parser;

/// Aiken: a smart-contract language and toolchain for Cardano
#[derive(Parser)]
#[clap(version, about, long_about = None)]
#[clap(propagate_version = true)]
#[clap(setting(clap::AppSettings::DeriveDisplayOrder))]
pub enum Cmd {
    New(new::Args),
    Build(build::Args),
    Check(check::Args),

    #[clap(subcommand)]
    Tx(tx::Cmd),

    #[clap(subcommand)]
    Uplc(uplc::Cmd),
}

impl Default for Cmd {
    fn default() -> Self {
        Self::parse()
    }
}

fn main() -> miette::Result<()> {
    miette::set_panic_hook();
    match Cmd::default() {
        Cmd::New(args) => new::exec(args),
        Cmd::Build(args) => build::exec(args),
        Cmd::Check(args) => check::exec(args),
        Cmd::Tx(sub_cmd) => tx::exec(sub_cmd),
        Cmd::Uplc(sub_cmd) => uplc::exec(sub_cmd),
    }
}
