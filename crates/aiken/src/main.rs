use aiken::cmd::{build, check, deps, docs, fmt, lsp, new, tx, uplc};
use clap::Parser;

/// Aiken: a smart-contract language and toolchain for Cardano
#[derive(Parser)]
#[clap(version, about, long_about = None)]
#[clap(propagate_version = true)]
#[clap(setting(clap::AppSettings::DeriveDisplayOrder))]
pub enum Cmd {
    New(new::Args),
    Fmt(fmt::Args),
    Build(build::Args),
    Check(check::Args),
    Docs(docs::Args),

    #[clap(subcommand)]
    Deps(deps::Cmd),

    #[clap(subcommand)]
    Tx(tx::Cmd),

    #[clap(subcommand)]
    Uplc(uplc::Cmd),

    #[clap(hide = true)]
    Lsp(lsp::Args),
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
        Cmd::Fmt(args) => fmt::exec(args),
        Cmd::Build(args) => build::exec(args),
        Cmd::Docs(args) => docs::exec(args),
        Cmd::Deps(args) => deps::exec(args),
        Cmd::Check(args) => check::exec(args),
        Cmd::Lsp(args) => lsp::exec(args),
        Cmd::Tx(sub_cmd) => tx::exec(sub_cmd),
        Cmd::Uplc(sub_cmd) => uplc::exec(sub_cmd),
    }
}
