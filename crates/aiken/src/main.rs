use aiken::{
    built_info,
    cmd::{
        blueprint::{self, address},
        build, check, docs, fmt, lsp, new,
        packages::{self, add},
        tx, uplc,
    },
};
use clap::Parser;

/// Aiken: a smart-contract language and toolchain for Cardano
#[derive(Parser)]
#[clap(version = version(), about, long_about = None)]
#[clap(propagate_version = true)]
pub enum Cmd {
    New(new::Args),
    Fmt(fmt::Args),
    Build(build::Args),
    Address(address::Args),
    Check(check::Args),
    Docs(docs::Args),
    Add(add::Args),

    #[clap(subcommand)]
    Blueprint(blueprint::Cmd),

    #[clap(subcommand)]
    Packages(packages::Cmd),

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
        Cmd::Address(args) => address::exec(args),
        Cmd::Check(args) => check::exec(args),
        Cmd::Docs(args) => docs::exec(args),
        Cmd::Add(args) => add::exec(args),
        Cmd::Blueprint(args) => blueprint::exec(args),
        Cmd::Packages(args) => packages::exec(args),
        Cmd::Lsp(args) => lsp::exec(args),
        Cmd::Tx(sub_cmd) => tx::exec(sub_cmd),
        Cmd::Uplc(sub_cmd) => uplc::exec(sub_cmd),
    }
}

fn version() -> String {
    format!(
        "v{} {}",
        built_info::PKG_VERSION,
        built_info::GIT_COMMIT_HASH_SHORT.unwrap_or("unknown")
    )
}
