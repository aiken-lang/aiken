use clap::Parser;
use std::env;

pub mod blueprint;
pub mod build;
pub mod check;
pub mod completion;
pub mod docs;
pub mod fmt;
pub mod lsp;
pub mod new;
pub mod packages;
pub mod tx;
pub mod uplc;

use crate::built_info;

/// Aiken: a smart-contract language and toolchain for Cardano
#[derive(Parser)]
#[clap(version = version(), about, long_about = None)]
#[clap(propagate_version = true)]
pub enum Cmd {
    New(new::Args),
    Fmt(fmt::Args),
    Build(build::Args),
    Address(blueprint::address::Args),
    Check(check::Args),
    Docs(docs::Args),
    Add(packages::add::Args),

    #[clap(subcommand)]
    Blueprint(blueprint::Cmd),

    #[clap(subcommand)]
    Packages(packages::Cmd),

    #[clap(subcommand)]
    Tx(tx::Cmd),

    #[clap(subcommand)]
    Uplc(uplc::Cmd),

    #[clap(subcommand)]
    Completion(completion::Cmd),

    #[clap(hide = true)]
    Lsp(lsp::Args),
}

impl Default for Cmd {
    fn default() -> Self {
        Self::parse()
    }
}

fn version() -> String {
    format!(
        "v{} {}",
        built_info::PKG_VERSION,
        built_info::GIT_COMMIT_HASH_SHORT
    )
}
