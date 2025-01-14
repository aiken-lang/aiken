use aiken_project::config;
use clap::Parser;

pub mod benchmark;
pub mod blueprint;
pub mod build;
pub mod check;
// only windows
#[cfg(not(target_os = "windows"))]
pub mod completion;
pub mod docs;
pub mod export;
pub mod fmt;
pub mod lsp;
pub mod new;
pub mod packages;
pub mod tx;
pub mod uplc;

/// Aiken: a smart-contract language and toolchain for Cardano
#[derive(Parser)]
#[clap(version = config::compiler_version(true), about, long_about = None)]
#[clap(propagate_version = true)]
pub enum Cmd {
    New(new::Args),
    Fmt(fmt::Args),

    Export(export::Args),

    #[clap(visible_alias("b"))]
    Build(build::Args),
    Address(blueprint::address::Args),

    #[clap(visible_alias("c"))]
    Check(check::Args),
    Docs(docs::Args),
    Add(packages::add::Args),

    Bench(benchmark::Args),

    #[clap(subcommand)]
    Blueprint(blueprint::Cmd),

    #[clap(subcommand)]
    Packages(packages::Cmd),

    #[clap(subcommand)]
    Tx(tx::Cmd),

    #[clap(subcommand)]
    Uplc(uplc::Cmd),

    #[cfg(not(target_os = "windows"))]
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
