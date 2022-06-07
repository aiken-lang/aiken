use std::path::PathBuf;

use clap::{Parser, Subcommand};

#[derive(Parser)]
#[clap(author, version, about, long_about = None)]
#[clap(propagate_version = true)]
pub enum Cli {
    #[clap(subcommand)]
    Uplc(UplcCommand),
}

#[derive(Subcommand)]
pub enum UplcCommand {
    Flat { input: PathBuf },
    Unflat { input: PathBuf },
}

impl Default for Cli {
    fn default() -> Self {
        Self::parse()
    }
}
