use std::path::PathBuf;

use clap::{Parser, Subcommand};

/// Cardano smart contract toolchain
#[derive(Parser)]
#[clap(version, about, long_about = None)]
#[clap(propagate_version = true)]
pub enum Args {
    /// A subcommand for working with Untyped Plutus Core
    #[clap(subcommand)]
    Uplc(UplcCommand),
}

/// Commands for working with Untyped Plutus Core
#[derive(Subcommand)]
pub enum UplcCommand {
    /// Encode textual Untyped Plutus Core to flat bytes
    Flat {
        input: PathBuf,
        #[clap(short, long)]
        print: bool,
        #[clap(short, long)]
        out: Option<String>,
    },
    /// Decode flat bytes to textual Untyped Plutus Core
    Unflat {
        input: PathBuf,
        #[clap(short, long)]
        print: bool,
        #[clap(short, long)]
        out: Option<String>,
    },
    /// Format an Untyped Plutus Core program
    Fmt {
        input: PathBuf,
        #[clap(short, long)]
        print: bool,
    },
    /// Evaluate an Untyped Plutus Core program
    Eval {
        script: PathBuf,
        #[clap(short, long)]
        flat: bool,
        /// Arguments to pass to the uplc program
        args: Vec<String>,
    },
}

impl Default for Args {
    fn default() -> Self {
        Self::parse()
    }
}
