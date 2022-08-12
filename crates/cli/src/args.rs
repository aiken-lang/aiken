use std::path::PathBuf;

use clap::{Parser, Subcommand};

/// Cardano smart contract toolchain
#[derive(Parser)]
#[clap(version, about, long_about = None)]
#[clap(propagate_version = true)]
pub enum Args {
    /// Build an aiken project
    Build,
    /// Start a development server
    Dev,
    /// Create a new aiken project
    New {
        /// Project name
        name: PathBuf,
    },
    /// A subcommand for working with Untyped Plutus Core
    #[clap(subcommand)]
    Uplc(UplcCommand),
}

/// Commands for working with Untyped Plutus Core
#[derive(Subcommand)]
pub enum UplcCommand {
    /// Evaluate an Untyped Plutus Core program
    Eval {
        /// Handle input as flat bytes
        #[clap(short, long)]
        flat: bool,

        /// File to load and evaluate
        input: PathBuf,
    },
    /// Encode textual Untyped Plutus Core to flat bytes
    Flat {
        /// Textual Untyped Plutus Core file
        input: PathBuf,

        /// Output file name
        #[clap(short, long)]
        out: Option<String>,

        /// Print output instead of saving to file
        #[clap(short, long)]
        print: bool,
    },
    /// Format an Untyped Plutus Core program
    Fmt {
        /// Textual Untyped Plutus Core file
        input: PathBuf,

        /// Print output instead of saving to file
        #[clap(short, long)]
        print: bool,
    },
    /// Decode flat bytes to textual Untyped Plutus Core
    Unflat {
        /// Flat encoded Untyped Plutus Core file
        input: PathBuf,

        /// Output file name
        #[clap(short, long)]
        out: Option<String>,

        /// Print output instead of saving to file
        #[clap(short, long)]
        print: bool,
    },
}

impl Default for Args {
    fn default() -> Self {
        Self::parse()
    }
}
