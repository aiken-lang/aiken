use std::path::PathBuf;

use clap::{Parser, Subcommand};

/// Cardano smart contract toolchain
#[derive(Parser)]
#[clap(version, about, long_about = None)]
#[clap(propagate_version = true)]
pub enum Args {
    /// A subcommand for working with transactions
    #[clap(subcommand)]
    Tx(TxCommand),
    /// A subcommand for working with Untyped Plutus Core
    #[clap(subcommand)]
    Uplc(UplcCommand),
}

/// Commands for working with transactions
#[derive(Subcommand)]
pub enum TxCommand {
    /// Simulate a transaction by evaluating it's script
    Simulate {
        /// A file containing cbor hex for a transaction
        input: PathBuf,

        /// Toggle whether input is raw cbor or a hex string
        #[clap(short, long)]
        cbor: bool,

        /// A file containing cbor hex for the raw inputs
        raw_inputs: PathBuf,

        /// A file containing cbor hex for the raw outputs
        raw_outputs: PathBuf,

        /// Time between each slot
        #[clap(short, long, default_value_t = 1000)]
        slot_length: u64,

        /// Time of shelley hardfork
        #[clap(long, default_value_t = 1596059091000)]
        zero_time: u64,

        /// Slot number at the start of the shelley hardfork
        #[clap(long, default_value_t = 4492800)]
        zero_slot: u64,
    },
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
        #[clap(short, long)]
        cbor_hex: bool,
    },
    /// Decode flat bytes to textual Untyped Plutus Core
    Unflat {
        input: PathBuf,
        #[clap(short, long)]
        print: bool,
        #[clap(short, long)]
        out: Option<String>,
        #[clap(short, long)]
        cbor_hex: bool,
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
