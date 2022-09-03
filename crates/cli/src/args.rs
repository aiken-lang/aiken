use std::{collections::HashMap, path::PathBuf};

use clap::{Parser, Subcommand};
use serde::{de, Deserialize};

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
        input: PathBuf,
        #[clap(short, long)]
        cbor: bool,
        resolved_inputs: PathBuf,
    },
}

#[derive(Deserialize)]
pub struct ResolvedInput {
    pub input: Input,
    pub ouput: Output,
}

#[derive(Deserialize)]
pub struct Input {
    pub tx_hash: String,
    pub index: u64,
}

#[derive(Deserialize)]
pub struct Output {
    pub address: String,
    pub value: (u64, HashMap<String, HashMap<String, u64>>),
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
        input: PathBuf,
        #[clap(short, long)]
        flat: bool,
    },
}

impl Default for Args {
    fn default() -> Self {
        Self::parse()
    }
}
