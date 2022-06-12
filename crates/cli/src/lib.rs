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
    Flat {
        input: PathBuf,
        #[clap(short, long)]
        print: bool,
    },
    Unflat {
        input: PathBuf,
        #[clap(short, long)]
        print: bool,
    },
}

impl Default for Cli {
    fn default() -> Self {
        Self::parse()
    }
}
