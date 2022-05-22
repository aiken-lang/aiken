use std::path::PathBuf;

use clap::Parser;

#[derive(Parser)]
pub struct Cli {
    pub input: PathBuf,
}

impl Default for Cli {
    fn default() -> Self {
        Self::parse()
    }
}
