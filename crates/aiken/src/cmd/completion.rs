use clap::{Subcommand, Command};
use clap_complete::{Shell, generate};

use crate::cmd::Cmd as MainCmd;

/// Generates shell completion scripts
#[derive(Subcommand)]
pub enum Cmd {
    Bash,
    Zsh,
    Fish
}

pub fn exec(sub_cmd: Cmd) -> miette::Result<()> {
    let shell = match sub_cmd {
        Cmd::Bash => Shell::Bash,
        Cmd::Zsh => Shell::Zsh,
        Cmd::Fish => Shell::Fish
    };

    let cli = Command::new("aiken").disable_version_flag(true);

    let mut main = MainCmd::augment_subcommands(cli);

    generate(shell, &mut main, "aiken".to_string(), &mut std::io::stdout());

    Ok(())
}
