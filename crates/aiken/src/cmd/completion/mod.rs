pub mod shell;

use clap::Subcommand;
use clap_complete::Shell;

/// Get completion scripts for various shells
#[derive(Subcommand)]
pub enum Cmd {
    Bash(shell::Args),
    Zsh(shell::Args),
    Fish(shell::Args),
}

pub fn exec(cmd: Cmd) -> miette::Result<()> {
    match cmd {
        Cmd::Bash(args) => shell::exec(args, Shell::Bash),
        Cmd::Zsh(args) => shell::exec(args, Shell::Zsh),
        Cmd::Fish(args) => shell::exec(args, Shell::Fish),
    }
}
