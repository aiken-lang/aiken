pub mod completion;

use clap::Subcommand;
use clap_complete::Shell;

/// Commands for working with transactions
#[derive(Subcommand)]
pub enum Cmd {
    Bash(completion::Args),
    Zsh(completion::Args),
    Fish(completion::Args),
}

pub fn exec(cmd: Cmd) -> miette::Result<()> {
    match cmd {
        Cmd::Bash(args) => completion::exec(args,Shell::Bash),
        Cmd::Zsh(args) => completion::exec(args,Shell::Zsh),
        Cmd::Fish(args) => completion::exec(args,Shell::Fish),
    }
}


