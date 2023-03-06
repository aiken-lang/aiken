pub mod add;
pub mod clear_cache;
pub mod upgrade;

use clap::Subcommand;

/// Managing project dependencies
#[derive(Subcommand)]
pub enum Cmd {
    /// Add a new package dependency
    Add(add::Args),

    /// Change the version of an installed dependency
    Upgrade(upgrade::Args),

    /// Clear the system-wide dependencies cache
    ClearCache,
}

pub fn exec(cmd: Cmd) -> miette::Result<()> {
    match cmd {
        Cmd::Add(args) => add::exec(args),
        Cmd::ClearCache => clear_cache::exec(),
        Cmd::Upgrade(args) => upgrade::exec(args),
    }
}
