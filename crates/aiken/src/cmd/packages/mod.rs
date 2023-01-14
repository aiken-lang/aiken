pub mod add;
pub mod clear_cache;

use clap::Subcommand;

/// Managing project dependencies
#[derive(Subcommand)]
#[clap(setting(clap::AppSettings::DeriveDisplayOrder))]
pub enum Cmd {
    /// Add a new package dependency
    Add(add::Args),

    /// Clear the system-wide dependencies cache
    ClearCache,
}

pub fn exec(cmd: Cmd) -> miette::Result<()> {
    match cmd {
        Cmd::Add(args) => add::exec(args),
        Cmd::ClearCache => clear_cache::exec(),
    }
}
