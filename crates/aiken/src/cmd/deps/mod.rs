pub mod clear_cache;

use clap::Subcommand;

/// Managing project dependencies
#[derive(Subcommand)]
#[clap(setting(clap::AppSettings::DeriveDisplayOrder))]
pub enum Cmd {
    /// Clear the system-wide dependencies cache
    ClearCache,
}

pub fn exec(cmd: Cmd) -> miette::Result<()> {
    match cmd {
        Cmd::ClearCache => clear_cache::exec(),
    }
}
