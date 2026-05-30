pub mod capabilities;
pub mod clean;
pub mod doctor;
pub mod run;

mod output;
mod paths;

use clap::Subcommand;

/// Commands for formal verification of property tests
#[derive(Subcommand)]
#[clap(disable_version_flag(true))]
pub enum Cmd {
    /// Run formal verification on property tests
    Run(run::Args),

    /// Check toolchain, dependencies, and configuration
    Doctor(doctor::Args),

    /// Remove generated verification artifacts and logs
    Clean(clean::Args),

    /// Show supported verification capabilities
    Capabilities(capabilities::Args),
}

pub fn exec(cmd: Cmd) -> miette::Result<()> {
    match cmd {
        Cmd::Run(args) => run::exec(args),
        Cmd::Doctor(args) => doctor::exec(args),
        Cmd::Clean(args) => clean::exec(args),
        Cmd::Capabilities(args) => capabilities::exec(args),
    }
}

#[cfg(test)]
mod tests;
