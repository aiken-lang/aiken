use aiken_project::assurance::consumer::ConsumerWorkspace;
use miette::IntoDiagnostic;
use std::path::PathBuf;

/// Capture the pinned consumer's loaded checking environment
#[derive(clap::Args)]
#[clap(disable_version_flag(true))]
#[clap(
    after_long_help = "Set [assurance] workspace in aiken.toml to the provisioned CardanoLedgerApiBlaster checkout, or set AIKEN_ASSURANCE_WORKSPACE. Config paths are relative to aiken.toml; environment-variable and output paths are relative to the current directory. Uses lake env lean with both assurance and ledger imports. Revision mismatches warn but do not prevent capture; <out>.run.json records revisions, warnings and capture status. See scripts/ual/README.md for setup; Aiken never installs Lean or Z3."
)]
pub struct Args {
    /// Destination manifest, usable by verify generate --environment
    #[clap(long, value_name = "PATH", default_value = "environment.json")]
    out: PathBuf,
}

pub fn exec(args: Args) -> miette::Result<()> {
    let directory = std::env::current_dir().into_diagnostic()?;
    let workspace = ConsumerWorkspace::load(&directory)?;
    let record = workspace.capture_environment(&args.out)?;
    eprintln!(
        "Captured consumer environment in {} (run record: {}).",
        args.out.display(),
        record.display()
    );
    Ok(())
}
