use miette::IntoDiagnostic;

#[derive(clap::Args)]
#[clap(disable_version_flag(true))]
/// Start the Aiken language server
pub struct Args {
    /// Run on stdio
    #[clap(long)]
    stdio: bool,
}

pub fn exec(_args: Args) -> miette::Result<()> {
    aiken_lsp::start().into_diagnostic()
}
