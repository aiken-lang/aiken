use miette::IntoDiagnostic;

#[derive(clap::Args)]
/// Start the Aiken language server
pub struct Args {
    /// Run on stdio
    #[clap(long)]
    stdio: bool,
}

pub fn exec(_args: Args) -> miette::Result<()> {
    aiken_lsp::start().into_diagnostic()
}
