use miette::IntoDiagnostic;
use std::{fs, path::PathBuf};
use uplc::parser;

#[derive(clap::Args)]
/// Format an Untyped Plutus Core program
pub struct Args {
    /// Textual Untyped Plutus Core file
    input: PathBuf,

    /// Print output instead of saving to file
    #[clap(short, long)]
    print: bool,
}

pub fn exec(Args { input, print }: Args) -> miette::Result<()> {
    let code = std::fs::read_to_string(&input).into_diagnostic()?;

    let program = parser::program(&code).into_diagnostic()?;

    let pretty = program.to_pretty();

    if print {
        println!("{pretty}");
    } else {
        fs::write(&input, pretty).into_diagnostic()?;
    }

    Ok(())
}
