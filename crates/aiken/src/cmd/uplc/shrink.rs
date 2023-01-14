use miette::IntoDiagnostic;
use std::{fs, path::PathBuf};
use uplc::ast::{Name, Program};
use uplc::parser;

// TODO: OR Unflatten, shrink, flatten

/// Shrink textual untyped plutus core, output untyped plutus core
#[derive(clap::Args)]
pub struct Args {
    /// Untyped Plutus Core file
    input: PathBuf,

    /// Output file name
    #[clap(short, long)]
    out: Option<String>,

    /// Print output instead of saving to file
    #[clap(short, long)]
    print: bool,
}

pub fn exec(
    Args {
        input,
        out,
        print,
        // cbor_hex,
    }: Args,
) -> miette::Result<()> {
    let code = std::fs::read_to_string(&input).into_diagnostic()?;
    let program = parser::program(&code).into_diagnostic()?;

    let program: Program<Name> = program.shrink();

    let pretty = program.to_pretty();

    if print {
        println!("{}", pretty);
    } else {
        let out_name = if let Some(out) = out {
            out
        } else {
            format!("{}.uplc", input.file_stem().unwrap().to_str().unwrap())
        };

        fs::write(out_name, pretty).into_diagnostic()?;
    }
    Ok(())
}
