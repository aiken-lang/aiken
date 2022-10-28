use miette::IntoDiagnostic;
use std::{fs, path::PathBuf};
use uplc::ast::{DeBruijn, Name, Program};

#[derive(clap::Args)]
/// Decode flat bytes to textual Untyped Plutus Core
pub struct Args {
    /// Flat encoded Untyped Plutus Core file
    input: PathBuf,

    /// Output file name
    #[clap(short, long)]
    out: Option<String>,

    /// Print output instead of saving to file
    #[clap(short, long)]
    print: bool,

    #[clap(short, long)]
    cbor_hex: bool,
}

pub fn exec(
    Args {
        input,
        out,
        print,
        cbor_hex,
    }: Args,
) -> miette::Result<()> {
    let program = if cbor_hex {
        let cbor = std::fs::read_to_string(&input).into_diagnostic()?;

        let mut cbor_buffer = Vec::new();
        let mut flat_buffer = Vec::new();

        Program::<DeBruijn>::from_hex(cbor.trim(), &mut cbor_buffer, &mut flat_buffer)
            .into_diagnostic()?
    } else {
        let bytes = std::fs::read(&input).into_diagnostic()?;

        Program::<DeBruijn>::from_flat(&bytes).into_diagnostic()?
    };

    let program: Program<Name> = program.try_into().into_diagnostic()?;

    let pretty = program.to_pretty();

    if print {
        println!("{}", pretty);
    } else {
        let out_name = if let Some(out) = out {
            out
        } else {
            format!("{}.uplc", input.file_stem().unwrap().to_str().unwrap())
        };

        fs::write(&out_name, pretty).into_diagnostic()?;
    }
    return Ok(());
}
