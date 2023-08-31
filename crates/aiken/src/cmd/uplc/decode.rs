use miette::IntoDiagnostic;
use std::{path::PathBuf, println};
use uplc::ast::{DeBruijn, Name, NamedDeBruijn, Program};

use super::Format;

#[derive(clap::Args)]
/// Decode flat bytes to textual Untyped Plutus Core
pub struct Args {
    /// Flat encoded Untyped Plutus Core file
    input: PathBuf,

    // Format to convert from
    #[clap(long, default_value = "debruijn")]
    from: Format,

    /// Input file contains cbor encoded flat bytes
    #[clap(short, long)]
    cbor: bool,

    /// Input file contents will be hex decoded
    #[clap(long)]
    hex: bool,
}

pub fn exec(
    Args {
        input,
        from,
        cbor,
        hex,
    }: Args,
) -> miette::Result<()> {
    let bytes = if hex {
        let hex_bytes = std::fs::read_to_string(&input).into_diagnostic()?;

        hex::decode(hex_bytes.trim()).into_diagnostic()?
    } else {
        std::fs::read(&input).into_diagnostic()?
    };

    let pretty_uplc = match from {
        Format::Name => {
            let program: Program<Name> = if cbor {
                let mut flat_buffer = Vec::new();
                Program::from_cbor(&bytes, &mut flat_buffer).into_diagnostic()?
            } else {
                Program::from_flat(&bytes).into_diagnostic()?
            };

            program.to_pretty()
        }
        Format::NamedDebruijn => {
            let program: Program<NamedDeBruijn> = if cbor {
                let mut flat_buffer = Vec::new();
                Program::from_cbor(&bytes, &mut flat_buffer).into_diagnostic()?
            } else {
                Program::from_flat(&bytes).into_diagnostic()?
            };

            let program: Program<Name> = program.try_into().unwrap();

            program.to_pretty()
        }
        Format::Debruijn => {
            let program: Program<DeBruijn> = if cbor {
                let mut flat_buffer = Vec::new();
                Program::from_cbor(&bytes, &mut flat_buffer).into_diagnostic()?
            } else {
                Program::from_flat(&bytes).into_diagnostic()?
            };

            let program: Program<Name> = program.try_into().unwrap();

            program.to_pretty()
        }
    };

    println!("{pretty_uplc}");

    Ok(())
}
