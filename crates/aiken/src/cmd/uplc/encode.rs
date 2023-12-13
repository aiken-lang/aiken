use miette::IntoDiagnostic;
use std::{
    io::{self, Write},
    path::PathBuf,
};
use uplc::{
    ast::{DeBruijn, NamedDeBruijn, Program},
    flat::Binder,
    parser,
};

use super::Format;

/// Encode textual Untyped Plutus Core to flat bytes
#[derive(clap::Args)]
pub struct Args {
    /// Textual Untyped Plutus Core file
    input: PathBuf,

    // Format to convert to
    #[clap(long, default_value = "debruijn")]
    to: Format,

    /// Further encode the flat bytes as cbor bytes
    #[clap(short, long)]
    cbor: bool,

    /// Hex encode the bytes
    #[clap(long)]
    hex: bool,
}

pub fn exec(
    Args {
        input,
        to,
        cbor,
        hex,
    }: Args,
) -> miette::Result<()> {
    let code = std::fs::read_to_string(input).into_diagnostic()?;

    let program = parser::program(&code).into_diagnostic()?;

    match to {
        Format::Name => encode(program, cbor, hex),
        Format::NamedDebruijn => {
            let program: Program<NamedDeBruijn> = program.try_into().into_diagnostic()?;

            encode(program, cbor, hex)
        }
        Format::Debruijn => {
            let program: Program<DeBruijn> = program.try_into().into_diagnostic()?;

            encode(program, cbor, hex)
        }
    }
}

pub(crate) fn encode<'a, T>(program: Program<T>, cbor: bool, hex: bool) -> miette::Result<()>
where
    T: Binder<'a> + std::fmt::Debug,
{
    let mut stdout = io::stdout();

    let bytes = if cbor {
        program.to_cbor().into_diagnostic()?
    } else {
        program.to_flat().into_diagnostic()?
    };

    if hex {
        let bytes_hex = hex::encode(bytes);

        print!("{bytes_hex}");
    } else {
        stdout.write_all(&bytes).into_diagnostic()?;
    }

    stdout.flush().into_diagnostic()?;

    Ok(())
}
