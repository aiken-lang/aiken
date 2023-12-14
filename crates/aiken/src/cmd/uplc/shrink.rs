use miette::IntoDiagnostic;
use std::path::PathBuf;
use uplc::ast::{DeBruijn, Name, NamedDeBruijn, Program};
use uplc::optimize::aiken_optimize_and_intern;

use super::{encode, Format};

#[derive(clap::Args)]
/// Shrink / Optimize UPLC code using a variety of optimization steps
pub struct Args {
    /// Flat encoded Untyped Plutus Core file
    input: PathBuf,

    // Format to convert from
    #[clap(long, default_value = "debruijn")]
    from: Format,

    // Format to convert into
    #[clap(long, default_value = "debruijn")]
    to: Format,

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
        to,
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

    let program: Program<Name> = match from {
        Format::Name => {
            if cbor {
                let mut flat_buffer = Vec::new();
                Program::from_cbor(&bytes, &mut flat_buffer).into_diagnostic()?
            } else {
                Program::from_flat(&bytes).into_diagnostic()?
            }
        }
        Format::NamedDebruijn => {
            let program: Program<NamedDeBruijn> = if cbor {
                let mut flat_buffer = Vec::new();
                Program::from_cbor(&bytes, &mut flat_buffer).into_diagnostic()?
            } else {
                Program::from_flat(&bytes).into_diagnostic()?
            };

            program.try_into().unwrap()
        }
        Format::Debruijn => {
            let program: Program<DeBruijn> = if cbor {
                let mut flat_buffer = Vec::new();
                Program::from_cbor(&bytes, &mut flat_buffer).into_diagnostic()?
            } else {
                Program::from_flat(&bytes).into_diagnostic()?
            };

            program.try_into().unwrap()
        }
    };

    let optimized_program = aiken_optimize_and_intern(program);

    match to {
        Format::Name => encode::encode(optimized_program, cbor, hex),
        Format::NamedDebruijn => {
            let program: Program<NamedDeBruijn> = optimized_program.try_into().into_diagnostic()?;

            encode::encode(program, cbor, hex)
        }
        Format::Debruijn => {
            let program: Program<DeBruijn> = optimized_program.try_into().into_diagnostic()?;

            encode::encode(program, cbor, hex)
        }
    }
}
