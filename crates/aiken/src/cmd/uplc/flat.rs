use miette::IntoDiagnostic;
use std::{fmt::Write, fs, path::PathBuf};
use uplc::{
    ast::{DeBruijn, Program},
    parser,
};

#[derive(clap::Args)]
/// Encode textual Untyped Plutus Core to flat bytes
pub struct Args {
    /// Textual Untyped Plutus Core file
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
    let code = std::fs::read_to_string(&input).into_diagnostic()?;

    let program = parser::program(&code).into_diagnostic()?;

    let program = Program::<DeBruijn>::try_from(program).into_diagnostic()?;

    if !cbor_hex {
        let bytes = program.to_flat().into_diagnostic()?;

        if print {
            let mut output = String::new();

            for (i, byte) in bytes.iter().enumerate() {
                let _ = write!(output, "{byte:08b}");

                if (i + 1) % 4 == 0 {
                    output.push('\n');
                } else {
                    output.push(' ');
                }
            }

            println!("{output}");
        } else {
            let out_name = if let Some(out) = out {
                out
            } else {
                format!("{}.flat", input.file_stem().unwrap().to_str().unwrap())
            };

            fs::write(out_name, &bytes).into_diagnostic()?;
        }
    } else {
        let cbor = program.to_hex().into_diagnostic()?;

        if print {
            println!("{}", &cbor);
        } else {
            let out_name = if let Some(out) = out {
                out
            } else {
                format!("{}.cbor", input.file_stem().unwrap().to_str().unwrap())
            };

            fs::write(out_name, &cbor).into_diagnostic()?;
        }
    }

    Ok(())
}
