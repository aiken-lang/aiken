use std::fs;

use uplc::{
    ast::{DeBruijn, Name, Program},
    parser,
};

mod args;

use args::{Args, UplcCommand};

fn main() -> anyhow::Result<()> {
    let args = Args::default();

    match args {
        Args::Uplc(uplc) => match uplc {
            UplcCommand::Flat { input, print, out } => {
                let code = std::fs::read_to_string(&input)?;

                let program = parser::program(&code)?;

                let program = Program::<DeBruijn>::try_from(program)?;

                let bytes = program.to_flat()?;

                if print {
                    let mut output = String::new();

                    for (i, byte) in bytes.iter().enumerate() {
                        output.push_str(&format!("{:08b}", byte));

                        if (i + 1) % 4 == 0 {
                            output.push('\n');
                        } else {
                            output.push(' ');
                        }
                    }

                    println!("{}", output);
                } else {
                    let out_name = if let Some(out) = out {
                        out
                    } else {
                        format!("{}.flat", input.file_stem().unwrap().to_str().unwrap())
                    };

                    fs::write(&out_name, &bytes)?;
                }
            }
            UplcCommand::Unflat { input, print, out } => {
                let bytes = std::fs::read(&input)?;

                let program = Program::<DeBruijn>::from_flat(&bytes)?;

                let program: Program<Name> = program.try_into()?;

                let pretty = program.to_pretty();

                if print {
                    println!("{}", pretty);
                } else {
                    let out_name = if let Some(out) = out {
                        out
                    } else {
                        format!("{}.uplc", input.file_stem().unwrap().to_str().unwrap())
                    };

                    fs::write(&out_name, pretty)?;
                }
            }
        },
    }

    Ok(())
}
