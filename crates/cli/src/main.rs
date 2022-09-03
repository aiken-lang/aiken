use std::{fmt::Write as _, fs};

use pallas_traverse::{Era, MultiEraTx};
use uplc::{
    ast::{DeBruijn, FakeNamedDeBruijn, Name, NamedDeBruijn, Program, Term},
    machine::cost_model::ExBudget,
    parser,
};

mod args;

use args::{Args, TxCommand, UplcCommand};

fn main() -> anyhow::Result<()> {
    let args = Args::default();

    match args {
        Args::Tx(tx_cmd) => match tx_cmd {
            TxCommand::Simulate { input, cbor } => {
                let tx_bytes = if cbor {
                    fs::read(input)?
                } else {
                    let cbor_hex = fs::read_to_string(input)?;

                    hex::decode(cbor_hex.trim())?
                };

                let tx = MultiEraTx::decode(Era::Alonzo, &tx_bytes)
                    .or_else(|_| MultiEraTx::decode(Era::Byron, &tx_bytes))?;

                println!("Simulating: {}", tx.hash());

                println!("\nPlutus Data:");

                println!("{:#?}", tx.witnesses().plutus_data());

                println!("\nRedeemer:");

                println!("{:#?}", tx.witnesses().redeemer());

                println!("\nPlutus V1 Script:");

                println!("{:#?}", tx.witnesses().plutus_v1_script());

                println!("\nPlutus V2 Script:");

                println!("{:#?}", tx.witnesses().plutus_v2_script());
            }
        },
        Args::Uplc(uplc_cmd) => match uplc_cmd {
            UplcCommand::Flat { input, print, out } => {
                let code = std::fs::read_to_string(&input)?;

                let program = parser::program(&code)?;

                let program = Program::<DeBruijn>::try_from(program)?;

                let bytes = program.to_flat()?;

                if print {
                    let mut output = String::new();

                    for (i, byte) in bytes.iter().enumerate() {
                        let _ = write!(output, "{:08b}", byte);

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
            UplcCommand::Fmt { input, print } => {
                let code = std::fs::read_to_string(&input)?;

                let program = parser::program(&code)?;

                let pretty = program.to_pretty();

                if print {
                    println!("{}", pretty);
                } else {
                    fs::write(&input, pretty)?;
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
            UplcCommand::Eval { input, flat } => {
                let program = if flat {
                    let bytes = std::fs::read(&input)?;

                    let prog = Program::<FakeNamedDeBruijn>::from_flat(&bytes)?;

                    prog.into()
                } else {
                    let code = std::fs::read_to_string(&input)?;

                    let prog = parser::program(&code)?;

                    Program::<NamedDeBruijn>::try_from(prog)?
                };

                let (term, cost, _logs) = program.eval();

                match term {
                    Ok(term) => {
                        let term: Term<Name> = term.try_into()?;

                        println!("\nResult\n------\n\n{}\n", term.to_pretty());
                    }
                    Err(err) => {
                        eprintln!("\nError\n-----\n\n{}\n", err);
                    }
                }

                let budget = ExBudget::default();

                println!(
                    "\nCosts\n-----\ncpu: {}\nmemory: {}",
                    budget.cpu - cost.cpu,
                    budget.mem - cost.mem
                );
                println!(
                    "\nBudget\n------\ncpu: {}\nmemory: {}\n",
                    cost.cpu, cost.mem
                );
            }
        },
    }

    Ok(())
}
