use std::{fmt::Write as _, fs};

use pallas_primitives::{
    babbage::{TransactionInput, TransactionOutput},
    Fragment,
};
use pallas_traverse::{Era, MultiEraTx};
use uplc::{
    ast::{DeBruijn, FakeNamedDeBruijn, Name, NamedDeBruijn, Program, Term},
    machine::cost_model::ExBudget,
    parser,
    tx::{
        self,
        script_context::{ResolvedInput, SlotConfig},
    },
};

mod args;

use args::{Args, TxCommand, UplcCommand};

fn main() -> anyhow::Result<()> {
    let args = Args::default();

    match args {
        Args::Build => {
            // 1. load and parse modules
            //    * lib - contains modules, types, and functions
            //    * contracts - contains validators
            //    * scripts - contains native scripts dsl
            // 2. type check everything
            // 3. generate uplc and policy/address if relevant
            todo!()
        }

        Args::Dev => {
            // launch a development server
            // this should allow people to test
            // their contracts over http
            todo!()
        }

        Args::New { name } => {
            if !name.exists() {
                fs::create_dir_all(name.join("lib"))?;
                fs::create_dir_all(name.join("policies"))?;
                fs::create_dir_all(name.join("scripts"))?;
            }
        }

        Args::Tx(tx_cmd) => match tx_cmd {
            TxCommand::Simulate {
                input,
                cbor,
                raw_inputs,
                raw_outputs,
                slot_length,
                zero_time,
                zero_slot,
            } => {
                let (tx_bytes, inputs_bytes, outputs_bytes) = if cbor {
                    (
                        fs::read(input)?,
                        fs::read(raw_inputs)?,
                        fs::read(raw_outputs)?,
                    )
                } else {
                    let cbor_hex = fs::read_to_string(input)?;
                    let inputs_hex = fs::read_to_string(raw_inputs)?;
                    let outputs_hex = fs::read_to_string(raw_outputs)?;

                    (
                        hex::decode(cbor_hex.trim())?,
                        hex::decode(inputs_hex.trim())?,
                        hex::decode(outputs_hex.trim())?,
                    )
                };

                let tx = MultiEraTx::decode(Era::Babbage, &tx_bytes)
                    .or_else(|_| MultiEraTx::decode(Era::Alonzo, &tx_bytes))?;

                let inputs = Vec::<TransactionInput>::decode_fragment(&inputs_bytes).unwrap();
                let outputs = Vec::<TransactionOutput>::decode_fragment(&outputs_bytes).unwrap();

                let resolved_inputs: Vec<ResolvedInput> = inputs
                    .iter()
                    .zip(outputs.iter())
                    .map(|(input, output)| ResolvedInput {
                        input: input.clone(),
                        output: output.clone(),
                    })
                    .collect();

                println!("Simulating: {}", tx.hash());

                if let Some(tx_babbage) = tx.as_babbage() {
                    let slot_config = SlotConfig {
                        zero_time,
                        zero_slot,
                        slot_length,
                    };

                    let result = tx::eval_phase_two(
                        tx_babbage,
                        &resolved_inputs,
                        None,
                        None,
                        &slot_config,
                        true,
                    );

                    match result {
                        Ok(redeemers) => {
                            println!("\nTotal Budget Used\n-----------------\n");

                            let total_budget_used = redeemers.iter().fold(
                                ExBudget { mem: 0, cpu: 0 },
                                |accum, curr| ExBudget {
                                    mem: accum.mem + curr.ex_units.mem as i64,
                                    cpu: accum.cpu + curr.ex_units.steps as i64,
                                },
                            );

                            println!("mem: {}", total_budget_used.mem);
                            println!("cpu: {}", total_budget_used.cpu);
                        }
                        Err(err) => {
                            eprintln!("\nError\n-----\n\n{}\n", err);
                        }
                    }
                }
            }
        },
        Args::Uplc(uplc_cmd) => match uplc_cmd {
            UplcCommand::Flat {
                input,
                print,
                out,
                cbor_hex,
            } => {
                let code = std::fs::read_to_string(&input)?;

                let program = parser::program(&code)?;

                let program = Program::<DeBruijn>::try_from(program)?;

                if cbor_hex {
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
                } else {
                    let cbor = program.to_hex()?;

                    if print {
                        println!("{}", &cbor);
                    } else {
                        let out_name = if let Some(out) = out {
                            out
                        } else {
                            format!("{}.cbor", input.file_stem().unwrap().to_str().unwrap())
                        };

                        fs::write(&out_name, &cbor)?;
                    }
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
            UplcCommand::Unflat {
                input,
                print,
                out,
                cbor_hex,
            } => {
                let program = if cbor_hex {
                    let cbor = std::fs::read_to_string(&input)?;

                    let mut cbor_buffer = Vec::new();
                    let mut flat_buffer = Vec::new();

                    Program::<DeBruijn>::from_hex(cbor.trim(), &mut cbor_buffer, &mut flat_buffer)?
                } else {
                    let bytes = std::fs::read(&input)?;

                    Program::<DeBruijn>::from_flat(&bytes)?
                };

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

            UplcCommand::Eval { script, flat, args } => {
                let mut program = if flat {
                    let bytes = std::fs::read(&script)?;

                    let prog = Program::<FakeNamedDeBruijn>::from_flat(&bytes)?;

                    prog.into()
                } else {
                    let code = std::fs::read_to_string(&script)?;

                    let prog = parser::program(&code)?;

                    Program::<NamedDeBruijn>::try_from(prog)?
                };

                for arg in args {
                    let term: Term<NamedDeBruijn> = parser::term(&arg)?.try_into()?;

                    program = program.apply_term(&term);
                }

                let (term, cost, logs) = program.eval();

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

                if !logs.is_empty() {
                    println!("\nLogs\n----\n{}", logs.join("\n"))
                }
            }
        },
    }

    Ok(())
}
