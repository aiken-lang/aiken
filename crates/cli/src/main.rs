use std::{env, fmt::Write as _, fs};

use miette::IntoDiagnostic;
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

use aiken::{config::Config, error, project::Project};

mod args;

use args::{Args, TxCommand, UplcCommand};

fn main() -> miette::Result<()> {
    miette::set_panic_hook();

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

        Args::Check { directory } => {
            let project_path = if let Some(d) = directory {
                d
            } else {
                env::current_dir().into_diagnostic()?
            };

            let config = Config::load(project_path.clone()).into_diagnostic()?;

            let mut project = Project::new(config, project_path);

            if let Err(err) = project.build() {
                match err {
                    error::Error::List(errors) => {
                        for error in errors {
                            eprintln!("Error: {:?}", error)
                        }
                    }
                    rest => Err(rest)?,
                }
            };
        }

        Args::Dev => {
            // launch a development server
            // this should allow people to test
            // their contracts over http
            todo!()
        }

        Args::New { name } => {
            if !name.exists() {
                fs::create_dir_all(name.join("lib")).into_diagnostic()?;
                fs::create_dir_all(name.join("policies")).into_diagnostic()?;
                fs::create_dir_all(name.join("scripts")).into_diagnostic()?;
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
                        fs::read(input).into_diagnostic()?,
                        fs::read(raw_inputs).into_diagnostic()?,
                        fs::read(raw_outputs).into_diagnostic()?,
                    )
                } else {
                    let cbor_hex = fs::read_to_string(input).into_diagnostic()?;
                    let inputs_hex = fs::read_to_string(raw_inputs).into_diagnostic()?;
                    let outputs_hex = fs::read_to_string(raw_outputs).into_diagnostic()?;

                    (
                        hex::decode(cbor_hex.trim()).into_diagnostic()?,
                        hex::decode(inputs_hex.trim()).into_diagnostic()?,
                        hex::decode(outputs_hex.trim()).into_diagnostic()?,
                    )
                };

                let tx = MultiEraTx::decode(Era::Babbage, &tx_bytes)
                    .or_else(|_| MultiEraTx::decode(Era::Alonzo, &tx_bytes))
                    .into_diagnostic()?;

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
                let code = std::fs::read_to_string(&input).into_diagnostic()?;

                let program = parser::program(&code).into_diagnostic()?;

                let program = Program::<DeBruijn>::try_from(program).into_diagnostic()?;

                if !cbor_hex {
                    let bytes = program.to_flat().into_diagnostic()?;

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

                        fs::write(&out_name, &bytes).into_diagnostic()?;
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

                        fs::write(&out_name, &cbor).into_diagnostic()?;
                    }
                }
            }

            UplcCommand::Fmt { input, print } => {
                let code = std::fs::read_to_string(&input).into_diagnostic()?;

                let program = parser::program(&code).into_diagnostic()?;

                let pretty = program.to_pretty();

                if print {
                    println!("{}", pretty);
                } else {
                    fs::write(&input, pretty).into_diagnostic()?;
                }
            }
            UplcCommand::Unflat {
                input,
                print,
                out,
                cbor_hex,
            } => {
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
            }

            UplcCommand::Eval { script, flat, args } => {
                let mut program = if flat {
                    let bytes = std::fs::read(&script).into_diagnostic()?;

                    let prog = Program::<FakeNamedDeBruijn>::from_flat(&bytes).into_diagnostic()?;

                    prog.into()
                } else {
                    let code = std::fs::read_to_string(&script).into_diagnostic()?;

                    let prog = parser::program(&code).into_diagnostic()?;

                    Program::<NamedDeBruijn>::try_from(prog).into_diagnostic()?
                };

                for arg in args {
                    let term: Term<NamedDeBruijn> = parser::term(&arg)
                        .into_diagnostic()?
                        .try_into()
                        .into_diagnostic()?;

                    program = program.apply_term(&term);
                }

                let (term, cost, logs) = program.eval();

                match term {
                    Ok(term) => {
                        let term: Term<Name> = term.try_into().into_diagnostic()?;

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
