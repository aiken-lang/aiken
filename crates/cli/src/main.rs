use std::{
    fmt::Write as _,
    fs::{self, File},
    io::BufReader, collections::HashMap, thread::LocalKey,
};

use pallas_addresses::Address;
use pallas_codec::{minicbor::bytes::ByteVec, utils::{MaybeIndefArray, KeyValuePairs}};
use pallas_primitives::babbage::{BigInt, Constr};
use pallas_traverse::{Era, MultiEraTx};
use uplc::{
    ast::{Constant, DeBruijn, FakeNamedDeBruijn, Name, NamedDeBruijn, Program, Term},
    machine::cost_model::ExBudget,
    parser, PlutusData,
};

mod args;

use args::{Args, TxCommand, UplcCommand};

use crate::args::ResolvedInput;

fn main() -> anyhow::Result<()> {
    let args = Args::default();

    match args {
        Args::Tx(tx_cmd) => match tx_cmd {
            TxCommand::Simulate {
                input,
                cbor,
                resolved_inputs,
            } => {
                let tx_bytes = if cbor {
                    fs::read(input)?
                } else {
                    let cbor_hex = fs::read_to_string(input)?;

                    hex::decode(cbor_hex.trim())?
                };

                let tx = MultiEraTx::decode(Era::Alonzo, &tx_bytes)
                    .or_else(|_| MultiEraTx::decode(Era::Byron, &tx_bytes))?;

                println!("Simulating: {}", tx.hash());

                let witnesses = tx.witnesses();

                if let Some(((datums, redeemers), scripts)) = witnesses
                    .plutus_data()
                    .zip(witnesses.redeemer())
                    .zip(witnesses.plutus_v1_script())
                {
                    for ((datum, redeemer), script) in
                        datums.iter().zip(redeemers.iter()).zip(scripts.iter())
                    {
                        let program: Program<NamedDeBruijn> = {
                            let mut buffer = Vec::new();

                            let prog =
                                Program::<FakeNamedDeBruijn>::from_cbor(&script.0, &mut buffer)?;

                            prog.into()
                        };

                        program
                            .apply_data(datum.clone())
                            .apply_data(redeemer.data.clone());

                        let file = File::open(&resolved_inputs)?;
                        let reader = BufReader::new(file);
                        let resolved_inputs: Vec<ResolvedInput> = serde_json::from_reader(reader)?;
                        let tx_in_info: Vec<PlutusData> = resolved_inputs
                            .iter()
                            .map(|resolved_input| {
                                let tx_out_ref = PlutusData::Constr(Constr {
                                    tag: 0,
                                    any_constructor: None,
                                    fields: MaybeIndefArray::Indef(vec![
                                        PlutusData::BoundedBytes(
                                            hex::decode(resolved_input.input.tx_hash.clone())
                                                .unwrap()
                                                .into(),
                                        ),
                                        PlutusData::BigInt(BigInt::Int(
                                            resolved_input.input.index.into(),
                                        )),
                                    ]),
                                });

                                let address =
                                    Address::from_bech32(&resolved_input.ouput.address).unwrap();

                                let payment_tag = match address.typeid() % 2 {
                                    0 => 0,
                                    1 => 1,
                                    _ => unreachable!(),
                                };
                                let stake_tag = match address.typeid() {
                                    0 | 1 => Some(0),
                                    2 | 3 => Some(1),
                                    _ => None,
                                };

                                let (payment_part, stake_part) = match address {
                                    Address::Shelley(s) => {
                                        (s.payment().to_vec(), s.delegation().to_vec())
                                    }
                                    _ => unreachable!(),
                                };

                                let lovelace = resolved_input.ouput.value.0;

                                let mut assets = resolved_input.ouput.value.1.clone();

                                assets.insert("".to_string(), vec![("".to_string(), lovelace)].into_iter().collect());

                                let tx_out = PlutusData::Constr(Constr {
                                    tag: 0,
                                    any_constructor: None,
                                    fields: MaybeIndefArray::Indef(vec![
                                        // txOutAddress
                                        PlutusData::Constr(Constr {
                                            tag: 0,
                                            any_constructor: None,
                                            fields: MaybeIndefArray::Indef(vec![
                                                // addressCredential
                                                PlutusData::Constr(Constr {
                                                    tag: payment_tag,
                                                    any_constructor: None,
                                                    fields: MaybeIndefArray::Indef(vec![
                                                        PlutusData::BoundedBytes(
                                                            payment_part.into(),
                                                        ),
                                                    ]),
                                                }),
                                                // addressStakingCredential
                                                PlutusData::Constr(Constr {
                                                    tag: if stake_tag.is_some() { 0 } else { 1 },
                                                    any_constructor: None,
                                                    fields: MaybeIndefArray::Indef(
                                                        if stake_tag.is_some() {
                                                            vec![
                                                                // StakingCredential
                                                                PlutusData::Constr(Constr {
                                                                    tag: 0,
                                                                    any_constructor: None,
                                                                    fields: MaybeIndefArray::Indef(vec![
                                                                        // StakingHash
                                                                        PlutusData::Constr(Constr {
                                                                            tag: stake_tag.unwrap(),
                                                                            any_constructor: None,
                                                                            fields: MaybeIndefArray::Indef(vec![
                                                                                PlutusData::BoundedBytes(
                                                                                    stake_part.into(),
                                                                                ),
                                                                            ]),
                                                                        }),
                                                                    ]),
                                                                }),
                                                            ]

                                                        } else {
                                                            vec![]
                                                        },
                                                    ),
                                                }),
                                            ]),
                                        }),
                                        
                                        // txOutValue
                                        PlutusData::Map(KeyValuePairs::Def(
                                            assets.iter().map(|val| {
                                                let currency_symbol = PlutusData::BoundedBytes(hex::decode(val.0).unwrap().into());
                                                let token_map = PlutusData::Map(KeyValuePairs::Def(
                                                    val.1.iter().map(|token| {
                                                        ( PlutusData::BoundedBytes(token.0.as_bytes().to_vec().into()),  PlutusData::BigInt(BigInt::Int((*token.1).into())))
                                                    }).collect()

                                                ));
                                                (currency_symbol, token_map)
                                            }).collect()
                                        )   ),


                                    ]),
                                });
                                PlutusData::Constr(Constr{
                                    tag: 0,
                                    any_constructor: None,
                                    fields: MaybeIndefArray::Indef(vec![
                                        tx_out_ref,
                                        tx_out
                                    ])
                                })
                            })
                            .collect();
                    }
                }

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
