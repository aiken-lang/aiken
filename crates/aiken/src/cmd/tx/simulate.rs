use miette::IntoDiagnostic;
use owo_colors::{OwoColorize, Stream::Stderr};
use pallas::ledger::{
    primitives::{
        babbage::{Redeemer, TransactionInput, TransactionOutput},
        Fragment,
    },
    traverse::{Era, MultiEraTx},
};

use std::{fmt, fs, path::PathBuf, process};
use uplc::{
    machine::cost_model::ExBudget,
    tx::{
        self,
        script_context::{ResolvedInput, SlotConfig},
    },
};

#[derive(clap::Args)]
/// Simulate a transaction by evaluating it's script
pub struct Args {
    /// A file containing cbor hex for a transaction
    input: PathBuf,

    /// Toggle whether input is raw cbor or a hex string
    #[clap(short, long)]
    cbor: bool,

    /// A file containing cbor hex for the raw inputs
    raw_inputs: PathBuf,

    /// A file containing cbor hex for the raw outputs
    raw_outputs: PathBuf,

    /// Time between each slot
    #[clap(short, long, default_value_t = 1000)]
    slot_length: u32,

    /// Time of shelley hardfork
    #[clap(long, default_value_t = 1596059091000)]
    zero_time: u64,

    /// Slot number at the start of the shelley hardfork
    #[clap(long, default_value_t = 4492800)]
    zero_slot: u64,
}

pub fn exec(
    Args {
        input,
        cbor,
        raw_inputs,
        raw_outputs,
        slot_length,
        zero_time,
        zero_slot,
    }: Args,
) -> miette::Result<()> {
    eprintln!(
        "{} script context",
        "      Parsing"
            .if_supports_color(Stderr, |s| s.purple())
            .if_supports_color(Stderr, |s| s.bold())
    );

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

    let tx = MultiEraTx::decode_for_era(Era::Babbage, &tx_bytes).into_diagnostic()?;

    eprintln!(
        "{} {}",
        "   Simulating"
            .if_supports_color(Stderr, |s| s.purple())
            .if_supports_color(Stderr, |s| s.bold()),
        tx.hash()
    );

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

    if let Some(tx_babbage) = tx.as_babbage() {
        let slot_config = SlotConfig {
            zero_time,
            zero_slot,
            slot_length,
        };

        let with_redeemer = |redeemer: &Redeemer| {
            eprintln!(
                "{} {:?} → {}",
                "     Redeemer"
                    .if_supports_color(Stderr, |s| s.purple())
                    .if_supports_color(Stderr, |s| s.bold()),
                redeemer.tag,
                redeemer.index
            )
        };

        let result = tx::eval_phase_two(
            tx_babbage,
            &resolved_inputs,
            None,
            None,
            &slot_config,
            true,
            with_redeemer,
        );

        match result {
            Ok(redeemers) => {
                // this should allow N scripts to be
                let total_budget_used: Vec<ExBudget> = redeemers
                    .iter()
                    .map(|curr| ExBudget {
                        mem: curr.ex_units.mem as i64,
                        cpu: curr.ex_units.steps as i64,
                    })
                    .collect();

                eprintln!("\n");
                println!(
                    "{}",
                    serde_json::to_string_pretty(&total_budget_used)
                        .map_err(|_| fmt::Error)
                        .into_diagnostic()?
                );
            }
            Err(err) => {
                eprintln!("{}", display_tx_error(&err));
                process::exit(1);
            }
        }
    }

    Ok(())
}

fn display_tx_error(err: &tx::error::Error) -> String {
    let mut msg = format!(
        "{} {}",
        "        Error"
            .if_supports_color(Stderr, |s| s.red())
            .if_supports_color(Stderr, |s| s.bold()),
        err.red()
    );
    match err {
        tx::error::Error::RedeemerError { err, .. } => {
            msg.push_str(&format!(
                "\n{}",
                display_tx_error(err)
                    .lines()
                    .skip(1)
                    .collect::<Vec<_>>()
                    .join("\n"),
            ));
            msg
        }
        tx::error::Error::Machine(_, _, traces) => {
            msg.push_str(
                traces
                    .iter()
                    .map(|s| {
                        format!(
                            "\n{} {}",
                            "        Trace"
                                .if_supports_color(Stderr, |s| s.yellow())
                                .if_supports_color(Stderr, |s| s.bold()),
                            s.if_supports_color(Stderr, |s| s.yellow())
                        )
                    })
                    .collect::<Vec<_>>()
                    .join("")
                    .as_str(),
            );
            msg
        }
        _ => msg,
    }
}
