use miette::IntoDiagnostic;
use owo_colors::{OwoColorize, Stream::Stderr};
use pallas_primitives::{
    conway::{Redeemer, TransactionInput, TransactionOutput},
    Fragment,
};
use pallas_traverse::{Era, MultiEraTx};
use std::{fmt, fs, path::PathBuf, process};
use uplc::{
    machine::cost_model::ExBudget,
    tx::{
        self, redeemer_tag_to_string,
        script_context::{ResolvedInput, SlotConfig},
    },
};

#[derive(clap::Args)]
/// Simulate a transaction by evaluating it's script
pub struct Args {
    /// A file containing cbor hex for a transaction
    #[clap(value_name = "FILEPATH")]
    input: PathBuf,

    /// Toggle whether input is raw cbor or a hex string
    #[clap(short, long)]
    cbor: bool,

    /// A file containing cbor hex for the raw inputs
    #[clap(value_name = "FILEPATH")]
    raw_inputs: PathBuf,

    /// A file containing cbor hex for the raw outputs
    #[clap(value_name = "FILEPATH")]
    raw_outputs: PathBuf,

    /// Time between each slot
    #[clap(short, long, default_value_t = 1000, value_name = "MILLISECOND")]
    slot_length: u32,

    /// Time of shelley hardfork
    #[clap(long, default_value_t = 1596059091000, value_name = "POSIX")]
    zero_time: u64,

    /// Slot number at the start of the shelley hardfork
    #[clap(long, default_value_t = 4492800, value_name = "SLOT")]
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

    let tx = MultiEraTx::decode_for_era(Era::Conway, &tx_bytes).into_diagnostic()?;

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

    if let Some(tx_conway) = tx.as_conway() {
        let slot_config = SlotConfig {
            zero_time,
            zero_slot,
            slot_length,
        };

        let with_redeemer = |redeemer: &Redeemer| {
            eprintln!(
                "{} {}[{}]",
                "   Evaluating"
                    .if_supports_color(Stderr, |s| s.purple())
                    .if_supports_color(Stderr, |s| s.bold()),
                redeemer_tag_to_string(&redeemer.tag),
                redeemer.index
            )
        };

        let result = tx::eval_phase_two(
            tx_conway,
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
                eprintln!(
                    "{} {}",
                    "        Error"
                        .if_supports_color(Stderr, |s| s.red())
                        .if_supports_color(Stderr, |s| s.bold()),
                    err.red()
                );

                process::exit(1);
            }
        }
    }

    Ok(())
}
