use miette::IntoDiagnostic;
use pallas_primitives::{
    babbage::{TransactionInput, TransactionOutput},
    Fragment,
};
use pallas_traverse::{Era, MultiEraTx};
use std::fs;
use std::path::PathBuf;
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

        let result =
            tx::eval_phase_two(tx_babbage, &resolved_inputs, None, None, &slot_config, true);

        match result {
            Ok(redeemers) => {
                println!("\nTotal Budget Used\n-----------------\n");

                let total_budget_used =
                    redeemers
                        .iter()
                        .fold(ExBudget { mem: 0, cpu: 0 }, |accum, curr| ExBudget {
                            mem: accum.mem + curr.ex_units.mem as i64,
                            cpu: accum.cpu + curr.ex_units.steps as i64,
                        });

                println!("mem: {}", total_budget_used.mem);
                println!("cpu: {}", total_budget_used.cpu);
            }
            Err(err) => {
                eprintln!("\nError\n-----\n\n{}\n", err);
            }
        }
    }

    Ok(())
}
