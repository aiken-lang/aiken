use crate::{
    ast::{DeBruijn, Program},
    machine::cost_model::ExBudget,
    PlutusData,
};
use error::Error;
use pallas_primitives::{
    conway::{
        CostModels, ExUnits, MintedTx, Redeemer, Redeemers, RedeemersKey, TransactionInput,
        TransactionOutput,
    },
    Fragment,
};
use pallas_traverse::{Era, MultiEraTx};
pub use phase_one::{eval_phase_one, redeemer_tag_to_string};
pub use script_context::{DataLookupTable, ResolvedInput, SlotConfig};

pub mod error;
pub mod eval;
mod phase_one;
pub mod script_context;
#[cfg(test)]
mod tests;
pub mod to_plutus_data;

/// Evaluate the scripts in a transaction using
/// the UPLC Cek Machine. This function collects
/// redeemers with ExUnits calculated from the evaluation.
/// You may optionally run a subset of phase one checks on
/// redeemers and scripts.
pub fn eval_phase_two(
    tx: &MintedTx,
    utxos: &[ResolvedInput],
    cost_mdls: Option<&CostModels>,
    initial_budget: Option<&ExBudget>,
    slot_config: &SlotConfig,
    run_phase_one: bool,
    with_redeemer: fn(&Redeemer) -> (),
) -> Result<Vec<Redeemer>, Error> {
    let redeemers = tx.transaction_witness_set.redeemer.as_ref();

    let lookup_table = DataLookupTable::from_transaction(tx, utxos);

    if run_phase_one {
        // subset of phase 1 check on redeemers and scripts
        eval_phase_one(tx, utxos, &lookup_table)?;
    }

    match redeemers {
        Some(rs) => {
            let mut collected_redeemers = vec![];

            let mut remaining_budget = *initial_budget.unwrap_or(&ExBudget::default());

            for (key, data, ex_units) in iter_redeemers(rs) {
                let redeemer = Redeemer {
                    tag: key.tag,
                    index: key.index,
                    data: data.clone(),
                    ex_units,
                };

                with_redeemer(&redeemer);

                let redeemer = eval::eval_redeemer(
                    tx,
                    utxos,
                    slot_config,
                    &redeemer,
                    &lookup_table,
                    cost_mdls,
                    &remaining_budget,
                )?;

                // The subtraction is safe here as ex units counting is done during evaluation.
                // Redeemer would fail already if budget was negative.
                remaining_budget.cpu -= redeemer.ex_units.steps as i64;
                remaining_budget.mem -= redeemer.ex_units.mem as i64;

                collected_redeemers.push(redeemer)
            }

            Ok(collected_redeemers)
        }
        None => Ok(vec![]),
    }
}

/// This function is the same as [`eval_phase_two`]
/// but the inputs are raw bytes.
/// initial_budget expects (cpu, mem).
/// slot_config (zero_time, zero_slot, slot_length)
pub fn eval_phase_two_raw(
    tx_bytes: &[u8],
    utxos_bytes: &[(Vec<u8>, Vec<u8>)],
    cost_mdls_bytes: Option<&[u8]>,
    initial_budget: (u64, u64),
    slot_config: (u64, u64, u32),
    run_phase_one: bool,
    with_redeemer: fn(&Redeemer) -> (),
) -> Result<Vec<Vec<u8>>, Error> {
    let multi_era_tx = MultiEraTx::decode_for_era(Era::Conway, tx_bytes)
        .or_else(|_| MultiEraTx::decode_for_era(Era::Babbage, tx_bytes))
        .or_else(|_| MultiEraTx::decode_for_era(Era::Alonzo, tx_bytes))?;

    let cost_mdls = cost_mdls_bytes
        .map(CostModels::decode_fragment)
        .transpose()?;

    let budget = ExBudget {
        cpu: initial_budget.0 as i64,
        mem: initial_budget.1 as i64,
    };

    let mut utxos = Vec::new();

    for (input, output) in utxos_bytes {
        utxos.push(ResolvedInput {
            input: TransactionInput::decode_fragment(input)?,
            output: TransactionOutput::decode_fragment(output)?,
        });
    }

    let sc = SlotConfig {
        zero_time: slot_config.0,
        zero_slot: slot_config.1,
        slot_length: slot_config.2,
    };

    match multi_era_tx {
        MultiEraTx::Conway(tx) => {
            match eval_phase_two(
                &tx,
                &utxos,
                cost_mdls.as_ref(),
                Some(&budget),
                &sc,
                run_phase_one,
                with_redeemer,
            ) {
                Ok(redeemers) => Ok(redeemers
                    .iter()
                    .map(|r| r.encode_fragment().unwrap())
                    .collect()),
                Err(err) => Err(err),
            }
        }
        _ => unimplemented!(
            r#"The transaction is serialized in an old era format. Because we're slightly lazy to
maintain backward compatibility with every possible transaction format AND, because
those formats are mostly forward-compatible, you are kindly expected to provide a
transaction in a format suitable for the Conway era."#
        ),
    }
}

pub fn apply_params_to_script(
    params_bytes: &[u8], // PlutusData array
    plutus_script_bytes: &[u8],
) -> Result<Vec<u8>, Error> {
    let params = match PlutusData::decode_fragment(params_bytes).unwrap() {
        PlutusData::Array(res) => res,
        _ => unreachable!(),
    };

    let mut buffer = Vec::new();
    let mut program = Program::<DeBruijn>::from_cbor(plutus_script_bytes, &mut buffer)?;

    for param in params.to_vec() {
        program = program.apply_data(param);
    }

    match program.to_cbor() {
        Ok(res) => Ok(res),
        Err(_) => Err(Error::ApplyParamsError),
    }
}

pub fn iter_redeemers(
    redeemers: &Redeemers,
) -> impl Iterator<Item = (RedeemersKey, &PlutusData, ExUnits)> {
    match redeemers {
        Redeemers::List(rs) => Box::new(rs.iter().map(|r| {
            (
                RedeemersKey {
                    tag: r.tag,
                    index: r.index,
                },
                &r.data,
                r.ex_units,
            )
        })),
        Redeemers::Map(kv) => Box::new(kv.iter().map(|(k, v)| {
            (
                RedeemersKey {
                    tag: k.tag,
                    index: k.index,
                },
                &v.data,
                v.ex_units,
            )
        }))
            as Box<dyn Iterator<Item = (RedeemersKey, &PlutusData, ExUnits)>>,
    }
}
