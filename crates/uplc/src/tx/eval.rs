use super::{
    script_context::{find_script, ResolvedInput, ScriptContext, SlotConfig, TxInfo},
    to_plutus_data::ToPlutusData,
    Error,
};
use crate::{
    ast::{FakeNamedDeBruijn, NamedDeBruijn, Program},
    machine::cost_model::ExBudget,
    tx::{
        phase_one::redeemer_tag_to_string,
        script_context::{DataLookupTable, ScriptVersion, TxInfoV1, TxInfoV2, TxInfoV3},
    },
    PlutusData,
};
use pallas_codec::utils::Bytes;
use pallas_primitives::conway::{CostModel, CostModels, ExUnits, Language, MintedTx, Redeemer};

pub fn eval_redeemer(
    tx: &MintedTx,
    utxos: &[ResolvedInput],
    slot_config: &SlotConfig,
    redeemer: &Redeemer,
    lookup_table: &DataLookupTable,
    cost_mdls_opt: Option<&CostModels>,
    initial_budget: &ExBudget,
) -> Result<Redeemer, Error> {
    fn do_eval_redeemer(
        cost_mdl_opt: Option<&CostModel>,
        initial_budget: &ExBudget,
        lang: &Language,
        datum: Option<PlutusData>,
        redeemer: &Redeemer,
        tx_info: TxInfo,
        program: Program<NamedDeBruijn>,
    ) -> Result<Redeemer, Error> {
        let script_context = tx_info
            .into_script_context(redeemer, datum.as_ref())
            .expect("couldn't create script context from transaction?");

        let program = match script_context {
            ScriptContext::V1V2 { .. } => if let Some(datum) = datum {
                program.apply_data(datum)
            } else {
                program
            }
            .apply_data(redeemer.data.clone())
            .apply_data(script_context.to_plutus_data()),

            ScriptContext::V3 { .. } => program.apply_data(script_context.to_plutus_data()),
        };

        let mut eval_result = if let Some(costs) = cost_mdl_opt {
            program.eval_as(lang, costs, Some(initial_budget))
        } else {
            program.eval_version(ExBudget::default(), lang)
        };

        let cost = eval_result.cost();
        let logs = eval_result.logs();

        match eval_result.result() {
            Ok(_) => (),
            Err(err) => return Err(Error::Machine(err, cost, logs)),
        }

        let new_redeemer = Redeemer {
            tag: redeemer.tag,
            index: redeemer.index,
            data: redeemer.data.clone(),
            ex_units: ExUnits {
                mem: cost.mem as u64,
                steps: cost.cpu as u64,
            },
        };

        Ok(new_redeemer)
    }

    let program = |script: Bytes| {
        let mut buffer = Vec::new();
        Program::<FakeNamedDeBruijn>::from_cbor(&script, &mut buffer)
            .map(Into::<Program<NamedDeBruijn>>::into)
    };

    match find_script(redeemer, tx, utxos, lookup_table)? {
        (ScriptVersion::Native(_), _) => Err(Error::NativeScriptPhaseTwo),

        (ScriptVersion::V1(script), datum) => do_eval_redeemer(
            cost_mdls_opt
                .map(|cost_mdls| {
                    cost_mdls
                        .plutus_v1
                        .as_ref()
                        .ok_or(Error::CostModelNotFound(Language::PlutusV2))
                })
                .transpose()?,
            initial_budget,
            &Language::PlutusV1,
            datum,
            redeemer,
            TxInfoV1::from_transaction(tx, utxos, slot_config)?,
            program(script.0)?,
        ),

        (ScriptVersion::V2(script), datum) => do_eval_redeemer(
            cost_mdls_opt
                .map(|cost_mdls| {
                    cost_mdls
                        .plutus_v2
                        .as_ref()
                        .ok_or(Error::CostModelNotFound(Language::PlutusV2))
                })
                .transpose()?,
            initial_budget,
            &Language::PlutusV2,
            datum,
            redeemer,
            TxInfoV2::from_transaction(tx, utxos, slot_config)?,
            program(script.0)?,
        ),

        (ScriptVersion::V3(script), datum) => do_eval_redeemer(
            cost_mdls_opt
                .map(|cost_mdls| {
                    cost_mdls
                        .plutus_v3
                        .as_ref()
                        .ok_or(Error::CostModelNotFound(Language::PlutusV3))
                })
                .transpose()?,
            initial_budget,
            &Language::PlutusV3,
            datum,
            redeemer,
            TxInfoV3::from_transaction(tx, utxos, slot_config)?,
            program(script.0)?,
        ),
    }
    .map_err(|err| Error::RedeemerError {
        tag: redeemer_tag_to_string(&redeemer.tag),
        index: redeemer.index,
        err: Box::new(err),
    })
}
