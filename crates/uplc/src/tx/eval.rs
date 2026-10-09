use super::{
    Error,
    script_context::{ResolvedInput, SlotConfig, TxInInfo, TxInfo, find_script_cached},
    to_plutus_data::ToPlutusData,
};
use crate::{
    PlutusData,
    ast::{Data, FakeNamedDeBruijn, NamedDeBruijn, Program},
    machine::{cost_model::ExBudget, eval_result::EvalResult},
    tx::{
        phase_one::redeemer_tag_to_string,
        script_context::{DataLookupTable, PlutusScript, TxInfoV1, TxInfoV2, TxInfoV3},
    },
};
use pallas_primitives::conway::{CostModel, CostModels, ExUnits, Language, MintedTx, Redeemer};
use std::collections::HashMap;

pub fn eval_redeemer(
    tx: &MintedTx,
    utxos: &[ResolvedInput],
    slot_config: &SlotConfig,
    redeemer: &Redeemer,
    lookup_table: &DataLookupTable,
    cost_mdls_opt: Option<&CostModels>,
    initial_budget: &ExBudget,
) -> Result<(Redeemer, EvalResult), Error> {
    eval_redeemer_with_optional_protocol(
        tx,
        utxos,
        slot_config,
        redeemer,
        lookup_table,
        cost_mdls_opt,
        initial_budget,
        None,
    )
}

#[allow(clippy::too_many_arguments)]
pub fn eval_redeemer_with_protocol(
    tx: &MintedTx,
    utxos: &[ResolvedInput],
    slot_config: &SlotConfig,
    redeemer: &Redeemer,
    lookup_table: &DataLookupTable,
    cost_mdls_opt: Option<&CostModels>,
    initial_budget: &ExBudget,
    protocol_major_version: u16,
) -> Result<(Redeemer, EvalResult), Error> {
    eval_redeemer_with_optional_protocol(
        tx,
        utxos,
        slot_config,
        redeemer,
        lookup_table,
        cost_mdls_opt,
        initial_budget,
        Some(protocol_major_version),
    )
}

/// What the redeemers of one transaction share: its transaction info for each
/// Plutus version, already converted to Data, its resolved spent inputs and its
/// decoded scripts. Each is built on first use and then reused for every other
/// redeemer. The Data is shared, so each redeemer's script context holds it
/// without copying it.
///
/// The cache lives for a single evaluation of one transaction and is dropped
/// with it; nothing is kept across transactions. It holds at most one
/// transaction info per Plutus version, one resolved output per spent input and
/// one program per distinct script the transaction's redeemers run, so its size
/// is bounded by the transaction and its resolved inputs, which the caller
/// already holds in memory.
#[derive(Default)]
pub(crate) struct TxEvalCache {
    tx_infos: [Option<(TxInfo, Data)>; 3],
    spend_inputs: Option<Vec<TxInInfo>>,
    programs: HashMap<Vec<u8>, Program<NamedDeBruijn>>,
}

impl TxEvalCache {
    fn tx_info(
        &mut self,
        lang: &Language,
        tx: &MintedTx,
        utxos: &[ResolvedInput],
        slot_config: &SlotConfig,
    ) -> Result<&(TxInfo, Data), Error> {
        let slot = match lang {
            Language::PlutusV1 => 0,
            Language::PlutusV2 => 1,
            Language::PlutusV3 => 2,
        };

        if self.tx_infos[slot].is_none() {
            let tx_info = match lang {
                Language::PlutusV1 => TxInfoV1::from_transaction(tx, utxos, slot_config)?,
                Language::PlutusV2 => TxInfoV2::from_transaction(tx, utxos, slot_config)?,
                Language::PlutusV3 => TxInfoV3::from_transaction(tx, utxos, slot_config)?,
            };
            let data = tx_info.to_plutus_data().into();

            self.tx_infos[slot] = Some((tx_info, data));
        }

        Ok(self.tx_infos[slot].as_ref().unwrap())
    }

    fn program(&mut self, script: &[u8]) -> Result<Program<NamedDeBruijn>, Error> {
        if let Some(program) = self.programs.get(script) {
            return Ok(program.clone());
        }

        let mut buffer = Vec::new();
        let program: Program<NamedDeBruijn> =
            Program::<FakeNamedDeBruijn>::from_cbor(script, &mut buffer)?.into();

        self.programs.insert(script.to_vec(), program.clone());

        Ok(program)
    }
}

#[allow(clippy::too_many_arguments)]
fn eval_redeemer_with_optional_protocol(
    tx: &MintedTx,
    utxos: &[ResolvedInput],
    slot_config: &SlotConfig,
    redeemer: &Redeemer,
    lookup_table: &DataLookupTable,
    cost_mdls_opt: Option<&CostModels>,
    initial_budget: &ExBudget,
    protocol_major_version: Option<u16>,
) -> Result<(Redeemer, EvalResult), Error> {
    eval_redeemer_cached(
        tx,
        utxos,
        slot_config,
        redeemer,
        lookup_table,
        cost_mdls_opt,
        initial_budget,
        protocol_major_version,
        &mut TxEvalCache::default(),
    )
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn eval_redeemer_cached(
    tx: &MintedTx,
    utxos: &[ResolvedInput],
    slot_config: &SlotConfig,
    redeemer: &Redeemer,
    lookup_table: &DataLookupTable,
    cost_mdls_opt: Option<&CostModels>,
    initial_budget: &ExBudget,
    protocol_major_version: Option<u16>,
    cache: &mut TxEvalCache,
) -> Result<(Redeemer, EvalResult), Error> {
    #[allow(clippy::too_many_arguments)]
    fn do_eval_redeemer(
        cost_mdl_opt: Option<&CostModel>,
        initial_budget: &ExBudget,
        lang: &Language,
        protocol_major_version: Option<u16>,
        datum: Option<PlutusData>,
        redeemer: &Redeemer,
        (tx_info, tx_info_data): &(TxInfo, Data),
        program: Program<NamedDeBruijn>,
    ) -> Result<(Redeemer, EvalResult), Error> {
        let script_context = tx_info
            .script_context_data(tx_info_data, redeemer, datum.as_ref())
            .expect("couldn't create script context from transaction?");

        let program = match tx_info {
            TxInfo::V1(..) | TxInfo::V2(..) => if let Some(datum) = datum {
                program.apply_data(datum)
            } else {
                program
            }
            .apply_data(redeemer.data.clone())
            .apply_data(script_context),

            TxInfo::V3(..) => program.apply_data(script_context),
        };
        let eval_result = if let Some(costs) = cost_mdl_opt {
            if let Some(protocol_major_version) = protocol_major_version {
                program.eval_as_with_protocol(
                    lang,
                    protocol_major_version,
                    costs,
                    Some(initial_budget),
                )
            } else {
                program.eval_as(lang, costs, Some(initial_budget))
            }
        } else if let Some(protocol_major_version) = protocol_major_version {
            program.eval_version_with_protocol(ExBudget::default(), lang, protocol_major_version)
        } else {
            program.eval_version(ExBudget::default(), lang)
        };

        let cost = eval_result.cost();

        if let Err(err) = eval_result.result() {
            return Err(Error::Machine(err, cost, eval_result.traces()));
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

        Ok((new_redeemer, eval_result))
    }

    let (script, datum) =
        find_script_cached(redeemer, tx, utxos, lookup_table, &mut cache.spend_inputs)?;

    let (lang, script, cost_mdl) = match script {
        PlutusScript::V1(script) => (
            Language::PlutusV1,
            script.0,
            cost_mdls_opt
                .map(|cost_mdls| {
                    cost_mdls
                        .plutus_v1
                        .as_ref()
                        .ok_or(Error::CostModelNotFound(Language::PlutusV1))
                })
                .transpose()?,
        ),
        PlutusScript::V2(script) => (
            Language::PlutusV2,
            script.0,
            cost_mdls_opt
                .map(|cost_mdls| {
                    cost_mdls
                        .plutus_v2
                        .as_ref()
                        .ok_or(Error::CostModelNotFound(Language::PlutusV2))
                })
                .transpose()?,
        ),
        PlutusScript::V3(script) => (
            Language::PlutusV3,
            script.0,
            cost_mdls_opt
                .map(|cost_mdls| {
                    cost_mdls
                        .plutus_v3
                        .as_ref()
                        .ok_or(Error::CostModelNotFound(Language::PlutusV3))
                })
                .transpose()?,
        ),
    };

    // Built before the script is decoded, so that errors come in the same order
    // as when nothing is cached.
    cache.tx_info(&lang, tx, utxos, slot_config)?;
    let program = cache.program(&script)?;
    let tx_info = cache.tx_info(&lang, tx, utxos, slot_config)?;

    do_eval_redeemer(
        cost_mdl,
        initial_budget,
        &lang,
        protocol_major_version,
        datum,
        redeemer,
        tx_info,
        program,
    )
    .map_err(|err| Error::RedeemerError {
        tag: redeemer_tag_to_string(&redeemer.tag),
        index: redeemer.index,
        err: Box::new(err),
    })
}
