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

/// Decoded scripts, keyed by their serialised bytes, so that a script is decoded
/// once and then reused by every redeemer that runs it.
///
/// Each evaluation of a transaction uses a fresh cache unless one is passed in,
/// for instance to [`super::eval_phase_two_with_script_cache`]. A cache that is
/// kept across transactions saves decoding the same scripts again; give it
/// limits with [`ScriptCache::with_limits`] so that it cannot grow without
/// bound. Evaluation results do not depend on what the cache holds.
#[derive(Debug, Default)]
pub struct ScriptCache {
    programs: HashMap<Vec<u8>, CachedProgram>,
    limits: Option<ScriptCacheLimits>,
    script_bytes: usize,
    clock: u64,
}

#[derive(Debug, Clone, Copy)]
struct ScriptCacheLimits {
    max_scripts: usize,
    max_script_bytes: usize,
}

#[derive(Debug)]
struct CachedProgram {
    program: Program<NamedDeBruijn>,
    last_used: u64,
}

impl ScriptCache {
    /// A cache that holds at most `max_scripts` scripts whose serialised sizes
    /// add up to at most `max_script_bytes`. Making room evicts the least
    /// recently used scripts first, and a script larger than `max_script_bytes`
    /// is never cached.
    ///
    /// A decoded script takes many times its serialised size in memory, so
    /// choose `max_script_bytes` with that in mind.
    pub fn with_limits(max_scripts: usize, max_script_bytes: usize) -> Self {
        ScriptCache {
            limits: Some(ScriptCacheLimits {
                max_scripts,
                max_script_bytes,
            }),
            ..ScriptCache::default()
        }
    }

    /// The number of scripts in the cache.
    pub fn len(&self) -> usize {
        self.programs.len()
    }

    pub fn is_empty(&self) -> bool {
        self.programs.is_empty()
    }

    /// Whether the cache holds the decoding of these serialised script bytes.
    pub fn contains(&self, script: &[u8]) -> bool {
        self.programs.contains_key(script)
    }

    pub fn clear(&mut self) {
        self.programs.clear();
        self.script_bytes = 0;
    }

    pub(crate) fn program(&mut self, script: &[u8]) -> Result<Program<NamedDeBruijn>, Error> {
        self.clock += 1;

        if let Some(cached) = self.programs.get_mut(script) {
            cached.last_used = self.clock;
            return Ok(cached.program.clone());
        }

        let mut buffer = Vec::new();
        let program: Program<NamedDeBruijn> =
            Program::<FakeNamedDeBruijn>::from_cbor(script, &mut buffer)?.into();

        if self.make_room(script.len()) {
            self.script_bytes += script.len();
            self.programs.insert(
                script.to_vec(),
                CachedProgram {
                    program: program.clone(),
                    last_used: self.clock,
                },
            );
        }

        Ok(program)
    }

    /// Evicts the least recently used scripts until one of `len` bytes fits, or
    /// returns false when it can never fit.
    fn make_room(&mut self, len: usize) -> bool {
        let Some(limits) = self.limits else {
            return true;
        };

        if limits.max_scripts == 0 || len > limits.max_script_bytes {
            return false;
        }

        while self.programs.len() >= limits.max_scripts
            || self.script_bytes + len > limits.max_script_bytes
        {
            let oldest = self
                .programs
                .iter()
                .min_by_key(|(_, cached)| cached.last_used)
                .map(|(script, _)| script.clone())
                .expect("a full cache holds at least one script");

            self.programs.remove(&oldest);
            self.script_bytes -= oldest.len();
        }

        true
    }
}

/// What the redeemers of one transaction share: its transaction info for each
/// Plutus version, already converted to Data, its resolved spent inputs and its
/// decoded scripts. Each is built on first use and then reused for every other
/// redeemer. The Data is shared, so each redeemer's script context holds it
/// without copying it.
///
/// The transaction info and spent inputs live for a single evaluation of one
/// transaction and are dropped with it. They hold at most one transaction info
/// per Plutus version and one resolved output per spent input, so their size is
/// bounded by the transaction and its resolved inputs, which the caller already
/// holds in memory. The scripts live as long as the [`ScriptCache`] they are in.
pub(crate) struct TxEvalCache<'a> {
    tx_infos: [Option<(TxInfo, Data)>; 3],
    spend_inputs: Option<Vec<TxInInfo>>,
    scripts: &'a mut ScriptCache,
}

impl<'a> TxEvalCache<'a> {
    pub(crate) fn new(scripts: &'a mut ScriptCache) -> Self {
        TxEvalCache {
            tx_infos: [None, None, None],
            spend_inputs: None,
            scripts,
        }
    }

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
        self.scripts.program(script)
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
        &mut TxEvalCache::new(&mut ScriptCache::default()),
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
    cache: &mut TxEvalCache<'_>,
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
