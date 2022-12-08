use crate::{
    ast::{FakeNamedDeBruijn, NamedDeBruijn, Program},
    machine::cost_model::ExBudget,
    PlutusData,
};
use pallas_addresses::{Address, ScriptHash, StakePayload};
use pallas_codec::utils::{KeyValuePairs, MaybeIndefArray};
use pallas_crypto::hash::Hash;
use pallas_primitives::babbage::{
    Certificate, CostMdls, DatumHash, DatumOption, ExUnits, Language, Mint, MintedTx, NativeScript,
    PlutusV1Script, PlutusV2Script, PolicyId, Redeemer, RedeemerTag, RewardAccount, Script,
    StakeCredential, TransactionInput, TransactionOutput, Value, Withdrawals,
};
use pallas_traverse::{ComputeHash, OriginalHash};
use std::{collections::HashMap, convert::TryInto, ops::Deref, vec};

use super::{
    script_context::{
        ResolvedInput, ScriptContext, ScriptPurpose, SlotConfig, TimeRange, TxInInfo, TxInfo,
        TxInfoV1, TxInfoV2, TxOut,
    },
    to_plutus_data::{MintValue, ToPlutusData},
    Error,
};
use itertools::Itertools;

fn slot_to_begin_posix_time(slot: u64, sc: &SlotConfig) -> u64 {
    let ms_after_begin = (slot - sc.zero_slot) * sc.slot_length as u64;
    sc.zero_time + ms_after_begin
}

fn slot_range_to_posix_time_range(slot_range: TimeRange, sc: &SlotConfig) -> TimeRange {
    TimeRange {
        lower_bound: slot_range
            .lower_bound
            .map(|lower_bound| slot_to_begin_posix_time(lower_bound, sc)),
        upper_bound: slot_range
            .upper_bound
            .map(|upper_bound| slot_to_begin_posix_time(upper_bound, sc)),
    }
}

fn redeemer_tag_to_string(redeemer_tag: &RedeemerTag) -> String {
    match redeemer_tag {
        RedeemerTag::Spend => "Spend".to_string(),
        RedeemerTag::Mint => "Mint".to_string(),
        RedeemerTag::Cert => "Cert".to_string(),
        RedeemerTag::Reward => "Reward".to_string(),
    }
}

fn sort_mint(mint: &Mint) -> Mint {
    let mut mint_vec = vec![];

    for m in mint.deref().iter().sorted() {
        mint_vec.push((
            m.0,
            KeyValuePairs::Indef(m.1.deref().clone().into_iter().sorted().clone().collect()),
        ));
    }

    KeyValuePairs::Indef(mint_vec)
}

fn sort_value(value: &Value) -> Value {
    match value {
        Value::Coin(_) => value.clone(),
        Value::Multiasset(coin, ma) => {
            let mut ma_vec = vec![];

            for m in ma.deref().iter().sorted() {
                ma_vec.push((
                    m.0,
                    KeyValuePairs::Indef(
                        m.1.deref().clone().into_iter().sorted().clone().collect(),
                    ),
                ));
            }

            Value::Multiasset(*coin, KeyValuePairs::Indef(ma_vec))
        }
    }
}

fn sort_tx_out_value(tx_output: &TransactionOutput) -> TransactionOutput {
    match tx_output {
        TransactionOutput::Legacy(output) => {
            let mut new_output = output.clone();
            new_output.amount = sort_value(&output.amount);
            TransactionOutput::Legacy(new_output)
        }
        TransactionOutput::PostAlonzo(output) => {
            let mut new_output = output.clone();
            new_output.value = sort_value(&output.value);
            TransactionOutput::PostAlonzo(new_output)
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ScriptVersion {
    Native(NativeScript),
    V1(PlutusV1Script),
    V2(PlutusV2Script),
}

#[derive(Debug, PartialEq, Clone)]
enum ExecutionPurpose {
    WithDatum(ScriptVersion, PlutusData), // Spending
    NoDatum(ScriptVersion),               // Minting, Wdrl, DCert
}

pub struct DataLookupTable {
    datum: HashMap<DatumHash, PlutusData>,
    scripts: HashMap<ScriptHash, ScriptVersion>,
}

impl DataLookupTable {
    pub fn scripts(&self) -> HashMap<ScriptHash, ScriptVersion> {
        self.scripts.clone()
    }
}

pub fn get_tx_in_info_v1(
    inputs: &[TransactionInput],
    utxos: &[ResolvedInput],
) -> Result<Vec<TxInInfo>, Error> {
    inputs
        .iter()
        .sorted()
        .map(|input| {
            let utxo = match utxos.iter().find(|utxo| utxo.input == *input) {
                Some(resolved) => resolved,
                None => return Err(Error::ResolvedInputNotFound),
            };
            let address = Address::from_bytes(match &utxo.output {
                TransactionOutput::Legacy(output) => output.address.as_ref(),
                TransactionOutput::PostAlonzo(output) => output.address.as_ref(),
            })
            .unwrap();

            match address {
                Address::Byron(_) => {
                    return Err(Error::ByronAddressNotAllowed);
                }
                Address::Stake(_) => {
                    return Err(Error::NoPaymentCredential);
                }
                _ => {}
            };

            match &utxo.output {
                TransactionOutput::Legacy(_) => {}
                TransactionOutput::PostAlonzo(output) => {
                    if let Some(DatumOption::Data(_)) = output.datum_option {
                        return Err(Error::InlineDatumNotAllowed);
                    }

                    if output.script_ref.is_some() {
                        return Err(Error::ScriptAndInputRefNotAllowed);
                    }
                }
            }

            Ok(TxInInfo {
                out_ref: utxo.input.clone(),
                resolved: TxOut::V1(sort_tx_out_value(&utxo.output)),
            })
        })
        .collect()
}

fn get_tx_in_info_v2(
    inputs: &[TransactionInput],
    utxos: &[ResolvedInput],
) -> Result<Vec<TxInInfo>, Error> {
    inputs
        .iter()
        .sorted()
        .map(|input| {
            let utxo = match utxos.iter().find(|utxo| utxo.input == *input) {
                Some(resolved) => resolved,
                None => return Err(Error::ResolvedInputNotFound),
            };
            let address = Address::from_bytes(match &utxo.output {
                TransactionOutput::Legacy(output) => output.address.as_ref(),
                TransactionOutput::PostAlonzo(output) => output.address.as_ref(),
            })
            .unwrap();

            match address {
                Address::Byron(_) => {
                    return Err(Error::ByronAddressNotAllowed);
                }
                Address::Stake(_) => {
                    return Err(Error::NoPaymentCredential);
                }
                _ => {}
            };

            Ok(TxInInfo {
                out_ref: utxo.input.clone(),
                resolved: TxOut::V2(sort_tx_out_value(&utxo.output)),
            })
        })
        .collect()
}

fn get_script_purpose(
    redeemer: &Redeemer,
    inputs: &[TransactionInput],
    mint: &Option<Mint>,
    dcert: &Option<Vec<Certificate>>,
    wdrl: &Option<Withdrawals>,
) -> Result<ScriptPurpose, Error> {
    // sorting according to specs section 4.1: https://hydra.iohk.io/build/18583827/download/1/alonzo-changes.pdf
    let tag = redeemer.tag.clone();
    let index = redeemer.index;
    match tag {
        RedeemerTag::Mint => {
            // sort lexical by policy id
            let mut policy_ids = mint
                .as_ref()
                .unwrap_or(&KeyValuePairs::Indef(vec![]))
                .iter()
                .map(|(policy_id, _)| *policy_id)
                .collect::<Vec<PolicyId>>();
            policy_ids.sort();
            match policy_ids.get(index as usize) {
                Some(policy_id) => Ok(ScriptPurpose::Minting(*policy_id)),
                None => Err(Error::ExtraneousRedeemer),
            }
        }
        RedeemerTag::Spend => {
            // sort lexical by tx_hash and index
            let mut inputs = inputs.to_vec();
            inputs.sort();
            match inputs.get(index as usize) {
                Some(input) => Ok(ScriptPurpose::Spending(input.clone())),
                None => Err(Error::ExtraneousRedeemer),
            }
        }
        RedeemerTag::Reward => {
            // sort lexical by reward account
            let mut reward_accounts = wdrl
                .as_ref()
                .unwrap_or(&KeyValuePairs::Indef(vec![]))
                .iter()
                .map(|(racnt, _)| racnt.clone())
                .collect::<Vec<RewardAccount>>();
            reward_accounts.sort();
            let reward_account = match reward_accounts.get(index as usize) {
                Some(ra) => ra.clone(),
                None => return Err(Error::ExtraneousRedeemer),
            };
            let address = Address::from_bytes(&reward_account)?;
            let credential = match address {
                Address::Stake(stake_address) => match stake_address.payload() {
                    StakePayload::Script(script_hash) => StakeCredential::Scripthash(*script_hash),
                    StakePayload::Stake(_) => {
                        return Err(Error::ScriptKeyHash);
                    }
                },
                _ => return Err(Error::BadWithdrawalAddress),
            };
            Ok(ScriptPurpose::Rewarding(credential))
        }
        RedeemerTag::Cert => {
            // sort by order given in the tx (just take it as it is basically)
            match dcert
                .as_ref()
                .unwrap_or(&MaybeIndefArray::Indef(vec![]))
                .get(index as usize)
            {
                Some(cert) => Ok(ScriptPurpose::Certifying(cert.clone())),
                None => Err(Error::ExtraneousRedeemer),
            }
        }
    }
}

fn get_tx_info_v1(
    tx: &MintedTx,
    utxos: &[ResolvedInput],
    slot_config: &SlotConfig,
) -> Result<TxInfo, Error> {
    let body = tx.transaction_body.clone();

    if body.reference_inputs.is_some() {
        return Err(Error::ScriptAndInputRefNotAllowed);
    }

    let inputs = get_tx_in_info_v1(&body.inputs, utxos)?;

    let outputs = body
        .outputs
        .iter()
        .map(|output| TxOut::V1(sort_tx_out_value(output)))
        .collect();

    let fee = Value::Coin(body.fee);

    let mint = sort_mint(&body.mint.clone().unwrap_or(KeyValuePairs::Indef(vec![])));

    let dcert = body.certificates.clone().unwrap_or_default();

    let wdrl = body
        .withdrawals
        .clone()
        .unwrap_or(KeyValuePairs::Indef(vec![]))
        .deref()
        .clone()
        .into_iter()
        .sorted()
        .collect();

    let valid_range = slot_range_to_posix_time_range(
        TimeRange {
            lower_bound: body.validity_interval_start,
            upper_bound: body.ttl,
        },
        slot_config,
    );

    let signatories = body
        .required_signers
        .clone()
        .unwrap_or_default()
        .into_iter()
        .sorted()
        .collect();

    let data = tx
        .transaction_witness_set
        .plutus_data
        .as_ref()
        .unwrap_or(&vec![])
        .iter()
        .map(|d| (d.original_hash(), d.clone().unwrap()))
        .sorted()
        .collect();

    let id = tx.transaction_body.compute_hash();

    Ok(TxInfo::V1(TxInfoV1 {
        inputs,
        outputs,
        fee,
        mint: MintValue { mint_value: mint },
        dcert,
        wdrl,
        valid_range,
        signatories,
        data,
        id,
    }))
}

fn get_tx_info_v2(
    tx: &MintedTx,
    utxos: &[ResolvedInput],
    slot_config: &SlotConfig,
) -> Result<TxInfo, Error> {
    let body = tx.transaction_body.clone();

    let inputs = get_tx_in_info_v2(&body.inputs, utxos)?;

    let reference_inputs =
        get_tx_in_info_v2(&body.reference_inputs.clone().unwrap_or_default(), utxos)?;

    let outputs = body
        .outputs
        .iter()
        .map(|output| TxOut::V2(sort_tx_out_value(output)))
        .collect();

    let fee = Value::Coin(body.fee);

    let mint = sort_mint(&body.mint.clone().unwrap_or(KeyValuePairs::Indef(vec![])));

    let dcert = body.certificates.clone().unwrap_or_default();

    let wdrl = KeyValuePairs::Indef(
        body.withdrawals
            .clone()
            .unwrap_or(KeyValuePairs::Indef(vec![]))
            .deref()
            .clone()
            .into_iter()
            .sorted()
            .collect(),
    );

    let valid_range = slot_range_to_posix_time_range(
        TimeRange {
            lower_bound: body.validity_interval_start,
            upper_bound: body.ttl,
        },
        slot_config,
    );

    let signatories = body
        .required_signers
        .clone()
        .unwrap_or_default()
        .into_iter()
        .sorted()
        .collect();

    let redeemers = KeyValuePairs::Indef(
        tx.transaction_witness_set
            .redeemer
            .as_ref()
            .unwrap_or(&MaybeIndefArray::Indef(vec![]))
            .iter()
            .sorted_by(|a, b| a.data.compute_hash().cmp(&b.data.compute_hash()))
            .map(|r| {
                (
                    get_script_purpose(
                        r,
                        &tx.transaction_body.inputs,
                        &tx.transaction_body.mint,
                        &tx.transaction_body.certificates,
                        &tx.transaction_body.withdrawals,
                    )
                    .unwrap(),
                    r.clone(),
                )
            })
            .collect(),
    );

    let data = KeyValuePairs::Indef(
        tx.transaction_witness_set
            .plutus_data
            .as_ref()
            .unwrap_or(&vec![])
            .iter()
            .map(|d| (d.original_hash(), d.clone().unwrap()))
            .sorted()
            .collect(),
    );

    let id = tx.transaction_body.compute_hash();

    Ok(TxInfo::V2(TxInfoV2 {
        inputs,
        reference_inputs,
        outputs,
        fee,
        mint: MintValue { mint_value: mint },
        dcert,
        wdrl,
        valid_range,
        signatories,
        redeemers,
        data,
        id,
    }))
}

fn get_execution_purpose(
    utxos: &[ResolvedInput],
    script_purpose: &ScriptPurpose,
    lookup_table: &DataLookupTable,
) -> Result<ExecutionPurpose, Error> {
    match script_purpose {
        ScriptPurpose::Minting(policy_id) => {
            let policy_id_array: [u8; 28] = policy_id.to_vec().try_into().unwrap();
            let hash = Hash::from(policy_id_array);

            let script = match lookup_table.scripts.get(&hash) {
                Some(s) => s.clone(),
                None => {
                    return Err(Error::MissingRequiredScript {
                        hash: hash.to_string(),
                    })
                }
            };
            Ok(ExecutionPurpose::NoDatum(script))
        }
        ScriptPurpose::Spending(out_ref) => {
            let utxo = match utxos.iter().find(|utxo| utxo.input == *out_ref) {
                Some(resolved) => resolved,
                None => return Err(Error::ResolvedInputNotFound),
            };
            match &utxo.output {
                TransactionOutput::Legacy(output) => {
                    let address = Address::from_bytes(&output.address).unwrap();
                    match address {
                        Address::Shelley(shelley_address) => {
                            let hash = shelley_address.payment().as_hash();
                            let script = match lookup_table.scripts.get(hash) {
                                Some(s) => s.clone(),
                                None => {
                                    return Err(Error::MissingRequiredScript {
                                        hash: hash.to_string(),
                                    })
                                }
                            };

                            let datum_hash = match &output.datum_hash {
                                Some(hash) => hash,
                                None => return Err(Error::MissingRequiredInlineDatumOrHash),
                            };

                            let datum = match lookup_table.datum.get(datum_hash) {
                                Some(d) => d.clone(),
                                None => {
                                    return Err(Error::MissingRequiredDatum {
                                        hash: datum_hash.to_string(),
                                    })
                                }
                            };

                            Ok(ExecutionPurpose::WithDatum(script, datum))
                        }
                        _ => Err(Error::ScriptKeyHash),
                    }
                }
                TransactionOutput::PostAlonzo(output) => {
                    let address = Address::from_bytes(&output.address).unwrap();
                    match address {
                        Address::Shelley(shelley_address) => {
                            let hash = shelley_address.payment().as_hash();
                            let script = match lookup_table.scripts.get(hash) {
                                Some(s) => s.clone(),
                                None => {
                                    return Err(Error::MissingRequiredScript {
                                        hash: hash.to_string(),
                                    })
                                }
                            };

                            let datum = match &output.datum_option {
                                Some(DatumOption::Hash(hash)) => {
                                    match lookup_table.datum.get(hash) {
                                        Some(d) => d.clone(),
                                        None => {
                                            return Err(Error::MissingRequiredDatum {
                                                hash: hash.to_string(),
                                            })
                                        }
                                    }
                                }
                                Some(DatumOption::Data(data)) => data.0.clone(),
                                _ => return Err(Error::MissingRequiredInlineDatumOrHash),
                            };

                            Ok(ExecutionPurpose::WithDatum(script, datum))
                        }
                        _ => Err(Error::ScriptKeyHash),
                    }
                }
            }
        }
        ScriptPurpose::Rewarding(stake_credential) => {
            let script_hash = match stake_credential {
                StakeCredential::Scripthash(hash) => *hash,
                _ => return Err(Error::ScriptKeyHash),
            };

            let script = match lookup_table.scripts.get(&script_hash) {
                Some(s) => s.clone(),
                None => {
                    return Err(Error::MissingRequiredScript {
                        hash: script_hash.to_string(),
                    })
                }
            };

            Ok(ExecutionPurpose::NoDatum(script))
        }
        ScriptPurpose::Certifying(cert) => match cert {
            Certificate::StakeDeregistration(stake_credential) => {
                let script_hash = match stake_credential {
                    StakeCredential::Scripthash(hash) => *hash,
                    _ => return Err(Error::ScriptKeyHash),
                };

                let script = match lookup_table.scripts.get(&script_hash) {
                    Some(s) => s.clone(),
                    None => {
                        return Err(Error::MissingRequiredScript {
                            hash: script_hash.to_string(),
                        })
                    }
                };

                Ok(ExecutionPurpose::NoDatum(script))
            }
            Certificate::StakeDelegation(stake_credential, _) => {
                let script_hash = match stake_credential {
                    StakeCredential::Scripthash(hash) => *hash,
                    _ => return Err(Error::ScriptKeyHash),
                };

                let script = match lookup_table.scripts.get(&script_hash) {
                    Some(s) => s.clone(),
                    None => {
                        return Err(Error::MissingRequiredScript {
                            hash: script_hash.to_string(),
                        })
                    }
                };

                Ok(ExecutionPurpose::NoDatum(script))
            }
            _ => Err(Error::OnlyStakeDeregAndDelegAllowed),
        },
    }
}

pub fn get_script_and_datum_lookup_table(
    tx: &MintedTx,
    utxos: &[ResolvedInput],
) -> DataLookupTable {
    let mut datum = HashMap::new();
    let mut scripts = HashMap::new();

    // discovery in witness set

    let plutus_data_witnesses = tx
        .transaction_witness_set
        .plutus_data
        .clone()
        .unwrap_or_default();

    let scripts_native_witnesses = tx
        .transaction_witness_set
        .native_script
        .clone()
        .unwrap_or_default();

    let scripts_v1_witnesses = tx
        .transaction_witness_set
        .plutus_v1_script
        .clone()
        .unwrap_or_default();

    let scripts_v2_witnesses = tx
        .transaction_witness_set
        .plutus_v2_script
        .clone()
        .unwrap_or_default();

    for plutus_data in plutus_data_witnesses.iter() {
        datum.insert(plutus_data.original_hash(), plutus_data.clone().unwrap());
    }

    for script in scripts_native_witnesses.iter() {
        scripts.insert(script.compute_hash(), ScriptVersion::Native(script.clone()));
    }

    for script in scripts_v1_witnesses.iter() {
        scripts.insert(script.compute_hash(), ScriptVersion::V1(script.clone()));
    }

    for script in scripts_v2_witnesses.iter() {
        scripts.insert(script.compute_hash(), ScriptVersion::V2(script.clone()));
    }

    // discovery in utxos (script ref)

    for utxo in utxos.iter() {
        match &utxo.output {
            TransactionOutput::Legacy(_) => {}
            TransactionOutput::PostAlonzo(output) => {
                if let Some(script) = &output.script_ref {
                    match &script.0 {
                        Script::NativeScript(ns) => {
                            scripts.insert(ns.compute_hash(), ScriptVersion::Native(ns.clone()));
                        }
                        Script::PlutusV1Script(v1) => {
                            scripts.insert(v1.compute_hash(), ScriptVersion::V1(v1.clone()));
                        }
                        Script::PlutusV2Script(v2) => {
                            scripts.insert(v2.compute_hash(), ScriptVersion::V2(v2.clone()));
                        }
                    }
                }
            }
        }
    }

    DataLookupTable { datum, scripts }
}

pub fn eval_redeemer(
    tx: &MintedTx,
    utxos: &[ResolvedInput],
    slot_config: &SlotConfig,
    redeemer: &Redeemer,
    lookup_table: &DataLookupTable,
    cost_mdls_opt: Option<&CostMdls>,
    initial_budget: Option<&ExBudget>,
) -> Result<Redeemer, Error> {
    let result = || {
        let purpose = get_script_purpose(
            redeemer,
            &tx.transaction_body.inputs,
            &tx.transaction_body.mint,
            &tx.transaction_body.certificates,
            &tx.transaction_body.withdrawals,
        )?;

        let execution_purpose: ExecutionPurpose =
            get_execution_purpose(utxos, &purpose, lookup_table)?;

        match execution_purpose {
            ExecutionPurpose::WithDatum(script_version, datum) => match script_version {
                ScriptVersion::V1(script) => {
                    let tx_info = get_tx_info_v1(tx, utxos, slot_config)?;
                    let script_context = ScriptContext { tx_info, purpose };

                    let program: Program<NamedDeBruijn> = {
                        let mut buffer = Vec::new();

                        let prog = Program::<FakeNamedDeBruijn>::from_cbor(&script.0, &mut buffer)?;

                        prog.into()
                    };

                    let program = program
                        .apply_data(datum)
                        .apply_data(redeemer.data.clone())
                        .apply_data(script_context.to_plutus_data());

                    let (result, budget, logs) = if let Some(cost_mdls) = cost_mdls_opt {
                        let costs = if let Some(costs) = &cost_mdls.plutus_v1 {
                            costs
                        } else {
                            return Err(Error::V1CostModelNotFound);
                        };

                        program.eval_as(&Language::PlutusV1, costs, initial_budget)
                    } else {
                        program.eval_v1()
                    };

                    match result {
                        Ok(_) => (),
                        Err(err) => return Err(Error::Machine(err, budget, logs)),
                    }

                    let initial_budget = match initial_budget {
                        Some(b) => *b,
                        None => ExBudget::default(),
                    };

                    let new_redeemer = Redeemer {
                        tag: redeemer.tag.clone(),
                        index: redeemer.index,
                        data: redeemer.data.clone(),
                        ex_units: ExUnits {
                            mem: (initial_budget.mem - budget.mem) as u32,
                            steps: (initial_budget.cpu - budget.cpu) as u64,
                        },
                    };

                    Ok(new_redeemer)
                }
                ScriptVersion::V2(script) => {
                    let tx_info = get_tx_info_v2(tx, utxos, slot_config)?;
                    let script_context = ScriptContext { tx_info, purpose };

                    let program: Program<NamedDeBruijn> = {
                        let mut buffer = Vec::new();

                        let prog = Program::<FakeNamedDeBruijn>::from_cbor(&script.0, &mut buffer)?;

                        prog.into()
                    };

                    let program = program
                        .apply_data(datum)
                        .apply_data(redeemer.data.clone())
                        .apply_data(script_context.to_plutus_data());

                    let (result, budget, logs) = if let Some(cost_mdls) = cost_mdls_opt {
                        let costs = if let Some(costs) = &cost_mdls.plutus_v2 {
                            costs
                        } else {
                            return Err(Error::V2CostModelNotFound);
                        };

                        program.eval_as(&Language::PlutusV2, costs, initial_budget)
                    } else {
                        program.eval(ExBudget::default())
                    };

                    match result {
                        Ok(_) => (),
                        Err(err) => return Err(Error::Machine(err, budget, logs)),
                    }

                    let initial_budget = match initial_budget {
                        Some(b) => *b,
                        None => ExBudget::default(),
                    };

                    let new_redeemer = Redeemer {
                        tag: redeemer.tag.clone(),
                        index: redeemer.index,
                        data: redeemer.data.clone(),
                        ex_units: ExUnits {
                            mem: (initial_budget.mem - budget.mem) as u32,
                            steps: (initial_budget.cpu - budget.cpu) as u64,
                        },
                    };

                    Ok(new_redeemer)
                }
                ScriptVersion::Native(_) => Err(Error::NativeScriptPhaseTwo),
            },
            ExecutionPurpose::NoDatum(script_version) => match script_version {
                ScriptVersion::V1(script) => {
                    let tx_info = get_tx_info_v1(tx, utxos, slot_config)?;
                    let script_context = ScriptContext { tx_info, purpose };

                    let program: Program<NamedDeBruijn> = {
                        let mut buffer = Vec::new();

                        let prog = Program::<FakeNamedDeBruijn>::from_cbor(&script.0, &mut buffer)?;

                        prog.into()
                    };

                    let program = program
                        .apply_data(redeemer.data.clone())
                        .apply_data(script_context.to_plutus_data());

                    let (result, budget, logs) = if let Some(cost_mdls) = cost_mdls_opt {
                        let costs = if let Some(costs) = &cost_mdls.plutus_v1 {
                            costs
                        } else {
                            return Err(Error::V1CostModelNotFound);
                        };

                        program.eval_as(&Language::PlutusV1, costs, initial_budget)
                    } else {
                        program.eval_v1()
                    };

                    match result {
                        Ok(_) => (),
                        Err(err) => return Err(Error::Machine(err, budget, logs)),
                    }

                    let initial_budget = match initial_budget {
                        Some(b) => *b,
                        None => ExBudget::default(),
                    };

                    let new_redeemer = Redeemer {
                        tag: redeemer.tag.clone(),
                        index: redeemer.index,
                        data: redeemer.data.clone(),
                        ex_units: ExUnits {
                            mem: (initial_budget.mem - budget.mem) as u32,
                            steps: (initial_budget.cpu - budget.cpu) as u64,
                        },
                    };

                    Ok(new_redeemer)
                }
                ScriptVersion::V2(script) => {
                    let tx_info = get_tx_info_v2(tx, utxos, slot_config)?;
                    let script_context = ScriptContext { tx_info, purpose };

                    let program: Program<NamedDeBruijn> = {
                        let mut buffer = Vec::new();

                        let prog = Program::<FakeNamedDeBruijn>::from_cbor(&script.0, &mut buffer)?;

                        prog.into()
                    };

                    let program = program
                        .apply_data(redeemer.data.clone())
                        .apply_data(script_context.to_plutus_data());

                    let (result, budget, logs) = if let Some(cost_mdls) = cost_mdls_opt {
                        let costs = if let Some(costs) = &cost_mdls.plutus_v2 {
                            costs
                        } else {
                            return Err(Error::V2CostModelNotFound);
                        };

                        program.eval_as(&Language::PlutusV2, costs, initial_budget)
                    } else {
                        program.eval(ExBudget::default())
                    };

                    match result {
                        Ok(_) => (),
                        Err(err) => return Err(Error::Machine(err, budget, logs)),
                    }

                    let initial_budget = match initial_budget {
                        Some(b) => *b,
                        None => ExBudget::default(),
                    };

                    let new_redeemer = Redeemer {
                        tag: redeemer.tag.clone(),
                        index: redeemer.index,
                        data: redeemer.data.clone(),
                        ex_units: ExUnits {
                            mem: (initial_budget.mem - budget.mem) as u32,
                            steps: (initial_budget.cpu - budget.cpu) as u64,
                        },
                    };

                    Ok(new_redeemer)
                }
                ScriptVersion::Native(_) => Err(Error::NativeScriptPhaseTwo),
            },
        }
    };

    match result() {
        Ok(r) => Ok(r),
        Err(err) => Err(Error::RedeemerError {
            tag: redeemer_tag_to_string(&redeemer.tag),
            index: redeemer.index,
            err: Box::new(err),
        }),
    }
}
