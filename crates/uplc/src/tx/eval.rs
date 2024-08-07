use super::{
    script_context::{ResolvedInput, ScriptContext, ScriptPurpose, SlotConfig, TxInfo},
    to_plutus_data::ToPlutusData,
    Error,
};
use crate::{
    ast::{FakeNamedDeBruijn, NamedDeBruijn, Program},
    machine::cost_model::ExBudget,
    tx::script_context::{TxInfoV1, TxInfoV2, TxInfoV3},
    PlutusData,
};
use pallas_addresses::{Address, ScriptHash, StakePayload};
use pallas_codec::utils::{Bytes, NonEmptyKeyValuePairs, NonEmptySet};
use pallas_crypto::hash::Hash;
use pallas_primitives::conway::{
    Certificate, CostMdls, CostModel, DatumHash, DatumOption, ExUnits, Language, Mint, MintedTx,
    NativeScript, PlutusV1Script, PlutusV2Script, PlutusV3Script, PolicyId, PseudoScript, Redeemer,
    RedeemerTag, RewardAccount, StakeCredential, TransactionInput, TransactionOutput, Withdrawals,
};
use pallas_traverse::{ComputeHash, OriginalHash};
use std::{collections::HashMap, convert::TryInto, vec};

fn redeemer_tag_to_string(redeemer_tag: &RedeemerTag) -> String {
    match redeemer_tag {
        RedeemerTag::Spend => "Spend".to_string(),
        RedeemerTag::Mint => "Mint".to_string(),
        RedeemerTag::Cert => "Cert".to_string(),
        RedeemerTag::Reward => "Reward".to_string(),
        tag => todo!("redeemer_tag_to_string for {tag:?}"),
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ScriptVersion {
    Native(NativeScript),
    V1(PlutusV1Script),
    V2(PlutusV2Script),
    V3(PlutusV3Script),
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

fn get_script_purpose(
    redeemer: &Redeemer,
    inputs: &[TransactionInput],
    mint: &Option<Mint>,
    dcert: &Option<NonEmptySet<Certificate>>,
    wdrl: &Option<Withdrawals>,
) -> Result<ScriptPurpose, Error> {
    // sorting according to specs section 4.1: https://hydra.iohk.io/build/18583827/download/1/alonzo-changes.pdf
    let tag = redeemer.tag;
    let index = redeemer.index;
    match tag {
        RedeemerTag::Mint => {
            // sort lexical by policy id
            let mut policy_ids = mint
                .as_ref()
                .unwrap_or(&NonEmptyKeyValuePairs::Indef(vec![]))
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
                .unwrap_or(&NonEmptyKeyValuePairs::Indef(vec![]))
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
            match dcert.as_deref().unwrap_or(&vec![]).get(index as usize) {
                Some(cert) => Ok(ScriptPurpose::Certifying(cert.clone())),
                None => Err(Error::ExtraneousRedeemer),
            }
        }
        tag => todo!("get_script_purpose for {tag:?}"),
    }
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
                    });
                }
            };
            Ok(ExecutionPurpose::NoDatum(script))
        }
        ScriptPurpose::Spending(out_ref) => {
            let utxo = match utxos.iter().find(|utxo| utxo.input == *out_ref) {
                Some(resolved) => resolved,
                None => return Err(Error::ResolvedInputNotFound(out_ref.clone())),
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
                                    });
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
                                    });
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
                                    });
                                }
                            };

                            let datum = match &output.datum_option {
                                Some(DatumOption::Hash(hash)) => {
                                    match lookup_table.datum.get(hash) {
                                        Some(d) => d.clone(),
                                        None => {
                                            return Err(Error::MissingRequiredDatum {
                                                hash: hash.to_string(),
                                            });
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
                    });
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
                        });
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
                        });
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
        .map(|s| s.to_vec())
        .unwrap_or_default();

    let scripts_native_witnesses = tx
        .transaction_witness_set
        .native_script
        .clone()
        .map(|s| s.to_vec())
        .unwrap_or_default();

    let scripts_v1_witnesses = tx
        .transaction_witness_set
        .plutus_v1_script
        .clone()
        .map(|s| s.to_vec())
        .unwrap_or_default();

    let scripts_v2_witnesses = tx
        .transaction_witness_set
        .plutus_v2_script
        .clone()
        .map(|s| s.to_vec())
        .unwrap_or_default();

    let scripts_v3_witnesses = tx
        .transaction_witness_set
        .plutus_v3_script
        .clone()
        .map(|s| s.to_vec())
        .unwrap_or_default();

    for plutus_data in plutus_data_witnesses.iter() {
        datum.insert(plutus_data.original_hash(), plutus_data.clone().unwrap());
    }

    for script in scripts_native_witnesses.iter() {
        scripts.insert(
            script.compute_hash(),
            ScriptVersion::Native(script.clone().unwrap()),
        );
    }

    for script in scripts_v1_witnesses.iter() {
        scripts.insert(script.compute_hash(), ScriptVersion::V1(script.clone()));
    }

    for script in scripts_v2_witnesses.iter() {
        scripts.insert(script.compute_hash(), ScriptVersion::V2(script.clone()));
    }

    for script in scripts_v3_witnesses.iter() {
        scripts.insert(script.compute_hash(), ScriptVersion::V3(script.clone()));
    }

    // discovery in utxos (script ref)

    for utxo in utxos.iter() {
        match &utxo.output {
            TransactionOutput::Legacy(_) => {}
            TransactionOutput::PostAlonzo(output) => {
                if let Some(script) = &output.script_ref {
                    match &script.0 {
                        PseudoScript::NativeScript(ns) => {
                            scripts.insert(ns.compute_hash(), ScriptVersion::Native(ns.clone()));
                        }
                        PseudoScript::PlutusV1Script(v1) => {
                            scripts.insert(v1.compute_hash(), ScriptVersion::V1(v1.clone()));
                        }
                        PseudoScript::PlutusV2Script(v2) => {
                            scripts.insert(v2.compute_hash(), ScriptVersion::V2(v2.clone()));
                        }
                        PseudoScript::PlutusV3Script(v3) => {
                            scripts.insert(v3.compute_hash(), ScriptVersion::V3(v3.clone()));
                        }
                    }
                }
            }
        }
    }

    DataLookupTable { datum, scripts }
}

fn mk_redeemer_with_datum(
    cost_mdl_opt: Option<&CostModel>,
    initial_budget: &ExBudget,
    lang: &Language,
    datum: PlutusData,
    (redeemer, purpose): (&Redeemer, ScriptPurpose),
    tx_info: TxInfo,
    program: Program<NamedDeBruijn>,
) -> Result<Redeemer, Error> {
    let script_context = ScriptContext { tx_info, purpose };

    let program = program
        .apply_data(datum)
        .apply_data(redeemer.data.clone())
        .apply_data(script_context.to_plutus_data());

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

pub fn eval_redeemer(
    tx: &MintedTx,
    utxos: &[ResolvedInput],
    slot_config: &SlotConfig,
    redeemer: &Redeemer,
    lookup_table: &DataLookupTable,
    cost_mdls_opt: Option<&CostMdls>,
    initial_budget: &ExBudget,
) -> Result<Redeemer, Error> {
    let result = || {
        let purpose = get_script_purpose(
            redeemer,
            &tx.transaction_body.inputs,
            &tx.transaction_body.mint,
            &tx.transaction_body.certificates,
            &tx.transaction_body.withdrawals,
        )?;

        let program = |script: Bytes| {
            let mut buffer = Vec::new();
            Program::<FakeNamedDeBruijn>::from_cbor(&script, &mut buffer)
                .map(Into::<Program<NamedDeBruijn>>::into)
        };

        let execution_purpose: ExecutionPurpose =
            get_execution_purpose(utxos, &purpose, lookup_table)?;

        match execution_purpose {
            ExecutionPurpose::WithDatum(script_version, datum) => match script_version {
                ScriptVersion::V1(script) => mk_redeemer_with_datum(
                    cost_mdls_opt
                        .map(|cost_mdls| {
                            cost_mdls
                                .plutus_v1
                                .as_ref()
                                .ok_or(Error::CostModelNotFound(Language::PlutusV1))
                        })
                        .transpose()?,
                    initial_budget,
                    &Language::PlutusV1,
                    datum,
                    (redeemer, purpose),
                    TxInfoV1::from_transaction(tx, utxos, slot_config)?,
                    program(script.0)?,
                ),

                ScriptVersion::V2(script) => mk_redeemer_with_datum(
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
                    (redeemer, purpose),
                    TxInfoV2::from_transaction(tx, utxos, slot_config)?,
                    program(script.0)?,
                ),

                ScriptVersion::V3(script) => mk_redeemer_with_datum(
                    cost_mdls_opt
                        .map(|cost_mdls| {
                            cost_mdls
                                .plutus_v3
                                .as_ref()
                                .ok_or(Error::CostModelNotFound(Language::PlutusV3))
                        })
                        .transpose()?,
                    initial_budget,
                    &Language::PlutusV2,
                    datum,
                    (redeemer, purpose),
                    TxInfoV3::from_transaction(tx, utxos, slot_config)?,
                    program(script.0)?,
                ),

                ScriptVersion::Native(_) => Err(Error::NativeScriptPhaseTwo),
            },
            ExecutionPurpose::NoDatum(script_version) => match script_version {
                ScriptVersion::V1(script) => {
                    let tx_info = TxInfoV1::from_transaction(tx, utxos, slot_config)?;
                    let script_context = ScriptContext { tx_info, purpose };

                    let program: Program<NamedDeBruijn> = {
                        let mut buffer = Vec::new();

                        let prog = Program::<FakeNamedDeBruijn>::from_cbor(&script.0, &mut buffer)?;

                        prog.into()
                    };

                    let program = program
                        .apply_data(redeemer.data.clone())
                        .apply_data(script_context.to_plutus_data());

                    let mut eval_result = if let Some(cost_mdls) = cost_mdls_opt {
                        let costs = if let Some(costs) = &cost_mdls.plutus_v1 {
                            costs
                        } else {
                            return Err(Error::CostModelNotFound(Language::PlutusV1));
                        };

                        program.eval_as(&Language::PlutusV1, costs, Some(initial_budget))
                    } else {
                        program.eval_version(ExBudget::default(), &Language::PlutusV1)
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
                ScriptVersion::V2(script) => {
                    let tx_info = TxInfoV2::from_transaction(tx, utxos, slot_config)?;
                    let script_context = ScriptContext { tx_info, purpose };

                    let program: Program<NamedDeBruijn> = {
                        let mut buffer = Vec::new();

                        let prog = Program::<FakeNamedDeBruijn>::from_cbor(&script.0, &mut buffer)?;

                        prog.into()
                    };

                    let program = program
                        .apply_data(redeemer.data.clone())
                        .apply_data(script_context.to_plutus_data());

                    let mut eval_result = if let Some(cost_mdls) = cost_mdls_opt {
                        let costs = if let Some(costs) = &cost_mdls.plutus_v2 {
                            costs
                        } else {
                            return Err(Error::CostModelNotFound(Language::PlutusV2));
                        };

                        program.eval_as(&Language::PlutusV2, costs, Some(initial_budget))
                    } else {
                        program.eval(ExBudget::default())
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
                ScriptVersion::V3(_script) => todo!(),
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
