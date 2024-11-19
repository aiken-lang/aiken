use super::{
    error::Error,
    script_context::{sort_voters, DataLookupTable, ResolvedInput, ScriptPurpose, ScriptVersion},
};
use crate::tx::script_context::sort_reward_accounts;
use itertools::Itertools;
use pallas_addresses::{Address, ScriptHash, ShelleyPaymentPart, StakePayload};
use pallas_codec::utils::Nullable;
use pallas_primitives::conway::{
    Certificate, GovAction, MintedTx, PolicyId, RedeemerTag, Redeemers, RedeemersKey,
    RewardAccount, StakeCredential, TransactionOutput, Voter,
};
use std::collections::HashMap;

type ScriptsNeeded = Vec<(ScriptPurpose, ScriptHash)>;

// subset of phase-1 ledger checks related to scripts
pub fn eval_phase_one(
    tx: &MintedTx,
    utxos: &[ResolvedInput],
    lookup_table: &DataLookupTable,
) -> Result<(), Error> {
    let scripts_needed = scripts_needed(tx, utxos)?;

    validate_missing_scripts(&scripts_needed, lookup_table.scripts())?;

    has_exact_set_of_redeemers(tx, &scripts_needed, lookup_table.scripts())?;

    Ok(())
}

pub fn validate_missing_scripts(
    needed: &ScriptsNeeded,
    txscripts: HashMap<ScriptHash, ScriptVersion>,
) -> Result<(), Error> {
    let received_hashes = txscripts.keys().copied().collect::<Vec<ScriptHash>>();

    let needed_hashes = needed.iter().map(|x| x.1).collect::<Vec<ScriptHash>>();

    let missing: Vec<_> = needed_hashes
        .clone()
        .into_iter()
        .filter(|x| !received_hashes.contains(x))
        .map(|x| x.to_string())
        .collect();

    let extra: Vec<_> = received_hashes
        .into_iter()
        .filter(|x| !needed_hashes.contains(x))
        .map(|x| x.to_string())
        .collect();

    if !missing.is_empty() || !extra.is_empty() {
        return Err(Error::RequiredRedeemersMismatch { missing, extra });
    }

    Ok(())
}

pub fn scripts_needed(tx: &MintedTx, utxos: &[ResolvedInput]) -> Result<ScriptsNeeded, Error> {
    let mut needed = Vec::new();

    let txb = tx.transaction_body.clone();

    let mut spend = Vec::new();

    for input in txb.inputs.iter() {
        let utxo = match utxos.iter().find(|utxo| utxo.input == *input) {
            Some(u) => u,
            None => return Err(Error::ResolvedInputNotFound(input.clone())),
        };

        let address = Address::from_bytes(match &utxo.output {
            TransactionOutput::Legacy(output) => output.address.as_ref(),
            TransactionOutput::PostAlonzo(output) => output.address.as_ref(),
        })?;

        if let Address::Shelley(a) = address {
            if let ShelleyPaymentPart::Script(h) = a.payment() {
                spend.push((ScriptPurpose::Spending(input.clone(), ()), *h));
            }
        }
    }

    let mut reward = txb
        .withdrawals
        .as_deref()
        .map(|w| {
            w.iter()
                .filter_map(|(acnt, _)| {
                    let address = Address::from_bytes(acnt).unwrap();

                    if let Address::Stake(a) = address {
                        if let StakePayload::Script(h) = a.payload() {
                            let cred = StakeCredential::ScriptHash(*h);
                            return Some((ScriptPurpose::Rewarding(cred), *h));
                        }
                    }

                    None
                })
                .collect::<ScriptsNeeded>()
        })
        .unwrap_or_default();

    let mut cert = txb
        .certificates
        .as_deref()
        .map(|m| {
            m.iter()
                .enumerate()
                .filter_map(|(ix, cert)| match cert {
                    Certificate::StakeDeregistration(StakeCredential::ScriptHash(h))
                    | Certificate::UnReg(StakeCredential::ScriptHash(h), _)
                    | Certificate::VoteDeleg(StakeCredential::ScriptHash(h), _)
                    | Certificate::VoteRegDeleg(StakeCredential::ScriptHash(h), _, _)
                    | Certificate::StakeVoteDeleg(StakeCredential::ScriptHash(h), _, _)
                    | Certificate::StakeRegDeleg(StakeCredential::ScriptHash(h), _, _)
                    | Certificate::StakeVoteRegDeleg(StakeCredential::ScriptHash(h), _, _, _)
                    | Certificate::RegDRepCert(StakeCredential::ScriptHash(h), _, _)
                    | Certificate::UnRegDRepCert(StakeCredential::ScriptHash(h), _)
                    | Certificate::UpdateDRepCert(StakeCredential::ScriptHash(h), _)
                    | Certificate::AuthCommitteeHot(StakeCredential::ScriptHash(h), _)
                    | Certificate::ResignCommitteeCold(StakeCredential::ScriptHash(h), _)
                    | Certificate::StakeDelegation(StakeCredential::ScriptHash(h), _) => {
                        Some((ScriptPurpose::Certifying(ix, cert.clone()), *h))
                    }

                    _ => None,
                })
                .collect::<ScriptsNeeded>()
        })
        .unwrap_or_default();

    let mut mint = txb
        .mint
        .as_deref()
        .map(|m| {
            m.iter()
                .map(|(policy_id, _)| (ScriptPurpose::Minting(*policy_id), *policy_id))
                .collect::<ScriptsNeeded>()
        })
        .unwrap_or_default();

    let mut propose = txb
        .proposal_procedures
        .as_deref()
        .map(|m| {
            m.iter()
                .enumerate()
                .filter_map(|(ix, procedure)| match procedure.gov_action {
                    GovAction::ParameterChange(_, _, Nullable::Some(hash)) => {
                        Some((ScriptPurpose::Proposing(ix, procedure.clone()), hash))
                    }
                    GovAction::TreasuryWithdrawals(_, Nullable::Some(hash)) => {
                        Some((ScriptPurpose::Proposing(ix, procedure.clone()), hash))
                    }
                    GovAction::HardForkInitiation(..)
                    | GovAction::Information
                    | GovAction::NewConstitution(..)
                    | GovAction::TreasuryWithdrawals(..)
                    | GovAction::ParameterChange(..)
                    | GovAction::NoConfidence(..)
                    | GovAction::UpdateCommittee(..) => None,
                })
                .collect::<ScriptsNeeded>()
        })
        .unwrap_or_default();

    let mut voting = txb
        .voting_procedures
        .as_deref()
        .map(|m| {
            m.iter()
                .filter_map(|(voter, _)| match voter {
                    Voter::ConstitutionalCommitteeScript(hash) | Voter::DRepScript(hash) => {
                        Some((ScriptPurpose::Voting(voter.clone()), *hash))
                    }
                    Voter::ConstitutionalCommitteeKey(_)
                    | Voter::DRepKey(_)
                    | Voter::StakePoolKey(_) => None,
                })
                .collect::<ScriptsNeeded>()
        })
        .unwrap_or_default();

    needed.append(&mut spend);
    needed.append(&mut reward);
    needed.append(&mut cert);
    needed.append(&mut mint);
    needed.append(&mut propose);
    needed.append(&mut voting);

    Ok(needed)
}

/// hasExactSetOfRedeemers in Ledger Spec, but we pass `txscripts` directly
pub fn has_exact_set_of_redeemers(
    tx: &MintedTx,
    needed: &ScriptsNeeded,
    tx_scripts: HashMap<ScriptHash, ScriptVersion>,
) -> Result<(), Error> {
    let mut redeemers_needed = Vec::new();

    for (script_purpose, script_hash) in needed {
        let redeemer_key = build_redeemer_key(tx, script_purpose)?;
        let script = tx_scripts.get(script_hash);

        if let (Some(key), Some(script)) = (redeemer_key, script) {
            match script {
                ScriptVersion::V1(_) => {
                    redeemers_needed.push((key, script_purpose.clone(), *script_hash))
                }
                ScriptVersion::V2(_) => {
                    redeemers_needed.push((key, script_purpose.clone(), *script_hash))
                }
                ScriptVersion::V3(_) => {
                    redeemers_needed.push((key, script_purpose.clone(), *script_hash))
                }
                ScriptVersion::Native(_) => (),
            }
        }
    }

    let wits_redeemer_keys: Vec<RedeemersKey> = tx
        .transaction_witness_set
        .redeemer
        .as_deref()
        .map(|m| match m {
            Redeemers::List(rs) => rs
                .iter()
                .map(|r| RedeemersKey {
                    index: r.index,
                    tag: r.tag,
                })
                .collect(),
            Redeemers::Map(kv) => kv
                .iter()
                .map(|(k, _)| RedeemersKey {
                    index: k.index,
                    tag: k.tag,
                })
                .collect(),
        })
        .unwrap_or_default();

    let needed_redeemer_keys: Vec<RedeemersKey> =
        redeemers_needed.iter().map(|x| x.0.clone()).collect();

    let missing: Vec<_> = redeemers_needed
        .into_iter()
        .filter(|x| !wits_redeemer_keys.contains(&x.0))
        .map(|x| {
            format!(
                "{}[{:?}] -> {}",
                redeemer_tag_to_string(&x.0.tag),
                x.0.index,
                x.2
            )
        })
        .collect();

    let extra: Vec<_> = wits_redeemer_keys
        .into_iter()
        .filter(|x| !needed_redeemer_keys.contains(x))
        .map(|x| format!("{}[{:?}]", redeemer_tag_to_string(&x.tag), x.index))
        .collect();

    if !missing.is_empty() || !extra.is_empty() {
        Err(Error::RequiredRedeemersMismatch { missing, extra })
    } else {
        Ok(())
    }
}

/// builds a redeemer pointer (tag, index) from a script purpose by setting the tag
/// according to the type of the script purpose, and the index according to the
/// placement of script purpose inside its container.
fn build_redeemer_key(
    tx: &MintedTx,
    script_purpose: &ScriptPurpose,
) -> Result<Option<RedeemersKey>, Error> {
    let tx_body = tx.transaction_body.clone();

    match script_purpose {
        ScriptPurpose::Minting(hash) => {
            let policy_ids: Vec<&PolicyId> = tx_body
                .mint
                .as_deref()
                .map(|m| m.iter().map(|(policy_id, _)| policy_id).sorted().collect())
                .unwrap_or_default();

            let redeemer_key =
                policy_ids
                    .iter()
                    .position(|x| x == &hash)
                    .map(|index| RedeemersKey {
                        tag: RedeemerTag::Mint,
                        index: index as u32,
                    });

            Ok(redeemer_key)
        }

        ScriptPurpose::Spending(txin, ()) => {
            let redeemer_key = tx_body
                .inputs
                .iter()
                .sorted_by(
                    |i_a, i_b| match i_a.transaction_id.cmp(&i_b.transaction_id) {
                        std::cmp::Ordering::Less => std::cmp::Ordering::Less,
                        std::cmp::Ordering::Equal => i_a.index.cmp(&i_b.index),
                        std::cmp::Ordering::Greater => std::cmp::Ordering::Greater,
                    },
                )
                .position(|x| x == txin)
                .map(|index| RedeemersKey {
                    tag: RedeemerTag::Spend,
                    index: index as u32,
                });

            Ok(redeemer_key)
        }

        ScriptPurpose::Rewarding(racnt) => {
            let mut reward_accounts: Vec<&RewardAccount> = tx_body
                .withdrawals
                .as_deref()
                .map(|m| m.iter().map(|(acnt, _)| acnt).collect())
                .unwrap_or_default();

            reward_accounts.sort_by(|acnt_a, acnt_b| sort_reward_accounts(acnt_a, acnt_b));

            let mut redeemer_key = None;

            for (idx, x) in reward_accounts.iter().enumerate() {
                let cred = match Address::from_bytes(x).unwrap() {
                    Address::Stake(a) => match a.payload() {
                        StakePayload::Script(sh) => Some(StakeCredential::ScriptHash(*sh)),
                        StakePayload::Stake(_) => None,
                    },
                    _ => return Err(Error::BadWithdrawalAddress),
                };

                if cred == Some(racnt.to_owned()) {
                    redeemer_key = Some(RedeemersKey {
                        tag: RedeemerTag::Reward,
                        index: idx as u32,
                    });
                }
            }

            Ok(redeemer_key)
        }

        ScriptPurpose::Certifying(_, d) => {
            let redeemer_key = tx_body
                .certificates
                .as_deref()
                .map(|m| m.iter().position(|x| x == d))
                .unwrap_or_default()
                .map(|index| RedeemersKey {
                    tag: RedeemerTag::Cert,
                    index: index as u32,
                });

            Ok(redeemer_key)
        }

        ScriptPurpose::Voting(v) => {
            let redeemer_key = tx_body
                .voting_procedures
                .as_deref()
                .map(|m| {
                    m.iter()
                        .sorted_by(|(a, _), (b, _)| sort_voters(a, b))
                        .position(|x| &x.0 == v)
                })
                .unwrap_or_default()
                .map(|index| RedeemersKey {
                    tag: RedeemerTag::Vote,
                    index: index as u32,
                });

            Ok(redeemer_key)
        }

        ScriptPurpose::Proposing(_, procedure) => {
            let redeemer_key = tx_body
                .proposal_procedures
                .as_deref()
                .map(|m| m.iter().position(|x| x == procedure))
                .unwrap_or_default()
                .map(|index| RedeemersKey {
                    tag: RedeemerTag::Propose,
                    index: index as u32,
                });

            Ok(redeemer_key)
        }
    }
}

pub fn redeemer_tag_to_string(redeemer_tag: &RedeemerTag) -> String {
    match redeemer_tag {
        RedeemerTag::Spend => "Spend",
        RedeemerTag::Mint => "Mint",
        RedeemerTag::Reward => "Withdraw",
        RedeemerTag::Cert => "Publish",
        RedeemerTag::Propose => "Propose",
        RedeemerTag::Vote => "Vote",
    }
    .to_string()
}
