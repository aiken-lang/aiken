use std::collections::HashMap;

use pallas_addresses::{ScriptHash, Address, ShelleyPaymentPart, StakePayload};
use pallas_codec::utils::{KeyValuePairs, MaybeIndefArray};
use pallas_primitives::babbage::{MintedTx, TransactionOutput, StakeCredential, Certificate, RedeemerTag, RewardAccount, PolicyId};

use super::{script_context::{ScriptPurpose, ResolvedInput}, eval::ScriptVersion};

// TODO: include in pallas eventually?
#[derive(Debug, PartialEq, Clone)]
struct RedeemerPtr {
    tag: RedeemerTag,
    index: u32,
}

type AlonzoScriptsNeeded = Vec<(ScriptPurpose, ScriptHash)>;

fn scripts_needed(
    tx: &MintedTx,
    utxos: &[ResolvedInput],
) -> anyhow::Result<AlonzoScriptsNeeded> {
    let mut needed = Vec::new();

    let txb = tx.transaction_body.clone();

    let mut spend = txb.inputs
        .iter()
        .map(|input| {
            let utxo = match utxos.iter().find(|utxo| utxo.input == *input) {
                Some(u) => u,
                None => panic!("Resolved input not found."),
            };
            let address = Address::from_bytes(match &utxo.output {
                TransactionOutput::Legacy(output) => output.address.as_ref(),
                TransactionOutput::PostAlonzo(output) => output.address.as_ref(),
            })
            .unwrap();

            if let Address::Shelley(a) = address {
                if let ShelleyPaymentPart::Script(h) = a.payment() {
                    return Some((ScriptPurpose::Spending(input.clone()), *h))
                }
            }

            None
        })
        .flatten()
        .collect::<AlonzoScriptsNeeded>();

    let mut reward = txb.withdrawals
        .as_ref()
        .unwrap_or(&KeyValuePairs::Indef(vec![]))
        .iter()
        .map(|(acnt, _)| {
            let address = Address::from_bytes(acnt).unwrap();

            if let Address::Stake(a) = address {
                if let StakePayload::Script(h) = a.payload() {
                    let cred = StakeCredential::Scripthash(*h);
                    return Some((ScriptPurpose::Rewarding(cred), *h))
                }
            }

            None
        })
        .flatten()
        .collect::<AlonzoScriptsNeeded>();

    let mut cert = txb.certificates
        .clone()
        .unwrap_or_default()
        .iter()
        .map(|cert| {
            // only Dereg and Deleg certs can require scripts
            match cert {
                Certificate::StakeDeregistration(StakeCredential::Scripthash(h)) => {
                    Some((ScriptPurpose::Certifying(cert.clone()), *h))
                },
                Certificate::StakeDelegation(StakeCredential::Scripthash(h), _) => {
                    Some((ScriptPurpose::Certifying(cert.clone()), *h))
                },
                _ => None
            }
        })
        .flatten()
        .collect::<AlonzoScriptsNeeded>();

    let mut mint = txb.mint
        .as_ref()
        .unwrap_or(&KeyValuePairs::Indef(vec![]))
        .iter()
        .map(|(policy_id, _)| {
            (ScriptPurpose::Minting(*policy_id), *policy_id)
        })
        .collect::<AlonzoScriptsNeeded>();

    needed.append(&mut spend);
    needed.append(&mut reward);
    needed.append(&mut cert);
    needed.append(&mut mint);

    Ok(needed)
}

/// hasExactSetOfRedeemers in Ledger Spec, but we pass `txscripts` directly
fn has_exact_set_of_redeemers(
    tx: &MintedTx,
    needed: AlonzoScriptsNeeded,
    txscripts: HashMap<ScriptHash, ScriptVersion>,
) -> anyhow::Result<()> {
    let redeemers_needed = needed
        .iter()
        .map(|(sp, sh)| {
            let rp = rdptr(tx, sp);
            let script = txscripts.get(&sh);

            match (rp, script) {
                (Some(ptr), Some(script)) => match script {
                    ScriptVersion::V1(_) => Some((ptr, sp.clone(), *sh)),
                    ScriptVersion::V2(_) => Some((ptr, sp.clone(), *sh)),
                    ScriptVersion::Native(_) => None,
                },
                _ => None
            }
        })
        .flatten()
        .collect::<Vec<(RedeemerPtr, ScriptPurpose, ScriptHash)>>();

    let wits_rdptrs = tx
        .transaction_witness_set
        .redeemer
        .as_ref()
        .unwrap_or(&MaybeIndefArray::Indef(vec![]))
        .iter()
        .map(|r| {
            RedeemerPtr { tag: r.tag.clone(), index: r.index }
        })
        .collect::<Vec<RedeemerPtr>>();

    let needed_rdptrs = redeemers_needed
        .iter()
        .map(|x| x.0.clone())
        .collect::<Vec<RedeemerPtr>>();

    let missing: Vec<_> = redeemers_needed
        .into_iter()
        .filter(|x| !wits_rdptrs.contains(&x.0))
        .map(|x| format!(
            "[Missing redeemer: rp: {:?}, sp: {:?}, sh: {}]",
            x.0,
            x.1,
            x.2.to_string(),
        ))
        .collect();

    let extra: Vec<_> = wits_rdptrs
        .into_iter()
        .filter(|x| !needed_rdptrs.contains(x))
        .map(|x| format!(
            "[Extra redeemer: rp: {:?}]",
            x
        ))
        .collect();

    if missing.len() > 0 || extra.len() > 0 {
        let missing_errors = missing.join(" ");
        let extra_errors = extra.join(" ");

        panic!("Mismatch in required redeemers: {} {}", missing_errors, extra_errors);
    }

    Ok(())
}

/// builds a redeemer pointer (tag, index) from a script purpose by setting the tag
/// according to the type of the script purpose, and the index according to the
/// placement of script purpose inside its container.
fn rdptr(
    tx: &MintedTx,
    sp: &ScriptPurpose,
) -> Option<RedeemerPtr> {
    let txb = tx.transaction_body.clone();

    match sp {
        ScriptPurpose::Minting(hash) => {
            let mut policy_ids = txb.mint
                .as_ref()
                .unwrap_or(&KeyValuePairs::Indef(vec![]))
                .iter()
                .map(|(policy_id, _)| *policy_id)
                .collect::<Vec<PolicyId>>();

            policy_ids.sort();

            let maybe_idx = policy_ids.iter().position(|x| x == hash);

            match maybe_idx {
                Some(idx) => Some(RedeemerPtr { tag: RedeemerTag::Mint, index: idx as u32 }),
                None => None,
            }
        }
        ScriptPurpose::Spending(txin) => {
            let mut inputs = txb.inputs.to_vec();
            inputs.sort_by(
                |i_a, i_b| match i_a.transaction_id.cmp(&i_b.transaction_id) {
                    std::cmp::Ordering::Less => std::cmp::Ordering::Less,
                    std::cmp::Ordering::Equal => i_a.index.cmp(&i_b.index),
                    std::cmp::Ordering::Greater => std::cmp::Ordering::Greater,
                },
            );

            let maybe_idx = inputs.iter().position(|x| x == txin);

            match maybe_idx {
                Some(idx) => Some(RedeemerPtr { tag: RedeemerTag::Spend, index: idx as u32 }),
                None => None,
            }
        },
        ScriptPurpose::Rewarding(racnt) => {
            let mut reward_accounts = txb.withdrawals
                .as_ref()
                .unwrap_or(&KeyValuePairs::Indef(vec![]))
                .iter()
                .map(|(acnt, _)| acnt.clone())
                .collect::<Vec<RewardAccount>>();

            reward_accounts.sort();

            let maybe_idx = reward_accounts.iter().position(|x| {
                let cred = match Address::from_bytes(x).unwrap() {
                    Address::Stake(a) => match a.payload() {
                        StakePayload::Script(sh) => {
                            StakeCredential::Scripthash(*sh)
                        }
                        StakePayload::Stake(_) => {
                            unreachable!(
                                "This is impossible. A key hash cannot be the hash of a script."
                            );
                        }
                    },
                    _ => unreachable!(
                        "This is impossible. Only shelley reward addresses can be a part of withdrawals."
                    ),
                };

                cred == *racnt
            });

            match maybe_idx {
                Some(idx) => Some(RedeemerPtr { tag: RedeemerTag::Reward, index: idx as u32 }),
                None => None,
            }
        },
        ScriptPurpose::Certifying(d) => {
            let maybe_idx = txb.certificates
                .as_ref()
                .unwrap_or(&MaybeIndefArray::Indef(vec![]))
                .iter()
                .position(|x| x == d);

            match maybe_idx {
                Some(idx) => Some(RedeemerPtr{ tag: RedeemerTag::Cert, index: idx as u32 }),
                None => None
            }
        },
    }
}
