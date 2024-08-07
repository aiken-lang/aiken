use super::{to_plutus_data::MintValue, Error};
use itertools::Itertools;
use pallas_addresses::{Address, StakePayload};
use pallas_codec::utils::{KeyValuePairs, NonEmptyKeyValuePairs, NonEmptySet};
use pallas_crypto::hash::Hash;
use pallas_primitives::{
    alonzo,
    conway::{
        AddrKeyhash, Certificate, Coin, DatumHash, DatumOption, Mint, MintedTransactionBody,
        MintedTransactionOutput, MintedTx, MintedWitnessSet, PlutusData, PolicyId,
        PostAlonzoTransactionOutput, PseudoDatumOption, Redeemer, RedeemerTag, RedeemersKey,
        RequiredSigners, RewardAccount, StakeCredential, TransactionInput, TransactionOutput,
        Value,
    },
};
use pallas_traverse::OriginalHash;
use std::{cmp::Ordering, ops::Deref};

#[derive(Debug, PartialEq, Clone)]
pub struct ResolvedInput {
    pub input: TransactionInput,
    pub output: TransactionOutput,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TxInInfo {
    pub out_ref: TransactionInput,
    pub resolved: TxOut,
}
#[derive(Debug, PartialEq, Clone)]
pub enum TxOut {
    V1(TransactionOutput),
    V2(TransactionOutput),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ScriptPurpose {
    Minting(PolicyId),
    Spending(TransactionInput),
    Rewarding(StakeCredential),
    Certifying(Certificate),
}

#[derive(Debug, PartialEq, Clone)]
pub struct TxInfoV1 {
    pub inputs: Vec<TxInInfo>,
    pub outputs: Vec<TxOut>,
    pub fee: Value,
    pub mint: MintValue,
    pub dcert: Vec<Certificate>,
    pub wdrl: Vec<(Address, Coin)>,
    pub valid_range: TimeRange,
    pub signatories: Vec<AddrKeyhash>,
    pub data: Vec<(DatumHash, PlutusData)>,
    pub id: Hash<32>,
}

impl TxInfoV1 {
    pub fn from_transaction(
        tx: &MintedTx,
        utxos: &[ResolvedInput],
        slot_config: &SlotConfig,
    ) -> Result<TxInfo, Error> {
        if tx.transaction_body.reference_inputs.is_some() {
            return Err(Error::ScriptAndInputRefNotAllowed);
        }

        Ok(TxInfo::V1(TxInfoV1 {
            inputs: get_tx_in_info_v1(&tx.transaction_body.inputs, utxos)?,
            outputs: get_outputs_info(TxOut::V1, &tx.transaction_body.outputs[..]),
            fee: get_fee_info(&tx.transaction_body.fee),
            mint: get_mint_info(&tx.transaction_body.mint),
            dcert: get_certificates_info(&tx.transaction_body.certificates),
            wdrl: get_withdrawal_info(&tx.transaction_body.withdrawals),
            valid_range: get_validity_range_info(&tx.transaction_body, slot_config),
            signatories: get_signatories_info(&tx.transaction_body.required_signers),
            data: get_data_info(&tx.transaction_witness_set),
            id: tx.transaction_body.original_hash(),
        }))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TxInfoV2 {
    pub inputs: Vec<TxInInfo>,
    pub reference_inputs: Vec<TxInInfo>,
    pub outputs: Vec<TxOut>,
    pub fee: Value,
    pub mint: MintValue,
    pub dcert: Vec<Certificate>,
    pub wdrl: KeyValuePairs<Address, Coin>,
    pub valid_range: TimeRange,
    pub signatories: Vec<AddrKeyhash>,
    pub redeemers: KeyValuePairs<ScriptPurpose, Redeemer>,
    pub data: KeyValuePairs<DatumHash, PlutusData>,
    pub id: Hash<32>,
}

impl TxInfoV2 {
    pub fn from_transaction(
        tx: &MintedTx,
        utxos: &[ResolvedInput],
        slot_config: &SlotConfig,
    ) -> Result<TxInfo, Error> {
        let inputs = get_tx_in_info_v2(&tx.transaction_body.inputs, utxos)?;
        let dcert = get_certificates_info(&tx.transaction_body.certificates);
        let wdrl = KeyValuePairs::from(get_withdrawal_info(&tx.transaction_body.withdrawals));
        let mint = get_mint_info(&tx.transaction_body.mint);

        let redeemers = get_redeemers_info(
            &tx.transaction_witness_set,
            script_purpose_builder(&inputs[..], &mint, &dcert, &wdrl),
        )?;

        let reference_inputs = tx
            .transaction_body
            .reference_inputs
            .clone()
            .map(|refs| get_tx_in_info_v2(&refs[..], utxos))
            .transpose()?
            .unwrap_or_default();

        Ok(TxInfo::V2(TxInfoV2 {
            inputs,
            reference_inputs,
            outputs: get_outputs_info(TxOut::V2, &tx.transaction_body.outputs[..]),
            fee: get_fee_info(&tx.transaction_body.fee),
            mint,
            dcert,
            wdrl,
            valid_range: get_validity_range_info(&tx.transaction_body, slot_config),
            signatories: get_signatories_info(&tx.transaction_body.required_signers),
            data: KeyValuePairs::from(get_data_info(&tx.transaction_witness_set)),
            redeemers,
            id: tx.transaction_body.original_hash(),
        }))
    }
}

pub struct TxInfoV3 {}

impl TxInfoV3 {
    pub fn from_transaction(
        _tx: &MintedTx,
        _utxos: &[ResolvedInput],
        _slot_config: &SlotConfig,
    ) -> Result<TxInfo, Error> {
        todo!("TxInfoV3")
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TxInfo {
    V1(TxInfoV1),
    V2(TxInfoV2),
}

#[derive(Debug, PartialEq, Clone)]
pub struct ScriptContext {
    pub tx_info: TxInfo,
    pub purpose: ScriptPurpose,
}

//---- Time conversion: slot range => posix time range
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TimeRange {
    pub lower_bound: Option<u64>,
    pub upper_bound: Option<u64>,
}

pub struct SlotConfig {
    pub slot_length: u32,
    pub zero_slot: u64,
    pub zero_time: u64,
}

impl Default for SlotConfig {
    fn default() -> Self {
        Self {
            slot_length: 1000,
            zero_slot: 4492800,
            zero_time: 1596059091000,
        }
    }
}

// --------------------- Translations

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
                None => return Err(Error::ResolvedInputNotFound(input.clone())),
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
                None => return Err(Error::ResolvedInputNotFound(input.clone())),
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

fn get_mint_info(mint: &Option<Mint>) -> MintValue {
    MintValue {
        mint_value: mint
            .as_ref()
            .map(sort_mint)
            .unwrap_or(NonEmptyKeyValuePairs::Indef(vec![])),
    }
}

fn get_outputs_info(
    to_tx_out: fn(TransactionOutput) -> TxOut,
    outputs: &[MintedTransactionOutput],
) -> Vec<TxOut> {
    outputs
        .iter()
        .cloned()
        .map(|output| to_tx_out(sort_tx_out_value(&output.into())))
        .collect()
}

fn get_fee_info(fee: &Coin) -> Value {
    Value::Coin(*fee)
}

fn get_certificates_info(certificates: &Option<NonEmptySet<Certificate>>) -> Vec<Certificate> {
    certificates.clone().map(|s| s.to_vec()).unwrap_or_default()
}

fn get_withdrawal_info(
    withdrawals: &Option<NonEmptyKeyValuePairs<RewardAccount, Coin>>,
) -> Vec<(Address, Coin)> {
    withdrawals
        .clone()
        .map(|w| {
            w.into_iter()
                .sorted()
                .map(|(reward_account, coin)| (Address::from_bytes(&reward_account).unwrap(), coin))
                .collect()
        })
        .unwrap_or_default()
}

fn get_validity_range_info(body: &MintedTransactionBody, slot_config: &SlotConfig) -> TimeRange {
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

    slot_range_to_posix_time_range(
        TimeRange {
            lower_bound: body.validity_interval_start,
            upper_bound: body.ttl,
        },
        slot_config,
    )
}

fn get_signatories_info(signers: &Option<RequiredSigners>) -> Vec<AddrKeyhash> {
    signers
        .as_deref()
        .map(|s| s.iter().cloned().sorted().collect())
        .unwrap_or_default()
}

fn get_data_info(witness_set: &MintedWitnessSet) -> Vec<(DatumHash, PlutusData)> {
    witness_set
        .plutus_data
        .as_deref()
        .map(|s| {
            s.iter()
                .cloned()
                .map(|d| (d.original_hash(), d.clone().unwrap()))
                .sorted()
                .collect()
        })
        .unwrap_or_default()
}

fn get_redeemers_info<'a>(
    witness_set: &'a MintedWitnessSet,
    to_script_purpose: impl Fn(&'a RedeemersKey) -> Result<ScriptPurpose, Error>,
) -> Result<KeyValuePairs<ScriptPurpose, Redeemer>, Error> {
    Ok(KeyValuePairs::from(
        witness_set
            .redeemer
            .as_deref()
            .map(|m| {
                m.iter()
                    .sorted_by(|a, b| sort_redeemers(&a.0, &b.0))
                    .map(|(redeemer_key, redeemer_value)| {
                        let redeemer = Redeemer {
                            tag: redeemer_key.tag,
                            index: redeemer_key.index,
                            data: redeemer_value.data.clone(),
                            ex_units: redeemer_value.ex_units,
                        };

                        to_script_purpose(redeemer_key).map(|purpose| (purpose, redeemer))
                    })
                    .collect::<Result<Vec<_>, _>>()
            })
            .transpose()?
            .unwrap_or_default(),
    ))
}

fn script_purpose_builder<'a>(
    inputs: &'a [TxInInfo],
    mint: &'a MintValue,
    dcert: &'a [Certificate],
    wdrl: &'a KeyValuePairs<Address, Coin>,
) -> impl Fn(&'a RedeemersKey) -> Result<ScriptPurpose, Error> {
    move |redeemer: &'a RedeemersKey| {
        let tag = redeemer.tag;
        let index = redeemer.index as usize;
        match tag {
            RedeemerTag::Mint => mint
                .mint_value
                .get(index)
                .map(|(policy_id, _)| ScriptPurpose::Minting(*policy_id)),
            RedeemerTag::Spend => inputs
                .get(index)
                .cloned()
                .map(|i| ScriptPurpose::Spending(i.out_ref)),
            RedeemerTag::Cert => dcert.get(index).cloned().map(ScriptPurpose::Certifying),
            RedeemerTag::Reward => wdrl
                .get(index)
                .cloned()
                .map(|(address, _)| match address {
                    Address::Stake(stake_address) => match stake_address.payload() {
                        StakePayload::Script(script_hash) => Ok(ScriptPurpose::Rewarding(
                            StakeCredential::Scripthash(*script_hash),
                        )),
                        StakePayload::Stake(_) => Err(Error::ScriptKeyHash),
                    },
                    _ => Err(Error::BadWithdrawalAddress),
                })
                .transpose()?,
            tag => todo!("get_script_purpose for {tag:?}"),
        }
        .ok_or(Error::ExtraneousRedeemer)
    }
}

fn from_alonzo_value(value: &alonzo::Value) -> Value {
    match value {
        alonzo::Value::Coin(coin) => Value::Coin(*coin),
        alonzo::Value::Multiasset(coin, assets) if assets.is_empty() => Value::Coin(*coin),
        alonzo::Value::Multiasset(coin, assets) => Value::Multiasset(
            *coin,
            NonEmptyKeyValuePairs::try_from(
                assets
                    .iter()
                    .cloned()
                    .map(|(policy_id, tokens)| {
                        (
                            policy_id,
                            NonEmptyKeyValuePairs::try_from(
                                tokens
                                    .iter()
                                    .cloned()
                                    .map(|(asset_name, quantity)| {
                                        (
                                            asset_name,
                                            quantity.try_into().expect("0 Ada in output value?"),
                                        )
                                    })
                                    .collect_vec(),
                            )
                            .expect("empty tokens under a policy?"),
                        )
                    })
                    .collect_vec(),
            )
            .expect("assets cannot be empty due to pattern-guard"),
        ),
    }
}

// --------------------- Sorting

fn sort_tx_out_value(tx_output: &TransactionOutput) -> TransactionOutput {
    match tx_output {
        TransactionOutput::Legacy(output) => {
            let new_output = PostAlonzoTransactionOutput {
                address: output.address.clone(),
                value: sort_value(&from_alonzo_value(&output.amount)),
                datum_option: output.datum_hash.map(PseudoDatumOption::Hash),
                script_ref: None,
            };
            TransactionOutput::PostAlonzo(new_output)
        }
        TransactionOutput::PostAlonzo(output) => {
            let mut new_output = output.clone();
            new_output.value = sort_value(&output.value);
            TransactionOutput::PostAlonzo(new_output)
        }
    }
}

fn sort_mint(mint: &Mint) -> Mint {
    let mut mint_vec = vec![];

    for m in mint.deref().iter().sorted() {
        mint_vec.push((
            m.0,
            NonEmptyKeyValuePairs::Indef(
                m.1.deref().clone().into_iter().sorted().clone().collect(),
            ),
        ));
    }

    NonEmptyKeyValuePairs::Indef(mint_vec)
}

fn sort_value(value: &Value) -> Value {
    match value {
        Value::Coin(_) => value.clone(),
        Value::Multiasset(coin, ma) => {
            let mut ma_vec = vec![];
            for m in ma.deref().iter().sorted() {
                ma_vec.push((
                    m.0,
                    NonEmptyKeyValuePairs::Indef(
                        m.1.deref().clone().into_iter().sorted().clone().collect(),
                    ),
                ));
            }
            Value::Multiasset(*coin, NonEmptyKeyValuePairs::Indef(ma_vec))
        }
    }
}

fn sort_redeemers(a: &RedeemersKey, b: &RedeemersKey) -> Ordering {
    fn redeemer_tag_as_usize(tag: &RedeemerTag) -> usize {
        match tag {
            RedeemerTag::Spend => 0,
            RedeemerTag::Mint => 1,
            RedeemerTag::Cert => 2,
            RedeemerTag::Reward => 3,
            RedeemerTag::Vote => 4,
            RedeemerTag::Propose => 5,
        }
    }

    if a.tag == b.tag {
        a.index.cmp(&b.index)
    } else {
        redeemer_tag_as_usize(&a.tag).cmp(&redeemer_tag_as_usize(&b.tag))
    }
}
