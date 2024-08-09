use super::{to_plutus_data::MintValue, Error};
use itertools::Itertools;
use pallas_addresses::{Address, StakePayload};
use pallas_codec::utils::{KeyValuePairs, NonEmptyKeyValuePairs, NonEmptySet};
use pallas_crypto::hash::Hash;
use pallas_primitives::{
    alonzo,
    conway::{
        AddrKeyhash, Certificate, Coin, DatumHash, DatumOption, Mint, MintedTransactionBody,
        MintedTransactionOutput, MintedTx, MintedWitnessSet, NativeScript, PlutusData,
        PlutusV1Script, PlutusV2Script, PlutusV3Script, PolicyId, PostAlonzoTransactionOutput,
        PseudoDatumOption, PseudoScript, Redeemer, RedeemerTag, RedeemersKey, RequiredSigners,
        RewardAccount, ScriptHash, StakeCredential, TransactionInput, TransactionOutput, Value,
    },
};
use pallas_traverse::{ComputeHash, OriginalHash};
use std::{cmp::Ordering, collections::HashMap, ops::Deref};

#[derive(Debug, PartialEq, Clone)]
pub struct ResolvedInput {
    pub input: TransactionInput,
    pub output: TransactionOutput,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TxInInfo {
    pub out_ref: TransactionInput,
    pub resolved: TransactionOutput,
}

pub fn output_address(output: &TransactionOutput) -> Address {
    match output {
        TransactionOutput::Legacy(x) => Address::from_bytes(&x.address).unwrap(),
        TransactionOutput::PostAlonzo(x) => Address::from_bytes(&x.address).unwrap(),
    }
}

pub fn output_datum(output: &TransactionOutput) -> Option<DatumOption> {
    match output {
        TransactionOutput::Legacy(x) => x.datum_hash.map(DatumOption::Hash),
        TransactionOutput::PostAlonzo(x) => x.datum_option.clone(),
    }
}

/// The ScriptPurpose is part of the ScriptContext is the case of Plutus V1 and V2.
/// It is superseded by the ScriptInfo in PlutusV3.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ScriptInfo<T> {
    Minting(PolicyId),
    Spending(TransactionInput, T),
    Rewarding(StakeCredential),
    Certifying(Certificate),
}

pub type ScriptPurpose = ScriptInfo<()>;

impl ScriptPurpose {
    pub fn into_script_info<T>(self, datum: T) -> ScriptInfo<T> {
        match self {
            Self::Minting(policy_id) => ScriptInfo::Minting(policy_id),
            Self::Spending(transaction_output, ()) => {
                ScriptInfo::Spending(transaction_output, datum)
            }
            Self::Rewarding(stake_credential) => ScriptInfo::Rewarding(stake_credential),
            Self::Certifying(certificate) => ScriptInfo::Certifying(certificate),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ScriptVersion {
    Native(NativeScript),
    V1(PlutusV1Script),
    V2(PlutusV2Script),
    V3(PlutusV3Script),
}

pub struct DataLookupTable {
    datum: HashMap<DatumHash, PlutusData>,
    scripts: HashMap<ScriptHash, ScriptVersion>,
}

impl DataLookupTable {
    pub fn from_transaction(tx: &MintedTx, utxos: &[ResolvedInput]) -> DataLookupTable {
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
                                scripts
                                    .insert(ns.compute_hash(), ScriptVersion::Native(ns.clone()));
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
}

impl DataLookupTable {
    pub fn scripts(&self) -> HashMap<ScriptHash, ScriptVersion> {
        self.scripts.clone()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TxInfoV1 {
    pub inputs: Vec<TxInInfo>,
    pub outputs: Vec<TransactionOutput>,
    pub fee: Value,
    pub mint: MintValue,
    pub certificates: Vec<Certificate>,
    pub withdrawals: Vec<(Address, Coin)>,
    pub valid_range: TimeRange,
    pub signatories: Vec<AddrKeyhash>,
    pub data: Vec<(DatumHash, PlutusData)>,
    pub redeemers: KeyValuePairs<ScriptPurpose, Redeemer>,
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

        let inputs = get_tx_in_info_v1(&tx.transaction_body.inputs, utxos)?;
        let certificates = get_certificates_info(&tx.transaction_body.certificates);
        let withdrawals =
            KeyValuePairs::from(get_withdrawal_info(&tx.transaction_body.withdrawals));
        let mint = get_mint_info(&tx.transaction_body.mint);

        let redeemers = get_redeemers_info(
            &tx.transaction_witness_set,
            script_purpose_builder(&inputs[..], &mint, &certificates, &withdrawals),
        )?;

        Ok(TxInfo::V1(TxInfoV1 {
            inputs,
            outputs: get_outputs_info(&tx.transaction_body.outputs[..]),
            fee: Value::Coin(get_fee_info(&tx.transaction_body.fee)),
            mint,
            certificates,
            withdrawals: withdrawals.into(),
            valid_range: get_validity_range_info(&tx.transaction_body, slot_config),
            signatories: get_signatories_info(&tx.transaction_body.required_signers),
            data: get_data_info(&tx.transaction_witness_set),
            redeemers,
            id: tx.transaction_body.original_hash(),
        }))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TxInfoV2 {
    pub inputs: Vec<TxInInfo>,
    pub reference_inputs: Vec<TxInInfo>,
    pub outputs: Vec<TransactionOutput>,
    pub fee: Value,
    pub mint: MintValue,
    pub certificates: Vec<Certificate>,
    pub withdrawals: KeyValuePairs<Address, Coin>,
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
        let certificates = get_certificates_info(&tx.transaction_body.certificates);
        let withdrawals =
            KeyValuePairs::from(get_withdrawal_info(&tx.transaction_body.withdrawals));
        let mint = get_mint_info(&tx.transaction_body.mint);

        let redeemers = get_redeemers_info(
            &tx.transaction_witness_set,
            script_purpose_builder(&inputs[..], &mint, &certificates, &withdrawals),
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
            outputs: get_outputs_info(&tx.transaction_body.outputs[..]),
            fee: Value::Coin(get_fee_info(&tx.transaction_body.fee)),
            mint,
            certificates,
            withdrawals,
            valid_range: get_validity_range_info(&tx.transaction_body, slot_config),
            signatories: get_signatories_info(&tx.transaction_body.required_signers),
            data: KeyValuePairs::from(get_data_info(&tx.transaction_witness_set)),
            redeemers,
            id: tx.transaction_body.original_hash(),
        }))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TxInfoV3 {
    pub inputs: Vec<TxInInfo>,
    pub reference_inputs: Vec<TxInInfo>,
    pub outputs: Vec<TransactionOutput>,
    pub fee: Coin,
    pub mint: MintValue,
    pub certificates: Vec<Certificate>,
    pub withdrawals: KeyValuePairs<Address, Coin>,
    pub valid_range: TimeRange,
    pub signatories: Vec<AddrKeyhash>,
    pub redeemers: KeyValuePairs<ScriptPurpose, Redeemer>,
    pub data: KeyValuePairs<DatumHash, PlutusData>,
    // TODO:
    // votes : KeyValuePairs<Voter, KeyValuePairs<GovernanceActionId, Vote>>
    // proposalProcedures : Vec<ProposalProcedure>
    // currentTreasuryAmount : Option<Coin>
    // treasuryDonation : Option<Coin>
    pub id: Hash<32>,
}

impl TxInfoV3 {
    pub fn from_transaction(
        tx: &MintedTx,
        utxos: &[ResolvedInput],
        slot_config: &SlotConfig,
    ) -> Result<TxInfo, Error> {
        if let TxInfo::V2(tx_info_v2) = TxInfoV2::from_transaction(tx, utxos, slot_config)? {
            Ok(TxInfo::V3(TxInfoV3 {
                inputs: tx_info_v2.inputs,
                reference_inputs: tx_info_v2.reference_inputs,
                outputs: tx_info_v2.outputs,
                fee: get_fee_info(&tx.transaction_body.fee),
                mint: tx_info_v2.mint,
                certificates: tx_info_v2.certificates,
                withdrawals: tx_info_v2.withdrawals,
                valid_range: tx_info_v2.valid_range,
                signatories: tx_info_v2.signatories,
                redeemers: tx_info_v2.redeemers,
                data: tx_info_v2.data,
                id: tx_info_v2.id,
            }))
        } else {
            unreachable!()
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TxInfo {
    V1(TxInfoV1),
    V2(TxInfoV2),
    V3(TxInfoV3),
}

impl TxInfo {
    pub fn into_script_context(
        self,
        redeemer: &Redeemer,
        datum: Option<&PlutusData>,
    ) -> Option<ScriptContext> {
        match self {
            TxInfo::V1(TxInfoV1 { ref redeemers, .. })
            | TxInfo::V2(TxInfoV2 { ref redeemers, .. }) => redeemers
                .iter()
                .find_map(move |(purpose, some_redeemer)| {
                    if redeemer == some_redeemer {
                        Some(purpose.clone())
                    } else {
                        None
                    }
                })
                .map(move |purpose| ScriptContext::V1V2 {
                    tx_info: self,
                    purpose: purpose.clone().into(),
                }),

            TxInfo::V3(TxInfoV3 { ref redeemers, .. }) => redeemers
                .iter()
                .find_map(move |(purpose, some_redeemer)| {
                    if redeemer == some_redeemer {
                        Some(purpose.clone())
                    } else {
                        None
                    }
                })
                .map(move |purpose| ScriptContext::V3 {
                    tx_info: self,
                    redeemer: redeemer.data.clone(),
                    purpose: purpose.clone().into_script_info(datum.cloned()),
                }),
        }
    }

    pub fn inputs(&self) -> &[TxInInfo] {
        match self {
            TxInfo::V1(info) => &info.inputs,
            TxInfo::V2(info) => &info.inputs,
            TxInfo::V3(info) => &info.inputs,
        }
    }

    pub fn mint(&self) -> &MintValue {
        match self {
            TxInfo::V1(info) => &info.mint,
            TxInfo::V2(info) => &info.mint,
            TxInfo::V3(info) => &info.mint,
        }
    }

    pub fn withdrawals(&self) -> &[(Address, Coin)] {
        match self {
            TxInfo::V1(info) => &info.withdrawals[..],
            TxInfo::V2(info) => &info.withdrawals[..],
            TxInfo::V3(info) => &info.withdrawals[..],
        }
    }

    pub fn certificates(&self) -> &[Certificate] {
        match self {
            TxInfo::V1(info) => &info.certificates[..],
            TxInfo::V2(info) => &info.certificates[..],
            TxInfo::V3(info) => &info.certificates[..],
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ScriptContext {
    V1V2 {
        tx_info: TxInfo,
        purpose: Box<ScriptPurpose>,
    },
    V3 {
        tx_info: TxInfo,
        redeemer: PlutusData,
        purpose: ScriptInfo<Option<PlutusData>>,
    },
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
                resolved: sort_tx_out_value(&utxo.output),
            })
        })
        .collect()
}

pub fn get_tx_in_info_v2(
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
                resolved: sort_tx_out_value(&utxo.output),
            })
        })
        .collect()
}

pub fn get_mint_info(mint: &Option<Mint>) -> MintValue {
    MintValue {
        mint_value: mint
            .as_ref()
            .map(sort_mint)
            .unwrap_or(NonEmptyKeyValuePairs::Indef(vec![])),
    }
}

pub fn get_outputs_info(outputs: &[MintedTransactionOutput]) -> Vec<TransactionOutput> {
    outputs
        .iter()
        .cloned()
        .map(|output| sort_tx_out_value(&output.into()))
        .collect()
}

pub fn get_fee_info(fee: &Coin) -> Coin {
    *fee
}

pub fn get_certificates_info(certificates: &Option<NonEmptySet<Certificate>>) -> Vec<Certificate> {
    certificates.clone().map(|s| s.to_vec()).unwrap_or_default()
}

pub fn get_withdrawal_info(
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

pub fn get_validity_range_info(
    body: &MintedTransactionBody,
    slot_config: &SlotConfig,
) -> TimeRange {
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

pub fn get_signatories_info(signers: &Option<RequiredSigners>) -> Vec<AddrKeyhash> {
    signers
        .as_deref()
        .map(|s| s.iter().cloned().sorted().collect())
        .unwrap_or_default()
}

pub fn get_data_info(witness_set: &MintedWitnessSet) -> Vec<(DatumHash, PlutusData)> {
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

pub fn get_redeemers_info<'a>(
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
    certificates: &'a [Certificate],
    withdrawals: &'a KeyValuePairs<Address, Coin>,
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
                .map(|i| ScriptPurpose::Spending(i.out_ref, ())),
            RedeemerTag::Cert => certificates
                .get(index)
                .cloned()
                .map(ScriptPurpose::Certifying),
            RedeemerTag::Reward => withdrawals
                .get(index)
                .cloned()
                .map(|(address, _)| match address {
                    Address::Stake(stake_address) => match stake_address.payload() {
                        StakePayload::Script(script_hash) => Ok(ScriptPurpose::Rewarding(
                            StakeCredential::Scripthash(*script_hash),
                        )),
                        StakePayload::Stake(_) => Err(Error::NonScriptWithdrawal),
                    },
                    _ => Err(Error::BadWithdrawalAddress),
                })
                .transpose()?,
            tag => todo!("get_script_purpose for {tag:?}"),
        }
        .ok_or(Error::ExtraneousRedeemer)
    }
}

pub fn find_script(
    redeemer: &Redeemer,
    tx: &MintedTx,
    utxos: &[ResolvedInput],
    lookup_table: &DataLookupTable,
) -> Result<(ScriptVersion, Option<PlutusData>), Error> {
    let lookup_script = |script_hash: &ScriptHash| match lookup_table.scripts.get(script_hash) {
        Some(s) => Ok((s.clone(), None)),
        None => Err(Error::MissingRequiredScript {
            hash: script_hash.to_string(),
        }),
    };

    let lookup_datum = |datum: Option<DatumOption>| match datum {
        Some(DatumOption::Hash(hash)) => match lookup_table.datum.get(&hash) {
            Some(d) => Ok(d.clone()),
            None => Err(Error::MissingRequiredDatum {
                hash: hash.to_string(),
            }),
        },
        Some(DatumOption::Data(data)) => Ok(data.0.clone()),
        _ => Err(Error::MissingRequiredInlineDatumOrHash),
    };

    match redeemer.tag {
        RedeemerTag::Mint => get_mint_info(&tx.transaction_body.mint)
            .mint_value
            .get(redeemer.index as usize)
            .ok_or(Error::MissingScriptForRedeemer)
            .and_then(|(policy_id, _)| {
                let policy_id_array: [u8; 28] = policy_id.to_vec().try_into().unwrap();
                let hash = Hash::from(policy_id_array);
                lookup_script(&hash)
            }),

        RedeemerTag::Reward => get_withdrawal_info(&tx.transaction_body.withdrawals)
            .get(redeemer.index as usize)
            .ok_or(Error::MissingScriptForRedeemer)
            .and_then(|(addr, _)| {
                let stake_addr = if let Address::Stake(stake_addr) = addr {
                    stake_addr
                } else {
                    unreachable!("withdrawal always contains stake addresses")
                };

                if let StakePayload::Script(hash) = stake_addr.payload() {
                    lookup_script(hash)
                } else {
                    Err(Error::NonScriptWithdrawal)
                }
            }),

        RedeemerTag::Cert => get_certificates_info(&tx.transaction_body.certificates)
            .get(redeemer.index as usize)
            .ok_or(Error::MissingScriptForRedeemer)
            .and_then(|cert| match cert {
                Certificate::StakeDeregistration(stake_credential) => match stake_credential {
                    StakeCredential::Scripthash(hash) => Ok(hash),
                    _ => Err(Error::NonScriptStakeCredential),
                },
                Certificate::StakeDelegation(stake_credential, _) => match stake_credential {
                    StakeCredential::Scripthash(hash) => Ok(hash),
                    _ => Err(Error::NonScriptStakeCredential),
                },
                Certificate::PoolRetirement { .. } | Certificate::PoolRegistration { .. } => {
                    Err(Error::UnsupportedCertificateType)
                }
                _ => {
                    todo!("remaining certificate types")
                }
            })
            .and_then(lookup_script),

        RedeemerTag::Spend => get_tx_in_info_v2(&tx.transaction_body.inputs, utxos)
            .or_else(|err| {
                if matches!(err, Error::ByronAddressNotAllowed) {
                    get_tx_in_info_v1(&tx.transaction_body.inputs, utxos)
                } else {
                    Err(err)
                }
            })?
            .get(redeemer.index as usize)
            .ok_or(Error::MissingScriptForRedeemer)
            .and_then(|input| match output_address(&input.resolved) {
                Address::Shelley(shelley_address) => {
                    let hash = shelley_address.payment().as_hash();

                    let script = lookup_script(hash);

                    let datum = lookup_datum(output_datum(&input.resolved));

                    script.and_then(|(script, _)| Ok((script, Some(datum?))))
                }
                _ => Err(Error::NonScriptStakeCredential),
            }),

        RedeemerTag::Propose => todo!("find_script: RedeemerTag::Propose"),

        RedeemerTag::Vote => todo!("find_script: RedeemerTag::Vote"),
    }
}

pub fn from_alonzo_value(value: &alonzo::Value) -> Value {
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

pub fn from_alonzo_output(output: &alonzo::TransactionOutput) -> TransactionOutput {
    TransactionOutput::PostAlonzo(PostAlonzoTransactionOutput {
        address: output.address.clone(),
        value: from_alonzo_value(&output.amount),
        datum_option: output.datum_hash.map(DatumOption::Hash),
        script_ref: None,
    })
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

#[cfg(test)]
mod tests {
    use crate::{
        ast::Data,
        tx::{
            script_context::{TxInfo, TxInfoV3},
            to_plutus_data::ToPlutusData,
            ResolvedInput, SlotConfig,
        },
    };
    use pallas_primitives::{
        conway::{ExUnits, PlutusData, Redeemer, RedeemerTag, TransactionInput, TransactionOutput},
        Fragment,
    };
    use pallas_traverse::{Era, MultiEraTx};

    fn fixture_tx_info(transaction: &str, inputs: &str, outputs: &str) -> TxInfo {
        let transaction_bytes = hex::decode(transaction).unwrap();
        let inputs_bytes = hex::decode(inputs).unwrap();
        let outputs_bytes = hex::decode(outputs).unwrap();

        let inputs = Vec::<TransactionInput>::decode_fragment(inputs_bytes.as_slice()).unwrap();
        let outputs = Vec::<TransactionOutput>::decode_fragment(outputs_bytes.as_slice()).unwrap();
        let resolved_inputs: Vec<ResolvedInput> = inputs
            .iter()
            .zip(outputs.iter())
            .map(|(input, output)| ResolvedInput {
                input: input.clone(),
                output: output.clone(),
            })
            .collect();

        TxInfoV3::from_transaction(
            MultiEraTx::decode_for_era(Era::Conway, transaction_bytes.as_slice())
                .unwrap()
                .as_conway()
                .unwrap(),
            &resolved_inputs,
            &SlotConfig::default(),
        )
        .unwrap()
    }

    #[allow(dead_code)]
    fn from_haskell(data: &str) -> PlutusData {
        PlutusData::decode_fragment(hex::decode(data).unwrap().as_slice()).unwrap()
    }

    #[test]
    fn tx_to_plutus_data() {
        let datum = Some(Data::constr(0, Vec::new()));

        let redeemer = Redeemer {
            tag: RedeemerTag::Spend,
            index: 0,
            data: Data::constr(0, Vec::new()),
            ex_units: ExUnits {
                mem: 1000000,
                steps: 100000000,
            },
        };

        let script_context = fixture_tx_info(
            "84a7008182582000000000000000000000000000000000000000000000000000\
             0000000000000000018182581d60111111111111111111111111111111111111\
             111111111111111111111a3b9aca0002182a0b5820ffffffffffffffffffffff\
             ffffffffffffffffffffffffffffffffffffffffff0d81825820000000000000\
             0000000000000000000000000000000000000000000000000000001082581d60\
             000000000000000000000000000000000000000000000000000000001a3b9aca\
             001101a20581840000d87980821a000f42401a05f5e100078152510101003222\
             253330044a229309b2b2b9a1f5f6",
            "8182582000000000000000000000000000000000000000000000000000000000\
             0000000000",
            "81a300581d7039f47fd3b388ef53c48f08de24766d3e55dade6cae908cc24e0f\
             4f3e011a3b9aca00028201d81843d87980",
        )
        .into_script_context(&redeemer, datum.as_ref())
        .unwrap();

        // NOTE: The initial snapshot has been generated using the Haskell
        // implementation of the ledger library for that same serialized
        // transactions. It is meant to control that our construction of the
        // script context and its serialization matches exactly those
        // from the Haskell ledger / cardano node.
        insta::assert_debug_snapshot!(script_context.to_plutus_data())
    }
}
