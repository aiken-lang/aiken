use super::{to_plutus_data::MintValue, Error};
use crate::tx::iter_redeemers;
use itertools::Itertools;
use pallas_addresses::{Address, Network, StakePayload};
use pallas_codec::utils::{
    Bytes, KeyValuePairs, NonEmptyKeyValuePairs, NonEmptySet, Nullable, PositiveCoin,
};
use pallas_crypto::hash::Hash;
use pallas_primitives::{
    alonzo,
    conway::{
        AddrKeyhash, Certificate, Coin, DatumHash, DatumOption, GovAction, GovActionId, Mint,
        MintedTransactionBody, MintedTransactionOutput, MintedTx, MintedWitnessSet, NativeScript,
        PlutusData, PlutusScript, PolicyId, PostAlonzoTransactionOutput, ProposalProcedure,
        PseudoDatumOption, PseudoScript, Redeemer, RedeemerTag, RedeemersKey, RequiredSigners,
        RewardAccount, ScriptHash, StakeCredential, TransactionInput, TransactionOutput, Value,
        Voter, VotingProcedure,
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
    Certifying(usize, Certificate),
    Voting(Voter),
    Proposing(usize, ProposalProcedure),
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
            Self::Certifying(ix, certificate) => ScriptInfo::Certifying(ix, certificate),
            Self::Voting(voter) => ScriptInfo::Voting(voter),
            Self::Proposing(ix, procedure) => ScriptInfo::Proposing(ix, procedure),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ScriptVersion {
    Native(NativeScript),
    V1(PlutusScript<1>),
    V2(PlutusScript<2>),
    V3(PlutusScript<3>),
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
            KeyValuePairs::from(get_withdrawals_info(&tx.transaction_body.withdrawals));
        let mint = get_mint_info(&tx.transaction_body.mint);

        let redeemers = get_redeemers_info(
            &tx.transaction_witness_set,
            script_purpose_builder(&inputs[..], &mint, &certificates, &withdrawals, &[], &[]),
        )?;

        Ok(TxInfo::V1(TxInfoV1 {
            inputs,
            outputs: get_outputs_info(&tx.transaction_body.outputs[..]),
            fee: Value::Coin(get_fee_info(&tx.transaction_body.fee)),
            mint,
            certificates,
            withdrawals: withdrawals.into(),
            valid_range: get_validity_range_info(&tx.transaction_body, slot_config)?,
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
            KeyValuePairs::from(get_withdrawals_info(&tx.transaction_body.withdrawals));
        let mint = get_mint_info(&tx.transaction_body.mint);

        let redeemers = get_redeemers_info(
            &tx.transaction_witness_set,
            script_purpose_builder(&inputs[..], &mint, &certificates, &withdrawals, &[], &[]),
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
            valid_range: get_validity_range_info(&tx.transaction_body, slot_config)?,
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
    pub id: Hash<32>,
    pub votes: KeyValuePairs<Voter, KeyValuePairs<GovActionId, VotingProcedure>>,
    pub proposal_procedures: Vec<ProposalProcedure>,
    pub current_treasury_amount: Option<Coin>,
    pub treasury_donation: Option<PositiveCoin>,
}

impl TxInfoV3 {
    pub fn from_transaction(
        tx: &MintedTx,
        utxos: &[ResolvedInput],
        slot_config: &SlotConfig,
    ) -> Result<TxInfo, Error> {
        let inputs = get_tx_in_info_v2(&tx.transaction_body.inputs, utxos)?;

        let certificates = get_certificates_info(&tx.transaction_body.certificates);

        let withdrawals =
            KeyValuePairs::from(get_withdrawals_info(&tx.transaction_body.withdrawals));

        let mint = get_mint_info(&tx.transaction_body.mint);

        let proposal_procedures =
            get_proposal_procedures_info(&tx.transaction_body.proposal_procedures);

        let votes = get_votes_info(&tx.transaction_body.voting_procedures);

        let redeemers = get_redeemers_info(
            &tx.transaction_witness_set,
            script_purpose_builder(
                &inputs[..],
                &mint,
                &certificates,
                &withdrawals,
                &proposal_procedures,
                &votes.iter().map(|(k, _v)| k).collect_vec()[..],
            ),
        )?;

        let reference_inputs = tx
            .transaction_body
            .reference_inputs
            .clone()
            .map(|refs| get_tx_in_info_v2(&refs[..], utxos))
            .transpose()?
            .unwrap_or_default();

        Ok(TxInfo::V3(TxInfoV3 {
            inputs,
            reference_inputs,
            outputs: get_outputs_info(&tx.transaction_body.outputs[..]),
            fee: get_fee_info(&tx.transaction_body.fee),
            mint,
            certificates,
            withdrawals,
            valid_range: get_validity_range_info(&tx.transaction_body, slot_config)?,
            signatories: get_signatories_info(&tx.transaction_body.required_signers),
            data: KeyValuePairs::from(get_data_info(&tx.transaction_witness_set)),
            redeemers,
            proposal_procedures,
            votes,
            current_treasury_amount: get_current_treasury_amount_info(
                &tx.transaction_body.treasury_value,
            ),
            treasury_donation: get_treasury_donation_info(&tx.transaction_body.donation),
            id: tx.transaction_body.original_hash(),
        }))
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
                    if redeemer.tag == some_redeemer.tag && redeemer.index == some_redeemer.index {
                        Some(purpose.clone())
                    } else {
                        None
                    }
                })
                .map(move |purpose| ScriptContext::V1V2 {
                    tx_info: self.into(),
                    purpose: purpose.clone().into(),
                }),

            TxInfo::V3(TxInfoV3 { ref redeemers, .. }) => redeemers
                .iter()
                .find_map(move |(purpose, some_redeemer)| {
                    if redeemer.tag == some_redeemer.tag && redeemer.index == some_redeemer.index {
                        Some(purpose.clone())
                    } else {
                        None
                    }
                })
                .map(move |purpose| ScriptContext::V3 {
                    tx_info: self.into(),
                    redeemer: redeemer.data.clone(),
                    purpose: purpose.clone().into_script_info(datum.cloned()).into(),
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
        tx_info: Box<TxInfo>,
        purpose: Box<ScriptPurpose>,
    },
    V3 {
        tx_info: Box<TxInfo>,
        redeemer: PlutusData,
        purpose: Box<ScriptInfo<Option<PlutusData>>>,
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

pub fn get_current_treasury_amount_info(amount: &Option<Coin>) -> Option<Coin> {
    *amount
}

pub fn get_treasury_donation_info(amount: &Option<PositiveCoin>) -> Option<PositiveCoin> {
    *amount
}

pub fn get_certificates_info(certificates: &Option<NonEmptySet<Certificate>>) -> Vec<Certificate> {
    certificates.clone().map(|s| s.to_vec()).unwrap_or_default()
}

pub fn get_proposal_procedures_info(
    proposal_procedures: &Option<NonEmptySet<ProposalProcedure>>,
) -> Vec<ProposalProcedure> {
    proposal_procedures
        .clone()
        .map(|s| s.to_vec())
        .unwrap_or_default()
}

pub fn get_withdrawals_info(
    withdrawals: &Option<NonEmptyKeyValuePairs<RewardAccount, Coin>>,
) -> Vec<(Address, Coin)> {
    withdrawals
        .clone()
        .map(|w| {
            w.into_iter()
                .sorted_by(|(accnt_a, _), (accnt_b, _)| sort_reward_accounts(accnt_a, accnt_b))
                .map(|(reward_account, coin)| (Address::from_bytes(&reward_account).unwrap(), coin))
                .collect()
        })
        .unwrap_or_default()
}

pub fn get_validity_range_info(
    body: &MintedTransactionBody,
    slot_config: &SlotConfig,
) -> Result<TimeRange, Error> {
    fn slot_to_begin_posix_time(slot: u64, sc: &SlotConfig) -> Result<u64, Error> {
        if slot < sc.zero_slot {
            return Err(Error::SlotTooFarInThePast {
                oldest_allowed: sc.zero_slot,
            });
        }
        let ms_after_begin = (slot - sc.zero_slot) * sc.slot_length as u64;
        Ok(sc.zero_time + ms_after_begin)
    }

    fn slot_range_to_posix_time_range(
        slot_range: TimeRange,
        sc: &SlotConfig,
    ) -> Result<TimeRange, Error> {
        Ok(TimeRange {
            lower_bound: slot_range
                .lower_bound
                .map(|lower_bound| slot_to_begin_posix_time(lower_bound, sc))
                .transpose()?,
            upper_bound: slot_range
                .upper_bound
                .map(|upper_bound| slot_to_begin_posix_time(upper_bound, sc))
                .transpose()?,
        })
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
    to_script_purpose: impl Fn(RedeemersKey) -> Result<ScriptPurpose, Error> + 'a,
) -> Result<KeyValuePairs<ScriptPurpose, Redeemer>, Error> {
    Ok(KeyValuePairs::from(
        witness_set
            .redeemer
            .as_deref()
            .map(|m| {
                iter_redeemers(m)
                    .sorted_by(|(a, _, _), (b, _, _)| sort_redeemers(a, b))
                    .map(|(key, data, ex_units)| {
                        let redeemer = Redeemer {
                            tag: key.tag,
                            index: key.index,
                            data: data.clone(),
                            ex_units,
                        };

                        to_script_purpose(key).map(|purpose| (purpose, redeemer))
                    })
                    .collect::<Result<Vec<_>, _>>()
            })
            .transpose()?
            .unwrap_or_default(),
    ))
}

pub fn get_votes_info(
    votes: &Option<
        NonEmptyKeyValuePairs<Voter, NonEmptyKeyValuePairs<GovActionId, VotingProcedure>>,
    >,
) -> KeyValuePairs<Voter, KeyValuePairs<GovActionId, VotingProcedure>> {
    KeyValuePairs::from(
        votes
            .as_deref()
            .map(|votes| {
                votes
                    .iter()
                    .sorted_by(|(a, _), (b, _)| sort_voters(a, b))
                    .cloned()
                    .map(|(voter, actions)| {
                        (
                            voter,
                            KeyValuePairs::from(
                                actions
                                    .iter()
                                    .sorted_by(|(a, _), (b, _)| sort_gov_action_id(a, b))
                                    .cloned()
                                    .collect::<Vec<_>>(),
                            ),
                        )
                    })
                    .collect_vec()
            })
            .unwrap_or_default(),
    )
}

fn script_purpose_builder<'a>(
    inputs: &'a [TxInInfo],
    mint: &'a MintValue,
    certificates: &'a [Certificate],
    withdrawals: &'a KeyValuePairs<Address, Coin>,
    proposal_procedures: &'a [ProposalProcedure],
    votes: &'a [&'a Voter],
) -> impl Fn(RedeemersKey) -> Result<ScriptPurpose, Error> + 'a {
    move |redeemer: RedeemersKey| {
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
                .map(|c| ScriptPurpose::Certifying(index, c)),

            RedeemerTag::Reward => withdrawals
                .get(index)
                .cloned()
                .map(|(address, _)| match address {
                    Address::Stake(stake_address) => match stake_address.payload() {
                        StakePayload::Script(script_hash) => Ok(ScriptPurpose::Rewarding(
                            StakeCredential::ScriptHash(*script_hash),
                        )),
                        StakePayload::Stake(_) => Err(Error::NonScriptWithdrawal),
                    },
                    _ => Err(Error::BadWithdrawalAddress),
                })
                .transpose()?,

            RedeemerTag::Vote => votes
                .get(index)
                .cloned()
                .cloned()
                .map(ScriptPurpose::Voting),

            RedeemerTag::Propose => proposal_procedures
                .get(index)
                .cloned()
                .map(|p| ScriptPurpose::Proposing(index, p)),
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
            Some(d) => Ok(Some(d.clone())),
            None => Err(Error::MissingRequiredDatum {
                hash: hash.to_string(),
            }),
        },
        Some(DatumOption::Data(data)) => Ok(Some(data.0.clone())),
        None => Ok(None),
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

        RedeemerTag::Reward => get_withdrawals_info(&tx.transaction_body.withdrawals)
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
                Certificate::StakeDeregistration(stake_credential)
                | Certificate::UnReg(stake_credential, _)
                | Certificate::VoteDeleg(stake_credential, _)
                | Certificate::VoteRegDeleg(stake_credential, _, _)
                | Certificate::StakeVoteDeleg(stake_credential, _, _)
                | Certificate::StakeRegDeleg(stake_credential, _, _)
                | Certificate::StakeVoteRegDeleg(stake_credential, _, _, _)
                | Certificate::RegDRepCert(stake_credential, _, _)
                | Certificate::UnRegDRepCert(stake_credential, _)
                | Certificate::UpdateDRepCert(stake_credential, _)
                | Certificate::AuthCommitteeHot(stake_credential, _)
                | Certificate::ResignCommitteeCold(stake_credential, _)
                | Certificate::StakeDelegation(stake_credential, _) => match stake_credential {
                    StakeCredential::ScriptHash(hash) => Ok(hash),
                    _ => Err(Error::NonScriptStakeCredential),
                },
                Certificate::StakeRegistration { .. }
                | Certificate::PoolRetirement { .. }
                | Certificate::Reg { .. }
                | Certificate::PoolRegistration { .. } => Err(Error::UnsupportedCertificateType),
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
                    let (script, _) = lookup_script(hash)?;
                    let datum = lookup_datum(output_datum(&input.resolved))?;

                    if datum.is_none()
                        && matches!(script, ScriptVersion::V1(..) | ScriptVersion::V2(..))
                    {
                        return Err(Error::MissingRequiredInlineDatumOrHash);
                    }

                    Ok((script, datum))
                }
                _ => Err(Error::NonScriptStakeCredential),
            }),

        RedeemerTag::Vote => get_votes_info(&tx.transaction_body.voting_procedures)
            .get(redeemer.index as usize)
            .ok_or(Error::MissingScriptForRedeemer)
            .and_then(|(voter, _)| match voter {
                Voter::ConstitutionalCommitteeScript(hash) => Ok(hash),
                Voter::ConstitutionalCommitteeKey(..) => Err(Error::NonScriptStakeCredential),
                Voter::DRepScript(hash) => Ok(hash),
                Voter::DRepKey(..) => Err(Error::NonScriptStakeCredential),
                Voter::StakePoolKey(..) => Err(Error::NonScriptStakeCredential),
            })
            .and_then(lookup_script),

        RedeemerTag::Propose => {
            get_proposal_procedures_info(&tx.transaction_body.proposal_procedures)
                .get(redeemer.index as usize)
                .ok_or(Error::MissingScriptForRedeemer)
                .and_then(|procedure| match procedure.gov_action {
                    GovAction::ParameterChange(_, _, Nullable::Some(ref hash)) => Ok(hash),
                    GovAction::TreasuryWithdrawals(_, Nullable::Some(ref hash)) => Ok(hash),
                    GovAction::HardForkInitiation(..)
                    | GovAction::Information
                    | GovAction::NewConstitution(..)
                    | GovAction::TreasuryWithdrawals(..)
                    | GovAction::ParameterChange(..)
                    | GovAction::NoConfidence(..)
                    | GovAction::UpdateCommittee(..) => Err(Error::NoGuardrailScriptForProcedure),
                })
                .and_then(lookup_script)
        }
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

pub fn sort_voters(a: &Voter, b: &Voter) -> Ordering {
    fn explode(voter: &Voter) -> (usize, &Hash<28>) {
        match voter {
            Voter::ConstitutionalCommitteeScript(hash) => (0, hash),
            Voter::ConstitutionalCommitteeKey(hash) => (1, hash),
            Voter::DRepScript(hash) => (2, hash),
            Voter::DRepKey(hash) => (3, hash),
            Voter::StakePoolKey(hash) => (4, hash),
        }
    }

    let (tag_a, hash_a) = explode(a);
    let (tag_b, hash_b) = explode(b);

    if tag_a == tag_b {
        hash_a.cmp(hash_b)
    } else {
        tag_a.cmp(&tag_b)
    }
}

fn sort_gov_action_id(a: &GovActionId, b: &GovActionId) -> Ordering {
    if a.transaction_id == b.transaction_id {
        a.action_index.cmp(&b.action_index)
    } else {
        a.transaction_id.cmp(&b.transaction_id)
    }
}

pub fn sort_reward_accounts(a: &Bytes, b: &Bytes) -> Ordering {
    let addr_a = Address::from_bytes(a).expect("invalid reward address in withdrawals.");
    let addr_b = Address::from_bytes(b).expect("invalid reward address in withdrawals.");

    fn network_tag(network: Network) -> u8 {
        match network {
            Network::Testnet => 0,
            Network::Mainnet => 1,
            Network::Other(tag) => tag,
        }
    }

    if let (Address::Stake(accnt_a), Address::Stake(accnt_b)) = (addr_a, addr_b) {
        if accnt_a.network() != accnt_b.network() {
            return network_tag(accnt_a.network()).cmp(&network_tag(accnt_b.network()));
        }

        match (accnt_a.payload(), accnt_b.payload()) {
            (StakePayload::Script(..), StakePayload::Stake(..)) => Ordering::Less,
            (StakePayload::Stake(..), StakePayload::Script(..)) => Ordering::Greater,
            (StakePayload::Script(hash_a), StakePayload::Script(hash_b)) => hash_a.cmp(hash_b),
            (StakePayload::Stake(hash_a), StakePayload::Stake(hash_b)) => hash_a.cmp(hash_b),
        }
    } else {
        unreachable!("invalid reward address in withdrawals.");
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
    fn script_context_simple_send() {
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

    #[test]
    fn script_context_mint() {
        let redeemer = Redeemer {
            tag: RedeemerTag::Mint,
            index: 1,
            data: Data::integer(42.into()),
            ex_units: ExUnits {
                mem: 1000000,
                steps: 100000000,
            },
        };

        let script_context = fixture_tx_info(
            "84a9008182582000000000000000000000000000000000000000000000000000\
             00000000000000000183a300581d600000000000000000000000000000000000\
             0000000000000000000000011a000f42400282005820923918e403bf43c34b4e\
             f6b48eb2ee04babed17320d8d1b9ff9ad086e86f44eca2005839000000000000\
             0000000000000000000000000000000000000000000000000000000000000000\
             0000000000000000000000000000000000000001821a000f4240a2581c12593b\
             4cbf7fdfd8636db99fe356437cd6af8539aadaa0a401964874a14474756e611b\
             00005af3107a4000581c0c8eaf490c53afbf27e3d84a3b57da51fbafe5aa7844\
             3fcec2dc262ea14561696b656e182aa300583910000000000000000000000000\
             0000000000000000000000000000000000000000000000000000000000000000\
             00000000000000000000000001821a000f4240a1581c0c8eaf490c53afbf27e3\
             d84a3b57da51fbafe5aa78443fcec2dc262ea14763617264616e6f0103d81847\
             82034463666f6f02182a09a2581c12593b4cbf7fdfd8636db99fe356437cd6af\
             8539aadaa0a401964874a14474756e611b00005af3107a4000581c0c8eaf490c\
             53afbf27e3d84a3b57da51fbafe5aa78443fcec2dc262ea24763617264616e6f\
             014561696b656e2d0b5820ffffffffffffffffffffffffffffffffffffffffff\
             ffffffffffffffffffffff0d8182582000000000000000000000000000000000\
             00000000000000000000000000000000001082581d6000000000000000000000\
             0000000000000000000000000000000000001a3b9aca00110112818258200000\
             00000000000000000000000000000000000000000000000000000000000000a3\
             0582840100d87980821a000f42401a05f5e100840101182a821a000f42401a05\
             f5e1000481d879800782587d587b010100323232323232322533333300800115\
             3330033370e900018029baa001153330073006375400224a6660089445261533\
             0054911856616c696461746f722072657475726e65642066616c736500136560\
             02002002002002002153300249010b5f746d70323a20566f696400165734ae71\
             55ceaab9e5573eae915895589301010032323232323232253333330080011533\
             30033370e900018029baa001153330073006375400224a666008a6600a920110\
             5f5f5f5f5f6d696e745f325f5f5f5f5f0014a22930a99802a4811856616c6964\
             61746f722072657475726e65642066616c736500136560020020020020020021\
             53300249010b5f746d70323a20566f696400165734ae7155ceaab9e5573eae91\
             f5f6",
            "8182582000000000000000000000000000000000000000000000000000000000\
             0000000000",
            "81a200581d600000000000000000000000000000000000000000000000000000\
             0000011a000f4240",
        )
        .into_script_context(&redeemer, None)
        .unwrap();

        // NOTE: The initial snapshot has been generated using the Haskell
        // implementation of the ledger library for that same serialized
        // transactions. It is meant to control that our construction of the
        // script context and its serialization matches exactly those
        // from the Haskell ledger / cardano node.
        insta::assert_debug_snapshot!(script_context.to_plutus_data());
    }

    #[test]
    fn script_context_propose_all_but_pparams() {
        let redeemer = Redeemer {
            tag: RedeemerTag::Propose,
            index: 3,
            data: Data::constr(0, vec![]),
            ex_units: ExUnits {
                mem: 1000000,
                steps: 100000000,
            },
        };

        let script_context = fixture_tx_info(
            "84a4008182582000000000000000000000000000000000000000000000000000\
             0000000000000000018002182a14d9010289841a001e8480581df00000000000\
             00000000000000000000000000000000000000000000008301f6820a00827668\
             747470733a2f2f61696b656e2d6c616e672e6f72675820000000000000000000\
             0000000000000000000000000000000000000000000000841a001e8480581df0\
             0000000000000000000000000000000000000000000000000000000083018258\
             2000000000000000000000000000000000000000000000000000000000000000\
             0000820b00827668747470733a2f2f61696b656e2d6c616e672e6f7267582000\
             0000000000000000000000000000000000000000000000000000000000000084\
             1a001e8480581df0000000000000000000000000000000000000000000000000\
             000000008302a1581de011111111111111111111111111111111111111111111\
             1111111111111a000f4240f6827668747470733a2f2f61696b656e2d6c616e67\
             2e6f726758200000000000000000000000000000000000000000000000000000\
             000000000000841a001e8480581df00000000000000000000000000000000000\
             00000000000000000000008302a1581de0222222222222222222222222222222\
             222222222222222222222222221a000f4240581c9b24324046544393443e1fb3\
             5c8b72c3c39e18a516a95df5f6654101827668747470733a2f2f61696b656e2d\
             6c616e672e6f7267582000000000000000000000000000000000000000000000\
             00000000000000000000841a001e8480581df000000000000000000000000000\
             0000000000000000000000000000008203f6827668747470733a2f2f61696b65\
             6e2d6c616e672e6f726758200000000000000000000000000000000000000000\
             000000000000000000000000841a001e8480581df00000000000000000000000\
             00000000000000000000000000000000008504f6818200581c00000000000000\
             000000000000000000000000000000000000000000a18200581c000000000000\
             000000000000000000000000000000000000000000001901f4d81e8201028276\
             68747470733a2f2f61696b656e2d6c616e672e6f726758200000000000000000\
             000000000000000000000000000000000000000000000000841a001e8480581d\
             f0000000000000000000000000000000000000000000000000000000008305f6\
             8282782068747470733a2f2f636f6e737469747574696f6e2e63617264616e6f\
             2e6f726758200000000000000000000000000000000000000000000000000000\
             000000000000f6827668747470733a2f2f61696b656e2d6c616e672e6f726758\
             2000000000000000000000000000000000000000000000000000000000000000\
             00841a001e8480581df000000000000000000000000000000000000000000000\
             0000000000008305f68282782068747470733a2f2f636f6e737469747574696f\
             6e2e63617264616e6f2e6f726758200000000000000000000000000000000000\
             000000000000000000000000000000581c000000000000000000000000000000\
             00000000000000000000000000827668747470733a2f2f61696b656e2d6c616e\
             672e6f7267582000000000000000000000000000000000000000000000000000\
             00000000000000841a001e8480581de000000000000000000000000000000000\
             0000000000000000000000008106827668747470733a2f2f61696b656e2d6c61\
             6e672e6f72675820000000000000000000000000000000000000000000000000\
             0000000000000000a20581840503d87980821a000f42401a05f5e1000781587d\
             587b0101003232323232323225333333008001153330033370e900018029baa0\
             01153330073006375400224a66600894452615330054911856616c696461746f\
             722072657475726e65642066616c736500136560020020020020020021533002\
             49010b5f746d70313a20566f696400165734ae7155ceaab9e5573eae91f5f6",
            "8182582000000000000000000000000000000000000000000000000000000000\
             0000000000",
            "81a200581d600000000000000000000000000000000000000000000000000000\
             0000011a000f4240",
        )
        .into_script_context(&redeemer, None)
        .unwrap();

        // NOTE: The initial snapshot has been generated using the Haskell
        // implementation of the ledger library for that same serialized
        // transactions. It is meant to control that our construction of the
        // script context and its serialization matches exactly those
        // from the Haskell ledger / cardano node.
        insta::assert_debug_snapshot!(script_context.to_plutus_data());
    }

    #[test]
    fn script_context_propose_pparams_no_cost_models() {
        let redeemer = Redeemer {
            tag: RedeemerTag::Propose,
            index: 0,
            data: Data::constr(0, vec![]),
            ex_units: ExUnits {
                mem: 1000000,
                steps: 100000000,
            },
        };

        let script_context = fixture_tx_info(
            "84a4008182582000000000000000000000000000000000000000000000000000\
             0000000000000000018002182a14d9010281841a001e8480581df00000000000\
             00000000000000000000000000000000000000000000008400f6b81d00182c01\
             1a00025ef50712081901f409d81e82030a0ad81e82031903e80bd81e82020a02\
             1a00016000031940000419044c051a001e8480061a1dcd650010190154111910\
             d61382d81e821902411903e8d81e821902d11a000f424014821a00d59f801b00\
             000002540be40015821a03b20b801b00000004a817c800161913881718961818\
             03181985d81e8218331864d81e8218341864d81e8218351864d81e8218361864\
             d81e8218371864181a8ad81e8218431864d81e8218431864d81e82183c1864d8\
             1e82184b1864d81e82183c1864d81e8218431864d81e8218431864d81e821843\
             1864d81e82184b1864d81e8218431864181b07181c1892181d06181e1b000000\
             174876e800181f1a1dcd65001820141821d81e820f01581c9b24324046544393\
             443e1fb35c8b72c3c39e18a516a95df5f6654101827668747470733a2f2f6169\
             6b656e2d6c616e672e6f72675820000000000000000000000000000000000000\
             0000000000000000000000000000a20581840500d87980821a000f42401a05f5\
             e1000781587d587b0101003232323232323225333333008001153330033370e9\
             00018029baa001153330073006375400224a6660089445261533005491185661\
             6c696461746f722072657475726e65642066616c736500136560020020020020\
             02002153300249010b5f746d70313a20566f696400165734ae7155ceaab9e557\
             3eae91f5f6",
            "8182582000000000000000000000000000000000000000000000000000000000\
             0000000000",
            "81a200581d600000000000000000000000000000000000000000000000000000\
             0000011a000f4240",
        )
        .into_script_context(&redeemer, None)
        .unwrap();

        // NOTE: The initial snapshot has been generated using the Haskell
        // implementation of the ledger library for that same serialized
        // transactions. It is meant to control that our construction of the
        // script context and its serialization matches exactly those
        // from the Haskell ledger / cardano node.
        insta::assert_debug_snapshot!(script_context.to_plutus_data());
    }

    #[test]
    fn script_context_certificates() {
        let redeemer = Redeemer {
            tag: RedeemerTag::Cert,
            index: 20,
            data: Data::constr(0, vec![]),
            ex_units: ExUnits {
                mem: 1000000,
                steps: 100000000,
            },
        };

        // NOTE: The transaction also contains treasury donation and current treasury amount
        let script_context = fixture_tx_info(
            "84a6008182582000000000000000000000000000000000000000000000000000\
             00000000000000000180049582008201581c2222222222222222222222222222\
             222222222222222222222222222282008200581c000000000000000000000000\
             0000000000000000000000000000000082018200581c00000000000000000000\
             0000000000000000000000000000000000008a03581c11111111111111111111\
             1111111111111111111111111111111111115820999999999999999999999999\
             99999999999999999999999999999999999999991a000f4240190154d81e8201\
             1864581de0000000000000000000000000000000000000000000000000000000\
             00d901028080f68304581c111111111111111111111111111111111111111111\
             1111111111111119053983078200581c00000000000000000000000000000000\
             0000000000000000000000001a002dc6c083088200581c000000000000000000\
             000000000000000000000000000000000000001a002dc6c083098200581c0000\
             00000000000000000000000000000000000000000000000000008200581c0000\
             000000000000000000000000000000000000000000000000000083098200581c\
             000000000000000000000000000000000000000000000000000000008201581c\
             0000000000000000000000000000000000000000000000000000000083098200\
             581c000000000000000000000000000000000000000000000000000000008102\
             83098200581c0000000000000000000000000000000000000000000000000000\
             00008103840a8200581c00000000000000000000000000000000000000000000\
             000000000000581c111111111111111111111111111111111111111111111111\
             111111118103840b8200581c0000000000000000000000000000000000000000\
             0000000000000000581c11111111111111111111111111111111111111111111\
             1111111111111a002dc6c0840c8200581c000000000000000000000000000000\
             0000000000000000000000000081031a002dc6c0850d8200581c000000000000\
             00000000000000000000000000000000000000000000581c1111111111111111\
             111111111111111111111111111111111111111181031a002dc6c0830e820058\
             1c00000000000000000000000000000000000000000000000000000000820058\
             1c22222222222222222222222222222222222222222222222222222222830f82\
             00581c00000000000000000000000000000000000000000000000000000000f6\
             84108200581c0000000000000000000000000000000000000000000000000000\
             00001a002dc6c0f683118200581c000000000000000000000000000000000000\
             000000000000000000001a002dc6c083128200581c0000000000000000000000\
             0000000000000000000000000000000000f683028201581c9b24324046544393\
             443e1fb35c8b72c3c39e18a516a95df5f6654101581c11111111111111111111\
             11111111111111111111111111111111111102182a151a00989680160ea20581\
             840214d87980821a000f42401a05f5e1000781587d587b010100323232323232\
             3225333333008001153330033370e900018029baa00115333007300637540022\
             4a66600894452615330054911856616c696461746f722072657475726e656420\
             66616c73650013656002002002002002002153300249010b5f746d70313a2056\
             6f696400165734ae7155ceaab9e5573eae91f5f6",
            "8182582000000000000000000000000000000000000000000000000000000000\
             0000000000",
            "81a200581d600000000000000000000000000000000000000000000000000000\
             0000011a000f4240",
        )
        .into_script_context(&redeemer, None)
        .unwrap();

        // NOTE: The initial snapshot has been generated using the Haskell
        // implementation of the ledger library for that same serialized
        // transactions. It is meant to control that our construction of the
        // script context and its serialization matches exactly those
        // from the Haskell ledger / cardano node.
        insta::assert_debug_snapshot!(script_context.to_plutus_data());
    }

    #[test]
    fn script_context_voting() {
        let redeemer = Redeemer {
            tag: RedeemerTag::Vote,
            index: 0,
            data: Data::constr(0, vec![Data::integer(42.into())]),
            ex_units: ExUnits {
                mem: 1000000,
                steps: 100000000,
            },
        };

        // NOTE: The transaction also contains treasury donation and current treasury amount
        let script_context = fixture_tx_info(
            "84a4008182582000000000000000000000000000000000000000000000000000\
             0000000000000000018002182a13a58200581c00000000000000000000000000\
             000000000000000000000000000000a182582099999999999999999999999999\
             9999999999999999999999999999999999999918988200827668747470733a2f\
             2f61696b656e2d6c616e672e6f72675820000000000000000000000000000000\
             00000000000000000000000000000000008202581c0000000000000000000000\
             0000000000000000000000000000000000a38258209999999999999999999999\
             999999999999999999999999999999999999999999008202f682582088888888\
             88888888888888888888888888888888888888888888888888888888018202f6\
             8258207777777777777777777777777777777777777777777777777777777777\
             777777028202f68203581c43fa47afc68a7913fbe2f400e3849cb492d9a2610c\
             85966de0f2ba1ea1825820999999999999999999999999999999999999999999\
             9999999999999999999999038200f68204581c00000000000000000000000000\
             000000000000000000000000000000a182582099999999999999999999999999\
             99999999999999999999999999999999999999048201f68201581c43fa47afc6\
             8a7913fbe2f400e3849cb492d9a2610c85966de0f2ba1ea18258209999999999\
             999999999999999999999999999999999999999999999999999999018201f6a2\
             0582840402d87980821a000f42401a05f5e100840400d87981182a821a000f42\
             401a05f5e1000781587d587b0101003232323232323225333333008001153330\
             033370e900018029baa001153330073006375400224a66600894452615330054\
             911856616c696461746f722072657475726e65642066616c7365001365600200\
             2002002002002153300249010b5f746d70303a20566f696400165734ae7155ce\
             aab9e5573eae91f5f6",
            "8182582000000000000000000000000000000000000000000000000000000000\
             0000000000",
            "81a200581d600000000000000000000000000000000000000000000000000000\
             0000011a000f4240",
        )
        .into_script_context(&redeemer, None)
        .unwrap();

        // NOTE: The initial snapshot has been generated using the Haskell
        // implementation of the ledger library for that same serialized
        // transactions. It is meant to control that our construction of the
        // script context and its serialization matches exactly those
        // from the Haskell ledger / cardano node.
        insta::assert_debug_snapshot!(script_context.to_plutus_data());
    }

    #[test]
    fn script_context_withdraw() {
        let redeemer = Redeemer {
            tag: RedeemerTag::Reward,
            index: 0,
            data: Data::constr(0, vec![]),
            ex_units: ExUnits {
                mem: 1000000,
                steps: 100000000,
            },
        };

        // NOTE: The transaction also contains treasury donation and current treasury amount
        let script_context = fixture_tx_info(
            "84a7008182582000000000000000000000000000000000000000000000000000\
             00000000000000000183a2005839200000000000000000000000000000000000\
             0000000000000000000000111111111111111111111111111111111111111111\
             11111111111111011a000f4240a2005823400000000000000000000000000000\
             00000000000000000000000000008198bd431b03011a000f4240a20058235011\
             1111111111111111111111111111111111111111111111111111118198bd431b\
             03011a000f424002182a031a00448e0105a1581df004036eecadc2f19e95f831\
             b4bc08919cde1d1088d74602bd3dcd78a2000e81581c00000000000000000000\
             0000000000000000000000000000000000001601a10582840000d87a81d87980\
             821a000f42401a05f5e100840300d87980821a000f42401a05f5e100f5f6",
            "8182582000000000000000000000000000000000000000000000000000000000\
             0000000000",
            "81a40058393004036eecadc2f19e95f831b4bc08919cde1d1088d74602bd3dcd\
             78a204036eecadc2f19e95f831b4bc08919cde1d1088d74602bd3dcd78a2011a\
             000f4240028201d81843d8798003d818590221820359021c5902190101003232\
             323232323232322232533333300c00215323330073001300937540062a660109\
             211c52756e6e696e672032206172672076616c696461746f72206d696e740013\
             533333300d004153330073001300937540082a66601660146ea8010494ccc021\
             288a4c2a660129211856616c696461746f722072657475726e65642066616c73\
             65001365600600600600600600600315330084911d52756e6e696e6720332061\
             72672076616c696461746f72207370656e640013533333300d00415333007300\
             1300937540082a66601660146ea8010494cccccc03800454ccc020c008c028dd\
             50008a99980618059baa0011253330094a22930a998052491856616c69646174\
             6f722072657475726e65642066616c7365001365600600600600600600600600\
             6006006006006300c300a37540066e1d20001533007001161533007001161533\
             00700116153300700116490191496e636f72726563742072656465656d657220\
             7479706520666f722076616c696461746f72207370656e642e0a202020202020\
             2020202020202020202020202020446f75626c6520636865636b20796f752068\
             6176652077726170706564207468652072656465656d65722074797065206173\
             2073706563696669656420696e20796f757220706c757475732e6a736f6e0015\
             330034910b5f746d70313a20566f6964001615330024910b5f746d70303a2056\
             6f696400165734ae7155ceaab9e5573eae855d21",
        )
        .into_script_context(&redeemer, None)
        .unwrap();

        // NOTE: The initial snapshot has been generated using the Haskell
        // implementation of the ledger library for that same serialized
        // transactions. It is meant to control that our construction of the
        // script context and its serialization matches exactly those
        // from the Haskell ledger / cardano node.
        insta::assert_debug_snapshot!(script_context.to_plutus_data());
    }
}
