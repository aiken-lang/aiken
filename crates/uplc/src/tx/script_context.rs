use pallas_addresses::Address;
use pallas_codec::utils::KeyValuePairs;
use pallas_crypto::hash::Hash;
use pallas_primitives::babbage::{
    AddrKeyhash, Certificate, Coin, DatumHash, PlutusData, PolicyId, Redeemer, StakeCredential,
    TransactionInput, TransactionOutput, Value,
};

use super::to_plutus_data::MintValue;

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
