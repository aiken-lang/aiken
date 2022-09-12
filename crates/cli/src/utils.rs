use pallas_addresses::{
    Address, ScriptHash, ShelleyDelegationPart, ShelleyPaymentPart, StakePayload,
};
use pallas_codec::{
    minicbor::{bytes::ByteVec, data::Int},
    utils::{AnyUInt, KeyValuePairs, MaybeIndefArray},
};
use pallas_crypto::hash::{Hash, Hasher};
use pallas_primitives::{
    babbage::{
        AddrKeyhash, AssetName, BigInt, Certificate, Constr, CostModel, DatumHash, DatumOption,
        ExUnits, Language, Mint, MintedTx, PlutusV1Script, PlutusV2Script, PolicyId, Redeemer,
        RedeemerTag, RewardAccount, Script, ScriptRef, StakeCredential, TransactionInput,
        TransactionOutput, Tx, Value, Withdrawals,
    },
    Fragment, ToHash,
};
use pallas_traverse::{Era, MultiEraTx};
use std::{
    collections::HashMap,
    convert::{TryFrom, TryInto},
    str::FromStr,
    vec,
};
use uplc::{
    ast::{FakeNamedDeBruijn, NamedDeBruijn, Program},
    machine::cost_model::ExBudget,
    PlutusData,
};

use crate::args::ResolvedInput;

pub fn get_tx_in_info_old(resolved_inputs: &[ResolvedInput]) -> anyhow::Result<Vec<PlutusData>> {
    let mut tx_in_info = Vec::new();

    for resolved_input in resolved_inputs {
        let tx_out_ref = TransactionInput {
            transaction_id: Hash::from_str(resolved_input.input.tx_hash.as_str())?,
            index: resolved_input.input.index,
        }
        .to_plutus_data();

        let address = Address::from_bech32(&resolved_input.output.address)?;

        let lovelace = resolved_input.output.value.0;

        let mut assets = resolved_input.output.value.1.clone();

        assets.insert(
            "".to_string(),
            vec![("".to_string(), lovelace)].into_iter().collect(),
        );

        let tx_out = PlutusData::Constr(Constr {
            tag: 0,
            any_constructor: None,
            fields: MaybeIndefArray::Indef(vec![
                // txOutAddress
                address.to_plutus_data(),
                // txOutValue
                PlutusData::Map(KeyValuePairs::Def(
                    assets
                        .iter()
                        .map(|val| {
                            let currency_symbol =
                                PlutusData::BoundedBytes(hex::decode(val.0).unwrap().into());
                            let token_map = PlutusData::Map(KeyValuePairs::Def(
                                val.1
                                    .iter()
                                    .map(|token| {
                                        (
                                            PlutusData::BoundedBytes(
                                                token.0.as_bytes().to_vec().into(),
                                            ),
                                            PlutusData::BigInt(BigInt::Int((*token.1).into())),
                                        )
                                    })
                                    .collect(),
                            ));
                            (currency_symbol, token_map)
                        })
                        .collect(),
                )),
            ]),
        });

        tx_in_info.push(PlutusData::Constr(Constr {
            tag: 0,
            any_constructor: None,
            fields: MaybeIndefArray::Indef(vec![tx_out_ref, tx_out]),
        }));
    }

    Ok(tx_in_info)
}

// ---------------

fn wrap_with_constr(index: u64, data: PlutusData) -> PlutusData {
    PlutusData::Constr(Constr {
        tag: constr_index(index),
        any_constructor: None,
        fields: MaybeIndefArray::Indef(vec![data]),
    })
}

fn wrap_multiple_with_constr(index: u64, data: Vec<PlutusData>) -> PlutusData {
    PlutusData::Constr(Constr {
        tag: constr_index(index),
        any_constructor: None,
        fields: MaybeIndefArray::Indef(data),
    })
}

fn empty_constr(index: u64) -> PlutusData {
    PlutusData::Constr(Constr {
        tag: constr_index(index),
        any_constructor: None,
        fields: MaybeIndefArray::Indef(vec![]),
    })
}

/// Translate constructor index to cbor tag
fn constr_index(index: u64) -> u64 {
    121 + index
}

pub trait ToPlutusData {
    fn to_plutus_data(&self) -> PlutusData;
}

impl ToPlutusData for Address {
    fn to_plutus_data(&self) -> PlutusData {
        match self {
            Address::Byron(byron_address) => {
                wrap_multiple_with_constr(
                    0,
                    vec![
                        //addressCredential
                        wrap_with_constr(0, byron_address.decode().unwrap().root.to_plutus_data()),
                        //addressStakeCredential
                        None::<StakeCredential>.to_plutus_data(),
                    ],
                )
            }
            Address::Shelley(shelley_address) => {
                let payment_part = shelley_address.payment();
                let stake_part = shelley_address.delegation();

                let payment_part_plutus_data = match payment_part {
                    ShelleyPaymentPart::Key(payment_keyhash) => {
                        wrap_with_constr(0, payment_keyhash.to_plutus_data())
                    }
                    ShelleyPaymentPart::Script(script_hash) => {
                        wrap_with_constr(1, script_hash.to_plutus_data())
                    }
                };

                let stake_part_plutus_data = match stake_part {
                    ShelleyDelegationPart::Key(stake_keyhash) => {
                        Some(StakeCredential::AddrKeyhash(stake_keyhash.clone())).to_plutus_data()
                    }
                    ShelleyDelegationPart::Script(script_hash) => {
                        Some(StakeCredential::Scripthash(script_hash.clone())).to_plutus_data()
                    }
                    ShelleyDelegationPart::Pointer(pointer) => Some(wrap_multiple_with_constr(
                        1,
                        vec![
                            pointer.slot().to_plutus_data(),
                            pointer.tx_idx().to_plutus_data(),
                            pointer.cert_idx().to_plutus_data(),
                        ],
                    ))
                    .to_plutus_data(),
                    ShelleyDelegationPart::Null => None::<StakeCredential>.to_plutus_data(),
                };

                wrap_multiple_with_constr(0, vec![payment_part_plutus_data, stake_part_plutus_data])
            }
            Address::Stake(_) => unreachable!(),
        }
    }
}

impl ToPlutusData for TransactionInput {
    fn to_plutus_data(&self) -> PlutusData {
        wrap_multiple_with_constr(
            0,
            vec![
                wrap_with_constr(0, self.transaction_id.to_plutus_data()),
                PlutusData::BigInt(BigInt::Int(self.index.into())),
            ],
        )
    }
}

impl<const BYTES: usize> ToPlutusData for Hash<BYTES> {
    fn to_plutus_data(&self) -> PlutusData {
        PlutusData::BoundedBytes(self.to_vec().into())
    }
}

impl ToPlutusData for ByteVec {
    fn to_plutus_data(&self) -> PlutusData {
        PlutusData::BoundedBytes(self.clone())
    }
}

impl<A: ToPlutusData> ToPlutusData for MaybeIndefArray<A> {
    fn to_plutus_data(&self) -> PlutusData {
        PlutusData::Array(MaybeIndefArray::Indef(
            self.iter().map(|p| p.to_plutus_data()).collect(),
        ))
    }
}

impl<K: ToPlutusData, V: ToPlutusData> ToPlutusData for KeyValuePairs<K, V> {
    fn to_plutus_data(&self) -> PlutusData {
        let mut data_vec: Vec<(PlutusData, PlutusData)> = vec![];
        for (key, value) in self.iter() {
            data_vec.push((key.to_plutus_data(), value.to_plutus_data()))
        }
        PlutusData::Map(KeyValuePairs::Def(data_vec))
    }
}

impl<A: ToPlutusData> ToPlutusData for Option<A> {
    fn to_plutus_data(&self) -> PlutusData {
        match self {
            None => empty_constr(1),
            Some(data) => wrap_with_constr(0, data.to_plutus_data()),
        }
    }
}

// Does this here surely overwrite Option from above for DatumOption?
impl ToPlutusData for Option<DatumOption> {
    // NoOutputDatum = 0 | OutputDatumHash = 1 | OutputDatum = 2
    fn to_plutus_data(&self) -> PlutusData {
        match self {
            None => empty_constr(0),
            Some(option) => match option {
                DatumOption::Hash(hash) => wrap_with_constr(1, hash.to_plutus_data()),
                DatumOption::Data(data) => wrap_with_constr(2, data.0.clone()),
            },
        }
    }
}

impl ToPlutusData for AnyUInt {
    fn to_plutus_data(&self) -> PlutusData {
        match self {
            AnyUInt::U8(u8) => PlutusData::BigInt(BigInt::Int(Int::from(*u8))),
            AnyUInt::U16(u16) => PlutusData::BigInt(BigInt::Int(Int::from(*u16))),
            AnyUInt::U32(u32) => PlutusData::BigInt(BigInt::Int(Int::from(*u32))),
            AnyUInt::U64(u64) => PlutusData::BigInt(BigInt::Int(Int::from(*u64))),
            AnyUInt::MajorByte(u8) => PlutusData::BigInt(BigInt::Int(Int::from(*u8))), // is this correct? I don't know exactly what is does
        }
    }
}

impl ToPlutusData for Int {
    fn to_plutus_data(&self) -> PlutusData {
        PlutusData::BigInt(BigInt::Int(self.clone()))
    }
}

impl ToPlutusData for BigInt {
    fn to_plutus_data(&self) -> PlutusData {
        PlutusData::BigInt(self.clone())
    }
}

impl ToPlutusData for i64 {
    fn to_plutus_data(&self) -> PlutusData {
        PlutusData::BigInt(BigInt::Int(Int::from(*self)))
    }
}

impl ToPlutusData for u64 {
    fn to_plutus_data(&self) -> PlutusData {
        PlutusData::BigInt(BigInt::Int(Int::from(*self)))
    }
}

impl ToPlutusData for Value {
    fn to_plutus_data(&self) -> PlutusData {
        match self {
            Value::Coin(coin) => PlutusData::Map(KeyValuePairs::Def(vec![(
                PolicyId::from(vec![]).to_plutus_data(),
                PlutusData::Map(KeyValuePairs::Def(vec![(
                    AssetName::from(vec![]).to_plutus_data(),
                    coin.to_plutus_data(),
                )])),
            )])),
            Value::Multiasset(coin, multiassets) => {
                let mut data_vec: Vec<(PlutusData, PlutusData)> = vec![(
                    PolicyId::from(vec![]).to_plutus_data(),
                    PlutusData::Map(KeyValuePairs::Def(vec![(
                        AssetName::from(vec![]).to_plutus_data(),
                        coin.to_plutus_data(),
                    )])),
                )];

                for (policy_id, assets) in multiassets.iter() {
                    let mut assets_vec = vec![];
                    for (asset, amount) in assets.iter() {
                        assets_vec.push((asset.to_plutus_data(), amount.to_plutus_data()));
                    }
                    data_vec.push((
                        policy_id.to_plutus_data(),
                        PlutusData::Map(KeyValuePairs::Def(assets_vec)),
                    ));
                }

                PlutusData::Map(KeyValuePairs::Def(data_vec))
            }
        }
    }
}

impl ToPlutusData for ScriptRef {
    fn to_plutus_data(&self) -> PlutusData {
        match &self.0 {
            Script::NativeScript(native_script) => native_script.to_hash().to_plutus_data(),
            Script::PlutusV1Script(plutus_v1) => plutus_v1.to_hash().to_plutus_data(),
            Script::PlutusV2Script(plutus_v2) => plutus_v2.to_hash().to_plutus_data(),
        }
    }
}

impl ToPlutusData for TransactionOutput {
    fn to_plutus_data(&self) -> PlutusData {
        match self {
            TransactionOutput::Legacy(legacy_output) => wrap_multiple_with_constr(
                0,
                vec![
                    Address::from_bytes(&legacy_output.address)
                        .unwrap()
                        .to_plutus_data(),
                    legacy_output.amount.to_plutus_data(),
                    None::<DatumOption>.to_plutus_data(),
                    None::<ScriptRef>.to_plutus_data(),
                ],
            ),
            TransactionOutput::PostAlonzo(post_alonzo_output) => wrap_multiple_with_constr(
                0,
                vec![
                    Address::from_bytes(&post_alonzo_output.address)
                        .unwrap()
                        .to_plutus_data(),
                    post_alonzo_output.value.to_plutus_data(),
                    post_alonzo_output.datum_option.to_plutus_data(),
                    post_alonzo_output.script_ref.to_plutus_data(),
                ],
            ),
        }
    }
}

impl ToPlutusData for StakeCredential {
    // Stake Credential needs to be wrapped inside another Constr, because we could have either a StakingHash or a StakingPtr
    // The current implementation of StakeCredential doesn't capture the credential of a Pointer address.
    // So a StakeCredential for a Pointer address needs to be converted separately
    fn to_plutus_data(&self) -> PlutusData {
        match self {
            StakeCredential::AddrKeyhash(addr_keyhas) => {
                wrap_with_constr(0, wrap_with_constr(0, addr_keyhas.to_plutus_data()))
            }
            StakeCredential::Scripthash(script_hash) => {
                wrap_with_constr(0, wrap_with_constr(1, script_hash.to_plutus_data()))
            }
        }
    }
}

impl ToPlutusData for Certificate {
    fn to_plutus_data(&self) -> PlutusData {
        match self {
            Certificate::StakeRegistration(stake_credential) => {
                wrap_with_constr(0, stake_credential.to_plutus_data())
            }
            Certificate::StakeDeregistration(stake_credential) => {
                wrap_with_constr(1, stake_credential.to_plutus_data())
            }
            Certificate::StakeDelegation(stake_credential, pool_keyhash) => {
                wrap_multiple_with_constr(
                    2,
                    vec![
                        stake_credential.to_plutus_data(),
                        pool_keyhash.to_plutus_data(),
                    ],
                )
            }
            Certificate::PoolRegistration {
                operator,
                vrf_keyhash,
                pledge: _,
                cost: _,
                margin: _,
                reward_account: _,
                pool_owners: _,
                relays: _,
                pool_metadata: _,
            } => wrap_multiple_with_constr(
                3,
                vec![operator.to_plutus_data(), vrf_keyhash.to_plutus_data()],
            ),
            Certificate::PoolRetirement(pool_keyhash, epoch) => wrap_multiple_with_constr(
                4,
                vec![pool_keyhash.to_plutus_data(), epoch.to_plutus_data()],
            ),
            Certificate::GenesisKeyDelegation(_, _, _) => empty_constr(5),
            Certificate::MoveInstantaneousRewardsCert(_) => empty_constr(6),
        }
    }
}

impl ToPlutusData for Redeemer {
    fn to_plutus_data(&self) -> PlutusData {
        self.data.clone()
    }
}

impl ToPlutusData for PlutusData {
    fn to_plutus_data(&self) -> PlutusData {
        self.clone()
    }
}

impl ToPlutusData for TimeRange {
    fn to_plutus_data(&self) -> PlutusData {
        match &self {
            TimeRange {
                lower_bound: Some(lower_bound),
                upper_bound: None,
            } => {
                wrap_multiple_with_constr(
                    0,
                    vec![
                        // LowerBound
                        wrap_multiple_with_constr(
                            0,
                            vec![
                                // Finite
                                wrap_with_constr(1, lower_bound.to_plutus_data()),
                                // Closure
                                true.to_plutus_data(),
                            ],
                        ), //UpperBound
                        wrap_multiple_with_constr(
                            0,
                            vec![
                                // PosInf
                                empty_constr(2),
                                // Closure
                                true.to_plutus_data(),
                            ],
                        ),
                    ],
                )
            }
            TimeRange {
                lower_bound: None,
                upper_bound: Some(upper_bound),
            } => {
                wrap_multiple_with_constr(
                    0,
                    vec![
                        // LowerBound
                        wrap_multiple_with_constr(
                            0,
                            vec![
                                // NegInf
                                empty_constr(0),
                                // Closure
                                true.to_plutus_data(),
                            ],
                        ),
                        //UpperBound
                        wrap_multiple_with_constr(
                            0,
                            vec![
                                // Finite
                                wrap_with_constr(1, upper_bound.to_plutus_data()),
                                // Closure
                                true.to_plutus_data(),
                            ],
                        ),
                    ],
                )
            }
            TimeRange {
                lower_bound: Some(lower_bound),
                upper_bound: Some(upper_bound),
            } => {
                wrap_multiple_with_constr(
                    0,
                    vec![
                        // LowerBound
                        wrap_multiple_with_constr(
                            0,
                            vec![
                                // Finite
                                wrap_with_constr(1, lower_bound.to_plutus_data()),
                                // Closure
                                true.to_plutus_data(),
                            ],
                        ),
                        //UpperBound
                        wrap_multiple_with_constr(
                            0,
                            vec![
                                // Finite
                                wrap_with_constr(1, upper_bound.to_plutus_data()),
                                // Closure
                                false.to_plutus_data(),
                            ],
                        ),
                    ],
                )
            }
            TimeRange {
                lower_bound: None,
                upper_bound: None,
            } => {
                wrap_multiple_with_constr(
                    0,
                    vec![
                        // LowerBound
                        wrap_multiple_with_constr(
                            0,
                            vec![
                                // NegInf
                                empty_constr(0),
                                // Closure
                                true.to_plutus_data(),
                            ],
                        ),
                        //UpperBound
                        wrap_multiple_with_constr(
                            0,
                            vec![
                                // PosInf
                                empty_constr(2),
                                // Closure
                                true.to_plutus_data(),
                            ],
                        ),
                    ],
                )
            }
        }
    }
}

impl ToPlutusData for TxInInfo {
    fn to_plutus_data(&self) -> PlutusData {
        wrap_multiple_with_constr(
            0,
            vec![
                self.out_ref.to_plutus_data(),
                self.resolved.to_plutus_data(),
            ],
        )
    }
}

impl ToPlutusData for ScriptPurpose {
    fn to_plutus_data(&self) -> PlutusData {
        match self {
            ScriptPurpose::Minting(policy_id) => wrap_with_constr(0, policy_id.to_plutus_data()),
            ScriptPurpose::Spending(out_ref) => wrap_with_constr(1, out_ref.to_plutus_data()),
            ScriptPurpose::Rewarding(stake_credential) => {
                wrap_with_constr(2, stake_credential.to_plutus_data())
            }
            ScriptPurpose::Certifying(dcert) => wrap_with_constr(3, dcert.to_plutus_data()),
        }
    }
}

impl ToPlutusData for TxInfo {
    fn to_plutus_data(&self) -> PlutusData {
        wrap_multiple_with_constr(
            0,
            vec![
                self.inputs.to_plutus_data(),
                self.reference_inputs.to_plutus_data(),
                self.outputs.to_plutus_data(),
                self.fee.to_plutus_data(),
                self.mint.to_plutus_data(),
                self.dcert.to_plutus_data(),
                self.wdrl.to_plutus_data(),
                self.valid_range.to_plutus_data(),
                self.signatories.to_plutus_data(),
                self.redeemers.to_plutus_data(),
                self.data.to_plutus_data(),
                wrap_with_constr(0, self.id.to_plutus_data()),
            ],
        )
    }
}

impl ToPlutusData for ScriptContext {
    fn to_plutus_data(&self) -> PlutusData {
        wrap_multiple_with_constr(
            0,
            vec![self.tx_info.to_plutus_data(), self.purpose.to_plutus_data()],
        )
    }
}

impl ToPlutusData for bool {
    fn to_plutus_data(&self) -> PlutusData {
        match self {
            false => empty_constr(0),
            true => empty_constr(1),
        }
    }
}

// Plutus V2 only for now

#[derive(Debug, PartialEq, Clone)]
pub struct TxInInfo {
    out_ref: TransactionInput,
    resolved: TransactionOutput,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ScriptPurpose {
    Minting(PolicyId),
    Spending(TransactionInput),
    Rewarding(StakeCredential),
    Certifying(Certificate),
}

#[derive(Debug, PartialEq, Clone)]
pub struct TxInfo {
    inputs: MaybeIndefArray<TxInInfo>,
    reference_inputs: MaybeIndefArray<TxInInfo>,
    outputs: MaybeIndefArray<TransactionOutput>,
    fee: Value,
    mint: Mint,
    dcert: MaybeIndefArray<Certificate>,
    wdrl: Withdrawals,
    valid_range: TimeRange,
    signatories: MaybeIndefArray<AddrKeyhash>,
    redeemers: KeyValuePairs<ScriptPurpose, Redeemer>,
    data: KeyValuePairs<DatumHash, PlutusData>,
    id: Hash<32>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ScriptContext {
    tx_info: TxInfo,
    purpose: ScriptPurpose,
}

//---- Time conversion: slot range => posix time range
#[derive(Debug, PartialEq, Clone)]
struct TimeRange {
    lower_bound: Option<u64>,
    upper_bound: Option<u64>,
}

struct SlotConfig {
    slot_length: u64,
    zero_time: u64,
}

fn slot_to_begin_posix_time(slot: u64, sc: &SlotConfig) -> u64 {
    let ms_after_begin = slot * sc.slot_length;
    sc.zero_time + ms_after_begin
}

fn slot_range_to_posix_time_range(slot_range: TimeRange, sc: &SlotConfig) -> TimeRange {
    TimeRange {
        lower_bound: match slot_range.lower_bound {
            Some(lower_bound) => Some(slot_to_begin_posix_time(lower_bound, sc)),
            None => None,
        },
        upper_bound: match slot_range.upper_bound {
            Some(upper_bound) => Some(slot_to_begin_posix_time(upper_bound, sc)),
            None => None,
        },
    }
}

#[derive(Debug, PartialEq, Clone)]
enum ScriptVersion {
    PlutusV1(PlutusV1Script),
    PlutusV2(PlutusV2Script),
}

#[derive(Debug, PartialEq, Clone)]
enum ExecutionPurpose {
    WithDatum(ScriptVersion, PlutusData), // Spending
    NoDatum(ScriptVersion),               // Minting, Wdrl, DCert
}

struct DataLookupTable {
    datum: HashMap<DatumHash, PlutusData>,
    scripts: HashMap<ScriptHash, ScriptVersion>,
}

fn get_tx_in_info(
    inputs: &MaybeIndefArray<TransactionInput>,
    utxos: &MaybeIndefArray<TxInInfo>,
) -> anyhow::Result<MaybeIndefArray<TxInInfo>> {
    Ok(MaybeIndefArray::Indef(
        utxos
            .iter()
            .filter(|utxo| inputs.contains(&utxo.out_ref))
            .map(|utxo| utxo.clone())
            .collect::<Vec<TxInInfo>>(),
    ))
}

fn get_script_purpose(
    redeemer: &Redeemer,
    inputs: &MaybeIndefArray<TransactionInput>,
    mint: &Option<Mint>,
    dcert: &Option<MaybeIndefArray<Certificate>>,
    wdrl: &Option<Withdrawals>,
) -> anyhow::Result<ScriptPurpose> {
    // sorting according to specs section 4.1: https://hydra.iohk.io/build/18583827/download/1/alonzo-changes.pdf
    let tag = redeemer.tag.clone();
    let index = redeemer.index;
    match tag {
        RedeemerTag::Mint => {
            // sort lexical by policy id
            let mut policy_ids = mint
                .as_ref()
                .unwrap()
                .iter()
                .map(|(policy_id, _)| policy_id.clone())
                .collect::<Vec<ByteVec>>();
            policy_ids.sort();
            let policy_id = policy_ids[index as usize].clone();
            Ok(ScriptPurpose::Minting(policy_id))
        }
        RedeemerTag::Spend => {
            // sort lexical by tx_hash and index
            let mut inputs = inputs
                .iter()
                .map(|input| input.clone())
                .collect::<Vec<TransactionInput>>();
            // is this correct? Does this sort lexical from low to high? maybe get Ordering into pallas for TransactionInput?
            inputs.sort_by(
                |i_a, i_b| match i_a.transaction_id.cmp(&i_b.transaction_id) {
                    std::cmp::Ordering::Less => std::cmp::Ordering::Less,
                    std::cmp::Ordering::Equal => i_a.index.cmp(&i_b.index),
                    std::cmp::Ordering::Greater => std::cmp::Ordering::Greater,
                },
            );
            let input = inputs[index as usize].clone();
            Ok(ScriptPurpose::Spending(input))
        }
        RedeemerTag::Reward => {
            // sort lexical by reward account
            let mut reward_accounts = wdrl
                .as_ref()
                .unwrap()
                .iter()
                .map(|(policy_id, _)| policy_id.clone())
                .collect::<Vec<RewardAccount>>();
            reward_accounts.sort();
            let reward_account = reward_accounts[index as usize].clone();
            let addresss = Address::from_bytes(&reward_account)?;
            let credential = match addresss {
                Address::Stake(stake_address) => match stake_address.payload() {
                    StakePayload::Stake(stake_keyhash) => {
                        StakeCredential::AddrKeyhash(stake_keyhash.clone())
                    }
                    StakePayload::Script(script_hash) => {
                        StakeCredential::Scripthash(script_hash.clone())
                    }
                },
                _ => panic!(),
            };
            Ok(ScriptPurpose::Rewarding(credential))
        }
        RedeemerTag::Cert => {
            // sort by order given in the tx (just take it as it is basically)
            let cert = dcert.as_ref().unwrap()[index as usize].clone();
            Ok(ScriptPurpose::Certifying(cert))
        }
    }
}

fn get_tx_info(
    tx: &MintedTx,
    utxos: &MaybeIndefArray<TxInInfo>,
    slot_config: &SlotConfig,
) -> anyhow::Result<TxInfo> {
    let body = tx.transaction_body.clone();

    let inputs = get_tx_in_info(&body.inputs, &utxos)?;
    let reference_inputs = get_tx_in_info(
        &body
            .reference_inputs
            .clone()
            .unwrap_or(MaybeIndefArray::Indef(vec![])),
        &utxos,
    )?;
    let outputs = body.outputs.clone();
    let fee = Value::Coin(AnyUInt::U64(body.fee));
    let mint = body.mint.clone().unwrap_or(KeyValuePairs::Indef(vec![]));
    let dcert = body
        .certificates
        .clone()
        .unwrap_or(MaybeIndefArray::Indef(vec![]));
    let wdrl = body
        .withdrawals
        .clone()
        .unwrap_or(KeyValuePairs::Indef(vec![]));
    let valid_range = slot_range_to_posix_time_range(
        TimeRange {
            lower_bound: body.validity_interval_start,
            upper_bound: body.ttl,
        },
        &slot_config,
    );
    let signatories = body
        .required_signers
        .clone()
        .unwrap_or(MaybeIndefArray::Indef(vec![]));
    let redeemers = KeyValuePairs::Indef(
        tx.transaction_witness_set
            .redeemer
            .as_ref()
            .unwrap_or(&MaybeIndefArray::Indef(vec![]))
            .iter()
            .map(|r| {
                (
                    get_script_purpose(
                        &r,
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
            .unwrap_or(&MaybeIndefArray::Indef(vec![]))
            .iter()
            .map(|d| (d.to_hash(), d.clone()))
            .collect(),
    );
    let id = tx.transaction_body.to_hash();

    Ok(TxInfo {
        inputs,
        reference_inputs,
        outputs,
        fee,
        mint,
        dcert,
        wdrl,
        valid_range,
        signatories,
        redeemers,
        data,
        id,
    })
}

fn get_execution_purpose(
    utxos: &MaybeIndefArray<TxInInfo>,
    script_purpose: &ScriptPurpose,
    lookup_table: &DataLookupTable,
) -> ExecutionPurpose {
    match script_purpose {
        ScriptPurpose::Minting(policy_id) => {
            let policy_id_array: [u8; 28] = policy_id.to_vec().try_into().unwrap();
            let hash = Hash::from(policy_id_array);

            let script = lookup_table.scripts.get(&hash).unwrap();
            ExecutionPurpose::NoDatum(script.clone())
        }
        ScriptPurpose::Spending(out_ref) => {
            let utxo = utxos.iter().find(|utxo| utxo.out_ref == *out_ref).unwrap();
            match &utxo.resolved {
                TransactionOutput::Legacy(output) => {
                    let address = Address::from_bytes(&output.address).unwrap();
                    match address {
                        Address::Shelley(shelley_address) => {
                            let script = lookup_table
                                .scripts
                                .get(&shelley_address.payment().as_hash())
                                .unwrap();

                            let datum =
                                lookup_table.datum.get(&output.datum_hash.unwrap()).unwrap();

                            let a = hex::encode(datum.encode_fragment().unwrap());

                            println!("{:?}", a);
                            ExecutionPurpose::WithDatum(script.clone(), datum.clone())
                        }
                        _ => unreachable!(),
                    }
                }
                TransactionOutput::PostAlonzo(output) => {
                    let address = Address::from_bytes(&output.address).unwrap();
                    match address {
                        Address::Shelley(shelley_address) => {
                            let script = lookup_table
                                .scripts
                                .get(&shelley_address.payment().as_hash())
                                .unwrap();

                            let datum = match &output.datum_option {
                                Some(DatumOption::Hash(hash)) => {
                                    lookup_table.datum.get(&hash).unwrap().clone()
                                }
                                Some(DatumOption::Data(data)) => data.0.clone(),
                                _ => unreachable!(),
                            };

                            ExecutionPurpose::WithDatum(script.clone(), datum)
                        }
                        _ => unreachable!(),
                    }
                }
            }
        }
        ScriptPurpose::Rewarding(stake_credential) => {
            let script_hash = match stake_credential {
                StakeCredential::Scripthash(hash) => hash.clone(),
                _ => unreachable!(),
            };
            let script = lookup_table.scripts.get(&script_hash).unwrap();
            ExecutionPurpose::NoDatum(script.clone())
        }
        ScriptPurpose::Certifying(cert) => match cert {
            // StakeRegistration doesn't require a witness from a stake key/script. So I assume it doesn't need to be handled in Plutus either?
            Certificate::StakeDeregistration(stake_credential) => {
                let script_hash = match stake_credential {
                    StakeCredential::Scripthash(hash) => hash.clone(),
                    _ => unreachable!(),
                };
                let script = lookup_table.scripts.get(&script_hash).unwrap();
                ExecutionPurpose::NoDatum(script.clone())
            }
            Certificate::StakeDelegation(stake_credential, _) => {
                let script_hash = match stake_credential {
                    StakeCredential::Scripthash(hash) => hash.clone(),
                    _ => unreachable!(),
                };
                let script = lookup_table.scripts.get(&script_hash).unwrap();
                ExecutionPurpose::NoDatum(script.clone())
            }
            _ => unreachable!(),
        },
    }
}

fn get_script_and_datum_lookup_table(
    tx: &MintedTx,
    utxos: &MaybeIndefArray<TxInInfo>,
) -> DataLookupTable {
    let mut datum = HashMap::new();
    let mut scripts = HashMap::new();

    // discovery in witness set

    let plutus_data_witnesses = tx
        .transaction_witness_set
        .plutus_data
        .clone()
        .unwrap_or(MaybeIndefArray::Indef(vec![]));

    let scripts_v1_witnesses = tx
        .transaction_witness_set
        .plutus_v1_script
        .clone()
        .unwrap_or(MaybeIndefArray::Indef(vec![]));

    let scripts_v2_witnesses = tx
        .transaction_witness_set
        .plutus_v2_script
        .clone()
        .unwrap_or(MaybeIndefArray::Indef(vec![]));

    for plutus_data in plutus_data_witnesses.iter() {
        datum.insert(plutus_data.to_hash(), plutus_data.clone());
    }

    for script in scripts_v1_witnesses.iter() {
        // scripts.insert(script.to_hash(), ScriptVersion::PlutusV1(script.clone())); // TODO: fix hashing bug in pallas

        let mut prefixed_script: Vec<u8> = vec![0x01];
        prefixed_script.extend(script.0.iter());

        let hash = Hasher::<224>::hash(&prefixed_script);
        scripts.insert(hash, ScriptVersion::PlutusV1(script.clone()));
    }

    for script in scripts_v2_witnesses.iter() {
        // scripts.insert(script.to_hash(), ScriptVersion::PlutusV2(script.clone())); // TODO: fix hashing bug in pallas

        let mut prefixed_script: Vec<u8> = vec![0x02];
        prefixed_script.extend(script.0.iter());

        let hash = Hasher::<224>::hash(&prefixed_script);
        scripts.insert(hash, ScriptVersion::PlutusV2(script.clone()));
    }

    // discovery in utxos (script ref)

    for utxo in utxos.iter() {
        match &utxo.resolved {
            TransactionOutput::Legacy(_) => {}
            TransactionOutput::PostAlonzo(output) => match &output.script_ref {
                Some(script) => match &script.0 {
                    Script::PlutusV1Script(v1) => {
                        // scripts.insert(v1.to_hash(), ScriptVersion::PlutusV1(v1.clone())); // TODO: fix hashing bug in pallas
                        let mut prefixed_script: Vec<u8> = vec![0x01];
                        prefixed_script.extend(v1.0.iter());

                        let hash = Hasher::<224>::hash(&prefixed_script);
                        scripts.insert(hash, ScriptVersion::PlutusV1(v1.clone()));
                    }
                    Script::PlutusV2Script(v2) => {
                        // scripts.insert(v2.to_hash(), ScriptVersion::PlutusV2(v2.clone())); // TODO: fix hashing bug in pallas

                        let mut prefixed_script: Vec<u8> = vec![0x02];
                        prefixed_script.extend(v2.0.iter());

                        let hash = Hasher::<224>::hash(&prefixed_script);
                        scripts.insert(hash, ScriptVersion::PlutusV2(v2.clone()));
                    }
                    _ => {}
                },
                _ => {}
            },
        }
    }

    DataLookupTable { datum, scripts }
}

fn eval_redeemer(
    tx: &MintedTx,
    utxos: &MaybeIndefArray<TxInInfo>,
    slot_config: &SlotConfig,
    redeemer: &Redeemer,
    lookup_table: &DataLookupTable,
) -> anyhow::Result<Redeemer> {
    let purpose = get_script_purpose(
        redeemer,
        &tx.transaction_body.inputs,
        &tx.transaction_body.mint,
        &tx.transaction_body.certificates,
        &tx.transaction_body.withdrawals,
    )?;

    let execution_purpose: ExecutionPurpose = get_execution_purpose(utxos, &purpose, lookup_table);

    match execution_purpose {
        ExecutionPurpose::WithDatum(script_version, datum) => match script_version {
            ScriptVersion::PlutusV1(script) => todo!(),
            ScriptVersion::PlutusV2(script) => {
                let tx_info = get_tx_info(tx, utxos, slot_config)?;
                let script_context = ScriptContext { tx_info, purpose };

                let program: Program<NamedDeBruijn> = {
                    let mut buffer = Vec::new();

                    let prog = Program::<FakeNamedDeBruijn>::from_cbor(&script.0, &mut buffer)?;

                    prog.into()
                };

                let result = program
                    .apply_data(datum.clone())
                    .apply_data(redeemer.data.clone())
                    .apply_data(script_context.to_plutus_data())
                    .eval();

                result.0.unwrap();

                let new_redeemer = Redeemer {
                    tag: redeemer.tag.clone(),
                    index: redeemer.index,
                    data: redeemer.data.clone(),
                    ex_units: ExUnits {
                        mem: (ExBudget::default().mem - result.1.mem) as u32,
                        steps: (ExBudget::default().cpu - result.1.cpu) as u64,
                    },
                };

                Ok(new_redeemer)
            }
        },
        ExecutionPurpose::NoDatum(script_version) => match script_version {
            ScriptVersion::PlutusV1(script) => todo!(),
            ScriptVersion::PlutusV2(script) => {
                let tx_info = get_tx_info(tx, utxos, slot_config)?;
                let script_context = ScriptContext { tx_info, purpose };

                let program: Program<NamedDeBruijn> = {
                    let mut buffer = Vec::new();

                    let prog = Program::<FakeNamedDeBruijn>::from_cbor(&script.0, &mut buffer)?;

                    prog.into()
                };

                let result = program
                    .apply_data(redeemer.data.clone())
                    .apply_data(script_context.to_plutus_data())
                    .eval();

                match result.0 {
                    Ok(_) => {
                        println!("SUCCESS")
                    }
                    Err(err) => {
                        println!("ERROR: {:?}", err.to_string())
                    }
                }

                println!("MEM: {:?}", ExBudget::default().mem - result.1.mem);
                println!("STEP: {:?}", ExBudget::default().cpu - result.1.cpu);

                // result.0.unwrap();

                let new_redeemer = Redeemer {
                    tag: redeemer.tag.clone(),
                    index: redeemer.index,
                    data: redeemer.data.clone(),
                    ex_units: ExUnits {
                        mem: (ExBudget::default().mem - result.1.mem) as u32,
                        steps: (ExBudget::default().cpu - result.1.cpu) as u64,
                    },
                };

                Ok(new_redeemer)
            }
        },
    }
}

fn eval_tx(
    tx: &MintedTx,
    utxos: &MaybeIndefArray<TxInInfo>,
    //TODO: costMdls
    slot_config: &SlotConfig,
) -> anyhow::Result<MaybeIndefArray<Redeemer>> {
    let redeemers = tx.transaction_witness_set.redeemer.as_ref();

    let lookup_table = get_script_and_datum_lookup_table(tx, utxos);

    match redeemers {
        Some(rs) => {
            let mut collected_redeemers = vec![];
            for redeemer in rs.iter() {
                collected_redeemers.push(eval_redeemer(
                    tx,
                    utxos,
                    slot_config,
                    &redeemer,
                    &lookup_table,
                )?)
            }
            Ok(MaybeIndefArray::Indef(collected_redeemers))
        }
        None => Ok(MaybeIndefArray::Indef(vec![])),
    }
}

#[cfg(test)]
mod tests {
    use pallas_codec::utils::MaybeIndefArray;
    use pallas_primitives::{
        babbage::{TransactionInput, TransactionOutput},
        Fragment, ToHash,
    };
    use pallas_traverse::{Era, MultiEraTx};
    use uplc::PlutusData;

    use super::{eval_tx, SlotConfig, TxInInfo};

    #[test]
    fn test_eval() {
        /*
        {-# INLINEABLE mintTestValidator #-}
        mintTestValidator :: () -> Api.ScriptContext -> Bool
        mintTestValidator _ ctx = Api.txInfoFee txInfo == Api.txInfoFee txInfo && (case Api.txInfoSignatories txInfo of [] -> True)

          where
            txInfo :: Api.TxInfo
            txInfo = Api.scriptContextTxInfo ctx */

        let tx_bytes = hex::decode("84a80081825820975c17a4fed0051be622328efa548e206657d2b65a19224bf6ff8132571e6a5002018282581d60b6c8794e9a7a26599440a4d0fd79cd07644d15917ff13694f1f67235821a000f41f0a1581cc4f241450001af08f3ddbaf9335db79883cbcd81071b8e3508de3055a1400a82581d60b6c8794e9a7a26599440a4d0fd79cd07644d15917ff13694f1f672351a0084192f021a00053b6109a1581cc4f241450001af08f3ddbaf9335db79883cbcd81071b8e3508de3055a1400a0b5820b4f96b0acec8beff2adededa8ba317bcac92174f0f65ccefe569b9a6aac7375a0d818258206c732139de33e916342707de2aebef2252c781640326ff37b86ec99d97f1ba8d011082581d60b6c8794e9a7a26599440a4d0fd79cd07644d15917ff13694f1f672351b00000001af0cdfa2111a0007d912a3008182582031ae74f8058527afb305d7495b10a99422d9337fc199e1f28044f2c477a0f9465840b8b97b7c3b4e19ecfc2fcd9884ee53a35887ee6e4d36901b9ecbac3fe032d7e8a4358305afa573a86396e378255651ed03501906e9def450e588d4bb36f42a050581840100d87980821a000b68081a0cf3a5bf06815909b25909af010000323322323232323232323232323232323232323232332232323232323232323233223232223232533533223233025323233355300f1200135028502623500122333553012120013502b50292350012233350012330314800000488cc0c80080048cc0c400520000013355300e1200123500122335501c0023335001233553012120012350012233550200023550140010012233355500f0150020012335530121200123500122335502000235501300100133355500a01000200130105002300f5001533532350012222222222220045001102a2216135001220023333573466e1cd55ce9baa0044800080808c98c8080cd5ce01081000f1999ab9a3370e6aae7540092000233221233001003002323232323232323232323232323333573466e1cd55cea8062400046666666666664444444444442466666666666600201a01801601401201000e00c00a00800600466a03803a6ae854030cd4070074d5d0a80599a80e00f1aba1500a3335502075ca03e6ae854024ccd54081d7280f9aba1500833501c02835742a00e666aa040052eb4d5d0a8031919191999ab9a3370e6aae75400920002332212330010030023232323333573466e1cd55cea8012400046644246600200600466a066eb4d5d0a801181a1aba135744a004464c6406c66ae700dc0d80d04d55cf280089baa00135742a0046464646666ae68cdc39aab9d5002480008cc8848cc00400c008cd40cdd69aba150023034357426ae8940088c98c80d8cd5ce01b81b01a09aab9e5001137540026ae84d5d1280111931901919ab9c033032030135573ca00226ea8004d5d0a80299a80e3ae35742a008666aa04004a40026ae85400cccd54081d710009aba150023027357426ae8940088c98c80b8cd5ce01781701609aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226aae7940044dd50009aba150023017357426ae8940088c98c8080cd5ce01081000f080f89931900f99ab9c4901035054350001f135573ca00226ea8004444888ccd54c010480054040cd54c01c480048d400488cd54054008d54024004ccd54c0104800488d4008894cd4ccd54c03048004c8cd409c88ccd400c88008008004d40048800448cc004894cd400840b040040a48d400488cc028008014018400c4cd405001000d4044004cd54c01c480048d400488c8cd5405800cc004014c8004d540a4894cd40044d5402800c884d4008894cd4cc03000802044888cc0080280104c01800c008c8004d5408888448894cd40044008884cc014008ccd54c01c480040140100044484888c00c0104484888c004010c8004d5407c8844894cd400454038884cd403cc010008cd54c01848004010004c8004d5407888448894cd40044d400c88004884ccd401488008c010008ccd54c01c4800401401000488ccd5cd19b8f00200101e01d2350012222222222220091232230023758002640026aa038446666aae7c004940288cd4024c010d5d080118019aba2002015232323333573466e1cd55cea80124000466442466002006004601a6ae854008c014d5d09aba2500223263201533573802c02a02626aae7940044dd50009191919191999ab9a3370e6aae75401120002333322221233330010050040030023232323333573466e1cd55cea80124000466442466002006004602c6ae854008cd4040054d5d09aba2500223263201a33573803603403026aae7940044dd50009aba150043335500875ca00e6ae85400cc8c8c8cccd5cd19b875001480108c84888c008010d5d09aab9e500323333573466e1d4009200223212223001004375c6ae84d55cf280211999ab9a3370ea00690001091100191931900e19ab9c01d01c01a019018135573aa00226ea8004d5d0a80119a8063ae357426ae8940088c98c8058cd5ce00b80b00a09aba25001135744a00226aae7940044dd5000899aa800bae75a224464460046eac004c8004d5406488c8cccd55cf80112804119a80399aa80498031aab9d5002300535573ca00460086ae8800c04c4d5d08008891001091091198008020018891091980080180109119191999ab9a3370ea0029000119091180100198029aba135573ca00646666ae68cdc3a801240044244002464c6402066ae700440400380344d55cea80089baa001232323333573466e1d400520062321222230040053007357426aae79400c8cccd5cd19b875002480108c848888c008014c024d5d09aab9e500423333573466e1d400d20022321222230010053007357426aae7940148cccd5cd19b875004480008c848888c00c014dd71aba135573ca00c464c6402066ae7004404003803403002c4d55cea80089baa001232323333573466e1cd55cea80124000466442466002006004600a6ae854008dd69aba135744a004464c6401866ae700340300284d55cf280089baa0012323333573466e1cd55cea800a400046eb8d5d09aab9e500223263200a33573801601401026ea80048c8c8c8c8c8cccd5cd19b8750014803084888888800c8cccd5cd19b875002480288488888880108cccd5cd19b875003480208cc8848888888cc004024020dd71aba15005375a6ae84d5d1280291999ab9a3370ea00890031199109111111198010048041bae35742a00e6eb8d5d09aba2500723333573466e1d40152004233221222222233006009008300c35742a0126eb8d5d09aba2500923333573466e1d40192002232122222223007008300d357426aae79402c8cccd5cd19b875007480008c848888888c014020c038d5d09aab9e500c23263201333573802802602202001e01c01a01801626aae7540104d55cf280189aab9e5002135573ca00226ea80048c8c8c8c8cccd5cd19b875001480088ccc888488ccc00401401000cdd69aba15004375a6ae85400cdd69aba135744a00646666ae68cdc3a80124000464244600400660106ae84d55cf280311931900619ab9c00d00c00a009135573aa00626ae8940044d55cf280089baa001232323333573466e1d400520022321223001003375c6ae84d55cf280191999ab9a3370ea004900011909118010019bae357426aae7940108c98c8024cd5ce00500480380309aab9d50011375400224464646666ae68cdc3a800a40084244400246666ae68cdc3a8012400446424446006008600c6ae84d55cf280211999ab9a3370ea00690001091100111931900519ab9c00b00a008007006135573aa00226ea80048c8cccd5cd19b8750014800880348cccd5cd19b8750024800080348c98c8018cd5ce00380300200189aab9d37540029309000a4810350543100112330010020072253350021001100612335002223335003220020020013500122001122123300100300222333573466e1c00800401000c488008488004448c8c00400488cc00cc008008005f5f6").unwrap();

        let raw_inputs = hex::decode("84825820b16778c9cf065d9efeefe37ec269b4fc5107ecdbd0dd6bf3274b224165c2edd9008258206c732139de33e916342707de2aebef2252c781640326ff37b86ec99d97f1ba8d01825820975c17a4fed0051be622328efa548e206657d2b65a19224bf6ff8132571e6a500282582018f86700660fc88d0370a8f95ea58f75507e6b27a18a17925ad3b1777eb0d77600").unwrap();
        let raw_outputs = hex::decode("8482581d60b6c8794e9a7a26599440a4d0fd79cd07644d15917ff13694f1f67235821a000f8548a1581c15be994a64bdb79dde7fe080d8e7ff81b33a9e4860e9ee0d857a8e85a144576177610182581d60b6c8794e9a7a26599440a4d0fd79cd07644d15917ff13694f1f672351b00000001af14b8b482581d60b6c8794e9a7a26599440a4d0fd79cd07644d15917ff13694f1f672351a0098968082581d60b6c8794e9a7a26599440a4d0fd79cd07644d15917ff13694f1f672351a00acd8c6").unwrap();

        let inputs = MaybeIndefArray::<TransactionInput>::decode_fragment(&raw_inputs).unwrap();
        let outputs = MaybeIndefArray::<TransactionOutput>::decode_fragment(&raw_outputs).unwrap();

        let utxos: MaybeIndefArray<TxInInfo> = MaybeIndefArray::Indef(
            inputs
                .iter()
                .zip(outputs.iter())
                .map(|(input, output)| TxInInfo {
                    out_ref: input.clone(),
                    resolved: output.clone(),
                })
                .collect(),
        );

        let slot_config = SlotConfig {
            zero_time: 1660003200000, // Preview network
            slot_length: 1000,
        };

        let multi_era_tx = MultiEraTx::decode(Era::Babbage, &tx_bytes)
            .or_else(|_| MultiEraTx::decode(Era::Alonzo, &tx_bytes))
            .unwrap();
        match multi_era_tx {
            MultiEraTx::Babbage(tx) => {
                let redeemers = eval_tx(&tx, &utxos, &slot_config).unwrap();

                println!("{:?}", redeemers.len());
            }
            _ => unreachable!(),
        };
    }

    #[test]
    fn test_eval_1() {
        /*
        {-# INLINEABLE mintTestValidator #-}
        mintTestValidator :: () -> Api.ScriptContext -> Bool
        mintTestValidator _ ctx = Api.txInfoFee txInfo == Api.txInfoFee txInfo

          where
            txInfo :: Api.TxInfo
            txInfo = Api.scriptContextTxInfo ctx */

        let tx_bytes = hex::decode("84a800818258206c732139de33e916342707de2aebef2252c781640326ff37b86ec99d97f1ba8d01018282581d60b6c8794e9a7a26599440a4d0fd79cd07644d15917ff13694f1f67235821a000f41f0a1581c88c9dfd60601e22509d58b904c2730fe2bdef6a52a41a6f376b0ba94a1400a82581d60b6c8794e9a7a26599440a4d0fd79cd07644d15917ff13694f1f672351b00000001af004152021a0005357209a1581c88c9dfd60601e22509d58b904c2730fe2bdef6a52a41a6f376b0ba94a1400a0b5820ff1a62ad8cb2d73ffb8687471e2c99b48bf3b067966a7ea9285f95adcee708a20d818258206c732139de33e916342707de2aebef2252c781640326ff37b86ec99d97f1ba8d011082581d60b6c8794e9a7a26599440a4d0fd79cd07644d15917ff13694f1f672351b00000001af0ce889111a0007d02ba3008182582031ae74f8058527afb305d7495b10a99422d9337fc199e1f28044f2c477a0f9465840b3dde25e3a2b825d3f120955211e722cc4a7c65fa67697076f2725a2ed0adec0d4bfc742934fe7c29d475bb0630aed1b1cdcf5fac9e06d84976455a661b0dc080581840100d87980821a000b46701a0cd5772f068159099a59099701000032332232323232323232323232323232323232323322323232323232323232332232322232325335332232323233355300f1200135027502623500122333553012120013502a50292350012233350012330304800000488cc0c40080048cc0c000520000013355300e1200123500122335501c0023335001233553012120012350012233550200023550140010012233355500f0150020012335530121200123500122335502000235501300100133355500a01000200130105002300f5001135001220023333573466e1cd55ce9baa0044800080808c98c8080cd5ce01081000f1999ab9a3370e6aae7540092000233221233001003002323232323232323232323232323333573466e1cd55cea8062400046666666666664444444444442466666666666600201a01801601401201000e00c00a00800600466a03803a6ae854030cd4070074d5d0a80599a80e00f1aba1500a3335502075ca03e6ae854024ccd54081d7280f9aba1500833501c02835742a00e666aa040052eb4d5d0a8031919191999ab9a3370e6aae75400920002332212330010030023232323333573466e1cd55cea8012400046644246600200600466a066eb4d5d0a801181a1aba135744a004464c6406c66ae700dc0d80d04d55cf280089baa00135742a0046464646666ae68cdc39aab9d5002480008cc8848cc00400c008cd40cdd69aba150023034357426ae8940088c98c80d8cd5ce01b81b01a09aab9e5001137540026ae84d5d1280111931901919ab9c033032030135573ca00226ea8004d5d0a80299a80e3ae35742a008666aa04004a40026ae85400cccd54081d710009aba150023027357426ae8940088c98c80b8cd5ce01781701609aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226aae7940044dd50009aba150023017357426ae8940088c98c8080cd5ce01081000f080f89931900f99ab9c491035054350001f135573ca00226ea8004444888ccd54c010480054040cd54c01c480048d400488cd54054008d54024004ccd54c0104800488d4008894cd4ccd54c03048004c8cd409888ccd400c88008008004d40048800448cc004894cd400840ac40040a08d400488cc028008014018400c4cd405001000d4044004cd54c01c480048d400488c8cd5405800cc004014c8004d540a0894cd40044d5402800c884d4008894cd4cc03000802044888cc0080280104c01800c008c8004d5408488448894cd40044008884cc014008ccd54c01c480040140100044484888c00c0104484888c004010c8004d540788844894cd400454038884cd403cc010008cd54c01848004010004c8004d5407488448894cd40044d400c88004884ccd401488008c010008ccd54c01c4800401401000488ccd5cd19b8f00200101d01c2350012222222222220091232230023758002640026aa036446666aae7c004940288cd4024c010d5d080118019aba2002015232323333573466e1cd55cea80124000466442466002006004601a6ae854008c014d5d09aba2500223263201533573802c02a02626aae7940044dd50009191919191999ab9a3370e6aae75401120002333322221233330010050040030023232323333573466e1cd55cea80124000466442466002006004602c6ae854008cd4040054d5d09aba2500223263201a33573803603403026aae7940044dd50009aba150043335500875ca00e6ae85400cc8c8c8cccd5cd19b875001480108c84888c008010d5d09aab9e500323333573466e1d4009200223212223001004375c6ae84d55cf280211999ab9a3370ea00690001091100191931900e19ab9c01d01c01a019018135573aa00226ea8004d5d0a80119a8063ae357426ae8940088c98c8058cd5ce00b80b00a09aba25001135744a00226aae7940044dd5000899aa800bae75a224464460046eac004c8004d5406088c8cccd55cf80112804119a80399aa80498031aab9d5002300535573ca00460086ae8800c04c4d5d08008891001091091198008020018891091980080180109119191999ab9a3370ea0029000119091180100198029aba135573ca00646666ae68cdc3a801240044244002464c6402066ae700440400380344d55cea80089baa001232323333573466e1d400520062321222230040053007357426aae79400c8cccd5cd19b875002480108c848888c008014c024d5d09aab9e500423333573466e1d400d20022321222230010053007357426aae7940148cccd5cd19b875004480008c848888c00c014dd71aba135573ca00c464c6402066ae7004404003803403002c4d55cea80089baa001232323333573466e1cd55cea80124000466442466002006004600a6ae854008dd69aba135744a004464c6401866ae700340300284d55cf280089baa0012323333573466e1cd55cea800a400046eb8d5d09aab9e500223263200a33573801601401026ea80048c8c8c8c8c8cccd5cd19b8750014803084888888800c8cccd5cd19b875002480288488888880108cccd5cd19b875003480208cc8848888888cc004024020dd71aba15005375a6ae84d5d1280291999ab9a3370ea00890031199109111111198010048041bae35742a00e6eb8d5d09aba2500723333573466e1d40152004233221222222233006009008300c35742a0126eb8d5d09aba2500923333573466e1d40192002232122222223007008300d357426aae79402c8cccd5cd19b875007480008c848888888c014020c038d5d09aab9e500c23263201333573802802602202001e01c01a01801626aae7540104d55cf280189aab9e5002135573ca00226ea80048c8c8c8c8cccd5cd19b875001480088ccc888488ccc00401401000cdd69aba15004375a6ae85400cdd69aba135744a00646666ae68cdc3a80124000464244600400660106ae84d55cf280311931900619ab9c00d00c00a009135573aa00626ae8940044d55cf280089baa001232323333573466e1d400520022321223001003375c6ae84d55cf280191999ab9a3370ea004900011909118010019bae357426aae7940108c98c8024cd5ce00500480380309aab9d50011375400224464646666ae68cdc3a800a40084244400246666ae68cdc3a8012400446424446006008600c6ae84d55cf280211999ab9a3370ea00690001091100111931900519ab9c00b00a008007006135573aa00226ea80048c8cccd5cd19b8750014800880308cccd5cd19b8750024800080308c98c8018cd5ce00380300200189aab9d37540029309000a4810350543100112330012253350021001100700612335002223335003220020020013500122001122123300100300222333573466e1c00800401000c488008488004448c8c00400488cc00cc0080080041f5f6").unwrap();

        let raw_inputs = hex::decode("84825820b16778c9cf065d9efeefe37ec269b4fc5107ecdbd0dd6bf3274b224165c2edd9008258206c732139de33e916342707de2aebef2252c781640326ff37b86ec99d97f1ba8d01825820975c17a4fed0051be622328efa548e206657d2b65a19224bf6ff8132571e6a500282582018f86700660fc88d0370a8f95ea58f75507e6b27a18a17925ad3b1777eb0d77600").unwrap();
        let raw_outputs = hex::decode("8482581d60b6c8794e9a7a26599440a4d0fd79cd07644d15917ff13694f1f67235821a000f8548a1581c15be994a64bdb79dde7fe080d8e7ff81b33a9e4860e9ee0d857a8e85a144576177610182581d60b6c8794e9a7a26599440a4d0fd79cd07644d15917ff13694f1f672351b00000001af14b8b482581d60b6c8794e9a7a26599440a4d0fd79cd07644d15917ff13694f1f672351a0098968082581d60b6c8794e9a7a26599440a4d0fd79cd07644d15917ff13694f1f672351a00acd8c6").unwrap();

        let inputs = MaybeIndefArray::<TransactionInput>::decode_fragment(&raw_inputs).unwrap();
        let outputs = MaybeIndefArray::<TransactionOutput>::decode_fragment(&raw_outputs).unwrap();

        let utxos: MaybeIndefArray<TxInInfo> = MaybeIndefArray::Indef(
            inputs
                .iter()
                .zip(outputs.iter())
                .map(|(input, output)| TxInInfo {
                    out_ref: input.clone(),
                    resolved: output.clone(),
                })
                .collect(),
        );

        let slot_config = SlotConfig {
            zero_time: 1660003200000, // Preview network
            slot_length: 1000,
        };

        let multi_era_tx = MultiEraTx::decode(Era::Babbage, &tx_bytes)
            .or_else(|_| MultiEraTx::decode(Era::Alonzo, &tx_bytes))
            .unwrap();
        match multi_era_tx {
            MultiEraTx::Babbage(tx) => {
                let redeemers = eval_tx(&tx, &utxos, &slot_config).unwrap();

                println!("{:?}", redeemers.len());
            }
            _ => unreachable!(),
        };
    }
}
