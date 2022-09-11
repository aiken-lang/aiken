use pallas_addresses::{
    Address, ScriptHash, ShelleyDelegationPart, ShelleyPaymentPart, StakePayload,
};
use pallas_codec::{
    minicbor::{bytes::ByteVec, data::Int},
    utils::{AnyUInt, KeyValuePairs, MaybeIndefArray},
};
use pallas_crypto::hash::Hash;
use pallas_primitives::{
    babbage::{
        AddrKeyhash, AssetName, BigInt, Certificate, Constr, CostModel, DatumHash, DatumOption,
        ExUnits, Language, Mint, PlutusV1Script, PlutusV2Script, PolicyId, Redeemer, RedeemerTag,
        RewardAccount, Script, ScriptRef, StakeCredential, TransactionInput, TransactionOutput, Tx,
        Value, Withdrawals,
    },
    ToHash,
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

fn wrap_with_constr(tag: u64, data: PlutusData) -> PlutusData {
    PlutusData::Constr(Constr {
        tag,
        any_constructor: None,
        fields: MaybeIndefArray::Indef(vec![data]),
    })
}

fn empty_constr(tag: u64) -> PlutusData {
    PlutusData::Constr(Constr {
        tag,
        any_constructor: None,
        fields: MaybeIndefArray::Indef(vec![]),
    })
}

pub trait ToPlutusData {
    fn to_plutus_data(&self) -> PlutusData;
}

impl ToPlutusData for Address {
    fn to_plutus_data(&self) -> PlutusData {
        match self {
            Address::Byron(byron_address) => {
                PlutusData::Constr(Constr {
                    tag: 0,
                    any_constructor: None,
                    fields: MaybeIndefArray::Indef(vec![
                        //addressCredential
                        wrap_with_constr(0, byron_address.decode().unwrap().root.to_plutus_data()),
                        //addressStakeCredential
                        None::<StakeCredential>.to_plutus_data(),
                    ]),
                })
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
                    ShelleyDelegationPart::Pointer(pointer) => Some(PlutusData::Constr(Constr {
                        tag: 1,
                        any_constructor: None,
                        fields: MaybeIndefArray::Indef(vec![
                            pointer.slot().to_plutus_data(),
                            pointer.tx_idx().to_plutus_data(),
                            pointer.cert_idx().to_plutus_data(),
                        ]),
                    }))
                    .to_plutus_data(),
                    ShelleyDelegationPart::Null => None::<StakeCredential>.to_plutus_data(),
                };

                PlutusData::Constr(Constr {
                    tag: 0,
                    any_constructor: None,
                    fields: MaybeIndefArray::Indef(vec![
                        payment_part_plutus_data,
                        stake_part_plutus_data,
                    ]),
                })
            }
            Address::Stake(_) => unreachable!(),
        }
    }
}

impl ToPlutusData for TransactionInput {
    fn to_plutus_data(&self) -> PlutusData {
        PlutusData::Constr(Constr {
            tag: 0,
            any_constructor: None,
            fields: MaybeIndefArray::Indef(vec![
                wrap_with_constr(0, self.transaction_id.to_plutus_data()),
                PlutusData::BigInt(BigInt::Int(self.index.into())),
            ]),
        })
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
            TransactionOutput::Legacy(legacy_output) => PlutusData::Constr(Constr {
                tag: 0,
                any_constructor: None,
                fields: MaybeIndefArray::Indef(vec![
                    Address::from_bytes(&legacy_output.address)
                        .unwrap()
                        .to_plutus_data(),
                    legacy_output.amount.to_plutus_data(),
                    None::<DatumOption>.to_plutus_data(),
                    None::<ScriptRef>.to_plutus_data(),
                ]),
            }),
            TransactionOutput::PostAlonzo(post_alonzo_output) => PlutusData::Constr(Constr {
                tag: 0,
                any_constructor: None,
                fields: MaybeIndefArray::Indef(vec![
                    Address::from_bytes(&post_alonzo_output.address)
                        .unwrap()
                        .to_plutus_data(),
                    post_alonzo_output.value.to_plutus_data(),
                    post_alonzo_output.datum_option.to_plutus_data(),
                    post_alonzo_output.script_ref.to_plutus_data(),
                ]),
            }),
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
                PlutusData::Constr(Constr {
                    tag: 2,
                    any_constructor: None,
                    fields: MaybeIndefArray::Indef(vec![
                        stake_credential.to_plutus_data(),
                        pool_keyhash.to_plutus_data(),
                    ]),
                })
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
            } => PlutusData::Constr(Constr {
                tag: 3,
                any_constructor: None,
                fields: MaybeIndefArray::Indef(vec![
                    operator.to_plutus_data(),
                    vrf_keyhash.to_plutus_data(),
                ]),
            }),
            Certificate::PoolRetirement(pool_keyhash, epoch) => PlutusData::Constr(Constr {
                tag: 4,
                any_constructor: None,
                fields: MaybeIndefArray::Indef(vec![
                    pool_keyhash.to_plutus_data(),
                    epoch.to_plutus_data(),
                ]),
            }),
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
                PlutusData::Constr(Constr {
                    tag: 0,
                    any_constructor: None,
                    fields: MaybeIndefArray::Indef(vec![
                        // LowerBound
                        PlutusData::Constr(Constr {
                            tag: 0,
                            any_constructor: None,
                            fields: MaybeIndefArray::Indef(vec![
                                // Finite
                                wrap_with_constr(1, lower_bound.to_plutus_data()),
                                // Closure
                                true.to_plutus_data(),
                            ]),
                        }), //UpperBound
                        PlutusData::Constr(Constr {
                            tag: 0,
                            any_constructor: None,
                            fields: MaybeIndefArray::Indef(vec![
                                // PosInf
                                empty_constr(2),
                                // Closure
                                true.to_plutus_data(),
                            ]),
                        }),
                    ]),
                })
            }
            TimeRange {
                lower_bound: None,
                upper_bound: Some(upper_bound),
            } => {
                PlutusData::Constr(Constr {
                    tag: 0,
                    any_constructor: None,
                    fields: MaybeIndefArray::Indef(vec![
                        // LowerBound
                        PlutusData::Constr(Constr {
                            tag: 0,
                            any_constructor: None,
                            fields: MaybeIndefArray::Indef(vec![
                                // NegInf
                                empty_constr(0),
                                // Closure
                                true.to_plutus_data(),
                            ]),
                        }), //UpperBound
                        PlutusData::Constr(Constr {
                            tag: 0,
                            any_constructor: None,
                            fields: MaybeIndefArray::Indef(vec![
                                // Finite
                                wrap_with_constr(1, upper_bound.to_plutus_data()),
                                // Closure
                                true.to_plutus_data(),
                            ]),
                        }),
                    ]),
                })
            }
            TimeRange {
                lower_bound: Some(lower_bound),
                upper_bound: Some(upper_bound),
            } => {
                PlutusData::Constr(Constr {
                    tag: 0,
                    any_constructor: None,
                    fields: MaybeIndefArray::Indef(vec![
                        // LowerBound
                        PlutusData::Constr(Constr {
                            tag: 0,
                            any_constructor: None,
                            fields: MaybeIndefArray::Indef(vec![
                                // Finite
                                wrap_with_constr(1, lower_bound.to_plutus_data()),
                                // Closure
                                true.to_plutus_data(),
                            ]),
                        }), //UpperBound
                        PlutusData::Constr(Constr {
                            tag: 0,
                            any_constructor: None,
                            fields: MaybeIndefArray::Indef(vec![
                                // Finite
                                wrap_with_constr(1, upper_bound.to_plutus_data()),
                                // Closure
                                false.to_plutus_data(),
                            ]),
                        }),
                    ]),
                })
            }
            TimeRange {
                lower_bound: None,
                upper_bound: None,
            } => {
                PlutusData::Constr(Constr {
                    tag: 0,
                    any_constructor: None,
                    fields: MaybeIndefArray::Indef(vec![
                        // LowerBound
                        PlutusData::Constr(Constr {
                            tag: 0,
                            any_constructor: None,
                            fields: MaybeIndefArray::Indef(vec![
                                // NegInf
                                empty_constr(0),
                                // Closure
                                true.to_plutus_data(),
                            ]),
                        }), //UpperBound
                        PlutusData::Constr(Constr {
                            tag: 0,
                            any_constructor: None,
                            fields: MaybeIndefArray::Indef(vec![
                                // PosInf
                                empty_constr(2),
                                // Closure
                                true.to_plutus_data(),
                            ]),
                        }),
                    ]),
                })
            }
        }
    }
}

impl ToPlutusData for TxInInfo {
    fn to_plutus_data(&self) -> PlutusData {
        PlutusData::Constr(Constr {
            tag: 0,
            any_constructor: None,
            fields: MaybeIndefArray::Indef(vec![
                self.out_ref.to_plutus_data(),
                self.resolved.to_plutus_data(),
            ]),
        })
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
        PlutusData::Constr(Constr {
            tag: 0,
            any_constructor: None,
            fields: MaybeIndefArray::Indef(vec![
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
            ]),
        })
    }
}

impl ToPlutusData for ScriptContext {
    fn to_plutus_data(&self) -> PlutusData {
        PlutusData::Constr(Constr {
            tag: 0,
            any_constructor: None,
            fields: MaybeIndefArray::Indef(vec![
                self.tx_info.to_plutus_data(),
                self.purpose.to_plutus_data(),
            ]),
        })
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
    tx: &Tx,
    utxos: &MaybeIndefArray<TxInInfo>,
    slot_config: &SlotConfig,
) -> anyhow::Result<TxInfo> {
    let body = tx.transaction_body.clone();

    let inputs = get_tx_in_info(&body.inputs, &utxos)?;
    let reference_inputs = get_tx_in_info(
        &body
            .reference_inputs
            .unwrap_or(MaybeIndefArray::Indef(vec![])),
        &utxos,
    )?;
    let outputs = body.outputs;
    let fee = Value::Coin(AnyUInt::U64(body.fee));
    let mint = body.mint.unwrap_or(KeyValuePairs::Indef(vec![]));
    let dcert = body.certificates.unwrap_or(MaybeIndefArray::Indef(vec![]));
    let wdrl = body.withdrawals.unwrap_or(KeyValuePairs::Indef(vec![]));
    let valid_range = slot_range_to_posix_time_range(
        TimeRange {
            lower_bound: body.validity_interval_start,
            upper_bound: body.ttl,
        },
        &slot_config,
    );
    let signatories = body
        .required_signers
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

            // Certificate::StakeRegistration(stake_credential) => {
            //     let script_hash = match stake_credential {
            //         StakeCredential::Scripthash(hash) => hash.clone(),
            //         _ => unreachable!(),
            //     };
            //     let script = lookup_table.scripts.get(&script_hash).unwrap();
            //     ExecutionPurpose::NoDatum(script.clone())
            // }
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
    tx: &Tx,
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
        scripts.insert(script.to_hash(), ScriptVersion::PlutusV1(script.clone()));
    }

    for script in scripts_v2_witnesses.iter() {
        scripts.insert(script.to_hash(), ScriptVersion::PlutusV2(script.clone()));
    }

    // discovery in utxos (script ref)

    for utxo in utxos.iter() {
        match &utxo.resolved {
            TransactionOutput::Legacy(_) => {}
            TransactionOutput::PostAlonzo(output) => match &output.script_ref {
                Some(script) => match &script.0 {
                    Script::PlutusV1Script(v1) => {
                        scripts.insert(v1.to_hash(), ScriptVersion::PlutusV1(v1.clone()));
                    }
                    Script::PlutusV2Script(v2) => {
                        scripts.insert(v2.to_hash(), ScriptVersion::PlutusV2(v2.clone()));
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
    tx: &Tx,
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
                        mem: result.1.mem as u32,
                        steps: result.1.cpu as u64,
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

                result.0.unwrap();

                let new_redeemer = Redeemer {
                    tag: redeemer.tag.clone(),
                    index: redeemer.index,
                    data: redeemer.data.clone(),
                    ex_units: ExUnits {
                        mem: result.1.mem as u32,
                        steps: result.1.cpu as u64,
                    },
                };

                Ok(new_redeemer)
            }
        },
    }
}

fn eval_tx(
    tx: &Tx,
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

// TODO: Maybe make ToPlutusData dependent on a Plutus Language so it works for V1 and V2?
