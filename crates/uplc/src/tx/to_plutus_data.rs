use pallas_addresses::{Address, ShelleyDelegationPart, ShelleyPaymentPart, StakePayload};
use pallas_codec::utils::{AnyUInt, Bytes, Int, KeyValuePairs};
use pallas_crypto::hash::Hash;
use pallas_primitives::babbage::{AssetName, BigInt, Constr, Mint, PlutusData, ScriptRef};
use pallas_primitives::babbage::{
    Certificate, DatumOption, PseudoScript, Redeemer, StakeCredential, TransactionInput,
    TransactionOutput, Value,
};
use pallas_traverse::ComputeHash;

use crate::machine::runtime::{convert_constr_to_tag, ANY_TAG};

use super::script_context::{ScriptContext, ScriptPurpose, TimeRange, TxInInfo, TxInfo, TxOut};

fn wrap_with_constr(index: u64, data: PlutusData) -> PlutusData {
    let converted = convert_constr_to_tag(index);
    PlutusData::Constr(Constr {
        tag: converted.unwrap_or(ANY_TAG),
        any_constructor: converted.map_or(Some(index), |_| None),
        fields: vec![data],
    })
}

fn wrap_multiple_with_constr(index: u64, data: Vec<PlutusData>) -> PlutusData {
    let converted = convert_constr_to_tag(index);
    PlutusData::Constr(Constr {
        tag: converted.unwrap_or(ANY_TAG),
        any_constructor: converted.map_or(Some(index), |_| None),
        fields: data,
    })
}

fn empty_constr(index: u64) -> PlutusData {
    let converted = convert_constr_to_tag(index);
    PlutusData::Constr(Constr {
        tag: converted.unwrap_or(ANY_TAG),
        any_constructor: converted.map_or(Some(index), |_| None),
        fields: vec![],
    })
}

pub trait ToPlutusData {
    fn to_plutus_data(&self) -> PlutusData;
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct MintValue {
    pub mint_value: Mint,
}

impl ToPlutusData for Address {
    fn to_plutus_data(&self) -> PlutusData {
        match self {
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
                        Some(StakeCredential::AddrKeyhash(*stake_keyhash)).to_plutus_data()
                    }
                    ShelleyDelegationPart::Script(script_hash) => {
                        Some(StakeCredential::Scripthash(*script_hash)).to_plutus_data()
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
            Address::Stake(stake_address) => {
                // This is right now only used in Withdrawals (Reward account)
                match stake_address.payload() {
                    StakePayload::Stake(stake_keyhash) => {
                        StakeCredential::AddrKeyhash(*stake_keyhash).to_plutus_data()
                    }
                    StakePayload::Script(script_hash) => {
                        StakeCredential::Scripthash(*script_hash).to_plutus_data()
                    }
                }
            }
            _ => unreachable!(),
        }
    }
}

impl ToPlutusData for TransactionInput {
    fn to_plutus_data(&self) -> PlutusData {
        wrap_multiple_with_constr(
            0,
            vec![
                wrap_with_constr(0, self.transaction_id.to_plutus_data()),
                PlutusData::BigInt(BigInt::Int((self.index as i128).try_into().unwrap())),
            ],
        )
    }
}

impl<const BYTES: usize> ToPlutusData for Hash<BYTES> {
    fn to_plutus_data(&self) -> PlutusData {
        PlutusData::BoundedBytes(self.to_vec().into())
    }
}

impl ToPlutusData for Bytes {
    fn to_plutus_data(&self) -> PlutusData {
        PlutusData::BoundedBytes(self.to_vec().into())
    }
}

impl<K: ToPlutusData, V: ToPlutusData> ToPlutusData for (K, V) {
    fn to_plutus_data(&self) -> PlutusData {
        wrap_multiple_with_constr(0, vec![self.0.to_plutus_data(), self.1.to_plutus_data()])
    }
}

impl<A> ToPlutusData for Vec<A>
where
    A: ToPlutusData,
{
    fn to_plutus_data(&self) -> PlutusData {
        PlutusData::Array(self.iter().map(|p| p.to_plutus_data()).collect())
    }
}

impl<K, V> ToPlutusData for KeyValuePairs<K, V>
where
    K: ToPlutusData + Clone,
    V: ToPlutusData + Clone,
{
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
            AnyUInt::U8(n) => PlutusData::BigInt(BigInt::Int(Int::from(*n as i64))),
            AnyUInt::U16(n) => PlutusData::BigInt(BigInt::Int(Int::from(*n as i64))),
            AnyUInt::U32(n) => PlutusData::BigInt(BigInt::Int(Int::from(*n as i64))),
            AnyUInt::U64(n) => PlutusData::BigInt(BigInt::Int(Int::try_from(*n as i128).unwrap())),
            AnyUInt::MajorByte(n) => PlutusData::BigInt(BigInt::Int(Int::from(*n as i64))), // is this correct? I don't know exactly what is does
        }
    }
}

impl ToPlutusData for Int {
    fn to_plutus_data(&self) -> PlutusData {
        PlutusData::BigInt(BigInt::Int(*self))
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
        PlutusData::BigInt(BigInt::Int(Int::try_from(*self as i128).unwrap()))
    }
}

impl ToPlutusData for Value {
    fn to_plutus_data(&self) -> PlutusData {
        match self {
            Value::Coin(coin) => PlutusData::Map(KeyValuePairs::Def(vec![(
                Bytes::from(vec![]).to_plutus_data(),
                PlutusData::Map(KeyValuePairs::Def(vec![(
                    AssetName::from(vec![]).to_plutus_data(),
                    coin.to_plutus_data(),
                )])),
            )])),
            Value::Multiasset(coin, multiassets) => {
                let mut data_vec: Vec<(PlutusData, PlutusData)> = vec![(
                    Bytes::from(vec![]).to_plutus_data(),
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

impl ToPlutusData for MintValue {
    fn to_plutus_data(&self) -> PlutusData {
        let mut data_vec: Vec<(PlutusData, PlutusData)> = vec![(
            Bytes::from(vec![]).to_plutus_data(),
            PlutusData::Map(KeyValuePairs::Def(vec![(
                AssetName::from(vec![]).to_plutus_data(),
                0_i64.to_plutus_data(),
            )])),
        )];

        for (policy_id, assets) in self.mint_value.iter() {
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

impl ToPlutusData for ScriptRef {
    fn to_plutus_data(&self) -> PlutusData {
        match &self {
            PseudoScript::NativeScript(native_script) => {
                native_script.compute_hash().to_plutus_data()
            }
            PseudoScript::PlutusV1Script(plutus_v1) => plutus_v1.compute_hash().to_plutus_data(),
            PseudoScript::PlutusV2Script(plutus_v2) => plutus_v2.compute_hash().to_plutus_data(),
        }
    }
}

impl ToPlutusData for TxOut {
    fn to_plutus_data(&self) -> PlutusData {
        match self {
            TxOut::V1(output) => match output {
                TransactionOutput::Legacy(legacy_output) => wrap_multiple_with_constr(
                    0,
                    vec![
                        Address::from_bytes(&legacy_output.address)
                            .unwrap()
                            .to_plutus_data(),
                        legacy_output.amount.to_plutus_data(),
                        legacy_output.datum_hash.to_plutus_data(),
                    ],
                ),
                TransactionOutput::PostAlonzo(post_alonzo_output) => wrap_multiple_with_constr(
                    0,
                    vec![
                        Address::from_bytes(&post_alonzo_output.address)
                            .unwrap()
                            .to_plutus_data(),
                        post_alonzo_output.value.to_plutus_data(),
                        match post_alonzo_output.datum_option {
                            Some(DatumOption::Hash(hash)) => Some(hash).to_plutus_data(),
                            _ => None::<Hash<32>>.to_plutus_data(),
                        },
                    ],
                ),
            },
            TxOut::V2(output) => match output {
                TransactionOutput::Legacy(legacy_output) => wrap_multiple_with_constr(
                    0,
                    vec![
                        Address::from_bytes(&legacy_output.address)
                            .unwrap()
                            .to_plutus_data(),
                        legacy_output.amount.to_plutus_data(),
                        match legacy_output.datum_hash {
                            Some(hash) => wrap_with_constr(1, hash.to_plutus_data()),
                            _ => empty_constr(0),
                        },
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
                        post_alonzo_output
                            .script_ref
                            .as_ref()
                            .map(|s| s.clone().unwrap())
                            .to_plutus_data(),
                    ],
                ),
            },
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
        match self {
            TxInfo::V1(tx_info) => wrap_multiple_with_constr(
                0,
                vec![
                    tx_info.inputs.to_plutus_data(),
                    tx_info.outputs.to_plutus_data(),
                    tx_info.fee.to_plutus_data(),
                    tx_info.mint.to_plutus_data(),
                    tx_info.dcert.to_plutus_data(),
                    tx_info.wdrl.to_plutus_data(),
                    tx_info.valid_range.to_plutus_data(),
                    tx_info.signatories.to_plutus_data(),
                    tx_info.data.to_plutus_data(),
                    wrap_with_constr(0, tx_info.id.to_plutus_data()),
                ],
            ),
            TxInfo::V2(tx_info) => wrap_multiple_with_constr(
                0,
                vec![
                    tx_info.inputs.to_plutus_data(),
                    tx_info.reference_inputs.to_plutus_data(),
                    tx_info.outputs.to_plutus_data(),
                    tx_info.fee.to_plutus_data(),
                    tx_info.mint.to_plutus_data(),
                    tx_info.dcert.to_plutus_data(),
                    tx_info.wdrl.to_plutus_data(),
                    tx_info.valid_range.to_plutus_data(),
                    tx_info.signatories.to_plutus_data(),
                    tx_info.redeemers.to_plutus_data(),
                    tx_info.data.to_plutus_data(),
                    wrap_with_constr(0, tx_info.id.to_plutus_data()),
                ],
            ),
        }
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
