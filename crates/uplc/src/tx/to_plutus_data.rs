use super::script_context::{
    ScriptContext, ScriptInfo, ScriptPurpose, TimeRange, TxInInfo, TxInfo,
};
use crate::{
    ast::Data,
    machine::runtime::{convert_constr_to_tag, ANY_TAG},
    tx::script_context::from_alonzo_output,
};
use pallas_addresses::{
    Address, ShelleyDelegationPart, ShelleyPaymentPart, StakeAddress, StakePayload,
};
use pallas_codec::utils::{AnyUInt, Bytes, Int, KeyValuePairs, NonEmptyKeyValuePairs, Nullable};
use pallas_crypto::hash::Hash;
use pallas_primitives::conway::{
    AssetName, BigInt, Certificate, Coin, Constitution, Constr, DatumOption, GovAction,
    GovActionId, Mint, PlutusData, PolicyId, ProposalProcedure, ProtocolParamUpdate, PseudoScript,
    RationalNumber, Redeemer, ScriptRef, StakeCredential, TransactionInput, TransactionOutput,
    Value,
};
use pallas_traverse::ComputeHash;

fn wrap_multiple_with_constr(index: u64, data: Vec<PlutusData>) -> PlutusData {
    let converted = convert_constr_to_tag(index);
    PlutusData::Constr(Constr {
        tag: converted.unwrap_or(ANY_TAG),
        any_constructor: converted.map_or(Some(index), |_| None),
        fields: data,
    })
}

fn wrap_with_constr(index: u64, data: PlutusData) -> PlutusData {
    wrap_multiple_with_constr(index, vec![data])
}

fn empty_constr(index: u64) -> PlutusData {
    wrap_multiple_with_constr(index, vec![])
}

struct WithWrappedTransactionId<'a, T>(&'a T);

struct WithWrappedStakeCredential<'a, T>(&'a T);

struct WithZeroAdaAsset<'a, T>(&'a T);

struct WithOptionDatum<'a, T>(&'a T);

pub trait ToPlutusData {
    fn to_plutus_data(&self) -> PlutusData;
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct MintValue {
    pub mint_value: Mint,
}

impl ToPlutusData for bool {
    fn to_plutus_data(&self) -> PlutusData {
        match self {
            false => empty_constr(0),
            true => empty_constr(1),
        }
    }
}
impl ToPlutusData for StakeAddress {
    fn to_plutus_data(&self) -> PlutusData {
        match self.payload() {
            StakePayload::Stake(x) => wrap_with_constr(0, x.to_plutus_data()),
            StakePayload::Script(x) => wrap_with_constr(1, x.to_plutus_data()),
        }
    }
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
                    ShelleyDelegationPart::Key(stake_keyhash) => Some(wrap_with_constr(
                        0,
                        StakeCredential::AddrKeyhash(*stake_keyhash).to_plutus_data(),
                    ))
                    .to_plutus_data(),
                    ShelleyDelegationPart::Script(script_hash) => Some(wrap_with_constr(
                        0,
                        StakeCredential::Scripthash(*script_hash).to_plutus_data(),
                    ))
                    .to_plutus_data(),
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
            Address::Stake(stake_address) => stake_address.to_plutus_data(),
            _ => unreachable!(),
        }
    }
}

impl<'a> ToPlutusData for WithWrappedTransactionId<'a, TransactionInput> {
    fn to_plutus_data(&self) -> PlutusData {
        wrap_multiple_with_constr(
            0,
            vec![
                wrap_with_constr(0, self.0.transaction_id.to_plutus_data()),
                PlutusData::BigInt(BigInt::Int((self.0.index as i128).try_into().unwrap())),
            ],
        )
    }
}

impl ToPlutusData for TransactionInput {
    fn to_plutus_data(&self) -> PlutusData {
        wrap_multiple_with_constr(
            0,
            vec![
                self.transaction_id.to_plutus_data(),
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

impl<'a> ToPlutusData for WithWrappedTransactionId<'a, KeyValuePairs<ScriptPurpose, Redeemer>> {
    fn to_plutus_data(&self) -> PlutusData {
        let mut data_vec: Vec<(PlutusData, PlutusData)> = vec![];
        for (key, value) in self.0.iter() {
            data_vec.push((
                WithWrappedTransactionId(key).to_plutus_data(),
                value.to_plutus_data(),
            ))
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

impl ToPlutusData for u32 {
    fn to_plutus_data(&self) -> PlutusData {
        PlutusData::BigInt(BigInt::Int(Int::try_from(*self as i128).unwrap()))
    }
}

impl ToPlutusData for u64 {
    fn to_plutus_data(&self) -> PlutusData {
        PlutusData::BigInt(BigInt::Int(Int::try_from(*self as i128).unwrap()))
    }
}

impl ToPlutusData for usize {
    fn to_plutus_data(&self) -> PlutusData {
        PlutusData::BigInt(BigInt::Int(Int::try_from(*self as i128).unwrap()))
    }
}

impl<'a> ToPlutusData for WithZeroAdaAsset<'a, Value> {
    fn to_plutus_data(&self) -> PlutusData {
        match self.0 {
            Value::Coin(coin) => {
                PlutusData::Map(KeyValuePairs::Def(vec![coin_to_plutus_data(coin)]))
            }
            Value::Multiasset(coin, multiassets) => value_to_plutus_data(
                multiassets.iter(),
                |amount| u64::from(amount).to_plutus_data(),
                vec![coin_to_plutus_data(coin)],
            ),
        }
    }
}

impl ToPlutusData for Value {
    fn to_plutus_data(&self) -> PlutusData {
        match self {
            Value::Coin(coin) => PlutusData::Map(KeyValuePairs::Def(if *coin > 0 {
                vec![coin_to_plutus_data(coin)]
            } else {
                vec![]
            })),
            Value::Multiasset(coin, multiassets) => value_to_plutus_data(
                multiassets.iter(),
                |amount| u64::from(amount).to_plutus_data(),
                if *coin > 0 {
                    vec![coin_to_plutus_data(coin)]
                } else {
                    vec![]
                },
            ),
        }
    }
}

impl<'a> ToPlutusData for WithZeroAdaAsset<'a, MintValue> {
    fn to_plutus_data(&self) -> PlutusData {
        value_to_plutus_data(
            self.0.mint_value.iter(),
            |amount| i64::from(amount).to_plutus_data(),
            vec![(
                Bytes::from(vec![]).to_plutus_data(),
                PlutusData::Map(KeyValuePairs::Def(vec![(
                    AssetName::from(vec![]).to_plutus_data(),
                    0_i64.to_plutus_data(),
                )])),
            )],
        )
    }
}

impl ToPlutusData for MintValue {
    fn to_plutus_data(&self) -> PlutusData {
        value_to_plutus_data(
            self.mint_value.iter(),
            |amount| i64::from(amount).to_plutus_data(),
            vec![],
        )
    }
}

fn value_to_plutus_data<'a, I, Q>(
    mint: I,
    from_quantity: fn(&'a Q) -> PlutusData,
    mut data_vec: Vec<(PlutusData, PlutusData)>,
) -> PlutusData
where
    I: Iterator<Item = &'a (PolicyId, NonEmptyKeyValuePairs<AssetName, Q>)>,
    Q: Clone,
{
    for (policy_id, assets) in mint {
        let mut assets_vec = vec![];
        for (asset, amount) in assets.iter() {
            assets_vec.push((asset.to_plutus_data(), from_quantity(amount)));
        }
        data_vec.push((
            policy_id.to_plutus_data(),
            PlutusData::Map(KeyValuePairs::Def(assets_vec)),
        ));
    }

    PlutusData::Map(KeyValuePairs::Def(data_vec))
}

fn coin_to_plutus_data(coin: &Coin) -> (PlutusData, PlutusData) {
    (
        Bytes::from(vec![]).to_plutus_data(),
        PlutusData::Map(KeyValuePairs::Def(vec![(
            AssetName::from(vec![]).to_plutus_data(),
            coin.to_plutus_data(),
        )])),
    )
}

impl ToPlutusData for ScriptRef {
    fn to_plutus_data(&self) -> PlutusData {
        match &self {
            PseudoScript::NativeScript(native_script) => {
                native_script.compute_hash().to_plutus_data()
            }
            PseudoScript::PlutusV1Script(plutus_v1) => plutus_v1.compute_hash().to_plutus_data(),
            PseudoScript::PlutusV2Script(plutus_v2) => plutus_v2.compute_hash().to_plutus_data(),
            PseudoScript::PlutusV3Script(plutus_v3) => plutus_v3.compute_hash().to_plutus_data(),
        }
    }
}

impl<'a> ToPlutusData for WithOptionDatum<'a, WithZeroAdaAsset<'a, Vec<TransactionOutput>>> {
    fn to_plutus_data(&self) -> PlutusData {
        PlutusData::Array(
            self.0
                 .0
                .iter()
                .map(|p| WithOptionDatum(&WithZeroAdaAsset(p)).to_plutus_data())
                .collect(),
        )
    }
}

impl<'a> ToPlutusData for WithZeroAdaAsset<'a, Vec<TransactionOutput>> {
    fn to_plutus_data(&self) -> PlutusData {
        PlutusData::Array(
            self.0
                .iter()
                .map(|p| WithZeroAdaAsset(p).to_plutus_data())
                .collect(),
        )
    }
}

impl<'a> ToPlutusData for WithOptionDatum<'a, WithZeroAdaAsset<'a, TransactionOutput>> {
    fn to_plutus_data(&self) -> PlutusData {
        match self.0 .0 {
            TransactionOutput::Legacy(legacy_output) => {
                WithOptionDatum(&WithZeroAdaAsset(&from_alonzo_output(legacy_output)))
                    .to_plutus_data()
            }

            TransactionOutput::PostAlonzo(post_alonzo_output) => wrap_multiple_with_constr(
                0,
                vec![
                    Address::from_bytes(&post_alonzo_output.address)
                        .unwrap()
                        .to_plutus_data(),
                    WithZeroAdaAsset(&post_alonzo_output.value).to_plutus_data(),
                    match post_alonzo_output.datum_option {
                        Some(DatumOption::Hash(hash)) => Some(hash).to_plutus_data(),
                        _ => None::<Hash<32>>.to_plutus_data(),
                    },
                ],
            ),
        }
    }
}

impl<'a> ToPlutusData for WithZeroAdaAsset<'a, TransactionOutput> {
    fn to_plutus_data(&self) -> PlutusData {
        match self.0 {
            TransactionOutput::Legacy(legacy_output) => {
                WithZeroAdaAsset(&from_alonzo_output(legacy_output)).to_plutus_data()
            }
            TransactionOutput::PostAlonzo(post_alonzo_output) => wrap_multiple_with_constr(
                0,
                vec![
                    Address::from_bytes(&post_alonzo_output.address)
                        .unwrap()
                        .to_plutus_data(),
                    WithZeroAdaAsset(&post_alonzo_output.value).to_plutus_data(),
                    post_alonzo_output.datum_option.to_plutus_data(),
                    post_alonzo_output
                        .script_ref
                        .as_ref()
                        .map(|s| s.clone().unwrap())
                        .to_plutus_data(),
                ],
            ),
        }
    }
}

impl ToPlutusData for TransactionOutput {
    fn to_plutus_data(&self) -> PlutusData {
        match self {
            TransactionOutput::Legacy(legacy_output) => {
                from_alonzo_output(legacy_output).to_plutus_data()
            }
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
        }
    }
}

impl ToPlutusData for StakeCredential {
    fn to_plutus_data(&self) -> PlutusData {
        match self {
            StakeCredential::AddrKeyhash(addr_keyhas) => {
                wrap_with_constr(0, addr_keyhas.to_plutus_data())
            }
            StakeCredential::Scripthash(script_hash) => {
                wrap_with_constr(1, script_hash.to_plutus_data())
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
            _ => todo!("other certificates"), // Reg(StakeCredential, Coin),
                                              // UnReg(StakeCredential, Coin),
                                              // VoteDeleg(StakeCredential, DRep),
                                              // StakeVoteDeleg(StakeCredential, PoolKeyhash, DRep),
                                              // StakeRegDeleg(StakeCredential, PoolKeyhash, Coin),
                                              // VoteRegDeleg(StakeCredential, DRep, Coin),
                                              // StakeVoteRegDeleg(StakeCredential, PoolKeyhash, DRep, Coin),
                                              // AuthCommitteeHot(CommitteeColdCredential, CommitteeHotCredential),
                                              // ResignCommitteeCold(CommitteeColdCredential, Nullable<Anchor>),
                                              // RegDRepCert(DRepCredential, Coin, Nullable<Anchor>),
                                              // UnRegDRepCert(DRepCredential, Coin),
                                              // UpdateDRepCert(StakeCredential, Nullable<Anchor>),
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

impl<'a> ToPlutusData
    for WithOptionDatum<'a, WithZeroAdaAsset<'a, WithWrappedTransactionId<'a, Vec<TxInInfo>>>>
{
    fn to_plutus_data(&self) -> PlutusData {
        PlutusData::Array(
            self.0
                 .0
                 .0
                .iter()
                .map(|p| {
                    WithOptionDatum(&WithZeroAdaAsset(&WithWrappedTransactionId(p)))
                        .to_plutus_data()
                })
                .collect(),
        )
    }
}

impl<'a> ToPlutusData for WithZeroAdaAsset<'a, WithWrappedTransactionId<'a, Vec<TxInInfo>>> {
    fn to_plutus_data(&self) -> PlutusData {
        PlutusData::Array(
            self.0
                 .0
                .iter()
                .map(|p| WithZeroAdaAsset(&WithWrappedTransactionId(p)).to_plutus_data())
                .collect(),
        )
    }
}

impl<'a> ToPlutusData for WithZeroAdaAsset<'a, WithWrappedTransactionId<'a, TxInInfo>> {
    fn to_plutus_data(&self) -> PlutusData {
        wrap_multiple_with_constr(
            0,
            vec![
                WithWrappedTransactionId(&self.0 .0.out_ref).to_plutus_data(),
                WithZeroAdaAsset(&self.0 .0.resolved).to_plutus_data(),
            ],
        )
    }
}

impl<'a> ToPlutusData
    for WithOptionDatum<'a, WithZeroAdaAsset<'a, WithWrappedTransactionId<'a, TxInInfo>>>
{
    fn to_plutus_data(&self) -> PlutusData {
        wrap_multiple_with_constr(
            0,
            vec![
                WithWrappedTransactionId(&self.0 .0 .0.out_ref).to_plutus_data(),
                WithOptionDatum(&WithZeroAdaAsset(&self.0 .0 .0.resolved)).to_plutus_data(),
            ],
        )
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

impl<'a> ToPlutusData for WithWrappedTransactionId<'a, ScriptPurpose> {
    fn to_plutus_data(&self) -> PlutusData {
        match self.0 {
            ScriptPurpose::Spending(out_ref, ()) => {
                wrap_with_constr(1, WithWrappedTransactionId(out_ref).to_plutus_data())
            }
            // NOTE: This is a _small_ abuse of the 'WithWrappedTransactionId'. We know the wrapped
            // is needed for V1 and V2, and it also appears that for V1 and V2, the certifying
            // purpose mustn't include the certificate index. So, we also short-circuit it here.
            ScriptPurpose::Certifying(_, dcert) => wrap_with_constr(3, dcert.to_plutus_data()),
            otherwise => otherwise.to_plutus_data(),
        }
    }
}

impl ToPlutusData for ScriptPurpose {
    fn to_plutus_data(&self) -> PlutusData {
        match self {
            ScriptPurpose::Minting(policy_id) => wrap_with_constr(0, policy_id.to_plutus_data()),
            ScriptPurpose::Spending(out_ref, ()) => wrap_with_constr(1, out_ref.to_plutus_data()),
            ScriptPurpose::Rewarding(stake_credential) => {
                wrap_with_constr(2, stake_credential.to_plutus_data())
            }
            ScriptPurpose::Certifying(ix, dcert) => {
                wrap_multiple_with_constr(3, vec![ix.to_plutus_data(), dcert.to_plutus_data()])
            }
            ScriptPurpose::Proposing(ix, procedure) => {
                wrap_multiple_with_constr(5, vec![ix.to_plutus_data(), procedure.to_plutus_data()])
            }
        }
    }
}

impl ToPlutusData for ProposalProcedure {
    fn to_plutus_data(&self) -> PlutusData {
        wrap_multiple_with_constr(
            0,
            vec![
                self.deposit.to_plutus_data(),
                Address::from_bytes(&self.reward_account)
                    .unwrap()
                    .to_plutus_data(),
                self.gov_action.to_plutus_data(),
            ],
        )
    }
}

impl<T> ToPlutusData for Nullable<T>
where
    T: ToPlutusData + Clone,
{
    fn to_plutus_data(&self) -> PlutusData {
        match self {
            Nullable::Some(t) => wrap_with_constr(0, t.to_plutus_data()),
            Nullable::Null | Nullable::Undefined => empty_constr(1),
        }
    }
}

impl ToPlutusData for GovActionId {
    fn to_plutus_data(&self) -> PlutusData {
        wrap_multiple_with_constr(
            0,
            vec![
                self.transaction_id.to_plutus_data(),
                self.action_index.to_plutus_data(),
            ],
        )
    }
}

impl ToPlutusData for ProtocolParamUpdate {
    fn to_plutus_data(&self) -> PlutusData {
        todo!("ToPlutusData for ProtocolParamUpdate")
    }
}

impl ToPlutusData for GovAction {
    fn to_plutus_data(&self) -> PlutusData {
        match self {
            GovAction::ParameterChange(previous_action, params, guardrail) => {
                wrap_multiple_with_constr(
                    0,
                    vec![
                        previous_action.to_plutus_data(),
                        params.as_ref().to_plutus_data(),
                        guardrail.to_plutus_data(),
                    ],
                )
            }
            GovAction::HardForkInitiation(previous_action, version) => wrap_multiple_with_constr(
                1,
                vec![previous_action.to_plutus_data(), version.to_plutus_data()],
            ),
            GovAction::TreasuryWithdrawals(withdrawals, guardrail) => wrap_multiple_with_constr(
                2,
                vec![
                    KeyValuePairs::from(
                        withdrawals
                            .iter()
                            .map(|(reward_account, amount)| {
                                (
                                    Address::from_bytes(reward_account)
                                        .expect("Invalid stake address in treasury withdrawal?"),
                                    *amount,
                                )
                            })
                            .collect::<Vec<_>>(),
                    )
                    .to_plutus_data(),
                    guardrail.to_plutus_data(),
                ],
            ),
            GovAction::NoConfidence(previous_action) => {
                wrap_with_constr(3, previous_action.to_plutus_data())
            }
            GovAction::UpdateCommittee(previous_action, removed, added, quorum) => {
                wrap_multiple_with_constr(
                    4,
                    vec![
                        previous_action.to_plutus_data(),
                        removed.to_plutus_data(),
                        added.to_plutus_data(),
                        quorum.to_plutus_data(),
                    ],
                )
            }
            GovAction::NewConstitution(previous_action, constitution) => wrap_multiple_with_constr(
                5,
                vec![
                    previous_action.to_plutus_data(),
                    constitution.to_plutus_data(),
                ],
            ),
            GovAction::Information => empty_constr(6),
        }
    }
}

impl ToPlutusData for Constitution {
    fn to_plutus_data(&self) -> PlutusData {
        wrap_with_constr(0, self.guardrail_script.to_plutus_data())
    }
}

impl ToPlutusData for RationalNumber {
    fn to_plutus_data(&self) -> PlutusData {
        (self.numerator, self.denominator).to_plutus_data()
    }
}

impl<'a> ToPlutusData for WithWrappedStakeCredential<'a, Vec<(Address, Coin)>> {
    fn to_plutus_data(&self) -> PlutusData {
        self.0
            .iter()
            .map(|(reward_account, amount)| {
                (
                    wrap_with_constr(0, reward_account.to_plutus_data()),
                    *amount,
                )
            })
            .collect::<Vec<_>>()
            .to_plutus_data()
    }
}

impl<'a> ToPlutusData for WithWrappedStakeCredential<'a, KeyValuePairs<Address, Coin>> {
    fn to_plutus_data(&self) -> PlutusData {
        KeyValuePairs::from(
            self.0
                .iter()
                .map(|(reward_account, amount)| {
                    (
                        wrap_with_constr(0, reward_account.to_plutus_data()),
                        *amount,
                    )
                })
                .collect::<Vec<_>>(),
        )
        .to_plutus_data()
    }
}

impl<T> ToPlutusData for ScriptInfo<T>
where
    T: ToPlutusData,
{
    fn to_plutus_data(&self) -> PlutusData {
        match self {
            ScriptInfo::Minting(policy_id) => wrap_with_constr(0, policy_id.to_plutus_data()),
            ScriptInfo::Spending(out_ref, datum) => {
                wrap_multiple_with_constr(1, vec![out_ref.to_plutus_data(), datum.to_plutus_data()])
            }
            ScriptInfo::Rewarding(stake_credential) => {
                wrap_with_constr(2, stake_credential.to_plutus_data())
            }
            ScriptInfo::Certifying(ix, dcert) => {
                wrap_multiple_with_constr(3, vec![ix.to_plutus_data(), dcert.to_plutus_data()])
            }
            ScriptInfo::Proposing(ix, procedure) => {
                wrap_multiple_with_constr(5, vec![ix.to_plutus_data(), procedure.to_plutus_data()])
            }
        }
    }
}

impl ToPlutusData for TxInfo {
    fn to_plutus_data(&self) -> PlutusData {
        match self {
            TxInfo::V1(tx_info) => wrap_multiple_with_constr(
                0,
                vec![
                    WithOptionDatum(&WithZeroAdaAsset(&WithWrappedTransactionId(
                        &tx_info.inputs,
                    )))
                    .to_plutus_data(),
                    WithOptionDatum(&WithZeroAdaAsset(&tx_info.outputs)).to_plutus_data(),
                    WithZeroAdaAsset(&tx_info.fee).to_plutus_data(),
                    WithZeroAdaAsset(&tx_info.mint).to_plutus_data(),
                    tx_info.certificates.to_plutus_data(),
                    WithWrappedStakeCredential(&tx_info.withdrawals).to_plutus_data(),
                    tx_info.valid_range.to_plutus_data(),
                    tx_info.signatories.to_plutus_data(),
                    tx_info.data.to_plutus_data(),
                    wrap_with_constr(0, tx_info.id.to_plutus_data()),
                ],
            ),
            TxInfo::V2(tx_info) => wrap_multiple_with_constr(
                0,
                vec![
                    WithZeroAdaAsset(&WithWrappedTransactionId(&tx_info.inputs)).to_plutus_data(),
                    WithZeroAdaAsset(&WithWrappedTransactionId(&tx_info.reference_inputs))
                        .to_plutus_data(),
                    WithZeroAdaAsset(&tx_info.outputs).to_plutus_data(),
                    WithZeroAdaAsset(&tx_info.fee).to_plutus_data(),
                    WithZeroAdaAsset(&tx_info.mint).to_plutus_data(),
                    tx_info.certificates.to_plutus_data(),
                    WithWrappedStakeCredential(&tx_info.withdrawals).to_plutus_data(),
                    tx_info.valid_range.to_plutus_data(),
                    tx_info.signatories.to_plutus_data(),
                    WithWrappedTransactionId(&tx_info.redeemers).to_plutus_data(),
                    tx_info.data.to_plutus_data(),
                    wrap_with_constr(0, tx_info.id.to_plutus_data()),
                ],
            ),
            TxInfo::V3(tx_info) => wrap_multiple_with_constr(
                0,
                vec![
                    tx_info.inputs.to_plutus_data(),
                    tx_info.reference_inputs.to_plutus_data(),
                    tx_info.outputs.to_plutus_data(),
                    tx_info.fee.to_plutus_data(),
                    tx_info.mint.to_plutus_data(),
                    tx_info.certificates.to_plutus_data(),
                    tx_info.withdrawals.to_plutus_data(),
                    tx_info.valid_range.to_plutus_data(),
                    tx_info.signatories.to_plutus_data(),
                    tx_info.redeemers.to_plutus_data(),
                    tx_info.data.to_plutus_data(),
                    tx_info.id.to_plutus_data(),
                    Data::map(vec![]), // TODO tx_info.votes :: Map Voter (Map GovernanceActionId Vote)
                    tx_info.proposal_procedures.to_plutus_data(),
                    empty_constr(1), // TODO tx_info.current_treasury_amount :: Haskell.Maybe V2.Lovelace
                    empty_constr(1), // TODO tx_info.treasury_donation :: Haskell.Maybe V2.Lovelace
                ],
            ),
        }
    }
}

impl ToPlutusData for ScriptContext {
    fn to_plutus_data(&self) -> PlutusData {
        match self {
            ScriptContext::V1V2 { tx_info, purpose } => wrap_multiple_with_constr(
                0,
                vec![
                    tx_info.to_plutus_data(),
                    WithWrappedTransactionId(purpose.as_ref()).to_plutus_data(),
                ],
            ),
            ScriptContext::V3 {
                tx_info,
                redeemer,
                purpose,
            } => wrap_multiple_with_constr(
                0,
                vec![
                    tx_info.to_plutus_data(),
                    redeemer.to_plutus_data(),
                    purpose.to_plutus_data(),
                ],
            ),
        }
    }
}
