use pallas_addresses::{Address, StakePayload};
use pallas_codec::{
    minicbor::{bytes::ByteVec, data::Int},
    utils::{AnyUInt, KeyValuePairs, MaybeIndefArray},
};
use pallas_crypto::hash::Hash;
use pallas_primitives::{
    babbage::{
        AddrKeyhash, AssetName, BigInt, Certificate, Constr, DatumHash, DatumOption, Mint,
        PolicyId, Redeemer, RedeemerTag, RewardAccount, Script, ScriptRef, StakeCredential,
        TransactionInput, TransactionOutput, Tx, Value, Withdrawals,
    },
    ToHash,
};
use std::{str::FromStr, vec};
use uplc::PlutusData;

use crate::args::ResolvedInput;

pub fn get_tx_in_info_old(resolved_inputs: &[ResolvedInput]) -> anyhow::Result<Vec<PlutusData>> {
    let mut tx_in_info = Vec::new();

    for resolved_input in resolved_inputs {
        let tx_out_ref = TransactionInput {
            transaction_id: Hash::from_str(resolved_input.input.tx_hash.as_str())?, // Not sure if this is the best approach?
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

//---- Time conversion: slot range => posix time range

type Slot = u64;
type PosixTime = u64; // in milliseconds

struct TimeRange<A> {
    lower_bound: A,
    upper_bound: A,
}

struct SlotConfig {
    slot_length: u64,
    zero_time: PosixTime,
}

fn slot_to_begin_posix_time(slot: Slot, sc: &SlotConfig) -> PosixTime {
    let ms_after_begin = slot * sc.slot_length;
    sc.zero_time + ms_after_begin
}

fn slot_range_to_posix_time_range(
    slot_range: TimeRange<Slot>,
    sc: &SlotConfig,
) -> TimeRange<PosixTime> {
    TimeRange {
        lower_bound: slot_to_begin_posix_time(slot_range.lower_bound, sc),
        upper_bound: slot_to_begin_posix_time(slot_range.upper_bound, sc),
    }
}

// ---------------

pub trait ToPlutusData {
    fn to_plutus_data(&self) -> PlutusData;
}

impl ToPlutusData for Address {
    fn to_plutus_data(&self) -> PlutusData {
        //TOD: Byron address and reward address

        let payment_tag = match self.typeid() % 2 {
            0 => 0,
            1 => 1,
            _ => unreachable!(),
        };
        let stake_tag = match self.typeid() {
            0 | 1 => Some(0),
            2 | 3 => Some(1),
            _ => None,
        };

        let (payment_part, stake_part) = match self {
            Address::Shelley(s) => (s.payment().to_vec(), s.delegation().to_vec()),
            _ => unreachable!(),
        };

        PlutusData::Constr(Constr {
            tag: 0,
            any_constructor: None,
            fields: MaybeIndefArray::Indef(vec![
                // addressCredential
                PlutusData::Constr(Constr {
                    tag: payment_tag,
                    any_constructor: None,
                    fields: MaybeIndefArray::Indef(vec![PlutusData::BoundedBytes(
                        payment_part.into(),
                    )]),
                }),
                // addressStakingCredential
                PlutusData::Constr(Constr {
                    tag: if stake_tag.is_some() { 0 } else { 1 },
                    any_constructor: None,
                    fields: MaybeIndefArray::Indef(match stake_tag {
                        Some(stake_tag) => vec![
                            // StakingCredential
                            PlutusData::Constr(Constr {
                                tag: 0,
                                any_constructor: None,
                                fields: MaybeIndefArray::Indef(vec![
                                    // StakingHash
                                    PlutusData::Constr(Constr {
                                        tag: stake_tag,
                                        any_constructor: None,
                                        fields: MaybeIndefArray::Indef(vec![
                                            PlutusData::BoundedBytes(stake_part.into()),
                                        ]),
                                    }),
                                ]),
                            }),
                        ],
                        None => vec![],
                    }),
                }),
            ]),
        })
    }
}

impl ToPlutusData for TransactionInput {
    fn to_plutus_data(&self) -> PlutusData {
        PlutusData::Constr(Constr {
            tag: 0,
            any_constructor: None,
            fields: MaybeIndefArray::Indef(vec![
                self.transaction_id.to_plutus_data(),
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
            None => PlutusData::Constr(Constr {
                tag: 1,
                any_constructor: None,
                fields: MaybeIndefArray::Indef(vec![]),
            }),
            Some(data) => PlutusData::Constr(Constr {
                tag: 0,
                any_constructor: None,
                fields: MaybeIndefArray::Indef(vec![data.to_plutus_data()]),
            }),
        }
    }
}

impl ToPlutusData for DatumOption {
    fn to_plutus_data(&self) -> PlutusData {
        match self {
            // tag : 0 is NoOutputDatum. Determined after unwrapping Option which is wrapped around DatumOption
            DatumOption::Hash(hash) => PlutusData::Constr(Constr {
                tag: 1,
                any_constructor: None,
                fields: MaybeIndefArray::Indef(vec![hash.to_plutus_data()]),
            }),
            DatumOption::Data(data) => PlutusData::Constr(Constr {
                tag: 2,
                any_constructor: None,
                fields: MaybeIndefArray::Indef(vec![data.0.clone()]), // does data need an extra wrapper constructor?
            }),
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
                    // DatumOption needs to be handled differently a bit. In Haskell it's NoOutputDatum | OutputDatumHash DatumHash | OutputDatum Datum
                    // So we unwrap first to check if it's someting. If it is then turn the unwrapped data to PlutusData, otherwise have None and turn that into PlutusData
                    if post_alonzo_output.datum_option.is_some() {
                        post_alonzo_output
                            .datum_option
                            .clone()
                            .unwrap()
                            .to_plutus_data()
                    } else {
                        None::<DatumOption>.to_plutus_data()
                    },
                    post_alonzo_output.script_ref.to_plutus_data(),
                ]),
            }),
        }
    }
}

impl ToPlutusData for StakeCredential {
    fn to_plutus_data(&self) -> PlutusData {
        match self {
            StakeCredential::AddrKeyhash(addr_keyhas) => PlutusData::Constr(Constr {
                tag: 0,
                any_constructor: None,
                fields: MaybeIndefArray::Indef(vec![addr_keyhas.to_plutus_data()]),
            }),
            StakeCredential::Scripthash(script_hash) => PlutusData::Constr(Constr {
                tag: 1,
                any_constructor: None,
                fields: MaybeIndefArray::Indef(vec![script_hash.to_plutus_data()]),
            }),
        }
    }
}

impl ToPlutusData for Certificate {
    fn to_plutus_data(&self) -> PlutusData {
        match self {
            Certificate::StakeRegistration(stake_credential) => PlutusData::Constr(Constr {
                tag: 0,
                any_constructor: None,
                fields: MaybeIndefArray::Indef(vec![stake_credential.to_plutus_data()]),
            }),
            Certificate::StakeDeregistration(stake_credential) => PlutusData::Constr(Constr {
                tag: 1,
                any_constructor: None,
                fields: MaybeIndefArray::Indef(vec![stake_credential.to_plutus_data()]),
            }),
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
            Certificate::GenesisKeyDelegation(_, _, _) => PlutusData::Constr(Constr {
                tag: 5,
                any_constructor: None,
                fields: MaybeIndefArray::Indef(vec![]),
            }),
            Certificate::MoveInstantaneousRewardsCert(_) => PlutusData::Constr(Constr {
                tag: 6,
                any_constructor: None,
                fields: MaybeIndefArray::Indef(vec![]),
            }),
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

impl ToPlutusData for TimeRange<u64> {
    fn to_plutus_data(&self) -> PlutusData {
        PlutusData::Constr(Constr {
            tag: 0,
            any_constructor: None,
            fields: MaybeIndefArray::Indef(vec![
                PlutusData::Constr(Constr {
                    tag: 0,
                    any_constructor: None,
                    fields: MaybeIndefArray::Indef(vec![self.lower_bound.to_plutus_data()]),
                }),
                PlutusData::Constr(Constr {
                    tag: 1,
                    any_constructor: None,
                    fields: MaybeIndefArray::Indef(vec![self.upper_bound.to_plutus_data()]),
                }),
            ]),
        })
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
            ScriptPurpose::Minting(policy_id) => PlutusData::Constr(Constr {
                tag: 0,
                any_constructor: None,
                fields: MaybeIndefArray::Indef(vec![policy_id.to_plutus_data()]),
            }),
            ScriptPurpose::Spending(out_ref) => PlutusData::Constr(Constr {
                tag: 1,
                any_constructor: None,
                fields: MaybeIndefArray::Indef(vec![out_ref.to_plutus_data()]),
            }),
            ScriptPurpose::Rewarding(stake_credential) => PlutusData::Constr(Constr {
                tag: 3,
                any_constructor: None,
                fields: MaybeIndefArray::Indef(vec![stake_credential.to_plutus_data()]),
            }),
            ScriptPurpose::Certifying(dcert) => PlutusData::Constr(Constr {
                tag: 4,
                any_constructor: None,
                fields: MaybeIndefArray::Indef(vec![dcert.to_plutus_data()]),
            }),
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
                self.id.to_plutus_data(),
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

// Plutus V2 only for now

pub struct TxInInfo {
    out_ref: TransactionInput,
    resolved: TransactionOutput,
}

pub enum ScriptPurpose {
    Minting(PolicyId),
    Spending(TransactionInput),
    Rewarding(StakeCredential),
    Certifying(Certificate),
}

pub struct TxInfo {
    inputs: MaybeIndefArray<TxInInfo>,
    reference_inputs: MaybeIndefArray<TxInInfo>,
    outputs: MaybeIndefArray<TransactionOutput>,
    fee: Value,
    mint: Mint,
    dcert: MaybeIndefArray<Certificate>,
    wdrl: Withdrawals,
    valid_range: TimeRange<PosixTime>,
    signatories: MaybeIndefArray<AddrKeyhash>,
    redeemers: KeyValuePairs<ScriptPurpose, Redeemer>,
    data: KeyValuePairs<DatumHash, PlutusData>,
    id: Hash<32>,
}

pub struct ScriptContext {
    tx_info: TxInfo,
    purpose: ScriptPurpose,
}

fn get_tx_in_info(
    inputs: &MaybeIndefArray<TransactionInput>,
    utxos: &Vec<(TransactionInput, TransactionOutput)>,
) -> anyhow::Result<MaybeIndefArray<TxInInfo>> {
    Ok(MaybeIndefArray::Indef(
        inputs
            .iter()
            .map(|input| {
                let utxo = utxos.iter().find(|utxo| utxo.0 == *input);
                match utxo {
                    Some(u) => TxInInfo {
                        out_ref: input.clone(),
                        resolved: u.1.clone(),
                    },
                    None => panic!(),
                }
            })
            .collect(),
    ))
}

fn get_script_purpose(
    redeemer: &Redeemer,
    inputs: &MaybeIndefArray<TxInInfo>,
    mint: &Mint,
    dcert: &MaybeIndefArray<Certificate>,
    wdrl: &Withdrawals,
) -> anyhow::Result<ScriptPurpose> {
    // sorting according to specs section 4.1: https://hydra.iohk.io/build/18583827/download/1/alonzo-changes.pdf
    let tag = redeemer.tag.clone();
    let index = redeemer.index;
    match tag {
        RedeemerTag::Mint => {
            // sort lexical by policy id
            let mut policy_ids = mint
                .iter()
                .map(|(policy_id, _)| policy_id)
                .collect::<Vec<&ByteVec>>()
                .clone();
            policy_ids.sort();
            let policy_id = policy_ids[index as usize].clone();
            Ok(ScriptPurpose::Minting(policy_id))
        }
        RedeemerTag::Spend => {
            // sort lexical by tx_hash and index
            let mut inputs = inputs
                .iter()
                .map(|input| input.out_ref.clone())
                .collect::<Vec<TransactionInput>>()
                .clone();
            // inputs.sort(); // TODO!!!!!!!!!;
            let input = inputs[index as usize].clone();
            Ok(ScriptPurpose::Spending(input))
        }
        RedeemerTag::Reward => {
            // sort lexical by reward account
            let mut reward_accounts = wdrl
                .iter()
                .map(|(policy_id, _)| policy_id)
                .collect::<Vec<&RewardAccount>>()
                .clone();
            reward_accounts.sort();
            let reward_account = reward_accounts[index as usize];
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
            let cert = dcert[index as usize].clone();
            Ok(ScriptPurpose::Certifying(cert.clone()))
        }
    }
}

fn get_tx_info(
    tx: &Tx,
    utxos: &Vec<(TransactionInput, TransactionOutput)>,
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
            lower_bound: 0,
            upper_bound: 0,
        },
        &slot_config,
    ); // TODO!!!!
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
                    get_script_purpose(&r, &inputs, &mint, &dcert, &wdrl).unwrap(),
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

fn get_script_context(
    tx: &Tx,
    utxos: &Vec<(TransactionInput, TransactionOutput)>,
    slot_config: &SlotConfig,
    redeemer: &Redeemer,
) -> anyhow::Result<ScriptContext> {
    let tx_info = get_tx_info(tx, utxos, slot_config)?;
    let purpose = get_script_purpose(
        redeemer,
        &tx_info.inputs,
        &tx_info.mint,
        &tx_info.dcert,
        &tx_info.wdrl,
    )?;
    Ok(ScriptContext { tx_info, purpose })
}
