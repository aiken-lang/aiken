use pallas_addresses::{Address, PaymentKeyHash};
use pallas_codec::utils::{KeyValuePairs, MaybeIndefArray};
use pallas_crypto::hash::Hash;
use pallas_primitives::babbage::{
    BigInt, Certificate, Constr, DatumHash, PolicyId, PostAlonzoTransactionOutput, Redeemer,
    StakeCredential, TransactionInput, Value, Withdrawals,
};
use pallas_traverse::OutputRef;
use std::str::FromStr;
use uplc::PlutusData;

use crate::args::ResolvedInput;

pub fn get_tx_in_info(resolved_inputs: &[ResolvedInput]) -> anyhow::Result<Vec<PlutusData>> {
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

type SlotRange = (Slot, Slot);
type PosixTimeRange = (PosixTime, PosixTime);

struct SlotConfig {
    slot_length: u64,
    zero_time: PosixTime,
}

fn slot_to_begin_posix_time(slot: Slot, sc: &SlotConfig) -> PosixTime {
    let ms_after_begin = slot * sc.slot_length;
    sc.zero_time + ms_after_begin
}

fn slot_range_to_posix_time_range(slot_range: SlotRange, sc: &SlotConfig) -> PosixTimeRange {
    (
        slot_to_begin_posix_time(slot_range.0, sc),
        slot_to_begin_posix_time(slot_range.1, sc),
    )
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
                PlutusData::BoundedBytes(hex::decode(self.transaction_id.clone()).unwrap().into()),
                PlutusData::BigInt(BigInt::Int(self.index.into())),
            ]),
        })
    }
}

// impl ToPlutusData for LegacyTransactionOutput {
//     fn to_plutus_data(&self) -> PlutusData {}
// }

// impl ToPlutusData for PostAlonzoTransactionOutput {
//     fn to_plutus_data(&self) -> PlutusData {}
// }

pub struct TxInInfo {
    out_ref: OutputRef,
    resolved: PostAlonzoTransactionOutput,
}

// Plutus V2
pub enum ScriptPurpose {
    Minting(PolicyId),
    Spending(OutputRef),
    Reward(StakeCredential),
    Certifying(Certificate),
}

pub struct TxInfo {
    inputs: Vec<TxInInfo>,
    reference_inputs: Vec<TxInInfo>,
    outputs: Vec<PostAlonzoTransactionOutput>,
    fee: Value,
    mint: Value,
    dcert: Vec<Certificate>,
    wdrl: Withdrawals,
    valid_range: PosixTimeRange,
    signatories: Vec<PaymentKeyHash>,
    redeemers: KeyValuePairs<ScriptPurpose, Redeemer>,
    data: KeyValuePairs<DatumHash, PlutusData>,
    id: Hash<32>,
}
