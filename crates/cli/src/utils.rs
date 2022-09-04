use pallas_addresses::Address;
use pallas_codec::utils::{KeyValuePairs, MaybeIndefArray};
use pallas_primitives::babbage::{BigInt, Constr};
use uplc::PlutusData;

use crate::args::ResolvedInput;

pub fn get_tx_in_info(resolved_inputs: &[ResolvedInput]) -> anyhow::Result<Vec<PlutusData>> {
    let mut tx_in_info = Vec::new();

    for resolved_input in resolved_inputs {
        let tx_out_ref = PlutusData::Constr(Constr {
            tag: 0,
            any_constructor: None,
            fields: MaybeIndefArray::Indef(vec![
                PlutusData::BoundedBytes(hex::decode(resolved_input.input.tx_hash.clone())?.into()),
                PlutusData::BigInt(BigInt::Int(resolved_input.input.index.into())),
            ]),
        });

        let address = Address::from_bech32(&resolved_input.output.address)?;

        let payment_tag = match address.typeid() % 2 {
            0 => 0,
            1 => 1,
            _ => unreachable!(),
        };
        let stake_tag = match address.typeid() {
            0 | 1 => Some(0),
            2 | 3 => Some(1),
            _ => None,
        };

        let (payment_part, stake_part) = match address {
            Address::Shelley(s) => (s.payment().to_vec(), s.delegation().to_vec()),
            _ => unreachable!(),
        };

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
                }),
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
