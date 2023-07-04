use num_bigint::ToBigInt;
use uplc::{
    ast::{Constant, Term, Type, Name},
    Constr, PlutusData, parser::term,
};

// Examples sourced from https://github.com/input-output-hk/plutus/issues/4751#issuecomment-1538377273

fn round_trip(old_term: Term::<Name>, pp: &str) {
    //assert_eq!(old_term.to_pretty(), pp);
    let new_term = term(pp).expect("failed to parse");
    assert_eq!(new_term, old_term);
}

#[test]
fn constant_list_integer() {
    round_trip(
        Term::<Name>::Constant(
            Constant::ProtoList(
                Type::Integer.into(),
                vec![
                    Constant::Integer(0.to_bigint().unwrap()),
                    Constant::Integer(1.to_bigint().unwrap()),
                    Constant::Integer(2.to_bigint().unwrap()),
                ],
            )
            .into(),
        ),
        "(con (list integer) [0, 1, 2])",
    );
}

#[test]
fn constant_pair_bool_bytestring() {
    round_trip(
        Term::<Name>::Constant(
            Constant::ProtoPair(
                Type::Bool.into(),
                Type::ByteString.into(),
                Constant::Bool(true).into(),
                Constant::ByteString(vec![0x01, 0x23, 0x45]).into(),
            )
            .into(),
        ),
        "(con (pair bool bytestring) (True, #012345))",
    );
}

#[test]
fn constant_pair_unit_string() {
    round_trip(
        Term::<Name>::Constant(
            Constant::ProtoPair(
                Type::Unit.into(),
                Type::String.into(),
                Constant::Unit.into(),
                Constant::String("hello universe".into()).into(),
            )
            .into(),
        ),
        "(con (pair unit string) ((), \"hello universe\"))",
    )
}

#[test]
fn constant_deeply_nested_list() {
    let t0 = Type::Integer;
    let t1 = Type::List(t0.clone().into());
    let t2 = Type::List(t1.clone().into());
    round_trip(
        Term::<Name>::Constant(
            Constant::ProtoList(
                t2,
                vec![
                    Constant::ProtoList(
                        t1.clone(),
                        vec![
                            Constant::ProtoList(
                                t0.clone(),
                                vec![Constant::Integer(-1.to_bigint().unwrap())],
                            ),
                            Constant::ProtoList(t0.clone(), vec![]),
                        ],
                    ),
                    Constant::ProtoList(
                        t1.clone(),
                        vec![
                            Constant::ProtoList(t0.clone(), vec![]),
                            Constant::ProtoList(
                                t0.clone(),
                                vec![
                                    Constant::Integer(2.to_bigint().unwrap()),
                                    Constant::Integer(3.to_bigint().unwrap()),
                                ],
                            ),
                        ],
                    ),
                ],
            )
            .into(),
        ),
        "(con (list (list (list integer))) [[[-1], []], [[], [2, 3]]])",
    );
}

#[test]
fn constant_data_constr() {
    round_trip(
        Term::<Name>::Constant(
            Constant::Data(PlutusData::Constr(Constr::<PlutusData> {
                tag: 1,
                any_constructor: None,
                fields: vec![PlutusData::BigInt(pallas_primitives::alonzo::BigInt::Int(
                    2.into(),
                ))],
            }))
            .into(),
        ),
        "(con data (Constr 1 [I 2]))",
    );
}

#[test]
fn constant_data_map() {
    round_trip(
        Term::<Name>::Constant(
            Constant::Data(PlutusData::Map(uplc::KeyValuePairs::Def(vec![
                (
                    PlutusData::BigInt(pallas_primitives::alonzo::BigInt::Int(0.into())),
                    PlutusData::BoundedBytes(vec![0x00].into()),
                ),
                (
                    PlutusData::BigInt(pallas_primitives::alonzo::BigInt::Int(1.into())),
                    PlutusData::BoundedBytes(vec![0x0f].into()),
                ),
            ])))
            .into(),
        ),
        "(con data (Map [(I 0, B #00), (I 1, B #0f)]))",
    );
}

#[test]
fn constant_data_list() {
    round_trip(
        Term::<Name>::Constant(
            Constant::Data(PlutusData::Array(vec![
                PlutusData::BigInt(pallas_primitives::alonzo::BigInt::Int(0.into())),
                PlutusData::BigInt(pallas_primitives::alonzo::BigInt::Int(1.into())),
            ]))
            .into(),
        ),
        "(con data (List [I 0, I 1]))",
    );
}

#[test]
fn constant_data_int() {
    round_trip(
        Term::<Name>::Constant(
            Constant::Data(PlutusData::BigInt(pallas_primitives::alonzo::BigInt::Int(
                2.into(),
            )))
            .into(),
        ),
        "(con data (I 2))",
    );

    // TODO: large integers currently encode as bytestrings, which isn't great
    /*
    let term = Term::<DeBruijn>::Constant(Constant::Data(
        PlutusData::BigInt(pallas_primitives::alonzo::BigInt::BigUInt(vec![2,3,4].into())),
    ).into());
    assert_eq!(term.to_pretty(), "(con data (I 131844))");
    let term = Term::<DeBruijn>::Constant(Constant::Data(
        PlutusData::BigInt(pallas_primitives::alonzo::BigInt::BigNInt(vec![FF,FD,FC,FC].into())),
    ).into());
    assert_eq!(term.to_pretty(), "(con data (I -131844))");
    */
}

#[test]
fn constant_data_bytes() {
    round_trip(
        Term::<Name>::Constant(
            Constant::Data(PlutusData::BoundedBytes(vec![0x00, 0x1A].into())).into(),
        ),
        "(con data (B #001a))",
    );
}
