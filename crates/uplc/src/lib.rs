pub mod ast;
pub mod builder;
pub mod builtins;
mod debruijn;
pub mod flat;
pub mod machine;
pub mod optimize;
pub mod parser;
mod pretty;
pub mod tx;

use pallas_codec::minicbor;
pub use pallas_primitives::{
    BigInt, BoundedBytes, Constr, Error, Fragment, KeyValuePairs, MaybeIndefArray, PlutusData,
    conway::{Language, PostAlonzoTransactionOutput, TransactionInput, TransactionOutput, Value},
};
pub use tx::redeemer_tag_to_string;

pub fn plutus_data(bytes: &[u8]) -> Result<PlutusData, Error> {
    PlutusData::decode_fragment(bytes)
}

// ---------------- re-encode Plutus Data

// The 'serialise_data' builtin must re-encode PlutusData in a 'canonical' way which mainly differs
// in few ways compared to the current Pallas' encoding:
//
// 1. Arrays are always encoded using indefinite arrays, except when empty. When empty, they're
//    always encoded using definite length.
// 2. Maps are always encoded with definite length, even when empty.
// 3. Constr fields follow the same rules as arrays.
pub fn plutus_data_to_bytes(data: &PlutusData) -> Vec<u8> {
    let mut buffer = Vec::new();
    let mut encoder = minicbor::Encoder::new(&mut buffer);
    reencode_plutus_data(data, &mut encoder);
    buffer
}

fn reencode_plutus_data(data: &PlutusData, e: &mut minicbor::Encoder<&mut Vec<u8>>) {
    match data {
        PlutusData::Constr(constr) => reencode_plutus_constr(constr, e),
        PlutusData::Map(kvs) => reencode_plutus_map(kvs, e),
        PlutusData::Array(array) => reencode_plutus_array(array, e),
        PlutusData::BoundedBytes(bytes) => reencode_plutus_bytes(bytes, e),
        PlutusData::BigInt(i) => reencode_plutus_bigint(i, e),
    };
}

fn reencode_plutus_constr(constr: &Constr<PlutusData>, e: &mut minicbor::Encoder<&mut Vec<u8>>) {
    e.tag(minicbor::data::Tag::new(constr.tag)).unwrap();
    if constr.tag == 102 {
        e.array(2).unwrap();
        e.encode(constr.any_constructor.unwrap_or_default())
            .unwrap();
    }
    reencode_plutus_array(&constr.fields, e);
}

fn reencode_plutus_map(
    kvs: &KeyValuePairs<PlutusData, PlutusData>,
    e: &mut minicbor::Encoder<&mut Vec<u8>>,
) {
    match kvs {
        KeyValuePairs::Def(pairs) | KeyValuePairs::Indef(pairs) => {
            // Force definite encoding for maps.
            e.map(pairs.len() as u64).unwrap();
            for (k, v) in pairs {
                reencode_plutus_data(k, e);
                reencode_plutus_data(v, e);
            }
        }
    }
}

fn reencode_plutus_array(
    array: &MaybeIndefArray<PlutusData>,
    e: &mut minicbor::Encoder<&mut Vec<u8>>,
) {
    match array {
        MaybeIndefArray::Def(vec) | MaybeIndefArray::Indef(vec) => {
            // Mimics default haskell list encoding from cborg:
            // We use indef array for non-empty arrays but definite 0-length array for empty
            if vec.is_empty() {
                e.array(0).unwrap();
            } else {
                e.begin_array().unwrap();
                for v in vec {
                    reencode_plutus_data(v, e);
                }
                e.end().unwrap();
            }
        }
    }
}

fn reencode_plutus_bigint(i: &BigInt, e: &mut minicbor::Encoder<&mut Vec<u8>>) {
    e.encode(i)
        .expect("failed to encode BigInt in a bytes buffer?!");
}

fn reencode_plutus_bytes(bytes: &BoundedBytes, e: &mut minicbor::Encoder<&mut Vec<u8>>) {
    e.encode(bytes)
        .expect("failed to encode BoundedBytes in a bytes buffer?!");
}
