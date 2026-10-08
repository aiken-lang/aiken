pub mod ast;
pub mod builder;
pub mod builtins;
pub mod data;
mod debruijn;
pub mod flat;
pub mod machine;
pub mod optimize;
pub mod parser;
mod pretty;
pub mod tx;

pub use pallas_primitives::{
    BigInt, BoundedBytes, Constr, Error, Fragment, KeyValuePairs, MaybeIndefArray, PlutusData,
    conway::{Language, PostAlonzoTransactionOutput, TransactionInput, TransactionOutput, Value},
};
pub use tx::redeemer_tag_to_string;

pub fn plutus_data(bytes: &[u8]) -> Result<PlutusData, Error> {
    PlutusData::decode_fragment(bytes)
}

/// The CBOR the `serialiseData` builtin produces for `data` (see
/// [`ast::Data::serialise`]).
pub fn plutus_data_to_bytes(data: &PlutusData) -> Vec<u8> {
    ast::Data::from(data).serialise()
}
