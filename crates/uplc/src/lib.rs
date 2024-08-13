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

pub use pallas_codec::utils::KeyValuePairs;
pub use pallas_crypto::hash::Hash;
pub use pallas_primitives::{
    alonzo::{BigInt, Constr, PlutusData},
    babbage::{PostAlonzoTransactionOutput, TransactionInput, TransactionOutput, Value},
    Error, Fragment,
};
pub use tx::redeemer_tag_to_string;

pub fn plutus_data(bytes: &[u8]) -> Result<PlutusData, Error> {
    PlutusData::decode_fragment(bytes)
}

pub fn plutus_data_to_bytes(data: &PlutusData) -> Result<Vec<u8>, Error> {
    PlutusData::encode_fragment(data)
}
