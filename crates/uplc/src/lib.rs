pub mod ast;
pub mod builtins;
mod debruijn;
mod flat;
pub mod machine;
pub mod parser;
mod pretty;
pub mod program_builder;

pub use pallas_primitives::alonzo::PlutusData;
use pallas_primitives::Fragment;

pub fn plutus_data(bytes: &[u8]) -> PlutusData {
    PlutusData::decode_fragment(bytes).unwrap()
}

pub fn plutus_data_to_bytes(data: &PlutusData) -> Vec<u8> {
    PlutusData::encode_fragment(data).unwrap()
}
