use crate::machine;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("{0}")]
    Address(#[from] pallas_addresses::Error),
    #[error("Only shelley reward addresses can be a part of withdrawals")]
    BadWithdrawalAddress,
    #[error("{0}")]
    FragmentDecode(#[from] pallas_primitives::Error),
    #[error("{0}")]
    Machine(#[from] machine::Error),
    #[error("Can't eval without redeemers")]
    NoRedeemers,
    #[error("Mismatch in required redeemers: {} {}", .missing.join(" "), .extra.join(" "))]
    RequiredRedeemersMismatch {
        missing: Vec<String>,
        extra: Vec<String>,
    },
    #[error("Resolved Input not found")]
    ResolvedInputNotFound,
    #[error("A key hash cannot be the hash of a script")]
    ScriptKeyHash,
    #[error("Wrong era, Please use Babbage or Alonzo: {0}")]
    WrongEra(#[from] pallas_codec::minicbor::decode::Error),
}
