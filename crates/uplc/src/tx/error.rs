use crate::{
    machine::{self, cost_model::ExBudget},
    TransactionInput,
};
use pallas_primitives::conway::Language;

#[derive(thiserror::Error, Debug, miette::Diagnostic)]
pub enum Error {
    #[error("{0}")]
    Address(#[from] pallas_addresses::Error),
    #[error("Only shelley reward addresses can be a part of withdrawals")]
    BadWithdrawalAddress,
    #[error("{0}")]
    FlatDecode(#[from] pallas_codec::flat::de::Error),
    #[error("{0}")]
    FragmentDecode(#[from] pallas_primitives::Error),
    #[error("{}\n\n{:#?}\n\n{}", .0, .1, .2.join("\n"))]
    Machine(machine::Error, ExBudget, Vec<String>),
    #[error("Native script can't be executed in phase-two")]
    NativeScriptPhaseTwo,
    #[error("Can't eval without redeemers")]
    NoRedeemers,
    #[error("Mismatch in required redeemers: {} {}", .missing.join(" "), .extra.join(" "))]
    RequiredRedeemersMismatch {
        missing: Vec<String>,
        extra: Vec<String>,
    },
    #[error("Extraneous redeemer")]
    ExtraneousRedeemer,
    #[error("Resolved Input not found.")]
    ResolvedInputNotFound(TransactionInput),
    #[error("Redeemer points to a non-script withdrawal.")]
    NonScriptWithdrawal,
    #[error("Stake credential points to a non-script withdrawal.")]
    NonScriptStakeCredential,
    #[error("Cost model not found for language: {:?}.", .0)]
    CostModelNotFound(Language),
    #[error("Wrong era, Please use Babbage or Alonzo: {0}")]
    WrongEra(#[from] pallas_codec::minicbor::decode::Error),
    #[error("Byron address not allowed in Plutus.")]
    ByronAddressNotAllowed,
    #[error("Inline datum not allowed in PlutusV1.")]
    InlineDatumNotAllowed,
    #[error("Script and input reference not allowed in PlutusV1.")]
    ScriptAndInputRefNotAllowed,
    #[error("Address doesn't contain a payment credential.")]
    NoPaymentCredential,
    #[error("Missing required datum: {}", hash)]
    MissingRequiredDatum { hash: String },
    #[error("Missing required script: {}", hash)]
    MissingRequiredScript { hash: String },
    #[error("Missing required inline datum or datum hash in script input.")]
    MissingRequiredInlineDatumOrHash,
    #[error("Redeemer points to an unsupported certificate type.")]
    UnsupportedCertificateType,
    #[error("Redeemer ({}, {}): {}", tag, index, err)]
    RedeemerError {
        tag: String,
        index: u32,
        err: Box<Error>,
    },
    #[error("Missing script for redeemer")]
    MissingScriptForRedeemer,
    #[error("Failed to apply parameters to Plutus script.")]
    ApplyParamsError,
}
