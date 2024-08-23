use super::{ExBudget, Value};
use crate::ast::{NamedDeBruijn, Term, Type};
use num_bigint::BigInt;
use std::string::FromUtf8Error;

#[derive(Debug, Clone, PartialEq, thiserror::Error, miette::Diagnostic)]
pub enum Error {
    #[error("Over budget mem: {} & cpu: {}", .0.mem, .0.cpu)]
    OutOfExError(ExBudget),
    #[error("Invalid Stepkind: {0}")]
    InvalidStepKind(u8),
    #[error(
        "Cannot evaluate an open term:\\n{}",
        indent(redacted(.0.to_pretty(), 10)),
    )]
    OpenTermEvaluated(Term<NamedDeBruijn>),
    #[error("The validator crashed / exited prematurely")]
    EvaluationFailure,
    #[error(
        "Attempted to instantiate a non-polymorphic term\n{:>13} {}",
        "Term",
        indent(redacted(format!("{:#?}", .0), 10)),
    )]
    NonPolymorphicInstantiation(Value),
    #[error(
      "Attempted to apply an argument to a non-function\n{:>13} {}\n{:>13} {}",
      "Thing",
      indent(redacted(format!("{:#?}", .0), 5)),
      "Argument",
      indent(redacted(format!("{:#?}", .1), 5)),
    )]
    NonFunctionalApplication(Value, Value),
    #[error("Attempted to case a non-const:\n\n{0:#?}")]
    NonConstrScrutinized(Value),
    #[error("Cases: {0:#?}\n\n are missing branch for constr:\n\n{1:#?}")]
    MissingCaseBranch(Vec<Term<NamedDeBruijn>>, Value),
    #[error("Type mismatch expected '{0}' got '{1}'")]
    TypeMismatch(Type, Type),
    #[error("Type mismatch expected '(list a)' got '{0}'")]
    ListTypeMismatch(Type),
    #[error("Type mismatch expected '(pair a b)' got '{0}'")]
    PairTypeMismatch(Type),
    #[error("Empty List:\n\n{0:#?}")]
    EmptyList(Value),
    #[error(
        "A builtin received a term argument when something else was expected:\n\n{0}\n\nYou probably forgot to wrap the builtin with a force."
    )]
    UnexpectedBuiltinTermArgument(Term<NamedDeBruijn>),
    #[error(
        "A builtin expected a term argument, but something else was received:\n\n{0}\n\nYou probably have an extra force wrapped around a builtin"
    )]
    BuiltinTermArgumentExpected(Term<NamedDeBruijn>),
    #[error(
        "Unable to unlift value because it is not a constant\n{:>13} {}",
        "Value",
        indent(redacted(format!("{:#?}", .0), 10)),
    )]
    NotAConstant(Value),
    #[error("The evaluation never reached a final state")]
    MachineNeverReachedDone,
    #[error("integerToByteString encountered negative size {0}")]
    IntegerToByteStringNegativeSize(BigInt),
    #[error("integerToByteString encountered negative input {0}")]
    IntegerToByteStringNegativeInput(BigInt),
    #[error("integerToByteString encountered size {0} which is bigger than the max size of {1}")]
    IntegerToByteStringSizeTooBig(BigInt, i64),
    #[error("integerToByteString encountered size {0} which is not enough space for {1} bytes")]
    IntegerToByteStringSizeTooSmall(BigInt, usize),
    #[error("Decoding utf8")]
    Utf8(#[from] FromUtf8Error),
    #[error("Out of Bounds\n\nindex: {}\nbytestring: {}\npossible: 0 - {}", .0, hex::encode(.1), .1.len() - 1)]
    ByteStringOutOfBounds(BigInt, Vec<u8>),
    #[error("Attempt to consByteString something than isn't a byte between [0-255]: {0}")]
    ByteStringConsNotAByte(BigInt),
    #[error("Divide By Zero\n\n{0} / {1}")]
    DivideByZero(BigInt, BigInt),
    #[error("Ed25519S PublicKey should be 32 bytes but it was {0}")]
    UnexpectedEd25519PublicKeyLength(usize),
    #[error("Ed25519S Signature should be 64 bytes but it was {0}")]
    UnexpectedEd25519SignatureLength(usize),
    #[error(
      "Failed to deserialise PlutusData using {0}:\n\n{}",
      redacted(format!("{:#?}", .1), 10),
    )]
    DeserialisationError(String, Value),
    #[error("Integer overflow")]
    OverflowError,
    #[error("blst error {0:?}")]
    Blst(blst::BLST_ERROR),
    #[error("blst::hashToGroup")]
    HashToCurveDstTooBig,
    #[cfg(not(target_family = "wasm"))]
    #[error(transparent)]
    Secp256k1(#[from] secp256k1::Error),
    #[cfg(target_family = "wasm")]
    #[error("{0}")]
    K256Error(String),
}

#[cfg(target_family = "wasm")]
impl From<k256::ecdsa::Error> for Error {
    fn from(error: k256::ecdsa::Error) -> Error {
        Error::K256Error(format!("K256 error: {}", error))
    }
}

/// Print only the first n lines of possibly long output, and redact the rest if longer.
fn redacted(s: String, max_rows: usize) -> String {
    let rows = s.lines();

    if rows.count() > max_rows {
        let last_line = s.lines().last().unwrap();
        let mut s = s.lines().take(max_rows).collect::<Vec<_>>().join("\n");
        s.push_str(&format!("\n    ...redacted...\n{last_line}"));
        s
    } else {
        s
    }
}

fn indent(s: String) -> String {
    s.lines().collect::<Vec<_>>().join(&format!("\n{:>14}", ""))
}
