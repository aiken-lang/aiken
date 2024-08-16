use super::{ExBudget, Value};
use crate::ast::{NamedDeBruijn, Term, Type};
use num_bigint::BigInt;
use std::string::FromUtf8Error;

#[derive(Debug, Clone, PartialEq, thiserror::Error, miette::Diagnostic)]
pub enum Error {
    #[error("execution went over budget\n{:>13} {}\n{:>13} {}", "Mem", .0.mem, "CPU", .0.cpu)]
    OutOfExError(ExBudget),
    #[error("invalid step kind: {0}")]
    InvalidStepKind(u8),
    #[error(
        "cannot evaluate an open term:\n{:>13} {}",
        "Term",
        indent(redacted(.0.to_pretty(), 10)),
    )]
    OpenTermEvaluated(Term<NamedDeBruijn>),
    #[error("the validator crashed / exited prematurely")]
    EvaluationFailure,
    #[error(
        "attempted to instantiate a non-polymorphic term\n{:>13} {}",
        "Term",
        indent(redacted(format!("{:#?}", .0), 10)),
    )]
    NonPolymorphicInstantiation(Value),
    #[error(
      "attempted to apply an argument to a non-function\n{:>13} {}\n{:>13} {}",
      "Thing",
      indent(redacted(format!("{:#?}", .0), 5)),
      "Argument",
      indent(redacted(format!("{:#?}", .1), 5)),
    )]
    NonFunctionalApplication(Value, Value),
    #[error(
        "attempted to case a non-const\n{:>13} {}",
        "Value",
        indent(redacted(format!("{:#?}", .0), 10)),
    )]
    NonConstrScrutinized(Value),
    #[error("Cases: {0:#?}\n\n are missing branch for constr:\n\n{1:#?}")]
    MissingCaseBranch(Vec<Term<NamedDeBruijn>>, Value),
    #[error("type mismatch\n{:>13} {0}\n{:>13} {1}", "Expected", "Got")]
    TypeMismatch(Type, Type),
    #[error("type mismatch\n{:>13} (list a)\n{:>13} {0}", "Expected", "Got")]
    ListTypeMismatch(Type),
    #[error("type mismatch\n{:>13}(pair a b)\n{:>13} {0}", "Expected", "Got")]
    PairTypeMismatch(Type),
    #[error(
        "unexpected empty list\n{:>13} {}",
        "List",
        indent(redacted(format!("{:#?}", .0), 10)),
    )]
    EmptyList(Value),
    #[error(
        "a builtin received a term argument when something else was expected\n{:>13} {}\n{:>13} You probably forgot to wrap the builtin with a force.",
        "Term",
        indent(redacted(format!("{:#?}", .0), 10)),
        "Hint"
    )]
    UnexpectedBuiltinTermArgument(Term<NamedDeBruijn>),
    #[error(
        "a builtin expected a term argument, but something else was received:\n{:>13} {}\n{:>13} You probably have an extra force wrapped around a builtin",
        "Term",
        indent(redacted(format!("{:#?}", .0), 10)),
        "Hint"
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
    #[error("integerToByteString encountered negative size\n{:>13} {0}", "Size")]
    IntegerToByteStringNegativeSize(BigInt),
    #[error("integerToByteString encountered negative input\n{:>13} {0}", "Input")]
    IntegerToByteStringNegativeInput(BigInt),
    #[error(
        "bytes size beyond limit when converting from integer\n{:>13} {0}\n{:>13} {1}",
        "Size",
        "Maximum"
    )]
    IntegerToByteStringSizeTooBig(BigInt, i64),
    #[error(
        "bytes size below limit when converting from integer\n{:>13} {0}\n{:>13} {1}",
        "Size",
        "Minimum"
    )]
    IntegerToByteStringSizeTooSmall(BigInt, usize),
    #[error("Decoding utf8")]
    Utf8(#[from] FromUtf8Error),
    #[error(
        "Out of Bounds\n{:>13} {}\nb{:>13} {}\n{:>13} 0 - {}",
        "Index",
        .0,
        "ByteArray",
        hex::encode(.1),
        "Allowed",
        .1.len() - 1
    )]
    ByteStringOutOfBounds(BigInt, Vec<u8>),
    #[error(
        "attempt to consByteString something than isn't a byte between [0-255]\n{:>13} {0}",
        "Found"
    )]
    ByteStringConsNotAByte(BigInt),
    #[error("divide By Zero: {0} / {1}")]
    DivideByZero(BigInt, BigInt),
    #[error("Ed25519S PublicKey should be 32 bytes but it was {0}")]
    UnexpectedEd25519PublicKeyLength(usize),
    #[error("Ed25519S Signature should be 64 bytes but it was {0}")]
    UnexpectedEd25519SignatureLength(usize),
    #[error(
      "failed to deserialise PlutusData using {0}\n{:>13} {}",
      "Value",
      indent(redacted(format!("{:#?}", .1), 10)),
    )]
    DeserialisationError(String, Value),
    #[error("integer overflow")]
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
