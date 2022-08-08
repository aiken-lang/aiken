use std::string::FromUtf8Error;

use thiserror::Error;

use crate::ast::{NamedDeBruijn, Term, Type};

use super::{ExBudget, Value};

#[derive(Error, Debug)]
pub enum Error {
    #[error("Over budget mem: {} & cpu: {}", .0.mem, .0.cpu)]
    OutOfExError(ExBudget),
    #[error("Invalid Stepkind: {0}")]
    InvalidStepKind(u8),
    #[error("Cannot evaluate an open term:\n\n{0}")]
    OpenTermEvaluated(Term<NamedDeBruijn>),
    #[error("The provided Plutus code called 'error'.")]
    EvaluationFailure,
    #[error("Attempted to instantiate a non-polymorphic term:\n\n{0:#?}")]
    NonPolymorphicInstantiation(Value),
    #[error("Attempted to apply a non-function:\n\n{0:#?}")]
    NonFunctionalApplication(Value),
    #[error("Type mismatch expected '{0}' got '{1}'")]
    TypeMismatch(Type, Type),
    #[error("A builtin received a term argument when something else was expected:\n\n{0}\n\nYou probably forgot to wrap the builtin with a force.")]
    UnexpectedBuiltinTermArgument(Term<NamedDeBruijn>),
    #[error("A builtin expected a term argument, but something else was received:\n\n{0}\n\nYou probably have an extra force wrapped around a builtin")]
    BuiltinTermArgumentExpected(Term<NamedDeBruijn>),
    #[error("Unable to unlift value because it is not a constant:\n\n{0:#?}")]
    NotAConstant(Value),
    #[error("The evaluation never reached a final state")]
    MachineNeverReachedDone,
    #[error("Decoding utf8")]
    Utf8(#[from] FromUtf8Error),
}
