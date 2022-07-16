use std::fmt::Display;

use thiserror::Error;

use crate::ast::{NamedDeBruijn, Term};

use super::{ExBudget, Value};

#[derive(Error, Debug)]
pub enum Error {
    #[error("Over budget mem: {} & cpu: {}", .0.mem, .0.cpu)]
    OutOfExError(ExBudget),
    #[error("Invalid Stepkind: {0}")]
    InvalidStepKind(u8),
    #[error("Cannot evaluate an open term: {0:#?}")]
    OpenTermEvaluated(Term<NamedDeBruijn>),
    #[error("The provided Plutus code called 'error'.")]
    EvaluationFailure,
    #[error("Attempted to instantiate a non-polymorphic term: {0:#?}")]
    NonPolymorphicInstantiation(Value),
    #[error("Attempted to apply a non-function: {0:#?}")]
    NonFunctionalApplication(Value),
    #[error("Type mismatch expected '{0}' got '{1}'")]
    TypeMismatch(Type, Type),
    #[error("A builtin received a term argument when something else was expected:\n\n{}\n\nYou probably forgot to wrap the builtin with a force.", .0.to_pretty())]
    UnexpectedBuiltinTermArgument(Term<NamedDeBruijn>),
    #[error("A builtin expected a term argument, but something else was received:\n\n{}\n\nYou probably have an extra force wrapped around a builtin", .0.to_pretty())]
    BuiltinTermArgumentExpected(Term<NamedDeBruijn>),
}

#[derive(Debug, Clone)]
pub enum Type {
    Bool,
    Integer,
    String,
    ByteString,
    Unit,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Bool => write!(f, "bool"),
            Type::Integer => write!(f, "integer"),
            Type::String => write!(f, "string"),
            Type::ByteString => write!(f, "bytestring"),
            Type::Unit => write!(f, "unit"),
        }
    }
}
