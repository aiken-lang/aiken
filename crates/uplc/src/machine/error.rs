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
}
