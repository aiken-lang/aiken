use crate::ast::{Constant, NamedDeBruijn, Term};

use super::{cost_model::ExBudget, Error};

pub struct EvalResult {
    result: Result<Term<NamedDeBruijn>, Error>,
    remaining_budget: ExBudget,
    initial_budget: ExBudget,
    logs: Vec<String>,
}

impl EvalResult {
    pub fn new(
        result: Result<Term<NamedDeBruijn>, Error>,
        remaining_budget: ExBudget,
        initial_budget: ExBudget,
        logs: Vec<String>,
    ) -> EvalResult {
        EvalResult {
            result,
            remaining_budget,
            initial_budget,
            logs,
        }
    }

    pub fn cost(&self) -> ExBudget {
        self.initial_budget - self.remaining_budget
    }

    pub fn logs(&mut self) -> Vec<String> {
        std::mem::take(&mut self.logs)
    }

    pub fn failed(&self, can_error: bool) -> bool {
        if can_error {
            !matches!(self.result, Err(_))
        } else {
            matches!(self.result, Err(_))
                || matches!(self.result, Ok(Term::Error))
                || matches!(self.result, Ok(Term::Constant(ref con)) if matches!(con.as_ref(), Constant::Bool(false)))
        }
    }

    pub fn result(self) -> Result<Term<NamedDeBruijn>, Error> {
        self.result
    }
}
