use super::{Error, Trace, cost_model::ExBudget};
use crate::ast::{Constant, NamedDeBruijn, Term};

#[derive(Debug)]
pub struct EvalResult {
    pub result: Result<Term<NamedDeBruijn>, Error>,
    pub remaining_budget: ExBudget,
    pub initial_budget: ExBudget,
    pub traces: Vec<Trace>,
    pub debug_cost: Option<Vec<i64>>,
}

impl EvalResult {
    pub fn new(
        result: Result<Term<NamedDeBruijn>, Error>,
        remaining_budget: ExBudget,
        initial_budget: ExBudget,
        traces: Vec<Trace>,
        debug_cost: Option<Vec<i64>>,
    ) -> EvalResult {
        EvalResult {
            result,
            remaining_budget,
            initial_budget,
            traces,
            debug_cost,
        }
    }

    pub fn cost(&self) -> ExBudget {
        self.initial_budget - self.remaining_budget
    }

    pub fn traces(&self) -> Vec<Trace> {
        self.traces.clone()
    }

    pub fn logs(&self) -> Vec<String> {
        self.traces
            .iter()
            .cloned()
            .filter_map(Trace::unwrap_log)
            .collect()
    }

    pub fn labels(&self) -> Vec<String> {
        self.traces
            .iter()
            .cloned()
            .filter_map(Trace::unwrap_label)
            .collect()
    }

    pub fn failed(&self, can_error: bool) -> bool {
        if can_error {
            self.result.is_ok()
                && !matches!(self.result, Ok(Term::Constant(ref con)) if matches!(con.as_ref(), Constant::Bool(false)))
        } else {
            self.result.is_err()
                || matches!(self.result, Ok(Term::Error))
                || !matches!(
                  self.result,
                  Ok(Term::Constant(ref con))
                  if matches!(con.as_ref(), Constant::Bool(true)) || matches!(con.as_ref(), Constant::Unit)
                )
        }
    }

    pub fn debug_cost(&self) -> Option<Vec<i64>> {
        self.debug_cost.clone()
    }

    #[allow(clippy::result_unit_err)]
    pub fn unwrap_constant(self) -> Result<Constant, ()> {
        match self.result {
            Ok(Term::Constant(cst)) => Ok(cst.as_ref().to_owned()),
            _ => Err(()),
        }
    }

    pub fn result(&self) -> Result<Term<NamedDeBruijn>, Error> {
        self.result.clone()
    }
}
