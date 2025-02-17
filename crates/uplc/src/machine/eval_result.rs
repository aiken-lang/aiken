use super::{cost_model::ExBudget, Error, Trace};
use crate::ast::{Constant, NamedDeBruijn, Term};

#[derive(Debug)]
pub struct EvalResult {
    result: Result<Term<NamedDeBruijn>, Error>,
    remaining_budget: ExBudget,
    initial_budget: ExBudget,
    traces: Vec<Trace>,
    debug_cost: Option<Vec<i64>>,
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

    pub fn traces(&mut self) -> Vec<Trace> {
        std::mem::take(&mut self.traces)
    }

    pub fn logs(&mut self) -> Vec<String> {
        std::mem::take(&mut self.traces)
            .into_iter()
            .filter_map(Trace::unwrap_log)
            .collect()
    }

    pub fn labels(&mut self) -> Vec<String> {
        std::mem::take(&mut self.traces)
            .into_iter()
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
