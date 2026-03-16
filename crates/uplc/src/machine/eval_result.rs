use super::{Error, Trace, cost_model::ExBudget};
use crate::ast::{Constant, NamedDeBruijn, Term};
use pallas_primitives::conway::Language;

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

    /// Determine whether script execution failed, taking the Plutus language
    /// version into account.
    ///
    /// This mirrors the Haskell reference implementation's `processLogsAndErrors`
    /// in `PlutusLedgerApi.Common.Eval`:
    ///
    /// - **PlutusV1/V2**: A script succeeds if it terminates without error.
    ///   Any non-error return value (not just `Bool(true)` or `Unit`) is
    ///   considered success.
    ///
    /// - **PlutusV3**: A script succeeds only if it returns `Bool(true)` or
    ///   `Unit` (the strict check, same as `failed(false)`).
    pub fn failed(&self, allow_bool_in_v3: bool, language: &Language) -> bool {
        let errored = self.result.is_err() || matches!(self.result, Ok(Term::Error));

        let expected_return = match language {
            // PlutusV1/V2: success = execution didn't error; any non-error result is acceptable
            Language::PlutusV1 | Language::PlutusV2 => true,
            // PlutusV3: strict check — must return Bool(true) or Unit.
            Language::PlutusV3 => matches!(
                self.result,
                  Ok(Term::Constant(ref con))
                  if matches!(con.as_ref(), Constant::Unit) || (allow_bool_in_v3 && matches!(con.as_ref(), Constant::Bool(true)))
            ),
        };

        errored || !expected_return
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

#[cfg(test)]
mod tests {
    use super::*;
    use Language::*;
    use test_case::test_case;

    #[test_case(Ok(Term::integer(42.into())), PlutusV1, false => false)]
    #[test_case(Ok(Term::integer(42.into())), PlutusV2, false => false)]
    #[test_case(Ok(Term::integer(42.into())), PlutusV3, false => true)]
    #[test_case(Ok(Term::bool(true)),         PlutusV1, false => false)]
    #[test_case(Ok(Term::bool(true)),         PlutusV2, false => false)]
    #[test_case(Ok(Term::bool(true)),         PlutusV3, false => true)]
    #[test_case(Ok(Term::bool(true)),         PlutusV3, true  => false)]
    #[test_case(Ok(Term::bool(false)),        PlutusV1, false => false)]
    #[test_case(Ok(Term::bool(false)),        PlutusV2, false => false)]
    #[test_case(Ok(Term::bool(false)),        PlutusV3, false => true)]
    #[test_case(Ok(Term::bool(false)),        PlutusV3, true  => true)]
    #[test_case(Ok(Term::unit()),             PlutusV1, false => false)]
    #[test_case(Ok(Term::unit()),             PlutusV2, false => false)]
    #[test_case(Ok(Term::unit()),             PlutusV3, false => false)]
    #[test_case(Ok(Term::Error),              PlutusV1, false => true)]
    #[test_case(Ok(Term::Error),              PlutusV2, false => true)]
    #[test_case(Ok(Term::Error),              PlutusV3, false => true)]
    #[test_case(Ok(Term::var("x").lambda("x").try_into().unwrap()), PlutusV1, false => false)]
    #[test_case(Ok(Term::var("x").lambda("x").try_into().unwrap()), PlutusV2, false => false)]
    #[test_case(Ok(Term::var("x").lambda("x").try_into().unwrap()), PlutusV3, false => true)]
    #[test_case(Err(Error::OutOfExError(ExBudget::default())), Language::PlutusV1, false => true)]
    #[test_case(Err(Error::OutOfExError(ExBudget::default())), Language::PlutusV2, false => true)]
    #[test_case(Err(Error::OutOfExError(ExBudget::default())), Language::PlutusV3, false => true;
    )]
    fn eval_result_failed(
        term: Result<Term<NamedDeBruijn>, Error>,
        lang: Language,
        allow_bool_in_v3: bool,
    ) -> bool {
        EvalResult::new(term, ExBudget::default(), ExBudget::default(), vec![], None)
            .failed(allow_bool_in_v3, &lang)
    }
}
