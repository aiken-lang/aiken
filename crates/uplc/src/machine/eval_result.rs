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
    ///
    /// When `can_error` is `true`, the `expected_error` semantics apply
    /// regardless of language version (same as `failed(true)`).
    pub fn failed_with_lang(&self, can_error: bool, language: &Language) -> bool {
        if can_error {
            // expected-error semantics: script must return Bool(false) or error
            return self.failed(true);
        }

        match language {
            // PlutusV1/V2: success = execution didn't error.
            // Any non-error result is acceptable, matching Haskell's
            // `processLogsAndErrors` which only checks for `EvaluationFailure`.
            Language::PlutusV1 | Language::PlutusV2 => {
                self.result.is_err() || matches!(self.result, Ok(Term::Error))
            }

            // PlutusV3: strict check — must return Bool(true) or Unit.
            Language::PlutusV3 => self.failed(false),
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

#[cfg(test)]
mod tests {
    use super::*;
    fn make_eval_result(result: Result<Term<NamedDeBruijn>, Error>) -> EvalResult {
        EvalResult::new(
            result,
            ExBudget::default(),
            ExBudget::default(),
            vec![],
            None,
        )
    }

    fn int_term(n: i128) -> Term<NamedDeBruijn> {
        Term::Constant(
            Constant::Integer(n.into()).into(),
        )
    }

    fn bool_term(b: bool) -> Term<NamedDeBruijn> {
        Term::Constant(Constant::Bool(b).into())
    }

    fn unit_term() -> Term<NamedDeBruijn> {
        Term::Constant(Constant::Unit.into())
    }

    fn lambda_term() -> Term<NamedDeBruijn> {
        Term::Lambda {
            parameter_name: NamedDeBruijn {
                text: String::from("x"),
                index: 0.into(),
            }
            .into(),
            body: Term::Var(
                NamedDeBruijn {
                    text: String::from("x"),
                    index: 0.into(),
                }
                .into(),
            )
            .into(),
        }
    }

    // -------------------------------------------------------
    // PlutusV1/V2: any non-error result should be success
    // -------------------------------------------------------

    #[test]
    fn v1_integer_return_succeeds() {
        let eval = make_eval_result(Ok(int_term(42)));
        // Old behavior: failed(false) considers this a failure
        assert!(eval.failed(false), "old `failed` rejects integer return");
        // New behavior: V1 accepts any non-error result
        assert!(
            !eval.failed_with_lang(false, &Language::PlutusV1),
            "V1 should accept integer return value"
        );
    }

    #[test]
    fn v2_integer_return_succeeds() {
        let eval = make_eval_result(Ok(int_term(42)));
        assert!(
            !eval.failed_with_lang(false, &Language::PlutusV2),
            "V2 should accept integer return value"
        );
    }

    #[test]
    fn v1_lambda_return_succeeds() {
        let eval = make_eval_result(Ok(lambda_term()));
        assert!(eval.failed(false), "old `failed` rejects lambda return");
        assert!(
            !eval.failed_with_lang(false, &Language::PlutusV1),
            "V1 should accept lambda return value"
        );
    }

    #[test]
    fn v1_bool_true_succeeds() {
        let eval = make_eval_result(Ok(bool_term(true)));
        assert!(!eval.failed_with_lang(false, &Language::PlutusV1));
    }

    #[test]
    fn v1_unit_succeeds() {
        let eval = make_eval_result(Ok(unit_term()));
        assert!(!eval.failed_with_lang(false, &Language::PlutusV1));
    }

    #[test]
    fn v1_error_result_fails() {
        let eval = make_eval_result(Ok(Term::Error));
        assert!(eval.failed_with_lang(false, &Language::PlutusV1));
    }

    #[test]
    fn v1_machine_error_fails() {
        let eval = make_eval_result(Err(Error::OutOfExError(ExBudget::default())));
        assert!(eval.failed_with_lang(false, &Language::PlutusV1));
    }

    // -------------------------------------------------------
    // PlutusV3: strict — only Bool(true) or Unit
    // -------------------------------------------------------

    #[test]
    fn v3_integer_return_fails() {
        let eval = make_eval_result(Ok(int_term(42)));
        assert!(
            eval.failed_with_lang(false, &Language::PlutusV3),
            "V3 should reject integer return value"
        );
    }

    #[test]
    fn v3_bool_true_succeeds() {
        let eval = make_eval_result(Ok(bool_term(true)));
        assert!(!eval.failed_with_lang(false, &Language::PlutusV3));
    }

    #[test]
    fn v3_unit_succeeds() {
        let eval = make_eval_result(Ok(unit_term()));
        assert!(!eval.failed_with_lang(false, &Language::PlutusV3));
    }

    #[test]
    fn v3_bool_false_fails() {
        let eval = make_eval_result(Ok(bool_term(false)));
        assert!(eval.failed_with_lang(false, &Language::PlutusV3));
    }

    #[test]
    fn v3_lambda_return_fails() {
        let eval = make_eval_result(Ok(lambda_term()));
        assert!(eval.failed_with_lang(false, &Language::PlutusV3));
    }

    #[test]
    fn v3_error_result_fails() {
        let eval = make_eval_result(Ok(Term::Error));
        assert!(eval.failed_with_lang(false, &Language::PlutusV3));
    }
}
