//! Coverage tracking for UPLC execution.
//!
//! This module provides functionality to run UPLC programs while tracking
//! which source locations are executed, for code coverage analysis.

use aiken_lang::ast::SourceLocation;
use pallas_primitives::conway::Language;
use std::collections::HashSet;
use uplc::{
    ast::{Constant, NamedDeBruijn, Program, Term},
    machine::{
        Machine, MachineState,
        cost_model::{CostModel, ExBudget},
    },
};

/// Coverage data collected during program execution.
#[derive(Debug, Clone, Default)]
pub struct CoverageData {
    /// Set of source locations that were executed.
    pub executed_locations: HashSet<SourceLocation>,
}

impl CoverageData {
    pub fn new() -> Self {
        Self {
            executed_locations: HashSet::new(),
        }
    }

    /// Merge coverage data from another run.
    pub fn merge(&mut self, other: &CoverageData) {
        self.executed_locations
            .extend(other.executed_locations.iter().cloned());
    }
}

/// Collect all source locations from a term.
/// This gives us the set of all "coverable" locations in a compiled program.
pub fn collect_all_locations<N: Clone>(term: &Term<N, SourceLocation>) -> HashSet<SourceLocation> {
    let mut locations = HashSet::new();
    collect_locations_recursive(term, &mut locations);
    locations
}

fn collect_locations_recursive<N: Clone>(
    term: &Term<N, SourceLocation>,
    locations: &mut HashSet<SourceLocation>,
) {
    // Add this term's location if non-empty
    let loc = term.context();
    if !loc.is_empty() {
        locations.insert(loc);
    }

    // Recursively collect from subterms
    match term {
        Term::Var { .. } | Term::Constant { .. } | Term::Error { .. } | Term::Builtin { .. } => {}
        Term::Lambda { body, .. }
        | Term::Delay { term: body, .. }
        | Term::Force { term: body, .. } => {
            collect_locations_recursive(body, locations);
        }
        Term::Apply {
            function, argument, ..
        } => {
            collect_locations_recursive(function, locations);
            collect_locations_recursive(argument, locations);
        }
        Term::Case {
            constr, branches, ..
        } => {
            collect_locations_recursive(constr, locations);
            for branch in branches {
                collect_locations_recursive(branch, locations);
            }
        }
        Term::Constr { fields, .. } => {
            for field in fields {
                collect_locations_recursive(field, locations);
            }
        }
    }
}

/// Collect all source locations from a program.
pub fn collect_all_program_locations(
    program: &Program<NamedDeBruijn, SourceLocation>,
) -> HashSet<SourceLocation> {
    collect_all_locations(&program.term)
}

/// Result of running a program with coverage tracking.
#[derive(Debug)]
pub struct CoverageResult {
    /// Whether the program returned Bool(true) or Unit.
    pub success: bool,
    /// Whether the program returned Term::Error.
    pub errored: bool,
    /// Whether the program returned Bool(false) (assertion failure).
    pub returned_false: bool,
    /// Whether the machine encountered a runtime error (budget exhaustion, etc).
    pub machine_error: bool,
    /// Coverage data collected during execution.
    pub coverage: CoverageData,
    /// Remaining budget after execution.
    pub remaining_budget: ExBudget,
    /// Logs/traces from execution.
    pub logs: Vec<String>,
}

/// Run a UPLC program with coverage tracking.
///
/// Uses the stepping interface to execute the program one step at a time,
/// collecting the source location of each term as it's computed.
///
/// Always returns Ok with a CoverageResult, even if the machine encounters an error.
/// This ensures coverage data is preserved for tests that are expected to fail.
pub fn run_with_coverage(
    term: Term<NamedDeBruijn, SourceLocation>,
    version: &Language,
    initial_budget: ExBudget,
) -> CoverageResult {
    let mut machine = Machine::new(version.clone(), CostModel::default(), initial_budget, 200);

    let mut coverage = CoverageData::new();

    // Get initial state, handling potential error
    let mut state = match machine.get_initial_machine_state(term) {
        Ok(s) => s,
        Err(_) => {
            return CoverageResult {
                success: false,
                errored: false,
                returned_false: false,
                machine_error: true,
                coverage,
                remaining_budget: machine.ex_budget,
                logs: vec![],
            };
        }
    };

    loop {
        // Collect coverage from Compute states
        if let MachineState::Compute(_, _, ref term) = state {
            let loc = term.context();
            if !loc.is_empty() {
                coverage.executed_locations.insert(loc);
            }
        }

        // Step the machine, handling errors while preserving coverage
        state = match machine.step(state) {
            Ok(s) => s,
            Err(_) => {
                // Machine error (e.g., from expect failing, budget exhaustion, etc.)
                // Still return the coverage we collected up to this point
                let logs = machine
                    .traces
                    .iter()
                    .filter_map(|t| t.clone().unwrap_log())
                    .collect();

                return CoverageResult {
                    success: false,
                    errored: true, // Treat machine errors as program errors for coverage purposes
                    returned_false: false,
                    machine_error: true,
                    coverage,
                    remaining_budget: machine.ex_budget,
                    logs,
                };
            }
        };

        // Check for completion
        if let MachineState::Done(result) = state {
            // Determine execution outcome using the same logic as EvalResult::failed()
            // - success: returned Bool(true) or Unit
            // - errored: returned Term::Error
            // - returned_false: returned Bool(false) (assertion failure)
            let is_success_result = matches!(
                &result,
                Term::Constant { value, .. }
                if matches!(value.as_ref(), Constant::Bool(true) | Constant::Unit)
            );
            let is_error = matches!(&result, Term::Error { .. });
            let is_false = matches!(
                &result,
                Term::Constant { value, .. }
                if matches!(value.as_ref(), Constant::Bool(false))
            );

            let logs = machine
                .traces
                .iter()
                .filter_map(|t| t.clone().unwrap_log())
                .collect();

            return CoverageResult {
                success: is_success_result,
                errored: is_error,
                returned_false: is_false,
                machine_error: false,
                coverage,
                remaining_budget: machine.ex_budget,
                logs,
            };
        }
    }
}

/// Run a UPLC program with coverage, converting from Program<Name, SourceLocation>.
pub fn run_program_with_coverage(
    program: Program<NamedDeBruijn, SourceLocation>,
    version: &Language,
    initial_budget: ExBudget,
) -> CoverageResult {
    run_with_coverage(program.term, version, initial_budget)
}

#[cfg(test)]
mod tests {
    use super::*;
    use aiken_lang::ast::Span;
    use std::rc::Rc;
    use uplc::ast::{Constant, DeBruijn};

    fn make_loc(module: &str, start: usize, end: usize) -> SourceLocation {
        SourceLocation::new(module, Span { start, end })
    }

    #[test]
    fn test_coverage_simple_constant() {
        // A simple program that just returns a constant
        let term: Term<NamedDeBruijn, SourceLocation> = Term::Constant {
            value: Rc::new(Constant::Bool(true)),
            context: make_loc("test", 0, 4),
        };

        let result = run_with_coverage(term, &Language::PlutusV3, ExBudget::max());

        assert!(result.success);
        assert!(!result.machine_error);
        assert!(
            result
                .coverage
                .executed_locations
                .contains(&make_loc("test", 0, 4))
        );
    }

    #[test]
    fn test_coverage_lambda_application() {
        // (\x -> x) True
        let identity_body: Term<NamedDeBruijn, SourceLocation> = Term::Var {
            name: Rc::new(NamedDeBruijn {
                text: "x".to_string(),
                index: DeBruijn::from(1),
            }),
            context: make_loc("test", 10, 11),
        };

        let identity: Term<NamedDeBruijn, SourceLocation> = Term::Lambda {
            parameter_name: Rc::new(NamedDeBruijn {
                text: "x".to_string(),
                index: DeBruijn::from(0),
            }),
            body: Rc::new(identity_body),
            context: make_loc("test", 5, 15),
        };

        let arg: Term<NamedDeBruijn, SourceLocation> = Term::Constant {
            value: Rc::new(Constant::Bool(true)),
            context: make_loc("test", 20, 24),
        };

        let app: Term<NamedDeBruijn, SourceLocation> = Term::Apply {
            function: Rc::new(identity),
            argument: Rc::new(arg),
            context: make_loc("test", 0, 25),
        };

        let result = run_with_coverage(app, &Language::PlutusV3, ExBudget::max());

        assert!(result.success);
        assert!(!result.machine_error);
        // Should have executed: Apply, Lambda, Constant (arg), Var (body)
        assert!(
            result
                .coverage
                .executed_locations
                .contains(&make_loc("test", 0, 25))
        ); // Apply
        assert!(
            result
                .coverage
                .executed_locations
                .contains(&make_loc("test", 5, 15))
        ); // Lambda
        assert!(
            result
                .coverage
                .executed_locations
                .contains(&make_loc("test", 20, 24))
        ); // Constant
        assert!(
            result
                .coverage
                .executed_locations
                .contains(&make_loc("test", 10, 11))
        ); // Var
    }

    #[test]
    fn test_coverage_merge() {
        let mut cov1 = CoverageData::new();
        cov1.executed_locations.insert(make_loc("a", 0, 10));
        cov1.executed_locations.insert(make_loc("a", 20, 30));

        let mut cov2 = CoverageData::new();
        cov2.executed_locations.insert(make_loc("a", 20, 30));
        cov2.executed_locations.insert(make_loc("b", 0, 5));

        cov1.merge(&cov2);

        assert_eq!(cov1.executed_locations.len(), 3);
        assert!(cov1.executed_locations.contains(&make_loc("a", 0, 10)));
        assert!(cov1.executed_locations.contains(&make_loc("a", 20, 30)));
        assert!(cov1.executed_locations.contains(&make_loc("b", 0, 5)));
    }
}
