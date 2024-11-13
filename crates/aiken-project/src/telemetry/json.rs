use super::{find_max_execution_units, group_by_module, Event, EventListener};
use aiken_lang::{
    ast::OnTestFailure,
    expr::UntypedExpr,
    format::Formatter,
    test_framework::{PropertyTestResult, TestResult, UnitTestResult},
};
use owo_colors::Stream::Stderr;
use serde_json::json;

#[derive(Debug, Default, Clone, Copy)]
pub struct Json;

impl EventListener for Json {
    fn handle_event(&self, event: Event) {
        match event {
            Event::FinishedTests { seed, tests, .. } => {
                let json_output = serde_json::json!({
                    "seed": seed,
                    "modules": group_by_module(&tests).iter().map(|(module, results)| {
                        serde_json::json!({
                            "name": module,
                            "tests": results.iter().map(|r| fmt_test_json(r)).collect::<Vec<_>>(),
                            "summary": fmt_test_summary_json(results)
                        })
                    }).collect::<Vec<_>>(),
                    "summary": fmt_overall_summary_json(&tests)
                });
                println!("{}", serde_json::to_string_pretty(&json_output).unwrap());
            }
            _ => super::Terminal.handle_event(event),
        }
    }
}

fn fmt_test_json(result: &TestResult<UntypedExpr, UntypedExpr>) -> serde_json::Value {
    let mut test = json!({
        "name": result.title(),
        "status": if result.is_success() { "PASS" } else { "FAIL" },
    });

    match result {
        TestResult::UnitTestResult(UnitTestResult {
            spent_budget,
            assertion,
            test: unit_test,
            ..
        }) => {
            test["execution_units"] = json!({
                "memory": spent_budget.mem,
                "cpu": spent_budget.cpu,
            });
            if !result.is_success() {
                if let Some(assertion) = assertion {
                    test["assertion"] = json!({
                        "message": assertion.to_string(Stderr, false),
                        "expected_to_fail": matches!(unit_test.on_test_failure, OnTestFailure::SucceedEventually | OnTestFailure::SucceedImmediately),
                    });
                }
            }
        }
        TestResult::PropertyTestResult(PropertyTestResult {
            iterations,
            labels,
            counterexample,
            ..
        }) => {
            test["iterations"] = json!(iterations);
            test["labels"] = json!(labels);
            test["counterexample"] = match counterexample {
                Ok(Some(expr)) => json!(Formatter::new().expr(expr, false).to_pretty_string(60)),
                Ok(None) => json!(null),
                Err(err) => json!({"error": err.to_string()}),
            };
        }
    }

    if !result.traces().is_empty() {
        test["traces"] = json!(result.traces());
    }

    test
}

fn fmt_test_summary_json(tests: &[&TestResult<UntypedExpr, UntypedExpr>]) -> serde_json::Value {
    let total = tests.len();
    let passed = tests.iter().filter(|t| t.is_success()).count();
    let failed = total - passed;

    json!({
        "total": total,
        "passed": passed,
        "failed": failed,
    })
}

fn fmt_overall_summary_json(tests: &[TestResult<UntypedExpr, UntypedExpr>]) -> serde_json::Value {
    let total = tests.len();
    let passed = tests.iter().filter(|t| t.is_success()).count();
    let failed = total - passed;

    let modules = group_by_module(tests);
    let module_count = modules.len();

    let (max_mem, max_cpu, max_iter) = find_max_execution_units(tests);

    // Separate counts for unit tests and property-based tests
    let unit_tests = tests
        .iter()
        .filter(|t| matches!(t, TestResult::UnitTestResult { .. }))
        .count();
    let property_tests = tests
        .iter()
        .filter(|t| matches!(t, TestResult::PropertyTestResult { .. }))
        .count();

    json!({
        "total_tests": total,
        "passed_tests": passed,
        "failed_tests": failed,
        "unit_tests": unit_tests,
        "property_tests": property_tests,
        "module_count": module_count,
        "max_execution_units": {
            "memory": max_mem,
            "cpu": max_cpu,
        },
        "max_iterations": max_iter,
        "modules": modules.into_iter().map(|(module, results)| {
            json!({
                "name": module,
                "tests": results.iter().map(|r| fmt_test_json(r)).collect::<Vec<_>>(),
                "summary": fmt_test_summary_json(&results)
            })
        }).collect::<Vec<_>>(),
    })
}
