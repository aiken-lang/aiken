use super::{group_by_module, Event, EventListener};
use aiken_lang::{
    ast::OnTestFailure,
    expr::UntypedExpr,
    format::Formatter,
    test_framework::{AssertionStyleOptions, PropertyTestResult, TestResult, UnitTestResult},
};
use serde_json::json;

#[derive(Debug, Default, Clone, Copy)]
pub struct Json;

impl EventListener for Json {
    fn handle_event(&self, event: Event) {
        match event {
            Event::FinishedTests { seed, tests, .. } => {
                let total = tests.len();
                let passed = tests.iter().filter(|t| t.is_success()).count();
                let failed = total - passed;

                let json_output = serde_json::json!({
                    "seed": seed,
                    "summary": json!({
                        "total": total,
                        "passed": passed,
                        "failed": failed,
                        "kind": json!({
                            "unit": count_unit_tests(tests.iter()),
                            "property": count_property_tests(tests.iter()),
                        })
                    }),
                    "modules": group_by_module(&tests).iter().map(|(module, results)| {
                        serde_json::json!({
                            "name": module,
                            "summary": fmt_test_summary_json(results),
                            "tests": results.iter().map(|r| fmt_test_json(r)).collect::<Vec<_>>(),
                        })
                    }).collect::<Vec<_>>(),
                });
                println!("{}", serde_json::to_string_pretty(&json_output).unwrap());
            }
            Event::FinishedBenchmarks { tests, seed } => {
                let benchmark_results: Vec<_> = tests
                    .into_iter()
                    .filter_map(|test| {
                        if let TestResult::Benchmark(result) = test {
                            Some(serde_json::json!({
                                "name": result.test.name,
                                "module": result.test.module,
                                "memory": result.cost.mem,
                                "cpu": result.cost.cpu
                            }))
                        } else {
                            None
                        }
                    })
                    .collect();

                let json = serde_json::json!({
                    "benchmarks": benchmark_results,
                    "seed": seed,
                });

                println!("{}", serde_json::to_string_pretty(&json).unwrap());
            }
            _ => super::Terminal.handle_event(event),
        }
    }
}

fn fmt_test_json(result: &TestResult<UntypedExpr, UntypedExpr>) -> serde_json::Value {
    let on_test_failure = match result {
        TestResult::UnitTestResult(UnitTestResult { ref test, .. }) => &test.on_test_failure,
        TestResult::PropertyTestResult(PropertyTestResult { ref test, .. }) => {
            &test.on_test_failure
        }
        TestResult::Benchmark(_) => unreachable!("benchmark returned in JSON output"),
    };

    let mut test = json!({
        "title": result.title(),
        "status": if result.is_success() { "pass" } else { "fail" },
        "on_failure": match on_test_failure {
            OnTestFailure::FailImmediately => "fail_immediately" ,
            OnTestFailure::SucceedEventually => "succeed_eventually" ,
            OnTestFailure::SucceedImmediately => "succeed_immediately",
        }
    });

    match result {
        TestResult::UnitTestResult(UnitTestResult {
            spent_budget,
            assertion,
            ..
        }) => {
            test["execution_units"] = json!({
                "mem": spent_budget.mem,
                "cpu": spent_budget.cpu,
            });
            if !result.is_success() {
                if let Some(assertion) = assertion {
                    test["assertion"] =
                        json!(assertion.to_string(false, &AssertionStyleOptions::new(None)));
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
            if !labels.is_empty() {
                test["labels"] = json!(labels);
            }
            test["counterexample"] = match counterexample {
                Ok(Some(expr)) => json!(Formatter::new().expr(expr, false).to_pretty_string(60)),
                Ok(None) => json!(null),
                Err(err) => json!({"error": err.to_string()}),
            };
        }
        TestResult::Benchmark(_) => unreachable!("benchmark returned in JSON output"),
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
        "kind": json!({
            "unit": count_unit_tests(tests.iter().copied()),
            "property": count_property_tests(tests.iter().copied())
        })
    })
}

fn count_unit_tests<'a, I>(tests: I) -> usize
where
    I: Iterator<Item = &'a TestResult<UntypedExpr, UntypedExpr>>,
{
    tests
        .filter(|t| matches!(t, TestResult::UnitTestResult { .. }))
        .count()
}

fn count_property_tests<'a, I>(tests: I) -> usize
where
    I: Iterator<Item = &'a TestResult<UntypedExpr, UntypedExpr>>,
{
    tests
        .filter(|t| matches!(t, TestResult::PropertyTestResult { .. }))
        .count()
}

pub fn json_schema() -> serde_json::Value {
    let definitions = json!({
      "Summary": {
        "type": "object",
        "required": ["total", "passed", "failed", "kind"],
        "properties": {
          "total": { "type": "integer" },
          "passed": { "type": "integer" },
          "failed": { "type": "integer" },
          "kind": {
            "type": "object",
            "required": ["unit", "property"],
            "properties": {
              "unit": { "type": "integer" },
              "property": { "type": "integer" }
            }
          }
        }
      },
      "Status": {
       "type": "string",
       "enum": [ "pass", "fail" ]
      },
      "OnFailure": {
       "type": "string",
       "enum": [
         "fail_immediately",
         "succeed_immediately",
         "succeed_eventually"
       ]
      }
    });

    let unit_test = json!({
      "type": "object",
      "required": [
        "kind",
        "title",
        "status",
        "on_failure",
        "execution_units"
      ],
      "properties": {
        "kind": {
          "type": "string",
          "enum": [ "unit" ]
        },
        "title": { "type": "string" },
        "status": { "$ref": "#/properties/definitions/Status" },
        "on_failure": { "$ref": "#/properties/definitions/OnFailure" },
        "execution_units": {
            "type": "object",
            "properties": {
              "mem": { "type": "integer" },
              "cpu": { "type": "integer" }
            }
        },
        "assertion": { "type": "string" },
      }
    });

    let property_test = json!({
      "type": "object",
      "required": [
        "kind",
        "title",
        "status",
        "on_failure",
        "iterations",
        "counterexample"
      ],
      "properties": {
        "kind": {
          "type": "string",
          "enum": [ "property" ]
        },
        "title": { "type": "string" },
        "status": { "$ref": "#/properties/definitions/Status" },
        "on_failure": { "$ref": "#/properties/definitions/OnFailure" },
        "iterations": { "type": "integer" },
        "labels": {
          "type": "object",
          "additionalProperties": { "type": "integer" }
        },
        "counterexample": {
          "oneOf": [
            { "type": "string" },
            { "type": "null" },
            {
              "type": "object",
              "properties": {
                "error": { "type": "string" }
              }
            }
          ]
        }
      }
    });

    json!({
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "$vocabulary": {
          "https://json-schema.org/draft/2020-12/vocab/core": true,
          "https://json-schema.org/draft/2020-12/vocab/applicator": true,
          "https://json-schema.org/draft/2020-12/vocab/validation": true
      },
      "title": "Aiken CLI JSON Schema",
      "type": "object",
      "properties": {
        "command[check]": {
          "seed": { "type": "integer" },
          "summary": { "$ref": "#/properties/definitions/Summary" },
          "modules": {
            "type": "array",
            "items": {
              "type": "object",
              "properties": {
                "name": { "type": "string" },
                "summary": { "$ref": "#/properties/definitions/Summary" },
                "test": {
                  "type": "array",
                  "items": {
                    "oneOf": [ unit_test, property_test ]
                  }
                }
              }
            }
          }
        },
        "definitions": definitions
      }
    })
}
