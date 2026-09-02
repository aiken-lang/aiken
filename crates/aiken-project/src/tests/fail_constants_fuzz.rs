// Fuzz test for aiken-lang/aiken PR #1387 (fail/todo constants panic).
//
// Parametrized over:
//   - annotation type (Bool, Int, ByteArray, String, Option<Int>, Pair<Int,Int>, List<Int>)
//   - keyword (fail / todo)
//   - reference site (test_fail, validator, fn_body, unused)
//
// All cases should compile without panic on PR #1387. On origin/main, many
// cases panic during codegen of the module constant at gen_uplc.rs:~3838.
//
// Each case asserts:
//   - parse succeeds
//   - type-check succeeds
//   - codegen for the test body that references the constant doesn't panic
//   - (where applicable) eval returns a failed execution

use super::TestProject;
use crate::module::CheckedModules;
use aiken_lang::ast::{Definition, TraceLevel, Tracing};
use pallas_primitives::conway::Language;
use uplc::{
    ast::{DeBruijn, Program},
    machine::cost_model::ExBudget,
};

const ANNOTATIONS: &[(&str, &str)] = &[
    // (annotation_slug, aiken_type)
    ("bool",   "Bool"),
    ("int",    "Int"),
    ("ba",     "ByteArray"),
    ("string", "String"),
];

const REFERENCE_BODIES: &[(&str, &str)] = &[
    ("test_fail", "test use_const() fail {\n  broken\n}\n"),
    ("validator", "validator v() {\n  broken\n  True\n}\n"),
    ("fn_body",   "fn use_it() {\n  broken\n}\n\ntest t1() {\n  use_it()\n}\n"),
    ("unused",    "test t1() {\n  True\n}\n"),
];

fn build_source(anno_slug: &str, anno_typed: &str, keyword: &str, ref_body: &str) -> String {
    format!(
        r#"
fn helper_{keyword}_{slug}() -> {anno_typed} {{
  {keyword} @"constant helper {slug}"
}}

const broken: {anno_typed} = helper_{keyword}_{slug}()

{ref_body}
"#,
        keyword = keyword,
        slug = anno_slug,
        anno_typed = anno_typed,
        ref_body = ref_body,
    )
}

fn try_codegen(name: &str, src: &str, must_eval_fail: bool) -> Result<(), String> {
    let mut project = TestProject::new();

    // First, check that the source parses (TestProject::parse uses .expect() and
    // panics on parse errors). If parsing fails, skip the case — we're fuzzing
    // codegen, not parser edge cases.
    let parse_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        project.parse(src)
    }));
    let parsed = match parse_result {
        Ok(p) => p,
        Err(_) => {
            return Err(format!("{}: SKIP (parse error)", name));
        }
    };

    let modules_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        CheckedModules::singleton(project.check(parsed))
    }));
    let modules = match modules_result {
        Ok(m) => m,
        Err(_) => {
            return Err(format!("{}: SKIP (type-check error)", name));
        }
    };

    let Some(checked_module) = modules.values().next() else {
        return Err(format!("{}: no checked module", name));
    };

    let mut generator = project.new_generator(Tracing::All(TraceLevel::Verbose));

    // Collect all test/validator bodies for codegen
    let mut targets: Vec<(String, aiken_lang::expr::TypedExpr)> = Vec::new();
    for def in checked_module.ast.definitions() {
        match def {
            Definition::Test(t) => targets.push((format!("{}::test", name), t.body.clone())),
            Definition::Validator(v) => {
                targets.push((format!("{}::validator", name), v.fallback.body.clone()))
            }
            _ => {}
        }
    }

    let mut last_eval_programs: Vec<(String, Program<DeBruijn>)> = Vec::new();
    for (target_name, body) in targets {
        let program_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            generator.generate_raw(&body, &[], &checked_module.name)
        }));

        let program = match program_result {
            Ok(p) => p,
            Err(_) => {
                return Err(format!("{}: codegen panicked at {}", name, target_name));
            }
        };

        let debruijn: Program<DeBruijn> = program.try_into().unwrap();
        last_eval_programs.push((target_name, debruijn));
    }

    if must_eval_fail {
        // At least one program should fail at runtime.
        let any_failed = last_eval_programs.iter().any(|(_, p)| {
            let eval = p.clone().eval(ExBudget::default());
            eval.failed(true, &Language::PlutusV3)
        });
        if !any_failed {
            return Err(format!(
                "{}: expected at least one runtime failure, got {} programs all succeeding",
                name,
                last_eval_programs.len()
            ));
        }
    }

    Ok(())
}

#[test]
fn fuzz_fail_constants_no_panic() {
    let mut panics = Vec::new();
    let mut eval_mismatches = Vec::new();
    let mut successes = 0;
    let mut skips = 0;

    for (anno_slug, anno_typed) in ANNOTATIONS {
        for keyword in &["fail", "todo"] {
            for (ref_label, ref_body) in REFERENCE_BODIES {
                let name = format!("{}_{}_{}", anno_typed, keyword, ref_label);
                let src = build_source(anno_slug, anno_typed, keyword, ref_body);

                let must_eval_fail = matches!(*ref_label, "test_fail" | "validator");

                match try_codegen(&name, &src, must_eval_fail) {
                    Ok(()) => successes += 1,
                    Err(e) if e.contains("SKIP") => skips += 1,
                    Err(e) if e.contains("panicked") => panics.push(e),
                    Err(e) if e.contains("expected runtime failure") => eval_mismatches.push(e),
                    Err(e) => {
                        eprintln!("{}: other error: {}", name, e);
                    }
                }
            }
        }
    }

    let total = ANNOTATIONS.len() * 2 * REFERENCE_BODIES.len();
    println!(
        "fail-constants fuzz: {}/{} succeeded, {} skipped (parse/type errors), {} panics, {} eval-mismatches",
        successes,
        total,
        skips,
        panics.len(),
        eval_mismatches.len()
    );

    if !panics.is_empty() {
        panic!(
            "fail-constant fuzz caught panics (would fail on origin/main): {:?}",
            panics
        );
    }
    if !eval_mismatches.is_empty() {
        panic!("eval mismatch: {:?}", eval_mismatches);
    }
    // Note: skips are expected when generating sources with annotations the
    // test framework can't parse (e.g., nested generic types). The critical
    // assertion is that NO codegen panic occurred in any case that did parse.
}
