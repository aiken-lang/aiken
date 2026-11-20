//! Compile-time timing fuzz for issue #1377
//!
//! Background: `aiken check` compile time grows roughly 2x per additional binary
//! operator in a single arithmetic expression. At ~24 chained operators, the
//! process effectively hangs with high `sys` time, suggesting a term-duplication
//! blow-up in some compiler pass.
//!
//! This harness measures *each* compile stage independently — `parse`,
//! `check` (type inference), and `codegen` (UPLC lowering) — across a
//! parametric sweep of `# chained let +/`. It prints a CSV curve and asserts
//! a per-N hard timeout to catch hangs.
//!
//! Methodology is intentionally timing-based (not panic/output-equivalence).
//! See also: docs/fuzzing-methodology.md (forthcoming).

use super::TestProject;
use crate::module::CheckedModules;
use aiken_lang::ast::{Definition, TraceLevel, Tracing};
use std::time::Instant;

/// Hard per-N timeout (ms). If `parse+check+codegen` exceeds this for any
/// input size, we record `verdict=EXPLODED` and stop the sweep. The point of
/// the cliff — the smallest N at which we hit the timeout — is the leading
/// edge of the asymptotic bug.
const HARD_TIMEOUT_MS: u128 = 30_000;

/// Parametric sweep sizes. Picked to bracket the reported ~24 cliff with
/// extra headroom on either side. Exponents are uneven so the curve shows
/// the cliff clearly even at low N.
const SIZES: &[usize] = &[
    1, 2, 3, 4, 5, 6, 7, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32,
];

/// Generate an Aiken module with `n` chained `let` bindings + one nested
/// arithmetic expression `x0 + x1 + ... + x(n-1) + y`.
///
/// Mirrors the reproducer in issue #1377 (Thomas Vellekoop, 2026-09-03):
///
/// ```aiken
/// pub fn s(y: Int) -> Int {
///   let x0 = 3
///   let x1 = 4
///   ...
///   let x{n-1} = n+3
///   x0 + x1 + ... + x{n-1} + y
/// }
/// ```
fn make_chained_let_source(n: usize) -> String {
    let mut src = String::with_capacity(64 * n + 128);
    src.push_str("pub fn s(y: Int) -> Int {\n");
    for i in 0..n {
        // 3..n+3 gives a small positive value per binding; ensures each `x_i`
        // is a distinct literal so the type-checker can't constant-fold the
        // sum at parse time.
        src.push_str(&format!("  let x{i} = {}\n", i + 3));
    }
    // Build the sum: x0 + x1 + ... + x(n-1) + y
    // Each `x_i` is a distinct binding → forces the elaborator to thread
    // `n` distinct uses through inference.
    src.push_str("  ");
    for i in 0..n {
        src.push_str(&format!("x{i} + "));
    }
    src.push_str("y\n}\n");
    src
}

/// Time each compile stage independently. Returns `(parse, check, codegen)`
/// in milliseconds, plus the source size in bytes for reference.
fn time_compile_stages(
    n: usize,
) -> Result<(u128, u128, u128, usize), String> {
    let src = make_chained_let_source(n);
    let src_bytes = src.len();

    let mut project = TestProject::new();

    // Stage 1: parse (lex + parse to AST)
    let t0 = Instant::now();
    let parsed = project.parse(&src);
    let t_parse = t0.elapsed().as_millis();

    // Stage 2: type-check (infer + register definitions)
    let t1 = Instant::now();
    let checked = project.check(parsed);
    let t_check = t1.elapsed().as_millis();

    // Stage 3: codegen (lower AST → UPLC + optimize).
    // The TestProject's `pub fn s(y: Int) -> Int` is a regular function, not
    // a validator or test, so we lower it directly via `generate_raw` on the
    // function body — same path a validator would take, minus validator
    // boilerplate. The function args must be passed explicitly here, since
    // unlike a Test/Validator (whose body is already wrapped with the args),
    // a raw `pub fn`'s body still references the parameter names via the
    // interned string table.
    let t2 = Instant::now();
    let modules = CheckedModules::singleton(checked);
    let mut generator = project.new_generator(Tracing::All(TraceLevel::Silent));
    let mut codegen_done = false;
    for module in modules.values() {
        for def in module.ast.definitions() {
            if let Definition::Fn(func) = def {
                if func.name == "s" {
                    // generate_raw signature:
                    //   fn generate_raw(
                    //     &mut self,
                    //     body: &TypedExpr,
                    //     args: &[TypedArg],
                    //     module_name: &str,
                    //   ) -> Program<Name>
                    // For a regular pub fn, we MUST pass the function's own
                    // arg list — the interner hasn't otherwise seen `y`.
                    let _program =
                        generator.generate_raw(&func.body, &func.arguments, &module.name);
                    codegen_done = true;
                    break;
                }
            }
        }
        if codegen_done {
            break;
        }
    }
    if !codegen_done {
        return Err(format!("no function named `s` found in module for n={n}"));
    }
    let t_codegen = t2.elapsed().as_millis();

    Ok((t_parse, t_check, t_codegen, src_bytes))
}

/// Main timing curve. Prints a CSV and asserts no N exceeds the hard timeout.
///
/// To regenerate a curve:  `cargo test -p aiken-project compile_time_curve_1377 -- --nocapture`
/// To pin baseline:        `cargo bench compile_time_curve_1377`  (TODO: wire benches)
#[test]
fn compile_time_curve_1377() {
    println!();
    println!("# issue #1377 — compile-time timing fuzz");
    println!("# format: n,parse_ms,check_ms,codegen_ms,total_ms,src_bytes,verdict");
    println!("# hard timeout: {HARD_TIMEOUT_MS} ms per N");
    println!("# SIZES = {SIZES:?}");
    println!();

    let mut cliff_n: Option<usize> = None;
    let mut last_total_ms: u128 = 0;

    for &n in SIZES {
        match time_compile_stages(n) {
            Ok((p, c, cg, sb)) => {
                let total = p + c + cg;
                let verdict = if total > HARD_TIMEOUT_MS {
                    "EXPLODED"
                } else if last_total_ms > 0 && total > 2 * last_total_ms && n > 1 {
                    // First doubling > 2x — flag the cliff candidate.
                    if cliff_n.is_none() {
                        cliff_n = Some(n);
                    }
                    "DOUBLE"
                } else {
                    "OK"
                };
                println!("{n},{p},{c},{cg},{total},{sb},{verdict}");
                last_total_ms = total;
                if total > HARD_TIMEOUT_MS {
                    println!(
                        "# STOPPED at n={n}: total={total}ms exceeded timeout {HARD_TIMEOUT_MS}ms"
                    );
                    break;
                }
            }
            Err(e) => {
                println!("{n},,,,{e},,ERR");
            }
        }
    }

    println!();
    match cliff_n {
        Some(_) => println!(
            "# ASCII curve (parse/check/codegen/total ms vs N):"
        ),
        None => println!("# No doubling detected up to N={}", SIZES.last().unwrap_or(&0)),
    }

    println!();
    println!("# Simple ASCII plot (total_ms vs N, log-ish scale, one '*' per 50ms):");
    println!("# N | total_ms | curve");
    println!("# --+----------+----------------------------------------------");
    for &n in SIZES {
        if let Ok((p, c, cg, _)) = time_compile_stages(n) {
            let total = p + c + cg;
            let stars = (total / 50).min(80) as usize;
            let bar: String = "*".repeat(stars);
            println!("# {n:>2} | {total:>8} | {bar}");
        }
    }

    // Hard assertion: nothing in the sweep may exceed the timeout. This is
    // the regression net — if a future PR makes this worse, CI will catch it.
    for &n in SIZES {
        if let Ok((p, c, cg, _)) = time_compile_stages(n) {
            let total = p + c + cg;
            assert!(
                total <= HARD_TIMEOUT_MS,
                "issue #1377 — compile blew up at n={n}: {total}ms > {HARD_TIMEOUT_MS}ms \
                 (parse={p}ms, check={c}ms, codegen={cg}ms)"
            );
        }
    }
}

/// Reproducer matching the exact module from issue #1377's report. Run this
/// individually with: `cargo test -p aiken-project compile_time_repro_1377 -- --nocapture`
///
/// The original repro has 24 `let`s + 24 chained `+`s. We time the full
/// parse+check+codegen pipeline and surface the numbers for eyeball
/// comparison against the parametric curve.
#[test]
fn compile_time_repro_1377() {
    const N: usize = 24;

    let (p, c, cg, sb) =
        time_compile_stages(N).expect("issue #1377 reproducer should compile");

    let total = p + c + cg;
    println!();
    println!("# issue #1377 reproducer (n={N})");
    println!("# parse:    {p:>8} ms");
    println!("# check:    {c:>8} ms");
    println!("# codegen:  {cg:>8} ms");
    println!("# total:    {total:>8} ms");
    println!("# source:   {sb} bytes");
    println!();

    // We do not assert a tight upper bound here — the whole point of the
    // parametric test is to characterize the curve. We just print.
    // The hard assertion lives in `compile_time_curve_1377`.
}
