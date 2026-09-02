// Fuzz test for aiken-lang/aiken issue #1407 (Shrinker + FreeUnique panic).
//
// Minimal repro from the issue:
//   use aiken/cbor.{deserialise}
//   const x = deserialise(#"80"#)
//   test minimal_trigger() {
//     [] == [as_data(deserialise(#"80"#)), as_data(x)]
//   }
//
// The panic happens in crates/uplc/src/optimize/shrinker.rs when a Name like
// "__cyclic_function_N" is encountered that doesn't exist in any scope.
//
// We generate variations of the minimal repro to find:
//   - Other reference patterns that trigger the same panic
//   - Edge cases around the cyclic_function wrapping
//   - Whether the panic reproduces with other builtin constant patterns

use super::TestProject;
use crate::module::CheckedModules;
use aiken_lang::ast::{Definition, TraceLevel, Tracing};
use uplc::{
    ast::{DeBruijn, Program},
    machine::cost_model::ExBudget,
};

// Variation matrix:
//   - helper shape: cbor_deserialise / serialise / other_constant
//   - number of references in test body: 1, 2, 3
//   - order: const_first vs inline_first (the issue notes "revert list order" fixes it)
//   - const count: 1, 2 (the issue mentions the const references another const?)

const VARIATIONS: &[(&str, &str)] = &[
    // ORIGINAL REPRO
    ("orig_repro",
     r##"
        use aiken/cbor.{deserialise}
        const x = deserialise(#"80"#)
        test minimal_trigger() {
          [] == [as_data(deserialise(#"80"#)), as_data(x)]
        }
     "##),

    // Reverted order (from issue: "Revert the list order" fixes it)
    ("reverted_order",
     r##"
        use aiken/cbor.{deserialise}
        const x = deserialise(#"80"#)
        test minimal_trigger() {
          [] == [as_data(x), as_data(deserialise(#"80"#))]
        }
     "##),

    // Inline x inside first as_data call
    ("inlined_const",
     r##"
        use aiken/cbor.{deserialise}
        const x = deserialise(#"80"#)
        test minimal_trigger() {
          [] == [as_data(deserialise(#"80"#)), as_data(deserialise(#"80"#))]
        }
     "##),

    // Two consts with shared pattern
    ("two_consts",
     r##"
        use aiken/cbor.{deserialise}
        const x = deserialise(#"80"#)
        const y = deserialise(#"81"#)
        test two_consts() {
          [] == [as_data(x), as_data(y)]
        }
     "##),

    // Different CBOR bytes
    ("different_bytes",
     r##"
        use aiken/cbor.{deserialise}
        const x = deserialise(#"00"#)
        test diff_bytes() {
          [] == [as_data(deserialise(#"80"#)), as_data(x)]
        }
     "##),

    // Three references
    ("three_refs",
     r##"
        use aiken/cbor.{deserialise}
        const x = deserialise(#"80"#)
        test three_refs() {
          [] == [as_data(deserialise(#"80"#)), as_data(deserialise(#"81"#)), as_data(x)]
        }
     "##),

    // as_data wrapping different ways
    ("no_as_data",
     r##"
        use aiken/cbor.{deserialise}
        const x = deserialise(#"80"#)
        test no_as_data() {
          deserialise(#"80"#) == x
        }
     "##),

    // Equal instead of list literal
    ("equal_op",
     r##"
        use aiken/cbor.{deserialise}
        const x = deserialise(#"80"#)
        test equal_op() {
          [deserialise(#"80"#)] == [x]
        }
     "##),
];

fn run_case(name: &str, src: &str) -> Result<(), String> {
    let mut project = TestProject::new();

    // Try parse — skip on parse error (parser edge cases aren't our target).
    let parsed = match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        project.parse(src)
    })) {
        Ok(p) => p,
        Err(_) => return Err(format!("{}: SKIP (parse error)", name)),
    };

    let modules = match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        CheckedModules::singleton(project.check(parsed))
    })) {
        Ok(m) => m,
        Err(_) => return Err(format!("{}: SKIP (type-check error)", name)),
    };

    let Some(checked_module) = modules.values().next() else {
        return Err(format!("{}: SKIP (no module)", name));
    };

    let mut generator = project.new_generator(Tracing::All(TraceLevel::Verbose));

    // Find all Test definitions and codegen each.
    let mut tests: Vec<(String, aiken_lang::expr::TypedExpr)> = Vec::new();
    for def in checked_module.ast.definitions() {
        if let Definition::Test(t) = def {
            tests.push((format!("{}::{}", name, t.name), t.body.clone()));
        }
    }

    for (target_name, body) in tests {
        let program_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            generator.generate_raw(&body, &[], &checked_module.name)
        }));

        let program = match program_result {
            Ok(p) => p,
            Err(_) => return Err(format!("{}: PANIC at codegen", target_name)),
        };

        // The actual bug is in the shrinker, which runs as part of UPLC
        // evaluation. So evaluate to trigger the shrinker path.
        let debruijn: Program<DeBruijn> = program.try_into().unwrap();
        let _ = debruijn.eval(ExBudget::max());
    }

    Ok(())
}

#[test]
fn fuzz_shrinker_free_unique() {
    let mut panics = Vec::new();
    let mut skips = 0;

    for (name, src) in VARIATIONS {
        match run_case(name, src) {
            Ok(()) => {}
            Err(e) if e.contains("SKIP") => {
                skips += 1;
                eprintln!("{}", e);
            }
            Err(e) => {
                panics.push(e);
            }
        }
    }

    println!(
        "shrinker-free-unique fuzz: {} cases, {} skipped, {} panics",
        VARIATIONS.len(),
        skips,
        panics.len()
    );

    if !panics.is_empty() {
        panic!(
            "shrinker-free-unique fuzz caught panics (reproduces #1407): {:?}",
            panics
        );
    }
}
