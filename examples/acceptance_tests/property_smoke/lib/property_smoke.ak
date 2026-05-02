//// Smoke fixture exercising every canonical verify IR shape.
////
//// This fixture is consumed by the `verify_real_toolchain` CI job, which
//// installs a real Lean toolchain via elan and a real Z3, then runs
//// `aiken verify run --json` against this project. Every property test
//// must prove cleanly (no `Partial`, no `WitnessProved`, no `Failed`,
//// no `TimedOut`, no `Unknown`) for the job to pass.
////
//// Each test pairs a canonical fuzzer shape with a property chosen to
//// exercise the corresponding verifier lowering. Most are bound-sensitive;
//// the Bool `such_that` smoke is a tautology so it still covers the filter
//// lowering path without assuming unsound singleton semantics.

use aiken/builtin
use aiken/collection/list
use aiken/fuzz

// ---------------------------------------------------------------------------
// Shape 1 — Int range
//
// `fuzz.int_between(0, 100)` lowers to a bounded `Integer` domain. The body
// `x >= 0` is a tautology under the lower bound and survives Z3 elimination
// without any further hypothesis.
// ---------------------------------------------------------------------------

test prop_int_within_bounds(x via fuzz.int_between(0, 100)) {
  x >= 0
}

// ---------------------------------------------------------------------------
// Shape 2 — ByteArray bounded length
//
// `fuzz.bytearray_between(4, 32)` lowers to a `ByteArray` domain bounded
// by the inclusive length pair `[4, 32]`. The body checks both bounds
// directly via the stdlib `length` builtin.
// ---------------------------------------------------------------------------

test prop_bytearray_length_within_bounds(b via fuzz.bytearray_between(4, 32)) {
  builtin.length_of_bytearray(b) >= 4 && builtin.length_of_bytearray(b) <= 32
}

// ---------------------------------------------------------------------------
// Shape 3 — Tuple<Int, Int>
//
// `fuzz.both` produces a tuple-typed fuzzer; the canonical IR splits the
// pair into two independent integer domains. The bounds are intentionally
// asymmetric so any domain aliasing/collapse regression breaks the proof.
// ---------------------------------------------------------------------------

test prop_tuple_int_int_components_stay_within_bounds(
  pair via fuzz.both(fuzz.int_between(0, 10), fuzz.int_between(20, 30)),
) {
  let (a, b) = pair
  a >= 0 && a <= 10 && b >= 20 && b <= 30
}

// ---------------------------------------------------------------------------
// Shape 4 — List<Data> bounded length
//
// `fuzz.list_between` over a `Fuzzer<Data>` exercises the list lowering
// for the `Data` element type. We synthesise the Data fuzzer by mapping
// `fuzz.int()` through `as_data`. The property asserts the generated list
// length respects the configured upper bound.
// ---------------------------------------------------------------------------

fn data_fuzzer() -> Fuzzer<Data> {
  fuzz.map(fuzz.int(), fn(n) { as_data(n) })
}

test prop_list_data_length_stays_within_upper_bound(
  xs via fuzz.list_between(data_fuzzer(), 0, 4),
) {
  list.length(xs) <= 4
}

// ---------------------------------------------------------------------------
// Shape 5 — Bool with such_that
//
// `fuzz.such_that(fuzz.bool(), p)` must still lower through the filter-shaped
// fuzzer path, but the verifier currently proves it over the conservative Bool
// domain rather than a singleton `True` domain. This smoke therefore only
// checks that the filter-shaped fuzzer remains accepted; it does not claim the
// predicate itself is reflected in the theorem precondition.
// ---------------------------------------------------------------------------

fn keep_only_true(flag: Bool) -> Bool {
  flag
}

fn smoke_bool_fuzzer() -> Fuzzer<Bool> {
  fuzz.such_that(fuzz.bool(), keep_only_true)
}

test prop_bool_such_that_lowers_without_singleton_assumption(
  flag via smoke_bool_fuzzer(),
) {
  flag == flag
}
