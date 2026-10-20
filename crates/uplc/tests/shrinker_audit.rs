//! Soundness tests for the rewrites of the UPLC shrinker.
//!
//! The property: for a program `p`, `aiken_optimize_and_intern(p)` and its
//! baseline, the program the shrinker is supposed to preserve, either both
//! fail or both succeed. A rewrite that must fire is checked on the optimized
//! text, which must have the shape the rule leaves; a rewrite that must not
//! apply is checked for soundness on an input where applying it would show.
//!
//! Programs are written in textual UPLC. Most are hand-written with lists that
//! are already typed, and their baseline is the program with only the
//! `__no_inline__` markers removed. The ones shaped like Aiken's output pass
//! `list data` to the typed-list builtins, and their baseline is
//! `Program::lower`, which adds the adapters.

use std::panic::{AssertUnwindSafe, catch_unwind};

use uplc::{
    ast::{Name, NamedDeBruijn, Program, Term},
    machine::cost_model::ExBudget,
    optimize::{aiken_optimize_and_intern, interner::CodeGenInterner},
    parser,
};

#[derive(Debug, Clone, PartialEq)]
enum Outcome {
    Value(Term<NamedDeBruijn>),
    Error,
    /// The program is not a closed term (the optimizer left a free variable).
    IllFormed,
    /// The optimizer or the conversion panicked.
    Panic,
}

impl Outcome {
    fn short(&self) -> String {
        match self {
            Outcome::Value(term) => {
                let text = format!("{term:?}");
                format!("Value({})", text.chars().take(100).collect::<String>())
            }
            other => format!("{other:?}"),
        }
    }

    fn succeeded(&self) -> bool {
        matches!(self, Outcome::Value(_))
    }
}

fn parse(src: &str) -> Program<Name> {
    parser::program(src).unwrap_or_else(|e| panic!("cannot parse {src}: {e}"))
}

fn run(program: Program<Name>) -> Outcome {
    match Program::<NamedDeBruijn>::try_from(program) {
        Err(_) => Outcome::IllFormed,
        Ok(program) => match program.eval(ExBudget::max()).result() {
            Ok(term) => Outcome::Value(term),
            Err(_) => Outcome::Error,
        },
    }
}

/// The baseline of a hand-written program: `p` interned with only the
/// `__no_inline__` markers removed.
fn reference(program: &Program<Name>) -> Program<Name> {
    let mut program = program.clone();

    CodeGenInterner::new().program(&mut program);

    program.clean_up_no_inlines()
}

/// The baseline of a program shaped like Aiken's output: `p` interned and
/// lowered.
fn lowered(program: &Program<Name>) -> Program<Name> {
    let mut program = program.clone();

    CodeGenInterner::new().program(&mut program);

    program.lower()
}

struct Run {
    optimized: Option<Program<Name>>,
    reference: Outcome,
    outcome: Outcome,
}

impl Run {
    fn optimized(&self) -> &Program<Name> {
        self.optimized
            .as_ref()
            .expect("the optimizer panicked, there is no optimized program")
    }

    /// The optimized program as one line of text, to look for the shape a
    /// rule leaves behind.
    fn text(&self) -> String {
        compact(self.optimized())
    }

    /// The soundness property: both fail or both succeed, with the same
    /// result when it is a constant. Two closures may differ in their code,
    /// so the tests that return one compare what it does on inputs.
    fn sound(&self) -> bool {
        match (&self.reference, &self.outcome) {
            (Outcome::Error, Outcome::Error) => true,
            (Outcome::Value(a), Outcome::Value(b)) => match (a, b) {
                (Term::Constant(_), _) | (_, Term::Constant(_)) => a == b,
                _ => true,
            },
            _ => false,
        }
    }

    fn describe(&self) -> String {
        format!(
            "reference: {}\noptimized: {}\noptimized program: {}",
            self.reference.short(),
            self.outcome.short(),
            self.optimized
                .as_ref()
                .map_or("<panic>".to_string(), compact)
        )
    }
}

fn compact(program: &Program<Name>) -> String {
    program
        .to_pretty()
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ")
}

fn parse_term(src: &str) -> Term<Name> {
    parser::term(src).unwrap_or_else(|e| panic!("cannot parse {src}: {e}"))
}

/// Run `src` applied to `args`, as its `baseline` and optimized.
///
/// The optimizer sees the program before the arguments are applied, as it
/// does for a validator, so the arguments are opaque to it.
fn check_against(baseline: fn(&Program<Name>) -> Program<Name>, src: &str, args: &[&str]) -> Run {
    let program = parse(src);
    let args: Vec<Term<Name>> = args.iter().map(|a| parse_term(a)).collect();

    let apply = |mut program: Program<Name>| {
        for arg in &args {
            program = program.apply_term(arg);
        }
        program
    };

    let reference = run(apply(baseline(&program)));

    let optimized = catch_unwind(AssertUnwindSafe(|| aiken_optimize_and_intern(program))).ok();

    let outcome = match &optimized {
        Some(optimized) => run(apply(optimized.clone())),
        None => Outcome::Panic,
    };

    Run {
        optimized,
        reference,
        outcome,
    }
}

const BAD: &str = "(con data (B #aa))";
const GOOD: &str = "(con data (I 5))";
const G1_BYTES: &str = "97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb";

fn program(body: &str) -> String {
    format!("(program 1.0.0 {body})")
}

/// One input of a test: the arguments the program is applied to and whether
/// the reference (unoptimized) program is expected to succeed on them.
struct Input {
    args: Vec<String>,
    reference_succeeds: bool,
}

fn on(args: &[&str], reference_succeeds: bool) -> Input {
    Input {
        args: args.iter().map(|a| a.to_string()).collect(),
        reference_succeeds,
    }
}

/// Check the soundness property on every input, against the baseline of a
/// hand-written program. Checking the reference outcome keeps the inputs
/// honest: a test feeds at least one input where the possibly failing term
/// fails and one where it does not.
fn assert_sound_on(src: &str, inputs: &[Input]) -> Vec<Run> {
    assert_sound_against(reference, src, inputs)
}

/// Like `assert_sound_on`, against `baseline`.
fn assert_sound_against(
    baseline: fn(&Program<Name>) -> Program<Name>,
    src: &str,
    inputs: &[Input],
) -> Vec<Run> {
    inputs
        .iter()
        .map(|input| {
            let args: Vec<&str> = input.args.iter().map(String::as_str).collect();
            let run = check_against(baseline, src, &args);

            assert_eq!(
                run.reference.succeeded(),
                input.reference_succeeds,
                "unexpected reference outcome\nprogram: {src}\nargs: {args:?}\n{}",
                run.describe()
            );

            assert!(
                run.sound(),
                "UNSOUND\nprogram: {src}\nargs: {args:?}\n{}",
                run.describe()
            );

            run
        })
        .collect()
}

/// Rewrites that must fire without changing what a program returns: each case checks the
/// optimized text for the shape its rule leaves, and the result on inputs that succeed and on
/// inputs that fail.
#[test]
fn rewrites_fire_and_keep_the_failures_they_must() {
    // inline_constr_ops keeps the unconstr check
    {
        for (exposer, pair_op) in [
            ("__constr_fields_exposer", "sndPair"),
            ("__constr_index_exposer", "fstPair"),
        ] {
            let src = program(&format!(
                "(lam d [(lam {exposer} [{exposer} d]) \
                    (lam __constr_var [(force (force (builtin {pair_op}))) \
                        [(builtin unConstrData) __constr_var]])])"
            ));

            let runs = assert_sound_on(
                &src,
                &[
                    on(&["(con data (Constr 0 [I 1]))"], true),
                    on(&[GOOD], false),
                    on(&[BAD], false),
                ],
            );

            let text = runs[0].text();

            // The rule fired: the exposer binder is gone and the call site does
            // the `unConstrData` itself.
            assert!(!text.contains(exposer), "{text}");
            assert!(text.contains("(builtin unConstrData) d"), "{text}");
        }
    }

    // bls381_compressor moves a constant without changing results
    {
        // The G1 constant sits in the branch that is not taken for `c = True`.
        // The compressor hoists its `uncompress` to the start of the program,
        // which can only cost budget: the compression of a valid point always
        // uncompresses.
        let src = program(&format!(
            "(lam c (force [(force (builtin ifThenElse)) c \
                (delay (con integer 0)) \
                (delay [(builtin bls12_381_G1_equal) \
                    (con bls12_381_G1_element 0x{G1_BYTES}) \
                    (con bls12_381_G1_element 0x{G1_BYTES})])]))"
        ));

        let runs = assert_sound_on(
            &src,
            &[
                on(&["(con bool True)"], true),
                on(&["(con bool False)"], true),
                // Not a bool: `ifThenElse` fails.
                on(&["(con integer 1)"], false),
            ],
        );

        let text = runs[0].text();

        assert!(text.contains("blst_p1_index_0"), "{text}");
        assert!(text.contains("(builtin bls12_381_G1_uncompress)"), "{text}");
        assert!(!text.contains("(con bls12_381_G1_element"), "{text}");
    }

    // builtin_force_reducer wraps fully forced builtin used applied
    {
        // `fstPair` needs two forces; used twice so the wrapper binding stays.
        let src = program(
            "(lam p [(builtin addInteger) \
                [(builtin unIData) [(force (force (builtin fstPair))) p]] \
                [(builtin unIData) [(force (force (builtin fstPair))) p]]])",
        );

        let runs = assert_sound_on(
            &src,
            &[
                on(&["(con (pair data data) (I 1, I 2))"], true),
                on(&["(con (pair data data) (B #aa, I 2))"], false),
            ],
        );

        assert!(runs[0].text().contains("__fst_pair_wrapped"));
    }

    // lambda_reducer drops a delayed error argument
    {
        let src = program(
            "(lam d [(lam f [(builtin addInteger) [(builtin unIData) d] (con integer 1)]) \
                (delay (error))])",
        );

        let runs = assert_sound_on(&src, &[on(&[GOOD], true), on(&[BAD], false)]);

        let text = runs[0].text();

        assert!(!text.contains("(lam f"), "{text}");
        assert!(!text.contains("(error)"), "{text}");
    }

    // lambda_reducer substitutes a delayed error where it is forced
    {
        let src = program(
            "(lam c (lam d [(lam x (force [(force (builtin ifThenElse)) c x (delay (con integer 1))])) \
                (delay (error))]))",
        );

        let runs = assert_sound_on(
            &src,
            &[
                // `x` is the branch taken: forcing it fails.
                on(&["(con bool True)", GOOD], false),
                on(&["(con bool False)", GOOD], true),
            ],
        );

        let text = runs[0].text();

        assert!(!text.contains("(lam x"), "{text}");
        assert!(text.contains("(delay (error))"), "{text}");
    }

    // identity_reducer keeps the argument evaluated
    {
        let src = program(
            "(lam d [(lam id [(builtin addInteger) [id [(builtin unIData) d]] [id (con integer 1)]]) \
                (lam x x)])",
        );

        let runs = assert_sound_on(&src, &[on(&[GOOD], true), on(&[BAD], false)]);

        let text = runs[0].text();

        assert!(!text.contains("(lam id"), "{text}");
        assert!(!text.contains("(lam x x)"), "{text}");
        assert!(text.contains("(builtin unIData) d"), "{text}");
    }

    // inline_reducer moves a failing argument to where it runs
    {
        // `x` occurs once, outside any delay: the failing argument is inlined.
        // Both operands can fail, so which failure is reported may change, but
        // whether the program fails may not.
        let src = program(
            "(lam l (lam d [(lam x [(builtin addInteger) \
                [(builtin unIData) [(force (builtin headList)) l]] x]) \
                [(builtin unIData) d]]))",
        );

        let runs = assert_sound_on(
            &src,
            &[
                on(&["(con (list data) [I 1])", GOOD], true),
                on(&["(con (list data) [I 1])", BAD], false),
                on(&["(con (list data) [])", GOOD], false),
                on(&["(con (list data) [])", BAD], false),
            ],
        );

        assert!(!runs[0].text().contains("(lam x"), "{}", runs[0].text());
    }

    // inline_reducer strips an unused value argument
    {
        // The unused argument is a lambda (a value): stripping it removes the
        // `unIData` inside it, which never ran.
        let src = program("(lam d [(lam k (con integer 1)) (lam y [(builtin unIData) y])])");

        let runs = assert_sound_on(&src, &[on(&[GOOD], true), on(&[BAD], true)]);

        assert!(!runs[0].text().contains("unIData"), "{}", runs[0].text());
    }

    // carry_args_to_branch inlines an argument used directly in the branch
    {
        // The intended use: `x` runs whenever the `then` branch is taken, and the
        // other branch is an error anyway.
        let src = program(
            "(lam c (lam d [(lam x (force [(force (builtin ifThenElse)) c \
                (delay [(builtin addInteger) x (con integer 1)]) (delay (error))])) \
                [(builtin unIData) d]]))",
        );

        let runs = assert_sound_on(
            &src,
            &[
                on(&["(con bool True)", GOOD], true),
                on(&["(con bool True)", BAD], false),
                on(&["(con bool False)", GOOD], false),
            ],
        );

        let text = runs[0].text();

        assert!(!text.contains("(lam x"), "{text}");
        assert!(
            text.contains(
                "(delay [ [ (builtin addInteger) [ (builtin unIData) d ] ] (con integer 1) ] )"
            ),
            "{text}"
        );
    }

    // force_delay_reducer cancels force of delay
    {
        let src = program("(lam d (force (delay [(builtin unIData) d])))");

        let runs = assert_sound_on(&src, &[on(&[GOOD], true), on(&[BAD], false)]);

        assert!(!runs[0].text().contains("delay"), "{}", runs[0].text());
    }

    // force_delay_reducer delay of force still fails when forced
    {
        // `delay (force v)` -> `v`: forcing either one fails unless `v` is a delay.
        let src = program("(lam d (force [(lam v (delay (force v))) d]))");

        let runs = assert_sound_on(
            &src,
            &[on(&[GOOD], false), on(&["(delay (con integer 1))"], true)],
        );

        assert!(!runs[0].text().contains("delay"), "{}", runs[0].text());
    }

    // cast_data_reducer unxdata of xdata keeps the inner failure
    {
        // `UnXData(XData x) -> x`: `XData` never fails on a well-typed `x` and
        // `x` itself is still evaluated.
        let src = program(
            "(lam d [(builtin unIData) [(builtin iData) \
                [(builtin addInteger) (con integer 1) [(builtin unIData) d]]]])",
        );
        let runs = assert_sound_on(&src, &[on(&[GOOD], true), on(&[BAD], false)]);
        assert!(
            !runs[0].text().contains("(builtin iData)"),
            "{}",
            runs[0].text()
        );

        let src = program("(lam n [(builtin unBData) [(builtin bData) n]])");
        // Only well-typed inputs: `unBData (bData n)` -> `n` would accept an
        // `n` that is not a bytestring, which typed code never produces.
        let runs = assert_sound_on(&src, &[on(&["(con bytestring #aa)"], true)]);
        assert!(
            !runs[0].text().contains("(builtin bData)"),
            "{}",
            runs[0].text()
        );

        // Lists and maps: `x` fails on the empty list (`tailList`).
        for (un_x_data, x_data, ok, empty) in [
            (
                "unListData",
                "listData",
                "(con (list data) [I 1])",
                "(con (list data) [])",
            ),
            (
                "unMapData",
                "mapData",
                "(con (list (pair data data)) [(I 1, I 2)])",
                "(con (list (pair data data)) [])",
            ),
        ] {
            let src = program(&format!(
                "(lam l [(builtin {un_x_data}) [(builtin {x_data}) [(force (builtin tailList)) l]]])"
            ));

            let runs = assert_sound_on(&src, &[on(&[ok], true), on(&[empty], false)]);

            assert!(
                !runs[0].text().contains(&format!("(builtin {x_data})")),
                "{}",
                runs[0].text()
            );
        }
    }

    // convert_arithmetic_ops turns subtract constant into add
    {
        let src =
            program("(lam d [(builtin subtractInteger) [(builtin unIData) d] (con integer 3)])");

        let runs = assert_sound_on(&src, &[on(&[GOOD], true), on(&[BAD], false)]);

        let text = runs[0].text();

        assert!(!text.contains("subtractInteger"), "{text}");
        assert!(text.contains("(builtin addInteger)"), "{text}");
        assert!(text.contains("(con integer -3)"), "{text}");
    }

    // remove_no_inlines marker around a function is removed and stays sound
    {
        let src = program(
            "(lam d [(lam f [(builtin addInteger) [f [(builtin unIData) d]] [f (con integer 1)]]) \
                (lam __no_inline__ (lam x [(builtin addInteger) x x]))])",
        );

        let runs = assert_sound_on(&src, &[on(&[GOOD], true), on(&[BAD], false)]);

        let text = runs[0].text();

        assert!(!text.contains("__no_inline__"), "{text}");
    }

    // builtin_curry_reducer two arg builtin keeps the failing operand
    {
        // `addInteger 7` occurs three times and is hoisted as a partial
        // application; the failing second operand stays at each site.
        let site = "[(builtin addInteger) (con integer 7) [(builtin unIData) d]]";
        let src = program(&format!(
            "(lam d [(builtin addInteger) {site} [(builtin addInteger) {site} {site}]])"
        ));

        let runs = assert_sound_on(&src, &[on(&[GOOD], true), on(&[BAD], false)]);

        let text = runs[0].text();

        assert!(text.contains("__add_integer_"), "{text}");
        assert!(text.contains("_curried"), "{text}");
    }

    // builtin_curry_reducer three arg builtin with constant prefix
    {
        // `sliceByteString 1 2` is a constant prefix at three sites; the third
        // operand is a parameter, so nothing folds away.
        let src = program(
            "(lam b1 (lam b2 (lam b3 [(builtin appendByteString) \
                [(builtin sliceByteString) (con integer 1) (con integer 2) b1] \
                [(builtin appendByteString) \
                    [(builtin sliceByteString) (con integer 1) (con integer 2) b2] \
                    [(builtin sliceByteString) (con integer 1) (con integer 2) b3]]])))",
        );

        let runs = assert_sound_on(
            &src,
            &[
                on(
                    &[
                        "(con bytestring #aabbcc)",
                        "(con bytestring #ddeeff)",
                        "(con bytestring #001122)",
                    ],
                    true,
                ),
                // Not a bytestring: `sliceByteString` fails.
                on(
                    &[
                        "(con bytestring #aabbcc)",
                        "(con integer 1)",
                        "(con bytestring #001122)",
                    ],
                    false,
                ),
            ],
        );

        let text = runs[0].text();

        assert!(text.contains("_curried"), "{text}");
    }

    // case_constr_apply_reducer still evaluates every argument and the head
    {
        // `[[[f a] b] c]` becomes `case (constr 0 [a b c]) [f]`: every operand is
        // still evaluated (constr fields are strict), only the order changes.
        let src =
            program("(lam f (lam d [f [(builtin unIData) d] (con integer 2) (con integer 3)]))");

        let first = "(lam x (lam y (lam z x)))";

        let runs = assert_sound_on(
            &src,
            &[
                on(&[first, GOOD], true),
                // A failing operand.
                on(&[first, BAD], false),
                // A failing head.
                on(&["(error)", GOOD], false),
                // Both fail.
                on(&["(error)", BAD], false),
                // A head that is not a function.
                on(&["(con integer 1)", GOOD], false),
            ],
        );

        let text = runs[0].text();

        assert!(text.contains("(case (constr 0"), "{text}");
    }
}

// ---------------------------------------------------------------------------
// builtin_force_reducer
// ---------------------------------------------------------------------------

#[test]
fn builtin_force_reducer_keeps_over_forcing_an_error() {
    // Three forces of `fstPair` fail; so must the wrapped form.
    let src = program("[(lam f (force f)) (force (force (builtin fstPair)))]");

    assert_sound_on(&src, &[on(&[], false)]);
}

#[test]
#[ignore = "builtin_force_reducer replaces a half-forced builtin by the fully forced wrapper, so a later legal `force` fails; not reachable from Aiken, not fixed"]
fn builtin_force_reducer_does_not_force_a_half_forced_builtin_further() {
    // Observed:
    //   reference: Value(Force(Force(Builtin(FstPair)))), a legal partially
    //              forced builtin value
    //   optimized: Error, program `(force (force (force (builtin fstPair))))`
    // The Aiken code generator never emits this shape.
    let src = program("[(lam f (force f)) (force (builtin fstPair))]");

    assert_sound_on(&src, &[on(&[], true)]);
}

// ---------------------------------------------------------------------------
// identity_reducer
// ---------------------------------------------------------------------------

#[test]
fn identity_reducer_with_an_unapplied_use_stays_sound() {
    // `id` is also passed around unapplied, so its binder must stay.
    let src = program(
        "(lam d [(lam id [(builtin addInteger) \
            [id [(builtin unIData) d]] [(lam g [g (con integer 1)]) id]]) \
            (lam x x)])",
    );

    assert_sound_on(&src, &[on(&[GOOD], true), on(&[BAD], false)]);
}

// ---------------------------------------------------------------------------
// inline_reducer
// ---------------------------------------------------------------------------

#[test]
fn inline_reducer_keeps_a_failing_argument_out_of_a_delay() {
    // `x` occurs under a `delay`: inlining would defer, and possibly skip,
    // the failure, so the argument stays where it is.
    let src = program("(lam d [(lam x (delay x)) [(builtin unIData) d]])");

    let runs = assert_sound_on(&src, &[on(&[GOOD], true), on(&[BAD], false)]);

    assert!(runs[0].text().contains("(lam x (delay x))"));
}

#[test]
fn inline_reducer_keeps_an_unused_failing_argument() {
    let src = program("(lam d [(lam _ (con integer 1)) [(builtin unIData) d]])");

    let runs = assert_sound_on(&src, &[on(&[GOOD], true), on(&[BAD], false)]);

    // Not stripped: the argument can fail.
    assert!(runs[0].text().contains("(builtin unIData) d"));
}

// ---------------------------------------------------------------------------
// carry_args_to_branch (`if c then body else error` special case)
// ---------------------------------------------------------------------------

#[test]
fn carry_args_to_branch_keeps_the_argument_when_the_other_branch_is_not_an_error() {
    // Control for the two ignored tests below: without the `error` branch the
    // special case does not apply and `x`, under a nested delay, stays bound.
    let src = program(
        "(lam c (lam d [(lam x (force [(force (builtin ifThenElse)) c \
            (delay (delay x)) (delay (con integer 1))])) \
            [(builtin unIData) d]]))",
    );

    let runs = assert_sound_on(
        &src,
        &[
            on(&["(con bool True)", GOOD], true),
            on(&["(con bool True)", BAD], false),
            on(&["(con bool False)", BAD], false),
        ],
    );

    assert!(runs[0].text().contains("(lam x"), "{}", runs[0].text());
}

#[test]
#[ignore = "carry_args_to_branch counts a delay nested in the then-branch as executed when the else branch is `error`, so a failing argument is inlined under it; not reachable from Aiken, not fixed"]
fn carry_args_to_branch_error_branch_nested_delay_keeps_the_failing_argument() {
    // Observed (c = True, d = B #aa):
    //   reference: Error (the argument `unIData d` is evaluated first)
    //   optimized: Value(Delay(unIData d)), i.e. the program
    //   `(force (case (constr 0 c (delay (delay [unIData d])) (delay (error))) ite))`
    let src = program(
        "(lam c (lam d [(lam x (force [(force (builtin ifThenElse)) c \
            (delay (delay x)) (delay (error))])) \
            [(builtin unIData) d]]))",
    );

    assert_sound_on(
        &src,
        &[
            on(&["(con bool True)", GOOD], true),
            on(&["(con bool True)", BAD], false),
        ],
    );
}

#[test]
#[ignore = "the carry_args_to_branch error-branch special case does not require a `force` above the ifThenElse, so a failing argument is inlined into a thunk; not reachable from Aiken, not fixed"]
fn carry_args_to_branch_error_branch_without_force_keeps_the_failing_argument() {
    // Observed (c = True, d = B #aa):
    //   reference: Error
    //   optimized: Value(Delay(unIData d))
    let src = program(
        "(lam c (lam d [(lam x [(force (builtin ifThenElse)) c (delay x) (delay (error))]) \
            [(builtin unIData) d]]))",
    );

    assert_sound_on(
        &src,
        &[
            on(&["(con bool True)", GOOD], true),
            on(&["(con bool True)", BAD], false),
        ],
    );
}

// ---------------------------------------------------------------------------
// force_delay_reducer
// ---------------------------------------------------------------------------

#[test]
#[ignore = "force_delay_reducer turns `delay (force v)` into `v`, which returns `v` instead of a delay when `v` isn't one; no Aiken program found so far produces this shape, not fixed"]
fn force_delay_reducer_delay_of_force_is_a_value_when_not_forced() {
    // Not forced: `delay (force v)` is a value, and so is `v`.
    let src = program("(lam d [(lam v (delay (force v))) d])");

    let runs = assert_sound_on(&src, &[on(&[GOOD], true)]);

    assert!(!runs[0].text().contains("delay"), "{}", runs[0].text());
}

// ---------------------------------------------------------------------------
// cast_data_reducer
// ---------------------------------------------------------------------------

#[test]
fn cast_data_reducer_constant_folds_only_fold_matching_shapes() {
    // (label, term, folds to a constant)
    for (label, term, folds) in [
        ("unIData of I", "[(builtin unIData) (con data (I 5))]", true),
        (
            "unIData of B",
            "[(builtin unIData) (con data (B #aa))]",
            false,
        ),
        ("iData", "[(builtin iData) (con integer 5)]", true),
        (
            "unBData of B",
            "[(builtin unBData) (con data (B #aa))]",
            true,
        ),
        (
            "unBData of I",
            "[(builtin unBData) (con data (I 1))]",
            false,
        ),
        ("bData", "[(builtin bData) (con bytestring #aa)]", true),
        (
            "unListData of List",
            "[(builtin unListData) (con data (List [I 1]))]",
            true,
        ),
        (
            "unListData of I",
            "[(builtin unListData) (con data (I 1))]",
            false,
        ),
        (
            "listData",
            "[(builtin listData) (con (list data) [I 1])]",
            true,
        ),
        (
            "unMapData of Map",
            "[(builtin unMapData) (con data (Map [(I 1, I 2)]))]",
            true,
        ),
        (
            "unMapData of I",
            "[(builtin unMapData) (con data (I 1))]",
            false,
        ),
        (
            "mapData",
            "[(builtin mapData) (con (list (pair data data)) [(I 1, I 2)])]",
            true,
        ),
    ] {
        let run = assert_sound_on(&program(term), &[on(&[], folds)]).remove(0);

        assert_eq!(
            run.text().contains("(builtin"),
            !folds,
            "{label}: {}",
            run.text()
        );
    }
}

// ---------------------------------------------------------------------------
// builtin_eval_reducer (and the `is_error_safe` guards behind it)
// ---------------------------------------------------------------------------

#[test]
fn builtin_eval_reducer_folds_only_what_the_machine_accepts() {
    let huge = (num_bigint::BigInt::from(1u32) << 70000usize).to_string();
    let long_key = "00".repeat(33);

    // (label, builtin, term, folds). The `false` entries include the builtins
    // whose `is_error_safe` guard is more permissive than the machine
    // (constrData tag, integerToByteString size, shift/rotate,
    // replicateByte length, insertCoin key): the evaluation that precedes the
    // fold must keep them from folding.
    let cases: Vec<(&str, &str, String, bool)> = vec![
        (
            "divide ok",
            "divideInteger",
            "[(builtin divideInteger) (con integer 6) (con integer 3)]".into(),
            true,
        ),
        (
            "divide by zero",
            "divideInteger",
            "[(builtin divideInteger) (con integer 6) (con integer 0)]".into(),
            false,
        ),
        (
            "replicateByte ok",
            "replicateByte",
            "[(builtin replicateByte) (con integer 3) (con integer 0)]".into(),
            true,
        ),
        (
            "replicateByte over 8192",
            "replicateByte",
            "[(builtin replicateByte) (con integer 8193) (con integer 0)]".into(),
            false,
        ),
        (
            "consByteString ok",
            "consByteString",
            "[(builtin consByteString) (con integer 255) (con bytestring #)]".into(),
            true,
        ),
        (
            "consByteString 256",
            "consByteString",
            "[(builtin consByteString) (con integer 256) (con bytestring #)]".into(),
            false,
        ),
        (
            "constrData ok",
            "constrData",
            "[(builtin constrData) (con integer 1) (con (list data) [])]".into(),
            true,
        ),
        (
            "constrData tag 2^64",
            "constrData",
            "[(builtin constrData) (con integer 18446744073709551616) (con (list data) [])]"
                .into(),
            false,
        ),
        (
            "integerToByteString ok",
            "integerToByteString",
            "[(builtin integerToByteString) (con bool True) (con integer 0) (con integer 5)]"
                .into(),
            true,
        ),
        (
            "integerToByteString too big",
            "integerToByteString",
            format!(
                "[(builtin integerToByteString) (con bool True) (con integer 0) (con integer {huge})]"
            ),
            false,
        ),
        (
            "shiftByteString shift not an i64",
            "shiftByteString",
            "[(builtin shiftByteString) (con bytestring #aa) (con integer 9223372036854775808)]"
                .into(),
            false,
        ),
        (
            "rotateByteString shift not an i64",
            "rotateByteString",
            "[(builtin rotateByteString) (con bytestring #aa) (con integer 9223372036854775808)]"
                .into(),
            false,
        ),
        (
            "indexByteString out of bounds",
            "indexByteString",
            "[(builtin indexByteString) (con bytestring #aa) (con integer 1)]".into(),
            false,
        ),
        (
            "insertCoin ok",
            "insertCoin",
            "[(builtin insertCoin) (con bytestring #00) (con bytestring #aa) (con integer 1) (con value [])]"
                .into(),
            true,
        ),
        (
            "insertCoin key over 32 bytes",
            "insertCoin",
            format!(
                "[(builtin insertCoin) (con bytestring #{long_key}) (con bytestring #aa) (con integer 1) (con value [])]"
            ),
            false,
        ),
    ];

    for (label, builtin, term, folds) in cases {
        // Evaluated right away.
        let run = assert_sound_on(&program(&term), &[on(&[], folds)]).remove(0);

        assert_eq!(
            run.text().contains(&format!("(builtin {builtin})")),
            !folds,
            "{label}: {}",
            run.text()
        );

        // Behind a `delay` the call never runs, so both succeed whether or
        // not it folds, and the failing ones must keep their call.
        let run = assert_sound_on(&program(&format!("(delay {term})")), &[on(&[], true)]).remove(0);

        assert_eq!(
            run.text().contains(&format!("(builtin {builtin})")),
            !folds,
            "{label} (delayed): {}",
            run.text()
        );
    }
}

// ---------------------------------------------------------------------------
// remove_no_inlines
// ---------------------------------------------------------------------------

#[test]
fn remove_no_inlines_marker_around_a_failing_term_does_not_hide_it() {
    // The semantics the shrinker preserves is the one of the stripped
    // program, where the argument is `unIData d` and can fail. The inliner
    // must see through the marker and keep the unused failing argument.
    let src =
        program("(lam d [(lam f (con integer 1)) (lam __no_inline__ [(builtin unIData) d])])");

    let runs = assert_sound_on(&src, &[on(&[GOOD], true), on(&[BAD], false)]);

    let text = runs[0].text();

    assert!(!text.contains("__no_inline__"), "{text}");
    assert!(text.contains("(builtin unIData) d"), "{text}");
}

// ---------------------------------------------------------------------------
// builtin_curry_reducer
// ---------------------------------------------------------------------------

#[test]
fn builtin_curry_reducer_three_arg_builtin_with_a_failing_first_argument() {
    // Three sites of `sliceByteString (unIData d) 2 b_i`. The
    // prefix `sliceByteString (unIData d)` isn't a constant, so nothing that
    // extends it is hoisted: hoisting `prefix 2` used to refer to a prefix
    // variable that was never defined (`FreeUnique` in `afterwards()`).
    let site = |b: &str| {
        format!(
            "[(builtin sliceByteString) [(builtin unIData) d] (con integer 2) (con bytestring #{b})]"
        )
    };
    let src = program(&format!(
        "(lam d [(builtin appendByteString) {} [(builtin appendByteString) {} {}]])",
        site("aabbcc"),
        site("ddeeff"),
        site("001122")
    ));

    assert_sound_on(&src, &[on(&[GOOD], true), on(&[BAD], false)]);
}

#[test]
fn builtin_curry_reducer_three_arg_builtin_with_a_variable_first_argument() {
    // Same shape with a plain variable as the first argument
    // (`sliceByteString s 2 b_i`), a well-typed program.
    let site =
        |b: &str| format!("[(builtin sliceByteString) s (con integer 2) (con bytestring #{b})]");
    let src = program(&format!(
        "(lam s [(builtin appendByteString) {} [(builtin appendByteString) {} {}]])",
        site("aabbcc"),
        site("ddeeff"),
        site("001122")
    ));

    assert_sound_on(&src, &[on(&["(con integer 1)"], true)]);
}

// ---------------------------------------------------------------------------
// typed_list_convert_arg
// ---------------------------------------------------------------------------

/// The code generator passes `List<Int>` and `List<G1>` arguments of these builtins as lists of
/// data, and the shrinker adds the per-element conversion: constant lists are retyped, other
/// lists are checked element by element.
#[test]
fn typed_list_adapters_check_every_element() {
    // typed list convert arg checks each element of a non constant list
    {
        let src = program("(lam l [(builtin writeBits) (con bytestring #00) l (con bool True)])");

        // An element that is not an integer fails in the converter.
        let runs = assert_sound_against(
            lowered,
            &src,
            &[
                on(&["(con (list data) [I 1])"], true),
                on(&["(con (list data) [B #aa])"], false),
            ],
        );
        let text = runs[0].text();

        assert!(text.contains("__indices_converter"), "{text}");
        assert!(text.contains("(builtin unIData)"), "{text}");
    }

    // typed list convert arg retypes a constant list
    {
        let src = program(
            "(lam d [(builtin writeBits) (con bytestring #00) (con (list data) [I 1]) (con bool True)])",
        );

        let text = assert_sound_against(lowered, &src, &[on(&[GOOD], true)])[0].text();

        assert!(text.contains("(con (list integer) [1])"), "{text}");
    }

    // typed list convert arg checks each point of a g1 list
    {
        let src =
            program("(lam l [(builtin bls12_381_G1_multiScalarMul) (con (list data) [I 2]) l])");
        let point = format!("(con (list data) [B #{G1_BYTES}])");

        // Not a point, the `uncompress` of the converter fails; not bytes, its `unBData` fails.
        let runs = assert_sound_against(
            lowered,
            &src,
            &[
                on(&[&point], true),
                on(&["(con (list data) [B #aa])"], false),
                on(&["(con (list data) [I 1])"], false),
            ],
        );
        let text = runs[0].text();

        assert!(text.contains("__g1_elements_converter"), "{text}");
    }
}

#[test]
#[ignore = "typed_list_convert_arg panics on a `list integer` constant, which it expects as `list data`; not reachable from Aiken, not fixed"]
fn typed_list_convert_arg_keeps_an_already_typed_constant_list_of_integers() {
    // A well-typed program that already passes `list integer`: there is
    // nothing to convert. The Aiken code generator never emits typed constant
    // lists for these arguments.
    let src = program(
        "(lam d [(builtin writeBits) (con bytestring #00) (con (list integer) [1]) (con bool True)])",
    );

    assert_sound_on(&src, &[on(&[GOOD], true)]);
}

#[test]
#[ignore = "typed_list_convert_arg always applies the points converter, so a well-typed `list bls12_381_G1_element` constant fails at runtime (unBData on a point); not reachable from Aiken, not fixed"]
fn typed_list_convert_arg_keeps_already_typed_constant_lists_of_points() {
    // The `list integer` scalars are left alone, but the `list G1` points go through
    // `__g1_elements_converter` and fail.
    let src = program(&format!(
        "(lam d [(builtin bls12_381_G1_multiScalarMul) (con (list integer) [2]) \
            (con (list bls12_381_G1_element) [0x{G1_BYTES}])])"
    ));

    assert_sound_on(&src, &[on(&[GOOD], true)]);
}

// ---------------------------------------------------------------------------
// split_body_lambda
// ---------------------------------------------------------------------------

/// `split_body_lambda` hoists an unapplied lambda over an argument that can't fail: a value, or a
/// partially applied builtin.
#[test]
fn split_body_lambda_hoists_over_values() {
    // a value argument
    {
        // The argument is a string constant: a value the inliner leaves alone
        // (it is used twice), so only `split_body_lambda` rewrites the shape.
        // Moving the unapplied `z` outside is harmless for a value. The rule
        // fires: `z` now encloses the application of `v`.
        let src = program(
            "(lam s [(lam v (lam z [(builtin appendString) v [(builtin appendString) v [(builtin appendString) z s]]])) \
                (con string \"a\")])",
        );

        let runs = assert_sound_on(
            &src,
            &[on(&["(con string \"b\")", "(con string \"c\")"], true)],
        );

        let text = runs[0].text();

        assert!(
            text.starts_with("(program 1.0.0 (lam s (lam z [ (lam v"),
            "{text}"
        );
    }

    // a partial builtin
    {
        let src = program(
            "(lam s [(lam v (lam z [(builtin appendString) [v z] [v s]])) \
                [(builtin appendString) (con string \"a\")]])",
        );

        let runs = assert_sound_on(
            &src,
            &[on(&["(con string \"b\")", "(con string \"c\")"], true)],
        );

        let text = runs[0].text();

        assert!(
            text.starts_with("(program 1.0.0 (lam s (lam z [ (lam v"),
            "{text}"
        );
    }
}

#[test]
fn split_body_lambda_does_not_defer_a_failing_argument_under_a_returned_closure() {
    // `unIData d` runs before the closure is returned. Hoisting `z` above it
    // would give `(lam d (lam z [(lam v v) [(builtin unIData) d]]))`, which
    // returns a closure for d = B #aa.
    let src = program("(lam d [(lam v (lam z v)) [(builtin unIData) d]])");

    assert_sound_on(&src, &[on(&[GOOD], true), on(&[BAD], false)]);
}

/// A builtin applied before it is forced fails, however it is forced afterwards. The full
/// optimizer never shows `split_body_lambda` this shape (`builtin_force_reducer` first replaces
/// the builtin with its forced wrapper), so the rule runs alone here.
#[test]
#[ignore = "split_body_lambda's is_value counts forces and applications in any order, so it hoists a lambda over a builtin applied before it is forced; not reachable from Aiken, not fixed"]
fn split_body_lambda_does_not_defer_a_builtin_applied_before_it_is_forced() {
    for partial in [
        "(force [(builtin ifThenElse) (con bool True)])",
        "[(force [(builtin ifThenElse) (con bool True)]) (con integer 1)]",
        "(force (force [(builtin fstPair) (con (pair integer integer) (1, 2))]))",
    ] {
        let src = program(&format!("(lam d [(lam v (lam z v)) {partial}])"));
        let program = reference(&parse(&src));
        let arg = parse_term(GOOD);

        assert_eq!(run(program.apply_term(&arg)), Outcome::Error, "{partial}");

        let split = program.split_body_lambda_reducer();
        assert_eq!(
            run(split.apply_term(&arg)),
            Outcome::Error,
            "{partial}: {}",
            compact(&split)
        );
    }
}
