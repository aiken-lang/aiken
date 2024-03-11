use crate::{
    ast::{Definition, ModuleKind, TraceLevel, Tracing, TypedModule, UntypedModule},
    builtins,
    expr::TypedExpr,
    parser,
    tipo::error::{Error, UnifyErrorSituation, Warning},
    IdGenerator,
};
use std::collections::HashMap;

fn parse(source_code: &str) -> UntypedModule {
    let kind = ModuleKind::Lib;
    let (ast, _) = parser::module(source_code, kind).expect("Failed to parse module");
    ast
}

fn check_module(
    ast: UntypedModule,
    kind: ModuleKind,
) -> Result<(Vec<Warning>, TypedModule), (Vec<Warning>, Error)> {
    let id_gen = IdGenerator::new();

    let mut warnings = vec![];

    let mut module_types = HashMap::new();
    module_types.insert("aiken".to_string(), builtins::prelude(&id_gen));
    module_types.insert("aiken/builtin".to_string(), builtins::plutus(&id_gen));

    let result = ast.infer(
        &id_gen,
        kind,
        "test/project",
        &module_types,
        Tracing::All(TraceLevel::Verbose),
        &mut warnings,
    );

    result
        .map(|o| (warnings.clone(), o))
        .map_err(|e| (warnings, e))
}

fn check(ast: UntypedModule) -> Result<(Vec<Warning>, TypedModule), (Vec<Warning>, Error)> {
    check_module(ast, ModuleKind::Lib)
}

fn check_validator(
    ast: UntypedModule,
) -> Result<(Vec<Warning>, TypedModule), (Vec<Warning>, Error)> {
    check_module(ast, ModuleKind::Validator)
}

#[test]
fn bls12_381_elements_in_data_type() {
    let source_code = r#"
        type Datum {
          D0(G1Element)
          D1(G2Element)
        }
    "#;

    assert!(check(parse(source_code)).is_ok())
}

#[test]
fn bls12_381_ml_result_in_data_type() {
    let source_code = r#"
        type Datum {
          thing: MillerLoopResult
        }
    "#;

    let res = check(parse(source_code));

    dbg!(&res);

    assert!(matches!(res, Err((_, Error::IllegalTypeInData { .. }))))
}

#[test]
fn validator_illegal_return_type() {
    let source_code = r#"
      validator {
        fn foo(d, r, c) {
          1
        }
      }
    "#;

    assert!(matches!(
        check_validator(parse(source_code)),
        Err((_, Error::ValidatorMustReturnBool { .. }))
    ))
}

#[test]
fn implicitly_discard_void() {
    let source_code = r#"
      pub fn label(str: String) -> Void {
        trace str Void
      }
    "#;

    let (warnings, _) = check_validator(parse(source_code)).expect("should type-check");

    assert!(warnings.is_empty(), "no warnings: {warnings:#?}");
}

#[test]
fn validator_illegal_arity() {
    let source_code = r#"
      validator {
        fn foo(c) {
          True
        }
      }
    "#;

    assert!(matches!(
        check_validator(parse(source_code)),
        Err((_, Error::IncorrectValidatorArity { .. }))
    ))
}

#[test]
fn list_illegal_inhabitants() {
    let source_code = r#"
        fn main() {
          [identity]
        }
    "#;

    assert!(matches!(
        check_validator(parse(source_code)),
        Err((_, Error::IllegalTypeInData { .. }))
    ))
}

#[test]
fn tuple_illegal_inhabitants() {
    let source_code = r#"
        fn main() {
          (identity, always)
        }
    "#;

    assert!(matches!(
        check_validator(parse(source_code)),
        Err((_, Error::IllegalTypeInData { .. }))
    ))
}

#[test]
fn illegal_inhabitants_nested() {
    let source_code = r#"
        fn main() {
          [(identity, always)]
        }
    "#;

    assert!(matches!(
        check_validator(parse(source_code)),
        Err((_, Error::IllegalTypeInData { .. }))
    ))
}

#[test]
fn illegal_inhabitants_returned() {
    let source_code = r#"
        type Fuzzer<a> = fn(PRNG) -> (a, PRNG)

        fn constant(a: a) -> Fuzzer<a> {
          fn (prng) {
            (a, prng)
          }
        }

        fn main() -> Fuzzer<Fuzzer<Int>> {
          constant(constant(42))
        }
    "#;

    assert!(matches!(
        check_validator(parse(source_code)),
        Err((_, Error::IllegalTypeInData { .. }))
    ))
}

#[test]
fn mark_constructors_as_used_via_field_access() {
    let source_code = r#"
      type Datum {
        D0(D0Params)
        D1(D1Params)
      }

      type D0Params {
        foo: Int,
      }

      type D1Params {
        bar: Int,
      }

      validator {
        fn foo(d: Datum, _r, _c) {
          when d is {
            D0(params) -> params.foo == 1
            D1(_params) -> False
          }
        }
      }
    "#;

    let (warnings, _) = check_validator(parse(source_code)).unwrap();

    assert_eq!(warnings.len(), 1)
}

#[test]
fn validator_correct_form() {
    let source_code = r#"
      validator {
        fn foo(d, r, c) {
          True
        }
      }
    "#;

    assert!(check_validator(parse(source_code)).is_ok())
}

#[test]
fn validator_in_lib_warning() {
    let source_code = r#"
      validator {
        fn foo(c) {
          True
        }
      }
    "#;

    let (warnings, _) = check(parse(source_code)).unwrap();

    assert!(matches!(
        warnings[0],
        Warning::ValidatorInLibraryModule { .. }
    ))
}

#[test]
fn multi_validator() {
    let source_code = r#"
      validator(foo: ByteArray, bar: Int) {
        fn spend(_d, _r, _c) {
          foo == #"aabb"
        }

        fn mint(_r, _c) {
          bar == 0
        }
      }
    "#;

    let (warnings, _) = check_validator(parse(source_code)).unwrap();

    assert_eq!(warnings.len(), 0)
}

#[test]
fn multi_validator_warning() {
    let source_code = r#"
      validator(foo: ByteArray, bar: Int) {
        fn spend(_d, _r, _c) {
          foo == #"aabb"
        }

        fn mint(_r, _c) {
          True
        }
      }
    "#;

    let (warnings, _) = check_validator(parse(source_code)).unwrap();

    assert!(matches!(
        warnings[0],
        Warning::UnusedVariable { ref name, .. } if name == "bar"
    ))
}

#[test]
fn exhaustiveness_simple() {
    let source_code = r#"
        type Foo {
          Bar
          Baz
        }

        fn foo() {
          let thing = Bar
          when thing is {
            Bar -> True
          }
        }
    "#;

    assert!(matches!(
        check(parse(source_code)),
        Err((
            _,
            Error::NotExhaustivePatternMatch {
                unmatched,
                ..
            }
        )) if unmatched[0] == "Baz"
    ))
}

#[test]
fn validator_args_no_annotation() {
    let source_code = r#"
      validator(d) {
        fn foo(a, b, c) {
          True
        }
      }
    "#;

    let (_, module) = check_validator(parse(source_code)).unwrap();

    module.definitions().for_each(|def| {
        let Definition::Validator(validator) = def else {
            unreachable!()
        };

        validator.params.iter().for_each(|param| {
            assert!(param.tipo.is_data());
        });

        validator.fun.arguments.iter().for_each(|arg| {
            assert!(arg.tipo.is_data());
        })
    })
}

#[test]
fn exhaustiveness_missing_empty_list() {
    let source_code = r#"
        fn foo() {
          let thing = [1, 2]
          when thing is {
            [a, ..] -> True
          }
        }
    "#;

    assert!(matches!(
        check(parse(source_code)),
        Err((
            _,
            Error::NotExhaustivePatternMatch {
                unmatched,
                ..
            }
        )) if unmatched[0] == "[]"
    ))
}

#[test]
fn exhaustiveness_missing_list_wildcards() {
    let source_code = r#"
        fn foo() {
          let thing = [1, 2]
          when thing is {
            [] -> True
          }
        }
    "#;

    assert!(matches!(
        check(parse(source_code)),
        Err((
            _,
            Error::NotExhaustivePatternMatch {
                unmatched,
                ..
            }
        )) if unmatched[0] == "[_, ..]"
    ))
}

#[test]
fn exhaustiveness_missing_list_wildcards_2() {
    let source_code = r#"
        fn foo() {
          let thing = [1, 2]
          when thing is {
            [] -> True
            [a] -> True
          }
        }
    "#;

    assert!(matches!(
        check(parse(source_code)),
        Err((
            _,
            Error::NotExhaustivePatternMatch {
                unmatched,
                ..
            }
        )) if unmatched[0] == "[_, _, ..]"
    ))
}

#[test]
fn exhaustiveness_int() {
    let source_code = r#"
        fn foo() {
          let thing = 1
          when thing is {
            1 -> True
          }
        }
    "#;

    assert!(matches!(
        check(parse(source_code)),
        Err((
            _,
            Error::NotExhaustivePatternMatch {
                unmatched,
                ..
            }
        )) if unmatched[0] == "_"
    ))
}

#[test]
fn exhaustiveness_int_redundant() {
    let source_code = r#"
        fn foo() {
          let thing = 1
          when thing is {
            1 -> True
            1 -> True
            _ -> True
          }
        }
    "#;

    assert!(matches!(
        check(parse(source_code)),
        Err((
            _,
            Error::RedundantMatchClause {
                original: Some(_),
                ..
            }
        ))
    ))
}

#[test]
fn exhaustiveness_let_binding() {
    let source_code = r#"
        fn foo() {
          let Some(x) = None
          True
        }
    "#;

    assert!(matches!(
        check(parse(source_code)),
        Err((
            _,
            Error::NotExhaustivePatternMatch {
                is_let,
                unmatched,
                ..
            }
        )) if unmatched[0] == "None" && is_let
    ))
}

#[test]
fn exhaustiveness_expect() {
    let source_code = r#"
        fn foo() {
          expect Some(x) = None
          True
        }
    "#;

    assert!(check(parse(source_code)).is_ok())
}

#[test]
fn exhaustiveness_expect_no_warning() {
    let source_code = r#"
        pub type A {
          int: Int,
          b: B,
        }

        pub type B {
          B0(Int)
          B1(Int)
        }

        pub fn bad_let(x: A, _: A) {
          expect A { b: B0(int), .. } = x
          int > 0
        }
    "#;

    let (warnings, _) = check(parse(source_code)).unwrap();

    assert_eq!(warnings.len(), 0)
}

#[test]
fn exhaustiveness_expect_warning() {
    let source_code = r#"
        pub type A {
          int: Int,
          b: Int,
        }

        pub fn thing(x: A, _: A) {
          expect A { b, .. } = x
          b > 0
        }
    "#;

    let (warnings, _) = check(parse(source_code)).unwrap();

    assert!(matches!(
        warnings[0],
        Warning::SingleConstructorExpect { .. }
    ))
}

#[test]
fn exhaustiveness_missing_constr_with_args() {
    let source_code = r#"
        type Foo {
          Bar
          Why(Int)
          Baz { other: Int }
        }

        fn foo() {
          let thing = Bar
          when thing is {
            Bar -> True
          }
        }
    "#;

    assert!(matches!(
        check(parse(source_code)),
        Err((
            _,
            Error::NotExhaustivePatternMatch {
                unmatched,
                ..
            }
        )) if unmatched[0] == "Why(_)" && unmatched[1] == "Baz { other }"
    ))
}

#[test]
fn exhaustiveness_redundant_pattern() {
    let source_code = r#"
        type Foo {
          A
          B
        }

        fn foo(a: Foo) {
          when a is {
            A -> todo
            B -> todo
            _ -> todo
          }
        }
    "#;

    assert!(matches!(
        check(parse(source_code)),
        Err((_, Error::RedundantMatchClause { original: None, .. }))
    ))
}

#[test]
fn exhaustiveness_redundant_pattern_2() {
    let source_code = r#"
        type Foo {
          A
          B
        }

        fn foo(a: Foo) {
          when a is {
            A -> todo
            B -> todo
            A -> todo
          }
        }
    "#;

    assert!(matches!(
        check(parse(source_code)),
        Err((
            _,
            Error::RedundantMatchClause {
                original: Some(_),
                ..
            }
        ))
    ))
}

#[test]
fn exhaustiveness_complex() {
    let source_code = r#"
        type Foo {
          Bar
          Why(Int)
          Baz { other: Int }
        }

        type Hello {
          Yes
          No { idk: Int, thing: Foo }
        }

        fn foo() {
          let thing = ((Yes, 1), (Yes, [1, 2]))
          when thing is {
            ((Yes, _), (Yes, [])) -> True
            ((Yes, _), (No { .. }, _)) -> True
            ((No { .. }, _), (No { .. }, _)) -> True
          }
        }
    "#;

    assert!(matches!(
        check(parse(source_code)),
        Err((
            _,
            Error::NotExhaustivePatternMatch {
                unmatched,
                ..
            }
        )) if unmatched[0] == "((Yes, _), (Yes, [_, ..]))" && unmatched[1] == "((No { idk, thing }, _), (Yes, _))"
    ))
}

#[test]
fn exhaustiveness_tuple() {
    let source_code = r#"
        fn foo() {
            when (14, True) is {
                (14, True) -> Void
            }
        }
    "#;

    assert!(matches!(
        check(parse(source_code)),
        Err((
            _,
            Error::NotExhaustivePatternMatch {
                unmatched,
                ..
            }
        )) if unmatched[0] == "(_, _)"
    ))
}

#[test]
fn exhaustiveness_nested_list_and_tuples() {
    fn assert_step(step: &str, expected: &str) {
        let result = check(parse(step));
        assert!(matches!(
            result,
            Err((
                _,
                Error::NotExhaustivePatternMatch {
                    unmatched,
                    ..
                }
            )) if unmatched[0] == expected
        ));
    }

    assert_step(
        r#"
        fn foo() {
            let xs : List<(List<(Int, Bool)>, Int)> = [([(14, True)], 42)]
            when xs is {
                [                      ] -> Void
                [([(14, True)], 42), ..] -> Void
            }
        }
        "#,
        "[([], _), ..]",
    );

    assert_step(
        r#"
        fn foo() {
            let xs : List<(List<(Int, Bool)>, Int)> = [([(14, True)], 42)]
            when xs is {
                [                     ] -> Void
                [([(_, True)], 42), ..] -> Void
                [([         ],  _), ..] -> Void
            }
        }
        "#,
        "[([(_, False), ..], _), ..]",
    );

    assert_step(
        r#"
        fn foo() {
            let xs : List<(List<(Int, Bool)>, Int)> = [([(14, True)], 42)]
            when xs is {
                [                          ] -> Void
                [([(_, True )    ], 42), ..] -> Void
                [([              ],  _), ..] -> Void
                [([(_, False), ..],  _), ..] -> Void
            }
        }
        "#,
        "[([(_, True), _, ..], _), ..]",
    );

    assert_step(
        r#"
        fn foo() {
            let xs : List<(List<(Int, Bool)>, Int)> = [([(14, True)], 42)]
            when xs is {
                [                             ] -> Void
                [([(_, True )       ], 42), ..] -> Void
                [([                 ],  _), ..] -> Void
                [([(_, False)   , ..],  _), ..] -> Void
                [([(_, True ), _, ..],  _), ..] -> Void
            }
        }
        "#,
        "[([(_, True)], _), ..]",
    );

    let source_code = r#"
        fn foo() {
            let xs : List<(List<(Int, Bool)>, Int)> = [([(14, True)], 42)]
            when xs is {
                [                             ] -> Void
                [([(_, True )       ], 42), ..] -> Void
                [([                 ],  _), ..] -> Void
                [([(_, False)   , ..],  _), ..] -> Void
                [([(_, True ), _, ..],  _), ..] -> Void
                [([(_, True )       ],  _), ..] -> Void
            }
        }
    "#;

    assert!(check(parse(source_code)).is_ok())
}

#[test]
fn exhaustiveness_guard() {
    let source_code = r#"
        fn foo() {
            when [(True, 42)] is {
                [(True,  x), ..] if x == 42 -> Void
                [(False, x), ..] -> Void
                [] -> Void
            }
        }
    "#;

    assert!(matches!(
        check(parse(source_code)),
        Err((
            _,
            Error::NotExhaustivePatternMatch {
                unmatched,
                ..
            }
        )) if unmatched[0] == "[(True, _), ..]"
    ));
}

#[test]
fn expect_sugar_correct_type() {
    let source_code = r#"
        fn foo() {
          expect 1 == 1
          2
        }
    "#;

    assert!(check(parse(source_code)).is_ok())
}

#[test]
fn expect_sugar_incorrect_type() {
    let source_code = r#"
        fn foo() {
          expect 1
          2
        }
    "#;

    assert!(matches!(
        check(parse(source_code)),
        Err((_, Error::CouldNotUnify { .. }))
    ))
}

#[test]
fn logical_op_chain_expressions_should_be_bool() {
    let source_code = r#"
        fn foo() {
          and {
            1 == 1,
            False,
            or {
              2 == 3,
              1
            }
          }
        }
    "#;

    assert!(matches!(
        check(parse(source_code)),
        Err((_, Error::CouldNotUnify { .. }))
    ))
}

#[test]
fn anonymous_function_scoping() {
    let source_code = r#"
        fn reduce(list, f, i) {
          todo
        }

        pub fn foo() {
          let sum =
            reduce(
              [1, 2, 3],
              fn(acc: Int, n: Int) { acc + n },
              0,
            )

          sum + acc
        }
    "#;

    assert!(matches!(
        check(parse(source_code)),
        Err((_, Error::UnknownVariable { name, .. })) if name == "acc"
    ))
}

#[test]
fn anonymous_function_dupicate_args() {
    let source_code = r#"
        fn reduce(list, f, i) {
          todo
        }

        pub fn foo() {
          let sum =
            reduce(
              [1, 2, 3],
              fn(acc: Int, acc: Int) { acc + acc },
              0,
            )

          sum
        }
    "#;

    assert!(matches!(
        check(parse(source_code)),
        Err((_, Error::DuplicateArgument { label, .. })) if label == "acc"
    ))
}

#[test]
fn assignement_last_expr_when() {
    let source_code = r#"
        pub fn foo() {
          let bar = None

          when bar is {
            Some(_) -> {
              let wow = 1
            }
            None -> {
              2
            }
          }
        }
    "#;

    assert!(matches!(
        check(parse(source_code)),
        Err((_, Error::LastExpressionIsAssignment { .. }))
    ))
}

#[test]
fn assignement_last_expr_if_first_branch() {
    let source_code = r#"
        pub fn foo() {
          if True {
            let thing = 1
          } else {
            1
          }
        }
    "#;

    assert!(matches!(
        check(parse(source_code)),
        Err((_, Error::LastExpressionIsAssignment { .. }))
    ))
}

#[test]
fn assignement_last_expr_if_branches() {
    let source_code = r#"
        pub fn foo() {
          if True {
            2
          } else if False {
            let thing = 1
          } else {
            1
          }
        }
    "#;

    assert!(matches!(
        check(parse(source_code)),
        Err((_, Error::LastExpressionIsAssignment { .. }))
    ))
}

#[test]
fn assignement_last_expr_if_final_else() {
    let source_code = r#"
        pub fn foo() {
          if True {
            1
          } else {
            let thing = 1
          }
        }
    "#;

    assert!(matches!(
        check(parse(source_code)),
        Err((_, Error::LastExpressionIsAssignment { .. }))
    ))
}

#[test]
fn if_scoping() {
    let source_code = r#"
        pub fn foo(c) {
          if c {
            let bar = 1
            bar
          } else if !c {
            bar
          } else {
            bar
          }
        }
    "#;

    assert!(matches!(
        check_validator(parse(source_code)),
        Err((_, Error::UnknownVariable { .. }))
    ))
}

#[test]
fn list_pattern_1() {
    let source_code = r#"
        test foo() {
          let xs = [1, 2, 3]
          let [x] = xs
          x == 1
        }
    "#;
    assert!(matches!(
        check(parse(source_code)),
        Err((_, Error::NotExhaustivePatternMatch { .. }))
    ))
}

#[test]
fn list_pattern_2() {
    let source_code = r#"
        test foo() {
          let xs = [1, 2, 3]
          let x = when xs is {
            [x] -> x
          }
          x == 1
        }
    "#;
    assert!(matches!(
        check(parse(source_code)),
        Err((_, Error::NotExhaustivePatternMatch { .. }))
    ))
}

#[test]
fn list_pattern_3() {
    let source_code = r#"
        test foo() {
          let xs = [1, 2, 3]
          let x = when xs is {
            [x] -> x
            [x, ..] -> x
          }
          x == 1
        }
    "#;
    assert!(matches!(
        check(parse(source_code)),
        Err((_, Error::NotExhaustivePatternMatch { .. }))
    ))
}

#[test]
fn list_pattern_4() {
    let source_code = r#"
        test foo() {
          let xs = [1, 2, 3]
          let x = when xs is {
            [] -> 1
            [x] -> x
            [x, ..] if x > 10 -> x
          }
          x == 1
        }
    "#;
    assert!(matches!(
        check(parse(source_code)),
        Err((_, Error::NotExhaustivePatternMatch { .. }))
    ))
}

#[test]
fn list_pattern_5() {
    let source_code = r#"
        test foo() {
          let xs = [1, 2, 3]
          let x = when xs is {
            [] -> 1
            [_, ..] -> 1
          }
          x == 1
        }
    "#;
    assert!(check(parse(source_code)).is_ok())
}

#[test]
fn list_pattern_6() {
    let source_code = r#"
        test foo() {
          let xs = [1, 2, 3]
          let x = when xs is {
            [x, ..] -> 1
            _ -> 1
          }
          x == 1
        }
    "#;
    assert!(check(parse(source_code)).is_ok())
}

#[test]
fn spread_with_positional_constr_args() {
    let source_code = r#"
        type Redeemer {
          First(Int)
          Second
        }

        fn foo(redeemer: Redeemer) {
          when redeemer is {
            First(..) -> True
            Second -> True
          }
        }
    "#;
    assert!(check(parse(source_code)).is_ok())
}

#[test]
fn unnecessary_spread_with_positional_constr_args() {
    let source_code = r#"
        type Redeemer {
          First(Int)
          Second
        }

        fn foo(redeemer: Redeemer) {
          when redeemer is {
            First(x, ..) -> True
            Second -> True
          }
        }
    "#;

    assert!(matches!(
        check(parse(source_code)),
        Err((_, Error::UnnecessarySpreadOperator { .. }))
    ))
}

#[test]
fn trace_strings() {
    let source_code = r#"
        fn bar() {
            @"BAR"
        }

        test foo() {
            let msg1 = @"FOO"
            trace(@"INLINE")
            trace(msg1)
            trace(bar())
            True
        }
    "#;
    assert!(check(parse(source_code)).is_ok())
}

#[test]
fn trace_non_strings() {
    let source_code = r#"
        test foo() {
            trace(14 + 42)
            True
        }
    "#;
    assert!(matches!(
        check(parse(source_code)),
        Err((_, Error::CouldNotUnify { .. }))
    ))
}

#[test]
fn trace_if_false_ok() {
    let source_code = r#"
        fn or_func(a: Bool, b: Bool) {
           (a || b)?
        }

        test foo() {
            or_func(True, False)?
        }

        test bar() {
            let must_be_signed = True
            must_be_signed?
        }
    "#;

    assert!(check(parse(source_code)).is_ok())
}

#[test]
fn backpassing_basic() {
    let source_code = r#"
        fn and_then(opt: Option<a>, then: fn(a) -> Option<b>) -> Option<b> {
          when opt is {
            None -> None
            Some(a) -> then(a)
          }
        }

        fn backpassing(opt_i: Option<Int>, opt_j: Option<Int>) -> Option<Int> {
          let i <- and_then(opt_i)
          let j <- and_then(opt_j)
          Some(i + j)
        }
    "#;

    assert!(check(parse(source_code)).is_ok())
}

#[test]
fn backpassing_expect_simple() {
    let source_code = r#"
        fn and_then(opt: Option<a>, then: fn(a) -> Option<b>) -> Option<b> {
          when opt is {
            None -> None
            Some(a) -> then(a)
          }
        }

        fn backpassing(opt_i: Option<Int>, opt_j: Option<Int>) -> Option<Int> {
          expect 42 <- and_then(opt_i)
          let j <- and_then(opt_j)
          Some(j + 42)
        }
    "#;

    assert!(check(parse(source_code)).is_ok())
}

#[test]
fn backpassing_expect_nested() {
    let source_code = r#"
        fn and_then(opt: Option<a>, then: fn(Option<a>) -> Option<b>) -> Option<b> {
          when opt is {
            None -> None
            Some(a) -> then(Some(a))
          }
        }

        fn backpassing(opt_i: Option<Int>, opt_j: Option<Int>) -> Option<Int> {
          expect Some(i) <- and_then(opt_i)
          expect Some(j) <- and_then(opt_j)
          Some(i + j)
        }
    "#;

    assert!(check(parse(source_code)).is_ok())
}

#[test]
fn backpassing_interleaved_capture() {
    let source_code = r#"
        fn and_then(opt: Option<a>, then: fn(a) -> Option<b>) -> Option<b> {
          when opt is {
            None -> None
            Some(a) -> then(a)
          }
        }

        fn backpassing(opt_i: Option<Int>, opt_j: Option<Int>) -> Option<Int> {
          let f = and_then(opt_i, _)
          let i <- f
          let g = and_then(opt_j, _)
          let j <- g
          Some(i + j)
        }
    "#;

    assert!(check(parse(source_code)).is_ok())
}

#[test]
fn backpassing_patterns() {
    let source_code = r#"
        fn and_then(opt: Option<a>, then: fn(a) -> Option<b>) -> Option<b> {
          when opt is {
            None -> None
            Some(a) -> then(a)
          }
        }

        type Foo {
          foo: Int,
        }

        fn backpassing(opt_i: Option<Foo>, opt_j: Option<Foo>) -> Option<Int> {
          let Foo { foo: i } <- and_then(opt_i)
          let Foo { foo: j } <- and_then(opt_j)
          Some(i + j)
        }
    "#;

    assert!(check(parse(source_code)).is_ok())
}

#[test]
fn backpassing_not_a_function() {
    let source_code = r#"
        fn and_then(opt: Option<a>, then: fn(a) -> Option<b>) -> Option<b> {
          when opt is {
            None -> None
            Some(a) -> then(a)
          }
        }

        fn backpassing(opt_i: Option<Int>, opt_j: Option<Int>) -> Option<Int> {
          let i <- opt_i
          let j <- and_then(opt_j)
          Some(i + j)
        }
    "#;

    assert!(matches!(
        check(parse(source_code)),
        Err((_, Error::NotFn { .. }))
    ))
}

#[test]
fn backpassing_non_exhaustive_pattern() {
    let source_code = r#"
        fn and_then(opt: Option<a>, then: fn(a) -> Option<b>) -> Option<b> {
          when opt is {
            None -> None
            Some(a) -> then(a)
          }
        }

        fn backpassing(opt_i: Option<Int>, opt_j: Option<Int>) -> Option<Int> {
          let 42 <- and_then(opt_i)
          let j <- and_then(opt_j)
          Some(i + j)
        }
    "#;

    assert!(matches!(
        check(parse(source_code)),
        Err((_, Error::NotExhaustivePatternMatch { .. }))
    ))
}

#[test]
fn backpassing_unsaturated_fn() {
    let source_code = r#"
        fn and_then(opt: Option<a>, then: fn(a) -> Option<b>) -> Option<b> {
          when opt is {
            None -> None
            Some(a) -> then(a)
          }
        }

        fn backpassing(opt_i: Option<Int>, opt_j: Option<Int>) -> Option<Int> {
          let i <- and_then
          let j <- and_then(opt_j)
          Some(i + j)
        }
    "#;

    assert!(matches!(
        check(parse(source_code)),
        Err((_, Error::IncorrectFieldsArity { .. }))
    ))
}

#[test]
fn backpassing_expect_type_mismatch() {
    let source_code = r#"
        fn and_then(opt: Option<a>, then: fn(a) -> Option<b>) -> Option<b> {
          when opt is {
            None -> None
            Some(a) -> then(a)
          }
        }

        fn backpassing(opt_i: Option<Int>, opt_j: Option<Int>) -> Option<Int> {
          expect Some(i) <- and_then(opt_i)
          let j <- and_then(opt_j)
          Some(i + j)
        }
    "#;

    assert!(matches!(
        check(parse(source_code)),
        Err((_, Error::CouldNotUnify { .. }))
    ))
}

#[test]
fn backpassing_multi_args() {
    let source_code = r#"
        fn fold(list: List<a>, init: b, then: fn(a, b) -> b) -> b {
          when list is {
            [] -> init
            [x, ..rest] -> fold(rest, then(x, init), then)
          }
        }

        fn backpassing() -> Int {
          let elem, acc <- fold([1, 2, 3], 0)

          elem + acc
        }
    "#;

    assert!(check(parse(source_code)).is_ok())
}

#[test]
fn backpassing_multi_args_expect() {
    let source_code = r#"
        pub type Bar {
          Foo(Int)
          Wow(Int)
        }

        fn fold(list: List<a>, init: b, then: fn(a, b) -> b) -> b {
          when list is {
            [] -> init
            [x, ..rest] -> fold(rest, then(x, init), then)
          }
        }

        pub fn backpassing() -> Bar {
          expect Foo(elem), Wow(acc) <- fold([Foo(1), Foo(2), Foo(3)], Wow(0))

          Wow(elem + acc)
        }
    "#;

    assert!(matches!(check(parse(source_code)), Ok((warnings, _)) if warnings.is_empty()))
}

#[test]
fn backpassing_multi_args_using_equals() {
    let source_code = r#"
        fn fold(list: List<a>, init: b, then: fn(a, b) -> b) -> b {
          when list is {
            [] -> init
            [x, ..rest] -> fold(rest, then(x, init), then)
          }
        }

        fn backpassing() -> Int {
          let elem, acc = fold([1, 2, 3], 0, fn(elem, acc) { elem + acc })

          elem + acc
        }
    "#;

    assert!(matches!(
        check(parse(source_code)),
        Err((_, Error::UnexpectedMultiPatternAssignment { .. }))
    ))
}

#[test]
fn trace_if_false_ko() {
    let source_code = r#"
        fn add(a: Int, b: Int) {
           (a + b)?
        }

        test foo() {
            add(14, 42) == 12
        }

        test bar() {
            let must_be_signed = #"FF00"
            must_be_signed? == #"FF00"
        }
    "#;

    assert!(matches!(
        check(parse(source_code)),
        Err((_, Error::CouldNotUnify { .. }))
    ))
}

#[test]
fn pipe_with_wrong_type() {
    let source_code = r#"
        test foo() {
          True |> bar
        }

        fn bar(n: Int) {
          n - 1
        }
    "#;

    assert!(matches!(
        check(parse(source_code)),
        Err((
            _,
            Error::CouldNotUnify {
                situation: Some(UnifyErrorSituation::PipeTypeMismatch),
                ..
            }
        ))
    ))
}

#[test]
fn pipe_with_wrong_type_and_args() {
    let source_code = r#"
        test foo() {
          True |> bar(False)
        }

        fn bar(n: Int, l: Bool) {
          n - 1
        }
    "#;

    assert!(matches!(
        check(parse(source_code)),
        Err((
            _,
            Error::CouldNotUnify {
                situation: Some(UnifyErrorSituation::PipeTypeMismatch),
                ..
            }
        ))
    ))
}

#[test]
fn pipe_with_right_type_and_wrong_args() {
    let source_code = r#"
        test foo() {
          1 |> bar(1)
        }

        fn bar(n: Int, l: Bool) {
          n - 1
        }
    "#;

    assert!(matches!(
        check(parse(source_code)),
        Err((
            _,
            Error::CouldNotUnify {
                situation: None,
                ..
            }
        ))
    ))
}

#[test]
fn pipe_with_wrong_type_and_full_args() {
    let source_code = r#"
        test foo() {
          True |> bar(False)
        }

        fn bar(l: Bool) -> fn(Int) -> Int {
          fn(n: Int) {
            n - 1
          }
        }
    "#;

    assert!(matches!(
        check(parse(source_code)),
        Err((
            _,
            Error::CouldNotUnify {
                situation: Some(UnifyErrorSituation::PipeTypeMismatch),
                ..
            }
        ))
    ))
}

#[test]
fn fuzzer_ok_basic() {
    let source_code = r#"
        fn int() -> Fuzzer<Int> { todo }

        test prop(n via int()) { todo }
    "#;

    assert!(check(parse(source_code)).is_ok());
}

#[test]
fn fuzzer_ok_explicit() {
    let source_code = r#"
        fn int(prng: PRNG) -> Option<(PRNG, Int)> { todo }

        test prop(n via int) { todo }
    "#;

    assert!(check(parse(source_code)).is_ok());
}

#[test]
fn fuzzer_ok_list() {
    let source_code = r#"
        fn int() -> Fuzzer<Int> { todo }
        fn list(a: Fuzzer<a>) -> Fuzzer<List<a>> { todo }

        test prop(xs via list(int())) { todo }
    "#;

    assert!(check(parse(source_code)).is_ok());
}

#[test]
fn fuzzer_err_unbound() {
    let source_code = r#"
        fn any() -> Fuzzer<a> { todo }
        fn list(a: Fuzzer<a>) -> Fuzzer<List<a>> { todo }

        test prop(xs via list(any())) { todo }
    "#;

    assert!(matches!(
        check(parse(source_code)),
        Err((_, Error::GenericLeftAtBoundary { .. }))
    ))
}

#[test]
fn fuzzer_err_unify_1() {
    let source_code = r#"
        test prop(xs via Void) { todo }
    "#;

    assert!(matches!(
        check(parse(source_code)),
        Err((
            _,
            Error::CouldNotUnify {
                situation: None,
                ..
            }
        ))
    ))
}

#[test]
fn fuzzer_err_unify_2() {
    let source_code = r#"
        fn any() -> Fuzzer<a> { todo }
        test prop(xs via any) { todo }
    "#;

    assert!(matches!(
        check(parse(source_code)),
        Err((
            _,
            Error::CouldNotUnify {
                situation: None,
                ..
            }
        ))
    ))
}

#[test]
fn fuzzer_err_unify_3() {
    let source_code = r#"
        fn list(a: Fuzzer<a>) -> Fuzzer<List<a>> { todo }
        fn int() -> Fuzzer<Int> { todo }

        test prop(xs: Int via list(int())) { todo }
    "#;

    assert!(matches!(
        check(parse(source_code)),
        Err((
            _,
            Error::CouldNotUnify {
                situation: Some(UnifyErrorSituation::FuzzerAnnotationMismatch),
                ..
            }
        ))
    ))
}

#[test]
fn utf8_hex_literal_warning() {
    let source_code = r#"
        pub const policy_id = "f43a62fdc3965df486de8a0d32fe800963589c41b38946602a0dc535"
    "#;

    let (warnings, _) = check(parse(source_code)).unwrap();

    assert!(matches!(
        warnings[0],
        Warning::Utf8ByteArrayIsValidHexString { .. }
    ))
}

#[test]
fn discarded_let_bindings() {
    let source_code = r#"
        fn foo() {
            let result = when 42 is {
                1 -> {
                    let unused = "foo"
                    Void
                }
                _ -> {
                    Void
                }
            }

            let _ = "foo"

            result
        }
    "#;

    let (warnings, ast) = check(parse(source_code)).unwrap();

    assert!(matches!(warnings[0], Warning::UnusedVariable { ref name, .. } if name == "unused"));
    assert!(matches!(warnings[1], Warning::DiscardedLetAssignment { ref name, .. } if name == "_"));

    // Controls that unused let-bindings have been erased from the transformed AST.
    match ast.definitions.first() {
        Some(Definition::Fn(def)) => match &def.body {
            TypedExpr::Sequence { expressions, .. } => {
                assert_eq!(expressions.len(), 2);
                assert!(
                    matches!(expressions[1], TypedExpr::Var { .. }),
                    "last expression isn't return variable"
                );
                match &expressions[0] {
                    TypedExpr::Assignment { value, .. } => match **value {
                        TypedExpr::When { ref clauses, .. } => {
                            assert!(
                                matches!(clauses[0].then, TypedExpr::Sequence { ref expressions, ..} if expressions.len() == 1)
                            )
                        }
                        _ => unreachable!("first expression isn't when/is"),
                    },
                    _ => unreachable!("first expression isn't assignment"),
                }
            }
            _ => unreachable!("body isn't a Sequence"),
        },
        _ => unreachable!("ast isn't a Fn"),
    }
}
