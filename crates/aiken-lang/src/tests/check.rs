use crate::{
    ast::{Definition, ModuleKind, Tracing, TypedModule, UntypedModule},
    builtins,
    expr::TypedExpr,
    parser,
    tipo::error::{Error, Warning},
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
        Tracing::KeepTraces,
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
fn expect_sugar_correct_type() {
    let source_code = r#"
        fn foo() {
          expect 1 == 1
          2
        }
    "#;

    assert!(matches!(check(parse(source_code)), Ok(_)))
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
        fn or(a: Bool, b: Bool) {
           (a || b)?
        }

        test foo() {
            or(True, False)?
        }

        test bar() {
            let must_be_signed = True
            must_be_signed?
        }
    "#;

    assert!(check(parse(source_code)).is_ok())
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
    assert!(matches!(warnings[1], Warning::UnusedVariable { ref name, .. } if name == "_"));

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
