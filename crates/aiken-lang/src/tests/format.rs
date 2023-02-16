use crate::{ast::ModuleKind, format, parser};
use indoc::indoc;
use pretty_assertions::assert_eq;

fn assert_fmt(src: &str, expected: &str) {
    let (module, extra) = parser::module(src, ModuleKind::Lib).unwrap();
    let mut out = String::new();
    format::pretty(&mut out, module, extra, src);

    // Output is what we expect
    assert_eq!(out, expected);

    // Formatting is idempotent
    let (module2, extra2) = parser::module(&out, ModuleKind::Lib).unwrap();
    let mut out2 = String::new();
    format::pretty(&mut out2, module2, extra2, &out);
    assert_eq!(out, out2, "formatting isn't idempotent");
}

#[test]
fn test_format_if() {
    let src = indoc! {r#"
        pub fn foo(a) {
          if a { 14 } else { 42 }
          }

        pub fn bar(xs) {
            list.map(xs, fn (x) { if x > 0 { "foo" } else { "bar" } })
        }
    "#};

    let expected = indoc! {r#"
        pub fn foo(a) {
          if a {
            14
          } else {
            42
          }
        }

        pub fn bar(xs) {
          list.map(
            xs,
            fn(x) {
              if x > 0 {
                "foo"
              } else {
                "bar"
              }
            },
          )
        }
    "#};

    assert_fmt(src, expected)
}

#[test]
fn test_format_validator() {
    let src = indoc! {r#"
      validator foo ( ) {
      fn(d: Datum, r: Redeemer, ctx: ScriptContext) -> Bool {
      True
      }
      }
    "#};

    let expected = indoc! {r#"
      validator foo {
        fn(d: Datum, r: Redeemer, ctx: ScriptContext) -> Bool {
          True
        }
      }
    "#};

    assert_fmt(src, expected)
}

#[test]
fn test_format_when() {
    let src = indoc! {r#"
        pub fn foo( a) {
          when  a   is{
            True  -> 14
            False ->
             42}
         }
    "#};

    let expected = indoc! {r#"
        pub fn foo(a) {
          when a is {
            True -> 14
            False -> 42
          }
        }
    "#};

    assert_fmt(src, expected)
}

#[test]
fn test_format_nested_if() {
    let src = indoc! {r#"
        pub fn foo(n) {
          if n > 0 {
            if n > 1 {
              if n > 2 {
                "foo"
              } else {
                "foo"
              }
            } else {
              "bar"
            }
          } else {
            if n < -1 {
              "baz"
            } else {
              "biz"
            }
          }
        }
    "#};

    assert_fmt(src, src)
}

#[test]
fn test_format_nested_when_if() {
    let src = indoc! {r#"
        pub fn drop(xs: List<a>, n: Int) -> List<a> {
          if n <= 0 {
            xs
          } else {
            when xs is {
            [] -> []
            [_x, ..rest] -> drop(rest, n - 1)
          }
          }
        }
    "#};

    let expected = indoc! {r#"
        pub fn drop(xs: List<a>, n: Int) -> List<a> {
          if n <= 0 {
            xs
          } else {
            when xs is {
              [] -> []
              [_x, ..rest] -> drop(rest, n - 1)
            }
          }
        }
    "#};

    assert_fmt(src, expected)
}

#[test]
fn test_format_nested_when() {
    let src = indoc! {r#"
        fn foo() {
          when a is {
            None -> "foo"
            Some(b) -> when b is {
              None -> "foo"
              Some(c) -> when c is {
                None -> "foo"
                Some(_) -> "foo"
              }
            }
          }
        }
    "#};

    let expected = indoc! {r#"
        fn foo() {
          when a is {
            None -> "foo"
            Some(b) ->
              when b is {
                None -> "foo"
                Some(c) ->
                  when c is {
                    None -> "foo"
                    Some(_) -> "foo"
                  }
              }
          }
        }
    "#};

    assert_fmt(src, expected)
}

#[test]
fn test_format_else_if() {
    let src = indoc! {r#"
        pub fn foo(xs: List<a>, n: Int) -> List<a> {
          if n <= 0 {
            xs
          } else if n <= 10 {
            xs
          } else {
            xs
          }
        }
    "#};

    assert_fmt(src, src)
}

#[test]
fn test_format_imports() {
    let src = indoc! {r#"
        use aiken/list
        // foo
        use aiken/bytearray
        use aiken/transaction/certificate
        // bar
        use aiken/transaction
        use aiken/transaction/value
    "#};

    let expected = indoc! {r#"
        // foo
        use aiken/bytearray
        use aiken/list
        // bar
        use aiken/transaction
        use aiken/transaction/certificate
        use aiken/transaction/value
    "#};

    assert_fmt(src, expected)
}

#[test]
fn test_negate() {
    let src = indoc! {r#"
        fn foo() {
            - 42
        }
    "#};

    let expected = indoc! {r#"
        fn foo() {
          -42
        }
    "#};

    assert_fmt(src, expected)
}

#[test]
fn test_block_arithmetic_expr() {
    let src = indoc! {r#"
        fn foo() {
          ( 14 + 42 ) * 1337
        }

        fn bar() {
          { 14 + 42 } * 1337
        }
    "#};

    let expected = indoc! {r#"
        fn foo() {
          ( 14 + 42 ) * 1337
        }

        fn bar() {
          ( 14 + 42 ) * 1337
        }
    "#};

    assert_fmt(src, expected);
}

#[test]
fn test_block_logical_expr() {
    let src = indoc! {r#"
        fn foo() {
          !(a && b)
        }

        fn bar() {
          (a || b) && (c || d)
        }

        fn baz() {
          a || (b && c) || d
        }
    "#};

    let expected = indoc! {r#"
        fn foo() {
          !(a && b)
        }

        fn bar() {
          ( a || b ) && ( c || d )
        }

        fn baz() {
          a || b && c || d
        }
    "#};

    assert_fmt(src, expected);
}

#[test]
fn test_format_bytearray_literals() {
    let src = indoc! {r#"
        const foo = #"ff00"
        const bar = #[0, 255]
    "#};

    let expected = indoc! { r#"
        const foo = #"ff00"

        const bar = #"00ff"
    "#};

    assert_fmt(src, expected);
}

#[test]
fn test_nested_function_calls() {
    let src = indoc! {r#"
        fn foo(output) {
          [
            output.address.stake_credential == Some(
            Inline(
            VerificationKeyCredential(
              #"66666666666666666666666666666666666666666666666666666666",
            ))
            )
            ,
            when output.datum is {
              InlineDatum(_) -> True
              _ -> error "expected inline datum"
            },
          ]
          |> list.and
        }
    "#};

    let expected = indoc! {r#"
        fn foo(output) {
          [
            output.address.stake_credential == Some(
              Inline(
                VerificationKeyCredential(
                  #"66666666666666666666666666666666666666666666666666666666",
                ),
              ),
            ),
            when output.datum is {
              InlineDatum(_) -> True
              _ -> error "expected inline datum"
            },
          ]
          |> list.and
        }
    "#};

    assert_fmt(src, expected);
}

#[test]
fn format_trace_todo_error() {
    let src = indoc! {r#"
        fn foo_1() {
          todo
        }

        fn foo_2() {
          todo "my custom message"
        }

        fn foo_3() {
          when x is {
            Foo -> True
            _ -> error
          }
        }

        fn foo_4() {
          if 14 == 42 {
            error "I don't think so"
          } else {
            trace "been there"
            True
          }
        }
    "#};

    assert_fmt(src, src);
}
