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
    assert!(out == out2, "formatting isn't idempotent");
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
fn test_block_expr() {
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
