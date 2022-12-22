use crate::{ast::ModuleKind, format, parser};
use indoc::indoc;
use pretty_assertions::assert_eq;

fn assert_fmt(src: &str, expected: &str) {
    let (module, extra) = parser::module(src, ModuleKind::Lib).unwrap();
    let mut out = String::new();
    format::pretty(&mut out, module.clone(), extra.clone(), src);

    // Output is what we expect
    assert_eq!(out, expected);

    // Formatting is idempotent
    let mut out2 = String::new();
    format::pretty(&mut out2, module, extra, &out);
    assert_eq!(out, out2);
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
