use crate::assert_format;

#[test]
fn format_comment_at_end_of_file() {
    assert_format!(
        r#"
      type Foo =
        Int

      //"#
    );
}

#[test]
fn format_simple_module() {
    assert_format!(
        r#"
      fn smth() { let a = foo.Foo { bar: 1 } }
    "#
    );
}

#[test]
fn format_g1_element_constant() {
    assert_format!(
        r#"
        pub const point = #<Bls12_381, G1>"950dfd33da2682260c76038dfb8bad6e84ae9d599a3c151815945ac1e6ef6b1027cd917f3907479d20d636ce437a41f5"
        "#
    );
}

#[test]
fn format_g2_element_constant() {
    assert_format!(
        r#"
        pub const point = #<Bls12_381, G2>"b0629fa1158c2d23a10413fe91d381a84d25e31d041cd0377d25828498fd02011b35893938ced97535395e4815201e67108bcd4665e0db25d602d76fa791fab706c54abf5e1a9e44b4ac1e6badf3d2ac0328f5e30be341677c8bac5dda7682f1"
        "#
    );
}

#[test]
fn format_logical_op_chain() {
    assert_format!(
        r#"
      fn smth() { and { foo, bar, or { bar, foo }} }
    "#
    );
}

#[test]
fn format_if() {
    assert_format!(
        r#"
        pub fn foo(a) {
          if a { 14 } else { 42 }
          }

        pub fn bar(xs) {
            list.map(xs, fn (x) { if x > 0 { "foo" } else { "bar" } })
        }
    "#
    );
}

#[test]
fn format_validator() {
    assert_format!(
        r#"
      validator ( ) {
      // What is the purpose of life

      fn foo (d: Datum, r: Redeemer, ctx: ScriptContext) -> Bool {
      True
      }
      }

      // What?
      validator {
        /// Some documentation for foo
        fn foo() {
          Void
        }

        // I am lost
      }
    "#
    );
}

#[test]
fn format_double_validator() {
    assert_format!(
        r#"
        validator ( param1 : ByteArray ) {
        fn foo (d: Datum, r: Redeemer, ctx: ScriptContext) -> Bool {
        True
        }
        /// This is bar
    fn bar(r: Redeemer, ctx    : ScriptContext  )   ->   Bool { True }
        }
    "#
    );
}

#[test]
fn format_when() {
    assert_format!(
        r#"
        pub fn foo( a) {
          when  a   is{
            True  -> {

              bar()

              14

            }
            False ->

             42

             }
         }
    "#
    );
}

#[test]
fn format_nested_if() {
    assert_format!(
        r#"
        pub fn foo(n) {
          if n > 0 {
            if n > 1 { if n > 2 { "foo" } else { "foo" } } else { "bar" }
          } else {
            if n < -1 { "baz" } else { "biz" }
          }
        }
    "#
    );
}

#[test]
fn format_nested_when_if() {
    assert_format!(
        r#"
        pub fn drop(xs: List<a>, n: Int) -> List<a> {
          if n <= 0 {
            xs
          } else {
            when xs is {
            [] ->
              []
            [_x, ..rest] ->
              drop(rest, n - 1)
          }
          }
        }
    "#
    );
}

#[test]
fn format_nested_when() {
    assert_format!(
        r#"
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
    "#
    );
}

#[test]
fn format_else_if() {
    assert_format!(
        r#"
        pub fn foo(xs: List<a>, n: Int) -> List<a> {
          if n <= 0 {
            xs
          } else if n <= 10 {
            xs
          } else {
            xs
          }
        }
    "#
    );
}

#[test]
fn format_imports() {
    // TODO: Fix this case, this is behaving weirdly, not keeping the order for the comments and
    // imports.
    assert_format!(
        r#"
        use aiken/list
        // foo
        use aiken/bytearray
        use aiken/transaction/certificate
        // bar
        use aiken/transaction
        use aiken/transaction/value
    "#
    );
}

#[test]
fn format_negate() {
    assert_format!(
        r#"
        fn foo() {
            - 42
        }
    "#
    );
}

#[test]
fn format_block_arithmetic_expr() {
    assert_format!(
        r#"
        fn foo() {
          ( 14 + 42 ) * 1337
        }

        fn bar() {
          { 14 + 42 } * 1337
        }
    "#
    );
}

#[test]
fn format_block_logical_expr() {
    assert_format!(
        r#"
        fn foo() {
          !(a && b)
        }

        fn bar() {
          (a || b) && (c || d)
        }

        fn baz() {
          a || (b && c) || d
        }
    "#
    );
}

#[test]
fn format_nested_function_calls() {
    assert_format!(
        r#"
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
              _ -> fail "expected inline datum"
            },
          ]
          |> list.and_func
        }
    "#
    );
}

#[test]
fn format_trace_todo_error() {
    assert_format!(
        r#"
        fn foo_1() {
          todo
        }

        fn foo_2() {
          todo "my custom message"
        }

        fn foo_3() {
          when x is {
            Foo -> True
            _ -> fail
          }
        }

        fn foo_4() {
          if 14 == 42 {
            fail "I don't think so"
          } else {
            trace "been there"
            True
          }
        }
    "#
    );
}

#[test]
fn format_trace_if_false() {
    assert_format!(
        r#"
        fn foo() {
          my_expression?
        }

        fn bar() {
          (True && False)? || foo()?
        }
    "#
    );
}

#[test]
fn format_newline_comments() {
    assert_format!(
        r#"
        // My comment
        //
        // has a newline.
        fn foo() {
          True
        }

        // My comments

        // can live apart
        fn bar() {
          True
        }
    "#
    );
}

#[test]
fn format_newline_doc_comments() {
    assert_format!(
        r#"
        /// My doc comment
        ///
        /// has a newline.
        fn foo() {
          True
        }

        /// My doc comments

        /// cannot be separated
        fn bar() {
          True
        }
    "#
    );
}

#[test]
fn format_newline_module_comments() {
    assert_format!(
        r#"
        //// My module comment
        ////
        //// has a newline.

        fn foo() {
          True
        }

        //// My module comments

        //// cannot be separated
    "#
    );
}

#[test]
fn format_bytearray_literals() {
    assert_format!(
        r#"
        const foo_const_array = #[102, 111, 111]

        const foo_const_hex = #"666f6f"

        const foo_const_utf8 = "foo"

        fn foo() {
          let foo_const_array = #[102, 111, 111]

          let foo_const_hex = #"666f6f"

          let foo_const_utf8 = "foo"
        }
    "#
    );
}

#[test]
fn escaped_utf8() {
    assert_format!(
        r#"
        const escaped_1 = "\"my_string\""
        const escaped_2 = "foo\nbar"
        const escaped_3 = "foo\rbar"
        const escaped_4 = "foo\tbar"
        const escaped_5 = "1/2"
        const escaped_6 = "1//2"
        "#
    );
}

#[test]
fn format_string_literal() {
    assert_format!(
        r#"
        const foo_const: String = @"foo"

        fn foo() {
          let foo_var: String = @"foo"
        }
    "#
    );
}

#[test]
fn format_unicode() {
    assert_format!(
        r#"
        /// ∞ ★ ♩ ♫ ✓
        fn foo() {
          trace @"∀💩"
          Void
        }
    "#
    );
}

#[test]
fn format_preserve_pipe() {
    assert_format!(
        r#"
        fn foo() {
          a |> b |> c |> d
        }

        fn foo() {
          a
            // Foo
            |> b// Some comments
            |> c
            |> d
        }

        fn baz() {
          // Commented
          however |> it_automatically_breaks |> into_multiple_lines |> anytime_when |> it_is_too_long // What?
        }
    "#
    );
}

#[test]
fn format_weird_comments() {
    assert_format!(
        r#"
        // A

        /// B

        /// C
        fn foo() {
          todo
        }

        // E

        /// F

        // G
        fn bar() {
          todo
        }
    "#
    );
}

#[test]
fn format_trace_callback() {
    assert_format!(
        r#"
      fn foo() {
        list.any([], fn (e) { trace @"foo"
          e
        })
      }
    "#
    );
}

#[test]
fn format_pipe_fn() {
    assert_format!(
        r#"
      fn foo() {
        outputs
          |> list.any(
          fn(output) { value.quantity_of(output.value, policy_id, asset_name) == 1 },
        )
      }
    "#
    );
}

#[test]
fn format_match_record() {
    assert_format!(
        r#"
      fn foo() {
        when bar is {
          Bar { a, b, c } -> Void
          Bar { a, .. } -> Void
          Bar { .. } -> Void
          Bar(a, b, c) -> Void
          Bar(..) -> Void
        }
      }
    "#
    );
}

#[test]
fn format_fail() {
    assert_format!(
        r#"
        test foo() fail {
          expect Some(a) = bar

          a
        }
        "#
    );
}

#[test]
fn format_pipes_and_expressions() {
    assert_format!(
        r#"
        test fmt() {
          (x == y) && ((z |> length()) == x)
        }
    "#
    );
}

#[test]
fn format_hex_and_numeric_underscore() {
    assert_format!(
        r#"
        fn foo() {
          let a = 1_000_000 + 1_423 + 10393841
          let b = 0xa4 - 0xcd
          let c = #[0xfd, 0x12, 0x00, 0x1b, 0x0a, 0x90]
          let d = -100_000
        }
    "#
    );
}

#[test]
fn format_first_class_binop() {
    assert_format!(
        r#"
        fn foo() {
          compare_with(a, >, b)
          compare_with(a, >=, b)
          compare_with(a, <, b)
          compare_with(a, <=, b)
          compare_with(a, ==, b)
          compare_with(a, !=, b)
          combine_with(a, &&, b)
          combine_with(a, ||, b)
          compute_with(a, +, b)
          compute_with(a, -, b)
          compute_with(a, /, b)
          compute_with(a, *, b)
          compute_with(a, %, b)
        }
    "#
    );
}

#[test]
fn format_int_uint() {
    assert_format!(
        r#"
        const i = 42

        const j = -14

        fn foo() {
          when y is {
            14 -> -14
            -42 -> 42
          }
        }
    "#
    );
}

#[test]
fn fail_expr() {
    assert_format!(
        r#"
        fn foo() {
          fail some_var
        }
        "#
    );
}
