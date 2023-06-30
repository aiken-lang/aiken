use crate::{ast, parser};

macro_rules! assert_parse {
    ($code:expr) => {
        let (module, _) =
            parser::module(indoc::indoc!{ $code }, ast::ModuleKind::Validator).expect("Failed to parse code");

        insta::with_settings!({
            description => concat!("Code:\n\n", indoc::indoc! { $code }),
            prepend_module_to_snapshot => false,
            omit_expression => true
        }, {
            insta::assert_debug_snapshot!(module);
        });
    };
}

#[test]
fn windows_newline() {
    assert_parse!("use aiken/list\r\n");
}

#[test]
fn can_handle_comments_at_end_of_file() {
    assert_parse!(
        r#"
        use aiken

        // some comment
        // more comments"#
    );
}

#[test]
fn type_annotation_with_module_prefix() {
    assert_parse!(
        r#"
        use aiken

        pub fn go() -> aiken.Option<Int> {
          False
        }
        "#
    );
}

#[test]
fn test_fail() {
    assert_parse!(
        r#"
        !test invalid_inputs() {
          expect True = False

          False
        }
        "#
    );
}

#[test]
fn validator() {
    assert_parse!(
        r#"
        validator {
          fn foo(datum, rdmr, ctx) {
            True
          }
        }
        "#
    );
}

#[test]
fn double_validator() {
    assert_parse!(
        r#"
        validator {
          fn foo(datum, rdmr, ctx) {
            True
          }

          fn bar(rdmr, ctx) {
            True
          }
        }
        "#
    );
}

#[test]
fn import() {
    assert_parse!(
        r#"
        use std/list
        "#
    );
}

#[test]
fn unqualified_imports() {
    assert_parse!(
        r#"
        use std/address.{Address as A, thing as w}
        "#
    );
}

#[test]
fn import_alias() {
    assert_parse!(
        r#"
        use std/tx as t
        "#
    );
}

#[test]
fn custom_type() {
    assert_parse!(
        r#"
        type Option<a> {
          Some(a, Int)
          None
          Wow { name: Int, age: Int }
        }
        "#
    );
}

#[test]
fn opaque_type() {
    assert_parse!(
        r#"
        pub opaque type User {
          name: _w
        }
        "#
    );
}

#[test]
fn type_alias() {
    assert_parse!(
        r#"
        type Thing = Option<Int>
        "#
    );
}

#[test]
fn pub_type_alias() {
    assert_parse!(
        r#"
        pub type Me = Option<String>
        "#
    );
}

#[test]
fn empty_function() {
    assert_parse!(
        r#"
        pub fn run() {}
        "#
    );
}

#[test]
fn expect() {
    assert_parse!(
        r#"
        pub fn run() {
            expect Some(x) = something.field
            x.other_field
        }
        "#
    );
}

#[test]
fn plus_binop() {
    assert_parse!(
        r#"
        pub fn add_one(a) -> Int {
          a + 1
        }
        "#
    );
}

#[test]
fn pipeline() {
    assert_parse!(
        r#"
        pub fn thing(thing a: Int) {
          a + 2
          |> add_one
          |> add_one
        }
        "#
    );
}

#[test]
fn if_expression() {
    assert_parse!(
        r#"
        fn ifs() {
          if True {
            1 + 1
          } else if a < 4 {
            5
          } else if a || b {
            6
          } else {
            3
          }
        }
    "#
    );
}

#[test]
fn let_bindings() {
    assert_parse!(
        r#"
        pub fn wow(a: Int) {
          let x =
            a + 2
            |> add_one
            |> add_one

          let thing = [ 1, 2, a ]

          let idk = thing

          y
        }
        "#
    );
}

#[test]
fn block() {
    assert_parse!(
        r#"
        pub fn wow2(a: Int){
          let b = {
            let x = 4

            x + 5
          }

          b
        }
        "#
    );
}

#[test]
fn when() {
    assert_parse!(
        r#"
        pub fn wow2(a: Int){
          when a is {
            2 -> 3
            1 | 4 | 5 -> {
              let amazing = 5
              amazing
            }
            3 -> 9
            _ -> 4
          }
        }
        "#
    );
}

#[test]
fn anonymous_function() {
    assert_parse!(
        r#"
        pub fn such() -> Int {
          let add_one = fn (a: Int) -> Int { a + 1 }

          2 |> add_one
        }
        "#
    );
}

#[test]
fn field_access() {
    assert_parse!(
        r#"
        fn name(user: User) {
          user.name
        }
        "#
    );
}

#[test]
fn call() {
    assert_parse!(
        r#"
        fn calls() {
          let x = add_one(3)

          let map_add_x = list.map(_, fn (y) { x + y })

          map_add_x([ 1, 2, 3 ])
        }
        "#
    );
}

#[test]
fn record_update() {
    assert_parse!(
        r#"
        fn update_name(user: User, name: ByteArray) -> User {
          User { ..user, name: "Aiken", age }
        }
        "#
    );
}

#[test]
fn record_create_labeled() {
    assert_parse!(
        r#"
        fn create() {
          User { name: "Aiken", age, thing: 2 }
        }
       "#
    );
}

#[test]
fn record_create_labeled_with_field_access() {
    assert_parse!(
        r#"
        fn create() {
          some_module.User { name: "Aiken", age, thing: 2 }
        }
        "#
    );
}

#[test]
fn cargo_create_unlabeled() {
    assert_parse!(
        r#"
        fn create() {
          some_module.Thing(1, a)
        }
        "#
    );
}

#[test]
fn parse_tuple() {
    assert_parse!(
        r#"
        fn foo() {
          let tuple = (1, 2, 3, 4)
          tuple.1st + tuple.2nd + tuple.3rd + tuple.4th
        }
        "#
    );
}

#[test]
fn parse_tuple2() {
    assert_parse!(
        r#"
        fn foo() {
          let a = foo(14)
          (a, 42)
        }
        "#
    );
}

#[test]
fn plain_bytearray_literals() {
    assert_parse!(
        r#"
        pub const my_policy_id = #[0, 170, 255]
        "#
    );
}

#[test]
fn base16_bytearray_literals() {
    assert_parse!(
        r#"
        pub const my_policy_id = #"00aaff"

        pub fn foo() {
            my_policy_id == #"00aaff"
        }
        "#
    );
}

#[test]
fn function_def() {
    assert_parse!(
        r#"
        fn foo() {}
        "#
    );
}

#[test]
fn function_invoke() {
    assert_parse!(
        r#"
        fn foo() {
            let a = bar(42)
        }
        "#
    );
}

#[test]
fn function_ambiguous_sequence() {
    assert_parse!(
        r#"
        fn foo_1() {
          let a = bar
          (40)
        }

        fn foo_2() {
          let a = bar
          {40}
        }

        fn foo_3() {
          let a = (40+2)
        }

        fn foo_4() {
          let a = bar(42)
          (a + 14) * 42
        }
        "#
    );
}

#[test]
fn tuple_type_alias() {
    assert_parse!(
        r#"
        type RoyaltyToken = (PolicyId, AssetName)
        "#
    );
}

#[test]
fn int_parsing_hex_bytes() {
    assert_parse!(
        r#"
        fn foo() {
          #[ 0x01, 0xa2, 0x03 ]
        }
        "#
    );
}

#[test]
fn test_parsing_numeric_underscore() {
    assert_parse!(
        r#"
        fn foo() {
          let i = 1_234_567
          let j = 1_000_000
          let k = -10_000
        }
        "#
    );
}

#[test]
fn first_class_binop() {
    assert_parse!(
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
fn parse_unicode_offset_1() {
    assert_module!(
        r#"
        fn foo() {
          let x = "★"
          x
        }
        "#
    );
}

#[test]
fn parse_unicode_offset_2() {
    assert_module!(
        r#"
        fn foo() {
          let x = "*"
          x
        }
        "#
    );
}
