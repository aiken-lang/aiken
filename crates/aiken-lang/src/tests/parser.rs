use crate::{ast, parser};

macro_rules! assert_parse {
    ($name:ident, $code:expr) => {
        #[test]
        fn $name() {
            let (module, _) =
                parser::module(indoc::indoc!{ $code }, ast::ModuleKind::Validator).expect("Failed to parse code");

            insta::with_settings!({
                info => &stringify!($name),
                description => $code,
                omit_expression => true
            }, {
                insta::assert_debug_snapshot!(module);
            });
        }
    };
}

assert_parse!(windows_newline, "use aiken/list\r\n");
assert_parse!(
    can_handle_comments_at_end_of_file,
    r#"
       use aiken

       // some comment
       // more comments"#
);
assert_parse!(
    type_annotation_with_module_prefix,
    r#"
       use aiken

       pub fn go() -> aiken.Option<Int> {
         False
       }
    "#
);
assert_parse!(
    test_fail,
    r#"
       !test invalid_inputs() {
         expect True = False

         False
       }
    "#
);
assert_parse!(
    validator,
    r#"
        validator {
          fn foo(datum, rdmr, ctx) {
            True
          }
        }
    "#
);
assert_parse!(
    double_validator,
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
assert_parse!(
    import,
    r#"
        use std/list
    "#
);
assert_parse!(
    unqualified_imports,
    r#"
        use std/address.{Address as A, thing as w}
    "#
);
assert_parse!(
    import_alias,
    r#"
        use std/tx as t
    "#
);
assert_parse!(
    custom_type,
    r#"
        type Option<a> {
          Some(a, Int)
          None
          Wow { name: Int, age: Int }
        }
    "#
);
assert_parse!(
    opaque_type,
    r#"
        pub opaque type User {
          name: _w
        }
    "#
);
assert_parse!(
    type_alias,
    r#"
        type Thing = Option<Int>
    "#
);
assert_parse!(
    pub_type_alias,
    r#"
        pub type Me = Option<String>
    "#
);
assert_parse!(
    empty_function,
    r#"
        pub fn run() {}
    "#
);
assert_parse!(
    expect,
    r#"
        pub fn run() {
            expect Some(x) = something.field
            x.other_field
        }
    "#
);
assert_parse!(
    plus_binop,
    r#"
        pub fn add_one(a) -> Int {
          a + 1
        }
    "#
);
assert_parse!(
    pipeline,
    r#"
        pub fn thing(thing a: Int) {
          a + 2
          |> add_one
          |> add_one
        }
    "#
);
assert_parse!(
    if_expression,
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
assert_parse!(
    let_bindings,
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
assert_parse!(
    block,
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
assert_parse!(
    when,
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
assert_parse!(
    anonymous_function,
    r#"
        pub fn such() -> Int {
          let add_one = fn (a: Int) -> Int { a + 1 }

          2 |> add_one
        }
    "#
);
assert_parse!(
    field_access,
    r#"
        fn name(user: User) {
          user.name
        }
    "#
);
assert_parse!(
    call,
    r#"
        fn calls() {
          let x = add_one(3)

          let map_add_x = list.map(_, fn (y) { x + y })

          map_add_x([ 1, 2, 3 ])
        }
    "#
);
assert_parse!(
    record_update,
    r#"
        fn update_name(user: User, name: ByteArray) -> User {
          User { ..user, name: "Aiken", age }
        }
    "#
);
assert_parse!(
    record_create_labeled,
    r#"
        fn create() {
          User { name: "Aiken", age, thing: 2 }
        }
    "#
);
assert_parse!(
    record_create_labeled_with_field_access,
    r#"
        fn create() {
          some_module.User { name: "Aiken", age, thing: 2 }
        }
    "#
);
assert_parse!(
    cargo_create_unlabeled,
    r#"
        fn create() {
          some_module.Thing(1, a)
        }
    "#
);
assert_parse!(
    parse_tuple,
    r#"
        fn foo() {
          let tuple = (1, 2, 3, 4)
          tuple.1st + tuple.2nd + tuple.3rd + tuple.4th
        }
    "#
);
assert_parse!(
    parse_tuple2,
    r#"
        fn foo() {
          let a = foo(14)
          (a, 42)
        }
    "#
);
assert_parse!(
    plain_bytearray_literals,
    r#"
        pub const my_policy_id = #[0, 170, 255]
    "#
);
assert_parse!(
    base16_bytearray_literals,
    r#"
        pub const my_policy_id = #"00aaff"

        pub fn foo() {
            my_policy_id == #"00aaff"
        }
    "#
);
assert_parse!(
    function_def,
    r#"
        fn foo() {}
    "#
);
assert_parse!(
    function_invoke,
    r#"
        fn foo() {
            let a = bar(42)
        }
    "#
);
assert_parse!(
    function_ambiguous_sequence,
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
assert_parse!(
    tuple_type_alias,
    r#"
          type RoyaltyToken = (PolicyId, AssetName)
    "#
);
assert_parse!(
    int_parsing_hex_bytes,
    r#"
          fn foo() {
            #[ 0x01, 0xa2, 0x03 ]
          }
    "#
);
assert_parse!(
    test_parsing_numeric_underscore,
    r#"
          fn foo() {
            let i = 1_234_567
            let j = 1_000_000
            let k = -10_000
        }
    "#
);
assert_parse!(
    first_class_binop,
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
