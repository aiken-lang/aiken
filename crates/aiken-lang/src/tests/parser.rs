use crate::assert_module;

#[test]
fn custom_type() {
    assert_module!(
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
    assert_module!(
        r#"
        pub opaque type User {
          name: _w
        }
        "#
    );
}

#[test]
fn expect() {
    assert_module!(
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
    assert_module!(
        r#"
        pub fn add_one(a) -> Int {
          a + 1
        }
        "#
    );
}

#[test]
fn pipeline() {
    assert_module!(
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
fn block() {
    assert_module!(
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
    assert_module!(
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
fn field_access() {
    assert_module!(
        r#"
        fn name(user: User) {
          user.name
        }
        "#
    );
}

#[test]
fn call() {
    assert_module!(
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
fn parse_tuple() {
    assert_module!(
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
    assert_module!(
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
    assert_module!(
        r#"
        pub const my_policy_id = #[0, 170, 255]
        "#
    );
}

#[test]
fn base16_bytearray_literals() {
    assert_module!(
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
    assert_module!(
        r#"
        fn foo() {}
        "#
    );
}

#[test]
fn function_invoke() {
    assert_module!(
        r#"
        fn foo() {
            let a = bar(42)
        }
        "#
    );
}

#[test]
fn function_ambiguous_sequence() {
    assert_module!(
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
fn first_class_binop() {
    assert_module!(
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
