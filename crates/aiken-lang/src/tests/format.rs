use crate::assert_format;

fn format_source(src: &str) -> String {
    let (module, extra) =
        crate::parser::module(src, crate::ast::ModuleKind::Lib).expect("source should parse");
    let mut formatted = String::new();
    crate::format::pretty(&mut formatted, module, extra, src);
    formatted
}

#[test]
fn onchain_declarations_roundtrip_idempotently() {
    for declaration in [
        "fn answer(){42}",
        "pub fn answer(){42}",
        "validator answer{else(_){True}}",
        "test answer(){True}",
        "bench answer(x via fuzz.any_int){True}",
        "const answer=42",
        "type Answer=Int",
        "use aiken/list",
    ] {
        let source = format!("@onchain(steps:1_000,semantics:e)\n{declaration}");
        let formatted = format_source(&source);
        assert!(formatted.starts_with("@onchain(steps: 1_000, semantics: e)\n"));
        assert_eq!(format_source(&formatted), formatted, "{declaration}");
        let (module, _) = crate::parser::module(&formatted, crate::ast::ModuleKind::Lib).unwrap();
        assert_eq!(module.definition_decorators.len(), 1);
        assert_eq!(
            module.definition_decorators[0].location,
            module.definitions[0].location()
        );
    }
}

#[test]
fn onchain_invalid_settings_are_not_lost_by_formatting() {
    for decorator in [
        "@onchain()",
        "@onchain(semantics: C)",
        "@onchain(steps: 0)",
        "@onchain(steps: -0x10)",
        "@onchain(steps: nope)",
        "@onchain(steps: \"100\")",
        "@onchain(steps: @\"100\")",
        "@onchain(steps: 18446744073709551616)",
        "@onchain(steps: 1, steps: 2)",
        "@onchain(steps: 1, unknown: C)",
        "@onchain(steps: 1, semantics: X)",
        "@onchain(steps: 1, semantics: \"C\")",
        "@onchain(steps: 1, semantics: C, semantics: E)",
    ] {
        let source = format!("{decorator}\nfn answer() {{ 42 }}");
        let formatted = format_source(&source);
        assert!(
            formatted.starts_with(&format!("{decorator}\n")),
            "{formatted}"
        );
        assert_eq!(format_source(&formatted), formatted);
    }
}

#[test]
fn onchain_keeps_mixed_comment_order_with_and_without_assurance_regions() {
    for region in ["", "//@ opaque ∀ x → x  \n@//\n"] {
        let source = format!(
            "//// module\n// ordinary before\n/// docs before\n{region}\
             @onchain(steps:1,semantics:c)\n\
             /// docs between\n// ordinary between\n{region}\
             @onchain(steps:2,semantics:E)\n\
             // ordinary after\n/// docs after\n{region}\
             pub fn answer(){{42}}\n// trailing\n"
        );
        let formatted = format_source(&source);
        let ordered = [
            "//// module",
            "// ordinary before",
            "/// docs before",
            "@onchain(steps: 1, semantics: c)",
            "/// docs between",
            "// ordinary between",
            "@onchain(steps: 2, semantics: E)",
            "// ordinary after",
            "/// docs after",
            "pub fn answer",
            "// trailing",
        ];
        for pair in ordered.windows(2) {
            assert!(
                formatted.find(pair[0]).unwrap() < formatted.find(pair[1]).unwrap(),
                "{formatted}"
            );
        }
        if !region.is_empty() {
            assert_eq!(formatted.matches(region.trim_end()).count(), 3);
        }
        assert_eq!(format_source(&formatted), formatted);
    }
}

#[test]
fn onchain_imports_keep_decorators_and_comment_anchors() {
    let source = concat!(
        "/// zeta docs\n",
        "@onchain(steps:1)\n",
        "// zeta import\n",
        "use zeta\n",
        "// alpha docs\n",
        "@onchain(steps:2)\n",
        "use alpha\n",
        "@onchain(steps:3)\n",
        "use zeta\n",
    );
    let formatted = format_source(source);
    assert_eq!(formatted.matches("@onchain").count(), 3);
    assert_eq!(formatted.matches("use zeta").count(), 2);
    let ordered = [
        "/// zeta docs",
        "@onchain(steps: 1)",
        "// zeta import",
        "use zeta",
        "// alpha docs",
        "@onchain(steps: 2)",
        "use alpha",
        "@onchain(steps: 3)",
    ];
    for pair in ordered.windows(2) {
        assert!(formatted.find(pair[0]).unwrap() < formatted.find(pair[1]).unwrap());
    }
    assert_eq!(format_source(&formatted), formatted);
}

#[test]
fn onchain_data_types_and_constructors_roundtrip_with_existing_decorators() {
    let source = concat!(
        "/// type docs\n",
        "@onchain(steps:1)\n",
        "// before list\n",
        "@list\n",
        "type Example {\n",
        "  /// constructor docs\n",
        "  @onchain(steps:2)\n",
        "  // before tag\n",
        "  @tag(7)\n",
        "  Example(Int)\n",
        "}\n",
    );
    let formatted = format_source(source);
    let ordered = [
        "/// type docs",
        "@onchain(steps: 1)",
        "// before list",
        "@list",
        "type Example",
        "/// constructor docs",
        "@onchain(steps: 2)",
        "// before tag",
        "@tag(7)",
        "Example(Int)",
    ];
    for pair in ordered.windows(2) {
        assert!(formatted.find(pair[0]).unwrap() < formatted.find(pair[1]).unwrap());
    }
    assert_eq!(format_source(&formatted), formatted);
}

#[test]
fn assurance_regions_preserve_bytes_and_are_idempotent() {
    let region =
        "//@ opaque\r\n\t∀ x → ¬ (λ y => y) // literal @ and x@// text\r\n  \r\n\u{2003}@//";
    let source = format!("{region}\nfn answer(){{42}}\n{region}\n");
    let formatted = format_source(&source);
    assert_eq!(formatted.matches(region).count(), 2);
    assert_eq!(format_source(&formatted), formatted);
}

#[test]
fn assurance_regions_standalone_and_trailing() {
    let region = "//@\n\tunknown syntax { [ @ // remains opaque  \r\n@//";
    for source in [region.to_string(), format!("\n{region}\n\n")] {
        let formatted = format_source(&source);
        assert_eq!(formatted, format!("{region}\n"));
        assert_eq!(format_source(&formatted), formatted);
    }

    let formatted = format_source(&format!("fn answer() {{ 42 }}\n{region}"));
    assert!(formatted.ends_with(&format!("{region}\n")));
    assert_eq!(format_source(&formatted), formatted);
}

#[test]
fn assurance_regions_preserve_comment_order() {
    let source = concat!(
        "//// module before\n",
        "//@ first @//\n",
        "//// module after\n",
        "// ordinary before\n",
        "/// documentation before\n",
        "//@ second @//\n",
        "/// documentation after\n",
        "// ordinary after\n",
        "fn first() { 1 }\n",
        "/// between before\n",
        "//@ between @//\n",
        "/// between after\n",
        "fn second() { 2 }\n",
        "/// trailing before\n",
        "//@ trailing @//\n",
        "// trailing after\n",
    );
    let formatted = format_source(source);
    let ordered = [
        "//// module before",
        "//@ first @//",
        "//// module after",
        "// ordinary before",
        "/// documentation before",
        "//@ second @//",
        "/// documentation after",
        "// ordinary after",
        "fn first",
        "/// between before",
        "//@ between @//",
        "/// between after",
        "fn second",
        "/// trailing before",
        "//@ trailing @//",
        "// trailing after",
    ];
    for pair in ordered.windows(2) {
        assert!(formatted.find(pair[0]).unwrap() < formatted.find(pair[1]).unwrap());
    }
    assert_eq!(format_source(&formatted), formatted);
}

#[test]
fn assurance_regions_keep_import_anchors() {
    let source = "//@ top @//\nuse zeta\n//@ middle @//\nuse alpha\n//@ end @//";
    let formatted = format_source(source);
    let ordered = [
        "//@ top @//",
        "use zeta",
        "//@ middle @//",
        "use alpha",
        "//@ end @//",
    ];
    for pair in ordered.windows(2) {
        assert!(formatted.find(pair[0]).unwrap() < formatted.find(pair[1]).unwrap());
    }
    assert_eq!(format_source(&formatted), formatted);
}

#[test]
fn assurance_regions_keep_decorator_order() {
    let source = concat!(
        "/// before type\n",
        "//@ before decorators @//\n",
        "@tag(7)\n",
        "//@ between decorators @//\n",
        "@list\n",
        "//@ after decorators @//\n",
        "type Datum { value: Int }\n",
        "type Choice {\n",
        "  @tag(1)\n",
        "  //@ before constructor @//\n",
        "  First\n",
        "  @tag(2)\n",
        "  //@ before second constructor @//\n",
        "  Second\n",
        "}\n",
    );
    let formatted = format_source(source);
    let ordered = [
        "/// before type",
        "//@ before decorators @//",
        "@tag(7)",
        "//@ between decorators @//",
        "@list",
        "//@ after decorators @//",
        "type Datum",
        "type Choice",
        "@tag(1)",
        "//@ before constructor @//",
        "First",
        "@tag(2)",
        "//@ before second constructor @//",
        "Second",
    ];
    for pair in ordered.windows(2) {
        assert!(formatted.find(pair[0]).unwrap() < formatted.find(pair[1]).unwrap());
    }
    assert_eq!(format_source(&formatted), formatted);
}

#[test]
fn assurance_regions_inside_functions_do_not_reindent() {
    let region = "//@\r\nunindented λ\r\n\t \t// text @\r\n @//";
    let source = format!("fn answer() {{\n  {region}\n42\n}}\n");
    let formatted = format_source(&source);
    assert!(formatted.contains(region));
    assert_eq!(format_source(&formatted), formatted);
}

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
fn format_single_hex_digit() {
    assert_format!(
        r#"
        const a = 0xa
        const b = 0x0f
        const c = 0x0000000f
        const d = 0x123
    "#
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
fn format_nul_byte() {
    assert_format!(
        r#"
        fn label(str: String) -> Void {
          str
            |> builtin.append_string(@"\0", _)
            |> builtin.debug(Void)
        }"#
    );
}

#[test]
fn format_preserve_punning() {
    assert_format!(
        r#"
        fn label() -> Void {
          let thing = Input { ..base, output_reference }
          let thing = Input { output_reference }
        }"#
    );
}

#[test]
fn format_preserve_auto_pun() {
    assert_format!(
        r#"
        fn label() -> Void {
          let thing = Input { ..base, output_reference: output_reference }
          let thing = Input { output_reference: something_else }
        }"#
    );
}

#[test]
fn format_allow_comments_in_byte_array() {
    assert_format!(
        r#"
        pub const thing =
          #[
            // thing
            0x12,
            // wow
            0x10,
          ]
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
fn format_value_literal_to_canonical_nested_syntax() {
    let source = r#"const value = {
        #"00000000000000000000000000000000000000000000000000000000": { #"bb": 42 }
    }
"#;
    let formatted = format_source(source);

    pretty_assertions::assert_eq!(
        formatted,
        r#"const value = {
  #"00000000000000000000000000000000000000000000000000000000": {
    #"bb": 42,
  },
}
"#,
    );
    pretty_assertions::assert_eq!(format_source(&formatted), formatted);
}

#[test]
fn format_value_literal_puts_lovelace_first() {
    let expected = r#"const value = {
  lovelace: 42,
  #"00000000000000000000000000000000000000000000000000000000": {
    "foo": 1,
  },
}
"#;

    for source in [
        r#"const value = {
  lovelace: 42,
  #"00000000000000000000000000000000000000000000000000000000": { "foo": 1 },
}
"#,
        r#"const value = {
  #"00000000000000000000000000000000000000000000000000000000": { "foo": 1 },
  lovelace: 42,
}
"#,
    ] {
        pretty_assertions::assert_eq!(format_source(source), expected);
    }

    pretty_assertions::assert_eq!(format_source(expected), expected);
}

#[test]
fn format_value_literal_keeps_comments_with_lovelace() {
    let source = r#"const value = {
      // custom policy
      #"00000000000000000000000000000000000000000000000000000000": {
        "foo": 1,
      },
      // ada
      lovelace:
        // quantity
        42,
    }
"#;
    let formatted = format_source(source);

    pretty_assertions::assert_eq!(
        formatted,
        r#"const value = {
  // ada
  lovelace:
    // quantity
    42,
  // custom policy
  #"00000000000000000000000000000000000000000000000000000000": {
    "foo": 1,
  },
}
"#,
    );
    pretty_assertions::assert_eq!(format_source(&formatted), formatted);
}

#[test]
fn format_value_literal_with_named_fields() {
    let source = r#"const value = {
  policy_1: {
    asset_1: 1,
    #"bb": quantity,
  },
  #"00000000000000000000000000000000000000000000000000000000": {
    asset_1: quantity,
    "foo": 1,
  },
  lovelace: quantity,
}
"#;
    let expected = r#"const value = {
  lovelace: quantity,
  #"00000000000000000000000000000000000000000000000000000000": {
    "foo": 1,
    asset_1: quantity,
  },
  policy_1: {
    asset_1: 1,
    #"bb": quantity,
  },
}
"#;

    pretty_assertions::assert_eq!(format_source(source), expected);
    pretty_assertions::assert_eq!(format_source(expected), expected);
}

#[test]
fn format_value_literal_preserves_comments_around_named_fields() {
    let source = r#"const value = {
      // policy
      policy:
        // assets
        {
          // asset
          asset:
            // quantity
            quantity,
          // before assets close
        },
      // before value close
    }
"#;
    let expected = r#"const value = {
  // policy
  policy:
    // assets
    {
      // asset
      asset:
        // quantity
        quantity,
      // before assets close
    },
  // before value close
}
"#;

    pretty_assertions::assert_eq!(format_source(source), expected);
    pretty_assertions::assert_eq!(format_source(expected), expected);
}

#[test]
fn format_insert_value_builtin_as_a_value_literal() {
    let expected = r#"const value = {
  policy: {
    asset: quantity,
  },
}
"#;

    for source in [
        "const value = insert_value(policy, asset, quantity, {})\n",
        "const value = builtin.insert_value(policy, asset, quantity, {})\n",
    ] {
        pretty_assertions::assert_eq!(format_source(source), expected);
    }
    pretty_assertions::assert_eq!(format_source(expected), expected);
}

#[test]
fn format_value_literal_nested_in_call_does_not_affect_the_next_argument() {
    let source = r#"fn example() {
  consume({ policy: { asset: quantity } }, next(first, second))
}
"#;
    let expected = r#"fn example() {
  consume(
    {
      policy: {
        asset: quantity,
      },
    },
    next(first, second),
  )
}
"#;

    pretty_assertions::assert_eq!(format_source(source), expected);
    pretty_assertions::assert_eq!(format_source(expected), expected);
}

#[test]
fn format_value_literal_preserves_asset_name_encoding() {
    let source = r#"const value = {
      #"00000000000000000000000000000000000000000000000000000000": {
        "bb": 2,
        #"aa": 1,
        "café": 3,
      },
    }
"#;
    let formatted = format_source(source);

    pretty_assertions::assert_eq!(
        formatted,
        r#"const value = {
  #"00000000000000000000000000000000000000000000000000000000": {
    "bb": 2,
    "café": 3,
    #"aa": 1,
  },
}
"#,
    );
    pretty_assertions::assert_eq!(format_source(&formatted), formatted);
}

#[test]
fn format_empty_value_literal_on_one_line() {
    let source = "const value = {}\n";
    let formatted = format_source(source);

    pretty_assertions::assert_eq!(formatted, source);
    pretty_assertions::assert_eq!(format_source(&formatted), formatted);
}

#[test]
fn format_empty_value_literal_preserves_comments() {
    let source = r#"const value = {
      // still here
    }
"#;
    let formatted = format_source(source);

    pretty_assertions::assert_eq!(
        formatted,
        r#"const value = {
  // still here
}
"#,
    );
    pretty_assertions::assert_eq!(format_source(&formatted), formatted);
}

#[test]
fn format_let_bound_value_literal() {
    let source = r#"fn example() {
      let value = {
        #"00000000000000000000000000000000000000000000000000000000": {
          #"aa": 1,
        },
      }
      let empty = {}
      (value, empty)
    }
"#;
    let formatted = format_source(source);

    pretty_assertions::assert_eq!(
        formatted,
        r#"fn example() {
  let value = {
    #"00000000000000000000000000000000000000000000000000000000": {
      #"aa": 1,
    },
  }
  let empty = {}
  (value, empty)
}
"#,
    );
    pretty_assertions::assert_eq!(format_source(&formatted), formatted);
}

#[test]
fn format_preserves_empty_line_after_value_literal() {
    let source = r#"fn example() {
  let value = {
    #"00000000000000000000000000000000000000000000000000000000": {
      "foo": 1,
    },
  }

  and {
    builtin.lookup_value(policy_00, "foo", value) == 1,
    builtin.lookup_value(policy_00, "bar", value) == 0,
    builtin.lookup_value(policy_ff, "foo", value) == 0,
  }
}
"#;
    let formatted = format_source(source);

    pretty_assertions::assert_eq!(formatted, source);
    pretty_assertions::assert_eq!(format_source(&formatted), formatted);
}

#[test]
fn format_value_literal_preserves_nested_comments() {
    let source = r#"const value = {
      // outer entry
      #"00000000000000000000000000000000000000000000000000000000": {
        // inner entry
        #"bb": 42,
      }
    }"#;

    let formatted = format_source(source);

    pretty_assertions::assert_eq!(
        formatted,
        r#"const value = {
  // outer entry
  #"00000000000000000000000000000000000000000000000000000000": {
    // inner entry
    #"bb": 42,
  },
}
"#,
    );
    pretty_assertions::assert_eq!(format_source(&formatted), formatted);
}

#[test]
fn format_value_literal_preserves_comments_before_closing_braces() {
    let source = r#"const value = {
      #"00000000000000000000000000000000000000000000000000000000": {
        #"aa": 1,
        // before inner close
      },
      // before outer close
    }"#;

    let formatted = format_source(source);

    pretty_assertions::assert_eq!(
        formatted,
        r#"const value = {
  #"00000000000000000000000000000000000000000000000000000000": {
    #"aa": 1,
    // before inner close
  },
  // before outer close
}
"#,
    );
    pretty_assertions::assert_eq!(format_source(&formatted), formatted);
}

#[test]
fn format_value_literal_preserves_comments_between_keys_and_values() {
    let source = r#"const value = {
      #"00000000000000000000000000000000000000000000000000000000":
        // before assets
        {
          #"aa":
            // before quantity
            1,
        },
    }"#;

    let formatted = format_source(source);

    pretty_assertions::assert_eq!(
        formatted,
        r#"const value = {
  #"00000000000000000000000000000000000000000000000000000000":
    // before assets
    {
      #"aa":
        // before quantity
        1,
    },
}
"#,
    );
    pretty_assertions::assert_eq!(format_source(&formatted), formatted);
}

#[test]
fn format_value_literal_keeps_comments_with_canonicalized_entries() {
    let source = r#"const value = {
      // policy 22
      #"22222222222222222222222222222222222222222222222222222222": {
        // asset bb
        #"bb": 2,
        // asset aa
        #"aa": 1,
      },
      // policy 11
      #"11111111111111111111111111111111111111111111111111111111": {
        // asset cc
        #"cc": 3,
      },
    }
"#;
    let formatted = format_source(source);

    pretty_assertions::assert_eq!(
        formatted,
        r#"const value = {
  // policy 11
  #"11111111111111111111111111111111111111111111111111111111": {
    // asset cc
    #"cc": 3,
  },
  // policy 22
  #"22222222222222222222222222222222222222222222222222222222": {
    // asset aa
    #"aa": 1,
    // asset bb
    #"bb": 2,
  },
}
"#,
    );
    pretty_assertions::assert_eq!(format_source(&formatted), formatted);
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
fn format_if_soft_cast() {
    assert_format!(
        r#"
        pub fn foo(a) {
          if a is Option<Int> { 14 } else { 42 }
          }
    "#
    );
}

#[test]
fn format_if_soft_cast_pattern() {
    assert_format!(
        r#"
        pub fn foo(a) {
          if a is Some(x): Option<Int> { 14 } else if b is Foo { b } else { 42 }
          }
    "#
    );
}

#[test]
fn format_if_soft_cast_record() {
    assert_format!(
        r#"
        pub fn foo(foo: Data) -> Int {
          if foo is Foo { a }: Foo {
            a
          } else {
            0
          }
        }
        "#
    );
}

#[test]
fn format_logic_op_with_code_block() {
    assert_format!(
        r#"
        fn foo() {
          True || {
            let bar = 1
            bar == bar
          }
        }
        "#
    );
}

#[test]
fn format_grouped_expression() {
    assert_format!(
        r#"
        fn foo() {
           y == { x |> f }
        }
        "#
    );
}

#[test]
fn format_grouped_expression_2() {
    assert_format!(
        r#"
        fn foo() {
           ( y == x ) |> f
        }
        "#
    );
}

#[test]
fn format_grouped_expression_3() {
    assert_format!(
        r#"
        fn foo() {
          { x |> f } == y
        }
        "#
    );
}

#[test]
fn format_grouped_expression_4() {
    assert_format!(
        r#"
        fn foo() {
          x |> { f == y }
        }
        "#
    );
}

#[test]
fn format_preserve_newline_after_bool_expect() {
    assert_format!(
        r#"
        fn foo() {
          expect 1 == 1

          False
        }
        "#
    );
}

#[test]
fn format_validator() {
    assert_format!(
        r#"
      validator thing ( ) {
      // What is the purpose of life

      spend(d: Datum, r: Redeemer, ctx: ScriptContext) -> Bool {
      True
      }
      }

      // What?
      validator foo {
        /// Some documentation for foo
        foo() {
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
        validator foo( param1 : ByteArray ) {
        spend(d: Datum, r: Redeemer, ctx: ScriptContext) -> Bool {
        True
        }
        /// This is bar
    mint(r: Redeemer, ctx    : ScriptContext  )   ->   Bool { True }
        }
    "#
    );
}

#[test]
fn format_double_validator_public() {
    assert_format!(
        r#"
        validator foo ( param1 : ByteArray ) {
        spend(d: Datum, r: Redeemer, ctx: ScriptContext) -> Bool {
        True
        }
        /// This is bar
    mint(r: Redeemer, ctx    : ScriptContext  )   ->   Bool { True }
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
            [x] -> [1, 2, 3]
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
fn format_merge_imports() {
    assert_format!(
        r#"
        use aiken/list.{bar, foo}
        use aiken/list.{baz}
    "#
    );
}

#[test]
fn format_merge_imports_2() {
    assert_format!(
        r#"
        use aiken/list.{bar, foo}
        use aiken/dict
        use aiken/list
    "#
    );
}

#[test]
fn format_merge_imports_alias() {
    assert_format!(
        r#"
        use aiken/list.{bar, foo}
        use aiken/list.{baz} as vector
    "#
    );
}

#[test]
fn format_merge_imports_alias_2() {
    assert_format!(
        r#"
        use aiken/list.{bar, foo} as vector
        use aiken/list.{baz} as vector
    "#
    );
}

#[test]
fn format_merge_imports_comments() {
    assert_format!(
        r#"
        // foo
        use aiken/list.{bar, foo}
        // bar
        use aiken/list.{baz}
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
fn backpassing_format_many_assignment_patterns() {
    assert_format!(
        r#"
        fn backpassing() -> Int {

          let
            elem, accumulator, wow,
            who,
            thing,
            what,
            idk,
            wee,
            will,
            it,
            break,



          <- fold([1, 2, 3],
          0)

          elem + accumulator
        }
        "#
    );
}

#[test]
fn long_backpassing_over_two_lines_when_possible() {
    assert_format!(
        r#"
        fn foo() {
          expect
            head_output_tokens,
            tail_output_assets,
            tail_output_assets,
            tail_output_assets,
            tail_output_assets,
            tail_output_assets,
            tail_output_assets,
            tail_output_assets,
          <- pairs.pop_until(output_assets, equals_bytearray(policy, _), alfdljfdskfjdsfsf, lfjsdfjsfsdmf, kfdsfkjhsdfhskjfs)

          head_output_tokens + tail_output_assets
        }
        "#
    );
}

#[test]
fn long_backpassing_over_two_lines_when_possible_2() {
    assert_format!(
        r#"
        fn foo() {
          let
            head_output_tokens,
            tail_output_assets,
          <- pairs.pop_until(output_assets, equals_bytearray(policy, _))

          head_output_tokens + tail_output_assets
        }
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
fn preserve_comment_in_record() {
    assert_format!(
        r#"
        fn foo() {
          let Output {
            // something
            address: own_address,
            // value: own_value,
            ..
          } = own_output

          own_address
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

#[test]
fn fuzzer_annotations() {
    assert_format!(
        r#"
        test foo(n: Int via int()) {
          todo
        }
        "#
    );
}

#[test]
fn preserve_associativity_parens_in_binop() {
    assert_format!(
        r#"
        pub fn bar() {
          ( a || b ) || c
        }
        "#
    );
}

#[test]
fn superfluous_parens_in_binop() {
    assert_format!(
        r#"
        pub fn bar() {
          a && ( b && c )
        }
        "#
    );
}

#[test]
fn format_pairs() {
    assert_format!(
        r#"
        pub fn foo(x: Pair<Int, Int>) {
            Pair(x.1st, x.2nd)
        }"#
    );
}

#[test]
fn format_fn_pattern() {
    assert_format!(
        r#"
        pub fn foo(Foo { a, b, .. }) {
            todo
        }

        pub fn bar([Bar] : List<Bar>) {
            todo
        }

        pub fn baz((Baz, Baz) as x) {
            todo
        }

        pub fn fiz(Pair(fst, snd) as x: Pair<Int, Int>) {
            todo
        }

        test buz((a, b) via some_fuzzer()) {
            todo
        }
        "#
    );
}

#[test]
fn format_anon_fn_pattern() {
    assert_format!(
        r#"
        pub fn main() {
            let foo = fn (Foo { a, b, .. }) { todo }
            let bar = fn ([Bar] : List<Bar>) { todo }
            let baz = fn ((Baz, Baz) as x) { todo }
            let fiz = fn (Pair(fst, snd) as x: Pair<Int, Int>) { todo }
            todo
        }
        "#
    );
}

#[test]
fn format_validator_pattern() {
    assert_format!(
        r#"
        validator foo(Foo { a, b, .. }) {
            spend() { todo }
        }

        validator foo([Bar] : List<Bar>) {
            spend() { todo }
        }

        validator foo((Baz, Baz) as x) {
            mint() { todo }
        }

        validator fiz((fst, snd) as x: Pair<Int, Int>) {
            spend() { todo }
        }
        "#
    );
}

#[test]
fn format_variadic_trace() {
    assert_format!(
        r#"
        fn foo() {
            trace @"foo": @"bar"
            trace "foo": "bar"
            trace @"foo": "bar", @"baz"
            trace bar: @"baz"
            Void
        }
        "#
    );
}

#[test]
fn format_pattern_bytearray() {
    assert_format!(
        r#"
        fn main(foo) {
            when foo is {
                "Aiken, rocks!" -> True
                #"00abcd" -> True
                #[1, 2, 3, 4] -> True
                #[0x00, 0xab, 0xcd] -> True
                _ -> False
            }
        }
        "#
    );
}

#[test]
fn format_long_bin_op_1() {
    assert_format!(
        r#"
        test foo() {
            [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] == [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
        }
        "#
    );
}

#[test]
fn format_long_bin_op_2() {
    assert_format!(
        r#"
        test foo() {
            [0, 0, 0] == [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
        }
        "#
    );
}

#[test]
fn format_long_bin_op_3() {
    assert_format!(
        r#"
        test foo() {
            [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] == [0, 0, 0]
        }
        "#
    );
}

#[test]
fn format_long_bin_op_4() {
    assert_format!(
        r#"
        test foo() {
            [foo, bar, baz, (2, 3), (4, 5), (6, 7), (8, 9), biz, buz, fizz, fuzz, alice, bob, carole, i, am, out, of, names] == [0, 0, 0]
        }
        "#
    );
}

#[test]
fn format_long_pattern_1() {
    assert_format!(
        r#"
        test foo() {
            when x is {
                [True, False, True, False, True, False, True, False, True, False, True, False, True, False, True, False, ..] -> todo
                _ -> todo
            }
        }
        "#
    );
}

#[test]
fn format_long_pattern_2() {
    assert_format!(
        r#"
        test foo() {
            when x is {
                [(1, 2, 3), (4, 5, 6), (7, 8, 9), (10, 11, 12), (13, 14, 15), (16, 17, 18), (19, 20, 21), (22, 23, 24)] -> todo
                _ -> todo
            }
        }
        "#
    );
}

#[test]
fn format_long_standalone_literal() {
    assert_format!(
        r#"
        test foo() {
          let left =
            [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
          let right =
            [
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0,
            ]
          left == right
        }
        "#
    );
}

#[test]
fn format_long_imports() {
    assert_format!(
        r#"
        use aiken/list.{foldr, foldl, is_empty, filter, map, find, any, all, flat_map, partition, push, reduce, reverse, repeat}
        "#
    );
}

#[test]
fn format_long_pair() {
    assert_format!(
        r#"
        test foo() {
            expect(Some([
                Pair(GovernanceActionId { transaction: only7s, proposal_procedure: 2 },
                Abstain),
            ])) == whatever

            expect(Some([
                Foo(GovernanceActionId { transaction: only7s, proposal_procedure: 2 },
                Abstain),
            ])) == whatever

            expect(Some([
                (GovernanceActionId { transaction: only7s, proposal_procedure: 2 },
                Abstain),
            ])) == whatever
        }
        "#
    );
}

#[test]
fn format_validator_exhaustive_handlers() {
    assert_format!(
        r#"
            validator foo {
              mint(_redeemer, _policy_id, _self) {
                True
              }

              spend(_datum, _redeemer, _policy_id, _self) {
                True
              }

              withdraw(_redeemer, _account, _self) {
                True
              }

              publish(_redeemer, _certificate, _self) {
                True
              }

              vote(_redeemer, _voter, _self) {
                True
              }

              propose(_redeemer, _proposal, _self) {
                True
              }
            }
        "#
    );
}

#[test]
fn format_validator_exhaustive_handlers_extra_default_fallback() {
    assert_format!(
        r#"
            validator foo {
              mint(_redeemer, _policy_id, _self) {
                True
              }

              spend(_datum, _redeemer, _policy_id, _self) {
                True
              }

              withdraw(_redeemer, _account, _self) {
                True
              }

              publish(_redeemer, _certificate, _self) {
                True
              }

              vote(_redeemer, _voter, _self) {
                True
              }

              propose(_redeemer, _proposal, _self) {
                True
              }

              else(_) {
                fail
              }
            }
        "#
    );
}

#[test]
fn format_validator_exhaustive_handlers_extra_non_default_fallback() {
    assert_format!(
        r#"
            validator foo {
              mint(_redeemer, _policy_id, _self) {
                True
              }

              spend(_datum, _redeemer, _policy_id, _self) {
                True
              }

              withdraw(_redeemer, _account, _self) {
                True
              }

              publish(_redeemer, _certificate, _self) {
                True
              }

              vote(_redeemer, _voter, _self) {
                True
              }

              propose(_redeemer, _proposal, _self) {
                True
              }

              else(_) {
                True
              }
            }
        "#
    );
}

#[test]
fn single_line_alternative_patterns() {
    assert_format!(
        r#"
        fn foo() {
            when bar is {
                a | b | c -> True
                d | e -> {
                    let x = e + d
                    x > 10
                }
                _ -> False
            }
        }
        "#
    );
}

#[test]
fn multiline_alternative_patterns() {
    assert_format!(
        r#"
        validator direct_proxy {
          mint(_redeemer: Void, policy_id: PolicyId, self: Transaction) {
            list.any(
              self.certificates,
              fn(certificate) {
                when certificate is {
                  RegisterDelegateRepresentative {
                    delegate_representative: credential,
                    ..
                  } | UnregisterDelegateRepresentative {
                    delegate_representative: credential,
                    ..
                  } | RegisterCredential { credential, .. } | UnregisterCredential {
                    credential,
                    ..
                  } | RegisterAndDelegateCredential { credential, .. } ->
                    credential == Script(policy_id)
                  _ -> False
                }
              },
            )
          }
        }
        "#
    );
}

#[test]
fn trace_if_false_pipeline() {
    assert_format!(
        r#"
        fn main(self) {
          (self.extra_signatories |> list.has(self.extra_signatories, config.cold_key))?
        }
        "#
    );
}

#[test]
fn trace_if_false_unop() {
    assert_format!(
        r#"
        fn main(self) {
            (!True)?
        }
        "#
    );
}

#[test]
fn trace_if_false_todo() {
    assert_format!(
        r#"
        fn main(self) {
            (todo @"whatever")?
        }
        "#
    );
}

#[test]
fn trace_if_false_fail() {
    assert_format!(
        r#"
        fn main(self) {
            (fail @"whatever")?
        }
        "#
    );
}

#[test]
fn multiline_constant() {
    assert_format!(
        r#"
        const n: Int = {
            let x = 0
            x + 1
        }
        "#
    );
}

#[test]
fn multiline_if_condition() {
    assert_format!(
        r#"
        fn foo() {
          if
          list.is_empty(outputs) && (
            !list.is_empty(mint_redeemers) || !list.is_empty(cert_redeemers)
          ){
            True
          } else {
              False
          }
        }
        "#
    );
}

#[test]
fn callback_and_op() {
    assert_format!(
        r#"
        fn foo() {
            let labels = list.filter(labels, fn(lbl) {
                and {
                lbl != sc_missing_admin_approval_for_foreign_assets,
                lbl != sc_missing_admin_approval_for_certificate_publish,
              }
            })
            labels
        }
        "#
    );
}

#[test]
fn multiline_if_is() {
    assert_format!(
        r#"
        fn foo() {
            if first_asset
            is
            Pair(first_asset_policy, first_asset_tokens): Pair<PolicyId, Data> {
                True
                } else {
                    False }
        }
        "#
    );
}

#[test]
fn multiline_if_is_2() {
    assert_format!(
        r#"
        fn foo() {
            if [1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0]
            is
            Pair(first_asset_policy, first_asset_tokens): Pair<PolicyId, Data> {
                True
                } else {
                    False }
        }
        "#
    );
}

#[test]
fn comment_in_pipeline() {
    assert_format!(
        r#"
        fn foo() {
            a
            // stuff
            // warning: wow
            |> b
            // Comment
            |> c
        }
        "#
    );
}

#[test]
fn capture_right_hand_side_assign() {
    assert_format!(
        r#"
        fn foo() {
            let (_aa, bb, _cc) = bar(foo: _a, _b, _)
            let _ = baz(_d, [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23])
            bb
        }
        "#
    );
}

#[test]
fn should_preserve_grouping() {
    assert_format!(
        r#"
        pub fn a0p(pair: Pair<ByteArray, Dict<ByteArray, Int>>) {
          (pair.2nd |> unsingleton).2nd
        }
        "#
    );
}

#[test]
fn types_as_namespace() {
    assert_format!(
        r#"
        use foo.{ Foo }

        fn predicate(val) {
          when val is {
            Foo.I(n) -> n >= 14
            foo.Foo.B(bytes) -> bytes == "aiken"
          }
        }

        test my_test() {
          and {
            predicate(foo.Foo.I(42)),
            predicate(Foo.b("aiken"))
          }
        }
        "#
    );
}

#[test]
fn list_decorator() {
    assert_format!(
        r#"
        @list
        type Datum {
          wow: Int
        }
        "#
    );
}

#[test]
fn tag_decorator() {
    assert_format!(
        r#"
        @tag(420)
        type Datum {
          wow: Int
        }
        "#
    );
}

#[test]
fn more_than_one_decorator() {
    assert_format!(
        r#"
        @tag(420)
        @list
        type Datum {
          wow: Int
        }
        "#
    );
}

#[test]
fn decorators_on_constructor() {
    assert_format!(
        r#"
        type Datum {
          @tag(420)
          Yes
          @tag(69)
          No
        }
        "#
    );
}

#[test]
fn long_import_line() {
    assert_format!(
        r#"
        use cardano/transaction.{Input,
          Output, OutputReference, Transaction, find_input}
        "#
    );
}

#[test]
fn long_import_line_2() {
    assert_format!(
        r#"
        use cardano/transaction.{Input,
          Output, OutputReference, Transaction, find_input, stuff, thing, wow, yea}
        "#
    );
}

#[test]
fn expect_comment() {
    assert_format!(
        r#"
        test foo() {
          /// Prevent silly mistake
          expect 1 + 1 == 3
        }
        "#
    );
}
