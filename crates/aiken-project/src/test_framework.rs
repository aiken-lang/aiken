#[cfg(test)]
mod test {
    use crate::{
        module::{CheckedModule, CheckedModules},
        utils,
    };
    use aiken_lang::{
        ast::{DataTypeKey, Definition, ModuleKind, TraceLevel, Tracing, TypedDataType},
        builtins,
        expr::UntypedExpr,
        format::Formatter,
        gen_uplc::CodeGenerator,
        line_numbers::LineNumbers,
        parser::{self, extra::ModuleExtra},
        plutus_version::PlutusVersion,
        test_framework::*,
        IdGenerator,
    };
    use indexmap::IndexMap;
    use indoc::indoc;
    use std::{
        collections::{BTreeMap, HashMap},
        path::PathBuf,
    };
    use uplc::PlutusData;

    const TEST_KIND: ModuleKind = ModuleKind::Lib;

    pub fn test_from_source(src: &str) -> (Test, IndexMap<DataTypeKey, TypedDataType>) {
        let id_gen = IdGenerator::new();

        let module_name = "";

        let mut module_types = HashMap::new();
        module_types.insert("aiken".to_string(), builtins::prelude(&id_gen));
        module_types.insert("aiken/builtin".to_string(), builtins::plutus(&id_gen));

        let mut warnings = vec![];
        let (ast, _) = parser::module(src, TEST_KIND).expect("Failed to parse module");
        let ast = ast
            .infer(
                &id_gen,
                TEST_KIND,
                module_name,
                &module_types,
                Tracing::All(TraceLevel::Verbose),
                &mut warnings,
                None,
            )
            .expect("Failed to type-check module.");

        module_types.insert(module_name.to_string(), ast.type_info.clone());

        let test = ast
            .definitions()
            .filter_map(|def| match def {
                Definition::Test(test) => Some(test.clone()),
                _ => None,
            })
            .last()
            .expect("No test found in declared src?");

        let mut functions = builtins::prelude_functions(&id_gen, &module_types);
        let mut data_types = builtins::prelude_data_types(&id_gen);
        let mut constants = IndexMap::new();
        ast.register_definitions(&mut functions, &mut constants, &mut data_types);

        let mut module_sources = HashMap::new();
        module_sources.insert(
            module_name.to_string(),
            (src.to_string(), LineNumbers::new(src)),
        );

        let mut modules = CheckedModules::default();
        modules.insert(
            module_name.to_string(),
            CheckedModule {
                kind: TEST_KIND,
                extra: ModuleExtra::default(),
                name: module_name.to_string(),
                code: src.to_string(),
                ast,
                package: String::new(),
                input_path: PathBuf::new(),
            },
        );

        let mut generator = CodeGenerator::new(
            PlutusVersion::default(),
            utils::indexmap::as_ref_values(&functions),
            utils::indexmap::as_ref_values(&constants),
            utils::indexmap::as_ref_values(&data_types),
            utils::indexmap::as_str_ref_values(&module_types),
            utils::indexmap::as_str_ref_values(&module_sources),
            Tracing::All(TraceLevel::Verbose),
        );

        (
            Test::from_function_definition(
                &mut generator,
                test.to_owned(),
                module_name.to_string(),
                PathBuf::new(),
            ),
            data_types,
        )
    }

    fn property(src: &str) -> (PropertyTest, impl Fn(PlutusData) -> String) {
        let prelude = indoc! { r#"
            use aiken/builtin

            const max_int: Int = 255

            pub fn int() -> Fuzzer<Int> {
              fn(prng: PRNG) -> Option<(PRNG, Int)> {
                when prng is {
                  Seeded { seed, choices } -> {
                     let choice =
                       seed
                         |> builtin.index_bytearray(0)

                     Some((
                       Seeded {
                         seed: builtin.blake2b_256(seed),
                         choices: builtin.cons_bytearray(choice, choices)
                       },
                       choice
                     ))
                  }

                  Replayed { cursor, choices } -> {
                    if cursor >= 1 {
                        let cursor = cursor - 1
                        Some((
                          Replayed { choices, cursor },
                          builtin.index_bytearray(choices, cursor)
                        ))
                    } else {
                        None
                    }
                  }
                }
              }
            }

            fn bool() -> Fuzzer<Bool> {
              int() |> map(fn(n) { n % 2 == 0 })
            }

            fn bytearray() -> Fuzzer<ByteArray> {
              int()
                |> map(
                     fn(n) {
                       n
                         |> builtin.integer_to_bytearray(True, 32, _)
                         |> builtin.blake2b_256()
                         |> builtin.slice_bytearray(8, 4, _)
                     },
                   )
            }

            pub fn constant(a: a) -> Fuzzer<a> {
              fn(s0) { Some((s0, a)) }
            }

            pub fn and_then(fuzz_a: Fuzzer<a>, f: fn(a) -> Fuzzer<b>) -> Fuzzer<b> {
              fn(s0) {
                when fuzz_a(s0) is {
                  Some((s1, a)) -> f(a)(s1)
                  None -> None
                }
              }
            }

            pub fn map(fuzz_a: Fuzzer<a>, f: fn(a) -> b) -> Fuzzer<b> {
              fn(s0) {
                when fuzz_a(s0) is {
                  Some((s1, a)) -> Some((s1, f(a)))
                  None -> None
                }
              }
            }

            pub fn map2(fuzz_a: Fuzzer<a>, fuzz_b: Fuzzer<b>, f: fn(a, b) -> c) -> Fuzzer<c> {
              fn(s0) {
                when fuzz_a(s0) is {
                  Some((s1, a)) ->
                    when fuzz_b(s1) is {
                      Some((s2, b)) -> Some((s2, f(a, b)))
                      None -> None
                    }
                  None -> None
                }
              }
            }
        "#};

        let src = format!("{prelude}\n{src}");

        match test_from_source(&src) {
            (Test::PropertyTest(test), data_types) => {
                let type_info = test.fuzzer.type_info.clone();

                let reify = move |counterexample| {
                    let data_type_refs = utils::indexmap::as_ref_values(&data_types);
                    let expr = UntypedExpr::reify_data(&data_type_refs, counterexample, &type_info)
                        .expect("Failed to reify value.");
                    Formatter::new().expr(&expr, false).to_pretty_string(70)
                };

                (test, reify)
            }
            (Test::UnitTest(..), _) => {
                panic!("Expected to yield a PropertyTest but found a UnitTest")
            }
        }
    }

    fn expect_failure<'a>(
        prop: &'a PropertyTest,
        plutus_version: &'a PlutusVersion,
    ) -> Counterexample<'a> {
        let mut labels = BTreeMap::new();
        let mut remaining = PropertyTest::DEFAULT_MAX_SUCCESS;
        match prop.run_n_times(
            &mut remaining,
            Prng::from_seed(42),
            &mut labels,
            plutus_version,
        ) {
            Ok(Some(counterexample)) => counterexample,
            _ => panic!("expected property to fail but it didn't."),
        }
    }

    #[test]
    fn test_prop_basic() {
        let (prop, _) = property(indoc! { r#"
            test foo(n: Int via int()) {
                n >= 0
            }
        "#});

        assert!(prop
            .run::<()>(
                42,
                PropertyTest::DEFAULT_MAX_SUCCESS,
                &PlutusVersion::default()
            )
            .is_success());
    }

    #[test]
    fn test_prop_labels() {
        let (prop, _) = property(indoc! { r#"
            fn label(str: String) -> Void {
              str
                |> builtin.append_string(@"\0", _)
                |> builtin.debug(Void)
            }

            test foo(head_or_tail via bool()) {
                if head_or_tail {
                    label(@"head")
                } else {
                    label(@"tail")
                }
                True
            }
        "#});

        match prop.run::<()>(
            42,
            PropertyTest::DEFAULT_MAX_SUCCESS,
            &PlutusVersion::default(),
        ) {
            TestResult::UnitTestResult(..) => unreachable!("property returned unit-test result ?!"),
            TestResult::PropertyTestResult(result) => {
                assert!(
                    result
                        .labels
                        .iter()
                        .eq(vec![(&"head".to_string(), &53), (&"tail".to_string(), &47)]),
                    "labels: {:#?}",
                    result.labels
                )
            }
        }
    }

    #[test]
    fn test_prop_always_odd() {
        let (prop, reify) = property(indoc! { r#"
            test foo(n: Int via int()) {
                n % 2 == 0
            }
        "#});

        let plutus_version = PlutusVersion::default();
        let mut counterexample = expect_failure(&prop, &plutus_version);

        counterexample.simplify();

        assert_eq!(counterexample.choices, vec![1]);
        assert_eq!(reify(counterexample.value), "1");
    }

    #[test]
    fn test_prop_combine() {
        let (prop, reify) = property(indoc! { r#"
            fn pair(fuzz_a: Fuzzer<a>, fuzz_b: Fuzzer<b>) -> Fuzzer<(a, b)> {
                fuzz_a
                    |> and_then(fn(a) {
                        fuzz_b
                            |> map(fn(b) {
                                (a, b)
                            })
                    })
            }


            test foo(t: (Int, Int) via pair(int(), int())) {
                t.1st + t.2nd <= 400
            }
        "#});

        let plutus_version = PlutusVersion::default();
        let mut counterexample = expect_failure(&prop, &plutus_version);

        counterexample.simplify();

        assert_eq!(counterexample.choices, vec![149, 252]);
        assert_eq!(reify(counterexample.value), "(149, 252)");
    }

    #[test]
    fn test_prop_enum_bool() {
        let (prop, reify) = property(indoc! { r#"
            test foo(predicate via bool()) {
                predicate
            }
        "#});

        let plutus_version = PlutusVersion::default();
        let mut counterexample = expect_failure(&prop, &plutus_version);

        counterexample.simplify();

        assert_eq!(counterexample.choices, vec![1]);
        assert_eq!(reify(counterexample.value), "False");
    }

    #[test]
    fn test_prop_enum_custom() {
        let (prop, reify) = property(indoc! { r#"
            type Temperature {
                Hot
                Cold
            }

            fn temperature() -> Fuzzer<Temperature> {
                bool() |> map(fn(is_cold) {
                    if is_cold { Cold } else { Hot }
                })
            }

            test foo(t via temperature()) {
                t == Hot
            }
        "#});

        let plutus_version = PlutusVersion::default();
        let mut counterexample = expect_failure(&prop, &plutus_version);

        counterexample.simplify();

        assert_eq!(counterexample.choices, vec![0]);
        assert_eq!(reify(counterexample.value), "Cold");
    }

    #[test]
    fn test_prop_opaque() {
        let (prop, reify) = property(indoc! { r#"
            opaque type Temperature {
                Hot
                Cold
            }

            fn temperature() -> Fuzzer<Temperature> {
                bool() |> map(fn(is_cold) {
                    if is_cold { Cold } else { Hot }
                })
            }

            test foo(t via temperature()) {
                t == Hot
            }
        "#});

        let plutus_version = PlutusVersion::default();
        let mut counterexample = expect_failure(&prop, &plutus_version);

        counterexample.simplify();

        assert_eq!(counterexample.choices, vec![0]);
        assert_eq!(reify(counterexample.value), "Cold");
    }

    #[test]
    fn test_prop_private_enum() {
        let (prop, reify) = property(indoc! { r#"
            type Vehicle {
                Car { wheels: Int }
                Bike { wheels: Int }
            }

            fn vehicle() -> Fuzzer<Vehicle> {
                bool() |> map(fn(is_car) {
                    if is_car { Car(4) } else { Bike(2) }
                })
            }

            test foo(v via vehicle()) {
                when v is {
                    Car { wheels } -> wheels
                    Bike { wheels } -> wheels
                } == 4
            }
        "#});

        let plutus_version = PlutusVersion::default();
        let mut counterexample = expect_failure(&prop, &plutus_version);

        counterexample.simplify();

        assert_eq!(counterexample.choices, vec![1]);
        assert_eq!(reify(counterexample.value), "Bike { wheels: 2 }");
    }

    #[test]
    fn test_prop_list() {
        let (prop, reify) = property(indoc! { r#"
            fn list(elem: Fuzzer<a>) -> Fuzzer<List<a>> {
              bool()
                |> and_then(fn(continue) {
                    if continue {
                      map2(elem, list(elem), fn(head, tail) { [head, ..tail] })
                    } else {
                      constant([])
                    }
                })
            }

            fn length(es: List<a>) -> Int {
              when es is {
                [] -> 0
                [_, ..tail] -> 1 + length(tail)
              }
            }

            test foo(es: List<Int> via list(int())) {
              length(es) < 3
            }
        "#});

        let plutus_version = PlutusVersion::default();

        let mut counterexample = expect_failure(&prop, &plutus_version);

        counterexample.simplify();

        assert_eq!(counterexample.choices, vec![0, 0, 0, 0, 0, 0, 1]);
        assert_eq!(reify(counterexample.value), "[0, 0, 0]");
    }

    #[test]
    fn test_prop_opaque_dict() {
        let (prop, reify) = property(indoc! { r#"
            pub opaque type Dict<a> {
              inner: List<(ByteArray, a)>,
            }

            fn dict(elem: Fuzzer<a>) -> Fuzzer<Dict<a>> {
              bool()
                |> and_then(
                     fn(continue) {
                       if continue {
                         let kv = map2(bytearray(), elem, fn(k, v) { (k, v) })
                         map2(kv, dict(elem), fn(head, tail) { Dict([head, ..tail.inner]) })
                       } else {
                         constant(Dict([]))
                       }
                     },
                   )
            }

            test foo(d via dict(bool())) {
              d == Dict([])
            }
        "#});

        let plutus_version = PlutusVersion::default();
        let mut counterexample = expect_failure(&prop, &plutus_version);

        counterexample.simplify();

        assert_eq!(counterexample.choices, vec![0, 0, 0, 1]);
        assert_eq!(reify(counterexample.value), "Dict([(#\"2cd15ed0\", True)])");
    }

    #[test]
    fn test_prop_opaque_nested_dict() {
        let (prop, reify) = property(indoc! { r#"
            pub opaque type Dict<a> {
              inner: List<(ByteArray, a)>,
            }

            fn dict(elem: Fuzzer<a>) -> Fuzzer<Dict<a>> {
              bool()
                |> and_then(
                     fn(continue) {
                       if continue {
                         let kv = map2(bytearray(), elem, fn(k, v) { (k, v) })
                         map2(kv, dict(elem), fn(head, tail) { Dict([head, ..tail.inner]) })
                       } else {
                         constant(Dict([]))
                       }
                     },
                   )
            }

            test foo(d via dict(dict(int()))) {
              d == Dict([])
            }
        "#});

        let plutus_version = PlutusVersion::default();
        let mut counterexample = expect_failure(&prop, &plutus_version);

        counterexample.simplify();

        assert_eq!(counterexample.choices, vec![0, 0, 1, 1]);
        assert_eq!(
            reify(counterexample.value),
            "Dict([(#\"2cd15ed0\", Dict([]))])"
        );
    }
}
