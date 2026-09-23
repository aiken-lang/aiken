use super::*;
use pretty_assertions::assert_eq;

fn compile_function(
    dependencies: &[(&str, &str)],
    source: &str,
    tracing: Tracing,
) -> Program<DeBruijn> {
    let mut project = TestProject::new();
    for (name, code) in dependencies {
        let mut dependency = project.parse(code);
        dependency.name = name.to_string();
        dependency.ast.name = dependency.name.clone();
        let checked = project.check(dependency);
        // Exercise the public module interface after serialization, not just
        // shared in-memory type-variable references from inference.
        let restored = crate::module::CheckedModule::from_cbor(&checked.to_cbor()).unwrap();
        project
            .module_types
            .insert(name.to_string(), restored.ast.type_info);
    }
    let module = project.check(project.parse(source));
    let function = module
        .ast
        .definitions()
        .find_map(|definition| match definition {
            Definition::Fn(function) if function.name == "observe" => Some(function),
            _ => None,
        })
        .unwrap();
    project
        .new_generator(tracing)
        .generate_raw(&function.body, &function.arguments, &module.name)
        .unwrap()
        .try_into()
        .unwrap()
}

fn compile_coercion(body: &str, tracing: Tracing) -> Program<DeBruijn> {
    compile_function(
        &[(
            "assets",
            r#"
        pub opaque type Dict<key, value> { inner: Pairs<key, value> }
        pub opaque type AssetValue {
            inner: Dict<ByteArray, Dict<ByteArray, Int>>
        }
    "#,
        )],
        &format!(
            r#"
        use aiken/builtin
        use assets.{{AssetValue}}
        pub fn observe(xs: Pairs<ByteArray, Pairs<ByteArray, Int>>) -> Data {{
            {body}
        }}
    "#
        ),
        tracing,
    )
}

#[test]
fn unsafe_coerce_dynamic_nested_value_is_zero_cost() {
    for tracing in [
        Tracing::All(TraceLevel::Silent),
        Tracing::All(TraceLevel::Verbose),
    ] {
        let baseline = compile_coercion("as_data(xs)", tracing);
        let coerced = compile_coercion(
            r#"
            let value: AssetValue = builtin.unsafe_coerce(xs)
            let pairs: Pairs<ByteArray, Pairs<ByteArray, Int>> = builtin.unsafe_coerce(value)
            as_data(pairs)
        "#,
            tracing,
        );
        assert_eq!(baseline.to_flat().unwrap(), coerced.to_flat().unwrap());

        for size in [0, 1, 8, 64] {
            let data = Data::map(
                (0..size)
                    .map(|index| {
                        (
                            Data::bytestring(vec![index]),
                            Data::map(vec![
                                (Data::bytestring(b"alpha".to_vec()), Data::integer(5.into())),
                                (Data::bytestring(b"beta".to_vec()), Data::integer(7.into())),
                            ]),
                        )
                    })
                    .collect(),
            );
            let expected = baseline.apply_data(data.clone()).eval(ExBudget::default());
            let actual = coerced.apply_data(data).eval(ExBudget::default());
            assert!(actual.result().is_ok());
            assert_eq!(expected.result().unwrap(), actual.result().unwrap());
            assert_eq!(expected.cost(), actual.cost());
        }
    }
}

#[test]
fn unsafe_coerce_annotated_alias_is_zero_cost() {
    let tracing = Tracing::All(TraceLevel::Silent);
    let baseline = compile_coercion("as_data(xs)", tracing);
    let coerced = compile_coercion(
        r#"
        let cast: fn(Pairs<ByteArray, Pairs<ByteArray, Int>>) -> AssetValue = builtin.unsafe_coerce
        as_data(cast(xs))
    "#,
        tracing,
    );
    assert_eq!(baseline.to_flat().unwrap(), coerced.to_flat().unwrap());
}

const WRAPPERS: &str = r#"
    pub opaque type Wrapped<a> { inner: a }
    pub opaque type Swapped<a, b> { inner: Pair<b, a> }
    pub type Record<a> { payload: a }
    pub fn unwrap(x: Wrapped<a>) -> a { x.inner }
    pub fn unwrap_swapped(x: Swapped<a, b>) -> Pair<b, a> { x.inner }
"#;

const COERCIONS: &str = r#"
    use aiken/builtin
    use wrappers.{Wrapped, Swapped}
    pub fn wrap(x: a) -> Wrapped<a> { builtin.unsafe_coerce(x) }
    pub fn unwrap(x: Wrapped<a>) -> a { builtin.unsafe_coerce(x) }
    pub fn swap(x: Pair<b, a>) -> Swapped<a, b> { builtin.unsafe_coerce(x) }
"#;

fn compile_observer(input_type: &str, body: &str, tracing: Tracing) -> Program<DeBruijn> {
    compile_function(
        &[("wrappers", WRAPPERS), ("coercions", COERCIONS)],
        &format!(
            r#"
            use aiken/builtin
            use wrappers.{{Wrapped, Record}}
            use coercions
            pub fn observe(xs: {input_type}) -> Data {{ {body} }}
        "#
        ),
        tracing,
    )
}

fn assert_same_execution(
    baseline: &Program<DeBruijn>,
    coerced: &Program<DeBruijn>,
    input: pallas_primitives::PlutusData,
) {
    let expected = baseline.apply_data(input.clone()).eval(ExBudget::default());
    let actual = coerced.apply_data(input).eval(ExBudget::default());
    assert!(expected.result().is_ok());
    assert_eq!(expected.result().unwrap(), actual.result().unwrap());
    assert_eq!(expected.cost(), actual.cost());
}

#[test]
fn unsafe_coerce_generic_imports_preserve_dynamic_representations() {
    let integer = Data::integer(42.into());
    let bytes = Data::bytestring(b"alpha".to_vec());
    let cases = [
        (
            "Int",
            vec![
                Data::integer((-17).into()),
                Data::integer(0.into()),
                integer.clone(),
            ],
        ),
        ("ByteArray", vec![Data::bytestring(vec![]), bytes.clone()]),
        (
            "Bool",
            vec![Data::constr(0, vec![]), Data::constr(1, vec![])],
        ),
        ("Void", vec![Data::constr(0, vec![])]),
        (
            "Data",
            vec![
                integer.clone(),
                Data::map(vec![]),
                Data::constr(5, vec![bytes.clone()]),
            ],
        ),
        (
            "List<Int>",
            vec![
                Data::list(vec![]),
                Data::list(vec![integer.clone(), Data::integer((-1).into())]),
            ],
        ),
        (
            "Pair<Int, ByteArray>",
            vec![Data::list(vec![integer.clone(), bytes.clone()])],
        ),
        (
            "(Int, ByteArray, Bool)",
            vec![Data::list(vec![
                integer.clone(),
                bytes.clone(),
                Data::constr(1, vec![]),
            ])],
        ),
        (
            "Option<Int>",
            vec![
                Data::constr(0, vec![integer.clone()]),
                Data::constr(1, vec![]),
            ],
        ),
        ("Record<Int>", vec![Data::constr(0, vec![integer])]),
    ];
    for tracing in [Tracing::silent(), Tracing::verbose()] {
        for (tipo, inputs) in &cases {
            let baseline = compile_observer(tipo, "as_data(xs)", tracing);
            // The defining module's ordinary accessor consumes the result of a
            // generic coercion imported through a second module.
            let coerced = compile_observer(
                tipo,
                "as_data(wrappers.unwrap(coercions.wrap(xs)))",
                tracing,
            );
            assert_eq!(
                baseline.to_flat().unwrap(),
                coerced.to_flat().unwrap(),
                "{tipo}"
            );
            for input in inputs {
                assert_same_execution(&baseline, &coerced, input.clone());
            }
        }
    }
}

#[test]
fn unsafe_coerce_inside_containers_preserves_access_and_pattern_matching() {
    let integer = Data::integer(42.into());
    let bytes = Data::bytestring(b"beta".to_vec());
    let cases = [
        (
            "List<Int>",
            "List<Wrapped<Int>>",
            "when xs is { [] -> as_data(0) [first, ..] -> as_data(first) }",
            "when ys is { [] -> as_data(0) [first, ..] -> as_data(wrappers.unwrap(first)) }",
            vec![Data::list(vec![]), Data::list(vec![integer.clone()])],
        ),
        (
            "Pair<Int, ByteArray>",
            "Pair<Wrapped<Int>, Wrapped<ByteArray>>",
            "let Pair(a, b) = xs as_data((a, b))",
            "let Pair(a, b) = ys as_data((wrappers.unwrap(a), wrappers.unwrap(b)))",
            vec![Data::list(vec![integer.clone(), bytes.clone()])],
        ),
        (
            "(Int, ByteArray)",
            "(Wrapped<Int>, Wrapped<ByteArray>)",
            "let (a, b) = xs as_data((a, b))",
            "let (a, b) = ys as_data((wrappers.unwrap(a), wrappers.unwrap(b)))",
            vec![Data::list(vec![integer.clone(), bytes])],
        ),
        (
            "Option<Int>",
            "Option<Wrapped<Int>>",
            "when xs is { None -> as_data(0) Some(x) -> as_data(x) }",
            "when ys is { None -> as_data(0) Some(x) -> as_data(wrappers.unwrap(x)) }",
            vec![
                Data::constr(0, vec![integer.clone()]),
                Data::constr(1, vec![]),
            ],
        ),
        (
            "Record<Int>",
            "Record<Wrapped<Int>>",
            "as_data(xs.payload)",
            "as_data(wrappers.unwrap(ys.payload))",
            vec![Data::constr(0, vec![integer])],
        ),
    ];
    for tracing in [Tracing::silent(), Tracing::verbose()] {
        for (source, target, direct, consume, inputs) in &cases {
            let baseline = compile_observer(source, direct, tracing);
            let coerced = compile_observer(
                source,
                &format!("let ys: {target} = builtin.unsafe_coerce(xs) {consume}"),
                tracing,
            );
            assert_eq!(
                baseline.to_flat().unwrap(),
                coerced.to_flat().unwrap(),
                "{source}"
            );
            for input in inputs {
                assert_same_execution(&baseline, &coerced, input.clone());
            }
        }
    }
}

#[test]
fn unsafe_coerce_multiple_specializations_in_one_script_are_zero_cost() {
    for tracing in [Tracing::silent(), Tracing::verbose()] {
        let baseline = compile_observer(
            "Pair<Int, ByteArray>",
            "let Pair(a, b) = xs as_data((a + 1, b))",
            tracing,
        );
        let coerced = compile_observer(
            "Pair<Int, ByteArray>",
            r#"
            let Pair(a, b) = wrappers.unwrap_swapped(coercions.swap(xs))
            let number = coercions.wrap(a)
            let bytes = coercions.wrap(b)
            let nested = coercions.wrap(number)
            as_data((wrappers.unwrap(wrappers.unwrap(nested)) + 1, coercions.unwrap(bytes)))
        "#,
            tracing,
        );
        assert_eq!(baseline.to_flat().unwrap(), coerced.to_flat().unwrap());
        assert_same_execution(
            &baseline,
            &coerced,
            Data::list(vec![
                Data::integer(41.into()),
                Data::bytestring(b"alpha".to_vec()),
            ]),
        );
    }
}

#[test]
fn unsafe_coerce_native_value_uses_its_explicit_data_conversion() {
    for tracing in [Tracing::silent(), Tracing::verbose()] {
        let baseline = compile_observer("Value", "builtin.value_data(xs)", tracing);
        let coerced = compile_observer(
            "Value",
            "builtin.value_data(wrappers.unwrap(coercions.wrap(xs)))",
            tracing,
        );
        assert_eq!(baseline.to_flat().unwrap(), coerced.to_flat().unwrap());
        for input in [
            Data::map(vec![]),
            Data::map(vec![(
                Data::bytestring(vec![]),
                Data::map(vec![(Data::bytestring(vec![]), Data::integer(42.into()))]),
            )]),
        ] {
            assert_same_execution(&baseline, &coerced, input);
        }
    }
}
