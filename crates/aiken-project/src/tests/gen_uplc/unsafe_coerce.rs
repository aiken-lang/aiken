use super::*;
use pretty_assertions::assert_eq;

fn compile_coercion(body: &str, tracing: Tracing) -> Program<DeBruijn> {
    let mut project = TestProject::new();
    let mut dependency = project.parse(
        r#"
        pub opaque type Dict<key, value> { inner: Pairs<key, value> }
        pub opaque type AssetValue {
            inner: Dict<ByteArray, Dict<ByteArray, Int>>
        }
    "#,
    );
    dependency.name = "assets".to_string();
    dependency.ast.name = dependency.name.clone();
    project.check(dependency);
    let module = project.check(project.parse(&format!(
        r#"
        use aiken/builtin
        use assets.{{AssetValue}}
        pub fn roundtrip(xs: Pairs<ByteArray, Pairs<ByteArray, Int>>) -> Data {{
            {body}
        }}
    "#
    )));
    let function = module
        .ast
        .definitions()
        .find_map(|definition| match definition {
            Definition::Fn(function) if function.name == "roundtrip" => Some(function),
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
