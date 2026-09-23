use super::*;

#[test]
fn unsafe_coerce_renamed_import_and_pipeline() {
    assert!(
        check(parse(
            r#"
        use aiken/builtin.{unsafe_coerce as coerce}
        pub opaque type Wrapped { inner: Int }
        pub fn wrap(x: Int) -> Wrapped { x |> coerce }
    "#
        ))
        .is_ok()
    );
}

#[test]
fn unsafe_coerce_rejects_unconstrained_alias() {
    let result = check(parse(
        r#"
        use aiken/builtin
        pub fn coercer() {
            let cast = builtin.unsafe_coerce
            cast
        }
    "#,
    ));
    assert!(matches!(result, Err((_, Error::UnsafeCoercion { .. }))));
}

#[test]
fn unsafe_coerce_rejects_unrelated_nominal_records() {
    let result = check(parse(
        r#"
        use aiken/builtin
        pub type Left { inner: Int }
        pub type Right { inner: Int }
        pub fn cast(x: Left) -> Right { builtin.unsafe_coerce(x) }
    "#,
    ));
    assert!(matches!(result, Err((_, Error::UnsafeCoercion { .. }))));
}

#[test]
fn unsafe_coerce_rejects_incompatible_higher_order_use() {
    let result = check(parse(
        r#"
        use aiken/builtin
        pub fn call(f: fn(Int) -> ByteArray) -> ByteArray { f(1) }
        pub fn bad() -> ByteArray { call(builtin.unsafe_coerce) }
    "#,
    ));
    assert!(matches!(result, Err((_, Error::UnsafeCoercion { .. }))));
}

#[test]
fn unsafe_coerce_local_wrapper() {
    assert!(
        check(parse(
            r#"
        use aiken/builtin
        pub opaque type Wrapped { inner: Int }
        pub fn wrap(x: Int) -> Wrapped { builtin.unsafe_coerce(x) }
    "#
        ))
        .is_ok()
    );
}

#[test]
fn unsafe_coerce_imported_nested_value() {
    let dict = parse_as(
        r#"
        pub opaque type Dict<key, value> { inner: Pairs<key, value> }
    "#,
        "dict",
    );
    let assets = parse_as(
        r#"
        use dict.{Dict}
        pub opaque type AssetValue { inner: Dict<ByteArray, Dict<ByteArray, Int>> }
    "#,
        "assets",
    );
    let consumer = parse(
        r#"
        use aiken/builtin
        use assets.{AssetValue}
        pub fn unsafe_coerce_value(xs: Pairs<ByteArray, Pairs<ByteArray, Int>>) -> AssetValue {
            builtin.unsafe_coerce(xs)
        }
    "#,
    );
    assert!(check_with_deps(consumer, vec![dict, assets]).is_ok());
}

#[test]
fn unsafe_coerce_generic_wrapper_and_nested_instantiation() {
    assert!(
        check(parse(
            r#"
        use aiken/builtin.{unsafe_coerce}
        pub opaque type Wrapped<a> { inner: a }
        pub fn wrap(x: a) -> Wrapped<a> { unsafe_coerce(x) }
        pub fn twice(x: Int) -> Wrapped<Wrapped<Int>> { unsafe_coerce(x) }
    "#
        ))
        .is_ok()
    );
}

#[test]
fn unsafe_coerce_annotated_function_alias() {
    assert!(
        check(parse(
            r#"
        use aiken/builtin
        pub opaque type Wrapped { inner: Int }
        pub fn wrap(x: Int) -> Wrapped {
            let coerce: fn(Int) -> Wrapped = builtin.unsafe_coerce
            coerce(x)
        }
    "#
        ))
        .is_ok()
    );
}

#[test]
fn unsafe_coerce_before_forward_reference() {
    assert!(
        check(parse(
            r#"
        use aiken/builtin
        pub opaque type Wrapped { inner: Int }
        pub fn wrap(x: Int) -> Wrapped {
            let value: Wrapped = builtin.unsafe_coerce(x)
            finish(value)
        }
        fn finish(x: Wrapped) -> Wrapped { x }
    "#
        ))
        .is_ok()
    );
}

#[test]
fn unsafe_coerce_rejects_incompatible_and_unresolved_types() {
    for source in [
        "pub fn cast(x: Int) -> ByteArray { builtin.unsafe_coerce(x) }",
        "pub fn cast(x: Data) -> Wrapped<Int> { builtin.unsafe_coerce(x) }",
        "pub fn cast(x: List<Int>) -> List<ByteArray> { builtin.unsafe_coerce(x) }",
        "pub fn cast(x: a) -> b { builtin.unsafe_coerce(x) }",
        "pub fn cast() { builtin.unsafe_coerce }",
        "pub fn cast(x: fn(Int) -> Int) -> fn(Int) -> Int { builtin.unsafe_coerce(x) }",
        "pub fn cast(x: Int) -> Regular { builtin.unsafe_coerce(x) }",
        "pub fn cast(x: Int) -> Multi { builtin.unsafe_coerce(x) }",
        "pub fn cast(x: Wrapped<Int>) -> Wrapped<ByteArray> { builtin.unsafe_coerce(x) }",
    ] {
        let result = check(parse(&format!(
            r#"
            use aiken/builtin
            pub opaque type Wrapped<a> {{ inner: a }}
            pub type Regular {{ inner: Int }}
            pub opaque type Multi {{ left: Int, right: Int }}
            {source}
        "#
        )));
        assert!(
            matches!(result, Err((_, Error::UnsafeCoercion { .. }))),
            "{source}: {result:?}"
        );
    }
}

#[test]
fn unsafe_coerce_does_not_relax_expect() {
    let result = check(parse(
        r#"
        use aiken/builtin
        pub opaque type Wrapped { inner: Int }
        pub fn cast(x: Data) -> Wrapped {
            expect value: Wrapped = x
            value
        }
    "#,
    ));
    assert!(matches!(result, Err((_, Error::ExpectOnOpaqueType { .. }))));
}

#[test]
fn unsafe_coerce_rejects_decorated_wrapper() {
    let result = check(parse(
        r#"
        use aiken/builtin
        @list
        pub opaque type Wrapped { inner: Int }
        pub fn cast(x: Int) -> Wrapped { builtin.unsafe_coerce(x) }
    "#,
    ));
    assert!(matches!(result, Err((_, Error::UnsafeCoercion { .. }))));
}

#[test]
fn unsafe_coerce_metadata_agrees_with_backend_erasure() {
    use crate::{
        ast::DataTypeKey,
        tipo::{check_replaceable_opaque_type, coercion, convert_opaque_type},
    };
    use indexmap::IndexMap;

    let (_, module) = check(parse(
        r#"
        pub opaque type Wrapped<a> { inner: a }
        pub opaque type Nested<a> { inner: Wrapped<Wrapped<a>> }
        pub opaque type Multi { first: Int, second: Int }
        pub opaque type Choice { First(Int) Second(Int) }
        pub type Regular { inner: Int }
        @list
        pub opaque type Listed { inner: Int }
        @tag(42)
        pub opaque type Tagged { inner: Int }
    "#,
    ))
    .unwrap();
    let definitions = module
        .definitions
        .iter()
        .filter_map(|def| match def {
            Definition::DataType(data) => Some((
                DataTypeKey {
                    module_name: module.name.clone(),
                    defined_type: data.name.clone(),
                },
                data.clone(),
            )),
            _ => None,
        })
        .collect::<IndexMap<_, _>>();
    let data_types = definitions.iter().collect::<IndexMap<_, _>>();
    for (name, constructor) in &module.type_info.types {
        let tipo = &constructor.tipo;
        let erasable = matches!(name.as_str(), "Wrapped" | "Nested");
        assert_eq!(
            check_replaceable_opaque_type(tipo, &data_types),
            erasable,
            "{name}"
        );
        assert_eq!(
            module.type_info.opaque_representations.contains_key(name),
            erasable,
            "{name}"
        );
        let erased = convert_opaque_type(tipo, &data_types, true);
        assert!(
            coercion::compatible(
                tipo,
                &erased,
                &module.name,
                &module.type_info.opaque_representations,
                &HashMap::new()
            ),
            "{name}"
        );
    }
}
