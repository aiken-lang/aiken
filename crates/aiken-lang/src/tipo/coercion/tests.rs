use super::*;
use std::cell::RefCell;

fn app(name: &str, args: Vec<Rc<Type>>) -> Rc<Type> {
    Type::App {
        public: true,
        module: "test".to_string(),
        name: name.to_string(),
        args,
        alias: None,
    }
    .into()
}

fn linked(tipo: Rc<Type>) -> Rc<Type> {
    Type::Var {
        tipo: Rc::new(RefCell::new(TypeVar::Link { tipo })),
        alias: None,
    }
    .into()
}

#[test]
fn unsafe_coerce_budget_includes_substitution_and_repeated_parameters() {
    let parameter = Type::generic_var(0);
    let wrappers = HashMap::from([(
        "Twice".to_string(),
        OpaqueRepresentation {
            parameters: vec![parameter.clone()],
            inner: Type::pair(parameter.clone(), parameter),
        },
    )]);
    let proof = Proof {
        current_module: "test",
        local: &wrappers,
        modules: &HashMap::new(),
    };
    let input = app("Twice", vec![Type::int()]);
    // App, parameter binding, Pair, and two (parameter, Int) visits.
    let mut nodes = 7;
    assert!(
        proof
            .representation(&input, &Bindings::new(), 4, &mut nodes)
            .is_some()
    );
    assert_eq!(nodes, 0);
    assert!(
        proof
            .representation(&input, &Bindings::new(), 4, &mut 6)
            .is_none()
    );
    assert!(
        proof
            .representation(&input, &Bindings::new(), 3, &mut 7)
            .is_none()
    );
}

#[test]
fn unsafe_coerce_budget_includes_type_links_and_parameter_links() {
    let parameter = Type::generic_var(0);
    let wrappers = HashMap::from([(
        "Wrapped".to_string(),
        OpaqueRepresentation {
            parameters: vec![linked(parameter.clone())],
            inner: linked(parameter),
        },
    )]);
    let proof = Proof {
        current_module: "test",
        local: &wrappers,
        modules: &HashMap::new(),
    };
    let input = app("Wrapped", vec![linked(Type::int())]);
    // App, two parameter-binding visits, two body visits, and Link -> Int.
    let mut nodes = 7;
    assert!(
        proof
            .representation(&input, &Bindings::new(), 5, &mut nodes)
            .is_some()
    );
    assert_eq!(nodes, 0);
    assert!(
        proof
            .representation(&input, &Bindings::new(), 5, &mut 6)
            .is_none()
    );
    assert!(
        proof
            .representation(&input, &Bindings::new(), 4, &mut 7)
            .is_none()
    );
}

#[test]
fn unsafe_coerce_nested_substitutions_keep_their_callers_scope() {
    let wrappers = HashMap::from([(
        "Wrapped".to_string(),
        OpaqueRepresentation {
            parameters: vec![Type::generic_var(0)],
            inner: Type::generic_var(0),
        },
    )]);
    let nested = app("Wrapped", vec![app("Wrapped", vec![Type::int()])]);
    assert!(compatible(
        &nested,
        &Type::int(),
        "test",
        &wrappers,
        &HashMap::new()
    ));
    assert!(!compatible(
        &nested,
        &Type::byte_array(),
        "test",
        &wrappers,
        &HashMap::new()
    ));
}

#[test]
fn unsafe_coerce_budget_stops_recursive_metadata() {
    let recursive = app("Recursive", vec![]);
    let wrappers = HashMap::from([(
        "Recursive".to_string(),
        OpaqueRepresentation {
            parameters: vec![],
            inner: recursive.clone(),
        },
    )]);
    let proof = Proof {
        current_module: "test",
        local: &wrappers,
        modules: &HashMap::new(),
    };
    assert!(
        proof
            .representation(&recursive, &Bindings::new(), 8, &mut 32)
            .is_none()
    );
    assert!(
        proof
            .representation(&recursive, &Bindings::new(), 32, &mut 8)
            .is_none()
    );
}

#[test]
fn unsafe_coerce_budget_covers_parameter_binding_and_wide_types() {
    let wrappers = HashMap::from([(
        "Phantom".to_string(),
        OpaqueRepresentation {
            parameters: (0..8).map(Type::generic_var).collect(),
            inner: Type::int(),
        },
    )]);
    let proof = Proof {
        current_module: "test",
        local: &wrappers,
        modules: &HashMap::new(),
    };
    let phantom = app("Phantom", vec![Type::int(); 8]);
    assert!(
        proof
            .representation(&phantom, &Bindings::new(), 4, &mut 10)
            .is_some()
    );
    assert!(
        proof
            .representation(&phantom, &Bindings::new(), 4, &mut 9)
            .is_none()
    );
    let wide = Type::tuple(vec![Type::int(); 8]);
    assert!(
        proof
            .representation(&wide, &Bindings::new(), 2, &mut 9)
            .is_some()
    );
    assert!(
        proof
            .representation(&wide, &Bindings::new(), 2, &mut 8)
            .is_none()
    );
}
