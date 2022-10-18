use std::{cell::RefCell, collections::HashMap, sync::Arc};

use crate::{
    ast::{ModuleKind, Span},
    tipo::{self, Type, TypeConstructor, TypeVar, ValueConstructor, ValueConstructorVariant},
    IdGenerator,
};

const BYTE_ARRAY: &str = "ByteArray";
const BOOL: &str = "Bool";
const INT: &str = "Int";
const LIST: &str = "List";
const NIL: &str = "Nil";
const RESULT: &str = "Result";
const STRING: &str = "String";

/// Build a prelude that can be injected
/// into a compiler pipeline
pub fn prelude(id_gen: &IdGenerator) -> tipo::Module {
    let mut prelude = tipo::Module {
        name: vec!["aiken".to_string()],
        package: "".to_string(),
        kind: ModuleKind::Lib,
        types: HashMap::new(),
        types_constructors: HashMap::new(),
        values: HashMap::new(),
        accessors: HashMap::new(),
    };

    // Int
    prelude.types.insert(
        INT.to_string(),
        TypeConstructor {
            parameters: vec![],
            tipo: int(),
            origin: Span::empty(),
            module: "".to_string(),
            public: true,
        },
    );

    // ByteArray
    prelude.types.insert(
        BYTE_ARRAY.to_string(),
        TypeConstructor {
            origin: Span::empty(),
            parameters: vec![],
            tipo: byte_array(),
            module: "".to_string(),
            public: true,
        },
    );

    // Bool
    prelude.types_constructors.insert(
        BOOL.to_string(),
        vec!["True".to_string(), "False".to_string()],
    );

    prelude.values.insert(
        "True".to_string(),
        ValueConstructor::public(
            bool(),
            ValueConstructorVariant::Record {
                module: "".into(),
                name: "True".to_string(),
                field_map: None,
                arity: 0,
                location: Span::empty(),
                constructors_count: 2,
            },
        ),
    );

    prelude.values.insert(
        "False".to_string(),
        ValueConstructor::public(
            bool(),
            ValueConstructorVariant::Record {
                module: "".into(),
                name: "False".to_string(),
                field_map: None,
                arity: 0,
                location: Span::empty(),
                constructors_count: 2,
            },
        ),
    );

    prelude.types.insert(
        BOOL.to_string(),
        TypeConstructor {
            origin: Span::empty(),
            parameters: vec![],
            tipo: bool(),
            module: "".to_string(),
            public: true,
        },
    );

    // List(a)
    let list_parameter = generic_var(id_gen.next());
    prelude.types.insert(
        LIST.to_string(),
        TypeConstructor {
            origin: Span::empty(),
            parameters: vec![list_parameter.clone()],
            tipo: list(list_parameter),
            module: "".to_string(),
            public: true,
        },
    );

    // String
    prelude.types.insert(
        STRING.to_string(),
        TypeConstructor {
            origin: Span::empty(),
            parameters: vec![],
            tipo: string(),
            module: "".to_string(),
            public: true,
        },
    );

    // Nil
    prelude.values.insert(
        NIL.to_string(),
        ValueConstructor::public(
            nil(),
            ValueConstructorVariant::Record {
                module: "".into(),
                name: NIL.to_string(),
                arity: 0,
                field_map: None,
                location: Span::empty(),
                constructors_count: 1,
            },
        ),
    );

    prelude.types.insert(
        NIL.to_string(),
        TypeConstructor {
            origin: Span::empty(),
            parameters: vec![],
            tipo: nil(),
            module: "".to_string(),
            public: true,
        },
    );

    // Result(value, error)
    let result_value = generic_var(id_gen.next());
    let result_error = generic_var(id_gen.next());

    prelude.types.insert(
        RESULT.to_string(),
        TypeConstructor {
            origin: Span::empty(),
            parameters: vec![result_value.clone(), result_error.clone()],
            tipo: result(result_value, result_error),
            module: "".to_string(),
            public: true,
        },
    );

    prelude.types_constructors.insert(
        RESULT.to_string(),
        vec!["Ok".to_string(), "Err".to_string()],
    );

    let ok = generic_var(id_gen.next());
    let error = generic_var(id_gen.next());
    let _ = prelude.values.insert(
        "Ok".to_string(),
        ValueConstructor::public(
            function(vec![ok.clone()], result(ok, error)),
            ValueConstructorVariant::Record {
                module: "".into(),
                name: "Ok".to_string(),
                field_map: None,
                arity: 1,
                location: Span::empty(),
                constructors_count: 2,
            },
        ),
    );

    let ok = generic_var(id_gen.next());
    let error = generic_var(id_gen.next());
    let _ = prelude.values.insert(
        "Error".to_string(),
        ValueConstructor::public(
            function(vec![error.clone()], result(ok, error)),
            ValueConstructorVariant::Record {
                module: "".into(),
                name: "Error".to_string(),
                field_map: None,
                arity: 1,
                location: Span::empty(),
                constructors_count: 2,
            },
        ),
    );

    prelude
}

pub fn int() -> Arc<Type> {
    Arc::new(Type::App {
        public: true,
        name: INT.to_string(),
        module: "".to_string(),
        args: vec![],
    })
}

pub fn byte_array() -> Arc<Type> {
    Arc::new(Type::App {
        args: vec![],
        public: true,
        name: BYTE_ARRAY.to_string(),
        module: "".to_string(),
    })
}

pub fn bool() -> Arc<Type> {
    Arc::new(Type::App {
        args: vec![],
        public: true,
        name: BOOL.to_string(),
        module: "".to_string(),
    })
}

pub fn list(t: Arc<Type>) -> Arc<Type> {
    Arc::new(Type::App {
        public: true,
        name: LIST.to_string(),
        module: "".to_string(),
        args: vec![t],
    })
}

pub fn string() -> Arc<Type> {
    Arc::new(Type::App {
        args: vec![],
        public: true,
        name: STRING.to_string(),
        module: "".to_string(),
    })
}

pub fn nil() -> Arc<Type> {
    Arc::new(Type::App {
        args: vec![],
        public: true,
        name: NIL.to_string(),
        module: "".to_string(),
    })
}

pub fn result(a: Arc<Type>, e: Arc<Type>) -> Arc<Type> {
    Arc::new(Type::App {
        public: true,
        name: RESULT.to_string(),
        module: "".to_string(),
        args: vec![a, e],
    })
}

pub fn function(args: Vec<Arc<Type>>, ret: Arc<Type>) -> Arc<Type> {
    Arc::new(Type::Fn { ret, args })
}

pub fn generic_var(id: u64) -> Arc<Type> {
    let tipo = Arc::new(RefCell::new(TypeVar::Generic { id }));

    Arc::new(Type::Var { tipo })
}

pub fn unbound_var(id: u64) -> Arc<Type> {
    let tipo = Arc::new(RefCell::new(TypeVar::Unbound { id }));

    Arc::new(Type::Var { tipo })
}
