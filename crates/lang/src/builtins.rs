use std::{cell::RefCell, collections::HashMap, sync::Arc};

use strum::IntoEnumIterator;

use uplc::builtins::DefaultFunction;

use crate::{
    ast::{ModuleKind, Span},
    tipo::{
        fields::FieldMap, Type, TypeConstructor, TypeInfo, TypeVar, ValueConstructor,
        ValueConstructorVariant,
    },
    IdGenerator,
};

pub const BYTE_ARRAY: &str = "ByteArray";
pub const BOOL: &str = "Bool";
pub const INT: &str = "Int";
pub const DATA: &str = "Data";
pub const LIST: &str = "List";
pub const NIL: &str = "Nil";
pub const RESULT: &str = "Result";
pub const STRING: &str = "String";
pub const OPTION: &str = "Option";

/// Build a prelude that can be injected
/// into a compiler pipeline
pub fn prelude(id_gen: &IdGenerator) -> TypeInfo {
    let mut prelude = TypeInfo {
        name: "aiken".to_string(),
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
            location: Span::empty(),
            module: "".to_string(),
            public: true,
        },
    );

    // Data
    prelude.types.insert(
        DATA.to_string(),
        TypeConstructor {
            parameters: vec![],
            tipo: data(),
            location: Span::empty(),
            module: "".to_string(),
            public: true,
        },
    );

    // ByteArray
    prelude.types.insert(
        BYTE_ARRAY.to_string(),
        TypeConstructor {
            location: Span::empty(),
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
                field_map: None::<FieldMap>,
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
                field_map: None::<FieldMap>,
                arity: 0,
                location: Span::empty(),
                constructors_count: 2,
            },
        ),
    );

    prelude.types.insert(
        BOOL.to_string(),
        TypeConstructor {
            location: Span::empty(),
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
            location: Span::empty(),
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
            location: Span::empty(),
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
                field_map: None::<FieldMap>,
                location: Span::empty(),
                constructors_count: 1,
            },
        ),
    );

    prelude.types.insert(
        NIL.to_string(),
        TypeConstructor {
            location: Span::empty(),
            parameters: vec![],
            tipo: nil(),
            module: "".to_string(),
            public: true,
        },
    );

    // Option(value)
    let option_value = generic_var(id_gen.next());

    prelude.types.insert(
        OPTION.to_string(),
        TypeConstructor {
            location: Span::empty(),
            parameters: vec![option_value.clone()],
            tipo: option(option_value),
            module: "".to_string(),
            public: true,
        },
    );

    prelude.types_constructors.insert(
        OPTION.to_string(),
        vec!["Some".to_string(), "None".to_string()],
    );

    let some = generic_var(id_gen.next());

    prelude.values.insert(
        "Some".to_string(),
        ValueConstructor::public(
            function(vec![some.clone()], option(some)),
            ValueConstructorVariant::Record {
                module: "".into(),
                name: "Some".to_string(),
                field_map: None::<FieldMap>,
                arity: 1,
                location: Span::empty(),
                constructors_count: 2,
            },
        ),
    );

    let some = generic_var(id_gen.next());

    prelude.values.insert(
        "None".to_string(),
        ValueConstructor::public(
            option(some),
            ValueConstructorVariant::Record {
                module: "".into(),
                name: "None".to_string(),
                field_map: None::<FieldMap>,
                arity: 0,
                location: Span::empty(),
                constructors_count: 2,
            },
        ),
    );

    prelude
}

pub fn plutus(id_gen: &IdGenerator) -> TypeInfo {
    let mut plutus = TypeInfo {
        name: "aiken/builtin".to_string(),
        package: "".to_string(),
        kind: ModuleKind::Lib,
        types: HashMap::new(),
        types_constructors: HashMap::new(),
        values: HashMap::new(),
        accessors: HashMap::new(),
    };

    for builtin in DefaultFunction::iter() {
        if let Some(value) = from_default_function(builtin, id_gen) {
            plutus.values.insert(builtin.aiken_name(), value);
        }
    }

    plutus
}

pub fn from_default_function(
    builtin: DefaultFunction,
    id_gen: &IdGenerator,
) -> Option<ValueConstructor> {
    let info = match builtin {
        DefaultFunction::AddInteger
        | DefaultFunction::SubtractInteger
        | DefaultFunction::MultiplyInteger
        | DefaultFunction::DivideInteger
        | DefaultFunction::QuotientInteger
        | DefaultFunction::RemainderInteger
        | DefaultFunction::ModInteger => {
            let tipo = function(vec![int(), int()], int());

            Some((tipo, 2))
        }

        DefaultFunction::EqualsInteger
        | DefaultFunction::LessThanInteger
        | DefaultFunction::LessThanEqualsInteger => {
            let tipo = function(vec![int(), int()], bool());

            Some((tipo, 2))
        }
        DefaultFunction::AppendByteString => {
            let tipo = function(vec![byte_array(), byte_array()], byte_array());

            Some((tipo, 2))
        }
        DefaultFunction::ConsByteString => {
            let tipo = function(vec![int(), byte_array()], byte_array());

            Some((tipo, 2))
        }
        DefaultFunction::SliceByteString => {
            let tipo = function(vec![int(), int(), byte_array()], byte_array());

            Some((tipo, 3))
        }
        DefaultFunction::LengthOfByteString => {
            let tipo = function(vec![byte_array()], int());

            Some((tipo, 1))
        }
        DefaultFunction::IndexByteString => {
            let tipo = function(vec![byte_array(), int()], int());

            Some((tipo, 2))
        }
        DefaultFunction::EqualsByteString
        | DefaultFunction::LessThanByteString
        | DefaultFunction::LessThanEqualsByteString => {
            let tipo = function(vec![byte_array(), byte_array()], bool());

            Some((tipo, 2))
        }
        DefaultFunction::Sha2_256 | DefaultFunction::Sha3_256 | DefaultFunction::Blake2b_256 => {
            let tipo = function(vec![byte_array()], byte_array());

            Some((tipo, 1))
        }

        DefaultFunction::VerifyEd25519Signature => {
            let tipo = function(vec![byte_array(), byte_array(), byte_array()], bool());

            Some((tipo, 3))
        }
        DefaultFunction::VerifyEcdsaSecp256k1Signature => None,
        DefaultFunction::VerifySchnorrSecp256k1Signature => None,
        DefaultFunction::AppendString => {
            let tipo = function(vec![string(), string()], string());

            Some((tipo, 2))
        }
        DefaultFunction::EqualsString => {
            let tipo = function(vec![string(), string()], bool());

            Some((tipo, 2))
        }
        DefaultFunction::EncodeUtf8 => {
            let tipo = function(vec![string()], byte_array());

            Some((tipo, 1))
        }
        DefaultFunction::DecodeUtf8 => {
            let tipo = function(vec![byte_array()], string());

            Some((tipo, 1))
        }
        DefaultFunction::IfThenElse => {
            let ret = generic_var(id_gen.next());

            let tipo = function(vec![bool(), ret.clone(), ret.clone()], ret);

            Some((tipo, 3))
        }
        DefaultFunction::ChooseUnit => None,
        DefaultFunction::Trace => {
            let ret = generic_var(id_gen.next());

            let tipo = function(vec![string(), ret.clone()], ret);

            Some((tipo, 2))
        }
        DefaultFunction::FstPair => None,
        DefaultFunction::SndPair => None,
        DefaultFunction::ChooseList => None,
        DefaultFunction::MkCons => None,
        DefaultFunction::HeadList => {
            let ret = generic_var(id_gen.next());

            let tipo = function(vec![list(ret.clone())], ret);

            Some((tipo, 1))
        }
        DefaultFunction::TailList => {
            let ret = list(generic_var(id_gen.next()));

            let tipo = function(vec![ret.clone()], ret);

            Some((tipo, 1))
        }
        DefaultFunction::NullList => {
            let ret = list(generic_var(id_gen.next()));

            let tipo = function(vec![ret], bool());

            Some((tipo, 1))
        }
        DefaultFunction::ChooseData => None,
        DefaultFunction::ConstrData => None,
        DefaultFunction::MapData => None,
        DefaultFunction::ListData => None,
        DefaultFunction::IData => None,
        DefaultFunction::BData => None,
        DefaultFunction::UnConstrData => None,
        DefaultFunction::UnMapData => None,
        DefaultFunction::UnListData => None,
        DefaultFunction::UnIData => None,
        DefaultFunction::UnBData => None,
        DefaultFunction::EqualsData => {
            let tipo = function(vec![data(), data()], bool());

            Some((tipo, 1))
        }
        DefaultFunction::SerialiseData => {
            let tipo = function(vec![data()], byte_array());

            Some((tipo, 1))
        }
        DefaultFunction::MkPairData => None,
        DefaultFunction::MkNilData => None,
        DefaultFunction::MkNilPairData => None,
    };

    info.map(|(tipo, arity)| {
        ValueConstructor::public(
            tipo,
            ValueConstructorVariant::ModuleFn {
                name: builtin.aiken_name(),
                field_map: None,
                module: "".to_string(),
                arity,
                location: Span::empty(),
                builtin: Some(builtin),
            },
        )
    })
}

pub fn int() -> Arc<Type> {
    Arc::new(Type::App {
        public: true,
        name: INT.to_string(),
        module: "".to_string(),
        args: vec![],
    })
}

pub fn data() -> Arc<Type> {
    Arc::new(Type::App {
        public: true,
        name: DATA.to_string(),
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

pub fn tuple(elems: Vec<Arc<Type>>) -> Arc<Type> {
    Arc::new(Type::Tuple { elems })
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

pub fn option(a: Arc<Type>) -> Arc<Type> {
    Arc::new(Type::App {
        public: true,
        name: OPTION.to_string(),
        module: "".to_string(),
        args: vec![a],
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
