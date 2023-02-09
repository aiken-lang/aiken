use crate::{
    ast::{Arg, ArgName, CallArg, Function, ModuleKind, Span, TypedDataType, TypedFunction, UnOp},
    builder::{DataTypeKey, FunctionAccessKey},
    expr::TypedExpr,
    tipo::{
        fields::FieldMap, Type, TypeConstructor, TypeInfo, TypeVar, ValueConstructor,
        ValueConstructorVariant,
    },
    IdGenerator,
};
use indexmap::IndexMap;
use std::{cell::RefCell, collections::HashMap, sync::Arc};
use strum::IntoEnumIterator;
use uplc::builtins::DefaultFunction;

pub const BYTE_ARRAY: &str = "ByteArray";
pub const BOOL: &str = "Bool";
pub const INT: &str = "Int";
pub const DATA: &str = "Data";
pub const LIST: &str = "List";
pub const VOID: &str = "Void";
pub const RESULT: &str = "Result";
pub const STRING: &str = "String";
pub const OPTION: &str = "Option";
pub const ORDERING: &str = "Ordering";

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

    // Ordering
    prelude.types_constructors.insert(
        ORDERING.to_string(),
        vec![
            "Less".to_string(),
            "Equal".to_string(),
            "Greater".to_string(),
        ],
    );

    prelude.values.insert(
        "Less".to_string(),
        ValueConstructor::public(
            ordering(),
            ValueConstructorVariant::Record {
                module: "".into(),
                name: "Less".to_string(),
                field_map: None::<FieldMap>,
                arity: 0,
                location: Span::empty(),
                constructors_count: 3,
            },
        ),
    );

    prelude.values.insert(
        "Equal".to_string(),
        ValueConstructor::public(
            ordering(),
            ValueConstructorVariant::Record {
                module: "".into(),
                name: "Equal".to_string(),
                field_map: None::<FieldMap>,
                arity: 0,
                location: Span::empty(),
                constructors_count: 3,
            },
        ),
    );

    prelude.values.insert(
        "Greater".to_string(),
        ValueConstructor::public(
            ordering(),
            ValueConstructorVariant::Record {
                module: "".into(),
                name: "Greater".to_string(),
                field_map: None::<FieldMap>,
                arity: 0,
                location: Span::empty(),
                constructors_count: 3,
            },
        ),
    );

    prelude.types.insert(
        ORDERING.to_string(),
        TypeConstructor {
            location: Span::empty(),
            parameters: vec![],
            tipo: ordering(),
            module: "".to_string(),
            public: true,
        },
    );

    // not
    prelude.values.insert(
        "not".to_string(),
        ValueConstructor::public(
            function(vec![bool()], bool()),
            ValueConstructorVariant::ModuleFn {
                name: "not".to_string(),
                field_map: None,
                module: "".to_string(),
                arity: 1,
                location: Span::empty(),
                builtin: None,
            },
        ),
    );

    // identity
    let identity_var = generic_var(id_gen.next());
    prelude.values.insert(
        "identity".to_string(),
        ValueConstructor::public(
            function(vec![identity_var.clone()], identity_var),
            ValueConstructorVariant::ModuleFn {
                name: "identity".to_string(),
                field_map: None,
                module: "".to_string(),
                arity: 1,
                location: Span::empty(),
                builtin: None,
            },
        ),
    );

    // always
    let always_a_var = generic_var(id_gen.next());
    let always_b_var = generic_var(id_gen.next());
    prelude.values.insert(
        "always".to_string(),
        ValueConstructor::public(
            function(vec![always_a_var.clone(), always_b_var], always_a_var),
            ValueConstructorVariant::ModuleFn {
                name: "always".to_string(),
                field_map: None,
                module: "".to_string(),
                arity: 2,
                location: Span::empty(),
                builtin: None,
            },
        ),
    );

    // flip
    let flip_a_var = generic_var(id_gen.next());
    let flip_b_var = generic_var(id_gen.next());
    let flip_c_var = generic_var(id_gen.next());

    let input_type = function(
        vec![flip_a_var.clone(), flip_b_var.clone()],
        flip_c_var.clone(),
    );
    let return_type = function(vec![flip_b_var, flip_a_var], flip_c_var);

    prelude.values.insert(
        "flip".to_string(),
        ValueConstructor::public(
            function(vec![input_type], return_type),
            ValueConstructorVariant::ModuleFn {
                name: "flip".to_string(),
                field_map: None,
                module: "".to_string(),
                arity: 1,
                location: Span::empty(),
                builtin: None,
            },
        ),
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

    // Void
    prelude.values.insert(
        VOID.to_string(),
        ValueConstructor::public(
            void(),
            ValueConstructorVariant::Record {
                module: "".into(),
                name: VOID.to_string(),
                arity: 0,
                field_map: None::<FieldMap>,
                location: Span::empty(),
                constructors_count: 1,
            },
        ),
    );

    prelude.types.insert(
        VOID.to_string(),
        TypeConstructor {
            location: Span::empty(),
            parameters: vec![],
            tipo: void(),
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

        DefaultFunction::VerifyEcdsaSecp256k1Signature => {
            let tipo = function(vec![byte_array(), byte_array(), byte_array()], bool());

            Some((tipo, 3))
        }
        DefaultFunction::VerifySchnorrSecp256k1Signature => {
            let tipo = function(vec![byte_array(), byte_array(), byte_array()], bool());

            Some((tipo, 3))
        }

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
        DefaultFunction::ConstrData => {
            let tipo = function(vec![int(), list(data())], data());

            Some((tipo, 2))
        }
        DefaultFunction::MapData => {
            let tipo = function(vec![list(tuple(vec![data(), data()]))], data());

            Some((tipo, 1))
        }
        DefaultFunction::ListData => {
            let tipo = function(vec![list(data())], data());

            Some((tipo, 1))
        }
        DefaultFunction::IData => {
            let tipo = function(vec![int()], data());

            Some((tipo, 1))
        }
        DefaultFunction::BData => {
            let tipo = function(vec![byte_array()], data());

            Some((tipo, 1))
        }
        DefaultFunction::UnConstrData => {
            let tipo = function(vec![data()], tuple(vec![int(), list(data())]));

            Some((tipo, 1))
        }
        DefaultFunction::UnMapData => {
            let tipo = function(vec![data()], list(tuple(vec![data(), data()])));

            Some((tipo, 1))
        }
        DefaultFunction::UnListData => {
            let tipo = function(vec![data()], list(data()));

            Some((tipo, 1))
        }
        DefaultFunction::UnIData => {
            let tipo = function(vec![data()], int());

            Some((tipo, 1))
        }
        DefaultFunction::UnBData => {
            let tipo = function(vec![data()], byte_array());

            Some((tipo, 1))
        }
        DefaultFunction::EqualsData => {
            let tipo = function(vec![data(), data()], bool());

            Some((tipo, 2))
        }
        DefaultFunction::SerialiseData => {
            let tipo = function(vec![data()], byte_array());

            Some((tipo, 1))
        }
        // Anything below has a direct syntax equivalent in Aiken, so
        // there's no need to support builtin for those.
        DefaultFunction::ChooseData => None,
        DefaultFunction::MkPairData => None,
        DefaultFunction::MkNilData => None,
        DefaultFunction::MkNilPairData => None,
        DefaultFunction::ChooseUnit => None,
        DefaultFunction::Trace => None,
        DefaultFunction::FstPair => None,
        DefaultFunction::SndPair => None,
        DefaultFunction::ChooseList => None,
        DefaultFunction::MkCons => None,
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

pub fn prelude_functions(id_gen: &IdGenerator) -> IndexMap<FunctionAccessKey, TypedFunction> {
    let mut functions = IndexMap::new();

    // /// Negate the argument. Useful for map/fold and pipelines.
    // pub fn not(self: Bool) -> Bool {
    //   !self
    // }
    functions.insert(
        FunctionAccessKey {
            module_name: "".to_string(),
            function_name: "not".to_string(),
            variant_name: "".to_string(),
        },
        Function {
            arguments: vec![Arg {
                arg_name: ArgName::Named {
                    name: "self".to_string(),
                    label: "self".to_string(),
                    location: Span::empty(),
                },
                location: Span::empty(),
                annotation: None,
                tipo: bool(),
            }],
            doc: None,
            location: Span::empty(),
            name: "not".to_string(),
            public: true,
            return_annotation: None,
            return_type: bool(),
            end_position: 0,
            body: TypedExpr::UnOp {
                location: Span::empty(),
                tipo: bool(),
                op: UnOp::Not,
                value: Box::new(TypedExpr::Var {
                    location: Span::empty(),
                    constructor: ValueConstructor {
                        public: true,
                        tipo: bool(),
                        variant: ValueConstructorVariant::LocalVariable {
                            location: Span::empty(),
                        },
                    },
                    name: "self".to_string(),
                }),
            },
        },
    );

    // /// A function that returns its argument. Handy as a default behavior sometimes.
    // pub fn identity(a: a) -> a {
    //   a
    // }
    let a_var = generic_var(id_gen.next());

    functions.insert(
        FunctionAccessKey {
            module_name: "".to_string(),
            function_name: "identity".to_string(),
            variant_name: "".to_string(),
        },
        Function {
            arguments: vec![Arg {
                arg_name: ArgName::Named {
                    name: "a".to_string(),
                    label: "a".to_string(),
                    location: Span::empty(),
                },
                location: Span::empty(),
                annotation: None,
                tipo: a_var.clone(),
            }],
            body: TypedExpr::Var {
                location: Span::empty(),
                constructor: ValueConstructor {
                    public: true,
                    tipo: a_var.clone(),
                    variant: ValueConstructorVariant::LocalVariable {
                        location: Span::empty(),
                    },
                },
                name: "a".to_string(),
            },
            doc: None,
            location: Span::empty(),
            name: "identity".to_string(),
            public: true,
            return_annotation: None,
            return_type: a_var,
            end_position: 0,
        },
    );

    // /// A function that always return its first argument. Handy in folds and maps.
    // pub fn always(a: a, b _b: b) -> a {
    //   a
    // }
    let a_var = generic_var(id_gen.next());
    let b_var = generic_var(id_gen.next());

    functions.insert(
        FunctionAccessKey {
            module_name: "".to_string(),
            function_name: "always".to_string(),
            variant_name: "".to_string(),
        },
        Function {
            arguments: vec![
                Arg {
                    arg_name: ArgName::Named {
                        name: "a".to_string(),
                        label: "a".to_string(),
                        location: Span::empty(),
                    },
                    location: Span::empty(),
                    annotation: None,
                    tipo: a_var.clone(),
                },
                Arg {
                    arg_name: ArgName::Discarded {
                        name: "_b".to_string(),
                        label: "_b".to_string(),
                        location: Span::empty(),
                    },
                    location: Span::empty(),
                    annotation: None,
                    tipo: b_var,
                },
            ],
            body: TypedExpr::Var {
                location: Span::empty(),
                constructor: ValueConstructor {
                    public: true,
                    tipo: a_var.clone(),
                    variant: ValueConstructorVariant::LocalVariable {
                        location: Span::empty(),
                    },
                },
                name: "a".to_string(),
            },
            doc: None,
            location: Span::empty(),
            name: "always".to_string(),
            public: true,
            return_annotation: None,
            return_type: a_var,
            end_position: 0,
        },
    );

    // /// A function that flips the arguments of a function.
    // pub fn flip(f: fn(a, b) -> c) -> fn(b, a) -> c {
    //   fn(b, a) { f(a, b) }
    // }
    let a_var = generic_var(id_gen.next());
    let b_var = generic_var(id_gen.next());
    let c_var = generic_var(id_gen.next());

    let input_type = function(vec![a_var.clone(), b_var.clone()], c_var.clone());
    let return_type = function(vec![b_var.clone(), a_var.clone()], c_var.clone());

    functions.insert(
        FunctionAccessKey {
            module_name: "".to_string(),
            function_name: "flip".to_string(),
            variant_name: "".to_string(),
        },
        Function {
            arguments: vec![Arg {
                arg_name: ArgName::Named {
                    name: "f".to_string(),
                    label: "f".to_string(),
                    location: Span::empty(),
                },
                location: Span::empty(),
                annotation: None,
                tipo: input_type.clone(),
            }],
            body: TypedExpr::Fn {
                location: Span::empty(),
                tipo: return_type.clone(),
                is_capture: false,
                args: vec![
                    Arg {
                        arg_name: ArgName::Named {
                            name: "b".to_string(),
                            label: "b".to_string(),
                            location: Span::empty(),
                        },
                        location: Span::empty(),
                        annotation: None,
                        tipo: b_var.clone(),
                    },
                    Arg {
                        arg_name: ArgName::Named {
                            name: "a".to_string(),
                            label: "a".to_string(),
                            location: Span::empty(),
                        },
                        location: Span::empty(),
                        annotation: None,
                        tipo: a_var.clone(),
                    },
                ],
                body: Box::new(TypedExpr::Call {
                    location: Span::empty(),
                    tipo: c_var,
                    fun: Box::new(TypedExpr::Var {
                        location: Span::empty(),
                        constructor: ValueConstructor {
                            public: true,
                            tipo: input_type,
                            variant: ValueConstructorVariant::LocalVariable {
                                location: Span::empty(),
                            },
                        },
                        name: "f".to_string(),
                    }),
                    args: vec![
                        CallArg {
                            label: None,
                            location: Span::empty(),
                            value: TypedExpr::Var {
                                location: Span::empty(),
                                constructor: ValueConstructor {
                                    public: true,
                                    tipo: a_var,
                                    variant: ValueConstructorVariant::LocalVariable {
                                        location: Span::empty(),
                                    },
                                },
                                name: "a".to_string(),
                            },
                        },
                        CallArg {
                            label: None,
                            location: Span::empty(),
                            value: TypedExpr::Var {
                                location: Span::empty(),
                                constructor: ValueConstructor {
                                    public: true,
                                    tipo: b_var,
                                    variant: ValueConstructorVariant::LocalVariable {
                                        location: Span::empty(),
                                    },
                                },
                                name: "b".to_string(),
                            },
                        },
                    ],
                }),
                return_annotation: None,
            },
            doc: None,
            location: Span::empty(),
            name: "flip".to_string(),
            public: true,
            return_annotation: None,
            return_type,
            end_position: 0,
        },
    );

    functions
}

pub fn prelude_data_types(id_gen: &IdGenerator) -> IndexMap<DataTypeKey, TypedDataType> {
    let mut data_types = IndexMap::new();

    // Ordering
    let ordering_data_type = TypedDataType::ordering();
    data_types.insert(
        DataTypeKey {
            module_name: "".to_string(),
            defined_type: "Ordering".to_string(),
        },
        ordering_data_type,
    );

    // Option
    let option_data_type = TypedDataType::option(generic_var(id_gen.next()));
    data_types.insert(
        DataTypeKey {
            module_name: "".to_string(),
            defined_type: "Option".to_string(),
        },
        option_data_type,
    );

    data_types
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

pub fn void() -> Arc<Type> {
    Arc::new(Type::App {
        args: vec![],
        public: true,
        name: VOID.to_string(),
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

pub fn ordering() -> Arc<Type> {
    Arc::new(Type::App {
        public: true,
        name: ORDERING.to_string(),
        module: "".to_string(),
        args: vec![],
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
