use crate::{
    ast::{
        Annotation, ArgName, CallArg, DataTypeKey, Function, FunctionAccessKey, ModuleKind,
        OnTestFailure, Span, TypedArg, TypedDataType, TypedFunction, UnOp,
    },
    expr::TypedExpr,
    tipo::{
        fields::FieldMap, Type, TypeAliasAnnotation, TypeConstructor, TypeInfo, TypeVar,
        ValueConstructor, ValueConstructorVariant,
    },
    IdGenerator,
};
use indexmap::IndexMap;
use std::{cell::RefCell, collections::HashMap, rc::Rc};
use strum::IntoEnumIterator;
use uplc::builtins::DefaultFunction;

pub const PRELUDE: &str = "aiken";
pub const BUILTIN: &str = "aiken/builtin";

pub const BYTE_ARRAY: &str = "ByteArray";
pub const BOOL: &str = "Bool";
pub const INT: &str = "Int";
pub const DATA: &str = "Data";
pub const LIST: &str = "List";
pub const PAIR: &str = "Pair";
pub const PAIRS: &str = "Pairs";
pub const VOID: &str = "Void";
pub const G1_ELEMENT: &str = "G1Element";
pub const G2_ELEMENT: &str = "G2Element";
pub const MILLER_LOOP_RESULT: &str = "MillerLoopResult";
pub const STRING: &str = "String";
pub const OPTION: &str = "Option";
pub const ORDERING: &str = "Ordering";
pub const REDEEMER_WRAPPER: &str = "RedeemerWrapper";
pub const PRNG: &str = "PRNG";
pub const FUZZER: &str = "Fuzzer";

/// Build a prelude that can be injected
/// into a compiler pipeline
pub fn prelude(id_gen: &IdGenerator) -> TypeInfo {
    let mut prelude = TypeInfo {
        name: PRELUDE.to_string(),
        package: "".to_string(),
        kind: ModuleKind::Lib,
        types: HashMap::new(),
        types_constructors: HashMap::new(),
        values: HashMap::new(),
        accessors: HashMap::new(),
        annotations: HashMap::new(),
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
        vec!["False".to_string(), "True".to_string()],
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

    // G1Element
    prelude.types.insert(
        G1_ELEMENT.to_string(),
        TypeConstructor {
            parameters: vec![],
            tipo: g1_element(),
            location: Span::empty(),
            module: "".to_string(),
            public: true,
        },
    );

    // G2Element
    prelude.types.insert(
        G2_ELEMENT.to_string(),
        TypeConstructor {
            parameters: vec![],
            tipo: g2_element(),
            location: Span::empty(),
            module: "".to_string(),
            public: true,
        },
    );

    // MillerLoopResult
    prelude.types.insert(
        MILLER_LOOP_RESULT.to_string(),
        TypeConstructor {
            parameters: vec![],
            tipo: miller_loop_result(),
            location: Span::empty(),
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

    // Pair(a, b)
    let fst_parameter = generic_var(id_gen.next());
    let snd_parameter = generic_var(id_gen.next());
    prelude.types.insert(
        PAIR.to_string(),
        TypeConstructor {
            location: Span::empty(),
            parameters: vec![fst_parameter.clone(), snd_parameter.clone()],
            tipo: pair(fst_parameter.clone(), snd_parameter.clone()),
            module: "".to_string(),
            public: true,
        },
    );

    prelude
        .types_constructors
        .insert(PAIR.to_string(), vec![PAIR.to_string()]);

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
    prelude
        .types_constructors
        .insert(VOID.to_string(), vec![VOID.to_string()]);

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
            tipo: option(option_value.clone()),
            module: "".to_string(),
            public: true,
        },
    );

    prelude.types_constructors.insert(
        OPTION.to_string(),
        vec!["Some".to_string(), "None".to_string()],
    );

    prelude.values.insert(
        "Some".to_string(),
        ValueConstructor::public(
            function(vec![option_value.clone()], option(option_value.clone())),
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

    prelude.values.insert(
        "None".to_string(),
        ValueConstructor::public(
            option(option_value),
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

    // PRNG
    //
    // pub type PRNG {
    //   Seeded { seed: ByteArray, choices: ByteArray }
    //   Replayed { cursor: Int, choices: ByteArray }
    // }
    prelude.types.insert(
        PRNG.to_string(),
        TypeConstructor {
            location: Span::empty(),
            parameters: vec![],
            tipo: prng(),
            module: "".to_string(),
            public: true,
        },
    );

    prelude.types_constructors.insert(
        PRNG.to_string(),
        vec!["Seeded".to_string(), "Replayed".to_string()],
    );

    let mut seeded_fields = HashMap::new();
    seeded_fields.insert("seed".to_string(), (0, Span::empty()));
    seeded_fields.insert("choices".to_string(), (1, Span::empty()));
    prelude.values.insert(
        "Seeded".to_string(),
        ValueConstructor::public(
            function(vec![byte_array(), byte_array()], prng()),
            ValueConstructorVariant::Record {
                module: "".into(),
                name: "Seeded".to_string(),
                field_map: Some(FieldMap {
                    arity: 2,
                    fields: seeded_fields,
                    is_function: false,
                }),
                arity: 2,
                location: Span::empty(),
                constructors_count: 2,
            },
        ),
    );

    let mut replayed_fields = HashMap::new();
    replayed_fields.insert("cursor".to_string(), (0, Span::empty()));
    replayed_fields.insert("choices".to_string(), (1, Span::empty()));
    prelude.values.insert(
        "Replayed".to_string(),
        ValueConstructor::public(
            function(vec![int(), byte_array()], prng()),
            ValueConstructorVariant::Record {
                module: "".into(),
                name: "Replayed".to_string(),
                field_map: Some(FieldMap {
                    arity: 2,
                    fields: replayed_fields,
                    is_function: false,
                }),
                arity: 2,
                location: Span::empty(),
                constructors_count: 2,
            },
        ),
    );

    // Fuzzer
    //
    // pub type Fuzzer<a> =
    //   fn(PRNG) -> Option<(PRNG, a)>
    let fuzzer_value = generic_var(id_gen.next());
    prelude.types.insert(
        FUZZER.to_string(),
        TypeConstructor {
            location: Span::empty(),
            parameters: vec![fuzzer_value.clone()],
            tipo: fuzzer(fuzzer_value),
            module: "".to_string(),
            public: true,
        },
    );

    // Map
    //
    // pub type Map<k, v> = List<Pair<k, v>>
    let alist_key = generic_var(id_gen.next());
    let alist_value = generic_var(id_gen.next());
    prelude.types.insert(
        PAIRS.to_string(),
        TypeConstructor {
            location: Span::empty(),
            parameters: vec![alist_key.clone(), alist_value.clone()],
            tipo: map(alist_key, alist_value),
            module: "".to_string(),
            public: true,
        },
    );

    prelude
}

pub fn plutus(id_gen: &IdGenerator) -> TypeInfo {
    let mut plutus = TypeInfo {
        name: BUILTIN.to_string(),
        package: "".to_string(),
        kind: ModuleKind::Lib,
        types: HashMap::new(),
        types_constructors: HashMap::new(),
        values: HashMap::new(),
        accessors: HashMap::new(),
        annotations: HashMap::new(),
    };

    for builtin in DefaultFunction::iter() {
        let value = from_default_function(builtin, id_gen);

        plutus.values.insert(builtin.aiken_name(), value);
    }

    plutus
}

pub fn from_default_function(builtin: DefaultFunction, id_gen: &IdGenerator) -> ValueConstructor {
    let (tipo, arity) = match builtin {
        DefaultFunction::AddInteger
        | DefaultFunction::SubtractInteger
        | DefaultFunction::MultiplyInteger
        | DefaultFunction::DivideInteger
        | DefaultFunction::QuotientInteger
        | DefaultFunction::RemainderInteger
        | DefaultFunction::ModInteger => {
            let tipo = function(vec![int(), int()], int());

            (tipo, 2)
        }

        DefaultFunction::EqualsInteger
        | DefaultFunction::LessThanInteger
        | DefaultFunction::LessThanEqualsInteger => {
            let tipo = function(vec![int(), int()], bool());

            (tipo, 2)
        }
        DefaultFunction::AppendByteString => {
            let tipo = function(vec![byte_array(), byte_array()], byte_array());

            (tipo, 2)
        }
        DefaultFunction::ConsByteString => {
            let tipo = function(vec![int(), byte_array()], byte_array());

            (tipo, 2)
        }
        DefaultFunction::SliceByteString => {
            let tipo = function(vec![int(), int(), byte_array()], byte_array());

            (tipo, 3)
        }
        DefaultFunction::LengthOfByteString => {
            let tipo = function(vec![byte_array()], int());

            (tipo, 1)
        }
        DefaultFunction::IndexByteString => {
            let tipo = function(vec![byte_array(), int()], int());

            (tipo, 2)
        }
        DefaultFunction::EqualsByteString
        | DefaultFunction::LessThanByteString
        | DefaultFunction::LessThanEqualsByteString => {
            let tipo = function(vec![byte_array(), byte_array()], bool());

            (tipo, 2)
        }
        DefaultFunction::Sha2_256
        | DefaultFunction::Sha3_256
        | DefaultFunction::Blake2b_224
        | DefaultFunction::Blake2b_256
        | DefaultFunction::Keccak_256 => {
            let tipo = function(vec![byte_array()], byte_array());

            (tipo, 1)
        }

        DefaultFunction::VerifyEd25519Signature => {
            let tipo = function(vec![byte_array(), byte_array(), byte_array()], bool());

            (tipo, 3)
        }

        DefaultFunction::VerifyEcdsaSecp256k1Signature => {
            let tipo = function(vec![byte_array(), byte_array(), byte_array()], bool());

            (tipo, 3)
        }
        DefaultFunction::VerifySchnorrSecp256k1Signature => {
            let tipo = function(vec![byte_array(), byte_array(), byte_array()], bool());

            (tipo, 3)
        }

        DefaultFunction::AppendString => {
            let tipo = function(vec![string(), string()], string());

            (tipo, 2)
        }
        DefaultFunction::EqualsString => {
            let tipo = function(vec![string(), string()], bool());

            (tipo, 2)
        }
        DefaultFunction::EncodeUtf8 => {
            let tipo = function(vec![string()], byte_array());

            (tipo, 1)
        }
        DefaultFunction::DecodeUtf8 => {
            let tipo = function(vec![byte_array()], string());

            (tipo, 1)
        }
        DefaultFunction::IfThenElse => {
            let ret = generic_var(id_gen.next());

            let tipo = function(vec![bool(), ret.clone(), ret.clone()], ret);

            (tipo, 3)
        }
        DefaultFunction::HeadList => {
            let ret = generic_var(id_gen.next());

            let tipo = function(vec![list(ret.clone())], ret);

            (tipo, 1)
        }
        DefaultFunction::TailList => {
            let ret = list(generic_var(id_gen.next()));

            let tipo = function(vec![ret.clone()], ret);

            (tipo, 1)
        }
        DefaultFunction::NullList => {
            let ret = list(generic_var(id_gen.next()));

            let tipo = function(vec![ret], bool());

            (tipo, 1)
        }
        DefaultFunction::ConstrData => {
            let tipo = function(vec![int(), list(data())], data());

            (tipo, 2)
        }
        DefaultFunction::MapData => {
            let tipo = function(vec![list(pair(data(), data()))], data());

            (tipo, 1)
        }
        DefaultFunction::ListData => {
            let tipo = function(vec![list(data())], data());

            (tipo, 1)
        }
        DefaultFunction::IData => {
            let tipo = function(vec![int()], data());

            (tipo, 1)
        }
        DefaultFunction::BData => {
            let tipo = function(vec![byte_array()], data());

            (tipo, 1)
        }
        DefaultFunction::UnConstrData => {
            let tipo = function(vec![data()], pair(int(), list(data())));

            (tipo, 1)
        }
        DefaultFunction::UnMapData => {
            let tipo = function(vec![data()], list(pair(data(), data())));

            (tipo, 1)
        }
        DefaultFunction::UnListData => {
            let tipo = function(vec![data()], list(data()));

            (tipo, 1)
        }
        DefaultFunction::UnIData => {
            let tipo = function(vec![data()], int());

            (tipo, 1)
        }
        DefaultFunction::UnBData => {
            let tipo = function(vec![data()], byte_array());

            (tipo, 1)
        }
        DefaultFunction::EqualsData => {
            let tipo = function(vec![data(), data()], bool());

            (tipo, 2)
        }
        DefaultFunction::SerialiseData => {
            let tipo = function(vec![data()], byte_array());

            (tipo, 1)
        }
        DefaultFunction::ChooseData => {
            let a = generic_var(id_gen.next());
            let tipo = function(
                vec![
                    data(),
                    a.clone(),
                    a.clone(),
                    a.clone(),
                    a.clone(),
                    a.clone(),
                ],
                a,
            );
            (tipo, 6)
        }
        DefaultFunction::MkPairData => {
            let tipo = function(vec![data(), data()], pair(data(), data()));
            (tipo, 2)
        }
        DefaultFunction::MkNilData => {
            let tipo = function(vec![], list(data()));
            (tipo, 0)
        }
        DefaultFunction::MkNilPairData => {
            let tipo = function(vec![], list(pair(data(), data())));
            (tipo, 0)
        }
        DefaultFunction::ChooseUnit => {
            let a = generic_var(id_gen.next());
            let tipo = function(vec![data(), a.clone()], a);
            (tipo, 2)
        }
        DefaultFunction::Trace => {
            let a = generic_var(id_gen.next());
            let tipo = function(vec![string(), a.clone()], a);
            (tipo, 2)
        }
        DefaultFunction::FstPair => {
            let a = generic_var(id_gen.next());
            let b = generic_var(id_gen.next());
            let tipo = function(vec![pair(a.clone(), b)], a);
            (tipo, 1)
        }
        DefaultFunction::SndPair => {
            let a = generic_var(id_gen.next());
            let b = generic_var(id_gen.next());
            let tipo = function(vec![pair(a, b.clone())], b);
            (tipo, 1)
        }
        DefaultFunction::ChooseList => {
            let a = generic_var(id_gen.next());
            let b = generic_var(id_gen.next());
            let tipo = function(vec![list(a), b.clone(), b.clone()], b);
            (tipo, 3)
        }
        DefaultFunction::MkCons => {
            let a = generic_var(id_gen.next());
            let tipo = function(vec![a.clone(), list(a.clone())], list(a));
            (tipo, 2)
        }
        DefaultFunction::Bls12_381_G1_Add => {
            let tipo = function(vec![g1_element(), g1_element()], g1_element());

            (tipo, 2)
        }
        DefaultFunction::Bls12_381_G1_Equal => {
            let tipo = function(vec![g1_element(), g1_element()], bool());

            (tipo, 2)
        }
        DefaultFunction::Bls12_381_G1_Neg => {
            let tipo = function(vec![g1_element()], g1_element());

            (tipo, 1)
        }
        DefaultFunction::Bls12_381_G1_ScalarMul => {
            let tipo = function(vec![int(), g1_element()], g1_element());

            (tipo, 2)
        }
        DefaultFunction::Bls12_381_G1_Compress => {
            let tipo = function(vec![g1_element()], byte_array());

            (tipo, 1)
        }
        DefaultFunction::Bls12_381_G1_Uncompress => {
            let tipo = function(vec![byte_array()], g1_element());

            (tipo, 1)
        }
        DefaultFunction::Bls12_381_G1_HashToGroup => {
            let tipo = function(vec![byte_array(), byte_array()], g1_element());

            (tipo, 2)
        }

        DefaultFunction::Bls12_381_G2_Add => {
            let tipo = function(vec![g2_element(), g2_element()], g2_element());

            (tipo, 2)
        }
        DefaultFunction::Bls12_381_G2_Equal => {
            let tipo = function(vec![g2_element(), g2_element()], bool());

            (tipo, 2)
        }
        DefaultFunction::Bls12_381_G2_Neg => {
            let tipo = function(vec![g2_element()], g2_element());

            (tipo, 1)
        }
        DefaultFunction::Bls12_381_G2_ScalarMul => {
            let tipo = function(vec![int(), g2_element()], g2_element());

            (tipo, 2)
        }
        DefaultFunction::Bls12_381_G2_Compress => {
            let tipo = function(vec![g2_element()], byte_array());

            (tipo, 1)
        }
        DefaultFunction::Bls12_381_G2_Uncompress => {
            let tipo = function(vec![byte_array()], g2_element());

            (tipo, 1)
        }
        DefaultFunction::Bls12_381_G2_HashToGroup => {
            let tipo = function(vec![byte_array(), byte_array()], g2_element());

            (tipo, 2)
        }
        DefaultFunction::Bls12_381_MillerLoop => {
            let tipo = function(vec![g1_element(), g2_element()], miller_loop_result());

            (tipo, 2)
        }
        DefaultFunction::Bls12_381_MulMlResult => {
            let tipo = function(
                vec![miller_loop_result(), miller_loop_result()],
                miller_loop_result(),
            );

            (tipo, 2)
        }
        DefaultFunction::Bls12_381_FinalVerify => {
            let tipo = function(vec![miller_loop_result(), miller_loop_result()], bool());

            (tipo, 2)
        }
        DefaultFunction::IntegerToByteString => {
            let tipo = function(vec![bool(), int(), int()], byte_array());

            (tipo, 3)
        }
        DefaultFunction::ByteStringToInteger => {
            let tipo = function(vec![bool(), byte_array()], int());

            (tipo, 2)
        }
    };

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
        },
        Function {
            arguments: vec![TypedArg {
                arg_name: ArgName::Named {
                    name: "self".to_string(),
                    label: "self".to_string(),
                    location: Span::empty(),
                },
                is_validator_param: false,
                doc: None,
                location: Span::empty(),
                annotation: None,
                tipo: bool(),
            }],
            on_test_failure: OnTestFailure::FailImmediately,
            doc: Some(
                indoc::indoc! {
                    r#"
                    /// Like `!`, but as a function. Handy for chaining using the pipe operator `|>` or to pass as a function.
                    "#
                }.to_string()
            ),
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
        },
        Function {
            arguments: vec![TypedArg {
                arg_name: ArgName::Named {
                    name: "a".to_string(),
                    label: "a".to_string(),
                    location: Span::empty(),
                },
                is_validator_param: false,
                location: Span::empty(),
                annotation: None,
                doc: None,
                tipo: a_var.clone(),
            }],
            on_test_failure: OnTestFailure::FailImmediately,
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
            doc: Some(
                indoc::indoc! {
                    r#"
                    A function that returns its argument. Handy as a default behavior sometimes.
                    "#
                }
                .to_string(),
            ),
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
        },
        Function {
            on_test_failure: OnTestFailure::FailImmediately,
            arguments: vec![
                TypedArg {
                    arg_name: ArgName::Named {
                        name: "a".to_string(),
                        label: "a".to_string(),
                        location: Span::empty(),
                    },
                    is_validator_param: false,
                    location: Span::empty(),
                    annotation: None,
                    doc: None,
                    tipo: a_var.clone(),
                },
                TypedArg {
                    arg_name: ArgName::Discarded {
                        name: "_b".to_string(),
                        label: "_b".to_string(),
                        location: Span::empty(),
                    },
                    is_validator_param: false,
                    location: Span::empty(),
                    annotation: None,
                    doc: None,
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
            doc: Some(
                indoc::indoc! {
                    r#"
                    A function that always return its first argument. Handy in folds and maps.

                    ```aiken
                    let always_14 = always(14, _)
                    always_14(42) == 14
                    always_14(1337) == 14
                    always_14(0) == 14
                    ```
                    "#
                }
                .to_string(),
            ),
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
        },
        Function {
            on_test_failure: OnTestFailure::FailImmediately,
            arguments: vec![TypedArg {
                arg_name: ArgName::Named {
                    name: "f".to_string(),
                    label: "f".to_string(),
                    location: Span::empty(),
                },
                is_validator_param: false,
                location: Span::empty(),
                annotation: None,
                doc: None,
                tipo: input_type.clone(),
            }],
            body: TypedExpr::Fn {
                location: Span::empty(),
                tipo: return_type.clone(),
                is_capture: false,
                args: vec![
                    TypedArg {
                        arg_name: ArgName::Named {
                            name: "b".to_string(),
                            label: "b".to_string(),
                            location: Span::empty(),
                        },
                        is_validator_param: false,
                        location: Span::empty(),
                        annotation: None,
                        doc: None,
                        tipo: b_var.clone(),
                    },
                    TypedArg {
                        arg_name: ArgName::Named {
                            name: "a".to_string(),
                            label: "a".to_string(),
                            location: Span::empty(),
                        },
                        is_validator_param: false,
                        location: Span::empty(),
                        annotation: None,
                        doc: None,
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
            doc: Some(
                indoc::indoc! {
                    r#"
                    A function that flips the arguments of a function.

                    ```aiken
                    pub fn titleize(left: String, right: String) {}

                    titleize("Hello", "World") // "Hello, World!"

                    flip(titleize)("Hello", "World") // "World, Hello!"
                    ```
                    "#
                }
                .to_string(),
            ),
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

    // Data
    let data_data_type = TypedDataType::data();
    data_types.insert(
        DataTypeKey {
            module_name: "".to_string(),
            defined_type: "Data".to_string(),
        },
        data_data_type,
    );

    // Ordering
    let ordering_data_type = TypedDataType::ordering();
    data_types.insert(
        DataTypeKey {
            module_name: "".to_string(),
            defined_type: "Ordering".to_string(),
        },
        ordering_data_type,
    );

    // Bool
    let bool_data_type = TypedDataType::bool();
    data_types.insert(
        DataTypeKey {
            module_name: "".to_string(),
            defined_type: "Bool".to_string(),
        },
        bool_data_type,
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

    // PRNG
    let prng_data_type = TypedDataType::prng();
    data_types.insert(
        DataTypeKey {
            module_name: "".to_string(),
            defined_type: "PRNG".to_string(),
        },
        prng_data_type,
    );

    data_types
}

pub fn int() -> Rc<Type> {
    Rc::new(Type::App {
        public: true,
        contains_opaque: false,
        name: INT.to_string(),
        module: "".to_string(),
        args: vec![],
        alias: None,
    })
}

pub fn data() -> Rc<Type> {
    Rc::new(Type::App {
        public: true,
        contains_opaque: false,
        name: DATA.to_string(),
        module: "".to_string(),
        args: vec![],
        alias: None,
    })
}

pub fn byte_array() -> Rc<Type> {
    Rc::new(Type::App {
        args: vec![],
        public: true,
        contains_opaque: false,
        name: BYTE_ARRAY.to_string(),
        module: "".to_string(),
        alias: None,
    })
}

pub fn g1_element() -> Rc<Type> {
    Rc::new(Type::App {
        public: true,
        contains_opaque: false,
        module: "".to_string(),
        name: G1_ELEMENT.to_string(),
        args: vec![],
        alias: None,
    })
}

pub fn g2_element() -> Rc<Type> {
    Rc::new(Type::App {
        public: true,
        contains_opaque: false,
        module: "".to_string(),
        name: G2_ELEMENT.to_string(),
        args: vec![],
        alias: None,
    })
}

pub fn miller_loop_result() -> Rc<Type> {
    Rc::new(Type::App {
        public: true,
        contains_opaque: false,
        module: "".to_string(),
        name: MILLER_LOOP_RESULT.to_string(),
        args: vec![],
        alias: None,
    })
}

pub fn tuple(elems: Vec<Rc<Type>>) -> Rc<Type> {
    Rc::new(Type::Tuple { elems, alias: None })
}

pub fn pair(fst: Rc<Type>, snd: Rc<Type>) -> Rc<Type> {
    Rc::new(Type::Pair {
        fst,
        snd,
        alias: None,
    })
}

pub fn bool() -> Rc<Type> {
    Rc::new(Type::App {
        args: vec![],
        public: true,
        contains_opaque: false,
        name: BOOL.to_string(),
        module: "".to_string(),
        alias: None,
    })
}

pub fn prng() -> Rc<Type> {
    Rc::new(Type::App {
        args: vec![],
        public: true,
        contains_opaque: false,
        name: PRNG.to_string(),
        module: "".to_string(),
        alias: None,
    })
}

pub fn fuzzer(a: Rc<Type>) -> Rc<Type> {
    let prng_annotation = Annotation::Constructor {
        location: Span::empty(),
        module: None,
        name: "PRNG".to_string(),
        arguments: vec![],
    };

    Rc::new(Type::Fn {
        args: vec![prng()],
        ret: option(tuple(vec![prng(), a])),
        alias: Some(
            TypeAliasAnnotation {
                alias: "Fuzzer".to_string(),
                parameters: vec!["a".to_string()],
                annotation: Annotation::Fn {
                    location: Span::empty(),
                    arguments: vec![prng_annotation.clone()],
                    ret: Annotation::Constructor {
                        location: Span::empty(),
                        module: None,
                        name: "Option".to_string(),
                        arguments: vec![Annotation::Tuple {
                            location: Span::empty(),
                            elems: vec![
                                prng_annotation,
                                Annotation::Var {
                                    location: Span::empty(),
                                    name: "a".to_string(),
                                },
                            ],
                        }],
                    }
                    .into(),
                },
            }
            .into(),
        ),
    })
}

pub fn map(k: Rc<Type>, v: Rc<Type>) -> Rc<Type> {
    Rc::new(Type::App {
        public: true,
        contains_opaque: false,
        module: "".to_string(),
        name: LIST.to_string(),
        args: vec![pair(k, v)],
        alias: Some(
            TypeAliasAnnotation {
                alias: PAIRS.to_string(),
                parameters: vec!["k".to_string(), "v".to_string()],
                annotation: Annotation::Constructor {
                    location: Span::empty(),
                    module: None,
                    name: LIST.to_string(),
                    arguments: vec![Annotation::Pair {
                        location: Span::empty(),
                        fst: Box::new(Annotation::Var {
                            location: Span::empty(),
                            name: "k".to_string(),
                        }),
                        snd: Box::new(Annotation::Var {
                            location: Span::empty(),
                            name: "v".to_string(),
                        }),
                    }],
                },
            }
            .into(),
        ),
    })
}

pub fn list(t: Rc<Type>) -> Rc<Type> {
    Rc::new(Type::App {
        public: true,
        // FIXME: We should probably have t.contains_opaque here?
        contains_opaque: false,
        name: LIST.to_string(),
        module: "".to_string(),
        args: vec![t],
        alias: None,
    })
}

pub fn string() -> Rc<Type> {
    Rc::new(Type::App {
        args: vec![],
        public: true,
        contains_opaque: false,
        name: STRING.to_string(),
        module: "".to_string(),
        alias: None,
    })
}

pub fn void() -> Rc<Type> {
    Rc::new(Type::App {
        args: vec![],
        public: true,
        contains_opaque: false,
        name: VOID.to_string(),
        module: "".to_string(),
        alias: None,
    })
}

pub fn option(a: Rc<Type>) -> Rc<Type> {
    Rc::new(Type::App {
        public: true,
        // FIXME: We should probably have t.contains_opaque here?
        contains_opaque: false,
        name: OPTION.to_string(),
        module: "".to_string(),
        args: vec![a],
        alias: None,
    })
}

pub fn ordering() -> Rc<Type> {
    Rc::new(Type::App {
        public: true,
        contains_opaque: false,
        name: ORDERING.to_string(),
        module: "".to_string(),
        args: vec![],
        alias: None,
    })
}

pub fn function(args: Vec<Rc<Type>>, ret: Rc<Type>) -> Rc<Type> {
    Rc::new(Type::Fn {
        ret,
        args,
        alias: None,
    })
}

pub fn generic_var(id: u64) -> Rc<Type> {
    let tipo = Rc::new(RefCell::new(TypeVar::Generic { id }));

    Rc::new(Type::Var { tipo, alias: None })
}

pub fn unbound_var(id: u64) -> Rc<Type> {
    let tipo = Rc::new(RefCell::new(TypeVar::Unbound { id }));

    Rc::new(Type::Var { tipo, alias: None })
}

pub fn wrapped_redeemer(redeemer: Rc<Type>) -> Rc<Type> {
    Rc::new(Type::App {
        public: true,
        contains_opaque: false,
        module: "".to_string(),
        name: REDEEMER_WRAPPER.to_string(),
        args: vec![redeemer],
        alias: None,
    })
}
