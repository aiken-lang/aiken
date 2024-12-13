use crate::{
    aiken_fn,
    ast::{
        well_known, Annotation, ArgName, CallArg, DataType, DataTypeKey, Function,
        FunctionAccessKey, ModuleKind, OnTestFailure, RecordConstructor, RecordConstructorArg,
        Span, TypedArg, TypedDataType, TypedFunction, UnOp,
    },
    expr::TypedExpr,
    tipo::{
        fields::FieldMap, Type, TypeConstructor, TypeInfo, ValueConstructor,
        ValueConstructorVariant,
    },
    IdGenerator,
};
use indexmap::IndexMap;
use std::{collections::HashMap, rc::Rc};
use strum::IntoEnumIterator;
use uplc::{
    builder::{CONSTR_FIELDS_EXPOSER, CONSTR_INDEX_EXPOSER},
    builtins::DefaultFunction,
};

pub const PRELUDE: &str = "aiken";
pub const BUILTIN: &str = "aiken/builtin";

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

    // Data
    prelude.types.insert(
        well_known::DATA.to_string(),
        TypeConstructor::primitive(Type::data()),
    );

    // Int
    prelude.types.insert(
        well_known::INT.to_string(),
        TypeConstructor::primitive(Type::int()),
    );

    // ByteArray
    prelude.types.insert(
        well_known::BYTE_ARRAY.to_string(),
        TypeConstructor::primitive(Type::byte_array()),
    );

    // Bool
    prelude.types.insert(
        well_known::BOOL.to_string(),
        TypeConstructor::primitive(Type::bool()),
    );
    prelude.types_constructors.insert(
        well_known::BOOL.to_string(),
        ValueConstructor::known_enum(
            &mut prelude.values,
            Type::bool(),
            well_known::BOOL_CONSTRUCTORS,
        ),
    );

    // G1Element
    prelude.types.insert(
        well_known::G1_ELEMENT.to_string(),
        TypeConstructor::primitive(Type::g1_element()),
    );

    // G2Element
    prelude.types.insert(
        well_known::G2_ELEMENT.to_string(),
        TypeConstructor::primitive(Type::g2_element()),
    );

    // MillerLoopResult
    prelude.types.insert(
        well_known::MILLER_LOOP_RESULT.to_string(),
        TypeConstructor::primitive(Type::miller_loop_result()),
    );

    // Ordering
    prelude.types.insert(
        well_known::ORDERING.to_string(),
        TypeConstructor::primitive(Type::ordering()),
    );
    prelude.types_constructors.insert(
        well_known::ORDERING.to_string(),
        ValueConstructor::known_enum(
            &mut prelude.values,
            Type::ordering(),
            well_known::ORDERING_CONSTRUCTORS,
        ),
    );

    // String
    prelude.types.insert(
        well_known::STRING.to_string(),
        TypeConstructor::primitive(Type::string()),
    );

    // Void
    prelude.types.insert(
        well_known::VOID.to_string(),
        TypeConstructor::primitive(Type::void()),
    );
    prelude.types_constructors.insert(
        well_known::VOID.to_string(),
        ValueConstructor::known_enum(
            &mut prelude.values,
            Type::void(),
            well_known::VOID_CONSTRUCTORS,
        ),
    );

    // List(a)
    prelude.types.insert(
        well_known::LIST.to_string(),
        TypeConstructor::primitive(Type::list(Type::generic_var(id_gen.next()))),
    );

    // Pair(a, b)
    prelude.types.insert(
        well_known::PAIR.to_string(),
        TypeConstructor::primitive(Type::pair(
            Type::generic_var(id_gen.next()),
            Type::generic_var(id_gen.next()),
        )),
    );
    prelude.types_constructors.insert(
        well_known::PAIR.to_string(),
        vec![well_known::PAIR.to_string()],
    );

    // Pairs<k, v> = List<Pair<k, v>>
    prelude.types.insert(
        well_known::PAIRS.to_string(),
        TypeConstructor::primitive(Type::map(
            Type::generic_var(id_gen.next()),
            Type::generic_var(id_gen.next()),
        )),
    );

    // Option(value)
    let option_value = Type::generic_var(id_gen.next());
    prelude.types.insert(
        well_known::OPTION.to_string(),
        TypeConstructor::primitive(Type::option(option_value.clone())),
    );
    let some_type = Type::function(
        vec![option_value.clone()],
        Type::option(option_value.clone()),
    );
    let none_type = Type::option(option_value);
    prelude.types_constructors.insert(
        well_known::OPTION.to_string(),
        ValueConstructor::known_adt(
            &mut prelude.values,
            &[
                (well_known::OPTION_CONSTRUCTORS[0], some_type),
                (well_known::OPTION_CONSTRUCTORS[1], none_type),
            ],
        ),
    );

    // Never
    prelude.types.insert(
        well_known::NEVER.to_string(),
        TypeConstructor::primitive(Type::never()),
    );
    prelude.types_constructors.insert(
        well_known::NEVER.to_string(),
        ValueConstructor::known_adt(
            &mut prelude.values,
            &[(well_known::NEVER_CONSTRUCTORS[1], Type::never())],
        ),
    );

    // Cardano ScriptContext
    prelude.types.insert(
        well_known::SCRIPT_CONTEXT.to_string(),
        TypeConstructor::primitive(Type::script_context()),
    );
    prelude.types_constructors.insert(
        well_known::SCRIPT_CONTEXT.to_string(),
        vec![
            well_known::SCRIPT_CONTEXT_TRANSACTION.to_string(),
            well_known::SCRIPT_CONTEXT_REDEEMER.to_string(),
            well_known::SCRIPT_CONTEXT_PURPOSE.to_string(),
        ],
    );

    // Cardano ScriptPurpose
    prelude.types.insert(
        well_known::SCRIPT_PURPOSE.to_string(),
        TypeConstructor::primitive(Type::script_purpose()),
    );

    prelude.types_constructors.insert(
        well_known::SCRIPT_PURPOSE.to_string(),
        ValueConstructor::known_adt(
            &mut prelude.values,
            &[
                (
                    well_known::SCRIPT_PURPOSE_MINT,
                    Type::function(vec![Type::data()], Type::script_purpose()),
                ),
                (
                    well_known::SCRIPT_PURPOSE_SPEND,
                    Type::function(
                        vec![Type::data(), Type::option(Type::data())],
                        Type::script_purpose(),
                    ),
                ),
                (
                    well_known::SCRIPT_PURPOSE_WITHDRAW,
                    Type::function(vec![Type::data()], Type::script_purpose()),
                ),
                (
                    well_known::SCRIPT_PURPOSE_PUBLISH,
                    Type::function(vec![Type::int(), Type::data()], Type::script_purpose()),
                ),
                (
                    well_known::SCRIPT_PURPOSE_VOTE,
                    Type::function(vec![Type::data()], Type::script_purpose()),
                ),
                (
                    well_known::SCRIPT_PURPOSE_PROPOSE,
                    Type::function(vec![Type::int(), Type::data()], Type::script_purpose()),
                ),
            ],
        ),
    );

    // not
    prelude.values.insert(
        "not".to_string(),
        ValueConstructor::public(
            Type::function(vec![Type::bool()], Type::bool()),
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
    let identity_var = Type::generic_var(id_gen.next());
    prelude.values.insert(
        "identity".to_string(),
        ValueConstructor::public(
            Type::function(vec![identity_var.clone()], identity_var),
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

    // enumerate
    let enumerate_a = Type::generic_var(id_gen.next());
    let enumerate_b = Type::generic_var(id_gen.next());
    prelude.values.insert(
        "enumerate".to_string(),
        ValueConstructor::public(
            Type::function(
                vec![
                    Type::list(enumerate_a.clone()),
                    enumerate_b.clone(),
                    Type::function(
                        vec![enumerate_a.clone(), enumerate_b.clone()],
                        enumerate_b.clone(),
                    ),
                    Type::function(
                        vec![enumerate_a.clone(), enumerate_b.clone()],
                        enumerate_b.clone(),
                    ),
                ],
                enumerate_b,
            ),
            ValueConstructorVariant::ModuleFn {
                name: "enumerate".to_string(),
                field_map: None,
                module: "".to_string(),
                arity: 4,
                location: Span::empty(),
                builtin: None,
            },
        ),
    );

    // encode_base16
    prelude.values.insert(
        "encode_base16".to_string(),
        ValueConstructor::public(
            Type::function(
                vec![Type::byte_array(), Type::int(), Type::byte_array()],
                Type::byte_array(),
            ),
            ValueConstructorVariant::ModuleFn {
                name: "encode_base16".to_string(),
                field_map: None,
                module: "".to_string(),
                arity: 3,
                location: Span::empty(),
                builtin: None,
            },
        ),
    );

    // from_int
    prelude.values.insert(
        "from_int".to_string(),
        ValueConstructor::public(
            Type::function(vec![Type::int(), Type::byte_array()], Type::byte_array()),
            ValueConstructorVariant::ModuleFn {
                name: "from_int".to_string(),
                field_map: None,
                module: "".to_string(),
                arity: 2,
                location: Span::empty(),
                builtin: None,
            },
        ),
    );

    // do_from_int
    prelude.values.insert(
        "do_from_int".to_string(),
        ValueConstructor::public(
            Type::function(vec![Type::int(), Type::byte_array()], Type::byte_array()),
            ValueConstructorVariant::ModuleFn {
                name: "do_from_int".to_string(),
                field_map: None,
                module: "".to_string(),
                arity: 2,
                location: Span::empty(),
                builtin: None,
            },
        ),
    );

    // diagnostic
    prelude.values.insert(
        "diagnostic".to_string(),
        ValueConstructor::public(
            Type::function(vec![Type::data(), Type::byte_array()], Type::byte_array()),
            ValueConstructorVariant::ModuleFn {
                name: "diagnostic".to_string(),
                field_map: None,
                module: "".to_string(),
                arity: 2,
                location: Span::empty(),
                builtin: None,
            },
        ),
    );

    // always
    let always_a_var = Type::generic_var(id_gen.next());
    let always_b_var = Type::generic_var(id_gen.next());
    prelude.values.insert(
        "always".to_string(),
        ValueConstructor::public(
            Type::function(vec![always_a_var.clone(), always_b_var], always_a_var),
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
    let flip_a_var = Type::generic_var(id_gen.next());
    let flip_b_var = Type::generic_var(id_gen.next());
    let flip_c_var = Type::generic_var(id_gen.next());

    let input_type = Type::function(
        vec![flip_a_var.clone(), flip_b_var.clone()],
        flip_c_var.clone(),
    );

    let return_type = Type::function(vec![flip_b_var, flip_a_var], flip_c_var);

    prelude.values.insert(
        "flip".to_string(),
        ValueConstructor::public(
            Type::function(vec![input_type], return_type),
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

    // PRNG
    //
    // pub type PRNG {
    //   Seeded { seed: ByteArray, choices: ByteArray }
    //   Replayed { cursor: Int, choices: ByteArray }
    // }
    prelude.types.insert(
        well_known::PRNG.to_string(),
        TypeConstructor::primitive(Type::prng()),
    );

    prelude.types_constructors.insert(
        well_known::PRNG.to_string(),
        vec!["Seeded".to_string(), "Replayed".to_string()],
    );

    let mut seeded_fields = HashMap::new();
    seeded_fields.insert("seed".to_string(), (0, Span::empty()));
    seeded_fields.insert("choices".to_string(), (1, Span::empty()));
    prelude.values.insert(
        "Seeded".to_string(),
        ValueConstructor::public(
            Type::function(vec![Type::byte_array(), Type::byte_array()], Type::prng()),
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
            Type::function(vec![Type::int(), Type::byte_array()], Type::prng()),
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
    let fuzzer_value = Type::generic_var(id_gen.next());
    prelude.types.insert(
        well_known::FUZZER.to_string(),
        TypeConstructor {
            location: Span::empty(),
            parameters: vec![fuzzer_value.clone()],
            tipo: Type::fuzzer(fuzzer_value),
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
        // FIXME: Disabling WriteBits for now, since its signature requires the ability to create
        // list of raw integers, which isn't possible through Aiken at the moment.
        if !matches!(builtin, DefaultFunction::WriteBits) {
            let value = from_default_function(builtin, id_gen);
            plutus.values.insert(builtin.aiken_name(), value);
        }
    }

    let index_tipo = Type::function(vec![Type::data()], Type::int());
    plutus.values.insert(
        "unconstr_index".to_string(),
        ValueConstructor::public(
            index_tipo,
            ValueConstructorVariant::ModuleFn {
                name: "unconstr_index".to_string(),
                field_map: None,
                module: "aiken/builtin".to_string(),
                arity: 1,
                location: Span::empty(),
                builtin: None,
            },
        ),
    );

    let fields_tipo = Type::function(vec![Type::data()], Type::list(Type::data()));
    plutus.values.insert(
        "unconstr_fields".to_string(),
        ValueConstructor::public(
            fields_tipo,
            ValueConstructorVariant::ModuleFn {
                name: "unconstr_fields".to_string(),
                field_map: None,
                module: "aiken/builtin".to_string(),
                arity: 1,
                location: Span::empty(),
                builtin: None,
            },
        ),
    );

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
            let tipo = Type::function(vec![Type::int(), Type::int()], Type::int());
            (tipo, 2)
        }

        DefaultFunction::EqualsInteger
        | DefaultFunction::LessThanInteger
        | DefaultFunction::LessThanEqualsInteger => {
            let tipo = Type::function(vec![Type::int(), Type::int()], Type::bool());

            (tipo, 2)
        }
        DefaultFunction::AppendByteString => {
            let tipo = Type::function(
                vec![Type::byte_array(), Type::byte_array()],
                Type::byte_array(),
            );

            (tipo, 2)
        }
        DefaultFunction::ConsByteString => {
            let tipo = Type::function(vec![Type::int(), Type::byte_array()], Type::byte_array());

            (tipo, 2)
        }
        DefaultFunction::SliceByteString => {
            let tipo = Type::function(
                vec![Type::int(), Type::int(), Type::byte_array()],
                Type::byte_array(),
            );

            (tipo, 3)
        }
        DefaultFunction::LengthOfByteString => {
            let tipo = Type::function(vec![Type::byte_array()], Type::int());

            (tipo, 1)
        }
        DefaultFunction::IndexByteString => {
            let tipo = Type::function(vec![Type::byte_array(), Type::int()], Type::int());

            (tipo, 2)
        }
        DefaultFunction::EqualsByteString
        | DefaultFunction::LessThanByteString
        | DefaultFunction::LessThanEqualsByteString => {
            let tipo = Type::function(vec![Type::byte_array(), Type::byte_array()], Type::bool());

            (tipo, 2)
        }
        DefaultFunction::Sha2_256
        | DefaultFunction::Sha3_256
        | DefaultFunction::Blake2b_224
        | DefaultFunction::Blake2b_256
        | DefaultFunction::Keccak_256 => {
            let tipo = Type::function(vec![Type::byte_array()], Type::byte_array());

            (tipo, 1)
        }

        DefaultFunction::VerifyEd25519Signature => {
            let tipo = Type::function(
                vec![Type::byte_array(), Type::byte_array(), Type::byte_array()],
                Type::bool(),
            );

            (tipo, 3)
        }

        DefaultFunction::VerifyEcdsaSecp256k1Signature => {
            let tipo = Type::function(
                vec![Type::byte_array(), Type::byte_array(), Type::byte_array()],
                Type::bool(),
            );

            (tipo, 3)
        }
        DefaultFunction::VerifySchnorrSecp256k1Signature => {
            let tipo = Type::function(
                vec![Type::byte_array(), Type::byte_array(), Type::byte_array()],
                Type::bool(),
            );

            (tipo, 3)
        }

        DefaultFunction::AppendString => {
            let tipo = Type::function(vec![Type::string(), Type::string()], Type::string());

            (tipo, 2)
        }
        DefaultFunction::EqualsString => {
            let tipo = Type::function(vec![Type::string(), Type::string()], Type::bool());

            (tipo, 2)
        }
        DefaultFunction::EncodeUtf8 => {
            let tipo = Type::function(vec![Type::string()], Type::byte_array());

            (tipo, 1)
        }
        DefaultFunction::DecodeUtf8 => {
            let tipo = Type::function(vec![Type::byte_array()], Type::string());

            (tipo, 1)
        }
        DefaultFunction::IfThenElse => {
            let ret = Type::generic_var(id_gen.next());

            let tipo = Type::function(vec![Type::bool(), ret.clone(), ret.clone()], ret);

            (tipo, 3)
        }
        DefaultFunction::HeadList => {
            let ret = Type::generic_var(id_gen.next());

            let tipo = Type::function(vec![Type::list(ret.clone())], ret);

            (tipo, 1)
        }
        DefaultFunction::TailList => {
            let ret = Type::list(Type::generic_var(id_gen.next()));

            let tipo = Type::function(vec![ret.clone()], ret);

            (tipo, 1)
        }
        DefaultFunction::NullList => {
            let ret = Type::list(Type::generic_var(id_gen.next()));

            let tipo = Type::function(vec![ret], Type::bool());

            (tipo, 1)
        }
        DefaultFunction::ConstrData => {
            let tipo = Type::function(vec![Type::int(), Type::list(Type::data())], Type::data());

            (tipo, 2)
        }
        DefaultFunction::MapData => {
            let tipo = Type::function(
                vec![Type::list(Type::pair(Type::data(), Type::data()))],
                Type::data(),
            );

            (tipo, 1)
        }
        DefaultFunction::ListData => {
            let tipo = Type::function(vec![Type::list(Type::data())], Type::data());

            (tipo, 1)
        }
        DefaultFunction::IData => {
            let tipo = Type::function(vec![Type::int()], Type::data());

            (tipo, 1)
        }
        DefaultFunction::BData => {
            let tipo = Type::function(vec![Type::byte_array()], Type::data());

            (tipo, 1)
        }
        DefaultFunction::UnConstrData => {
            let tipo = Type::function(
                vec![Type::data()],
                Type::pair(Type::int(), Type::list(Type::data())),
            );

            (tipo, 1)
        }
        DefaultFunction::UnMapData => {
            let tipo = Type::function(
                vec![Type::data()],
                Type::list(Type::pair(Type::data(), Type::data())),
            );

            (tipo, 1)
        }
        DefaultFunction::UnListData => {
            let tipo = Type::function(vec![Type::data()], Type::list(Type::data()));

            (tipo, 1)
        }
        DefaultFunction::UnIData => {
            let tipo = Type::function(vec![Type::data()], Type::int());

            (tipo, 1)
        }
        DefaultFunction::UnBData => {
            let tipo = Type::function(vec![Type::data()], Type::byte_array());

            (tipo, 1)
        }
        DefaultFunction::EqualsData => {
            let tipo = Type::function(vec![Type::data(), Type::data()], Type::bool());

            (tipo, 2)
        }
        DefaultFunction::SerialiseData => {
            let tipo = Type::function(vec![Type::data()], Type::byte_array());

            (tipo, 1)
        }
        DefaultFunction::ChooseData => {
            let a = Type::generic_var(id_gen.next());
            let tipo = Type::function(
                vec![
                    Type::data(),
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
            let tipo = Type::function(
                vec![Type::data(), Type::data()],
                Type::pair(Type::data(), Type::data()),
            );
            (tipo, 2)
        }
        DefaultFunction::MkNilData => {
            let tipo = Type::function(vec![], Type::list(Type::data()));
            (tipo, 0)
        }
        DefaultFunction::MkNilPairData => {
            let tipo = Type::function(vec![], Type::list(Type::pair(Type::data(), Type::data())));
            (tipo, 0)
        }
        DefaultFunction::ChooseUnit => {
            let a = Type::generic_var(id_gen.next());
            let tipo = Type::function(vec![Type::data(), a.clone()], a);
            (tipo, 2)
        }
        DefaultFunction::Trace => {
            let a = Type::generic_var(id_gen.next());
            let tipo = Type::function(vec![Type::string(), a.clone()], a);
            (tipo, 2)
        }
        DefaultFunction::FstPair => {
            let a = Type::generic_var(id_gen.next());
            let b = Type::generic_var(id_gen.next());
            let tipo = Type::function(vec![Type::pair(a.clone(), b)], a);
            (tipo, 1)
        }
        DefaultFunction::SndPair => {
            let a = Type::generic_var(id_gen.next());
            let b = Type::generic_var(id_gen.next());
            let tipo = Type::function(vec![Type::pair(a, b.clone())], b);
            (tipo, 1)
        }
        DefaultFunction::ChooseList => {
            let a = Type::generic_var(id_gen.next());
            let b = Type::generic_var(id_gen.next());
            let tipo = Type::function(vec![Type::list(a), b.clone(), b.clone()], b);
            (tipo, 3)
        }
        DefaultFunction::MkCons => {
            let a = Type::generic_var(id_gen.next());
            let tipo = Type::function(vec![a.clone(), Type::list(a.clone())], Type::list(a));
            (tipo, 2)
        }
        DefaultFunction::Bls12_381_G1_Add => {
            let tipo = Type::function(
                vec![Type::g1_element(), Type::g1_element()],
                Type::g1_element(),
            );

            (tipo, 2)
        }
        DefaultFunction::Bls12_381_G1_Equal => {
            let tipo = Type::function(vec![Type::g1_element(), Type::g1_element()], Type::bool());

            (tipo, 2)
        }
        DefaultFunction::Bls12_381_G1_Neg => {
            let tipo = Type::function(vec![Type::g1_element()], Type::g1_element());

            (tipo, 1)
        }
        DefaultFunction::Bls12_381_G1_ScalarMul => {
            let tipo = Type::function(vec![Type::int(), Type::g1_element()], Type::g1_element());

            (tipo, 2)
        }
        DefaultFunction::Bls12_381_G1_Compress => {
            let tipo = Type::function(vec![Type::g1_element()], Type::byte_array());

            (tipo, 1)
        }
        DefaultFunction::Bls12_381_G1_Uncompress => {
            let tipo = Type::function(vec![Type::byte_array()], Type::g1_element());

            (tipo, 1)
        }
        DefaultFunction::Bls12_381_G1_HashToGroup => {
            let tipo = Type::function(
                vec![Type::byte_array(), Type::byte_array()],
                Type::g1_element(),
            );

            (tipo, 2)
        }

        DefaultFunction::Bls12_381_G2_Add => {
            let tipo = Type::function(
                vec![Type::g2_element(), Type::g2_element()],
                Type::g2_element(),
            );

            (tipo, 2)
        }
        DefaultFunction::Bls12_381_G2_Equal => {
            let tipo = Type::function(vec![Type::g2_element(), Type::g2_element()], Type::bool());

            (tipo, 2)
        }
        DefaultFunction::Bls12_381_G2_Neg => {
            let tipo = Type::function(vec![Type::g2_element()], Type::g2_element());

            (tipo, 1)
        }
        DefaultFunction::Bls12_381_G2_ScalarMul => {
            let tipo = Type::function(vec![Type::int(), Type::g2_element()], Type::g2_element());

            (tipo, 2)
        }
        DefaultFunction::Bls12_381_G2_Compress => {
            let tipo = Type::function(vec![Type::g2_element()], Type::byte_array());

            (tipo, 1)
        }
        DefaultFunction::Bls12_381_G2_Uncompress => {
            let tipo = Type::function(vec![Type::byte_array()], Type::g2_element());

            (tipo, 1)
        }
        DefaultFunction::Bls12_381_G2_HashToGroup => {
            let tipo = Type::function(
                vec![Type::byte_array(), Type::byte_array()],
                Type::g2_element(),
            );

            (tipo, 2)
        }
        DefaultFunction::Bls12_381_MillerLoop => {
            let tipo = Type::function(
                vec![Type::g1_element(), Type::g2_element()],
                Type::miller_loop_result(),
            );

            (tipo, 2)
        }
        DefaultFunction::Bls12_381_MulMlResult => {
            let tipo = Type::function(
                vec![Type::miller_loop_result(), Type::miller_loop_result()],
                Type::miller_loop_result(),
            );

            (tipo, 2)
        }
        DefaultFunction::Bls12_381_FinalVerify => {
            let tipo = Type::function(
                vec![Type::miller_loop_result(), Type::miller_loop_result()],
                Type::bool(),
            );

            (tipo, 2)
        }
        DefaultFunction::IntegerToByteString => {
            let tipo = Type::function(
                vec![Type::bool(), Type::int(), Type::int()],
                Type::byte_array(),
            );

            (tipo, 3)
        }
        DefaultFunction::ByteStringToInteger => {
            let tipo = Type::function(vec![Type::bool(), Type::byte_array()], Type::int());

            (tipo, 2)
        }
        DefaultFunction::AndByteString => {
            let tipo = Type::function(
                vec![Type::bool(), Type::byte_array(), Type::byte_array()],
                Type::byte_array(),
            );

            (tipo, 3)
        }
        DefaultFunction::OrByteString => {
            let tipo = Type::function(
                vec![Type::bool(), Type::byte_array(), Type::byte_array()],
                Type::byte_array(),
            );

            (tipo, 3)
        }
        DefaultFunction::XorByteString => {
            let tipo = Type::function(
                vec![Type::bool(), Type::byte_array(), Type::byte_array()],
                Type::byte_array(),
            );

            (tipo, 3)
        }
        DefaultFunction::ComplementByteString => {
            let tipo = Type::function(vec![Type::byte_array()], Type::byte_array());

            (tipo, 1)
        }
        DefaultFunction::ReadBit => {
            let tipo = Type::function(vec![Type::byte_array(), Type::int()], Type::bool());

            (tipo, 2)
        }
        DefaultFunction::WriteBits => {
            let tipo = Type::function(
                vec![Type::byte_array(), Type::list(Type::int()), Type::bool()],
                Type::byte_array(),
            );

            (tipo, 3)
        }
        DefaultFunction::ReplicateByte => {
            let tipo = Type::function(vec![Type::int(), Type::int()], Type::byte_array());

            (tipo, 2)
        }
        DefaultFunction::ShiftByteString => {
            let tipo = Type::function(vec![Type::byte_array(), Type::int()], Type::byte_array());

            (tipo, 2)
        }
        DefaultFunction::RotateByteString => {
            let tipo = Type::function(vec![Type::byte_array(), Type::int()], Type::byte_array());

            (tipo, 2)
        }
        DefaultFunction::CountSetBits => {
            let tipo = Type::function(vec![Type::byte_array()], Type::int());

            (tipo, 1)
        }
        DefaultFunction::FindFirstSetBit => {
            let tipo = Type::function(vec![Type::byte_array()], Type::int());

            (tipo, 1)
        }
        DefaultFunction::Ripemd_160 => {
            let tipo = Type::function(vec![Type::byte_array()], Type::byte_array());

            (tipo, 1)
        } // DefaultFunction::ExpModInteger => {
          //     let tipo = Type::function(vec![Type::int(), Type::int(), Type::int()], Type::int());

          //     (tipo, 3)
          // }
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

pub fn prelude_functions(
    id_gen: &IdGenerator,
    module_types: &HashMap<String, TypeInfo>,
) -> IndexMap<FunctionAccessKey, TypedFunction> {
    let mut functions = IndexMap::new();

    let unconstr_index_body = TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::int(),
        fun: TypedExpr::local_var(
            CONSTR_INDEX_EXPOSER,
            Type::function(vec![Type::data()], Type::int()),
            Span::empty(),
        )
        .into(),
        args: vec![CallArg {
            label: None,
            location: Span::empty(),
            value: TypedExpr::Var {
                location: Span::empty(),
                constructor: ValueConstructor {
                    public: true,
                    tipo: Type::data(),
                    variant: ValueConstructorVariant::LocalVariable {
                        location: Span::empty(),
                    },
                },
                name: "constr".to_string(),
            },
        }],
    };

    let unconstr_index_func = Function {
        arguments: vec![TypedArg {
            arg_name: ArgName::Named {
                name: "constr".to_string(),
                label: "constr".to_string(),
                location: Span::empty(),
            },
            is_validator_param: false,
            doc: None,
            location: Span::empty(),
            annotation: None,
            tipo: Type::data(),
        }],
        on_test_failure: OnTestFailure::FailImmediately,
        doc: Some(
            indoc::indoc! {
                r#"
                /// Access the index of a constr typed as Data. Fails if the Data object is not a constr.
                "#
            }.to_string()
        ),
        location: Span::empty(),
        name: "unconstr_index".to_string(),
        public: true,
        return_annotation: None,
        return_type: Type::int(),
        end_position: 0,
        body: unconstr_index_body,
    };

    functions.insert(
        FunctionAccessKey {
            module_name: "aiken/builtin".to_string(),
            function_name: "unconstr_index".to_string(),
        },
        unconstr_index_func,
    );

    let unconstr_fields_body = TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::list(Type::data()),
        fun: TypedExpr::local_var(
            CONSTR_FIELDS_EXPOSER,
            Type::function(vec![Type::data()], Type::list(Type::data())),
            Span::empty(),
        )
        .into(),
        args: vec![CallArg {
            label: None,
            location: Span::empty(),
            value: TypedExpr::Var {
                location: Span::empty(),
                constructor: ValueConstructor {
                    public: true,
                    tipo: Type::data(),
                    variant: ValueConstructorVariant::LocalVariable {
                        location: Span::empty(),
                    },
                },
                name: "constr".to_string(),
            },
        }],
    };

    let unconstr_fields_func = Function {
        arguments: vec![TypedArg {
            arg_name: ArgName::Named {
                name: "constr".to_string(),
                label: "constr".to_string(),
                location: Span::empty(),
            },
            is_validator_param: false,
            doc: None,
            location: Span::empty(),
            annotation: None,
            tipo: Type::data(),
        }],
        on_test_failure: OnTestFailure::FailImmediately,
        doc: Some(
            indoc::indoc! {
                r#"
                /// Access the fields of a constr typed as Data. Fails if the Data object is not a constr.
                "#
            }.to_string()
        ),
        location: Span::empty(),
        name: "unconstr_fields".to_string(),
        public: true,
        return_annotation: None,
        return_type: Type::list(Type::data()),
        end_position: 0,
        body: unconstr_fields_body,
    };

    functions.insert(
        FunctionAccessKey {
            module_name: "aiken/builtin".to_string(),
            function_name: "unconstr_fields".to_string(),
        },
        unconstr_fields_func,
    );

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
                tipo: Type::bool(),
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
            return_type: Type::bool(),
            end_position: 0,
            body: TypedExpr::UnOp {
                location: Span::empty(),
                tipo: Type::bool(),
                op: UnOp::Not,
                value: Box::new(TypedExpr::Var {
                    location: Span::empty(),
                    constructor: ValueConstructor {
                        public: true,
                        tipo: Type::bool(),
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
    let a_var = Type::generic_var(id_gen.next());

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
    let a_var = Type::generic_var(id_gen.next());
    let b_var = Type::generic_var(id_gen.next());

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
    let a_var = Type::generic_var(id_gen.next());
    let b_var = Type::generic_var(id_gen.next());
    let c_var = Type::generic_var(id_gen.next());

    let input_type = Type::function(vec![a_var.clone(), b_var.clone()], c_var.clone());
    let return_type = Type::function(vec![b_var.clone(), a_var.clone()], c_var.clone());

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

    functions.insert(
        FunctionAccessKey {
            module_name: "".to_string(),
            function_name: "enumerate".to_string(),
        },
        aiken_fn!(
            &module_types,
            &id_gen,
            r#"
                fn enumerate(
                  self: List<a>,
                  zero: b,
                  with: fn(a, b) -> b,
                  last: fn(a, b) -> b,
                ) -> b {
                  when self is {
                    [] -> zero
                    [x] -> last(x, zero)
                    [x, ..xs] -> with(x, enumerate(xs, zero, with, last))
                  }
                }
            "#
        ),
    );

    functions.insert(
        FunctionAccessKey {
            module_name: "".to_string(),
            function_name: "encode_base16".to_string(),
        },
        aiken_fn!(
            &module_types,
            &id_gen,
            r#"
                use aiken/builtin

                fn encode_base16(bytes: ByteArray, ix: Int, builder: ByteArray) -> ByteArray {
                  if ix < 0 {
                    builder
                  } else {
                    let byte = builtin.index_bytearray(bytes, ix)
                    let msb = byte / 16
                    let lsb = byte % 16
                    let builder =
                      builtin.cons_bytearray(
                        msb + if msb < 10 {
                          48
                        } else {
                          55
                        },
                        builtin.cons_bytearray(
                          lsb + if lsb < 10 {
                            48
                          } else {
                            55
                          },
                          builder,
                        ),
                      )
                    encode_base16(bytes, ix - 1, builder)
                  }
                }
            "#
        ),
    );

    functions.insert(
        FunctionAccessKey {
            module_name: "".to_string(),
            function_name: "do_from_int".to_string(),
        },
        aiken_fn!(
            &module_types,
            &id_gen,
            r#"
                use aiken/builtin

                fn do_from_int(i: Int, digits: ByteArray) -> ByteArray {
                  if i <= 0 {
                    digits
                  } else {
                    do_from_int(
                      builtin.quotient_integer(i, 10),
                      builtin.cons_bytearray(builtin.remainder_integer(i, 10) + 48, digits),
                    )
                  }
                }
            "#
        ),
    );

    functions.insert(
        FunctionAccessKey {
            module_name: "".to_string(),
            function_name: "from_int".to_string(),
        },
        aiken_fn!(
            &module_types,
            &id_gen,
            r#"
                use aiken/builtin

                /// Encode an integer into UTF-8.
                fn from_int(i: Int, digits: ByteArray) -> ByteArray {
                  if i == 0 {
                    builtin.append_bytearray(#"30", digits)
                  } else if i < 0 {
                    builtin.append_bytearray(#"2d", from_int(-i, digits))
                  } else {
                    do_from_int(
                      builtin.quotient_integer(i, 10),
                      builtin.cons_bytearray(builtin.remainder_integer(i, 10) + 48, digits),
                    )
                  }
                }
            "#
        ),
    );

    functions.insert(
        FunctionAccessKey {
            module_name: "".to_string(),
            function_name: "diagnostic".to_string(),
        },
        aiken_fn!(
            &module_types,
            &id_gen,
            r#"
              use aiken/builtin

              fn diagnostic(self: Data, builder: ByteArray) -> ByteArray {
                builtin.choose_data(
                  self,
                  {
                    let Pair(constr, fields) = builtin.un_constr_data(self)

                    let builder =
                      when fields is {
                        [] -> builtin.append_bytearray(#"5b5d29", builder)
                        _ -> {
                          let bytes =
                            enumerate(
                              fields,
                              builtin.append_bytearray(#"5d29", builder),
                              fn(e: Data, st: ByteArray) {
                                diagnostic(e, builtin.append_bytearray(#"2c20", st))
                              },
                              fn(e: Data, st: ByteArray) { diagnostic(e, st) },
                            )
                          builtin.append_bytearray(#"5b5f20", bytes)
                        }
                      }

                    let constr_tag =
                      if constr < 7 {
                        121 + constr
                      } else if constr < 128 {
                        1280 + constr - 7
                      } else {
                        fail @"What are you doing? No I mean, seriously."
                      }

                    builder
                      |> builtin.append_bytearray(#"28", _)
                      |> from_int(constr_tag, _)
                  },
                  {
                    let elems = builtin.un_map_data(self)
                    when elems is {
                      [] -> builtin.append_bytearray(#"7b7d", builder)
                      _ -> {
                        let bytes =
                          enumerate(
                            elems,
                            builtin.append_bytearray(#"207d", builder),
                            fn(e: Pair<Data, Data>, st: ByteArray) {
                              let value = diagnostic(e.2nd, builtin.append_bytearray(#"2c20", st))
                              diagnostic(e.1st, builtin.append_bytearray(#"3a20", value))
                            },
                            fn(e: Pair<Data, Data>, st: ByteArray) {
                              let value = diagnostic(e.2nd, st)
                              diagnostic(e.1st, builtin.append_bytearray(#"3a20", value))
                            },
                          )
                        builtin.append_bytearray(#"7b5f20", bytes)
                      }
                    }
                  },
                  {
                    let elems = builtin.un_list_data(self)
                    when elems is {
                      [] -> builtin.append_bytearray(#"5b5d", builder)
                      _ -> {
                        let bytes =
                          enumerate(
                            elems,
                            builtin.append_bytearray(#"5d", builder),
                            fn(e: Data, st: ByteArray) {
                              diagnostic(e, builtin.append_bytearray(#"2c20", st))
                            },
                            fn(e: Data, st: ByteArray) { diagnostic(e, st) },
                          )
                        builtin.append_bytearray(#"5b5f20", bytes)
                      }
                    }
                  },
                  self
                    |> builtin.un_i_data
                    |> from_int(builder),
                  {
                    let bytes = builtin.un_b_data(self)
                    bytes
                      |> encode_base16(
                          builtin.length_of_bytearray(bytes) - 1,
                          builtin.append_bytearray(#"27", builder),
                        )
                      |> builtin.append_bytearray(#"6827", _)
                  },
                )
              }
            "#
        ),
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
            defined_type: well_known::DATA.to_string(),
        },
        data_data_type,
    );

    // Void
    let void_data_type = TypedDataType::void();
    data_types.insert(
        DataTypeKey {
            module_name: "".to_string(),
            defined_type: well_known::VOID.to_string(),
        },
        void_data_type,
    );

    // Ordering
    let ordering_data_type = TypedDataType::ordering();
    data_types.insert(
        DataTypeKey {
            module_name: "".to_string(),
            defined_type: well_known::ORDERING.to_string(),
        },
        ordering_data_type,
    );

    // Bool
    let bool_data_type = TypedDataType::bool();
    data_types.insert(
        DataTypeKey {
            module_name: "".to_string(),
            defined_type: well_known::BOOL.to_string(),
        },
        bool_data_type,
    );

    // Option
    let option_data_type = TypedDataType::option(Type::generic_var(id_gen.next()));
    data_types.insert(
        DataTypeKey {
            module_name: "".to_string(),
            defined_type: well_known::OPTION.to_string(),
        },
        option_data_type,
    );

    // Never
    data_types.insert(
        DataTypeKey {
            module_name: "".to_string(),
            defined_type: well_known::NEVER.to_string(),
        },
        TypedDataType::never(),
    );

    // PRNG
    let prng_data_type = TypedDataType::prng();
    data_types.insert(
        DataTypeKey {
            module_name: "".to_string(),
            defined_type: well_known::PRNG.to_string(),
        },
        prng_data_type,
    );

    // __ScriptPurpose
    let script_purpose_data_type = TypedDataType::script_purpose();
    data_types.insert(
        DataTypeKey {
            module_name: "".to_string(),
            defined_type: well_known::SCRIPT_PURPOSE.to_string(),
        },
        script_purpose_data_type,
    );

    // __ScriptContext
    let script_context_data_type = TypedDataType::script_context();
    data_types.insert(
        DataTypeKey {
            module_name: "".to_string(),
            defined_type: well_known::SCRIPT_CONTEXT.to_string(),
        },
        script_context_data_type,
    );

    data_types
}

// ----------------------------------------------------------------------------
// TypedDataTypes
//
// TODO: Rewrite in terms of ValueConstructor to avoid duplication and ensure
// consistency with prelude definitions.

impl TypedDataType {
    pub fn data() -> Self {
        DataType::known_enum(well_known::DATA, &[])
    }

    pub fn void() -> Self {
        DataType::known_enum(well_known::VOID, well_known::VOID_CONSTRUCTORS)
    }

    pub fn bool() -> Self {
        DataType::known_enum(well_known::BOOL, well_known::BOOL_CONSTRUCTORS)
    }

    pub fn script_purpose() -> Self {
        DataType::known_enum(
            well_known::SCRIPT_PURPOSE,
            well_known::SCRIPT_PURPOSE_CONSTRUCTORS,
        )
    }

    pub fn script_context() -> Self {
        DataType::known_enum(
            well_known::SCRIPT_CONTEXT,
            well_known::SCRIPT_CONTEXT_CONSTRUCTORS,
        )
    }

    pub fn prng() -> Self {
        let bytearray_arg = |label: &str| RecordConstructorArg {
            label: Some(label.to_string()),
            doc: None,
            annotation: Annotation::bytearray(Span::empty()),
            location: Span::empty(),
            tipo: Type::byte_array(),
        };

        let int_arg = |label: &str| RecordConstructorArg {
            label: Some(label.to_string()),
            doc: None,
            annotation: Annotation::int(Span::empty()),
            location: Span::empty(),
            tipo: Type::int(),
        };

        DataType::known_data_type(
            well_known::PRNG,
            &[
                RecordConstructor::known_record(
                    well_known::PRNG_CONSTRUCTORS[0],
                    &[bytearray_arg("seed"), bytearray_arg("choices")],
                ),
                RecordConstructor::known_record(
                    well_known::PRNG_CONSTRUCTORS[1],
                    &[int_arg("cursor"), bytearray_arg("choices")],
                ),
            ],
        )
    }

    pub fn ordering() -> Self {
        DataType::known_enum(well_known::ORDERING, well_known::ORDERING_CONSTRUCTORS)
    }

    pub fn option(tipo: Rc<Type>) -> Self {
        DataType {
            constructors: vec![
                RecordConstructor {
                    location: Span::empty(),
                    name: well_known::OPTION_CONSTRUCTORS[0].to_string(),
                    arguments: vec![RecordConstructorArg {
                        label: None,
                        annotation: Annotation::Var {
                            location: Span::empty(),
                            name: "a".to_string(),
                        },
                        location: Span::empty(),
                        tipo: tipo.clone(),
                        doc: None,
                    }],
                    doc: None,
                    sugar: false,
                },
                RecordConstructor {
                    location: Span::empty(),
                    name: well_known::OPTION_CONSTRUCTORS[1].to_string(),
                    arguments: vec![],
                    doc: None,
                    sugar: false,
                },
            ],
            doc: None,
            location: Span::empty(),
            name: well_known::OPTION.to_string(),
            opaque: false,
            parameters: vec!["a".to_string()],
            public: true,
            typed_parameters: vec![tipo],
        }
    }

    pub fn never() -> Self {
        DataType::known_enum(well_known::NEVER, well_known::NEVER_CONSTRUCTORS)
    }
}
