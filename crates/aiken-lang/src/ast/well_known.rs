use crate::{
    ast::{Annotation, Span},
    tipo::{Type, TypeAliasAnnotation, TypeVar},
};
use std::{cell::RefCell, rc::Rc};

pub const BOOL: &str = "Bool";
pub const BOOL_CONSTRUCTORS: &[&str] = &["False", "True"];
pub const BYTE_ARRAY: &str = "ByteArray";
pub const DATA: &str = "Data";
pub const FUZZER: &str = "Fuzzer";
pub const SAMPLER: &str = "Sampler";
pub const G1_ELEMENT: &str = "G1Element";
pub const G2_ELEMENT: &str = "G2Element";
pub const INT: &str = "Int";
pub const LIST: &str = "List";
pub const MILLER_LOOP_RESULT: &str = "MillerLoopResult";
pub const OPTION: &str = "Option";
pub const OPTION_CONSTRUCTORS: &[&str] = &["Some", "None"];
pub const NEVER: &str = "Never";
pub const NEVER_CONSTRUCTORS: &[&str] = &["__hole", "Never"];
pub const ORDERING: &str = "Ordering";
pub const ORDERING_CONSTRUCTORS: &[&str] = &["Less", "Equal", "Greater"];
pub const PAIR: &str = "Pair";
pub const PAIRS: &str = "Pairs";
pub const PRNG: &str = "PRNG";
pub const PRNG_CONSTRUCTORS: &[&str] = &["Seeded", "Replayed"];
pub const REDEEMER_WRAPPER: &str = "RedeemerWrapper";
pub const STRING: &str = "String";
pub const VOID: &str = "Void";
pub const VOID_CONSTRUCTORS: &[&str] = &["Void"];

pub const SCRIPT_CONTEXT: &str = "__ScriptContext";
pub const SCRIPT_CONTEXT_CONSTRUCTORS: &[&str] = &["__ScriptContext"];
pub const SCRIPT_CONTEXT_TRANSACTION: &str = "__Transaction";
pub const SCRIPT_CONTEXT_REDEEMER: &str = "__Redeemer";
pub const SCRIPT_CONTEXT_PURPOSE: &str = "__ScriptPurpose";

pub const SCRIPT_PURPOSE: &str = "__ScriptPurpose";
pub const SCRIPT_PURPOSE_MINT: &str = "__Mint";
pub const SCRIPT_PURPOSE_SPEND: &str = "__Spend";
pub const SCRIPT_PURPOSE_WITHDRAW: &str = "__Withdraw";
pub const SCRIPT_PURPOSE_PUBLISH: &str = "__Publish";
pub const SCRIPT_PURPOSE_VOTE: &str = "__Vote";
pub const SCRIPT_PURPOSE_PROPOSE: &str = "__Propose";
pub const SCRIPT_PURPOSE_CONSTRUCTORS: &[&str] = &[
    SCRIPT_PURPOSE_MINT,
    SCRIPT_PURPOSE_SPEND,
    SCRIPT_PURPOSE_WITHDRAW,
    SCRIPT_PURPOSE_PUBLISH,
    SCRIPT_PURPOSE_VOTE,
    SCRIPT_PURPOSE_PROPOSE,
];

pub const VALIDATOR_ELSE: &str = "else";

// ----------------------------------------------------------------------------
// Types

impl Type {
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

    pub fn bool() -> Rc<Self> {
        Rc::new(Type::App {
            args: vec![],
            public: true,
            contains_opaque: false,
            name: BOOL.to_string(),
            module: "".to_string(),
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

    pub fn script_purpose() -> Rc<Type> {
        Rc::new(Type::App {
            args: vec![],
            public: true,
            contains_opaque: false,
            name: SCRIPT_PURPOSE.to_string(),
            module: "".to_string(),
            alias: None,
        })
    }

    pub fn script_context() -> Rc<Type> {
        Rc::new(Type::App {
            args: vec![],
            public: true,
            contains_opaque: false,
            name: SCRIPT_CONTEXT.to_string(),
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
            name: PRNG.to_string(),
            arguments: vec![],
        };

        Rc::new(Type::Fn {
            args: vec![Type::prng()],
            ret: Type::option(Type::tuple(vec![Type::prng(), a])),
            alias: Some(
                TypeAliasAnnotation {
                    alias: FUZZER.to_string(),
                    parameters: vec!["a".to_string()],
                    annotation: Annotation::Fn {
                        location: Span::empty(),
                        arguments: vec![prng_annotation.clone()],
                        ret: Annotation::Constructor {
                            location: Span::empty(),
                            module: None,
                            name: OPTION.to_string(),
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

    pub fn sampler(a: Rc<Type>) -> Rc<Type> {
        let prng_annotation = Annotation::Constructor {
            location: Span::empty(),
            module: None,
            name: PRNG.to_string(),
            arguments: vec![],
        };

        Rc::new(Type::Fn {
            args: vec![Type::int()],
            ret: Type::fuzzer(a),
            alias: Some(
                TypeAliasAnnotation {
                    alias: SAMPLER.to_string(),
                    parameters: vec!["a".to_string()],
                    annotation: Annotation::Fn {
                        location: Span::empty(),
                        arguments: vec![Annotation::int(Span::empty())],
                        ret: Annotation::Fn {
                            location: Span::empty(),
                            arguments: vec![prng_annotation.clone()],
                            ret: Annotation::Constructor {
                                location: Span::empty(),
                                module: None,
                                name: OPTION.to_string(),
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
            args: vec![Type::pair(k, v)],
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
            contains_opaque: t.contains_opaque(),
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
            contains_opaque: a.contains_opaque(),
            name: OPTION.to_string(),
            module: "".to_string(),
            args: vec![a],
            alias: None,
        })
    }

    pub fn never() -> Rc<Type> {
        Rc::new(Type::App {
            public: true,
            contains_opaque: false,
            name: NEVER.to_string(),
            module: "".to_string(),
            args: vec![],
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
}
