use crate::{
    ast::{self, Constant, DataType, Function, ModuleConstant, Span, TypeAlias, Use},
    expr,
    parser::{self, token::Base},
};
use chumsky::prelude::*;
use indoc::indoc;
use pretty_assertions::assert_eq;
use vec1::vec1;

fn assert_definitions(code: &str, definitions: Vec<ast::UntypedDefinition>) {
    let (module, _extra) = parser::module(code, ast::ModuleKind::Validator).unwrap();

    assert_eq!(
        ast::UntypedModule {
            docs: vec![],
            kind: ast::ModuleKind::Validator,
            name: "".to_string(),
            type_info: (),
            definitions,
        },
        module
    )
}

#[test]
fn windows_newline() {
    let code = "use aiken/list\r\n";

    assert_definitions(
        code,
        vec![ast::UntypedDefinition::Use(Use {
            location: Span::new((), 0..14),
            module: vec!["aiken".to_string(), "list".to_string()],
            as_name: None,
            unqualified: vec![],
            package: (),
        })],
    )
}

#[test]
fn can_handle_comments_at_end_of_file() {
    let code = indoc! {r#"
       use aiken

       // some comment
       // more comments"#};

    assert_definitions(
        code,
        vec![ast::UntypedDefinition::Use(Use {
            location: Span::new((), 0..9),
            module: vec!["aiken".to_string()],
            as_name: None,
            unqualified: vec![],
            package: (),
        })],
    )
}

#[test]
fn type_annotation_with_module_prefix() {
    let code = indoc! {r#"
       use aiken

       pub fn go() -> aiken.Option<Int> {
         False
       }
    "#};

    assert_definitions(
        code,
        vec![
            ast::UntypedDefinition::Use(ast::Use {
                as_name: None,
                location: Span::new((), 0..9),
                module: vec!["aiken".to_string()],
                package: (),
                unqualified: vec![],
            }),
            ast::UntypedDefinition::Fn(ast::Function {
                arguments: vec![],
                body: expr::UntypedExpr::Var {
                    location: Span::new((), 48..53),
                    name: "False".to_string(),
                },
                doc: None,
                location: Span::new((), 11..43),
                name: "go".to_string(),
                public: true,
                return_annotation: Some(ast::Annotation::Constructor {
                    location: Span::new((), 26..43),
                    module: Some("aiken".to_string()),
                    name: "Option".to_string(),
                    arguments: vec![ast::Annotation::Constructor {
                        location: Span::new((), 39..42),
                        module: None,
                        name: "Int".to_string(),
                        arguments: vec![],
                    }],
                }),
                return_type: (),
                end_position: 54,
                can_error: true,
            }),
        ],
    )
}

#[test]
fn test_fail() {
    let code = indoc! {r#"
       !test invalid_inputs() {
         expect True = False

         False
       }
    "#};

    assert_definitions(
        code,
        vec![ast::UntypedDefinition::Test(ast::Function {
            arguments: vec![],
            body: expr::UntypedExpr::Sequence {
                location: Span::new((), 27..55),
                expressions: vec![
                    expr::UntypedExpr::Assignment {
                        location: Span::new((), 27..46),
                        value: Box::new(expr::UntypedExpr::Var {
                            location: Span::new((), 41..46),
                            name: "False".to_string(),
                        }),
                        pattern: ast::UntypedPattern::Constructor {
                            is_record: false,
                            location: Span::new((), 34..38),
                            name: "True".to_string(),
                            arguments: vec![],
                            module: None,
                            constructor: (),
                            with_spread: false,
                            tipo: (),
                        },
                        kind: ast::AssignmentKind::Expect,
                        annotation: None,
                    },
                    expr::UntypedExpr::Var {
                        location: Span::new((), 50..55),
                        name: "False".to_string(),
                    },
                ],
            },
            doc: None,
            location: Span::new((), 0..22),
            name: "invalid_inputs".to_string(),
            public: false,
            return_annotation: None,
            return_type: (),
            end_position: 56,
            can_error: true,
        })],
    );
}

#[test]
fn validator() {
    let code = indoc! {r#"
        validator {
          fn foo(datum, rdmr, ctx) {
            True
          }
        }
    "#};

    assert_definitions(
        code,
        vec![ast::UntypedDefinition::Validator(ast::Validator {
            doc: None,
            end_position: 54,
            fun: Function {
                can_error: true,
                arguments: vec![
                    ast::Arg {
                        arg_name: ast::ArgName::Named {
                            name: "datum".to_string(),
                            label: "datum".to_string(),
                            location: Span::new((), 21..26),
                            is_validator_param: false,
                        },
                        location: Span::new((), 21..26),
                        annotation: None,
                        tipo: (),
                    },
                    ast::Arg {
                        arg_name: ast::ArgName::Named {
                            name: "rdmr".to_string(),
                            label: "rdmr".to_string(),
                            location: Span::new((), 28..32),
                            is_validator_param: false,
                        },
                        location: Span::new((), 28..32),
                        annotation: None,
                        tipo: (),
                    },
                    ast::Arg {
                        arg_name: ast::ArgName::Named {
                            name: "ctx".to_string(),
                            label: "ctx".to_string(),
                            location: Span::new((), 34..37),
                            is_validator_param: false,
                        },
                        location: Span::new((), 34..37),
                        annotation: None,
                        tipo: (),
                    },
                ],
                body: expr::UntypedExpr::Var {
                    location: Span::new((), 45..49),
                    name: "True".to_string(),
                },
                doc: None,
                location: Span::new((), 14..38),
                name: "foo".to_string(),
                public: false,
                return_annotation: None,
                return_type: (),
                end_position: 52,
            },
            other_fun: None,
            location: Span::new((), 0..9),
            params: vec![],
        })],
    )
}

#[test]
fn double_validator() {
    let code = indoc! {r#"
        validator {
          fn foo(datum, rdmr, ctx) {
            True
          }

          fn bar(rdmr, ctx) {
            True
          }
        }
    "#};

    assert_definitions(
        code,
        vec![ast::UntypedDefinition::Validator(ast::Validator {
            doc: None,
            end_position: 90,
            fun: Function {
                can_error: true,
                arguments: vec![
                    ast::Arg {
                        arg_name: ast::ArgName::Named {
                            name: "datum".to_string(),
                            label: "datum".to_string(),
                            location: Span::new((), 21..26),
                            is_validator_param: false,
                        },
                        location: Span::new((), 21..26),
                        annotation: None,
                        tipo: (),
                    },
                    ast::Arg {
                        arg_name: ast::ArgName::Named {
                            name: "rdmr".to_string(),
                            label: "rdmr".to_string(),
                            location: Span::new((), 28..32),
                            is_validator_param: false,
                        },
                        location: Span::new((), 28..32),
                        annotation: None,
                        tipo: (),
                    },
                    ast::Arg {
                        arg_name: ast::ArgName::Named {
                            name: "ctx".to_string(),
                            label: "ctx".to_string(),
                            location: Span::new((), 34..37),
                            is_validator_param: false,
                        },
                        location: Span::new((), 34..37),
                        annotation: None,
                        tipo: (),
                    },
                ],
                body: expr::UntypedExpr::Var {
                    location: Span::new((), 45..49),
                    name: "True".to_string(),
                },
                doc: None,
                location: Span::new((), 14..38),
                name: "foo".to_string(),
                public: false,
                return_annotation: None,
                return_type: (),
                end_position: 52,
            },
            other_fun: Some(Function {
                can_error: true,
                arguments: vec![
                    ast::Arg {
                        arg_name: ast::ArgName::Named {
                            name: "rdmr".to_string(),
                            label: "rdmr".to_string(),
                            location: Span::new((), 64..68),
                            is_validator_param: false,
                        },
                        location: Span::new((), 64..68),
                        annotation: None,
                        tipo: (),
                    },
                    ast::Arg {
                        arg_name: ast::ArgName::Named {
                            name: "ctx".to_string(),
                            label: "ctx".to_string(),
                            location: Span::new((), 70..73),
                            is_validator_param: false,
                        },
                        location: Span::new((), 70..73),
                        annotation: None,
                        tipo: (),
                    },
                ],
                body: expr::UntypedExpr::Var {
                    location: Span::new((), 81..85),
                    name: "True".to_string(),
                },
                doc: None,
                location: Span::new((), 57..74),
                name: "bar".to_string(),
                public: false,
                return_annotation: None,
                return_type: (),
                end_position: 88,
            }),
            location: Span::new((), 0..9),
            params: vec![],
        })],
    )
}

#[test]
fn import() {
    let code = indoc! {r#"
        use std/list
    "#};

    assert_definitions(
        code,
        vec![ast::UntypedDefinition::Use(Use {
            location: Span::new((), 0..12),
            module: vec!["std".to_string(), "list".to_string()],
            as_name: None,
            unqualified: vec![],
            package: (),
        })],
    )
}

#[test]
fn unqualified_imports() {
    let code = indoc! {r#"
        use std/address.{Address as A, thing as w}
    "#};

    assert_definitions(
        code,
        vec![ast::UntypedDefinition::Use(Use {
            location: Span::new((), 0..42),
            module: vec!["std".to_string(), "address".to_string()],
            as_name: None,
            unqualified: vec![
                ast::UnqualifiedImport {
                    as_name: Some("A".to_string()),
                    location: Span::new((), 17..29),
                    layer: Default::default(),
                    name: "Address".to_string(),
                },
                ast::UnqualifiedImport {
                    as_name: Some("w".to_string()),
                    location: Span::new((), 31..41),
                    layer: Default::default(),
                    name: "thing".to_string(),
                },
            ],
            package: (),
        })],
    )
}

#[test]
fn import_alias() {
    let code = indoc! {r#"
        use std/tx as t
    "#};

    assert_definitions(
        code,
        vec![ast::UntypedDefinition::Use(Use {
            location: Span::new((), 0..15),
            module: vec!["std".to_string(), "tx".to_string()],
            as_name: Some("t".to_string()),
            unqualified: vec![],
            package: (),
        })],
    )
}

#[test]
fn custom_type() {
    let code = indoc! {r#"
        type Option<a> {
          Some(a, Int)
          None
          Wow { name: Int, age: Int }
        }
    "#};

    assert_definitions(
        code,
        vec![ast::UntypedDefinition::DataType(DataType {
            constructors: vec![
                ast::RecordConstructor {
                    location: Span::new((), 19..31),
                    name: "Some".to_string(),
                    arguments: vec![
                        ast::RecordConstructorArg {
                            label: None,
                            annotation: ast::Annotation::Var {
                                location: Span::new((), 24..25),
                                name: "a".to_string(),
                            },
                            location: Span::new((), 24..25),
                            tipo: (),
                            doc: None,
                        },
                        ast::RecordConstructorArg {
                            label: None,
                            annotation: ast::Annotation::Constructor {
                                location: Span::new((), 27..30),
                                module: None,
                                name: "Int".to_string(),
                                arguments: vec![],
                            },
                            location: Span::new((), 27..30),
                            tipo: (),
                            doc: None,
                        },
                    ],
                    doc: None,
                    sugar: false,
                },
                ast::RecordConstructor {
                    location: Span::new((), 34..38),
                    name: "None".to_string(),
                    arguments: vec![],
                    doc: None,
                    sugar: false,
                },
                ast::RecordConstructor {
                    location: Span::new((), 41..68),
                    name: "Wow".to_string(),
                    arguments: vec![
                        ast::RecordConstructorArg {
                            label: Some("name".to_string()),
                            annotation: ast::Annotation::Constructor {
                                location: Span::new((), 53..56),
                                module: None,
                                name: "Int".to_string(),
                                arguments: vec![],
                            },
                            location: Span::new((), 47..56),
                            tipo: (),
                            doc: None,
                        },
                        ast::RecordConstructorArg {
                            label: Some("age".to_string()),
                            annotation: ast::Annotation::Constructor {
                                location: Span::new((), 63..66),
                                module: None,
                                name: "Int".to_string(),
                                arguments: vec![],
                            },
                            location: Span::new((), 58..66),
                            tipo: (),
                            doc: None,
                        },
                    ],
                    doc: None,
                    sugar: false,
                },
            ],
            doc: None,
            location: Span::new((), 0..70),
            name: "Option".to_string(),
            opaque: false,
            parameters: vec!["a".to_string()],
            public: false,
            typed_parameters: vec![],
        })],
    )
}

#[test]
fn opaque_type() {
    let code = indoc! {r#"
        pub opaque type User {
          name: _w
        }
    "#};

    assert_definitions(
        code,
        vec![ast::UntypedDefinition::DataType(DataType {
            constructors: vec![ast::RecordConstructor {
                location: Span::new((), 21..35),
                name: "User".to_string(),
                arguments: vec![ast::RecordConstructorArg {
                    label: Some("name".to_string()),
                    annotation: ast::Annotation::Hole {
                        location: Span::new((), 31..33),
                        name: "_w".to_string(),
                    },
                    location: Span::new((), 25..33),
                    tipo: (),
                    doc: None,
                }],
                doc: None,
                sugar: true,
            }],
            doc: None,
            location: Span::new((), 0..35),
            name: "User".to_string(),
            opaque: true,
            parameters: vec![],
            public: true,
            typed_parameters: vec![],
        })],
    )
}

#[test]
fn type_alias() {
    let code = indoc! {r#"
        type Thing = Option<Int>
    "#};

    assert_definitions(
        code,
        vec![ast::UntypedDefinition::TypeAlias(TypeAlias {
            alias: "Thing".to_string(),
            annotation: ast::Annotation::Constructor {
                location: Span::new((), 13..24),
                module: None,
                name: "Option".to_string(),
                arguments: vec![ast::Annotation::Constructor {
                    location: Span::new((), 20..23),
                    module: None,
                    name: "Int".to_string(),
                    arguments: vec![],
                }],
            },
            doc: None,
            location: Span::new((), 0..24),
            parameters: vec![],
            public: false,
            tipo: (),
        })],
    )
}

#[test]
fn pub_type_alias() {
    let code = indoc! {r#"
        pub type Me = Option<String>
    "#};

    assert_definitions(
        code,
        vec![ast::UntypedDefinition::TypeAlias(TypeAlias {
            alias: "Me".to_string(),
            annotation: ast::Annotation::Constructor {
                location: Span::new((), 14..28),
                module: None,
                name: "Option".to_string(),
                arguments: vec![ast::Annotation::Constructor {
                    location: Span::new((), 21..27),
                    module: None,
                    name: "String".to_string(),
                    arguments: vec![],
                }],
            },
            doc: None,
            location: Span::new((), 0..28),
            parameters: vec![],
            public: true,
            tipo: (),
        })],
    )
}

#[test]
fn empty_function() {
    let code = indoc! {r#"
        pub fn run() {}
    "#};

    assert_definitions(
        code,
        vec![ast::UntypedDefinition::Fn(Function {
            can_error: true,
            arguments: vec![],
            body: expr::UntypedExpr::Trace {
                kind: ast::TraceKind::Todo,
                location: Span::new((), 0..15),
                text: Box::new(expr::UntypedExpr::String {
                    value: "aiken::todo".to_string(),
                    location: Span::new((), 0..15),
                }),
                then: Box::new(expr::UntypedExpr::ErrorTerm {
                    location: Span::new((), 0..15),
                }),
            },
            doc: None,
            location: Span::new((), 0..12),
            name: "run".to_string(),
            public: true,
            return_annotation: None,
            return_type: (),
            end_position: 14,
        })],
    )
}

#[test]
fn expect() {
    let code = indoc! {r#"
        pub fn run() {
            expect Some(x) = something.field
            x.other_field
        }
    "#};

    assert_definitions(
        code,
        vec![ast::UntypedDefinition::Fn(Function {
            can_error: true,
            arguments: vec![],
            body: expr::UntypedExpr::Sequence {
                location: Span::new((), 19..69),
                expressions: vec![
                    expr::UntypedExpr::Assignment {
                        location: Span::new((), 19..51),
                        value: expr::UntypedExpr::FieldAccess {
                            location: Span::new((), 36..51),
                            label: "field".to_string(),
                            container: expr::UntypedExpr::Var {
                                location: Span::new((), 36..45),
                                name: "something".to_string(),
                            }
                            .into(),
                        }
                        .into(),
                        pattern: ast::Pattern::Constructor {
                            is_record: false,
                            location: Span::new((), 26..33),
                            name: "Some".to_string(),
                            arguments: vec![ast::CallArg {
                                label: None,
                                location: Span::new((), 31..32),
                                value: ast::Pattern::Var {
                                    location: Span::new((), 31..32),
                                    name: "x".to_string(),
                                },
                            }],
                            module: None,
                            constructor: (),
                            with_spread: false,
                            tipo: (),
                        },
                        kind: ast::AssignmentKind::Expect,
                        annotation: None,
                    },
                    expr::UntypedExpr::FieldAccess {
                        location: Span::new((), 56..69),
                        label: "other_field".to_string(),
                        container: expr::UntypedExpr::Var {
                            location: Span::new((), 56..57),
                            name: "x".to_string(),
                        }
                        .into(),
                    },
                ],
            },
            doc: None,

            location: Span::new((), 0..12),
            name: "run".to_string(),
            public: true,
            return_annotation: None,
            return_type: (),
            end_position: 70,
        })],
    )
}

#[test]
fn plus_binop() {
    let code = indoc! {r#"
        pub fn add_one(a) -> Int {
          a + 1
        }
    "#};

    assert_definitions(
        code,
        vec![ast::UntypedDefinition::Fn(Function {
            can_error: true,
            arguments: vec![ast::Arg {
                arg_name: ast::ArgName::Named {
                    label: "a".to_string(),
                    name: "a".to_string(),
                    location: Span::new((), 15..16),
                    is_validator_param: false,
                },
                location: Span::new((), 15..16),
                annotation: None,
                tipo: (),
            }],
            body: expr::UntypedExpr::BinOp {
                location: Span::new((), 29..34),
                name: ast::BinOp::AddInt,
                left: Box::new(expr::UntypedExpr::Var {
                    location: Span::new((), 29..30),
                    name: "a".to_string(),
                }),
                right: Box::new(expr::UntypedExpr::Int {
                    location: Span::new((), 33..34),
                    value: "1".to_string(),
                    base: Base::Decimal {
                        numeric_underscore: false,
                    },
                }),
            },
            doc: None,
            location: Span::new((), 0..24),
            name: "add_one".to_string(),
            public: true,
            return_annotation: Some(ast::Annotation::Constructor {
                location: Span::new((), 21..24),
                module: None,
                name: "Int".to_string(),
                arguments: vec![],
            }),
            return_type: (),
            end_position: 35,
        })],
    )
}

#[test]
fn pipeline() {
    let code = indoc! {r#"
        pub fn thing(thing a: Int) {
          a + 2
          |> add_one
          |> add_one
        }
    "#};

    assert_definitions(
        code,
        vec![ast::UntypedDefinition::Fn(Function {
            can_error: true,
            arguments: vec![ast::Arg {
                arg_name: ast::ArgName::Named {
                    name: "a".to_string(),
                    label: "thing".to_string(),
                    location: Span::new((), 13..20),
                    is_validator_param: false,
                },
                location: Span::new((), 13..25),
                annotation: Some(ast::Annotation::Constructor {
                    location: Span::new((), 22..25),
                    module: None,
                    name: "Int".to_string(),
                    arguments: vec![],
                }),
                tipo: (),
            }],
            body: expr::UntypedExpr::PipeLine {
                one_liner: false,
                expressions: vec1::vec1![
                    expr::UntypedExpr::BinOp {
                        location: Span::new((), 31..36),
                        name: ast::BinOp::AddInt,
                        left: Box::new(expr::UntypedExpr::Var {
                            location: Span::new((), 31..32),
                            name: "a".to_string(),
                        }),
                        right: Box::new(expr::UntypedExpr::Int {
                            location: Span::new((), 35..36),
                            value: "2".to_string(),
                            base: Base::Decimal {
                                numeric_underscore: false,
                            },
                        }),
                    },
                    expr::UntypedExpr::Var {
                        location: Span::new((), 42..49),
                        name: "add_one".to_string(),
                    },
                    expr::UntypedExpr::Var {
                        location: Span::new((), 55..62),
                        name: "add_one".to_string(),
                    },
                ],
            },
            doc: None,
            location: Span::new((), 0..26),
            name: "thing".to_string(),
            public: true,
            return_annotation: None,
            return_type: (),
            end_position: 63,
        })],
    )
}

#[test]
fn if_expression() {
    let code = indoc! {r#"
        fn ifs() {
          if True {
            1 + 1
          } else if a < 4 {
            5
          } else if a || b {
            6
          } else {
            3
          }
        }
    "#};

    assert_definitions(
        code,
        vec![ast::UntypedDefinition::Fn(Function {
            can_error: true,
            arguments: vec![],
            body: expr::UntypedExpr::If {
                location: Span::new((), 13..106),
                branches: vec1::vec1![
                    ast::IfBranch {
                        condition: expr::UntypedExpr::Var {
                            location: Span::new((), 16..20),
                            name: "True".to_string(),
                        },
                        body: expr::UntypedExpr::BinOp {
                            location: Span::new((), 27..32),
                            name: ast::BinOp::AddInt,
                            left: Box::new(expr::UntypedExpr::Int {
                                location: Span::new((), 27..28),
                                value: "1".to_string(),
                                base: Base::Decimal {
                                    numeric_underscore: false,
                                },
                            }),
                            right: Box::new(expr::UntypedExpr::Int {
                                location: Span::new((), 31..32),
                                value: "1".to_string(),
                                base: Base::Decimal {
                                    numeric_underscore: false,
                                },
                            }),
                        },
                        location: Span::new((), 16..36),
                    },
                    ast::IfBranch {
                        condition: expr::UntypedExpr::BinOp {
                            location: Span::new((), 45..50),
                            name: ast::BinOp::LtInt,
                            left: Box::new(expr::UntypedExpr::Var {
                                location: Span::new((), 45..46),
                                name: "a".to_string(),
                            }),
                            right: Box::new(expr::UntypedExpr::Int {
                                location: Span::new((), 49..50),
                                value: "4".to_string(),
                                base: Base::Decimal {
                                    numeric_underscore: false,
                                },
                            }),
                        },
                        body: expr::UntypedExpr::Int {
                            location: Span::new((), 57..58),
                            value: "5".to_string(),
                            base: Base::Decimal {
                                numeric_underscore: false,
                            },
                        },
                        location: Span::new((), 45..62),
                    },
                    ast::IfBranch {
                        condition: expr::UntypedExpr::BinOp {
                            location: Span::new((), 71..77),
                            name: ast::BinOp::Or,
                            left: Box::new(expr::UntypedExpr::Var {
                                location: Span::new((), 71..72),
                                name: "a".to_string(),
                            }),
                            right: Box::new(expr::UntypedExpr::Var {
                                location: Span::new((), 76..77),
                                name: "b".to_string(),
                            }),
                        },
                        body: expr::UntypedExpr::Int {
                            location: Span::new((), 84..85),
                            value: "6".to_string(),
                            base: Base::Decimal {
                                numeric_underscore: false,
                            },
                        },
                        location: Span::new((), 71..89),
                    },
                ],
                final_else: Box::new(expr::UntypedExpr::Int {
                    location: Span::new((), 101..102),
                    value: "3".to_string(),
                    base: Base::Decimal {
                        numeric_underscore: false,
                    },
                }),
            },
            doc: None,
            location: Span::new((), 0..8),
            name: "ifs".to_string(),
            public: false,
            return_annotation: None,
            return_type: (),
            end_position: 107,
        })],
    )
}

#[test]
fn let_bindings() {
    let code = indoc! {r#"
        pub fn wow(a: Int) {
          let x =
            a + 2
            |> add_one
            |> add_one

          let thing = [ 1, 2, a ]

          let idk = thing

          y
        }
    "#};

    assert_definitions(
        code,
        vec![ast::UntypedDefinition::Fn(Function {
            can_error: true,
            arguments: vec![ast::Arg {
                arg_name: ast::ArgName::Named {
                    label: "a".to_string(),
                    name: "a".to_string(),
                    location: Span::new((), 11..12),
                    is_validator_param: false,
                },
                location: Span::new((), 11..17),
                annotation: Some(ast::Annotation::Constructor {
                    location: Span::new((), 14..17),
                    module: None,
                    name: "Int".to_string(),
                    arguments: vec![],
                }),
                tipo: (),
            }],
            body: expr::UntypedExpr::Sequence {
                location: Span::new((), 23..121),
                expressions: vec![
                    expr::UntypedExpr::Assignment {
                        location: Span::new((), 23..70),
                        value: Box::new(expr::UntypedExpr::PipeLine {
                            one_liner: false,
                            expressions: vec1::vec1![
                                expr::UntypedExpr::BinOp {
                                    location: Span::new((), 35..40),
                                    name: ast::BinOp::AddInt,
                                    left: Box::new(expr::UntypedExpr::Var {
                                        location: Span::new((), 35..36),
                                        name: "a".to_string(),
                                    }),
                                    right: Box::new(expr::UntypedExpr::Int {
                                        location: Span::new((), 39..40),
                                        value: "2".to_string(),
                                        base: Base::Decimal {
                                            numeric_underscore: false,
                                        },
                                    }),
                                },
                                expr::UntypedExpr::Var {
                                    location: Span::new((), 48..55),
                                    name: "add_one".to_string(),
                                },
                                expr::UntypedExpr::Var {
                                    location: Span::new((), 63..70),
                                    name: "add_one".to_string(),
                                },
                            ],
                        }),
                        pattern: ast::Pattern::Var {
                            location: Span::new((), 27..28),
                            name: "x".to_string(),
                        },
                        kind: ast::AssignmentKind::Let,
                        annotation: None,
                    },
                    expr::UntypedExpr::Assignment {
                        location: Span::new((), 74..97),
                        value: Box::new(expr::UntypedExpr::List {
                            location: Span::new((), 86..97),
                            elements: vec![
                                expr::UntypedExpr::Int {
                                    location: Span::new((), 88..89),
                                    value: "1".to_string(),
                                    base: Base::Decimal {
                                        numeric_underscore: false,
                                    },
                                },
                                expr::UntypedExpr::Int {
                                    location: Span::new((), 91..92),
                                    value: "2".to_string(),
                                    base: Base::Decimal {
                                        numeric_underscore: false,
                                    },
                                },
                                expr::UntypedExpr::Var {
                                    location: Span::new((), 94..95),
                                    name: "a".to_string(),
                                },
                            ],
                            tail: None,
                        }),
                        pattern: ast::Pattern::Var {
                            location: Span::new((), 78..83),
                            name: "thing".to_string(),
                        },
                        kind: ast::AssignmentKind::Let,
                        annotation: None,
                    },
                    expr::UntypedExpr::Assignment {
                        location: Span::new((), 101..116),
                        value: Box::new(expr::UntypedExpr::Var {
                            location: Span::new((), 111..116),
                            name: "thing".to_string(),
                        }),
                        pattern: ast::Pattern::Var {
                            location: Span::new((), 105..108),
                            name: "idk".to_string(),
                        },
                        kind: ast::AssignmentKind::Let,
                        annotation: None,
                    },
                    expr::UntypedExpr::Var {
                        location: Span::new((), 120..121),
                        name: "y".to_string(),
                    },
                ],
            },
            doc: None,
            location: Span::new((), 0..18),
            name: "wow".to_string(),
            public: true,
            return_annotation: None,
            return_type: (),
            end_position: 122,
        })],
    )
}

#[test]
fn block() {
    let code = indoc! {r#"
        pub fn wow2(a: Int){
          let b = {
            let x = 4

            x + 5
          }

          b
        }
    "#};

    assert_definitions(
        code,
        vec![ast::UntypedDefinition::Fn(Function {
            can_error: true,
            arguments: vec![ast::Arg {
                arg_name: ast::ArgName::Named {
                    label: "a".to_string(),
                    name: "a".to_string(),
                    location: Span::new((), 12..13),
                    is_validator_param: false,
                },
                location: Span::new((), 12..18),
                annotation: Some(ast::Annotation::Constructor {
                    location: Span::new((), 15..18),
                    module: None,
                    name: "Int".to_string(),
                    arguments: vec![],
                }),
                tipo: (),
            }],
            body: expr::UntypedExpr::Sequence {
                location: Span::new((), 23..66),
                expressions: vec![
                    expr::UntypedExpr::Assignment {
                        location: Span::new((), 23..61),
                        value: Box::new(expr::UntypedExpr::Sequence {
                            location: Span::new((), 37..57),
                            expressions: vec![
                                expr::UntypedExpr::Assignment {
                                    location: Span::new((), 37..46),
                                    value: Box::new(expr::UntypedExpr::Int {
                                        location: Span::new((), 45..46),
                                        value: "4".to_string(),
                                        base: Base::Decimal {
                                            numeric_underscore: false,
                                        },
                                    }),
                                    pattern: ast::Pattern::Var {
                                        location: Span::new((), 41..42),
                                        name: "x".to_string(),
                                    },
                                    kind: ast::AssignmentKind::Let,
                                    annotation: None,
                                },
                                expr::UntypedExpr::BinOp {
                                    location: Span::new((), 52..57),
                                    name: ast::BinOp::AddInt,
                                    left: Box::new(expr::UntypedExpr::Var {
                                        location: Span::new((), 52..53),
                                        name: "x".to_string(),
                                    }),
                                    right: Box::new(expr::UntypedExpr::Int {
                                        location: Span::new((), 56..57),
                                        value: "5".to_string(),
                                        base: Base::Decimal {
                                            numeric_underscore: false,
                                        },
                                    }),
                                },
                            ],
                        }),
                        pattern: ast::Pattern::Var {
                            location: Span::new((), 27..28),
                            name: "b".to_string(),
                        },
                        kind: ast::AssignmentKind::Let,
                        annotation: None,
                    },
                    expr::UntypedExpr::Var {
                        location: Span::new((), 65..66),
                        name: "b".to_string(),
                    },
                ],
            },
            doc: None,
            location: Span::new((), 0..19),
            name: "wow2".to_string(),
            public: true,
            return_annotation: None,
            return_type: (),
            end_position: 67,
        })],
    )
}

#[test]
fn when() {
    let code = indoc! {r#"
        pub fn wow2(a: Int){
          when a is {
            2 -> 3
            1 | 4 | 5 -> {
              let amazing = 5
              amazing
            }
            3 -> 9
            _ -> 4
          }
        }
    "#};

    assert_definitions(
        code,
        vec![ast::UntypedDefinition::Fn(Function {
            can_error: true,
            arguments: vec![ast::Arg {
                arg_name: ast::ArgName::Named {
                    label: "a".to_string(),
                    name: "a".to_string(),
                    location: Span::new((), 12..13),
                    is_validator_param: false,
                },
                location: Span::new((), 12..18),
                annotation: Some(ast::Annotation::Constructor {
                    location: Span::new((), 15..18),
                    module: None,
                    name: "Int".to_string(),
                    arguments: vec![],
                }),
                tipo: (),
            }],
            body: expr::UntypedExpr::When {
                location: Span::new((), 23..132),
                subject: Box::new(expr::UntypedExpr::Var {
                    location: Span::new((), 28..29),
                    name: "a".to_string(),
                }),
                clauses: vec![
                    ast::UntypedClause {
                        location: Span::new((), 39..45),
                        patterns: vec1![ast::Pattern::Int {
                            location: Span::new((), 39..40),
                            value: "2".to_string(),
                            base: Base::Decimal {
                                numeric_underscore: false,
                            },
                        }],
                        guard: None,
                        then: expr::UntypedExpr::Int {
                            location: Span::new((), 44..45),
                            value: "3".to_string(),
                            base: Base::Decimal {
                                numeric_underscore: false,
                            },
                        },
                    },
                    ast::UntypedClause {
                        location: Span::new((), 50..106),
                        patterns: vec1![
                            ast::Pattern::Int {
                                location: Span::new((), 50..51),
                                value: "1".to_string(),
                                base: Base::Decimal {
                                    numeric_underscore: false,
                                },
                            },
                            ast::Pattern::Int {
                                location: Span::new((), 54..55),
                                value: "4".to_string(),
                                base: Base::Decimal {
                                    numeric_underscore: false,
                                },
                            },
                            ast::Pattern::Int {
                                location: Span::new((), 58..59),
                                value: "5".to_string(),
                                base: Base::Decimal {
                                    numeric_underscore: false,
                                },
                            },
                        ],
                        guard: None,
                        then: expr::UntypedExpr::Sequence {
                            location: Span::new((), 71..100),
                            expressions: vec![
                                expr::UntypedExpr::Assignment {
                                    location: Span::new((), 71..86),
                                    value: Box::new(expr::UntypedExpr::Int {
                                        location: Span::new((), 85..86),
                                        value: "5".to_string(),
                                        base: Base::Decimal {
                                            numeric_underscore: false,
                                        },
                                    }),
                                    pattern: ast::Pattern::Var {
                                        location: Span::new((), 75..82),
                                        name: "amazing".to_string(),
                                    },
                                    kind: ast::AssignmentKind::Let,
                                    annotation: None,
                                },
                                expr::UntypedExpr::Var {
                                    location: Span::new((), 93..100),
                                    name: "amazing".to_string(),
                                },
                            ],
                        },
                    },
                    ast::UntypedClause {
                        location: Span::new((), 111..117),
                        patterns: vec1![ast::Pattern::Int {
                            location: Span::new((), 111..112),
                            value: "3".to_string(),
                            base: Base::Decimal {
                                numeric_underscore: false,
                            },
                        }],
                        guard: None,
                        then: expr::UntypedExpr::Int {
                            location: Span::new((), 116..117),
                            value: "9".to_string(),
                            base: Base::Decimal {
                                numeric_underscore: false,
                            },
                        },
                    },
                    ast::UntypedClause {
                        location: Span::new((), 122..128),
                        patterns: vec1![ast::Pattern::Discard {
                            name: "_".to_string(),
                            location: Span::new((), 122..123),
                        }],
                        guard: None,
                        then: expr::UntypedExpr::Int {
                            location: Span::new((), 127..128),
                            value: "4".to_string(),
                            base: Base::Decimal {
                                numeric_underscore: false,
                            },
                        },
                    },
                ],
            },
            doc: None,
            location: Span::new((), 0..19),
            name: "wow2".to_string(),
            public: true,
            return_annotation: None,
            return_type: (),
            end_position: 133,
        })],
    )
}

#[test]
fn anonymous_function() {
    let code = indoc! {r#"
        pub fn such() -> Int {
          let add_one = fn (a: Int) -> Int { a + 1 }

          2 |> add_one
        }
    "#};

    assert_definitions(
        code,
        vec![ast::UntypedDefinition::Fn(Function {
            can_error: true,
            arguments: vec![],
            body: expr::UntypedExpr::Sequence {
                location: Span::new((), 25..83),
                expressions: vec![
                    expr::UntypedExpr::Assignment {
                        location: Span::new((), 25..67),
                        value: Box::new(expr::UntypedExpr::Fn {
                            location: Span::new((), 39..67),
                            is_capture: false,
                            arguments: vec![ast::Arg {
                                arg_name: ast::ArgName::Named {
                                    label: "a".to_string(),
                                    name: "a".to_string(),
                                    location: Span::new((), 43..44),
                                    is_validator_param: false,
                                },
                                location: Span::new((), 43..49),
                                annotation: Some(ast::Annotation::Constructor {
                                    location: Span::new((), 46..49),
                                    module: None,
                                    name: "Int".to_string(),
                                    arguments: vec![],
                                }),
                                tipo: (),
                            }],
                            body: Box::new(expr::UntypedExpr::BinOp {
                                location: Span::new((), 60..65),
                                name: ast::BinOp::AddInt,
                                left: Box::new(expr::UntypedExpr::Var {
                                    location: Span::new((), 60..61),
                                    name: "a".to_string(),
                                }),
                                right: Box::new(expr::UntypedExpr::Int {
                                    location: Span::new((), 64..65),
                                    value: "1".to_string(),
                                    base: Base::Decimal {
                                        numeric_underscore: false,
                                    },
                                }),
                            }),
                            return_annotation: Some(ast::Annotation::Constructor {
                                location: Span::new((), 54..57),
                                module: None,
                                name: "Int".to_string(),
                                arguments: vec![],
                            }),
                        }),
                        pattern: ast::Pattern::Var {
                            location: Span::new((), 29..36),
                            name: "add_one".to_string(),
                        },
                        kind: ast::AssignmentKind::Let,
                        annotation: None,
                    },
                    expr::UntypedExpr::PipeLine {
                        one_liner: true,
                        expressions: vec1::vec1![
                            expr::UntypedExpr::Int {
                                location: Span::new((), 71..72),
                                value: "2".to_string(),
                                base: Base::Decimal {
                                    numeric_underscore: false,
                                },
                            },
                            expr::UntypedExpr::Var {
                                location: Span::new((), 76..83),
                                name: "add_one".to_string(),
                            },
                        ],
                    },
                ],
            },
            doc: None,
            location: Span::new((), 0..20),
            name: "such".to_string(),
            public: true,
            return_annotation: Some(ast::Annotation::Constructor {
                location: Span::new((), 17..20),
                module: None,
                name: "Int".to_string(),
                arguments: vec![],
            }),
            return_type: (),
            end_position: 84,
        })],
    )
}

#[test]
fn field_access() {
    let code = indoc! {r#"
        fn name(user: User) {
          user.name
        }
    "#};

    assert_definitions(
        code,
        vec![ast::UntypedDefinition::Fn(Function {
            can_error: true,
            arguments: vec![ast::Arg {
                arg_name: ast::ArgName::Named {
                    label: "user".to_string(),
                    name: "user".to_string(),
                    location: Span::new((), 8..12),
                    is_validator_param: false,
                },
                location: Span::new((), 8..18),
                annotation: Some(ast::Annotation::Constructor {
                    location: Span::new((), 14..18),
                    module: None,
                    name: "User".to_string(),
                    arguments: vec![],
                }),
                tipo: (),
            }],
            body: expr::UntypedExpr::FieldAccess {
                location: Span::new((), 24..33),
                label: "name".to_string(),
                container: Box::new(expr::UntypedExpr::Var {
                    location: Span::new((), 24..28),
                    name: "user".to_string(),
                }),
            },
            doc: None,
            location: Span::new((), 0..19),
            name: "name".to_string(),
            public: false,
            return_annotation: None,
            return_type: (),
            end_position: 34,
        })],
    )
}

#[test]
fn call() {
    let code = indoc! {r#"
        fn calls() {
          let x = add_one(3)

          let map_add_x = list.map(_, fn (y) { x + y })

          map_add_x([ 1, 2, 3 ])
        }
    "#};

    assert_definitions(
        code,
        vec![ast::UntypedDefinition::Fn(Function {
            can_error: true,
            arguments: vec![],
            body: expr::UntypedExpr::Sequence {
                location: Span::new((), 15..108),
                expressions: vec![
                    expr::UntypedExpr::Assignment {
                        location: Span::new((), 15..33),
                        value: Box::new(expr::UntypedExpr::Call {
                            arguments: vec![ast::CallArg {
                                label: None,
                                location: Span::new((), 31..32),
                                value: expr::UntypedExpr::Int {
                                    location: Span::new((), 31..32),
                                    value: "3".to_string(),
                                    base: Base::Decimal {
                                        numeric_underscore: false,
                                    },
                                },
                            }],
                            fun: Box::new(expr::UntypedExpr::Var {
                                location: Span::new((), 23..30),
                                name: "add_one".to_string(),
                            }),
                            location: Span::new((), 23..33),
                        }),
                        pattern: ast::Pattern::Var {
                            location: Span::new((), 19..20),
                            name: "x".to_string(),
                        },
                        kind: ast::AssignmentKind::Let,
                        annotation: None,
                    },
                    expr::UntypedExpr::Assignment {
                        location: Span::new((), 37..82),
                        value: Box::new(expr::UntypedExpr::Fn {
                            location: Span::new((), 53..82),
                            is_capture: true,
                            arguments: vec![ast::Arg {
                                arg_name: ast::ArgName::Named {
                                    label: "_capture__0".to_string(),
                                    name: "_capture__0".to_string(),
                                    location: Span::new((), 0..0),
                                    is_validator_param: false,
                                },
                                location: Span::new((), 0..0),
                                annotation: None,
                                tipo: (),
                            }],
                            body: Box::new(expr::UntypedExpr::Call {
                                arguments: vec![
                                    ast::CallArg {
                                        label: None,
                                        location: Span::new((), 62..63),
                                        value: expr::UntypedExpr::Var {
                                            location: Span::new((), 62..63),
                                            name: "_capture__0".to_string(),
                                        },
                                    },
                                    ast::CallArg {
                                        label: None,
                                        location: Span::new((), 65..81),
                                        value: expr::UntypedExpr::Fn {
                                            location: Span::new((), 65..81),
                                            is_capture: false,
                                            arguments: vec![ast::Arg {
                                                arg_name: ast::ArgName::Named {
                                                    label: "y".to_string(),
                                                    name: "y".to_string(),
                                                    location: Span::new((), 69..70),
                                                    is_validator_param: false,
                                                },
                                                location: Span::new((), 69..70),
                                                annotation: None,
                                                tipo: (),
                                            }],
                                            body: Box::new(expr::UntypedExpr::BinOp {
                                                location: Span::new((), 74..79),
                                                name: ast::BinOp::AddInt,
                                                left: Box::new(expr::UntypedExpr::Var {
                                                    location: Span::new((), 74..75),
                                                    name: "x".to_string(),
                                                }),
                                                right: Box::new(expr::UntypedExpr::Var {
                                                    location: Span::new((), 78..79),
                                                    name: "y".to_string(),
                                                }),
                                            }),
                                            return_annotation: None,
                                        },
                                    },
                                ],
                                fun: Box::new(expr::UntypedExpr::FieldAccess {
                                    location: Span::new((), 53..61),
                                    label: "map".to_string(),
                                    container: Box::new(expr::UntypedExpr::Var {
                                        location: Span::new((), 53..57),
                                        name: "list".to_string(),
                                    }),
                                }),
                                location: Span::new((), 53..82),
                            }),
                            return_annotation: None,
                        }),
                        pattern: ast::Pattern::Var {
                            location: Span::new((), 41..50),
                            name: "map_add_x".to_string(),
                        },
                        kind: ast::AssignmentKind::Let,
                        annotation: None,
                    },
                    expr::UntypedExpr::Call {
                        arguments: vec![ast::CallArg {
                            label: None,
                            location: Span::new((), 96..107),
                            value: expr::UntypedExpr::List {
                                location: Span::new((), 96..107),
                                elements: vec![
                                    expr::UntypedExpr::Int {
                                        location: Span::new((), 98..99),
                                        value: "1".to_string(),
                                        base: Base::Decimal {
                                            numeric_underscore: false,
                                        },
                                    },
                                    expr::UntypedExpr::Int {
                                        location: Span::new((), 101..102),
                                        value: "2".to_string(),
                                        base: Base::Decimal {
                                            numeric_underscore: false,
                                        },
                                    },
                                    expr::UntypedExpr::Int {
                                        location: Span::new((), 104..105),
                                        value: "3".to_string(),
                                        base: Base::Decimal {
                                            numeric_underscore: false,
                                        },
                                    },
                                ],
                                tail: None,
                            },
                        }],
                        fun: Box::new(expr::UntypedExpr::Var {
                            location: Span::new((), 86..95),
                            name: "map_add_x".to_string(),
                        }),
                        location: Span::new((), 86..108),
                    },
                ],
            },
            doc: None,
            location: Span::new((), 0..10),
            name: "calls".to_string(),
            public: false,
            return_annotation: None,
            return_type: (),
            end_position: 109,
        })],
    )
}

#[test]
fn record_update() {
    let code = indoc! {r#"
        fn update_name(user: User, name: ByteArray) -> User {
          User { ..user, name: "Aiken", age }
        }
    "#};

    assert_definitions(
        code,
        vec![ast::UntypedDefinition::Fn(Function {
            can_error: true,
            arguments: vec![
                ast::Arg {
                    arg_name: ast::ArgName::Named {
                        label: "user".to_string(),
                        name: "user".to_string(),
                        location: Span::new((), 15..19),
                        is_validator_param: false,
                    },
                    location: Span::new((), 15..25),
                    annotation: Some(ast::Annotation::Constructor {
                        location: Span::new((), 21..25),
                        module: None,
                        name: "User".to_string(),
                        arguments: vec![],
                    }),
                    tipo: (),
                },
                ast::Arg {
                    arg_name: ast::ArgName::Named {
                        label: "name".to_string(),
                        name: "name".to_string(),
                        location: Span::new((), 27..31),
                        is_validator_param: false,
                    },
                    location: Span::new((), 27..42),
                    annotation: Some(ast::Annotation::Constructor {
                        location: Span::new((), 33..42),
                        module: None,
                        name: "ByteArray".to_string(),
                        arguments: vec![],
                    }),
                    tipo: (),
                },
            ],
            body: expr::UntypedExpr::RecordUpdate {
                location: Span::new((), 56..91),
                constructor: Box::new(expr::UntypedExpr::Var {
                    location: Span::new((), 56..60),
                    name: "User".to_string(),
                }),
                spread: ast::RecordUpdateSpread {
                    base: Box::new(expr::UntypedExpr::Var {
                        location: Span::new((), 65..69),
                        name: "user".to_string(),
                    }),
                    location: Span::new((), 63..69),
                },
                arguments: vec![
                    ast::UntypedRecordUpdateArg {
                        label: "name".to_string(),
                        location: Span::new((), 71..84),
                        value: expr::UntypedExpr::ByteArray {
                            location: Span::new((), 77..84),
                            bytes: String::from("Aiken").into_bytes(),
                            preferred_format: ast::ByteArrayFormatPreference::Utf8String,
                        },
                    },
                    ast::UntypedRecordUpdateArg {
                        label: "age".to_string(),
                        location: Span::new((), 86..89),
                        value: expr::UntypedExpr::Var {
                            location: Span::new((), 86..89),
                            name: "age".to_string(),
                        },
                    },
                ],
            },
            doc: None,
            location: Span::new((), 0..51),
            name: "update_name".to_string(),
            public: false,
            return_annotation: Some(ast::Annotation::Constructor {
                location: Span::new((), 47..51),
                module: None,
                name: "User".to_string(),
                arguments: vec![],
            }),
            return_type: (),
            end_position: 92,
        })],
    )
}

#[test]
fn record_create_labeled() {
    let code = indoc! {r#"
        fn create() {
          User { name: "Aiken", age, thing: 2 }
        }
    "#};

    assert_definitions(
        code,
        vec![ast::UntypedDefinition::Fn(ast::Function {
            can_error: true,
            arguments: vec![],
            body: expr::UntypedExpr::Call {
                arguments: vec![
                    ast::CallArg {
                        label: Some("name".to_string()),
                        location: Span::new((), 23..36),
                        value: expr::UntypedExpr::ByteArray {
                            location: Span::new((), 29..36),
                            bytes: String::from("Aiken").into_bytes(),
                            preferred_format: ast::ByteArrayFormatPreference::Utf8String,
                        },
                    },
                    ast::CallArg {
                        label: Some("age".to_string()),
                        location: Span::new((), 38..41),
                        value: expr::UntypedExpr::Var {
                            location: Span::new((), 38..41),
                            name: "age".to_string(),
                        },
                    },
                    ast::CallArg {
                        label: Some("thing".to_string()),
                        location: Span::new((), 43..51),
                        value: expr::UntypedExpr::Int {
                            location: Span::new((), 50..51),
                            value: "2".to_string(),
                            base: Base::Decimal {
                                numeric_underscore: false,
                            },
                        },
                    },
                ],
                fun: Box::new(expr::UntypedExpr::Var {
                    location: Span::new((), 16..20),
                    name: "User".to_string(),
                }),
                location: Span::new((), 16..53),
            },
            doc: None,
            location: Span::new((), 0..11),
            name: "create".to_string(),
            public: false,
            return_annotation: None,
            return_type: (),
            end_position: 54,
        })],
    )
}

#[test]
fn record_create_labeled_with_field_access() {
    let code = indoc! {r#"
        fn create() {
          some_module.User { name: "Aiken", age, thing: 2 }
        }
    "#};

    assert_definitions(
        code,
        vec![ast::UntypedDefinition::Fn(ast::Function {
            can_error: true,
            arguments: vec![],
            body: expr::UntypedExpr::Call {
                arguments: vec![
                    ast::CallArg {
                        label: Some("name".to_string()),
                        location: Span::new((), 35..48),
                        value: expr::UntypedExpr::ByteArray {
                            location: Span::new((), 41..48),
                            bytes: String::from("Aiken").into_bytes(),
                            preferred_format: ast::ByteArrayFormatPreference::Utf8String,
                        },
                    },
                    ast::CallArg {
                        label: Some("age".to_string()),
                        location: Span::new((), 50..53),
                        value: expr::UntypedExpr::Var {
                            location: Span::new((), 50..53),
                            name: "age".to_string(),
                        },
                    },
                    ast::CallArg {
                        label: Some("thing".to_string()),
                        location: Span::new((), 55..63),
                        value: expr::UntypedExpr::Int {
                            location: Span::new((), 62..63),
                            value: "2".to_string(),
                            base: Base::Decimal {
                                numeric_underscore: false,
                            },
                        },
                    },
                ],
                fun: Box::new(expr::UntypedExpr::FieldAccess {
                    location: Span::new((), 16..32),
                    label: "User".to_string(),
                    container: Box::new(expr::UntypedExpr::Var {
                        location: Span::new((), 16..27),
                        name: "some_module".to_string(),
                    }),
                }),
                location: Span::new((), 16..65),
            },
            doc: None,
            location: Span::new((), 0..11),
            name: "create".to_string(),
            public: false,
            return_annotation: None,
            return_type: (),
            end_position: 66,
        })],
    )
}

#[test]
fn record_create_unlabeled() {
    let code = indoc! {r#"
        fn create() {
          some_module.Thing(1, a)
        }
    "#};

    assert_definitions(
        code,
        vec![ast::UntypedDefinition::Fn(ast::Function {
            can_error: true,
            arguments: vec![],
            body: expr::UntypedExpr::Call {
                arguments: vec![
                    ast::CallArg {
                        label: None,
                        location: Span::new((), 34..35),
                        value: expr::UntypedExpr::Int {
                            location: Span::new((), 34..35),
                            value: "1".to_string(),
                            base: Base::Decimal {
                                numeric_underscore: false,
                            },
                        },
                    },
                    ast::CallArg {
                        label: None,
                        location: Span::new((), 37..38),
                        value: expr::UntypedExpr::Var {
                            location: Span::new((), 37..38),
                            name: "a".to_string(),
                        },
                    },
                ],
                fun: Box::new(expr::UntypedExpr::FieldAccess {
                    location: Span::new((), 16..33),
                    label: "Thing".to_string(),
                    container: Box::new(expr::UntypedExpr::Var {
                        location: Span::new((), 16..27),
                        name: "some_module".to_string(),
                    }),
                }),
                location: Span::new((), 16..39),
            },
            doc: None,
            location: Span::new((), 0..11),
            name: "create".to_string(),
            public: false,
            return_annotation: None,
            return_type: (),
            end_position: 40,
        })],
    )
}

#[test]
fn parse_tuple() {
    let code = indoc! {r#"
            fn foo() {
              let tuple = (1, 2, 3, 4)
              tuple.1st + tuple.2nd + tuple.3rd + tuple.4th
            }
        "#};

    assert_definitions(
        code,
        vec![ast::UntypedDefinition::Fn(Function {
            can_error: true,
            arguments: vec![],
            body: expr::UntypedExpr::Sequence {
                location: Span::new((), 13..85),
                expressions: vec![
                    expr::UntypedExpr::Assignment {
                        location: Span::new((), 13..37),
                        value: Box::new(expr::UntypedExpr::Tuple {
                            location: Span::new((), 25..37),
                            elems: vec![
                                expr::UntypedExpr::Int {
                                    location: Span::new((), 26..27),
                                    value: "1".to_string(),
                                    base: Base::Decimal {
                                        numeric_underscore: false,
                                    },
                                },
                                expr::UntypedExpr::Int {
                                    location: Span::new((), 29..30),
                                    value: "2".to_string(),
                                    base: Base::Decimal {
                                        numeric_underscore: false,
                                    },
                                },
                                expr::UntypedExpr::Int {
                                    location: Span::new((), 32..33),
                                    value: "3".to_string(),
                                    base: Base::Decimal {
                                        numeric_underscore: false,
                                    },
                                },
                                expr::UntypedExpr::Int {
                                    location: Span::new((), 35..36),
                                    value: "4".to_string(),
                                    base: Base::Decimal {
                                        numeric_underscore: false,
                                    },
                                },
                            ],
                        }),
                        pattern: ast::Pattern::Var {
                            location: Span::new((), 17..22),
                            name: "tuple".to_string(),
                        },
                        kind: ast::AssignmentKind::Let,
                        annotation: None,
                    },
                    expr::UntypedExpr::BinOp {
                        location: Span::new((), 40..85),
                        name: ast::BinOp::AddInt,
                        left: Box::new(expr::UntypedExpr::BinOp {
                            location: Span::new((), 40..73),
                            name: ast::BinOp::AddInt,
                            left: Box::new(expr::UntypedExpr::BinOp {
                                location: Span::new((), 40..61),
                                name: ast::BinOp::AddInt,
                                left: Box::new(expr::UntypedExpr::TupleIndex {
                                    location: Span::new((), 40..49),
                                    index: 0,
                                    tuple: Box::new(expr::UntypedExpr::Var {
                                        location: Span::new((), 40..45),
                                        name: "tuple".to_string(),
                                    }),
                                }),
                                right: Box::new(expr::UntypedExpr::TupleIndex {
                                    location: Span::new((), 52..61),
                                    index: 1,
                                    tuple: Box::new(expr::UntypedExpr::Var {
                                        location: Span::new((), 52..57),
                                        name: "tuple".to_string(),
                                    }),
                                }),
                            }),
                            right: Box::new(expr::UntypedExpr::TupleIndex {
                                location: Span::new((), 64..73),
                                index: 2,
                                tuple: Box::new(expr::UntypedExpr::Var {
                                    location: Span::new((), 64..69),
                                    name: "tuple".to_string(),
                                }),
                            }),
                        }),
                        right: Box::new(expr::UntypedExpr::TupleIndex {
                            location: Span::new((), 76..85),
                            index: 3,
                            tuple: Box::new(expr::UntypedExpr::Var {
                                location: Span::new((), 76..81),
                                name: "tuple".to_string(),
                            }),
                        }),
                    },
                ],
            },
            doc: None,
            location: Span::new((), 0..8),
            name: "foo".to_string(),
            public: false,
            return_annotation: None,
            return_type: (),
            end_position: 86,
        })],
    )
}

#[test]
fn parse_tuple2() {
    let code = indoc! {r#"
            fn foo() {
              let a = foo(14)
              (a, 42)
            }
        "#};

    assert_definitions(
        code,
        vec![ast::UntypedDefinition::Fn(Function {
            can_error: true,
            arguments: vec![],
            body: expr::UntypedExpr::Sequence {
                location: Span::new((), 13..38),
                expressions: vec![
                    expr::UntypedExpr::Assignment {
                        location: Span::new((), 13..28),
                        value: Box::new(expr::UntypedExpr::Call {
                            arguments: vec![ast::CallArg {
                                label: None,
                                location: Span::new((), 25..27),
                                value: expr::UntypedExpr::Int {
                                    location: Span::new((), 25..27),
                                    value: "14".to_string(),
                                    base: Base::Decimal {
                                        numeric_underscore: false,
                                    },
                                },
                            }],
                            fun: Box::new(expr::UntypedExpr::Var {
                                location: Span::new((), 21..24),
                                name: "foo".to_string(),
                            }),
                            location: Span::new((), 21..28),
                        }),
                        pattern: ast::Pattern::Var {
                            location: Span::new((), 17..18),
                            name: "a".to_string(),
                        },
                        kind: ast::AssignmentKind::Let,
                        annotation: None,
                    },
                    expr::UntypedExpr::Tuple {
                        location: Span::new((), 31..38),
                        elems: vec![
                            expr::UntypedExpr::Var {
                                location: Span::new((), 32..33),
                                name: "a".to_string(),
                            },
                            expr::UntypedExpr::Int {
                                location: Span::new((), 35..37),
                                value: "42".to_string(),
                                base: Base::Decimal {
                                    numeric_underscore: false,
                                },
                            },
                        ],
                    },
                ],
            },
            doc: None,
            location: Span::new((), 0..8),
            name: "foo".to_string(),
            public: false,
            return_annotation: None,
            return_type: (),
            end_position: 39,
        })],
    );
}

#[test]
fn large_integer_constants() {
    let code = indoc! {r#"
      pub const my_big_int = 999999999999999999999999
    "#};

    assert_definitions(
        code,
        vec![ast::UntypedDefinition::ModuleConstant(ModuleConstant {
            doc: None,
            location: Span::new((), 0..47),
            public: true,
            name: "my_big_int".to_string(),
            annotation: None,
            value: Box::new(ast::Constant::Int {
                location: Span::new((), 23..47),
                value: "999999999999999999999999".to_string(),
                base: Base::Decimal {
                    numeric_underscore: false,
                },
            }),
            tipo: (),
        })],
    )
}

#[test]
fn plain_bytearray_literals() {
    let code = indoc! {r#"
        pub const my_policy_id = #[0, 170, 255]
    "#};

    assert_definitions(
        code,
        vec![ast::UntypedDefinition::ModuleConstant(ModuleConstant {
            doc: None,
            location: Span::new((), 0..39),
            public: true,
            name: "my_policy_id".to_string(),
            annotation: None,
            value: Box::new(Constant::ByteArray {
                location: Span::new((), 25..39),
                bytes: vec![0, 170, 255],
                preferred_format: ast::ByteArrayFormatPreference::ArrayOfBytes(Base::Decimal {
                    numeric_underscore: false,
                }),
            }),
            tipo: (),
        })],
    )
}

#[test]
fn base16_bytearray_literals() {
    let code = indoc! {r#"
        pub const my_policy_id = #"00aaff"

        pub fn foo() {
            my_policy_id == #"00aaff"
        }
    "#};

    assert_definitions(
        code,
        vec![
            ast::UntypedDefinition::ModuleConstant(ModuleConstant {
                doc: None,
                location: Span::new((), 0..34),
                public: true,
                name: "my_policy_id".to_string(),
                annotation: None,
                value: Box::new(Constant::ByteArray {
                    location: Span::new((), 25..34),
                    bytes: vec![0, 170, 255],
                    preferred_format: ast::ByteArrayFormatPreference::HexadecimalString,
                }),
                tipo: (),
            }),
            ast::UntypedDefinition::Fn(Function {
                can_error: true,
                arguments: vec![],
                body: expr::UntypedExpr::BinOp {
                    location: Span::new((), 55..80),
                    name: ast::BinOp::Eq,
                    left: Box::new(expr::UntypedExpr::Var {
                        location: Span::new((), 55..67),
                        name: "my_policy_id".to_string(),
                    }),
                    right: Box::new(expr::UntypedExpr::ByteArray {
                        location: Span::new((), 71..80),
                        bytes: vec![0, 170, 255],
                        preferred_format: ast::ByteArrayFormatPreference::HexadecimalString,
                    }),
                },
                doc: None,
                location: Span::new((), 36..48),
                name: "foo".to_string(),
                public: true,
                return_annotation: None,
                return_type: (),
                end_position: 81,
            }),
        ],
    )
}

#[test]
fn function_def() {
    let code = indoc! {r#"
          fn foo() {}
    "#};

    assert_definitions(
        code,
        vec![ast::UntypedDefinition::Fn(Function {
            can_error: true,
            doc: None,
            arguments: vec![],
            body: expr::UntypedExpr::Trace {
                kind: ast::TraceKind::Todo,
                location: Span::new((), 0..11),
                text: Box::new(expr::UntypedExpr::String {
                    value: "aiken::todo".to_string(),
                    location: Span::new((), 0..11),
                }),
                then: Box::new(expr::UntypedExpr::ErrorTerm {
                    location: Span::new((), 0..11),
                }),
            },
            location: Span::new((), 0..8),
            name: "foo".to_string(),
            public: false,
            return_annotation: None,
            return_type: (),
            end_position: 10,
        })],
    )
}

#[test]
fn function_invoke() {
    let code = indoc! {r#"
          fn foo() {
            let a = bar(42)
          }
    "#};

    assert_definitions(
        code,
        vec![ast::UntypedDefinition::Fn(Function {
            can_error: true,
            doc: None,
            arguments: vec![],
            body: expr::UntypedExpr::Assignment {
                location: Span::new((), 13..28),
                kind: ast::AssignmentKind::Let,
                annotation: None,
                pattern: ast::Pattern::Var {
                    location: Span::new((), 17..18),
                    name: "a".to_string(),
                },
                value: Box::new(expr::UntypedExpr::Call {
                    location: Span::new((), 21..28),
                    fun: Box::new(expr::UntypedExpr::Var {
                        location: Span::new((), 21..24),
                        name: "bar".to_string(),
                    }),
                    arguments: vec![ast::CallArg {
                        label: None,
                        location: Span::new((), 25..27),
                        value: expr::UntypedExpr::Int {
                            location: Span::new((), 25..27),
                            value: "42".to_string(),
                            base: Base::Decimal {
                                numeric_underscore: false,
                            },
                        },
                    }],
                }),
            },
            location: Span::new((), 0..8),
            name: "foo".to_string(),
            public: false,
            return_annotation: None,
            return_type: (),
            end_position: 29,
        })],
    )
}

#[test]
fn function_ambiguous_sequence() {
    let code = indoc! {r#"
          fn foo_1() {
            let a = bar
            (40)
          }

          fn foo_2() {
            let a = bar
            {40}
          }

          fn foo_3() {
            let a = (40+2)
          }

          fn foo_4() {
            let a = bar(42)
            (a + 14) * 42
          }
    "#};

    assert_definitions(
        code,
        vec![
            ast::UntypedDefinition::Fn(Function {
                can_error: true,
                arguments: vec![],
                body: expr::UntypedExpr::Sequence {
                    location: Span::new((), 15..32),
                    expressions: vec![
                        expr::UntypedExpr::Assignment {
                            location: Span::new((), 15..26),
                            value: Box::new(expr::UntypedExpr::Var {
                                location: Span::new((), 23..26),
                                name: "bar".to_string(),
                            }),
                            pattern: ast::Pattern::Var {
                                location: Span::new((), 19..20),
                                name: "a".to_string(),
                            },
                            kind: ast::AssignmentKind::Let,
                            annotation: None,
                        },
                        expr::UntypedExpr::Int {
                            location: Span::new((), 30..32),
                            value: "40".to_string(),
                            base: Base::Decimal {
                                numeric_underscore: false,
                            },
                        },
                    ],
                },
                doc: None,
                location: Span::new((), 0..10),
                name: "foo_1".to_string(),
                public: false,
                return_annotation: None,
                return_type: (),
                end_position: 34,
            }),
            ast::UntypedDefinition::Fn(Function {
                can_error: true,
                arguments: vec![],
                body: expr::UntypedExpr::Sequence {
                    location: Span::new((), 52..69),
                    expressions: vec![
                        expr::UntypedExpr::Assignment {
                            location: Span::new((), 52..63),
                            value: Box::new(expr::UntypedExpr::Var {
                                location: Span::new((), 60..63),
                                name: "bar".to_string(),
                            }),
                            pattern: ast::Pattern::Var {
                                location: Span::new((), 56..57),
                                name: "a".to_string(),
                            },
                            kind: ast::AssignmentKind::Let,
                            annotation: None,
                        },
                        expr::UntypedExpr::Int {
                            location: Span::new((), 67..69),
                            value: "40".to_string(),
                            base: Base::Decimal {
                                numeric_underscore: false,
                            },
                        },
                    ],
                },
                doc: None,
                location: Span::new((), 37..47),
                name: "foo_2".to_string(),
                public: false,
                return_annotation: None,
                return_type: (),
                end_position: 71,
            }),
            ast::UntypedDefinition::Fn(Function {
                can_error: true,
                arguments: vec![],
                body: expr::UntypedExpr::Assignment {
                    location: Span::new((), 89..103),
                    value: Box::new(expr::UntypedExpr::BinOp {
                        location: Span::new((), 98..102),
                        name: ast::BinOp::AddInt,
                        left: Box::new(expr::UntypedExpr::Int {
                            location: Span::new((), 98..100),
                            value: "40".to_string(),
                            base: Base::Decimal {
                                numeric_underscore: false,
                            },
                        }),
                        right: Box::new(expr::UntypedExpr::Int {
                            location: Span::new((), 101..102),
                            value: "2".to_string(),
                            base: Base::Decimal {
                                numeric_underscore: false,
                            },
                        }),
                    }),
                    pattern: ast::Pattern::Var {
                        location: Span::new((), 93..94),
                        name: "a".to_string(),
                    },
                    kind: ast::AssignmentKind::Let,
                    annotation: None,
                },
                doc: None,
                location: Span::new((), 74..84),
                name: "foo_3".to_string(),
                public: false,
                return_annotation: None,
                return_type: (),
                end_position: 104,
            }),
            ast::UntypedDefinition::Fn(Function {
                can_error: true,
                arguments: vec![],
                body: expr::UntypedExpr::Sequence {
                    location: Span::new((), 122..153),
                    expressions: vec![
                        expr::UntypedExpr::Assignment {
                            location: Span::new((), 122..137),
                            value: Box::new(expr::UntypedExpr::Call {
                                arguments: vec![ast::CallArg {
                                    label: None,
                                    location: Span::new((), 134..136),
                                    value: expr::UntypedExpr::Int {
                                        location: Span::new((), 134..136),
                                        value: "42".to_string(),
                                        base: Base::Decimal {
                                            numeric_underscore: false,
                                        },
                                    },
                                }],
                                fun: Box::new(expr::UntypedExpr::Var {
                                    location: Span::new((), 130..133),
                                    name: "bar".to_string(),
                                }),
                                location: Span::new((), 130..137),
                            }),
                            pattern: ast::Pattern::Var {
                                location: Span::new((), 126..127),
                                name: "a".to_string(),
                            },
                            kind: ast::AssignmentKind::Let,
                            annotation: None,
                        },
                        expr::UntypedExpr::BinOp {
                            location: Span::new((), 141..153),
                            name: ast::BinOp::MultInt,
                            left: Box::new(expr::UntypedExpr::BinOp {
                                location: Span::new((), 141..147),
                                name: ast::BinOp::AddInt,
                                left: Box::new(expr::UntypedExpr::Var {
                                    location: Span::new((), 141..142),
                                    name: "a".to_string(),
                                }),
                                right: Box::new(expr::UntypedExpr::Int {
                                    location: Span::new((), 145..147),
                                    value: "14".to_string(),
                                    base: Base::Decimal {
                                        numeric_underscore: false,
                                    },
                                }),
                            }),
                            right: Box::new(expr::UntypedExpr::Int {
                                location: Span::new((), 151..153),
                                value: "42".to_string(),
                                base: Base::Decimal {
                                    numeric_underscore: false,
                                },
                            }),
                        },
                    ],
                },
                doc: None,
                location: Span::new((), 107..117),
                name: "foo_4".to_string(),
                public: false,
                return_annotation: None,
                return_type: (),
                end_position: 154,
            }),
        ],
    )
}

#[test]
fn tuple_type_alias() {
    let code = indoc! {r#"
          type RoyaltyToken =
            (PolicyId, AssetName)
        "#};

    assert_definitions(
        code,
        vec![ast::UntypedDefinition::TypeAlias(TypeAlias {
            alias: "RoyaltyToken".to_string(),
            annotation: ast::Annotation::Tuple {
                location: Span::new((), 22..43),
                elems: vec![
                    ast::Annotation::Constructor {
                        location: Span::new((), 23..31),
                        module: None,
                        name: "PolicyId".to_string(),
                        arguments: vec![],
                    },
                    ast::Annotation::Constructor {
                        location: Span::new((), 33..42),
                        module: None,
                        name: "AssetName".to_string(),
                        arguments: vec![],
                    },
                ],
            },
            doc: None,
            location: Span::new((), 0..43),
            parameters: vec![],
            public: false,
            tipo: (),
        })],
    )
}

#[test]
fn tuple_pattern() {
    let code = indoc! {r#"
          fn foo() {
            when a is {
              (u, dic) -> True
            }
          }
        "#};

    assert_definitions(
        code,
        vec![ast::UntypedDefinition::Fn(Function {
            can_error: true,
            arguments: vec![],
            body: expr::UntypedExpr::When {
                location: Span::new((), 13..49),
                subject: Box::new(expr::UntypedExpr::Var {
                    location: Span::new((), 18..19),
                    name: "a".to_string(),
                }),
                clauses: vec![ast::UntypedClause {
                    location: Span::new((), 29..45),
                    patterns: vec1![ast::Pattern::Tuple {
                        location: Span::new((), 29..37),
                        elems: vec![
                            ast::Pattern::Var {
                                location: Span::new((), 30..31),
                                name: "u".to_string(),
                            },
                            ast::Pattern::Var {
                                location: Span::new((), 33..36),
                                name: "dic".to_string(),
                            },
                        ],
                    }],
                    guard: None,
                    then: expr::UntypedExpr::Var {
                        location: Span::new((), 41..45),
                        name: "True".to_string(),
                    },
                }],
            },
            doc: None,
            location: Span::new((), 0..8),
            name: "foo".to_string(),
            public: false,
            return_annotation: None,
            return_type: (),
            end_position: 50,
        })],
    );
}

#[test]
fn subtraction_vs_negate() {
    let code = indoc! {r#"
          fn foo() {
            (1-1) == 0
            let x = -2
            bar()-4
            bar(-1) - 42
          }
        "#};
    assert_definitions(
        code,
        vec![ast::Definition::Fn(Function {
            can_error: true,
            arguments: vec![],
            body: expr::UntypedExpr::Sequence {
                location: Span::new((), 14..61),
                expressions: vec![
                    expr::UntypedExpr::BinOp {
                        location: Span::new((), 14..23),
                        name: ast::BinOp::Eq,
                        left: Box::new(expr::UntypedExpr::BinOp {
                            location: Span::new((), 14..17),
                            name: ast::BinOp::SubInt,
                            left: Box::new(expr::UntypedExpr::Int {
                                location: Span::new((), 14..15),
                                value: "1".to_string(),
                                base: Base::Decimal {
                                    numeric_underscore: false,
                                },
                            }),
                            right: Box::new(expr::UntypedExpr::Int {
                                location: Span::new((), 16..17),
                                value: "1".to_string(),
                                base: Base::Decimal {
                                    numeric_underscore: false,
                                },
                            }),
                        }),
                        right: Box::new(expr::UntypedExpr::Int {
                            location: Span::new((), 22..23),
                            value: "0".to_string(),
                            base: Base::Decimal {
                                numeric_underscore: false,
                            },
                        }),
                    },
                    expr::UntypedExpr::Assignment {
                        location: Span::new((), 26..36),
                        value: Box::new(expr::UntypedExpr::UnOp {
                            op: ast::UnOp::Negate,
                            location: Span::new((), 34..36),
                            value: Box::new(expr::UntypedExpr::Int {
                                location: Span::new((), 35..36),
                                value: "2".to_string(),
                                base: Base::Decimal {
                                    numeric_underscore: false,
                                },
                            }),
                        }),
                        pattern: ast::Pattern::Var {
                            location: Span::new((), 30..31),
                            name: "x".to_string(),
                        },
                        kind: ast::AssignmentKind::Let,
                        annotation: None,
                    },
                    expr::UntypedExpr::BinOp {
                        location: Span::new((), 39..46),
                        name: ast::BinOp::SubInt,
                        left: Box::new(expr::UntypedExpr::Call {
                            arguments: vec![],
                            fun: Box::new(expr::UntypedExpr::Var {
                                location: Span::new((), 39..42),
                                name: "bar".to_string(),
                            }),
                            location: Span::new((), 39..44),
                        }),
                        right: Box::new(expr::UntypedExpr::Int {
                            location: Span::new((), 45..46),
                            value: "4".to_string(),
                            base: Base::Decimal {
                                numeric_underscore: false,
                            },
                        }),
                    },
                    expr::UntypedExpr::BinOp {
                        location: Span::new((), 49..61),
                        name: ast::BinOp::SubInt,
                        left: Box::new(expr::UntypedExpr::Call {
                            arguments: vec![ast::CallArg {
                                label: None,
                                location: Span::new((), 53..55),
                                value: expr::UntypedExpr::UnOp {
                                    op: ast::UnOp::Negate,
                                    location: Span::new((), 53..55),
                                    value: Box::new(expr::UntypedExpr::Int {
                                        location: Span::new((), 54..55),
                                        value: "1".to_string(),
                                        base: Base::Decimal {
                                            numeric_underscore: false,
                                        },
                                    }),
                                },
                            }],
                            fun: Box::new(expr::UntypedExpr::Var {
                                location: Span::new((), 49..52),
                                name: "bar".to_string(),
                            }),
                            location: Span::new((), 49..56),
                        }),
                        right: Box::new(expr::UntypedExpr::Int {
                            location: Span::new((), 59..61),
                            value: "42".to_string(),
                            base: Base::Decimal {
                                numeric_underscore: false,
                            },
                        }),
                    },
                ],
            },
            doc: None,
            location: Span::new((), 0..8),
            name: "foo".to_string(),
            public: false,
            return_annotation: None,
            return_type: (),
            end_position: 62,
        })],
    );
}

#[test]
fn clause_guards() {
    let code = indoc! {r#"
          fn foo() {
            when a is {
              _ if 42 -> Void
              _ if bar -> Void
              _ if True -> Void
              _ if a || b && c -> Void
              _ if (a || b) && c -> Void
              _ if a <= 42 || b > 14 || "str" -> Void
              _ if a == 14 && !b -> Void
              _ if !!True -> Void
            }
          }
        "#};
    assert_definitions(
        code,
        vec![ast::Definition::Fn(Function {
            can_error: true,
            arguments: vec![],
            body: expr::UntypedExpr::When {
                location: Span::new((), 13..250),
                subject: Box::new(expr::UntypedExpr::Var {
                    location: Span::new((), 18..19),
                    name: "a".to_string(),
                }),
                clauses: vec![
                    ast::UntypedClause {
                        location: Span::new((), 29..44),
                        patterns: vec1![ast::Pattern::Discard {
                            name: "_".to_string(),
                            location: Span::new((), 29..30),
                        }],
                        guard: Some(ast::UntypedClauseGuard::Constant(ast::Constant::Int {
                            location: Span::new((), 34..36),
                            value: "42".to_string(),
                            base: Base::Decimal {
                                numeric_underscore: false,
                            },
                        })),
                        then: expr::UntypedExpr::Var {
                            location: Span::new((), 40..44),
                            name: "Void".to_string(),
                        },
                    },
                    ast::UntypedClause {
                        location: Span::new((), 49..65),
                        patterns: vec1![ast::Pattern::Discard {
                            name: "_".to_string(),
                            location: Span::new((), 49..50),
                        }],
                        guard: Some(ast::UntypedClauseGuard::Var {
                            location: Span::new((), 54..57),
                            name: "bar".to_string(),
                            tipo: (),
                        }),
                        then: expr::UntypedExpr::Var {
                            location: Span::new((), 61..65),
                            name: "Void".to_string(),
                        },
                    },
                    ast::UntypedClause {
                        location: Span::new((), 70..87),
                        patterns: vec1![ast::Pattern::Discard {
                            name: "_".to_string(),
                            location: Span::new((), 70..71),
                        }],
                        guard: Some(ast::UntypedClauseGuard::Var {
                            location: Span::new((), 75..79),
                            tipo: (),
                            name: "True".to_string(),
                        }),
                        then: expr::UntypedExpr::Var {
                            location: Span::new((), 83..87),
                            name: "Void".to_string(),
                        },
                    },
                    ast::UntypedClause {
                        location: Span::new((), 92..116),
                        patterns: vec1![ast::Pattern::Discard {
                            name: "_".to_string(),
                            location: Span::new((), 92..93),
                        }],
                        guard: Some(ast::UntypedClauseGuard::Or {
                            location: Span::new((), 97..108),
                            left: Box::new(ast::UntypedClauseGuard::Var {
                                location: Span::new((), 97..98),
                                name: "a".to_string(),
                                tipo: (),
                            }),
                            right: Box::new(ast::UntypedClauseGuard::And {
                                location: Span::new((), 102..108),
                                left: Box::new(ast::UntypedClauseGuard::Var {
                                    location: Span::new((), 102..103),
                                    name: "b".to_string(),
                                    tipo: (),
                                }),
                                right: Box::new(ast::UntypedClauseGuard::Var {
                                    location: Span::new((), 107..108),
                                    name: "c".to_string(),
                                    tipo: (),
                                }),
                            }),
                        }),
                        then: expr::UntypedExpr::Var {
                            location: Span::new((), 112..116),
                            name: "Void".to_string(),
                        },
                    },
                    ast::UntypedClause {
                        location: Span::new((), 121..147),
                        patterns: vec1![ast::Pattern::Discard {
                            name: "_".to_string(),
                            location: Span::new((), 121..122),
                        }],
                        guard: Some(ast::UntypedClauseGuard::And {
                            location: Span::new((), 127..139),
                            left: Box::new(ast::UntypedClauseGuard::Or {
                                location: Span::new((), 127..133),
                                left: Box::new(ast::UntypedClauseGuard::Var {
                                    location: Span::new((), 127..128),
                                    name: "a".to_string(),
                                    tipo: (),
                                }),
                                right: Box::new(ast::UntypedClauseGuard::Var {
                                    location: Span::new((), 132..133),
                                    name: "b".to_string(),
                                    tipo: (),
                                }),
                            }),
                            right: Box::new(ast::UntypedClauseGuard::Var {
                                location: Span::new((), 138..139),
                                name: "c".to_string(),
                                tipo: (),
                            }),
                        }),
                        then: expr::UntypedExpr::Var {
                            location: Span::new((), 143..147),
                            name: "Void".to_string(),
                        },
                    },
                    ast::UntypedClause {
                        location: Span::new((), 152..191),
                        patterns: vec1![ast::Pattern::Discard {
                            name: "_".to_string(),
                            location: Span::new((), 152..153),
                        }],
                        guard: Some(ast::UntypedClauseGuard::Or {
                            location: Span::new((), 157..183),
                            left: Box::new(ast::UntypedClauseGuard::Or {
                                location: Span::new((), 157..174),
                                left: Box::new(ast::UntypedClauseGuard::LtEqInt {
                                    location: Span::new((), 157..164),
                                    left: Box::new(ast::UntypedClauseGuard::Var {
                                        location: Span::new((), 157..158),
                                        name: "a".to_string(),
                                        tipo: (),
                                    }),
                                    right: Box::new(ast::UntypedClauseGuard::Constant(
                                        ast::Constant::Int {
                                            location: Span::new((), 162..164),
                                            value: "42".to_string(),
                                            base: Base::Decimal {
                                                numeric_underscore: false,
                                            },
                                        },
                                    )),
                                }),
                                right: Box::new(ast::UntypedClauseGuard::GtInt {
                                    location: Span::new((), 168..174),
                                    left: Box::new(ast::UntypedClauseGuard::Var {
                                        location: Span::new((), 168..169),
                                        name: "b".to_string(),
                                        tipo: (),
                                    }),
                                    right: Box::new(ast::UntypedClauseGuard::Constant(
                                        ast::Constant::Int {
                                            location: Span::new((), 172..174),
                                            value: "14".to_string(),
                                            base: Base::Decimal {
                                                numeric_underscore: false,
                                            },
                                        },
                                    )),
                                }),
                            }),
                            right: Box::new(ast::UntypedClauseGuard::Constant(
                                ast::Constant::ByteArray {
                                    location: Span::new((), 178..183),
                                    bytes: String::from("str").into_bytes(),
                                    preferred_format: ast::ByteArrayFormatPreference::Utf8String,
                                },
                            )),
                        }),
                        then: expr::UntypedExpr::Var {
                            location: Span::new((), 187..191),
                            name: "Void".to_string(),
                        },
                    },
                    ast::UntypedClause {
                        location: Span::new((), 196..222),
                        patterns: vec1![ast::Pattern::Discard {
                            name: "_".to_string(),
                            location: Span::new((), 196..197),
                        }],
                        guard: Some(ast::UntypedClauseGuard::And {
                            location: Span::new((), 201..214),
                            left: Box::new(ast::UntypedClauseGuard::Equals {
                                location: Span::new((), 201..208),
                                left: Box::new(ast::UntypedClauseGuard::Var {
                                    location: Span::new((), 201..202),
                                    name: "a".to_string(),
                                    tipo: (),
                                }),
                                right: Box::new(ast::UntypedClauseGuard::Constant(
                                    ast::Constant::Int {
                                        location: Span::new((), 206..208),
                                        value: "14".to_string(),
                                        base: Base::Decimal {
                                            numeric_underscore: false,
                                        },
                                    },
                                )),
                            }),
                            right: Box::new(ast::UntypedClauseGuard::Not {
                                location: Span::new((), 212..214),
                                value: Box::new(ast::UntypedClauseGuard::Var {
                                    location: Span::new((), 213..214),
                                    name: "b".to_string(),
                                    tipo: (),
                                }),
                            }),
                        }),
                        then: expr::UntypedExpr::Var {
                            location: Span::new((), 218..222),
                            name: "Void".to_string(),
                        },
                    },
                    ast::UntypedClause {
                        location: Span::new((), 227..246),
                        patterns: vec1![ast::Pattern::Discard {
                            name: "_".to_string(),
                            location: Span::new((), 227..228),
                        }],
                        guard: Some(ast::UntypedClauseGuard::Not {
                            location: Span::new((), 232..238),
                            value: Box::new(ast::UntypedClauseGuard::Not {
                                location: Span::new((), 233..238),
                                value: Box::new(ast::UntypedClauseGuard::Var {
                                    location: Span::new((), 234..238),
                                    tipo: (),
                                    name: "True".to_string(),
                                }),
                            }),
                        }),
                        then: expr::UntypedExpr::Var {
                            location: Span::new((), 242..246),
                            name: "Void".to_string(),
                        },
                    },
                ],
            },
            doc: None,
            location: Span::new((), 0..8),
            name: "foo".to_string(),
            public: false,
            return_annotation: None,
            return_type: (),
            end_position: 251,
        })],
    );
}

#[test]
fn scope_logical_expression() {
    let code = indoc! {r#"
          fn foo() {
            let x = !(a && b)
            let y = a || b && c || d
            x
          }
        "#};
    assert_definitions(
        code,
        vec![ast::Definition::Fn(Function {
            can_error: true,
            arguments: vec![],
            body: expr::UntypedExpr::Sequence {
                location: Span::new((), 13..61),
                expressions: vec![
                    expr::UntypedExpr::Assignment {
                        location: Span::new((), 13..30),
                        value: Box::new(expr::UntypedExpr::UnOp {
                            op: ast::UnOp::Not,
                            location: Span::new((), 21..29),
                            value: Box::new(expr::UntypedExpr::BinOp {
                                location: Span::new((), 23..29),
                                name: ast::BinOp::And,
                                left: Box::new(expr::UntypedExpr::Var {
                                    location: Span::new((), 23..24),
                                    name: "a".to_string(),
                                }),
                                right: Box::new(expr::UntypedExpr::Var {
                                    location: Span::new((), 28..29),
                                    name: "b".to_string(),
                                }),
                            }),
                        }),
                        pattern: ast::Pattern::Var {
                            location: Span::new((), 17..18),
                            name: "x".to_string(),
                        },
                        kind: ast::AssignmentKind::Let,
                        annotation: None,
                    },
                    expr::UntypedExpr::Assignment {
                        location: Span::new((), 33..57),
                        value: Box::new(expr::UntypedExpr::BinOp {
                            location: Span::new((), 41..57),
                            name: ast::BinOp::Or,
                            left: Box::new(expr::UntypedExpr::BinOp {
                                location: Span::new((), 41..52),
                                name: ast::BinOp::Or,
                                left: Box::new(expr::UntypedExpr::Var {
                                    location: Span::new((), 41..42),
                                    name: "a".to_string(),
                                }),
                                right: Box::new(expr::UntypedExpr::BinOp {
                                    location: Span::new((), 46..52),
                                    name: ast::BinOp::And,
                                    left: Box::new(expr::UntypedExpr::Var {
                                        location: Span::new((), 46..47),
                                        name: "b".to_string(),
                                    }),
                                    right: Box::new(expr::UntypedExpr::Var {
                                        location: Span::new((), 51..52),
                                        name: "c".to_string(),
                                    }),
                                }),
                            }),
                            right: Box::new(expr::UntypedExpr::Var {
                                location: Span::new((), 56..57),
                                name: "d".to_string(),
                            }),
                        }),
                        pattern: ast::Pattern::Var {
                            location: Span::new((), 37..38),
                            name: "y".to_string(),
                        },
                        kind: ast::AssignmentKind::Let,
                        annotation: None,
                    },
                    expr::UntypedExpr::Var {
                        location: Span::new((), 60..61),
                        name: "x".to_string(),
                    },
                ],
            },
            doc: None,
            location: Span::new((), 0..8),
            name: "foo".to_string(),
            public: false,
            return_annotation: None,
            return_type: (),
            end_position: 62,
        })],
    )
}

#[test]
fn trace_expressions() {
    let code = indoc! {r#"
          fn foo() {
            let msg1 = @"FOO"
            trace @"INLINE"
            trace msg1
            trace string.concat(msg1, @"BAR")
            trace ( 14 + 42 * 1337 )
            Void
          }
        "#};
    assert_definitions(
        code,
        vec![ast::Definition::Fn(Function {
            can_error: true,
            arguments: vec![],
            body: expr::UntypedExpr::Sequence {
                location: Span::new((), 13..131),
                expressions: vec![
                    expr::UntypedExpr::Assignment {
                        location: Span::new((), 13..30),
                        value: Box::new(expr::UntypedExpr::String {
                            location: Span::new((), 24..30),
                            value: "FOO".to_string(),
                        }),
                        pattern: ast::Pattern::Var {
                            location: Span::new((), 17..21),
                            name: "msg1".to_string(),
                        },
                        kind: ast::AssignmentKind::Let,
                        annotation: None,
                    },
                    expr::UntypedExpr::Trace {
                        kind: ast::TraceKind::Trace,
                        location: Span::new((), 33..131),
                        then: Box::new(expr::UntypedExpr::Trace {
                            kind: ast::TraceKind::Trace,
                            location: Span::new((), 51..131),
                            then: Box::new(expr::UntypedExpr::Trace {
                                kind: ast::TraceKind::Trace,
                                location: Span::new((), 64..131),
                                then: Box::new(expr::UntypedExpr::Trace {
                                    kind: ast::TraceKind::Trace,
                                    location: Span::new((), 100..131),
                                    then: Box::new(expr::UntypedExpr::Var {
                                        location: Span::new((), 127..131),
                                        name: "Void".to_string(),
                                    }),
                                    text: Box::new(expr::UntypedExpr::BinOp {
                                        location: Span::new((), 108..122),
                                        name: ast::BinOp::AddInt,
                                        left: Box::new(expr::UntypedExpr::Int {
                                            location: Span::new((), 108..110),
                                            value: "14".to_string(),
                                            base: Base::Decimal {
                                                numeric_underscore: false,
                                            },
                                        }),
                                        right: Box::new(expr::UntypedExpr::BinOp {
                                            location: Span::new((), 113..122),
                                            name: ast::BinOp::MultInt,
                                            left: Box::new(expr::UntypedExpr::Int {
                                                location: Span::new((), 113..115),
                                                value: "42".to_string(),
                                                base: Base::Decimal {
                                                    numeric_underscore: false,
                                                },
                                            }),
                                            right: Box::new(expr::UntypedExpr::Int {
                                                location: Span::new((), 118..122),
                                                value: "1337".to_string(),
                                                base: Base::Decimal {
                                                    numeric_underscore: false,
                                                },
                                            }),
                                        }),
                                    }),
                                }),
                                text: Box::new(expr::UntypedExpr::Call {
                                    arguments: vec![
                                        ast::CallArg {
                                            label: None,
                                            location: Span::new((), 84..88),
                                            value: expr::UntypedExpr::Var {
                                                location: Span::new((), 84..88),
                                                name: "msg1".to_string(),
                                            },
                                        },
                                        ast::CallArg {
                                            label: None,
                                            location: Span::new((), 90..96),
                                            value: expr::UntypedExpr::String {
                                                location: Span::new((), 90..96),
                                                value: "BAR".to_string(),
                                            },
                                        },
                                    ],
                                    fun: Box::new(expr::UntypedExpr::FieldAccess {
                                        location: Span::new((), 70..83),
                                        label: "concat".to_string(),
                                        container: Box::new(expr::UntypedExpr::Var {
                                            location: Span::new((), 70..76),
                                            name: "string".to_string(),
                                        }),
                                    }),
                                    location: Span::new((), 70..97),
                                }),
                            }),
                            text: Box::new(expr::UntypedExpr::Var {
                                location: Span::new((), 57..61),
                                name: "msg1".to_string(),
                            }),
                        }),
                        text: Box::new(expr::UntypedExpr::String {
                            location: Span::new((), 39..48),
                            value: "INLINE".to_string(),
                        }),
                    },
                ],
            },
            doc: None,
            location: Span::new((), 0..8),
            name: "foo".to_string(),
            public: false,
            return_annotation: None,
            return_type: (),
            end_position: 132,
        })],
    )
}

#[test]
fn parse_keyword_error() {
    let code = indoc! {r#"
          fn foo() {
            error @"not implemented"
          }

          fn bar() {
            when x is {
                Something -> Void
                _ -> error
            }
          }
        "#};
    assert_definitions(
        code,
        vec![
            ast::Definition::Fn(Function {
                can_error: true,
                arguments: vec![],
                body: expr::UntypedExpr::Trace {
                    kind: ast::TraceKind::Error,
                    location: Span::new((), 13..37),
                    then: Box::new(expr::UntypedExpr::ErrorTerm {
                        location: Span::new((), 13..37),
                    }),
                    text: Box::new(expr::UntypedExpr::String {
                        location: Span::new((), 19..37),
                        value: "not implemented".to_string(),
                    }),
                },
                doc: None,
                location: Span::new((), 0..8),
                name: "foo".to_string(),
                public: false,
                return_annotation: None,
                return_type: (),
                end_position: 38,
            }),
            ast::Definition::Fn(Function {
                can_error: true,
                arguments: vec![],
                body: expr::UntypedExpr::When {
                    location: Span::new((), 54..110),
                    subject: Box::new(expr::UntypedExpr::Var {
                        location: Span::new((), 59..60),
                        name: "x".to_string(),
                    }),
                    clauses: vec![
                        ast::UntypedClause {
                            location: Span::new((), 72..89),
                            patterns: vec1![ast::Pattern::Constructor {
                                is_record: false,
                                location: Span::new((), 72..81),
                                name: "Something".to_string(),
                                arguments: vec![],
                                module: None,
                                constructor: (),
                                with_spread: false,
                                tipo: (),
                            }],
                            guard: None,
                            then: expr::UntypedExpr::Var {
                                location: Span::new((), 85..89),
                                name: "Void".to_string(),
                            },
                        },
                        ast::UntypedClause {
                            location: Span::new((), 96..106),
                            patterns: vec1![ast::Pattern::Discard {
                                name: "_".to_string(),
                                location: Span::new((), 96..97),
                            }],
                            guard: None,
                            then: expr::UntypedExpr::Trace {
                                kind: ast::TraceKind::Error,
                                location: Span::new((), 101..106),
                                then: Box::new(expr::UntypedExpr::ErrorTerm {
                                    location: Span::new((), 101..106),
                                }),
                                text: Box::new(expr::UntypedExpr::String {
                                    location: Span::new((), 101..106),
                                    value: "aiken::error".to_string(),
                                }),
                            },
                        },
                    ],
                },
                doc: None,
                location: Span::new((), 41..49),
                name: "bar".to_string(),
                public: false,
                return_annotation: None,
                return_type: (),
                end_position: 111,
            }),
        ],
    )
}

#[test]
fn parse_keyword_todo() {
    let code = indoc! {r#"
          fn foo() {
            todo @"not implemented"
          }

          fn bar() {
            when x is {
                Foo -> todo
                Bar -> True
                _ -> False
            }
          }
        "#};
    assert_definitions(
        code,
        vec![
            ast::Definition::Fn(Function {
                can_error: true,
                arguments: vec![],
                body: expr::UntypedExpr::Trace {
                    kind: ast::TraceKind::Todo,
                    location: Span::new((), 13..36),
                    then: Box::new(expr::UntypedExpr::ErrorTerm {
                        location: Span::new((), 13..36),
                    }),
                    text: Box::new(expr::UntypedExpr::String {
                        location: Span::new((), 18..36),
                        value: "not implemented".to_string(),
                    }),
                },
                doc: None,
                location: Span::new((), 0..8),
                name: "foo".to_string(),
                public: false,
                return_annotation: None,
                return_type: (),
                end_position: 37,
            }),
            ast::Definition::Fn(Function {
                can_error: true,
                arguments: vec![],
                body: expr::UntypedExpr::When {
                    location: Span::new((), 53..121),
                    subject: Box::new(expr::UntypedExpr::Var {
                        location: Span::new((), 58..59),
                        name: "x".to_string(),
                    }),
                    clauses: vec![
                        ast::UntypedClause {
                            location: Span::new((), 71..82),
                            patterns: vec1![ast::Pattern::Constructor {
                                is_record: false,
                                location: Span::new((), 71..74),
                                name: "Foo".to_string(),
                                arguments: vec![],
                                module: None,
                                constructor: (),
                                with_spread: false,
                                tipo: (),
                            }],
                            guard: None,
                            then: expr::UntypedExpr::Trace {
                                kind: ast::TraceKind::Todo,
                                location: Span::new((), 78..82),
                                then: Box::new(expr::UntypedExpr::ErrorTerm {
                                    location: Span::new((), 78..82),
                                }),
                                text: Box::new(expr::UntypedExpr::String {
                                    location: Span::new((), 78..82),
                                    value: "aiken::todo".to_string(),
                                }),
                            },
                        },
                        ast::UntypedClause {
                            location: Span::new((), 89..100),
                            patterns: vec1![ast::Pattern::Constructor {
                                is_record: false,
                                location: Span::new((), 89..92),
                                name: "Bar".to_string(),
                                arguments: vec![],
                                module: None,
                                constructor: (),
                                with_spread: false,
                                tipo: (),
                            }],
                            guard: None,
                            then: expr::UntypedExpr::Var {
                                location: Span::new((), 96..100),
                                name: "True".to_string(),
                            },
                        },
                        ast::UntypedClause {
                            location: Span::new((), 107..117),
                            patterns: vec1![ast::Pattern::Discard {
                                name: "_".to_string(),
                                location: Span::new((), 107..108),
                            }],
                            guard: None,
                            then: expr::UntypedExpr::Var {
                                location: Span::new((), 112..117),
                                name: "False".to_string(),
                            },
                        },
                    ],
                },
                doc: None,
                location: Span::new((), 40..48),
                name: "bar".to_string(),
                public: false,
                return_annotation: None,
                return_type: (),
                end_position: 122,
            }),
        ],
    )
}

#[test]
fn brackets_followed_by_parenthesis() {
    fn assert_sequence(code: &str) {
        let (module, _extra) = parser::module(code, ast::ModuleKind::Validator).unwrap();
        assert!(
            matches!(
                module.definitions[..],
                [ast::Definition::Test(Function {
                    body: expr::UntypedExpr::Sequence { .. },
                    ..
                })]
            ),
            "{}",
            code.to_string()
        );
    }

    assert_sequence(indoc! {r#"
       test foo () {
         let a = []
         (x |> y) == []
       }
   "#});

    assert_sequence(indoc! {r#"
       test foo () {
         let a = []
         (x |> y) == []
       }
   "#});

    assert_sequence(indoc! {r#"
        test foo () {
          let a = []
          // foo
          (x |> y) == []
        }
    "#});
}

#[test]
fn int_parsing_hex() {
    let code = indoc! {r#"
          fn foo() {
            let i = 0xff
          }
        "#};
    assert_definitions(
        code,
        vec![ast::Definition::Fn(Function {
            arguments: vec![],
            body: expr::UntypedExpr::Assignment {
                location: Span::new((), 13..25),
                value: Box::new(expr::UntypedExpr::Int {
                    location: Span::new((), 21..25),
                    value: "255".to_string(),
                    base: Base::Hexadecimal,
                }),
                pattern: ast::Pattern::Var {
                    location: Span::new((), 17..18),
                    name: "i".to_string(),
                },
                kind: ast::AssignmentKind::Let,
                annotation: None,
            },
            doc: None,
            location: Span::new((), 0..8),
            name: "foo".to_string(),
            public: false,
            return_annotation: None,
            return_type: (),
            end_position: 26,
            can_error: true,
        })],
    )
}

#[test]
fn int_parsing_hex_bytes() {
    let code = indoc! {r#"
          fn foo() {
            #[ 0x01, 0xa2, 0x03 ]
          }
        "#};
    assert_definitions(
        code,
        vec![ast::Definition::Fn(Function {
            arguments: vec![],
            body: expr::UntypedExpr::ByteArray {
                location: Span::new((), 13..34),
                bytes: vec![1, 162, 3],
                preferred_format: ast::ByteArrayFormatPreference::ArrayOfBytes(Base::Hexadecimal),
            },
            doc: None,
            location: Span::new((), 0..8),
            name: "foo".to_string(),
            public: false,
            return_annotation: None,
            return_type: (),
            end_position: 35,
            can_error: true,
        })],
    )
}

#[test]
fn int_parsing_numeric_underscore() {
    let code = indoc! {r#"
          fn foo() {
            let i = 1_234_567
            let j = 1_000_000
          }
        "#};
    assert_definitions(
        code,
        vec![ast::Definition::Fn(Function {
            arguments: vec![],
            body: expr::UntypedExpr::Sequence {
                location: Span::new((), 13..50),
                expressions: vec![
                    expr::UntypedExpr::Assignment {
                        location: Span::new((), 13..30),
                        value: Box::new(expr::UntypedExpr::Int {
                            location: Span::new((), 21..30),
                            value: "1234567".to_string(),
                            base: Base::Decimal {
                                numeric_underscore: true,
                            },
                        }),
                        pattern: ast::Pattern::Var {
                            location: Span::new((), 17..18),
                            name: "i".to_string(),
                        },
                        kind: ast::AssignmentKind::Let,
                        annotation: None,
                    },
                    expr::UntypedExpr::Assignment {
                        location: Span::new((), 33..50),
                        value: Box::new(expr::UntypedExpr::Int {
                            location: Span::new((), 41..50),
                            value: "1000000".to_string(),
                            base: Base::Decimal {
                                numeric_underscore: true,
                            },
                        }),
                        pattern: ast::Pattern::Var {
                            location: Span::new((), 37..38),
                            name: "j".to_string(),
                        },
                        kind: ast::AssignmentKind::Let,
                        annotation: None,
                    },
                ],
            },
            doc: None,
            location: Span::new((), 0..8),
            name: "foo".to_string(),
            public: false,
            return_annotation: None,
            return_type: (),
            end_position: 51,
            can_error: true,
        })],
    )
}
