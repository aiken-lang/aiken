use chumsky::prelude::*;
use indoc::indoc;
use pretty_assertions::assert_eq;

use crate::{
    ast::{self, Constant, DataType, Function, ModuleConstant, Span, TypeAlias, Use},
    expr, parser,
};

fn assert_definitions(code: &str, definitions: Vec<ast::UntypedDefinition>) {
    let (module, _extra) = parser::module(code, ast::ModuleKind::Validator).unwrap();

    assert_eq!(
        module,
        ast::UntypedModule {
            docs: vec![],
            kind: ast::ModuleKind::Validator,
            name: "".to_string(),
            type_info: (),
            definitions,
        }
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
            arguments: vec![],
            body: expr::UntypedExpr::Todo {
                kind: ast::TodoKind::EmptyFunction,
                location: Span::new((), 0..15),
                label: None,
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
fn plus_binop() {
    let code = indoc! {r#"
        pub fn add_one(a) -> Int {
          a + 1
        }
    "#};

    assert_definitions(
        code,
        vec![ast::UntypedDefinition::Fn(Function {
            arguments: vec![ast::Arg {
                arg_name: ast::ArgName::Named {
                    label: "a".to_string(),
                    name: "a".to_string(),
                    location: Span::new((), 15..16),
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
            arguments: vec![ast::Arg {
                arg_name: ast::ArgName::Named {
                    name: "a".to_string(),
                    label: "thing".to_string(),
                    location: Span::new((), 13..20),
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
                            }),
                            right: Box::new(expr::UntypedExpr::Int {
                                location: Span::new((), 31..32),
                                value: "1".to_string(),
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
                            }),
                        },
                        body: expr::UntypedExpr::Int {
                            location: Span::new((), 57..58),
                            value: "5".to_string(),
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
                        },
                        location: Span::new((), 71..89),
                    },
                ],
                final_else: Box::new(expr::UntypedExpr::Int {
                    location: Span::new((), 101..102),
                    value: "3".to_string(),
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
            arguments: vec![ast::Arg {
                arg_name: ast::ArgName::Named {
                    label: "a".to_string(),
                    name: "a".to_string(),
                    location: Span::new((), 11..12),
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
                                },
                                expr::UntypedExpr::Int {
                                    location: Span::new((), 91..92),
                                    value: "2".to_string(),
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
            arguments: vec![ast::Arg {
                arg_name: ast::ArgName::Named {
                    label: "a".to_string(),
                    name: "a".to_string(),
                    location: Span::new((), 12..13),
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
          when a, b is {
            1, 2 -> 3
            1 | 4, 5 -> {
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
            arguments: vec![ast::Arg {
                arg_name: ast::ArgName::Named {
                    label: "a".to_string(),
                    name: "a".to_string(),
                    location: Span::new((), 12..13),
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
                location: Span::new((), 23..138),
                subjects: vec![
                    expr::UntypedExpr::Var {
                        location: Span::new((), 28..29),
                        name: "a".to_string(),
                    },
                    expr::UntypedExpr::Var {
                        location: Span::new((), 31..32),
                        name: "b".to_string(),
                    },
                ],
                clauses: vec![
                    ast::Clause {
                        location: Span::new((), 42..51),
                        pattern: vec![
                            ast::Pattern::Int {
                                location: Span::new((), 42..43),
                                value: "1".to_string(),
                            },
                            ast::Pattern::Int {
                                location: Span::new((), 45..46),
                                value: "2".to_string(),
                            },
                        ],
                        alternative_patterns: vec![],
                        guard: None,
                        then: expr::UntypedExpr::Int {
                            location: Span::new((), 50..51),
                            value: "3".to_string(),
                        },
                    },
                    ast::Clause {
                        location: Span::new((), 56..112),
                        pattern: vec![ast::Pattern::Int {
                            location: Span::new((), 56..57),
                            value: "1".to_string(),
                        }],
                        alternative_patterns: vec![vec![
                            ast::Pattern::Int {
                                location: Span::new((), 60..61),
                                value: "4".to_string(),
                            },
                            ast::Pattern::Int {
                                location: Span::new((), 63..64),
                                value: "5".to_string(),
                            },
                        ]],
                        guard: None,
                        then: expr::UntypedExpr::Sequence {
                            location: Span::new((), 76..106),
                            expressions: vec![
                                expr::UntypedExpr::Assignment {
                                    location: Span::new((), 76..91),
                                    value: Box::new(expr::UntypedExpr::Int {
                                        location: Span::new((), 90..91),
                                        value: "5".to_string(),
                                    }),
                                    pattern: ast::Pattern::Var {
                                        location: Span::new((), 80..87),
                                        name: "amazing".to_string(),
                                    },
                                    kind: ast::AssignmentKind::Let,
                                    annotation: None,
                                },
                                expr::UntypedExpr::Var {
                                    location: Span::new((), 99..106),
                                    name: "amazing".to_string(),
                                },
                            ],
                        },
                    },
                    ast::Clause {
                        location: Span::new((), 117..123),
                        pattern: vec![ast::Pattern::Int {
                            location: Span::new((), 117..118),
                            value: "3".to_string(),
                        }],
                        alternative_patterns: vec![],
                        guard: None,
                        then: expr::UntypedExpr::Int {
                            location: Span::new((), 122..123),
                            value: "9".to_string(),
                        },
                    },
                    ast::Clause {
                        location: Span::new((), 128..134),
                        pattern: vec![ast::Pattern::Discard {
                            name: "_".to_string(),
                            location: Span::new((), 128..129),
                        }],
                        alternative_patterns: vec![],
                        guard: None,
                        then: expr::UntypedExpr::Int {
                            location: Span::new((), 133..134),
                            value: "4".to_string(),
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
            end_position: 139,
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
                        expressions: vec1::vec1![
                            expr::UntypedExpr::Int {
                                location: Span::new((), 71..72),
                                value: "2".to_string(),
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
            arguments: vec![ast::Arg {
                arg_name: ast::ArgName::Named {
                    label: "user".to_string(),
                    name: "user".to_string(),
                    location: Span::new((), 8..12),
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
                                    },
                                    expr::UntypedExpr::Int {
                                        location: Span::new((), 101..102),
                                        value: "2".to_string(),
                                    },
                                    expr::UntypedExpr::Int {
                                        location: Span::new((), 104..105),
                                        value: "3".to_string(),
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
        fn update_name(user: User, name: String) -> User {
          User { ..user, name: "Aiken", age }
        }
    "#};

    assert_definitions(
        code,
        vec![ast::UntypedDefinition::Fn(Function {
            arguments: vec![
                ast::Arg {
                    arg_name: ast::ArgName::Named {
                        label: "user".to_string(),
                        name: "user".to_string(),
                        location: Span::new((), 15..19),
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
                    },
                    location: Span::new((), 27..39),
                    annotation: Some(ast::Annotation::Constructor {
                        location: Span::new((), 33..39),
                        module: None,
                        name: "String".to_string(),
                        arguments: vec![],
                    }),
                    tipo: (),
                },
            ],
            body: expr::UntypedExpr::RecordUpdate {
                location: Span::new((), 53..88),
                constructor: Box::new(expr::UntypedExpr::Var {
                    location: Span::new((), 53..57),
                    name: "User".to_string(),
                }),
                spread: ast::RecordUpdateSpread {
                    base: Box::new(expr::UntypedExpr::Var {
                        location: Span::new((), 62..66),
                        name: "user".to_string(),
                    }),
                    location: Span::new((), 60..66),
                },
                arguments: vec![
                    ast::UntypedRecordUpdateArg {
                        label: "name".to_string(),
                        location: Span::new((), 68..81),
                        value: expr::UntypedExpr::String {
                            location: Span::new((), 74..81),
                            value: "Aiken".to_string(),
                        },
                    },
                    ast::UntypedRecordUpdateArg {
                        label: "age".to_string(),
                        location: Span::new((), 83..86),
                        value: expr::UntypedExpr::Var {
                            location: Span::new((), 83..86),
                            name: "age".to_string(),
                        },
                    },
                ],
            },
            doc: None,
            location: Span::new((), 0..48),
            name: "update_name".to_string(),
            public: false,
            return_annotation: Some(ast::Annotation::Constructor {
                location: Span::new((), 44..48),
                module: None,
                name: "User".to_string(),
                arguments: vec![],
            }),
            return_type: (),
            end_position: 89,
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
            arguments: vec![],
            body: expr::UntypedExpr::Call {
                arguments: vec![
                    ast::CallArg {
                        label: Some("name".to_string()),
                        location: Span::new((), 23..36),
                        value: expr::UntypedExpr::String {
                            location: Span::new((), 29..36),
                            value: "Aiken".to_string(),
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
            arguments: vec![],
            body: expr::UntypedExpr::Call {
                arguments: vec![
                    ast::CallArg {
                        label: Some("name".to_string()),
                        location: Span::new((), 35..48),
                        value: expr::UntypedExpr::String {
                            location: Span::new((), 41..48),
                            value: "Aiken".to_string(),
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
            arguments: vec![],
            body: expr::UntypedExpr::Call {
                arguments: vec![
                    ast::CallArg {
                        label: None,
                        location: Span::new((), 34..35),
                        value: expr::UntypedExpr::Int {
                            location: Span::new((), 34..35),
                            value: "1".to_string(),
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
                                },
                                expr::UntypedExpr::Int {
                                    location: Span::new((), 29..30),
                                    value: "2".to_string(),
                                },
                                expr::UntypedExpr::Int {
                                    location: Span::new((), 32..33),
                                    value: "3".to_string(),
                                },
                                expr::UntypedExpr::Int {
                                    location: Span::new((), 35..36),
                                    value: "4".to_string(),
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
            }),
            tipo: (),
        })],
    )
}

#[test]
fn base16_bytearray_literals() {
    let code = indoc! {r#"
        pub const my_policy_id = #"00aaff"
    "#};

    assert_definitions(
        code,
        vec![ast::UntypedDefinition::ModuleConstant(ModuleConstant {
            doc: None,
            location: Span::new((), 0..34),
            public: true,
            name: "my_policy_id".to_string(),
            annotation: None,
            value: Box::new(Constant::ByteArray {
                location: Span::new((), 25..34),
                bytes: vec![0, 170, 255],
            }),
            tipo: (),
        })],
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
            doc: None,
            arguments: vec![],
            body: expr::UntypedExpr::Todo {
                kind: ast::TodoKind::EmptyFunction,
                location: Span::new((), 0..11),
                label: None,
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
                arguments: vec![],
                body: expr::UntypedExpr::Assignment {
                    location: Span::new((), 89..103),
                    value: Box::new(expr::UntypedExpr::BinOp {
                        location: Span::new((), 98..102),
                        name: ast::BinOp::AddInt,
                        left: Box::new(expr::UntypedExpr::Int {
                            location: Span::new((), 98..100),
                            value: "40".to_string(),
                        }),
                        right: Box::new(expr::UntypedExpr::Int {
                            location: Span::new((), 101..102),
                            value: "2".to_string(),
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
                                }),
                            }),
                            right: Box::new(expr::UntypedExpr::Int {
                                location: Span::new((), 151..153),
                                value: "42".to_string(),
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
