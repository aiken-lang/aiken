use chumsky::prelude::*;
use pretty_assertions::assert_eq;

use crate::{
    ast::{self, Span, SrcId},
    expr, lexer,
    parser::module_parser,
};

#[test]
fn module() {
    let code = r#"
            use std/list
            use std/address.{Address as A, thing as w}
            use std/tx as t

            type Option(a) {
              Some(a, Int)
              None
              Wow { name: Int, age: Int }
            }

            pub opaque type User {
              name: _w
            }

            type Thing = Option(Int)

            pub type Me = Option(String)

            pub fn add_one(a) -> Int {
              a + 1
            }

            pub fn thing(thing a: Int) {
                a + 2
                |> add_one
                |> add_one
            }

            pub fn wow(a: Int) {
              let x =
                a + 2
                |> add_one
                |> add_one
        
              let thing = [ 1, 2, a ]

              let idk = thing

              y
            }
        "#;
    let len = code.chars().count();

    let span = |i| Span::new(SrcId::empty(), i..i + 1);

    let tokens = lexer::lexer()
        .parse(chumsky::Stream::from_iter(
            span(len),
            code.chars().enumerate().map(|(i, c)| (c, span(i))),
        ))
        .unwrap();

    let res = module_parser(ast::ModuleKind::Script)
        .parse(chumsky::Stream::from_iter(span(len), tokens.into_iter()))
        .unwrap();

    assert_eq!(
        res,
        ast::UntypedModule {
            docs: vec![],
            kind: ast::ModuleKind::Script,
            name: vec![],
            type_info: (),
            definitions: vec![
                ast::UntypedDefinition::Use {
                    location: Span::new(SrcId::empty(), 13..25),
                    module: vec!["std".to_string(), "list".to_string()],
                    as_name: None,
                    unqualified: vec![],
                    package: (),
                },
                ast::UntypedDefinition::Use {
                    location: Span::new(SrcId::empty(), 38..80),
                    module: vec!["std".to_string(), "address".to_string()],
                    as_name: None,
                    unqualified: vec![
                        ast::UnqualifiedImport {
                            as_name: Some("A".to_string()),
                            location: Span::new(SrcId::empty(), 55..67),
                            layer: Default::default(),
                            name: "Address".to_string()
                        },
                        ast::UnqualifiedImport {
                            as_name: Some("w".to_string()),
                            location: Span::new(SrcId::empty(), 69..79),
                            layer: Default::default(),
                            name: "thing".to_string()
                        }
                    ],
                    package: (),
                },
                ast::UntypedDefinition::Use {
                    location: Span::new(SrcId::empty(), 93..108),
                    module: vec!["std".to_string(), "tx".to_string()],
                    as_name: Some("t".to_string()),
                    unqualified: vec![],
                    package: (),
                },
                ast::UntypedDefinition::DataType {
                    location: Span::new(SrcId::empty(), 122..240),
                    constructors: vec![
                        ast::RecordConstructor {
                            location: Span::new(SrcId::empty(), 153..165),
                            name: "Some".to_string(),
                            arguments: vec![
                                ast::RecordConstructorArg {
                                    label: None,
                                    annotation: ast::Annotation::Var {
                                        location: Span::new(SrcId::empty(), 158..159),
                                        name: "a".to_string(),
                                    },
                                    location: Span::new(SrcId::empty(), 158..159),
                                    tipo: (),
                                    doc: None,
                                },
                                ast::RecordConstructorArg {
                                    label: None,
                                    annotation: ast::Annotation::Constructor {
                                        location: Span::new(SrcId::empty(), 161..164),
                                        module: None,
                                        name: "Int".to_string(),
                                        arguments: vec![],
                                    },
                                    location: Span::new(SrcId::empty(), 161..164),
                                    tipo: (),
                                    doc: None,
                                },
                            ],
                            documentation: None,
                            sugar: false,
                        },
                        ast::RecordConstructor {
                            location: Span::new(SrcId::empty(), 180..184),
                            name: "None".to_string(),
                            arguments: vec![],
                            documentation: None,
                            sugar: false,
                        },
                        ast::RecordConstructor {
                            location: Span::new(SrcId::empty(), 199..226),
                            name: "Wow".to_string(),
                            arguments: vec![
                                ast::RecordConstructorArg {
                                    label: Some("name".to_string(),),
                                    annotation: ast::Annotation::Constructor {
                                        location: Span::new(SrcId::empty(), 211..214),
                                        module: None,
                                        name: "Int".to_string(),
                                        arguments: vec![],
                                    },
                                    location: Span::new(SrcId::empty(), 205..214),
                                    tipo: (),
                                    doc: None,
                                },
                                ast::RecordConstructorArg {
                                    label: Some("age".to_string(),),
                                    annotation: ast::Annotation::Constructor {
                                        location: Span::new(SrcId::empty(), 221..224),
                                        module: None,
                                        name: "Int".to_string(),
                                        arguments: vec![],
                                    },
                                    location: Span::new(SrcId::empty(), 216..224),
                                    tipo: (),
                                    doc: None,
                                },
                            ],
                            documentation: None,
                            sugar: false,
                        },
                    ],
                    doc: None,
                    name: "Option".to_string(),
                    opaque: false,
                    parameters: vec!["a".to_string(),],
                    public: false,
                    typed_parameters: vec![],
                },
                ast::UntypedDefinition::DataType {
                    location: Span::new(SrcId::empty(), 254..313),
                    constructors: vec![ast::RecordConstructor {
                        location: Span::new(SrcId::empty(), 275..313),
                        name: "User".to_string(),
                        arguments: vec![ast::RecordConstructorArg {
                            label: Some("name".to_string()),
                            annotation: ast::Annotation::Hole {
                                location: Span::new(SrcId::empty(), 297..299),
                                name: "_w".to_string(),
                            },
                            location: Span::new(SrcId::empty(), 291..299),
                            tipo: (),
                            doc: None,
                        },],
                        documentation: None,
                        sugar: true,
                    },],
                    doc: None,
                    name: "User".to_string(),
                    opaque: true,
                    parameters: vec![],
                    public: true,
                    typed_parameters: vec![],
                },
                ast::UntypedDefinition::TypeAlias {
                    alias: "Thing".to_string(),
                    annotation: ast::Annotation::Constructor {
                        location: Span::new(SrcId::empty(), 340..351),
                        module: None,
                        name: "Option".to_string(),
                        arguments: vec![ast::Annotation::Constructor {
                            location: Span::new(SrcId::empty(), 347..350),
                            module: None,
                            name: "Int".to_string(),
                            arguments: vec![],
                        },],
                    },
                    doc: None,
                    location: Span::new(SrcId::empty(), 327..351),
                    parameters: vec![],
                    public: false,
                    tipo: (),
                },
                ast::UntypedDefinition::TypeAlias {
                    alias: "Me".to_string(),
                    annotation: ast::Annotation::Constructor {
                        location: Span::new(SrcId::empty(), 379..393),
                        module: None,
                        name: "Option".to_string(),
                        arguments: vec![ast::Annotation::Constructor {
                            location: Span::new(SrcId::empty(), 386..392),
                            module: None,
                            name: "String".to_string(),
                            arguments: vec![],
                        },],
                    },
                    doc: None,
                    location: Span::new(SrcId::empty(), 365..393),
                    parameters: vec![],
                    public: true,
                    tipo: (),
                },
                ast::UntypedDefinition::Fn {
                    arguments: vec![ast::Arg {
                        arg_name: ast::ArgName::Named {
                            name: "a".to_string(),
                            location: Span::new(SrcId::empty(), 422..423),
                        },
                        location: Span::new(SrcId::empty(), 422..423),
                        annotation: None,
                        tipo: (),
                    },],
                    body: expr::UntypedExpr::BinOp {
                        location: Span::new(SrcId::empty(), 448..453),
                        name: ast::BinOp::AddInt,
                        left: Box::new(expr::UntypedExpr::Var {
                            location: Span::new(SrcId::empty(), 448..449),
                            name: "a".to_string(),
                        }),
                        right: Box::new(expr::UntypedExpr::Int {
                            location: Span::new(SrcId::empty(), 452..453),
                            value: "1".to_string(),
                        }),
                    },
                    doc: None,
                    location: Span::new(SrcId::empty(), 407..467),
                    name: "add_one".to_string(),
                    public: true,
                    return_annotation: Some(ast::Annotation::Constructor {
                        location: Span::new(SrcId::empty(), 428..431),
                        module: None,
                        name: "Int".to_string(),
                        arguments: vec![],
                    },),
                    return_type: (),
                },
                ast::UntypedDefinition::Fn {
                    arguments: vec![ast::Arg {
                        arg_name: ast::ArgName::NamedLabeled {
                            name: "a".to_string(),
                            label: "thing".to_string(),
                            location: Span::new(SrcId::empty(), 494..501),
                        },
                        location: Span::new(SrcId::empty(), 494..506),
                        annotation: Some(ast::Annotation::Constructor {
                            location: Span::new(SrcId::empty(), 503..506),
                            module: None,
                            name: "Int".to_string(),
                            arguments: vec![],
                        },),
                        tipo: (),
                    },],
                    body: expr::UntypedExpr::PipeLine {
                        expressions: vec1::vec1![
                            expr::UntypedExpr::BinOp {
                                location: Span::new(SrcId::empty(), 526..531),
                                name: ast::BinOp::AddInt,
                                left: Box::new(expr::UntypedExpr::Var {
                                    location: Span::new(SrcId::empty(), 526..527),
                                    name: "a".to_string(),
                                }),
                                right: Box::new(expr::UntypedExpr::Int {
                                    location: Span::new(SrcId::empty(), 530..531),
                                    value: "2".to_string(),
                                }),
                            },
                            expr::UntypedExpr::Var {
                                location: Span::new(SrcId::empty(), 551..558),
                                name: "add_one".to_string(),
                            },
                            expr::UntypedExpr::Var {
                                location: Span::new(SrcId::empty(), 578..585),
                                name: "add_one".to_string(),
                            },
                        ],
                    },
                    doc: None,
                    location: Span::new(SrcId::empty(), 481..599),
                    name: "thing".to_string(),
                    public: true,
                    return_annotation: None,
                    return_type: (),
                },
                ast::UntypedDefinition::Fn {
                    arguments: vec![ast::Arg {
                        arg_name: ast::ArgName::Named {
                            name: "a".to_string(),
                            location: Span::new(SrcId::empty(), 624..625),
                        },
                        location: Span::new(SrcId::empty(), 624..630),
                        annotation: Some(ast::Annotation::Constructor {
                            location: Span::new(SrcId::empty(), 627..630),
                            module: None,
                            name: "Int".to_string(),
                            arguments: vec![],
                        },),
                        tipo: (),
                    },],
                    body: expr::UntypedExpr::Sequence {
                        location: Span::new(SrcId::empty(), 648..826),
                        expressions: vec![
                            expr::UntypedExpr::Assignment {
                                location: Span::new(SrcId::empty(), 648..731),
                                value: Box::new(expr::UntypedExpr::PipeLine {
                                    expressions: vec1::vec1![
                                        expr::UntypedExpr::BinOp {
                                            location: Span::new(SrcId::empty(), 672..677),
                                            name: ast::BinOp::AddInt,
                                            left: Box::new(expr::UntypedExpr::Var {
                                                location: Span::new(SrcId::empty(), 672..673),
                                                name: "a".to_string(),
                                            }),
                                            right: Box::new(expr::UntypedExpr::Int {
                                                location: Span::new(SrcId::empty(), 676..677),
                                                value: "2".to_string(),
                                            }),
                                        },
                                        expr::UntypedExpr::Var {
                                            location: Span::new(SrcId::empty(), 697..704),
                                            name: "add_one".to_string(),
                                        },
                                        expr::UntypedExpr::Var {
                                            location: Span::new(SrcId::empty(), 724..731),
                                            name: "add_one".to_string(),
                                        },
                                    ],
                                }),
                                pattern: ast::Pattern::Var {
                                    location: Span::new(SrcId::empty(), 652..653),
                                    name: "x".to_string(),
                                },
                                kind: ast::AssignmentKind::Let,
                                annotation: None,
                            },
                            expr::UntypedExpr::Assignment {
                                location: Span::new(SrcId::empty(), 755..778),
                                value: Box::new(expr::UntypedExpr::List {
                                    location: Span::new(SrcId::empty(), 767..778),
                                    elements: vec![
                                        expr::UntypedExpr::Int {
                                            location: Span::new(SrcId::empty(), 769..770),
                                            value: "1".to_string(),
                                        },
                                        expr::UntypedExpr::Int {
                                            location: Span::new(SrcId::empty(), 772..773),
                                            value: "2".to_string(),
                                        },
                                        expr::UntypedExpr::Var {
                                            location: Span::new(SrcId::empty(), 775..776),
                                            name: "a".to_string(),
                                        },
                                    ],
                                    tail: None,
                                }),
                                pattern: ast::Pattern::Var {
                                    location: Span::new(SrcId::empty(), 759..764),
                                    name: "thing".to_string(),
                                },
                                kind: ast::AssignmentKind::Let,
                                annotation: None,
                            },
                            expr::UntypedExpr::Assignment {
                                location: Span::new(SrcId::empty(), 794..809),
                                value: Box::new(expr::UntypedExpr::Var {
                                    location: Span::new(SrcId::empty(), 804..809),
                                    name: "thing".to_string(),
                                }),
                                pattern: ast::Pattern::Var {
                                    location: Span::new(SrcId::empty(), 798..801),
                                    name: "idk".to_string(),
                                },
                                kind: ast::AssignmentKind::Let,
                                annotation: None,
                            },
                            expr::UntypedExpr::Var {
                                location: Span::new(SrcId::empty(), 825..826),
                                name: "y".to_string(),
                            },
                        ],
                    },
                    doc: None,
                    location: Span::new(SrcId::empty(), 613..840),
                    name: "wow".to_string(),
                    public: true,
                    return_annotation: None,
                    return_type: (),
                },
            ]
        },
    );
}
