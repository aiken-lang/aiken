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

            pub fn wow2(a: Int){
              let b = {
                let x = 4
                 
                x + 5
              }

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

            pub fn such() -> Int {
                let add_one = fn (a: Int) -> Int { a + 1 }

                2 |> add_one
            }

            fn run() {}

            fn name(user: User) {
                user.name
            }

            fn calls() {
                let x = add_one(3)

                let map_add_x = list.map(_, fn (y) { x + y })

                map_add_x([ 1, 2, 3 ])
            }

            fn update_name(user: User, name: String) -> User {
                User { ..user, name: "Aiken", }
            }

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
                ast::UntypedDefinition::Fn {
                    arguments: vec![ast::Arg {
                        arg_name: ast::ArgName::Named {
                            name: "a".to_string(),
                            location: Span::new(SrcId::empty(), 866..867),
                        },
                        location: Span::new(SrcId::empty(), 866..872),
                        annotation: Some(ast::Annotation::Constructor {
                            location: Span::new(SrcId::empty(), 869..872),
                            module: None,
                            name: "Int".to_string(),
                            arguments: vec![],
                        },),
                        tipo: (),
                    },],
                    body: expr::UntypedExpr::Sequence {
                        location: Span::new(SrcId::empty(), 889..1225),
                        expressions: vec![
                            expr::UntypedExpr::Assignment {
                                location: Span::new(SrcId::empty(), 889..980),
                                value: Box::new(expr::UntypedExpr::Sequence {
                                    location: Span::new(SrcId::empty(), 915..964),
                                    expressions: vec![
                                        expr::UntypedExpr::Assignment {
                                            location: Span::new(SrcId::empty(), 915..924),
                                            value: Box::new(expr::UntypedExpr::Int {
                                                location: Span::new(SrcId::empty(), 923..924),
                                                value: "4".to_string(),
                                            }),
                                            pattern: ast::Pattern::Var {
                                                location: Span::new(SrcId::empty(), 919..920),
                                                name: "x".to_string(),
                                            },
                                            kind: ast::AssignmentKind::Let,
                                            annotation: None,
                                        },
                                        expr::UntypedExpr::BinOp {
                                            location: Span::new(SrcId::empty(), 959..964),
                                            name: ast::BinOp::AddInt,
                                            left: Box::new(expr::UntypedExpr::Var {
                                                location: Span::new(SrcId::empty(), 959..960),
                                                name: "x".to_string(),
                                            }),
                                            right: Box::new(expr::UntypedExpr::Int {
                                                location: Span::new(SrcId::empty(), 963..964),
                                                value: "5".to_string(),
                                            }),
                                        },
                                    ],
                                }),
                                pattern: ast::Pattern::Var {
                                    location: Span::new(SrcId::empty(), 893..894),
                                    name: "b".to_string(),
                                },
                                kind: ast::AssignmentKind::Let,
                                annotation: None,
                            },
                            expr::UntypedExpr::When {
                                location: Span::new(SrcId::empty(), 996..1225),
                                subjects: vec![
                                    expr::UntypedExpr::Var {
                                        location: Span::new(SrcId::empty(), 1001..1002),
                                        name: "a".to_string(),
                                    },
                                    expr::UntypedExpr::Var {
                                        location: Span::new(SrcId::empty(), 1004..1005),
                                        name: "b".to_string(),
                                    },
                                ],
                                clauses: vec![
                                    ast::Clause {
                                        location: Span::new(SrcId::empty(), 1027..1036),
                                        pattern: vec![
                                            ast::Pattern::Int {
                                                location: Span::new(SrcId::empty(), 1027..1028),
                                                value: "1".to_string(),
                                            },
                                            ast::Pattern::Int {
                                                location: Span::new(SrcId::empty(), 1030..1031),
                                                value: "2".to_string(),
                                            },
                                        ],
                                        alternative_patterns: vec![],
                                        guard: None,
                                        then: expr::UntypedExpr::Int {
                                            location: Span::new(SrcId::empty(), 1035..1036),
                                            value: "3".to_string(),
                                        },
                                    },
                                    ast::Clause {
                                        location: Span::new(SrcId::empty(), 1053..1163),
                                        pattern: vec![ast::Pattern::Int {
                                            location: Span::new(SrcId::empty(), 1053..1054),
                                            value: "1".to_string(),
                                        },],
                                        alternative_patterns: vec![vec![
                                            ast::Pattern::Int {
                                                location: Span::new(SrcId::empty(), 1057..1058),
                                                value: "4".to_string(),
                                            },
                                            ast::Pattern::Int {
                                                location: Span::new(SrcId::empty(), 1060..1061),
                                                value: "5".to_string(),
                                            },
                                        ],],
                                        guard: None,
                                        then: expr::UntypedExpr::Sequence {
                                            location: Span::new(SrcId::empty(), 1085..1145),
                                            expressions: vec![
                                                expr::UntypedExpr::Assignment {
                                                    location: Span::new(SrcId::empty(), 1085..1100),
                                                    value: Box::new(expr::UntypedExpr::Int {
                                                        location: Span::new(
                                                            SrcId::empty(),
                                                            1099..1100
                                                        ),
                                                        value: "5".to_string(),
                                                    }),
                                                    pattern: ast::Pattern::Var {
                                                        location: Span::new(
                                                            SrcId::empty(),
                                                            1089..1096
                                                        ),
                                                        name: "amazing".to_string(),
                                                    },
                                                    kind: ast::AssignmentKind::Let,
                                                    annotation: None,
                                                },
                                                expr::UntypedExpr::Var {
                                                    location: Span::new(SrcId::empty(), 1138..1145),
                                                    name: "amazing".to_string(),
                                                },
                                            ],
                                        },
                                    },
                                    ast::Clause {
                                        location: Span::new(SrcId::empty(), 1180..1186),
                                        pattern: vec![ast::Pattern::Int {
                                            location: Span::new(SrcId::empty(), 1180..1181),
                                            value: "3".to_string(),
                                        },],
                                        alternative_patterns: vec![],
                                        guard: None,
                                        then: expr::UntypedExpr::Int {
                                            location: Span::new(SrcId::empty(), 1185..1186),
                                            value: "9".to_string(),
                                        },
                                    },
                                    ast::Clause {
                                        location: Span::new(SrcId::empty(), 1203..1209),
                                        pattern: vec![ast::Pattern::Discard {
                                            name: "_".to_string(),
                                            location: Span::new(SrcId::empty(), 1203..1204),
                                        },],
                                        alternative_patterns: vec![],
                                        guard: None,
                                        then: expr::UntypedExpr::Int {
                                            location: Span::new(SrcId::empty(), 1208..1209),
                                            value: "4".to_string(),
                                        },
                                    },
                                ],
                            },
                        ],
                    },
                    doc: None,
                    location: Span::new(SrcId::empty(), 854..1239),
                    name: "wow2".to_string(),
                    public: true,
                    return_annotation: None,
                    return_type: (),
                },
                ast::UntypedDefinition::Fn {
                    arguments: vec![],
                    body: expr::UntypedExpr::Sequence {
                        location: Span::new(SrcId::empty(), 1292..1364),
                        expressions: vec![
                            expr::UntypedExpr::Assignment {
                                location: Span::new(SrcId::empty(), 1292..1334),
                                value: Box::new(expr::UntypedExpr::Fn {
                                    location: Span::new(SrcId::empty(), 1306..1334),
                                    is_capture: false,
                                    arguments: vec![ast::Arg {
                                        arg_name: ast::ArgName::Named {
                                            name: "a".to_string(),
                                            location: Span::new(SrcId::empty(), 1310..1311),
                                        },
                                        location: Span::new(SrcId::empty(), 1310..1316),
                                        annotation: Some(ast::Annotation::Constructor {
                                            location: Span::new(SrcId::empty(), 1313..1316),
                                            module: None,
                                            name: "Int".to_string(),
                                            arguments: vec![],
                                        },),
                                        tipo: (),
                                    },],
                                    body: Box::new(expr::UntypedExpr::BinOp {
                                        location: Span::new(SrcId::empty(), 1327..1332),
                                        name: ast::BinOp::AddInt,
                                        left: Box::new(expr::UntypedExpr::Var {
                                            location: Span::new(SrcId::empty(), 1327..1328),
                                            name: "a".to_string(),
                                        }),
                                        right: Box::new(expr::UntypedExpr::Int {
                                            location: Span::new(SrcId::empty(), 1331..1332),
                                            value: "1".to_string(),
                                        }),
                                    }),
                                    return_annotation: Some(ast::Annotation::Constructor {
                                        location: Span::new(SrcId::empty(), 1321..1324),
                                        module: None,
                                        name: "Int".to_string(),
                                        arguments: vec![],
                                    },),
                                }),
                                pattern: ast::Pattern::Var {
                                    location: Span::new(SrcId::empty(), 1296..1303),
                                    name: "add_one".to_string(),
                                },
                                kind: ast::AssignmentKind::Let,
                                annotation: None,
                            },
                            expr::UntypedExpr::PipeLine {
                                expressions: vec1::vec1![
                                    expr::UntypedExpr::Int {
                                        location: Span::new(SrcId::empty(), 1352..1353),
                                        value: "2".to_string(),
                                    },
                                    expr::UntypedExpr::Var {
                                        location: Span::new(SrcId::empty(), 1357..1364),
                                        name: "add_one".to_string(),
                                    },
                                ],
                            },
                        ],
                    },
                    doc: None,
                    location: Span::new(SrcId::empty(), 1253..1378),
                    name: "such".to_string(),
                    public: true,
                    return_annotation: Some(ast::Annotation::Constructor {
                        location: Span::new(SrcId::empty(), 1270..1273),
                        module: None,
                        name: "Int".to_string(),
                        arguments: vec![],
                    },),
                    return_type: (),
                },
                ast::UntypedDefinition::Fn {
                    arguments: vec![],
                    body: expr::UntypedExpr::Todo {
                        kind: ast::TodoKind::EmptyFunction,
                        location: Span::new(SrcId::empty(), 1392..1403),
                        label: None,
                    },
                    doc: None,
                    location: Span::new(SrcId::empty(), 1392..1403),
                    name: "run".to_string(),
                    public: false,
                    return_annotation: None,
                    return_type: (),
                },
                ast::UntypedDefinition::Fn {
                    arguments: vec![ast::Arg {
                        arg_name: ast::ArgName::Named {
                            name: "user".to_string(),
                            location: Span::new(SrcId::empty(), 1425..1429),
                        },
                        location: Span::new(SrcId::empty(), 1425..1435),
                        annotation: Some(ast::Annotation::Constructor {
                            location: Span::new(SrcId::empty(), 1431..1435),
                            module: None,
                            name: "User".to_string(),
                            arguments: vec![],
                        },),
                        tipo: (),
                    },],
                    body: expr::UntypedExpr::FieldAccess {
                        location: Span::new(SrcId::empty(), 1455..1464),
                        label: "name".to_string(),
                        container: Box::new(expr::UntypedExpr::Var {
                            location: Span::new(SrcId::empty(), 1455..1459),
                            name: "user".to_string(),
                        }),
                    },
                    doc: None,
                    location: Span::new(SrcId::empty(), 1417..1478),
                    name: "name".to_string(),
                    public: false,
                    return_annotation: None,
                    return_type: (),
                },
                ast::UntypedDefinition::Fn {
                    arguments: vec![],
                    body: expr::UntypedExpr::Sequence {
                        location: Span::new(SrcId::empty(), 1521..1642),
                        expressions: vec![
                            expr::UntypedExpr::Assignment {
                                location: Span::new(SrcId::empty(), 1521..1539),
                                value: Box::new(expr::UntypedExpr::Call {
                                    location: Span::new(SrcId::empty(), 1536..1539),
                                    fun: Box::new(expr::UntypedExpr::Var {
                                        location: Span::new(SrcId::empty(), 1529..1536),
                                        name: "add_one".to_string(),
                                    }),
                                    arguments: vec![ast::CallArg {
                                        label: None,
                                        location: Span::new(SrcId::empty(), 1537..1538),
                                        value: expr::UntypedExpr::Int {
                                            location: Span::new(SrcId::empty(), 1537..1538),
                                            value: "3".to_string(),
                                        },
                                    },],
                                }),
                                pattern: ast::Pattern::Var {
                                    location: Span::new(SrcId::empty(), 1525..1526),
                                    name: "x".to_string(),
                                },
                                kind: ast::AssignmentKind::Let,
                                annotation: None,
                            },
                            expr::UntypedExpr::Assignment {
                                location: Span::new(SrcId::empty(), 1557..1602),
                                value: Box::new(expr::UntypedExpr::Fn {
                                    location: Span::new(SrcId::empty(), 1581..1602),
                                    is_capture: true,
                                    arguments: vec![ast::Arg {
                                        arg_name: ast::ArgName::Named {
                                            name: "_capture__0".to_string(),
                                            location: Span::new(SrcId::empty(), 0..0),
                                        },
                                        location: Span::new(SrcId::empty(), 0..0),
                                        annotation: None,
                                        tipo: (),
                                    },],
                                    body: Box::new(expr::UntypedExpr::Call {
                                        location: Span::new(SrcId::empty(), 1581..1602),
                                        fun: Box::new(expr::UntypedExpr::FieldAccess {
                                            location: Span::new(SrcId::empty(), 1573..1581),
                                            label: "map".to_string(),
                                            container: Box::new(expr::UntypedExpr::Var {
                                                location: Span::new(SrcId::empty(), 1573..1577),
                                                name: "list".to_string(),
                                            }),
                                        }),
                                        arguments: vec![
                                            ast::CallArg {
                                                label: None,
                                                location: Span::new(SrcId::empty(), 1582..1583),
                                                value: expr::UntypedExpr::Var {
                                                    location: Span::new(SrcId::empty(), 1582..1583),
                                                    name: "_capture__0".to_string(),
                                                },
                                            },
                                            ast::CallArg {
                                                label: None,
                                                location: Span::new(SrcId::empty(), 1585..1601),
                                                value: expr::UntypedExpr::Fn {
                                                    location: Span::new(SrcId::empty(), 1585..1601),
                                                    is_capture: false,
                                                    arguments: vec![ast::Arg {
                                                        arg_name: ast::ArgName::Named {
                                                            name: "y".to_string(),
                                                            location: Span::new(
                                                                SrcId::empty(),
                                                                1589..1590
                                                            ),
                                                        },
                                                        location: Span::new(
                                                            SrcId::empty(),
                                                            1589..1590
                                                        ),
                                                        annotation: None,
                                                        tipo: (),
                                                    },],
                                                    body: Box::new(expr::UntypedExpr::BinOp {
                                                        location: Span::new(
                                                            SrcId::empty(),
                                                            1594..1599
                                                        ),
                                                        name: ast::BinOp::AddInt,
                                                        left: Box::new(expr::UntypedExpr::Var {
                                                            location: Span::new(
                                                                SrcId::empty(),
                                                                1594..1595
                                                            ),
                                                            name: "x".to_string(),
                                                        }),
                                                        right: Box::new(expr::UntypedExpr::Var {
                                                            location: Span::new(
                                                                SrcId::empty(),
                                                                1598..1599
                                                            ),
                                                            name: "y".to_string(),
                                                        }),
                                                    }),
                                                    return_annotation: None,
                                                },
                                            },
                                        ],
                                    }),
                                    return_annotation: None,
                                }),
                                pattern: ast::Pattern::Var {
                                    location: Span::new(SrcId::empty(), 1561..1570),
                                    name: "map_add_x".to_string(),
                                },
                                kind: ast::AssignmentKind::Let,
                                annotation: None,
                            },
                            expr::UntypedExpr::Call {
                                location: Span::new(SrcId::empty(), 1629..1642),
                                fun: Box::new(expr::UntypedExpr::Var {
                                    location: Span::new(SrcId::empty(), 1620..1629),
                                    name: "map_add_x".to_string(),
                                }),
                                arguments: vec![ast::CallArg {
                                    label: None,
                                    location: Span::new(SrcId::empty(), 1630..1641),
                                    value: expr::UntypedExpr::List {
                                        location: Span::new(SrcId::empty(), 1630..1641),
                                        elements: vec![
                                            expr::UntypedExpr::Int {
                                                location: Span::new(SrcId::empty(), 1632..1633),
                                                value: "1".to_string(),
                                            },
                                            expr::UntypedExpr::Int {
                                                location: Span::new(SrcId::empty(), 1635..1636),
                                                value: "2".to_string(),
                                            },
                                            expr::UntypedExpr::Int {
                                                location: Span::new(SrcId::empty(), 1638..1639),
                                                value: "3".to_string(),
                                            },
                                        ],
                                        tail: None,
                                    },
                                },],
                            },
                        ],
                    },
                    doc: None,
                    location: Span::new(SrcId::empty(), 1492..1656),
                    name: "calls".to_string(),
                    public: false,
                    return_annotation: None,
                    return_type: (),
                },
                ast::UntypedDefinition::Fn {
                    arguments: vec![
                        ast::Arg {
                            arg_name: ast::ArgName::Named {
                                name: "user".to_string(),
                                location: Span::new(SrcId::empty(), 1685..1689),
                            },
                            location: Span::new(SrcId::empty(), 1685..1695),
                            annotation: Some(ast::Annotation::Constructor {
                                location: Span::new(SrcId::empty(), 1691..1695),
                                module: None,
                                name: "User".to_string(),
                                arguments: vec![],
                            },),
                            tipo: (),
                        },
                        ast::Arg {
                            arg_name: ast::ArgName::Named {
                                name: "name".to_string(),
                                location: Span::new(SrcId::empty(), 1697..1701),
                            },
                            location: Span::new(SrcId::empty(), 1697..1709),
                            annotation: Some(ast::Annotation::Constructor {
                                location: Span::new(SrcId::empty(), 1703..1709),
                                module: None,
                                name: "String".to_string(),
                                arguments: vec![],
                            },),
                            tipo: (),
                        },
                    ],
                    body: expr::UntypedExpr::RecordUpdate {
                        location: Span::new(SrcId::empty(), 1737..1768),
                        constructor: Box::new(expr::UntypedExpr::Var {
                            location: Span::new(SrcId::empty(), 1737..1741),
                            name: "User".to_string(),
                        }),
                        spread: ast::RecordUpdateSpread {
                            base: Box::new(expr::UntypedExpr::Var {
                                location: Span::new(SrcId::empty(), 1746..1750),
                                name: "user".to_string(),
                            }),
                            location: Span::new(SrcId::empty(), 1742..1768),
                        },
                        arguments: vec![ast::UntypedRecordUpdateArg {
                            label: "name".to_string(),
                            location: Span::new(SrcId::empty(), 1752..1765),
                            value: expr::UntypedExpr::String {
                                location: Span::new(SrcId::empty(), 1758..1765),
                                value: "Aiken".to_string(),
                            },
                        },],
                    },
                    doc: None,
                    location: Span::new(SrcId::empty(), 1670..1782),
                    name: "update_name".to_string(),
                    public: false,
                    return_annotation: Some(ast::Annotation::Constructor {
                        location: Span::new(SrcId::empty(), 1714..1718),
                        module: None,
                        name: "User".to_string(),
                        arguments: vec![],
                    },),
                    return_type: (),
                },
                ast::UntypedDefinition::Fn {
                    arguments: vec![],
                    body: expr::UntypedExpr::If {
                        location: Span::new(SrcId::empty(), 1823..2036),
                        branches: vec1::vec1![
                            ast::IfBranch {
                                condition: expr::UntypedExpr::Var {
                                    location: Span::new(SrcId::empty(), 1826..1830),
                                    name: "True".to_string(),
                                },
                                body: expr::UntypedExpr::BinOp {
                                    location: Span::new(SrcId::empty(), 1853..1858),
                                    name: ast::BinOp::AddInt,
                                    left: Box::new(expr::UntypedExpr::Int {
                                        location: Span::new(SrcId::empty(), 1853..1854),
                                        value: "1".to_string(),
                                    }),
                                    right: Box::new(expr::UntypedExpr::Int {
                                        location: Span::new(SrcId::empty(), 1857..1858),
                                        value: "1".to_string(),
                                    }),
                                },
                                location: Span::new(SrcId::empty(), 1826..1876),
                            },
                            ast::IfBranch {
                                condition: expr::UntypedExpr::BinOp {
                                    location: Span::new(SrcId::empty(), 1885..1890),
                                    name: ast::BinOp::LtInt,
                                    left: Box::new(expr::UntypedExpr::Var {
                                        location: Span::new(SrcId::empty(), 1885..1886),
                                        name: "a".to_string(),
                                    }),
                                    right: Box::new(expr::UntypedExpr::Int {
                                        location: Span::new(SrcId::empty(), 1889..1890),
                                        value: "4".to_string(),
                                    }),
                                },
                                body: expr::UntypedExpr::Int {
                                    location: Span::new(SrcId::empty(), 1913..1914),
                                    value: "5".to_string(),
                                },
                                location: Span::new(SrcId::empty(), 1885..1932),
                            },
                            ast::IfBranch {
                                condition: expr::UntypedExpr::BinOp {
                                    location: Span::new(SrcId::empty(), 1941..1947),
                                    name: ast::BinOp::Or,
                                    left: Box::new(expr::UntypedExpr::Var {
                                        location: Span::new(SrcId::empty(), 1941..1942),
                                        name: "a".to_string(),
                                    }),
                                    right: Box::new(expr::UntypedExpr::Var {
                                        location: Span::new(SrcId::empty(), 1946..1947),
                                        name: "b".to_string(),
                                    }),
                                },
                                body: expr::UntypedExpr::Int {
                                    location: Span::new(SrcId::empty(), 1970..1971),
                                    value: "6".to_string(),
                                },
                                location: Span::new(SrcId::empty(), 1941..1989),
                            },
                        ],
                        final_else: Box::new(expr::UntypedExpr::Int {
                            location: Span::new(SrcId::empty(), 2017..2018),
                            value: "3".to_string(),
                        }),
                    },
                    doc: None,
                    location: Span::new(SrcId::empty(), 1796..2050),
                    name: "ifs".to_string(),
                    public: false,
                    return_annotation: None,
                    return_type: (),
                },
            ]
        },
    );
}
