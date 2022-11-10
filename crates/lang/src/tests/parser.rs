use chumsky::prelude::*;
use pretty_assertions::assert_eq;

use crate::{
    ast::{self, DataType, Function, Span, TypeAlias, Use},
    expr, parser,
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

    let (module, _extra) = parser::module(code, ast::ModuleKind::Validator).unwrap();

    assert_eq!(
        module,
        ast::UntypedModule {
            docs: vec![],
            kind: ast::ModuleKind::Validator,
            name: "".to_string(),
            type_info: (),
            definitions: vec![
                ast::UntypedDefinition::Use(Use {
                    location: Span::new((), 13..25),
                    module: vec!["std".to_string(), "list".to_string()],
                    as_name: None,
                    unqualified: vec![],
                    package: (),
                }),
                ast::UntypedDefinition::Use(Use {
                    location: Span::new((), 38..80),
                    module: vec!["std".to_string(), "address".to_string()],
                    as_name: None,
                    unqualified: vec![
                        ast::UnqualifiedImport {
                            as_name: Some("A".to_string()),
                            location: Span::new((), 55..67),
                            layer: Default::default(),
                            name: "Address".to_string()
                        },
                        ast::UnqualifiedImport {
                            as_name: Some("w".to_string()),
                            location: Span::new((), 69..79),
                            layer: Default::default(),
                            name: "thing".to_string()
                        }
                    ],
                    package: (),
                }),
                ast::UntypedDefinition::Use(Use {
                    location: Span::new((), 93..108),
                    module: vec!["std".to_string(), "tx".to_string()],
                    as_name: Some("t".to_string()),
                    unqualified: vec![],
                    package: (),
                }),
                ast::UntypedDefinition::DataType(DataType {
                    location: Span::new((), 122..240),
                    constructors: vec![
                        ast::RecordConstructor {
                            location: Span::new((), 153..165),
                            name: "Some".to_string(),
                            arguments: vec![
                                ast::RecordConstructorArg {
                                    label: None,
                                    annotation: ast::Annotation::Var {
                                        location: Span::new((), 158..159),
                                        name: "a".to_string(),
                                    },
                                    location: Span::new((), 158..159),
                                    tipo: (),
                                    doc: None,
                                },
                                ast::RecordConstructorArg {
                                    label: None,
                                    annotation: ast::Annotation::Constructor {
                                        location: Span::new((), 161..164),
                                        module: None,
                                        name: "Int".to_string(),
                                        arguments: vec![],
                                    },
                                    location: Span::new((), 161..164),
                                    tipo: (),
                                    doc: None,
                                },
                            ],
                            documentation: None,
                            sugar: false,
                        },
                        ast::RecordConstructor {
                            location: Span::new((), 180..184),
                            name: "None".to_string(),
                            arguments: vec![],
                            documentation: None,
                            sugar: false,
                        },
                        ast::RecordConstructor {
                            location: Span::new((), 199..226),
                            name: "Wow".to_string(),
                            arguments: vec![
                                ast::RecordConstructorArg {
                                    label: Some("name".to_string(),),
                                    annotation: ast::Annotation::Constructor {
                                        location: Span::new((), 211..214),
                                        module: None,
                                        name: "Int".to_string(),
                                        arguments: vec![],
                                    },
                                    location: Span::new((), 205..214),
                                    tipo: (),
                                    doc: None,
                                },
                                ast::RecordConstructorArg {
                                    label: Some("age".to_string(),),
                                    annotation: ast::Annotation::Constructor {
                                        location: Span::new((), 221..224),
                                        module: None,
                                        name: "Int".to_string(),
                                        arguments: vec![],
                                    },
                                    location: Span::new((), 216..224),
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
                }),
                ast::UntypedDefinition::DataType(DataType {
                    location: Span::new((), 254..313),
                    constructors: vec![ast::RecordConstructor {
                        location: Span::new((), 275..313),
                        name: "User".to_string(),
                        arguments: vec![ast::RecordConstructorArg {
                            label: Some("name".to_string()),
                            annotation: ast::Annotation::Hole {
                                location: Span::new((), 297..299),
                                name: "_w".to_string(),
                            },
                            location: Span::new((), 291..299),
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
                }),
                ast::UntypedDefinition::TypeAlias(TypeAlias {
                    alias: "Thing".to_string(),
                    annotation: ast::Annotation::Constructor {
                        location: Span::new((), 340..351),
                        module: None,
                        name: "Option".to_string(),
                        arguments: vec![ast::Annotation::Constructor {
                            location: Span::new((), 347..350),
                            module: None,
                            name: "Int".to_string(),
                            arguments: vec![],
                        },],
                    },
                    doc: None,
                    location: Span::new((), 327..351),
                    parameters: vec![],
                    public: false,
                    tipo: (),
                }),
                ast::UntypedDefinition::TypeAlias(TypeAlias {
                    alias: "Me".to_string(),
                    annotation: ast::Annotation::Constructor {
                        location: Span::new((), 379..393),
                        module: None,
                        name: "Option".to_string(),
                        arguments: vec![ast::Annotation::Constructor {
                            location: Span::new((), 386..392),
                            module: None,
                            name: "String".to_string(),
                            arguments: vec![],
                        },],
                    },
                    doc: None,
                    location: Span::new((), 365..393),
                    parameters: vec![],
                    public: true,
                    tipo: (),
                }),
                ast::UntypedDefinition::Fn(Function {
                    end_position: 466,
                    arguments: vec![ast::Arg {
                        arg_name: ast::ArgName::Named {
                            name: "a".to_string(),
                            location: Span::new((), 422..423),
                        },
                        location: Span::new((), 422..423),
                        annotation: None,
                        tipo: (),
                    },],
                    body: expr::UntypedExpr::BinOp {
                        location: Span::new((), 448..453),
                        name: ast::BinOp::AddInt,
                        left: Box::new(expr::UntypedExpr::Var {
                            location: Span::new((), 448..449),
                            name: "a".to_string(),
                        }),
                        right: Box::new(expr::UntypedExpr::Int {
                            location: Span::new((), 452..453),
                            value: "1".to_string(),
                        }),
                    },
                    doc: None,
                    location: Span::new((), 407..431),
                    name: "add_one".to_string(),
                    public: true,
                    return_annotation: Some(ast::Annotation::Constructor {
                        location: Span::new((), 428..431),
                        module: None,
                        name: "Int".to_string(),
                        arguments: vec![],
                    },),
                    return_type: (),
                }),
                ast::UntypedDefinition::Fn(Function {
                    end_position: 598,
                    arguments: vec![ast::Arg {
                        arg_name: ast::ArgName::NamedLabeled {
                            name: "a".to_string(),
                            label: "thing".to_string(),
                            location: Span::new((), 494..501),
                        },
                        location: Span::new((), 494..506),
                        annotation: Some(ast::Annotation::Constructor {
                            location: Span::new((), 503..506),
                            module: None,
                            name: "Int".to_string(),
                            arguments: vec![],
                        },),
                        tipo: (),
                    },],
                    body: expr::UntypedExpr::PipeLine {
                        expressions: vec1::vec1![
                            expr::UntypedExpr::BinOp {
                                location: Span::new((), 526..531),
                                name: ast::BinOp::AddInt,
                                left: Box::new(expr::UntypedExpr::Var {
                                    location: Span::new((), 526..527),
                                    name: "a".to_string(),
                                }),
                                right: Box::new(expr::UntypedExpr::Int {
                                    location: Span::new((), 530..531),
                                    value: "2".to_string(),
                                }),
                            },
                            expr::UntypedExpr::Var {
                                location: Span::new((), 551..558),
                                name: "add_one".to_string(),
                            },
                            expr::UntypedExpr::Var {
                                location: Span::new((), 578..585),
                                name: "add_one".to_string(),
                            },
                        ],
                    },
                    doc: None,
                    location: Span::new((), 481..507),
                    name: "thing".to_string(),
                    public: true,
                    return_annotation: None,
                    return_type: (),
                }),
                ast::UntypedDefinition::Fn(Function {
                    arguments: vec![ast::Arg {
                        arg_name: ast::ArgName::Named {
                            name: "a".to_string(),
                            location: Span::new((), 624..625),
                        },
                        location: Span::new((), 624..630),
                        annotation: Some(ast::Annotation::Constructor {
                            location: Span::new((), 627..630),
                            module: None,
                            name: "Int".to_string(),
                            arguments: vec![],
                        },),
                        tipo: (),
                    },],
                    body: expr::UntypedExpr::Sequence {
                        location: Span::new((), 648..818),
                        expressions: vec![
                            expr::UntypedExpr::Assignment {
                                location: Span::new((), 648..731),
                                value: Box::new(expr::UntypedExpr::PipeLine {
                                    expressions: vec1::vec1![
                                        expr::UntypedExpr::BinOp {
                                            location: Span::new((), 672..677),
                                            name: ast::BinOp::AddInt,
                                            left: Box::new(expr::UntypedExpr::Var {
                                                location: Span::new((), 672..673),
                                                name: "a".to_string(),
                                            }),
                                            right: Box::new(expr::UntypedExpr::Int {
                                                location: Span::new((), 676..677),
                                                value: "2".to_string(),
                                            }),
                                        },
                                        expr::UntypedExpr::Var {
                                            location: Span::new((), 697..704),
                                            name: "add_one".to_string(),
                                        },
                                        expr::UntypedExpr::Var {
                                            location: Span::new((), 724..731),
                                            name: "add_one".to_string(),
                                        },
                                    ],
                                }),
                                pattern: ast::Pattern::Var {
                                    location: Span::new((), 652..653),
                                    name: "x".to_string(),
                                },
                                kind: ast::AssignmentKind::Let,
                                annotation: None,
                            },
                            expr::UntypedExpr::Assignment {
                                location: Span::new((), 747..770),
                                value: Box::new(expr::UntypedExpr::List {
                                    location: Span::new((), 759..770),
                                    elements: vec![
                                        expr::UntypedExpr::Int {
                                            location: Span::new((), 761..762),
                                            value: "1".to_string(),
                                        },
                                        expr::UntypedExpr::Int {
                                            location: Span::new((), 764..765),
                                            value: "2".to_string(),
                                        },
                                        expr::UntypedExpr::Var {
                                            location: Span::new((), 767..768),
                                            name: "a".to_string(),
                                        },
                                    ],
                                    tail: None,
                                }),
                                pattern: ast::Pattern::Var {
                                    location: Span::new((), 751..756),
                                    name: "thing".to_string(),
                                },
                                kind: ast::AssignmentKind::Let,
                                annotation: None,
                            },
                            expr::UntypedExpr::Assignment {
                                location: Span::new((), 786..801),
                                value: Box::new(expr::UntypedExpr::Var {
                                    location: Span::new((), 796..801),
                                    name: "thing".to_string(),
                                }),
                                pattern: ast::Pattern::Var {
                                    location: Span::new((), 790..793),
                                    name: "idk".to_string(),
                                },
                                kind: ast::AssignmentKind::Let,
                                annotation: None,
                            },
                            expr::UntypedExpr::Var {
                                location: Span::new((), 817..818),
                                name: "y".to_string(),
                            },
                        ],
                    },
                    doc: None,
                    location: Span::new((), 613..631),
                    name: "wow".to_string(),
                    public: true,
                    return_annotation: None,
                    return_type: (),
                    end_position: 831,
                }),
                ast::UntypedDefinition::Fn(Function {
                    arguments: vec![ast::Arg {
                        arg_name: ast::ArgName::Named {
                            name: "a".to_string(),
                            location: Span::new((), 858..859),
                        },
                        location: Span::new((), 858..864),
                        annotation: Some(ast::Annotation::Constructor {
                            location: Span::new((), 861..864),
                            module: None,
                            name: "Int".to_string(),
                            arguments: vec![],
                        },),
                        tipo: (),
                    },],
                    body: expr::UntypedExpr::Sequence {
                        location: Span::new((), 881..1182),
                        expressions: vec![
                            expr::UntypedExpr::Assignment {
                                location: Span::new((), 881..955),
                                value: Box::new(expr::UntypedExpr::Sequence {
                                    location: Span::new((), 907..939),
                                    expressions: vec![
                                        expr::UntypedExpr::Assignment {
                                            location: Span::new((), 907..916),
                                            value: Box::new(expr::UntypedExpr::Int {
                                                location: Span::new((), 915..916),
                                                value: "4".to_string(),
                                            }),
                                            pattern: ast::Pattern::Var {
                                                location: Span::new((), 911..912),
                                                name: "x".to_string(),
                                            },
                                            kind: ast::AssignmentKind::Let,
                                            annotation: None,
                                        },
                                        expr::UntypedExpr::BinOp {
                                            location: Span::new((), 934..939),
                                            name: ast::BinOp::AddInt,
                                            left: Box::new(expr::UntypedExpr::Var {
                                                location: Span::new((), 934..935),
                                                name: "x".to_string(),
                                            }),
                                            right: Box::new(expr::UntypedExpr::Int {
                                                location: Span::new((), 938..939),
                                                value: "5".to_string(),
                                            }),
                                        },
                                    ],
                                }),
                                pattern: ast::Pattern::Var {
                                    location: Span::new((), 885..886),
                                    name: "b".to_string(),
                                },
                                kind: ast::AssignmentKind::Let,
                                annotation: None,
                            },
                            expr::UntypedExpr::When {
                                location: Span::new((), 971..1182),
                                subjects: vec![
                                    expr::UntypedExpr::Var {
                                        location: Span::new((), 976..977),
                                        name: "a".to_string(),
                                    },
                                    expr::UntypedExpr::Var {
                                        location: Span::new((), 979..980),
                                        name: "b".to_string(),
                                    },
                                ],
                                clauses: vec![
                                    ast::Clause {
                                        location: Span::new((), 1002..1011),
                                        pattern: vec![
                                            ast::Pattern::Int {
                                                location: Span::new((), 1002..1003),
                                                value: "1".to_string(),
                                            },
                                            ast::Pattern::Int {
                                                location: Span::new((), 1005..1006),
                                                value: "2".to_string(),
                                            },
                                        ],
                                        alternative_patterns: vec![],
                                        guard: None,
                                        then: expr::UntypedExpr::Int {
                                            location: Span::new((), 1010..1011),
                                            value: "3".to_string(),
                                        },
                                    },
                                    ast::Clause {
                                        location: Span::new((), 1028..1120),
                                        pattern: vec![ast::Pattern::Int {
                                            location: Span::new((), 1028..1029),
                                            value: "1".to_string(),
                                        },],
                                        alternative_patterns: vec![vec![
                                            ast::Pattern::Int {
                                                location: Span::new((), 1032..1033),
                                                value: "4".to_string(),
                                            },
                                            ast::Pattern::Int {
                                                location: Span::new((), 1035..1036),
                                                value: "5".to_string(),
                                            },
                                        ],],
                                        guard: None,
                                        then: expr::UntypedExpr::Sequence {
                                            location: Span::new((), 1060..1102),
                                            expressions: vec![
                                                expr::UntypedExpr::Assignment {
                                                    location: Span::new((), 1060..1075),
                                                    value: Box::new(expr::UntypedExpr::Int {
                                                        location: Span::new((), 1074..1075),
                                                        value: "5".to_string(),
                                                    }),
                                                    pattern: ast::Pattern::Var {
                                                        location: Span::new((), 1064..1071),
                                                        name: "amazing".to_string(),
                                                    },
                                                    kind: ast::AssignmentKind::Let,
                                                    annotation: None,
                                                },
                                                expr::UntypedExpr::Var {
                                                    location: Span::new((), 1095..1102),
                                                    name: "amazing".to_string(),
                                                },
                                            ],
                                        },
                                    },
                                    ast::Clause {
                                        location: Span::new((), 1137..1143),
                                        pattern: vec![ast::Pattern::Int {
                                            location: Span::new((), 1137..1138),
                                            value: "3".to_string(),
                                        },],
                                        alternative_patterns: vec![],
                                        guard: None,
                                        then: expr::UntypedExpr::Int {
                                            location: Span::new((), 1142..1143),
                                            value: "9".to_string(),
                                        },
                                    },
                                    ast::Clause {
                                        location: Span::new((), 1160..1166),
                                        pattern: vec![ast::Pattern::Discard {
                                            name: "_".to_string(),
                                            location: Span::new((), 1160..1161),
                                        },],
                                        alternative_patterns: vec![],
                                        guard: None,
                                        then: expr::UntypedExpr::Int {
                                            location: Span::new((), 1165..1166),
                                            value: "4".to_string(),
                                        },
                                    },
                                ],
                            },
                        ],
                    },
                    doc: None,
                    location: Span::new((), 846..865),
                    name: "wow2".to_string(),
                    public: true,
                    return_annotation: None,
                    return_type: (),
                    end_position: 1195,
                }),
                ast::UntypedDefinition::Fn(Function {
                    arguments: vec![],
                    body: expr::UntypedExpr::Sequence {
                        location: Span::new((), 1249..1321),
                        expressions: vec![
                            expr::UntypedExpr::Assignment {
                                location: Span::new((), 1249..1291),
                                value: Box::new(expr::UntypedExpr::Fn {
                                    location: Span::new((), 1263..1291),
                                    is_capture: false,
                                    arguments: vec![ast::Arg {
                                        arg_name: ast::ArgName::Named {
                                            name: "a".to_string(),
                                            location: Span::new((), 1267..1268),
                                        },
                                        location: Span::new((), 1267..1273),
                                        annotation: Some(ast::Annotation::Constructor {
                                            location: Span::new((), 1270..1273),
                                            module: None,
                                            name: "Int".to_string(),
                                            arguments: vec![],
                                        },),
                                        tipo: (),
                                    },],
                                    body: Box::new(expr::UntypedExpr::BinOp {
                                        location: Span::new((), 1284..1289),
                                        name: ast::BinOp::AddInt,
                                        left: Box::new(expr::UntypedExpr::Var {
                                            location: Span::new((), 1284..1285),
                                            name: "a".to_string(),
                                        }),
                                        right: Box::new(expr::UntypedExpr::Int {
                                            location: Span::new((), 1288..1289),
                                            value: "1".to_string(),
                                        }),
                                    }),
                                    return_annotation: Some(ast::Annotation::Constructor {
                                        location: Span::new((), 1278..1281),
                                        module: None,
                                        name: "Int".to_string(),
                                        arguments: vec![],
                                    },),
                                }),
                                pattern: ast::Pattern::Var {
                                    location: Span::new((), 1253..1260),
                                    name: "add_one".to_string(),
                                },
                                kind: ast::AssignmentKind::Let,
                                annotation: None,
                            },
                            expr::UntypedExpr::PipeLine {
                                expressions: vec1::vec1![
                                    expr::UntypedExpr::Int {
                                        location: Span::new((), 1309..1310),
                                        value: "2".to_string(),
                                    },
                                    expr::UntypedExpr::Var {
                                        location: Span::new((), 1314..1321),
                                        name: "add_one".to_string(),
                                    },
                                ],
                            },
                        ],
                    },
                    doc: None,
                    location: Span::new((), 1210..1230),
                    name: "such".to_string(),
                    public: true,
                    return_annotation: Some(ast::Annotation::Constructor {
                        location: Span::new((), 1227..1230),
                        module: None,
                        name: "Int".to_string(),
                        arguments: vec![],
                    },),
                    return_type: (),
                    end_position: 1334,
                }),
                ast::UntypedDefinition::Fn(Function {
                    arguments: vec![],
                    body: expr::UntypedExpr::Todo {
                        kind: ast::TodoKind::EmptyFunction,
                        location: Span::new((), 1349..1360),
                        label: None,
                    },
                    doc: None,
                    location: Span::new((), 1349..1357),
                    name: "run".to_string(),
                    public: false,
                    return_annotation: None,
                    return_type: (),
                    end_position: 1359,
                }),
                ast::UntypedDefinition::Fn(Function {
                    arguments: vec![ast::Arg {
                        arg_name: ast::ArgName::Named {
                            name: "user".to_string(),
                            location: Span::new((), 1382..1386),
                        },
                        location: Span::new((), 1382..1392),
                        annotation: Some(ast::Annotation::Constructor {
                            location: Span::new((), 1388..1392),
                            module: None,
                            name: "User".to_string(),
                            arguments: vec![],
                        },),
                        tipo: (),
                    },],
                    body: expr::UntypedExpr::FieldAccess {
                        location: Span::new((), 1412..1421),
                        label: "name".to_string(),
                        container: Box::new(expr::UntypedExpr::Var {
                            location: Span::new((), 1412..1416),
                            name: "user".to_string(),
                        }),
                    },
                    doc: None,
                    location: Span::new((), 1374..1393),
                    name: "name".to_string(),
                    public: false,
                    return_annotation: None,
                    return_type: (),
                    end_position: 1434,
                }),
                ast::UntypedDefinition::Fn(Function {
                    arguments: vec![],
                    body: expr::UntypedExpr::Sequence {
                        location: Span::new((), 1478..1599),
                        expressions: vec![
                            expr::UntypedExpr::Assignment {
                                location: Span::new((), 1478..1496),
                                value: Box::new(expr::UntypedExpr::Call {
                                    arguments: vec![ast::CallArg {
                                        label: None,
                                        location: Span::new((), 1494..1495),
                                        value: expr::UntypedExpr::Int {
                                            location: Span::new((), 1494..1495),
                                            value: "3".to_string(),
                                        },
                                    }],
                                    fun: Box::new(expr::UntypedExpr::Var {
                                        location: Span::new((), 1486..1493),
                                        name: "add_one".to_string(),
                                    }),
                                    location: Span::new((), 1493..1496),
                                }),
                                pattern: ast::Pattern::Var {
                                    location: Span::new((), 1482..1483),
                                    name: "x".to_string(),
                                },
                                kind: ast::AssignmentKind::Let,
                                annotation: None,
                            },
                            expr::UntypedExpr::Assignment {
                                location: Span::new((), 1514..1559),
                                value: Box::new(expr::UntypedExpr::Fn {
                                    location: Span::new((), 1538..1559),
                                    is_capture: true,
                                    arguments: vec![ast::Arg {
                                        arg_name: ast::ArgName::Named {
                                            name: "_capture__0".to_string(),
                                            location: Span::new((), 0..0),
                                        },
                                        location: Span::new((), 0..0),
                                        annotation: None,
                                        tipo: (),
                                    },],
                                    body: Box::new(expr::UntypedExpr::Call {
                                        arguments: vec![
                                            ast::CallArg {
                                                label: None,
                                                location: Span::new((), 1539..1540),
                                                value: expr::UntypedExpr::Var {
                                                    location: Span::new((), 1539..1540),
                                                    name: "_capture__0".to_string(),
                                                },
                                            },
                                            ast::CallArg {
                                                label: None,
                                                location: Span::new((), 1542..1558),
                                                value: expr::UntypedExpr::Fn {
                                                    location: Span::new((), 1542..1558),
                                                    is_capture: false,
                                                    arguments: vec![ast::Arg {
                                                        arg_name: ast::ArgName::Named {
                                                            name: "y".to_string(),
                                                            location: Span::new((), 1546..1547),
                                                        },
                                                        location: Span::new((), 1546..1547),
                                                        annotation: None,
                                                        tipo: (),
                                                    },],
                                                    body: Box::new(expr::UntypedExpr::BinOp {
                                                        location: Span::new((), 1551..1556),
                                                        name: ast::BinOp::AddInt,
                                                        left: Box::new(expr::UntypedExpr::Var {
                                                            location: Span::new((), 1551..1552),
                                                            name: "x".to_string(),
                                                        }),
                                                        right: Box::new(expr::UntypedExpr::Var {
                                                            location: Span::new((), 1555..1556),
                                                            name: "y".to_string(),
                                                        }),
                                                    }),
                                                    return_annotation: None,
                                                },
                                            },
                                        ],
                                        fun: Box::new(expr::UntypedExpr::FieldAccess {
                                            location: Span::new((), 1530..1538),
                                            label: "map".to_string(),
                                            container: Box::new(expr::UntypedExpr::Var {
                                                location: Span::new((), 1530..1534),
                                                name: "list".to_string(),
                                            }),
                                        }),
                                        location: Span::new((), 1538..1559),
                                    }),
                                    return_annotation: None,
                                }),
                                pattern: ast::Pattern::Var {
                                    location: Span::new((), 1518..1527),
                                    name: "map_add_x".to_string(),
                                },
                                kind: ast::AssignmentKind::Let,
                                annotation: None,
                            },
                            expr::UntypedExpr::Call {
                                arguments: vec![ast::CallArg {
                                    label: None,
                                    location: Span::new((), 1587..1598),
                                    value: expr::UntypedExpr::List {
                                        location: Span::new((), 1587..1598),
                                        elements: vec![
                                            expr::UntypedExpr::Int {
                                                location: Span::new((), 1589..1590),
                                                value: "1".to_string(),
                                            },
                                            expr::UntypedExpr::Int {
                                                location: Span::new((), 1592..1593),
                                                value: "2".to_string(),
                                            },
                                            expr::UntypedExpr::Int {
                                                location: Span::new((), 1595..1596),
                                                value: "3".to_string(),
                                            },
                                        ],
                                        tail: None,
                                    },
                                }],
                                fun: Box::new(expr::UntypedExpr::Var {
                                    location: Span::new((), 1577..1586),
                                    name: "map_add_x".to_string(),
                                }),
                                location: Span::new((), 1586..1599),
                            },
                        ],
                    },
                    doc: None,
                    location: Span::new((), 1449..1459),
                    name: "calls".to_string(),
                    public: false,
                    return_annotation: None,
                    return_type: (),
                    end_position: 1612,
                }),
                ast::UntypedDefinition::Fn(Function {
                    arguments: vec![
                        ast::Arg {
                            arg_name: ast::ArgName::Named {
                                name: "user".to_string(),
                                location: Span::new((), 1642..1646),
                            },
                            location: Span::new((), 1642..1652),
                            annotation: Some(ast::Annotation::Constructor {
                                location: Span::new((), 1648..1652),
                                module: None,
                                name: "User".to_string(),
                                arguments: vec![],
                            },),
                            tipo: (),
                        },
                        ast::Arg {
                            arg_name: ast::ArgName::Named {
                                name: "name".to_string(),
                                location: Span::new((), 1654..1658),
                            },
                            location: Span::new((), 1654..1666),
                            annotation: Some(ast::Annotation::Constructor {
                                location: Span::new((), 1660..1666),
                                module: None,
                                name: "String".to_string(),
                                arguments: vec![],
                            },),
                            tipo: (),
                        },
                    ],
                    body: expr::UntypedExpr::RecordUpdate {
                        location: Span::new((), 1694..1725),
                        constructor: Box::new(expr::UntypedExpr::Var {
                            location: Span::new((), 1694..1698),
                            name: "User".to_string(),
                        }),
                        spread: ast::RecordUpdateSpread {
                            base: Box::new(expr::UntypedExpr::Var {
                                location: Span::new((), 1703..1707),
                                name: "user".to_string(),
                            }),
                            location: Span::new((), 1699..1725),
                        },
                        arguments: vec![ast::UntypedRecordUpdateArg {
                            label: "name".to_string(),
                            location: Span::new((), 1709..1722),
                            value: expr::UntypedExpr::String {
                                location: Span::new((), 1715..1722),
                                value: "Aiken".to_string(),
                            },
                        },],
                    },
                    doc: None,
                    location: Span::new((), 1627..1675),
                    name: "update_name".to_string(),
                    public: false,
                    return_annotation: Some(ast::Annotation::Constructor {
                        location: Span::new((), 1671..1675),
                        module: None,
                        name: "User".to_string(),
                        arguments: vec![],
                    },),
                    return_type: (),
                    end_position: 1738,
                }),
                ast::UntypedDefinition::Fn(Function {
                    arguments: vec![],
                    body: expr::UntypedExpr::If {
                        location: Span::new((), 1780..1993),
                        branches: vec1::vec1![
                            ast::IfBranch {
                                condition: expr::UntypedExpr::Var {
                                    location: Span::new((), 1783..1787),
                                    name: "True".to_string(),
                                },
                                body: expr::UntypedExpr::BinOp {
                                    location: Span::new((), 1810..1815),
                                    name: ast::BinOp::AddInt,
                                    left: Box::new(expr::UntypedExpr::Int {
                                        location: Span::new((), 1810..1811),
                                        value: "1".to_string(),
                                    }),
                                    right: Box::new(expr::UntypedExpr::Int {
                                        location: Span::new((), 1814..1815),
                                        value: "1".to_string(),
                                    }),
                                },
                                location: Span::new((), 1783..1833),
                            },
                            ast::IfBranch {
                                condition: expr::UntypedExpr::BinOp {
                                    location: Span::new((), 1842..1847),
                                    name: ast::BinOp::LtInt,
                                    left: Box::new(expr::UntypedExpr::Var {
                                        location: Span::new((), 1842..1843),
                                        name: "a".to_string(),
                                    }),
                                    right: Box::new(expr::UntypedExpr::Int {
                                        location: Span::new((), 1846..1847),
                                        value: "4".to_string(),
                                    }),
                                },
                                body: expr::UntypedExpr::Int {
                                    location: Span::new((), 1870..1871),
                                    value: "5".to_string(),
                                },
                                location: Span::new((), 1842..1889),
                            },
                            ast::IfBranch {
                                condition: expr::UntypedExpr::BinOp {
                                    location: Span::new((), 1898..1904),
                                    name: ast::BinOp::Or,
                                    left: Box::new(expr::UntypedExpr::Var {
                                        location: Span::new((), 1898..1899),
                                        name: "a".to_string(),
                                    }),
                                    right: Box::new(expr::UntypedExpr::Var {
                                        location: Span::new((), 1903..1904),
                                        name: "b".to_string(),
                                    }),
                                },
                                body: expr::UntypedExpr::Int {
                                    location: Span::new((), 1927..1928),
                                    value: "6".to_string(),
                                },
                                location: Span::new((), 1898..1946),
                            },
                        ],
                        final_else: Box::new(expr::UntypedExpr::Int {
                            location: Span::new((), 1974..1975),
                            value: "3".to_string(),
                        }),
                    },
                    doc: None,
                    location: Span::new((), 1753..1761),
                    name: "ifs".to_string(),
                    public: false,
                    return_annotation: None,
                    return_type: (),
                    end_position: 2006,
                }),
            ]
        },
    );
}
