use chumsky::prelude::*;
use vec1::Vec1;

use crate::{
    ast::{self, BinOp, TodoKind},
    error::ParseError,
    expr,
    token::Token,
};

pub fn module_parser(
    kind: ast::ModuleKind,
) -> impl Parser<Token, ast::UntypedModule, Error = ParseError> {
    choice((
        import_parser(),
        data_parser(),
        type_alias_parser(),
        fn_parser(),
    ))
    .repeated()
    .then_ignore(end())
    .map(move |definitions| ast::UntypedModule {
        kind,
        definitions,
        docs: vec![],
        name: vec![],
        type_info: (),
    })
}

pub fn import_parser() -> impl Parser<Token, ast::UntypedDefinition, Error = ParseError> {
    let unqualified_import = choice((
        select! {Token::Name { name } => name}.then(
            just(Token::As)
                .ignore_then(select! {Token::Name { name } => name})
                .or_not(),
        ),
        select! {Token::UpName { name } => name}.then(
            just(Token::As)
                .ignore_then(select! {Token::UpName { name } => name})
                .or_not(),
        ),
    ))
    .map_with_span(|(name, as_name), span| ast::UnqualifiedImport {
        name,
        location: span,
        as_name,
        layer: Default::default(),
    });

    let unqualified_imports = just(Token::Dot)
        .ignore_then(
            unqualified_import
                .separated_by(just(Token::Comma))
                .delimited_by(just(Token::LeftBrace), just(Token::RightBrace)),
        )
        .or_not();

    let as_name = just(Token::As)
        .ignore_then(select! {Token::Name { name } => name})
        .or_not();

    let module_path = select! {Token::Name { name } => name}
        .separated_by(just(Token::Slash))
        .then(unqualified_imports)
        .then(as_name);

    just(Token::Use).ignore_then(module_path).map_with_span(
        |((module, unqualified), as_name), span| ast::UntypedDefinition::Use {
            module,
            as_name,
            unqualified: unqualified.unwrap_or_default(),
            package: (),
            location: span,
        },
    )
}

pub fn data_parser() -> impl Parser<Token, ast::UntypedDefinition, Error = ParseError> {
    let unlabeled_constructor_type_args = type_parser()
        .map_with_span(|annotation, span| ast::RecordConstructorArg {
            label: None,
            annotation,
            tipo: (),
            doc: None,
            location: span,
        })
        .separated_by(just(Token::Comma))
        .delimited_by(just(Token::LeftParen), just(Token::RightParen));

    let constructors = select! {Token::UpName { name } => name}
        .then(
            choice((
                labeled_constructor_type_args(),
                unlabeled_constructor_type_args,
            ))
            .or_not(),
        )
        .map_with_span(|(name, arguments), span| ast::RecordConstructor {
            location: span,
            arguments: arguments.unwrap_or_default(),
            name,
            documentation: None,
            sugar: false,
        })
        .repeated()
        .delimited_by(just(Token::LeftBrace), just(Token::RightBrace));

    let record_sugar = labeled_constructor_type_args().map_with_span(|arguments, span| {
        vec![ast::RecordConstructor {
            location: span,
            arguments,
            documentation: None,
            name: String::from("_replace"),
            sugar: true,
        }]
    });

    pub_parser()
        .then(just(Token::Opaque).ignored().or_not())
        .or_not()
        .then(type_name_with_args())
        .then(choice((constructors, record_sugar)))
        .map_with_span(|((pub_opaque, (name, parameters)), constructors), span| {
            ast::UntypedDefinition::DataType {
                location: span,
                constructors: constructors
                    .into_iter()
                    .map(|mut constructor| {
                        if constructor.sugar {
                            constructor.name = name.clone();
                        }

                        constructor
                    })
                    .collect(),
                doc: None,
                name,
                opaque: pub_opaque
                    .map(|(_, opt_opaque)| opt_opaque.is_some())
                    .unwrap_or(false),
                parameters: parameters.unwrap_or_default(),
                public: pub_opaque.is_some(),
                typed_parameters: vec![],
            }
        })
}

pub fn type_alias_parser() -> impl Parser<Token, ast::UntypedDefinition, Error = ParseError> {
    pub_parser()
        .or_not()
        .then(type_name_with_args())
        .then_ignore(just(Token::Equal))
        .then(type_parser())
        .map_with_span(|((opt_pub, (alias, parameters)), annotation), span| {
            ast::UntypedDefinition::TypeAlias {
                alias,
                annotation,
                doc: None,
                location: span,
                parameters: parameters.unwrap_or_default(),
                public: opt_pub.is_some(),
                tipo: (),
            }
        })
}

pub fn fn_parser() -> impl Parser<Token, ast::UntypedDefinition, Error = ParseError> {
    pub_parser()
        .or_not()
        .then_ignore(just(Token::Fn))
        .then(select! {Token::Name {name} => name})
        .then(
            fn_param_parser()
                .separated_by(just(Token::Comma))
                .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
        )
        .then(just(Token::RArrow).ignore_then(type_parser()).or_not())
        .then(expr_seq_parser().delimited_by(just(Token::LeftBrace), just(Token::RightBrace)))
        .map_with_span(
            |((((opt_pub, name), arguments), return_annotation), body), span| {
                ast::UntypedDefinition::Fn {
                    arguments,
                    body,
                    doc: None,
                    location: span,
                    name,
                    public: opt_pub.is_some(),
                    return_annotation,
                    return_type: (),
                }
            },
        )
}

pub fn fn_param_parser() -> impl Parser<Token, ast::UntypedArg, Error = ParseError> {
    choice((
        select! {Token::Name {name} => name}
            .then(select! {Token::DiscardName {name} => name})
            .map_with_span(|(label, name), span| ast::ArgName::LabeledDiscard {
                label,
                name,
                location: span,
            }),
        select! {Token::DiscardName {name} => name}.map_with_span(|name, span| {
            ast::ArgName::Discard {
                name,
                location: span,
            }
        }),
        select! {Token::Name {name} => name}
            .then(select! {Token::Name {name} => name})
            .map_with_span(|(label, name), span| ast::ArgName::NamedLabeled {
                label,
                name,
                location: span,
            }),
        select! {Token::Name {name} => name}.map_with_span(|name, span| ast::ArgName::Named {
            name,
            location: span,
        }),
    ))
    .then(just(Token::Colon).ignore_then(type_parser()).or_not())
    .map_with_span(|(arg_name, annotation), span| ast::Arg {
        location: span,
        annotation,
        tipo: (),
        arg_name,
    })
}

pub fn expr_seq_parser() -> impl Parser<Token, expr::UntypedExpr, Error = ParseError> {
    recursive(|r| {
        choice((
            just(Token::Try)
                .ignore_then(pattern_parser())
                .then(just(Token::Colon).ignore_then(type_parser()).or_not())
                .then_ignore(just(Token::Equal))
                .then(expr_parser())
                .then(r.clone())
                .map_with_span(|(((pattern, annotation), value), then_), span| {
                    expr::UntypedExpr::Try {
                        location: span,
                        value: Box::new(value),
                        pattern,
                        then: Box::new(then_),
                        annotation,
                    }
                }),
            expr_parser()
                .then(r.repeated())
                .map_with_span(|(expr, exprs), _span| {
                    exprs
                        .into_iter()
                        .fold(expr, |acc, elem| acc.append_in_sequence(elem))
                }),
        ))
    })
}

pub fn expr_parser() -> impl Parser<Token, expr::UntypedExpr, Error = ParseError> {
    recursive(|_r| {
        // Product
        let op = choice((
            just(Token::Star).to(BinOp::MultInt),
            just(Token::Slash).to(BinOp::DivInt),
            just(Token::Percent).to(BinOp::ModInt),
        ));

        let product = expr_unit_parser()
            .then(op.then(expr_unit_parser()).repeated())
            .foldl(|a, (op, b)| expr::UntypedExpr::BinOp {
                location: a.location().union(b.location()),
                name: op,
                left: Box::new(a),
                right: Box::new(b),
            })
            .boxed();

        // Sum
        let op = choice((
            just(Token::Plus).to(BinOp::AddInt),
            just(Token::Minus).to(BinOp::SubInt),
        ));

        let sum = product
            .clone()
            .then(op.then(product).repeated())
            .foldl(|a, (op, b)| expr::UntypedExpr::BinOp {
                location: a.location().union(b.location()),
                name: op,
                left: Box::new(a),
                right: Box::new(b),
            })
            .boxed();

        // Logical
        let op = choice((
            just(Token::EqualEqual).to(BinOp::Eq),
            just(Token::NotEqual).to(BinOp::NotEq),
            just(Token::Less).to(BinOp::LtInt),
            just(Token::Greater).to(BinOp::GtInt),
            just(Token::LessEqual).to(BinOp::LtEqInt),
            just(Token::GreaterEqual).to(BinOp::GtEqInt),
        ));

        let comparison = sum
            .clone()
            .then(op.then(sum).repeated())
            .foldl(|a, (op, b)| expr::UntypedExpr::BinOp {
                location: a.location().union(b.location()),
                name: op,
                left: Box::new(a),
                right: Box::new(b),
            })
            .boxed();

        let op = choice((
            just(Token::AmperAmper).to(BinOp::And),
            just(Token::VbarVbar).to(BinOp::Or),
        ));

        let logical = comparison
            .clone()
            .then(op.then(comparison).repeated())
            .foldl(|a, (op, b)| expr::UntypedExpr::BinOp {
                location: a.location().union(b.location()),
                name: op,
                left: Box::new(a),
                right: Box::new(b),
            })
            .boxed();

        // Pipeline
        logical
            .clone()
            .then(just(Token::Pipe).ignore_then(logical).repeated())
            .foldl(|l, r| {
                let expressions = if let expr::UntypedExpr::PipeLine { mut expressions } = l {
                    expressions.push(r);
                    expressions
                } else {
                    let mut expressions = Vec1::new(l);
                    expressions.push(r);
                    expressions
                };
                expr::UntypedExpr::PipeLine { expressions }
            })
    })
}

pub fn expr_unit_parser() -> impl Parser<Token, expr::UntypedExpr, Error = ParseError> {
    choice((
        select! {Token::String {value} => value}.map_with_span(|value, span| {
            expr::UntypedExpr::String {
                location: span,
                value,
            }
        }),
        select! { Token::Int {value} => value}.map_with_span(|value, span| {
            expr::UntypedExpr::Int {
                location: span,
                value,
            }
        }),
        select! {
            Token::Name { name } => name,
            Token::UpName { name } => name,
        }
        .map_with_span(|name, span| expr::UntypedExpr::Var {
            location: span,
            name,
        }),
        just(Token::Todo)
            .ignore_then(
                select! {Token::String {value} => value}
                    .delimited_by(just(Token::LeftParen), just(Token::RightParen))
                    .or_not(),
            )
            .map_with_span(|label, span| expr::UntypedExpr::Todo {
                kind: TodoKind::Keyword,
                location: span,
                label,
            }),
    ))
}

pub fn type_parser() -> impl Parser<Token, ast::Annotation, Error = ParseError> {
    recursive(|r| {
        choice((
            select! {Token::DiscardName { name } => name}.map_with_span(|name, span| {
                ast::Annotation::Hole {
                    location: span,
                    name,
                }
            }),
            just(Token::Fn)
                .ignore_then(
                    r.clone()
                        .separated_by(just(Token::Comma))
                        .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
                )
                .then_ignore(just(Token::RArrow))
                .then(r.clone())
                .map_with_span(|(arguments, ret), span| ast::Annotation::Fn {
                    location: span,
                    arguments,
                    ret: Box::new(ret),
                }),
            select! {Token::UpName { name } => name}
                .then(
                    r.clone()
                        .separated_by(just(Token::Comma))
                        .delimited_by(just(Token::LeftParen), just(Token::RightParen))
                        .or_not(),
                )
                .map_with_span(|(name, arguments), span| ast::Annotation::Constructor {
                    location: span,
                    module: None,
                    name,
                    arguments: arguments.unwrap_or_default(),
                }),
            select! {Token::Name { name } => name}
                .then(
                    just(Token::Dot)
                        .ignore_then(select! {Token::UpName {name} => name})
                        .then(
                            r.separated_by(just(Token::Comma))
                                .delimited_by(just(Token::LeftParen), just(Token::RightParen))
                                .or_not(),
                        )
                        .or_not(),
                )
                .map_with_span(|(mod_name, opt_dot), span| {
                    if let Some((name, arguments)) = opt_dot {
                        ast::Annotation::Constructor {
                            location: span,
                            module: Some(mod_name),
                            name,
                            arguments: arguments.unwrap_or_default(),
                        }
                    } else {
                        ast::Annotation::Var {
                            location: span,
                            name: mod_name,
                        }
                    }
                }),
        ))
    })
}

pub fn labeled_constructor_type_args(
) -> impl Parser<Token, Vec<ast::RecordConstructorArg<()>>, Error = ParseError> {
    select! {Token::Name {name} => name}
        .then_ignore(just(Token::Colon))
        .then(type_parser())
        .map_with_span(|(name, annotation), span| ast::RecordConstructorArg {
            label: Some(name),
            annotation,
            tipo: (),
            doc: None,
            location: span,
        })
        .separated_by(just(Token::Comma))
        .delimited_by(just(Token::LeftBrace), just(Token::RightBrace))
}

pub fn type_name_with_args() -> impl Parser<Token, (String, Option<Vec<String>>), Error = ParseError>
{
    just(Token::Type).ignore_then(
        select! {Token::UpName { name } => name}.then(
            select! {Token::Name { name } => name}
                .separated_by(just(Token::Comma))
                .delimited_by(just(Token::LeftParen), just(Token::RightParen))
                .or_not(),
        ),
    )
}

pub fn pub_parser() -> impl Parser<Token, (), Error = ParseError> {
    just(Token::Pub).ignored()
}

pub fn pattern_parser() -> impl Parser<Token, ast::UntypedPattern, Error = ParseError> {
    recursive(|r| {
        let constructor_pattern_arg_parser = choice((
            select! {Token::Name {name} => name}
                .then_ignore(just(Token::Colon))
                .then(r.clone())
                .map_with_span(|(name, pattern), span| ast::CallArg {
                    location: span,
                    label: Some(name),
                    value: pattern,
                }),
            r.map_with_span(|pattern, span| ast::CallArg {
                location: span,
                value: pattern,
                label: None,
            }),
        ));

        let constructor_pattern_args_parser = constructor_pattern_arg_parser
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .then(
                just(Token::DotDot)
                    .then_ignore(just(Token::Comma).or_not())
                    .ignored()
                    .or_not(),
            )
            .delimited_by(just(Token::LeftParen), just(Token::RightParen))
            .or_not()
            .map(|opt_args| {
                opt_args
                    .map(|(a, b)| (a, b.is_some()))
                    .unwrap_or_else(|| (vec![], false))
            });

        let constructor_pattern_parser =
            select! {Token::UpName { name } => name}.then(constructor_pattern_args_parser);

        choice((
            select! { Token::Name {name} => name }
                .then(
                    just(Token::Dot)
                        .ignore_then(constructor_pattern_parser.clone())
                        .or_not(),
                )
                .map_with_span(|(name, opt_pattern), span| {
                    if let Some((c_name, (arguments, with_spread))) = opt_pattern {
                        ast::UntypedPattern::Constructor {
                            location: span,
                            name: c_name,
                            arguments,
                            module: Some(name),
                            constructor: (),
                            with_spread,
                            tipo: (),
                        }
                    } else {
                        ast::UntypedPattern::Var {
                            location: span,
                            name,
                        }
                    }
                }),
            constructor_pattern_parser.map_with_span(|(name, (arguments, with_spread)), span| {
                ast::UntypedPattern::Constructor {
                    location: span,
                    name,
                    arguments,
                    module: None,
                    constructor: (),
                    with_spread,
                    tipo: (),
                }
            }),
            select! {Token::DiscardName {name} => name}.map_with_span(|name, span| {
                ast::UntypedPattern::Discard {
                    name,
                    location: span,
                }
            }),
            select! {Token::String {value} => value}.map_with_span(|value, span| {
                ast::UntypedPattern::String {
                    location: span,
                    value,
                }
            }),
            select! {Token::Int {value} => value}.map_with_span(|value, span| {
                ast::UntypedPattern::Int {
                    location: span,
                    value,
                }
            }),
        ))
        .then(
            just(Token::As)
                .ignore_then(select! { Token::Name {name} => name})
                .or_not(),
        )
        .map_with_span(|(pattern, opt_as), span| {
            if let Some(name) = opt_as {
                ast::UntypedPattern::Assign {
                    name,
                    location: span,
                    pattern: Box::new(pattern),
                }
            } else {
                pattern
            }
        })
    })
}

#[cfg(test)]
mod tests {
    use chumsky::prelude::*;
    use pretty_assertions::assert_eq;

    use crate::{
        ast::{self, Span, SrcId},
        expr, lexer,
        parser::module_parser,
    };

    #[test]
    fn simple() {
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

            pub fn add_one(a) {
              a + 1
            }

            pub fn thing(thing a: Int) {
                a + 2
                |> add_one
                |> add_one
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
                            location: Span::new(SrcId::empty(), 441..446),
                            name: ast::BinOp::AddInt,
                            left: Box::new(expr::UntypedExpr::Var {
                                location: Span::new(SrcId::empty(), 441..442),
                                name: "a".to_string(),
                            }),
                            right: Box::new(expr::UntypedExpr::Int {
                                location: Span::new(SrcId::empty(), 445..446),
                                value: "1".to_string(),
                            }),
                        },
                        doc: None,
                        location: Span::new(SrcId::empty(), 407..460),
                        name: "add_one".to_string(),
                        public: true,
                        return_annotation: None,
                        return_type: (),
                    },
                    ast::UntypedDefinition::Fn {
                        arguments: vec![ast::Arg {
                            arg_name: ast::ArgName::NamedLabeled {
                                name: "a".to_string(),
                                label: "thing".to_string(),
                                location: Span::new(SrcId::empty(), 487..494),
                            },
                            location: Span::new(SrcId::empty(), 487..499),
                            annotation: Some(ast::Annotation::Constructor {
                                location: Span::new(SrcId::empty(), 496..499),
                                module: None,
                                name: "Int".to_string(),
                                arguments: vec![],
                            },),
                            tipo: (),
                        },],
                        body: expr::UntypedExpr::PipeLine {
                            expressions: vec1::vec1![
                                expr::UntypedExpr::BinOp {
                                    location: Span::new(SrcId::empty(), 519..524),
                                    name: ast::BinOp::AddInt,
                                    left: Box::new(expr::UntypedExpr::Var {
                                        location: Span::new(SrcId::empty(), 519..520),
                                        name: "a".to_string(),
                                    }),
                                    right: Box::new(expr::UntypedExpr::Int {
                                        location: Span::new(SrcId::empty(), 523..524),
                                        value: "2".to_string(),
                                    }),
                                },
                                expr::UntypedExpr::Var {
                                    location: Span::new(SrcId::empty(), 544..551),
                                    name: "add_one".to_string(),
                                },
                                expr::UntypedExpr::Var {
                                    location: Span::new(SrcId::empty(), 571..578),
                                    name: "add_one".to_string(),
                                },
                            ],
                        },
                        doc: None,
                        location: Span::new(SrcId::empty(), 474..592),
                        name: "thing".to_string(),
                        public: true,
                        return_annotation: None,
                        return_type: (),
                    },
                ]
            },
        );
    }
}
