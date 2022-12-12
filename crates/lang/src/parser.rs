use chumsky::prelude::*;
use vec1::Vec1;

pub mod error;
pub mod extra;
pub mod lexer;
pub mod token;

use crate::{
    ast::{self, BinOp, Span, TodoKind, UntypedDefinition, CAPTURE_VARIABLE},
    expr,
};

use error::ParseError;
use extra::ModuleExtra;
use token::Token;

pub fn module(
    src: &str,
    kind: ast::ModuleKind,
) -> Result<(ast::UntypedModule, ModuleExtra), Vec<ParseError>> {
    let len = src.chars().count();

    let span = |i| Span::new((), i..i + 1);

    let tokens = lexer::lexer().parse(chumsky::Stream::from_iter(
        span(len),
        src.chars().enumerate().map(|(i, c)| (c, span(i))),
    ))?;

    let mut extra = ModuleExtra::new();

    let tokens = tokens.into_iter().filter(|(token, span)| match token {
        Token::ModuleComment => {
            extra.module_comments.push(*span);

            false
        }
        Token::DocComment => {
            extra.doc_comments.push(*span);

            false
        }
        Token::Comment => {
            extra.comments.push(*span);

            false
        }
        Token::EmptyLine => {
            println!("{:?}", span);
            extra.empty_lines.push(span.start);

            false
        }
        _ => true,
    });

    let definitions = module_parser().parse(chumsky::Stream::from_iter(span(len), tokens))?;

    let module = ast::UntypedModule {
        kind,
        definitions,
        docs: vec![],
        name: "".to_string(),
        type_info: (),
    };

    Ok((module, extra))
}

fn module_parser() -> impl Parser<Token, Vec<UntypedDefinition>, Error = ParseError> {
    choice((
        import_parser(),
        data_parser(),
        type_alias_parser(),
        fn_parser(),
        test_parser(),
        constant_parser(),
    ))
    .repeated()
    .then_ignore(end())
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
                .allow_trailing()
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
        |((module, unqualified), as_name), span| {
            ast::UntypedDefinition::Use(ast::Use {
                module,
                as_name,
                unqualified: unqualified.unwrap_or_default(),
                package: (),
                location: span,
            })
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
        .allow_trailing()
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
            ast::UntypedDefinition::DataType(ast::DataType {
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
            })
        })
}

pub fn type_alias_parser() -> impl Parser<Token, ast::UntypedDefinition, Error = ParseError> {
    pub_parser()
        .or_not()
        .then(type_name_with_args())
        .then_ignore(just(Token::Equal))
        .then(type_parser())
        .map_with_span(|((opt_pub, (alias, parameters)), annotation), span| {
            ast::UntypedDefinition::TypeAlias(ast::TypeAlias {
                alias,
                annotation,
                doc: None,
                location: span,
                parameters: parameters.unwrap_or_default(),
                public: opt_pub.is_some(),
                tipo: (),
            })
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
                .allow_trailing()
                .delimited_by(just(Token::LeftParen), just(Token::RightParen))
                .map_with_span(|arguments, span| (arguments, span)),
        )
        .then(just(Token::RArrow).ignore_then(type_parser()).or_not())
        .then(
            expr_seq_parser()
                .or_not()
                .delimited_by(just(Token::LeftBrace), just(Token::RightBrace)),
        )
        .map_with_span(
            |((((opt_pub, name), (arguments, args_span)), return_annotation), body), span| {
                ast::UntypedDefinition::Fn(ast::Function {
                    arguments,
                    body: body.unwrap_or(expr::UntypedExpr::Todo {
                        kind: TodoKind::EmptyFunction,
                        location: span,
                        label: None,
                    }),
                    doc: None,
                    location: Span {
                        start: span.start,
                        end: return_annotation
                            .as_ref()
                            .map(|l| l.location().end)
                            .unwrap_or_else(|| args_span.end),
                    },
                    end_position: span.end - 1,
                    name,
                    public: opt_pub.is_some(),
                    return_annotation,
                    return_type: (),
                })
            },
        )
}

pub fn test_parser() -> impl Parser<Token, ast::UntypedDefinition, Error = ParseError> {
    just(Token::Test)
        .ignore_then(select! {Token::Name {name} => name})
        .then_ignore(just(Token::LeftParen))
        .then_ignore(just(Token::RightParen))
        .map_with_span(|name, span| (name, span))
        .then(
            expr_seq_parser()
                .or_not()
                .delimited_by(just(Token::LeftBrace), just(Token::RightBrace)),
        )
        .map_with_span(|((name, span_end), body), span| {
            ast::UntypedDefinition::Test(ast::Function {
                arguments: vec![],
                body: body.unwrap_or(expr::UntypedExpr::Todo {
                    kind: TodoKind::EmptyFunction,
                    location: span,
                    label: None,
                }),
                doc: None,
                location: span_end,
                end_position: span.end - 1,
                name,
                public: true,
                return_annotation: None,
                return_type: (),
            })
        })
}

fn constant_parser() -> impl Parser<Token, ast::UntypedDefinition, Error = ParseError> {
    pub_parser()
        .or_not()
        .then_ignore(just(Token::Const))
        .then(select! {Token::Name{name} => name})
        .then(just(Token::Colon).ignore_then(type_parser()).or_not())
        .then_ignore(just(Token::Equal))
        .then(constant_value_parser())
        .map_with_span(|(((public, name), annotation), value), span| {
            ast::UntypedDefinition::ModuleConstant(ast::ModuleConstant {
                doc: None,
                location: span,
                public: public.is_some(),
                name,
                annotation,
                value: Box::new(value),
                tipo: (),
            })
        })
}

fn constant_value_parser() -> impl Parser<Token, ast::UntypedConstant, Error = ParseError> {
    recursive(|r| {
        let constant_string_parser =
            select! {Token::String {value} => value}.map_with_span(|value, span| {
                ast::UntypedConstant::String {
                    location: span,
                    value,
                }
            });

        let constant_int_parser =
            select! {Token::Int {value} => value}.map_with_span(|value, span| {
                ast::UntypedConstant::Int {
                    location: span,
                    value,
                }
            });

        let constant_tuple_parser = r
            .clone()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .delimited_by(just(Token::LeftParen), just(Token::RightParen))
            .map_with_span(|elements, span| ast::UntypedConstant::Tuple {
                location: span,
                elements,
            });

        let constant_bytearray_parser = just(Token::Hash)
            .ignore_then(
                select! {Token::Int {value} => value}
                    .validate(|value, span, emit| {
                        let byte: u8 = match value.parse() {
                            Ok(b) => b,
                            Err(_) => {
                                emit(ParseError::expected_input_found(
                                    span,
                                    None,
                                    Some(error::Pattern::Byte),
                                ));

                                0
                            }
                        };

                        byte
                    })
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .delimited_by(just(Token::LeftSquare), just(Token::RightSquare)),
            )
            .map_with_span(|bytes, span| ast::UntypedConstant::ByteArray {
                location: span,
                bytes,
            });

        let constant_list_parser = r
            .clone()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .delimited_by(just(Token::LeftSquare), just(Token::RightSquare))
            .map_with_span(|elements, span| ast::UntypedConstant::List {
                location: span,
                elements,
                tipo: (),
            });

        let constant_record_parser = choice((
            choice((
                select! {Token::Name { name } => name}
                    .then_ignore(just(Token::Dot))
                    .or_not()
                    .then(select! {Token::UpName { name } => name})
                    .then(
                        select! {Token::Name {name} => name}
                            .then_ignore(just(Token::Colon))
                            .or_not()
                            .then(r.clone())
                            .validate(|(label_opt, value), span, emit| {
                                let label = if label_opt.is_some() {
                                    label_opt
                                } else if let ast::UntypedConstant::Var { name, .. } = &value {
                                    Some(name.clone())
                                } else {
                                    emit(ParseError::expected_input_found(
                                        value.location(),
                                        None,
                                        Some(error::Pattern::RecordPunning),
                                    ));

                                    None
                                };

                                ast::CallArg {
                                    location: span,
                                    value,
                                    label,
                                }
                            })
                            .separated_by(just(Token::Comma))
                            .allow_trailing()
                            .delimited_by(just(Token::LeftBrace), just(Token::RightBrace)),
                    )
                    .map_with_span(
                        |((module, name), args), span| ast::UntypedConstant::Record {
                            location: span,
                            module,
                            name,
                            args,
                            tag: (),
                            tipo: (),
                            field_map: None,
                        },
                    ),
                select! {Token::Name { name } => name}
                    .then_ignore(just(Token::Dot))
                    .or_not()
                    .then(select! {Token::UpName { name } => name})
                    .then(
                        r.clone()
                            .map_with_span(|value, span| ast::CallArg {
                                location: span,
                                value,
                                label: None,
                            })
                            .separated_by(just(Token::Comma))
                            .allow_trailing()
                            .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
                    )
                    .map_with_span(
                        |((module, name), args), span| ast::UntypedConstant::Record {
                            location: span,
                            module,
                            name,
                            args,
                            tag: (),
                            tipo: (),
                            field_map: None,
                        },
                    ),
            )),
            select! {Token::Name { name } => name}
                .then_ignore(just(Token::Dot))
                .then(select! {Token::Name{name} => name})
                .map_with_span(|(module, name), span: Span| ast::UntypedConstant::Var {
                    location: span.union(span),
                    module: Some(module),
                    name,
                    constructor: None,
                    tipo: (),
                }),
        ));

        let constant_var_parser =
            select! {Token::Name {name} => name}.map_with_span(|name, span| {
                ast::UntypedConstant::Var {
                    location: span,
                    module: None,
                    name,
                    constructor: None,
                    tipo: (),
                }
            });

        choice((
            constant_string_parser,
            constant_int_parser,
            constant_bytearray_parser,
            constant_tuple_parser,
            constant_list_parser,
            constant_record_parser,
            constant_var_parser,
        ))
    })
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

pub fn anon_fn_param_parser() -> impl Parser<Token, ast::UntypedArg, Error = ParseError> {
    // TODO: return a better error when a label is provided `UnexpectedLabel`
    choice((
        select! {Token::DiscardName {name} => name}.map_with_span(|name, span| {
            ast::ArgName::Discard {
                name,
                location: span,
            }
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
            just(Token::Trace)
                .ignore_then(
                    select! {Token::String {value} => value}
                        .delimited_by(just(Token::LeftParen), just(Token::RightParen))
                        .or_not(),
                )
                .then(r.clone())
                .map_with_span(|(text, then_), span| expr::UntypedExpr::Trace {
                    location: span,
                    then: Box::new(then_),
                    text,
                }),
            expr_parser(r.clone())
                .then(r.repeated())
                .foldl(|current, next| current.append_in_sequence(next)),
        ))
    })
}

pub fn expr_parser(
    seq_r: Recursive<'_, Token, expr::UntypedExpr, ParseError>,
) -> impl Parser<Token, expr::UntypedExpr, Error = ParseError> + '_ {
    recursive(|r| {
        let string_parser =
            select! {Token::String {value} => value}.map_with_span(|value, span| {
                expr::UntypedExpr::String {
                    location: span,
                    value,
                }
            });

        let int_parser = select! { Token::Int {value} => value}.map_with_span(|value, span| {
            expr::UntypedExpr::Int {
                location: span,
                value,
            }
        });

        let record_update_parser = select! {Token::Name { name } => name}
            .map_with_span(|module, span: Span| (module, span))
            .then_ignore(just(Token::Dot))
            .or_not()
            .then(select! {Token::UpName { name } => name}.map_with_span(|name, span| (name, span)))
            .then(
                just(Token::DotDot)
                    .ignore_then(r.clone())
                    .then(
                        just(Token::Comma)
                            .ignore_then(
                                choice((
                                    select! { Token::Name {name} => name }
                                        .then_ignore(just(Token::Colon))
                                        .then(r.clone())
                                        .map_with_span(|(label, value), span| {
                                            ast::UntypedRecordUpdateArg {
                                                label,
                                                value,
                                                location: span,
                                            }
                                        }),
                                    select! {Token::Name {name} => name}.map_with_span(
                                        |name, span| ast::UntypedRecordUpdateArg {
                                            location: span,
                                            value: expr::UntypedExpr::Var {
                                                name: name.clone(),
                                                location: span,
                                            },
                                            label: name,
                                        },
                                    ),
                                ))
                                .separated_by(just(Token::Comma))
                                .allow_trailing(),
                            )
                            .or_not(),
                    )
                    .delimited_by(just(Token::LeftBrace), just(Token::RightBrace))
                    .map_with_span(|a, span: Span| (a, span)),
            )
            .map(|((module, (name, n_span)), ((spread, opt_args), span))| {
                let constructor = if let Some((module, m_span)) = module {
                    expr::UntypedExpr::FieldAccess {
                        location: m_span.union(n_span),
                        label: name,
                        container: Box::new(expr::UntypedExpr::Var {
                            location: m_span,
                            name: module,
                        }),
                    }
                } else {
                    expr::UntypedExpr::Var {
                        location: n_span,
                        name,
                    }
                };

                let spread_span = spread.location();

                let location = Span::new((), spread_span.start - 2..spread_span.end);

                let spread = ast::RecordUpdateSpread {
                    base: Box::new(spread),
                    location,
                };

                expr::UntypedExpr::RecordUpdate {
                    location: constructor.location().union(span),
                    constructor: Box::new(constructor),
                    spread,
                    arguments: opt_args.unwrap_or_default(),
                }
            });

        let record_parser = choice((
            select! {Token::Name { name } => name}
                .map_with_span(|module, span: Span| (module, span))
                .then_ignore(just(Token::Dot))
                .or_not()
                .then(
                    select! {Token::UpName { name } => name}
                        .map_with_span(|name, span| (name, span)),
                )
                .then(
                    choice((
                        select! {Token::Name {name} => name}
                            .then_ignore(just(Token::Colon))
                            .then(choice((
                                r.clone(),
                                select! {Token::DiscardName {name} => name }.validate(
                                    |_name, span, emit| {
                                        emit(ParseError::expected_input_found(
                                            span,
                                            None,
                                            Some(error::Pattern::Discard),
                                        ));

                                        expr::UntypedExpr::Var {
                                            location: span,
                                            name: CAPTURE_VARIABLE.to_string(),
                                        }
                                    },
                                ),
                            )))
                            .map_with_span(|(label, value), span| ast::CallArg {
                                location: span,
                                value,
                                label: Some(label),
                            }),
                        choice((
                            select! {Token::Name {name} => name}.map_with_span(|name, span| {
                                (
                                    expr::UntypedExpr::Var {
                                        name: name.clone(),
                                        location: span,
                                    },
                                    name,
                                )
                            }),
                            select! {Token::DiscardName {name} => name }.validate(
                                |name, span, emit| {
                                    emit(ParseError::expected_input_found(
                                        span,
                                        None,
                                        Some(error::Pattern::Discard),
                                    ));

                                    (
                                        expr::UntypedExpr::Var {
                                            location: span,
                                            name: CAPTURE_VARIABLE.to_string(),
                                        },
                                        name,
                                    )
                                },
                            ),
                        ))
                        .map(|(value, name)| ast::CallArg {
                            location: value.location(),
                            value,
                            label: Some(name),
                        }),
                    ))
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .delimited_by(just(Token::LeftBrace), just(Token::RightBrace)),
                ),
            select! {Token::Name { name } => name}
                .map_with_span(|module, span| (module, span))
                .then_ignore(just(Token::Dot))
                .or_not()
                .then(
                    select! {Token::UpName { name } => name}
                        .map_with_span(|name, span| (name, span)),
                )
                .then(
                    select! {Token::Name {name} => name}
                        .ignored()
                        .then_ignore(just(Token::Colon))
                        .validate(|_label, span, emit| {
                            emit(ParseError::expected_input_found(
                                span,
                                None,
                                Some(error::Pattern::Label),
                            ))
                        })
                        .or_not()
                        .then(choice((
                            r.clone(),
                            select! {Token::DiscardName {name} => name }.validate(
                                |_name, span, emit| {
                                    emit(ParseError::expected_input_found(
                                        span,
                                        None,
                                        Some(error::Pattern::Discard),
                                    ));

                                    expr::UntypedExpr::Var {
                                        location: span,
                                        name: CAPTURE_VARIABLE.to_string(),
                                    }
                                },
                            ),
                        )))
                        .map(|(_label, value)| ast::CallArg {
                            location: value.location(),
                            value,
                            label: None,
                        })
                        .separated_by(just(Token::Comma))
                        .allow_trailing()
                        .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
                ),
        ))
        .map_with_span(|((module, (name, n_span)), arguments), span| {
            let fun = if let Some((module, m_span)) = module {
                expr::UntypedExpr::FieldAccess {
                    location: m_span.union(n_span),
                    label: name,
                    container: Box::new(expr::UntypedExpr::Var {
                        location: m_span,
                        name: module,
                    }),
                }
            } else {
                expr::UntypedExpr::Var {
                    location: n_span,
                    name,
                }
            };

            expr::UntypedExpr::Call {
                arguments,
                fun: Box::new(fun),
                location: span,
            }
        });

        let field_access_constructor = select! {Token::Name { name } => name}
            .map_with_span(|module, span| (module, span))
            .then_ignore(just(Token::Dot))
            .then(select! {Token::UpName { name } => name})
            .map_with_span(
                |((module, m_span), name), span| expr::UntypedExpr::FieldAccess {
                    location: span,
                    label: name,
                    container: Box::new(expr::UntypedExpr::Var {
                        location: m_span,
                        name: module,
                    }),
                },
            );

        let var_parser = select! {
            Token::Name { name } => name,
            Token::UpName { name } => name,
        }
        .map_with_span(|name, span| expr::UntypedExpr::Var {
            location: span,
            name,
        });

        let todo_parser = just(Token::Todo)
            .ignore_then(
                select! {Token::String {value} => value}
                    .delimited_by(just(Token::LeftParen), just(Token::RightParen))
                    .or_not(),
            )
            .map_with_span(|label, span| expr::UntypedExpr::Todo {
                kind: TodoKind::Keyword,
                location: span,
                label,
            });

        let tuple = r
            .clone()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .delimited_by(just(Token::LeftParen), just(Token::RightParen))
            .map_with_span(|elems, span| expr::UntypedExpr::Tuple {
                location: span,
                elems,
            });

        let bytearray = just(Token::Hash)
            .ignore_then(
                select! {Token::Int {value} => value}
                    .validate(|value, span, emit| {
                        let byte: u8 = match value.parse() {
                            Ok(b) => b,
                            Err(_) => {
                                emit(ParseError::expected_input_found(
                                    span,
                                    None,
                                    Some(error::Pattern::Byte),
                                ));

                                0
                            }
                        };

                        byte
                    })
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .delimited_by(just(Token::LeftSquare), just(Token::RightSquare)),
            )
            .map_with_span(|bytes, span| expr::UntypedExpr::ByteArray {
                location: span,
                bytes,
            });

        let list_parser = just(Token::LeftSquare)
            .ignore_then(r.clone().separated_by(just(Token::Comma)))
            .then(choice((
                just(Token::Comma).ignore_then(
                    just(Token::DotDot)
                        .ignore_then(r.clone())
                        .map(Box::new)
                        .or_not(),
                ),
                just(Token::Comma).ignored().or_not().map(|_| None),
            )))
            .then_ignore(just(Token::RightSquare))
            // TODO: check if tail.is_some and elements.is_empty then return ListSpreadWithoutElements error
            .map_with_span(|(elements, tail), span| expr::UntypedExpr::List {
                location: span,
                elements,
                tail,
            });

        let block_parser = seq_r
            .clone()
            .delimited_by(just(Token::LeftBrace), just(Token::RightBrace));

        let anon_fn_parser = just(Token::Fn)
            .ignore_then(
                anon_fn_param_parser()
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
            )
            .then(just(Token::RArrow).ignore_then(type_parser()).or_not())
            .then(seq_r.delimited_by(just(Token::LeftBrace), just(Token::RightBrace)))
            .map_with_span(
                |((arguments, return_annotation), body), span| expr::UntypedExpr::Fn {
                    arguments,
                    body: Box::new(body),
                    location: span,
                    is_capture: false,
                    return_annotation,
                },
            );

        // TODO: do guards later
        // let when_clause_guard_parser = just(Token::If);

        let when_clause_parser = pattern_parser()
            .separated_by(just(Token::Comma))
            .at_least(1)
            .then(
                just(Token::Vbar)
                    .ignore_then(
                        pattern_parser()
                            .separated_by(just(Token::Comma))
                            .at_least(1),
                    )
                    .repeated()
                    .or_not(),
            )
            // TODO: do guards later
            // .then(when_clause_guard_parser)
            // TODO: add hint "Did you mean to wrap a multi line clause in curly braces?"
            .then_ignore(just(Token::RArrow))
            .then(r.clone())
            .map_with_span(|((patterns, alternative_patterns_opt), then), span| {
                ast::UntypedClause {
                    location: span,
                    pattern: patterns,
                    alternative_patterns: alternative_patterns_opt.unwrap_or_default(),
                    guard: None,
                    then,
                }
            });

        let when_parser = just(Token::When)
            // TODO: If subject is empty we should return ParseErrorType::ExpectedExpr,
            .ignore_then(r.clone().separated_by(just(Token::Comma)))
            .then_ignore(just(Token::Is))
            .then_ignore(just(Token::LeftBrace))
            // TODO: If clauses are empty we should return ParseErrorType::NoCaseClause
            .then(when_clause_parser.repeated())
            .then_ignore(just(Token::RightBrace))
            .map_with_span(|(subjects, clauses), span| expr::UntypedExpr::When {
                location: span,
                subjects,
                clauses,
            });

        let let_parser = just(Token::Let)
            .ignore_then(pattern_parser())
            .then(just(Token::Colon).ignore_then(type_parser()).or_not())
            .then_ignore(just(Token::Equal))
            .then(r.clone())
            .map_with_span(
                |((pattern, annotation), value), span| expr::UntypedExpr::Assignment {
                    location: span,
                    value: Box::new(value),
                    pattern,
                    kind: ast::AssignmentKind::Let,
                    annotation,
                },
            );

        let assert_parser = just(Token::Assert)
            .ignore_then(pattern_parser())
            .then(just(Token::Colon).ignore_then(type_parser()).or_not())
            .then_ignore(just(Token::Equal))
            .then(r.clone())
            .map_with_span(
                |((pattern, annotation), value), span| expr::UntypedExpr::Assignment {
                    location: span,
                    value: Box::new(value),
                    pattern,
                    kind: ast::AssignmentKind::Assert,
                    annotation,
                },
            );

        let check_parser = just(Token::Check)
            .ignore_then(pattern_parser())
            .then(just(Token::Colon).ignore_then(type_parser()).or_not())
            .then_ignore(just(Token::Equal))
            .then(r.clone())
            .map_with_span(
                |((pattern, annotation), value), span| expr::UntypedExpr::Assignment {
                    location: span,
                    value: Box::new(value),
                    pattern,
                    kind: ast::AssignmentKind::Check,
                    annotation,
                },
            );

        let if_parser = just(Token::If)
            .ignore_then(r.clone().then(block_parser.clone()).map_with_span(
                |(condition, body), span| ast::IfBranch {
                    condition,
                    body,
                    location: span,
                },
            ))
            .then(
                just(Token::Else)
                    .ignore_then(just(Token::If))
                    .ignore_then(r.clone().then(block_parser.clone()).map_with_span(
                        |(condition, body), span| ast::IfBranch {
                            condition,
                            body,
                            location: span,
                        },
                    ))
                    .repeated(),
            )
            .then_ignore(just(Token::Else))
            .then(block_parser.clone())
            .map_with_span(|((first, alternative_branches), final_else), span| {
                let mut branches = vec1::vec1![first];

                branches.extend(alternative_branches);

                expr::UntypedExpr::If {
                    location: span,
                    branches,
                    final_else: Box::new(final_else),
                }
            });

        let expr_unit_parser = choice((
            string_parser,
            int_parser,
            record_update_parser,
            record_parser,
            field_access_constructor,
            var_parser,
            todo_parser,
            tuple,
            bytearray,
            list_parser,
            anon_fn_parser,
            block_parser,
            when_parser,
            let_parser,
            assert_parser,
            check_parser,
            if_parser,
        ));

        // Parsing a function call into the appropriate structure
        #[derive(Debug)]
        enum ParserArg {
            Arg(Box<ast::CallArg<expr::UntypedExpr>>),
            Hole {
                location: Span,
                label: Option<String>,
            },
        }

        enum Chain {
            Call(Vec<ParserArg>, Span),
            FieldAccess(String, Span),
        }

        let field_access_parser = just(Token::Dot)
            .ignore_then(select! {
                Token::Name { name } => name,
            })
            .map_with_span(Chain::FieldAccess);

        let call_parser = choice((
            select! { Token::Name { name } => name }
                .then_ignore(just(Token::Colon))
                .or_not()
                .then(r.clone())
                .map_with_span(|(label, value), span| {
                    ParserArg::Arg(Box::new(ast::CallArg {
                        label,
                        location: span,
                        value,
                    }))
                }),
            select! { Token::Name { name } => name }
                .then_ignore(just(Token::Colon))
                .or_not()
                .then_ignore(select! {Token::DiscardName {name} => name })
                .map_with_span(|label, span| ParserArg::Hole {
                    location: span,
                    label,
                }),
        ))
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .delimited_by(just(Token::LeftParen), just(Token::RightParen))
        .map_with_span(Chain::Call);

        let chain = choice((field_access_parser, call_parser));

        let chained = expr_unit_parser
            .then(chain.repeated())
            .foldl(|e, chain| match chain {
                Chain::Call(args, span) => {
                    let mut holes = Vec::new();

                    let args = args
                        .into_iter()
                        .enumerate()
                        .map(|(index, a)| match a {
                            ParserArg::Arg(arg) => *arg,
                            ParserArg::Hole { location, label } => {
                                holes.push(ast::Arg {
                                    location: Span::empty(),
                                    annotation: None,
                                    arg_name: ast::ArgName::Named {
                                        name: format!("{}__{}", CAPTURE_VARIABLE, index),
                                        location: Span::empty(),
                                    },
                                    tipo: (),
                                });

                                ast::CallArg {
                                    label,
                                    location,
                                    value: expr::UntypedExpr::Var {
                                        location,
                                        name: format!("{}__{}", CAPTURE_VARIABLE, index),
                                    },
                                }
                            }
                        })
                        .collect();

                    let call = expr::UntypedExpr::Call {
                        location: span,
                        fun: Box::new(e),
                        arguments: args,
                    };

                    if holes.is_empty() {
                        call
                    } else {
                        expr::UntypedExpr::Fn {
                            location: call.location(),
                            is_capture: true,
                            arguments: holes,
                            body: Box::new(call),
                            return_annotation: None,
                        }
                    }
                }

                Chain::FieldAccess(label, span) => expr::UntypedExpr::FieldAccess {
                    location: e.location().union(span),
                    label,
                    container: Box::new(e),
                },
            });

        // Negate
        let op = just(Token::Bang);

        let unary = op
            .ignored()
            .map_with_span(|_, span| span)
            .repeated()
            .then(chained)
            .foldr(|span, value| expr::UntypedExpr::Negate {
                location: span.union(value.location()),
                value: Box::new(value),
            })
            .boxed();

        // Product
        let op = choice((
            just(Token::Star).to(BinOp::MultInt),
            just(Token::Slash).to(BinOp::DivInt),
            just(Token::Percent).to(BinOp::ModInt),
        ));

        let product = unary
            .clone()
            .then(op.then(unary).repeated())
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

        // Comparison
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

        // Logical
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

pub fn type_parser() -> impl Parser<Token, ast::Annotation, Error = ParseError> {
    recursive(|r| {
        choice((
            // Type hole
            select! {Token::DiscardName { name } => name}.map_with_span(|name, span| {
                ast::Annotation::Hole {
                    location: span,
                    name,
                }
            }),
            // Tuple
            r.clone()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .delimited_by(just(Token::LeftParen), just(Token::RightParen))
                .map_with_span(|elems, span| ast::Annotation::Tuple {
                    location: span,
                    elems,
                }),
            // Function
            just(Token::Fn)
                .ignore_then(
                    r.clone()
                        .separated_by(just(Token::Comma))
                        .allow_trailing()
                        .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
                )
                .then_ignore(just(Token::RArrow))
                .then(r.clone())
                .map_with_span(|(arguments, ret), span| ast::Annotation::Fn {
                    location: span,
                    arguments,
                    ret: Box::new(ret),
                }),
            // Constructor function
            select! {Token::UpName { name } => name}
                .then(
                    r.clone()
                        .separated_by(just(Token::Comma))
                        .allow_trailing()
                        .delimited_by(just(Token::Less), just(Token::Greater))
                        .or_not(),
                )
                .map_with_span(|(name, arguments), span| ast::Annotation::Constructor {
                    location: span,
                    module: None,
                    name,
                    arguments: arguments.unwrap_or_default(),
                }),
            // Constructor Module or type Variable
            select! {Token::Name { name } => name}
                .then(
                    just(Token::Dot)
                        .ignore_then(select! {Token::UpName {name} => name})
                        .then(
                            r.separated_by(just(Token::Comma))
                                .allow_trailing()
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
                        // TODO: parse_error(ParseErrorType::NotConstType, SrcSpan { start, end })
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
        .allow_trailing()
        .delimited_by(just(Token::LeftBrace), just(Token::RightBrace))
}

pub fn type_name_with_args() -> impl Parser<Token, (String, Option<Vec<String>>), Error = ParseError>
{
    just(Token::Type).ignore_then(
        select! {Token::UpName { name } => name}.then(
            select! {Token::Name { name } => name}
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .delimited_by(just(Token::Less), just(Token::Greater))
                .or_not(),
        ),
    )
}

pub fn pub_parser() -> impl Parser<Token, (), Error = ParseError> {
    just(Token::Pub).ignored()
}

pub fn pattern_parser() -> impl Parser<Token, ast::UntypedPattern, Error = ParseError> {
    recursive(|r| {
        let record_constructor_pattern_arg_parser = choice((
            select! {Token::Name {name} => name}
                .then_ignore(just(Token::Colon))
                .then(r.clone())
                .map_with_span(|(name, pattern), span| ast::CallArg {
                    location: span,
                    label: Some(name),
                    value: pattern,
                }),
            select! {Token::Name{name} => name}.map_with_span(|name, span| ast::CallArg {
                location: span,
                value: ast::UntypedPattern::Var {
                    name: name.clone(),
                    location: span,
                },
                label: Some(name),
            }),
        ))
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .then(
            just(Token::DotDot)
                .then_ignore(just(Token::Comma).or_not())
                .ignored()
                .or_not(),
        )
        .delimited_by(just(Token::LeftBrace), just(Token::RightBrace));

        let tuple_constructor_pattern_arg_parser = r
            .clone()
            .map(|pattern| ast::CallArg {
                location: pattern.location(),
                value: pattern,
                label: None,
            })
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .then(
                just(Token::DotDot)
                    .then_ignore(just(Token::Comma).or_not())
                    .ignored()
                    .or_not(),
            )
            .delimited_by(just(Token::LeftParen), just(Token::RightParen));

        let constructor_pattern_args_parser = choice((
            record_constructor_pattern_arg_parser.map(|a| (a, true)),
            tuple_constructor_pattern_arg_parser.map(|a| (a, false)),
        ))
        .or_not()
        .map(|opt_args| {
            opt_args
                .map(|((a, b), c)| (a, b.is_some(), c))
                .unwrap_or_else(|| (vec![], false, false))
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
                    if let Some((c_name, (arguments, with_spread, is_record))) = opt_pattern {
                        ast::UntypedPattern::Constructor {
                            is_record,
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
            constructor_pattern_parser.map_with_span(
                |(name, (arguments, with_spread, is_record)), span| {
                    ast::UntypedPattern::Constructor {
                        is_record,
                        location: span,
                        name,
                        arguments,
                        module: None,
                        constructor: (),
                        with_spread,
                        tipo: (),
                    }
                },
            ),
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
            r.clone()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .delimited_by(just(Token::LeftParen), just(Token::RightParen))
                .map_with_span(|elems, span| ast::UntypedPattern::Tuple {
                    location: span,
                    elems,
                }),
            just(Token::LeftSquare)
                .ignore_then(r.clone().separated_by(just(Token::Comma)))
                .then(choice((
                    just(Token::Comma)
                        .ignore_then(just(Token::DotDot).ignore_then(r.clone().or_not()).or_not()),
                    just(Token::Comma).ignored().or_not().map(|_| None),
                )))
                .then_ignore(just(Token::RightSquare))
                .validate(|(elements, tail), span: Span, emit| {
                    let tail = match tail {
                        // There is a tail and it has a Pattern::Var or Pattern::Discard
                        Some(Some(
                            pat @ (ast::UntypedPattern::Var { .. }
                            | ast::UntypedPattern::Discard { .. }),
                        )) => Some(pat),
                        Some(Some(pat)) => {
                            emit(ParseError::expected_input_found(
                                pat.location(),
                                None,
                                Some(error::Pattern::Match),
                            ));

                            Some(pat)
                        }
                        // There is a tail but it has no content, implicit discard
                        Some(None) => Some(ast::UntypedPattern::Discard {
                            location: Span {
                                start: span.end - 1,
                                end: span.end,
                            },
                            name: "_".to_string(),
                        }),
                        // No tail specified
                        None => None,
                    };

                    ast::UntypedPattern::List {
                        location: span,
                        elements,
                        tail: tail.map(Box::new),
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
