mod annotation;
pub mod definitions;
pub mod error;
mod expr;
pub mod extra;
pub mod lexer;
mod module;
pub mod token;
mod utils;

pub use annotation::parser as annotation;

use crate::{
    ast::{self, BinOp, ByteArrayFormatPreference, Span, UnOp, CAPTURE_VARIABLE},
    expr::{FnStyle, UntypedExpr},
};
use chumsky::{chain::Chain, prelude::*};
use error::ParseError;
use extra::ModuleExtra;
use token::Token;
use vec1::{vec1, Vec1};

pub fn module(
    src: &str,
    kind: ast::ModuleKind,
) -> Result<(ast::UntypedModule, ModuleExtra), Vec<ParseError>> {
    let len = src.as_bytes().len();

    let span = |i, n| Span::new((), i..i + n);

    let tokens = lexer::lexer().parse(chumsky::Stream::from_iter(
        span(len, 1),
        src.chars().scan(0, |i, c| {
            let start = *i;
            let offset = c.len_utf8();
            *i = start + offset;
            Some((c, span(start, offset)))
        }),
    ))?;

    let mut extra = ModuleExtra::new();

    let mut previous_is_newline = false;

    let tokens = tokens.into_iter().filter_map(|(token, ref span)| {
        let current_is_newline = token == Token::NewLine || token == Token::EmptyLine;
        let result = match token {
            Token::ModuleComment => {
                extra.module_comments.push(*span);
                None
            }
            Token::DocComment => {
                extra.doc_comments.push(*span);
                None
            }
            Token::Comment => {
                extra.comments.push(*span);
                None
            }
            Token::EmptyLine => {
                extra.empty_lines.push(span.start);
                None
            }
            Token::LeftParen => {
                if previous_is_newline {
                    Some((Token::NewLineLeftParen, *span))
                } else {
                    Some((Token::LeftParen, *span))
                }
            }
            Token::Pipe => {
                if previous_is_newline {
                    Some((Token::NewLinePipe, *span))
                } else {
                    Some((Token::Pipe, *span))
                }
            }
            Token::NewLine => None,
            _ => Some((token, *span)),
        };

        previous_is_newline = current_is_newline;

        result
    });

    let definitions =
        module::parser().parse(chumsky::Stream::from_iter(span(tokens.len()), tokens))?;

    let module = ast::UntypedModule {
        kind,
        definitions,
        docs: vec![],
        name: "".to_string(),
        type_info: (),
    };

    Ok((module, extra))
}

pub fn anon_fn_param_parser() -> impl Parser<Token, ast::UntypedArg, Error = ParseError> {
    // TODO: return a better error when a label is provided `UnexpectedLabel`
    choice((
        select! {Token::DiscardName {name} => name}.map_with_span(|name, span| {
            ast::ArgName::Discarded {
                label: name.clone(),
                name,
                location: span,
            }
        }),
        select! {Token::Name {name} => name}.map_with_span(|name, span| ast::ArgName::Named {
            label: name.clone(),
            name,
            location: span,
            is_validator_param: false,
        }),
    ))
    .then(just(Token::Colon).ignore_then(annotation()).or_not())
    .map_with_span(|(arg_name, annotation), span| ast::Arg {
        location: span,
        annotation,
        tipo: (),
        arg_name,
    })
}

// Interpret bytearray string literals written as utf-8 strings, as strings.
//
// This is mostly convenient so that todo & error works with either @"..." or plain "...".
// In this particular context, there's actually no ambiguity about the right-hand-side, so
// we can provide this syntactic sugar.
fn flexible_string_literal(expr: UntypedExpr) -> UntypedExpr {
    match expr {
        UntypedExpr::ByteArray {
            preferred_format: ByteArrayFormatPreference::Utf8String,
            bytes,
            location,
        } => UntypedExpr::String {
            location,
            value: String::from_utf8(bytes).unwrap(),
        },
        _ => expr,
    }
}

pub fn expr_parser(
    seq_r: Recursive<'_, Token, UntypedExpr, ParseError>,
) -> impl Parser<Token, UntypedExpr, Error = ParseError> + '_ {
    recursive(|r| {
        let string_parser =
            select! {Token::String {value} => value}.map_with_span(|value, span| {
                UntypedExpr::String {
                    location: span,
                    value,
                }
            });

        let int_parser = select! { Token::Int {value, base} => (value, base)}.map_with_span(
            |(value, base), span| UntypedExpr::Int {
                location: span,
                value,
                base,
            },
        );

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
                                            value: UntypedExpr::Var {
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
                    UntypedExpr::FieldAccess {
                        location: m_span.union(n_span),
                        label: name,
                        container: Box::new(UntypedExpr::Var {
                            location: m_span,
                            name: module,
                        }),
                    }
                } else {
                    UntypedExpr::Var {
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

                UntypedExpr::RecordUpdate {
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

                                        UntypedExpr::Var {
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
                                    UntypedExpr::Var {
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
                                        UntypedExpr::Var {
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

                                    UntypedExpr::Var {
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
                UntypedExpr::FieldAccess {
                    location: m_span.union(n_span),
                    label: name,
                    container: Box::new(UntypedExpr::Var {
                        location: m_span,
                        name: module,
                    }),
                }
            } else {
                UntypedExpr::Var {
                    location: n_span,
                    name,
                }
            };

            UntypedExpr::Call {
                arguments,
                fun: Box::new(fun),
                location: span,
            }
        });

        let field_access_constructor = select! {Token::Name { name } => name}
            .map_with_span(|module, span| (module, span))
            .then_ignore(just(Token::Dot))
            .then(select! {Token::UpName { name } => name})
            .map_with_span(|((module, m_span), name), span| UntypedExpr::FieldAccess {
                location: span,
                label: name,
                container: Box::new(UntypedExpr::Var {
                    location: m_span,
                    name: module,
                }),
            });

        let var_parser = select! {
            Token::Name { name } => name,
            Token::UpName { name } => name,
        }
        .map_with_span(|name, span| UntypedExpr::Var {
            location: span,
            name,
        });

        let tuple = r
            .clone()
            .separated_by(just(Token::Comma))
            .at_least(2)
            .allow_trailing()
            .delimited_by(
                choice((just(Token::LeftParen), just(Token::NewLineLeftParen))),
                just(Token::RightParen),
            )
            .map_with_span(|elems, span| UntypedExpr::Tuple {
                location: span,
                elems,
            });

        let bytearray = utils::bytearray().map_with_span(|(preferred_format, bytes), span| {
            UntypedExpr::ByteArray {
                location: span,
                bytes,
                preferred_format,
            }
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
            .map_with_span(|(elements, tail), span| UntypedExpr::List {
                location: span,
                elements,
                tail,
            });

        let block_parser = choice((
            seq_r
                .clone()
                .delimited_by(just(Token::LeftBrace), just(Token::RightBrace)),
            seq_r.clone().delimited_by(
                choice((just(Token::LeftParen), just(Token::NewLineLeftParen))),
                just(Token::RightParen),
            ),
        ));

        let anon_fn_parser = just(Token::Fn)
            .ignore_then(
                anon_fn_param_parser()
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
            )
            .then(just(Token::RArrow).ignore_then(annotation()).or_not())
            .then(seq_r.delimited_by(just(Token::LeftBrace), just(Token::RightBrace)))
            .map_with_span(
                |((arguments, return_annotation), body), span| UntypedExpr::Fn {
                    arguments,
                    body: Box::new(body),
                    location: span,
                    fn_style: FnStyle::Plain,
                    return_annotation,
                },
            );

        let anon_binop_parser = select! {
            Token::EqualEqual => BinOp::Eq,
            Token::NotEqual => BinOp::NotEq,
            Token::Less => BinOp::LtInt,
            Token::LessEqual => BinOp::LtEqInt,
            Token::Greater => BinOp::GtInt,
            Token::GreaterEqual => BinOp::GtEqInt,
            Token::VbarVbar => BinOp::Or,
            Token::AmperAmper => BinOp::And,
            Token::Plus => BinOp::AddInt,
            Token::Minus => BinOp::SubInt,
            Token::Slash => BinOp::DivInt,
            Token::Star => BinOp::MultInt,
            Token::Percent => BinOp::ModInt,
        }
        .map_with_span(|name, location| {
            use BinOp::*;

            let arg_annotation = match name {
                Or | And => Some(ast::Annotation::boolean(location)),
                Eq | NotEq => None,
                LtInt | LtEqInt | GtInt | GtEqInt | AddInt | SubInt | MultInt | DivInt | ModInt => {
                    Some(ast::Annotation::int(location))
                }
            };

            let return_annotation = match name {
                Or | And | Eq | NotEq | LtInt | LtEqInt | GtInt | GtEqInt => {
                    Some(ast::Annotation::boolean(location))
                }
                AddInt | SubInt | MultInt | DivInt | ModInt => Some(ast::Annotation::int(location)),
            };

            let arguments = vec![
                ast::Arg {
                    arg_name: ast::ArgName::Named {
                        name: "left".to_string(),
                        label: "left".to_string(),
                        location,
                        is_validator_param: false,
                    },
                    annotation: arg_annotation.clone(),
                    location,
                    tipo: (),
                },
                ast::Arg {
                    arg_name: ast::ArgName::Named {
                        name: "right".to_string(),
                        label: "right".to_string(),
                        location,
                        is_validator_param: false,
                    },
                    annotation: arg_annotation,
                    location,
                    tipo: (),
                },
            ];

            let body = UntypedExpr::BinOp {
                location,
                name,
                left: Box::new(UntypedExpr::Var {
                    location,
                    name: "left".to_string(),
                }),
                right: Box::new(UntypedExpr::Var {
                    location,
                    name: "right".to_string(),
                }),
            };

            UntypedExpr::Fn {
                arguments,
                body: Box::new(body),
                return_annotation,
                fn_style: FnStyle::BinOp(name),
                location,
            }
        });

        let when_clause_parser = pattern_parser()
            .then(
                just(Token::Vbar)
                    .ignore_then(pattern_parser())
                    .repeated()
                    .or_not(),
            )
            .then(choice((
                just(Token::If)
                    .ignore_then(when_clause_guard_parser())
                    .or_not()
                    .then_ignore(just(Token::RArrow)),
                just(Token::If)
                    .ignore_then(take_until(just(Token::RArrow)))
                    .validate(|_value, span, emit| {
                        emit(ParseError::invalid_when_clause_guard(span));
                        None
                    }),
            )))
            // TODO: add hint "Did you mean to wrap a multi line clause in curly braces?"
            .then(choice((
                r.clone(),
                just(Token::Todo)
                    .ignore_then(
                        r.clone()
                            .then_ignore(one_of(Token::RArrow).not().rewind())
                            .or_not(),
                    )
                    .map_with_span(|reason, span| {
                        UntypedExpr::todo(span, reason.map(flexible_string_literal))
                    }),
                just(Token::ErrorTerm)
                    .ignore_then(
                        r.clone()
                            .then_ignore(just(Token::RArrow).not().rewind())
                            .or_not(),
                    )
                    .map_with_span(|reason, span| {
                        UntypedExpr::error(span, reason.map(flexible_string_literal))
                    }),
            )))
            .map_with_span(
                |(((pattern, alternative_patterns_opt), guard), then), span| {
                    let mut patterns = vec1![pattern];
                    patterns.append(&mut alternative_patterns_opt.unwrap_or_default());
                    ast::UntypedClause {
                        location: span,
                        patterns,
                        guard,
                        then,
                    }
                },
            );

        let when_parser = just(Token::When)
            // TODO: If subject is empty we should return ParseErrorType::ExpectedExpr,
            .ignore_then(r.clone().map(Box::new))
            .then_ignore(just(Token::Is))
            .then_ignore(just(Token::LeftBrace))
            // TODO: If clauses are empty we should return ParseErrorType::NoCaseClause
            .then(when_clause_parser.repeated())
            .then_ignore(just(Token::RightBrace))
            .map_with_span(|(subject, clauses), span| UntypedExpr::When {
                location: span,
                subject,
                clauses,
            });

        let let_parser = just(Token::Let)
            .ignore_then(pattern_parser())
            .then(just(Token::Colon).ignore_then(annotation()).or_not())
            .then_ignore(just(Token::Equal))
            .then(r.clone())
            .map_with_span(
                |((pattern, annotation), value), span| UntypedExpr::Assignment {
                    location: span,
                    value: Box::new(value),
                    pattern,
                    kind: ast::AssignmentKind::Let,
                    annotation,
                },
            );

        let expect_parser = just(Token::Expect)
            .ignore_then(pattern_parser())
            .then(just(Token::Colon).ignore_then(annotation()).or_not())
            .then_ignore(just(Token::Equal))
            .then(r.clone())
            .map_with_span(
                |((pattern, annotation), value), span| UntypedExpr::Assignment {
                    location: span,
                    value: Box::new(value),
                    pattern,
                    kind: ast::AssignmentKind::Expect,
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

                UntypedExpr::If {
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
            tuple,
            bytearray,
            list_parser,
            anon_fn_parser,
            anon_binop_parser,
            block_parser,
            when_parser,
            let_parser,
            expect_parser,
            if_parser,
        ));

        // Parsing a function call into the appropriate structure
        #[derive(Debug)]
        enum ParserArg {
            Arg(Box<ast::CallArg<UntypedExpr>>),
            Hole {
                location: Span,
                label: Option<String>,
            },
        }

        enum Chain {
            Call(Vec<ParserArg>, Span),
            FieldAccess(String, Span),
            TupleIndex(usize, Span),
        }

        let field_access_parser = just(Token::Dot)
            .ignore_then(select! {
                Token::Name { name } => name,
            })
            .map_with_span(Chain::FieldAccess);

        let tuple_index_parser = just(Token::Dot)
            .ignore_then(select! {
                Token::Ordinal { index } => index,
            })
            .validate(|index, span, emit| {
                if index < 1 {
                    emit(ParseError::invalid_tuple_index(
                        span,
                        index.to_string(),
                        None,
                    ));
                    Chain::TupleIndex(0, span)
                } else {
                    Chain::TupleIndex(index as usize - 1, span)
                }
            });

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

        let chain = choice((tuple_index_parser, field_access_parser, call_parser));

        let chained = expr_unit_parser
            .then(chain.repeated())
            .foldl(|expr, chain| match chain {
                Chain::Call(args, span) => {
                    let mut holes = Vec::new();

                    let args = args
                        .into_iter()
                        .enumerate()
                        .map(|(index, a)| match a {
                            ParserArg::Arg(arg) => *arg,
                            ParserArg::Hole { location, label } => {
                                let name = format!("{CAPTURE_VARIABLE}__{index}");
                                holes.push(ast::Arg {
                                    location: Span::empty(),
                                    annotation: None,
                                    arg_name: ast::ArgName::Named {
                                        label: name.clone(),
                                        name,
                                        location: Span::empty(),
                                        is_validator_param: false,
                                    },
                                    tipo: (),
                                });

                                ast::CallArg {
                                    label,
                                    location,
                                    value: UntypedExpr::Var {
                                        location,
                                        name: format!("{CAPTURE_VARIABLE}__{index}"),
                                    },
                                }
                            }
                        })
                        .collect();

                    let call = UntypedExpr::Call {
                        location: expr.location().union(span),
                        fun: Box::new(expr),
                        arguments: args,
                    };

                    if holes.is_empty() {
                        call
                    } else {
                        UntypedExpr::Fn {
                            location: call.location(),
                            fn_style: FnStyle::Capture,
                            arguments: holes,
                            body: Box::new(call),
                            return_annotation: None,
                        }
                    }
                }

                Chain::FieldAccess(label, span) => UntypedExpr::FieldAccess {
                    location: expr.location().union(span),
                    label,
                    container: Box::new(expr),
                },

                Chain::TupleIndex(index, span) => UntypedExpr::TupleIndex {
                    location: expr.location().union(span),
                    index,
                    tuple: Box::new(expr),
                },
            });

        let debug = chained.then(just(Token::Question).or_not()).map_with_span(
            |(value, token), location| match token {
                Some(_) => UntypedExpr::TraceIfFalse {
                    value: Box::new(value),
                    location,
                },
                None => value,
            },
        );

        // Negate
        let op = choice((
            just(Token::Bang).to(UnOp::Not),
            just(Token::Minus)
                // NOTE: Prevent conflict with usage for '-' as a standalone binary op.
                // This will make '-' parse when used as standalone binop in a function call.
                // For example:
                //
                //    foo(a, -, b)
                //
                // but it'll fail in a let-binding:
                //
                //    let foo = -
                //
                // which seems acceptable.
                .then_ignore(just(Token::Comma).not().rewind())
                .to(UnOp::Negate),
        ));

        let unary = op
            .map_with_span(|op, span| (op, span))
            .repeated()
            .then(debug)
            .foldr(|(un_op, span), value| UntypedExpr::UnOp {
                op: un_op,
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
            .foldl(|a, (op, b)| UntypedExpr::BinOp {
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
            .foldl(|a, (op, b)| UntypedExpr::BinOp {
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
            .foldl(|a, (op, b)| UntypedExpr::BinOp {
                location: a.location().union(b.location()),
                name: op,
                left: Box::new(a),
                right: Box::new(b),
            })
            .boxed();

        // Conjunction
        let op = just(Token::AmperAmper).to(BinOp::And);
        let conjunction = comparison
            .clone()
            .then(op.then(comparison).repeated())
            .foldl(|a, (op, b)| UntypedExpr::BinOp {
                location: a.location().union(b.location()),
                name: op,
                left: Box::new(a),
                right: Box::new(b),
            })
            .boxed();

        // Disjunction
        let op = just(Token::VbarVbar).to(BinOp::Or);
        let disjunction = conjunction
            .clone()
            .then(op.then(conjunction).repeated())
            .foldl(|a, (op, b)| UntypedExpr::BinOp {
                location: a.location().union(b.location()),
                name: op,
                left: Box::new(a),
                right: Box::new(b),
            })
            .boxed();

        // Pipeline
        disjunction
            .clone()
            .then(
                choice((just(Token::Pipe), just(Token::NewLinePipe)))
                    .then(disjunction)
                    .repeated(),
            )
            .foldl(|l, (pipe, r)| {
                if let UntypedExpr::PipeLine {
                    mut expressions,
                    one_liner,
                } = l
                {
                    expressions.push(r);
                    return UntypedExpr::PipeLine {
                        expressions,
                        one_liner,
                    };
                }

                let mut expressions = Vec1::new(l);
                expressions.push(r);
                UntypedExpr::PipeLine {
                    expressions,
                    one_liner: pipe != Token::NewLinePipe,
                }
            })
    })
}

pub fn when_clause_guard_parser() -> impl Parser<Token, ast::ClauseGuard<()>, Error = ParseError> {
    recursive(|r| {
        let var_parser = select! {
            Token::Name { name } => name,
            Token::UpName { name } => name,
        }
        .map_with_span(|name, span| ast::ClauseGuard::Var {
            name,
            tipo: (),
            location: span,
        });

        let constant_parser = definitions::constant::value().map(ast::ClauseGuard::Constant);

        let block_parser = r
            .clone()
            .delimited_by(just(Token::LeftParen), just(Token::RightParen));

        let leaf_parser = choice((var_parser, constant_parser, block_parser)).boxed();

        let unary_op = just(Token::Bang);

        let unary = unary_op
            .map_with_span(|op, span| (op, span))
            .repeated()
            .then(leaf_parser)
            .foldr(|(_, span), value| ast::ClauseGuard::Not {
                location: span.union(value.location()),
                value: Box::new(value),
            })
            .boxed();

        let comparison_op = choice((
            just(Token::EqualEqual).to(BinOp::Eq),
            just(Token::NotEqual).to(BinOp::NotEq),
            just(Token::Less).to(BinOp::LtInt),
            just(Token::Greater).to(BinOp::GtInt),
            just(Token::LessEqual).to(BinOp::LtEqInt),
            just(Token::GreaterEqual).to(BinOp::GtEqInt),
        ));

        let comparison = unary
            .clone()
            .then(comparison_op.then(unary).repeated())
            .foldl(|left, (op, right)| {
                let location = left.location().union(right.location());
                let left = Box::new(left);
                let right = Box::new(right);
                match op {
                    BinOp::Eq => ast::ClauseGuard::Equals {
                        location,
                        left,
                        right,
                    },
                    BinOp::NotEq => ast::ClauseGuard::NotEquals {
                        location,
                        left,
                        right,
                    },
                    BinOp::LtInt => ast::ClauseGuard::LtInt {
                        location,
                        left,
                        right,
                    },
                    BinOp::GtInt => ast::ClauseGuard::GtInt {
                        location,
                        left,
                        right,
                    },
                    BinOp::LtEqInt => ast::ClauseGuard::LtEqInt {
                        location,
                        left,
                        right,
                    },
                    BinOp::GtEqInt => ast::ClauseGuard::GtEqInt {
                        location,
                        left,
                        right,
                    },
                    _ => unreachable!(),
                }
            })
            .boxed();

        let and_op = just(Token::AmperAmper);
        let conjunction = comparison
            .clone()
            .then(and_op.then(comparison).repeated())
            .foldl(|left, (_tok, right)| {
                let location = left.location().union(right.location());
                let left = Box::new(left);
                let right = Box::new(right);
                ast::ClauseGuard::And {
                    location,
                    left,
                    right,
                }
            });

        let or_op = just(Token::VbarVbar);
        conjunction
            .clone()
            .then(or_op.then(conjunction).repeated())
            .foldl(|left, (_tok, right)| {
                let location = left.location().union(right.location());
                let left = Box::new(left);
                let right = Box::new(right);
                ast::ClauseGuard::Or {
                    location,
                    left,
                    right,
                }
            })
    })
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
            select! {Token::Int {value, base} => (value, base)}.map_with_span(
                |(value, base), span| ast::UntypedPattern::Int {
                    location: span,
                    value,
                    base,
                },
            ),
            r.clone()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .delimited_by(
                    choice((just(Token::LeftParen), just(Token::NewLineLeftParen))),
                    just(Token::RightParen),
                )
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
