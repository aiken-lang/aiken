use chumsky::prelude::*;
use vec1::{vec1, Vec1};

mod sequence;
pub mod string;

pub use sequence::parser as sequence;

use crate::{
    ast::{self, Span},
    expr::UntypedExpr,
    parser::error,
};

use super::{error::ParseError, token::Token};

pub fn parser(
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
                                            name: ast::CAPTURE_VARIABLE.to_string(),
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
