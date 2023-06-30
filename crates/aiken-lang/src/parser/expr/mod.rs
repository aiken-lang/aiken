use chumsky::prelude::*;
use vec1::{vec1, Vec1};

mod block;
mod bytearray;
mod if_else;
mod int;
mod list;
mod record;
mod record_update;
mod sequence;
pub mod string;
mod tuple;
mod var;

pub use block::parser as block;
pub use bytearray::parser as bytearray;
pub use if_else::parser as if_else;
pub use int::parser as int;
pub use list::parser as list;
pub use record::parser as record;
pub use record_update::parser as record_update;
pub use sequence::parser as sequence;
pub use string::parser as string;
pub use tuple::parser as tuple;
pub use var::parser as var;

use crate::{
    ast::{self, Span},
    expr::UntypedExpr,
};

use super::{error::ParseError, token::Token};

pub fn parser(
    seq_r: Recursive<'_, Token, UntypedExpr, ParseError>,
) -> impl Parser<Token, UntypedExpr, Error = ParseError> + '_ {
    recursive(|r| {
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

        let expr_unit_parser = choice((
            string(),
            int(),
            record_update(r),
            record(r),
            field_access_constructor,
            var(),
            tuple(r),
            bytearray(),
            list(r),
            anon_fn_parser,
            anon_binop_parser,
            block(seq_r),
            when_parser,
            let_parser,
            expect_parser,
            if_else(seq_r, r),
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
