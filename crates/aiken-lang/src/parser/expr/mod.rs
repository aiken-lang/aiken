use chumsky::prelude::*;
use vec1::Vec1;

mod anonymous_binop;
pub mod anonymous_function;
pub mod assignment;
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
pub mod when;

use anonymous_binop::parser as anonymous_binop;
pub use anonymous_function::parser as anonymous_function;
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
pub use when::parser as when;

use crate::{
    ast::{self, Span},
    expr::{FnStyle, UntypedExpr},
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

        let expr_unit_parser = choice((
            string(),
            int(),
            record_update(r.clone()),
            record(r.clone()),
            field_access_constructor,
            var(),
            tuple(r.clone()),
            bytearray(),
            list(r.clone()),
            anonymous_function(seq_r.clone()),
            anonymous_binop(),
            block(seq_r.clone()),
            when(r.clone()),
            assignment::let_(r.clone()),
            assignment::expect(r.clone()),
            if_else(seq_r, r.clone()),
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
                .then(r)
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
                                let name = format!("{}__{index}", ast::CAPTURE_VARIABLE);

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
                                        name: format!("{}__{index}", ast::CAPTURE_VARIABLE),
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
            just(Token::Bang).to(ast::UnOp::Not),
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
                .to(ast::UnOp::Negate),
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
            just(Token::Star).to(ast::BinOp::MultInt),
            just(Token::Slash).to(ast::BinOp::DivInt),
            just(Token::Percent).to(ast::BinOp::ModInt),
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
            just(Token::Plus).to(ast::BinOp::AddInt),
            just(Token::Minus).to(ast::BinOp::SubInt),
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
            just(Token::EqualEqual).to(ast::BinOp::Eq),
            just(Token::NotEqual).to(ast::BinOp::NotEq),
            just(Token::Less).to(ast::BinOp::LtInt),
            just(Token::Greater).to(ast::BinOp::GtInt),
            just(Token::LessEqual).to(ast::BinOp::LtEqInt),
            just(Token::GreaterEqual).to(ast::BinOp::GtEqInt),
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
        let op = just(Token::AmperAmper).to(ast::BinOp::And);
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
        let op = just(Token::VbarVbar).to(ast::BinOp::Or);
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
