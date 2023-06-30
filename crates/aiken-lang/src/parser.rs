mod annotation;
pub mod definitions;
pub mod error;
pub mod expr;
pub mod extra;
pub mod lexer;
pub mod pattern;
pub mod token;
mod utils;

pub use annotation::parser as annotation;
pub use definitions::parser as definitions;
pub use expr::parser as expression;
pub use pattern::parser as pattern;

use crate::ast::{self, BinOp, Span};
use chumsky::{chain::Chain, prelude::*};
use error::ParseError;
use extra::ModuleExtra;
use token::Token;

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
        definitions().parse(chumsky::Stream::from_iter(span(tokens.len()), tokens))?;

    let module = ast::UntypedModule {
        kind,
        definitions,
        docs: vec![],
        name: "".to_string(),
        type_info: (),
    };

    Ok((module, extra))
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
