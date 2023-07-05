use chumsky::prelude::*;

use crate::{
    ast,
    parser::{definition, error::ParseError, token::Token},
};

pub fn parser() -> impl Parser<Token, ast::UntypedClauseGuard, Error = ParseError> {
    recursive(|expression| {
        let var_parser = select! {
            Token::Name { name } => name,
            Token::UpName { name } => name,
        }
        .map_with_span(|name, span| ast::ClauseGuard::Var {
            name,
            tipo: (),
            location: span,
        });

        let constant_parser = definition::constant::value().map(ast::ClauseGuard::Constant);

        let block_parser = expression
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
            just(Token::EqualEqual).to(ast::BinOp::Eq),
            just(Token::NotEqual).to(ast::BinOp::NotEq),
            just(Token::Less).to(ast::BinOp::LtInt),
            just(Token::Greater).to(ast::BinOp::GtInt),
            just(Token::LessEqual).to(ast::BinOp::LtEqInt),
            just(Token::GreaterEqual).to(ast::BinOp::GtEqInt),
        ));

        let comparison = unary
            .clone()
            .then(comparison_op.then(unary).repeated())
            .foldl(|left, (op, right)| {
                let location = left.location().union(right.location());
                let left = Box::new(left);
                let right = Box::new(right);
                match op {
                    ast::BinOp::Eq => ast::ClauseGuard::Equals {
                        location,
                        left,
                        right,
                    },
                    ast::BinOp::NotEq => ast::ClauseGuard::NotEquals {
                        location,
                        left,
                        right,
                    },
                    ast::BinOp::LtInt => ast::ClauseGuard::LtInt {
                        location,
                        left,
                        right,
                    },
                    ast::BinOp::GtInt => ast::ClauseGuard::GtInt {
                        location,
                        left,
                        right,
                    },
                    ast::BinOp::LtEqInt => ast::ClauseGuard::LtEqInt {
                        location,
                        left,
                        right,
                    },
                    ast::BinOp::GtEqInt => ast::ClauseGuard::GtEqInt {
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
