use crate::{
    ast,
    parser::{definition, error::ParseError, token::Token},
};
use chumsky::prelude::*;

pub fn parser() -> impl Parser<Token, ast::UntypedClauseGuard, Error = ParseError> {
    recursive(|expression| {
        let var_parser = select! {
            Token::Name { name } => name,
            Token::UpName { name } => name,
        }
        .map_with_span(|_name, _span| ast::UntypedClauseGuard {});

        let constant_parser = definition::constant::value().map(|_| ast::UntypedClauseGuard {});

        let block_parser = expression
            .clone()
            .delimited_by(just(Token::LeftParen), just(Token::RightParen));

        let leaf_parser = choice((var_parser, constant_parser, block_parser)).boxed();

        let unary_op = just(Token::Bang);

        let unary = unary_op
            .map_with_span(|op, span| (op, span))
            .repeated()
            .then(leaf_parser)
            .foldr(|(_, _span), _value| ast::UntypedClauseGuard {})
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
            .foldl(|_left, (_op, _right)| ast::UntypedClauseGuard {})
            .boxed();

        let and_op = just(Token::AmperAmper);
        let conjunction = comparison
            .clone()
            .then(and_op.then(comparison).repeated())
            .foldl(|_left, (_tok, _right)| ast::UntypedClauseGuard {});

        let or_op = just(Token::VbarVbar);
        conjunction
            .clone()
            .then(or_op.then(conjunction).repeated())
            .foldl(|_left, (_tok, _right)| ast::UntypedClauseGuard {})
    })
}
