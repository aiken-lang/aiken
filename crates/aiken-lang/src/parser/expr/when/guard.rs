use crate::{
    ast,
    parser::{error::ParseError, literal, token::Token},
};
use chumsky::prelude::*;

pub fn parser() -> impl Parser<Token, ast::UntypedClauseGuard, Error = ParseError> {
    recursive(|expression| {
        let var_parser = select! {
            Token::Name { name } => name,
            Token::UpName { name } => name,
        }
        .map_with_span(|_name, _span| ast::UntypedClauseGuard {});

        let block_parser = expression
            .clone()
            .delimited_by(just(Token::LeftParen), just(Token::RightParen));

        let leaf_parser = choice((var_parser, constant(), block_parser)).boxed();

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

// NOTE: This is only there for backward-compatibility, in order to provide nicer error message
// when a clause guard is found. However, Aiken no longer supports clause guards.
pub fn constant() -> impl Parser<Token, ast::UntypedClauseGuard, Error = ParseError> {
    let constant_string_parser =
        select! {Token::String {value} => value}.map(|_| ast::UntypedClauseGuard {});

    let constant_int_parser = literal::int().map(|_| ast::UntypedClauseGuard {});

    let constant_bytearray_parser = literal::bytearray(|_, _, _, _, _| ast::UntypedClauseGuard {});

    choice((
        constant_string_parser,
        constant_int_parser,
        constant_bytearray_parser,
    ))
}
