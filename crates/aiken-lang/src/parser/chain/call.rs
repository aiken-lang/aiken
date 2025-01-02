use super::Chain;
use crate::{
    ast::CallArg,
    expr::UntypedExpr,
    parser::{token::Token, ParseError},
};
use chumsky::prelude::*;

pub(crate) fn parser(
    expression: Recursive<'_, Token, UntypedExpr, ParseError>,
) -> impl Parser<Token, Chain, Error = ParseError> + '_ {
    choice((
        select! { Token::Name { name } => name }
            .then_ignore(just(Token::Colon))
            .or_not()
            .then(expression)
            .map_with_span(|(label, value), location| CallArg {
                label,
                location,
                value: Some(value),
            }),
        select! { Token::Name { name } => name }
            .then_ignore(just(Token::Colon))
            .or_not()
            .then(select! {Token::DiscardName {name} => name })
            .map_with_span(|(label, name), location| CallArg {
                location,
                label,
                value: Some(UntypedExpr::Var { location, name }),
            }),
    ))
    .separated_by(just(Token::Comma))
    .allow_trailing()
    .delimited_by(just(Token::LeftParen), just(Token::RightParen))
    .map_with_span(Chain::Call)
}
