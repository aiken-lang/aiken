use chumsky::prelude::*;

use super::Chain;
use crate::{
    ast::CallArg,
    expr::UntypedExpr,
    parser::{token::Token, ParseError},
};

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
            .then_ignore(select! {Token::DiscardName {name} => name })
            .map_with_span(|label, location| CallArg {
                location,
                label,
                value: None,
            }),
    ))
    .separated_by(just(Token::Comma))
    .allow_trailing()
    .delimited_by(just(Token::LeftParen), just(Token::RightParen))
    .map_with_span(Chain::Call)
}
