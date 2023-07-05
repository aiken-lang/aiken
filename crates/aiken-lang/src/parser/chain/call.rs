use chumsky::prelude::*;

use super::{Chain, ParserArg};
use crate::{
    ast,
    expr::UntypedExpr,
    parser::{token::Token, ParseError},
};

pub(crate) fn parser<'a>(
    expression: Recursive<'a, Token, UntypedExpr, ParseError>,
) -> impl Parser<Token, Chain, Error = ParseError> + 'a {
    choice((
        select! { Token::Name { name } => name }
            .then_ignore(just(Token::Colon))
            .or_not()
            .then(expression)
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
    .map_with_span(Chain::Call)
}
