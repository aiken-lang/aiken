use chumsky::prelude::*;

use crate::{
    ast::UntypedPattern,
    parser::{error::ParseError, token::Token},
};

pub fn parser(
    expression: Recursive<'_, Token, UntypedPattern, ParseError>,
) -> impl Parser<Token, UntypedPattern, Error = ParseError> + '_ {
    expression
        .clone()
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .delimited_by(
            choice((just(Token::LeftParen), just(Token::NewLineLeftParen))),
            just(Token::RightParen),
        )
        .map_with_span(|elems, location| UntypedPattern::Tuple { location, elems })
}
