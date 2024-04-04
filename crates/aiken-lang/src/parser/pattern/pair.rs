use crate::{
    ast::UntypedPattern,
    builtins::PAIR,
    parser::{error::ParseError, token::Token},
};
use chumsky::prelude::*;

pub fn parser(
    pattern: Recursive<'_, Token, UntypedPattern, ParseError>,
) -> impl Parser<Token, UntypedPattern, Error = ParseError> + '_ {
    select! {Token::UpName { name } if name == PAIR => name}
        .ignore_then(choice((
            just(Token::LeftParen),
            just(Token::NewLineLeftParen),
        )))
        .then(pattern.clone())
        .then_ignore(just(Token::Comma))
        .then(pattern.clone())
        .then_ignore(just(Token::RightParen))
        .map_with_span(|((_name, fst), snd), location| UntypedPattern::Pair {
            fst: Box::new(fst),
            snd: Box::new(snd),
            location,
        })
}
