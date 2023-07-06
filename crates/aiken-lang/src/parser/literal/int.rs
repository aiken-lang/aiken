use chumsky::prelude::*;

use crate::parser::{
    error::ParseError,
    token::{Base, Token},
};

pub fn parser() -> impl Parser<Token, (String, Base), Error = ParseError> {
    choice((just(Token::NewLineMinus), just(Token::Minus)))
        .ignored()
        .or_not()
        .map(|v| v.is_some())
        .then(select! { Token::Int {value, base} => (value, base)})
        .map(|(is_negative, (value, base))| {
            if is_negative {
                (format!("-{value}"), base)
            } else {
                (value, base)
            }
        })
}
