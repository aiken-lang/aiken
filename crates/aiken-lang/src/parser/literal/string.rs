use chumsky::prelude::*;

use crate::parser::{error::ParseError, token::Token};

pub fn parser() -> impl Parser<Token, String, Error = ParseError> {
    select! {Token::String {value} => value}
}
