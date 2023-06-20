use chumsky::prelude::*;

use super::{error::ParseError, token::Token};

pub fn public() -> impl Parser<Token, (), Error = ParseError> {
    just(Token::Pub).ignored()
}
