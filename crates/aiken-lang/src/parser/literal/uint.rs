use chumsky::prelude::*;

use crate::parser::{
    error::ParseError,
    token::{Base, Token},
};

pub fn parser() -> impl Parser<Token, (String, Base), Error = ParseError> {
    select! { Token::Int {value, base} => (value, base)}
}
