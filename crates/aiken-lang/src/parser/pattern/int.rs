use chumsky::prelude::*;

use crate::{
    ast::UntypedPattern,
    parser::{error::ParseError, token::Token},
};

pub fn parser() -> impl Parser<Token, UntypedPattern, Error = ParseError> {
    select! {Token::Int {value, base} => (value, base)}.map_with_span(|(value, base), location| {
        UntypedPattern::Int {
            location,
            value,
            base,
        }
    })
}
