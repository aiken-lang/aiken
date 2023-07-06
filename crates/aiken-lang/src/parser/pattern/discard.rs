use chumsky::prelude::*;

use crate::{
    ast::UntypedPattern,
    parser::{error::ParseError, token::Token},
};

pub fn parser() -> impl Parser<Token, UntypedPattern, Error = ParseError> {
    select! {Token::DiscardName {name} => name}
        .map_with_span(|name, location| UntypedPattern::Discard { name, location })
}
