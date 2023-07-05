use chumsky::prelude::*;

use super::Chain;
use crate::parser::{token::Token, ParseError};

pub(crate) fn parser() -> impl Parser<Token, Chain, Error = ParseError> {
    just(Token::Dot)
        .ignore_then(select! {
            Token::Ordinal { index } => index,
        })
        .validate(|index, span, emit| {
            if index < 1 {
                emit(ParseError::invalid_tuple_index(
                    span,
                    index.to_string(),
                    None,
                ));
                Chain::TupleIndex(0, span)
            } else {
                Chain::TupleIndex(index as usize - 1, span)
            }
        })
}
