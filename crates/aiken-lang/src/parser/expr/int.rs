use chumsky::prelude::*;

use crate::{
    expr::UntypedExpr,
    parser::{error::ParseError, token::Token},
};

pub fn parser() -> impl Parser<Token, UntypedExpr, Error = ParseError> {
    select! { Token::Int {value, base} => (value, base)}.map_with_span(|(value, base), span| {
        UntypedExpr::Int {
            location: span,
            value,
            base,
        }
    })
}
