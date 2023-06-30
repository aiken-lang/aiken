use chumsky::prelude::*;

use crate::{
    expr::UntypedExpr,
    parser::{error::ParseError, token::Token},
};

pub fn parser() -> impl Parser<Token, UntypedExpr, Error = ParseError> {
    select! {
        Token::Name { name } => name,
        Token::UpName { name } => name,
    }
    .map_with_span(|name, span| UntypedExpr::Var {
        location: span,
        name,
    })
}
