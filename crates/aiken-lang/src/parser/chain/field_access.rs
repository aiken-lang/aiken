use chumsky::prelude::*;

use super::Chain;
use crate::{
    expr::UntypedExpr,
    parser::{token::Token, ParseError},
};

pub(crate) fn parser() -> impl Parser<Token, Chain, Error = ParseError> {
    just(Token::Dot)
        .ignore_then(select! {
            Token::Name { name } => name,
        })
        .map_with_span(Chain::FieldAccess)
}

pub(crate) fn constructor() -> impl Parser<Token, UntypedExpr, Error = ParseError> {
    select! {Token::Name { name } => name}
        .map_with_span(|module, span| (module, span))
        .then_ignore(just(Token::Dot))
        .then(select! {Token::UpName { name } => name})
        .map_with_span(|((module, m_span), name), span| UntypedExpr::FieldAccess {
            location: span,
            label: name,
            container: Box::new(UntypedExpr::Var {
                location: m_span,
                name: module,
            }),
        })
}
