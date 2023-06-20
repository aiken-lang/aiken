use chumsky::prelude::*;

use crate::{
    ast,
    parser::{error::ParseError, token::Token, utils},
};

pub fn parser() -> impl Parser<Token, ast::UntypedDefinition, Error = ParseError> {
    utils::public()
        .or_not()
        .then_ignore(just(Token::Const))
        .then(select! {Token::Name{name} => name})
        .then(just(Token::Colon).ignore_then(type_parser()).or_not())
        .then_ignore(just(Token::Equal))
        .then(constant_value_parser())
        .map_with_span(|(((public, name), annotation), value), span| {
            ast::UntypedDefinition::ModuleConstant(ast::ModuleConstant {
                doc: None,
                location: span,
                public: public.is_some(),
                name,
                annotation,
                value: Box::new(value),
                tipo: (),
            })
        })
}
