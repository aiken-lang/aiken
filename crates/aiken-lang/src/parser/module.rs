use chumsky::prelude::*;

use crate::ast::UntypedDefinition;

use super::{definitions, error::ParseError, token::Token};

pub fn parser() -> impl Parser<Token, Vec<UntypedDefinition>, Error = ParseError> {
    choice((
        definitions::import(),
        definitions::data_type(),
        definitions::type_alias(),
        definitions::validator(),
        definitions::function(),
        definitions::test(),
        definitions::constant(),
    ))
    .repeated()
    .then_ignore(end())
}
