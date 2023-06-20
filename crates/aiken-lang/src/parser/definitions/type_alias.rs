use chumsky::prelude::*;

use crate::{
    ast,
    parser::{error::ParseError, token::Token, utils},
};

pub fn parser() -> impl Parser<Token, ast::UntypedDefinition, Error = ParseError> {
    utils::public()
        .or_not()
        .then(type_name_with_args())
        .then_ignore(just(Token::Equal))
        .then(type_parser())
        .map_with_span(|((opt_pub, (alias, parameters)), annotation), span| {
            ast::UntypedDefinition::TypeAlias(ast::TypeAlias {
                alias,
                annotation,
                doc: None,
                location: span,
                parameters: parameters.unwrap_or_default(),
                public: opt_pub.is_some(),
                tipo: (),
            })
        })
}
