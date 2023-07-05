use chumsky::prelude::*;

use crate::{
    ast,
    parser::{annotation, error::ParseError, token::Token, utils},
};

pub fn parser() -> impl Parser<Token, ast::UntypedDefinition, Error = ParseError> {
    utils::optional_flag(Token::Pub)
        .then(utils::type_name_with_args())
        .then_ignore(just(Token::Equal))
        .then(annotation())
        .map_with_span(|((public, (alias, parameters)), annotation), span| {
            ast::UntypedDefinition::TypeAlias(ast::TypeAlias {
                alias,
                annotation,
                doc: None,
                location: span,
                parameters: parameters.unwrap_or_default(),
                public,
                tipo: (),
            })
        })
}

#[cfg(test)]
mod tests {
    use crate::assert_definition;

    #[test]
    fn type_alias_tuple() {
        assert_definition!(
            r#"
            type RoyaltyToken = (PolicyId, AssetName)"#
        );
    }

    #[test]
    fn type_alias_basic() {
        assert_definition!(
            r#"
            type Thing = Int"#
        );
    }

    #[test]
    fn type_alias_pub() {
        assert_definition!(
            r#"
            pub type Me = String"#
        );
    }
}
