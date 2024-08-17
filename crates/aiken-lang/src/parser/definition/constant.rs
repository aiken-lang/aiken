use crate::{
    ast,
    parser::{annotation, error::ParseError, expr::pure_expression, token::Token, utils},
};
use chumsky::prelude::*;

pub fn parser() -> impl Parser<Token, ast::UntypedDefinition, Error = ParseError> {
    utils::optional_flag(Token::Pub)
        .then_ignore(just(Token::Const))
        .then(select! {Token::Name{name} => name})
        .then(
            just(Token::Colon)
                .ignore_then(annotation::parser())
                .or_not(),
        )
        .then_ignore(just(Token::Equal))
        .then(recursive(|sequence| {
            recursive(|expression| pure_expression(sequence.clone(), expression))
                .then(sequence.repeated())
                .foldl(|current, next| current.append_in_sequence(next))
        }))
        .map_with_span(|(((public, name), annotation), value), span| {
            ast::UntypedDefinition::ModuleConstant(ast::ModuleConstant {
                doc: None,
                location: span,
                public,
                name,
                annotation,
                value,
            })
        })
}

#[cfg(test)]
mod tests {
    use crate::assert_definition;

    #[test]
    fn g1_element_constant() {
        assert_definition!(
            r#"
            pub const point =
              #<Bls12_381, G1>"950dfd33da2682260c76038dfb8bad6e84ae9d599a3c151815945ac1e6ef6b1027cd917f3907479d20d636ce437a41f5"
            "#
        );
    }

    #[test]
    fn g2_element_constant() {
        assert_definition!(
            r#"
            pub const point =
              #<Bls12_381, G2>"b0629fa1158c2d23a10413fe91d381a84d25e31d041cd0377d25828498fd02011b35893938ced97535395e4815201e67108bcd4665e0db25d602d76fa791fab706c54abf5e1a9e44b4ac1e6badf3d2ac0328f5e30be341677c8bac5dda7682f1"
            "#
        );
    }
}
