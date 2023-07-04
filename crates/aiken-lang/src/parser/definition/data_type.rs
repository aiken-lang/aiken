use chumsky::prelude::*;

use crate::{
    ast,
    parser::{annotation, error::ParseError, token::Token, utils},
};

pub fn parser() -> impl Parser<Token, ast::UntypedDefinition, Error = ParseError> {
    let unlabeled_constructor_type_args = annotation()
        .map_with_span(|annotation, span| ast::RecordConstructorArg {
            label: None,
            annotation,
            tipo: (),
            doc: None,
            location: span,
        })
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .delimited_by(just(Token::LeftParen), just(Token::RightParen));

    let constructors = select! {Token::UpName { name } => name}
        .then(
            choice((
                labeled_constructor_type_args(),
                unlabeled_constructor_type_args,
            ))
            .or_not(),
        )
        .map_with_span(|(name, arguments), span| ast::RecordConstructor {
            location: span,
            arguments: arguments.unwrap_or_default(),
            name,
            doc: None,
            sugar: false,
        })
        .repeated()
        .delimited_by(just(Token::LeftBrace), just(Token::RightBrace));

    let record_sugar = labeled_constructor_type_args().map_with_span(|arguments, span| {
        vec![ast::RecordConstructor {
            location: span,
            arguments,
            doc: None,
            name: String::from("_replace"),
            sugar: true,
        }]
    });

    utils::public()
        .then(just(Token::Opaque).ignored().or_not())
        .or_not()
        .then(utils::type_name_with_args())
        .then(choice((constructors, record_sugar)))
        .map_with_span(|((pub_opaque, (name, parameters)), constructors), span| {
            ast::UntypedDefinition::DataType(ast::DataType {
                location: span,
                constructors: constructors
                    .into_iter()
                    .map(|mut constructor| {
                        if constructor.sugar {
                            constructor.name = name.clone();
                        }

                        constructor
                    })
                    .collect(),
                doc: None,
                name,
                opaque: pub_opaque
                    .map(|(_, opt_opaque)| opt_opaque.is_some())
                    .unwrap_or(false),
                parameters: parameters.unwrap_or_default(),
                public: pub_opaque.is_some(),
                typed_parameters: vec![],
            })
        })
}

fn labeled_constructor_type_args(
) -> impl Parser<Token, Vec<ast::RecordConstructorArg<()>>, Error = ParseError> {
    select! {Token::Name {name} => name}
        .then_ignore(just(Token::Colon))
        .then(annotation())
        .map_with_span(|(name, annotation), span| ast::RecordConstructorArg {
            label: Some(name),
            annotation,
            tipo: (),
            doc: None,
            location: span,
        })
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .delimited_by(just(Token::LeftBrace), just(Token::RightBrace))
}

#[cfg(test)]
mod tests {
    use crate::assert_definition;

    #[test]
    fn custom_type() {
        assert_definition!(
            r#"
            type Option<a> {
              Some(a, Int)
              None
              Wow { name: Int, age: Int }
            }
            "#
        );
    }

    #[test]
    fn opaque_type() {
        assert_definition!(
            r#"
            pub opaque type User {
              name: _w
            }
            "#
        );
    }
}
