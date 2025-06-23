use crate::{
    ast,
    parser::{annotation, error::ParseError, token::Token, utils},
};
use chumsky::prelude::*;

pub fn decorators() -> impl Parser<Token, Vec<ast::Decorator>, Error = ParseError> {
    let tag_value =
        select! { Token::Int { value, base } => ast::DecoratorKind::Tag { value, base } };
    let encoding_value = select! {
        Token::Name { name } if name == "list" => ast::DecoratorEncoding::List,
        Token::Name { name } if name == "constr" => ast::DecoratorEncoding::Constr
    }
    .map(ast::DecoratorKind::Encoding);

    just(Token::At)
        .ignore_then(choice((
            select! { Token::Name { name } if name == "tag" => name }.ignore_then(
                tag_value.delimited_by(just(Token::LeftParen), just(Token::RightParen)),
            ),
            select! { Token::Name { name } if name == "encoding" => name }.ignore_then(
                encoding_value.delimited_by(just(Token::LeftParen), just(Token::RightParen)),
            ),
        )))
        .map_with_span(|kind, span| ast::Decorator {
            kind,
            location: span,
        })
        .repeated()
        .or_not()
        .map(|t| t.unwrap_or_default())
}

pub fn parser() -> impl Parser<Token, ast::UntypedDefinition, Error = ParseError> {
    let unlabeled_constructor_type_args = decorators()
        .then(annotation())
        .map_with_span(|(d, annotation), span| ast::RecordConstructorArg {
            decorators: d,
            label: None,
            annotation,
            tipo: (),
            doc: None,
            location: span,
        })
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .delimited_by(just(Token::LeftParen), just(Token::RightParen));

    let constructors = decorators()
        .then(select! {Token::UpName { name } => name})
        .then(choice((labeled_args(), unlabeled_constructor_type_args)).or_not())
        .map_with_span(|((d, name), arguments), span| ast::RecordConstructor {
            decorators: d,
            location: span,
            arguments: arguments.unwrap_or_default(),
            name,
            doc: None,
            sugar: false,
        })
        .repeated()
        .delimited_by(just(Token::LeftBrace), just(Token::RightBrace));

    let record_sugar = labeled_args().map_with_span(|arguments, span| {
        vec![ast::RecordConstructor {
            decorators: vec![],
            location: span,
            arguments,
            doc: None,
            name: String::from("_replace"),
            sugar: true,
        }]
    });

    decorators()
        .then(utils::optional_flag(Token::Pub))
        .then(utils::optional_flag(Token::Opaque))
        .then(utils::type_name_with_args())
        .then(choice((constructors, record_sugar)))
        .map_with_span(
            |((((d, public), opaque), (name, parameters)), constructors), span| {
                ast::UntypedDefinition::DataType(ast::DataType {
                    decorators: d.clone(),
                    location: span,
                    constructors: if constructors.is_empty() {
                        vec![ast::RecordConstructor {
                            decorators: d,
                            location: span,
                            arguments: vec![],
                            doc: None,
                            name: name.clone(),
                            sugar: true,
                        }]
                    } else {
                        constructors
                            .into_iter()
                            .map(|mut constructor| {
                                if constructor.sugar {
                                    constructor.name.clone_from(&name);
                                    constructor.decorators = d.clone();
                                }

                                constructor
                            })
                            .collect()
                    },
                    doc: None,
                    name,
                    opaque,
                    parameters: parameters.unwrap_or_default(),
                    public,
                    typed_parameters: vec![],
                })
            },
        )
}

fn labeled_args() -> impl Parser<Token, Vec<ast::RecordConstructorArg<()>>, Error = ParseError> {
    decorators()
        .then(select! {Token::Name {name} => name})
        .then_ignore(just(Token::Colon))
        .then(annotation())
        .map_with_span(|((d, name), annotation), span| ast::RecordConstructorArg {
            decorators: d,
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

    #[test]
    fn record_sugar() {
        assert_definition!(
            r#"
            pub type Foo {
              wow: Int,
            }
            "#
        );
    }

    #[test]
    fn empty_record_sugar() {
        assert_definition!(
            r#"
            pub type Foo {
            }
            "#
        );
    }

    #[test]
    fn decorators_record_and_fields() {
        assert_definition!(
            r#"
            @tag(12)
            pub type User {
              name: ByteArray
            }
            "#
        );
    }

    #[test]
    fn decorators_record_encoding() {
        assert_definition!(
            r#"
            @encoding(list)
            pub type Thing {
              name: ByteArray
            }
            "#
        );
    }

    #[test]
    fn decorators_enum() {
        assert_definition!(
            r#"
            pub type L2Datum {
              @tag(117)
              Inactive

              @tag(118)
              BatchTxs(List<ByteArray>)

              @tag(1170)
              Committed(Bool, List<ByteArray>)

              @tag(1171)
              Withdrawn(Bool, ByteArray)
            }
            "#
        );
    }
}
