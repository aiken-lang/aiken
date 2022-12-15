use chumsky::prelude::*;

use crate::{ast::Span, parser::lexer, parser::token::Token};

#[test]
fn tokens() {
    let code = "pub type |> >=\n{ Thing _na_thing name";
    let len = code.chars().count();

    let span = |i| Span::new((), i..i + 1);

    assert_eq!(
        lexer::lexer()
            .parse(chumsky::Stream::from_iter(
                span(len),
                code.chars().enumerate().map(|(i, c)| (c, span(i))),
            ))
            .map(|tokens| tokens.into_iter().map(|(tok, _)| tok).collect::<Vec<_>>()),
        Ok(vec![
            Token::Pub,
            Token::Type,
            Token::Pipe,
            Token::GreaterEqual,
            Token::NewLine,
            Token::LeftBrace,
            Token::UpName {
                name: "Thing".to_string()
            },
            Token::DiscardName {
                name: "_na_thing".to_string()
            },
            Token::Name {
                name: "name".to_string()
            }
        ]),
    );
}
