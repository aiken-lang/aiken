use chumsky::prelude::*;

use crate::{ast, error::ParseError, token::Token};

pub fn module_parser(
    kind: ast::ModuleKind,
) -> impl Parser<Token, ast::UntypedModule, Error = ParseError> {
    let unqualified_import = choice((
        select! {Token::Name { name } => name}.then(
            just(Token::As)
                .ignore_then(select! {Token::Name { name } => name})
                .or_not(),
        ),
        select! {Token::UpName { name } => name}.then(
            just(Token::As)
                .ignore_then(select! {Token::UpName { name } => name})
                .or_not(),
        ),
    ))
    .map_with_span(|(name, as_name), span| ast::UnqualifiedImport {
        name,
        location: span,
        as_name,
        layer: Default::default(),
    });

    let unqualified_imports = just(Token::Dot)
        .ignore_then(
            unqualified_import
                .separated_by(just(Token::Comma))
                .delimited_by(just(Token::LeftBrace), just(Token::RightBrace)),
        )
        .or_not();

    let as_name = just(Token::As)
        .ignore_then(select! {Token::Name { name } => name})
        .or_not();

    let module_path = select! {Token::Name { name } => name}
        .separated_by(just(Token::Slash))
        .then(unqualified_imports)
        .then(as_name);

    let import = just(Token::Use).ignore_then(module_path).map_with_span(
        |((module, unqualified), as_name), span| ast::UntypedDefinition::Use {
            module,
            as_name,
            unqualified: unqualified.unwrap_or_default(),
            package: (),
            location: span,
        },
    );

    choice((import,))
        .repeated()
        .then_ignore(end())
        .map(move |definitions| ast::UntypedModule {
            kind,
            definitions,
            docs: vec![],
            name: vec![],
            type_info: (),
        })
}

#[cfg(test)]
mod tests {
    use chumsky::prelude::*;

    use crate::{
        ast::{self, Span, SrcId},
        lexer,
        parser::module_parser,
    };

    #[test]
    fn simple() {
        let code = r#"
            use std/list
            use std/address.{Address as A, thing as w}
            use std/tx as t
        "#;
        let len = code.chars().count();

        let span = |i| Span::new(SrcId::empty(), i..i + 1);

        let tokens = lexer::lexer()
            .parse(chumsky::Stream::from_iter(
                span(len),
                code.chars().enumerate().map(|(i, c)| (c, span(i))),
            ))
            .unwrap();

        dbg!(tokens.clone());

        let res = module_parser(ast::ModuleKind::Script)
            .parse(chumsky::Stream::from_iter(span(len), tokens.into_iter()))
            .unwrap();

        assert_eq!(
            res,
            ast::UntypedModule {
                docs: vec![],
                kind: ast::ModuleKind::Script,
                name: vec![],
                type_info: (),
                definitions: vec![
                    ast::UntypedDefinition::Use {
                        location: Span::new(SrcId::empty(), 13..25),
                        module: vec!["std".to_string(), "list".to_string()],
                        as_name: None,
                        unqualified: vec![],
                        package: (),
                    },
                    ast::UntypedDefinition::Use {
                        location: Span::new(SrcId::empty(), 38..80),
                        module: vec!["std".to_string(), "address".to_string()],
                        as_name: None,
                        unqualified: vec![
                            ast::UnqualifiedImport {
                                as_name: Some("A".to_string()),
                                location: Span::new(SrcId::empty(), 55..67),
                                layer: Default::default(),
                                name: "Address".to_string()
                            },
                            ast::UnqualifiedImport {
                                as_name: Some("w".to_string()),
                                location: Span::new(SrcId::empty(), 69..79),
                                layer: Default::default(),
                                name: "thing".to_string()
                            }
                        ],
                        package: (),
                    },
                    ast::UntypedDefinition::Use {
                        location: Span::new(SrcId::empty(), 93..108),
                        module: vec!["std".to_string(), "tx".to_string()],
                        as_name: Some("t".to_string()),
                        unqualified: vec![],
                        package: (),
                    }
                ]
            },
            "{:#?}",
            res,
        );
    }
}
