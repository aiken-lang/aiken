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

#[test]
fn assurance_region_is_opaque_trivia() {
    let source = "//@ arbitrary \"unterminated string ! { @// fn after() { 1 }";
    let info = lexer::run(source).expect("annotation contents must not be lexed");

    assert_eq!(
        info.tokens.first().map(|(token, _)| token),
        Some(&Token::Fn)
    );
    assert!(info.extra.comments.is_empty());
}

#[test]
fn assurance_region_closer_boundaries() {
    for boundary in [
        " ", "\t", "\n", "\r\n", "\r", "\u{00a0}", "\u{2003}", "\u{2028}",
    ] {
        let region = format!("//@ opaque{boundary}@//");
        let source = format!("{region} fn after() {{ 1 }}");
        let info = lexer::run(&source).expect("whitespace permits a closer");

        assert_eq!(
            info.extra.assurance_comments,
            vec![Span::new((), 0..region.len())]
        );
        assert_eq!(
            info.tokens.first().map(|(token, _)| token),
            Some(&Token::Fn)
        );
    }

    for prefix in ["", "word", ".", "/", "\u{200b}", "é"] {
        let region = format!("//@{prefix}@// still opaque\n@//");
        let source = format!("{region} fn after() {{ 1 }}");
        let info = lexer::run(&source).expect("non-boundary closers remain content");

        assert_eq!(
            info.extra.assurance_comments,
            vec![Span::new((), 0..region.len())]
        );
        assert_eq!(
            info.tokens.first().map(|(token, _)| token),
            Some(&Token::Fn)
        );
    }
}

#[test]
fn assurance_region_uses_first_eligible_closer() {
    let source = "//@ first @// // ordinary @//\n//@ second\n@//";
    let info = lexer::run(source).expect("both regions should lex");
    let regions = info.extra.assurance_comments(source).collect::<Vec<_>>();

    assert_eq!(regions.len(), 2);
    assert_eq!(regions[0].content, "//@ first @//");
    assert_eq!(regions[1].content, "//@ second\n@//");
    assert_eq!(info.extra.comments.len(), 1);
    assert!(info.tokens.is_empty());
}

#[test]
fn assurance_region_borrows_delimiter_inclusive_unicode_bytes() {
    let source = "@\"é\" //@ λ\r\n\r\n未知 kind\t@// 42";
    let start = source.find("//@").unwrap();
    let end = source.find("@//").unwrap() + 3;
    let info = lexer::run(source).expect("arbitrary annotation text should lex");

    assert_eq!(
        info.extra.assurance_comments,
        vec![Span::new((), start..end)]
    );
    let region = info.extra.assurance_comments(source).next().unwrap();
    assert_eq!(region.start, start);
    assert_eq!(region.content, &source[start..end]);
    assert_eq!(region.content.as_ptr(), source[start..end].as_ptr());
    assert!(info.extra.empty_lines.is_empty());
    assert_eq!(info.tokens.len(), 2);
    assert_eq!(info.tokens[1].1.start, end + 1);
}

#[test]
fn assurance_markers_inside_strings_and_line_comments_are_not_regions() {
    let source = concat!(
        "@\"//@ string @//\" \"//@ bytes @//\"\n",
        "// ordinary //@ marker @//\n",
        "/// doc //@ marker @//\n",
        "//// module //@ marker @//\n",
    );
    let info = lexer::run(source).expect("existing string and comment syntax stays unchanged");

    assert!(info.extra.assurance_comments.is_empty());
    assert_eq!(info.extra.comments.len(), 1);
    assert_eq!(info.extra.doc_comments.len(), 1);
    assert_eq!(info.extra.module_comments.len(), 1);
    assert_eq!(
        info.tokens
            .iter()
            .map(|(token, _)| token.clone())
            .collect::<Vec<_>>(),
        vec![
            Token::String {
                value: "//@ string @//".into()
            },
            Token::ByteString {
                value: "//@ bytes @//".into()
            },
        ]
    );
}

#[test]
fn assurance_region_nested_opener_diagnostic_uses_inner_byte_span() {
    use crate::parser::error::ErrorKind;
    use miette::Diagnostic;

    let source = "@\"é\" //@ λ\n//@ inner @//";
    let start = source.rfind("//@").unwrap();
    let errors = lexer::run(source).err().expect("nested regions must fail");

    assert_eq!(errors.len(), 1);
    assert_eq!(*errors[0].kind, ErrorKind::NestedAssuranceComment);
    let label = errors[0].labels().unwrap().next().unwrap();
    assert_eq!(label.offset(), start);
    assert_eq!(label.len(), 3);
}

#[test]
fn assurance_region_unterminated_diagnostic_runs_from_opener_to_eof() {
    use crate::parser::error::ErrorKind;
    use miette::Diagnostic;

    for region in ["//@", "//@ λ", "//@ text@//", "//@ λ\n\n"] {
        let prefix = "@\"é\" ";
        let source = format!("{prefix}{region}");
        let errors = lexer::run(&source)
            .err()
            .expect("unclosed regions must fail");

        assert_eq!(errors.len(), 1);
        assert_eq!(*errors[0].kind, ErrorKind::UnterminatedAssuranceComment);
        let label = errors[0].labels().unwrap().next().unwrap();
        assert_eq!(label.offset(), prefix.len());
        assert_eq!(label.len(), region.len());
    }
}

#[test]
fn assurance_region_is_parser_trivia_at_module_and_expression_positions() {
    let source = concat!(
        "//@ before @// fn answer() { ",
        "let x = //@ expression\n@// 42\n",
        "x //@ after expression @// }\n",
        "//@ trailing @//",
    );
    let (module, extra) = crate::parser::module(source, crate::ast::ModuleKind::Lib)
        .expect("annotations must be transparent to the parser");

    assert_eq!(module.definitions.len(), 1);
    assert_eq!(extra.assurance_comments.len(), 4);
}

#[test]
fn assurance_region_metadata_defaults_when_deserializing_older_modules() {
    use serde::{
        Deserialize,
        de::value::{Error, MapDeserializer, SeqDeserializer},
    };

    let fields = ["module_comments", "doc_comments", "comments", "empty_lines"]
        .into_iter()
        .map(|name| {
            (
                name,
                SeqDeserializer::<_, Error>::new(std::iter::empty::<usize>()),
            )
        });
    let extra = crate::parser::extra::ModuleExtra::deserialize(MapDeserializer::new(fields))
        .expect("previously serialized module extras remain readable");

    assert!(extra.assurance_comments.is_empty());
}
