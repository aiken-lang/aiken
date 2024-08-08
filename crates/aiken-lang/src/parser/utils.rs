use super::{error::ParseError, token::Token};
use chumsky::prelude::*;

pub fn optional_flag(token: Token) -> impl Parser<Token, bool, Error = ParseError> {
    just(token).ignored().or_not().map(|v| v.is_some())
}

pub fn type_name_with_args() -> impl Parser<Token, (String, Option<Vec<String>>), Error = ParseError>
{
    just(Token::Type).ignore_then(
        select! {Token::UpName { name } => name}.then(
            select! {Token::Name { name } => name}
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .delimited_by(just(Token::Less), just(Token::Greater))
                .or_not(),
        ),
    )
}

#[macro_export]
macro_rules! assert_expr {
    ($code:expr) => {
        use chumsky::Parser;

        let $crate::parser::lexer::LexInfo { tokens, .. } = $crate::parser::lexer::run(indoc::indoc! { $code }).unwrap();

        let stream = chumsky::Stream::from_iter($crate::ast::Span::create(tokens.len(), 1), tokens.into_iter());

        let result = $crate::parser::expr::sequence().parse(stream);

        match result {
            Ok(expr) => {
                insta::with_settings!({
                    description => concat!("Code:\n\n", indoc::indoc! { $code }),
                    prepend_module_to_snapshot => false,
                    omit_expression => true
                }, {
                    insta::assert_debug_snapshot!(expr);
                })
            },
            Err(err) => {
                insta::with_settings!({
                    description => concat!("Invalid code (parse error):\n\n", indoc::indoc! { $code }),
                    prepend_module_to_snapshot => false,
                    omit_expression => true
                }, {
                    insta::assert_debug_snapshot!(err);
                })
            }
        }
    };
}

#[macro_export]
macro_rules! assert_annotation {
    ($code:expr) => {
        use chumsky::Parser;

        let $crate::parser::lexer::LexInfo { tokens, .. } = $crate::parser::lexer::run(indoc::indoc! { $code }).unwrap();

        let stream = chumsky::Stream::from_iter($crate::ast::Span::create(tokens.len(), 1), tokens.into_iter());

        let result = $crate::parser::annotation().parse(stream).unwrap();

        insta::with_settings!({
            description => concat!("Code:\n\n", indoc::indoc! { $code }),
            prepend_module_to_snapshot => false,
            omit_expression => true
        }, {
            insta::assert_debug_snapshot!(result);
        });
    };
}

#[macro_export]
macro_rules! assert_pattern {
    ($code:expr) => {
        use chumsky::Parser;

        let $crate::parser::lexer::LexInfo { tokens, .. } = $crate::parser::lexer::run(indoc::indoc! { $code }).unwrap();

        let stream = chumsky::Stream::from_iter($crate::ast::Span::create(tokens.len(), 1), tokens.into_iter());

        let result = $crate::parser::pattern().parse(stream).unwrap();

        insta::with_settings!({
            description => concat!("Code:\n\n", indoc::indoc! { $code }),
            prepend_module_to_snapshot => false,
            omit_expression => true
        }, {
            insta::assert_debug_snapshot!(result);
        });
    };
}

#[macro_export]
macro_rules! assert_module {
    ($code:expr) => {
        let (module, _) =
            $crate::parser::module(indoc::indoc!{ $code }, $crate::ast::ModuleKind::Validator).expect("Failed to parse code");

        insta::with_settings!({
            description => concat!("Code:\n\n", indoc::indoc! { $code }),
            prepend_module_to_snapshot => false,
            omit_expression => true
        }, {
            insta::assert_debug_snapshot!(module);
        });
    };
}

#[macro_export]
macro_rules! assert_definition {
    ($code:expr) => {
        use chumsky::Parser;

        let $crate::parser::lexer::LexInfo { tokens, .. } = $crate::parser::lexer::run(indoc::indoc! { $code }).unwrap();

        let stream = chumsky::Stream::from_iter($crate::ast::Span::create(tokens.len(), 1), tokens.into_iter());

        let result = $crate::parser::definition().parse(stream).unwrap();

        insta::with_settings!({
            description => concat!("Code:\n\n", indoc::indoc! { $code }),
            prepend_module_to_snapshot => false,
            omit_expression => true
        }, {
            insta::assert_debug_snapshot!(result);
        });
    };
}

#[macro_export]
macro_rules! assert_import {
    ($code:expr) => {
        use chumsky::Parser;

        let $crate::parser::lexer::LexInfo { tokens, .. } = $crate::parser::lexer::run(indoc::indoc! { $code }).unwrap();

        let stream = chumsky::Stream::from_iter($crate::ast::Span::create(tokens.len(), 1), tokens.into_iter());

        let result = $crate::parser::import().parse(stream).unwrap();

        insta::with_settings!({
            description => concat!("Code:\n\n", indoc::indoc! { $code }),
            prepend_module_to_snapshot => false,
            omit_expression => true
        }, {
            insta::assert_debug_snapshot!(result);
        });
    };
}

#[macro_export]
macro_rules! assert_format {
    ($code:expr) => {
        let src = indoc::indoc! { $code };

        let (module, extra) =
            $crate::parser::module(src, $crate::ast::ModuleKind::Lib).expect("Failed to parse code");

        let mut out = String::new();
        $crate::format::pretty(&mut out, module, extra, &src);

        insta::with_settings!({
            description => concat!("Code:\n\n", indoc::indoc! { $code }),
            prepend_module_to_snapshot => false,
            omit_expression => true
        }, {
            insta::assert_snapshot!(out);
        });

        // Check if formatting is imdepotent
        let (module2, extra2) = $crate::parser::module(&out, $crate::ast::ModuleKind::Lib).unwrap();
        let mut out2 = String::new();
        $crate::format::pretty(&mut out2, module2, extra2, &out);
        pretty_assertions::assert_eq!(out, out2, "formatting isn't idempotent");
    };
}
