use chumsky::prelude::*;
use num_bigint::BigInt;
use std::sync::Arc;
use uplc::ast::Value;

use crate::{
    ast::well_known,
    expr::{UntypedExpr, ValueEntrySpans, ValueLiteralSpans, ValueTokenSpans},
    parser::{error::ParseError, literal::int, token::Token},
};

pub fn parser() -> impl Parser<Token, UntypedExpr, Error = ParseError> {
    let byte_array = just(Token::Hash)
        .ignore_then(select! { Token::ByteString { value } => value }.validate(
            |value, span, emit| match hex::decode(value) {
                Ok(bytes) => bytes,
                Err(_) => {
                    emit(ParseError::malformed_base16_string_literal(span));
                    vec![]
                }
            },
        ))
        .map_with_span(|bytes, span| (bytes, span));

    let quantity = int()
        .map(|(value, _)| {
            BigInt::parse_bytes(value.as_bytes(), 10)
                .expect("the lexer must produce valid base-10 integer strings")
        })
        .map_with_span(|quantity, span| (quantity, span));

    let inner_entry = byte_array
        .clone()
        .then_ignore(just(Token::Comma))
        .then(quantity)
        .delimited_by(left_paren(), just(Token::RightParen))
        .map_with_span(|((token, token_span), (quantity, quantity_span)), tuple| {
            (
                (token, quantity),
                ValueTokenSpans {
                    tuple,
                    token: token_span,
                    quantity: quantity_span,
                },
            )
        });

    let inner = inner_entry
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .delimited_by(just(Token::LeftSquare), just(Token::RightSquare))
        .map_with_span(|entries, token_list| {
            let (entries, spans) = entries.into_iter().unzip();
            (entries, token_list, spans)
        });

    let outer_entry = byte_array
        .then_ignore(just(Token::Comma))
        .then(inner)
        .delimited_by(left_paren(), just(Token::RightParen))
        .map_with_span(
            |((currency, currency_span), (tokens, token_list, token_spans)), tuple| {
                (
                    (currency, tokens),
                    ValueEntrySpans {
                        tuple,
                        currency: currency_span,
                        token_list,
                        tokens: token_spans,
                    },
                )
            },
        );

    just(Token::Hash)
        .then_ignore(just(Token::Less))
        .then_ignore(select! { Token::UpName { name } if name == well_known::VALUE => name })
        .then_ignore(just(Token::Greater))
        .ignore_then(
            outer_entry
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .delimited_by(just(Token::LeftSquare), just(Token::RightSquare))
                .map_with_span(|entries, list| {
                    let (entries, spans) = entries.into_iter().unzip();
                    (
                        entries,
                        ValueLiteralSpans {
                            list,
                            entries: spans,
                        },
                    )
                }),
        )
        .validate(|(entries, mut spans), location, emit| {
            let value = match Value::from_canonical_entries(entries) {
                Ok(value) => value,
                Err(error) => {
                    emit(ParseError::invalid_value_literal(
                        location,
                        error.to_string(),
                    ));
                    spans.entries.clear();
                    Value::empty()
                }
            };

            UntypedExpr::Value {
                location,
                value: Arc::new(value.into_entries()),
                spans,
            }
        })
}

fn left_paren() -> impl Parser<Token, (), Error = ParseError> + Clone {
    choice((just(Token::LeftParen), just(Token::NewLineLeftParen))).ignored()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ast::Span,
        parser::{
            error::ErrorKind,
            lexer::{self, LexInfo},
        },
    };
    use chumsky::Parser;
    use uplc::ast::{ValueEntries, ValueError};

    fn parse(source: &str) -> Result<UntypedExpr, Vec<ParseError>> {
        let LexInfo { tokens, .. } = lexer::run(source)?;
        let stream = chumsky::Stream::from_iter(Span::create(tokens.len(), 1), tokens.into_iter());

        crate::parser::expr::sequence()
            .then_ignore(end())
            .parse(stream)
    }

    fn entries(source: &str) -> Arc<ValueEntries> {
        match parse(source).expect("Value literal should parse") {
            UntypedExpr::Value { value, .. } => value,
            expression => panic!("expected a Value literal, got {expression:#?}"),
        }
    }

    #[test]
    fn parses_empty_value_literal() {
        assert!(entries("#<Value>[]").is_empty());
    }

    #[test]
    fn preserves_canonical_entries_in_populated_value_literal() {
        let parsed = entries(
            r#"
            #<Value>[
              (#"", [
                (#"00", -1),
                (#"ff", 2),
              ]),
              (#"aa", [
                (#"", 3),
                (#"bb", -42),
              ]),
            ]
            "#,
        );

        assert_eq!(
            parsed.as_ref(),
            &vec![
                (vec![], vec![(vec![0x00], -1), (vec![0xff], 2)]),
                (vec![0xaa], vec![(vec![], 3), (vec![0xbb], -42)]),
            ],
        );
    }

    #[test]
    fn rejects_non_canonical_or_out_of_bounds_value_literals() {
        let upper_overflow =
            BigInt::parse_bytes(b"170141183460469231731687303715884105728", 10).unwrap();
        let lower_overflow =
            BigInt::parse_bytes(b"-170141183460469231731687303715884105729", 10).unwrap();
        let cases = vec![
            (
                "currency symbols out of order",
                r#"#<Value>[(#"bb", [(#"00", 1)]), (#"aa", [(#"00", 2)])]"#.to_string(),
                ValueError::CurrencySymbolsNotStrictlyAscending,
            ),
            (
                "duplicate currency symbol",
                r#"#<Value>[(#"aa", [(#"00", 1)]), (#"aa", [(#"01", 2)])]"#.to_string(),
                ValueError::CurrencySymbolsNotStrictlyAscending,
            ),
            (
                "token names out of order",
                r#"#<Value>[(#"aa", [(#"bb", 1), (#"00", 2)])]"#.to_string(),
                ValueError::TokenNamesNotStrictlyAscending,
            ),
            (
                "duplicate token name",
                r#"#<Value>[(#"aa", [(#"bb", 1), (#"bb", 2)])]"#.to_string(),
                ValueError::TokenNamesNotStrictlyAscending,
            ),
            (
                "empty inner map",
                r#"#<Value>[(#"aa", [])]"#.to_string(),
                ValueError::EmptyInnerMap,
            ),
            (
                "zero quantity",
                r#"#<Value>[(#"aa", [(#"bb", 0)])]"#.to_string(),
                ValueError::ZeroQuantity,
            ),
            (
                "overlong key",
                format!("#<Value>[(#\"{}\", [(#\"\", 1)])]", "00".repeat(33)),
                ValueError::KeyTooLong(33),
            ),
            (
                "quantity above i128::MAX",
                format!(r#"#<Value>[(#"aa", [(#"bb", {upper_overflow})])]"#),
                ValueError::QuantityOutOfBounds(upper_overflow),
            ),
            (
                "quantity below i128::MIN",
                format!(r#"#<Value>[(#"aa", [(#"bb", {lower_overflow})])]"#),
                ValueError::QuantityOutOfBounds(lower_overflow),
            ),
        ];

        for (case, source, expected) in cases {
            let errors = parse(&source).expect_err(case);
            let reason = errors
                .iter()
                .find_map(|error| match error.kind.as_ref() {
                    ErrorKind::InvalidValueLiteral { reason } => Some(reason),
                    _ => None,
                })
                .unwrap_or_else(|| panic!("{case}: expected InvalidValueLiteral, got {errors:#?}"));

            assert_eq!(reason, &expected.to_string(), "{case}");
        }
    }
}
