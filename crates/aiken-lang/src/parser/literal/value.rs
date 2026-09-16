use crate::expr::UntypedExpr;
use chumsky::prelude::*;
use num_bigint::BigInt;
use std::sync::Arc;
use uplc::ast::{VALUE_MAX_KEY_LEN, Value, ValueEntries, ValueError};

use crate::{
    ast::{ByteArrayFormatPreference, Span},
    expr::{ValueEntrySpans, ValueLiteralSpans, ValueTokenSpans},
    parser::{error::ParseError, literal::int, token::Token},
};

const POLICY_ID_LENGTH: usize = 28;

struct ParsedAsset {
    name: Vec<u8>,
    preferred_format: ByteArrayFormatPreference,
    quantity: BigInt,
    location: Span,
    name_location: Span,
    quantity_location: Span,
}

struct ParsedPolicy {
    id: Vec<u8>,
    assets: Vec<ParsedAsset>,
    location: Span,
    id_location: Span,
    assets_location: Span,
}

pub fn parser() -> impl Parser<Token, UntypedExpr, Error = ParseError> {
    let hexadecimal_key = just(Token::Hash)
        .ignore_then(select! { Token::ByteString { value } => value }.validate(
            |value, span, emit| match hex::decode(value) {
                Ok(bytes) => (bytes, true),
                Err(_) => {
                    emit(ParseError::malformed_base16_string_literal(span));
                    (vec![], false)
                }
            },
        ))
        .map_with_span(|bytes, location| (bytes, location));

    let policy_id = hexadecimal_key
        .clone()
        .validate(|((bytes, is_valid), location), _, emit| {
            if is_valid && bytes.len() != POLICY_ID_LENGTH {
                emit(ParseError::invalid_value_literal(
                    location,
                    Some(format!("should have {} hex-digits", POLICY_ID_LENGTH * 2)),
                    format!(
                        "Value script hash must be exactly {POLICY_ID_LENGTH} bytes: got {} bytes",
                        bytes.len(),
                    ),
                ));
            }

            (bytes, location)
        });

    let asset_name = choice((
        hexadecimal_key.map(|((bytes, is_valid), location)| {
            (
                (bytes, is_valid),
                location,
                ByteArrayFormatPreference::HexadecimalString,
            )
        }),
        select! { Token::ByteString { value } => value.into_bytes() }.map_with_span(
            |bytes, location| {
                (
                    (bytes, true),
                    location,
                    ByteArrayFormatPreference::Utf8String,
                )
            },
        ),
    ))
    .validate(|((bytes, is_valid), location, preferred_format), _, emit| {
        if is_valid && bytes.len() > VALUE_MAX_KEY_LEN {
            emit(ParseError::invalid_value_literal(
                location,
                Some("too long".to_string()),
                ValueError::KeyTooLong(bytes.len()).to_string(),
            ));
        }

        (bytes, location, preferred_format)
    });

    let quantity = int()
        .map(|(value, _)| {
            BigInt::parse_bytes(value.as_bytes(), 10)
                .expect("the lexer must produce valid integer strings")
        })
        .map_with_span(|quantity, location| (quantity, location));

    let asset = asset_name
        .then_ignore(just(Token::Colon))
        .then(quantity)
        .map_with_span(
            |((name, name_location, preferred_format), (quantity, quantity_location)), location| {
                ParsedAsset {
                    name,
                    preferred_format,
                    quantity,
                    location,
                    name_location,
                    quantity_location,
                }
            },
        );

    let assets = asset
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .delimited_by(just(Token::LeftBrace), just(Token::RightBrace))
        .map_with_span(|assets, location| (assets, location));

    let policy = choice((
        policy_id,
        select! { Token::ByteString { value } => value }.validate(|_value, span, emit| {
            emit(ParseError::missing_pound_sign(span));
            (vec![], span)
        }),
    ))
    .then_ignore(just(Token::Colon))
    .then(assets)
    .map_with_span(
        |((id, id_location), (assets, assets_location)), location| ParsedPolicy {
            id,
            assets,
            location,
            id_location,
            assets_location,
        },
    );

    policy
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .delimited_by(just(Token::LeftBrace), just(Token::RightBrace))
        .validate(|mut policies, location, emit| {
            for policy in &mut policies {
                policy
                    .assets
                    .sort_by(|left, right| left.name.cmp(&right.name));
            }
            policies.sort_by(|left, right| left.id.cmp(&right.id));

            let (entries, mut entry_spans): (Vec<_>, Vec<_>) = policies
                .into_iter()
                .map(|policy| {
                    let (assets, asset_spans): (Vec<_>, Vec<_>) = policy
                        .assets
                        .into_iter()
                        .map(|asset| {
                            (
                                (asset.name, asset.quantity),
                                ValueTokenSpans {
                                    entry: asset.location,
                                    asset_name: asset.name_location,
                                    quantity: asset.quantity_location,
                                    preferred_format: asset.preferred_format,
                                },
                            )
                        })
                        .unzip();

                    (
                        (policy.id, assets),
                        ValueEntrySpans {
                            entry: policy.location,
                            policy: policy.id_location,
                            assets: policy.assets_location,
                            asset_entries: asset_spans,
                        },
                    )
                })
                .unzip();

            let value = match Value::from_canonical_entries(entries) {
                Ok(value) => Arc::new(value.into_entries()),
                Err(error) => {
                    emit(ParseError::invalid_value_literal(
                        location,
                        None,
                        error.to_string(),
                    ));
                    entry_spans.clear();
                    Arc::new(ValueEntries::new())
                }
            };

            (
                value,
                ValueLiteralSpans {
                    list: location,
                    entries: entry_spans,
                },
            )
        })
        .map_with_span(|(value, spans), location| UntypedExpr::Value {
            location,
            value,
            spans,
        })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        expr::UntypedExpr,
        parser::{
            error::ErrorKind,
            lexer::{self, LexInfo},
        },
    };

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

    fn policy(byte: &str) -> String {
        byte.repeat(POLICY_ID_LENGTH)
    }

    fn invalid_value_reason(source: &str) -> String {
        parse(source)
            .expect_err("Value literal should be rejected")
            .into_iter()
            .find_map(|error| match *error.kind {
                ErrorKind::InvalidValueLiteral { reason } => Some(reason),
                _ => None,
            })
            .unwrap_or_else(|| panic!("expected an invalid Value literal error for {source}"))
    }

    #[test]
    fn parses_empty_value_literal() {
        assert!(entries("{}").is_empty());
    }

    #[test]
    fn parses_and_canonicalizes_value_literal() {
        let first_policy = policy("11");
        let second_policy = policy("22");
        let source = format!(
            r#"{{
              #"{second_policy}": {{
                "é": -0x2a,
                #"": 14_000,
              }},
              #"{first_policy}": {{
                "": 1,
              }},
            }}"#,
        );

        assert_eq!(
            entries(&source).as_ref(),
            &vec![
                (vec![0x11; POLICY_ID_LENGTH], vec![(vec![], 1)]),
                (
                    vec![0x22; POLICY_ID_LENGTH],
                    vec![(vec![], 14_000), ("é".as_bytes().to_vec(), -42)],
                ),
            ],
        );
    }

    #[test]
    fn accepts_asset_names_at_the_32_byte_limit() {
        let first_policy = policy("11");
        let second_policy = policy("22");
        let utf8_name = "é".repeat(16);
        let hexadecimal_name = "ff".repeat(VALUE_MAX_KEY_LEN);
        let source = format!(
            r#"{{
              #"{first_policy}": {{ "{utf8_name}": 1 }},
              #"{second_policy}": {{ #"{hexadecimal_name}": 2 }},
            }}"#,
        );

        assert_eq!(entries(&source).len(), 2);
    }

    #[test]
    fn rejects_policy_ids_that_are_not_28_bytes() {
        for length in [POLICY_ID_LENGTH - 1, POLICY_ID_LENGTH + 1] {
            let source = format!(r#"{{ #"{}": {{ "": 1 }} }}"#, "11".repeat(length));
            let reason = invalid_value_reason(&source);

            assert!(
                reason.contains(&format!("exactly {POLICY_ID_LENGTH} bytes")),
                "unexpected error: {reason}",
            );
        }
    }

    #[test]
    fn rejects_empty_asset_maps() {
        let source = format!(r#"{{ #"{}": {{}} }}"#, policy("11"));

        assert_eq!(
            invalid_value_reason(&source),
            ValueError::EmptyInnerMap.to_string(),
        );
    }

    #[test]
    fn leaves_non_value_braces_to_the_block_parser() {
        assert!(matches!(
            parse("{ foo }").expect("block expression should parse"),
            UntypedExpr::Var { name, .. } if name == "foo"
        ));
    }

    #[test]
    fn rejects_asset_names_over_32_bytes() {
        let utf8_name = "é".repeat(17);
        let hexadecimal_name = "ff".repeat(VALUE_MAX_KEY_LEN + 1);

        for name in [
            format!(r#""{utf8_name}""#),
            format!(r#"#"{hexadecimal_name}""#),
        ] {
            let source = format!(r#"{{ #"{}": {{ {name}: 1 }} }}"#, policy("11"));
            let reason = invalid_value_reason(&source);

            assert!(
                reason.contains("exceeds maximum length"),
                "unexpected error: {reason}"
            );
        }
    }

    #[test]
    fn rejects_duplicate_policy_ids() {
        let policy = policy("11");
        let source = format!(
            r#"{{
              #"{policy}": {{ "first": 1 }},
              #"{policy}": {{ "second": 2 }},
            }}"#,
        );

        assert_eq!(
            invalid_value_reason(&source),
            ValueError::CurrencySymbolsNotStrictlyAscending.to_string(),
        );
    }

    #[test]
    fn rejects_duplicate_asset_names_across_encodings() {
        let source = format!(
            r#"{{
              #"{}": {{
                "foo": 1,
                #"666f6f": 2,
              }},
            }}"#,
            policy("11"),
        );

        assert_eq!(
            invalid_value_reason(&source),
            ValueError::TokenNamesNotStrictlyAscending.to_string(),
        );
    }

    #[test]
    fn rejects_malformed_hexadecimal_keys() {
        let malformed_policy = format!(r#"{{ #"{}f": {{ "": 1 }} }}"#, policy("11"));
        let malformed_asset = format!(r#"{{ #"{}": {{ #"abcdef01234": 1 }} }}"#, policy("11"),);

        for source in [malformed_policy, malformed_asset] {
            let errors = parse(&source).expect_err("malformed key should be rejected");
            assert!(errors.iter().any(|error| {
                matches!(error.kind.as_ref(), ErrorKind::MalformedBase16StringLiteral)
            }));
        }
    }
}
