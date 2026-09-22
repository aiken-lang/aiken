use crate::expr::UntypedExpr;
use chumsky::prelude::*;
use num_bigint::BigInt;
use std::sync::Arc;
use uplc::ast::{VALUE_MAX_KEY_LEN, Value, ValueEntries, ValueError};

use crate::{
    ast::{ByteArrayFormatPreference, CallArg, Span},
    builtins::INSERT_VALUE,
    expr::{ValueEntrySpans, ValueLiteralSpans, ValueTokenSpans},
    parser::{
        error::ParseError,
        literal::int,
        token::{Base, Token},
    },
};

const POLICY_ID_LENGTH: usize = 28;
const LOVELACE: &str = "lovelace";

#[derive(Clone)]
enum ParsedKey {
    Literal {
        bytes: Vec<u8>,
        preferred_format: ByteArrayFormatPreference,
        location: Span,
    },
    Named {
        name: String,
        location: Span,
    },
}

impl ParsedKey {
    fn location(&self) -> Span {
        match self {
            Self::Literal { location, .. } | Self::Named { location, .. } => *location,
        }
    }

    fn into_expression(self) -> UntypedExpr {
        match self {
            Self::Literal {
                bytes,
                preferred_format,
                location,
            } => UntypedExpr::ByteArray {
                location,
                bytes: bytes.into_iter().map(|byte| (byte, location)).collect(),
                preferred_format,
            },
            Self::Named { name, location } => UntypedExpr::Var { name, location },
        }
    }
}

enum ParsedQuantity {
    Literal { value: BigInt, location: Span },
    Named { name: String, location: Span },
}

impl ParsedQuantity {
    fn location(&self) -> Span {
        match self {
            Self::Literal { location, .. } | Self::Named { location, .. } => *location,
        }
    }

    fn into_expression(self) -> UntypedExpr {
        match self {
            Self::Literal { value, location } => UntypedExpr::UInt {
                location,
                value: value.to_string(),
                base: Base::Decimal {
                    numeric_underscore: false,
                },
            },
            Self::Named { name, location } => UntypedExpr::Var { name, location },
        }
    }
}

struct ParsedAsset {
    name: ParsedKey,
    quantity: ParsedQuantity,
    location: Span,
}

struct ParsedPolicy {
    id: ParsedKey,
    assets: Vec<ParsedAsset>,
    location: Span,
    assets_location: Span,
}

struct ParsedInsertion {
    policy: ParsedKey,
    asset: ParsedKey,
    quantity: ParsedQuantity,
    policy_location: Span,
    asset_location: Span,
    assets_location: Span,
}

impl ParsedInsertion {
    fn apply(self, tail: UntypedExpr, literal_location: Span) -> UntypedExpr {
        let quantity_location = self.quantity.location();

        UntypedExpr::Call {
            location: literal_location,
            // Source variables always have a non-empty span. The empty span marks this callee as
            // compiler-generated so the typer can resolve the builtin without a user import.
            fun: Box::new(UntypedExpr::Var {
                location: Span::empty(),
                name: INSERT_VALUE.to_string(),
            }),
            arguments: vec![
                CallArg {
                    label: None,
                    location: self.policy_location,
                    value: self.policy.into_expression(),
                },
                CallArg {
                    label: None,
                    location: self.asset_location,
                    value: self.asset.into_expression(),
                },
                CallArg {
                    label: None,
                    location: quantity_location,
                    value: self.quantity.into_expression(),
                },
                CallArg {
                    label: None,
                    location: self.assets_location,
                    value: tail,
                },
            ],
        }
    }
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

            ParsedKey::Literal {
                bytes,
                preferred_format: ByteArrayFormatPreference::HexadecimalString,
                location,
            }
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

        ParsedKey::Literal {
            bytes,
            preferred_format,
            location,
        }
    });

    let named_policy_key = || {
        select! { Token::Name { name } if name != LOVELACE => name }
            .map_with_span(|name, location| ParsedKey::Named { name, location })
    };

    let named_key = || {
        select! { Token::Name { name } => name }
            .map_with_span(|name, location| ParsedKey::Named { name, location })
    };

    let quantity = || {
        choice((
            int()
                .map(|(value, _)| {
                    BigInt::parse_bytes(value.as_bytes(), 10)
                        .expect("the lexer must produce valid integer strings")
                })
                .map_with_span(|value, location| ParsedQuantity::Literal { value, location }),
            select! { Token::Name { name } => name }
                .map_with_span(|name, location| ParsedQuantity::Named { name, location }),
        ))
    };

    let asset = choice((asset_name, named_key()))
        .then_ignore(just(Token::Colon))
        .then(quantity())
        .map_with_span(|(name, quantity), location| ParsedAsset {
            name,
            quantity,
            location,
        });

    let assets = asset
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .delimited_by(just(Token::LeftBrace), just(Token::RightBrace))
        .map_with_span(|assets, location| (assets, location));

    let policy = choice((
        policy_id,
        select! { Token::ByteString { value } => value }.validate(|_value, span, emit| {
            emit(ParseError::missing_pound_sign(span));
            ParsedKey::Literal {
                bytes: vec![],
                preferred_format: ByteArrayFormatPreference::Utf8String,
                location: span,
            }
        }),
        named_policy_key(),
    ))
    .then_ignore(just(Token::Colon))
    .then(assets)
    .map_with_span(|(id, (assets, assets_location)), location| ParsedPolicy {
        id,
        assets,
        location,
        assets_location,
    });

    let lovelace = select! { Token::Name { name } if name == LOVELACE => () }
        .map_with_span(|(), location| location)
        .then_ignore(just(Token::Colon))
        .then(quantity())
        .map_with_span(|(name_location, quantity), location| {
            let quantity_location = quantity.location();

            ParsedPolicy {
                id: ParsedKey::Literal {
                    bytes: vec![],
                    preferred_format: ByteArrayFormatPreference::HexadecimalString,
                    location: name_location,
                },
                assets: vec![ParsedAsset {
                    name: ParsedKey::Literal {
                        bytes: vec![],
                        preferred_format: ByteArrayFormatPreference::HexadecimalString,
                        location: name_location,
                    },
                    quantity,
                    location,
                }],
                location,
                assets_location: quantity_location,
            }
        });

    choice((lovelace, policy))
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .delimited_by(just(Token::LeftBrace), just(Token::RightBrace))
        .validate(|policies, location, emit| {
            let mut static_policies = Vec::new();
            let mut insertions = Vec::new();

            for policy in policies {
                let ParsedPolicy {
                    id,
                    assets,
                    location: policy_location,
                    assets_location,
                } = policy;

                if assets.is_empty() {
                    emit(ParseError::invalid_value_literal(
                        policy_location,
                        None,
                        ValueError::EmptyInnerMap.to_string(),
                    ));
                    continue;
                }

                let static_policy_id = match &id {
                    ParsedKey::Literal { bytes, .. } => Some(bytes.clone()),
                    ParsedKey::Named { .. } => None,
                };
                let mut static_assets = Vec::new();

                for asset in assets {
                    match (static_policy_id.as_ref(), asset.name, asset.quantity) {
                        (
                            Some(_),
                            ParsedKey::Literal {
                                bytes,
                                preferred_format,
                                location: name_location,
                            },
                            ParsedQuantity::Literal {
                                value,
                                location: quantity_location,
                            },
                        ) => static_assets.push((
                            (bytes, value),
                            ValueTokenSpans {
                                entry: asset.location,
                                asset_name: name_location,
                                quantity: quantity_location,
                                preferred_format,
                            },
                        )),
                        (_, name, quantity) => insertions.push(ParsedInsertion {
                            policy: id.clone(),
                            asset: name,
                            quantity,
                            policy_location,
                            asset_location: asset.location,
                            assets_location,
                        }),
                    }
                }

                if !static_assets.is_empty() {
                    static_assets.sort_by(|left, right| left.0.0.cmp(&right.0.0));
                    let (assets, asset_spans) = static_assets.into_iter().unzip();

                    static_policies.push((
                        (
                            static_policy_id.expect("static assets require a literal policy"),
                            assets,
                        ),
                        ValueEntrySpans {
                            entry: policy_location,
                            policy: id.location(),
                            assets: assets_location,
                            asset_entries: asset_spans,
                        },
                    ));
                }
            }

            static_policies.sort_by(|left, right| left.0.0.cmp(&right.0.0));
            let (entries, mut entry_spans): (Vec<_>, Vec<_>) = static_policies.into_iter().unzip();

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

            let base = UntypedExpr::Value {
                location,
                value,
                spans: ValueLiteralSpans {
                    list: location,
                    entries: entry_spans,
                },
            };

            insertions
                .into_iter()
                .rev()
                .fold(base, |tail, insertion| insertion.apply(tail, location))
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
    fn parses_lovelace_entry() {
        assert_eq!(
            entries("{ lovelace: 42 }").as_ref(),
            &vec![(vec![], vec![(vec![], 42)])],
        );
    }

    #[test]
    fn parses_lovelace_mixed_with_custom_policies_in_any_order() {
        let custom_policy = policy("11");

        for source in [
            format!(
                r#"{{
                  lovelace: 42,
                  #"{custom_policy}": {{ "foo": 1 }},
                }}"#,
            ),
            format!(
                r#"{{
                  #"{custom_policy}": {{ "foo": 1 }},
                  lovelace: 42,
                }}"#,
            ),
        ] {
            assert_eq!(
                entries(&source).as_ref(),
                &vec![
                    (vec![], vec![(vec![], 42)]),
                    (vec![0x11; POLICY_ID_LENGTH], vec![(b"foo".to_vec(), 1)],),
                ],
            );
        }
    }

    #[test]
    fn named_value_fields_desugar_to_insert_value_with_exact_spans() {
        let source = "{ policy_1: { asset_1: quantity } }";
        let expression = parse(source).expect("Value literal should parse");
        let UntypedExpr::Call { fun, arguments, .. } = expression else {
            panic!("expected a dynamic Value literal to become a call");
        };
        let UntypedExpr::Var { name, location } = fun.as_ref() else {
            panic!("expected insert_value to be a variable");
        };
        let [policy, asset, quantity, tail] = arguments.as_slice() else {
            panic!("expected insert_value to have four arguments");
        };

        assert_eq!(name, INSERT_VALUE);
        assert_eq!(*location, Span::empty());

        for (argument, expected_name) in [
            (policy, "policy_1"),
            (asset, "asset_1"),
            (quantity, "quantity"),
        ] {
            let UntypedExpr::Var { name, location } = &argument.value else {
                panic!("expected {expected_name} to remain a variable");
            };
            let start = source.find(expected_name).unwrap();

            assert_eq!(name, expected_name);
            assert_eq!(*location, Span::create(start, expected_name.len()));
        }

        assert!(matches!(
            &tail.value,
            UntypedExpr::Value { value, .. } if value.is_empty()
        ));
    }

    #[test]
    fn named_value_fields_wrap_a_terminal_static_value() {
        let static_policy = policy("11");
        let source = format!(
            r#"{{
              lovelace: ada,
              #"{static_policy}": {{
                #"aa": 1,
                asset: quantity,
              }},
              policy: {{ #"bb": 2 }},
            }}"#,
        );
        let expression = parse(&source).expect("Value literal should parse");
        let mut insertion_count = 0;
        let mut current = &expression;

        loop {
            match current {
                UntypedExpr::Call { arguments, .. } => {
                    insertion_count += 1;
                    current = &arguments[3].value;
                }
                UntypedExpr::Value { value, .. } => {
                    assert_eq!(
                        value.as_ref(),
                        &vec![(vec![0x11; POLICY_ID_LENGTH], vec![(vec![0xaa], 1)])],
                    );
                    break;
                }
                expression => panic!("unexpected expression in insertion chain: {expression:#?}"),
            }
        }

        assert_eq!(insertion_count, 3);
    }

    #[test]
    fn rejects_duplicate_lovelace_entries() {
        assert_eq!(
            invalid_value_reason("{ lovelace: 1, lovelace: 2 }"),
            ValueError::CurrencySymbolsNotStrictlyAscending.to_string(),
        );
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
