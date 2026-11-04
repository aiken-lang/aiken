use aiken_lang::ast::Span;

#[derive(Debug, PartialEq, Eq)]
pub(super) enum Block {
    Predicate {
        body: String,
        body_location: Span,
        location: Span,
    },
    Property {
        id: String,
        id_location: Span,
        scopes: Vec<Scope>,
        description: String,
        body: String,
        body_location: Span,
        location: Span,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub(super) struct Scope {
    pub name: String,
    pub handler: Option<String>,
    pub location: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub(super) struct ParseError {
    pub message: String,
    pub location: Span,
    pub help: Option<String>,
}

/// Parse one delimiter-inclusive annotation span in the complete module source.
/// Bodies start immediately after the header or description, including whitespace;
/// `body_location` indexes exactly those bytes, excluding the closing `@//`.
pub(super) fn parse(source: &str, span: Span) -> Result<Block, ParseError> {
    let region = source.get(span.start..span.end).ok_or_else(|| ParseError {
        message: "Annotation span is outside the module source.".into(),
        location: span,
        help: None,
    })?;
    let content = region
        .strip_prefix("//@")
        .and_then(|region| region.strip_suffix("@//"))
        .ok_or_else(|| ParseError {
            message: "An assurance annotation must be enclosed by `//@` and `@//`.".into(),
            location: span,
            help: None,
        })?;
    let mut parser = Parser {
        content,
        offset: span.start + 3,
        position: 0,
    };
    let (kind, kind_location) = parser.word();
    match kind {
        "PREDICATE" => {
            let (body, body_location) = parser.body()?;
            Ok(Block::Predicate {
                body,
                body_location,
                location: span,
            })
        }
        "PROPERTY" => {
            let (id, id_location) = parser.word();
            if id.is_empty()
                || !id
                    .bytes()
                    .all(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'_' | b'-'))
            {
                return Err(Parser::error(
                    "A property id must match `[A-Za-z0-9_-]+`.",
                    id_location,
                ));
            }
            let (keyword, keyword_location) = parser.word();
            if keyword != "for" {
                return Err(Parser::error(
                    "Expected `for` after the property id.",
                    keyword_location,
                ));
            }
            let mut scopes = Vec::new();
            loop {
                parser.whitespace();
                if parser.remaining().starts_with('"') || parser.remaining().is_empty() {
                    break;
                }
                let (scope, location) = parser.word();
                let mut parts = scope.split('.');
                let name = parts.next().unwrap_or_default();
                let handler = parts.next();
                if !plain_name(name)
                    || handler.is_some_and(|handler| !plain_name(handler))
                    || parts.next().is_some()
                {
                    return Err(Parser::error(
                        "A scope must be a plain target name, optionally followed by `.handler`.",
                        location,
                    ));
                }
                scopes.push(Scope {
                    name: name.into(),
                    handler: handler.map(str::to_owned),
                    location,
                });
            }
            if scopes.is_empty() {
                return Err(Parser::error(
                    "A property must name at least one scope after `for`.",
                    parser.location(parser.position, parser.position),
                ));
            }
            let description = parser.description()?;
            let (body, body_location) = parser.body()?;
            Ok(Block::Property {
                id: id.into(),
                id_location,
                scopes,
                description,
                body,
                body_location,
                location: span,
            })
        }
        "ONCHAIN" => Err(ParseError {
            message: "ONCHAIN annotations are derived from Aiken declarations.".into(),
            location: kind_location,
            help: Some("Use `@onchain` on the function or validator instead.".into()),
        }),
        _ => Err(ParseError {
            message: "Expected `PREDICATE` or `PROPERTY` as the annotation header.".into(),
            location: kind_location,
            help: Some("Annotation headers are uppercase and have no aliases.".into()),
        }),
    }
}

fn plain_name(name: &str) -> bool {
    let mut chars = name.chars();
    chars.next().is_some_and(|char| char.is_lowercase())
        && chars.all(|char| char.is_alphanumeric() || char == '_')
}

struct Parser<'a> {
    content: &'a str,
    offset: usize,
    position: usize,
}

impl<'a> Parser<'a> {
    fn remaining(&self) -> &'a str {
        &self.content[self.position..]
    }

    fn location(&self, start: usize, end: usize) -> Span {
        Span {
            start: self.offset + start,
            end: self.offset + end,
        }
    }

    fn error(message: &str, location: Span) -> ParseError {
        ParseError {
            message: message.into(),
            location,
            help: None,
        }
    }

    fn whitespace(&mut self) {
        self.position += self.remaining().len() - self.remaining().trim_start().len();
    }

    fn word(&mut self) -> (&'a str, Span) {
        self.whitespace();
        let start = self.position;
        self.position += self
            .remaining()
            .find(char::is_whitespace)
            .unwrap_or(self.remaining().len());
        (
            &self.content[start..self.position],
            self.location(start, self.position),
        )
    }

    fn description(&mut self) -> Result<String, ParseError> {
        let start = self.position;
        if !self.remaining().starts_with('"') {
            return Err(Self::error(
                "Expected a nonempty quoted property description.",
                self.location(start, start),
            ));
        }
        self.position += 1;
        let mut description = String::new();
        while let Some(char) = self.remaining().chars().next() {
            let char_start = self.position;
            self.position += char.len_utf8();
            match char {
                '"' => {
                    if description.trim().is_empty() {
                        return Err(Self::error(
                            "A property description must not be empty.",
                            self.location(start, self.position),
                        ));
                    }
                    return Ok(description);
                }
                '\\' => {
                    let Some(escaped) = self.remaining().chars().next() else {
                        return Err(Self::error(
                            "Unterminated escape in the property description.",
                            self.location(char_start, self.position),
                        ));
                    };
                    self.position += escaped.len_utf8();
                    description.push(match escaped {
                        '"' => '"',
                        '\\' => '\\',
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        _ => {
                            return Err(ParseError {
                                message: "Unknown escape in the property description.".into(),
                                location: self.location(char_start, self.position),
                                help: Some(
                                    r#"Use \" for quotes, \\ for backslashes, or \n, \r, \t."#
                                        .into(),
                                ),
                            });
                        }
                    });
                }
                _ => description.push(char),
            }
        }
        Err(Self::error(
            "Unterminated quoted property description.",
            self.location(start, self.position),
        ))
    }

    fn body(&self) -> Result<(String, Span), ParseError> {
        let location = self.location(self.position, self.content.len());
        if self.remaining().trim().is_empty() {
            return Err(Self::error(
                "An assurance annotation must contain a nonempty Lean body.",
                location,
            ));
        }
        Ok((self.remaining().into(), location))
    }
}

#[cfg(test)]
mod tests {
    use super::{Block, parse};
    use aiken_lang::ast::Span;

    fn block(source: &str) -> Result<Block, super::ParseError> {
        parse(
            source,
            Span {
                start: 0,
                end: source.len(),
            },
        )
    }

    fn invalid(source: &str) -> super::ParseError {
        let error = block(source).expect_err("expected an annotation syntax error");
        assert!(error.location.start <= error.location.end);
        assert!(error.location.end <= source.len());
        assert!(source.is_char_boundary(error.location.start));
        assert!(source.is_char_boundary(error.location.end));

        let prefix = "// λ before annotation\n";
        let full_source = format!("{prefix}{source}\n");
        let shifted = parse(
            &full_source,
            Span {
                start: prefix.len(),
                end: prefix.len() + source.len(),
            },
        )
        .unwrap_err();
        assert_eq!(shifted.location.start, prefix.len() + error.location.start);
        assert_eq!(shifted.location.end, prefix.len() + error.location.end);
        assert!(full_source.is_char_boundary(shifted.location.start));
        assert!(full_source.is_char_boundary(shifted.location.end));
        error
    }

    #[test]
    fn predicate_preserves_body_and_absolute_utf8_spans() {
        let prefix = "// λ before annotation\n";
        let annotation = "//@ PREDICATE\r\n  def café := \"λ\"\n @//";
        let source = format!("{prefix}{annotation}\nfn after() {{ True }}");
        let location = Span {
            start: prefix.len(),
            end: prefix.len() + annotation.len(),
        };
        let Block::Predicate {
            body,
            body_location,
            location: actual,
        } = parse(&source, location).unwrap()
        else {
            panic!("expected predicate");
        };
        assert_eq!(actual, location);
        assert_eq!(body, "\r\n  def café := \"λ\"\n ");
        assert_eq!(&source[body_location.start..body_location.end], body);
    }

    #[test]
    fn property_parses_scopes_escapes_and_opaque_body() {
        let source = concat!(
            "//@ PROPERTY 7_property-id for vesting.spend helper vesting.else\n",
            r#"  "A \"quoted\" λ claim\\path\nnext\tline\r""#,
            "\n  by\n    arbitrary opaque Lean ≤ text\n @//",
        );
        let Block::Property {
            id,
            id_location,
            scopes,
            description,
            body,
            body_location,
            location,
        } = block(source).unwrap()
        else {
            panic!("expected property");
        };
        assert_eq!(id, "7_property-id");
        assert_eq!(&source[id_location.start..id_location.end], id);
        assert_eq!(scopes.len(), 3);
        for (scope, expected) in scopes
            .iter()
            .zip(["vesting.spend", "helper", "vesting.else"])
        {
            assert_eq!(&source[scope.location.start..scope.location.end], expected);
        }
        assert_eq!(scopes[0].name, "vesting");
        assert_eq!(scopes[0].handler.as_deref(), Some("spend"));
        assert_eq!(scopes[1].handler, None);
        assert_eq!(description, "A \"quoted\" λ claim\\path\nnext\tline\r");
        assert_eq!(body, "\n  by\n    arbitrary opaque Lean ≤ text\n ");
        assert_eq!(&source[body_location.start..body_location.end], body);
        assert_eq!(location.end, source.len());
    }

    #[test]
    fn rejects_headers_and_points_onchain_to_decorator() {
        for header in ["predicate", "UNKNOWN", "UPLC_DATA"] {
            let source = format!("//@ {header} x @//");
            let error = invalid(&source);
            assert_eq!(&source[error.location.start..error.location.end], header);
        }
        let error = invalid("//@ ONCHAIN f @//");
        assert!(error.help.unwrap().contains("@onchain"));
        invalid("//@ \n @//");
    }

    #[test]
    fn rejects_empty_bodies_and_descriptions() {
        for source in [
            "//@ PREDICATE \n @//",
            "//@ PROPERTY p for f \"description\" \n @//",
            "//@ PROPERTY p for f \"\" True @//",
            "//@ PROPERTY p for f \"  \\n\" True @//",
        ] {
            invalid(source);
        }
    }

    #[test]
    fn pinpoints_invalid_header_fields() {
        for (source, expected) in [
            ("//@ PROPERTY bad.id for f \"d\" True @//", "bad.id"),
            ("//@ PROPERTY p with f \"d\" True @//", "with"),
            ("//@ PROPERTY p for mod/f \"d\" True @//", "mod/f"),
            (
                "//@ PROPERTY p for f.spend.more \"d\" True @//",
                "f.spend.more",
            ),
            ("//@ PROPERTY p for f. \"d\" True @//", "f."),
            ("//@ PROPERTY p for .spend \"d\" True @//", ".spend"),
            ("//@ PROPERTY p for f\"d\" True @//", "f\"d\""),
            ("//@ PROPERTY p for f \"λ \\q\" True @//", "\\q"),
        ] {
            let error = invalid(source);
            assert_eq!(&source[error.location.start..error.location.end], expected);
        }
    }

    #[test]
    fn rejects_missing_header_components_and_unterminated_description() {
        for source in [
            "//@ PROPERTY @//",
            "//@ PROPERTY p @//",
            "//@ PROPERTY p for @//",
            "//@ PROPERTY p for \"d\" True @//",
            "//@ PROPERTY p for f @//",
            "//@ PROPERTY p for f \"unterminated @//",
            "//@ PROPERTY p for f \"unfinished \\@//",
        ] {
            invalid(source);
        }
    }
}
