use crate::{
    ast::{CurveType, Span},
    parser::token::Token,
};
use indoc::formatdoc;
use miette::Diagnostic;
use owo_colors::{OwoColorize, Stream::Stdout};
use std::collections::HashSet;

#[derive(Debug, Clone, Diagnostic, thiserror::Error)]
#[error("{kind}\n")]
#[diagnostic(
    help(
        "{}",
        match kind {
            ErrorKind::Unexpected(..) if !expected.is_empty() => {
                format!(
                    "I am looking for one of the following patterns:\n{}",
                    expected
                        .iter()
                        .map(|x| format!(
                            "→ {}",
                            x.to_aiken()
                             .if_supports_color(Stdout, |s| s.purple())
                        ))
                        .collect::<Vec<_>>()
                        .join("\n")
                )
            },
            _ => {
                kind.help().map(|x| x.to_string()).unwrap_or_default()
            }
        }
    )
)]
pub struct ParseError {
    pub kind: ErrorKind,
    #[label("{}", .label.unwrap_or_default())]
    pub span: Span,
    #[allow(dead_code)]
    while_parsing: Option<(Span, &'static str)>,
    expected: HashSet<Pattern>,
    label: Option<&'static str>,
}

impl ParseError {
    pub fn merge(mut self, other: Self) -> Self {
        // TODO: Use HashSet
        for expected in other.expected.into_iter() {
            self.expected.insert(expected);
        }
        self
    }

    pub fn expected_but_got(expected: Pattern, got: Pattern, span: Span) -> Self {
        Self {
            kind: ErrorKind::Unexpected(got),
            expected: HashSet::from_iter([expected]),
            span,
            while_parsing: None,
            label: None,
        }
    }

    pub fn invalid_assignment_right_hand_side(span: Span) -> Self {
        Self {
            kind: ErrorKind::UnfinishedAssignmentRightHandSide,
            span,
            while_parsing: None,
            expected: HashSet::new(),
            label: Some("invalid assignment right-hand side"),
        }
    }

    pub fn invalid_tuple_index(span: Span, index: String, suffix: Option<String>) -> Self {
        let hint = suffix.map(|suffix| format!("Did you mean '{index}{suffix}'?"));
        Self {
            kind: ErrorKind::InvalidTupleIndex { hint },
            span,
            while_parsing: None,
            expected: HashSet::new(),
            label: None,
        }
    }

    pub fn deprecated_when_clause_guard(span: Span) -> Self {
        Self {
            kind: ErrorKind::DeprecatedWhenClause,
            span,
            while_parsing: None,
            expected: HashSet::new(),
            label: Some("deprecated"),
        }
    }

    pub fn point_not_on_curve(curve: CurveType, span: Span) -> Self {
        Self {
            kind: ErrorKind::PointNotOnCurve { curve },
            span,
            while_parsing: None,
            expected: HashSet::new(),
            label: Some("out off curve"),
        }
    }

    pub fn unknown_point_curve(curve: String, point: Option<String>, span: Span) -> Self {
        let label = if point.is_some() {
            Some("unknown curve")
        } else {
            Some("unknown point")
        };

        Self {
            kind: ErrorKind::UnknownCurvePoint { curve, point },
            span,
            while_parsing: None,
            expected: HashSet::new(),
            label,
        }
    }

    pub fn malformed_base16_string_literal(span: Span) -> Self {
        Self {
            kind: ErrorKind::MalformedBase16StringLiteral,
            span,
            while_parsing: None,
            expected: HashSet::new(),
            label: None,
        }
    }

    pub fn malformed_base16_digits(span: Span) -> Self {
        Self {
            kind: ErrorKind::MalformedBase16Digits,
            span,
            while_parsing: None,
            expected: HashSet::new(),
            label: None,
        }
    }

    pub fn hybrid_notation_in_bytearray(span: Span) -> Self {
        Self {
            kind: ErrorKind::HybridNotationInByteArray,
            span,
            while_parsing: None,
            expected: HashSet::new(),
            label: None,
        }
    }

    pub fn match_on_curve(span: Span) -> Self {
        Self {
            kind: ErrorKind::PatternMatchOnCurvePoint,
            span,
            while_parsing: None,
            expected: HashSet::new(),
            label: Some("cannot pattern-match on curve point"),
        }
    }

    pub fn match_string(span: Span) -> Self {
        Self {
            kind: ErrorKind::PatternMatchOnString,
            span,
            while_parsing: None,
            expected: HashSet::new(),
            label: Some("cannot pattern-match on string"),
        }
    }
}

impl PartialEq for ParseError {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind && self.span == other.span && self.label == other.label
    }
}

impl<T: Into<Pattern>> chumsky::Error<T> for ParseError {
    type Span = Span;

    type Label = &'static str;

    fn expected_input_found<Iter: IntoIterator<Item = Option<T>>>(
        span: Self::Span,
        expected: Iter,
        found: Option<T>,
    ) -> Self {
        Self {
            kind: found
                .map(Into::into)
                .map(ErrorKind::Unexpected)
                .unwrap_or(ErrorKind::UnexpectedEnd),
            span,
            while_parsing: None,
            expected: expected
                .into_iter()
                .map(|x| x.map(Into::into).unwrap_or(Pattern::End))
                .collect(),
            label: None,
        }
    }

    fn with_label(mut self, label: Self::Label) -> Self {
        self.label.get_or_insert(label);
        self
    }

    fn merge(self, other: Self) -> Self {
        ParseError::merge(self, other)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Diagnostic, thiserror::Error)]
pub enum ErrorKind {
    #[error("I arrived at the end of the file unexpectedly.")]
    UnexpectedEnd,

    #[error("{0}")]
    #[diagnostic(help("{}", .0.help().unwrap_or_else(|| Box::new("")))) ]
    Unexpected(Pattern),

    #[error("I discovered an invalid tuple index.")]
    #[diagnostic()]
    InvalidTupleIndex {
        #[help]
        hint: Option<String>,
    },

    #[error("I spotted an unfinished assignment.")]
    #[diagnostic(
        help(
            "{} and {} bindings must be followed by a valid, complete, expression.",
            "let".if_supports_color(Stdout, |s| s.yellow()),
            "expect".if_supports_color(Stdout, |s| s.yellow()),
        ),
    )]
    UnfinishedAssignmentRightHandSide,

    #[error("I tripped over a {}", fmt_curve_type(.curve))]
    PointNotOnCurve { curve: CurveType },

    #[error("I tripped over a {}", fmt_unknown_curve(.curve, .point))]
    UnknownCurvePoint {
        curve: String,
        point: Option<String>,
    },

    #[error("I tripped over a malformed hexadecimal digits.")]
    #[diagnostic(help("{}", formatdoc! {
        r#"When numbers starts with '0x', they are treated as hexadecimal numbers. Thus, only digits from 0-9 or letter from a-f (or A-F) can be used following a '0x' number declaration. Plus, hexadecimal digits always go by pairs, so the total number of digits must be even (not counting leading zeros)."#
    }))]
    MalformedBase16Digits,

    #[error("I tripped over a malformed base16-encoded string literal.")]
    #[diagnostic(help("{}", formatdoc! {
        r#"You can declare literal bytearrays from base16-encoded (a.k.a. hexadecimal) string literals.

           For example:

             ┍━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
             │ {} my_policy_id {}
             │   #{}
        "#,
        "pub const".if_supports_color(Stdout, |s| s.bright_blue()),
        "=".if_supports_color(Stdout, |s| s.yellow()),
        "\"f4c9f9c4252d86702c2f4c2e49e6648c7cffe3c8f2b6b7d779788f50\""
            .if_supports_color(Stdout, |s| s.bright_purple())
    }))]
    MalformedBase16StringLiteral,

    #[error("I came across a bytearray declared using two different notations.")]
    #[diagnostic(url("https://aiken-lang.org/language-tour/primitive-types#bytearray"))]
    #[diagnostic(help("Either use decimal or hexadecimal notation, but don't mix them."))]
    HybridNotationInByteArray,

    #[error("I found a now-deprecated clause guard in a when/is expression.")]
    #[diagnostic(help("{}", formatdoc! {
        r#"Clause guards have been removed from Aiken. They were underused, considered potentially harmful and created needless complexity in the compiler. If you were using clause guards, our apologies, but you can now update your code and move the clause guards patterns inside a nested if/else expression.
        "#
    }))]
    DeprecatedWhenClause,

    #[error("I choked on a curve point in a bytearray pattern.")]
    #[diagnostic(help(
        "You can pattern-match on bytearrays just fine, but not on G1 nor G2 elements. Use if/else with an equality if you have to compare those."
    ))]
    PatternMatchOnCurvePoint,

    #[error("I refuse to cooperate and match a utf-8 string.")]
    #[diagnostic(help(
        "You can pattern-match on bytearrays but not on strings. Note that I can parse utf-8 encoded bytearrays just fine, so you probably want to drop the extra '@' and only manipulate bytearrays wherever you need to. On-chain, strings shall be avoided as much as possible."
    ))]
    PatternMatchOnString,
}

fn fmt_curve_type(curve: &CurveType) -> String {
    match curve {
        CurveType::Bls12_381(point) => {
            format!("{point} point that is not in the bls12_381 curve")
        }
    }
}

fn fmt_unknown_curve(curve: &String, point: &Option<String>) -> String {
    match point {
        Some(point) => {
            format!(
                "{} which is an unknown point for curve {}",
                point.if_supports_color(Stdout, |s| s.purple()),
                curve.if_supports_color(Stdout, |s| s.purple()),
            )
        }
        None => {
            format!(
                "{} which is an unknown curve",
                curve.if_supports_color(Stdout, |s| s.purple())
            )
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Diagnostic, thiserror::Error)]
pub enum Pattern {
    #[error("I found an unexpected char '{0:?}'.")]
    #[diagnostic(help("Try removing it!"))]
    Char(char),
    #[error("I found an unexpected token '{0}'.")]
    #[diagnostic(help("Try removing it!"))]
    Token(Token),
    #[error("I found an unexpected end of input.")]
    End,
    #[error("I found a malformed list spread pattern.")]
    #[diagnostic(help("List spread in matches can use a discard '_' or var."))]
    Match,
    #[error("I found an out-of-bound byte literal.")]
    #[diagnostic(help("Bytes must be between 0-255."))]
    Byte,
    #[error("I found an unexpected label.")]
    #[diagnostic(help("You can only use labels surrounded by curly braces"))]
    Label,
    #[error("I found an unexpected discard '_'.")]
    #[diagnostic(help("You can only use capture syntax with functions not constructors."))]
    Discard,
}

impl Pattern {
    fn to_aiken(&self) -> String {
        use Pattern::*;
        match self {
            Token(tok) => tok.to_string(),
            Char(c) => c.to_string(),
            End => "<END OF FILE>".to_string(),
            Match => "A pattern (a discard, a var, etc...)".to_string(),
            Byte => "A byte between [0; 255]".to_string(),
            Label => "A label".to_string(),
            Discard => "_".to_string(),
        }
    }
}

impl From<char> for Pattern {
    fn from(c: char) -> Self {
        Self::Char(c)
    }
}

impl From<Token> for Pattern {
    fn from(tok: Token) -> Self {
        Self::Token(tok)
    }
}
