use std::collections::HashSet;

use miette::Diagnostic;

use crate::{ast::Span, parser::token::Token};

#[derive(Debug, Diagnostic, thiserror::Error)]
#[error("{kind}\n")]
pub struct ParseError {
    pub kind: ErrorKind,
    #[label]
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

    pub fn invalid_tuple_index(span: Span, index: u32, suffix: Option<String>) -> Self {
        let hint = suffix.map(|suffix| format!("Did you mean '{index}{suffix}'?"));
        Self {
            kind: ErrorKind::InvalidTupleIndex { hint },
            span,
            while_parsing: None,
            expected: HashSet::new(),
            label: None,
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

#[derive(Debug, PartialEq, Eq, Diagnostic, thiserror::Error)]
pub enum ErrorKind {
    #[error("I arrived at the end of the file unexpectedly.")]
    UnexpectedEnd,
    #[error("{0}")]
    #[diagnostic(help("{}", .0.help().unwrap_or_else(|| Box::new(""))))]
    Unexpected(Pattern),
    #[error("I discovered an invalid tuple index.")]
    #[diagnostic()]
    InvalidTupleIndex {
        #[help]
        hint: Option<String>,
    },
}

#[derive(Debug, PartialEq, Eq, Hash, Diagnostic, thiserror::Error)]
pub enum Pattern {
    #[error("I found an unexpected char '{0:?}'.")]
    #[diagnostic(help("Try removing it!"))]
    Char(char),
    #[error("I found an unexpected token '{0}'.")]
    #[diagnostic(help("Try removing it!"))]
    Token(Token),
    #[error("I found an unexpected literal value.")]
    #[diagnostic(help("Try removing it!"))]
    Literal,
    #[error("I found an unexpected type name.")]
    #[diagnostic(help("Try removing it!"))]
    TypeIdent,
    #[error("I found an unexpected indentifier.")]
    #[diagnostic(help("Try removing it!"))]
    TermIdent,
    #[error("I found an unexpected end of input.")]
    End,
    #[error("I found a malformed list spread pattern.")]
    #[diagnostic(help("List spread in matches can use a discard '_' or var."))]
    Match,
    #[error("I found an out-of-bound byte literal.")]
    #[diagnostic(help("Bytes must be between 0-255."))]
    Byte,
    #[error("I found an unexpected pattern.")]
    #[diagnostic(help(
        "If no label is provided then only variables\nmatching a field name are allowed."
    ))]
    RecordPunning,
    #[error("I found an unexpected label.")]
    #[diagnostic(help("You can only use labels surrounded by curly braces"))]
    Label,
    #[error("I found an unexpected discard '_'.")]
    #[diagnostic(help("You can only use capture syntax with functions not constructors."))]
    Discard,
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
