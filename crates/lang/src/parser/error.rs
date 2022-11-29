use std::collections::HashSet;

use miette::Diagnostic;

use crate::{ast::Span, parser::token::Token};

#[derive(Debug, Diagnostic, thiserror::Error)]
#[error("{}", .kind)]
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
    #[error("unexpected end")]
    UnexpectedEnd,
    #[error("unexpected {0}")]
    #[diagnostic(help("{}", .0.help().unwrap_or_else(|| Box::new(""))))]
    Unexpected(Pattern),
    #[error("unclosed {start}")]
    Unclosed {
        start: Pattern,
        #[label]
        before_span: Span,
        before: Option<Pattern>,
    },
    #[error("no end branch")]
    NoEndBranch,
}

#[derive(Debug, PartialEq, Eq, Hash, Diagnostic, thiserror::Error)]
pub enum Pattern {
    #[error("{0:?}")]
    Char(char),
    #[error("{0}")]
    #[diagnostic(help("try removing it"))]
    Token(Token),
    #[error("literal")]
    Literal,
    #[error("type name")]
    TypeIdent,
    #[error("indentifier")]
    TermIdent,
    #[error("end of input")]
    End,
    #[error("pattern")]
    #[diagnostic(help("list spread in match can only have a discard or var"))]
    Match,
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
