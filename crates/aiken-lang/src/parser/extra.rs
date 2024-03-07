use crate::ast::Span;
use std::iter::Peekable;

#[derive(Debug, PartialEq, Eq, Default, Clone, serde::Serialize, serde::Deserialize)]
pub struct ModuleExtra {
    pub module_comments: Vec<Span>,
    pub doc_comments: Vec<Span>,
    pub comments: Vec<Span>,
    pub empty_lines: Vec<usize>,
}

impl ModuleExtra {
    pub fn new() -> Self {
        Default::default()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Comment<'a> {
    pub start: usize,
    pub content: &'a str,
}

impl<'a> From<(&Span, &'a str)> for Comment<'a> {
    fn from(src: (&Span, &'a str)) -> Comment<'a> {
        let start = src.0.start;
        let end = src.0.end;
        Comment {
            start,
            content: std::str::from_utf8(src.1.as_bytes()[start..end].as_ref())
                .expect("From span to comment"),
        }
    }
}

pub fn comments_before<'a>(
    comment_spans: &mut Peekable<impl Iterator<Item = &'a Span>>,
    byte: usize,
    src: &'a str,
) -> Vec<&'a str> {
    let mut comments = vec![];
    while let Some(Span { start, .. }) = comment_spans.peek() {
        if start <= &byte {
            let comment = comment_spans
                .next()
                .expect("Comment before accessing next span");
            comments.push(Comment::from((comment, src)).content)
        } else {
            break;
        }
    }
    comments
}
