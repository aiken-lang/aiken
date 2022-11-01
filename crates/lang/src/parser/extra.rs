use crate::ast::Span;

#[derive(Debug, PartialEq, Eq, Default)]
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
        let end = src.0.end as usize;
        Comment {
            start,
            content: src
                .1
                .get(start as usize..end)
                .expect("From span to comment"),
        }
    }
}
