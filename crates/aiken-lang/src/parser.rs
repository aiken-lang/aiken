mod annotation;
pub mod definitions;
pub mod error;
pub mod expr;
pub mod extra;
pub mod lexer;
pub mod pattern;
pub mod token;
mod utils;

pub use annotation::parser as annotation;
pub use definitions::parser as definitions;
pub use expr::parser as expression;
pub use pattern::parser as pattern;

use crate::ast::{self, Span};
use chumsky::{chain::Chain, prelude::*};
use error::ParseError;
use extra::ModuleExtra;
use token::Token;

pub fn module(
    src: &str,
    kind: ast::ModuleKind,
) -> Result<(ast::UntypedModule, ModuleExtra), Vec<ParseError>> {
    let len = src.as_bytes().len();

    let span = |i, n| Span::new((), i..i + n);

    let tokens = lexer::lexer().parse(chumsky::Stream::from_iter(
        span(len, 1),
        src.chars().scan(0, |i, c| {
            let start = *i;
            let offset = c.len_utf8();
            *i = start + offset;
            Some((c, span(start, offset)))
        }),
    ))?;

    let mut extra = ModuleExtra::new();

    let mut previous_is_newline = false;

    let tokens = tokens.into_iter().filter_map(|(token, ref span)| {
        let current_is_newline = token == Token::NewLine || token == Token::EmptyLine;
        let result = match token {
            Token::ModuleComment => {
                extra.module_comments.push(*span);
                None
            }
            Token::DocComment => {
                extra.doc_comments.push(*span);
                None
            }
            Token::Comment => {
                extra.comments.push(*span);
                None
            }
            Token::EmptyLine => {
                extra.empty_lines.push(span.start);
                None
            }
            Token::LeftParen => {
                if previous_is_newline {
                    Some((Token::NewLineLeftParen, *span))
                } else {
                    Some((Token::LeftParen, *span))
                }
            }
            Token::Pipe => {
                if previous_is_newline {
                    Some((Token::NewLinePipe, *span))
                } else {
                    Some((Token::Pipe, *span))
                }
            }
            Token::NewLine => None,
            _ => Some((token, *span)),
        };

        previous_is_newline = current_is_newline;

        result
    });

    let definitions =
        definitions().parse(chumsky::Stream::from_iter(span(tokens.len()), tokens))?;

    let module = ast::UntypedModule {
        kind,
        definitions,
        docs: vec![],
        name: "".to_string(),
        type_info: (),
    };

    Ok((module, extra))
}
