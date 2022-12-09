use chumsky::prelude::*;

use crate::ast::Span;

use super::{error::ParseError, token::Token};

pub fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = ParseError> {
    let int = choice((text::int(10), text::int(16))).map(|value| Token::Int { value });

    let op = choice((
        just("==").to(Token::EqualEqual),
        just('=').to(Token::Equal),
        just("..").to(Token::DotDot),
        just('.').to(Token::Dot),
        just("!=").to(Token::NotEqual),
        just('!').to(Token::Bang),
        just("<=").to(Token::LessEqual),
        just('<').to(Token::Less),
        just(">=").to(Token::GreaterEqual),
        just('>').to(Token::Greater),
        just('+').to(Token::Plus),
        just("->").to(Token::RArrow),
        just('-').to(Token::Minus),
        just('*').to(Token::Star),
        just('/').to(Token::Slash),
        just('%').to(Token::Percent),
        just("|>").to(Token::Pipe),
        just(',').to(Token::Comma),
        just(':').to(Token::Colon),
        just("||").to(Token::VbarVbar),
        just('|').to(Token::Vbar),
        just("&&").to(Token::AmperAmper),
        just('#').to(Token::Hash),
        just("\n\n").to(Token::EmptyLine),
    ));

    let grouping = choice((
        just('(').to(Token::LeftParen),
        just(')').to(Token::RightParen),
        just('[').to(Token::LeftSquare),
        just(']').to(Token::RightSquare),
        just('{').to(Token::LeftBrace),
        just('}').to(Token::RightBrace),
    ));

    let escape = just('\\').ignore_then(
        just('\\')
            .or(just('/'))
            .or(just('"'))
            .or(just('b').to('\x08'))
            .or(just('f').to('\x0C'))
            .or(just('n').to('\n'))
            .or(just('r').to('\r'))
            .or(just('t').to('\t')),
    );

    let string = just('"')
        .ignore_then(filter(|c| *c != '\\' && *c != '"').or(escape).repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(|value| Token::String { value })
        .labelled("string");

    let keyword = text::ident().map(|s: String| match s.as_str() {
        "as" => Token::As,
        "assert" => Token::Assert,
        "check" => Token::Assert,
        "const" => Token::Const,
        "fn" => Token::Fn,
        "test" => Token::Test,
        "if" => Token::If,
        "else" => Token::Else,
        "is" => Token::Is,
        "let" => Token::Let,
        "opaque" => Token::Opaque,
        "pub" => Token::Pub,
        "use" => Token::Use,
        "todo" => Token::Todo,
        "trace" => Token::Trace,
        "type" => Token::Type,
        "when" => Token::When,
        _ => {
            if s.chars().next().map_or(false, |c| c.is_uppercase()) {
                Token::UpName {
                    // TODO: do not allow _ in upname
                    name: s,
                }
            } else if s.starts_with('_') {
                Token::DiscardName {
                    // TODO: do not allow uppercase letters in discard name
                    name: s,
                }
            } else {
                Token::Name {
                    // TODO: do not allow uppercase letters in name
                    name: s,
                }
            }
        }
    });

    let module_comments = just("////").ignore_then(
        take_until(text::newline().rewind())
            .to(Token::ModuleComment)
            .map_with_span(|token, span| (token, span)),
    );

    let doc_comments = just("///").ignore_then(
        take_until(text::newline().rewind())
            .to(Token::DocComment)
            .map_with_span(|token, span| (token, span)),
    );

    let comments = just("//").ignore_then(
        take_until(text::newline().rewind())
            .to(Token::Comment)
            .map_with_span(|token, span| (token, span)),
    );

    choice((
        module_comments,
        doc_comments,
        comments,
        choice((keyword, int, op, grouping, string))
            .or(any().map(Token::Error).validate(|t, span, emit| {
                emit(ParseError::expected_input_found(
                    span,
                    None,
                    Some(t.clone()),
                ));
                t
            }))
            .map_with_span(|token, span| (token, span)),
    ))
    .padded()
    .recover_with(skip_then_retry_until([]))
    .repeated()
    .padded()
    .then_ignore(end())
}
