use super::{
    error::ParseError,
    extra::ModuleExtra,
    token::{Base, Token},
};
use crate::ast::Span;
use chumsky::prelude::*;
use num_bigint::BigInt;
use ordinal::Ordinal;

pub struct LexInfo {
    pub tokens: Vec<(Token, Span)>,
    pub extra: ModuleExtra,
}

pub fn run(src: &str) -> Result<LexInfo, Vec<ParseError>> {
    let len = src.len();

    let tokens = lexer().parse(chumsky::Stream::from_iter(
        Span::create(len, 1),
        src.chars().scan(0, |i, c| {
            let start = *i;
            let offset = c.len_utf8();
            *i = start + offset;
            Some((c, Span::create(start, offset)))
        }),
    ))?;

    let mut extra = ModuleExtra::new();

    let mut previous_is_newline = false;

    let tokens = tokens
        .into_iter()
        .filter_map(|(token, ref span)| {
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
                Token::Minus => {
                    if previous_is_newline {
                        Some((Token::NewLineMinus, *span))
                    } else {
                        Some((Token::Minus, *span))
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
        })
        .collect::<Vec<(Token, Span)>>();

    Ok(LexInfo { tokens, extra })
}

pub fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = ParseError> {
    let base10 = text::int(10).map(|value| Token::Int {
        value,
        base: Base::Decimal {
            numeric_underscore: false,
        },
    });

    let base10_underscore = one_of("0123456789")
        .repeated()
        .at_least(1)
        .at_most(3)
        .separated_by(just("_"))
        .at_least(2)
        .flatten()
        .collect::<String>()
        .map(|value| Token::Int {
            value,
            base: Base::Decimal {
                numeric_underscore: true,
            },
        });

    let base16 = just("0x")
        .ignore_then(
            one_of("0123456789abcdefABCDEF")
                .repeated()
                .at_least(1)
                .collect::<String>(),
        )
        .validate(|value: String, span, emit| {
            let value = match BigInt::parse_bytes(value.as_bytes(), 16) {
                None => {
                    emit(ParseError::malformed_base16_digits(span));
                    String::new()
                }
                Some(n) => n.to_str_radix(10),
            };

            Token::Int {
                value,
                base: Base::Hexadecimal,
            }
        });

    let int = choice((base16, base10_underscore, base10));

    let ordinal = text::int(10)
        .then_with(|index: String| {
            choice((just("st"), just("nd"), just("rd"), just("th")))
                .map(move |suffix| (index.to_string(), suffix))
        })
        .validate(|(index, suffix), span, emit| match index.parse() {
            Err { .. } => {
                emit(ParseError::invalid_tuple_index(span, index, None));
                Token::Ordinal { index: 0 }
            }
            Ok(index) => {
                let expected_suffix = Ordinal::<u32>(index).suffix();
                if expected_suffix != suffix {
                    emit(ParseError::invalid_tuple_index(
                        span,
                        index.to_string(),
                        Some(expected_suffix.to_string()),
                    ))
                }
                Token::Ordinal { index }
            }
        });

    let op = choice((
        just("==").to(Token::EqualEqual),
        just('=').to(Token::Equal),
        just("..").to(Token::DotDot),
        just('.').to(Token::Dot),
        just("!=").to(Token::NotEqual),
        just('!').to(Token::Bang),
        just('?').to(Token::Question),
        just("<-").to(Token::LArrow),
        just("->").to(Token::RArrow),
        choice((
            just("<=").to(Token::LessEqual),
            just('<').to(Token::Less),
            just(">=").to(Token::GreaterEqual),
            just('>').to(Token::Greater),
        )),
        just('+').to(Token::Plus),
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
            .or(just('"'))
            .or(just('n').to('\n'))
            .or(just('r').to('\r'))
            .or(just('t').to('\t'))
            .or(just('0').to('\0')),
    );

    let string = just('@')
        .ignore_then(just('"'))
        .ignore_then(filter(|c| *c != '\\' && *c != '"').or(escape).repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(|value| Token::String { value })
        .labelled("string");

    let bytestring = just('"')
        .ignore_then(filter(|c| *c != '\\' && *c != '"').or(escape).repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(|value| Token::ByteString { value })
        .labelled("bytestring");

    let keyword = text::ident().map(|s: String| match s.as_str() {
        "trace" => Token::Trace,
        // TODO: remove this in a future release
        "error" => Token::Fail,
        "fail" => Token::Fail,
        "once" => Token::Once,
        "as" => Token::As,
        "and" => Token::And,
        "or" => Token::Or,
        "expect" => Token::Expect,
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
        "type" => Token::Type,
        "when" => Token::When,
        "validator" => Token::Validator,
        "via" => Token::Via,
        "bench" => Token::Benchmark,
        _ => {
            if s.chars().next().is_some_and(|c| c.is_uppercase()) {
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

    fn comment_parser(token: Token) -> impl Parser<char, (Token, Span), Error = ParseError> {
        let n = match token {
            Token::ModuleComment => 4,
            Token::DocComment => 3,
            Token::Comment => 2,
            _ => unreachable!(),
        };

        choice((
            // NOTE: The first case here work around a bug introduced with chumsky=0.9.0 which
            // miscalculate the offset for empty comments.
            just("/".repeat(n))
                .ignore_then(choice((text::newline().rewind(), end())))
                .to(token.clone())
                .map_with_span(move |token, span: Span| {
                    (token, Span::new((), span.start + n..span.end))
                }),
            just("/".repeat(n)).ignore_then(
                take_until(choice((text::newline().rewind(), end())))
                    .to(token)
                    .map_with_span(|token, span| (token, span)),
            ),
        ))
    }

    let newlines = choice((
        choice((just("\n\n"), just("\r\n\r\n"))).to(Token::EmptyLine),
        choice((just("\n"), just("\r\n"))).to(Token::NewLine),
    ));

    choice((
        comment_parser(Token::ModuleComment),
        comment_parser(Token::DocComment),
        comment_parser(Token::Comment),
        choice((
            ordinal, keyword, int, op, newlines, grouping, bytestring, string,
        ))
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
    .padded_by(one_of(" \t").ignored().repeated())
    .recover_with(skip_then_retry_until([]))
    .repeated()
    .padded_by(one_of(" \t").ignored().repeated())
    .then_ignore(end())
}
