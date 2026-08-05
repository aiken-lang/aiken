use chumsky::prelude::*;

use crate::{
    expr::UntypedExpr,
    parser::{error::ParseError, token::Token},
};

enum ParenthesizedTail {
    Tuple(Vec<UntypedExpr>),
    Sequence(Vec<UntypedExpr>),
}

pub fn parser<'a>(
    sequence: Recursive<'a, Token, UntypedExpr, ParseError>,
    expression: Recursive<'a, Token, UntypedExpr, ParseError>,
) -> impl Parser<Token, UntypedExpr, Error = ParseError> + 'a {
    expression
        .clone()
        .then(choice((
            just(Token::Comma)
                .ignore_then(
                    expression
                        .separated_by(just(Token::Comma))
                        .at_least(1)
                        .allow_trailing(),
                )
                .map(ParenthesizedTail::Tuple),
            just(Token::Comma)
                .not()
                .rewind()
                .ignore_then(sequence.repeated())
                .map(ParenthesizedTail::Sequence),
        )))
        .delimited_by(
            choice((just(Token::LeftParen), just(Token::NewLineLeftParen))),
            just(Token::RightParen),
        )
        .map_with_span(|(first, tail), span| match tail {
            ParenthesizedTail::Tuple(tail) => {
                let mut elems = Vec::with_capacity(tail.len() + 1);
                elems.push(first);
                elems.extend(tail);

                UntypedExpr::Tuple {
                    location: span,
                    elems,
                }
            }
            ParenthesizedTail::Sequence(tail) => {
                let expression = tail
                    .into_iter()
                    .fold(first, UntypedExpr::append_in_sequence);

                super::block::assignment_into_sequence(expression, span)
            }
        })
}

#[cfg(test)]
mod tests {
    use chumsky::{Parser, Stream};

    use crate::{
        ast::Span,
        parser::{expr, lexer},
    };

    #[test]
    fn parse_deeply_nested_grouping() {
        for depth in [16, 32, 64, 128] {
            let source = format!("{}x{}", "(".repeat(depth), ")".repeat(depth));
            let lexer::LexInfo { tokens, .. } =
                lexer::run(&source).expect("nested grouping should lex");
            let stream = Stream::from_iter(Span::create(tokens.len(), 1), tokens.into_iter());

            expr::sequence()
                .parse(stream)
                .unwrap_or_else(|errors| panic!("depth {depth} failed to parse: {errors:?}"));
        }
    }
}
