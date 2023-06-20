use chumsky::prelude::*;

use crate::{
    ast, expr,
    parser::{error::ParseError, token::Token, utils},
};

pub fn parser() -> impl Parser<Token, ast::UntypedDefinition, Error = ParseError> {
    utils::public()
        .or_not()
        .then_ignore(just(Token::Fn))
        .then(select! {Token::Name {name} => name})
        .then(
            fn_param_parser(false)
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .delimited_by(just(Token::LeftParen), just(Token::RightParen))
                .map_with_span(|arguments, span| (arguments, span)),
        )
        .then(just(Token::RArrow).ignore_then(type_parser()).or_not())
        .then(
            expr_seq_parser()
                .or_not()
                .delimited_by(just(Token::LeftBrace), just(Token::RightBrace)),
        )
        .map_with_span(
            |((((opt_pub, name), (arguments, args_span)), return_annotation), body), span| {
                ast::UntypedDefinition::Fn(ast::Function {
                    arguments,
                    body: body.unwrap_or_else(|| expr::UntypedExpr::todo(span, None)),
                    doc: None,
                    location: ast::Span {
                        start: span.start,
                        end: return_annotation
                            .as_ref()
                            .map(|l| l.location().end)
                            .unwrap_or_else(|| args_span.end),
                    },
                    end_position: span.end - 1,
                    name,
                    public: opt_pub.is_some(),
                    return_annotation,
                    return_type: (),
                    can_error: true,
                })
            },
        )
}
