use chumsky::prelude::*;

use crate::{
    ast,
    parser::{error::ParseError, token::Token},
};

use super::function;

pub fn parser() -> impl Parser<Token, ast::UntypedDefinition, Error = ParseError> {
    just(Token::Validator)
        .ignore_then(
            function::param(true)
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .delimited_by(just(Token::LeftParen), just(Token::RightParen))
                .map_with_span(|arguments, span| (arguments, span))
                .or_not(),
        )
        .then(
            function()
                .repeated()
                .at_least(1)
                .at_most(2)
                .delimited_by(just(Token::LeftBrace), just(Token::RightBrace))
                .map(|defs| {
                    defs.into_iter().map(|def| {
                        let ast::UntypedDefinition::Fn(fun) = def else {
                            unreachable!("It should be a fn definition");
                        };

                        fun
                    })
                }),
        )
        .map_with_span(|(opt_extra_params, mut functions), span| {
            let (params, params_span) = opt_extra_params.unwrap_or((
                vec![],
                ast::Span {
                    start: 0,
                    end: span.start + "validator".len(),
                },
            ));

            let fun = functions
                .next()
                .expect("unwrapping safe because there's 'at_least(1)' function");

            let other_fun = functions.next();

            ast::UntypedDefinition::Validator(ast::Validator {
                doc: None,
                fun,
                other_fun,
                location: ast::Span {
                    start: span.start,
                    // capture the span from the optional params
                    end: params_span.end,
                },
                params,
                end_position: span.end - 1,
            })
        })
}

#[cfg(test)]
mod tests {
    use crate::assert_definition;

    #[test]
    fn validator() {
        assert_definition!(
            r#"
            validator {
              fn foo(datum, rdmr, ctx) {
                True
              }
            }
            "#
        );
    }

    #[test]
    fn double_validator() {
        assert_definition!(
            r#"
            validator {
              fn foo(datum, rdmr, ctx) {
                True
              }

              fn bar(rdmr, ctx) {
                True
              }
            }
            "#
        );
    }
}
