use chumsky::prelude::*;

use crate::{
    ast,
    expr::UntypedExpr,
    parser::{
        chain::{call::parser as call, field_access, tuple_index::parser as tuple_index, Chain},
        error::ParseError,
        expr::{self, var},
        token::Token,
    },
};

pub fn parser() -> impl Parser<Token, ast::UntypedDefinition, Error = ParseError> {
    // TODO: can remove Token::Bang after a few releases (curr v1.0.11)
    just(Token::Bang)
        .ignored()
        .or_not()
        .then_ignore(just(Token::Test))
        .then(select! {Token::Name {name} => name})
        .then(
            via()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
        )
        .then(just(Token::Fail).ignored().or_not())
        .map_with_span(|name, span| (name, span))
        .then(
            expr::sequence()
                .or_not()
                .delimited_by(just(Token::LeftBrace), just(Token::RightBrace)),
        )
        .map_with_span(
            |(((((old_fail, name), arguments), fail), span_end), body), span| {
                ast::UntypedDefinition::Test(ast::Function {
                    arguments,
                    body: body.unwrap_or_else(|| UntypedExpr::todo(None, span)),
                    doc: None,
                    location: span_end,
                    end_position: span.end - 1,
                    name,
                    public: false,
                    return_annotation: Some(ast::Annotation::boolean(span)),
                    return_type: (),
                    can_error: fail.is_some() || old_fail.is_some(),
                })
            },
        )
}

pub fn via() -> impl Parser<Token, ast::UntypedArgVia, Error = ParseError> {
    choice((
        select! {Token::DiscardName {name} => name}.map_with_span(|name, span| {
            ast::ArgName::Discarded {
                label: name.clone(),
                name,
                location: span,
            }
        }),
        select! {Token::Name {name} => name}.map_with_span(move |name, location| {
            ast::ArgName::Named {
                label: name.clone(),
                name,
                location,
                is_validator_param: false,
            }
        }),
    ))
    .then_ignore(just(Token::Via))
    .then(fuzzer())
    .map_with_span(|(arg_name, via), location| ast::ArgVia {
        arg_name,
        via,
        tipo: (),
        location,
    })
}

pub fn fuzzer<'a>() -> impl Parser<Token, UntypedExpr, Error = ParseError> + 'a {
    recursive(|expression| {
        let chain = choice((
            tuple_index(),
            field_access::parser(),
            call(expression.clone()),
        ));

        var()
            .then(chain.repeated())
            .foldl(|expr, chain| match chain {
                Chain::Call(args, span) => expr.call(args, span),
                Chain::FieldAccess(label, span) => expr.field_access(label, span),
                Chain::TupleIndex(index, span) => expr.tuple_index(index, span),
            })
    })
}

#[cfg(test)]
mod tests {
    use crate::assert_definition;

    #[test]
    fn def_test() {
        assert_definition!(
            r#"
            test foo() {
                True
            }
            "#
        );
    }

    #[test]
    fn def_test_fail() {
        assert_definition!(
            r#"
            test invalid_inputs() fail {
              expect True = False

              False
            }
            "#
        );
    }

    #[test]
    fn def_property_test() {
        assert_definition!(
            r#"
            test foo(x via fuzz.any_int) {
                True
            }
            "#
        );
    }

    #[test]
    fn def_invalid_property_test() {
        assert_definition!(
            r#"
            test foo(x via f, y via g) {
                True
            }
            "#
        );
    }
}
