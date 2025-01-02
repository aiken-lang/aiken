use crate::{
    ast,
    expr::UntypedExpr,
    parser::{
        error::{self, ParseError},
        token::Token,
    },
};
use chumsky::prelude::*;

pub fn parser(
    r: Recursive<'_, Token, UntypedExpr, ParseError>,
) -> impl Parser<Token, UntypedExpr, Error = ParseError> + '_ {
    choice((
        select! {Token::Name { name } => name}
            .map_with_span(|module, span: ast::Span| (module, span))
            .then_ignore(just(Token::Dot))
            .or_not()
            .then(select! {Token::UpName { name } => name}.map_with_span(|name, span| (name, span)))
            .then(
                choice((
                    select! {Token::Name {name} => name}
                        .then_ignore(just(Token::Colon))
                        .then(choice((
                            r.clone(),
                            select! {Token::DiscardName {name} => name }.validate(
                                |_name, span, emit| {
                                    emit(ParseError::expected_input_found(
                                        span,
                                        None,
                                        Some(error::Pattern::Discard),
                                    ));

                                    UntypedExpr::Var {
                                        location: span,
                                        name: ast::CAPTURE_VARIABLE.to_string(),
                                    }
                                },
                            ),
                        )))
                        .map_with_span(|(label, value), span| ast::CallArg {
                            location: span,
                            value,
                            label: Some(label),
                        }),
                    choice((
                        select! {Token::Name {name} => name}.map_with_span(|name, span| {
                            (
                                UntypedExpr::Var {
                                    name: name.clone(),
                                    location: span,
                                },
                                name,
                            )
                        }),
                        select! {Token::DiscardName {name} => name }.validate(
                            |name, span, emit| {
                                emit(ParseError::expected_input_found(
                                    span,
                                    None,
                                    Some(error::Pattern::Discard),
                                ));

                                (
                                    UntypedExpr::Var {
                                        location: span,
                                        name: ast::CAPTURE_VARIABLE.to_string(),
                                    },
                                    name,
                                )
                            },
                        ),
                    ))
                    // NOTE: There's an ambiguity when the record shorthand syntax is used
                    // from within an if-else statement in the case of single-variable if-branch.
                    //
                    // For example, imagine the following:
                    //
                    // ```
                    // if season == Summer {
                    //   foo
                    // } else {
                    //   bar
                    // }
                    // ```
                    //
                    // Without that next odd parser combinator, the parser would parse:
                    //
                    // ```
                    // if season == Summer { foo }
                    // else {
                    //   bar
                    // }
                    // ```
                    //
                    // And immediately choke on the next `else` because the if-branch body has
                    // already been consumed and interpreted as a record definition. So the next
                    // combinator ensures that we give priority back to an if-then statement rather
                    // than to the record definition.
                    .then_ignore(
                        just(Token::RightBrace)
                            .ignore_then(just(Token::Else))
                            .not()
                            .rewind(),
                    )
                    .map(|(value, name)| ast::CallArg {
                        location: value.location(),
                        value,
                        label: Some(name),
                    }),
                ))
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .delimited_by(just(Token::LeftBrace), just(Token::RightBrace)),
            ),
        select! {Token::Name { name } => name}
            .map_with_span(|module, span| (module, span))
            .then_ignore(just(Token::Dot))
            .or_not()
            .then(select! {Token::UpName { name } => name}.map_with_span(|name, span| (name, span)))
            .then(
                select! {Token::Name {name} => name}
                    .ignored()
                    .then_ignore(just(Token::Colon))
                    .validate(|_label, span, emit| {
                        emit(ParseError::expected_input_found(
                            span,
                            None,
                            Some(error::Pattern::Label),
                        ))
                    })
                    .or_not()
                    .then(choice((
                        r.clone(),
                        select! {Token::DiscardName {name} => name }.validate(
                            |_name, span, emit| {
                                emit(ParseError::expected_input_found(
                                    span,
                                    None,
                                    Some(error::Pattern::Discard),
                                ));

                                UntypedExpr::Var {
                                    location: span,
                                    name: ast::CAPTURE_VARIABLE.to_string(),
                                }
                            },
                        ),
                    )))
                    .map(|(_label, value)| ast::CallArg {
                        location: value.location(),
                        value,
                        label: None,
                    })
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
            ),
    ))
    .map_with_span(|((module, (name, n_span)), arguments), span| {
        let fun = if let Some((module, m_span)) = module {
            UntypedExpr::FieldAccess {
                location: m_span.union(n_span),
                label: name,
                container: Box::new(UntypedExpr::Var {
                    location: m_span,
                    name: module,
                }),
            }
        } else {
            UntypedExpr::Var {
                location: n_span,
                name,
            }
        };

        UntypedExpr::Call {
            arguments,
            fun: Box::new(fun),
            location: span,
        }
    })
}

#[cfg(test)]
mod tests {
    use crate::assert_expr;

    #[test]
    fn record_enum() {
        assert_expr!(r#"Winter"#);
    }

    #[test]
    fn record_create_labeled() {
        assert_expr!(r#"User { name: "Aiken", age, thing: 2 }"#);
    }

    #[test]
    fn record_create_labeled_with_field_access() {
        assert_expr!(r#"some_module.User { name: "Aiken", age, thing: 2 }"#);
    }

    #[test]
    fn record_create_unlabeled() {
        assert_expr!(r#"some_module.Thing(1, a)"#);
    }
}
