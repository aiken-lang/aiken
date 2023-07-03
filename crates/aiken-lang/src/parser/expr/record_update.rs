use chumsky::prelude::*;

use crate::{
    ast,
    expr::UntypedExpr,
    parser::{error::ParseError, token::Token},
};

pub fn parser(
    r: Recursive<'_, Token, UntypedExpr, ParseError>,
) -> impl Parser<Token, UntypedExpr, Error = ParseError> + '_ {
    select! {Token::Name { name } => name}
        .map_with_span(|module, span: ast::Span| (module, span))
        .then_ignore(just(Token::Dot))
        .or_not()
        .then(select! {Token::UpName { name } => name}.map_with_span(|name, span| (name, span)))
        .then(
            just(Token::DotDot)
                .ignore_then(r.clone())
                .then(
                    just(Token::Comma)
                        .ignore_then(
                            choice((
                                select! { Token::Name {name} => name }
                                    .then_ignore(just(Token::Colon))
                                    .then(r.clone())
                                    .map_with_span(|(label, value), span| {
                                        ast::UntypedRecordUpdateArg {
                                            label,
                                            value,
                                            location: span,
                                        }
                                    }),
                                select! {Token::Name {name} => name}.map_with_span(|name, span| {
                                    ast::UntypedRecordUpdateArg {
                                        location: span,
                                        value: UntypedExpr::Var {
                                            name: name.clone(),
                                            location: span,
                                        },
                                        label: name,
                                    }
                                }),
                            ))
                            .separated_by(just(Token::Comma))
                            .allow_trailing(),
                        )
                        .or_not(),
                )
                .delimited_by(just(Token::LeftBrace), just(Token::RightBrace))
                .map_with_span(|a, span: ast::Span| (a, span)),
        )
        .map(|((module, (name, n_span)), ((spread, opt_args), span))| {
            let constructor = if let Some((module, m_span)) = module {
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

            let spread_span = spread.location();

            let location = ast::Span::new((), spread_span.start - 2..spread_span.end);

            let spread = ast::RecordUpdateSpread {
                base: Box::new(spread),
                location,
            };

            UntypedExpr::RecordUpdate {
                location: constructor.location().union(span),
                constructor: Box::new(constructor),
                spread,
                arguments: opt_args.unwrap_or_default(),
            }
        })
}

#[cfg(test)]
mod tests {
    use crate::assert_expr;

    #[test]
    fn record_update_basic() {
        assert_expr!(r#"User { ..user, name: "Aiken", age }"#);
    }
}
