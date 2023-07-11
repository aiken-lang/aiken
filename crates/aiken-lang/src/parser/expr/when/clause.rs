use chumsky::prelude::*;
use vec1::vec1;

use crate::{
    ast,
    expr::UntypedExpr,
    parser::{error::ParseError, pattern, token::Token},
};

use super::guard;

pub fn parser(
    expression: Recursive<'_, Token, UntypedExpr, ParseError>,
) -> impl Parser<Token, ast::UntypedClause, Error = ParseError> + '_ {
    pattern()
        .then(just(Token::Vbar).ignore_then(pattern()).repeated().or_not())
        .then(choice((
            just(Token::If)
                .ignore_then(guard())
                .or_not()
                .then_ignore(just(Token::RArrow)),
            just(Token::If)
                .ignore_then(take_until(just(Token::RArrow)))
                .validate(|_value, span, emit| {
                    emit(ParseError::invalid_when_clause_guard(span));
                    None
                }),
        )))
        // TODO: add hint "Did you mean to wrap a multi line clause in curly braces?"
        .then(expression)
        .map_with_span(
            |(((pattern, alternative_patterns_opt), guard), then), span| {
                let mut patterns = vec1![pattern];
                patterns.append(&mut alternative_patterns_opt.unwrap_or_default());
                ast::UntypedClause {
                    location: span,
                    patterns,
                    guard,
                    then,
                }
            },
        )
}

#[cfg(test)]
mod tests {
    use crate::assert_expr;

    #[test]
    fn when_clause_todo() {
        assert_expr!(
            r#"
            when val is {
              Bar1{..} -> True
              Bar2{..} -> todo @"unimplemented"
            }
            "#
        );
    }

    #[test]
    fn when_clause_solo_error() {
        assert_expr!(
            r#"
            when val is {
              Bar1{..} -> fail
            }
            "#
        );
    }

    #[test]
    fn when_clause_double_todo() {
        assert_expr!(
            r#"
            when val is {
              Bar1{..} -> todo
              Bar2{..} -> todo
            }
            "#
        );
    }
}
