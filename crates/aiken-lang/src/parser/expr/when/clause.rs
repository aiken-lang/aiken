use super::guard;
use crate::{
    ast,
    expr::UntypedExpr,
    parser::{error::ParseError, pattern, token::Token},
};
use chumsky::prelude::*;
use vec1::vec1;

pub fn parser(
    expression: Recursive<'_, Token, UntypedExpr, ParseError>,
) -> impl Parser<Token, ast::UntypedClause, Error = ParseError> + '_ {
    pattern()
        .then(
            choice((
                just(Token::Vbar),
                just(Token::VbarVbar),
                just(Token::Or),
                just(Token::Comma),
            ))
            .ignore_then(pattern())
            .repeated()
            .or_not(),
        )
        .then(choice((just(Token::If)
            .ignore_then(guard())
            .or_not()
            .then_ignore(just(Token::RArrow)),)))
        // TODO: add hint "Did you mean to wrap a multi line clause in curly braces?"
        .then(expression)
        .validate(
            |(((pattern, alternative_patterns_opt), guard), then), span, emit| {
                if guard.is_some() {
                    emit(ParseError::deprecated_when_clause_guard(span));
                }

                (pattern, alternative_patterns_opt, then)
            },
        )
        .map_with_span(|(pattern, alternative_patterns_opt, then), span| {
            let mut patterns = vec1![pattern];
            patterns.append(&mut alternative_patterns_opt.unwrap_or_default());
            ast::UntypedClause {
                location: span,
                patterns,
                then,
            }
        })
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

    #[test]
    fn when_clause_alternative() {
        assert_expr!(
            r#"
            when val is {
              Bar1{..} | Bar2{..} -> todo
              Bar3{..} || Bar4{..} -> todo
              Bar5{..} or Bar6{..} -> todo
              Bar5{..}, Bar6{..} -> todo
            }
            "#
        );
    }
}
