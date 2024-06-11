use crate::{
    ast::{self, Span},
    expr::UntypedExpr,
    parser::{annotation, error::ParseError, pattern, token::Token},
};
use chumsky::prelude::*;

pub fn let_(
    r: Recursive<'_, Token, UntypedExpr, ParseError>,
) -> impl Parser<Token, UntypedExpr, Error = ParseError> + '_ {
    just(Token::Let)
        .ignore_then(assignment_patterns())
        .then(choice((just(Token::Equal), just(Token::LArrow))))
        .then(r.clone())
        .validate(move |((patterns, kind), value), span, emit| {
            if matches!(value, UntypedExpr::Assignment { .. }) {
                emit(ParseError::invalid_assignment_right_hand_side(span))
            }

            let patterns = patterns
                .try_into()
                .expect("We use at_least(1) so this should never be empty");

            UntypedExpr::Assignment {
                location: span,
                value: Box::new(value),
                patterns,
                kind: ast::AssignmentKind::Let {
                    backpassing: kind == Token::LArrow,
                },
            }
        })
}

fn assignment_patterns() -> impl Parser<Token, Vec<ast::AssignmentPattern>, Error = ParseError> {
    assignment_pattern()
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .at_least(1)
}

pub fn assignment_pattern() -> impl Parser<Token, ast::AssignmentPattern, Error = ParseError> {
    pattern()
        .then(just(Token::Colon).ignore_then(annotation()).or_not())
        .map_with_span(|(pattern, annotation), span| ast::AssignmentPattern {
            pattern,
            annotation,
            location: span,
        })
}

pub fn expect(
    r: Recursive<'_, Token, UntypedExpr, ParseError>,
) -> impl Parser<Token, UntypedExpr, Error = ParseError> + '_ {
    just(Token::Expect)
        .ignore_then(
            assignment_patterns()
                .then(choice((just(Token::Equal), just(Token::LArrow))))
                .or_not(),
        )
        .then(r.clone())
        .validate(move |(opt_pattern, value), span, emit| {
            if matches!(value, UntypedExpr::Assignment { .. }) {
                emit(ParseError::invalid_assignment_right_hand_side(span))
            }

            let (patterns, kind) = opt_pattern.unwrap_or_else(|| {
                let filler_true = ast::AssignmentPattern::new(
                    ast::UntypedPattern::true_(span),
                    None,
                    Span::empty(),
                );

                (vec![filler_true], Token::Equal)
            });

            let patterns = patterns
                .try_into()
                .expect("We use at_least(1) so this should never be empty");

            UntypedExpr::Assignment {
                location: span,
                patterns,
                value: Box::new(value),
                kind: ast::AssignmentKind::Expect {
                    backpassing: kind == Token::LArrow,
                },
            }
        })
}

#[cfg(test)]
mod tests {
    use crate::assert_expr;

    #[test]
    fn let_bindings() {
        assert_expr!("let thing = [ 1, 2, a ]");
    }

    #[test]
    fn expect() {
        assert_expr!("expect Some(x) = something.field");
    }

    #[test]
    fn expect_bool_sugar() {
        assert_expr!("expect something.field == wow");
    }

    #[test]
    fn expect_trace_if_false() {
        assert_expr!("expect foo?");
    }

    #[test]
    fn expect_unfinished_let() {
        assert_expr!(
            "
            let a =
            // foo
            let b = 42
            "
        );
    }

    #[test]
    fn expect_let_in_let() {
        assert_expr!("let a = { let b = 42 }");
    }

    #[test]
    fn expect_let_in_let_return() {
        assert_expr!(
            "
            let a = {
              let b = 42
              b
            }
            "
        );
    }

    #[test]
    fn expect_let_in_let_parens() {
        assert_expr!("let a = ( let b = 42 )");
    }

    #[test]
    fn expect_expect_let() {
        assert_expr!("expect { let a = 42 } = foo");
    }
}
