use chumsky::prelude::*;

use crate::{
    ast,
    expr::UntypedExpr,
    parser::{error::ParseError, token::Token},
};

use super::block;

pub fn parser<'a>(
    seq_r: Recursive<'a, Token, UntypedExpr, ParseError>,
    r: Recursive<'a, Token, UntypedExpr, ParseError>,
) -> impl Parser<Token, UntypedExpr, Error = ParseError> + 'a {
    just(Token::If)
        .ignore_then(r.clone().then(block(seq_r.clone())).map_with_span(
            |(condition, body), span| ast::IfBranch {
                condition,
                body,
                location: span,
            },
        ))
        .then(
            just(Token::Else)
                .ignore_then(just(Token::If))
                .ignore_then(r.clone().then(block(seq_r.clone())).map_with_span(
                    |(condition, body), span| ast::IfBranch {
                        condition,
                        body,
                        location: span,
                    },
                ))
                .repeated(),
        )
        .then_ignore(just(Token::Else))
        .then(block(seq_r))
        .map_with_span(|((first, alternative_branches), final_else), span| {
            let mut branches = vec1::vec1![first];

            branches.extend(alternative_branches);

            UntypedExpr::If {
                location: span,
                branches,
                final_else: Box::new(final_else),
            }
        })
}
