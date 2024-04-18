use chumsky::prelude::*;
use vec1::Vec1;

mod and_or_chain;
mod anonymous_binop;
pub mod anonymous_function;
pub mod assignment;
mod block;
pub(crate) mod bytearray;
mod chained;
mod fail_todo_trace;
mod if_else;
mod int;
mod list;
mod pair;
mod record;
mod record_update;
mod sequence;
pub mod string;
mod tuple;
mod var;
pub mod when;

use super::{error::ParseError, token::Token};
use crate::{ast, expr::UntypedExpr};
pub use and_or_chain::parser as and_or_chain;
pub use anonymous_function::parser as anonymous_function;
pub use block::parser as block;
pub use bytearray::parser as bytearray;
pub use chained::parser as chained;
pub use fail_todo_trace::parser as fail_todo_trace;
pub use if_else::parser as if_else;
pub use int::parser as int;
pub use list::parser as list;
pub use pair::parser as pair;
pub use record::parser as record;
pub use record_update::parser as record_update;
pub use sequence::parser as sequence;
pub use string::parser as string;
pub use tuple::parser as tuple;
pub use var::parser as var;
pub use when::parser as when;

pub fn parser(
    sequence: Recursive<'_, Token, UntypedExpr, ParseError>,
) -> impl Parser<Token, UntypedExpr, Error = ParseError> + '_ {
    recursive(|expression| {
        choice((
            fail_todo_trace(expression.clone(), sequence.clone()),
            pure_expression(sequence, expression),
        ))
    })
}

pub fn pure_expression<'a>(
    sequence: Recursive<'a, Token, UntypedExpr, ParseError>,
    expression: Recursive<'a, Token, UntypedExpr, ParseError>,
) -> impl Parser<Token, UntypedExpr, Error = ParseError> + 'a {
    // Negate
    let op = choice((
        just(Token::Bang).to(ast::UnOp::Not),
        choice((just(Token::Minus), just(Token::NewLineMinus)))
            // NOTE: Prevent conflict with usage for '-' as a standalone binary op.
            // This will make '-' parse when used as standalone binop in a function call.
            // For example:
            //
            //    foo(a, -, b)
            //
            // but it'll fail in a let-binding:
            //
            //    let foo = -
            //
            // which seems acceptable.
            .then_ignore(just(Token::Comma).not().rewind())
            .to(ast::UnOp::Negate),
    ));

    let unary = op
        .map_with_span(|op, span| (op, span))
        .repeated()
        .then(chained(sequence, expression))
        .foldr(|(un_op, span), value| UntypedExpr::UnOp {
            op: un_op,
            location: span.union(value.location()),
            value: Box::new(value),
        })
        .boxed();

    // Product
    let op = choice((
        just(Token::Star).to(ast::BinOp::MultInt),
        just(Token::Slash).to(ast::BinOp::DivInt),
        just(Token::Percent).to(ast::BinOp::ModInt),
    ));

    let product = unary
        .clone()
        .then(op.then(unary).repeated())
        .foldl(|a, (op, b)| UntypedExpr::BinOp {
            location: a.location().union(b.location()),
            name: op,
            left: Box::new(a),
            right: Box::new(b),
        })
        .boxed();

    // Sum
    let op = choice((
        just(Token::Plus).to(ast::BinOp::AddInt),
        just(Token::Minus).to(ast::BinOp::SubInt),
    ));

    let sum = product
        .clone()
        .then(op.then(product).repeated())
        .foldl(|a, (op, b)| UntypedExpr::BinOp {
            location: a.location().union(b.location()),
            name: op,
            left: Box::new(a),
            right: Box::new(b),
        })
        .boxed();

    // Comparison
    let op = choice((
        just(Token::EqualEqual).to(ast::BinOp::Eq),
        just(Token::NotEqual).to(ast::BinOp::NotEq),
        just(Token::Less).to(ast::BinOp::LtInt),
        just(Token::Greater).to(ast::BinOp::GtInt),
        just(Token::LessEqual).to(ast::BinOp::LtEqInt),
        just(Token::GreaterEqual).to(ast::BinOp::GtEqInt),
    ));

    let comparison = sum
        .clone()
        .then(op.then(sum).repeated())
        .foldl(|a, (op, b)| UntypedExpr::BinOp {
            location: a.location().union(b.location()),
            name: op,
            left: Box::new(a),
            right: Box::new(b),
        })
        .boxed();

    // Conjunction
    //
    // NOTE: This can be written in a nicer way with `foldl_with` in chumsky =^ 1.0.0.
    // DO NOT however try to write this as:
    //
    //  comparison
    //    .clone()
    //    .then(op)
    //    .repeated
    //    .then(comparison)
    //    .foldr(...)
    //
    // This has somehow incredibly slow performances in Chumsky. Hence the approach below.
    let op = just(Token::AmperAmper).to(ast::BinOp::And);
    let conjunction = comparison
        .clone()
        .map(|e| vec![e])
        .clone()
        .then(op.then(comparison).repeated())
        .foldl(|a, (_op, b)| {
            let mut tail = vec![b];
            tail.extend(a);
            tail
        })
        .map(|xs| {
            xs.into_iter()
                .reduce(|right, left| UntypedExpr::BinOp {
                    location: left.location().union(right.location()),
                    name: ast::BinOp::And,
                    left: Box::new(left),
                    right: Box::new(right),
                })
                .unwrap()
        })
        .boxed();

    // NOTE: see comment about conjunctions just above.
    // Disjunction
    let op = just(Token::VbarVbar).to(ast::BinOp::Or);
    let disjunction = conjunction
        .clone()
        .map(|e| vec![e])
        .then(op.then(conjunction).repeated())
        .foldl(|a, (_op, b)| {
            let mut tail = vec![b];
            tail.extend(a);
            tail
        })
        .map(|xs| {
            xs.into_iter()
                .reduce(|right, left| UntypedExpr::BinOp {
                    location: left.location().union(right.location()),
                    name: ast::BinOp::Or,
                    left: Box::new(left),
                    right: Box::new(right),
                })
                .unwrap()
        })
        .boxed();

    // Pipeline
    disjunction
        .clone()
        .then(
            choice((just(Token::Pipe), just(Token::NewLinePipe)))
                .then(disjunction)
                .repeated(),
        )
        .foldl(|l, (pipe, r)| {
            if let UntypedExpr::PipeLine {
                mut expressions,
                one_liner,
            } = l
            {
                expressions.push(r);
                return UntypedExpr::PipeLine {
                    expressions,
                    one_liner,
                };
            }

            let mut expressions = Vec1::new(l);
            expressions.push(r);
            UntypedExpr::PipeLine {
                expressions,
                one_liner: pipe != Token::NewLinePipe,
            }
        })
}

#[cfg(test)]
mod tests {
    use crate::assert_expr;

    #[test]
    fn plus_binop() {
        assert_expr!("a + 1");
    }

    #[test]
    fn pipeline() {
        assert_expr!(
            r#"
            a + 2
            |> add_one
            |> add_one
            "#
        );
    }

    #[test]
    fn field_access() {
        assert_expr!("user.name");
    }

    #[test]
    fn function_invoke() {
        assert_expr!(
            r#"
            let x = add_one(3)

            let map_add_x = list.map(_, fn (y) { x + y })

            map_add_x([ 1, 2, 3 ])
            "#
        );
    }
}
