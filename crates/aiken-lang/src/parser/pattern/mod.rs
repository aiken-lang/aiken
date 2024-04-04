use chumsky::prelude::*;

mod constructor;
mod discard;
mod int;
mod list;
mod pair;
mod tuple;
mod var;

use crate::{
    ast::UntypedPattern,
    parser::{error::ParseError, token::Token},
};
pub use constructor::parser as constructor;
pub use discard::parser as discard;
pub use int::parser as int;
pub use list::parser as list;
pub use pair::parser as pair;
pub use tuple::parser as tuple;
pub use var::parser as var;

pub fn parser() -> impl Parser<Token, UntypedPattern, Error = ParseError> {
    recursive(|pattern| {
        choice((
            var(pattern.clone()),
            pair(pattern.clone()),
            constructor(pattern.clone()),
            discard(),
            int(),
            tuple(pattern.clone()),
            list(pattern),
        ))
        .then(
            just(Token::As)
                .ignore_then(select! { Token::Name {name} => name})
                .or_not(),
        )
        .map_with_span(|(pattern, opt_as), span| {
            if let Some(name) = opt_as {
                UntypedPattern::Assign {
                    name,
                    location: span,
                    pattern: Box::new(pattern),
                }
            } else {
                pattern
            }
        })
    })
}

#[cfg(test)]
mod tests {
    use crate::assert_pattern;

    #[test]
    fn pattern_var() {
        assert_pattern!("foo");
    }

    #[test]
    fn pattern_discard_unnamed() {
        assert_pattern!("_");
    }

    #[test]
    fn pattern_discard_named() {
        assert_pattern!("_foo");
    }

    #[test]
    fn pattern_pair_discards() {
        assert_pattern!("Pair(_, _)");
    }

    #[test]
    fn pattern_pair_explicit_depth_1() {
        assert_pattern!("Pair(14, True)");
    }

    #[test]
    fn pattern_pair_explicit_depth_2() {
        assert_pattern!("Pair([1,2,3], Pair((14, 42), _))");
    }

    #[test]
    fn pattern_constructor_no_labels() {
        assert_pattern!("Foo(a, b)");
    }

    #[test]
    fn pattern_constructor_labels() {
        assert_pattern!("Foo { a, b }");
    }

    #[test]
    fn pattern_constructor_spread() {
        assert_pattern!("Foo { a, .. }");
    }

    #[test]
    fn pattern_constructor_pair_interleaved() {
        assert_pattern!("Foo(a, Pair(1, 2))");
    }

    #[test]
    fn pattern_list_spread() {
        assert_pattern!("[head, ..]");
    }
}
