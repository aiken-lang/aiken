use chumsky::prelude::*;

pub mod benchmark;
pub mod constant;
mod data_type;
mod function;
pub mod import;
mod test;
pub mod test_like;
mod type_alias;
mod validator;

use super::{error::ParseError, token::Token};
use crate::ast;
pub use benchmark::parser as benchmark;
pub use constant::parser as constant;
pub use data_type::parser as data_type;
pub use function::parser as function;
pub use test::parser as test;
pub use type_alias::parser as type_alias;
pub use validator::parser as validator;

pub fn parser() -> impl Parser<Token, ast::UntypedDefinition, Error = ParseError> {
    choice((
        data_type(),
        type_alias(),
        validator(),
        function(),
        test(),
        benchmark(),
        constant(),
    ))
}

/// Keep declaration decorators separate from generator-facing definition structs.
pub fn with_decorators()
-> impl Parser<Token, (ast::UntypedDefinition, Vec<ast::Decorator>), Error = ParseError> {
    choice((
        data_type().map(|definition| (definition, Vec::new())),
        data_type::decorators()
            .then(choice((
                type_alias(),
                validator(),
                function(),
                test(),
                benchmark(),
                constant(),
            )))
            .map(|(decorators, definition)| (definition, decorators)),
    ))
}

pub fn decorated_import()
-> impl Parser<Token, (ast::Use<()>, Vec<ast::Decorator>), Error = ParseError> {
    data_type::decorators()
        .then(import::parser())
        .map(|(decorators, import)| (import, decorators))
}
