use chumsky::prelude::*;

pub mod constant;
mod data_type;
mod function;
mod import;
mod test;
mod type_alias;
mod validator;

pub use constant::parser as constant;
pub use data_type::parser as data_type;
pub use function::parser as function;
pub use import::parser as import;
pub use test::parser as test;
pub use type_alias::parser as type_alias;
pub use validator::parser as validator;

use super::{error::ParseError, token::Token};
use crate::ast;

pub fn parser() -> impl Parser<Token, Vec<ast::UntypedDefinition>, Error = ParseError> {
    choice((
        import(),
        data_type(),
        type_alias(),
        validator(),
        function(),
        test(),
        constant(),
    ))
    .repeated()
    .then_ignore(end())
}
