use combine::{
    between,
    parser::char::{digit, spaces, string},
    sep_by, token, ParseError, Parser, Stream,
};

use crate::ast::{Program, Term};

pub fn program(src: &str) -> anyhow::Result<Program> {
    let mut parser = program_();
    let result = parser.parse(src);

    match result {
        Ok((program, _)) => Ok(program),
        Err(err) => Err(anyhow::anyhow!("{}", err)),
    }
}

pub fn program_<'a, Input>() -> impl Parser<Input, Output = Program<'a>>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    let prog = string("program")
        .with(spaces())
        .with((version(), spaces(), term()).map(|(version, _, term)| Program { version, term }));

    between(token('('), token(')'), prog).skip(spaces())
}

pub fn version<Input>() -> impl Parser<Input, Output = String>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    sep_by(digit(), token('.'))
}

pub fn term<'a, Input>() -> impl Parser<Input, Output = Term<'a>>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    string("var").map(|x| Term::Var(x.to_string()))
}

#[cfg(test)]
mod test {
    use combine::Parser;

    const CODE: &str = include_str!("../example/plutus-core");

    #[test]
    fn parse_program() {
        let result = super::program_().parse(CODE);

        assert!(result.is_ok());

        let program = result.unwrap().0;

        assert_eq!(program.version, "1.0.0");
    }
}
