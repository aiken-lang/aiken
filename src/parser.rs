use combine::{
    between, many1,
    parser::char::{digit, spaces, string},
    token, ParseError, Parser, Stream,
};

use crate::ast::{Constant, Program, Term};

pub fn program(src: &str) -> anyhow::Result<Program> {
    let mut parser = program_();
    let result = parser.parse(src);

    match result {
        Ok((program, _)) => Ok(program),
        Err(err) => Err(anyhow::anyhow!("{}", err)),
    }
}

pub fn program_<Input>() -> impl Parser<Input, Output = Program>
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
    (
        many1(digit()),
        token('.'),
        many1(digit()),
        token('.'),
        many1(digit()),
    )
        .map(
            |(major, _, minor, _, patch): (String, char, String, char, String)| {
                format!("{}.{}.{}", major, minor, patch)
            },
        )
}

pub fn term<Input>() -> impl Parser<Input, Output = Term>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    between(token('('), token(')'), constant()).skip(spaces())
}

pub fn delay<Input>() -> impl Parser<Input, Output = Term>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    string("delay")
        .skip(spaces())
        .with(term())
        .map(|term| Term::Delay(Box::new(term)))
}

pub fn constant<Input>() -> impl Parser<Input, Output = Term>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    string("con")
        .skip(spaces())
        .with(constant_integer().or(unit()).or(constant_bool()))
        .map(Term::Constant)
}

pub fn constant_integer<Input>() -> impl Parser<Input, Output = Constant>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    string("integer")
        .skip(spaces())
        .with(many1(digit()))
        .map(|d: String| Constant::Integer(d.parse::<i64>().unwrap()))
}

pub fn constant_bool<Input>() -> impl Parser<Input, Output = Constant>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    string("bool")
        .skip(spaces())
        .with(string("True").or(string("False")))
        .map(|b| Constant::Bool(b == "True"))
}

pub fn unit<Input>() -> impl Parser<Input, Output = Constant>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    string("unit")
        .skip(spaces())
        .with(string("()"))
        .map(|_| Constant::Unit)
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
