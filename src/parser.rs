use combine::{
    attempt, between, choice, many1,
    parser::char::{alpha_num, digit, hex_digit, space, spaces, string},
    skip_many1, token, EasyParser, ParseError, Parser, Stream,
};

use crate::ast::{Constant, Program, Term};

pub fn program(src: &str) -> anyhow::Result<Program> {
    let mut parser = program_();
    let result = parser.easy_parse(src);

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
    choice((
        attempt(between(token('['), token(']'), apply())),
        attempt(between(
            token('('),
            token(')'),
            choice((
                attempt(delay()),
                attempt(lambda()),
                attempt(constant()),
                attempt(force()),
            )),
        )),
    ))
    .skip(spaces())
}

parser! {
    fn term_[I]()(I) -> Term
    where [I: Stream<Token = char>]
    {
        term()
    }
}

pub fn delay<Input>() -> impl Parser<Input, Output = Term>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    string("delay")
        .with(skip_many1(space()))
        .with(term_())
        .map(|term| Term::Delay(Box::new(term)))
}

pub fn force<Input>() -> impl Parser<Input, Output = Term>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    string("force")
        .with(skip_many1(space()))
        .with(term_())
        .map(|term| Term::Force(Box::new(term)))
}

pub fn lambda<Input>() -> impl Parser<Input, Output = Term>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    string("lam")
        .with(skip_many1(space()))
        .with((many1(alpha_num()), skip_many1(space()), term_()))
        .map(|(parameter_name, _, term)| Term::Lambda {
            parameter_name,
            body: Box::new(term),
        })
}

pub fn apply<Input>() -> impl Parser<Input, Output = Term>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    println!("daid");
    (term_(), skip_many1(space()), term_()).map(|(function, _, argument)| Term::Apply {
        function: Box::new(function),
        argument: Box::new(argument),
    })
}

pub fn constant<Input>() -> impl Parser<Input, Output = Term>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    string("con")
        .with(skip_many1(space()))
        .with(choice((
            attempt(constant_integer()),
            attempt(constant_bytestring()),
            attempt(constant_string()),
            attempt(constant_unit()),
            attempt(constant_bool()),
        )))
        .map(Term::Constant)
}

pub fn constant_integer<Input>() -> impl Parser<Input, Output = Constant>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    string("integer")
        .with(skip_many1(space()))
        .with(many1(digit()))
        .map(|d: String| Constant::Integer(d.parse::<i64>().unwrap()))
}

pub fn constant_bytestring<Input>() -> impl Parser<Input, Output = Constant>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    string("bytestring")
        .with(skip_many1(space()))
        .with(token('#'))
        .with(many1(hex_digit()))
        .map(|b: String| Constant::ByteString(hex::decode(b).unwrap()))
}

pub fn constant_string<Input>() -> impl Parser<Input, Output = Constant>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    string("string")
        .with(skip_many1(space()))
        .with(between(token('"'), token('"'), many1(alpha_num())))
        .map(Constant::String)
}

pub fn constant_unit<Input>() -> impl Parser<Input, Output = Constant>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    string("unit")
        .with(skip_many1(space()))
        .with(string("()"))
        .map(|_| Constant::Unit)
}

pub fn constant_bool<Input>() -> impl Parser<Input, Output = Constant>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    string("bool")
        .with(skip_many1(space()))
        .with(string("True").or(string("False")))
        .map(|b| Constant::Bool(b == "True"))
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
