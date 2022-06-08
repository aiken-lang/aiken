use std::{collections::HashMap, str::FromStr};

use combine::{
    attempt, between, choice,
    error::StringStreamError,
    look_ahead, many1,
    parser::{
        char::{alpha_num, digit, hex_digit, letter, space, spaces, string},
        combinator::no_partial,
    },
    skip_many1,
    stream::{position, state},
    token, ParseError, Parser, Stream,
};

use crate::{
    ast::{Constant, Name, Program, Term, Unique},
    builtins::DefaultFunction,
};

struct ParserState {
    identifiers: HashMap<String, Unique>,
    current: Unique,
}

type StateStream<Input> = state::Stream<Input, ParserState>;

impl ParserState {
    fn new() -> Self {
        ParserState {
            identifiers: HashMap::new(),
            current: Unique::new(0),
        }
    }

    fn intern(&mut self, text: &str) -> Unique {
        if let Some(u) = self.identifiers.get(text) {
            *u
        } else {
            let unique = self.current;

            self.identifiers.insert(text.to_string(), unique);

            self.current.increment();

            unique
        }
    }
}

pub fn program(src: &str) -> Result<Program<Name>, StringStreamError> {
    let mut parser = program_();

    let (program, _) = parser.parse(state::Stream {
        stream: position::Stream::new(src.trim()),
        state: ParserState::new(),
    })?;

    Ok(program)
}

fn program_<Input>() -> impl Parser<StateStream<Input>, Output = Program<Name>>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    let prog = string("program").with(skip_many1(space())).with(
        (version(), skip_many1(space()), term())
            .map(|(version, _, term)| Program { version, term }),
    );

    between(token('('), token(')'), prog).skip(spaces())
}

fn version<Input>() -> impl Parser<StateStream<Input>, Output = (usize, usize, usize)>
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
                (
                    major.parse::<usize>().unwrap(),
                    minor.parse::<usize>().unwrap(),
                    patch.parse::<usize>().unwrap(),
                )
            },
        )
}

fn term<Input>() -> impl Parser<StateStream<Input>, Output = Term<Name>>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    opaque!(no_partial(
        choice((
            attempt(constant()),
            attempt(builtin()),
            attempt(var()),
            attempt(lambda()),
            attempt(apply()),
            attempt(delay()),
            attempt(force()),
            attempt(error()),
        ))
        .skip(spaces())
    ))
}

fn var<Input>() -> impl Parser<StateStream<Input>, Output = Term<Name>>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    name().map(Term::Var)
}

fn delay<Input>() -> impl Parser<StateStream<Input>, Output = Term<Name>>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    between(
        token('('),
        token(')'),
        (spaces(), string("delay"), skip_many1(space()), term())
            .map(|(_, _, _, term)| Term::Delay(Box::new(term))),
    )
}

fn force<Input>() -> impl Parser<StateStream<Input>, Output = Term<Name>>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    between(
        token('('),
        token(')'),
        (spaces(), string("force"), skip_many1(space()), term())
            .map(|(_, _, _, term)| Term::Force(Box::new(term))),
    )
}

fn lambda<Input>() -> impl Parser<StateStream<Input>, Output = Term<Name>>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    between(
        token('('),
        token(')'),
        (
            spaces(),
            string("lam"),
            skip_many1(space()),
            name(),
            skip_many1(space()),
            term(),
        )
            .map(|(_, _, _, parameter_name, _, term)| Term::Lambda {
                parameter_name,
                body: Box::new(term),
            }),
    )
}

fn apply<Input>() -> impl Parser<StateStream<Input>, Output = Term<Name>>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    between(
        token('['),
        token(']'),
        (spaces(), term(), spaces(), term()).map(|(_, function, _, argument)| Term::Apply {
            function: Box::new(function),
            argument: Box::new(argument),
        }),
    )
}

fn builtin<Input>() -> impl Parser<StateStream<Input>, Output = Term<Name>>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    between(
        token('(').skip(spaces()),
        token(')'),
        string("builtin")
            .with(skip_many1(space()))
            .with(many1(alpha_num()))
            .map(|builtin_name: String| {
                Term::Builtin(DefaultFunction::from_str(&builtin_name).unwrap())
            }),
    )
}

fn error<Input>() -> impl Parser<StateStream<Input>, Output = Term<Name>>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    between(
        token('(').skip(spaces()),
        token(')'),
        string("error")
            .with(skip_many1(space()))
            .map(|_| Term::Error),
    )
}

fn constant<Input>() -> impl Parser<StateStream<Input>, Output = Term<Name>>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    between(
        token('(').skip(spaces()),
        token(')'),
        (
            spaces(),
            string("con"),
            skip_many1(space()),
            choice((
                attempt(constant_integer()),
                attempt(constant_bytestring()),
                attempt(constant_string()),
                attempt(constant_unit()),
                attempt(constant_bool()),
            )),
            spaces(),
        )
            .map(|(_, _, _, con, _)| Term::Constant(con)),
    )
}

fn constant_integer<Input>() -> impl Parser<StateStream<Input>, Output = Constant>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    string("integer")
        .with(skip_many1(space()))
        .with(many1(digit()))
        .map(|d: String| Constant::Integer(d.parse::<isize>().unwrap()))
}

fn constant_bytestring<Input>() -> impl Parser<StateStream<Input>, Output = Constant>
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

fn constant_string<Input>() -> impl Parser<StateStream<Input>, Output = Constant>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    string("string")
        .with(skip_many1(space()))
        .with(between(token('"'), token('"'), many1(alpha_num())))
        .map(Constant::String)
}

fn constant_unit<Input>() -> impl Parser<StateStream<Input>, Output = Constant>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    string("unit")
        .with(skip_many1(space()))
        .with(string("()"))
        .map(|_| Constant::Unit)
}

fn constant_bool<Input>() -> impl Parser<StateStream<Input>, Output = Constant>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    string("bool")
        .with(skip_many1(space()))
        .with(string("True").or(string("False")))
        .map(|b| Constant::Bool(b == "True"))
}

fn name<Input>() -> impl Parser<StateStream<Input>, Output = Name>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    look_ahead(letter())
        .with(many1(alpha_num().or(token('_').or(token('\'')))))
        .map_input(|text: String, input: &mut StateStream<Input>| Name {
            unique: input.state.intern(&text),
            text,
        })
}

#[cfg(test)]
mod test {
    #[test]
    fn parse_program() {
        let code = r#"
        (program 11.22.33
            (con integer 11)
        )
        "#;
        let result = super::program(code);

        assert!(result.is_ok());

        let program = result.unwrap();

        assert_eq!(program.version, (11, 22, 33));
    }
}
