use std::{collections::HashMap, str::FromStr};

use combine::{
    attempt, between, choice, many1,
    parser::{
        char::{alpha_num, digit, hex_digit, space, spaces, string},
        combinator::no_partial,
    },
    skip_many1,
    stream::{position, state},
    token, ParseError, Parser, Stream,
};

use crate::{
    ast::{Constant, Name, Program, Term},
    builtins::DefaultFunction,
};

struct ParserState {
    identifiers: HashMap<String, isize>,
    current_unique: isize,
}

type StateStream<Input> = state::Stream<Input, ParserState>;

impl ParserState {
    fn new() -> Self {
        ParserState {
            identifiers: HashMap::new(),
            current_unique: 0,
        }
    }

    fn intern(&mut self, text: &str) -> isize {
        if let Some(u) = self.identifiers.get(text) {
            *u
        } else {
            let unique = self.current_unique;
            self.identifiers.insert(text.to_string(), unique);
            self.current_unique += 1;
            unique
        }
    }
}

pub fn program(src: &str) -> anyhow::Result<Program> {
    let mut parser = program_();

    let result = parser.parse(state::Stream {
        stream: position::Stream::new(src.trim()),
        state: ParserState::new(),
    });

    match result {
        Ok((program, _)) => Ok(program),
        Err(err) => Err(anyhow::anyhow!("{}", err)),
    }
}

fn program_<Input>() -> impl Parser<StateStream<Input>, Output = Program>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    let prog = string("program").with(skip_many1(space())).with(
        (version(), skip_many1(space()), term().skip(spaces()))
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

fn term<Input>() -> impl Parser<StateStream<Input>, Output = Term>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    opaque!(no_partial(choice((
        attempt(delay()),
        attempt(lambda()),
        attempt(apply()),
        attempt(constant()),
        attempt(force()),
        attempt(error()),
        attempt(builtin()),
    ))))
}

fn delay<Input>() -> impl Parser<StateStream<Input>, Output = Term>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    between(
        token('('),
        token(')'),
        string("delay")
            .with(skip_many1(space()))
            .with(term())
            .map(|term| Term::Delay(Box::new(term))),
    )
}

fn force<Input>() -> impl Parser<StateStream<Input>, Output = Term>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    between(
        token('('),
        token(')'),
        string("force")
            .with(skip_many1(space()))
            .with(term())
            .map(|term| Term::Force(Box::new(term))),
    )
}

fn lambda<Input>() -> impl Parser<StateStream<Input>, Output = Term>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    between(
        token('('),
        token(')'),
        string("lam")
            .with(skip_many1(space()))
            .with((many1(alpha_num()), skip_many1(space()), term()))
            .map_input(
                |(parameter_name, _, term): (String, _, Term), input| Term::Lambda {
                    parameter_name: Name {
                        unique: input.state.intern(&parameter_name),
                        text: parameter_name,
                    },
                    body: Box::new(term),
                },
            ),
    )
}

fn apply<Input>() -> impl Parser<StateStream<Input>, Output = Term>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    between(
        token('['),
        token(']'),
        (term().skip(skip_many1(space())), term()).map(|(function, argument)| Term::Apply {
            function: Box::new(function),
            argument: Box::new(argument),
        }),
    )
}

fn builtin<Input>() -> impl Parser<StateStream<Input>, Output = Term>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    between(
        token('('),
        token(')'),
        string("builtin")
            .with(skip_many1(space()))
            .with(many1(alpha_num()))
            .map(|builtin_name: String| {
                Term::Builtin(DefaultFunction::from_str(&builtin_name).unwrap())
            }),
    )
}

fn error<Input>() -> impl Parser<StateStream<Input>, Output = Term>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    between(
        token('('),
        token(')'),
        string("error")
            .with(skip_many1(space()))
            .map(|_| Term::Error),
    )
}

fn constant<Input>() -> impl Parser<StateStream<Input>, Output = Term>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    between(
        token('('),
        token(')'),
        string("con")
            .with(skip_many1(space()))
            .with(choice((
                attempt(constant_integer()),
                attempt(constant_bytestring()),
                attempt(constant_string()),
                attempt(constant_unit()),
                attempt(constant_bool()),
            )))
            .map(Term::Constant),
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
