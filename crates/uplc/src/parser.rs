use std::{collections::HashMap, str::FromStr};

use combine::{
    attempt, between, choice, many1,
    parser::char::{alpha_num, digit, hex_digit, space, spaces, string},
    skip_many1,
    stream::position,
    token, EasyParser, ParseError, Parser, Stream,
};

use crate::{
    ast::{Constant, Name, Program, Term},
    builtins::DefaultFunction,
};

struct ParserState {
    identifiers: HashMap<String, isize>,
    current_unique: isize,
}

impl ParserState {
    fn new() -> Self {
        ParserState {
            identifiers: HashMap::new(),
            current_unique: 0,
        }
    }

    fn intern(&mut self, text: String) -> isize {
        if let Some(u) = self.identifiers.get(&text) {
            *u
        } else {
            let unique = self.current_unique;
            self.identifiers.insert(text, unique);
            self.current_unique += 1;
            unique
        }
    }
}

pub fn program(src: &str) -> anyhow::Result<Program> {
    let mut state = ParserState::new();
    let mut parser = program_(&mut state);

    let result = parser.easy_parse(position::Stream::new(src.trim()));

    match result {
        Ok((program, _)) => Ok(program),
        Err(err) => Err(anyhow::anyhow!("{}", err)),
    }
}

fn program_<Input>(state: &mut ParserState) -> impl Parser<Input, Output = Program>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    let prog = string("program").with(skip_many1(space())).with(
        (version(), skip_many1(space()), term(state).skip(spaces()))
            .map(|(version, _, term)| Program { version, term }),
    );

    between(token('('), token(')'), prog).skip(spaces())
}

fn version<Input>() -> impl Parser<Input, Output = (usize, usize, usize)>
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

fn term<Input>(state: &mut ParserState) -> impl Parser<Input, Output = Term>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    choice((
        attempt(delay(state)),
        attempt(lambda(state)),
        attempt(apply(state)),
        attempt(constant()),
        attempt(force(state)),
        attempt(error()),
        attempt(builtin()),
    ))
}

parser! {
    fn term_[I](state: &mut ParserState)(I) -> Term
    where [I: Stream<Token = char>]
    {
        term(state)
    }
}

fn delay<Input>(state: &mut ParserState) -> impl Parser<Input, Output = Term>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    between(
        token('('),
        token(')'),
        string("delay")
            .with(skip_many1(space()))
            .with(term_(state))
            .map(|term| Term::Delay(Box::new(term))),
    )
}

fn force<Input>(state: &mut ParserState) -> impl Parser<Input, Output = Term>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    between(
        token('('),
        token(')'),
        string("force")
            .with(skip_many1(space()))
            .with(term_(state))
            .map(|term| Term::Force(Box::new(term))),
    )
}

fn lambda<Input>(state: &mut ParserState) -> impl Parser<Input, Output = Term>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    between(
        token('('),
        token(')'),
        string("lam")
            .with(skip_many1(space()))
            .with((many1(alpha_num()), skip_many1(space()), term_(state)))
            .map(|(parameter_name, _, term)| Term::Lambda {
                parameter_name: Name {
                    text: parameter_name,
                    unique: state.intern(parameter_name),
                },
                body: Box::new(term),
            }),
    )
}

fn apply<Input>(state: &mut ParserState) -> impl Parser<Input, Output = Term>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    between(
        token('['),
        token(']'),
        (term_(state).skip(skip_many1(space())), term_(state)).map(|(function, argument)| {
            Term::Apply {
                function: Box::new(function),
                argument: Box::new(argument),
            }
        }),
    )
}

pub fn builtin<Input>() -> impl Parser<Input, Output = Term>
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

pub fn error<Input>() -> impl Parser<Input, Output = Term>
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

pub fn constant<Input>() -> impl Parser<Input, Output = Term>
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

fn constant_integer<Input>() -> impl Parser<Input, Output = Constant>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    string("integer")
        .with(skip_many1(space()))
        .with(many1(digit()))
        .map(|d: String| Constant::Integer(d.parse::<isize>().unwrap()))
}

fn constant_bytestring<Input>() -> impl Parser<Input, Output = Constant>
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

fn constant_string<Input>() -> impl Parser<Input, Output = Constant>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    string("string")
        .with(skip_many1(space()))
        .with(between(token('"'), token('"'), many1(alpha_num())))
        .map(Constant::String)
}

fn constant_unit<Input>() -> impl Parser<Input, Output = Constant>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    string("unit")
        .with(skip_many1(space()))
        .with(string("()"))
        .map(|_| Constant::Unit)
}

fn constant_bool<Input>() -> impl Parser<Input, Output = Constant>
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
