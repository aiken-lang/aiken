use std::{collections::HashMap, str::FromStr};

use chumsky::{
    prelude::{end, filter, just, recursive, Simple},
    text::{ident, int, keyword, TextParser},
    Parser,
};

use crate::{
    ast::{Constant, Name, Program, Term, Unique},
    builtins::DefaultFunction,
};

struct ParserState {
    identifiers: HashMap<String, Unique>,
    current: Unique,
}

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

pub fn program(src: &str) -> Result<Program<Name>, Vec<Simple<char>>> {
    let parser = program_();

    parser.parse(src)
}

fn program_() -> impl Parser<char, Program<Name>, Error = Simple<char>> {
    keyword("program")
        .ignore_then(version().padded())
        .then(term())
        .map(|(version, term)| Program { version, term })
        .delimited_by(just('(').padded(), just(')').padded())
        .then_ignore(end())
}

fn version() -> impl Parser<char, (usize, usize, usize), Error = Simple<char>> {
    int(10)
        .then_ignore(just('.'))
        .then(int(10))
        .then_ignore(just('.'))
        .then(int(10))
        .map(|((major, minor), patch)| {
            (
                major.parse::<usize>().unwrap(),
                minor.parse::<usize>().unwrap(),
                patch.parse::<usize>().unwrap(),
            )
        })
}

fn term() -> impl Parser<char, Term<Name>, Error = Simple<char>> {
    recursive(|term| {
        let atom = || var().or(term.clone());

        let delay = keyword("delay")
            .ignore_then(atom().padded())
            .delimited_by(just('(').padded(), just(')').padded())
            .map(|t| dbg!(Term::Delay(Box::new(t))));

        let force = keyword("force")
            .ignore_then(atom().padded())
            .delimited_by(just('(').padded(), just(')').padded())
            .map(|t| dbg!(Term::Force(Box::new(t))));

        let lambda = keyword("lam")
            .ignore_then(name().padded())
            .then(atom())
            .delimited_by(just('(').padded(), just(')').padded())
            .map(|(parameter_name, t)| {
                dbg!(Term::Lambda {
                    parameter_name,
                    body: Box::new(t),
                })
            });

        let apply = atom()
            .padded()
            .then(atom())
            .delimited_by(just('[').padded(), just(']').padded())
            .map(|(function, argument)| {
                dbg!(Term::Apply {
                    function: Box::new(function),
                    argument: Box::new(argument),
                })
            });

        constant()
            .or(builtin())
            .or(var())
            .or(lambda)
            .or(apply)
            .or(delay)
            .or(force)
            .or(error())
    })
}

fn constant() -> impl Parser<char, Term<Name>, Error = Simple<char>> {
    keyword("con")
        .ignore_then(
            constant_integer()
                .or(constant_bytestring())
                .or(constant_string())
                .or(constant_unit())
                .or(constant_bool()),
        )
        .delimited_by(just('(').padded(), just(')').padded())
        .map(Term::Constant)
}

fn builtin() -> impl Parser<char, Term<Name>, Error = Simple<char>> {
    keyword("builtin")
        .ignore_then(ident().padded())
        .delimited_by(just('(').padded(), just(')').padded())
        .map(|builtin_name: String| {
            Term::Builtin(DefaultFunction::from_str(&builtin_name).unwrap())
        })
}

fn var() -> impl Parser<char, Term<Name>, Error = Simple<char>> {
    name().map(Term::Var)
}

fn error() -> impl Parser<char, Term<Name>, Error = Simple<char>> {
    keyword("error")
        .ignored()
        .delimited_by(just('(').padded(), just(')').padded())
        .map(|_| Term::Error)
}

fn name() -> impl Parser<char, Name, Error = Simple<char>> {
    ident().map(|text| Name {
        text,
        unique: 0.into(),
    })
}

fn constant_integer() -> impl Parser<char, Constant, Error = Simple<char>> {
    keyword("integer")
        .padded()
        .ignore_then(int(10))
        .map(|d: String| Constant::Integer(d.parse::<isize>().unwrap()))
}

fn constant_bytestring() -> impl Parser<char, Constant, Error = Simple<char>> {
    keyword("bytestring")
        .padded()
        .ignore_then(just('#'))
        .ignore_then(int(16))
        .map(|b: String| Constant::ByteString(hex::decode(b).unwrap()))
}

fn constant_string() -> impl Parser<char, Constant, Error = Simple<char>> {
    keyword("string")
        .padded()
        .ignore_then(just('"'))
        .ignore_then(filter(|c| *c != '"').repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(Constant::String)
}

fn constant_unit() -> impl Parser<char, Constant, Error = Simple<char>> {
    keyword("unit")
        .padded()
        .ignore_then(just('('))
        .ignore_then(just(')'))
        .ignored()
        .map(|_| Constant::Unit)
}

fn constant_bool() -> impl Parser<char, Constant, Error = Simple<char>> {
    keyword("bool")
        .padded()
        .ignore_then(just("True").or(just("False")))
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
