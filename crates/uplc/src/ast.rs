use crate::{builtins::DefaultFunction, debruijn::Converter};

#[derive(Debug, Clone, PartialEq)]
pub struct Program<T> {
    pub version: (usize, usize, usize),
    pub term: Term<T>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Term<T> {
    // tag: 0
    Var(T),
    // tag: 1
    Delay(Box<Term<T>>),
    // tag: 2
    Lambda {
        parameter_name: T,
        body: Box<Term<T>>,
    },
    // tag: 3
    Apply {
        function: Box<Term<T>>,
        argument: Box<Term<T>>,
    },
    // tag: 4
    Constant(Constant),
    // tag: 5
    Force(Box<Term<T>>),
    // tag: 6
    Error,
    // tag: 7
    Builtin(DefaultFunction),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Constant {
    // tag: 0
    Integer(isize),
    // tag: 1
    ByteString(Vec<u8>),
    // tag: 2
    String(String),
    // tag: 3
    Char(char),
    // tag: 4
    Unit,
    // tag: 5
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Name {
    pub text: String,
    pub unique: Unique,
}

#[derive(Debug, Clone, PartialEq, Copy, Eq, Hash)]
pub struct Unique(isize);

impl Unique {
    pub fn new(unique: isize) -> Self {
        Unique(unique)
    }

    pub fn increment(&mut self) {
        self.0 += 1;
    }
}

impl From<isize> for Unique {
    fn from(i: isize) -> Self {
        Unique(i)
    }
}

impl From<Unique> for isize {
    fn from(d: Unique) -> Self {
        d.0
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct NamedDeBruijn {
    pub text: String,
    pub index: DeBruijn,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub struct DeBruijn(usize);

impl From<usize> for DeBruijn {
    fn from(i: usize) -> Self {
        DeBruijn(i)
    }
}

impl From<DeBruijn> for usize {
    fn from(d: DeBruijn) -> Self {
        d.0
    }
}

impl From<NamedDeBruijn> for DeBruijn {
    fn from(n: NamedDeBruijn) -> Self {
        n.index
    }
}

impl From<DeBruijn> for NamedDeBruijn {
    fn from(index: DeBruijn) -> Self {
        NamedDeBruijn {
            text: String::from("i"),
            index,
        }
    }
}

impl TryFrom<Program<Name>> for Program<NamedDeBruijn> {
    type Error = String;

    fn try_from(value: Program<Name>) -> Result<Self, Self::Error> {
        Ok(Program::<NamedDeBruijn> {
            version: value.version,
            term: value.term.try_into()?,
        })
    }
}

impl TryFrom<Program<NamedDeBruijn>> for Program<Name> {
    type Error = String;

    fn try_from(value: Program<NamedDeBruijn>) -> Result<Self, Self::Error> {
        Ok(Program::<Name> {
            version: value.version,
            term: value.term.try_into()?,
        })
    }
}

impl TryFrom<Term<Name>> for Term<NamedDeBruijn> {
    type Error = String;

    fn try_from(
        value: Term<Name>,
    ) -> Result<Self, <Term<NamedDeBruijn> as TryFrom<Term<Name>>>::Error> {
        let mut converter = Converter::new();

        let term = converter
            .name_to_named_debruijn(value)
            .map_err(|err| err.to_string())?;

        Ok(term)
    }
}

impl TryFrom<Term<NamedDeBruijn>> for Term<Name> {
    type Error = String;

    fn try_from(
        value: Term<NamedDeBruijn>,
    ) -> Result<Self, <Term<Name> as TryFrom<Term<NamedDeBruijn>>>::Error> {
        let mut converter = Converter::new();

        let term = converter
            .named_debruijn_to_name(value)
            .map_err(|err| err.to_string())?;

        Ok(term)
    }
}
