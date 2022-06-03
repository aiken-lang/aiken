use crate::builtins::DefaultFunction;

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
    pub unique: isize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NamedDeBruijn {
    pub text: String,
    pub index: isize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DeBruijn(isize);
