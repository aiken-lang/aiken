use crate::builtins::DefaultFunction;

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub version: (usize, usize, usize),
    pub term: Term,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    // tag: 0
    Var(String),
    // tag: 1
    Delay(Box<Term>),
    // tag: 2
    Lambda {
        parameter_name: String,
        body: Box<Term>,
    },
    // tag: 3
    Apply {
        function: Box<Term>,
        argument: Box<Term>,
    },
    // tag: 4
    Constant(Constant),
    // tag: 5
    Force(Box<Term>),
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
