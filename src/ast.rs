pub struct Program<'a> {
    pub version: String,
    pub term: &'a Term<'a>,
}

pub enum Term<'a> {
    // tag: 0
    Var(String),
    // tag: 1
    Delay(&'a Term<'a>),
    // tag: 2
    Lambda {
        parameter_name: String,
        body: &'a Term<'a>,
    },
    // tag: 3
    Apply {
        function: &'a Term<'a>,
        argument: &'a Term<'a>,
    },
    // tag: 4
    Constant(Constant),
    // tag: 5
    Force(&'a Term<'a>),
    // tag: 6
    Error(&'a Term<'a>),
    // tag: 7
    // Builtin
}

pub enum Constant {
    // TODO: figure out the right size for this
    // tag: 0
    Integer(i64),
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
