use flat::en::{Encode, Encoder};

use crate::builtins::DefaultFunction;

const TERM_TAG_WIDTH: u32 = 4;

#[derive(Debug)]
pub struct Program {
    pub version: String,
    pub term: Term,
}

#[derive(Debug, Clone)]
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

pub fn encode_term_tag(tag: u8, e: &mut Encoder) -> Result<(), String> {
    safe_encode_bits(TERM_TAG_WIDTH, tag, e)
}

pub fn safe_encode_bits(num_bits: u32, byte: u8, e: &mut Encoder) -> Result<(), String> {
    if 2_u8.pow(num_bits) < byte {
        Err(format!(
            "Overflow detected, cannot fit {} in {} bits.",
            byte, num_bits
        ))
    } else {
        e.bits(num_bits as i64, byte);
        Ok(())
    }
}

#[derive(Debug, Clone)]
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

impl Encode for Program {
    fn encode(&self, e: &mut Encoder) -> Result<(), String> {
        self.version.encode(e)?;
        self.term.encode(e)?;

        Ok(())
    }
}

impl Encode for Term {
    fn encode(&self, e: &mut Encoder) -> Result<(), String> {
        match self {
            Term::Constant(constant) => {
                encode_term_tag(4, e)?;
                constant.encode(e)?;
            }
            rest => {
                todo!("Implement: {:?}", rest)
            }
        }

        Ok(())
    }
}

impl Encode for &Constant {
    fn encode(&self, e: &mut Encoder) -> Result<(), String> {
        match self {
            Constant::Integer(_) => todo!(),
            Constant::ByteString(bytes) => bytes.encode(e)?,
            Constant::String(s) => s.encode(e)?,
            Constant::Char(c) => c.encode(e)?,
            Constant::Unit => todo!(),
            Constant::Bool(b) => b.encode(e)?,
        }

        Ok(())
    }
}
