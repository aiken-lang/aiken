use anyhow::anyhow;
use flat::en::{Encode, Encoder};

use crate::builtins::DefaultFunction;

const TERM_TAG_WIDTH: u32 = 4;
const CONST_TAG_WIDTH: u32 = 4;

#[derive(Debug, Clone)]
pub struct Program {
    pub version: String,
    pub term: Term,
}

impl Program {
    pub fn flat(&self) -> anyhow::Result<Vec<u8>> {
        let bytes = flat::encode(self.clone()).map_err(|err| anyhow!("{}", err))?;

        Ok(bytes)
    }

    pub fn flat_hex(&self) -> anyhow::Result<String> {
        let bytes = self.flat()?;

        let hex = hex::encode(&bytes);

        Ok(hex)
    }
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

pub fn encode_constant(tag: u8, e: &mut Encoder) -> Result<(), String> {
    e.encode_list_with(encode_constant_tag, [tag].to_vec())
}

pub fn encode_constant_tag(tag: u8, e: &mut Encoder) -> Result<(), String> {
    safe_encode_bits(CONST_TAG_WIDTH, tag, e)
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
        // still need annotation but here we have the term tags
        match self {
            Term::Var(name) => {
                encode_term_tag(0, e)?;
                name.encode(e)?;
            }
            Term::Delay(term) => {
                encode_term_tag(1, e)?;
                term.encode(e)?;
            }
            Term::Lambda {
                parameter_name,
                body,
            } => {
                encode_term_tag(2, e)?;
                // need to create encoding for Binder
                todo!();
                body.encode(e)?;
            }
            Term::Apply { function, argument } => {
                encode_term_tag(3, e)?;
                function.encode(e)?;
                argument.encode(e)?;
            }

            Term::Constant(constant) => {
                encode_term_tag(4, e)?;
                constant.encode(e)?;
            }

            Term::Force(term) => {
                encode_term_tag(5, e)?;
                term.encode(e)?;
            }

            Term::Error => {
                encode_term_tag(6, e)?;
                todo!()
            }
            Term::Builtin(b) => {
                encode_term_tag(7, e)?;
                // implement encode for builtins
                todo!()
            }
        }

        Ok(())
    }
}

impl Encode for &Constant {
    fn encode(&self, e: &mut Encoder) -> Result<(), String> {
        match self {
            Constant::Integer(i) => {
                encode_constant(0, e)?;
                i.encode(e)?;
            }
            Constant::ByteString(bytes) => {
                encode_constant(1, e)?;
                bytes.encode(e)?;
            }
            Constant::String(s) => {
                encode_constant(2, e)?;
                s.encode(e)?;
            }
            // there is no char constant tag
            Constant::Char(c) => {
                c.encode(e)?;
                todo!()
            }
            Constant::Unit => encode_constant(3, e)?,
            Constant::Bool(b) => {
                encode_constant(4, e)?;
                b.encode(e)?;
            }
        }

        Ok(())
    }
}
