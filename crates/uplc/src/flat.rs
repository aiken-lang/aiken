use flat::{
    de::{self, Decode, Decoder},
    en::{self, Encode, Encoder},
    Flat,
};

use crate::{
    ast::{Constant, DeBruijn, FakeNamedDeBruijn, Name, NamedDeBruijn, Program, Term, Unique},
    builtins::DefaultFunction,
};

const BUILTIN_TAG_WIDTH: u32 = 7;
const CONST_TAG_WIDTH: u32 = 4;
const TERM_TAG_WIDTH: u32 = 4;

pub trait Binder<'b>: Encode + Decode<'b> {
    fn binder_encode(&self, e: &mut Encoder) -> Result<(), en::Error>;
    fn binder_decode(d: &mut Decoder) -> Result<Self, de::Error>;
}

impl<'b, T> Flat<'b> for Program<T> where T: Binder<'b> {}

impl<'b, T> Program<T>
where
    T: Binder<'b>,
{
    // convenient so that people don't need to depend on the flat crate
    // directly to call programs flat function
    pub fn to_flat(&self) -> Result<Vec<u8>, en::Error> {
        self.flat()
    }

    pub fn flat_hex(&self) -> Result<String, en::Error> {
        let bytes = self.flat()?;

        let hex = hex::encode(&bytes);

        Ok(hex)
    }
}

impl<'b, T> Encode for Program<T>
where
    T: Binder<'b>,
{
    fn encode(&self, e: &mut Encoder) -> Result<(), en::Error> {
        let (major, minor, patch) = self.version;

        major.encode(e)?;
        minor.encode(e)?;
        patch.encode(e)?;

        self.term.encode(e)?;

        Ok(())
    }
}

impl<'b, T> Decode<'b> for Program<T>
where
    T: Binder<'b>,
{
    fn decode(d: &mut Decoder) -> Result<Self, de::Error> {
        let version = (usize::decode(d)?, usize::decode(d)?, usize::decode(d)?);
        let term = Term::decode(d)?;
        Ok(Program { version, term })
    }
}

impl<'b, T> Encode for Term<T>
where
    T: Binder<'b>,
{
    fn encode(&self, e: &mut Encoder) -> Result<(), en::Error> {
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
                parameter_name.binder_encode(e)?;
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
            }
            Term::Builtin(builtin) => {
                encode_term_tag(7, e)?;

                builtin.encode(e)?;
            }
        }

        Ok(())
    }
}

impl<'b, T> Decode<'b> for Term<T>
where
    T: Binder<'b>,
{
    fn decode(d: &mut Decoder) -> Result<Self, de::Error> {
        match decode_term_tag(d)? {
            0 => Ok(Term::Var(T::decode(d)?)),
            1 => Ok(Term::Delay(Box::new(Term::decode(d)?))),
            2 => Ok(Term::Lambda {
                parameter_name: T::binder_decode(d)?,
                body: Box::new(Term::decode(d)?),
            }),
            3 => Ok(Term::Apply {
                function: Box::new(Term::decode(d)?),
                argument: Box::new(Term::decode(d)?),
            }),
            // Need size limit for Constant
            4 => Ok(Term::Constant(Constant::decode(d)?)),
            5 => Ok(Term::Force(Box::new(Term::decode(d)?))),
            6 => Ok(Term::Error),
            7 => Ok(Term::Builtin(DefaultFunction::decode(d)?)),
            x => Err(de::Error::Message(format!(
                "Unknown term constructor tag: {}",
                x
            ))),
        }
    }
}

impl Encode for &Constant {
    fn encode(&self, e: &mut Encoder) -> Result<(), en::Error> {
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
                let mut b = [0; 4];

                let s = c.encode_utf8(&mut b);

                s.as_bytes().encode(e)?;
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

impl<'b> Decode<'b> for Constant {
    fn decode(d: &mut Decoder) -> Result<Self, de::Error> {
        match decode_constant(d)? {
            0 => Ok(Constant::Integer(isize::decode(d)?)),
            1 => Ok(Constant::ByteString(Vec::<u8>::decode(d)?)),
            2 => Ok(Constant::String(String::decode(d)?)),
            3 => Ok(Constant::Unit),
            4 => Ok(Constant::Bool(bool::decode(d)?)),
            x => Err(de::Error::Message(format!(
                "Unknown constant constructor tag: {}",
                x
            ))),
        }
    }
}

impl Encode for Unique {
    fn encode(&self, e: &mut Encoder) -> Result<(), en::Error> {
        isize::from(*self).encode(e)?;

        Ok(())
    }
}

impl<'b> Decode<'b> for Unique {
    fn decode(d: &mut Decoder) -> Result<Self, de::Error> {
        Ok(isize::decode(d)?.into())
    }
}

impl Encode for Name {
    fn encode(&self, e: &mut Encoder) -> Result<(), en::Error> {
        self.text.encode(e)?;
        self.unique.encode(e)?;

        Ok(())
    }
}

impl<'b> Decode<'b> for Name {
    fn decode(d: &mut Decoder) -> Result<Self, de::Error> {
        Ok(Name {
            text: String::decode(d)?,
            unique: Unique::decode(d)?,
        })
    }
}

impl<'b> Binder<'b> for Name {
    fn binder_encode(&self, e: &mut Encoder) -> Result<(), en::Error> {
        self.encode(e)?;

        Ok(())
    }

    fn binder_decode(d: &mut Decoder) -> Result<Self, de::Error> {
        Name::decode(d)
    }
}

impl Encode for NamedDeBruijn {
    fn encode(&self, e: &mut Encoder) -> Result<(), en::Error> {
        self.text.encode(e)?;
        self.index.encode(e)?;

        Ok(())
    }
}

impl<'b> Decode<'b> for NamedDeBruijn {
    fn decode(d: &mut Decoder) -> Result<Self, de::Error> {
        Ok(NamedDeBruijn {
            text: String::decode(d)?,
            index: DeBruijn::decode(d)?,
        })
    }
}

impl<'b> Binder<'b> for NamedDeBruijn {
    fn binder_encode(&self, e: &mut Encoder) -> Result<(), en::Error> {
        self.text.encode(e)?;

        Ok(())
    }

    fn binder_decode(d: &mut Decoder) -> Result<Self, de::Error> {
        Ok(NamedDeBruijn {
            text: String::decode(d)?,
            index: DeBruijn::new(0),
        })
    }
}

impl Encode for DeBruijn {
    fn encode(&self, e: &mut Encoder) -> Result<(), en::Error> {
        usize::from(*self).encode(e)?;

        Ok(())
    }
}

impl<'b> Decode<'b> for DeBruijn {
    fn decode(d: &mut Decoder) -> Result<Self, de::Error> {
        Ok(usize::decode(d)?.into())
    }
}

impl<'b> Binder<'b> for DeBruijn {
    fn binder_encode(&self, _: &mut Encoder) -> Result<(), en::Error> {
        Ok(())
    }

    fn binder_decode(_d: &mut Decoder) -> Result<Self, de::Error> {
        Ok(DeBruijn::new(0))
    }
}

impl Encode for FakeNamedDeBruijn {
    fn encode(&self, e: &mut Encoder) -> Result<(), en::Error> {
        let index: DeBruijn = self.clone().into();

        index.encode(e)?;

        Ok(())
    }
}

impl<'b> Decode<'b> for FakeNamedDeBruijn {
    fn decode(d: &mut Decoder) -> Result<Self, de::Error> {
        let index = DeBruijn::decode(d)?;

        Ok(index.into())
    }
}

impl<'b> Binder<'b> for FakeNamedDeBruijn {
    fn binder_encode(&self, _: &mut Encoder) -> Result<(), en::Error> {
        Ok(())
    }

    fn binder_decode(_d: &mut Decoder) -> Result<Self, de::Error> {
        let index = DeBruijn::new(0);

        Ok(index.into())
    }
}

impl Encode for DefaultFunction {
    fn encode(&self, e: &mut Encoder) -> Result<(), en::Error> {
        e.bits(BUILTIN_TAG_WIDTH as i64, *self as u8);

        Ok(())
    }
}

impl<'b> Decode<'b> for DefaultFunction {
    fn decode(d: &mut Decoder) -> Result<Self, de::Error> {
        let builtin_tag = d.bits8(BUILTIN_TAG_WIDTH as usize)?;
        builtin_tag.try_into()
    }
}

fn encode_term_tag(tag: u8, e: &mut Encoder) -> Result<(), en::Error> {
    safe_encode_bits(TERM_TAG_WIDTH, tag, e)
}

fn decode_term_tag(d: &mut Decoder) -> Result<u8, de::Error> {
    d.bits8(TERM_TAG_WIDTH as usize)
}

fn safe_encode_bits(num_bits: u32, byte: u8, e: &mut Encoder) -> Result<(), en::Error> {
    if 2_u8.pow(num_bits) < byte {
        Err(en::Error::Message(format!(
            "Overflow detected, cannot fit {} in {} bits.",
            byte, num_bits
        )))
    } else {
        e.bits(num_bits as i64, byte);
        Ok(())
    }
}

pub fn encode_constant(tag: u8, e: &mut Encoder) -> Result<(), en::Error> {
    e.encode_list_with([tag].to_vec(), encode_constant_tag)?;

    Ok(())
}

pub fn decode_constant(d: &mut Decoder) -> Result<u8, de::Error> {
    let u8_list = d.decode_list_with(decode_constant_tag)?;
    if u8_list.len() > 1 {
        Err(de::Error::Message(
            "Improper encoding on constant tag. Should be list of one item encoded in 4 bits"
                .to_string(),
        ))
    } else {
        Ok(u8_list[0])
    }
}

pub fn encode_constant_tag(tag: u8, e: &mut Encoder) -> Result<(), en::Error> {
    safe_encode_bits(CONST_TAG_WIDTH, tag, e)
}

pub fn decode_constant_tag(d: &mut Decoder) -> Result<u8, de::Error> {
    d.bits8(CONST_TAG_WIDTH as usize)
}

#[cfg(test)]
mod test {
    use flat::Flat;

    use crate::ast::Name;

    use super::{Constant, Program, Term};

    #[test]
    fn flat_encode_integer() {
        let program = Program::<Name> {
            version: (11, 22, 33),
            term: Term::Constant(Constant::Integer(11)),
        };

        let bytes = program.to_flat().unwrap();

        assert_eq!(
            bytes,
            vec![0b00001011, 0b00010110, 0b00100001, 0b01001000, 0b00000101, 0b10000001]
        )
    }

    #[test]
    fn flat_decode_integer() {
        let flat_encoded = vec![
            0b00001011, 0b00010110, 0b00100001, 0b01001000, 0b00000101, 0b10000001,
        ];

        let expected_program = Program {
            version: (11, 22, 33),
            term: Term::Constant(Constant::Integer(11)),
        };

        let actual_program: Program<Name> = Program::unflat(&flat_encoded).unwrap();

        assert_eq!(actual_program, expected_program)
    }
}
