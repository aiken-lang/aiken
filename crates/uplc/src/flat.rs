use std::fmt::Debug;

use flat_rs::{
    de::{self, Decode, Decoder},
    en::{self, Encode, Encoder},
    Flat,
};
use pallas_primitives::Fragment;

use crate::{
    ast::{
        Constant, DeBruijn, FakeNamedDeBruijn, Name, NamedDeBruijn, Program, Term, Type, Unique,
    },
    builtins::DefaultFunction,
};

const BUILTIN_TAG_WIDTH: u32 = 7;
const CONST_TAG_WIDTH: u32 = 4;
const TERM_TAG_WIDTH: u32 = 4;

pub trait Binder<'b>: Encode + Decode<'b> {
    fn binder_encode(&self, e: &mut Encoder) -> Result<(), en::Error>;
    fn binder_decode(d: &mut Decoder) -> Result<Self, de::Error>;
    fn text(&self) -> &str;
}

impl<'b, T> Flat<'b> for Program<T> where T: Binder<'b> + Debug {}

impl<'b, T> Program<T>
where
    T: Binder<'b> + Debug,
{
    // convenient so that people don't need to depend on the flat crate
    // directly to call programs flat function
    pub fn to_flat(&self) -> Result<Vec<u8>, en::Error> {
        self.flat()
    }

    pub fn to_cbor(&self) -> Result<Vec<u8>, en::Error> {
        let flat_bytes = self.flat()?;

        minicbor::to_vec(&flat_bytes).map_err(|err| en::Error::Message(err.to_string()))
    }

    pub fn to_hex(&self) -> Result<String, en::Error> {
        let bytes = self.to_cbor()?;

        let hex = hex::encode(&bytes);

        Ok(hex)
    }

    pub fn from_flat(bytes: &'b [u8]) -> Result<Self, de::Error> {
        Self::unflat(bytes)
    }
}

impl<'b, T> Encode for Program<T>
where
    T: Binder<'b> + Debug,
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
    T: Binder<'b> + Debug,
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

/// Integers are typically smaller so we save space
/// by encoding them in 7 bits and this allows it to be byte alignment agnostic.
/// Strings and bytestrings span multiple bytes so using bytestring is
/// the most effective encoding.
/// i.e. A 17 or greater length byte array loses efficiency being encoded as
/// a unsigned integer instead of a byte array
impl Encode for Constant {
    fn encode(&self, e: &mut Encoder) -> Result<(), en::Error> {
        match self {
            Constant::Integer(i) => {
                encode_constant(&[0], e)?;
                i.encode(e)?;
            }

            Constant::ByteString(bytes) => {
                encode_constant(&[1], e)?;
                bytes.encode(e)?;
            }
            Constant::String(s) => {
                encode_constant(&[2], e)?;
                s.encode(e)?;
            }
            Constant::Unit => encode_constant(&[3], e)?,
            Constant::Bool(b) => {
                encode_constant(&[4], e)?;
                b.encode(e)?;
            }
            Constant::ProtoList(typ, list) => {
                let mut type_encode = vec![7, 5];

                encode_type(typ, &mut type_encode);

                encode_constant(&type_encode, e)?;

                e.encode_list_with(list, encode_constant_value)?;
            }
            Constant::ProtoPair(type1, type2, a, b) => {
                let mut type_encode = vec![7, 7, 6];

                encode_type(type1, &mut type_encode);

                encode_type(type2, &mut type_encode);

                encode_constant(&type_encode, e)?;
                encode_constant_value(a, e)?;
                encode_constant_value(b, e)?;
            }
            Constant::Data(data) => {
                encode_constant(&[8], e)?;

                let cbor = data
                    .encode_fragment()
                    .map_err(|err| en::Error::Message(err.to_string()))?;

                cbor.encode(e)?;
            }
        }

        Ok(())
    }
}

fn encode_constant_value(x: &Constant, e: &mut Encoder) -> Result<(), en::Error> {
    match x {
        Constant::Integer(x) => x.encode(e),
        Constant::ByteString(b) => b.encode(e),
        Constant::String(s) => s.encode(e),
        Constant::Unit => Ok(()),
        Constant::Bool(b) => b.encode(e),
        Constant::ProtoList(_, list) => {
            e.encode_list_with(list, encode_constant_value)?;
            Ok(())
        }
        Constant::ProtoPair(_, _, a, b) => {
            encode_constant_value(a, e)?;

            encode_constant_value(b, e)
        }
        Constant::Data(data) => {
            let cbor = data
                .encode_fragment()
                .map_err(|err| en::Error::Message(err.to_string()))?;

            cbor.encode(e)
        }
    }
}

fn encode_type(typ: &Type, bytes: &mut Vec<u8>) {
    match typ {
        Type::Bool => bytes.push(4),
        Type::Integer => bytes.push(0),
        Type::String => bytes.push(2),
        Type::ByteString => bytes.push(1),
        Type::Unit => bytes.push(3),
        Type::List(sub_typ) => {
            bytes.extend(vec![7, 5]);
            encode_type(sub_typ, bytes);
        }
        Type::Pair(type1, type2) => {
            bytes.extend(vec![7, 7, 6]);
            encode_type(type1, bytes);
            encode_type(type2, bytes);
        }
        Type::Data => bytes.push(8),
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

    fn text(&self) -> &str {
        &self.text
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

    fn text(&self) -> &str {
        &self.text
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

    fn text(&self) -> &str {
        "i"
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

    fn text(&self) -> &str {
        &self.0.text
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

pub fn encode_constant(tag: &[u8], e: &mut Encoder) -> Result<(), en::Error> {
    e.encode_list_with(tag, encode_constant_tag)?;

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

pub fn encode_constant_tag(tag: &u8, e: &mut Encoder) -> Result<(), en::Error> {
    safe_encode_bits(CONST_TAG_WIDTH, *tag, e)
}

pub fn decode_constant_tag(d: &mut Decoder) -> Result<u8, de::Error> {
    d.bits8(CONST_TAG_WIDTH as usize)
}

#[cfg(test)]
mod test {
    use flat_rs::Flat;

    use crate::ast::{Name, Type};

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
    fn flat_encode_list_list_integer() {
        let program = Program::<Name> {
            version: (1, 0, 0),
            term: Term::Constant(Constant::ProtoList(
                Type::List(Box::new(Type::Integer)),
                vec![
                    Constant::ProtoList(Type::Integer, vec![Constant::Integer(7)]),
                    Constant::ProtoList(Type::Integer, vec![Constant::Integer(5)]),
                ],
            )),
        };

        let bytes = program.to_flat().unwrap();

        assert_eq!(
            bytes,
            vec![
                0b00000001, 0b00000000, 0b00000000, 0b01001011, 0b11010110, 0b11110101, 0b10000011,
                0b00001110, 0b01100001, 0b01000001
            ]
        )
    }

    #[test]
    fn flat_encode_pair_pair_integer_bool_integer() {
        let program = Program::<Name> {
            version: (1, 0, 0),
            term: Term::Constant(Constant::ProtoPair(
                Type::Pair(Box::new(Type::Integer), Box::new(Type::Bool)),
                Type::Integer,
                Box::new(Constant::ProtoPair(
                    Type::Integer,
                    Type::Bool,
                    Box::new(Constant::Integer(11)),
                    Box::new(Constant::Bool(true)),
                )),
                Box::new(Constant::Integer(11)),
            )),
        };

        let bytes = program.to_flat().unwrap();

        assert_eq!(
            bytes,
            vec![
                0b00000001, 0b00000000, 0b00000000, 0b01001011, 0b11011110, 0b11010111, 0b10111101,
                0b10100001, 0b01001000, 0b00000101, 0b10100010, 0b11000001
            ]
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
