use std::{collections::VecDeque, fmt::Debug, rc::Rc};

use anyhow::anyhow;
use flat_rs::{
    de::{self, Decode, Decoder},
    en::{self, Encode, Encoder},
    Flat,
};
use pallas_primitives::{babbage::PlutusData, Fragment};

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
    pub fn from_cbor(bytes: &'b [u8], buffer: &'b mut Vec<u8>) -> Result<Self, de::Error> {
        let mut cbor_decoder = pallas_codec::minicbor::Decoder::new(bytes);

        let flat_bytes = cbor_decoder
            .bytes()
            .map_err(|err| de::Error::Message(err.to_string()))?;

        buffer.extend(flat_bytes);

        Self::unflat(buffer)
    }

    pub fn from_flat(bytes: &'b [u8]) -> Result<Self, de::Error> {
        Self::unflat(bytes)
    }

    pub fn from_hex(
        hex_str: &str,
        cbor_buffer: &'b mut Vec<u8>,
        flat_buffer: &'b mut Vec<u8>,
    ) -> Result<Self, de::Error> {
        let cbor_bytes = hex::decode(hex_str).map_err(|err| de::Error::Message(err.to_string()))?;

        cbor_buffer.extend(cbor_bytes);

        Self::from_cbor(cbor_buffer, flat_buffer)
    }

    pub fn to_cbor(&self) -> Result<Vec<u8>, en::Error> {
        let flat_bytes = self.flat()?;

        let mut bytes = Vec::new();

        let mut cbor_encoder = pallas_codec::minicbor::Encoder::new(&mut bytes);

        cbor_encoder
            .bytes(&flat_bytes)
            .map_err(|err| en::Error::Message(err.to_string()))?;

        Ok(bytes)
    }

    // convenient so that people don't need to depend on the flat crate
    // directly to call programs flat function
    pub fn to_flat(&self) -> Result<Vec<u8>, en::Error> {
        self.flat()
    }

    pub fn to_hex(&self) -> Result<String, en::Error> {
        let bytes = self.to_cbor()?;

        let hex = hex::encode(&bytes);

        Ok(hex)
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
        let mut state_log: Vec<String> = vec![];
        let version = (usize::decode(d)?, usize::decode(d)?, usize::decode(d)?);
        let term_option = Term::decode_debug(d, &mut state_log);

        match term_option {
            Ok(term) => Ok(Program { version, term }),
            Err(error) => Err(de::Error::ParseError(state_log.join(""), anyhow!(error))),
        }
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
            1 => Ok(Term::Delay(Rc::new(Term::decode(d)?))),
            2 => Ok(Term::Lambda {
                parameter_name: T::binder_decode(d)?,
                body: Rc::new(Term::decode(d)?),
            }),
            3 => Ok(Term::Apply {
                function: Rc::new(Term::decode(d)?),
                argument: Rc::new(Term::decode(d)?),
            }),
            // Need size limit for Constant
            4 => Ok(Term::Constant(Constant::decode(d)?)),
            5 => Ok(Term::Force(Rc::new(Term::decode(d)?))),
            6 => Ok(Term::Error),
            7 => Ok(Term::Builtin(DefaultFunction::decode(d)?)),
            x => {
                let buffer_slice: Vec<u8> = d
                    .buffer
                    .to_vec()
                    .iter()
                    .skip(if d.pos > 5 { d.pos - 5 } else { 0 })
                    .take(10)
                    .cloned()
                    .collect();

                Err(de::Error::UnknownTermConstructor(
                    x,
                    if d.pos > 5 { 5 } else { d.pos },
                    format!("{:02X?}", buffer_slice),
                    d.pos,
                    d.buffer.len(),
                ))
            }
        }
    }
}

impl<'b, T> Term<T>
where
    T: Binder<'b>,
{
    fn decode_debug(d: &mut Decoder, state_log: &mut Vec<String>) -> Result<Term<T>, de::Error> {
        match decode_term_tag(d)? {
            0 => {
                state_log.push("(var ".to_string());
                let var_option = T::decode(d);
                match var_option {
                    Ok(var) => {
                        state_log.push(format!("{})", var.text()));
                        Ok(Term::Var(var))
                    }
                    Err(error) => {
                        state_log.push("parse error)".to_string());
                        Err(error)
                    }
                }
            }

            1 => {
                state_log.push("(delay ".to_string());
                let term_option = Term::decode_debug(d, state_log);
                match term_option {
                    Ok(term) => {
                        state_log.push(")".to_string());
                        Ok(Term::Delay(Rc::new(term)))
                    }
                    Err(error) => {
                        state_log.push(")".to_string());
                        Err(error)
                    }
                }
            }
            2 => {
                state_log.push("(lam ".to_string());

                let var_option = T::binder_decode(d);
                match var_option {
                    Ok(var) => {
                        state_log.push(var.text().to_string());
                        let term_option = Term::decode_debug(d, state_log);
                        match term_option {
                            Ok(term) => {
                                state_log.push(")".to_string());
                                Ok(Term::Lambda {
                                    parameter_name: var,
                                    body: Rc::new(term),
                                })
                            }
                            Err(error) => {
                                state_log.push(")".to_string());
                                Err(error)
                            }
                        }
                    }
                    Err(error) => {
                        state_log.push(")".to_string());
                        Err(error)
                    }
                }
            }
            3 => {
                state_log.push("[ ".to_string());

                let function_term_option = Term::decode_debug(d, state_log);
                match function_term_option {
                    Ok(function) => {
                        state_log.push(" ".to_string());
                        let arg_term_option = Term::decode_debug(d, state_log);
                        match arg_term_option {
                            Ok(argument) => {
                                state_log.push("]".to_string());
                                Ok(Term::Apply {
                                    function: Rc::new(function),
                                    argument: Rc::new(argument),
                                })
                            }
                            Err(error) => {
                                state_log.push("]".to_string());
                                Err(error)
                            }
                        }
                    }
                    Err(error) => {
                        state_log.push(" not parsed]".to_string());
                        Err(error)
                    }
                }
            }
            // Need size limit for Constant
            4 => {
                state_log.push("(con ".to_string());

                let con_option = Constant::decode(d);
                match con_option {
                    Ok(constant) => {
                        state_log.push(format!("{})", constant.to_pretty()));
                        Ok(Term::Constant(constant))
                    }
                    Err(error) => {
                        state_log.push("parse error)".to_string());
                        Err(error)
                    }
                }
            }
            5 => {
                state_log.push("(force ".to_string());
                let term_option = Term::decode_debug(d, state_log);
                match term_option {
                    Ok(term) => {
                        state_log.push(")".to_string());
                        Ok(Term::Force(Rc::new(term)))
                    }
                    Err(error) => {
                        state_log.push(")".to_string());
                        Err(error)
                    }
                }
            }
            6 => {
                state_log.push("(error)".to_string());
                Ok(Term::Error)
            }
            7 => {
                state_log.push("(builtin ".to_string());

                let builtin_option = DefaultFunction::decode(d);
                match builtin_option {
                    Ok(builtin) => {
                        state_log.push(format!("{})", builtin));
                        Ok(Term::Builtin(builtin))
                    }
                    Err(error) => {
                        state_log.push("parse error)".to_string());
                        Err(error)
                    }
                }
            }
            x => {
                state_log.push("parse error".to_string());

                let buffer_slice: Vec<u8> = d
                    .buffer
                    .to_vec()
                    .iter()
                    .skip(if d.pos > 5 { d.pos - 5 } else { 0 })
                    .take(10)
                    .cloned()
                    .collect();

                Err(de::Error::UnknownTermConstructor(
                    x,
                    if d.pos > 5 { 5 } else { d.pos },
                    format!("{:02X?}", buffer_slice),
                    d.pos,
                    d.buffer.len(),
                ))
            }
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
        match &decode_constant(d)?[..] {
            [0] => Ok(Constant::Integer(i128::decode(d)?)),
            [1] => Ok(Constant::ByteString(Vec::<u8>::decode(d)?)),
            [2] => Ok(Constant::String(String::decode(d)?)),
            [3] => Ok(Constant::Unit),
            [4] => Ok(Constant::Bool(bool::decode(d)?)),
            [7, 5, rest @ ..] => {
                let mut rest = VecDeque::from(rest.to_vec());

                let typ = decode_type(&mut rest)?;

                let list: Vec<Constant> =
                    d.decode_list_with(|d| decode_constant_value(typ.clone(), d))?;

                Ok(Constant::ProtoList(typ, list))
            }
            [7, 7, 6, rest @ ..] => {
                let mut rest = VecDeque::from(rest.to_vec());

                let type1 = decode_type(&mut rest)?;
                let type2 = decode_type(&mut rest)?;

                let a = decode_constant_value(type1.clone(), d)?;
                let b = decode_constant_value(type2.clone(), d)?;

                Ok(Constant::ProtoPair(type1, type2, Box::new(a), Box::new(b)))
            }
            [8] => {
                let cbor = Vec::<u8>::decode(d)?;

                let data = PlutusData::decode_fragment(&cbor)
                    .map_err(|err| de::Error::Message(err.to_string()))?;

                Ok(Constant::Data(data))
            }
            x => Err(de::Error::Message(format!(
                "Unknown constant constructor tag: {:?}",
                x
            ))),
        }
    }
}

fn decode_constant_value(typ: Type, d: &mut Decoder) -> Result<Constant, de::Error> {
    match typ {
        Type::Integer => Ok(Constant::Integer(i128::decode(d)?)),
        Type::ByteString => Ok(Constant::ByteString(Vec::<u8>::decode(d)?)),
        Type::String => Ok(Constant::String(String::decode(d)?)),
        Type::Unit => Ok(Constant::Unit),
        Type::Bool => Ok(Constant::Bool(bool::decode(d)?)),
        Type::List(sub_type) => {
            let list: Vec<Constant> =
                d.decode_list_with(|d| decode_constant_value(*sub_type.clone(), d))?;

            Ok(Constant::ProtoList(*sub_type, list))
        }
        Type::Pair(type1, type2) => {
            let a = decode_constant_value(*type1.clone(), d)?;
            let b = decode_constant_value(*type2.clone(), d)?;

            Ok(Constant::ProtoPair(
                *type1,
                *type2,
                Box::new(a),
                Box::new(b),
            ))
        }
        Type::Data => {
            let cbor = Vec::<u8>::decode(d)?;

            let data = PlutusData::decode_fragment(&cbor)
                .map_err(|err| de::Error::Message(err.to_string()))?;

            Ok(Constant::Data(data))
        }
    }
}

fn decode_type(types: &mut VecDeque<u8>) -> Result<Type, de::Error> {
    match types.pop_front() {
        Some(4) => Ok(Type::Bool),
        Some(0) => Ok(Type::Integer),
        Some(2) => Ok(Type::String),
        Some(1) => Ok(Type::ByteString),
        Some(3) => Ok(Type::Unit),
        Some(8) => Ok(Type::Data),
        Some(7) => match types.pop_front() {
            Some(5) => Ok(Type::List(Box::new(decode_type(types)?))),
            Some(7) => match types.pop_front() {
                Some(6) => {
                    let type1 = decode_type(types)?;
                    let type2 = decode_type(types)?;

                    Ok(Type::Pair(Box::new(type1), Box::new(type2)))
                }
                Some(x) => Err(de::Error::Message(format!(
                    "Unknown constant type tag: {}",
                    x
                ))),
                None => Err(de::Error::Message("Unexpected empty buffer".to_string())),
            },
            Some(x) => Err(de::Error::Message(format!(
                "Unknown constant type tag: {}",
                x
            ))),
            None => Err(de::Error::Message("Unexpected empty buffer".to_string())),
        },

        Some(x) => Err(de::Error::Message(format!(
            "Unknown constant type tag: {}",
            x
        ))),
        None => Err(de::Error::Message("Unexpected empty buffer".to_string())),
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

pub fn decode_constant(d: &mut Decoder) -> Result<Vec<u8>, de::Error> {
    d.decode_list_with(decode_constant_tag)
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

        let expected_bytes = vec![
            0b00001011, 0b00010110, 0b00100001, 0b01001000, 0b00000101, 0b10000001,
        ];

        let actual_bytes = program.to_flat().unwrap();

        assert_eq!(actual_bytes, expected_bytes)
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

        let expected_bytes = vec![
            0b00000001, 0b00000000, 0b00000000, 0b01001011, 0b11010110, 0b11110101, 0b10000011,
            0b00001110, 0b01100001, 0b01000001,
        ];

        let actual_bytes = program.to_flat().unwrap();

        assert_eq!(actual_bytes, expected_bytes)
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

        let expected_bytes = vec![
            0b00000001, 0b00000000, 0b00000000, 0b01001011, 0b11011110, 0b11010111, 0b10111101,
            0b10100001, 0b01001000, 0b00000101, 0b10100010, 0b11000001,
        ];

        let actual_bytes = program.to_flat().unwrap();

        assert_eq!(actual_bytes, expected_bytes)
    }

    #[test]
    fn flat_decode_list_list_integer() {
        let bytes = vec![
            0b00000001, 0b00000000, 0b00000000, 0b01001011, 0b11010110, 0b11110101, 0b10000011,
            0b00001110, 0b01100001, 0b01000001,
        ];

        let expected_program = Program::<Name> {
            version: (1, 0, 0),
            term: Term::Constant(Constant::ProtoList(
                Type::List(Box::new(Type::Integer)),
                vec![
                    Constant::ProtoList(Type::Integer, vec![Constant::Integer(7)]),
                    Constant::ProtoList(Type::Integer, vec![Constant::Integer(5)]),
                ],
            )),
        };

        let actual_program: Program<Name> = Program::unflat(&bytes).unwrap();

        assert_eq!(actual_program, expected_program)
    }

    #[test]
    fn flat_decode_pair_pair_integer_bool_integer() {
        let bytes = vec![
            0b00000001, 0b00000000, 0b00000000, 0b01001011, 0b11011110, 0b11010111, 0b10111101,
            0b10100001, 0b01001000, 0b00000101, 0b10100010, 0b11000001,
        ];

        let expected_program = Program::<Name> {
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

        let actual_program: Program<Name> = Program::unflat(&bytes).unwrap();

        assert_eq!(actual_program, expected_program)
    }

    #[test]
    fn flat_decode_integer() {
        let bytes = vec![
            0b00001011, 0b00010110, 0b00100001, 0b01001000, 0b00000101, 0b10000001,
        ];

        let expected_program = Program {
            version: (11, 22, 33),
            term: Term::Constant(Constant::Integer(11)),
        };

        let actual_program: Program<Name> = Program::unflat(&bytes).unwrap();

        assert_eq!(actual_program, expected_program)
    }
}
