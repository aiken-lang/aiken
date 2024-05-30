use num_bigint::BigInt;
use pallas_codec::flat::{
    de::{self, Decode, Decoder},
    en::{self, Encode, Encoder},
    Flat,
};
use pallas_primitives::{babbage::PlutusData, Fragment};

use std::{collections::VecDeque, fmt::Debug, rc::Rc};

use crate::{
    ast::{
        Constant, DeBruijn, FakeNamedDeBruijn, Name, NamedDeBruijn, Program, Term, Type, Unique,
    },
    builtins::DefaultFunction,
    machine::runtime::Compressable,
};

const BUILTIN_TAG_WIDTH: u32 = 7;
const CONST_TAG_WIDTH: u32 = 4;
const TERM_TAG_WIDTH: u32 = 4;

pub trait Binder<'b>: Encode + Decode<'b> {
    fn binder_encode(&self, e: &mut Encoder) -> Result<(), en::Error>;
    fn binder_decode(d: &mut Decoder) -> Result<Self, de::Error>;
    fn text(&self) -> String;
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

    /// Convert a program to cbor bytes.
    ///
    /// _note: The cbor bytes of a program are merely
    /// the flat bytes of the program encoded as cbor bytes._
    ///
    /// # Examples
    ///
    /// ```
    /// use uplc::ast::{Program, Name, Term};
    ///
    /// let term = Term::var("x").lambda("x");
    /// let program = Program { version: (1, 0, 0), term };
    ///
    /// assert_eq!(
    ///     program.to_debruijn().unwrap().to_cbor().unwrap(),
    ///     vec![
    ///         0x46, 0x01, 0x00, 0x00,
    ///         0x20, 0x01, 0x01
    ///     ],
    /// );
    /// ```
    pub fn to_cbor(&self) -> Result<Vec<u8>, en::Error> {
        let flat_bytes = self.flat()?;

        let mut bytes = Vec::new();

        let mut cbor_encoder = pallas_codec::minicbor::Encoder::new(&mut bytes);

        cbor_encoder
            .bytes(&flat_bytes)
            .map_err(|err| en::Error::Message(err.to_string()))?;

        Ok(bytes)
    }

    /// Convert a program to a flat bytes.
    ///
    /// _**note**: Convenient so that people don't need to depend on the flat crate
    /// directly to call programs flat function._
    ///
    /// # Examples
    ///
    /// ```
    /// use uplc::ast::{Program, Name, Term};
    ///
    /// let term = Term::var("x").lambda("x");
    /// let program = Program { version: (1, 0, 0), term };
    ///
    /// assert_eq!(
    ///     program
    ///         .to_debruijn()
    ///         .unwrap()
    ///         .to_flat()
    ///         .unwrap(),
    ///     vec![
    ///         0x01, 0x00, 0x00,
    ///         0x20, 0x01, 0x01
    ///     ],
    /// );
    /// ```
    pub fn to_flat(&self) -> Result<Vec<u8>, en::Error> {
        self.flat()
    }

    /// Convert a program to hex encoded cbor bytes
    ///
    /// # Examples
    ///
    /// ```
    /// use uplc::ast::{Program, Name, Term};
    ///
    /// let term = Term::var("x").lambda("x");
    /// let program = Program { version: (1, 0, 0), term };
    ///
    /// assert_eq!(
    ///     program.to_debruijn().unwrap().to_hex().unwrap(),
    ///     "46010000200101".to_string(),
    /// );
    /// ```
    pub fn to_hex(&self) -> Result<String, en::Error> {
        let bytes = self.to_cbor()?;

        let hex = hex::encode(bytes);

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
            Err(error) => Err(de::Error::Message(format!(
                "{} {error}",
                state_log.join("")
            ))),
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
            Term::Constr { tag, fields } => {
                encode_term_tag(8, e)?;

                tag.encode(e)?;

                e.encode_list_with(fields, |term, e| (*term).encode(e))?;
            }
            Term::Case { constr, branches } => {
                encode_term_tag(9, e)?;

                constr.encode(e)?;

                e.encode_list_with(branches, |term, e| (*term).encode(e))?;
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
            0 => Ok(Term::Var(T::decode(d)?.into())),
            1 => Ok(Term::Delay(Rc::new(Term::decode(d)?))),
            2 => Ok(Term::Lambda {
                parameter_name: T::binder_decode(d)?.into(),
                body: Rc::new(Term::decode(d)?),
            }),
            3 => Ok(Term::Apply {
                function: Rc::new(Term::decode(d)?),
                argument: Rc::new(Term::decode(d)?),
            }),
            // Need size limit for Constant
            4 => Ok(Term::Constant(Constant::decode(d)?.into())),
            5 => Ok(Term::Force(Rc::new(Term::decode(d)?))),
            6 => Ok(Term::Error),
            7 => Ok(Term::Builtin(DefaultFunction::decode(d)?)),
            8 => {
                let tag = usize::decode(d)?;
                let fields = d.decode_list_with(Term::<T>::decode)?;

                Ok(Term::Constr { tag, fields })
            }
            9 => {
                let constr = (Term::<T>::decode(d)?).into();

                let branches = d.decode_list_with(Term::<T>::decode)?;

                Ok(Term::Case { constr, branches })
            }
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
                    format!("{buffer_slice:02X?}"),
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
                        Ok(Term::Var(var.into()))
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
                        state_log.push(var.text());
                        let term_option = Term::decode_debug(d, state_log);
                        match term_option {
                            Ok(term) => {
                                state_log.push(")".to_string());
                                Ok(Term::Lambda {
                                    parameter_name: var.into(),
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
                        Ok(Term::Constant(constant.into()))
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
                        state_log.push(format!("{builtin})"));
                        Ok(Term::Builtin(builtin))
                    }
                    Err(error) => {
                        state_log.push("parse error)".to_string());
                        Err(error)
                    }
                }
            }
            8 => {
                state_log.push("(constr ".to_string());

                let tag = usize::decode(d)?;

                let fields = d.decode_list_with_debug(
                    |d, state_log| Term::<T>::decode_debug(d, state_log),
                    state_log,
                )?;

                Ok(Term::Constr { tag, fields })
            }
            9 => {
                state_log.push("(case ".to_string());
                let constr = Term::<T>::decode_debug(d, state_log)?.into();

                let branches = d.decode_list_with_debug(
                    |d, state_log| Term::<T>::decode_debug(d, state_log),
                    state_log,
                )?;

                Ok(Term::Case { constr, branches })
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
                    format!("{buffer_slice:02X?}"),
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
            Constant::Bls12_381G1Element(b) => {
                encode_constant(&[9], e)?;

                let x = b.compress();

                x.encode(e)?;
            }
            Constant::Bls12_381G2Element(b) => {
                encode_constant(&[10], e)?;

                let x = b.compress();

                x.encode(e)?;
            }
            Constant::Bls12_381MlResult(_) => {
                encode_constant(&[11], e)?;

                return Err(en::Error::Message(
                    "BLS12-381 ML results are not supported for flat encoding".to_string(),
                ));
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
        Constant::Bls12_381G1Element(b) => {
            let x = b.compress();

            x.encode(e)
        }
        Constant::Bls12_381G2Element(b) => {
            let x = b.compress();

            x.encode(e)
        }
        Constant::Bls12_381MlResult(_) => Err(en::Error::Message(
            "BLS12-381 ML results are not supported for flat encoding".to_string(),
        )),
    }
}

fn encode_type(typ: &Type, bytes: &mut Vec<u8>) {
    match typ {
        Type::Integer => bytes.push(0),
        Type::ByteString => bytes.push(1),
        Type::String => bytes.push(2),
        Type::Unit => bytes.push(3),
        Type::Bool => bytes.push(4),
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
        Type::Bls12_381G1Element => bytes.push(9),
        Type::Bls12_381G2Element => bytes.push(10),
        Type::Bls12_381MlResult => bytes.push(11),
    }
}

impl<'b> Decode<'b> for Constant {
    fn decode(d: &mut Decoder) -> Result<Self, de::Error> {
        match &decode_constant(d)?[..] {
            [0] => Ok(Constant::Integer(BigInt::decode(d)?)),
            [1] => Ok(Constant::ByteString(Vec::<u8>::decode(d)?)),
            [2] => Ok(Constant::String(String::decode(d)?)),
            [3] => Ok(Constant::Unit),
            [4] => Ok(Constant::Bool(bool::decode(d)?)),
            [7, 5, rest @ ..] => {
                let mut rest = VecDeque::from(rest.to_vec());

                let typ = decode_type(&mut rest)?;

                let list: Vec<Constant> =
                    d.decode_list_with(|d| decode_constant_value(typ.clone().into(), d))?;

                Ok(Constant::ProtoList(typ, list))
            }
            [7, 7, 6, rest @ ..] => {
                let mut rest = VecDeque::from(rest.to_vec());

                let type1 = decode_type(&mut rest)?;
                let type2 = decode_type(&mut rest)?;

                let a = decode_constant_value(type1.clone().into(), d)?;
                let b = decode_constant_value(type2.clone().into(), d)?;

                Ok(Constant::ProtoPair(type1, type2, a.into(), b.into()))
            }
            [8] => {
                let cbor = Vec::<u8>::decode(d)?;

                let data = PlutusData::decode_fragment(&cbor)
                    .map_err(|err| de::Error::Message(err.to_string()))?;

                Ok(Constant::Data(data))
            }
            [9] => {
                let p1 = Vec::<u8>::decode(d)?;

                let p1 = blst::blst_p1::uncompress(&p1).map_err(|err| {
                    de::Error::Message(format!("Failed to uncompress p1: {}", err))
                })?;

                Ok(Constant::Bls12_381G1Element(p1.into()))
            }

            [10] => {
                let p2 = Vec::<u8>::decode(d)?;

                let p2 = blst::blst_p2::uncompress(&p2).map_err(|err| {
                    de::Error::Message(format!("Failed to uncompress p2: {}", err))
                })?;

                Ok(Constant::Bls12_381G2Element(p2.into()))
            }
            [11] => Err(de::Error::Message(
                "BLS12-381 ML results are not supported for flat decoding".to_string(),
            )),
            x => Err(de::Error::Message(format!(
                "Unknown constant constructor tag: {x:?}"
            ))),
        }
    }
}

fn decode_constant_value(typ: Rc<Type>, d: &mut Decoder) -> Result<Constant, de::Error> {
    match typ.as_ref() {
        Type::Integer => Ok(Constant::Integer(BigInt::decode(d)?)),
        Type::ByteString => Ok(Constant::ByteString(Vec::<u8>::decode(d)?)),
        Type::String => Ok(Constant::String(String::decode(d)?)),
        Type::Unit => Ok(Constant::Unit),
        Type::Bool => Ok(Constant::Bool(bool::decode(d)?)),
        Type::List(sub_type) => {
            let list: Vec<Constant> =
                d.decode_list_with(|d| decode_constant_value(sub_type.clone(), d))?;

            Ok(Constant::ProtoList(sub_type.as_ref().clone(), list))
        }
        Type::Pair(type1, type2) => {
            let a = decode_constant_value(type1.clone(), d)?;
            let b = decode_constant_value(type2.clone(), d)?;

            Ok(Constant::ProtoPair(
                type1.as_ref().clone(),
                type2.as_ref().clone(),
                a.into(),
                b.into(),
            ))
        }
        Type::Data => {
            let cbor = Vec::<u8>::decode(d)?;

            let data = PlutusData::decode_fragment(&cbor)
                .map_err(|err| de::Error::Message(err.to_string()))?;

            Ok(Constant::Data(data))
        }
        Type::Bls12_381G1Element => {
            let p1 = Vec::<u8>::decode(d)?;

            let p1 = blst::blst_p1::uncompress(&p1)
                .map_err(|err| de::Error::Message(format!("Failed to uncompress p1: {}", err)))?;

            Ok(Constant::Bls12_381G1Element(p1.into()))
        }
        Type::Bls12_381G2Element => {
            let p2 = Vec::<u8>::decode(d)?;

            let p2 = blst::blst_p2::uncompress(&p2)
                .map_err(|err| de::Error::Message(format!("Failed to uncompress p2: {}", err)))?;

            Ok(Constant::Bls12_381G2Element(p2.into()))
        }
        Type::Bls12_381MlResult => Err(de::Error::Message(
            "BLS12-381 ML results are not supported for flat decoding".to_string(),
        )),
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
        Some(9) => Ok(Type::Bls12_381G1Element),
        Some(10) => Ok(Type::Bls12_381G2Element),
        Some(11) => Ok(Type::Bls12_381MlResult),
        Some(7) => match types.pop_front() {
            Some(5) => Ok(Type::List(decode_type(types)?.into())),
            Some(7) => match types.pop_front() {
                Some(6) => {
                    let type1 = decode_type(types)?;
                    let type2 = decode_type(types)?;

                    Ok(Type::Pair(type1.into(), type2.into()))
                }
                Some(x) => Err(de::Error::Message(format!(
                    "Unknown constant type tag: {x}"
                ))),
                None => Err(de::Error::Message("Unexpected empty buffer".to_string())),
            },
            Some(x) => Err(de::Error::Message(format!(
                "Unknown constant type tag: {x}"
            ))),
            None => Err(de::Error::Message("Unexpected empty buffer".to_string())),
        },

        Some(x) => Err(de::Error::Message(format!(
            "Unknown constant type tag: {x}"
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

    fn text(&self) -> String {
        self.text.clone()
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
        self.index.encode(e)?;

        Ok(())
    }

    fn binder_decode(d: &mut Decoder) -> Result<Self, de::Error> {
        Ok(NamedDeBruijn {
            text: String::decode(d)?,
            index: DeBruijn::decode(d)?,
        })
    }

    fn text(&self) -> String {
        format!("{}_{}", &self.text, self.index)
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

    fn text(&self) -> String {
        format!("i_{}", self)
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

    fn text(&self) -> String {
        format!("{}_{}", self.0.text, self.0.index)
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
    if 2_u8.pow(num_bits) <= byte {
        Err(en::Error::Message(format!(
            "Overflow detected, cannot fit {byte} in {num_bits} bits."
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
mod tests {
    use super::{Constant, Program, Term};
    use crate::{
        ast::{DeBruijn, Name, Type},
        parser,
    };
    use indoc::indoc;
    use pallas_codec::flat::Flat;

    #[test]
    fn flat_encode_integer() {
        let program = Program::<Name> {
            version: (11, 22, 33),
            term: Term::Constant(Constant::Integer(11.into()).into()),
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
            term: Term::Constant(
                Constant::ProtoList(
                    Type::List(Type::Integer.into()),
                    vec![
                        Constant::ProtoList(Type::Integer, vec![Constant::Integer(7.into())]),
                        Constant::ProtoList(Type::Integer, vec![Constant::Integer(5.into())]),
                    ],
                )
                .into(),
            ),
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
            term: Term::Constant(
                Constant::ProtoPair(
                    Type::Pair(Type::Integer.into(), Type::Bool.into()),
                    Type::Integer,
                    Constant::ProtoPair(
                        Type::Integer,
                        Type::Bool,
                        Constant::Integer(11.into()).into(),
                        Constant::Bool(true).into(),
                    )
                    .into(),
                    Constant::Integer(11.into()).into(),
                )
                .into(),
            ),
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
            term: Term::Constant(
                Constant::ProtoList(
                    Type::List(Type::Integer.into()),
                    vec![
                        Constant::ProtoList(Type::Integer, vec![Constant::Integer(7.into())]),
                        Constant::ProtoList(Type::Integer, vec![Constant::Integer(5.into())]),
                    ],
                )
                .into(),
            ),
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
            term: Term::Constant(
                Constant::ProtoPair(
                    Type::Pair(Type::Integer.into(), Type::Bool.into()),
                    Type::Integer,
                    Constant::ProtoPair(
                        Type::Integer,
                        Type::Bool,
                        Constant::Integer(11.into()).into(),
                        Constant::Bool(true).into(),
                    )
                    .into(),
                    Constant::Integer(11.into()).into(),
                )
                .into(),
            ),
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
            term: Term::Constant(Constant::Integer(11.into()).into()),
        };

        let actual_program: Program<Name> = Program::unflat(&bytes).unwrap();

        assert_eq!(actual_program, expected_program)
    }

    #[test]
    fn unflat_string_escape() {
        let cbor = "490000004901015c0001";

        let program =
            Program::<DeBruijn>::from_hex(cbor, &mut Vec::new(), &mut Vec::new()).unwrap();

        assert_eq!(
            program.to_pretty().as_str(),
            indoc! { r#"
              (program
                0.0.0
                (con string "\\")
              )"#}
        );
    }

    #[test]
    fn uplc_parser_string_escape() {
        let source = indoc! { r#"
            (program
              0.0.0
              (con string "\n\t\\\"\'\r")
            )"#};

        let program = parser::program(source).unwrap();

        assert_eq!(program.to_pretty(), source);
    }
}
