use std::{mem::size_of, ops::Deref, rc::Rc};

use num_bigint::BigInt;
use num_integer::Integer;
use num_traits::{Signed, Zero};
use once_cell::sync::Lazy;
use pallas_primitives::conway::{Language, PlutusData};

use crate::{
    ast::{Constant, Data, Type},
    builtins::DefaultFunction,
    machine::value::integer_log2,
    plutus_data_to_bytes,
};

use super::{
    cost_model::{BuiltinCosts, ExBudget},
    value::{from_pallas_bigint, to_pallas_bigint},
    Error, Value,
};

static SCALAR_PERIOD: Lazy<BigInt> = Lazy::new(|| {
    BigInt::from_bytes_be(
        num_bigint::Sign::Plus,
        &[
            0x73, 0xed, 0xa7, 0x53, 0x29, 0x9d, 0x7d, 0x48, 0x33, 0x39, 0xd8, 0x08, 0x09, 0xa1,
            0xd8, 0x05, 0x53, 0xbd, 0xa4, 0x02, 0xff, 0xfe, 0x5b, 0xfe, 0xff, 0xff, 0xff, 0xff,
            0x00, 0x00, 0x00, 0x01,
        ],
    )
});

const BLST_P1_COMPRESSED_SIZE: usize = 48;

const BLST_P2_COMPRESSED_SIZE: usize = 96;

const INTEGER_TO_BYTE_STRING_MAXIMUM_OUTPUT_LENGTH: i64 = 8192;

//#[derive(std::cmp::PartialEq)]
//pub enum EvalMode {
//    Immediate,
//    Deferred,
//}

pub enum BuiltinSemantics {
    V1,
    V2,
}

impl From<&Language> for BuiltinSemantics {
    fn from(language: &Language) -> Self {
        match language {
            Language::PlutusV1 => BuiltinSemantics::V1,
            Language::PlutusV2 => BuiltinSemantics::V1,
            Language::PlutusV3 => BuiltinSemantics::V2,
        }
    }
}

#[derive(Clone, Debug)]
pub struct BuiltinRuntime {
    pub(super) args: Vec<Value>,
    fun: DefaultFunction,
    pub(super) forces: u32,
}

impl BuiltinRuntime {
    pub fn new(fun: DefaultFunction) -> BuiltinRuntime {
        Self {
            args: vec![],
            fun,
            forces: 0,
        }
    }

    pub fn is_arrow(&self) -> bool {
        self.args.len() != self.fun.arity()
    }

    pub fn is_ready(&self) -> bool {
        self.args.len() == self.fun.arity()
    }

    pub fn needs_force(&self) -> bool {
        self.forces < self.fun.force_count()
    }

    pub fn consume_force(&mut self) {
        self.forces += 1;
    }

    pub fn call(&self, language: &Language, logs: &mut Vec<String>) -> Result<Value, Error> {
        self.fun.call(language.into(), &self.args, logs)
    }

    pub fn push(&mut self, arg: Value) -> Result<(), Error> {
        self.args.push(arg);

        Ok(())
    }

    pub fn to_ex_budget(&self, costs: &BuiltinCosts) -> ExBudget {
        costs.to_ex_budget(self.fun, &self.args)
    }
}

impl From<DefaultFunction> for BuiltinRuntime {
    fn from(fun: DefaultFunction) -> Self {
        BuiltinRuntime::new(fun)
    }
}

impl DefaultFunction {
    pub fn arity(&self) -> usize {
        match self {
            DefaultFunction::AddInteger => 2,
            DefaultFunction::SubtractInteger => 2,
            DefaultFunction::MultiplyInteger => 2,
            DefaultFunction::DivideInteger => 2,
            DefaultFunction::QuotientInteger => 2,
            DefaultFunction::RemainderInteger => 2,
            DefaultFunction::ModInteger => 2,
            DefaultFunction::EqualsInteger => 2,
            DefaultFunction::LessThanInteger => 2,
            DefaultFunction::LessThanEqualsInteger => 2,
            DefaultFunction::AppendByteString => 2,
            DefaultFunction::ConsByteString => 2,
            DefaultFunction::SliceByteString => 3,
            DefaultFunction::LengthOfByteString => 1,
            DefaultFunction::IndexByteString => 2,
            DefaultFunction::EqualsByteString => 2,
            DefaultFunction::LessThanByteString => 2,
            DefaultFunction::LessThanEqualsByteString => 2,
            DefaultFunction::Sha2_256 => 1,
            DefaultFunction::Sha3_256 => 1,
            DefaultFunction::Blake2b_224 => 1,
            DefaultFunction::Blake2b_256 => 1,
            DefaultFunction::Keccak_256 => 1,
            DefaultFunction::VerifyEd25519Signature => 3,
            DefaultFunction::VerifyEcdsaSecp256k1Signature => 3,
            DefaultFunction::VerifySchnorrSecp256k1Signature => 3,
            DefaultFunction::AppendString => 2,
            DefaultFunction::EqualsString => 2,
            DefaultFunction::EncodeUtf8 => 1,
            DefaultFunction::DecodeUtf8 => 1,
            DefaultFunction::IfThenElse => 3,
            DefaultFunction::ChooseUnit => 2,
            DefaultFunction::Trace => 2,
            DefaultFunction::FstPair => 1,
            DefaultFunction::SndPair => 1,
            DefaultFunction::ChooseList => 3,
            DefaultFunction::MkCons => 2,
            DefaultFunction::HeadList => 1,
            DefaultFunction::TailList => 1,
            DefaultFunction::NullList => 1,
            DefaultFunction::ChooseData => 6,
            DefaultFunction::ConstrData => 2,
            DefaultFunction::MapData => 1,
            DefaultFunction::ListData => 1,
            DefaultFunction::IData => 1,
            DefaultFunction::BData => 1,
            DefaultFunction::UnConstrData => 1,
            DefaultFunction::UnMapData => 1,
            DefaultFunction::UnListData => 1,
            DefaultFunction::UnIData => 1,
            DefaultFunction::UnBData => 1,
            DefaultFunction::EqualsData => 2,
            DefaultFunction::SerialiseData => 1,
            DefaultFunction::MkPairData => 2,
            DefaultFunction::MkNilData => 1,
            DefaultFunction::MkNilPairData => 1,
            DefaultFunction::Bls12_381_G1_Add => 2,
            DefaultFunction::Bls12_381_G1_Neg => 1,
            DefaultFunction::Bls12_381_G1_ScalarMul => 2,
            DefaultFunction::Bls12_381_G1_Equal => 2,
            DefaultFunction::Bls12_381_G1_Compress => 1,
            DefaultFunction::Bls12_381_G1_Uncompress => 1,
            DefaultFunction::Bls12_381_G1_HashToGroup => 2,
            DefaultFunction::Bls12_381_G2_Add => 2,
            DefaultFunction::Bls12_381_G2_Neg => 1,
            DefaultFunction::Bls12_381_G2_ScalarMul => 2,
            DefaultFunction::Bls12_381_G2_Equal => 2,
            DefaultFunction::Bls12_381_G2_Compress => 1,
            DefaultFunction::Bls12_381_G2_Uncompress => 1,
            DefaultFunction::Bls12_381_G2_HashToGroup => 2,
            DefaultFunction::Bls12_381_MillerLoop => 2,
            DefaultFunction::Bls12_381_MulMlResult => 2,
            DefaultFunction::Bls12_381_FinalVerify => 2,
            DefaultFunction::IntegerToByteString => 3,
            DefaultFunction::ByteStringToInteger => 2,
        }
    }

    pub fn force_count(&self) -> u32 {
        match self {
            DefaultFunction::AddInteger => 0,
            DefaultFunction::SubtractInteger => 0,
            DefaultFunction::MultiplyInteger => 0,
            DefaultFunction::DivideInteger => 0,
            DefaultFunction::QuotientInteger => 0,
            DefaultFunction::RemainderInteger => 0,
            DefaultFunction::ModInteger => 0,
            DefaultFunction::EqualsInteger => 0,
            DefaultFunction::LessThanInteger => 0,
            DefaultFunction::LessThanEqualsInteger => 0,
            DefaultFunction::AppendByteString => 0,
            DefaultFunction::ConsByteString => 0,
            DefaultFunction::SliceByteString => 0,
            DefaultFunction::LengthOfByteString => 0,
            DefaultFunction::IndexByteString => 0,
            DefaultFunction::EqualsByteString => 0,
            DefaultFunction::LessThanByteString => 0,
            DefaultFunction::LessThanEqualsByteString => 0,
            DefaultFunction::Sha2_256 => 0,
            DefaultFunction::Sha3_256 => 0,
            DefaultFunction::Blake2b_224 => 0,
            DefaultFunction::Blake2b_256 => 0,
            DefaultFunction::Keccak_256 => 0,
            DefaultFunction::VerifyEd25519Signature => 0,
            DefaultFunction::VerifyEcdsaSecp256k1Signature => 0,
            DefaultFunction::VerifySchnorrSecp256k1Signature => 0,
            DefaultFunction::AppendString => 0,
            DefaultFunction::EqualsString => 0,
            DefaultFunction::EncodeUtf8 => 0,
            DefaultFunction::DecodeUtf8 => 0,
            DefaultFunction::IfThenElse => 1,
            DefaultFunction::ChooseUnit => 1,
            DefaultFunction::Trace => 1,
            DefaultFunction::FstPair => 2,
            DefaultFunction::SndPair => 2,
            DefaultFunction::ChooseList => 2,
            DefaultFunction::MkCons => 1,
            DefaultFunction::HeadList => 1,
            DefaultFunction::TailList => 1,
            DefaultFunction::NullList => 1,
            DefaultFunction::ChooseData => 1,
            DefaultFunction::ConstrData => 0,
            DefaultFunction::MapData => 0,
            DefaultFunction::ListData => 0,
            DefaultFunction::IData => 0,
            DefaultFunction::BData => 0,
            DefaultFunction::UnConstrData => 0,
            DefaultFunction::UnMapData => 0,
            DefaultFunction::UnListData => 0,
            DefaultFunction::UnIData => 0,
            DefaultFunction::UnBData => 0,
            DefaultFunction::EqualsData => 0,
            DefaultFunction::SerialiseData => 0,
            DefaultFunction::MkPairData => 0,
            DefaultFunction::MkNilData => 0,
            DefaultFunction::MkNilPairData => 0,
            DefaultFunction::Bls12_381_G1_Add => 0,
            DefaultFunction::Bls12_381_G1_Neg => 0,
            DefaultFunction::Bls12_381_G1_ScalarMul => 0,
            DefaultFunction::Bls12_381_G1_Equal => 0,
            DefaultFunction::Bls12_381_G1_Compress => 0,
            DefaultFunction::Bls12_381_G1_Uncompress => 0,
            DefaultFunction::Bls12_381_G1_HashToGroup => 0,
            DefaultFunction::Bls12_381_G2_Add => 0,
            DefaultFunction::Bls12_381_G2_Neg => 0,
            DefaultFunction::Bls12_381_G2_ScalarMul => 0,
            DefaultFunction::Bls12_381_G2_Equal => 0,
            DefaultFunction::Bls12_381_G2_Compress => 0,
            DefaultFunction::Bls12_381_G2_Uncompress => 0,
            DefaultFunction::Bls12_381_G2_HashToGroup => 0,
            DefaultFunction::Bls12_381_MillerLoop => 0,
            DefaultFunction::Bls12_381_MulMlResult => 0,
            DefaultFunction::Bls12_381_FinalVerify => 0,
            DefaultFunction::IntegerToByteString => 0,
            DefaultFunction::ByteStringToInteger => 0,
        }
    }

    pub fn call(
        &self,
        semantics: BuiltinSemantics,
        args: &[Value],
        logs: &mut Vec<String>,
    ) -> Result<Value, Error> {
        match self {
            DefaultFunction::AddInteger => {
                let arg1 = args[0].unwrap_integer()?;
                let arg2 = args[1].unwrap_integer()?;

                let result = arg1 + arg2;

                let value = Value::integer(result);

                Ok(value)
            }
            DefaultFunction::SubtractInteger => {
                let arg1 = args[0].unwrap_integer()?;
                let arg2 = args[1].unwrap_integer()?;

                let result = arg1 - arg2;

                let value = Value::integer(result);

                Ok(value)
            }
            DefaultFunction::MultiplyInteger => {
                let arg1 = args[0].unwrap_integer()?;
                let arg2 = args[1].unwrap_integer()?;

                let result = arg1 * arg2;

                let value = Value::integer(result);

                Ok(value)
            }
            DefaultFunction::DivideInteger => {
                let arg1 = args[0].unwrap_integer()?;
                let arg2 = args[1].unwrap_integer()?;

                if *arg2 != 0.into() {
                    let (result, _) = arg1.div_mod_floor(arg2);

                    let value = Value::integer(result);

                    Ok(value)
                } else {
                    Err(Error::DivideByZero(arg1.clone(), arg2.clone()))
                }
            }
            DefaultFunction::QuotientInteger => {
                let arg1 = args[0].unwrap_integer()?;
                let arg2 = args[1].unwrap_integer()?;

                if *arg2 != 0.into() {
                    let (result, _) = arg1.div_rem(arg2);

                    let value = Value::integer(result);

                    Ok(value)
                } else {
                    Err(Error::DivideByZero(arg1.clone(), arg2.clone()))
                }
            }
            DefaultFunction::RemainderInteger => {
                let arg1 = args[0].unwrap_integer()?;
                let arg2 = args[1].unwrap_integer()?;

                if *arg2 != 0.into() {
                    let (_, result) = arg1.div_rem(arg2);

                    let value = Value::integer(result);

                    Ok(value)
                } else {
                    Err(Error::DivideByZero(arg1.clone(), arg2.clone()))
                }
            }
            DefaultFunction::ModInteger => {
                let arg1 = args[0].unwrap_integer()?;
                let arg2 = args[1].unwrap_integer()?;

                if *arg2 != 0.into() {
                    let (_, result) = arg1.div_mod_floor(arg2);

                    let value = Value::integer(result);

                    Ok(value)
                } else {
                    Err(Error::DivideByZero(arg1.clone(), arg2.clone()))
                }
            }
            DefaultFunction::EqualsInteger => {
                let arg1 = args[0].unwrap_integer()?;
                let arg2 = args[1].unwrap_integer()?;

                let value = Value::bool(arg1 == arg2);

                Ok(value)
            }
            DefaultFunction::LessThanInteger => {
                let arg1 = args[0].unwrap_integer()?;
                let arg2 = args[1].unwrap_integer()?;

                let value = Value::bool(arg1 < arg2);

                Ok(value)
            }
            DefaultFunction::LessThanEqualsInteger => {
                let arg1 = args[0].unwrap_integer()?;
                let arg2 = args[1].unwrap_integer()?;

                let value = Value::bool(arg1 <= arg2);

                Ok(value)
            }
            DefaultFunction::AppendByteString => {
                let arg1 = args[0].unwrap_byte_string()?;
                let arg2 = args[1].unwrap_byte_string()?;

                let result = arg1.iter().copied().chain(arg2.iter().copied()).collect();

                let value = Value::byte_string(result);

                Ok(value)
            }
            DefaultFunction::ConsByteString => {
                let arg1 = args[0].unwrap_integer()?;
                let arg2 = args[1].unwrap_byte_string()?;

                let byte: u8 = match semantics {
                    BuiltinSemantics::V1 => {
                        let wrap = arg1.mod_floor(&256.into());

                        wrap.try_into().unwrap()
                    }
                    BuiltinSemantics::V2 => {
                        if *arg1 > 255.into() {
                            return Err(Error::ByteStringConsBiggerThanOneByte(arg1.clone()));
                        }

                        arg1.try_into().unwrap()
                    }
                };

                let mut ret = vec![byte];

                ret.extend(arg2.clone());

                let value = Value::byte_string(ret);

                Ok(value)
            }
            DefaultFunction::SliceByteString => {
                let arg1 = args[0].unwrap_integer()?;
                let arg2 = args[1].unwrap_integer()?;
                let arg3 = args[2].unwrap_byte_string()?;

                let skip: usize = if arg1.lt(&0.into()) {
                    0
                } else {
                    arg1.try_into().unwrap()
                };
                let take: usize = if arg2.lt(&0.into()) {
                    0
                } else {
                    arg2.try_into().unwrap()
                };

                let ret: Vec<u8> = arg3.iter().skip(skip).take(take).cloned().collect();

                let value = Value::byte_string(ret);

                Ok(value)
            }
            DefaultFunction::LengthOfByteString => {
                let arg1 = args[0].unwrap_byte_string()?;

                let value = Value::integer(arg1.len().into());

                Ok(value)
            }
            DefaultFunction::IndexByteString => {
                let arg1 = args[0].unwrap_byte_string()?;
                let arg2 = args[1].unwrap_integer()?;

                let index: i128 = arg2.try_into().unwrap();

                if 0 <= index && index < arg1.len() as i128 {
                    let ret = arg1[index as usize];

                    let value = Value::integer(ret.into());

                    Ok(value)
                } else {
                    Err(Error::ByteStringOutOfBounds(arg2.clone(), arg1.to_vec()))
                }
            }
            DefaultFunction::EqualsByteString => {
                let arg1 = args[0].unwrap_byte_string()?;
                let arg2 = args[1].unwrap_byte_string()?;

                let value = Value::bool(arg1 == arg2);

                Ok(value)
            }
            DefaultFunction::LessThanByteString => {
                let arg1 = args[0].unwrap_byte_string()?;
                let arg2 = args[1].unwrap_byte_string()?;

                let value = Value::bool(arg1 < arg2);

                Ok(value)
            }
            DefaultFunction::LessThanEqualsByteString => {
                let arg1 = args[0].unwrap_byte_string()?;
                let arg2 = args[1].unwrap_byte_string()?;

                let value = Value::bool(arg1 <= arg2);

                Ok(value)
            }
            DefaultFunction::Sha2_256 => {
                use cryptoxide::{digest::Digest, sha2::Sha256};

                let arg1 = args[0].unwrap_byte_string()?;

                let mut hasher = Sha256::new();

                hasher.input(arg1);

                let mut bytes = vec![0; hasher.output_bytes()];

                hasher.result(&mut bytes);

                let value = Value::byte_string(bytes);

                Ok(value)
            }
            DefaultFunction::Sha3_256 => {
                use cryptoxide::{digest::Digest, sha3::Sha3_256};

                let arg1 = args[0].unwrap_byte_string()?;

                let mut hasher = Sha3_256::new();

                hasher.input(arg1);

                let mut bytes = vec![0; hasher.output_bytes()];

                hasher.result(&mut bytes);

                let value = Value::byte_string(bytes);

                Ok(value)
            }

            DefaultFunction::Blake2b_224 => {
                use cryptoxide::{blake2b::Blake2b, digest::Digest};

                let arg1 = args[0].unwrap_byte_string()?;

                let mut digest = [0u8; 28];
                let mut context = Blake2b::new(28);

                context.input(arg1);
                context.result(&mut digest);

                let value = Value::byte_string(digest.to_vec());

                Ok(value)
            }
            DefaultFunction::Blake2b_256 => {
                use cryptoxide::{blake2b::Blake2b, digest::Digest};

                let arg1 = args[0].unwrap_byte_string()?;

                let mut digest = [0u8; 32];
                let mut context = Blake2b::new(32);

                context.input(arg1);
                context.result(&mut digest);

                let value = Value::byte_string(digest.to_vec());

                Ok(value)
            }
            DefaultFunction::Keccak_256 => {
                use cryptoxide::{digest::Digest, sha3::Keccak256};

                let arg1 = args[0].unwrap_byte_string()?;

                let mut hasher = Keccak256::new();

                hasher.input(arg1);

                let mut bytes = vec![0; hasher.output_bytes()];

                hasher.result(&mut bytes);

                let value = Value::byte_string(bytes);

                Ok(value)
            }
            DefaultFunction::VerifyEd25519Signature => {
                use cryptoxide::ed25519;

                let public_key = args[0].unwrap_byte_string()?;
                let message = args[1].unwrap_byte_string()?;
                let signature = args[2].unwrap_byte_string()?;

                let public_key: [u8; 32] = public_key
                    .clone()
                    .try_into()
                    .map_err(|e: Vec<u8>| Error::UnexpectedEd25519PublicKeyLength(e.len()))?;

                let signature: [u8; 64] = signature
                    .clone()
                    .try_into()
                    .map_err(|e: Vec<u8>| Error::UnexpectedEd25519SignatureLength(e.len()))?;

                let valid = ed25519::verify(message, &public_key, &signature);

                let value = Value::bool(valid);

                Ok(value)
            }
            DefaultFunction::VerifyEcdsaSecp256k1Signature => {
                let public_key = args[0].unwrap_byte_string()?;
                let message = args[1].unwrap_byte_string()?;
                let signature = args[2].unwrap_byte_string()?;

                verify_ecdsa(public_key, message, signature)
            }
            DefaultFunction::VerifySchnorrSecp256k1Signature => {
                let public_key = args[0].unwrap_byte_string()?;
                let message = args[1].unwrap_byte_string()?;
                let signature = args[2].unwrap_byte_string()?;

                verify_schnorr(public_key, message, signature)
            }
            DefaultFunction::AppendString => {
                let arg1 = args[0].unwrap_string()?;
                let arg2 = args[1].unwrap_string()?;

                let value = Value::string(format!("{arg1}{arg2}"));

                Ok(value)
            }
            DefaultFunction::EqualsString => {
                let arg1 = args[0].unwrap_string()?;
                let arg2 = args[1].unwrap_string()?;

                let value = Value::bool(arg1 == arg2);

                Ok(value)
            }
            DefaultFunction::EncodeUtf8 => {
                let arg1 = args[0].unwrap_string()?;

                let bytes = arg1.as_bytes().to_vec();

                let value = Value::byte_string(bytes);

                Ok(value)
            }
            DefaultFunction::DecodeUtf8 => {
                let arg1 = args[0].unwrap_byte_string()?;

                let string = String::from_utf8(arg1.clone())?;

                let value = Value::string(string);

                Ok(value)
            }
            DefaultFunction::IfThenElse => {
                let condition = args[0].unwrap_bool()?;

                if *condition {
                    Ok(args[1].clone())
                } else {
                    Ok(args[2].clone())
                }
            }
            DefaultFunction::ChooseUnit => {
                args[0].unwrap_unit()?;

                Ok(args[1].clone())
            }
            DefaultFunction::Trace => {
                let arg1 = args[0].unwrap_string()?;

                logs.push(arg1.clone());

                Ok(args[1].clone())
            }
            DefaultFunction::FstPair => {
                let (_, _, first, _) = args[0].unwrap_pair()?;

                let value = Value::Con(first.clone());

                Ok(value)
            }
            DefaultFunction::SndPair => {
                let (_, _, _, second) = args[0].unwrap_pair()?;

                let value = Value::Con(second.clone());

                Ok(value)
            }
            DefaultFunction::ChooseList => {
                let (_, list) = args[0].unwrap_list()?;

                if list.is_empty() {
                    Ok(args[1].clone())
                } else {
                    Ok(args[2].clone())
                }
            }
            DefaultFunction::MkCons => {
                let item = args[0].unwrap_constant()?;
                let (r#type, list) = args[1].unwrap_list()?;

                if *r#type != Type::from(item) {
                    return Err(Error::TypeMismatch(Type::from(item), r#type.clone()));
                }

                let mut ret = vec![item.clone()];

                ret.extend(list.clone());

                let value = Value::list(r#type.clone(), ret);

                Ok(value)
            }
            DefaultFunction::HeadList => {
                let (_, list) = args[0].unwrap_list()?;

                if list.is_empty() {
                    Err(Error::EmptyList(args[0].clone()))
                } else {
                    let value = Value::Con(list[0].clone().into());

                    Ok(value)
                }
            }
            DefaultFunction::TailList => {
                let (r#type, list) = args[0].unwrap_list()?;

                if list.is_empty() {
                    Err(Error::EmptyList(args[0].clone()))
                } else {
                    let value = Value::list(r#type.clone(), list[1..].to_vec());

                    Ok(value)
                }
            }
            DefaultFunction::NullList => {
                let (_, list) = args[0].unwrap_list()?;

                let value = Value::bool(list.is_empty());

                Ok(value)
            }
            DefaultFunction::ChooseData => {
                let con = args[0].unwrap_data()?;

                match con {
                    PlutusData::Constr(_) => Ok(args[1].clone()),
                    PlutusData::Map(_) => Ok(args[2].clone()),
                    PlutusData::Array(_) => Ok(args[3].clone()),
                    PlutusData::BigInt(_) => Ok(args[4].clone()),
                    PlutusData::BoundedBytes(_) => Ok(args[5].clone()),
                }
            }
            DefaultFunction::ConstrData => {
                let i = args[0].unwrap_integer()?;
                let l = args[1].unwrap_data_list()?;

                let data_list: Vec<PlutusData> = l
                    .iter()
                    .map(|item| match item {
                        Constant::Data(d) => d.clone(),
                        _ => unreachable!(),
                    })
                    .collect();

                let i: u64 = i.try_into().unwrap();

                let constr_data = Data::constr(i, data_list);

                let value = Value::data(constr_data);

                Ok(value)
            }
            DefaultFunction::MapData => {
                let (r#type, list) = args[0].unwrap_list()?;

                if *r#type != Type::Pair(Rc::new(Type::Data), Rc::new(Type::Data)) {
                    return Err(Error::TypeMismatch(
                        Type::List(Rc::new(Type::Pair(
                            Rc::new(Type::Data),
                            Rc::new(Type::Data),
                        ))),
                        r#type.clone(),
                    ));
                }

                let mut map = Vec::new();

                for item in list {
                    let Constant::ProtoPair(Type::Data, Type::Data, left, right) = item else {
                        unreachable!()
                    };

                    let (Constant::Data(key), Constant::Data(value)) =
                        (left.as_ref(), right.as_ref())
                    else {
                        unreachable!()
                    };

                    map.push((key.clone(), value.clone()));
                }

                let value = Value::data(PlutusData::Map(map.into()));

                Ok(value)
            }
            DefaultFunction::ListData => {
                let list = args[0].unwrap_data_list()?;

                let data_list: Vec<PlutusData> = list
                    .iter()
                    .map(|item| match item {
                        Constant::Data(d) => d.clone(),
                        _ => unreachable!(),
                    })
                    .collect();

                let value = Value::data(PlutusData::Array(data_list));

                Ok(value)
            }
            DefaultFunction::IData => {
                let i = args[0].unwrap_integer()?;

                let value = Value::data(PlutusData::BigInt(to_pallas_bigint(i)));

                Ok(value)
            }
            DefaultFunction::BData => {
                let b = args[0].unwrap_byte_string()?;

                let value = Value::data(PlutusData::BoundedBytes(b.clone().into()));

                Ok(value)
            }
            DefaultFunction::UnConstrData => match &args[0] {
                v @ Value::Con(inner) => {
                    let Constant::Data(PlutusData::Constr(c)) = inner.as_ref() else {
                        return Err(Error::DeserialisationError(
                            "UnConstrData".to_string(),
                            v.clone(),
                        ));
                    };

                    let constant = Constant::ProtoPair(
                        Type::Integer,
                        Type::List(Type::Data.into()),
                        Constant::Integer(
                            convert_tag_to_constr(c.tag)
                                .unwrap_or_else(|| c.any_constructor.unwrap())
                                .into(),
                        )
                        .into(),
                        Constant::ProtoList(
                            Type::Data,
                            c.fields
                                .deref()
                                .iter()
                                .map(|d| Constant::Data(d.clone()))
                                .collect(),
                        )
                        .into(),
                    );

                    let value = Value::Con(constant.into());

                    Ok(value)
                }
                v => Err(Error::NotAConstant(v.clone())),
            },
            DefaultFunction::UnMapData => match &args[0] {
                v @ Value::Con(inner) => {
                    let Constant::Data(PlutusData::Map(m)) = inner.as_ref() else {
                        return Err(Error::DeserialisationError(
                            "UnMapData".to_string(),
                            v.clone(),
                        ));
                    };

                    let constant = Constant::ProtoList(
                        Type::Pair(Type::Data.into(), Type::Data.into()),
                        m.deref()
                            .iter()
                            .map(|p| -> Constant {
                                Constant::ProtoPair(
                                    Type::Data,
                                    Type::Data,
                                    Constant::Data(p.0.clone()).into(),
                                    Constant::Data(p.1.clone()).into(),
                                )
                            })
                            .collect(),
                    );

                    let value = Value::Con(constant.into());

                    Ok(value)
                }
                v => Err(Error::NotAConstant(v.clone())),
            },
            DefaultFunction::UnListData => match &args[0] {
                v @ Value::Con(inner) => {
                    let Constant::Data(PlutusData::Array(l)) = inner.as_ref() else {
                        return Err(Error::DeserialisationError(
                            "UnListData".to_string(),
                            v.clone(),
                        ));
                    };

                    let value = Value::list(
                        Type::Data,
                        l.deref()
                            .iter()
                            .map(|d| Constant::Data(d.clone()))
                            .collect(),
                    );

                    Ok(value)
                }
                v => Err(Error::NotAConstant(v.clone())),
            },
            DefaultFunction::UnIData => match &args[0] {
                v @ Value::Con(inner) => {
                    let Constant::Data(PlutusData::BigInt(b)) = inner.as_ref() else {
                        return Err(Error::DeserialisationError(
                            "UnIData".to_string(),
                            v.clone(),
                        ));
                    };

                    let value = Value::integer(from_pallas_bigint(b));

                    Ok(value)
                }
                v => Err(Error::NotAConstant(v.clone())),
            },
            DefaultFunction::UnBData => match &args[0] {
                v @ Value::Con(inner) => {
                    let Constant::Data(PlutusData::BoundedBytes(b)) = inner.as_ref() else {
                        return Err(Error::DeserialisationError(
                            "UnBData".to_string(),
                            v.clone(),
                        ));
                    };

                    let value = Value::byte_string(b.to_vec());

                    Ok(value)
                }
                v => Err(Error::NotAConstant(v.clone())),
            },
            DefaultFunction::EqualsData => {
                let d1 = args[0].unwrap_data()?;
                let d2 = args[1].unwrap_data()?;

                let value = Value::bool(d1.eq(d2));

                Ok(value)
            }
            DefaultFunction::SerialiseData => {
                let d = args[0].unwrap_data()?;

                let serialized_data = plutus_data_to_bytes(d).unwrap();

                let value = Value::byte_string(serialized_data);

                Ok(value)
            }
            DefaultFunction::MkPairData => {
                let d1 = args[0].unwrap_data()?;
                let d2 = args[1].unwrap_data()?;

                let constant = Constant::ProtoPair(
                    Type::Data,
                    Type::Data,
                    Constant::Data(d1.clone()).into(),
                    Constant::Data(d2.clone()).into(),
                );

                let value = Value::Con(constant.into());

                Ok(value)
            }
            DefaultFunction::MkNilData => {
                args[0].unwrap_unit()?;

                let value = Value::list(Type::Data, vec![]);

                Ok(value)
            }
            DefaultFunction::MkNilPairData => {
                args[0].unwrap_unit()?;

                let constant = Constant::ProtoList(
                    Type::Pair(Rc::new(Type::Data), Rc::new(Type::Data)),
                    vec![],
                );

                let value = Value::Con(constant.into());

                Ok(value)
            }

            DefaultFunction::Bls12_381_G1_Add => {
                let arg1 = args[0].unwrap_bls12_381_g1_element()?;
                let arg2 = args[1].unwrap_bls12_381_g1_element()?;

                let mut out = blst::blst_p1::default();

                unsafe {
                    blst::blst_p1_add_or_double(
                        &mut out as *mut _,
                        arg1 as *const _,
                        arg2 as *const _,
                    );
                }

                let constant = Constant::Bls12_381G1Element(out.into());

                Ok(Value::Con(constant.into()))
            }
            DefaultFunction::Bls12_381_G1_Neg => {
                let arg1 = args[0].unwrap_bls12_381_g1_element()?;

                let mut out = *arg1;

                unsafe {
                    blst::blst_p1_cneg(
                        &mut out as *mut _,
                        // This was true in the Cardano code
                        true,
                    );
                }

                let constant = Constant::Bls12_381G1Element(out.into());

                Ok(Value::Con(constant.into()))
            }
            DefaultFunction::Bls12_381_G1_ScalarMul => {
                let arg1 = args[0].unwrap_integer()?;
                let arg2 = args[1].unwrap_bls12_381_g1_element()?;

                let size_scalar = size_of::<blst::blst_scalar>();

                let arg1 = arg1.mod_floor(&SCALAR_PERIOD);

                let (_, mut arg1) = arg1.to_bytes_be();

                if size_scalar > arg1.len() {
                    let diff = size_scalar - arg1.len();

                    let mut new_vec = vec![0; diff];

                    new_vec.append(&mut arg1);

                    arg1 = new_vec;
                }

                let mut out = blst::blst_p1::default();
                let mut scalar = blst::blst_scalar::default();

                unsafe {
                    blst::blst_scalar_from_bendian(
                        &mut scalar as *mut _,
                        arg1.as_ptr() as *const _,
                    );

                    blst::blst_p1_mult(
                        &mut out as *mut _,
                        arg2 as *const _,
                        scalar.b.as_ptr() as *const _,
                        size_scalar * 8,
                    );
                }

                let constant = Constant::Bls12_381G1Element(out.into());

                Ok(Value::Con(constant.into()))
            }
            DefaultFunction::Bls12_381_G1_Equal => {
                let arg1 = args[0].unwrap_bls12_381_g1_element()?;
                let arg2 = args[1].unwrap_bls12_381_g1_element()?;

                let is_equal = unsafe { blst::blst_p1_is_equal(arg1, arg2) };

                let constant = Constant::Bool(is_equal);

                Ok(Value::Con(constant.into()))
            }
            DefaultFunction::Bls12_381_G1_Compress => {
                let arg1 = args[0].unwrap_bls12_381_g1_element()?;

                let out = arg1.compress();

                let constant = Constant::ByteString(out.to_vec());

                Ok(Value::Con(constant.into()))
            }
            DefaultFunction::Bls12_381_G1_Uncompress => {
                let arg1 = args[0].unwrap_byte_string()?;

                let out = blst::blst_p1::uncompress(arg1)?;

                let constant = Constant::Bls12_381G1Element(out.into());

                Ok(Value::Con(constant.into()))
            }
            DefaultFunction::Bls12_381_G1_HashToGroup => {
                let arg1 = args[0].unwrap_byte_string()?;
                let arg2 = args[1].unwrap_byte_string()?;

                if arg2.len() > 255 {
                    return Err(Error::HashToCurveDstTooBig);
                }

                let mut out = blst::blst_p1::default();
                let aug = [];

                unsafe {
                    blst::blst_hash_to_g1(
                        &mut out as *mut _,
                        arg1.as_ptr(),
                        arg1.len(),
                        arg2.as_ptr(),
                        arg2.len(),
                        aug.as_ptr(),
                        0,
                    );
                };

                let constant = Constant::Bls12_381G1Element(out.into());

                Ok(Value::Con(constant.into()))
            }
            DefaultFunction::Bls12_381_G2_Add => {
                let arg1 = args[0].unwrap_bls12_381_g2_element()?;
                let arg2 = args[1].unwrap_bls12_381_g2_element()?;

                let mut out = blst::blst_p2::default();

                unsafe {
                    blst::blst_p2_add_or_double(
                        &mut out as *mut _,
                        arg1 as *const _,
                        arg2 as *const _,
                    );
                }

                let constant = Constant::Bls12_381G2Element(out.into());

                Ok(Value::Con(constant.into()))
            }
            DefaultFunction::Bls12_381_G2_Neg => {
                let arg1 = args[0].unwrap_bls12_381_g2_element()?;

                let mut out = *arg1;

                unsafe {
                    blst::blst_p2_cneg(
                        &mut out as *mut _,
                        // This was true in the Cardano code
                        true,
                    );
                }

                let constant = Constant::Bls12_381G2Element(out.into());

                Ok(Value::Con(constant.into()))
            }
            DefaultFunction::Bls12_381_G2_ScalarMul => {
                let arg1 = args[0].unwrap_integer()?;
                let arg2 = args[1].unwrap_bls12_381_g2_element()?;

                let size_scalar = size_of::<blst::blst_scalar>();

                let arg1 = arg1.mod_floor(&SCALAR_PERIOD);

                let (_, mut arg1) = arg1.to_bytes_be();

                if size_scalar > arg1.len() {
                    let diff = size_scalar - arg1.len();

                    let mut new_vec = vec![0; diff];

                    new_vec.append(&mut arg1);

                    arg1 = new_vec;
                }

                let mut out = blst::blst_p2::default();
                let mut scalar = blst::blst_scalar::default();

                unsafe {
                    blst::blst_scalar_from_bendian(
                        &mut scalar as *mut _,
                        arg1.as_ptr() as *const _,
                    );

                    blst::blst_p2_mult(
                        &mut out as *mut _,
                        arg2 as *const _,
                        scalar.b.as_ptr() as *const _,
                        size_scalar * 8,
                    );
                }

                let constant = Constant::Bls12_381G2Element(out.into());

                Ok(Value::Con(constant.into()))
            }
            DefaultFunction::Bls12_381_G2_Equal => {
                let arg1 = args[0].unwrap_bls12_381_g2_element()?;
                let arg2 = args[1].unwrap_bls12_381_g2_element()?;

                let is_equal = unsafe { blst::blst_p2_is_equal(arg1, arg2) };

                let constant = Constant::Bool(is_equal);

                Ok(Value::Con(constant.into()))
            }
            DefaultFunction::Bls12_381_G2_Compress => {
                let arg1 = args[0].unwrap_bls12_381_g2_element()?;

                let out = arg1.compress();

                let constant = Constant::ByteString(out.to_vec());

                Ok(Value::Con(constant.into()))
            }
            DefaultFunction::Bls12_381_G2_Uncompress => {
                let arg1 = args[0].unwrap_byte_string()?;

                let out = blst::blst_p2::uncompress(arg1)?;

                let constant = Constant::Bls12_381G2Element(out.into());

                Ok(Value::Con(constant.into()))
            }
            DefaultFunction::Bls12_381_G2_HashToGroup => {
                let arg1 = args[0].unwrap_byte_string()?;
                let arg2 = args[1].unwrap_byte_string()?;

                if arg2.len() > 255 {
                    return Err(Error::HashToCurveDstTooBig);
                }

                let mut out = blst::blst_p2::default();
                let aug = [];

                unsafe {
                    blst::blst_hash_to_g2(
                        &mut out as *mut _,
                        arg1.as_ptr(),
                        arg1.len(),
                        arg2.as_ptr(),
                        arg2.len(),
                        aug.as_ptr(),
                        0,
                    );
                };

                let constant = Constant::Bls12_381G2Element(out.into());

                Ok(Value::Con(constant.into()))
            }
            DefaultFunction::Bls12_381_MillerLoop => {
                let arg1 = args[0].unwrap_bls12_381_g1_element()?;
                let arg2 = args[1].unwrap_bls12_381_g2_element()?;

                let mut out = blst::blst_fp12::default();

                let mut affine1 = blst::blst_p1_affine::default();
                let mut affine2 = blst::blst_p2_affine::default();

                unsafe {
                    blst::blst_p1_to_affine(&mut affine1 as *mut _, arg1);
                    blst::blst_p2_to_affine(&mut affine2 as *mut _, arg2);

                    blst::blst_miller_loop(&mut out as *mut _, &affine2, &affine1);
                }

                let constant = Constant::Bls12_381MlResult(out.into());

                Ok(Value::Con(constant.into()))
            }
            DefaultFunction::Bls12_381_MulMlResult => {
                let arg1 = args[0].unwrap_bls12_381_ml_result()?;
                let arg2 = args[1].unwrap_bls12_381_ml_result()?;

                let mut out = blst::blst_fp12::default();

                unsafe {
                    blst::blst_fp12_mul(&mut out as *mut _, arg1, arg2);
                }

                let constant = Constant::Bls12_381MlResult(out.into());

                Ok(Value::Con(constant.into()))
            }
            DefaultFunction::Bls12_381_FinalVerify => {
                let arg1 = args[0].unwrap_bls12_381_ml_result()?;
                let arg2 = args[1].unwrap_bls12_381_ml_result()?;

                let verified = unsafe { blst::blst_fp12_finalverify(arg1, arg2) };

                let constant = Constant::Bool(verified);

                Ok(Value::Con(constant.into()))
            }
            DefaultFunction::IntegerToByteString => {
                let endianness = args[0].unwrap_bool()?;
                let size = args[1].unwrap_integer()?;
                let input = args[2].unwrap_integer()?;

                if size.is_negative() {
                    return Err(Error::IntegerToByteStringNegativeSize(size.clone()));
                }

                if size > &INTEGER_TO_BYTE_STRING_MAXIMUM_OUTPUT_LENGTH.into() {
                    return Err(Error::IntegerToByteStringSizeTooBig(
                        size.clone(),
                        INTEGER_TO_BYTE_STRING_MAXIMUM_OUTPUT_LENGTH,
                    ));
                }

                if size.is_zero()
                    && integer_log2(input.clone())
                        >= 8 * INTEGER_TO_BYTE_STRING_MAXIMUM_OUTPUT_LENGTH
                {
                    let required = integer_log2(input.clone()) / 8 + 1;

                    return Err(Error::IntegerToByteStringSizeTooBig(
                        required.into(),
                        INTEGER_TO_BYTE_STRING_MAXIMUM_OUTPUT_LENGTH,
                    ));
                }

                if input.is_negative() {
                    return Err(Error::IntegerToByteStringNegativeInput(input.clone()));
                }

                let size_unwrapped: usize = size.try_into().unwrap();

                if input.is_zero() {
                    let constant = Constant::ByteString(vec![0; size_unwrapped]);

                    return Ok(Value::Con(constant.into()));
                }

                let (_, mut bytes) = if *endianness {
                    input.to_bytes_be()
                } else {
                    input.to_bytes_le()
                };

                if !size.is_zero() && bytes.len() > size_unwrapped {
                    return Err(Error::IntegerToByteStringSizeTooSmall(
                        size.clone(),
                        bytes.len(),
                    ));
                }

                if size_unwrapped > 0 {
                    let padding_size = size_unwrapped - bytes.len();

                    let mut padding = vec![0; padding_size];

                    if *endianness {
                        padding.append(&mut bytes);

                        bytes = padding;
                    } else {
                        bytes.append(&mut padding);
                    }
                };

                let constant = Constant::ByteString(bytes);

                Ok(Value::Con(constant.into()))
            }
            DefaultFunction::ByteStringToInteger => {
                let endianness = args[0].unwrap_bool()?;
                let bytes = args[1].unwrap_byte_string()?;

                let number = if *endianness {
                    BigInt::from_bytes_be(num_bigint::Sign::Plus, bytes)
                } else {
                    BigInt::from_bytes_le(num_bigint::Sign::Plus, bytes)
                };

                let constant = Constant::Integer(number);

                Ok(Value::Con(constant.into()))
            }
        }
    }
}

pub trait Compressable {
    fn compress(&self) -> Vec<u8>;

    fn uncompress(bytes: &[u8]) -> Result<Self, Error>
    where
        Self: std::marker::Sized;
}

impl Compressable for blst::blst_p1 {
    fn compress(&self) -> Vec<u8> {
        let mut out = [0; BLST_P1_COMPRESSED_SIZE];

        unsafe {
            blst::blst_p1_compress(&mut out as *mut _, self);
        };

        out.to_vec()
    }

    fn uncompress(bytes: &[u8]) -> Result<Self, Error> {
        if bytes.len() != BLST_P1_COMPRESSED_SIZE {
            return Err(Error::Blst(blst::BLST_ERROR::BLST_BAD_ENCODING));
        }

        let mut affine = blst::blst_p1_affine::default();

        let mut out = blst::blst_p1::default();

        unsafe {
            let err = blst::blst_p1_uncompress(&mut affine as *mut _, bytes.as_ptr());

            if err != blst::BLST_ERROR::BLST_SUCCESS {
                return Err(Error::Blst(err));
            }

            blst::blst_p1_from_affine(&mut out as *mut _, &affine);

            let in_group = blst::blst_p1_in_g1(&out);

            if !in_group {
                return Err(Error::Blst(blst::BLST_ERROR::BLST_POINT_NOT_IN_GROUP));
            }
        };

        Ok(out)
    }
}

impl Compressable for blst::blst_p2 {
    fn compress(&self) -> Vec<u8> {
        let mut out = [0; BLST_P2_COMPRESSED_SIZE];

        unsafe {
            blst::blst_p2_compress(&mut out as *mut _, self);
        };

        out.to_vec()
    }

    fn uncompress(bytes: &[u8]) -> Result<Self, Error> {
        if bytes.len() != BLST_P2_COMPRESSED_SIZE {
            return Err(Error::Blst(blst::BLST_ERROR::BLST_BAD_ENCODING));
        }

        let mut affine = blst::blst_p2_affine::default();

        let mut out = blst::blst_p2::default();

        unsafe {
            let err = blst::blst_p2_uncompress(&mut affine as *mut _, bytes.as_ptr());

            if err != blst::BLST_ERROR::BLST_SUCCESS {
                return Err(Error::Blst(err));
            }

            blst::blst_p2_from_affine(&mut out as *mut _, &affine);

            let in_group = blst::blst_p2_in_g2(&out);

            if !in_group {
                return Err(Error::Blst(blst::BLST_ERROR::BLST_POINT_NOT_IN_GROUP));
            }
        };

        Ok(out)
    }
}

pub fn convert_tag_to_constr(tag: u64) -> Option<u64> {
    if (121..=127).contains(&tag) {
        Some(tag - 121)
    } else if (1280..=1400).contains(&tag) {
        Some(tag - 1280 + 7)
    } else {
        None
    }
}

pub fn convert_constr_to_tag(constr: u64) -> Option<u64> {
    if (0..=6).contains(&constr) {
        Some(121 + constr)
    } else if (7..=127).contains(&constr) {
        Some(1280 - 7 + constr)
    } else {
        None // 102 otherwise
    }
}

pub static ANY_TAG: u64 = 102;

#[cfg(not(target_family = "wasm"))]
fn verify_ecdsa(public_key: &[u8], message: &[u8], signature: &[u8]) -> Result<Value, Error> {
    use secp256k1::{ecdsa::Signature, Message, PublicKey, Secp256k1};

    let secp = Secp256k1::verification_only();

    let public_key = PublicKey::from_slice(public_key)?;

    let signature = Signature::from_compact(signature)?;

    let message = Message::from_slice(message)?;

    let valid = secp.verify_ecdsa(&message, &signature, &public_key);

    Ok(Value::Con(Constant::Bool(valid.is_ok()).into()))
}

/// Unlike the Haskell implementation the schnorr verification function in Aiken doesn't allow for arbitrary message sizes (at the moment).
/// The message needs to be 32 bytes (ideally prehashed, but not a requirement).
#[cfg(not(target_family = "wasm"))]
fn verify_schnorr(public_key: &[u8], message: &[u8], signature: &[u8]) -> Result<Value, Error> {
    use secp256k1::{schnorr::Signature, Message, Secp256k1, XOnlyPublicKey};

    let secp = Secp256k1::verification_only();

    let public_key = XOnlyPublicKey::from_slice(public_key)?;

    let signature = Signature::from_slice(signature)?;

    let message = Message::from_slice(message)?;

    let valid = secp.verify_schnorr(&signature, &message, &public_key);

    Ok(Value::Con(Constant::Bool(valid.is_ok()).into()))
}

#[cfg(target_family = "wasm")]
fn verify_ecdsa(public_key: &[u8], message: &[u8], signature: &[u8]) -> Result<Value, Error> {
    use k256::ecdsa::{self, signature::hazmat::PrehashVerifier};

    let verifying_key = ecdsa::VerifyingKey::try_from(public_key)?;

    let signature = ecdsa::Signature::try_from(signature)?;

    let valid = verifying_key.verify_prehash(message, &signature);

    Ok(Value::Con(Constant::Bool(valid.is_ok()).into()))
}

#[cfg(target_family = "wasm")]
fn verify_schnorr(public_key: &[u8], message: &[u8], signature: &[u8]) -> Result<Value, Error> {
    use k256::schnorr::{self, signature::hazmat::PrehashVerifier};

    let verifying_key = schnorr::VerifyingKey::from_bytes(public_key)?;

    let signature = schnorr::Signature::try_from(signature)?;

    let valid = verifying_key.verify_prehash(message, &signature);

    Ok(Value::Con(Constant::Bool(valid.is_ok()).into()))
}

#[cfg(test)]
mod tests {
    use super::{convert_constr_to_tag, convert_tag_to_constr};

    #[test]
    fn compact_tag_range() {
        assert_eq!(convert_constr_to_tag(0), Some(121));
        assert_eq!(convert_constr_to_tag(1), Some(122));
        assert_eq!(convert_constr_to_tag(6), Some(127));
        assert_ne!(convert_constr_to_tag(7), Some(128)); // This is not allowed
    }
    #[test]
    fn compact_tag_mid_range() {
        assert_eq!(convert_constr_to_tag(7), Some(1280));
        assert_eq!(convert_constr_to_tag(8), Some(1281));
        assert_eq!(convert_constr_to_tag(100), Some(1373));
        assert_eq!(convert_constr_to_tag(127), Some(1400));
        assert_ne!(convert_constr_to_tag(128), Some(1401)); // This is not allowed
    }

    #[test]
    fn any_range() {
        assert_eq!(convert_constr_to_tag(128), None);
        assert_eq!(
            convert_constr_to_tag(128).map_or(Some(128), |_| None),
            Some(128)
        );
        assert_eq!(convert_constr_to_tag(123124125125), None);
        assert_eq!(convert_constr_to_tag(1).map_or(Some(1), |_| None), None); // This is a compact tag
    }

    #[test]
    fn to_compact_tag() {
        assert_eq!(convert_tag_to_constr(121), Some(0));
        assert_eq!(convert_tag_to_constr(122), Some(1));
        assert_eq!(convert_tag_to_constr(127), Some(6));
        assert_eq!(convert_tag_to_constr(128), None); // This can never happen actually. Pallas sorts that out already during deserialization.
    }
    #[test]
    fn to_compact_tag_mid() {
        assert_eq!(convert_tag_to_constr(1280), Some(7));
        assert_eq!(convert_tag_to_constr(1281), Some(8));
        assert_eq!(convert_tag_to_constr(1400), Some(127));
        assert_eq!(convert_tag_to_constr(1401), None); // This can never happen actually. Pallas sorts that out already during deserialization.
    }

    #[test]
    fn to_any_tag() {
        assert_eq!(convert_tag_to_constr(102), None);
    }
}
