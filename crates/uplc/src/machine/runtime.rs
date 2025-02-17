use super::{
    cost_model::{BuiltinCosts, ExBudget},
    value::{from_pallas_bigint, to_pallas_bigint},
    Error, Trace, Value,
};
use crate::{
    ast::{Constant, Data, Type},
    builtins::DefaultFunction,
    machine::value::integer_log2,
    plutus_data_to_bytes,
};
use bitvec::{order::Msb0, vec::BitVec};
use itertools::Itertools;
use num_bigint::BigInt;
use num_integer::Integer;
use num_traits::{FromPrimitive, Signed, Zero};
use once_cell::sync::Lazy;
use pallas_primitives::conway::{Language, PlutusData};
use std::{mem::size_of, ops::Deref, rc::Rc};

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

pub const INTEGER_TO_BYTE_STRING_MAXIMUM_OUTPUT_LENGTH: i64 = 8192;

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

#[derive(Clone, Debug, PartialEq)]
pub struct BuiltinRuntime {
    pub(super) args: Vec<Value>,
    pub fun: DefaultFunction,
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

    pub fn call(&self, language: &Language, traces: &mut Vec<Trace>) -> Result<Value, Error> {
        self.fun.call(language.into(), &self.args, traces)
    }

    pub fn push(&mut self, arg: Value) -> Result<(), Error> {
        self.args.push(arg);

        Ok(())
    }

    pub fn to_ex_budget(&self, costs: &BuiltinCosts) -> Result<ExBudget, Error> {
        costs.to_ex_budget(self.fun, &self.args)
    }
}

impl From<DefaultFunction> for BuiltinRuntime {
    fn from(fun: DefaultFunction) -> Self {
        BuiltinRuntime::new(fun)
    }
}

impl DefaultFunction {
    pub fn arg_is_unit(&self) -> bool {
        match self {
            DefaultFunction::MkNilData | DefaultFunction::MkNilPairData => true,
            DefaultFunction::AddInteger
            | DefaultFunction::SubtractInteger
            | DefaultFunction::MultiplyInteger
            | DefaultFunction::DivideInteger
            | DefaultFunction::QuotientInteger
            | DefaultFunction::RemainderInteger
            | DefaultFunction::ModInteger
            | DefaultFunction::EqualsInteger
            | DefaultFunction::LessThanInteger
            | DefaultFunction::LessThanEqualsInteger
            | DefaultFunction::AppendByteString
            | DefaultFunction::ConsByteString
            | DefaultFunction::SliceByteString
            | DefaultFunction::LengthOfByteString
            | DefaultFunction::IndexByteString
            | DefaultFunction::EqualsByteString
            | DefaultFunction::LessThanByteString
            | DefaultFunction::LessThanEqualsByteString
            | DefaultFunction::Sha2_256
            | DefaultFunction::Sha3_256
            | DefaultFunction::Blake2b_224
            | DefaultFunction::Blake2b_256
            | DefaultFunction::Keccak_256
            | DefaultFunction::VerifyEd25519Signature
            | DefaultFunction::VerifyEcdsaSecp256k1Signature
            | DefaultFunction::VerifySchnorrSecp256k1Signature
            | DefaultFunction::AppendString
            | DefaultFunction::EqualsString
            | DefaultFunction::EncodeUtf8
            | DefaultFunction::DecodeUtf8
            | DefaultFunction::IfThenElse
            | DefaultFunction::ChooseUnit
            | DefaultFunction::Trace
            | DefaultFunction::FstPair
            | DefaultFunction::SndPair
            | DefaultFunction::ChooseList
            | DefaultFunction::MkCons
            | DefaultFunction::HeadList
            | DefaultFunction::TailList
            | DefaultFunction::NullList
            | DefaultFunction::ChooseData
            | DefaultFunction::ConstrData
            | DefaultFunction::MapData
            | DefaultFunction::ListData
            | DefaultFunction::IData
            | DefaultFunction::BData
            | DefaultFunction::UnConstrData
            | DefaultFunction::UnMapData
            | DefaultFunction::UnListData
            | DefaultFunction::UnIData
            | DefaultFunction::UnBData
            | DefaultFunction::EqualsData
            | DefaultFunction::SerialiseData
            | DefaultFunction::MkPairData
            | DefaultFunction::Bls12_381_G1_Add
            | DefaultFunction::Bls12_381_G1_Neg
            | DefaultFunction::Bls12_381_G1_ScalarMul
            | DefaultFunction::Bls12_381_G1_Equal
            | DefaultFunction::Bls12_381_G1_Compress
            | DefaultFunction::Bls12_381_G1_Uncompress
            | DefaultFunction::Bls12_381_G1_HashToGroup
            | DefaultFunction::Bls12_381_G2_Add
            | DefaultFunction::Bls12_381_G2_Neg
            | DefaultFunction::Bls12_381_G2_ScalarMul
            | DefaultFunction::Bls12_381_G2_Equal
            | DefaultFunction::Bls12_381_G2_Compress
            | DefaultFunction::Bls12_381_G2_Uncompress
            | DefaultFunction::Bls12_381_G2_HashToGroup
            | DefaultFunction::Bls12_381_MillerLoop
            | DefaultFunction::Bls12_381_MulMlResult
            | DefaultFunction::Bls12_381_FinalVerify
            | DefaultFunction::IntegerToByteString
            | DefaultFunction::ByteStringToInteger
            | DefaultFunction::AndByteString
            | DefaultFunction::OrByteString
            | DefaultFunction::XorByteString
            | DefaultFunction::ComplementByteString
            | DefaultFunction::ReadBit
            | DefaultFunction::WriteBits
            | DefaultFunction::ReplicateByte
            | DefaultFunction::ShiftByteString
            | DefaultFunction::RotateByteString
            | DefaultFunction::CountSetBits
            | DefaultFunction::FindFirstSetBit
            | DefaultFunction::Ripemd_160 => false,
            // | DefaultFunction::ExpModInteger
            // | DefaultFunction::CaseList
            // | DefaultFunction::CaseData
        }
    }

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
            DefaultFunction::AndByteString => 3,
            DefaultFunction::OrByteString => 3,
            DefaultFunction::XorByteString => 3,
            DefaultFunction::ComplementByteString => 1,
            DefaultFunction::ReadBit => 2,
            DefaultFunction::WriteBits => 3,
            DefaultFunction::ReplicateByte => 2,
            DefaultFunction::ShiftByteString => 2,
            DefaultFunction::RotateByteString => 2,
            DefaultFunction::CountSetBits => 1,
            DefaultFunction::FindFirstSetBit => 1,
            DefaultFunction::Ripemd_160 => 1,
            // DefaultFunction::ExpModInteger => 3,
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
            DefaultFunction::AndByteString => 0,
            DefaultFunction::OrByteString => 0,
            DefaultFunction::XorByteString => 0,
            DefaultFunction::ComplementByteString => 0,
            DefaultFunction::ReadBit => 0,
            DefaultFunction::WriteBits => 0,
            DefaultFunction::ReplicateByte => 0,
            DefaultFunction::ShiftByteString => 0,
            DefaultFunction::RotateByteString => 0,
            DefaultFunction::CountSetBits => 0,
            DefaultFunction::FindFirstSetBit => 0,
            DefaultFunction::Ripemd_160 => 0,
            // DefaultFunction::ExpModInteger => 0,
        }
    }

    pub fn call(
        &self,
        semantics: BuiltinSemantics,
        args: &[Value],
        traces: &mut Vec<Trace>,
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
                        if *arg1 > 255.into() || *arg1 < 0.into() {
                            return Err(Error::ByteStringConsNotAByte(arg1.clone()));
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

                if arg1.starts_with('\0') {
                    traces.push(Trace::Label(arg1.split_at(1).1.to_string()));
                } else {
                    traces.push(Trace::Log(arg1.clone()));
                }

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

                let value = Value::data(Data::list(data_list));

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

                // NOTE:
                // We ought to also check for negative size and too large sizes. These checks
                // however happens prior to calling the builtin as part of the costing step. So by
                // the time we reach this builtin call, the size can be assumed to be
                //
                // >= 0 && < INTEGER_TO_BYTE_STRING_MAXIMUM_OUTPUT_LENGTH

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
            DefaultFunction::AndByteString => {
                let should_pad = args[0].unwrap_bool()?;
                let bytes1 = args[1].unwrap_byte_string()?;
                let bytes2 = args[2].unwrap_byte_string()?;

                let bytes_result = if *should_pad {
                    bytes1
                        .iter()
                        .zip_longest(bytes2)
                        .map(|b| match b {
                            itertools::EitherOrBoth::Both(left_byte, right_byte) => {
                                left_byte & right_byte
                            }
                            // Shorter is appended with FF bytes that when and-ed produce the other bytestring
                            itertools::EitherOrBoth::Left(byte)
                            | itertools::EitherOrBoth::Right(byte) => *byte,
                        })
                        .collect_vec()
                } else {
                    bytes1
                        .iter()
                        .zip(bytes2)
                        .map(|(b1, b2)| b1 & b2)
                        .collect_vec()
                };

                Ok(Value::byte_string(bytes_result))
            }
            DefaultFunction::OrByteString => {
                let should_pad = args[0].unwrap_bool()?;
                let bytes1 = args[1].unwrap_byte_string()?;
                let bytes2 = args[2].unwrap_byte_string()?;

                let bytes_result = if *should_pad {
                    bytes1
                        .iter()
                        .zip_longest(bytes2)
                        .map(|b| match b {
                            itertools::EitherOrBoth::Both(left_byte, right_byte) => {
                                left_byte | right_byte
                            }
                            // Shorter is appended with 00 bytes that when or-ed produce the other bytestring
                            itertools::EitherOrBoth::Left(byte)
                            | itertools::EitherOrBoth::Right(byte) => *byte,
                        })
                        .collect_vec()
                } else {
                    bytes1
                        .iter()
                        .zip(bytes2)
                        .map(|(b1, b2)| b1 | b2)
                        .collect_vec()
                };

                Ok(Value::byte_string(bytes_result))
            }
            DefaultFunction::XorByteString => {
                let should_pad = args[0].unwrap_bool()?;
                let bytes1 = args[1].unwrap_byte_string()?;
                let bytes2 = args[2].unwrap_byte_string()?;

                let bytes_result = if *should_pad {
                    bytes1
                        .iter()
                        .zip_longest(bytes2)
                        .map(|b| match b {
                            itertools::EitherOrBoth::Both(left_byte, right_byte) => {
                                left_byte ^ right_byte
                            }
                            // Shorter is appended with 00 bytes that when xor-ed produce the other bytestring
                            itertools::EitherOrBoth::Left(byte)
                            | itertools::EitherOrBoth::Right(byte) => *byte,
                        })
                        .collect_vec()
                } else {
                    bytes1
                        .iter()
                        .zip(bytes2)
                        .map(|(b1, b2)| b1 ^ b2)
                        .collect_vec()
                };

                Ok(Value::byte_string(bytes_result))
            }
            DefaultFunction::ComplementByteString => {
                let bytes = args[0].unwrap_byte_string()?;

                let result = bytes.iter().map(|b| b ^ 255).collect_vec();

                Ok(Value::byte_string(result))
            }
            DefaultFunction::ReadBit => {
                let bytes = args[0].unwrap_byte_string()?;
                let bit_index = args[1].unwrap_integer()?;

                if bytes.is_empty() {
                    return Err(Error::EmptyByteArray);
                }

                // This ensures there is at least one byte in bytes
                if *bit_index < 0.into() || *bit_index >= (bytes.len() * 8).into() {
                    return Err(Error::ReadBitOutOfBounds);
                }

                let (byte_index, bit_offset) = bit_index.div_rem(&8.into());

                let bit_offset = usize::try_from(bit_offset).unwrap();

                let flipped_index = bytes.len() - 1 - usize::try_from(byte_index).unwrap();

                let byte = bytes[flipped_index];

                let bit_test = (byte >> bit_offset) & 1 == 1;

                Ok(Value::bool(bit_test))
            }
            DefaultFunction::WriteBits => {
                let mut bytes = args[0].unwrap_byte_string()?.clone();
                let indices = args[1].unwrap_int_list()?;
                let set_bit = args[2].unwrap_bool()?;

                for index in indices {
                    let Constant::Integer(bit_index) = index else {
                        unreachable!()
                    };

                    if *bit_index < 0.into() || *bit_index >= (bytes.len() * 8).into() {
                        return Err(Error::WriteBitsOutOfBounds);
                    }

                    let (byte_index, bit_offset) = bit_index.div_rem(&8.into());

                    let bit_offset = usize::try_from(bit_offset).unwrap();

                    let flipped_index = bytes.len() - 1 - usize::try_from(byte_index).unwrap();

                    let bit_mask: u8 = 1 << bit_offset;

                    if *set_bit {
                        bytes[flipped_index] |= bit_mask;
                    } else {
                        bytes[flipped_index] &= !bit_mask;
                    }
                }

                Ok(Value::byte_string(bytes))
            }
            DefaultFunction::ReplicateByte => {
                let size = args[0].unwrap_integer()?;
                let byte = args[1].unwrap_integer()?;

                // Safe since this is checked by cost model
                let size = usize::try_from(size).unwrap();

                let Ok(byte) = u8::try_from(byte) else {
                    return Err(Error::OutsideByteBounds(byte.clone()));
                };

                let value = if size == 0 {
                    Value::byte_string(vec![])
                } else {
                    Value::byte_string([byte].repeat(size))
                };

                Ok(value)
            }
            DefaultFunction::ShiftByteString => {
                let bytes = args[0].unwrap_byte_string()?;
                let shift = args[1].unwrap_integer()?;

                let byte_length = bytes.len();

                if BigInt::from_usize(byte_length).unwrap() * 8 <= shift.abs() {
                    let new_vec = vec![0; byte_length];

                    return Ok(Value::byte_string(new_vec));
                }

                let is_shl = shift >= &0.into();

                let mut bv = BitVec::<u8, Msb0>::from_vec(bytes.clone());

                if is_shl {
                    bv.shift_left(usize::try_from(shift.abs()).unwrap());
                } else {
                    bv.shift_right(usize::try_from(shift.abs()).unwrap());
                }

                Ok(Value::byte_string(bv.into_vec()))
            }
            DefaultFunction::RotateByteString => {
                let bytes = args[0].unwrap_byte_string()?;
                let shift = args[1].unwrap_integer()?;

                let byte_length = bytes.len();

                if bytes.is_empty() {
                    return Ok(Value::byte_string(bytes.clone()));
                }

                let shift = shift.mod_floor(&(byte_length * 8).into());

                let mut bv = BitVec::<u8, Msb0>::from_vec(bytes.clone());

                bv.rotate_left(usize::try_from(shift).unwrap());

                Ok(Value::byte_string(bv.into_vec()))
            }
            DefaultFunction::CountSetBits => {
                let bytes = args[0].unwrap_byte_string()?;

                Ok(Value::integer(hamming::weight(bytes).into()))
            }
            DefaultFunction::FindFirstSetBit => {
                let bytes = args[0].unwrap_byte_string()?;

                let first_bit = bytes
                    .iter()
                    .rev()
                    .enumerate()
                    .find_map(|(byte_index, value)| {
                        let value = value.reverse_bits();

                        let first_bit: Option<usize> = if value >= 128 {
                            Some(0)
                        } else if value >= 64 {
                            Some(1)
                        } else if value >= 32 {
                            Some(2)
                        } else if value >= 16 {
                            Some(3)
                        } else if value >= 8 {
                            Some(4)
                        } else if value >= 4 {
                            Some(5)
                        } else if value >= 2 {
                            Some(6)
                        } else if value >= 1 {
                            Some(7)
                        } else {
                            None
                        };

                        first_bit.map(|bit| isize::try_from(bit + byte_index * 8).unwrap())
                    });

                Ok(Value::integer(first_bit.unwrap_or(-1).into()))
            }
            DefaultFunction::Ripemd_160 => {
                use cryptoxide::{digest::Digest, ripemd160::Ripemd160};

                let arg1 = args[0].unwrap_byte_string()?;

                let mut hasher = Ripemd160::new();

                hasher.input(arg1);

                let mut bytes = vec![0; hasher.output_bytes()];

                hasher.result(&mut bytes);

                let value = Value::byte_string(bytes);

                Ok(value)
            } // DefaultFunction::ExpModInteger => todo!(),
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
