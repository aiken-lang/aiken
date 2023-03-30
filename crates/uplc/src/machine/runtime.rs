use bumpalo::{collections::Vec as BumpVec, Bump};
use std::{ops::Deref, rc::Rc};

use num_integer::Integer;
use pallas_primitives::babbage::{Constr, PlutusData};

use crate::{
    ast::{Constant, Type},
    builtins::DefaultFunction,
    plutus_data_to_bytes,
};

use super::{
    cost_model::{BuiltinCosts, ExBudget},
    from_pallas_bigint, to_pallas_bigint, Error, Value,
};

//#[derive(std::cmp::PartialEq)]
//pub enum EvalMode {
//    Immediate,
//    Deferred,
//}

#[derive(Clone, Debug)]
pub struct BuiltinRuntime<'a> {
    args: Vec<&'a Value<'a>>,
    fun: DefaultFunction,
    forces: u32,
}

impl<'a> BuiltinRuntime<'a> {
    pub fn new(fun: DefaultFunction) -> BuiltinRuntime<'a> {
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

    pub fn call(
        &self,
        arena: &'a Bump,
        logs: &'a mut BumpVec<'a, String>,
    ) -> Result<&'a Value, Error> {
        self.fun.call(arena, &self.args, logs)
    }

    pub fn push(&mut self, arg: Rc<Value>) -> Result<(), Error> {
        self.fun.check_type(&arg, &self.args)?;

        self.args.push(arg);

        Ok(())
    }

    pub fn to_ex_budget_v2(&self, costs: &BuiltinCosts) -> ExBudget {
        costs.to_ex_budget_v2(self.fun, &self.args)
    }

    pub fn to_ex_budget_v1(&self, costs: &BuiltinCosts) -> ExBudget {
        costs.to_ex_budget_v1(self.fun, &self.args)
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
            DefaultFunction::Blake2b_256 => 1,
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
            DefaultFunction::Blake2b_256 => 0,
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
        }
    }

    pub fn check_type(&self, arg: &Value, args: &[Rc<Value>]) -> Result<(), Error> {
        match self {
            DefaultFunction::AddInteger => arg.expect_type(Type::Integer),
            DefaultFunction::SubtractInteger => arg.expect_type(Type::Integer),
            DefaultFunction::MultiplyInteger => arg.expect_type(Type::Integer),
            DefaultFunction::DivideInteger => arg.expect_type(Type::Integer),
            DefaultFunction::QuotientInteger => arg.expect_type(Type::Integer),
            DefaultFunction::RemainderInteger => arg.expect_type(Type::Integer),
            DefaultFunction::ModInteger => arg.expect_type(Type::Integer),
            DefaultFunction::EqualsInteger => arg.expect_type(Type::Integer),
            DefaultFunction::LessThanInteger => arg.expect_type(Type::Integer),
            DefaultFunction::LessThanEqualsInteger => arg.expect_type(Type::Integer),
            DefaultFunction::AppendByteString => arg.expect_type(Type::ByteString),
            DefaultFunction::ConsByteString => {
                if args.is_empty() {
                    arg.expect_type(Type::Integer)
                } else {
                    arg.expect_type(Type::ByteString)
                }
            }
            DefaultFunction::SliceByteString => {
                if args.len() < 2 {
                    arg.expect_type(Type::Integer)
                } else {
                    arg.expect_type(Type::ByteString)
                }
            }
            DefaultFunction::LengthOfByteString => arg.expect_type(Type::ByteString),
            DefaultFunction::IndexByteString => {
                if args.is_empty() {
                    arg.expect_type(Type::ByteString)
                } else {
                    arg.expect_type(Type::Integer)
                }
            }
            DefaultFunction::EqualsByteString => arg.expect_type(Type::ByteString),
            DefaultFunction::LessThanByteString => arg.expect_type(Type::ByteString),
            DefaultFunction::LessThanEqualsByteString => arg.expect_type(Type::ByteString),
            DefaultFunction::Sha2_256 => arg.expect_type(Type::ByteString),
            DefaultFunction::Sha3_256 => arg.expect_type(Type::ByteString),
            DefaultFunction::Blake2b_256 => arg.expect_type(Type::ByteString),
            DefaultFunction::VerifyEd25519Signature => arg.expect_type(Type::ByteString),
            DefaultFunction::VerifyEcdsaSecp256k1Signature => arg.expect_type(Type::ByteString),
            DefaultFunction::VerifySchnorrSecp256k1Signature => arg.expect_type(Type::ByteString),
            DefaultFunction::AppendString => arg.expect_type(Type::String),
            DefaultFunction::EqualsString => arg.expect_type(Type::String),
            DefaultFunction::EncodeUtf8 => arg.expect_type(Type::String),
            DefaultFunction::DecodeUtf8 => arg.expect_type(Type::ByteString),
            DefaultFunction::IfThenElse => {
                if args.is_empty() {
                    arg.expect_type(Type::Bool)
                } else {
                    Ok(())
                }
            }
            DefaultFunction::ChooseUnit => {
                if args.is_empty() {
                    arg.expect_type(Type::Unit)
                } else {
                    Ok(())
                }
            }
            DefaultFunction::Trace => {
                if args.is_empty() {
                    arg.expect_type(Type::String)
                } else {
                    Ok(())
                }
            }
            DefaultFunction::FstPair => arg.expect_pair(),
            DefaultFunction::SndPair => arg.expect_pair(),
            DefaultFunction::ChooseList => {
                if args.is_empty() {
                    arg.expect_list()
                } else {
                    Ok(())
                }
            }
            DefaultFunction::MkCons => {
                if args.is_empty() {
                    Ok(())
                } else {
                    let first = args[0].as_ref();

                    arg.expect_type(Type::List(Rc::new(first.try_into()?)))
                }
            }
            DefaultFunction::HeadList => arg.expect_list(),
            DefaultFunction::TailList => arg.expect_list(),
            DefaultFunction::NullList => arg.expect_list(),
            DefaultFunction::ChooseData => {
                if args.is_empty() {
                    arg.expect_type(Type::Data)
                } else {
                    Ok(())
                }
            }
            DefaultFunction::ConstrData => {
                if args.is_empty() {
                    arg.expect_type(Type::Integer)
                } else {
                    arg.expect_type(Type::List(Rc::new(Type::Data)))
                }
            }
            DefaultFunction::MapData => arg.expect_type(Type::List(Rc::new(Type::Pair(
                Rc::new(Type::Data),
                Rc::new(Type::Data),
            )))),
            DefaultFunction::ListData => arg.expect_type(Type::List(Rc::new(Type::Data))),
            DefaultFunction::IData => arg.expect_type(Type::Integer),
            DefaultFunction::BData => arg.expect_type(Type::ByteString),
            DefaultFunction::UnConstrData => arg.expect_type(Type::Data),
            DefaultFunction::UnMapData => arg.expect_type(Type::Data),
            DefaultFunction::UnListData => arg.expect_type(Type::Data),
            DefaultFunction::UnIData => arg.expect_type(Type::Data),
            DefaultFunction::UnBData => arg.expect_type(Type::Data),
            DefaultFunction::EqualsData => arg.expect_type(Type::Data),
            DefaultFunction::SerialiseData => arg.expect_type(Type::Data),
            DefaultFunction::MkPairData => arg.expect_type(Type::Data),
            DefaultFunction::MkNilData => arg.expect_type(Type::Unit),
            DefaultFunction::MkNilPairData => arg.expect_type(Type::Unit),
        }
    }

    // This should be safe because we've already checked
    // the types of the args as they were pushed. Although
    // the unreachables look ugly, it's the reality of the situation.
    pub fn call<'a>(
        &self,
        arena: &'a Bump,
        args: &[&'a Value],
        logs: &'a mut BumpVec<'a, String>,
    ) -> Result<&'a Value, Error> {
        match self {
            DefaultFunction::AddInteger => match (args[0], args[1]) {
                (Value::Con(Constant::Integer(arg1)), Value::Con(Constant::Integer(arg2))) => {
                    let result = arg1 + arg2;

                    let constant = arena.alloc(Constant::Integer(result));

                    let value = arena.alloc(Value::Con(constant));

                    Ok(value)
                }
                _ => unreachable!(),
            },
            DefaultFunction::SubtractInteger => match (args[0].as_ref(), args[1].as_ref()) {
                (Value::Con(integer1), Value::Con(integer2)) => {
                    match (integer1.as_ref(), integer2.as_ref()) {
                        (Constant::Integer(arg1), Constant::Integer(arg2)) => {
                            let result = arg1 - arg2;

                            Ok(Value::Con(Constant::Integer(result).into()).into())
                        }
                        _ => unreachable!(),
                    }
                }
                _ => unreachable!(),
            },
            DefaultFunction::MultiplyInteger => match (args[0].as_ref(), args[1].as_ref()) {
                (Value::Con(integer1), Value::Con(integer2)) => {
                    match (integer1.as_ref(), integer2.as_ref()) {
                        (Constant::Integer(arg1), Constant::Integer(arg2)) => {
                            let result = arg1 * arg2;

                            Ok(Value::Con(Constant::Integer(result).into()).into())
                        }
                        _ => unreachable!(),
                    }
                }
                _ => unreachable!(),
            },
            DefaultFunction::DivideInteger => match (args[0].as_ref(), args[1].as_ref()) {
                (Value::Con(integer1), Value::Con(integer2)) => {
                    match (integer1.as_ref(), integer2.as_ref()) {
                        (Constant::Integer(arg1), Constant::Integer(arg2)) => {
                            if *arg2 != 0.into() {
                                let (result, _) = arg1.div_mod_floor(arg2);

                                Ok(Value::Con(Constant::Integer(result).into()).into())
                            } else {
                                Err(Error::DivideByZero(arg1.clone(), arg2.clone()))
                            }
                        }
                        _ => unreachable!(),
                    }
                }
                _ => unreachable!(),
            },
            DefaultFunction::QuotientInteger => match (args[0].as_ref(), args[1].as_ref()) {
                (Value::Con(integer1), Value::Con(integer2)) => {
                    match (integer1.as_ref(), integer2.as_ref()) {
                        (Constant::Integer(arg1), Constant::Integer(arg2)) => {
                            if *arg2 != 0.into() {
                                let (result, _) = arg1.div_rem(arg2);

                                Ok(Value::Con(Constant::Integer(result).into()).into())
                            } else {
                                Err(Error::DivideByZero(arg1.clone(), arg2.clone()))
                            }
                        }
                        _ => unreachable!(),
                    }
                }
                _ => unreachable!(),
            },
            DefaultFunction::RemainderInteger => match (args[0].as_ref(), args[1].as_ref()) {
                (Value::Con(integer1), Value::Con(integer2)) => {
                    match (integer1.as_ref(), integer2.as_ref()) {
                        (Constant::Integer(arg1), Constant::Integer(arg2)) => {
                            if *arg2 != 0.into() {
                                let (_, result) = arg1.div_rem(arg2);

                                Ok(Value::Con(Constant::Integer(result).into()).into())
                            } else {
                                Err(Error::DivideByZero(arg1.clone(), arg2.clone()))
                            }
                        }
                        _ => unreachable!(),
                    }
                }
                _ => unreachable!(),
            },
            DefaultFunction::ModInteger => match (args[0].as_ref(), args[1].as_ref()) {
                (Value::Con(integer1), Value::Con(integer2)) => {
                    match (integer1.as_ref(), integer2.as_ref()) {
                        (Constant::Integer(arg1), Constant::Integer(arg2)) => {
                            if *arg2 != 0.into() {
                                let (_, result) = arg1.div_mod_floor(arg2);

                                Ok(Value::Con(Constant::Integer(result).into()).into())
                            } else {
                                Err(Error::DivideByZero(arg1.clone(), arg2.clone()))
                            }
                        }
                        _ => unreachable!(),
                    }
                }
                _ => unreachable!(),
            },
            DefaultFunction::EqualsInteger => match (args[0].as_ref(), args[1].as_ref()) {
                (Value::Con(integer1), Value::Con(integer2)) => {
                    match (integer1.as_ref(), integer2.as_ref()) {
                        (Constant::Integer(arg1), Constant::Integer(arg2)) => {
                            Ok(Value::Con(Constant::Bool(arg1 == arg2).into()).into())
                        }
                        _ => unreachable!(),
                    }
                }
                _ => unreachable!(),
            },
            DefaultFunction::LessThanInteger => match (args[0].as_ref(), args[1].as_ref()) {
                (Value::Con(integer1), Value::Con(integer2)) => {
                    match (integer1.as_ref(), integer2.as_ref()) {
                        (Constant::Integer(arg1), Constant::Integer(arg2)) => {
                            Ok(Value::Con(Constant::Bool(arg1 < arg2).into()).into())
                        }
                        _ => unreachable!(),
                    }
                }
                _ => unreachable!(),
            },
            DefaultFunction::LessThanEqualsInteger => match (args[0].as_ref(), args[1].as_ref()) {
                (Value::Con(integer1), Value::Con(integer2)) => {
                    match (integer1.as_ref(), integer2.as_ref()) {
                        (Constant::Integer(arg1), Constant::Integer(arg2)) => {
                            Ok(Value::Con(Constant::Bool(arg1 <= arg2).into()).into())
                        }
                        _ => unreachable!(),
                    }
                }
                _ => unreachable!(),
            },
            DefaultFunction::AppendByteString => match (args[0].as_ref(), args[1].as_ref()) {
                (Value::Con(byte_string1), Value::Con(byte_string2)) => {
                    match (byte_string1.as_ref(), byte_string2.as_ref()) {
                        (Constant::ByteString(arg1), Constant::ByteString(arg2)) => Ok(Value::Con(
                            Constant::ByteString(
                                arg1.iter().copied().chain(arg2.iter().copied()).collect(),
                            )
                            .into(),
                        )
                        .into()),
                        _ => unreachable!(),
                    }
                }
                _ => unreachable!(),
            },
            DefaultFunction::ConsByteString => match (args[0].as_ref(), args[1].as_ref()) {
                (Value::Con(integer), Value::Con(byte_string)) => {
                    match (integer.as_ref(), byte_string.as_ref()) {
                        (Constant::Integer(arg1), Constant::ByteString(arg2)) => {
                            let wrap = arg1.mod_floor(&256.into());

                            let byte: u8 = wrap.try_into().unwrap();

                            let mut ret = vec![byte];

                            ret.extend(arg2.clone());

                            Ok(Value::Con(Constant::ByteString(ret).into()).into())
                        }
                        _ => unreachable!(),
                    }
                }
                _ => unreachable!(),
            },
            DefaultFunction::SliceByteString => {
                match (args[0].as_ref(), args[1].as_ref(), args[2].as_ref()) {
                    (Value::Con(integer1), Value::Con(integer2), Value::Con(byte_string)) => {
                        match (integer1.as_ref(), integer2.as_ref(), byte_string.as_ref()) {
                            (
                                Constant::Integer(arg1),
                                Constant::Integer(arg2),
                                Constant::ByteString(arg3),
                            ) => {
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

                                let ret: Vec<u8> =
                                    arg3.iter().skip(skip).take(take).cloned().collect();

                                Ok(Value::Con(Constant::ByteString(ret).into()).into())
                            }
                            _ => unreachable!(),
                        }
                    }
                    _ => unreachable!(),
                }
            }
            DefaultFunction::LengthOfByteString => match args[0].as_ref() {
                Value::Con(byte_string) => match byte_string.as_ref() {
                    Constant::ByteString(arg1) => {
                        Ok(Value::Con(Constant::Integer(arg1.len().into()).into()).into())
                    }
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            },
            DefaultFunction::IndexByteString => match (args[0].as_ref(), args[1].as_ref()) {
                (Value::Con(byte_string), Value::Con(integer)) => {
                    match (byte_string.as_ref(), integer.as_ref()) {
                        (Constant::ByteString(arg1), Constant::Integer(arg2)) => {
                            let index: i128 = arg2.try_into().unwrap();

                            if 0 <= index && index < arg1.len() as i128 {
                                let ret = arg1[index as usize];

                                Ok(Value::Con(Constant::Integer(ret.into()).into()).into())
                            } else {
                                Err(Error::ByteStringOutOfBounds(arg2.clone(), arg1.to_vec()))
                            }
                        }
                        _ => unreachable!(),
                    }
                }
                _ => unreachable!(),
            },
            DefaultFunction::EqualsByteString => match (args[0].as_ref(), args[1].as_ref()) {
                (Value::Con(byte_string1), Value::Con(byte_string2)) => {
                    match (byte_string1.as_ref(), byte_string2.as_ref()) {
                        (Constant::ByteString(arg1), Constant::ByteString(arg2)) => {
                            Ok(Value::Con(Constant::Bool(arg1 == arg2).into()).into())
                        }
                        _ => unreachable!(),
                    }
                }
                _ => unreachable!(),
            },
            DefaultFunction::LessThanByteString => match (args[0].as_ref(), args[1].as_ref()) {
                (Value::Con(byte_string1), Value::Con(byte_string2)) => {
                    match (byte_string1.as_ref(), byte_string2.as_ref()) {
                        (Constant::ByteString(arg1), Constant::ByteString(arg2)) => {
                            Ok(Value::Con(Constant::Bool(arg1 < arg2).into()).into())
                        }
                        _ => unreachable!(),
                    }
                }
                _ => unreachable!(),
            },
            DefaultFunction::LessThanEqualsByteString => match (args[0].as_ref(), args[1].as_ref())
            {
                (Value::Con(byte_string1), Value::Con(byte_string2)) => {
                    match (byte_string1.as_ref(), byte_string2.as_ref()) {
                        (Constant::ByteString(arg1), Constant::ByteString(arg2)) => {
                            Ok(Value::Con(Constant::Bool(arg1 <= arg2).into()).into())
                        }
                        _ => unreachable!(),
                    }
                }
                _ => unreachable!(),
            },
            DefaultFunction::Sha2_256 => match args[0].as_ref() {
                Value::Con(byte_string) => match byte_string.as_ref() {
                    Constant::ByteString(arg1) => {
                        use cryptoxide::{digest::Digest, sha2::Sha256};

                        let mut hasher = Sha256::new();

                        hasher.input(arg1);

                        let mut bytes = vec![0; hasher.output_bytes()];

                        hasher.result(&mut bytes);

                        Ok(Value::Con(Constant::ByteString(bytes).into()).into())
                    }
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            },
            DefaultFunction::Sha3_256 => match args[0].as_ref() {
                Value::Con(byte_string) => match byte_string.as_ref() {
                    Constant::ByteString(arg1) => {
                        use cryptoxide::{digest::Digest, sha3::Sha3_256};

                        let mut hasher = Sha3_256::new();

                        hasher.input(arg1);

                        let mut bytes = vec![0; hasher.output_bytes()];

                        hasher.result(&mut bytes);

                        Ok(Value::Con(Constant::ByteString(bytes).into()).into())
                    }
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            },
            DefaultFunction::Blake2b_256 => match args[0].as_ref() {
                Value::Con(byte_string) => match byte_string.as_ref() {
                    Constant::ByteString(arg1) => {
                        use cryptoxide::{blake2b::Blake2b, digest::Digest};

                        let mut digest = [0u8; 32];
                        let mut context = Blake2b::new(32);

                        context.input(arg1);
                        context.result(&mut digest);

                        Ok(Value::Con(Constant::ByteString(digest.to_vec()).into()).into())
                    }
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            },
            DefaultFunction::VerifyEd25519Signature => {
                match (args[0].as_ref(), args[1].as_ref(), args[2].as_ref()) {
                    (Value::Con(public_key), Value::Con(message), Value::Con(signature)) => {
                        match (public_key.as_ref(), message.as_ref(), signature.as_ref()) {
                            (
                                Constant::ByteString(public_key),
                                Constant::ByteString(message),
                                Constant::ByteString(signature),
                            ) => {
                                use cryptoxide::ed25519;

                                let public_key: [u8; 32] =
                                    public_key.clone().try_into().map_err(|e: Vec<u8>| {
                                        Error::UnexpectedEd25519PublicKeyLength(e.len())
                                    })?;

                                let signature: [u8; 64] =
                                    signature.clone().try_into().map_err(|e: Vec<u8>| {
                                        Error::UnexpectedEd25519SignatureLength(e.len())
                                    })?;

                                let valid = ed25519::verify(message, &public_key, &signature);

                                Ok(Value::Con(Constant::Bool(valid).into()).into())
                            }
                            _ => unreachable!(),
                        }
                    }
                    _ => unreachable!(),
                }
            }
            DefaultFunction::VerifyEcdsaSecp256k1Signature => {
                match (args[0].as_ref(), args[1].as_ref(), args[2].as_ref()) {
                    (Value::Con(public_key), Value::Con(message), Value::Con(signature)) => {
                        match (public_key.as_ref(), message.as_ref(), signature.as_ref()) {
                            (
                                Constant::ByteString(public_key),
                                Constant::ByteString(message),
                                Constant::ByteString(signature),
                            ) => verify_ecdsa(public_key, message, signature),
                            _ => unreachable!(),
                        }
                    }
                    _ => unreachable!(),
                }
            }
            DefaultFunction::VerifySchnorrSecp256k1Signature => {
                match (args[0].as_ref(), args[1].as_ref(), args[2].as_ref()) {
                    (Value::Con(public_key), Value::Con(message), Value::Con(signature)) => {
                        match (public_key.as_ref(), message.as_ref(), signature.as_ref()) {
                            (
                                Constant::ByteString(public_key),
                                Constant::ByteString(message),
                                Constant::ByteString(signature),
                            ) => verify_schnorr(public_key, message, signature),
                            _ => unreachable!(),
                        }
                    }
                    _ => unreachable!(),
                }
            }
            DefaultFunction::AppendString => match (args[0].as_ref(), args[1].as_ref()) {
                (Value::Con(string1), Value::Con(string2)) => {
                    match (string1.as_ref(), string2.as_ref()) {
                        (Constant::String(arg1), Constant::String(arg2)) => {
                            Ok(Value::Con(Constant::String(format!("{arg1}{arg2}")).into()).into())
                        }
                        _ => unreachable!(),
                    }
                }
                _ => unreachable!(),
            },
            DefaultFunction::EqualsString => match (args[0].as_ref(), args[1].as_ref()) {
                (Value::Con(string1), Value::Con(string2)) => {
                    match (string1.as_ref(), string2.as_ref()) {
                        (Constant::String(arg1), Constant::String(arg2)) => {
                            Ok(Value::Con(Constant::Bool(arg1 == arg2).into()).into())
                        }
                        _ => unreachable!(),
                    }
                }
                _ => unreachable!(),
            },
            DefaultFunction::EncodeUtf8 => match args[0].as_ref() {
                Value::Con(string) => match string.as_ref() {
                    Constant::String(arg1) => {
                        let bytes = arg1.as_bytes().to_vec();

                        Ok(Value::Con(Constant::ByteString(bytes).into()).into())
                    }
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            },
            DefaultFunction::DecodeUtf8 => match args[0].as_ref() {
                Value::Con(byte_string) => match byte_string.as_ref() {
                    Constant::ByteString(arg1) => {
                        let string = String::from_utf8(arg1.clone())?;

                        Ok(Value::Con(Constant::String(string).into()).into())
                    }
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            },
            DefaultFunction::IfThenElse => match args[0].as_ref() {
                Value::Con(boolean) => match boolean.as_ref() {
                    Constant::Bool(condition) => {
                        if *condition {
                            Ok(args[1].clone())
                        } else {
                            Ok(args[2].clone())
                        }
                    }
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            },
            DefaultFunction::ChooseUnit => match args[0].as_ref() {
                Value::Con(unit) => match unit.as_ref() {
                    Constant::Unit => Ok(args[1].clone()),
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            },
            DefaultFunction::Trace => match args[0].as_ref() {
                Value::Con(string) => match string.as_ref() {
                    Constant::String(arg1) => {
                        logs.push(arg1.clone());

                        Ok(args[1].clone())
                    }
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            },
            DefaultFunction::FstPair => match args[0].as_ref() {
                Value::Con(pair) => match pair.as_ref() {
                    Constant::ProtoPair(_, _, first, _) => Ok(Value::Con(first.clone()).into()),
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            },
            DefaultFunction::SndPair => match args[0].as_ref() {
                Value::Con(pair) => match pair.as_ref() {
                    Constant::ProtoPair(_, _, _, second) => Ok(Value::Con(second.clone()).into()),
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            },
            DefaultFunction::ChooseList => match args[0].as_ref() {
                Value::Con(list) => match list.as_ref() {
                    Constant::ProtoList(_, list) => {
                        if list.is_empty() {
                            Ok(args[1].clone())
                        } else {
                            Ok(args[2].clone())
                        }
                    }
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            },
            DefaultFunction::MkCons => match (args[0].as_ref(), args[1].as_ref()) {
                (Value::Con(item), Value::Con(list)) => match list.as_ref() {
                    Constant::ProtoList(r#type, list) => {
                        let mut ret = vec![item.as_ref().clone()];
                        ret.extend(list.clone());

                        Ok(Value::Con(Constant::ProtoList(r#type.clone(), ret).into()).into())
                    }
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            },
            DefaultFunction::HeadList => match args[0].as_ref() {
                c @ Value::Con(list) => match list.as_ref() {
                    Constant::ProtoList(_, list) => {
                        if list.is_empty() {
                            Err(Error::EmptyList(c.clone()))
                        } else {
                            Ok(Value::Con(list[0].clone().into()).into())
                        }
                    }
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            },
            DefaultFunction::TailList => match args[0].as_ref() {
                c @ Value::Con(list) => match list.as_ref() {
                    Constant::ProtoList(r#type, list) => {
                        if list.is_empty() {
                            Err(Error::EmptyList(c.clone()))
                        } else {
                            Ok(Value::Con(
                                Constant::ProtoList(r#type.clone(), list[1..].to_vec()).into(),
                            )
                            .into())
                        }
                    }
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            },
            DefaultFunction::NullList => match args[0].as_ref() {
                Value::Con(list) => match list.as_ref() {
                    Constant::ProtoList(_, list) => {
                        Ok(Value::Con(Constant::Bool(list.is_empty()).into()).into())
                    }
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            },
            DefaultFunction::ChooseData => match args[0].as_ref() {
                Value::Con(con) => match con.as_ref() {
                    Constant::Data(PlutusData::Constr(_)) => Ok(args[1].clone()),
                    Constant::Data(PlutusData::Map(_)) => Ok(args[2].clone()),
                    Constant::Data(PlutusData::Array(_)) => Ok(args[3].clone()),
                    Constant::Data(PlutusData::BigInt(_)) => Ok(args[4].clone()),
                    Constant::Data(PlutusData::BoundedBytes(_)) => Ok(args[5].clone()),
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            },
            DefaultFunction::ConstrData => match (args[0].as_ref(), args[1].as_ref()) {
                (Value::Con(integer), Value::Con(list)) => {
                    match (integer.as_ref(), list.as_ref()) {
                        (Constant::Integer(i), Constant::ProtoList(Type::Data, l)) => {
                            let data_list: Vec<PlutusData> = l
                                .iter()
                                .map(|item| match item {
                                    Constant::Data(d) => d.clone(),
                                    _ => unreachable!(),
                                })
                                .collect();

                            let i: u64 = i.try_into().unwrap();

                            let constr_data = PlutusData::Constr(Constr {
                                tag: convert_constr_to_tag(i).unwrap_or(ANY_TAG),
                                any_constructor: convert_constr_to_tag(i).map_or(Some(i), |_| None),
                                fields: data_list,
                            });

                            Ok(Value::Con(Constant::Data(constr_data).into()).into())
                        }
                        _ => unreachable!(),
                    }
                }
                _ => unreachable!(),
            },
            DefaultFunction::MapData => match args[0].as_ref() {
                Value::Con(list) => match list.as_ref() {
                    Constant::ProtoList(_, list) => {
                        let mut map = Vec::new();

                        for item in list {
                            match item {
                                Constant::ProtoPair(Type::Data, Type::Data, left, right) => {
                                    match (left.as_ref(), right.as_ref()) {
                                        (Constant::Data(key), Constant::Data(value)) => {
                                            map.push((key.clone(), value.clone()));
                                        }
                                        _ => unreachable!(),
                                    }
                                }
                                _ => unreachable!(),
                            }
                        }

                        Ok(Value::Con(Constant::Data(PlutusData::Map(map.into())).into()).into())
                    }
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            },
            DefaultFunction::ListData => match args[0].as_ref() {
                Value::Con(list) => match list.as_ref() {
                    Constant::ProtoList(_, list) => {
                        let data_list: Vec<PlutusData> = list
                            .iter()
                            .map(|item| match item {
                                Constant::Data(d) => d.clone(),
                                _ => unreachable!(),
                            })
                            .collect();

                        Ok(Value::Con(Constant::Data(PlutusData::Array(data_list)).into()).into())
                    }
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            },
            DefaultFunction::IData => match args[0].as_ref() {
                Value::Con(integer) => match integer.as_ref() {
                    Constant::Integer(i) => Ok(Value::Con(
                        Constant::Data(PlutusData::BigInt(to_pallas_bigint(i))).into(),
                    )
                    .into()),
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            },
            DefaultFunction::BData => match args[0].as_ref() {
                Value::Con(byte_string) => match byte_string.as_ref() {
                    Constant::ByteString(b) => Ok(Value::Con(
                        Constant::Data(PlutusData::BoundedBytes(b.clone().try_into().unwrap()))
                            .into(),
                    )
                    .into()),
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            },
            DefaultFunction::UnConstrData => match args[0].as_ref() {
                Value::Con(con) => match con.as_ref() {
                    Constant::Data(PlutusData::Constr(c)) => Ok(Value::Con(
                        Constant::ProtoPair(
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
                        )
                        .into(),
                    )
                    .into()),
                    v => Err(Error::DeserialisationError(
                        "UnConstrData".to_string(),
                        Value::Con(v.clone().into()),
                    )),
                },
                v => Err(Error::DeserialisationError(
                    "UnConstrData".to_string(),
                    v.clone(),
                )),
            },
            DefaultFunction::UnMapData => match args[0].as_ref() {
                Value::Con(data) => match data.as_ref() {
                    Constant::Data(PlutusData::Map(m)) => Ok(Value::Con(
                        Constant::ProtoList(
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
                        )
                        .into(),
                    )
                    .into()),
                    v => Err(Error::DeserialisationError(
                        "UnMapData".to_string(),
                        Value::Con(v.clone().into()),
                    )),
                },
                v => Err(Error::DeserialisationError(
                    "UnMapData".to_string(),
                    v.clone(),
                )),
            },
            DefaultFunction::UnListData => match args[0].as_ref() {
                Value::Con(data) => match data.as_ref() {
                    Constant::Data(PlutusData::Array(l)) => Ok(Value::Con(
                        Constant::ProtoList(
                            Type::Data,
                            l.deref()
                                .iter()
                                .map(|d| Constant::Data(d.clone()))
                                .collect(),
                        )
                        .into(),
                    )
                    .into()),
                    v => Err(Error::DeserialisationError(
                        "UnListData".to_string(),
                        Value::Con(v.clone().into()),
                    )),
                },
                v => Err(Error::DeserialisationError(
                    "UnListData".to_string(),
                    v.clone(),
                )),
            },
            DefaultFunction::UnIData => match args[0].as_ref() {
                Value::Con(data) => match data.as_ref() {
                    Constant::Data(PlutusData::BigInt(b)) => {
                        Ok(Value::Con(Constant::Integer(from_pallas_bigint(b)).into()).into())
                    }
                    v => Err(Error::DeserialisationError(
                        "UnIData".to_string(),
                        Value::Con(v.clone().into()),
                    )),
                },
                v => Err(Error::DeserialisationError(
                    "UnIData".to_string(),
                    v.clone(),
                )),
            },
            DefaultFunction::UnBData => match args[0].as_ref() {
                Value::Con(data) => match data.as_ref() {
                    Constant::Data(PlutusData::BoundedBytes(b)) => {
                        Ok(Value::Con(Constant::ByteString(b.to_vec()).into()).into())
                    }
                    v => Err(Error::DeserialisationError(
                        "UnBData".to_string(),
                        Value::Con(v.clone().into()),
                    )),
                },
                v => Err(Error::DeserialisationError(
                    "UnBData".to_string(),
                    v.clone(),
                )),
            },
            DefaultFunction::EqualsData => match (args[0].as_ref(), args[1].as_ref()) {
                (Value::Con(data1), Value::Con(data2)) => match (data1.as_ref(), data2.as_ref()) {
                    (Constant::Data(d1), Constant::Data(d2)) => {
                        Ok(Value::Con(Constant::Bool(d1.eq(d2)).into()).into())
                    }
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            },
            DefaultFunction::SerialiseData => match args[0].as_ref() {
                Value::Con(data) => match data.as_ref() {
                    Constant::Data(d) => {
                        let serialized_data = plutus_data_to_bytes(d).unwrap();
                        Ok(Value::Con(Constant::ByteString(serialized_data).into()).into())
                    }
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            },
            DefaultFunction::MkPairData => match (args[0].as_ref(), args[1].as_ref()) {
                (Value::Con(data1), Value::Con(data2)) => match (data1.as_ref(), data2.as_ref()) {
                    (Constant::Data(d1), Constant::Data(d2)) => Ok(Value::Con(
                        Constant::ProtoPair(
                            Type::Data,
                            Type::Data,
                            Constant::Data(d1.clone()).into(),
                            Constant::Data(d2.clone()).into(),
                        )
                        .into(),
                    )
                    .into()),
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            },
            DefaultFunction::MkNilData => {
                Ok(Value::Con(Constant::ProtoList(Type::Data, vec![]).into()).into())
            }
            DefaultFunction::MkNilPairData => Ok(Value::Con(
                Constant::ProtoList(Type::Pair(Rc::new(Type::Data), Rc::new(Type::Data)), vec![])
                    .into(),
            )
            .into()),
        }
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

#[cfg(not(feature = "native-secp256k1"))]
fn verify_ecdsa(public_key: &[u8], message: &[u8], signature: &[u8]) -> Result<Rc<Value>, Error> {
    use secp256k1::{ecdsa::Signature, Message, PublicKey, Secp256k1};

    let secp = Secp256k1::verification_only();

    let public_key = PublicKey::from_slice(public_key)?;

    let signature = Signature::from_compact(signature)?;

    let message = Message::from_slice(message)?;

    let valid = secp.verify_ecdsa(&message, &signature, &public_key);

    Ok(Value::Con(Constant::Bool(valid.is_ok()).into()).into())
}

/// Unlike the Haskell implementation the schnorr verification function in Aiken doesn't allow for arbitrary message sizes (at the moment).
/// The message needs to be 32 bytes (ideally prehashed, but not a requirement).
#[cfg(not(feature = "native-secp256k1"))]
fn verify_schnorr(public_key: &[u8], message: &[u8], signature: &[u8]) -> Result<Rc<Value>, Error> {
    use secp256k1::{schnorr::Signature, Message, Secp256k1, XOnlyPublicKey};

    let secp = Secp256k1::verification_only();

    let public_key = XOnlyPublicKey::from_slice(public_key)?;

    let signature = Signature::from_slice(signature)?;

    let message = Message::from_slice(message)?;

    let valid = secp.verify_schnorr(&signature, &message, &public_key);

    Ok(Value::Con(Constant::Bool(valid.is_ok()).into()).into())
}

#[cfg(feature = "native-secp256k1")]
fn verify_ecdsa(public_key: &[u8], message: &[u8], signature: &[u8]) -> Result<Rc<Value>, Error> {
    use k256::ecdsa::{self, signature::hazmat::PrehashVerifier};

    let verifying_key = ecdsa::VerifyingKey::try_from(public_key)?;

    let signature = ecdsa::Signature::try_from(signature)?;

    let valid = verifying_key.verify_prehash(message, &signature);

    Ok(Value::Con(Constant::Bool(valid.is_ok()).into()).into())
}

#[cfg(feature = "native-secp256k1")]
fn verify_schnorr(public_key: &[u8], message: &[u8], signature: &[u8]) -> Result<Rc<Value>, Error> {
    use k256::schnorr::{self, signature::hazmat::PrehashVerifier};

    let verifying_key = schnorr::VerifyingKey::from_bytes(public_key)?;

    let signature = schnorr::Signature::try_from(signature)?;

    let valid = verifying_key.verify_prehash(message, &signature);

    Ok(Value::Con(Constant::Bool(valid.is_ok()).into()).into())
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
