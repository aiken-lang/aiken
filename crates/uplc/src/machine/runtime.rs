use std::ops::Deref;

use pallas_primitives::babbage::PlutusData;

use crate::{
    ast::{Constant, Type},
    builtins::DefaultFunction,
};

use super::{
    cost_model::{BuiltinCosts, ExBudget},
    Error, Value,
};

//#[derive(std::cmp::PartialEq)]
//pub enum EvalMode {
//    Immediate,
//    Deferred,
//}

#[derive(Clone, Debug)]
pub struct BuiltinRuntime {
    args: Vec<Value>,
    fun: DefaultFunction,
    forces: u32,
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

    pub fn call(&self, logs: &mut Vec<String>) -> Result<Value, Error> {
        self.fun.call(&self.args, logs)
    }

    pub fn push(&mut self, arg: Value) -> Result<(), Error> {
        self.fun.check_type(&arg, &self.args)?;

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
            DefaultFunction::Blake2b_256 => 1,
            DefaultFunction::VerifyEd25519Signature => 3,
            DefaultFunction::VerifyEcdsaSecp256k1Signature => todo!(),
            DefaultFunction::VerifySchnorrSecp256k1Signature => todo!(),
            DefaultFunction::AppendString => 2,
            DefaultFunction::EqualsString => 2,
            DefaultFunction::EncodeUtf8 => 1,
            DefaultFunction::DecodeUtf8 => 1,
            DefaultFunction::IfThenElse => 3,
            DefaultFunction::ChooseUnit => 2,
            DefaultFunction::Trace => 2,
            DefaultFunction::FstPair => todo!(),
            DefaultFunction::SndPair => todo!(),
            DefaultFunction::ChooseList => todo!(),
            DefaultFunction::MkCons => 2,
            DefaultFunction::HeadList => 1,
            DefaultFunction::TailList => 1,
            DefaultFunction::NullList => 1,
            DefaultFunction::ChooseData => todo!(),
            DefaultFunction::ConstrData => todo!(),
            DefaultFunction::MapData => todo!(),
            DefaultFunction::ListData => todo!(),
            DefaultFunction::IData => todo!(),
            DefaultFunction::BData => todo!(),
            DefaultFunction::UnConstrData => 1,
            DefaultFunction::UnMapData => 1,
            DefaultFunction::UnListData => 1,
            DefaultFunction::UnIData => todo!(),
            DefaultFunction::UnBData => 1,
            DefaultFunction::EqualsData => todo!(),
            DefaultFunction::SerialiseData => todo!(),
            DefaultFunction::MkPairData => todo!(),
            DefaultFunction::MkNilData => todo!(),
            DefaultFunction::MkNilPairData => todo!(),
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
            DefaultFunction::VerifyEcdsaSecp256k1Signature => todo!(),
            DefaultFunction::VerifySchnorrSecp256k1Signature => todo!(),
            DefaultFunction::AppendString => 0,
            DefaultFunction::EqualsString => 0,
            DefaultFunction::EncodeUtf8 => 0,
            DefaultFunction::DecodeUtf8 => 0,
            DefaultFunction::IfThenElse => 1,
            DefaultFunction::ChooseUnit => 1,
            DefaultFunction::Trace => 1,
            DefaultFunction::FstPair => todo!(),
            DefaultFunction::SndPair => todo!(),
            DefaultFunction::ChooseList => todo!(),
            DefaultFunction::MkCons => 1,
            DefaultFunction::HeadList => 1,
            DefaultFunction::TailList => 1,
            DefaultFunction::NullList => 1,
            DefaultFunction::ChooseData => todo!(),
            DefaultFunction::ConstrData => todo!(),
            DefaultFunction::MapData => todo!(),
            DefaultFunction::ListData => todo!(),
            DefaultFunction::IData => todo!(),
            DefaultFunction::BData => todo!(),
            DefaultFunction::UnConstrData => 0,
            DefaultFunction::UnMapData => 0,
            DefaultFunction::UnListData => 0,
            DefaultFunction::UnIData => todo!(),
            DefaultFunction::UnBData => 0,
            DefaultFunction::EqualsData => todo!(),
            DefaultFunction::SerialiseData => todo!(),
            DefaultFunction::MkPairData => todo!(),
            DefaultFunction::MkNilData => todo!(),
            DefaultFunction::MkNilPairData => todo!(),
        }
    }

    pub fn check_type(&self, arg: &Value, args: &[Value]) -> Result<(), Error> {
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
            DefaultFunction::VerifyEcdsaSecp256k1Signature => todo!(),
            DefaultFunction::VerifySchnorrSecp256k1Signature => todo!(),
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
            DefaultFunction::FstPair => todo!(),
            DefaultFunction::SndPair => todo!(),
            DefaultFunction::ChooseList => todo!(),
            DefaultFunction::MkCons => {
                if args.is_empty() {
                    Ok(())
                } else {
                    let first = args[0].clone();

                    arg.expect_type(Type::List(Box::new(first.try_into()?)))
                }
            }
            DefaultFunction::HeadList => arg.expect_list(),
            DefaultFunction::TailList => arg.expect_list(),
            DefaultFunction::NullList => arg.expect_list(),
            DefaultFunction::ChooseData => todo!(),
            DefaultFunction::ConstrData => todo!(),
            DefaultFunction::MapData => todo!(),
            DefaultFunction::ListData => todo!(),
            DefaultFunction::IData => todo!(),
            DefaultFunction::BData => todo!(),
            DefaultFunction::UnConstrData => arg.expect_type(Type::Data),
            DefaultFunction::UnMapData => arg.expect_type(Type::Data),
            DefaultFunction::UnListData => arg.expect_type(Type::Data),
            DefaultFunction::UnIData => todo!(),
            DefaultFunction::UnBData => arg.expect_type(Type::Data),
            DefaultFunction::EqualsData => todo!(),
            DefaultFunction::SerialiseData => todo!(),
            DefaultFunction::MkPairData => todo!(),
            DefaultFunction::MkNilData => todo!(),
            DefaultFunction::MkNilPairData => todo!(),
        }
    }

    // This should be safe because we've already checked
    // the types of the args as they were pushed. Although
    // the unreachables look ugly, it's the reality of the situation.
    pub fn call(&self, args: &[Value], logs: &mut Vec<String>) -> Result<Value, Error> {
        match self {
            DefaultFunction::AddInteger => match (&args[0], &args[1]) {
                (Value::Con(Constant::Integer(arg1)), Value::Con(Constant::Integer(arg2))) => {
                    Ok(Value::Con(Constant::Integer(arg1 + arg2)))
                }
                _ => unreachable!(),
            },
            DefaultFunction::SubtractInteger => match (&args[0], &args[1]) {
                (Value::Con(Constant::Integer(arg1)), Value::Con(Constant::Integer(arg2))) => {
                    Ok(Value::Con(Constant::Integer(arg1 - arg2)))
                }
                _ => unreachable!(),
            },
            DefaultFunction::MultiplyInteger => match (&args[0], &args[1]) {
                (Value::Con(Constant::Integer(arg1)), Value::Con(Constant::Integer(arg2))) => {
                    Ok(Value::Con(Constant::Integer(arg1 * arg2)))
                }
                _ => unreachable!(),
            },
            DefaultFunction::DivideInteger => match (&args[0], &args[1]) {
                (Value::Con(Constant::Integer(arg1)), Value::Con(Constant::Integer(arg2))) => {
                    if *arg2 != 0 {
                        let ret = (*arg1 as f64) / (*arg2 as f64);

                        Ok(Value::Con(Constant::Integer(ret.floor() as isize)))
                    } else {
                        Err(Error::DivideByZero(*arg1, *arg2))
                    }
                }
                _ => unreachable!(),
            },
            DefaultFunction::QuotientInteger => match (&args[0], &args[1]) {
                (Value::Con(Constant::Integer(arg1)), Value::Con(Constant::Integer(arg2))) => {
                    if *arg2 != 0 {
                        let ret = (*arg1 as f64) / (*arg2 as f64);

                        let ret = if ret < 0. { ret.ceil() } else { ret.floor() };

                        Ok(Value::Con(Constant::Integer(ret as isize)))
                    } else {
                        Err(Error::DivideByZero(*arg1, *arg2))
                    }
                }
                _ => unreachable!(),
            },
            DefaultFunction::RemainderInteger => match (&args[0], &args[1]) {
                (Value::Con(Constant::Integer(arg1)), Value::Con(Constant::Integer(arg2))) => {
                    if *arg2 != 0 {
                        let ret = arg1 % arg2;

                        Ok(Value::Con(Constant::Integer(ret)))
                    } else {
                        Err(Error::DivideByZero(*arg1, *arg2))
                    }
                }
                _ => unreachable!(),
            },
            DefaultFunction::ModInteger => match (&args[0], &args[1]) {
                (Value::Con(Constant::Integer(arg1)), Value::Con(Constant::Integer(arg2))) => {
                    if *arg2 != 0 {
                        let ret = arg1 % arg2;

                        Ok(Value::Con(Constant::Integer(ret.abs())))
                    } else {
                        Err(Error::DivideByZero(*arg1, *arg2))
                    }
                }
                _ => unreachable!(),
            },
            DefaultFunction::EqualsInteger => match (&args[0], &args[1]) {
                (Value::Con(Constant::Integer(arg1)), Value::Con(Constant::Integer(arg2))) => {
                    Ok(Value::Con(Constant::Bool(arg1 == arg2)))
                }
                _ => unreachable!(),
            },
            DefaultFunction::LessThanInteger => match (&args[0], &args[1]) {
                (Value::Con(Constant::Integer(arg1)), Value::Con(Constant::Integer(arg2))) => {
                    Ok(Value::Con(Constant::Bool(arg1 < arg2)))
                }
                _ => unreachable!(),
            },
            DefaultFunction::LessThanEqualsInteger => match (&args[0], &args[1]) {
                (Value::Con(Constant::Integer(arg1)), Value::Con(Constant::Integer(arg2))) => {
                    Ok(Value::Con(Constant::Bool(arg1 <= arg2)))
                }
                _ => unreachable!(),
            },
            DefaultFunction::AppendByteString => match (&args[0], &args[1]) {
                (
                    Value::Con(Constant::ByteString(arg1)),
                    Value::Con(Constant::ByteString(arg2)),
                ) => Ok(Value::Con(Constant::ByteString(
                    arg1.iter().copied().chain(arg2.iter().copied()).collect(),
                ))),
                _ => unreachable!(),
            },
            DefaultFunction::ConsByteString => match (&args[0], &args[1]) {
                (Value::Con(Constant::Integer(arg1)), Value::Con(Constant::ByteString(arg2))) => {
                    let mut ret = vec![(arg1 % 256) as u8];
                    ret.extend(arg2.clone());

                    Ok(Value::Con(Constant::ByteString(ret)))
                }
                _ => unreachable!(),
            },
            DefaultFunction::SliceByteString => match (&args[0], &args[1], &args[2]) {
                (
                    Value::Con(Constant::Integer(arg1)),
                    Value::Con(Constant::Integer(arg2)),
                    Value::Con(Constant::ByteString(arg3)),
                ) => {
                    let skip = if 0 > *arg1 { 0 } else { *arg1 as usize };
                    let take = if 0 > *arg2 { 0 } else { *arg2 as usize };

                    let ret: Vec<u8> = arg3.iter().skip(skip).take(take).cloned().collect();

                    Ok(Value::Con(Constant::ByteString(ret)))
                }
                _ => unreachable!(),
            },
            DefaultFunction::LengthOfByteString => match &args[0] {
                Value::Con(Constant::ByteString(arg1)) => {
                    Ok(Value::Con(Constant::Integer(arg1.len() as isize)))
                }
                _ => unreachable!(),
            },
            DefaultFunction::IndexByteString => match (&args[0], &args[1]) {
                (Value::Con(Constant::ByteString(arg1)), Value::Con(Constant::Integer(arg2))) => {
                    let index = *arg2 as usize;

                    if 0 <= *arg2 && index < arg1.len() {
                        let ret = arg1[index] as isize;

                        Ok(Value::Con(Constant::Integer(ret)))
                    } else {
                        Err(Error::ByteStringOutOfBounds(*arg2, arg1.to_vec()))
                    }
                }
                _ => unreachable!(),
            },
            DefaultFunction::EqualsByteString => match (&args[0], &args[1]) {
                (
                    Value::Con(Constant::ByteString(arg1)),
                    Value::Con(Constant::ByteString(arg2)),
                ) => Ok(Value::Con(Constant::Bool(arg1 == arg2))),
                _ => unreachable!(),
            },
            DefaultFunction::LessThanByteString => match (&args[0], &args[1]) {
                (
                    Value::Con(Constant::ByteString(arg1)),
                    Value::Con(Constant::ByteString(arg2)),
                ) => Ok(Value::Con(Constant::Bool(arg1 < arg2))),
                _ => unreachable!(),
            },
            DefaultFunction::LessThanEqualsByteString => match (&args[0], &args[1]) {
                (
                    Value::Con(Constant::ByteString(arg1)),
                    Value::Con(Constant::ByteString(arg2)),
                ) => Ok(Value::Con(Constant::Bool(arg1 <= arg2))),
                _ => unreachable!(),
            },
            DefaultFunction::Sha2_256 => match &args[0] {
                Value::Con(Constant::ByteString(arg1)) => {
                    use cryptoxide::{digest::Digest, sha2::Sha256};

                    let mut hasher = Sha256::new();

                    hasher.input(arg1);

                    let mut bytes = vec![0; hasher.output_bytes()];

                    hasher.result(&mut bytes);

                    Ok(Value::Con(Constant::ByteString(bytes)))
                }
                _ => unreachable!(),
            },
            DefaultFunction::Sha3_256 => match &args[0] {
                Value::Con(Constant::ByteString(arg1)) => {
                    use cryptoxide::{digest::Digest, sha3::Sha3_256};

                    let mut hasher = Sha3_256::new();

                    hasher.input(arg1);

                    let mut bytes = vec![0; hasher.output_bytes()];

                    hasher.result(&mut bytes);

                    Ok(Value::Con(Constant::ByteString(bytes)))
                }
                _ => unreachable!(),
            },
            DefaultFunction::Blake2b_256 => match &args[0] {
                Value::Con(Constant::ByteString(arg1)) => {
                    use cryptoxide::{blake2b::Blake2b, digest::Digest};

                    let mut digest = [0u8; 32];
                    let mut context = Blake2b::new(32);

                    context.input(arg1);
                    context.result(&mut digest);

                    Ok(Value::Con(Constant::ByteString(digest.to_vec())))
                }
                _ => unreachable!(),
            },
            DefaultFunction::VerifyEd25519Signature => match (&args[0], &args[1], &args[2]) {
                (
                    Value::Con(Constant::ByteString(public_key)),
                    Value::Con(Constant::ByteString(message)),
                    Value::Con(Constant::ByteString(signature)),
                ) => {
                    use cryptoxide::ed25519;

                    let public_key: [u8; 32] = public_key
                        .clone()
                        .try_into()
                        .map_err(|e: Vec<u8>| Error::UnexpectedEd25519PublicKeyLength(e.len()))?;

                    let signature: [u8; 64] = signature
                        .clone()
                        .try_into()
                        .map_err(|e: Vec<u8>| Error::UnexpectedEd25519SignatureLength(e.len()))?;

                    let valid = ed25519::verify(message, &public_key, &signature);

                    Ok(Value::Con(Constant::Bool(valid)))
                }
                _ => unreachable!(),
            },
            DefaultFunction::VerifyEcdsaSecp256k1Signature => todo!(),
            DefaultFunction::VerifySchnorrSecp256k1Signature => todo!(),
            DefaultFunction::AppendString => match (&args[0], &args[1]) {
                (Value::Con(Constant::String(arg1)), Value::Con(Constant::String(arg2))) => {
                    Ok(Value::Con(Constant::String(format!("{}{}", arg1, arg2))))
                }
                _ => unreachable!(),
            },
            DefaultFunction::EqualsString => match (&args[0], &args[1]) {
                (Value::Con(Constant::String(arg1)), Value::Con(Constant::String(arg2))) => {
                    Ok(Value::Con(Constant::Bool(arg1 == arg2)))
                }
                _ => unreachable!(),
            },
            DefaultFunction::EncodeUtf8 => match &args[0] {
                Value::Con(Constant::String(arg1)) => {
                    let bytes = arg1.as_bytes().to_vec();

                    Ok(Value::Con(Constant::ByteString(bytes)))
                }
                _ => unreachable!(),
            },
            DefaultFunction::DecodeUtf8 => match &args[0] {
                Value::Con(Constant::ByteString(arg1)) => {
                    let string = String::from_utf8(arg1.clone())?;

                    Ok(Value::Con(Constant::String(string)))
                }
                _ => unreachable!(),
            },
            DefaultFunction::IfThenElse => match args[0] {
                Value::Con(Constant::Bool(condition)) => {
                    if condition {
                        Ok(args[1].clone())
                    } else {
                        Ok(args[2].clone())
                    }
                }
                _ => unreachable!(),
            },
            DefaultFunction::ChooseUnit => match &args[0] {
                Value::Con(Constant::Unit) => Ok(args[1].clone()),
                _ => unreachable!(),
            },
            DefaultFunction::Trace => match &args[0] {
                Value::Con(Constant::String(arg1)) => {
                    logs.push(arg1.clone());

                    Ok(args[1].clone())
                }
                _ => unreachable!(),
            },
            DefaultFunction::FstPair => todo!(),
            DefaultFunction::SndPair => todo!(),
            DefaultFunction::ChooseList => todo!(),
            DefaultFunction::MkCons => match (&args[0], &args[1]) {
                (Value::Con(item), Value::Con(Constant::ProtoList(r#type, list))) => {
                    let mut ret = vec![item.clone()];
                    ret.extend(list.clone());

                    Ok(Value::Con(Constant::ProtoList(r#type.clone(), ret)))
                }
                _ => unreachable!(),
            },
            DefaultFunction::HeadList => match &args[0] {
                c @ Value::Con(Constant::ProtoList(_, list)) => {
                    if list.is_empty() {
                        Err(Error::EmptyList(c.clone()))
                    } else {
                        Ok(Value::Con(list[0].clone()))
                    }
                }
                _ => unreachable!(),
            },
            DefaultFunction::TailList => match &args[0] {
                c @ Value::Con(Constant::ProtoList(r#type, list)) => {
                    if list.is_empty() {
                        Err(Error::EmptyList(c.clone()))
                    } else {
                        Ok(Value::Con(Constant::ProtoList(
                            r#type.clone(),
                            list[1..].to_vec(),
                        )))
                    }
                }
                _ => unreachable!(),
            },
            DefaultFunction::NullList => match &args[0] {
                Value::Con(Constant::ProtoList(_, list)) => {
                    Ok(Value::Con(Constant::Bool(list.is_empty())))
                }
                _ => unreachable!(),
            },
            DefaultFunction::ChooseData => todo!(),
            DefaultFunction::ConstrData => todo!(),
            DefaultFunction::MapData => todo!(),
            DefaultFunction::ListData => todo!(),
            DefaultFunction::IData => todo!(),
            DefaultFunction::BData => todo!(),
            DefaultFunction::UnConstrData => match &args[0] {
                Value::Con(Constant::Data(PlutusData::Constr(c))) => {
                    Ok(Value::Con(Constant::ProtoPair(
                        Type::Integer,
                        Type::List(Box::new(Type::Data)),
                        // TODO: handle other types of constructor tags
                        Box::new(Constant::Integer(c.tag as isize - 121)),
                        Box::new(Constant::ProtoList(
                            Type::Data,
                            c.fields
                                .deref()
                                .iter()
                                .map(|d| Constant::Data(d.clone()))
                                .collect(),
                        )),
                    )))
                }
                _ => unreachable!(),
            },
            DefaultFunction::UnMapData => match &args[0] {
                Value::Con(Constant::Data(PlutusData::Map(m))) => {
                    Ok(Value::Con(Constant::ProtoList(
                        Type::Pair(Box::new(Type::Data), Box::new(Type::Data)),
                        m.deref()
                            .iter()
                            .map(|p| -> Constant {
                                Constant::ProtoPair(
                                    Type::Data,
                                    Type::Data,
                                    Box::new(Constant::Data(p.0.clone())),
                                    Box::new(Constant::Data(p.1.clone())),
                                )
                            })
                            .collect(),
                    )))
                }
                _ => unreachable!(),
            },
            DefaultFunction::UnListData => match &args[0] {
                Value::Con(Constant::Data(PlutusData::Array(l)))
                | Value::Con(Constant::Data(PlutusData::ArrayIndef(l))) => {
                    Ok(Value::Con(Constant::ProtoList(
                        Type::Data,
                        l.deref()
                            .iter()
                            .map(|d| Constant::Data(d.clone()))
                            .collect(),
                    )))
                }
                _ => unreachable!(),
            },
            DefaultFunction::UnIData => todo!(),
            DefaultFunction::UnBData => match &args[0] {
                Value::Con(Constant::Data(PlutusData::BoundedBytes(b))) => {
                    Ok(Value::Con(Constant::ByteString(b.to_vec())))
                }
                _ => unreachable!(),
            },
            DefaultFunction::EqualsData => todo!(),
            DefaultFunction::SerialiseData => todo!(),
            DefaultFunction::MkPairData => todo!(),
            DefaultFunction::MkNilData => todo!(),
            DefaultFunction::MkNilPairData => todo!(),
        }
    }
}
