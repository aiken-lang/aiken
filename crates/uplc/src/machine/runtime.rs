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
            DefaultFunction::QuotientInteger => todo!(),
            DefaultFunction::RemainderInteger => todo!(),
            DefaultFunction::ModInteger => todo!(),
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
            DefaultFunction::VerifySignature => todo!(),
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
            DefaultFunction::MkCons => todo!(),
            DefaultFunction::HeadList => todo!(),
            DefaultFunction::TailList => todo!(),
            DefaultFunction::NullList => todo!(),
            DefaultFunction::ChooseData => todo!(),
            DefaultFunction::ConstrData => todo!(),
            DefaultFunction::MapData => todo!(),
            DefaultFunction::ListData => todo!(),
            DefaultFunction::IData => todo!(),
            DefaultFunction::BData => todo!(),
            DefaultFunction::UnConstrData => todo!(),
            DefaultFunction::UnMapData => todo!(),
            DefaultFunction::UnListData => todo!(),
            DefaultFunction::UnIData => todo!(),
            DefaultFunction::UnBData => todo!(),
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
            DefaultFunction::QuotientInteger => todo!(),
            DefaultFunction::RemainderInteger => todo!(),
            DefaultFunction::ModInteger => todo!(),
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
            DefaultFunction::VerifySignature => todo!(),
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
            DefaultFunction::MkCons => todo!(),
            DefaultFunction::HeadList => todo!(),
            DefaultFunction::TailList => todo!(),
            DefaultFunction::NullList => todo!(),
            DefaultFunction::ChooseData => todo!(),
            DefaultFunction::ConstrData => todo!(),
            DefaultFunction::MapData => todo!(),
            DefaultFunction::ListData => todo!(),
            DefaultFunction::IData => todo!(),
            DefaultFunction::BData => todo!(),
            DefaultFunction::UnConstrData => todo!(),
            DefaultFunction::UnMapData => todo!(),
            DefaultFunction::UnListData => todo!(),
            DefaultFunction::UnIData => todo!(),
            DefaultFunction::UnBData => todo!(),
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
            DefaultFunction::QuotientInteger => todo!(),
            DefaultFunction::RemainderInteger => todo!(),
            DefaultFunction::ModInteger => todo!(),
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
            DefaultFunction::VerifySignature => todo!(),
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
            DefaultFunction::MkCons => todo!(),
            DefaultFunction::HeadList => todo!(),
            DefaultFunction::TailList => todo!(),
            DefaultFunction::NullList => todo!(),
            DefaultFunction::ChooseData => todo!(),
            DefaultFunction::ConstrData => todo!(),
            DefaultFunction::MapData => todo!(),
            DefaultFunction::ListData => todo!(),
            DefaultFunction::IData => todo!(),
            DefaultFunction::BData => todo!(),
            DefaultFunction::UnConstrData => todo!(),
            DefaultFunction::UnMapData => todo!(),
            DefaultFunction::UnListData => todo!(),
            DefaultFunction::UnIData => todo!(),
            DefaultFunction::UnBData => todo!(),
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
            DefaultFunction::QuotientInteger => todo!(),
            DefaultFunction::RemainderInteger => todo!(),
            DefaultFunction::ModInteger => todo!(),
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
            DefaultFunction::VerifySignature => todo!(),
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
            DefaultFunction::MkCons => todo!(),
            DefaultFunction::HeadList => todo!(),
            DefaultFunction::TailList => todo!(),
            DefaultFunction::NullList => todo!(),
            DefaultFunction::ChooseData => todo!(),
            DefaultFunction::ConstrData => todo!(),
            DefaultFunction::MapData => todo!(),
            DefaultFunction::ListData => todo!(),
            DefaultFunction::IData => todo!(),
            DefaultFunction::BData => todo!(),
            DefaultFunction::UnConstrData => todo!(),
            DefaultFunction::UnMapData => todo!(),
            DefaultFunction::UnListData => todo!(),
            DefaultFunction::UnIData => todo!(),
            DefaultFunction::UnBData => todo!(),
            DefaultFunction::EqualsData => todo!(),
            DefaultFunction::SerialiseData => todo!(),
            DefaultFunction::MkPairData => todo!(),
            DefaultFunction::MkNilData => todo!(),
            DefaultFunction::MkNilPairData => todo!(),
        }
    }
}
