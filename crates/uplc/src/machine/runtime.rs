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
            DefaultFunction::DivideInteger => todo!(),
            DefaultFunction::QuotientInteger => todo!(),
            DefaultFunction::RemainderInteger => todo!(),
            DefaultFunction::ModInteger => todo!(),
            DefaultFunction::EqualsInteger => 2,
            DefaultFunction::LessThanInteger => 2,
            DefaultFunction::LessThanEqualsInteger => 2,
            DefaultFunction::AppendByteString => 2,
            DefaultFunction::ConsByteString => todo!(),
            DefaultFunction::SliceByteString => todo!(),
            DefaultFunction::LengthOfByteString => todo!(),
            DefaultFunction::IndexByteString => todo!(),
            DefaultFunction::EqualsByteString => 2,
            DefaultFunction::LessThanByteString => todo!(),
            DefaultFunction::LessThanEqualsByteString => todo!(),
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
            DefaultFunction::DivideInteger => todo!(),
            DefaultFunction::QuotientInteger => todo!(),
            DefaultFunction::RemainderInteger => todo!(),
            DefaultFunction::ModInteger => todo!(),
            DefaultFunction::EqualsInteger => 0,
            DefaultFunction::LessThanInteger => 0,
            DefaultFunction::LessThanEqualsInteger => 0,
            DefaultFunction::AppendByteString => 0,
            DefaultFunction::ConsByteString => todo!(),
            DefaultFunction::SliceByteString => todo!(),
            DefaultFunction::LengthOfByteString => todo!(),
            DefaultFunction::IndexByteString => todo!(),
            DefaultFunction::EqualsByteString => 0,
            DefaultFunction::LessThanByteString => todo!(),
            DefaultFunction::LessThanEqualsByteString => todo!(),
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
            DefaultFunction::DivideInteger => todo!(),
            DefaultFunction::QuotientInteger => todo!(),
            DefaultFunction::RemainderInteger => todo!(),
            DefaultFunction::ModInteger => todo!(),
            DefaultFunction::EqualsInteger => arg.expect_type(Type::Integer),
            DefaultFunction::LessThanInteger => arg.expect_type(Type::Integer),
            DefaultFunction::LessThanEqualsInteger => arg.expect_type(Type::Integer),
            DefaultFunction::AppendByteString => arg.expect_type(Type::ByteString),
            DefaultFunction::ConsByteString => todo!(),
            DefaultFunction::SliceByteString => todo!(),
            DefaultFunction::LengthOfByteString => todo!(),
            DefaultFunction::IndexByteString => todo!(),
            DefaultFunction::EqualsByteString => arg.expect_type(Type::ByteString),
            DefaultFunction::LessThanByteString => todo!(),
            DefaultFunction::LessThanEqualsByteString => todo!(),
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
            DefaultFunction::AddInteger => {
                let args = (&args[0], &args[1]);

                match args {
                    (Value::Con(Constant::Integer(arg1)), Value::Con(Constant::Integer(arg2))) => {
                        Ok(Value::Con(Constant::Integer(arg1 + arg2)))
                    }
                    _ => unreachable!(),
                }
            }
            DefaultFunction::SubtractInteger => {
                let args = (&args[0], &args[1]);

                match args {
                    (Value::Con(Constant::Integer(arg1)), Value::Con(Constant::Integer(arg2))) => {
                        Ok(Value::Con(Constant::Integer(arg1 - arg2)))
                    }
                    _ => unreachable!(),
                }
            }
            DefaultFunction::MultiplyInteger => {
                let args = (&args[0], &args[1]);

                match args {
                    (Value::Con(Constant::Integer(arg1)), Value::Con(Constant::Integer(arg2))) => {
                        Ok(Value::Con(Constant::Integer(arg1 * arg2)))
                    }
                    _ => unreachable!(),
                }
            }
            DefaultFunction::DivideInteger => todo!(),
            DefaultFunction::QuotientInteger => todo!(),
            DefaultFunction::RemainderInteger => todo!(),
            DefaultFunction::ModInteger => todo!(),
            DefaultFunction::EqualsInteger => {
                let args = (&args[0], &args[1]);

                match args {
                    (Value::Con(Constant::Integer(arg1)), Value::Con(Constant::Integer(arg2))) => {
                        Ok(Value::Con(Constant::Bool(arg1 == arg2)))
                    }
                    _ => unreachable!(),
                }
            }
            DefaultFunction::LessThanInteger => {
                let args = (&args[0], &args[1]);

                match args {
                    (Value::Con(Constant::Integer(arg1)), Value::Con(Constant::Integer(arg2))) => {
                        Ok(Value::Con(Constant::Bool(arg1 < arg2)))
                    }
                    _ => unreachable!(),
                }
            }
            DefaultFunction::LessThanEqualsInteger => {
                let args = (&args[0], &args[1]);

                match args {
                    (Value::Con(Constant::Integer(arg1)), Value::Con(Constant::Integer(arg2))) => {
                        Ok(Value::Con(Constant::Bool(arg1 <= arg2)))
                    }
                    _ => unreachable!(),
                }
            }
            DefaultFunction::AppendByteString => {
                let args = (&args[0], &args[1]);

                match args {
                    (
                        Value::Con(Constant::ByteString(arg1)),
                        Value::Con(Constant::ByteString(arg2)),
                    ) => Ok(Value::Con(Constant::ByteString(
                        arg1.iter().copied().chain(arg2.iter().copied()).collect(),
                    ))),
                    _ => unreachable!(),
                }
            }
            DefaultFunction::ConsByteString => todo!(),
            DefaultFunction::SliceByteString => todo!(),
            DefaultFunction::LengthOfByteString => todo!(),
            DefaultFunction::IndexByteString => todo!(),
            DefaultFunction::EqualsByteString => {
                let args = (&args[0], &args[1]);

                match args {
                    (
                        Value::Con(Constant::ByteString(arg1)),
                        Value::Con(Constant::ByteString(arg2)),
                    ) => Ok(Value::Con(Constant::Bool(arg1 == arg2))),
                    _ => unreachable!(),
                }
            }
            DefaultFunction::LessThanByteString => todo!(),
            DefaultFunction::LessThanEqualsByteString => todo!(),
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
            DefaultFunction::AppendString => {
                let args = (&args[0], &args[1]);

                match args {
                    (Value::Con(Constant::String(arg1)), Value::Con(Constant::String(arg2))) => {
                        Ok(Value::Con(Constant::String(format!("{}{}", arg1, arg2))))
                    }
                    _ => unreachable!(),
                }
            }
            DefaultFunction::EqualsString => {
                let args = (&args[0], &args[1]);

                match args {
                    (Value::Con(Constant::String(arg1)), Value::Con(Constant::String(arg2))) => {
                        Ok(Value::Con(Constant::Bool(arg1 == arg2)))
                    }
                    _ => unreachable!(),
                }
            }
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
