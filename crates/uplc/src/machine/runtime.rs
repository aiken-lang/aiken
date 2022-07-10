use crate::{ast::Constant, builtins::DefaultFunction};

use super::{
    // cost_model::{CostingFun, ExBudget},
    Error,
    Value,
};

//#[derive(std::cmp::PartialEq)]
//pub enum EvalMode {
//    Immediate,
//    Deferred,
//}

// pub struct BuiltinRuntimeOptions<T, G> {
//     immediate_eval: T,
//     deferred_eval: T,
//     budget: fn(CostingFun<G>) -> fn(Vec<u32>) -> ExBudget,
// }

#[derive(Clone, Debug)]
pub struct BuiltinRuntime {
    meaning: BuiltinMeaning,
    // budget: fn(Vec<u32>) -> ExBudget,
}

#[derive(Clone, Debug)]
struct BuiltinMeaning {
    args: Vec<Value>,
    fun: DefaultFunction,
}

impl BuiltinMeaning {
    pub fn new(fun: DefaultFunction) -> BuiltinMeaning {
        Self { args: vec![], fun }
    }
}

impl BuiltinRuntime {
    // fn from_builtin_runtime_options<G>(
    //     eval_mode: EvalMode,
    //     cost: CostingFun<G>,
    //     runtime_options: BuiltinRuntimeOptions<T, G>,
    // ) -> BuiltinRuntime {
    //    Self {
    //      budget: (runtime_options.budget)(cost),
    //    }
    // }

    pub fn is_arrow(&self) -> bool {
        self.meaning.args.len() != self.meaning.fun.arity()
    }

    pub fn is_ready(&self) -> bool {
        self.meaning.args.len() == self.meaning.fun.arity()
    }

    pub fn call(&self) -> Result<Value, Error> {
        self.meaning.fun.call(&self.meaning.args)
    }

    pub fn push(&mut self, arg: Value) -> Result<(), Error> {
        self.meaning.fun.check_type(&arg)?;

        self.meaning.args.push(arg);

        Ok(())
    }
}

impl From<DefaultFunction> for BuiltinMeaning {
    fn from(fun: DefaultFunction) -> Self {
        BuiltinMeaning::new(fun)
    }
}

impl From<DefaultFunction> for BuiltinRuntime {
    fn from(fun: DefaultFunction) -> Self {
        BuiltinRuntime {
            meaning: fun.into(),
        }
    }
}

impl DefaultFunction {
    pub fn arity(&self) -> usize {
        match self {
            DefaultFunction::AddInteger => 2,
            DefaultFunction::SubtractInteger => todo!(),
            DefaultFunction::MultiplyInteger => todo!(),
            DefaultFunction::DivideInteger => todo!(),
            DefaultFunction::QuotientInteger => todo!(),
            DefaultFunction::RemainderInteger => todo!(),
            DefaultFunction::ModInteger => todo!(),
            DefaultFunction::EqualsInteger => todo!(),
            DefaultFunction::LessThanInteger => todo!(),
            DefaultFunction::LessThanEqualsInteger => todo!(),
            DefaultFunction::AppendByteString => todo!(),
            DefaultFunction::ConsByteString => todo!(),
            DefaultFunction::SliceByteString => todo!(),
            DefaultFunction::LengthOfByteString => todo!(),
            DefaultFunction::IndexByteString => todo!(),
            DefaultFunction::EqualsByteString => todo!(),
            DefaultFunction::LessThanByteString => todo!(),
            DefaultFunction::LessThanEqualsByteString => todo!(),
            DefaultFunction::Sha2_256 => todo!(),
            DefaultFunction::Sha3_256 => todo!(),
            DefaultFunction::Blake2b_256 => todo!(),
            DefaultFunction::VerifySignature => todo!(),
            DefaultFunction::VerifyEcdsaSecp256k1Signature => todo!(),
            DefaultFunction::VerifySchnorrSecp256k1Signature => todo!(),
            DefaultFunction::AppendString => todo!(),
            DefaultFunction::EqualsString => todo!(),
            DefaultFunction::EncodeUtf8 => todo!(),
            DefaultFunction::DecodeUtf8 => todo!(),
            DefaultFunction::IfThenElse => todo!(),
            DefaultFunction::ChooseUnit => todo!(),
            DefaultFunction::Trace => todo!(),
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

    pub fn check_type(&self, arg: &Value) -> Result<(), Error> {
        match self {
            DefaultFunction::AddInteger => {
                if arg.is_integer() {
                    Ok(())
                } else {
                    todo!("type error")
                }
            }
            DefaultFunction::SubtractInteger => todo!(),
            DefaultFunction::MultiplyInteger => todo!(),
            DefaultFunction::DivideInteger => todo!(),
            DefaultFunction::QuotientInteger => todo!(),
            DefaultFunction::RemainderInteger => todo!(),
            DefaultFunction::ModInteger => todo!(),
            DefaultFunction::EqualsInteger => todo!(),
            DefaultFunction::LessThanInteger => todo!(),
            DefaultFunction::LessThanEqualsInteger => todo!(),
            DefaultFunction::AppendByteString => todo!(),
            DefaultFunction::ConsByteString => todo!(),
            DefaultFunction::SliceByteString => todo!(),
            DefaultFunction::LengthOfByteString => todo!(),
            DefaultFunction::IndexByteString => todo!(),
            DefaultFunction::EqualsByteString => todo!(),
            DefaultFunction::LessThanByteString => todo!(),
            DefaultFunction::LessThanEqualsByteString => todo!(),
            DefaultFunction::Sha2_256 => todo!(),
            DefaultFunction::Sha3_256 => todo!(),
            DefaultFunction::Blake2b_256 => todo!(),
            DefaultFunction::VerifySignature => todo!(),
            DefaultFunction::VerifyEcdsaSecp256k1Signature => todo!(),
            DefaultFunction::VerifySchnorrSecp256k1Signature => todo!(),
            DefaultFunction::AppendString => todo!(),
            DefaultFunction::EqualsString => todo!(),
            DefaultFunction::EncodeUtf8 => todo!(),
            DefaultFunction::DecodeUtf8 => todo!(),
            DefaultFunction::IfThenElse => todo!(),
            DefaultFunction::ChooseUnit => todo!(),
            DefaultFunction::Trace => todo!(),
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

    pub fn call(&self, args: &[Value]) -> Result<Value, Error> {
        match self {
            DefaultFunction::AddInteger => {
                assert_eq!(args.len(), self.arity());

                let args = (&args[0], &args[1]);

                match args {
                    (Value::Con(Constant::Integer(arg1)), Value::Con(Constant::Integer(arg2))) => {
                        Ok(Value::Con(Constant::Integer(arg1 + arg2)))
                    }
                    _ => todo!("handle error"),
                }
            }
            DefaultFunction::SubtractInteger => todo!(),
            DefaultFunction::MultiplyInteger => todo!(),
            DefaultFunction::DivideInteger => todo!(),
            DefaultFunction::QuotientInteger => todo!(),
            DefaultFunction::RemainderInteger => todo!(),
            DefaultFunction::ModInteger => todo!(),
            DefaultFunction::EqualsInteger => todo!(),
            DefaultFunction::LessThanInteger => todo!(),
            DefaultFunction::LessThanEqualsInteger => todo!(),
            DefaultFunction::AppendByteString => todo!(),
            DefaultFunction::ConsByteString => todo!(),
            DefaultFunction::SliceByteString => todo!(),
            DefaultFunction::LengthOfByteString => todo!(),
            DefaultFunction::IndexByteString => todo!(),
            DefaultFunction::EqualsByteString => todo!(),
            DefaultFunction::LessThanByteString => todo!(),
            DefaultFunction::LessThanEqualsByteString => todo!(),
            DefaultFunction::Sha2_256 => todo!(),
            DefaultFunction::Sha3_256 => todo!(),
            DefaultFunction::Blake2b_256 => todo!(),
            DefaultFunction::VerifySignature => todo!(),
            DefaultFunction::VerifyEcdsaSecp256k1Signature => todo!(),
            DefaultFunction::VerifySchnorrSecp256k1Signature => todo!(),
            DefaultFunction::AppendString => todo!(),
            DefaultFunction::EqualsString => todo!(),
            DefaultFunction::EncodeUtf8 => todo!(),
            DefaultFunction::DecodeUtf8 => todo!(),
            DefaultFunction::IfThenElse => todo!(),
            DefaultFunction::ChooseUnit => todo!(),
            DefaultFunction::Trace => todo!(),
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
