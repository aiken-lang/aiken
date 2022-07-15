use crate::{ast::Constant, builtins::DefaultFunction};

use super::{
    cost_model::{BuiltinCosts, ExBudget},
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
    args: Vec<Value>,
    fun: DefaultFunction,
}

impl BuiltinRuntime {
    pub fn new(fun: DefaultFunction) -> BuiltinRuntime {
        Self { args: vec![], fun }
    }

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
        self.args.len() != self.fun.arity()
    }

    pub fn is_ready(&self) -> bool {
        self.args.len() == self.fun.arity()
    }

    pub fn is_all(&self) -> bool {
        self.args.is_empty()
    }

    pub fn call(&self) -> Result<Value, Error> {
        self.fun.call(&self.args)
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
            DefaultFunction::IfThenElse => 3,
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

    pub fn force_count() -> i32 {
        3
    }

    pub fn check_type(&self, arg: &Value, args: &[Value]) -> Result<(), Error> {
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
            DefaultFunction::IfThenElse => {
                if args.is_empty() {
                    if arg.is_bool() {
                        return Ok(());
                    } else {
                        todo!("type error")
                    }
                }

                Ok(())
            }
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
            DefaultFunction::IfThenElse => match args[0] {
                Value::Con(Constant::Bool(condition)) => {
                    if condition {
                        Ok(args[1].clone())
                    } else {
                        Ok(args[2].clone())
                    }
                }
                _ => todo!("handle error"),
            },
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
