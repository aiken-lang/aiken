use std::rc::Rc;

use crate::{
    ast::{Constant, NamedDeBruijn, Term, Type},
    builtins::DefaultFunction,
};

pub mod cost_model;
mod discharge;
mod error;
pub mod eval_result;
pub mod runtime;
pub mod value;

use cost_model::{ExBudget, StepKind};
pub use error::Error;
use pallas_primitives::babbage::Language;

use self::{
    cost_model::CostModel,
    runtime::BuiltinRuntime,
    value::{Env, Value},
};

enum MachineState {
    Return(Context, Value),
    Compute(Context, Env, Term<NamedDeBruijn>),
    Done(Rc<Term<NamedDeBruijn>>),
}

#[derive(Clone)]
enum Context {
    FrameApplyFun(Value, Box<Context>),
    FrameApplyArg(Env, Rc<Term<NamedDeBruijn>>, Box<Context>),
    FrameForce(Box<Context>),
    NoFrame,
}

pub struct Machine {
    costs: CostModel,
    pub ex_budget: ExBudget,
    slippage: u32,
    unbudgeted_steps: [u32; 8],
    pub logs: Vec<String>,
    version: Language,
}

impl Machine {
    pub fn new(
        version: Language,
        costs: CostModel,
        initial_budget: ExBudget,
        slippage: u32,
    ) -> Machine {
        Machine {
            costs,
            ex_budget: initial_budget,
            slippage,
            unbudgeted_steps: [0; 8],
            logs: vec![],
            version,
        }
    }

    pub fn run(&mut self, term: Term<NamedDeBruijn>) -> Result<Term<NamedDeBruijn>, Error> {
        use MachineState::*;

        let startup_budget = self.costs.machine_costs.get(StepKind::StartUp);

        self.spend_budget(startup_budget)?;

        let mut state = Compute(Context::NoFrame, Rc::new(vec![]), term);

        loop {
            state = match state {
                Compute(context, env, t) => self.compute(context, env, t)?,
                Return(context, value) => self.return_compute(context, value)?,
                Done(t) => {
                    return Ok(t.as_ref().clone());
                }
            };
        }
    }

    fn compute(
        &mut self,
        context: Context,
        env: Env,
        term: Term<NamedDeBruijn>,
    ) -> Result<MachineState, Error> {
        match term {
            Term::Var(name) => {
                self.step_and_maybe_spend(StepKind::Var)?;

                let val = self.lookup_var(name.as_ref(), &env)?;

                Ok(MachineState::Return(context, val))
            }
            Term::Delay(body) => {
                self.step_and_maybe_spend(StepKind::Delay)?;

                Ok(MachineState::Return(context, Value::Delay(body, env)))
            }
            Term::Lambda {
                parameter_name,
                body,
            } => {
                self.step_and_maybe_spend(StepKind::Lambda)?;

                Ok(MachineState::Return(
                    context,
                    Value::Lambda {
                        parameter_name,
                        body,
                        env,
                    },
                ))
            }
            Term::Apply { function, argument } => {
                self.step_and_maybe_spend(StepKind::Apply)?;

                Ok(MachineState::Compute(
                    Context::FrameApplyArg(Rc::clone(&env), argument, context.into()),
                    env,
                    function.as_ref().clone(),
                ))
            }
            Term::Constant(x) => {
                self.step_and_maybe_spend(StepKind::Constant)?;

                Ok(MachineState::Return(context, Value::Con(x)))
            }
            Term::Force(body) => {
                self.step_and_maybe_spend(StepKind::Force)?;

                Ok(MachineState::Compute(
                    Context::FrameForce(context.into()),
                    env,
                    body.as_ref().clone(),
                ))
            }
            Term::Error => Err(Error::EvaluationFailure),
            Term::Builtin(fun) => {
                self.step_and_maybe_spend(StepKind::Builtin)?;

                let runtime: BuiltinRuntime = fun.into();

                Ok(MachineState::Return(
                    context,
                    Value::Builtin {
                        fun,
                        term: term.into(),
                        runtime,
                    },
                ))
            }
        }
    }

    fn return_compute(&mut self, context: Context, value: Value) -> Result<MachineState, Error> {
        match context {
            Context::FrameApplyFun(function, ctx) => self.apply_evaluate(*ctx, function, value),
            Context::FrameApplyArg(arg_var_env, arg, ctx) => Ok(MachineState::Compute(
                Context::FrameApplyFun(value, ctx),
                arg_var_env,
                arg.as_ref().clone(),
            )),
            Context::FrameForce(ctx) => self.force_evaluate(*ctx, value),
            Context::NoFrame => {
                if self.unbudgeted_steps[7] > 0 {
                    self.spend_unbudgeted_steps()?;
                }

                let term = discharge::value_as_term(value);

                Ok(MachineState::Done(term))
            }
        }
    }

    fn force_evaluate(&mut self, context: Context, value: Value) -> Result<MachineState, Error> {
        match value {
            Value::Delay(body, env) => {
                Ok(MachineState::Compute(context, env, body.as_ref().clone()))
            }
            Value::Builtin {
                fun,
                term,
                mut runtime,
            } => {
                let force_term = Term::Force(term);

                if runtime.needs_force() {
                    runtime.consume_force();

                    let res = self.eval_builtin_app(fun, force_term.into(), runtime)?;

                    Ok(MachineState::Return(context, res))
                } else {
                    Err(Error::BuiltinTermArgumentExpected(force_term))
                }
            }
            rest => Err(Error::NonPolymorphicInstantiation(rest)),
        }
    }

    fn apply_evaluate(
        &mut self,
        context: Context,
        function: Value,
        argument: Value,
    ) -> Result<MachineState, Error> {
        match function {
            Value::Lambda { body, mut env, .. } => {
                let e = Rc::make_mut(&mut env);

                e.push(argument);

                Ok(MachineState::Compute(
                    context,
                    Rc::new(e.clone()),
                    body.as_ref().clone(),
                ))
            }
            Value::Builtin { fun, term, runtime } => {
                let arg_term = discharge::value_as_term(argument.clone());

                let t = Rc::new(Term::<NamedDeBruijn>::Apply {
                    function: term,
                    argument: arg_term,
                });

                if runtime.is_arrow() && !runtime.needs_force() {
                    let mut runtime = runtime;

                    runtime.push(argument)?;

                    let res = self.eval_builtin_app(fun, t, runtime.to_owned())?;

                    Ok(MachineState::Return(context, res))
                } else {
                    Err(Error::UnexpectedBuiltinTermArgument(t.as_ref().clone()))
                }
            }
            rest => Err(Error::NonFunctionalApplication(rest, argument)),
        }
    }

    fn eval_builtin_app(
        &mut self,
        fun: DefaultFunction,
        term: Rc<Term<NamedDeBruijn>>,
        runtime: BuiltinRuntime,
    ) -> Result<Value, Error> {
        if runtime.is_ready() {
            let cost = match self.version {
                Language::PlutusV1 => runtime.to_ex_budget_v1(&self.costs.builtin_costs),
                Language::PlutusV2 => runtime.to_ex_budget_v2(&self.costs.builtin_costs),
            };
            self.spend_budget(cost)?;

            runtime.call(&mut self.logs)
        } else {
            Ok(Value::Builtin { fun, term, runtime })
        }
    }

    fn lookup_var(&mut self, name: &NamedDeBruijn, env: &[Value]) -> Result<Value, Error> {
        env.get::<usize>(env.len() - usize::from(name.index))
            .cloned()
            .ok_or_else(|| Error::OpenTermEvaluated(Term::Var(name.clone().into())))
    }

    fn step_and_maybe_spend(&mut self, step: StepKind) -> Result<(), Error> {
        let index = step as u8;
        self.unbudgeted_steps[index as usize] += 1;
        self.unbudgeted_steps[7] += 1;

        if self.unbudgeted_steps[7] >= self.slippage {
            self.spend_unbudgeted_steps()?;
        }

        Ok(())
    }

    fn spend_unbudgeted_steps(&mut self) -> Result<(), Error> {
        for i in 0..self.unbudgeted_steps.len() - 1 {
            let mut unspent_step_budget =
                self.costs.machine_costs.get(StepKind::try_from(i as u8)?);

            unspent_step_budget.occurrences(self.unbudgeted_steps[i] as i64);

            self.spend_budget(unspent_step_budget)?;

            self.unbudgeted_steps[i] = 0;
        }

        self.unbudgeted_steps[7] = 0;

        Ok(())
    }

    fn spend_budget(&mut self, spend_budget: ExBudget) -> Result<(), Error> {
        self.ex_budget.mem -= spend_budget.mem;
        self.ex_budget.cpu -= spend_budget.cpu;

        if self.ex_budget.mem < 0 || self.ex_budget.cpu < 0 {
            Err(Error::OutOfExError(self.ex_budget))
        } else {
            Ok(())
        }
    }
}

impl From<&Constant> for Type {
    fn from(constant: &Constant) -> Self {
        match constant {
            Constant::Integer(_) => Type::Integer,
            Constant::ByteString(_) => Type::ByteString,
            Constant::String(_) => Type::String,
            Constant::Unit => Type::Unit,
            Constant::Bool(_) => Type::Bool,
            Constant::ProtoList(t, _) => Type::List(Rc::new(t.clone())),
            Constant::ProtoPair(t1, t2, _, _) => {
                Type::Pair(Rc::new(t1.clone()), Rc::new(t2.clone()))
            }
            Constant::Data(_) => Type::Data,
        }
    }
}

#[cfg(test)]
mod tests {
    use num_bigint::BigInt;

    use super::cost_model::ExBudget;
    use crate::{
        ast::{Constant, NamedDeBruijn, Program, Term},
        builtins::DefaultFunction,
    };

    #[test]
    fn add_big_ints() {
        let program: Program<NamedDeBruijn> = Program {
            version: (0, 0, 0),
            term: Term::Apply {
                function: Term::Apply {
                    function: Term::Builtin(DefaultFunction::AddInteger).into(),
                    argument: Term::Constant(Constant::Integer(i128::MAX.into()).into()).into(),
                }
                .into(),
                argument: Term::Constant(Constant::Integer(i128::MAX.into()).into()).into(),
            },
        };

        let eval_result = program.eval(ExBudget::default());

        let term = eval_result.result().unwrap();

        assert_eq!(
            term,
            Term::Constant(
                Constant::Integer(
                    Into::<BigInt>::into(i128::MAX) + Into::<BigInt>::into(i128::MAX)
                )
                .into()
            )
        );
    }

    #[test]
    fn divide_integer() {
        let make_program = |fun: DefaultFunction, n: i32, m: i32| Program::<NamedDeBruijn> {
            version: (0, 0, 0),
            term: Term::Apply {
                function: Term::Apply {
                    function: Term::Builtin(fun).into(),
                    argument: Term::Constant(Constant::Integer(n.into()).into()).into(),
                }
                .into(),
                argument: Term::Constant(Constant::Integer(m.into()).into()).into(),
            },
        };

        let test_data = vec![
            (DefaultFunction::DivideInteger, 8, 3, 2),
            (DefaultFunction::DivideInteger, 8, -3, -3),
            (DefaultFunction::DivideInteger, -8, 3, -3),
            (DefaultFunction::DivideInteger, -8, -3, 2),
            (DefaultFunction::QuotientInteger, 8, 3, 2),
            (DefaultFunction::QuotientInteger, 8, -3, -2),
            (DefaultFunction::QuotientInteger, -8, 3, -2),
            (DefaultFunction::QuotientInteger, -8, -3, 2),
            (DefaultFunction::RemainderInteger, 8, 3, 2),
            (DefaultFunction::RemainderInteger, 8, -3, 2),
            (DefaultFunction::RemainderInteger, -8, 3, -2),
            (DefaultFunction::RemainderInteger, -8, -3, -2),
            (DefaultFunction::ModInteger, 8, 3, 2),
            (DefaultFunction::ModInteger, 8, -3, -1),
            (DefaultFunction::ModInteger, -8, 3, 1),
            (DefaultFunction::ModInteger, -8, -3, -2),
        ];

        for (fun, n, m, result) in test_data {
            let eval_result = make_program(fun, n, m).eval(ExBudget::default());

            assert_eq!(
                eval_result.result().unwrap(),
                Term::Constant(Constant::Integer(result.into()).into())
            );
        }
    }
}
