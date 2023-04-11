use bumpalo::{collections::Vec as BumpVec, Bump};
use std::rc::Rc;

use crate::{
    ast::{Constant, NamedDeBruijn, Term, Type},
    builtins::DefaultFunction,
};

pub mod cost_model;
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

enum MachineStep<'a> {
    Return(&'a Context<'a>, &'a Value<'a>),
    Compute(&'a Context<'a>, Env<'a>, Rc<Term<NamedDeBruijn>>),
    Done(Term<NamedDeBruijn>),
}

#[derive(Clone)]
enum PartialTerm {
    Delay,
    Lambda(Rc<NamedDeBruijn>),
    Apply,
    Force,
}

enum DischargeStep<'a> {
    DischargeValue(&'a Value<'a>),
    DischargeValueEnv(usize, Env<'a>, Rc<Term<NamedDeBruijn>>),
    PopArgStack(PartialTerm),
}

pub struct Machine<'a> {
    arena: &'a Bump,
    costs: CostModel,
    pub ex_budget: ExBudget,
    slippage: u32,
    unbudgeted_steps: [u32; 8],
    pub logs: Vec<String>,
    stack: Vec<MachineStep<'a>>,
    version: Language,
}

impl<'a> Machine<'a> {
    pub fn new(
        arena: &'a Bump,
        version: Language,
        costs: CostModel,
        initial_budget: ExBudget,
        slippage: u32,
    ) -> Machine<'a> {
        Machine {
            arena,
            costs,
            ex_budget: initial_budget,
            slippage,
            unbudgeted_steps: [0; 8],
            logs: vec![],
            stack: vec![],
            version,
        }
    }

    pub fn run(&mut self, term: Term<NamedDeBruijn>) -> Result<Term<NamedDeBruijn>, Error> {
        use MachineStep::*;

        let startup_budget = self.costs.machine_costs.get(StepKind::StartUp);

        self.spend_budget(startup_budget)?;

        let ctx = self.arena.alloc(Context::NoFrame);

        let env = BumpVec::new_in(&self.arena);

        self.stack.push(Compute(ctx, env, term.into()));

        while let Some(step) = self.stack.pop() {
            match step {
                Compute(context, env, t) => {
                    self.compute(context, env, t)?;
                }
                Return(context, value) => {
                    self.return_compute(context, value)?;
                }
                Done(term) => {
                    return Ok(term);
                }
            };
        }

        Err(Error::MachineNeverReachedDone)
    }

    fn compute(
        &mut self,
        context: &'a Context<'a>,
        env: Env<'a>,
        term: Rc<Term<NamedDeBruijn>>,
    ) -> Result<(), Error> {
        match term.as_ref() {
            Term::Var(name) => {
                self.step_and_maybe_spend(StepKind::Var)?;

                let val = self.lookup_var(name.as_ref(), &env)?;

                self.stack.push(MachineStep::Return(context, val));
            }
            Term::Delay(body) => {
                self.step_and_maybe_spend(StepKind::Delay)?;

                let value = self.arena.alloc(Value::Delay(body.clone(), env));

                self.stack.push(MachineStep::Return(context, value));
            }
            Term::Lambda {
                parameter_name,
                body,
            } => {
                self.step_and_maybe_spend(StepKind::Lambda)?;

                let value = self.arena.alloc(Value::Lambda {
                    parameter_name: parameter_name.clone(),
                    body: body.clone(),
                    env,
                });

                self.stack.push(MachineStep::Return(context, value));
            }
            Term::Apply { function, argument } => {
                self.step_and_maybe_spend(StepKind::Apply)?;

                let context = self.arena.alloc(Context::FrameApplyArg(
                    env.clone(),
                    argument.clone(),
                    context,
                ));

                self.stack
                    .push(MachineStep::Compute(context, env, function.clone()));
            }
            Term::Constant(x) => {
                self.step_and_maybe_spend(StepKind::Constant)?;

                let x = self.arena.alloc(x.as_ref().clone());

                let value = self.arena.alloc(Value::Con(x));

                self.stack.push(MachineStep::Return(context, value));
            }
            Term::Force(body) => {
                self.step_and_maybe_spend(StepKind::Force)?;

                let context = self.arena.alloc(Context::FrameForce(context));

                self.stack
                    .push(MachineStep::Compute(context, env, body.clone()));
            }
            Term::Error => return Err(Error::EvaluationFailure),
            Term::Builtin(fun) => {
                self.step_and_maybe_spend(StepKind::Builtin)?;

                let runtime = BuiltinRuntime::new(&self.arena, *fun);

                let value = self.arena.alloc(Value::Builtin {
                    fun: *fun,
                    term,
                    runtime,
                });

                self.stack.push(MachineStep::Return(context, value));
            }
        };

        Ok(())
    }

    fn return_compute(
        &mut self,
        context: &'a Context<'a>,
        value: &'a Value<'a>,
    ) -> Result<(), Error> {
        match context {
            Context::FrameApplyFun(function, ctx) => self.apply_evaluate(ctx, *function, value)?,
            Context::FrameApplyArg(arg_var_env, arg, ctx) => {
                let context = self.arena.alloc(Context::FrameApplyFun(value, ctx));

                self.stack.push(MachineStep::Compute(
                    context,
                    arg_var_env.clone(),
                    arg.clone(),
                ));
            }
            Context::FrameForce(ctx) => self.force_evaluate(*ctx, value)?,
            Context::NoFrame => {
                if self.unbudgeted_steps[7] > 0 {
                    self.spend_unbudgeted_steps()?;
                }

                let term = discharge_value(&self.arena, value);

                self.stack.push(MachineStep::Done(term.as_ref().clone()));
            }
        };

        Ok(())
    }

    fn force_evaluate(
        &mut self,
        context: &'a Context<'a>,
        value: &'a Value<'a>,
    ) -> Result<(), Error> {
        match value {
            Value::Delay(body, env) => {
                self.stack
                    .push(MachineStep::Compute(context, env.clone(), body.clone()));

                Ok(())
            }
            Value::Builtin { fun, term, runtime } => {
                let force_term = Term::Force(term.clone());

                if runtime.needs_force() {
                    let mut runtime = runtime.clone();

                    runtime.consume_force();

                    let mut res = self.eval_builtin_app(*fun, force_term.into(), &runtime)?;

                    self.stack.push(MachineStep::Return(context, &mut res));

                    Ok(())
                } else {
                    Err(Error::BuiltinTermArgumentExpected(force_term))
                }
            }
            rest => Err(Error::NonPolymorphicInstantiation(format!("{:#?}", rest))),
        }
    }

    fn apply_evaluate(
        &mut self,
        context: &'a Context,
        function: &'a Value,
        argument: &'a Value,
    ) -> Result<(), Error> {
        match function {
            Value::Lambda { body, env, .. } => {
                let mut new_env = env.clone();

                new_env.push(argument);

                self.stack
                    .push(MachineStep::Compute(context, new_env, body.clone()));

                Ok(())
            }
            Value::Builtin { fun, term, runtime } => {
                let arg_term = discharge_value(&self.arena, argument);

                let t = Term::<NamedDeBruijn>::Apply {
                    function: term.clone(),
                    argument: arg_term.into(),
                };

                if runtime.is_arrow() && !runtime.needs_force() {
                    let mut runtime = runtime.clone();

                    runtime.push(argument)?;

                    let mut res = self.eval_builtin_app(*fun, t.into(), &runtime)?;

                    self.stack.push(MachineStep::Return(context, &mut res));

                    Ok(())
                } else {
                    Err(Error::UnexpectedBuiltinTermArgument(t))
                }
            }
            rest => Err(Error::NonFunctionalApplication(
                format!("{:#?}", rest),
                format!("{:#?}", argument),
            )),
        }
    }

    fn eval_builtin_app(
        &mut self,
        fun: DefaultFunction,
        term: Rc<Term<NamedDeBruijn>>,
        runtime: &'a BuiltinRuntime<'a>,
    ) -> Result<&'a Value<'a>, Error> {
        if runtime.is_ready() {
            let cost = match self.version {
                Language::PlutusV1 => {
                    runtime.to_ex_budget_v1(&self.arena, &self.costs.builtin_costs)
                }
                Language::PlutusV2 => {
                    runtime.to_ex_budget_v2(&self.arena, &self.costs.builtin_costs)
                }
            };

            self.spend_budget(cost)?;

            runtime.call(&self.arena, &mut self.logs)
        } else {
            let value = self.arena.alloc(Value::Builtin {
                fun,
                term,
                runtime: runtime.clone(),
            });

            Ok(value)
        }
    }

    fn lookup_var(&mut self, name: &NamedDeBruijn, env: &Env<'a>) -> Result<&'a Value, Error> {
        env.get::<usize>(env.len() - usize::from(name.index))
            .map(|v| *v)
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

fn discharge_value<'a>(arena: &'a Bump, value: &'a Value<'a>) -> Rc<Term<NamedDeBruijn>> {
    let mut stack = vec![DischargeStep::DischargeValue(value)];

    let mut term_stack = vec![];

    while let Some(stack_frame) = stack.pop() {
        match stack_frame {
            DischargeStep::DischargeValue(value) => match value {
                Value::Con(x) => {
                    let term = Term::Constant(x.clone().clone().into());

                    term_stack.push(Rc::new(term));
                }
                Value::Builtin { term, .. } => term_stack.push(term.clone()),
                Value::Delay(body, env) => {
                    let term = Term::Delay(body.clone());

                    stack.push(DischargeStep::DischargeValueEnv(
                        0,
                        env.clone(),
                        term.into(),
                    ));
                }
                Value::Lambda {
                    parameter_name,
                    body,
                    env,
                } => {
                    let term = Term::Lambda {
                        parameter_name: parameter_name.clone(),
                        body: body.clone(),
                    };

                    stack.push(DischargeStep::DischargeValueEnv(
                        0,
                        env.clone(),
                        term.into(),
                    ));
                }
            },
            DischargeStep::DischargeValueEnv(lam_cnt, env, term) => match term.as_ref() {
                Term::Var(name) => {
                    let index: usize = name.index.into();

                    if lam_cnt >= index {
                        let term = Term::Var(name.clone());

                        term_stack.push(term.into());
                    } else {
                        let env = env.get::<usize>(env.len() - (index - lam_cnt));

                        if let Some(v) = env {
                            stack.push(DischargeStep::DischargeValue(v));
                        } else {
                            let term = Term::Var(name.clone());

                            term_stack.push(term.into());
                        }
                    }
                }
                Term::Lambda {
                    parameter_name,
                    body,
                } => {
                    stack.push(DischargeStep::PopArgStack(PartialTerm::Lambda(
                        parameter_name.clone(),
                    )));

                    stack.push(DischargeStep::DischargeValueEnv(
                        lam_cnt + 1,
                        env,
                        body.clone(),
                    ));
                }
                Term::Apply { function, argument } => {
                    stack.push(DischargeStep::PopArgStack(PartialTerm::Apply));

                    stack.push(DischargeStep::DischargeValueEnv(
                        lam_cnt,
                        env,
                        argument.clone(),
                    ));

                    stack.push(DischargeStep::DischargeValueEnv(
                        lam_cnt,
                        env,
                        function.clone(),
                    ));
                }
                Term::Delay(body) => {
                    stack.push(DischargeStep::PopArgStack(PartialTerm::Delay));

                    stack.push(DischargeStep::DischargeValueEnv(lam_cnt, env, body.clone()));
                }

                Term::Force(body) => {
                    stack.push(DischargeStep::PopArgStack(PartialTerm::Force));

                    stack.push(DischargeStep::DischargeValueEnv(lam_cnt, env, body.clone()));
                }
                rest => {
                    term_stack.push(term);
                }
            },
            DischargeStep::PopArgStack(term) => match term {
                PartialTerm::Delay => {
                    let body = term_stack.pop().unwrap();

                    let term = Term::Delay(body.into());

                    term_stack.push(term.into());
                }
                PartialTerm::Lambda(parameter_name) => {
                    let body = term_stack.pop().unwrap();

                    let term = Term::Lambda {
                        parameter_name: parameter_name.clone(),
                        body: body.into(),
                    };

                    term_stack.push(term.into());
                }
                PartialTerm::Apply => {
                    let argument = term_stack.pop().unwrap();
                    let function = term_stack.pop().unwrap();

                    let term = Term::Apply {
                        function: function.into(),
                        argument: argument.into(),
                    };

                    term_stack.push(term.into());
                }
                PartialTerm::Force => {
                    let body = term_stack.pop().unwrap();

                    let term = Term::Force(body.clone().into());

                    term_stack.push(term.into());
                }
            },
        }
    }

    term_stack.pop().expect("arg_stack should not be empty")
}

enum Context<'a> {
    FrameApplyFun(&'a Value<'a>, &'a Context<'a>),
    FrameApplyArg(Env<'a>, Rc<Term<NamedDeBruijn>>, &'a Context<'a>),
    FrameForce(&'a Context<'a>),
    NoFrame,
}

impl<'a> TryFrom<Value<'a>> for Type {
    type Error = Error;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        let constant: Constant = value.try_into()?;

        let constant_type = Type::from(&constant);

        Ok(constant_type)
    }
}

impl<'a> TryFrom<&'a Value<'a>> for Type {
    type Error = Error;

    fn try_from(value: &'a Value) -> Result<Self, Self::Error> {
        let constant: Constant = value.try_into()?;

        let constant_type = Type::from(&constant);

        Ok(constant_type)
    }
}

impl<'a> TryFrom<Value<'a>> for Constant {
    type Error = Error;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Con(constant) => Ok(constant.clone()),
            rest => Err(Error::NotAConstant(format!("{:#?}", rest))),
        }
    }
}

impl<'a> TryFrom<&'a mut Value<'a>> for Constant {
    type Error = Error;

    fn try_from(value: &'a mut Value) -> Result<Self, Self::Error> {
        match value {
            Value::Con(constant) => Ok(constant.clone()),
            rest => Err(Error::NotAConstant(format!("{:#?}", rest))),
        }
    }
}

impl<'a> TryFrom<&'a Value<'a>> for Constant {
    type Error = Error;

    fn try_from(value: &'a Value) -> Result<Self, Self::Error> {
        match value {
            Value::Con(constant) => Ok(*constant.clone()),
            rest => Err(Error::NotAConstant(format!("{:#?}", rest))),
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
