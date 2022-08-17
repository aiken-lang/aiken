use std::rc::Rc;

use crate::{
    ast::{Constant, NamedDeBruijn, Term, Type},
    builtins::DefaultFunction,
};

pub mod cost_model;
mod error;
mod runtime;

use cost_model::{ExBudget, StepKind};
pub use error::Error;

use self::{cost_model::CostModel, runtime::BuiltinRuntime};

enum MachineStep {
    Return(Rc<Context>, Value),
    Compute(Rc<Context>, Rc<Vec<Value>>, Rc<Term<NamedDeBruijn>>),
    Done(Rc<Term<NamedDeBruijn>>),
}

impl TryFrom<Option<MachineStep>> for Term<NamedDeBruijn> {
    type Error = Error;

    fn try_from(value: Option<MachineStep>) -> Result<Self, Error> {
        match value {
            Some(MachineStep::Done(term)) => Ok(Rc::as_ref(&term).clone()),
            _ => Err(Error::MachineNeverReachedDone),
        }
    }
}

pub struct Machine {
    costs: CostModel,
    pub ex_budget: ExBudget,
    slippage: u32,
    unbudgeted_steps: [u32; 8],
    pub logs: Vec<String>,
    stack: Vec<MachineStep>,
}

impl Machine {
    pub fn new(costs: CostModel, initial_budget: ExBudget, slippage: u32) -> Machine {
        Machine {
            costs,
            ex_budget: initial_budget,
            slippage,
            unbudgeted_steps: [0; 8],
            logs: vec![],
            stack: vec![],
        }
    }

    pub fn run(&mut self, term: &Term<NamedDeBruijn>) -> Result<Term<NamedDeBruijn>, Error> {
        use MachineStep::*;

        let startup_budget = self.costs.machine_costs.get(StepKind::StartUp);

        self.spend_budget(startup_budget)?;

        self.stack.push(Compute(
            Rc::new(Context::NoFrame),
            Rc::new(vec![]),
            Rc::new(term.clone()),
        ));

        while let Some(step) = self.stack.pop() {
            match step {
                Compute(context, env, t) => {
                    self.compute(context, env, t)?;
                }
                Return(context, value) => {
                    self.return_compute(context, value)?;
                }
                d @ Done(_) => {
                    self.stack.push(d);

                    break;
                }
            };
        }

        self.stack.pop().try_into()
    }

    fn compute(
        &mut self,
        context: Rc<Context>,
        env: Rc<Vec<Value>>,
        term: Rc<Term<NamedDeBruijn>>,
    ) -> Result<(), Error> {
        match term.as_ref() {
            Term::Var(name) => {
                self.step_and_maybe_spend(StepKind::Var)?;

                let val = self.lookup_var(name, env)?;

                self.stack.push(MachineStep::Return(context, val));
            }
            Term::Delay(body) => {
                self.step_and_maybe_spend(StepKind::Delay)?;

                self.stack.push(MachineStep::Return(
                    context,
                    Value::Delay(Rc::clone(body), env),
                ));
            }
            Term::Lambda {
                parameter_name,
                body,
            } => {
                self.step_and_maybe_spend(StepKind::Lambda)?;

                self.stack.push(MachineStep::Return(
                    context,
                    Value::Lambda {
                        parameter_name: parameter_name.clone(),
                        body: Rc::clone(body),
                        env,
                    },
                ));
            }
            Term::Apply { function, argument } => {
                self.step_and_maybe_spend(StepKind::Apply)?;

                self.stack.push(MachineStep::Compute(
                    Rc::new(Context::FrameApplyArg(
                        Rc::clone(&env),
                        Rc::clone(argument),
                        context,
                    )),
                    env,
                    Rc::clone(function),
                ));
            }
            Term::Constant(x) => {
                self.step_and_maybe_spend(StepKind::Constant)?;

                self.stack
                    .push(MachineStep::Return(context, Value::Con(x.clone())));
            }
            Term::Force(body) => {
                self.step_and_maybe_spend(StepKind::Force)?;

                self.stack.push(MachineStep::Compute(
                    Rc::new(Context::FrameForce(context)),
                    env,
                    Rc::clone(body),
                ));
            }
            Term::Error => return Err(Error::EvaluationFailure),
            Term::Builtin(fun) => {
                self.step_and_maybe_spend(StepKind::Builtin)?;

                let runtime: BuiltinRuntime = (*fun).into();

                self.stack.push(MachineStep::Return(
                    context,
                    Value::Builtin {
                        fun: *fun,
                        term,
                        runtime,
                    },
                ));
            }
        };

        Ok(())
    }

    fn return_compute(&mut self, context: Rc<Context>, value: Value) -> Result<(), Error> {
        match context.as_ref() {
            Context::FrameApplyFun(function, ctx) => {
                self.apply_evaluate(ctx.to_owned(), function.to_owned(), value)?
            }
            Context::FrameApplyArg(arg_var_env, arg, ctx) => {
                self.stack.push(MachineStep::Compute(
                    Rc::new(Context::FrameApplyFun(value, ctx.to_owned())),
                    arg_var_env.to_owned(),
                    Rc::clone(arg),
                ));
            }
            Context::FrameForce(ctx) => self.force_evaluate(ctx.to_owned(), value)?,
            Context::NoFrame => {
                if self.unbudgeted_steps[7] > 0 {
                    self.spend_unbudgeted_steps()?;
                }

                let term = self.discharge_value(value);

                self.stack.push(MachineStep::Done(term));
            }
        };

        Ok(())
    }

    fn discharge_value(&mut self, value: Value) -> Rc<Term<NamedDeBruijn>> {
        match value {
            Value::Con(x) => Rc::new(Term::Constant(x)),
            Value::Builtin { term, .. } => term,
            Value::Delay(body, env) => self.discharge_value_env(env, Rc::new(Term::Delay(body))),
            Value::Lambda {
                parameter_name,
                body,
                env,
            } => self.discharge_value_env(
                env,
                Rc::new(Term::Lambda {
                    parameter_name: NamedDeBruijn {
                        text: parameter_name.text,
                        index: 0.into(),
                    },
                    body,
                }),
            ),
        }
    }

    fn discharge_value_env(
        &mut self,
        env: Rc<Vec<Value>>,
        term: Rc<Term<NamedDeBruijn>>,
    ) -> Rc<Term<NamedDeBruijn>> {
        fn rec(
            lam_cnt: usize,
            t: Rc<Term<NamedDeBruijn>>,
            this: &mut Machine,
            env: Rc<Vec<Value>>,
        ) -> Rc<Term<NamedDeBruijn>> {
            match t.as_ref() {
                Term::Var(name) => {
                    let index: usize = name.index.into();
                    if lam_cnt >= index {
                        Rc::new(Term::Var(name.clone()))
                    } else {
                        env.get::<usize>(env.len() - (index - lam_cnt))
                            .cloned()
                            .map_or(Rc::new(Term::Var(name.clone())), |v| {
                                this.discharge_value(v)
                            })
                    }
                }
                Term::Lambda {
                    parameter_name,
                    body,
                } => Rc::new(Term::Lambda {
                    parameter_name: parameter_name.clone(),
                    body: rec(lam_cnt + 1, Rc::clone(body), this, env),
                }),
                Term::Apply { function, argument } => Rc::new(Term::Apply {
                    function: rec(lam_cnt, Rc::clone(function), this, Rc::clone(&env)),
                    argument: rec(lam_cnt, Rc::clone(argument), this, env),
                }),

                Term::Delay(x) => Rc::new(Term::Delay(rec(lam_cnt, Rc::clone(x), this, env))),
                Term::Force(x) => Rc::new(Term::Force(rec(lam_cnt, Rc::clone(x), this, env))),
                rest => Rc::new(rest.clone()),
            }
        }
        rec(0, term, self, env)
    }

    fn force_evaluate(&mut self, context: Rc<Context>, value: Value) -> Result<(), Error> {
        match value {
            Value::Delay(body, env) => {
                self.stack.push(MachineStep::Compute(context, env, body));

                Ok(())
            }
            Value::Builtin {
                fun,
                term,
                mut runtime,
            } => {
                let force_term = Rc::new(Term::Force(term));

                if runtime.needs_force() {
                    runtime.consume_force();

                    let res = self.eval_builtin_app(fun, force_term, runtime)?;

                    self.stack.push(MachineStep::Return(context, res));

                    Ok(())
                } else {
                    Err(Error::BuiltinTermArgumentExpected(
                        force_term.as_ref().clone(),
                    ))
                }
            }
            rest => Err(Error::NonPolymorphicInstantiation(rest)),
        }
    }

    fn apply_evaluate(
        &mut self,
        context: Rc<Context>,
        function: Value,
        argument: Value,
    ) -> Result<(), Error> {
        match function {
            Value::Lambda { body, mut env, .. } => {
                let e = Rc::make_mut(&mut env);

                e.push(argument);

                self.stack
                    .push(MachineStep::Compute(context, Rc::new(e.clone()), body));

                Ok(())
            }
            Value::Builtin {
                fun,
                term,
                mut runtime,
            } => {
                let arg_term = self.discharge_value(argument.clone());

                let t = Rc::new(Term::<NamedDeBruijn>::Apply {
                    function: term,
                    argument: arg_term,
                });

                if runtime.is_arrow() && !runtime.needs_force() {
                    runtime.push(argument)?;

                    let res = self.eval_builtin_app(fun, t, runtime)?;

                    self.stack.push(MachineStep::Return(context, res));

                    Ok(())
                } else {
                    Err(Error::UnexpectedBuiltinTermArgument(t.as_ref().clone()))
                }
            }
            rest => Err(Error::NonFunctionalApplication(rest)),
        }
    }

    fn eval_builtin_app(
        &mut self,
        fun: DefaultFunction,
        term: Rc<Term<NamedDeBruijn>>,
        runtime: BuiltinRuntime,
    ) -> Result<Value, Error> {
        if runtime.is_ready() {
            let cost = runtime.to_ex_budget(&self.costs.builtin_costs);

            self.spend_budget(cost)?;

            runtime.call(&mut self.logs)
        } else {
            Ok(Value::Builtin { fun, term, runtime })
        }
    }

    fn lookup_var(&mut self, name: &NamedDeBruijn, env: Rc<Vec<Value>>) -> Result<Value, Error> {
        env.get::<usize>(env.len() - usize::from(name.index))
            .cloned()
            .ok_or_else(|| Error::OpenTermEvaluated(Term::Var(name.clone())))
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

            unspent_step_budget.occurences(self.unbudgeted_steps[i] as i64);

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

#[derive(Clone)]
enum Context {
    FrameApplyFun(Value, Rc<Context>),
    FrameApplyArg(Rc<Vec<Value>>, Rc<Term<NamedDeBruijn>>, Rc<Context>),
    FrameForce(Rc<Context>),
    NoFrame,
}

#[derive(Clone, Debug)]
pub enum Value {
    Con(Constant),
    Delay(Rc<Term<NamedDeBruijn>>, Rc<Vec<Value>>),
    Lambda {
        parameter_name: NamedDeBruijn,
        body: Rc<Term<NamedDeBruijn>>,
        env: Rc<Vec<Value>>,
    },
    Builtin {
        fun: DefaultFunction,
        term: Rc<Term<NamedDeBruijn>>,
        runtime: BuiltinRuntime,
    },
}

impl Value {
    pub fn is_integer(&self) -> bool {
        matches!(self, Value::Con(Constant::Integer(_)))
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, Value::Con(Constant::Bool(_)))
    }

    pub fn to_ex_mem(&self) -> i64 {
        match self {
            Value::Con(c) => match c {
                Constant::Integer(i) => {
                    if *i == 0 {
                        1
                    } else {
                        ((i.abs() as f64).log2().floor() as i64 / 64) + 1
                    }
                }
                Constant::ByteString(b) => (((b.len() as i64 - 1) / 8) + 1),
                Constant::String(s) => s.chars().count() as i64,
                Constant::Unit => 1,
                Constant::Bool(_) => 1,
                Constant::ProtoList(_, _) => todo!(),
                Constant::ProtoPair(_, _, _, _) => todo!(),
                Constant::Data(_) => todo!(),
            },
            Value::Delay(_, _) => 1,
            Value::Lambda { .. } => 1,
            Value::Builtin { .. } => 1,
        }
    }

    pub fn expect_type(&self, r#type: Type) -> Result<(), Error> {
        match self {
            Value::Con(constant) => {
                let constant_type = Type::from(constant);

                if constant_type == r#type {
                    Ok(())
                } else {
                    Err(Error::TypeMismatch(r#type, constant_type))
                }
            }
            rest => Err(Error::NotAConstant(rest.clone())),
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
            Constant::ProtoList(t, _) => Type::List(Box::new(t.clone())),
            Constant::ProtoPair(t1, t2, _, _) => {
                Type::Pair(Box::new(t1.clone()), Box::new(t2.clone()))
            }
            Constant::Data(_) => todo!(),
        }
    }
}
