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
    Return(Context, Value),
    Compute(Context, Vec<Value>, Term<NamedDeBruijn>),
    Done(Term<NamedDeBruijn>),
}

impl TryFrom<Option<MachineStep>> for Term<NamedDeBruijn> {
    type Error = Error;

    fn try_from(value: Option<MachineStep>) -> Result<Self, Error> {
        match value {
            Some(MachineStep::Done(term)) => Ok(term),
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

        self.stack
            .push(Compute(Context::NoFrame, vec![], term.clone()));

        while let Some(step) = self.stack.pop() {
            match step {
                Compute(context, env, t) => {
                    self.compute(context, env, &t)?;
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
        context: Context,
        env: Vec<Value>,
        term: &Term<NamedDeBruijn>,
    ) -> Result<(), Error> {
        match term {
            Term::Var(name) => {
                self.step_and_maybe_spend(StepKind::Var)?;

                let val = self.lookup_var(name.clone(), env)?;

                self.stack.push(MachineStep::Return(context, val));
            }
            Term::Delay(body) => {
                self.step_and_maybe_spend(StepKind::Delay)?;

                self.stack.push(MachineStep::Return(
                    context,
                    Value::Delay(*body.clone(), env),
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
                        body: *body.clone(),
                        env,
                    },
                ));
            }
            Term::Apply { function, argument } => {
                self.step_and_maybe_spend(StepKind::Apply)?;

                self.stack.push(MachineStep::Compute(
                    Context::FrameApplyArg(env.clone(), *argument.clone(), Box::new(context)),
                    env,
                    *function.clone(),
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
                    Context::FrameForce(Box::new(context)),
                    env,
                    *body.clone(),
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
                        term: term.clone(),
                        runtime,
                    },
                ));
            }
        };

        Ok(())
    }

    fn return_compute(&mut self, context: Context, value: Value) -> Result<(), Error> {
        match context {
            Context::FrameApplyFun(function, ctx) => self.apply_evaluate(*ctx, function, value)?,
            Context::FrameApplyArg(arg_var_env, arg, ctx) => {
                self.stack.push(MachineStep::Compute(
                    Context::FrameApplyFun(value, ctx),
                    arg_var_env,
                    arg,
                ));
            }
            Context::FrameForce(ctx) => self.force_evaluate(*ctx, value)?,
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

    fn discharge_value(&mut self, value: Value) -> Term<NamedDeBruijn> {
        match value {
            Value::Con(x) => Term::Constant(x),
            Value::Builtin { term, .. } => term,
            Value::Delay(body, env) => self.discharge_value_env(env, Term::Delay(Box::new(body))),
            Value::Lambda {
                parameter_name,
                body,
                env,
            } => self.discharge_value_env(
                env,
                Term::Lambda {
                    parameter_name: NamedDeBruijn {
                        text: parameter_name.text,
                        index: 0.into(),
                    },
                    body: Box::new(body),
                },
            ),
        }
    }

    fn discharge_value_env(
        &mut self,
        env: Vec<Value>,
        term: Term<NamedDeBruijn>,
    ) -> Term<NamedDeBruijn> {
        fn rec(
            lam_cnt: usize,
            t: Term<NamedDeBruijn>,
            this: &mut Machine,
            env: &[Value],
        ) -> Term<NamedDeBruijn> {
            match t {
                Term::Var(name) => {
                    let index: usize = name.index.into();
                    if lam_cnt >= index {
                        Term::Var(name)
                    } else {
                        env.get::<usize>(env.len() - (index - lam_cnt))
                            .cloned()
                            .map_or(Term::Var(name), |v| this.discharge_value(v))
                    }
                }
                Term::Lambda {
                    parameter_name,
                    body,
                } => Term::Lambda {
                    parameter_name,
                    body: Box::new(rec(lam_cnt + 1, *body, this, env)),
                },
                Term::Apply { function, argument } => Term::Apply {
                    function: Box::new(rec(lam_cnt, *function, this, env)),
                    argument: Box::new(rec(lam_cnt, *argument, this, env)),
                },

                Term::Delay(x) => Term::Delay(Box::new(rec(lam_cnt, *x, this, env))),
                Term::Force(x) => Term::Force(Box::new(rec(lam_cnt, *x, this, env))),
                rest => rest,
            }
        }
        rec(0, term, self, &env)
    }

    fn force_evaluate(&mut self, context: Context, value: Value) -> Result<(), Error> {
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
                let force_term = Term::Force(Box::new(term));

                if runtime.needs_force() {
                    runtime.consume_force();

                    let res = self.eval_builtin_app(fun, force_term, runtime)?;

                    self.stack.push(MachineStep::Return(context, res));

                    Ok(())
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
    ) -> Result<(), Error> {
        match function {
            Value::Lambda { body, env, .. } => {
                let mut e = env;

                e.push(argument);

                self.stack.push(MachineStep::Compute(context, e, body));

                Ok(())
            }
            Value::Builtin {
                fun,
                term,
                mut runtime,
            } => {
                let arg_term = self.discharge_value(argument.clone());

                let t = Term::<NamedDeBruijn>::Apply {
                    function: Box::new(term),
                    argument: Box::new(arg_term),
                };

                if runtime.is_arrow() && !runtime.needs_force() {
                    runtime.push(argument)?;

                    let res = self.eval_builtin_app(fun, t, runtime)?;

                    self.stack.push(MachineStep::Return(context, res));

                    Ok(())
                } else {
                    Err(Error::UnexpectedBuiltinTermArgument(t))
                }
            }
            rest => Err(Error::NonFunctionalApplication(rest)),
        }
    }

    fn eval_builtin_app(
        &mut self,
        fun: DefaultFunction,
        term: Term<NamedDeBruijn>,
        runtime: BuiltinRuntime,
    ) -> Result<Value, Error> {
        if runtime.is_ready() {
            let cost = runtime.to_ex_budget(&self.costs.builtin_costs);

            self.spend_budget(cost)?;

            runtime.call()
        } else {
            Ok(Value::Builtin { fun, term, runtime })
        }
    }

    fn lookup_var(&mut self, name: NamedDeBruijn, env: Vec<Value>) -> Result<Value, Error> {
        env.get::<usize>(env.len() - usize::from(name.index))
            .cloned()
            .ok_or(Error::OpenTermEvaluated(Term::Var(name)))
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
    FrameApplyFun(Value, Box<Context>),
    FrameApplyArg(Vec<Value>, Term<NamedDeBruijn>, Box<Context>),
    FrameForce(Box<Context>),
    NoFrame,
}

#[derive(Clone, Debug)]
pub enum Value {
    Con(Constant),
    Delay(Term<NamedDeBruijn>, Vec<Value>),
    Lambda {
        parameter_name: NamedDeBruijn,
        body: Term<NamedDeBruijn>,
        env: Vec<Value>,
    },
    Builtin {
        fun: DefaultFunction,
        term: Term<NamedDeBruijn>,
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
                Constant::ByteString(b) => (((b.len() - 1) / 8) + 1) as i64,
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
