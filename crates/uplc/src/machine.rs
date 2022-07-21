use crate::{
    ast::{Constant, NamedDeBruijn, Term},
    builtins::DefaultFunction,
};

pub mod cost_model;
mod error;
mod runtime;

use cost_model::{ExBudget, StepKind};
pub use error::Error;

use self::{cost_model::CostModel, error::Type, runtime::BuiltinRuntime};

pub struct Machine {
    costs: CostModel,
    pub ex_budget: ExBudget,
    frames: Vec<Context>,
    slippage: u32,
    env: Vec<Value>,
    unbudgeted_steps: [u32; 8],
    pub logs: Vec<String>,
}

impl Machine {
    pub fn new(costs: CostModel, initial_budget: ExBudget, slippage: u32) -> Machine {
        Machine {
            costs,
            ex_budget: initial_budget,
            slippage,
            frames: vec![Context::NoFrame],
            env: vec![],
            unbudgeted_steps: [0; 8],
            logs: vec![],
        }
    }

    pub fn run(&mut self, term: &Term<NamedDeBruijn>) -> Result<Term<NamedDeBruijn>, Error> {
        let startup_budget = self.costs.machine_costs.get(StepKind::StartUp);

        self.spend_budget(startup_budget)?;

        self.compute(term)
    }

    fn compute(&mut self, term: &Term<NamedDeBruijn>) -> Result<Term<NamedDeBruijn>, Error> {
        match term {
            Term::Var(name) => {
                self.step_and_maybe_spend(StepKind::Var)?;

                let val = self.lookup_var(name.clone())?;

                self.return_compute(val)
            }
            Term::Delay(body) => {
                self.step_and_maybe_spend(StepKind::Delay)?;

                self.return_compute(Value::Delay(*body.clone()))
            }
            Term::Lambda {
                parameter_name,
                body,
            } => {
                self.step_and_maybe_spend(StepKind::Lambda)?;

                self.return_compute(Value::Lambda {
                    parameter_name: parameter_name.clone(),
                    body: *body.clone(),
                })
            }
            Term::Apply { function, argument } => {
                self.step_and_maybe_spend(StepKind::Apply)?;

                self.push_frame(Context::FrameApplyArg(*argument.clone()));

                self.compute(function)
            }
            Term::Constant(x) => {
                self.step_and_maybe_spend(StepKind::Constant)?;

                self.return_compute(Value::Con(x.clone()))
            }
            Term::Force(body) => {
                self.step_and_maybe_spend(StepKind::Force)?;

                self.push_frame(Context::FrameForce);

                self.compute(body)
            }
            Term::Error => Err(Error::EvaluationFailure),
            Term::Builtin(fun) => {
                self.step_and_maybe_spend(StepKind::Builtin)?;

                let runtime: BuiltinRuntime = (*fun).into();

                self.return_compute(Value::Builtin {
                    fun: *fun,
                    term: term.clone(),
                    runtime,
                })
            }
        }
    }

    fn return_compute(&mut self, value: Value) -> Result<Term<NamedDeBruijn>, Error> {
        // avoid unwrap if possible and just return an err when None
        // but honestly it should never be empty anyways because Machine
        // is initialized with `Context::NoFrame`.
        let frame = self.frames.last().cloned().unwrap();

        match frame {
            Context::FrameApplyFun(function) => {
                self.pop_frame();

                self.apply_evaluate(function, value)
            }
            Context::FrameApplyArg(arg) => {
                self.pop_frame();

                self.push_frame(Context::FrameApplyFun(value));

                self.compute(&arg)
            }
            Context::FrameForce => {
                self.pop_frame();

                self.force_evaluate(value)
            }
            Context::NoFrame => {
                if self.unbudgeted_steps[7] > 0 {
                    self.spend_unbudgeted_steps()?;
                }

                let term = self.discharge_value(value);

                Ok(term)
            }
        }
    }

    fn discharge_value(&mut self, value: Value) -> Term<NamedDeBruijn> {
        match value {
            Value::Con(x) => Term::Constant(x),
            Value::Builtin { term, .. } => term,
            Value::Delay(body) => self.discharge_value_env(Term::Delay(Box::new(body))),
            Value::Lambda {
                parameter_name,
                body,
            } => self.discharge_value_env(Term::Lambda {
                parameter_name: NamedDeBruijn {
                    text: parameter_name.text,
                    index: 0.into(),
                },
                body: Box::new(body),
            }),
        }
    }

    fn discharge_value_env(&mut self, term: Term<NamedDeBruijn>) -> Term<NamedDeBruijn> {
        fn rec(lam_cnt: usize, t: Term<NamedDeBruijn>, this: &mut Machine) -> Term<NamedDeBruijn> {
            match t {
                Term::Var(name) => {
                    let index: usize = name.index.into();
                    if lam_cnt >= index {
                        Term::Var(name)
                    } else {
                        this.env
                            .get::<usize>(index - lam_cnt - 1)
                            .cloned()
                            .map_or(Term::Var(name), |v| this.discharge_value(v))
                    }
                }
                Term::Lambda {
                    parameter_name,
                    body,
                } => Term::Lambda {
                    parameter_name,
                    body: Box::new(rec(lam_cnt + 1, *body, this)),
                },
                Term::Apply { function, argument } => Term::Apply {
                    function: Box::new(rec(lam_cnt, *function, this)),
                    argument: Box::new(rec(lam_cnt, *argument, this)),
                },

                Term::Delay(x) => Term::Delay(Box::new(rec(lam_cnt, *x, this))),
                Term::Force(x) => Term::Force(Box::new(rec(lam_cnt, *x, this))),
                rest => rest,
            }
        }
        rec(0, term, self)
    }

    fn force_evaluate(&mut self, value: Value) -> Result<Term<NamedDeBruijn>, Error> {
        match value {
            Value::Delay(body) => self.compute(&body),
            Value::Builtin {
                fun,
                term,
                mut runtime,
            } => {
                let force_term = Term::Force(Box::new(term));

                if runtime.needs_force() {
                    runtime.consume_force();

                    let res = self.eval_builtin_app(fun, force_term, runtime)?;

                    self.return_compute(res)
                } else {
                    Err(Error::BuiltinTermArgumentExpected(force_term))
                }
            }
            rest => Err(Error::NonPolymorphicInstantiation(rest)),
        }
    }

    fn apply_evaluate(
        &mut self,
        function: Value,
        argument: Value,
    ) -> Result<Term<NamedDeBruijn>, Error> {
        match function {
            Value::Lambda { body, .. } => {
                self.env.push(argument);
                let term = self.compute(&body)?;
                self.env.pop();
                Ok(term)
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

                    self.return_compute(res)
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

    fn push_frame(&mut self, frame: Context) {
        self.frames.push(frame);
    }

    fn pop_frame(&mut self) {
        self.frames.pop();
    }

    fn lookup_var(&mut self, name: NamedDeBruijn) -> Result<Value, Error> {
        self.env
            .get::<usize>(usize::from(name.index) - 1)
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
    FrameApplyFun(Value),
    FrameApplyArg(Term<NamedDeBruijn>),
    FrameForce,
    NoFrame,
}

#[derive(Clone, Debug)]
pub enum Value {
    Con(Constant),
    Delay(Term<NamedDeBruijn>),
    Lambda {
        parameter_name: NamedDeBruijn,
        body: Term<NamedDeBruijn>,
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
            },
            Value::Delay(_) => 1,
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
        }
    }
}
