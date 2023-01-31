use std::{collections::VecDeque, ops::Deref, rc::Rc};

use crate::{
    ast::{Constant, NamedDeBruijn, Term, Type},
    builtins::DefaultFunction,
};

pub mod cost_model;
mod error;
pub mod runtime;

use cost_model::{ExBudget, StepKind};
pub use error::Error;
use pallas_primitives::babbage::{BigInt, Language, PlutusData};

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

#[derive(Clone)]
enum PartialTerm {
    // tag: 0
    // Var(NamedDeBruijn),
    // tag: 1
    Delay,
    // tag: 2
    Lambda(Rc<NamedDeBruijn>),
    // tag: 3
    Apply,
    // tag: 4
    // Constant(Constant),
    // tag: 5
    Force,
    // tag: 6
    // Error,
    // tag: 7
    // Builtin(DefaultFunction),
}

#[derive(Clone)]
enum DischargeStep {
    DischargeValue(Value),
    DischargeValueEnv(usize, Rc<Vec<Value>>, Rc<Term<NamedDeBruijn>>),
    PopArgStack(PartialTerm),
}

pub struct Machine {
    costs: CostModel,
    pub ex_budget: ExBudget,
    slippage: u32,
    unbudgeted_steps: [u32; 8],
    pub logs: Vec<String>,
    stack: Vec<MachineStep>,
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
            stack: vec![],
            version,
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

                let term = discharge_value(value);

                self.stack.push(MachineStep::Done(term));
            }
        };

        Ok(())
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
                let arg_term = discharge_value(argument.clone());

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

    fn lookup_var(&mut self, name: &NamedDeBruijn, env: Rc<Vec<Value>>) -> Result<Value, Error> {
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

fn discharge_value(value: Value) -> Rc<Term<NamedDeBruijn>> {
    let mut stack = vec![DischargeStep::DischargeValue(value)];
    let mut arg_stack = vec![];
    while let Some(stack_frame) = stack.pop() {
        match stack_frame {
            DischargeStep::DischargeValue(value) => match value {
                Value::Con(x) => arg_stack.push(Term::Constant(x).into()),
                Value::Builtin { term, .. } => arg_stack.push(term.clone()),
                Value::Delay(body, env) => {
                    stack.push(DischargeStep::DischargeValueEnv(
                        0,
                        env,
                        Term::Delay(body).into(),
                    ));
                }
                Value::Lambda {
                    parameter_name,
                    body,
                    env,
                } => {
                    stack.push(DischargeStep::DischargeValueEnv(
                        0,
                        env,
                        Term::Lambda {
                            parameter_name: parameter_name.clone(),
                            body,
                        }
                        .into(),
                    ));
                }
            },
            DischargeStep::DischargeValueEnv(lam_cnt, env, term) => match term.as_ref() {
                Term::Var(name) => {
                    let index: usize = name.index.into();

                    if lam_cnt >= index {
                        arg_stack.push(Rc::new(Term::Var(name.clone())));
                    } else {
                        let env = env.get::<usize>(env.len() - (index - lam_cnt)).cloned();
                        if let Some(v) = env {
                            stack.push(DischargeStep::DischargeValue(v));
                        } else {
                            arg_stack.push(Rc::new(Term::Var(name.clone())));
                        }
                    }
                }
                Term::Lambda {
                    parameter_name,
                    body,
                } => {
                    stack.push(DischargeStep::PopArgStack(PartialTerm::Lambda(
                        parameter_name.to_owned(),
                    )));
                    stack.push(DischargeStep::DischargeValueEnv(
                        lam_cnt + 1,
                        env,
                        body.to_owned(),
                    ));
                }
                Term::Apply { function, argument } => {
                    stack.push(DischargeStep::PopArgStack(PartialTerm::Apply));
                    stack.push(DischargeStep::DischargeValueEnv(
                        lam_cnt,
                        env.clone(),
                        function.to_owned(),
                    ));
                    stack.push(DischargeStep::DischargeValueEnv(
                        lam_cnt,
                        env,
                        argument.to_owned(),
                    ));
                }
                Term::Delay(body) => {
                    stack.push(DischargeStep::PopArgStack(PartialTerm::Delay));
                    stack.push(DischargeStep::DischargeValueEnv(
                        lam_cnt,
                        env.clone(),
                        body.to_owned(),
                    ));
                }

                Term::Force(body) => {
                    stack.push(DischargeStep::PopArgStack(PartialTerm::Force));
                    stack.push(DischargeStep::DischargeValueEnv(
                        lam_cnt,
                        env.clone(),
                        body.to_owned(),
                    ));
                }
                rest => {
                    arg_stack.push(rest.to_owned().into());
                }
            },
            DischargeStep::PopArgStack(term) => match term {
                PartialTerm::Delay => {
                    let body = arg_stack.pop().unwrap();
                    arg_stack.push(Term::Delay(body).into())
                }
                PartialTerm::Lambda(parameter_name) => {
                    let body = arg_stack.pop().unwrap();
                    arg_stack.push(
                        Term::Lambda {
                            parameter_name,
                            body,
                        }
                        .into(),
                    )
                }
                PartialTerm::Apply => {
                    let argument = arg_stack.pop().unwrap();
                    let function = arg_stack.pop().unwrap();
                    arg_stack.push(Term::Apply { function, argument }.into());
                }
                PartialTerm::Force => {
                    let body = arg_stack.pop().unwrap();
                    arg_stack.push(Term::Force(body).into())
                }
            },
        }
    }

    arg_stack.pop().unwrap()
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
    Con(Rc<Constant>),
    Delay(Rc<Term<NamedDeBruijn>>, Rc<Vec<Value>>),
    Lambda {
        parameter_name: Rc<NamedDeBruijn>,
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
        matches!(self, Value::Con(i) if matches!(i.as_ref(), Constant::Integer(_)))
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, Value::Con(b) if matches!(b.as_ref(), Constant::Bool(_)))
    }

    // TODO: Make this to_ex_mem not recursive.
    pub fn to_ex_mem(&self) -> i64 {
        match self {
            Value::Con(c) => match c.as_ref() {
                Constant::Integer(i) => {
                    if *i == 0 {
                        1
                    } else {
                        ((i.abs() as f64).log2().floor() as i64 / 64) + 1
                    }
                }
                Constant::ByteString(b) => {
                    if b.is_empty() {
                        1
                    } else {
                        ((b.len() as i64 - 1) / 8) + 1
                    }
                }
                Constant::String(s) => s.chars().count() as i64,
                Constant::Unit => 1,
                Constant::Bool(_) => 1,
                Constant::ProtoList(_, items) => items.iter().fold(0, |acc, constant| {
                    acc + Value::Con(constant.clone().into()).to_ex_mem()
                }),
                Constant::ProtoPair(_, _, l, r) => {
                    Value::Con(l.clone()).to_ex_mem() + Value::Con(r.clone()).to_ex_mem()
                }
                Constant::Data(item) => self.data_to_ex_mem(item),
            },
            Value::Delay(_, _) => 1,
            Value::Lambda { .. } => 1,
            Value::Builtin { .. } => 1,
        }
    }

    // I made data not recursive since data tends to be deeply nested
    // thus causing a significant hit on performance
    pub fn data_to_ex_mem(&self, data: &PlutusData) -> i64 {
        let mut stack: VecDeque<&PlutusData> = VecDeque::new();
        let mut total = 0;
        stack.push_front(data);
        while let Some(item) = stack.pop_front() {
            // each time we deconstruct a data we add 4 memory units
            total += 4;
            match item {
                PlutusData::Constr(c) => {
                    // note currently tag is not factored into cost of memory
                    // create new stack with of items from the list of data
                    let mut new_stack: VecDeque<&PlutusData> =
                        VecDeque::from_iter(c.fields.deref().iter());
                    // Append old stack to the back of the new stack
                    new_stack.append(&mut stack);
                    stack = new_stack;
                }
                PlutusData::Map(m) => {
                    let mut new_stack: VecDeque<&PlutusData>;
                    // create new stack with of items from the list of pairs of data
                    new_stack = m.iter().fold(VecDeque::new(), |mut acc, d| {
                        acc.push_back(&d.0);
                        acc.push_back(&d.1);
                        acc
                    });
                    // Append old stack to the back of the new stack
                    new_stack.append(&mut stack);
                    stack = new_stack;
                }
                PlutusData::BigInt(i) => {
                    if let BigInt::Int(g) = i {
                        let numb: i128 = (*g).try_into().unwrap();
                        total += Value::Con(Constant::Integer(numb).into()).to_ex_mem();
                    } else {
                        unreachable!()
                    };
                }
                PlutusData::BoundedBytes(b) => {
                    let byte_string: Vec<u8> = b.deref().clone();
                    total += Value::Con(Constant::ByteString(byte_string).into()).to_ex_mem();
                }
                PlutusData::Array(a) => {
                    // create new stack with of items from the list of data
                    let mut new_stack: VecDeque<&PlutusData> =
                        VecDeque::from_iter(a.deref().iter());
                    // Append old stack to the back of the new stack
                    new_stack.append(&mut stack);
                    stack = new_stack;
                }
            }
        }
        total
    }

    pub fn expect_type(&self, r#type: Type) -> Result<(), Error> {
        let constant: Constant = self.clone().try_into()?;

        let constant_type = Type::from(&constant);

        if constant_type == r#type {
            Ok(())
        } else {
            Err(Error::TypeMismatch(r#type, constant_type))
        }
    }

    pub fn expect_list(&self) -> Result<(), Error> {
        let constant: Constant = self.clone().try_into()?;

        let constant_type = Type::from(&constant);

        if matches!(constant_type, Type::List(_)) {
            Ok(())
        } else {
            Err(Error::ListTypeMismatch(constant_type))
        }
    }

    pub fn expect_pair(&self) -> Result<(), Error> {
        let constant: Constant = self.clone().try_into()?;

        let constant_type = Type::from(&constant);

        if matches!(constant_type, Type::Pair(_, _)) {
            Ok(())
        } else {
            Err(Error::PairTypeMismatch(constant_type))
        }
    }
}

impl TryFrom<Value> for Type {
    type Error = Error;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        let constant: Constant = value.try_into()?;

        let constant_type = Type::from(&constant);

        Ok(constant_type)
    }
}

impl TryFrom<Value> for Constant {
    type Error = Error;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Con(constant) => Ok(constant.as_ref().clone()),
            rest => Err(Error::NotAConstant(rest)),
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
