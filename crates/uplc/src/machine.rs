use bumpalo::{collections::Vec as BumpVec, Bump};
use num_traits::sign::Signed;
use std::{collections::VecDeque, ops::Deref, rc::Rc};

use crate::{
    ast::{Constant, NamedDeBruijn, Term, Type},
    builtins::DefaultFunction,
};

pub mod cost_model;
mod error;
pub mod eval_result;
pub mod runtime;

use cost_model::{ExBudget, StepKind};
pub use error::Error;
use num_bigint::BigInt;
use pallas_primitives::babbage::{self as pallas, Language, PlutusData};

use self::{cost_model::CostModel, runtime::BuiltinRuntime};

enum MachineStep<'a> {
    Return(&'a Context<'a>, &'a Value<'a>),
    Compute(
        &'a Context<'a>,
        Rc<Vec<&'a Value<'a>>>,
        &'a Term<NamedDeBruijn>,
    ),
    Done(&'a Term<NamedDeBruijn>),
}

impl<'a> TryFrom<Option<MachineStep<'a>>> for Term<NamedDeBruijn> {
    type Error = Error;

    fn try_from(value: Option<MachineStep>) -> Result<Self, Error> {
        match value {
            Some(MachineStep::Done(term)) => Ok(term.clone()),
            _ => Err(Error::MachineNeverReachedDone),
        }
    }
}

#[derive(Clone)]
enum PartialTerm<'a> {
    Delay,
    Lambda(&'a NamedDeBruijn),
    Apply,
    Force,
}

#[derive(Clone)]
enum DischargeStep<'a> {
    DischargeValue(&'a Value<'a>),
    DischargeValueEnv(usize, Rc<Vec<&'a Value<'a>>>, &'a Term<NamedDeBruijn>),
    PopArgStack(PartialTerm<'a>),
}

pub struct Machine<'a> {
    arena: &'a Bump,
    costs: CostModel,
    pub ex_budget: ExBudget,
    slippage: u32,
    unbudgeted_steps: [u32; 8],
    pub logs: BumpVec<'a, String>,
    stack: BumpVec<'a, MachineStep<'a>>,
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
        let logs = BumpVec::new_in(arena);
        let stack = BumpVec::new_in(arena);

        Machine {
            arena,
            costs,
            ex_budget: initial_budget,
            slippage,
            unbudgeted_steps: [0; 8],
            logs,
            stack,
            version,
        }
    }

    pub fn run(&mut self, term: &Term<NamedDeBruijn>) -> Result<Term<NamedDeBruijn>, Error> {
        use MachineStep::*;

        let startup_budget = self.costs.machine_costs.get(StepKind::StartUp);

        self.spend_budget(startup_budget)?;

        self.stack.push(Compute(
            self.arena.alloc(Context::NoFrame),
            Rc::new(vec![]),
            term,
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
        context: &'a Context,
        env: Rc<Vec<&'a Value>>,
        term: &'a Term<NamedDeBruijn>,
    ) -> Result<(), Error> {
        match term {
            Term::Var(name) => {
                self.step_and_maybe_spend(StepKind::Var)?;

                let val = self.lookup_var(name, &env)?;

                self.stack.push(MachineStep::Return(context, val));
            }
            Term::Delay(body) => {
                self.step_and_maybe_spend(StepKind::Delay)?;

                let value = self.arena.alloc(Value::Delay(body.as_ref(), env));

                self.stack.push(MachineStep::Return(context, value));
            }
            Term::Lambda {
                parameter_name,
                body,
            } => {
                self.step_and_maybe_spend(StepKind::Lambda)?;

                let value = self.arena.alloc(Value::Lambda {
                    parameter_name: parameter_name.as_ref(),
                    body: body.as_ref(),
                    env,
                });

                self.stack.push(MachineStep::Return(context, value));
            }
            Term::Apply { function, argument } => {
                self.step_and_maybe_spend(StepKind::Apply)?;

                let context =
                    self.arena
                        .alloc(Context::FrameApplyArg(env, argument.as_ref(), context));

                self.stack
                    .push(MachineStep::Compute(context, env, function.as_ref()));
            }
            Term::Constant(x) => {
                self.step_and_maybe_spend(StepKind::Constant)?;

                let value = self.arena.alloc(Value::Con(x));

                self.stack.push(MachineStep::Return(context, value));
            }
            Term::Force(body) => {
                self.step_and_maybe_spend(StepKind::Force)?;

                let context = self.arena.alloc(Context::FrameForce(context));

                self.stack
                    .push(MachineStep::Compute(context, env, body.as_ref()));
            }
            Term::Error => return Err(Error::EvaluationFailure),
            Term::Builtin(fun) => {
                self.step_and_maybe_spend(StepKind::Builtin)?;

                let value = self.arena.alloc(Value::Builtin {
                    fun: *fun,
                    term,
                    runtime: self.arena.alloc((*fun).into()),
                });

                self.stack.push(MachineStep::Return(context, value));
            }
        };

        Ok(())
    }

    fn return_compute(&mut self, context: &'a Context, value: &'a Value) -> Result<(), Error> {
        match context {
            Context::FrameApplyFun(function, ctx) => {
                self.apply_evaluate(ctx, &mut function, value)?
            }
            Context::FrameApplyArg(arg_var_env, arg, ctx) => {
                let context = self.arena.alloc(Context::FrameApplyFun(value, ctx));

                self.stack
                    .push(MachineStep::Compute(context, arg_var_env.to_owned(), arg));
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

    fn force_evaluate(&mut self, context: &'a Context, value: &mut Value) -> Result<(), Error> {
        match value {
            Value::Delay(body, env) => {
                self.stack
                    .push(MachineStep::Compute(context, env.clone(), body));

                Ok(())
            }
            Value::Builtin {
                fun,
                term,
                mut runtime,
            } => {
                let force_term = Rc::new(Term::Force(term.clone()));

                if runtime.needs_force() {
                    runtime.consume_force();

                    let res = self.eval_builtin_app(*fun, force_term, runtime)?;

                    self.stack.push(MachineStep::Return(context, res));

                    Ok(())
                } else {
                    Err(Error::BuiltinTermArgumentExpected(
                        force_term.as_ref().clone(),
                    ))
                }
            }
            rest => Err(Error::NonPolymorphicInstantiation(rest.clone())),
        }
    }

    fn apply_evaluate(
        &mut self,
        context: &'a Context,
        function: &'a mut Value,
        argument: &'a Value,
    ) -> Result<(), Error> {
        match function {
            Value::Lambda { body, env, .. } => {
                let e = Rc::make_mut(env);

                e.push(argument);

                self.stack
                    .push(MachineStep::Compute(context, Rc::new(e.clone()), body));

                Ok(())
            }
            Value::Builtin { fun, term, runtime } => {
                let arg_term = discharge_value(argument.clone());

                let t = Rc::new(Term::<NamedDeBruijn>::Apply {
                    function: term.clone(),
                    argument: arg_term,
                });

                let mut_runtime = Rc::make_mut(runtime);

                if mut_runtime.is_arrow() && !mut_runtime.needs_force() {
                    mut_runtime.push(argument)?;

                    let res = self.eval_builtin_app(*fun, t, runtime.to_owned())?;

                    self.stack.push(MachineStep::Return(context, res));

                    Ok(())
                } else {
                    Err(Error::UnexpectedBuiltinTermArgument(t.as_ref().clone()))
                }
            }
            rest => Err(Error::NonFunctionalApplication(
                rest.clone(),
                argument.as_ref().clone(),
            )),
        }
    }

    fn eval_builtin_app(
        &mut self,
        fun: DefaultFunction,
        term: &'a Term<NamedDeBruijn>,
        runtime: &'a BuiltinRuntime,
    ) -> Result<&'a Value<'a>, Error> {
        if runtime.is_ready() {
            let cost = match self.version {
                Language::PlutusV1 => runtime.to_ex_budget_v1(&self.costs.builtin_costs),
                Language::PlutusV2 => runtime.to_ex_budget_v2(&self.costs.builtin_costs),
            };

            self.spend_budget(cost)?;

            runtime.call(self.arena, &mut self.logs)
        } else {
            let value = self.arena.alloc(Value::Builtin { fun, term, runtime });

            Ok(value)
        }
    }

    fn lookup_var(&mut self, name: &NamedDeBruijn, env: &[&'a Value]) -> Result<&'a Value, Error> {
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

fn discharge_value<'a>(arena: &'a Bump, value: &'a Value<'a>) -> &'a Term<NamedDeBruijn> {
    let mut stack = vec![DischargeStep::DischargeValue(value)];

    let mut arg_stack = vec![];

    while let Some(stack_frame) = stack.pop() {
        match stack_frame {
            DischargeStep::DischargeValue(value) => match value {
                Value::Con(x) => arg_stack.push(Term::Constant(x.clone()).into()),
                Value::Builtin { term, .. } => arg_stack.push(term.clone()),
                Value::Delay(body, env) => {
                    stack.push(DischargeStep::DischargeValueEnv(
                        0,
                        env.clone(),
                        Term::Delay(body.clone()).into(),
                    ));
                }
                Value::Lambda {
                    parameter_name,
                    body,
                    env,
                } => {
                    stack.push(DischargeStep::DischargeValueEnv(
                        0,
                        env.clone(),
                        Term::Lambda {
                            parameter_name: parameter_name.clone(),
                            body: body.clone(),
                        }
                        .into(),
                    ));
                }
            },
            DischargeStep::DischargeValueEnv(lam_cnt, env, term) => match term {
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
                        argument.as_ref(),
                    ));
                    stack.push(DischargeStep::DischargeValueEnv(
                        lam_cnt,
                        env,
                        function.as_ref(),
                    ));
                }
                Term::Delay(body) => {
                    stack.push(DischargeStep::PopArgStack(PartialTerm::Delay));

                    stack.push(DischargeStep::DischargeValueEnv(
                        lam_cnt,
                        env.clone(),
                        body.as_ref(),
                    ));
                }

                Term::Force(body) => {
                    stack.push(DischargeStep::PopArgStack(PartialTerm::Force));

                    stack.push(DischargeStep::DischargeValueEnv(
                        lam_cnt,
                        env.clone(),
                        body.as_ref(),
                    ));
                }
                rest => {
                    arg_stack.push(rest);
                }
            },
            DischargeStep::PopArgStack(term) => match term {
                PartialTerm::Delay => {
                    let body = arg_stack.pop().unwrap();

                    arg_stack.push(Term::Delay(body))
                }
                PartialTerm::Lambda(parameter_name) => {
                    let body = arg_stack.pop().unwrap();

                    arg_stack.push(Term::Lambda {
                        parameter_name,
                        body,
                    })
                }
                PartialTerm::Apply => {
                    let argument = arg_stack.pop().unwrap();
                    let function = arg_stack.pop().unwrap();

                    arg_stack.push(Term::Apply { function, argument });
                }
                PartialTerm::Force => {
                    let body = arg_stack.pop().unwrap();

                    arg_stack.push(Term::Force(body))
                }
            },
        }
    }

    arg_stack.pop().expect("arg_stack should not be empty")
}

#[derive(Clone)]
enum Context<'a> {
    FrameApplyFun(&'a Value<'a>, &'a Context<'a>),
    FrameApplyArg(
        Rc<Vec<&'a Value<'a>>>,
        &'a Term<NamedDeBruijn>,
        &'a Context<'a>,
    ),
    FrameForce(&'a Context<'a>),
    NoFrame,
}

#[derive(Clone, Debug)]
pub enum Value<'a> {
    Con(&'a Constant),
    Delay(&'a Term<NamedDeBruijn>, Rc<Vec<&'a Value<'a>>>),
    Lambda {
        parameter_name: &'a NamedDeBruijn,
        body: &'a Term<NamedDeBruijn>,
        env: Rc<Vec<&'a Value<'a>>>,
    },
    Builtin {
        fun: DefaultFunction,
        term: &'a Term<NamedDeBruijn>,
        runtime: &'a BuiltinRuntime,
    },
}

fn integer_log2(i: BigInt) -> i64 {
    let (_, bytes) = i.to_bytes_be();
    match bytes.first() {
        None => unreachable!("empty number?"),
        Some(u) => (8 - u.leading_zeros() - 1) as i64 + 8 * (bytes.len() - 1) as i64,
    }
}

pub fn from_pallas_bigint(n: &pallas::BigInt) -> BigInt {
    match n {
        pallas::BigInt::Int(i) => i128::from(*i).into(),
        pallas::BigInt::BigUInt(bytes) => BigInt::from_bytes_be(num_bigint::Sign::Plus, bytes),
        pallas::BigInt::BigNInt(bytes) => BigInt::from_bytes_be(num_bigint::Sign::Minus, bytes),
    }
}

pub fn to_pallas_bigint(n: &BigInt) -> pallas::BigInt {
    if n.bits() <= 64 {
        let regular_int: i64 = n.try_into().unwrap();
        let pallas_int: pallas_codec::utils::Int = regular_int.into();

        pallas::BigInt::Int(pallas_int)
    } else if n.is_positive() {
        let (_, bytes) = n.to_bytes_be();
        pallas::BigInt::BigUInt(bytes.into())
    } else {
        let (_, bytes) = n.to_bytes_be();
        pallas::BigInt::BigNInt(bytes.into())
    }
}

impl<'a> Value<'a> {
    pub fn is_integer(&self) -> bool {
        matches!(self, Value::Con(i) if matches!(i, Constant::Integer(_)))
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, Value::Con(b) if matches!(b, Constant::Bool(_)))
    }

    // TODO: Make this to_ex_mem not recursive.
    pub fn to_ex_mem(&self, arena: &'a Bump) -> i64 {
        match self {
            Value::Con(c) => match c {
                Constant::Integer(i) => {
                    if *i == 0.into() {
                        1
                    } else {
                        (integer_log2(i.abs()) / 64) + 1
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
                    acc + Value::Con(constant).to_ex_mem(arena)
                }),
                Constant::ProtoPair(_, _, l, r) => {
                    Value::Con(l).to_ex_mem(arena) + Value::Con(r).to_ex_mem(arena)
                }
                Constant::Data(item) => self.data_to_ex_mem(arena, item),
            },
            Value::Delay(_, _) => 1,
            Value::Lambda { .. } => 1,
            Value::Builtin { .. } => 1,
        }
    }

    // I made data not recursive since data tends to be deeply nested
    // thus causing a significant hit on performance
    pub fn data_to_ex_mem(&self, arena: &'a Bump, data: &PlutusData) -> i64 {
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
                    let i = from_pallas_bigint(i);

                    let constant = arena.alloc(Constant::Integer(i));
                    let value = arena.alloc(Value::Con(constant));

                    total += value.to_ex_mem(arena);
                }
                PlutusData::BoundedBytes(b) => {
                    let byte_string: Vec<u8> = b.deref().clone();

                    let constant = arena.alloc(Constant::ByteString(byte_string));
                    let value = arena.alloc(Value::Con(constant));

                    total += value.to_ex_mem(arena);
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

impl<'a> TryFrom<Value<'a>> for Type {
    type Error = Error;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        let constant: Constant = value.try_into()?;

        let constant_type = Type::from(&constant);

        Ok(constant_type)
    }
}

impl<'a> TryFrom<&Value<'a>> for Type {
    type Error = Error;

    fn try_from(value: &Value) -> Result<Self, Self::Error> {
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
            rest => Err(Error::NotAConstant(rest)),
        }
    }
}

impl<'a> TryFrom<&Value<'a>> for Constant {
    type Error = Error;

    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        match value {
            Value::Con(constant) => Ok(*constant.clone()),
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

    use super::{cost_model::ExBudget, integer_log2, Value};
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

    #[test]
    fn to_ex_mem_bigint() {
        let arena = bumpalo::Bump::new();

        let constant = arena.alloc(Constant::Integer(1.into()));

        let value = Value::Con(constant);

        assert_eq!(value.to_ex_mem(&arena), 1);

        let constant = arena.alloc(Constant::Integer(42.into()));

        let value = Value::Con(constant);

        assert_eq!(value.to_ex_mem(&arena), 1);

        let constant = arena.alloc(Constant::Integer(
            BigInt::parse_bytes("18446744073709551615".as_bytes(), 10).unwrap(),
        ));

        let value = Value::Con(constant);

        assert_eq!(value.to_ex_mem(&arena), 1);

        let constant = arena.alloc(Constant::Integer(
            BigInt::parse_bytes("999999999999999999999999999999".as_bytes(), 10).unwrap(),
        ));

        let value = Value::Con(constant);

        assert_eq!(value.to_ex_mem(&arena), 2);

        let constant = arena.alloc(Constant::Integer(
            BigInt::parse_bytes("170141183460469231731687303715884105726".as_bytes(), 10).unwrap(),
        ));

        let value = Value::Con(constant);

        assert_eq!(value.to_ex_mem(&arena), 2);

        let constant = arena.alloc(Constant::Integer(
            BigInt::parse_bytes("170141183460469231731687303715884105727".as_bytes(), 10).unwrap(),
        ));

        let value = Value::Con(constant);

        assert_eq!(value.to_ex_mem(&arena), 2);

        let constant = arena.alloc(Constant::Integer(
            BigInt::parse_bytes("170141183460469231731687303715884105728".as_bytes(), 10).unwrap(),
        ));

        let value = Value::Con(constant);

        assert_eq!(value.to_ex_mem(&arena), 2);

        let constant = arena.alloc(Constant::Integer(
            BigInt::parse_bytes("170141183460469231731687303715884105729".as_bytes(), 10).unwrap(),
        ));

        let value = Value::Con(constant);

        assert_eq!(value.to_ex_mem(&arena), 2);

        let constant = arena.alloc(Constant::Integer(
            BigInt::parse_bytes("340282366920938463463374607431768211458".as_bytes(), 10).unwrap(),
        ));

        let value = Value::Con(constant);

        assert_eq!(value.to_ex_mem(&arena), 3);

        let constant = arena.alloc(Constant::Integer(
            BigInt::parse_bytes("999999999999999999999999999999999999999999".as_bytes(), 10)
                .unwrap(),
        ));

        let value = Value::Con(constant);

        assert_eq!(value.to_ex_mem(&arena), 3);

        let constant = arena.alloc(Constant::Integer(BigInt::parse_bytes("999999999999999999999999999999999999999999999999999999999999999999999999999999999999".as_bytes(), 10).unwrap()));

        let value = Value::Con(constant);

        assert_eq!(value.to_ex_mem(&arena), 5);
    }

    #[test]
    fn integer_log2_oracle() {
        // Values come from the Haskell implementation
        assert_eq!(integer_log2(1.into()), 0);
        assert_eq!(integer_log2(42.into()), 5);
        assert_eq!(
            integer_log2(BigInt::parse_bytes("18446744073709551615".as_bytes(), 10).unwrap()),
            63
        );
        assert_eq!(
            integer_log2(
                BigInt::parse_bytes("999999999999999999999999999999".as_bytes(), 10).unwrap()
            ),
            99
        );
        assert_eq!(
            integer_log2(
                BigInt::parse_bytes("170141183460469231731687303715884105726".as_bytes(), 10)
                    .unwrap()
            ),
            126
        );
        assert_eq!(
            integer_log2(
                BigInt::parse_bytes("170141183460469231731687303715884105727".as_bytes(), 10)
                    .unwrap()
            ),
            126
        );
        assert_eq!(
            integer_log2(
                BigInt::parse_bytes("170141183460469231731687303715884105728".as_bytes(), 10)
                    .unwrap()
            ),
            127
        );
        assert_eq!(
            integer_log2(
                BigInt::parse_bytes("340282366920938463463374607431768211458".as_bytes(), 10)
                    .unwrap()
            ),
            128
        );
        assert_eq!(
            integer_log2(
                BigInt::parse_bytes("999999999999999999999999999999999999999999".as_bytes(), 10)
                    .unwrap()
            ),
            139
        );
        assert_eq!(
            integer_log2(BigInt::parse_bytes("999999999999999999999999999999999999999999999999999999999999999999999999999999999999".as_bytes(), 10).unwrap()),
            279
        );
    }
}
