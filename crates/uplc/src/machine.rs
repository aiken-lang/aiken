use std::{fmt::Display, ptr::NonNull, rc::Rc};

use crate::ast::{Constant, NamedDeBruijn, Term, Type};
use num_traits::ToPrimitive;

pub mod cost_model;
mod discharge;
mod error;
pub mod eval_result;
pub mod runtime;
pub mod value;

use cost_model::{ExBudget, StepKind};
pub use error::Error;
use pallas_primitives::conway::Language;

use self::{
    cost_model::CostModel,
    runtime::{BuiltinRuntime, BuiltinSemantics},
    value::{Env, Value},
};

enum MachineState {
    Return(Value),
    Compute(Env, TermRef),
    Done(Term<NamedDeBruijn>),
}

/// A pending continuation. The machine keeps them on a stack, with the next
/// one to resume on top.
enum Frame {
    AwaitArg(Value),
    AwaitFunTerm(Env, Rc<Term<NamedDeBruijn>>),
    AwaitFunValue(Value),
    Force,
    /// The `constr` term being evaluated, the index of the next field to
    /// compute, and the fields computed so far.
    Constr(Env, TermRef, Vec<Value>),
    /// The `case` term whose scrutinee is being evaluated.
    Cases(Env, TermRef),
}

/// A term to evaluate. The fields of `constr` and the branches of `case`
/// are stored inline rather than behind their own `Rc`, so evaluation
/// addresses them through the `Rc` that owns them instead of cloning each
/// one into a new allocation whenever it is entered.
#[derive(Clone)]
struct TermRef {
    owner: Rc<Term<NamedDeBruijn>>,
    term: NonNull<Term<NamedDeBruijn>>,
}

impl TermRef {
    #[inline(always)]
    fn get(&self) -> &Term<NamedDeBruijn> {
        // SAFETY: `term` points into the allocation kept alive by `owner`.
        // Terms behind an `Rc` are never mutated while shared, and the
        // machine never takes `&mut` to them, so the pointee is stable.
        unsafe { self.term.as_ref() }
    }

    /// The `n`th inline child of this `constr` or `case` term.
    #[inline(always)]
    fn child(&self, n: usize) -> TermRef {
        let (Term::Constr {
            fields: children, ..
        }
        | Term::Case {
            branches: children, ..
        }) = self.get()
        else {
            unreachable!("only constr and case terms have inline children")
        };

        TermRef {
            owner: self.owner.clone(),
            term: NonNull::from(&children[n]),
        }
    }
}

impl From<Rc<Term<NamedDeBruijn>>> for TermRef {
    #[inline(always)]
    fn from(owner: Rc<Term<NamedDeBruijn>>) -> Self {
        let term = NonNull::from(owner.as_ref());

        TermRef { owner, term }
    }
}

pub const TERM_COUNT: usize = 9;
// Builtin tags are sparse; this is the highest tag plus one for debug counters.
pub const BUILTIN_COUNT: usize = 101;

#[derive(Debug, Clone)]
pub enum Trace {
    Log(String),
    Label(String),
}

impl Display for Trace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Trace::Log(log) => f.write_str(log),
            Trace::Label(label) => f.write_str(label),
        }
    }
}

impl Trace {
    pub fn unwrap_log(self) -> Option<String> {
        match self {
            Trace::Log(log) => Some(log),
            _ => None,
        }
    }

    pub fn unwrap_label(self) -> Option<String> {
        match self {
            Trace::Label(label) => Some(label),
            _ => None,
        }
    }
}

pub struct Machine {
    costs: CostModel,
    pub ex_budget: ExBudget,
    slippage: u32,
    unbudgeted_steps: [u32; 10],
    pub traces: Vec<Trace>,
    pub spend_counter: Option<[i64; (TERM_COUNT + BUILTIN_COUNT) * 2]>,
    semantics: BuiltinSemantics,
    frames: Vec<Frame>,
}

impl Machine {
    pub fn new(
        version: Language,
        costs: CostModel,
        initial_budget: ExBudget,
        slippage: u32,
    ) -> Machine {
        let semantics = BuiltinSemantics::for_language(&version);

        Machine {
            costs,
            ex_budget: initial_budget,
            slippage,
            unbudgeted_steps: [0; 10],
            traces: vec![],
            spend_counter: None,
            semantics,
            frames: vec![],
        }
    }

    pub fn new_with_protocol(
        version: Language,
        protocol_major_version: u16,
        costs: CostModel,
        initial_budget: ExBudget,
        slippage: u32,
    ) -> Machine {
        let semantics =
            BuiltinSemantics::for_language_and_protocol(&version, protocol_major_version);

        Machine {
            costs,
            ex_budget: initial_budget,
            slippage,
            unbudgeted_steps: [0; 10],
            traces: vec![],
            spend_counter: None,
            semantics,
            frames: vec![],
        }
    }

    pub fn new_debug(
        version: Language,
        costs: CostModel,
        initial_budget: ExBudget,
        slippage: u32,
    ) -> Machine {
        let semantics = BuiltinSemantics::for_language(&version);

        Machine {
            costs,
            ex_budget: initial_budget,
            slippage,
            unbudgeted_steps: [0; 10],
            traces: vec![],
            spend_counter: Some([0; (TERM_COUNT + BUILTIN_COUNT) * 2]),
            semantics,
            frames: vec![],
        }
    }

    pub fn new_debug_with_protocol(
        version: Language,
        protocol_major_version: u16,
        costs: CostModel,
        initial_budget: ExBudget,
        slippage: u32,
    ) -> Machine {
        let semantics =
            BuiltinSemantics::for_language_and_protocol(&version, protocol_major_version);

        Machine {
            costs,
            ex_budget: initial_budget,
            slippage,
            unbudgeted_steps: [0; 10],
            traces: vec![],
            spend_counter: Some([0; (TERM_COUNT + BUILTIN_COUNT) * 2]),
            semantics,
            frames: vec![],
        }
    }

    pub fn run(&mut self, term: Term<NamedDeBruijn>) -> Result<Term<NamedDeBruijn>, Error> {
        if !self.semantics.supports_values() {
            Self::assert_no_values(&term)?;
        }

        let startup_budget = self.costs.machine_costs.get(StepKind::StartUp);

        self.spend_budget(startup_budget)?;

        let result = self.evaluate(term);

        // A failed evaluation leaves its pending frames on the stack; drop them
        // now instead of keeping what they hold alive with the machine.
        self.frames.clear();

        result
    }

    fn evaluate(&mut self, term: Term<NamedDeBruijn>) -> Result<Term<NamedDeBruijn>, Error> {
        use MachineState::*;

        let mut state = Compute(Env::default(), Rc::new(term).into());

        loop {
            state = match state {
                Compute(env, t) => self.compute(env, t)?,
                Return(value) => self.return_compute(value)?,
                Done(t) => {
                    return Ok(t);
                }
            };
        }
    }

    /// The CIP-0153 value builtins and native `Value` constants only exist for
    /// Plutus V3 from protocol version 11 (Van Rossem) onwards. Availability is
    /// a whole-program well-formedness rule: occurrences in latent code (under
    /// lambdas, delays or untaken branches) are rejected too, so the entire
    /// term is validated up-front before evaluation starts.
    fn assert_no_values(term: &Term<NamedDeBruijn>) -> Result<(), Error> {
        let mut stack = vec![term];

        while let Some(term) = stack.pop() {
            match term {
                Term::Delay(body) | Term::Lambda { body, .. } | Term::Force(body) => {
                    stack.push(body.as_ref());
                }
                Term::Apply { function, argument } => {
                    stack.push(function.as_ref());
                    stack.push(argument.as_ref());
                }
                Term::Constant(constant) => {
                    if Constant::contains_value(constant) {
                        return Err(Error::ValueConstantNotAvailable);
                    }
                }
                Term::Builtin(fun) => {
                    if fun.is_value_builtin() {
                        return Err(Error::BuiltinNotAvailable(*fun));
                    }
                }
                Term::Constr { fields, .. } => {
                    stack.extend(fields.iter());
                }
                Term::Case { constr, branches } => {
                    stack.push(constr.as_ref());
                    stack.extend(branches.iter());
                }
                Term::Var(_) | Term::Error => {}
            }
        }

        Ok(())
    }

    #[inline(always)]
    fn compute(&mut self, env: Env, term: TermRef) -> Result<MachineState, Error> {
        match term.get() {
            Term::Var(name) => {
                self.step_and_maybe_spend(StepKind::Var)?;

                let val = self.lookup_var(name.as_ref(), &env)?;

                Ok(MachineState::Return(val))
            }
            Term::Delay(body) => {
                self.step_and_maybe_spend(StepKind::Delay)?;

                Ok(MachineState::Return(Value::Delay(body.clone(), env)))
            }
            Term::Lambda {
                parameter_name,
                body,
            } => {
                self.step_and_maybe_spend(StepKind::Lambda)?;

                Ok(MachineState::Return(Value::Lambda {
                    parameter_name: parameter_name.clone(),
                    body: body.clone(),
                    env,
                }))
            }
            Term::Apply { function, argument } => {
                self.step_and_maybe_spend(StepKind::Apply)?;

                self.frames
                    .push(Frame::AwaitFunTerm(env.clone(), argument.clone()));

                Ok(MachineState::Compute(env, function.clone().into()))
            }
            Term::Constant(x) => {
                self.step_and_maybe_spend(StepKind::Constant)?;

                Ok(MachineState::Return(Value::Con(x.clone())))
            }
            Term::Force(body) => {
                self.step_and_maybe_spend(StepKind::Force)?;

                self.frames.push(Frame::Force);

                Ok(MachineState::Compute(env, body.clone().into()))
            }
            Term::Error => Err(Error::EvaluationFailure),
            Term::Builtin(fun) => {
                self.step_and_maybe_spend(StepKind::Builtin)?;

                let runtime: BuiltinRuntime = (*fun).into();

                Ok(MachineState::Return(Value::Builtin { fun: *fun, runtime }))
            }
            Term::Constr { tag, fields } => {
                self.step_and_maybe_spend(StepKind::Constr)?;

                match fields.first() {
                    Some(_) => {
                        let field = term.child(0);
                        let resolved_fields = Vec::with_capacity(fields.len());

                        self.frames
                            .push(Frame::Constr(env.clone(), term, resolved_fields));

                        Ok(MachineState::Compute(env, field))
                    }
                    None => Ok(MachineState::Return(Value::Constr {
                        tag: *tag,
                        fields: Rc::new(vec![]),
                    })),
                }
            }
            Term::Case { constr, .. } => {
                self.step_and_maybe_spend(StepKind::Case)?;

                let constr = constr.clone();

                self.frames.push(Frame::Cases(env.clone(), term));

                Ok(MachineState::Compute(env, constr.into()))
            }
        }
    }

    #[inline(always)]
    fn return_compute(&mut self, value: Value) -> Result<MachineState, Error> {
        let Some(frame) = self.frames.pop() else {
            if self.unbudgeted_steps[9] > 0 {
                self.spend_unbudgeted_steps()?;
            }

            let term = discharge::value_as_term(value);

            return Ok(MachineState::Done(term));
        };

        match frame {
            Frame::Force => self.force_evaluate(value),
            Frame::AwaitFunTerm(arg_env, arg) => {
                self.frames.push(Frame::AwaitArg(value));

                Ok(MachineState::Compute(arg_env, arg.into()))
            }
            Frame::AwaitArg(fun) => self.apply_evaluate(fun, value),
            Frame::AwaitFunValue(arg) => self.apply_evaluate(value, arg),
            Frame::Constr(env, term, mut resolved_fields) => {
                let Term::Constr { tag, fields } = term.get() else {
                    unreachable!("FrameConstr always holds a constr term")
                };

                resolved_fields.push(value);

                let next = resolved_fields.len();

                match fields.get(next) {
                    Some(_) => {
                        let field = term.child(next);

                        self.frames
                            .push(Frame::Constr(env.clone(), term, resolved_fields));

                        Ok(MachineState::Compute(env, field))
                    }
                    None => Ok(MachineState::Return(Value::Constr {
                        tag: *tag,
                        fields: Rc::new(resolved_fields),
                    })),
                }
            }
            Frame::Cases(env, term) => self.case_evaluate(env, term, value),
        }
    }

    #[inline(always)]
    fn case_evaluate(
        &mut self,
        env: Env,
        term: TermRef,
        value: Value,
    ) -> Result<MachineState, Error> {
        let Term::Case { branches, .. } = term.get() else {
            unreachable!("FrameCases always holds a case term")
        };

        match value {
            Value::Constr { tag, fields } => match branches.get(tag) {
                Some(_) => {
                    self.transfer_arg_stack(Rc::unwrap_or_clone(fields));

                    Ok(MachineState::Compute(env, term.child(tag)))
                }
                None => Err(Error::MissingCaseBranch(
                    branches.to_vec(),
                    Value::Constr { tag, fields },
                )),
            },
            Value::Con(constant) => {
                if !matches!(self.semantics, BuiltinSemantics::E) {
                    return Err(Error::NonConstrScrutinized(Value::Con(constant)));
                }

                let (tag, fields, max_branches) = match constant.as_ref() {
                    Constant::Unit => (0, vec![], 1),
                    Constant::Bool(false) => (0, vec![], 2),
                    Constant::Bool(true) => (1, vec![], 2),
                    Constant::Integer(integer) => {
                        let Some(tag) = integer.to_usize() else {
                            return Err(Error::MissingCaseBranch(
                                branches.to_vec(),
                                Value::Con(constant),
                            ));
                        };

                        (tag, vec![], usize::MAX)
                    }
                    Constant::ProtoList(_, items) if items.is_empty() => (1, vec![], 2),
                    Constant::ProtoList(item_type, items) => {
                        let head = items[0].clone();
                        let tail = Constant::ProtoList(
                            item_type.clone(),
                            items.skip(1).expect("list is non-empty"),
                        );

                        (0, vec![Value::Con(head), Value::Con(tail.into())], 2)
                    }
                    Constant::ProtoPair(_, _, first, second) => (
                        0,
                        vec![Value::Con(first.clone()), Value::Con(second.clone())],
                        1,
                    ),
                    _ => return Err(Error::NonConstrScrutinized(Value::Con(constant))),
                };

                if branches.len() > max_branches {
                    return Err(Error::MissingCaseBranch(
                        branches.to_vec(),
                        Value::Con(constant),
                    ));
                }

                match branches.get(tag) {
                    Some(_) => {
                        self.transfer_arg_stack(fields);

                        Ok(MachineState::Compute(env, term.child(tag)))
                    }
                    None => Err(Error::MissingCaseBranch(
                        branches.to_vec(),
                        Value::Con(constant),
                    )),
                }
            }
            v => Err(Error::NonConstrScrutinized(v)),
        }
    }

    #[inline(always)]
    fn force_evaluate(&mut self, value: Value) -> Result<MachineState, Error> {
        match value {
            Value::Delay(body, env) => Ok(MachineState::Compute(env, body.into())),
            Value::Builtin { fun, mut runtime } => {
                if runtime.needs_force() {
                    runtime.consume_force();

                    let res = if runtime.is_ready() {
                        self.eval_builtin_app(runtime)?
                    } else {
                        Value::Builtin { fun, runtime }
                    };

                    Ok(MachineState::Return(res))
                } else {
                    let term = discharge::value_as_term(Value::Builtin { fun, runtime });

                    Err(Error::BuiltinTermArgumentExpected(term))
                }
            }
            rest => Err(Error::NonPolymorphicInstantiation(rest)),
        }
    }

    #[inline(always)]
    fn apply_evaluate(&mut self, function: Value, argument: Value) -> Result<MachineState, Error> {
        match function {
            Value::Lambda { body, mut env, .. } => {
                env.push(argument);

                Ok(MachineState::Compute(env, body.into()))
            }
            Value::Builtin { fun, runtime } => {
                if runtime.is_arrow() && !runtime.needs_force() {
                    let mut runtime = runtime;

                    runtime.push(argument)?;

                    let res = if runtime.is_ready() {
                        self.eval_builtin_app(runtime)?
                    } else {
                        Value::Builtin { fun, runtime }
                    };

                    Ok(MachineState::Return(res))
                } else {
                    let term = discharge::value_as_term(Value::Builtin { fun, runtime });

                    Err(Error::UnexpectedBuiltinTermArgument(term))
                }
            }
            rest => Err(Error::NonFunctionalApplication(rest, argument)),
        }
    }

    fn eval_builtin_app(&mut self, runtime: BuiltinRuntime) -> Result<Value, Error> {
        let cost = runtime.to_ex_budget(&self.costs.builtin_costs, self.semantics)?;

        self.spend_budget(cost)?;

        if let Some(counter) = &mut self.spend_counter {
            let i = (runtime.fun as usize + TERM_COUNT) * 2;

            counter[i] += cost.mem;
            counter[i + 1] += cost.cpu;
        }

        runtime.call(self.semantics, &mut self.traces)
    }

    fn lookup_var(&mut self, name: &NamedDeBruijn, env: &Env) -> Result<Value, Error> {
        env.get(usize::from(name.index))
            .cloned()
            .ok_or_else(|| Error::OpenTermEvaluated(Term::Var(name.clone().into())))
    }

    /// Pushes the fields of a scrutinised constructor as pending arguments,
    /// the first field on top.
    fn transfer_arg_stack(&mut self, args: Vec<Value>) {
        self.frames
            .extend(args.into_iter().rev().map(Frame::AwaitFunValue));
    }

    fn step_and_maybe_spend(&mut self, step: StepKind) -> Result<(), Error> {
        let index = step as u8;
        self.unbudgeted_steps[index as usize] += 1;
        self.unbudgeted_steps[9] += 1;

        if self.unbudgeted_steps[9] >= self.slippage {
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

            if let Some(counter) = &mut self.spend_counter {
                counter[i * 2] += unspent_step_budget.mem;
                counter[i * 2 + 1] += unspent_step_budget.cpu;
            }
        }

        self.unbudgeted_steps[9] = 0;

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
            Constant::Value(_) => Type::Value,
            Constant::Bls12_381G1Element(_) => Type::Bls12_381G1Element,
            Constant::Bls12_381G2Element(_) => Type::Bls12_381G2Element,
            Constant::Bls12_381MlResult(_) => Type::Bls12_381MlResult,
        }
    }
}

#[cfg(test)]
mod tests {
    use num_bigint::BigInt;
    use std::rc::Rc;

    use super::{
        Error, Machine,
        cost_model::{CostModel, ExBudget},
        runtime::Compressable,
    };
    use crate::{
        ast::{Constant, NamedDeBruijn, Program, Term},
        builtins::DefaultFunction,
    };
    use pallas_primitives::conway::Language;

    #[test]
    fn value_builtins_unavailable_before_van_rossem_or_outside_v3() {
        let program = |term: Term<NamedDeBruijn>| Program {
            version: (1, 1, 0),
            term,
        };

        let builtin = Term::Builtin(DefaultFunction::LookupCoin);
        let constant: Term<NamedDeBruijn> =
            Term::Constant(Constant::Value(crate::ast::Value::empty()).into());
        // Availability is a whole-program rule: latent occurrences that
        // evaluation never reaches must be rejected too.
        let latent: Term<NamedDeBruijn> = Term::Lambda {
            parameter_name: NamedDeBruijn {
                text: "x".to_string(),
                index: 0.into(),
            }
            .into(),
            body: Term::Builtin(DefaultFunction::ScaleValue).into(),
        };
        // Public AST callers can construct type-inconsistent constants even
        // though Flat decoding cannot. Inspect both declared types and actual
        // elements so this cannot bypass protocol availability checks.
        let inconsistent: Term<NamedDeBruijn> = Term::Constant(
            Constant::ProtoList(
                crate::ast::Type::Integer,
                vec![Rc::new(Constant::Value(crate::ast::Value::empty()))].into(),
            )
            .into(),
        );

        for (language, protocol) in [
            (Language::PlutusV3, 10),
            (Language::PlutusV2, 11),
            (Language::PlutusV1, 11),
        ] {
            let result = program(builtin.clone())
                .eval_version_with_protocol(ExBudget::default(), &language, protocol)
                .result();
            assert!(
                matches!(result, Err(Error::BuiltinNotAvailable(_))),
                "expected lookupCoin to be unavailable for {language:?} at protocol {protocol}, got {result:?}"
            );

            let result = program(constant.clone())
                .eval_version_with_protocol(ExBudget::default(), &language, protocol)
                .result();
            assert!(
                matches!(result, Err(Error::ValueConstantNotAvailable)),
                "expected Value constant to be unavailable for {language:?} at protocol {protocol}, got {result:?}"
            );

            let result = program(inconsistent.clone())
                .eval_version_with_protocol(ExBudget::default(), &language, protocol)
                .result();
            assert!(
                matches!(result, Err(Error::ValueConstantNotAvailable)),
                "expected nested Value constant to be unavailable for {language:?} at protocol {protocol}, got {result:?}"
            );

            let result = program(latent.clone())
                .eval_version_with_protocol(ExBudget::default(), &language, protocol)
                .result();
            assert!(
                matches!(result, Err(Error::BuiltinNotAvailable(_))),
                "expected latent scaleValue to be unavailable for {language:?} at protocol {protocol}, got {result:?}"
            );

            // Direct calls through the public runtime API are gated too.
            let semantics =
                super::runtime::BuiltinSemantics::for_language_and_protocol(&language, protocol);
            let result = DefaultFunction::UnValueData.call(
                semantics,
                &[super::value::Value::Con(
                    Constant::Data(crate::PlutusData::Map(crate::KeyValuePairs::Def(vec![])))
                        .into(),
                )],
                &mut vec![],
            );
            assert!(
                matches!(result, Err(Error::BuiltinNotAvailable(_))),
                "expected direct unValueData call to be unavailable for {language:?} at protocol {protocol}, got {result:?}"
            );
        }

        for term in [builtin, constant, latent] {
            let result = program(term)
                .eval_version_with_protocol(ExBudget::default(), &Language::PlutusV3, 11)
                .result();
            assert!(result.is_ok(), "{result:?}");
        }
    }

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
    fn case_constr_case_0() {
        let make_program =
            |fun: DefaultFunction, tag: usize, n: i32, m: i32| Program::<NamedDeBruijn> {
                version: (0, 0, 0),
                term: Term::Case {
                    constr: Term::Constr {
                        tag,
                        fields: vec![
                            Term::Constant(Constant::Integer(n.into()).into()),
                            Term::Constant(Constant::Integer(m.into()).into()),
                        ],
                    }
                    .into(),
                    branches: vec![Term::Builtin(fun), Term::subtract_integer()],
                },
            };

        let test_data = vec![
            (DefaultFunction::AddInteger, 0, 8, 3, 11),
            (DefaultFunction::AddInteger, 1, 8, 3, 5),
        ];

        for (fun, tag, n, m, result) in test_data {
            let eval_result = make_program(fun, tag, n, m).eval(ExBudget::max());

            assert_eq!(
                eval_result.result().unwrap(),
                Term::Constant(Constant::Integer(result.into()).into())
            );
        }
    }

    #[test]
    fn case_constr_case_1() {
        let make_program = |tag: usize| Program::<NamedDeBruijn> {
            version: (0, 0, 0),
            term: Term::Case {
                constr: Term::Constr {
                    tag,
                    fields: vec![],
                }
                .into(),
                branches: vec![
                    Term::integer(5.into()),
                    Term::integer(10.into()),
                    Term::integer(15.into()),
                ],
            },
        };

        let test_data = vec![(0, 5), (1, 10), (2, 15)];

        for (tag, result) in test_data {
            let eval_result = make_program(tag).eval(ExBudget::max());

            assert_eq!(
                eval_result.result().unwrap(),
                Term::Constant(Constant::Integer(result.into()).into())
            );
        }
    }

    #[test]
    fn bls_g1_add_associative() {
        let a = blst::blst_p1::uncompress(&[
            0xab, 0xd6, 0x18, 0x64, 0xf5, 0x19, 0x74, 0x80, 0x32, 0x55, 0x1e, 0x42, 0xe0, 0xac,
            0x41, 0x7f, 0xd8, 0x28, 0xf0, 0x79, 0x45, 0x4e, 0x3e, 0x3c, 0x98, 0x91, 0xc5, 0xc2,
            0x9e, 0xd7, 0xf1, 0x0b, 0xde, 0xcc, 0x04, 0x68, 0x54, 0xe3, 0x93, 0x1c, 0xb7, 0x00,
            0x27, 0x79, 0xbd, 0x76, 0xd7, 0x1f,
        ])
        .unwrap();

        let b = blst::blst_p1::uncompress(&[
            0x95, 0x0d, 0xfd, 0x33, 0xda, 0x26, 0x82, 0x26, 0x0c, 0x76, 0x03, 0x8d, 0xfb, 0x8b,
            0xad, 0x6e, 0x84, 0xae, 0x9d, 0x59, 0x9a, 0x3c, 0x15, 0x18, 0x15, 0x94, 0x5a, 0xc1,
            0xe6, 0xef, 0x6b, 0x10, 0x27, 0xcd, 0x91, 0x7f, 0x39, 0x07, 0x47, 0x9d, 0x20, 0xd6,
            0x36, 0xce, 0x43, 0x7a, 0x41, 0xf5,
        ])
        .unwrap();

        let c = blst::blst_p1::uncompress(&[
            0xb9, 0x62, 0xfd, 0x0c, 0xc8, 0x10, 0x48, 0xe0, 0xcf, 0x75, 0x57, 0xbf, 0x3e, 0x4b,
            0x6e, 0xdc, 0x5a, 0xb4, 0xbf, 0xb3, 0xdc, 0x87, 0xf8, 0x3a, 0xf4, 0x28, 0xb6, 0x30,
            0x07, 0x27, 0xb1, 0x39, 0xc4, 0x04, 0xab, 0x15, 0x9b, 0xdf, 0x2e, 0xae, 0xa3, 0xf6,
            0x49, 0x90, 0x34, 0x21, 0x53, 0x7f,
        ])
        .unwrap();

        let term: Term<NamedDeBruijn> = Term::bls12_381_g1_equal()
            .apply(
                Term::bls12_381_g1_add().apply(Term::bls12_381_g1(a)).apply(
                    Term::bls12_381_g1_add()
                        .apply(Term::bls12_381_g1(b))
                        .apply(Term::bls12_381_g1(c)),
                ),
            )
            .apply(
                Term::bls12_381_g1_add()
                    .apply(
                        Term::bls12_381_g1_add()
                            .apply(Term::bls12_381_g1(a))
                            .apply(Term::bls12_381_g1(b)),
                    )
                    .apply(Term::bls12_381_g1(c)),
            );

        let program = Program {
            version: (1, 0, 0),
            term,
        };

        let eval_result = program.eval(Default::default());

        let final_term = eval_result.result().unwrap();

        assert_eq!(final_term, Term::bool(true))
    }

    #[test]
    fn bls_g2_add_associative() {
        let a = blst::blst_p1::uncompress(&[
            0xab, 0xd6, 0x18, 0x64, 0xf5, 0x19, 0x74, 0x80, 0x32, 0x55, 0x1e, 0x42, 0xe0, 0xac,
            0x41, 0x7f, 0xd8, 0x28, 0xf0, 0x79, 0x45, 0x4e, 0x3e, 0x3c, 0x98, 0x91, 0xc5, 0xc2,
            0x9e, 0xd7, 0xf1, 0x0b, 0xde, 0xcc, 0x04, 0x68, 0x54, 0xe3, 0x93, 0x1c, 0xb7, 0x00,
            0x27, 0x79, 0xbd, 0x76, 0xd7, 0x1f,
        ])
        .unwrap();

        let b = blst::blst_p1::uncompress(&[
            0x95, 0x0d, 0xfd, 0x33, 0xda, 0x26, 0x82, 0x26, 0x0c, 0x76, 0x03, 0x8d, 0xfb, 0x8b,
            0xad, 0x6e, 0x84, 0xae, 0x9d, 0x59, 0x9a, 0x3c, 0x15, 0x18, 0x15, 0x94, 0x5a, 0xc1,
            0xe6, 0xef, 0x6b, 0x10, 0x27, 0xcd, 0x91, 0x7f, 0x39, 0x07, 0x47, 0x9d, 0x20, 0xd6,
            0x36, 0xce, 0x43, 0x7a, 0x41, 0xf5,
        ])
        .unwrap();

        let c = blst::blst_p1::uncompress(&[
            0xb9, 0x62, 0xfd, 0x0c, 0xc8, 0x10, 0x48, 0xe0, 0xcf, 0x75, 0x57, 0xbf, 0x3e, 0x4b,
            0x6e, 0xdc, 0x5a, 0xb4, 0xbf, 0xb3, 0xdc, 0x87, 0xf8, 0x3a, 0xf4, 0x28, 0xb6, 0x30,
            0x07, 0x27, 0xb1, 0x39, 0xc4, 0x04, 0xab, 0x15, 0x9b, 0xdf, 0x2e, 0xae, 0xa3, 0xf6,
            0x49, 0x90, 0x34, 0x21, 0x53, 0x7f,
        ])
        .unwrap();

        let term: Term<NamedDeBruijn> = Term::bls12_381_g1_equal()
            .apply(
                Term::bls12_381_g1_add().apply(Term::bls12_381_g1(a)).apply(
                    Term::bls12_381_g1_add()
                        .apply(Term::bls12_381_g1(b))
                        .apply(Term::bls12_381_g1(c)),
                ),
            )
            .apply(
                Term::bls12_381_g1_add()
                    .apply(
                        Term::bls12_381_g1_add()
                            .apply(Term::bls12_381_g1(a))
                            .apply(Term::bls12_381_g1(b)),
                    )
                    .apply(Term::bls12_381_g1(c)),
            );

        let program = Program {
            version: (1, 0, 0),
            term,
        };

        let eval_result = program.eval(Default::default());

        let final_term = eval_result.result().unwrap();

        assert_eq!(final_term, Term::bool(true))
    }

    #[test]
    fn failed_run_drops_pending_frames() {
        // `[(lam x x) (error)]` fails while the lambda still awaits its argument.
        let term: Term<NamedDeBruijn> = Term::Apply {
            function: Term::Lambda {
                parameter_name: NamedDeBruijn {
                    text: "x".to_string(),
                    index: 0.into(),
                }
                .into(),
                body: Term::Var(
                    NamedDeBruijn {
                        text: "x".to_string(),
                        index: 1.into(),
                    }
                    .into(),
                )
                .into(),
            }
            .into(),
            argument: Term::Error.into(),
        };

        let mut machine = Machine::new(
            Language::PlutusV3,
            CostModel::default(),
            ExBudget::default(),
            200,
        );

        assert!(matches!(machine.run(term), Err(Error::EvaluationFailure)));
        assert!(machine.frames.is_empty());
    }
}
