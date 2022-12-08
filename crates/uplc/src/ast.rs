use std::{fmt::Display, rc::Rc};

use pallas_primitives::{alonzo::PlutusData, babbage::Language};

use crate::{
    builtins::DefaultFunction,
    debruijn::{self, Converter},
    flat::Binder,
    machine::{
        cost_model::{initialize_cost_model, CostModel, ExBudget},
        Machine,
    },
};

pub mod builder;

/// This represents a program in Untyped Plutus Core.
/// A program contains a version tuple and a term.
/// It is generic because Term requires a generic type.
#[derive(Debug, Clone, PartialEq)]
pub struct Program<T> {
    pub version: (usize, usize, usize),
    pub term: Term<T>,
}

impl<T> Program<T>
where
    T: Clone,
{
    /// We use this to apply the validator to Datum,
    /// then redeemer, then ScriptContext. If datum is
    /// even necessary (i.e. minting policy).
    pub fn apply(&self, program: &Self) -> Self {
        let applied_term = Term::Apply {
            function: Rc::new(self.term.clone()),
            argument: Rc::new(program.term.clone()),
        };

        Program {
            version: self.version,
            term: applied_term,
        }
    }

    /// We use this to apply the validator to Datum,
    /// then redeemer, then ScriptContext. If datum is
    /// even necessary (i.e. minting policy).
    pub fn apply_term(&self, term: &Term<T>) -> Self {
        let applied_term = Term::Apply {
            function: Rc::new(self.term.clone()),
            argument: Rc::new(term.clone()),
        };

        Program {
            version: self.version,
            term: applied_term,
        }
    }

    pub fn apply_data(&self, plutus_data: PlutusData) -> Self {
        let applied_term = Term::Apply {
            function: Rc::new(self.term.clone()),
            argument: Rc::new(Term::Constant(Constant::Data(plutus_data))),
        };

        Program {
            version: self.version,
            term: applied_term,
        }
    }
}

impl<'a, T> Display for Program<T>
where
    T: Binder<'a>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_pretty())
    }
}

/// This represents a term in Untyped Plutus Core.
/// We need a generic type for the different forms that a program may be in.
/// Specifically, `Var` and `parameter_name` in `Lambda` can be a `Name`,
/// `NamedDebruijn`, or `DeBruijn`. When encoded to flat for on chain usage
/// we must encode using the `DeBruijn` form.
#[derive(Debug, Clone, PartialEq)]
pub enum Term<T> {
    // tag: 0
    Var(T),
    // tag: 1
    Delay(Rc<Term<T>>),
    // tag: 2
    Lambda {
        parameter_name: T,
        body: Rc<Term<T>>,
    },
    // tag: 3
    Apply {
        function: Rc<Term<T>>,
        argument: Rc<Term<T>>,
    },
    // tag: 4
    Constant(Constant),
    // tag: 5
    Force(Rc<Term<T>>),
    // tag: 6
    Error,
    // tag: 7
    Builtin(DefaultFunction),
}

impl<T> Term<T> {
    pub fn is_unit(&self) -> bool {
        matches!(self, Term::Constant(Constant::Unit))
    }

    pub fn force_wrap(self) -> Self {
        Term::Force(self.into())
    }
}

impl<'a, T> Display for Term<T>
where
    T: Binder<'a>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_pretty())
    }
}

/// A container for the various constants that are available
/// in Untyped Plutus Core. Used in the `Constant` variant of `Term`.
#[derive(Debug, Clone, PartialEq)]
pub enum Constant {
    // tag: 0
    Integer(i128),
    // tag: 1
    ByteString(Vec<u8>),
    // tag: 2
    String(String),
    // tag: 3
    Unit,
    // tag: 4
    Bool(bool),
    // tag: 5
    ProtoList(Type, Vec<Constant>),
    // tag: 6
    ProtoPair(Type, Type, Box<Constant>, Box<Constant>),
    // tag: 7
    // Apply(Box<Constant>, Type),
    // tag: 8
    Data(PlutusData),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Bool,
    Integer,
    String,
    ByteString,
    Unit,
    List(Box<Type>),
    Pair(Box<Type>, Box<Type>),
    Data,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Bool => write!(f, "bool"),
            Type::Integer => write!(f, "integer"),
            Type::String => write!(f, "string"),
            Type::ByteString => write!(f, "bytestring"),
            Type::Unit => write!(f, "unit"),
            Type::List(t) => write!(f, "list {}", t),
            Type::Pair(t1, t2) => write!(f, "pair {} {}", t1, t2),
            Type::Data => write!(f, "data"),
        }
    }
}

/// A Name containing it's parsed textual representation
/// and a unique id from string interning. The Name's text is
/// interned during parsing.
#[derive(Debug, Clone)]
pub struct Name {
    pub text: String,
    pub unique: Unique,
}

impl PartialEq for Name {
    fn eq(&self, other: &Self) -> bool {
        self.unique == other.unique
    }
}

/// A unique id used for string interning.
#[derive(Debug, Clone, PartialEq, Copy, Eq, Hash)]
pub struct Unique(isize);

impl Unique {
    /// Create a new unique id.
    pub fn new(unique: isize) -> Self {
        Unique(unique)
    }

    /// Increment the available unique id. This is used during
    /// string interning to get the next available unique id.
    pub fn increment(&mut self) {
        self.0 += 1;
    }
}

impl From<isize> for Unique {
    fn from(i: isize) -> Self {
        Unique(i)
    }
}

impl From<Unique> for isize {
    fn from(d: Unique) -> Self {
        d.0
    }
}

impl Display for Unique {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Similar to `Name` but for Debruijn indices.
/// `Name` is replaced by `NamedDebruijn` when converting
/// program to it's debruijn form.
#[derive(Debug, Clone)]
pub struct NamedDeBruijn {
    pub text: String,
    pub index: DeBruijn,
}

impl PartialEq for NamedDeBruijn {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

/// This is useful for decoding a on chain program into debruijn form.
/// It allows for injecting fake textual names while also using Debruijn for decoding
/// without having to loop through twice.
#[derive(Debug, Clone, PartialEq)]
pub struct FakeNamedDeBruijn(pub NamedDeBruijn);

impl From<DeBruijn> for FakeNamedDeBruijn {
    fn from(d: DeBruijn) -> Self {
        FakeNamedDeBruijn(d.into())
    }
}

impl From<FakeNamedDeBruijn> for DeBruijn {
    fn from(d: FakeNamedDeBruijn) -> Self {
        d.0.into()
    }
}

impl From<FakeNamedDeBruijn> for NamedDeBruijn {
    fn from(d: FakeNamedDeBruijn) -> Self {
        d.0
    }
}

impl From<NamedDeBruijn> for FakeNamedDeBruijn {
    fn from(d: NamedDeBruijn) -> Self {
        FakeNamedDeBruijn(d)
    }
}

/// Represents a debruijn index.
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct DeBruijn(usize);

impl DeBruijn {
    /// Create a new debruijn index.
    pub fn new(index: usize) -> Self {
        DeBruijn(index)
    }

    pub fn inner(&self) -> usize {
        self.0
    }
}

impl From<usize> for DeBruijn {
    fn from(i: usize) -> Self {
        DeBruijn(i)
    }
}

impl From<DeBruijn> for usize {
    fn from(d: DeBruijn) -> Self {
        d.0
    }
}

impl Display for DeBruijn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<NamedDeBruijn> for DeBruijn {
    fn from(n: NamedDeBruijn) -> Self {
        n.index
    }
}

impl From<DeBruijn> for NamedDeBruijn {
    fn from(index: DeBruijn) -> Self {
        NamedDeBruijn {
            // Inject fake name. We got `i` from the Plutus code base.
            text: String::from("i"),
            index,
        }
    }
}

/// Convert a Parsed `Program` to a `Program` in `NamedDebruijn` form.
/// This checks for any Free Uniques in the `Program` and returns an error if found.
impl TryFrom<Program<Name>> for Program<NamedDeBruijn> {
    type Error = debruijn::Error;

    fn try_from(value: Program<Name>) -> Result<Self, Self::Error> {
        Ok(Program::<NamedDeBruijn> {
            version: value.version,
            term: value.term.try_into()?,
        })
    }
}

/// Convert a Parsed `Term` to a `Term` in `NamedDebruijn` form.
/// This checks for any Free Uniques in the `Term` and returns an error if found.
impl TryFrom<Term<Name>> for Term<NamedDeBruijn> {
    type Error = debruijn::Error;

    fn try_from(value: Term<Name>) -> Result<Self, debruijn::Error> {
        let mut converter = Converter::new();

        let term = converter.name_to_named_debruijn(&value)?;

        Ok(term)
    }
}

/// Convert a Parsed `Program` to a `Program` in `Debruijn` form.
/// This checks for any Free Uniques in the `Program` and returns an error if found.
impl TryFrom<Program<Name>> for Program<DeBruijn> {
    type Error = debruijn::Error;

    fn try_from(value: Program<Name>) -> Result<Self, Self::Error> {
        Ok(Program::<DeBruijn> {
            version: value.version,
            term: value.term.try_into()?,
        })
    }
}

/// Convert a Parsed `Term` to a `Term` in `Debruijn` form.
/// This checks for any Free Uniques in the `Program` and returns an error if found.
impl TryFrom<Term<Name>> for Term<DeBruijn> {
    type Error = debruijn::Error;

    fn try_from(value: Term<Name>) -> Result<Self, debruijn::Error> {
        let mut converter = Converter::new();

        let term = converter.name_to_debruijn(&value)?;

        Ok(term)
    }
}

impl TryFrom<Program<NamedDeBruijn>> for Program<Name> {
    type Error = debruijn::Error;

    fn try_from(value: Program<NamedDeBruijn>) -> Result<Self, Self::Error> {
        Ok(Program::<Name> {
            version: value.version,
            term: value.term.try_into()?,
        })
    }
}

impl TryFrom<Term<NamedDeBruijn>> for Term<Name> {
    type Error = debruijn::Error;

    fn try_from(value: Term<NamedDeBruijn>) -> Result<Self, debruijn::Error> {
        let mut converter = Converter::new();

        let term = converter.named_debruijn_to_name(&value)?;

        Ok(term)
    }
}

impl From<Program<NamedDeBruijn>> for Program<DeBruijn> {
    fn from(value: Program<NamedDeBruijn>) -> Self {
        Program::<DeBruijn> {
            version: value.version,
            term: value.term.into(),
        }
    }
}

impl From<Term<NamedDeBruijn>> for Term<DeBruijn> {
    fn from(value: Term<NamedDeBruijn>) -> Self {
        let mut converter = Converter::new();

        converter.named_debruijn_to_debruijn(&value)
    }
}

impl From<Program<NamedDeBruijn>> for Program<FakeNamedDeBruijn> {
    fn from(value: Program<NamedDeBruijn>) -> Self {
        Program::<FakeNamedDeBruijn> {
            version: value.version,
            term: value.term.into(),
        }
    }
}

impl From<Term<NamedDeBruijn>> for Term<FakeNamedDeBruijn> {
    fn from(value: Term<NamedDeBruijn>) -> Self {
        let mut converter = Converter::new();

        converter.named_debruijn_to_fake_named_debruijn(&value)
    }
}

impl TryFrom<Program<DeBruijn>> for Program<Name> {
    type Error = debruijn::Error;

    fn try_from(value: Program<DeBruijn>) -> Result<Self, Self::Error> {
        Ok(Program::<Name> {
            version: value.version,
            term: value.term.try_into()?,
        })
    }
}

impl TryFrom<Term<DeBruijn>> for Term<Name> {
    type Error = debruijn::Error;

    fn try_from(value: Term<DeBruijn>) -> Result<Self, debruijn::Error> {
        let mut converter = Converter::new();

        let term = converter.debruijn_to_name(&value)?;

        Ok(term)
    }
}

impl From<Program<DeBruijn>> for Program<NamedDeBruijn> {
    fn from(value: Program<DeBruijn>) -> Self {
        Program::<NamedDeBruijn> {
            version: value.version,
            term: value.term.into(),
        }
    }
}

impl From<Term<DeBruijn>> for Term<NamedDeBruijn> {
    fn from(value: Term<DeBruijn>) -> Self {
        let mut converter = Converter::new();

        converter.debruijn_to_named_debruijn(&value)
    }
}

impl From<Program<FakeNamedDeBruijn>> for Program<NamedDeBruijn> {
    fn from(value: Program<FakeNamedDeBruijn>) -> Self {
        Program::<NamedDeBruijn> {
            version: value.version,
            term: value.term.into(),
        }
    }
}

impl From<Term<FakeNamedDeBruijn>> for Term<NamedDeBruijn> {
    fn from(value: Term<FakeNamedDeBruijn>) -> Self {
        let mut converter = Converter::new();

        converter.fake_named_debruijn_to_named_debruijn(&value)
    }
}

impl Program<NamedDeBruijn> {
    pub fn eval(
        &self,
        initial_budget: ExBudget,
    ) -> (
        Result<Term<NamedDeBruijn>, crate::machine::Error>,
        ExBudget,
        Vec<String>,
    ) {
        let mut machine = Machine::new(
            Language::PlutusV2,
            CostModel::default(),
            initial_budget,
            200,
        );

        let term = machine.run(&self.term);

        (term, machine.ex_budget, machine.logs)
    }

    /// Evaluate a Program as PlutusV1
    pub fn eval_v1(
        &self,
    ) -> (
        Result<Term<NamedDeBruijn>, crate::machine::Error>,
        ExBudget,
        Vec<String>,
    ) {
        let mut machine = Machine::new(Language::PlutusV1, CostModel::v1(), ExBudget::v1(), 200);

        let term = machine.run(&self.term);

        (term, machine.ex_budget, machine.logs)
    }

    pub fn eval_as(
        &self,
        version: &Language,
        costs: &[i64],
        initial_budget: Option<&ExBudget>,
    ) -> (
        Result<Term<NamedDeBruijn>, crate::machine::Error>,
        ExBudget,
        Vec<String>,
    ) {
        let budget = match initial_budget {
            Some(b) => *b,
            None => ExBudget::default(),
        };

        let mut machine = Machine::new(
            version.clone(),
            initialize_cost_model(version, costs),
            budget,
            200, //slippage
        );

        let term = machine.run(&self.term);

        (term, machine.ex_budget, machine.logs)
    }
}

impl Program<DeBruijn> {
    pub fn eval(
        &self,
        initial_budget: ExBudget,
    ) -> (
        Result<Term<NamedDeBruijn>, crate::machine::Error>,
        ExBudget,
        Vec<String>,
    ) {
        let program: Program<NamedDeBruijn> = self.clone().into();

        program.eval(initial_budget)
    }
}

impl Term<NamedDeBruijn> {
    pub fn is_valid_script_result(&self) -> bool {
        !matches!(self, Term::Error)
    }
}
