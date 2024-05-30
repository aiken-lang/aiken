use crate::{
    builtins::DefaultFunction,
    debruijn::{self, Converter},
    flat::Binder,
    machine::{
        cost_model::{initialize_cost_model, CostModel, ExBudget},
        eval_result::EvalResult,
        Machine,
    },
    optimize::interner::CodeGenInterner,
};
use num_bigint::BigInt;
use num_traits::ToPrimitive;
use pallas_addresses::{Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart};
use pallas_primitives::{
    alonzo::{self, Constr, PlutusData},
    conway::{self, Language},
};
use pallas_traverse::ComputeHash;
use serde::{
    self,
    de::{self, Deserialize, Deserializer, MapAccess, Visitor},
    ser::{Serialize, SerializeStruct, Serializer},
};
use std::{
    fmt::{self, Display},
    hash::{self, Hash},
    rc::Rc,
};

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

    /// A convenient and faster version that `apply_term` since the program doesn't need to be
    /// re-interned (constant Data do not introduce new bindings).
    pub fn apply_data(&self, plutus_data: PlutusData) -> Self {
        let applied_term = Term::Apply {
            function: Rc::new(self.term.clone()),
            argument: Rc::new(Term::Constant(Constant::Data(plutus_data).into())),
        };

        Program {
            version: self.version,
            term: applied_term,
        }
    }
}

impl Program<Name> {
    /// We use this to apply the validator to Datum,
    /// then redeemer, then ScriptContext. If datum is
    /// even necessary (i.e. minting policy).
    pub fn apply_term(&self, term: &Term<Name>) -> Self {
        let applied_term = Term::Apply {
            function: Rc::new(self.term.clone()),
            argument: Rc::new(term.clone()),
        };

        let mut program = Program {
            version: self.version,
            term: applied_term,
        };

        CodeGenInterner::new().program(&mut program);

        program
    }

    /// A convenient method to convery named programs to debruijn programs.
    pub fn to_debruijn(self) -> Result<Program<DeBruijn>, debruijn::Error> {
        self.try_into()
    }

    /// A convenient method to convery named programs to named debruijn programs.
    pub fn to_named_debruijn(self) -> Result<Program<NamedDeBruijn>, debruijn::Error> {
        self.try_into()
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

impl Serialize for Program<DeBruijn> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let cbor = self.to_cbor().unwrap();
        let mut s = serializer.serialize_struct("Program<DeBruijn>", 2)?;
        s.serialize_field("compiledCode", &hex::encode(&cbor))?;
        s.serialize_field("hash", &conway::PlutusV2Script(cbor.into()).compute_hash())?;
        s.end()
    }
}

impl<'a> Deserialize<'a> for Program<DeBruijn> {
    fn deserialize<D: Deserializer<'a>>(deserializer: D) -> Result<Self, D::Error> {
        #[derive(serde::Deserialize)]
        #[serde(field_identifier, rename_all = "camelCase")]
        enum Fields {
            CompiledCode,
        }

        struct ProgramVisitor;

        impl<'a> Visitor<'a> for ProgramVisitor {
            type Value = Program<DeBruijn>;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("Program<Visitor>")
            }

            fn visit_map<V>(self, mut map: V) -> Result<Program<DeBruijn>, V::Error>
            where
                V: MapAccess<'a>,
            {
                let mut compiled_code: Option<String> = None;
                while let Some(key) = map.next_key()? {
                    match key {
                        Fields::CompiledCode => {
                            if compiled_code.is_some() {
                                return Err(de::Error::duplicate_field("compiledCode"));
                            }
                            compiled_code = Some(map.next_value()?);
                        }
                    }
                }
                let compiled_code =
                    compiled_code.ok_or_else(|| de::Error::missing_field("compiledCode"))?;

                let mut cbor_buffer = Vec::new();
                let mut flat_buffer = Vec::new();

                Program::<DeBruijn>::from_hex(&compiled_code, &mut cbor_buffer, &mut flat_buffer)
                    .map_err(|e| {
                        de::Error::invalid_value(
                            de::Unexpected::Other(&format!("{e}")),
                            &"a base16-encoded CBOR-serialized UPLC program",
                        )
                    })
            }
        }

        const FIELDS: &[&str] = &["compiledCode"];
        deserializer.deserialize_struct("Program<DeBruijn>", FIELDS, ProgramVisitor)
    }
}

impl Program<DeBruijn> {
    pub fn address(
        &self,
        network: Network,
        delegation: ShelleyDelegationPart,
        plutus_version: &Language,
    ) -> ShelleyAddress {
        let cbor = self.to_cbor().unwrap();

        let validator_hash = match plutus_version {
            Language::PlutusV1 => conway::PlutusV1Script(cbor.into()).compute_hash(),
            Language::PlutusV2 => conway::PlutusV2Script(cbor.into()).compute_hash(),
            Language::PlutusV3 => conway::PlutusV3Script(cbor.into()).compute_hash(),
        };

        ShelleyAddress::new(
            network,
            ShelleyPaymentPart::Script(validator_hash),
            delegation,
        )
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
    Var(Rc<T>),
    // tag: 1
    Delay(Rc<Term<T>>),
    // tag: 2
    Lambda {
        parameter_name: Rc<T>,
        body: Rc<Term<T>>,
    },
    // tag: 3
    Apply {
        function: Rc<Term<T>>,
        argument: Rc<Term<T>>,
    },
    // tag: 4
    Constant(Rc<Constant>),
    // tag: 5
    Force(Rc<Term<T>>),
    // tag: 6
    Error,
    // tag: 7
    Builtin(DefaultFunction),
    Constr {
        tag: usize,
        fields: Vec<Term<T>>,
    },
    Case {
        constr: Rc<Term<T>>,
        branches: Vec<Term<T>>,
    },
}

impl<T> Term<T> {
    pub fn is_unit(&self) -> bool {
        matches!(self, Term::Constant(c) if c.as_ref() == &Constant::Unit)
    }

    pub fn is_int(&self) -> bool {
        matches!(self, Term::Constant(c) if matches!(c.as_ref(), &Constant::Integer(_)))
    }
}

impl<T> TryInto<PlutusData> for Term<T> {
    type Error = String;

    fn try_into(self) -> Result<PlutusData, String> {
        match self {
            Term::Constant(rc) => match &*rc {
                Constant::Data(data) => Ok(data.to_owned()),
                _ => Err("not a data".to_string()),
            },
            _ => Err("not a data".to_string()),
        }
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
    Integer(BigInt),
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
    ProtoPair(Type, Type, Rc<Constant>, Rc<Constant>),
    // tag: 7
    // Apply(Box<Constant>, Type),
    // tag: 8
    Data(PlutusData),
    Bls12_381G1Element(Box<blst::blst_p1>),
    Bls12_381G2Element(Box<blst::blst_p2>),
    Bls12_381MlResult(Box<blst::blst_fp12>),
}

pub struct Data;

// TODO: See about moving these builders upstream to Pallas?
impl Data {
    pub fn to_hex(data: PlutusData) -> String {
        let mut bytes = Vec::new();
        pallas_codec::minicbor::Encoder::new(&mut bytes)
            .encode(data)
            .expect("failed to encode Plutus Data as cbor?");
        hex::encode(bytes)
    }
    pub fn integer(i: BigInt) -> PlutusData {
        match i.to_i128().map(|n| n.try_into()) {
            Some(Ok(i)) => PlutusData::BigInt(alonzo::BigInt::Int(i)),
            _ => {
                let (sign, bytes) = i.to_bytes_be();
                match sign {
                    num_bigint::Sign::Minus => {
                        PlutusData::BigInt(alonzo::BigInt::BigNInt(bytes.into()))
                    }
                    _ => PlutusData::BigInt(alonzo::BigInt::BigUInt(bytes.into())),
                }
            }
        }
    }

    pub fn bytestring(bytes: Vec<u8>) -> PlutusData {
        PlutusData::BoundedBytes(bytes.into())
    }

    pub fn map(kvs: Vec<(PlutusData, PlutusData)>) -> PlutusData {
        PlutusData::Map(kvs.into())
    }

    pub fn list(xs: Vec<PlutusData>) -> PlutusData {
        PlutusData::Array(xs)
    }

    pub fn constr(ix: u64, fields: Vec<PlutusData>) -> PlutusData {
        // NOTE: see https://github.com/input-output-hk/plutus/blob/9538fc9829426b2ecb0628d352e2d7af96ec8204/plutus-core/plutus-core/src/PlutusCore/Data.hs#L139-L155
        if ix < 7 {
            PlutusData::Constr(Constr {
                tag: 121 + ix,
                any_constructor: None,
                fields,
            })
        } else if ix < 128 {
            PlutusData::Constr(Constr {
                tag: 1280 + ix - 7,
                any_constructor: None,
                fields,
            })
        } else {
            PlutusData::Constr(Constr {
                tag: 102,
                any_constructor: Some(ix),
                fields,
            })
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Bool,
    Integer,
    String,
    ByteString,
    Unit,
    List(Rc<Type>),
    Pair(Rc<Type>, Rc<Type>),
    Data,
    Bls12_381G1Element,
    Bls12_381G2Element,
    Bls12_381MlResult,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Bool => write!(f, "bool"),
            Type::Integer => write!(f, "integer"),
            Type::String => write!(f, "string"),
            Type::ByteString => write!(f, "bytestring"),
            Type::Unit => write!(f, "unit"),
            Type::List(t) => write!(f, "list {t}"),
            Type::Pair(t1, t2) => write!(f, "pair {t1} {t2}"),
            Type::Data => write!(f, "data"),
            Type::Bls12_381G1Element => write!(f, "bls12_381_G1_element"),
            Type::Bls12_381G2Element => write!(f, "bls12_381_G2_element"),
            Type::Bls12_381MlResult => write!(f, "bls12_381_mlresult"),
        }
    }
}

/// A Name containing it's parsed textual representation
/// and a unique id from string interning. The Name's text is
/// interned during parsing.
#[derive(Debug, Clone, Eq)]
pub struct Name {
    pub text: String,
    pub unique: Unique,
}

impl Name {
    pub fn text(t: impl ToString) -> Name {
        Name {
            text: t.to_string(),
            unique: 0.into(),
        }
    }
}

impl hash::Hash for Name {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.text.hash(state);
        self.unique.hash(state);
    }
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
#[derive(Debug, Clone, Eq)]
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
#[derive(Debug, Clone)]
pub struct FakeNamedDeBruijn(pub(crate) NamedDeBruijn);

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

impl TryFrom<&Program<DeBruijn>> for Program<Name> {
    type Error = debruijn::Error;

    fn try_from(value: &Program<DeBruijn>) -> Result<Self, Self::Error> {
        Ok(Program::<Name> {
            version: value.version,
            term: (&value.term).try_into()?,
        })
    }
}

impl TryFrom<&Term<DeBruijn>> for Term<Name> {
    type Error = debruijn::Error;

    fn try_from(value: &Term<DeBruijn>) -> Result<Self, debruijn::Error> {
        let mut converter = Converter::new();

        let term = converter.debruijn_to_name(value)?;

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
    pub fn eval(self, initial_budget: ExBudget) -> EvalResult {
        let mut machine = Machine::new(
            Language::PlutusV2,
            CostModel::default(),
            initial_budget,
            200,
        );

        let term = machine.run(self.term);

        EvalResult::new(term, machine.ex_budget, initial_budget, machine.logs)
    }

    /// Evaluate a Program as a specific PlutusVersion
    pub fn eval_version(self, initial_budget: ExBudget, version: &Language) -> EvalResult {
        let mut machine = Machine::new(version.clone(), CostModel::default(), initial_budget, 200);

        let term = machine.run(self.term);

        EvalResult::new(term, machine.ex_budget, initial_budget, machine.logs)
    }

    pub fn eval_as(
        self,
        version: &Language,
        costs: &[i64],
        initial_budget: Option<&ExBudget>,
    ) -> EvalResult {
        let budget = initial_budget.copied().unwrap_or_default();

        let mut machine = Machine::new(
            version.clone(),
            initialize_cost_model(version, costs),
            budget,
            200, //slippage
        );

        let term = machine.run(self.term);

        EvalResult::new(term, machine.ex_budget, budget, machine.logs)
    }
}

impl Program<DeBruijn> {
    pub fn eval(&self, initial_budget: ExBudget) -> EvalResult {
        let program: Program<NamedDeBruijn> = self.clone().into();

        program.eval(initial_budget)
    }
}

impl Term<NamedDeBruijn> {
    pub fn is_valid_script_result(&self) -> bool {
        !matches!(self, Term::Error)
    }
}
