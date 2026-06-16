use crate::{
    builtins::DefaultFunction,
    debruijn::{self, Converter},
    flat::Binder,
    machine::{
        Machine,
        cost_model::{
            CostModel, ExBudget, initialize_cost_model, initialize_cost_model_with_protocol,
        },
        eval_result::EvalResult,
        value::to_pallas_bigint,
    },
    optimize::interner::CodeGenInterner,
    tx::script_context::PlutusScript,
};
use num_bigint::BigInt;
use num_traits::Zero;
use pallas_addresses::{Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart};
use pallas_primitives::{
    alonzo::{Constr, PlutusData},
    conway::{self, Language},
};
use pallas_traverse::ComputeHash;
use serde::{
    self,
    de::{self, Deserialize, Deserializer, MapAccess, Visitor},
    ser::{Serialize, SerializeStruct, Serializer},
};
use std::{
    convert::AsRef,
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

#[derive(Debug, Clone, PartialEq)]
pub enum SerializableProgram {
    PlutusV1Program(Program<DeBruijn>),
    PlutusV2Program(Program<DeBruijn>),
    PlutusV3Program(Program<DeBruijn>),
}

impl SerializableProgram {
    pub fn inner(&self) -> &Program<DeBruijn> {
        use SerializableProgram::*;

        match self {
            PlutusV1Program(program) => program,
            PlutusV2Program(program) => program,
            PlutusV3Program(program) => program,
        }
    }

    pub fn map<F>(self, f: F) -> Self
    where
        F: FnOnce(Program<DeBruijn>) -> Program<DeBruijn>,
    {
        use SerializableProgram::*;

        match self {
            PlutusV1Program(program) => PlutusV1Program(f(program)),
            PlutusV2Program(program) => PlutusV2Program(f(program)),
            PlutusV3Program(program) => PlutusV3Program(f(program)),
        }
    }

    pub fn compiled_code_and_hash(&self) -> (pallas_crypto::hash::Hash<28>, PlutusScript) {
        use SerializableProgram::*;

        match self {
            PlutusV1Program(pgrm) => {
                let cbor = pgrm.to_cbor().unwrap();
                let script = conway::PlutusScript::<1>(cbor.into());
                let hash = script.compute_hash();
                (hash, PlutusScript::V1(script))
            }

            PlutusV2Program(pgrm) => {
                let cbor = pgrm.to_cbor().unwrap();
                let script = conway::PlutusScript::<2>(cbor.into());
                let hash = script.compute_hash();
                (hash, PlutusScript::V2(script))
            }

            PlutusV3Program(pgrm) => {
                let cbor = pgrm.to_cbor().unwrap();
                let script = conway::PlutusScript::<3>(cbor.into());
                let hash = script.compute_hash();
                (hash, PlutusScript::V3(script))
            }
        }
    }
}

impl Serialize for SerializableProgram {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let (hash, compiled_code) = self.compiled_code_and_hash();
        let mut s = serializer.serialize_struct("Program<DeBruijn>", 2)?;
        s.serialize_field("compiledCode", &hex::encode(compiled_code.as_ref()))?;
        s.serialize_field("hash", &hash)?;
        s.end()
    }
}

impl<'a> Deserialize<'a> for SerializableProgram {
    fn deserialize<D: Deserializer<'a>>(deserializer: D) -> Result<Self, D::Error> {
        #[derive(serde::Deserialize)]
        #[serde(field_identifier, rename_all = "camelCase")]
        enum Fields {
            CompiledCode,
            Hash,
        }

        struct ProgramVisitor;

        impl<'a> Visitor<'a> for ProgramVisitor {
            type Value = SerializableProgram;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("validator")
            }

            fn visit_map<V>(self, mut map: V) -> Result<SerializableProgram, V::Error>
            where
                V: MapAccess<'a>,
            {
                let mut compiled_code: Option<String> = None;
                let mut hash: Option<String> = None;
                while let Some(key) = map.next_key()? {
                    match key {
                        Fields::CompiledCode => {
                            if compiled_code.is_some() {
                                return Err(de::Error::duplicate_field("compiledCode"));
                            }
                            compiled_code = Some(map.next_value()?);
                        }

                        Fields::Hash => {
                            if hash.is_some() {
                                return Err(de::Error::duplicate_field("hash"));
                            }
                            hash = Some(map.next_value()?);
                        }
                    }
                }
                let compiled_code =
                    compiled_code.ok_or_else(|| de::Error::missing_field("compiledCode"))?;

                let hash = hash.ok_or_else(|| de::Error::missing_field("hash"))?;

                let mut cbor_buffer = Vec::new();
                let mut flat_buffer = Vec::new();

                Program::<DeBruijn>::from_hex(&compiled_code, &mut cbor_buffer, &mut flat_buffer)
                    .map_err(|e| {
                        de::Error::invalid_value(
                            de::Unexpected::Other(&format!("{e}")),
                            &"a base16-encoded CBOR-serialized UPLC program",
                        )
                    })
                    .and_then(|program| {
                        let cbor = || program.to_cbor().unwrap().into();

                        if conway::PlutusScript::<3>(cbor()).compute_hash().to_string() == hash {
                            return Ok(SerializableProgram::PlutusV3Program(program));
                        }

                        if conway::PlutusScript::<2>(cbor()).compute_hash().to_string() == hash {
                            return Ok(SerializableProgram::PlutusV2Program(program));
                        }

                        if conway::PlutusScript::<1>(cbor()).compute_hash().to_string() == hash {
                            return Ok(SerializableProgram::PlutusV1Program(program));
                        }

                        Err(de::Error::custom(
                            "hash doesn't match any recognisable Plutus version.",
                        ))
                    })
            }
        }

        const FIELDS: &[&str] = &["compiledCode", "hash"];
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
            Language::PlutusV1 => conway::PlutusScript::<1>(cbor.into()).compute_hash(),
            Language::PlutusV2 => conway::PlutusScript::<2>(cbor.into()).compute_hash(),
            Language::PlutusV3 => conway::PlutusScript::<3>(cbor.into()).compute_hash(),
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
    // tag: 8
    Constr {
        tag: usize,
        fields: Vec<Term<T>>,
    },
    // tag: 9
    Case {
        constr: Rc<Term<T>>,
        branches: Vec<Term<T>>,
    },
}

impl<T> Term<T> {
    pub fn is_constant(&self) -> bool {
        matches!(self, Term::Constant(..))
            || matches!(self, Term::Delay(term) | Term::Force(term) if term.is_constant())
    }

    pub fn is_true(&self) -> bool {
        matches!(self, Term::Constant(c) if c.as_ref() == &Constant::Bool(true))
    }

    pub fn is_false(&self) -> bool {
        matches!(self, Term::Constant(c) if c.as_ref() == &Constant::Bool(false))
    }

    pub fn is_unit(&self) -> bool {
        matches!(self, Term::Constant(c) if c.as_ref() == &Constant::Unit)
    }

    pub fn is_int(&self) -> bool {
        matches!(self, Term::Constant(c) if matches!(c.as_ref(), &Constant::Integer(_)))
    }

    /// Change a constant integer to its opposite.
    pub fn try_negate(&self) -> Option<Self> {
        match self {
            Self::Constant(cst) => match cst.as_ref() {
                Constant::Integer(i) => Some(Self::Constant(Rc::new(Constant::Integer(-1 * i)))),
                _ => None,
            },
            Self::Delay(rc) => rc.try_negate().map(Rc::new).map(Self::Delay),
            Self::Force(rc) => rc.try_negate().map(Rc::new).map(Self::Force),
            _ => None,
        }
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
    // tag: 12
    ProtoArray(Type, Vec<Constant>),
    // tag: 13
    Value(Value),
}

/// Maximum length, in bytes, of a currency symbol or token name used as a key
/// in a `Value`. Mirrors `maxKeyLen` in PlutusCore.Value (currency symbols are
/// in practice either empty or 28 bytes, but plutus allows anything in
/// `0..=32`, so we do the same).
pub const VALUE_MAX_KEY_LEN: usize = 32;

/// Inclusive lower bound for a `Value` quantity: `-(2^127)` (i.e. `i128::MIN`).
pub const VALUE_QUANTITY_MIN: i128 = i128::MIN;

/// Inclusive upper bound for a `Value` quantity: `2^127 - 1` (i.e. `i128::MAX`).
pub const VALUE_QUANTITY_MAX: i128 = i128::MAX;

/// Errors that can occur while constructing/normalizing a [`Value`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValueError {
    /// A key (currency symbol or token name) exceeded [`VALUE_MAX_KEY_LEN`] bytes.
    KeyTooLong(usize),
    /// A quantity (either as provided or as the result of merging duplicate
    /// keys) fell outside the signed 128-bit integer bounds.
    QuantityOutOfBounds(BigInt),
}

impl Display for ValueError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueError::KeyTooLong(len) => write!(
                f,
                "Value key exceeds maximum length of {VALUE_MAX_KEY_LEN} bytes: got {len} bytes"
            ),
            ValueError::QuantityOutOfBounds(q) => {
                write!(f, "Value quantity out of signed 128-bit integer bounds: {q}")
            }
        }
    }
}

impl std::error::Error for ValueError {}

/// The underlying type of the UPLC built-in type `Value`.
///
/// This is a *normalized* nested association list mirroring plutus's
/// `Map CurrencySymbol (Map TokenName Quantity)` (see
/// `plutus-core/plutus-core/src/PlutusCore/Value.hs`).
///
/// Invariants (enforced by every constructor):
///   * outer keys (currency symbols) are strictly ascending and unique;
///   * inner keys (token names) are strictly ascending and unique;
///   * no inner map is empty;
///   * no quantity is zero;
///   * every key is at most [`VALUE_MAX_KEY_LEN`] bytes;
///   * every quantity is a valid signed 128-bit integer.
///
/// Keys are ordered lexicographically by their raw bytes, exactly matching
/// the `Ord ByteString` instance plutus relies on.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Value {
    // Outer: (currency symbol, inner association list of (token name, quantity)).
    inner: Vec<(Vec<u8>, Vec<(Vec<u8>, i128)>)>,
}

impl Value {
    /// The empty `Value`.
    pub fn empty() -> Self {
        Value { inner: Vec::new() }
    }

    /// Build a normalized `Value` from a (possibly ill-formed) list of entries.
    ///
    /// This mirrors `PlutusCore.Value.fromList`: duplicate currency/token keys
    /// are merged by *summing* their quantities (using arbitrary-precision
    /// arithmetic so a transient overflow during merge is not falsely
    /// rejected), then the resulting nested map is normalized (zero quantities
    /// and empty inner maps dropped, keys sorted/deduplicated) and every final
    /// quantity is validated to be within the signed 128-bit integer bounds.
    pub fn from_entries(
        entries: Vec<(Vec<u8>, Vec<(Vec<u8>, BigInt)>)>,
    ) -> Result<Self, ValueError> {
        // Outer map: currency -> (inner map: token -> summed quantity).
        let mut outer: Vec<(Vec<u8>, Vec<(Vec<u8>, BigInt)>)> = Vec::new();

        for (currency, tokens) in entries {
            if currency.len() > VALUE_MAX_KEY_LEN {
                return Err(ValueError::KeyTooLong(currency.len()));
            }

            let inner = match outer.iter_mut().find(|(c, _)| *c == currency) {
                Some((_, inner)) => inner,
                None => {
                    outer.push((currency, Vec::new()));
                    &mut outer.last_mut().expect("just pushed").1
                }
            };

            for (token, quantity) in tokens {
                if token.len() > VALUE_MAX_KEY_LEN {
                    return Err(ValueError::KeyTooLong(token.len()));
                }

                match inner.iter_mut().find(|(t, _)| *t == token) {
                    // Unchecked (arbitrary-precision) addition while merging.
                    Some((_, q)) => *q += quantity,
                    None => inner.push((token, quantity)),
                }
            }
        }

        // Normalize: drop zero quantities, validate bounds, sort token names,
        // drop empty inner maps, sort currency symbols.
        let mut normalized: Vec<(Vec<u8>, Vec<(Vec<u8>, i128)>)> = Vec::new();

        for (currency, tokens) in outer {
            let mut inner: Vec<(Vec<u8>, i128)> = Vec::new();

            for (token, quantity) in tokens {
                if quantity.is_zero() {
                    continue;
                }

                let quantity = i128::try_from(&quantity)
                    .map_err(|_| ValueError::QuantityOutOfBounds(quantity.clone()))?;

                inner.push((token, quantity));
            }

            if inner.is_empty() {
                continue;
            }

            inner.sort_by(|(a, _), (b, _)| a.cmp(b));
            normalized.push((currency, inner));
        }

        normalized.sort_by(|(a, _), (b, _)| a.cmp(b));

        Ok(Value { inner: normalized })
    }

    /// The raw, normalized nested association list backing this `Value`.
    pub fn entries(&self) -> &[(Vec<u8>, Vec<(Vec<u8>, i128)>)] {
        &self.inner
    }

    /// Total size: the number of distinct `(currency, token)` pairs. This is
    /// plutus's `Value.totalSize`, used as the default `memoryUsage` of a
    /// `Value` for costing.
    pub fn total_size(&self) -> usize {
        self.inner.iter().map(|(_, inner)| inner.len()).sum()
    }

    /// Size of the largest inner map (plutus's `Value.maxInnerSize`).
    pub fn max_inner_size(&self) -> usize {
        self.inner
            .iter()
            .map(|(_, inner)| inner.len())
            .max()
            .unwrap_or(0)
    }

    /// Number of policies (outer map size).
    pub fn outer_size(&self) -> usize {
        self.inner.len()
    }

    /// The number of negative quantities contained in this `Value`
    /// (plutus's `Value.negativeAmounts`).
    pub fn negative_amounts(&self) -> usize {
        self.inner
            .iter()
            .flat_map(|(_, inner)| inner.iter())
            .filter(|(_, q)| *q < 0)
            .count()
    }
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
        PlutusData::BigInt(to_pallas_bigint(&i))
    }

    pub fn bytestring(bytes: Vec<u8>) -> PlutusData {
        PlutusData::BoundedBytes(bytes.into())
    }

    pub fn map(kvs: Vec<(PlutusData, PlutusData)>) -> PlutusData {
        PlutusData::Map(kvs.into())
    }

    pub fn list(xs: Vec<PlutusData>) -> PlutusData {
        PlutusData::Array(if xs.is_empty() {
            conway::MaybeIndefArray::Def(xs)
        } else {
            conway::MaybeIndefArray::Indef(xs)
        })
    }

    pub fn constr(ix: u64, fields: Vec<PlutusData>) -> PlutusData {
        let fields = if fields.is_empty() {
            conway::MaybeIndefArray::Def(fields)
        } else {
            conway::MaybeIndefArray::Indef(fields)
        };

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
    Array(Rc<Type>),
    Value,
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
            Type::Array(t) => write!(f, "array {t}"),
            Type::Value => write!(f, "value"),
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
        self.unique == other.unique && self.text == other.text
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
            Language::PlutusV3,
            CostModel::default(),
            initial_budget,
            200,
        );

        let term = machine.run(self.term);

        EvalResult::new(
            term,
            machine.ex_budget,
            initial_budget,
            machine.traces,
            machine.spend_counter.map(|i| i.into()),
        )
    }

    /// Evaluate a Program as a specific PlutusVersion
    pub fn eval_version(self, initial_budget: ExBudget, version: &Language) -> EvalResult {
        let mut machine = Machine::new(version.clone(), CostModel::default(), initial_budget, 200);

        let term = machine.run(self.term);

        EvalResult::new(
            term,
            machine.ex_budget,
            initial_budget,
            machine.traces,
            machine.spend_counter.map(|i| i.into()),
        )
    }

    /// Evaluate a Program as a specific PlutusVersion and protocol version,
    /// using the default cost model for the chosen PlutusVersion.
    pub fn eval_version_with_protocol(
        self,
        initial_budget: ExBudget,
        version: &Language,
        protocol_major_version: u16,
    ) -> EvalResult {
        let mut machine = Machine::new_with_protocol(
            version.clone(),
            protocol_major_version,
            CostModel::default(),
            initial_budget,
            200,
        );

        let term = machine.run(self.term);

        EvalResult::new(
            term,
            machine.ex_budget,
            initial_budget,
            machine.traces,
            machine.spend_counter.map(|i| i.into()),
        )
    }

    /// Evaluate the program in *counting* mode: the machine measures the budget
    /// that would be consumed but never fails when a cap is exceeded. This
    /// mirrors Plutus' `counting` evaluation mode used to generate the
    /// conformance budget goldens (where, e.g., `dropList` with a huge count
    /// reports a saturated `i64::MAX` cost while still succeeding).
    pub fn eval_version_counting(self, version: &Language) -> EvalResult {
        let initial_budget = ExBudget::counting();

        let mut machine =
            Machine::new_counting(version.clone(), CostModel::default(), initial_budget, 200);

        let term = machine.run(self.term);

        EvalResult::new(
            term,
            machine.ex_budget,
            initial_budget,
            machine.traces,
            machine.spend_counter.map(|i| i.into()),
        )
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

        EvalResult::new(
            term,
            machine.ex_budget,
            budget,
            machine.traces,
            machine.spend_counter.map(|i| i.into()),
        )
    }

    /// Evaluate a Program with an explicit ledger cost model and protocol
    /// version.
    pub fn eval_as_with_protocol(
        self,
        version: &Language,
        protocol_major_version: u16,
        costs: &[i64],
        initial_budget: Option<&ExBudget>,
    ) -> EvalResult {
        let budget = initial_budget.copied().unwrap_or_default();

        let mut machine = Machine::new_with_protocol(
            version.clone(),
            protocol_major_version,
            initialize_cost_model_with_protocol(version, protocol_major_version, costs),
            budget,
            200, //slippage
        );

        let term = machine.run(self.term);

        EvalResult::new(
            term,
            machine.ex_budget,
            budget,
            machine.traces,
            machine.spend_counter.map(|i| i.into()),
        )
    }

    pub fn eval_debug(self, initial_budget: ExBudget, version: &Language) -> EvalResult {
        let mut machine = Machine::new_debug(
            version.clone(),
            CostModel::default(),
            initial_budget,
            200, //slippage
        );

        let term = machine.run(self.term);

        EvalResult::new(
            term,
            machine.ex_budget,
            initial_budget,
            machine.traces,
            machine.spend_counter.map(|i| i.into()),
        )
    }
}

impl Program<DeBruijn> {
    pub fn eval(&self, initial_budget: ExBudget) -> EvalResult {
        let program: Program<NamedDeBruijn> = self.clone().into();
        program.eval(initial_budget)
    }

    pub fn eval_version(self, initial_budget: ExBudget, version: &Language) -> EvalResult {
        let program: Program<NamedDeBruijn> = self.clone().into();
        program.eval_version(initial_budget, version)
    }

    pub fn eval_version_with_protocol(
        self,
        initial_budget: ExBudget,
        version: &Language,
        protocol_major_version: u16,
    ) -> EvalResult {
        let program: Program<NamedDeBruijn> = self.clone().into();
        program.eval_version_with_protocol(initial_budget, version, protocol_major_version)
    }
}

impl Term<NamedDeBruijn> {
    pub fn is_valid_script_result(&self) -> bool {
        !matches!(self, Term::Error)
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Data, Value, ValueError};
    use num_bigint::{BigInt, Sign};
    use pallas_codec::minicbor;

    // Data's negative integers are encoded with an offset of 1, as an unsigned payload. This is unlike
    // num_bigint's BigInt; so both types representations aren't quite compatible with one another.
    #[test]
    fn integer_bigint_negative() {
        let large_negative_num: BigInt = BigInt::from(i128::MIN) - 1;

        let mut buf = vec![];
        minicbor::encode(Data::integer(large_negative_num.clone()), &mut buf)
            .expect("failed to encode bigint to CBOR");

        // NOTE: [2..] removes the CBOR tag and bytes len declaration.
        let large_negative_num_decoded = BigInt::from_bytes_be(Sign::Plus, &buf[2..]);

        assert_eq!(large_negative_num_decoded, -1 - large_negative_num);
    }

    fn entries(value: &Value) -> Vec<(Vec<u8>, Vec<(Vec<u8>, i128)>)> {
        value.entries().to_vec()
    }

    #[test]
    fn value_merges_duplicate_token_keys() {
        let v = Value::from_entries(vec![(
            vec![],
            vec![(vec![], 123.into()), (vec![], 456.into())],
        )])
        .unwrap();

        assert_eq!(entries(&v), vec![(vec![], vec![(vec![], 579)])]);
    }

    #[test]
    fn value_drops_zero_quantities_and_empty_inner_maps() {
        let v = Value::from_entries(vec![
            (vec![], vec![(vec![], 0.into()), (vec![0xaa], 1.into())]),
            (vec![0x01], vec![(vec![], 0.into())]),
            (vec![0x02], vec![]),
        ])
        .unwrap();

        // Only the (#, [(#aa, 1)]) entry survives; the all-zero and empty
        // currencies are dropped.
        assert_eq!(entries(&v), vec![(vec![], vec![(vec![0xaa], 1)])]);
    }

    #[test]
    fn value_sorts_keys_lexicographically() {
        let v = Value::from_entries(vec![
            (vec![0xff, 0xff], vec![(vec![0xbb], 123.into()), (vec![0xaa], 456.into())]),
            (vec![0xaa], vec![(vec![0xaa], 123.into())]),
            (vec![], vec![(vec![0xaa], 123.into())]),
        ])
        .unwrap();

        assert_eq!(
            entries(&v),
            vec![
                (vec![], vec![(vec![0xaa], 123)]),
                (vec![0xaa], vec![(vec![0xaa], 123)]),
                (vec![0xff, 0xff], vec![(vec![0xaa], 456), (vec![0xbb], 123)]),
            ]
        );
    }

    #[test]
    fn value_accepts_i128_bounds() {
        let max = Value::from_entries(vec![(vec![], vec![(vec![], i128::MAX.into())])]).unwrap();
        assert_eq!(entries(&max), vec![(vec![], vec![(vec![], i128::MAX)])]);

        let min = Value::from_entries(vec![(vec![], vec![(vec![], i128::MIN.into())])]).unwrap();
        assert_eq!(entries(&min), vec![(vec![], vec![(vec![], i128::MIN)])]);
    }

    #[test]
    fn value_rejects_out_of_bounds_quantities() {
        let overflow: BigInt = BigInt::from(i128::MAX) + 1;
        assert_eq!(
            Value::from_entries(vec![(vec![], vec![(vec![], overflow.clone())])]),
            Err(ValueError::QuantityOutOfBounds(overflow))
        );

        let underflow: BigInt = BigInt::from(i128::MIN) - 1;
        assert_eq!(
            Value::from_entries(vec![(vec![], vec![(vec![], underflow.clone())])]),
            Err(ValueError::QuantityOutOfBounds(underflow))
        );
    }

    #[test]
    fn value_accepts_max_key_length() {
        let key = vec![0xaa; 32];
        let v = Value::from_entries(vec![(key.clone(), vec![(key.clone(), 1.into())])]).unwrap();
        assert_eq!(entries(&v), vec![(key.clone(), vec![(key, 1)])]);
    }

    #[test]
    fn value_rejects_keys_that_are_too_long() {
        let long = vec![0xaa; 33];

        assert_eq!(
            Value::from_entries(vec![(long.clone(), vec![(vec![], 1.into())])]),
            Err(ValueError::KeyTooLong(33))
        );

        assert_eq!(
            Value::from_entries(vec![(vec![], vec![(long, 1.into())])]),
            Err(ValueError::KeyTooLong(33))
        );
    }

    #[test]
    fn value_size_metrics() {
        let v = Value::from_entries(vec![
            (vec![], vec![(vec![], 1.into()), (vec![0xaa], (-2).into())]),
            (vec![0xff], vec![(vec![0xbb], 3.into())]),
        ])
        .unwrap();

        assert_eq!(v.total_size(), 3);
        assert_eq!(v.max_inner_size(), 2);
        assert_eq!(v.outer_size(), 2);
        assert_eq!(v.negative_amounts(), 1);
    }
}
