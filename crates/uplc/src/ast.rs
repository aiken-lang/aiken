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
use num_traits::{One, Zero};
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
    collections::BTreeMap,
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
    // tag: 13
    Value(Value),
}

impl Constant {
    /// Whether this constant contains a native `Value`, which is only
    /// available from Plutus V3 / protocol version 11 (Van Rossem) onwards.
    pub fn contains_value(&self) -> bool {
        match self {
            Constant::Value(_) => true,
            Constant::ProtoList(r#type, elements) => {
                r#type.contains_value() || elements.iter().any(Constant::contains_value)
            }
            Constant::ProtoPair(fst, snd, left, right) => {
                fst.contains_value()
                    || snd.contains_value()
                    || left.contains_value()
                    || right.contains_value()
            }
            _ => false,
        }
    }
}

pub const VALUE_MAX_KEY_LEN: usize = 32;
pub const VALUE_DATA_MAX_SIZE: usize = 40_000;

pub type ValueEntry<Quantity> = (Vec<u8>, Vec<(Vec<u8>, Quantity)>);
pub type ValueEntries = Vec<ValueEntry<i128>>;
pub type BigIntValueEntries = Vec<ValueEntry<BigInt>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValueError {
    KeyTooLong(usize),
    QuantityOutOfBounds(BigInt),
    DataQuantityOutOfBounds,
    CurrencySymbolsNotStrictlyAscending,
    TokenNamesNotStrictlyAscending,
    EmptyInnerMap,
    ZeroQuantity,
    FirstValueContainsNegativeAmounts,
    SecondValueContainsNegativeAmounts,
    ExpectedDataMap,
    ExpectedDataBytes,
    ExpectedDataInteger,
    ValueDataInputTooLarge(usize),
}

impl Display for ValueError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::KeyTooLong(length) => write!(
                f,
                "Value key exceeds maximum length of {VALUE_MAX_KEY_LEN} bytes: got {length} bytes"
            ),
            Self::QuantityOutOfBounds(quantity) => write!(
                f,
                "Value quantity out of signed 128-bit integer bounds: {quantity}"
            ),
            Self::DataQuantityOutOfBounds => {
                f.write_str("Value quantity out of signed 128-bit integer bounds")
            }
            Self::CurrencySymbolsNotStrictlyAscending => {
                f.write_str("Value currency symbols are not strictly ascending")
            }
            Self::TokenNamesNotStrictlyAscending => {
                f.write_str("Value token names are not strictly ascending")
            }
            Self::EmptyInnerMap => f.write_str("Value contains an empty inner map"),
            Self::ZeroQuantity => f.write_str("Value contains a zero quantity"),
            Self::FirstValueContainsNegativeAmounts => {
                f.write_str("valueContains: first value contains negative amounts")
            }
            Self::SecondValueContainsNegativeAmounts => {
                f.write_str("valueContains: second value contains negative amounts")
            }
            Self::ExpectedDataMap => f.write_str("unValueData: non-Map constructor"),
            Self::ExpectedDataBytes => f.write_str("unValueData: non-B constructor"),
            Self::ExpectedDataInteger => f.write_str("unValueData: non-I constructor"),
            Self::ValueDataInputTooLarge(size) => write!(
                f,
                "valueData: maximum input size ({VALUE_DATA_MAX_SIZE}) exceeded: got {size}"
            ),
        }
    }
}

impl std::error::Error for ValueError {}

type ValueInner = BTreeMap<Vec<u8>, i128>;
type ValueMap = BTreeMap<Vec<u8>, ValueInner>;

#[cfg(test)]
std::thread_local! {
    static VALUE_DATA_KEY_COPIES: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

#[derive(Debug, Clone)]
pub struct Value {
    entries: ValueMap,
    total_size: usize,
    max_inner_size: usize,
    negative_amounts: usize,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        self.entries == other.entries
    }
}

impl Eq for Value {}

impl Default for Value {
    fn default() -> Self {
        Self::empty()
    }
}

impl Value {
    pub fn empty() -> Self {
        Self {
            entries: ValueMap::new(),
            total_size: 0,
            max_inner_size: 0,
            negative_amounts: 0,
        }
    }

    pub fn from_canonical_entries(entries: BigIntValueEntries) -> Result<Self, ValueError> {
        Self::from_strict_entries(entries, BigInt::is_zero, |quantity| {
            i128::try_from(&quantity).map_err(|_| ValueError::QuantityOutOfBounds(quantity))
        })
    }

    pub fn from_canonical_bounded_entries(entries: ValueEntries) -> Result<Self, ValueError> {
        Self::from_strict_entries(entries, |quantity| *quantity == 0, Ok)
    }

    fn from_strict_entries<Quantity>(
        entries: Vec<ValueEntry<Quantity>>,
        is_zero: impl Fn(&Quantity) -> bool,
        into_bounded: impl Fn(Quantity) -> Result<i128, ValueError>,
    ) -> Result<Self, ValueError> {
        let mut canonical = ValueEntries::with_capacity(entries.len());

        for (currency, tokens) in entries {
            Self::check_key(&currency)?;
            if canonical
                .last()
                .is_some_and(|(previous, _)| previous.as_slice() >= currency.as_slice())
            {
                return Err(ValueError::CurrencySymbolsNotStrictlyAscending);
            }
            if tokens.is_empty() {
                return Err(ValueError::EmptyInnerMap);
            }

            let mut inner: Vec<(Vec<u8>, i128)> = Vec::with_capacity(tokens.len());

            for (token, quantity) in tokens {
                Self::check_key(&token)?;
                if inner
                    .last()
                    .is_some_and(|(previous, _)| previous.as_slice() >= token.as_slice())
                {
                    return Err(ValueError::TokenNamesNotStrictlyAscending);
                }
                if is_zero(&quantity) {
                    return Err(ValueError::ZeroQuantity);
                }

                inner.push((token, into_bounded(quantity)?));
            }

            canonical.push((currency, inner));
        }

        Ok(Self::from_normalized(canonical))
    }

    pub(crate) fn iter(
        &self,
    ) -> impl Iterator<Item = (&Vec<u8>, impl Iterator<Item = (&Vec<u8>, &i128)>)> {
        self.entries
            .iter()
            .map(|(currency, inner)| (currency, inner.iter()))
    }

    pub fn into_entries(self) -> ValueEntries {
        self.entries
            .into_iter()
            .map(|(currency, inner)| (currency, inner.into_iter().collect()))
            .collect()
    }

    pub fn total_size(&self) -> usize {
        self.total_size
    }

    pub fn max_inner_size(&self) -> usize {
        self.max_inner_size
    }

    pub fn outer_size(&self) -> usize {
        self.entries.len()
    }

    pub fn negative_amounts(&self) -> usize {
        self.negative_amounts
    }

    pub fn insert_coin(
        &self,
        currency: &[u8],
        token: &[u8],
        quantity: &BigInt,
    ) -> Result<Self, ValueError> {
        if quantity.is_zero() {
            if self
                .entries
                .get(currency)
                .is_none_or(|inner| !inner.contains_key(token))
            {
                return Ok(self.clone());
            }

            let mut entries = self.entries.clone();
            let inner = entries
                .get_mut(currency)
                .expect("deleted coin's currency exists in the value");
            inner.remove(token);
            if inner.is_empty() {
                entries.remove(currency);
            }

            return Ok(Self::from_map(entries));
        }

        Self::check_key(currency)?;
        Self::check_key(token)?;
        let quantity = i128::try_from(quantity)
            .map_err(|_| ValueError::QuantityOutOfBounds(quantity.clone()))?;

        if self
            .entries
            .get(currency)
            .and_then(|inner| inner.get(token))
            == Some(&quantity)
        {
            return Ok(self.clone());
        }

        let mut entries = self.entries.clone();
        entries
            .entry(currency.to_vec())
            .or_default()
            .insert(token.to_vec(), quantity);

        Ok(Self::from_map(entries))
    }

    pub fn lookup_coin(&self, currency: &[u8], token: &[u8]) -> i128 {
        self.entries
            .get(currency)
            .and_then(|inner| inner.get(token))
            .copied()
            .unwrap_or(0)
    }

    pub fn union(&self, other: &Self) -> Result<Self, ValueError> {
        if self.total_size == 0 {
            return Ok(other.clone());
        }
        if other.total_size == 0 {
            return Ok(self.clone());
        }

        let mut entries = self.entries.clone();

        for (currency, right_tokens) in &other.entries {
            let merged = match entries.get(currency) {
                Some(left_tokens) => Self::union_inner(left_tokens, right_tokens)?,
                None => right_tokens.clone(),
            };
            if merged.is_empty() {
                entries.remove(currency);
            } else {
                entries.insert(currency.clone(), merged);
            }
        }

        Ok(Self::from_map(entries))
    }

    fn union_inner(left: &ValueInner, right: &ValueInner) -> Result<ValueInner, ValueError> {
        let mut merged = left.clone();

        for (token, quantity) in right {
            let existing = merged.get(token).copied().unwrap_or(0);
            let combined = existing.checked_add(*quantity).ok_or_else(|| {
                ValueError::QuantityOutOfBounds(BigInt::from(existing) + BigInt::from(*quantity))
            })?;
            if combined == 0 {
                merged.remove(token);
            } else {
                merged.insert(token.clone(), combined);
            }
        }

        Ok(merged)
    }

    pub fn contains(&self, other: &Self) -> Result<bool, ValueError> {
        if self.negative_amounts != 0 {
            return Err(ValueError::FirstValueContainsNegativeAmounts);
        }
        if other.negative_amounts != 0 {
            return Err(ValueError::SecondValueContainsNegativeAmounts);
        }
        if self.total_size < other.total_size {
            return Ok(false);
        }

        Ok(other.entries.iter().all(|(currency, tokens)| {
            tokens
                .iter()
                .all(|(token, quantity)| self.lookup_coin(currency, token) >= *quantity)
        }))
    }

    pub fn scale(&self, scalar: &BigInt) -> Result<Self, ValueError> {
        if scalar.is_zero() {
            return Ok(Self::empty());
        }
        if scalar.is_one() {
            return Ok(self.clone());
        }

        let mut entries = Vec::with_capacity(self.entries.len());
        for (currency, tokens) in self.entries.iter() {
            let mut inner = Vec::with_capacity(tokens.len());
            for (token, quantity) in tokens.iter() {
                let product = scalar * BigInt::from(*quantity);
                let bounded = i128::try_from(&product)
                    .map_err(|_| ValueError::QuantityOutOfBounds(product))?;
                inner.push((token.clone(), bounded));
            }
            entries.push((currency.clone(), inner));
        }

        Ok(Self::from_normalized(entries))
    }

    fn to_data_unchecked(&self) -> PlutusData {
        Data::map(
            self.entries
                .iter()
                .map(|(currency, tokens)| {
                    (
                        Data::bytestring(currency.clone()),
                        Data::map(
                            tokens
                                .iter()
                                .map(|(token, quantity)| {
                                    (
                                        Data::bytestring(token.clone()),
                                        Data::integer(BigInt::from(*quantity)),
                                    )
                                })
                                .collect(),
                        ),
                    )
                })
                .collect(),
        )
    }

    pub fn to_data_checked(&self) -> Result<PlutusData, ValueError> {
        if self.total_size > VALUE_DATA_MAX_SIZE {
            Err(ValueError::ValueDataInputTooLarge(self.total_size))
        } else {
            Ok(self.to_data_unchecked())
        }
    }

    pub fn from_data(data: &PlutusData) -> Result<Self, ValueError> {
        let PlutusData::Map(outer) = data else {
            return Err(ValueError::ExpectedDataMap);
        };
        let mut entries = ValueEntries::with_capacity(outer.len());

        for (currency, tokens) in outer.iter() {
            let PlutusData::BoundedBytes(currency) = currency else {
                return Err(ValueError::ExpectedDataBytes);
            };
            Self::check_key(currency)?;

            let PlutusData::Map(tokens) = tokens else {
                return Err(ValueError::ExpectedDataMap);
            };

            if entries
                .last()
                .is_some_and(|(previous, _)| previous.as_slice() >= currency.as_slice())
            {
                return Err(ValueError::CurrencySymbolsNotStrictlyAscending);
            }

            let mut inner: Vec<(Vec<u8>, i128)> = Vec::with_capacity(tokens.len());
            for (token, quantity) in tokens.iter() {
                let PlutusData::BoundedBytes(token) = token else {
                    return Err(ValueError::ExpectedDataBytes);
                };
                Self::check_key(token)?;

                let PlutusData::BigInt(quantity) = quantity else {
                    return Err(ValueError::ExpectedDataInteger);
                };
                let quantity = pallas_bigint_to_i128(quantity)?;

                if inner
                    .last()
                    .is_some_and(|(previous, _)| previous.as_slice() >= token.as_slice())
                {
                    return Err(ValueError::TokenNamesNotStrictlyAscending);
                }
                if quantity == 0 {
                    return Err(ValueError::ZeroQuantity);
                }

                inner.push((Self::clone_data_key(token), quantity));
            }

            if inner.is_empty() {
                return Err(ValueError::EmptyInnerMap);
            }

            entries.push((Self::clone_data_key(currency), inner));
        }

        Ok(Self::from_normalized(entries))
    }

    fn check_key(key: &[u8]) -> Result<(), ValueError> {
        if key.len() > VALUE_MAX_KEY_LEN {
            Err(ValueError::KeyTooLong(key.len()))
        } else {
            Ok(())
        }
    }

    fn clone_data_key(key: &[u8]) -> Vec<u8> {
        #[cfg(test)]
        VALUE_DATA_KEY_COPIES.with(|copies| copies.set(copies.get() + 1));

        key.to_vec()
    }

    fn from_normalized(entries: ValueEntries) -> Self {
        Self::from_map(
            entries
                .into_iter()
                .map(|(currency, inner)| (currency, inner.into_iter().collect()))
                .collect(),
        )
    }

    fn from_map(entries: ValueMap) -> Self {
        let mut total_size = 0;
        let mut max_inner_size = 0;
        let mut negative_amounts = 0;

        for inner in entries.values() {
            total_size += inner.len();
            max_inner_size = max_inner_size.max(inner.len());
            negative_amounts += inner
                .values()
                .filter(|quantity| quantity.is_negative())
                .count();
        }

        Self {
            entries,
            total_size,
            max_inner_size,
            negative_amounts,
        }
    }

    #[cfg(test)]
    fn reset_data_key_copy_count() {
        VALUE_DATA_KEY_COPIES.with(|copies| copies.set(0));
    }

    #[cfg(test)]
    fn data_key_copy_count() -> usize {
        VALUE_DATA_KEY_COPIES.with(std::cell::Cell::get)
    }
}

fn pallas_bigint_to_i128(quantity: &conway::BigInt) -> Result<i128, ValueError> {
    let magnitude = match quantity {
        conway::BigInt::Int(quantity) => return Ok(i128::from(*quantity)),
        conway::BigInt::BigUInt(bytes) | conway::BigInt::BigNInt(bytes) => bytes,
    };
    let first_nonzero = magnitude
        .iter()
        .position(|byte| *byte != 0)
        .unwrap_or(magnitude.len());
    let magnitude = &magnitude[first_nonzero..];

    if magnitude.len() > i128::BITS as usize / 8
        || (magnitude.len() == i128::BITS as usize / 8 && magnitude[0] & 0x80 != 0)
    {
        return Err(ValueError::DataQuantityOutOfBounds);
    }

    i128::try_from(crate::machine::value::from_pallas_bigint(quantity))
        .map_err(|_| ValueError::DataQuantityOutOfBounds)
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
    Value,
}

impl Type {
    /// Whether this type mentions the native `Value` type, which is only
    /// available from Plutus V3 / protocol version 11 (Van Rossem) onwards.
    pub fn contains_value(&self) -> bool {
        match self {
            Type::Value => true,
            Type::List(r#type) => r#type.contains_value(),
            Type::Pair(fst, snd) => fst.contains_value() || snd.contains_value(),
            _ => false,
        }
    }
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
    /// using the local protocol-aware default cost model for the chosen
    /// PlutusVersion.
    pub fn eval_version_with_protocol(
        self,
        initial_budget: ExBudget,
        version: &Language,
        protocol_major_version: u16,
    ) -> EvalResult {
        let mut machine = Machine::new_with_protocol(
            version.clone(),
            protocol_major_version,
            CostModel::default_for_language_and_protocol(version, protocol_major_version),
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
    use crate::ast::{Data, Value, ValueEntries, ValueError};
    use num_bigint::{BigInt, Sign};
    use pallas_codec::minicbor;
    use pallas_primitives::{alonzo::PlutusData, conway};
    use proptest::prelude::*;
    use std::collections::BTreeMap;

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

    fn data_value_with_quantity(quantity: conway::BigInt) -> PlutusData {
        Data::map(vec![(
            Data::bytestring(vec![0]),
            Data::map(vec![(
                Data::bytestring(vec![0]),
                PlutusData::BigInt(quantity),
            )]),
        )])
    }

    #[test]
    fn insert_coin_updates_size_metadata_at_max_boundaries() {
        let original = Value::from_canonical_bounded_entries(vec![
            (vec![0], vec![(vec![0], -1), (vec![1], 1), (vec![2], 1)]),
            (vec![1], vec![(vec![0], 2), (vec![1], 3)]),
            (vec![2], vec![(vec![0], -4)]),
        ])
        .unwrap();
        assert_eq!(original.total_size(), 6);
        assert_eq!(original.outer_size(), 3);
        assert_eq!(original.max_inner_size(), 3);
        assert_eq!(original.negative_amounts(), 2);

        let largest_shrunk = original.insert_coin(&[0], &[0], &BigInt::from(0)).unwrap();
        assert_eq!(largest_shrunk.total_size(), 5);
        assert_eq!(largest_shrunk.max_inner_size(), 2);
        assert_eq!(largest_shrunk.negative_amounts(), 1);

        let quantity_overwritten = largest_shrunk
            .insert_coin(&[1], &[0], &BigInt::from(-2))
            .unwrap();
        assert_eq!(quantity_overwritten.total_size(), 5);
        assert_eq!(quantity_overwritten.max_inner_size(), 2);
        assert_eq!(quantity_overwritten.negative_amounts(), 2);

        let inner_removed = quantity_overwritten
            .insert_coin(&[2], &[0], &BigInt::from(0))
            .unwrap();
        assert_eq!(inner_removed.total_size(), 4);
        assert_eq!(inner_removed.outer_size(), 2);
        assert_eq!(inner_removed.max_inner_size(), 2);
        assert_eq!(inner_removed.negative_amounts(), 1);

        let singleton =
            Value::from_canonical_bounded_entries(vec![(vec![0], vec![(vec![0], 1)])]).unwrap();
        let empty = singleton.insert_coin(&[0], &[0], &BigInt::from(0)).unwrap();
        assert_eq!(empty.total_size(), 0);
        assert_eq!(empty.outer_size(), 0);
        assert_eq!(empty.max_inner_size(), 0);
    }

    #[test]
    fn value_preserves_canonical_entries_and_data_roundtrip() {
        let entries = vec![
            (vec![], vec![(vec![], i128::MIN), (vec![0xff], i128::MAX)]),
            (vec![0xff; 32], vec![(vec![0; 32], -1)]),
        ];
        let value = Value::from_canonical_bounded_entries(entries.clone()).unwrap();

        assert_eq!(
            Value::from_data(&value.to_data_unchecked()),
            Ok(value.clone())
        );
        assert_eq!(value.into_entries(), entries);
    }

    #[test]
    fn from_data_rejects_oversized_keys_before_copying() {
        Value::reset_data_key_copy_count();
        let valid = data_value_with_quantity(conway::BigInt::BigUInt(vec![1].into()));
        Value::from_data(&valid).unwrap();
        assert_eq!(Value::data_key_copy_count(), 2);

        Value::reset_data_key_copy_count();
        let oversized_currency = vec![0xff; 256 * 1024];
        let data = Data::map(vec![(
            Data::bytestring(oversized_currency),
            Data::integer(BigInt::from(1)),
        )]);
        assert_eq!(
            Value::from_data(&data),
            Err(ValueError::KeyTooLong(256 * 1024))
        );
        assert_eq!(Value::data_key_copy_count(), 0);

        Value::reset_data_key_copy_count();
        let oversized_token = vec![0xff; 256 * 1024];
        let data = Data::map(vec![(
            Data::bytestring(vec![0]),
            Data::map(vec![(
                Data::bytestring(oversized_token),
                Data::bytestring(vec![0]),
            )]),
        )]);
        assert_eq!(
            Value::from_data(&data),
            Err(ValueError::KeyTooLong(256 * 1024))
        );
        assert_eq!(Value::data_key_copy_count(), 0);
    }

    #[test]
    fn from_data_accepts_leading_zero_pallas_bignums() {
        for (quantity, expected) in [
            (conway::BigInt::BigUInt(vec![0, 0, 1].into()), 1),
            (conway::BigInt::BigNInt(vec![0, 0, 1].into()), -2),
        ] {
            let data = data_value_with_quantity(quantity);
            assert_eq!(
                Value::from_data(&data).unwrap().lookup_coin(&[0], &[0]),
                expected
            );
        }
    }

    type ValueModel = BTreeMap<Vec<u8>, BTreeMap<Vec<u8>, i128>>;

    fn apply_model_update(model: &mut ValueModel, currency: u8, token: u8, quantity: i16) {
        let currency = vec![currency];
        let token = vec![token];
        if quantity == 0 {
            if let Some(tokens) = model.get_mut(&currency) {
                tokens.remove(&token);
                if tokens.is_empty() {
                    model.remove(&currency);
                }
            }
        } else {
            model
                .entry(currency)
                .or_default()
                .insert(token, i128::from(quantity));
        }
    }

    fn model_entries(model: &ValueModel) -> ValueEntries {
        model
            .iter()
            .map(|(currency, tokens)| {
                (
                    currency.clone(),
                    tokens
                        .iter()
                        .map(|(token, quantity)| (token.clone(), *quantity))
                        .collect(),
                )
            })
            .collect()
    }

    fn assert_value_matches_model(value: &Value, model: &ValueModel) {
        assert_eq!(value.clone().into_entries(), model_entries(model));
        assert_eq!(value.outer_size(), model.len());
        assert_eq!(
            value.total_size(),
            model.values().map(BTreeMap::len).sum::<usize>()
        );
        assert_eq!(
            value.max_inner_size(),
            model.values().map(BTreeMap::len).max().unwrap_or(0)
        );
        assert_eq!(
            value.negative_amounts(),
            model
                .values()
                .flat_map(BTreeMap::values)
                .filter(|quantity| quantity.is_negative())
                .count()
        );
        assert_eq!(
            Value::from_data(&value.to_data_unchecked()),
            Ok(value.clone())
        );
    }

    fn value_and_model(operations: &[(u8, u8, i16)]) -> (Value, ValueModel) {
        let mut value = Value::empty();
        let mut model = ValueModel::new();
        for (currency, token, quantity) in operations {
            value = value
                .insert_coin(&[*currency], &[*token], &BigInt::from(*quantity))
                .unwrap();
            apply_model_update(&mut model, *currency, *token, *quantity);
            assert_value_matches_model(&value, &model);
        }
        (value, model)
    }

    fn union_models(mut left: ValueModel, right: &ValueModel) -> ValueModel {
        for (currency, tokens) in right {
            for (token, quantity) in tokens {
                let combined = left
                    .get(currency)
                    .and_then(|tokens| tokens.get(token))
                    .copied()
                    .unwrap_or(0)
                    + quantity;
                if combined == 0 {
                    if let Some(tokens) = left.get_mut(currency) {
                        tokens.remove(token);
                        if tokens.is_empty() {
                            left.remove(currency);
                        }
                    }
                } else {
                    left.entry(currency.clone())
                        .or_default()
                        .insert(token.clone(), combined);
                }
            }
        }
        left
    }

    fn scale_model(model: &ValueModel, scalar: i16) -> ValueModel {
        if scalar == 0 {
            return ValueModel::new();
        }
        model
            .iter()
            .map(|(currency, tokens)| {
                (
                    currency.clone(),
                    tokens
                        .iter()
                        .map(|(token, quantity)| (token.clone(), quantity * i128::from(scalar)))
                        .collect(),
                )
            })
            .collect()
    }

    proptest! {
        #[test]
        fn value_operations_match_btree_model(
            left_operations in prop::collection::vec((any::<u8>(), any::<u8>(), -1000i16..=1000), 0..80),
            right_operations in prop::collection::vec((any::<u8>(), any::<u8>(), -1000i16..=1000), 0..80),
            scalar in -4i16..=4,
        ) {
            let (left, left_model) = value_and_model(&left_operations);
            let (right, right_model) = value_and_model(&right_operations);

            for (currency, tokens) in &left_model {
                for (token, quantity) in tokens {
                    prop_assert_eq!(left.lookup_coin(currency, token), *quantity);
                }
            }

            let union = left.union(&right).unwrap();
            let union_model = union_models(left_model.clone(), &right_model);
            assert_value_matches_model(&union, &union_model);

            let scaled = left.scale(&BigInt::from(scalar)).unwrap();
            assert_value_matches_model(&scaled, &scale_model(&left_model, scalar));

            let expected_contains = if left_model
                .values()
                .flat_map(BTreeMap::values)
                .any(|quantity| quantity.is_negative())
            {
                Err(ValueError::FirstValueContainsNegativeAmounts)
            } else if right_model
                .values()
                .flat_map(BTreeMap::values)
                .any(|quantity| quantity.is_negative())
            {
                Err(ValueError::SecondValueContainsNegativeAmounts)
            } else {
                Ok(right_model.iter().all(|(currency, tokens)| {
                    tokens.iter().all(|(token, quantity)| {
                        left_model
                            .get(currency)
                            .and_then(|tokens| tokens.get(token))
                            .copied()
                            .unwrap_or(0)
                            >= *quantity
                    })
                }))
            };
            prop_assert_eq!(left.contains(&right), expected_contains);
        }
    }
}
