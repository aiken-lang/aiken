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
    },
    optimize::interner::CodeGenInterner,
    tx::script_context::PlutusScript,
};
use num_bigint::BigInt;
use num_traits::{One, Zero};
use pallas_addresses::{Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart};
use pallas_primitives::{
    alonzo::PlutusData,
    conway::{self, Language},
};
use pallas_traverse::ComputeHash;
use serde::{
    self,
    de::{self, Deserialize, Deserializer, MapAccess, Visitor},
    ser::{Serialize, SerializeStruct, Serializer},
};
use std::{
    cell::{Cell, UnsafeCell},
    collections::BTreeMap,
    convert::AsRef,
    fmt::{self, Display},
    hash::{self, Hash},
    mem::MaybeUninit,
    rc::Rc,
};

pub use crate::data::Data;

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
    pub fn apply_data(&self, data: impl Into<Data>) -> Self {
        let applied_term = Term::Apply {
            function: Rc::new(self.term.clone()),
            argument: Rc::new(Term::Constant(Constant::Data(data.into()).into())),
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

impl<T: Clone> Term<T> {
    /// Rebuild the term with every `Rc` allocation freshly boxed, sharing
    /// nothing with `self`. Unlike `clone`, this only ever borrows existing
    /// allocations (it never bumps a reference count), so it is safe to call
    /// from several threads on terms that share `Rc` nodes — which `Rc`'s
    /// non-atomic counts would otherwise forbid.
    pub fn deep_clone(&self) -> Term<T> {
        match self {
            Term::Var(name) => Term::Var(Rc::new(name.as_ref().clone())),
            Term::Delay(term) => Term::Delay(Rc::new(term.deep_clone())),
            Term::Lambda {
                parameter_name,
                body,
            } => Term::Lambda {
                parameter_name: Rc::new(parameter_name.as_ref().clone()),
                body: Rc::new(body.deep_clone()),
            },
            Term::Apply { function, argument } => Term::Apply {
                function: Rc::new(function.deep_clone()),
                argument: Rc::new(argument.deep_clone()),
            },
            Term::Constant(constant) => Term::Constant(Rc::new(constant.deep_clone())),
            Term::Force(term) => Term::Force(Rc::new(term.deep_clone())),
            Term::Error => Term::Error,
            Term::Builtin(fun) => Term::Builtin(*fun),
            Term::Constr { tag, fields } => Term::Constr {
                tag: *tag,
                fields: fields.iter().map(|field| field.deep_clone()).collect(),
            },
            Term::Case { constr, branches } => Term::Case {
                constr: Rc::new(constr.deep_clone()),
                branches: branches.iter().map(|branch| branch.deep_clone()).collect(),
            },
        }
    }
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
                Constant::Data(data) => Ok(data.into()),
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
    // Elements are `Rc`-shared so list builtins (mkCons, tailList) can build
    // derived lists without deep-cloning every element, and the spine itself
    // is shared so tails are O(1); see `deep_clone` for the thread-isolation
    // caveat.
    ProtoList(Type, ListSpine),
    // tag: 6
    ProtoPair(Type, Type, Rc<Constant>, Rc<Constant>),
    // tag: 7
    // Apply(Box<Constant>, Type),
    // tag: 8
    Data(Data),
    Bls12_381G1Element(Box<blst::blst_p1>),
    Bls12_381G2Element(Box<blst::blst_p2>),
    Bls12_381MlResult(Box<blst::blst_fp12>),
    // tag: 13
    Value(Value),
}

/// The elements of a builtin list constant.
///
/// A view onto a shared, immutable spine: cloning it, or taking any of its
/// tails, is O(1) and allocates nothing. It dereferences to the slice of its
/// elements.
///
/// Spines keep free slots in front of their first element, so prepending to
/// the frontmost view of a spine (the usual case when a list is built up one
/// `mkCons` at a time, whether or not other views share it) writes into the
/// next free slot instead of copying: mkCons is amortised O(1).
#[derive(Clone)]
pub struct ListSpine {
    buf: Rc<SpineBuf>,
    start: usize,
}

/// Slots `front..` are initialised and never written again; slots `..front`
/// are free. Only the view starting at `front` may claim slot `front - 1`.
struct SpineBuf {
    slots: Box<[UnsafeCell<MaybeUninit<Rc<Constant>>>]>,
    front: Cell<usize>,
}

/// How many spines may be dropped inside one another before the elements of
/// deeper ones are queued instead, so that dropping a deeply nested list or
/// Data uses bounded stack.
const MAX_NESTED_SPINE_DROPS: usize = 512;

std::thread_local! {
    /// How many spines are being dropped inside one another on this thread.
    static SPINE_DROP_DEPTH: Cell<usize> = const { Cell::new(0) };

    /// Elements of spines dropped too deep, left for the outermost spine drop.
    static DEFERRED_SPINE_DROPS: std::cell::RefCell<Vec<Rc<Constant>>> =
        const { std::cell::RefCell::new(Vec::new()) };
}

impl Drop for SpineBuf {
    fn drop(&mut self) {
        let mut items = self.slots[self.front.get()..].iter_mut().map(|slot| {
            // SAFETY: slots from `front` onwards are initialised, and each is
            // read once here, after which the buffer is never read again.
            unsafe { slot.get_mut().assume_init_read() }
        });

        let depth = SPINE_DROP_DEPTH.get();

        if depth >= MAX_NESTED_SPINE_DROPS {
            // Only fails while the thread is exiting, when its locals are
            // being destroyed; the elements are then dropped right here.
            let _ =
                DEFERRED_SPINE_DROPS.try_with(|deferred| deferred.borrow_mut().extend(&mut items));
            items.for_each(drop);
            return;
        }

        SPINE_DROP_DEPTH.set(depth + 1);
        items.for_each(drop);

        if depth == 0 {
            while let Some(item) = DEFERRED_SPINE_DROPS
                .try_with(|deferred| deferred.borrow_mut().pop())
                .ok()
                .flatten()
            {
                drop(item);
            }
        }

        SPINE_DROP_DEPTH.set(depth);
    }
}

impl SpineBuf {
    /// A spine holding `items` with `free` free slots in front of them.
    fn new(free: usize, items: impl ExactSizeIterator<Item = Rc<Constant>>) -> SpineBuf {
        let mut slots = Vec::with_capacity(free + items.len());
        slots.extend((0..free).map(|_| UnsafeCell::new(MaybeUninit::uninit())));
        slots.extend(items.map(|item| UnsafeCell::new(MaybeUninit::new(item))));

        SpineBuf {
            slots: slots.into_boxed_slice(),
            front: Cell::new(free),
        }
    }
}

impl Default for ListSpine {
    fn default() -> Self {
        Vec::new().into()
    }
}

impl ListSpine {
    /// The list without its first `n` elements, or `None` if it has fewer.
    /// The tail shares this spine, so it also keeps the skipped elements
    /// alive until every view of the spine is dropped.
    pub fn skip(&self, n: usize) -> Option<ListSpine> {
        (n <= self.len()).then(|| ListSpine {
            buf: self.buf.clone(),
            start: self.start + n,
        })
    }

    /// The list with `item` prepended.
    pub fn cons(&self, item: Rc<Constant>) -> ListSpine {
        let front = self.buf.front.get();

        if self.start == front && front > 0 {
            let start = front - 1;

            // SAFETY: slot `start` is free, and only this view (the one
            // starting at `front`) may claim it. No reference into a free
            // slot exists, since views only expose slots from their start.
            unsafe { (*self.buf.slots[start].get()).write(item) };
            self.buf.front.set(start);

            return ListSpine {
                buf: self.buf.clone(),
                start,
            };
        }

        // Double the room in front so a chain of conses copies O(1)
        // amortised elements each.
        let free = self.len().max(4);

        ListSpine {
            buf: Rc::new(SpineBuf::new(
                free,
                std::iter::once(item)
                    .chain(self.iter().cloned())
                    .collect::<Vec<_>>()
                    .into_iter(),
            )),
            start: free,
        }
    }

    /// Whether both views start at the same element of the same spine, and
    /// so hold the same elements.
    pub(crate) fn ptr_eq(&self, other: &ListSpine) -> bool {
        Rc::ptr_eq(&self.buf, &other.buf) && self.start == other.start
    }

    pub fn into_vec(self) -> Vec<Rc<Constant>> {
        self.to_vec()
    }
}

impl std::ops::Deref for ListSpine {
    type Target = [Rc<Constant>];

    fn deref(&self) -> &Self::Target {
        let slots = &self.buf.slots[self.start..];

        // SAFETY: `start >= front`, so these slots are initialised and are
        // never written again while the spine is alive. `UnsafeCell` and
        // `MaybeUninit` are `repr(transparent)`, so the layouts match.
        unsafe { std::slice::from_raw_parts(slots.as_ptr().cast(), slots.len()) }
    }
}

impl From<Vec<Rc<Constant>>> for ListSpine {
    fn from(items: Vec<Rc<Constant>>) -> Self {
        items.into_iter().collect()
    }
}

impl FromIterator<Rc<Constant>> for ListSpine {
    fn from_iter<I: IntoIterator<Item = Rc<Constant>>>(iter: I) -> Self {
        // Without free slots, the elements are collected straight into the
        // spine's slots (in place, when they come from a `Vec`).
        let slots = iter
            .into_iter()
            .map(|item| UnsafeCell::new(MaybeUninit::new(item)))
            .collect();

        ListSpine {
            buf: Rc::new(SpineBuf {
                slots,
                front: Cell::new(0),
            }),
            start: 0,
        }
    }
}

impl IntoIterator for ListSpine {
    type Item = Rc<Constant>;
    type IntoIter = std::vec::IntoIter<Rc<Constant>>;

    fn into_iter(self) -> Self::IntoIter {
        self.into_vec().into_iter()
    }
}

impl<'a> IntoIterator for &'a ListSpine {
    type Item = &'a Rc<Constant>;
    type IntoIter = std::slice::Iter<'a, Rc<Constant>>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl PartialEq for ListSpine {
    fn eq(&self, other: &Self) -> bool {
        **self == **other
    }
}

impl std::fmt::Debug for ListSpine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(self.iter()).finish()
    }
}

impl Constant {
    /// Whether this constant contains a native `Value`, which is only
    /// available from Plutus V3 / protocol version 11 (Van Rossem) onwards.
    pub fn contains_value(constant: impl AsRef<Constant>) -> bool {
        match constant.as_ref() {
            Constant::Value(_) => true,
            Constant::ProtoList(r#type, elements) => {
                r#type.contains_value() || elements.iter().any(Constant::contains_value)
            }
            Constant::ProtoPair(fst, snd, left, right) => {
                fst.contains_value()
                    || snd.contains_value()
                    || Constant::contains_value(left)
                    || Constant::contains_value(right)
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

    fn to_data_unchecked(&self) -> Data {
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

    pub fn to_data_checked(&self) -> Result<Data, ValueError> {
        if self.total_size > VALUE_DATA_MAX_SIZE {
            Err(ValueError::ValueDataInputTooLarge(self.total_size))
        } else {
            Ok(self.to_data_unchecked())
        }
    }

    pub fn from_data(data: &Data) -> Result<Self, ValueError> {
        let Data::Map(outer) = data else {
            return Err(ValueError::ExpectedDataMap);
        };
        let mut entries = ValueEntries::with_capacity(outer.len());

        for (currency, tokens) in outer.iter() {
            let Data::BoundedBytes(currency) = currency else {
                return Err(ValueError::ExpectedDataBytes);
            };
            Self::check_key(currency)?;

            let Data::Map(tokens) = tokens else {
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
                let Data::BoundedBytes(token) = token else {
                    return Err(ValueError::ExpectedDataBytes);
                };
                Self::check_key(token)?;

                let Data::BigInt(quantity) = quantity else {
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

impl Constant {
    /// An equal constant sharing no allocation with `self`.
    ///
    /// `Rc` reference counts are not atomic, and neither is a list spine's
    /// record of its free slots, so a constant that may end up embedded in
    /// programs handled by different threads (e.g. tests run in parallel)
    /// must not share `Rc`s or spines with anything else.
    pub fn deep_clone(&self) -> Constant {
        match self {
            Constant::ProtoList(tipo, items) => Constant::ProtoList(
                tipo.deep_clone(),
                items
                    .iter()
                    .map(|item| Rc::new(item.deep_clone()))
                    .collect(),
            ),
            Constant::ProtoPair(fst_tipo, snd_tipo, fst, snd) => Constant::ProtoPair(
                fst_tipo.deep_clone(),
                snd_tipo.deep_clone(),
                Rc::new(fst.deep_clone()),
                Rc::new(snd.deep_clone()),
            ),
            Constant::Data(data) => Constant::Data(data.deep_clone()),
            // The remaining variants own their contents outright.
            other => other.clone(),
        }
    }
}

impl Type {
    /// An equal type sharing no allocation with `self` (see
    /// `Constant::deep_clone`).
    pub fn deep_clone(&self) -> Type {
        match self {
            Type::List(inner) => Type::List(Rc::new(inner.deep_clone())),
            Type::Pair(fst, snd) => {
                Type::Pair(Rc::new(fst.deep_clone()), Rc::new(snd.deep_clone()))
            }
            other => other.clone(),
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
    use pallas_primitives::conway;
    use proptest::prelude::*;
    use std::collections::BTreeMap;

    #[test]
    fn drops_deep_lists_in_bounded_stack() {
        use crate::ast::{Constant, ListSpine, Type};
        use std::rc::Rc;

        #[cfg(not(miri))]
        const DEPTH: usize = 1_000_000;
        #[cfg(miri)]
        const DEPTH: usize = 2_000;

        // Room for the nested drops allowed before queuing, even in a debug
        // build, but far too little to drop the whole nesting recursively.
        std::thread::Builder::new()
            .stack_size(4 * 1024 * 1024)
            .spawn(|| {
                let mut list = Rc::new(Constant::ProtoList(Type::Data, ListSpine::default()));
                let mut kept = Vec::new();

                for i in 0..DEPTH {
                    let spine = if i % 2 == 0 {
                        ListSpine::from(vec![list, Rc::new(Constant::Integer(i.into()))])
                    } else {
                        // The second prepend goes into a free slot in
                        // front of the first.
                        let Constant::ProtoList(_, spine) = list.as_ref() else {
                            unreachable!()
                        };
                        spine
                            .cons(Rc::new(Constant::Integer(i.into())))
                            .cons(Rc::new(Constant::Integer(i.into())))
                    };

                    list = Rc::new(Constant::ProtoList(Type::Data, spine));

                    // Shared below the top, so parts of the list outlive
                    // the first drop.
                    if i % (DEPTH / 4) == 0 {
                        kept.push(list.clone());
                    }
                }

                drop(list);
                drop(kept);
            })
            .unwrap()
            .join()
            .unwrap();
    }

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

    fn data_value_with_quantity(quantity: conway::BigInt) -> Data {
        Data::map(vec![(
            Data::bytestring(vec![0]),
            Data::map(vec![(Data::bytestring(vec![0]), Data::BigInt(quantity))]),
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

#[cfg(test)]
mod list_spine_tests {
    use super::{Constant, ListSpine};
    use std::rc::Rc;

    fn int(i: i64) -> Rc<Constant> {
        Rc::new(Constant::Integer(i.into()))
    }

    fn ints(list: &ListSpine) -> Vec<i64> {
        list.iter()
            .map(|c| match c.as_ref() {
                Constant::Integer(i) => i64::try_from(i).unwrap(),
                _ => unreachable!(),
            })
            .collect()
    }

    #[test]
    fn cons_shares_and_branches() {
        let base: ListSpine = vec![int(1), int(2)].into();
        let a = base.cons(int(0));
        // Claims a free slot in front of `a`'s spine.
        let b = a.cons(int(-1));
        // `a` is no longer the frontmost view, so this must copy.
        let c = a.cons(int(-2));
        let d = b.skip(2).unwrap().cons(int(9));

        assert_eq!(ints(&base), vec![1, 2]);
        assert_eq!(ints(&a), vec![0, 1, 2]);
        assert_eq!(ints(&b), vec![-1, 0, 1, 2]);
        assert_eq!(ints(&c), vec![-2, 0, 1, 2]);
        assert_eq!(ints(&d), vec![9, 1, 2]);
        assert!(b.skip(5).is_none());
        assert!(b.skip(4).unwrap().is_empty());

        let mut long = ListSpine::default();
        for i in 0..100 {
            long = long.cons(int(i));
        }
        let tail = long.skip(50).unwrap();
        drop(long);
        assert_eq!(ints(&tail), (0..50).rev().collect::<Vec<_>>());
        assert_eq!(tail, tail.to_vec().into());
    }

    #[test]
    fn cons_while_views_are_borrowed() {
        let base: ListSpine = (0..3).map(int).collect();
        let front = base.cons(int(-1));
        let sibling = front.clone();

        // Borrow both views' elements, then fill the free slots in front of
        // them, through each view in turn.
        let borrowed: &[Rc<Constant>] = &front;
        let from_sibling = sibling.cons(int(-2));
        let from_front = front.cons(int(-3));
        let from_tail = from_sibling.skip(1).unwrap().cons(int(-4));

        assert_eq!(borrowed.len(), 4);
        assert_eq!(ints(&sibling), vec![-1, 0, 1, 2]);
        assert_eq!(ints(&from_sibling), vec![-2, -1, 0, 1, 2]);
        assert_eq!(ints(&from_front), vec![-3, -1, 0, 1, 2]);
        assert_eq!(ints(&from_tail), vec![-4, -1, 0, 1, 2]);

        // Dropping every view but one frees the spine's elements exactly once.
        let empty = ListSpine::default().cons(int(1)).skip(1).unwrap();
        drop((base, front, sibling, from_front, from_tail));
        assert_eq!(ints(&from_sibling), vec![-2, -1, 0, 1, 2]);
        assert_eq!(ints(&empty.cons(int(5))), vec![5]);
        assert_eq!(from_sibling.into_vec().len(), 5);
    }
}
