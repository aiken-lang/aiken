//! Plutus Data whose children are shared between copies.

use crate::{
    ast::{Constant, ListSpine, Type},
    machine::{
        runtime::{ANY_TAG, convert_constr_to_tag},
        value::to_pallas_bigint,
    },
};
use pallas_codec::minicbor::{self, data::Tag, encode::Write};
use pallas_primitives::{BigInt, BoundedBytes, KeyValuePairs, MaybeIndefArray, PlutusData};
use std::{cmp::Ordering, fmt, rc::Rc};

/// A Plutus Data value.
///
/// It keeps every encoding detail pallas' `PlutusData` keeps (definite or
/// indefinite arrays and maps, the form of a constructor's tag, how an
/// integer is encoded), so converting between the two is lossless and both
/// encode to the same bytes. Unlike `PlutusData`, its children are shared:
/// cloning a `Data`, or taking any of its children, is O(1).
///
/// Equality and ordering are pallas', which ignore encoding details.
///
/// ```
/// use uplc::{PlutusData, ast::Data};
///
/// let data = Data::constr(0, vec![Data::integer(42.into()), Data::bytestring(vec![0xff])]);
/// let pallas = PlutusData::from(&data);
///
/// assert_eq!(Data::from(pallas), data);
/// ```
#[derive(Clone, Debug)]
pub enum Data {
    Constr(Constr),
    Map(Map),
    Array(Array),
    BigInt(BigInt),
    BoundedBytes(BoundedBytes),
}

/// A constructor application, with its tag as encoded (see
/// [`Constr::constr_index`]).
#[derive(Clone, Debug)]
pub struct Constr {
    pub tag: u64,
    pub any_constructor: Option<u64>,
    pub fields: Array,
}

/// The items of a Data array, or the fields of a constructor.
///
/// The items are kept as `Constant::Data` in a list spine, so the builtins
/// that convert between Data and builtin lists (`unListData`,
/// `unConstrData`, `listData`, `constrData`) share it instead of copying.
#[derive(Clone)]
pub struct Array {
    items: ListSpine,
    indefinite: bool,
}

/// The entries of a Data map.
///
/// Entries are kept as `Constant::ProtoPair`s of `Constant::Data` in a list
/// spine, so `unMapData` and `mapData` share it instead of copying.
#[derive(Clone)]
pub struct Map {
    entries: ListSpine,
    indefinite: bool,
}

impl Data {
    pub fn integer(i: num_bigint::BigInt) -> Data {
        Data::BigInt(to_pallas_bigint(&i))
    }

    pub fn bytestring(bytes: Vec<u8>) -> Data {
        Data::BoundedBytes(bytes.into())
    }

    pub fn map(kvs: Vec<(Data, Data)>) -> Data {
        Data::Map(Map::def(kvs))
    }

    /// A list, encoded as an indefinite array unless it is empty, as the
    /// `listData` builtin encodes it.
    pub fn list(xs: Vec<Data>) -> Data {
        Data::Array(Array::canonical(xs))
    }

    /// A constructor application, with the tag form and field encoding the
    /// `constrData` builtin uses.
    pub fn constr(ix: u64, fields: Vec<Data>) -> Data {
        Data::Constr(Constr::new(ix, Array::canonical(fields)))
    }

    /// The hex-encoded CBOR of this Data, as `PlutusData` would encode it.
    pub fn to_hex(&self) -> String {
        hex::encode(self.to_cbor())
    }

    /// The CBOR of this Data, as `PlutusData` would encode it.
    pub fn to_cbor(&self) -> Vec<u8> {
        minicbor::to_vec(self).expect("writing to a Vec cannot fail")
    }

    /// The CBOR the `serialiseData` builtin produces, which differs from
    /// [`Data::to_cbor`] in a few ways:
    ///
    /// 1. Arrays are always encoded using indefinite arrays, except when empty. When empty, they're
    ///    always encoded using definite length.
    /// 2. Maps are always encoded with definite length, even when empty.
    /// 3. Constr fields follow the same rules as arrays.
    pub fn serialise(&self) -> Vec<u8> {
        let mut bytes = Vec::new();

        encode(self, &mut minicbor::Encoder::new(&mut bytes), true)
            .expect("writing to a Vec cannot fail");

        bytes
    }

    /// An equal Data sharing no allocation with `self` (see
    /// `Constant::deep_clone`).
    pub fn deep_clone(&self) -> Data {
        match self {
            Data::Constr(constr) => Data::Constr(Constr {
                tag: constr.tag,
                any_constructor: constr.any_constructor,
                fields: constr.fields.deep_clone(),
            }),
            Data::Map(map) => Data::Map(map.deep_clone()),
            Data::Array(array) => Data::Array(array.deep_clone()),
            Data::BigInt(i) => Data::BigInt(i.clone()),
            Data::BoundedBytes(bytes) => Data::BoundedBytes(bytes.clone()),
        }
    }
}

impl Constr {
    /// A constructor application with index `ix`, using the tag form
    /// `constrData` uses: 121–127, then 1280–1400, then 102 with an explicit
    /// index.
    pub(crate) fn new(ix: u64, fields: Array) -> Constr {
        // NOTE: see https://github.com/input-output-hk/plutus/blob/9538fc9829426b2ecb0628d352e2d7af96ec8204/plutus-core/plutus-core/src/PlutusCore/Data.hs#L139-L155
        let tag = convert_constr_to_tag(ix);

        Constr {
            tag: tag.unwrap_or(ANY_TAG),
            any_constructor: tag.is_none().then_some(ix),
            fields,
        }
    }

    /// The constructor index the tag encodes. Panics on a tag that is not a
    /// constructor tag, as pallas' `Constr::constr_index` does.
    pub fn constr_index(&self) -> u64 {
        match self.tag {
            121..=127 => self.tag - 121,
            1280..=1400 => self.tag - 1280 + 7,
            102 => self
                .any_constructor
                .unwrap_or_else(|| panic!("malformed Constr: missing 'any_constructor'")),
            tag => panic!("malformed Constr: invalid tag {tag:?}"),
        }
    }
}

impl Array {
    /// An array encoded with a definite length.
    pub fn def(items: impl IntoIterator<Item = Data>) -> Array {
        Array::from_items(items, false)
    }

    /// An array encoded with an indefinite length.
    pub fn indef(items: impl IntoIterator<Item = Data>) -> Array {
        Array::from_items(items, true)
    }

    fn canonical(items: Vec<Data>) -> Array {
        let indefinite = !items.is_empty();

        Array::from_items(items, indefinite)
    }

    fn from_items(items: impl IntoIterator<Item = Data>, indefinite: bool) -> Array {
        Array {
            items: items
                .into_iter()
                .map(|item| Rc::new(Constant::Data(item)))
                .collect(),
            indefinite,
        }
    }

    /// An array of the items of a builtin list of Data, encoded as
    /// `listData` encodes it. Every item must be a `Constant::Data`.
    pub(crate) fn from_data_list(items: ListSpine) -> Array {
        Array {
            indefinite: !items.is_empty(),
            items,
        }
    }

    /// The items, as a builtin list of Data.
    pub(crate) fn as_data_list(&self) -> &ListSpine {
        &self.items
    }

    pub fn is_indefinite(&self) -> bool {
        self.indefinite
    }

    pub fn len(&self) -> usize {
        self.items.len()
    }

    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    pub fn get(&self, index: usize) -> Option<&Data> {
        self.items.get(index).map(|item| as_data(item))
    }

    pub fn iter(&self) -> impl ExactSizeIterator<Item = &Data> + DoubleEndedIterator {
        self.items.iter().map(|item| as_data(item))
    }

    fn deep_clone(&self) -> Array {
        Array::from_items(self.iter().map(Data::deep_clone), self.indefinite)
    }
}

impl Map {
    /// A map encoded with a definite length.
    pub fn def(entries: impl IntoIterator<Item = (Data, Data)>) -> Map {
        Map::from_entries(entries, false)
    }

    /// A map encoded with an indefinite length.
    pub fn indef(entries: impl IntoIterator<Item = (Data, Data)>) -> Map {
        Map::from_entries(entries, true)
    }

    fn from_entries(entries: impl IntoIterator<Item = (Data, Data)>, indefinite: bool) -> Map {
        Map {
            entries: entries
                .into_iter()
                .map(|(key, value)| {
                    Rc::new(Constant::ProtoPair(
                        Type::Data,
                        Type::Data,
                        Rc::new(Constant::Data(key)),
                        Rc::new(Constant::Data(value)),
                    ))
                })
                .collect(),
            indefinite,
        }
    }

    /// A map of the entries of a builtin list of pairs of Data, encoded as
    /// `mapData` encodes it. Every entry must be a `Constant::ProtoPair` of
    /// `Constant::Data`.
    pub(crate) fn from_pair_list(entries: ListSpine) -> Map {
        Map {
            entries,
            indefinite: false,
        }
    }

    /// The entries, as a builtin list of pairs of Data.
    pub(crate) fn as_pair_list(&self) -> &ListSpine {
        &self.entries
    }

    pub fn is_indefinite(&self) -> bool {
        self.indefinite
    }

    pub fn len(&self) -> usize {
        self.entries.len()
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    pub fn iter(&self) -> impl ExactSizeIterator<Item = (&Data, &Data)> + DoubleEndedIterator {
        self.entries.iter().map(|entry| as_entry(entry))
    }

    fn deep_clone(&self) -> Map {
        Map::from_entries(
            self.iter()
                .map(|(key, value)| (key.deep_clone(), value.deep_clone())),
            self.indefinite,
        )
    }
}

fn as_data(item: &Constant) -> &Data {
    match item {
        Constant::Data(data) => data,
        _ => unreachable!("Data arrays only hold Data"),
    }
}

fn as_entry(entry: &Constant) -> (&Data, &Data) {
    match entry {
        Constant::ProtoPair(_, _, key, value) => (as_data(key), as_data(value)),
        _ => unreachable!("Data maps only hold pairs of Data"),
    }
}

// ---------------- Comparison

impl PartialEq for Data {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl Eq for Data {}

impl PartialOrd for Data {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

/// The same order as `PlutusData`'s, so equality is unchanged from it.
/// Shared children compare equal without being walked.
impl Ord for Data {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Data::Constr(left), Data::Constr(right)) => left
                .constr_index()
                .cmp(&right.constr_index())
                .then_with(|| left.fields.cmp(&right.fields)),
            (Data::Constr(..), _) => Ordering::Less,
            (_, Data::Constr(..)) => Ordering::Greater,
            (Data::Map(left), Data::Map(right)) => left.cmp(right),
            (Data::Map(..), _) => Ordering::Less,
            (_, Data::Map(..)) => Ordering::Greater,
            (Data::Array(left), Data::Array(right)) => left.cmp(right),
            (Data::Array(..), _) => Ordering::Less,
            (_, Data::Array(..)) => Ordering::Greater,
            (Data::BigInt(BigInt::Int(left)), Data::BigInt(BigInt::Int(right))) => {
                i128::from(*left).cmp(&i128::from(*right))
            }
            (Data::BigInt(left), Data::BigInt(right)) => left.cmp(right),
            (Data::BigInt(..), _) => Ordering::Less,
            (_, Data::BigInt(..)) => Ordering::Greater,
            (Data::BoundedBytes(left), Data::BoundedBytes(right)) => left.cmp(right),
        }
    }
}

/// Compares two spines lexicographically, as slices compare.
fn cmp_spines(
    left: &ListSpine,
    right: &ListSpine,
    cmp: impl Fn(&Constant, &Constant) -> Ordering,
) -> Ordering {
    if left.ptr_eq(right) {
        return Ordering::Equal;
    }

    for (left, right) in left.iter().zip(right.iter()) {
        if Rc::ptr_eq(left, right) {
            continue;
        }

        match cmp(left, right) {
            Ordering::Equal => {}
            ordering => return ordering,
        }
    }

    left.len().cmp(&right.len())
}

/// Ignores the encoding, as `PlutusData`'s order does.
impl Ord for Array {
    fn cmp(&self, other: &Self) -> Ordering {
        cmp_spines(&self.items, &other.items, |left, right| {
            as_data(left).cmp(as_data(right))
        })
    }
}

impl PartialOrd for Array {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for Array {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl Eq for Array {}

/// Ignores the encoding, as `PlutusData`'s order does.
impl Ord for Map {
    fn cmp(&self, other: &Self) -> Ordering {
        cmp_spines(&self.entries, &other.entries, |left, right| {
            as_entry(left).cmp(&as_entry(right))
        })
    }
}

impl PartialOrd for Map {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for Map {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl Eq for Map {}

// ---------------- Debug, in the same shape as PlutusData's

impl fmt::Debug for Array {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple(if self.indefinite { "Indef" } else { "Def" })
            .field(&self.iter().collect::<Vec<_>>())
            .finish()
    }
}

impl fmt::Debug for Map {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple(if self.indefinite { "Indef" } else { "Def" })
            .field(&self.iter().collect::<Vec<_>>())
            .finish()
    }
}

// ---------------- Conversions from and to PlutusData

impl From<PlutusData> for Data {
    fn from(data: PlutusData) -> Self {
        match data {
            PlutusData::Constr(constr) => Data::Constr(Constr {
                tag: constr.tag,
                any_constructor: constr.any_constructor,
                fields: match constr.fields {
                    MaybeIndefArray::Def(items) => Array::def(items.into_iter().map(Data::from)),
                    MaybeIndefArray::Indef(items) => {
                        Array::indef(items.into_iter().map(Data::from))
                    }
                },
            }),
            PlutusData::Map(entries) => {
                let entries_into = |entries: Vec<(PlutusData, PlutusData)>| {
                    entries
                        .into_iter()
                        .map(|(key, value)| (Data::from(key), Data::from(value)))
                };

                Data::Map(match entries {
                    KeyValuePairs::Def(entries) => Map::def(entries_into(entries)),
                    KeyValuePairs::Indef(entries) => Map::indef(entries_into(entries)),
                })
            }
            PlutusData::Array(items) => Data::Array(match items {
                MaybeIndefArray::Def(items) => Array::def(items.into_iter().map(Data::from)),
                MaybeIndefArray::Indef(items) => Array::indef(items.into_iter().map(Data::from)),
            }),
            PlutusData::BigInt(i) => Data::BigInt(i),
            PlutusData::BoundedBytes(bytes) => Data::BoundedBytes(bytes),
        }
    }
}

impl From<&PlutusData> for Data {
    fn from(data: &PlutusData) -> Self {
        data.clone().into()
    }
}

impl From<&Data> for PlutusData {
    fn from(data: &Data) -> Self {
        match data {
            Data::Constr(constr) => PlutusData::Constr(pallas_primitives::Constr {
                tag: constr.tag,
                any_constructor: constr.any_constructor,
                fields: (&constr.fields).into(),
            }),
            Data::Map(map) => {
                let entries = map
                    .iter()
                    .map(|(key, value)| (key.into(), value.into()))
                    .collect();

                PlutusData::Map(if map.indefinite {
                    KeyValuePairs::Indef(entries)
                } else {
                    KeyValuePairs::Def(entries)
                })
            }
            Data::Array(array) => PlutusData::Array(array.into()),
            Data::BigInt(i) => PlutusData::BigInt(i.clone()),
            Data::BoundedBytes(bytes) => PlutusData::BoundedBytes(bytes.clone()),
        }
    }
}

impl From<Data> for PlutusData {
    fn from(data: Data) -> Self {
        (&data).into()
    }
}

impl From<&Array> for MaybeIndefArray<PlutusData> {
    fn from(array: &Array) -> Self {
        let items = array.iter().map(PlutusData::from).collect();

        if array.indefinite {
            MaybeIndefArray::Indef(items)
        } else {
            MaybeIndefArray::Def(items)
        }
    }
}

// ---------------- CBOR

/// Decodes as `PlutusData` decodes, with the same errors.
impl<'b, C> minicbor::Decode<'b, C> for Data {
    fn decode(d: &mut minicbor::Decoder<'b>, ctx: &mut C) -> Result<Self, minicbor::decode::Error> {
        PlutusData::decode(d, ctx).map(Data::from)
    }
}

/// Encodes as `PlutusData` encodes.
impl<C> minicbor::Encode<C> for Data {
    fn encode<W: Write>(
        &self,
        e: &mut minicbor::Encoder<W>,
        _ctx: &mut C,
    ) -> Result<(), minicbor::encode::Error<W::Error>> {
        encode(self, e, false)
    }
}

/// Encodes `data` as `PlutusData` encodes it or, if `canonical`, as
/// `serialiseData` does (see [`Data::serialise`]).
fn encode<W: Write>(
    data: &Data,
    e: &mut minicbor::Encoder<W>,
    canonical: bool,
) -> Result<(), minicbor::encode::Error<W::Error>> {
    match data {
        Data::Constr(constr) => {
            e.tag(Tag::new(constr.tag))?;

            if constr.tag == ANY_TAG {
                e.array(2)?;
                e.u64(constr.any_constructor.unwrap_or_default())?;
            }

            encode_array(&constr.fields, e, canonical)?;
        }
        Data::Map(map) => {
            if map.indefinite && !canonical {
                e.begin_map()?;
            } else {
                e.map(map.len() as u64)?;
            }

            for (key, value) in map.iter() {
                encode(key, e, canonical)?;
                encode(value, e, canonical)?;
            }

            if map.indefinite && !canonical {
                e.end()?;
            }
        }
        Data::Array(array) => encode_array(array, e, canonical)?,
        Data::BigInt(i) => {
            e.encode(i)?;
        }
        Data::BoundedBytes(bytes) => {
            e.encode(bytes)?;
        }
    }

    Ok(())
}

fn encode_array<W: Write>(
    array: &Array,
    e: &mut minicbor::Encoder<W>,
    canonical: bool,
) -> Result<(), minicbor::encode::Error<W::Error>> {
    // Mimics default haskell list encoding from cborg: canonically, an
    // indefinite array unless it is empty.
    let indefinite = if canonical {
        !array.is_empty()
    } else {
        array.indefinite
    };

    if indefinite {
        e.begin_array()?;
    } else {
        e.array(array.len() as u64)?;
    }

    for item in array.iter() {
        encode(item, e, canonical)?;
    }

    if indefinite {
        e.end()?;
    }

    Ok(())
}
