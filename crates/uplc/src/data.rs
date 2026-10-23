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
/// Shared children compare equal without being walked, and the comparison
/// never recurses, so the depth of the Data is not limited by the stack.
impl Ord for Data {
    fn cmp(&self, other: &Self) -> Ordering {
        cmp_all(vec![Comparison::Data(self, other)])
    }
}

/// What is left to compare, from the top of the stack down. The first that
/// is not equal decides.
enum Comparison<'a> {
    Data(&'a Data, &'a Data),
    Items(&'a ListSpine, &'a ListSpine),
    Entries(&'a ListSpine, &'a ListSpine),
    Lengths(usize, usize),
}

fn cmp_all(mut stack: Vec<Comparison<'_>>) -> Ordering {
    while let Some(comparison) = stack.pop() {
        let ordering = match comparison {
            Comparison::Data(left, right) => match (left, right) {
                (Data::Constr(left), Data::Constr(right)) => {
                    stack.push(Comparison::Items(&left.fields.items, &right.fields.items));
                    left.constr_index().cmp(&right.constr_index())
                }
                (Data::Map(left), Data::Map(right)) => {
                    stack.push(Comparison::Entries(&left.entries, &right.entries));
                    Ordering::Equal
                }
                (Data::Array(left), Data::Array(right)) => {
                    stack.push(Comparison::Items(&left.items, &right.items));
                    Ordering::Equal
                }
                (Data::BigInt(BigInt::Int(left)), Data::BigInt(BigInt::Int(right))) => {
                    i128::from(*left).cmp(&i128::from(*right))
                }
                (Data::BigInt(left), Data::BigInt(right)) => left.cmp(right),
                (Data::BoundedBytes(left), Data::BoundedBytes(right)) => left.cmp(right),
                (left, right) => constructor_rank(left).cmp(&constructor_rank(right)),
            },
            // Lexicographically, as slices compare: item by item, then by
            // length.
            Comparison::Items(left, right) => {
                if !left.ptr_eq(right) {
                    stack.push(Comparison::Lengths(left.len(), right.len()));
                    stack.extend(
                        left.iter()
                            .zip(right.iter())
                            .rev()
                            .filter(|(left, right)| !Rc::ptr_eq(left, right))
                            .map(|(left, right)| Comparison::Data(as_data(left), as_data(right))),
                    );
                }
                Ordering::Equal
            }
            // As `Items`, each entry comparing by key, then by value.
            Comparison::Entries(left, right) => {
                if !left.ptr_eq(right) {
                    stack.push(Comparison::Lengths(left.len(), right.len()));
                    for (left, right) in left.iter().zip(right.iter()).rev() {
                        if Rc::ptr_eq(left, right) {
                            continue;
                        }

                        let ((left_key, left_value), (right_key, right_value)) =
                            (as_entry(left), as_entry(right));
                        stack.push(Comparison::Data(left_value, right_value));
                        stack.push(Comparison::Data(left_key, right_key));
                    }
                }
                Ordering::Equal
            }
            Comparison::Lengths(left, right) => left.cmp(&right),
        };

        if ordering != Ordering::Equal {
            return ordering;
        }
    }

    Ordering::Equal
}

/// The position of a Data's constructor in Plutus' `Data` type, which orders
/// Data of different constructors.
fn constructor_rank(data: &Data) -> u8 {
    match data {
        Data::Constr(..) => 0,
        Data::Map(..) => 1,
        Data::Array(..) => 2,
        Data::BigInt(..) => 3,
        Data::BoundedBytes(..) => 4,
    }
}

/// Ignores the encoding, as `PlutusData`'s order does.
impl Ord for Array {
    fn cmp(&self, other: &Self) -> Ordering {
        cmp_all(vec![Comparison::Items(&self.items, &other.items)])
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
        cmp_all(vec![Comparison::Entries(&self.entries, &other.entries)])
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

/// Decodes as Plutus' `decodeData` does, keeping the encoding details
/// `PlutusData` keeps. It rejects what pallas accepts but Plutus does not:
/// byte strings and big integer payloads with a chunk of more than 64 bytes,
/// a big integer tag not in the head of its own byte (`0xc2` or `0xc3`), and
/// a tag 102 constructor that is not a pair of an index and fields. It never
/// recurses, so the depth of the Data is not limited by the stack.
impl<'b, C> minicbor::Decode<'b, C> for Data {
    fn decode(
        d: &mut minicbor::Decoder<'b>,
        _ctx: &mut C,
    ) -> Result<Self, minicbor::decode::Error> {
        decode(d)
    }
}

/// A Data array, map or constructor whose items are being decoded.
struct Partial {
    kind: PartialKind,
    indefinite: bool,
    /// Items still to decode, when the length is definite. A map has two
    /// items, a key and a value, per entry.
    remaining: u64,
    items: Vec<Data>,
}

enum PartialKind {
    Array,
    Map,
    Constr {
        tag: u64,
        any_constructor: Option<u64>,
        /// Whether a tag 102 constructor's index and fields are in an
        /// indefinite array, which must end once the fields do.
        indefinite_pair: bool,
    },
}

impl Partial {
    fn is_complete(&self, d: &mut minicbor::Decoder<'_>) -> Result<bool, minicbor::decode::Error> {
        if self.indefinite {
            take_break(d)
        } else {
            Ok(self.remaining == 0)
        }
    }

    fn finish(self, d: &mut minicbor::Decoder<'_>) -> Result<Data, minicbor::decode::Error> {
        let items = |items: Vec<Data>| {
            if self.indefinite {
                Array::indef(items)
            } else {
                Array::def(items)
            }
        };

        Ok(match self.kind {
            PartialKind::Array => Data::Array(items(self.items)),
            PartialKind::Map => {
                // An indefinite map can end between a key and its value.
                if !self.items.len().is_multiple_of(2) {
                    return Err(minicbor::decode::Error::message(
                        "expected a value after the last key of a map",
                    )
                    .at(d.position()));
                }

                let mut items = self.items.into_iter();
                let mut entries = Vec::with_capacity(items.len() / 2);
                while let (Some(key), Some(value)) = (items.next(), items.next()) {
                    entries.push((key, value));
                }

                Data::Map(if self.indefinite {
                    Map::indef(entries)
                } else {
                    Map::def(entries)
                })
            }
            PartialKind::Constr {
                tag,
                any_constructor,
                indefinite_pair,
            } => {
                if indefinite_pair && !take_break(d)? {
                    return Err(minicbor::decode::Error::message(
                        "expected exactly two elements in a tag 102 constructor",
                    )
                    .at(d.position()));
                }

                Data::Constr(Constr {
                    tag,
                    any_constructor,
                    fields: items(self.items),
                })
            }
        })
    }
}

/// Decodes Data as Plutus' `decodeData` does (see `PlutusCore.Data`), with
/// an explicit stack of the arrays, maps and constructors being decoded.
fn decode(d: &mut minicbor::Decoder<'_>) -> Result<Data, minicbor::decode::Error> {
    let mut stack: Vec<Partial> = Vec::new();

    loop {
        let mut data = match stack.last_mut() {
            Some(partial) if partial.is_complete(d)? => {
                let partial = stack.pop().expect("the stack has a last item");
                partial.finish(d)?
            }
            parent => {
                if let Some(parent) = parent {
                    parent.remaining = parent.remaining.saturating_sub(1);
                }

                match decode_item(d)? {
                    Item::Data(data) => data,
                    Item::Partial(partial) => {
                        stack.push(partial);
                        continue;
                    }
                }
            }
        };

        // Hand the decoded item to its parent, completing every parent it
        // completes.
        loop {
            match stack.last_mut() {
                None => return Ok(data),
                Some(parent) => {
                    parent.items.push(data);

                    if parent.indefinite || parent.remaining > 0 {
                        break;
                    }

                    let parent = stack.pop().expect("the stack has a last item");
                    data = parent.finish(d)?;
                }
            }
        }
    }
}

enum Item {
    Data(Data),
    Partial(Partial),
}

/// Decodes a leaf, or the head of an array, map or constructor, by its CBOR
/// token type as `decodeData` dispatches on cborg's `peekTokenType`.
fn decode_item(d: &mut minicbor::Decoder<'_>) -> Result<Item, minicbor::decode::Error> {
    let position = d.position();
    let head = *d
        .input()
        .get(position)
        .ok_or_else(minicbor::decode::Error::end_of_input)?;

    Ok(match head {
        // Unsigned and negative integers, up to 64 bits.
        0x00..=0x1b | 0x20..=0x3b => {
            Item::Data(Data::BigInt(BigInt::Int(pallas_primitives::Int(d.int()?))))
        }
        0x40..=0x5b | 0x5f => Item::Data(Data::BoundedBytes(decode_bounded_bytes(d)?.into())),
        0x80..=0x9b | 0x9f => {
            let length = d.array()?;
            Item::Partial(Partial::new(PartialKind::Array, length))
        }
        0xa0..=0xbb | 0xbf => {
            let length = d.map()?;
            Item::Partial(Partial::new(PartialKind::Map, length))
        }
        // cborg only takes a tag in the head of its own byte for a big
        // integer; any other encoding of tags 2 and 3 is a constructor tag,
        // which they are not.
        0xc2 | 0xc3 => {
            d.tag()?;

            let n = decode_bounded_bytes(d)?.into();

            Item::Data(Data::BigInt(if head == 0xc2 {
                BigInt::BigUInt(n)
            } else {
                BigInt::BigNInt(n)
            }))
        }
        0xc0..=0xdb => {
            let tag = d.tag()?.as_u64();

            let (any_constructor, indefinite_pair) = match tag {
                121..=127 | 1280..=1400 => (None, false),
                _ if tag == ANY_TAG => {
                    let pair = d.array()?;

                    if pair.is_some_and(|length| length != 2) {
                        return Err(minicbor::decode::Error::message(
                            "expected exactly two elements in a tag 102 constructor",
                        )
                        .at(position));
                    }

                    (Some(d.u64()?), pair.is_none())
                }
                _ => {
                    return Err(minicbor::decode::Error::message(format!(
                        "unrecognized tag {tag} for Data"
                    ))
                    .at(position));
                }
            };

            let length = d.array()?;

            Item::Partial(Partial::new(
                PartialKind::Constr {
                    tag,
                    any_constructor,
                    indefinite_pair,
                },
                length,
            ))
        }
        _ => {
            return Err(minicbor::decode::Error::message(format!(
                "unexpected CBOR {:?} for Data",
                d.datatype()?
            ))
            .at(position));
        }
    })
}

impl Partial {
    fn new(kind: PartialKind, length: Option<u64>) -> Partial {
        let per_item = if matches!(kind, PartialKind::Map) {
            2
        } else {
            1
        };

        Partial {
            kind,
            indefinite: length.is_none(),
            remaining: length.unwrap_or(0).saturating_mul(per_item),
            items: Vec::new(),
        }
    }
}

/// Decodes a byte string, definite or in definite chunks, each of at most 64
/// bytes, as Plutus' `decodeBoundedBytes` and `decodeBoundedBytesIndef` do.
fn decode_bounded_bytes(d: &mut minicbor::Decoder<'_>) -> Result<Vec<u8>, minicbor::decode::Error> {
    fn chunk<'b>(d: &mut minicbor::Decoder<'b>) -> Result<&'b [u8], minicbor::decode::Error> {
        let position = d.position();
        let chunk = d.bytes()?;

        if chunk.len() > 64 {
            return Err(
                minicbor::decode::Error::message("byte string exceeds 64 bytes").at(position),
            );
        }

        Ok(chunk)
    }

    match d.datatype()? {
        minicbor::data::Type::BytesIndef => {
            d.set_position(d.position() + 1);

            let mut bytes = Vec::new();
            while !take_break(d)? {
                bytes.extend_from_slice(chunk(d)?);
            }

            Ok(bytes)
        }
        _ => chunk(d).map(<[u8]>::to_vec),
    }
}

/// Consumes a break, if it is next.
fn take_break(d: &mut minicbor::Decoder<'_>) -> Result<bool, minicbor::decode::Error> {
    match d.input().get(d.position()) {
        Some(0xff) => {
            d.set_position(d.position() + 1);
            Ok(true)
        }
        Some(_) => Ok(false),
        None => Err(minicbor::decode::Error::end_of_input()),
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
/// `serialiseData` does (see [`Data::serialise`]). It never recurses, so the
/// depth of the Data is not limited by the stack.
fn encode<W: Write>(
    data: &Data,
    e: &mut minicbor::Encoder<W>,
    canonical: bool,
) -> Result<(), minicbor::encode::Error<W::Error>> {
    // What is left to encode, from the top of the stack down.
    enum Encoding<'a> {
        Data(&'a Data),
        Break,
    }

    let mut stack = vec![Encoding::Data(data)];

    while let Some(encoding) = stack.pop() {
        let data = match encoding {
            Encoding::Data(data) => data,
            Encoding::Break => {
                e.end()?;
                continue;
            }
        };

        let items = match data {
            Data::Constr(constr) => {
                e.tag(Tag::new(constr.tag))?;

                if constr.tag == ANY_TAG {
                    e.array(2)?;
                    e.u64(constr.any_constructor.unwrap_or_default())?;
                }

                &constr.fields
            }
            Data::Map(map) => {
                if map.indefinite && !canonical {
                    e.begin_map()?;
                    stack.push(Encoding::Break);
                } else {
                    e.map(map.len() as u64)?;
                }

                for (key, value) in map.iter().rev() {
                    stack.push(Encoding::Data(value));
                    stack.push(Encoding::Data(key));
                }

                continue;
            }
            Data::Array(array) => array,
            Data::BigInt(i) => {
                e.encode(i)?;
                continue;
            }
            Data::BoundedBytes(bytes) => {
                e.encode(bytes)?;
                continue;
            }
        };

        // Mimics default haskell list encoding from cborg: canonically, an
        // indefinite array unless it is empty.
        let indefinite = if canonical {
            !items.is_empty()
        } else {
            items.indefinite
        };

        if indefinite {
            e.begin_array()?;
            stack.push(Encoding::Break);
        } else {
            e.array(items.len() as u64)?;
        }

        stack.extend(items.iter().rev().map(Encoding::Data));
    }

    Ok(())
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use crate::{
        ast::{NamedDeBruijn, Program, Term},
        builtins::DefaultFunction,
        machine::cost_model::ExBudget,
    };
    use pallas_crypto::hash::Hasher;
    use pallas_primitives::{Constr as PallasConstr, Fragment};
    use proptest::{collection::vec, prelude::*};
    use std::collections::HashSet;

    /// Any `PlutusData`, with every encoding variant pallas keeps.
    pub(crate) fn arb_plutus_data() -> impl Strategy<Value = PlutusData> {
        let int = prop_oneof![
            any::<i64>().prop_map(minicbor::data::Int::from),
            any::<u64>().prop_map(minicbor::data::Int::from),
            any::<u64>().prop_map(|n| minicbor::data::Int::try_from(-1 - i128::from(n)).unwrap()),
        ];

        let leaf = prop_oneof![
            int.prop_map(|i| PlutusData::BigInt(BigInt::Int(pallas_primitives::Int(i)))),
            // Big integers, including non-canonical ones (leading zeros, or
            // small enough for `Int`).
            vec(any::<u8>(), 0..20).prop_map(|b| PlutusData::BigInt(BigInt::BigUInt(b.into()))),
            vec(any::<u8>(), 0..20).prop_map(|b| PlutusData::BigInt(BigInt::BigNInt(b.into()))),
            // Byte strings on either side of the 64-byte chunk size.
            vec(any::<u8>(), 0..140).prop_map(|b| PlutusData::BoundedBytes(b.into())),
        ];

        let tag = prop_oneof![
            (121u64..=127).prop_map(|tag| (tag, None)),
            (1280u64..=1400).prop_map(|tag| (tag, None)),
            any::<u64>().prop_map(|ix| (102, Some(ix))),
        ];

        leaf.prop_recursive(4, 64, 5, move |inner| {
            let array = |indefinite: bool, items| {
                if indefinite {
                    MaybeIndefArray::Indef(items)
                } else {
                    MaybeIndefArray::Def(items)
                }
            };

            prop_oneof![
                (tag.clone(), any::<bool>(), vec(inner.clone(), 0..5)).prop_map(
                    move |((tag, any_constructor), indefinite, fields)| {
                        PlutusData::Constr(PallasConstr {
                            tag,
                            any_constructor,
                            fields: array(indefinite, fields),
                        })
                    }
                ),
                (any::<bool>(), vec((inner.clone(), inner.clone()), 0..4)).prop_map(
                    |(indefinite, entries)| PlutusData::Map(if indefinite {
                        KeyValuePairs::Indef(entries)
                    } else {
                        KeyValuePairs::Def(entries)
                    })
                ),
                (any::<bool>(), vec(inner, 0..5)).prop_map(move |(indefinite, items)| {
                    PlutusData::Array(array(indefinite, items))
                }),
            ]
        })
    }

    /// The `serialiseData` encoding as it was computed from `PlutusData`
    /// before `Data` existed.
    fn reencode(data: &PlutusData) -> Vec<u8> {
        fn go(data: &PlutusData, e: &mut minicbor::Encoder<&mut Vec<u8>>) {
            match data {
                PlutusData::Constr(constr) => {
                    e.tag(Tag::new(constr.tag)).unwrap();
                    if constr.tag == 102 {
                        e.array(2).unwrap();
                        e.encode(constr.any_constructor.unwrap_or_default())
                            .unwrap();
                    }
                    array(&constr.fields, e);
                }
                PlutusData::Map(entries) => {
                    e.map(entries.len() as u64).unwrap();
                    for (k, v) in entries.iter() {
                        go(k, e);
                        go(v, e);
                    }
                }
                PlutusData::Array(items) => array(items, e),
                PlutusData::BoundedBytes(bytes) => {
                    e.encode(bytes).unwrap();
                }
                PlutusData::BigInt(i) => {
                    e.encode(i).unwrap();
                }
            }
        }

        fn array(items: &MaybeIndefArray<PlutusData>, e: &mut minicbor::Encoder<&mut Vec<u8>>) {
            if items.is_empty() {
                e.array(0).unwrap();
            } else {
                e.begin_array().unwrap();
                for item in items.iter() {
                    go(item, e);
                }
                e.end().unwrap();
            }
        }

        let mut bytes = Vec::new();
        go(data, &mut minicbor::Encoder::new(&mut bytes));
        bytes
    }

    /// The addresses of every node below `data`.
    fn nodes(data: &Data, found: &mut HashSet<*const Constant>) {
        let mut visit = |item: &Rc<Constant>| {
            found.insert(Rc::as_ptr(item));
            match item.as_ref() {
                Constant::Data(data) => nodes(data, found),
                Constant::ProtoPair(_, _, key, value) => {
                    for child in [key, value] {
                        found.insert(Rc::as_ptr(child));
                        nodes(as_data(child), found);
                    }
                }
                _ => unreachable!(),
            }
        };

        match data {
            Data::Constr(constr) => constr.fields.items.iter().for_each(&mut visit),
            Data::Array(array) => array.items.iter().for_each(&mut visit),
            Data::Map(map) => map.entries.iter().for_each(&mut visit),
            Data::BigInt(_) | Data::BoundedBytes(_) => {}
        }
    }

    fn equals_data(left: Data, right: Data) -> (Term<NamedDeBruijn>, ExBudget) {
        let program = Program::<NamedDeBruijn> {
            version: (1, 1, 0),
            term: Term::Builtin(DefaultFunction::EqualsData)
                .apply(Term::data(left))
                .apply(Term::data(right)),
        };

        let result = program.eval(ExBudget::default());
        let cost = result.cost();

        (result.result().unwrap(), cost)
    }

    proptest! {
        #[test]
        fn encodes_like_pallas(pallas in arb_plutus_data()) {
            let bytes = pallas.encode_fragment().unwrap();
            let data = Data::from(&pallas);

            prop_assert_eq!(&data.to_cbor(), &bytes);
            prop_assert_eq!(&data.to_hex(), &hex::encode(&bytes));
            // cryptoxide's hashing is not miri-clean.
            if !cfg!(miri) {
                prop_assert_eq!(
                    Hasher::<256>::hash(&data.to_cbor()),
                    Hasher::<256>::hash_cbor(&pallas)
                );
            }

            // CBOR in, Data, CBOR out.
            let decoded = Data::decode_fragment(&bytes).unwrap();
            prop_assert_eq!(&decoded.to_cbor(), &bytes);
            prop_assert_eq!(&PlutusData::from(&decoded).encode_fragment().unwrap(), &bytes);
            prop_assert_eq!(&PlutusData::from(decoded).encode_fragment().unwrap(), &bytes);
        }

        #[test]
        fn decodes_like_pallas_where_plutus_agrees(
            pallas in arb_plutus_data(),
            cut in any::<prop::sample::Index>(),
            at in any::<prop::sample::Index>(),
            byte in any::<u8>(),
        ) {
            let bytes = pallas.encode_fragment().unwrap();
            let mut changed = bytes.clone();
            changed[at.index(bytes.len())] = byte;

            // What pallas encodes is Data to Plutus too.
            prop_assert!(Data::decode_fragment(&bytes).is_ok());

            for bytes in [&bytes[..], &bytes[..cut.index(bytes.len())], &changed[..]] {
                match (Data::decode_fragment(bytes), PlutusData::decode_fragment(bytes)) {
                    (Ok(data), Ok(expected)) => {
                        prop_assert_eq!(data.to_cbor(), expected.encode_fragment().unwrap());
                        prop_assert_eq!(data, Data::from(expected));
                    }
                    (Ok(data), Err(error)) => {
                        prop_assert!(false, "{}: {error}, yet decoded {data:?}", hex::encode(bytes));
                    }
                    // Plutus only rejects an encoding pallas accepts, never
                    // the Data it encodes: pallas' own encoding of that Data
                    // decodes, as the same Data.
                    (Err(_), Ok(expected)) => {
                        let reencoded = expected.encode_fragment().unwrap();
                        prop_assert_eq!(
                            Data::decode_fragment(&reencoded).map_err(|e| e.to_string()),
                            Ok(Data::from(expected))
                        );
                    }
                    (Err(_), Err(_)) => {}
                }
            }
        }

        #[test]
        fn serialises_like_pallas(pallas in arb_plutus_data()) {
            let serialised = Data::from(&pallas).serialise();

            prop_assert_eq!(&serialised, &reencode(&pallas));
            prop_assert_eq!(&serialised, &crate::plutus_data_to_bytes(&pallas));
        }

        #[test]
        fn debugs_like_pallas(pallas in arb_plutus_data()) {
            prop_assert_eq!(format!("{:?}", Data::from(&pallas)), format!("{pallas:?}"));
            prop_assert_eq!(format!("{:#?}", Data::from(&pallas)), format!("{pallas:#?}"));
        }

        #[test]
        fn compares_like_pallas(left in arb_plutus_data(), right in arb_plutus_data()) {
            let (data_left, data_right) = (Data::from(&left), Data::from(&right));

            prop_assert_eq!(data_left.cmp(&data_right), left.cmp(&right));
            prop_assert_eq!(data_left == data_right, left == right);
            prop_assert_eq!(data_left.cmp(&data_left.clone()), Ordering::Equal);
            prop_assert_eq!(data_left.cmp(&data_left.deep_clone()), Ordering::Equal);
        }

        #[test]
        fn deep_clone_shares_nothing(pallas in arb_plutus_data()) {
            let data = Data::from(&pallas);
            let copy = data.deep_clone();

            let (mut original, mut copied) = (HashSet::new(), HashSet::new());
            nodes(&data, &mut original);
            nodes(&copy, &mut copied);

            prop_assert_eq!(original.len(), copied.len());
            prop_assert!(original.is_disjoint(&copied));
            prop_assert_eq!(copy.to_cbor(), data.to_cbor());
        }
    }

    #[test]
    fn round_trips_every_encoding() {
        let mut chunked = vec![0x5f, 0x58, 0x40];
        chunked.extend([0xab; 64]);
        chunked.extend([0x41, 0xcd, 0xff]);

        for hex in [
            // integers, including non-canonical big integers
            "00",
            "17",
            "1b7fffffffffffffff",
            "1bffffffffffffffff",
            "20",
            "3bffffffffffffffff",
            "c240",
            "c24101",
            "c243000001",
            "c249010000000000000000",
            "c340",
            "c34100",
            "c349010000000000000000",
            // byte strings, up to and over one chunk
            "40",
            "4100",
            &format!("5840{}", "ab".repeat(64)),
            &hex::encode(&chunked),
            // arrays
            "80",
            "9fff",
            "8101",
            "9f01ff",
            "829f01ff80",
            // maps
            "a0",
            "bfff",
            "a10102",
            "bf0102ff",
            "a1a0bfff",
            // constructors, in each tag form
            "d87980",
            "d8799fff",
            "d8798101",
            "d8799f01ff",
            "d87f80",
            "d9050080",
            "d905789f01ff",
            "d86682188080",
            "d866821bffffffffffffffff9f01ff",
        ] {
            let bytes = hex::decode(hex).unwrap();
            let data = Data::decode_fragment(&bytes).unwrap();

            assert_eq!(data.to_hex(), hex, "{data:?}");
            assert_eq!(
                data.serialise(),
                reencode(&PlutusData::decode_fragment(&bytes).unwrap()),
                "{hex}"
            );
        }
    }

    #[test]
    fn serialises_canonically() {
        for (hex, serialised) in [
            ("9fff", "80"),
            ("8101", "9f01ff"),
            ("bf0102ff", "a10102"),
            ("bfff", "a0"),
            ("d8798101", "d8799f01ff"),
            ("d8799fff", "d87980"),
            ("d86682188081a0", "d8668218809fa0ff"),
        ] {
            let data = Data::decode_fragment(&hex::decode(hex).unwrap()).unwrap();

            assert_eq!(hex::encode(data.serialise()), serialised, "{hex}");
        }
    }

    #[test]
    fn rejects_what_plutus_rejects() {
        let over_64_bytes = "ab".repeat(65);

        for hex in [
            "",
            "ff",
            "d87a",
            "f6",
            "9f01",
            "6161",
            "1c",
            // A big integer whose payload is not a byte string.
            "c2ff",
            "c2a0",
            // A byte string, or a chunk of one, over 64 bytes.
            &format!("5841{over_64_bytes}"),
            &format!("5f5841{over_64_bytes}ff"),
            &format!("c25841{over_64_bytes}"),
            &format!("c35f5841{over_64_bytes}ff"),
            // A chunk that is not a definite byte string.
            "5f5f40ffff",
            "5f01ff",
            // An indefinite map ending between a key and its value.
            "bf01ff",
            "bf010203ff",
            "bf9fffff",
            // Tags 2 and 3 in a longer head than cborg takes for a big integer.
            "d8024101",
            "d8034100",
            "d9000241ff",
            // Tags that are neither a big integer nor a constructor.
            "c0",
            "c4820001",
            "d8184100",
            "d87880",
            "d904ff80",
            "d9057980",
            // A tag 102 constructor that is not a pair of an index and fields.
            "d86680",
            "d8668100",
            "d86683008000",
            "d8669f0080",
            "d8669f008000ff",
            "d8669fff",
            "d866822080",
            "d866824100",
            // Constructor fields that are not an array.
            "d879a0",
            "d87900",
            "d866820000",
            // A break in place of a map value.
            "bf00ff",
            "a100ff",
            // Lengths far beyond the input.
            "bbffffffffffffffff",
            "9bffffffffffffffff",
            "d8799bffffffffffffffff",
            // More tags that are neither a constructor nor a big integer.
            "d9010000",
            "c000",
            "c19f00ff",
            // Truncated and malformed items inside containers.
            "d8668201",
            "9f9f00ff",
            "a1a10000",
            "d8799f5f4100ff",
            "d8799f5f41ff",
            "9f1cff",
            "81",
            "8200",
            "bf",
        ] {
            let bytes = hex::decode(hex).unwrap();

            assert!(Data::decode_fragment(&bytes).is_err(), "{hex}");
        }
    }

    #[test]
    fn accepts_what_plutus_accepts() {
        let chunk = "ab".repeat(64);

        for (hex, expected) in [
            (format!("5840{chunk}"), Data::bytestring(vec![0xab; 64])),
            // Chunks of at most 64 bytes, however many.
            (
                format!("5f5840{chunk}5840{chunk}40ff"),
                Data::bytestring(vec![0xab; 128]),
            ),
            (
                format!("c25f5840{chunk}4101ff"),
                Data::integer(num_bigint::BigInt::from_bytes_be(
                    num_bigint::Sign::Plus,
                    &[[0xab; 64].as_slice(), &[0x01]].concat(),
                )),
            ),
            // Heads longer than they need to be.
            ("1800".to_string(), Data::integer(0.into())),
            ("3a00000000".to_string(), Data::integer((-1).into())),
            ("d9007980".to_string(), Data::constr(0, vec![])),
            ("da0000050080".to_string(), Data::constr(7, vec![])),
            (
                "9b000000000000000101".to_string(),
                Data::list(vec![Data::integer(1.into())]),
            ),
            // Tag 102, with an index of any size, in either kind of array.
            ("d866820080".to_string(), Data::constr(0, vec![])),
            ("d8669f0080ff".to_string(), Data::constr(0, vec![])),
            (
                "d866821bffffffffffffffff9f01ff".to_string(),
                Data::constr(u64::MAX, vec![Data::integer(1.into())]),
            ),
        ] {
            let bytes = hex::decode(&hex).unwrap();

            assert_eq!(Data::decode_fragment(&bytes).unwrap(), expected, "{hex}");
        }
    }

    #[test]
    #[cfg_attr(
        target_family = "wasm",
        ignore = "spawns a thread with a bounded stack"
    )]
    fn decodes_compares_and_encodes_deep_data_in_bounded_stack() {
        #[cfg(not(miri))]
        const DEPTH: usize = 200_000;
        #[cfg(miri)]
        const DEPTH: usize = 2_000;

        // Room for the nested drops allowed before queuing (see
        // `drops_deep_data_in_bounded_stack`), but far too little to walk the
        // nesting recursively.
        std::thread::Builder::new()
            .stack_size(4 * 1024 * 1024)
            .spawn(|| {
                let deep = |leaf: u64| {
                    let mut data = Data::integer(leaf.into());
                    for i in 0..DEPTH {
                        data = match i % 4 {
                            0 => Data::constr(0, vec![Data::integer(i.into()), data]),
                            1 => Data::list(vec![data]),
                            2 => Data::map(vec![(Data::bytestring(vec![]), data)]),
                            _ => Data::constr(1_000, vec![data]),
                        };
                    }
                    data
                };

                // Built apart, so equal without sharing anything.
                let (left, same, right) = (deep(0), deep(0), deep(1));

                assert_eq!(left.cmp(&same), Ordering::Equal);
                assert_eq!(left, same);
                assert_eq!(left.cmp(&right), Ordering::Less);
                assert_ne!(left, right);

                for data in [left, right] {
                    let serialised = data.serialise();
                    let decoded = Data::decode_fragment(&serialised).unwrap();
                    assert_eq!(decoded.serialise(), serialised);
                    assert_eq!(decoded.to_cbor(), data.to_cbor());
                    assert_eq!(decoded, data);
                }
            })
            .unwrap()
            .join()
            .unwrap();
    }
    /// Data nested `depth` levels deep in each shape and encoding, with the
    /// CBOR `serialiseData` gives it. `leaf` is the innermost item.
    fn deep_data(depth: usize, leaf: &str) -> Vec<(&'static str, Vec<u8>, Vec<u8>)> {
        let nest = |open: &str, inner: &str, close: &str| {
            hex::decode(open.repeat(depth) + inner + &close.repeat(depth)).unwrap()
        };

        [
            ("constr", ("d87981", ""), ("d8799f", "ff")),
            ("indefinite constr", ("d8799f", "ff"), ("d8799f", "ff")),
            (
                "general constr",
                ("d86682188081", ""),
                ("d8668218809f", "ff"),
            ),
            ("list", ("81", ""), ("9f", "ff")),
            ("indefinite list", ("9f", "ff"), ("9f", "ff")),
            ("map value", ("a100", ""), ("a100", "")),
            ("indefinite map value", ("bf00", "ff"), ("a100", "")),
            ("map key", ("a1", "00"), ("a1", "00")),
            ("indefinite map key", ("bf", "00ff"), ("a1", "00")),
        ]
        .into_iter()
        .map(
            |(name, (open, close), (serialised_open, serialised_close))| {
                (
                    name,
                    nest(open, leaf, close),
                    nest(serialised_open, leaf, serialised_close),
                )
            },
        )
        .collect()
    }

    /// Runs `f` on a thread whose stack is far too small to walk the Data
    /// `f` builds recursively.
    fn in_small_stack(f: impl FnOnce() + Send + 'static) {
        std::thread::Builder::new()
            .stack_size(1024 * 1024)
            .spawn(f)
            .unwrap()
            .join()
            .unwrap();
    }

    #[test]
    #[cfg_attr(
        target_family = "wasm",
        ignore = "spawns a thread with a bounded stack"
    )]
    fn handles_every_deep_data_encoding_in_small_stack() {
        #[cfg(not(miri))]
        const DEPTH: usize = 100_000;
        #[cfg(miri)]
        const DEPTH: usize = 2_000;

        in_small_stack(|| {
            let zeros = deep_data(DEPTH, "00");
            let ones = deep_data(DEPTH, "01");

            for ((name, bytes, serialised), (_, other_bytes, _)) in zeros.iter().zip(&ones) {
                let data = Data::decode_fragment(bytes).unwrap();
                let again = Data::decode_fragment(bytes).unwrap();
                let other = Data::decode_fragment(other_bytes).unwrap();

                assert!(data.to_cbor() == *bytes, "{name}: encodes as decoded");
                assert!(data.serialise() == *serialised, "{name}: serialises");
                assert_eq!(data.cmp(&again), Ordering::Equal, "{name}");
                assert_eq!(data.cmp(&other), Ordering::Less, "{name}");
                assert_eq!(other.cmp(&data), Ordering::Greater, "{name}");
            }

            // Equal whatever the encoding, as at any depth.
            for (left_name, left, left_serialised) in &zeros {
                for (right_name, right, right_serialised) in &zeros {
                    let left_data = Data::decode_fragment(left).unwrap();
                    let right_data = Data::decode_fragment(right).unwrap();

                    assert_eq!(
                        left_data == right_data,
                        left_serialised == right_serialised,
                        "{left_name} and {right_name}"
                    );
                }
            }
        });
    }

    /// Compares its two arguments, then serialises the first: each walks the
    /// whole Data.
    fn compares_then_serialises() -> Program<NamedDeBruijn> {
        crate::parser::program(
            "(program 1.1.0
              (lam a (lam b
                (force [ [ [ (force (builtin ifThenElse))
                             [ [ (builtin equalsData) a ] b ] ]
                           (delay [ (builtin serialiseData) a ]) ]
                         (delay (error)) ]))))",
        )
        .unwrap()
        .try_into()
        .unwrap()
    }

    #[test]
    #[cfg_attr(
        target_family = "wasm",
        ignore = "spawns a thread with a bounded stack"
    )]
    fn evaluates_deep_data_arguments_in_small_stack() {
        // The result and cost of each evaluation, as they were when the Data
        // was walked recursively, given enough stack.
        for (name, cpu, mem) in [
            ("list", 96_239_884_758, 802_312),
            ("constr", 96_239_884_758, 802_312),
            ("indefinite map value", 216_535_384_758, 1_802_312),
        ] {
            in_small_stack(move || {
                let (_, bytes, serialised) = deep_data(100_000, "00")
                    .into_iter()
                    .find(|(shape, _, _)| *shape == name)
                    .unwrap();
                let argument = || Data::decode_fragment(&bytes).unwrap();
                let result = compares_then_serialises()
                    .apply_data(argument())
                    .apply_data(argument())
                    .eval(ExBudget::max());

                assert_eq!(
                    (name, result.result().unwrap(), result.cost()),
                    (
                        name,
                        Term::Constant(Constant::ByteString(serialised).into()),
                        ExBudget { mem, cpu },
                    )
                );
            });
        }
    }

    #[test]
    fn equals_data_on_shared_and_unshared_trees() {
        let items = (0..50).map(|i| Data::integer(i.into())).collect::<Vec<_>>();
        let data = Data::constr(3, vec![Data::list(items.clone()), Data::map(vec![])]);

        let true_term = Term::bool(true);
        let false_term = Term::bool(false);

        let (shared, shared_cost) = equals_data(data.clone(), data.clone());
        let (unshared, unshared_cost) = equals_data(data.clone(), data.deep_clone());
        assert_eq!(shared, true_term);
        assert_eq!(unshared, true_term);
        assert_eq!(shared_cost, unshared_cost);

        // Equal regardless of how either side is encoded.
        let indefinite = Data::Constr(Constr::new(
            3,
            Array::def([
                Data::Array(Array::def(items.clone())),
                Data::Map(Map::indef([])),
            ]),
        ));
        let (result, cost) = equals_data(data.clone(), indefinite);
        assert_eq!(result, true_term);
        assert_eq!(cost, shared_cost);

        // Sharing a prefix, but differing in the last element.
        let mut other_items = items;
        *other_items.last_mut().unwrap() = Data::integer(1000.into());
        let other = Data::constr(3, vec![Data::list(other_items), Data::map(vec![])]);
        let (result, _) = equals_data(data.clone(), other.clone());
        assert_eq!(result, false_term);
        assert_eq!(data.cmp(&other), Ordering::Less);
    }

    #[test]
    fn children_are_shared() {
        let fields = Data::list((0..10).map(|i| Data::integer(i.into())).collect());
        let Data::Array(fields) = fields else {
            unreachable!()
        };
        let constr = Constr::new(0, Array::from_data_list(fields.as_data_list().clone()));

        assert!(constr.fields.as_data_list().ptr_eq(fields.as_data_list()));
        assert!(Rc::ptr_eq(
            &constr.fields.as_data_list()[3],
            &fields.as_data_list()[3]
        ));
    }

    #[test]
    fn drops_deep_data_in_bounded_stack() {
        #[cfg(not(miri))]
        const DEPTH: usize = 1_000_000;
        #[cfg(miri)]
        const DEPTH: usize = 2_000;

        // Room for the nested drops allowed before queuing, even in a debug
        // build, but far too little to drop the whole nesting recursively.
        std::thread::Builder::new()
            .stack_size(4 * 1024 * 1024)
            .spawn(|| {
                let mut data = Data::bytestring(vec![]);
                for i in 0..DEPTH {
                    data = match i % 3 {
                        0 => Data::constr(0, vec![data, Data::integer(i.into())]),
                        1 => Data::list(vec![data]),
                        _ => Data::map(vec![(Data::bytestring(vec![]), data)]),
                    };
                }

                // Shared below the top, so part of the tree outlives the
                // first drop.
                let inner = match &data {
                    Data::Constr(constr) => constr.fields.get(0),
                    Data::Array(array) => array.get(0),
                    Data::Map(map) => map.iter().next().map(|(_, value)| value),
                    Data::BigInt(_) | Data::BoundedBytes(_) => None,
                }
                .unwrap()
                .clone();

                drop(data);
                drop(inner);
            })
            .unwrap()
            .join()
            .unwrap();
    }
}
