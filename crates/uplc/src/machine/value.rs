use std::{collections::VecDeque, ops::Deref, rc::Rc};

use bumpalo::{collections::Vec as BumpVec, Bump};
use num_bigint::BigInt;
use num_traits::Signed;
use pallas_primitives::babbage::{self as pallas, PlutusData};

use crate::{
    ast::{Constant, NamedDeBruijn, Term, Type},
    builtins::DefaultFunction,
};

use super::{runtime::BuiltinRuntime, Error};

pub(super) type Env<'a> = BumpVec<'a, &'a Value<'a>>;

#[derive(Clone, Debug)]
pub enum Value<'a> {
    Con(&'a Constant),
    Delay(Rc<Term<NamedDeBruijn>>, Env<'a>),
    Lambda {
        parameter_name: Rc<NamedDeBruijn>,
        body: Rc<Term<NamedDeBruijn>>,
        env: Env<'a>,
    },
    Builtin {
        fun: DefaultFunction,
        term: Rc<Term<NamedDeBruijn>>,
        runtime: BuiltinRuntime<'a>,
    },
}

impl<'a> Value<'a> {
    pub fn integer(arena: &'a Bump, n: BigInt) -> &'a mut Self {
        let constant = arena.alloc(Constant::Integer(n));

        arena.alloc(Value::Con(constant))
    }

    pub fn bool(arena: &'a Bump, n: bool) -> &'a mut Self {
        let constant = arena.alloc(Constant::Bool(n));

        arena.alloc(Value::Con(constant))
    }

    pub fn byte_string(arena: &'a Bump, n: Vec<u8>) -> &'a mut Self {
        let constant = arena.alloc(Constant::ByteString(n));

        arena.alloc(Value::Con(constant))
    }

    pub fn string(arena: &'a Bump, n: String) -> &'a mut Self {
        let constant = arena.alloc(Constant::String(n));

        arena.alloc(Value::Con(constant))
    }

    pub fn list(arena: &'a Bump, typ: Type, n: Vec<Constant>) -> &'a mut Self {
        let constant = arena.alloc(Constant::ProtoList(typ, n));

        arena.alloc(Value::Con(constant))
    }

    pub fn data(arena: &'a Bump, d: PlutusData) -> &'a mut Self {
        let constant = arena.alloc(Constant::Data(d));

        arena.alloc(Value::Con(constant))
    }

    pub(super) fn unwrap_integer(&self) -> &BigInt {
        let Value::Con(Constant::Integer(integer)) = self else {unreachable!()};

        integer
    }

    pub(super) fn unwrap_byte_string(&self) -> &Vec<u8> {
        let Value::Con(Constant::ByteString(byte_string)) = self else {unreachable!()};

        byte_string
    }

    pub(super) fn unwrap_string(&self) -> &String {
        let Value::Con(Constant::String(string)) = self else {unreachable!()};

        string
    }

    pub(super) fn unwrap_bool(&self) -> &bool {
        let Value::Con(Constant::Bool(condition)) = self else {unreachable!()};

        condition
    }

    pub(super) fn unwrap_pair(&self) -> (&Type, &Type, &Rc<Constant>, &Rc<Constant>) {
        let Value::Con(Constant::ProtoPair(t1, t2, first, second)) = self else {unreachable!()};

        (t1, t2, first, second)
    }

    pub(super) fn unwrap_list(&self) -> (&Type, &Vec<Constant>) {
        let Value::Con(Constant::ProtoList(t, list)) = self else {unreachable!()};

        (t, list)
    }

    pub(super) fn unwrap_constant(&self) -> &'a Constant {
        let Value::Con(item) = self else {unreachable!()};

        *item
    }

    pub(super) fn unwrap_data_list(&self) -> &Vec<Constant> {
        let Value::Con(Constant::ProtoList(Type::Data, list)) = self else {unreachable!()};

        list
    }

    pub(super) fn is_integer(&self) -> bool {
        matches!(self, Value::Con(i) if matches!(i, Constant::Integer(_)))
    }

    pub(super) fn is_bool(&self) -> bool {
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
        let constant: Constant = self.try_into()?;

        let constant_type = Type::from(&constant);

        if constant_type == r#type {
            Ok(())
        } else {
            Err(Error::TypeMismatch(r#type, constant_type))
        }
    }

    pub fn expect_list(&self) -> Result<(), Error> {
        let constant: Constant = self.try_into()?;

        let constant_type = Type::from(&constant);

        if matches!(constant_type, Type::List(_)) {
            Ok(())
        } else {
            Err(Error::ListTypeMismatch(constant_type))
        }
    }

    pub fn expect_pair(&self) -> Result<(), Error> {
        let constant: Constant = self.try_into()?;

        let constant_type = Type::from(&constant);

        if matches!(constant_type, Type::Pair(_, _)) {
            Ok(())
        } else {
            Err(Error::PairTypeMismatch(constant_type))
        }
    }
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

#[cfg(test)]
mod tests {
    use num_bigint::BigInt;

    use super::{integer_log2, Value};
    use crate::ast::Constant;

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
