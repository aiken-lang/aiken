use super::{
    Error,
    runtime::{self, BuiltinRuntime},
};
use crate::{
    ast::{Constant, NamedDeBruijn, Term, Type},
    builtins::DefaultFunction,
};
use num_bigint::BigInt;
use num_traits::{Signed, ToPrimitive, Zero};
use pallas_primitives::conway::{self, PlutusData};
use std::{collections::VecDeque, mem::size_of, ops::Deref, rc::Rc};

/// Environment type that preserves variable names for debugging.
/// Each entry is a (name, value) pair where the name comes from the Lambda parameter.
pub type Env<C> = Rc<Vec<(Rc<NamedDeBruijn>, Value<C>)>>;

#[derive(Clone, Debug, PartialEq)]
pub enum Value<C> {
    Con(Rc<Constant>),
    Delay(Rc<Term<NamedDeBruijn, C>>, Env<C>),
    Lambda {
        parameter_name: Rc<NamedDeBruijn>,
        body: Rc<Term<NamedDeBruijn, C>>,
        env: Env<C>,
    },
    Builtin {
        fun: DefaultFunction,
        runtime: BuiltinRuntime<C>,
    },
    Constr {
        tag: usize,
        fields: Vec<Value<C>>,
    },
}

impl<C> Value<C> {
    pub fn integer(n: BigInt) -> Self {
        let constant = Constant::Integer(n);

        Value::Con(constant.into())
    }

    pub fn bool(n: bool) -> Self {
        let constant = Constant::Bool(n);

        Value::Con(constant.into())
    }

    pub fn byte_string(n: Vec<u8>) -> Self {
        let constant = Constant::ByteString(n);

        Value::Con(constant.into())
    }

    pub fn string(n: String) -> Self {
        let constant = Constant::String(n);

        Value::Con(constant.into())
    }

    pub fn list(typ: Type, n: Vec<Constant>) -> Self {
        let constant = Constant::ProtoList(typ, n);

        Value::Con(constant.into())
    }

    pub fn data(d: PlutusData) -> Self {
        let constant = Constant::Data(d);

        Value::Con(constant.into())
    }

    /// Erase the context type, converting Value<C> to Value<()>.
    /// Used when creating errors that need a context-free value.
    pub fn erase_context(&self) -> Value<()>
    where
        C: Clone,
    {
        match self {
            Value::Con(c) => Value::Con(c.clone()),
            Value::Delay(term, env) => Value::Delay(
                Rc::new(term.as_ref().clone().map_context(|_| ())),
                Rc::new(
                    env.iter()
                        .map(|(name, v)| (name.clone(), v.erase_context()))
                        .collect(),
                ),
            ),
            Value::Lambda {
                parameter_name,
                body,
                env,
            } => Value::Lambda {
                parameter_name: parameter_name.clone(),
                body: Rc::new(body.as_ref().clone().map_context(|_| ())),
                env: Rc::new(
                    env.iter()
                        .map(|(name, v)| (name.clone(), v.erase_context()))
                        .collect(),
                ),
            },
            Value::Builtin { fun, runtime } => Value::Builtin {
                fun: *fun,
                runtime: BuiltinRuntime {
                    fun: runtime.fun,
                    args: runtime.args.iter().map(|v| v.erase_context()).collect(),
                    forces: runtime.forces,
                },
            },
            Value::Constr { tag, fields } => Value::Constr {
                tag: *tag,
                fields: fields.iter().map(|v| v.erase_context()).collect(),
            },
        }
    }

    pub(super) fn unwrap_integer(&self) -> Result<&BigInt, Error>
    where
        C: Clone,
    {
        let inner = self.unwrap_constant()?;

        let Constant::Integer(integer) = inner else {
            return Err(Error::TypeMismatch(Type::Integer, inner.into()));
        };

        Ok(integer)
    }

    pub(super) fn unwrap_byte_string(&self) -> Result<&Vec<u8>, Error>
    where
        C: Clone,
    {
        let inner = self.unwrap_constant()?;

        let Constant::ByteString(byte_string) = inner else {
            return Err(Error::TypeMismatch(Type::ByteString, inner.into()));
        };

        Ok(byte_string)
    }

    pub(super) fn unwrap_string(&self) -> Result<&String, Error>
    where
        C: Clone,
    {
        let inner = self.unwrap_constant()?;

        let Constant::String(string) = inner else {
            return Err(Error::TypeMismatch(Type::String, inner.into()));
        };

        Ok(string)
    }

    pub(super) fn unwrap_bool(&self) -> Result<&bool, Error>
    where
        C: Clone,
    {
        let inner = self.unwrap_constant()?;

        let Constant::Bool(condition) = inner else {
            return Err(Error::TypeMismatch(Type::Bool, inner.into()));
        };

        Ok(condition)
    }

    #[allow(clippy::type_complexity)]
    pub(super) fn unwrap_pair(&self) -> Result<(&Type, &Type, &Rc<Constant>, &Rc<Constant>), Error>
    where
        C: Clone,
    {
        let inner = self.unwrap_constant()?;

        let Constant::ProtoPair(t1, t2, first, second) = inner else {
            return Err(Error::PairTypeMismatch(inner.into()));
        };

        Ok((t1, t2, first, second))
    }

    pub(super) fn unwrap_list(&self) -> Result<(&Type, &Vec<Constant>), Error>
    where
        C: Clone,
    {
        let inner = self.unwrap_constant()?;

        let Constant::ProtoList(t, list) = inner else {
            return Err(Error::ListTypeMismatch(inner.into()));
        };

        Ok((t, list))
    }

    pub(super) fn unwrap_data(&self) -> Result<&PlutusData, Error>
    where
        C: Clone,
    {
        let inner = self.unwrap_constant()?;

        let Constant::Data(data) = inner else {
            return Err(Error::TypeMismatch(Type::Data, inner.into()));
        };

        Ok(data)
    }

    pub(super) fn unwrap_unit(&self) -> Result<(), Error>
    where
        C: Clone,
    {
        let inner = self.unwrap_constant()?;

        let Constant::Unit = inner else {
            return Err(Error::TypeMismatch(Type::Unit, inner.into()));
        };

        Ok(())
    }

    pub(super) fn unwrap_constant(&self) -> Result<&Constant, Error>
    where
        C: Clone,
    {
        let Value::Con(item) = self else {
            return Err(Error::NotAConstant(self.erase_context()));
        };

        Ok(item.as_ref())
    }

    pub(super) fn unwrap_data_list(&self) -> Result<&Vec<Constant>, Error>
    where
        C: Clone,
    {
        let inner = self.unwrap_constant()?;

        let Constant::ProtoList(Type::Data, list) = inner else {
            return Err(Error::TypeMismatch(
                Type::List(Type::Data.into()),
                inner.into(),
            ));
        };

        Ok(list)
    }

    pub(super) fn unwrap_int_list(&self) -> Result<&Vec<Constant>, Error>
    where
        C: Clone,
    {
        let inner = self.unwrap_constant()?;

        let Constant::ProtoList(Type::Integer, list) = inner else {
            return Err(Error::TypeMismatch(
                Type::List(Type::Integer.into()),
                inner.into(),
            ));
        };

        Ok(list)
    }

    pub(super) fn unwrap_bls12_381_g1_element(&self) -> Result<&blst::blst_p1, Error>
    where
        C: Clone,
    {
        let inner = self.unwrap_constant()?;

        let Constant::Bls12_381G1Element(element) = inner else {
            return Err(Error::TypeMismatch(Type::Bls12_381G1Element, inner.into()));
        };

        Ok(element)
    }

    pub(super) fn unwrap_bls12_381_g2_element(&self) -> Result<&blst::blst_p2, Error>
    where
        C: Clone,
    {
        let inner = self.unwrap_constant()?;

        let Constant::Bls12_381G2Element(element) = inner else {
            return Err(Error::TypeMismatch(Type::Bls12_381G2Element, inner.into()));
        };

        Ok(element)
    }

    pub(super) fn unwrap_bls12_381_ml_result(&self) -> Result<&blst::blst_fp12, Error>
    where
        C: Clone,
    {
        let inner = self.unwrap_constant()?;

        let Constant::Bls12_381MlResult(element) = inner else {
            return Err(Error::TypeMismatch(Type::Bls12_381MlResult, inner.into()));
        };

        Ok(element)
    }

    pub fn is_integer(&self) -> bool {
        matches!(self, Value::Con(i) if matches!(i.as_ref(), Constant::Integer(_)))
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, Value::Con(b) if matches!(b.as_ref(), Constant::Bool(_)))
    }

    pub fn cost_as_size(&self, func: DefaultFunction) -> Result<i64, Error>
    where
        C: Clone,
    {
        let size = self.unwrap_integer()?;

        if size.is_negative() {
            let error = match func {
                DefaultFunction::IntegerToByteString => {
                    Error::IntegerToByteStringNegativeSize(size.clone())
                }
                DefaultFunction::ReplicateByte => Error::ReplicateByteNegativeSize(size.clone()),
                _ => unreachable!(),
            };
            return Err(error);
        }

        if size > &BigInt::from(runtime::INTEGER_TO_BYTE_STRING_MAXIMUM_OUTPUT_LENGTH) {
            let error = match func {
                DefaultFunction::IntegerToByteString => Error::IntegerToByteStringSizeTooBig(
                    size.clone(),
                    runtime::INTEGER_TO_BYTE_STRING_MAXIMUM_OUTPUT_LENGTH,
                ),
                DefaultFunction::ReplicateByte => Error::ReplicateByteSizeTooBig(
                    size.clone(),
                    runtime::INTEGER_TO_BYTE_STRING_MAXIMUM_OUTPUT_LENGTH,
                ),
                _ => unreachable!(),
            };
            return Err(error);
        }

        let arg1: i64 = u64::try_from(size).unwrap().try_into().unwrap();

        let arg1_exmem = if arg1 == 0 { 0 } else { ((arg1 - 1) / 8) + 1 };

        Ok(arg1_exmem)
    }

    // TODO: Make this to_ex_mem not recursive.
    pub fn to_ex_mem(&self) -> i64 {
        match self {
            Value::Con(c) => match c.as_ref() {
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
                    acc + Value::<()>::Con(constant.clone().into()).to_ex_mem()
                }),
                Constant::ProtoPair(_, _, l, r) => {
                    Value::<()>::Con(l.clone()).to_ex_mem()
                        + Value::<()>::Con(r.clone()).to_ex_mem()
                }
                Constant::Data(item) => self.data_to_ex_mem(item),
                Constant::Bls12_381G1Element(_) => size_of::<blst::blst_p1>() as i64 / 8,
                Constant::Bls12_381G2Element(_) => size_of::<blst::blst_p2>() as i64 / 8,
                Constant::Bls12_381MlResult(_) => size_of::<blst::blst_fp12>() as i64 / 8,
            },
            Value::Delay(_, _) => 1,
            Value::Lambda { .. } => 1,
            Value::Builtin { .. } => 1,
            Value::Constr { .. } => 1,
        }
    }

    // I made data not recursive since data tends to be deeply nested
    // thus causing a significant hit on performance
    pub fn data_to_ex_mem(&self, data: &PlutusData) -> i64 {
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

                    total += Value::<()>::Con(Constant::Integer(i).into()).to_ex_mem();
                }
                PlutusData::BoundedBytes(b) => {
                    let byte_string: Vec<u8> = b.deref().clone();
                    total += Value::<()>::Con(Constant::ByteString(byte_string).into()).to_ex_mem();
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

    pub fn expect_type(&self, r#type: Type) -> Result<(), Error>
    where
        C: Clone,
    {
        let constant: Constant = self.try_into()?;

        let constant_type = Type::from(&constant);

        if constant_type == r#type {
            Ok(())
        } else {
            Err(Error::TypeMismatch(r#type, constant_type))
        }
    }

    pub fn expect_list(&self) -> Result<(), Error>
    where
        C: Clone,
    {
        let constant: Constant = self.try_into()?;

        let constant_type = Type::from(&constant);

        if matches!(constant_type, Type::List(_)) {
            Ok(())
        } else {
            Err(Error::ListTypeMismatch(constant_type))
        }
    }

    pub fn expect_pair(&self) -> Result<(), Error>
    where
        C: Clone,
    {
        let constant: Constant = self.try_into()?;

        let constant_type = Type::from(&constant);

        if matches!(constant_type, Type::Pair(_, _)) {
            Ok(())
        } else {
            Err(Error::PairTypeMismatch(constant_type))
        }
    }
}

impl Value<()> {
    /// Convert Value<()> to Value<C> using C::default() for all context values.
    /// Used when builtin results need to be lifted into a generic context.
    pub fn with_default_context<C: Clone + Default>(self) -> Value<C> {
        match self {
            Value::Con(c) => Value::Con(c),
            Value::Delay(term, env) => Value::Delay(
                Rc::new(term.as_ref().clone().map_context(|_| C::default())),
                Rc::new(
                    env.iter()
                        .map(|(name, v)| (name.clone(), v.clone().with_default_context()))
                        .collect(),
                ),
            ),
            Value::Lambda {
                parameter_name,
                body,
                env,
            } => Value::Lambda {
                parameter_name,
                body: Rc::new(body.as_ref().clone().map_context(|_| C::default())),
                env: Rc::new(
                    env.iter()
                        .map(|(name, v)| (name.clone(), v.clone().with_default_context()))
                        .collect(),
                ),
            },
            Value::Builtin { fun, runtime } => Value::Builtin {
                fun,
                runtime: BuiltinRuntime {
                    fun: runtime.fun,
                    args: runtime
                        .args
                        .into_iter()
                        .map(|v| v.with_default_context())
                        .collect(),
                    forces: runtime.forces,
                },
            },
            Value::Constr { tag, fields } => Value::Constr {
                tag,
                fields: fields
                    .into_iter()
                    .map(|v| v.with_default_context())
                    .collect(),
            },
        }
    }
}

impl<C: Clone> TryFrom<Value<C>> for Type {
    type Error = Error;

    fn try_from(value: Value<C>) -> Result<Self, Self::Error> {
        let constant: Constant = value.try_into()?;

        let constant_type = Type::from(&constant);

        Ok(constant_type)
    }
}

impl<C: Clone> TryFrom<&Value<C>> for Type {
    type Error = Error;

    fn try_from(value: &Value<C>) -> Result<Self, Self::Error> {
        let constant: Constant = value.try_into()?;

        let constant_type = Type::from(&constant);

        Ok(constant_type)
    }
}

impl<C: Clone> TryFrom<Value<C>> for Constant {
    type Error = Error;

    fn try_from(value: Value<C>) -> Result<Self, Self::Error> {
        match value {
            Value::Con(constant) => Ok(constant.as_ref().clone()),
            rest => Err(Error::NotAConstant(rest.erase_context())),
        }
    }
}

impl<C: Clone> TryFrom<&Value<C>> for Constant {
    type Error = Error;

    fn try_from(value: &Value<C>) -> Result<Self, Self::Error> {
        match value {
            Value::Con(constant) => Ok(constant.as_ref().clone()),
            rest => Err(Error::NotAConstant(rest.erase_context())),
        }
    }
}

pub fn integer_log2(i: BigInt) -> i64 {
    if i.is_zero() {
        return 0;
    }

    let (_, bytes) = i.to_bytes_be();

    match bytes.first() {
        None => unreachable!("empty number?"),
        Some(u) => (8 - u.leading_zeros() - 1) as i64 + 8 * (bytes.len() - 1) as i64,
    }
}

pub fn from_pallas_bigint(n: &conway::BigInt) -> BigInt {
    match n {
        conway::BigInt::Int(i) => i128::from(*i).into(),
        conway::BigInt::BigUInt(bytes) => BigInt::from_bytes_be(num_bigint::Sign::Plus, bytes),
        conway::BigInt::BigNInt(bytes) => BigInt::from_bytes_be(num_bigint::Sign::Minus, bytes) - 1,
    }
}

pub fn to_pallas_bigint(n: &BigInt) -> conway::BigInt {
    if let Some(i) = n.to_i128() {
        if let Ok(i) = i.try_into() {
            let pallas_int: pallas_codec::utils::Int = i;
            return conway::BigInt::Int(pallas_int);
        }
    }

    if n.is_positive() {
        let (_, bytes) = n.to_bytes_be();
        conway::BigInt::BigUInt(bytes.into())
    } else {
        // Note that this would break if n == 0
        // BUT n == 0 always fits into 64bits and hence would end up in the first branch.
        let n: BigInt = n + 1;
        let (_, bytes) = n.to_bytes_be();
        conway::BigInt::BigNInt(bytes.into())
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::Constant,
        machine::value::{Value, integer_log2},
    };
    use num_bigint::BigInt;

    #[test]
    fn to_ex_mem_bigint() {
        let value: Value<()> = Value::Con(Constant::Integer(1.into()).into());

        assert_eq!(value.to_ex_mem(), 1);

        let value: Value<()> = Value::Con(Constant::Integer(42.into()).into());

        assert_eq!(value.to_ex_mem(), 1);

        let value: Value<()> = Value::Con(
            Constant::Integer(BigInt::parse_bytes("18446744073709551615".as_bytes(), 10).unwrap())
                .into(),
        );

        assert_eq!(value.to_ex_mem(), 1);

        let value: Value<()> = Value::Con(
            Constant::Integer(
                BigInt::parse_bytes("999999999999999999999999999999".as_bytes(), 10).unwrap(),
            )
            .into(),
        );

        assert_eq!(value.to_ex_mem(), 2);

        let value: Value<()> = Value::Con(
            Constant::Integer(
                BigInt::parse_bytes("170141183460469231731687303715884105726".as_bytes(), 10)
                    .unwrap(),
            )
            .into(),
        );

        assert_eq!(value.to_ex_mem(), 2);

        let value: Value<()> = Value::Con(
            Constant::Integer(
                BigInt::parse_bytes("170141183460469231731687303715884105727".as_bytes(), 10)
                    .unwrap(),
            )
            .into(),
        );

        assert_eq!(value.to_ex_mem(), 2);

        let value: Value<()> = Value::Con(
            Constant::Integer(
                BigInt::parse_bytes("170141183460469231731687303715884105728".as_bytes(), 10)
                    .unwrap(),
            )
            .into(),
        );

        assert_eq!(value.to_ex_mem(), 2);

        let value: Value<()> = Value::Con(
            Constant::Integer(
                BigInt::parse_bytes("170141183460469231731687303715884105729".as_bytes(), 10)
                    .unwrap(),
            )
            .into(),
        );

        assert_eq!(value.to_ex_mem(), 2);

        let value: Value<()> = Value::Con(
            Constant::Integer(
                BigInt::parse_bytes("340282366920938463463374607431768211458".as_bytes(), 10)
                    .unwrap(),
            )
            .into(),
        );

        assert_eq!(value.to_ex_mem(), 3);

        let value: Value<()> = Value::Con(
            Constant::Integer(
                BigInt::parse_bytes("999999999999999999999999999999999999999999".as_bytes(), 10)
                    .unwrap(),
            )
            .into(),
        );

        assert_eq!(value.to_ex_mem(), 3);

        let value: Value<()> =
            Value::Con(Constant::Integer(BigInt::parse_bytes("999999999999999999999999999999999999999999999999999999999999999999999999999999999999".as_bytes(), 10).unwrap()).into());

        assert_eq!(value.to_ex_mem(), 5);
    }

    #[test]
    fn integer_log2_oracle() {
        // Values come from the Haskell implementation
        assert_eq!(integer_log2(0.into()), 0);
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
