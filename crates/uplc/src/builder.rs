use crate::{
    ast::{Constant, Name, Term, Type},
    builtins::DefaultFunction,
};
use pallas_primitives::alonzo::PlutusData;
use std::rc::Rc;

pub const CONSTR_FIELDS_EXPOSER: &str = "__constr_fields_exposer";
pub const CONSTR_INDEX_EXPOSER: &str = "__constr_index_exposer";
pub const EXPECT_ON_LIST: &str = "__expect_on_list";
pub const INNER_EXPECT_ON_LIST: &str = "__inner_expect_on_list";
pub const INDICES_CONVERTER: &str = "__indices_converter";

impl<T, C> Term<T, C>
where
    T: std::fmt::Debug,
    C: Default,
{
    // Ergonomic constructors for variants that previously had no fields
    pub fn error() -> Self {
        Term::Error {
            context: C::default(),
        }
    }

    pub fn builtin(func: DefaultFunction) -> Self {
        Term::Builtin {
            func,
            context: C::default(),
        }
    }

    pub fn constant(value: impl Into<Rc<Constant>>) -> Self {
        Term::Constant {
            value: value.into(),
            context: C::default(),
        }
    }

    // Terms
    pub fn apply(self, arg: Self) -> Self {
        Term::Apply {
            function: self.into(),
            argument: arg.into(),
            context: C::default(),
        }
    }

    pub fn force(self) -> Self {
        Term::Force {
            term: self.into(),
            context: C::default(),
        }
    }

    /// Like `force()` but preserves source location context.
    pub fn force_with_ctx(self, context: C) -> Self {
        Term::Force {
            term: self.into(),
            context,
        }
    }

    pub fn delay(self) -> Self {
        Term::Delay {
            term: self.into(),
            context: C::default(),
        }
    }

    pub fn constr(tag: usize, fields: Vec<Term<T, C>>) -> Self {
        Term::Constr {
            tag,
            fields,
            context: C::default(),
        }
    }

    pub fn case(self, branches: Vec<Term<T, C>>) -> Self {
        Term::Case {
            constr: self.into(),
            branches,
            context: C::default(),
        }
    }

    // Primitives
    pub fn integer(i: num_bigint::BigInt) -> Self {
        Term::Constant {
            value: Constant::Integer(i).into(),
            context: C::default(),
        }
    }

    pub fn string(s: impl ToString) -> Self {
        Term::Constant {
            value: Constant::String(s.to_string()).into(),
            context: C::default(),
        }
    }

    pub fn byte_string(b: Vec<u8>) -> Self {
        Term::Constant {
            value: Constant::ByteString(b).into(),
            context: C::default(),
        }
    }

    pub fn bls12_381_g1(b: blst::blst_p1) -> Self {
        Term::Constant {
            value: Constant::Bls12_381G1Element(b.into()).into(),
            context: C::default(),
        }
    }

    pub fn bls12_381_g2(b: blst::blst_p2) -> Self {
        Term::Constant {
            value: Constant::Bls12_381G2Element(b.into()).into(),
            context: C::default(),
        }
    }

    pub fn bool(b: bool) -> Self {
        Term::Constant {
            value: Constant::Bool(b).into(),
            context: C::default(),
        }
    }

    pub fn unit() -> Self {
        Term::Constant {
            value: Constant::Unit.into(),
            context: C::default(),
        }
    }

    pub fn data(d: PlutusData) -> Self {
        Term::Constant {
            value: Constant::Data(d).into(),
            context: C::default(),
        }
    }

    pub fn empty_list() -> Self {
        Term::Constant {
            value: Constant::ProtoList(Type::Data, vec![]).into(),
            context: C::default(),
        }
    }

    pub fn list_values(vals: Vec<Constant>) -> Self {
        Term::Constant {
            value: Constant::ProtoList(Type::Data, vals).into(),
            context: C::default(),
        }
    }

    pub fn int_values(vals: Vec<Constant>) -> Self {
        Term::Constant {
            value: Constant::ProtoList(Type::Integer, vals).into(),
            context: C::default(),
        }
    }

    pub fn empty_map() -> Self {
        Term::Constant {
            value: Constant::ProtoList(Type::Pair(Type::Data.into(), Type::Data.into()), vec![])
                .into(),
            context: C::default(),
        }
    }

    pub fn map_values(vals: Vec<Constant>) -> Self {
        Term::Constant {
            value: Constant::ProtoList(Type::Pair(Type::Data.into(), Type::Data.into()), vals)
                .into(),
            context: C::default(),
        }
    }

    pub fn pair_values(fst_val: Constant, snd_val: Constant) -> Self {
        Term::Constant {
            value: Constant::ProtoPair(Type::Data, Type::Data, fst_val.into(), snd_val.into())
                .into(),
            context: C::default(),
        }
    }

    // Context-aware primitives (for source map support)
    pub fn integer_with_ctx(i: num_bigint::BigInt, context: C) -> Self {
        Term::Constant {
            value: Constant::Integer(i).into(),
            context,
        }
    }

    pub fn string_with_ctx(s: impl ToString, context: C) -> Self {
        Term::Constant {
            value: Constant::String(s.to_string()).into(),
            context,
        }
    }

    pub fn byte_string_with_ctx(b: Vec<u8>, context: C) -> Self {
        Term::Constant {
            value: Constant::ByteString(b).into(),
            context,
        }
    }

    pub fn bool_with_ctx(b: bool, context: C) -> Self {
        Term::Constant {
            value: Constant::Bool(b).into(),
            context,
        }
    }

    pub fn unit_with_ctx(context: C) -> Self {
        Term::Constant {
            value: Constant::Unit.into(),
            context,
        }
    }

    // This section contains builders for builtins from default functions
    // Theses are in _alphabetical order_
    // The naming convention almost follows PascalCase -> snake_case
    // Exceptions include the use of `un`.

    pub fn add_integer() -> Self {
        Term::builtin(DefaultFunction::AddInteger)
    }

    pub fn append_bytearray() -> Self {
        Term::builtin(DefaultFunction::AppendByteString)
    }

    pub fn append_string() -> Self {
        Term::builtin(DefaultFunction::AppendString)
    }

    pub fn b_data() -> Self {
        Term::builtin(DefaultFunction::BData)
    }

    pub fn blake2b_224() -> Self {
        Term::builtin(DefaultFunction::Blake2b_224)
    }

    pub fn blake2b_256() -> Self {
        Term::builtin(DefaultFunction::Blake2b_256)
    }

    pub fn bls12_381_g1_add() -> Self {
        Term::builtin(DefaultFunction::Bls12_381_G1_Add)
    }
    pub fn bls12_381_g1_neg() -> Self {
        Term::builtin(DefaultFunction::Bls12_381_G1_Neg)
    }
    pub fn bls12_381_g1_scalar_mul() -> Self {
        Term::builtin(DefaultFunction::Bls12_381_G1_ScalarMul)
    }
    pub fn bls12_381_g1_equal() -> Self {
        Term::builtin(DefaultFunction::Bls12_381_G1_Equal)
    }
    pub fn bls12_381_g1_compress() -> Self {
        Term::builtin(DefaultFunction::Bls12_381_G1_Compress)
    }
    pub fn bls12_381_g1_uncompress() -> Self {
        Term::builtin(DefaultFunction::Bls12_381_G1_Uncompress)
    }
    pub fn bls12_381_g1_hash_to_group() -> Self {
        Term::builtin(DefaultFunction::Bls12_381_G1_HashToGroup)
    }
    pub fn bls12_381_g2_add() -> Self {
        Term::builtin(DefaultFunction::Bls12_381_G2_Add)
    }
    pub fn bls12_381_g2_neg() -> Self {
        Term::builtin(DefaultFunction::Bls12_381_G2_Neg)
    }
    pub fn bls12_381_g2_scalar_mul() -> Self {
        Term::builtin(DefaultFunction::Bls12_381_G2_ScalarMul)
    }
    pub fn bls12_381_g2_equal() -> Self {
        Term::builtin(DefaultFunction::Bls12_381_G2_Equal)
    }
    pub fn bls12_381_g2_compress() -> Self {
        Term::builtin(DefaultFunction::Bls12_381_G2_Compress)
    }
    pub fn bls12_381_g2_uncompress() -> Self {
        Term::builtin(DefaultFunction::Bls12_381_G2_Uncompress)
    }
    pub fn bls12_381_g2_hash_to_group() -> Self {
        Term::builtin(DefaultFunction::Bls12_381_G2_HashToGroup)
    }
    pub fn bls12_381_miller_loop() -> Self {
        Term::builtin(DefaultFunction::Bls12_381_MillerLoop)
    }
    pub fn bls12_381_mul_ml_result() -> Self {
        Term::builtin(DefaultFunction::Bls12_381_MulMlResult)
    }
    pub fn bls12_381_final_verify() -> Self {
        Term::builtin(DefaultFunction::Bls12_381_FinalVerify)
    }

    pub fn choose_data(
        self,
        constr_case: Self,
        map_case: Self,
        array_case: Self,
        int_case: Self,
        bytes_case: Self,
    ) -> Self {
        Term::builtin(DefaultFunction::ChooseData)
            .force()
            .apply(self)
            .apply(constr_case)
            .apply(map_case)
            .apply(array_case)
            .apply(int_case)
            .apply(bytes_case)
    }

    pub fn choose_list(self, then_term: Self, else_term: Self) -> Self {
        Term::builtin(DefaultFunction::ChooseList)
            .force()
            .force()
            .apply(self)
            .apply(then_term)
            .apply(else_term)
    }

    pub fn choose_unit(self, then_term: Self) -> Self {
        Term::builtin(DefaultFunction::ChooseUnit)
            .force()
            .apply(self)
            .apply(then_term)
    }

    pub fn cons_bytearray() -> Self {
        Term::builtin(DefaultFunction::ConsByteString)
    }

    pub fn constr_data() -> Self {
        Term::builtin(DefaultFunction::ConstrData)
    }

    pub fn decode_utf8() -> Self {
        Term::builtin(DefaultFunction::DecodeUtf8)
    }

    pub fn div_integer() -> Self {
        Term::builtin(DefaultFunction::DivideInteger)
    }

    pub fn divide_integer() -> Self {
        Term::builtin(DefaultFunction::DivideInteger)
    }

    pub fn encode_utf8() -> Self {
        Term::builtin(DefaultFunction::EncodeUtf8)
    }

    pub fn equals_bytestring() -> Self {
        Term::builtin(DefaultFunction::EqualsByteString)
    }

    pub fn equals_data() -> Self {
        Term::builtin(DefaultFunction::EqualsData)
    }

    pub fn equals_integer() -> Self {
        Term::builtin(DefaultFunction::EqualsInteger)
    }

    pub fn equals_string() -> Self {
        Term::builtin(DefaultFunction::EqualsString)
    }

    pub fn fst_pair() -> Self {
        Term::builtin(DefaultFunction::FstPair)
            .force()
            .force()
    }

    pub fn head_list() -> Self {
        Term::builtin(DefaultFunction::HeadList)
            .force()
    }

    pub fn i_data() -> Self {
        Term::builtin(DefaultFunction::IData)
    }

    pub fn if_then_else(self, then_term: Self, else_term: Self) -> Self {
        Term::builtin(DefaultFunction::IfThenElse)
            .force()
            .apply(self)
            .apply(then_term)
            .apply(else_term)
    }

    pub fn index_bytearray() -> Self {
        Term::builtin(DefaultFunction::IndexByteString)
    }

    pub fn keccak_256() -> Self {
        Term::builtin(DefaultFunction::Keccak_256)
    }

    pub fn length_of_bytearray() -> Self {
        Term::builtin(DefaultFunction::LengthOfByteString)
    }

    pub fn less_than_bytearray() -> Self {
        Term::builtin(DefaultFunction::LessThanByteString)
    }

    pub fn less_than_equals_bytearray() -> Self {
        Term::builtin(DefaultFunction::LessThanEqualsByteString)
    }

    pub fn less_than_equals_integer() -> Self {
        Term::builtin(DefaultFunction::LessThanEqualsInteger)
    }

    pub fn less_than_integer() -> Self {
        Term::builtin(DefaultFunction::LessThanInteger)
    }

    pub fn list_data() -> Self {
        Term::builtin(DefaultFunction::ListData)
    }

    pub fn map_data() -> Self {
        Term::builtin(DefaultFunction::MapData)
    }

    pub fn mk_cons() -> Self {
        Term::builtin(DefaultFunction::MkCons)
            .force()
    }

    pub fn mk_pair_data() -> Self {
        Term::builtin(DefaultFunction::MkPairData)
    }

    pub fn mod_integer() -> Self {
        Term::builtin(DefaultFunction::ModInteger)
    }

    pub fn multiply_integer() -> Self {
        Term::builtin(DefaultFunction::MultiplyInteger)
    }

    pub fn quotient_integer() -> Self {
        Term::builtin(DefaultFunction::QuotientInteger)
    }

    pub fn remainder_integer() -> Self {
        Term::builtin(DefaultFunction::RemainderInteger)
    }

    pub fn sha2_256() -> Self {
        Term::builtin(DefaultFunction::Sha2_256)
    }

    pub fn sha3_256() -> Self {
        Term::builtin(DefaultFunction::Sha3_256)
    }

    pub fn slice_bytearray() -> Self {
        Term::builtin(DefaultFunction::SliceByteString)
    }

    pub fn snd_pair() -> Self {
        Term::builtin(DefaultFunction::SndPair)
            .force()
            .force()
    }

    pub fn subtract_integer() -> Self {
        Term::builtin(DefaultFunction::SubtractInteger)
    }

    pub fn tail_list() -> Self {
        Term::builtin(DefaultFunction::TailList)
            .force()
    }

    pub fn un_b_data() -> Self {
        Term::builtin(DefaultFunction::UnBData)
    }

    pub fn un_i_data() -> Self {
        Term::builtin(DefaultFunction::UnIData)
    }

    pub fn unconstr_data() -> Self {
        Term::builtin(DefaultFunction::UnConstrData)
    }

    pub fn unlist_data() -> Self {
        Term::builtin(DefaultFunction::UnListData)
    }

    pub fn unmap_data() -> Self {
        Term::builtin(DefaultFunction::UnMapData)
    }

    pub fn verify_ecdsa_secp256k1_signature() -> Self {
        Term::builtin(DefaultFunction::VerifyEcdsaSecp256k1Signature)
    }

    pub fn verify_ed25519_signature() -> Self {
        Term::builtin(DefaultFunction::VerifyEd25519Signature)
    }

    pub fn verify_schnorr_secp256k1_signature() -> Self {
        Term::builtin(DefaultFunction::VerifySchnorrSecp256k1Signature)
    }

    // Unused builtins
    pub fn mk_nil_data() -> Self {
        Term::builtin(DefaultFunction::MkNilData)
    }

    pub fn mk_nil_pair_data() -> Self {
        Term::builtin(DefaultFunction::MkNilPairData)
    }

    pub fn null_list() -> Self {
        Term::builtin(DefaultFunction::NullList)
    }

    pub fn serialise_data() -> Self {
        Term::builtin(DefaultFunction::SerialiseData)
    }

    pub fn write_bits() -> Self {
        Term::builtin(DefaultFunction::WriteBits)
    }

    // Delayed variants - these use force/apply chains
    pub fn delayed_choose_data(
        self,
        constr_case: Self,
        map_case: Self,
        array_case: Self,
        int_case: Self,
        bytes_case: Self,
    ) -> Self {
        Term::builtin(DefaultFunction::ChooseData)
            .force()
            .apply(self)
            .apply(constr_case.delay())
            .apply(map_case.delay())
            .apply(array_case.delay())
            .apply(int_case.delay())
            .apply(bytes_case.delay())
            .force()
    }

    pub fn delayed_choose_list(self, then_term: Self, else_term: Self) -> Self {
        Term::builtin(DefaultFunction::ChooseList)
            .force()
            .force()
            .apply(self)
            .apply(then_term.delay())
            .apply(else_term.delay())
            .force()
    }

    /// Note the otherwise is expected to be a delayed term cast to a Var
    pub fn delay_empty_choose_list(self, empty: Self, otherwise: Self) -> Self {
        Term::builtin(DefaultFunction::ChooseList)
            .force()
            .force()
            .apply(self)
            .apply(empty.delay())
            .apply(otherwise)
            .force()
    }

    /// Note the otherwise is expected to be a delayed term cast to a Var
    pub fn delay_filled_choose_list(self, otherwise: Self, filled: Self) -> Self {
        Term::builtin(DefaultFunction::ChooseList)
            .force()
            .force()
            .apply(self)
            .apply(otherwise)
            .apply(filled.delay())
            .force()
    }

    pub fn delayed_choose_unit(self, then_term: Self) -> Self {
        Term::builtin(DefaultFunction::ChooseUnit)
            .force()
            .apply(self)
            .apply(then_term.delay())
            .force()
    }

    pub fn delayed_if_then_else(self, then_term: Self, else_term: Self) -> Self {
        Term::builtin(DefaultFunction::IfThenElse)
            .force()
            .apply(self)
            .apply(then_term.delay())
            .apply(else_term.delay())
            .force()
    }

    /// Like `delayed_if_then_else` but attaches source location context to the overall expression.
    pub fn delayed_if_then_else_with_ctx(
        self,
        then_term: Self,
        else_term: Self,
        context: C,
    ) -> Self {
        Term::builtin(DefaultFunction::IfThenElse)
            .force()
            .apply(self)
            .apply(then_term.delay())
            .apply(else_term.delay())
            .force_with_ctx(context)
    }

    /// Note the otherwise is expected to be a delayed term cast to a Var
    pub fn delay_true_if_then_else(self, then: Self, otherwise: Self) -> Self {
        Term::builtin(DefaultFunction::IfThenElse)
            .force()
            .apply(self)
            .apply(then.delay())
            .apply(otherwise)
            .force()
    }

    /// Note the otherwise is expected to be a delayed term cast to a Var
    pub fn delay_false_if_then_else(self, otherwise: Self, alternative: Self) -> Self {
        Term::builtin(DefaultFunction::IfThenElse)
            .force()
            .apply(self)
            .apply(otherwise)
            .apply(alternative.delay())
            .force()
    }

    pub fn delayed_trace(self, msg_term: Self) -> Self {
        Term::builtin(DefaultFunction::Trace)
            .force()
            .apply(msg_term)
            .apply(self.delay())
            .force()
    }

    // Misc.
    pub fn repeat_tail_list(self, repeat: usize) -> Self {
        let mut term = self;

        for _ in 0..repeat {
            term = Term::tail_list().apply(term);
        }

        term
    }
}

impl<C: Default> Term<Name, C> {
    pub fn lambda(self, parameter_name: impl ToString) -> Self {
        Term::Lambda {
            parameter_name: Name::text(parameter_name).into(),
            body: self.into(),
            context: Default::default(),
        }
    }

    /// Create a Var term from a string name (Name-specific convenience method)
    pub fn var(name: impl ToString) -> Self {
        Term::Var {
            name: Name::text(name).into(),
            context: Default::default(),
        }
    }
}

impl<C> Term<Name, C> {
    /// Create a Lambda term with explicit context (source location).
    /// Unlike `lambda()` which uses Default::default(), this preserves source info.
    pub fn lambda_with_ctx(self, parameter_name: impl ToString, context: C) -> Self {
        Term::Lambda {
            parameter_name: Name::text(parameter_name).into(),
            body: self.into(),
            context,
        }
    }
}

impl<C: Default + Clone> Term<Name, C> {

    // Misc.
    pub fn constr_fields_exposer(self) -> Self {
        self.lambda(CONSTR_FIELDS_EXPOSER).apply(
            Term::snd_pair()
                .apply(Term::unconstr_data().apply(Term::var("__constr_var")))
                .lambda("__constr_var"),
        )
    }

    pub fn constr_index_exposer(self) -> Self {
        self.lambda(CONSTR_INDEX_EXPOSER).apply(
            Term::fst_pair()
                .apply(Term::unconstr_data().apply(Term::var("__constr_var")))
                .lambda("__constr_var"),
        )
    }

    pub fn data_list_to_integer_list(self) -> Self {
        self.lambda(INDICES_CONVERTER)
            .apply(Term::var(INDICES_CONVERTER).apply(Term::var(INDICES_CONVERTER)))
            .lambda(INDICES_CONVERTER)
            .apply(
                Term::var("xs")
                    .delayed_choose_list(
                        Term::int_values(vec![]),
                        Term::mk_cons()
                            .apply(Term::var("x"))
                            .apply(
                                Term::var(INDICES_CONVERTER)
                                    .apply(Term::var(INDICES_CONVERTER))
                                    .apply(Term::var("rest")),
                            )
                            .lambda("rest")
                            .apply(Term::tail_list().apply(Term::var("xs")))
                            .lambda("x")
                            .apply(
                                Term::un_i_data().apply(Term::head_list().apply(Term::var("xs"))),
                            ),
                    )
                    .lambda("xs")
                    .lambda(INDICES_CONVERTER),
            )
    }

    /// Introduce a let-binding for a given term. The callback receives a Term::Var
    /// whose name matches the given 'var_name'. Handy to re-use a same var across
    /// multiple lambda expressions.
    ///
    /// ## Example
    ///
    ///
    ///
    /// use uplc::ast::Term
    /// let value = Term::var("thing");
    ///
    /// value.as_var("__val", |val| {
    ///   val.do_something()
    ///      .do_another_thing()
    /// })
    ///
    pub fn as_var<F>(self, var_name: &str, callback: F) -> Term<Name, C>
    where
        F: FnOnce(Rc<Name>) -> Term<Name, C>,
    {
        callback(Name::text(var_name).into())
            .lambda(var_name)
            .apply(self)
    }

    /// Continue a computation provided that the current term is a Data-wrapped integer.
    /// The 'callback' receives an integer constant Term as argument.
    pub fn choose_data_integer<F>(var: Rc<Name>, callback: F, otherwise: &Term<Name, C>) -> Self
    where
        F: FnOnce(Term<Name, C>) -> Term<Name, C>,
    {
        Term::Var { name: var.clone(), context: C::default() }
            .choose_data(
                otherwise.clone(),
                otherwise.clone(),
                otherwise.clone(),
                callback(Term::un_i_data().apply(Term::Var { name: var, context: C::default() })).delay(),
                otherwise.clone(),
            )
            .force()
    }

    /// Continue a computation provided that the current term is a Data-wrapped
    /// bytearray. The 'callback' receives a bytearray constant Term as argument.
    pub fn choose_data_bytearray<F>(var: Rc<Name>, callback: F, otherwise: &Term<Name, C>) -> Self
    where
        F: FnOnce(Term<Name, C>) -> Term<Name, C>,
    {
        Term::Var { name: var.clone(), context: C::default() }
            .choose_data(
                otherwise.clone(),
                otherwise.clone(),
                otherwise.clone(),
                otherwise.clone(),
                callback(Term::un_b_data().apply(Term::Var { name: var, context: C::default() })).delay(),
            )
            .force()
    }

    /// Continue a computation provided that the current term is a Data-wrapped
    /// list. The 'callback' receives a ProtoList Term as argument.
    pub fn choose_data_list<F>(var: Rc<Name>, callback: F, otherwise: &Term<Name, C>) -> Self
    where
        F: FnOnce(Term<Name, C>) -> Term<Name, C>,
    {
        Term::Var { name: var.clone(), context: C::default() }
            .choose_data(
                otherwise.clone(),
                otherwise.clone(),
                callback(Term::unlist_data().apply(Term::Var { name: var, context: C::default() })).delay(),
                otherwise.clone(),
                otherwise.clone(),
            )
            .force()
    }

    /// Continue a computation provided that the current term is a Data-wrapped
    /// list. The 'callback' receives a ProtoMap Term as argument.
    pub fn choose_data_map<F>(var: Rc<Name>, callback: F, otherwise: &Term<Name, C>) -> Self
    where
        F: FnOnce(Term<Name, C>) -> Term<Name, C>,
    {
        Term::Var { name: var.clone(), context: C::default() }
            .choose_data(
                otherwise.clone(),
                callback(Term::unmap_data().apply(Term::Var { name: var, context: C::default() })).delay(),
                otherwise.clone(),
                otherwise.clone(),
                otherwise.clone(),
            )
            .force()
    }

    /// Continue a computation provided that the current term is a Data-wrapped
    /// constr. The 'callback' receives a Data as argument.
    pub fn choose_data_constr<F>(var: Rc<Name>, callback: F, otherwise: &Term<Name, C>) -> Self
    where
        F: FnOnce(Term<Name, C>) -> Term<Name, C>,
    {
        Term::Var { name: var.clone(), context: C::default() }
            .choose_data(
                callback(Term::Var { name: var, context: C::default() }).delay(),
                otherwise.clone(),
                otherwise.clone(),
                otherwise.clone(),
                otherwise.clone(),
            )
            .force()
    }

    /// Convert an arbitrary 'term' into a bool term and pass it into a 'callback'.
    /// Continue the execution 'otherwise' with a different branch.
    ///
    /// Note that the 'otherwise' term is expected
    /// to be a delayed term.
    pub fn unwrap_bool_or<F>(self, callback: F, otherwise: &Term<Name, C>) -> Term<Name, C>
    where
        F: FnOnce(Term<Name, C>) -> Term<Name, C>,
    {
        Term::unconstr_data()
            .apply(self)
            .as_var("__pair__", |pair| {
                Term::snd_pair()
                    .apply(Term::Var { name: pair.clone(), context: C::default() })
                    .delay_empty_choose_list(
                        Term::less_than_equals_integer()
                            .apply(Term::integer(2.into()))
                            .apply(Term::fst_pair().apply(Term::Var { name: pair.clone(), context: C::default() }))
                            .delay_false_if_then_else(
                                otherwise.clone(),
                                callback(
                                    Term::equals_integer()
                                        .apply(Term::integer(1.into()))
                                        .apply(Term::fst_pair().apply(Term::Var { name: pair, context: C::default() })),
                                ),
                            ),
                        otherwise.clone(),
                    )
            })
    }

    /// Convert an arbitrary 'term' into a unit term and pass it into a 'callback'.
    /// Continue the execution 'otherwise' with a different branch.
    ///
    /// Note that the 'otherwise' term is expected
    /// to be a delayed term.
    pub fn unwrap_void_or<F>(self, callback: F, otherwise: &Term<Name, C>) -> Term<Name, C>
    where
        F: FnOnce(Term<Name, C>) -> Term<Name, C>,
    {
        assert!(matches!(self, Term::Var { .. }));
        Term::equals_integer()
            .apply(Term::integer(0.into()))
            .apply(Term::fst_pair().apply(Term::unconstr_data().apply(self.clone())))
            .delay_true_if_then_else(
                Term::snd_pair()
                    .apply(Term::unconstr_data().apply(self))
                    .delay_empty_choose_list(callback(Term::unit()), otherwise.clone()),
                otherwise.clone(),
            )
    }

    /// Convert an arbitrary 'term' into a pair and pass it into a 'callback'.
    /// Continue the execution 'otherwise' with a different branch.
    ///
    /// Note that the 'otherwise' term is expected
    /// to be a delayed term.
    pub fn unwrap_pair_or<F>(self, callback: F, otherwise: &Term<Name, C>) -> Term<Name, C>
    where
        F: FnOnce(Term<Name, C>) -> Term<Name, C>,
    {
        self.as_var("__list_data", |list| {
            let left = Term::head_list().apply(Term::Var { name: list.clone(), context: C::default() });

            Term::unwrap_tail_or(
                list,
                |tail| {
                    tail.as_var("__tail", |tail| {
                        let right = Term::head_list().apply(Term::Var { name: tail.clone(), context: C::default() });

                        Term::unwrap_tail_or(
                            tail,
                            |leftovers| {
                                leftovers.delay_empty_choose_list(
                                    callback(Term::mk_pair_data().apply(left).apply(right)),
                                    otherwise.clone(),
                                )
                            },
                            otherwise,
                        )
                    })
                },
                otherwise,
            )
        })
    }

    /// Continue with the tail of a list, if any; or fallback 'otherwise'.
    ///
    /// Note that the 'otherwise' term is expected
    /// to be a delayed term.
    pub fn unwrap_tail_or<F>(var: Rc<Name>, callback: F, otherwise: &Term<Name, C>) -> Term<Name, C>
    where
        F: FnOnce(Term<Name, C>) -> Term<Name, C>,
    {
        Term::Var { name: var.clone(), context: C::default() }.delay_filled_choose_list(
            otherwise.clone(),
            callback(Term::tail_list().apply(Term::Var { name: var, context: C::default() })),
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{Data, Name, NamedDeBruijn, Program, Term},
        builder::Constant,
        machine::{Error, cost_model::ExBudget},
        optimize::interner::CodeGenInterner,
    };

    fn quick_eval(term: Term<Name>) -> Result<Term<NamedDeBruijn>, Error> {
        let version = (1, 0, 0);
        let mut program = Program { version, term };
        CodeGenInterner::new().program(&mut program);
        program
            .to_named_debruijn()
            .expect("failed to convert program to NamedDeBruijn")
            .eval(ExBudget::default())
            .result()
    }

    #[test]
    fn unwrap_bool_or_false() {
        let result = quick_eval(
            Term::data(Data::constr(0, vec![])).unwrap_bool_or(|b| b, &Term::error().delay()),
        );

        assert_eq!(result, Ok(Term::bool(false)));
    }

    #[test]
    fn unwrap_bool_or_true() {
        let result = quick_eval(
            Term::data(Data::constr(1, vec![])).unwrap_bool_or(|b| b, &Term::error().delay()),
        );

        assert_eq!(result, Ok(Term::bool(true)));
    }

    #[test]
    fn unwrap_bool_or_extra_args() {
        let result = quick_eval(
            Term::data(Data::constr(1, vec![Data::integer(42.into())]))
                .unwrap_bool_or(|b| b, &Term::error().delay()),
        );

        assert_eq!(result, Err(Error::EvaluationFailure));
    }

    #[test]
    fn unwrap_bool_or_invalid_constr_hi() {
        let result = quick_eval(
            Term::data(Data::constr(2, vec![])).unwrap_bool_or(|b| b, &Term::error().delay()),
        );

        assert_eq!(result, Err(Error::EvaluationFailure));
    }

    #[test]
    fn unwrap_tail_or_0_elems() {
        let result = quick_eval(Term::list_values(vec![]).as_var("__tail", |tail| {
            Term::unwrap_tail_or(tail, |p| p, &Term::error().delay())
        }));

        assert_eq!(result, Err(Error::EvaluationFailure));
    }

    #[test]
    fn unwrap_tail_or_1_elem() {
        let result = quick_eval(
            Term::list_values(vec![Constant::Data(Data::integer(1.into()))])
                .as_var("__tail", |tail| {
                    Term::unwrap_tail_or(tail, |p| p, &Term::error().delay())
                }),
        );

        assert_eq!(result, Ok(Term::list_values(vec![])),);
    }

    #[test]
    fn unwrap_tail_or_2_elems() {
        let result = quick_eval(
            Term::list_values(vec![
                Constant::Data(Data::integer(1.into())),
                Constant::Data(Data::integer(2.into())),
            ])
            .as_var("__tail", |tail| {
                Term::unwrap_tail_or(tail, |p| p, &Term::error().delay())
            }),
        );

        assert_eq!(
            result,
            Ok(Term::list_values(vec![Constant::Data(Data::integer(
                2.into()
            ))]))
        );
    }

    #[test]
    fn unwrap_pair_or() {
        let result = quick_eval(
            Term::list_values(vec![
                Constant::Data(Data::integer(14.into())),
                Constant::Data(Data::bytestring(vec![1, 2, 3])),
            ])
            .unwrap_pair_or(|p| p, &Term::error().delay()),
        );

        assert_eq!(
            result,
            Ok(Term::pair_values(
                Constant::Data(Data::integer(14.into())),
                Constant::Data(Data::bytestring(vec![1, 2, 3])),
            ))
        );
    }

    #[test]
    fn unwrap_pair_or_not_enough_args_1() {
        let result = quick_eval(
            Term::list_values(vec![Constant::Data(Data::integer(1.into()))])
                .unwrap_pair_or(|p| p, &Term::error().delay()),
        );

        assert_eq!(result, Err(Error::EvaluationFailure));
    }

    #[test]
    fn unwrap_pair_or_not_enough_args_0() {
        let result =
            quick_eval(Term::list_values(vec![]).unwrap_pair_or(|p| p, &Term::error().delay()));

        assert_eq!(result, Err(Error::EvaluationFailure));
    }

    #[test]
    fn unwrap_pair_or_too_many_args() {
        let result = quick_eval(
            Term::list_values(vec![
                Constant::Data(Data::integer(1.into())),
                Constant::Data(Data::integer(2.into())),
                Constant::Data(Data::integer(3.into())),
            ])
            .unwrap_pair_or(|p| p, &Term::error().delay()),
        );

        assert_eq!(result, Err(Error::EvaluationFailure));
    }

    #[test]
    fn unwrap_void_or_happy() {
        let result = quick_eval(
            Term::data(Data::constr(0, vec![])).as_var("__unit", |unit| {
                Term::Var { name: unit, context: () }.unwrap_void_or(|u| u, &Term::error().delay())
            }),
        );

        assert_eq!(result, Ok(Term::unit()));
    }

    #[test]
    fn unwrap_void_or_wrong_constr() {
        let result = quick_eval(
            Term::data(Data::constr(14, vec![])).as_var("__unit", |unit| {
                Term::Var { name: unit, context: () }.unwrap_void_or(|u| u, &Term::error().delay())
            }),
        );

        assert_eq!(result, Err(Error::EvaluationFailure));
    }

    #[test]
    fn unwrap_void_or_too_many_args() {
        let result = quick_eval(
            Term::data(Data::constr(0, vec![Data::integer(0.into())])).as_var("__unit", |unit| {
                Term::Var { name: unit, context: () }.unwrap_void_or(|u| u, &Term::error().delay())
            }),
        );

        assert_eq!(result, Err(Error::EvaluationFailure));
    }
}
