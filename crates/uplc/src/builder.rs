use crate::{
    ast::{Constant, Name, Term, Type},
    builtins::DefaultFunction,
};
use pallas_primitives::alonzo::PlutusData;

pub const CONSTR_FIELDS_EXPOSER: &str = "__constr_fields_exposer";
pub const CONSTR_INDEX_EXPOSER: &str = "__constr_index_exposer";
pub const EXPECT_ON_LIST: &str = "__expect_on_list";

impl<T> Term<T> {
    // Terms
    pub fn apply(self, arg: Self) -> Self {
        Term::Apply {
            function: self.into(),
            argument: arg.into(),
        }
    }

    pub fn force(self) -> Self {
        Term::Force(self.into())
    }

    pub fn delay(self) -> Self {
        Term::Delay(self.into())
    }

    // Primitives
    pub fn integer(i: num_bigint::BigInt) -> Self {
        Term::Constant(Constant::Integer(i).into())
    }

    pub fn string(s: impl ToString) -> Self {
        Term::Constant(Constant::String(s.to_string()).into())
    }

    pub fn byte_string(b: Vec<u8>) -> Self {
        Term::Constant(Constant::ByteString(b).into())
    }

    pub fn bls12_381_g1(b: blst::blst_p1) -> Self {
        Term::Constant(Constant::Bls12_381G1Element(b.into()).into())
    }

    pub fn bls12_381_g2(b: blst::blst_p2) -> Self {
        Term::Constant(Constant::Bls12_381G2Element(b.into()).into())
    }

    pub fn bool(b: bool) -> Self {
        Term::Constant(Constant::Bool(b).into())
    }

    pub fn unit() -> Self {
        Term::Constant(Constant::Unit.into())
    }

    pub fn data(d: PlutusData) -> Self {
        Term::Constant(Constant::Data(d).into())
    }

    pub fn empty_list() -> Self {
        Term::Constant(Constant::ProtoList(Type::Data, vec![]).into())
    }

    pub fn list_values(vals: Vec<Constant>) -> Self {
        Term::Constant(Constant::ProtoList(Type::Data, vals).into())
    }

    pub fn empty_map() -> Self {
        Term::Constant(
            Constant::ProtoList(Type::Pair(Type::Data.into(), Type::Data.into()), vec![]).into(),
        )
    }

    pub fn map_values(vals: Vec<Constant>) -> Self {
        Term::Constant(
            Constant::ProtoList(Type::Pair(Type::Data.into(), Type::Data.into()), vals).into(),
        )
    }

    pub fn pair_values(fst_val: Constant, snd_val: Constant) -> Self {
        Term::Constant(
            Constant::ProtoPair(Type::Data, Type::Data, fst_val.into(), snd_val.into()).into(),
        )
    }

    // This section contains builders for builtins from default functions
    // Theses are in _alphabetical order_
    // The naming convention almost follows PascalCase -> snake_case
    // Exceptions include the use of `un`.

    pub fn add_integer() -> Self {
        Term::Builtin(DefaultFunction::AddInteger)
    }

    pub fn append_bytearray() -> Self {
        Term::Builtin(DefaultFunction::AppendByteString)
    }

    pub fn append_string() -> Self {
        Term::Builtin(DefaultFunction::AppendString)
    }

    pub fn b_data() -> Self {
        Term::Builtin(DefaultFunction::BData)
    }

    pub fn blake2b_224() -> Self {
        Term::Builtin(DefaultFunction::Blake2b_224)
    }

    pub fn blake2b_256() -> Self {
        Term::Builtin(DefaultFunction::Blake2b_256)
    }

    pub fn bls12_381_g1_add() -> Self {
        Term::Builtin(DefaultFunction::Bls12_381_G1_Add)
    }
    pub fn bls12_381_g1_neg() -> Self {
        Term::Builtin(DefaultFunction::Bls12_381_G1_Neg)
    }
    pub fn bls12_381_g1_scalar_mul() -> Self {
        Term::Builtin(DefaultFunction::Bls12_381_G1_ScalarMul)
    }
    pub fn bls12_381_g1_equal() -> Self {
        Term::Builtin(DefaultFunction::Bls12_381_G1_Equal)
    }
    pub fn bls12_381_g1_compress() -> Self {
        Term::Builtin(DefaultFunction::Bls12_381_G1_Compress)
    }
    pub fn bls12_381_g1_uncompress() -> Self {
        Term::Builtin(DefaultFunction::Bls12_381_G1_Uncompress)
    }
    pub fn bls12_381_g1_hash_to_group() -> Self {
        Term::Builtin(DefaultFunction::Bls12_381_G1_HashToGroup)
    }
    pub fn bls12_381_g2_add() -> Self {
        Term::Builtin(DefaultFunction::Bls12_381_G2_Add)
    }
    pub fn bls12_381_g2_neg() -> Self {
        Term::Builtin(DefaultFunction::Bls12_381_G2_Neg)
    }
    pub fn bls12_381_g2_scalar_mul() -> Self {
        Term::Builtin(DefaultFunction::Bls12_381_G2_ScalarMul)
    }
    pub fn bls12_381_g2_equal() -> Self {
        Term::Builtin(DefaultFunction::Bls12_381_G2_Equal)
    }
    pub fn bls12_381_g2_compress() -> Self {
        Term::Builtin(DefaultFunction::Bls12_381_G2_Compress)
    }
    pub fn bls12_381_g2_uncompress() -> Self {
        Term::Builtin(DefaultFunction::Bls12_381_G2_Uncompress)
    }
    pub fn bls12_381_g2_hash_to_group() -> Self {
        Term::Builtin(DefaultFunction::Bls12_381_G2_HashToGroup)
    }
    pub fn bls12_381_miller_loop() -> Self {
        Term::Builtin(DefaultFunction::Bls12_381_MillerLoop)
    }
    pub fn bls12_381_mul_ml_result() -> Self {
        Term::Builtin(DefaultFunction::Bls12_381_MulMlResult)
    }
    pub fn bls12_381_final_verify() -> Self {
        Term::Builtin(DefaultFunction::Bls12_381_FinalVerify)
    }

    pub fn choose_data(
        self,
        constr_case: Self,
        map_case: Self,
        array_case: Self,
        int_case: Self,
        bytes_case: Self,
    ) -> Self {
        Term::Builtin(DefaultFunction::ChooseData)
            .force()
            .apply(self)
            .apply(constr_case)
            .apply(map_case)
            .apply(array_case)
            .apply(int_case)
            .apply(bytes_case)
    }

    pub fn choose_list(self, then_term: Self, else_term: Self) -> Self {
        Term::Builtin(DefaultFunction::ChooseList)
            .force()
            .force()
            .apply(self)
            .apply(then_term)
            .apply(else_term)
    }

    pub fn choose_unit(self, then_term: Self) -> Self {
        Term::Builtin(DefaultFunction::ChooseUnit)
            .force()
            .apply(self)
            .apply(then_term)
    }

    pub fn cons_bytearray() -> Self {
        Term::Builtin(DefaultFunction::ConsByteString)
    }

    pub fn constr_data() -> Self {
        Term::Builtin(DefaultFunction::ConstrData)
    }

    pub fn decode_utf8() -> Self {
        Term::Builtin(DefaultFunction::DecodeUtf8)
    }

    pub fn div_integer() -> Self {
        Term::Builtin(DefaultFunction::DivideInteger)
    }

    pub fn divide_integer() -> Self {
        Term::Builtin(DefaultFunction::DivideInteger)
    }

    pub fn encode_utf8() -> Self {
        Term::Builtin(DefaultFunction::EncodeUtf8)
    }

    pub fn equals_bytestring() -> Self {
        Term::Builtin(DefaultFunction::EqualsByteString)
    }

    pub fn equals_data() -> Self {
        Term::Builtin(DefaultFunction::EqualsData)
    }

    pub fn equals_integer() -> Self {
        Term::Builtin(DefaultFunction::EqualsInteger)
    }

    pub fn equals_string() -> Self {
        Term::Builtin(DefaultFunction::EqualsString)
    }

    pub fn fst_pair() -> Self {
        Term::Builtin(DefaultFunction::FstPair).force().force()
    }

    pub fn head_list() -> Self {
        Term::Builtin(DefaultFunction::HeadList).force()
    }

    pub fn i_data() -> Self {
        Term::Builtin(DefaultFunction::IData)
    }

    pub fn if_then_else(self, then_term: Self, else_term: Self) -> Self {
        Term::Builtin(DefaultFunction::IfThenElse)
            .force()
            .apply(self)
            .apply(then_term)
            .apply(else_term)
    }

    pub fn index_bytearray() -> Self {
        Term::Builtin(DefaultFunction::IndexByteString)
    }

    pub fn keccak_256() -> Self {
        Term::Builtin(DefaultFunction::Keccak_256)
    }

    pub fn length_of_bytearray() -> Self {
        Term::Builtin(DefaultFunction::LengthOfByteString)
    }

    pub fn less_than_bytearray() -> Self {
        Term::Builtin(DefaultFunction::LessThanByteString)
    }

    pub fn less_than_equals_bytearray() -> Self {
        Term::Builtin(DefaultFunction::LessThanEqualsByteString)
    }

    pub fn less_than_equals_integer() -> Self {
        Term::Builtin(DefaultFunction::LessThanEqualsInteger)
    }

    pub fn less_than_integer() -> Self {
        Term::Builtin(DefaultFunction::LessThanInteger)
    }

    pub fn list_data() -> Self {
        Term::Builtin(DefaultFunction::ListData)
    }

    pub fn map_data() -> Self {
        Term::Builtin(DefaultFunction::MapData)
    }

    pub fn mk_cons() -> Self {
        Term::Builtin(DefaultFunction::MkCons).force()
    }

    pub fn mk_pair_data() -> Self {
        Term::Builtin(DefaultFunction::MkPairData)
    }

    pub fn mod_integer() -> Self {
        Term::Builtin(DefaultFunction::ModInteger)
    }

    pub fn multiply_integer() -> Self {
        Term::Builtin(DefaultFunction::MultiplyInteger)
    }

    pub fn quotient_integer() -> Self {
        Term::Builtin(DefaultFunction::QuotientInteger)
    }

    pub fn remainder_integer() -> Self {
        Term::Builtin(DefaultFunction::RemainderInteger)
    }

    pub fn sha2_256() -> Self {
        Term::Builtin(DefaultFunction::Sha2_256)
    }

    pub fn sha3_256() -> Self {
        Term::Builtin(DefaultFunction::Sha3_256)
    }

    pub fn slice_bytearray() -> Self {
        Term::Builtin(DefaultFunction::SliceByteString)
    }

    pub fn snd_pair() -> Self {
        Term::Builtin(DefaultFunction::SndPair).force().force()
    }

    pub fn subtract_integer() -> Self {
        Term::Builtin(DefaultFunction::SubtractInteger)
    }

    pub fn tail_list() -> Self {
        Term::Builtin(DefaultFunction::TailList).force()
    }

    pub fn un_b_data() -> Self {
        Term::Builtin(DefaultFunction::UnBData)
    }

    pub fn un_i_data() -> Self {
        Term::Builtin(DefaultFunction::UnIData)
    }

    pub fn unconstr_data() -> Self {
        Term::Builtin(DefaultFunction::UnConstrData)
    }

    pub fn unlist_data() -> Self {
        Term::Builtin(DefaultFunction::UnListData)
    }
    pub fn unmap_data() -> Self {
        Term::Builtin(DefaultFunction::UnMapData)
    }

    pub fn verify_ecdsa_secp256k1_signature() -> Self {
        Term::Builtin(DefaultFunction::VerifyEcdsaSecp256k1Signature)
    }

    pub fn verify_ed25519_signature() -> Self {
        Term::Builtin(DefaultFunction::VerifyEd25519Signature)
    }

    pub fn verify_schnorr_secp256k1_signature() -> Self {
        Term::Builtin(DefaultFunction::VerifySchnorrSecp256k1Signature)
    }

    // Unused bultins
    pub fn mk_nil_data() -> Self {
        Term::Builtin(DefaultFunction::MkNilData)
    }
    pub fn mk_nil_pair_data() -> Self {
        Term::Builtin(DefaultFunction::MkNilPairData)
    }
    pub fn null_list() -> Self {
        Term::Builtin(DefaultFunction::NullList)
    }
    pub fn serialise_data() -> Self {
        Term::Builtin(DefaultFunction::SerialiseData)
    }
}

impl<T> Term<T> {
    pub fn delayed_choose_data(
        self,
        constr_case: Self,
        map_case: Self,
        array_case: Self,
        int_case: Self,
        bytes_case: Self,
    ) -> Self {
        Term::Builtin(DefaultFunction::ChooseData)
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
        Term::Builtin(DefaultFunction::ChooseList)
            .force()
            .force()
            .apply(self)
            .apply(then_term.delay())
            .apply(else_term.delay())
            .force()
    }

    pub fn delayed_choose_unit(self, then_term: Self) -> Self {
        Term::Builtin(DefaultFunction::ChooseUnit)
            .force()
            .apply(self)
            .apply(then_term.delay())
            .force()
    }

    pub fn delayed_if_then_else(self, then_term: Self, else_term: Self) -> Self {
        Term::Builtin(DefaultFunction::IfThenElse)
            .force()
            .apply(self)
            .apply(then_term.delay())
            .apply(else_term.delay())
            .force()
    }

    pub fn delayed_trace(self, msg_term: Self) -> Self {
        Term::Builtin(DefaultFunction::Trace)
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

impl Term<Name> {
    pub fn lambda(self, parameter_name: impl ToString) -> Self {
        Term::Lambda {
            parameter_name: Name::text(parameter_name).into(),
            body: self.into(),
        }
    }

    pub fn var(name: impl ToString) -> Self {
        Term::Var(Name::text(name).into())
    }

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
}
