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

    // Builtins
    pub fn constr_data() -> Self {
        Term::Builtin(DefaultFunction::ConstrData)
    }

    pub fn map_data() -> Self {
        Term::Builtin(DefaultFunction::MapData)
    }

    pub fn list_data() -> Self {
        Term::Builtin(DefaultFunction::ListData)
    }

    pub fn b_data() -> Self {
        Term::Builtin(DefaultFunction::BData)
    }

    pub fn i_data() -> Self {
        Term::Builtin(DefaultFunction::IData)
    }

    pub fn unconstr_data() -> Self {
        Term::Builtin(DefaultFunction::UnConstrData)
    }

    pub fn un_i_data() -> Self {
        Term::Builtin(DefaultFunction::UnIData)
    }

    pub fn un_b_data() -> Self {
        Term::Builtin(DefaultFunction::UnBData)
    }

    pub fn unmap_data() -> Self {
        Term::Builtin(DefaultFunction::UnMapData)
    }

    pub fn unlist_data() -> Self {
        Term::Builtin(DefaultFunction::UnListData)
    }

    pub fn equals_integer() -> Self {
        Term::Builtin(DefaultFunction::EqualsInteger)
    }

    pub fn less_than_integer() -> Self {
        Term::Builtin(DefaultFunction::LessThanInteger)
    }

    pub fn less_than_equals_integer() -> Self {
        Term::Builtin(DefaultFunction::LessThanEqualsInteger)
    }

    pub fn equals_string() -> Self {
        Term::Builtin(DefaultFunction::EqualsString)
    }

    pub fn equals_bytestring() -> Self {
        Term::Builtin(DefaultFunction::EqualsByteString)
    }

    pub fn less_than_bytearray() -> Self {
        Term::Builtin(DefaultFunction::LessThanByteString)
    }

    pub fn less_than_equals_bytearray() -> Self {
        Term::Builtin(DefaultFunction::LessThanEqualsByteString)
    }

    pub fn equals_data() -> Self {
        Term::Builtin(DefaultFunction::EqualsData)
    }

    pub fn add_integer() -> Self {
        Term::Builtin(DefaultFunction::AddInteger)
    }

    pub fn sub_integer() -> Self {
        Term::Builtin(DefaultFunction::SubtractInteger)
    }

    pub fn div_integer() -> Self {
        Term::Builtin(DefaultFunction::DivideInteger)
    }

    pub fn mod_integer() -> Self {
        Term::Builtin(DefaultFunction::ModInteger)
    }

    pub fn length_of_bytearray() -> Self {
        Term::Builtin(DefaultFunction::LengthOfByteString)
    }

    pub fn cons_bytearray() -> Self {
        Term::Builtin(DefaultFunction::ConsByteString)
    }

    pub fn slice_bytearray() -> Self {
        Term::Builtin(DefaultFunction::SliceByteString)
    }

    pub fn append_bytearray() -> Self {
        Term::Builtin(DefaultFunction::AppendByteString)
    }

    pub fn index_bytearray() -> Self {
        Term::Builtin(DefaultFunction::IndexByteString)
    }

    pub fn sha2_256() -> Self {
        Term::Builtin(DefaultFunction::Sha2_256)
    }

    pub fn sha3_256() -> Self {
        Term::Builtin(DefaultFunction::Sha3_256)
    }

    pub fn blake2b_256() -> Self {
        Term::Builtin(DefaultFunction::Blake2b_256)
    }

    pub fn verify_ed25519_signature() -> Self {
        Term::Builtin(DefaultFunction::VerifyEd25519Signature)
    }

    pub fn verify_ecdsa_secp256k1_signature() -> Self {
        Term::Builtin(DefaultFunction::VerifyEcdsaSecp256k1Signature)
    }

    pub fn verify_schnorr_secp256k1_signature() -> Self {
        Term::Builtin(DefaultFunction::VerifySchnorrSecp256k1Signature)
    }

    pub fn decode_utf8() -> Self {
        Term::Builtin(DefaultFunction::DecodeUtf8)
    }

    pub fn append_string() -> Self {
        Term::Builtin(DefaultFunction::AppendString)
    }

    pub fn encode_utf8() -> Self {
        Term::Builtin(DefaultFunction::EncodeUtf8)
    }

    pub fn head_list() -> Self {
        Term::Builtin(DefaultFunction::HeadList).force()
    }

    pub fn tail_list() -> Self {
        Term::Builtin(DefaultFunction::TailList).force()
    }

    pub fn mk_cons() -> Self {
        Term::Builtin(DefaultFunction::MkCons).force()
    }

    pub fn fst_pair() -> Self {
        Term::Builtin(DefaultFunction::FstPair).force().force()
    }

    pub fn snd_pair() -> Self {
        Term::Builtin(DefaultFunction::SndPair).force().force()
    }

    pub fn mk_pair_data() -> Self {
        Term::Builtin(DefaultFunction::MkPairData)
    }

    pub fn if_else(self, then_term: Self, else_term: Self) -> Self {
        Term::Builtin(DefaultFunction::IfThenElse)
            .force()
            .apply(self)
            .apply(then_term)
            .apply(else_term)
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

    pub fn delayed_choose_unit(self, then_term: Self) -> Self {
        Term::Builtin(DefaultFunction::ChooseUnit)
            .force()
            .apply(self)
            .apply(then_term.delay())
            .force()
    }

    pub fn delayed_if_else(self, then_term: Self, else_term: Self) -> Self {
        Term::Builtin(DefaultFunction::IfThenElse)
            .force()
            .apply(self)
            .apply(then_term.delay())
            .apply(else_term.delay())
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

    pub fn trace(self, msg_term: Self) -> Self {
        Term::Builtin(DefaultFunction::Trace)
            .force()
            .apply(msg_term)
            .apply(self.delay())
            .force()
    }

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
