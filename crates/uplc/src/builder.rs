use crate::{
    ast::{Constant, Name, Term, Type},
    builtins::DefaultFunction,
};
use pallas_primitives::alonzo::PlutusData;

pub const CONSTR_FIELDS_EXPOSER: &str = "__constr_fields_exposer";
pub const CONSTR_INDEX_EXPOSER: &str = "__constr_index_exposer";
pub const CONSTR_GET_FIELD: &str = "__constr_get_field";
pub const EXPECT_ON_LIST: &str = "__expect_on_list";

impl<T> Term<T> {
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

    pub fn equals_data() -> Self {
        Term::Builtin(DefaultFunction::EqualsData)
    }

    pub fn add_integer() -> Self {
        Term::Builtin(DefaultFunction::AddInteger)
    }

    pub fn sub_integer() -> Self {
        Term::Builtin(DefaultFunction::SubtractInteger)
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
        self.lambda(CONSTR_FIELDS_EXPOSER.to_string()).apply(
            Term::snd_pair()
                .apply(Term::unconstr_data().apply(Term::var("__constr_var".to_string())))
                .lambda("__constr_var"),
        )
    }

    pub fn constr_index_exposer(self) -> Self {
        self.lambda(CONSTR_INDEX_EXPOSER.to_string()).apply(
            Term::fst_pair()
                .apply(Term::unconstr_data().apply(Term::var("__constr_var".to_string())))
                .lambda("__constr_var".to_string()),
        )
    }

    pub fn constr_get_field(self) -> Self {
        self.lambda(CONSTR_GET_FIELD.to_string())
            .apply(
                Term::var(CONSTR_GET_FIELD.to_string())
                    .apply(Term::var(CONSTR_GET_FIELD.to_string()))
                    .apply(Term::integer(0.into())),
            )
            .lambda(CONSTR_GET_FIELD)
            .apply(
                Term::equals_integer()
                    .apply(Term::var("__wanted_arg".to_string()))
                    .apply(Term::var("__current_arg_number".to_string()))
                    .if_else(
                        Term::head_list(),
                        Term::var(CONSTR_GET_FIELD)
                            .apply(Term::var(CONSTR_GET_FIELD))
                            .apply(
                                Term::add_integer()
                                    .apply(Term::var("__current_arg_number".to_string()))
                                    .apply(Term::integer(1.into())),
                            )
                            .apply(
                                Term::tail_list()
                                    .apply(Term::var("__current_list_of_constr_args".to_string())),
                            )
                            .apply(Term::var("__wanted_arg"))
                            .lambda("__current_list_of_constr_args".to_string()),
                    )
                    .apply(Term::var("__list_of_constr_args".to_string()))
                    .lambda("__wanted_arg".to_string())
                    .lambda("__list_of_constr_args")
                    .lambda("__current_arg_number".to_string())
                    .lambda(CONSTR_GET_FIELD.to_string()),
            )
    }

    pub fn assert_on_list(self) -> Self {
        self.lambda(EXPECT_ON_LIST.to_string())
            .apply(
                Term::var(EXPECT_ON_LIST.to_string()).apply(Term::var(EXPECT_ON_LIST.to_string())),
            )
            .lambda(EXPECT_ON_LIST.to_string())
            .apply(
                Term::var("__list_to_check".to_string())
                    .delayed_choose_list(
                        Term::unit(),
                        Term::var("__check_with".to_string())
                            .apply(
                                Term::head_list().apply(Term::var("__list_to_check".to_string())),
                            )
                            .choose_unit(
                                Term::var(EXPECT_ON_LIST.to_string())
                                    .apply(Term::var(EXPECT_ON_LIST.to_string()))
                                    .apply(
                                        Term::tail_list()
                                            .apply(Term::var("__list_to_check".to_string())),
                                    )
                                    .apply(Term::var("__check_with".to_string())),
                            ),
                    )
                    .lambda("__check_with".to_string())
                    .lambda("__list_to_check".to_string())
                    .lambda(EXPECT_ON_LIST),
            )
    }
}
