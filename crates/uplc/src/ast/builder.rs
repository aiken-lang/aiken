use crate::builtins::DefaultFunction;

use super::{Constant, Name, Term};

pub const CONSTR_FIELDS_EXPOSER: &str = "__constr_fields_exposer";
pub const CONSTR_INDEX_EXPOSER: &str = "__constr_index_exposer";
pub const CONSTR_GET_FIELD: &str = "__constr_get_field";
pub const ASSERT_ON_LIST: &str = "__assert_on_list";

impl Term<Name> {
    pub fn apply(self, arg: Self) -> Self {
        Term::Apply {
            function: self.into(),
            argument: arg.into(),
        }
    }

    pub fn lambda(self, parameter_name: impl ToString) -> Self {
        Term::Lambda {
            parameter_name: Name::text(parameter_name).into(),
            body: self.into(),
        }
    }

    pub fn force(self) -> Self {
        Term::Force(self.into())
    }

    pub fn delay(self) -> Self {
        Term::Delay(self.into())
    }

    pub fn var(name: impl ToString) -> Self {
        Term::Var(Name::text(name).into())
    }

    pub fn integer(i: num_bigint::BigInt) -> Self {
        Term::Constant(Constant::Integer(i).into())
    }

    pub fn constr_data() -> Self {
        Term::Builtin(DefaultFunction::ConstrData)
    }

    pub fn equals_integer() -> Self {
        Term::Builtin(DefaultFunction::EqualsInteger)
    }

    pub fn head_list() -> Self {
        Term::Builtin(DefaultFunction::HeadList).force()
    }

    pub fn delayed_if_else(self, then_term: Self, else_term: Self) -> Self {
        Term::Apply {
            function: Term::Apply {
                function: Term::Apply {
                    function: Term::Builtin(DefaultFunction::IfThenElse).force().into(),
                    argument: self.into(),
                }
                .into(),
                argument: Term::Delay(then_term.into()).into(),
            }
            .into(),
            argument: Term::Delay(else_term.into()).into(),
        }
        .force()
    }
}

pub fn apply_wrap(function: Term<Name>, arg: Term<Name>) -> Term<Name> {
    Term::Apply {
        function: function.into(),
        argument: arg.into(),
    }
}

pub fn final_wrapper(term: Term<Name>) -> Term<Name> {
    Term::Force(
        Term::Apply {
            function: Term::Apply {
                function: Term::Apply {
                    function: Term::Force(Term::Builtin(DefaultFunction::IfThenElse).into()).into(),
                    argument: term.into(),
                }
                .into(),
                argument: Term::Delay(Term::Constant(Constant::Unit.into()).into()).into(),
            }
            .into(),
            argument: Term::Delay(Term::Error.into()).into(),
        }
        .into(),
    )
}

pub fn assert_on_list(term: Term<Name>) -> Term<Name> {
    apply_wrap(
        Term::Lambda {
            parameter_name: Name {
                text: ASSERT_ON_LIST.to_string(),
                unique: 0.into(),
            }
            .into(),
            body: apply_wrap(
                Term::Lambda {
                    parameter_name: Name {
                        text: ASSERT_ON_LIST.to_string(),
                        unique: 0.into(),
                    }
                    .into(),
                    body: term.into(),
                },
                apply_wrap(
                    Term::Var(
                        Name {
                            text: ASSERT_ON_LIST.to_string(),
                            unique: 0.into(),
                        }
                        .into(),
                    ),
                    Term::Var(
                        Name {
                            text: ASSERT_ON_LIST.to_string(),
                            unique: 0.into(),
                        }
                        .into(),
                    ),
                ),
            )
            .into(),
        },
        Term::Lambda {
            parameter_name: Name {
                text: ASSERT_ON_LIST.to_string(),
                unique: 0.into(),
            }
            .into(),
            body: Term::Lambda {
                parameter_name: Name {
                    text: "list_to_check".to_string(),
                    unique: 0.into(),
                }
                .into(),
                body: Term::Lambda {
                    parameter_name: Name {
                        text: "check_with".to_string(),
                        unique: 0.into(),
                    }
                    .into(),
                    body: delayed_choose_list(
                        Term::Var(
                            Name {
                                text: "list_to_check".to_string(),
                                unique: 0.into(),
                            }
                            .into(),
                        ),
                        Term::Constant(Constant::Unit.into()),
                        apply_wrap(
                            apply_wrap(
                                Term::Builtin(DefaultFunction::ChooseUnit).force(),
                                apply_wrap(
                                    Term::Var(
                                        Name {
                                            text: "check_with".to_string(),
                                            unique: 0.into(),
                                        }
                                        .into(),
                                    ),
                                    apply_wrap(
                                        Term::Builtin(DefaultFunction::HeadList).force(),
                                        Term::Var(
                                            Name {
                                                text: "list_to_check".to_string(),
                                                unique: 0.into(),
                                            }
                                            .into(),
                                        ),
                                    ),
                                ),
                            ),
                            apply_wrap(
                                apply_wrap(
                                    apply_wrap(
                                        Term::Var(
                                            Name {
                                                text: ASSERT_ON_LIST.to_string(),
                                                unique: 0.into(),
                                            }
                                            .into(),
                                        ),
                                        Term::Var(
                                            Name {
                                                text: ASSERT_ON_LIST.to_string(),
                                                unique: 0.into(),
                                            }
                                            .into(),
                                        ),
                                    ),
                                    apply_wrap(
                                        Term::Builtin(DefaultFunction::TailList).force(),
                                        Term::Var(
                                            Name {
                                                text: "list_to_check".to_string(),
                                                unique: 0.into(),
                                            }
                                            .into(),
                                        ),
                                    ),
                                ),
                                Term::Var(
                                    Name {
                                        text: "check_with".to_string(),
                                        unique: 0.into(),
                                    }
                                    .into(),
                                ),
                            ),
                        ),
                    )
                    .into(),
                }
                .into(),
            }
            .into(),
        },
    )
}

pub fn constr_fields_exposer(term: Term<Name>) -> Term<Name> {
    Term::Apply {
        function: Term::Lambda {
            parameter_name: Name {
                text: CONSTR_FIELDS_EXPOSER.to_string(),
                unique: 0.into(),
            }
            .into(),
            body: term.into(),
        }
        .into(),
        argument: Term::Lambda {
            parameter_name: Name {
                text: "__constr_var".to_string(),
                unique: 0.into(),
            }
            .into(),
            body: Term::Apply {
                function: Term::Force(
                    Term::Force(Term::Builtin(DefaultFunction::SndPair).into()).into(),
                )
                .into(),
                argument: Term::Apply {
                    function: Term::Builtin(DefaultFunction::UnConstrData).into(),
                    argument: Term::Var(
                        Name {
                            text: "__constr_var".to_string(),
                            unique: 0.into(),
                        }
                        .into(),
                    )
                    .into(),
                }
                .into(),
            }
            .into(),
        }
        .into(),
    }
}

pub fn constr_index_exposer(term: Term<Name>) -> Term<Name> {
    Term::Apply {
        function: Term::Force(Term::Force(Term::Builtin(DefaultFunction::FstPair).into()).into())
            .into(),
        argument: Term::Apply {
            function: Term::Builtin(DefaultFunction::UnConstrData).into(),
            argument: term.into(),
        }
        .into(),
    }
}

pub fn constr_get_field(term: Term<Name>) -> Term<Name> {
    Term::Apply {
        function: Term::Lambda {
            parameter_name: Name {
                text: CONSTR_GET_FIELD.to_string(),
                unique: 0.into(),
            }
            .into(),
            body: term.into(),
        }
        .into(),
        argument: Term::Lambda {
            parameter_name: Name {
                text: "__constr_list".to_string(),
                unique: 0.into(),
            }
            .into(),
            body: Term::Lambda {
                parameter_name: Name {
                    text: "__arg_number".to_string(),
                    unique: 0.into(),
                }
                .into(),
                body: Term::Apply {
                    function: Term::Lambda {
                        parameter_name: Name {
                            text: "__recurse".to_string(),
                            unique: 0.into(),
                        }
                        .into(),
                        body: Term::Apply {
                            function: Term::Apply {
                                function: Term::Apply {
                                    function: Term::Var(
                                        Name {
                                            text: "__recurse".to_string(),
                                            unique: 0.into(),
                                        }
                                        .into(),
                                    )
                                    .into(),
                                    argument: Term::Var(
                                        Name {
                                            text: "__recurse".to_string(),
                                            unique: 0.into(),
                                        }
                                        .into(),
                                    )
                                    .into(),
                                }
                                .into(),

                                // Start recursive with index 0 of list
                                argument: Term::Constant(Constant::Integer(0.into()).into()).into(),
                            }
                            .into(),
                            argument: Term::Var(
                                Name {
                                    text: "__constr_list".to_string(),
                                    unique: 0.into(),
                                }
                                .into(),
                            )
                            .into(),
                        }
                        .into(),
                    }
                    .into(),

                    argument: Term::Lambda {
                        parameter_name: Name {
                            text: "__self_recursor".to_string(),
                            unique: 0.into(),
                        }
                        .into(),
                        body: Term::Lambda {
                            parameter_name: Name {
                                text: "__current_arg_number".to_string(),
                                unique: 0.into(),
                            }
                            .into(),
                            body: Term::Lambda {
                                parameter_name: Name {
                                    text: "__list_of_constr_args".to_string(),
                                    unique: 0.into(),
                                }
                                .into(),
                                body: Term::Apply {
                                    function: Term::Apply {
                                        function: Term::Apply {
                                            function: Term::Apply {
                                                function: Term::Force(
                                                    Term::Builtin(DefaultFunction::IfThenElse)
                                                        .into(),
                                                )
                                                .into(),
                                                argument: Term::Apply {
                                                    function: Term::Apply {
                                                        function: Term::Builtin(
                                                            DefaultFunction::EqualsInteger,
                                                        )
                                                        .into(),
                                                        argument: Term::Var(
                                                            Name {
                                                                text: "__arg_number".to_string(),
                                                                unique: 0.into(),
                                                            }
                                                            .into(),
                                                        )
                                                        .into(),
                                                    }
                                                    .into(),
                                                    argument: Term::Var(
                                                        Name {
                                                            text: "__current_arg_number"
                                                                .to_string(),
                                                            unique: 0.into(),
                                                        }
                                                        .into(),
                                                    )
                                                    .into(),
                                                }
                                                .into(),
                                            }
                                            .into(),
                                            argument: Term::Force(
                                                Term::Builtin(DefaultFunction::HeadList).into(),
                                            )
                                            .into(),
                                        }
                                        .into(),
                                        argument: Term::Lambda {
                                            parameter_name: Name {
                                                text: "__current_list_of_constr_args".to_string(),
                                                unique: 0.into(),
                                            }
                                            .into(),
                                            body: Term::Apply {
                                                function: Term::Apply {
                                                    function: Term::Apply {
                                                        function: Term::Var(
                                                            Name {
                                                                text: "__self_recursor".to_string(),
                                                                unique: 0.into(),
                                                            }
                                                            .into(),
                                                        )
                                                        .into(),
                                                        argument: Term::Var(
                                                            Name {
                                                                text: "__self_recursor".to_string(),
                                                                unique: 0.into(),
                                                            }
                                                            .into(),
                                                        )
                                                        .into(),
                                                    }
                                                    .into(),

                                                    argument: Term::Apply {
                                                        function: Term::Apply {
                                                            function: Term::Builtin(
                                                                DefaultFunction::AddInteger,
                                                            )
                                                            .into(),
                                                            argument: Term::Var(
                                                                Name {
                                                                    text: "__current_arg_number"
                                                                        .to_string(),
                                                                    unique: 0.into(),
                                                                }
                                                                .into(),
                                                            )
                                                            .into(),
                                                        }
                                                        .into(),
                                                        argument: Term::Constant(
                                                            Constant::Integer(1.into()).into(),
                                                        )
                                                        .into(),
                                                    }
                                                    .into(),
                                                }
                                                .into(),

                                                argument: Term::Apply {
                                                    function: Term::Force(
                                                        Term::Builtin(DefaultFunction::TailList)
                                                            .into(),
                                                    )
                                                    .into(),

                                                    argument: Term::Var(
                                                        Name {
                                                            text: "__current_list_of_constr_args"
                                                                .to_string(),
                                                            unique: 0.into(),
                                                        }
                                                        .into(),
                                                    )
                                                    .into(),
                                                }
                                                .into(),
                                            }
                                            .into(),
                                        }
                                        .into(),
                                    }
                                    .into(),
                                    argument: Term::Var(
                                        Name {
                                            text: "__list_of_constr_args".to_string(),
                                            unique: 0.into(),
                                        }
                                        .into(),
                                    )
                                    .into(),
                                }
                                .into(),
                            }
                            .into(),
                        }
                        .into(),
                    }
                    .into(),
                }
                .into(),
            }
            .into(),
        }
        .into(),
    }
}

pub fn delayed_if_else(
    condition: Term<Name>,
    then_term: Term<Name>,
    else_term: Term<Name>,
) -> Term<Name> {
    Term::Apply {
        function: Term::Apply {
            function: Term::Apply {
                function: Term::Builtin(DefaultFunction::IfThenElse).force().into(),
                argument: condition.into(),
            }
            .into(),
            argument: Term::Delay(then_term.into()).into(),
        }
        .into(),
        argument: Term::Delay(else_term.into()).into(),
    }
    .force()
}

pub fn if_else(condition: Term<Name>, then_term: Term<Name>, else_term: Term<Name>) -> Term<Name> {
    Term::Apply {
        function: Term::Apply {
            function: Term::Apply {
                function: Term::Builtin(DefaultFunction::IfThenElse).force().into(),
                argument: condition.into(),
            }
            .into(),
            argument: then_term.into(),
        }
        .into(),
        argument: else_term.into(),
    }
}

pub fn delayed_choose_list(
    list: Term<Name>,
    empty_list_term: Term<Name>,
    else_term: Term<Name>,
) -> Term<Name> {
    Term::Apply {
        function: Term::Apply {
            function: Term::Apply {
                function: Term::Builtin(DefaultFunction::ChooseList)
                    .force()
                    .force()
                    .into(),
                argument: list.into(),
            }
            .into(),
            argument: Term::Delay(empty_list_term.into()).into(),
        }
        .into(),
        argument: Term::Delay(else_term.into()).into(),
    }
    .force()
}

pub fn choose_list(
    list: Term<Name>,
    empty_list_term: Term<Name>,
    else_term: Term<Name>,
) -> Term<Name> {
    Term::Apply {
        function: Term::Apply {
            function: Term::Apply {
                function: Term::Builtin(DefaultFunction::ChooseList)
                    .force()
                    .force()
                    .into(),
                argument: list.into(),
            }
            .into(),
            argument: empty_list_term.into(),
        }
        .into(),
        argument: else_term.into(),
    }
}

pub fn repeat_tail_list(term: Term<Name>, repeat: usize) -> Term<Name> {
    let mut term = term;

    for _ in 0..repeat {
        term = Term::Apply {
            function: Term::Builtin(DefaultFunction::TailList).force().into(),
            argument: term.into(),
        };
    }
    term
}
