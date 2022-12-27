use crate::builtins::DefaultFunction;

use super::{Constant, Name, Term};

pub const CONSTR_FIELDS_EXPOSER: &str = "__constr_fields_exposer";
pub const CONSTR_INDEX_EXPOSER: &str = "__constr_index_exposer";
pub const CONSTR_GET_FIELD: &str = "__constr_get_field";

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
                argument: Term::Delay(Term::Constant(Constant::Unit).into()).into(),
            }
            .into(),
            argument: Term::Delay(Term::Error.into()).into(),
        }
        .into(),
    )
}

pub fn constr_fields_exposer(term: Term<Name>) -> Term<Name> {
    Term::Apply {
        function: Term::Lambda {
            parameter_name: Name {
                text: CONSTR_FIELDS_EXPOSER.to_string(),
                unique: 0.into(),
            },
            body: term.into(),
        }
        .into(),
        argument: Term::Lambda {
            parameter_name: Name {
                text: "__constr_var".to_string(),
                unique: 0.into(),
            },
            body: Term::Apply {
                function: Term::Force(
                    Term::Force(Term::Builtin(DefaultFunction::SndPair).into()).into(),
                )
                .into(),
                argument: Term::Apply {
                    function: Term::Builtin(DefaultFunction::UnConstrData).into(),
                    argument: Term::Var(Name {
                        text: "__constr_var".to_string(),
                        unique: 0.into(),
                    })
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
            },
            body: term.into(),
        }
        .into(),
        argument: Term::Lambda {
            parameter_name: Name {
                text: "__constr_list".to_string(),
                unique: 0.into(),
            },
            body: Term::Lambda {
                parameter_name: Name {
                    text: "__arg_number".to_string(),
                    unique: 0.into(),
                },
                body: Term::Apply {
                    function: Term::Lambda {
                        parameter_name: Name {
                            text: "__recurse".to_string(),
                            unique: 0.into(),
                        },
                        body: Term::Apply {
                            function: Term::Apply {
                                function: Term::Apply {
                                    function: Term::Var(Name {
                                        text: "__recurse".to_string(),
                                        unique: 0.into(),
                                    })
                                    .into(),
                                    argument: Term::Var(Name {
                                        text: "__recurse".to_string(),
                                        unique: 0.into(),
                                    })
                                    .into(),
                                }
                                .into(),

                                // Start recursive with index 0 of list
                                argument: Term::Constant(Constant::Integer(0.into())).into(),
                            }
                            .into(),
                            argument: Term::Var(Name {
                                text: "__constr_list".to_string(),
                                unique: 0.into(),
                            })
                            .into(),
                        }
                        .into(),
                    }
                    .into(),

                    argument: Term::Lambda {
                        parameter_name: Name {
                            text: "__self_recursor".to_string(),
                            unique: 0.into(),
                        },
                        body: Term::Lambda {
                            parameter_name: Name {
                                text: "__current_arg_number".to_string(),
                                unique: 0.into(),
                            },
                            body: Term::Lambda {
                                parameter_name: Name {
                                    text: "__list_of_constr_args".to_string(),
                                    unique: 0.into(),
                                },
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
                                                        argument: Term::Var(Name {
                                                            text: "__arg_number".to_string(),
                                                            unique: 0.into(),
                                                        })
                                                        .into(),
                                                    }
                                                    .into(),
                                                    argument: Term::Var(Name {
                                                        text: "__current_arg_number".to_string(),
                                                        unique: 0.into(),
                                                    })
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
                                            },
                                            body: Term::Apply {
                                                function: Term::Apply {
                                                    function: Term::Apply {
                                                        function: Term::Var(Name {
                                                            text: "__self_recursor".to_string(),
                                                            unique: 0.into(),
                                                        })
                                                        .into(),
                                                        argument: Term::Var(Name {
                                                            text: "__self_recursor".to_string(),
                                                            unique: 0.into(),
                                                        })
                                                        .into(),
                                                    }
                                                    .into(),

                                                    argument: Term::Apply {
                                                        function: Term::Apply {
                                                            function: Term::Builtin(
                                                                DefaultFunction::AddInteger,
                                                            )
                                                            .into(),
                                                            argument: Term::Var(Name {
                                                                text: "__current_arg_number"
                                                                    .to_string(),
                                                                unique: 0.into(),
                                                            })
                                                            .into(),
                                                        }
                                                        .into(),
                                                        argument: Term::Constant(
                                                            Constant::Integer(1.into()),
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

                                                    argument: Term::Var(Name {
                                                        text: "__current_list_of_constr_args"
                                                            .to_string(),
                                                        unique: 0.into(),
                                                    })
                                                    .into(),
                                                }
                                                .into(),
                                            }
                                            .into(),
                                        }
                                        .into(),
                                    }
                                    .into(),
                                    argument: Term::Var(Name {
                                        text: "__list_of_constr_args".to_string(),
                                        unique: 0.into(),
                                    })
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
                function: Term::Builtin(DefaultFunction::IfThenElse)
                    .force_wrap()
                    .into(),
                argument: condition.into(),
            }
            .into(),
            argument: Term::Delay(then_term.into()).into(),
        }
        .into(),
        argument: Term::Delay(else_term.into()).into(),
    }
    .force_wrap()
}

pub fn if_else(condition: Term<Name>, then_term: Term<Name>, else_term: Term<Name>) -> Term<Name> {
    Term::Apply {
        function: Term::Apply {
            function: Term::Apply {
                function: Term::Builtin(DefaultFunction::IfThenElse)
                    .force_wrap()
                    .into(),
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
                    .force_wrap()
                    .force_wrap()
                    .into(),
                argument: list.into(),
            }
            .into(),
            argument: Term::Delay(empty_list_term.into()).into(),
        }
        .into(),
        argument: Term::Delay(else_term.into()).into(),
    }
    .force_wrap()
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
                    .force_wrap()
                    .force_wrap()
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
            function: Term::Builtin(DefaultFunction::TailList).force_wrap().into(),
            argument: term.into(),
        };
    }
    term
}
