use std::rc::Rc;

use indexmap::IndexMap;
use itertools::Itertools;

use crate::{
    ast::{Name, Program, Term},
    builtins::DefaultFunction,
};

#[derive(Eq, Hash, PartialEq, Clone)]
pub struct Occurrence {
    name: Rc<Name>,
    lambda_count: usize,
}

impl Program<Name> {
    pub fn lambda_reduce(self) -> Program<Name> {
        let mut term = self.term;
        lambda_reduce(&mut term);
        Program {
            version: self.version,
            term,
        }
    }

    pub fn builtin_force_reduce(self) -> Program<Name> {
        let mut term = self.term;
        let mut builtin_map = IndexMap::new();
        builtin_force_reduce(&mut term, &mut builtin_map);

        for default_func_index in builtin_map.keys().sorted().cloned() {
            let default_func: DefaultFunction = default_func_index.try_into().unwrap();

            term = term
                .lambda(format!("__{}_wrapped", default_func.aiken_name()))
                .apply(if default_func.force_count() == 1 {
                    Term::Builtin(default_func).force()
                } else {
                    Term::Builtin(default_func).force().force()
                });
        }

        Program {
            version: self.version,
            term,
        }
    }

    pub fn inline_reduce(self) -> Program<Name> {
        let mut term = self.term;
        inline_basic_reduce(&mut term);
        inline_direct_reduce(&mut term);

        Program {
            version: self.version,
            term,
        }
    }

    pub fn force_delay_reduce(self) -> Program<Name> {
        let mut term = self.term;
        force_delay_reduce(&mut term);
        Program {
            version: self.version,
            term,
        }
    }

    pub fn wrap_data_reduce(self) -> Program<Name> {
        let mut term = self.term;
        wrap_data_reduce(&mut term);
        Program {
            version: self.version,
            term,
        }
    }
}

fn builtin_force_reduce(term: &mut Term<Name>, builtin_map: &mut IndexMap<u8, ()>) {
    match term {
        Term::Force(f) => {
            let f = Rc::make_mut(f);

            match f {
                Term::Force(inner_f) => {
                    if let Term::Builtin(func) = inner_f.as_ref() {
                        builtin_map.insert(*func as u8, ());
                        *term = Term::Var(
                            Name {
                                text: format!("__{}_wrapped", func.aiken_name()),
                                unique: 0.into(),
                            }
                            .into(),
                        );
                        return;
                    }
                }
                Term::Builtin(func) => {
                    builtin_map.insert(*func as u8, ());
                    *term = Term::Var(
                        Name {
                            text: format!("__{}_wrapped", func.aiken_name()),
                            unique: 0.into(),
                        }
                        .into(),
                    );

                    return;
                }
                _ => {}
            }
            builtin_force_reduce(f, builtin_map);
        }
        Term::Delay(d) => {
            let d = Rc::make_mut(d);
            builtin_force_reduce(d, builtin_map);
        }
        Term::Lambda { body, .. } => {
            let body = Rc::make_mut(body);
            builtin_force_reduce(body, builtin_map);
        }
        Term::Apply { function, argument } => {
            let func = Rc::make_mut(function);
            builtin_force_reduce(func, builtin_map);

            let arg = Rc::make_mut(argument);
            builtin_force_reduce(arg, builtin_map);
        }
        _ => {}
    }
}

fn force_delay_reduce(term: &mut Term<Name>) {
    match term {
        Term::Force(f) => {
            let f = Rc::make_mut(f);

            if let Term::Delay(body) = f {
                *term = body.as_ref().clone();
                force_delay_reduce(term);
            } else {
                force_delay_reduce(f);
            }
        }
        Term::Delay(d) => {
            let d = Rc::make_mut(d);
            force_delay_reduce(d);
        }
        Term::Lambda { body, .. } => {
            let body = Rc::make_mut(body);
            force_delay_reduce(body);
        }
        Term::Apply { function, argument } => {
            let func = Rc::make_mut(function);
            force_delay_reduce(func);

            let arg = Rc::make_mut(argument);
            force_delay_reduce(arg);
        }
        _ => {}
    }
}

fn inline_direct_reduce(term: &mut Term<Name>) {
    match term {
        Term::Delay(d) => {
            let d = Rc::make_mut(d);
            inline_direct_reduce(d);
        }
        Term::Lambda { body, .. } => {
            let body = Rc::make_mut(body);
            inline_direct_reduce(body);
        }
        Term::Apply { function, argument } => {
            let func = Rc::make_mut(function);
            let arg = Rc::make_mut(argument);

            inline_direct_reduce(func);
            inline_direct_reduce(arg);

            let Term::Lambda { parameter_name, body } = func
            else{
                return;
            };

            let Term::Var(name) = body.as_ref()
            else {
                return;
            };

            if name.as_ref() == parameter_name.as_ref() {
                *term = arg.clone();
            }
        }
        Term::Force(f) => {
            let f = Rc::make_mut(f);
            inline_direct_reduce(f);
        }
        _ => {}
    }
}

fn inline_basic_reduce(term: &mut Term<Name>) {
    match term {
        Term::Delay(d) => {
            let d = Rc::make_mut(d);
            inline_basic_reduce(d);
        }
        Term::Lambda { body, .. } => {
            let body = Rc::make_mut(body);
            inline_basic_reduce(body);
        }
        Term::Apply { function, argument } => {
            let arg = Rc::make_mut(argument);
            inline_basic_reduce(arg);

            let func = Rc::make_mut(function);
            inline_basic_reduce(func);

            if let Term::Lambda {
                parameter_name,
                body,
            } = func
            {
                let occurrences = var_occurrences(body, parameter_name.clone());
                if occurrences == 1 {
                    if let replace_term @ (Term::Var(_)
                    | Term::Constant(_)
                    | Term::Error
                    | Term::Delay(_)
                    | Term::Lambda { .. }
                    | Term::Builtin(_)) = arg
                    {
                        *term =
                            substitute_term(body.as_ref(), parameter_name.clone(), replace_term);
                    }
                }
            }
        }
        Term::Force(f) => {
            let f = Rc::make_mut(f);
            inline_basic_reduce(f);
        }
        _ => {}
    }
}

fn wrap_data_reduce(term: &mut Term<Name>) {
    match term {
        Term::Delay(d) => {
            wrap_data_reduce(Rc::make_mut(d));
        }
        Term::Lambda { body, .. } => {
            wrap_data_reduce(Rc::make_mut(body));
        }
        Term::Apply { function, argument } => {
            let Term::Builtin(
                first_action
            ) = function.as_ref()
            else {
                wrap_data_reduce(Rc::make_mut(function));
                wrap_data_reduce(Rc::make_mut(argument));
                return;
            };

            let Term::Apply { function: inner_func, argument: inner_arg } = Rc::make_mut(argument)
            else {
                wrap_data_reduce(Rc::make_mut(argument));
                return;
            };

            let Term::Builtin(second_action) = inner_func.as_ref()
            else {
                wrap_data_reduce(Rc::make_mut(argument));
                return;
            };

            match (first_action, second_action) {
                (DefaultFunction::UnIData, DefaultFunction::IData)
                | (DefaultFunction::IData, DefaultFunction::UnIData)
                | (DefaultFunction::BData, DefaultFunction::UnBData)
                | (DefaultFunction::UnBData, DefaultFunction::BData)
                | (DefaultFunction::ListData, DefaultFunction::UnListData)
                | (DefaultFunction::UnListData, DefaultFunction::ListData)
                | (DefaultFunction::MapData, DefaultFunction::UnMapData)
                | (DefaultFunction::UnMapData, DefaultFunction::MapData)
                | (DefaultFunction::UnConstrData, DefaultFunction::ConstrData)
                | (DefaultFunction::ConstrData, DefaultFunction::UnConstrData) => {
                    wrap_data_reduce(Rc::make_mut(inner_arg));
                    *term = inner_arg.as_ref().clone();
                }
                _ => {
                    wrap_data_reduce(Rc::make_mut(argument));
                }
            }
        }
        Term::Force(f) => {
            wrap_data_reduce(Rc::make_mut(f));
        }
        _ => {}
    }
}

fn var_occurrences(term: &Term<Name>, search_for: Rc<Name>) -> usize {
    match term {
        Term::Var(name) => {
            if name.as_ref() == search_for.as_ref() {
                1
            } else {
                0
            }
        }
        Term::Delay(body) => var_occurrences(body.as_ref(), search_for),
        Term::Lambda {
            parameter_name,
            body,
        } => {
            if parameter_name.clone() != search_for {
                var_occurrences(body.as_ref(), search_for)
            } else {
                0
            }
        }
        Term::Apply { function, argument } => {
            var_occurrences(function.as_ref(), search_for.clone())
                + var_occurrences(argument.as_ref(), search_for)
        }
        Term::Force(x) => var_occurrences(x.as_ref(), search_for),
        _ => 0,
    }
}

fn lambda_reduce(term: &mut Term<Name>) {
    match term {
        Term::Apply { function, argument } => {
            let func = Rc::make_mut(function);
            lambda_reduce(func);

            let arg = Rc::make_mut(argument);
            lambda_reduce(arg);

            if let Term::Lambda {
                parameter_name,
                body,
            } = func
            {
                if let replace_term @ (Term::Var(_) | Term::Constant(_) | Term::Builtin(_)) = arg {
                    let body = Rc::make_mut(body);
                    *term = substitute_term(body, parameter_name.clone(), replace_term);
                }
            }
        }
        Term::Delay(d) => {
            let d = Rc::make_mut(d);
            lambda_reduce(d);
        }
        Term::Lambda { body, .. } => {
            let body = Rc::make_mut(body);
            lambda_reduce(body);
        }
        Term::Force(f) => {
            let f = Rc::make_mut(f);
            lambda_reduce(f);
        }
        _ => {}
    }
}

fn substitute_term(term: &Term<Name>, original: Rc<Name>, replace_with: &Term<Name>) -> Term<Name> {
    match term {
        Term::Var(name) => {
            if name.as_ref() == original.as_ref() {
                replace_with.clone()
            } else {
                Term::Var(name.clone())
            }
        }
        Term::Delay(body) => {
            Term::Delay(substitute_term(body.as_ref(), original, replace_with).into())
        }
        Term::Lambda {
            parameter_name,
            body,
        } => {
            if parameter_name.as_ref() != original.as_ref() {
                Term::Lambda {
                    parameter_name: parameter_name.clone(),
                    body: Rc::new(substitute_term(body.as_ref(), original, replace_with)),
                }
            } else {
                Term::Lambda {
                    parameter_name: parameter_name.clone(),
                    body: body.clone(),
                }
            }
        }
        Term::Apply { function, argument } => Term::Apply {
            function: Rc::new(substitute_term(
                function.as_ref(),
                original.clone(),
                replace_with,
            )),
            argument: Rc::new(substitute_term(argument.as_ref(), original, replace_with)),
        },
        Term::Force(x) => Term::Force(Rc::new(substitute_term(x.as_ref(), original, replace_with))),
        x => x.clone(),
    }
}

#[cfg(test)]
mod test {

    use pallas_primitives::babbage::{BigInt, PlutusData};

    use crate::ast::{Constant, Name, NamedDeBruijn, Program, Term};

    #[test]
    fn lambda_reduce_var() {
        let program: Program<NamedDeBruijn> = Program {
            version: (1, 0, 0),
            term: Term::var("bar")
                .lambda("bar")
                .apply(Term::var("foo"))
                .lambda("foo")
                .apply(
                    Term::constr_data()
                        .apply(Term::integer(3.into()))
                        .apply(Term::list_values(vec![])),
                ),
        }
        .try_into()
        .unwrap();

        let program: Program<Name> = program.try_into().unwrap();

        let expected = Program {
            version: (1, 0, 0),
            term: Term::var("foo").lambda("foo").apply(
                Term::constr_data()
                    .apply(Term::integer(3.into()))
                    .apply(Term::list_values(vec![])),
            ),
        };
        let expected: Program<NamedDeBruijn> = expected.try_into().unwrap();
        let actual = program.lambda_reduce();

        let actual: Program<NamedDeBruijn> = actual.try_into().unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn lambda_reduce_constant() {
        let program: Program<NamedDeBruijn> = Program {
            version: (1, 0, 0),
            term: Term::var("foo")
                .lambda("foo")
                .apply(Term::integer(6.into())),
        }
        .try_into()
        .unwrap();

        let program: Program<Name> = program.try_into().unwrap();

        let expected: Program<Name> = Program {
            version: (1, 0, 0),
            term: Term::integer(6.into()),
        };
        let expected: Program<NamedDeBruijn> = expected.try_into().unwrap();
        let actual = program.lambda_reduce();

        let actual: Program<NamedDeBruijn> = actual.try_into().unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn lambda_reduce_builtin() {
        let program: Program<NamedDeBruijn> = Program {
            version: (1, 0, 0),
            term: Term::var("foo").lambda("foo").apply(Term::add_integer()),
        }
        .try_into()
        .unwrap();

        let program: Program<Name> = program.try_into().unwrap();

        let expected: Program<Name> = Program {
            version: (1, 0, 0),
            term: Term::add_integer(),
        };
        let expected: Program<NamedDeBruijn> = expected.try_into().unwrap();
        let actual = program.lambda_reduce();

        let actual: Program<NamedDeBruijn> = actual.try_into().unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn lambda_reduce_force_delay_error_lam() {
        let program: Program<NamedDeBruijn> = Program {
            version: (1, 0, 0),
            term: Term::var("foo")
                .apply(Term::var("bar"))
                .apply(Term::var("baz"))
                .apply(Term::var("bat"))
                .lambda("foo")
                .apply(Term::snd_pair())
                .lambda("bar")
                .apply(Term::integer(1.into()).delay())
                .lambda("baz")
                .apply(Term::Error)
                .lambda("bat")
                .apply(Term::bool(false).lambda("x")),
        }
        .try_into()
        .unwrap();

        let program: Program<Name> = program.try_into().unwrap();

        let expected = Program {
            version: (1, 0, 0),
            term: Term::var("foo")
                .apply(Term::var("bar"))
                .apply(Term::var("baz"))
                .apply(Term::var("bat"))
                .lambda("foo")
                .apply(Term::snd_pair())
                .lambda("bar")
                .apply(Term::integer(1.into()).delay())
                .lambda("baz")
                .apply(Term::Error)
                .lambda("bat")
                .apply(Term::bool(false).lambda("x")),
        };

        let expected: Program<NamedDeBruijn> = expected.try_into().unwrap();
        let actual = program.lambda_reduce();

        let actual: Program<NamedDeBruijn> = actual.try_into().unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn wrap_data_reduce_i_data() {
        let program: Program<NamedDeBruijn> = Program {
            version: (1, 0, 0),
            term: Term::equals_data()
                .apply(Term::i_data().apply(Term::un_i_data().apply(Term::Constant(
                    Constant::Data(PlutusData::BigInt(BigInt::Int(5.into()))).into(),
                ))))
                .apply(Term::i_data().apply(Term::integer(1.into())))
                .lambda("x"),
        }
        .try_into()
        .unwrap();

        let program: Program<Name> = program.try_into().unwrap();

        let expected = Program {
            version: (1, 0, 0),
            term: Term::equals_data()
                .apply(Term::Constant(
                    Constant::Data(PlutusData::BigInt(BigInt::Int(5.into()))).into(),
                ))
                .apply(Term::i_data().apply(Term::integer(1.into())))
                .lambda("x"),
        };

        let expected: Program<NamedDeBruijn> = expected.try_into().unwrap();
        let actual = program.wrap_data_reduce();

        let actual: Program<NamedDeBruijn> = actual.try_into().unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn wrap_data_reduce_un_i_data() {
        let program: Program<NamedDeBruijn> = Program {
            version: (1, 0, 0),
            term: Term::equals_integer()
                .apply(Term::un_i_data().apply(Term::i_data().apply(Term::integer(1.into()))))
                .apply(Term::un_i_data().apply(Term::Constant(
                    Constant::Data(PlutusData::BigInt(BigInt::Int(5.into()))).into(),
                )))
                .lambda("x"),
        }
        .try_into()
        .unwrap();

        let program: Program<Name> = program.try_into().unwrap();

        let expected = Program {
            version: (1, 0, 0),
            term: Term::equals_integer()
                .apply(Term::integer(1.into()))
                .apply(Term::un_i_data().apply(Term::Constant(
                    Constant::Data(PlutusData::BigInt(BigInt::Int(5.into()))).into(),
                )))
                .lambda("x"),
        };

        let expected: Program<NamedDeBruijn> = expected.try_into().unwrap();
        let actual = program.wrap_data_reduce();

        let actual: Program<NamedDeBruijn> = actual.try_into().unwrap();

        assert_eq!(actual, expected);
    }
}
