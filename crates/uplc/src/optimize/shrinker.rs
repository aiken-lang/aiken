use std::{rc::Rc, vec};

use indexmap::IndexMap;
use itertools::Itertools;
use pallas_primitives::babbage::{BigInt, PlutusData};

use crate::{
    ast::{Constant, Data, Name, Program, Term, Type},
    builtins::DefaultFunction,
};

#[derive(Eq, Hash, PartialEq, Clone)]
pub struct Occurrence {
    name: Rc<Name>,
    lambda_count: usize,
}

pub struct IdGen {
    id: u64,
}

impl IdGen {
    pub fn new() -> Self {
        Self { id: 0 }
    }

    pub fn next_id(&mut self) -> u64 {
        self.id += 1;
        self.id
    }
}

impl Default for IdGen {
    fn default() -> Self {
        Self::new()
    }
}

impl Program<Name> {
    pub fn lambda_reducer(self) -> Program<Name> {
        let mut term = self.term;
        lambda_reducer(&mut term, &mut vec![], &mut vec![], &mut IdGen::new());
        Program {
            version: self.version,
            term,
        }
    }

    pub fn builtin_force_reducer(self) -> Program<Name> {
        let mut term = self.term;
        let mut builtin_map = IndexMap::new();
        builtin_force_reducer(&mut term, &mut builtin_map);

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

    pub fn inline_reducer(self) -> Program<Name> {
        let mut term = self.term;
        inline_single_occurrence_reducer(&mut term, &mut vec![], &mut vec![], &mut IdGen::new());
        inline_direct_reducer(&mut term);
        inline_identity_reducer(&mut term);

        Program {
            version: self.version,
            term,
        }
    }

    pub fn force_delay_reducer(self) -> Program<Name> {
        let mut term = self.term;
        force_delay_reducer(&mut term);
        Program {
            version: self.version,
            term,
        }
    }

    pub fn cast_data_reducer(self) -> Program<Name> {
        let mut term = self.term;
        cast_data_reducer(&mut term);
        Program {
            version: self.version,
            term,
        }
    }
}

fn builtin_force_reducer(term: &mut Term<Name>, builtin_map: &mut IndexMap<u8, ()>) {
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
            builtin_force_reducer(f, builtin_map);
        }
        Term::Delay(d) => {
            let d = Rc::make_mut(d);
            builtin_force_reducer(d, builtin_map);
        }
        Term::Lambda { body, .. } => {
            let body = Rc::make_mut(body);
            builtin_force_reducer(body, builtin_map);
        }
        Term::Apply { function, argument } => {
            let func = Rc::make_mut(function);
            builtin_force_reducer(func, builtin_map);

            let arg = Rc::make_mut(argument);
            builtin_force_reducer(arg, builtin_map);
        }
        Term::Case { .. } => todo!(),
        Term::Constr { .. } => todo!(),
        _ => {}
    }
}

fn force_delay_reducer(term: &mut Term<Name>) {
    match term {
        Term::Force(f) => {
            let f = Rc::make_mut(f);

            if let Term::Delay(body) = f {
                *term = body.as_ref().clone();
                force_delay_reducer(term);
            } else {
                force_delay_reducer(f);
            }
        }
        Term::Delay(d) => {
            let d = Rc::make_mut(d);
            force_delay_reducer(d);
        }
        Term::Lambda { body, .. } => {
            let body = Rc::make_mut(body);
            force_delay_reducer(body);
        }
        Term::Apply { function, argument } => {
            let func = Rc::make_mut(function);
            force_delay_reducer(func);

            let arg = Rc::make_mut(argument);
            force_delay_reducer(arg);
        }
        Term::Case { .. } => todo!(),
        Term::Constr { .. } => todo!(),
        _ => {}
    }
}

fn lambda_reducer(
    term: &mut Term<Name>,
    reduce_stack: &mut Vec<Option<(Term<Name>, u64)>>,
    lambda_applied_ids: &mut Vec<u64>,
    id_gen: &mut IdGen,
) {
    match term {
        // TODO: change this to handle any amount of consecutive applies and lambdas
        Term::Apply { function, argument } => {
            let arg = Rc::make_mut(argument);
            // SO args can't have existing stack args applied to them anyway, so we pass in empty list
            let mut arg_stack = vec![];
            lambda_reducer(arg, &mut arg_stack, lambda_applied_ids, id_gen);

            let next_id = id_gen.next_id();

            let arg_applied = match arg {
                Term::Constant(c) if matches!(c.as_ref(), Constant::String(_)) => None,
                Term::Constant(_) | Term::Var(_) | Term::Builtin(_) => Some((arg.clone(), next_id)),
                _ => None,
            };

            reduce_stack.push(arg_applied);

            let func = Rc::make_mut(function);
            lambda_reducer(func, reduce_stack, lambda_applied_ids, id_gen);
            // The reason we don't need to pop is because the Lambda case and will pop for us
            // It is guaranteed to pop otherwise the script is not valid anyways
            if lambda_applied_ids.contains(&next_id) {
                // we inlined the arg so now remove the apply and arg from the program
                *term = func.clone();
            }
        }
        Term::Delay(d) => {
            let d = Rc::make_mut(d);
            lambda_reducer(d, reduce_stack, lambda_applied_ids, id_gen);
        }
        Term::Lambda {
            parameter_name,
            body,
        } => {
            // pops stack here no matter what
            // match on only Some(Some(arg)) because we don't want to inline None
            if let Some(Some((arg_term, arg_id))) = reduce_stack.pop() {
                let body = Rc::make_mut(body);
                *body = substitute_term(body, parameter_name.clone(), &arg_term);
                lambda_reducer(body, reduce_stack, lambda_applied_ids, id_gen);
                lambda_applied_ids.push(arg_id);
                *term = body.clone();
            } else {
                let body = Rc::make_mut(body);
                lambda_reducer(body, reduce_stack, lambda_applied_ids, id_gen);
            }
        }
        Term::Force(f) => {
            let f = Rc::make_mut(f);
            lambda_reducer(f, reduce_stack, lambda_applied_ids, id_gen);
        }
        Term::Case { .. } => todo!(),
        Term::Constr { .. } => todo!(),
        _ => {}
    }
}

fn inline_direct_reducer(term: &mut Term<Name>) {
    match term {
        Term::Delay(d) => {
            let d = Rc::make_mut(d);
            inline_direct_reducer(d);
        }
        Term::Lambda { body, .. } => {
            let body = Rc::make_mut(body);
            inline_direct_reducer(body);
        }

        Term::Apply { function, argument } => {
            let func = Rc::make_mut(function);
            let arg = Rc::make_mut(argument);

            inline_direct_reducer(func);
            inline_direct_reducer(arg);

            let Term::Lambda {
                parameter_name,
                body,
            } = func
            else {
                return;
            };

            let Term::Var(name) = body.as_ref() else {
                return;
            };

            if name.as_ref() == parameter_name.as_ref() {
                *term = arg.clone();
            }
        }
        Term::Force(f) => {
            let f = Rc::make_mut(f);
            inline_direct_reducer(f);
        }
        Term::Case { .. } => todo!(),
        Term::Constr { .. } => todo!(),
        _ => {}
    }
}

fn inline_identity_reducer(term: &mut Term<Name>) {
    match term {
        Term::Delay(d) => {
            let d = Rc::make_mut(d);
            inline_identity_reducer(d);
        }
        Term::Lambda { body, .. } => {
            let body = Rc::make_mut(body);
            inline_identity_reducer(body);
        }
        Term::Apply { function, argument } => {
            let func = Rc::make_mut(function);
            let arg = Rc::make_mut(argument);

            inline_identity_reducer(func);
            inline_identity_reducer(arg);

            let Term::Lambda {
                parameter_name,
                body,
            } = func
            else {
                return;
            };

            let Term::Lambda {
                parameter_name: identity_name,
                body: identity_body,
            } = arg
            else {
                return;
            };

            let Term::Var(identity_var) = Rc::make_mut(identity_body) else {
                return;
            };

            if identity_var.as_ref() == identity_name.as_ref() {
                let temp_term = replace_identity_usage(body, parameter_name.clone());
                if var_occurrences(body, parameter_name.clone()) > 0 {
                    let body = Rc::make_mut(body);
                    *body = temp_term;
                } else {
                    *term = temp_term;
                }
            }
        }
        Term::Force(f) => {
            let f = Rc::make_mut(f);
            inline_identity_reducer(f);
        }
        Term::Case { .. } => todo!(),
        Term::Constr { .. } => todo!(),
        _ => {}
    }
}

fn inline_single_occurrence_reducer(
    term: &mut Term<Name>,
    reduce_stack: &mut Vec<(Term<Name>, u64)>,
    lambda_applied_ids: &mut Vec<u64>,
    id_gen: &mut IdGen,
) {
    match term {
        // TODO: change this to handle any amount of consecutive applies and lambdas
        Term::Apply { function, argument } => {
            let arg = Rc::make_mut(argument);
            // SO args can't have existing stack args applied to them anyway, so we pass in empty list
            let mut arg_stack = vec![];
            inline_single_occurrence_reducer(arg, &mut arg_stack, lambda_applied_ids, id_gen);

            let next_id = id_gen.next_id();

            let arg_applied = (arg.clone(), next_id);

            reduce_stack.push(arg_applied);

            let func = Rc::make_mut(function);
            inline_single_occurrence_reducer(func, reduce_stack, lambda_applied_ids, id_gen);
            // The reason we don't need to pop is because the Lambda case and will pop for us
            // It is guaranteed to pop otherwise the script is not valid anyways
            if lambda_applied_ids.contains(&next_id) {
                // we inlined the arg so now remove the apply and arg from the program
                *term = func.clone();
            }
        }
        Term::Delay(d) => {
            let d = Rc::make_mut(d);
            inline_single_occurrence_reducer(d, reduce_stack, lambda_applied_ids, id_gen);
        }
        Term::Lambda {
            parameter_name,
            body,
        } => {
            // pops stack here no matter what
            // match on only Some(Some(arg)) because we don't want to inline None

            if let Some((arg_term, arg_id)) = reduce_stack.pop() {
                let body = Rc::make_mut(body);
                inline_single_occurrence_reducer(body, reduce_stack, lambda_applied_ids, id_gen);
                let occurrences = var_occurrences(body, parameter_name.clone());

                let delays = delayed_execution(body);

                if occurrences == 1 {
                    if delays == 0
                        || matches!(
                            &arg_term,
                            Term::Var(_)
                                | Term::Constant(_)
                                | Term::Delay(_)
                                | Term::Lambda { .. }
                                | Term::Builtin(_),
                        )
                    {
                        *body = substitute_term(body, parameter_name.clone(), &arg_term);

                        lambda_applied_ids.push(arg_id);
                        *term = body.clone();
                    }
                // This will strip out unused terms that can't throw an error by themselves
                } else if occurrences == 0 {
                    if let Term::Var(_)
                    | Term::Constant(_)
                    | Term::Delay(_)
                    | Term::Lambda { .. }
                    | Term::Builtin(_) = &arg_term
                    {
                        lambda_applied_ids.push(arg_id);
                        *term = body.clone();
                    }
                }
            } else {
                let body = Rc::make_mut(body);
                inline_single_occurrence_reducer(body, reduce_stack, lambda_applied_ids, id_gen);
            }
        }
        Term::Force(f) => {
            let f = Rc::make_mut(f);
            inline_single_occurrence_reducer(f, reduce_stack, lambda_applied_ids, id_gen);
        }
        Term::Case { .. } => todo!(),
        Term::Constr { .. } => todo!(),
        _ => {}
    }
}

fn cast_data_reducer(term: &mut Term<Name>) {
    match term {
        Term::Delay(d) => {
            cast_data_reducer(Rc::make_mut(d));
        }
        Term::Lambda { body, .. } => {
            cast_data_reducer(Rc::make_mut(body));
        }
        Term::Apply { function, argument } => {
            let Term::Builtin(first_action) = function.as_ref() else {
                cast_data_reducer(Rc::make_mut(function));
                cast_data_reducer(Rc::make_mut(argument));
                return;
            };

            if let Term::Apply {
                function: inner_func,
                argument: inner_arg,
            } = Rc::make_mut(argument)
            {
                let Term::Builtin(second_action) = inner_func.as_ref() else {
                    cast_data_reducer(Rc::make_mut(argument));
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
                    | (DefaultFunction::UnMapData, DefaultFunction::MapData) => {
                        cast_data_reducer(Rc::make_mut(inner_arg));
                        *term = inner_arg.as_ref().clone();
                    }
                    _ => {
                        cast_data_reducer(Rc::make_mut(argument));
                    }
                }
            } else if let Term::Constant(c) = Rc::make_mut(argument) {
                match (first_action, Rc::make_mut(c)) {
                    (
                        DefaultFunction::UnIData,
                        Constant::Data(PlutusData::BigInt(BigInt::Int(i))),
                    ) => {
                        *term = Term::integer(i128::from(*i).into());
                    }
                    (DefaultFunction::IData, Constant::Integer(i)) => {
                        *term = Term::data(Data::integer(i.clone()));
                    }
                    (DefaultFunction::UnBData, Constant::Data(PlutusData::BoundedBytes(b))) => {
                        *term = Term::byte_string(b.clone().into());
                    }
                    (DefaultFunction::BData, Constant::ByteString(b)) => {
                        *term = Term::data(Data::bytestring(b.clone()));
                    }
                    (DefaultFunction::UnListData, Constant::Data(PlutusData::Array(l))) => {
                        *term = Term::list_values(
                            l.iter()
                                .map(|item| Constant::Data(item.clone()))
                                .collect_vec(),
                        );
                    }
                    (DefaultFunction::ListData, Constant::ProtoList(_, l)) => {
                        *term = Term::data(Data::list(
                            l.iter()
                                .map(|item| match item {
                                    Constant::Data(d) => d.clone(),
                                    _ => unreachable!(),
                                })
                                .collect_vec(),
                        ));
                    }
                    (DefaultFunction::MapData, Constant::ProtoList(_, m)) => {
                        *term = Term::data(Data::map(
                            m.iter()
                                .map(|m| match m {
                                    Constant::ProtoPair(_, _, f, s) => {
                                        match (f.as_ref(), s.as_ref()) {
                                            (Constant::Data(d), Constant::Data(d2)) => {
                                                (d.clone(), d2.clone())
                                            }
                                            _ => unreachable!(),
                                        }
                                    }
                                    _ => unreachable!(),
                                })
                                .collect_vec(),
                        ));
                    }
                    (DefaultFunction::UnMapData, Constant::Data(PlutusData::Map(m))) => {
                        *term = Term::map_values(
                            m.iter()
                                .map(|item| {
                                    Constant::ProtoPair(
                                        Type::Data,
                                        Type::Data,
                                        Constant::Data(item.0.clone()).into(),
                                        Constant::Data(item.1.clone()).into(),
                                    )
                                })
                                .collect_vec(),
                        );
                    }
                    _ => {
                        cast_data_reducer(Rc::make_mut(argument));
                    }
                }
            } else {
                cast_data_reducer(Rc::make_mut(argument));
            };
        }
        Term::Force(f) => {
            cast_data_reducer(Rc::make_mut(f));
        }
        Term::Case { .. } => todo!(),
        Term::Constr { .. } => todo!(),
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
        Term::Case { .. } => todo!(),
        Term::Constr { .. } => todo!(),
        _ => 0,
    }
}

fn delayed_execution(term: &Term<Name>) -> usize {
    match term {
        Term::Delay(body) => 1 + delayed_execution(body.as_ref()),
        Term::Lambda { body, .. } => 1 + delayed_execution(body.as_ref()),
        Term::Apply { function, argument } => {
            delayed_execution(function.as_ref()) + delayed_execution(argument.as_ref())
        }
        Term::Force(x) => delayed_execution(x.as_ref()),
        Term::Case { constr, branches } => {
            1 + delayed_execution(constr.as_ref())
                + branches
                    .iter()
                    .fold(0, |acc, branch| acc + delayed_execution(branch))
        }
        Term::Constr { fields, .. } => fields
            .iter()
            .fold(0, |acc, field| acc + delayed_execution(field)),
        _ => 0,
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
        Term::Case { .. } => todo!(),
        Term::Constr { .. } => todo!(),
        x => x.clone(),
    }
}

fn replace_identity_usage(term: &Term<Name>, original: Rc<Name>) -> Term<Name> {
    match term {
        Term::Delay(body) => Term::Delay(replace_identity_usage(body.as_ref(), original).into()),
        Term::Lambda {
            parameter_name,
            body,
        } => {
            if parameter_name.as_ref() != original.as_ref() {
                Term::Lambda {
                    parameter_name: parameter_name.clone(),
                    body: Rc::new(replace_identity_usage(body.as_ref(), original)),
                }
            } else {
                Term::Lambda {
                    parameter_name: parameter_name.clone(),
                    body: body.clone(),
                }
            }
        }
        Term::Apply { function, argument } => {
            let func = function.as_ref();
            let arg = argument.as_ref();

            let func = replace_identity_usage(func, original.clone());
            let arg = replace_identity_usage(arg, original.clone());

            let Term::Var(f) = function.as_ref() else {
                return Term::Apply {
                    function: func.into(),
                    argument: arg.into(),
                };
            };

            if f.as_ref() == original.as_ref() {
                arg
            } else {
                Term::Apply {
                    function: func.into(),
                    argument: arg.into(),
                }
            }
        }
        Term::Force(x) => Term::Force(Rc::new(replace_identity_usage(x.as_ref(), original))),
        Term::Case { .. } => todo!(),
        Term::Constr { .. } => todo!(),
        x => x.clone(),
    }
}

#[cfg(test)]
mod tests {

    use pallas_primitives::babbage::{BigInt, PlutusData};
    use pretty_assertions::assert_eq;

    use crate::{
        ast::{Constant, Data, Name, NamedDeBruijn, Program, Term},
        builtins::DefaultFunction,
        parser::interner::Interner,
    };

    #[test]
    fn lambda_reduce_var() {
        let mut program = Program {
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
        };

        let mut interner = Interner::new();

        interner.program(&mut program);

        let mut expected = Program {
            version: (1, 0, 0),
            term: Term::var("foo").lambda("foo").apply(
                Term::constr_data()
                    .apply(Term::integer(3.into()))
                    .apply(Term::list_values(vec![])),
            ),
        };

        let mut interner = Interner::new();

        interner.program(&mut expected);

        let expected: Program<NamedDeBruijn> = expected.try_into().unwrap();
        let actual = program.lambda_reducer();

        let actual: Program<NamedDeBruijn> = actual.try_into().unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn lambda_reduce_constant() {
        let mut program = Program {
            version: (1, 0, 0),
            term: Term::var("foo")
                .lambda("foo")
                .apply(Term::integer(6.into())),
        };

        let mut interner = Interner::new();

        interner.program(&mut program);

        let mut expected: Program<Name> = Program {
            version: (1, 0, 0),
            term: Term::integer(6.into()),
        };

        let mut interner = Interner::new();

        interner.program(&mut expected);

        let expected: Program<NamedDeBruijn> = expected.try_into().unwrap();
        let actual = program.lambda_reducer();

        let actual: Program<NamedDeBruijn> = actual.try_into().unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn lambda_reduce_builtin() {
        let mut program = Program {
            version: (1, 0, 0),
            term: Term::var("foo").lambda("foo").apply(Term::add_integer()),
        };

        let mut interner = Interner::new();

        interner.program(&mut program);

        let mut expected: Program<Name> = Program {
            version: (1, 0, 0),
            term: Term::add_integer(),
        };

        let mut interner = Interner::new();

        interner.program(&mut expected);

        let expected: Program<NamedDeBruijn> = expected.try_into().unwrap();

        let actual = program.lambda_reducer();

        let actual: Program<NamedDeBruijn> = actual.try_into().unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn lambda_reduce_force_delay_error_lam() {
        let mut program: Program<Name> = Program {
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

        let mut interner = Interner::new();

        interner.program(&mut program);

        let mut expected = Program {
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

        let mut interner = Interner::new();

        interner.program(&mut expected);

        let expected: Program<NamedDeBruijn> = expected.try_into().unwrap();

        let actual = program.lambda_reducer();

        let actual: Program<NamedDeBruijn> = actual.try_into().unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn wrap_data_reduce_i_data() {
        let mut program: Program<Name> = Program {
            version: (1, 0, 0),
            term: Term::equals_data()
                .apply(Term::i_data().apply(Term::un_i_data().apply(Term::Constant(
                    Constant::Data(PlutusData::BigInt(BigInt::Int(5.into()))).into(),
                ))))
                .apply(Term::i_data().apply(Term::integer(1.into())))
                .lambda("x"),
        };

        let mut interner = Interner::new();

        interner.program(&mut program);

        let mut expected = Program {
            version: (1, 0, 0),
            term: Term::equals_data()
                .apply(Term::Constant(
                    Constant::Data(PlutusData::BigInt(BigInt::Int(5.into()))).into(),
                ))
                .apply(Term::data(Data::integer(1.into())))
                .lambda("x"),
        };

        let mut interner = Interner::new();

        interner.program(&mut expected);

        let expected: Program<NamedDeBruijn> = expected.try_into().unwrap();

        let actual = program.cast_data_reducer();

        let actual: Program<NamedDeBruijn> = actual.try_into().unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn wrap_data_reduce_un_i_data() {
        let mut program: Program<Name> = Program {
            version: (1, 0, 0),
            term: Term::equals_integer()
                .apply(Term::un_i_data().apply(Term::i_data().apply(Term::integer(1.into()))))
                .apply(Term::un_i_data().apply(Term::Constant(
                    Constant::Data(PlutusData::BigInt(BigInt::Int(5.into()))).into(),
                )))
                .lambda("x"),
        };

        let mut interner = Interner::new();

        interner.program(&mut program);

        let mut expected = Program {
            version: (1, 0, 0),
            term: Term::equals_integer()
                .apply(Term::integer(1.into()))
                .apply(Term::integer(5.into()))
                .lambda("x"),
        };

        let mut interner = Interner::new();

        interner.program(&mut expected);

        let expected: Program<NamedDeBruijn> = expected.try_into().unwrap();

        let actual = program.cast_data_reducer();

        let actual: Program<NamedDeBruijn> = actual.try_into().unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn builtin_force_reduce_list_builtins() {
        let program: Program<Name> = Program {
            version: (1, 0, 0),
            term: Term::mk_cons()
                .apply(Term::var("x"))
                .apply(Term::tail_list().apply(Term::head_list().apply(Term::var("y"))))
                .lambda("x")
                .lambda("y"),
        };

        let mut expected = Program {
            version: (1, 0, 0),
            term: Term::var("__cons_list_wrapped")
                .apply(Term::var("x"))
                .apply(
                    Term::var("__tail_list_wrapped")
                        .apply(Term::var("__head_list_wrapped").apply(Term::var("y"))),
                )
                .lambda("x")
                .lambda("y")
                .lambda("__cons_list_wrapped")
                .apply(Term::mk_cons())
                .lambda("__head_list_wrapped")
                .apply(Term::head_list())
                .lambda("__tail_list_wrapped")
                .apply(Term::tail_list()),
        };

        let mut interner = Interner::new();

        interner.program(&mut expected);

        let expected: Program<NamedDeBruijn> = expected.try_into().unwrap();

        let mut actual = program.builtin_force_reducer();

        let mut interner = Interner::new();

        interner.program(&mut actual);

        let actual: Program<NamedDeBruijn> = actual.try_into().unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn builtin_force_reduce_if_builtin() {
        let program: Program<Name> = Program {
            version: (1, 0, 0),
            term: Term::equals_integer()
                .apply(Term::var("x"))
                .apply(
                    Term::add_integer()
                        .apply(Term::integer(2.into()))
                        .apply(Term::var("y")),
                )
                .delayed_if_then_else(
                    Term::length_of_bytearray().apply(Term::byte_string(vec![])),
                    Term::Error,
                )
                .lambda("x")
                .lambda("y"),
        };

        let mut expected = Program {
            version: (1, 0, 0),
            term: Term::var("__if_then_else_wrapped")
                .apply(
                    Term::equals_integer().apply(Term::var("x")).apply(
                        Term::add_integer()
                            .apply(Term::integer(2.into()))
                            .apply(Term::var("y")),
                    ),
                )
                .apply(
                    Term::length_of_bytearray()
                        .apply(Term::byte_string(vec![]))
                        .delay(),
                )
                .apply(Term::Error.delay())
                .force()
                .lambda("x")
                .lambda("y")
                .lambda("__if_then_else_wrapped")
                .apply(Term::Builtin(DefaultFunction::IfThenElse).force()),
        };

        let mut interner = Interner::new();

        interner.program(&mut expected);

        let expected: Program<NamedDeBruijn> = expected.try_into().unwrap();

        let mut actual = program.builtin_force_reducer();

        let mut interner = Interner::new();

        interner.program(&mut actual);

        let actual: Program<NamedDeBruijn> = actual.try_into().unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn inline_reduce_delay_sha() {
        let mut program: Program<Name> = Program {
            version: (1, 0, 0),
            term: Term::sha2_256()
                .apply(Term::var("x"))
                .lambda("x")
                .apply(Term::byte_string(vec![]).delay()),
        };

        let mut interner = Interner::new();

        interner.program(&mut program);

        let mut expected = Program {
            version: (1, 0, 0),
            term: Term::sha2_256().apply(Term::byte_string(vec![]).delay()),
        };

        let mut interner = Interner::new();

        interner.program(&mut expected);

        let expected: Program<NamedDeBruijn> = expected.try_into().unwrap();

        let actual = program.inline_reducer();

        let actual: Program<NamedDeBruijn> = actual.try_into().unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn inline_reduce_identity() {
        let mut program: Program<Name> = Program {
            version: (1, 0, 0),
            term: Term::sha2_256()
                .apply(Term::var("identity").apply(Term::var("x")))
                .lambda("x")
                .apply(Term::byte_string(vec![]).delay())
                .lambda("identity")
                .apply(Term::var("y").lambda("y")),
        };

        let mut interner = Interner::new();

        interner.program(&mut program);

        let mut expected = Program {
            version: (1, 0, 0),
            term: Term::sha2_256().apply(Term::byte_string(vec![]).delay()),
        };

        let mut interner = Interner::new();

        interner.program(&mut expected);

        let expected: Program<NamedDeBruijn> = expected.try_into().unwrap();

        let actual = program.inline_reducer();

        let actual: Program<NamedDeBruijn> = actual.try_into().unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn inline_reduce_identity_param() {
        let mut program: Program<Name> = Program {
            version: (1, 0, 0),
            term: Term::sha2_256()
                .apply(
                    Term::var("f")
                        .apply(Term::var("x"))
                        .apply(Term::var("identity")),
                )
                .lambda("x")
                .apply(Term::byte_string(vec![]).delay())
                .lambda("identity")
                .apply(Term::var("y").lambda("y"))
                .lambda("f")
                .apply(
                    Term::var("with")
                        .apply(Term::var("x"))
                        .lambda("with")
                        .lambda("x"),
                ),
        };

        let mut interner = Interner::new();

        interner.program(&mut program);

        let mut expected = Program {
            version: (1, 0, 0),
            term: Term::sha2_256().apply(
                Term::var("with")
                    .apply(Term::var("x"))
                    .lambda("with")
                    .lambda("x")
                    .apply(Term::byte_string(vec![]).delay())
                    .apply(Term::var("y").lambda("y")),
            ),
        };

        let mut interner = Interner::new();

        interner.program(&mut expected);

        let expected: Program<NamedDeBruijn> = expected.try_into().unwrap();

        let actual = program.inline_reducer();

        let actual: Program<NamedDeBruijn> = actual.try_into().unwrap();

        assert_eq!(actual, expected);
    }
}
