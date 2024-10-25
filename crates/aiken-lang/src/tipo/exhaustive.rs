use crate::{
    ast,
    tipo::{self, environment::Environment, error::Error, Type},
};
use itertools::Itertools;
use std::{collections::BTreeMap, iter, ops::Deref};

const NIL_NAME: &str = "[]";
const CONS_NAME: &str = "::";
const TUPLE_NAME: &str = "__Tuple";

#[derive(Debug, Clone)]
pub(crate) struct PatternStack(Vec<Pattern>);

impl From<Pattern> for PatternStack {
    fn from(value: Pattern) -> Self {
        Self(vec![value])
    }
}

impl From<Vec<Pattern>> for PatternStack {
    fn from(value: Vec<Pattern>) -> Self {
        Self(value)
    }
}

impl From<PatternStack> for Vec<Pattern> {
    fn from(value: PatternStack) -> Self {
        value.0
    }
}

impl PatternStack {
    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    fn insert(&mut self, index: usize, element: Pattern) {
        self.0.insert(index, element);
    }

    pub(super) fn head(&self) -> &Pattern {
        &self.0[0]
    }

    fn tail(&self) -> PatternStack {
        self.0
            .iter()
            .skip(1)
            .cloned()
            .collect::<Vec<Pattern>>()
            .into()
    }

    fn iter(&self) -> impl Iterator<Item = &Pattern> {
        self.0.iter()
    }

    fn chain_tail_to_iter<'a>(&'a self, front: impl Iterator<Item = &'a Pattern>) -> PatternStack {
        front
            .chain(self.iter().skip(1))
            .cloned()
            .collect::<Vec<Pattern>>()
            .into()
    }

    fn chain_tail_into_iter(&self, front: impl Iterator<Item = Pattern>) -> PatternStack {
        front
            .chain(self.iter().skip(1).cloned())
            .collect::<Vec<Pattern>>()
            .into()
    }

    // INVARIANT: (length row == N) ==> (length result == arity + N - 1)
    fn specialize_row_by_ctor(&self, name: &String, arity: usize) -> Option<PatternStack> {
        match self.head() {
            Pattern::Constructor(p_name, _, p_args) => {
                if p_name == name && p_args.len() == arity {
                    Some(self.chain_tail_to_iter(p_args.iter()))
                } else {
                    None
                }
            }
            Pattern::Wildcard => {
                Some(self.chain_tail_into_iter(vec![Pattern::Wildcard; arity].into_iter()))
            }
            Pattern::Literal(_) => unreachable!(
                "constructors and literals should never align in pattern match exhaustiveness checks."
            ),
        }
    }

    // INVARIANT: (length row == N) ==> (length result == N-1)
    fn specialize_row_by_wildcard(&self) -> Option<PatternStack> {
        if self.is_empty() {
            return None;
        }

        match self.head() {
            Pattern::Constructor(_, _, _) => None,
            Pattern::Literal(_) => None,
            Pattern::Wildcard => Some(self.tail()),
        }
    }

    // INVARIANT: (length row == N) ==> (length result == N-1)
    fn specialize_row_by_literal(&self, literal: &Literal) -> Option<PatternStack> {
        match self.head() {
            Pattern::Literal(p_literal) => {
                if p_literal == literal {
                    Some(self.tail())
                } else {
                    None
                }
            }
            Pattern::Wildcard => Some(self.tail()),
            Pattern::Constructor(_, _, _) => unreachable!(
                "constructors and literals should never align in pattern match exhaustiveness checks."
            ),
        }
    }

    fn split_at(self, arity: usize) -> (PatternStack, PatternStack) {
        let mut rest = self.0;

        let mut args = rest.split_off(arity);

        std::mem::swap(&mut rest, &mut args);

        (args.into(), rest.into())
    }
}

#[derive(Debug)]
pub(super) struct Matrix(Vec<PatternStack>);

impl Matrix {
    pub(super) fn new() -> Self {
        Matrix(vec![])
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub(crate) fn push(&mut self, pattern_stack: PatternStack) {
        self.0.push(pattern_stack);
    }

    /// Iterate over the first component of each row
    pub(super) fn iter(&self) -> impl Iterator<Item = &PatternStack> {
        self.0.iter()
    }

    /// Iterate over the first component of each row, mutably
    pub(super) fn into_iter(self) -> impl Iterator<Item = PatternStack> {
        self.0.into_iter()
    }

    pub(super) fn concat(self, other: Matrix) -> Matrix {
        let mut patterns = self.0;
        patterns.extend(other.0);
        Matrix(patterns)
    }

    pub(crate) fn is_complete(&self) -> Complete {
        let ctors = self.collect_ctors();
        let num_seen = ctors.len();

        if num_seen == 0 {
            Complete::No
        } else {
            let (_, alts) = ctors.first_key_value().to_owned().unwrap();

            if num_seen == alts.len() {
                Complete::Yes(alts.to_vec())
            } else {
                Complete::No
            }
        }
    }

    pub(crate) fn collect_ctors(&self) -> BTreeMap<String, Vec<tipo::ValueConstructor>> {
        let mut ctors = BTreeMap::new();

        for pattern_stack in self.iter() {
            match pattern_stack.head() {
                Pattern::Constructor(name, alts, _) => {
                    ctors.insert(name.clone(), alts.clone());
                }
                Pattern::Wildcard | Pattern::Literal(_) => {}
            }
        }

        ctors
    }

    fn specialize_rows_by_ctor(&self, name: &String, arity: usize) -> Matrix {
        self.iter()
            .filter_map(|p_stack| p_stack.specialize_row_by_ctor(name, arity))
            .collect()
    }

    fn specialize_rows_by_wildcard(&self) -> Matrix {
        self.iter()
            .filter_map(|p_stack| p_stack.specialize_row_by_wildcard())
            .collect()
    }

    fn specialize_rows_by_literal(&self, literal: &Literal) -> Matrix {
        self.iter()
            .filter_map(|p_stack| p_stack.specialize_row_by_literal(literal))
            .collect()
    }

    pub(super) fn is_useful(&self, vector: &PatternStack) -> bool {
        // No rows are the same as the new vector! The vector is useful!
        if self.is_empty() {
            return true;
        }

        // There is nothing left in the new vector, but we still have
        // rows that match the same things. This is not a useful vector!
        if vector.is_empty() {
            return false;
        }

        let first_pattern = vector.head();

        match first_pattern {
            Pattern::Constructor(name, _, args) => {
                let arity = args.len();

                let new_matrix = self.specialize_rows_by_ctor(name, arity);

                let new_vector: PatternStack = vector.chain_tail_to_iter(args.iter());

                new_matrix.is_useful(&new_vector)
            }
            Pattern::Wildcard => {
                // check if all alts appear in matrix
                match self.is_complete() {
                    Complete::No => {
                        // This Wildcard is useful because some Ctors are missing.
                        // But what if a previous row has a Wildcard?
                        // If so, this one is not useful.
                        let new_matrix = self.specialize_rows_by_wildcard();

                        let new_vector = vector.tail();

                        new_matrix.is_useful(&new_vector)
                    }
                    Complete::Yes(alts) => alts.into_iter().any(|alt| {
                        let tipo::ValueConstructor { variant, .. } = alt;
                        let (name, arity) = match variant {
                            tipo::ValueConstructorVariant::Record { name, arity, .. } => {
                                (name, arity)
                            }
                            _ => unreachable!("variant should be a ValueConstructorVariant"),
                        };

                        let new_matrix = self.specialize_rows_by_ctor(&name, arity);

                        let new_vector =
                            vector.chain_tail_into_iter(vec![Pattern::Wildcard; arity].into_iter());

                        new_matrix.is_useful(&new_vector)
                    }),
                }
            }
            Pattern::Literal(literal) => {
                let new_matrix: Matrix = self.specialize_rows_by_literal(literal);

                let new_vector = vector.tail();

                new_matrix.is_useful(&new_vector)
            }
        }
    }

    pub(super) fn flatten(self) -> Vec<Pattern> {
        self.into_iter().fold(vec![], |mut acc, p_stack| {
            acc.extend(p_stack.0);

            acc
        })
    }

    // INVARIANTS:
    //
    //   The initial rows "matrix" are all of length 1
    //   The initial count of items per row "n" is also 1
    //   The resulting rows are examples of missing patterns
    //
    pub(super) fn collect_missing_patterns(self, n: usize) -> Matrix {
        if self.is_empty() {
            return Matrix(vec![vec![Pattern::Wildcard; n].into()]);
        }

        if n == 0 {
            return Matrix::new();
        }

        let ctors = self.collect_ctors();
        let num_seen = ctors.len();

        if num_seen == 0 {
            let new_matrix = self.specialize_rows_by_wildcard();

            let new_matrix = new_matrix.collect_missing_patterns(n - 1);

            let new_matrix = new_matrix
                .iter()
                .map(|p_stack| {
                    let mut new_p_stack = p_stack.clone();
                    new_p_stack.insert(0, Pattern::Wildcard);
                    new_p_stack
                })
                .collect::<Matrix>();

            return new_matrix;
        }

        let (_, alts) = ctors.first_key_value().unwrap();

        if num_seen < alts.len() {
            let new_matrix = self.specialize_rows_by_wildcard();

            let new_matrix = new_matrix.collect_missing_patterns(n - 1);

            let prefix = alts.iter().filter_map(|alt| is_missing(alts, &ctors, alt));

            let mut m = Matrix::new();

            for p_stack in new_matrix.into_iter() {
                for p in prefix.clone() {
                    let mut p_stack = p_stack.clone();
                    p_stack.insert(0, p);
                    m.push(p_stack);
                }
            }

            return m;
        }

        alts.iter()
            .map(|ctor| {
                let tipo::ValueConstructor { variant, .. } = ctor;
                let (name, arity) = match variant {
                    tipo::ValueConstructorVariant::Record { name, arity, .. } => (name, arity),
                    _ => unreachable!("variant should be a ValueConstructorVariant"),
                };

                let new_matrix = self.specialize_rows_by_ctor(name, *arity);

                let new_matrix = new_matrix.collect_missing_patterns(*arity + n - 1);

                new_matrix
                    .into_iter()
                    .map(|p_stack| recover_ctor(alts.clone(), name, *arity, p_stack))
                    .collect()
            })
            .fold(Matrix::new(), |acc, m| acc.concat(m))
    }
}

#[derive(Debug)]
pub(crate) enum Complete {
    Yes(Vec<tipo::ValueConstructor>),
    No,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Pattern {
    Wildcard,
    Literal(Literal),
    Constructor(String, Vec<tipo::ValueConstructor>, Vec<Pattern>),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Literal {
    Int(String),
    ByteArray(Vec<u8>),
}

impl Pattern {
    pub(super) fn pretty(self) -> String {
        match self {
            Pattern::Wildcard => "_".to_string(),
            Pattern::Literal(_) => unreachable!("maybe never happens?"),
            Pattern::Constructor(name, _alts, args) if name.contains(TUPLE_NAME) => {
                let mut pretty_pattern = "(".to_string();

                pretty_pattern.push_str(&args.into_iter().map(Pattern::pretty).join(", "));

                pretty_pattern.push(')');

                pretty_pattern
            }

            Pattern::Constructor(name, _alts, args) if name == CONS_NAME => {
                let mut pretty_pattern = "[".to_string();

                let args = args
                    .into_iter()
                    .enumerate()
                    .filter_map(|(index, p)| {
                        if index == 1 {
                            let tail = pretty_tail(p);
                            if tail == "[]" {
                                None
                            } else {
                                Some(tail)
                            }
                        } else {
                            Some(p.pretty())
                        }
                    })
                    .join(", ");

                pretty_pattern.push_str(&args);

                pretty_pattern.push(']');

                pretty_pattern
            }
            Pattern::Constructor(mut name, alts, args) => {
                let field_map = alts.into_iter().find_map(|alt| {
                    let tipo::ValueConstructor { variant, .. } = alt;

                    match variant {
                        tipo::ValueConstructorVariant::Record {
                            name: r_name,
                            field_map,
                            ..
                        } if r_name == name => field_map,
                        _ => None,
                    }
                });

                if let Some(field_map) = field_map {
                    name.push_str(" { ");

                    let labels = field_map
                        .fields
                        .into_iter()
                        .sorted_by(|(_, (index_a, _)), (_, (index_b, _))| index_a.cmp(index_b))
                        .map(|(label, _)| label)
                        .zip(args)
                        .map(|(label, arg)| match arg {
                            Pattern::Wildcard => label,
                            rest => format!("{label}: {}", rest.pretty()),
                        })
                        .join(", ");

                    name.push_str(&labels);

                    name.push_str(" }");

                    name
                } else {
                    if !args.is_empty() {
                        name.push('(');
                        name.push_str(&args.into_iter().map(Pattern::pretty).join(", "));
                        name.push(')');
                    }

                    name
                }
            }
        }
    }
}

fn pretty_tail(tail: Pattern) -> String {
    match tail {
        Pattern::Constructor(name, _alts, args) if name == CONS_NAME => {
            let mut pretty_pattern = "".to_string();

            let args = args
                .into_iter()
                .enumerate()
                .map(|(index, p)| {
                    if index == 1 {
                        pretty_tail(p)
                    } else {
                        p.pretty()
                    }
                })
                .join(", ");

            pretty_pattern.push_str(&args);

            pretty_pattern
        }
        Pattern::Wildcard => "..".to_string(),
        rest => rest.pretty(),
    }
}

fn list_constructors() -> Vec<tipo::ValueConstructor> {
    let list_parameter = Type::generic_var(0);
    let list_type = Type::list(list_parameter);

    vec![
        tipo::ValueConstructor {
            public: true,
            tipo: list_type.clone(),
            variant: tipo::ValueConstructorVariant::Record {
                name: CONS_NAME.to_string(),
                arity: 2,
                field_map: None,
                location: ast::Span::empty(),
                module: "".to_string(),
                constructors_count: 2,
            },
        },
        tipo::ValueConstructor {
            public: true,
            tipo: list_type,
            variant: tipo::ValueConstructorVariant::Record {
                name: NIL_NAME.to_string(),
                arity: 0,
                field_map: None,
                location: ast::Span::empty(),
                module: "".to_string(),
                constructors_count: 2,
            },
        },
    ]
}

#[allow(clippy::result_large_err)]
pub(super) fn simplify(
    environment: &mut Environment,
    value: &ast::TypedPattern,
) -> Result<Pattern, Error> {
    match value {
        ast::Pattern::Int { value, .. } => Ok(Pattern::Literal(Literal::Int(value.clone()))),
        ast::Pattern::ByteArray { value, .. } => {
            Ok(Pattern::Literal(Literal::ByteArray(value.clone())))
        }
        ast::Pattern::Assign { pattern, .. } => simplify(environment, pattern.as_ref()),
        ast::Pattern::List { elements, tail, .. } => {
            let mut p = if let Some(t) = tail {
                simplify(environment, t)?
            } else {
                Pattern::Constructor(NIL_NAME.to_string(), list_constructors(), vec![])
            };

            for hd in elements.iter().rev() {
                p = Pattern::Constructor(
                    CONS_NAME.to_string(),
                    list_constructors(),
                    vec![simplify(environment, hd)?, p],
                );
            }

            Ok(p)
        }
        ast::Pattern::Constructor {
            arguments,
            location,
            tipo,
            spread_location,
            constructor: super::PatternConstructor::Record { name, .. },
            ..
        } => {
            let (module, type_name, arity) = match tipo.deref() {
                tipo::Type::App {
                    name: type_name,
                    module,
                    ..
                } => (module, type_name, 0),
                tipo::Type::Fn { ret, args, .. } => match ret.deref() {
                    tipo::Type::App {
                        name: type_name,
                        module,
                        ..
                    } => (module, type_name, args.len()),
                    _ => {
                        unreachable!("ret should be a Type::App")
                    }
                },
                _ => unreachable!("tipo should be a Type::App"),
            };

            let alts = environment.get_constructors_for_type(module, type_name, *location)?;

            let mut args = Vec::new();

            for argument in arguments {
                args.push(simplify(environment, &argument.value)?);
            }

            if spread_location.is_some() {
                for _ in 0..(arity - arguments.len()) {
                    args.push(Pattern::Wildcard)
                }
            }

            Ok(Pattern::Constructor(name.to_string(), alts, args))
        }
        ast::Pattern::Pair { fst, snd, location } => simplify(
            environment,
            &ast::Pattern::Tuple {
                elems: vec![*fst.clone(), *snd.clone()],
                location: *location,
            },
        ),
        ast::Pattern::Tuple { elems, .. } => {
            let mut args = vec![];

            for elem in elems {
                args.push(simplify(environment, elem)?);
            }

            Ok(Pattern::Constructor(
                TUPLE_NAME.to_string(),
                vec![tipo::ValueConstructor {
                    tipo: tipo::Type::Tuple {
                        elems: vec![],
                        alias: None,
                    }
                    .into(),
                    public: true,
                    variant: tipo::ValueConstructorVariant::Record {
                        name: TUPLE_NAME.to_string(),
                        arity: elems.len(),
                        field_map: None,
                        location: ast::Span::empty(),
                        module: "".to_string(),
                        constructors_count: 1,
                    },
                }],
                args,
            ))
        }
        ast::Pattern::Var { .. } | ast::Pattern::Discard { .. } => Ok(Pattern::Wildcard),
    }
}

impl iter::FromIterator<PatternStack> for Matrix {
    fn from_iter<T: IntoIterator<Item = PatternStack>>(iter: T) -> Self {
        Matrix(iter.into_iter().collect())
    }
}

fn recover_ctor(
    alts: Vec<tipo::ValueConstructor>,
    name: &str,
    arity: usize,
    patterns: PatternStack,
) -> PatternStack {
    let (args, mut rest) = patterns.split_at(arity);

    rest.insert(0, Pattern::Constructor(name.to_string(), alts, args.into()));

    rest
}

fn is_missing(
    alts: &[tipo::ValueConstructor],
    ctors: &BTreeMap<String, Vec<tipo::ValueConstructor>>,
    ctor: &tipo::ValueConstructor,
) -> Option<Pattern> {
    let tipo::ValueConstructor { variant, .. } = ctor;
    let (name, arity) = match variant {
        tipo::ValueConstructorVariant::Record { name, arity, .. } => (name, arity),
        _ => unreachable!("variant should be a ValueConstructorVariant"),
    };

    if ctors.contains_key(name) {
        None
    } else {
        Some(Pattern::Constructor(
            name.clone(),
            alts.to_vec(),
            vec![Pattern::Wildcard; *arity],
        ))
    }
}
