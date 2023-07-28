use std::{collections::BTreeMap, iter, ops::Deref};

use crate::{
    ast::{self, Span, TypedPattern},
    tipo::{self, environment::Environment, error::Error}, builtins,
};

const NIL_NAME: &str = "[]";
const CONS_NAME: &str = "::";

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

    fn head(&self) -> &Pattern {
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
            .chain(
                self.iter()
                    .skip(1)
                    .cloned()
            )
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
    fn new() -> Self {
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

    fn is_useful(&self, vector: &PatternStack) -> bool {
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
    
                        new_matrix.is_useful( &new_vector)
                    }
                    Complete::Yes(alts) => alts.into_iter().any(|alt| {
                        let tipo::ValueConstructor { variant, .. } = alt;
                        let tipo::ValueConstructorVariant::Record {
                            name,
                            arity,
                            ..
                        } = variant else {unreachable!("variant should be a ValueConstructorVariant")};
    
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
}

#[derive(Debug)]
pub(crate) enum Complete {
    Yes(Vec<tipo::ValueConstructor>),
    No,
}

#[derive(Debug)]
pub(crate) struct Witness(Vec<TypedPattern>);

#[derive(Debug)]
enum Usefulness {
    /// If we don't care about witnesses, simply remember if the pattern was useful.
    NoWitnesses { useful: bool },
    /// Carries a list of witnesses of non-exhaustiveness. If empty, indicates that the whole
    /// pattern is unreachable.
    WithWitnesses(Vec<Witness>),
}

#[derive(Copy, Clone, Debug)]
enum ArmType {
    FakeExtraWildcard,
    RealArm,
}

#[derive(Clone, Debug)]
pub(crate) enum Reachability {
    /// The arm is reachable. This additionally carries a set of or-pattern branches that have been
    /// found to be unreachable despite the overall arm being reachable. Used only in the presence
    /// of or-patterns, otherwise it stays empty.
    Reachable(Vec<Span>),
    /// The arm is unreachable.
    Unreachable,
}

#[derive(Debug)]
pub(crate) struct UsefulnessReport {
    /// For each arm of the input, whether that arm is reachable after the arms above it.
    pub(crate) arm_usefulness: Vec<(ast::TypedClause, Reachability)>,
    /// If the match is exhaustive, this is empty. If not, this contains witnesses for the lack of
    /// exhaustiveness.
    pub(crate) non_exhaustiveness_witnesses: Vec<TypedPattern>,
}

#[derive(Debug, Clone)]
pub(crate) enum Pattern {
    Wildcard,
    Literal(Literal),
    Constructor(String, Vec<tipo::ValueConstructor>, Vec<Pattern>),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Literal {
    Int(String),
}

fn list_constructors() -> Vec<tipo::ValueConstructor> {
    let list_parameter = builtins::generic_var(0);
    let list_type = builtins::list(list_parameter);

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
                constructors_count: 2
            }
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
                constructors_count: 2
            }
        },
    ]
}

fn simplify(environment: &mut Environment, value: &ast::TypedPattern) -> Result<Pattern, Error> {
    match value {
        ast::Pattern::Int { value, .. } => Ok(Pattern::Literal(Literal::Int(value.clone()))),
        ast::Pattern::Assign { pattern, .. } => simplify(environment, pattern.as_ref()),
        ast::Pattern::List { elements, tail, .. } => {
            let mut p = if let Some(t) = tail {
                simplify(environment, t)?
            } else {
                Pattern::Constructor(NIL_NAME.to_string(), list_constructors(), vec![])
            };

            for hd in elements.iter().rev() {
                p = Pattern::Constructor(CONS_NAME.to_string(), list_constructors(), vec![simplify(environment, hd)?, p]);
            }

            Ok(p)
        },
        ast::Pattern::Constructor {
            name,
            arguments,
            module,
            location,
            tipo,
            ..
        } => {
            let type_name = match tipo.deref() {
                tipo::Type::App {
                    name: type_name, ..
                } => type_name,
                tipo::Type::Fn { ret, .. } => {
                    let tipo::Type::App {
                        name: type_name, ..
                    } = ret.deref() else {unreachable!("ret should be a Type::App")};

                    type_name
                }
                _ => unreachable!("tipo should be a Type::App"),
            };

            let constructors = environment
                .get_constructors_for_type(module, type_name, *location)?
                .clone();

            let mut alts = Vec::new();

            for constructor in constructors {
                let value_constructor =
                    environment.get_value_constructor(module.as_ref(), &constructor, *location)?;

                alts.push(value_constructor.clone());
            }

            let mut args = Vec::new();

            for argument in arguments {
                args.push(simplify(environment, &argument.value)?);
            }

            Ok(Pattern::Constructor(name.to_string(), alts, args))
        }
        ast::Pattern::Tuple { elems, .. } =>  {
            let mut p = Pattern::Constructor(NIL_NAME.to_string(), list_constructors(), vec![]);

            for hd in elems.iter().rev() {
                p = Pattern::Constructor(CONS_NAME.to_string(), list_constructors(), vec![simplify(environment, hd)?, p]);
            }

            Ok(p)
        },
        ast::Pattern::Var { .. } | ast::Pattern::Discard { .. } => Ok(Pattern::Wildcard),
    }
}

impl iter::FromIterator<PatternStack> for Matrix {
    fn from_iter<T: IntoIterator<Item = PatternStack>>(iter: T) -> Self {
        Matrix(iter.into_iter().collect())
    }
}

pub(crate) fn compute_match_usefulness(
    environment: &mut Environment,
    unchecked_patterns: &[&ast::TypedPattern],
) -> Result<UsefulnessReport, Error> {
    let mut matrix = Matrix::new();

    for unchecked_pattern in unchecked_patterns {
        let pattern = simplify(environment, unchecked_pattern)?;
        let pattern_stack = PatternStack::from(pattern);

        if matrix.is_useful(&pattern_stack) {
            matrix.push(pattern_stack);
        } else {
            return Err(Error::RedundantMatchClause { location: unchecked_pattern.location() })
        }
    }

    dbg!(&matrix);

    let bad_patterns = is_exhaustive(matrix, 1);

    dbg!(bad_patterns);

    Ok(UsefulnessReport {
        arm_usefulness: vec![],
        non_exhaustiveness_witnesses: vec![],
    })
}

// INVARIANTS:
//
//   The initial rows "matrix" are all of length 1
//   The initial count of items per row "n" is also 1
//   The resulting rows are examples of missing patterns
//
fn is_exhaustive(matrix: Matrix, n: usize) -> Matrix {
    if matrix.is_empty() {
        return Matrix(vec![vec![Pattern::Wildcard; n].into()]);
    }

    if n == 0 {
        return Matrix::new();
    }

    let ctors = matrix.collect_ctors();
    let num_seen = ctors.len();

    if num_seen == 0 {
        let new_matrix = matrix.specialize_rows_by_wildcard();

        let new_matrix = is_exhaustive(new_matrix, n - 1);

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
        let new_matrix = matrix.specialize_rows_by_wildcard();

        let new_matrix = is_exhaustive(new_matrix, n - 1);

        let prefix = alts.iter().filter_map(|alt| is_missing(alts, &ctors, alt));

        let mut m = Matrix::new();

        for p_stack in new_matrix.into_iter() {
            for p in prefix.clone() {
                let mut p_stack = p_stack.clone();
                p_stack.insert(0, p);
                m.push(p_stack);
            }
        }

        // (:)
        //     <$> Maybe.mapMaybe (isMissing alts ctors) altList
        //     <*> isExhaustive (Maybe.mapMaybe specializeRowByAnything matrix) (n - 1)
        return m;
    }

    // let
    //   isAltExhaustive (Can.Ctor name _ arity _) =
    //     recoverCtor alts name arity <$>
    //     isExhaustive
    //       (Maybe.mapMaybe (specializeRowByCtor name arity) matrix)
    //       (arity + n - 1)
    // in
    // concatMap isAltExhaustive altList
    //

    alts.iter()
        .map(|ctor| {
            let tipo::ValueConstructor { variant, .. } = ctor;
            let tipo::ValueConstructorVariant::Record {
                name,
                arity,
                ..
            } = variant else {unreachable!("variant should be a ValueConstructorVariant")};

            let new_matrix = matrix.specialize_rows_by_ctor(name, *arity);

            let new_matrix = is_exhaustive(new_matrix, *arity + n - 1);

            new_matrix
                .into_iter()
                .map(|p_stack| recover_ctor(alts.clone(), name, *arity, p_stack))
                .collect()
        })
        .fold(Matrix::new(), |acc, m| acc.concat(m))
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
    let tipo::ValueConstructorVariant::Record {
        name,
        arity,
        ..
    } = variant else {unreachable!("variant should be a ValueConstructorVariant")};

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


