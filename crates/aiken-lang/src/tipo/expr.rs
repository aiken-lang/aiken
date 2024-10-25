use super::{
    environment::{
        assert_no_labeled_arguments, collapse_links, generalise, EntityKind, Environment,
    },
    error::{Error, Warning},
    hydrator::Hydrator,
    pattern::PatternTyper,
    pipe::PipeTyper,
    RecordAccessor, Type, ValueConstructor, ValueConstructorVariant,
};
use crate::{
    ast::{
        self, Annotation, ArgName, AssignmentKind, AssignmentPattern, BinOp, Bls12_381Point,
        ByteArrayFormatPreference, CallArg, Curve, Function, IfBranch, LogicalOpChainKind, Pattern,
        RecordUpdateSpread, Span, TraceKind, TraceLevel, Tracing, TypedArg, TypedCallArg,
        TypedClause, TypedIfBranch, TypedPattern, TypedRecordUpdateArg, TypedValidator, UnOp,
        UntypedArg, UntypedAssignmentKind, UntypedClause, UntypedFunction, UntypedIfBranch,
        UntypedPattern, UntypedRecordUpdateArg,
    },
    builtins::{from_default_function, BUILTIN},
    expr::{FnStyle, TypedExpr, UntypedExpr},
    format,
    parser::token::Base,
    tipo::{fields::FieldMap, DefaultFunction, ModuleKind, PatternConstructor, TypeVar},
    IdGenerator,
};
use std::{
    cmp::Ordering,
    collections::{BTreeSet, HashMap},
    ops::Deref,
    rc::Rc,
};
use vec1::Vec1;

#[allow(clippy::result_large_err)]
pub(crate) fn infer_function(
    fun: &UntypedFunction,
    module_name: &str,
    hydrators: &mut HashMap<String, Hydrator>,
    environment: &mut Environment<'_>,
    tracing: Tracing,
) -> Result<Function<Rc<Type>, TypedExpr, TypedArg>, Error> {
    if let Some(typed_fun) = environment.inferred_functions.get(&fun.name) {
        return Ok(typed_fun.clone());
    };

    let Function {
        doc,
        location,
        name,
        public,
        arguments,
        body,
        return_annotation,
        end_position,
        on_test_failure,
        return_type: _,
    } = fun;

    let mut extra_let_assignments = Vec::new();
    for (i, arg) in arguments.iter().enumerate() {
        let let_assignment = arg.by.clone().into_extra_assignment(
            &arg.arg_name(i),
            arg.annotation.as_ref(),
            arg.location,
        );
        match let_assignment {
            None => {}
            Some(expr) => extra_let_assignments.push(expr),
        }
    }

    let sequence;

    let body = if extra_let_assignments.is_empty() {
        body
    } else if let UntypedExpr::Sequence { expressions, .. } = body {
        extra_let_assignments.extend(expressions.clone());
        sequence = UntypedExpr::Sequence {
            expressions: extra_let_assignments,
            location: *location,
        };
        &sequence
    } else {
        extra_let_assignments.extend([body.clone()]);
        sequence = UntypedExpr::Sequence {
            expressions: extra_let_assignments,
            location: body.location(),
        };
        &sequence
    };

    let preregistered_fn = environment
        .get_variable(name)
        .unwrap_or_else(|| panic!("Could not find preregistered type for function: {name}"));

    let field_map = preregistered_fn.field_map().cloned();

    let preregistered_type = preregistered_fn.tipo.clone();

    let (args_types, return_type) = preregistered_type
        .function_types()
        .unwrap_or_else(|| panic!("Preregistered type for fn {name} was not a fn"));

    let warnings = environment.warnings.clone();

    // ━━━ open new scope ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
    let initial_scope = environment.open_new_scope();

    let arguments = arguments
        .iter()
        .zip(&args_types)
        .enumerate()
        .map(|(ix, (arg_name, tipo))| arg_name.to_owned().set_type(tipo.clone(), ix))
        .collect();

    let hydrator = hydrators
        .remove(name)
        .unwrap_or_else(|| panic!("Could not find hydrator for fn {name}"));

    let mut expr_typer = ExprTyper::new(environment, tracing);
    expr_typer.hydrator = hydrator;
    expr_typer.not_yet_inferred = BTreeSet::from_iter(hydrators.keys().cloned());

    // Infer the type using the preregistered args + return types as a starting point
    let inferred =
        expr_typer.infer_fn_with_known_types(arguments, body.to_owned(), Some(return_type));

    // We try to always perform a deep-first inferrence. So callee are inferred before callers,
    // since this provides better -- and necessary -- information in particular with regards to
    // generics.
    //
    // In principle, the compiler requires function definitions to be processed *in order*. So if
    // A calls B, we must have inferred B before A. This is detected during inferrence, and we
    // raise an error about it. Here however, we backtrack from that error and infer the caller
    // first. Then, re-attempt to infer the current function. It may takes multiple attempts, but
    // should eventually succeed.
    //
    // Note that we need to close the scope before backtracking to not mess with the scope of the
    // callee. Otherwise, identifiers present in the caller's scope may become available to the
    // callee.
    if let Err(Error::MustInferFirst { function, .. }) = inferred {
        // Reset the environment & scope.
        hydrators.insert(name.to_string(), expr_typer.hydrator);
        environment.close_scope(initial_scope);
        *environment.warnings = warnings;

        // Backtrack and infer callee first.
        infer_function(
            &function,
            environment.current_module,
            hydrators,
            environment,
            tracing,
        )?;

        // Then, try again the entire function definition.
        return infer_function(fun, module_name, hydrators, environment, tracing);
    }

    let (arguments, body, return_type) = inferred?;

    let args_types = arguments.iter().map(|a| a.tipo.clone()).collect();

    let tipo = Type::function(args_types, return_type);

    let safe_to_generalise = !expr_typer.ungeneralised_function_used;

    environment.close_scope(initial_scope);
    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛

    // Assert that the inferred type matches the type of any recursive call
    environment.unify(preregistered_type, tipo.clone(), *location, false)?;

    // Generalise the function if safe to do so
    let tipo = if safe_to_generalise {
        environment.ungeneralised_functions.remove(name);

        let tipo = generalise(tipo, 0);

        let module_fn = ValueConstructorVariant::ModuleFn {
            name: name.clone(),
            field_map,
            module: module_name.to_owned(),
            arity: arguments.len(),
            location: *location,
            builtin: None,
        };

        environment.insert_variable(name.clone(), module_fn, tipo.clone());

        tipo
    } else {
        tipo
    };

    let inferred_fn = Function {
        doc: doc.clone(),
        location: *location,
        name: name.clone(),
        public: *public,
        arguments,
        return_annotation: return_annotation.clone(),
        return_type: tipo
            .return_type()
            .expect("Could not find return type for fn"),
        body,
        on_test_failure: on_test_failure.clone(),
        end_position: *end_position,
    };

    environment
        .inferred_functions
        .insert(name.to_string(), inferred_fn.clone());

    Ok(inferred_fn)
}

#[derive(Debug)]
pub(crate) struct ExprTyper<'a, 'b> {
    pub(crate) environment: &'a mut Environment<'b>,

    // We tweak the tracing behavior during type-check. Traces are either kept or left out of the
    // typed AST depending on this setting.
    pub(crate) tracing: Tracing,

    // Type hydrator for creating types from annotations
    pub(crate) hydrator: Hydrator,

    // A static set of remaining function names that are known but not yet inferred
    pub(crate) not_yet_inferred: BTreeSet<String>,

    // We keep track of whether any ungeneralised functions have been used
    // to determine whether it is safe to generalise this expression after
    // it has been inferred.
    pub(crate) ungeneralised_function_used: bool,
}

impl<'a, 'b> ExprTyper<'a, 'b> {
    pub fn new(environment: &'a mut Environment<'b>, tracing: Tracing) -> Self {
        Self {
            hydrator: Hydrator::new(),
            not_yet_inferred: BTreeSet::new(),
            environment,
            tracing,
            ungeneralised_function_used: false,
        }
    }

    #[allow(clippy::result_large_err)]
    fn check_when_exhaustiveness(
        &mut self,
        typed_clauses: &[TypedClause],
        location: Span,
    ) -> Result<(), Error> {
        // Currently guards in exhaustiveness checking are assumed that they can fail,
        // so we go through all clauses and pluck out only the patterns
        // for clauses that don't have guards.
        let mut patterns = Vec::new();
        for clause in typed_clauses {
            patterns.push(&clause.pattern);
        }

        self.environment
            .check_exhaustiveness(&patterns, location, false)?;

        Ok(())
    }

    #[allow(clippy::result_large_err)]
    pub fn do_infer_call(
        &mut self,
        fun: UntypedExpr,
        args: Vec<CallArg<UntypedExpr>>,
        location: Span,
    ) -> Result<(TypedExpr, Vec<TypedCallArg>, Rc<Type>), Error> {
        let fun = self.infer(fun)?;

        let (fun, args, typ) = self.do_infer_call_with_known_fun(fun, args, location, |e| e)?;

        Ok((fun, args, typ))
    }

    #[allow(clippy::result_large_err)]
    pub fn do_infer_call_with_known_fun<F>(
        &mut self,
        fun: TypedExpr,
        mut args: Vec<CallArg<UntypedExpr>>,
        location: Span,
        map_err: F,
    ) -> Result<(TypedExpr, Vec<TypedCallArg>, Rc<Type>), Error>
    where
        F: Copy + FnOnce(Error) -> Error,
    {
        // Check to see if the function accepts labelled arguments
        match self.get_field_map(&fun, location)? {
            // The fun has a field map so labelled arguments may be present and need to be reordered.
            Some(field_map) => field_map.reorder(&mut args, location)?,

            // The fun has no field map and so we error if arguments have been labelled
            None => assert_no_labeled_arguments(&args)
                .map(|(location, label)| Err(Error::UnexpectedLabeledArg { location, label }))
                .unwrap_or(Ok(()))?,
        }

        // Extract the type of the fun, ensuring it actually is a function
        let (mut args_types, return_type) =
            self.environment
                .match_fun_type(fun.tipo(), args.len(), fun.location(), location)?;

        let mut arguments = Vec::new();

        for (index, (tipo, arg)) in args_types.iter_mut().zip(args).enumerate() {
            let CallArg {
                label,
                value,
                location,
            } = arg;

            let value = self.infer_call_argument(value, tipo.clone());

            // This is so that we can annotate the error properly
            // with the pipe type mismatch situation when this is called from
            // `infer_pipeline`
            let value = if index == 0 {
                value.map_err(map_err)?
            } else {
                value?
            };

            arguments.push(CallArg {
                label,
                value,
                location,
            });
        }

        Ok((fun, arguments, return_type))
    }

    #[allow(clippy::result_large_err)]
    pub fn do_infer_fn(
        &mut self,
        args: Vec<UntypedArg>,
        expected_args: &[Rc<Type>],
        body: UntypedExpr,
        return_annotation: &Option<Annotation>,
    ) -> Result<(Vec<TypedArg>, TypedExpr, Rc<Type>), Error> {
        // Construct an initial type for each argument of the function- either an unbound
        // type variable or a type provided by an annotation.

        let mut arguments = Vec::new();

        let mut extra_let_assignments = Vec::new();
        for (i, arg) in args.into_iter().enumerate() {
            let (arg, extra_let_assignment) =
                self.infer_param(arg, expected_args.get(i).cloned(), i)?;
            if let Some(expr) = extra_let_assignment {
                extra_let_assignments.push(expr);
            }
            arguments.push(arg);
        }

        let return_type = match return_annotation {
            Some(ann) => Some(self.type_from_annotation(ann)?),
            None => None,
        };

        let body_location = body.location();

        let body = if extra_let_assignments.is_empty() {
            body
        } else if let UntypedExpr::Sequence {
            location,
            expressions,
        } = body
        {
            extra_let_assignments.extend(expressions);
            UntypedExpr::Sequence {
                expressions: extra_let_assignments,
                location,
            }
        } else {
            extra_let_assignments.extend([body]);
            UntypedExpr::Sequence {
                expressions: extra_let_assignments,
                location: body_location,
            }
        };

        self.infer_fn_with_known_types(arguments, body, return_type)
    }

    #[allow(clippy::result_large_err)]
    fn get_field_map(
        &mut self,
        constructor: &TypedExpr,
        location: Span,
    ) -> Result<Option<&FieldMap>, Error> {
        let (module, name) = match constructor {
            TypedExpr::ModuleSelect {
                module_alias,
                label,
                ..
            } => (Some(module_alias), label),

            TypedExpr::Var { name, .. } => (None, name),

            _ => return Ok(None),
        };

        Ok(self
            .environment
            .get_value_constructor(module, name, location)?
            .field_map())
    }

    pub fn in_new_scope<T>(&mut self, process_scope: impl FnOnce(&mut Self) -> T) -> T {
        // Create new scope
        let environment_reset_data = self.environment.open_new_scope();
        let hydrator_reset_data = self.hydrator.open_new_scope();

        // Process the scope
        let result = process_scope(self);

        // Close scope, discarding any scope local state
        self.environment.close_scope(environment_reset_data);
        self.hydrator.close_scope(hydrator_reset_data);

        result
    }

    /// Crawl the AST, annotating each node with the inferred type or
    /// returning an error.
    #[allow(clippy::result_large_err)]
    pub fn infer(&mut self, expr: UntypedExpr) -> Result<TypedExpr, Error> {
        match expr {
            UntypedExpr::ErrorTerm { location } => Ok(self.infer_error_term(location)),

            UntypedExpr::Var { location, name } => self.infer_var(name, location),

            UntypedExpr::UInt {
                location,
                value,
                base,
            } => Ok(self.infer_uint(value, base, location)),

            UntypedExpr::Sequence {
                expressions,
                location,
            } => self.infer_seq(location, expressions),

            UntypedExpr::Tuple { location, elems } => self.infer_tuple(elems, location),

            UntypedExpr::Pair { location, fst, snd } => self.infer_pair(*fst, *snd, location),

            UntypedExpr::String { location, value } => Ok(self.infer_string(value, location)),

            UntypedExpr::LogicalOpChain {
                kind,
                expressions,
                location,
            } => self.infer_logical_op_chain(kind, expressions, location),

            UntypedExpr::PipeLine { expressions, .. } => self.infer_pipeline(expressions),

            UntypedExpr::Fn {
                location,
                fn_style,
                arguments: args,
                body,
                return_annotation,
            } => self.infer_fn(
                args,
                &[],
                *body,
                fn_style == FnStyle::Capture,
                return_annotation,
                location,
            ),

            UntypedExpr::If {
                location,
                branches,
                final_else,
            } => self.infer_if(branches, *final_else, location),

            UntypedExpr::Assignment {
                location,
                patterns,
                value,
                kind,
            } => {
                // at this point due to backpassing rewrites,
                // patterns is guaranteed to have one item
                let AssignmentPattern {
                    pattern,
                    annotation,
                    location: _,
                } = patterns.into_vec().swap_remove(0);

                self.infer_assignment(pattern, *value, kind, &annotation, location)
            }

            UntypedExpr::Trace {
                location,
                then,
                label,
                arguments,
                kind,
                ..
            } => self.infer_trace(kind, *then, location, *label, arguments),

            UntypedExpr::When {
                location,
                subject,
                clauses,
                ..
            } => self.infer_when(*subject, clauses, location),

            UntypedExpr::List {
                location,
                elements,
                tail,
            } => self.infer_list(elements, tail, location),

            UntypedExpr::Call {
                location,
                fun,
                arguments: args,
            } => self.infer_call(*fun, args, location),

            UntypedExpr::BinOp {
                location,
                name,
                left,
                right,
            } => self.infer_binop(name, *left, *right, location),

            UntypedExpr::FieldAccess {
                location,
                label,
                container,
            } => self.infer_field_access(*container, label, location),

            UntypedExpr::TupleIndex {
                location,
                index,
                tuple,
            } => self.infer_tuple_index(*tuple, index, location),

            UntypedExpr::ByteArray {
                bytes,
                preferred_format,
                location,
            } => self.infer_bytearray(bytes, preferred_format, location),

            UntypedExpr::CurvePoint {
                location,
                point,
                preferred_format,
            } => self.infer_curve_point(*point, preferred_format, location),

            UntypedExpr::RecordUpdate {
                location,
                constructor,
                spread,
                arguments: args,
            } => self.infer_record_update(*constructor, spread, args, location),

            UntypedExpr::UnOp {
                location,
                value,
                op,
            } => self.infer_un_op(location, *value, op),

            UntypedExpr::TraceIfFalse { value, location } => {
                self.infer_trace_if_false(*value, location)
            }
        }
    }

    #[allow(clippy::result_large_err)]
    fn infer_bytearray(
        &mut self,
        bytes: Vec<u8>,
        preferred_format: ByteArrayFormatPreference,
        location: Span,
    ) -> Result<TypedExpr, Error> {
        if let ByteArrayFormatPreference::Utf8String = preferred_format {
            let value = String::from_utf8(bytes.clone()).unwrap();
            let is_hex_string = hex::decode(&value).is_ok();
            if bytes.len() >= 56 && is_hex_string {
                self.environment
                    .warnings
                    .push(Warning::Utf8ByteArrayIsValidHexString { location, value });
            }
        }

        Ok(TypedExpr::ByteArray {
            location,
            bytes,
            tipo: Type::byte_array(),
            preferred_format,
        })
    }

    #[allow(clippy::result_large_err)]
    fn infer_curve_point(
        &mut self,
        curve: Curve,
        preferred_format: ByteArrayFormatPreference,
        location: Span,
    ) -> Result<TypedExpr, Error> {
        let tipo = match curve {
            Curve::Bls12_381(point) => match point {
                Bls12_381Point::G1(_) => Type::g1_element(),
                Bls12_381Point::G2(_) => Type::g2_element(),
            },
        };

        Ok(TypedExpr::CurvePoint {
            location,
            point: curve.into(),
            tipo,
            preferred_format,
        })
    }

    #[allow(clippy::result_large_err)]
    fn infer_trace_if_false(
        &mut self,
        value: UntypedExpr,
        location: Span,
    ) -> Result<TypedExpr, Error> {
        let var_true = TypedExpr::Var {
            location,
            name: "True".to_string(),
            constructor: ValueConstructor {
                public: true,
                variant: ValueConstructorVariant::Record {
                    name: "True".to_string(),
                    arity: 0,
                    field_map: None,
                    location: Span::empty(),
                    module: String::new(),
                    constructors_count: 2,
                },
                tipo: Type::bool(),
            },
        };

        let var_false = TypedExpr::Var {
            location,
            name: "False".to_string(),
            constructor: ValueConstructor {
                public: true,
                variant: ValueConstructorVariant::Record {
                    name: "False".to_string(),
                    arity: 0,
                    field_map: None,
                    location: Span::empty(),
                    module: String::new(),
                    constructors_count: 2,
                },
                tipo: Type::bool(),
            },
        };

        let text = match self.tracing.trace_level(false) {
            TraceLevel::Verbose => Some(TypedExpr::String {
                location,
                tipo: Type::string(),
                value: format!(
                    "{} ? False",
                    format::Formatter::new()
                        .expr(&value, false)
                        .to_pretty_string(999)
                ),
            }),
            TraceLevel::Compact | TraceLevel::Silent => None,
        };

        let typed_value = self.infer(value)?;

        self.unify(
            Type::bool(),
            typed_value.tipo(),
            typed_value.location(),
            false,
        )?;

        match text {
            None => Ok(typed_value),
            Some(text) => Ok(TypedExpr::If {
                location,
                branches: vec1::vec1![IfBranch {
                    condition: typed_value,
                    body: var_true,
                    is: None,
                    location,
                }],
                final_else: Box::new(TypedExpr::Trace {
                    location,
                    tipo: Type::bool(),
                    text: Box::new(text),
                    then: Box::new(var_false),
                }),
                tipo: Type::bool(),
            }),
        }
    }

    #[allow(clippy::result_large_err)]
    fn infer_binop(
        &mut self,
        name: BinOp,
        left: UntypedExpr,
        right: UntypedExpr,
        location: Span,
    ) -> Result<TypedExpr, Error> {
        let (input_type, output_type) = match &name {
            BinOp::Eq | BinOp::NotEq => {
                let left = self.infer(left)?;

                let right = self.infer(right)?;

                self.unify(left.tipo(), right.tipo(), right.location(), false)?;

                for tipo in &[left.tipo(), right.tipo()] {
                    ensure_serialisable(false, tipo.clone(), location)
                        .map_err(|_| Error::IllegalComparison { location })?;
                }

                return Ok(TypedExpr::BinOp {
                    location,
                    name,
                    tipo: Type::bool(),
                    left: Box::new(left),
                    right: Box::new(right),
                });
            }
            BinOp::And => (Type::bool(), Type::bool()),
            BinOp::Or => (Type::bool(), Type::bool()),
            BinOp::LtInt => (Type::int(), Type::bool()),
            BinOp::LtEqInt => (Type::int(), Type::bool()),
            BinOp::GtEqInt => (Type::int(), Type::bool()),
            BinOp::GtInt => (Type::int(), Type::bool()),
            BinOp::AddInt => (Type::int(), Type::int()),
            BinOp::SubInt => (Type::int(), Type::int()),
            BinOp::MultInt => (Type::int(), Type::int()),
            BinOp::DivInt => (Type::int(), Type::int()),
            BinOp::ModInt => (Type::int(), Type::int()),
        };

        let left = self.infer(left)?;

        self.unify(
            input_type.clone(),
            left.tipo(),
            left.type_defining_location(),
            false,
        )
        .map_err(|e| e.operator_situation(name))?;

        let right = self.infer(right)?;

        self.unify(
            input_type,
            right.tipo(),
            right.type_defining_location(),
            false,
        )
        .map_err(|e| e.operator_situation(name))?;

        Ok(TypedExpr::BinOp {
            location,
            name,
            tipo: output_type,
            left: Box::new(left),
            right: Box::new(right),
        })
    }

    #[allow(clippy::result_large_err)]
    fn infer_record_update(
        &mut self,
        constructor: UntypedExpr,
        spread: RecordUpdateSpread,
        args: Vec<UntypedRecordUpdateArg>,
        location: Span,
    ) -> Result<TypedExpr, Error> {
        let (module, name): (Option<String>, String) = match self.infer(constructor.clone())? {
            TypedExpr::ModuleSelect {
                module_alias,
                label,
                ..
            } => (Some(module_alias), label),

            TypedExpr::Var { name, .. } => (None, name),

            constructor => {
                return Err(Error::RecordUpdateInvalidConstructor {
                    location: constructor.location(),
                });
            }
        };

        let value_constructor = self
            .environment
            .get_value_constructor(module.as_ref(), &name, location)?
            .clone();

        // It must be a record with a field map for us to be able to update it
        let (field_map, constructors_count) = match &value_constructor.variant {
            ValueConstructorVariant::Record {
                field_map: Some(field_map),
                constructors_count,
                name: _,
                arity: _,
                location: _,
                module: _,
            } => (field_map, *constructors_count),
            _ => {
                return Err(Error::RecordUpdateInvalidConstructor {
                    location: constructor.location(),
                });
            }
        };

        // We can only update a record if it is the only variant of its type.
        // If a record has multiple variants it cannot be safely updated as it
        // could be one of the other variants.
        if constructors_count != 1 {
            return Err(Error::UpdateMultiConstructorType {
                location: constructor.location(),
            });
        }

        // The type must be a function for it to be a record constructor
        let ret = match value_constructor.tipo.as_ref() {
            Type::Fn { ret, .. } => ret,
            _ => {
                return Err(Error::RecordUpdateInvalidConstructor {
                    location: constructor.location(),
                });
            }
        };

        let spread = self.infer(*spread.base)?;
        let return_type = self.instantiate(ret.clone(), &mut HashMap::new(), location)?;

        // Check that the spread variable unifies with the return type of the constructor
        self.unify(return_type, spread.tipo(), spread.location(), false)?;

        let mut arguments = Vec::new();

        for UntypedRecordUpdateArg {
            label,
            value,
            location,
        } in args
        {
            let value = self.infer(value.clone())?;
            let spread_field =
                self.infer_known_record_access(spread.clone(), label.to_string(), location)?;

            // Check that the update argument unifies with the corresponding
            // field in the record contained within the spread variable. We
            // need to check the spread, and not the constructor, in order
            // to handle polymorphic types.
            self.unify(
                spread_field.tipo(),
                value.tipo(),
                value.location(),
                spread_field.tipo().is_data(),
            )?;

            match field_map.fields.get(&label) {
                None => {
                    panic!("Failed to lookup record field after successfully inferring that field",)
                }
                Some((p, _)) => arguments.push(TypedRecordUpdateArg {
                    location,
                    label: label.to_string(),
                    value,
                    index: *p,
                }),
            }
        }

        if arguments.is_empty() {
            self.environment
                .warnings
                .push(Warning::NoFieldsRecordUpdate { location });
        }

        if arguments.len() == field_map.arity {
            self.environment
                .warnings
                .push(Warning::AllFieldsRecordUpdate { location });
        }

        Ok(TypedExpr::RecordUpdate {
            location,
            tipo: spread.tipo(),
            spread: Box::new(spread),
            args: arguments,
        })
    }

    #[allow(clippy::result_large_err)]
    fn infer_un_op(
        &mut self,
        location: Span,
        value: UntypedExpr,
        op: UnOp,
    ) -> Result<TypedExpr, Error> {
        let value = self.infer(value)?;

        let tipo = match op {
            UnOp::Not => Type::bool(),
            UnOp::Negate => Type::int(),
        };

        self.unify(tipo.clone(), value.tipo(), value.location(), false)?;

        Ok(TypedExpr::UnOp {
            location,
            value: Box::new(value),
            op,
            tipo,
        })
    }

    fn infer_validator_handler_access(
        &mut self,
        container: &UntypedExpr,
        label: &str,
        access_location: Span,
    ) -> Option<Result<TypedExpr, Error>> {
        match container {
            UntypedExpr::Var { name, location } => {
                if let Some((_, available_handlers)) = self
                    .environment
                    .module_validators
                    .get(name.as_str())
                    .cloned()
                {
                    return Some(
                        self.infer_var(
                            TypedValidator::handler_name(name.as_str(), label),
                            *location,
                        )
                        .map_err(|err| match err {
                            Error::UnknownVariable { .. } => Error::UnknownValidatorHandler {
                                location: access_location.map(|_start, end| (location.end, end)),
                                available_handlers,
                            },
                            _ => err,
                        }),
                    );
                }
            }
            UntypedExpr::FieldAccess {
                label: name,
                container,
                location,
            } => {
                if let UntypedExpr::Var {
                    name: ref module,
                    location: module_location,
                } = container.as_ref()
                {
                    match self.environment.imported_modules.get(module) {
                        Some((_, info)) if info.kind == ModuleKind::Validator => {
                            let has_validator = info
                                .values
                                .keys()
                                .any(|k| k.split('.').next() == Some(name));

                            let value_constructors = info
                                .values
                                .keys()
                                .map(|k| k.split('.').next().unwrap_or(k).to_string())
                                .collect::<Vec<_>>();

                            return Some(
                                self.infer_module_access(
                                    module,
                                    TypedValidator::handler_name(name, label),
                                    location,
                                    access_location,
                                )
                                .and_then(|access| {
                                    let export_values = self
                                        .environment
                                        .module_values
                                        .iter()
                                        .any(|(_, constructor)| constructor.public);
                                    let export_functions = self
                                        .environment
                                        .module_functions
                                        .iter()
                                        .any(|(_, function)| function.public);
                                    let export_validators =
                                        !self.environment.module_validators.is_empty();

                                    if export_values || export_functions || export_validators {
                                        return Err(Error::ValidatorImported {
                                            location: location
                                                .map(|_start, end| (module_location.end, end)),
                                            name: name.to_string(),
                                        });
                                    }

                                    Ok(access)
                                })
                                .map_err(|err| match err {
                                    Error::UnknownModuleValue { .. } => {
                                        if has_validator {
                                            Error::UnknownValidatorHandler {
                                                location: access_location
                                                    .map(|_start, end| (location.end, end)),
                                                available_handlers: Vec::new(),
                                            }
                                        } else {
                                            Error::UnknownModuleValue {
                                                location: location
                                                    .map(|_start, end| (module_location.end, end)),
                                                name: name.to_string(),
                                                module_name: module.to_string(),
                                                value_constructors,
                                            }
                                        }
                                    }
                                    _ => err,
                                }),
                            );
                        }
                        _ => (),
                    }
                }
            }
            _ => (),
        }

        None
    }

    #[allow(clippy::result_large_err)]
    fn infer_field_access(
        &mut self,
        container: UntypedExpr,
        label: String,
        access_location: Span,
    ) -> Result<TypedExpr, Error> {
        // NOTE: Before we actually resolve the field access, we try to short-circuit the access if
        // we detect a validator handler access. This can happen in two cases:
        //
        // - Either it is a direct access from a validator in the same module.
        // - Or it is an attempt to pull a handler from an imported validator module.
        if let Some(shortcircuit) =
            self.infer_validator_handler_access(&container, &label, access_location)
        {
            return shortcircuit;
        }

        // Attempt to infer the container as a record access. If that fails, we may be shadowing the name
        // of an imported module, so attempt to infer the container as a module access.
        // TODO: Remove this cloning
        match self.infer_record_access(container.clone(), label.clone(), access_location) {
            Ok(record_access) => Ok(record_access),

            Err(err) => match container {
                UntypedExpr::Var { name, location } => {
                    let module_access =
                        self.infer_module_access(&name, label, &location, access_location);

                    // If the name is in the environment, use the original error from
                    // inferring the record access, so that we can suggest possible
                    // misspellings of field names
                    if self.environment.scope.contains_key(&name) {
                        module_access.map_err(|_| err)
                    } else {
                        module_access
                    }
                }
                _ => Err(err),
            },
        }
    }

    #[allow(clippy::result_large_err)]
    fn infer_module_access(
        &mut self,
        module_alias: &str,
        label: String,
        module_location: &Span,
        select_location: Span,
    ) -> Result<TypedExpr, Error> {
        let (module_name, constructor) = {
            let (_, module) = self
                .environment
                .imported_modules
                .get(module_alias)
                .ok_or_else(|| Error::UnknownModule {
                    name: module_alias.to_string(),
                    location: *module_location,
                    known_modules: self
                        .environment
                        .importable_modules
                        .keys()
                        .map(|t| t.to_string())
                        .collect(),
                })?;

            let constructor =
                module
                    .values
                    .get(&label)
                    .ok_or_else(|| Error::UnknownModuleValue {
                        name: label.clone(),
                        location: Span {
                            start: module_location.end,
                            end: select_location.end,
                        },
                        module_name: module.name.clone(),
                        value_constructors: module.values.keys().map(|t| t.to_string()).collect(),
                    })?;

            // Register this imported module as having been used, to inform
            // warnings of unused imports later
            self.environment.unused_modules.remove(module_alias);

            (module.name.clone(), constructor.clone())
        };

        let tipo = self.instantiate(constructor.tipo, &mut HashMap::new(), select_location)?;

        let constructor = match &constructor.variant {
            variant @ ValueConstructorVariant::ModuleFn { name, module, .. } => {
                variant.to_module_value_constructor(Rc::clone(&tipo), module, name)
            }

            variant @ (ValueConstructorVariant::LocalVariable { .. }
            | ValueConstructorVariant::ModuleConstant { .. }
            | ValueConstructorVariant::Record { .. }) => {
                variant.to_module_value_constructor(Rc::clone(&tipo), &module_name, &label)
            }
        };

        Ok(TypedExpr::ModuleSelect {
            label,
            tipo: Rc::clone(&tipo),
            location: select_location,
            module_name,
            module_alias: module_alias.to_string(),
            constructor,
        })
    }

    #[allow(clippy::result_large_err)]
    fn infer_record_access(
        &mut self,
        record: UntypedExpr,
        label: String,
        location: Span,
    ) -> Result<TypedExpr, Error> {
        // Infer the type of the (presumed) record
        let record = self.infer(record)?;

        self.infer_known_record_access(record, label, location)
    }

    #[allow(clippy::result_large_err)]
    fn infer_known_record_access(
        &mut self,
        record: TypedExpr,
        label: String,
        location: Span,
    ) -> Result<TypedExpr, Error> {
        let record = Box::new(record);

        // If we don't yet know the type of the record then we cannot use any accessors
        if record.tipo().is_unbound() {
            return Err(Error::RecordAccessUnknownType {
                location: record.location(),
            });
        }

        // Error constructor helper function
        let unknown_field = |fields| Error::UnknownRecordField {
            situation: None,
            typ: record.tipo(),
            location,
            label: label.clone(),
            fields,
        };

        // Check to see if it's a Type that can have accessible fields
        let accessors = match collapse_links(record.tipo()).as_ref() {
            // A type in the current module which may have fields
            Type::App { module, name, .. } if module == self.environment.current_module => {
                self.environment.accessors.get(name)
            }

            // A type in another module which may have fields
            Type::App { module, name, .. } => self
                .environment
                .importable_modules
                .get(module)
                .and_then(|module| module.accessors.get(name)),

            Type::Pair { .. } => self.environment.accessors.get("Pair"),

            _something_without_fields => {
                return Err(unknown_field(vec![]));
            }
        }
        .ok_or_else(|| unknown_field(vec![]))?;

        // Find the accessor, if the type has one with the same label
        let RecordAccessor { index, label, tipo } = accessors
            .accessors
            .get(&label)
            .ok_or_else(|| {
                unknown_field(accessors.accessors.keys().map(|t| t.to_string()).collect())
            })?
            .clone();

        // Unify the record type with the accessor's stored copy of the record type.
        // This ensure that the type parameters of the retrieved value have the correct
        // types for this instance of the record.
        let accessor_record_type = accessors.tipo.clone();

        let mut type_vars = HashMap::new();

        let accessor_record_type =
            self.instantiate(accessor_record_type, &mut type_vars, record.location())?;

        let tipo = self.instantiate(tipo, &mut type_vars, record.location())?;

        self.unify(
            accessor_record_type,
            record.tipo(),
            record.location(),
            false,
        )?;

        if let Type::App { name, .. } = record.tipo().as_ref() {
            self.environment.increment_usage(name);
        };

        Ok(TypedExpr::RecordAccess {
            record,
            label,
            index,
            location,
            tipo,
        })
    }

    #[allow(clippy::result_large_err)]
    fn infer_param(
        &mut self,
        untyped_arg: UntypedArg,
        expected: Option<Rc<Type>>,
        ix: usize,
    ) -> Result<(TypedArg, Option<UntypedExpr>), Error> {
        let arg_name = untyped_arg.arg_name(ix);

        let UntypedArg {
            by,
            annotation,
            location,
            doc,
            is_validator_param,
        } = untyped_arg;

        let tipo = annotation
            .clone()
            .map(|t| self.type_from_annotation(&t))
            .unwrap_or_else(|| Ok(self.new_unbound_var()))?;

        // If we know the expected type of the argument from its contextual
        // usage then unify the newly constructed type with the expected type.
        // We do this here because then there is more type information for the
        // function being type checked, resulting in better type errors and the
        // record field access syntax working.
        if let Some(expected) = expected {
            self.unify(expected, tipo.clone(), location, false)?;
        }

        let extra_assignment = by.into_extra_assignment(&arg_name, annotation.as_ref(), location);

        let typed_arg = TypedArg {
            arg_name,
            location,
            annotation,
            tipo,
            is_validator_param,
            doc,
        };

        Ok((typed_arg, extra_assignment))
    }

    #[allow(clippy::result_large_err)]
    pub fn infer_assignment(
        &mut self,
        untyped_pattern: UntypedPattern,
        untyped_value: UntypedExpr,
        kind: UntypedAssignmentKind,
        annotation: &Option<Annotation>,
        location: Span,
    ) -> Result<TypedExpr, Error> {
        let typed_value = self.infer(untyped_value.clone())?;

        let mut value_typ = typed_value.tipo();

        let value_is_data = value_typ.is_data();

        // Check that any type annotation is accurate.
        let pattern = if let Some(ann) = annotation {
            let ann_typ = self
                .type_from_annotation(ann)
                .and_then(|t| self.instantiate(t, &mut HashMap::new(), location))?;

            self.unify(
                ann_typ.clone(),
                value_typ.clone(),
                typed_value.type_defining_location(),
                (kind.is_let() && ann_typ.is_data()) || kind.is_expect() || kind.if_is(),
            )?;

            value_typ = ann_typ.clone();

            // Ensure the pattern matches the type of the value
            PatternTyper::new(self.environment, &self.hydrator).unify(
                untyped_pattern.clone(),
                value_typ.clone(),
                Some(ann_typ),
                kind.is_let(),
            )
        } else if value_is_data && !kind.is_let() {
            let cast_data_no_ann = || {
                let ann = Annotation::Constructor {
                    location: Span::empty(),
                    module: None,
                    name: "Type".to_string(),
                    arguments: vec![],
                };

                Err(Error::CastDataNoAnn {
                    location,
                    value: UntypedExpr::Assignment {
                        location,
                        value: untyped_value.clone().into(),
                        patterns: AssignmentPattern::new(
                            untyped_pattern.clone(),
                            Some(ann),
                            Span::empty(),
                        )
                        .into(),
                        kind,
                    },
                })
            };

            if !untyped_pattern.is_var() && !untyped_pattern.is_discard() {
                let ann_typ = self.new_unbound_var();

                match PatternTyper::new(self.environment, &self.hydrator).unify(
                    untyped_pattern.clone(),
                    ann_typ.clone(),
                    None,
                    false,
                ) {
                    Ok(pattern) if ann_typ.is_monomorphic() => {
                        self.unify(
                            ann_typ.clone(),
                            value_typ.clone(),
                            typed_value.type_defining_location(),
                            true,
                        )?;

                        value_typ = ann_typ.clone();

                        Ok(pattern)
                    }
                    Ok(..) | Err(..) => cast_data_no_ann(),
                }
            } else {
                cast_data_no_ann()
            }
        } else {
            // Ensure the pattern matches the type of the value
            PatternTyper::new(self.environment, &self.hydrator).unify(
                untyped_pattern.clone(),
                value_typ.clone(),
                None,
                kind.is_let(),
            )
        }?;

        // If `expect` is explicitly used, we still check exhaustiveness but instead of returning an
        // error we emit a warning which explains that using `expect` is unnecessary.
        match kind {
            AssignmentKind::Is => {
                let pattern_var_name = match pattern {
                    Pattern::Var { ref name, .. } => Some(name),
                    _ => None,
                };

                let value_var_name = match typed_value {
                    TypedExpr::Var { ref name, .. } => Some(name),
                    _ => None,
                };

                // In case where we have no explicit pattern, we end up introducing a new let
                // binding with the same name as the value. However, the assigned value may not
                // necessarily be used, resulting in an annoying warning when one only wants to
                // assert a type.
                //
                // if foo is Int { // foo is unused here but shouldn't generated warnings.
                //   True
                // } else {
                //   False
                // }
                //
                // The following check removes the warning by marking the new let-binding as used
                // in this particular context.
                if let Some(pattern_var_name) = pattern_var_name {
                    if Some(pattern_var_name) == value_var_name {
                        self.environment.increment_usage(pattern_var_name);
                    }
                }
            }
            AssignmentKind::Let { .. } => {
                self.environment
                    .check_exhaustiveness(&[&pattern], location, true)?
            }

            AssignmentKind::Expect { .. } => {
                let is_exaustive_pattern = self
                    .environment
                    .check_exhaustiveness(&[&pattern], location, false)
                    .is_ok();

                if !value_is_data && is_exaustive_pattern && !pattern.is_discard() {
                    self.environment
                        .warnings
                        .push(Warning::SingleConstructorExpect {
                            location: Span {
                                start: location.start,
                                end: location.start + kind.location_offset(),
                            },
                            pattern_location: untyped_pattern.location(),
                            value_location: untyped_value.location(),
                            sample: match untyped_value {
                                UntypedExpr::Var { name, .. } if name == ast::BACKPASS_VARIABLE => {
                                    UntypedExpr::Assignment {
                                        location: Span::empty(),
                                        value: Box::new(UntypedExpr::Var {
                                            name: "...".to_string(),
                                            location: Span::empty(),
                                        }),
                                        patterns: AssignmentPattern::new(
                                            untyped_pattern,
                                            None,
                                            Span::empty(),
                                        )
                                        .into(),
                                        kind: AssignmentKind::Let { backpassing: true },
                                    }
                                }
                                _ => UntypedExpr::Assignment {
                                    location: Span::empty(),
                                    value: Box::new(untyped_value),
                                    patterns: AssignmentPattern::new(
                                        untyped_pattern,
                                        None,
                                        Span::empty(),
                                    )
                                    .into(),
                                    kind: AssignmentKind::let_(),
                                },
                            },
                        });
                }
            }
        }

        Ok(TypedExpr::Assignment {
            location,
            tipo: value_typ,
            kind: kind.into(),
            pattern,
            value: Box::new(typed_value),
        })
    }

    #[allow(clippy::result_large_err)]
    fn infer_call(
        &mut self,
        fun: UntypedExpr,
        args: Vec<CallArg<UntypedExpr>>,
        location: Span,
    ) -> Result<TypedExpr, Error> {
        let (fun, args, tipo) = self
            .do_infer_call(fun, args, location)
            .map_err(|e| e.call_situation())?;

        Ok(TypedExpr::Call {
            location,
            tipo,
            args,
            fun: Box::new(fun),
        })
    }

    #[allow(clippy::result_large_err)]
    fn infer_call_argument(
        &mut self,
        value: UntypedExpr,
        tipo: Rc<Type>,
    ) -> Result<TypedExpr, Error> {
        let tipo = collapse_links(tipo);

        let value = match (&*tipo, value) {
            // If the argument is expected to be a function and we are passed a
            // function literal with the correct number of arguments then we
            // have special handling of this argument, passing in information
            // about what the expected arguments are. This extra information
            // when type checking the function body means that the
            // `record.field` access syntax can be used, and improves error
            // messages.
            (
                Type::Fn {
                    args: expected_arguments,
                    ret: _,
                    alias: _,
                },
                UntypedExpr::Fn {
                    arguments,
                    body,
                    return_annotation,
                    location,
                    fn_style,
                },
            ) if fn_style != FnStyle::Capture && expected_arguments.len() == arguments.len() => {
                self.infer_fn(
                    arguments,
                    expected_arguments,
                    *body,
                    false,
                    return_annotation,
                    location,
                )
            }

            // Otherwise just perform normal type inference.
            (_, value) => self.infer(value),
        }?;

        self.unify(tipo.clone(), value.tipo(), value.location(), tipo.is_data())?;

        Ok(value)
    }

    #[allow(clippy::result_large_err)]
    fn infer_clause(
        &mut self,
        clause: UntypedClause,
        subject: &Type,
    ) -> Result<Vec<TypedClause>, Error> {
        let UntypedClause {
            patterns,
            then,
            location,
        } = clause;

        let (then, typed_patterns) = self.in_new_scope(|scope| {
            let typed_patterns = scope.infer_clause_pattern(patterns, subject, &location)?;

            let then = if let Some(filler) =
                recover_from_no_assignment(assert_no_assignment(&then), then.location())?
            {
                scope.infer(then)?.and_then(filler)
            } else {
                scope.infer(then)?
            };

            Ok::<_, Error>((then, typed_patterns))
        })?;

        Ok(typed_patterns
            .into_iter()
            .map(|pattern| TypedClause {
                location,
                pattern,
                then: then.clone(),
            })
            .collect())
    }

    #[allow(clippy::result_large_err)]
    fn infer_clause_pattern(
        &mut self,
        patterns: Vec1<UntypedPattern>,
        subject: &Type,
        location: &Span,
    ) -> Result<Vec<TypedPattern>, Error> {
        let mut pattern_typer = PatternTyper::new(self.environment, &self.hydrator);

        let mut typed_patterns = Vec::with_capacity(patterns.len());
        for (ix, pattern) in patterns.into_iter().enumerate() {
            if ix == 0 {
                typed_patterns.push(pattern_typer.infer_pattern(pattern, subject)?);
            } else {
                typed_patterns
                    .push(pattern_typer.infer_alternative_pattern(pattern, subject, location)?);
            }
        }

        Ok(typed_patterns)
    }

    #[allow(clippy::result_large_err)]
    fn infer_if(
        &mut self,
        branches: Vec1<UntypedIfBranch>,
        final_else: UntypedExpr,
        location: Span,
    ) -> Result<TypedExpr, Error> {
        let mut branches = branches.into_iter();
        let first = branches.next().unwrap();

        let first_typed_if_branch = self.infer_if_branch(first)?;

        let first_body_type = first_typed_if_branch.body.tipo();

        let mut typed_branches = vec1::vec1![first_typed_if_branch];

        for branch in branches {
            let typed_branch = self.infer_if_branch(branch)?;

            self.unify(
                first_body_type.clone(),
                typed_branch.body.tipo(),
                typed_branch.body.type_defining_location(),
                false,
            )?;

            typed_branches.push(typed_branch);
        }

        let typed_final_else = if let Some(filler) =
            recover_from_no_assignment(assert_no_assignment(&final_else), final_else.location())?
        {
            self.infer(final_else)?.and_then(filler)
        } else {
            self.infer(final_else)?
        };

        self.unify(
            first_body_type.clone(),
            typed_final_else.tipo(),
            typed_final_else.type_defining_location(),
            false,
        )?;

        Ok(TypedExpr::If {
            location,
            branches: typed_branches,
            final_else: Box::new(typed_final_else),
            tipo: first_body_type,
        })
    }

    #[allow(clippy::result_large_err)]
    fn infer_if_branch(&mut self, branch: UntypedIfBranch) -> Result<TypedIfBranch, Error> {
        let (condition, body, is) = match branch.is {
            Some(is) => self.in_new_scope(|typer| {
                let AssignmentPattern {
                    pattern,
                    annotation,
                    location,
                } = is;

                let TypedExpr::Assignment {
                    value,
                    pattern,
                    tipo,
                    ..
                } = typer.infer_assignment(
                    pattern,
                    branch.condition.clone(),
                    AssignmentKind::is(),
                    &annotation,
                    location,
                )?
                else {
                    unreachable!()
                };

                if !value.tipo().is_data() {
                    typer.environment.warnings.push(Warning::UseWhenInstead {
                        location: branch.condition.location().union(location),
                    })
                }

                let body = if let Some(filler) = recover_from_no_assignment(
                    assert_no_assignment(&branch.body),
                    branch.body.location(),
                )? {
                    typer.infer(branch.body.clone())?.and_then(filler)
                } else {
                    typer.infer(branch.body.clone())?
                };

                Ok((*value, body, Some((pattern, tipo))))
            })?,
            None => {
                let condition = self.infer(branch.condition.clone())?;

                self.unify(
                    Type::bool(),
                    condition.tipo(),
                    condition.type_defining_location(),
                    false,
                )?;

                let body = if let Some(filler) = recover_from_no_assignment(
                    assert_no_assignment(&branch.body),
                    branch.body.location(),
                )? {
                    self.infer(branch.body.clone())?.and_then(filler)
                } else {
                    self.infer(branch.body.clone())?
                };

                (condition, body, None)
            }
        };

        Ok(TypedIfBranch {
            body,
            condition,
            is,
            location: branch.location,
        })
    }

    #[allow(clippy::result_large_err)]
    pub fn infer_fn(
        &mut self,
        args: Vec<UntypedArg>,
        expected_args: &[Rc<Type>],
        body: UntypedExpr,
        is_capture: bool,
        return_annotation: Option<Annotation>,
        location: Span,
    ) -> Result<TypedExpr, Error> {
        let (args, body, return_type) =
            self.do_infer_fn(args, expected_args, body, &return_annotation)?;

        let args_types = args.iter().map(|a| a.tipo.clone()).collect();

        let tipo = Type::function(args_types, return_type);

        Ok(TypedExpr::Fn {
            location,
            tipo,
            is_capture,
            args,
            body: Box::new(body),
            return_annotation,
        })
    }

    #[allow(clippy::result_large_err)]
    pub fn infer_fn_with_known_types(
        &mut self,
        args: Vec<TypedArg>,
        body: UntypedExpr,
        return_type: Option<Rc<Type>>,
    ) -> Result<(Vec<TypedArg>, TypedExpr, Rc<Type>), Error> {
        let location = body.location();

        let no_assignment = assert_no_assignment(&body);

        let (body_rigid_names, body_infer) = self.in_new_scope(|body_typer| {
            let mut argument_names = HashMap::with_capacity(args.len());

            for arg in &args {
                match &arg.arg_name {
                    ArgName::Named { name, location, .. } if !arg.is_validator_param => {
                        if let Some(duplicate_location) = argument_names.insert(name, location) {
                            return Err(Error::DuplicateArgument {
                                location: *location,
                                duplicate_location: *duplicate_location,
                                label: name.to_string(),
                            });
                        }

                        body_typer.environment.insert_variable(
                            name.to_string(),
                            ValueConstructorVariant::LocalVariable {
                                location: arg.location,
                            },
                            arg.tipo.clone(),
                        );

                        body_typer.environment.init_usage(
                            name.to_string(),
                            EntityKind::Variable,
                            arg.location,
                        );
                    }
                    ArgName::Named { .. } | ArgName::Discarded { .. } => (),
                };
            }

            Ok((body_typer.hydrator.rigid_names(), body_typer.infer(body)))
        })?;

        let inferred_body =
            body_infer.map_err(|e| e.with_unify_error_rigid_names(&body_rigid_names));

        let body = if let Some(filler) = recover_from_no_assignment(no_assignment, location)? {
            inferred_body?.and_then(filler)
        } else {
            inferred_body?
        };

        // Check that any return type is accurate.
        let return_type = match return_type {
            Some(return_type) => {
                self.unify(
                    return_type.clone(),
                    body.tipo(),
                    body.type_defining_location(),
                    return_type.is_data(),
                )
                .map_err(|e| {
                    e.return_annotation_mismatch()
                        .with_unify_error_rigid_names(&body_rigid_names)
                })?;

                Type::with_alias(body.tipo(), return_type.alias())
            }
            None => body.tipo(),
        };

        // Ensure elements are serialisable to Data.
        ensure_serialisable(true, return_type.clone(), body.type_defining_location())?;

        Ok((args, body, return_type))
    }

    fn infer_uint(&mut self, value: String, base: Base, location: Span) -> TypedExpr {
        TypedExpr::UInt {
            location,
            value,
            tipo: Type::int(),
            base,
        }
    }

    #[allow(clippy::result_large_err)]
    fn infer_list(
        &mut self,
        elements: Vec<UntypedExpr>,
        tail: Option<Box<UntypedExpr>>,
        location: Span,
    ) -> Result<TypedExpr, Error> {
        let tipo = self.new_unbound_var();

        let mut elems = Vec::new();

        for elem in elements.into_iter() {
            let element = self.infer(elem)?;

            // Ensure they all have the same type
            self.unify(tipo.clone(), element.tipo(), location, false)?;

            elems.push(element)
        }

        // Ensure elements are serialisable to Data.
        ensure_serialisable(false, tipo.clone(), location)?;

        // Type check the ..tail, if there is one
        let tipo = Type::list(tipo);

        let tail = match tail {
            Some(tail) => {
                let tail = self.infer(*tail)?;

                // Ensure the tail has the same type as the preceding elements
                self.unify(tipo.clone(), tail.tipo(), location, false)?;

                Some(Box::new(tail))
            }
            None => None,
        };

        Ok(TypedExpr::List {
            location,
            tipo,
            elements: elems,
            tail,
        })
    }

    #[allow(clippy::result_large_err)]
    fn infer_logical_op_chain(
        &mut self,
        kind: LogicalOpChainKind,
        expressions: Vec<UntypedExpr>,
        location: Span,
    ) -> Result<TypedExpr, Error> {
        let mut typed_expressions = vec![];

        for expression in expressions {
            assert_no_assignment(&expression)?;
            let typed_expression = self.infer(expression)?;

            self.unify(
                Type::bool(),
                typed_expression.tipo(),
                typed_expression.location(),
                false,
            )?;

            typed_expressions.push(typed_expression);
        }

        if typed_expressions.len() < 2 {
            return Err(Error::LogicalOpChainMissingExpr {
                op: kind,
                location,
                missing: 2 - typed_expressions.len() as u8,
            });
        }

        let name: BinOp = kind.into();

        let chain = typed_expressions
            .into_iter()
            .rev()
            .reduce(|acc, typed_expression| TypedExpr::BinOp {
                location,
                tipo: Type::bool(),
                name,
                left: typed_expression.into(),
                right: acc.into(),
            })
            .expect("should have at least two");

        Ok(chain)
    }

    #[allow(clippy::result_large_err)]
    fn infer_pipeline(&mut self, expressions: Vec1<UntypedExpr>) -> Result<TypedExpr, Error> {
        PipeTyper::infer(self, expressions)
    }

    fn backpass(
        &mut self,
        breakpoint: UntypedExpr,
        mut continuation: Vec<UntypedExpr>,
    ) -> UntypedExpr {
        let UntypedExpr::Assignment {
            location,
            value,
            kind,
            patterns,
        } = breakpoint
        else {
            unreachable!("backpass misuse: breakpoint isn't an Assignment ?!");
        };

        let value_location = value.location();

        let call_location = Span {
            start: location.start,
            end: continuation
                .last()
                .map(|expr| expr.location().end)
                .unwrap_or_else(|| value_location.end),
        };

        let lambda_span = Span {
            start: location.end, // Start immediately after the assignment
            end: continuation
                .last()
                .map(|expr| expr.location().end)
                .unwrap_or_else(|| value_location.end),
        };

        let mut names = Vec::new();

        for (index, assignment_pattern) in patterns.into_iter().enumerate() {
            let AssignmentPattern {
                pattern,
                annotation,
                location: assignment_pattern_location,
            } = assignment_pattern;

            // In case where we have a Pattern that isn't simply a let-binding to a name, we do insert an extra let-binding
            // in front of the continuation sequence. This is because we do not support patterns in function argument
            // (which is perhaps something we should support?).
            match pattern {
                Pattern::Var {
                    name,
                    location: var_location,
                } if kind.is_let() => {
                    let name = ArgName::Named {
                        label: name.clone(),
                        name,
                        location: var_location,
                    };

                    names.push((name, var_location, annotation));
                }
                Pattern::Discard {
                    name,
                    location: var_location,
                } if kind.is_let() => {
                    let name = ArgName::Discarded {
                        label: name.clone(),
                        name,
                        location: var_location,
                    };

                    names.push((name, var_location, annotation));
                }
                _ => {
                    let name = format!("{}_{}", ast::BACKPASS_VARIABLE, index);

                    let pattern_location = pattern.location();

                    let arg_name = ArgName::Named {
                        label: name.clone(),
                        name: name.clone(),
                        location: pattern_location,
                    };

                    let pattern_is_var = pattern.is_var();

                    continuation.insert(
                        0,
                        UntypedExpr::Assignment {
                            location: assignment_pattern_location,
                            value: UntypedExpr::Var {
                                location: pattern_location,
                                name: name.clone(),
                            }
                            .into(),
                            patterns: AssignmentPattern::new(
                                pattern,
                                annotation.clone(),
                                assignment_pattern_location,
                            )
                            .into(),
                            // erase backpassing while preserving assignment kind.
                            kind: match kind {
                                AssignmentKind::Is => unreachable!(),
                                AssignmentKind::Let { .. } => AssignmentKind::let_(),
                                AssignmentKind::Expect { .. }
                                    if pattern_is_var && annotation.is_none() =>
                                {
                                    AssignmentKind::let_()
                                }
                                AssignmentKind::Expect { .. } => AssignmentKind::expect(),
                            },
                        },
                    );

                    names.push((arg_name, pattern_location, annotation));
                }
            }
        }

        match *value {
            UntypedExpr::Call {
                fun,
                arguments,
                location: _,
            } => {
                let mut new_arguments = Vec::new();
                new_arguments.extend(arguments);
                new_arguments.push(CallArg {
                    location: lambda_span,
                    label: None,
                    value: UntypedExpr::lambda(names, continuation, lambda_span),
                });

                UntypedExpr::Call {
                    location: call_location,
                    fun,
                    arguments: new_arguments,
                }
            }

            // This typically occurs on function captures. We do not try to assert anything on the
            // length of the arguments here. We defer that to the rest of the type-checker. The
            // only thing we have to do is rewrite the AST as-if someone had passed a callback.
            //
            // Now, whether this leads to an invalid call usage, that's not *our* immediate
            // problem.
            UntypedExpr::Fn {
                fn_style,
                ref arguments,
                ref return_annotation,
                location: _,
                body: _,
            } => {
                let return_annotation = return_annotation.clone();

                let arguments = arguments.iter().skip(1).cloned().collect::<Vec<_>>();

                let call = UntypedExpr::Call {
                    location: call_location,
                    fun: value,
                    arguments: vec![CallArg {
                        location: lambda_span,
                        label: None,
                        value: UntypedExpr::lambda(names, continuation, lambda_span),
                    }],
                };

                if arguments.is_empty() {
                    call
                } else {
                    UntypedExpr::Fn {
                        location: call_location,
                        fn_style,
                        arguments,
                        body: call.into(),
                        return_annotation,
                    }
                }
            }

            // Similarly to function captures, if we have any other expression we simply call it
            // with our continuation. If the expression isn't callable? No problem, the
            // type-checker will catch that eventually in exactly the same way as if the code was
            // written like that to begin with.
            _ => UntypedExpr::Call {
                location: call_location,
                fun: value,
                arguments: vec![CallArg {
                    location: lambda_span,
                    label: None,
                    value: UntypedExpr::lambda(names, continuation, lambda_span),
                }],
            },
        }
    }

    #[allow(clippy::result_large_err)]
    fn infer_seq(&mut self, location: Span, untyped: Vec<UntypedExpr>) -> Result<TypedExpr, Error> {
        // Search for backpassing.
        let mut breakpoint = None;
        let mut prefix = Vec::with_capacity(untyped.len());
        let mut suffix = Vec::with_capacity(untyped.len());
        for expression in untyped.into_iter() {
            if breakpoint.is_some() {
                suffix.push(expression);
            } else {
                match expression {
                    UntypedExpr::Assignment { kind, .. } if kind.is_backpassing() => {
                        breakpoint = Some(expression);
                    }
                    UntypedExpr::Assignment {
                        patterns,
                        location,
                        value: _,
                        kind: _,
                    } if patterns.len() > 1 => {
                        return Err(Error::UnexpectedMultiPatternAssignment {
                            arrow: patterns
                                .last()
                                .pattern
                                .location()
                                .map(|_, c_end| (c_end + 1, c_end + 1)),
                            location: patterns[1..]
                                .iter()
                                .map(|ap| ap.pattern.location())
                                .reduce(|acc, loc| acc.union(loc))
                                .unwrap_or(location),
                        });
                    }
                    _ => prefix.push(expression),
                }
            }
        }

        if let Some(breakpoint) = breakpoint {
            prefix.push(self.backpass(breakpoint, suffix));
            return self.infer_seq(location, prefix);
        }

        let sequence = self.in_new_scope(|scope| {
            let count = prefix.len();

            let mut expressions = Vec::with_capacity(count);

            for (i, expression) in prefix.into_iter().enumerate() {
                let no_assignment = assert_no_assignment(&expression);

                let typed_expression = scope.infer(expression)?;

                match i.cmp(&(count - 1)) {
                    // When the expression is the last in a sequence, we enforce it is NOT
                    // an assignment (kind of treat assignments like statements).
                    Ordering::Equal => {
                        if let Some(filler) =
                            recover_from_no_assignment(no_assignment, typed_expression.location())?
                        {
                            match typed_expression.and_then(filler) {
                                TypedExpr::Sequence {
                                    expressions: seq, ..
                                } => expressions.extend(seq),
                                trace => expressions.push(trace),
                            }
                        } else {
                            expressions.push(typed_expression);
                        }
                    }

                    // This isn't the final expression in the sequence, so it *must*
                    // be a let-binding; we do not allow anything else.
                    Ordering::Less => {
                        expressions.push(assert_assignment(typed_expression)?);
                    }

                    // Can't actually happen
                    Ordering::Greater => unreachable!(),
                }
            }

            Ok(expressions)
        })?;

        let unused = self
            .environment
            .warnings
            .iter()
            .filter_map(|w| match w {
                Warning::UnusedVariable { location, .. }
                | Warning::DiscardedLetAssignment { location, .. } => Some(*location),
                _ => None,
            })
            .collect::<Vec<_>>();

        let expressions = sequence
            .into_iter()
            .filter(|expr| {
                if let TypedExpr::Assignment { pattern, .. } = expr {
                    !unused.contains(&pattern.location())
                } else {
                    true
                }
            })
            .collect::<Vec<_>>();

        Ok(TypedExpr::Sequence {
            location,
            expressions,
        })
    }

    fn infer_string(&mut self, value: String, location: Span) -> TypedExpr {
        TypedExpr::String {
            location,
            value,
            tipo: Type::string(),
        }
    }

    #[allow(clippy::result_large_err)]
    fn infer_pair(
        &mut self,
        fst: UntypedExpr,
        snd: UntypedExpr,
        location: Span,
    ) -> Result<TypedExpr, Error> {
        let typed_fst = self.infer(fst)?;
        ensure_serialisable(false, typed_fst.tipo(), location)?;

        let typed_snd = self.infer(snd)?;
        ensure_serialisable(false, typed_snd.tipo(), location)?;

        Ok(TypedExpr::Pair {
            location,
            tipo: Type::pair(typed_fst.tipo(), typed_snd.tipo()),
            fst: typed_fst.into(),
            snd: typed_snd.into(),
        })
    }

    #[allow(clippy::result_large_err)]
    fn infer_tuple(&mut self, elems: Vec<UntypedExpr>, location: Span) -> Result<TypedExpr, Error> {
        let mut typed_elems = vec![];

        for elem in elems {
            let typed_elem = self.infer(elem)?;

            // Ensure elements are serialisable to Data.
            ensure_serialisable(false, typed_elem.tipo(), location)?;

            typed_elems.push(typed_elem);
        }

        let tipo = Type::tuple(typed_elems.iter().map(|e| e.tipo()).collect());

        Ok(TypedExpr::Tuple {
            location,
            elems: typed_elems,
            tipo,
        })
    }

    #[allow(clippy::result_large_err)]
    fn infer_tuple_index(
        &mut self,
        tuple_or_pair: UntypedExpr,
        index: usize,
        location: Span,
    ) -> Result<TypedExpr, Error> {
        let tuple_or_pair = self.infer(tuple_or_pair)?;

        let tipo = match *collapse_links(tuple_or_pair.tipo()) {
            Type::Tuple {
                ref elems,
                alias: _,
            } => {
                let size = elems.len();
                if index >= size {
                    Err(Error::TupleIndexOutOfBound {
                        location,
                        index,
                        size,
                    })
                } else {
                    Ok(elems[index].clone())
                }
            }
            Type::Pair {
                ref fst,
                ref snd,
                alias: _,
            } => {
                if index == 0 {
                    Ok(fst.clone())
                } else if index == 1 {
                    Ok(snd.clone())
                } else {
                    Err(Error::PairIndexOutOfBound { location, index })
                }
            }
            _ => Err(Error::NotIndexable {
                location,
                tipo: tuple_or_pair.tipo(),
            }),
        }?;

        Ok(TypedExpr::TupleIndex {
            location,
            tipo,
            index,
            tuple: Box::new(tuple_or_pair),
        })
    }

    fn infer_error_term(&mut self, location: Span) -> TypedExpr {
        let tipo = self.new_unbound_var();

        TypedExpr::ErrorTerm { location, tipo }
    }

    #[allow(clippy::result_large_err)]
    fn infer_trace_arg(&mut self, arg: UntypedExpr) -> Result<TypedExpr, Error> {
        let typed_arg = self.infer(arg)?;
        match self.unify(
            Type::string(),
            typed_arg.tipo(),
            typed_arg.location(),
            false,
        ) {
            Err(_) => {
                self.unify(Type::data(), typed_arg.tipo(), typed_arg.location(), true)?;
                Ok(diagnose_expr(typed_arg))
            }
            Ok(()) => Ok(typed_arg),
        }
    }

    #[allow(clippy::result_large_err)]
    fn infer_trace(
        &mut self,
        kind: TraceKind,
        then: UntypedExpr,
        location: Span,
        label: UntypedExpr,
        arguments: Vec<UntypedExpr>,
    ) -> Result<TypedExpr, Error> {
        let typed_arguments = arguments
            .into_iter()
            .map(|arg| self.infer_trace_arg(arg))
            .collect::<Result<Vec<_>, Error>>()?;

        let then = self.infer(then)?;

        let tipo = then.tipo();

        if let TraceKind::Todo = kind {
            self.environment.warnings.push(Warning::Todo {
                location,
                tipo: tipo.clone(),
            })
        }

        match self.tracing.trace_level(false) {
            TraceLevel::Silent => Ok(then),
            TraceLevel::Compact => {
                let text = self.infer(label)?;
                self.unify(Type::string(), text.tipo(), text.location(), false)?;
                Ok(TypedExpr::Trace {
                    location,
                    tipo,
                    then: Box::new(then),
                    text: Box::new(text),
                })
            }
            TraceLevel::Verbose => {
                let label = self.infer_trace_arg(label)?;

                let text = if typed_arguments.is_empty() {
                    label
                } else {
                    let delimiter = |ix| TypedExpr::String {
                        location: Span::empty(),
                        tipo: Type::string(),
                        value: if ix == 0 { ": " } else { ", " }.to_string(),
                    };
                    typed_arguments
                        .into_iter()
                        .enumerate()
                        .fold(label, |text, (ix, arg)| {
                            append_string_expr(append_string_expr(text, delimiter(ix)), arg)
                        })
                };

                Ok(TypedExpr::Trace {
                    location,
                    tipo,
                    then: Box::new(then),
                    text: Box::new(text),
                })
            }
        }
    }

    #[allow(clippy::result_large_err)]
    pub fn infer_value_constructor(
        &mut self,
        module: &Option<String>,
        name: &str,
        location: &Span,
    ) -> Result<ValueConstructor, Error> {
        let constructor = match module {
            // Look in the current scope for a binding with this name
            None => {
                let constructor =
                    self.environment
                        .get_variable(name)
                        .cloned()
                        .ok_or_else(|| Error::UnknownVariable {
                            location: *location,
                            name: name.to_string(),
                            variables: self.environment.local_value_names(),
                        })?;

                if let ValueConstructorVariant::ModuleFn { name: fn_name, .. } =
                    &constructor.variant
                {
                    // Note whether we are using an ungeneralised function so that we can
                    // tell if it is safe to generalise this function after inference has
                    // completed.
                    let is_ungeneralised = self.environment.ungeneralised_functions.contains(name);

                    self.ungeneralised_function_used =
                        self.ungeneralised_function_used || is_ungeneralised;

                    // In case we use another function, infer it first before going further.
                    // This ensures we have as much information possible about the function
                    // when we start inferring expressions using it (i.e. calls).
                    //
                    // In a way, this achieves a cheap topological processing of definitions
                    // where we infer used definitions first. And as a consequence, it solves
                    // issues where expressions would be wrongly assigned generic variables
                    // from other definitions.
                    if let Some(fun) = self.environment.module_functions.remove(fn_name) {
                        // NOTE: Recursive functions should not run into this multiple time.
                        // If we have no hydrator for this function, it means that we have already
                        // encountered it.
                        if self.not_yet_inferred.contains(&fun.name) {
                            return Err(Error::MustInferFirst {
                                function: fun.clone(),
                                location: *location,
                            });
                        }
                    }
                }

                // Register the value as seen for detection of unused values
                self.environment.increment_usage(name);

                constructor
            }

            // Look in an imported module for a binding with this name
            Some(module_name) => {
                let (_, module) = &self
                    .environment
                    .imported_modules
                    .get(module_name)
                    .ok_or_else(|| Error::UnknownModule {
                        location: *location,
                        name: module_name.to_string(),
                        known_modules: self
                            .environment
                            .importable_modules
                            .keys()
                            .map(|t| t.to_string())
                            .collect(),
                    })?;

                module
                    .values
                    .get(name)
                    .cloned()
                    .ok_or_else(|| Error::UnknownModuleValue {
                        location: *location,
                        module_name: module_name.to_string(),
                        name: name.to_string(),
                        value_constructors: module.values.keys().map(|t| t.to_string()).collect(),
                    })?
            }
        };

        let ValueConstructor {
            public,
            variant,
            tipo,
        } = constructor;

        // Instantiate generic variables into unbound variables for this usage
        let tipo = self.instantiate(tipo, &mut HashMap::new(), *location)?;

        Ok(ValueConstructor {
            public,
            variant,
            tipo,
        })
    }

    #[allow(clippy::result_large_err)]
    fn infer_var(&mut self, name: String, location: Span) -> Result<TypedExpr, Error> {
        let constructor = self.infer_value_constructor(&None, &name, &location)?;

        Ok(TypedExpr::Var {
            constructor,
            location,
            name,
        })
    }

    #[allow(clippy::result_large_err)]
    fn infer_when(
        &mut self,
        subject: UntypedExpr,
        clauses: Vec<UntypedClause>,
        location: Span,
    ) -> Result<TypedExpr, Error> {
        // if there is only one clause we want to present a warning
        // that suggests that a `let` binding should be used instead.
        let mut sample = None;

        if clauses.len() == 1 && clauses[0].patterns.len() == 1 {
            sample = Some(Warning::SingleWhenClause {
                location: clauses[0].patterns[0].location(),
                sample: UntypedExpr::Assignment {
                    location: Span::empty(),
                    value: Box::new(subject.clone()),
                    patterns: AssignmentPattern::new(
                        clauses[0].patterns[0].clone(),
                        None,
                        Span::empty(),
                    )
                    .into(),
                    kind: AssignmentKind::let_(),
                },
            });
        }

        let typed_subject = self.infer(subject)?;
        let subject_type = typed_subject.tipo();
        let return_type = self.new_unbound_var();

        let mut typed_clauses = Vec::new();
        for clause in clauses {
            for typed_clause in self.infer_clause(clause, &subject_type)? {
                self.unify(
                    return_type.clone(),
                    typed_clause.then.tipo(),
                    typed_clause.location(),
                    false,
                )
                .map_err(|e| e.case_clause_mismatch())?;

                typed_clauses.push(typed_clause)
            }
        }

        self.check_when_exhaustiveness(&typed_clauses, location)?;

        if let Some(sample) = sample {
            self.environment.warnings.push(sample);
        }

        Ok(TypedExpr::When {
            location,
            tipo: return_type,
            subject: Box::new(typed_subject),
            clauses: typed_clauses,
        })
    }

    #[allow(clippy::result_large_err)]
    fn instantiate(
        &mut self,
        t: Rc<Type>,
        ids: &mut HashMap<u64, Rc<Type>>,
        location: Span,
    ) -> Result<Rc<Type>, Error> {
        let result = self.environment.instantiate(t, ids, &self.hydrator);
        ensure_serialisable(true, result.clone(), location)?;
        Ok(result)
    }

    #[allow(clippy::result_large_err)]
    pub fn new_unbound_var(&mut self) -> Rc<Type> {
        self.environment.new_unbound_var()
    }

    #[allow(clippy::result_large_err)]
    pub fn type_from_annotation(&mut self, annotation: &Annotation) -> Result<Rc<Type>, Error> {
        self.hydrator
            .type_from_annotation(annotation, self.environment)
    }

    #[allow(clippy::result_large_err)]
    fn unify(
        &mut self,
        t1: Rc<Type>,
        t2: Rc<Type>,
        location: Span,
        allow_cast: bool,
    ) -> Result<(), Error> {
        self.environment.unify(t1, t2, location, allow_cast)
    }
}

#[allow(clippy::result_large_err)]
fn recover_from_no_assignment(
    result: Result<(), Error>,
    span: Span,
) -> Result<Option<TypedExpr>, Error> {
    if let Err(Error::LastExpressionIsAssignment {
        ref patterns,
        ref kind,
        ..
    }) = result
    {
        if matches!(kind, AssignmentKind::Expect { ..} if patterns.len() == 1) {
            return Ok(Some(TypedExpr::void(span)));
        }
    }

    result.map(|()| None)
}

#[allow(clippy::result_large_err)]
fn assert_no_assignment(expr: &UntypedExpr) -> Result<(), Error> {
    match expr {
        UntypedExpr::Assignment {
            value,
            patterns,
            kind,
            ..
        } => Err(Error::LastExpressionIsAssignment {
            location: expr.location(),
            expr: *value.clone(),
            patterns: patterns.clone(),
            kind: *kind,
        }),
        UntypedExpr::Trace { then, .. } => assert_no_assignment(then),
        UntypedExpr::Fn { .. }
        | UntypedExpr::BinOp { .. }
        | UntypedExpr::ByteArray { .. }
        | UntypedExpr::Call { .. }
        | UntypedExpr::ErrorTerm { .. }
        | UntypedExpr::FieldAccess { .. }
        | UntypedExpr::If { .. }
        | UntypedExpr::UInt { .. }
        | UntypedExpr::List { .. }
        | UntypedExpr::PipeLine { .. }
        | UntypedExpr::RecordUpdate { .. }
        | UntypedExpr::Sequence { .. }
        | UntypedExpr::String { .. }
        | UntypedExpr::Tuple { .. }
        | UntypedExpr::Pair { .. }
        | UntypedExpr::TupleIndex { .. }
        | UntypedExpr::UnOp { .. }
        | UntypedExpr::Var { .. }
        | UntypedExpr::LogicalOpChain { .. }
        | UntypedExpr::TraceIfFalse { .. }
        | UntypedExpr::When { .. }
        | UntypedExpr::CurvePoint { .. } => Ok(()),
    }
}

#[allow(clippy::result_large_err)]
fn assert_assignment(expr: TypedExpr) -> Result<TypedExpr, Error> {
    if !matches!(expr, TypedExpr::Assignment { .. }) {
        if expr.tipo().is_void() {
            return Ok(TypedExpr::Assignment {
                location: expr.location(),
                tipo: Type::void(),
                value: expr.clone().into(),
                pattern: Pattern::Constructor {
                    is_record: false,
                    location: expr.location(),
                    name: "Void".to_string(),
                    constructor: PatternConstructor::Record {
                        name: "Void".to_string(),
                        field_map: None,
                    },
                    arguments: vec![],
                    module: None,
                    spread_location: None,
                    tipo: Type::void(),
                },
                kind: AssignmentKind::let_(),
            });
        }

        return Err(Error::ImplicitlyDiscardedExpression {
            location: expr.location(),
        });
    }

    Ok(expr)
}

#[allow(clippy::result_large_err)]
pub fn ensure_serialisable(is_top_level: bool, t: Rc<Type>, location: Span) -> Result<(), Error> {
    match t.deref() {
        Type::App {
            args,
            name: _,
            module: _,
            public: _,
            contains_opaque: _,
            alias: _,
        } => {
            if !is_top_level && t.is_ml_result() {
                return Err(Error::IllegalTypeInData {
                    tipo: t.clone(),
                    location,
                });
            }

            args.iter()
                .map(|e| ensure_serialisable(false, e.clone(), location))
                .collect::<Result<Vec<_>, _>>()?;

            Ok(())
        }

        Type::Tuple { elems, alias: _ } => {
            elems
                .iter()
                .map(|e| ensure_serialisable(false, e.clone(), location))
                .collect::<Result<Vec<_>, _>>()?;

            Ok(())
        }

        Type::Fn {
            args,
            ret,
            alias: _,
        } => {
            if !is_top_level {
                return Err(Error::IllegalTypeInData {
                    tipo: t.clone(),
                    location,
                });
            }

            args.iter()
                .map(|e| ensure_serialisable(true, e.clone(), location))
                .collect::<Result<Vec<_>, _>>()?;

            ensure_serialisable(true, ret.clone(), location)
        }

        Type::Var { tipo, alias } => match tipo.borrow().deref() {
            TypeVar::Unbound { .. } => Ok(()),
            TypeVar::Generic { .. } => Ok(()),
            TypeVar::Link { tipo } => ensure_serialisable(
                is_top_level,
                Type::with_alias(tipo.clone(), alias.clone()),
                location,
            ),
        },

        Type::Pair { fst, snd, .. } => {
            ensure_serialisable(false, fst.clone(), location)?;
            ensure_serialisable(false, snd.clone(), location)
        }
    }
}

fn diagnose_expr(expr: TypedExpr) -> TypedExpr {
    // NOTE: The IdGenerator is unused. See similar note in 'append_string_expr'
    let decode_utf8_constructor =
        from_default_function(DefaultFunction::DecodeUtf8, &IdGenerator::new());

    let decode_utf8 = TypedExpr::ModuleSelect {
        location: expr.location(),
        tipo: decode_utf8_constructor.tipo.clone(),
        label: DefaultFunction::DecodeUtf8.aiken_name(),
        module_name: BUILTIN.to_string(),
        module_alias: BUILTIN.to_string(),
        constructor: decode_utf8_constructor.variant.to_module_value_constructor(
            decode_utf8_constructor.tipo,
            BUILTIN,
            &DefaultFunction::AppendString.aiken_name(),
        ),
    };

    let diagnostic = TypedExpr::Var {
        location: expr.location(),
        name: "diagnostic".to_string(),
        constructor: ValueConstructor {
            public: true,
            tipo: Type::function(vec![Type::data(), Type::byte_array()], Type::byte_array()),
            variant: ValueConstructorVariant::ModuleFn {
                name: "diagnostic".to_string(),
                field_map: None,
                module: "".to_string(),
                arity: 2,
                location: Span::empty(),
                builtin: None,
            },
        },
    };

    let location = expr.location();

    TypedExpr::Call {
        tipo: Type::string(),
        fun: Box::new(decode_utf8.clone()),
        args: vec![CallArg {
            label: None,
            location: expr.location(),
            value: TypedExpr::Call {
                tipo: Type::byte_array(),
                fun: Box::new(diagnostic.clone()),
                args: vec![
                    CallArg {
                        label: None,
                        value: expr,
                        location,
                    },
                    CallArg {
                        label: None,
                        location,
                        value: TypedExpr::ByteArray {
                            tipo: Type::byte_array(),
                            bytes: vec![],
                            location,
                            preferred_format: ByteArrayFormatPreference::HexadecimalString,
                        },
                    },
                ],
                location,
            },
        }],
        location,
    }
}

fn append_string_expr(left: TypedExpr, right: TypedExpr) -> TypedExpr {
    // NOTE: The IdGenerator is unused here, as it's only necessary for generic builtin
    // functions such as if_then_else or head_list. However, if such functions were needed,
    // passing a brand new IdGenerator here would be WRONG and cause issues down the line.
    //
    // So this is merely a small work-around for convenience. The proper way here would be to
    // pull the function definition for append_string from the pre-registered builtins
    // functions somewhere in the environment.
    let value_constructor =
        from_default_function(DefaultFunction::AppendString, &IdGenerator::new());

    let append_string = TypedExpr::ModuleSelect {
        location: Span::empty(),
        tipo: value_constructor.tipo.clone(),
        label: DefaultFunction::AppendString.aiken_name(),
        module_name: BUILTIN.to_string(),
        module_alias: BUILTIN.to_string(),
        constructor: value_constructor.variant.to_module_value_constructor(
            value_constructor.tipo,
            BUILTIN,
            &DefaultFunction::AppendString.aiken_name(),
        ),
    };

    TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::string(),
        fun: Box::new(append_string.clone()),
        args: vec![
            CallArg {
                label: None,
                location: left.location(),
                value: left,
            },
            CallArg {
                label: None,
                location: right.location(),
                value: right,
            },
        ],
    }
}
