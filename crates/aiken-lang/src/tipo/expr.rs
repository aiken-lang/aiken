use std::{cmp::Ordering, collections::HashMap, sync::Arc};

use vec1::Vec1;

use crate::{
    ast::{
        Annotation, Arg, ArgName, AssignmentKind, BinOp, ByteArrayFormatPreference, CallArg,
        Clause, ClauseGuard, Constant, IfBranch, RecordUpdateSpread, Span, TraceKind, Tracing,
        TypedArg, TypedCallArg, TypedClause, TypedClauseGuard, TypedIfBranch, TypedMultiPattern,
        TypedRecordUpdateArg, UnOp, UntypedArg, UntypedClause, UntypedClauseGuard, UntypedIfBranch,
        UntypedMultiPattern, UntypedPattern, UntypedRecordUpdateArg,
    },
    builtins::{bool, byte_array, function, int, list, string, tuple},
    expr::{TypedExpr, UntypedExpr},
    format,
    tipo::fields::FieldMap,
};

use super::{
    environment::{assert_no_labeled_arguments, collapse_links, EntityKind, Environment},
    error::{Error, Warning},
    hydrator::Hydrator,
    pattern::PatternTyper,
    pipe::PipeTyper,
    PatternConstructor, RecordAccessor, Type, ValueConstructor, ValueConstructorVariant,
};

#[derive(Debug)]
pub(crate) struct ExprTyper<'a, 'b> {
    pub(crate) environment: &'a mut Environment<'b>,

    // We tweak the tracing behavior during type-check. Traces are either kept or left out of the
    // typed AST depending on this setting.
    pub(crate) tracing: Tracing,

    // Type hydrator for creating types from annotations
    pub(crate) hydrator: Hydrator,

    // We keep track of whether any ungeneralised functions have been used
    // to determine whether it is safe to generalise this expression after
    // it has been inferred.
    pub(crate) ungeneralised_function_used: bool,
}

impl<'a, 'b> ExprTyper<'a, 'b> {
    fn check_when_exhaustiveness(
        &mut self,
        subjects_count: usize,
        subjects: &[Arc<Type>],
        typed_clauses: &[Clause<TypedExpr, PatternConstructor, Arc<Type>>],
        location: Span,
    ) -> Result<(), Vec<String>> {
        // Because exhaustiveness checking in presence of multiple subjects is similar
        // to full exhaustiveness checking of tuples or other nested record patterns,
        // and we currently only do only limited exhaustiveness checking of custom types
        // at the top level of patterns, only consider case expressions with one subject.
        if subjects_count != 1 {
            return Ok(());
        }

        let subject_type = subjects
            .get(0)
            .expect("Asserted there's one case subject but found none");

        let value_typ = collapse_links(subject_type.clone());

        // Currently guards in exhaustiveness checking are assumed that they can fail,
        // so we go through all clauses and pluck out only the patterns
        // for clauses that don't have guards.
        let mut patterns = Vec::new();
        for clause in typed_clauses {
            if let Clause { guard: None, .. } = clause {
                // clause.pattern is a list of patterns for all subjects
                if let Some(pattern) = clause.pattern.get(0) {
                    patterns.push(pattern.clone());
                }

                // A clause can be built with alternative patterns as well, e.g. `Audio(_) | Text(_) ->`.
                // We're interested in all patterns so we build a flattened list.
                for alternative_pattern in &clause.alternative_patterns {
                    // clause.alternative_pattern is a list of patterns for all subjects
                    if let Some(pattern) = alternative_pattern.get(0) {
                        patterns.push(pattern.clone());
                    }
                }
            }
        }

        self.environment
            .check_exhaustiveness(patterns, value_typ, location)
    }

    pub fn do_infer_call(
        &mut self,
        fun: UntypedExpr,
        args: Vec<CallArg<UntypedExpr>>,
        location: Span,
    ) -> Result<(TypedExpr, Vec<TypedCallArg>, Arc<Type>), Error> {
        let fun = self.infer(fun)?;

        let (fun, args, typ) = self.do_infer_call_with_known_fun(fun, args, location)?;

        Ok((fun, args, typ))
    }

    pub fn do_infer_call_with_known_fun(
        &mut self,
        fun: TypedExpr,
        mut args: Vec<CallArg<UntypedExpr>>,
        location: Span,
    ) -> Result<(TypedExpr, Vec<TypedCallArg>, Arc<Type>), Error> {
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

        for (tipo, arg) in args_types.iter_mut().zip(args) {
            let CallArg {
                label,
                value,
                location,
            } = arg;

            let value = self.infer_call_argument(value, tipo.clone())?;

            arguments.push(CallArg {
                label,
                value,
                location,
            });
        }

        Ok((fun, arguments, return_type))
    }

    pub fn do_infer_fn(
        &mut self,
        args: Vec<UntypedArg>,
        expected_args: &[Arc<Type>],
        body: UntypedExpr,
        return_annotation: &Option<Annotation>,
    ) -> Result<(Vec<TypedArg>, TypedExpr), Error> {
        // Construct an initial type for each argument of the function- either an unbound
        // type variable or a type provided by an annotation.

        let mut arguments = Vec::new();

        for (i, arg) in args.into_iter().enumerate() {
            let arg = self.infer_param(arg, expected_args.get(i).cloned())?;

            arguments.push(arg);
        }

        let return_type = match return_annotation {
            Some(ann) => Some(self.type_from_annotation(ann)?),
            None => None,
        };

        self.infer_fn_with_known_types(arguments, body, return_type)
    }

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

    fn assert_assignment(&self, expr: &UntypedExpr) -> Result<(), Error> {
        if !matches!(*expr, UntypedExpr::Assignment { .. }) {
            return Err(Error::ImplicitlyDiscardedExpression {
                location: expr.location(),
            });
        }
        Ok(())
    }

    #[allow(clippy::only_used_in_recursion)]
    fn assert_no_assignment(&self, expr: &UntypedExpr) -> Result<(), Error> {
        match expr {
            UntypedExpr::Assignment { value, .. } => Err(Error::LastExpressionIsAssignment {
                location: expr.location(),
                expr: *value.clone(),
            }),
            UntypedExpr::Trace { then, .. } => self.assert_no_assignment(then),
            UntypedExpr::Fn { .. }
            | UntypedExpr::BinOp { .. }
            | UntypedExpr::ByteArray { .. }
            | UntypedExpr::Call { .. }
            | UntypedExpr::ErrorTerm { .. }
            | UntypedExpr::FieldAccess { .. }
            | UntypedExpr::If { .. }
            | UntypedExpr::Int { .. }
            | UntypedExpr::List { .. }
            | UntypedExpr::PipeLine { .. }
            | UntypedExpr::RecordUpdate { .. }
            | UntypedExpr::Sequence { .. }
            | UntypedExpr::String { .. }
            | UntypedExpr::Tuple { .. }
            | UntypedExpr::TupleIndex { .. }
            | UntypedExpr::UnOp { .. }
            | UntypedExpr::Var { .. }
            | UntypedExpr::TraceIfFalse { .. }
            | UntypedExpr::When { .. } => Ok(()),
        }
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
    pub fn infer(&mut self, expr: UntypedExpr) -> Result<TypedExpr, Error> {
        match expr {
            UntypedExpr::ErrorTerm { location } => Ok(self.infer_error_term(location)),

            UntypedExpr::Var { location, name, .. } => self.infer_var(name, location),

            UntypedExpr::Int {
                location, value, ..
            } => Ok(self.infer_int(value, location)),

            UntypedExpr::Sequence {
                expressions,
                location,
            } => self.infer_seq(location, expressions),

            UntypedExpr::Tuple {
                location, elems, ..
            } => self.infer_tuple(elems, location),

            UntypedExpr::String {
                location, value, ..
            } => Ok(self.infer_string(value, location)),

            UntypedExpr::PipeLine { expressions } => self.infer_pipeline(expressions),

            UntypedExpr::Fn {
                location,
                is_capture,
                arguments: args,
                body,
                return_annotation,
                ..
            } => self.infer_fn(args, &[], *body, is_capture, return_annotation, location),

            UntypedExpr::If {
                location,
                branches,
                final_else,
            } => self.infer_if(branches, *final_else, location),

            UntypedExpr::Assignment {
                location,
                pattern,
                value,
                kind,
                annotation,
                ..
            } => self.infer_assignment(pattern, *value, kind, &annotation, location),

            UntypedExpr::Trace {
                location,
                then,
                text,
                kind,
            } => self.infer_trace(kind, *then, location, *text),

            UntypedExpr::When {
                location,
                subjects,
                clauses,
                ..
            } => self.infer_when(subjects, clauses, location),

            UntypedExpr::List {
                location,
                elements,
                tail,
                ..
            } => self.infer_list(elements, tail, location),

            UntypedExpr::Call {
                location,
                fun,
                arguments: args,
                ..
            } => self.infer_call(*fun, args, location),

            UntypedExpr::BinOp {
                location,
                name,
                left,
                right,
                ..
            } => self.infer_binop(name, *left, *right, location),

            UntypedExpr::FieldAccess {
                location,
                label,
                container,
                ..
            } => self.infer_field_access(*container, label, location),

            UntypedExpr::TupleIndex {
                location,
                index,
                tuple,
                ..
            } => self.infer_tuple_index(*tuple, index, location),

            UntypedExpr::ByteArray {
                bytes,
                preferred_format,
                location,
            } => self.infer_bytearray(bytes, preferred_format, location),

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
            } => self.infer_un_op(location, value, op),

            UntypedExpr::TraceIfFalse { value, location } => {
                self.infer_trace_if_false(*value, location)
            }
        }
    }

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
            tipo: byte_array(),
        })
    }

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
                tipo: bool(),
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
                tipo: bool(),
            },
        };

        let text = TypedExpr::String {
            location,
            tipo: string(),
            value: format!(
                "{} ? False",
                format::Formatter::new().expr(&value).to_pretty_string(999)
            ),
        };

        let typed_value = self.infer(value)?;

        self.unify(bool(), typed_value.tipo(), typed_value.location(), false)?;

        match self.tracing {
            Tracing::NoTraces => Ok(typed_value),
            Tracing::KeepTraces => Ok(TypedExpr::If {
                location,
                branches: vec1::vec1![IfBranch {
                    condition: typed_value,
                    body: var_true,
                    location,
                }],
                final_else: Box::new(TypedExpr::Trace {
                    location,
                    tipo: bool(),
                    text: Box::new(text),
                    then: Box::new(var_false),
                }),
                tipo: bool(),
            }),
        }
    }

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

                return Ok(TypedExpr::BinOp {
                    location,
                    name,
                    tipo: bool(),
                    left: Box::new(left),
                    right: Box::new(right),
                });
            }
            BinOp::And => (bool(), bool()),
            BinOp::Or => (bool(), bool()),
            BinOp::LtInt => (int(), bool()),
            BinOp::LtEqInt => (int(), bool()),
            BinOp::GtEqInt => (int(), bool()),
            BinOp::GtInt => (int(), bool()),
            BinOp::AddInt => (int(), int()),
            BinOp::SubInt => (int(), int()),
            BinOp::MultInt => (int(), int()),
            BinOp::DivInt => (int(), int()),
            BinOp::ModInt => (int(), int()),
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
                ..
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
                })
            }
        };

        let spread = self.infer(*spread.base)?;
        let return_type = self.instantiate(ret.clone(), &mut HashMap::new());

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

    fn infer_un_op(
        &mut self,
        location: Span,
        value: Box<UntypedExpr>,
        op: UnOp,
    ) -> Result<TypedExpr, Error> {
        let value = self.infer(*value)?;

        let tipo = match op {
            UnOp::Not => bool(),
            UnOp::Negate => int(),
        };

        self.unify(tipo.clone(), value.tipo(), value.location(), false)?;

        Ok(TypedExpr::UnOp {
            location,
            value: Box::new(value),
            op,
            tipo,
        })
    }

    fn infer_field_access(
        &mut self,
        container: UntypedExpr,
        label: String,
        access_location: Span,
    ) -> Result<TypedExpr, Error> {
        // Attempt to infer the container as a record access. If that fails, we may be shadowing the name
        // of an imported module, so attempt to infer the container as a module access.
        // TODO: Remove this cloning
        match self.infer_record_access(container.clone(), label.clone(), access_location) {
            Ok(record_access) => Ok(record_access),

            Err(err) => match container {
                UntypedExpr::Var { name, location, .. } => {
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
                    imported_modules: self
                        .environment
                        .imported_modules
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

        let tipo = self.instantiate(constructor.tipo, &mut HashMap::new());

        let constructor = match &constructor.variant {
            variant @ ValueConstructorVariant::ModuleFn { name, module, .. } => {
                variant.to_module_value_constructor(Arc::clone(&tipo), module, name)
            }

            variant @ (ValueConstructorVariant::LocalVariable { .. }
            | ValueConstructorVariant::ModuleConstant { .. }
            | ValueConstructorVariant::Record { .. }) => {
                variant.to_module_value_constructor(Arc::clone(&tipo), &module_name, &label)
            }
        };

        Ok(TypedExpr::ModuleSelect {
            label,
            tipo: Arc::clone(&tipo),
            location: select_location,
            module_name,
            module_alias: module_alias.to_string(),
            constructor,
        })
    }

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

            _something_without_fields => return Err(unknown_field(vec![])),
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

        let accessor_record_type = self.instantiate(accessor_record_type, &mut type_vars);

        let tipo = self.instantiate(tipo, &mut type_vars);

        self.unify(
            accessor_record_type,
            record.tipo(),
            record.location(),
            false,
        )?;

        Ok(TypedExpr::RecordAccess {
            record,
            label,
            index,
            location,
            tipo,
        })
    }

    fn infer_param(
        &mut self,
        arg: UntypedArg,
        expected: Option<Arc<Type>>,
    ) -> Result<TypedArg, Error> {
        let Arg {
            arg_name,
            annotation,
            location,
            ..
        } = arg;

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

        Ok(Arg {
            arg_name,
            location,
            annotation,
            tipo,
        })
    }

    fn infer_assignment(
        &mut self,
        untyped_pattern: UntypedPattern,
        untyped_value: UntypedExpr,
        kind: AssignmentKind,
        annotation: &Option<Annotation>,
        location: Span,
    ) -> Result<TypedExpr, Error> {
        let typed_value =
            self.in_new_scope(|value_typer| value_typer.infer(untyped_value.clone()))?;
        let mut value_typ = typed_value.tipo();

        let value_is_data = value_typ.is_data();

        // Check that any type annotation is accurate.
        let pattern = if let Some(ann) = annotation {
            let ann_typ = self
                .type_from_annotation(ann)
                .map(|t| self.instantiate(t, &mut HashMap::new()))?;

            self.unify(
                ann_typ.clone(),
                value_typ.clone(),
                typed_value.type_defining_location(),
                (kind.is_let() && ann_typ.is_data()) || (kind.is_expect() && value_is_data),
            )?;

            value_typ = ann_typ.clone();

            // Ensure the pattern matches the type of the value
            PatternTyper::new(self.environment, &self.hydrator).unify(
                untyped_pattern.clone(),
                value_typ.clone(),
                Some(ann_typ),
            )?
        } else {
            if value_is_data && !untyped_pattern.is_var() && !untyped_pattern.is_discard() {
                return Err(Error::CastDataNoAnn {
                    location,
                    value: UntypedExpr::Assignment {
                        location,
                        value: untyped_value.into(),
                        pattern: untyped_pattern,
                        kind,
                        annotation: Some(Annotation::Constructor {
                            location: Span::empty(),
                            module: None,
                            name: "Type".to_string(),
                            arguments: vec![],
                        }),
                    },
                });
            }

            // Ensure the pattern matches the type of the value
            PatternTyper::new(self.environment, &self.hydrator).unify(
                untyped_pattern.clone(),
                value_typ.clone(),
                None,
            )?
        };

        // We currently only do limited exhaustiveness checking of custom types
        // at the top level of patterns.
        // Do not perform exhaustiveness checking if user explicitly used `assert`.
        if kind != AssignmentKind::Expect {
            if let Err(unmatched) = self.environment.check_exhaustiveness(
                vec![pattern.clone()],
                collapse_links(value_typ.clone()),
                location,
            ) {
                return Err(Error::NotExhaustivePatternMatch {
                    location,
                    unmatched,
                    is_let: true,
                });
            }
        } else if !value_is_data
            && !value_typ.is_list()
            && self
                .environment
                .check_exhaustiveness(
                    vec![pattern.clone()],
                    collapse_links(value_typ.clone()),
                    location,
                )
                .is_ok()
        {
            self.environment
                .warnings
                .push(Warning::SingleConstructorExpect {
                    location: Span {
                        start: location.start,
                        end: location.start + kind.location_offset(),
                    },
                    pattern_location: dbg!(untyped_pattern.location()),
                    value_location: dbg!(untyped_value.location()),
                    sample: UntypedExpr::Assignment {
                        location: Span::empty(),
                        value: Box::new(untyped_value),
                        pattern: untyped_pattern,
                        kind: AssignmentKind::Let,
                        annotation: None,
                    },
                })
        }

        Ok(TypedExpr::Assignment {
            location,
            tipo: value_typ,
            kind,
            pattern,
            value: Box::new(typed_value),
        })
    }

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

    fn infer_call_argument(
        &mut self,
        value: UntypedExpr,
        tipo: Arc<Type>,
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
                    ..
                },
                UntypedExpr::Fn {
                    arguments,
                    body,
                    return_annotation,
                    location,
                    is_capture: false,
                    ..
                },
            ) if expected_arguments.len() == arguments.len() => self.infer_fn(
                arguments,
                expected_arguments,
                *body,
                false,
                return_annotation,
                location,
            ),

            // Otherwise just perform normal type inference.
            (_, value) => self.infer(value),
        }?;

        self.unify(tipo.clone(), value.tipo(), value.location(), tipo.is_data())?;

        Ok(value)
    }

    fn infer_clause(
        &mut self,
        clause: UntypedClause,
        subjects: &[Arc<Type>],
    ) -> Result<TypedClause, Error> {
        let Clause {
            pattern,
            alternative_patterns,
            guard,
            then,
            location,
        } = clause;

        let (guard, then, typed_pattern, typed_alternatives) =
            self.in_new_scope(|clause_typer| {
                // Check the types
                let (typed_pattern, typed_alternatives) = clause_typer.infer_clause_pattern(
                    pattern,
                    alternative_patterns,
                    subjects,
                    &location,
                )?;

                let guard = clause_typer.infer_optional_clause_guard(guard)?;

                let then = clause_typer.infer(then)?;

                Ok::<_, Error>((guard, then, typed_pattern, typed_alternatives))
            })?;

        Ok(Clause {
            location,
            pattern: typed_pattern,
            alternative_patterns: typed_alternatives,
            guard,
            then,
        })
    }

    fn infer_clause_guard(&mut self, guard: UntypedClauseGuard) -> Result<TypedClauseGuard, Error> {
        match guard {
            ClauseGuard::Var { location, name, .. } => {
                let constructor = self.infer_value_constructor(&None, &name, &location)?;

                // We cannot support all values in guard expressions as the BEAM does not
                match &constructor.variant {
                    ValueConstructorVariant::LocalVariable { .. } => (),

                    ValueConstructorVariant::ModuleFn { .. }
                    | ValueConstructorVariant::Record { .. } => {
                        return Err(Error::NonLocalClauseGuardVariable { location, name });
                    }

                    ValueConstructorVariant::ModuleConstant { literal, .. } => {
                        return Ok(ClauseGuard::Constant(literal.clone()))
                    }
                };

                Ok(ClauseGuard::Var {
                    location,
                    name,
                    tipo: constructor.tipo,
                })
            }

            ClauseGuard::Not {
                location, value, ..
            } => {
                let value = self.infer_clause_guard(*value)?;

                self.unify(bool(), value.tipo(), value.location(), false)?;

                Ok(ClauseGuard::Not {
                    location,
                    value: Box::new(value),
                })
            }

            ClauseGuard::And {
                location,
                left,
                right,
                ..
            } => {
                let left = self.infer_clause_guard(*left)?;

                self.unify(bool(), left.tipo(), left.location(), false)?;

                let right = self.infer_clause_guard(*right)?;

                self.unify(bool(), right.tipo(), right.location(), false)?;

                Ok(ClauseGuard::And {
                    location,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            ClauseGuard::Or {
                location,
                left,
                right,
                ..
            } => {
                let left = self.infer_clause_guard(*left)?;

                self.unify(bool(), left.tipo(), left.location(), false)?;

                let right = self.infer_clause_guard(*right)?;

                self.unify(bool(), right.tipo(), right.location(), false)?;

                Ok(ClauseGuard::Or {
                    location,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            ClauseGuard::Equals {
                location,
                left,
                right,
                ..
            } => {
                let left = self.infer_clause_guard(*left)?;
                let right = self.infer_clause_guard(*right)?;

                self.unify(left.tipo(), right.tipo(), location, false)?;

                Ok(ClauseGuard::Equals {
                    location,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            ClauseGuard::NotEquals {
                location,
                left,
                right,
                ..
            } => {
                let left = self.infer_clause_guard(*left)?;
                let right = self.infer_clause_guard(*right)?;

                self.unify(left.tipo(), right.tipo(), location, false)?;

                Ok(ClauseGuard::NotEquals {
                    location,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            ClauseGuard::GtInt {
                location,
                left,
                right,
                ..
            } => {
                let left = self.infer_clause_guard(*left)?;

                self.unify(int(), left.tipo(), left.location(), false)?;

                let right = self.infer_clause_guard(*right)?;

                self.unify(int(), right.tipo(), right.location(), false)?;

                Ok(ClauseGuard::GtInt {
                    location,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            ClauseGuard::GtEqInt {
                location,
                left,
                right,
                ..
            } => {
                let left = self.infer_clause_guard(*left)?;

                self.unify(int(), left.tipo(), left.location(), false)?;

                let right = self.infer_clause_guard(*right)?;

                self.unify(int(), right.tipo(), right.location(), false)?;

                Ok(ClauseGuard::GtEqInt {
                    location,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            ClauseGuard::LtInt {
                location,
                left,
                right,
                ..
            } => {
                let left = self.infer_clause_guard(*left)?;

                self.unify(int(), left.tipo(), left.location(), false)?;

                let right = self.infer_clause_guard(*right)?;

                self.unify(int(), right.tipo(), right.location(), false)?;

                Ok(ClauseGuard::LtInt {
                    location,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            ClauseGuard::LtEqInt {
                location,
                left,
                right,
                ..
            } => {
                let left = self.infer_clause_guard(*left)?;

                self.unify(int(), left.tipo(), left.location(), false)?;

                let right = self.infer_clause_guard(*right)?;

                self.unify(int(), right.tipo(), right.location(), false)?;

                Ok(ClauseGuard::LtEqInt {
                    location,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            ClauseGuard::Constant(constant) => {
                self.infer_const(&None, constant).map(ClauseGuard::Constant)
            }
        }
    }

    fn infer_clause_pattern(
        &mut self,
        pattern: UntypedMultiPattern,
        alternatives: Vec<UntypedMultiPattern>,
        subjects: &[Arc<Type>],
        location: &Span,
    ) -> Result<(TypedMultiPattern, Vec<TypedMultiPattern>), Error> {
        let mut pattern_typer = PatternTyper::new(self.environment, &self.hydrator);

        let typed_pattern = pattern_typer.infer_multi_pattern(pattern, subjects, location)?;

        // Each case clause has one or more patterns that may match the
        // subject in order for the clause to be selected, so we must type
        // check every pattern.
        let mut typed_alternatives = Vec::with_capacity(alternatives.len());

        for m in alternatives {
            typed_alternatives
                .push(pattern_typer.infer_alternative_multi_pattern(m, subjects, location)?);
        }

        Ok((typed_pattern, typed_alternatives))
    }

    // TODO: extract the type annotation checking into a infer_module_const
    // function that uses this function internally
    pub fn infer_const(
        &mut self,
        annotation: &Option<Annotation>,
        value: Constant,
    ) -> Result<Constant, Error> {
        let inferred = match value {
            Constant::Int {
                location, value, ..
            } => Ok(Constant::Int { location, value }),

            Constant::String {
                location, value, ..
            } => Ok(Constant::String { location, value }),

            Constant::ByteArray {
                location,
                bytes,
                preferred_format,
            } => {
                let _ = self.infer_bytearray(bytes.clone(), preferred_format, location)?;
                Ok(Constant::ByteArray {
                    location,
                    bytes,
                    preferred_format,
                })
            }
        }?;

        // Check type annotation is accurate.
        if let Some(ann) = annotation {
            let const_ann = self.type_from_annotation(ann)?;

            self.unify(
                const_ann.clone(),
                inferred.tipo(),
                inferred.location(),
                const_ann.is_data(),
            )?;
        };

        Ok(inferred)
    }

    fn infer_if(
        &mut self,
        branches: Vec1<UntypedIfBranch>,
        final_else: UntypedExpr,
        location: Span,
    ) -> Result<TypedExpr, Error> {
        let first = branches.first();

        let condition = self.infer(first.condition.clone())?;

        self.unify(
            bool(),
            condition.tipo(),
            condition.type_defining_location(),
            false,
        )?;

        let body = self.in_new_scope(|body_typer| body_typer.infer(first.body.clone()))?;

        let tipo = body.tipo();

        let mut typed_branches = Vec1::new(TypedIfBranch {
            body,
            condition,
            location: first.location,
        });

        for branch in &branches[1..] {
            let condition = self.infer(branch.condition.clone())?;

            self.unify(
                bool(),
                condition.tipo(),
                condition.type_defining_location(),
                false,
            )?;

            let body = self.in_new_scope(|body_typer| body_typer.infer(branch.body.clone()))?;

            self.unify(
                tipo.clone(),
                body.tipo(),
                body.type_defining_location(),
                false,
            )?;

            typed_branches.push(TypedIfBranch {
                body,
                condition,
                location: branch.location,
            });
        }

        let typed_final_else = self.in_new_scope(|body_typer| body_typer.infer(final_else))?;

        self.unify(
            tipo.clone(),
            typed_final_else.tipo(),
            typed_final_else.type_defining_location(),
            false,
        )?;

        Ok(TypedExpr::If {
            location,
            branches: typed_branches,
            final_else: Box::new(typed_final_else),
            tipo,
        })
    }

    fn infer_fn(
        &mut self,
        args: Vec<UntypedArg>,
        expected_args: &[Arc<Type>],
        body: UntypedExpr,
        is_capture: bool,
        return_annotation: Option<Annotation>,
        location: Span,
    ) -> Result<TypedExpr, Error> {
        let (args, body) = self.do_infer_fn(args, expected_args, body, &return_annotation)?;

        let args_types = args.iter().map(|a| a.tipo.clone()).collect();

        let tipo = function(args_types, body.tipo());

        Ok(TypedExpr::Fn {
            location,
            tipo,
            is_capture,
            args,
            body: Box::new(body),
            return_annotation,
        })
    }

    pub fn infer_fn_with_known_types(
        &mut self,
        args: Vec<TypedArg>,
        body: UntypedExpr,
        return_type: Option<Arc<Type>>,
    ) -> Result<(Vec<TypedArg>, TypedExpr), Error> {
        self.assert_no_assignment(&body)?;

        let (body_rigid_names, body_infer) = self.in_new_scope(|body_typer| {
            for (arg, t) in args.iter().zip(args.iter().map(|arg| arg.tipo.clone())) {
                match &arg.arg_name {
                    ArgName::Named { name, .. } => {
                        body_typer.environment.insert_variable(
                            name.to_string(),
                            ValueConstructorVariant::LocalVariable {
                                location: arg.location,
                            },
                            t,
                        );

                        body_typer.environment.init_usage(
                            name.to_string(),
                            EntityKind::Variable,
                            arg.location,
                        );
                    }
                    ArgName::Discarded { .. } => (),
                };
            }

            (body_typer.hydrator.rigid_names(), body_typer.infer(body))
        });

        let body = body_infer.map_err(|e| e.with_unify_error_rigid_names(&body_rigid_names))?;

        // Check that any return type is accurate.
        if let Some(return_type) = return_type {
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
        }

        Ok((args, body))
    }

    fn infer_int(&mut self, value: String, location: Span) -> TypedExpr {
        TypedExpr::Int {
            location,
            value,
            tipo: int(),
        }
    }

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

        // Type check the ..tail, if there is one
        let tipo = list(tipo);

        let tail = match tail {
            Some(tail) => {
                let tail = self.infer(*tail)?;

                // Ensure the tail has the same type as the preceeding elements
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

    fn infer_optional_clause_guard(
        &mut self,
        guard: Option<UntypedClauseGuard>,
    ) -> Result<Option<TypedClauseGuard>, Error> {
        match guard {
            // If there is no guard we do nothing
            None => Ok(None),

            // If there is a guard we assert that it is of type Bool
            Some(guard) => {
                let guard = self.infer_clause_guard(guard)?;

                self.unify(bool(), guard.tipo(), guard.location(), false)?;

                Ok(Some(guard))
            }
        }
    }

    fn infer_pipeline(&mut self, expressions: Vec1<UntypedExpr>) -> Result<TypedExpr, Error> {
        PipeTyper::infer(self, expressions)
    }

    fn infer_seq(&mut self, location: Span, untyped: Vec<UntypedExpr>) -> Result<TypedExpr, Error> {
        let count = untyped.len();

        let mut expressions = Vec::with_capacity(count);

        for (i, expression) in untyped.into_iter().enumerate() {
            match i.cmp(&(count - 1)) {
                // When the expression is the last in a sequence, we enforce it is NOT
                // an assignment (kind of treat assignments like statements).
                Ordering::Equal => self.assert_no_assignment(&expression)?,

                // This isn't the final expression in the sequence, so it *must*
                // be a let-binding; we do not allow anything else.
                Ordering::Less => self.assert_assignment(&expression)?,

                // Can't actually happen
                Ordering::Greater => (),
            }

            expressions.push(self.infer(expression)?);
        }

        Ok(TypedExpr::Sequence {
            location,
            expressions,
        })
    }

    fn infer_string(&mut self, value: String, location: Span) -> TypedExpr {
        TypedExpr::String {
            location,
            value,
            tipo: string(),
        }
    }

    fn infer_tuple(&mut self, elems: Vec<UntypedExpr>, location: Span) -> Result<TypedExpr, Error> {
        let mut typed_elems = vec![];

        for elem in elems {
            let typed_elem = self.infer(elem)?;

            typed_elems.push(typed_elem);
        }

        let tipo = tuple(typed_elems.iter().map(|e| e.tipo()).collect());

        Ok(TypedExpr::Tuple {
            location,
            elems: typed_elems,
            tipo,
        })
    }

    fn infer_tuple_index(
        &mut self,
        tuple: UntypedExpr,
        index: usize,
        location: Span,
    ) -> Result<TypedExpr, Error> {
        let tuple = self.infer(tuple)?;

        let tipo = match *tuple.tipo() {
            Type::Tuple { ref elems, .. } => {
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
            _ => Err(Error::NotATuple {
                location,
                tipo: tuple.tipo(),
            }),
        }?;

        Ok(TypedExpr::TupleIndex {
            location,
            tipo,
            index,
            tuple: Box::new(tuple),
        })
    }

    fn infer_error_term(&mut self, location: Span) -> TypedExpr {
        let tipo = self.new_unbound_var();

        TypedExpr::ErrorTerm { location, tipo }
    }

    fn infer_trace(
        &mut self,
        kind: TraceKind,
        then: UntypedExpr,
        location: Span,
        text: UntypedExpr,
    ) -> Result<TypedExpr, Error> {
        let text = self.infer(text)?;
        self.unify(string(), text.tipo(), text.location(), false)?;

        let then = self.infer(then)?;
        let tipo = then.tipo();

        if let TraceKind::Todo = kind {
            self.environment.warnings.push(Warning::Todo {
                location,
                tipo: tipo.clone(),
            })
        }

        match self.tracing {
            Tracing::NoTraces => Ok(then),
            Tracing::KeepTraces => Ok(TypedExpr::Trace {
                location,
                tipo,
                then: Box::new(then),
                text: Box::new(text),
            }),
        }
    }

    fn infer_value_constructor(
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

                // Note whether we are using an ungeneralised function so that we can
                // tell if it is safe to generalise this function after inference has
                // completed.
                if matches!(
                    &constructor.variant,
                    ValueConstructorVariant::ModuleFn { .. }
                ) {
                    let is_ungeneralised = self.environment.ungeneralised_functions.contains(name);

                    self.ungeneralised_function_used =
                        self.ungeneralised_function_used || is_ungeneralised;
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
                        imported_modules: self
                            .environment
                            .imported_modules
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
        let tipo = self.instantiate(tipo, &mut HashMap::new());

        Ok(ValueConstructor {
            public,
            variant,
            tipo,
        })
    }

    fn infer_var(&mut self, name: String, location: Span) -> Result<TypedExpr, Error> {
        let constructor = self.infer_value_constructor(&None, &name, &location)?;

        Ok(TypedExpr::Var {
            constructor,
            location,
            name,
        })
    }

    fn infer_when(
        &mut self,
        subjects: Vec<UntypedExpr>,
        clauses: Vec<UntypedClause>,
        location: Span,
    ) -> Result<TypedExpr, Error> {
        // if there is only one clause we want to present a warning
        // that suggests that a `let` binding should be used instead.
        if clauses.len() == 1 {
            self.environment.warnings.push(Warning::SingleWhenClause {
                location: clauses[0].pattern[0].location(),
                sample: UntypedExpr::Assignment {
                    location: Span::empty(),
                    value: Box::new(subjects[0].clone()),
                    pattern: clauses[0].pattern[0].clone(),
                    kind: AssignmentKind::Let,
                    annotation: None,
                },
            });
        }

        let subjects_count = subjects.len();

        let mut typed_subjects = Vec::with_capacity(subjects_count);
        let mut subject_types = Vec::with_capacity(subjects_count);
        let mut typed_clauses = Vec::with_capacity(clauses.len());

        let return_type = self.new_unbound_var();

        for subject in subjects {
            let subject = self.in_new_scope(|subject_typer| {
                let subject = subject_typer.infer(subject)?;

                Ok::<_, Error>(subject)
            })?;

            subject_types.push(subject.tipo());

            typed_subjects.push(subject);
        }

        for clause in clauses {
            let typed_clause = self.infer_clause(clause, &subject_types)?;

            self.unify(
                return_type.clone(),
                typed_clause.then.tipo(),
                typed_clause.location(),
                false,
            )
            .map_err(|e| e.case_clause_mismatch())?;

            typed_clauses.push(typed_clause);
        }

        if let Err(unmatched) =
            self.check_when_exhaustiveness(subjects_count, &subject_types, &typed_clauses, location)
        {
            return Err(Error::NotExhaustivePatternMatch {
                location,
                unmatched,
                is_let: false,
            });
        }

        Ok(TypedExpr::When {
            location,
            tipo: return_type,
            subjects: typed_subjects,
            clauses: typed_clauses,
        })
    }

    fn instantiate(&mut self, t: Arc<Type>, ids: &mut HashMap<u64, Arc<Type>>) -> Arc<Type> {
        self.environment.instantiate(t, ids, &self.hydrator)
    }

    pub fn new(environment: &'a mut Environment<'b>, tracing: Tracing) -> Self {
        let mut hydrator = Hydrator::new();

        hydrator.permit_holes(true);

        Self {
            hydrator,
            environment,
            tracing,
            ungeneralised_function_used: false,
        }
    }

    pub fn new_unbound_var(&mut self) -> Arc<Type> {
        self.environment.new_unbound_var()
    }

    pub fn type_from_annotation(&mut self, annotation: &Annotation) -> Result<Arc<Type>, Error> {
        self.hydrator
            .type_from_annotation(annotation, self.environment)
    }

    fn unify(
        &mut self,
        t1: Arc<Type>,
        t2: Arc<Type>,
        location: Span,
        allow_cast: bool,
    ) -> Result<(), Error> {
        self.environment.unify(t1, t2, location, allow_cast)
    }
}
