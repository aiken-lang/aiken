use std::{collections::HashMap, sync::Arc};

use crate::{
    ast::{Annotation, ArgName, CallArg, Constant, Span, TypedArg, TypedConstant, UntypedConstant},
    builtins::list,
    expr::{TypedExpr, UntypedExpr},
    tipo::fields::FieldMap,
};

use super::{
    environment::{assert_no_labeled_arguments, EntityKind, Environment},
    error::Error,
    hydrator::Hydrator,
    ModuleValueConstructor, Type, ValueConstructor, ValueConstructorVariant,
};

#[derive(Debug)]
pub(crate) struct ExprTyper<'a, 'b> {
    pub(crate) environment: &'a mut Environment<'b>,

    // Type hydrator for creating types from annotations
    pub(crate) hydrator: Hydrator,

    // We keep track of whether any ungeneralised functions have been used
    // to determine whether it is safe to generalise this expression after
    // it has been inferred.
    pub(crate) ungeneralised_function_used: bool,
}

impl<'a, 'b> ExprTyper<'a, 'b> {
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

    pub fn infer_fn_with_known_types(
        &mut self,
        args: Vec<TypedArg>,
        body: UntypedExpr,
        return_type: Option<Arc<Type>>,
    ) -> Result<(Vec<TypedArg>, TypedExpr), Error> {
        let (body_rigid_names, body_infer) = self.in_new_scope(|body_typer| {
            for (arg, t) in args.iter().zip(args.iter().map(|arg| arg.tipo.clone())) {
                match &arg.arg_name {
                    ArgName::Named { name, .. } | ArgName::NamedLabeled { name, .. } => {
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
                    ArgName::Discard { .. } | ArgName::LabeledDiscard { .. } => (),
                };
            }

            (body_typer.hydrator.rigid_names(), body_typer.infer(body))
        });

        let body = body_infer.map_err(|e| e.with_unify_error_rigid_names(&body_rigid_names))?;

        // Check that any return type is accurate.
        if let Some(return_type) = return_type {
            self.unify(return_type, body.tipo(), body.type_defining_location())
                .map_err(|e| {
                    e.return_annotation_mismatch()
                        .with_unify_error_rigid_names(&body_rigid_names)
                })?;
        }

        Ok((args, body))
    }

    /// Crawl the AST, annotating each node with the inferred type or
    /// returning an error.
    ///
    pub fn infer(&mut self, expr: UntypedExpr) -> Result<TypedExpr, Error> {
        match expr {
            UntypedExpr::Todo {
                location,
                label,
                kind,
                ..
            } => Ok(self.infer_todo(location, kind, label)),

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

            UntypedExpr::Assignment {
                location,
                pattern,
                value,
                kind,
                annotation,
                ..
            } => self.infer_assignment(pattern, *value, kind, &annotation, location),

            UntypedExpr::Try {
                location,
                pattern,
                value,
                then,
                annotation,
                ..
            } => self.infer_try(pattern, *value, *then, &annotation, location),

            UntypedExpr::Case {
                location,
                subjects,
                clauses,
                ..
            } => self.infer_case(subjects, clauses, location),

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

            UntypedExpr::BitString { location, segments } => {
                self.infer_bit_string(segments, location)
            }

            UntypedExpr::RecordUpdate {
                location,
                constructor,
                spread,
                arguments: args,
            } => self.infer_record_update(*constructor, spread, args, location),

            UntypedExpr::Negate { location, value } => self.infer_negate(location, value),
        }
    }

    pub fn type_from_annotation(&mut self, annotation: &Annotation) -> Result<Arc<Type>, Error> {
        self.hydrator
            .type_from_annotation(annotation, self.environment)
    }

    // TODO: extract the type annotation checking into a infer_module_const
    // function that uses this function internally
    pub fn infer_const(
        &mut self,
        annotation: &Option<Annotation>,
        value: UntypedConstant,
    ) -> Result<TypedConstant, Error> {
        let inferred = match value {
            Constant::Int {
                location, value, ..
            } => Ok(Constant::Int { location, value }),

            Constant::String {
                location, value, ..
            } => Ok(Constant::String { location, value }),

            Constant::List {
                elements, location, ..
            } => self.infer_const_list(elements, location),

            Constant::ByteArray { location, bytes } => Ok(Constant::ByteArray { location, bytes }),

            Constant::Record {
                module,
                location,
                name,
                args,
                // field_map, is always None here because untyped not yet unified
                ..
            } if args.is_empty() => {
                // Register the module as having been used if it was imported
                if let Some(ref module) = &module {
                    self.environment.unused_modules.remove(module);
                }

                // Type check the record constructor
                let constructor = self.infer_value_constructor(&module, &name, &location)?;

                let (tag, field_map) = match &constructor.variant {
                    ValueConstructorVariant::Record {
                        name, field_map, ..
                    } => (name.clone(), field_map.clone()),

                    ValueConstructorVariant::ModuleFn { .. }
                    | ValueConstructorVariant::LocalVariable { .. } => {
                        return Err(Error::NonLocalClauseGuardVariable { location, name })
                    }

                    // TODO: remove this clone. Could use an rc instead
                    ValueConstructorVariant::ModuleConstant { literal, .. } => {
                        return Ok(literal.clone())
                    }
                };

                Ok(Constant::Record {
                    module,
                    location,
                    name,
                    args: vec![],
                    tipo: constructor.tipo,
                    tag,
                    field_map,
                })
            }

            Constant::Record {
                module,
                location,
                name,
                mut args,
                // field_map, is always None here because untyped not yet unified
                ..
            } => {
                // Register the module as having been used if it was imported
                if let Some(ref module) = &module {
                    self.environment.unused_modules.remove(module);
                }

                let constructor = self.infer_value_constructor(&module, &name, &location)?;

                let (tag, field_map) = match &constructor.variant {
                    ValueConstructorVariant::Record {
                        name, field_map, ..
                    } => (name.clone(), field_map.clone()),

                    ValueConstructorVariant::ModuleFn { .. }
                    | ValueConstructorVariant::LocalVariable { .. } => {
                        return Err(Error::NonLocalClauseGuardVariable { location, name })
                    }

                    // TODO: remove this clone. Could be an rc instead
                    ValueConstructorVariant::ModuleConstant { literal, .. } => {
                        return Ok(literal.clone())
                    }
                };

                // Pretty much all the other infer functions operate on UntypedExpr
                // or TypedExpr rather than ClauseGuard. To make things easier we
                // build the TypedExpr equivalent of the constructor and use that
                // TODO: resvisit this. It is rather awkward at present how we
                // have to convert to this other data structure.
                let fun = match &module {
                    Some(module_name) => {
                        let tipo = Arc::clone(&constructor.tipo);

                        let module_name = self
                            .environment
                            .imported_modules
                            .get(module_name)
                            .expect("Failed to find previously located module import")
                            .1
                            .name;

                        let module_value_constructor = ModuleValueConstructor::Record {
                            name: name.clone(),
                            field_map: field_map.clone(),
                            arity: args.len(),
                            tipo: Arc::clone(&tipo),
                            location: constructor.variant.location(),
                        };

                        TypedExpr::ModuleSelect {
                            label: name.clone(),
                            module_alias: module_name.clone(),
                            module_name,
                            tipo,
                            constructor: module_value_constructor,
                            location,
                        }
                    }

                    None => TypedExpr::Var {
                        constructor,
                        location,
                        name: name.clone(),
                    },
                };

                // This is basically the same code as do_infer_call_with_known_fun()
                // except the args are typed with infer_clause_guard() here.
                // This duplication is a bit awkward but it works!
                // Potentially this could be improved later
                match self.get_field_map(&fun, location)? {
                    // The fun has a field map so labelled arguments may be present and need to be reordered.
                    Some(field_map) => field_map.reorder(&mut args, location)?,

                    // The fun has no field map and so we error if arguments have been labelled
                    None => assert_no_labeled_arguments(&args)?,
                }

                let (mut args_types, return_type) = self.environment.match_fun_type(
                    fun.tipo(),
                    args.len(),
                    fun.location(),
                    location,
                )?;

                let mut typed_args = Vec::new();

                for (tipo, arg) in args_types.iter_mut().zip(args) {
                    let CallArg {
                        label,
                        value,
                        location,
                    } = arg;

                    let value = self.infer_const(&None, value)?;

                    self.unify(tipo.clone(), value.tipo(), value.location())?;

                    typed_args.push(CallArg {
                        label,
                        value,
                        location,
                    });
                }

                Ok(Constant::Record {
                    module,
                    location,
                    name,
                    args: typed_args,
                    tipo: return_type,
                    tag,
                    field_map,
                })
            }
            Constant::Var {
                location,
                module,
                name,
                ..
            } => {
                // Register the module as having been used if it was imported
                if let Some(ref module) = &module {
                    self.environment.unused_modules.remove(module);
                }

                // Infer the type of this constant
                let constructor = self.infer_value_constructor(&module, &name, &location)?;

                match constructor.variant {
                    ValueConstructorVariant::ModuleConstant { .. }
                    | ValueConstructorVariant::ModuleFn { .. } => Ok(Constant::Var {
                        location,
                        module,
                        name,
                        tipo: Arc::clone(&constructor.tipo),
                        constructor: Some(Box::from(constructor)),
                    }),
                    // constructor.variant cannot be a LocalVariable because module constants can
                    // only be defined at module scope. It also cannot be a Record because then
                    // this constant would have been parsed as a Constant::Record. Therefore this
                    // code is unreachable.
                    _ => unreachable!(),
                }
            }
        }?;

        // Check type annotation is accurate.
        if let Some(ann) = annotation {
            let const_ann = self.type_from_annotation(ann)?;

            self.unify(const_ann, inferred.tipo(), inferred.location())?;
        };

        Ok(inferred)
    }

    fn infer_const_list(
        &mut self,
        untyped_elements: Vec<UntypedConstant>,
        location: Span,
    ) -> Result<TypedConstant, Error> {
        let tipo = self.new_unbound_var();

        let mut elements = Vec::with_capacity(untyped_elements.len());

        for element in untyped_elements {
            let element = self.infer_const(&None, element)?;

            self.unify(tipo.clone(), element.tipo(), element.location())?;

            elements.push(element);
        }

        Ok(Constant::List {
            elements,
            location,
            tipo: list(tipo),
        })
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

    fn instantiate(&mut self, t: Arc<Type>, ids: &mut HashMap<u64, Arc<Type>>) -> Arc<Type> {
        self.environment.instantiate(t, ids, &self.hydrator)
    }

    pub fn new(environment: &'a mut Environment<'b>) -> Self {
        let mut hydrator = Hydrator::new();

        hydrator.permit_holes(true);

        Self {
            hydrator,
            environment,
            ungeneralised_function_used: false,
        }
    }

    pub fn new_unbound_var(&mut self) -> Arc<Type> {
        self.environment.new_unbound_var()
    }

    fn unify(&mut self, t1: Arc<Type>, t2: Arc<Type>, location: Span) -> Result<(), Error> {
        self.environment.unify(t1, t2, location)
    }
}
