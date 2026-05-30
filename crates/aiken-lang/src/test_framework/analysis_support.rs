use super::*;

pub(super) type FunctionIndex<'a> = HashMap<String, HashMap<String, &'a TypedFunction>>;
pub(super) type ConstantIndex<'a> = HashMap<String, HashMap<String, &'a TypedExpr>>;

#[derive(Debug, Clone)]
pub(super) struct ResolvedFunction<'a> {
    pub(super) module_name: String,
    pub(super) function_name: String,
    pub(super) function: &'a TypedFunction,
}

pub(super) fn index_known_functions<'a>(
    known_functions: &'a IndexMap<&FunctionAccessKey, &TypedFunction>,
) -> FunctionIndex<'a> {
    let mut index: FunctionIndex<'a> = HashMap::new();
    for (key, function) in known_functions {
        index
            .entry(key.module_name.clone())
            .or_default()
            .insert(key.function_name.clone(), *function);
    }
    index
}

pub(super) fn index_known_constants<'a>(
    known_constants: &'a IndexMap<&FunctionAccessKey, &TypedExpr>,
) -> ConstantIndex<'a> {
    let mut index: ConstantIndex<'a> = HashMap::new();
    for (key, expr) in known_constants {
        index
            .entry(key.module_name.clone())
            .or_default()
            .insert(key.function_name.clone(), *expr);
    }
    index
}

pub(super) fn find_function<'a>(
    function_index: &'a FunctionIndex<'a>,
    module_name: &str,
    function_name: &str,
) -> Option<&'a TypedFunction> {
    function_index.get(module_name)?.get(function_name).copied()
}

pub(super) fn pattern_var_name(pattern: &TypedPattern) -> Option<&str> {
    match pattern {
        TypedPattern::Var { name, .. } | TypedPattern::Assign { name, .. } => Some(name.as_str()),
        _ => None,
    }
}

pub(super) fn collect_call_argument_values(args: &[CallArg<TypedExpr>]) -> Vec<TypedExpr> {
    args.iter().map(|arg| arg.value.clone()).collect()
}

pub(super) fn make_synthetic_call_args(values: Vec<TypedExpr>) -> Vec<CallArg<TypedExpr>> {
    values
        .into_iter()
        .map(|value| CallArg {
            label: None,
            location: Span::empty(),
            value,
        })
        .collect()
}

/// Flatten a callable expression by resolving local aliases and collecting
/// arguments from partial applications.
pub(super) fn flatten_call_head_and_args(
    fun: &TypedExpr,
    args: &[CallArg<TypedExpr>],
    local_values: &BTreeMap<String, TypedExpr>,
) -> Option<(TypedExpr, Vec<TypedExpr>)> {
    let mut resolved_args = collect_call_argument_values(args);
    let mut current = terminal_expression(fun).clone();
    let mut visiting_local_aliases = BTreeSet::new();

    loop {
        let terminal = terminal_expression(&current).clone();
        match terminal {
            TypedExpr::Var {
                name, constructor, ..
            } if matches!(
                constructor.variant,
                ValueConstructorVariant::LocalVariable { .. }
            ) =>
            {
                let bound_expr = local_values.get(&name)?;
                if !visiting_local_aliases.insert(name) {
                    return None;
                }
                current = bound_expr.clone();
            }
            TypedExpr::Call { fun, args, .. } => {
                let mut prefix = collect_call_argument_values(&args);
                prefix.extend(resolved_args);
                resolved_args = prefix;
                current = fun.as_ref().clone();
            }
            other => return Some((other, resolved_args)),
        }
    }
}

/// Resolve a function expression while collecting/binding any pre-applied
/// arguments from partial applications and local aliases.
pub(super) fn resolve_function_with_applied_args<'a>(
    expr: &TypedExpr,
    current_module: &str,
    function_index: &'a FunctionIndex<'a>,
    local_values: &BTreeMap<String, TypedExpr>,
) -> Option<(ResolvedFunction<'a>, BTreeMap<String, TypedExpr>, usize)> {
    let (resolved_head, applied_args) = flatten_call_head_and_args(expr, &[], local_values)
        .unwrap_or_else(|| (terminal_expression(expr).clone(), Vec::new()));

    let mut visiting_local_aliases = BTreeSet::new();
    let resolved = resolve_function_from_expr(
        &resolved_head,
        current_module,
        function_index,
        local_values,
        &mut visiting_local_aliases,
    )?;

    if applied_args.len() > resolved.function.arguments.len() {
        return None;
    }

    let mut resolved_locals = local_values.clone();
    for (param, arg) in resolved.function.arguments.iter().zip(applied_args.iter()) {
        if let Some(name) = param.get_variable_name() {
            let mut visiting_local_aliases = BTreeSet::new();
            let materialized =
                materialize_local_alias_argument(arg, local_values, &mut visiting_local_aliases);
            resolved_locals.insert(name.to_string(), materialized);
        }
    }

    Some((resolved, resolved_locals, applied_args.len()))
}

pub(super) fn resolve_local_var_name_with_aliases(
    expr: &TypedExpr,
    local_values: &BTreeMap<String, TypedExpr>,
    visiting_local_aliases: &mut BTreeSet<String>,
) -> Option<String> {
    let expr = terminal_expression(expr);

    let TypedExpr::Var {
        name, constructor, ..
    } = expr
    else {
        return None;
    };

    if !matches!(
        constructor.variant,
        ValueConstructorVariant::LocalVariable { .. }
    ) {
        return None;
    }

    let Some(bound_expr) = local_values.get(name) else {
        return Some(name.clone());
    };

    if !visiting_local_aliases.insert(name.clone()) {
        return None;
    }

    let resolved =
        resolve_local_var_name_with_aliases(bound_expr, local_values, visiting_local_aliases);
    visiting_local_aliases.remove(name);
    resolved
}

pub(super) fn describe_expr(expr: &TypedExpr) -> String {
    match expr {
        TypedExpr::Call { .. } => "call".to_string(),
        TypedExpr::Var { name, .. } => format!("variable '{name}'"),
        TypedExpr::Fn { .. } => "function literal".to_string(),
        TypedExpr::Pipeline { .. } => "pipeline".to_string(),
        TypedExpr::Sequence { .. } => "sequence".to_string(),
        TypedExpr::ModuleSelect {
            module_name, label, ..
        } => {
            format!("module selection '{module_name}.{label}'")
        }
        _ => "expression".to_string(),
    }
}

/// H2 — produce a short, human-readable description of a `TypedPattern`
/// suitable for inclusion in an audit log entry. The shape mirrors the
/// way patterns appear in source: `Some(x)`, `Cons(head, tail)`,
/// `(a, _)`, etc. This is intentionally NOT a full pretty-printer: only
/// the constructor head and the immediate binders are surfaced.
///
/// Used by the `When → Or` widening site to record which patterns were
/// dropped (their constructor-tag conditional was widened to `True`)
/// in the per-clause `[E0033]` `unsupported_log` entry.
#[allow(dead_code)]
pub(super) fn describe_pattern(pat: &TypedPattern) -> String {
    match pat {
        TypedPattern::Var { name, .. } => name.clone(),
        TypedPattern::Discard { name, .. } => name.clone(),
        TypedPattern::Int { value, .. } => value.clone(),
        TypedPattern::ByteArray { .. } => "<bytearray>".to_string(),
        TypedPattern::Assign { name, pattern, .. } => {
            format!("{} as {}", describe_pattern(pattern), name)
        }
        TypedPattern::List { elements, tail, .. } => {
            let inner: Vec<String> = elements.iter().map(describe_pattern).collect();
            match tail {
                Some(t) => format!("[{}, ..{}]", inner.join(", "), describe_pattern(t)),
                None => format!("[{}]", inner.join(", ")),
            }
        }
        TypedPattern::Constructor {
            name, arguments, ..
        } => {
            if arguments.is_empty() {
                name.clone()
            } else {
                let inner: Vec<String> = arguments
                    .iter()
                    .map(|arg| describe_pattern(&arg.value))
                    .collect();
                format!("{}({})", name, inner.join(", "))
            }
        }
        TypedPattern::Pair { fst, snd, .. } => {
            format!("Pair({}, {})", describe_pattern(fst), describe_pattern(snd))
        }
        TypedPattern::Tuple { elems, .. } => {
            let inner: Vec<String> = elems.iter().map(describe_pattern).collect();
            format!("({})", inner.join(", "))
        }
    }
}

/// H2 — recursively collect all `Var` binder names introduced by a
/// pattern. Discard, literal, and bytearray patterns introduce no
/// binders. Constructor / List / Pair / Tuple patterns recurse into
/// their sub-patterns. `Assign` (e.g. `[_, _] as the_list`) contributes
/// both the assigned name and any binders inside the inner pattern.
///
/// Used by the `When → Or` widening site to record which binders were
/// dropped (i.e. would have been threaded through the precondition if
/// constructor-conditional lowering existed) in the per-clause
/// `[E0033]` `unsupported_log` entry.
pub(super) fn collect_pattern_binders(pat: &TypedPattern) -> Vec<String> {
    fn walk(pat: &TypedPattern, out: &mut Vec<String>) {
        match pat {
            TypedPattern::Var { name, .. } => out.push(name.clone()),
            TypedPattern::Discard { .. }
            | TypedPattern::Int { .. }
            | TypedPattern::ByteArray { .. } => {}
            TypedPattern::Assign { name, pattern, .. } => {
                out.push(name.clone());
                walk(pattern, out);
            }
            TypedPattern::List { elements, tail, .. } => {
                for e in elements {
                    walk(e, out);
                }
                if let Some(t) = tail {
                    walk(t, out);
                }
            }
            TypedPattern::Constructor { arguments, .. } => {
                for arg in arguments {
                    walk(&arg.value, out);
                }
            }
            TypedPattern::Pair { fst, snd, .. } => {
                walk(fst, out);
                walk(snd, out);
            }
            TypedPattern::Tuple { elems, .. } => {
                for e in elems {
                    walk(e, out);
                }
            }
        }
    }
    let mut out = Vec::new();
    walk(pat, &mut out);
    out
}

pub(super) fn materialize_local_alias_argument(
    expr: &TypedExpr,
    local_values: &BTreeMap<String, TypedExpr>,
    visiting_local_aliases: &mut BTreeSet<String>,
) -> TypedExpr {
    let expr = terminal_expression(expr);

    let TypedExpr::Var {
        name, constructor, ..
    } = expr
    else {
        return expr.clone();
    };

    if !matches!(
        constructor.variant,
        ValueConstructorVariant::LocalVariable { .. }
    ) {
        return expr.clone();
    }

    let Some(bound_expr) = local_values.get(name) else {
        return expr.clone();
    };

    if !visiting_local_aliases.insert(name.clone()) {
        return expr.clone();
    }

    let resolved =
        materialize_local_alias_argument(bound_expr, local_values, visiting_local_aliases);
    visiting_local_aliases.remove(name);
    resolved
}

#[allow(clippy::only_used_in_recursion)]
pub(super) fn resolve_function_from_expr<'a>(
    expr: &TypedExpr,
    current_module: &str,
    function_index: &'a FunctionIndex<'a>,
    local_values: &BTreeMap<String, TypedExpr>,
    visiting_local_aliases: &mut BTreeSet<String>,
) -> Option<ResolvedFunction<'a>> {
    match expr {
        TypedExpr::Var {
            name, constructor, ..
        } => match &constructor.variant {
            ValueConstructorVariant::ModuleFn { module, name, .. } => {
                let function = find_function(function_index, module, name)?;
                Some(ResolvedFunction {
                    module_name: module.clone(),
                    function_name: name.clone(),
                    function,
                })
            }
            ValueConstructorVariant::LocalVariable { .. } => {
                if let Some(bound_expr) = local_values.get(name) {
                    if !visiting_local_aliases.insert(name.clone()) {
                        return None;
                    }
                    let result = resolve_function_from_expr(
                        bound_expr,
                        current_module,
                        function_index,
                        local_values,
                        visiting_local_aliases,
                    );
                    visiting_local_aliases.remove(name);
                    result
                } else {
                    None
                }
            }
            _ => None,
        },
        TypedExpr::ModuleSelect {
            constructor: ModuleValueConstructor::Fn { module, name, .. },
            ..
        } => {
            let function = find_function(function_index, module, name)?;
            Some(ResolvedFunction {
                module_name: module.clone(),
                function_name: name.clone(),
                function,
            })
        }
        _ => None,
    }
}

#[cfg(test)]
pub(super) fn map2_mapper_arg_order(
    mapper: &TypedExpr,
    current_module: &str,
    function_index: &FunctionIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
) -> Option<[usize; 2]> {
    let order = mapn_mapper_arg_order(mapper, 2, current_module, function_index, local_values)?;
    let [first, second] = order.as_slice() else {
        return None;
    };
    Some([*first, *second])
}

pub(super) fn mapn_mapper_arg_order(
    mapper: &TypedExpr,
    arity: usize,
    current_module: &str,
    function_index: &FunctionIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
) -> Option<Vec<usize>> {
    if arity < 2 {
        return None;
    }

    let mut mapper_expr = terminal_expression(mapper).clone();
    let mut mapper_module = current_module.to_string();
    let mut mapper_locals = local_values.clone();
    let mut visiting_functions = BTreeSet::new();

    loop {
        let mapper = terminal_expression(&mapper_expr);
        match mapper {
            TypedExpr::Fn { args, body, .. } => {
                return mapn_tuple_arg_order(args, body, arity, &mapper_locals);
            }
            _ => {
                let (resolved, resolved_locals, applied_arg_count) =
                    resolve_function_with_applied_args(
                        mapper,
                        &mapper_module,
                        function_index,
                        &mapper_locals,
                    )?;
                let key = (resolved.module_name.clone(), resolved.function_name.clone());
                if !visiting_functions.insert(key) {
                    return None;
                }

                let remaining_args = resolved
                    .function
                    .arguments
                    .len()
                    .saturating_sub(applied_arg_count);

                if remaining_args == arity {
                    return mapn_tuple_arg_order(
                        &resolved.function.arguments[applied_arg_count..],
                        &resolved.function.body,
                        arity,
                        &resolved_locals,
                    );
                }

                if remaining_args == 0 {
                    mapper_expr = resolved.function.body.clone();
                    mapper_module = resolved.module_name;
                    mapper_locals = resolved_locals;
                    continue;
                }

                return None;
            }
        }
    }
}

pub(super) fn mapn_tuple_arg_order(
    args: &[TypedArg],
    body: &TypedExpr,
    arity: usize,
    local_values: &BTreeMap<String, TypedExpr>,
) -> Option<Vec<usize>> {
    if args.len() != arity {
        return None;
    }
    let arg_names: Vec<String> = args
        .iter()
        .map(|arg| arg.get_variable_name().map(|name| name.to_string()))
        .collect::<Option<Vec<_>>>()?;

    let body = terminal_expression(body);
    let TypedExpr::Tuple { elems, .. } = body else {
        return None;
    };
    if elems.len() != arity {
        return None;
    }

    let mut seen = vec![false; arity];
    let mut order = Vec::with_capacity(arity);

    for elem in elems {
        let index = tuple_elem_arg_index_by_names(elem, &arg_names, local_values)?;
        if seen[index] {
            return None;
        }
        seen[index] = true;
        order.push(index);
    }

    Some(order)
}

pub(super) fn summarize_nary_mapper_shape(
    mapper: &TypedExpr,
    arity: usize,
    current_module: &str,
    function_index: &FunctionIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
) -> Option<NaryMapperShape> {
    if arity < 2 {
        return None;
    }

    let mut mapper_expr = terminal_expression(mapper).clone();
    let mut mapper_module = current_module.to_string();
    let mut mapper_locals = local_values.clone();
    let mut visiting_functions = BTreeSet::new();

    loop {
        let mapper = terminal_expression(&mapper_expr);
        match mapper {
            TypedExpr::Fn { args, body, .. } => {
                return summarize_nary_mapper_body(args, body, arity, &mapper_locals);
            }
            _ => {
                let (resolved, resolved_locals, applied_arg_count) =
                    resolve_function_with_applied_args(
                        mapper,
                        &mapper_module,
                        function_index,
                        &mapper_locals,
                    )?;
                let key = (resolved.module_name.clone(), resolved.function_name.clone());
                if !visiting_functions.insert(key) {
                    return None;
                }

                let remaining_args = resolved
                    .function
                    .arguments
                    .len()
                    .saturating_sub(applied_arg_count);

                if remaining_args == arity {
                    return summarize_nary_mapper_body(
                        &resolved.function.arguments[applied_arg_count..],
                        &resolved.function.body,
                        arity,
                        &resolved_locals,
                    );
                }

                if remaining_args == 0 {
                    mapper_expr = resolved.function.body.clone();
                    mapper_module = resolved.module_name;
                    mapper_locals = resolved_locals;
                    continue;
                }

                return None;
            }
        }
    }
}

pub(super) fn summarize_nary_mapper_body(
    args: &[TypedArg],
    body: &TypedExpr,
    arity: usize,
    local_values: &BTreeMap<String, TypedExpr>,
) -> Option<NaryMapperShape> {
    if args.len() != arity {
        return None;
    }
    let arg_names: Vec<String> = args
        .iter()
        .map(|arg| arg.get_variable_name().map(|name| name.to_string()))
        .collect::<Option<Vec<_>>>()?;

    let TypedExpr::Call {
        fun,
        args: call_args,
        ..
    } = terminal_expression(body)
    else {
        return None;
    };
    if call_args.len() != arity {
        return None;
    }

    let mut seen = vec![false; arity];
    let mut arg_order = Vec::with_capacity(arity);
    for arg in call_args {
        let index = tuple_elem_arg_index_by_names(&arg.value, &arg_names, local_values)?;
        if seen[index] {
            return None;
        }
        seen[index] = true;
        arg_order.push(index);
    }

    let constructor = match terminal_expression(fun.as_ref()) {
        TypedExpr::Var {
            name, constructor, ..
        } => match &constructor.variant {
            ValueConstructorVariant::Record {
                arity: ctor_arity, ..
            } if usize::from(*ctor_arity) == arity => Some(name.clone()),
            _ => None,
        },
        TypedExpr::ModuleSelect {
            label, constructor, ..
        } => match constructor {
            ModuleValueConstructor::Record {
                arity: ctor_arity, ..
            } if usize::from(*ctor_arity) == arity => Some(label.clone()),
            _ => None,
        },
        _ => None,
    }?;

    Some(NaryMapperShape::ConstructorApply {
        constructor,
        type_name: data_with_schema_type_name(body.tipo().as_ref()),
        arg_order,
    })
}

pub(super) fn tuple_elem_arg_index_by_names(
    elem: &TypedExpr,
    arg_names: &[String],
    local_values: &BTreeMap<String, TypedExpr>,
) -> Option<usize> {
    let elem = terminal_expression(elem);
    if let TypedExpr::Var {
        name, constructor, ..
    } = elem
        && matches!(
            constructor.variant,
            ValueConstructorVariant::LocalVariable { .. }
        )
        && let Some(index) = arg_names.iter().position(|arg_name| arg_name == name)
    {
        return Some(index);
    }

    let mut visiting_local_aliases = BTreeSet::new();
    let name =
        resolve_local_var_name_with_aliases(elem, local_values, &mut visiting_local_aliases)?;

    arg_names.iter().position(|arg_name| arg_name == &name)
}

pub(super) fn terminal_expression(mut expr: &TypedExpr) -> &TypedExpr {
    loop {
        match expr {
            TypedExpr::Pipeline { expressions, .. } | TypedExpr::Sequence { expressions, .. } => {
                if let Some(last) = expressions.last() {
                    expr = last;
                } else {
                    return expr;
                }
            }
            _ => return expr,
        }
    }
}

/// Collect the set of free `Var` names referenced anywhere in `expr`.
///
/// Used by `translate_bind` to gate the self-referential
/// `local_values[binder] -> source` insertion: if `source` mentions
/// `binder` (free), inserting the alias would create a self-referential
/// substitution entry that the `Var`-lookup arm of
/// `typed_expr_to_transition_prop` recurses on indefinitely
/// (stack-overflowing the lowering on legal Aiken code such as
/// `let g = some_fuzzer; and_then(g, fn(g) { g })`).
///
/// Binding-introducing variants subtract their bound names from the
/// inner walk:
///   - `Fn { args, body }` subtracts each `arg.name` from the body's
///     free-vars (a lambda parameter is locally bound).
///   - `When` clause patterns subtract the pattern's binders from the
///     clause body's free-vars.
///   - `Assignment { pattern, value }` subtracts the pattern binders
///     from any *subsequent* expressions in a `Sequence`/`Pipeline`,
///     but NOT from the `value` itself (the binding is recursive in
///     name-resolution but `let` is not let-rec in Aiken). The
///     assignment's own value still counts its free vars.
///
/// Variants without sub-expressions (literals, `ErrorTerm`,
/// `ModuleSelect`, non-`LocalVariable` `Var` like module fns / record
/// constructors) contribute no free vars.
pub(super) fn free_vars_in_typed_expr(expr: &TypedExpr) -> BTreeSet<String> {
    let mut out = BTreeSet::new();
    collect_free_vars(expr, &mut out);
    out
}

pub(super) fn collect_free_vars(expr: &TypedExpr, out: &mut BTreeSet<String>) {
    match expr {
        // Literals — no free vars.
        TypedExpr::UInt { .. }
        | TypedExpr::String { .. }
        | TypedExpr::ByteArray { .. }
        | TypedExpr::CurvePoint { .. }
        | TypedExpr::ErrorTerm { .. } => {}

        // Module-level references contribute no *local* free vars.
        // Only `LocalVariable` references are meaningful for the
        // self-reference check (binder names always resolve as
        // `LocalVariable`).
        TypedExpr::Var {
            name, constructor, ..
        } => {
            if matches!(
                constructor.variant,
                ValueConstructorVariant::LocalVariable { .. }
            ) {
                out.insert(name.clone());
            }
        }

        // ModuleSelect references a module-level value; no local free
        // vars.
        TypedExpr::ModuleSelect { .. } => {}

        // Sequence/Pipeline: walk every sub-expression. Assignment
        // binders introduced earlier in the sequence shadow later
        // free-var occurrences of the same name. Track shadowed names
        // and skip them when collecting from the tail.
        TypedExpr::Sequence { expressions, .. } | TypedExpr::Pipeline { expressions, .. } => {
            let mut shadowed: BTreeSet<String> = BTreeSet::new();
            for sub in expressions {
                let mut sub_free = BTreeSet::new();
                collect_free_vars(sub, &mut sub_free);
                for name in sub_free {
                    if !shadowed.contains(&name) {
                        out.insert(name);
                    }
                }
                if let TypedExpr::Assignment { pattern, .. } = sub {
                    for binder in collect_pattern_binders(pattern) {
                        shadowed.insert(binder);
                    }
                }
            }
        }

        // Lambda: subtract its parameter names from the body's free
        // vars before merging.
        TypedExpr::Fn { args, body, .. } => {
            let mut body_free = BTreeSet::new();
            collect_free_vars(body, &mut body_free);
            for arg in args {
                if let Some(name) = arg.get_variable_name() {
                    body_free.remove(name);
                }
            }
            for name in body_free {
                out.insert(name);
            }
        }

        TypedExpr::List { elements, tail, .. } => {
            for e in elements {
                collect_free_vars(e, out);
            }
            if let Some(t) = tail {
                collect_free_vars(t, out);
            }
        }

        TypedExpr::Call { fun, args, .. } => {
            collect_free_vars(fun, out);
            for a in args {
                collect_free_vars(&a.value, out);
            }
        }

        TypedExpr::BinOp { left, right, .. } => {
            collect_free_vars(left, out);
            collect_free_vars(right, out);
        }

        // Assignment value contributes its free vars; the assignment's
        // own pattern binders only affect *subsequent* expressions in
        // a sequence (handled in the Sequence/Pipeline arm above).
        TypedExpr::Assignment { value, .. } => {
            collect_free_vars(value, out);
        }

        TypedExpr::Trace { then, text, .. } => {
            collect_free_vars(then, out);
            collect_free_vars(text, out);
        }

        // When: subject contributes its free vars; each clause's body
        // is walked with the clause pattern's binders subtracted.
        TypedExpr::When {
            subject, clauses, ..
        } => {
            collect_free_vars(subject, out);
            for clause in clauses {
                let mut clause_free = BTreeSet::new();
                collect_free_vars(&clause.then, &mut clause_free);
                for binder in collect_pattern_binders(&clause.pattern) {
                    clause_free.remove(&binder);
                }
                for name in clause_free {
                    out.insert(name);
                }
            }
        }

        TypedExpr::If {
            branches,
            final_else,
            ..
        } => {
            for branch in branches.iter() {
                collect_free_vars(&branch.condition, out);
                collect_free_vars(&branch.body, out);
            }
            collect_free_vars(final_else, out);
        }

        TypedExpr::RecordAccess { record, .. } => {
            collect_free_vars(record, out);
        }

        TypedExpr::Tuple { elems, .. } => {
            for e in elems {
                collect_free_vars(e, out);
            }
        }

        TypedExpr::Pair { fst, snd, .. } => {
            collect_free_vars(fst, out);
            collect_free_vars(snd, out);
        }

        TypedExpr::TupleIndex { tuple, .. } => {
            collect_free_vars(tuple, out);
        }

        TypedExpr::RecordUpdate { spread, args, .. } => {
            collect_free_vars(spread, out);
            for a in args {
                collect_free_vars(&a.value, out);
            }
        }

        TypedExpr::UnOp { value, .. } => {
            collect_free_vars(value, out);
        }
    }
}
