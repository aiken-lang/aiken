//! A helper property is not a property of validators that happen to use it.

use super::{AnnotationModel, TargetKind};
use crate::{Project, module::CheckedModule, telemetry::EventListener};
use aiken_lang::{
    ast::Definition,
    expr::TypedExpr,
    tipo::{ModuleValueConstructor, ValueConstructorVariant},
};
use std::collections::{BTreeMap, BTreeSet};

type Function<'a> = (&'a str, &'a str);

impl<T: EventListener> Project<T> {
    /// Warn on uncovered validators, following resolved function references transitively.
    /// Local variables with the same spelling as a helper are not references to it.
    pub fn assurance_scope_warnings(&self) -> Vec<String> {
        let Ok(model) = self.annotation_model() else {
            // Generation reports invalid annotations before this advisory pass runs.
            return Vec::new();
        };
        scope_warnings(&model, self.checked_modules.values())
    }
}

fn scope_warnings<'a>(
    model: &AnnotationModel,
    modules: impl IntoIterator<Item = &'a CheckedModule>,
) -> Vec<String> {
    let scoped: BTreeSet<_> = model
        .properties
        .values()
        .flat_map(|property| property.scopes.iter().map(|scope| scope.target.as_str()))
        .collect();
    let helpers: BTreeMap<_, _> = model
        .targets
        .values()
        .filter(|target| target.kind == TargetKind::Function && scoped.contains(target.id.as_str()))
        .map(|target| {
            (
                (target.module.as_str(), target.name.as_str()),
                target.id.as_str(),
            )
        })
        .collect();
    if helpers.is_empty() {
        return Vec::new();
    }
    let mut graph = BTreeMap::new();
    let mut validators = Vec::new();
    for module in modules {
        for definition in &module.ast.definitions {
            match definition {
                Definition::Fn(function) => {
                    let mut refs = BTreeSet::new();
                    references(&function.body, &mut refs);
                    graph.insert((module.name.as_str(), function.name.as_str()), refs);
                }
                Definition::Validator(validator) => {
                    if model.targets.values().any(|target| {
                        target.module == module.name
                            && target.name == validator.name
                            && scoped.contains(target.id.as_str())
                            && matches!(target.kind, TargetKind::Validator { .. })
                    }) {
                        continue;
                    }
                    let mut refs = BTreeSet::new();
                    for handler in validator.handlers.iter().chain([&validator.fallback]) {
                        references(&handler.body, &mut refs);
                    }
                    validators.push((module.name.as_str(), validator.name.as_str(), refs));
                }
                _ => (),
            }
        }
    }
    let mut warnings = Vec::new();
    for (module, validator, refs) in validators {
        let mut pending: Vec<_> = refs.into_iter().collect();
        let mut seen = BTreeSet::new();
        while let Some(reference) = pending.pop() {
            if !seen.insert(reference) {
                continue;
            }
            if let Some(helper) = helpers.get(&reference) {
                warnings.push(format!(
                    "Property scope covers helper '{helper}', used by validator '{module}.{validator}', but no property scopes that validator; helper verification does not verify the validator."
                ));
            }
            if let Some(dependencies) = graph.get(&reference) {
                pending.extend(dependencies.iter().copied());
            }
        }
    }
    warnings.sort();
    warnings
}

// Traverse the typed tree, not source spelling: imports, aliases, captures and
// higher-order references retain their resolved module/function identity here.
fn references<'a>(expression: &'a TypedExpr, out: &mut BTreeSet<Function<'a>>) {
    use TypedExpr::*;
    match expression {
        Var { constructor, .. } => {
            if let ValueConstructorVariant::ModuleFn { module, name, .. } = &constructor.variant {
                out.insert((module, name));
            }
        }
        ModuleSelect {
            constructor: ModuleValueConstructor::Fn { module, name, .. },
            ..
        } => {
            out.insert((module, name));
        }
        Sequence { expressions, .. } | Pipeline { expressions, .. } => {
            for expression in expressions {
                references(expression, out);
            }
        }
        Fn { body, .. } => references(body, out),
        List { elements, tail, .. } => {
            for element in elements {
                references(element, out);
            }
            if let Some(tail) = tail {
                references(tail, out);
            }
        }
        Call { fun, args, .. } => {
            references(fun, out);
            for arg in args {
                references(&arg.value, out);
            }
        }
        BinOp { left, right, .. }
        | Pair {
            fst: left,
            snd: right,
            ..
        } => {
            references(left, out);
            references(right, out);
        }
        Assignment { value, .. } | UnOp { value, .. } => references(value, out),
        Trace { text, then, .. } => {
            references(text, out);
            references(then, out);
        }
        When {
            subject, clauses, ..
        } => {
            references(subject, out);
            for clause in clauses {
                references(&clause.then, out);
            }
        }
        If {
            branches,
            final_else,
            ..
        } => {
            for branch in branches {
                references(&branch.condition, out);
                references(&branch.body, out);
            }
            references(final_else, out);
        }
        RecordAccess { record, .. } => references(record, out),
        Tuple { elems, .. } => {
            for element in elems {
                references(element, out);
            }
        }
        TupleIndex { tuple, .. } => references(tuple, out),
        RecordUpdate { spread, args, .. } => {
            references(spread, out);
            for arg in args {
                references(&arg.value, out);
            }
        }
        UInt { .. }
        | String { .. }
        | Value { .. }
        | ByteArray { .. }
        | CurvePoint { .. }
        | ErrorTerm { .. }
        | ModuleSelect { .. } => (),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::TestProject;

    fn checked(project: &mut TestProject, name: &str, source: &str) -> CheckedModule {
        let mut module = project.parse(source);
        module.name = name.to_owned();
        module.ast.name = name.to_owned();
        project.check(module)
    }

    #[test]
    fn follows_imported_aliases_and_transitive_helpers() {
        let mut project = TestProject::new();
        let helper = checked(
            &mut project,
            "helper",
            r#"
            @onchain(steps: 100, semantics: e)
            pub fn positive(n: Int) -> Bool { n > 0 }
            //@ PROPERTY positive for positive "Positive helper." True @//
        "#,
        );
        let validator = checked(
            &mut project,
            "contract",
            r#"
            use helper.{positive as test_positive}
            fn indirect(n: Int) -> Bool { test_positive(n) }
            validator contract {
                else(_ctx: Data) { indirect(1) }
            }
        "#,
        );
        let model = super::super::validate([&helper, &validator]).unwrap();
        let warnings = scope_warnings(&model, [&helper, &validator]);
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].contains("helper__positive"));
        assert!(warnings[0].contains("contract.contract"));
    }

    #[test]
    fn validator_scope_suppresses_warning_but_local_shadow_does_not_create_one() {
        for (body, property) in [
            (
                "positive(1)",
                "//@ PROPERTY validator for contract \"Validator claim.\" True @//",
            ),
            ("let positive = fn(n) { n == 1 }\npositive(1)", ""),
        ] {
            let mut project = TestProject::new();
            let module = checked(
                &mut project,
                "contract",
                &format!(
                    r#"
                @onchain(steps: 100, semantics: e)
                pub fn positive(n: Int) -> Bool {{ n > 0 }}
                //@ PROPERTY helper for positive "Helper claim." True @//
                @onchain(steps: 100, semantics: e)
                validator contract {{
                    else(_ctx: Data) {{ {body} }}
                }}
                {property}
            "#
                ),
            );
            let model = super::super::validate([&module]).unwrap();
            assert!(scope_warnings(&model, [&module]).is_empty());
        }
    }
}
