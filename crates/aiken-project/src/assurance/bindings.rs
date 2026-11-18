//! Lean declarations the pinned consumer adds to one property's environment.
//!
//! PlutusCoreBlaster 1c23de8 checks each property in a fresh copy of one namespace:
//! `elabBlueprintImport` declares every blueprint validator, `emitAssuranceFunction`
//! declares each scoped function, then the property's fragment closure is elaborated
//! in the same namespace. A name declared twice aborts that property, so the
//! producer rejects such bundles before writing them. The declaration set below is
//! read from the consumer source; schema-derived types are not modeled (see
//! ADR-0007): their names contain an uppercase letter, while every name modeled
//! here derives from lowercase Aiken module paths and identifiers or from helpers.

use super::{AnnotationModel, Error, ErrorKind, Property, TargetKind, fragment_closure, lean};
use crate::module::{CheckedModule, CheckedModules};
use aiken_lang::ast::{Definition, Span};
use aiken_ual as ual;
use std::collections::{BTreeMap, BTreeSet};

/// PlutusCoreBlaster `sanitizeName`: ASCII letters, digits and `_` are kept, and
/// every other character becomes `_`. Distinct ids can therefore share a binding.
pub(crate) fn lean_binding(name: &str) -> String {
    name.chars()
        .map(|c| {
            if c.is_ascii_alphanumeric() || c == '_' {
                c
            } else {
                '_'
            }
        })
        .collect()
}

/// What introduced a declaration, in Aiken terms.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Source<'a> {
    /// A blueprint validator, by its `module.name` title.
    Validator(&'a str),
    /// A scoped function, by its emitted target id.
    Function(&'a str),
    /// A single-segment helper declaration in a fragment.
    Helper {
        module: &'a str,
        name: &'a str,
        /// Span bounds; `Span` itself is not ordered.
        location: (usize, usize),
    },
}

#[derive(Debug, Clone, Copy)]
struct Binding<'a> {
    source: Source<'a>,
    /// The consumer declaration kind, e.g. "`_returns` predicate".
    role: &'static str,
}

/// The declarations the consumer adds for `property`, in elaboration order.
/// Helper-to-helper collisions are reported by the fragment-closure check.
fn environment<'a>(
    validators: &'a [ual::CompiledValidator],
    model: &'a AnnotationModel,
    property: &'a Property,
    helpers: &'a BTreeMap<&'a str, Vec<(String, &'a str, Span)>>,
) -> Vec<(String, Binding<'a>)> {
    let mut selected = BTreeMap::new();
    let mut functions = Vec::new();
    for scope in &property.scopes {
        match model.targets[&scope.target].kind {
            TargetKind::Validator { .. } => {
                // The first scope of a validator selects its checking target.
                selected
                    .entry(scope.target.as_str())
                    .or_insert(scope.applied_parameters.is_some());
            }
            TargetKind::Function if !functions.contains(&scope.target.as_str()) => {
                functions.push(scope.target.as_str());
            }
            TargetKind::Function => {}
        }
    }

    let mut declarations = Vec::new();
    let mut declare = |name: String, source, role| {
        declarations.push((name, Binding { source, role }));
    };
    // elabBlueprintImport: every validator, scoped or not.
    for validator in validators {
        let source = Source::Validator(&validator.title);
        let title = lean_binding(&validator.title);
        let id = lean_binding(&validator.id);
        let selection = selected.get(validator.id.as_str()).copied();
        declare(title.clone(), source, "title binding");
        if id != title {
            declare(id.clone(), source, "id binding");
        }
        if validator.compiled_code.is_none() {
            continue;
        }
        // A selected target gets an applied wrapper; the script is then renamed.
        if selection.is_some() {
            declare(format!("{title}_script"), source, "applied-wrapper script");
        }
        if validator.hash.is_some() {
            declare(format!("{title}_hash"), source, "`_hash` constant");
        }
        // An applied selection is specialized with its parameters removed.
        let parameters = validator.parameters.as_ref().is_some_and(|p| !p.is_empty());
        if parameters && selection != Some(true) {
            declare(
                format!("{title}_paramCount"),
                source,
                "`_paramCount` constant",
            );
        }
        if selection.is_some() && id != title {
            declare(format!("{id}_script"), source, "id script alias");
        }
    }
    // emitAssuranceFunction: scoped functions only.
    for id in functions {
        let source = Source::Function(id);
        let name = lean_binding(id);
        declare(name.clone(), source, "function binding");
        declare(format!("{name}_script"), source, "`_script` program");
        declare(format!("{name}_returns"), source, "`_returns` predicate");
    }
    // Fragments are elaborated in the same namespace. Dotted helpers and the
    // module-qualified copies are nested names, so only single segments can meet a
    // generated declaration.
    for fragment in fragment_closure(model, property) {
        for (name, module, location) in &helpers[fragment] {
            declare(
                name.clone(),
                Source::Helper {
                    module,
                    name,
                    location: (location.start, location.end),
                },
                "helper",
            );
        }
    }
    declarations
}

fn describe(binding: &Binding<'_>, model: &AnnotationModel) -> String {
    match binding.source {
        Source::Validator(title) => format!("validator '{title}' ({})", binding.role),
        Source::Function(id) => {
            let target = &model.targets[id];
            format!(
                "function '{}.{}' ({})",
                target.module, target.name, binding.role
            )
        }
        Source::Helper { module, name, .. } => format!("helper '{name}' in module '{module}'"),
    }
}

/// The module and span that define a source, if it is still in the checked project.
fn locate<'m>(
    source: Source<'_>,
    model: &AnnotationModel,
    modules: &'m CheckedModules,
) -> Option<(&'m CheckedModule, Span)> {
    match source {
        Source::Validator(title) => {
            let (module, name) = title.rsplit_once('.')?;
            let module = modules.get(module)?;
            module
                .ast
                .definitions
                .iter()
                .find_map(|definition| match definition {
                    Definition::Validator(validator) if validator.name == name => {
                        Some((module, validator.location))
                    }
                    _ => None,
                })
        }
        Source::Function(id) => {
            let target = model.targets.get(id)?;
            Some((modules.get(&target.module)?, target.location))
        }
        Source::Helper {
            module, location, ..
        } => Some((
            modules.get(module)?,
            Span {
                start: location.0,
                end: location.1,
            },
        )),
    }
}

/// Reject any name the consumer would declare twice in a property environment.
/// Each colliding pair is reported once, for the first property where it occurs.
pub(super) fn check(
    model: &AnnotationModel,
    modules: &CheckedModules,
    validators: &[ual::CompiledValidator],
) -> Vec<Error> {
    let helpers: BTreeMap<&str, Vec<(String, &str, Span)>> = model
        .fragments
        .iter()
        .map(|(id, fragment)| {
            let names = fragment
                .predicates
                .iter()
                .flat_map(|predicate| lean::helpers(&predicate.body, predicate.body_location.start))
                .filter_map(|(name, location)| match lean::segments(&name).as_slice() {
                    [leaf] => Some((leaf.clone(), fragment.module.as_str(), location)),
                    _ => None,
                })
                .collect();
            (id.as_str(), names)
        })
        .collect();

    let mut errors = Vec::new();
    let mut reported = BTreeSet::new();
    for property in model.properties.values() {
        let mut declared: BTreeMap<String, Binding<'_>> = BTreeMap::new();
        for (name, binding) in environment(validators, model, property, &helpers) {
            let Some(first) = declared.get(&name).copied() else {
                declared.insert(name, binding);
                continue;
            };
            if matches!(
                (first.source, binding.source),
                (Source::Helper { .. }, Source::Helper { .. })
            ) || !reported.insert((name.clone(), first.source, binding.source))
            {
                continue;
            }
            errors.push(collision(&name, first, binding, property, model, modules));
        }
    }
    errors
}

fn collision(
    name: &str,
    first: Binding<'_>,
    second: Binding<'_>,
    property: &Property,
    model: &AnnotationModel,
    modules: &CheckedModules,
) -> Error {
    let property_module = &modules[&property.module];
    let (module, location) =
        locate(second.source, model, modules).unwrap_or((property_module, property.location));
    let mut error = Error::new(
        module,
        ErrorKind::GeneratedNameCollision,
        location,
        format!(
            "Generated Lean name '{name}' is declared by both {} and {} in the consumer environment of property '{}'.",
            describe(&first, model),
            describe(&second, model),
            property.id
        ),
    );
    if let Some((module, location)) = locate(first.source, model, modules) {
        error = error.with_related(
            module,
            location,
            format!("first declaration of '{name}': {}", describe(&first, model)),
        );
    }
    error = error.with_related(
        property_module,
        property.location,
        format!("property '{}' elaborates both declarations", property.id),
    );
    let both_validators = matches!(
        (first.source, second.source),
        (Source::Validator(_), Source::Validator(_))
    );
    error.help = Some(if both_validators {
        "Every property imports every blueprint validator, so rename one of the validators or its module."
            .to_owned()
    } else {
        "Rename one of them. The consumer maps every character other than ASCII letters, digits and '_' to '_', and adds '_script', '_returns', '_hash' and '_paramCount' declarations."
            .to_owned()
    });
    error
}
