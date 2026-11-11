//! UAL annotation validation, bundle generation and consumer integration.
//!
//! Lean elaboration and its accepted syntax profile belong to the consumer.

mod bindings;
pub mod bundle;
pub mod check;
pub mod consumer;
mod function;
pub mod generate;
pub(crate) mod lean;
mod parser;
mod purpose;
mod scope;

#[cfg(test)]
mod tests;

use crate::module::CheckedModule;
use aiken_lang::ast::{DecoratorKind, Definition, OnchainValue, Span};
use miette::{Diagnostic, NamedSource};
use serde::Serialize;
use std::collections::{BTreeMap, BTreeSet};

#[derive(Debug, Default, Clone, PartialEq, Eq, Serialize)]
pub struct AnnotationModel {
    /// Keyed by ADR-0007 emitted id, shared by functions and validators.
    pub targets: BTreeMap<String, Target>,
    /// Keyed by fragment id ([`lean::fragment_id`]), not by Lean spelling.
    pub fragments: BTreeMap<String, Fragment>,
    /// Project-wide property ids.
    pub properties: BTreeMap<String, Property>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Target {
    pub id: String,
    pub module: String,
    pub name: String,
    pub kind: TargetKind,
    pub location: Span,
    pub settings: ExecutionSettings,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum TargetKind {
    Function,
    /// Defined handlers only; `else` is always a valid scope, including the implicit fallback.
    Validator {
        handlers: Vec<String>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ExecutionSettings {
    pub steps: u64,
    /// Normalized by type checking to C or E.
    pub semantics: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Fragment {
    /// Schema-legal bundle key, injective over module names.
    pub id: String,
    pub module: String,
    /// Lean namespace of the module-qualified declaration copies.
    pub namespace: String,
    /// Nearest imported fragments, in source-edge order, without duplicates.
    pub imports: Vec<String>,
    /// Predicate blocks in source order; their bodies retain original whitespace.
    pub predicates: Vec<Predicate>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Predicate {
    pub body: String,
    pub location: Span,
    pub body_location: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Property {
    pub id: String,
    pub module: String,
    pub location: Span,
    pub description: String,
    pub body: String,
    pub body_location: Span,
    pub scopes: Vec<ResolvedScope>,
    /// Direct fragment roots, including this module's fragment when present.
    pub uses: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ResolvedScope {
    pub target: String,
    /// Source handler selector, deliberately not lowered to a purpose or premise.
    pub handler: Option<String>,
    /// Concrete validator parameters in declaration order; absent means universal.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub applied_parameters: Option<Vec<String>>,
    pub location: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorKind {
    BlockSyntax,
    TargetIdCollision,
    UnknownScope,
    MissingOnchain,
    FunctionHandler,
    FunctionParameters,
    ConflictingParameters,
    AppliedParameters,
    UnknownHandler,
    DuplicateProperty,
    HelperCollision,
    /// Two sources map to one declaration in a property's consumer environment.
    GeneratedNameCollision,
    MissingFragment,
    MixedExecutionSettings,
    /// The scopes' purpose premises cannot hold for one script context.
    IncompatiblePurposes,
    PurposeLowering,
}

/// An insertion template, not an automatically applicable execution-policy choice.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FixIt {
    pub location: Span,
    pub template: String,
}

#[derive(Debug, thiserror::Error, Diagnostic)]
#[error("{message}")]
#[diagnostic(code("aiken::assurance"))]
pub struct Error {
    pub kind: ErrorKind,
    pub message: String,
    #[source_code]
    pub source_code: NamedSource<String>,
    #[label("invalid assurance annotation")]
    pub location: Span,
    #[help]
    pub help: Option<String>,
    pub fix: Option<FixIt>,
    #[related]
    pub related: Vec<RelatedLocation>,
}

#[derive(Debug, thiserror::Error, Diagnostic)]
#[error("{message}")]
pub struct RelatedLocation {
    pub message: String,
    #[source_code]
    pub source_code: NamedSource<String>,
    #[label("{message}")]
    pub location: Span,
}

impl Error {
    fn new(module: &CheckedModule, kind: ErrorKind, location: Span, message: String) -> Self {
        Self {
            kind,
            message,
            source_code: source(module),
            location,
            help: None,
            fix: None,
            related: Vec::new(),
        }
    }

    fn with_related(mut self, module: &CheckedModule, location: Span, message: String) -> Self {
        self.related.push(RelatedLocation {
            message,
            source_code: source(module),
            location,
        });
        self
    }
}

fn source(module: &CheckedModule) -> NamedSource<String> {
    NamedSource::new(module.input_path.display().to_string(), module.code.clone())
}

/// The single ADR-0007 target-id mapping, shared by function and validator targets.
///
/// Module paths use `[a-z0-9_-]`, `/` and, in the file name, `.`. Each of `/`, `-`
/// and `.` becomes `_`, so the id is also the Lean name the consumer binds and
/// propositions call (its `sanitizeName` is the identity on these ids). The mapping
/// is not injective: ids that merge are reported as collisions, never renamed.
pub fn emitted_target_id(module: &str, name: &str) -> String {
    let mut id = String::with_capacity(module.len() + 2 + name.len());
    for character in module.chars() {
        id.push(match character {
            '/' | '-' | '.' => '_',
            other => other,
        });
    }
    id.push_str("__");
    id.push_str(name);
    id
}

/// Only Lean bodies count as import uses, not descriptions or annotation headers.
pub(crate) fn references_namespace(
    code: &str,
    extra: &aiken_lang::parser::extra::ModuleExtra,
    namespace: &str,
) -> bool {
    extra.assurance_comments.iter().any(|&span| {
        let Ok(block) = parser::parse(code, span) else {
            return false;
        };
        let body = match &block {
            parser::Block::Predicate { body, .. } | parser::Block::Property { body, .. } => body,
        };
        lean::references_namespace(body, namespace)
    })
}

/// Build a deterministic annotation model from the complete type-checked module graph.
/// No Lean process, target compilation or filesystem writes are performed.
pub fn validate<'a>(
    modules: impl IntoIterator<Item = &'a CheckedModule>,
) -> Result<AnnotationModel, Vec<Error>> {
    let modules: BTreeMap<_, _> = modules.into_iter().map(|m| (m.name.as_str(), m)).collect();
    let mut model = AnnotationModel::default();
    if modules.values().all(|module| {
        module.extra.assurance_comments.is_empty() && module.ast.definition_decorators.is_empty()
    }) {
        return Ok(model);
    }
    let mut errors = Vec::new();
    let mut blocks = BTreeMap::new();
    let mut edges = BTreeMap::new();

    for (&name, &module) in &modules {
        let mut parsed = Vec::new();
        let mut predicates = Vec::new();
        for &span in &module.extra.assurance_comments {
            match parser::parse(&module.code, span) {
                Ok(parser::Block::Predicate {
                    body,
                    location,
                    body_location,
                }) => {
                    predicates.push(Predicate {
                        body,
                        location,
                        body_location,
                    });
                }
                Ok(block @ parser::Block::Property { .. }) => parsed.push(block),
                Err(error) => {
                    let mut diagnostic = Error::new(
                        module,
                        ErrorKind::BlockSyntax,
                        error.location,
                        error.message,
                    );
                    diagnostic.help = error.help;
                    errors.push(diagnostic);
                }
            }
        }
        if !predicates.is_empty() {
            let id = lean::fragment_id(name);
            model.fragments.insert(
                id.clone(),
                Fragment {
                    id,
                    module: name.to_string(),
                    namespace: lean::namespace(name),
                    imports: Vec::new(),
                    predicates,
                },
            );
        }
        blocks.insert(name, parsed);
        let mut imports: Vec<_> = module
            .ast
            .definitions
            .iter()
            .filter_map(|definition| {
                if let Definition::Use(import) = definition {
                    Some((import.location.start, import.module.join("/")))
                } else {
                    None
                }
            })
            .collect();
        imports.sort_by_key(|(start, _)| *start);
        edges.insert(
            name,
            imports
                .into_iter()
                .map(|(_, module)| module)
                .collect::<Vec<_>>(),
        );
        collect_targets(module, &modules, &mut model, &mut errors);
    }

    let fragment_modules: BTreeMap<_, _> = model
        .fragments
        .values()
        .map(|fragment| (fragment.module.as_str(), fragment.id.as_str()))
        .collect();
    let roots: BTreeMap<_, _> = modules
        .keys()
        .map(|&name| {
            let mut seen = BTreeSet::new();
            let mut imports = Vec::new();
            nearest_fragments(name, &edges, &fragment_modules, &mut seen, &mut imports);
            (name, imports)
        })
        .collect();
    for fragment in model.fragments.values_mut() {
        fragment
            .imports
            .clone_from(&roots[fragment.module.as_str()]);
    }

    for (name, parsed) in blocks {
        let module = modules[name];
        for block in parsed {
            let parser::Block::Property {
                id,
                id_location,
                scopes,
                description,
                body,
                location,
                body_location,
            } = block
            else {
                continue;
            };
            if let Some(previous) = model.properties.get(&id) {
                errors.push(
                    Error::new(
                        module,
                        ErrorKind::DuplicateProperty,
                        id_location,
                        format!(
                            "Property id '{id}' is already defined in module '{}'.",
                            previous.module
                        ),
                    )
                    .with_related(
                        modules[previous.module.as_str()],
                        previous.location,
                        format!("first definition of property '{id}'"),
                    ),
                );
                continue;
            }
            let mut resolved: Vec<ResolvedScope> = Vec::new();
            for scope in &scopes {
                match resolve_scope(module, scope) {
                    Ok(scope) => {
                        if let Some(previous) = resolved.iter().find(|previous| {
                            previous.target == scope.target
                                && previous.applied_parameters != scope.applied_parameters
                        }) {
                            errors.push(
                                Error::new(
                                    module,
                                    ErrorKind::ConflictingParameters,
                                    scope.location,
                                    format!(
                                        "Repeated scopes for '{}' must use identical parameter bindings.",
                                        scope.target
                                    ),
                                )
                                .with_related(
                                    module,
                                    previous.location,
                                    "first scope with different parameter bindings".into(),
                                ),
                            );
                        }
                        resolved.push(scope);
                    }
                    Err(error) => errors.push(*error),
                }
            }
            let own = lean::fragment_id(name);
            let uses = if model.fragments.contains_key(&own) {
                vec![own]
            } else {
                roots[name].clone()
            };
            model.properties.insert(
                id.clone(),
                Property {
                    id,
                    module: name.to_string(),
                    location,
                    description,
                    body,
                    body_location,
                    scopes: resolved,
                    uses,
                },
            );
        }
    }
    check_helpers(&model, &modules, &mut errors);
    if errors.is_empty() {
        Ok(model)
    } else {
        Err(errors)
    }
}

fn collect_targets(
    module: &CheckedModule,
    modules: &BTreeMap<&str, &CheckedModule>,
    model: &mut AnnotationModel,
    errors: &mut Vec<Error>,
) {
    for metadata in &module.ast.definition_decorators {
        let Some(arguments) = metadata.decorators.iter().find_map(|decorator| {
            if let DecoratorKind::Onchain { arguments } = &decorator.kind {
                Some(arguments)
            } else {
                None
            }
        }) else {
            continue;
        };
        let Some(definition) = module
            .ast
            .definitions
            .iter()
            .find(|definition| definition.location() == metadata.location)
        else {
            continue;
        };
        let (name, kind) = match definition {
            Definition::Fn(function) => (&function.name, TargetKind::Function),
            Definition::Validator(validator) => (
                &validator.name,
                TargetKind::Validator {
                    handlers: validator
                        .handlers
                        .iter()
                        .map(|handler| handler.name.clone())
                        .collect(),
                },
            ),
            _ => continue, // Type checking rejects other decorated definitions.
        };
        let steps = arguments
            .iter()
            .find_map(|argument| match &argument.value {
                OnchainValue::Int { value, .. } if argument.name == "steps" => value.parse().ok(),
                _ => None,
            })
            .expect("checked @onchain has positive u64 steps");
        let semantics = arguments
            .iter()
            .find_map(|argument| match &argument.value {
                OnchainValue::Name { name } if argument.name == "semantics" => Some(name.clone()),
                _ => None,
            })
            .expect("checked @onchain has normalized semantics");
        let id = emitted_target_id(&module.name, name);
        if let Some(previous) = model.targets.get(&id) {
            let location = module
                .extra
                .assurance_comments
                .first()
                .copied()
                .unwrap_or(metadata.location);
            errors.push(
                Error::new(
                    module,
                    ErrorKind::TargetIdCollision,
                    location,
                    format!(
                        "Targets '{}.{}' and '{}.{name}' both map to emitted id '{id}'.",
                        previous.module, previous.name, module.name
                    ),
                )
                .with_related(
                    modules[previous.module.as_str()],
                    previous.location,
                    format!("first target '{}.{}'", previous.module, previous.name),
                )
                .with_related(
                    module,
                    metadata.location,
                    format!("conflicting target '{}.{name}'", module.name),
                ),
            );
            continue;
        }
        model.targets.insert(
            id.clone(),
            Target {
                id,
                module: module.name.clone(),
                name: name.clone(),
                kind,
                location: metadata.location,
                settings: ExecutionSettings { steps, semantics },
            },
        );
    }
}

fn resolve_scope(
    module: &CheckedModule,
    scope: &parser::Scope,
) -> Result<ResolvedScope, Box<Error>> {
    let definition = module
        .ast
        .definitions
        .iter()
        .find(|definition| match definition {
            Definition::Fn(function) => function.name == scope.name,
            Definition::Validator(validator) => validator.name == scope.name,
            _ => false,
        });
    let Some(definition) = definition else {
        return Err(Box::new(Error::new(
            module,
            ErrorKind::UnknownScope,
            scope.location,
            format!(
                "Scope '{}' must name an @onchain target defined in this module ('{}').",
                scope.name, module.name
            ),
        )));
    };
    let id = emitted_target_id(&module.name, &scope.name);
    let onchain = module.ast.definition_decorators.iter().any(|metadata| {
        metadata.location == definition.location()
            && metadata
                .decorators
                .iter()
                .any(|decorator| matches!(decorator.kind, DecoratorKind::Onchain { .. }))
    });
    if !onchain {
        let mut error = Error::new(
            module,
            ErrorKind::MissingOnchain,
            scope.location,
            format!("Scope '{}' names a target without @onchain.", scope.name),
        );
        error.help = Some(format!(
            "Add @onchain(steps: <positive integer>, semantics: <c|e>) before '{}'; choose the execution settings for this property.",
            scope.name
        ));
        error.fix = Some(FixIt {
            location: Span {
                start: definition.location().start,
                end: definition.location().start,
            },
            template: "@onchain(steps: <positive integer>, semantics: <c|e>)\n".to_string(),
        });
        return Err(Box::new(error));
    };
    if scope.applied_parameters.is_some() && matches!(definition, Definition::Fn(_)) {
        return Err(Box::new(Error::new(
            module,
            ErrorKind::FunctionParameters,
            scope.location,
            format!(
                "Function scope '{}' cannot have applied validator parameters.",
                scope.name
            ),
        )));
    }
    if let Some(handler) = &scope.handler {
        match definition {
            Definition::Fn(_) => {
                return Err(Box::new(Error::new(
                    module,
                    ErrorKind::FunctionHandler,
                    scope.location,
                    format!(
                        "Function scope '{}' cannot have a handler suffix '.{handler}'.",
                        scope.name
                    ),
                )));
            }
            Definition::Validator(validator)
                if handler != "else"
                    && !validator
                        .handlers
                        .iter()
                        .any(|defined| defined.name == *handler) =>
            {
                return Err(Box::new(Error::new(
                    module,
                    ErrorKind::UnknownHandler,
                    scope.location,
                    format!(
                        "Validator '{}' does not define handler '{handler}'; use a defined handler or 'else'.",
                        scope.name
                    ),
                )));
            }
            _ => (),
        }
    }
    Ok(ResolvedScope {
        target: id,
        handler: scope.handler.clone(),
        applied_parameters: scope.applied_parameters.clone(),
        location: scope.location,
    })
}

/// Follow use edges through non-fragment modules, stopping at the first fragment.
/// A visited set deduplicates diamonds and keeps this traversal bounded.
fn nearest_fragments(
    module: &str,
    edges: &BTreeMap<&str, Vec<String>>,
    fragments: &BTreeMap<&str, &str>,
    seen: &mut BTreeSet<String>,
    result: &mut Vec<String>,
) {
    if let Some(imports) = edges.get(module) {
        for imported in imports {
            if !seen.insert(imported.clone()) {
                continue;
            }
            if let Some(id) = fragments.get(imported.as_str()) {
                result.push((*id).to_string());
            } else {
                nearest_fragments(imported, edges, fragments, seen, result);
            }
        }
    }
}

/// A helper declaration as the bundle emits it: the source spelling and its copy
/// in the module namespace. Names are unescaped Lean segments.
struct Declaration<'a> {
    module: &'a str,
    leaf: Vec<String>,
    qualified: Vec<String>,
    location: Span,
}

/// Which declaration an emitted name belongs to; both of its copies share one origin.
#[derive(Clone, Copy)]
struct Origin<'a> {
    declaration: &'a Declaration<'a>,
}

impl PartialEq for Origin<'_> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.declaration, other.declaration)
    }
}

/// The emitted name Lean picks for `reference` inside a declaration named `within`:
/// the innermost opened prefix `P` with `P.reference` declared wins, then the root.
/// Consumer and Lean library names are outside this model.
fn resolve<'n, 'a>(
    emitted: &'n BTreeMap<Vec<String>, Origin<'a>>,
    within: &[String],
    reference: &[String],
) -> Option<(&'n Vec<String>, Origin<'a>)> {
    (1..within.len())
        .rev()
        .find_map(|length| {
            let candidate = [&within[..length], reference].concat();
            emitted.get_key_value(&candidate)
        })
        .or_else(|| emitted.get_key_value(reference))
        .map(|(name, origin)| (name, *origin))
}

/// Every fragment a property's closure contains, deterministically ordered: depth-first
/// over `uses`, then each fragment's `imports`, without duplicates.
fn fragment_closure<'m>(model: &'m AnnotationModel, property: &Property) -> Vec<&'m str> {
    let mut seen = BTreeSet::new();
    let mut closure = Vec::new();
    let mut pending: Vec<_> = property.uses.iter().rev().map(String::as_str).collect();
    while let Some(id) = pending.pop() {
        if seen.insert(id) {
            let (id, fragment) = model
                .fragments
                .get_key_value(id)
                .expect("property uses and fragment imports name known fragments");
            closure.push(id.as_str());
            pending.extend(fragment.imports.iter().rev().map(String::as_str));
        }
    }
    closure
}

fn check_helpers(
    model: &AnnotationModel,
    modules: &BTreeMap<&str, &CheckedModule>,
    errors: &mut Vec<Error>,
) {
    let declarations: BTreeMap<_, Vec<_>> = model
        .fragments
        .iter()
        .map(|(id, fragment)| {
            let namespace = lean::segments(&fragment.namespace);
            (
                id.as_str(),
                fragment
                    .predicates
                    .iter()
                    .flat_map(|predicate| {
                        lean::helpers(&predicate.body, predicate.body_location.start)
                    })
                    .map(|(name, location)| {
                        let leaf = lean::segments(&name);
                        Declaration {
                            module: fragment.module.as_str(),
                            qualified: [namespace.as_slice(), &leaf].concat(),
                            leaf,
                            location,
                        }
                    })
                    .collect(),
            )
        })
        .collect();
    for property in model.properties.values() {
        let reporting = modules[property.module.as_str()];
        let closure = fragment_closure(model, property);
        let seen_fragments: BTreeSet<_> = closure.iter().copied().collect();

        // Every emitted spelling, source and module-qualified, names one declaration.
        let mut emitted: BTreeMap<Vec<String>, Origin<'_>> = BTreeMap::new();
        let mut collided = false;
        for declaration in closure.iter().flat_map(|id| &declarations[id]) {
            let mut reported = Vec::new();
            for name in [&declaration.leaf, &declaration.qualified] {
                if let Some(previous) = emitted.insert(name.clone(), Origin { declaration }) {
                    collided = true;
                    let previous = previous.declaration;
                    // Equal source spellings in one module also collide when qualified.
                    if reported.iter().any(|seen| std::ptr::eq(*seen, previous)) {
                        continue;
                    }
                    reported.push(previous);
                    let spelling = lean::spell(name);
                    errors.push(Error::new(reporting, ErrorKind::HelperCollision, property.location,
                        format!("Helper '{spelling}' is defined more than once in property '{}' fragment closure (modules '{}' and '{}').", property.id, previous.module, declaration.module))
                        .with_related(modules[previous.module], previous.location, format!("first definition of '{spelling}'"))
                        .with_related(modules[declaration.module], declaration.location, format!("conflicting definition of '{spelling}'")));
                }
            }
        }
        // A qualified copy is elaborated with its module namespace opened. It must
        // resolve every emitted name to the same helper as the source spelling does,
        // or the two copies would silently call different helpers.
        if !collided {
            'declarations: for declaration in closure.iter().flat_map(|id| &declarations[id]) {
                for reference in emitted.keys() {
                    let (Some((source, expected)), Some((qualified, actual))) = (
                        resolve(&emitted, &declaration.leaf, reference),
                        resolve(&emitted, &declaration.qualified, reference),
                    ) else {
                        continue;
                    };
                    if expected == actual {
                        continue;
                    }
                    let (expected, actual) = (expected.declaration, actual.declaration);
                    let mut error = Error::new(reporting, ErrorKind::HelperCollision, property.location,
                        format!("Helper '{}' from module '{}' shadows '{}' from module '{}' inside the qualified declaration '{}' in property '{}' fragment closure.",
                            lean::spell(qualified), actual.module, lean::spell(source), expected.module, lean::spell(&declaration.qualified), property.id))
                        .with_related(modules[actual.module], actual.location, format!("shadowing definition of '{}'", lean::spell(qualified)))
                        .with_related(modules[expected.module], expected.location, format!("shadowed definition of '{}'", lean::spell(source)));
                    error.help = Some("Rename one of the helpers; module-qualified names must resolve to the same helpers as source names.".into());
                    errors.push(error);
                    break 'declarations;
                }
            }
        }

        // A reference that spells a project helper's exact qualified declaration is
        // a missing import. Prefixes alone are not: `ctx.scriptContextTxInfo` is
        // usually a projection of a bound variable, not module `ctx`. Unqualified
        // and unknown names are left to consumer elaboration.
        let bodies: Vec<_> = std::iter::once(property.body.as_str())
            .chain(closure.iter().flat_map(|id| {
                model.fragments[*id]
                    .predicates
                    .iter()
                    .map(|predicate| predicate.body.as_str())
            }))
            .collect();
        for (id, fragment) in &model.fragments {
            if seen_fragments.contains(id.as_str()) {
                continue;
            }
            let Some(referenced) = declarations[id.as_str()].iter().find(|declaration| {
                bodies
                    .iter()
                    .any(|body| lean::references_declaration(body, &declaration.qualified))
            }) else {
                continue;
            };
            let mut error = Error::new(
                reporting,
                ErrorKind::MissingFragment,
                property.body_location,
                format!(
                    "Fragment '{id}' is not available in property '{}' fragment closure, but '{}' names its helper.",
                    property.id,
                    lean::spell(&referenced.qualified)
                ),
            )
            .with_related(
                modules[fragment.module.as_str()],
                referenced.location,
                format!("'{}' is defined here", lean::spell(&referenced.qualified)),
            );
            error.help = Some(format!(
                "Import module '{}' where its predicate helpers are referenced.",
                fragment.module
            ));
            errors.push(error);
        }
    }
}
