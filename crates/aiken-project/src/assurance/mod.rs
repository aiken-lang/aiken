//! Lean-free annotation validation and in-memory UAL bundle generation.
//!
//! Lean elaboration and its accepted syntax profile belong to the consumer.

pub mod bundle;
mod function;
pub(crate) mod lean;
mod parser;
mod purpose;

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
    /// Keyed by escaped Lean module namespace.
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
    pub id: String,
    pub module: String,
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
    pub location: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorKind {
    BlockSyntax,
    TargetIdCollision,
    UnknownScope,
    MissingOnchain,
    FunctionHandler,
    UnknownHandler,
    DuplicateProperty,
    HelperCollision,
    MixedExecutionSettings,
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
pub fn emitted_target_id(module: &str, name: &str) -> String {
    let mut id = String::with_capacity(module.len() + 2 + name.len());
    for character in module.chars() {
        id.push(match character {
            '/' | '-' => '_',
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
            let id = lean::namespace(name);
            model.fragments.insert(
                id.clone(),
                Fragment {
                    id,
                    module: name.to_string(),
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
            let mut resolved = Vec::new();
            for scope in &scopes {
                match resolve_scope(module, scope) {
                    Ok(scope) => resolved.push(scope),
                    Err(error) => errors.push(*error),
                }
            }
            let namespace = lean::namespace(name);
            let uses = if model.fragments.contains_key(&namespace) {
                vec![namespace]
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

fn check_helpers(
    model: &AnnotationModel,
    modules: &BTreeMap<&str, &CheckedModule>,
    errors: &mut Vec<Error>,
) {
    let helpers: BTreeMap<_, Vec<_>> = model
        .fragments
        .iter()
        .map(|(id, fragment)| {
            (
                id.as_str(),
                fragment
                    .predicates
                    .iter()
                    .flat_map(|predicate| {
                        lean::helpers(&predicate.body, predicate.body_location.start)
                    })
                    .collect(),
            )
        })
        .collect();
    for property in model.properties.values() {
        let mut seen_fragments = BTreeSet::new();
        let mut names = BTreeMap::new();
        let mut pending: Vec<_> = property.uses.iter().rev().map(String::as_str).collect();
        while let Some(id) = pending.pop() {
            if !seen_fragments.insert(id) {
                continue;
            }
            let fragment = &model.fragments[id];
            for (name, location) in &helpers[id] {
                if let Some((previous_module, previous_location)) =
                    names.insert(name.as_str(), (fragment.module.as_str(), *location))
                {
                    errors.push(Error::new(modules[property.module.as_str()], ErrorKind::HelperCollision, property.location,
                        format!("Helper '{name}' is defined more than once in property '{}' fragment closure (modules '{previous_module}' and '{}').", property.id, fragment.module))
                        .with_related(modules[previous_module], previous_location, format!("first definition of '{name}'"))
                        .with_related(modules[fragment.module.as_str()], *location, format!("conflicting definition of '{name}'")));
                }
            }
            pending.extend(fragment.imports.iter().rev().map(String::as_str));
        }
    }
}
