use std::collections::{BTreeMap, BTreeSet};

use crate::{
    function_digest,
    model::{
        AssuranceDocument, CheckingContext, CheckingEnvironment, CompiledInterfaceBlueprint,
        CompiledValidator, ContextTarget, Digest,
    },
    validation::{DocumentKind, validate},
};

/// A failed structural or cross-document binding, with the affected document path.
#[derive(Debug, thiserror::Error)]
#[error("{path}: {message}")]
pub struct BindingError {
    pub path: String,
    pub message: String,
}

fn failure(path: impl Into<String>, message: impl ToString) -> BindingError {
    BindingError {
        path: path.into(),
        message: message.to_string(),
    }
}

fn same_digest(left: &Digest, right: &Digest) -> bool {
    left.alg == right.alg && left.digest == right.digest
}

// The consumer resolves an id first, then a unique human-readable title.
fn resolve_validator<'a>(
    validators: &BTreeMap<&str, &'a CompiledValidator>,
    reference: &str,
) -> Result<&'a CompiledValidator, BindingError> {
    if let Some(validator) = validators.get(reference) {
        return Ok(validator);
    }
    let mut matches = validators.values().filter(|v| v.title == reference);
    match (matches.next(), matches.next()) {
        (Some(validator), None) => Ok(validator),
        (None, _) => Err(failure(reference, "unknown validator")),
        _ => Err(failure(reference, "ambiguous validator title")),
    }
}

/// Validate schemas and the blueprint/context/function/evidence binding chain.
/// Context bytes are keyed by `checkingContexts` registry id, not by URI. The
/// caller resolves paths relative to the assurance file; this crate performs no
/// filesystem or network access. Every registered context must be supplied.
///
/// This checks metadata identity, not UPLC execution, script-hash computation,
/// parameter application, or the truth of evidence. Environment bytes are checked
/// separately with [`validate_environment`]. Historical evidence with stale
/// bindings is rejected rather than being presented as current verification.
pub fn validate_bindings(
    assurance: &AssuranceDocument,
    blueprint_bytes: &[u8],
    contexts: &BTreeMap<String, &[u8]>,
) -> Result<(), BindingError> {
    let value = serde_json::to_value(assurance).map_err(|e| failure("assurance", e))?;
    validate(DocumentKind::Assurance, &value).map_err(|e| failure("assurance", e))?;
    let hash = assurance
        .blueprint
        .hash
        .as_ref()
        .ok_or_else(|| failure("blueprint.hash", "missing byte binding"))?;
    hash.verify(blueprint_bytes)
        .map_err(|e| failure("blueprint.hash", e))?;
    let value = serde_json::from_slice(blueprint_bytes).map_err(|e| failure("blueprint", e))?;
    validate(DocumentKind::Blueprint, &value).map_err(|e| failure("blueprint", e))?;
    let blueprint: CompiledInterfaceBlueprint =
        serde_json::from_value(value).map_err(|e| failure("blueprint", e))?;
    let mut validators = BTreeMap::new();
    for validator in &blueprint.validators {
        if validators
            .insert(validator.id.as_str(), validator)
            .is_some()
        {
            return Err(failure(&validator.id, "duplicate validator id"));
        }
    }
    for (id, function) in assurance.functions.iter().flatten() {
        if validators.contains_key(id.as_str()) {
            return Err(failure(id, "function id collides with validator id"));
        }
        let computed = function_digest(&function.compiled_code).map_err(|e| failure(id, e))?;
        if !same_digest(&computed, &function.hash) {
            return Err(failure(id, "function compiledCode hash mismatch"));
        }
    }
    let mut parsed_contexts = BTreeMap::new();
    for (id, artifact) in assurance.checking_contexts.iter().flatten() {
        let bytes = contexts
            .get(id)
            .ok_or_else(|| failure(id, "missing checking context bytes"))?;
        artifact.hash.verify(bytes).map_err(|e| failure(id, e))?;
        let value = serde_json::from_slice(bytes).map_err(|e| failure(id, e))?;
        validate(DocumentKind::CheckingContext, &value).map_err(|e| failure(id, e))?;
        let context: CheckingContext = serde_json::from_value(value).map_err(|e| failure(id, e))?;
        let mut targets = BTreeSet::new();
        for target in &context.targets {
            let target_id = match target {
                ContextTarget::Function(target) => {
                    let function = assurance
                        .functions
                        .as_ref()
                        .and_then(|fs| fs.get(&target.function))
                        .ok_or_else(|| failure(id, "unknown checking function"))?;
                    if !same_digest(&target.function_hash, &function.hash) {
                        return Err(failure(id, "checking function hash mismatch"));
                    }
                    let interface = &target.function_interface;
                    if interface.serialization != function.serialization
                        || interface.plutus_version != function.plutus_version
                        || interface.arguments != function.arguments
                        || interface.result != function.result
                        || interface
                            .definitions
                            .iter()
                            .flatten()
                            .ne(assurance.definitions.iter().flatten())
                    {
                        return Err(failure(id, "checking function interface mismatch"));
                    }
                    (true, target.function.as_str())
                }
                ContextTarget::Validator(target) => {
                    let validator = resolve_validator(&validators, &target.validator)?;
                    if !validator
                        .interface
                        .invocations
                        .iter()
                        .any(|i| i.purpose == target.purpose)
                    {
                        return Err(failure(id, "checking purpose has no validator invocation"));
                    }
                    (false, validator.id.as_str())
                }
            };
            if !targets.insert(target_id) {
                return Err(failure(id, "duplicate checking target"));
            }
        }
        parsed_contexts.insert(id, context);
    }
    let mut property_ids = BTreeSet::new();
    for property in &assurance.properties {
        let id = &property.id;
        if !property_ids.insert(id) {
            return Err(failure(id, "duplicate property id"));
        }
        let mut expected = BTreeSet::new();
        for name in property.scope.validators.iter().flatten() {
            let validator = resolve_validator(&validators, name)?;
            if !expected.insert((false, validator.id.as_str())) {
                return Err(failure(
                    id,
                    "duplicate scoped validator after title resolution",
                ));
            }
        }
        for name in property.scope.functions.iter().flatten() {
            if !assurance
                .functions
                .as_ref()
                .is_some_and(|fs| fs.contains_key(name))
            {
                return Err(failure(id, format!("unknown scoped function {name}")));
            }
            expected.insert((true, name.as_str()));
        }
        if let Some(context_id) = &property.checking_context {
            let context = parsed_contexts
                .get(context_id)
                .ok_or_else(|| failure(id, "unknown property checking context"))?;
            let actual: BTreeSet<_> = context
                .targets
                .iter()
                .map(|target| match target {
                    ContextTarget::Function(t) => Ok((true, t.function.as_str())),
                    ContextTarget::Validator(t) => {
                        resolve_validator(&validators, &t.validator).map(|v| (false, v.id.as_str()))
                    }
                })
                .collect::<Result<_, _>>()?;
            if actual != expected {
                return Err(failure(
                    id,
                    "checking targets do not exactly cover property scope",
                ));
            }
        }
        for evidence in property.evidence.iter().flatten() {
            if let Some(hash) = &evidence.checking_context_hash {
                let context_hash = property
                    .checking_context
                    .as_ref()
                    .and_then(|key| assurance.checking_contexts.as_ref()?.get(key))
                    .map(|artifact| &artifact.hash)
                    .ok_or_else(|| failure(id, "evidence has no property checking context"))?;
                if !same_digest(hash, context_hash) {
                    return Err(failure(id, "evidence checkingContextHash mismatch"));
                }
            }
            if let Some(hashes) = &evidence.function_hashes {
                for name in property.scope.functions.iter().flatten() {
                    let function =
                        &assurance.functions.as_ref().expect("scope checked above")[name];
                    if !hashes
                        .get(name)
                        .is_some_and(|hash| same_digest(hash, &function.hash))
                    {
                        return Err(failure(
                            id,
                            "evidence functionHashes mismatch or missing function",
                        ));
                    }
                }
            }
            if let Some(hash) = &evidence.script_hash {
                for name in property.scope.validators.iter().flatten() {
                    if resolve_validator(&validators, name)?.hash.as_ref() != Some(hash) {
                        return Err(failure(id, "evidence scriptHash mismatch"));
                    }
                }
            }
        }
    }
    Ok(())
}

/// Bind and validate a captured environment without invoking Lean or the solver.
/// Equality with a *loaded* checking environment remains the consumer's job.
pub fn validate_environment(
    context: &CheckingContext,
    environment_bytes: &[u8],
) -> Result<CheckingEnvironment, BindingError> {
    context
        .environment
        .hash
        .verify(environment_bytes)
        .map_err(|e| failure("environment.hash", e))?;
    let value = serde_json::from_slice(environment_bytes).map_err(|e| failure("environment", e))?;
    validate(DocumentKind::Environment, &value).map_err(|e| failure("environment", e))?;
    serde_json::from_value(value).map_err(|e| failure("environment", e))
}
