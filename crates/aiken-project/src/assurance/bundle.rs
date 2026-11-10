//! Assemble byte-bound UAL documents without writing files or invoking a consumer.
//!
//! [`lower`] leaves the environment slot unbound. [`PendingBundle::bind_environment`]
//! supplies the caller's artifact and validates the complete binding chain before
//! exposing final bytes. These bytes must be written unchanged by a future writer.

use super::{AnnotationModel, ErrorKind, Fragment, TargetKind, function, lean, purpose};
use crate::{
    blueprint::{Blueprint, dialect},
    module::CheckedModules,
};
use aiken_lang::gen_uplc::CodeGenerator;
use aiken_ual as ual;
use std::collections::{BTreeMap, BTreeSet};

pub use super::function::Error as FunctionError;

mod applied;

#[cfg(test)]
mod tests;

impl<T: crate::telemetry::EventListener> crate::Project<T> {
    /// Generate an in-memory assurance draft from the already checked project.
    /// Call `compile` with no code generation first; this method never writes the
    /// ordinary blueprint. Use the same tracing policy when comparing with export.
    pub fn assurance_bundle(
        &self,
        preamble: ual::AssurancePreamble,
        tracing: aiken_lang::ast::Tracing,
    ) -> Result<PendingBundle, Error> {
        let model = self
            .annotation_model()
            .map_err(|errors| Error::Annotations { errors })?;
        let mut generator = self.new_generator(tracing);
        let blueprint = Blueprint::new(&self.config, &self.checked_modules, &mut generator, false)?;
        lower(
            &model,
            &self.checked_modules,
            &mut generator,
            &blueprint,
            preamble,
        )
    }
}

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
pub enum Error {
    #[error("invalid assurance annotations")]
    Annotations {
        #[related]
        errors: Vec<super::Error>,
    },
    #[error(transparent)]
    Function(#[from] FunctionError),
    #[error(transparent)]
    Blueprint(#[from] dialect::Error),
    #[error(transparent)]
    CompileBlueprint(#[from] crate::blueprint::Error),
    #[error(transparent)]
    Json(#[from] serde_json::Error),
    #[error(transparent)]
    Encode(#[from] ual::EncodeError),
    #[error(transparent)]
    Validation(#[from] ual::ValidationError),
    #[error(transparent)]
    Bindings(#[from] ual::BindingError),
    #[error(transparent)]
    References(#[from] dialect::ReferenceError),
    #[error("conflicting function definition {0:?}")]
    DefinitionConflict(String),
    #[error("annotation target {0:?} is missing from the checked modules or blueprint")]
    MissingTarget(String),
    #[error("property {0:?} has no targets")]
    EmptyScope(String),
    #[error("no assurance properties to lower")]
    NoProperties,
    #[error("the pinned UAL blueprint schema requires at least one real validator")]
    NoValidators,
    #[error("unsupported execution semantics {0:?}; expected C or E")]
    Semantics(String),
}

/// A checking context whose environment artifact has not yet been supplied.
/// This intentionally has no serializer: it is not a runnable UAL document.
#[derive(Debug)]
pub struct PendingContext {
    pub execution: ual::Execution,
    pub targets: Vec<ual::ContextTarget>,
}

/// Fully lowered documents, deliberately not serializable until environment binding.
#[derive(Debug)]
pub struct PendingBundle {
    assurance: ual::AssuranceDocument,
    blueprint: Vec<u8>,
    contexts: BTreeMap<String, PendingContext>,
    artifacts: BTreeMap<String, Vec<u8>>,
}

/// Schema- and binding-validated final artifacts. No evidence is emitted.
#[derive(Debug)]
pub struct Bundle {
    assurance: ual::AssuranceDocument,
    assurance_bytes: Vec<u8>,
    blueprint: Vec<u8>,
    contexts: BTreeMap<String, Vec<u8>>,
    artifacts: BTreeMap<String, Vec<u8>>,
}

impl PendingBundle {
    /// Inspect lowered propositions and registries before an environment is available.
    /// Context artifact hashes are absent until binding; do not serialize this draft.
    pub fn assurance(&self) -> &ual::AssuranceDocument {
        &self.assurance
    }

    pub fn contexts(&self) -> &BTreeMap<String, PendingContext> {
        &self.contexts
    }

    /// Fill every environment slot, hash the final contexts, then validate all documents.
    /// The caller owns the environment URI and hash; this does not invoke or identify
    /// a loaded consumer. Use `aiken_ual::validate_environment` when bytes are available.
    pub fn bind_environment(mut self, environment: ual::Artifact) -> Result<Bundle, Error> {
        let mut contexts = BTreeMap::new();
        let mut artifacts = BTreeMap::new();
        for (id, pending) in self.contexts {
            let context = ual::CheckingContext {
                schema: ual::CHECKING_CONTEXT_SCHEMA_ID.to_owned(),
                profile: ual::UPLC_STEP_CHECK_PROFILE.to_owned(),
                environment: environment.clone(),
                execution: pending.execution,
                targets: pending.targets,
            };
            let bytes = ual::validated_bytes(ual::DocumentKind::CheckingContext, &context)?;
            artifacts.insert(
                id.clone(),
                ual::Artifact {
                    uri: format!("checking-{id}.json"),
                    hash: ual::sha256_digest(&bytes),
                    media_type: Some("application/json".to_owned()),
                    extensions: BTreeMap::new(),
                },
            );
            contexts.insert(id, bytes);
        }
        self.assurance.checking_contexts = Some(artifacts);
        let borrowed = contexts
            .iter()
            .map(|(id, bytes)| (id.clone(), bytes.as_slice()))
            .collect();
        ual::validate_bindings(&self.assurance, &self.blueprint, &borrowed)?;
        let assurance_bytes = ual::validated_bytes(ual::DocumentKind::Assurance, &self.assurance)?;
        Ok(Bundle {
            assurance: self.assurance,
            assurance_bytes,
            blueprint: self.blueprint,
            contexts,
            artifacts: self.artifacts,
        })
    }
}

impl Bundle {
    pub fn assurance(&self) -> &ual::AssuranceDocument {
        &self.assurance
    }

    pub fn assurance_bytes(&self) -> &[u8] {
        &self.assurance_bytes
    }

    pub fn blueprint_bytes(&self) -> &[u8] {
        &self.blueprint
    }

    /// Final context bytes keyed by property id (artifact URI: `checking-<id>.json`).
    pub fn contexts(&self) -> &BTreeMap<String, Vec<u8>> {
        &self.contexts
    }

    /// Additional byte-bound artifacts keyed by their bundle-relative URI.
    pub fn artifacts(&self) -> &BTreeMap<String, Vec<u8>> {
        &self.artifacts
    }
}

/// Selectors whose generated purpose premises admit no common purpose would make the
/// property vacuously true, so this is a producer error rather than a solver question.
/// The predicate blocks in source order, followed by their module-qualified copies.
///
/// Source spellings keep helpers callable unqualified anywhere in the closure, as
/// before; the copies make the documented `module.path.helper` names resolve. Both
/// are plain `def`/`abbrev` commands, the only fragment commands the consumer accepts.
fn fragment_source(fragment: &Fragment) -> String {
    let source: Vec<_> = fragment
        .predicates
        .iter()
        .map(|predicate| predicate.body.as_str())
        .collect();
    let qualified: Vec<_> = fragment
        .predicates
        .iter()
        .map(|predicate| lean::qualify(&predicate.body, &fragment.namespace))
        .collect();
    format!(
        "{}\n-- Module-qualified copies in namespace {}\n{}",
        source.join("\n"),
        fragment.namespace,
        qualified.join("\n")
    )
}

fn incompatible_purposes(
    module: &crate::module::CheckedModule,
    property: &super::Property,
    conflict: &purpose::Conflict,
) -> super::Error {
    let selector = |index: usize| module.code[property.scopes[index].location.range()].to_owned();
    let culprit = selector(conflict.at);
    let message = if conflict.at == 0 {
        format!(
            "property {:?}: scope '{culprit}' admits no script purpose, because the validator defines a handler for every purpose; the property would hold vacuously",
            property.id
        )
    } else {
        let admitted = (0..=conflict.at)
            .map(|index| format!("'{}' admits {}", selector(index), conflict.sets[index]))
            .collect::<Vec<_>>()
            .join(", ");
        format!(
            "property {:?}: scopes cannot hold together for one script context ({admitted}); every scope constrains the same 'ctx', and no purpose satisfies all of them, so the property would hold vacuously",
            property.id
        )
    };
    let mut error = super::Error::new(
        module,
        ErrorKind::IncompatiblePurposes,
        property.scopes[conflict.at].location,
        message,
    );
    for index in 0..conflict.at {
        if conflict.sets[index] != purpose::Purposes::ALL {
            error = error.with_related(
                module,
                property.scopes[index].location,
                format!("'{}' admits {}", selector(index), conflict.sets[index]),
            );
        }
    }
    error.help = Some(
        "Write a separate property for each purpose. To state alternatives, scope the validator without a handler suffix (e.g. `for vesting`) and write the purpose condition in the proposition yourself."
            .to_owned(),
    );
    error
}

/// Compile functions through `aiken export`'s generator path and lower the annotation
/// model against the UAL-07 blueprint. All inputs must describe the same checked
/// project and tracing policy. No filesystem or consumer operations are performed.
///
/// A validator-free project is rejected by the pinned blueprint schema; functions
/// are never fabricated as blueprint validators. Each function interface copies the
/// complete merged assurance definitions, as required by the consumer's equality check.
pub fn lower(
    model: &AnnotationModel,
    modules: &CheckedModules,
    generator: &mut CodeGenerator<'_>,
    blueprint: &Blueprint,
    preamble: ual::AssurancePreamble,
) -> Result<PendingBundle, Error> {
    if model.properties.is_empty() {
        return Err(Error::NoProperties);
    }
    // Report incompatible policies and unsafe purpose lowering before code generation.
    let mut errors = Vec::new();
    let mut propositions = BTreeMap::new();
    for property in model.properties.values() {
        let module = modules
            .get(&property.module)
            .ok_or_else(|| Error::MissingTarget(property.module.clone()))?;
        let first = property
            .scopes
            .first()
            .ok_or_else(|| Error::EmptyScope(property.id.clone()))?;
        let first_target = model
            .targets
            .get(&first.target)
            .ok_or_else(|| Error::MissingTarget(first.target.clone()))?;
        for scope in property.scopes.iter().skip(1) {
            let target = model
                .targets
                .get(&scope.target)
                .ok_or_else(|| Error::MissingTarget(scope.target.clone()))?;
            if target.settings != first_target.settings {
                errors.push(super::Error::new(module, ErrorKind::MixedExecutionSettings, scope.location,
                    format!("property {:?} mixes execution settings: {} uses steps={} semantics={}, but {} uses steps={} semantics={}",
                        property.id, first_target.id, first_target.settings.steps, first_target.settings.semantics,
                        target.id, target.settings.steps, target.settings.semantics))
                    .with_related(module, first.location, "first scoped execution policy".to_owned()));
            }
        }
        match purpose::conflict(property, &model.targets) {
            Ok(Some(conflict)) => {
                errors.push(incompatible_purposes(module, property, &conflict));
                continue;
            }
            Ok(None) => {}
            Err(message) => {
                errors.push(super::Error::new(
                    module,
                    ErrorKind::PurposeLowering,
                    property.body_location,
                    message,
                ));
                continue;
            }
        }
        match purpose::lower(property, &model.targets) {
            Ok(source) => {
                propositions.insert(property.id.clone(), source);
            }
            Err(message) => errors.push(super::Error::new(
                module,
                ErrorKind::PurposeLowering,
                property.body_location,
                message,
            )),
        }
    }
    if !errors.is_empty() {
        return Err(Error::Annotations { errors });
    }
    if blueprint.validators.is_empty() {
        return Err(Error::NoValidators);
    }
    let dialect = blueprint.to_compiled_interface()?;
    let validators: BTreeSet<_> = dialect.validators.iter().map(|v| v.id.as_str()).collect();
    let mut functions = BTreeMap::new();
    let mut definitions = BTreeMap::new();
    for target in model.targets.values() {
        match target.kind {
            TargetKind::Function => {
                let (function, schemas) = function::compile(target, modules, generator)?;
                for (name, schema) in schemas {
                    match definitions.entry(name) {
                        std::collections::btree_map::Entry::Vacant(entry) => {
                            entry.insert(schema);
                        }
                        std::collections::btree_map::Entry::Occupied(entry)
                            if entry.get() != &schema =>
                        {
                            return Err(Error::DefinitionConflict(entry.key().clone()));
                        }
                        _ => {}
                    }
                }
                functions.insert(target.id.clone(), function);
            }
            TargetKind::Validator { .. } if !validators.contains(target.id.as_str()) => {
                return Err(Error::MissingTarget(target.id.clone()));
            }
            TargetKind::Validator { .. } => {}
        }
    }
    let definitions = (!definitions.is_empty()).then_some(definitions);
    let mut properties = Vec::with_capacity(model.properties.len());
    let mut contexts = BTreeMap::new();
    let mut artifacts = BTreeMap::new();
    for property in model.properties.values() {
        let mut seen = BTreeSet::new();
        let mut scoped_functions = Vec::new();
        let mut scoped_validators = Vec::new();
        let mut targets = Vec::new();
        for scope in &property.scopes {
            // A validator may have several handler scopes, but the consumer requires
            // one checking target. V3 purposes select the same invocation; premises
            // retain every selector. Select the first source scope deterministically.
            if !seen.insert(&scope.target) {
                continue;
            }
            let target = &model.targets[&scope.target];
            match target.kind {
                TargetKind::Function => {
                    let function = &functions[&scope.target];
                    scoped_functions.push(scope.target.clone());
                    targets.push(ual::ContextTarget::Function(ual::FunctionTarget {
                        function: scope.target.clone(),
                        function_hash: function.hash.clone(),
                        function_interface: ual::FunctionInterface {
                            serialization: function.serialization,
                            plutus_version: function.plutus_version,
                            arguments: function.arguments.clone(),
                            result: function.result.clone(),
                            definitions: definitions.clone(),
                        },
                    }));
                }
                TargetKind::Validator { .. } => {
                    scoped_validators.push(scope.target.clone());
                    let parameters = match &scope.applied_parameters {
                        None => ual::ParameterBinding::Universal {},
                        Some(parameters) => {
                            applied::lower(blueprint, target, parameters, &mut artifacts).map_err(
                                |message| Error::Annotations {
                                    errors: vec![super::Error::new(
                                        modules
                                            .get(&property.module)
                                            .expect("property module checked before lowering"),
                                        ErrorKind::AppliedParameters,
                                        scope.location,
                                        message,
                                    )],
                                },
                            )?
                        }
                    };
                    targets.push(ual::ContextTarget::Validator(ual::ValidatorTarget {
                        validator: scope.target.clone(),
                        purpose: purpose::purpose(scope.handler.as_deref()),
                        parameters,
                    }));
                }
            }
        }
        let settings = &model.targets[&property.scopes[0].target].settings;
        let semantics_variant = match settings.semantics.as_str() {
            "C" => ual::SemanticsVariant::C,
            "E" => ual::SemanticsVariant::E,
            other => return Err(Error::Semantics(other.to_owned())),
        };
        contexts.insert(
            property.id.clone(),
            PendingContext {
                execution: ual::Execution {
                    semantics_variant,
                    budget: ual::Budget::CekSteps {
                        steps: settings.steps,
                    },
                    acceptance: ual::Acceptance::Evaluation,
                    protocol_version: None,
                },
                targets,
            },
        );
        properties.push(ual::Property {
            id: property.id.clone(),
            title: None,
            scope: ual::PropertyScope {
                validators: (!scoped_validators.is_empty()).then_some(scoped_validators),
                functions: (!scoped_functions.is_empty()).then_some(scoped_functions),
            },
            statement: ual::Statement {
                text: property.description.clone(),
                formal: Some(ual::FormalStatement {
                    language: "ual".to_owned(),
                    source: propositions.remove(&property.id),
                    uri: None,
                    uses: (!property.uses.is_empty()).then(|| property.uses.clone()),
                    extensions: BTreeMap::new(),
                }),
                extensions: BTreeMap::new(),
            },
            assumptions: None,
            tags: None,
            evidence: None,
            checking_context: Some(property.id.clone()),
            extensions: BTreeMap::new(),
        });
    }
    let blueprint = ual::validated_bytes(ual::DocumentKind::Blueprint, &dialect)?;
    let lock: serde_json::Value = serde_json::from_str(ual::ASSURANCE_WORKSPACE)?;
    let ual_commit = lock["repositories"]["ual"]["commit"]
        .as_str()
        .expect("vendored UAL pin");
    let lean_version = lock["lean"].as_str().expect("vendored Lean pin");
    let fragments: Vec<_> = model
        .fragments
        .values()
        .map(|fragment| ual::FormalFragment {
            id: fragment.id.clone(),
            language: "lean".to_owned(),
            imports: (!fragment.imports.is_empty()).then(|| fragment.imports.clone()),
            source: fragment_source(fragment),
            extensions: BTreeMap::new(),
        })
        .collect();
    let assurance = ual::AssuranceDocument {
        schema: ual::ASSURANCE_SCHEMA_ID.to_owned(),
        preamble,
        blueprint: ual::BlueprintReference {
            uri: "plutus.json".to_owned(),
            hash: Some(ual::sha256_digest(&blueprint)),
            extensions: BTreeMap::new(),
        },
        languages: Some(BTreeMap::from([
            (
                "lean".to_owned(),
                ual::RegistryEntry {
                    name: "Lean".to_owned(),
                    version: lean_version.to_owned(),
                    uri: Some("https://lean-lang.org".to_owned()),
                    description: None,
                    extensions: BTreeMap::new(),
                },
            ),
            (
                "ual".to_owned(),
                ual::RegistryEntry {
                    name: "UAL".to_owned(),
                    version: "1.0".to_owned(),
                    uri: Some(format!(
                        "https://github.com/input-output-hk/UniversalAnnotationLanguage/tree/{ual_commit}"
                    )),
                    description: None,
                    extensions: BTreeMap::from([("commit".to_owned(), ual_commit.into())]),
                },
            ),
        ])),
        tools: None,
        formal_fragments: (!fragments.is_empty()).then_some(fragments),
        properties,
        checking_contexts: None,
        functions: (!functions.is_empty()).then_some(functions),
        definitions,
        extensions: BTreeMap::new(),
    };
    let value = serde_json::to_value(&assurance)?;
    ual::validate(ual::DocumentKind::Assurance, &value)?;
    dialect::graph::validate(&value)?;
    Ok(PendingBundle {
        assurance,
        blueprint,
        contexts,
        artifacts,
    })
}
