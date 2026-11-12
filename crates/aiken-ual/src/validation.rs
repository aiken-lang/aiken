//! Offline validation against the schemas pinned to the assurance consumer.
//!
//! Document contents are instances, never schemas to compile or URI resources to fetch.
//! `ValueSchema` checks the pinned structural meta-schema; it does not resolve value
//! definition references or perform the consumer's semantic checks.

use std::sync::LazyLock;

use jsonschema::{Draft, Resource, Retrieve, Uri, ValidationOptions, Validator};
use serde_json::{Value, json};
use thiserror::Error;

const ASSURANCE_ID: &str = "https://cips.cardano.org/cips/cipXXXX/schemas/assurance-v2.json";
const CHECKING_CONTEXT_ID: &str =
    "https://cips.cardano.org/cips/cipXXXX/schemas/checking-context.json";
const INTERFACE_ID: &str =
    "https://cips.cardano.org/cips/cip57/extensions/compiled-interface/v1/schema.json";
const VALUE_ID: &str =
    "https://cips.cardano.org/cips/cip57/extensions/compiled-interface/v1/value-schema.json";
const BASE_BLUEPRINT_ID: &str = "https://cips.cardano.org/cips/cip57/schemas/plutus-blueprint.json";
const ENVIRONMENT_ID: &str = "urn:aiken:ual:checking-environment:v1";

const SCHEMAS: [(&str, &str); 6] = [
    (
        ASSURANCE_ID,
        include_str!("../schemas/assurance-v2.schema.json"),
    ),
    (
        CHECKING_CONTEXT_ID,
        include_str!("../schemas/checking-context.schema.json"),
    ),
    (
        INTERFACE_ID,
        include_str!("../schemas/interface.schema.json"),
    ),
    (VALUE_ID, include_str!("../schemas/value.schema.json")),
    (
        BASE_BLUEPRINT_ID,
        include_str!("../schemas/blueprint.schema.json"),
    ),
    (
        ENVIRONMENT_ID,
        include_str!("../schemas/environment.schema.json"),
    ),
];

/// A supported document contract, independent of the instance's `$schema` field.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DocumentKind {
    Assurance,
    CheckingContext,
    /// The compiled-interface extension, not the legacy base CIP-57 blueprint.
    Blueprint,
    /// The locally specified shape of the pinned consumer's environment capture.
    Environment,
    ValueSchema,
}

impl DocumentKind {
    fn schema_id(self) -> &'static str {
        match self {
            Self::Assurance => ASSURANCE_ID,
            Self::CheckingContext => CHECKING_CONTEXT_ID,
            Self::Blueprint => INTERFACE_ID,
            Self::Environment => ENVIRONMENT_ID,
            Self::ValueSchema => VALUE_ID,
        }
    }
}

/// Validation diagnostics use RFC 6901 JSON Pointers; `""` denotes the root.
#[derive(Clone, Debug, Error, Eq, PartialEq)]
pub enum ValidationError {
    /// An embedded schema could not be loaded or compiled, rather than bad input.
    #[error("cannot compile {kind:?} schema at instance location {instance_path:?}: {message}")]
    Schema {
        kind: DocumentKind,
        instance_path: String,
        message: String,
    },
    #[error(
        "invalid {kind:?} at instance location {instance_path:?} (schema {schema_path:?}): {message}"
    )]
    Document {
        kind: DocumentKind,
        instance_path: String,
        schema_path: String,
        message: String,
    },
}

/// Validate a document without network or filesystem resource retrieval.
///
/// Validators, including compilation failures, are cached independently by kind.
/// This checks JSON Schema constraints only, not cross-document references,
/// artifact digests, or equality with a live checking environment.
pub fn validate(kind: DocumentKind, document: &Value) -> Result<(), ValidationError> {
    validator(kind)?
        .validate(document)
        .map_err(|error| ValidationError::Document {
            kind,
            instance_path: error.instance_path.to_string(),
            schema_path: error.schema_path.to_string(),
            message: error.to_string(),
        })
}

// An explicit retriever prevents future Cargo feature unification from enabling
// HTTP/file retrieval, even if another dependency enables jsonschema defaults.
struct RejectExternal;

impl Retrieve for RejectExternal {
    fn retrieve(
        &self,
        uri: &Uri<String>,
    ) -> Result<Value, Box<dyn std::error::Error + Send + Sync>> {
        Err(format!("unregistered offline schema resource: {uri}").into())
    }
}

fn offline_options() -> Result<&'static ValidationOptions, &'static str> {
    static OPTIONS: LazyLock<Result<ValidationOptions, String>> = LazyLock::new(|| {
        let mut options = jsonschema::options()
            .with_draft(Draft::Draft202012)
            .with_retriever(RejectExternal);
        for (id, source) in SCHEMAS {
            let mut schema: Value = serde_json::from_str(source)
                .map_err(|error| format!("embedded schema {id}: {error}"))?;
            if schema.get("$id").and_then(Value::as_str) != Some(id) {
                return Err(format!(
                    "embedded schema does not declare expected $id {id}"
                ));
            }
            if id == BASE_BLUEPRINT_ID {
                // referencing 0.33 crawls all registered resources eagerly.
                // The pinned consumer instead resolves references on demand:
                // only this exact base preamble subtree is reachable from the
                // compiled-interface schema. The unused legacy validator has
                // references to argument/parameter schemas absent at the pin.
                // Preserve the vendored bytes and register just the reachable
                // subtree under the exact base ID. Reject the base root rather
                // than expose this adapter as a permissive blueprint schema.
                let preamble = schema
                    .pointer_mut("/$defs/preamble")
                    .ok_or_else(|| "embedded base blueprint has no preamble".to_owned())?
                    .take();
                schema = json!({
                    "$schema": "https://json-schema.org/draft/2020-12/schema",
                    "$id": id,
                    "not": {},
                    "$defs": {"preamble": preamble}
                });
            }
            let resource = Resource::from_contents(schema)
                .map_err(|error| format!("embedded schema {id}: {error}"))?;
            options = options.with_resource(id, resource);
        }
        Ok(options)
    });
    OPTIONS.as_ref().map_err(String::as_str)
}

fn compile(kind: DocumentKind) -> Result<Validator, ValidationError> {
    let options = offline_options().map_err(|message| ValidationError::Schema {
        kind,
        instance_path: String::new(),
        message: message.to_owned(),
    })?;
    options
        .build(&json!({"$ref": kind.schema_id()}))
        .map_err(|error| ValidationError::Schema {
            kind,
            instance_path: error.instance_path.to_string(),
            message: error.to_string(),
        })
}

fn validator(kind: DocumentKind) -> Result<&'static Validator, ValidationError> {
    static ASSURANCE: LazyLock<Result<Validator, ValidationError>> =
        LazyLock::new(|| compile(DocumentKind::Assurance));
    static CHECKING_CONTEXT: LazyLock<Result<Validator, ValidationError>> =
        LazyLock::new(|| compile(DocumentKind::CheckingContext));
    static BLUEPRINT: LazyLock<Result<Validator, ValidationError>> =
        LazyLock::new(|| compile(DocumentKind::Blueprint));
    static ENVIRONMENT: LazyLock<Result<Validator, ValidationError>> =
        LazyLock::new(|| compile(DocumentKind::Environment));
    static VALUE: LazyLock<Result<Validator, ValidationError>> =
        LazyLock::new(|| compile(DocumentKind::ValueSchema));

    let cache = match kind {
        DocumentKind::Assurance => &ASSURANCE,
        DocumentKind::CheckingContext => &CHECKING_CONTEXT,
        DocumentKind::Blueprint => &BLUEPRINT,
        DocumentKind::Environment => &ENVIRONMENT,
        DocumentKind::ValueSchema => &VALUE,
    };
    cache.as_ref().map_err(Clone::clone)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn environment() -> Value {
        json!({
            "format": "blaster-loaded-environment-v1",
            "leanVersion": "4.19.0",
            "compilerSha256": "00".repeat(32),
            "solverSha256": "11".repeat(32),
            "solverOptions": {
                "timeoutSeconds": 60,
                "maxRecDepth": 100000,
                "maxHeartbeats": 0
            },
            "modules": {"Lean": "22".repeat(32)}
        })
    }

    fn blueprint() -> Value {
        json!({
            "$schema": INTERFACE_ID,
            "$vocabulary": {
                "https://cips.cardano.org/cips/cip57": true,
                "https://cips.cardano.org/cips/cip57/extensions/compiled-interface/v1/vocabulary": true
            },
            "preamble": {"title": "Example", "version": "1", "plutusVersion": "v3"},
            "validators": [{
                "id": "example",
                "title": "Example validator",
                "redeemer": {"schema": {}},
                "interface": {
                    "callingConvention": "ledger-v3",
                    "invocations": [{"purpose": "spend", "arguments": [{"role": "context"}]}]
                }
            }]
        })
    }

    #[test]
    fn every_document_kind_compiles_and_rejects_nonobjects() {
        for kind in [
            DocumentKind::Assurance,
            DocumentKind::CheckingContext,
            DocumentKind::Blueprint,
            DocumentKind::Environment,
            DocumentKind::ValueSchema,
        ] {
            assert!(matches!(
                validate(kind, &Value::Null),
                Err(ValidationError::Document { instance_path, .. }) if instance_path.is_empty()
            ));
        }
    }

    #[test]
    fn unknown_http_file_and_relative_resources_are_rejected() {
        let options = offline_options().unwrap();
        for reference in [
            "https://example.invalid/schema.json",
            "http://127.0.0.1:1/schema.json",
            "file:///etc/passwd",
            "unknown-relative.json",
        ] {
            let error = options
                .build(&json!({
                    "$id": "https://example.invalid/root.json",
                    "$ref": reference
                }))
                .unwrap_err();
            assert!(
                error
                    .to_string()
                    .contains("unregistered offline schema resource")
            );
        }
    }

    #[test]
    fn broken_schema_is_not_treated_as_a_document() {
        let error = offline_options()
            .unwrap()
            .build(&json!({"type": 7}))
            .unwrap_err();
        assert_eq!(error.instance_path.to_string(), "/type");
    }

    #[test]
    fn base_adapter_preserves_preamble_but_rejects_legacy_root() {
        let mut document = blueprint();
        validate(DocumentKind::Blueprint, &document).unwrap();
        document["preamble"]["plutusVersion"] = json!("v4");
        assert!(matches!(
            validate(DocumentKind::Blueprint, &document),
            Err(ValidationError::Document { instance_path, .. })
                if instance_path == "/preamble/plutusVersion"
        ));
        let base = offline_options()
            .unwrap()
            .build(&json!({"$ref": BASE_BLUEPRINT_ID}))
            .unwrap();
        assert!(!base.is_valid(&blueprint()));
        assert!(
            offline_options()
                .unwrap()
                .build(&json!({"$ref": format!("{BASE_BLUEPRINT_ID}#/$defs/validator")}))
                .is_err()
        );
    }

    #[test]
    fn blueprint_requires_compiled_interface_contract() {
        let mut document = blueprint();
        document["$schema"] = json!(BASE_BLUEPRINT_ID);
        assert!(validate(DocumentKind::Blueprint, &document).is_err());
        document = blueprint();
        document.as_object_mut().unwrap().remove("$vocabulary");
        assert!(validate(DocumentKind::Blueprint, &document).is_err());
    }

    #[test]
    fn value_references_are_instance_strings_not_fetch_requests() {
        validate(
            DocumentKind::ValueSchema,
            &json!({"$ref": "#/definitions/Unknown"}),
        )
        .unwrap();
        assert!(matches!(
            validate(DocumentKind::ValueSchema, &json!({"$ref": "https://example.invalid/value"})),
            Err(ValidationError::Document { instance_path, .. }) if instance_path == "/$ref"
        ));
        let nested = json!({"dataType": "list", "items": {"dataType": "not-a-type"}});
        assert!(matches!(
            validate(DocumentKind::ValueSchema, &nested),
            Err(ValidationError::Document { instance_path, .. }) if instance_path == "/items"
        ));
    }

    #[test]
    fn environment_matches_capture_shape_and_fixed_options() {
        let mut document = environment();
        validate(DocumentKind::Environment, &document).unwrap();
        document["nativeBlasterSha256"] = json!("33".repeat(32));
        validate(DocumentKind::Environment, &document).unwrap();
        for (pointer, replacement) in [
            ("/solverOptions/timeoutSeconds", json!(61)),
            ("/solverOptions/maxRecDepth", json!(100001)),
            ("/solverOptions/maxHeartbeats", json!(1)),
            ("/modules/Lean", json!("bad hash")),
            ("/compilerSha256", json!("AA".repeat(32))),
            ("/format", json!("other-environment")),
        ] {
            let mut invalid = document.clone();
            *invalid.pointer_mut(pointer).unwrap() = replacement;
            let error = validate(DocumentKind::Environment, &invalid).unwrap_err();
            assert!(matches!(
                &error,
                ValidationError::Document { instance_path, .. } if instance_path == pointer
            ));
            assert!(error.to_string().contains(pointer));
        }
        for field in [
            "format",
            "leanVersion",
            "compilerSha256",
            "solverSha256",
            "solverOptions",
            "modules",
        ] {
            let mut invalid = document.clone();
            invalid.as_object_mut().unwrap().remove(field);
            assert!(validate(DocumentKind::Environment, &invalid).is_err());
        }
        document["$schema"] = json!(ENVIRONMENT_ID);
        assert!(validate(DocumentKind::Environment, &document).is_err());
        let mut invalid = environment();
        invalid["solverOptions"]["extra"] = json!(true);
        assert!(validate(DocumentKind::Environment, &invalid).is_err());
    }
}
