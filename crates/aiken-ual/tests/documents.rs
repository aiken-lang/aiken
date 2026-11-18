use std::collections::{BTreeMap, HashMap};

use aiken_ual::*;
use serde::{Serialize, de::DeserializeOwned};
use serde_json::{Value, json};

const BLUEPRINT: &[u8] = include_bytes!("fixtures/plutus.json");
const CONTEXT: &[u8] = include_bytes!("fixtures/checking-property.json");
const ENVIRONMENT: &[u8] = include_bytes!("fixtures/environment.json");
const ASSURANCE: &[u8] = include_bytes!("fixtures/assurance.json");
const VERIFIED: &[u8] = include_bytes!("fixtures/verified-assurance.json");

fn round_trip<T: Serialize + DeserializeOwned>(kind: DocumentKind, bytes: &[u8]) {
    let original: Value = serde_json::from_slice(bytes).unwrap();
    validate(kind, &original).unwrap();
    let document: T = serde_json::from_slice(bytes).unwrap();
    assert_eq!(serde_json::to_value(&document).unwrap(), original);
    assert_eq!(validated_bytes(kind, &document).unwrap(), bytes);
    let again: T = serde_json::from_slice(&canonical_bytes(&document).unwrap()).unwrap();
    assert_eq!(canonical_bytes(&again).unwrap(), bytes);
}

#[test]
fn document_round_trips_match_independent_canonical_goldens() {
    round_trip::<AssuranceDocument>(DocumentKind::Assurance, ASSURANCE);
    round_trip::<AssuranceDocument>(DocumentKind::Assurance, VERIFIED);
    round_trip::<CheckingContext>(DocumentKind::CheckingContext, CONTEXT);
    round_trip::<CompiledInterfaceBlueprint>(DocumentKind::Blueprint, BLUEPRINT);
    round_trip::<CheckingEnvironment>(DocumentKind::Environment, ENVIRONMENT);
}

#[test]
fn canonical_order_is_recursive_and_independent_of_hashmap_iteration() {
    let expected = b"{\n  \"a\": {\n    \"a\": 1,\n    \"z\": 2\n  },\n  \"z\": [\n    3,\n    2,\n    1\n  ]\n}\n";
    for _ in 0..32 {
        let nested: HashMap<_, _> = [("z", 2), ("a", 1)].into();
        let outer: HashMap<_, _> = [("z", json!([3, 2, 1])), ("a", json!(nested))].into();
        assert_eq!(canonical_bytes(&outer).unwrap(), expected);
    }
    assert_eq!(
        canonical_bytes(&json!({"unicode": "∀ ≥\n\""})).unwrap(),
        "{\n  \"unicode\": \"∀ ≥\\n\\\"\"\n}\n".as_bytes()
    );
}

#[test]
fn each_document_rejects_a_broken_required_contract() {
    for (kind, bytes, pointer, bad) in [
        (
            DocumentKind::Assurance,
            ASSURANCE,
            "/properties/0/statement/text",
            json!(""),
        ),
        (
            DocumentKind::Assurance,
            VERIFIED,
            "/properties/0/evidence/0/checkingContextHash/digest",
            json!("invalid"),
        ),
        (
            DocumentKind::CheckingContext,
            CONTEXT,
            "/execution/budget/steps",
            json!(-1),
        ),
        (
            DocumentKind::Blueprint,
            BLUEPRINT,
            "/validators/0/interface/invocations/0/arguments/0/source",
            json!("/parameters/-1"),
        ),
        (
            DocumentKind::Environment,
            ENVIRONMENT,
            "/compilerSha256",
            json!("00"),
        ),
    ] {
        let mut document: Value = serde_json::from_slice(bytes).unwrap();
        *document.pointer_mut(pointer).unwrap() = bad;
        assert!(validate(kind, &document).is_err(), "{kind:?}: {pointer}");
        assert!(validated_bytes(kind, &document).is_err());
    }
    assert!(validate(DocumentKind::ValueSchema, &json!({"dataType":"list"})).is_err());
}

#[test]
fn evidence_requires_context_and_target_bindings() {
    for field in [
        "checkingContextHash",
        "functionHashes",
        "scriptHash",
        "artifact",
        "tool",
    ] {
        let mut document: Value = serde_json::from_slice(VERIFIED).unwrap();
        document["properties"][0]["evidence"][0]
            .as_object_mut()
            .unwrap()
            .remove(field);
        assert!(
            validate(DocumentKind::Assurance, &document).is_err(),
            "{field}"
        );
    }
}

fn contexts(bytes: &[u8]) -> BTreeMap<String, &[u8]> {
    BTreeMap::from([("check".into(), bytes)])
}

#[test]
fn raw_blueprint_and_context_bytes_are_bound_not_normalized_json() {
    let document: AssuranceDocument = serde_json::from_slice(VERIFIED).unwrap();
    validate_bindings(&document, BLUEPRINT, &contexts(CONTEXT)).unwrap();
    let mut changed = BLUEPRINT.to_vec();
    changed.push(b' ');
    let error = validate_bindings(&document, &changed, &contexts(CONTEXT)).unwrap_err();
    assert_eq!(error.path, "blueprint.hash");
    assert!(error.message.contains("mismatch"));
    let mut changed = CONTEXT.to_vec();
    changed.push(b'\n');
    let error = validate_bindings(&document, BLUEPRINT, &contexts(&changed)).unwrap_err();
    assert_eq!(error.path, "check");
    assert!(error.message.contains("mismatch"));
    assert!(validate_bindings(&document, BLUEPRINT, &BTreeMap::new()).is_err());
}

#[test]
fn environment_binding_checks_raw_bytes_and_manifest_shape() {
    let mut context: CheckingContext = serde_json::from_slice(CONTEXT).unwrap();
    let environment = validate_environment(&context, ENVIRONMENT).unwrap();
    assert_eq!(environment.lean_version, "4.24.0");
    assert!(
        validate_environment(&context, b"{}\n")
            .unwrap_err()
            .message
            .contains("mismatch")
    );
    context.environment.hash = sha256_digest(b"{}\n");
    assert!(validate_environment(&context, b"{}\n").is_err());
}

#[test]
fn fresh_context_digest_does_not_refresh_historical_evidence() {
    let mut document: AssuranceDocument = serde_json::from_slice(VERIFIED).unwrap();
    let mut context: Value = serde_json::from_slice(CONTEXT).unwrap();
    context["execution"]["budget"]["steps"] = json!(101);
    let changed = canonical_bytes(&context).unwrap();
    document
        .checking_contexts
        .as_mut()
        .unwrap()
        .get_mut("check")
        .unwrap()
        .hash = sha256_digest(&changed);
    let error = validate_bindings(&document, BLUEPRINT, &contexts(&changed)).unwrap_err();
    assert!(
        error
            .message
            .contains("evidence checkingContextHash mismatch")
    );
}

#[test]
fn function_bytes_interface_and_evidence_hashes_must_match() {
    let mut document: AssuranceDocument = serde_json::from_slice(VERIFIED).unwrap();
    document
        .functions
        .as_mut()
        .unwrap()
        .get_mut("sample__function")
        .unwrap()
        .compiled_code = "00".into();
    assert!(
        validate_bindings(&document, BLUEPRINT, &contexts(CONTEXT))
            .unwrap_err()
            .message
            .contains("compiledCode hash mismatch")
    );

    let mut document: AssuranceDocument = serde_json::from_slice(ASSURANCE).unwrap();
    let mut context: Value = serde_json::from_slice(CONTEXT).unwrap();
    context["targets"][1]["functionInterface"]["result"] = json!({"dataType":"#integer"});
    let changed = canonical_bytes(&context).unwrap();
    document
        .checking_contexts
        .as_mut()
        .unwrap()
        .get_mut("check")
        .unwrap()
        .hash = sha256_digest(&changed);
    assert!(
        validate_bindings(&document, BLUEPRINT, &contexts(&changed))
            .unwrap_err()
            .message
            .contains("interface mismatch")
    );

    let mut document: AssuranceDocument = serde_json::from_slice(VERIFIED).unwrap();
    document.properties[0].evidence.as_mut().unwrap()[0]
        .function_hashes
        .as_mut()
        .unwrap()
        .insert("sample__function".into(), sha256_digest(b"other"));
    assert!(
        validate_bindings(&document, BLUEPRINT, &contexts(CONTEXT))
            .unwrap_err()
            .message
            .contains("functionHashes mismatch")
    );
}

#[test]
fn duplicate_targets_and_incomplete_scope_are_rejected() {
    let mut document: AssuranceDocument = serde_json::from_slice(ASSURANCE).unwrap();
    let mut context: Value = serde_json::from_slice(CONTEXT).unwrap();
    let duplicate = context["targets"][0].clone();
    context["targets"].as_array_mut().unwrap().push(duplicate);
    let changed = canonical_bytes(&context).unwrap();
    document
        .checking_contexts
        .as_mut()
        .unwrap()
        .get_mut("check")
        .unwrap()
        .hash = sha256_digest(&changed);
    assert!(
        validate_bindings(&document, BLUEPRINT, &contexts(&changed))
            .unwrap_err()
            .message
            .contains("duplicate checking target")
    );

    let mut document: AssuranceDocument = serde_json::from_slice(ASSURANCE).unwrap();
    document.properties[0].scope.functions = None;
    assert!(
        validate_bindings(&document, BLUEPRINT, &contexts(CONTEXT))
            .unwrap_err()
            .message
            .contains("exactly cover")
    );
}

#[test]
fn digest_vectors_include_cbor_wrapper_and_reject_invalid_hex() {
    assert_eq!(
        hex::encode(sha256(b"abc")),
        "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
    );
    assert_eq!(
        function_digest("46010000200101").unwrap().digest,
        "9028cd01283a3f51a726124b222ea4179c58e8feebf14bfe84d7a812b6361299"
    );
    assert_ne!(
        function_digest("46010000200101").unwrap(),
        sha256_digest(b"46010000200101")
    );
    assert_ne!(
        function_digest("46010000200101").unwrap(),
        function_digest("010000200101").unwrap()
    );
    let large = vec![0xab; 2051];
    assert_eq!(
        function_digest(&hex::encode(&large)).unwrap(),
        sha256_digest(&large)
    );
    assert!(function_digest("123").is_err());
    assert!(function_digest("xx").is_err());
    let mut digest = sha256_digest(b"abc");
    digest.alg = "SHA-256".into();
    digest.verify(b"abc").unwrap();
    digest.alg = "blake2b-224".into();
    assert!(matches!(
        digest.verify(b"abc"),
        Err(DigestError::UnsupportedAlgorithm(_))
    ));
}

#[test]
fn applied_parameters_and_ledger_execution_round_trip_without_being_executed() {
    let mut value: Value = serde_json::from_slice(CONTEXT).unwrap();
    let artifact = json!({"uri":"term.flat","hash":{"alg":"sha256","digest":"ab".repeat(32)}});
    value["targets"][0]["parameters"] = json!({
        "mode":"applied", "values":[{"parameter":"/parameters/0","term":artifact}],
        "appliedScriptHash":"cd".repeat(28), "appliedScript":artifact
    });
    value["execution"] = json!({"semanticsVariant":"C","acceptance":"ledger-script",
        "budget":{"kind":"ledger-units","exCPU":1000,"exMem":100,"costModel":artifact},
        "protocolVersion":{"major":9,"minor":0}});
    let bytes = canonical_bytes(&value).unwrap();
    round_trip::<CheckingContext>(DocumentKind::CheckingContext, &bytes);
    value["targets"][0]["parameters"]["values"][0]["parameter"] = json!("/parameters/01");
    assert!(validate(DocumentKind::CheckingContext, &value).is_err());
}

#[test]
fn optional_presence_and_open_extensions_survive_reading() {
    let mut value: Value = serde_json::from_slice(ASSURANCE).unwrap();
    value["formalFragments"] = json!([]);
    value["properties"][0]["evidence"] = json!([]);
    value["blueprint"]["hash"]["x-note"] = json!({"retained":true});
    let bytes = canonical_bytes(&value).unwrap();
    round_trip::<AssuranceDocument>(DocumentKind::Assurance, &bytes);
    let mut context: Value = serde_json::from_slice(CONTEXT).unwrap();
    context["targets"][0]["parameters"]["unknown"] = json!(true);
    assert!(serde_json::from_value::<CheckingContext>(context).is_err());
    let mut blueprint: Value = serde_json::from_slice(BLUEPRINT).unwrap();
    blueprint["validators"][0]["interface"]["invocations"][0]["arguments"][1]["source"] =
        json!("/redeemer");
    assert!(serde_json::from_value::<CompiledInterfaceBlueprint>(blueprint).is_err());
}

#[test]
fn evidence_write_api_produces_bound_readable_records() {
    let mut document: AssuranceDocument = serde_json::from_slice(ASSURANCE).unwrap();
    let context_hash = document.checking_contexts.as_ref().unwrap()["check"]
        .hash
        .clone();
    let function_hash = document.functions.as_ref().unwrap()["sample__function"]
        .hash
        .clone();
    let blueprint: CompiledInterfaceBlueprint = serde_json::from_slice(BLUEPRINT).unwrap();
    let artifact = Artifact {
        uri: "report.json".into(),
        hash: sha256_digest(b"{}\n"),
        media_type: None,
        extensions: Extensions::new(),
    };
    let record = EvidenceRecord::smt_check(
        "reader",
        EvidenceOutcome::Verified,
        "2026-11-04",
        "blaster",
        artifact,
        context_hash,
    )
    .with_script_hash(blueprint.validators[0].hash.clone().unwrap())
    .with_function_hashes(BTreeMap::from([("sample__function".into(), function_hash)]));
    document.properties[0].evidence = Some(vec![record]);
    let bytes = validated_bytes(DocumentKind::Assurance, &document).unwrap();
    let read: AssuranceDocument = serde_json::from_slice(&bytes).unwrap();
    assert_eq!(read, document);
    validate_bindings(&read, BLUEPRINT, &contexts(CONTEXT)).unwrap();
}

#[test]
fn validator_titles_resolve_before_scope_coverage_and_duplicates() {
    let mut document: AssuranceDocument = serde_json::from_slice(VERIFIED).unwrap();
    document.properties[0].scope.validators = Some(vec!["Sample validator".into()]);
    validate_bindings(&document, BLUEPRINT, &contexts(CONTEXT)).unwrap();
    document.properties[0]
        .scope
        .validators
        .as_mut()
        .unwrap()
        .push("sample__validator".into());
    assert!(
        validate_bindings(&document, BLUEPRINT, &contexts(CONTEXT))
            .unwrap_err()
            .message
            .contains("duplicate scoped validator")
    );
}
