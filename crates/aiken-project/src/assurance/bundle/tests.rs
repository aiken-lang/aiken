use super::*;
use crate::{config::ProjectConfig, export::Export, tests::TestProject};
use aiken_lang::{ast::Tracing, plutus_version::PlutusVersion};
use serde_json::{Value, json};

const FUNCTION: &str = include_str!("../fixtures/bundle_function.ak");
const VALIDATOR: &str = include_str!("../fixtures/bundle_validator.ak");
const RECURSIVE: &str = include_str!("../fixtures/bundle_recursive.ak");

fn preamble() -> ual::AssurancePreamble {
    ual::AssurancePreamble {
        title: "UAL bundle fixture".to_owned(),
        description: None,
        version: Some("1.0".to_owned()),
        authors: vec!["Fixture author".to_owned()],
        created: "2026-11-04".to_owned(),
        license: Some("Apache-2.0".to_owned()),
        extensions: BTreeMap::new(),
    }
}

fn environment() -> ual::Artifact {
    let bytes = include_bytes!("../../../../aiken-ual/tests/fixtures/environment.json");
    ual::Artifact {
        uri: "environment.json".to_owned(),
        hash: ual::sha256_digest(bytes),
        media_type: Some("application/json".to_owned()),
        extensions: BTreeMap::new(),
    }
}

fn checked(source: &str) -> (TestProject, CheckedModules) {
    let mut project = TestProject::new();
    let mut parsed = project.parse(source);
    parsed.name = "fixtures".to_owned();
    parsed.ast.name = parsed.name.clone();
    parsed.path = "fixtures.ak".into();
    let modules = CheckedModules::singleton(project.check(parsed));
    (project, modules)
}

fn draft(source: &str) -> Result<PendingBundle, Error> {
    let (project, modules) = checked(source);
    let model = super::super::validate(modules.values()).unwrap();
    let config = ProjectConfig::default(&project.package);
    let mut generator = project.new_generator(Tracing::silent());
    let mut blueprint = Blueprint::new(&config, &modules, &mut generator, false).unwrap();
    blueprint.preamble.compiler.as_mut().unwrap().version = "test-compiler".to_owned();
    lower(&model, &modules, &mut generator, &blueprint, preamble())
}

fn bundle(source: &str) -> Bundle {
    draft(source)
        .unwrap()
        .bind_environment(environment())
        .unwrap()
}

fn assert_bindings(bundle: &Bundle) {
    let borrowed = bundle
        .contexts()
        .iter()
        .map(|(id, bytes)| (id.clone(), bytes.as_slice()))
        .collect();
    ual::validate_bindings(bundle.assurance(), bundle.blueprint_bytes(), &borrowed).unwrap();
    let assurance: ual::AssuranceDocument =
        serde_json::from_slice(bundle.assurance_bytes()).unwrap();
    assert_eq!(&assurance, bundle.assurance());
    for context in bundle.contexts().values() {
        let context: ual::CheckingContext = serde_json::from_slice(context).unwrap();
        ual::validate_environment(
            &context,
            include_bytes!("../../../../aiken-ual/tests/fixtures/environment.json"),
        )
        .unwrap();
        for target in &context.targets {
            if let ual::ContextTarget::Validator(target) = target {
                assert_eq!(target.parameters, ual::ParameterBinding::Universal {});
            }
        }
    }
    assert!(
        assurance
            .properties
            .iter()
            .all(|property| property.evidence.is_none())
    );
}

fn snapshot(bundle: &Bundle) -> Value {
    assert_bindings(bundle);
    json!({
        "assurance": bundle.assurance(),
        "blueprint": serde_json::from_slice::<Value>(bundle.blueprint_bytes()).unwrap(),
        "contexts": bundle.contexts().iter().map(|(id, bytes)| (id, serde_json::from_slice::<Value>(bytes).unwrap())).collect::<BTreeMap<_, _>>(),
    })
}

#[test]
fn compiled_interface_properties_select_the_ual_profile() {
    let bundle = bundle(FUNCTION);
    let assurance = bundle.assurance();
    for property in &assurance.properties {
        let formal = property.statement.formal.as_ref().unwrap();
        let language = &assurance.languages.as_ref().unwrap()[&formal.language];
        // The pinned consumer rejects plain Lean for compiled-interface checking.
        assert_eq!(language.name, "UAL");
        assert_eq!(language.version, "1.0");
    }
}

#[test]
fn fixture_1_native_function_bundle() {
    insta::assert_json_snapshot!(snapshot(&bundle(FUNCTION)));
}

#[test]
fn fixture_2_universal_validator_bundle() {
    let bundle = bundle(VALIDATOR);
    let properties: BTreeMap<_, _> = bundle
        .assurance()
        .properties
        .iter()
        .map(|p| (p.id.as_str(), p))
        .collect();
    let source = |id| {
        properties[id]
            .statement
            .formal
            .as_ref()
            .unwrap()
            .source
            .as_deref()
            .unwrap()
    };
    assert!(
        source("positive_minimum")
            .contains("∃ (ref : _) (d : _), ctx.scriptContextScriptInfo = SpendingScript ref d")
    );
    assert!(
        source("fallback_rejects").contains(
            "¬ (∃ (ref : _) (d : _), ctx.scriptContextScriptInfo = SpendingScript ref d)"
        )
    );
    insta::assert_json_snapshot!(snapshot(&bundle));
}

#[test]
fn fixture_4_recursive_function_bundle() {
    insta::assert_json_snapshot!(snapshot(&bundle(RECURSIVE)));
}

#[test]
fn exported_bytes_and_context_hash_follow_function_body() {
    for source in [
        FUNCTION.to_owned(),
        FUNCTION.replace("proposed > old", "proposed >= old"),
    ] {
        let (project, modules) = checked(&source);
        let mut generator = project.new_generator(Tracing::silent());
        let (module, function) = modules
            .functions()
            .find(|(_, f)| f.name == "outbids")
            .unwrap();
        let export = Export::from_function(
            function,
            module,
            &mut generator,
            &modules,
            &PlutusVersion::V3,
        )
        .unwrap();
        let bundle = bundle(&source);
        let actual = &bundle.assurance().functions.as_ref().unwrap()["fixtures__outbids"];
        assert_eq!(
            actual.compiled_code,
            serde_json::to_value(export).unwrap()["compiledCode"]
        );
        assert_eq!(
            actual.hash,
            ual::function_digest(&actual.compiled_code).unwrap()
        );
    }
    let before = bundle(FUNCTION);
    let after = bundle(&FUNCTION.replace("proposed > old", "proposed >= old"));
    assert_ne!(
        before.assurance().functions.as_ref().unwrap()["fixtures__outbids"].hash,
        after.assurance().functions.as_ref().unwrap()["fixtures__outbids"].hash
    );
    assert_ne!(
        before.assurance().checking_contexts.as_ref().unwrap()["outbids"].hash,
        after.assurance().checking_contexts.as_ref().unwrap()["outbids"].hash
    );
    assert_eq!(before.blueprint_bytes(), after.blueprint_bytes());
}

#[test]
fn mixed_settings_report_both_scope_locations() {
    for (steps, semantics) in [(101, "e"), (100, "c")] {
        let source = format!(
            "{FUNCTION}\n@onchain(steps: {steps}, semantics: {semantics})\npub fn other() {{ True }}\n//@ PROPERTY mixed for outbids other \"mixed policy\" True @//"
        );
        let Error::Annotations { errors } = draft(&source).unwrap_err() else {
            panic!("expected annotation diagnostic")
        };
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].kind, ErrorKind::MixedExecutionSettings);
        assert_eq!(&source[errors[0].location.range()], "other");
        assert_eq!(&source[errors[0].related[0].location.range()], "outbids");
    }
}

#[test]
fn environment_binding_changes_context_hash_without_recompiling() {
    let first = bundle(FUNCTION);
    let mut changed = environment();
    changed.hash = ual::sha256_digest(b"different environment bytes supplied by caller");
    let second = draft(FUNCTION).unwrap().bind_environment(changed).unwrap();
    assert_eq!(first.blueprint_bytes(), second.blueprint_bytes());
    assert_eq!(first.assurance().functions, second.assurance().functions);
    assert_ne!(
        first.assurance().checking_contexts,
        second.assurance().checking_contexts
    );
    let mut invalid = environment();
    invalid.hash.digest = "not a digest".to_owned();
    assert!(draft(FUNCTION).unwrap().bind_environment(invalid).is_err());
}

#[test]
fn function_interfaces_copy_all_merged_definitions() {
    let source = format!(
        "{FUNCTION}\n@onchain(steps: 100, semantics: e)\npub fn bytes(value: ByteArray) {{ value }}\n//@ PROPERTY both for outbids bytes \"shared definitions\" True @//"
    );
    let bundle = bundle(&source);
    assert_bindings(&bundle);
    let context: ual::CheckingContext = serde_json::from_slice(&bundle.contexts()["both"]).unwrap();
    for target in context.targets {
        let ual::ContextTarget::Function(target) = target else {
            panic!("function target")
        };
        assert_eq!(
            target.function_interface.definitions,
            bundle.assurance().definitions
        );
        assert!(
            target
                .function_interface
                .definitions
                .as_ref()
                .unwrap()
                .contains_key("ByteArray")
        );
        assert!(
            target
                .function_interface
                .definitions
                .as_ref()
                .unwrap()
                .contains_key("Int")
        );
    }
}

#[test]
fn duplicate_handler_scopes_keep_one_checking_target_and_all_premises() {
    let source = format!(
        "{VALIDATOR}\n//@ PROPERTY both for vesting.spend vesting.else vesting.spend \"all selectors\" ∀ (ctx : ScriptContext), True @//"
    );
    let bundle = bundle(&source);
    assert_bindings(&bundle);
    let context: ual::CheckingContext = serde_json::from_slice(&bundle.contexts()["both"]).unwrap();
    assert_eq!(context.targets.len(), 1);
    let property = bundle
        .assurance()
        .properties
        .iter()
        .find(|p| p.id == "both")
        .unwrap();
    assert_eq!(
        property.scope.validators.as_ref().unwrap(),
        &["fixtures__vesting"]
    );
    let source = property
        .statement
        .formal
        .as_ref()
        .unwrap()
        .source
        .as_ref()
        .unwrap();
    assert!(
        source.contains("∃ (ref : _) (d : _), ctx.scriptContextScriptInfo = SpendingScript ref d")
    );
    assert!(
        source.contains(
            "¬ (∃ (ref : _) (d : _), ctx.scriptContextScriptInfo = SpendingScript ref d)"
        )
    );
}

#[test]
fn function_only_project_reports_pinned_blueprint_constraint() {
    let source = "@onchain(steps: 100, semantics: e) pub fn f() { True }\n//@ PROPERTY p for f \"function\" True @//";
    assert!(matches!(draft(source), Err(Error::NoValidators)));
}

#[test]
fn function_definition_graph_rejects_missing_and_unguarded_references() {
    for document in [
        json!({"functions": {"f": {"arguments": [{"$ref": "#/definitions/missing"}], "result": {}}}}),
        json!({"definitions": {"Loop": {"$ref": "#/definitions/Loop"}}, "functions": {"f": {"arguments": [], "result": {"$ref": "#/definitions/Loop"}}}}),
    ] {
        assert!(dialect::graph::validate(&document).is_err());
    }
}

#[test]
fn applied_values_reject_partial_extra_malformed_and_wrongly_typed_parameters() {
    for values in ["", "00,01", "40", "ff", "0001"] {
        let source =
            VALIDATOR.replace("for vesting.spend", &format!("for vesting.spend[{values}]"));
        assert!(draft(&source).is_err(), "accepted parameters [{values}]");
    }
    let source = VALIDATOR
        .replace("vesting(minimum: Int)", "vesting")
        .replace("minimum > 0", "True")
        .replace("for vesting.spend", "for vesting.spend[]");
    assert!(
        draft(&source).is_err(),
        "accepted application without parameters"
    );
}

#[test]
fn applied_parameters_preserve_declaration_order_and_data_encoding() {
    let source = VALIDATOR
        .replace(
            "vesting(minimum: Int)",
            "vesting(minimum: Int, owner: ByteArray)",
        )
        .replace("minimum > 0", "minimum > 0 && owner == #\"ab\"")
        .replace("for vesting.spend", "for vesting.spend[182a,41ab]");
    let result = bundle(&source);
    let context: ual::CheckingContext =
        serde_json::from_slice(&result.contexts()["positive_minimum"]).unwrap();
    let ual::ContextTarget::Validator(target) = &context.targets[0] else {
        panic!("expected validator")
    };
    let ual::ParameterBinding::Applied {
        values,
        applied_script,
        applied_script_hash,
    } = &target.parameters
    else {
        panic!("expected applied binding")
    };
    let blueprint: ual::CompiledInterfaceBlueprint =
        serde_json::from_slice(result.blueprint_bytes()).unwrap();
    let mut cbor_buffer = Vec::new();
    let mut flat_buffer = Vec::new();
    let template = uplc::ast::Program::<uplc::ast::DeBruijn>::from_hex(
        blueprint.validators[0].compiled_code.as_ref().unwrap(),
        &mut cbor_buffer,
        &mut flat_buffer,
    )
    .unwrap();
    let mut expected = template;
    for (index, cbor) in ["182a", "41ab"].iter().enumerate() {
        assert_eq!(values[index].parameter, format!("/parameters/{index}"));
        let bytes = &result.artifacts()[&values[index].term.uri];
        values[index].term.hash.verify(bytes).unwrap();
        // Decode as a term, not a program: Flat artifacts have no version prefix.
        let term: uplc::ast::Term<uplc::ast::DeBruijn> = pallas_codec::flat::decode(bytes).unwrap();
        let data = uplc::plutus_data(&hex::decode(cbor).unwrap()).unwrap();
        assert_eq!(term, uplc::ast::Term::data(data.clone()));
        expected = expected.apply_data(data);
    }
    let bytes = &result.artifacts()[&applied_script.uri];
    applied_script.hash.verify(bytes).unwrap();
    assert_eq!(*bytes, expected.to_cbor().unwrap());
    assert_eq!(
        applied_script_hash,
        &uplc::ast::SerializableProgram::PlutusV3Program(expected)
            .compiled_code_and_hash()
            .0
            .to_string()
    );
    let swapped = source.replace("[182a,41ab]", "[41ab,182a]");
    assert!(
        draft(&swapped).is_err(),
        "parameter schemas must follow declaration order"
    );
}
