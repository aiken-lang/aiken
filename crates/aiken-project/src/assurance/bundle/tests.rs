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

const SPENDING: &str = "∃ (ref : _) (d : _), ctx.scriptContextScriptInfo = SpendingScript ref d";

fn formal_source<'a>(bundle: &'a Bundle, id: &str) -> &'a str {
    bundle
        .assurance()
        .properties
        .iter()
        .find(|p| p.id == id)
        .unwrap()
        .statement
        .formal
        .as_ref()
        .unwrap()
        .source
        .as_deref()
        .unwrap()
}

fn rejected(source: &str) -> Vec<super::super::Error> {
    match draft(source) {
        Err(Error::Annotations { errors }) => errors,
        other => panic!("expected annotation diagnostics, got {other:?}"),
    }
}

#[test]
fn compatible_repeated_scopes_keep_one_checking_target_and_every_premise() {
    let source = format!(
        "{VALIDATOR}\n//@ PROPERTY both for vesting.spend vesting vesting.spend \"all selectors\" ∀ (minimum : Int) (ctx : ScriptContext), isUnsuccessful (fixtures__vesting minimum (toLedgerData ctx)) @//"
    );
    let bundle = bundle(&source);
    assert_bindings(&bundle);
    let context: ual::CheckingContext = serde_json::from_slice(&bundle.contexts()["both"]).unwrap();
    assert_eq!(context.targets.len(), 1);
    let ual::ContextTarget::Validator(target) = &context.targets[0] else {
        panic!("validator target")
    };
    assert_eq!(target.purpose, ual::Purpose::Spend);
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
    // Target deduplication keeps one checking target; it never drops a selector's premise.
    assert_eq!(formal_source(&bundle, "both").matches(SPENDING).count(), 2);
    let again = super::tests::bundle(&source);
    assert_eq!(again.assurance_bytes(), bundle.assurance_bytes());
}

#[test]
fn contradictory_purpose_scopes_are_rejected_before_lowering() {
    // The review's reproduction: spend is explicit on vesting, so vesting.else excludes
    // it, and the conjoined premises would make this impossible conclusion "verify".
    let source = format!(
        "{VALIDATOR}\n//@ PROPERTY vacuous_impossible for vesting.spend vesting.else\n  \"An impossible conclusion should never verify.\"\n  ∀ (minimum : Int) (ctx : ScriptContext),\n    isUnsuccessful (fixtures__vesting minimum (toLedgerData ctx)) ∧ False\n@//"
    );
    let errors = rejected(&source);
    assert_eq!(errors.len(), 1);
    let error = &errors[0];
    assert_eq!(error.kind, ErrorKind::IncompatiblePurposes);
    assert_eq!(&source[error.location.range()], "vesting.else");
    assert_eq!(error.related.len(), 1);
    assert_eq!(&source[error.related[0].location.range()], "vesting.spend");
    for fragment in [
        "\"vacuous_impossible\"",
        "'vesting.spend' admits {spend}",
        "'vesting.else' admits {mint, withdraw, publish, vote, propose}",
        "same 'ctx'",
        "vacuously",
    ] {
        assert!(
            error.message.contains(fragment),
            "{fragment}: {}",
            error.message
        );
    }
    assert!(error.help.as_ref().unwrap().contains("separate property"));

    // Two distinct handlers can't both describe one context either.
    let source = format!(
        "{VALIDATOR}\n//@ PROPERTY two for vesting.spend vesting.mint \"two purposes\" ∀ (minimum : Int) (ctx : ScriptContext), True @//"
    );
    let source = source.replace(
        "  else(_ctx: Data) {",
        "  mint(_redeemer: Data, _policy: ByteArray, _tx: Data) {\n    True\n  }\n\n  else(_ctx: Data) {",
    );
    let errors = rejected(&source);
    assert_eq!(errors.len(), 1);
    assert_eq!(errors[0].kind, ErrorKind::IncompatiblePurposes);
    assert_eq!(&source[errors[0].location.range()], "vesting.mint");
}

#[test]
fn cross_validator_scopes_follow_each_validators_own_handlers() {
    let minter = "@onchain(steps: 2500, semantics: e)\nvalidator minter {\n  mint(_redeemer: Data, _policy: ByteArray, _tx: Data) {\n    True\n  }\n\n  else(_ctx: Data) {\n    fail\n  }\n}\n";
    let with = |scopes: &str| {
        format!(
            "{VALIDATOR}\n{minter}\n//@ PROPERTY pair for {scopes} \"pair\" ∀ (minimum : Int) (ctx : ScriptContext), isUnsuccessful (fixtures__vesting minimum (toLedgerData ctx)) @//"
        )
    };
    // minter.else excludes only mint, so it admits spend; both premises can hold.
    let accepted = draft(&with("vesting.spend minter.else")).unwrap();
    let pair = accepted
        .assurance()
        .properties
        .iter()
        .find(|p| p.id == "pair")
        .unwrap();
    let source = pair
        .statement
        .formal
        .as_ref()
        .unwrap()
        .source
        .as_deref()
        .unwrap();
    assert!(source.contains(SPENDING));
    assert!(source.contains("¬ (∃ (cs : _), ctx.scriptContextScriptInfo = MintingScript cs)"));
    assert!(draft(&with("vesting.else minter.else")).is_ok());

    for (scopes, culprit) in [
        ("vesting.spend minter.mint", "minter.mint"),
        ("minter.mint minter.else", "minter.else"),
    ] {
        let source = with(scopes);
        let errors = rejected(&source);
        assert_eq!(errors.len(), 1, "{scopes}");
        assert_eq!(errors[0].kind, ErrorKind::IncompatiblePurposes, "{scopes}");
        assert_eq!(&source[errors[0].location.range()], culprit, "{scopes}");
    }
}

#[test]
fn else_scope_of_a_validator_handling_every_purpose_is_rejected() {
    // Aiken rejects an explicit fallback here, but `else` still names the implicit one.
    let source = "@onchain(steps: 100, semantics: e)\nvalidator full {\n  spend(_d: Option<Data>, _r: Data, _o: Data, _tx: Data) { True }\n  mint(_r: Data, _p: ByteArray, _tx: Data) { True }\n  withdraw(_r: Data, _c: Data, _tx: Data) { True }\n  publish(_r: Data, _c: Data, _tx: Data) { True }\n  vote(_r: Data, _v: Data, _tx: Data) { True }\n  propose(_r: Data, _p: Data, _tx: Data) { True }\n}\n//@ PROPERTY nothing for full.else \"no purpose left\" ∀ (ctx : ScriptContext), True @//";
    let errors = rejected(source);
    assert_eq!(errors.len(), 1);
    assert_eq!(errors[0].kind, ErrorKind::IncompatiblePurposes);
    assert_eq!(&source[errors[0].location.range()], "full.else");
    assert!(errors[0].message.contains("admits no script purpose"));
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

fn multi_module_bundle(order: &[usize]) -> Bundle {
    let sources = [
        (
            "proofs/helpers",
            "//@ PREDICATE\ndef larger (old proposed : Int) : Prop := old < proposed\ndef pick {α : Type} (x : α) : α := x\n@//\npub fn identity(value: Int) -> Int { value }",
        ),
        (
            "proofs/bridge",
            "use proofs/helpers\npub fn same(value: Int) -> Int { helpers.identity(value) }",
        ),
        (
            "example",
            "use proofs/bridge\n//@ PREDICATE\ndef positive (x : Int) : Prop := proofs.helpers.larger 0 x\n@//\npub fn same(value: Int) -> Int { bridge.same(value) }",
        ),
        ("a/b", "//@ PREDICATE def f : Prop := True @//"),
        ("a.b", "//@ PREDICATE def f : Prop := False @//"),
        (
            "fixtures",
            "use example\n//@ PREDICATE\ndef raised (old proposed : Int) : Prop := proofs.helpers.larger (proofs.helpers.pick old) proposed\n@//\n@onchain(steps: 100, semantics: e)\npub fn outbids(old: Int, proposed: Int) -> Bool { example.same(proposed) > old }\n//@ PROPERTY outbids for outbids \"qualified\" ∀ (old proposed : Int), raised old proposed → «example».positive 1 → fixtures__outbids_returns (Data.I old) (Data.I proposed) true @//\nvalidator companion { else(_ctx: Data) { fail } }",
        ),
    ];
    let ordered: Vec<_> = order.iter().map(|&index| sources[index]).collect();
    project_draft(&ordered)
        .unwrap()
        .bind_environment(environment())
        .unwrap()
}

/// Check `sources` as one project, in the given order, and lower it.
fn project_draft(sources: &[(&str, &str)]) -> Result<PendingBundle, Error> {
    let mut project = TestProject::new();
    let mut modules = std::collections::HashMap::new();
    for &(name, source) in sources {
        let mut parsed = project.parse(source);
        parsed.name = name.to_owned();
        parsed.ast.name = name.to_owned();
        parsed.path = format!("{name}.ak").into();
        let module = project.check(parsed);
        modules.insert(name.to_owned(), module);
    }
    let modules = CheckedModules::from(modules);
    let model = super::super::validate(modules.values()).unwrap();
    let config = ProjectConfig::default(&project.package);
    let mut generator = project.new_generator(Tracing::silent());
    let mut blueprint = Blueprint::new(&config, &modules, &mut generator, false).unwrap();
    blueprint.preamble.compiler.as_mut().unwrap().version = "test-compiler".to_owned();
    lower(&model, &modules, &mut generator, &blueprint, preamble())
}

#[test]
fn fragments_emit_schema_legal_ids_and_qualified_declarations() {
    // `lower` validates against the pinned schema; reaching here means the ids passed.
    let bundle = multi_module_bundle(&[0, 1, 2, 3, 4, 5]);
    assert_bindings(&bundle);
    let fragments = bundle.assurance().formal_fragments.as_ref().unwrap();
    let identities: Vec<_> = fragments
        .iter()
        .map(|fragment| {
            (
                fragment.id.as_str(),
                fragment.imports.clone().unwrap_or_default(),
            )
        })
        .collect();
    assert_eq!(
        identities,
        [
            ("a-b", vec![]),
            ("a.b", vec![]),
            ("example", vec!["proofs.helpers".to_owned()]),
            ("fixtures", vec!["example".to_owned()]),
            ("proofs.helpers", vec![]),
        ]
    );
    let source = |id: &str| {
        fragments
            .iter()
            .find(|fragment| fragment.id == id)
            .unwrap()
            .source
            .as_str()
    };
    assert_eq!(
        source("proofs.helpers"),
        "\ndef larger (old proposed : Int) : Prop := old < proposed\ndef pick {α : Type} (x : α) : α := x\n\n-- Module-qualified copies in namespace proofs.helpers\n\ndef proofs.helpers.larger (old proposed : Int) : Prop := old < proposed\ndef proofs.helpers.pick {α : Type} (x : α) : α := x\n"
    );
    assert_eq!(
        source("example"),
        "\ndef positive (x : Int) : Prop := proofs.helpers.larger 0 x\n\n-- Module-qualified copies in namespace «example»\n\ndef «example».positive (x : Int) : Prop := proofs.helpers.larger 0 x\n"
    );
    assert!(source("a-b").ends_with("def «a.b».f : Prop := False "));
    assert!(source("a.b").ends_with("def a.b.f : Prop := True "));
    let formal = bundle.assurance().properties[0]
        .statement
        .formal
        .as_ref()
        .unwrap();
    assert_eq!(formal.uses.as_deref(), Some(&["fixtures".to_owned()][..]));
    // Neither check order of independent modules nor (randomized) map order
    // changes a byte. Imports must be checked before their importers.
    let reordered = multi_module_bundle(&[4, 3, 0, 1, 2, 5]);
    assert_eq!(bundle.assurance_bytes(), reordered.assurance_bytes());
}

const COMPANION: &str = "validator companion { else(_ctx: Data) { fail } }";

fn project_rejected(sources: &[(&str, &str)]) -> Vec<super::super::Error> {
    match project_draft(sources) {
        Err(Error::Annotations { errors }) => errors,
        other => panic!("expected annotation diagnostics, got {other:?}"),
    }
}

/// Every emitted document, serialized and validated against the vendored schemas.
fn assert_schema_valid(bundle: &Bundle) {
    let value = |bytes: &[u8]| serde_json::from_slice::<Value>(bytes).unwrap();
    ual::validate(
        ual::DocumentKind::Assurance,
        &value(bundle.assurance_bytes()),
    )
    .unwrap();
    ual::validate(
        ual::DocumentKind::Blueprint,
        &value(bundle.blueprint_bytes()),
    )
    .unwrap();
    for bytes in bundle.contexts().values() {
        ual::validate(ual::DocumentKind::CheckingContext, &value(bytes)).unwrap();
    }
    let contexts = bundle
        .contexts()
        .iter()
        .map(|(id, bytes)| (id.clone(), bytes.as_slice()))
        .collect();
    ual::validate_bindings(bundle.assurance(), bundle.blueprint_bytes(), &contexts).unwrap();
}

#[test]
fn dotted_module_validators_keep_their_module_and_get_lean_bound_ids() {
    let dotted = "@onchain(steps: 2500, semantics: e)\nvalidator v(minimum: Int) {\n  spend(_d: Option<Data>, _r: Data, _o: Data, _t: Data) { minimum > 0 }\n  else(_ctx: Data) { fail }\n}\n//@ PROPERTY dotted for v.spend \"Dotted module.\" ∀ (minimum : Int) (ctx : ScriptContext), minimum ≤ 0 → isUnsuccessful (fixtures_extra__v minimum (toLedgerData ctx)) @//\n//@ PROPERTY dotted_applied for v.spend[01] \"Applied.\" ∀ (ctx : ScriptContext), isUnsuccessful (fixtures_extra__v (toLedgerData ctx)) @//";
    let sources = [("fixtures.extra", dotted), ("fixtures", COMPANION)];
    let first = project_draft(&sources)
        .unwrap()
        .bind_environment(environment())
        .unwrap();
    assert_schema_valid(&first);
    let blueprint: ual::CompiledInterfaceBlueprint =
        serde_json::from_slice(first.blueprint_bytes()).unwrap();
    let identities: Vec<_> = blueprint
        .validators
        .iter()
        .map(|v| (v.id.as_str(), v.title.as_str()))
        .collect();
    assert_eq!(
        identities,
        [
            ("fixtures__companion", "fixtures.companion"),
            ("fixtures_extra__v", "fixtures.extra.v")
        ]
    );
    for property in ["dotted", "dotted_applied"] {
        let context: ual::CheckingContext =
            serde_json::from_slice(&first.contexts()[property]).unwrap();
        let ual::ContextTarget::Validator(target) = &context.targets[0] else {
            panic!("expected validator")
        };
        assert_eq!(target.validator, "fixtures_extra__v");
        // The applied path finds the dotted module's template by its own title.
        assert_eq!(
            matches!(target.parameters, ual::ParameterBinding::Applied { .. }),
            property == "dotted_applied"
        );
    }
    // Repeated generation is byte-identical, in either module check order.
    let reversed = [sources[1], sources[0]];
    let second = project_draft(&reversed)
        .unwrap()
        .bind_environment(environment())
        .unwrap();
    assert_eq!(first.assurance_bytes(), second.assurance_bytes());
    assert_eq!(first.blueprint_bytes(), second.blueprint_bytes());
    assert_eq!(first.contexts(), second.contexts());
    assert_eq!(first.artifacts(), second.artifacts());
}

#[test]
fn dotted_and_underscored_modules_merging_to_one_id_are_reported() {
    let validator = "validator v { else(_ctx: Data) { fail } }";
    let fixtures = format!(
        "{COMPANION}\n@onchain(steps: 100, semantics: e)\npub fn f(x: Int) -> Int {{ x }}\n//@ PROPERTY p for f \"f\" ∀ (x : Int), fixtures__f_returns (Data.I x) (Data.I x) @//"
    );
    let error = project_draft(&[
        ("a.b", validator),
        ("a_b", validator),
        ("fixtures", &fixtures),
    ])
    .unwrap_err();
    let Error::Blueprint(dialect::Error::TargetIdCollision { id, first, second }) = error else {
        panic!("expected target id collision, got {error:?}")
    };
    assert_eq!(
        (id.as_str(), first.as_str(), second.as_str()),
        ("a_b__v", "a.b.v", "a_b.v")
    );
}

fn function_pair(second: &str, formula: &str) -> String {
    format!(
        "@onchain(steps: 100, semantics: e)\npub fn f(x: Int) -> Int {{ x }}\n@onchain(steps: 100, semantics: e)\npub fn {second}(x: Int) -> Int {{ x }}\n{formula}\n{COMPANION}"
    )
}

#[test]
fn functions_colliding_with_generated_function_declarations_are_rejected() {
    // Both functions are called, so no disconnected-target check can mask the collision.
    for (second, role) in [
        ("f_returns", "`_returns` predicate"),
        ("f_script", "`_script` program"),
    ] {
        let source = function_pair(
            second,
            &format!(
                "//@ PROPERTY both for f {second} \"both\" ∀ (x : Int), fixtures__f_returns (Data.I x) (Data.I x) ∧ fixtures__{second}_returns (Data.I x) (Data.I x) @//"
            ),
        );
        let errors = rejected(&source);
        assert_eq!(errors.len(), 1, "{second}: {errors:?}");
        assert_eq!(errors[0].kind, ErrorKind::GeneratedNameCollision);
        assert_eq!(
            errors[0].message,
            format!(
                "Generated Lean name 'fixtures__{second}' is declared by both function 'fixtures.f' ({role}) and function 'fixtures.{second}' (function binding) in the consumer environment of property 'both'."
            )
        );
        // Labels: the second function, the first function, and the property.
        assert!(source[errors[0].location.range()].starts_with(&format!("pub fn {second}(")));
        assert_eq!(errors[0].related.len(), 2);
    }
}

#[test]
fn generated_suffixes_in_separate_property_environments_are_allowed() {
    for second in ["f_returns", "f_script"] {
        let source = function_pair(
            second,
            &format!(
                "//@ PROPERTY first for f \"f\" ∀ (x : Int), fixtures__f_returns (Data.I x) (Data.I x) @//\n//@ PROPERTY second for {second} \"g\" ∀ (x : Int), fixtures__{second}_returns (Data.I x) (Data.I x) @//"
            ),
        );
        let bundle = bundle(&source);
        assert_schema_valid(&bundle);
        let functions = bundle.assurance().functions.as_ref().unwrap();
        assert!(functions.contains_key("fixtures__f"));
        assert!(functions.contains_key(&format!("fixtures__{second}")));
    }
}

#[test]
fn validator_declarations_collide_in_every_property_environment() {
    // `v` declares `fixtures_v_hash`; validator `v_hash` binds the same title name.
    let source = format!(
        "validator v {{ else(_ctx: Data) {{ fail }} }}\nvalidator v_hash {{ else(_ctx: Data) {{ fail }} }}\n{FUNCTION}"
    );
    let errors = rejected(&source);
    assert_eq!(errors.len(), 1, "{errors:?}");
    assert_eq!(
        errors[0].message,
        "Generated Lean name 'fixtures_v_hash' is declared by both validator 'fixtures.v' (`_hash` constant) and validator 'fixtures.v_hash' (title binding) in the consumer environment of property 'outbids'."
    );
    assert!(
        errors[0]
            .help
            .as_ref()
            .unwrap()
            .contains("every blueprint validator")
    );

    // Module `m_`'s title binding spells module `m`'s id binding.
    let fixtures = format!(
        "{COMPANION}\n@onchain(steps: 100, semantics: e)\npub fn f(x: Int) -> Int {{ x }}\n//@ PROPERTY p for f \"f\" ∀ (x : Int), fixtures__f_returns (Data.I x) (Data.I x) @//"
    );
    let validator = "validator v { else(_ctx: Data) { fail } }";
    let errors = project_rejected(&[("m", validator), ("m_", validator), ("fixtures", &fixtures)]);
    assert_eq!(errors.len(), 1, "{errors:?}");
    assert_eq!(
        errors[0].message,
        "Generated Lean name 'm__v' is declared by both validator 'm.v' (id binding) and validator 'm_.v' (title binding) in the consumer environment of property 'p'."
    );
}

#[test]
fn helpers_colliding_with_generated_declarations_are_rejected() {
    let source = FUNCTION.replace(
        "def larger (old proposed : Int) : Prop := old < proposed",
        "def larger (old proposed : Int) : Prop := old < proposed\ndef fixtures__outbids_script : Prop := True",
    );
    let errors = rejected(&source);
    assert_eq!(errors.len(), 1, "{errors:?}");
    assert_eq!(
        errors[0].message,
        "Generated Lean name 'fixtures__outbids_script' is declared by both function 'fixtures.outbids' (`_script` program) and helper 'fixtures__outbids_script' in module 'fixtures' in the consumer environment of property 'outbids'."
    );
    assert_eq!(
        &source[errors[0].location.range()],
        "fixtures__outbids_script"
    );
}

#[test]
fn applied_targets_declare_no_parameter_count() {
    let helper = "//@ PREDICATE\ndef fixtures_vesting_paramCount : Prop := True\n@//\n";
    let universal = format!("{helper}{VALIDATOR}");
    let errors = rejected(&universal);
    assert_eq!(errors.len(), 1, "{errors:?}");
    assert!(errors[0].message.contains("validator 'fixtures.vesting' (`_paramCount` constant) and helper 'fixtures_vesting_paramCount'"));
    // Specialized checking targets drop their parameters, so no `_paramCount` exists.
    let applied = universal
        .replace("for vesting.spend", "for vesting.spend[01]")
        .replace("for vesting.else", "for vesting.else[01]");
    assert_schema_valid(&bundle(&applied));
}
