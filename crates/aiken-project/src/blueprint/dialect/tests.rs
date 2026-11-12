use crate::{
    assurance::emitted_target_id,
    blueprint::{
        Blueprint,
        schema::{Data, Declaration, Items, Schema},
    },
    config::{PlutusVersion, ProjectConfig},
    module::CheckedModules,
    tests::TestProject,
};
use aiken_lang::ast::Tracing;
use aiken_ual::{
    CallingConvention, CompiledInterfaceBlueprint, DocumentKind, InvocationArgument, Purpose,
};
use serde_json::json;

const MODULE: &str = "fixtures/contracts";

fn compile(source: &str) -> Blueprint {
    let mut project = TestProject::new();
    let mut parsed = project.parse(source);
    parsed.name = MODULE.to_owned();
    parsed.ast.name = MODULE.to_owned();
    let modules = CheckedModules::singleton(project.check(parsed));
    let config = ProjectConfig::default(&project.package);
    let mut generator = project.new_generator(Tracing::silent());
    let mut blueprint = Blueprint::new(&config, &modules, &mut generator, false).unwrap();
    // Git/compiler metadata is not part of these fixtures' behavior under test.
    blueprint.preamble.compiler.as_mut().unwrap().version = "test-compiler".to_owned();
    blueprint
}

fn assert_conversion(legacy: &Blueprint, names: &[&str]) -> CompiledInterfaceBlueprint {
    let before = serde_json::to_vec(legacy).unwrap();
    let compiled = legacy.to_compiled_interface().unwrap();
    assert_eq!(serde_json::to_vec(legacy).unwrap(), before);
    assert_eq!(legacy.to_compiled_interface().unwrap(), compiled);

    let original = serde_json::to_value(legacy).unwrap();
    let document = serde_json::to_value(&compiled).unwrap();
    aiken_ual::validate(DocumentKind::Blueprint, &document).unwrap();
    assert_eq!(document["preamble"], original["preamble"]);
    assert_eq!(document["definitions"], original["definitions"]);
    assert_eq!(compiled.validators.len(), names.len());

    for name in names {
        let title = format!("{MODULE}.{name}");
        let matches = compiled
            .validators
            .iter()
            .filter(|validator| validator.title == title)
            .collect::<Vec<_>>();
        assert_eq!(matches.len(), 1, "missing or duplicated validator {title}");
        let validator = matches[0];
        assert_eq!(validator.id, emitted_target_id(MODULE, name));
        assert_eq!(
            validator.interface.calling_convention,
            CallingConvention::LedgerV3
        );
        assert_eq!(
            validator
                .interface
                .invocations
                .iter()
                .map(|invocation| invocation.purpose)
                .collect::<Vec<_>>(),
            [
                Purpose::Spend,
                Purpose::Mint,
                Purpose::Withdraw,
                Purpose::Publish,
                Purpose::Vote,
                Purpose::Propose,
            ]
        );
        let parameters = validator.parameters.as_deref().unwrap_or_default();
        let mut arguments = (0..parameters.len())
            .map(|index| InvocationArgument::Parameter {
                source: format!("/parameters/{index}"),
            })
            .collect::<Vec<_>>();
        arguments.push(InvocationArgument::Context {});
        for invocation in &validator.interface.invocations {
            assert_eq!(invocation.arguments, arguments);
            // The pinned V3 consumer still selects each descriptive payload
            // slot for every invocation; a spend-only datum selector is invalid.
            let purpose = serde_json::to_value(invocation.purpose).unwrap();
            let value = serde_json::to_value(validator).unwrap();
            for slot in ["datum", "redeemer"] {
                if let Some(alternatives) = value[slot]["oneOf"].as_array() {
                    let selected = alternatives
                        .iter()
                        .filter(|argument| argument["purpose"] == purpose)
                        .collect::<Vec<_>>();
                    assert_eq!(selected.len(), 1, "{slot} selection for {purpose}");
                    if slot == "datum" && invocation.purpose != Purpose::Spend {
                        assert_eq!(selected[0]["schema"], json!({}));
                    }
                }
            }
        }
    }

    // Check EVERY legacy handler, including else, before snapshots redact bytes.
    for handler in &legacy.validators {
        let (module, name) = handler.get_module_and_name();
        let validator = compiled
            .validators
            .iter()
            .find(|validator| validator.id == emitted_target_id(module, name))
            .unwrap();
        let actual = serde_json::to_value(validator).unwrap();
        let expected = serde_json::to_value(handler).unwrap();
        assert_eq!(
            actual["compiledCode"], expected["compiledCode"],
            "{}",
            handler.title
        );
        assert_eq!(actual["hash"], expected["hash"], "{}", handler.title);
        assert_eq!(
            actual["parameters"], expected["parameters"],
            "{}",
            handler.title
        );
    }
    compiled
}

#[test]
fn single_handler() {
    let legacy = compile(indoc::indoc! {r#"
        validator first {
          spend(_datum: Option<Data>, _redeemer: Data, _output: Data, _transaction: Data) {
            True
          }
        }

        validator second {
          spend(_datum: Option<Data>, _redeemer: Data, _output: Data, _transaction: Data) {
            True
          }
        }
    "#});
    let compiled = assert_conversion(&legacy, &["first", "second"]);
    let first = compiled
        .validators
        .iter()
        .find(|v| v.title.ends_with(".first"))
        .unwrap();
    let second = compiled
        .validators
        .iter()
        .find(|v| v.title.ends_with(".second"))
        .unwrap();
    assert_eq!(first.id, "fixtures_contracts__first");
    assert_eq!(first.title, "fixtures/contracts.first");
    assert_eq!(second.id, "fixtures_contracts__second");
    assert_eq!(second.title, "fixtures/contracts.second");
    // Identical programs still represent two separately addressable validators.
    assert_eq!(first.compiled_code, second.compiled_code);
    assert_eq!(first.hash, second.hash);
    assert_ne!(first.id, second.id);
    insta::assert_json_snapshot!(compiled, {
        ".validators[].compiledCode" => "<redacted>",
        ".validators[].hash" => "<redacted>",
    });
}

#[test]
fn multi_handler_with_explicit_else() {
    let legacy = compile(indoc::indoc! {r#"
        validator multiple {
          spend(_datum: Option<List<Bool>>, redeemer: Int, _output: Data, _transaction: Data) {
            redeemer >= 0
          }

          mint(redeemer: Bool, _policy: ByteArray, _transaction: Data) {
            redeemer
          }

          else(_) {
            True
          }
        }
    "#});
    assert_eq!(legacy.validators.len(), 3);
    assert!(
        legacy
            .validators
            .iter()
            .any(|handler| handler.title.ends_with(".else"))
    );
    let compiled = assert_conversion(&legacy, &["multiple"]);
    insta::assert_json_snapshot!(compiled, {
        ".validators[].compiledCode" => "<redacted>",
        ".validators[].hash" => "<redacted>",
    });
}

#[test]
fn parameterised() {
    let legacy = compile(indoc::indoc! {r#"
        validator parameters(
          raw: Data,
          number: Int,
          flag: Bool,
          numbers: List<Int>,
          pair: Pair<Int, Bool>,
        ) {
          mint(redeemer: Data, _policy: ByteArray, _transaction: Data) {
            raw == redeemer && number >= 0 && flag && numbers == [number] && pair == Pair(number, flag)
          }
        }
    "#});
    let compiled = assert_conversion(&legacy, &["parameters"]);
    let parameters = compiled.validators[0].parameters.as_ref().unwrap();
    assert_eq!(parameters.len(), 5);
    for (parameter, definition) in
        parameters
            .iter()
            .zip(["Data", "Int", "Bool", "List<Int>", "Pair<Int,Bool>"])
    {
        assert_eq!(
            parameter.schema.0,
            json!({"$ref": format!("#/definitions/{definition}")})
        );
    }
    let definitions = serde_json::to_value(compiled.definitions.as_ref().unwrap()).unwrap();
    assert!(definitions["Data"].get("dataType").is_none());
    assert_eq!(definitions["Int"], json!({"dataType": "integer"}));
    assert_eq!(definitions["Bool"]["anyOf"][0]["dataType"], "constructor");
    assert_eq!(definitions["Bool"]["anyOf"][0]["index"], 0);
    assert_eq!(definitions["Bool"]["anyOf"][1]["index"], 1);
    assert_eq!(definitions["List<Int>"]["dataType"], "list");
    assert_eq!(
        definitions["List<Int>"]["items"],
        json!({"$ref": "#/definitions/Int"})
    );
    insta::assert_json_snapshot!(compiled, {
        ".validators[].compiledCode" => "<redacted>",
        ".validators[].hash" => "<redacted>",
    });
}

#[test]
fn recursive_types() {
    let legacy = compile(indoc::indoc! {r#"
        pub type Expr {
          Val(Int)
          Sum(Expr, Expr)
        }

        pub type LinkedList<a> {
          Cons(a, LinkedList<a>)
          Nil
        }

        validator recursive(seed: LinkedList<Int>) {
          spend(datum: Option<Expr>, redeemer: LinkedList<Int>, _output: Data, _transaction: Data) {
            seed == redeemer && datum != None
          }
        }
    "#});
    let compiled = assert_conversion(&legacy, &["recursive"]);
    let document = serde_json::to_value(&compiled).unwrap();
    let definitions = &document["definitions"];
    assert_eq!(
        definitions["fixtures/contracts/Expr"]["anyOf"][1]["fields"][0],
        json!({"$ref": "#/definitions/fixtures~1contracts~1Expr"})
    );
    assert_eq!(
        definitions["fixtures/contracts/LinkedList<Int>"]["anyOf"][0]["fields"][1],
        json!({"$ref": "#/definitions/fixtures~1contracts~1LinkedList<Int>"})
    );
    insta::assert_json_snapshot!(compiled, {
        ".validators[].compiledCode" => "<redacted>",
        ".validators[].hash" => "<redacted>",
    });
}

#[test]
fn native_and_data_parameter_schemas_are_not_rewritten() {
    let mut legacy = compile(indoc::indoc! {r#"
        validator parameters(value: Int) {
          mint(redeemer: Int, _policy: ByteArray, _transaction: Data) {
            value == redeemer
          }
        }
    "#});
    let variants = [
        Schema::Integer,
        Schema::Data(Data::Integer),
        Schema::Boolean,
        Schema::bool(),
        Schema::List(Items::One(Declaration::Inline(Box::new(Schema::Integer)))),
        Schema::Data(Data::List(Items::One(Declaration::Inline(Box::new(
            Data::Integer,
        ))))),
    ];
    for schema in variants {
        for handler in &mut legacy.validators {
            handler.parameters[0].schema = Declaration::Inline(Box::new(schema.clone()));
        }
        let compiled = assert_conversion(&legacy, &["parameters"]);
        assert_eq!(
            compiled.validators[0].parameters.as_ref().unwrap()[0]
                .schema
                .0,
            serde_json::to_value(&schema).unwrap()
        );
    }
}

#[test]
fn rejects_legacy_ledger_versions() {
    let mut legacy = compile(indoc::indoc! {r#"
        validator unsupported {
          else(_) { True }
        }
    "#});
    for version in [PlutusVersion::V1, PlutusVersion::V2] {
        legacy.preamble.plutus_version = version;
        let before = serde_json::to_vec(&legacy).unwrap();
        assert!(legacy.to_compiled_interface().is_err());
        assert_eq!(serde_json::to_vec(&legacy).unwrap(), before);
    }
}

#[test]
fn rejects_emitted_id_collisions() {
    let mut legacy = compile(indoc::indoc! {r#"
        validator collision {
          else(_) { True }
        }
    "#});
    assert_eq!(legacy.validators.len(), 1);
    let mut other = legacy.validators[0].clone();
    legacy.validators[0].title = "a/b.collision.else".to_owned();
    other.title = "a_b.collision.else".to_owned();
    legacy.validators.push(other);
    assert_eq!(
        emitted_target_id("a/b", "collision"),
        emitted_target_id("a_b", "collision")
    );
    let before = serde_json::to_vec(&legacy).unwrap();
    assert!(legacy.to_compiled_interface().is_err());
    assert_eq!(serde_json::to_vec(&legacy).unwrap(), before);
}
