use super::Project;

#[test]
fn native_function_matches_golden() {
    let project = Project::fixture("positive/native_function");
    project.generate();
    project.assert_golden("positive/native_function");
}

#[test]
fn universal_validator_matches_golden() {
    let project = Project::fixture("positive/universal_validator");
    project.generate();
    project.assert_golden("positive/universal_validator");
}

#[test]
fn falsified_property_still_generates_matching_golden() {
    let project = Project::fixture("positive/falsified");
    project.generate();
    project.assert_golden("positive/falsified");
}

#[test]
fn recursive_data_matches_golden() {
    let project = Project::fixture("positive/recursive_data");
    project.generate();
    project.assert_golden("positive/recursive_data");
}

#[test]
fn qualified_local_and_escaped_helpers_match_golden() {
    let project = Project::fixture("positive/qualified_helpers");
    project.generate();
    project.assert_golden("positive/qualified_helpers");
    let assurance = project.json("assurance/assurance.json");
    let ids: Vec<_> = assurance["formalFragments"]
        .as_array()
        .unwrap()
        .iter()
        .map(|fragment| fragment["id"].as_str().unwrap())
        .collect();
    assert_eq!(ids, ["example", "fixtures", "proofs.helpers"]);
}

#[test]
fn unrelated_ctx_module_does_not_block_bound_ctx_projections() {
    let project = Project::fixture("positive/universal_validator");
    let path = project.0.join("validators/fixtures.ak");
    let source = std::fs::read_to_string(&path).unwrap();
    assert_eq!(source.matches("minimum ≤ 0 →").count(), 1);
    project.put(
        "validators/fixtures.ak",
        source.replace(
            "minimum ≤ 0 →",
            "ctx.scriptContextTxInfo = ctx.scriptContextTxInfo → minimum ≤ 0 →",
        ),
    );
    project.put(
        "lib/ctx.ak",
        "//@ PREDICATE\ndef unrelated : Prop := True\n@//\n",
    );
    for args in [
        &["build"][..],
        &["check"][..],
        &["verify", "generate", "--environment", "environment.json"][..],
    ] {
        let output = project.run(args);
        assert!(
            output.status.success(),
            "{args:?}: {}",
            super::diagnostic(&output)
        );
    }
    let assurance = project.json("assurance/assurance.json");
    // The unrelated module is still a fragment, but no property uses it.
    assert!(
        assurance["properties"]
            .as_array()
            .unwrap()
            .iter()
            .all(|property| property["statement"]["formal"].get("uses").is_none())
    );
}
