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

#[test]
fn dotted_module_file_builds_and_generates_schema_valid_bundle() {
    let project = Project::fixture("positive/dotted_module");
    // The ordinary build accepts `validators/fixtures.extra.ak` unchanged.
    let output = project.run(&["build"]);
    assert!(output.status.success(), "{}", super::diagnostic(&output));
    let root = std::fs::read(project.0.join("plutus.json")).unwrap();
    let titles: Vec<_> = serde_json::from_slice::<serde_json::Value>(&root).unwrap()["validators"]
        .as_array()
        .unwrap()
        .iter()
        .map(|validator| validator["title"].as_str().unwrap().to_owned())
        .collect();
    assert_eq!(
        titles,
        [
            "fixtures.extra.vesting.spend",
            "fixtures.extra.vesting.else"
        ]
    );

    project.generate();
    project.assert_golden("positive/dotted_module");
    // Verification generation never rewrites the ordinary root blueprint.
    assert_eq!(std::fs::read(project.0.join("plutus.json")).unwrap(), root);
    let blueprint = project.json("assurance/plutus.json");
    aiken_ual::validate(aiken_ual::DocumentKind::Blueprint, &blueprint).unwrap();
    let assurance = project.json("assurance/assurance.json");
    aiken_ual::validate(aiken_ual::DocumentKind::Assurance, &assurance).unwrap();
    let context = project.json("assurance/checking-dotted_minimum.json");
    aiken_ual::validate(aiken_ual::DocumentKind::CheckingContext, &context).unwrap();
    assert_eq!(blueprint["validators"][0]["id"], "fixtures_extra__vesting");
    assert_eq!(
        blueprint["validators"][0]["title"],
        "fixtures.extra.vesting"
    );
    assert_eq!(
        context["targets"][0]["validator"],
        "fixtures_extra__vesting"
    );

    // A second generation publishes identical bytes.
    let first = project.bundle();
    project.generate();
    assert_eq!(project.bundle(), first);
}
