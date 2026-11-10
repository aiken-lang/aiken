use super::{Project, diagnostic};

fn rejects_fixture(fixture: &str, expected: &[&str]) {
    let project = Project::fixture(&format!("negative/{fixture}"));
    let output = project.run(&["verify", "generate", "--environment", "environment.json"]);
    let message = diagnostic(&output);
    assert!(
        !output.status.success(),
        "negative fixture {fixture} unexpectedly succeeded:\n{message}"
    );
    for fragment in expected {
        assert!(
            message.contains(fragment),
            "negative fixture {fixture} did not report {fragment:?}:\n{message}"
        );
    }
    assert!(
        !project.0.join("assurance").exists(),
        "negative fixture {fixture} emitted assurance output:\n{message}"
    );
}

#[test]
fn unknown_scope_is_rejected() {
    rejects_fixture(
        "unknown_scope",
        &[
            "aiken::assurance",
            "Scope 'missing'",
            "defined in this module",
        ],
    );
}

#[test]
fn target_in_another_module_is_not_a_local_scope() {
    rejects_fixture(
        "wrong_module",
        &[
            "aiken::assurance",
            "Scope 'target'",
            "defined in this module",
            "'main'",
        ],
    );
}

#[test]
fn helper_outside_the_fragment_closure_is_rejected() {
    rejects_fixture(
        "missing_fragment_helper",
        &["aiken::assurance", "Fragment 'helpers' is not available"],
    );
}

#[test]
fn opaque_parameter_is_rejected() {
    rejects_fixture(
        "opaque_parameter",
        &[
            "illegal::opaque_type_in_abi",
            "opaque type in an exported interface",
            "Secret",
        ],
    );
}

#[test]
fn missing_steps_is_rejected() {
    rejects_fixture(
        "missing_steps",
        &[
            "decorators::validation",
            "@onchain requires a positive 'steps' setting",
        ],
    );
}

#[test]
fn invalid_semantics_is_rejected() {
    rejects_fixture(
        "invalid_semantics",
        &[
            "decorators::validation",
            "@onchain semantics must be C or E",
        ],
    );
}

#[test]
fn slash_and_underscore_target_ids_cannot_collide() {
    rejects_fixture(
        "id_collision",
        &[
            "aiken::assurance",
            "a/b.target",
            "a_b.target",
            "both map to emitted id",
            "a_b__target",
        ],
    );
}

#[test]
fn mixed_step_budgets_are_rejected() {
    rejects_fixture(
        "mixed_execution_settings/steps",
        &[
            "aiken::assurance",
            "mixes execution settings",
            "main__first",
            "main__second",
            "steps=100",
            "steps=101",
            "semantics=E",
        ],
    );
}

#[test]
fn mixed_semantics_are_rejected() {
    rejects_fixture(
        "mixed_execution_settings/semantics",
        &[
            "aiken::assurance",
            "mixes execution settings",
            "main__first",
            "main__second",
            "steps=100",
            "semantics=E",
            "semantics=C",
        ],
    );
}

#[test]
fn incompatible_purpose_scopes_are_rejected() {
    let expected = [
        "aiken::assurance",
        "\"vacuous_impossible\"",
        "'vesting.spend' admits {spend}",
        "'vesting.else' admits {mint, withdraw, publish, vote, propose}",
        "same 'ctx'",
        "vacuously",
        "separate property",
    ];
    rejects_fixture("incompatible_purposes", &expected);
    // Both write-free dry runs reject it the same way: nothing is generated to inspect.
    for args in [
        &[
            "verify",
            "generate",
            "--environment",
            "environment.json",
            "--dry-run",
        ][..],
        &["verify", "generate", "--no-environment"][..],
    ] {
        let project = Project::fixture("negative/incompatible_purposes");
        let output = project.run(args);
        let message = diagnostic(&output);
        assert!(!output.status.success(), "{args:?} succeeded:\n{message}");
        for fragment in expected {
            assert!(
                message.contains(fragment),
                "{args:?}: {fragment:?}:\n{message}"
            );
        }
        assert!(
            !project.0.join("assurance").exists(),
            "{args:?}:\n{message}"
        );
    }
}

#[test]
fn unterminated_annotation_block_is_rejected() {
    rejects_fixture(
        "unterminated_block",
        &[
            "end of the file inside an annotation region",
            "annotation is not closed",
            "Close the annotation with '@//'",
        ],
    );
}
