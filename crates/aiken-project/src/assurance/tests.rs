use super::*;
use crate::tests::TestProject;
use miette::Diagnostic;

fn check(project: &mut TestProject, name: &str, source: &str) -> CheckedModule {
    let mut module = project.parse(source);
    module.name = name.to_string();
    module.ast.name = name.to_string();
    module.path = format!("{name}.ak").into();
    project.check(module)
}

fn assert_block_label(error: &Error, source: &str) {
    let labels: Vec<_> = error.labels().unwrap().collect();
    assert_eq!(labels.len(), 1);
    let label = &labels[0];
    let start = source
        .match_indices("//@")
        .map(|(offset, _)| offset)
        .take_while(|offset| *offset <= label.offset())
        .last()
        .unwrap();
    let end = start + source[start..].find("@//").unwrap() + 3;
    assert!(label.offset() >= start && label.offset() + label.len() <= end);
    assert!(!label.is_empty());
    assert!(source.is_char_boundary(label.offset()));
    assert!(source.is_char_boundary(label.offset() + label.len()));
}

#[test]
fn scope_diagnostics_label_the_exact_scope() {
    for (declaration, scope, expected) in [
        ("pub fn f() { 1 }", "missing", ErrorKind::UnknownScope),
        ("pub fn f() { 1 }", "f", ErrorKind::MissingOnchain),
        (
            "@onchain(steps: 1, semantics: e)\npub fn f() { 1 }",
            "f.mint",
            ErrorKind::FunctionHandler,
        ),
        (
            "@onchain(steps: 1, semantics: e)\nvalidator v { mint(_r: Data, _p: ByteArray, _c: Data) { True } }",
            "v.spend",
            ErrorKind::UnknownHandler,
        ),
        (
            "validator v { mint(_r: Data, _p: ByteArray, _c: Data) { True } }",
            "v",
            ErrorKind::MissingOnchain,
        ),
    ] {
        let source =
            format!("{declaration}\n//@ PROPERTY p for {scope} \"é description\" True @//");
        let mut project = TestProject::new();
        let module = check(&mut project, "local", &source);
        let errors = validate([&module]).unwrap_err();
        assert_eq!(errors.len(), 1, "{errors:?}");
        let error = &errors[0];
        assert_eq!(error.kind, expected);
        assert_block_label(error, &source);
        assert_eq!(&source[error.location.range()], scope);
        if expected == ErrorKind::MissingOnchain {
            let fix = error.fix.as_ref().unwrap();
            assert_eq!(fix.location.start, fix.location.end);
            assert!(
                source[fix.location.start..].starts_with("pub fn")
                    || source[fix.location.start..].starts_with("validator")
            );
            assert_eq!(
                fix.template,
                "@onchain(steps: <positive integer>, semantics: <c|e>)\n"
            );
            assert!(error.help.as_ref().unwrap().contains("@onchain"));
        }
    }
}

#[test]
fn imported_target_is_not_a_local_scope() {
    let mut project = TestProject::new();
    let dependency = check(
        &mut project,
        "dependency",
        "@onchain(steps: 1, semantics: e) pub fn f() { 1 }",
    );
    let source = "use dependency.{f}\n//@ PROPERTY p for f \"local only\" True @//";
    let module = check(&mut project, "local", source);
    let errors = validate([&dependency, &module]).unwrap_err();
    assert_eq!(errors.len(), 1);
    assert_eq!(errors[0].kind, ErrorKind::UnknownScope);
    assert_block_label(&errors[0], source);
}

#[test]
fn onchain_comment_points_to_the_decorator() {
    let source = "//@ ONCHAIN [steps: 1] @//";
    let mut project = TestProject::new();
    let module = check(&mut project, "local", source);
    let errors = validate([&module]).unwrap_err();
    assert_eq!(errors.len(), 1);
    assert_eq!(errors[0].kind, ErrorKind::BlockSyntax);
    assert_block_label(&errors[0], source);
    assert_eq!(&source[errors[0].location.range()], "ONCHAIN");
    assert!(errors[0].help.as_ref().unwrap().contains("@onchain"));
}

#[test]
fn property_ids_are_project_unique() {
    let source = "@onchain(steps: 1, semantics: e) pub fn f() { 1 }\n//@ PROPERTY duplicate for f \"description\" True @//";
    let mut project = TestProject::new();
    let first = check(&mut project, "first", source);
    let second = check(&mut project, "second", source);
    for modules in [[&first, &second], [&second, &first]] {
        let errors = validate(modules).unwrap_err();
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].kind, ErrorKind::DuplicateProperty);
        assert_block_label(&errors[0], source);
        assert_eq!(&source[errors[0].location.range()], "duplicate");
        assert_eq!(errors[0].related.len(), 1);
        assert_eq!(errors[0].related[0].source_code.name(), "first.ak");
    }
}

#[test]
fn mapped_ids_collide_across_modules_and_target_kinds() {
    for declaration in [
        "pub fn f() { 2 }",
        "validator f { mint(_r: Data, _p: ByteArray, _c: Data) { True } }",
    ] {
        let mut project = TestProject::new();
        let first = check(
            &mut project,
            "a/b",
            "@onchain(steps: 1, semantics: e) pub fn f() { 1 }",
        );
        let source = format!(
            "@onchain(steps: 1, semantics: e) {declaration}\n//@ PREDICATE def helper := True @//"
        );
        let second = check(&mut project, "a_b", &source);
        let errors = validate([&first, &second]).unwrap_err();
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].kind, ErrorKind::TargetIdCollision);
        assert_block_label(&errors[0], &source);
        assert!(errors[0].message.contains("a/b.f"));
        assert!(errors[0].message.contains("a_b.f"));
        assert!(errors[0].message.contains("a_b__f"));
        assert_eq!(errors[0].related.len(), 2);
    }
    assert_eq!(emitted_target_id("a-b/c", "f"), "a_b_c__f");
    assert_eq!(emitted_target_id("a_b_c", "f"), "a_b_c__f");
}

#[test]
fn helper_collisions_are_checked_per_property_closure() {
    let mut project = TestProject::new();
    let first = check(
        &mut project,
        "first",
        "//@ PREDICATE def duplicate := True @//",
    );
    let second = check(
        &mut project,
        "second",
        "//@ PREDICATE abbrev duplicate := False @//",
    );
    let unrelated = check(
        &mut project,
        "unrelated",
        "//@ PREDICATE def duplicate := True @//",
    );
    let source = "use first\nuse second\n@onchain(steps: 1, semantics: e) pub fn f() { 1 }\n//@ PROPERTY p for f \"description\" duplicate @//";
    let module = check(&mut project, "local", source);
    let errors = validate([&module, &first, &second, &unrelated]).unwrap_err();
    assert_eq!(errors.len(), 1);
    assert_eq!(errors[0].kind, ErrorKind::HelperCollision);
    assert_block_label(&errors[0], source);
    assert!(errors[0].message.contains("first"));
    assert!(errors[0].message.contains("second"));
    for related in &errors[0].related {
        assert_eq!(
            &related.source_code.inner()[related.location.range()],
            "duplicate"
        );
    }
}

#[test]
fn duplicate_helpers_in_separate_blocks_are_not_silently_merged() {
    let mut project = TestProject::new();
    let source = "@onchain(steps: 1, semantics: e) pub fn f() { 1 }\n//@ PREDICATE def duplicate := True @//\n//@ PREDICATE abbrev «duplicate» := False @//\n//@ PROPERTY p for f \"description\" True @//";
    let module = check(&mut project, "local", source);
    let errors = validate([&module]).unwrap_err();
    assert_eq!(errors.len(), 1);
    assert_eq!(errors[0].kind, ErrorKind::HelperCollision);
    assert_block_label(&errors[0], source);
}

#[test]
fn diamond_imports_do_not_duplicate_helpers_and_property_only_modules_inherit_fragments() {
    let mut project = TestProject::new();
    let base = check(&mut project, "base", "//@ PREDICATE def shared := True @//");
    let left = check(&mut project, "left", "use base");
    let right = check(&mut project, "right", "use base");
    let source = "use left\nuse right\n@onchain(steps: 1, semantics: e) pub fn f() { 1 }\n//@ PROPERTY p for f \"description\" shared @//";
    let module = check(&mut project, "local", source);
    let model = validate([&module, &right, &base, &left]).unwrap();
    assert_eq!(model.properties["p"].uses, ["base"]);
    assert_eq!(model.fragments["base"].imports, Vec::<String>::new());
}

#[test]
fn no_annotations_leave_an_empty_model() {
    let mut project = TestProject::new();
    let module = check(&mut project, "local", "pub fn f() { 42 }");
    assert_eq!(validate([&module]).unwrap(), AnnotationModel::default());
}

#[test]
fn multi_module_purpose_scopes_model() {
    let mut project = TestProject::new();
    let base = check(
        &mut project,
        "predicates/base",
        include_str!("fixtures/base.ak"),
    );
    let bridge = check(&mut project, "bridge", include_str!("fixtures/bridge.ak"));
    let other = check(
        &mut project,
        "namespace",
        include_str!("fixtures/namespace.ak"),
    );
    let module = check(
        &mut project,
        "contract",
        include_str!("fixtures/contract.ak"),
    );
    let model = validate([&module, &other, &base, &bridge]).unwrap();
    let reordered = validate([&bridge, &base, &other, &module]).unwrap();
    assert_eq!(model, reordered);
    assert_eq!(
        model.fragments["contract"].imports,
        ["predicates.base", "«namespace»"]
    );
    assert_eq!(model.fragments["contract"].predicates.len(), 2);
    assert_eq!(
        model.properties["mint_or_fallback"]
            .scopes
            .iter()
            .map(|scope| scope.handler.as_deref())
            .collect::<Vec<_>>(),
        [Some("mint"), Some("else"), None]
    );
    insta::assert_json_snapshot!(model);
}
