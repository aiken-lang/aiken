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
            "@onchain(steps: 1, semantics: e)\npub fn f() { 1 }",
            "f[00]",
            ErrorKind::FunctionParameters,
        ),
        (
            "@onchain(steps: 1, semantics: e)\npub fn f() { 1 }",
            "f[]",
            ErrorKind::FunctionParameters,
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
fn repeated_validator_scopes_require_identical_parameter_bindings() {
    for (scopes, conflicting) in [
        ("v[00] v.else[01]", true),
        ("v v.else[00]", true),
        ("v[00] v.else", true),
        ("v[] v.else[00]", true),
        ("v[00] v.else[00]", false),
        ("v v.else", false),
    ] {
        let source = format!(
            "@onchain(steps: 1, semantics: e)\nvalidator v(n: Int) {{ else(_ctx: Data) {{ n == 0 }} }}\n//@ PROPERTY p for {scopes} \"claim\" True @//"
        );
        let mut project = TestProject::new();
        let module = check(&mut project, "local", &source);
        let result = validate([&module]);
        if conflicting {
            let errors = result.unwrap_err();
            assert_eq!(errors.len(), 1, "{errors:?}");
            assert_eq!(errors[0].kind, ErrorKind::ConflictingParameters);
            assert_eq!(
                &source[errors[0].location.range()],
                scopes.split_whitespace().nth(1).unwrap()
            );
            assert_eq!(errors[0].related.len(), 1);
        } else {
            let model = result.unwrap();
            let scopes = &model.properties["p"].scopes;
            assert_eq!(scopes[0].applied_parameters, scopes[1].applied_parameters);
            assert_eq!(
                scopes[0].applied_parameters.is_none(),
                !source.contains("[00]")
            );
        }
    }
}

#[test]
fn separate_properties_may_bind_different_parameters() {
    let source = r#"
        @onchain(steps: 1, semantics: e)
        validator v(n: Int) { else(_ctx: Data) { n == 0 } }
        //@ PROPERTY universal for v "universal" True @//
        //@ PROPERTY zero for v[00] "zero" True @//
        //@ PROPERTY one for v[01] "one" True @//
    "#;
    let mut project = TestProject::new();
    let module = check(&mut project, "local", source);
    let model = validate([&module]).unwrap();
    assert_eq!(
        model.properties["universal"].scopes[0].applied_parameters,
        None
    );
    assert_eq!(
        model.properties["zero"].scopes[0].applied_parameters,
        Some(vec!["00".to_string()])
    );
    assert_eq!(
        model.properties["one"].scopes[0].applied_parameters,
        Some(vec!["01".to_string()])
    );
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
        ["predicates.base", "namespace"]
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

#[test]
fn property_requires_referenced_project_fragment_in_its_import_closure() {
    for (imports, body, missing) in [
        ("", "helpers.valid", true),
        ("use helpers\n", "helpers.valid", false),
        ("", "True -- helpers.valid", false),
        ("", "True /- helpers.valid -/", false),
    ] {
        let mut project = TestProject::new();
        let helper = check(
            &mut project,
            "helpers",
            "//@ PREDICATE\ndef valid : Prop := True\n@//",
        );
        let source = format!(
            "{imports}@onchain(steps: 100, semantics: e)\npub fn f() {{ True }}\n//@ PROPERTY p for f \"fragment closure\" {body}\n@//"
        );
        let module = check(&mut project, "local", &source);
        let result = validate([&helper, &module]);
        if missing {
            let errors = result.unwrap_err();
            assert_eq!(errors.len(), 1);
            assert_eq!(errors[0].kind, ErrorKind::MissingFragment);
            assert!(
                errors[0]
                    .message
                    .contains("Fragment 'helpers' is not available")
            );
            assert_block_label(&errors[0], &source);
        } else {
            result.unwrap();
        }
    }
}

#[test]
fn fragment_helper_dependencies_must_be_in_the_transitive_closure() {
    for imported in [false, true] {
        let mut project = TestProject::new();
        let base = check(
            &mut project,
            "base",
            "//@ PREDICATE def valid : Prop := True @//",
        );
        let imports = if imported { "use base\n" } else { "" };
        let bridge = check(
            &mut project,
            "bridge",
            &format!("{imports}//@ PREDICATE def bridged : Prop := base.valid @//"),
        );
        let source = "use bridge\n@onchain(steps: 100, semantics: e) pub fn f() { True }\n//@ PROPERTY p for f \"transitive closure\" bridge.bridged @//";
        let module = check(&mut project, "local", source);
        let result = validate([&base, &bridge, &module]);
        if imported {
            result.unwrap();
        } else {
            let errors = result.unwrap_err();
            assert_eq!(errors.len(), 1);
            assert_eq!(errors[0].kind, ErrorKind::MissingFragment);
            assert!(
                errors[0]
                    .message
                    .contains("Fragment 'base' is not available")
            );
            assert_block_label(&errors[0], source);
        }
    }
}

#[test]
fn bound_variable_projections_are_not_module_references() {
    // An unrelated predicate module named like the bound variable must not turn
    // `ctx.scriptContextTxInfo` into a missing-import error.
    for (imports, body, missing) in [
        (
            "",
            "∀ ctx : ScriptContext, ctx.scriptContextTxInfo = ctx.scriptContextTxInfo",
            false,
        ),
        (
            "",
            "∀ ctx : ScriptContext, ctx.unrelated ∧ ctx.scriptContextTxInfo = ctx.scriptContextTxInfo",
            true,
        ),
        ("use ctx\n", "ctx.unrelated", false),
    ] {
        let mut project = TestProject::new();
        let unrelated = check(
            &mut project,
            "ctx",
            "//@ PREDICATE\ndef unrelated : Prop := True\n@//",
        );
        let source = format!(
            "{imports}@onchain(steps: 100, semantics: e)\npub fn f() {{ True }}\n//@ PROPERTY p for f \"projection\" {body} @//"
        );
        let module = check(&mut project, "local", &source);
        let result = validate([&unrelated, &module]);
        if missing {
            let errors = result.unwrap_err();
            assert_eq!(errors.len(), 1, "{errors:?}");
            assert_eq!(errors[0].kind, ErrorKind::MissingFragment);
            assert!(
                errors[0]
                    .message
                    .contains("Fragment 'ctx' is not available")
            );
            assert!(errors[0].message.contains("'ctx.unrelated'"));
            assert_eq!(
                errors[0].help.as_deref(),
                Some("Import module 'ctx' where its predicate helpers are referenced.")
            );
            assert_eq!(
                &errors[0].related[0].source_code.inner()[errors[0].related[0].location.range()],
                "unrelated"
            );
        } else {
            result.unwrap();
        }
    }
}

#[test]
fn annotation_free_bridges_give_a_deterministic_transitive_closure() {
    let mut project = TestProject::new();
    let helpers = check(
        &mut project,
        "proofs/helpers",
        "//@ PREDICATE def larger (a b : Int) : Prop := a < b @//\npub fn identity(value: Int) -> Int { value }",
    );
    let bridge = check(
        &mut project,
        "proofs/bridge",
        "use proofs/helpers\npub fn same(value: Int) -> Int { helpers.identity(value) }",
    );
    let keyword = check(
        &mut project,
        "example",
        "use proofs/bridge\n//@ PREDICATE def positive (x : Int) : Prop := proofs.helpers.larger 0 x @//\npub fn same(value: Int) -> Int { bridge.same(value) }",
    );
    let source = "use example\nuse proofs/bridge\n//@ PREDICATE def raised (a b : Int) : Prop := proofs.helpers.larger a b @//\n@onchain(steps: 100, semantics: e)\npub fn f() { True }\n//@ PROPERTY p for f \"closure\" «example».positive 1 ∧ raised 0 1 @//";
    let module = check(&mut project, "fixtures", source);
    let model = validate([&helpers, &bridge, &keyword, &module]).unwrap();
    assert_eq!(
        model,
        validate([&module, &keyword, &bridge, &helpers]).unwrap()
    );
    assert_eq!(
        model.fragments.keys().collect::<Vec<_>>(),
        ["example", "fixtures", "proofs.helpers"]
    );
    assert_eq!(model.fragments["example"].namespace, "«example»");
    assert_eq!(model.fragments["example"].imports, ["proofs.helpers"]);
    assert_eq!(
        model.fragments["fixtures"].imports,
        ["example", "proofs.helpers"]
    );
    assert_eq!(model.properties["p"].uses, ["fixtures"]);
}

#[test]
fn nested_and_dotted_module_paths_keep_distinct_identities() {
    let mut project = TestProject::new();
    let nested = check(
        &mut project,
        "a/b",
        "//@ PREDICATE def f : Prop := True @//",
    );
    let dotted = check(
        &mut project,
        "a.b",
        "//@ PREDICATE def f : Prop := False @//\n@onchain(steps: 1, semantics: e) pub fn g() { 1 }\n//@ PROPERTY dotted for g \"dotted\" f @//",
    );
    let model = validate([&nested, &dotted]).unwrap();
    let identities: Vec<_> = model
        .fragments
        .values()
        .map(|fragment| {
            (
                fragment.id.as_str(),
                fragment.module.as_str(),
                fragment.namespace.as_str(),
            )
        })
        .collect();
    assert_eq!(identities, [("a-b", "a.b", "«a.b»"), ("a.b", "a/b", "a.b")]);
    assert_eq!(model.properties["dotted"].uses, ["a-b"]);
}

#[test]
fn qualified_spellings_collide_with_any_other_emitted_name() {
    // Module `a`'s helper `b.f` and module `a/b`'s helper `f` are both `a.b.f`.
    let mut project = TestProject::new();
    let outer = check(
        &mut project,
        "a",
        "//@ PREDICATE def b.f : Prop := True @//",
    );
    let inner = check(
        &mut project,
        "a/b",
        "//@ PREDICATE def f : Prop := False @//",
    );
    let source = "use a\nuse a/b\n@onchain(steps: 1, semantics: e) pub fn g() { 1 }\n//@ PROPERTY p for g \"claim\" a.b.f @//";
    let module = check(&mut project, "local", source);
    let errors = validate([&outer, &inner, &module]).unwrap_err();
    assert_eq!(errors.len(), 1, "{errors:?}");
    assert_eq!(errors[0].kind, ErrorKind::HelperCollision);
    assert!(
        errors[0].message.contains("Helper 'a.b.f'"),
        "{}",
        errors[0].message
    );
    assert!(errors[0].message.contains("modules 'a' and 'a/b'"));
    let related: Vec<_> = errors[0]
        .related
        .iter()
        .map(|related| &related.source_code.inner()[related.location.range()])
        .collect();
    assert_eq!(related, ["b.f", "f"]);
}

#[test]
fn qualified_copies_cannot_resolve_to_a_different_helper() {
    // Inside `x.g`, Lean resolves `f` to `x.f` (module z) although `g` means
    // module y's `f`; the copies would silently disagree.
    let mut project = TestProject::new();
    let y = check(&mut project, "y", "//@ PREDICATE def f : Prop := True @//");
    let z = check(
        &mut project,
        "z",
        "//@ PREDICATE def x.f : Prop := False @//",
    );
    let x = check(
        &mut project,
        "x",
        "use y\nuse z\n//@ PREDICATE def g : Prop := f @//",
    );
    let source = "use x\n@onchain(steps: 1, semantics: e) pub fn h() { 1 }\n//@ PROPERTY p for h \"claim\" x.g @//";
    let module = check(&mut project, "local", source);
    let errors = validate([&x, &y, &z, &module]).unwrap_err();
    assert_eq!(errors.len(), 1, "{errors:?}");
    assert_eq!(errors[0].kind, ErrorKind::HelperCollision);
    assert!(
        errors[0].message.contains("Helper 'x.f' from module 'z' shadows 'f' from module 'y' inside the qualified declaration 'x.g'"),
        "{}",
        errors[0].message
    );

    // Same-module dotted helpers resolve alike in both copies and are accepted.
    let mut project = TestProject::new();
    let source = "//@ PREDICATE def x : Prop := True\ndef Some.x : Prop := False\ndef Some.y : Prop := x @//\n@onchain(steps: 1, semantics: e) pub fn h() { 1 }\n//@ PROPERTY p for h \"claim\" local.Some.y @//";
    let module = check(&mut project, "local", source);
    validate([&module]).unwrap();
}
