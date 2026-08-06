#[cfg(not(target_family = "wasm"))]
use crate::expr::{native_stack_segment_size, with_native_stack_segment};
use crate::{
    IdGenerator,
    ast::{Definition, ModuleKind, TraceLevel, Tracing, TypedModule, UntypedModule},
    builtins,
    expr::{MAX_EXPRESSION_NESTING, UntypedExpr},
    parser,
    tipo::{
        TypeInfo,
        error::{Error, Warning},
    },
};

use std::collections::HashMap;

const MODULE_NAME: &str = "deep_nesting";
const PACKAGE_NAME: &str = "local/deep_nesting";

fn push_deep_constructor(source: &mut String, depth: usize) {
    for _ in 0..depth {
        source.push_str("S(");
    }

    source.push('Z');

    for _ in 0..depth {
        source.push(')');
    }
}

fn deep_module(depth: usize) -> String {
    let mut source = String::with_capacity(160 + depth * 3);
    source.push_str("type Natural {\n  Z\n  S(Natural)\n}\n\ntest deeply_nested_constructor() {\n  let value = ");
    push_deep_constructor(&mut source, depth);
    source.push_str("\n  value == value\n}\n");
    source
}

fn deep_single_when_module(depth: usize) -> String {
    let mut source = String::with_capacity(200 + depth * 3);
    source.push_str("type Natural {\n  Z\n  S(Natural)\n}\n\ntest deeply_nested_when() {\n  when ");
    push_deep_constructor(&mut source, depth);
    source.push_str(" is {\n    value -> value == value\n  }\n}\n");
    source
}

fn deep_if_module(depth: usize) -> String {
    let mut source = String::with_capacity(220 + depth * 3);
    source.push_str(
        "type Natural {\n  Z\n  S(Natural)\n}\n\ntest deeply_nested_if() {\n  let value = if True {\n    ",
    );
    push_deep_constructor(&mut source, depth);
    source.push_str("\n  } else {\n    Z\n  }\n  value == value\n}\n");
    source
}

fn deep_record_update_module(depth: usize) -> String {
    let mut source = String::with_capacity(280 + depth * 3);
    source.push_str(
        "type Natural {\n  Z\n  S(Natural)\n}\n\ntype Boxed {\n  value: Natural\n}\n\ntest deeply_nested_record_update() {\n  let original = Boxed { value: Z }\n  let updated = Boxed { ..original, value: ",
    );
    push_deep_constructor(&mut source, depth);
    source.push_str(" }\n  updated == updated\n}\n");
    source
}

fn deep_discarded_void_expression_module(depth: usize) -> String {
    let mut source = String::with_capacity(260 + depth * 3);
    source.push_str(
        "type Natural {\n  Z\n  S(Natural)\n}\n\nfn discard(_value: Natural) {\n  Void\n}\n\ntest deeply_nested_discarded_void_expression() {\n  discard(",
    );
    push_deep_constructor(&mut source, depth);
    source.push_str(")\n  True\n}\n");
    source
}

fn parse_generated(source: String, expectation: &str) -> UntypedModule {
    let (mut module, _) = parser::module(&source, ModuleKind::Lib).expect(expectation);
    module.name = MODULE_NAME.to_string();
    module
}

fn parse(depth: usize) -> UntypedModule {
    parse_generated(deep_module(depth), "deep module should parse")
}

fn parse_deep_single_when(depth: usize) -> UntypedModule {
    parse_generated(
        deep_single_when_module(depth),
        "deep single-when module should parse",
    )
}

fn test_body(module: &UntypedModule) -> &UntypedExpr {
    module
        .definitions
        .iter()
        .find_map(|definition| match definition {
            Definition::Test(test) => Some(&test.body),
            _ => None,
        })
        .expect("generated module contains a test")
}

fn infer_result(module: UntypedModule) -> Result<TypedModule, Error> {
    let id_gen = IdGenerator::new();
    let mut module_types = HashMap::<String, TypeInfo>::new();
    module_types.insert(builtins::PRELUDE.to_string(), builtins::prelude(&id_gen));
    module_types.insert(builtins::BUILTIN.to_string(), builtins::plutus(&id_gen));
    let mut warnings = Vec::<Warning>::new();

    module.infer(
        &id_gen,
        ModuleKind::Lib,
        PACKAGE_NAME,
        &module_types,
        Tracing::All(TraceLevel::Verbose),
        &mut warnings,
        None,
    )
}

fn infer(module: UntypedModule) -> TypedModule {
    infer_result(module).expect("deep module should infer")
}

#[test]
fn parses_and_drops_deep_constructor_sequence() {
    let module = parse(2_048);
    drop(module);
}

#[test]
fn infers_deep_constructor_sequence() {
    let module = infer(parse(2_048));
    drop(module);
}

#[cfg(not(target_family = "wasm"))]
#[test]
fn configured_native_limit_matches_the_covered_constructor_chain() {
    let module = parse(6_144);
    assert_eq!(test_body(&module).nesting_depth(), MAX_EXPRESSION_NESTING);

    let stack_size = native_stack_segment_size(MAX_EXPRESSION_NESTING);
    with_native_stack_segment(stack_size, || drop(module));
}

#[test]
fn rejects_excessive_nesting_with_a_normal_error() {
    for constructor_depth in [
        MAX_EXPRESSION_NESTING + 1,
        MAX_EXPRESSION_NESTING.saturating_mul(10),
    ] {
        let error = infer_result(parse(constructor_depth))
            .expect_err("excessively nested module should be rejected");

        assert_eq!(
            error.to_string(),
            "Expression nesting exceeds the supported limit."
        );

        assert!(matches!(
            error,
            Error::ExpressionNestingLimitExceeded {
                depth,
                limit: MAX_EXPRESSION_NESTING,
                ..
            } if depth > MAX_EXPRESSION_NESTING
        ));
    }
}

#[cfg(not(target_family = "wasm"))]
fn infer_on_small_native_stack(module: UntypedModule, thread_name: &str) {
    let fixture_depth = test_body(&module).nesting_depth();
    std::thread::Builder::new()
        .name(thread_name.to_string())
        .stack_size(128 * 1024)
        .spawn(move || {
            let typed_module = infer(module);
            let stack_size = native_stack_segment_size(fixture_depth);
            with_native_stack_segment(stack_size, || drop(typed_module));
        })
        .expect("spawn small-stack inference thread")
        .join()
        .expect("infer generated module on small native stack");
}

#[cfg(not(target_family = "wasm"))]
#[test]
fn infers_on_an_intentionally_small_native_stack() {
    infer_on_small_native_stack(parse(4_096), "deep-inference-small-stack");
}

#[cfg(not(target_family = "wasm"))]
#[test]
fn infers_deep_single_when_subject_on_a_small_native_stack() {
    infer_on_small_native_stack(
        parse_deep_single_when(4_096),
        "deep-single-when-small-stack",
    );
}

#[cfg(not(target_family = "wasm"))]
#[test]
fn infers_deep_if_branch_on_a_small_native_stack() {
    infer_on_small_native_stack(
        parse_generated(deep_if_module(4_096), "deep if module should parse"),
        "deep-if-small-stack",
    );
}

#[cfg(not(target_family = "wasm"))]
#[test]
fn infers_deep_record_update_on_a_small_native_stack() {
    infer_on_small_native_stack(
        parse_generated(
            deep_record_update_module(4_096),
            "deep record update module should parse",
        ),
        "deep-record-update-small-stack",
    );
}

#[cfg(not(target_family = "wasm"))]
#[test]
fn infers_deep_discarded_void_expression_on_a_small_native_stack() {
    infer_on_small_native_stack(
        parse_generated(
            deep_discarded_void_expression_module(4_096),
            "deep discarded void expression module should parse",
        ),
        "deep-discarded-void-expression-small-stack",
    );
}
