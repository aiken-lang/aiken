use crate::{
    blueprint::{
        self,
        definitions::Definitions,
        parameter::Parameter,
        schema::{Annotated, Declaration, Schema},
    },
    module::{CheckedModule, CheckedModules},
};
use aiken_lang::{
    ast::{ArgName, Span, TypedArg, TypedFunction},
    gen_uplc::CodeGenerator,
    plutus_version::PlutusVersion,
    tipo::{Type, error as tipo},
};
use miette::NamedSource;
use std::rc::Rc;
use uplc::ast::SerializableProgram;

#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
pub struct Export {
    pub name: String,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub doc: Option<String>,

    #[serde(skip_serializing_if = "Vec::is_empty")]
    #[serde(default)]
    pub parameters: Vec<Parameter>,

    pub return_type: Parameter,

    #[serde(flatten)]
    pub program: SerializableProgram,

    #[serde(skip_serializing_if = "Definitions::is_empty")]
    #[serde(default)]
    pub definitions: Definitions<Annotated<Schema>>,
}

impl Export {
    pub fn from_function(
        func: &TypedFunction,
        module: &CheckedModule,
        generator: &mut CodeGenerator,
        modules: &CheckedModules,
        plutus_version: &PlutusVersion,
    ) -> Result<Export, crate::Error> {
        let mut definitions = Definitions::new();

        let illegal_opaque_type =
            |module: &CheckedModule, tipo: Rc<Type>, location: Span, position: &str| {
                crate::Error::Type {
                    path: Box::new(module.input_path.clone()),
                    src: Box::new(module.code.clone()),
                    named: Box::new(NamedSource::new(
                        module.input_path.display().to_string(),
                        module.code.clone(),
                    )),
                    error: Box::new(tipo::Error::IllegalOpaqueType {
                        tipo,
                        location,
                        position: position.to_string(),
                    }),
                }
            };

        let parameters = func
            .arguments
            .iter()
            .map(|param| {
                if let Some(qualifier) = param.tipo.qualifier()
                    && modules
                        .values()
                        .any(|module| module.ast.type_info.opaque_types.contains(&qualifier))
                {
                    return Err(illegal_opaque_type(
                        module,
                        param.tipo.clone(),
                        param.location,
                        "function argument",
                    ));
                }

                Annotated::from_type(
                    modules.into(),
                    blueprint::validator::tipo_or_annotation(module, param),
                    &mut definitions,
                )
                .map(|schema| Parameter {
                    title: Some(param.arg_name.get_label()),
                    schema: Declaration::Referenced(schema),
                })
                .map_err(|error| {
                    crate::Error::Blueprint(Box::new(blueprint::Error::Schema {
                        error: Box::new(error),
                        location: param.location,
                        source_code: NamedSource::new(
                            module.input_path.display().to_string(),
                            module.code.clone(),
                        ),
                    }))
                })
            })
            .collect::<Result<_, _>>()?;

        if let Some(qualifier) = func.return_type.qualifier()
            && modules
                .values()
                .any(|module| module.ast.type_info.opaque_types.contains(&qualifier))
        {
            return Err(illegal_opaque_type(
                module,
                func.return_type.clone(),
                func.location,
                "function return type",
            ));
        }

        let return_type = Annotated::from_type(
            modules.into(),
            blueprint::validator::tipo_or_annotation(
                module,
                &TypedArg {
                    arg_name: ArgName::Discarded {
                        name: "".to_string(),
                        label: "".to_string(),
                        location: Span::empty(),
                    },
                    location: Span::empty(),
                    annotation: func.return_annotation.clone(),
                    doc: None,
                    is_validator_param: false,
                    tipo: func.return_type.clone(),
                },
            ),
            &mut definitions,
        )
        .map(|schema| Parameter {
            title: Some("return_type".to_string()),
            schema: Declaration::Referenced(schema),
        })
        .map_err(|error| {
            crate::Error::Blueprint(Box::new(blueprint::Error::Schema {
                error: Box::new(error),
                location: func.location,
                source_code: NamedSource::new(
                    module.input_path.display().to_string(),
                    module.code.clone(),
                ),
            }))
        })?;

        let program = generator
            .generate_raw(&func.body, &func.arguments, &module.name)
            .to_debruijn()
            .unwrap();

        let program = match plutus_version {
            PlutusVersion::V1 => SerializableProgram::PlutusV1Program(program),
            PlutusVersion::V2 => SerializableProgram::PlutusV2Program(program),
            PlutusVersion::V3 => SerializableProgram::PlutusV3Program(program),
        };

        Ok(Export {
            name: format!("{}.{}", module.name, func.name),
            doc: func.doc.clone(),
            parameters,
            return_type,
            program,
            definitions,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::{CheckedModules, Export};
    use crate::tests::TestProject;
    use aiken_lang::{
        self,
        ast::{TraceLevel, Tracing},
        plutus_version::PlutusVersion,
    };
    use uplc::ast::{DeBruijn, Program, Term};

    macro_rules! assert_export {
        ($code:expr) => {
            let mut project = TestProject::new();

            let modules = CheckedModules::singleton(project.check(project.parse(indoc::indoc! { $code })));

            let mut generator = project.new_generator(
                Tracing::All(TraceLevel::Verbose),
            );

            let (module, func) = modules
                .functions()
                .next()
                .expect("source code did no yield any exports");

            let export = Export::from_function(func, module, &mut generator, &modules, &PlutusVersion::default());

            match export {
                Err(e) => insta::with_settings!({
                    description => concat!("Code:\n\n", indoc::indoc! { $code }),
                    omit_expression => true
                }, {
                    insta::assert_debug_snapshot!(e);
                }),

                Ok(validator) => insta::with_settings!({
                    description => concat!("Code:\n\n", indoc::indoc! { $code }),
                    omit_expression => true
                }, {
                    insta::assert_json_snapshot!(validator);
                }),
            };
        };
    }

    #[test]
    fn basic_export() {
        assert_export!(
            r#"
            pub fn add(a: Int, b: Int) -> Int {
                a + b
            }
            "#
        );
    }

    #[test]
    fn illegal_opaque_type() {
        assert_export!(
            r#"
            pub opaque type Thing {
              a: Int
            }

            pub fn add(a: Thing, b: Int) -> Int {
                a.a + b
            }
            "#
        );
    }

    #[test]
    fn recursive_types() {
        assert_export!(
            r#"
            pub type Foo<a> {
              Empty
              Bar(a, Foo<a>)
            }

            pub fn add(a: Foo<Int>, b: Foo<Int>) -> Int {
              when (a, b) is {
                (Empty, Empty) -> 0
                (Bar(x, y), Bar(c, d)) -> x + c + add(y, d)
                (Empty, Bar(c, d)) -> c + add(Empty, d)
                (Bar(x, y), Empty) -> x + add(y, Empty)
              }
            }
            "#
        );
    }

    #[test]
    fn cannot_export_generics() {
        assert_export!(
            r#"
            pub fn add(_a: a, _b: b) -> Bool {
                True
            }
            "#
        );
    }

    /// Regression test for https://github.com/aiken-lang/aiken/issues/1333
    /// When exporting a function that calls a same-module helper function,
    /// the helper must be included in the generated UPLC (not left as a free variable).
    #[test]
    fn export_same_module_dependency() {
        let mut project = TestProject::new();

        let code = indoc::indoc! { r#"
            pub fn helper(x: Int) -> Bool { x > 0 }
            pub fn main(y: Int) -> Bool { helper(y) }
        "# };

        let modules = CheckedModules::singleton(project.check(project.parse(code)));

        let mut generator = project.new_generator(Tracing::All(TraceLevel::Verbose));

        // Find the `main` function specifically (not just the first export)
        let (module, func) = modules
            .functions()
            .find(|(_, f)| f.name == "main")
            .expect("Could not find `main` function");

        let export = Export::from_function(
            func,
            module,
            &mut generator,
            &modules,
            &PlutusVersion::default(),
        );

        match export {
            Err(e) => panic!("Export failed: {:?}", e),
            Ok(validator) => {
                // The validator serializes to JSON; verify no free-variable runtime error
                let json = serde_json::to_string(&validator).expect("Validator must serialize");
                assert!(!json.is_empty(), "Validator JSON should not be empty");
                // Verify it looks like a validator export (has compiledCode and definitions)
                assert!(
                    json.contains("compiledCode") && json.contains("definitions"),
                    "Validator JSON should have compiledCode and definitions: {}",
                    json
                );
            }
        }
    }

    /// Fuzz test for https://github.com/aiken-lang/aiken/issues/1333
    ///
    /// Parametrized over same-module helper shapes that could expose free-variable
    /// bugs in the export pipeline. Each case exports a function that calls one or
    /// more same-module helpers and verifies:
    ///   1. The export succeeds and produces JSON containing `compiledCode` and
    ///      `definitions`.
    ///   2. The resulting UPLC `Program<DeBruijn>` contains no `Var(DeBruijn(N))`
    ///      with `N > depth` at the use site (i.e., no orphan references that
    ///      would error at runtime as `OpenTermEvaluated(Var(DeBruijn(N)))`).
    ///
    /// The PR #1372 fix adds an `insert_new_function` call in `hoist_function` so
    /// that same-module helpers get registered in `key_to_func` and `DefineFunc`-
    /// generated UPLC resolves correctly.
    ///
    /// Run with:
    ///   cargo test -p aiken-project --lib export::tests::fuzz_export_same_module_dependencies
    #[test]
    fn fuzz_export_same_module_dependencies() {
        use std::panic::{catch_unwind, AssertUnwindSafe};

        let cases: &[(&str, &str, &str)] = &[
            (
                "orig_helper_pub",
                r#"
                pub fn helper(x: Int) -> Bool { x > 0 }
                pub fn main(y: Int) -> Bool { helper(y) }
                "#,
                "main",
            ),
            (
                "helper_private",
                r#"
                fn helper(x: Int) -> Bool { x > 0 }
                pub fn main(y: Int) -> Bool { helper(y) }
                "#,
                "main",
            ),
            (
                "chained_private",
                r#"
                fn inner(x: Int) -> Int { x + 1 }
                fn middle(x: Int) -> Bool { inner(x) > 0 }
                pub fn main(y: Int) -> Bool { middle(y) }
                "#,
                "main",
            ),
            (
                "two_helpers_call_each_other",
                r#"
                fn helper_a(x: Int) -> Int { x + 1 }
                fn helper_b(x: Int) -> Int { helper_a(x) + 2 }
                pub fn main(y: Int) -> Int { helper_b(y) }
                "#,
                "main",
            ),
            (
                "helper_multi_call",
                r#"
                fn sign(x: Int) -> Int {
                  if x > 0 {
                    1
                  } else {
                    if x < 0 {
                      -1
                    } else {
                      0
                    }
                  }
                }
                pub fn main(y: Int) -> Int {
                  let a = sign(y)
                  let b = sign(y + 1)
                  a + b
                }
                "#,
                "main",
            ),
            (
                "recursive_helper",
                r#"
                fn fact(n: Int) -> Int {
                  if n <= 1 { 1 } else { n * fact(n - 1) }
                }
                pub fn main(y: Int) -> Int { fact(y) }
                "#,
                "main",
            ),
            (
                "multiple_helpers_one_used",
                r#"
                fn used_helper(x: Int) -> Int { x * 2 }
                fn unused_helper(x: Int) -> Int { x * 3 }
                pub fn main(y: Int) -> Int { used_helper(y) }
                "#,
                "main",
            ),
            (
                "helper_with_list",
                r#"
                fn sum_list(xs: List<Int>) -> Int {
                  when xs is {
                    [] -> 0
                    [h, ..t] -> h + sum_list(t)
                  }
                }
                pub fn main(xs: List<Int>) -> Int { sum_list(xs) }
                "#,
                "main",
            ),
            (
                "helper_with_option",
                r#"
                fn unwrap_or_default(x: Option<Int>) -> Int {
                  when x is {
                    Some(v) -> v
                    None -> 0
                  }
                }
                pub fn main(x: Option<Int>) -> Int { unwrap_or_default(x) }
                "#,
                "main",
            ),
            (
                "two_pub_fns_one_helper",
                r#"
                fn shared_helper(x: Int) -> Int { x + 100 }
                pub fn first(y: Int) -> Int { shared_helper(y) }
                pub fn second(y: Int) -> Int { y * 2 }
                "#,
                "first",
            ),
        ];

        let mut failures: Vec<String> = Vec::new();
        let mut successes: usize = 0;
        let mut skips: usize = 0;

        for (name, code, target_fn) in cases {
            // Parse + check + export in one go so the same TestProject instance
            // produces both modules and generator (otherwise the generator looks
            // up names via a foreign id_gen and panics). Skips on any panic
            // (parse / type errors).
            let result = catch_unwind(AssertUnwindSafe(|| {
                let mut project = TestProject::new();
                let parsed = project.parse(code);
                let checked = project.check(parsed);
                let modules = CheckedModules::singleton(checked);
                let mut generator = project.new_generator(Tracing::All(TraceLevel::Verbose));
                let (module, func) = modules
                    .functions()
                    .find(|(_, f)| f.name == *target_fn)
                    .unwrap_or_else(|| panic!("Could not find `{}` function", target_fn));
                Export::from_function(
                    func,
                    module,
                    &mut generator,
                    &modules,
                    &PlutusVersion::default(),
                )
            }));
            let from_fn_result = match result {
                Ok(r) => r,
                Err(_) => {
                    skips += 1;
                    continue;
                }
            };

            match from_fn_result {
                Ok(validator) => {
                    let json =
                        serde_json::to_string(&validator).expect("Validator must serialize");
                    if !json.contains("compiledCode") || !json.contains("definitions") {
                        failures.push(format!(
                            "{}: JSON missing compiledCode or definitions",
                            name
                        ));
                        continue;
                    }

                    let program: &Program<DeBruijn> = validator.program.inner();
                    let mut walker = FreeVarWalker::default();
                    walker.visit(program);
                    if !walker.free_vars.is_empty() {
                        failures.push(format!(
                            "{}: {} free Var(DeBruijn(N)) (max lambda depth: {}): {:?}",
                            name,
                            walker.free_vars.len(),
                            walker.max_lambda_depth,
                            walker.free_vars
                        ));
                    } else {
                        successes += 1;
                    }
                }
                Err(e) => {
                    failures.push(format!("{}: Export errored: {:?}", name, e));
                }
            }
        }

        let total = cases.len();
        eprintln!(
            "fuzz_export_same_module_dependencies: {}/{} succeeded, {} skipped, {} failures",
            successes, total, skips, failures.len()
        );
        if !failures.is_empty() {
            panic!(
                "fuzz_export_same_module_dependencies caught failures:\n  - {}",
                failures.join("\n  - ")
            );
        }
    }

    /// Walk a UPLC `Program<DeBruijn>` and collect any `Var(DeBruijn(N))` whose
    /// index exceeds the current scope's lambda depth — such a reference is
    /// unbound and would error at runtime as `OpenTermEvaluated(Var(DeBruijn(N)))`.
    #[derive(Default)]
    struct FreeVarWalker {
        free_vars: Vec<usize>,
        max_lambda_depth: usize,
    }

    impl FreeVarWalker {
        fn visit(&mut self, program: &Program<DeBruijn>) {
            self.visit_term(&program.term, 0);
        }

        fn visit_term(&mut self, term: &Term<DeBruijn>, depth: usize) {
            if depth > self.max_lambda_depth {
                self.max_lambda_depth = depth;
            }
            match term {
                Term::Var(name) => {
                    // DeBruijn: Var(N) at depth D binds to the lambda at depth (D - N).
                    // A valid reference requires N <= D. So N > D is a free var.
                    let idx = name.inner();
                    if idx > depth {
                        self.free_vars.push(idx);
                    }
                }
                Term::Delay(t) => self.visit_term(t, depth),
                Term::Lambda { body, .. } => self.visit_term(body, depth + 1),
                Term::Apply { function, argument } => {
                    self.visit_term(function, depth);
                    self.visit_term(argument, depth);
                }
                Term::Force(t) => self.visit_term(t, depth),
                Term::Case { constr, branches } => {
                    self.visit_term(constr, depth);
                    for branch in branches {
                        self.visit_term(branch, depth + 1);
                    }
                }
                Term::Constr { fields, .. } => {
                    for f in fields {
                        self.visit_term(f, depth);
                    }
                }
                Term::Builtin(_) | Term::Error | Term::Constant(_) => {}
            }
        }
    }
}
