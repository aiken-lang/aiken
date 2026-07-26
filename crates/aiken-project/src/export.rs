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
}
