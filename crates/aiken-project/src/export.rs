use crate::{
    blueprint::{
        self,
        definitions::Definitions,
        parameter::Parameter,
        schema::{Annotated, Declaration, Schema},
        source_map::SourceMap,
    },
    module::{CheckedModule, CheckedModules},
    options::SourceMapMode,
};
use aiken_lang::{
    ast::{ArgName, Span, TypedArg, TypedFunction},
    gen_uplc::CodeGenerator,
    line_numbers::LineNumbers,
    plutus_version::PlutusVersion,
};
use indexmap::IndexMap;
use miette::NamedSource;
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

    /// Inline source map (when embedded in export)
    #[serde(rename = "sourceMap", skip_serializing_if = "Option::is_none")]
    pub source_map: Option<SourceMap>,
}

impl Export {
    pub fn from_function(
        func: &TypedFunction,
        module: &CheckedModule,
        generator: &mut CodeGenerator,
        modules: &CheckedModules,
        plutus_version: &PlutusVersion,
        source_map_mode: &SourceMapMode,
        module_sources: &IndexMap<&str, &(String, LineNumbers)>,
    ) -> Result<Export, blueprint::Error> {
        let mut definitions = Definitions::new();

        let parameters = func
            .arguments
            .iter()
            .map(|param| {
                Annotated::from_type(
                    modules.into(),
                    blueprint::validator::tipo_or_annotation(module, param),
                    &mut definitions,
                )
                .map(|schema| Parameter {
                    title: Some(param.arg_name.get_label()),
                    schema: Declaration::Referenced(schema),
                })
                .map_err(|error| blueprint::Error::Schema {
                    error,
                    location: param.location,
                    source_code: NamedSource::new(
                        module.input_path.display().to_string(),
                        module.code.clone(),
                    ),
                })
            })
            .collect::<Result<_, _>>()?;

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
        .map_err(|error| blueprint::Error::Schema {
            error,
            location: func.location,
            source_code: NamedSource::new(
                module.input_path.display().to_string(),
                module.code.clone(),
            ),
        })?;

        // Generate the term with spans if source maps are requested
        let (program, source_map) = match source_map_mode {
            SourceMapMode::None => {
                let program = generator
                    .generate_raw(&func.body, &func.arguments, &module.name)
                    .to_debruijn()
                    .unwrap();
                (program, None)
            }
            SourceMapMode::Inline | SourceMapMode::External(_) => {
                // Generate with spans to build source map
                let term_with_spans =
                    generator.generate_raw_with_spans(&func.body, &func.arguments, &module.name);

                // Build source map from the term
                let source_map =
                    SourceMap::from_term(&term_with_spans, &module.name, module_sources);

                // Convert to program and then to DeBruijn
                // Strip the Span context from the term
                let term = term_with_spans.map_context(|_| ());

                // Create version tuple based on plutus version
                let version = match plutus_version {
                    PlutusVersion::V1 | PlutusVersion::V2 => (1, 0, 0),
                    PlutusVersion::V3 => (1, 1, 0),
                };

                let program = uplc::ast::Program { version, term }
                    .to_debruijn()
                    .unwrap();

                (program, Some(source_map))
            }
        };

        let program = match plutus_version {
            PlutusVersion::V1 => SerializableProgram::PlutusV1Program(program),
            PlutusVersion::V2 => SerializableProgram::PlutusV2Program(program),
            PlutusVersion::V3 => SerializableProgram::PlutusV3Program(program),
        };

        Ok(Export {
            name: format!("{}.{}", &module.name, &func.name),
            doc: func.doc.clone(),
            parameters,
            return_type,
            program,
            definitions,
            source_map,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::{CheckedModules, Export};
    use crate::{options::SourceMapMode, tests::TestProject};
    use aiken_lang::{
        self,
        ast::{TraceLevel, Tracing},
        plutus_version::PlutusVersion,
    };
    use indexmap::IndexMap;

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

            let module_sources: IndexMap<&str, &(String, aiken_lang::line_numbers::LineNumbers)> = IndexMap::new();

            let export = Export::from_function(
                func,
                module,
                &mut generator,
                &modules,
                &PlutusVersion::default(),
                &SourceMapMode::None,
                &module_sources,
            );

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
}
