use aiken_lang::{ast::TypedFunction, gen_uplc::CodeGenerator};
use miette::NamedSource;
use uplc::ast::{DeBruijn, Program};

use crate::{
    blueprint::{
        self,
        definitions::Definitions,
        parameter::Parameter,
        schema::{Annotated, Schema},
    },
    module::{CheckedModule, CheckedModules},
};

#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
pub struct Export {
    pub name: String,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub doc: Option<String>,

    #[serde(skip_serializing_if = "Vec::is_empty")]
    #[serde(default)]
    pub parameters: Vec<Parameter>,

    #[serde(flatten)]
    pub program: Program<DeBruijn>,

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
    ) -> Result<Export, blueprint::Error> {
        let program = generator
            .generate_raw(&func.body, &func.arguments, &module.name)
            .to_debruijn()
            .unwrap();

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
                    schema,
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

        Ok(Export {
            name: format!("{}.{}", &module.name, &func.name),
            doc: func.doc.clone(),
            parameters,
            program,
            definitions,
        })
    }
}
