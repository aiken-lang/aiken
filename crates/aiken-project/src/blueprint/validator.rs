use super::{
    error::{assert_min_arity, assert_return_bool, Error},
    schema::{Annotated, Schema},
};
use crate::module::{CheckedModule, CheckedModules};
use aiken_lang::{ast::TypedFunction, uplc::CodeGenerator};
use pallas::ledger::primitives::babbage as cardano;
use pallas_traverse::ComputeHash;
use serde::{
    self,
    ser::{Serialize, SerializeStruct, Serializer},
};
use std::fmt::{self, Display};
use uplc::ast::{NamedDeBruijn, Program};

#[derive(Debug, PartialEq, Clone)]
pub struct Validator {
    pub title: String,
    pub purpose: Purpose,
    pub description: Option<String>,
    pub datum: Option<Annotated<Schema>>,
    pub redeemer: Annotated<Schema>,
    pub program: Program<NamedDeBruijn>,
}

#[derive(Debug, PartialEq, Clone, serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub enum Purpose {
    Spend,
    Mint,
    Withdraw,
    Publish,
}

impl Serialize for Validator {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let cbor = self.program.to_cbor().unwrap();

        let source_code = hex::encode(&cbor);

        let hash = cardano::PlutusV2Script(cbor.into()).compute_hash();

        let fields = 5
            + self.description.as_ref().map(|_| 1).unwrap_or_default()
            + self.datum.as_ref().map(|_| 1).unwrap_or_default();

        let mut s = serializer.serialize_struct("Validator", fields)?;
        s.serialize_field("title", &self.title)?;
        s.serialize_field("purpose", &self.purpose)?;
        s.serialize_field("hash", &hash)?;
        if let Some { .. } = self.description {
            s.serialize_field("description", &self.description)?;
        }
        if let Some { .. } = self.datum {
            s.serialize_field("datum", &self.datum)?;
        }
        s.serialize_field("redeemer", &self.redeemer)?;
        s.serialize_field("compiledCode", &source_code)?;
        s.end()
    }
}

impl Display for Validator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = serde_json::to_string_pretty(self).map_err(|_| fmt::Error)?;
        f.write_str(&s)
    }
}

impl Validator {
    pub fn from_checked_module(
        modules: &CheckedModules,
        generator: &mut CodeGenerator,
        validator: &CheckedModule,
        def: &TypedFunction,
    ) -> Result<Validator, Error> {
        let purpose: Purpose = def.name.clone().into();

        assert_return_bool(validator, def)?;
        assert_min_arity(validator, def, purpose.min_arity())?;

        let mut args = def.arguments.iter().rev();
        let (_, redeemer, datum) = (args.next(), args.next().unwrap(), args.next());

        Ok(Validator {
            title: validator.name.clone(),
            description: None,
            purpose,
            datum: datum
                .map(|datum| {
                    Annotated::from_type(modules.into(), &datum.tipo).map_err(Error::Schema)
                })
                .transpose()?,
            redeemer: Annotated::from_type(modules.into(), &redeemer.tipo)
                .map_err(Error::Schema)?,
            program: generator
                .generate(&def.body, &def.arguments, true)
                .try_into()
                .unwrap(),
        })
    }
}

impl Purpose {
    pub fn min_arity(&self) -> u8 {
        match self {
            Purpose::Spend => 3,
            Purpose::Mint | Purpose::Withdraw | Purpose::Publish => 2,
        }
    }
}

impl Display for Purpose {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(match self {
            Purpose::Spend => "spend",
            Purpose::Mint => "mint",
            Purpose::Withdraw => "withdraw",
            Purpose::Publish => "publish",
        })
    }
}

impl From<String> for Purpose {
    fn from(purpose: String) -> Purpose {
        match &purpose[..] {
            "spend" => Purpose::Spend,
            "mint" => Purpose::Mint,
            "withdraw" => Purpose::Withdraw,
            "publish" => Purpose::Publish,
            unexpected => panic!("Can't turn '{}' into any Purpose", unexpected),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{module::ParsedModule, PackageName};
    use aiken_lang::{
        self,
        ast::{ModuleKind, TypedDataType, TypedFunction},
        builder::{DataTypeKey, FunctionAccessKey},
        builtins, parser,
        tipo::TypeInfo,
        IdGenerator,
    };
    use assert_json_diff::assert_json_eq;
    use serde_json::{self, json};
    use std::{collections::HashMap, path::PathBuf};

    // TODO: Possible refactor this out of the module and have it used by `Project`. The idea would
    // be to make this struct below the actual project, and wrap it in another metadata struct
    // which contains all the config and I/O stuff regarding the project.
    struct TestProject {
        package: PackageName,
        id_gen: IdGenerator,
        module_types: HashMap<String, TypeInfo>,
        functions: HashMap<FunctionAccessKey, TypedFunction>,
        data_types: HashMap<DataTypeKey, TypedDataType>,
    }

    impl TestProject {
        fn new() -> Self {
            let id_gen = IdGenerator::new();

            let package = PackageName {
                owner: "test".to_owned(),
                repo: "project".to_owned(),
            };

            let mut module_types = HashMap::new();
            module_types.insert("aiken".to_string(), builtins::prelude(&id_gen));
            module_types.insert("aiken/builtin".to_string(), builtins::plutus(&id_gen));

            let functions = builtins::prelude_functions(&id_gen);
            let data_types = builtins::prelude_data_types(&id_gen);

            TestProject {
                package,
                id_gen,
                module_types,
                functions,
                data_types,
            }
        }

        fn parse(&self, source_code: &str) -> ParsedModule {
            let kind = ModuleKind::Validator;
            let (ast, extra) = parser::module(source_code, kind).unwrap();
            let mut module = ParsedModule {
                kind,
                ast,
                code: source_code.to_string(),
                name: "test".to_owned(),
                path: PathBuf::new(),
                extra,
                package: self.package.to_string(),
            };
            module.attach_doc_and_module_comments();
            module
        }

        fn check(&self, module: ParsedModule) -> CheckedModule {
            let mut warnings = vec![];

            let ast = module
                .ast
                .infer(
                    &self.id_gen,
                    module.kind,
                    &self.package.to_string(),
                    &self.module_types,
                    &mut warnings,
                )
                .unwrap();

            CheckedModule {
                kind: module.kind,
                extra: module.extra,
                name: module.name,
                code: module.code,
                package: module.package,
                input_path: module.path,
                ast,
            }
        }
    }

    fn assert_validator(source_code: &str, json: serde_json::Value) {
        let project = TestProject::new();

        let modules = CheckedModules::singleton(project.check(project.parse(source_code)));
        let mut generator = modules.new_generator(
            &project.functions,
            &project.data_types,
            &project.module_types,
        );

        let (validator, def) = modules
            .validators()
            .next()
            .expect("source code did no yield any validator");

        let validator =
            Validator::from_checked_module(&modules, &mut generator, validator, def).unwrap();

        println!("{}", validator);
        assert_json_eq!(serde_json::to_value(&validator).unwrap(), json);
    }

    #[test]
    fn validator_1() {
        assert_validator(
            r#"
            fn spend(datum: Data, redeemer: Data, ctx: Data) {
                True
            }
            "#,
            json!({
              "title": "test",
              "purpose": "spend",
              "hash": "cf2cd3bed32615bfecbd280618c1c1bec2198fc0f72b04f323a8a0d2",
              "datum": {
                "title": "Data",
                "description": "Any Plutus data."
              },
              "redeemer": {
                "title": "Data",
                "description": "Any Plutus data."
              },
              "compiledCode": "58250100002105646174756d00210872656465656d657200210363747800533357349445261601"
            }),
        );
    }
}
