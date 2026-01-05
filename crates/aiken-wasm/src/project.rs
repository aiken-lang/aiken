use crate::{CompileError, ParsedModule, Tracing};
use aiken_lang::{IdGenerator, builtins, tipo::TypeInfo};
use std::collections::HashMap;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct Project {
    id_generator: IdGenerator,
    definitions: HashMap<String, TypeInfo>,
}

#[wasm_bindgen]
impl Project {
    #[wasm_bindgen(constructor)]
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        console_error_panic_hook::set_once();

        let id_generator = IdGenerator::new();

        let mut definitions = HashMap::new();

        definitions.insert("aiken".to_string(), builtins::prelude(&id_generator));
        definitions.insert("aiken/builtin".to_string(), builtins::plutus(&id_generator));

        Self {
            id_generator,
            definitions,
        }
    }
}

#[wasm_bindgen]
impl Project {
    #[wasm_bindgen]
    pub fn add(mut self, module: ParsedModule, tracing: &Tracing) -> Result<Self, CompileError> {
        let mut warnings = vec![];

        let env: Option<&str> = None;

        let typed_module = module.ast.infer(
            &self.id_generator,
            module.kind,
            module.package.as_str(),
            &self.definitions,
            tracing.into(),
            &mut warnings,
            env,
        )?;

        self.definitions
            .insert(typed_module.name.clone(), typed_module.type_info.clone());

        Ok(self)
    }
}
