use std::sync::{
    atomic::{AtomicU64, Ordering},
    Arc,
};

pub mod ast;
pub mod builtins;
pub mod error;
pub mod expr;
pub mod format;
pub mod gen_uplc;
pub mod levenshtein;
pub mod line_numbers;
pub mod parser;
pub mod plutus_version;
pub mod pretty;
pub mod test_framework;
pub mod tipo;
pub mod utils;
pub mod version;

#[derive(Debug, Default, Clone)]
pub struct IdGenerator {
    id: Arc<AtomicU64>,
}

impl IdGenerator {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn next(&self) -> u64 {
        self.id.fetch_add(1, Ordering::Relaxed)
    }
}

#[macro_export]
macro_rules! aiken_fn {
    ($module_types:expr, $id_gen:expr, $src:expr) => {{
        let (untyped_module, _) = $crate::parser::module($src, $crate::ast::ModuleKind::Lib)
            .expect("failed to parse module.");

        let module_name = "";

        let mut warnings = vec![];

        let typed_module = untyped_module
            .infer(
                $id_gen,
                $crate::ast::ModuleKind::Lib,
                module_name,
                $module_types,
                $crate::ast::Tracing::silent(),
                &mut warnings,
                None,
            )
            .unwrap();

        if let Some($crate::ast::Definition::Fn(typed_fn)) =
            typed_module.definitions.into_iter().last()
        {
            typed_fn
        } else {
            unreachable!()
        }
    }};
}

#[cfg(test)]
mod tests;
