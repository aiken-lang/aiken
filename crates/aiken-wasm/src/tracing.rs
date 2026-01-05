use aiken_lang::ast::{self};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct Tracing(ast::Tracing);

impl From<&Tracing> for ast::Tracing {
    fn from(tracing: &Tracing) -> Self {
        tracing.0
    }
}

#[wasm_bindgen]
impl Tracing {
    #[wasm_bindgen]
    pub fn verbose() -> Self {
        Self(ast::Tracing::verbose())
    }

    #[wasm_bindgen]
    pub fn compact() -> Self {
        Self(ast::Tracing::compact())
    }

    #[wasm_bindgen]
    pub fn silent() -> Self {
        Self(ast::Tracing::silent())
    }
}
