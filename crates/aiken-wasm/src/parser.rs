use crate::CompileError;
use aiken_lang::{
    ast::{self, ModuleKind},
    parser,
};
use wasm_bindgen::prelude::*;

const DEFAULT_MODULE_NAME: &str = "__inline";

const DEFAULT_PACKAGE: &str = "./.";

#[wasm_bindgen]
#[derive(Debug, Clone)]
pub struct ParsedModule {
    pub(crate) package: String,
    pub(crate) kind: ModuleKind,
    pub(crate) ast: ast::UntypedModule,
}

#[wasm_bindgen]
pub fn parse(source_code: &str) -> Result<ParsedModule, CompileError> {
    parse_as(
        source_code,
        DEFAULT_PACKAGE,
        DEFAULT_MODULE_NAME,
        ModuleKind::Lib,
    )
}

#[wasm_bindgen]
pub fn parse_as(
    source_code: &str,
    package: &str,
    name: &str,
    kind: ModuleKind,
) -> Result<ParsedModule, CompileError> {
    let (mut ast, _module_extra) = parser::module(source_code, kind)?;
    ast.name = name.to_string();
    Ok(ParsedModule {
        ast,
        kind,
        package: package.to_string(),
    })
}
