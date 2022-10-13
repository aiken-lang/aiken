use std::collections::HashMap;

use crate::ast::{ModuleKind, TypedModule, UntypedModule};

use super::{
    error::{Error, Warning},
    Module,
};

pub fn module(
    // ids: &UniqueIdGenerator,
    mut module: UntypedModule,
    kind: ModuleKind,
    package: &str,
    modules: &HashMap<String, Module>,
    warnings: &mut Vec<Warning>,
) -> Result<TypedModule, Error> {
    todo!()
}
