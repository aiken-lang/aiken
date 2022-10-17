use std::collections::HashMap;

use crate::{
    ast::{ModuleKind, TypedModule, UntypedModule},
    IdGenerator,
};

use super::{
    error::{Error, Warning},
    Module,
};

pub fn module(
    id_gen: &IdGenerator,
    mut module: UntypedModule,
    kind: ModuleKind,
    package: &str,
    modules: &HashMap<String, Module>,
    warnings: &mut Vec<Warning>,
) -> Result<TypedModule, Error> {
    todo!()
}
