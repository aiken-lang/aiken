use std::collections::HashMap;

use crate::{ast::ModuleKind, tipo};

pub fn prelude() -> tipo::Module {
    let mut prelude = tipo::Module {
        name: vec!["gleam".to_string()],
        package: "".to_string(),
        kind: ModuleKind::Lib,
        types: HashMap::new(),
        types_constructors: HashMap::new(),
        values: HashMap::new(),
        accessors: HashMap::new(),
    };

    prelude
}
