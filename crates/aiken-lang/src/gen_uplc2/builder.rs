use std::sync::Arc;

use crate::{ast::Constant, tipo::Type};

use super::tree::AirTree;

pub fn convert_opaque_type() -> Arc<Type> {
    todo!()
}

pub fn constants_ir(literal: &Constant) -> AirTree {
    match literal {
        Constant::Int { value, .. } => AirTree::int(value),
        Constant::String { value, .. } => AirTree::string(value),
        Constant::ByteArray { bytes, .. } => AirTree::byte_array(bytes.clone()),
    }
}
