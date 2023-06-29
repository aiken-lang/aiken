use std::sync::Arc;

use crate::{
    ast::{Constant, Pattern},
    gen_uplc::{builder::AssignmentProperties, tree::AirTree},
    tipo::{PatternConstructor, Type},
};

pub fn convert_opaque_type() -> Arc<Type> {
    todo!()
}

pub fn handle_each_clause(
    pattern: &Pattern<PatternConstructor, Arc<Type>>,
    value: AirTree,
    tipo: &Arc<Type>,
    props: AssignmentProperties,
) -> AirTree {
    todo!()
}

pub fn constants_ir(literal: &Constant) -> AirTree {
    match literal {
        Constant::Int { value, .. } => AirTree::int(value),
        Constant::String { value, .. } => AirTree::string(value),
        Constant::ByteArray { bytes, .. } => AirTree::byte_array(bytes.clone()),
    }
}
