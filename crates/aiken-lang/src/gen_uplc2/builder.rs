use std::sync::Arc;

use crate::{
    ast::Pattern,
    gen_uplc::{builder::AssignmentProperties, tree::AirTree},
    tipo::{PatternConstructor, Type},
};

pub fn assignment_air_tree(
    pattern: &Pattern<PatternConstructor, Arc<Type>>,
    value: AirTree,
    tipo: &Arc<Type>,
    props: AssignmentProperties,
) -> AirTree {
    todo!()
}



