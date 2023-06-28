use std::sync::Arc;

use crate::{
    ast::{AssignmentKind, BinOp, Constant, Pattern},
    builtins::int,
    gen_uplc::{
        air::{self, Air},
        builder::AssignmentProperties,
        tree::AirTree,
    },
    tipo::{PatternConstructor, Type},
};

pub fn assignment_air_tree(
    pattern: &Pattern<PatternConstructor, Arc<Type>>,
    mut value: AirTree,
    tipo: &Arc<Type>,
    props: AssignmentProperties,
) -> AirTree {
    if props.value_type.is_data() && props.kind.is_expect() && !tipo.is_data() {
        value = AirTree::unwrap_data(value, tipo.clone());
    } else if !props.value_type.is_data() && tipo.is_data() {
        value = AirTree::wrap_data(value, tipo.clone());
    }

    match pattern {
        Pattern::Int {
            value: expected_int,
            ..
        } => {
            if props.kind.is_expect() {
                let name = format!("__expected_by_{}", expected_int);
                let assignment = AirTree::let_assignment(&name, value);

                let expect = AirTree::binop(
                    BinOp::Eq,
                    int(),
                    AirTree::int(expected_int),
                    AirTree::local_var(name, int()),
                );

                AirTree::assert_bool(true, AirTree::hoist_let(assignment, expect))
            } else {
                unreachable!("Code Gen should never reach here")
            }
        }
        Pattern::Var { name, .. } => {
            if props.kind.is_expect() && props.value_type.is_data() && !tipo.is_data() {
                let assignment = AirTree::let_assignment(name, value);
                let expect = todo!();
                let assign = AirTree::let_assignment("_", AirTree::hoist_let(assignment, expect));
                let val = AirTree::local_var(name, tipo.clone());
                AirTree::let_assignment(name, AirTree::hoist_let(assign, val))
            } else {
                AirTree::let_assignment(name, value)
            }
        }
        Pattern::Assign { name, pattern, .. } => {
            let inner_pattern =
                assignment_air_tree(pattern, AirTree::local_var(name, tipo.clone()), tipo, props);

            AirTree::let_assignment(name, inner_pattern)
        }
        Pattern::Discard { name, .. } => {
            if props.kind.is_expect() {
                AirTree::let_assignment(name, value)
            } else {
                AirTree::no_op()
            }
        }
        Pattern::List {
            location,
            elements,
            tail,
        } => todo!(),
        Pattern::Constructor {
            is_record,
            location,
            name,
            arguments,
            module,
            constructor,
            with_spread,
            tipo,
        } => todo!(),
        Pattern::Tuple { location, elems } => todo!(),
    }
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
