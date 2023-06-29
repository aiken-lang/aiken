use std::sync::Arc;

use indexmap::IndexMap;
use itertools::Itertools;

use crate::{
    ast::{BinOp, Constant, DataType, Pattern},
    builtins::int,
    gen_uplc::{
        builder::{self, AssignmentProperties, DataTypeKey},
        tree::AirTree,
    },
    tipo::{PatternConstructor, Type},
};

pub fn assignment_air_tree(
    pattern: &Pattern<PatternConstructor, Arc<Type>>,
    mut value: AirTree,
    tipo: &Arc<Type>,
    data_types: &IndexMap<DataTypeKey, &DataType<Arc<Type>>>,
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
            location,
            ..
        } => {
            if props.kind.is_expect() {
                let name = format!(
                    "__expected_by_{}_span_{}_{}",
                    expected_int, location.start, location.end
                );
                let assignment = AirTree::let_assignment(&name, value);

                let expect = AirTree::binop(
                    BinOp::Eq,
                    int(),
                    AirTree::int(expected_int),
                    AirTree::local_var(name, int()),
                );
                AirTree::assert_bool(true, AirTree::hoist_over(assignment, expect))
            } else {
                unreachable!("Code Gen should never reach here")
            }
        }
        Pattern::Var { name, .. } => {
            if props.kind.is_expect() && props.value_type.is_data() && !tipo.is_data() {
                let mut index_map = IndexMap::new();
                let tipo = convert_opaque_type();
                let assignment = AirTree::let_assignment(name, value);
                let val = AirTree::local_var(name, tipo.clone());
                let expect = expect_type(&tipo, val.clone(), &mut index_map);
                let assign = AirTree::let_assignment("_", AirTree::hoist_over(assignment, expect));
                AirTree::let_assignment(name, AirTree::hoist_over(assign, val))
            } else {
                AirTree::let_assignment(name, value)
            }
        }
        Pattern::Assign { name, pattern, .. } => {
            let inner_pattern = assignment_air_tree(
                pattern,
                AirTree::local_var(name, tipo.clone()),
                tipo,
                data_types,
                props,
            );
            let assign = AirTree::let_assignment(name, value);

            AirTree::UnhoistedSequence(vec![assign, inner_pattern])
        }
        Pattern::Discard { name, .. } => {
            if props.kind.is_expect() && props.value_type.is_data() && !tipo.is_data() {
                let mut index_map = IndexMap::new();
                let tipo = convert_opaque_type();
                let assignment = AirTree::let_assignment(name, value);
                let val = AirTree::local_var(name, tipo.clone());
                let expect = expect_type(&tipo, val.clone(), &mut index_map);
                let assign = AirTree::let_assignment("_", AirTree::hoist_over(assignment, expect));
                AirTree::let_assignment(name, AirTree::hoist_over(assign, val))
            } else if !props.remove_unused {
                AirTree::let_assignment(name, value)
            } else {
                AirTree::no_op()
            }
        }
        Pattern::List { elements, tail, .. } => {
            assert!(tipo.is_list());
            assert!(props.kind.is_expect());
            let list_elem_types = tipo.get_inner_types();

            let list_elem_type = list_elem_types
                .get(0)
                .unwrap_or_else(|| unreachable!("No list element type?"));

            let mut elems = elements
                .iter()
                .enumerate()
                .map(|(index, elem)| {
                    let elem_name = match elem {
                        Pattern::Var { name, .. } => name.to_string(),
                        Pattern::Assign { name, .. } => name.to_string(),
                        Pattern::Discard { name, .. } => {
                            if props.kind.is_expect()
                                && props.value_type.is_data()
                                && !tipo.is_data()
                            {
                                format!("__discard_{}", name)
                            } else {
                                "_".to_string()
                            }
                        }
                        _ => format!(
                            "elem_{}_span_{}_{}",
                            index,
                            elem.location().start,
                            elem.location().end
                        ),
                    };

                    let val = AirTree::local_var(&elem_name, list_elem_type.clone());

                    (
                        elem_name,
                        assignment_air_tree(
                            elem,
                            val,
                            list_elem_type,
                            data_types,
                            AssignmentProperties {
                                value_type: props.value_type.clone(),
                                kind: props.kind,
                                remove_unused: true,
                            },
                        ),
                    )
                })
                .collect_vec();

            // If Some then push tail onto elems
            tail.iter().for_each(|tail| {
                let tail_name = match tail.as_ref() {
                    Pattern::Var { name, .. } => name.to_string(),
                    Pattern::Assign { name, .. } => name.to_string(),
                    Pattern::Discard { name, .. } => {
                        if props.kind.is_expect() && props.value_type.is_data() && !tipo.is_data() {
                            format!("__discard_{}", name)
                        } else {
                            "_".to_string()
                        }
                    }
                    _ => format!(
                        "tail_span_{}_{}",
                        tail.location().start,
                        tail.location().end
                    ),
                };

                let val = AirTree::local_var(&tail_name, tipo.clone());

                elems.push((
                    tail_name,
                    assignment_air_tree(
                        tail,
                        val,
                        tipo,
                        data_types,
                        AssignmentProperties {
                            value_type: props.value_type.clone(),
                            kind: props.kind,
                            remove_unused: true,
                        },
                    ),
                ));
            });

            let names = elems.iter().map(|(name, _)| name.to_string()).collect_vec();

            let mut sequence = vec![AirTree::list_access(
                names,
                tipo.clone(),
                tail.is_some(),
                tail.is_none(),
                value,
            )];

            sequence.append(&mut elems.into_iter().map(|(_, elem)| elem).collect_vec());

            AirTree::UnhoistedSequence(sequence)
        }
        Pattern::Constructor {
            arguments,
            constructor,
            name,
            ..
        } => {
            let mut sequence = vec![];

            if tipo.is_bool() {
                assert!(props.kind.is_expect());

                sequence.push(AirTree::assert_bool(name == "True", value));

                AirTree::UnhoistedSequence(sequence)
            } else if tipo.is_void() {
                todo!()
            } else {
                if props.kind.is_expect() {
                    let data_type = builder::lookup_data_type_by_tipo(data_types, tipo)
                        .unwrap_or_else(|| panic!("Failed to find definition for {}", name));

                    if data_type.constructors.len() > 1
                        || (!tipo.is_data() && props.value_type.is_data())
                    {
                        let (index, _) = data_type
                            .constructors
                            .iter()
                            .enumerate()
                            .find(|(_, constr)| constr.name == *name)
                            .unwrap_or_else(|| {
                                panic!("Found constructor type {} with 0 constructors", name)
                            });

                        let constructor_name = format!(
                            "__constructor_{}_span_{}_{}",
                            name,
                            pattern.location().start,
                            pattern.location().end
                        );

                        // I'm moving `value` here
                        let constructor_val = AirTree::let_assignment(&constructor_name, value);

                        sequence.push(constructor_val);

                        let assert_constr = AirTree::assert_constr_index(
                            index,
                            AirTree::local_var(&constructor_name, tipo.clone()),
                        );

                        sequence.push(assert_constr);

                        //I'm reusing the `value` pointer
                        value = AirTree::local_var(constructor_name, tipo.clone());
                    }
                }

                let field_map = match constructor {
                    PatternConstructor::Record { field_map, .. } => field_map.clone(),
                };

                let mut type_map: IndexMap<usize, Arc<Type>> = IndexMap::new();

                println!("tipo is {tipo:#?}");

                for (index, arg) in tipo.arg_types().unwrap().iter().enumerate() {
                    let field_type = arg.clone();
                    type_map.insert(index, field_type);
                }

                let fields = arguments
                    .iter()
                    .enumerate()
                    .map(|(index, arg)| {
                        let label = arg.label.clone().unwrap_or_default();

                        let field_index = if let Some(field_map) = &field_map {
                            *field_map.fields.get(&label).map(|x| &x.0).unwrap_or(&index)
                        } else {
                            index
                        };

                        let field_name = match &arg.value {
                            Pattern::Var { name, .. } => name.to_string(),
                            Pattern::Assign { name, .. } => name.to_string(),
                            Pattern::Discard { name, .. } => {
                                if props.kind.is_expect()
                                    && props.value_type.is_data()
                                    && !tipo.is_data()
                                {
                                    format!("__discard_{}", name)
                                } else {
                                    "_".to_string()
                                }
                            }
                            _ => format!(
                                "field_{}_span_{}_{}",
                                field_index,
                                arg.value.location().start,
                                arg.value.location().end
                            ),
                        };

                        let arg_type = type_map.get(&field_index).unwrap_or_else(|| {
                            unreachable!(
                                "Missing type for field {} of constr {}",
                                field_index, name
                            )
                        });

                        let val = AirTree::local_var(field_name.to_string(), arg_type.clone());

                        (
                            field_index,
                            field_name,
                            arg_type.clone(),
                            assignment_air_tree(
                                &arg.value,
                                val,
                                arg_type,
                                data_types,
                                AssignmentProperties {
                                    value_type: props.value_type.clone(),
                                    kind: props.kind,
                                    remove_unused: true,
                                },
                            ),
                        )
                    })
                    .collect_vec();

                let indices = fields
                    .iter()
                    .map(|(index, name, tipo, _)| (*index, name.to_string(), tipo.clone()))
                    .collect_vec();

                // This `value` is either value param that was passed in or
                // local var
                sequence.push(AirTree::fields_expose(indices, false, value));

                sequence.append(
                    &mut fields
                        .into_iter()
                        .map(|(_, _, _, field)| field)
                        .collect_vec(),
                );

                AirTree::UnhoistedSequence(sequence)
            }
        }
        Pattern::Tuple { elems, .. } => {
            todo!()
        }
    }
}

pub fn expect_type(
    tipo: &Arc<Type>,
    value: AirTree,
    defined_data_types: &mut IndexMap<String, u64>,
) -> AirTree {
    todo!()
}

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
