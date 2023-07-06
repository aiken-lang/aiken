pub mod air;
mod builder;
pub mod tree;

use std::sync::Arc;

use indexmap::{IndexMap, IndexSet};
use itertools::Itertools;
use uplc::{
    ast::{Name, Program, Term},
    builder::EXPECT_ON_LIST,
};

use crate::{
    ast::{
        AssignmentKind, BinOp, Pattern, Span, TypedClause, TypedDataType, TypedFunction,
        TypedValidator,
    },
    builtins::{bool, int, void},
    expr::TypedExpr,
    gen_uplc::{
        air::Air,
        builder::{
            self as build, AssignmentProperties, ClauseProperties, DataTypeKey, FunctionAccessKey,
            SpecificClause,
        },
    },
    gen_uplc2::builder::convert_opaque_type,
    tipo::{
        ModuleValueConstructor, PatternConstructor, Type, TypeInfo, ValueConstructor,
        ValueConstructorVariant,
    },
};

use self::tree::AirTree;

#[derive(Clone, Debug)]
pub enum CodeGenFunction {
    Function(AirTree, Vec<String>),
    Link(String),
}

#[derive(Clone)]
pub struct CodeGenerator<'a> {
    defined_functions: IndexMap<FunctionAccessKey, ()>,
    functions: IndexMap<FunctionAccessKey, &'a TypedFunction>,
    data_types: IndexMap<DataTypeKey, &'a TypedDataType>,
    module_types: IndexMap<&'a String, &'a TypeInfo>,
    needs_field_access: bool,
    code_gen_functions: IndexMap<String, CodeGenFunction>,
    zero_arg_functions: IndexMap<FunctionAccessKey, Vec<Air>>,
    tracing: bool,
}

impl<'a> CodeGenerator<'a> {
    pub fn new(
        functions: IndexMap<FunctionAccessKey, &'a TypedFunction>,
        data_types: IndexMap<DataTypeKey, &'a TypedDataType>,
        module_types: IndexMap<&'a String, &'a TypeInfo>,
        tracing: bool,
    ) -> Self {
        CodeGenerator {
            defined_functions: IndexMap::new(),
            functions,
            data_types,
            module_types,
            needs_field_access: false,
            code_gen_functions: IndexMap::new(),
            zero_arg_functions: IndexMap::new(),
            tracing,
        }
    }

    pub fn reset(&mut self) {
        self.code_gen_functions = IndexMap::new();
        self.zero_arg_functions = IndexMap::new();
        self.needs_field_access = false;
        self.defined_functions = IndexMap::new();
    }

    pub fn generate(
        &mut self,
        TypedValidator {
            fun: _,
            other_fun: _,
            params: _,
            ..
        }: &TypedValidator,
    ) -> Program<Name> {
        todo!()
    }

    pub fn generate_test(&mut self, test_body: &TypedExpr) -> Program<Name> {
        let mut air_tree = self.build(test_body);

        air_tree = AirTree::no_op().hoist_over(air_tree);
        println!("{:#?}", air_tree);
        println!("{:#?}", air_tree.to_vec());

        todo!()
    }

    fn finalize(&mut self, _term: Term<Name>) -> Program<Name> {
        todo!()
    }

    fn build(&mut self, body: &TypedExpr) -> AirTree {
        match body {
            TypedExpr::Int { value, .. } => AirTree::int(value),
            TypedExpr::String { value, .. } => AirTree::string(value),
            TypedExpr::ByteArray { bytes, .. } => AirTree::byte_array(bytes.clone()),
            TypedExpr::Sequence { expressions, .. } | TypedExpr::Pipeline { expressions, .. } => {
                let mut expressions = expressions.clone();

                let mut last_exp = self.build(&expressions.pop().unwrap_or_else(|| {
                    unreachable!("Sequence or Pipeline should have at least one expression")
                }));

                while let Some(expression) = expressions.pop() {
                    let exp_tree = self.build(&expression);

                    last_exp = exp_tree.hoist_over(last_exp);
                }
                last_exp
            }

            TypedExpr::Var {
                constructor, name, ..
            } => AirTree::var(constructor.clone(), name, ""),

            TypedExpr::Fn { args, body, .. } => AirTree::anon_func(
                args.iter()
                    .map(|arg| arg.arg_name.get_variable_name().unwrap_or("_").to_string())
                    .collect_vec(),
                self.build(body),
            ),

            TypedExpr::List {
                tipo,
                elements,
                tail,
                ..
            } => AirTree::list(
                elements.iter().map(|elem| self.build(elem)).collect_vec(),
                tipo.clone(),
                tail.as_ref().map(|tail| self.build(tail)),
            ),

            TypedExpr::Call {
                tipo, fun, args, ..
            } => match fun.as_ref() {
                TypedExpr::Var {
                    constructor:
                        ValueConstructor {
                            variant:
                                ValueConstructorVariant::Record {
                                    name: constr_name, ..
                                },
                            ..
                        },
                    ..
                }
                | TypedExpr::ModuleSelect {
                    constructor:
                        ModuleValueConstructor::Record {
                            name: constr_name, ..
                        },
                    ..
                } => {
                    let Some(data_type) = build::lookup_data_type_by_tipo(&self.data_types, tipo)
                    else {unreachable!("Creating a record with no record definition.")};

                    let (constr_index, _) = data_type
                        .constructors
                        .iter()
                        .enumerate()
                        .find(|(_, dt)| &dt.name == constr_name)
                        .unwrap();

                    let constr_args = args.iter().map(|arg| self.build(&arg.value)).collect_vec();

                    AirTree::create_constr(constr_index, tipo.clone(), constr_args)
                }

                TypedExpr::Var {
                    constructor:
                        ValueConstructor {
                            variant: ValueConstructorVariant::ModuleFn { builtin, .. },
                            ..
                        },
                    ..
                } => {
                    let Some(fun_arg_types) = fun.tipo().arg_types() else {unreachable!("Expected a function type with arguments")};

                    let func_args = args
                        .iter()
                        .zip(fun_arg_types)
                        .map(|(arg, arg_tipo)| {
                            let mut arg_val = self.build(&arg.value);

                            if arg_tipo.is_data() && !arg.value.tipo().is_data() {
                                arg_val = AirTree::wrap_data(arg_val, arg.value.tipo())
                            }
                            arg_val
                        })
                        .collect_vec();

                    if let Some(func) = builtin {
                        AirTree::builtin(*func, tipo.clone(), func_args)
                    } else {
                        AirTree::call(self.build(fun.as_ref()), tipo.clone(), func_args)
                    }
                }

                TypedExpr::ModuleSelect {
                    tipo,
                    module_name,
                    constructor: ModuleValueConstructor::Fn { name, .. },
                    ..
                } => {
                    let type_info = self.module_types.get(module_name).unwrap();
                    let value = type_info.values.get(name).unwrap();

                    let ValueConstructorVariant::ModuleFn { builtin, .. } = &value.variant else {unreachable!("Missing module function definition")};

                    let Some(fun_arg_types) = fun.tipo().arg_types() else {unreachable!("Expected a function type with arguments")};

                    let func_args = args
                        .iter()
                        .zip(fun_arg_types)
                        .map(|(arg, arg_tipo)| {
                            let mut arg_val = self.build(&arg.value);

                            if arg_tipo.is_data() && !arg.value.tipo().is_data() {
                                arg_val = AirTree::wrap_data(arg_val, arg.value.tipo())
                            }
                            arg_val
                        })
                        .collect_vec();

                    if let Some(func) = builtin {
                        AirTree::builtin(*func, tipo.clone(), func_args)
                    } else {
                        AirTree::call(self.build(fun.as_ref()), tipo.clone(), func_args)
                    }
                }
                _ => {
                    let Some(fun_arg_types) = fun.tipo().arg_types() else {unreachable!("Expected a function type with arguments")};

                    let func_args = args
                        .iter()
                        .zip(fun_arg_types)
                        .map(|(arg, arg_tipo)| {
                            let mut arg_val = self.build(&arg.value);

                            if arg_tipo.is_data() && !arg.value.tipo().is_data() {
                                arg_val = AirTree::wrap_data(arg_val, arg.value.tipo())
                            }
                            arg_val
                        })
                        .collect_vec();

                    AirTree::call(self.build(fun.as_ref()), tipo.clone(), func_args)
                }
            },
            TypedExpr::BinOp {
                name,
                tipo,
                left,
                right,
                ..
            } => AirTree::binop(*name, tipo.clone(), self.build(left), self.build(right)),

            TypedExpr::Assignment {
                tipo,
                value,
                pattern,
                kind,
                ..
            } => {
                let mut replaced_type = tipo.clone();
                build::replace_opaque_type(&mut replaced_type, &self.data_types);

                let air_value = self.build(value);

                self.assignment(
                    pattern,
                    air_value,
                    tipo,
                    AssignmentProperties {
                        value_type: value.tipo(),
                        kind: *kind,
                        remove_unused: kind.is_let(),
                        full_check: !tipo.is_data() && value.tipo().is_data() && kind.is_expect(),
                    },
                )
            }

            TypedExpr::Trace {
                tipo, then, text, ..
            } => AirTree::trace(self.build(text), tipo.clone(), self.build(then)),

            TypedExpr::When {
                tipo,
                subject,
                clauses,
                ..
            } => {
                let mut clauses = clauses.clone();

                if clauses.is_empty() {
                    unreachable!("We should have one clause at least")
                } else if clauses.len() == 1 {
                    let last_clause = clauses.pop().unwrap();

                    let clause_then = self.build(&last_clause.then);

                    let subject_val = self.build(subject);

                    let assignment = self.assignment(
                        &last_clause.pattern,
                        subject_val,
                        tipo,
                        AssignmentProperties {
                            value_type: subject.tipo(),
                            kind: AssignmentKind::Let,
                            remove_unused: false,
                            full_check: false,
                        },
                    );

                    assignment.hoist_over(clause_then)
                } else {
                    clauses = if subject.tipo().is_list() {
                        build::rearrange_clauses(clauses)
                    } else {
                        clauses
                    };

                    let last_clause = clauses.pop().unwrap();

                    let constr_var = format!(
                        "__when_var_span_{}_{}",
                        subject.location().start,
                        subject.location().end
                    );

                    let subject_name = format!(
                        "__subject_var_span_{}_{}",
                        subject.location().start,
                        subject.location().end
                    );

                    let clauses = self.handle_each_clause(
                        &clauses,
                        last_clause,
                        &subject.tipo(),
                        &mut ClauseProperties::init(
                            &subject.tipo(),
                            constr_var.clone(),
                            subject_name.clone(),
                        ),
                    );

                    let constr_assign = AirTree::let_assignment(&constr_var, self.build(subject));
                    let when_assign = AirTree::when(
                        subject_name,
                        subject.tipo(),
                        AirTree::local_var(constr_var, tipo.clone()),
                        clauses,
                    );

                    constr_assign.hoist_over(when_assign)
                }
            }

            TypedExpr::If {
                branches,
                final_else,
                tipo,
                ..
            } => AirTree::if_branches(
                branches
                    .iter()
                    .map(|branch| (self.build(&branch.condition), self.build(&branch.body)))
                    .collect_vec(),
                tipo.clone(),
                self.build(final_else),
            ),

            TypedExpr::RecordAccess {
                tipo,
                index,
                record,
                ..
            } => AirTree::record_access(*index, tipo.clone(), self.build(record)),

            TypedExpr::ModuleSelect {
                tipo,
                module_name,
                constructor,
                ..
            } => match constructor {
                ModuleValueConstructor::Record {
                    name,
                    arity,
                    tipo,
                    field_map,
                    ..
                } => {
                    let data_type = build::lookup_data_type_by_tipo(&self.data_types, tipo);

                    let val_constructor = ValueConstructor::public(
                        tipo.clone(),
                        ValueConstructorVariant::Record {
                            name: name.clone(),
                            arity: *arity,
                            field_map: field_map.clone(),
                            location: Span::empty(),
                            module: module_name.clone(),
                            constructors_count: data_type
                                .unwrap_or_else(|| {
                                    unreachable!("Created a module type without a definition?")
                                })
                                .constructors
                                .len() as u16,
                        },
                    );

                    AirTree::var(val_constructor, name, "")
                }
                ModuleValueConstructor::Fn { name, module, .. } => {
                    let func = self.functions.get(&FunctionAccessKey {
                        module_name: module_name.clone(),
                        function_name: name.clone(),
                        variant_name: String::new(),
                    });

                    let type_info = self.module_types.get(module_name).unwrap();
                    let value = type_info.values.get(name).unwrap();

                    if let Some(_func) = func {
                        AirTree::var(
                            ValueConstructor::public(tipo.clone(), value.variant.clone()),
                            format!("{module}_{name}"),
                            "",
                        )
                    } else {
                        let ValueConstructorVariant::ModuleFn {
                            builtin: Some(builtin), ..
                        } = &value.variant else {
                            unreachable!("Didn't find the function definition.")
                        };

                        AirTree::builtin(*builtin, tipo.clone(), vec![])
                    }
                }
                ModuleValueConstructor::Constant { literal, .. } => builder::constants_ir(literal),
            },

            TypedExpr::Tuple { tipo, elems, .. } => AirTree::tuple(
                elems.iter().map(|elem| self.build(elem)).collect_vec(),
                tipo.clone(),
            ),

            TypedExpr::TupleIndex {
                tipo, index, tuple, ..
            } => AirTree::tuple_index(*index, tipo.clone(), self.build(tuple)),

            TypedExpr::ErrorTerm { tipo, .. } => AirTree::error(tipo.clone()),

            TypedExpr::RecordUpdate {
                tipo, spread, args, ..
            } => {
                let mut index_types = vec![];
                let mut update_args = vec![];

                let mut highest_index = 0;

                for arg in args
                    .iter()
                    .sorted_by(|arg1, arg2| arg1.index.cmp(&arg2.index))
                {
                    let arg_val = self.build(&arg.value);

                    if arg.index > highest_index {
                        highest_index = arg.index;
                    }

                    index_types.push((arg.index, arg.value.tipo()));
                    update_args.push(arg_val);
                }

                AirTree::record_update(
                    index_types,
                    highest_index,
                    tipo.clone(),
                    self.build(spread),
                    update_args,
                )
            }

            TypedExpr::UnOp { value, op, .. } => AirTree::unop(*op, self.build(value)),
        }
    }

    pub fn assignment(
        &mut self,
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
                    AirTree::assert_bool(true, assignment.hoist_over(expect))
                } else {
                    unreachable!("Code Gen should never reach here")
                }
            }
            Pattern::Var { name, .. } => {
                if props.full_check {
                    let mut index_map = IndexMap::new();
                    // let tipo = builder::convert_opaque_type();
                    let assignment = AirTree::let_assignment(name, value);
                    let val = AirTree::local_var(name, tipo.clone());

                    if tipo.is_primitive() {
                        AirTree::let_assignment(name, assignment.hoist_over(val))
                    } else {
                        let expect = self.expect_type_assign(
                            tipo,
                            val.clone(),
                            &mut index_map,
                            pattern.location(),
                        );
                        let assign = AirTree::let_assignment("_", assignment.hoist_over(expect));
                        AirTree::let_assignment(name, assign.hoist_over(val))
                    }
                } else {
                    AirTree::let_assignment(name, value)
                }
            }
            Pattern::Assign { name, pattern, .. } => {
                let inner_pattern =
                    self.assignment(pattern, AirTree::local_var(name, tipo.clone()), tipo, props);
                let assign = AirTree::let_assignment(name, value);

                AirTree::UnhoistedSequence(vec![assign, inner_pattern])
            }
            Pattern::Discard { name, .. } => {
                if props.full_check {
                    let name = &format!("__discard_expect_{}", name);
                    let mut index_map = IndexMap::new();
                    // let tipo = builder::convert_opaque_type();
                    let assignment = AirTree::let_assignment(name, value);
                    let val = AirTree::local_var(name, tipo.clone());
                    if tipo.is_primitive() {
                        AirTree::let_assignment(name, assignment.hoist_over(val))
                    } else {
                        let expect = self.expect_type_assign(
                            tipo,
                            val.clone(),
                            &mut index_map,
                            pattern.location(),
                        );
                        let assign = AirTree::let_assignment("_", assignment.hoist_over(expect));
                        AirTree::let_assignment(name, assign.hoist_over(val))
                    }
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
                                if props.full_check {
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
                            self.assignment(
                                elem,
                                val,
                                list_elem_type,
                                AssignmentProperties {
                                    value_type: list_elem_type.clone(),
                                    kind: props.kind,
                                    remove_unused: true,
                                    full_check: props.full_check,
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
                            "tail_span_{}_{}",
                            tail.location().start,
                            tail.location().end
                        ),
                    };

                    let val = AirTree::local_var(&tail_name, tipo.clone());

                    elems.push((
                        tail_name,
                        self.assignment(
                            tail,
                            val,
                            tipo,
                            AssignmentProperties {
                                value_type: tipo.clone(),
                                kind: props.kind,
                                remove_unused: true,
                                full_check: props.full_check,
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

                    AirTree::assert_bool(name == "True", value)
                } else if tipo.is_void() {
                    AirTree::let_assignment("_", value)
                } else {
                    if props.kind.is_expect() {
                        let data_type = build::lookup_data_type_by_tipo(&self.data_types, tipo)
                            .unwrap_or_else(|| panic!("Failed to find definition for {}", name));

                        if data_type.constructors.len() > 1 || props.full_check {
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

                            // I'm consuming `value` here
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
                                    if props.full_check {
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
                                self.assignment(
                                    &arg.value,
                                    val,
                                    arg_type,
                                    AssignmentProperties {
                                        value_type: props.value_type.clone(),
                                        kind: props.kind,
                                        remove_unused: true,
                                        full_check: props.full_check,
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
                    sequence.push(AirTree::fields_expose(indices, props.full_check, value));

                    sequence.append(
                        &mut fields
                            .into_iter()
                            .map(|(_, _, _, field)| field)
                            .collect_vec(),
                    );

                    AirTree::UnhoistedSequence(sequence)
                }
            }
            Pattern::Tuple {
                elems, location, ..
            } => {
                let mut type_map: IndexMap<usize, Arc<Type>> = IndexMap::new();

                let mut sequence = vec![];

                for (index, arg) in tipo.get_inner_types().iter().enumerate() {
                    let field_type = arg.clone();
                    type_map.insert(index, field_type);
                }

                let elems = elems
                    .iter()
                    .enumerate()
                    .map(|(index, arg)| {
                        let tuple_name = match &arg {
                            Pattern::Var { name, .. } => name.to_string(),
                            Pattern::Assign { name, .. } => name.to_string(),
                            Pattern::Discard { name, .. } => {
                                if props.full_check {
                                    format!("__discard_{}", name)
                                } else {
                                    "_".to_string()
                                }
                            }
                            _ => format!(
                                "tuple_{}_span_{}_{}",
                                index,
                                arg.location().start,
                                arg.location().end
                            ),
                        };

                        let arg_type = type_map.get(&index).unwrap_or_else(|| {
                            unreachable!(
                                "Missing type for tuple index {} of tuple_span_{}_{}",
                                index, location.start, location.end
                            )
                        });

                        let val = AirTree::local_var(tuple_name.to_string(), arg_type.clone());

                        (
                            tuple_name,
                            self.assignment(
                                arg,
                                val,
                                arg_type,
                                AssignmentProperties {
                                    value_type: props.value_type.clone(),
                                    kind: props.kind,
                                    remove_unused: true,
                                    full_check: props.full_check,
                                },
                            ),
                        )
                    })
                    .collect_vec();

                let indices = elems.iter().map(|(name, _)| name.to_string()).collect_vec();

                // This `value` is either value param that was passed in or
                // local var
                sequence.push(AirTree::tuple_access(
                    indices,
                    tipo.clone(),
                    props.full_check,
                    value,
                ));

                sequence.append(&mut elems.into_iter().map(|(_, field)| field).collect_vec());

                AirTree::UnhoistedSequence(sequence)
            }
        }
    }

    pub fn expect_type_assign(
        &mut self,
        tipo: &Arc<Type>,
        value: AirTree,
        defined_data_types: &mut IndexMap<String, u64>,
        location: Span,
    ) -> AirTree {
        assert!(tipo.get_generic().is_none());
        if tipo.is_primitive() {
            AirTree::void()
        } else if tipo.is_map() {
            assert!(!tipo.get_inner_types().is_empty());
            let inner_list_type = &tipo.get_inner_types()[0];
            let inner_pair_types = inner_list_type.get_inner_types();
            assert!(inner_pair_types.len() == 2);

            let map_name = format!("__map_span_{}_{}", location.start, location.end);
            let pair_name = format!("__pair_span_{}_{}", location.start, location.end);
            let fst_name = format!("__pair_fst_span_{}_{}", location.start, location.end);
            let snd_name = format!("__pair_snd_span_{}_{}", location.start, location.end);

            let assign = AirTree::let_assignment(&map_name, value);

            let tuple_access = AirTree::tuple_access(
                vec![fst_name.clone(), snd_name.clone()],
                inner_list_type.clone(),
                false,
                AirTree::local_var(&pair_name, inner_list_type.clone()),
            );

            let expect_fst = self.expect_type_assign(
                &inner_pair_types[0],
                AirTree::local_var(fst_name, inner_pair_types[0].clone()),
                defined_data_types,
                location,
            );

            let expect_snd = self.expect_type_assign(
                &inner_pair_types[1],
                AirTree::local_var(snd_name, inner_pair_types[1].clone()),
                defined_data_types,
                location,
            );

            let anon_func_body = AirTree::UnhoistedSequence(vec![
                tuple_access,
                AirTree::let_assignment("_", expect_fst),
                AirTree::let_assignment("_", expect_snd),
            ])
            .hoist_over(AirTree::void());

            let unwrap_function = AirTree::anon_func(vec![pair_name], anon_func_body);

            let function = self.code_gen_functions.get(EXPECT_ON_LIST);

            if function.is_none() {
                let expect_list_func = AirTree::expect_on_list();
                self.code_gen_functions.insert(
                    EXPECT_ON_LIST.to_string(),
                    CodeGenFunction::Function(expect_list_func, vec![]),
                );
            }

            if let Some(counter) = defined_data_types.get_mut(EXPECT_ON_LIST) {
                *counter += 1
            } else {
                defined_data_types.insert(EXPECT_ON_LIST.to_string(), 1);
            }

            let func_call = AirTree::call(
                AirTree::local_var(EXPECT_ON_LIST, void()),
                void(),
                vec![AirTree::local_var(map_name, tipo.clone()), unwrap_function],
            );

            assign.hoist_over(func_call)
        } else if tipo.is_list() {
            assert!(!tipo.get_inner_types().is_empty());
            let inner_list_type = &tipo.get_inner_types()[0];

            let list_name = format!("__list_span_{}_{}", location.start, location.end);
            let item_name = format!("__item_span_{}_{}", location.start, location.end);

            let assign = AirTree::let_assignment(&list_name, value);

            let expect_item = self.expect_type_assign(
                inner_list_type,
                AirTree::local_var(&item_name, inner_list_type.clone()),
                defined_data_types,
                location,
            );

            let anon_func_body =
                AirTree::let_assignment("_", expect_item).hoist_over(AirTree::void());

            let unwrap_function = AirTree::anon_func(vec![item_name], anon_func_body);

            let function = self.code_gen_functions.get(EXPECT_ON_LIST);

            if function.is_none() {
                let expect_list_func = AirTree::expect_on_list();
                self.code_gen_functions.insert(
                    EXPECT_ON_LIST.to_string(),
                    CodeGenFunction::Function(expect_list_func, vec![]),
                );
            }

            if let Some(counter) = defined_data_types.get_mut(EXPECT_ON_LIST) {
                *counter += 1
            } else {
                defined_data_types.insert(EXPECT_ON_LIST.to_string(), 1);
            }

            let func_call = AirTree::call(
                AirTree::local_var(EXPECT_ON_LIST, void()),
                void(),
                vec![AirTree::local_var(list_name, tipo.clone()), unwrap_function],
            );

            assign.hoist_over(func_call)
        } else if tipo.is_2_tuple() {
            let tuple_inner_types = tipo.get_inner_types();

            let pair_name = format!("__pair_span_{}_{}", location.start, location.end);

            let fst_name = format!("__pair_fst_span_{}_{}", location.start, location.end);
            let snd_name = format!("__pair_snd_span_{}_{}", location.start, location.end);

            let tuple_assign = AirTree::let_assignment(&pair_name, value);

            let tuple_access = AirTree::tuple_access(
                vec![fst_name.clone(), snd_name.clone()],
                tipo.clone(),
                false,
                AirTree::local_var(pair_name, tipo.clone()),
            );

            let expect_fst = self.expect_type_assign(
                &tuple_inner_types[0],
                AirTree::local_var(fst_name, tuple_inner_types[0].clone()),
                defined_data_types,
                location,
            );

            let expect_snd = self.expect_type_assign(
                &tuple_inner_types[1],
                AirTree::local_var(snd_name, tuple_inner_types[1].clone()),
                defined_data_types,
                location,
            );

            AirTree::UnhoistedSequence(vec![
                tuple_assign,
                tuple_access,
                AirTree::let_assignment("_", expect_fst),
                AirTree::let_assignment("_", expect_snd),
            ])
            .hoist_over(AirTree::void())
        } else if tipo.is_tuple() {
            let tuple_inner_types = tipo.get_inner_types();

            let tuple_name = format!("__tuple_span_{}_{}", location.start, location.end);
            let tuple_assign = AirTree::let_assignment(&tuple_name, value);

            let tuple_expect_items = tuple_inner_types
                .iter()
                .enumerate()
                .map(|(index, arg)| {
                    let tuple_index_name = format!(
                        "__tuple_index_{}_span_{}_{}",
                        index, location.start, location.end
                    );

                    let expect_tuple_item = self.expect_type_assign(
                        tipo,
                        AirTree::local_var(&tuple_index_name, arg.clone()),
                        defined_data_types,
                        location,
                    );

                    (tuple_index_name, expect_tuple_item)
                })
                .collect_vec();

            let tuple_index_names = tuple_expect_items
                .iter()
                .map(|(name, _)| name.clone())
                .collect_vec();

            let tuple_access = AirTree::tuple_access(
                tuple_index_names,
                tipo.clone(),
                false,
                AirTree::local_var(tuple_name, tipo.clone()),
            );

            let mut tuple_expects = tuple_expect_items
                .into_iter()
                .map(|(_, item)| item)
                .collect_vec();

            let mut sequence = vec![tuple_assign, tuple_access];

            sequence.append(&mut tuple_expects);

            AirTree::UnhoistedSequence(sequence).hoist_over(AirTree::void())
        } else {
            let data_type =
                build::lookup_data_type_by_tipo(&self.data_types, tipo).unwrap_or_else(|| {
                    unreachable!("We need a data type definition fot type {:#?}", tipo)
                });

            let mut tipo = convert_opaque_type();

            // for (index, arg) in tipo.arg_types().unwrap().iter().enumerate() {
            //     let field_type = arg.clone();
            //     type_map.insert(index, field_type);
            // }

            // TODO calculate the variant name.
            let data_type_name = format!("__expect_{}{}", data_type.name, "");
            let function = self.code_gen_functions.get(&data_type_name);
            todo!();
            if function.is_none() && defined_data_types.get(&data_type_name).is_none() {
                todo!()
            } else if let Some(counter) = defined_data_types.get_mut(&data_type_name) {
                *counter += 1;
            } else {
                defined_data_types.insert(data_type_name.to_string(), 0);
            }

            let func_var = AirTree::var(
                ValueConstructor::public(
                    tipo,
                    ValueConstructorVariant::ModuleFn {
                        name: data_type_name.to_string(),
                        field_map: None,
                        module: "".to_string(),
                        arity: 1,
                        location: Span::empty(),
                        builtin: None,
                    },
                ),
                data_type_name,
                "",
            );

            AirTree::call(func_var, void(), vec![value])
        }
    }

    pub fn handle_each_clause(
        &mut self,
        clauses: &[TypedClause],
        final_clause: TypedClause,
        subject_tipo: &Arc<Type>,
        props: &mut ClauseProperties,
    ) -> AirTree {
        assert!(
            !subject_tipo.is_void(),
            "WHY ARE YOU PATTERN MATCHING VOID???"
        );
        props.complex_clause = false;

        if let Some((clause, rest_clauses)) = clauses.split_first() {
            let mut clause_then = self.build(&clause.then);

            if let Some(guard) = &clause.guard {
                props.complex_clause = true;
                let clause_guard_name = format!(
                    "__clause_guard_span_{}_{}",
                    clause.location.start, clause.location.end
                );

                let clause_guard_assign = AirTree::let_assignment(
                    &clause_guard_name,
                    builder::handle_clause_guard(guard),
                );

                clause_then = clause_guard_assign.hoist_over(
                    AirTree::clause_guard(clause_guard_name, AirTree::bool(true), bool())
                        .hoist_over(clause_then),
                );
            }

            match &mut props.specific_clause {
                SpecificClause::ConstrClause => {
                    let data_type = build::lookup_data_type_by_tipo(&self.data_types, subject_tipo);

                    let (clause_cond, clause_assign) =
                        self.clause_pattern(&clause.pattern, subject_tipo, props);

                    let clause_assign_hoisted = clause_assign.hoist_over(clause_then);

                    let complex_clause = props.complex_clause;

                    let mut next_clause_props = ClauseProperties::init(
                        subject_tipo,
                        props.clause_var_name.clone(),
                        props.original_subject_name.clone(),
                    );

                    if let Some(data_type) = data_type {
                        if data_type.constructors.len() > 1 {
                            AirTree::clause(
                                &props.original_subject_name,
                                clause_cond,
                                subject_tipo.clone(),
                                clause_assign_hoisted,
                                self.handle_each_clause(
                                    rest_clauses,
                                    final_clause,
                                    subject_tipo,
                                    &mut next_clause_props,
                                ),
                                complex_clause,
                            )
                        } else {
                            AirTree::wrap_clause(
                                clause_assign_hoisted,
                                self.handle_each_clause(
                                    rest_clauses,
                                    final_clause,
                                    subject_tipo,
                                    &mut next_clause_props,
                                ),
                            )
                        }
                    } else {
                        AirTree::clause(
                            &props.original_subject_name,
                            clause_cond,
                            subject_tipo.clone(),
                            clause_assign_hoisted,
                            self.handle_each_clause(
                                rest_clauses,
                                final_clause,
                                subject_tipo,
                                &mut next_clause_props,
                            ),
                            complex_clause,
                        )
                    }
                }
                SpecificClause::ListClause {
                    current_index,
                    defined_tails,
                } => {
                    let Pattern::List { elements, tail, .. } = &clause.pattern
                    else {
                        let mut next_clause_props = ClauseProperties {
                            clause_var_name: props.clause_var_name.clone(),
                            complex_clause: false,
                            needs_constr_var: false,
                            original_subject_name: props.original_subject_name.clone(),
                            final_clause: false,
                            specific_clause: SpecificClause::ListClause {
                                current_index: *current_index,
                                defined_tails: defined_tails.clone(),
                            },
                        };

                        let (_, clause_assign) =
                            self.clause_pattern(&clause.pattern, subject_tipo, props);

                        let clause_assign_hoisted = clause_assign.hoist_over(clause_then);

                        return AirTree::wrap_clause(
                            clause_assign_hoisted,
                            self.handle_each_clause(
                                rest_clauses,
                                final_clause,
                                subject_tipo,
                                &mut next_clause_props,
                            ),
                        );
                    };

                    let tail_name = if *current_index == 0 {
                        props.original_subject_name.clone()
                    } else {
                        format!(
                            "tail_index_{}_span_{}_{}",
                            *current_index,
                            clause.pattern.location().start,
                            clause.pattern.location().end
                        )
                    };

                    let next_tail_name = {
                        if rest_clauses.is_empty() {
                            None
                        } else {
                            let next_clause = &rest_clauses[0];
                            let next_elements_len = match &next_clause.pattern {
                                Pattern::List { elements, .. } => elements.len(),
                                _ => 0,
                            };

                            if (*current_index as usize) < next_elements_len {
                                Some(format!(
                                    "tail_index_{}_span_{}_{}",
                                    *current_index + 1,
                                    next_clause.pattern.location().start,
                                    next_clause.pattern.location().end
                                ))
                            } else {
                                None
                            }
                        }
                    };

                    let mut use_wrap_clause = false;

                    if elements.len() - usize::from(tail.is_some() && !elements.is_empty())
                        >= *current_index as usize
                    {
                        *current_index += 1;
                        defined_tails.push(tail_name.clone());
                    } else if next_tail_name.is_none() {
                        use_wrap_clause = true;
                    }

                    let mut next_clause_props = ClauseProperties {
                        clause_var_name: props.clause_var_name.clone(),
                        complex_clause: false,
                        needs_constr_var: false,
                        original_subject_name: props.original_subject_name.clone(),
                        final_clause: false,
                        specific_clause: SpecificClause::ListClause {
                            current_index: *current_index,
                            defined_tails: defined_tails.clone(),
                        },
                    };

                    let (_, clause_assign) =
                        self.clause_pattern(&clause.pattern, subject_tipo, props);

                    let clause_assign_hoisted = clause_assign.hoist_over(clause_then);

                    let complex_clause = props.complex_clause;

                    if use_wrap_clause {
                        AirTree::wrap_clause(
                            clause_assign_hoisted,
                            self.handle_each_clause(
                                rest_clauses,
                                final_clause,
                                subject_tipo,
                                &mut next_clause_props,
                            ),
                        )
                    } else {
                        AirTree::list_clause(
                            tail_name,
                            subject_tipo.clone(),
                            clause_assign_hoisted,
                            self.handle_each_clause(
                                rest_clauses,
                                final_clause,
                                subject_tipo,
                                &mut next_clause_props,
                            ),
                            next_tail_name,
                            complex_clause,
                        )
                    }
                }
                SpecificClause::TupleClause {
                    defined_tuple_indices,
                } => {
                    let current_defined_indices = defined_tuple_indices.clone();

                    let (_, pattern_assigns) =
                        self.clause_pattern(&clause.pattern, subject_tipo, props);

                    let ClauseProperties{ specific_clause: SpecificClause::TupleClause { defined_tuple_indices }, ..} = props
                    else {
                        unreachable!()
                    };

                    let new_defined_indices: IndexSet<(usize, String)> = defined_tuple_indices
                        .difference(&current_defined_indices)
                        .cloned()
                        .collect();

                    let mut next_clause_props = ClauseProperties {
                        clause_var_name: props.clause_var_name.clone(),
                        complex_clause: false,
                        needs_constr_var: false,
                        original_subject_name: props.original_subject_name.clone(),
                        final_clause: false,
                        specific_clause: SpecificClause::TupleClause {
                            defined_tuple_indices: defined_tuple_indices.clone(),
                        },
                    };

                    if new_defined_indices.is_empty() {
                        AirTree::wrap_clause(
                            pattern_assigns.hoist_over(clause_then),
                            self.handle_each_clause(
                                rest_clauses,
                                final_clause,
                                subject_tipo,
                                &mut next_clause_props,
                            ),
                        )
                    } else {
                        AirTree::tuple_clause(
                            &props.original_subject_name,
                            subject_tipo.clone(),
                            new_defined_indices,
                            current_defined_indices,
                            pattern_assigns.hoist_over(clause_then),
                            self.handle_each_clause(
                                rest_clauses,
                                final_clause,
                                subject_tipo,
                                &mut next_clause_props,
                            ),
                            props.complex_clause,
                        )
                    }
                }
            }
        } else {
            // handle final_clause
            props.final_clause = true;
            assert!(final_clause.guard.is_none());
            let clause_then = self.build(&final_clause.then);
            let (condition, assignments) =
                self.clause_pattern(&final_clause.pattern, subject_tipo, props);

            AirTree::finally(condition, assignments.hoist_over(clause_then))
        }
    }

    pub fn clause_pattern(
        &mut self,
        pattern: &Pattern<PatternConstructor, Arc<Type>>,
        subject_tipo: &Arc<Type>,
        props: &mut ClauseProperties,
        // We return condition and then assignments sequence
    ) -> (AirTree, AirTree) {
        match pattern {
            Pattern::Int { value, .. } => {
                assert!(!props.final_clause);
                (AirTree::int(value), AirTree::no_op())
            }
            Pattern::Var { name, .. } => (
                AirTree::void(),
                AirTree::let_assignment(
                    name,
                    AirTree::local_var(&props.clause_var_name, subject_tipo.clone()),
                ),
            ),
            Pattern::Assign { name, pattern, .. } => {
                let (inner_condition, inner_assignment) =
                    self.clause_pattern(pattern, subject_tipo, props);

                let sequence = vec![
                    AirTree::let_assignment(
                        name,
                        AirTree::local_var(&props.clause_var_name, subject_tipo.clone()),
                    ),
                    inner_assignment,
                ];

                (inner_condition, AirTree::UnhoistedSequence(sequence))
            }
            Pattern::Discard { .. } => (AirTree::void(), AirTree::no_op()),
            Pattern::List { elements, tail, .. } => {
                let ClauseProperties {
                    specific_clause: SpecificClause::ListClause { defined_tails, .. },
                    complex_clause,
                    ..
                } = props
                else { unreachable!() };

                let list_elem_types = subject_tipo.get_inner_types();

                let list_elem_type = list_elem_types
                    .get(0)
                    .unwrap_or_else(|| unreachable!("No list element type?"));

                let defined_tails = defined_tails.clone();

                let elems = elements
                    .iter()
                    .enumerate()
                    .zip(defined_tails.iter())
                    .map(|((index, elem), tail)| {
                        let elem_name = match elem {
                            Pattern::Var { name, .. } => name.to_string(),
                            Pattern::Assign { name, .. } => name.to_string(),
                            Pattern::Discard { .. } => "_".to_string(),
                            _ => format!(
                                "elem_{}_span_{}_{}",
                                index,
                                elem.location().start,
                                elem.location().end
                            ),
                        };

                        let mut elem_props = ClauseProperties::init_inner(
                            list_elem_type,
                            elem_name.clone(),
                            elem_name.clone(),
                            props.final_clause,
                        );

                        let statement =
                            self.nested_clause_condition(elem, list_elem_type, &mut elem_props);

                        *complex_clause = *complex_clause || elem_props.complex_clause;

                        (tail, elem_name, statement)
                    })
                    .collect_vec();

                let defined_tail_heads = elems
                    .iter()
                    .map(|(tail, head, _)| (tail.to_string(), head.to_string()))
                    .collect_vec();

                let mut air_elems = elems
                    .into_iter()
                    .map(|(_, _, statement)| statement)
                    .collect_vec();

                let mut list_tail = None;

                tail.iter().for_each(|elem| {
                    let tail = defined_tails
                        .last()
                        .unwrap_or_else(|| panic!("WHERE IS THE TAIL???"));
                    let elem_name = match elem.as_ref() {
                        Pattern::Var { name, .. } => name.to_string(),
                        Pattern::Assign { name, .. } => name.to_string(),
                        Pattern::Discard { .. } => "_".to_string(),
                        _ => format!(
                            "tail_span_{}_{}",
                            elem.location().start,
                            elem.location().end
                        ),
                    };

                    let mut elem_props = ClauseProperties::init_inner(
                        subject_tipo,
                        elem_name.clone(),
                        elem_name.clone(),
                        props.final_clause,
                    );

                    let statement =
                        self.nested_clause_condition(elem, subject_tipo, &mut elem_props);
                    *complex_clause = *complex_clause || elem_props.complex_clause;

                    air_elems.push(statement);
                    list_tail = Some((tail.to_string(), elem_name));
                });

                let list_assign = AirTree::list_expose(
                    defined_tail_heads,
                    list_tail,
                    subject_tipo.clone(),
                    AirTree::local_var(&props.original_subject_name, subject_tipo.clone()),
                );

                let mut sequence = vec![list_assign];

                sequence.append(&mut air_elems);

                (AirTree::void(), AirTree::UnhoistedSequence(sequence))
            }
            Pattern::Constructor {
                name,
                arguments,
                constructor,
                tipo: function_tipo,
                ..
            } => {
                if subject_tipo.is_bool() {
                    (AirTree::bool(name == "True"), AirTree::no_op())
                } else {
                    assert!(matches!(function_tipo.as_ref().clone(), Type::Fn { .. }));
                    let data_type = build::lookup_data_type_by_tipo(&self.data_types, subject_tipo)
                        .unwrap_or_else(|| {
                            unreachable!(
                                "Code Gen should have the definition for this constructor {}",
                                name
                            )
                        });

                    assert!(!data_type.constructors.is_empty());

                    let (constr_index, _) = data_type
                        .constructors
                        .iter()
                        .enumerate()
                        .find(|(_, dt)| &dt.name == name)
                        .unwrap();

                    let field_map = match constructor {
                        PatternConstructor::Record { field_map, .. } => field_map.clone(),
                    };

                    let mut type_map: IndexMap<usize, Arc<Type>> = IndexMap::new();

                    for (index, arg) in function_tipo.arg_types().unwrap().iter().enumerate() {
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
                                Pattern::Discard { .. } => "_".to_string(),
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

                            let mut field_props = ClauseProperties::init_inner(
                                arg_type,
                                field_name.clone(),
                                field_name.clone(),
                                props.final_clause,
                            );

                            let statement = self.nested_clause_condition(
                                &arg.value,
                                arg_type,
                                &mut field_props,
                            );

                            props.complex_clause =
                                props.complex_clause || field_props.complex_clause;

                            (field_index, field_name, arg_type, statement)
                        })
                        .collect_vec();

                    let indices = fields
                        .iter()
                        .map(|(constr_index, name, tipo, _)| {
                            (*constr_index, name.to_string(), (*tipo).clone())
                        })
                        .collect_vec();

                    let mut air_fields = fields.into_iter().map(|(_, _, _, val)| val).collect_vec();

                    let field_assign = AirTree::fields_expose(
                        indices,
                        false,
                        AirTree::local_var(props.clause_var_name.clone(), subject_tipo.clone()),
                    );

                    let mut sequence = vec![field_assign];

                    sequence.append(&mut air_fields);

                    (
                        AirTree::int(constr_index),
                        AirTree::UnhoistedSequence(sequence),
                    )
                }
            }
            Pattern::Tuple { elems, .. } => {
                let items_type = subject_tipo.get_inner_types();

                let name_index_assigns = elems
                    .iter()
                    .enumerate()
                    .map(|(index, element)| {
                        let elem_name = match element {
                            Pattern::Var { name, .. } => name.to_string(),
                            Pattern::Assign { name, .. } => name.to_string(),
                            Pattern::Discard { .. } => "_".to_string(),
                            _ => format!(
                                "tuple_index_{}_span_{}_{}",
                                index,
                                element.location().start,
                                element.location().end
                            ),
                        };

                        let mut tuple_props = ClauseProperties::init_inner(
                            &items_type[index],
                            elem_name.clone(),
                            elem_name.clone(),
                            props.final_clause,
                        );

                        let elem = self.nested_clause_condition(
                            element,
                            &items_type[index],
                            &mut tuple_props,
                        );

                        (elem_name, index, elem)
                    })
                    .collect_vec();

                let mut defined_indices = match props.clone() {
                    ClauseProperties {
                        specific_clause:
                            SpecificClause::TupleClause {
                                defined_tuple_indices,
                            },
                        ..
                    } => defined_tuple_indices,
                    _ => unreachable!(),
                };

                let mut previous_defined_names = vec![];
                name_index_assigns.iter().for_each(|(name, index, _)| {
                    if let Some((index, prev_name)) = defined_indices
                        .iter()
                        .find(|(defined_index, _)| defined_index == index)
                    {
                        previous_defined_names.push((*index, prev_name.clone(), name.clone()));
                    } else {
                        assert!(defined_indices.insert((*index, name.clone())));
                    }
                });

                let tuple_name_assigns = previous_defined_names
                    .into_iter()
                    .map(|(index, prev_name, name)| {
                        AirTree::let_assignment(
                            name,
                            AirTree::local_var(prev_name, items_type[index].clone()),
                        )
                    })
                    .collect_vec();

                let mut tuple_item_assigns = name_index_assigns
                    .into_iter()
                    .map(|(_, _, item)| item)
                    .collect_vec();

                match props {
                    ClauseProperties {
                        specific_clause:
                            SpecificClause::TupleClause {
                                defined_tuple_indices,
                            },
                        ..
                    } => {
                        *defined_tuple_indices = defined_indices;
                    }
                    _ => unreachable!(),
                }

                let mut sequence = tuple_name_assigns;
                sequence.append(&mut tuple_item_assigns);

                (AirTree::void(), AirTree::UnhoistedSequence(sequence))
            }
        }
    }

    fn nested_clause_condition(
        &mut self,
        pattern: &Pattern<PatternConstructor, Arc<Type>>,
        subject_tipo: &Arc<Type>,
        props: &mut ClauseProperties,
    ) -> AirTree {
        if props.final_clause {
            props.complex_clause = false;
            let (_, assign) = self.clause_pattern(pattern, subject_tipo, props);
            assign
        } else {
            assert!(
                !subject_tipo.is_void(),
                "WHY ARE YOU PATTERN MATCHING ON A NESTED VOID???"
            );
            match pattern {
                Pattern::Int { value, .. } => {
                    props.complex_clause = true;
                    AirTree::clause_guard(&props.original_subject_name, AirTree::int(value), int())
                }
                Pattern::Var { name, .. } => AirTree::let_assignment(
                    name,
                    AirTree::local_var(&props.clause_var_name, subject_tipo.clone()),
                ),
                Pattern::Assign { name, pattern, .. } => AirTree::UnhoistedSequence(vec![
                    AirTree::let_assignment(
                        name,
                        AirTree::local_var(&props.clause_var_name, subject_tipo.clone()),
                    ),
                    self.nested_clause_condition(pattern, subject_tipo, props),
                ]),
                Pattern::Discard { .. } => AirTree::no_op(),
                Pattern::List { elements, tail, .. } => {
                    props.complex_clause = true;
                    // let item_name = format!("__list_item_id_{}", self.id_gen.next());
                    let tail_name_base = "__tail".to_string();

                    if elements.is_empty() {
                        assert!(
                            tail.is_none(),
                            "Why do you have a [..] in a clause? Use a var."
                        );

                        AirTree::list_clause_guard(
                            &props.original_subject_name,
                            subject_tipo.clone(),
                            false,
                            None,
                        )
                    } else {
                        let mut clause_assigns = vec![];

                        let ClauseProperties {
                            specific_clause:
                                SpecificClause::ListClause {
                                    current_index,
                                    defined_tails,
                                },
                            ..
                        } = props
                        else { unreachable!() };

                        for (index, _) in elements.iter().enumerate() {
                            let prev_tail_name = if index == 0 {
                                props.original_subject_name.clone()
                            } else {
                                format!("{}_{}", tail_name_base, index - 1)
                            };

                            // let mut clause_properties = ClauseProperties {
                            //     clause_var_name: item_name.clone(),
                            //     needs_constr_var: false,
                            //     complex_clause: false,
                            //     original_subject_name: item_name.clone(),
                            //     specific_clause: SpecificClause::ListClause {
                            //         current_index: index as i64,
                            //         defined_tails: vec![],
                            //     },
                            //     final_clause,
                            // };

                            let tail_name = format!("{tail_name_base}_{index}");

                            if elements.len() - 1 == index {
                                if tail.is_some() {
                                    clause_assigns.push(AirTree::list_clause_guard(
                                        prev_tail_name,
                                        subject_tipo.clone(),
                                        true,
                                        None,
                                    ));
                                } else {
                                    clause_assigns.push(AirTree::list_clause_guard(
                                        prev_tail_name,
                                        subject_tipo.clone(),
                                        true,
                                        Some(tail_name.to_string()),
                                    ));

                                    clause_assigns.push(AirTree::list_clause_guard(
                                        tail_name.to_string(),
                                        subject_tipo.clone(),
                                        false,
                                        None,
                                    ));
                                    *current_index += 1;
                                    defined_tails.push(tail_name);
                                }
                            } else {
                                clause_assigns.push(AirTree::list_clause_guard(
                                    prev_tail_name,
                                    subject_tipo.clone(),
                                    true,
                                    Some(tail_name.to_string()),
                                ));

                                *current_index += 1;
                                defined_tails.push(tail_name);
                            };
                        }
                        let (_, assigns) = self.clause_pattern(pattern, subject_tipo, props);
                        clause_assigns.push(assigns);
                        AirTree::UnhoistedSequence(clause_assigns)
                    }
                }
                Pattern::Constructor {
                    name: constr_name,
                    is_record,
                    ..
                } => {
                    props.complex_clause = true;
                    if subject_tipo.is_bool() {
                        AirTree::clause_guard(
                            &props.original_subject_name,
                            AirTree::bool(constr_name == "True"),
                            bool(),
                        )
                    } else {
                        let (cond, assign) = self.clause_pattern(pattern, subject_tipo, props);

                        if *is_record {
                            assign
                        } else {
                            AirTree::UnhoistedSequence(vec![
                                AirTree::clause_guard(
                                    &props.original_subject_name,
                                    cond,
                                    subject_tipo.clone(),
                                ),
                                assign,
                            ])
                        }
                    }
                }
                Pattern::Tuple { .. } => {
                    props.complex_clause = true;
                    let (_, assign) = self.clause_pattern(pattern, subject_tipo, props);

                    let defined_indices = match &props.specific_clause {
                        SpecificClause::TupleClause {
                            defined_tuple_indices,
                        } => defined_tuple_indices.clone(),
                        _ => unreachable!(),
                    };

                    let tuple_access = AirTree::tuple_clause_guard(
                        &props.original_subject_name,
                        subject_tipo.clone(),
                        defined_indices,
                    );

                    AirTree::UnhoistedSequence(vec![tuple_access, assign])
                }
            }
        }
    }
}
