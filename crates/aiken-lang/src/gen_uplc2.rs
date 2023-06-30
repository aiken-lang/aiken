pub mod air;
mod builder;
pub mod tree;

use std::sync::Arc;

use indexmap::IndexMap;
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
    builtins::{int, void},
    expr::TypedExpr,
    gen_uplc::{
        air::Air,
        builder::{
            self as build, AssignmentProperties, ClauseProperties, DataTypeKey, FunctionAccessKey,
            SpecificClause,
        },
    },
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
            fun,
            other_fun,
            params,
            ..
        }: &TypedValidator,
    ) -> Program<Name> {
        todo!()
    }

    pub fn generate_test(&mut self, test_body: &TypedExpr) -> Program<Name> {
        let mut air_tree = self.build(test_body);
        air_tree = AirTree::hoist_over(AirTree::no_op(), air_tree);
        println!("{:#?}", air_tree);

        todo!()
    }

    fn finalize(&mut self, term: Term<Name>) -> Program<Name> {
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

                    last_exp = AirTree::hoist_over(exp_tree, last_exp);
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
                _ => unreachable!("IS THIS REACHABLE? Well you reached it with {:#?}", body),
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

                    AirTree::hoist_over(assignment, clause_then)
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
                    let when_assign = AirTree::when(subject_name, subject.tipo(), clauses);

                    AirTree::hoist_over(constr_assign, when_assign)
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
                    AirTree::assert_bool(true, AirTree::hoist_over(assignment, expect))
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
                        AirTree::let_assignment(name, AirTree::hoist_over(assignment, val))
                    } else {
                        let expect =
                            self.expect_type(tipo, val.clone(), &mut index_map, pattern.location());
                        let assign =
                            AirTree::let_assignment("_", AirTree::hoist_over(assignment, expect));
                        AirTree::let_assignment(name, AirTree::hoist_over(assign, val))
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
                        AirTree::let_assignment(name, AirTree::hoist_over(assignment, val))
                    } else {
                        let expect =
                            self.expect_type(tipo, val.clone(), &mut index_map, pattern.location());
                        let assign =
                            AirTree::let_assignment("_", AirTree::hoist_over(assignment, expect));
                        AirTree::let_assignment(name, AirTree::hoist_over(assign, val))
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
                    println!("ELEMS IS {:#?}", elems);
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
                sequence.push(AirTree::tuple_access(indices, tipo.clone(), false, value));

                sequence.append(&mut elems.into_iter().map(|(_, field)| field).collect_vec());

                AirTree::UnhoistedSequence(sequence)
            }
        }
    }

    pub fn expect_type(
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

            let expect_fst = self.expect_type(
                &inner_pair_types[0],
                AirTree::local_var(fst_name, inner_pair_types[0].clone()),
                defined_data_types,
                location,
            );

            let expect_snd = self.expect_type(
                &inner_pair_types[1],
                AirTree::local_var(snd_name, inner_pair_types[1].clone()),
                defined_data_types,
                location,
            );

            let anon_func_body = AirTree::hoist_over(
                AirTree::UnhoistedSequence(vec![
                    tuple_access,
                    AirTree::let_assignment("_", expect_fst),
                    AirTree::let_assignment("_", expect_snd),
                ]),
                AirTree::void(),
            );

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

            AirTree::hoist_over(assign, func_call)
        } else if tipo.is_list() {
            assert!(!tipo.get_inner_types().is_empty());
            let inner_list_type = &tipo.get_inner_types()[0];

            let list_name = format!("__list_span_{}_{}", location.start, location.end);
            let item_name = format!("__item_span_{}_{}", location.start, location.end);

            let assign = AirTree::let_assignment(&list_name, value);

            let expect_item = self.expect_type(
                inner_list_type,
                AirTree::local_var(&item_name, inner_list_type.clone()),
                defined_data_types,
                location,
            );

            let anon_func_body =
                AirTree::hoist_over(AirTree::let_assignment("_", expect_item), AirTree::void());

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

            AirTree::hoist_over(assign, func_call)
        } else {
            todo!()
        }
    }

    pub fn handle_each_clause(
        &mut self,
        clauses: &[TypedClause],
        final_clause: TypedClause,
        subject_tipo: &Arc<Type>,
        props: &mut ClauseProperties,
    ) -> AirTree {
        assert!(!clauses.is_empty());
        props.complex_clause = false;

        if let Some((clause, rest_clauses)) = clauses.split_first() {
            todo!()
        } else {
            // handle final_clause
            props.final_clause = true;
            assert!(final_clause.guard.is_none());
            let clause_then = self.build(&final_clause.then);
            let (_, assignments) = self.clause_pattern(&final_clause.pattern, subject_tipo, props);

            AirTree::hoist_over(assignments, clause_then)
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
                    specific_clause:
                        SpecificClause::ListClause {
                            current_index: _,
                            defined_tails,
                        },
                    clause_var_name: _,
                    complex_clause: _,
                    needs_constr_var: _,
                    original_subject_name: _,
                    final_clause: _,
                } = props
                else { unreachable!()};

                let list_elem_types = subject_tipo.get_inner_types();

                let list_elem_type = list_elem_types
                    .get(0)
                    .unwrap_or_else(|| unreachable!("No list element type?"));

                let elems = elements
                    .iter()
                    .enumerate()
                    .zip(defined_tails.clone())
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

                        let mut elem_props = ClauseProperties::init(
                            list_elem_type,
                            elem_name.clone(),
                            elem_name.clone(),
                        );

                        let statement =
                            self.nested_clause_condition(elem, list_elem_type, &mut elem_props);
                        props.complex_clause = props.complex_clause || elem_props.complex_clause;

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

                tail.iter()
                    .zip(
                        defined_tails
                            .clone()
                            .get(defined_tails.clone().len() - 1)
                            .iter(),
                    )
                    .for_each(|(elem, tail)| {
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

                        let mut elem_props = ClauseProperties {
                            clause_var_name: elem_name.clone(),
                            complex_clause: false,
                            needs_constr_var: false,
                            original_subject_name: elem_name.clone(),
                            final_clause: props.final_clause,
                            specific_clause: props.specific_clause.clone(),
                        };

                        let statement =
                            self.nested_clause_condition(elem, subject_tipo, &mut elem_props);
                        props.complex_clause = props.complex_clause || elem_props.complex_clause;

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
            Pattern::Constructor { .. } => todo!(),
            Pattern::Tuple { .. } => todo!(),
        }
    }

    fn nested_clause_condition(
        &mut self,
        pattern: &Pattern<PatternConstructor, Arc<Type>>,
        subject_tipo: &Arc<Type>,
        props: &mut ClauseProperties,
    ) -> AirTree {
        todo!()
    }
}
