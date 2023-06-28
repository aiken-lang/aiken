mod builder;

use indexmap::IndexMap;
use itertools::Itertools;
use uplc::ast::{Name, Program, Term};

use crate::{
    ast::{AssignmentKind, Span, TypedDataType, TypedFunction, TypedValidator},
    expr::TypedExpr,
    gen_uplc::{
        air::Air,
        builder::{self as build, AssignmentProperties, DataTypeKey, FunctionAccessKey},
        tree::AirTree,
        CodeGenFunction,
    },
    tipo::{ModuleValueConstructor, TypeInfo, ValueConstructor, ValueConstructorVariant},
};

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

                assert!(matches!(last_exp, AirTree::Expression(_)));

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
                _ => todo!("IS THIS REACHABLE?"),
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

                builder::assignment_air_tree(
                    pattern,
                    air_value,
                    tipo,
                    AssignmentProperties {
                        value_type: value.tipo(),
                        kind: *kind,
                        remove_unused: true,
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

                    let assignment = builder::assignment_air_tree(
                        &last_clause.pattern,
                        subject_val,
                        tipo,
                        AssignmentProperties {
                            value_type: subject.tipo(),
                            kind: AssignmentKind::Let,
                            remove_unused: false,
                        },
                    );

                    AirTree::hoist_over(assignment, clause_then)
                } else {
                    clauses = if subject.tipo().is_list() {
                        build::rearrange_clauses(clauses.clone())
                    } else {
                        clauses
                    };

                    let last_clause = clauses.pop().unwrap();

                    todo!()
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
}
