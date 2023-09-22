pub mod air;
pub mod builder;
pub mod tree;

use petgraph::{algo, Graph};
use std::collections::HashMap;
use std::rc::Rc;

use indexmap::{IndexMap, IndexSet};
use itertools::Itertools;
use uplc::{
    ast::{Constant as UplcConstant, Name, NamedDeBruijn, Program, Term, Type as UplcType},
    builder::{CONSTR_FIELDS_EXPOSER, CONSTR_GET_FIELD, CONSTR_INDEX_EXPOSER, EXPECT_ON_LIST},
    builtins::DefaultFunction,
    machine::cost_model::ExBudget,
    optimize::aiken_optimize_and_intern,
    parser::interner::Interner,
};

use crate::{
    ast::{
        AssignmentKind, BinOp, Pattern, Span, TypedArg, TypedClause, TypedDataType, TypedFunction,
        TypedPattern, TypedValidator, UnOp,
    },
    builtins::{bool, data, int, void},
    expr::TypedExpr,
    gen_uplc::builder::{
        check_replaceable_opaque_type, convert_opaque_type, erase_opaque_type_operations,
        find_and_replace_generics, find_list_clause_or_default_first, get_arg_type_name,
        get_generic_id_and_type, get_variant_name, monomorphize, pattern_has_conditions,
        wrap_as_multi_validator, wrap_validator_condition, CodeGenFunction, SpecificClause,
    },
    tipo::{
        ModuleValueConstructor, PatternConstructor, Type, TypeInfo, ValueConstructor,
        ValueConstructorVariant,
    },
    IdGenerator,
};

use self::{
    air::Air,
    builder::{
        cast_validator_args, constants_ir, convert_type_to_data, extract_constant,
        lookup_data_type_by_tipo, modify_cyclic_calls, modify_self_calls, rearrange_list_clauses,
        AssignmentProperties, ClauseProperties, DataTypeKey, FunctionAccessKey, HoistableFunction,
        Variant,
    },
    tree::{AirExpression, AirTree, TreePath},
};

#[derive(Clone)]
pub struct CodeGenerator<'a> {
    defined_functions: IndexMap<FunctionAccessKey, ()>,
    functions: IndexMap<FunctionAccessKey, &'a TypedFunction>,
    data_types: IndexMap<DataTypeKey, &'a TypedDataType>,
    module_types: IndexMap<&'a String, &'a TypeInfo>,
    needs_field_access: bool,
    code_gen_functions: IndexMap<String, CodeGenFunction>,
    zero_arg_functions: IndexMap<(FunctionAccessKey, Variant), Vec<Air>>,
    cyclic_functions:
        IndexMap<(FunctionAccessKey, Variant), (Vec<String>, usize, FunctionAccessKey)>,
    tracing: bool,
    id_gen: IdGenerator,
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
            cyclic_functions: IndexMap::new(),
            tracing,
            id_gen: IdGenerator::new(),
        }
    }

    pub fn reset(&mut self) {
        self.code_gen_functions = IndexMap::new();
        self.zero_arg_functions = IndexMap::new();
        self.needs_field_access = false;
        self.defined_functions = IndexMap::new();
        self.cyclic_functions = IndexMap::new();
        self.id_gen = IdGenerator::new();
    }

    pub fn insert_function(
        &mut self,
        module_name: String,
        function_name: String,
        _variant_name: String,
        value: &'a TypedFunction,
    ) -> Option<&'a TypedFunction> {
        self.functions.insert(
            FunctionAccessKey {
                module_name,
                function_name,
            },
            value,
        )
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
        let mut air_tree_fun = self.build(&fun.body);

        air_tree_fun = wrap_validator_condition(air_tree_fun);

        let mut validator_args_tree = self.check_validator_args(&fun.arguments, true, air_tree_fun);

        validator_args_tree = AirTree::no_op().hoist_over(validator_args_tree);

        let full_tree = self.hoist_functions_to_validator(validator_args_tree);

        // optimizations on air tree

        let full_vec = full_tree.to_vec();

        let mut term = self.uplc_code_gen(full_vec);

        if let Some(other) = other_fun {
            self.reset();

            let mut air_tree_fun_other = self.build(&other.body);

            air_tree_fun_other = wrap_validator_condition(air_tree_fun_other);

            let mut validator_args_tree_other =
                self.check_validator_args(&other.arguments, true, air_tree_fun_other);

            validator_args_tree_other = AirTree::no_op().hoist_over(validator_args_tree_other);

            let full_tree_other = self.hoist_functions_to_validator(validator_args_tree_other);

            // optimizations on air tree

            let full_vec_other = full_tree_other.to_vec();

            let other_term = self.uplc_code_gen(full_vec_other);

            let (spend, mint) = if other.arguments.len() > fun.arguments.len() {
                (other_term, term)
            } else {
                (term, other_term)
            };

            term = wrap_as_multi_validator(spend, mint);

            self.needs_field_access = true;
        }

        term = cast_validator_args(term, params);

        self.finalize(term)
    }

    pub fn generate_test(&mut self, test_body: &TypedExpr) -> Program<Name> {
        let mut air_tree = self.build(test_body);

        air_tree = AirTree::no_op().hoist_over(air_tree);

        let full_tree = self.hoist_functions_to_validator(air_tree);

        // optimizations on air tree
        let full_vec = full_tree.to_vec();

        let term = self.uplc_code_gen(full_vec);

        self.finalize(term)
    }

    fn finalize(&mut self, mut term: Term<Name>) -> Program<Name> {
        if self.needs_field_access {
            term = term
                .constr_get_field()
                .constr_fields_exposer()
                .constr_index_exposer();
        }

        // TODO: Once SOP is implemented, new version is 1.1.0
        let mut program = Program {
            version: (1, 0, 0),
            term,
        };

        program = aiken_optimize_and_intern(program);

        // This is very important to call here.
        // If this isn't done, re-using the same instance
        // of the generator will result in free unique errors
        // among other unpredictable things. In fact,
        // switching to a shared code generator caused some
        // instability issues and we fixed it by placing this
        // method here.
        self.reset();

        program
    }

    fn build(&mut self, body: &TypedExpr) -> AirTree {
        match body {
            TypedExpr::UInt { value, .. } => AirTree::int(value),
            TypedExpr::String { value, .. } => AirTree::string(value),
            TypedExpr::ByteArray { bytes, .. } => AirTree::byte_array(bytes.clone()),
            TypedExpr::Sequence { expressions, .. } | TypedExpr::Pipeline { expressions, .. } => {
                let mut expressions = expressions.clone();

                assert!(
                    !expressions.is_empty(),
                    "Sequence or Pipeline should have at least one expression"
                );

                let mut last_exp = self.build(&expressions.pop().unwrap_or_else(|| unreachable!()));

                while let Some(expression) = expressions.pop() {
                    let exp_tree = self.build(&expression);

                    last_exp = exp_tree.hoist_over(last_exp);
                }
                last_exp
            }

            TypedExpr::Var {
                constructor, name, ..
            } => match &constructor.variant {
                ValueConstructorVariant::ModuleConstant { literal, .. } => constants_ir(literal),
                _ => AirTree::var(constructor.clone(), name, ""),
            },

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
                            tipo: constr_tipo,
                            ..
                        },
                    ..
                }
                | TypedExpr::ModuleSelect {
                    constructor:
                        ModuleValueConstructor::Record {
                            name: constr_name,
                            tipo: constr_tipo,
                            ..
                        },
                    ..
                } => {
                    let data_type = lookup_data_type_by_tipo(&self.data_types, tipo)
                        .expect("Creating a record with no record definition.");

                    let (constr_index, _) = data_type
                        .constructors
                        .iter()
                        .enumerate()
                        .find(|(_, dt)| &dt.name == constr_name)
                        .unwrap();

                    let constr_args = args
                        .iter()
                        .zip(constr_tipo.arg_types().unwrap())
                        .map(|(arg, tipo)| {
                            if tipo.is_data() {
                                AirTree::cast_to_data(self.build(&arg.value), arg.value.tipo())
                            } else {
                                self.build(&arg.value)
                            }
                        })
                        .collect_vec();

                    AirTree::create_constr(constr_index, constr_tipo.clone(), constr_args)
                }

                TypedExpr::Var {
                    constructor:
                        ValueConstructor {
                            variant: ValueConstructorVariant::ModuleFn { builtin, .. },
                            ..
                        },
                    ..
                } => {
                    let fun_arg_types = fun
                        .tipo()
                        .arg_types()
                        .expect("Expected a function type with arguments");

                    assert!(args.len() == fun_arg_types.len());

                    let func_args = args
                        .iter()
                        .zip(fun_arg_types)
                        .map(|(arg, arg_tipo)| {
                            let mut arg_val = self.build(&arg.value);

                            if arg_tipo.is_data() && !arg.value.tipo().is_data() {
                                arg_val = AirTree::cast_to_data(arg_val, arg.value.tipo())
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
                    module_name,
                    constructor: ModuleValueConstructor::Fn { name, .. },
                    ..
                } => {
                    let type_info = self.module_types.get(module_name).unwrap();
                    let value = type_info.values.get(name).unwrap();

                    let ValueConstructorVariant::ModuleFn { builtin, .. } = &value.variant else {
                        unreachable!("Missing module function definition")
                    };

                    let fun_arg_types = fun
                        .tipo()
                        .arg_types()
                        .expect("Expected a function type with arguments");

                    assert!(args.len() == fun_arg_types.len());

                    let func_args = args
                        .iter()
                        .zip(fun_arg_types)
                        .map(|(arg, arg_tipo)| {
                            let mut arg_val = self.build(&arg.value);

                            if arg_tipo.is_data() && !arg.value.tipo().is_data() {
                                arg_val = AirTree::cast_to_data(arg_val, arg.value.tipo())
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
                    let fun_arg_types = fun
                        .tipo()
                        .arg_types()
                        .expect("Expected a function type with arguments");

                    assert!(args.len() == fun_arg_types.len());

                    let func_args = args
                        .iter()
                        .zip(fun_arg_types)
                        .map(|(arg, arg_tipo)| {
                            let mut arg_val = self.build(&arg.value);

                            if arg_tipo.is_data() && !arg.value.tipo().is_data() {
                                arg_val = AirTree::cast_to_data(arg_val, arg.value.tipo())
                            }
                            arg_val
                        })
                        .collect_vec();

                    AirTree::call(self.build(fun.as_ref()), tipo.clone(), func_args)
                }
            },
            TypedExpr::BinOp {
                name,
                left,
                right,
                tipo,
                ..
            } => AirTree::binop(
                *name,
                tipo.clone(),
                self.build(left),
                self.build(right),
                left.tipo(),
            ),

            TypedExpr::Assignment {
                tipo,
                value,
                pattern,
                kind,
                ..
            } => {
                let replaced_type = convert_opaque_type(tipo, &self.data_types);

                let air_value = self.build(value);

                self.assignment(
                    pattern,
                    air_value,
                    &replaced_type,
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

                    let subject_type = subject.tipo();

                    let subject_val = self.build(subject);

                    let assignment = self.assignment(
                        &last_clause.pattern,
                        subject_val,
                        &subject_type,
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
                        rearrange_list_clauses(clauses, &self.data_types)
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
                        tipo.clone(),
                        subject.tipo(),
                        AirTree::local_var(constr_var, subject.tipo()),
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
            } => {
                if check_replaceable_opaque_type(&record.tipo(), &self.data_types) {
                    self.build(record)
                } else {
                    AirTree::record_access(*index, tipo.clone(), self.build(record))
                }
            }

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
                    let data_type = lookup_data_type_by_tipo(&self.data_types, tipo);

                    let val_constructor = ValueConstructor::public(
                        tipo.clone(),
                        ValueConstructorVariant::Record {
                            name: name.clone(),
                            arity: *arity,
                            field_map: field_map.clone(),
                            location: Span::empty(),
                            module: module_name.clone(),
                            constructors_count: data_type
                                .expect("Created a module type without a definition?")
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
                            builtin: Some(builtin),
                            ..
                        } = &value.variant
                        else {
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

            TypedExpr::TupleIndex { index, tuple, .. } => {
                AirTree::tuple_index(*index, tuple.tipo(), self.build(tuple))
            }

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
        pattern: &TypedPattern,
        mut value: AirTree,
        tipo: &Rc<Type>,
        props: AssignmentProperties,
    ) -> AirTree {
        assert!(
            if let AirTree::Expression(AirExpression::Var { name, .. }) = &value {
                name != "_"
            } else {
                true
            }
        );
        if props.value_type.is_data() && props.kind.is_expect() && !tipo.is_data() {
            value = AirTree::cast_from_data(value, tipo.clone());
        } else if !props.value_type.is_data() && tipo.is_data() {
            value = AirTree::cast_to_data(value, props.value_type.clone());
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
                        bool(),
                        AirTree::int(expected_int),
                        AirTree::local_var(name, int()),
                        int(),
                    );
                    AirTree::assert_bool(true, assignment.hoist_over(expect))
                } else {
                    unreachable!("Code Gen should never reach here")
                }
            }
            Pattern::Var { name, .. } => {
                if props.full_check {
                    let mut index_map = IndexMap::new();

                    let non_opaque_tipo = convert_opaque_type(tipo, &self.data_types);

                    let assignment = AirTree::let_assignment(name, value);

                    let val = AirTree::local_var(name, tipo.clone());

                    if non_opaque_tipo.is_primitive() {
                        assignment
                    } else {
                        let expect = self.expect_type_assign(
                            &non_opaque_tipo,
                            val,
                            &mut index_map,
                            pattern.location(),
                        );

                        let assign_expect = AirTree::let_assignment("_", expect);

                        let sequence = vec![assignment, assign_expect];

                        AirTree::UnhoistedSequence(sequence)
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

                    let non_opaque_tipo = convert_opaque_type(tipo, &self.data_types);

                    let assignment = AirTree::let_assignment(name, value);

                    let val = AirTree::local_var(name, tipo.clone());

                    if non_opaque_tipo.is_primitive() {
                        assignment
                    } else {
                        let expect = self.expect_type_assign(
                            &non_opaque_tipo,
                            val,
                            &mut index_map,
                            pattern.location(),
                        );

                        let assign_expect = AirTree::let_assignment("_", expect);

                        let sequence = vec![assignment, assign_expect];

                        AirTree::UnhoistedSequence(sequence)
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
                                    format!("__discard_{}_{}", name, index)
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

                        let assign = if elem_name != "_" {
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
                            )
                        } else {
                            AirTree::no_op()
                        };

                        (elem_name, assign)
                    })
                    .collect_vec();

                // If Some then push tail onto elems
                tail.iter().for_each(|tail| {
                    let tail_name = match tail.as_ref() {
                        Pattern::Var { name, .. } => name.to_string(),
                        Pattern::Assign { name, .. } => name.to_string(),
                        Pattern::Discard { name, .. } => {
                            if props.full_check {
                                format!("__discard_{}_tail", name)
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

                    let assign = if tail_name != "_" {
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
                        )
                    } else {
                        AirTree::no_op()
                    };

                    elems.push((tail_name, assign));
                });

                let names = elems.iter().map(|(name, _)| name.to_string()).collect_vec();

                let list_access = if elements.is_empty() {
                    AirTree::list_empty(value)
                } else {
                    AirTree::list_access(names, tipo.clone(), tail.is_some(), tail.is_none(), value)
                };

                let mut sequence = vec![list_access];

                sequence.append(&mut elems.into_iter().map(|(_, elem)| elem).collect_vec());

                AirTree::UnhoistedSequence(sequence)
            }
            Pattern::Constructor {
                arguments,
                constructor: PatternConstructor::Record { name, field_map },
                tipo: constr_tipo,
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
                        let data_type = lookup_data_type_by_tipo(&self.data_types, tipo)
                            .unwrap_or_else(|| {
                                unreachable!("Failed to find definition for {}", name)
                            });

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

                    let field_map = field_map.clone();

                    let mut type_map: IndexMap<usize, Rc<Type>> = IndexMap::new();

                    for (index, arg) in constr_tipo
                        .arg_types()
                        .expect("Mismatched type")
                        .iter()
                        .enumerate()
                    {
                        let field_type = arg.clone();

                        type_map.insert(index, field_type);
                    }

                    assert!(type_map.len() >= arguments.len());

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
                                        format!("__discard_{}_{}", name, index)
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

                            let assign = if field_name != "_" {
                                self.assignment(
                                    &arg.value,
                                    val,
                                    arg_type,
                                    AssignmentProperties {
                                        value_type: arg_type.clone(),
                                        kind: props.kind,
                                        remove_unused: true,
                                        full_check: props.full_check,
                                    },
                                )
                            } else {
                                AirTree::no_op()
                            };

                            (field_index, field_name, arg_type.clone(), assign)
                        })
                        .collect_vec();

                    let indices = fields
                        .iter()
                        .map(|(index, name, tipo, _)| (*index, name.to_string(), tipo.clone()))
                        .collect_vec();

                    // This `value` is either value param that was passed in or
                    // local var
                    if check_replaceable_opaque_type(tipo, &self.data_types) {
                        sequence.push(AirTree::let_assignment(&indices[0].1, value));
                    } else {
                        sequence.push(AirTree::fields_expose(indices, props.full_check, value));
                    }

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
                let mut type_map: IndexMap<usize, Rc<Type>> = IndexMap::new();

                let mut sequence = vec![];

                for (index, arg) in tipo.get_inner_types().iter().enumerate() {
                    let field_type = arg.clone();
                    type_map.insert(index, field_type);
                }

                assert!(type_map.len() == elems.len());

                let elems = elems
                    .iter()
                    .enumerate()
                    .map(|(index, arg)| {
                        let tuple_name = match &arg {
                            Pattern::Var { name, .. } => name.to_string(),
                            Pattern::Assign { name, .. } => name.to_string(),
                            Pattern::Discard { name, .. } => {
                                if props.full_check {
                                    format!("__discard_{}_{}", name, index)
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

                        let assign = if "_" != tuple_name {
                            self.assignment(
                                arg,
                                val,
                                arg_type,
                                AssignmentProperties {
                                    value_type: arg_type.clone(),
                                    kind: props.kind,
                                    remove_unused: true,
                                    full_check: props.full_check,
                                },
                            )
                        } else {
                            AirTree::no_op()
                        };

                        (tuple_name, assign)
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
        tipo: &Rc<Type>,
        value: AirTree,
        defined_data_types: &mut IndexMap<String, u64>,
        location: Span,
    ) -> AirTree {
        assert!(tipo.get_generic().is_none());
        let tipo = &convert_opaque_type(tipo, &self.data_types);

        if tipo.is_primitive() {
            // Since we would return void anyway and ignore then we can just return value here and ignore
            value
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
            ])
            .hoist_over(expect_snd);

            let unwrap_function = AirTree::anon_func(vec![pair_name], anon_func_body);

            let function = self.code_gen_functions.get(EXPECT_ON_LIST);

            if function.is_none() {
                let expect_list_func = AirTree::expect_on_list();
                self.code_gen_functions.insert(
                    EXPECT_ON_LIST.to_string(),
                    CodeGenFunction::Function {
                        body: expect_list_func,
                        params: vec!["__list_to_check".to_string(), "__check_with".to_string()],
                    },
                );
            }

            if let Some(counter) = defined_data_types.get_mut(EXPECT_ON_LIST) {
                *counter += 1
            } else {
                defined_data_types.insert(EXPECT_ON_LIST.to_string(), 1);
            }

            let func_call = AirTree::call(
                AirTree::var(
                    ValueConstructor::public(
                        void(),
                        ValueConstructorVariant::ModuleFn {
                            name: EXPECT_ON_LIST.to_string(),
                            field_map: None,
                            module: "".to_string(),
                            arity: 1,
                            location,
                            builtin: None,
                        },
                    ),
                    EXPECT_ON_LIST,
                    "",
                ),
                void(),
                vec![AirTree::local_var(map_name, tipo.clone()), unwrap_function],
            );

            assign.hoist_over(func_call)
        } else if tipo.is_list() {
            assert!(!tipo.get_inner_types().is_empty());

            let inner_list_type = &tipo.get_inner_types()[0];

            if inner_list_type.is_data() {
                value
            } else {
                let list_name = format!("__list_span_{}_{}", location.start, location.end);
                let item_name = format!("__item_span_{}_{}", location.start, location.end);

                let assign = AirTree::let_assignment(&list_name, value);

                let expect_item = self.expect_type_assign(
                    inner_list_type,
                    AirTree::cast_from_data(
                        AirTree::local_var(&item_name, data()),
                        inner_list_type.clone(),
                    ),
                    defined_data_types,
                    location,
                );

                let anon_func_body = expect_item;

                let unwrap_function = AirTree::anon_func(vec![item_name], anon_func_body);

                let function = self.code_gen_functions.get(EXPECT_ON_LIST);

                if function.is_none() {
                    let expect_list_func = AirTree::expect_on_list();
                    self.code_gen_functions.insert(
                        EXPECT_ON_LIST.to_string(),
                        CodeGenFunction::Function {
                            body: expect_list_func,
                            params: vec!["__list_to_check".to_string(), "__check_with".to_string()],
                        },
                    );
                }

                if let Some(counter) = defined_data_types.get_mut(EXPECT_ON_LIST) {
                    *counter += 1
                } else {
                    defined_data_types.insert(EXPECT_ON_LIST.to_string(), 1);
                }

                let func_call = AirTree::call(
                    AirTree::var(
                        ValueConstructor::public(
                            void(),
                            ValueConstructorVariant::ModuleFn {
                                name: EXPECT_ON_LIST.to_string(),
                                field_map: None,
                                module: "".to_string(),
                                arity: 1,
                                location,
                                builtin: None,
                            },
                        ),
                        EXPECT_ON_LIST,
                        "",
                    ),
                    void(),
                    vec![AirTree::local_var(list_name, tipo.clone()), unwrap_function],
                );

                assign.hoist_over(func_call)
            }
        } else if tipo.is_2_tuple() {
            let tuple_inner_types = tipo.get_inner_types();

            assert!(tuple_inner_types.len() == 2);

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
            ])
            .hoist_over(expect_snd)
        } else if tipo.is_tuple() {
            let tuple_inner_types = tipo.get_inner_types();

            assert!(!tuple_inner_types.is_empty());

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
                        arg,
                        AirTree::local_var(&tuple_index_name, arg.clone()),
                        defined_data_types,
                        location,
                    );

                    (
                        tuple_index_name,
                        AirTree::let_assignment("_", expect_tuple_item),
                    )
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
            let data_type = lookup_data_type_by_tipo(&self.data_types, tipo).unwrap_or_else(|| {
                unreachable!("We need a data type definition for type {:#?}", tipo)
            });

            let data_type_variant = tipo
                .get_inner_types()
                .iter()
                .map(|arg| get_arg_type_name(arg))
                .join("_");

            assert!(data_type.typed_parameters.len() == tipo.arg_types().unwrap().len());

            let mono_types: IndexMap<u64, Rc<Type>> = if !data_type.typed_parameters.is_empty() {
                data_type
                    .typed_parameters
                    .iter()
                    .zip(tipo.arg_types().unwrap())
                    .flat_map(|item| get_generic_id_and_type(item.0, &item.1))
                    .collect()
            } else {
                IndexMap::new()
            };

            let data_type_name = format!("__expect_{}_{}", data_type.name, data_type_variant);
            let function = self.code_gen_functions.get(&data_type_name);

            let error_term = if self.tracing {
                AirTree::trace(
                    AirTree::string("Constr index did not match any type variant"),
                    tipo.clone(),
                    AirTree::error(tipo.clone()),
                )
            } else {
                AirTree::error(tipo.clone())
            };

            if function.is_none() && defined_data_types.get(&data_type_name).is_none() {
                defined_data_types.insert(data_type_name.clone(), 1);

                let current_defined = defined_data_types.clone();
                let mut diff_defined_types = vec![];

                let constr_clauses = data_type.constructors.iter().enumerate().rfold(
                    error_term,
                    |acc, (index, constr)| {
                        let constr_args = constr
                            .arguments
                            .iter()
                            .enumerate()
                            .map(|(index, arg)| {
                                let arg_name =
                                    arg.label.clone().unwrap_or(format!("__field_{index}"));

                                let arg_tipo = find_and_replace_generics(&arg.tipo, &mono_types);

                                let assign = AirTree::let_assignment(
                                    "_",
                                    self.expect_type_assign(
                                        &arg_tipo,
                                        AirTree::local_var(&arg_name, arg_tipo.clone()),
                                        defined_data_types,
                                        location,
                                    ),
                                );

                                (index, arg_name, arg_tipo, assign)
                            })
                            .collect_vec();

                        let indices = constr_args
                            .iter()
                            .map(|(index, arg_name, arg_tipo, _)| {
                                (*index, arg_name.clone(), (*arg_tipo).clone())
                            })
                            .collect_vec();

                        let mut assigns = constr_args
                            .into_iter()
                            .map(|(_, _, _, assign)| assign)
                            .collect_vec();

                        if assigns.is_empty() {
                            let empty = AirTree::fields_empty(AirTree::local_var(
                                format!("__constr_var_span_{}_{}", location.start, location.end),
                                tipo.clone(),
                            ));

                            assigns.insert(0, empty);
                        } else {
                            let expose = AirTree::fields_expose(
                                indices,
                                true,
                                AirTree::local_var(
                                    format!(
                                        "__constr_var_span_{}_{}",
                                        location.start, location.end
                                    ),
                                    tipo.clone(),
                                ),
                            );

                            assigns.insert(0, expose);
                        };

                        let then = AirTree::UnhoistedSequence(assigns).hoist_over(AirTree::void());

                        AirTree::clause(
                            format!("__subject_span_{}_{}", location.start, location.end),
                            AirTree::int(index),
                            tipo.clone(),
                            then,
                            acc,
                            false,
                        )
                    },
                );

                let overhead_assign = AirTree::let_assignment(
                    format!("__constr_var_span_{}_{}", location.start, location.end),
                    AirTree::local_var("__param_0", tipo.clone()),
                );

                let when_expr = AirTree::when(
                    format!("__subject_span_{}_{}", location.start, location.end),
                    void(),
                    tipo.clone(),
                    AirTree::local_var(
                        format!("__constr_var_span_{}_{}", location.start, location.end),
                        tipo.clone(),
                    ),
                    constr_clauses,
                );

                let func_body = overhead_assign.hoist_over(when_expr);

                for (inner_data_type, inner_count) in defined_data_types.iter() {
                    if let Some(prev_count) = current_defined.get(inner_data_type) {
                        if inner_count - prev_count > 0 {
                            diff_defined_types.push(inner_data_type.to_string());
                        }
                    } else {
                        diff_defined_types.push(inner_data_type.to_string());
                    }
                }

                let code_gen_func = CodeGenFunction::Function {
                    body: func_body,
                    params: vec!["__param_0".to_string()],
                };

                self.code_gen_functions
                    .insert(data_type_name.clone(), code_gen_func);
            } else if let Some(counter) = defined_data_types.get_mut(&data_type_name) {
                *counter += 1;
            } else {
                defined_data_types.insert(data_type_name.to_string(), 1);
            }

            let func_var = AirTree::var(
                ValueConstructor::public(
                    tipo.clone(),
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
        subject_tipo: &Rc<Type>,
        props: &mut ClauseProperties,
    ) -> AirTree {
        assert!(
            !subject_tipo.is_void(),
            "WHY ARE YOU PATTERN MATCHING VOID???"
        );
        props.complex_clause = false;

        if let Some((clause, rest_clauses)) = clauses.split_first() {
            let mut clause_then = self.build(&clause.then);

            // handles clause guard if it exists
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
                    let data_type = lookup_data_type_by_tipo(&self.data_types, subject_tipo);

                    let (clause_cond, clause_assign) =
                        self.clause_pattern(&clause.pattern, subject_tipo, props);

                    let clause_assign_hoisted = clause_assign.hoist_over(clause_then);

                    let complex_clause = props.complex_clause;

                    let mut next_clause_props = ClauseProperties::init(
                        subject_tipo,
                        props.clause_var_name.clone(),
                        props.original_subject_name.clone(),
                    );

                    if matches!(
                        &clause.pattern,
                        Pattern::Var { .. } | Pattern::Discard { .. }
                    ) {
                        AirTree::wrap_clause(
                            clause_assign_hoisted,
                            self.handle_each_clause(
                                rest_clauses,
                                final_clause,
                                subject_tipo,
                                &mut next_clause_props,
                            ),
                        )
                    } else if let Some(data_type) = data_type {
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
                        // Case of ByteArray or Int or Bool matches
                        assert!(subject_tipo.is_primitive());

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
                    defined_tails_index,
                    defined_tails,
                    checked_index,
                } => {
                    let mut clause_pattern = &clause.pattern;

                    if let Pattern::Assign { pattern, .. } = clause_pattern {
                        clause_pattern = pattern;
                    }

                    assert!(matches!(
                        clause_pattern,
                        Pattern::List { .. } | Pattern::Var { .. } | Pattern::Discard { .. }
                    ));

                    let Pattern::List { elements, tail, .. } = clause_pattern else {
                        let mut next_clause_props = ClauseProperties {
                            clause_var_name: props.clause_var_name.clone(),
                            complex_clause: false,
                            needs_constr_var: false,
                            original_subject_name: props.original_subject_name.clone(),
                            final_clause: false,
                            specific_clause: SpecificClause::ListClause {
                                defined_tails_index: *defined_tails_index,
                                defined_tails: defined_tails.clone(),
                                checked_index: *checked_index,
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

                    assert!(!elements.is_empty() || tail.is_none());
                    let elements_len = elements.len() + usize::from(tail.is_none()) - 1;
                    let current_checked_index = *checked_index;

                    let tail_name = defined_tails
                        .get(elements_len)
                        .cloned()
                        .unwrap_or(props.original_subject_name.clone());

                    let next_tail_name = {
                        if rest_clauses.is_empty() {
                            None
                        } else {
                            let next_clause = find_list_clause_or_default_first(rest_clauses);
                            let mut next_clause_pattern = &next_clause.pattern;

                            if let Pattern::Assign { pattern, .. } = next_clause_pattern {
                                next_clause_pattern = pattern;
                            }

                            let next_elements_len = match next_clause_pattern {
                                Pattern::List { elements, tail, .. } => {
                                    assert!(!elements.is_empty() || tail.is_none());
                                    elements.len() + usize::from(tail.is_none()) - 1
                                }
                                _ => 0,
                            };

                            if (*defined_tails_index as usize) < next_elements_len {
                                *defined_tails_index += 1;
                                let current_defined_tail = defined_tails.last().unwrap().clone();

                                defined_tails.push(format!(
                                    "tail_index_{}_span_{}_{}",
                                    *defined_tails_index,
                                    next_clause.pattern.location().start,
                                    next_clause.pattern.location().end
                                ));

                                Some((
                                    current_defined_tail,
                                    format!(
                                        "tail_index_{}_span_{}_{}",
                                        *defined_tails_index,
                                        next_clause.pattern.location().start,
                                        next_clause.pattern.location().end
                                    ),
                                ))
                            } else {
                                None
                            }
                        }
                    };

                    let mut is_wild_card_elems_clause = clause.guard.is_none();
                    for element in elements.iter() {
                        is_wild_card_elems_clause = is_wild_card_elems_clause
                            && !pattern_has_conditions(element, &self.data_types);
                    }

                    if *checked_index < elements_len.try_into().unwrap()
                        && is_wild_card_elems_clause
                    {
                        *checked_index += 1;
                    }

                    let mut next_clause_props = ClauseProperties {
                        clause_var_name: props.clause_var_name.clone(),
                        complex_clause: false,
                        needs_constr_var: false,
                        original_subject_name: props.original_subject_name.clone(),
                        final_clause: false,
                        specific_clause: SpecificClause::ListClause {
                            defined_tails_index: *defined_tails_index,
                            defined_tails: defined_tails.clone(),
                            checked_index: *checked_index,
                        },
                    };

                    let (_, clause_assign) =
                        self.clause_pattern(&clause.pattern, subject_tipo, props);

                    let clause_assign_hoisted = clause_assign.hoist_over(clause_then);

                    let complex_clause = props.complex_clause;

                    if current_checked_index < elements_len.try_into().unwrap()
                        || next_tail_name.is_some()
                    {
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
                }
                SpecificClause::TupleClause {
                    defined_tuple_indices,
                } => {
                    let current_defined_indices = defined_tuple_indices.clone();

                    let (_, pattern_assigns) =
                        self.clause_pattern(&clause.pattern, subject_tipo, props);

                    let ClauseProperties {
                        specific_clause:
                            SpecificClause::TupleClause {
                                defined_tuple_indices,
                            },
                        ..
                    } = props
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
        &self,
        pattern: &Pattern<PatternConstructor, Rc<Type>>,
        subject_tipo: &Rc<Type>,
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
                else {
                    unreachable!()
                };

                let list_elem_types = subject_tipo.get_inner_types();

                let list_elem_type = list_elem_types
                    .get(0)
                    .unwrap_or_else(|| unreachable!("No list element type?"));

                let defined_tails = defined_tails.clone();

                let elems = elements
                    .iter()
                    .enumerate()
                    .map(|(index, elem)| {
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

                        let statement = if elem_name != "_" {
                            self.nested_clause_condition(elem, list_elem_type, &mut elem_props)
                        } else {
                            AirTree::no_op()
                        };

                        *complex_clause = *complex_clause || elem_props.complex_clause;

                        (elem_name, statement)
                    })
                    .collect_vec();

                let mut defined_heads =
                    elems.iter().map(|(head, _)| head.to_string()).collect_vec();

                let mut air_elems = elems
                    .into_iter()
                    .map(|(_, statement)| statement)
                    .collect_vec();

                let mut list_tail = None;

                tail.iter().for_each(|elem| {
                    assert!(!elements.is_empty());
                    let tail = defined_tails.get(elements.len() - 1);
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

                    let statement = if elem_name != "_" {
                        self.nested_clause_condition(elem, subject_tipo, &mut elem_props)
                    } else {
                        AirTree::no_op()
                    };

                    *complex_clause = *complex_clause || elem_props.complex_clause;

                    air_elems.push(statement);
                    if &elem_name != "_" && !defined_tails.is_empty() {
                        list_tail = Some((tail.unwrap().to_string(), elem_name.to_string()));
                    }

                    if props.final_clause && defined_tails.is_empty() {
                        defined_heads.push(elem_name);
                    }
                });

                let list_assign = if props.final_clause && defined_tails.is_empty() {
                    AirTree::list_access(
                        defined_heads,
                        subject_tipo.clone(),
                        tail.is_some(),
                        false,
                        AirTree::local_var(&props.original_subject_name, subject_tipo.clone()),
                    )
                } else {
                    assert!(defined_tails.len() >= defined_heads.len());

                    AirTree::list_expose(
                        defined_heads
                            .into_iter()
                            .zip(defined_tails)
                            .filter(|(head, _)| head != "_")
                            .map(|(head, tail)| (tail, head))
                            .collect_vec(),
                        list_tail,
                        subject_tipo.clone(),
                    )
                };

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
                    assert!(
                        matches!(function_tipo.as_ref().clone(), Type::Fn { .. })
                            || matches!(function_tipo.as_ref().clone(), Type::App { .. })
                    );
                    let data_type = lookup_data_type_by_tipo(&self.data_types, subject_tipo)
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

                    let mut type_map: IndexMap<usize, Rc<Type>> = IndexMap::new();

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

                            let statement = if field_name != "_" {
                                self.nested_clause_condition(&arg.value, arg_type, &mut field_props)
                            } else {
                                AirTree::no_op()
                            };

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

                    let field_assign =
                        if check_replaceable_opaque_type(subject_tipo, &self.data_types) {
                            AirTree::let_assignment(
                                &indices[0].1,
                                AirTree::local_var(
                                    props.clause_var_name.clone(),
                                    subject_tipo.clone(),
                                ),
                            )
                        } else {
                            AirTree::fields_expose(
                                indices,
                                false,
                                AirTree::local_var(
                                    props.clause_var_name.clone(),
                                    subject_tipo.clone(),
                                ),
                            )
                        };

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

                        let elem = if elem_name != "_" {
                            self.nested_clause_condition(
                                element,
                                &items_type[index],
                                &mut tuple_props,
                            )
                        } else {
                            AirTree::no_op()
                        };

                        props.complex_clause = props.complex_clause || tuple_props.complex_clause;

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
                let mut names_to_define = vec![];
                name_index_assigns.iter().for_each(|(name, index, _)| {
                    if let Some((index, prev_name)) = defined_indices
                        .iter()
                        .find(|(defined_index, _nm)| defined_index == index)
                    {
                        previous_defined_names.push((*index, prev_name.clone(), name.clone()));
                    } else if name != "_" {
                        assert!(defined_indices.insert((*index, name.clone())));
                        names_to_define.push((*index, name.clone()));
                    } else {
                        names_to_define.push((*index, name.clone()));
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

                if props.final_clause && !names_to_define.is_empty() {
                    names_to_define.sort_by(|(id1, _), (id2, _)| id1.cmp(id2));

                    let names =
                        names_to_define
                            .into_iter()
                            .fold(vec![], |mut names, (index, name)| {
                                while names.len() < index {
                                    names.push("_".to_string());
                                }
                                names.push(name);
                                names
                            });

                    sequence.insert(
                        0,
                        AirTree::tuple_access(
                            names,
                            subject_tipo.clone(),
                            false,
                            AirTree::local_var(&props.original_subject_name, subject_tipo.clone()),
                        ),
                    );
                }
                (AirTree::void(), AirTree::UnhoistedSequence(sequence))
            }
        }
    }

    fn nested_clause_condition(
        &self,
        pattern: &Pattern<PatternConstructor, Rc<Type>>,
        subject_tipo: &Rc<Type>,
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
                                    defined_tails_index: current_index,
                                    defined_tails,
                                    checked_index: _,
                                },
                            ..
                        } = props
                        else {
                            unreachable!()
                        };

                        defined_tails.push(props.original_subject_name.clone());

                        for (index, _) in elements.iter().enumerate() {
                            let prev_tail_name = if index == 0 {
                                props.original_subject_name.clone()
                            } else {
                                format!("{}_{}", tail_name_base, index - 1)
                            };

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
                    name: constr_name, ..
                } => {
                    if subject_tipo.is_bool() {
                        props.complex_clause = true;
                        AirTree::clause_guard(
                            &props.original_subject_name,
                            AirTree::bool(constr_name == "True"),
                            bool(),
                        )
                    } else {
                        let (cond, assign) = self.clause_pattern(pattern, subject_tipo, props);

                        let data_type = lookup_data_type_by_tipo(&self.data_types, subject_tipo)
                            .expect("Missing data type");

                        if data_type.constructors.len() == 1 {
                            assign
                        } else {
                            props.complex_clause = true;
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

    pub fn check_validator_args(
        &mut self,
        arguments: &[TypedArg],
        has_context: bool,
        body: AirTree,
    ) -> AirTree {
        let checked_args = arguments
            .iter()
            .enumerate()
            .map(|(index, arg)| {
                let arg_name = arg.arg_name.get_variable_name().unwrap_or("_").to_string();
                if !(has_context && index == arguments.len() - 1) && &arg_name != "_" {
                    let param = AirTree::local_var(&arg_name, data());

                    let actual_type = convert_opaque_type(&arg.tipo, &self.data_types);

                    let assign = self.assignment(
                        &Pattern::Var {
                            location: Span::empty(),
                            name: arg_name.to_string(),
                        },
                        param,
                        &actual_type,
                        AssignmentProperties {
                            value_type: data(),
                            kind: AssignmentKind::Expect,
                            remove_unused: false,
                            full_check: true,
                        },
                    );

                    (arg_name, assign)
                } else {
                    (arg_name, AirTree::no_op())
                }
            })
            .collect_vec();

        let arg_names = checked_args
            .iter()
            .map(|(name, _)| name.to_string())
            .collect_vec();

        let arg_assigns =
            AirTree::UnhoistedSequence(checked_args.into_iter().map(|(_, arg)| arg).collect_vec());

        AirTree::anon_func(arg_names, arg_assigns.hoist_over(body))
    }

    fn hoist_functions_to_validator(&mut self, mut air_tree: AirTree) -> AirTree {
        let mut functions_to_hoist = IndexMap::new();
        let mut used_functions = vec![];
        let mut defined_functions = vec![];
        let mut hoisted_functions = vec![];
        let mut validator_hoistable;

        // TODO change subsequent tree traversals to be more like a stream.
        air_tree.traverse_tree_with(
            &mut |air_tree: &mut AirTree, _| {
                erase_opaque_type_operations(air_tree, &self.data_types);
            },
            true,
        );

        self.find_function_vars_and_depth(
            &mut air_tree,
            &mut functions_to_hoist,
            &mut used_functions,
            &mut TreePath::new(),
            0,
            0,
        );

        validator_hoistable = used_functions.clone();

        while let Some((key, variant_name)) = used_functions.pop() {
            defined_functions.push((key.clone(), variant_name.clone()));

            let function_variants = functions_to_hoist
                .get(&key)
                .unwrap_or_else(|| panic!("Missing Function Definition"));

            let (tree_path, function) = function_variants
                .get(&variant_name)
                .unwrap_or_else(|| panic!("Missing Function Variant Definition"));

            match function {
                HoistableFunction::Function { body, deps, params } => {
                    let mut hoist_body = body.clone();
                    let mut hoist_deps = deps.clone();
                    let params = params.clone();
                    let tree_path = tree_path.clone();

                    self.define_dependent_functions(
                        &mut hoist_body,
                        &mut functions_to_hoist,
                        &mut used_functions,
                        &defined_functions,
                        &mut hoist_deps,
                        tree_path,
                    );

                    let function_variants = functions_to_hoist
                        .get_mut(&key)
                        .unwrap_or_else(|| panic!("Missing Function Definition"));

                    let (_, function) = function_variants
                        .get_mut(&variant_name)
                        .expect("Missing Function Variant Definition");

                    if params.is_empty() {
                        validator_hoistable.push((key, variant_name));
                    }

                    *function = HoistableFunction::Function {
                        body: hoist_body,
                        deps: hoist_deps,
                        params,
                    };
                }
                HoistableFunction::Link(_) => todo!("Deal with Link later"),
                _ => unreachable!(),
            }
        }
        validator_hoistable.dedup();

        // First we need to sort functions by dependencies
        // here's also where we deal with mutual recursion

        // Mutual Recursion
        let inputs = functions_to_hoist
            .iter()
            .flat_map(|(function_name, val)| {
                val.into_iter()
                    .map(|(variant, (_, function))| {
                        if let HoistableFunction::Function { deps, .. } = function {
                            ((function_name.clone(), variant.clone()), deps)
                        } else {
                            todo!("Deal with Link later")
                        }
                    })
                    .collect_vec()
            })
            .collect_vec();

        let capacity = inputs.len();

        let mut graph = Graph::<(), ()>::with_capacity(capacity, capacity * 5);

        let mut indices = HashMap::with_capacity(capacity);
        let mut values = HashMap::with_capacity(capacity);

        for (value, _) in &inputs {
            let index = graph.add_node(());

            indices.insert(value.clone(), index);

            values.insert(index, value.clone());
        }

        for (value, deps) in inputs {
            if let Some(from_index) = indices.get(&value) {
                let deps = deps.iter().filter_map(|dep| indices.get(dep));

                for to_index in deps {
                    graph.add_edge(*from_index, *to_index, ());
                }
            }
        }

        let strong_connections = algo::tarjan_scc(&graph);

        for (index, connections) in strong_connections.into_iter().enumerate() {
            // If there's only one function, then it's only self recursive
            if connections.len() < 2 {
                continue;
            }

            let cyclic_function_names = connections
                .iter()
                .map(|index| values.get(index).unwrap())
                .collect_vec();

            // TODO: Maybe I could come up with a name based off the functions involved?
            let function_key = FunctionAccessKey {
                function_name: format!("__cyclic_function_{}", index),
                module_name: "".to_string(),
            };

            let mut path = TreePath::new();
            let mut cycle_of_functions = vec![];
            let mut cycle_deps = vec![];

            let function_list = cyclic_function_names
                .iter()
                .map(|(key, variant)| {
                    format!(
                        "{}{}{}",
                        key.module_name,
                        key.function_name,
                        if variant.is_empty() {
                            "".to_string()
                        } else {
                            format!("_{}", variant)
                        }
                    )
                })
                .collect_vec();

            // By doing this any vars that "call" into a function in the cycle will be
            // redirected to call the cyclic function instead with the proper index
            for (index, (func_name, variant)) in cyclic_function_names.iter().enumerate() {
                self.cyclic_functions.insert(
                    (func_name.clone(), variant.clone()),
                    (function_list.clone(), index, function_key.clone()),
                );

                let (tree_path, func) = functions_to_hoist
                    .get_mut(func_name)
                    .expect("Missing Function Definition")
                    .get_mut(variant)
                    .expect("Missing Function Variant Definition");

                match func {
                    HoistableFunction::Function { params, body, deps } => {
                        cycle_of_functions.push((params.clone(), body.clone()));
                        cycle_deps.push(deps.clone());
                    }

                    _ => unreachable!(),
                }

                if path.is_empty() {
                    path = tree_path.clone();
                } else {
                    path = path.common_ancestor(tree_path);
                }

                // Here we change function to be link so all functions that depend on it know its a
                // cyclic function
                *func = HoistableFunction::CyclicLink(function_key.clone());
            }

            let cyclic_function = HoistableFunction::CyclicFunction {
                functions: cycle_of_functions,
                deps: cycle_deps
                    .into_iter()
                    .flatten()
                    .dedup()
                    // Make sure to filter out cyclic dependencies
                    .filter(|dependency| {
                        !cyclic_function_names.iter().any(|(func_name, variant)| {
                            func_name == &dependency.0 && variant == &dependency.1
                        })
                    })
                    .collect_vec(),
            };

            let mut cyclic_map = IndexMap::new();
            cyclic_map.insert("".to_string(), (path, cyclic_function));

            functions_to_hoist.insert(function_key, cyclic_map);
        }

        // Rest of code is for hoisting functions
        // TODO: replace with graph implementation of sorting
        let mut sorted_function_vec: Vec<(FunctionAccessKey, String)> = vec![];

        let functions_to_hoist_cloned = functions_to_hoist.clone();

        let mut sorting_attempts: u64 = 0;
        while let Some((generic_func, variant)) = validator_hoistable.pop() {
            assert!(
                sorting_attempts < 5_000_000_000,
                "Sorting dependency attempts exceeded"
            );

            let function_variants = functions_to_hoist_cloned
                .get(&generic_func)
                .unwrap_or_else(|| panic!("Missing Function Definition"));

            let (_, function) = function_variants
                .get(&variant)
                .unwrap_or_else(|| panic!("Missing Function Variant Definition"));

            match function {
                HoistableFunction::Function { deps, params, .. } => {
                    if !params.is_empty() {
                        for (dep_generic_func, dep_variant) in deps.iter() {
                            if !(dep_generic_func == &generic_func && dep_variant == &variant) {
                                validator_hoistable
                                    .insert(0, (dep_generic_func.clone(), dep_variant.clone()));

                                sorted_function_vec.retain(|(generic_func, variant)| {
                                    !(generic_func == dep_generic_func && variant == dep_variant)
                                });
                            }
                        }
                    }

                    // Fix dependencies path to be updated to common ancestor
                    for (dep_key, dep_variant) in deps {
                        let (func_tree_path, _) = functions_to_hoist
                            .get(&generic_func)
                            .unwrap()
                            .get(&variant)
                            .unwrap()
                            .clone();

                        let (dep_path, _) = functions_to_hoist
                            .get_mut(dep_key)
                            .unwrap()
                            .get_mut(dep_variant)
                            .unwrap();

                        *dep_path = func_tree_path.common_ancestor(dep_path);
                    }
                }
                HoistableFunction::Link(_) => todo!("Deal with Link later"),
                HoistableFunction::CyclicLink(cyclic_name) => {
                    validator_hoistable.insert(0, (cyclic_name.clone(), "".to_string()));

                    sorted_function_vec.retain(|(generic_func, variant)| {
                        !(generic_func == cyclic_name && variant.is_empty())
                    });

                    let (func_tree_path, _) = functions_to_hoist
                        .get(&generic_func)
                        .unwrap()
                        .get(&variant)
                        .unwrap()
                        .clone();

                    let (dep_path, _) = functions_to_hoist
                        .get_mut(cyclic_name)
                        .unwrap()
                        .get_mut("")
                        .unwrap();

                    *dep_path = func_tree_path.common_ancestor(dep_path);
                }
                HoistableFunction::CyclicFunction { deps, .. } => {
                    for (dep_generic_func, dep_variant) in deps.iter() {
                        if !(dep_generic_func == &generic_func && dep_variant == &variant) {
                            validator_hoistable
                                .insert(0, (dep_generic_func.clone(), dep_variant.clone()));

                            sorted_function_vec.retain(|(generic_func, variant)| {
                                !(generic_func == dep_generic_func && variant == dep_variant)
                            });
                        }
                    }

                    // Fix dependencies path to be updated to common ancestor
                    for (dep_key, dep_variant) in deps {
                        let (func_tree_path, _) = functions_to_hoist
                            .get(&generic_func)
                            .unwrap()
                            .get(&variant)
                            .unwrap()
                            .clone();

                        let (dep_path, _) = functions_to_hoist
                            .get_mut(dep_key)
                            .unwrap()
                            .get_mut(dep_variant)
                            .unwrap();

                        *dep_path = func_tree_path.common_ancestor(dep_path);
                    }
                }
            }

            sorted_function_vec.push((generic_func, variant));
            sorting_attempts += 1;
        }
        sorted_function_vec.dedup();

        // Now we need to hoist the functions to the top of the validator
        for (key, variant) in sorted_function_vec {
            if hoisted_functions
                .iter()
                .any(|(func_key, func_variant)| func_key == &key && func_variant == &variant)
            {
                continue;
            }

            let function_variants = functions_to_hoist
                .get(&key)
                .unwrap_or_else(|| panic!("Missing Function Definition"));

            let (tree_path, function) = function_variants
                .get(&variant)
                .unwrap_or_else(|| panic!("Missing Function Variant Definition"));

            self.hoist_function(
                &mut air_tree,
                tree_path,
                function,
                (&key, &variant),
                &functions_to_hoist,
                &mut hoisted_functions,
            );
        }

        air_tree
    }

    fn hoist_function(
        &mut self,
        air_tree: &mut AirTree,
        tree_path: &TreePath,
        function: &HoistableFunction,
        key_var: (&FunctionAccessKey, &String),
        functions_to_hoist: &IndexMap<
            FunctionAccessKey,
            IndexMap<String, (TreePath, HoistableFunction)>,
        >,
        hoisted_functions: &mut Vec<(FunctionAccessKey, String)>,
    ) {
        match function {
            HoistableFunction::Function {
                body,
                deps: func_deps,
                params,
            } => {
                let mut body = body.clone();

                let (key, variant) = key_var;

                // check for recursiveness
                let is_recursive = func_deps
                    .iter()
                    .any(|(dep_key, dep_variant)| dep_key == key && dep_variant == variant);

                // first grab dependencies
                let func_params = params;

                let params_empty = func_params.is_empty();

                let deps = (tree_path, func_deps.clone());

                if !params_empty {
                    let recursive_nonstatics = if is_recursive {
                        modify_self_calls(&mut body, key, variant, func_params)
                    } else {
                        func_params.clone()
                    };

                    body = AirTree::define_func(
                        &key.function_name,
                        &key.module_name,
                        variant,
                        func_params.clone(),
                        is_recursive,
                        recursive_nonstatics,
                        body,
                    );

                    let function_deps = self.hoist_dependent_functions(
                        deps,
                        params_empty,
                        key,
                        variant,
                        hoisted_functions,
                        functions_to_hoist,
                    );
                    let node_to_edit = air_tree.find_air_tree_node(tree_path);

                    // now hoist full function onto validator tree
                    *node_to_edit = function_deps.hoist_over(body.hoist_over(node_to_edit.clone()));

                    hoisted_functions.push((key.clone(), variant.clone()));
                } else {
                    body = self
                        .hoist_dependent_functions(
                            deps,
                            params_empty,
                            key,
                            variant,
                            hoisted_functions,
                            functions_to_hoist,
                        )
                        .hoist_over(body);

                    self.zero_arg_functions
                        .insert((key.clone(), variant.clone()), body.to_vec());
                }
            }
            HoistableFunction::CyclicFunction {
                functions,
                deps: func_deps,
            } => {
                let (key, variant) = key_var;

                let deps = (tree_path, func_deps.clone());

                let mut functions = functions.clone();

                for (_, body) in functions.iter_mut() {
                    modify_cyclic_calls(body, key, &self.cyclic_functions);
                }

                let cyclic_body = AirTree::define_cyclic_func(
                    &key.function_name,
                    &key.module_name,
                    variant,
                    functions,
                );

                let function_deps = self.hoist_dependent_functions(
                    deps,
                    // cyclic functions always have params
                    false,
                    key,
                    variant,
                    hoisted_functions,
                    functions_to_hoist,
                );
                let node_to_edit = air_tree.find_air_tree_node(tree_path);

                // now hoist full function onto validator tree
                *node_to_edit =
                    function_deps.hoist_over(cyclic_body.hoist_over(node_to_edit.clone()));

                hoisted_functions.push((key.clone(), variant.clone()));
            }
            HoistableFunction::Link(_) => {
                todo!("This should probably be unreachable when I get to it")
            }
            HoistableFunction::CyclicLink(_) => {
                unreachable!("Sorted functions should not contain cyclic links")
            }
        }
    }

    fn hoist_dependent_functions(
        &mut self,
        deps: (&TreePath, Vec<(FunctionAccessKey, String)>),
        params_empty: bool,
        key: &FunctionAccessKey,
        variant: &String,
        hoisted_functions: &mut Vec<(FunctionAccessKey, String)>,
        functions_to_hoist: &IndexMap<
            FunctionAccessKey,
            IndexMap<String, (TreePath, HoistableFunction)>,
        >,
    ) -> AirTree {
        let (func_path, func_deps) = deps;

        let mut deps_vec = func_deps;
        let mut sorted_dep_vec = vec![];

        let mut dep_insertions = vec![];

        while let Some(dep) = deps_vec.pop() {
            let function_variants = functions_to_hoist
                .get(&dep.0)
                .unwrap_or_else(|| panic!("Missing Function Definition"));

            let (_, function) = function_variants
                .get(&dep.1)
                .unwrap_or_else(|| panic!("Missing Function Variant Definition"));

            match function {
                HoistableFunction::Function { deps, params, .. } => {
                    if !params.is_empty() {
                        for (dep_generic_func, dep_variant) in deps.iter() {
                            if !(dep_generic_func == &dep.0 && dep_variant == &dep.1) {
                                sorted_dep_vec.retain(|(generic_func, variant)| {
                                    !(generic_func == dep_generic_func && variant == dep_variant)
                                });

                                deps_vec.insert(0, (dep_generic_func.clone(), dep_variant.clone()));
                            }
                        }
                    }
                }
                HoistableFunction::CyclicFunction { deps, .. } => {
                    for (dep_generic_func, dep_variant) in deps.iter() {
                        if !(dep_generic_func == &dep.0 && dep_variant == &dep.1) {
                            sorted_dep_vec.retain(|(generic_func, variant)| {
                                !(generic_func == dep_generic_func && variant == dep_variant)
                            });

                            deps_vec.insert(0, (dep_generic_func.clone(), dep_variant.clone()));
                        }
                    }
                }
                HoistableFunction::Link(_) => todo!("Deal with Link later"),
                HoistableFunction::CyclicLink(cyclic_func) => {
                    let (_, HoistableFunction::CyclicFunction { deps, .. }) = functions_to_hoist
                        .get(cyclic_func)
                        .unwrap()
                        .get("")
                        .unwrap()
                    else {
                        unreachable!()
                    };

                    for (dep_generic_func, dep_variant) in deps.iter() {
                        if !(dep_generic_func == &dep.0 && dep_variant == &dep.1) {
                            sorted_dep_vec.retain(|(generic_func, variant)| {
                                !(generic_func == dep_generic_func && variant == dep_variant)
                            });

                            deps_vec.insert(0, (dep_generic_func.clone(), dep_variant.clone()));
                        }
                    }
                }
            }

            sorted_dep_vec.push((dep.0.clone(), dep.1.clone()));
        }

        sorted_dep_vec.dedup();
        sorted_dep_vec.reverse();

        // This part handles hoisting dependencies
        while let Some((dep_key, dep_variant)) = sorted_dep_vec.pop() {
            if (!params_empty
            // if the dependency is the same as the function we're hoisting
            // or we hoisted it, then skip it
                && hoisted_functions.iter().any(|(generic, variant)| {
                        generic == &dep_key && variant == &dep_variant
                    }))
                || (&dep_key == key && &dep_variant == variant)
            {
                continue;
            }

            let dependency = functions_to_hoist
                .get(&dep_key)
                .unwrap_or_else(|| panic!("Missing Function Definition"));

            let (dep_path, dep_function) = dependency
                .get(&dep_variant)
                .unwrap_or_else(|| panic!("Missing Function Variant Definition"));

            // In the case of zero args, we need to hoist the dependency function to the top of the zero arg function
            if &dep_path.common_ancestor(func_path) == func_path || params_empty {
                match dep_function.clone() {
                    HoistableFunction::Function {
                        body: mut dep_air_tree,
                        deps: dependency_deps,
                        params: dependent_params,
                    } => {
                        if dependent_params.is_empty() {
                            // continue for zero arg functions. They are treated like global hoists.
                            continue;
                        }

                        let is_dependent_recursive = dependency_deps
                            .iter()
                            .any(|(key, variant)| &dep_key == key && &dep_variant == variant);

                        let recursive_nonstatics = if is_dependent_recursive {
                            modify_self_calls(
                                &mut dep_air_tree,
                                &dep_key,
                                &dep_variant,
                                &dependent_params,
                            )
                        } else {
                            dependent_params.clone()
                        };

                        dep_insertions.push(AirTree::define_func(
                            &dep_key.function_name,
                            &dep_key.module_name,
                            &dep_variant,
                            dependent_params,
                            is_dependent_recursive,
                            recursive_nonstatics,
                            dep_air_tree,
                        ));

                        if !params_empty {
                            hoisted_functions.push((dep_key.clone(), dep_variant.clone()));
                        }
                    }
                    HoistableFunction::CyclicFunction { functions, .. } => {
                        let mut functions = functions.clone();

                        for (_, body) in functions.iter_mut() {
                            modify_cyclic_calls(body, key, &self.cyclic_functions);
                        }

                        dep_insertions.push(AirTree::define_cyclic_func(
                            &dep_key.function_name,
                            &dep_key.module_name,
                            &dep_variant,
                            functions,
                        ));

                        if !params_empty {
                            hoisted_functions.push((dep_key.clone(), dep_variant.clone()));
                        }
                    }
                    HoistableFunction::Link(_) => unreachable!(),
                    HoistableFunction::CyclicLink(_) => unreachable!(),
                }
            }
        }

        dep_insertions.reverse();

        AirTree::UnhoistedSequence(dep_insertions)
    }

    fn define_dependent_functions(
        &mut self,
        air_tree: &mut AirTree,
        function_usage: &mut IndexMap<
            FunctionAccessKey,
            IndexMap<String, (TreePath, HoistableFunction)>,
        >,
        used_functions: &mut Vec<(FunctionAccessKey, String)>,
        defined_functions: &[(FunctionAccessKey, String)],
        current_function_deps: &mut Vec<(FunctionAccessKey, String)>,
        mut function_tree_path: TreePath,
    ) {
        let Some((depth, index)) = function_tree_path.pop() else {
            return;
        };

        function_tree_path.push(depth, index);

        self.find_function_vars_and_depth(
            air_tree,
            function_usage,
            current_function_deps,
            &mut function_tree_path,
            depth + 1,
            0,
        );

        for (generic_function_key, variant_name) in current_function_deps.iter() {
            if !used_functions
                .iter()
                .any(|(key, name)| key == generic_function_key && name == variant_name)
                && !defined_functions
                    .iter()
                    .any(|(key, name)| key == generic_function_key && name == variant_name)
            {
                used_functions.push((generic_function_key.clone(), variant_name.clone()));
            }
        }
    }

    fn find_function_vars_and_depth(
        &mut self,
        air_tree: &mut AirTree,
        function_usage: &mut IndexMap<
            FunctionAccessKey,
            IndexMap<String, (TreePath, HoistableFunction)>,
        >,
        dependency_functions: &mut Vec<(FunctionAccessKey, String)>,
        path: &mut TreePath,
        current_depth: usize,
        depth_index: usize,
    ) {
        air_tree.traverse_tree_with_path(
            path,
            current_depth,
            depth_index,
            &mut |air_tree, tree_path| {
                if let AirTree::Expression(AirExpression::Var {
                    constructor,
                    variant_name,
                    ..
                }) = air_tree
                {
                    let ValueConstructorVariant::ModuleFn {
                        name: func_name,
                        module,
                        builtin: None,
                        ..
                    } = &constructor.variant
                    else {
                        return;
                    };

                    let function_var_tipo = &constructor.tipo;

                    let generic_function_key = FunctionAccessKey {
                        module_name: module.clone(),
                        function_name: func_name.clone(),
                    };

                    let function_def = self.functions.get(&generic_function_key);

                    let Some(function_def) = function_def else {
                        let code_gen_func = self
                            .code_gen_functions
                            .get(&generic_function_key.function_name)
                            .unwrap_or_else(|| panic!("Missing Code Gen Function Definition"));

                        if !dependency_functions
                            .iter()
                            .any(|(key, name)| key == &generic_function_key && name.is_empty())
                        {
                            dependency_functions
                                .push((generic_function_key.clone(), "".to_string()));
                        }

                        if let Some(func_variants) = function_usage.get_mut(&generic_function_key) {
                            let (path, _) = func_variants.get_mut("").unwrap();
                            *path = path.common_ancestor(tree_path);
                        } else {
                            let CodeGenFunction::Function { body, params } = code_gen_func else {
                                unreachable!()
                            };

                            let mut function_variant_path = IndexMap::new();

                            let mut body = body.clone();

                            body.traverse_tree_with(
                                &mut |air_tree, _| {
                                    erase_opaque_type_operations(air_tree, &self.data_types);
                                },
                                true,
                            );

                            function_variant_path.insert(
                                "".to_string(),
                                (
                                    tree_path.clone(),
                                    HoistableFunction::Function {
                                        body,
                                        deps: vec![],
                                        params: params.clone(),
                                    },
                                ),
                            );

                            function_usage.insert(generic_function_key, function_variant_path);
                        }
                        return;
                    };

                    let mut function_var_types = function_var_tipo
                        .arg_types()
                        .unwrap_or_else(|| panic!("Expected a function tipo with arg types"));

                    function_var_types.push(
                        function_var_tipo
                            .return_type()
                            .unwrap_or_else(|| panic!("Should have return type")),
                    );

                    let mut function_def_types = function_def
                        .arguments
                        .iter()
                        .map(|arg| convert_opaque_type(&arg.tipo, &self.data_types))
                        .collect_vec();

                    function_def_types.push(convert_opaque_type(
                        &function_def.return_type,
                        &self.data_types,
                    ));

                    let mono_types: IndexMap<u64, Rc<Type>> = if !function_def_types.is_empty() {
                        function_def_types
                            .into_iter()
                            .zip(function_var_types)
                            .flat_map(|(func_tipo, var_tipo)| {
                                get_generic_id_and_type(&func_tipo, &var_tipo)
                            })
                            .collect()
                    } else {
                        IndexMap::new()
                    };

                    let variant = mono_types
                        .iter()
                        .sorted_by(|(id, _), (id2, _)| id.cmp(id2))
                        .map(|(_, tipo)| get_variant_name(tipo))
                        .join("");

                    *variant_name = variant.clone();

                    if !dependency_functions
                        .iter()
                        .any(|(key, name)| key == &generic_function_key && name == &variant)
                    {
                        dependency_functions.push((generic_function_key.clone(), variant.clone()));
                    }

                    if let Some(func_variants) = function_usage.get_mut(&generic_function_key) {
                        if let Some((path, _)) = func_variants.get_mut(&variant) {
                            *path = path.common_ancestor(tree_path);
                        } else {
                            let params = function_def
                                .arguments
                                .iter()
                                .map(|arg| {
                                    arg.arg_name.get_variable_name().unwrap_or("_").to_string()
                                })
                                .collect_vec();

                            let mut function_air_tree_body = self.build(&function_def.body);

                            function_air_tree_body.traverse_tree_with(
                                &mut |air_tree, _| {
                                    erase_opaque_type_operations(air_tree, &self.data_types);
                                    monomorphize(air_tree, &mono_types);
                                },
                                true,
                            );

                            func_variants.insert(
                                variant,
                                (
                                    tree_path.clone(),
                                    HoistableFunction::Function {
                                        body: function_air_tree_body,
                                        deps: vec![],
                                        params,
                                    },
                                ),
                            );
                        }
                    } else {
                        let params = function_def
                            .arguments
                            .iter()
                            .map(|arg| arg.arg_name.get_variable_name().unwrap_or("_").to_string())
                            .collect_vec();

                        let mut function_air_tree_body = self.build(&function_def.body);

                        function_air_tree_body.traverse_tree_with(
                            &mut |air_tree, _| {
                                erase_opaque_type_operations(air_tree, &self.data_types);
                                monomorphize(air_tree, &mono_types);
                            },
                            true,
                        );

                        let mut function_variant_path = IndexMap::new();

                        function_variant_path.insert(
                            variant,
                            (
                                tree_path.clone(),
                                HoistableFunction::Function {
                                    body: function_air_tree_body,
                                    deps: vec![],
                                    params,
                                },
                            ),
                        );

                        function_usage.insert(generic_function_key, function_variant_path);
                    }
                }
            },
            true,
        );
    }

    fn uplc_code_gen(&mut self, mut ir_stack: Vec<Air>) -> Term<Name> {
        let mut arg_stack: Vec<Term<Name>> = vec![];

        while let Some(ir_element) = ir_stack.pop() {
            self.gen_uplc(ir_element, &mut arg_stack);
        }
        arg_stack.pop().unwrap()
    }

    fn gen_uplc(&mut self, ir: Air, arg_stack: &mut Vec<Term<Name>>) {
        // Going to mark the changes made to code gen after air tree implementation
        match ir {
            Air::Int { value } => {
                arg_stack.push(Term::integer(value.parse().unwrap()));
            }
            Air::String { value } => {
                arg_stack.push(Term::string(value));
            }
            Air::ByteArray { bytes } => {
                arg_stack.push(Term::byte_string(bytes));
            }
            Air::Bool { value } => {
                arg_stack.push(Term::bool(value));
            }
            Air::Var {
                name,
                constructor,
                variant_name,
            } => {
                match &constructor.variant {
                    ValueConstructorVariant::LocalVariable { .. } => arg_stack.push(Term::Var(
                        Name {
                            text: name,
                            unique: 0.into(),
                        }
                        .into(),
                    )),
                    ValueConstructorVariant::ModuleConstant { .. } => {
                        unreachable!("{:#?}, {}", constructor, name)
                    }

                    ValueConstructorVariant::ModuleFn {
                        builtin: Some(builtin),
                        ..
                    } => {
                        let term = match builtin {
                            DefaultFunction::IfThenElse
                            | DefaultFunction::ChooseUnit
                            | DefaultFunction::Trace
                            | DefaultFunction::ChooseList
                            | DefaultFunction::ChooseData
                            | DefaultFunction::UnConstrData => {
                                builder::special_case_builtin(builtin, 0, vec![])
                            }

                            DefaultFunction::FstPair
                            | DefaultFunction::SndPair
                            | DefaultFunction::HeadList => builder::undata_builtin(
                                builtin,
                                0,
                                &constructor.tipo.return_type().unwrap(),
                                vec![],
                            ),

                            DefaultFunction::MkCons | DefaultFunction::MkPairData => {
                                unimplemented!("MkCons and MkPairData should be handled by an anon function or using [] or ( a, b, .., z).\n")
                            }
                            _ => {
                                let mut term: Term<Name> = (*builtin).into();

                                term = builder::apply_builtin_forces(term, builtin.force_count());

                                term
                            }
                        };
                        arg_stack.push(term);
                    }
                    ValueConstructorVariant::ModuleFn {
                        name: func_name,
                        module,
                        ..
                    } => {
                        if let Some((names, index, cyclic_name)) = self.cyclic_functions.get(&(
                            FunctionAccessKey {
                                module_name: module.clone(),
                                function_name: func_name.clone(),
                            },
                            variant_name.clone(),
                        )) {
                            let cyclic_var_name = if cyclic_name.module_name.is_empty() {
                                cyclic_name.function_name.to_string()
                            } else {
                                format!("{}_{}", cyclic_name.module_name, cyclic_name.function_name)
                            };

                            let index_name = names[*index].clone();

                            let mut arg_var = Term::var(index_name.clone());

                            for name in names.iter().rev() {
                                arg_var = arg_var.lambda(name);
                            }

                            let term = Term::var(cyclic_var_name).apply(arg_var);

                            arg_stack.push(term);
                        } else {
                            let name = if (*func_name == name
                                || name == format!("{module}_{func_name}"))
                                && !module.is_empty()
                            {
                                format!("{module}_{func_name}{variant_name}")
                            } else {
                                format!("{func_name}{variant_name}")
                            };

                            arg_stack.push(Term::Var(
                                Name {
                                    text: name,
                                    unique: 0.into(),
                                }
                                .into(),
                            ));
                        }
                    }
                    ValueConstructorVariant::Record {
                        name: constr_name, ..
                    } => {
                        if constructor.tipo.is_bool() {
                            arg_stack.push(Term::bool(constr_name == "True"));
                        } else if constructor.tipo.is_void() {
                            arg_stack.push(Term::Constant(UplcConstant::Unit.into()));
                        } else {
                            let data_type = builder::lookup_data_type_by_tipo(
                                &self.data_types,
                                &constructor.tipo,
                            )
                            .unwrap();

                            let (constr_index, constr_type) = data_type
                                .constructors
                                .iter()
                                .enumerate()
                                .find(|(_, x)| x.name == *constr_name)
                                .unwrap();

                            let mut term = Term::empty_list();

                            if constr_type.arguments.is_empty() {
                                term = Term::constr_data()
                                    .apply(Term::integer(constr_index.try_into().unwrap()))
                                    .apply(term);

                                let mut program: Program<Name> = Program {
                                    version: (1, 0, 0),
                                    term,
                                };

                                let mut interner = Interner::new();

                                interner.program(&mut program);

                                let eval_program: Program<NamedDeBruijn> =
                                    program.try_into().unwrap();

                                let evaluated_term: Term<NamedDeBruijn> =
                                    eval_program.eval(ExBudget::default()).result().unwrap();
                                term = evaluated_term.try_into().unwrap();
                            } else {
                                for (index, arg) in constr_type.arguments.iter().enumerate().rev() {
                                    term = Term::mk_cons()
                                        .apply(convert_type_to_data(
                                            Term::var(
                                                arg.label
                                                    .clone()
                                                    .unwrap_or_else(|| format!("arg_{index}")),
                                            ),
                                            &arg.tipo,
                                        ))
                                        .apply(term);
                                }

                                term = Term::constr_data()
                                    .apply(Term::integer(constr_index.into()))
                                    .apply(term);

                                for (index, arg) in constr_type.arguments.iter().enumerate().rev() {
                                    term = term.lambda(
                                        arg.label.clone().unwrap_or_else(|| format!("arg_{index}")),
                                    )
                                }
                            }
                            arg_stack.push(term);
                        }
                    }
                };
            }
            Air::Void => arg_stack.push(Term::Constant(UplcConstant::Unit.into())),
            Air::List { count, tipo, tail } => {
                let mut args = vec![];

                for _ in 0..count {
                    let arg = arg_stack.pop().unwrap();
                    args.push(arg);
                }
                let mut constants = vec![];
                for arg in &args {
                    let maybe_const = extract_constant(arg);
                    if let Some(c) = maybe_const {
                        constants.push(c);
                    }
                }

                if constants.len() == args.len() && !tail {
                    let list = if tipo.is_map() {
                        let mut convert_keys = vec![];
                        let mut convert_values = vec![];
                        for constant in constants {
                            match constant.as_ref() {
                                UplcConstant::ProtoPair(_, _, fst, snd) => {
                                    convert_keys.push(fst.clone());
                                    convert_values.push(snd.clone());
                                }
                                _ => unreachable!(),
                            }
                        }

                        let convert_keys = builder::convert_constants_to_data(convert_keys);
                        let convert_values = builder::convert_constants_to_data(convert_values);

                        Term::Constant(
                            UplcConstant::ProtoList(
                                UplcType::Pair(UplcType::Data.into(), UplcType::Data.into()),
                                convert_keys
                                    .into_iter()
                                    .zip(convert_values)
                                    .map(|(key, value)| {
                                        UplcConstant::ProtoPair(
                                            UplcType::Data,
                                            UplcType::Data,
                                            key.into(),
                                            value.into(),
                                        )
                                    })
                                    .collect_vec(),
                            )
                            .into(),
                        )
                    } else {
                        Term::Constant(
                            UplcConstant::ProtoList(
                                UplcType::Data,
                                builder::convert_constants_to_data(constants),
                            )
                            .into(),
                        )
                    };

                    arg_stack.push(list);
                } else {
                    let mut term = if tail {
                        args.pop().unwrap()
                    } else if tipo.is_map() {
                        Term::empty_map()
                    } else {
                        Term::empty_list()
                    };

                    // move this down here since the constant list path doesn't need to do this
                    let list_element_type = tipo.get_inner_types()[0].clone();

                    for arg in args.into_iter().rev() {
                        let list_item = if tipo.is_map() {
                            arg
                        } else {
                            builder::convert_type_to_data(arg, &list_element_type)
                        };
                        term = Term::mk_cons().apply(list_item).apply(term);
                    }
                    arg_stack.push(term);
                }
            }
            Air::ListAccessor {
                names,
                tail,
                // TODO: rename tipo -
                // tipo here refers to the list type while the actual return
                // type is nothing since this is an assignment over some expression
                tipo,
                check_last_item,
            } => {
                let value = arg_stack.pop().unwrap();
                let mut term = arg_stack.pop().unwrap();

                let list_id = self.id_gen.next();

                let mut id_list = vec![];
                id_list.push(list_id);

                for _ in 0..names.len() {
                    id_list.push(self.id_gen.next());
                }

                let inner_types = tipo
                    .get_inner_types()
                    .into_iter()
                    .cycle()
                    .take(names.len())
                    .collect_vec();

                if !names.is_empty() {
                    term = builder::list_access_to_uplc(
                        &names,
                        &id_list,
                        tail,
                        0,
                        term,
                        inner_types,
                        check_last_item,
                        true,
                        self.tracing,
                    )
                    .apply(value);

                    arg_stack.push(term);
                } else if check_last_item {
                    let trace_term = if self.tracing {
                        Term::Error.trace(Term::string("Expected no items for List"))
                    } else {
                        Term::Error
                    };

                    term = value.delayed_choose_list(term, trace_term);

                    arg_stack.push(term);
                } else {
                    arg_stack.push(term);
                }
            }
            Air::ListExpose {
                tail_head_names,
                tail,
                // TODO: another case where tipo is not the actual return type,
                // but the list type
                tipo,
            } => {
                let mut term = arg_stack.pop().unwrap();

                if let Some((tail_var, tail_name)) = &tail {
                    term = term
                        .lambda(tail_name)
                        .apply(Term::tail_list().apply(Term::var(tail_var)));
                }

                for (tail_var, head_name) in tail_head_names.iter().rev() {
                    let head_list = if tipo.is_map() {
                        Term::head_list().apply(Term::var(tail_var))
                    } else {
                        builder::convert_data_to_type(
                            Term::head_list().apply(Term::var(tail_var)),
                            &tipo.get_inner_types()[0],
                        )
                    };
                    term = term.lambda(head_name).apply(head_list);
                }

                arg_stack.push(term);
            }
            Air::Fn { params } => {
                let mut term = arg_stack.pop().unwrap();

                for param in params.iter().rev() {
                    term = term.lambda(param);
                }

                if params.is_empty() {
                    arg_stack.push(term.delay())
                } else {
                    arg_stack.push(term);
                }
            }
            Air::Call { count, .. } => {
                if count >= 1 {
                    let mut term = arg_stack.pop().unwrap();

                    for _ in 0..count {
                        let arg = arg_stack.pop().unwrap();

                        term = term.apply(arg);
                    }
                    arg_stack.push(term);
                } else {
                    let term = arg_stack.pop().unwrap();

                    let zero_arg_functions = self.zero_arg_functions.clone();

                    // How we handle zero arg anon functions has changed
                    // We now delay zero arg anon functions and force them on a call operation
                    if let Term::Var(name) = &term {
                        let text = &name.text;

                        if let Some((_, air_vec)) = zero_arg_functions.iter().find(
                            |(
                                (
                                    FunctionAccessKey {
                                        module_name,
                                        function_name,
                                    },
                                    variant,
                                ),
                                _,
                            )| {
                                let name_module = format!("{module_name}_{function_name}{variant}");
                                let name = format!("{function_name}{variant}");

                                text == &name || text == &name_module
                            },
                        ) {
                            let mut term = self.uplc_code_gen(air_vec.clone());

                            term = term
                                .constr_get_field()
                                .constr_fields_exposer()
                                .constr_index_exposer();

                            let mut program: Program<Name> = Program {
                                version: (1, 0, 0),
                                term,
                            };

                            let mut interner = Interner::new();

                            interner.program(&mut program);

                            let eval_program: Program<NamedDeBruijn> = program.try_into().unwrap();

                            let evaluated_term: Term<NamedDeBruijn> =
                                eval_program.eval(ExBudget::max()).result().unwrap();

                            arg_stack.push(evaluated_term.try_into().unwrap());
                        } else {
                            arg_stack.push(term.force())
                        }
                    } else {
                        unreachable!("Shouldn't call anything other than var")
                    }
                }
            }
            Air::Builtin { func, tipo, count } => {
                let mut arg_vec = vec![];
                for _ in 0..count {
                    arg_vec.push(arg_stack.pop().unwrap());
                }

                let tipo = match tipo.as_ref() {
                    Type::Fn { ret, .. } => ret,
                    _ => &tipo,
                };

                let term = match &func {
                    DefaultFunction::IfThenElse
                    | DefaultFunction::ChooseUnit
                    | DefaultFunction::Trace
                    | DefaultFunction::ChooseList
                    | DefaultFunction::ChooseData
                    | DefaultFunction::UnConstrData => {
                        builder::special_case_builtin(&func, count, arg_vec)
                    }

                    DefaultFunction::FstPair
                    | DefaultFunction::SndPair
                    | DefaultFunction::HeadList => {
                        builder::undata_builtin(&func, count, tipo, arg_vec)
                    }

                    DefaultFunction::MkCons | DefaultFunction::MkPairData => {
                        unimplemented!("MkCons and MkPairData should be handled by an anon function or using [] or ( a, b, .., z).\n")
                    }
                    _ => {
                        let mut term: Term<Name> = func.into();

                        term = builder::apply_builtin_forces(term, func.force_count());

                        for arg in arg_vec {
                            term = term.apply(arg.clone());
                        }

                        term
                    }
                };

                arg_stack.push(term);
            }
            Air::BinOp {
                name,
                // changed this to argument tipo
                argument_tipo: tipo,
                ..
            } => {
                let left = arg_stack.pop().unwrap();
                let right = arg_stack.pop().unwrap();

                let builtin = if tipo.is_int() {
                    Term::equals_integer()
                } else if tipo.is_string() {
                    Term::equals_string()
                } else if tipo.is_bytearray() {
                    Term::equals_bytestring()
                } else {
                    Term::equals_data()
                };

                let term =
                    match name {
                        BinOp::And => left.delayed_if_else(right, Term::bool(false)),
                        BinOp::Or => left.delayed_if_else(Term::bool(true), right),
                        BinOp::Eq => {
                            if tipo.is_bool() {
                                let term = left.delayed_if_else(
                                    right.clone(),
                                    right.if_else(Term::bool(false), Term::bool(true)),
                                );

                                arg_stack.push(term);
                                return;
                            } else if tipo.is_map() {
                                let term = builtin
                                    .apply(Term::map_data().apply(left))
                                    .apply(Term::map_data().apply(right));

                                arg_stack.push(term);
                                return;
                            } else if tipo.is_tuple()
                                && matches!(tipo.get_uplc_type(), UplcType::Pair(_, _))
                            {
                                let term = builtin
                                    .apply(Term::map_data().apply(
                                        Term::mk_cons().apply(left).apply(Term::empty_map()),
                                    ))
                                    .apply(Term::map_data().apply(
                                        Term::mk_cons().apply(right).apply(Term::empty_map()),
                                    ));

                                arg_stack.push(term);
                                return;
                            } else if tipo.is_list() || tipo.is_tuple() {
                                let term = builtin
                                    .apply(Term::list_data().apply(left))
                                    .apply(Term::list_data().apply(right));

                                arg_stack.push(term);
                                return;
                            } else if tipo.is_void() {
                                let term = left.choose_unit(right.choose_unit(Term::bool(true)));
                                arg_stack.push(term);
                                return;
                            }

                            builtin.apply(left).apply(right)
                        }
                        BinOp::NotEq => {
                            if tipo.is_bool() {
                                let term = left.delayed_if_else(
                                    right.clone().if_else(Term::bool(false), Term::bool(true)),
                                    right,
                                );

                                arg_stack.push(term);
                                return;
                            } else if tipo.is_map() {
                                let term = builtin
                                    .apply(Term::map_data().apply(left))
                                    .apply(Term::map_data().apply(right))
                                    .if_else(Term::bool(false), Term::bool(true));

                                arg_stack.push(term);
                                return;
                            } else if tipo.is_tuple()
                                && matches!(tipo.get_uplc_type(), UplcType::Pair(_, _))
                            {
                                let term = builtin
                                    .apply(Term::map_data().apply(
                                        Term::mk_cons().apply(left).apply(Term::empty_map()),
                                    ))
                                    .apply(Term::map_data().apply(
                                        Term::mk_cons().apply(right).apply(Term::empty_map()),
                                    ))
                                    .if_else(Term::bool(false), Term::bool(true));

                                arg_stack.push(term);
                                return;
                            } else if tipo.is_list() || tipo.is_tuple() {
                                let term = builtin
                                    .apply(Term::list_data().apply(left))
                                    .apply(Term::list_data().apply(right))
                                    .if_else(Term::bool(false), Term::bool(true));

                                arg_stack.push(term);
                                return;
                            } else if tipo.is_void() {
                                arg_stack.push(Term::bool(false));
                                return;
                            }

                            builtin
                                .apply(left)
                                .apply(right)
                                .if_else(Term::bool(false), Term::bool(true))
                        }
                        BinOp::LtInt => Term::Builtin(DefaultFunction::LessThanInteger)
                            .apply(left)
                            .apply(right),
                        BinOp::LtEqInt => Term::Builtin(DefaultFunction::LessThanEqualsInteger)
                            .apply(left)
                            .apply(right),
                        BinOp::GtEqInt => Term::Builtin(DefaultFunction::LessThanEqualsInteger)
                            .apply(right)
                            .apply(left),
                        BinOp::GtInt => Term::Builtin(DefaultFunction::LessThanInteger)
                            .apply(right)
                            .apply(left),
                        BinOp::AddInt => Term::add_integer().apply(left).apply(right),
                        BinOp::SubInt => Term::Builtin(DefaultFunction::SubtractInteger)
                            .apply(left)
                            .apply(right),
                        BinOp::MultInt => Term::Builtin(DefaultFunction::MultiplyInteger)
                            .apply(left)
                            .apply(right),
                        BinOp::DivInt => Term::Builtin(DefaultFunction::DivideInteger)
                            .apply(left)
                            .apply(right),
                        BinOp::ModInt => Term::Builtin(DefaultFunction::ModInteger)
                            .apply(left)
                            .apply(right),
                    };
                arg_stack.push(term);
            }
            Air::DefineFunc {
                func_name,
                params,
                recursive,
                recursive_nonstatic_params,
                module_name,
                variant_name,
            } => {
                let func_name = if module_name.is_empty() {
                    format!("{func_name}{variant_name}")
                } else {
                    format!("{module_name}_{func_name}{variant_name}")
                };
                let mut func_body = arg_stack.pop().unwrap();

                let mut term = arg_stack.pop().unwrap();

                // Introduce a parameter for each parameter
                // NOTE: we use recursive_nonstatic_params here because
                // if this is recursive, those are the ones that need to be passed
                // each time
                for param in recursive_nonstatic_params.iter().rev() {
                    func_body = func_body.lambda(param.clone());
                }

                if !recursive {
                    term = term.lambda(func_name).apply(func_body);

                    arg_stack.push(term);
                } else {
                    func_body = func_body.lambda(func_name.clone());

                    if recursive_nonstatic_params == params {
                        // If we don't have any recursive-static params, we can just emit the function as is
                        term = term
                            .lambda(func_name.clone())
                            .apply(Term::var(func_name.clone()).apply(Term::var(func_name.clone())))
                            .lambda(func_name)
                            .apply(func_body);
                    } else {
                        // If we have parameters that remain static in each recursive call,
                        // we can construct an *outer* function to take those in
                        // and simplify the recursive part to only accept the non-static arguments
                        let mut recursive_func_body =
                            Term::var(&func_name).apply(Term::var(&func_name));
                        for param in recursive_nonstatic_params.iter() {
                            recursive_func_body = recursive_func_body.apply(Term::var(param));
                        }

                        // Then construct an outer function with *all* parameters, not just the nonstatic ones.
                        let mut outer_func_body =
                            recursive_func_body.lambda(&func_name).apply(func_body);

                        // Now, add *all* parameters, so that other call sites don't know the difference
                        for param in params.iter().rev() {
                            outer_func_body = outer_func_body.lambda(param);
                        }

                        // And finally, fold that definition into the rest of our program
                        term = term.lambda(&func_name).apply(outer_func_body);
                    }

                    arg_stack.push(term);
                }
            }
            Air::DefineCyclicFuncs {
                func_name,
                module_name,
                variant_name,
                contained_functions,
            } => {
                let func_name = if module_name.is_empty() {
                    format!("{func_name}{variant_name}")
                } else {
                    format!("{module_name}_{func_name}{variant_name}")
                };
                let mut cyclic_functions = vec![];

                for params in contained_functions {
                    let func_body = arg_stack.pop().unwrap();

                    cyclic_functions.push((params, func_body));
                }
                let mut term = arg_stack.pop().unwrap();

                let mut cyclic_body = Term::var("__chooser");

                for (params, func_body) in cyclic_functions.into_iter() {
                    let mut function = func_body;
                    for param in params.iter().rev() {
                        function = function.lambda(param);
                    }

                    // We basically Scott encode our function bodies and use the chooser function
                    // to determine which function body and params is run
                    // For example say there is a cycle of 3 function bodies
                    // Our choose function can look like this:
                    // \func1 -> \func2 -> \func3 -> func1
                    // In this case our chooser is a function that takes in 3 functions
                    // and returns the first one to run
                    cyclic_body = cyclic_body.apply(function)
                }

                term = term
                    .lambda(&func_name)
                    .apply(Term::var(&func_name).apply(Term::var(&func_name)))
                    .lambda(&func_name)
                    .apply(cyclic_body.lambda("__chooser").lambda(func_name));

                arg_stack.push(term);
            }
            Air::Let { name } => {
                let arg = arg_stack.pop().unwrap();

                let mut term = arg_stack.pop().unwrap();

                term = term.lambda(name).apply(arg);

                arg_stack.push(term);
            }
            Air::CastFromData { tipo } => {
                let mut term = arg_stack.pop().unwrap();

                if extract_constant(&term).is_some() {
                    term = builder::convert_data_to_type(term, &tipo);

                    let mut program: Program<Name> = Program {
                        version: (1, 0, 0),
                        term,
                    };

                    let mut interner = Interner::new();

                    interner.program(&mut program);

                    let eval_program: Program<NamedDeBruijn> = program.try_into().unwrap();

                    let evaluated_term: Term<NamedDeBruijn> =
                        eval_program.eval(ExBudget::default()).result().unwrap();
                    term = evaluated_term.try_into().unwrap();
                } else {
                    term = builder::convert_data_to_type(term, &tipo);
                }

                arg_stack.push(term);
            }
            Air::CastToData { tipo } => {
                let mut term = arg_stack.pop().unwrap();

                if extract_constant(&term).is_some() {
                    term = builder::convert_type_to_data(term, &tipo);

                    let mut program: Program<Name> = Program {
                        version: (1, 0, 0),
                        term,
                    };

                    let mut interner = Interner::new();

                    interner.program(&mut program);

                    let eval_program: Program<NamedDeBruijn> = program.try_into().unwrap();

                    let evaluated_term: Term<NamedDeBruijn> =
                        eval_program.eval(ExBudget::default()).result().unwrap();
                    term = evaluated_term.try_into().unwrap();
                } else {
                    term = builder::convert_type_to_data(term, &tipo);
                }

                arg_stack.push(term);
            }
            Air::AssertConstr { constr_index } => {
                self.needs_field_access = true;
                let constr = arg_stack.pop().unwrap();

                let mut term = arg_stack.pop().unwrap();

                let trace_term = if self.tracing {
                    Term::Error.trace(Term::string("Expected on incorrect constructor variant."))
                } else {
                    Term::Error
                };

                term = Term::equals_integer()
                    .apply(Term::integer(constr_index.into()))
                    .apply(Term::var(CONSTR_INDEX_EXPOSER).apply(constr))
                    .delayed_if_else(term, trace_term);

                arg_stack.push(term);
            }
            Air::AssertBool { is_true } => {
                let value = arg_stack.pop().unwrap();
                let mut term = arg_stack.pop().unwrap();

                let trace_term = if self.tracing {
                    Term::Error.trace(Term::string("Expected on incorrect boolean variant"))
                } else {
                    Term::Error
                };

                if is_true {
                    term = value.delayed_if_else(term, trace_term)
                } else {
                    term = value.delayed_if_else(trace_term, term)
                }
                arg_stack.push(term);
            }
            Air::When {
                subject_name,
                // using subject type here
                subject_tipo: tipo,
                ..
            } => {
                let subject = arg_stack.pop().unwrap();

                let subject = if tipo.is_int()
                    || tipo.is_bytearray()
                    || tipo.is_string()
                    || tipo.is_list()
                    || tipo.is_tuple()
                    || tipo.is_bool()
                {
                    subject
                } else {
                    self.needs_field_access = true;
                    Term::var(CONSTR_INDEX_EXPOSER).apply(subject)
                };

                let mut term = arg_stack.pop().unwrap();

                term = term.lambda(subject_name).apply(subject);

                arg_stack.push(term);
            }
            Air::Clause {
                subject_tipo: tipo,
                subject_name,
                complex_clause,
            } => {
                // clause to compare
                let clause = arg_stack.pop().unwrap();

                // the body to be run if the clause matches
                let mut body = arg_stack.pop().unwrap();

                // the next branch in the when expression
                let mut term = arg_stack.pop().unwrap();

                if tipo.is_bool() {
                    let other_clauses = if complex_clause {
                        Term::var("__other_clauses_delayed")
                    } else {
                        term.clone().delay()
                    };

                    if matches!(clause, Term::Constant(boolean) if matches!(boolean.as_ref(), UplcConstant::Bool(true)))
                    {
                        body = Term::var(subject_name)
                            .if_else(body.delay(), other_clauses)
                            .force();
                    } else {
                        body = Term::var(subject_name)
                            .if_else(other_clauses, body.delay())
                            .force();
                    }

                    if complex_clause {
                        term = body.lambda("__other_clauses_delayed").apply(term.delay());
                    } else {
                        term = body;
                    }
                } else {
                    let condition = if tipo.is_int() {
                        Term::equals_integer()
                            .apply(clause)
                            .apply(Term::var(subject_name))
                    } else if tipo.is_bytearray() {
                        Term::equals_bytestring()
                            .apply(clause)
                            .apply(Term::var(subject_name))
                    } else if tipo.is_string() {
                        Term::equals_string()
                            .apply(clause)
                            .apply(Term::var(subject_name))
                    } else if tipo.is_list() || tipo.is_tuple() {
                        unreachable!("{:#?}", tipo)
                    } else {
                        Term::equals_integer()
                            .apply(clause)
                            .apply(Term::var(subject_name))
                    };

                    if complex_clause {
                        term = condition
                            .if_else(body.delay(), Term::var("__other_clauses_delayed"))
                            .force()
                            .lambda("__other_clauses_delayed")
                            .apply(term.delay());
                    } else {
                        term = condition.delayed_if_else(body, term);
                    }
                }

                arg_stack.push(term);
            }
            Air::ListClause {
                tail_name,
                next_tail_name,
                complex_clause,
                ..
            } => {
                // no longer need to pop off discard
                let body = arg_stack.pop().unwrap();
                let mut term = arg_stack.pop().unwrap();

                let arg = if let Some((current_tail, next_tail_name)) = next_tail_name {
                    term.lambda(next_tail_name)
                        .apply(Term::tail_list().apply(Term::var(current_tail.clone())))
                } else {
                    term
                };

                if complex_clause {
                    term = Term::var(tail_name)
                        .choose_list(body.delay(), Term::var("__other_clauses_delayed"))
                        .force()
                        .lambda("__other_clauses_delayed")
                        .apply(arg.delay());
                } else {
                    term = Term::var(tail_name).delayed_choose_list(body, arg);
                }

                arg_stack.push(term);
            }
            Air::WrapClause => {
                // no longer need to pop off discard
                let mut term = arg_stack.pop().unwrap();
                let arg = arg_stack.pop().unwrap();

                term = term.lambda("__other_clauses_delayed").apply(arg.delay());

                arg_stack.push(term);
            }
            Air::TupleClause {
                subject_tipo: tipo,
                indices,
                subject_name,
                complex_clause,
                ..
            } => {
                let mut term = arg_stack.pop().unwrap();

                let tuple_types = tipo.get_inner_types();

                let next_clause = arg_stack.pop().unwrap();

                if complex_clause {
                    term = term
                        .lambda("__other_clauses_delayed")
                        .apply(next_clause.delay());
                }

                if tipo.is_2_tuple() {
                    for (index, name) in indices.iter() {
                        if name == "_" {
                            continue;
                        }
                        let builtin = if *index == 0 {
                            Term::fst_pair()
                        } else {
                            Term::snd_pair()
                        };

                        term = term.lambda(name).apply(builder::convert_data_to_type(
                            builtin.apply(Term::var(subject_name.clone())),
                            &tuple_types[*index].clone(),
                        ));
                    }
                } else {
                    for (index, name) in indices.iter() {
                        term = term
                            .lambda(name.clone())
                            .apply(builder::convert_data_to_type(
                                Term::head_list().apply(
                                    Term::var(subject_name.clone()).repeat_tail_list(*index),
                                ),
                                &tuple_types[*index].clone(),
                            ));
                    }
                }
                arg_stack.push(term);
            }
            Air::ClauseGuard {
                subject_name,
                subject_tipo: tipo,
            } => {
                let checker = arg_stack.pop().unwrap();

                let then = arg_stack.pop().unwrap();

                if tipo.is_bool() {
                    let mut term = Term::var("__other_clauses_delayed");
                    if matches!(checker, Term::Constant(boolean) if matches!(boolean.as_ref(), UplcConstant::Bool(true)))
                    {
                        term = Term::var(subject_name).if_else(then.delay(), term).force();
                    } else {
                        term = Term::var(subject_name).if_else(term, then.delay()).force();
                    }
                    arg_stack.push(term);
                } else {
                    let condition = if tipo.is_int() {
                        Term::equals_integer()
                            .apply(checker)
                            .apply(Term::var(subject_name))
                    } else if tipo.is_bytearray() {
                        Term::equals_bytestring()
                            .apply(checker)
                            .apply(Term::var(subject_name))
                    } else if tipo.is_string() {
                        Term::equals_string()
                            .apply(checker)
                            .apply(Term::var(subject_name))
                    } else if tipo.is_list() || tipo.is_tuple() {
                        unreachable!()
                    } else {
                        self.needs_field_access = true;
                        Term::equals_integer()
                            .apply(checker)
                            .apply(Term::var(CONSTR_INDEX_EXPOSER).apply(Term::var(subject_name)))
                    };

                    let term = condition
                        .if_else(then.delay(), Term::var("__other_clauses_delayed"))
                        .force();
                    arg_stack.push(term);
                }
            }
            Air::ListClauseGuard {
                tail_name,
                next_tail_name,
                inverse,
                ..
            } => {
                // no longer need to pop off discard

                // the body to be run if the clause matches
                // the next branch in the when expression
                let mut term = arg_stack.pop().unwrap();

                term = if let Some(next_tail_name) = next_tail_name {
                    term.lambda(next_tail_name)
                        .apply(Term::tail_list().apply(Term::var(tail_name.clone())))
                } else {
                    term
                };

                if !inverse {
                    term = Term::var(tail_name)
                        .choose_list(term.delay(), Term::var("__other_clauses_delayed"))
                        .force();
                } else {
                    term = Term::var(tail_name)
                        .choose_list(Term::var("__other_clauses_delayed"), term.delay())
                        .force();
                }

                arg_stack.push(term);
            }
            Air::TupleGuard {
                subject_tipo: tipo,
                indices,
                subject_name,
            } => {
                let mut term = arg_stack.pop().unwrap();

                let tuple_types = tipo.get_inner_types();

                if tuple_types.len() == 2 {
                    for (index, name) in indices.iter() {
                        if name == "_" {
                            continue;
                        }
                        let builtin = if *index == 0 {
                            Term::fst_pair()
                        } else {
                            Term::snd_pair()
                        };

                        term = term.lambda(name).apply(builder::convert_data_to_type(
                            builtin.apply(Term::var(subject_name.clone())),
                            &tuple_types[*index].clone(),
                        ));
                    }
                } else {
                    for (index, name) in indices.iter() {
                        term = term
                            .lambda(name.clone())
                            .apply(builder::convert_data_to_type(
                                Term::head_list().apply(
                                    Term::var(subject_name.clone()).repeat_tail_list(*index),
                                ),
                                &tuple_types[*index].clone(),
                            ));
                    }
                }
                arg_stack.push(term);
            }
            Air::Finally => {
                let _clause = arg_stack.pop().unwrap();
            }
            Air::If { .. } => {
                let condition = arg_stack.pop().unwrap();
                let then = arg_stack.pop().unwrap();
                let mut term = arg_stack.pop().unwrap();

                term = condition.delayed_if_else(then, term);

                arg_stack.push(term);
            }
            Air::Constr {
                tag: constr_index,
                count,
                tipo,
            } => {
                let mut arg_vec = vec![];
                for _ in 0..count {
                    arg_vec.push(arg_stack.pop().unwrap());
                }

                let mut term = Term::empty_list();

                for (index, arg) in arg_vec.iter().enumerate().rev() {
                    term = Term::mk_cons()
                        .apply(builder::convert_type_to_data(
                            arg.clone(),
                            &tipo.arg_types().unwrap()[index],
                        ))
                        .apply(term);
                }

                term = Term::constr_data()
                    .apply(Term::integer(constr_index.into()))
                    .apply(term);

                if arg_vec.iter().all(|item| {
                    let maybe_const = extract_constant(item);
                    maybe_const.is_some()
                }) {
                    let mut program: Program<Name> = Program {
                        version: (1, 0, 0),
                        term,
                    };

                    let mut interner = Interner::new();

                    interner.program(&mut program);

                    let eval_program: Program<NamedDeBruijn> = program.try_into().unwrap();

                    let evaluated_term: Term<NamedDeBruijn> =
                        eval_program.eval(ExBudget::default()).result().unwrap();
                    term = evaluated_term.try_into().unwrap();
                }

                arg_stack.push(term);
            }
            Air::RecordAccess { record_index, tipo } => {
                self.needs_field_access = true;
                let constr = arg_stack.pop().unwrap();

                let mut term = Term::var(CONSTR_GET_FIELD)
                    .apply(Term::var(CONSTR_FIELDS_EXPOSER).apply(constr))
                    .apply(Term::integer(record_index.into()));

                term = builder::convert_data_to_type(term, &tipo);

                arg_stack.push(term);
            }
            Air::FieldsExpose {
                indices,
                check_last_item,
            } => {
                self.needs_field_access = true;
                let mut id_list = vec![];

                let value = arg_stack.pop().unwrap();
                let mut term = arg_stack.pop().unwrap();
                let list_id = self.id_gen.next();

                id_list.push(list_id);

                for _ in 0..indices.len() {
                    id_list.push(self.id_gen.next());
                }

                let current_index = 0;

                let names = indices.iter().cloned().map(|item| item.1).collect_vec();
                let inner_types = indices.iter().cloned().map(|item| item.2).collect_vec();

                if !indices.is_empty() {
                    term = builder::list_access_to_uplc(
                        &names,
                        &id_list,
                        false,
                        current_index,
                        term,
                        inner_types,
                        check_last_item,
                        false,
                        self.tracing,
                    );

                    term = term.apply(Term::var(CONSTR_FIELDS_EXPOSER).apply(value));

                    arg_stack.push(term);
                } else if check_last_item {
                    let trace_term = if self.tracing {
                        Term::Error.trace(Term::string("Expected no fields for Constr"))
                    } else {
                        Term::Error
                    };

                    term = Term::var(CONSTR_FIELDS_EXPOSER)
                        .apply(value)
                        .delayed_choose_list(term, trace_term);

                    arg_stack.push(term);
                } else {
                    arg_stack.push(term);
                };
            }
            Air::FieldsEmpty => {
                self.needs_field_access = true;

                let value = arg_stack.pop().unwrap();
                let mut term = arg_stack.pop().unwrap();

                let trace_term = if self.tracing {
                    Term::Error.trace(Term::string("Expected no fields for Constr"))
                } else {
                    Term::Error
                };

                term = Term::var(CONSTR_FIELDS_EXPOSER)
                    .apply(value)
                    .delayed_choose_list(term, trace_term);

                arg_stack.push(term);
            }
            Air::ListEmpty => {
                let value = arg_stack.pop().unwrap();
                let mut term = arg_stack.pop().unwrap();

                let trace_term = if self.tracing {
                    Term::Error.trace(Term::string("Expected no items for List"))
                } else {
                    Term::Error
                };

                term = value.delayed_choose_list(term, trace_term);

                arg_stack.push(term);
            }
            Air::Tuple { count, tipo } => {
                let mut args = vec![];

                let tuple_sub_types = tipo.get_inner_types();

                for _ in 0..count {
                    let arg = arg_stack.pop().unwrap();
                    args.push(arg);
                }
                let mut constants = vec![];
                for arg in &args {
                    let maybe_const = extract_constant(arg);
                    if let Some(c) = maybe_const {
                        constants.push(c);
                    }
                }

                if constants.len() == args.len() {
                    let data_constants = builder::convert_constants_to_data(constants);

                    if count == 2 {
                        let term = Term::Constant(
                            UplcConstant::ProtoPair(
                                UplcType::Data,
                                UplcType::Data,
                                data_constants[0].clone().into(),
                                data_constants[1].clone().into(),
                            )
                            .into(),
                        );
                        arg_stack.push(term);
                    } else {
                        let term = Term::Constant(
                            UplcConstant::ProtoList(UplcType::Data, data_constants).into(),
                        );
                        arg_stack.push(term);
                    }
                } else if count == 2 {
                    let term = Term::mk_pair_data()
                        .apply(builder::convert_type_to_data(
                            args[0].clone(),
                            &tuple_sub_types[0],
                        ))
                        .apply(builder::convert_type_to_data(
                            args[1].clone(),
                            &tuple_sub_types[1],
                        ));

                    arg_stack.push(term);
                } else {
                    let mut term = Term::empty_list();
                    for (arg, tipo) in args.into_iter().zip(tuple_sub_types.into_iter()).rev() {
                        term = Term::mk_cons()
                            .apply(builder::convert_type_to_data(arg, &tipo))
                            .apply(term);
                    }
                    arg_stack.push(term);
                }
            }
            Air::RecordUpdate {
                highest_index,
                indices,
                tipo,
            } => {
                self.needs_field_access = true;
                let tail_name_prefix = "__tail_index";

                let data_type = lookup_data_type_by_tipo(&self.data_types, &tipo)
                    .unwrap_or_else(|| panic!("HOW DID YOU DO THIS ON BOOL OR VOID"));

                let constructor_field_count = data_type.constructors[0].arguments.len();
                let record = arg_stack.pop().unwrap();

                let mut args = IndexMap::new();
                let mut unchanged_field_indices = vec![];
                // plus 2 so we get one index higher than the record update index
                // then we add that and any other unchanged fields to an array to later create the
                // lambda bindings
                unchanged_field_indices.push(0);
                let mut prev_index = 0;

                for (index, tipo) in indices
                    .into_iter()
                    .sorted_by(|(index1, _), (index2, _)| index1.cmp(index2))
                {
                    let arg = arg_stack.pop().unwrap();
                    args.insert(index, (tipo.clone(), arg));

                    for field_index in (prev_index + 1)..index {
                        unchanged_field_indices.push(field_index);
                    }
                    prev_index = index;
                }

                unchanged_field_indices.push(prev_index + 1);

                let mut term = Term::var(format!("{tail_name_prefix}_{}", highest_index + 1));

                for current_index in (0..(highest_index + 1)).rev() {
                    let tail_name = format!("{tail_name_prefix}_{current_index}");

                    if let Some((tipo, arg)) = args.get(&current_index) {
                        term = Term::mk_cons()
                            .apply(builder::convert_type_to_data(arg.clone(), tipo))
                            .apply(term);
                    } else {
                        term = Term::mk_cons()
                            .apply(Term::head_list().apply(Term::var(tail_name)))
                            .apply(term);
                    }
                }

                term = Term::constr_data()
                    .apply(Term::integer(0.into()))
                    .apply(term);

                if unchanged_field_indices.len() > 1 {
                    let (prev_index, rest_list) = unchanged_field_indices
                        .split_last()
                        .unwrap_or_else(|| panic!("WHAT HAPPENED"));

                    let mut prev_index = *prev_index;

                    for index in rest_list.iter().rev() {
                        let index = *index;
                        let suffix_tail = format!("{tail_name_prefix}_{prev_index}");
                        let tail = format!("{tail_name_prefix}_{index}");

                        let mut tail_list = Term::var(tail);

                        if index < prev_index {
                            tail_list = tail_list.repeat_tail_list(prev_index - index);

                            if prev_index == constructor_field_count {
                                term = term.lambda(suffix_tail).apply(Term::empty_list());
                            } else {
                                term = term.lambda(suffix_tail).apply(tail_list);
                            }
                        }
                        prev_index = index;
                    }
                }

                term = term
                    .lambda(format!("{tail_name_prefix}_0"))
                    .apply(Term::var(CONSTR_FIELDS_EXPOSER).apply(record));

                arg_stack.push(term);
            }
            Air::UnOp { op } => {
                let value = arg_stack.pop().unwrap();

                let term = match op {
                    UnOp::Not => value.if_else(Term::bool(false), Term::bool(true)),
                    UnOp::Negate => {
                        if let Term::Constant(c) = &value {
                            if let UplcConstant::Integer(i) = c.as_ref() {
                                Term::integer(-i)
                            } else {
                                Term::sub_integer()
                                    .apply(Term::integer(0.into()))
                                    .apply(value)
                            }
                        } else {
                            Term::sub_integer()
                                .apply(Term::integer(0.into()))
                                .apply(value)
                        }
                    }
                };

                arg_stack.push(term);
            }
            Air::TupleIndex { tipo, tuple_index } => {
                let mut term = arg_stack.pop().unwrap();

                if matches!(tipo.get_uplc_type(), UplcType::Pair(_, _)) {
                    if tuple_index == 0 {
                        term = builder::convert_data_to_type(
                            Term::fst_pair().apply(term),
                            &tipo.get_inner_types()[0],
                        );
                    } else {
                        term = builder::convert_data_to_type(
                            Term::snd_pair().apply(term),
                            &tipo.get_inner_types()[1],
                        );
                    }
                } else {
                    self.needs_field_access = true;
                    term = builder::convert_data_to_type(
                        Term::var(CONSTR_GET_FIELD)
                            .apply(term)
                            .apply(Term::integer(tuple_index.into())),
                        &tipo.get_inner_types()[tuple_index],
                    );
                }

                arg_stack.push(term);
            }
            Air::TupleAccessor {
                tipo,
                names,
                check_last_item,
            } => {
                let inner_types = tipo.get_inner_types();
                let value = arg_stack.pop().unwrap();
                let mut term = arg_stack.pop().unwrap();
                let list_id = self.id_gen.next();

                if tipo.is_2_tuple() {
                    assert!(names.len() == 2);

                    if names[1] != "_" {
                        term = term
                            .lambda(names[1].clone())
                            .apply(builder::convert_data_to_type(
                                Term::snd_pair().apply(Term::var(format!("__tuple_{list_id}"))),
                                &inner_types[1],
                            ));
                    }

                    if names[0] != "_" {
                        term = term
                            .lambda(names[0].clone())
                            .apply(builder::convert_data_to_type(
                                Term::fst_pair().apply(Term::var(format!("__tuple_{list_id}"))),
                                &inner_types[0],
                            ))
                    }

                    term = term.lambda(format!("__tuple_{list_id}")).apply(value);

                    arg_stack.push(term);
                } else if !names.is_empty() {
                    let mut id_list = vec![];
                    id_list.push(list_id);

                    for _ in 0..names.len() {
                        id_list.push(self.id_gen.next());
                    }

                    term = builder::list_access_to_uplc(
                        &names,
                        &id_list,
                        false,
                        0,
                        term,
                        tipo.get_inner_types(),
                        check_last_item,
                        false,
                        self.tracing,
                    )
                    .apply(value);

                    arg_stack.push(term);
                } else if check_last_item {
                    unreachable!("HOW DID YOU DO THIS");
                } else {
                    arg_stack.push(term);
                }
            }
            Air::Trace { .. } => {
                let text = arg_stack.pop().unwrap();

                let term = arg_stack.pop().unwrap();

                let term = term.trace(text);

                arg_stack.push(term);
            }
            Air::ErrorTerm { .. } => arg_stack.push(Term::Error),
            Air::NoOp => {}
        }
    }
}
