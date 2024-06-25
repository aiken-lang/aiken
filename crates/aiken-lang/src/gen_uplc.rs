pub mod air;
pub mod builder;
pub mod tree;

use self::{
    air::Air,
    builder::{
        cast_validator_args, constants_ir, convert_type_to_data, extract_constant,
        modify_cyclic_calls, modify_self_calls, rearrange_list_clauses, AssignmentProperties,
        ClauseProperties, CodeGenSpecialFuncs, CycleFunctionNames, HoistableFunction, Variant,
    },
    tree::{AirTree, TreePath},
};
use crate::{
    ast::{
        AssignmentKind, BinOp, Bls12_381Point, Curve, DataTypeKey, FunctionAccessKey, Pattern,
        Span, TraceLevel, Tracing, TypedArg, TypedClause, TypedDataType, TypedFunction,
        TypedPattern, TypedValidator, UnOp,
    },
    builtins::{bool, data, int, list, void},
    expr::TypedExpr,
    gen_uplc::{
        air::ExpectLevel,
        builder::{
            erase_opaque_type_operations, find_list_clause_or_default_first,
            get_generic_variant_name, get_line_columns_by_span, get_src_code_by_span,
            known_data_to_type, monomorphize, pattern_has_conditions, wrap_as_multi_validator,
            wrap_validator_condition, CodeGenFunction, SpecificClause,
        },
    },
    line_numbers::LineNumbers,
    plutus_version::PlutusVersion,
    tipo::{
        check_replaceable_opaque_type, convert_opaque_type, find_and_replace_generics,
        get_arg_type_name, get_generic_id_and_type, lookup_data_type_by_tipo,
        ModuleValueConstructor, PatternConstructor, Type, TypeInfo, ValueConstructor,
        ValueConstructorVariant,
    },
    IdGenerator,
};
use indexmap::{IndexMap, IndexSet};
use itertools::Itertools;
use petgraph::{algo, Graph};
use std::{collections::HashMap, rc::Rc};
use tree::Fields;

use uplc::{
    ast::{Constant as UplcConstant, Name, NamedDeBruijn, Program, Term, Type as UplcType},
    builder::{CONSTR_FIELDS_EXPOSER, CONSTR_INDEX_EXPOSER, EXPECT_ON_LIST},
    builtins::DefaultFunction,
    machine::cost_model::ExBudget,
    optimize::{aiken_optimize_and_intern, interner::CodeGenInterner, shrinker::NO_INLINE},
};

#[derive(Clone)]
pub struct CodeGenerator<'a> {
    #[allow(dead_code)]
    plutus_version: PlutusVersion,
    /// immutable index maps
    functions: IndexMap<&'a FunctionAccessKey, &'a TypedFunction>,
    data_types: IndexMap<&'a DataTypeKey, &'a TypedDataType>,
    module_types: IndexMap<&'a str, &'a TypeInfo>,
    module_src: IndexMap<&'a str, &'a (String, LineNumbers)>,
    /// immutable option
    tracing: TraceLevel,
    /// mutable index maps that are reset
    defined_functions: IndexMap<FunctionAccessKey, ()>,
    special_functions: CodeGenSpecialFuncs,
    code_gen_functions: IndexMap<String, CodeGenFunction>,
    zero_arg_functions: IndexMap<(FunctionAccessKey, Variant), Vec<Air>>,
    cyclic_functions:
        IndexMap<(FunctionAccessKey, Variant), (CycleFunctionNames, usize, FunctionAccessKey)>,
    /// mutable and reset as well
    id_gen: IdGenerator,
}

impl<'a> CodeGenerator<'a> {
    pub fn data_types(&self) -> &IndexMap<&'a DataTypeKey, &'a TypedDataType> {
        &self.data_types
    }

    pub fn new(
        plutus_version: PlutusVersion,
        functions: IndexMap<&'a FunctionAccessKey, &'a TypedFunction>,
        data_types: IndexMap<&'a DataTypeKey, &'a TypedDataType>,
        module_types: IndexMap<&'a str, &'a TypeInfo>,
        module_src: IndexMap<&'a str, &'a (String, LineNumbers)>,
        tracing: Tracing,
    ) -> Self {
        CodeGenerator {
            plutus_version,
            functions,
            data_types,
            module_types,
            module_src,
            tracing: tracing.trace_level(true),
            defined_functions: IndexMap::new(),
            special_functions: CodeGenSpecialFuncs::new(),
            code_gen_functions: IndexMap::new(),
            zero_arg_functions: IndexMap::new(),
            cyclic_functions: IndexMap::new(),
            id_gen: IdGenerator::new(),
        }
    }

    pub fn reset(&mut self, reset_special_functions: bool) {
        self.code_gen_functions = IndexMap::new();
        self.zero_arg_functions = IndexMap::new();
        self.defined_functions = IndexMap::new();
        self.cyclic_functions = IndexMap::new();
        self.id_gen = IdGenerator::new();
        if reset_special_functions {
            self.special_functions = CodeGenSpecialFuncs::new();
        }
    }

    pub fn generate(
        &mut self,
        TypedValidator {
            fun,
            other_fun,
            params,
            ..
        }: &TypedValidator,
        module_name: &str,
    ) -> Program<Name> {
        let mut air_tree_fun = self.build(&fun.body, module_name, &[]);

        air_tree_fun = wrap_validator_condition(air_tree_fun, self.tracing);

        let (src_code, lines) = self.module_src.get(module_name).unwrap();

        let mut validator_args_tree =
            self.check_validator_args(&fun.arguments, true, air_tree_fun, src_code, lines);

        validator_args_tree = AirTree::no_op(validator_args_tree);

        let full_tree = self.hoist_functions_to_validator(validator_args_tree);

        // optimizations on air tree

        let full_vec = full_tree.to_vec();

        let mut term = self.uplc_code_gen(full_vec);

        if let Some(other) = other_fun {
            self.reset(false);

            let mut air_tree_fun_other = self.build(&other.body, module_name, &[]);

            air_tree_fun_other = wrap_validator_condition(air_tree_fun_other, self.tracing);

            let mut validator_args_tree_other = self.check_validator_args(
                &other.arguments,
                true,
                air_tree_fun_other,
                src_code,
                lines,
            );

            validator_args_tree_other = AirTree::no_op(validator_args_tree_other);

            let full_tree_other = self.hoist_functions_to_validator(validator_args_tree_other);

            // optimizations on air tree

            let full_vec_other = full_tree_other.to_vec();

            let other_term = self.uplc_code_gen(full_vec_other);

            let (spend, spend_name, mint, mint_name) =
                if other.arguments.len() > fun.arguments.len() {
                    (other_term, other.name.clone(), term, fun.name.clone())
                } else {
                    (term, fun.name.clone(), other_term, other.name.clone())
                };

            // Special Case with multi_validators
            self.special_functions
                .use_function_uplc(CONSTR_FIELDS_EXPOSER.to_string());

            self.special_functions
                .use_function_uplc(CONSTR_INDEX_EXPOSER.to_string());

            term = wrap_as_multi_validator(spend, mint, self.tracing, spend_name, mint_name);
        }

        term = cast_validator_args(term, params);

        self.finalize(term)
    }

    pub fn generate_raw(
        &mut self,
        body: &TypedExpr,
        args: &[TypedArg],
        module_name: &str,
    ) -> Program<Name> {
        let mut air_tree = self.build(body, module_name, &[]);

        air_tree = AirTree::no_op(air_tree);

        let full_tree = self.hoist_functions_to_validator(air_tree);

        // optimizations on air tree
        let full_vec = full_tree.to_vec();

        let mut term = self.uplc_code_gen(full_vec);

        term = if args.is_empty() {
            term
        } else {
            cast_validator_args(term, args)
        };

        self.finalize(term)
    }

    fn finalize(&mut self, mut term: Term<Name>) -> Program<Name> {
        term = self.special_functions.apply_used_functions(term);

        let version = match self.plutus_version {
            PlutusVersion::V1 | PlutusVersion::V2 => (1, 0, 0),
            PlutusVersion::V3 => (1, 1, 0),
        };

        let mut program = Program { version, term };

        program = aiken_optimize_and_intern(program);

        // This is very important to call here.
        // If this isn't done, re-using the same instance
        // of the generator will result in free unique errors
        // among other unpredictable things. In fact,
        // switching to a shared code generator caused some
        // instability issues and we fixed it by placing this
        // method here.
        self.reset(true);

        program
    }

    fn build(
        &mut self,
        body: &TypedExpr,
        module_build_name: &str,
        context: &[TypedExpr],
    ) -> AirTree {
        if !context.is_empty() {
            let TypedExpr::Assignment {
                location,
                tipo,
                value,
                pattern,
                kind,
            } = body
            else {
                panic!("Dangling expressions without an assignment")
            };

            let air_value = self.build(value, module_build_name, &[]);

            let otherwise_delayed = {
                let msg = match (self.tracing, kind) {
                    (TraceLevel::Silent, _) | (_, AssignmentKind::Let { .. }) => "".to_string(),
                    (TraceLevel::Compact, _) => {
                        get_line_columns_by_span(module_build_name, location, &self.module_src)
                            .to_string()
                    }
                    (TraceLevel::Verbose, _) => {
                        get_src_code_by_span(module_build_name, location, &self.module_src)
                    }
                };

                let msg_func_name = msg.split_whitespace().join("");

                self.special_functions.insert_new_function(
                    msg_func_name.clone(),
                    if msg.is_empty() {
                        Term::Error.delay()
                    } else {
                        Term::Error.delayed_trace(Term::string(msg)).delay()
                    },
                    void(),
                );

                self.special_functions.use_function_tree(msg_func_name)
            };

            let (then, context) = context.split_first().unwrap();

            let then = self.build(then, module_build_name, context);

            self.assignment(
                pattern,
                air_value,
                then,
                tipo,
                AssignmentProperties {
                    value_type: value.tipo(),
                    kind: *kind,
                    remove_unused: kind.is_let(),
                    full_check: !tipo.is_data() && value.tipo().is_data() && kind.is_expect(),
                    otherwise: otherwise_delayed,
                },
            )
        } else {
            match body {
                TypedExpr::Assignment { .. } => {
                    panic!("Reached assignment with no dangling expressions")
                }
                TypedExpr::UInt { value, .. } => AirTree::int(value),
                TypedExpr::String { value, .. } => AirTree::string(value),
                TypedExpr::ByteArray { bytes, .. } => AirTree::byte_array(bytes.clone()),
                TypedExpr::Sequence { expressions, .. }
                | TypedExpr::Pipeline { expressions, .. } => {
                    let (expr, dangling_expressions) = expressions
                        .split_first()
                        .expect("Sequence or Pipeline should have at least one expression");
                    self.build(expr, module_build_name, dangling_expressions)
                }

                TypedExpr::Var {
                    constructor, name, ..
                } => match &constructor.variant {
                    ValueConstructorVariant::ModuleConstant { literal, .. } => {
                        constants_ir(literal)
                    }
                    _ => AirTree::var(constructor.clone(), name, ""),
                },

                TypedExpr::Fn { args, body, .. } => AirTree::anon_func(
                    args.iter()
                        .map(|arg| arg.arg_name.get_variable_name().unwrap_or("_").to_string())
                        .collect_vec(),
                    self.build(body, module_build_name, &[]),
                    false,
                ),

                TypedExpr::List {
                    tipo,
                    elements,
                    tail,
                    ..
                } => AirTree::list(
                    elements
                        .iter()
                        .map(|elem| self.build(elem, module_build_name, &[]))
                        .collect_vec(),
                    tipo.clone(),
                    tail.as_ref()
                        .map(|tail| self.build(tail, module_build_name, &[])),
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
                            .unwrap_or_else(||
                                panic!(
                                    "Creating a record of type {:?} with no record definition. Known definitions: {:?}",
                                    tipo.to_pretty(0),
                                    self.data_types.keys()
                                )
                            );

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
                                    AirTree::cast_to_data(
                                        self.build(&arg.value, module_build_name, &[]),
                                        arg.value.tipo(),
                                    )
                                } else {
                                    self.build(&arg.value, module_build_name, &[])
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
                                let mut arg_val = self.build(&arg.value, module_build_name, &[]);
                                if arg_tipo.is_data() && !arg.value.tipo().is_data() {
                                    arg_val = AirTree::cast_to_data(arg_val, arg.value.tipo())
                                }
                                arg_val
                            })
                            .collect_vec();

                        if let Some(func) = builtin {
                            AirTree::builtin(*func, tipo.clone(), func_args)
                        } else {
                            AirTree::call(
                                self.build(fun.as_ref(), module_build_name, &[]),
                                tipo.clone(),
                                func_args,
                            )
                        }
                    }

                    TypedExpr::ModuleSelect {
                        module_name,
                        constructor: ModuleValueConstructor::Fn { name, .. },
                        ..
                    } => {
                        let type_info = self.module_types.get(module_name.as_str()).unwrap();
                        let value = type_info.values.get(name).unwrap();

                        let ValueConstructorVariant::ModuleFn { builtin, .. } = &value.variant
                        else {
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
                                let mut arg_val = self.build(&arg.value, module_build_name, &[]);

                                if arg_tipo.is_data() && !arg.value.tipo().is_data() {
                                    arg_val = AirTree::cast_to_data(arg_val, arg.value.tipo())
                                }
                                arg_val
                            })
                            .collect_vec();

                        if let Some(func) = builtin {
                            AirTree::builtin(*func, tipo.clone(), func_args)
                        } else {
                            AirTree::call(
                                self.build(fun.as_ref(), module_build_name, &[]),
                                tipo.clone(),
                                func_args,
                            )
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
                                let mut arg_val = self.build(&arg.value, module_build_name, &[]);
                                if arg_tipo.is_data() && !arg.value.tipo().is_data() {
                                    arg_val = AirTree::cast_to_data(arg_val, arg.value.tipo())
                                }
                                arg_val
                            })
                            .collect_vec();

                        AirTree::call(
                            self.build(fun.as_ref(), module_build_name, &[]),
                            tipo.clone(),
                            func_args,
                        )
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
                    self.build(left, module_build_name, &[]),
                    self.build(right, module_build_name, &[]),
                    left.tipo(),
                ),

                TypedExpr::Trace {
                    tipo, then, text, ..
                } => AirTree::trace(
                    self.build(text, module_build_name, &[]),
                    tipo.clone(),
                    self.build(then, module_build_name, &[]),
                ),

                TypedExpr::When {
                    tipo,
                    subject,
                    clauses,
                    ..
                } => {
                    let mut clauses = clauses.clone();

                    if clauses.is_empty() {
                        unreachable!("We should have one clause at least")
                    // TODO: This whole branch can _probably_ be removed, if handle_each_clause
                    // works fine with an empty clauses list. This is orthogonal to the
                    // current refactoring so not changing it now.
                    } else if clauses.len() == 1 {
                        let last_clause = clauses.pop().unwrap();

                        let clause_then = self.build(&last_clause.then, module_build_name, &[]);

                        let subject_type = subject.tipo();

                        let subject_val = self.build(subject, module_build_name, &[]);

                        self.assignment(
                            &last_clause.pattern,
                            subject_val,
                            clause_then,
                            &subject_type,
                            AssignmentProperties {
                                value_type: subject.tipo(),
                                kind: AssignmentKind::let_(),
                                remove_unused: false,
                                full_check: false,
                                otherwise: AirTree::error(void(), false),
                            },
                        )
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
                            module_build_name,
                        );

                        let when_assign = AirTree::when(
                            subject_name,
                            tipo.clone(),
                            subject.tipo(),
                            AirTree::local_var(&constr_var, subject.tipo()),
                            clauses,
                        );

                        AirTree::let_assignment(
                            constr_var,
                            self.build(subject, module_build_name, &[]),
                            when_assign,
                        )
                    }
                }
                // let pattern = branch.condition
                // branch.body
                //
                // if <expr:condition> is <pattern>: <annotation> { <expr:body> }
                // [(builtin ifThenElse) (condition is pattern) (body) (else) ]
                TypedExpr::If {
                    branches,
                    final_else,
                    tipo,
                    ..
                } => branches.iter().rfold(
                    self.build(final_else, module_build_name, &[]),
                    |acc, branch| {
                        let condition = self.build(&branch.condition, module_build_name, &[]);
                        let body = self.build(&branch.body, module_build_name, &[]);

                        match &branch.is {
                            Some(pattern) => AirTree::let_assignment(
                                "acc_var",
                                // use anon function as a delay to avoid evaluating the acc
                                AirTree::anon_func(vec![], acc, true),
                                self.assignment(
                                    pattern,
                                    condition,
                                    body,
                                    &pattern.tipo(&branch.condition).unwrap(),
                                    AssignmentProperties {
                                        value_type: branch.condition.tipo(),
                                        kind: AssignmentKind::Expect { backpassing: () },
                                        remove_unused: false,
                                        full_check: true,
                                        otherwise: AirTree::local_var("acc_var", void()),
                                    },
                                ),
                            ),
                            None => AirTree::if_branch(tipo.clone(), condition, body, acc),
                        }
                    },
                ),

                TypedExpr::RecordAccess {
                    tipo,
                    index,
                    record,
                    ..
                } => {
                    assert!(
                        !record.tipo().is_pair(),
                        "illegal record access on a Pair. This should have been a tuple-index access."
                    );

                    if check_replaceable_opaque_type(&record.tipo(), &self.data_types) {
                        self.build(record, module_build_name, &[])
                    } else {
                        let function_name = format!("__access_index_{}", *index);

                        if self.code_gen_functions.get(&function_name).is_none() {
                            let mut body = AirTree::local_var("__fields", list(data()));

                            for _ in 0..*index {
                                body = AirTree::builtin(
                                    DefaultFunction::TailList,
                                    list(data()),
                                    vec![body],
                                )
                            }

                            body = AirTree::builtin(DefaultFunction::HeadList, data(), vec![body]);

                            self.code_gen_functions.insert(
                                function_name.clone(),
                                CodeGenFunction::Function {
                                    body,
                                    params: vec!["__fields".to_string()],
                                },
                            );
                        }

                        let list_of_fields = AirTree::call(
                            self.special_functions
                                .use_function_tree(CONSTR_FIELDS_EXPOSER.to_string()),
                            list(data()),
                            vec![self.build(record, module_build_name, &[])],
                        );

                        AirTree::index_access(function_name, tipo.clone(), list_of_fields)
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
                        let val_constructor = {
                            let data_type = lookup_data_type_by_tipo(&self.data_types, tipo);

                            ValueConstructor::public(
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
                                        .len()
                                        as u16,
                                },
                            )
                        };

                        AirTree::var(val_constructor, name, "")
                    }
                    ModuleValueConstructor::Fn { name, module, .. } => {
                        let func = self.functions.get(&FunctionAccessKey {
                            module_name: module_name.clone(),
                            function_name: name.clone(),
                        });

                        let type_info = self.module_types.get(module_name.as_str()).unwrap();

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
                    ModuleValueConstructor::Constant { literal, .. } => {
                        builder::constants_ir(literal)
                    }
                },

                TypedExpr::Pair { tipo, fst, snd, .. } => AirTree::pair(
                    self.build(fst, module_build_name, &[]),
                    self.build(snd, module_build_name, &[]),
                    tipo.clone(),
                ),

                TypedExpr::Tuple { tipo, elems, .. } => AirTree::tuple(
                    elems
                        .iter()
                        .map(|elem| self.build(elem, module_build_name, &[]))
                        .collect_vec(),
                    tipo.clone(),
                ),

                TypedExpr::TupleIndex {
                    index, tuple, tipo, ..
                } => {
                    if tuple.tipo().is_pair() {
                        AirTree::pair_index(
                            *index,
                            tipo.clone(),
                            self.build(tuple, module_build_name, &[]),
                        )
                    } else {
                        let function_name = format!("__access_index_{}", *index);

                        if self.code_gen_functions.get(&function_name).is_none() {
                            let mut body = AirTree::local_var("__fields", list(data()));

                            for _ in 0..*index {
                                body = AirTree::builtin(
                                    DefaultFunction::TailList,
                                    list(data()),
                                    vec![body],
                                )
                            }

                            body = AirTree::builtin(DefaultFunction::HeadList, data(), vec![body]);

                            self.code_gen_functions.insert(
                                function_name.clone(),
                                CodeGenFunction::Function {
                                    body,
                                    params: vec!["__fields".to_string()],
                                },
                            );
                        }

                        AirTree::index_access(
                            function_name,
                            tipo.clone(),
                            self.build(tuple, module_build_name, &[]),
                        )
                    }
                }

                TypedExpr::ErrorTerm { tipo, .. } => AirTree::error(tipo.clone(), false),

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
                        let arg_val = self.build(&arg.value, module_build_name, &[]);

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
                        self.build(spread, module_build_name, &[]),
                        update_args,
                    )
                }
                TypedExpr::UnOp { value, op, .. } => {
                    AirTree::unop(*op, self.build(value, module_build_name, &[]))
                }
                TypedExpr::CurvePoint { point, .. } => AirTree::curve(*point.as_ref()),
            }
        }
    }

    pub fn assignment(
        &mut self,
        pattern: &TypedPattern,
        mut value: AirTree,
        then: AirTree,
        tipo: &Rc<Type>,
        props: AssignmentProperties,
    ) -> AirTree {
        assert!(
            match &value {
                AirTree::Var { name, .. } if props.kind.is_let() => {
                    name != "_"
                }
                _ => true,
            },
            "No discard expressions or let bindings should be in the tree at this point."
        );

        // Cast value to or from data so we don't have to worry from this point onward
        if props.value_type.is_data() && props.kind.is_expect() && !tipo.is_data() {
            value = AirTree::cast_from_data(value, tipo.clone(), props.otherwise.clone(), true);
        } else if !props.value_type.is_data() && tipo.is_data() {
            value = AirTree::cast_to_data(value, props.value_type.clone());
        }

        match pattern {
            Pattern::Int {
                value: expected_int,
                location,
                ..
            } => {
                assert!(props.kind.is_expect());

                let name = format!(
                    "__expected_by_{}_span_{}_{}",
                    expected_int, location.start, location.end
                );

                let expect = AirTree::binop(
                    BinOp::Eq,
                    bool(),
                    AirTree::int(expected_int),
                    AirTree::local_var(&name, int()),
                    int(),
                );

                let expr = AirTree::let_assignment(name, value, expect);

                AirTree::assert_bool(true, expr, then, props.otherwise.clone())
            }

            Pattern::Var { name, .. } => {
                if props.full_check {
                    let mut index_map = IndexMap::new();

                    let non_opaque_tipo = convert_opaque_type(tipo, &self.data_types, true);

                    let val = AirTree::local_var(name, tipo.clone());

                    if non_opaque_tipo.is_primitive() {
                        AirTree::let_assignment(name, value, then)
                    } else {
                        let expect = self.expect_type_assign(
                            &non_opaque_tipo,
                            val,
                            &mut index_map,
                            pattern.location(),
                            props.otherwise,
                        );

                        let assign_expect = AirTree::let_assignment("_", expect, then);

                        AirTree::let_assignment(name, value, assign_expect)
                    }
                } else {
                    AirTree::let_assignment(name, value, then)
                }
            }

            Pattern::Assign { name, pattern, .. } => {
                let inner_pattern = self.assignment(
                    pattern,
                    AirTree::local_var(name, tipo.clone()),
                    then,
                    tipo,
                    props,
                );
                AirTree::let_assignment(name, value, inner_pattern)
            }

            Pattern::Discard { name, .. } => {
                if props.full_check {
                    let name = &format!("__discard_expect_{}", name);
                    let mut index_map = IndexMap::new();

                    let non_opaque_tipo = convert_opaque_type(tipo, &self.data_types, true);

                    let val = AirTree::local_var(name, tipo.clone());

                    if non_opaque_tipo.is_primitive() {
                        AirTree::let_assignment(name, value, then)
                    } else {
                        let expect = self.expect_type_assign(
                            &non_opaque_tipo,
                            val,
                            &mut index_map,
                            pattern.location(),
                            props.otherwise,
                        );

                        let assignment = AirTree::let_assignment("_", expect, then);

                        AirTree::let_assignment(name, value, assignment)
                    }
                } else if !props.remove_unused {
                    AirTree::let_assignment(name, value, then)
                } else {
                    then
                }
            }

            Pattern::List { elements, tail, .. } => {
                assert!(tipo.is_list());
                assert!(props.kind.is_expect());

                let list_elem_types = tipo.get_inner_types();

                let list_elem_type = list_elem_types
                    .first()
                    .unwrap_or_else(|| unreachable!("No list element type?"));

                let mut elems = vec![];

                // If Some then push tail onto elems
                let then = match tail {
                    None => then,
                    Some(tail) => {
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

                        let then = if tail_name != "_" {
                            self.assignment(
                                tail,
                                val,
                                then,
                                tipo,
                                AssignmentProperties {
                                    value_type: tipo.clone(),
                                    kind: props.kind,
                                    remove_unused: true,
                                    full_check: props.full_check,
                                    otherwise: props.otherwise.clone(),
                                },
                            )
                        } else {
                            then
                        };

                        elems.push(tail_name);

                        then
                    }
                };

                let then = elements
                    .iter()
                    .enumerate()
                    .rfold(then, |then, (index, elem)| {
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

                        let then = if elem_name != "_" {
                            self.assignment(
                                elem,
                                val,
                                then,
                                list_elem_type,
                                AssignmentProperties {
                                    value_type: list_elem_type.clone(),
                                    kind: props.kind,
                                    remove_unused: true,
                                    full_check: props.full_check,
                                    otherwise: props.otherwise.clone(),
                                },
                            )
                        } else {
                            then
                        };

                        elems.push(elem_name);

                        then
                    });

                elems.reverse();

                if elements.is_empty() {
                    AirTree::list_empty(value, then, props.otherwise.clone())
                } else {
                    AirTree::list_access(
                        elems,
                        tipo.clone(),
                        tail.is_some(),
                        value,
                        if props.full_check {
                            ExpectLevel::Full
                        } else {
                            ExpectLevel::Items
                        },
                        then,
                        props.otherwise.clone(),
                    )
                }
            }

            Pattern::Pair {
                fst,
                snd,
                location: _,
            } => {
                let mut type_map: IndexMap<usize, Rc<Type>> = IndexMap::new();

                for (index, arg) in tipo.get_inner_types().iter().enumerate() {
                    let field_type = arg.clone();
                    type_map.insert(index, field_type);
                }

                assert!(type_map.len() == 2);

                let mut fields = vec![];

                let then = [fst, snd]
                    .iter()
                    .enumerate()
                    .rfold(then, |then, (field_index, arg)| {
                        let field_name = match arg.as_ref() {
                            Pattern::Var { name, .. } => name.to_string(),
                            Pattern::Assign { name, .. } => name.to_string(),
                            Pattern::Discard { name, .. } => {
                                if props.full_check {
                                    format!("__discard_{}_{}", name, field_index)
                                } else {
                                    "_".to_string()
                                }
                            }
                            _ => format!(
                                "field_{}_span_{}_{}",
                                field_index,
                                arg.location().start,
                                arg.location().end
                            ),
                        };

                        let arg_type = type_map.get(&field_index).unwrap_or_else(|| {
                            unreachable!(
                                "Missing type for field {} of constr {}",
                                field_index, field_name
                            )
                        });

                        let val = AirTree::local_var(&field_name, arg_type.clone());

                        let then = if field_name != "_" {
                            self.assignment(
                                arg,
                                val,
                                then,
                                arg_type,
                                AssignmentProperties {
                                    value_type: arg_type.clone(),
                                    kind: props.kind,
                                    remove_unused: true,
                                    full_check: props.full_check,
                                    otherwise: props.otherwise.clone(),
                                },
                            )
                        } else {
                            then
                        };

                        fields.push((field_index, field_name, arg_type.clone()));

                        then
                    });

                fields.reverse();

                // This `value` is either value param that was passed in or
                // local var
                let constructor_name = format!(
                    "__constructor_{}_span_{}_{}",
                    "Pair",
                    pattern.location().start,
                    pattern.location().end
                );

                let local_value = AirTree::local_var(&constructor_name, tipo.clone());

                let then = {
                    assert!(fields.len() == 2);

                    AirTree::pair_access(
                        fields
                            .first()
                            .map(|x| if x.1 == "_" { None } else { Some(x.1.clone()) })
                            .unwrap(),
                        fields
                            .last()
                            .map(|x| if x.1 == "_" { None } else { Some(x.1.clone()) })
                            .unwrap(),
                        tipo.clone(),
                        local_value,
                        props.full_check,
                        then,
                        props.otherwise.clone(),
                    )
                };

                AirTree::let_assignment(constructor_name, value, then)
            }

            Pattern::Constructor {
                constructor: PatternConstructor::Record { name, .. },
                ..
            } if tipo.is_bool() => {
                assert!(props.kind.is_expect());

                AirTree::assert_bool(name == "True", value, then, props.otherwise.clone())
            }

            Pattern::Constructor { .. } if tipo.is_void() => {
                // Void type is checked when casting from data
                // So we just assign the value and move on
                AirTree::let_assignment("_", value, then)
            }

            Pattern::Constructor {
                arguments,
                constructor: PatternConstructor::Record { name, field_map },
                tipo: constr_tipo,
                ..
            } => {
                // Constr execution branch
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

                let mut fields = vec![];

                let then = arguments
                    .iter()
                    .enumerate()
                    .rfold(then, |then, (index, arg)| {
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

                        let val = AirTree::local_var(&field_name, arg_type.clone());

                        let then = if field_name != "_" {
                            self.assignment(
                                &arg.value,
                                val,
                                then,
                                arg_type,
                                AssignmentProperties {
                                    value_type: arg_type.clone(),
                                    kind: props.kind,
                                    remove_unused: true,
                                    full_check: props.full_check,
                                    otherwise: props.otherwise.clone(),
                                },
                            )
                        } else {
                            then
                        };

                        fields.push((field_index, field_name, arg_type.clone()));

                        then
                    });

                fields.reverse();

                // This `value` is either value param that was passed in or
                // local var
                let constructor_name = format!(
                    "__constructor_{}_span_{}_{}",
                    name,
                    pattern.location().start,
                    pattern.location().end
                );

                let subject_name = format!(
                    "__subject_{}_span_{}_{}",
                    name,
                    pattern.location().start,
                    pattern.location().end
                );

                let local_value = AirTree::local_var(&constructor_name, tipo.clone());

                let then = if check_replaceable_opaque_type(tipo, &self.data_types) {
                    AirTree::let_assignment(&fields[0].1, local_value, then)
                } else {
                    AirTree::fields_expose(
                        fields,
                        local_value,
                        props.full_check,
                        then,
                        props.otherwise.clone(),
                    )
                };

                let data_type = lookup_data_type_by_tipo(&self.data_types, tipo)
                    .unwrap_or_else(|| unreachable!("Failed to find definition for {}", name));

                let then = if props.kind.is_expect()
                    && (data_type.constructors.len() > 1 || props.full_check)
                {
                    let (index, _) = data_type
                        .constructors
                        .iter()
                        .enumerate()
                        .find(|(_, constr)| constr.name == *name)
                        .unwrap_or_else(|| {
                            panic!("Found constructor type {} with 0 constructors", name)
                        });

                    AirTree::when(
                        &subject_name,
                        void(),
                        tipo.clone(),
                        AirTree::local_var(&constructor_name, tipo.clone()),
                        AirTree::assert_constr_index(
                            index,
                            AirTree::local_var(&subject_name, tipo.clone()),
                            then,
                            props.otherwise.clone(),
                        ),
                    )
                } else {
                    assert!(data_type.constructors.len() == 1);
                    then
                };

                AirTree::let_assignment(constructor_name, value, then)
            }

            Pattern::Tuple {
                elems, location, ..
            } => {
                let mut type_map: IndexMap<usize, Rc<Type>> = IndexMap::new();

                for (index, arg) in tipo.get_inner_types().iter().enumerate() {
                    let field_type = arg.clone();
                    type_map.insert(index, field_type);
                }

                assert!(type_map.len() == elems.len());

                let mut fields = vec![];

                let then = elems.iter().enumerate().rfold(then, |then, (index, arg)| {
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

                    let val = AirTree::local_var(&tuple_name, arg_type.clone());

                    let then = if "_" != tuple_name {
                        self.assignment(
                            arg,
                            val,
                            then,
                            arg_type,
                            AssignmentProperties {
                                value_type: arg_type.clone(),
                                kind: props.kind,
                                remove_unused: true,
                                full_check: props.full_check,
                                otherwise: props.otherwise.clone(),
                            },
                        )
                    } else {
                        then
                    };

                    fields.push(tuple_name);

                    then
                });

                fields.reverse();

                // This `value` is either value param that was passed in or local var

                AirTree::tuple_access(
                    fields,
                    tipo.clone(),
                    value,
                    props.full_check,
                    then,
                    props.otherwise.clone(),
                )
            }
        }
    }

    pub fn expect_type_assign(
        &mut self,
        tipo: &Rc<Type>,
        value: AirTree,
        defined_data_types: &mut IndexMap<String, u64>,
        location: Span,
        otherwise: AirTree,
    ) -> AirTree {
        assert!(tipo.get_generic().is_none());
        // Shouldn't be needed but still here just in case
        // this function is called from anywhere else besides assignment
        let tipo = &convert_opaque_type(tipo, &self.data_types, true);
        let uplc_type = tipo.get_uplc_type();

        match uplc_type {
            // primitives
            // Untyped Data
            Some(
                UplcType::Integer
                | UplcType::String
                | UplcType::Bool
                | UplcType::ByteString
                | UplcType::Unit
                | UplcType::Bls12_381G1Element
                | UplcType::Bls12_381G2Element
                | UplcType::Bls12_381MlResult
                | UplcType::Data,
            ) => value,

            // Map type
            Some(UplcType::List(_)) if tipo.is_map() => {
                assert!(!tipo.get_inner_types().is_empty());

                let inner_list_type = &tipo.get_inner_types()[0];
                let inner_pair_types = inner_list_type.get_inner_types();

                assert!(inner_pair_types.len() == 2);

                let map_name = format!("__map_span_{}_{}", location.start, location.end);
                let pair_name = format!("__pair_span_{}_{}", location.start, location.end);
                let fst_name = format!("__pair_fst_span_{}_{}", location.start, location.end);
                let snd_name = format!("__pair_snd_span_{}_{}", location.start, location.end);

                let expect_fst = self.expect_type_assign(
                    &inner_pair_types[0],
                    AirTree::local_var(fst_name.clone(), inner_pair_types[0].clone()),
                    defined_data_types,
                    location,
                    otherwise.clone(),
                );

                let expect_snd = self.expect_type_assign(
                    &inner_pair_types[1],
                    AirTree::local_var(snd_name.clone(), inner_pair_types[1].clone()),
                    defined_data_types,
                    location,
                    otherwise.clone(),
                );

                let anon_func_body = AirTree::pair_access(
                    Some(fst_name),
                    Some(snd_name),
                    inner_list_type.clone(),
                    AirTree::local_var(&pair_name, inner_list_type.clone()),
                    true,
                    AirTree::let_assignment("_", expect_fst, expect_snd),
                    otherwise.clone(),
                );

                let unwrap_function = AirTree::anon_func(vec![pair_name], anon_func_body, false);

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
                    vec![AirTree::local_var(&map_name, tipo.clone()), unwrap_function],
                );

                AirTree::let_assignment(&map_name, value, func_call)
            }
            // Tuple type
            Some(UplcType::List(_)) if tipo.is_tuple() => {
                let tuple_inner_types = tipo.get_inner_types();

                assert!(!tuple_inner_types.is_empty());

                let tuple_name = format!("__tuple_span_{}_{}", location.start, location.end);

                let mut tuple_expect_items = vec![];

                let then = tuple_inner_types.iter().enumerate().rfold(
                    AirTree::void(),
                    |then, (index, arg)| {
                        let tuple_index_name = format!(
                            "__tuple_index_{}_span_{}_{}",
                            index, location.start, location.end
                        );

                        let expect_tuple_item = self.expect_type_assign(
                            arg,
                            AirTree::local_var(&tuple_index_name, arg.clone()),
                            defined_data_types,
                            location,
                            otherwise.clone(),
                        );

                        tuple_expect_items.push(tuple_index_name);

                        AirTree::let_assignment("_", expect_tuple_item, then)
                    },
                );

                tuple_expect_items.reverse();

                let tuple_access = AirTree::tuple_access(
                    tuple_expect_items,
                    tipo.clone(),
                    AirTree::local_var(&tuple_name, tipo.clone()),
                    true,
                    then,
                    otherwise,
                );

                AirTree::let_assignment(&tuple_name, value, tuple_access)
            }
            // Regular List type
            Some(UplcType::List(_)) => {
                assert!(!tipo.get_inner_types().is_empty());

                let inner_list_type = &tipo.get_inner_types()[0];

                if inner_list_type.is_data() {
                    value
                } else {
                    let list_name = format!("__list_span_{}_{}", location.start, location.end);
                    let item_name = format!("__item_span_{}_{}", location.start, location.end);

                    let expect_item = self.expect_type_assign(
                        inner_list_type,
                        AirTree::cast_from_data(
                            AirTree::local_var(&item_name, data()),
                            inner_list_type.clone(),
                            otherwise.clone(),
                            true,
                        ),
                        defined_data_types,
                        location,
                        otherwise,
                    );

                    let anon_func_body = expect_item;

                    let unwrap_function =
                        AirTree::anon_func(vec![item_name], anon_func_body, false);

                    let function = self.code_gen_functions.get(EXPECT_ON_LIST);

                    if function.is_none() {
                        let expect_list_func = AirTree::expect_on_list();
                        self.code_gen_functions.insert(
                            EXPECT_ON_LIST.to_string(),
                            CodeGenFunction::Function {
                                body: expect_list_func,
                                params: vec![
                                    "__list_to_check".to_string(),
                                    "__check_with".to_string(),
                                ],
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
                        vec![
                            AirTree::local_var(&list_name, tipo.clone()),
                            unwrap_function,
                        ],
                    );

                    AirTree::let_assignment(&list_name, value, func_call)
                }
            }
            // Pair type
            Some(UplcType::Pair(_, _)) => {
                let tuple_inner_types = tipo.get_inner_types();

                assert!(tuple_inner_types.len() == 2);

                let pair_name = format!("__pair_span_{}_{}", location.start, location.end);

                let fst_name = format!("__pair_fst_span_{}_{}", location.start, location.end);
                let snd_name = format!("__pair_snd_span_{}_{}", location.start, location.end);

                let expect_fst = self.expect_type_assign(
                    &tuple_inner_types[0],
                    AirTree::local_var(fst_name.clone(), tuple_inner_types[0].clone()),
                    defined_data_types,
                    location,
                    otherwise.clone(),
                );

                let expect_snd = self.expect_type_assign(
                    &tuple_inner_types[1],
                    AirTree::local_var(snd_name.clone(), tuple_inner_types[1].clone()),
                    defined_data_types,
                    location,
                    otherwise.clone(),
                );

                let pair_access = AirTree::pair_access(
                    Some(fst_name.clone()),
                    Some(snd_name.clone()),
                    tipo.clone(),
                    AirTree::local_var(&pair_name, tipo.clone()),
                    true,
                    AirTree::let_assignment("_", expect_fst, expect_snd),
                    otherwise,
                );

                AirTree::let_assignment(&pair_name, value, pair_access)
            }

            // Constr type
            None => {
                let data_type =
                    lookup_data_type_by_tipo(&self.data_types, tipo).unwrap_or_else(|| {
                        unreachable!("We need a data type definition for type {:#?}", tipo)
                    });

                let data_type_variant = tipo
                    .get_inner_types()
                    .iter()
                    .map(|arg| get_arg_type_name(arg))
                    .join("_");

                assert!(data_type.typed_parameters.len() == tipo.arg_types().unwrap().len());

                let mono_types: IndexMap<u64, Rc<Type>> = if !data_type.typed_parameters.is_empty()
                {
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

                // mutate code_gen_funcs and defined_data_types in this if branch
                if function.is_none() && defined_data_types.get(&data_type_name).is_none() {
                    defined_data_types.insert(data_type_name.clone(), 1);

                    let current_defined = defined_data_types.clone();
                    let mut diff_defined_types = vec![];

                    let constr_clauses = data_type.constructors.iter().enumerate().rfold(
                        otherwise.clone(),
                        |acc, (index, constr)| {
                            let mut constr_args = vec![];

                            let constr_then = constr.arguments.iter().enumerate().rfold(
                                AirTree::void(),
                                |then, (index, arg)| {
                                    let arg_name =
                                        arg.label.clone().unwrap_or(format!("__field_{index}"));

                                    let arg_tipo =
                                        find_and_replace_generics(&arg.tipo, &mono_types);

                                    constr_args.push((index, arg_name.clone(), arg_tipo.clone()));

                                    AirTree::let_assignment(
                                        "_",
                                        self.expect_type_assign(
                                            &arg_tipo.clone(),
                                            AirTree::local_var(arg_name, arg_tipo),
                                            defined_data_types,
                                            location,
                                            otherwise.clone(),
                                        ),
                                        then,
                                    )
                                },
                            );
                            constr_args.reverse();

                            let then = if constr_args.is_empty() {
                                AirTree::fields_empty(
                                    AirTree::local_var(
                                        format!(
                                            "__constr_var_span_{}_{}",
                                            location.start, location.end
                                        ),
                                        tipo.clone(),
                                    ),
                                    constr_then,
                                    otherwise.clone(),
                                )
                            } else {
                                AirTree::fields_expose(
                                    constr_args,
                                    AirTree::local_var(
                                        format!(
                                            "__constr_var_span_{}_{}",
                                            location.start, location.end
                                        ),
                                        tipo.clone(),
                                    ),
                                    true,
                                    constr_then,
                                    otherwise.clone(),
                                )
                            };

                            // Special case here for future refactoring
                            AirTree::anon_func(
                                vec![],
                                AirTree::assert_constr_index(
                                    index,
                                    AirTree::local_var(
                                        format!(
                                            "__subject_span_{}_{}",
                                            location.start, location.end
                                        ),
                                        tipo.clone(),
                                    ),
                                    then,
                                    acc,
                                ),
                                true,
                            )
                        },
                    );

                    let when_expr = AirTree::when(
                        format!("__subject_span_{}_{}", location.start, location.end),
                        void(),
                        tipo.clone(),
                        AirTree::local_var(
                            format!("__constr_var_span_{}_{}", location.start, location.end),
                            tipo.clone(),
                        ),
                        AirTree::call(constr_clauses, void(), vec![]),
                    );

                    let func_body = AirTree::let_assignment(
                        format!("__constr_var_span_{}_{}", location.start, location.end),
                        AirTree::local_var("__param_0", tipo.clone()),
                        when_expr,
                    );

                    for (inner_data_type, inner_count) in defined_data_types.iter() {
                        if let Some(prev_count) = current_defined.get(inner_data_type) {
                            if inner_count - prev_count > 0 {
                                diff_defined_types.push(inner_data_type.to_string());
                            }
                        } else {
                            diff_defined_types.push(inner_data_type.to_string());
                        }
                    }

                    let code_gen_func = match self.tracing {
                        TraceLevel::Silent => CodeGenFunction::Function {
                            body: func_body,
                            params: vec!["__param_0".to_string()],
                        },
                        TraceLevel::Compact | TraceLevel::Verbose => CodeGenFunction::Function {
                            body: func_body,
                            params: vec!["__param_0".to_string(), "__param_msg".to_string()],
                        },
                    };

                    self.code_gen_functions
                        .insert(data_type_name.clone(), code_gen_func);
                } else if let Some(counter) = defined_data_types.get_mut(&data_type_name) {
                    *counter += 1;
                } else {
                    defined_data_types.insert(data_type_name.to_string(), 1);
                }

                let args = match self.tracing {
                    TraceLevel::Silent => vec![value],
                    TraceLevel::Compact | TraceLevel::Verbose => vec![value, otherwise],
                };

                let module_fn = ValueConstructorVariant::ModuleFn {
                    name: data_type_name.to_string(),
                    field_map: None,
                    module: "".to_string(),
                    arity: args.len(),
                    location,
                    builtin: None,
                };

                let func_var = AirTree::var(
                    ValueConstructor::public(tipo.clone(), module_fn),
                    data_type_name,
                    "",
                );

                AirTree::call(func_var, void(), args)
            }
        }
    }

    pub fn handle_each_clause(
        &mut self,
        clauses: &[TypedClause],
        final_clause: TypedClause,
        subject_tipo: &Rc<Type>,
        props: &mut ClauseProperties,
        module_name: &str,
    ) -> AirTree {
        assert!(
            !subject_tipo.is_void(),
            "WHY ARE YOU PATTERN MATCHING VOID???"
        );
        props.complex_clause = false;

        if let Some((clause, rest_clauses)) = clauses.split_first() {
            let mut clause_then = self.build(&clause.then, module_name, &[]);

            // handles clause guard if it exists
            if let Some(guard) = &clause.guard {
                props.complex_clause = true;

                let clause_guard_name = format!(
                    "__clause_guard_span_{}_{}",
                    clause.location.start, clause.location.end
                );

                clause_then = AirTree::let_assignment(
                    &clause_guard_name,
                    builder::handle_clause_guard(guard),
                    AirTree::clause_guard(
                        &clause_guard_name,
                        AirTree::bool(true),
                        bool(),
                        clause_then,
                    ),
                );
            }

            match &mut props.specific_clause {
                // TODO: Implement PairClause and PairClauseGuard
                SpecificClause::ConstrClause => {
                    let data_type = lookup_data_type_by_tipo(&self.data_types, subject_tipo);

                    let (clause_cond, clause_assign) =
                        self.clause_pattern(&clause.pattern, subject_tipo, props, clause_then);

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
                            clause_assign,
                            self.handle_each_clause(
                                rest_clauses,
                                final_clause,
                                subject_tipo,
                                &mut next_clause_props,
                                module_name,
                            ),
                        )
                    } else if let Some(data_type) = data_type {
                        if data_type.constructors.len() > 1 {
                            AirTree::clause(
                                &props.original_subject_name,
                                clause_cond,
                                subject_tipo.clone(),
                                clause_assign,
                                self.handle_each_clause(
                                    rest_clauses,
                                    final_clause,
                                    subject_tipo,
                                    &mut next_clause_props,
                                    module_name,
                                ),
                                complex_clause,
                            )
                        } else {
                            AirTree::wrap_clause(
                                clause_assign,
                                self.handle_each_clause(
                                    rest_clauses,
                                    final_clause,
                                    subject_tipo,
                                    &mut next_clause_props,
                                    module_name,
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
                            clause_assign,
                            self.handle_each_clause(
                                rest_clauses,
                                final_clause,
                                subject_tipo,
                                &mut next_clause_props,
                                module_name,
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
                            self.clause_pattern(&clause.pattern, subject_tipo, props, clause_then);

                        return AirTree::wrap_clause(
                            clause_assign,
                            self.handle_each_clause(
                                rest_clauses,
                                final_clause,
                                subject_tipo,
                                &mut next_clause_props,
                                module_name,
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
                        self.clause_pattern(&clause.pattern, subject_tipo, props, clause_then);

                    let complex_clause = props.complex_clause;

                    if current_checked_index < elements_len.try_into().unwrap()
                        || next_tail_name.is_some()
                    {
                        AirTree::list_clause(
                            tail_name,
                            subject_tipo.clone(),
                            clause_assign,
                            self.handle_each_clause(
                                rest_clauses,
                                final_clause,
                                subject_tipo,
                                &mut next_clause_props,
                                module_name,
                            ),
                            next_tail_name,
                            complex_clause,
                        )
                    } else {
                        AirTree::wrap_clause(
                            clause_assign,
                            self.handle_each_clause(
                                rest_clauses,
                                final_clause,
                                subject_tipo,
                                &mut next_clause_props,
                                module_name,
                            ),
                        )
                    }
                }
                SpecificClause::TupleClause {
                    defined_tuple_indices,
                } => {
                    let current_defined_indices = defined_tuple_indices.clone();

                    let (_, pattern_assigns) =
                        self.clause_pattern(&clause.pattern, subject_tipo, props, clause_then);

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
                            pattern_assigns,
                            self.handle_each_clause(
                                rest_clauses,
                                final_clause,
                                subject_tipo,
                                &mut next_clause_props,
                                module_name,
                            ),
                        )
                    } else {
                        AirTree::tuple_clause(
                            &props.original_subject_name,
                            subject_tipo.clone(),
                            new_defined_indices,
                            current_defined_indices,
                            pattern_assigns,
                            self.handle_each_clause(
                                rest_clauses,
                                final_clause,
                                subject_tipo,
                                &mut next_clause_props,
                                module_name,
                            ),
                            props.complex_clause,
                        )
                    }
                }
                SpecificClause::PairClause => {
                    let (_, pattern_assigns) =
                        self.clause_pattern(&clause.pattern, subject_tipo, props, clause_then);

                    let mut next_clause_props = ClauseProperties::init(
                        subject_tipo,
                        props.clause_var_name.clone(),
                        props.original_subject_name.clone(),
                    );

                    AirTree::wrap_clause(
                        pattern_assigns,
                        self.handle_each_clause(
                            rest_clauses,
                            final_clause,
                            subject_tipo,
                            &mut next_clause_props,
                            module_name,
                        ),
                    )
                }
            }
        } else {
            // handle final_clause
            props.final_clause = true;

            assert!(final_clause.guard.is_none());

            let clause_then = self.build(&final_clause.then, module_name, &[]);

            let (condition, assignments) =
                self.clause_pattern(&final_clause.pattern, subject_tipo, props, clause_then);

            AirTree::finally(condition, assignments)
        }
    }

    pub fn clause_pattern(
        &self,
        pattern: &Pattern<PatternConstructor, Rc<Type>>,
        subject_tipo: &Rc<Type>,
        props: &mut ClauseProperties,
        then: AirTree,
        // We return condition and then assignments sequence
    ) -> (AirTree, AirTree) {
        match pattern {
            Pattern::Int { value, .. } => {
                assert!(!props.final_clause);
                (AirTree::int(value), then)
            }
            Pattern::Var { name, .. } => (
                AirTree::void(),
                AirTree::let_assignment(
                    name,
                    AirTree::local_var(&props.clause_var_name, subject_tipo.clone()),
                    then,
                ),
            ),
            Pattern::Assign { name, pattern, .. } => {
                let (inner_condition, inner_assignment) =
                    self.clause_pattern(pattern, subject_tipo, props, then);

                (
                    inner_condition,
                    AirTree::let_assignment(
                        name,
                        AirTree::local_var(&props.clause_var_name, subject_tipo.clone()),
                        inner_assignment,
                    ),
                )
            }
            Pattern::Discard { .. } => (AirTree::void(), then),
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
                    .first()
                    .unwrap_or_else(|| unreachable!("No list element type?"));

                let defined_tails = defined_tails.clone();

                let mut elems = vec![];

                let mut list_tail = None;

                let elems_then = tail.iter().rfold(then, |inner_then, elem| {
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

                    if &elem_name != "_" && !defined_tails.is_empty() {
                        list_tail = Some((tail.unwrap().to_string(), elem_name.to_string()));
                    }

                    let inner_then = if elem_name != "_" {
                        self.nested_clause_condition(
                            elem,
                            subject_tipo,
                            &mut elem_props,
                            inner_then,
                        )
                    } else {
                        inner_then
                    };

                    if props.final_clause && defined_tails.is_empty() {
                        elems.push(elem_name);
                    }

                    *complex_clause = *complex_clause || elem_props.complex_clause;

                    inner_then
                });

                let elems_then =
                    elements
                        .iter()
                        .enumerate()
                        .rfold(elems_then, |elems_then, (index, elem)| {
                            // TODO: Turn 'Pattern' into another type instead of using strings and
                            // expecting a special magic string '_'.
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

                            let elems_then = if elem_name != "_" {
                                self.nested_clause_condition(
                                    elem,
                                    list_elem_type,
                                    &mut elem_props,
                                    elems_then,
                                )
                            } else {
                                elems_then
                            };

                            elems.push(elem_name);

                            *complex_clause = *complex_clause || elem_props.complex_clause;

                            elems_then
                        });

                elems.reverse();

                // This case is really only possible with something like
                // when_tuple_empty_lists

                let list_assign = if props.final_clause && defined_tails.is_empty() {
                    AirTree::list_access(
                        elems,
                        subject_tipo.clone(),
                        tail.is_some(),
                        AirTree::local_var(&props.clause_var_name, subject_tipo.clone()),
                        // One special usage of list access here
                        // So for the msg we pass in empty string if tracing is on
                        // Since check_last_item is false this will never get added to the final uplc anyway
                        ExpectLevel::None,
                        elems_then,
                        AirTree::error(void(), false),
                    )
                } else {
                    assert!(defined_tails.len() >= elems.len());

                    AirTree::list_expose(
                        elems
                            .into_iter()
                            .zip(defined_tails)
                            .filter(|(head, _)| head != "_")
                            .map(|(head, tail)| (tail, head))
                            .collect_vec(),
                        list_tail,
                        subject_tipo.clone(),
                        elems_then,
                    )
                };

                (AirTree::void(), list_assign)
            }

            Pattern::Pair { fst, snd, .. } => {
                let items_type = subject_tipo.get_inner_types();

                let mut name_index_assigns = vec![];

                let next_then =
                    [fst, snd]
                        .iter()
                        .enumerate()
                        .rfold(then, |inner_then, (index, element)| {
                            let elem_name = match element.as_ref() {
                                Pattern::Var { name, .. } => Some(name.to_string()),
                                Pattern::Assign { name, .. } => Some(name.to_string()),
                                Pattern::Discard { .. } => None,
                                _ => Some(format!(
                                    "pair_index_{}_span_{}_{}",
                                    index,
                                    element.location().start,
                                    element.location().end
                                )),
                            };

                            let mut pair_props = ClauseProperties::init_inner(
                                &items_type[index],
                                elem_name.clone().unwrap_or_else(|| "_".to_string()),
                                elem_name.clone().unwrap_or_else(|| "_".to_string()),
                                props.final_clause,
                            );

                            let elem = if elem_name.is_some() {
                                self.nested_clause_condition(
                                    element,
                                    &items_type[index],
                                    &mut pair_props,
                                    inner_then,
                                )
                            } else {
                                inner_then
                            };

                            props.complex_clause =
                                props.complex_clause || pair_props.complex_clause;

                            name_index_assigns.push((elem_name, index));

                            elem
                        });

                name_index_assigns.reverse();

                let field_assign = if name_index_assigns.iter().all(|s| s.0.is_none()) {
                    next_then
                } else {
                    AirTree::pair_access(
                        name_index_assigns[0].0.clone(),
                        name_index_assigns[1].0.clone(),
                        subject_tipo.clone(),
                        AirTree::local_var(props.clause_var_name.clone(), subject_tipo.clone()),
                        false,
                        next_then,
                        AirTree::error(void(), false),
                    )
                };

                (AirTree::void(), field_assign)
            }

            Pattern::Constructor { name, .. } if subject_tipo.is_bool() => {
                (AirTree::bool(name == "True"), then)
            }

            Pattern::Constructor {
                name,
                arguments,
                constructor,
                tipo: function_tipo,
                ..
            } => {
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

                let mut fields = vec![];

                let next_then =
                    arguments
                        .iter()
                        .enumerate()
                        .rfold(then, |inner_then, (index, arg)| {
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
                                self.nested_clause_condition(
                                    &arg.value,
                                    arg_type,
                                    &mut field_props,
                                    inner_then,
                                )
                            } else {
                                inner_then
                            };

                            props.complex_clause =
                                props.complex_clause || field_props.complex_clause;

                            fields.push((field_index, field_name, arg_type.clone()));

                            statement
                        });

                fields.reverse();

                let field_assign = if check_replaceable_opaque_type(subject_tipo, &self.data_types)
                {
                    AirTree::let_assignment(
                        &fields[0].1,
                        AirTree::local_var(props.clause_var_name.clone(), subject_tipo.clone()),
                        next_then,
                    )
                } else if fields.iter().all(|s| s.1 == "_") {
                    next_then
                } else {
                    AirTree::fields_expose(
                        fields,
                        AirTree::local_var(props.clause_var_name.clone(), subject_tipo.clone()),
                        false,
                        next_then,
                        AirTree::error(void(), false),
                    )
                };

                (AirTree::int(constr_index), field_assign)
            }
            Pattern::Tuple { elems, .. } => {
                let items_type = subject_tipo.get_inner_types();

                let mut name_index_assigns = vec![];

                let next_then =
                    elems
                        .iter()
                        .enumerate()
                        .rfold(then, |inner_then, (index, element)| {
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
                                    inner_then,
                                )
                            } else {
                                inner_then
                            };

                            props.complex_clause =
                                props.complex_clause || tuple_props.complex_clause;

                            name_index_assigns.push((elem_name, index));

                            elem
                        });

                name_index_assigns.reverse();

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
                name_index_assigns.iter().for_each(|(name, index)| {
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

                let tuple_name_assigns = previous_defined_names.into_iter().rev().fold(
                    next_then,
                    |inner_then, (index, prev_name, name)| {
                        AirTree::let_assignment(
                            name,
                            AirTree::local_var(prev_name, items_type[index].clone()),
                            inner_then,
                        )
                    },
                );

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

                    (
                        AirTree::void(),
                        AirTree::tuple_access(
                            names,
                            subject_tipo.clone(),
                            AirTree::local_var(&props.clause_var_name, subject_tipo.clone()),
                            false,
                            tuple_name_assigns,
                            AirTree::error(void(), false),
                        ),
                    )
                } else {
                    (AirTree::void(), tuple_name_assigns)
                }
            }
        }
    }

    fn nested_clause_condition(
        &self,
        pattern: &Pattern<PatternConstructor, Rc<Type>>,
        subject_tipo: &Rc<Type>,
        props: &mut ClauseProperties,
        then: AirTree,
    ) -> AirTree {
        if props.final_clause {
            props.complex_clause = false;
            let (_, assign) = self.clause_pattern(pattern, subject_tipo, props, then);
            assign
        } else {
            match pattern {
                Pattern::Int { value, .. } => {
                    props.complex_clause = true;
                    AirTree::clause_guard(
                        &props.original_subject_name,
                        AirTree::int(value),
                        int(),
                        then,
                    )
                }
                Pattern::Var { name, .. } => AirTree::let_assignment(
                    name,
                    AirTree::local_var(&props.clause_var_name, subject_tipo.clone()),
                    then,
                ),
                Pattern::Assign { name, pattern, .. } => AirTree::let_assignment(
                    name,
                    AirTree::local_var(&props.clause_var_name, subject_tipo.clone()),
                    self.nested_clause_condition(pattern, subject_tipo, props, then),
                ),
                Pattern::Discard { .. } => then,
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
                            then,
                        )
                    } else {
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
                            let tail_name = format!("{tail_name_base}_{index}");

                            if elements.len() - 1 == index {
                                if tail.is_none() {
                                    *current_index += 1;
                                    defined_tails.push(tail_name);
                                }
                            } else {
                                *current_index += 1;
                                defined_tails.push(tail_name);
                            };
                        }

                        let (_, assigns) = self.clause_pattern(pattern, subject_tipo, props, then);

                        elements
                            .iter()
                            .enumerate()
                            .rfold(assigns, |then, (index, _)| {
                                let prev_tail_name = if index == 0 {
                                    props.original_subject_name.clone()
                                } else {
                                    format!("{}_{}", tail_name_base, index - 1)
                                };

                                let tail_name = format!("{tail_name_base}_{index}");
                                if elements.len() - 1 == index {
                                    if tail.is_some() {
                                        AirTree::list_clause_guard(
                                            prev_tail_name,
                                            subject_tipo.clone(),
                                            true,
                                            None,
                                            then,
                                        )
                                    } else {
                                        AirTree::list_clause_guard(
                                            prev_tail_name,
                                            subject_tipo.clone(),
                                            true,
                                            Some(tail_name.to_string()),
                                            AirTree::list_clause_guard(
                                                tail_name.to_string(),
                                                subject_tipo.clone(),
                                                false,
                                                None,
                                                then,
                                            ),
                                        )
                                    }
                                } else {
                                    AirTree::list_clause_guard(
                                        prev_tail_name,
                                        subject_tipo.clone(),
                                        true,
                                        Some(tail_name.to_string()),
                                        then,
                                    )
                                }
                            })
                    }
                }

                Pattern::Pair { .. } => {
                    let (_, assign) = self.clause_pattern(pattern, subject_tipo, props, then);
                    assign
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
                            then,
                        )
                    } else if subject_tipo.is_void() {
                        AirTree::clause_guard(
                            &props.original_subject_name,
                            AirTree::void(),
                            void(),
                            then,
                        )
                    } else {
                        let (cond, assign) =
                            self.clause_pattern(pattern, subject_tipo, props, then);

                        let data_type = lookup_data_type_by_tipo(&self.data_types, subject_tipo)
                            .expect("Missing data type");

                        if data_type.constructors.len() == 1 {
                            assign
                        } else {
                            props.complex_clause = true;
                            AirTree::clause_guard(
                                &props.original_subject_name,
                                cond,
                                subject_tipo.clone(),
                                assign,
                            )
                        }
                    }
                }
                Pattern::Tuple { .. } => {
                    let (_, assign) = self.clause_pattern(pattern, subject_tipo, props, then);

                    let defined_indices = match &props.specific_clause {
                        SpecificClause::TupleClause {
                            defined_tuple_indices,
                        } => defined_tuple_indices.clone(),
                        _ => unreachable!(),
                    };

                    AirTree::tuple_clause_guard(
                        &props.original_subject_name,
                        subject_tipo.clone(),
                        defined_indices,
                        assign,
                    )
                }
            }
        }
    }

    pub fn check_validator_args(
        &mut self,
        arguments: &[TypedArg],
        has_context: bool,
        body: AirTree,
        src_code: &str,
        lines: &LineNumbers,
    ) -> AirTree {
        let mut arg_names = vec![];

        arguments
            .iter()
            .rev()
            .with_position()
            .fold(body, |inner_then, arg_position| match arg_position {
                itertools::Position::First(arg) if has_context => {
                    let arg_name = arg.arg_name.get_variable_name().unwrap_or("_").to_string();

                    AirTree::anon_func(vec![arg_name], inner_then, true)
                }
                itertools::Position::First(arg)
                | itertools::Position::Middle(arg)
                | itertools::Position::Last(arg) => {
                    let arg_name = arg.arg_name.get_variable_name().unwrap_or("_").to_string();
                    let arg_span = arg.location;

                    arg_names.push(arg_name.clone());

                    let param = AirTree::local_var(&arg_name, data());

                    let actual_type = convert_opaque_type(&arg.tipo, &self.data_types, true);

                    let otherwise_delayed = {
                        let msg = match self.tracing {
                            TraceLevel::Silent => "".to_string(),
                            TraceLevel::Compact => lines
                                .line_and_column_number(arg_span.start)
                                .expect("Out of bounds span")
                                .to_string(),
                            TraceLevel::Verbose => src_code
                                .get(arg_span.start..arg_span.end)
                                .expect("Out of bounds span")
                                .to_string(),
                        };

                        let msg_func_name = msg.split_whitespace().join("");

                        self.special_functions.insert_new_function(
                            msg_func_name.clone(),
                            if msg.is_empty() {
                                Term::Error.delay()
                            } else {
                                Term::Error.delayed_trace(Term::string(msg)).delay()
                            },
                            void(),
                        );

                        self.special_functions.use_function_tree(msg_func_name)
                    };

                    let inner_then = self.assignment(
                        &Pattern::Var {
                            location: Span::empty(),
                            name: arg_name.to_string(),
                        },
                        param,
                        inner_then,
                        &actual_type,
                        AssignmentProperties {
                            value_type: data(),
                            kind: AssignmentKind::expect(),
                            remove_unused: false,
                            full_check: true,
                            otherwise: otherwise_delayed,
                        },
                    );

                    AirTree::anon_func(vec![arg_name], inner_then, true)
                }
                itertools::Position::Only(_) => unreachable!(),
            })
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
            Fields::FirstField,
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
                    sorted_function_vec.push((generic_func, variant));
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
                    sorted_function_vec.push((generic_func, variant));
                }
            }

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

                    let node_to_edit = air_tree.find_air_tree_node(tree_path);

                    let defined_function = AirTree::define_func(
                        &key.function_name,
                        &key.module_name,
                        variant,
                        func_params.clone(),
                        is_recursive,
                        recursive_nonstatics,
                        body,
                        node_to_edit.clone(),
                    );

                    let defined_dependencies = self.hoist_dependent_functions(
                        deps,
                        params_empty,
                        (key, variant),
                        hoisted_functions,
                        functions_to_hoist,
                        defined_function,
                    );

                    // now hoist full function onto validator tree
                    *node_to_edit = defined_dependencies;

                    hoisted_functions.push((key.clone(), variant.clone()));
                } else {
                    let defined_func = self.hoist_dependent_functions(
                        deps,
                        params_empty,
                        (key, variant),
                        hoisted_functions,
                        functions_to_hoist,
                        body,
                    );

                    self.zero_arg_functions
                        .insert((key.clone(), variant.clone()), defined_func.to_vec());
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

                let node_to_edit = air_tree.find_air_tree_node(tree_path);

                let cyclic_func = AirTree::define_cyclic_func(
                    &key.function_name,
                    &key.module_name,
                    variant,
                    functions,
                    node_to_edit.clone(),
                );

                let defined_dependencies = self.hoist_dependent_functions(
                    deps,
                    // cyclic functions always have params
                    false,
                    (key, variant),
                    hoisted_functions,
                    functions_to_hoist,
                    cyclic_func,
                );

                // now hoist full function onto validator tree
                *node_to_edit = defined_dependencies;

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
        func_key_variant: (&FunctionAccessKey, &Variant),
        hoisted_functions: &mut Vec<(FunctionAccessKey, String)>,
        functions_to_hoist: &IndexMap<
            FunctionAccessKey,
            IndexMap<String, (TreePath, HoistableFunction)>,
        >,
        air_tree: AirTree,
    ) -> AirTree {
        let (key, variant) = func_key_variant;
        let (func_path, func_deps) = deps;

        let mut deps_vec = func_deps;
        let mut sorted_dep_vec = vec![];

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
                    sorted_dep_vec.push((dep.0.clone(), dep.1.clone()));
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
                    sorted_dep_vec.push((dep.0.clone(), dep.1.clone()));
                }
                HoistableFunction::Link(_) => todo!("Deal with Link later"),
                HoistableFunction::CyclicLink(cyclic_func) => {
                    sorted_dep_vec.retain(|(generic_func, variant)| {
                        !(generic_func == cyclic_func && variant.is_empty())
                    });

                    deps_vec.insert(0, (cyclic_func.clone(), "".to_string()));
                }
            }
        }

        sorted_dep_vec.dedup();

        sorted_dep_vec
            .into_iter()
            .fold(air_tree, |then, (dep_key, dep_variant)| {
                if (!params_empty
                    // if the dependency is the same as the function we're hoisting
                    // or we hoisted it, then skip it
                        && hoisted_functions.iter().any(|(generic, variant)| {
                                generic == &dep_key && variant == &dep_variant
                            }))
                    || (&dep_key == key && &dep_variant == variant)
                {
                    return then;
                }

                let dependency = functions_to_hoist
                    .get(&dep_key)
                    .unwrap_or_else(|| panic!("Missing Function Definition"));

                let (dep_path, dep_function) = dependency
                    .get(&dep_variant)
                    .unwrap_or_else(|| panic!("Missing Function Variant Definition"));

                // In the case of zero args, we need to hoist the dependency function to the top of the zero arg function
                // The dependency we are hoisting should have an equal path to the function we hoisted
                // if we are going to hoist it
                if &dep_path.common_ancestor(func_path) == func_path || params_empty {
                    match dep_function.clone() {
                        HoistableFunction::Function {
                            body: mut dep_air_tree,
                            deps: dependency_deps,
                            params: dependent_params,
                        } => {
                            if dependent_params.is_empty() {
                                // continue for zero arg functions. They are treated like global hoists.
                                return then;
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

                            if !params_empty {
                                hoisted_functions.push((dep_key.clone(), dep_variant.clone()));
                            }

                            AirTree::define_func(
                                &dep_key.function_name,
                                &dep_key.module_name,
                                &dep_variant,
                                dependent_params,
                                is_dependent_recursive,
                                recursive_nonstatics,
                                dep_air_tree,
                                then,
                            )
                        }
                        HoistableFunction::CyclicFunction { functions, .. } => {
                            let mut functions = functions.clone();

                            for (_, body) in functions.iter_mut() {
                                modify_cyclic_calls(body, &dep_key, &self.cyclic_functions);
                            }

                            if !params_empty {
                                hoisted_functions.push((dep_key.clone(), dep_variant.clone()));
                            }

                            AirTree::define_cyclic_func(
                                &dep_key.function_name,
                                &dep_key.module_name,
                                &dep_variant,
                                functions,
                                then,
                            )
                        }
                        HoistableFunction::Link(_) => unreachable!(),
                        HoistableFunction::CyclicLink(_) => unreachable!(),
                    }
                } else {
                    then
                }
            })
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
            Fields::FirstField,
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
        depth_index: Fields,
    ) {
        air_tree.traverse_tree_with_path(
            path,
            current_depth,
            depth_index,
            &mut |air_tree, tree_path| {
                if let AirTree::Var {
                    constructor,
                    variant_name,
                    ..
                } = air_tree
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
                            .unwrap_or_else(|| {
                                panic!(
                                    "Missing function definition for {}. Known definitions: {:?}",
                                    generic_function_key.function_name,
                                    self.code_gen_functions.keys(),
                                )
                            });

                        if !dependency_functions
                            .iter()
                            .any(|(key, name)| key == &generic_function_key && name.is_empty())
                        {
                            dependency_functions
                                .push((generic_function_key.clone(), "".to_string()));
                        }

                        // Code gen functions are already monomorphized
                        if let Some(func_variants) = function_usage.get_mut(&generic_function_key) {
                            let (path, _) = func_variants.get_mut("").unwrap();
                            *path = path.common_ancestor(tree_path);
                        } else {
                            let CodeGenFunction::Function { body, params } = code_gen_func else {
                                unreachable!()
                            };

                            let mut function_variant_path = IndexMap::new();

                            let mut body = AirTree::no_op(body.clone());

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
                        .map(|arg| convert_opaque_type(&arg.tipo, &self.data_types, true))
                        .collect_vec();

                    function_def_types.push(convert_opaque_type(
                        &function_def.return_type,
                        &self.data_types,
                        true,
                    ));

                    let mono_types: IndexMap<u64, Rc<Type>> = if !function_def_types.is_empty() {
                        function_def_types
                            .iter()
                            .zip(function_var_types.iter())
                            .flat_map(|(func_tipo, var_tipo)| {
                                get_generic_id_and_type(func_tipo, var_tipo)
                            })
                            .collect()
                    } else {
                        IndexMap::new()
                    };

                    // Don't sort here. Mono types map is already in argument order.
                    let variant = mono_types
                        .iter()
                        .map(|(_, tipo)| get_generic_variant_name(tipo))
                        .join("");

                    variant_name.clone_from(&variant);

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

                            let mut function_air_tree_body = AirTree::no_op(self.build(
                                &function_def.body,
                                &generic_function_key.module_name,
                                &[],
                            ));

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

                        let mut function_air_tree_body = AirTree::no_op(self.build(
                            &function_def.body,
                            &generic_function_key.module_name,
                            &[],
                        ));

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

        while let Some(air_element) = ir_stack.pop() {
            let arg = self.gen_uplc(air_element, &mut arg_stack);
            if let Some(arg) = arg {
                arg_stack.push(arg);
            }
        }
        assert!(arg_stack.len() == 1, "Expected one term on the stack");
        arg_stack.pop().unwrap()
    }

    fn gen_uplc(&mut self, ir: Air, arg_stack: &mut Vec<Term<Name>>) -> Option<Term<Name>> {
        let convert_data_to_type = |term, tipo, otherwise| {
            if otherwise == Term::Error.delay() {
                builder::unknown_data_to_type(term, tipo)
            } else {
                builder::unknown_data_to_type_otherwise(term, tipo, otherwise)
            }
        };

        match ir {
            Air::Int { value } => Some(Term::integer(value.parse().unwrap())),
            Air::String { value } => Some(Term::string(value)),
            Air::ByteArray { bytes } => Some(Term::byte_string(bytes)),
            Air::Bool { value } => Some(Term::bool(value)),
            Air::CurvePoint { point, .. } => match point {
                Curve::Bls12_381(Bls12_381Point::G1(g1)) => Some(Term::bls12_381_g1(g1)),
                Curve::Bls12_381(Bls12_381Point::G2(g2)) => Some(Term::bls12_381_g2(g2)),
            },
            Air::Var {
                name,
                constructor,
                variant_name,
            } => match &constructor.variant {
                ValueConstructorVariant::LocalVariable { .. } => Some(Term::Var(
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

                        DefaultFunction::FstPair | DefaultFunction::SndPair => {
                            builder::undata_builtin(
                                builtin,
                                0,
                                &constructor.tipo.return_type().unwrap(),
                                vec![],
                            )
                        }

                        DefaultFunction::HeadList
                            if !constructor.tipo.return_type().unwrap().is_pair() =>
                        {
                            builder::undata_builtin(
                                builtin,
                                0,
                                &constructor.tipo.return_type().unwrap(),
                                vec![],
                            )
                        }

                        DefaultFunction::MkCons | DefaultFunction::MkPairData => {
                            unimplemented!(
                                "MkCons and MkPairData should be handled by an anon function or using [] or ( a, b, .., z) or Pair {{fst:a, snd: b}}.\n"
                            )
                        }
                        _ => {
                            let mut term: Term<Name> = (*builtin).into();

                            term = builder::apply_builtin_forces(term, builtin.force_count());

                            term
                        }
                    };
                    Some(term)
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

                        Some(term)
                    } else {
                        let name = if !module.is_empty() {
                            format!("{module}_{func_name}{variant_name}")
                        } else {
                            format!("{func_name}{variant_name}")
                        };

                        Some(Term::Var(
                            Name {
                                text: name,
                                unique: 0.into(),
                            }
                            .into(),
                        ))
                    }
                }
                ValueConstructorVariant::Record {
                    name: constr_name, ..
                } => {
                    // TODO handle pair
                    if constructor.tipo.is_bool() {
                        Some(Term::bool(constr_name == "True"))
                    } else if constructor.tipo.is_void() {
                        Some(Term::Constant(UplcConstant::Unit.into()))
                    } else {
                        let data_type = crate::tipo::lookup_data_type_by_tipo(
                            &self.data_types,
                            &constructor.tipo,
                        )
                        .unwrap_or_else(|| {
                            panic!(
                                "could not find data-type definition for {} within known set: {:?}",
                                constructor.tipo.to_pretty(0),
                                self.data_types.keys()
                            )
                        });

                        let (constr_index, constr_type) = data_type
                            .constructors
                            .iter()
                            .enumerate()
                            .find(|(_, x)| x.name == *constr_name)
                            .unwrap();

                        let mut term = Term::empty_list();

                        if constr_type.arguments.is_empty() {
                            term = Term::constr_data()
                                .apply(Term::integer(constr_index.into()))
                                .apply(term);

                            let mut program: Program<Name> = Program {
                                version: (1, 0, 0),
                                term,
                            };

                            let mut interner = CodeGenInterner::new();

                            interner.program(&mut program);

                            let eval_program: Program<NamedDeBruijn> =
                                program.remove_no_inlines().try_into().unwrap();

                            let evaluated_term: Term<NamedDeBruijn> = eval_program
                                .eval(ExBudget::default())
                                .result()
                                .expect("Evaluated a constant record and got an error");

                            term = evaluated_term.try_into().unwrap();
                        } else {
                            for (index, arg) in constructor
                                .tipo
                                .arg_types()
                                .unwrap()
                                .iter()
                                .enumerate()
                                .rev()
                            {
                                term = Term::mk_cons()
                                    .apply(convert_type_to_data(
                                        Term::var(format!("arg_{index}")),
                                        arg,
                                    ))
                                    .apply(term);
                            }

                            term = Term::constr_data()
                                .apply(Term::integer(constr_index.into()))
                                .apply(term);

                            for (index, _) in constr_type.arguments.iter().enumerate().rev() {
                                term = term.lambda(format!("arg_{index}"))
                            }
                        }
                        Some(term)
                    }
                }
            },
            Air::Void => Some(Term::Constant(UplcConstant::Unit.into())),
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

                    Some(list)
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
                    Some(term)
                }
            }
            Air::ListAccessor {
                names,
                tail,
                // TODO: rename tipo -
                // tipo here refers to the list type while the actual return
                // type is nothing since this is an assignment over some expression
                tipo,
                expect_level,
            } => {
                let value = arg_stack.pop().unwrap();

                let mut term = arg_stack.pop().unwrap();

                let otherwise = if matches!(expect_level, ExpectLevel::Full | ExpectLevel::Items) {
                    arg_stack.pop().unwrap()
                } else {
                    Term::Error.delay()
                };

                let list_id = self.id_gen.next();

                let mut id_list = vec![];
                id_list.push(list_id);

                names.iter().for_each(|_| {
                    id_list.push(self.id_gen.next());
                });

                let names_types = tipo
                    .get_inner_types()
                    .into_iter()
                    .cycle()
                    .take(names.len())
                    .zip(names)
                    .zip(id_list)
                    .map(|((tipo, name), id)| (name, tipo, id))
                    .collect_vec();

                term = builder::list_access_to_uplc(
                    &names_types,
                    tail,
                    term,
                    true,
                    expect_level,
                    otherwise,
                )
                .apply(value);

                Some(term)
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
                        builder::known_data_to_type(
                            Term::head_list().apply(Term::var(tail_var)),
                            &tipo.get_inner_types()[0],
                        )
                    };
                    term = term.lambda(head_name).apply(head_list);
                }

                Some(term)
            }
            Air::Fn {
                params,
                allow_inline,
            } => {
                let mut term = arg_stack.pop().unwrap();

                for param in params.iter().rev() {
                    term = term.lambda(param);
                }
                term = if allow_inline {
                    term
                } else {
                    term.lambda(NO_INLINE)
                };

                if params.is_empty() {
                    Some(term.delay())
                } else {
                    Some(term)
                }
            }
            Air::Call { count, .. } => {
                if count >= 1 {
                    let mut term = arg_stack.pop().unwrap();

                    for _ in 0..count {
                        let arg = arg_stack.pop().unwrap();

                        term = term.apply(arg);
                    }
                    Some(term)
                } else {
                    let term = arg_stack.pop().unwrap();

                    // How we handle zero arg anon functions has changed
                    // We now delay zero arg anon functions and force them on a call operation
                    match &term {
                        Term::Var(name) => {
                            let zero_arg_functions = self.zero_arg_functions.clone();
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
                                    let name_module =
                                        format!("{module_name}_{function_name}{variant}");
                                    let name = format!("{function_name}{variant}");

                                    text == &name || text == &name_module
                                },
                            ) {
                                let mut term = self.uplc_code_gen(air_vec.clone());

                                term = term.constr_fields_exposer().constr_index_exposer();

                                let mut program: Program<Name> = Program {
                                    version: (1, 0, 0),
                                    term: self.special_functions.apply_used_functions(term),
                                };

                                let mut interner = CodeGenInterner::new();

                                interner.program(&mut program);

                                let eval_program: Program<NamedDeBruijn> =
                                    program.remove_no_inlines().try_into().unwrap();

                                let result = eval_program.eval(ExBudget::max()).result();

                                let evaluated_term: Term<NamedDeBruijn> = result.unwrap_or_else(|e| {
                                    panic!("Evaluated a zero argument function and received this error: {e:#?}")
                                });

                                Some(evaluated_term.try_into().unwrap())
                            } else {
                                Some(term.force())
                            }
                        }
                        Term::Delay(inner_term) => Some(inner_term.as_ref().clone()),
                        Term::Apply { .. } => Some(term.force()),
                        _ => unreachable!(
                            "Shouldn't call anything other than var or apply\n{:#?}",
                            term
                        ),
                    }
                }
            }
            Air::Builtin { func, tipo, count } => {
                let mut arg_vec = vec![];
                for _ in 0..count {
                    arg_vec.push(arg_stack.pop().unwrap());
                }

                let ret_tipo = match tipo.as_ref() {
                    Type::Fn { ret, .. } => ret,
                    // In this case the Air Opcode only holds the return type and not the function type
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

                    DefaultFunction::FstPair | DefaultFunction::SndPair => {
                        builder::undata_builtin(&func, count, ret_tipo, arg_vec)
                    }

                    DefaultFunction::HeadList if !tipo.is_pair() => {
                        builder::undata_builtin(&func, count, ret_tipo, arg_vec)
                    }

                    DefaultFunction::MkCons | DefaultFunction::MkPairData => {
                        unimplemented!(
                            "MkCons and MkPairData should be handled by an anon function or using [] or ( a, b, .., z).\n"
                        )
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

                Some(term)
            }
            Air::BinOp {
                name,
                // changed this to argument tipo
                argument_tipo: tipo,
                ..
            } => {
                let left = arg_stack.pop().unwrap();
                let right = arg_stack.pop().unwrap();

                let uplc_type = tipo.get_uplc_type();

                let term = match name {
                    BinOp::And => left.delayed_if_then_else(right, Term::bool(false)),
                    BinOp::Or => left.delayed_if_then_else(Term::bool(true), right),
                    BinOp::Eq | BinOp::NotEq => {
                        let builtin = match &uplc_type {
                            Some(UplcType::Integer) => Term::equals_integer(),
                            Some(UplcType::String) => Term::equals_string(),
                            Some(UplcType::ByteString) => Term::equals_bytestring(),
                            Some(UplcType::Bls12_381G1Element) => Term::bls12_381_g1_equal(),
                            Some(UplcType::Bls12_381G2Element) => Term::bls12_381_g2_equal(),
                            Some(UplcType::Bool | UplcType::Unit) => Term::unit(),
                            Some(UplcType::List(_) | UplcType::Pair(_, _) | UplcType::Data)
                            | None => Term::equals_data(),
                            Some(UplcType::Bls12_381MlResult) => {
                                panic!("ML Result equality is not supported")
                            }
                        };

                        let binop_eq =
                            match uplc_type {
                                Some(UplcType::Bool) => {
                                    if matches!(name, BinOp::Eq) {
                                        left.delayed_if_then_else(
                                            right.clone(),
                                            right.if_then_else(Term::bool(false), Term::bool(true)),
                                        )
                                    } else {
                                        left.delayed_if_then_else(
                                            right
                                                .clone()
                                                .if_then_else(Term::bool(false), Term::bool(true)),
                                            right,
                                        )
                                    }
                                }
                                Some(UplcType::List(_)) if tipo.is_map() => builtin
                                    .apply(Term::map_data().apply(left))
                                    .apply(Term::map_data().apply(right)),
                                Some(UplcType::List(_)) => builtin
                                    .apply(Term::list_data().apply(left))
                                    .apply(Term::list_data().apply(right)),
                                Some(UplcType::Pair(_, _)) => builtin
                                    .apply(Term::map_data().apply(
                                        Term::mk_cons().apply(left).apply(Term::empty_map()),
                                    ))
                                    .apply(Term::map_data().apply(
                                        Term::mk_cons().apply(right).apply(Term::empty_map()),
                                    )),
                                Some(
                                    UplcType::Data
                                    | UplcType::Bls12_381G1Element
                                    | UplcType::Bls12_381G2Element
                                    | UplcType::Bls12_381MlResult
                                    | UplcType::Integer
                                    | UplcType::String
                                    | UplcType::ByteString,
                                )
                                | None => builtin.apply(left).apply(right),
                                Some(UplcType::Unit) => {
                                    left.choose_unit(right.choose_unit(Term::bool(true)))
                                }
                            };

                        if !tipo.is_bool() && matches!(name, BinOp::NotEq) {
                            binop_eq.if_then_else(Term::bool(false), Term::bool(true))
                        } else {
                            binop_eq
                        }
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
                Some(term)
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
                    term = term.lambda(func_name).apply(func_body.lambda(NO_INLINE));

                    Some(term)
                } else {
                    func_body = func_body.lambda(func_name.clone());

                    if recursive_nonstatic_params == params {
                        // If we don't have any recursive-static params, we can just emit the function as is
                        term = term
                            .lambda(func_name.clone())
                            .apply(Term::var(func_name.clone()).apply(Term::var(func_name.clone())))
                            .lambda(func_name)
                            .apply(func_body.lambda(NO_INLINE));
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
                        term = term
                            .lambda(&func_name)
                            .apply(outer_func_body.lambda(NO_INLINE));
                    }

                    Some(term)
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
                    if params.is_empty() {
                        function = function.delay();
                    } else {
                        for param in params.iter().rev() {
                            function = function.lambda(param);
                        }
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
                    .apply(
                        cyclic_body
                            .lambda("__chooser")
                            .lambda(func_name)
                            .lambda(NO_INLINE),
                    );

                Some(term)
            }
            Air::Let { name } => {
                let arg = arg_stack.pop().unwrap();

                let mut term = arg_stack.pop().unwrap();

                term = term.lambda(name).apply(arg);

                Some(term)
            }
            Air::CastFromData { tipo, full_cast } => {
                let mut term = arg_stack.pop().unwrap();

                let otherwise = if full_cast {
                    arg_stack.pop().unwrap()
                } else {
                    Term::Error.delay()
                };

                term = if full_cast {
                    convert_data_to_type(term, &tipo, otherwise)
                } else {
                    known_data_to_type(term, &tipo)
                };

                if extract_constant(&term).is_some() {
                    let mut program: Program<Name> = Program {
                        version: (1, 0, 0),
                        term,
                    };

                    let mut interner = CodeGenInterner::new();

                    interner.program(&mut program);

                    let eval_program: Program<NamedDeBruijn> =
                        program.remove_no_inlines().try_into().unwrap();

                    let evaluated_term: Term<NamedDeBruijn> = eval_program
                        .eval(ExBudget::default())
                        .result()
                        .expect("Evaluated on unwrapping a data constant and got an error");

                    term = evaluated_term.try_into().unwrap();
                }

                Some(term)
            }
            Air::CastToData { tipo } => {
                let mut term = arg_stack.pop().unwrap();

                if extract_constant(&term).is_some() {
                    term = builder::convert_type_to_data(term, &tipo);

                    let mut program: Program<Name> = Program {
                        version: (1, 0, 0),
                        term,
                    };

                    let mut interner = CodeGenInterner::new();

                    interner.program(&mut program);

                    let eval_program: Program<NamedDeBruijn> =
                        program.remove_no_inlines().try_into().unwrap();

                    let evaluated_term: Term<NamedDeBruijn> = eval_program
                        .eval(ExBudget::default())
                        .result()
                        .expect("Evaluated on wrapping a constant into data and got an error");

                    term = evaluated_term.try_into().unwrap();
                } else {
                    term = builder::convert_type_to_data(term, &tipo);
                }

                Some(term)
            }
            Air::AssertConstr { constr_index } => {
                let constr = arg_stack.pop().unwrap();

                let mut term = arg_stack.pop().unwrap();
                let otherwise = arg_stack.pop().unwrap();

                term = Term::equals_integer()
                    .apply(Term::integer(constr_index.into()))
                    .apply(constr)
                    .if_then_else(term.delay(), otherwise)
                    .force();

                Some(term)
            }
            Air::AssertBool { is_true } => {
                let value = arg_stack.pop().unwrap();

                let mut term = arg_stack.pop().unwrap();
                let otherwise = arg_stack.pop().unwrap();

                if is_true {
                    term = value.if_then_else(term.delay(), otherwise).force()
                } else {
                    term = value.if_then_else(otherwise, term.delay()).force()
                }
                Some(term)
            }
            Air::When {
                subject_name,
                // using subject type here
                subject_tipo: tipo,
                ..
            } => {
                let subject = arg_stack.pop().unwrap();

                let uplc_type = tipo.get_uplc_type();

                let subject = match uplc_type {
                    Some(
                        UplcType::Bool
                        | UplcType::Integer
                        | UplcType::String
                        | UplcType::ByteString
                        | UplcType::Unit
                        | UplcType::List(_)
                        | UplcType::Pair(_, _)
                        | UplcType::Bls12_381G1Element
                        | UplcType::Bls12_381G2Element
                        | UplcType::Bls12_381MlResult,
                    ) => subject,

                    Some(UplcType::Data) => subject,

                    None => Term::var(
                        self.special_functions
                            .use_function_uplc(CONSTR_INDEX_EXPOSER.to_string()),
                    )
                    .apply(subject),
                };

                let mut term = arg_stack.pop().unwrap();

                term = term.lambda(subject_name).apply(subject);

                Some(term)
            }
            Air::Clause {
                subject_tipo: tipo,
                subject_name,
                complex_clause,
            } => {
                // clause to compare
                let clause = arg_stack.pop().unwrap();

                // the body to be run if the clause matches
                let body = arg_stack.pop().unwrap();

                // the next branch in the when expression
                let term = arg_stack.pop().unwrap();

                let other_clauses = if complex_clause {
                    Term::var("__other_clauses_delayed")
                } else {
                    term.clone().delay()
                };

                let body = if tipo.is_bool() {
                    if matches!(clause, Term::Constant(boolean) if matches!(boolean.as_ref(), UplcConstant::Bool(true)))
                    {
                        Term::var(subject_name)
                            .if_then_else(body.delay(), other_clauses)
                            .force()
                    } else {
                        Term::var(subject_name)
                            .if_then_else(other_clauses, body.delay())
                            .force()
                    }
                } else {
                    let uplc_type = tipo.get_uplc_type();

                    let condition = match uplc_type {
                        Some(
                            UplcType::Bool
                            | UplcType::Unit
                            | UplcType::List(_)
                            | UplcType::Pair(_, _)
                            | UplcType::Bls12_381MlResult,
                        ) => unreachable!("{:#?}", tipo),
                        Some(UplcType::Data) => unimplemented!(),
                        Some(UplcType::Integer) => Term::equals_integer()
                            .apply(clause)
                            .apply(Term::var(subject_name)),
                        Some(UplcType::String) => Term::equals_string()
                            .apply(clause)
                            .apply(Term::var(subject_name)),
                        Some(UplcType::ByteString) => Term::equals_bytestring()
                            .apply(clause)
                            .apply(Term::var(subject_name)),
                        Some(UplcType::Bls12_381G1Element) => Term::bls12_381_g1_equal()
                            .apply(clause)
                            .apply(Term::var(subject_name)),
                        Some(UplcType::Bls12_381G2Element) => Term::bls12_381_g2_equal()
                            .apply(clause)
                            .apply(Term::var(subject_name)),
                        None => Term::equals_integer()
                            .apply(clause)
                            .apply(Term::var(subject_name)),
                    };

                    condition.if_then_else(body.delay(), other_clauses).force()
                };

                if complex_clause {
                    Some(body.lambda("__other_clauses_delayed").apply(term.delay()))
                } else {
                    Some(body)
                }
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

                Some(term)
            }
            Air::WrapClause => {
                // no longer need to pop off discard
                let mut term = arg_stack.pop().unwrap();
                let arg = arg_stack.pop().unwrap();

                term = term.lambda("__other_clauses_delayed").apply(arg.delay());

                Some(term)
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

                for (index, name) in indices.iter() {
                    term = term.lambda(name.clone()).apply(builder::known_data_to_type(
                        Term::head_list()
                            .apply(Term::var(subject_name.clone()).repeat_tail_list(*index)),
                        &tuple_types[*index].clone(),
                    ));
                }

                Some(term)
            }
            Air::PairClause {
                subject_tipo: tipo,
                complex_clause,
                subject_name,
                fst_name,
                snd_name,
            } => {
                let mut term = arg_stack.pop().unwrap();

                let next_clause = arg_stack.pop().unwrap();

                let pair_types = tipo.get_inner_types();

                if complex_clause {
                    term = term
                        .lambda("__other_clauses_delayed")
                        .apply(next_clause.delay());
                }

                if let Some(fst) = fst_name {
                    term = term.lambda(fst).apply(builder::known_data_to_type(
                        Term::fst_pair().apply(Term::var(subject_name.clone())),
                        &pair_types[0].clone(),
                    ));
                }

                if let Some(snd) = snd_name {
                    term = term.lambda(snd).apply(builder::known_data_to_type(
                        Term::snd_pair().apply(Term::var(subject_name.clone())),
                        &pair_types[1].clone(),
                    ));
                }

                Some(term)
            }
            Air::ClauseGuard {
                subject_name,
                subject_tipo: tipo,
            } => {
                let checker = arg_stack.pop().unwrap();

                let then = arg_stack.pop().unwrap();

                let term = Term::var("__other_clauses_delayed");

                if tipo.is_bool() {
                    if matches!(checker, Term::Constant(boolean) if matches!(boolean.as_ref(), UplcConstant::Bool(true)))
                    {
                        Some(
                            Term::var(subject_name)
                                .if_then_else(then.delay(), term)
                                .force(),
                        )
                    } else {
                        Some(
                            Term::var(subject_name)
                                .if_then_else(term, then.delay())
                                .force(),
                        )
                    }
                } else if tipo.is_void() {
                    Some(then.lambda("_").apply(Term::var(subject_name)))
                } else {
                    let uplc_type = tipo.get_uplc_type();

                    let condition = match uplc_type {
                        Some(
                            UplcType::Bool
                            | UplcType::Unit
                            | UplcType::List(_)
                            | UplcType::Pair(_, _)
                            | UplcType::Bls12_381MlResult,
                        ) => unreachable!("{:#?}", tipo),
                        Some(UplcType::Data) => unimplemented!(),
                        Some(UplcType::Integer) => Term::equals_integer()
                            .apply(checker)
                            .apply(Term::var(subject_name)),
                        Some(UplcType::String) => Term::equals_string()
                            .apply(checker)
                            .apply(Term::var(subject_name)),
                        Some(UplcType::ByteString) => Term::equals_bytestring()
                            .apply(checker)
                            .apply(Term::var(subject_name)),
                        Some(UplcType::Bls12_381G1Element) => Term::bls12_381_g1_equal()
                            .apply(checker)
                            .apply(Term::var(subject_name)),
                        Some(UplcType::Bls12_381G2Element) => Term::bls12_381_g2_equal()
                            .apply(checker)
                            .apply(Term::var(subject_name)),
                        None => Term::equals_integer().apply(checker).apply(
                            Term::var(
                                self.special_functions
                                    .use_function_uplc(CONSTR_INDEX_EXPOSER.to_string()),
                            )
                            .apply(Term::var(subject_name)),
                        ),
                    };

                    Some(condition.if_then_else(then.delay(), term).force())
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

                Some(term)
            }
            Air::TupleGuard {
                subject_tipo: tipo,
                indices,
                subject_name,
            } => {
                let mut term = arg_stack.pop().unwrap();

                let tuple_types = tipo.get_inner_types();

                for (index, name) in indices.iter() {
                    term = term.lambda(name.clone()).apply(builder::known_data_to_type(
                        Term::head_list()
                            .apply(Term::var(subject_name.clone()).repeat_tail_list(*index)),
                        &tuple_types[*index].clone(),
                    ));
                }

                Some(term)
            }
            Air::PairGuard {
                subject_tipo: tipo,
                subject_name,
                fst_name,
                snd_name,
            } => {
                let mut term = arg_stack.pop().unwrap();

                let tuple_types = tipo.get_inner_types();

                if let Some(fst) = fst_name {
                    term = term.lambda(fst).apply(builder::known_data_to_type(
                        Term::fst_pair().apply(Term::var(subject_name.clone())),
                        &tuple_types[0].clone(),
                    ));
                }

                if let Some(snd) = snd_name {
                    term = term.lambda(snd).apply(builder::known_data_to_type(
                        Term::snd_pair().apply(Term::var(subject_name.clone())),
                        &tuple_types[1].clone(),
                    ));
                }

                Some(term)
            }
            Air::Finally => {
                let _clause = arg_stack.pop().unwrap();

                None
            }
            Air::If { .. } => {
                let condition = arg_stack.pop().unwrap();
                let then = arg_stack.pop().unwrap();
                let mut term = arg_stack.pop().unwrap();

                term = condition.delayed_if_then_else(then, term);

                Some(term)
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

                    let mut interner = CodeGenInterner::new();

                    interner.program(&mut program);

                    let eval_program: Program<NamedDeBruijn> =
                        program.remove_no_inlines().try_into().unwrap();

                    let evaluated_term: Term<NamedDeBruijn> = eval_program
                        .eval(ExBudget::default())
                        .result()
                        .expect("Evaluated a constant record with args and got an error");

                    term = evaluated_term.try_into().unwrap();
                }

                Some(term)
            }
            Air::FieldsExpose { indices, is_expect } => {
                let mut id_list = vec![];

                let value = arg_stack.pop().unwrap();

                let mut term = arg_stack.pop().unwrap();

                let otherwise = if is_expect {
                    arg_stack.pop().unwrap()
                } else {
                    Term::Error.delay()
                };

                let list_id = self.id_gen.next();

                id_list.push(list_id);

                indices.iter().for_each(|_| {
                    id_list.push(self.id_gen.next());
                });

                let names_types = indices
                    .iter()
                    .cloned()
                    .zip(id_list)
                    .map(|(item, id)| (item.1, item.2, id))
                    .collect_vec();

                let named_indices = names_types
                    .iter()
                    .skip_while(|(name, _, _)| name == "_")
                    .collect_vec();

                if !named_indices.is_empty() || is_expect {
                    term = builder::list_access_to_uplc(
                        &names_types,
                        false,
                        term,
                        false,
                        is_expect.into(),
                        otherwise,
                    );

                    term = term.apply(
                        Term::var(
                            self.special_functions
                                .use_function_uplc(CONSTR_FIELDS_EXPOSER.to_string()),
                        )
                        .apply(value),
                    );

                    Some(term)
                } else {
                    Some(term)
                }
            }
            Air::FieldsEmpty => {
                let value = arg_stack.pop().unwrap();

                let mut term = arg_stack.pop().unwrap();
                let otherwise = arg_stack.pop().unwrap();

                term = Term::var(
                    self.special_functions
                        .use_function_uplc(CONSTR_FIELDS_EXPOSER.to_string()),
                )
                .apply(value)
                .choose_list(term.delay(), otherwise)
                .force();

                Some(term)
            }
            Air::ListEmpty => {
                let value = arg_stack.pop().unwrap();

                let mut term = arg_stack.pop().unwrap();
                let otherwise = arg_stack.pop().unwrap();

                term = value.choose_list(term.delay(), otherwise).force();

                Some(term)
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

                    let term = Term::Constant(
                        UplcConstant::ProtoList(UplcType::Data, data_constants).into(),
                    );
                    Some(term)
                } else {
                    let mut term = Term::empty_list();
                    for (arg, tipo) in args.into_iter().zip(tuple_sub_types.into_iter()).rev() {
                        term = Term::mk_cons()
                            .apply(builder::convert_type_to_data(arg, &tipo))
                            .apply(term);
                    }
                    Some(term)
                }
            }
            Air::Pair { tipo } => {
                let fst = arg_stack.pop().unwrap();
                let snd = arg_stack.pop().unwrap();

                match (extract_constant(&fst), extract_constant(&snd)) {
                    (Some(fst), Some(snd)) => {
                        let mut pair_fields = builder::convert_constants_to_data(vec![fst, snd]);
                        let term = Term::Constant(
                            UplcConstant::ProtoPair(
                                UplcType::Data,
                                UplcType::Data,
                                pair_fields.remove(0).into(),
                                pair_fields.remove(0).into(),
                            )
                            .into(),
                        );
                        Some(term)
                    }
                    _ => {
                        let term = Term::mk_pair_data()
                            .apply(builder::convert_type_to_data(
                                fst,
                                &tipo.get_inner_types()[0],
                            ))
                            .apply(builder::convert_type_to_data(
                                snd,
                                &tipo.get_inner_types()[1],
                            ));

                        Some(term)
                    }
                }
            }
            Air::RecordUpdate {
                highest_index,
                indices,
                tipo,
            } => {
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

                term = term.lambda(format!("{tail_name_prefix}_0")).apply(
                    Term::var(
                        self.special_functions
                            .use_function_uplc(CONSTR_FIELDS_EXPOSER.to_string()),
                    )
                    .apply(record),
                );

                Some(term)
            }
            Air::UnOp { op } => {
                let value = arg_stack.pop().unwrap();

                let term = match op {
                    UnOp::Not => value.if_then_else(Term::bool(false), Term::bool(true)),
                    UnOp::Negate => {
                        if let Term::Constant(c) = &value {
                            if let UplcConstant::Integer(i) = c.as_ref() {
                                Term::integer(-i)
                            } else {
                                Term::subtract_integer()
                                    .apply(Term::integer(0.into()))
                                    .apply(value)
                            }
                        } else {
                            Term::subtract_integer()
                                .apply(Term::integer(0.into()))
                                .apply(value)
                        }
                    }
                };

                Some(term)
            }
            Air::TupleAccessor {
                tipo,
                names,
                is_expect,
            } => {
                let inner_types = tipo.get_inner_types();
                let value = arg_stack.pop().unwrap();

                let mut term = arg_stack.pop().unwrap();
                let otherwise = if is_expect {
                    arg_stack.pop().unwrap()
                } else {
                    Term::Error.delay()
                };
                let list_id = self.id_gen.next();

                let mut id_list = vec![];
                id_list.push(list_id);

                names.iter().for_each(|_| {
                    id_list.push(self.id_gen.next());
                });

                let names_types = names
                    .into_iter()
                    .zip(inner_types)
                    .zip(id_list)
                    .map(|((name, tipo), id)| (name, tipo, id))
                    .collect_vec();

                term = builder::list_access_to_uplc(
                    &names_types,
                    false,
                    term,
                    false,
                    is_expect.into(),
                    otherwise,
                )
                .apply(value);

                Some(term)
            }
            Air::PairAccessor {
                fst,
                snd,
                tipo,
                is_expect,
            } => {
                let inner_types = tipo.get_inner_types();
                let value = arg_stack.pop().unwrap();

                let mut term = arg_stack.pop().unwrap();
                let otherwise = if is_expect {
                    arg_stack.pop().unwrap()
                } else {
                    Term::Error.delay()
                };

                let list_id = self.id_gen.next();

                if let Some(name) = snd {
                    term = term.lambda(name).apply(if is_expect {
                        convert_data_to_type(
                            Term::snd_pair().apply(Term::var(format!("__pair_{list_id}"))),
                            &inner_types[1],
                            otherwise.clone(),
                        )
                    } else {
                        known_data_to_type(
                            Term::snd_pair().apply(Term::var(format!("__pair_{list_id}"))),
                            &inner_types[1],
                        )
                    });
                }

                if let Some(name) = fst {
                    term = term.lambda(name).apply(if is_expect {
                        convert_data_to_type(
                            Term::fst_pair().apply(Term::var(format!("__pair_{list_id}"))),
                            &inner_types[0],
                            otherwise,
                        )
                    } else {
                        known_data_to_type(
                            Term::fst_pair().apply(Term::var(format!("__pair_{list_id}"))),
                            &inner_types[0],
                        )
                    })
                }

                term = term.lambda(format!("__pair_{list_id}")).apply(value);

                Some(term)
            }
            Air::Trace { .. } => {
                let text = arg_stack.pop().unwrap();

                let term = arg_stack.pop().unwrap();

                let term = term.delayed_trace(text);

                Some(term)
            }
            Air::ErrorTerm { validator, .. } => {
                if validator {
                    Some(Term::Error.apply(Term::Error.force()))
                } else {
                    Some(Term::Error)
                }
            }

            Air::NoOp => None,
        }
    }
}
