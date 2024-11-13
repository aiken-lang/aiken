pub mod air;
pub mod builder;
pub mod decision_tree;
pub mod interner;
pub mod stick_break_set;
pub mod tree;

use self::{
    air::Air,
    builder::{
        cast_validator_args, convert_type_to_data, extract_constant, modify_cyclic_calls,
        modify_self_calls, AssignmentProperties, CodeGenSpecialFuncs, CycleFunctionNames,
        HoistableFunction, Variant,
    },
    tree::{AirTree, TreePath},
};
use crate::{
    ast::{
        AssignmentKind, BinOp, Bls12_381Point, Curve, DataTypeKey, FunctionAccessKey, Pattern,
        Span, TraceLevel, Tracing, TypedArg, TypedDataType, TypedFunction, TypedPattern,
        TypedValidator, UnOp,
    },
    builtins::PRELUDE,
    expr::TypedExpr,
    gen_uplc::{
        air::ExpectLevel,
        builder::{
            erase_opaque_type_operations, get_generic_variant_name, get_line_columns_by_span,
            get_src_code_by_span, known_data_to_type, monomorphize, wrap_validator_condition,
            CodeGenFunction,
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
use builder::{
    introduce_name, introduce_pattern, pop_pattern, softcast_data_to_type_otherwise,
    unknown_data_to_type, DISCARDED,
};

use decision_tree::{get_tipo_by_path, Assigned, CaseTest, DecisionTree, TreeGen};
use indexmap::IndexMap;
use interner::AirInterner;
use itertools::Itertools;
use petgraph::{algo, Graph};
use std::{collections::HashMap, rc::Rc};
use stick_break_set::{Builtins, TreeSet};
use tree::Fields;
use uplc::{
    ast::{Constant as UplcConstant, Name, NamedDeBruijn, Program, Term, Type as UplcType},
    builder::{CONSTR_FIELDS_EXPOSER, CONSTR_INDEX_EXPOSER, EXPECT_ON_LIST},
    builtins::DefaultFunction,
    machine::cost_model::ExBudget,
    optimize::{aiken_optimize_and_intern, interner::CodeGenInterner, shrinker::NO_INLINE},
};

type Otherwise = Option<AirTree>;

const DELAY_ERROR: fn() -> AirTree =
    || AirTree::anon_func(vec![], AirTree::error(Type::void(), false), true);

#[derive(Clone)]
pub struct CodeGenerator<'a> {
    #[allow(dead_code)]
    plutus_version: PlutusVersion,
    /// immutable index maps
    functions: IndexMap<&'a FunctionAccessKey, &'a TypedFunction>,
    constants: IndexMap<&'a FunctionAccessKey, &'a TypedExpr>,
    data_types: IndexMap<&'a DataTypeKey, &'a TypedDataType>,
    module_types: IndexMap<&'a str, &'a TypeInfo>,
    module_src: IndexMap<&'a str, &'a (String, LineNumbers)>,
    /// immutable option
    tracing: TraceLevel,
    /// mutable index maps that are reset
    defined_functions: IndexMap<FunctionAccessKey, ()>,
    special_functions: CodeGenSpecialFuncs,
    code_gen_functions: IndexMap<String, CodeGenFunction>,
    cyclic_functions:
        IndexMap<(FunctionAccessKey, Variant), (CycleFunctionNames, usize, FunctionAccessKey)>,
    /// mutable and reset as well
    interner: AirInterner,
    id_gen: IdGenerator,
}

impl<'a> CodeGenerator<'a> {
    pub fn data_types(&self) -> &IndexMap<&'a DataTypeKey, &'a TypedDataType> {
        &self.data_types
    }

    pub fn new(
        plutus_version: PlutusVersion,
        functions: IndexMap<&'a FunctionAccessKey, &'a TypedFunction>,
        constants: IndexMap<&'a FunctionAccessKey, &'a TypedExpr>,
        data_types: IndexMap<&'a DataTypeKey, &'a TypedDataType>,
        module_types: IndexMap<&'a str, &'a TypeInfo>,
        module_src: IndexMap<&'a str, &'a (String, LineNumbers)>,
        tracing: Tracing,
    ) -> Self {
        CodeGenerator {
            plutus_version,
            functions,
            constants,
            data_types,
            module_types,
            module_src,
            tracing: tracing.trace_level(true),
            defined_functions: IndexMap::new(),
            special_functions: CodeGenSpecialFuncs::new(),
            code_gen_functions: IndexMap::new(),
            cyclic_functions: IndexMap::new(),
            interner: AirInterner::new(),
            id_gen: IdGenerator::new(),
        }
    }

    pub fn reset(&mut self, reset_special_functions: bool) {
        self.code_gen_functions = IndexMap::new();
        self.defined_functions = IndexMap::new();
        self.cyclic_functions = IndexMap::new();
        self.interner = AirInterner::new();
        self.id_gen = IdGenerator::new();
        if reset_special_functions {
            self.special_functions = CodeGenSpecialFuncs::new();
        }
    }

    pub fn generate(&mut self, validator: &TypedValidator, module_name: &str) -> Program<Name> {
        let context_name = "__context__".to_string();
        let context_name_interned = introduce_name(&mut self.interner, &context_name);
        validator.params.iter().for_each(|arg| {
            arg.get_variable_name()
                .iter()
                .for_each(|arg_name| self.interner.intern(arg_name.to_string()))
        });

        let air_tree_fun = wrap_validator_condition(
            self.build(&validator.into_script_context_handler(), module_name, &[]),
            self.tracing,
        );

        let air_tree_fun = AirTree::anon_func(vec![context_name_interned], air_tree_fun, true);

        let validator_args_tree = AirTree::no_op(air_tree_fun);

        let full_tree = self.hoist_functions_to_validator(validator_args_tree);

        // optimizations on air tree

        let full_vec = full_tree.to_vec();

        let term = self.uplc_code_gen(full_vec);

        let term = cast_validator_args(term, &validator.params, &self.interner);

        self.interner.pop_text(context_name);
        validator.params.iter().for_each(|arg| {
            arg.get_variable_name()
                .iter()
                .for_each(|arg_name| self.interner.pop_text(arg_name.to_string()))
        });

        self.finalize(term)
    }

    pub fn generate_raw(
        &mut self,
        body: &TypedExpr,
        args: &[TypedArg],
        module_name: &str,
    ) -> Program<Name> {
        args.iter().for_each(|arg| {
            arg.get_variable_name()
                .iter()
                .for_each(|arg_name| self.interner.intern(arg_name.to_string()))
        });

        let mut air_tree = self.build(body, module_name, &[]);

        air_tree = AirTree::no_op(air_tree);

        let full_tree = self.hoist_functions_to_validator(air_tree);

        // optimizations on air tree
        let full_vec = full_tree.to_vec();

        let mut term = self.uplc_code_gen(full_vec);

        term = if args.is_empty() {
            term
        } else {
            cast_validator_args(term, args, &self.interner)
        };

        args.iter().for_each(|arg| {
            arg.get_variable_name()
                .iter()
                .for_each(|arg_name| self.interner.pop_text(arg_name.to_string()))
        });

        self.finalize(term)
    }

    fn new_program<T>(&self, term: Term<T>) -> Program<T> {
        let version = match self.plutus_version {
            PlutusVersion::V1 | PlutusVersion::V2 => (1, 0, 0),
            PlutusVersion::V3 => (1, 1, 0),
        };

        Program { version, term }
    }

    fn finalize(&mut self, mut term: Term<Name>) -> Program<Name> {
        term = self.special_functions.apply_used_functions(term);

        let program = aiken_optimize_and_intern(self.new_program(term));

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

                if msg_func_name.is_empty() {
                    None
                } else {
                    self.special_functions.insert_new_function(
                        msg_func_name.clone(),
                        Term::Error.delayed_trace(Term::string(msg)).delay(),
                        Type::void(),
                    );

                    Some(self.special_functions.use_function_tree(msg_func_name))
                }
            };

            // Intern vars from pattern here
            introduce_pattern(&mut self.interner, pattern);

            let (then, context) = context.split_first().unwrap();

            let then = self.build(then, module_build_name, context);

            let tree = self.assignment(
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
            );

            // Now pop off interned pattern
            pop_pattern(&mut self.interner, pattern);

            tree
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
                } => match constructor.variant {
                    ValueConstructorVariant::LocalVariable { .. } => {
                        AirTree::var(constructor.clone(), self.interner.lookup_interned(name), "")
                    }
                    _ => AirTree::var(constructor.clone(), name, ""),
                },

                TypedExpr::Fn { args, body, .. } => {
                    let params = args
                        .iter()
                        .map(|arg| {
                            arg.get_variable_name()
                                .map(|arg| introduce_name(&mut self.interner, &arg.to_string()))
                                .unwrap_or_else(|| DISCARDED.to_string())
                        })
                        .collect_vec();

                    let anon =
                        AirTree::anon_func(params, self.build(body, module_build_name, &[]), false);

                    args.iter()
                        .filter_map(|arg| arg.get_variable_name())
                        .for_each(|arg| {
                            self.interner.pop_text(arg.to_string());
                        });

                    anon
                }

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
                    subject,
                    clauses,
                    tipo,
                    ..
                } => {
                    if clauses.is_empty() {
                        unreachable!("We should have one clause at least")
                    // TODO: This whole branch can _probably_ be removed, if handle_each_clause
                    // works fine with an empty clauses list. This is orthogonal to the
                    // current refactoring so not changing it now.
                    } else if clauses.len() == 1 {
                        let subject_val = self.build(subject, module_build_name, &[]);

                        let last_clause = &clauses[0];

                        // Intern vars from pattern here
                        introduce_pattern(&mut self.interner, &last_clause.pattern);

                        let clause_then = self.build(&last_clause.then, module_build_name, &[]);

                        let subject_type = subject.tipo();

                        let tree = self.assignment(
                            &last_clause.pattern,
                            subject_val,
                            clause_then,
                            &subject_type,
                            AssignmentProperties {
                                value_type: subject.tipo(),
                                kind: AssignmentKind::let_(),
                                remove_unused: false,
                                full_check: false,
                                otherwise: None,
                            },
                        );

                        // Now pop off interned pattern
                        pop_pattern(&mut self.interner, &last_clause.pattern);

                        tree
                    } else {
                        let subject_name = format!(
                            "__subject_var_span_{}_{}",
                            subject.location().start,
                            subject.location().end
                        );

                        self.interner.intern(subject_name.clone());

                        let subject_name_interned = self.interner.lookup_interned(&subject_name);

                        let wild_card = TypedPattern::Discard {
                            name: "".to_string(),
                            location: Span::empty(),
                        };

                        let tree_gen =
                            TreeGen::new(&mut self.interner, &self.data_types, &wild_card);

                        let tree = tree_gen.build_tree(&subject.tipo(), clauses);

                        let stick_set = TreeSet::new();

                        let clauses = self.handle_decision_tree(
                            &subject_name_interned,
                            subject.tipo(),
                            tipo.clone(),
                            module_build_name,
                            tree,
                            stick_set,
                        );

                        self.interner.pop_text(subject_name);

                        AirTree::let_assignment(
                            subject_name_interned,
                            self.build(subject, module_build_name, &[]),
                            clauses,
                        )
                    }
                }

                TypedExpr::If {
                    branches,
                    final_else,
                    tipo,
                    ..
                } => {
                    branches.iter().rfold(
                        self.build(final_else, module_build_name, &[]),
                        |acc, branch| {
                            let condition = self.build(&branch.condition, module_build_name, &[]);

                            match &branch.is {
                                Some((pattern, tipo)) => {
                                    introduce_pattern(&mut self.interner, pattern);
                                    self.interner.intern("acc_var".to_string());

                                    let body = self.build(&branch.body, module_build_name, &[]);

                                    let acc_var =
                                        self.interner.lookup_interned(&"acc_var".to_string());

                                    let tree = AirTree::let_assignment(
                                        &acc_var,
                                        // use anon function as a delay to avoid evaluating the acc
                                        AirTree::anon_func(vec![], acc, true),
                                        self.assignment(
                                            pattern,
                                            condition,
                                            body,
                                            tipo,
                                            AssignmentProperties {
                                                value_type: branch.condition.tipo(),
                                                kind: AssignmentKind::Expect { backpassing: () },
                                                remove_unused: false,
                                                full_check: true,
                                                otherwise: Some(AirTree::local_var(
                                                    &acc_var,
                                                    tipo.clone(),
                                                )),
                                            },
                                        ),
                                    );

                                    pop_pattern(&mut self.interner, pattern);
                                    self.interner.pop_text("acc_var".to_string());

                                    tree
                                }
                                None => AirTree::if_branch(
                                    tipo.clone(),
                                    condition,
                                    self.build(&branch.body, module_build_name, &[]),
                                    acc,
                                ),
                            }
                        },
                    )
                }
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
                            let mut body = AirTree::local_var("__fields", Type::list(Type::data()));

                            for _ in 0..*index {
                                body = AirTree::builtin(
                                    DefaultFunction::TailList,
                                    Type::list(Type::data()),
                                    vec![body],
                                )
                            }

                            body = AirTree::builtin(
                                DefaultFunction::HeadList,
                                Type::data(),
                                vec![body],
                            );

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
                            Type::list(Type::data()),
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
                            // NOTE: This is needed because we register prelude functions under an
                            // empty module name. This is to facilitate their access when used
                            // directly. Note that, if we weren't doing this particular
                            // transformation, we would need to do the other direction anyway:
                            //
                            //     if module_name.is_empty() { PRELUDE.to_string() } else { module_name.clone() }
                            //
                            // So either way, we need to take care of this.
                            module_name: if module_name == PRELUDE {
                                String::new()
                            } else {
                                module_name.clone()
                            },
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
                    ModuleValueConstructor::Constant { module, name, .. } => {
                        let type_info = self.module_types.get(module_name.as_str()).unwrap();

                        let value = type_info.values.get(name).unwrap();

                        AirTree::var(
                            ValueConstructor::public(tipo.clone(), value.variant.clone()),
                            format!("{module}_{name}"),
                            "",
                        )
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
                            let mut body = AirTree::local_var("__fields", Type::list(Type::data()));

                            for _ in 0..*index {
                                body = AirTree::builtin(
                                    DefaultFunction::TailList,
                                    Type::list(Type::data()),
                                    vec![body],
                                )
                            }

                            body = AirTree::builtin(
                                DefaultFunction::HeadList,
                                Type::data(),
                                vec![body],
                            );

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
        value: AirTree,
        then: AirTree,
        tipo: &Rc<Type>,
        props: AssignmentProperties,
    ) -> AirTree {
        assert!(
            match &value {
                AirTree::Var { name, .. } if props.kind.is_let() => {
                    name != DISCARDED
                }
                _ => true,
            },
            "No discard expressions or let bindings should be in the tree at this point."
        );

        // Cast value to or from data so we don't have to worry from this point onward
        let assign_casted_value = |name, value, then| {
            if props.value_type.is_data() && props.kind.is_expect() && !tipo.is_data() {
                if props.otherwise.is_some() {
                    AirTree::soft_cast_assignment(
                        name,
                        tipo.clone(),
                        value,
                        then,
                        props.otherwise.as_ref().unwrap().clone(),
                    )
                } else {
                    AirTree::let_assignment(
                        name,
                        AirTree::cast_from_data(value, tipo.clone(), true),
                        then,
                    )
                }
            } else if !props.value_type.is_data() && tipo.is_data() {
                AirTree::let_assignment(
                    name,
                    AirTree::cast_to_data(value, props.value_type.clone()),
                    then,
                )
            } else {
                AirTree::let_assignment(name, value, then)
            }
        };

        let otherwise = match &props.otherwise {
            Some(x) => x.clone(),
            // (delay (error ))
            None => AirTree::anon_func(vec![], AirTree::error(Type::void(), false), true),
        };

        match pattern {
            Pattern::Int {
                value: expected_int,
                location,
                ..
            } => {
                let name = format!(
                    "__expected_by_{}_span_{}_{}",
                    expected_int, location.start, location.end
                );

                let expect = AirTree::binop(
                    BinOp::Eq,
                    Type::bool(),
                    AirTree::int(expected_int),
                    AirTree::local_var(&name, Type::int()),
                    Type::int(),
                );

                assign_casted_value(
                    name,
                    value,
                    AirTree::assert_bool(true, expect, then, otherwise),
                )
            }

            Pattern::ByteArray {
                value: expected_bytes,
                location,
                ..
            } => {
                let name = format!("__expected_bytes_span_{}_{}", location.start, location.end);

                let expect = AirTree::binop(
                    BinOp::Eq,
                    Type::bool(),
                    AirTree::byte_array(expected_bytes.clone()),
                    AirTree::local_var(&name, Type::byte_array()),
                    Type::byte_array(),
                );

                assign_casted_value(
                    name,
                    value,
                    AirTree::assert_bool(true, expect, then, otherwise),
                )
            }

            Pattern::Var { name, .. } => {
                let name = self.interner.lookup_interned(name);

                if props.full_check {
                    let mut index_map = IndexMap::new();

                    let non_opaque_tipo = convert_opaque_type(tipo, &self.data_types, true);

                    let val = AirTree::local_var(&name, tipo.clone());

                    if non_opaque_tipo.is_primitive() {
                        assign_casted_value(name.clone(), value, then)
                    } else {
                        assign_casted_value(
                            name,
                            value,
                            self.expect_type_assign(
                                &non_opaque_tipo,
                                val,
                                &mut index_map,
                                pattern.location(),
                                then,
                                props.otherwise.clone(),
                            ),
                        )
                    }
                } else {
                    assign_casted_value(name.clone(), value, then)
                }
            }

            Pattern::Assign { name, pattern, .. } => {
                let name = self.interner.lookup_interned(name);
                // Don't need any data casting for Assign
                let inner_pattern = self.assignment(
                    pattern,
                    AirTree::local_var(&name, tipo.clone()),
                    then,
                    tipo,
                    props,
                );
                AirTree::let_assignment(name, value, inner_pattern)
            }

            Pattern::Discard { name, .. } => {
                if props.full_check {
                    let name = format!("__discard_expect_{}", name);

                    let name_interned = introduce_name(&mut self.interner, &name);

                    let mut index_map = IndexMap::new();

                    let non_opaque_tipo = convert_opaque_type(tipo, &self.data_types, true);

                    let val = AirTree::local_var(&name_interned, tipo.clone());

                    let tree = if non_opaque_tipo.is_primitive() {
                        assign_casted_value(name_interned, value, then)
                    } else {
                        assign_casted_value(
                            name_interned,
                            value,
                            self.expect_type_assign(
                                &non_opaque_tipo,
                                val,
                                &mut index_map,
                                pattern.location(),
                                then,
                                props.otherwise.clone(),
                            ),
                        )
                    };

                    self.interner.pop_text(name);

                    tree
                } else if !props.remove_unused {
                    //No need to intern, name not used
                    assign_casted_value(name.clone(), value, then)
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
                        let (tail_name, tail_name_interned) = match tail.as_ref() {
                            Pattern::Var { name, .. } => {
                                (None, self.interner.lookup_interned(name))
                            }
                            // This Pattern one doesn't even make sense
                            Pattern::Assign { .. } => {
                                todo!("Has this ever been reached before?")
                            }
                            Pattern::Discard { name, .. } => {
                                if props.full_check {
                                    (
                                        Some(format!("__discard_{}_tail", name)),
                                        introduce_name(
                                            &mut self.interner,
                                            &format!("__discard_{}_tail", name),
                                        ),
                                    )
                                } else {
                                    (None, DISCARDED.to_string())
                                }
                            }
                            _ => unreachable!(),
                        };

                        let val = AirTree::local_var(&tail_name_interned, tipo.clone());

                        let then = if tail_name_interned != DISCARDED {
                            self.assignment(
                                tail,
                                val,
                                then,
                                tipo,
                                AssignmentProperties {
                                    value_type: tipo.clone(),
                                    kind: props.kind,
                                    // The reason the top level of recursion might have remove_unused
                                    // false is to deal with expect _ = thing
                                    //                       next_thing
                                    remove_unused: true,
                                    full_check: props.full_check,
                                    otherwise: props.otherwise.clone(),
                                },
                            )
                        } else {
                            then
                        };

                        elems.push(tail_name_interned);

                        if let Some(tail_name) = tail_name {
                            self.interner.pop_text(tail_name);
                        }

                        then
                    }
                };

                let then = elements
                    .iter()
                    .enumerate()
                    .rfold(then, |then, (index, elem)| {
                        let (elem_name, elem_name_interned) = match elem {
                            Pattern::Var { name, .. } => {
                                (None, self.interner.lookup_interned(name))
                            }
                            Pattern::Assign { name, .. } => {
                                (None, self.interner.lookup_interned(name))
                            }
                            Pattern::Discard { name, .. } => {
                                if props.full_check {
                                    (
                                        Some(format!("__discard_{}_{}", name, index)),
                                        introduce_name(
                                            &mut self.interner,
                                            &format!("__discard_{}_{}", name, index),
                                        ),
                                    )
                                } else {
                                    (None, DISCARDED.to_string())
                                }
                            }
                            _ => {
                                let name = format!(
                                    "elem_{}_span_{}_{}",
                                    index,
                                    elem.location().start,
                                    elem.location().end
                                );
                                let interned = introduce_name(&mut self.interner, &name);

                                (Some(name), interned)
                            }
                        };

                        let val = AirTree::local_var(&elem_name_interned, list_elem_type.clone());

                        let then = if elem_name_interned != DISCARDED {
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

                        elems.push(elem_name_interned);

                        if let Some(elem_name) = elem_name {
                            self.interner.pop_text(elem_name);
                        }

                        then
                    });

                elems.reverse();

                let name = format!(
                    "__List_span_{}_{}",
                    pattern.location().start,
                    pattern.location().end
                );

                let name_interned = introduce_name(&mut self.interner, &name);

                let casted_var = AirTree::local_var(&name_interned, tipo.clone());

                let tree = if elements.is_empty() {
                    assign_casted_value(
                        name_interned,
                        value,
                        AirTree::list_empty(casted_var, then, otherwise),
                    )
                } else {
                    assign_casted_value(
                        name_interned,
                        value,
                        AirTree::list_access(
                            elems,
                            tipo.clone(),
                            tail.is_some(),
                            casted_var,
                            if props.full_check {
                                ExpectLevel::Full
                            } else {
                                ExpectLevel::Items
                            },
                            then,
                            otherwise,
                        ),
                    )
                };

                self.interner.pop_text(name);

                tree
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
                        let (field_name, field_name_interned) = match arg.as_ref() {
                            Pattern::Var { name, .. } => {
                                (None, self.interner.lookup_interned(name))
                            }
                            Pattern::Assign { name, .. } => {
                                (None, self.interner.lookup_interned(name))
                            }
                            Pattern::Discard { name, .. } => {
                                if props.full_check {
                                    (
                                        Some(format!("__discard_{}_{}", name, field_index)),
                                        introduce_name(
                                            &mut self.interner,
                                            &format!("__discard_{}_{}", name, field_index),
                                        ),
                                    )
                                } else {
                                    (None, DISCARDED.to_string())
                                }
                            }
                            _ => {
                                let name = format!(
                                    "field_{}_span_{}_{}",
                                    field_index,
                                    arg.location().start,
                                    arg.location().end
                                );
                                let interned = introduce_name(&mut self.interner, &name);

                                (Some(name), interned)
                            }
                        };

                        let arg_type = type_map.get(&field_index).unwrap_or_else(|| {
                            unreachable!("Missing type for field {} of Pair", field_index,)
                        });

                        let val = AirTree::local_var(&field_name_interned, arg_type.clone());

                        let then = if field_name_interned != DISCARDED {
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

                        fields.push((field_index, field_name_interned, arg_type.clone()));

                        if let Some(field_name) = field_name {
                            self.interner.pop_text(field_name);
                        }

                        then
                    });

                fields.reverse();

                // This `value` is either value param that was passed in or
                // local var
                let constructor_name = format!(
                    "Pair_span_{}_{}",
                    pattern.location().start,
                    pattern.location().end
                );

                let constructor_name_interned =
                    introduce_name(&mut self.interner, &constructor_name);

                let local_value = AirTree::local_var(&constructor_name_interned, tipo.clone());

                let then = AirTree::pair_access(
                    fields
                        .first()
                        .map(|x| {
                            if x.1 == DISCARDED {
                                None
                            } else {
                                Some(x.1.clone())
                            }
                        })
                        .unwrap(),
                    fields
                        .last()
                        .map(|x| {
                            if x.1 == DISCARDED {
                                None
                            } else {
                                Some(x.1.clone())
                            }
                        })
                        .unwrap(),
                    tipo.clone(),
                    local_value,
                    props.full_check,
                    then,
                    otherwise,
                );

                let tree = assign_casted_value(constructor_name_interned, value, then);

                self.interner.pop_text(constructor_name);

                tree
            }

            Pattern::Constructor {
                constructor: PatternConstructor::Record { name, .. },
                ..
            } if tipo.is_bool() => {
                assert!(props.kind.is_expect());

                let name_var = format!(
                    "__Bool_{}_{}",
                    pattern.location().start,
                    pattern.location().end
                );

                let local_var = AirTree::local_var(&name_var, tipo.clone());

                assign_casted_value(
                    name_var,
                    value,
                    AirTree::assert_bool(name == "True", local_var, then, otherwise),
                )
            }

            Pattern::Constructor { .. } if tipo.is_void() => {
                // Void type is checked when casting from data
                // So we just assign the value and move on
                assign_casted_value(DISCARDED.to_string(), value, then)
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

                assert!(
                    type_map.len() >= arguments.len(),
                    "type map len: {}, arguments len: {}; for constructor {:?}",
                    type_map.len(),
                    arguments.len(),
                    name,
                );

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

                        let (field_name, field_name_interned) = match &arg.value {
                            Pattern::Var { name, .. } => {
                                (None, self.interner.lookup_interned(name))
                            }
                            Pattern::Assign { name, .. } => {
                                (None, self.interner.lookup_interned(name))
                            }
                            Pattern::Discard { name, .. } => {
                                if props.full_check {
                                    (
                                        Some(format!("__discard_{}_{}", name, index)),
                                        introduce_name(
                                            &mut self.interner,
                                            &format!("__discard_{}_{}", name, index),
                                        ),
                                    )
                                } else {
                                    (None, DISCARDED.to_string())
                                }
                            }
                            _ => {
                                let name = format!(
                                    "field_{}_span_{}_{}",
                                    field_index,
                                    arg.value.location().start,
                                    arg.value.location().end
                                );
                                let interned = introduce_name(&mut self.interner, &name);

                                (Some(name), interned)
                            }
                        };

                        let arg_type = type_map.get(&field_index).unwrap_or_else(|| {
                            unreachable!(
                                "Missing type for field {} of constr {}",
                                field_index, name
                            )
                        });

                        let val = AirTree::local_var(&field_name_interned, arg_type.clone());

                        let then = if field_name_interned != DISCARDED {
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

                        fields.push((field_index, field_name_interned, arg_type.clone()));

                        if let Some(field_name) = field_name {
                            self.interner.pop_text(field_name);
                        }

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

                let constructor_name_interned =
                    introduce_name(&mut self.interner, &constructor_name);

                let subject_name_interned = introduce_name(&mut self.interner, &subject_name);

                let local_value = AirTree::local_var(&constructor_name_interned, tipo.clone());

                let then = if check_replaceable_opaque_type(tipo, &self.data_types) {
                    AirTree::let_assignment(&fields[0].1, local_value, then)
                } else {
                    AirTree::fields_expose(
                        fields,
                        local_value,
                        props.full_check,
                        then,
                        otherwise.clone(),
                    )
                };

                let data_type = lookup_data_type_by_tipo(&self.data_types, tipo)
                    .unwrap_or_else(|| unreachable!("Failed to find definition for {}", name));

                let then = if props.kind.is_expect()
                    && (data_type.constructors.len() > 1
                        || props.full_check
                        || data_type.is_never())
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
                        &subject_name_interned,
                        Type::void(),
                        tipo.clone(),
                        AirTree::local_var(&constructor_name_interned, tipo.clone()),
                        AirTree::clause(
                            &subject_name_interned,
                            AirTree::int(index),
                            tipo.clone(),
                            then,
                            otherwise,
                        ),
                    )
                } else {
                    assert!(
                        data_type.constructors.len() == 1 || data_type.is_never(),
                        "attempted let-assignment on a type with more or less than 1 constructor: \nis_expect? {}\nfull_check? {}\ndata_type={data_type:#?}\n{}",
                        props.kind.is_expect(),
                        props.full_check,
                        name,
                    );

                    then
                };

                let tree = assign_casted_value(constructor_name_interned, value, then);

                self.interner.pop_text(constructor_name);
                self.interner.pop_text(subject_name);

                tree
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
                    let (tuple_name, tuple_name_interned) = match &arg {
                        Pattern::Var { name, .. } => (None, self.interner.lookup_interned(name)),
                        Pattern::Assign { name, .. } => (None, self.interner.lookup_interned(name)),
                        Pattern::Discard { name, .. } => {
                            if props.full_check {
                                (
                                    Some(format!("__discard_{}_{}", name, index)),
                                    introduce_name(
                                        &mut self.interner,
                                        &format!("__discard_{}_{}", name, index),
                                    ),
                                )
                            } else {
                                (None, DISCARDED.to_string())
                            }
                        }
                        _ => {
                            let name = format!(
                                "tuple_{}_span_{}_{}",
                                index,
                                arg.location().start,
                                arg.location().end
                            );

                            let interned = introduce_name(&mut self.interner, &name);

                            (Some(name), interned)
                        }
                    };

                    let arg_type = type_map.get(&index).unwrap_or_else(|| {
                        unreachable!(
                            "Missing type for tuple index {} of tuple_span_{}_{}",
                            index, location.start, location.end
                        )
                    });

                    let val = AirTree::local_var(&tuple_name_interned, arg_type.clone());

                    let then = if DISCARDED != tuple_name_interned {
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

                    fields.push(tuple_name_interned);

                    if let Some(tuple_name) = tuple_name {
                        self.interner.pop_text(tuple_name);
                    }

                    then
                });

                fields.reverse();

                // This `value` is either value param that was passed in or local var

                let name = format!(
                    "__Tuple_span_{}_{}",
                    pattern.location().start,
                    pattern.location().end
                );

                let name_interned = introduce_name(&mut self.interner, &name);

                let local_var = AirTree::local_var(&name_interned, tipo.clone());

                let tree = assign_casted_value(
                    name_interned,
                    value,
                    AirTree::tuple_access(
                        fields,
                        tipo.clone(),
                        local_var,
                        props.full_check,
                        then,
                        otherwise,
                    ),
                );

                self.interner.pop_text(name);

                tree
            }
        }
    }

    pub fn expect_type_assign(
        &mut self,
        tipo: &Rc<Type>,
        value: AirTree,
        defined_data_types: &mut IndexMap<String, u64>,
        location: Span,
        then: AirTree,
        otherwise: Otherwise,
    ) -> AirTree {
        assert!(
            tipo.get_generic().is_none(),
            "left-hand side of expect is generic: {}",
            tipo.to_pretty(0)
        );
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
            ) => then,

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
                let curried_expect_on_list = "__curried_expect_on_list".to_string();
                let list = "__list".to_string();

                let map_name_interned = introduce_name(&mut self.interner, &map_name);
                let pair_name_interned = introduce_name(&mut self.interner, &pair_name);
                let fst_name_interned = introduce_name(&mut self.interner, &fst_name);
                let snd_name_interned = introduce_name(&mut self.interner, &snd_name);
                let curried_expect_on_list_interned =
                    introduce_name(&mut self.interner, &curried_expect_on_list);
                let list_interned = introduce_name(&mut self.interner, &list);

                let expect_snd = self.expect_type_assign(
                    &inner_pair_types[1],
                    AirTree::local_var(snd_name_interned.clone(), inner_pair_types[1].clone()),
                    defined_data_types,
                    location,
                    AirTree::call(
                        AirTree::local_var(&curried_expect_on_list_interned, Type::void()),
                        Type::void(),
                        vec![AirTree::builtin(
                            DefaultFunction::TailList,
                            Type::list(Type::data()),
                            vec![AirTree::local_var(&list_interned, tipo.clone())],
                        )],
                    ),
                    otherwise.clone(),
                );

                let expect_fst = self.expect_type_assign(
                    &inner_pair_types[0],
                    AirTree::local_var(fst_name_interned.clone(), inner_pair_types[0].clone()),
                    defined_data_types,
                    location,
                    expect_snd,
                    otherwise.clone(),
                );

                let unwrap_function = AirTree::anon_func(
                    vec![list_interned.clone(), curried_expect_on_list_interned],
                    AirTree::list_empty(
                        AirTree::local_var(&list_interned, tipo.clone()),
                        then,
                        AirTree::anon_func(
                            vec![],
                            AirTree::let_assignment(
                                &pair_name_interned,
                                AirTree::builtin(
                                    DefaultFunction::HeadList,
                                    Type::pair(Type::data(), Type::data()),
                                    vec![AirTree::local_var(list_interned, tipo.clone())],
                                ),
                                AirTree::pair_access(
                                    Some(fst_name_interned),
                                    Some(snd_name_interned),
                                    inner_list_type.clone(),
                                    AirTree::local_var(
                                        &pair_name_interned,
                                        inner_list_type.clone(),
                                    ),
                                    true,
                                    expect_fst,
                                    otherwise.unwrap_or_else(DELAY_ERROR),
                                ),
                            ),
                            true,
                        ),
                    ),
                    false,
                );

                let function = self.code_gen_functions.get(EXPECT_ON_LIST);

                // This function can be defined here on in the branch below
                if function.is_none() {
                    let expect_list_func = AirTree::expect_on_list2();
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
                            Type::void(),
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
                    Type::void(),
                    vec![
                        AirTree::local_var(&map_name_interned, tipo.clone()),
                        unwrap_function,
                    ],
                );

                let tree = AirTree::let_assignment(map_name_interned, value, func_call);

                self.interner.pop_text(map_name);
                self.interner.pop_text(pair_name);
                self.interner.pop_text(fst_name);
                self.interner.pop_text(snd_name);
                self.interner.pop_text(curried_expect_on_list);
                self.interner.pop_text(list);

                tree
            }
            // Tuple type
            Some(UplcType::List(_)) if tipo.is_tuple() => {
                let tuple_inner_types = tipo.get_inner_types();

                assert!(!tuple_inner_types.is_empty());

                let tuple_name = format!("__tuple_span_{}_{}", location.start, location.end);

                let tuple_name_interned = introduce_name(&mut self.interner, &tuple_name);

                let mut tuple_expect_items = vec![];

                let then =
                    tuple_inner_types
                        .iter()
                        .enumerate()
                        .rfold(then, |then, (index, arg)| {
                            let tuple_index_name = format!(
                                "__tuple_index_{}_span_{}_{}",
                                index, location.start, location.end
                            );

                            let tuple_index_name_interned =
                                introduce_name(&mut self.interner, &tuple_index_name);

                            let expect_tuple_item = self.expect_type_assign(
                                arg,
                                AirTree::local_var(&tuple_index_name_interned, arg.clone()),
                                defined_data_types,
                                location,
                                then,
                                otherwise.clone(),
                            );

                            tuple_expect_items.push(tuple_index_name_interned);

                            self.interner.pop_text(tuple_index_name);

                            expect_tuple_item
                        });

                tuple_expect_items.reverse();

                let tuple_access = AirTree::tuple_access(
                    tuple_expect_items,
                    tipo.clone(),
                    AirTree::local_var(&tuple_name_interned, tipo.clone()),
                    true,
                    then,
                    otherwise.unwrap_or_else(DELAY_ERROR),
                );

                let tree = AirTree::let_assignment(tuple_name_interned, value, tuple_access);

                self.interner.pop_text(tuple_name);

                tree
            }
            // Regular List type
            Some(UplcType::List(_)) => {
                assert!(!tipo.get_inner_types().is_empty());

                let inner_list_type = &tipo.get_inner_types()[0];

                if inner_list_type.is_data() {
                    then
                } else {
                    let list_name = format!("__list_span_{}_{}", location.start, location.end);
                    let item_name = format!("__item_span_{}_{}", location.start, location.end);
                    let list = "__list".to_string();
                    let curried_func = "__curried_expect_on_list".to_string();

                    let list_name_interned = introduce_name(&mut self.interner, &list_name);
                    let item_name_interned = introduce_name(&mut self.interner, &item_name);
                    let list_interned = introduce_name(&mut self.interner, &list);
                    let curried_func_interned = introduce_name(&mut self.interner, &curried_func);

                    let unwrap_function = AirTree::anon_func(
                        vec![list_interned.clone(), curried_func_interned.clone()],
                        AirTree::list_empty(
                            AirTree::local_var(&list_interned, tipo.clone()),
                            then,
                            AirTree::anon_func(
                                vec![],
                                AirTree::let_assignment(
                                    &item_name_interned,
                                    AirTree::builtin(
                                        DefaultFunction::HeadList,
                                        Type::data(),
                                        vec![AirTree::local_var(&list_interned, tipo.clone())],
                                    ),
                                    AirTree::soft_cast_assignment(
                                        &item_name_interned,
                                        inner_list_type.clone(),
                                        AirTree::local_var(&item_name_interned, Type::data()),
                                        self.expect_type_assign(
                                            inner_list_type,
                                            AirTree::local_var(
                                                &item_name_interned,
                                                inner_list_type.clone(),
                                            ),
                                            defined_data_types,
                                            location,
                                            AirTree::call(
                                                AirTree::local_var(
                                                    curried_func_interned,
                                                    Type::void(),
                                                ),
                                                Type::void(),
                                                vec![AirTree::builtin(
                                                    DefaultFunction::TailList,
                                                    Type::list(Type::data()),
                                                    vec![AirTree::local_var(
                                                        list_interned,
                                                        tipo.clone(),
                                                    )],
                                                )],
                                            ),
                                            otherwise.clone(),
                                        ),
                                        otherwise.unwrap_or_else(DELAY_ERROR),
                                    ),
                                ),
                                true,
                            ),
                        ),
                        false,
                    );

                    let function = self.code_gen_functions.get(EXPECT_ON_LIST);

                    if function.is_none() {
                        let expect_list_func = AirTree::expect_on_list2();
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
                                Type::void(),
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
                        Type::void(),
                        vec![
                            AirTree::local_var(&list_name_interned, tipo.clone()),
                            unwrap_function,
                        ],
                    );

                    let tree = AirTree::let_assignment(list_name_interned, value, func_call);

                    self.interner.pop_text(list_name);
                    self.interner.pop_text(item_name);
                    self.interner.pop_text(list);
                    self.interner.pop_text(curried_func);

                    tree
                }
            }
            // Pair type
            Some(UplcType::Pair(_, _)) => {
                let tuple_inner_types = tipo.get_inner_types();

                assert!(tuple_inner_types.len() == 2);

                let pair_name = format!("__pair_span_{}_{}", location.start, location.end);
                let fst_name = format!("__pair_fst_span_{}_{}", location.start, location.end);
                let snd_name = format!("__pair_snd_span_{}_{}", location.start, location.end);

                let pair_name_interned = introduce_name(&mut self.interner, &pair_name);
                let fst_name_interned = introduce_name(&mut self.interner, &fst_name);
                let snd_name_interned = introduce_name(&mut self.interner, &snd_name);

                let expect_snd = self.expect_type_assign(
                    &tuple_inner_types[1],
                    AirTree::local_var(snd_name_interned.clone(), tuple_inner_types[1].clone()),
                    defined_data_types,
                    location,
                    then,
                    otherwise.clone(),
                );

                let expect_fst = self.expect_type_assign(
                    &tuple_inner_types[0],
                    AirTree::local_var(fst_name_interned.clone(), tuple_inner_types[0].clone()),
                    defined_data_types,
                    location,
                    expect_snd,
                    otherwise.clone(),
                );

                let pair_access = AirTree::pair_access(
                    Some(fst_name_interned),
                    Some(snd_name_interned),
                    tipo.clone(),
                    AirTree::local_var(&pair_name_interned, tipo.clone()),
                    true,
                    expect_fst,
                    otherwise.unwrap_or_else(DELAY_ERROR),
                );

                let tree = AirTree::let_assignment(pair_name_interned, value, pair_access);

                self.interner.pop_text(pair_name);
                self.interner.pop_text(fst_name);
                self.interner.pop_text(snd_name);

                tree
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
                    .join(DISCARDED);

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

                let data_type_name = if otherwise.is_some() {
                    format!(
                        "__expect_{}_{}_otherwise",
                        data_type.name, data_type_variant
                    )
                } else {
                    format!("__expect_{}_{}", data_type.name, data_type_variant)
                };
                let function = self.code_gen_functions.get(&data_type_name);

                // mutate code_gen_funcs and defined_data_types in this if branch
                if function.is_none() && defined_data_types.get(&data_type_name).is_none() {
                    defined_data_types.insert(data_type_name.clone(), 1);

                    let current_defined = defined_data_types.clone();
                    let mut diff_defined_types = vec![];

                    let var_then = AirTree::call(
                        AirTree::local_var("then_delayed", Type::void()),
                        Type::void(),
                        vec![],
                    );

                    let otherwise_delayed = otherwise
                        .as_ref()
                        .map(|_| AirTree::local_var("otherwise_delayed", Type::void()));

                    let is_never = data_type.is_never();

                    let constr_clauses = data_type.constructors.iter().enumerate().rfold(
                        otherwise_delayed.clone().unwrap_or_else(DELAY_ERROR),
                        |acc, (index, constr)| {
                            // NOTE: For the Never type, we have an placeholder first constructor
                            // that must be ignored. The Never type is considered to have only one
                            // constructor starting at index 1 so it shouldn't be possible to
                            // cast from Data into the first constructor. There's virtually no
                            // constructor at index 0.
                            if is_never && index == 0 {
                                return acc;
                            }

                            let mut constr_args = vec![];

                            let constr_then = constr.arguments.iter().enumerate().rfold(
                                var_then.clone(),
                                |then, (index, arg)| {
                                    let arg_name =
                                        arg.label.clone().unwrap_or(format!("__field_{index}"));

                                    let arg_tipo =
                                        find_and_replace_generics(&arg.tipo, &mono_types);

                                    constr_args.push((index, arg_name.clone(), arg_tipo.clone()));

                                    self.expect_type_assign(
                                        &arg_tipo.clone(),
                                        AirTree::local_var(arg_name, arg_tipo),
                                        defined_data_types,
                                        location,
                                        then,
                                        otherwise_delayed.clone(),
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
                                    otherwise_delayed.clone().unwrap_or_else(DELAY_ERROR),
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
                                    otherwise_delayed.clone().unwrap_or_else(DELAY_ERROR),
                                )
                            };

                            AirTree::anon_func(
                                vec![],
                                AirTree::clause(
                                    format!("__subject_span_{}_{}", location.start, location.end),
                                    AirTree::int(index),
                                    tipo.clone(),
                                    then,
                                    acc,
                                ),
                                true,
                            )
                        },
                    );

                    let when_expr = AirTree::when(
                        format!("__subject_span_{}_{}", location.start, location.end),
                        Type::void(),
                        tipo.clone(),
                        AirTree::local_var(
                            format!("__constr_var_span_{}_{}", location.start, location.end),
                            tipo.clone(),
                        ),
                        AirTree::call(constr_clauses, Type::void(), vec![]),
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

                    let code_gen_func = CodeGenFunction::Function {
                        body: func_body,
                        params: if otherwise.is_some() {
                            vec![
                                "__param_0".to_string(),
                                "then_delayed".to_string(),
                                "otherwise_delayed".to_string(),
                            ]
                        } else {
                            vec!["__param_0".to_string(), "then_delayed".to_string()]
                        },
                    };

                    self.code_gen_functions
                        .insert(data_type_name.clone(), code_gen_func);
                } else if let Some(counter) = defined_data_types.get_mut(&data_type_name) {
                    *counter += 1;
                } else {
                    defined_data_types.insert(data_type_name.to_string(), 1);
                }

                let args = if let Some(otherwise) = otherwise {
                    vec![value, AirTree::anon_func(vec![], then, true), otherwise]
                } else {
                    vec![value, AirTree::anon_func(vec![], then, true)]
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

                AirTree::call(func_var, Type::void(), args)
            }
        }
    }

    fn handle_decision_tree(
        &mut self,
        subject_name: &String,
        subject_tipo: Rc<Type>,
        return_tipo: Rc<Type>,
        module_build_name: &str,
        tree: decision_tree::DecisionTree<'_>,
        mut stick_set: TreeSet,
    ) -> AirTree {
        match tree {
            DecisionTree::Switch {
                path,
                mut cases,
                default,
            } => {
                //Current path to test
                let current_tipo = get_tipo_by_path(subject_tipo.clone(), &path);
                let builtins_path = Builtins::new_from_path(subject_tipo.clone(), path);
                let current_subject_name = if builtins_path.is_empty() {
                    subject_name.clone()
                } else {
                    format!("{}_{}", subject_name, builtins_path)
                };

                // Transition process from previous to current
                let builtins_to_add = stick_set.diff_union_builtins(builtins_path.clone());

                // Previous path to apply the transition process too
                let prev_builtins = Builtins {
                    vec: builtins_path.vec[0..(builtins_path.len() - builtins_to_add.len())]
                        .to_vec(),
                };

                let prev_subject_name = if prev_builtins.is_empty() {
                    subject_name.clone()
                } else {
                    format!("{}_{}", subject_name, prev_builtins)
                };
                let prev_tipo = prev_builtins
                    .vec
                    .last()
                    .map_or(subject_tipo.clone(), |last| last.tipo());

                let data_type = lookup_data_type_by_tipo(&self.data_types, &current_tipo);

                let last_clause = if data_type
                    .as_ref()
                    .map_or(true, |d| d.constructors.len() != cases.len())
                {
                    *default.unwrap()
                } else {
                    cases.pop().unwrap().1
                };

                let last_clause = self.handle_decision_tree(
                    subject_name,
                    subject_tipo.clone(),
                    return_tipo.clone(),
                    module_build_name,
                    last_clause,
                    stick_set.clone(),
                );

                let test_subject_name = if data_type.is_some() {
                    format!("{}_index", current_subject_name.clone(),)
                } else {
                    current_subject_name.clone()
                };

                let clauses = cases.into_iter().rfold(last_clause, |acc, (case, then)| {
                    let case_air = self.handle_decision_tree(
                        subject_name,
                        subject_tipo.clone(),
                        return_tipo.clone(),
                        module_build_name,
                        then,
                        stick_set.clone(),
                    );

                    AirTree::clause(
                        test_subject_name.clone(),
                        case.get_air_pattern(current_tipo.clone()),
                        current_tipo.clone(),
                        case_air,
                        AirTree::anon_func(vec![], acc, true),
                    )
                });

                let when_air_clauses = AirTree::when(
                    test_subject_name,
                    return_tipo.clone(),
                    current_tipo.clone(),
                    AirTree::local_var(current_subject_name, current_tipo.clone()),
                    clauses,
                );

                builtins_to_add.produce_air(
                    // The only reason I pass this in is to ensure I signal
                    // whether or not constr_fields_exposer was used. I could
                    // probably optimize this part out to simplify codegen in
                    // the future
                    &mut self.special_functions,
                    prev_subject_name,
                    prev_tipo,
                    when_air_clauses,
                )
            }
            DecisionTree::ListSwitch {
                path,
                cases,
                tail_cases,
                default,
            } => {
                //Current path to test
                let current_tipo = get_tipo_by_path(subject_tipo.clone(), &path);
                let builtins_path = Builtins::new_from_path(subject_tipo.clone(), path);
                let current_subject_name = if builtins_path.is_empty() {
                    subject_name.clone()
                } else {
                    format!("{}_{}", subject_name, builtins_path)
                };

                // Transition process from previous to current
                let builtins_to_add = stick_set.diff_union_builtins(builtins_path.clone());

                // Previous path to apply the transition process too
                let prev_builtins = Builtins {
                    vec: builtins_path.vec[0..(builtins_path.len() - builtins_to_add.len())]
                        .to_vec(),
                };

                let prev_subject_name = if prev_builtins.is_empty() {
                    subject_name.clone()
                } else {
                    format!("{}_{}", subject_name, prev_builtins)
                };
                let prev_tipo = prev_builtins
                    .vec
                    .last()
                    .map_or(subject_tipo.clone(), |last| last.tipo());

                let longest_pattern = cases.iter().chain(tail_cases.iter()).fold(
                    0,
                    |longest, (case, _)| match case {
                        CaseTest::List(i) => {
                            if longest < *i {
                                *i
                            } else {
                                longest
                            }
                        }
                        CaseTest::ListWithTail(i) => {
                            if longest < *i {
                                *i - 1
                            } else {
                                longest
                            }
                        }
                        _ => unreachable!(),
                    },
                );

                let last_pattern = if tail_cases.is_empty() {
                    *default.as_ref().unwrap().clone()
                } else {
                    let tree = tail_cases.last().unwrap();

                    tree.1.clone()
                };

                let builtins_for_pattern = builtins_path.merge(Builtins::new_from_list_case(
                    CaseTest::List(longest_pattern),
                ));

                stick_set.diff_union_builtins(builtins_for_pattern.clone());

                let last_pattern = self.handle_decision_tree(
                    subject_name,
                    subject_tipo.clone(),
                    return_tipo.clone(),
                    module_build_name,
                    last_pattern,
                    stick_set.clone(),
                );

                let list_clauses = (0..=longest_pattern).rev().with_position().fold(
                    (builtins_for_pattern, last_pattern),
                    |(mut builtins_for_pattern, acc), list_item| match list_item {
                        itertools::Position::First(index) | itertools::Position::Only(index) => {
                            let (_, tree) = cases
                                .iter()
                                .chain(tail_cases.iter())
                                .find(|x| match x.0 {
                                    CaseTest::List(i) => i == index,
                                    CaseTest::ListWithTail(i) => i <= index,
                                    _ => unreachable!(),
                                })
                                .cloned()
                                .unwrap_or_else(|| {
                                    (CaseTest::Wild, *default.as_ref().unwrap().clone())
                                });

                            let tail_name = if builtins_for_pattern.is_empty() {
                                subject_name.clone()
                            } else {
                                format!("{}_{}", subject_name, builtins_for_pattern)
                            };

                            let then = self.handle_decision_tree(
                                subject_name,
                                subject_tipo.clone(),
                                return_tipo.clone(),
                                module_build_name,
                                tree,
                                stick_set.clone(),
                            );

                            let acc = AirTree::list_clause(
                                tail_name.clone(),
                                subject_tipo.clone(),
                                then,
                                AirTree::anon_func(vec![], acc, true),
                                None,
                            );

                            builtins_for_pattern.pop();

                            (builtins_for_pattern, acc)
                        }

                        itertools::Position::Middle(index) | itertools::Position::Last(index) => {
                            let (_, tree) = cases
                                .iter()
                                .chain(tail_cases.iter())
                                .find(|x| match x.0 {
                                    CaseTest::List(i) => i == index,
                                    CaseTest::ListWithTail(i) => i <= index,
                                    _ => unreachable!(),
                                })
                                .cloned()
                                .unwrap_or_else(|| {
                                    (CaseTest::Wild, *default.as_ref().unwrap().clone())
                                });

                            let tail_name = if builtins_for_pattern.is_empty() {
                                subject_name.clone()
                            } else {
                                format!("{}_{}", subject_name, builtins_for_pattern)
                            };

                            // TODO: change this in the future to use the Builtins to_string method
                            // to ensure future changes don't break things
                            let next_tail_name = Some(format!("{}_tail", tail_name));

                            let then = self.handle_decision_tree(
                                subject_name,
                                subject_tipo.clone(),
                                return_tipo.clone(),
                                module_build_name,
                                tree,
                                stick_set.clone(),
                            );

                            let acc = AirTree::list_clause(
                                tail_name.clone(),
                                subject_tipo.clone(),
                                then,
                                AirTree::anon_func(vec![], acc, true),
                                next_tail_name.map(|next| (tail_name, next)),
                            );

                            // since we iterate over the list cases in reverse
                            // We pop off a builtin to make it easier to get the name of
                            // prev_tested list case since each name is based off the builtins
                            builtins_for_pattern.pop();

                            (builtins_for_pattern, acc)
                        }
                    },
                );

                let when_list_cases = AirTree::when(
                    current_subject_name.clone(),
                    return_tipo.clone(),
                    current_tipo.clone(),
                    AirTree::local_var(current_subject_name, current_tipo.clone()),
                    list_clauses.1,
                );

                builtins_to_add.produce_air(
                    // The only reason I pass this in is to ensure I signal
                    // whether or not constr_fields_exposer was used. I could
                    // probably optimize this part out to simplify codegen in
                    // the future
                    &mut self.special_functions,
                    prev_subject_name,
                    prev_tipo,
                    when_list_cases,
                )
            }
            DecisionTree::HoistedLeaf(name, args) => {
                let air_args = args
                    .iter()
                    .map(|item| {
                        let current_tipo = get_tipo_by_path(subject_tipo.clone(), &item.path);

                        (
                            current_tipo.clone(),
                            AirTree::local_var(item.assigned.clone(), current_tipo),
                        )
                    })
                    .collect_vec();

                let then = AirTree::call(
                    AirTree::local_var(
                        name,
                        Type::function(
                            air_args.iter().map(|i| i.0.clone()).collect_vec(),
                            return_tipo.clone(),
                        ),
                    ),
                    Type::void(),
                    air_args.into_iter().map(|i| i.1).collect_vec(),
                );

                self.handle_assigns(subject_name, subject_tipo, &args, &mut stick_set, then)
            }
            DecisionTree::HoistThen {
                name,
                assigns,
                pattern,
                then,
            } => {
                let assign = AirTree::let_assignment(
                    name,
                    AirTree::anon_func(
                        assigns
                            .iter()
                            .map(|i| introduce_name(&mut self.interner, &i.assigned))
                            .collect_vec(),
                        // The one reason we have to pass in mutable self
                        // So we can build the TypedExpr into Air
                        self.build(then, module_build_name, &[]),
                        true,
                    ),
                    self.handle_decision_tree(
                        subject_name,
                        subject_tipo,
                        return_tipo,
                        module_build_name,
                        *pattern,
                        stick_set,
                    ),
                );

                assigns.into_iter().for_each(|x| {
                    self.interner.pop_text(x.assigned);
                });

                assign
            }
        }
    }

    fn handle_assigns(
        &mut self,
        subject_name: &String,
        subject_tipo: Rc<Type>,
        assigns: &[Assigned],
        stick_set: &mut TreeSet,
        then: AirTree,
    ) -> AirTree {
        match assigns {
            [] => then,
            [assign, rest @ ..] => {
                let Assigned { path, assigned } = assign;

                let current_tipo = get_tipo_by_path(subject_tipo.clone(), path);
                let builtins_path = Builtins::new_from_path(subject_tipo.clone(), path.clone());
                let current_subject_name = if builtins_path.is_empty() {
                    subject_name.clone()
                } else {
                    format!("{}_{}", subject_name, builtins_path)
                };

                // Transition process from previous to current
                let builtins_to_add = stick_set.diff_union_builtins(builtins_path.clone());

                // Previous path to apply the transition process too
                let prev_builtins = Builtins {
                    vec: builtins_path.vec[0..(builtins_path.len() - builtins_to_add.len())]
                        .to_vec(),
                };

                let prev_subject_name = if prev_builtins.is_empty() {
                    subject_name.clone()
                } else {
                    format!("{}_{}", subject_name, prev_builtins)
                };
                let prev_tipo = prev_builtins
                    .vec
                    .last()
                    .map_or(subject_tipo.clone(), |last| last.tipo());

                let assignment = AirTree::let_assignment(
                    assigned,
                    AirTree::local_var(current_subject_name, current_tipo),
                    self.handle_assigns(subject_name, subject_tipo, rest, stick_set, then),
                );

                builtins_to_add.produce_air(
                    &mut self.special_functions,
                    prev_subject_name,
                    prev_tipo,
                    assignment,
                )
            }
        }
    }

    fn hoist_functions_to_validator(&mut self, mut air_tree: AirTree) -> AirTree {
        let mut functions_to_hoist = IndexMap::new();
        let mut used_functions = vec![];
        let mut defined_functions = vec![];
        let mut hoisted_functions = vec![];
        let mut validator_hoistable;

        // TODO change subsequent tree traversals to be more like a stream.
        air_tree.traverse_tree_with(&mut |air_tree: &mut AirTree, _| {
            erase_opaque_type_operations(air_tree, &self.data_types);
        });

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
                HoistableFunction::Function { deps, .. } => {
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

                let deps = (tree_path, func_deps.clone());

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
                    (key, variant),
                    hoisted_functions,
                    functions_to_hoist,
                    defined_function,
                );

                // now hoist full function onto validator tree
                *node_to_edit = defined_dependencies;

                hoisted_functions.push((key.clone(), variant.clone()));
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
                HoistableFunction::Function { deps, .. } => {
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
                if
                // if the dependency is the same as the function we're hoisting
                // or we hoisted it, then skip it
                hoisted_functions
                    .iter()
                    .any(|(generic, variant)| generic == &dep_key && variant == &dep_variant)
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
                if &dep_path.common_ancestor(func_path) == func_path {
                    match dep_function.clone() {
                        HoistableFunction::Function {
                            body: mut dep_air_tree,
                            deps: dependency_deps,
                            params: dependent_params,
                        } => {
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

                            hoisted_functions.push((dep_key.clone(), dep_variant.clone()));

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

                            hoisted_functions.push((dep_key.clone(), dep_variant.clone()));

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
                                    "Missing function definition for {}. Known functions: {:?}",
                                    generic_function_key.function_name,
                                    self.functions.keys(),
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
                            // Shortcut path for compiler generated functions
                            let CodeGenFunction::Function { body, params } = code_gen_func else {
                                unreachable!()
                            };

                            let mut function_variant_path = IndexMap::new();

                            let mut body = AirTree::no_op(body.clone());

                            body.traverse_tree_with(&mut |air_tree, _| {
                                erase_opaque_type_operations(air_tree, &self.data_types);
                            });

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
                            let args = function_def.arguments.clone();

                            let params = args
                                .iter()
                                .map(|arg| {
                                    arg.arg_name
                                        .get_variable_name()
                                        .map(|arg| {
                                            introduce_name(&mut self.interner, &arg.to_string())
                                        })
                                        .unwrap_or_else(|| DISCARDED.to_string())
                                })
                                .collect_vec();

                            let mut function_air_tree_body = AirTree::no_op(self.build(
                                &function_def.body,
                                &generic_function_key.module_name,
                                &[],
                            ));

                            function_air_tree_body.traverse_tree_with(&mut |air_tree, _| {
                                erase_opaque_type_operations(air_tree, &self.data_types);
                                monomorphize(air_tree, &mono_types);
                            });

                            args.iter().for_each(|arg| {
                                arg.arg_name.get_variable_name().iter().for_each(|arg| {
                                    self.interner.pop_text(arg.to_string());
                                })
                            });

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
                        let args = function_def.arguments.clone();

                        let params = args
                            .iter()
                            .map(|arg| {
                                arg.arg_name
                                    .get_variable_name()
                                    .map(|arg| introduce_name(&mut self.interner, &arg.to_string()))
                                    .unwrap_or_else(|| DISCARDED.to_string())
                            })
                            .collect_vec();

                        let mut function_air_tree_body = AirTree::no_op(self.build(
                            &function_def.body,
                            &generic_function_key.module_name,
                            &[],
                        ));

                        function_air_tree_body.traverse_tree_with(&mut |air_tree, _| {
                            erase_opaque_type_operations(air_tree, &self.data_types);
                            monomorphize(air_tree, &mono_types);
                        });

                        let mut function_variant_path = IndexMap::new();

                        args.iter().for_each(|arg| {
                            arg.arg_name
                                .get_variable_name()
                                .iter()
                                .for_each(|arg| self.interner.pop_text(arg.to_string()))
                        });

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
                ValueConstructorVariant::ModuleConstant { module, name, .. } => {
                    let access_key = FunctionAccessKey {
                        module_name: module.clone(),
                        function_name: name.clone(),
                    };

                    let definition = self
                        .constants
                        .get(&access_key)
                        .unwrap_or_else(|| panic!("unknown constant {module}.{name}"));

                    let mut value =
                        AirTree::no_op(self.build(definition, &access_key.module_name, &[]));

                    value.traverse_tree_with(&mut |air_tree, _| {
                        erase_opaque_type_operations(air_tree, &self.data_types);
                    });

                    value = self.hoist_functions_to_validator(value);

                    let term = self
                        .uplc_code_gen(value.to_vec())
                        .constr_fields_exposer()
                        .constr_index_exposer();

                    let mut program =
                        self.new_program(self.special_functions.apply_used_functions(term));

                    let mut interner = CodeGenInterner::new();

                    interner.program(&mut program);

                    let eval_program: Program<NamedDeBruijn> =
                        program.clean_up().try_into().unwrap();

                    Some(
                        eval_program
                            .eval(ExBudget::max())
                            .result()
                            .unwrap_or_else(|e| panic!("Failed to evaluate constant: {e:#?}"))
                            .try_into()
                            .unwrap(),
                    )
                }
                ValueConstructorVariant::ModuleFn {
                    name: func_name,
                    module,
                    builtin,
                    ..
                } => {
                    if let Some(func) = builtin {
                        return self.gen_uplc(
                            Air::Builtin {
                                count: 0,
                                func: *func,
                                tipo: constructor.tipo,
                            },
                            arg_stack,
                        );
                    }

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

                            let mut program = self.new_program(term);

                            let mut interner = CodeGenInterner::new();

                            interner.program(&mut program);

                            let eval_program: Program<NamedDeBruijn> =
                                program.clean_up().try_into().unwrap();

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

                    match term.pierce_no_inlines() {
                        Term::Var(_) => Some(term.force()),
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
                    | DefaultFunction::MkCons
                    | DefaultFunction::UnConstrData => {
                        builder::special_case_builtin(&func, tipo, count, arg_vec)
                    }
                    DefaultFunction::FstPair | DefaultFunction::SndPair => {
                        builder::undata_builtin(&func, count, ret_tipo, arg_vec)
                    }
                    DefaultFunction::HeadList if !tipo.is_pair() => {
                        builder::undata_builtin(&func, count, ret_tipo, arg_vec)
                    }
                    _ => {
                        let mut term: Term<Name> = func.into();

                        term = builder::apply_builtin_forces(term, func.force_count());

                        if func.arg_is_unit() {
                            term = term.apply(Term::unit())
                        } else {
                            for arg in arg_vec {
                                term = term.apply(arg.clone());
                            }
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
                module_name,
                variant_name,
                variant,
            } => {
                let func_name = if module_name.is_empty() {
                    format!("{func_name}{variant_name}")
                } else {
                    format!("{module_name}_{func_name}{variant_name}")
                };

                match variant {
                    air::FunctionVariants::Standard(params) => {
                        let mut func_body = arg_stack.pop().unwrap();

                        let term = arg_stack.pop().unwrap();

                        if params.is_empty() {
                            func_body = func_body.delay();
                        }

                        let func_body = params
                            .into_iter()
                            .rfold(func_body, |term, arg| term.lambda(arg))
                            .lambda(NO_INLINE);

                        Some(term.lambda(func_name).apply(func_body))
                    }
                    air::FunctionVariants::Recursive {
                        params,
                        recursive_nonstatic_params,
                    } => {
                        let mut func_body = arg_stack.pop().unwrap();

                        let term = arg_stack.pop().unwrap();

                        let no_statics = recursive_nonstatic_params == params;

                        if recursive_nonstatic_params.is_empty() || params.is_empty() {
                            func_body = func_body.delay();
                        }

                        let func_body = recursive_nonstatic_params
                            .iter()
                            .rfold(func_body, |term, arg| term.lambda(arg));

                        let func_body = func_body.lambda(func_name.clone());

                        if no_statics {
                            // If we don't have any recursive-static params, we can just emit the function as is
                            Some(
                                term.lambda(func_name.clone())
                                    .apply(
                                        Term::var(func_name.clone())
                                            .apply(Term::var(func_name.clone())),
                                    )
                                    .lambda(func_name)
                                    .apply(func_body.lambda(NO_INLINE)),
                            )
                        } else {
                            // If we have parameters that remain static in each recursive call,
                            // we can construct an *outer* function to take those in
                            // and simplify the recursive part to only accept the non-static arguments
                            let mut recursive_func_body =
                                Term::var(&func_name).apply(Term::var(&func_name));

                            if recursive_nonstatic_params.is_empty() {
                                recursive_func_body = recursive_func_body.force();
                            }

                            // Introduce a parameter for each parameter
                            // NOTE: we use recursive_nonstatic_params here because
                            // if this is recursive, those are the ones that need to be passed
                            // each time
                            for param in recursive_nonstatic_params.into_iter() {
                                recursive_func_body = recursive_func_body.apply(Term::var(param));
                            }

                            // Then construct an outer function with *all* parameters, not just the nonstatic ones.
                            let mut outer_func_body =
                                recursive_func_body.lambda(&func_name).apply(func_body);

                            // Now, add *all* parameters, so that other call sites don't know the difference
                            outer_func_body = params
                                .clone()
                                .into_iter()
                                .rfold(outer_func_body, |term, arg| term.lambda(arg));

                            // And finally, fold that definition into the rest of our program
                            Some(
                                term.lambda(&func_name)
                                    .apply(outer_func_body.lambda(NO_INLINE)),
                            )
                        }
                    }
                    air::FunctionVariants::Cyclic(contained_functions) => {
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
                }
            }

            Air::Let { name } => {
                let arg = arg_stack.pop().unwrap();

                let mut term = arg_stack.pop().unwrap();

                term = term.lambda(name).apply(arg);

                Some(term)
            }
            Air::CastFromData { tipo, full_cast } => {
                let mut term = arg_stack.pop().unwrap();

                term = if full_cast {
                    unknown_data_to_type(term, &tipo)
                } else {
                    known_data_to_type(term, &tipo)
                };

                if extract_constant(term.pierce_no_inlines()).is_some() {
                    let mut program = self.new_program(term);

                    let mut interner = CodeGenInterner::new();

                    interner.program(&mut program);

                    let eval_program: Program<NamedDeBruijn> =
                        program.clean_up().try_into().unwrap();

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

                if extract_constant(term.pierce_no_inlines()).is_some() {
                    term = builder::convert_type_to_data(term, &tipo);

                    let mut program = self.new_program(term);

                    let mut interner = CodeGenInterner::new();

                    interner.program(&mut program);

                    let eval_program: Program<NamedDeBruijn> =
                        program.clean_up().try_into().unwrap();

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
            } => {
                // clause to compare
                let clause = arg_stack.pop().unwrap();

                // the body to be run if the clause matches
                let body = arg_stack.pop().unwrap();

                // the next branch in the when expression
                // Expected to be delayed
                let term = arg_stack.pop().unwrap();

                assert!(matches!(term, Term::Delay(_) | Term::Var(_)));

                let other_clauses = term.clone();

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

                    condition.delay_true_if_then_else(body, other_clauses)
                };

                Some(body)
            }
            Air::ListClause {
                tail_name,
                next_tail_name,
                ..
            } => {
                // no longer need to pop off discard
                let body = arg_stack.pop().unwrap();
                let mut term = arg_stack.pop().unwrap();

                assert!(matches!(term, Term::Delay(_)));

                term = if let Some((current_tail, next_tail_name)) = next_tail_name {
                    term.force()
                        .lambda(next_tail_name)
                        .apply(Term::tail_list().apply(Term::var(current_tail.clone())))
                        .delay()
                } else {
                    term
                };

                term = Term::var(tail_name).delay_empty_choose_list(body, term);

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
                    Some(then.lambda(DISCARDED).apply(Term::var(subject_name)))
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
                    let maybe_const = extract_constant(item.pierce_no_inlines());
                    maybe_const.is_some()
                }) {
                    let mut program = self.new_program(term);

                    let mut interner = CodeGenInterner::new();

                    interner.program(&mut program);

                    let eval_program: Program<NamedDeBruijn> =
                        program.clean_up().try_into().unwrap();

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
                    .skip_while(|(name, _, _)| name == DISCARDED)
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

                let data_type =
                    lookup_data_type_by_tipo(&self.data_types, &tipo).unwrap_or_else(|| {
                        panic!(
                            "Attempted record update on an unknown type!\ntype: {:#?}",
                            tipo
                        )
                    });

                assert!(
                    !data_type.is_never(),
                    "Attempted record update on a Never type.",
                );

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
                    let value = Term::snd_pair().apply(Term::var(format!("__pair_{list_id}")));
                    term = if is_expect {
                        if otherwise == Term::Error.delay() {
                            term.lambda(name)
                                .apply(unknown_data_to_type(value, &inner_types[1]))
                        } else {
                            softcast_data_to_type_otherwise(
                                value,
                                &name,
                                &inner_types[1],
                                term,
                                otherwise.clone(),
                            )
                        }
                    } else {
                        term.lambda(name)
                            .apply(known_data_to_type(value, &inner_types[1]))
                    }
                }

                if let Some(name) = fst {
                    let value = Term::fst_pair().apply(Term::var(format!("__pair_{list_id}")));
                    term = if is_expect {
                        if otherwise == Term::Error.delay() {
                            term.lambda(name)
                                .apply(unknown_data_to_type(value, &inner_types[0]))
                        } else {
                            softcast_data_to_type_otherwise(
                                value,
                                &name,
                                &inner_types[0],
                                term,
                                otherwise,
                            )
                        }
                    } else {
                        term.lambda(name)
                            .apply(known_data_to_type(value, &inner_types[0]))
                    }
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
            Air::SoftCastLet { name, tipo } => {
                let value = arg_stack.pop().unwrap();
                let then = arg_stack.pop().unwrap();
                let otherwise = arg_stack.pop().unwrap();

                if otherwise == Term::Error.delay() {
                    Some(then.lambda(name).apply(unknown_data_to_type(value, &tipo)))
                } else {
                    Some(softcast_data_to_type_otherwise(
                        value, &name, &tipo, then, otherwise,
                    ))
                }
            }
            Air::ExtractField { tipo } => {
                let arg = arg_stack.pop().unwrap();

                Some(known_data_to_type(Term::head_list().apply(arg), &tipo))
            }
        }
    }
}
