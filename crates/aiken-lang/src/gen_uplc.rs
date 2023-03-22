use std::sync::Arc;

use indexmap::{IndexMap, IndexSet};
use itertools::Itertools;
use uplc::{
    ast::{Constant as UplcConstant, Name, NamedDeBruijn, Program, Term, Type as UplcType},
    builder::{ASSERT_ON_LIST, CONSTR_FIELDS_EXPOSER, CONSTR_GET_FIELD, CONSTR_INDEX_EXPOSER},
    builtins::DefaultFunction,
    machine::cost_model::ExBudget,
    optimize::aiken_optimize_and_intern,
    parser::interner::Interner,
};

use crate::{
    ast::{
        ArgName, AssignmentKind, BinOp, Pattern, Span, TypedClause, TypedDataType, TypedFunction,
        TypedValidator, UnOp,
    },
    builtins::bool,
    expr::TypedExpr,
    tipo::{
        ModuleValueConstructor, PatternConstructor, Type, TypeInfo, ValueConstructor,
        ValueConstructorVariant,
    },
    IdGenerator,
};

pub mod air;
pub mod builder;
pub mod stack;

use air::Air;
use builder::{
    AssignmentProperties, ClauseProperties, DataTypeKey, FuncComponents, FunctionAccessKey,
};

use self::stack::AirStack;

#[derive(Clone)]
pub struct CodeGenerator<'a> {
    defined_functions: IndexMap<FunctionAccessKey, ()>,
    functions: IndexMap<FunctionAccessKey, &'a TypedFunction>,
    data_types: IndexMap<DataTypeKey, &'a TypedDataType>,
    module_types: IndexMap<&'a String, &'a TypeInfo>,
    id_gen: IdGenerator,
    needs_field_access: bool,
    used_data_assert_on_list: bool,
    zero_arg_functions: IndexMap<FunctionAccessKey, Vec<Air>>,
}

impl<'a> CodeGenerator<'a> {
    pub fn new(
        functions: IndexMap<FunctionAccessKey, &'a TypedFunction>,
        data_types: IndexMap<DataTypeKey, &'a TypedDataType>,
        module_types: IndexMap<&'a String, &'a TypeInfo>,
    ) -> Self {
        CodeGenerator {
            defined_functions: IndexMap::new(),
            functions,
            data_types,
            module_types,
            id_gen: IdGenerator::new(),
            needs_field_access: false,
            used_data_assert_on_list: false,
            zero_arg_functions: IndexMap::new(),
        }
    }

    pub fn reset(&mut self) {
        self.needs_field_access = false;
        self.used_data_assert_on_list = false;
        self.zero_arg_functions = IndexMap::new();
        self.id_gen = IdGenerator::new();
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
        let mut ir_stack = AirStack::new(&mut self.id_gen);

        self.build_ir(&fun.body, &mut ir_stack);

        self.define_ir(&mut ir_stack);

        self.convert_opaque_type_to_inner_ir(&mut ir_stack);

        let mut term = self.uplc_code_gen(&mut ir_stack);

        // Wrap the validator body if ifThenElse term unit error
        term = term.final_wrapper();

        term = builder::wrap_validator_args(term, &fun.arguments);

        if let Some(other) = other_fun {
            self.reset();

            let mut other_ir_stack = vec![];

            let scope = vec![self.id_gen.next()];

            self.build_ir(&other.body, &mut other_ir_stack, scope);

            self.define_ir(&mut other_ir_stack);

            self.convert_opaque_type_to_inner_ir(&mut other_ir_stack);

            let other_term = self.uplc_code_gen(&mut other_ir_stack);

            let other_term = other_term.final_wrapper();

            let other_term = builder::wrap_validator_args(other_term, &other.arguments);

            let (spend, mint) = if other.arguments.len() > fun.arguments.len() {
                (other_term, term)
            } else {
                (term, other_term)
            };

            term = builder::wrap_as_multi_validator(spend, mint);
            self.needs_field_access = true;
        }

        term = builder::wrap_validator_args(term, params);

        self.finalize(term, true)
    }

    pub fn generate_test(&mut self, test_body: &TypedExpr) -> Program<Name> {
        let mut ir_stack = AirStack::new(&mut self.id_gen);

        self.build_ir(test_body, &mut ir_stack);

        self.define_ir(&mut ir_stack);

        self.convert_opaque_type_to_inner_ir(&mut ir_stack);

        let term = self.uplc_code_gen(&mut ir_stack);

        self.finalize(term, false)
    }

    fn finalize(&mut self, term: Term<Name>, wrap_as_validator: bool) -> Program<Name> {
        let mut term = if wrap_as_validator || self.used_data_assert_on_list {
            term.assert_on_list()
        } else {
            term
        };

        if self.needs_field_access {
            term = term
                .constr_get_field()
                .constr_fields_exposer()
                .constr_index_exposer();
        }

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

    pub(crate) fn build_ir(&mut self, body: &TypedExpr, ir_stack: &mut AirStack) {
        match body {
            TypedExpr::Int { value, .. } => ir_stack.integer(value.to_string()),
            TypedExpr::String { value, .. } => ir_stack.string(value.to_string()),
            TypedExpr::ByteArray { bytes, .. } => ir_stack.byte_array(bytes.to_vec()),
            TypedExpr::Pipeline { expressions, .. } | TypedExpr::Sequence { expressions, .. } => {
                let stacks = Vec::new();

                for (index, expr) in expressions.iter().enumerate() {
                    if index == 0 {
                        self.build_ir(expr, ir_stack);
                    } else {
                        let mut stack = ir_stack.in_new_scope();
                        self.build_ir(expr, &mut stack);
                    }
                }

                ir_stack.sequence(stacks);
            }
            TypedExpr::Var {
                constructor, name, ..
            } => match &constructor.variant {
                ValueConstructorVariant::ModuleConstant { literal, .. } => {
                    builder::constants_ir(literal, ir_stack);
                }
                ValueConstructorVariant::ModuleFn {
                    builtin: Some(builtin),
                    ..
                } => {
                    ir_stack.builtin(*builtin, constructor.tipo.clone(), vec![]);
                }
                _ => {
                    ir_stack.var(constructor.clone(), name, "");
                }
            },
            TypedExpr::Fn { args, body, .. } => {
                let mut body_stack = ir_stack.in_new_scope();

                self.build_ir(body, &mut body_stack);

                let mut params = args
                    .iter()
                    .map(|arg| arg.arg_name.get_variable_name().unwrap_or("_").to_string())
                    .collect();

                ir_stack.anonymous_function(params, body_stack);
            }
            TypedExpr::List {
                elements,
                tail,
                tipo,
                ..
            } => {
                let stacks = Vec::new();
                for element in elements {
                    let mut stack = ir_stack.in_new_scope();

                    self.build_ir(element, &mut stack);

                    stacks.push(stack);
                }

                let tail = tail.as_ref().map(|tail| {
                    let mut tail_stack = ir_stack.in_new_scope();

                    self.build_ir(tail, &mut tail_stack);

                    tail_stack
                });

                ir_stack.list(tipo.clone(), stacks, tail);
            }
            TypedExpr::Call {
                fun, args, tipo, ..
            } => {
                match &**fun {
                    TypedExpr::Var { constructor, .. } => match &constructor.variant {
                        ValueConstructorVariant::Record {
                            name: constr_name, ..
                        } => {
                            if let Some(data_type) =
                                builder::lookup_data_type_by_tipo(self.data_types.clone(), tipo)
                            {
                                let (constr_index, _) = data_type
                                    .constructors
                                    .iter()
                                    .enumerate()
                                    .find(|(_, dt)| &dt.name == constr_name)
                                    .unwrap();

                                let Some(fun_arg_types) = fun.tipo().arg_types() else {
                                    unreachable!()
                                };

                                let stacks = Vec::new();

                                for (arg, func_type) in args.iter().zip(fun_arg_types) {
                                    let mut stack = ir_stack.in_new_scope();

                                    if func_type.is_data() && !arg.value.tipo().is_data() {
                                        stack.wrap_data(arg.value.tipo());
                                    }

                                    self.build_ir(&arg.value, &mut stack);

                                    stacks.push(stack);
                                }

                                ir_stack.record(constructor.tipo.clone(), constr_index, stacks);

                                return;
                            }
                        }
                        ValueConstructorVariant::ModuleFn {
                            builtin: Some(func),
                            ..
                        } => {
                            let Some(fun_arg_types) = fun.tipo().arg_types() else {unreachable!()};

                            let stacks = Vec::new();

                            for (arg, func_type) in args.iter().zip(fun_arg_types) {
                                let mut stack = ir_stack.in_new_scope();

                                if func_type.is_data() && !arg.value.tipo().is_data() {
                                    stack.wrap_data(arg.value.tipo());
                                }

                                self.build_ir(&arg.value, &mut stack);
                            }

                            ir_stack.builtin(*func, tipo.clone(), stacks);

                            return;
                        }
                        _ => {}
                    },
                    TypedExpr::ModuleSelect {
                        constructor,
                        module_name,
                        ..
                    } => match constructor {
                        ModuleValueConstructor::Record {
                            name: constr_name,
                            tipo,
                            ..
                        } => {
                            if let Some(data_type) =
                                builder::lookup_data_type_by_tipo(self.data_types.clone(), tipo)
                            {
                                let (constr_index, _) = data_type
                                    .constructors
                                    .iter()
                                    .enumerate()
                                    .find(|(_, dt)| &dt.name == constr_name)
                                    .unwrap();

                                let Some(fun_arg_types) = fun.tipo().arg_types() else {unreachable!()};

                                let mut stacks = Vec::new();

                                for (arg, func_type) in args.iter().zip(fun_arg_types) {
                                    let mut stack = ir_stack.in_new_scope();

                                    if func_type.is_data() && !arg.value.tipo().is_data() {
                                        stack.wrap_data(arg.value.tipo());
                                    }

                                    self.build_ir(&arg.value, &mut stack);
                                }

                                ir_stack.record(tipo.clone(), constr_index, stacks);

                                return;
                            }
                        }
                        ModuleValueConstructor::Fn { name, .. } => {
                            let type_info = self.module_types.get(module_name).unwrap();
                            let value = type_info.values.get(name).unwrap();

                            let ValueConstructorVariant::ModuleFn { builtin, .. } = &value.variant else {unreachable!()};

                            if let Some(func) = builtin {
                                let Some(fun_arg_types) = fun.tipo().arg_types() else {unreachable!()};

                                let mut stacks = Vec::new();
                                for (arg, func_type) in args.iter().zip(fun_arg_types) {
                                    let mut stack = ir_stack.in_new_scope();

                                    if func_type.is_data() && !arg.value.tipo().is_data() {
                                        stack.wrap_data(arg.value.tipo());
                                    }

                                    self.build_ir(&arg.value, &mut stack);
                                }

                                ir_stack.builtin(*func, tipo.clone(), stacks);

                                return;
                            }
                        }
                        _ => {}
                    },
                    _ => {}
                }

                let mut fun_stack = ir_stack.in_new_scope();

                self.build_ir(fun, &mut fun_stack);

                let fun_arg_types = fun.tipo().arg_types().unwrap_or_default();

                let mut stacks = Vec::new();
                for (arg, func_type) in args.iter().zip(fun_arg_types) {
                    let mut stack = ir_stack.in_new_scope();

                    if func_type.is_data() && !arg.value.tipo().is_data() {
                        stack.wrap_data(arg.value.tipo());
                    }

                    self.build_ir(&arg.value, &mut stack);
                }

                ir_stack.call(tipo.clone(), fun_stack, stacks);
            }
            TypedExpr::BinOp {
                name, left, right, ..
            } => {
                let mut left_stack = ir_stack.in_new_scope();
                let mut right_stack = ir_stack.in_new_scope();

                self.build_ir(left, &mut left_stack);
                self.build_ir(right, &mut right_stack);

                ir_stack.binop(*name, left.tipo(), left_stack, right_stack);
            }
            TypedExpr::Assignment {
                value,
                pattern,
                kind,
                tipo,
                ..
            } => {
                let mut value_stack = ir_stack.in_new_scope();
                let mut pattern_stack = ir_stack.in_new_scope();

                let mut replaced_type = tipo.clone();
                builder::replace_opaque_type(&mut replaced_type, self.data_types.clone());

                self.build_ir(value, &mut value_stack);

                self.assignment_ir(
                    pattern,
                    &mut pattern_stack,
                    value_stack,
                    &replaced_type,
                    AssignmentProperties {
                        value_type: value.tipo(),
                        kind: *kind,
                    },
                );

                ir_stack.merge(pattern_stack);
            }
            TypedExpr::When {
                subject, clauses, ..
            } => {
                let subject_name = format!("__subject_name_{}", self.id_gen.next());
                let constr_var = format!("__constr_name_{}", self.id_gen.next());

                let subject_tipo = subject.tipo();
                if clauses.len() <= 1 {
                    let mut value_vec: Vec<Air> = vec![];
                    let mut pattern_vec: Vec<Air> = vec![];
                    let mut subject_vec: Vec<Air> = vec![];

                    self.build_ir(&clauses[0].then, &mut value_vec, scope.clone());

                    self.build_ir(subject, &mut subject_vec, scope.clone());

                    self.assignment_ir(
                        &clauses[0].pattern,
                        &mut pattern_vec,
                        &mut subject_vec,
                        &subject_tipo,
                        AssignmentProperties {
                            value_type: clauses[0].then.tipo(),
                            kind: AssignmentKind::Let,
                        },
                        scope,
                    );

                    ir_stack.append(&mut pattern_vec);
                    ir_stack.append(&mut value_vec);
                } else {
                    // HERE TODO
                    let clauses = if subject_tipo.is_list() {
                        builder::rearrange_clauses(clauses.clone())
                    } else {
                        clauses.clone()
                    };

                    if let Some((last_clause, clauses)) = clauses.split_last() {
                        let mut pattern_vec = vec![];

                        let mut clause_properties = ClauseProperties::init(
                            &subject_tipo,
                            constr_var.clone(),
                            subject_name.clone(),
                        );

                        self.handle_each_clause(
                            &mut pattern_vec,
                            &mut clause_properties,
                            clauses,
                            &subject_tipo,
                            scope.clone(),
                        );

                        let last_pattern = &last_clause.pattern;

                        let mut final_scope = scope.clone();

                        final_scope.push(self.id_gen.next());

                        if !matches!(last_pattern, Pattern::Tuple { .. }) {
                            pattern_vec.push(Air::Finally {
                                scope: final_scope.clone(),
                            });
                        }

                        let mut final_clause_vec = vec![];

                        self.build_ir(
                            &last_clause.then,
                            &mut final_clause_vec,
                            final_scope.clone(),
                        );

                        *clause_properties.is_final_clause() = true;

                        self.when_ir(
                            last_pattern,
                            &mut pattern_vec,
                            &mut final_clause_vec,
                            &subject_tipo,
                            &mut clause_properties,
                            final_scope,
                        );

                        if *clause_properties.needs_constr_var() {
                            ir_stack.push(Air::Let {
                                scope: scope.clone(),
                                name: constr_var.clone(),
                            });
                            let mut subject_scope = scope.clone();
                            subject_scope.push(self.id_gen.next());

                            self.build_ir(subject, ir_stack, subject_scope.clone());

                            let mut scope = scope;
                            scope.push(self.id_gen.next());

                            ir_stack.push(Air::When {
                                scope: scope.clone(),
                                subject_name,
                                tipo: subject_tipo.clone(),
                            });

                            ir_stack.push(Air::Var {
                                scope,
                                constructor: ValueConstructor::public(
                                    subject_tipo,
                                    ValueConstructorVariant::LocalVariable {
                                        location: Span::empty(),
                                    },
                                ),
                                name: constr_var,
                                variant_name: String::new(),
                            })
                        } else {
                            ir_stack.push(Air::When {
                                scope: scope.clone(),
                                subject_name,
                                tipo: subject_tipo,
                            });

                            let mut scope = scope;
                            scope.push(self.id_gen.next());

                            self.build_ir(subject, ir_stack, scope);
                        }

                        ir_stack.append(&mut pattern_vec);
                    }
                }
            }
            TypedExpr::If {
                branches,
                final_else,
                tipo,
                ..
            } => {
                let mut if_ir = vec![];

                for (index, branch) in branches.iter().enumerate() {
                    let mut branch_scope = scope.clone();
                    branch_scope.push(self.id_gen.next());

                    if index == 0 {
                        if_ir.push(Air::If {
                            scope: scope.clone(),
                            tipo: tipo.clone(),
                        });
                    } else {
                        if_ir.push(Air::If {
                            scope: branch_scope.clone(),
                            tipo: tipo.clone(),
                        });
                    }
                    self.build_ir(&branch.condition, &mut if_ir, branch_scope.clone());

                    let mut branch_scope = scope.clone();
                    branch_scope.push(self.id_gen.next());

                    self.build_ir(&branch.body, &mut if_ir, branch_scope);
                }

                let mut branch_scope = scope;
                branch_scope.push(self.id_gen.next());

                self.build_ir(final_else, &mut if_ir, branch_scope);

                ir_stack.append(&mut if_ir);
            }
            TypedExpr::RecordAccess {
                record,
                index,
                tipo,
                ..
            } => {
                ir_stack.push(Air::RecordAccess {
                    scope: scope.clone(),
                    record_index: *index,
                    tipo: tipo.clone(),
                });

                self.build_ir(record, ir_stack, scope);
            }
            TypedExpr::ModuleSelect {
                constructor,
                module_name,
                tipo,
                ..
            } => match constructor {
                ModuleValueConstructor::Record { .. } => {
                    todo!("Records from modules not yet implemented.")
                }
                ModuleValueConstructor::Fn { name, module, .. } => {
                    let func = self.functions.get(&FunctionAccessKey {
                        module_name: module_name.clone(),
                        function_name: name.clone(),
                        variant_name: String::new(),
                    });

                    if let Some(func) = func {
                        ir_stack.push(Air::Var {
                            scope,
                            constructor: ValueConstructor::public(
                                tipo.clone(),
                                ValueConstructorVariant::ModuleFn {
                                    name: name.clone(),
                                    field_map: None,
                                    module: module.clone(),
                                    arity: func.arguments.len(),
                                    location: Span::empty(),
                                    builtin: None,
                                },
                            ),
                            name: format!("{module}_{name}"),
                            variant_name: String::new(),
                        });
                    } else {
                        let type_info = self.module_types.get(module_name).unwrap();
                        let value = type_info.values.get(name).unwrap();
                        match &value.variant {
                            ValueConstructorVariant::ModuleFn { builtin, .. } => {
                                if let Some(builtin) = builtin {
                                    ir_stack.push(Air::Builtin {
                                        func: *builtin,
                                        scope,
                                        tipo: tipo.clone(),
                                        count: 0,
                                    });
                                }
                            }
                            _ => unreachable!(),
                        }
                    }
                }
                ModuleValueConstructor::Constant { literal, .. } => {
                    builder::constants_ir(literal, ir_stack, scope);
                }
            },
            TypedExpr::RecordUpdate {
                spread, args, tipo, ..
            } => {
                let mut update_ir = vec![];
                let mut spread_scope = scope.clone();
                let mut index_types = vec![];
                let mut highest_index = 0;
                spread_scope.push(self.id_gen.next());

                self.build_ir(spread, &mut update_ir, spread_scope);

                for arg in args {
                    let mut arg_scope = scope.clone();
                    arg_scope.push(self.id_gen.next());

                    self.build_ir(&arg.value, &mut update_ir, arg_scope);

                    if arg.index > highest_index {
                        highest_index = arg.index;
                    }
                    index_types.push((arg.index, arg.value.tipo()));
                }

                ir_stack.push(Air::RecordUpdate {
                    scope,
                    highest_index,
                    indices: index_types,
                    tipo: tipo.clone(),
                });

                ir_stack.append(&mut update_ir);
            }
            TypedExpr::UnOp { value, op, .. } => {
                ir_stack.push(Air::UnOp {
                    scope: scope.clone(),
                    op: *op,
                });

                self.build_ir(value, ir_stack, scope);
            }
            TypedExpr::Tuple { elems, tipo, .. } => {
                ir_stack.push(Air::Tuple {
                    scope: scope.clone(),
                    tipo: tipo.clone(),
                    count: elems.len(),
                });

                let mut elems_air = vec![];

                for elem in elems {
                    let mut scope = scope.clone();
                    scope.push(self.id_gen.next());
                    self.build_ir(elem, &mut elems_air, scope);
                }

                ir_stack.append(&mut elems_air);
            }

            TypedExpr::Trace {
                tipo, then, text, ..
            } => {
                let mut scope = scope;

                ir_stack.push(Air::Trace {
                    tipo: tipo.clone(),
                    scope: scope.clone(),
                });

                scope.push(self.id_gen.next());
                self.build_ir(text, ir_stack, scope.clone());

                scope.push(self.id_gen.next());
                self.build_ir(then, ir_stack, scope);
            }

            TypedExpr::TupleIndex { index, tuple, .. } => {
                ir_stack.push(Air::TupleIndex {
                    scope: scope.clone(),
                    tipo: tuple.tipo(),
                    tuple_index: *index,
                });

                self.build_ir(tuple, ir_stack, scope);
            }

            TypedExpr::ErrorTerm { tipo, .. } => {
                ir_stack.push(Air::ErrorTerm {
                    scope,
                    tipo: tipo.clone(),
                });
            }
        }
    }

    fn handle_each_clause(
        &mut self,
        ir_stack: &mut Vec<Air>,
        clause_properties: &mut ClauseProperties,
        clauses: &[TypedClause],
        subject_type: &Arc<Type>,
        scope: Vec<u64>,
    ) {
        for (index, clause) in clauses.iter().enumerate() {
            // scope per clause is different
            let mut scope = scope.clone();
            scope.push(self.id_gen.next());

            // holds when clause pattern Air
            let mut clause_subject_vec = vec![];
            let mut clause_then_vec = vec![];

            // reset complex clause setting per clause back to default
            *clause_properties.is_complex_clause() = false;

            let mut clause_scope = scope.clone();
            clause_scope.push(self.id_gen.next());

            self.build_ir(&clause.then, &mut clause_then_vec, clause_scope);

            if let Some(clause_guard) = &clause.guard {
                let mut clause_guard_vec = vec![];
                *clause_properties.is_complex_clause() = true;
                let clause_guard_name = format!("__clause_guard_{}", self.id_gen.next());

                let mut clause_guard_scope = scope.clone();
                clause_guard_scope.push(self.id_gen.next());

                clause_guard_vec.push(Air::Let {
                    scope: clause_guard_scope.clone(),
                    name: clause_guard_name.clone(),
                });

                builder::handle_clause_guard(
                    clause_guard,
                    &mut clause_guard_vec,
                    clause_guard_scope.clone(),
                );

                clause_guard_vec.push(Air::ClauseGuard {
                    scope: clause_guard_scope.clone(),
                    subject_name: clause_guard_name,
                    tipo: bool(),
                });

                clause_guard_vec.push(Air::Bool {
                    scope: clause_guard_scope.clone(),
                    value: true,
                });

                clause_guard_vec.append(&mut clause_then_vec);
                clause_then_vec = clause_guard_vec;
            }

            match clause_properties {
                ClauseProperties::ConstrClause {
                    original_subject_name,
                    ..
                } => {
                    let subject_name = original_subject_name.clone();

                    let mut clause_scope = scope.clone();
                    clause_scope.push(self.id_gen.next());
                    self.when_ir(
                        &clause.pattern,
                        &mut clause_subject_vec,
                        &mut clause_then_vec,
                        subject_type,
                        clause_properties,
                        clause_scope,
                    );

                    let data_type =
                        builder::lookup_data_type_by_tipo(self.data_types.clone(), subject_type);

                    if let Some(data_type) = data_type {
                        if data_type.constructors.len() > 1 {
                            ir_stack.push(Air::Clause {
                                scope,
                                tipo: subject_type.clone(),
                                complex_clause: *clause_properties.is_complex_clause(),
                                subject_name,
                            });
                        } else {
                            ir_stack.push(Air::Clause {
                                scope: scope.clone(),
                                tipo: subject_type.clone(),
                                complex_clause: *clause_properties.is_complex_clause(),
                                subject_name,
                            });
                            let mut scope = scope;
                            scope.push(self.id_gen.next());

                            ir_stack.push(Air::Int {
                                scope,
                                value: "0".to_string(),
                            });
                        }
                    } else {
                        ir_stack.push(Air::Clause {
                            scope: scope.clone(),
                            tipo: subject_type.clone(),
                            complex_clause: *clause_properties.is_complex_clause(),
                            subject_name,
                        });
                    }
                }
                ClauseProperties::ListClause {
                    original_subject_name,
                    current_index,
                    ..
                } => {
                    let (current_clause_index, has_tail) =
                        if let Pattern::List { elements, tail, .. } = &clause.pattern {
                            (elements.len(), tail.is_some())
                        } else if let Pattern::Assign { pattern, .. } = &clause.pattern {
                            if let Pattern::List { elements, tail, .. } = pattern.as_ref() {
                                (elements.len(), tail.is_some())
                            } else {
                                unreachable!("{:#?}", pattern)
                            }
                        } else {
                            unreachable!("{:#?}", &clause.pattern)
                        };

                    let prev_index = *current_index;

                    let subject_name = if current_clause_index == 0 {
                        original_subject_name.clone()
                    } else {
                        format!("__tail_{}", current_clause_index - 1)
                    };

                    self.when_ir(
                        &clause.pattern,
                        &mut clause_subject_vec,
                        &mut clause_then_vec,
                        subject_type,
                        clause_properties,
                        scope.clone(),
                    );

                    let next_tail = if index == clauses.len() - 1 {
                        None
                    } else {
                        let next_list_size = if let Pattern::List { elements, .. } =
                            &clauses[index + 1].pattern
                        {
                            elements.len()
                        } else if let Pattern::Assign { pattern, .. } = &clauses[index + 1].pattern
                        {
                            if let Pattern::List { elements, .. } = pattern.as_ref() {
                                elements.len()
                            } else {
                                unreachable!("{:#?}", pattern)
                            }
                        } else {
                            unreachable!()
                        };

                        if next_list_size == current_clause_index {
                            None
                        } else {
                            Some(format!("__tail_{current_clause_index}"))
                        }
                    };

                    #[allow(clippy::bool_to_int_with_if)]
                    let minus_tail = if has_tail { 1 } else { 0 };

                    if current_clause_index as i64 - minus_tail == prev_index {
                        ir_stack.push(Air::WrapClause { scope });
                    } else {
                        ir_stack.push(Air::ListClause {
                            scope,
                            tipo: subject_type.clone(),
                            tail_name: subject_name,
                            next_tail_name: next_tail,
                            complex_clause: *clause_properties.is_complex_clause(),
                        });
                    }

                    match clause_properties {
                        ClauseProperties::ListClause { current_index, .. } => {
                            *current_index = current_clause_index as i64;
                        }
                        _ => unreachable!(),
                    }
                }
                ClauseProperties::TupleClause {
                    original_subject_name,
                    defined_tuple_indices,
                    ..
                } => {
                    let prev_defined_tuple_indices = defined_tuple_indices.clone();
                    let subject_name = original_subject_name.clone();

                    self.when_ir(
                        &clause.pattern,
                        &mut clause_subject_vec,
                        &mut clause_then_vec,
                        subject_type,
                        clause_properties,
                        scope.clone(),
                    );
                    let current_defined_tuple_indices = match clause_properties {
                        ClauseProperties::TupleClause {
                            defined_tuple_indices,
                            ..
                        } => defined_tuple_indices.clone(),
                        _ => unreachable!(),
                    };

                    let indices_to_define = current_defined_tuple_indices
                        .difference(&prev_defined_tuple_indices)
                        .cloned()
                        .collect();

                    ir_stack.push(Air::TupleClause {
                        scope,
                        tipo: subject_type.clone(),
                        indices: indices_to_define,
                        predefined_indices: prev_defined_tuple_indices,
                        subject_name,
                        count: subject_type.get_inner_types().len(),
                        complex_clause: *clause_properties.is_complex_clause(),
                    });
                }
            }

            ir_stack.append(&mut clause_subject_vec);
        }
    }

    fn when_ir(
        &mut self,
        pattern: &Pattern<PatternConstructor, Arc<Type>>,
        pattern_vec: &mut Vec<Air>,
        values: &mut Vec<Air>,
        tipo: &Type,
        clause_properties: &mut ClauseProperties,
        scope: Vec<u64>,
    ) {
        match pattern {
            Pattern::Int { value, .. } => {
                pattern_vec.push(Air::Int {
                    scope,
                    value: value.clone(),
                });

                pattern_vec.append(values);
            }
            Pattern::Var { name, .. } => {
                pattern_vec.push(Air::Void {
                    scope: scope.clone(),
                });
                pattern_vec.push(Air::Let {
                    scope: scope.clone(),
                    name: name.clone(),
                });

                pattern_vec.push(Air::Var {
                    scope,
                    constructor: ValueConstructor::public(
                        tipo.clone().into(),
                        ValueConstructorVariant::LocalVariable {
                            location: Span::empty(),
                        },
                    ),
                    name: clause_properties.original_subject_name().clone(),
                    variant_name: String::new(),
                });
                pattern_vec.append(values);
            }
            Pattern::Assign { name, pattern, .. } => {
                let mut new_vec = vec![];
                new_vec.push(Air::Let {
                    scope: scope.clone(),
                    name: name.clone(),
                });
                new_vec.push(Air::Var {
                    scope: scope.clone(),
                    constructor: ValueConstructor::public(
                        tipo.clone().into(),
                        ValueConstructorVariant::LocalVariable {
                            location: Span::empty(),
                        },
                    ),
                    name: clause_properties.original_subject_name().clone(),
                    variant_name: String::new(),
                });

                new_vec.append(values);

                self.when_ir(
                    pattern,
                    pattern_vec,
                    &mut new_vec,
                    tipo,
                    clause_properties,
                    scope,
                );
            }
            Pattern::Discard { .. } => {
                pattern_vec.push(Air::Void { scope });
                pattern_vec.append(values);
            }
            Pattern::List { elements, tail, .. } => {
                for element in elements {
                    builder::check_when_pattern_needs(element, clause_properties);
                }

                if let Some(tail) = tail {
                    builder::check_when_pattern_needs(tail, clause_properties);
                }
                *clause_properties.needs_constr_var() = false;

                pattern_vec.push(Air::Void {
                    scope: scope.clone(),
                });

                self.when_recursive_ir(
                    pattern,
                    pattern_vec,
                    values,
                    clause_properties,
                    tipo,
                    scope,
                );
            }
            Pattern::Constructor {
                arguments,
                name: constr_name,
                ..
            } => {
                let mut temp_clause_properties = clause_properties.clone();
                *temp_clause_properties.needs_constr_var() = false;

                if tipo.is_bool() {
                    pattern_vec.push(Air::Bool {
                        scope,
                        value: constr_name == "True",
                    });
                } else {
                    for arg in arguments {
                        builder::check_when_pattern_needs(&arg.value, &mut temp_clause_properties);
                    }

                    // find data type definition
                    let data_type =
                        builder::lookup_data_type_by_tipo(self.data_types.clone(), tipo).unwrap();

                    let (index, _) = data_type
                        .constructors
                        .iter()
                        .enumerate()
                        .find(|(_, dt)| &dt.name == constr_name)
                        .unwrap();

                    let mut new_vec = vec![Air::Var {
                        constructor: ValueConstructor::public(
                            tipo.clone().into(),
                            ValueConstructorVariant::LocalVariable {
                                location: Span::empty(),
                            },
                        ),
                        name: temp_clause_properties.clause_var_name().clone(),
                        scope: scope.clone(),
                        variant_name: String::new(),
                    }];

                    // if only one constructor, no need to check
                    if data_type.constructors.len() > 1 {
                        // push constructor Index
                        pattern_vec.push(Air::Int {
                            value: index.to_string(),
                            scope: scope.clone(),
                        });
                    }

                    if *temp_clause_properties.needs_constr_var() {
                        self.when_recursive_ir(
                            pattern,
                            pattern_vec,
                            &mut new_vec,
                            clause_properties,
                            tipo,
                            scope,
                        );
                    } else {
                        self.when_recursive_ir(
                            pattern,
                            pattern_vec,
                            &mut vec![],
                            clause_properties,
                            tipo,
                            scope,
                        );
                    }
                }
                pattern_vec.append(values);

                // unify clause properties
                *clause_properties.is_complex_clause() = *clause_properties.is_complex_clause()
                    || *temp_clause_properties.is_complex_clause();

                *clause_properties.needs_constr_var() = *clause_properties.needs_constr_var()
                    || *temp_clause_properties.needs_constr_var();
            }
            Pattern::Tuple { elems, .. } => {
                for elem in elems {
                    builder::check_when_pattern_needs(elem, clause_properties);
                }
                *clause_properties.needs_constr_var() = false;

                self.when_recursive_ir(
                    pattern,
                    pattern_vec,
                    &mut vec![],
                    clause_properties,
                    tipo,
                    scope,
                );

                pattern_vec.append(values);
            }
        }
    }

    fn when_recursive_ir(
        &mut self,
        pattern: &Pattern<PatternConstructor, Arc<Type>>,
        pattern_vec: &mut Vec<Air>,
        values: &mut Vec<Air>,
        clause_properties: &mut ClauseProperties,
        tipo: &Type,
        scope: Vec<u64>,
    ) {
        match pattern {
            Pattern::Int { .. } => unreachable!(),
            Pattern::Var { .. } => unreachable!(),
            Pattern::Assign { .. } => todo!("Nested assign not yet implemented"),
            Pattern::Discard { .. } => {
                pattern_vec.push(Air::Void { scope });

                pattern_vec.append(values);
            }
            Pattern::List { elements, tail, .. } => {
                let mut names = vec![];
                let mut nested_pattern = vec![];
                let items_type = &tipo.get_inner_types()[0];
                // let mut nested_pattern = vec![];
                for element in elements {
                    let name = self.nested_pattern_ir_and_label(
                        element,
                        &mut nested_pattern,
                        items_type,
                        scope.clone(),
                        *clause_properties.is_final_clause(),
                    );

                    names.push(name.unwrap_or_else(|| "_".to_string()))
                }

                let mut tail_name = String::new();

                if let Some(tail) = tail {
                    match &**tail {
                        Pattern::Var { name, .. } => {
                            tail_name = name.clone();
                        }
                        Pattern::Discard { .. } => {
                            tail_name = "_".to_string();
                        }
                        _ => unreachable!("Patterns in tail of list should not allow this"),
                    }
                }

                let tail_head_names = names
                    .iter()
                    .enumerate()
                    .filter(|(_, name)| *name != &"_".to_string())
                    .map(|(index, name)| {
                        if index == 0 {
                            (
                                clause_properties.original_subject_name().clone(),
                                name.clone(),
                            )
                        } else {
                            (format!("__tail_{}", index - 1), name.clone())
                        }
                    })
                    .collect_vec();

                if tail.is_some() && !elements.is_empty() {
                    let tail_var = if elements.len() == 1 {
                        clause_properties.original_subject_name().clone()
                    } else {
                        format!("__tail_{}", elements.len() - 2)
                    };

                    let tail = if &tail_name == "_" {
                        None
                    } else {
                        Some((tail_var, tail_name))
                    };

                    pattern_vec.push(Air::ListExpose {
                        scope,
                        tipo: tipo.clone().into(),
                        tail_head_names,
                        tail,
                    });
                } else if !elements.is_empty() {
                    pattern_vec.push(Air::ListExpose {
                        scope,
                        tipo: tipo.clone().into(),
                        tail_head_names,
                        tail: None,
                    });
                }

                pattern_vec.append(&mut nested_pattern);
                pattern_vec.append(values);
            }
            Pattern::Constructor {
                is_record,
                name: constr_name,
                arguments,
                constructor,
                tipo,
                ..
            } => {
                let data_type =
                    builder::lookup_data_type_by_tipo(self.data_types.clone(), tipo).unwrap();

                let (_, constructor_type) = data_type
                    .constructors
                    .iter()
                    .enumerate()
                    .find(|(_, dt)| &dt.name == constr_name)
                    .unwrap();
                let mut nested_pattern = vec![];
                if *is_record {
                    let field_map = match constructor {
                        PatternConstructor::Record { field_map, .. } => field_map.clone().unwrap(),
                    };

                    let mut type_map: IndexMap<String, Arc<Type>> = IndexMap::new();

                    for (index, arg) in tipo.arg_types().unwrap().iter().enumerate() {
                        let label = constructor_type.arguments[index].label.clone().unwrap();
                        let field_type = arg.clone();

                        type_map.insert(label, field_type);
                    }

                    let arguments_index = arguments
                        .iter()
                        .filter_map(|item| {
                            let label = item.label.clone().unwrap_or_default();
                            let field_index = field_map
                                .fields
                                .get(&label)
                                .map(|(index, _)| index)
                                .unwrap_or(&0);
                            let var_name = self.nested_pattern_ir_and_label(
                                &item.value,
                                &mut nested_pattern,
                                type_map.get(&label).unwrap_or(
                                    &Type::App {
                                        public: true,
                                        module: "".to_string(),
                                        name: "Discard".to_string(),
                                        args: vec![],
                                    }
                                    .into(),
                                ),
                                scope.clone(),
                                *clause_properties.is_final_clause(),
                            );

                            var_name.map_or(
                                Some((label.clone(), "_".to_string(), *field_index)),
                                |var_name| Some((label, var_name, *field_index)),
                            )
                        })
                        .sorted_by(|item1, item2| item1.2.cmp(&item2.2))
                        .collect::<Vec<(String, String, usize)>>();

                    if !arguments_index.is_empty() {
                        pattern_vec.push(Air::FieldsExpose {
                            indices: arguments_index
                                .iter()
                                .map(|(label, var_name, index)| {
                                    let field_type = type_map
                                        .get(label)
                                        .unwrap_or_else(|| type_map.get_index(*index).unwrap().1);
                                    (*index, var_name.clone(), field_type.clone())
                                })
                                .collect_vec(),
                            scope,
                            check_last_item: false,
                        });
                    }
                } else {
                    let mut type_map: IndexMap<usize, Arc<Type>> = IndexMap::new();

                    for (index, arg) in tipo.arg_types().unwrap().iter().enumerate() {
                        let field_type = arg.clone();

                        type_map.insert(index, field_type);
                    }

                    let arguments_index = arguments
                        .iter()
                        .enumerate()
                        .filter_map(|(index, item)| {
                            let var_name = self.nested_pattern_ir_and_label(
                                &item.value,
                                &mut nested_pattern,
                                type_map.get(&index).unwrap(),
                                scope.clone(),
                                *clause_properties.is_final_clause(),
                            );

                            var_name.map_or(Some(("_".to_string(), index)), |var_name| {
                                Some((var_name, index))
                            })
                        })
                        .collect::<Vec<(String, usize)>>();

                    if !arguments_index.is_empty() {
                        pattern_vec.push(Air::FieldsExpose {
                            indices: arguments_index
                                .iter()
                                .map(|(name, index)| {
                                    let field_type = type_map.get(index).unwrap();

                                    (*index, name.clone(), field_type.clone())
                                })
                                .collect_vec(),
                            scope,
                            check_last_item: false,
                        });
                    }
                }

                pattern_vec.append(values);
                pattern_vec.append(&mut nested_pattern);
            }
            Pattern::Tuple { elems, .. } => {
                let mut names = vec![];
                let mut nested_pattern = vec![];
                let items_type = &tipo.get_inner_types();

                for (index, element) in elems.iter().enumerate() {
                    let name = self.nested_pattern_ir_and_label(
                        element,
                        &mut nested_pattern,
                        &items_type[index],
                        scope.clone(),
                        *clause_properties.is_final_clause(),
                    );

                    names.push((name.unwrap_or_else(|| "_".to_string()), index))
                }
                let mut defined_indices = match clause_properties.clone() {
                    ClauseProperties::TupleClause {
                        defined_tuple_indices,
                        ..
                    } => defined_tuple_indices,
                    _ => unreachable!(),
                };

                let mut previous_defined_names = vec![];
                for (name, index) in names.clone() {
                    if let Some(defined_index) = defined_indices
                        .iter()
                        .find(|(defined_index, _)| *defined_index == index)
                    {
                        previous_defined_names.push(defined_index.clone());
                    } else {
                        defined_indices.insert((index, name));
                    }
                }

                for (index, name) in previous_defined_names {
                    let new_name = names
                        .iter()
                        .find(|(_, current_index)| *current_index == index)
                        .map(|(new_name, _)| new_name)
                        .unwrap();

                    let pattern_type = &tipo.get_inner_types()[index];

                    pattern_vec.push(Air::Let {
                        scope: scope.clone(),
                        name: new_name.clone(),
                    });
                    pattern_vec.push(Air::Var {
                        scope: scope.clone(),
                        constructor: ValueConstructor::public(
                            pattern_type.clone(),
                            ValueConstructorVariant::LocalVariable {
                                location: Span::empty(),
                            },
                        ),
                        name,
                        variant_name: String::new(),
                    });
                }

                match clause_properties {
                    ClauseProperties::TupleClause {
                        defined_tuple_indices,
                        ..
                    } => {
                        *defined_tuple_indices = defined_indices;
                    }
                    _ => unreachable!(),
                }

                pattern_vec.append(&mut nested_pattern);
                pattern_vec.append(values);
            }
        }
    }

    fn nested_pattern_ir_and_label(
        &mut self,
        pattern: &Pattern<PatternConstructor, Arc<Type>>,
        pattern_vec: &mut Vec<Air>,
        pattern_type: &Arc<Type>,
        scope: Vec<u64>,
        final_clause: bool,
    ) -> Option<String> {
        match pattern {
            Pattern::Var { name, .. } => Some(name.clone()),
            Pattern::Discard { .. } => None,
            a @ Pattern::List { elements, tail, .. } => {
                let item_name = format!("__list_item_id_{}", self.id_gen.next());
                let new_tail_name = "__tail".to_string();

                if elements.is_empty() {
                    pattern_vec.push(Air::ListClauseGuard {
                        scope: scope.clone(),
                        tipo: pattern_type.clone(),
                        tail_name: item_name.clone(),
                        next_tail_name: None,
                        inverse: false,
                    });

                    pattern_vec.push(Air::Void { scope });
                } else {
                    for (index, _) in elements.iter().enumerate() {
                        let prev_tail_name = if index == 0 {
                            item_name.clone()
                        } else {
                            format!("{}_{}", new_tail_name, index - 1)
                        };

                        let mut clause_properties = ClauseProperties::ListClause {
                            clause_var_name: item_name.clone(),
                            needs_constr_var: false,
                            is_complex_clause: false,
                            original_subject_name: item_name.clone(),
                            current_index: index as i64,
                            final_clause,
                        };

                        let tail_name = format!("{new_tail_name}_{index}");

                        if elements.len() - 1 == index {
                            if tail.is_some() {
                                pattern_vec.push(Air::ListClauseGuard {
                                    scope: scope.clone(),
                                    tipo: pattern_type.clone(),
                                    tail_name: prev_tail_name,
                                    next_tail_name: None,
                                    inverse: true,
                                });

                                self.when_ir(
                                    a,
                                    pattern_vec,
                                    &mut vec![],
                                    pattern_type,
                                    &mut clause_properties,
                                    scope.clone(),
                                );
                            } else {
                                pattern_vec.push(Air::ListClauseGuard {
                                    scope: scope.clone(),
                                    tipo: pattern_type.clone(),
                                    tail_name: prev_tail_name,
                                    next_tail_name: Some(tail_name.clone()),
                                    inverse: true,
                                });

                                pattern_vec.push(Air::Void {
                                    scope: scope.clone(),
                                });

                                pattern_vec.push(Air::ListClauseGuard {
                                    scope: scope.clone(),
                                    tipo: pattern_type.clone(),
                                    tail_name: tail_name.clone(),
                                    next_tail_name: None,
                                    inverse: false,
                                });

                                self.when_ir(
                                    a,
                                    pattern_vec,
                                    &mut vec![],
                                    pattern_type,
                                    &mut clause_properties,
                                    scope.clone(),
                                );
                            }
                        } else {
                            pattern_vec.push(Air::ListClauseGuard {
                                scope: scope.clone(),
                                tipo: pattern_type.clone(),
                                tail_name: prev_tail_name,
                                next_tail_name: Some(tail_name),
                                inverse: true,
                            });

                            pattern_vec.push(Air::Void {
                                scope: scope.clone(),
                            });
                        };
                    }
                }

                Some(item_name)
            }
            a @ Pattern::Constructor {
                tipo,
                name: constr_name,
                ..
            } => {
                let id = self.id_gen.next();
                let constr_var_name = format!("{constr_name}_{id}");
                let data_type =
                    builder::lookup_data_type_by_tipo(self.data_types.clone(), tipo).unwrap();

                if data_type.constructors.len() > 1 {
                    if final_clause {
                        pattern_vec.push(Air::Finally {
                            scope: scope.clone(),
                        });
                    } else {
                        pattern_vec.push(Air::ClauseGuard {
                            scope: scope.clone(),
                            tipo: tipo.clone(),
                            subject_name: constr_var_name.clone(),
                        });
                    }
                }

                let mut clause_properties = ClauseProperties::ConstrClause {
                    clause_var_name: constr_var_name.clone(),
                    needs_constr_var: false,
                    is_complex_clause: false,
                    original_subject_name: constr_var_name.clone(),
                    final_clause,
                };

                self.when_ir(
                    a,
                    pattern_vec,
                    &mut vec![],
                    tipo,
                    &mut clause_properties,
                    scope,
                );

                Some(constr_var_name)
            }
            a @ Pattern::Tuple { elems, .. } => {
                let item_name = format!("__tuple_item_id_{}", self.id_gen.next());

                let mut clause_properties = ClauseProperties::TupleClause {
                    clause_var_name: item_name.clone(),
                    needs_constr_var: false,
                    is_complex_clause: false,
                    original_subject_name: item_name.clone(),
                    defined_tuple_indices: IndexSet::new(),
                    final_clause,
                };

                let mut inner_pattern_vec = vec![];

                self.when_ir(
                    a,
                    &mut inner_pattern_vec,
                    &mut vec![],
                    pattern_type,
                    &mut clause_properties,
                    scope.clone(),
                );

                let defined_indices = match clause_properties.clone() {
                    ClauseProperties::TupleClause {
                        defined_tuple_indices,
                        ..
                    } => defined_tuple_indices,
                    _ => unreachable!(),
                };

                pattern_vec.push(Air::TupleClause {
                    scope,
                    tipo: pattern_type.clone(),
                    indices: defined_indices,
                    predefined_indices: IndexSet::new(),
                    subject_name: clause_properties.original_subject_name().to_string(),
                    count: elems.len(),
                    complex_clause: false,
                });

                pattern_vec.append(&mut inner_pattern_vec);

                Some(item_name)
            }
            Pattern::Assign { name, pattern, .. } => {
                let inner_name = self.nested_pattern_ir_and_label(
                    pattern,
                    pattern_vec,
                    pattern_type,
                    scope.clone(),
                    final_clause,
                );

                pattern_vec.push(Air::Let {
                    scope: scope.clone(),
                    name: name.clone(),
                });

                pattern_vec.push(Air::Var {
                    scope,
                    constructor: ValueConstructor::public(
                        pattern_type.clone(),
                        ValueConstructorVariant::LocalVariable {
                            location: Span::empty(),
                        },
                    ),
                    name: inner_name.clone().unwrap(),
                    variant_name: String::new(),
                });

                inner_name
            }
            Pattern::Int { .. } => {
                let error_message = "Nested pattern-match on integers isn't implemented yet. Use when clause-guard as an alternative, or break down the pattern.";
                todo!("{}", error_message)
            }
        }
    }

    fn assignment_ir(
        &mut self,
        pattern: &Pattern<PatternConstructor, Arc<Type>>,
        pattern_stack: &mut AirStack,
        value_stack: AirStack,
        tipo: &Type,
        assignment_properties: AssignmentProperties,
    ) {
        let value_stack = if assignment_properties.value_type.is_data()
            && !tipo.is_data()
            && !pattern.is_discard()
        {
            let mut wrap_stack = pattern_stack.in_new_scope();
            wrap_stack.un_wrap_data(tipo.clone().into());
            wrap_stack.merge(value_stack);
            wrap_stack
        } else if !assignment_properties.value_type.is_data()
            && tipo.is_data()
            && !pattern.is_discard()
        {
            let mut wrap_stack = pattern_stack.in_new_scope();
            wrap_stack.wrap_data(assignment_properties.value_type.clone());
            wrap_stack.merge(value_stack);
            wrap_stack
        } else {
            value_stack
        };

        match pattern {
            Pattern::Int { .. } => todo!(),
            Pattern::Var { name, .. } => {
                let assert_value_stack = value_stack.in_new_scope();
                pattern_stack.let_assignment(name, value_stack);

                if matches!(assignment_properties.kind, AssignmentKind::Expect)
                    && assignment_properties.value_type.is_data()
                    && !tipo.is_data()
                {
                    let mut assert_stack = pattern_stack.in_new_scope();

                    self.recursive_assert_pattern(
                        pattern,
                        &mut assert_stack,
                        assert_value_stack,
                        tipo,
                        assignment_properties,
                    );

                    pattern_stack.merge(assert_stack);
                }
            }
            Pattern::Assign { .. } => todo!("Assign not yet implemented"),
            Pattern::Discard { .. } => {
                pattern_stack.let_assignment("_", value_stack);
            }
            list @ Pattern::List { .. } => {
                if matches!(assignment_properties.kind, AssignmentKind::Expect)
                    && assignment_properties.value_type.is_data()
                    && !tipo.is_data()
                {
                    self.recursive_assert_pattern(
                        list,
                        pattern_vec,
                        value_vec,
                        tipo,
                        assignment_properties,
                        scope,
                    );
                } else {
                    self.pattern_ir(
                        list,
                        pattern_vec,
                        value_vec,
                        tipo,
                        assignment_properties,
                        scope,
                    );
                }
            }
            constr @ Pattern::Constructor { .. } => {
                if matches!(assignment_properties.kind, AssignmentKind::Expect)
                    && assignment_properties.value_type.is_data()
                    && !tipo.is_data()
                {
                    self.recursive_assert_pattern(
                        constr,
                        pattern_vec,
                        value_vec,
                        tipo,
                        assignment_properties,
                        scope,
                    );
                } else {
                    self.pattern_ir(
                        constr,
                        pattern_vec,
                        value_vec,
                        tipo,
                        assignment_properties,
                        scope,
                    );
                }
            }
            tuple @ Pattern::Tuple { .. } => {
                if matches!(assignment_properties.kind, AssignmentKind::Expect)
                    && assignment_properties.value_type.is_data()
                    && !tipo.is_data()
                {
                    self.recursive_assert_pattern(
                        tuple,
                        pattern_vec,
                        value_vec,
                        tipo,
                        assignment_properties,
                        scope,
                    );
                } else {
                    self.pattern_ir(
                        tuple,
                        pattern_vec,
                        value_vec,
                        tipo,
                        assignment_properties,
                        scope,
                    );
                }
            }
        }
    }

    fn pattern_ir(
        &mut self,
        pattern: &Pattern<PatternConstructor, Arc<Type>>,
        pattern_vec: &mut Vec<Air>,
        values: &mut Vec<Air>,
        tipo: &Type,
        assignment_properties: AssignmentProperties,
        scope: Vec<u64>,
    ) {
        match pattern {
            Pattern::Int { .. } => todo!(),
            Pattern::Var { .. } => todo!(),
            Pattern::Assign { .. } => todo!(),
            Pattern::Discard { .. } => todo!(),
            Pattern::List { elements, tail, .. } => {
                let mut elements_vec = vec![];
                let mut names = vec![];

                for element in elements {
                    match element {
                        Pattern::Var { name, .. } => {
                            names.push(name.clone());
                        }
                        a @ (Pattern::List { .. }
                        | Pattern::Constructor { .. }
                        | Pattern::Tuple { .. }) => {
                            let mut var_vec = vec![];

                            let item_name = format!("list_item_id_{}", self.id_gen.next());

                            names.push(item_name.clone());

                            let mut scope = scope.clone();

                            scope.push(self.id_gen.next());

                            var_vec.push(Air::Var {
                                constructor: ValueConstructor::public(
                                    Type::App {
                                        public: true,
                                        module: String::new(),
                                        name: String::new(),
                                        args: vec![],
                                    }
                                    .into(),
                                    ValueConstructorVariant::LocalVariable {
                                        location: Span::empty(),
                                    },
                                ),
                                name: item_name,
                                scope: scope.clone(),
                                variant_name: String::new(),
                            });
                            self.pattern_ir(
                                a,
                                &mut elements_vec,
                                &mut var_vec,
                                &tipo.get_inner_types()[0],
                                assignment_properties.clone(),
                                scope.clone(),
                            );
                        }
                        Pattern::Int { .. } => todo!(),
                        Pattern::Assign { .. } => todo!(),
                        Pattern::Discard { .. } => {
                            names.push("_".to_string());
                        }
                    }
                }

                if let Some(tail) = tail {
                    match &**tail {
                        Pattern::Var { name, .. } => names.push(name.clone()),
                        Pattern::Discard { .. } => {}
                        _ => unreachable!(),
                    }
                }

                if !names.is_empty() {
                    pattern_vec.push(Air::ListAccessor {
                        names,
                        tail: tail.is_some(),
                        scope,
                        tipo: tipo.clone().into(),
                        check_last_item: true,
                    });
                } else {
                    pattern_vec.push(Air::Let {
                        scope,
                        name: "_".to_string(),
                    })
                }

                pattern_vec.append(values);
                pattern_vec.append(&mut elements_vec);
            }
            Pattern::Constructor {
                arguments,
                constructor,
                tipo: constr_tipo,
                name: constr_name,
                ..
            } => {
                let mut nested_pattern = vec![];

                let field_map = match constructor {
                    PatternConstructor::Record { field_map, .. } => field_map.clone(),
                };

                let mut type_map: IndexMap<usize, Arc<Type>> = IndexMap::new();

                for (index, arg) in constr_tipo.arg_types().unwrap().iter().enumerate() {
                    let field_type = arg.clone();
                    type_map.insert(index, field_type);
                }

                let arguments_index = arguments
                    .iter()
                    .enumerate()
                    .filter_map(|(index, item)| {
                        let label = item.label.clone().unwrap_or_default();
                        let field_index = if let Some(field_map) = &field_map {
                            *field_map.fields.get(&label).map(|x| &x.0).unwrap_or(&index)
                        } else {
                            index
                        };
                        let mut inner_scope = scope.clone();
                        inner_scope.push(self.id_gen.next());
                        self.extract_arg_and_index(
                            &item.value,
                            field_index,
                            &mut nested_pattern,
                            type_map.get(&field_index).unwrap(),
                            &assignment_properties,
                            &inner_scope,
                        )
                        .map_or(Some(("_".to_string(), index)), Some)
                    })
                    .sorted_by(|item1, item2| item1.1.cmp(&item2.1))
                    .collect::<Vec<(String, usize)>>();

                if !arguments_index.is_empty() {
                    pattern_vec.push(Air::FieldsExpose {
                        indices: arguments_index
                            .iter()
                            .map(|(var_name, index)| {
                                let field_type = type_map.get(index).unwrap();
                                (*index, var_name.clone(), field_type.clone())
                            })
                            .collect_vec(),
                        scope: scope.clone(),
                        check_last_item: false,
                    });
                } else if !tipo.is_bool() {
                    pattern_vec.push(Air::Let {
                        scope: scope.clone(),
                        name: "_".to_string(),
                    });
                }

                match assignment_properties.kind {
                    AssignmentKind::Let => {
                        pattern_vec.append(values);
                    }
                    AssignmentKind::Expect => {
                        if tipo.is_bool() {
                            pattern_vec.push(Air::AssertBool {
                                scope,
                                is_true: constr_name == "True",
                            });

                            pattern_vec.append(values);
                        } else {
                            let data_type =
                                builder::lookup_data_type_by_tipo(self.data_types.clone(), tipo)
                                    .unwrap();

                            let (index, _) = data_type
                                .constructors
                                .iter()
                                .enumerate()
                                .find(|(_, constr)| &constr.name == constr_name)
                                .unwrap();

                            let constr_name = format!("__{}_{}", constr_name, self.id_gen.next());

                            let mut scope = scope;
                            scope.push(self.id_gen.next());

                            pattern_vec.push(Air::Let {
                                scope: scope.clone(),
                                name: constr_name.clone(),
                            });

                            pattern_vec.append(values);

                            pattern_vec.push(Air::AssertConstr {
                                scope: scope.clone(),
                                constr_index: index,
                            });

                            pattern_vec.push(Air::Var {
                                scope: scope.clone(),
                                constructor: ValueConstructor::public(
                                    tipo.clone().into(),
                                    ValueConstructorVariant::LocalVariable {
                                        location: Span::empty(),
                                    },
                                ),
                                name: constr_name.clone(),
                                variant_name: String::new(),
                            });

                            pattern_vec.push(Air::Var {
                                scope,
                                constructor: ValueConstructor::public(
                                    tipo.clone().into(),
                                    ValueConstructorVariant::LocalVariable {
                                        location: Span::empty(),
                                    },
                                ),
                                name: constr_name,
                                variant_name: String::new(),
                            });
                        }
                    }
                }

                pattern_vec.append(&mut nested_pattern);
            }
            Pattern::Tuple { elems, .. } => {
                let mut nested_pattern = vec![];
                let mut type_map: IndexMap<usize, Arc<Type>> = IndexMap::new();

                for (index, arg) in tipo.get_inner_types().iter().enumerate() {
                    let field_type = arg.clone();
                    type_map.insert(index, field_type);
                }

                let arguments_index = elems
                    .iter()
                    .enumerate()
                    .filter_map(|(tuple_index, item)| {
                        let mut inner_scope = scope.clone();
                        inner_scope.push(self.id_gen.next());
                        self.extract_arg_and_index(
                            item,
                            tuple_index,
                            &mut nested_pattern,
                            type_map.get(&tuple_index).unwrap(),
                            &assignment_properties,
                            &inner_scope,
                        )
                    })
                    .sorted_by(|item1, item2| item1.1.cmp(&item2.1))
                    .collect::<Vec<(String, usize)>>();

                if !arguments_index.is_empty() {
                    let mut current_index = 0;
                    let mut final_args = vec![];

                    for index in 0..elems.len() {
                        if arguments_index.get(current_index).is_some()
                            && arguments_index[current_index].1 == index
                        {
                            final_args.push(arguments_index.get(current_index).unwrap().clone());
                            current_index += 1;
                        } else {
                            final_args.push(("_".to_string(), index));
                        }
                    }

                    pattern_vec.push(Air::TupleAccessor {
                        scope,
                        names: final_args.into_iter().map(|(item, _)| item).collect_vec(),
                        tipo: tipo.clone().into(),
                        check_last_item: false,
                    });
                } else {
                    pattern_vec.push(Air::Let {
                        scope,
                        name: "_".to_string(),
                    });
                }

                pattern_vec.append(values);
                pattern_vec.append(&mut nested_pattern);
            }
        }
    }

    pub fn recursive_assert_pattern(
        &mut self,
        pattern: &Pattern<PatternConstructor, Arc<Type>>,
        assert_stack: &mut AirStack,
        value_stack: AirStack,
        tipo: &Type,
        assignment_properties: AssignmentProperties,
    ) {
        match pattern {
            Pattern::Int { .. } => unreachable!(),
            Pattern::Var { name, .. } => {
                assert_stack.merge(value_stack);

                self.recursive_assert_tipo(tipo, assert_stack, name);
            }
            Pattern::Assign { .. } => todo!(),
            Pattern::Discard { .. } => unreachable!(),
            Pattern::List { elements, tail, .. } => {
                let mut assert_list_vec = vec![];
                let inner_list_type = &tipo.get_inner_types()[0];
                let mut names = vec![];
                for element in elements {
                    match element {
                        Pattern::Var { name, .. } => {
                            names.push(name.clone());
                        }
                        Pattern::Assign { .. } => todo!(),
                        l @ (Pattern::List { .. }
                        | Pattern::Constructor { .. }
                        | Pattern::Tuple { .. }) => {
                            let name = format!("list_item_id_{}", self.id_gen.next());
                            names.push(name.clone());

                            self.recursive_assert_pattern(
                                l,
                                &mut assert_list_vec,
                                &mut vec![Air::Var {
                                    scope: scope.clone(),
                                    constructor: ValueConstructor::public(
                                        tipo.clone().into(),
                                        ValueConstructorVariant::LocalVariable {
                                            location: Span::empty(),
                                        },
                                    ),
                                    name,
                                    variant_name: String::new(),
                                }],
                                inner_list_type,
                                assignment_properties.clone(),
                                scope.clone(),
                            );
                        }
                        _ => {}
                    }
                }

                let name = if let Some(tail) = tail {
                    match &**tail {
                        Pattern::Var { name, .. } => name.clone(),
                        _ => format!("__tail_{}", self.id_gen.next()),
                    }
                } else {
                    format!("__tail_{}", self.id_gen.next())
                };

                self.recursive_assert_tipo(tipo, &mut assert_list_vec, &name, scope.clone());

                names.push(name);

                pattern_vec.push(Air::ListAccessor {
                    scope,
                    tipo: tipo.clone().into(),
                    names,
                    tail: true,
                    check_last_item: false,
                });

                pattern_vec.append(value_vec);

                pattern_vec.append(&mut assert_list_vec);
            }
            Pattern::Constructor {
                arguments,
                constructor,
                name: constr_name,
                tipo,
                ..
            } => {
                let mut nested_pattern = vec![];

                let field_map = match constructor {
                    PatternConstructor::Record { field_map, .. } => field_map.clone().unwrap(),
                };

                let data_type =
                    builder::lookup_data_type_by_tipo(self.data_types.clone(), tipo).unwrap();

                let (index, data_type_constr) = data_type
                    .constructors
                    .iter()
                    .enumerate()
                    .find(|(_, constr)| &constr.name == constr_name)
                    .unwrap();

                let mut type_map: IndexMap<usize, Arc<Type>> = IndexMap::new();

                for (index, arg) in tipo.arg_types().unwrap().iter().enumerate() {
                    let field_type = arg.clone();
                    type_map.insert(index, field_type);
                }

                let arguments_index = arguments
                    .iter()
                    .filter_map(|item| {
                        let label = item.label.clone().unwrap_or_default();
                        let field_index = field_map.fields.get(&label).map(|x| &x.0).unwrap_or(&0);
                        let mut inner_scope = scope.clone();
                        inner_scope.push(self.id_gen.next());
                        self.extract_arg_and_index(
                            &item.value,
                            *field_index,
                            &mut nested_pattern,
                            type_map.get(field_index).unwrap(),
                            &assignment_properties,
                            &inner_scope,
                        )
                    })
                    .sorted_by(|item1, item2| item1.1.cmp(&item2.1))
                    .collect::<Vec<(String, usize)>>();

                let total_fields = data_type_constr.arguments.len();
                let mut final_args = vec![];
                let mut current_index = 0;
                for index in 0..total_fields {
                    if arguments_index.get(current_index).is_some()
                        && arguments_index[current_index].1 == index
                    {
                        final_args.push(arguments_index.get(current_index).unwrap().clone());
                        current_index += 1;
                    } else {
                        let id_next = self.id_gen.next();
                        final_args.push((format!("__field_{index}_{id_next}"), index));
                        self.recursive_assert_tipo(
                            type_map.get(&index).unwrap(),
                            &mut nested_pattern,
                            &format!("__field_{index}_{id_next}"),
                            scope.clone(),
                        )
                    }
                }

                let constr_var = format!("__constr_{}", self.id_gen.next());
                pattern_vec.push(Air::Let {
                    scope: scope.clone(),
                    name: constr_var.clone(),
                });

                pattern_vec.append(value_vec);

                let mut scope = scope;
                scope.push(self.id_gen.next());

                pattern_vec.push(Air::AssertConstr {
                    scope: scope.clone(),
                    constr_index: index,
                });

                pattern_vec.push(Air::Var {
                    scope: scope.clone(),
                    constructor: ValueConstructor::public(
                        tipo.clone(),
                        ValueConstructorVariant::LocalVariable {
                            location: Span::empty(),
                        },
                    ),
                    name: constr_var.clone(),
                    variant_name: String::new(),
                });

                if !arguments_index.is_empty() {
                    pattern_vec.push(Air::FieldsExpose {
                        indices: final_args
                            .iter()
                            .map(|(var_name, index)| {
                                let field_type = type_map.get(index).unwrap();
                                (*index, var_name.clone(), field_type.clone())
                            })
                            .collect_vec(),
                        scope: scope.clone(),
                        check_last_item: true,
                    });

                    pattern_vec.push(Air::Var {
                        scope,
                        constructor: ValueConstructor::public(
                            tipo.clone(),
                            ValueConstructorVariant::LocalVariable {
                                location: Span::empty(),
                            },
                        ),
                        name: constr_var,
                        variant_name: String::new(),
                    });
                }

                pattern_vec.append(&mut nested_pattern);
            }
            Pattern::Tuple { elems, .. } => {
                let mut type_map: IndexMap<usize, Arc<Type>> = IndexMap::new();

                for (index, arg) in tipo.arg_types().unwrap().iter().enumerate() {
                    let field_type = arg.clone();
                    type_map.insert(index, field_type);
                }
                let mut nested_pattern = vec![];
                let arguments_index = elems
                    .iter()
                    .enumerate()
                    .filter_map(|(index, item)| {
                        let field_index = index;
                        let mut inner_scope = scope.clone();
                        inner_scope.push(self.id_gen.next());
                        self.extract_arg_and_index(
                            item,
                            field_index,
                            &mut nested_pattern,
                            type_map.get(&field_index).unwrap(),
                            &assignment_properties,
                            &inner_scope,
                        )
                    })
                    .sorted_by(|item1, item2| item1.1.cmp(&item2.1))
                    .collect::<Vec<(String, usize)>>();

                let total_fields = type_map.len();
                let mut final_args = vec![];
                let mut current_index = 0;
                for index in 0..total_fields {
                    if arguments_index.get(current_index).is_some()
                        && arguments_index[current_index].1 == index
                    {
                        final_args.push(arguments_index.get(current_index).unwrap().clone());
                        current_index += 1;
                    } else {
                        let id_next = self.id_gen.next();
                        final_args.push((format!("__tuple_{index}_{id_next}"), index));
                        self.recursive_assert_tipo(
                            type_map.get(&index).unwrap(),
                            &mut nested_pattern,
                            &format!("__tuple_{index}_{id_next}"),
                            scope.clone(),
                        )
                    }
                }

                if !final_args.is_empty() {
                    pattern_vec.push(Air::TupleAccessor {
                        scope,
                        names: final_args.into_iter().map(|(item, _)| item).collect_vec(),
                        tipo: tipo.clone().into(),
                        check_last_item: true,
                    });
                }

                pattern_vec.append(value_vec);

                pattern_vec.append(&mut nested_pattern);
            }
        }
    }

    fn recursive_assert_tipo(&mut self, tipo: &Type, assert_vec: &mut AirStack, name: &str) {
        let mut tipo = tipo.clone().into();
        builder::replace_opaque_type(&mut tipo, self.data_types.clone());

        if tipo.is_bool()
            || tipo.is_bytearray()
            || tipo.is_int()
            || tipo.is_string()
            || tipo.is_void()
            || tipo.get_generic().is_some()
            || tipo.is_data()
        {
        } else if tipo.is_map() {
            self.used_data_assert_on_list = true;
            let new_id = self.id_gen.next();
            let id_pair = (self.id_gen.next(), self.id_gen.next());
            let inner_list_type = &tipo.get_inner_types()[0];
            let inner_pair_types = inner_list_type.get_inner_types();

            assert_vec.push(Air::Builtin {
                scope: scope.clone(),
                func: DefaultFunction::ChooseUnit,
                tipo: tipo.clone(),
                count: DefaultFunction::ChooseUnit.arity(),
            });

            assert_vec.push(Air::Call {
                scope: scope.clone(),
                count: 2,
                tipo: tipo.clone(),
            });

            assert_vec.push(Air::Var {
                scope: scope.clone(),
                constructor: ValueConstructor::public(
                    tipo.clone(),
                    ValueConstructorVariant::LocalVariable {
                        location: Span::empty(),
                    },
                ),
                name: ASSERT_ON_LIST.to_string(),
                variant_name: String::new(),
            });

            assert_vec.push(Air::Var {
                scope: scope.clone(),
                constructor: ValueConstructor::public(
                    tipo.clone(),
                    ValueConstructorVariant::LocalVariable {
                        location: Span::empty(),
                    },
                ),
                name: name.to_owned(),
                variant_name: String::new(),
            });

            assert_vec.push(Air::Fn {
                scope: scope.clone(),
                params: vec![format!("__pair_{new_id}")],
            });

            assert_vec.push(Air::TupleAccessor {
                scope: scope.clone(),
                names: vec![
                    format!("__pair_fst_{}", id_pair.0),
                    format!("__pair_snd_{}", id_pair.1),
                ],
                tipo: inner_list_type.clone(),
                check_last_item: false,
            });

            assert_vec.push(Air::Var {
                scope: scope.clone(),
                constructor: ValueConstructor::public(
                    tipo.clone(),
                    ValueConstructorVariant::LocalVariable {
                        location: Span::empty(),
                    },
                ),
                name: format!("__pair_{new_id}"),
                variant_name: String::new(),
            });

            self.recursive_assert_tipo(
                &inner_pair_types[0],
                assert_vec,
                &format!("__pair_fst_{}", id_pair.0),
                scope.clone(),
            );

            self.recursive_assert_tipo(
                &inner_pair_types[1],
                assert_vec,
                &format!("__pair_snd_{}", id_pair.1),
                scope.clone(),
            );

            assert_vec.push(Air::Void { scope });
        } else if tipo.is_list() {
            self.used_data_assert_on_list = true;
            let new_id = self.id_gen.next();
            let inner_list_type = &tipo.get_inner_types()[0];

            assert_vec.push(Air::Builtin {
                scope: scope.clone(),
                func: DefaultFunction::ChooseUnit,
                tipo: tipo.clone(),
                count: DefaultFunction::ChooseUnit.arity(),
            });

            assert_vec.push(Air::Call {
                scope: scope.clone(),
                count: 2,
                tipo: tipo.clone(),
            });

            assert_vec.push(Air::Var {
                scope: scope.clone(),
                constructor: ValueConstructor::public(
                    tipo.clone(),
                    ValueConstructorVariant::LocalVariable {
                        location: Span::empty(),
                    },
                ),
                name: ASSERT_ON_LIST.to_string(),
                variant_name: String::new(),
            });

            assert_vec.push(Air::Var {
                scope: scope.clone(),
                constructor: ValueConstructor::public(
                    tipo.clone(),
                    ValueConstructorVariant::LocalVariable {
                        location: Span::empty(),
                    },
                ),
                name: name.to_owned(),
                variant_name: String::new(),
            });

            assert_vec.push(Air::Fn {
                scope: scope.clone(),
                params: vec![format!("__list_item_{new_id}")],
            });

            assert_vec.push(Air::Let {
                scope: scope.clone(),
                name: format!("__list_item_{new_id}"),
            });

            assert_vec.push(Air::UnWrapData {
                scope: scope.clone(),
                tipo: inner_list_type.clone(),
            });

            assert_vec.push(Air::Var {
                scope: scope.clone(),
                constructor: ValueConstructor::public(
                    tipo.clone(),
                    ValueConstructorVariant::LocalVariable {
                        location: Span::empty(),
                    },
                ),
                name: format!("__list_item_{new_id}"),
                variant_name: String::new(),
            });

            self.recursive_assert_tipo(
                inner_list_type,
                assert_vec,
                &format!("__list_item_{new_id}"),
                scope.clone(),
            );

            assert_vec.push(Air::Void { scope });
        } else if tipo.is_tuple() {
            let tuple_inner_types = tipo.get_inner_types();
            let mut new_id_list = vec![];
            for (index, _) in tuple_inner_types.iter().enumerate() {
                new_id_list.push((index, self.id_gen.next()));
            }

            assert_vec.push(Air::TupleAccessor {
                scope: scope.clone(),
                names: new_id_list
                    .iter()
                    .map(|(index, id)| format!("__tuple_index_{index}_{id}"))
                    .collect_vec(),
                tipo: tipo.clone(),
                check_last_item: true,
            });

            assert_vec.push(Air::Var {
                scope: scope.clone(),
                constructor: ValueConstructor::public(
                    tipo.clone(),
                    ValueConstructorVariant::LocalVariable {
                        location: Span::empty(),
                    },
                ),
                name: name.to_owned(),
                variant_name: String::new(),
            });

            for (index, name) in new_id_list
                .into_iter()
                .map(|(index, id)| (index, format!("__tuple_index_{index}_{id}")))
            {
                self.recursive_assert_tipo(
                    &tuple_inner_types[index],
                    assert_vec,
                    &name,
                    scope.clone(),
                );
            }
        } else {
            let data_type =
                builder::lookup_data_type_by_tipo(self.data_types.clone(), &tipo).unwrap();
            let new_id = self.id_gen.next();

            assert_vec.push(Air::Builtin {
                scope: scope.clone(),
                func: DefaultFunction::ChooseUnit,
                tipo: tipo.clone(),
                count: DefaultFunction::ChooseUnit.arity(),
            });

            assert_vec.push(Air::When {
                scope: scope.clone(),
                tipo: tipo.clone(),
                subject_name: format!("__subject_{new_id}"),
            });

            assert_vec.push(Air::Var {
                scope: scope.clone(),
                constructor: ValueConstructor::public(
                    tipo.clone(),
                    ValueConstructorVariant::LocalVariable {
                        location: Span::empty(),
                    },
                ),
                name: name.to_owned(),
                variant_name: String::new(),
            });

            for (index, constr) in data_type.constructors.iter().enumerate() {
                let arg_indices = constr
                    .arguments
                    .iter()
                    .enumerate()
                    .map(|(index, arg)| {
                        let arg_name = arg
                            .label
                            .clone()
                            .unwrap_or(format!("__field_{index}_{new_id}"));
                        (index, arg_name, arg.tipo.clone())
                    })
                    .collect_vec();

                assert_vec.push(Air::Clause {
                    scope: scope.clone(),
                    tipo: tipo.clone(),
                    subject_name: format!("__subject_{new_id}"),
                    complex_clause: false,
                });

                assert_vec.push(Air::Int {
                    scope: scope.clone(),
                    value: index.to_string(),
                });

                if !arg_indices.is_empty() {
                    assert_vec.push(Air::FieldsExpose {
                        scope: scope.clone(),
                        indices: arg_indices.clone(),
                        check_last_item: true,
                    });

                    assert_vec.push(Air::Var {
                        scope: scope.clone(),
                        constructor: ValueConstructor::public(
                            tipo.clone(),
                            ValueConstructorVariant::LocalVariable {
                                location: Span::empty(),
                            },
                        ),
                        name: name.to_owned(),
                        variant_name: String::new(),
                    });
                }

                for (_, name, tipo) in arg_indices {
                    self.recursive_assert_tipo(&tipo, assert_vec, &name, scope.clone());
                }

                assert_vec.push(Air::Void {
                    scope: scope.clone(),
                });
            }

            assert_vec.push(Air::Trace {
                scope: scope.clone(),
                tipo: tipo.clone(),
            });

            assert_vec.push(Air::String {
                scope: scope.clone(),
                value: "Constr index did not match any type variant".to_string(),
            });

            assert_vec.push(Air::ErrorTerm {
                scope,
                tipo: tipo.clone(),
            });
        }
    }

    fn extract_arg_and_index(
        &mut self,
        item: &Pattern<PatternConstructor, Arc<Type>>,
        field_index: usize,
        nested_pattern: &mut Vec<Air>,
        tipo: &Type,
        assignment_properties: &AssignmentProperties,
        scope: &[u64],
    ) -> Option<(String, usize)> {
        {
            let (discard, var_name) = match item {
                Pattern::Var { name, .. } => (false, name.clone()),
                Pattern::Discard { .. } => (true, "".to_string()),
                a @ Pattern::List { .. } => {
                    let id = self.id_gen.next();
                    let list_name = format!("__list_{id}");

                    if matches!(assignment_properties.kind, AssignmentKind::Expect)
                        && assignment_properties.value_type.is_data()
                        && !tipo.is_data()
                    {
                        self.recursive_assert_pattern(
                            a,
                            nested_pattern,
                            &mut vec![Air::Var {
                                scope: scope.to_owned(),
                                constructor: ValueConstructor::public(
                                    tipo.clone().into(),
                                    ValueConstructorVariant::LocalVariable {
                                        location: Span::empty(),
                                    },
                                ),
                                name: list_name.clone(),
                                variant_name: String::new(),
                            }],
                            tipo,
                            assignment_properties.clone(),
                            scope.to_owned(),
                        );
                    } else {
                        self.pattern_ir(
                            a,
                            nested_pattern,
                            &mut vec![Air::Var {
                                scope: scope.to_owned(),
                                constructor: ValueConstructor::public(
                                    tipo.clone().into(),
                                    ValueConstructorVariant::LocalVariable {
                                        location: Span::empty(),
                                    },
                                ),
                                name: list_name.clone(),
                                variant_name: String::new(),
                            }],
                            tipo,
                            assignment_properties.clone(),
                            scope.to_owned(),
                        );
                    }

                    (false, list_name)
                }
                a @ Pattern::Constructor {
                    tipo,
                    name: constr_name,
                    ..
                } => {
                    let id = self.id_gen.next();
                    let constr_name = format!("{constr_name}_{id}");

                    if matches!(assignment_properties.kind, AssignmentKind::Expect)
                        && assignment_properties.value_type.is_data()
                        && !tipo.is_data()
                    {
                        self.recursive_assert_pattern(
                            a,
                            nested_pattern,
                            &mut vec![Air::Var {
                                scope: scope.to_owned(),
                                constructor: ValueConstructor::public(
                                    tipo.clone(),
                                    ValueConstructorVariant::LocalVariable {
                                        location: Span::empty(),
                                    },
                                ),
                                name: constr_name.clone(),
                                variant_name: String::new(),
                            }],
                            tipo,
                            assignment_properties.clone(),
                            scope.to_owned(),
                        );
                    } else {
                        self.pattern_ir(
                            a,
                            nested_pattern,
                            &mut vec![Air::Var {
                                scope: scope.to_owned(),
                                constructor: ValueConstructor::public(
                                    tipo.clone(),
                                    ValueConstructorVariant::LocalVariable {
                                        location: Span::empty(),
                                    },
                                ),
                                name: constr_name.clone(),
                                variant_name: String::new(),
                            }],
                            tipo,
                            assignment_properties.clone(),
                            scope.to_owned(),
                        );
                    }

                    (false, constr_name)
                }
                a @ Pattern::Tuple { .. } => {
                    let id = self.id_gen.next();
                    let tuple_name = format!("__tuple_name_{id}");

                    if matches!(assignment_properties.kind, AssignmentKind::Expect)
                        && assignment_properties.value_type.is_data()
                        && !tipo.is_data()
                    {
                        self.recursive_assert_pattern(
                            a,
                            nested_pattern,
                            &mut vec![Air::Var {
                                scope: scope.to_owned(),
                                constructor: ValueConstructor::public(
                                    tipo.clone().into(),
                                    ValueConstructorVariant::LocalVariable {
                                        location: Span::empty(),
                                    },
                                ),
                                name: tuple_name.clone(),
                                variant_name: String::new(),
                            }],
                            tipo,
                            assignment_properties.clone(),
                            scope.to_owned(),
                        );
                    } else {
                        self.pattern_ir(
                            a,
                            nested_pattern,
                            &mut vec![Air::Var {
                                scope: scope.to_owned(),
                                constructor: ValueConstructor::public(
                                    tipo.clone().into(),
                                    ValueConstructorVariant::LocalVariable {
                                        location: Span::empty(),
                                    },
                                ),
                                name: tuple_name.clone(),
                                variant_name: String::new(),
                            }],
                            tipo,
                            assignment_properties.clone(),
                            scope.to_owned(),
                        );
                    }

                    (false, tuple_name)
                }
                Pattern::Int { .. } => todo!(),
                Pattern::Assign { .. } => todo!(),
            };

            if discard {
                None
            } else {
                Some((var_name, field_index))
            }
        }
    }

    fn define_ir(&mut self, ir_stack: &mut Vec<Air>) {
        let mut function_definitions = IndexMap::new();
        let mut func_index_map = IndexMap::new();

        let recursion_func_map = IndexMap::new();

        self.define_ir_recurse(
            ir_stack,
            &mut function_definitions,
            &mut func_index_map,
            recursion_func_map,
            false,
        );

        let mut final_func_dep_ir = IndexMap::new();
        let mut to_be_defined = IndexMap::new();

        let mut dependency_map = IndexMap::new();
        let mut dependency_vec = vec![];

        let mut func_keys = function_definitions
            .clone()
            .into_iter()
            .filter(|(_, val)| !val.defined_by_zero_arg)
            .map(|(key, val)| (key, val.defined_by_zero_arg))
            .collect_vec();

        // deal with function dependencies by sorting order in which we iter over them.
        while let Some(function) = func_keys.pop() {
            let funct_comp = function_definitions.get(&function.0).unwrap();
            if dependency_map.contains_key(&function.0) {
                dependency_map.shift_remove(&function.0);
            }
            dependency_map.insert(function.0, function.1);
            func_keys.extend(
                funct_comp
                    .dependencies
                    .iter()
                    .map(|key| {
                        (
                            key.clone(),
                            function_definitions.get(key).unwrap().defined_by_zero_arg,
                        )
                    })
                    .collect_vec(),
            );
        }

        dependency_vec.extend(
            dependency_map
                .iter()
                .filter(|(_, defined_in_zero_arg)| !**defined_in_zero_arg)
                .map(|(key, _)| key.clone())
                .collect_vec(),
        );

        for func in dependency_vec {
            if self.defined_functions.contains_key(&func) {
                continue;
            }
            let function_component = function_definitions.get(&func).unwrap();
            let func_scope = func_index_map.get(&func).unwrap();

            let mut dep_ir = vec![];

            if !function_component.args.is_empty() {
                // deal with function dependencies
                builder::handle_func_dependencies_ir(
                    &mut dep_ir,
                    function_component,
                    &function_definitions,
                    &mut self.defined_functions,
                    &func_index_map,
                    func_scope,
                    &mut to_be_defined,
                );
                final_func_dep_ir.insert(func, dep_ir);
            } else {
                // since zero arg functions are run at compile time we need to pull all deps
                // note anon functions are not included in the above. They exist in a function anyway
                let mut defined_functions = IndexMap::new();

                // deal with function dependencies in zero arg functions
                builder::handle_func_dependencies_ir(
                    &mut dep_ir,
                    function_component,
                    &function_definitions,
                    &mut defined_functions,
                    &func_index_map,
                    func_scope,
                    &mut to_be_defined,
                );

                let mut final_zero_arg_ir = dep_ir;
                final_zero_arg_ir.extend(function_component.ir.clone());

                self.convert_opaque_type_to_inner_ir(&mut final_zero_arg_ir);

                self.zero_arg_functions.insert(func, final_zero_arg_ir);
                // zero arg functions don't contain the dependencies since they are pre-evaluated
                // As such we add functions to defined only after dependencies for all other functions are calculated
            }
        }

        while let Some(func) = to_be_defined.pop() {
            let mut dep_ir = vec![];
            let mut defined_functions = IndexMap::new();

            // deal with function dependencies in zero arg functions
            let funt_comp = function_definitions.get(&func.0).unwrap();
            let func_scope = func_index_map.get(&func.0).unwrap();

            builder::handle_func_dependencies_ir(
                &mut dep_ir,
                funt_comp,
                &function_definitions,
                &mut defined_functions,
                &func_index_map,
                func_scope,
                &mut to_be_defined,
            );

            let mut final_zero_arg_ir = dep_ir;
            final_zero_arg_ir.extend(funt_comp.ir.clone());

            self.convert_opaque_type_to_inner_ir(&mut final_zero_arg_ir);

            self.zero_arg_functions.insert(func.0, final_zero_arg_ir);
        }

        for (index, ir) in ir_stack.clone().into_iter().enumerate().rev() {
            {
                let temp_func_index_map = func_index_map.clone();
                let to_insert = final_func_dep_ir
                    .iter()
                    .filter_map(|(func_key, _)| {
                        temp_func_index_map
                            .get(func_key)
                            .map(|scope| (func_key.clone(), scope.clone()))
                    })
                    .filter(|func| {
                        builder::get_common_ancestor(&func.1, &ir.scope()) == ir.scope()
                            && !self.defined_functions.contains_key(&func.0)
                            && !self.zero_arg_functions.contains_key(&func.0)
                            && !(*dependency_map.get(&func.0).unwrap())
                    })
                    .collect_vec();

                for (function_access_key, scopes) in to_insert.into_iter().rev() {
                    func_index_map.remove(&function_access_key);

                    self.defined_functions
                        .insert(function_access_key.clone(), ());

                    let mut full_func_ir =
                        final_func_dep_ir.get(&function_access_key).unwrap().clone();

                    let func_comp = function_definitions
                        .get(&function_access_key)
                        .unwrap()
                        .clone();

                    // zero arg functions are not recursive
                    if !func_comp.args.is_empty() {
                        let mut recursion_ir = vec![];
                        builder::handle_recursion_ir(
                            &function_access_key,
                            &func_comp,
                            &mut recursion_ir,
                        );

                        full_func_ir.push(Air::DefineFunc {
                            scope: scopes.clone(),
                            func_name: function_access_key.function_name.clone(),
                            module_name: function_access_key.module_name.clone(),
                            params: func_comp.args.clone(),
                            recursive: func_comp.recursive,
                            variant_name: function_access_key.variant_name.clone(),
                        });

                        full_func_ir.extend(recursion_ir);

                        for ir in full_func_ir.into_iter().rev() {
                            ir_stack.insert(index, ir);
                        }
                    } else {
                        full_func_ir.extend(func_comp.ir.clone());

                        self.zero_arg_functions
                            .insert(function_access_key, full_func_ir);
                    }
                }
            }
        }
    }

    fn define_ir_recurse(
        &mut self,
        ir_stack: &mut [Air],
        func_components: &mut IndexMap<FunctionAccessKey, FuncComponents>,
        func_index_map: &mut IndexMap<FunctionAccessKey, Vec<u64>>,
        mut recursion_func_map: IndexMap<FunctionAccessKey, ()>,
        in_zero_arg_func: bool,
    ) {
        self.define_ir_processor(ir_stack, func_components, func_index_map, in_zero_arg_func);

        let mut recursion_func_map_to_add = recursion_func_map.clone();

        for func_index in func_index_map.clone().iter() {
            let func = func_index.0;

            let function_components = func_components.get_mut(func).unwrap();
            let mut function_ir = function_components.ir.clone();
            let in_zero_arg = function_components.args.is_empty() || in_zero_arg_func;
            let mut skip = false;

            for ir in function_ir.clone() {
                if let Air::Var {
                    constructor:
                        ValueConstructor {
                            variant:
                                ValueConstructorVariant::ModuleFn {
                                    name: func_name,
                                    module,
                                    ..
                                },
                            ..
                        },
                    variant_name,
                    ..
                } = ir
                {
                    if recursion_func_map.contains_key(&FunctionAccessKey {
                        module_name: module.clone(),
                        function_name: func_name.clone(),
                        variant_name: variant_name.clone(),
                    }) && func.clone()
                        == (FunctionAccessKey {
                            module_name: module.clone(),
                            function_name: func_name.clone(),
                            variant_name: variant_name.clone(),
                        })
                    {
                        skip = true;
                    } else if func.clone()
                        == (FunctionAccessKey {
                            module_name: module.clone(),
                            function_name: func_name.clone(),
                            variant_name: variant_name.clone(),
                        })
                    {
                        recursion_func_map_to_add.insert(
                            FunctionAccessKey {
                                module_name: module.clone(),
                                function_name: func_name.clone(),
                                variant_name: variant_name.clone(),
                            },
                            (),
                        );
                    }
                }
            }

            recursion_func_map = recursion_func_map_to_add.clone();
            if !skip {
                let mut inner_func_components = IndexMap::new();

                let mut inner_func_index_map = IndexMap::new();

                self.define_ir_recurse(
                    &mut function_ir,
                    &mut inner_func_components,
                    &mut inner_func_index_map,
                    recursion_func_map.clone(),
                    in_zero_arg,
                );

                function_components.ir = function_ir;

                //now unify
                for item in inner_func_components {
                    if let Some(entry) = func_components.get_mut(&item.0) {
                        entry.defined_by_zero_arg =
                            entry.defined_by_zero_arg && item.1.defined_by_zero_arg
                    } else {
                        func_components.insert(item.0, item.1);
                    }
                }

                for item in inner_func_index_map {
                    if let Some(entry) = func_index_map.get_mut(&item.0) {
                        *entry = builder::get_common_ancestor(entry, &item.1);
                    } else {
                        func_index_map.insert(item.0, item.1);
                    }
                }
            }
        }
    }

    fn define_ir_processor(
        &mut self,
        ir_stack: &mut [Air],
        func_components: &mut IndexMap<FunctionAccessKey, FuncComponents>,
        func_index_map: &mut IndexMap<FunctionAccessKey, Vec<u64>>,
        in_zero_arg_func: bool,
    ) {
        let mut to_be_defined_map: IndexMap<FunctionAccessKey, Vec<u64>> = IndexMap::new();
        for (index, ir) in ir_stack.to_vec().iter().enumerate().rev() {
            match ir {
                Air::Var {
                    scope, constructor, ..
                } => {
                    if let ValueConstructorVariant::ModuleFn {
                        name,
                        module,
                        builtin: None,
                        ..
                    } = &constructor.variant
                    {
                        let non_variant_function_key = FunctionAccessKey {
                            module_name: module.clone(),
                            function_name: name.clone(),
                            variant_name: String::new(),
                        };

                        let function = *self.functions.get(&non_variant_function_key).unwrap();

                        let mut func_ir = vec![];

                        self.build_ir(&function.body, &mut func_ir, scope.to_vec());

                        let param_types = constructor.tipo.arg_types().unwrap();

                        let mut mono_types: IndexMap<u64, Arc<Type>> = IndexMap::new();
                        let mut map = mono_types.into_iter().collect_vec();

                        for (index, arg) in function.arguments.iter().enumerate() {
                            if arg.tipo.is_generic() {
                                let param_type = &param_types[index];

                                map.append(&mut builder::get_generic_id_and_type(
                                    &arg.tipo, param_type,
                                ));
                            }
                        }

                        if function.return_type.is_generic() {
                            if let Type::Fn { ret, .. } = &*constructor.tipo {
                                map.append(&mut builder::get_generic_id_and_type(
                                    &function.return_type,
                                    ret,
                                ))
                            }
                        }

                        mono_types = map.into_iter().collect();

                        let (variant_name, func_ir) =
                            builder::monomorphize(func_ir, mono_types, &constructor.tipo);

                        let function_key = FunctionAccessKey {
                            module_name: module.clone(),
                            function_name: non_variant_function_key.function_name,
                            variant_name: variant_name.clone(),
                        };

                        ir_stack[index] = Air::Var {
                            scope: scope.clone(),
                            constructor: constructor.clone(),
                            name: name.clone(),
                            variant_name: variant_name.clone(),
                        };

                        if let Some(scope_prev) = to_be_defined_map.get(&function_key) {
                            let new_scope = builder::get_common_ancestor(scope, scope_prev);

                            to_be_defined_map.insert(function_key, new_scope);
                        } else if func_components.get(&function_key).is_some() {
                            to_be_defined_map.insert(function_key.clone(), scope.to_vec());
                        } else {
                            to_be_defined_map.insert(function_key.clone(), scope.to_vec());
                            let mut func_calls = IndexMap::new();

                            for ir in func_ir.clone().into_iter() {
                                if let Air::Var {
                                    constructor:
                                        ValueConstructor {
                                            variant:
                                                ValueConstructorVariant::ModuleFn {
                                                    name: func_name,
                                                    module,
                                                    ..
                                                },
                                            tipo,
                                            ..
                                        },
                                    ..
                                } = ir
                                {
                                    let current_func = FunctionAccessKey {
                                        module_name: module.clone(),
                                        function_name: func_name.clone(),
                                        variant_name: String::new(),
                                    };

                                    let current_func_as_variant = FunctionAccessKey {
                                        module_name: module.clone(),
                                        function_name: func_name.clone(),
                                        variant_name: variant_name.clone(),
                                    };

                                    let function = self.functions.get(&current_func);
                                    if function_key.clone() == current_func_as_variant {
                                        func_calls.insert(current_func_as_variant, ());
                                    } else if let (Some(function), Type::Fn { .. }) =
                                        (function, &*tipo)
                                    {
                                        let param_types = tipo.arg_types().unwrap();

                                        let mut mono_types: IndexMap<u64, Arc<Type>> =
                                            IndexMap::new();
                                        let mut map = mono_types.into_iter().collect_vec();

                                        for (index, arg) in function.arguments.iter().enumerate() {
                                            if arg.tipo.is_generic() {
                                                let param_type = &param_types[index];

                                                map.append(&mut builder::get_generic_id_and_type(
                                                    &arg.tipo, param_type,
                                                ));
                                            }
                                        }

                                        if function.return_type.is_generic() {
                                            if let Type::Fn { ret, .. } = &*constructor.tipo {
                                                map.append(&mut builder::get_generic_id_and_type(
                                                    &function.return_type,
                                                    ret,
                                                ))
                                            }
                                        }

                                        mono_types = map.into_iter().collect();
                                        let mut func_ir = vec![];

                                        self.build_ir(&function.body, &mut func_ir, scope.to_vec());

                                        let (variant_name, _) =
                                            builder::monomorphize(func_ir, mono_types, &tipo);

                                        func_calls.insert(
                                            FunctionAccessKey {
                                                module_name: current_func.module_name,
                                                function_name: current_func.function_name,
                                                variant_name,
                                            },
                                            (),
                                        );
                                    } else {
                                        func_calls.insert(current_func, ());
                                    }
                                }
                            }

                            let mut args = vec![];

                            for arg in function.arguments.iter() {
                                match &arg.arg_name {
                                    ArgName::Named { name, .. } => {
                                        args.push(name.clone());
                                    }
                                    _ => {
                                        args.push("_".to_string());
                                    }
                                }
                            }

                            let recursive = if func_calls.get(&function_key).is_some() {
                                func_calls.remove(&function_key);
                                true
                            } else {
                                false
                            };

                            func_components.insert(
                                function_key,
                                FuncComponents {
                                    ir: func_ir,
                                    dependencies: func_calls.keys().cloned().collect_vec(),
                                    recursive,
                                    args,
                                    defined_by_zero_arg: in_zero_arg_func,
                                },
                            );
                        }
                    } else {
                        for func in to_be_defined_map.clone().iter() {
                            if builder::get_common_ancestor(scope, func.1) == scope.to_vec() {
                                if let Some(index_scope) = func_index_map.get(func.0) {
                                    if builder::get_common_ancestor(index_scope, func.1)
                                        == scope.to_vec()
                                    {
                                        func_index_map.insert(func.0.clone(), scope.clone());
                                        to_be_defined_map.shift_remove(func.0);
                                    } else {
                                        to_be_defined_map.insert(
                                            func.0.clone(),
                                            builder::get_common_ancestor(index_scope, func.1),
                                        );
                                    }
                                } else {
                                    func_index_map.insert(func.0.clone(), scope.clone());
                                    to_be_defined_map.shift_remove(func.0);
                                }
                            }
                        }
                    }
                }
                a => {
                    let scope = a.scope();

                    for func in to_be_defined_map.clone().iter() {
                        if builder::get_common_ancestor(&scope, func.1) == scope.to_vec() {
                            if let Some(index_scope) = func_index_map.get(func.0) {
                                if builder::get_common_ancestor(index_scope, func.1)
                                    == scope.to_vec()
                                {
                                    func_index_map.insert(func.0.clone(), scope.clone());
                                    to_be_defined_map.shift_remove(func.0);
                                } else {
                                    to_be_defined_map.insert(
                                        func.0.clone(),
                                        builder::get_common_ancestor(index_scope, func.1),
                                    );
                                }
                            } else {
                                func_index_map.insert(func.0.clone(), scope.clone());
                                to_be_defined_map.shift_remove(func.0);
                            }
                        }
                    }
                }
            }
        }

        // Still to be defined
        for func in to_be_defined_map.clone().iter() {
            let index_scope = func_index_map.get(func.0).unwrap();
            func_index_map.insert(
                func.0.clone(),
                builder::get_common_ancestor(func.1, index_scope),
            );
        }
    }

    fn convert_opaque_type_to_inner_ir(&mut self, ir_stack: &mut Vec<Air>) {
        let mut indices_to_remove = vec![];
        for (index, ir) in ir_stack.clone().into_iter().enumerate() {
            match ir {
                Air::Var {
                    scope,
                    constructor,
                    name,
                    variant_name,
                } => {
                    let mut replaced_type = constructor.tipo.clone();
                    builder::replace_opaque_type(&mut replaced_type, self.data_types.clone());

                    ir_stack[index] = Air::Var {
                        scope,
                        constructor: ValueConstructor {
                            public: constructor.public,
                            variant: constructor.variant,
                            tipo: replaced_type,
                        },
                        name,
                        variant_name,
                    };
                }
                Air::List {
                    tipo,
                    scope,
                    count,
                    tail,
                } => {
                    let mut replaced_type = tipo.clone();
                    builder::replace_opaque_type(&mut replaced_type, self.data_types.clone());

                    ir_stack[index] = Air::List {
                        scope,
                        tipo: replaced_type,
                        count,
                        tail,
                    };
                }
                Air::ListAccessor {
                    tipo,
                    scope,
                    names,
                    tail,
                    check_last_item,
                } => {
                    let mut replaced_type = tipo.clone();
                    builder::replace_opaque_type(&mut replaced_type, self.data_types.clone());

                    ir_stack[index] = Air::ListAccessor {
                        scope,
                        tipo: replaced_type,
                        names,
                        tail,
                        check_last_item,
                    };
                }
                Air::ListExpose {
                    tipo,
                    scope,
                    tail_head_names,
                    tail,
                } => {
                    let mut replaced_type = tipo.clone();
                    builder::replace_opaque_type(&mut replaced_type, self.data_types.clone());

                    ir_stack[index] = Air::ListExpose {
                        scope,
                        tipo: replaced_type,
                        tail_head_names,
                        tail,
                    };
                }
                Air::Builtin {
                    tipo,
                    scope,
                    func,
                    count,
                } => {
                    let mut replaced_type = tipo.clone();
                    builder::replace_opaque_type(&mut replaced_type, self.data_types.clone());

                    ir_stack[index] = Air::Builtin {
                        scope,
                        func,
                        count,
                        tipo: replaced_type,
                    };
                }
                Air::BinOp { tipo, scope, name } => {
                    let mut replaced_type = tipo.clone();
                    builder::replace_opaque_type(&mut replaced_type, self.data_types.clone());

                    ir_stack[index] = Air::BinOp {
                        scope,
                        name,
                        tipo: replaced_type,
                    };
                }
                Air::When {
                    tipo,
                    scope,
                    subject_name,
                } => {
                    let mut replaced_type = tipo.clone();
                    builder::replace_opaque_type(&mut replaced_type, self.data_types.clone());

                    ir_stack[index] = Air::When {
                        scope,
                        tipo: replaced_type,
                        subject_name,
                    };
                }
                Air::Clause {
                    tipo,
                    scope,
                    subject_name,
                    complex_clause,
                } => {
                    let mut replaced_type = tipo.clone();
                    builder::replace_opaque_type(&mut replaced_type, self.data_types.clone());

                    ir_stack[index] = Air::Clause {
                        scope,
                        tipo: replaced_type,
                        subject_name,
                        complex_clause,
                    };
                }
                Air::ListClause {
                    tipo,
                    scope,
                    tail_name,
                    next_tail_name,
                    complex_clause,
                } => {
                    let mut replaced_type = tipo.clone();
                    builder::replace_opaque_type(&mut replaced_type, self.data_types.clone());

                    ir_stack[index] = Air::ListClause {
                        scope,
                        tipo: replaced_type,
                        tail_name,
                        next_tail_name,
                        complex_clause,
                    };
                }
                Air::TupleClause {
                    tipo,
                    scope,
                    indices,
                    predefined_indices,
                    subject_name,
                    count,
                    complex_clause,
                } => {
                    let mut replaced_type = tipo.clone();
                    builder::replace_opaque_type(&mut replaced_type, self.data_types.clone());

                    ir_stack[index] = Air::TupleClause {
                        scope,
                        tipo: replaced_type,
                        indices,
                        predefined_indices,
                        subject_name,
                        count,
                        complex_clause,
                    };
                }
                Air::ClauseGuard {
                    tipo,
                    scope,
                    subject_name,
                } => {
                    let mut replaced_type = tipo.clone();
                    builder::replace_opaque_type(&mut replaced_type, self.data_types.clone());

                    ir_stack[index] = Air::ClauseGuard {
                        scope,
                        subject_name,
                        tipo: replaced_type,
                    };
                }
                Air::ListClauseGuard {
                    tipo,
                    scope,
                    tail_name,
                    next_tail_name,
                    inverse,
                } => {
                    let mut replaced_type = tipo.clone();
                    builder::replace_opaque_type(&mut replaced_type, self.data_types.clone());

                    ir_stack[index] = Air::ListClauseGuard {
                        scope,
                        tipo: replaced_type,
                        tail_name,
                        next_tail_name,
                        inverse,
                    };
                }
                Air::Tuple { tipo, scope, count } => {
                    let mut replaced_type = tipo.clone();
                    builder::replace_opaque_type(&mut replaced_type, self.data_types.clone());

                    ir_stack[index] = Air::Tuple {
                        scope,
                        tipo: replaced_type,
                        count,
                    };
                }
                Air::TupleIndex {
                    tipo,
                    scope,
                    tuple_index,
                } => {
                    let mut replaced_type = tipo.clone();
                    builder::replace_opaque_type(&mut replaced_type, self.data_types.clone());

                    ir_stack[index] = Air::TupleIndex {
                        scope,
                        tipo: replaced_type,
                        tuple_index,
                    };
                }
                Air::ErrorTerm { tipo, scope } => {
                    let mut replaced_type = tipo.clone();
                    builder::replace_opaque_type(&mut replaced_type, self.data_types.clone());

                    ir_stack[index] = Air::ErrorTerm {
                        scope,
                        tipo: replaced_type,
                    };
                }
                Air::Trace { tipo, scope } => {
                    let mut replaced_type = tipo.clone();
                    builder::replace_opaque_type(&mut replaced_type, self.data_types.clone());

                    ir_stack[index] = Air::Trace {
                        scope,
                        tipo: replaced_type,
                    };
                }
                Air::TupleAccessor {
                    tipo,
                    scope,
                    names,
                    check_last_item,
                } => {
                    let mut replaced_type = tipo.clone();
                    builder::replace_opaque_type(&mut replaced_type, self.data_types.clone());

                    ir_stack[index] = Air::TupleAccessor {
                        scope,
                        names,
                        tipo: replaced_type,
                        check_last_item,
                    };
                }
                Air::RecordUpdate {
                    highest_index,
                    indices,
                    scope,
                    tipo,
                } => {
                    let mut new_indices = vec![];
                    for (ind, tipo) in indices {
                        let mut replaced_type = tipo.clone();
                        builder::replace_opaque_type(&mut replaced_type, self.data_types.clone());
                        new_indices.push((ind, replaced_type));
                    }

                    ir_stack[index] = Air::RecordUpdate {
                        scope,
                        indices: new_indices,
                        highest_index,
                        tipo,
                    };
                }
                Air::Record {
                    tag: constr_index,
                    tipo,
                    count,
                    scope,
                } => {
                    if builder::check_replaceable_opaque_type(&tipo, &self.data_types) {
                        indices_to_remove.push(index);
                    } else {
                        let mut replaced_type = tipo.clone();
                        builder::replace_opaque_type(&mut replaced_type, self.data_types.clone());

                        ir_stack[index] = Air::Record {
                            scope,
                            tag: constr_index,
                            tipo: replaced_type,
                            count,
                        };
                    }
                }
                Air::RecordAccess {
                    record_index,
                    tipo,
                    scope,
                } => {
                    let record = ir_stack[index + 1].clone();
                    let record_type = record.tipo();
                    if let Some(record_type) = record_type {
                        if builder::check_replaceable_opaque_type(&record_type, &self.data_types) {
                            indices_to_remove.push(index);
                        } else {
                            let mut replaced_type = tipo.clone();
                            builder::replace_opaque_type(
                                &mut replaced_type,
                                self.data_types.clone(),
                            );

                            ir_stack[index] = Air::RecordAccess {
                                scope,
                                record_index,
                                tipo: replaced_type,
                            };
                        }
                    } else {
                        let mut replaced_type = tipo.clone();
                        builder::replace_opaque_type(&mut replaced_type, self.data_types.clone());

                        ir_stack[index] = Air::RecordAccess {
                            scope,
                            record_index,
                            tipo: replaced_type,
                        };
                    }
                }
                Air::FieldsExpose {
                    indices,
                    scope,
                    check_last_item,
                } => {
                    let record = ir_stack[index + 1].clone();
                    let record_type = record.tipo();
                    if let Some(record_type) = record_type {
                        if builder::check_replaceable_opaque_type(&record_type, &self.data_types) {
                            ir_stack[index] = Air::Let {
                                scope,
                                name: indices[0].1.clone(),
                            };
                        } else {
                            let mut new_indices = vec![];
                            for (ind, name, tipo) in indices {
                                let mut replaced_type = tipo.clone();
                                builder::replace_opaque_type(
                                    &mut replaced_type,
                                    self.data_types.clone(),
                                );
                                new_indices.push((ind, name, replaced_type));
                            }

                            ir_stack[index] = Air::FieldsExpose {
                                scope,
                                indices: new_indices,
                                check_last_item,
                            };
                        }
                    } else {
                        let mut new_indices = vec![];
                        for (ind, name, tipo) in indices {
                            let mut replaced_type = tipo.clone();
                            builder::replace_opaque_type(
                                &mut replaced_type,
                                self.data_types.clone(),
                            );
                            new_indices.push((ind, name, replaced_type));
                        }

                        ir_stack[index] = Air::FieldsExpose {
                            scope,
                            indices: new_indices,
                            check_last_item,
                        };
                    }
                }
                Air::Call { scope, count, tipo } => {
                    let mut replaced_type = tipo.clone();
                    builder::replace_opaque_type(&mut replaced_type, self.data_types.clone());

                    ir_stack[index] = Air::Call {
                        scope,
                        tipo: replaced_type,
                        count,
                    };
                }
                Air::If { scope, tipo } => {
                    let mut replaced_type = tipo.clone();
                    builder::replace_opaque_type(&mut replaced_type, self.data_types.clone());

                    ir_stack[index] = Air::If {
                        scope,
                        tipo: replaced_type,
                    };
                }
                Air::UnWrapData { scope, tipo } => {
                    let mut replaced_type = tipo.clone();
                    builder::replace_opaque_type(&mut replaced_type, self.data_types.clone());

                    ir_stack[index] = Air::UnWrapData {
                        scope,
                        tipo: replaced_type,
                    };
                }
                Air::WrapData { scope, tipo } => {
                    let mut replaced_type = tipo.clone();
                    builder::replace_opaque_type(&mut replaced_type, self.data_types.clone());

                    ir_stack[index] = Air::WrapData {
                        scope,
                        tipo: replaced_type,
                    };
                }
                _ => {}
            }
        }

        for index in indices_to_remove.into_iter().rev() {
            ir_stack.remove(index);
        }
    }

    fn uplc_code_gen(&mut self, ir_stack: &mut Vec<Air>) -> Term<Name> {
        let mut arg_stack: Vec<Term<Name>> = vec![];

        while let Some(ir_element) = ir_stack.pop() {
            self.gen_uplc(ir_element, &mut arg_stack);
        }
        arg_stack[0].clone()
    }

    fn gen_uplc(&mut self, ir: Air, arg_stack: &mut Vec<Term<Name>>) {
        match ir {
            Air::Int { value, .. } => {
                arg_stack.push(Term::integer(value.parse().unwrap()));
            }
            Air::String { value, .. } => {
                arg_stack.push(Term::string(value));
            }
            Air::ByteArray { bytes, .. } => {
                arg_stack.push(Term::byte_string(bytes));
            }
            Air::Bool { value, .. } => {
                arg_stack.push(Term::bool(value));
            }
            Air::Var {
                name,
                constructor,
                variant_name,
                ..
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
                        unreachable!()
                    }
                    ValueConstructorVariant::ModuleFn {
                        name: func_name,
                        module,
                        ..
                    } => {
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
                    ValueConstructorVariant::Record {
                        name: constr_name, ..
                    } => {
                        if constructor.tipo.is_bool() {
                            arg_stack.push(Term::bool(constr_name == "True"));
                        } else if constructor.tipo.is_void() {
                            arg_stack.push(Term::Constant(UplcConstant::Unit.into()));
                        } else {
                            let data_type = builder::lookup_data_type_by_tipo(
                                self.data_types.clone(),
                                &constructor.tipo,
                            )
                            .unwrap();

                            let (constr_index, _) = data_type
                                .constructors
                                .iter()
                                .enumerate()
                                .find(|(_, x)| x.name == *constr_name)
                                .unwrap();

                            let fields = Term::empty_list();

                            let term = Term::constr_data()
                                .apply(Term::integer(constr_index.try_into().unwrap()))
                                .apply(fields);

                            arg_stack.push(term);
                        }
                    }
                };
            }
            Air::Void { .. } => arg_stack.push(Term::Constant(UplcConstant::Unit.into())),
            Air::List {
                count, tipo, tail, ..
            } => {
                let mut args = vec![];

                for _ in 0..count {
                    let arg = arg_stack.pop().unwrap();
                    args.push(arg);
                }
                let mut constants = vec![];
                for arg in &args {
                    if let Term::Constant(c) = arg {
                        constants.push(c.clone())
                    }
                }

                let list_type = tipo.get_inner_types()[0].clone();

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
                                    .zip(convert_values.into_iter())
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
                        arg_stack.pop().unwrap()
                    } else if tipo.is_map() {
                        Term::empty_map()
                    } else {
                        Term::empty_list()
                    };

                    for arg in args.into_iter().rev() {
                        let list_item = if tipo.is_map() {
                            arg
                        } else {
                            builder::convert_type_to_data(arg, &list_type)
                        };
                        term = Term::mk_cons().apply(list_item).apply(term);
                    }
                    arg_stack.push(term);
                }
            }
            Air::ListAccessor {
                names,
                tail,
                tipo,
                check_last_item,
                ..
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

                term = builder::list_access_to_uplc(
                    &names,
                    &id_list,
                    tail,
                    0,
                    term,
                    inner_types,
                    check_last_item,
                    true,
                )
                .apply(value);

                arg_stack.push(term);
            }
            Air::ListExpose {
                tail_head_names,
                tail,
                tipo,
                ..
            } => {
                let mut term = arg_stack.pop().unwrap();

                if let Some((tail_var, tail_name)) = tail {
                    term = term
                        .lambda(tail_name)
                        .apply(Term::tail_list().apply(Term::var(tail_var)));
                }

                for (tail_var, head_name) in tail_head_names.into_iter().rev() {
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
            Air::Fn { params, .. } => {
                let mut term = arg_stack.pop().unwrap();

                for param in params.iter().rev() {
                    term = term.lambda(param);
                }

                arg_stack.push(term);
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
                    let mut anon_func = true;

                    if let Term::Var(name) = term.clone() {
                        let text = &name.text;

                        for (
                            FunctionAccessKey {
                                module_name,
                                function_name,
                                variant_name,
                            },
                            ir,
                        ) in zero_arg_functions.into_iter()
                        {
                            let name_module =
                                format!("{module_name}_{function_name}{variant_name}");
                            let name = format!("{function_name}{variant_name}");
                            if text == &name || text == &name_module {
                                let mut term = self.uplc_code_gen(&mut ir.clone());
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

                                let eval_program: Program<NamedDeBruijn> =
                                    program.try_into().unwrap();

                                let evaluated_term: Term<NamedDeBruijn> =
                                    eval_program.eval(ExBudget::default()).result().unwrap();

                                arg_stack.push(evaluated_term.try_into().unwrap());
                                anon_func = false;
                            }
                        }
                    }
                    if anon_func {
                        arg_stack.push(term);
                    }
                }
            }
            Air::Builtin {
                func, tipo, count, ..
            } => {
                let mut term: Term<Name> = Term::Builtin(func);
                for _ in 0..func.force_count() {
                    term = term.force();
                }

                let mut arg_vec = vec![];
                for _ in 0..count {
                    arg_vec.push(arg_stack.pop().unwrap());
                }

                for (index, arg) in arg_vec.into_iter().enumerate() {
                    let arg = if matches!(func, DefaultFunction::ChooseData) && index > 0 {
                        arg.delay()
                    } else {
                        arg
                    };
                    term = term.apply(arg.clone());
                }

                match func {
                    DefaultFunction::FstPair
                    | DefaultFunction::SndPair
                    | DefaultFunction::HeadList => {
                        let temp_var = format!("__item_{}", self.id_gen.next());

                        if count == 0 {
                            term = term.apply(Term::var(temp_var.clone()));
                        }

                        term = builder::convert_data_to_type(term, &tipo);

                        if count == 0 {
                            term = term.lambda(temp_var);
                        }
                    }
                    DefaultFunction::UnConstrData => {
                        let temp_tuple = format!("__unconstr_tuple_{}", self.id_gen.next());

                        let temp_var = format!("__item_{}", self.id_gen.next());

                        if count == 0 {
                            term = term.apply(Term::var(temp_var.clone()));
                        }

                        term = Term::mk_pair_data()
                            .apply(
                                Term::i_data()
                                    .apply(Term::fst_pair().apply(Term::var(temp_tuple.clone()))),
                            )
                            .apply(
                                Term::list_data()
                                    .apply(Term::snd_pair().apply(Term::var(temp_tuple.clone()))),
                            )
                            .lambda(temp_tuple)
                            .apply(term);

                        if count == 0 {
                            term = term.lambda(temp_var);
                        }
                    }
                    DefaultFunction::MkCons => {
                        unimplemented!("Use brackets instead.");
                    }
                    DefaultFunction::IfThenElse
                    | DefaultFunction::ChooseList
                    | DefaultFunction::Trace => unimplemented!("{func:#?}"),
                    DefaultFunction::ChooseData => {
                        let temp_vars = (0..func.arity())
                            .map(|_| format!("__item_{}", self.id_gen.next()))
                            .collect_vec();

                        if count == 0 {
                            for (index, temp_var) in temp_vars.iter().enumerate() {
                                term = term.apply(if index > 0 {
                                    Term::var(temp_var.clone()).delay()
                                } else {
                                    Term::var(temp_var.clone())
                                });
                            }
                        }

                        term = term.force();

                        if count == 0 {
                            for temp_var in temp_vars.into_iter().rev() {
                                term = term.lambda(temp_var);
                            }
                        }
                    }
                    _ => {}
                }
                arg_stack.push(term);
            }
            Air::BinOp { name, tipo, .. } => {
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
                                arg_stack.push(Term::bool(true));
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
                module_name,
                variant_name,
                ..
            } => {
                let func_name = if module_name.is_empty() {
                    format!("{func_name}{variant_name}")
                } else {
                    format!("{module_name}_{func_name}{variant_name}")
                };
                let mut func_body = arg_stack.pop().unwrap();

                let mut term = arg_stack.pop().unwrap();

                for param in params.iter().rev() {
                    func_body = func_body.lambda(param.clone());
                }

                if !recursive {
                    term = term.lambda(func_name).apply(func_body);

                    arg_stack.push(term);
                } else {
                    func_body = func_body.lambda(func_name.clone());

                    term = term
                        .lambda(func_name.clone())
                        .apply(Term::var(func_name.clone()).apply(Term::var(func_name.clone())))
                        .lambda(func_name)
                        .apply(func_body);

                    arg_stack.push(term);
                }
            }
            Air::Let { name, .. } => {
                let arg = arg_stack.pop().unwrap();

                let mut term = arg_stack.pop().unwrap();

                term = term.lambda(name).apply(arg);

                arg_stack.push(term);
            }
            Air::UnWrapData { tipo, .. } => {
                let mut term = arg_stack.pop().unwrap();

                term = builder::convert_data_to_type(term, &tipo);

                arg_stack.push(term);
            }
            Air::WrapData { tipo, .. } => {
                let mut term = arg_stack.pop().unwrap();

                term = builder::convert_type_to_data(term, &tipo);

                arg_stack.push(term);
            }
            Air::AssertConstr { constr_index, .. } => {
                self.needs_field_access = true;
                let constr = arg_stack.pop().unwrap();

                let mut term = arg_stack.pop().unwrap();

                let error_term =
                    Term::Error.trace(Term::string("Expected on incorrect constructor variant."));

                term = Term::equals_integer()
                    .apply(Term::integer(constr_index.into()))
                    .apply(Term::var(CONSTR_INDEX_EXPOSER).apply(constr))
                    .delayed_if_else(term, error_term);

                arg_stack.push(term);
            }
            Air::AssertBool { is_true, .. } => {
                let value = arg_stack.pop().unwrap();
                let mut term = arg_stack.pop().unwrap();

                let error_term =
                    Term::Error.trace(Term::string("Expected on incorrect boolean variant"));

                if is_true {
                    term = value.delayed_if_else(term, error_term)
                } else {
                    term = value.delayed_if_else(error_term, term)
                }
                arg_stack.push(term);
            }
            Air::When {
                subject_name, tipo, ..
            } => {
                self.needs_field_access = true;
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
                    Term::var(CONSTR_INDEX_EXPOSER).apply(subject)
                };

                let mut term = arg_stack.pop().unwrap();

                term = term.lambda(subject_name).apply(subject);

                arg_stack.push(term);
            }
            Air::Clause {
                tipo,
                subject_name,
                complex_clause,
                ..
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
                        term.clone()
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
                // discard to pop off
                let _ = arg_stack.pop().unwrap();

                let body = arg_stack.pop().unwrap();
                let mut term = arg_stack.pop().unwrap();

                let arg = if let Some(next_tail_name) = next_tail_name {
                    term.lambda(next_tail_name)
                        .apply(Term::tail_list().apply(Term::var(tail_name.clone())))
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
            Air::WrapClause { .. } => {
                let _ = arg_stack.pop().unwrap();

                let mut term = arg_stack.pop().unwrap();
                let arg = arg_stack.pop().unwrap();

                term = term.lambda("__other_clauses_delayed").apply(arg.delay());

                arg_stack.push(term);
            }
            Air::ClauseGuard {
                subject_name, tipo, ..
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
                // discard to pop off
                let _ = arg_stack.pop().unwrap();

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
            Air::Finally { .. } => {
                let _clause = arg_stack.pop().unwrap();
            }
            Air::If { .. } => {
                let condition = arg_stack.pop().unwrap();
                let then = arg_stack.pop().unwrap();
                let mut term = arg_stack.pop().unwrap();

                term = condition.delayed_if_else(then, term);

                arg_stack.push(term);
            }
            Air::Record {
                tag: constr_index,
                tipo,
                count,
                ..
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

                if arg_vec.iter().all(|item| matches!(item, Term::Constant(_))) {
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
            Air::RecordAccess {
                record_index, tipo, ..
            } => {
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
                ..
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

                term = if !indices.is_empty() {
                    builder::list_access_to_uplc(
                        &names,
                        &id_list,
                        false,
                        current_index,
                        term,
                        inner_types,
                        check_last_item,
                        false,
                    )
                } else {
                    term
                };

                term = term.apply(Term::var(CONSTR_FIELDS_EXPOSER).apply(value));

                arg_stack.push(term);
            }
            Air::Tuple { tipo, count, .. } => {
                let mut args = vec![];

                for _ in 0..count {
                    let arg = arg_stack.pop().unwrap();
                    args.push(arg);
                }
                let mut constants = vec![];
                for arg in &args {
                    if let Term::Constant(c) = arg {
                        constants.push(c.clone())
                    }
                }

                let tuple_sub_types = tipo.get_inner_types();

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
                ..
            } => {
                self.needs_field_access = true;
                let tail_name_prefix = "__tail_index";

                let record = arg_stack.pop().unwrap();

                let mut args = IndexMap::new();
                let mut unchanged_field_indices = vec![];
                let mut prev_index = 0;
                for (index, tipo) in indices
                    .iter()
                    .sorted_by(|(index1, _), (index2, _)| index1.cmp(index2))
                    .rev()
                {
                    let arg = arg_stack.pop().unwrap();
                    args.insert(*index, (tipo.clone(), arg));

                    for field_index in prev_index..*index {
                        unchanged_field_indices.push(field_index);
                    }
                    prev_index = *index;
                }

                unchanged_field_indices.reverse();

                let mut term = Term::tail_list()
                    .apply(Term::var(format!("{tail_name_prefix}_{highest_index}")));

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

                if !unchanged_field_indices.is_empty() {
                    prev_index = highest_index;
                    for index in unchanged_field_indices.into_iter() {
                        let tail_name = format!("{tail_name_prefix}_{prev_index}");
                        let prev_tail_name = format!("{tail_name_prefix}_{index}");

                        let mut tail_list = Term::var(prev_tail_name);

                        if index < prev_index {
                            for _ in index..prev_index {
                                tail_list = Term::tail_list().apply(tail_list);
                            }

                            term = term.lambda(tail_name).apply(tail_list);
                        }
                        prev_index = index;
                    }
                }
                let tail_name = format!("{tail_name_prefix}_{prev_index}");
                let prev_tail_name = format!("{tail_name_prefix}_0");

                let mut tail_list = Term::var(prev_tail_name.clone());

                for _ in 0..prev_index {
                    tail_list = Term::tail_list().apply(tail_list);
                }
                if prev_index != 0 {
                    term = term.lambda(tail_name).apply(tail_list);
                }

                term = term
                    .lambda(prev_tail_name)
                    .apply(Term::var(CONSTR_FIELDS_EXPOSER).apply(record));

                arg_stack.push(term);
            }
            Air::UnOp { op, .. } => {
                let value = arg_stack.pop().unwrap();

                let term = match op {
                    UnOp::Not => value.if_else(Term::bool(false), Term::bool(true)),
                    UnOp::Negate => Term::sub_integer()
                        .apply(Term::integer(0.into()))
                        .apply(value),
                };

                arg_stack.push(term);
            }
            Air::TupleIndex {
                tipo, tuple_index, ..
            } => {
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
                ..
            } => {
                let inner_types = tipo.get_inner_types();
                let value = arg_stack.pop().unwrap();
                let mut term = arg_stack.pop().unwrap();
                let list_id = self.id_gen.next();

                if names.len() == 2 {
                    term = term
                        .lambda(names[1].clone())
                        .apply(builder::convert_data_to_type(
                            Term::snd_pair().apply(Term::var(format!("__tuple_{list_id}"))),
                            &inner_types[1],
                        ))
                        .lambda(names[0].clone())
                        .apply(builder::convert_data_to_type(
                            Term::fst_pair().apply(Term::var(format!("__tuple_{list_id}"))),
                            &inner_types[0],
                        ))
                        .lambda(format!("__tuple_{list_id}"))
                        .apply(value);
                } else {
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
                    )
                    .apply(value);
                }

                arg_stack.push(term);
            }
            Air::Trace { .. } => {
                let text = arg_stack.pop().unwrap();

                let term = arg_stack.pop().unwrap();

                let term = term.trace(text);

                arg_stack.push(term);
            }
            Air::ErrorTerm { .. } => arg_stack.push(Term::Error),
            Air::TupleClause {
                tipo,
                indices,
                subject_name,
                complex_clause,
                ..
            } => {
                let mut term = arg_stack.pop().unwrap();

                let tuple_types = tipo.get_inner_types();

                if complex_clause {
                    let next_clause = arg_stack.pop().unwrap();

                    term = term
                        .lambda("__other_clauses_delayed")
                        .apply(next_clause.delay());
                }

                if tuple_types.len() == 2 {
                    for (index, name) in indices.iter() {
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
        }
    }
}
