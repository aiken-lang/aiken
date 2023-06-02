use std::{rc::Rc, sync::Arc};

use indexmap::{IndexMap, IndexSet};
use itertools::Itertools;
use uplc::{
    ast::{
        Constant as UplcConstant, DeBruijn, Name, NamedDeBruijn, Program, Term, Type as UplcType,
    },
    builder::{CONSTR_FIELDS_EXPOSER, CONSTR_GET_FIELD, CONSTR_INDEX_EXPOSER, EXPECT_ON_LIST},
    builtins::DefaultFunction,
    machine::cost_model::ExBudget,
    optimize::aiken_optimize_and_intern,
    parser::interner::Interner,
};

use crate::{
    ast::{
        ArgName, AssignmentKind, BinOp, Pattern, Span, TypedArg, TypedClause, TypedDataType,
        TypedFunction, TypedValidator, UnOp,
    },
    builtins::{bool, data, void},
    expr::TypedExpr,
    gen_uplc::builder::{
        find_and_replace_generics, get_generic_id_and_type, get_variant_name,
        lookup_data_type_by_tipo,
    },
    tipo::{
        ModuleValueConstructor, PatternConstructor, Type, TypeInfo, ValueConstructor,
        ValueConstructorVariant,
    },
    IdGenerator,
};

pub mod air;
pub mod builder;
pub mod scope;
pub mod stack;

use air::Air;
use builder::{
    AssignmentProperties, ClauseProperties, DataTypeKey, FuncComponents, FunctionAccessKey,
};

use self::{builder::replace_opaque_type, scope::Scope, stack::AirStack};

#[derive(Clone, Debug)]
pub enum CodeGenFunction {
    Function(Vec<Air>, Vec<String>),
    Link(String),
}

#[derive(Clone)]
pub struct CodeGenerator<'a> {
    defined_functions: IndexMap<FunctionAccessKey, ()>,
    functions: IndexMap<FunctionAccessKey, &'a TypedFunction>,
    data_types: IndexMap<DataTypeKey, &'a TypedDataType>,
    module_types: IndexMap<&'a String, &'a TypeInfo>,
    id_gen: Rc<IdGenerator>,
    needs_field_access: bool,
    code_gen_functions: IndexMap<String, CodeGenFunction>,
    zero_arg_functions: IndexMap<FunctionAccessKey, Vec<Air>>,
    uplc_to_function: IndexMap<Program<DeBruijn>, FunctionAccessKey>,
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
            needs_field_access: false,
            id_gen: IdGenerator::new().into(),
            code_gen_functions: IndexMap::new(),
            zero_arg_functions: IndexMap::new(),
            uplc_to_function: IndexMap::new(),
        }
    }

    pub fn reset(&mut self) {
        self.code_gen_functions = IndexMap::new();
        self.zero_arg_functions = IndexMap::new();
        self.id_gen = IdGenerator::new().into();
        self.needs_field_access = false;
        self.defined_functions = IndexMap::new();
        self.uplc_to_function = IndexMap::new();
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
        let mut ir_stack = AirStack::new(self.id_gen.clone());

        ir_stack.noop();

        let mut args_stack = ir_stack.empty_with_scope();
        let mut body_stack = ir_stack.empty_with_scope();
        let mut unit_stack = ir_stack.empty_with_scope();
        let mut error_stack = ir_stack.empty_with_scope();

        self.wrap_validator_args(&mut args_stack, &fun.arguments, true);

        self.build(&fun.body, &mut body_stack);

        unit_stack.void();
        error_stack.error(void());

        ir_stack.merge_child(args_stack);
        ir_stack.if_branch(bool(), body_stack, unit_stack);
        ir_stack.merge_child(error_stack);

        let mut ir_stack = ir_stack.complete();

        self.define_ir(&mut ir_stack);

        self.convert_opaque_type_to_inner_ir(&mut ir_stack);

        let mut term = self.uplc_code_gen(&mut ir_stack);

        if let Some(other) = other_fun {
            self.reset();

            let mut other_ir_stack = AirStack::new(self.id_gen.clone());

            other_ir_stack.noop();

            let mut args_stack = other_ir_stack.empty_with_scope();
            let mut body_stack = other_ir_stack.empty_with_scope();
            let mut unit_stack = other_ir_stack.empty_with_scope();
            let mut error_stack = other_ir_stack.empty_with_scope();

            self.wrap_validator_args(&mut args_stack, &other.arguments, true);

            self.build(&other.body, &mut body_stack);

            unit_stack.void();
            error_stack.error(void());

            other_ir_stack.merge_child(args_stack);
            other_ir_stack.if_branch(bool(), body_stack, unit_stack);
            other_ir_stack.merge_child(error_stack);

            let mut other_ir_stack = other_ir_stack.complete();

            self.define_ir(&mut other_ir_stack);

            self.convert_opaque_type_to_inner_ir(&mut other_ir_stack);

            let other_term = self.uplc_code_gen(&mut other_ir_stack);

            let (spend, mint) = if other.arguments.len() > fun.arguments.len() {
                (other_term, term)
            } else {
                (term, other_term)
            };

            term = builder::wrap_as_multi_validator(spend, mint);

            self.needs_field_access = true;
        }

        term = builder::wrap_validator_args(term, params);

        self.finalize(term)
    }

    pub fn generate_test(&mut self, test_body: &TypedExpr) -> Program<Name> {
        let mut ir_stack = AirStack::new(self.id_gen.clone());

        ir_stack.noop();

        self.build(test_body, &mut ir_stack);

        let mut ir_stack = ir_stack.complete();

        self.define_ir(&mut ir_stack);

        self.convert_opaque_type_to_inner_ir(&mut ir_stack);

        let term = self.uplc_code_gen(&mut ir_stack);

        self.finalize(term)
    }

    fn finalize(&mut self, term: Term<Name>) -> Program<Name> {
        let mut term = term;

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

    pub(crate) fn build(&mut self, body: &TypedExpr, ir_stack: &mut AirStack) {
        match body {
            TypedExpr::Int { value, .. } => ir_stack.integer(value.to_string()),
            TypedExpr::String { value, .. } => ir_stack.string(value.to_string()),
            TypedExpr::ByteArray { bytes, .. } => ir_stack.byte_array(bytes.to_vec()),
            TypedExpr::Pipeline { expressions, .. } | TypedExpr::Sequence { expressions, .. } => {
                let mut stacks = Vec::new();

                for expr in expressions.iter() {
                    let mut stack = ir_stack.empty_with_scope();
                    self.build(expr, &mut stack);
                    stacks.push(stack);
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
                let mut body_stack = ir_stack.empty_with_scope();

                self.build(body, &mut body_stack);

                let params = args
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
                let mut stacks = Vec::new();
                for element in elements {
                    let mut stack = ir_stack.empty_with_scope();

                    self.build(element, &mut stack);

                    stacks.push(stack);
                }

                let tail = tail.as_ref().map(|tail| {
                    let mut tail_stack = ir_stack.empty_with_scope();

                    self.build(tail, &mut tail_stack);

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
                                builder::lookup_data_type_by_tipo(&self.data_types, tipo)
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

                                let mut stacks = Vec::new();

                                for (arg, func_type) in args.iter().zip(fun_arg_types) {
                                    let mut stack = ir_stack.empty_with_scope();

                                    if func_type.is_data() && !arg.value.tipo().is_data() {
                                        stack.wrap_data(arg.value.tipo());
                                    }

                                    self.build(&arg.value, &mut stack);

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

                            let mut stacks = Vec::new();

                            for (arg, func_type) in args.iter().zip(fun_arg_types) {
                                let mut stack = ir_stack.empty_with_scope();

                                if func_type.is_data() && !arg.value.tipo().is_data() {
                                    stack.wrap_data(arg.value.tipo());
                                }

                                self.build(&arg.value, &mut stack);

                                stacks.push(stack);
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
                                builder::lookup_data_type_by_tipo(&self.data_types, tipo)
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
                                    let mut stack = ir_stack.empty_with_scope();

                                    if func_type.is_data() && !arg.value.tipo().is_data() {
                                        stack.wrap_data(arg.value.tipo());
                                    }

                                    self.build(&arg.value, &mut stack);

                                    stacks.push(stack);
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
                                    let mut stack = ir_stack.empty_with_scope();

                                    if func_type.is_data() && !arg.value.tipo().is_data() {
                                        stack.wrap_data(arg.value.tipo());
                                    }

                                    self.build(&arg.value, &mut stack);

                                    stacks.push(stack);
                                }

                                ir_stack.builtin(*func, tipo.clone(), stacks);

                                return;
                            }
                        }
                        _ => {}
                    },
                    _ => {}
                }

                let mut fun_stack = ir_stack.empty_with_scope();

                self.build(fun, &mut fun_stack);

                let fun_arg_types = fun.tipo().arg_types().unwrap_or_default();

                let mut stacks = Vec::new();
                for (arg, func_type) in args.iter().zip(fun_arg_types) {
                    let mut stack = ir_stack.empty_with_scope();

                    if func_type.is_data() && !arg.value.tipo().is_data() {
                        stack.wrap_data(arg.value.tipo());
                    }

                    self.build(&arg.value, &mut stack);

                    stacks.push(stack);
                }

                ir_stack.call(tipo.clone(), fun_stack, stacks);
            }
            TypedExpr::BinOp {
                name, left, right, ..
            } => {
                let mut left_stack = ir_stack.empty_with_scope();
                let mut right_stack = ir_stack.empty_with_scope();

                self.build(left, &mut left_stack);
                self.build(right, &mut right_stack);

                ir_stack.binop(*name, left.tipo(), left_stack, right_stack);
            }
            TypedExpr::Assignment {
                value,
                pattern,
                kind,
                tipo,
                ..
            } => {
                let mut value_stack = ir_stack.empty_with_scope();
                let mut pattern_stack = ir_stack.empty_with_scope();

                let mut replaced_type = tipo.clone();
                builder::replace_opaque_type(&mut replaced_type, &self.data_types);

                self.build(value, &mut value_stack);

                self.assignment(
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
                    let mut value_stack = ir_stack.empty_with_scope();
                    let mut pattern_stack = ir_stack.empty_with_scope();
                    let mut subject_stack = ir_stack.empty_with_scope();

                    self.build(&clauses[0].then, &mut value_stack);

                    self.build(subject, &mut subject_stack);

                    self.assignment(
                        &clauses[0].pattern,
                        &mut pattern_stack,
                        subject_stack,
                        &subject_tipo,
                        AssignmentProperties {
                            value_type: clauses[0].then.tipo(),
                            kind: AssignmentKind::Let,
                        },
                    );

                    pattern_stack.merge_child(value_stack);
                    ir_stack.merge(pattern_stack);
                } else {
                    // TODO: go over rearrange clauses
                    let clauses = if subject_tipo.is_list() {
                        builder::rearrange_clauses(clauses.clone())
                    } else {
                        clauses.clone()
                    };

                    if let Some((last_clause, clauses)) = clauses.split_last() {
                        let mut pattern_stack = ir_stack.empty_with_scope();

                        let mut clause_properties = ClauseProperties::init(
                            &subject_tipo,
                            constr_var.clone(),
                            subject_name.clone(),
                        );

                        self.handle_each_clause(
                            &mut pattern_stack,
                            &mut clause_properties,
                            clauses,
                            &subject_tipo,
                        );

                        let last_pattern = &last_clause.pattern;

                        let mut final_pattern_stack = ir_stack.empty_with_scope();
                        let mut final_clause_stack = ir_stack.empty_with_scope();
                        let mut finally_stack = ir_stack.empty_with_scope();

                        self.build(&last_clause.then, &mut final_clause_stack);

                        *clause_properties.is_final_clause() = true;

                        self.when_pattern(
                            last_pattern,
                            &mut final_pattern_stack,
                            final_clause_stack,
                            &subject_tipo,
                            &mut clause_properties,
                        );

                        if !matches!(last_pattern, Pattern::Tuple { .. }) {
                            finally_stack.finally(final_pattern_stack);
                        } else {
                            finally_stack.merge(final_pattern_stack);
                        }

                        if *clause_properties.needs_constr_var() {
                            let mut subject_stack = ir_stack.empty_with_scope();

                            self.build(subject, &mut subject_stack);

                            let mut let_stack = ir_stack.empty_with_scope();

                            let_stack.let_assignment(constr_var.clone(), subject_stack);

                            ir_stack.merge(let_stack);

                            let mut var_stack = ir_stack.empty_with_scope();
                            let mut when_stack = ir_stack.empty_with_scope();

                            var_stack.local_var(subject_tipo.clone(), constr_var);

                            when_stack.when(
                                subject_tipo,
                                subject_name,
                                var_stack,
                                pattern_stack,
                                finally_stack,
                            );
                            ir_stack.merge(when_stack);
                        } else {
                            let mut subject_stack = ir_stack.empty_with_scope();
                            let mut when_stack = ir_stack.empty_with_scope();

                            self.build(subject, &mut subject_stack);

                            when_stack.when(
                                subject_tipo,
                                subject_name,
                                subject_stack,
                                pattern_stack,
                                finally_stack,
                            );
                            ir_stack.merge(when_stack);
                        }
                    }
                }
            }
            TypedExpr::If {
                branches,
                final_else,
                tipo,
                ..
            } => {
                for branch in branches.iter() {
                    let mut condition_stack = ir_stack.empty_with_scope();
                    let mut branch_body_stack = ir_stack.empty_with_scope();

                    self.build(&branch.condition, &mut condition_stack);

                    self.build(&branch.body, &mut branch_body_stack);

                    ir_stack.if_branch(tipo.clone(), condition_stack, branch_body_stack);
                }

                let mut else_stack = ir_stack.empty_with_scope();

                self.build(final_else, &mut else_stack);

                ir_stack.merge_child(else_stack);
            }
            TypedExpr::RecordAccess {
                record,
                index,
                tipo,
                ..
            } => {
                let mut record_access_stack = ir_stack.empty_with_scope();

                self.build(record, &mut record_access_stack);

                ir_stack.record_access(tipo.clone(), *index, record_access_stack);
            }
            TypedExpr::ModuleSelect {
                constructor,
                module_name,
                tipo,
                ..
            } => match constructor {
                ModuleValueConstructor::Record {
                    name,
                    arity,
                    tipo,
                    field_map,
                    ..
                } => {
                    assert!(arity == &0, "Wait how did you get here?");
                    let data_type = lookup_data_type_by_tipo(&self.data_types, tipo);

                    let val_constructor = ValueConstructor::public(
                        tipo.clone(),
                        ValueConstructorVariant::Record {
                            name: name.clone(),
                            arity: *arity,
                            field_map: field_map.clone(),
                            location: Span::empty(),
                            module: module_name.clone(),
                            constructors_count: data_type.unwrap().constructors.len() as u16,
                        },
                    );

                    ir_stack.var(val_constructor, name, "");
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
                        ir_stack.var(
                            ValueConstructor::public(tipo.clone(), value.variant.clone()),
                            format!("{module}_{name}"),
                            "",
                        );
                    } else {
                        let ValueConstructorVariant::ModuleFn {
                            builtin: Some(builtin), ..
                        } = &value.variant else {
                            unreachable!()
                        };

                        ir_stack.builtin(*builtin, tipo.clone(), vec![]);
                    }
                }
                ModuleValueConstructor::Constant { literal, .. } => {
                    builder::constants_ir(literal, ir_stack);
                }
            },
            TypedExpr::RecordUpdate {
                spread, args, tipo, ..
            } => {
                let mut index_types = vec![];
                let mut highest_index = 0;

                let mut update_stack = ir_stack.empty_with_scope();

                self.build(spread, &mut update_stack);

                for arg in args
                    .iter()
                    .sorted_by(|arg1, arg2| arg1.index.cmp(&arg2.index))
                {
                    let mut arg_stack = update_stack.empty_with_scope();

                    self.build(&arg.value, &mut arg_stack);

                    update_stack.merge(arg_stack);

                    if arg.index > highest_index {
                        highest_index = arg.index;
                    }

                    index_types.push((arg.index, arg.value.tipo()));
                }

                ir_stack.record_update(tipo.clone(), highest_index, index_types, update_stack);
            }
            TypedExpr::UnOp { value, op, .. } => {
                let mut value_stack = ir_stack.empty_with_scope();

                self.build(value, &mut value_stack);

                ir_stack.unop(*op, value_stack);
            }
            TypedExpr::Tuple { elems, tipo, .. } => {
                let mut stacks = vec![];

                for elem in elems {
                    let mut elem_stack = ir_stack.empty_with_scope();
                    self.build(elem, &mut elem_stack);
                    stacks.push(elem_stack);
                }

                ir_stack.tuple(tipo.clone(), stacks);
            }

            TypedExpr::Trace {
                tipo, then, text, ..
            } => {
                let mut text_stack = ir_stack.empty_with_scope();
                let mut then_stack = ir_stack.empty_with_scope();

                self.build(text, &mut text_stack);
                self.build(then, &mut then_stack);

                ir_stack.trace(tipo.clone());
                ir_stack.merge_child(text_stack);
                ir_stack.merge_child(then_stack);
            }

            TypedExpr::TupleIndex { index, tuple, .. } => {
                let mut tuple_stack = ir_stack.empty_with_scope();

                self.build(tuple, &mut tuple_stack);

                ir_stack.tuple_index(tuple.tipo(), *index, tuple_stack);
            }

            TypedExpr::ErrorTerm { tipo, .. } => {
                ir_stack.error(tipo.clone());
            }
        }
    }

    fn handle_each_clause(
        &mut self,
        ir_stack: &mut AirStack,
        clause_properties: &mut ClauseProperties,
        clauses: &[TypedClause],
        subject_type: &Arc<Type>,
    ) {
        for (index, clause) in clauses.iter().enumerate() {
            // holds when clause pattern Air
            let mut clause_pattern_stack = ir_stack.empty_with_scope();
            let mut clause_then_stack = ir_stack.empty_with_scope();

            // reset complex clause setting per clause back to default
            *clause_properties.is_complex_clause() = false;

            self.build(&clause.then, &mut clause_then_stack);

            if let Some(clause_guard) = &clause.guard {
                let mut clause_guard_stack = ir_stack.empty_with_scope();
                let mut clause_guard_condition = ir_stack.empty_with_scope();

                *clause_properties.is_complex_clause() = true;

                let clause_guard_name = format!("__clause_guard_{}", self.id_gen.next());

                builder::handle_clause_guard(clause_guard, &mut clause_guard_condition);

                clause_guard_stack
                    .let_assignment(clause_guard_name.clone(), clause_guard_condition);

                let mut condition_stack = ir_stack.empty_with_scope();

                condition_stack.bool(true);

                clause_guard_stack.clause_guard(
                    clause_guard_name,
                    bool(),
                    condition_stack,
                    clause_then_stack,
                );

                clause_then_stack = clause_guard_stack;
            }

            // deal with clause pattern and then itself
            self.when_pattern(
                &clause.pattern,
                &mut clause_pattern_stack,
                clause_then_stack,
                subject_type,
                clause_properties,
            );

            match clause_properties {
                ClauseProperties::ConstrClause {
                    original_subject_name,
                    ..
                } => {
                    let subject_name = original_subject_name.clone();

                    if clause.pattern.is_var() || clause.pattern.is_discard() {
                        ir_stack.wrap_clause(clause_pattern_stack);
                    } else {
                        let data_type =
                            builder::lookup_data_type_by_tipo(&self.data_types, subject_type);

                        if let Some(data_type) = data_type {
                            if data_type.constructors.len() > 1 {
                                ir_stack.clause(
                                    subject_type.clone(),
                                    subject_name,
                                    *clause_properties.is_complex_clause(),
                                    clause_pattern_stack,
                                );
                            } else {
                                let mut condition_stack = ir_stack.empty_with_scope();

                                condition_stack.integer(0.to_string());

                                condition_stack.merge_child(clause_pattern_stack);

                                ir_stack.clause(
                                    subject_type.clone(),
                                    subject_name,
                                    *clause_properties.is_complex_clause(),
                                    condition_stack,
                                );
                            }
                        } else {
                            ir_stack.clause(
                                subject_type.clone(),
                                subject_name,
                                *clause_properties.is_complex_clause(),
                                clause_pattern_stack,
                            );
                        }
                    }
                }
                ClauseProperties::ListClause {
                    original_subject_name,
                    current_index,
                    ..
                } => {
                    let original_subject_name = original_subject_name.clone();

                    let prev_index = *current_index;

                    let elements_count_and_has_tail =
                        builder::get_list_elements_len_and_tail(&clause.pattern);

                    if let Some((current_clause_index, has_tail)) = elements_count_and_has_tail {
                        let subject_name = if current_clause_index == 0 {
                            original_subject_name.clone()
                        } else {
                            format!("__tail_{}", current_clause_index - 1)
                        };

                        // If current clause has already exposed all needed list items then no need to expose the
                        // same items again.
                        if current_clause_index as i64 - i64::from(has_tail) == prev_index {
                            ir_stack.wrap_clause(clause_pattern_stack);
                        } else {
                            let next_elements_count_and_has_tail = if index == clauses.len() - 1 {
                                None
                            } else {
                                builder::get_list_elements_len_and_tail(
                                    &clauses
                                        .get(index + 1)
                                        .unwrap_or_else(|| {
                                            unreachable!(
                                                "We checked length how are we out of bounds"
                                            )
                                        })
                                        .pattern,
                                )
                            };

                            let next_tail = if let Some((next_elements_len, _)) =
                                next_elements_count_and_has_tail
                            {
                                if next_elements_len == current_clause_index {
                                    None
                                } else {
                                    Some(format!("__tail_{current_clause_index}"))
                                }
                            } else {
                                None
                            };

                            //mutate current index if we use list clause
                            *current_index = current_clause_index as i64;

                            ir_stack.list_clause(
                                subject_type.clone(),
                                subject_name,
                                next_tail,
                                *clause_properties.is_complex_clause(),
                                clause_pattern_stack,
                            );
                        }
                    } else {
                        ir_stack.wrap_clause(clause_pattern_stack);
                    }
                }
                ClauseProperties::TupleClause {
                    original_subject_name,
                    defined_tuple_indices,
                    ..
                } => {
                    let prev_defined_tuple_indices = defined_tuple_indices.clone();
                    let subject_name = original_subject_name.clone();

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

                    ir_stack.tuple_clause(
                        subject_type.clone(),
                        subject_name,
                        indices_to_define,
                        prev_defined_tuple_indices,
                        *clause_properties.is_complex_clause(),
                        clause_pattern_stack,
                    );
                }
            }
        }
    }

    fn when_pattern(
        &mut self,
        pattern: &Pattern<PatternConstructor, Arc<Type>>,
        pattern_stack: &mut AirStack,
        value_stack: AirStack,
        tipo: &Type,
        clause_properties: &mut ClauseProperties,
    ) {
        match pattern {
            Pattern::Int { value, .. } => {
                pattern_stack.integer(value.clone());

                pattern_stack.merge_child(value_stack);
            }
            Pattern::Var { name, .. } => {
                pattern_stack.void();

                let mut var_stack = pattern_stack.empty_with_scope();

                var_stack.local_var(
                    tipo.clone().into(),
                    clause_properties.original_subject_name(),
                );

                pattern_stack.let_assignment(name, var_stack);

                pattern_stack.merge_child(value_stack);
            }
            Pattern::Assign { name, pattern, .. } => {
                let mut new_stack = pattern_stack.empty_with_scope();

                new_stack.local_var(
                    tipo.clone().into(),
                    clause_properties.original_subject_name(),
                );

                let mut let_stack = pattern_stack.empty_with_scope();

                let_stack.let_assignment(name.clone(), new_stack);

                let_stack.merge_child(value_stack);

                self.when_pattern(pattern, pattern_stack, let_stack, tipo, clause_properties);
            }
            Pattern::Discard { .. } => {
                pattern_stack.void();
                pattern_stack.merge_child(value_stack);
            }
            Pattern::List { elements, tail, .. } => {
                for element in elements {
                    builder::check_when_pattern_needs(element, clause_properties);
                }

                if let Some(tail) = tail {
                    builder::check_when_pattern_needs(tail, clause_properties);
                }

                *clause_properties.needs_constr_var() = false;

                let mut void_stack = pattern_stack.empty_with_scope();

                void_stack.void();

                pattern_stack.merge(void_stack);

                self.expose_elements(pattern, pattern_stack, value_stack, clause_properties, tipo);
            }
            Pattern::Constructor {
                arguments,
                name: constr_name,
                ..
            } => {
                let mut temp_clause_properties = clause_properties.clone();
                *temp_clause_properties.needs_constr_var() = false;

                if tipo.is_bool() {
                    pattern_stack.bool(constr_name == "True");
                } else {
                    for arg in arguments {
                        builder::check_when_pattern_needs(&arg.value, &mut temp_clause_properties);
                    }

                    // find data type definition
                    let data_type =
                        builder::lookup_data_type_by_tipo(&self.data_types, tipo).unwrap();

                    let (index, _) = data_type
                        .constructors
                        .iter()
                        .enumerate()
                        .find(|(_, dt)| &dt.name == constr_name)
                        .unwrap();

                    let mut new_stack = pattern_stack.empty_with_scope();

                    new_stack.local_var(
                        tipo.clone().into(),
                        temp_clause_properties.clause_var_name(),
                    );

                    // if only one constructor, no need to check
                    if data_type.constructors.len() > 1 {
                        // push constructor Index
                        let mut tag_stack = pattern_stack.empty_with_scope();
                        tag_stack.integer(index.to_string());
                        pattern_stack.merge_child(tag_stack);
                    }

                    if *temp_clause_properties.needs_constr_var() {
                        self.expose_elements(
                            pattern,
                            pattern_stack,
                            new_stack,
                            clause_properties,
                            tipo,
                        );
                    } else {
                        let empty_stack = pattern_stack.empty_with_scope();

                        self.expose_elements(
                            pattern,
                            pattern_stack,
                            empty_stack,
                            clause_properties,
                            tipo,
                        );
                    }
                }

                pattern_stack.merge_child(value_stack);

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

                let temp = pattern_stack.empty_with_scope();

                self.expose_elements(pattern, pattern_stack, temp, clause_properties, tipo);

                pattern_stack.merge_child(value_stack);
            }
        }
    }

    fn expose_elements(
        &mut self,
        pattern: &Pattern<PatternConstructor, Arc<Type>>,
        pattern_stack: &mut AirStack,
        value_stack: AirStack,
        clause_properties: &mut ClauseProperties,
        tipo: &Type,
    ) {
        match pattern {
            Pattern::Int { .. } => unreachable!(),
            Pattern::Var { .. } => unreachable!(),
            Pattern::Assign { .. } => todo!("Nested assign not yet implemented"),
            Pattern::Discard { .. } => {
                pattern_stack.void();

                pattern_stack.merge_child(value_stack);
            }
            Pattern::List { elements, tail, .. } => {
                let mut names = vec![];
                let mut nested_pattern = pattern_stack.empty_with_scope();
                let items_type = &tipo.get_inner_types()[0];

                for element in elements {
                    let name = self.nested_pattern_ir_and_label(
                        element,
                        &mut nested_pattern,
                        items_type,
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

                if tail.is_some() && !tail_head_names.is_empty() {
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

                    pattern_stack.list_expose(
                        tipo.clone().into(),
                        tail_head_names,
                        tail,
                        nested_pattern,
                    );
                } else if !tail_head_names.is_empty() {
                    pattern_stack.list_expose(
                        tipo.clone().into(),
                        tail_head_names,
                        None,
                        nested_pattern,
                    );
                }

                pattern_stack.merge_child(value_stack);
            }
            Pattern::Constructor {
                is_record,
                name: constr_name,
                arguments,
                constructor,
                tipo,
                ..
            } => {
                let data_type = builder::lookup_data_type_by_tipo(&self.data_types, tipo).unwrap();

                let constructor_type = data_type
                    .constructors
                    .iter()
                    .find(|dt| &dt.name == constr_name)
                    .unwrap();
                let mut nested_pattern = pattern_stack.empty_with_scope();

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
                        .enumerate()
                        .map(|(index, item)| {
                            let label = item.label.clone().unwrap_or_default();

                            let field_index = field_map
                                .fields
                                .get(&label)
                                .map(|(index, _)| index)
                                .unwrap_or(&index);

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
                                *clause_properties.is_final_clause(),
                            );

                            var_name.map_or(
                                (label.clone(), "_".to_string(), *field_index),
                                |var_name| (label, var_name, *field_index),
                            )
                        })
                        .sorted_by(|item1, item2| item1.2.cmp(&item2.2))
                        .collect::<Vec<(String, String, usize)>>();

                    let indices = arguments_index
                        .iter()
                        .map(|(label, var_name, index)| {
                            let field_type = type_map
                                .get(label)
                                .unwrap_or_else(|| type_map.get_index(*index).unwrap().1);
                            (*index, var_name.clone(), field_type.clone())
                        })
                        .collect_vec();

                    if indices.is_empty() {
                        pattern_stack.merge_child(value_stack);
                    } else {
                        pattern_stack.fields_expose(indices, false, value_stack);
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
                        .map(|(index, item)| {
                            let var_name = self.nested_pattern_ir_and_label(
                                &item.value,
                                &mut nested_pattern,
                                type_map.get(&index).unwrap(),
                                *clause_properties.is_final_clause(),
                            );

                            var_name.map_or(("_".to_string(), index), |var_name| (var_name, index))
                        })
                        .collect::<Vec<(String, usize)>>();

                    let indices = arguments_index
                        .iter()
                        .map(|(name, index)| {
                            let field_type = type_map.get(index).unwrap();

                            (*index, name.clone(), field_type.clone())
                        })
                        .collect_vec();

                    if indices.is_empty() {
                        pattern_stack.merge_child(value_stack);
                    } else {
                        pattern_stack.fields_expose(indices, false, value_stack);
                    }
                }

                pattern_stack.merge_child(nested_pattern);
            }
            Pattern::Tuple { elems, .. } => {
                let mut names = vec![];
                let mut nested_pattern = pattern_stack.empty_with_scope();
                let items_type = &tipo.get_inner_types();

                for (index, element) in elems.iter().enumerate() {
                    let name = self.nested_pattern_ir_and_label(
                        element,
                        &mut nested_pattern,
                        &items_type[index],
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
                    let mut var_stack = pattern_stack.empty_with_scope();

                    let new_name = names
                        .iter()
                        .find(|(_, current_index)| *current_index == index)
                        .map(|(new_name, _)| new_name)
                        .unwrap();

                    let pattern_type = &tipo.get_inner_types()[index];

                    var_stack.local_var(pattern_type.clone(), name);

                    pattern_stack.let_assignment(new_name, var_stack);
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

                pattern_stack.merge_child(nested_pattern);
                pattern_stack.merge_child(value_stack);
            }
        }
    }

    fn nested_pattern_ir_and_label(
        &mut self,
        pattern: &Pattern<PatternConstructor, Arc<Type>>,
        pattern_stack: &mut AirStack,
        pattern_type: &Type,
        final_clause: bool,
    ) -> Option<String> {
        match pattern {
            Pattern::Var { name, .. } => Some(name.clone()),
            Pattern::Discard { .. } => None,
            pattern @ Pattern::List { elements, tail, .. } => {
                let item_name = format!("__list_item_id_{}", self.id_gen.next());
                let new_tail_name = "__tail".to_string();

                if elements.is_empty() {
                    let mut void_stack = pattern_stack.empty_with_scope();

                    void_stack.void();

                    pattern_stack.list_clause_guard(
                        pattern_type.clone().into(),
                        item_name.clone(),
                        None,
                        false,
                        void_stack,
                    );
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
                                let mut elements_stack = pattern_stack.empty_with_scope();

                                self.when_pattern(
                                    pattern,
                                    &mut elements_stack,
                                    pattern_stack.empty_with_scope(),
                                    pattern_type,
                                    &mut clause_properties,
                                );

                                pattern_stack.list_clause_guard(
                                    pattern_type.clone().into(),
                                    prev_tail_name,
                                    None,
                                    true,
                                    elements_stack,
                                );
                            } else {
                                let mut elements_stack = pattern_stack.empty_with_scope();

                                let mut void_stack = pattern_stack.empty_with_scope();

                                void_stack.void();

                                self.when_pattern(
                                    pattern,
                                    &mut elements_stack,
                                    pattern_stack.empty_with_scope(),
                                    pattern_type,
                                    &mut clause_properties,
                                );

                                void_stack.list_clause_guard(
                                    pattern_type.clone().into(),
                                    &tail_name,
                                    None,
                                    false,
                                    elements_stack,
                                );

                                pattern_stack.list_clause_guard(
                                    pattern_type.clone().into(),
                                    prev_tail_name,
                                    Some(tail_name),
                                    true,
                                    void_stack,
                                );
                            }
                        } else {
                            let mut void_stack = pattern_stack.empty_with_scope();

                            void_stack.void();

                            pattern_stack.list_clause_guard(
                                pattern_type.clone().into(),
                                prev_tail_name,
                                Some(tail_name),
                                true,
                                void_stack,
                            );
                        };
                    }
                }

                Some(item_name)
            }
            pattern @ Pattern::Constructor {
                tipo,
                name: constr_name,
                ..
            } => {
                let id = self.id_gen.next();
                let constr_var_name = format!("{constr_name}_{id}");
                let data_type = builder::lookup_data_type_by_tipo(&self.data_types, tipo).unwrap();

                let mut when_stack = pattern_stack.empty_with_scope();

                let mut clause_properties = ClauseProperties::ConstrClause {
                    clause_var_name: constr_var_name.clone(),
                    needs_constr_var: false,
                    is_complex_clause: false,
                    original_subject_name: constr_var_name.clone(),
                    final_clause,
                };

                self.when_pattern(
                    pattern,
                    &mut when_stack,
                    pattern_stack.empty_with_scope(),
                    tipo,
                    &mut clause_properties,
                );

                if data_type.constructors.len() > 1 {
                    if final_clause {
                        pattern_stack.finally(when_stack);
                    } else {
                        let empty_stack = pattern_stack.empty_with_scope();
                        pattern_stack.clause_guard(
                            constr_var_name.clone(),
                            tipo.clone(),
                            when_stack,
                            empty_stack,
                        );
                    }
                } else {
                    pattern_stack.merge_child(when_stack);
                }

                Some(constr_var_name)
            }
            a @ Pattern::Tuple { .. } => {
                let item_name = format!("__tuple_item_id_{}", self.id_gen.next());

                let mut clause_properties = ClauseProperties::TupleClause {
                    clause_var_name: item_name.clone(),
                    needs_constr_var: false,
                    is_complex_clause: false,
                    original_subject_name: item_name.clone(),
                    defined_tuple_indices: IndexSet::new(),
                    final_clause,
                };

                let mut inner_pattern_stack = pattern_stack.empty_with_scope();

                self.when_pattern(
                    a,
                    &mut inner_pattern_stack,
                    pattern_stack.empty_with_scope(),
                    pattern_type,
                    &mut clause_properties,
                );

                let defined_indices = match clause_properties.clone() {
                    ClauseProperties::TupleClause {
                        defined_tuple_indices,
                        ..
                    } => defined_tuple_indices,
                    _ => unreachable!(),
                };

                pattern_stack.tuple_clause(
                    pattern_type.clone().into(),
                    clause_properties.original_subject_name(),
                    defined_indices,
                    IndexSet::new(),
                    false,
                    inner_pattern_stack,
                );

                Some(item_name)
            }
            Pattern::Assign { name, pattern, .. } => {
                let inner_name = self.nested_pattern_ir_and_label(
                    pattern,
                    pattern_stack,
                    pattern_type,
                    final_clause,
                );

                let mut var_stack = pattern_stack.empty_with_scope();
                var_stack.local_var(pattern_type.clone().into(), inner_name.clone().unwrap());

                pattern_stack.let_assignment(name, var_stack);

                inner_name
            }
            Pattern::Int { .. } => {
                let error_message = "Nested pattern-match on integers isn't implemented yet. Use when clause-guard as an alternative, or break down the pattern.";
                todo!("{}", error_message)
            }
        }
    }

    fn assignment(
        &mut self,
        pattern: &Pattern<PatternConstructor, Arc<Type>>,
        pattern_stack: &mut AirStack,
        value_stack: AirStack,
        tipo: &Type,
        assignment_properties: AssignmentProperties,
    ) {
        let mut value_stack = if assignment_properties.value_type.is_data()
            && !tipo.is_data()
            && !pattern.is_discard()
        {
            let mut wrap_stack = pattern_stack.empty_with_scope();
            wrap_stack.un_wrap_data(tipo.clone().into());
            wrap_stack.merge_child(value_stack);
            wrap_stack
        } else if !assignment_properties.value_type.is_data()
            && tipo.is_data()
            && !pattern.is_discard()
        {
            let mut wrap_stack = pattern_stack.empty_with_scope();
            wrap_stack.wrap_data(assignment_properties.value_type.clone());
            wrap_stack.merge_child(value_stack);
            wrap_stack
        } else {
            value_stack
        };

        match pattern {
            Pattern::Int { .. } => todo!("Pattern match with integer assignment not supported"),
            Pattern::Var { name, .. } => {
                let expect_value_stack = value_stack.empty_with_scope();
                pattern_stack.let_assignment(name, value_stack);

                if matches!(assignment_properties.kind, AssignmentKind::Expect)
                    && assignment_properties.value_type.is_data()
                    && !tipo.is_data()
                {
                    let mut expect_stack = pattern_stack.empty_with_scope();

                    self.expect_pattern(
                        pattern,
                        &mut expect_stack,
                        expect_value_stack,
                        tipo,
                        assignment_properties,
                    );

                    pattern_stack.merge(expect_stack);
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
                    self.expect_pattern(
                        list,
                        pattern_stack,
                        value_stack,
                        tipo,
                        assignment_properties,
                    );
                } else {
                    self.pattern_ir(
                        list,
                        pattern_stack,
                        value_stack,
                        tipo,
                        assignment_properties,
                    );
                }
            }
            constr @ Pattern::Constructor { .. } => {
                if matches!(assignment_properties.kind, AssignmentKind::Expect)
                    && assignment_properties.value_type.is_data()
                    && !tipo.is_data()
                {
                    self.expect_pattern(
                        constr,
                        pattern_stack,
                        value_stack,
                        tipo,
                        assignment_properties,
                    );
                } else {
                    self.pattern_ir(
                        constr,
                        pattern_stack,
                        value_stack,
                        tipo,
                        assignment_properties,
                    );
                }
            }
            tuple @ Pattern::Tuple { .. } => {
                if matches!(assignment_properties.kind, AssignmentKind::Expect)
                    && assignment_properties.value_type.is_data()
                    && !tipo.is_data()
                {
                    self.expect_pattern(
                        tuple,
                        pattern_stack,
                        value_stack,
                        tipo,
                        assignment_properties,
                    );
                } else {
                    self.pattern_ir(
                        tuple,
                        pattern_stack,
                        value_stack,
                        tipo,
                        assignment_properties,
                    );
                }
            }
        }
    }

    fn pattern_ir(
        &mut self,
        pattern: &Pattern<PatternConstructor, Arc<Type>>,
        pattern_stack: &mut AirStack,
        value_stack: AirStack,
        tipo: &Type,
        assignment_properties: AssignmentProperties,
    ) {
        match pattern {
            Pattern::Int { .. } => unreachable!("Pattern Integer"),
            Pattern::Var { .. } => unreachable!("Pattern Var"),
            Pattern::Assign { .. } => unreachable!("Pattern Assign"),
            Pattern::Discard { .. } => unreachable!("Pattern Discard"),
            Pattern::List { elements, tail, .. } => {
                let inner_list_type = &tipo.get_inner_types()[0];
                let mut elements_stack = pattern_stack.empty_with_scope();
                let mut names = vec![];

                for element in elements {
                    match element {
                        Pattern::Var { name, .. } => {
                            names.push(name.clone());
                        }
                        pattern @ (Pattern::List { .. }
                        | Pattern::Constructor { .. }
                        | Pattern::Tuple { .. }) => {
                            let mut var_stack = pattern_stack.empty_with_scope();

                            let item_name = format!("list_item_id_{}", self.id_gen.next());

                            names.push(item_name.clone());

                            let mut element_stack = pattern_stack.empty_with_scope();

                            var_stack.local_var(inner_list_type.clone(), item_name);

                            self.pattern_ir(
                                pattern,
                                &mut element_stack,
                                var_stack,
                                &tipo.get_inner_types()[0],
                                assignment_properties.clone(),
                            );

                            elements_stack.merge(element_stack);
                        }
                        Pattern::Int { .. } => unreachable!("Inner List: Pattern Integer"),
                        Pattern::Assign { .. } => todo!("Assign in lists not supported yet"),
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
                    pattern_stack.list_accessor(
                        tipo.clone().into(),
                        names,
                        tail.is_some(),
                        true,
                        value_stack,
                    );
                } else {
                    pattern_stack.list_empty(value_stack);
                }

                pattern_stack.merge_child(elements_stack);
            }
            Pattern::Constructor {
                arguments,
                constructor,
                tipo: constr_tipo,
                name: constructor_name,
                ..
            } => {
                let mut stacks = pattern_stack.empty_with_scope();

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
                    .map(|(index, item)| {
                        let label = item.label.clone().unwrap_or_default();

                        let field_index = if let Some(field_map) = &field_map {
                            *field_map.fields.get(&label).map(|x| &x.0).unwrap_or(&index)
                        } else {
                            index
                        };

                        let mut nested_pattern = pattern_stack.empty_with_scope();

                        let name = self.extract_arg_name(
                            &item.value,
                            &mut nested_pattern,
                            type_map.get(&field_index).unwrap(),
                            &assignment_properties,
                        );

                        // Note the stacks mutation here
                        stacks.merge(nested_pattern);

                        name.map_or(("_".to_string(), field_index), |name| (name, field_index))
                    })
                    .sorted_by(|item1, item2| item1.1.cmp(&item2.1))
                    .collect::<Vec<(String, usize)>>();

                let constr_name = format!("__{}_{}", constructor_name, self.id_gen.next());

                let mut expect_stack = pattern_stack.empty_with_scope();

                match assignment_properties.kind {
                    AssignmentKind::Let => {
                        expect_stack.merge_child(value_stack);
                    }
                    AssignmentKind::Expect => {
                        if tipo.is_bool() {
                            expect_stack.expect_bool(constructor_name == "True", value_stack);
                        } else if tipo.is_void() {
                            expect_stack.choose_unit(value_stack);
                        } else if tipo.is_data() {
                            unimplemented!("What are you doing with Data type?")
                        } else {
                            let data_type =
                                builder::lookup_data_type_by_tipo(&self.data_types, tipo).unwrap();

                            let (index, _) = data_type
                                .constructors
                                .iter()
                                .enumerate()
                                .find(|(_, constr)| constr.name == *constructor_name)
                                .unwrap();

                            let constr_name = format!("__{}_{}", constr_name, self.id_gen.next());
                            let mut var_stack = expect_stack.empty_with_scope();

                            var_stack.local_var(tipo.clone().into(), constr_name.clone());

                            expect_stack.let_assignment(constr_name.clone(), value_stack);

                            expect_stack.expect_constr(index, var_stack);

                            expect_stack.local_var(tipo.clone().into(), constr_name);
                        }
                    }
                }
                if !arguments_index.is_empty() {
                    let indices = arguments_index
                        .iter()
                        .map(|(var_name, index)| {
                            let field_type = type_map.get(index).unwrap();
                            (*index, var_name.clone(), field_type.clone())
                        })
                        .collect_vec();

                    pattern_stack.fields_expose(indices, false, expect_stack);
                } else if (tipo.is_bool() || tipo.is_void())
                    && assignment_properties.kind.is_expect()
                {
                    pattern_stack.merge_child(expect_stack);
                } else {
                    pattern_stack.let_assignment("_", expect_stack);
                }

                pattern_stack.merge_child(stacks);
            }
            Pattern::Tuple { elems, .. } => {
                let mut stacks = pattern_stack.empty_with_scope();
                let mut type_map: IndexMap<usize, Arc<Type>> = IndexMap::new();

                for (index, arg) in tipo.get_inner_types().iter().enumerate() {
                    let field_type = arg.clone();
                    type_map.insert(index, field_type);
                }

                let arguments_index = elems
                    .iter()
                    .enumerate()
                    .filter_map(|(tuple_index, item)| {
                        let mut nested_stack = pattern_stack.empty_with_scope();

                        let name = self
                            .extract_arg_name(
                                item,
                                &mut nested_stack,
                                type_map.get(&tuple_index).unwrap(),
                                &assignment_properties,
                            )
                            .map(|name| (name, tuple_index));

                        stacks.merge(nested_stack);

                        name
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

                    pattern_stack.tuple_accessor(
                        tipo.clone().into(),
                        final_args.into_iter().map(|(item, _)| item).collect_vec(),
                        false,
                        value_stack,
                    );
                } else {
                    pattern_stack.let_assignment("_", value_stack);
                }

                pattern_stack.merge_child(stacks);
            }
        }
    }

    pub fn expect_pattern(
        &mut self,
        pattern: &Pattern<PatternConstructor, Arc<Type>>,
        expect_stack: &mut AirStack,
        value_stack: AirStack,
        tipo: &Type,
        assignment_properties: AssignmentProperties,
    ) {
        match pattern {
            Pattern::Int { .. } => unreachable!("Expect Integer"),
            Pattern::Var { name, .. } => {
                expect_stack.merge(value_stack);

                self.expect_type(tipo, expect_stack, name, &mut IndexMap::new());
            }
            Pattern::Assign { .. } => todo!("Expect Assign not supported yet"),
            Pattern::Discard { .. } => unreachable!(),
            Pattern::List { elements, tail, .. } => {
                let inner_list_type = &tipo.get_inner_types()[0];
                let mut names = vec![];

                let mut expect_list_stacks = vec![];

                for element in elements {
                    match element {
                        Pattern::Var { name, .. } => {
                            names.push(name.clone());
                        }
                        Pattern::Assign { .. } => {
                            todo!("Inner List: Expect Assign not supported yet")
                        }
                        element_pattern @ (Pattern::List { .. }
                        | Pattern::Constructor { .. }
                        | Pattern::Tuple { .. }) => {
                            let name = format!("list_item_id_{}", self.id_gen.next());

                            names.push(name.clone());

                            let mut element_stack = expect_stack.empty_with_scope();
                            let mut value_stack = element_stack.empty_with_scope();

                            value_stack.local_var(inner_list_type.clone(), name);

                            self.expect_pattern(
                                element_pattern,
                                &mut element_stack,
                                value_stack,
                                inner_list_type,
                                assignment_properties.clone(),
                            );

                            expect_list_stacks.push(element_stack);
                        }
                        Pattern::Int { .. } => unreachable!("Inner List: Expect Integer"),
                        _ => {}
                    }
                }

                let mut tail_stack = expect_stack.empty_with_scope();

                let name = if let Some(tail) = tail {
                    match &**tail {
                        Pattern::Var { name, .. } => name.clone(),
                        _ => format!("__tail_{}", self.id_gen.next()),
                    }
                } else {
                    format!("__tail_{}", self.id_gen.next())
                };

                self.expect_type(
                    inner_list_type,
                    &mut tail_stack,
                    &name,
                    &mut IndexMap::new(),
                );

                expect_list_stacks.push(tail_stack);

                names.push(name);

                expect_stack.list_accessor(tipo.clone().into(), names, true, false, value_stack);

                expect_stack.merge_children(expect_list_stacks);
            }
            Pattern::Constructor {
                arguments,
                constructor,
                name: constr_name,
                tipo,
                ..
            } => {
                if tipo.is_bool() {
                    let PatternConstructor::Record { name, .. } = constructor;

                    expect_stack.expect_bool(name == "True", value_stack);
                } else if tipo.is_void() {
                    expect_stack.choose_unit(value_stack);
                } else {
                    let field_map = match constructor {
                        PatternConstructor::Record { field_map, .. } => field_map,
                    };

                    let data_type =
                        builder::lookup_data_type_by_tipo(&self.data_types, tipo).unwrap();

                    let (index, data_type_constr) = data_type
                        .constructors
                        .iter()
                        .enumerate()
                        .find(|(_, constr)| &constr.name == constr_name)
                        .unwrap();

                    let mut type_map: IndexMap<usize, Arc<Type>> = IndexMap::new();

                    let arg_types = tipo.arg_types().unwrap();

                    for (index, arg) in arg_types.iter().enumerate() {
                        let field_type = arg.clone();
                        type_map.insert(index, field_type);
                    }

                    let mut stacks = expect_stack.empty_with_scope();

                    let arguments_index = arguments
                        .iter()
                        .enumerate()
                        .filter_map(|(index, item)| {
                            let label = item.label.clone().unwrap_or_default();

                            let field_index = field_map
                                .as_ref()
                                .map(|field_map| {
                                    field_map.fields.get(&label).map(|x| &x.0).unwrap_or(&index)
                                })
                                .unwrap_or(&index);

                            let mut inner_stack = expect_stack.empty_with_scope();

                            let name = self.extract_arg_name(
                                &item.value,
                                &mut inner_stack,
                                type_map.get(field_index).unwrap(),
                                &assignment_properties,
                            );

                            stacks.merge(inner_stack);

                            name.map(|name| (name, *field_index))
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

                            self.expect_type(
                                type_map.get(&index).unwrap(),
                                &mut stacks,
                                &format!("__field_{index}_{id_next}"),
                                &mut IndexMap::new(),
                            )
                        }
                    }

                    let constr_var = format!("__constr_{}", self.id_gen.next());

                    expect_stack.let_assignment(constr_var.clone(), value_stack);

                    let mut var_stack = expect_stack.empty_with_scope();
                    var_stack.local_var(tipo.clone(), constr_var.clone());
                    expect_stack.expect_constr(index, var_stack);

                    if !final_args.is_empty() {
                        let mut fields_stack = expect_stack.empty_with_scope();
                        fields_stack.local_var(tipo.clone(), constr_var);

                        let indices = final_args
                            .iter()
                            .map(|(var_name, index)| {
                                let field_type = type_map.get(index).unwrap();
                                (*index, var_name.clone(), field_type.clone())
                            })
                            .collect_vec();

                        expect_stack.fields_expose(indices, true, fields_stack);
                    }

                    expect_stack.merge_child(stacks);
                }
            }
            Pattern::Tuple { elems, .. } => {
                let mut type_map: IndexMap<usize, Arc<Type>> = IndexMap::new();

                for (index, arg) in tipo.arg_types().unwrap().iter().enumerate() {
                    let field_type = arg.clone();
                    type_map.insert(index, field_type);
                }

                let mut stacks = expect_stack.empty_with_scope();

                let arguments_index = elems
                    .iter()
                    .enumerate()
                    .filter_map(|(index, item)| {
                        let field_index = index;

                        let mut inner_stack = expect_stack.empty_with_scope();

                        let name = self.extract_arg_name(
                            item,
                            &mut inner_stack,
                            type_map.get(&field_index).unwrap(),
                            &assignment_properties,
                        );

                        stacks.merge(inner_stack);

                        name.map(|name| (name, field_index))
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

                        self.expect_type(
                            type_map.get(&index).unwrap(),
                            &mut stacks,
                            &format!("__tuple_{index}_{id_next}"),
                            &mut IndexMap::new(),
                        )
                    }
                }

                expect_stack.tuple_accessor(
                    tipo.clone().into(),
                    final_args.into_iter().map(|(item, _)| item).collect_vec(),
                    true,
                    value_stack,
                );

                expect_stack.merge_child(stacks);
            }
        }
    }

    fn expect_type(
        &mut self,
        tipo: &Type,
        expect_stack: &mut AirStack,
        name: &str,
        defined_data_types: &mut IndexMap<String, u64>,
    ) {
        let mut tipo = tipo.clone().into();
        builder::replace_opaque_type(&mut tipo, &self.data_types);

        if tipo.is_bool()
            || tipo.is_bytearray()
            || tipo.is_int()
            || tipo.is_string()
            || tipo.is_void()
            || tipo.get_generic().is_some()
            || tipo.is_data()
        {
        } else if tipo.is_map() {
            let new_id = self.id_gen.next();
            let id_pair = (self.id_gen.next(), self.id_gen.next());
            let inner_list_type = &tipo.get_inner_types()[0];
            let inner_pair_types = inner_list_type.get_inner_types();

            let mut unwrap_function_stack = expect_stack.empty_with_scope();
            let mut pair_access_stack = unwrap_function_stack.empty_with_scope();
            let mut local_var_stack = pair_access_stack.empty_with_scope();

            local_var_stack.local_var(inner_list_type.clone(), format!("__pair_{new_id}"));

            pair_access_stack.tuple_accessor(
                inner_list_type.clone(),
                vec![
                    format!("__pair_fst_{}", id_pair.0),
                    format!("__pair_snd_{}", id_pair.1),
                ],
                false,
                local_var_stack,
            );

            self.expect_type(
                &inner_pair_types[0],
                &mut pair_access_stack,
                &format!("__pair_fst_{}", id_pair.0),
                defined_data_types,
            );

            self.expect_type(
                &inner_pair_types[1],
                &mut pair_access_stack,
                &format!("__pair_snd_{}", id_pair.1),
                defined_data_types,
            );

            unwrap_function_stack
                .anonymous_function(vec![format!("__pair_{new_id}")], pair_access_stack);

            let function = self.code_gen_functions.get(EXPECT_ON_LIST);

            if function.is_none() {
                let mut expect_list_stack = expect_stack.empty_with_scope();

                expect_list_stack.expect_on_list();
                self.code_gen_functions.insert(
                    EXPECT_ON_LIST.to_string(),
                    CodeGenFunction::Function(expect_list_stack.complete(), vec![]),
                );
            }

            if let Some(counter) = defined_data_types.get_mut(EXPECT_ON_LIST) {
                *counter += 1
            } else {
                defined_data_types.insert(EXPECT_ON_LIST.to_string(), 1);
            }

            expect_stack.expect_list_from_data(tipo.clone(), name, unwrap_function_stack);

            expect_stack.void();
        } else if tipo.is_list() {
            let new_id = self.id_gen.next();
            let inner_list_type = &tipo.get_inner_types()[0];

            let mut unwrap_function_stack = expect_stack.empty_with_scope();
            let mut list_access_stack = unwrap_function_stack.empty_with_scope();
            let mut local_var_stack = list_access_stack.empty_with_scope();

            local_var_stack.un_wrap_data(inner_list_type.clone());
            local_var_stack.local_var(inner_list_type.clone(), format!("__list_item_{new_id}"));

            list_access_stack.let_assignment(format!("__list_item_{new_id}"), local_var_stack);

            self.expect_type(
                inner_list_type,
                &mut list_access_stack,
                &format!("__list_item_{new_id}"),
                defined_data_types,
            );

            unwrap_function_stack
                .anonymous_function(vec![format!("__list_item_{new_id}")], list_access_stack);

            let function = self.code_gen_functions.get(EXPECT_ON_LIST);

            if function.is_none() {
                let mut expect_list_stack = expect_stack.empty_with_scope();

                expect_list_stack.expect_on_list();
                self.code_gen_functions.insert(
                    EXPECT_ON_LIST.to_string(),
                    CodeGenFunction::Function(expect_list_stack.complete(), vec![]),
                );
            }

            if let Some(counter) = defined_data_types.get_mut(EXPECT_ON_LIST) {
                *counter += 1
            } else {
                defined_data_types.insert(EXPECT_ON_LIST.to_string(), 1);
            }

            expect_stack.expect_list_from_data(tipo.clone(), name, unwrap_function_stack);

            expect_stack.void();
        } else if tipo.is_tuple() {
            let tuple_inner_types = tipo.get_inner_types();
            let mut new_id_list = vec![];

            for (index, _) in tuple_inner_types.iter().enumerate() {
                new_id_list.push((index, self.id_gen.next()));
            }

            let mut local_var_stack = expect_stack.empty_with_scope();

            local_var_stack.local_var(tipo.clone(), name);

            let names = new_id_list
                .iter()
                .map(|(index, id)| format!("__tuple_index_{index}_{id}"))
                .collect();

            expect_stack.tuple_accessor(tipo.clone(), names, true, local_var_stack);

            for (index, name) in new_id_list
                .into_iter()
                .map(|(index, id)| (index, format!("__tuple_index_{index}_{id}")))
            {
                self.expect_type(
                    &tuple_inner_types[index],
                    expect_stack,
                    &name,
                    defined_data_types,
                );
            }
        } else {
            let data_type = builder::lookup_data_type_by_tipo(&self.data_types, &tipo).unwrap();

            let new_id = self.id_gen.next();

            let mut var_stack = expect_stack.empty_with_scope();
            let mut func_stack = expect_stack.empty_with_scope();
            let mut call_stack = expect_stack.empty_with_scope();

            let mut data_type_variant = String::new();

            if let Some(types) = tipo.arg_types() {
                for tipo in types {
                    get_variant_name(&mut data_type_variant, &tipo);
                }
            }

            let data_type_name = format!("__expect_{}{}", data_type.name, data_type_variant);

            let function = self.code_gen_functions.get(&data_type_name);

            if function.is_none() && defined_data_types.get(&data_type_name).is_none() {
                defined_data_types.insert(data_type_name.clone(), 1);

                let mono_types: IndexMap<u64, Arc<Type>> = if !data_type.typed_parameters.is_empty()
                {
                    data_type
                        .typed_parameters
                        .iter()
                        .zip(tipo.arg_types().unwrap())
                        .flat_map(|item| get_generic_id_and_type(item.0, &item.1))
                        .collect()
                } else {
                    vec![].into_iter().collect()
                };

                let current_defined_state = defined_data_types.clone();
                let mut diff_defined_types = IndexMap::new();

                let mut clause_stack = expect_stack.empty_with_scope();
                let mut when_stack = expect_stack.empty_with_scope();
                let mut trace_stack = expect_stack.empty_with_scope();
                let mut subject_stack = expect_stack.empty_with_scope();
                let mut data_type_stack = expect_stack.empty_with_scope();

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

                            let mut arg_tipo = arg.tipo.clone();

                            find_and_replace_generics(&mut arg_tipo, &mono_types);

                            (index, arg_name, arg_tipo)
                        })
                        .collect_vec();

                    let mut arg_stack = expect_stack.empty_with_scope();

                    let mut arg_stack = if !arg_indices.is_empty() {
                        let mut field_expose_stack = expect_stack.empty_with_scope();

                        field_expose_stack.integer(index.to_string());

                        arg_stack.local_var(tipo.clone(), name);

                        field_expose_stack.fields_expose(arg_indices.clone(), true, arg_stack);

                        field_expose_stack
                    } else {
                        let mut var_stack = expect_stack.empty_with_scope();

                        var_stack.local_var(tipo.clone(), name);

                        arg_stack.integer(index.to_string());

                        arg_stack.fields_empty(var_stack);

                        arg_stack
                    };

                    for (_index, name, tipo) in arg_indices.clone() {
                        let mut call_stack = arg_stack.empty_with_scope();

                        self.expect_type(&tipo, &mut call_stack, &name, defined_data_types);

                        arg_stack.merge_child(call_stack);
                    }

                    for (inner_data_type, inner_count) in defined_data_types.iter() {
                        if let Some(prev_count) = current_defined_state.get(inner_data_type) {
                            diff_defined_types
                                .insert(inner_data_type.to_string(), *inner_count - *prev_count);
                        } else {
                            diff_defined_types.insert(inner_data_type.to_string(), *inner_count);
                        }
                    }

                    arg_stack.void();

                    clause_stack.clause(
                        tipo.clone(),
                        format!("__subject_{new_id}"),
                        false,
                        arg_stack,
                    );
                }

                trace_stack.trace(tipo.clone());

                trace_stack.string("Constr index did not match any type variant");

                trace_stack.error(tipo.clone());

                subject_stack.local_var(tipo.clone(), name);

                when_stack.when(
                    tipo.clone(),
                    format!("__subject_{new_id}"),
                    subject_stack,
                    clause_stack,
                    trace_stack,
                );

                let recursive = *diff_defined_types.get(&data_type_name).unwrap() > 0;

                data_type_stack.define_func(
                    &data_type_name,
                    "",
                    "",
                    vec![name.to_string()],
                    recursive,
                    when_stack,
                );

                self.code_gen_functions.insert(
                    data_type_name.clone(),
                    CodeGenFunction::Function(
                        data_type_stack.complete(),
                        diff_defined_types
                            .into_iter()
                            .filter(|(dt, counter)| dt != &data_type_name && *counter > 0)
                            .map(|(x, _)| x)
                            .collect_vec(),
                    ),
                );
            } else if let Some(counter) = defined_data_types.get_mut(&data_type_name) {
                *counter += 1;
            } else {
                defined_data_types.insert(data_type_name.clone(), 1);
            }

            func_stack.var(
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

            var_stack.local_var(tipo.clone(), name);

            call_stack.call(tipo.clone(), func_stack, vec![var_stack]);

            expect_stack.expect_constr_from_data(tipo, call_stack);
        }
    }

    fn extract_arg_name(
        &mut self,
        item: &Pattern<PatternConstructor, Arc<Type>>,
        nested_pattern_stack: &mut AirStack,
        tipo: &Type,
        assignment_properties: &AssignmentProperties,
    ) -> Option<String> {
        match item {
            Pattern::Var { name, .. } => Some(name.clone()),
            Pattern::Discard { .. } => None,
            a @ Pattern::List { .. } => {
                let id = self.id_gen.next();
                let list_name = format!("__list_{id}");

                let mut value_stack = nested_pattern_stack.empty_with_scope();

                value_stack.local_var(tipo.clone().into(), list_name.clone());

                if matches!(assignment_properties.kind, AssignmentKind::Expect)
                    && assignment_properties.value_type.is_data()
                    && !tipo.is_data()
                {
                    self.expect_pattern(
                        a,
                        nested_pattern_stack,
                        value_stack,
                        tipo,
                        assignment_properties.clone(),
                    );
                } else {
                    self.pattern_ir(
                        a,
                        nested_pattern_stack,
                        value_stack,
                        tipo,
                        assignment_properties.clone(),
                    );
                }

                Some(list_name)
            }
            a @ Pattern::Constructor {
                tipo,
                name: constr_name,
                ..
            } => {
                let id = self.id_gen.next();
                let constr_name = format!("{constr_name}_{id}");

                let mut local_var_stack = nested_pattern_stack.empty_with_scope();

                local_var_stack.local_var(tipo.clone(), constr_name.clone());

                if matches!(assignment_properties.kind, AssignmentKind::Expect)
                    && assignment_properties.value_type.is_data()
                    && !tipo.is_data()
                {
                    self.expect_pattern(
                        a,
                        nested_pattern_stack,
                        local_var_stack,
                        tipo,
                        assignment_properties.clone(),
                    );
                } else {
                    self.pattern_ir(
                        a,
                        nested_pattern_stack,
                        local_var_stack,
                        tipo,
                        assignment_properties.clone(),
                    );
                }

                Some(constr_name)
            }
            a @ Pattern::Tuple { .. } => {
                let id = self.id_gen.next();
                let tuple_name = format!("__tuple_name_{id}");

                let mut local_var_stack = nested_pattern_stack.empty_with_scope();

                local_var_stack.local_var(tipo.clone().into(), tuple_name.clone());

                if matches!(assignment_properties.kind, AssignmentKind::Expect)
                    && assignment_properties.value_type.is_data()
                    && !tipo.is_data()
                {
                    self.expect_pattern(
                        a,
                        nested_pattern_stack,
                        local_var_stack,
                        tipo,
                        assignment_properties.clone(),
                    );
                } else {
                    self.pattern_ir(
                        a,
                        nested_pattern_stack,
                        local_var_stack,
                        tipo,
                        assignment_properties.clone(),
                    );
                }

                Some(tuple_name)
            }
            Pattern::Int { .. } => todo!("Extract Arg Name: Int"),
            Pattern::Assign { .. } => todo!("Extract Arg Name: Assign"),
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

            let func_scope = func_index_map.get(&function.0).unwrap().clone();

            for dep in funct_comp.dependencies.iter() {
                let Some(dep_scope) = func_index_map.get_mut(dep) else { unreachable!("Missing dependency scope.")};

                *dep_scope = dep_scope.common_ancestor(&func_scope);
            }

            dependency_map.insert(function.0, function.1);
        }

        dependency_vec.extend(
            dependency_map
                .iter()
                .filter(|(_, defined_in_zero_arg)| !**defined_in_zero_arg)
                .map(|(key, _)| key.clone())
                .unique()
                .collect_vec(),
        );

        for func in dependency_vec {
            if self.defined_functions.contains_key(&func) {
                continue;
            }

            let func_scope = func_index_map.get(&func).unwrap();

            let function_component = function_definitions.get(&func).unwrap();

            let mut dep_ir = vec![];

            if !function_component.args.is_empty() {
                // deal with function dependencies
                builder::handle_func_dependencies(
                    &mut dep_ir,
                    function_component,
                    &function_definitions,
                    &mut self.defined_functions,
                    &func_index_map,
                    func_scope,
                    &mut to_be_defined,
                    self.id_gen.clone(),
                );
                final_func_dep_ir.insert(func, dep_ir);
            } else {
                // since zero arg functions are run at compile time we need to pull all deps
                // note anon functions are not included in the above. They exist in a function anyway
                let mut defined_functions = IndexMap::new();

                // deal with function dependencies in zero arg functions
                builder::handle_func_dependencies(
                    &mut dep_ir,
                    function_component,
                    &function_definitions,
                    &mut defined_functions,
                    &func_index_map,
                    func_scope,
                    &mut to_be_defined,
                    self.id_gen.clone(),
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

            builder::handle_func_dependencies(
                &mut dep_ir,
                funt_comp,
                &function_definitions,
                &mut defined_functions,
                &func_index_map,
                func_scope,
                &mut to_be_defined,
                self.id_gen.clone(),
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
                        (func.1.common_ancestor(&ir.scope()) == ir.scope()
                            || (index == 0 && func.1.is_empty()))
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

                        let recursion_stack = AirStack {
                            id_gen: self.id_gen.clone(),
                            scope: scopes.clone(),
                            air: recursion_ir,
                        };

                        let mut func_stack = AirStack {
                            id_gen: self.id_gen.clone(),
                            scope: scopes.clone(),
                            air: vec![],
                        };

                        if func_comp.is_code_gen_func {
                            func_stack = recursion_stack
                        } else {
                            func_stack.define_func(
                                function_access_key.function_name.clone(),
                                function_access_key.module_name.clone(),
                                function_access_key.variant_name.clone(),
                                func_comp.args.clone(),
                                func_comp.recursive,
                                recursion_stack,
                            );
                        }

                        full_func_ir.extend(func_stack.complete());

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
        func_index_map: &mut IndexMap<FunctionAccessKey, Scope>,
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
                let Air::Var {  constructor,  variant_name, .. } = ir
                else {
                    continue;
                };

                let ValueConstructorVariant::ModuleFn { name: func_name,  module, builtin: None, .. } = constructor.variant
                else {
                    continue;
                };

                let ir_function_key = FunctionAccessKey {
                    module_name: module.clone(),
                    function_name: func_name.clone(),
                    variant_name: variant_name.clone(),
                };

                if recursion_func_map.contains_key(&ir_function_key) && func == &ir_function_key {
                    skip = true;
                } else if func == &ir_function_key {
                    recursion_func_map_to_add.insert(ir_function_key, ());
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
                        *entry = entry.common_ancestor(&item.1);
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
        func_index_map: &mut IndexMap<FunctionAccessKey, Scope>,
        in_zero_arg_func: bool,
    ) {
        let mut to_be_defined_map: IndexMap<FunctionAccessKey, Scope> = IndexMap::new();
        for (index, ir) in ir_stack.to_vec().iter().enumerate().rev() {
            // I tried putting the 2 let else together, but then formatting stopped working
            #[rustfmt::skip]
            let Air::Var {
                scope, constructor, ..
            } = ir else {
                let scope = ir.scope();

                process_scope_updates(&mut to_be_defined_map, &scope, func_index_map);
                continue;
            };

            #[rustfmt::skip]
            let ValueConstructorVariant::ModuleFn {name, module, builtin: None, ..} = &constructor.variant else {
                let scope = ir.scope();

                process_scope_updates(&mut to_be_defined_map, &scope, func_index_map);
                continue;
            };

            let non_variant_function_key = FunctionAccessKey {
                module_name: module.clone(),
                function_name: name.clone(),
                variant_name: String::new(),
            };

            if let Some(function) = self.functions.get(&non_variant_function_key).cloned() {
                let mut func_stack = AirStack::with_scope(self.id_gen.clone(), scope.clone());

                self.build(&function.body, &mut func_stack);

                let func_ir = func_stack.complete();

                let param_types = constructor.tipo.arg_types().unwrap();

                let mut mono_types: IndexMap<u64, Arc<Type>> = IndexMap::new();
                let mut map = mono_types.into_iter().collect_vec();

                for (index, arg) in function.arguments.iter().enumerate() {
                    if arg.tipo.is_generic() {
                        let param_type = &param_types[index];

                        map.append(&mut builder::get_generic_id_and_type(&arg.tipo, param_type));
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
                    let new_scope = scope.common_ancestor(scope_prev);

                    to_be_defined_map.insert(function_key, new_scope);
                } else if func_components.get(&function_key).is_some() {
                    to_be_defined_map.insert(function_key.clone(), scope.clone());
                } else {
                    to_be_defined_map.insert(function_key.clone(), scope.clone());
                    let mut func_calls = IndexMap::new();

                    for ir in func_ir.clone().into_iter() {
                        let Air::Var { constructor, ..} = ir else {
                            continue;
                        };

                        let ValueConstructorVariant::ModuleFn {
                                name : func_name, module, builtin: None, ..
                            } = &constructor.variant
                        else {
                            continue;
                        };

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
                            (function, &*constructor.tipo)
                        {
                            let param_types = constructor.tipo.arg_types().unwrap();

                            let mut map = vec![];

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

                            let mono_types: IndexMap<u64, Arc<Type>> = map.into_iter().collect();
                            let mut func_stack =
                                AirStack::with_scope(self.id_gen.clone(), scope.clone());

                            self.build(&function.body, &mut func_stack);

                            let temp_ir = func_stack.complete();

                            let (variant_name, _) =
                                builder::monomorphize(temp_ir, mono_types, &constructor.tipo);

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
                            is_code_gen_func: false,
                        },
                    );
                }
            } else if let Some(code_gen_func) = self.code_gen_functions.get(name).cloned() {
                // Get actual code gen func if link
                let (func_ir, dependencies) = match code_gen_func {
                    CodeGenFunction::Function(func_ir, dependencies) => (func_ir, dependencies),
                    CodeGenFunction::Link(func) => {
                        if let Some(CodeGenFunction::Function(func_ir, dependencies)) =
                            self.code_gen_functions.get(&func).cloned()
                        {
                            (func_ir, dependencies)
                        } else {
                            unreachable!("Link must resolve to a code gen function.");
                        }
                    }
                };

                let function_key = FunctionAccessKey {
                    module_name: "".to_string(),
                    function_name: name.to_string(),
                    variant_name: "".to_string(),
                };

                let function_stack = AirStack {
                    id_gen: self.id_gen.clone(),
                    scope: scope.clone(),
                    air: func_ir,
                };

                let mut new_stack = AirStack::with_scope(self.id_gen.clone(), scope.clone());

                new_stack.merge_child(function_stack);

                func_components.insert(
                    function_key.clone(),
                    FuncComponents {
                        ir: new_stack.complete(),
                        dependencies: dependencies
                            .into_iter()
                            .map(|item| FunctionAccessKey {
                                module_name: "".to_string(),
                                function_name: item,
                                variant_name: "".to_string(),
                            })
                            .collect_vec(),
                        recursive: false,
                        args: vec!["__one".to_string()],
                        defined_by_zero_arg: in_zero_arg_func,
                        is_code_gen_func: true,
                    },
                );

                to_be_defined_map.insert(function_key, scope.clone());
            } else {
                unreachable!("We found a function with no definitions");
            }
            process_scope_updates(&mut to_be_defined_map, scope, func_index_map);
        }

        // Still to be defined
        for func in to_be_defined_map.clone().iter() {
            let index_scope = func_index_map.get(func.0).unwrap();
            func_index_map.insert(func.0.clone(), func.1.common_ancestor(index_scope));
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
                    builder::replace_opaque_type(&mut replaced_type, &self.data_types);

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
                    builder::replace_opaque_type(&mut replaced_type, &self.data_types);

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
                    builder::replace_opaque_type(&mut replaced_type, &self.data_types);

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
                    builder::replace_opaque_type(&mut replaced_type, &self.data_types);

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
                    builder::replace_opaque_type(&mut replaced_type, &self.data_types);

                    ir_stack[index] = Air::Builtin {
                        scope,
                        func,
                        count,
                        tipo: replaced_type,
                    };
                }
                Air::BinOp { tipo, scope, name } => {
                    let mut replaced_type = tipo.clone();
                    builder::replace_opaque_type(&mut replaced_type, &self.data_types);

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
                    builder::replace_opaque_type(&mut replaced_type, &self.data_types);

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
                    builder::replace_opaque_type(&mut replaced_type, &self.data_types);

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
                    builder::replace_opaque_type(&mut replaced_type, &self.data_types);

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
                    builder::replace_opaque_type(&mut replaced_type, &self.data_types);

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
                    builder::replace_opaque_type(&mut replaced_type, &self.data_types);

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
                    builder::replace_opaque_type(&mut replaced_type, &self.data_types);

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
                    builder::replace_opaque_type(&mut replaced_type, &self.data_types);

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
                    builder::replace_opaque_type(&mut replaced_type, &self.data_types);

                    ir_stack[index] = Air::TupleIndex {
                        scope,
                        tipo: replaced_type,
                        tuple_index,
                    };
                }
                Air::ErrorTerm { tipo, scope } => {
                    let mut replaced_type = tipo.clone();
                    builder::replace_opaque_type(&mut replaced_type, &self.data_types);

                    ir_stack[index] = Air::ErrorTerm {
                        scope,
                        tipo: replaced_type,
                    };
                }
                Air::Trace { tipo, scope } => {
                    let mut replaced_type = tipo.clone();
                    builder::replace_opaque_type(&mut replaced_type, &self.data_types);

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
                    builder::replace_opaque_type(&mut replaced_type, &self.data_types);

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
                        builder::replace_opaque_type(&mut replaced_type, &self.data_types);
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
                        builder::replace_opaque_type(&mut replaced_type, &self.data_types);

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
                            builder::replace_opaque_type(&mut replaced_type, &self.data_types);

                            ir_stack[index] = Air::RecordAccess {
                                scope,
                                record_index,
                                tipo: replaced_type,
                            };
                        }
                    } else {
                        let mut replaced_type = tipo.clone();
                        builder::replace_opaque_type(&mut replaced_type, &self.data_types);

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
                                builder::replace_opaque_type(&mut replaced_type, &self.data_types);
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
                            builder::replace_opaque_type(&mut replaced_type, &self.data_types);
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
                    builder::replace_opaque_type(&mut replaced_type, &self.data_types);

                    ir_stack[index] = Air::Call {
                        scope,
                        tipo: replaced_type,
                        count,
                    };
                }
                Air::If { scope, tipo } => {
                    let mut replaced_type = tipo.clone();
                    builder::replace_opaque_type(&mut replaced_type, &self.data_types);

                    ir_stack[index] = Air::If {
                        scope,
                        tipo: replaced_type,
                    };
                }
                Air::UnWrapData { scope, tipo } => {
                    let mut replaced_type = tipo.clone();
                    builder::replace_opaque_type(&mut replaced_type, &self.data_types);

                    ir_stack[index] = Air::UnWrapData {
                        scope,
                        tipo: replaced_type,
                    };
                }
                Air::WrapData { scope, tipo } => {
                    let mut replaced_type = tipo.clone();
                    builder::replace_opaque_type(&mut replaced_type, &self.data_types);

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
                                        .apply(builder::convert_type_to_data(
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
                let mut arg_vec = vec![];
                for _ in 0..count {
                    arg_vec.push(arg_stack.pop().unwrap());
                }

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
                        builder::undata_builtin(&func, count, &tipo, arg_vec)
                    }

                    DefaultFunction::MkCons | DefaultFunction::MkPairData => {
                        builder::to_data_builtin(&func, count, &tipo, arg_vec)
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
            Air::FieldsEmpty { .. } => {
                self.needs_field_access = true;

                let value = arg_stack.pop().unwrap();
                let mut term = arg_stack.pop().unwrap();

                term = Term::var(CONSTR_FIELDS_EXPOSER)
                    .apply(value)
                    .delayed_choose_list(
                        term,
                        Term::Error.trace(Term::string("Expected no fields for Constr")),
                    );

                arg_stack.push(term);
            }
            Air::ListEmpty { .. } => {
                let value = arg_stack.pop().unwrap();
                let mut term = arg_stack.pop().unwrap();

                term = value.delayed_choose_list(
                    term,
                    Term::Error.trace(Term::string("Expected no items for List")),
                );

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
                tipo,
                ..
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
            Air::UnOp { op, .. } => {
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
            Air::NoOp { .. } => {}
        }
    }

    pub fn wrap_validator_args(
        &mut self,
        validator_stack: &mut AirStack,
        arguments: &[TypedArg],
        has_context: bool,
    ) {
        let mut arg_stack = validator_stack.empty_with_scope();
        for (index, arg) in arguments.iter().enumerate().rev() {
            let arg_name = arg.arg_name.get_variable_name().unwrap_or("_").to_string();
            if !(has_context && index == arguments.len() - 1) && &arg_name != "_" {
                let mut param_stack = validator_stack.empty_with_scope();
                let mut value_stack = validator_stack.empty_with_scope();

                param_stack.local_var(data(), arg.arg_name.get_variable_name().unwrap_or("_"));

                let mut actual_type = arg.tipo.clone();

                replace_opaque_type(&mut actual_type, &self.data_types);

                self.assignment(
                    &Pattern::Var {
                        location: Span::empty(),
                        name: arg_name.to_string(),
                    },
                    &mut value_stack,
                    param_stack,
                    &actual_type,
                    AssignmentProperties {
                        value_type: data(),
                        kind: AssignmentKind::Expect,
                    },
                );
                value_stack.local_var(actual_type, &arg_name);

                arg_stack.let_assignment(arg_name, value_stack);
            }
        }

        validator_stack.anonymous_function(
            arguments
                .iter()
                .map(|arg| arg.arg_name.get_variable_name().unwrap_or("_").to_string())
                .collect_vec(),
            arg_stack,
        )
    }
}

fn process_scope_updates(
    to_be_defined_map: &mut IndexMap<FunctionAccessKey, Scope>,
    scope: &Scope,
    func_index_map: &mut IndexMap<FunctionAccessKey, Scope>,
) {
    for func in to_be_defined_map.clone().iter() {
        if &scope.common_ancestor(func.1) == scope {
            if let Some(index_scope) = func_index_map.get(func.0) {
                if &index_scope.common_ancestor(func.1) == scope {
                    func_index_map.insert(func.0.clone(), scope.clone());
                    to_be_defined_map.shift_remove(func.0);
                } else {
                    to_be_defined_map.insert(func.0.clone(), index_scope.common_ancestor(func.1));
                }
            } else {
                func_index_map.insert(func.0.clone(), scope.clone());
                to_be_defined_map.shift_remove(func.0);
            }
        }
    }
}
