use std::{cmp::Ordering, collections::HashMap, rc::Rc, sync::Arc};

use indexmap::IndexMap;

use uplc::{
    ast::{Constant, Name, Program, Term, Unique},
    builtins::DefaultFunction,
    parser::interner::Interner,
};

use crate::{
    ast::{DataType, Function, ModuleConstant, Pattern, TypeAlias, TypedArg, Use},
    expr::TypedExpr,
    tipo::{self, ModuleValueConstructor, Type, ValueConstructorVariant},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ScopeLevels {
    scope_tracker: Vec<i32>,
    field_depth: i32,
}

impl ScopeLevels {
    pub fn new() -> Self {
        ScopeLevels {
            scope_tracker: vec![0],
            field_depth: 0,
        }
    }

    pub fn is_less_than(&self, other: &ScopeLevels, include_depth: bool) -> bool {
        if self.scope_tracker.is_empty() && !other.scope_tracker.is_empty() {
            return true;
        } else if other.scope_tracker.is_empty() {
            return false;
        }

        let mut result = self.scope_tracker.len() < other.scope_tracker.len()
            || (self.scope_tracker.len() == other.scope_tracker.len()
                && include_depth
                && self.field_depth < other.field_depth);

        for (scope_self, scope_other) in self.scope_tracker.iter().zip(other.scope_tracker.iter()) {
            match scope_self.cmp(scope_other) {
                std::cmp::Ordering::Less => {
                    result = true;
                    break;
                }
                std::cmp::Ordering::Equal => {}
                std::cmp::Ordering::Greater => {
                    result = false;
                    break;
                }
            }
        }
        result
    }

    pub fn scope_increment_sequence(&self, inc: i32) -> ScopeLevels {
        let mut new_scope = self.clone();
        *new_scope.scope_tracker.last_mut().unwrap() += inc;
        new_scope.scope_tracker.push(0);
        new_scope
    }

    pub fn scope_increment(&self, inc: i32) -> ScopeLevels {
        let mut new_scope = self.clone();
        *new_scope.scope_tracker.last_mut().unwrap() += inc;
        new_scope
    }

    pub fn depth_increment(&self, inc: i32) -> ScopeLevels {
        let mut new_scope = self.clone();
        new_scope.field_depth += inc;
        new_scope
    }
}

impl Default for ScopeLevels {
    fn default() -> Self {
        Self::new()
    }
}

pub struct CodeGenerator<'a> {
    uplc_function_holder: Vec<(String, Term<Name>)>,
    uplc_function_holder_lookup: IndexMap<(String, String), (ScopeLevels, TypedExpr)>,
    uplc_data_holder_lookup: IndexMap<(String, String, String), (ScopeLevels, TypedExpr)>,
    uplc_data_usage_holder_lookup: IndexMap<(String, String), ScopeLevels>,
    functions: &'a HashMap<(String, String), &'a Function<Arc<tipo::Type>, TypedExpr>>,
    type_aliases: &'a HashMap<(String, String), &'a TypeAlias<Arc<tipo::Type>>>,
    data_types: &'a HashMap<(String, String), &'a DataType<Arc<tipo::Type>>>,
    imports: &'a HashMap<(String, String), &'a Use<String>>,
    constants: &'a HashMap<(String, String), &'a ModuleConstant<Arc<tipo::Type>, String>>,
}

impl<'a> CodeGenerator<'a> {
    pub fn new(
        functions: &'a HashMap<(String, String), &'a Function<Arc<tipo::Type>, TypedExpr>>,
        type_aliases: &'a HashMap<(String, String), &'a TypeAlias<Arc<tipo::Type>>>,
        data_types: &'a HashMap<(String, String), &'a DataType<Arc<tipo::Type>>>,
        imports: &'a HashMap<(String, String), &'a Use<String>>,
        constants: &'a HashMap<(String, String), &'a ModuleConstant<Arc<tipo::Type>, String>>,
    ) -> Self {
        CodeGenerator {
            uplc_function_holder: Vec::new(),
            uplc_function_holder_lookup: IndexMap::new(),
            uplc_data_holder_lookup: IndexMap::new(),
            uplc_data_usage_holder_lookup: IndexMap::new(),
            functions,
            type_aliases,
            data_types,
            imports,
            constants,
        }
    }

    pub fn generate(&mut self, body: TypedExpr, arguments: Vec<TypedArg>) -> Program<Name> {
        self.recurse_scope_level(&body, ScopeLevels::new());

        let mut term = self.recurse_code_gen(&body, ScopeLevels::new());

        // Apply constr exposer to top level.
        term = Term::Apply {
            function: Term::Lambda {
                parameter_name: Name {
                    text: "constr_fields_exposer".to_string(),
                    unique: 0.into(),
                },
                body: term.into(),
            }
            .into(),
            argument: Term::Lambda {
                parameter_name: Name {
                    text: "constr_var".to_string(),
                    unique: 0.into(),
                },
                body: Term::Apply {
                    function: Term::Builtin(DefaultFunction::UnListData).into(),
                    argument: Term::Apply {
                        function: Term::Builtin(DefaultFunction::SndPair).into(),
                        argument: Term::Apply {
                            function: Term::Builtin(DefaultFunction::UnConstrData).into(),
                            argument: Term::Var(Name {
                                text: "constr_var".to_string(),
                                unique: 0.into(),
                            })
                            .into(),
                        }
                        .into(),
                    }
                    .into(),
                }
                .into(),
            }
            .into(),
        };

        //Apply constr arg getter to top level.

        term = Term::Apply {
            function: Term::Lambda {
                parameter_name: Name {
                    text: "constr_field_get_arg".to_string(),
                    unique: 0.into(),
                },
                body: term.into(),
            }
            .into(),
            argument: Term::Lambda {
                parameter_name: Name {
                    text: "constr_list".to_string(),
                    unique: 0.into(),
                },
                body: Term::Lambda {
                    parameter_name: Name {
                        text: "arg_number".to_string(),
                        unique: 0.into(),
                    },
                    body: Term::Apply {
                        function: Term::Lambda {
                            parameter_name: Name {
                                text: "recurse".to_string(),
                                unique: 0.into(),
                            },
                            body: Term::Apply {
                                function: Term::Apply {
                                    function: Term::Apply {
                                        function: Term::Var(Name {
                                            text: "recurse".to_string(),
                                            unique: 0.into(),
                                        })
                                        .into(),
                                        argument: Term::Var(Name {
                                            text: "recurse".to_string(),
                                            unique: 0.into(),
                                        })
                                        .into(),
                                    }
                                    .into(),

                                    // Start recursive with index 0 of list
                                    argument: Term::Constant(Constant::Integer(0.into())).into(),
                                }
                                .into(),
                                argument: Term::Var(Name {
                                    text: "constr_list".to_string(),
                                    unique: 0.into(),
                                })
                                .into(),
                            }
                            .into(),
                        }
                        .into(),

                        argument: Term::Lambda {
                            parameter_name: Name {
                                text: "self_recursor".to_string(),
                                unique: 0.into(),
                            },
                            body: Term::Lambda {
                                parameter_name: Name {
                                    text: "current_arg_number".to_string(),
                                    unique: 0.into(),
                                },
                                body: Term::Lambda {
                                    parameter_name: Name {
                                        text: "list_of_constr_args".to_string(),
                                        unique: 0.into(),
                                    },
                                    body: Term::Apply {
                                        function: Term::Apply {
                                            function: Term::Apply {
                                                function: Term::Apply {
                                                    function: Term::Force(
                                                        Term::Builtin(DefaultFunction::IfThenElse)
                                                            .into(),
                                                    )
                                                    .into(),
                                                    argument: Term::Apply {
                                                        function: Term::Apply {
                                                            function: Term::Builtin(
                                                                DefaultFunction::EqualsInteger,
                                                            )
                                                            .into(),
                                                            argument: Term::Var(Name {
                                                                text: "arg_number".to_string(),
                                                                unique: 0.into(),
                                                            })
                                                            .into(),
                                                        }
                                                        .into(),
                                                        argument: Term::Var(Name {
                                                            text: "current_arg_number".to_string(),
                                                            unique: 0.into(),
                                                        })
                                                        .into(),
                                                    }
                                                    .into(),
                                                }
                                                .into(),
                                                argument: Term::Force(
                                                    Term::Builtin(DefaultFunction::HeadList).into(),
                                                )
                                                .into(),
                                            }
                                            .into(),
                                            argument: Term::Lambda {
                                                parameter_name: Name {
                                                    text: "current_list_of_constr_args".to_string(),
                                                    unique: 0.into(),
                                                },
                                                body: Term::Apply {
                                                    function: Term::Apply {
                                                        function: Term::Apply {
                                                            function: Term::Var(Name {
                                                                text: "self_recursor".to_string(),
                                                                unique: 0.into(),
                                                            })
                                                            .into(),
                                                            argument: Term::Var(Name {
                                                                text: "self_recursor".to_string(),
                                                                unique: 0.into(),
                                                            })
                                                            .into(),
                                                        }
                                                        .into(),

                                                        argument: Term::Apply {
                                                            function: Term::Apply {
                                                                function: Term::Builtin(
                                                                    DefaultFunction::AddInteger,
                                                                )
                                                                .into(),
                                                                argument: Term::Var(Name {
                                                                    text: "current_arg_number"
                                                                        .to_string(),
                                                                    unique: 0.into(),
                                                                })
                                                                .into(),
                                                            }
                                                            .into(),
                                                            argument: Term::Constant(
                                                                Constant::Integer(1.into()),
                                                            )
                                                            .into(),
                                                        }
                                                        .into(),
                                                    }
                                                    .into(),

                                                    argument: Term::Apply {
                                                        function: Term::Force(
                                                            Term::Builtin(
                                                                DefaultFunction::TailList,
                                                            )
                                                            .into(),
                                                        )
                                                        .into(),

                                                        argument: Term::Var(Name {
                                                            text: "current_list_of_constr_args"
                                                                .to_string(),
                                                            unique: 0.into(),
                                                        })
                                                        .into(),
                                                    }
                                                    .into(),
                                                }
                                                .into(),
                                            }
                                            .into(),
                                        }
                                        .into(),
                                        argument: Term::Var(Name {
                                            text: "list_of_constr_args".to_string(),
                                            unique: 0.into(),
                                        })
                                        .into(),
                                    }
                                    .into(),
                                }
                                .into(),
                            }
                            .into(),
                        }
                        .into(),
                    }
                    .into(),
                }
                .into(),
            }
            .into(),
        };

        for arg in arguments.iter().rev() {
            term = Term::Lambda {
                parameter_name: uplc::ast::Name {
                    text: arg.arg_name.get_variable_name().unwrap_or("_").to_string(),
                    unique: Unique::new(0),
                },
                body: Rc::new(term),
            }
        }

        let mut program = Program {
            version: (1, 0, 0),
            term,
        };

        let mut interner = Interner::new();

        interner.program(&mut program);

        println!("{}", program.to_pretty());

        program
    }

    pub(crate) fn recurse_scope_level(&mut self, body: &TypedExpr, scope_level: ScopeLevels) {
        match dbg!(body) {
            TypedExpr::Int { .. } => {}
            TypedExpr::String { .. } => {}
            TypedExpr::ByteArray { .. } => {}
            TypedExpr::Sequence { expressions, .. } => {
                // let mut terms = Vec::new();
                for (i, exp) in expressions.iter().enumerate().rev() {
                    self.recurse_scope_level(
                        exp,
                        scope_level.scope_increment_sequence(i as i32 + 1),
                    );
                }
            }
            TypedExpr::Pipeline {
                location,
                expressions,
            } => todo!(),
            TypedExpr::Var {
                constructor, name, ..
            } => match (constructor.variant.clone(), (*constructor.tipo).clone()) {
                (ValueConstructorVariant::LocalVariable { .. }, Type::App { module, .. }) => {
                    if let Some(val) = self
                        .uplc_data_usage_holder_lookup
                        .get(&(module.to_string(), name.clone()))
                    {
                        if scope_level.is_less_than(val, false) {
                            self.uplc_data_usage_holder_lookup
                                .insert((module, name.clone()), scope_level);
                        }
                    }
                }
                _ => todo!(),
            },
            TypedExpr::Fn {
                location,
                tipo,
                is_capture,
                args,
                body,
                return_annotation,
            } => todo!(),
            TypedExpr::List {
                location,
                tipo,
                elements,
                tail,
            } => todo!(),
            TypedExpr::Call { fun, args, .. } => {
                self.recurse_scope_level(fun, scope_level.scope_increment(args.len() as i32 + 1));

                for (i, arg) in args.iter().enumerate() {
                    self.recurse_scope_level(&arg.value, scope_level.scope_increment(i as i32 + 1));
                }
            }
            TypedExpr::BinOp { left, right, .. } => {
                self.recurse_scope_level(left, scope_level.clone());
                self.recurse_scope_level(right, scope_level);
            }
            TypedExpr::Assignment { value, pattern, .. } => {
                self.recurse_scope_level_pattern(pattern, value, scope_level)
            }
            TypedExpr::Try {
                location,
                tipo,
                value,
                then,
                pattern,
            } => todo!(),
            TypedExpr::When {
                location,
                tipo,
                subjects,
                clauses,
            } => {
                for clause in clauses {
                    for pattern in clause.pattern.iter() {
                        self.recurse_scope_level_pattern(
                            pattern,
                            &clause.then,
                            scope_level.scope_increment_sequence(1),
                        )
                    }
                }
                todo!()
            }
            //if statements increase scope due to branching.
            TypedExpr::If {
                branches,
                final_else,
                ..
            } => {
                self.recurse_scope_level(final_else, scope_level.scope_increment_sequence(1));

                for branch in branches {
                    // Need some scoping count to potentially replace condition with var since we should assume a condition
                    // may be repeated 3 + times or be large enough series of binops to warrant var replacement
                    self.recurse_scope_level(
                        &branch.condition,
                        scope_level.scope_increment_sequence(1), // Since this happens before branching. Maybe not increase scope level
                    );

                    self.recurse_scope_level(&branch.body, scope_level.scope_increment_sequence(1));
                }
            }
            a @ TypedExpr::RecordAccess { label, record, .. } => {
                self.recurse_scope_level(record, scope_level.clone());
                let mut is_var = false;
                let mut current_var_name = "".to_string();
                let mut module = "".to_string();
                let mut current_record = *record.clone();
                let mut current_scope = scope_level.clone();
                while !is_var {
                    match current_record.clone() {
                        TypedExpr::Var {
                            constructor, name, ..
                        } => match (
                            constructor.clone().variant.clone(),
                            (*constructor.tipo).clone(),
                        ) {
                            (
                                ValueConstructorVariant::LocalVariable { .. },
                                Type::App {
                                    module: app_module, ..
                                },
                            ) => {
                                current_var_name = if current_var_name.is_empty() {
                                    name
                                } else {
                                    format!("{name}_field_{current_var_name}")
                                };
                                is_var = true;
                                module = app_module.to_string();
                            }
                            _ => todo!(),
                        },
                        TypedExpr::RecordAccess { label, record, .. } => {
                            current_var_name = if current_var_name.is_empty() {
                                label.to_string()
                            } else {
                                format!("{label}_field_{current_var_name}")
                            };
                            current_record = *record.clone();
                            current_scope = current_scope.depth_increment(1);
                        }
                        _ => {}
                    }
                }

                if let Some(val) = self.uplc_data_holder_lookup.get(&(
                    module.to_string(),
                    current_var_name.clone(),
                    label.clone(),
                )) {
                    if current_scope.is_less_than(&val.0, false) {
                        self.uplc_data_holder_lookup.insert(
                            (module.to_string(), current_var_name.clone(), label.clone()),
                            (current_scope.clone(), a.clone()),
                        );
                    }
                } else {
                    self.uplc_data_holder_lookup.insert(
                        (module.to_string(), current_var_name.clone(), label.clone()),
                        (current_scope.clone(), a.clone()),
                    );
                }

                if let Some(val) = self
                    .uplc_data_usage_holder_lookup
                    .get(&(module.to_string(), current_var_name.clone()))
                {
                    if current_scope.is_less_than(val, false) {
                        self.uplc_data_usage_holder_lookup
                            .insert((module, current_var_name), current_scope);
                    }
                } else {
                    self.uplc_data_usage_holder_lookup
                        .insert((module, current_var_name), current_scope);
                }
            }
            a @ TypedExpr::ModuleSelect { constructor, .. } => match constructor {
                ModuleValueConstructor::Record {
                    name,
                    arity,
                    tipo,
                    field_map,
                    location,
                } => todo!(),
                ModuleValueConstructor::Fn { module, name, .. } => {
                    if self
                        .uplc_function_holder_lookup
                        .get(&(module.to_string(), name.to_string()))
                        .is_none()
                    {
                        let func_def = self
                            .functions
                            .get(&(module.to_string(), name.to_string()))
                            .unwrap();

                        self.recurse_scope_level(
                            &func_def.body,
                            scope_level.scope_increment(func_def.arguments.len() as i32 + 1),
                        );

                        self.uplc_function_holder_lookup.insert(
                            (module.to_string(), name.to_string()),
                            (scope_level, a.clone()),
                        );
                    } else if scope_level.is_less_than(
                        &self
                            .uplc_function_holder_lookup
                            .get(&(module.to_string(), name.to_string()))
                            .unwrap()
                            .0,
                        false,
                    ) {
                        self.uplc_function_holder_lookup.insert(
                            (module.to_string(), name.to_string()),
                            (scope_level, a.clone()),
                        );
                    }
                }
                ModuleValueConstructor::Constant { literal, location } => todo!(),
            },
            TypedExpr::Todo {
                location,
                label,
                tipo,
            } => todo!(),
            TypedExpr::RecordUpdate {
                location,
                tipo,
                spread,
                args,
            } => todo!(),
            TypedExpr::Negate { location, value } => todo!(),
        }
    }

    fn recurse_scope_level_pattern<Constructor, Type>(
        &mut self,
        pattern: &Pattern<Constructor, Type>,
        value: &TypedExpr,
        scope_level: ScopeLevels,
    ) {
        match pattern {
            Pattern::Int { .. } | Pattern::String { .. } | Pattern::Var { .. } => {
                self.recurse_scope_level(value, scope_level);
            }

            Pattern::VarUsage {
                location,
                name,
                tipo,
            } => todo!(),
            Pattern::Assign {
                name,
                location,
                pattern,
            } => todo!(),
            Pattern::Discard { name, location } => todo!(),
            Pattern::List {
                location,
                elements,
                tail,
            } => todo!(),
            Pattern::Constructor {
                location,
                name,
                arguments,
                module,
                constructor,
                with_spread,
                tipo,
            } => {
                self.recurse_scope_level(value, scope_level);
                todo!()
            }
        }
    }

    fn recurse_code_gen(&mut self, body: &TypedExpr, scope_level: ScopeLevels) -> Term<Name> {
        match dbg!(body) {
            TypedExpr::Int { value, .. } => {
                Term::Constant(Constant::Integer(value.parse::<i128>().unwrap()))
            }
            TypedExpr::String { value, .. } => Term::Constant(Constant::String(value.clone())),
            TypedExpr::ByteArray { bytes, .. } => {
                Term::Constant(Constant::ByteString(bytes.clone()))
            }
            TypedExpr::Sequence {
                location,
                expressions,
            } => {
                for (i, exp) in expressions.iter().enumerate().rev() {
                    let mut term = self
                        .recurse_code_gen(exp, scope_level.scope_increment_sequence(i as i32 + 1));

                    term =
                        self.maybe_insert_def(term, scope_level.scope_increment_sequence(i as i32));

                    self.uplc_function_holder
                        .push(("".to_string(), term.clone()));
                }

                self.uplc_function_holder.pop().unwrap().1
            }
            TypedExpr::Pipeline {
                location,
                expressions,
            } => todo!(),
            TypedExpr::Var {
                location,
                constructor,
                name,
            } => {
                if name == "True" || name == "False" {
                    Term::Constant(Constant::Bool(name == "True"))
                } else {
                    match constructor.variant.clone() {
                        ValueConstructorVariant::LocalVariable { location } => Term::Var(Name {
                            text: name.to_string(),
                            unique: 0.into(),
                        }),
                        ValueConstructorVariant::ModuleConstant {
                            location,
                            module,
                            literal,
                        } => todo!(),
                        ValueConstructorVariant::ModuleFn {
                            name,
                            field_map,
                            module,
                            arity,
                            location,
                            builtin,
                        } => todo!(),
                        ValueConstructorVariant::Record {
                            name,
                            arity,
                            field_map,
                            location,
                            module,
                            constructors_count,
                        } => todo!(),
                    }
                }
            }
            TypedExpr::Fn {
                location,
                tipo,
                is_capture,
                args,
                body,
                return_annotation,
            } => todo!(),
            TypedExpr::List {
                location,
                tipo,
                elements,
                tail,
            } => todo!(),
            TypedExpr::Call {
                location,
                tipo,
                fun,
                args,
            } => {
                let mut term =
                    self.recurse_code_gen(fun, scope_level.scope_increment(args.len() as i32 + 1));

                for (i, arg) in args.iter().enumerate() {
                    term = Term::Apply {
                        function: term.into(),
                        argument: self
                            .recurse_code_gen(&arg.value, scope_level.scope_increment(i as i32 + 1))
                            .into(),
                    };
                }

                term
            }
            TypedExpr::BinOp {
                location,
                tipo,
                name,
                left,
                right,
            } => {
                let left_term = self.recurse_code_gen(left, scope_level.clone());

                let right_term = self.recurse_code_gen(right, scope_level);
                println!("NAME IS {name:#?}");
                match name {
                    Eq => match &*left.tipo() {
                        Type::App {
                            public,
                            module,
                            name,
                            args,
                        } => match name.as_str() {
                            "Int" => Term::Apply {
                                function: Term::Apply {
                                    function: Term::Builtin(DefaultFunction::EqualsInteger).into(),
                                    argument: left_term.into(),
                                }
                                .into(),
                                argument: right_term.into(),
                            },

                            "String" => Term::Apply {
                                function: Term::Apply {
                                    function: Term::Builtin(DefaultFunction::EqualsString).into(),
                                    argument: left_term.into(),
                                }
                                .into(),
                                argument: right_term.into(),
                            },

                            _ => todo!(),
                        },
                        Type::Fn { args, ret } => todo!(),
                        Type::Var { tipo } => todo!(),
                    },
                    And => todo!(),
                    Or => todo!(),
                    NotEq => todo!(),
                    LtInt => todo!(),
                    LtEqInt => todo!(),
                    GtEqInt => todo!(),
                    GtInt => todo!(),
                    AddInt => todo!(),
                    SubInt => todo!(),
                    MultInt => todo!(),
                    DivInt => todo!(),
                    ModInt => todo!(),
                }
            }
            TypedExpr::Assignment {
                location,
                tipo,
                value,
                pattern,
                kind,
            } => match pattern {
                Pattern::Int { location, value } => todo!(),
                Pattern::String { location, value } => todo!(),
                Pattern::Var { location, name } => Term::Apply {
                    function: Term::Lambda {
                        parameter_name: Name {
                            text: name.to_string(),
                            unique: 0.into(),
                        },
                        body: self.uplc_function_holder.pop().unwrap().1.into(),
                    }
                    .into(),
                    argument: self
                        .recurse_code_gen(value, scope_level.scope_increment_sequence(1))
                        .into(),
                },

                Pattern::VarUsage {
                    location,
                    name,
                    tipo,
                } => todo!(),
                Pattern::Assign {
                    name,
                    location,
                    pattern,
                } => todo!(),
                Pattern::Discard { name, location } => todo!(),
                Pattern::List {
                    location,
                    elements,
                    tail,
                } => todo!(),
                Pattern::Constructor {
                    location,
                    name,
                    arguments,
                    module,
                    constructor,
                    with_spread,
                    tipo,
                } => todo!(),
            },
            TypedExpr::Try {
                location,
                tipo,
                value,
                then,
                pattern,
            } => todo!(),
            TypedExpr::When {
                location,
                tipo,
                subjects,
                clauses,
            } => todo!(),
            //if statements increase scope due to branching.
            TypedExpr::If {
                branches,
                final_else,
                ..
            } => {
                let mut final_if_term =
                    self.recurse_code_gen(final_else, scope_level.scope_increment_sequence(1));

                if branches.len() == 1 {
                    let condition_term = self.recurse_code_gen(
                        &branches[0].condition,
                        scope_level.scope_increment_sequence(1),
                    );

                    let branch_term = self.recurse_code_gen(
                        &branches[0].body,
                        scope_level.scope_increment_sequence(1),
                    );

                    match (final_if_term.clone(), branch_term.clone()) {
                        (
                            Term::Var(..) | Term::Constant(..),
                            Term::Var(..) | Term::Constant(..),
                        ) => {
                            final_if_term = Term::Apply {
                                function: Rc::new(Term::Apply {
                                    function: Rc::new(Term::Apply {
                                        function: Rc::new(Term::Force(Rc::new(Term::Builtin(
                                            DefaultFunction::IfThenElse,
                                        )))),
                                        argument: Rc::new(condition_term),
                                    }),
                                    //If this is just a var then don't include delay
                                    argument: Rc::new(branch_term),
                                }),
                                //If this is just a var then don't include delay
                                argument: Rc::new(final_if_term.clone()),
                            };
                        }
                        _ => {
                            final_if_term = Term::Force(
                                Term::Apply {
                                    function: Rc::new(Term::Apply {
                                        function: Rc::new(Term::Apply {
                                            function: Rc::new(Term::Force(Rc::new(Term::Builtin(
                                                DefaultFunction::IfThenElse,
                                            )))),
                                            argument: Rc::new(condition_term),
                                        }),
                                        argument: Rc::new(Term::Delay(Rc::new(branch_term))),
                                    }),
                                    argument: Rc::new(Term::Delay(Rc::new(final_if_term.clone()))),
                                }
                                .into(),
                            );
                        }
                    }
                } else {
                    //TODO: for multi branch if statements we can insert function definitions between branches
                    for branch in branches {
                        let condition_term = self.recurse_code_gen(
                            &branch.condition,
                            scope_level.scope_increment_sequence(1),
                        );

                        let branch_term = self.recurse_code_gen(
                            &branch.body,
                            scope_level.scope_increment_sequence(1),
                        );

                        final_if_term = Term::Force(
                            Term::Apply {
                                function: Rc::new(Term::Apply {
                                    function: Rc::new(Term::Apply {
                                        function: Rc::new(Term::Force(Rc::new(Term::Builtin(
                                            DefaultFunction::IfThenElse,
                                        )))),
                                        argument: Rc::new(condition_term),
                                    }),
                                    argument: Rc::new(Term::Delay(Rc::new(branch_term))),
                                }),
                                argument: Rc::new(Term::Delay(Rc::new(final_if_term.clone()))),
                            }
                            .into(),
                        );
                    }
                }
                self.maybe_insert_def(final_if_term, scope_level)
            }
            TypedExpr::RecordAccess { label, record, .. } => {
                let mut is_var = false;
                let mut current_var_name = "".to_string();
                let mut current_record = *record.clone();
                while !is_var {
                    match current_record.clone() {
                        TypedExpr::Var {
                            constructor, name, ..
                        } => match (
                            constructor.clone().variant.clone(),
                            (*constructor.tipo).clone(),
                        ) {
                            (
                                ValueConstructorVariant::LocalVariable { .. },
                                Type::App {
                                    module: app_module, ..
                                },
                            ) => {
                                current_var_name = if current_var_name.is_empty() {
                                    name
                                } else {
                                    format!("{name}_field_{current_var_name}")
                                };
                                is_var = true;
                            }
                            _ => todo!(),
                        },
                        TypedExpr::RecordAccess { label, record, .. } => {
                            current_var_name = if current_var_name.is_empty() {
                                label.to_string()
                            } else {
                                format!("{label}_field_{current_var_name}")
                            };
                            current_record = *record.clone();
                        }
                        _ => {}
                    }
                }
                Term::Var(Name {
                    text: format!("{current_var_name}_field_{label}"),
                    unique: 0.into(),
                })
            }
            TypedExpr::ModuleSelect {
                location,
                tipo,
                label,
                module_name,
                module_alias,
                constructor,
            } => match constructor {
                ModuleValueConstructor::Record {
                    name,
                    arity,
                    tipo,
                    field_map,
                    location,
                } => todo!(),
                ModuleValueConstructor::Fn {
                    location,
                    module,
                    name,
                } => Term::Var(Name {
                    text: format!("{module}_{name}"),
                    unique: 0.into(),
                }),
                ModuleValueConstructor::Constant { literal, location } => todo!(),
            },
            TypedExpr::Todo {
                location,
                label,
                tipo,
            } => todo!(),
            TypedExpr::RecordUpdate {
                location,
                tipo,
                spread,
                args,
            } => todo!(),
            TypedExpr::Negate { location, value } => todo!(),
        }
    }

    fn maybe_insert_def(
        &mut self,
        current_term: Term<Name>,
        scope_level: ScopeLevels,
    ) -> Term<Name> {
        let mut term = current_term;
        for func in self.uplc_function_holder_lookup.clone().keys() {
            if scope_level.is_less_than(
                &self
                    .uplc_function_holder_lookup
                    .clone()
                    .get(func)
                    .unwrap()
                    .0,
                false,
            ) {
                let func_def = self
                    .functions
                    .get(&(func.0.to_string(), func.1.to_string()))
                    .unwrap();

                let mut function_body = self.recurse_code_gen(
                    &func_def.body,
                    scope_level.scope_increment_sequence(func_def.arguments.len() as i32),
                );

                for arg in func_def.arguments.iter().rev() {
                    function_body = Term::Lambda {
                        parameter_name: Name {
                            text: arg.arg_name.get_variable_name().unwrap_or("_").to_string(),
                            unique: Unique::new(0),
                        },
                        body: Rc::new(function_body),
                    }
                }

                term = Term::Apply {
                    function: Term::Lambda {
                        parameter_name: Name {
                            text: format!("{}_{}", func.0, func.1),
                            unique: 0.into(),
                        },
                        body: term.into(),
                    }
                    .into(),
                    argument: function_body.into(),
                };
                self.uplc_function_holder_lookup.shift_remove(func);
            }
        }

        // Pull out all uplc data holder and data usage, Sort By Scope Level, Then
        let mut data_holder: Vec<((String, String, String), (bool, ScopeLevels, u64))> = self
            .uplc_data_usage_holder_lookup
            .iter()
            .filter(|record_scope| scope_level.is_less_than(&record_scope.1, false))
            .map(|((module, name), scope)| {
                (
                    (module.to_string(), name.to_string(), "".to_string()),
                    (true, scope.clone(), 0),
                )
            })
            .collect();

        data_holder.extend(
            self.uplc_data_holder_lookup
                .iter()
                .filter(|record_scope| scope_level.is_less_than(&record_scope.1 .0, false))
                .map(|((module, name, label), (scope, expr))| {
                    let index = match expr {
                        TypedExpr::RecordAccess { index, .. } => index,
                        _ => todo!(),
                    };
                    (
                        (module.to_string(), name.to_string(), label.to_string()),
                        (false, scope.clone(), *index),
                    )
                })
                .collect::<Vec<((String, String, String), (bool, ScopeLevels, u64))>>(),
        );
        data_holder.sort_by(|b, d| {
            if b.1 .1.is_less_than(&d.1 .1, true) {
                Ordering::Less
            } else if d.1 .1.is_less_than(&b.1 .1, true) {
                Ordering::Greater
            } else if b.1 .0 && !d.1 .0 {
                Ordering::Less
            } else if d.1 .0 && !b.1 .0 {
                Ordering::Greater
            } else {
                Ordering::Equal
            }
        });

        for (key @ (module, name, label), (is_data_usage, _, index)) in data_holder.iter().rev() {
            if *is_data_usage {
                term = Term::Apply {
                    function: Term::Lambda {
                        parameter_name: Name {
                            text: format!("{name}_fields"),
                            unique: 0.into(),
                        },
                        body: term.into(),
                    }
                    .into(),
                    // TODO: Find proper scope for this function if at all.
                    argument: Term::Apply {
                        function: Term::Var(Name {
                            text: "constr_fields_exposer".to_string(),
                            unique: 0.into(),
                        })
                        .into(),
                        argument: Term::Var(Name {
                            text: name.to_string(),
                            unique: 0.into(),
                        })
                        .into(),
                    }
                    .into(),
                };

                self.uplc_data_usage_holder_lookup
                    .shift_remove(&(module.clone(), name.clone()));
            } else {
                term = Term::Apply {
                    function: Term::Lambda {
                        parameter_name: Name {
                            text: format!("{name}_field_{label}"),
                            unique: 0.into(),
                        },
                        body: term.into(),
                    }
                    .into(),
                    argument: Term::Apply {
                        function: Term::Apply {
                            function: Term::Var(Name {
                                text: "constr_field_get_arg".to_string(),
                                unique: 0.into(),
                            })
                            .into(),
                            argument: Term::Var(Name {
                                text: format!("{name}_fields"),
                                unique: 0.into(),
                            })
                            .into(),
                        }
                        .into(),
                        argument: Term::Constant(Constant::Integer(*index as i128)).into(),
                    }
                    .into(),
                };
                self.uplc_data_holder_lookup.shift_remove(key);
            }
        }

        term
    }
}
