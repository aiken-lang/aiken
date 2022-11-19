use std::{cmp::Ordering, collections::HashMap, rc::Rc, sync::Arc};

use indexmap::IndexMap;

use uplc::{
    ast::{Constant, Name, Program, Term, Unique},
    builtins::DefaultFunction,
    parser::interner::Interner,
};

use crate::{
    ast::{AssignmentKind, BinOp, DataType, Function, Pattern, Span, TypedArg, TypedPattern},
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

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct ConstrFieldKey {
    pub local_var: String,
    pub field_name: String,
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct DataTypeKey {
    pub module_name: String,
    pub defined_type: String,
}

pub type ConstrUsageKey = String;

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct FunctionAccessKey {
    pub module_name: String,
    pub function_name: String,
}

#[derive(Clone)]
pub struct ConstrConversionInfo {
    local_var: String,
    field: Option<String>,
    scope: ScopeLevels,
    index: Option<u64>,
    returning_type: String,
}

#[derive(Clone)]
pub struct ScopedExpr {
    scope: ScopeLevels,
    expr: TypedExpr,
}

pub struct CodeGenerator<'a> {
    uplc_function_holder: Vec<(String, Term<Name>)>,
    uplc_function_holder_lookup: IndexMap<FunctionAccessKey, ScopeLevels>,
    uplc_data_holder_lookup: IndexMap<ConstrFieldKey, ScopedExpr>,
    uplc_data_constr_lookup: IndexMap<DataTypeKey, ScopeLevels>,
    uplc_data_usage_holder_lookup: IndexMap<ConstrUsageKey, ScopeLevels>,
    functions: &'a HashMap<FunctionAccessKey, &'a Function<Arc<tipo::Type>, TypedExpr>>,
    // type_aliases: &'a HashMap<(String, String), &'a TypeAlias<Arc<tipo::Type>>>,
    data_types: &'a HashMap<DataTypeKey, &'a DataType<Arc<tipo::Type>>>,
    // imports: &'a HashMap<(String, String), &'a Use<String>>,
    // constants: &'a HashMap<(String, String), &'a ModuleConstant<Arc<tipo::Type>, String>>,
}

impl<'a> CodeGenerator<'a> {
    pub fn new(
        functions: &'a HashMap<FunctionAccessKey, &'a Function<Arc<tipo::Type>, TypedExpr>>,
        // type_aliases: &'a HashMap<(String, String), &'a TypeAlias<Arc<tipo::Type>>>,
        data_types: &'a HashMap<DataTypeKey, &'a DataType<Arc<tipo::Type>>>,
        // imports: &'a HashMap<(String, String), &'a Use<String>>,
        // constants: &'a HashMap<(String, String), &'a ModuleConstant<Arc<tipo::Type>, String>>,
    ) -> Self {
        CodeGenerator {
            uplc_function_holder: Vec::new(),
            uplc_function_holder_lookup: IndexMap::new(),
            uplc_data_holder_lookup: IndexMap::new(),
            uplc_data_constr_lookup: IndexMap::new(),
            uplc_data_usage_holder_lookup: IndexMap::new(),
            functions,
            // type_aliases,
            data_types,
            // imports,
            // constants,
        }
    }

    pub fn generate(&mut self, body: TypedExpr, arguments: Vec<TypedArg>) -> Program<Name> {
        self.recurse_scope_level(&body, ScopeLevels::new());

        self.uplc_function_holder_lookup
            .sort_by(|_key1, value1, _key2, value2| {
                if value1.is_less_than(value2, true) {
                    Ordering::Less
                } else if value2.is_less_than(value1, true) {
                    Ordering::Greater
                } else {
                    Ordering::Equal
                }
            });

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

        term = self.add_arg_getter(term);

        term = Term::Force(
            Term::Apply {
                function: Term::Apply {
                    function: Term::Apply {
                        function: Term::Force(Term::Builtin(DefaultFunction::IfThenElse).into())
                            .into(),
                        argument: term.into(),
                    }
                    .into(),
                    argument: Term::Delay(Term::Constant(Constant::Unit).into()).into(),
                }
                .into(),
                argument: Term::Delay(Term::Error.into()).into(),
            }
            .into(),
        );

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

        program
    }

    pub(crate) fn recurse_scope_level(&mut self, body: &TypedExpr, scope_level: ScopeLevels) {
        match body {
            TypedExpr::Int { .. } => {}
            TypedExpr::String { .. } => {}
            TypedExpr::ByteArray { .. } => {}
            TypedExpr::Sequence { expressions, .. } | TypedExpr::Pipeline { expressions, .. } => {
                // let mut terms = Vec::new();
                for (i, exp) in expressions.iter().enumerate().rev() {
                    self.recurse_scope_level(
                        exp,
                        scope_level.scope_increment_sequence(i as i32 + 1),
                    );
                }
            }

            TypedExpr::Var { constructor, .. } => {
                match constructor.variant.clone() {
                    ValueConstructorVariant::LocalVariable { .. } => {}
                    ValueConstructorVariant::ModuleConstant { .. } => todo!(),
                    ValueConstructorVariant::ModuleFn { name, module, .. } => {
                        if self
                            .uplc_function_holder_lookup
                            .get(&FunctionAccessKey {
                                module_name: module.to_string(),
                                function_name: name.to_string(),
                            })
                            .is_none()
                        {
                            let func_def = self
                                .functions
                                .get(&FunctionAccessKey {
                                    module_name: module.to_string(),
                                    function_name: name.to_string(),
                                })
                                .unwrap();

                            self.recurse_scope_level(&func_def.body, scope_level.clone());

                            self.uplc_function_holder_lookup.insert(
                                FunctionAccessKey {
                                    module_name: module,
                                    function_name: name,
                                },
                                scope_level,
                            );
                        } else if scope_level.is_less_than(
                            self.uplc_function_holder_lookup
                                .get(&FunctionAccessKey {
                                    module_name: module.to_string(),
                                    function_name: name.to_string(),
                                })
                                .unwrap(),
                            false,
                        ) {
                            self.uplc_function_holder_lookup.insert(
                                FunctionAccessKey {
                                    module_name: module,
                                    function_name: name,
                                },
                                scope_level,
                            );
                        }
                    }
                    ValueConstructorVariant::Record { .. } => {
                        match &*constructor.tipo {
                            Type::App { .. } => {}
                            Type::Fn { .. } | Type::Var { .. } => {}
                        };
                    }
                };
            }
            TypedExpr::Fn { .. } => todo!(),
            TypedExpr::List { .. } => todo!(),
            TypedExpr::Call { fun, args, .. } => {
                self.recurse_scope_level(fun, scope_level.scope_increment(1));

                for (index, arg) in args.iter().enumerate() {
                    self.recurse_scope_level(
                        &arg.value,
                        scope_level.scope_increment(index as i32 + 2),
                    );
                }
            }
            TypedExpr::BinOp { left, right, .. } => {
                self.recurse_scope_level(left, scope_level.clone());
                self.recurse_scope_level(right, scope_level);
            }
            TypedExpr::Assignment { value, pattern, .. } => self.recurse_scope_level_pattern(
                pattern,
                value,
                scope_level.scope_increment(1),
                &[],
            ),
            TypedExpr::Try { .. } => todo!(),
            TypedExpr::When {
                subjects, clauses, ..
            } => {
                for clause in clauses {
                    for pattern in clause.pattern.iter() {
                        self.recurse_scope_level_pattern(
                            pattern,
                            &clause.then,
                            scope_level.scope_increment_sequence(1),
                            subjects,
                        );
                    }
                }
                for subject in subjects {
                    self.recurse_scope_level(subject, scope_level.clone());
                }
            }
            // if statements increase scope due to branching.
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
            expr @ TypedExpr::RecordAccess { label, record, .. } => {
                self.recurse_scope_level(record, scope_level.clone());
                let mut is_var = false;
                let mut current_var_name = String::new();
                let mut current_record = *record.clone();
                let mut current_scope = scope_level;
                while !is_var {
                    match current_record.clone() {
                        TypedExpr::Var {
                            constructor, name, ..
                        } => match (
                            constructor.clone().variant.clone(),
                            (*constructor.tipo).clone(),
                        ) {
                            (ValueConstructorVariant::LocalVariable { .. }, Type::App { .. }) => {
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
                            current_scope = current_scope.depth_increment(1);
                        }
                        _ => {}
                    }
                }

                if let Some(val) = self.uplc_data_holder_lookup.get(&ConstrFieldKey {
                    local_var: current_var_name.clone(),
                    field_name: label.clone(),
                }) {
                    if current_scope.is_less_than(&val.scope, false) {
                        self.uplc_data_holder_lookup.insert(
                            ConstrFieldKey {
                                local_var: current_var_name.clone(),
                                field_name: label.clone(),
                            },
                            ScopedExpr {
                                scope: current_scope.clone(),
                                expr: expr.clone(),
                            },
                        );
                    }
                } else {
                    self.uplc_data_holder_lookup.insert(
                        ConstrFieldKey {
                            local_var: current_var_name.clone(),
                            field_name: label.clone(),
                        },
                        ScopedExpr {
                            scope: current_scope.clone(),
                            expr: expr.clone(),
                        },
                    );
                }

                if let Some(val) = self
                    .uplc_data_usage_holder_lookup
                    .get(&current_var_name.clone())
                {
                    if current_scope.is_less_than(val, false) {
                        self.uplc_data_usage_holder_lookup
                            .insert(current_var_name, current_scope);
                    }
                } else {
                    self.uplc_data_usage_holder_lookup
                        .insert(current_var_name, current_scope);
                }
            }
            TypedExpr::ModuleSelect { constructor, .. } => match constructor {
                ModuleValueConstructor::Record { .. } => todo!(),
                ModuleValueConstructor::Fn { module, name, .. } => {
                    if self
                        .uplc_function_holder_lookup
                        .get(&FunctionAccessKey {
                            module_name: module.to_string(),
                            function_name: name.to_string(),
                        })
                        .is_none()
                    {
                        let func_def = self
                            .functions
                            .get(&FunctionAccessKey {
                                module_name: module.to_string(),
                                function_name: name.to_string(),
                            })
                            .unwrap();

                        self.recurse_scope_level(
                            &func_def.body,
                            scope_level
                                .scope_increment_sequence(func_def.arguments.len() as i32 + 1),
                        );

                        self.uplc_function_holder_lookup.insert(
                            FunctionAccessKey {
                                module_name: module.to_string(),
                                function_name: name.to_string(),
                            },
                            scope_level,
                        );
                    } else if scope_level.is_less_than(
                        self.uplc_function_holder_lookup
                            .get(&FunctionAccessKey {
                                module_name: module.to_string(),
                                function_name: name.to_string(),
                            })
                            .unwrap(),
                        false,
                    ) {
                        let func_def = self
                            .functions
                            .get(&FunctionAccessKey {
                                module_name: module.to_string(),
                                function_name: name.to_string(),
                            })
                            .unwrap();

                        self.uplc_function_holder_lookup.insert(
                            FunctionAccessKey {
                                module_name: module.to_string(),
                                function_name: name.to_string(),
                            },
                            scope_level
                                .scope_increment_sequence(func_def.arguments.len() as i32 + 1),
                        );
                    }
                }
                ModuleValueConstructor::Constant { .. } => todo!(),
            },
            TypedExpr::Todo { .. } => todo!(),
            TypedExpr::RecordUpdate { .. } => todo!(),
            TypedExpr::Negate { .. } => todo!(),
        }
    }

    fn recurse_scope_level_pattern(
        &mut self,
        pattern: &TypedPattern,
        value: &TypedExpr,
        scope_level: ScopeLevels,
        vars: &[TypedExpr],
    ) {
        match pattern {
            Pattern::Int { .. } | Pattern::String { .. } | Pattern::Var { .. } => {
                self.recurse_scope_level(value, scope_level);
            }

            Pattern::VarUsage { .. } => todo!(),
            Pattern::Assign { .. } => todo!(),
            Pattern::Discard { .. } => todo!(),
            Pattern::List { .. } => todo!(),
            Pattern::Constructor {
                name: constructor_name,
                tipo,
                arguments,
                constructor,
                module,
                ..
            } => {
                self.recurse_scope_level(value, scope_level.scope_increment_sequence(1));

                match &**tipo {
                    Type::App { module, name, .. } => {
                        if let Some(val) = self.uplc_data_constr_lookup.get(&DataTypeKey {
                            module_name: module.to_string(),
                            defined_type: name.clone(),
                        }) {
                            if scope_level.is_less_than(val, false) {
                                self.uplc_data_constr_lookup.insert(
                                    DataTypeKey {
                                        module_name: module.to_string(),
                                        defined_type: name.clone(),
                                    },
                                    scope_level,
                                );
                            }
                        } else {
                            self.uplc_data_constr_lookup.insert(
                                DataTypeKey {
                                    module_name: module.to_string(),
                                    defined_type: name.clone(),
                                },
                                scope_level,
                            );
                        }
                    }
                    Type::Fn { .. } => {
                        let mut mapping_index: IndexMap<String, usize> = IndexMap::new();

                        match constructor {
                            tipo::PatternConstructor::Record { field_map, .. } => {
                                if let Some(fields_mapping) = field_map {
                                    mapping_index.extend(fields_mapping.fields.clone());
                                    mapping_index
                                        .sort_by(|_, value1, _, value2| value1.cmp(value2));
                                    mapping_index.reverse();
                                }
                            }
                        };

                        let module = module.clone().unwrap();
                        // TODO: support multiple subjects
                        let (var_name, tipo) = match &vars[0] {
                            TypedExpr::Var {
                                name, constructor, ..
                            } => (name, constructor.tipo.clone()),
                            rest => todo!("implement: {:#?}", rest),
                        };

                        let mut type_name = String::new();
                        let mut is_app = false;
                        let current_tipo = &*tipo;
                        while !is_app {
                            match current_tipo {
                                Type::App { name, .. } => {
                                    type_name = name.to_string();
                                    is_app = true;
                                }
                                _ => todo!(),
                            };
                        }

                        for (ind, arg) in arguments.iter().rev().enumerate() {
                            let (label, index) = if let Some(arg_label) = &arg.label {
                                (
                                    arg_label.to_string(),
                                    mapping_index.remove(arg_label).unwrap() as u64,
                                )
                            } else {
                                let arg_field =
                                    mapping_index.pop().unwrap_or((format!("{ind}"), ind));
                                (arg_field.0, arg_field.1 as u64)
                            };

                            match &arg.value {
                                Pattern::Var {
                                    name: field_name, ..
                                } => {
                                    let record_access = TypedExpr::Assignment {
                                        location: Span::empty(),
                                        tipo: Type::App {
                                            public: true,
                                            module: module.clone(),
                                            name: constructor_name.to_string(),
                                            args: vec![],
                                        }
                                        .into(),
                                        value: TypedExpr::RecordAccess {
                                            location: Span::empty(),
                                            tipo: Type::App {
                                                public: true,
                                                module: module.clone(),
                                                name: constructor_name.to_string(),
                                                args: vec![],
                                            }
                                            .into(),
                                            label: label.clone(),
                                            index,
                                            record: TypedExpr::Var {
                                                location: Span::empty(),
                                                constructor: tipo::ValueConstructor {
                                                    public: false,
                                                    variant:
                                                        ValueConstructorVariant::LocalVariable {
                                                            location: Span::empty(),
                                                        },
                                                    tipo: Type::App {
                                                        public: true,
                                                        module: module.clone(),
                                                        name: type_name.clone(),
                                                        args: vec![],
                                                    }
                                                    .into(),
                                                },
                                                name: var_name.clone(),
                                            }
                                            .into(),
                                        }
                                        .into(),
                                        pattern: TypedPattern::Var {
                                            location: Span::empty(),
                                            name: field_name.clone(),
                                        },
                                        kind: AssignmentKind::Let,
                                    };

                                    if let Some(val) =
                                        self.uplc_data_holder_lookup.get(&ConstrFieldKey {
                                            local_var: var_name.clone(),
                                            field_name: label.clone(),
                                        })
                                    {
                                        if scope_level.is_less_than(&val.scope, false) {
                                            self.uplc_data_holder_lookup.insert(
                                                ConstrFieldKey {
                                                    local_var: var_name.clone(),
                                                    field_name: label.clone(),
                                                },
                                                ScopedExpr {
                                                    scope: scope_level.scope_increment(1),
                                                    expr: record_access.clone(),
                                                },
                                            );
                                        }
                                    } else {
                                        self.uplc_data_holder_lookup.insert(
                                            ConstrFieldKey {
                                                local_var: var_name.clone(),
                                                field_name: label.clone(),
                                            },
                                            ScopedExpr {
                                                scope: scope_level.scope_increment(1),
                                                expr: record_access.clone(),
                                            },
                                        );
                                    }

                                    if let Some(val) =
                                        self.uplc_data_usage_holder_lookup.get(&var_name.clone())
                                    {
                                        if scope_level.is_less_than(val, false) {
                                            self.uplc_data_usage_holder_lookup
                                                .insert(var_name.clone(), scope_level.clone());
                                        }
                                    } else {
                                        self.uplc_data_usage_holder_lookup
                                            .insert(var_name.clone(), scope_level.clone());
                                    }

                                    if let Some(val) =
                                        self.uplc_data_constr_lookup.get(&DataTypeKey {
                                            module_name: module.to_string(),
                                            defined_type: type_name.clone(),
                                        })
                                    {
                                        if scope_level.is_less_than(val, false) {
                                            self.uplc_data_constr_lookup.insert(
                                                DataTypeKey {
                                                    module_name: module.to_string(),
                                                    defined_type: type_name.clone(),
                                                },
                                                scope_level.clone(),
                                            );
                                        }
                                    } else {
                                        self.uplc_data_constr_lookup.insert(
                                            DataTypeKey {
                                                module_name: module.to_string(),
                                                defined_type: type_name.clone(),
                                            },
                                            scope_level.clone(),
                                        );
                                    }
                                }
                                _ => todo!(),
                            };
                        }
                    }
                    _ => todo!(),
                };
            }
        }
    }

    fn recurse_code_gen(&mut self, body: &TypedExpr, scope_level: ScopeLevels) -> Term<Name> {
        match body {
            TypedExpr::Int { value, .. } => {
                Term::Constant(Constant::Integer(value.parse::<i128>().unwrap()))
            }
            TypedExpr::String { value, .. } => Term::Constant(Constant::String(value.clone())),
            TypedExpr::ByteArray { bytes, .. } => {
                Term::Constant(Constant::ByteString(bytes.clone()))
            }
            TypedExpr::Sequence { expressions, .. } | TypedExpr::Pipeline { expressions, .. } => {
                for (i, exp) in expressions.iter().enumerate().rev() {
                    let mut term = self
                        .recurse_code_gen(exp, scope_level.scope_increment_sequence(i as i32 + 1));

                    term = self
                        .maybe_insert_def(term, scope_level.scope_increment_sequence(i as i32 + 1));

                    self.uplc_function_holder
                        .push((String::new(), term.clone()));
                }

                self.uplc_function_holder.pop().unwrap().1
            }
            TypedExpr::Var {
                constructor, name, ..
            } => {
                if name == "True" || name == "False" {
                    Term::Constant(Constant::Bool(name == "True"))
                } else {
                    match constructor.variant.clone() {
                        ValueConstructorVariant::LocalVariable { .. } => Term::Var(Name {
                            text: name.to_string(),
                            unique: 0.into(),
                        }),
                        ValueConstructorVariant::ModuleConstant { .. } => todo!(),
                        ValueConstructorVariant::ModuleFn { module, name, .. } => Term::Var(Name {
                            text: format!("{module}_{name}"),
                            unique: 0.into(),
                        }),
                        ValueConstructorVariant::Record { .. } => todo!(),
                    }
                }
            }
            TypedExpr::Fn { .. } => todo!(),
            TypedExpr::List { .. } => todo!(),
            TypedExpr::Call {
                fun, args, tipo, ..
            } => {
                if let (
                    Type::App { module, name, .. },
                    TypedExpr::Var {
                        name: constr_name, ..
                    },
                ) = (&**tipo, &**fun)
                {
                    let mut term: Term<Name> =
                        Term::Constant(Constant::ProtoList(uplc::ast::Type::Data, vec![]));

                    if let Some(data_type) = self.data_types.get(&DataTypeKey {
                        module_name: module.to_string(),
                        defined_type: name.to_string(),
                    }) {
                        let constr = data_type
                            .constructors
                            .iter()
                            .find(|x| x.name == *constr_name)
                            .unwrap();

                        let arg_to_data: Vec<(bool, Term<Name>)> = constr
                            .arguments
                            .iter()
                            .map(|x| {
                                if let Type::App { name, .. } = &*x.tipo {
                                    if name == "ByteArray" {
                                        (true, Term::Builtin(DefaultFunction::BData))
                                    } else if name == "Int" {
                                        (true, Term::Builtin(DefaultFunction::IData))
                                    } else {
                                        (false, Term::Constant(Constant::Unit))
                                    }
                                } else {
                                    unreachable!()
                                }
                            })
                            .collect();

                        for (i, arg) in args.iter().enumerate().rev() {
                            let arg_term = self.recurse_code_gen(
                                &arg.value,
                                scope_level.scope_increment(i as i32 + 1),
                            );

                            term = Term::Apply {
                                function: Term::Apply {
                                    function: Term::Force(
                                        Term::Builtin(DefaultFunction::MkCons).into(),
                                    )
                                    .into(),
                                    argument: if arg_to_data[i].0 {
                                        Term::Apply {
                                            function: arg_to_data[i].1.clone().into(),
                                            argument: arg_term.into(),
                                        }
                                        .into()
                                    } else {
                                        arg_term.into()
                                    },
                                }
                                .into(),
                                argument: term.into(),
                            };
                        }
                        term
                    } else {
                        let mut term = self.recurse_code_gen(fun, scope_level.scope_increment(1));

                        for (i, arg) in args.iter().enumerate() {
                            term = Term::Apply {
                                function: term.into(),
                                argument: self
                                    .recurse_code_gen(
                                        &arg.value,
                                        scope_level.scope_increment(i as i32 + 2),
                                    )
                                    .into(),
                            };
                        }
                        term
                    }
                } else {
                    let mut term = self.recurse_code_gen(fun, scope_level.scope_increment(1));

                    for (i, arg) in args.iter().enumerate() {
                        term = Term::Apply {
                            function: term.into(),
                            argument: self
                                .recurse_code_gen(
                                    &arg.value,
                                    scope_level.scope_increment(i as i32 + 2),
                                )
                                .into(),
                        };
                    }
                    term
                }
            }
            TypedExpr::BinOp {
                name, left, right, ..
            } => {
                let left_term = self.recurse_code_gen(left, scope_level.clone());

                let right_term = self.recurse_code_gen(right, scope_level);

                match name {
                    BinOp::Eq => match &*left.tipo() {
                        Type::App { name, .. } => match name.as_str() {
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

                            "ByteArray" => Term::Apply {
                                function: Term::Apply {
                                    function: Term::Builtin(DefaultFunction::EqualsByteString)
                                        .into(),
                                    argument: left_term.into(),
                                }
                                .into(),
                                argument: right_term.into(),
                            },

                            _ => todo!(),
                        },
                        Type::Fn { .. } => todo!(),
                        Type::Var { .. } => todo!(),
                    },
                    BinOp::And => Term::Force(
                        Term::Apply {
                            function: Term::Apply {
                                function: Term::Apply {
                                    function: Term::Force(
                                        Term::Builtin(DefaultFunction::IfThenElse).into(),
                                    )
                                    .into(),
                                    argument: left_term.into(),
                                }
                                .into(),
                                argument: Term::Delay(
                                    Term::Apply {
                                        function: Term::Apply {
                                            function: Term::Apply {
                                                function: Term::Force(
                                                    Term::Builtin(DefaultFunction::IfThenElse)
                                                        .into(),
                                                )
                                                .into(),
                                                argument: right_term.into(),
                                            }
                                            .into(),
                                            argument: Term::Constant(Constant::Bool(true)).into(),
                                        }
                                        .into(),
                                        argument: Term::Constant(Constant::Bool(false)).into(),
                                    }
                                    .into(),
                                )
                                .into(),
                            }
                            .into(),
                            argument: Term::Delay(Term::Constant(Constant::Bool(false)).into())
                                .into(),
                        }
                        .into(),
                    ),
                    BinOp::Or => Term::Force(
                        Term::Apply {
                            function: Term::Apply {
                                function: Term::Apply {
                                    function: Term::Force(
                                        Term::Builtin(DefaultFunction::IfThenElse).into(),
                                    )
                                    .into(),
                                    argument: left_term.into(),
                                }
                                .into(),
                                argument: Term::Delay(Term::Constant(Constant::Bool(true)).into())
                                    .into(),
                            }
                            .into(),
                            argument: Term::Delay(
                                Term::Apply {
                                    function: Term::Apply {
                                        function: Term::Apply {
                                            function: Term::Force(
                                                Term::Builtin(DefaultFunction::IfThenElse).into(),
                                            )
                                            .into(),
                                            argument: right_term.into(),
                                        }
                                        .into(),
                                        argument: Term::Constant(Constant::Bool(true)).into(),
                                    }
                                    .into(),
                                    argument: Term::Constant(Constant::Bool(false)).into(),
                                }
                                .into(),
                            )
                            .into(),
                        }
                        .into(),
                    ),
                    BinOp::NotEq => match &*left.tipo() {
                        Type::App { name, .. } => {
                            let equality = match name.as_str() {
                                "Int" => Term::Apply {
                                    function: Term::Apply {
                                        function: Term::Builtin(DefaultFunction::EqualsInteger)
                                            .into(),
                                        argument: left_term.into(),
                                    }
                                    .into(),
                                    argument: right_term.into(),
                                },

                                "String" => Term::Apply {
                                    function: Term::Apply {
                                        function: Term::Builtin(DefaultFunction::EqualsString)
                                            .into(),
                                        argument: left_term.into(),
                                    }
                                    .into(),
                                    argument: right_term.into(),
                                },

                                "ByteArray" => Term::Apply {
                                    function: Term::Apply {
                                        function: Term::Builtin(DefaultFunction::EqualsByteString)
                                            .into(),
                                        argument: left_term.into(),
                                    }
                                    .into(),
                                    argument: right_term.into(),
                                },

                                _ => todo!(),
                            };
                            Term::Apply {
                                function: Term::Apply {
                                    function: Term::Apply {
                                        function: Term::Force(
                                            Term::Builtin(DefaultFunction::IfThenElse).into(),
                                        )
                                        .into(),
                                        argument: equality.into(),
                                    }
                                    .into(),
                                    argument: Term::Constant(Constant::Bool(false)).into(),
                                }
                                .into(),
                                argument: Term::Constant(Constant::Bool(true)).into(),
                            }
                        }
                        Type::Fn { .. } => todo!(),
                        Type::Var { .. } => todo!(),
                    },
                    BinOp::LtInt => Term::Apply {
                        function: Term::Apply {
                            function: Term::Builtin(DefaultFunction::LessThanInteger).into(),
                            argument: left_term.into(),
                        }
                        .into(),
                        argument: right_term.into(),
                    },
                    BinOp::LtEqInt => Term::Apply {
                        function: Term::Apply {
                            function: Term::Builtin(DefaultFunction::LessThanEqualsInteger).into(),
                            argument: left_term.into(),
                        }
                        .into(),
                        argument: right_term.into(),
                    },
                    BinOp::GtEqInt => Term::Apply {
                        function: Term::Apply {
                            function: Term::Builtin(DefaultFunction::LessThanEqualsInteger).into(),
                            argument: right_term.into(),
                        }
                        .into(),
                        argument: left_term.into(),
                    },
                    BinOp::GtInt => Term::Apply {
                        function: Term::Apply {
                            function: Term::Builtin(DefaultFunction::LessThanInteger).into(),
                            argument: right_term.into(),
                        }
                        .into(),
                        argument: left_term.into(),
                    },
                    BinOp::AddInt => Term::Apply {
                        function: Term::Apply {
                            function: Term::Builtin(DefaultFunction::AddInteger).into(),
                            argument: left_term.into(),
                        }
                        .into(),
                        argument: right_term.into(),
                    },
                    BinOp::SubInt => Term::Apply {
                        function: Term::Apply {
                            function: Term::Builtin(DefaultFunction::SubtractInteger).into(),
                            argument: left_term.into(),
                        }
                        .into(),
                        argument: right_term.into(),
                    },
                    BinOp::MultInt => Term::Apply {
                        function: Term::Apply {
                            function: Term::Builtin(DefaultFunction::MultiplyInteger).into(),
                            argument: left_term.into(),
                        }
                        .into(),
                        argument: right_term.into(),
                    },
                    BinOp::DivInt => Term::Apply {
                        function: Term::Apply {
                            function: Term::Builtin(DefaultFunction::DivideInteger).into(),
                            argument: left_term.into(),
                        }
                        .into(),
                        argument: right_term.into(),
                    },
                    BinOp::ModInt => Term::Apply {
                        function: Term::Apply {
                            function: Term::Builtin(DefaultFunction::ModInteger).into(),
                            argument: left_term.into(),
                        }
                        .into(),
                        argument: right_term.into(),
                    },
                }
            }
            TypedExpr::Assignment { value, pattern, .. } => match pattern {
                Pattern::Int { .. } => todo!(),
                Pattern::String { .. } => todo!(),
                Pattern::Var { name, .. } => Term::Apply {
                    function: Term::Lambda {
                        parameter_name: Name {
                            text: name.to_string(),
                            unique: 0.into(),
                        },
                        body: self.uplc_function_holder.pop().unwrap().1.into(),
                    }
                    .into(),
                    argument: self
                        .recurse_code_gen(value, scope_level.scope_increment(1))
                        .into(),
                },

                Pattern::VarUsage { .. } => todo!(),
                Pattern::Assign { .. } => todo!(),
                Pattern::Discard { .. } => todo!(),
                Pattern::List { .. } => todo!(),
                Pattern::Constructor { .. } => todo!(),
            },
            TypedExpr::Try { .. } => todo!(),
            TypedExpr::When {
                subjects, clauses, ..
            } => {
                let subject = &subjects[0];

                let mut is_var = false;

                let mut current_var_name = String::new();

                let mut current_subject = subject.clone();

                while !is_var {
                    match current_subject.clone() {
                        TypedExpr::Var {
                            constructor, name, ..
                        } => match (
                            constructor.clone().variant.clone(),
                            (*constructor.tipo).clone(),
                        ) {
                            (ValueConstructorVariant::LocalVariable { .. }, Type::App { .. }) => {
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
                            current_subject = *record.clone();
                        }
                        _ => {}
                    }
                }

                let current_clauses = clauses.clone();

                let mut current_module = String::new();

                let mut total_constr_length = 0;
                let pattern = &clauses[0].pattern[0];

                let key = match pattern {
                    Pattern::Constructor { tipo, .. } => {
                        let mut is_app = false;
                        let mut tipo = &**tipo;
                        let mut key = DataTypeKey {
                            module_name: String::new(),
                            defined_type: String::new(),
                        };
                        while !is_app {
                            match tipo {
                                Type::App { module, name, .. } => {
                                    is_app = true;
                                    key.module_name = module.clone();
                                    key.defined_type = name.clone();
                                }
                                Type::Fn { ret, .. } => {
                                    tipo = ret;
                                }
                                _ => todo!(),
                            };
                        }

                        key
                    }
                    _ => todo!(),
                };

                let dt = self.data_types.get(&key).unwrap();
                let data_type = &dt.name;
                let mut new_current_clauses: Vec<(usize, Term<Name>)> = current_clauses
                    .iter()
                    .map(|clause| {
                        let pattern = &clause.pattern[0];
                        let pair = match pattern {
                            Pattern::Constructor { name, module, .. } => {
                                let index =
                                    dt.constructors.iter().position(|c| name.clone() == c.name);

                                let mut current_term = self.recurse_code_gen(
                                    &clause.then,
                                    scope_level.scope_increment_sequence(1),
                                );
                                if let Some(ind) = index {
                                    for (index, field) in
                                        dt.constructors[ind].arguments.iter().enumerate()
                                    {
                                        let label =
                                            field.clone().label.unwrap_or(format!("{index}"));

                                        if let Some(ScopedExpr {
                                            expr: TypedExpr::Assignment { pattern, .. },
                                            ..
                                        }) = self.uplc_data_holder_lookup.get(&ConstrFieldKey {
                                            local_var: current_var_name.to_string(),
                                            field_name: label.clone(),
                                        }) {
                                            let var_name = match pattern {
                                                Pattern::Var { name, .. } => name,
                                                _ => todo!(),
                                            };

                                            current_term = Term::Apply {
                                                function: Term::Lambda {
                                                    parameter_name: Name {
                                                        text: var_name.to_string(),
                                                        unique: 0.into(),
                                                    },
                                                    body: current_term.into(),
                                                }
                                                .into(),
                                                argument: Term::Var(Name {
                                                    text: format!(
                                                        "{current_var_name}_field_{label}"
                                                    ),
                                                    unique: 0.into(),
                                                })
                                                .into(),
                                            };
                                        }
                                    }
                                }

                                current_module = module.clone().unwrap_or_default();
                                total_constr_length = dt.constructors.len();

                                (index.unwrap_or(dt.constructors.len()), current_term)
                            }
                            _ => todo!(),
                        };
                        pair
                    })
                    .collect();

                new_current_clauses.sort_by(|a, b| a.0.cmp(&b.0));

                let mut term = Term::Apply {
                    function: Term::Var(Name {
                        text: format!("choose_{current_module}_{data_type}_constr"),
                        unique: 0.into(),
                    })
                    .into(),
                    argument: Term::Var(Name {
                        text: current_var_name,
                        unique: 0.into(),
                    })
                    .into(),
                };
                let need_lam = total_constr_length - new_current_clauses.len() > 0;

                let (last, new_current_clauses) = new_current_clauses.split_last().unwrap();

                let mut new_current_clauses = new_current_clauses.to_vec();
                new_current_clauses.reverse();
                let last_term = last.1.clone();

                let mut current: Option<(usize, Term<Name>)> = None;
                for index in 0..total_constr_length - 1 {
                    if current.is_none() {
                        current = new_current_clauses.pop();
                    }
                    if let Some(val) = current.clone() {
                        if val.0 == index {
                            let branch_term = val.1;

                            term = Term::Apply {
                                function: term.into(),
                                argument: Term::Delay(branch_term.into()).into(),
                            };
                            current = None;
                        } else {
                            term = Term::Apply {
                                function: term.into(),
                                argument: Term::Var(Name {
                                    text: "last_constr_then".to_string(),
                                    unique: 0.into(),
                                })
                                .into(),
                            }
                        }
                    } else {
                        term = Term::Apply {
                            function: term.into(),
                            argument: Term::Var(Name {
                                text: "last_constr_then".to_string(),
                                unique: 0.into(),
                            })
                            .into(),
                        }
                    }
                }
                if need_lam {
                    term = Term::Apply {
                        function: Term::Lambda {
                            parameter_name: Name {
                                text: "last_constr_then".to_string(),
                                unique: 0.into(),
                            },
                            body: Term::Apply {
                                function: term.into(),
                                argument: Term::Var(Name {
                                    text: "last_constr_then".to_string(),
                                    unique: 0.into(),
                                })
                                .into(),
                            }
                            .into(),
                        }
                        .into(),
                        argument: Term::Delay(last_term.into()).into(),
                    }
                } else {
                    term = Term::Apply {
                        function: term.into(),
                        argument: Term::Delay(last_term.into()).into(),
                    };
                }

                term
            }
            // if statements increase scope due to branching.
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
                    // TODO: for multi branch if statements we can insert function definitions between branches
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
                let mut current_var_name = String::new();
                let mut current_record = *record.clone();
                while !is_var {
                    match current_record.clone() {
                        TypedExpr::Var {
                            constructor, name, ..
                        } => match (
                            constructor.clone().variant.clone(),
                            (*constructor.tipo).clone(),
                        ) {
                            (ValueConstructorVariant::LocalVariable { .. }, Type::App { .. }) => {
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
            TypedExpr::ModuleSelect { constructor, .. } => match constructor {
                ModuleValueConstructor::Record { .. } => todo!(),
                ModuleValueConstructor::Fn { module, name, .. } => Term::Var(Name {
                    text: format!("{module}_{name}"),
                    unique: 0.into(),
                }),
                ModuleValueConstructor::Constant { .. } => todo!(),
            },
            TypedExpr::Todo { .. } => todo!(),
            TypedExpr::RecordUpdate { .. } => todo!(),
            TypedExpr::Negate { .. } => todo!(),
        }
    }

    fn maybe_insert_def(
        &mut self,
        current_term: Term<Name>,
        scope_level: ScopeLevels,
    ) -> Term<Name> {
        let mut term = current_term;

        // attempt to insert function definitions where needed
        for func_key in self.uplc_function_holder_lookup.clone().keys() {
            if scope_level.is_less_than(
                self.uplc_function_holder_lookup
                    .clone()
                    .get(func_key)
                    .unwrap(),
                false,
            ) {
                let func_def = self.functions.get(func_key).unwrap();

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
                            text: format!("{}_{}", func_key.module_name, func_key.function_name),
                            unique: 0.into(),
                        },
                        body: term.into(),
                    }
                    .into(),
                    argument: function_body.into(),
                };
                self.uplc_function_holder_lookup.shift_remove(func_key);
            }
        }

        for (key, scope) in self.uplc_data_constr_lookup.clone().iter() {
            if scope_level.is_less_than(scope, false) {
                let data_constrs = *self.data_types.get(key).unwrap();
                let mut constr_term = Term::Var(Name {
                    text: "last_constructor_result".to_string(),
                    unique: 0.into(),
                });

                let length = data_constrs.constructors.len();

                for index in (0..length - 1).rev() {
                    constr_term = Term::Apply {
                        function: Term::Apply {
                            function: Term::Apply {
                                function: Term::Force(
                                    Term::Builtin(DefaultFunction::IfThenElse).into(),
                                )
                                .into(),
                                argument: Term::Apply {
                                    function: Term::Apply {
                                        function: Term::Builtin(DefaultFunction::EqualsInteger)
                                            .into(),
                                        argument: Term::Constant(Constant::Integer(index as i128))
                                            .into(),
                                    }
                                    .into(),
                                    argument: Term::Var(Name {
                                        text: "constr_index".to_string(),
                                        unique: 0.into(),
                                    })
                                    .into(),
                                }
                                .into(),
                            }
                            .into(),
                            argument: Term::Var(Name {
                                text: format!("constr_{index}_result"),
                                unique: 0.into(),
                            })
                            .into(),
                        }
                        .into(),
                        argument: constr_term.into(),
                    }
                }

                constr_term = Term::Lambda {
                    parameter_name: Name {
                        text: "last_constructor_result".to_string(),
                        unique: 0.into(),
                    },
                    body: Term::Force(constr_term.into()).into(),
                };

                for index in (0..length - 1).rev() {
                    constr_term = Term::Lambda {
                        parameter_name: Name {
                            text: format!("constr_{index}_result"),
                            unique: 0.into(),
                        },
                        body: constr_term.into(),
                    }
                }
                let data_type_name = data_constrs.name.clone();

                constr_term = Term::Lambda {
                    parameter_name: Name {
                        text: "constr_data".to_string(),
                        unique: 0.into(),
                    },
                    body: Term::Apply {
                        function: Term::Lambda {
                            parameter_name: Name {
                                text: "constr_index".to_string(),
                                unique: 0.into(),
                            },
                            body: constr_term.into(),
                        }
                        .into(),
                        argument: Term::Apply {
                            function: Term::Builtin(DefaultFunction::FstPair).into(),
                            argument: Term::Apply {
                                function: Term::Builtin(DefaultFunction::UnConstrData).into(),
                                argument: Term::Var(Name {
                                    text: "constr_data".to_string(),
                                    unique: 0.into(),
                                })
                                .into(),
                            }
                            .into(),
                        }
                        .into(),
                    }
                    .into(),
                };
                let module = &key.module_name;

                term = Term::Apply {
                    function: Term::Lambda {
                        parameter_name: Name {
                            text: format!("choose_{module}_{data_type_name}_constr"),
                            unique: 0.into(),
                        },
                        body: term.into(),
                    }
                    .into(),
                    argument: constr_term.into(),
                };
                self.uplc_data_constr_lookup.shift_remove(key);
            }
        }

        // Pull out all uplc data holder fields and data usage, filter by Scope Level, Sort By Scope Depth, Then Apply
        let mut data_holder: Vec<ConstrConversionInfo> = self
            .uplc_data_usage_holder_lookup
            .clone()
            .into_iter()
            .filter(|record_scope| scope_level.is_less_than(&record_scope.1, false))
            .map(|(var_name, scope)| ConstrConversionInfo {
                local_var: var_name,
                field: None,
                scope,
                index: None,
                returning_type: String::new(),
            })
            .collect();

        data_holder.extend(
            self.uplc_data_holder_lookup
                .clone()
                .into_iter()
                .filter(|record_scope| scope_level.is_less_than(&record_scope.1.scope, false))
                .map(
                    |(
                        ConstrFieldKey {
                            local_var,
                            field_name,
                        },
                        ScopedExpr { scope, expr },
                    )| {
                        let index_type = match expr {
                            TypedExpr::RecordAccess { index, tipo, .. } => {
                                let tipo = &*tipo;

                                let name = match tipo {
                                    Type::App { name, .. } => name,
                                    Type::Fn { .. } => todo!(),
                                    Type::Var { .. } => todo!(),
                                };
                                (index, name.clone())
                            }
                            TypedExpr::Assignment { value, .. } => match *value {
                                TypedExpr::RecordAccess { index, tipo, .. } => {
                                    let tipo = &*tipo;

                                    let name = match tipo {
                                        Type::App { name, .. } => name,
                                        Type::Fn { .. } => todo!(),
                                        Type::Var { .. } => todo!(),
                                    };
                                    (index, name.clone())
                                }
                                _ => todo!(),
                            },
                            _ => todo!(),
                        };

                        ConstrConversionInfo {
                            local_var,
                            field: Some(field_name),
                            scope,
                            index: Some(index_type.0),
                            returning_type: index_type.1,
                        }
                    },
                )
                .collect::<Vec<ConstrConversionInfo>>(),
        );
        data_holder.sort_by(|item1, item2| {
            if item1.scope.is_less_than(&item2.scope, true) {
                Ordering::Less
            } else if item2.scope.is_less_than(&item1.scope, true) {
                Ordering::Greater
            } else if item1.index < item2.index {
                Ordering::Less
            } else if item2.index < item1.index {
                Ordering::Greater
            } else {
                Ordering::Equal
            }
        });

        for ConstrConversionInfo {
            local_var,
            field,
            index,
            returning_type,
            ..
        } in data_holder.into_iter().rev()
        {
            if let (Some(index), Some(field)) = (index, field) {
                let var_term = Term::Apply {
                    function: Term::Apply {
                        function: Term::Var(Name {
                            text: "constr_field_get_arg".to_string(),
                            unique: 0.into(),
                        })
                        .into(),
                        argument: Term::Var(Name {
                            text: format!("{local_var}_fields"),
                            unique: 0.into(),
                        })
                        .into(),
                    }
                    .into(),
                    argument: Term::Constant(Constant::Integer(index as i128)).into(),
                };

                let type_conversion = match returning_type.as_str() {
                    "ByteArray" => Term::Apply {
                        function: Term::Builtin(DefaultFunction::UnBData).into(),
                        argument: var_term.into(),
                    },
                    "Int" => Term::Apply {
                        function: Term::Builtin(DefaultFunction::UnIData).into(),
                        argument: var_term.into(),
                    },
                    _ => var_term,
                };

                term = Term::Apply {
                    function: Term::Lambda {
                        parameter_name: Name {
                            text: format!("{local_var}_field_{field}"),
                            unique: 0.into(),
                        },
                        body: term.into(),
                    }
                    .into(),
                    argument: type_conversion.into(),
                };
                self.uplc_data_holder_lookup.shift_remove(&ConstrFieldKey {
                    local_var,
                    field_name: field,
                });
            } else {
                term = Term::Apply {
                    function: Term::Lambda {
                        parameter_name: Name {
                            text: format!("{local_var}_fields"),
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
                            text: local_var.to_string(),
                            unique: 0.into(),
                        })
                        .into(),
                    }
                    .into(),
                };

                self.uplc_data_usage_holder_lookup.shift_remove(&local_var);
            }
        }

        term
    }

    fn add_arg_getter(&self, term: Term<Name>) -> Term<Name> {
        // Apply constr arg getter to top level.
        Term::Apply {
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
        }
    }
}
