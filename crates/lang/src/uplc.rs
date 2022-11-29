use std::{cmp::Ordering, collections::HashMap, rc::Rc, sync::Arc};

use indexmap::IndexMap;

use uplc::{
    ast::{Constant, Name, Program, Term, Type as UplcType, Unique},
    builtins::DefaultFunction,
    parser::interner::Interner,
    BigInt, PlutusData,
};

use crate::{
    ast::{AssignmentKind, BinOp, DataType, Function, Pattern, Span, TypedArg, TypedPattern},
    expr::TypedExpr,
    tipo::{self, ModuleValueConstructor, Type, ValueConstructor, ValueConstructorVariant},
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

#[derive(Clone, Eq, Debug, PartialEq, Hash)]
pub struct ConstrFieldKey {
    pub local_var: String,
    pub field_name: String,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct DataTypeKey {
    pub module_name: String,
    pub defined_type: String,
}

pub type ConstrUsageKey = String;

#[derive(Clone, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct FunctionAccessKey {
    pub module_name: String,
    pub function_name: String,
}

#[derive(Clone, Debug)]
pub struct ConstrConversionInfo {
    local_var: String,
    field: Option<String>,
    scope: ScopeLevels,
    index: Option<u64>,
    returning_type: String,
}

#[derive(Clone, Debug)]
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
    function_recurse_lookup: IndexMap<FunctionAccessKey, usize>,
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
            function_recurse_lookup: IndexMap::new(),
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

        println!("DATA HOLDER LOOKUP{:#?}", self.uplc_data_holder_lookup);

        println!(
            "DATA USAGE HOLDER {:#?}",
            self.uplc_data_usage_holder_lookup
        );

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
                    function: Term::Force(
                        Term::Force(Term::Builtin(DefaultFunction::SndPair).into()).into(),
                    )
                    .into(),
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

        println!("{}", program.to_pretty());

        interner.program(&mut program);

        program
    }

    pub(crate) fn recurse_scope_level(&mut self, body: &TypedExpr, scope_level: ScopeLevels) {
        match dbg!(body) {
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

                            self.uplc_function_holder_lookup.insert(
                                FunctionAccessKey {
                                    module_name: module,
                                    function_name: name,
                                },
                                scope_level.clone(),
                            );

                            self.recurse_scope_level(&func_def.body, scope_level);
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
                            Type::Fn { .. } | Type::Var { .. } | Type::Tuple { .. } => {}
                        };
                    }
                };
            }
            TypedExpr::Fn { .. } => todo!(),
            TypedExpr::List { elements, tail, .. } => {
                for element in elements {
                    self.recurse_scope_level(element, scope_level.clone());
                }
                if let Some(tail_element) = tail {
                    self.recurse_scope_level(tail_element, scope_level)
                }
            }
            TypedExpr::Call { fun, args, .. } => {
                self.recurse_scope_level(fun, scope_level.clone());

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
            TypedExpr::Assignment { value, pattern, .. } => {
                self.recurse_scope_level_pattern(pattern, value, scope_level, &[])
            }
            TypedExpr::Trace { .. } => todo!(),
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
                ModuleValueConstructor::Record { .. } => {}
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
            TypedExpr::Tuple { .. } => todo!(),
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
            Pattern::Int { .. }
            | Pattern::String { .. }
            | Pattern::Var { .. }
            | Pattern::List { .. }
            | Pattern::Discard { .. } => {
                self.recurse_scope_level(value, scope_level);
            }

            Pattern::VarUsage { .. } => todo!(),
            Pattern::Assign { .. } => todo!(),
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

                        let module = module.clone().unwrap_or_default();
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
                                Pattern::Discard { .. } => {}
                                _ => todo!(),
                            };
                        }
                    }
                    _ => todo!(),
                };
            }
            Pattern::Tuple { .. } => todo!(),
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

                    term =
                        self.maybe_insert_def(term, scope_level.scope_increment_sequence(i as i32));

                    self.uplc_function_holder
                        .push((String::new(), term.clone()));
                }

                self.maybe_insert_def(
                    self.uplc_function_holder.clone().pop().unwrap().1,
                    scope_level,
                )
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
                        ValueConstructorVariant::Record {
                            name: constr_name, ..
                        } => {
                            let data_type_key = match &*constructor.tipo {
                                Type::App { module, name, .. } => DataTypeKey {
                                    module_name: module.to_string(),
                                    defined_type: name.to_string(),
                                },
                                Type::Fn { .. } => todo!(),
                                Type::Var { .. } => todo!(),
                                Type::Tuple { .. } => todo!(),
                            };

                            if let Some(data_type) = self.data_types.get(&data_type_key) {
                                let (constr_index, _constr) = data_type
                                    .constructors
                                    .iter()
                                    .enumerate()
                                    .find(|(_, x)| x.name == *constr_name)
                                    .unwrap();

                                Term::Apply {
                                    function: Term::Builtin(DefaultFunction::ConstrData).into(),
                                    argument: Term::Apply {
                                        function: Term::Apply {
                                            function: Term::Builtin(DefaultFunction::MkPairData)
                                                .into(),
                                            argument: Term::Constant(Constant::Data(
                                                PlutusData::BigInt(BigInt::Int(
                                                    (constr_index as i128).try_into().unwrap(),
                                                )),
                                            ))
                                            .into(),
                                        }
                                        .into(),
                                        argument: Term::Constant(Constant::Data(
                                            PlutusData::Array(vec![]),
                                        ))
                                        .into(),
                                    }
                                    .into(),
                                }
                            } else {
                                todo!()
                            }
                        }
                    }
                }
            }
            TypedExpr::Fn { .. } => todo!(),
            TypedExpr::List {
                elements,
                tail,
                tipo,
                ..
            } => {
                let mut type_list = vec![];
                let mut is_final_type = false;
                // TODO use lifetimes instead of clone
                // Skip first type since we know we have a list
                let mut current_tipo = match (**tipo).clone() {
                    Type::App { args, .. } => (*args[0]).clone(),
                    Type::Fn { .. } => todo!(),
                    Type::Var { .. } => todo!(),
                    Type::Tuple { .. } => todo!(),
                };
                while !is_final_type {
                    match current_tipo.clone() {
                        Type::App { name, args, .. } => {
                            if args.is_empty() {
                                type_list.push(name);
                                is_final_type = true;
                            } else {
                                type_list.push(name);
                                current_tipo = (*args[0]).clone();
                            }
                        }
                        Type::Fn { .. } => todo!(),
                        Type::Var { tipo } => match (*tipo).borrow().clone() {
                            tipo::TypeVar::Unbound { .. } => todo!(),
                            tipo::TypeVar::Link { tipo } => {
                                current_tipo = (*tipo).clone();
                            }
                            tipo::TypeVar::Generic { .. } => todo!(),
                        },
                        Type::Tuple { .. } => todo!(),
                    };
                }

                let mut list_term = if let Some(tail_list) = tail {
                    // Get list of tail items
                    self.recurse_code_gen(tail_list, scope_level.clone())
                } else {
                    // Or get empty list of correct type
                    let mut current_type = vec![];
                    for type_name in type_list.into_iter().rev() {
                        match type_name.as_str() {
                            "ByteArray" => current_type.push(UplcType::ByteString),
                            "Int" => current_type.push(UplcType::Integer),
                            "String" => current_type.push(UplcType::String),
                            "Bool" => current_type.push(UplcType::Bool),
                            "List" => {
                                if let Some(prev_type) = current_type.pop() {
                                    current_type.push(UplcType::List(prev_type.into()));
                                } else {
                                    unreachable!()
                                }
                            }
                            "Pair" => todo!(),
                            _ => current_type.push(UplcType::Data),
                        };
                    }
                    Term::Constant(Constant::ProtoList(current_type.pop().unwrap(), vec![]))
                };

                // use mkCons to prepend all elements in reverse
                for element in elements.iter().rev() {
                    let element_term = self.recurse_code_gen(element, scope_level.clone());

                    list_term = Term::Apply {
                        function: Term::Apply {
                            function: Term::Force(Term::Builtin(DefaultFunction::MkCons).into())
                                .into(),
                            argument: element_term.into(),
                        }
                        .into(),
                        argument: list_term.into(),
                    }
                }
                list_term
            }
            TypedExpr::Call {
                fun, args, tipo, ..
            } => {
                match (&**tipo, &**fun) {
                    (
                        Type::App {
                            name: tipo_name, ..
                        },
                        TypedExpr::Var {
                            constructor: ValueConstructor { variant, .. },
                            ..
                        },
                    ) => match variant {
                        ValueConstructorVariant::LocalVariable { .. } => todo!(),
                        ValueConstructorVariant::ModuleConstant { .. } => todo!(),
                        ValueConstructorVariant::ModuleFn { name, module, .. } => {
                            let func_key = FunctionAccessKey {
                                module_name: module.to_string(),
                                function_name: name.to_string(),
                            };
                            if let Some(val) = self.function_recurse_lookup.get(&func_key) {
                                self.function_recurse_lookup.insert(func_key, *val + 1);
                            } else {
                                self.function_recurse_lookup.insert(func_key, 1);
                            }
                            let mut term =
                                self.recurse_code_gen(fun, scope_level.scope_increment(1));

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
                        ValueConstructorVariant::Record {
                            name: constr_name,
                            module,
                            ..
                        } => {
                            let mut term: Term<Name> =
                                Term::Constant(Constant::ProtoList(uplc::ast::Type::Data, vec![]));

                            if let Some(data_type) = self.data_types.get(&DataTypeKey {
                                module_name: module.to_string(),
                                defined_type: tipo_name.to_string(),
                            }) {
                                let (constr_index, constr) = data_type
                                    .constructors
                                    .iter()
                                    .enumerate()
                                    .find(|(_, x)| x.name == *constr_name)
                                    .unwrap();

                                // TODO: order arguments by data type field map
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

                                term = Term::Apply {
                                    function: Term::Builtin(DefaultFunction::ConstrData).into(),
                                    argument: Term::Apply {
                                        function: Term::Apply {
                                            function: Term::Builtin(DefaultFunction::MkPairData)
                                                .into(),
                                            argument: Term::Constant(Constant::Data(
                                                PlutusData::BigInt(BigInt::Int(
                                                    (constr_index as i128).try_into().unwrap(),
                                                )),
                                            ))
                                            .into(),
                                        }
                                        .into(),
                                        argument: Term::Apply {
                                            function: Term::Builtin(DefaultFunction::ListData)
                                                .into(),
                                            argument: term.into(),
                                        }
                                        .into(),
                                    }
                                    .into(),
                                };

                                term
                            } else {
                                let mut term =
                                    self.recurse_code_gen(fun, scope_level.scope_increment(1));

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
                    },

                    (
                        Type::App {
                            name: tipo_name, ..
                        },
                        TypedExpr::ModuleSelect {
                            constructor,
                            module_name: module,
                            ..
                        },
                    ) => {
                        match constructor {
                            ModuleValueConstructor::Constant { .. } => todo!(),
                            ModuleValueConstructor::Fn { name, module, .. } => {
                                let func_key = FunctionAccessKey {
                                    module_name: module.to_string(),
                                    function_name: name.to_string(),
                                };
                                if let Some(val) = self.function_recurse_lookup.get(&func_key) {
                                    self.function_recurse_lookup.insert(func_key, *val + 1);
                                } else {
                                    self.function_recurse_lookup.insert(func_key, 1);
                                }
                                let mut term =
                                    self.recurse_code_gen(fun, scope_level.scope_increment(1));

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
                            ModuleValueConstructor::Record {
                                name: constr_name, ..
                            } => {
                                let mut term: Term<Name> = Term::Constant(Constant::ProtoList(
                                    uplc::ast::Type::Data,
                                    vec![],
                                ));

                                if let Some(data_type) = self.data_types.get(&DataTypeKey {
                                    module_name: module.to_string(),
                                    defined_type: tipo_name.to_string(),
                                }) {
                                    let (constr_index, constr) = data_type
                                        .constructors
                                        .iter()
                                        .enumerate()
                                        .find(|(_, x)| x.name == *constr_name)
                                        .unwrap();

                                    // TODO: order arguments by data type field map
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

                                    term = Term::Apply {
                                        function: Term::Builtin(DefaultFunction::ConstrData).into(),
                                        argument: Term::Apply {
                                            function: Term::Apply {
                                                function: Term::Builtin(
                                                    DefaultFunction::MkPairData,
                                                )
                                                .into(),
                                                argument: Term::Constant(Constant::Data(
                                                    PlutusData::BigInt(BigInt::Int(
                                                        (constr_index as i128).try_into().unwrap(),
                                                    )),
                                                ))
                                                .into(),
                                            }
                                            .into(),
                                            argument: Term::Apply {
                                                function: Term::Builtin(DefaultFunction::ListData)
                                                    .into(),
                                                argument: term.into(),
                                            }
                                            .into(),
                                        }
                                        .into(),
                                    };

                                    term
                                } else {
                                    let mut term =
                                        self.recurse_code_gen(fun, scope_level.scope_increment(1));

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
                        }
                    }
                    _ => todo!(),
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
                        Type::Tuple { .. } => todo!(),
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
                        Type::Tuple { .. } => todo!(),
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
                Pattern::Tuple { .. } => todo!(),
            },
            TypedExpr::Trace { .. } => todo!(),
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

                        Some(key)
                    }
                    Pattern::List { .. } => None,
                    Pattern::Discard { .. } => None,
                    Pattern::Int { .. } => None,
                    rest => todo!("{rest:?}"),
                };

                if let Some(key) = key {
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
                                            }) =
                                                self.uplc_data_holder_lookup.get(&ConstrFieldKey {
                                                    local_var: current_var_name.to_string(),
                                                    field_name: label.clone(),
                                                })
                                            {
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
                                Pattern::Discard { .. } => (
                                    dt.constructors.len(),
                                    self.recurse_code_gen(
                                        &clause.then,
                                        scope_level.scope_increment_sequence(1),
                                    ),
                                ),
                                rest => todo!("{rest:?}"),
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
                } else {
                    let mut type_list = vec![];
                    let mut is_final_type = false;
                    // TODO use lifetimes instead of clone
                    // Skip first type since we know we have a list
                    let tipo = match subject {
                        TypedExpr::Var { constructor, .. } => &constructor.tipo,
                        rest => todo!("{rest:?}"),
                    };
                    let mut current_tipo = match (**tipo).clone() {
                        Type::App { args, .. } => (*args[0]).clone(),
                        Type::Fn { .. } => todo!(),
                        Type::Var { .. } => todo!(),
                        Type::Tuple { .. } => todo!(),
                    };

                    while !is_final_type {
                        match current_tipo.clone() {
                            Type::App { name, args, .. } => {
                                if args.is_empty() {
                                    type_list.push(name);
                                    is_final_type = true;
                                } else {
                                    type_list.push(name);
                                    current_tipo = (*args[0]).clone();
                                }
                            }
                            Type::Fn { .. } => todo!(),
                            Type::Var { tipo } => match (*tipo).borrow().clone() {
                                tipo::TypeVar::Unbound { .. } => todo!(),
                                tipo::TypeVar::Link { tipo } => {
                                    current_tipo = (*tipo).clone();
                                }
                                tipo::TypeVar::Generic { .. } => todo!(),
                            },
                            Type::Tuple { .. } => todo!(),
                        };
                    }

                    let mut new_current_clauses: Vec<(Option<usize>, bool, Term<Name>)> =
                        current_clauses
                            .iter()
                            .map(|clause| {
                                let pattern = &clause.pattern[0];
                                let mut current_term = self.recurse_code_gen(
                                    &clause.then,
                                    scope_level.scope_increment_sequence(1),
                                );
                                let triplet = match pattern {
                                    Pattern::List { elements, tail, .. } => {
                                        let element_names: Vec<String> = elements
                                            .clone()
                                            .iter()
                                            .map(|element| match element {
                                                Pattern::Var { name, .. } => name.to_string(),
                                                _ => todo!(),
                                            })
                                            .collect();

                                        let tail_name: Option<String> = if let Some(tail) = tail {
                                            match &**tail {
                                                Pattern::Var { name, .. } => Some(name.to_string()),
                                                _ => todo!(),
                                            }
                                        } else {
                                            None
                                        };

                                        for (index, var_name) in element_names.iter().enumerate() {
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
                                                        "{current_var_name}_item_{index}"
                                                    ),
                                                    unique: 0.into(),
                                                })
                                                .into(),
                                            };
                                        }

                                        if let Some(tail_name) = tail_name {
                                            current_term = Term::Apply {
                                                function: Term::Lambda {
                                                    parameter_name: Name {
                                                        text: tail_name,
                                                        unique: 0.into(),
                                                    },
                                                    body: current_term.into(),
                                                }
                                                .into(),
                                                argument: Term::Var(Name {
                                                    text: format!("{current_var_name}_rest"),
                                                    unique: 0.into(),
                                                })
                                                .into(),
                                            };
                                        }

                                        (Some(elements.len()), tail.is_some(), current_term)
                                    }
                                    Pattern::Discard { .. } => (None, false, current_term),
                                    _ => todo!(),
                                };
                                triplet
                            })
                            .collect();

                    new_current_clauses.sort_by(|item1, item2| {
                        if item1.0.is_none() && item2.0.is_some() {
                            Ordering::Greater
                        } else if item2.0.is_none() && item1.0.is_some() {
                            Ordering::Less
                        } else {
                            match item1.0.cmp(&item2.0) {
                                Ordering::Less => Ordering::Less,
                                Ordering::Equal => item1.1.cmp(&item2.1),
                                Ordering::Greater => Ordering::Greater,
                            }
                        }
                    });

                    let (last, new_current_clauses) = new_current_clauses.split_last().unwrap();

                    let new_current_clauses = new_current_clauses.to_vec();

                    let mut current_term: Term<Name> = last.2.clone();
                    let last_term = last.2.clone();

                    //if last clause had a tail then we need the lambda to expose rest
                    if last.1 && last.0.is_some() {
                        let last_index = last.0.unwrap() - 1;
                        current_term = Term::Apply {
                            function: Term::Lambda {
                                parameter_name: Name {
                                    text: format!("{current_var_name}_rest"),
                                    unique: 0.into(),
                                },
                                body: current_term.into(),
                            }
                            .into(),
                            argument: Term::Var(Name {
                                text: format!("{current_var_name}_tail_{last_index}"),
                                unique: 0.into(),
                            })
                            .into(),
                        };
                    }

                    for (index, (array_length, has_tail, then)) in
                        new_current_clauses.iter().enumerate().rev()
                    {
                        let prev_length: Option<usize> = if index == 0 {
                            None
                        } else {
                            new_current_clauses
                                .get(index - 1)
                                .and_then(|(index_opt, _, _)| *index_opt)
                        };

                        match (*array_length, prev_length) {
                            (Some(length), Some(prev_length)) => {
                                let check_length = if prev_length == length {
                                    length + 2
                                } else {
                                    length + 1
                                };
                                // 0, 3, 3, None
                                // Go index by index to create cases for each possible len
                                for expose_index in (prev_length + 1..check_length).rev() {
                                    let prev_exposed = expose_index - 1;
                                    let list_var_name =
                                        format!("{current_var_name}_tail_{prev_exposed}");

                                    if prev_length != length {
                                        // Just expose head list and tail list. Check for empty list happens above
                                        current_term = Term::Apply {
                                                function: Term::Lambda {
                                                    parameter_name: Name {
                                                        text: format!(
                                                            "{current_var_name}_item_{expose_index}"
                                                        ),
                                                        unique: 0.into(),
                                                    },
                                                    body: Term::Apply {
                                                        function: Term::Lambda {
                                                            parameter_name: Name {
                                                                text: format!(
                                                                    "{current_var_name}_tail_{expose_index}"
                                                                ),
                                                                unique: 0.into(),
                                                            },
                                                            body: current_term.into(),
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
                                                                text: list_var_name.to_string(),
                                                                unique: 0.into(),
                                                            })
                                                            .into(),
                                                        }
                                                        .into(),
                                                    }
                                                    .into(),
                                                }
                                                .into(),
                                                argument: Term::Apply {
                                                    function: Term::Force(
                                                        Term::Builtin(DefaultFunction::HeadList).into(),
                                                    )
                                                    .into(),
                                                    argument: Term::Var(Name {
                                                        text: list_var_name.to_string(),
                                                        unique: 0.into(),
                                                    })
                                                    .into(),
                                                }
                                                .into(),
                                            };
                                    }

                                    // For a given list length if we encounter a tail and we are checking a clause length = current index
                                    // then expose a var for tail and run clause then
                                    current_term = if *has_tail
                                        && (expose_index == check_length - 1
                                            || prev_length == length)
                                    {
                                        Term::Apply {
                                            function: Term::Lambda {
                                                parameter_name: Name {
                                                    text: format!("{current_var_name}_rest"),
                                                    unique: 0.into(),
                                                },
                                                body: then.clone().into(),
                                            }
                                            .into(),
                                            argument: Term::Var(Name {
                                                text: format!(
                                                    "{current_var_name}_tail_{prev_exposed}"
                                                ),
                                                unique: 0.into(),
                                            })
                                            .into(),
                                        }
                                    // we are checking a clause length = current index so check empty tail list and run clause then if tail list is empty
                                    } else if expose_index == check_length - 1 {
                                        Term::Force(
                                            Term::Apply {
                                                function: Term::Apply {
                                                    function: Term::Apply {
                                                        function: Term::Force(
                                                            Term::Force(
                                                                Term::Builtin(
                                                                    DefaultFunction::ChooseList,
                                                                )
                                                                .into(),
                                                            )
                                                            .into(),
                                                        )
                                                        .into(),
                                                        argument: Term::Var(Name {
                                                            text: format!(
                                                                "{current_var_name}_tail_{prev_exposed}"
                                                            ),
                                                            unique: 0.into(),
                                                        })
                                                        .into(),
                                                    }
                                                    .into(),
                                                    argument: Term::Delay(then.clone().into())
                                                        .into(),
                                                }
                                                .into(),
                                                argument: Term::Delay(current_term.into()).into(),
                                            }
                                            .into(),
                                        )
                                    // We are not checking for a list of this length, so fallback to last clause then if tail list is empty
                                    } else {
                                        Term::Force(
                                            Term::Apply {
                                                function: Term::Apply {
                                                    function: Term::Apply {
                                                        function: Term::Force(
                                                            Term::Force(
                                                                Term::Builtin(
                                                                    DefaultFunction::ChooseList,
                                                                )
                                                                .into(),
                                                            )
                                                            .into(),
                                                        )
                                                        .into(),
                                                        argument: Term::Var(Name {
                                                            text: format!(
                                                                "{current_var_name}_tail_{prev_exposed}"
                                                            ),
                                                            unique: 0.into(),
                                                        })
                                                        .into(),
                                                    }
                                                    .into(),
                                                    argument: Term::Delay(last_term.clone().into())
                                                        .into(),
                                                }
                                                .into(),
                                                argument: Term::Delay(current_term.into()).into(),
                                            }
                                            .into(),
                                        )
                                    };
                                }
                            }
                            (Some(length), None) => {
                                for expose_index in 0..length + 1 {
                                    let list_var_name = if expose_index == 0 {
                                        current_var_name.clone()
                                    } else {
                                        let prev_exposed = expose_index - 1;
                                        format!("{current_var_name}_tail_{prev_exposed}")
                                    };

                                    // Just expose head list and tail list. Check for empty list happens above
                                    current_term = Term::Apply {
                                        function: Term::Lambda {
                                            parameter_name: Name {
                                                text: format!(
                                                    "{current_var_name}_item_{expose_index}"
                                                ),
                                                unique: 0.into(),
                                            },
                                            body: Term::Apply {
                                                function: Term::Lambda {
                                                    parameter_name: Name {
                                                        text: format!(
                                                            "{current_var_name}_tail_{expose_index}"
                                                        ),
                                                        unique: 0.into(),
                                                    },
                                                    body: current_term.into(),
                                                }
                                                .into(),
                                                argument: Term::Apply {
                                                    function: Term::Force(
                                                        Term::Builtin(DefaultFunction::TailList)
                                                            .into(),
                                                    )
                                                    .into(),
                                                    argument: Term::Var(Name {
                                                        text: list_var_name.to_string(),
                                                        unique: 0.into(),
                                                    })
                                                    .into(),
                                                }
                                                .into(),
                                            }
                                            .into(),
                                        }
                                        .into(),
                                        argument: Term::Apply {
                                            function: Term::Force(
                                                Term::Builtin(DefaultFunction::HeadList).into(),
                                            )
                                            .into(),
                                            argument: Term::Var(Name {
                                                text: list_var_name.to_string(),
                                                unique: 0.into(),
                                            })
                                            .into(),
                                        }
                                        .into(),
                                    };

                                    // For a given list length if we encounter a tail and we are checking a clause length = current index
                                    // then expose a var for tail and run clause then
                                    current_term = if *has_tail && expose_index == length {
                                        Term::Apply {
                                            function: Term::Lambda {
                                                parameter_name: Name {
                                                    text: format!("{current_var_name}_rest"),
                                                    unique: 0.into(),
                                                },
                                                body: then.clone().into(),
                                            }
                                            .into(),
                                            argument: Term::Var(Name {
                                                text: list_var_name.clone(),
                                                unique: 0.into(),
                                            })
                                            .into(),
                                        }
                                    // we are checking a clause length = current index so check empty tail list and run clause then if tail list is empty
                                    } else if expose_index == length {
                                        Term::Force(
                                            Term::Apply {
                                                function: Term::Apply {
                                                    function: Term::Apply {
                                                        function: Term::Force(
                                                            Term::Force(
                                                                Term::Builtin(
                                                                    DefaultFunction::ChooseList,
                                                                )
                                                                .into(),
                                                            )
                                                            .into(),
                                                        )
                                                        .into(),
                                                        argument: Term::Var(Name {
                                                            text: list_var_name.clone(),
                                                            unique: 0.into(),
                                                        })
                                                        .into(),
                                                    }
                                                    .into(),
                                                    argument: Term::Delay(then.clone().into())
                                                        .into(),
                                                }
                                                .into(),
                                                argument: Term::Delay(current_term.into()).into(),
                                            }
                                            .into(),
                                        )
                                    // We are not checking for a list of this length, so fallback to last clause then if tail list is empty
                                    } else {
                                        Term::Force(
                                            Term::Apply {
                                                function: Term::Apply {
                                                    function: Term::Apply {
                                                        function: Term::Force(
                                                            Term::Force(
                                                                Term::Builtin(
                                                                    DefaultFunction::ChooseList,
                                                                )
                                                                .into(),
                                                            )
                                                            .into(),
                                                        )
                                                        .into(),
                                                        argument: Term::Var(Name {
                                                            text: list_var_name.clone(),
                                                            unique: 0.into(),
                                                        })
                                                        .into(),
                                                    }
                                                    .into(),
                                                    argument: Term::Delay(last_term.clone().into())
                                                        .into(),
                                                }
                                                .into(),
                                                argument: Term::Delay(current_term.into()).into(),
                                            }
                                            .into(),
                                        )
                                    };
                                }
                            }
                            (None, None) => todo!(),
                            (None, Some(_)) => todo!(),
                        }
                    }
                    current_term
                }
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
            TypedExpr::Tuple { .. } => todo!(),
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

                let current_called = *self.function_recurse_lookup.get(func_key).unwrap_or(&0);

                let mut function_body = self.recurse_code_gen(
                    &func_def.body,
                    scope_level.scope_increment_sequence(func_def.arguments.len() as i32),
                );

                let recurse_called = *self.function_recurse_lookup.get(func_key).unwrap_or(&0);

                if recurse_called > current_called {
                    for arg in func_def.arguments.iter().rev() {
                        function_body = Term::Lambda {
                            parameter_name: Name {
                                text: arg.arg_name.get_variable_name().unwrap_or("_").to_string(),
                                unique: Unique::new(0),
                            },
                            body: Rc::new(function_body),
                        }
                    }

                    function_body = Term::Lambda {
                        parameter_name: Name {
                            text: format!("{}_{}", func_key.module_name, func_key.function_name),
                            unique: 0.into(),
                        },
                        body: function_body.into(),
                    };

                    let mut recurse_term = Term::Apply {
                        function: Term::Var(Name {
                            text: "recurse".to_string(),
                            unique: 0.into(),
                        })
                        .into(),
                        argument: Term::Var(Name {
                            text: "recurse".into(),
                            unique: 0.into(),
                        })
                        .into(),
                    };

                    for arg in func_def.arguments.iter() {
                        recurse_term = Term::Apply {
                            function: recurse_term.into(),
                            argument: Term::Var(Name {
                                text: arg.arg_name.get_variable_name().unwrap_or("_").to_string(),
                                unique: 0.into(),
                            })
                            .into(),
                        };
                    }

                    function_body = Term::Apply {
                        function: Term::Lambda {
                            parameter_name: Name {
                                text: "recurse".into(),
                                unique: 0.into(),
                            },
                            body: recurse_term.into(),
                        }
                        .into(),
                        argument: function_body.into(),
                    }
                }

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
                            function: Term::Force(
                                Term::Force(Term::Builtin(DefaultFunction::FstPair).into()).into(),
                            )
                            .into(),
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
                                    Type::Tuple { .. } => todo!(),
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
                                        Type::Tuple { .. } => todo!(),
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

        println!("SCOPE LEVEL IS {scope_level:#?}");
        println!("DATA HOLDER COMBINED {:#?}", data_holder);

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

    // fn add_field_length_check(&self, term: Term<Name>) -> Term<Name> {
    // Term::Apply {
    //     function: Term::Lambda {
    //         parameter_name: Name {
    //             text: "field_length_check".to_string(),
    //             unique: 0.into(),
    //         },
    //         body: term.into(),
    //     }
    //     .into(),
    //     argument: Term::Lambda {
    //         parameter_name: Name {
    //             text: "expected_field_length".to_string(),
    //             unique: 0.into(),
    //         },
    //         body: Term::Lambda {
    //             parameter_name: Name {
    //                 text: "type_to_check".to_string(),
    //                 unique: 0.into(),
    //             },
    //             body: Term::Apply {
    //                 function: Term::Apply {
    //                     function: Term::Apply {
    //                         function: Term::Force(
    //                             Term::Builtin(DefaultFunction::IfThenElse).into(),
    //                         )
    //                         .into(),
    //                         argument: Term::Apply {
    //                             function: Term::Apply {
    //                                 function: Term::Apply {
    //                                     function: Term::Builtin(DefaultFunction::EqualsInteger),
    //                                     argument: Term::Apply { function: , argument: () },
    //                                 },
    //                                 argument: (),
    //                             },
    //                             argument: (),
    //                         },
    //                     }
    //                     .into(),
    //                     argument: (),
    //                 }
    //                 .into(),
    //                 argument: (),
    //             }
    //             .into(),
    //         }
    //         .into(),
    //     }
    //     .into(),
    // }
    // todo!()
    // }
}
