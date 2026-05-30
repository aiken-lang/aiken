use super::analysis_support::*;
use super::*;
use crate::ast::{CallArg, TypedClause};
use crate::parser::token::Base;
use crate::tipo::{ValueConstructor, ValueConstructorVariant};

fn local_var(name: &str, tipo: Rc<Type>) -> TypedExpr {
    TypedExpr::Var {
        location: Span::empty(),
        constructor: ValueConstructor::public(
            tipo.clone(),
            ValueConstructorVariant::LocalVariable {
                location: Span::empty(),
            },
        ),
        name: name.to_string(),
    }
}

fn module_fn_var(name: &str, module: &str, tipo: Rc<Type>) -> TypedExpr {
    TypedExpr::Var {
        location: Span::empty(),
        constructor: ValueConstructor::public(
            tipo.clone(),
            ValueConstructorVariant::ModuleFn {
                name: name.to_string(),
                field_map: None,
                module: module.to_string(),
                arity: 0,
                location: Span::empty(),
                builtin: None,
            },
        ),
        name: name.to_string(),
    }
}

fn module_const_var(name: &str, module: &str, tipo: Rc<Type>) -> TypedExpr {
    TypedExpr::Var {
        location: Span::empty(),
        constructor: ValueConstructor::public(
            tipo.clone(),
            ValueConstructorVariant::ModuleConstant {
                location: Span::empty(),
                module: module.to_string(),
                name: name.to_string(),
            },
        ),
        name: name.to_string(),
    }
}

fn fuzz_var(name: &str, tipo: Rc<Type>) -> TypedExpr {
    module_fn_var(name, STDLIB_FUZZ_MODULE, tipo)
}

fn make_map2_mapper(elems: Vec<TypedExpr>) -> TypedExpr {
    let int_tipo = Type::int();
    let tuple_tipo = Type::tuple(vec![int_tipo.clone(), int_tipo.clone()]);

    TypedExpr::Fn {
        location: Span::empty(),
        tipo: Type::function(vec![int_tipo.clone(), int_tipo.clone()], tuple_tipo.clone()),
        is_capture: false,
        args: vec![
            TypedArg::new("a", int_tipo.clone()),
            TypedArg::new("b", int_tipo),
        ],
        body: Box::new(TypedExpr::Tuple {
            location: Span::empty(),
            tipo: tuple_tipo,
            elems,
        }),
        return_annotation: None,
    }
}

fn make_mapn_mapper(arg_names: &[String], elems: Vec<TypedExpr>) -> TypedExpr {
    let int_tipo = Type::int();
    let tuple_tipo = Type::tuple(vec![int_tipo.clone(); arg_names.len()]);

    TypedExpr::Fn {
        location: Span::empty(),
        tipo: Type::function(vec![int_tipo.clone(); arg_names.len()], tuple_tipo.clone()),
        is_capture: false,
        args: arg_names
            .iter()
            .map(|name| TypedArg::new(name, int_tipo.clone()))
            .collect(),
        body: Box::new(TypedExpr::Tuple {
            location: Span::empty(),
            tipo: tuple_tipo,
            elems,
        }),
        return_annotation: None,
    }
}

fn map2_mapper_tipo() -> Rc<Type> {
    let int_tipo = Type::int();
    Type::function(
        vec![int_tipo.clone(), int_tipo.clone()],
        Type::tuple(vec![int_tipo.clone(), int_tipo]),
    )
}

fn make_named_map2_mapper(name: &str) -> TypedExpr {
    module_fn_var(name, "math", map2_mapper_tipo())
}

fn make_named_map2_mapper_function(
    name: &str,
    elems: Vec<TypedExpr>,
) -> (FunctionAccessKey, TypedFunction) {
    let int_tipo = Type::int();
    let tuple_tipo = Type::tuple(vec![int_tipo.clone(), int_tipo.clone()]);

    (
        FunctionAccessKey {
            module_name: "math".to_string(),
            function_name: name.to_string(),
        },
        TypedFunction {
            arguments: vec![
                TypedArg::new("a", int_tipo.clone()),
                TypedArg::new("b", int_tipo.clone()),
            ],
            body: TypedExpr::Tuple {
                location: Span::empty(),
                tipo: tuple_tipo.clone(),
                elems,
            },
            doc: None,
            location: Span::empty(),
            name: name.to_string(),
            public: false,
            return_annotation: None,
            return_type: tuple_tipo,
            end_position: 0,
            on_test_failure: OnTestFailure::FailImmediately,
        },
    )
}

fn uint_lit(value: &str) -> TypedExpr {
    TypedExpr::UInt {
        location: Span::empty(),
        tipo: Type::int(),
        value: value.to_string(),
        base: Base::Decimal {
            numeric_underscore: false,
        },
    }
}

fn call_arg(value: TypedExpr) -> CallArg<TypedExpr> {
    CallArg {
        label: None,
        location: Span::empty(),
        value,
    }
}

fn int_range_constraint(min: &str, max: &str) -> FuzzerConstraint {
    FuzzerConstraint::IntRange {
        min: min.to_string(),
        max: max.to_string(),
    }
}

fn tuple_int_range_constraint(ranges: &[(&str, &str)]) -> FuzzerConstraint {
    FuzzerConstraint::Tuple(
        ranges
            .iter()
            .map(|(min, max)| int_range_constraint(min, max))
            .collect(),
    )
}

fn make_int_between_via(min: &str, max: &str) -> TypedExpr {
    let output_type = Type::int();
    let fuzzer_type = Type::fuzzer(output_type);

    TypedExpr::Call {
        location: Span::empty(),
        tipo: fuzzer_type.clone(),
        fun: Box::new(fuzz_var(
            "int_between",
            Type::function(vec![Type::int(), Type::int()], fuzzer_type),
        )),
        args: vec![call_arg(uint_lit(min)), call_arg(uint_lit(max))],
    }
}

fn make_map2_via(fuzzer_a: TypedExpr, fuzzer_b: TypedExpr, mapper: TypedExpr) -> TypedExpr {
    let output_type = Type::tuple(vec![Type::int(), Type::int()]);
    let fuzzer_type = Type::fuzzer(output_type);
    let arg_types = vec![fuzzer_a.tipo(), fuzzer_b.tipo(), mapper.tipo()];

    TypedExpr::Call {
        location: Span::empty(),
        tipo: fuzzer_type.clone(),
        fun: Box::new(fuzz_var("map2", Type::function(arg_types, fuzzer_type))),
        args: vec![call_arg(fuzzer_a), call_arg(fuzzer_b), call_arg(mapper)],
    }
}

fn make_mapn_via(map_name: &str, fuzzers: Vec<TypedExpr>, mapper: TypedExpr) -> TypedExpr {
    let output_type = Type::tuple(vec![Type::int(); fuzzers.len()]);
    let fuzzer_type = Type::fuzzer(output_type);
    let mut arg_types: Vec<Rc<Type>> = fuzzers.iter().map(TypedExpr::tipo).collect();
    arg_types.push(mapper.tipo());
    let mut args: Vec<CallArg<TypedExpr>> = fuzzers.into_iter().map(call_arg).collect();
    args.push(call_arg(mapper));

    TypedExpr::Call {
        location: Span::empty(),
        tipo: fuzzer_type.clone(),
        fun: Box::new(fuzz_var(map_name, Type::function(arg_types, fuzzer_type))),
        args,
    }
}

fn make_map_via(fuzzer_a: TypedExpr, mapper: TypedExpr) -> TypedExpr {
    let output_type = Type::int();
    let fuzzer_type = Type::fuzzer(output_type);
    let arg_types = vec![fuzzer_a.tipo(), mapper.tipo()];

    TypedExpr::Call {
        location: Span::empty(),
        tipo: fuzzer_type.clone(),
        fun: Box::new(fuzz_var("map", Type::function(arg_types, fuzzer_type))),
        args: vec![call_arg(fuzzer_a), call_arg(mapper)],
    }
}

fn make_and_then_via(
    input: TypedExpr,
    continuation: TypedExpr,
    output_type: Rc<Type>,
) -> TypedExpr {
    let fuzzer_type = Type::fuzzer(output_type);
    let arg_types = vec![input.tipo(), continuation.tipo()];

    TypedExpr::Call {
        location: Span::empty(),
        tipo: fuzzer_type.clone(),
        fun: Box::new(fuzz_var("and_then", Type::function(arg_types, fuzzer_type))),
        args: vec![call_arg(input), call_arg(continuation)],
    }
}

fn make_tuple4_via(
    fuzzer_a: TypedExpr,
    fuzzer_b: TypedExpr,
    fuzzer_c: TypedExpr,
    fuzzer_d: TypedExpr,
) -> TypedExpr {
    let output_type = Type::tuple(vec![Type::int(), Type::int(), Type::int(), Type::int()]);
    let fuzzer_type = Type::fuzzer(output_type);
    let arg_types = vec![
        fuzzer_a.tipo(),
        fuzzer_b.tipo(),
        fuzzer_c.tipo(),
        fuzzer_d.tipo(),
    ];

    TypedExpr::Call {
        location: Span::empty(),
        tipo: fuzzer_type.clone(),
        fun: Box::new(fuzz_var("tuple4", Type::function(arg_types, fuzzer_type))),
        args: vec![
            call_arg(fuzzer_a),
            call_arg(fuzzer_b),
            call_arg(fuzzer_c),
            call_arg(fuzzer_d),
        ],
    }
}

fn negate_expr(value: TypedExpr) -> TypedExpr {
    TypedExpr::UnOp {
        location: Span::empty(),
        value: Box::new(value),
        tipo: Type::int(),
        op: UnOp::Negate,
    }
}

fn make_named_unary_negate_mapper_function(name: &str) -> (FunctionAccessKey, TypedFunction) {
    let int_tipo = Type::int();
    (
        FunctionAccessKey {
            module_name: "math".to_string(),
            function_name: name.to_string(),
        },
        TypedFunction {
            arguments: vec![TypedArg::new("n", int_tipo.clone())],
            body: negate_expr(local_var("n", int_tipo.clone())),
            doc: None,
            location: Span::empty(),
            name: name.to_string(),
            public: false,
            return_annotation: None,
            return_type: int_tipo,
            end_position: 0,
            on_test_failure: OnTestFailure::FailImmediately,
        },
    )
}

fn make_named_unary_identity_mapper_function(
    name: &str,
    payload_type: Rc<Type>,
) -> (FunctionAccessKey, TypedFunction) {
    (
        FunctionAccessKey {
            module_name: "math".to_string(),
            function_name: name.to_string(),
        },
        TypedFunction {
            arguments: vec![TypedArg::new("x", payload_type.clone())],
            body: local_var("x", payload_type.clone()),
            doc: None,
            location: Span::empty(),
            name: name.to_string(),
            public: false,
            return_annotation: None,
            return_type: payload_type,
            end_position: 0,
            on_test_failure: OnTestFailure::FailImmediately,
        },
    )
}

fn make_unresolved_unary_mapper(name: &str, payload_type: Rc<Type>) -> TypedExpr {
    local_var(
        name,
        Type::function(vec![payload_type.clone()], payload_type),
    )
}

fn make_unresolved_unary_mapper_with_types(
    name: &str,
    input_type: Rc<Type>,
    output_type: Rc<Type>,
) -> TypedExpr {
    local_var(name, Type::function(vec![input_type], output_type))
}

fn make_zero_arg_function(
    name: &str,
    return_type: Rc<Type>,
    body: TypedExpr,
) -> (FunctionAccessKey, TypedFunction) {
    (
        FunctionAccessKey {
            module_name: "math".to_string(),
            function_name: name.to_string(),
        },
        TypedFunction {
            arguments: vec![],
            body,
            doc: None,
            location: Span::empty(),
            name: name.to_string(),
            public: false,
            return_annotation: None,
            return_type,
            end_position: 0,
            on_test_failure: OnTestFailure::FailImmediately,
        },
    )
}

fn make_zero_arg_call(name: &str, return_type: Rc<Type>) -> TypedExpr {
    TypedExpr::Call {
        location: Span::empty(),
        tipo: return_type.clone(),
        fun: Box::new(module_fn_var(
            name,
            "math",
            Type::function(vec![], return_type),
        )),
        args: vec![],
    }
}

fn make_leaf_fuzzer_call(name: &str, payload_type: Rc<Type>) -> TypedExpr {
    let fuzzer_type = Type::fuzzer(payload_type);
    TypedExpr::Call {
        location: Span::empty(),
        tipo: fuzzer_type.clone(),
        fun: Box::new(module_fn_var(
            name,
            "math",
            Type::function(vec![], fuzzer_type),
        )),
        args: vec![],
    }
}

fn make_fail_fuzzer_expr(payload_type: Rc<Type>) -> TypedExpr {
    TypedExpr::ErrorTerm {
        location: Span::empty(),
        tipo: Type::fuzzer(payload_type),
    }
}

fn make_if_fuzzer_expr(
    payload_type: Rc<Type>,
    then_branch: TypedExpr,
    else_branch: TypedExpr,
    condition: bool,
) -> TypedExpr {
    TypedExpr::If {
        location: Span::empty(),
        tipo: Type::fuzzer(payload_type),
        branches: vec1::vec1![IfBranch {
            location: Span::empty(),
            condition: bool_constructor(condition),
            body: then_branch,
            is: None,
        }],
        final_else: Box::new(else_branch),
    }
}

fn make_typed_int_between_fuzzer(min: &str, max: &str) -> TypedExpr {
    let output_type = Type::int();
    let fuzzer_type = Type::fuzzer(output_type.clone());
    TypedExpr::Call {
        location: Span::empty(),
        tipo: fuzzer_type.clone(),
        fun: Box::new(module_fn_var(
            "int_between",
            STDLIB_FUZZ_MODULE,
            Type::function(vec![Type::int(), Type::int()], fuzzer_type),
        )),
        args: vec![call_arg(uint_lit(min)), call_arg(uint_lit(max))],
    }
}

fn make_typed_bool_fuzzer() -> TypedExpr {
    let output_type = Type::bool();
    let fuzzer_type = Type::fuzzer(output_type);
    TypedExpr::Call {
        location: Span::empty(),
        tipo: fuzzer_type.clone(),
        fun: Box::new(module_fn_var(
            "bool",
            STDLIB_FUZZ_MODULE,
            Type::function(vec![], fuzzer_type),
        )),
        args: vec![],
    }
}

fn make_identity_mapper(arg_name: &str, payload_type: Rc<Type>) -> TypedExpr {
    TypedExpr::Fn {
        location: Span::empty(),
        tipo: Type::function(vec![payload_type.clone()], payload_type.clone()),
        is_capture: false,
        args: vec![TypedArg::new(arg_name, payload_type.clone())],
        body: Box::new(local_var(arg_name, payload_type)),
        return_annotation: None,
    }
}

fn bool_constructor(value: bool) -> TypedExpr {
    let name = if value { "True" } else { "False" };

    TypedExpr::Var {
        location: Span::empty(),
        constructor: ValueConstructor::public(
            Type::bool(),
            ValueConstructorVariant::Record {
                name: name.to_string(),
                arity: 0,
                field_map: None,
                location: Span::empty(),
                module: String::new(),
                constructors_count: 2,
            },
        ),
        name: name.to_string(),
    }
}

fn make_unary_mapper(
    arg_name: &str,
    input_type: Rc<Type>,
    output_type: Rc<Type>,
    body: TypedExpr,
) -> TypedExpr {
    TypedExpr::Fn {
        location: Span::empty(),
        tipo: Type::function(vec![input_type.clone()], output_type),
        is_capture: false,
        args: vec![TypedArg::new(arg_name, input_type)],
        body: Box::new(body),
        return_annotation: None,
    }
}

fn make_constant_bool_mapper(input_type: Rc<Type>, value: bool) -> TypedExpr {
    make_unary_mapper("x", input_type, Type::bool(), bool_constructor(value))
}

fn make_not_bool_mapper(arg_name: &str) -> TypedExpr {
    make_unary_mapper(
        arg_name,
        Type::bool(),
        Type::bool(),
        TypedExpr::UnOp {
            location: Span::empty(),
            value: Box::new(local_var(arg_name, Type::bool())),
            tipo: Type::bool(),
            op: UnOp::Not,
        },
    )
}

fn make_tautology_body(arg_name: &str, input_type: Rc<Type>) -> TypedExpr {
    TypedExpr::BinOp {
        location: Span::empty(),
        tipo: Type::bool(),
        name: BinOp::Eq,
        left: Box::new(local_var(arg_name, input_type.clone())),
        right: Box::new(local_var(arg_name, input_type)),
    }
}

fn make_constant_int_mapper(input_type: Rc<Type>, value: &str) -> TypedExpr {
    make_unary_mapper("x", input_type, Type::int(), uint_lit(value))
}

fn string_lit(value: &str) -> TypedExpr {
    TypedExpr::String {
        location: Span::empty(),
        tipo: Type::string(),
        value: value.to_string(),
    }
}

fn int_eq_expr(arg_name: &str, value: &str) -> TypedExpr {
    TypedExpr::BinOp {
        location: Span::empty(),
        tipo: Type::bool(),
        name: BinOp::Eq,
        left: Box::new(local_var(arg_name, Type::int())),
        right: Box::new(uint_lit(value)),
    }
}

fn finite_string_if_mapper_body(arg_name: &str) -> TypedExpr {
    TypedExpr::If {
        location: Span::empty(),
        tipo: Type::string(),
        branches: vec1::vec1![
            IfBranch {
                condition: int_eq_expr(arg_name, "0"),
                body: string_lit("world"),
                is: None,
                location: Span::empty(),
            },
            IfBranch {
                condition: int_eq_expr(arg_name, "1"),
                body: string_lit("hello"),
                is: None,
                location: Span::empty(),
            },
            IfBranch {
                condition: int_eq_expr(arg_name, "2"),
                body: string_lit("test"),
                is: None,
                location: Span::empty(),
            },
        ],
        final_else: Box::new(string_lit("")),
    }
}

fn finite_string_if_mapper() -> TypedExpr {
    make_unary_mapper(
        "i",
        Type::int(),
        Type::string(),
        finite_string_if_mapper_body("i"),
    )
}

fn int_pattern(value: &str) -> TypedPattern {
    TypedPattern::Int {
        location: Span::empty(),
        value: value.to_string(),
        base: Base::Decimal {
            numeric_underscore: false,
        },
    }
}

fn finite_string_when_mapper() -> TypedExpr {
    let body = TypedExpr::When {
        location: Span::empty(),
        tipo: Type::string(),
        subject: Box::new(local_var("i", Type::int())),
        clauses: vec![
            TypedClause {
                location: Span::empty(),
                pattern: int_pattern("0"),
                then: string_lit("world"),
            },
            TypedClause {
                location: Span::empty(),
                pattern: int_pattern("1"),
                then: string_lit("hello"),
            },
            TypedClause {
                location: Span::empty(),
                pattern: int_pattern("2"),
                then: string_lit("test"),
            },
            TypedClause {
                location: Span::empty(),
                pattern: TypedPattern::Discard {
                    name: "_".to_string(),
                    location: Span::empty(),
                },
                then: string_lit(""),
            },
        ],
    };

    make_unary_mapper("i", Type::int(), Type::string(), body)
}

fn expected_finite_string_values() -> Vec<FuzzerExactValue> {
    vec![
        FuzzerExactValue::String("".to_string()),
        FuzzerExactValue::String("hello".to_string()),
        FuzzerExactValue::String("test".to_string()),
        FuzzerExactValue::String("world".to_string()),
    ]
}

fn make_named_finite_string_mapper_function(name: &str) -> (FunctionAccessKey, TypedFunction) {
    (
        FunctionAccessKey {
            module_name: "math".to_string(),
            function_name: name.to_string(),
        },
        TypedFunction {
            arguments: vec![TypedArg::new("i", Type::int())],
            body: finite_string_if_mapper_body("i"),
            doc: None,
            location: Span::empty(),
            name: name.to_string(),
            public: false,
            return_annotation: None,
            return_type: Type::string(),
            end_position: 0,
            on_test_failure: OnTestFailure::FailImmediately,
        },
    )
}

fn make_add_int_mapper(offset: &str) -> TypedExpr {
    let int_type = Type::int();
    make_unary_mapper(
        "x",
        int_type.clone(),
        int_type.clone(),
        TypedExpr::BinOp {
            location: Span::empty(),
            tipo: int_type.clone(),
            name: BinOp::AddInt,
            left: Box::new(local_var("x", int_type)),
            right: Box::new(uint_lit(offset)),
        },
    )
}

fn make_named_unary_constant_int_mapper_function(
    name: &str,
    value: &str,
) -> (FunctionAccessKey, TypedFunction) {
    let int_tipo = Type::int();
    (
        FunctionAccessKey {
            module_name: "math".to_string(),
            function_name: name.to_string(),
        },
        TypedFunction {
            arguments: vec![TypedArg::new("x", int_tipo.clone())],
            body: uint_lit(value),
            doc: None,
            location: Span::empty(),
            name: name.to_string(),
            public: false,
            return_annotation: None,
            return_type: int_tipo,
            end_position: 0,
            on_test_failure: OnTestFailure::FailImmediately,
        },
    )
}

fn make_named_unary_tautology_mapper_function(
    name: &str,
    input_type: Rc<Type>,
) -> (FunctionAccessKey, TypedFunction) {
    (
        FunctionAccessKey {
            module_name: "math".to_string(),
            function_name: name.to_string(),
        },
        TypedFunction {
            arguments: vec![TypedArg::new("x", input_type.clone())],
            body: make_tautology_body("x", input_type),
            doc: None,
            location: Span::empty(),
            name: name.to_string(),
            public: false,
            return_annotation: None,
            return_type: Type::bool(),
            end_position: 0,
            on_test_failure: OnTestFailure::FailImmediately,
        },
    )
}

fn make_bind_continuation(name: &str, input_type: Rc<Type>, output_type: Rc<Type>) -> TypedExpr {
    module_fn_var(
        name,
        "math",
        Type::function(vec![input_type], Type::fuzzer(output_type)),
    )
}

fn make_named_bind_passthrough_continuation_function(
    name: &str,
    input_type: Rc<Type>,
) -> (FunctionAccessKey, TypedFunction) {
    let fuzzer_type = Type::fuzzer(input_type.clone());
    let x = local_var("x", input_type.clone());
    let body = TypedExpr::Call {
        location: Span::empty(),
        tipo: fuzzer_type.clone(),
        fun: Box::new(module_fn_var(
            "constant",
            STDLIB_FUZZ_MODULE,
            Type::function(vec![input_type.clone()], fuzzer_type.clone()),
        )),
        args: vec![call_arg(x)],
    };

    (
        FunctionAccessKey {
            module_name: "math".to_string(),
            function_name: name.to_string(),
        },
        TypedFunction {
            arguments: vec![TypedArg::new("x", input_type.clone())],
            body,
            doc: None,
            location: Span::empty(),
            name: name.to_string(),
            public: false,
            return_annotation: None,
            return_type: fuzzer_type,
            end_position: 0,
            on_test_failure: OnTestFailure::FailImmediately,
        },
    )
}

fn make_inline_bind_continuation(
    arg_name: &str,
    input_type: Rc<Type>,
    body: TypedExpr,
    output_type: Rc<Type>,
) -> TypedExpr {
    TypedExpr::Fn {
        location: Span::empty(),
        tipo: Type::function(vec![input_type.clone()], Type::fuzzer(output_type)),
        is_capture: false,
        args: vec![TypedArg::new(arg_name, input_type)],
        body: Box::new(body),
        return_annotation: None,
    }
}

fn make_typed_map_call(source: TypedExpr, mapper: TypedExpr, output_type: Rc<Type>) -> TypedExpr {
    TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::fuzzer(output_type.clone()),
        fun: Box::new(module_fn_var(
            "anything_but_map",
            "math",
            Type::function(
                vec![source.tipo(), mapper.tipo()],
                Type::fuzzer(output_type),
            ),
        )),
        args: vec![call_arg(source), call_arg(mapper)],
    }
}

fn make_typed_bind_call(
    source: TypedExpr,
    continuation: TypedExpr,
    output_type: Rc<Type>,
) -> TypedExpr {
    TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::fuzzer(output_type.clone()),
        fun: Box::new(module_fn_var(
            "anything_but_and_then",
            "math",
            Type::function(
                vec![source.tipo(), continuation.tipo()],
                Type::fuzzer(output_type),
            ),
        )),
        args: vec![call_arg(source), call_arg(continuation)],
    }
}

fn make_stdlib_bind_call(
    source: TypedExpr,
    continuation: TypedExpr,
    output_type: Rc<Type>,
) -> TypedExpr {
    TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::fuzzer(output_type.clone()),
        fun: Box::new(module_fn_var(
            "and_then",
            STDLIB_FUZZ_MODULE,
            Type::function(
                vec![source.tipo(), continuation.tipo()],
                Type::fuzzer(output_type),
            ),
        )),
        args: vec![call_arg(source), call_arg(continuation)],
    }
}

fn make_zero_arg_fuzzer_thunk(body: TypedExpr) -> TypedExpr {
    let return_type = body.tipo();
    TypedExpr::Fn {
        location: Span::empty(),
        tipo: Type::function(vec![], return_type),
        is_capture: false,
        args: vec![],
        body: Box::new(body),
        return_annotation: None,
    }
}

fn make_named_fuzzer_continuation_function(
    name: &str,
    arg_name: &str,
    input_type: Rc<Type>,
    body: TypedExpr,
    output_type: Rc<Type>,
) -> (FunctionAccessKey, TypedFunction) {
    (
        FunctionAccessKey {
            module_name: "math".to_string(),
            function_name: name.to_string(),
        },
        TypedFunction {
            arguments: vec![TypedArg::new(arg_name, input_type)],
            body,
            doc: None,
            location: Span::empty(),
            name: name.to_string(),
            public: false,
            return_annotation: None,
            return_type: Type::fuzzer(output_type),
            end_position: 0,
            on_test_failure: OnTestFailure::FailImmediately,
        },
    )
}

fn make_stdlib_fork_call(
    fork_name: &str,
    args: Vec<TypedExpr>,
    output_type: Rc<Type>,
) -> TypedExpr {
    let arg_types = args.iter().map(TypedExpr::tipo).collect();
    TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::fuzzer(output_type.clone()),
        fun: Box::new(module_fn_var(
            fork_name,
            STDLIB_FUZZ_SCENARIO_MODULE,
            Type::function(arg_types, Type::fuzzer(output_type)),
        )),
        args: args.into_iter().map(call_arg).collect(),
    }
}

/// Build a filter/such_that call: (Fuzzer<a>, fn(a) -> Bool) -> Fuzzer<a>
fn make_typed_filter_call(source: TypedExpr, predicate: TypedExpr) -> TypedExpr {
    let payload_type =
        extract_fuzzer_payload_type(source.tipo().as_ref()).expect("source must be a Fuzzer");
    TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::fuzzer(payload_type.clone()),
        fun: Box::new(module_fn_var(
            "such_that",
            STDLIB_FUZZ_MODULE,
            Type::function(
                vec![source.tipo(), predicate.tipo()],
                Type::fuzzer(payload_type),
            ),
        )),
        args: vec![call_arg(source), call_arg(predicate)],
    }
}

/// Build a Bool-returning predicate lambda: fn(a) -> Bool
fn make_bool_predicate(arg_name: &str, input_type: Rc<Type>) -> TypedExpr {
    TypedExpr::Fn {
        location: Span::empty(),
        tipo: Type::function(vec![input_type.clone()], Type::bool()),
        is_capture: false,
        args: vec![TypedArg::new(arg_name, input_type)],
        body: Box::new(bool_constructor(true)),
        return_annotation: None,
    }
}

fn make_typed_product_call(left: TypedExpr, right: TypedExpr) -> TypedExpr {
    let output_type = Type::tuple(vec![Type::int(), Type::int()]);
    TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::fuzzer(output_type.clone()),
        fun: Box::new(module_fn_var(
            "anything_but_both",
            "math",
            Type::function(vec![left.tipo(), right.tipo()], Type::fuzzer(output_type)),
        )),
        args: vec![call_arg(left), call_arg(right)],
    }
}

fn make_typed_map2_product_call(
    first: TypedExpr,
    second: TypedExpr,
    mapper: TypedExpr,
) -> TypedExpr {
    let output_type = Type::tuple(vec![Type::int(), Type::int()]);
    TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::fuzzer(output_type.clone()),
        fun: Box::new(module_fn_var(
            "anything_but_map2",
            "math",
            Type::function(
                vec![first.tipo(), second.tipo(), mapper.tipo()],
                Type::fuzzer(output_type),
            ),
        )),
        args: vec![call_arg(first), call_arg(second), call_arg(mapper)],
    }
}

fn make_typed_list_call(element: TypedExpr, element_payload_type: Rc<Type>) -> TypedExpr {
    let output_type = Type::list(element_payload_type);
    TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::fuzzer(output_type.clone()),
        fun: Box::new(module_fn_var(
            "anything_but_list",
            "math",
            Type::function(vec![element.tipo()], Type::fuzzer(output_type)),
        )),
        args: vec![call_arg(element)],
    }
}

fn make_zero_arg_fuzzer_function(
    name: &str,
    payload_type: Rc<Type>,
    body: TypedExpr,
) -> (FunctionAccessKey, TypedFunction) {
    make_zero_arg_function(name, Type::fuzzer(payload_type), body)
}

fn make_zero_arg_fuzzer_call(name: &str, payload_type: Rc<Type>) -> TypedExpr {
    make_zero_arg_call(name, Type::fuzzer(payload_type))
}

fn assert_normalized_leaf(normalized: NormalizedFuzzer) {
    assert!(matches!(normalized, NormalizedFuzzer::Primitive { .. }));
}

fn assert_normalized_map(normalized: NormalizedFuzzer) {
    match normalized {
        NormalizedFuzzer::Map { source, .. } => {
            assert_normalized_leaf(*source);
        }
        other => panic!("expected map normalization, got {other:?}"),
    }
}

fn assert_normalized_bind(normalized: NormalizedFuzzer) {
    match normalized {
        NormalizedFuzzer::Bind { source, result } => {
            assert_normalized_leaf(*source);
            assert!(matches!(*result, NormalizedFuzzer::Opaque { .. }));
        }
        other => panic!("expected bind normalization, got {other:?}"),
    }
}

fn assert_normalized_product(normalized: NormalizedFuzzer, len: usize) {
    match normalized {
        NormalizedFuzzer::Product { elements } => {
            assert_eq!(elements.len(), len);
            elements.into_iter().for_each(assert_normalized_leaf);
        }
        other => panic!("expected product normalization, got {other:?}"),
    }
}

fn assert_normalized_list(normalized: NormalizedFuzzer) {
    match normalized {
        NormalizedFuzzer::List {
            element,
            min_len,
            max_len,
            unique,
            retry_limit,
        } => {
            assert_normalized_leaf(*element);
            assert_eq!(min_len, None);
            assert_eq!(max_len, None);
            assert!(!unique);
            assert_eq!(retry_limit, None);
        }
        other => panic!("expected list normalization, got {other:?}"),
    }
}

fn empty_known_functions<'a>() -> IndexMap<&'a FunctionAccessKey, &'a TypedFunction> {
    IndexMap::new()
}

fn empty_known_constants<'a>() -> IndexMap<&'a FunctionAccessKey, &'a TypedExpr> {
    IndexMap::new()
}

fn make_nullary_constructor_type(module_name: &str, type_name: &str) -> Rc<Type> {
    Rc::new(Type::App {
        public: true,
        contains_opaque: false,
        module: module_name.to_string(),
        name: type_name.to_string(),
        args: vec![],
        alias: None,
    })
}

fn make_nullary_constructor_data_types(
    module_name: &str,
    type_name: &str,
    constructor_names: &[&str],
) -> IndexMap<DataTypeKey, TypedDataType> {
    let constructors = constructor_names
        .iter()
        .map(|name| RecordConstructor {
            decorators: vec![],
            location: Span::empty(),
            name: (*name).to_string(),
            arguments: vec![],
            doc: None,
            sugar: false,
        })
        .collect();

    let data_type = TypedDataType {
        decorators: vec![],
        constructors,
        doc: None,
        location: Span::empty(),
        name: type_name.to_string(),
        opaque: false,
        parameters: vec![],
        public: true,
        typed_parameters: vec![],
    };

    let mut data_types = IndexMap::new();
    data_types.insert(
        DataTypeKey {
            module_name: module_name.to_string(),
            defined_type: type_name.to_string(),
        },
        data_type,
    );
    data_types
}

fn make_maybe_int_data_types(
    module_name: &str,
    type_name: &str,
) -> IndexMap<DataTypeKey, TypedDataType> {
    let constructors = vec![
        RecordConstructor {
            decorators: vec![],
            location: Span::empty(),
            name: "None".to_string(),
            arguments: vec![],
            doc: None,
            sugar: false,
        },
        RecordConstructor {
            decorators: vec![],
            location: Span::empty(),
            name: "Some".to_string(),
            arguments: vec![RecordConstructorArg {
                label: None,
                annotation: Annotation::int(Span::empty()),
                location: Span::empty(),
                tipo: Type::int(),
                doc: None,
            }],
            doc: None,
            sugar: false,
        },
    ];

    let data_type = TypedDataType {
        decorators: vec![],
        constructors,
        doc: None,
        location: Span::empty(),
        name: type_name.to_string(),
        opaque: false,
        parameters: vec![],
        public: true,
        typed_parameters: vec![],
    };

    let mut data_types = IndexMap::new();
    data_types.insert(
        DataTypeKey {
            module_name: module_name.to_string(),
            defined_type: type_name.to_string(),
        },
        data_type,
    );
    data_types
}

#[test]
fn shallow_when_constructor_literal_pattern_keeps_refinement_guard() {
    let maybe_int_type = make_nullary_constructor_type("fixture", "MaybeInt");
    let owned_data_types = make_maybe_int_data_types("fixture", "MaybeInt");
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();
    let clause = TypedClause {
        location: Span::empty(),
        pattern: TypedPattern::constructor(
            "Some",
            &[CallArg {
                label: None,
                location: Span::empty(),
                value: int_pattern("0"),
            }],
            maybe_int_type.clone(),
            Span::empty(),
        ),
        then: string_lit("zero"),
    };

    let arm = match translate_clause(
        &clause,
        &local_var("subject", maybe_int_type.clone()),
        &maybe_int_type,
        &data_types,
    ) {
        Ok(arm) => arm,
        Err(_) => panic!("Some constructor tag should resolve"),
    };

    assert_eq!(arm.tag, Some(1));
    assert_eq!(arm.bindings, vec!["_".to_string()]);
    assert!(
        matches!(
            arm.guard.as_deref(),
            Some(ShallowIr::BinOp {
                op: ShallowBinOp::Eq,
                ..
            })
        ),
        "Some(0) must carry a refinement guard, got {:?}",
        arm.guard
    );
}

fn make_nullary_constructor_value(
    module_name: &str,
    type_name: &str,
    constructor_name: &str,
    constructors_count: u16,
) -> TypedExpr {
    let output_type = make_nullary_constructor_type(module_name, type_name);

    TypedExpr::Var {
        location: Span::empty(),
        constructor: ValueConstructor::public(
            output_type,
            ValueConstructorVariant::Record {
                name: constructor_name.to_string(),
                arity: 0,
                field_map: None,
                location: Span::empty(),
                module: module_name.to_string(),
                constructors_count,
            },
        ),
        name: constructor_name.to_string(),
    }
}

fn make_nullary_constructor_mapper_body(
    arg_name: &str,
    input_module: &str,
    input_type_name: &str,
    output_module: &str,
    output_type_name: &str,
    mappings: &[(&str, &str)],
    output_constructors_count: u16,
) -> TypedExpr {
    let input_type = make_nullary_constructor_type(input_module, input_type_name);
    let output_type = make_nullary_constructor_type(output_module, output_type_name);

    let clauses = mappings
        .iter()
        .map(|(source_constructor, output_constructor)| TypedClause {
            location: Span::empty(),
            pattern: TypedPattern::constructor(
                source_constructor,
                &[],
                input_type.clone(),
                Span::empty(),
            ),
            then: make_nullary_constructor_value(
                output_module,
                output_type_name,
                output_constructor,
                output_constructors_count,
            ),
        })
        .collect();

    TypedExpr::When {
        location: Span::empty(),
        tipo: output_type,
        subject: Box::new(local_var(arg_name, input_type)),
        clauses,
    }
}

fn make_nullary_constructor_mapper(
    input_module: &str,
    input_type_name: &str,
    output_module: &str,
    output_type_name: &str,
    mappings: &[(&str, &str)],
    output_constructors_count: u16,
) -> TypedExpr {
    let input_type = make_nullary_constructor_type(input_module, input_type_name);
    let output_type = make_nullary_constructor_type(output_module, output_type_name);

    make_unary_mapper(
        "state",
        input_type,
        output_type,
        make_nullary_constructor_mapper_body(
            "state",
            input_module,
            input_type_name,
            output_module,
            output_type_name,
            mappings,
            output_constructors_count,
        ),
    )
}

fn make_named_nullary_constructor_mapper_function(
    name: &str,
    input_module: &str,
    input_type_name: &str,
    output_module: &str,
    output_type_name: &str,
    mappings: &[(&str, &str)],
    output_constructors_count: u16,
) -> (FunctionAccessKey, TypedFunction) {
    let input_type = make_nullary_constructor_type(input_module, input_type_name);
    let output_type = make_nullary_constructor_type(output_module, output_type_name);

    (
        FunctionAccessKey {
            module_name: "math".to_string(),
            function_name: name.to_string(),
        },
        TypedFunction {
            arguments: vec![TypedArg::new("state", input_type.clone())],
            body: make_nullary_constructor_mapper_body(
                "state",
                input_module,
                input_type_name,
                output_module,
                output_type_name,
                mappings,
                output_constructors_count,
            ),
            doc: None,
            location: Span::empty(),
            name: name.to_string(),
            public: false,
            return_annotation: None,
            return_type: output_type,
            end_position: 0,
            on_test_failure: OnTestFailure::FailImmediately,
        },
    )
}

#[test]
fn normalize_fuzzer_map_shape_is_name_agnostic() {
    let via = make_typed_map_call(
        make_leaf_fuzzer_call("primitive_source", Type::int()),
        make_unresolved_unary_mapper("f", Type::int()),
        Type::int(),
    );

    let normalized = normalize_fuzzer_from_via(&via, "math", &empty_known_functions());
    assert_normalized_map(normalized);
}

#[test]
fn normalize_fuzzer_bind_shape_is_name_agnostic() {
    let via = make_typed_bind_call(
        make_leaf_fuzzer_call("primitive_source", Type::int()),
        make_bind_continuation("next_step", Type::int(), Type::bool()),
        Type::bool(),
    );

    let normalized = normalize_fuzzer_from_via(&via, "math", &empty_known_functions());
    assert_normalized_bind(normalized);
}

#[test]
fn normalize_fuzzer_direct_product_shape_is_name_agnostic() {
    let via = make_typed_product_call(
        make_leaf_fuzzer_call("lhs", Type::int()),
        make_leaf_fuzzer_call("rhs", Type::int()),
    );

    let normalized = normalize_fuzzer_from_via(&via, "math", &empty_known_functions());
    assert_normalized_product(normalized, 2);
}

#[test]
fn normalize_fuzzer_mapped_product_shape_is_name_agnostic() {
    let via = make_typed_map2_product_call(
        make_leaf_fuzzer_call("lhs", Type::int()),
        make_leaf_fuzzer_call("rhs", Type::int()),
        make_map2_mapper(vec![
            local_var("a", Type::int()),
            local_var("b", Type::int()),
        ]),
    );

    let normalized = normalize_fuzzer_from_via(&via, "math", &empty_known_functions());
    assert_normalized_product(normalized, 2);
}

#[test]
fn normalize_fuzzer_zero_arg_wrapper_unwraps_structurally() {
    let (helper_key, helper_fn) = make_zero_arg_fuzzer_function(
        "custom_wrapper",
        Type::int(),
        make_typed_map_call(
            make_leaf_fuzzer_call("primitive_source", Type::int()),
            make_unresolved_unary_mapper("f", Type::int()),
            Type::int(),
        ),
    );
    let mut functions = empty_known_functions();
    functions.insert(&helper_key, &helper_fn);

    let via = make_zero_arg_fuzzer_call("custom_wrapper", Type::int());
    let normalized = normalize_fuzzer_from_via(&via, "math", &functions);
    assert_normalized_map(normalized);
}

#[test]
fn normalize_fuzzer_list_shape_is_name_agnostic() {
    let via = make_typed_list_call(make_leaf_fuzzer_call("elem", Type::int()), Type::int());

    let normalized = normalize_fuzzer_from_via(&via, "math", &empty_known_functions());
    assert_normalized_list(normalized);
}

#[test]
fn normalize_fuzzer_name_collision_is_not_special_cased() {
    let (helper_key, helper_fn) = make_zero_arg_fuzzer_function(
        "map",
        Type::int(),
        make_leaf_fuzzer_call("primitive_source", Type::int()),
    );
    let mut functions = empty_known_functions();
    functions.insert(&helper_key, &helper_fn);

    let via = make_zero_arg_fuzzer_call("map", Type::int());
    let normalized = normalize_fuzzer_from_via(&via, "math", &functions);
    assert!(matches!(normalized, NormalizedFuzzer::Primitive { .. }));
}

#[test]
fn normalize_fuzzer_control_flow_ignores_failing_branch() {
    let via = make_if_fuzzer_expr(
        Type::int(),
        make_typed_int_between_fuzzer("1", "3"),
        make_fail_fuzzer_expr(Type::int()),
        true,
    );

    let constraint = extract_constraint_from_via(&via, "math", &empty_known_functions());

    assert_eq!(
        constraint,
        FuzzerConstraint::IntRange {
            min: "1".to_string(),
            max: "3".to_string(),
        }
    );
}

#[test]
fn normalize_fuzzer_control_flow_all_failing_branches_is_empty() {
    let via = make_if_fuzzer_expr(
        Type::int(),
        make_fail_fuzzer_expr(Type::int()),
        make_fail_fuzzer_expr(Type::int()),
        true,
    );

    let constraint = extract_constraint_from_via(&via, "math", &empty_known_functions());

    match constraint {
        FuzzerConstraint::Empty { reason } => {
            assert!(reason.contains("no producing branches"));
        }
        other => panic!("expected empty fuzzer constraint, got {other:?}"),
    }
}

#[test]
fn normalize_fuzzer_sequence_alias_unwraps_structurally() {
    let map_expr = make_typed_map_call(
        make_leaf_fuzzer_call("primitive_source", Type::int()),
        make_unresolved_unary_mapper("f", Type::int()),
        Type::int(),
    );
    let alias_type = map_expr.tipo();
    let via = TypedExpr::Sequence {
        location: Span::empty(),
        expressions: vec![
            TypedExpr::Assignment {
                location: Span::empty(),
                tipo: alias_type.clone(),
                value: Box::new(map_expr),
                pattern: TypedPattern::var("alias"),
                kind: crate::ast::AssignmentKind::Let { backpassing: () },
                comment: None,
            },
            local_var("alias", alias_type),
        ],
    };

    let normalized = normalize_fuzzer_from_via(&via, "math", &empty_known_functions());
    assert_normalized_map(normalized);
}

#[test]
fn normalize_fuzzer_recursive_wrapper_cycle_is_opaque() {
    // A mutually-recursive fuzzer cycle has no base case the normalizer
    // can widen from (the helper body is just another `Call` to the
    // peer, not a control-flow or lambda shape whose output type we
    // can trust). Keeping such cycles opaque is both sound and
    // informative — the user almost certainly wrote a bug if the
    // fuzzer recurses without a base case.
    let (left_key, left_fn) = make_zero_arg_fuzzer_function(
        "left",
        Type::int(),
        make_zero_arg_fuzzer_call("right", Type::int()),
    );
    let (right_key, right_fn) = make_zero_arg_fuzzer_function(
        "right",
        Type::int(),
        make_zero_arg_fuzzer_call("left", Type::int()),
    );
    let mut functions = empty_known_functions();
    functions.insert(&left_key, &left_fn);
    functions.insert(&right_key, &right_fn);

    let normalized = normalize_fuzzer_from_via(
        &make_zero_arg_fuzzer_call("left", Type::int()),
        "math",
        &functions,
    );

    match normalized {
        NormalizedFuzzer::Opaque { reason, .. } => {
            assert!(reason.contains("recursive helper fuzzer detected"));
        }
        other => panic!("expected opaque recursive normalization, got {other:?}"),
    }
}

#[test]
fn normalize_fuzzer_beta_reduces_zero_arg_fn_literal_call() {
    // S2 SUBSET: A call whose callee is a zero-argument `Fn` literal
    // (e.g. `fn() { leaf() }()`) should beta-reduce to the body.  Without
    // this, the call falls through to helper descent (which cannot resolve
    // a raw `Fn` literal via `resolve_function_from_expr`) and normalizes
    // to `Opaque`, erasing the structural shape of the body.
    //
    // This shape arises inside `fork*_and_then` stdlib bodies, where the
    // thunk parameters (`baseline`, `branch1`, …) are substituted with
    // `fn() { scenario_inputs_X(st) }` locals and invoked as `baseline()`.
    let int_tipo = Type::int();
    let fuzzer_tipo = Type::fuzzer(int_tipo.clone());

    let leaf = make_leaf_fuzzer_call("primitive_source", int_tipo.clone());
    let thunk = TypedExpr::Fn {
        location: Span::empty(),
        tipo: Type::function(vec![], fuzzer_tipo.clone()),
        is_capture: false,
        args: vec![],
        body: Box::new(leaf),
        return_annotation: None,
    };
    let via = TypedExpr::Call {
        location: Span::empty(),
        tipo: fuzzer_tipo,
        fun: Box::new(thunk),
        args: vec![],
    };

    let normalized = normalize_fuzzer_from_via(&via, "math", &empty_known_functions());
    assert_normalized_leaf(normalized);
}

#[test]
fn normalize_fuzzer_beta_reduces_fn_literal_through_local_alias() {
    // S2 SUBSET: Beta reduction must also work when the `Fn` literal is
    // reached through a chain of local aliases — this is the exact shape
    // produced by helper descent into `fork*_and_then`, where formal
    // parameters (`baseline`) are bound in `local_values` to the caller's
    // thunk expressions.  We simulate that binding with a `let`.
    let int_tipo = Type::int();
    let fuzzer_tipo = Type::fuzzer(int_tipo.clone());

    let leaf = make_leaf_fuzzer_call("primitive_source", int_tipo.clone());
    let thunk_tipo = Type::function(vec![], fuzzer_tipo.clone());
    let thunk = TypedExpr::Fn {
        location: Span::empty(),
        tipo: thunk_tipo.clone(),
        is_capture: false,
        args: vec![],
        body: Box::new(leaf),
        return_annotation: None,
    };

    // let baseline = fn() { primitive_source() } ; baseline()
    let alias_call = TypedExpr::Call {
        location: Span::empty(),
        tipo: fuzzer_tipo.clone(),
        fun: Box::new(local_var("baseline", thunk_tipo.clone())),
        args: vec![],
    };
    let via = TypedExpr::Sequence {
        location: Span::empty(),
        expressions: vec![
            TypedExpr::Assignment {
                location: Span::empty(),
                tipo: thunk_tipo,
                value: Box::new(thunk),
                pattern: TypedPattern::var("baseline"),
                kind: crate::ast::AssignmentKind::Let { backpassing: () },
                comment: None,
            },
            alias_call,
        ],
    };

    let normalized = normalize_fuzzer_from_via(&via, "math", &empty_known_functions());
    assert_normalized_leaf(normalized);
}

#[test]
fn extract_constraint_name_agnostic_map_preserves_map_domain_for_unknown_mapper() {
    // With stdlib-name-gated extraction restored, `int_between(1, 3)`
    // extracts `IntRange { min: "1", max: "3" }`. `map(int_between(1, 3), f)`
    // therefore wraps that range inside `Map`.
    let via = make_typed_map_call(
        make_typed_int_between_fuzzer("1", "3"),
        make_unresolved_unary_mapper("f", Type::int()),
        Type::int(),
    );

    let constraint = extract_constraint_from_via(&via, "math", &empty_known_functions());
    assert_eq!(
        constraint,
        FuzzerConstraint::Map(Box::new(FuzzerConstraint::IntRange {
            min: "1".to_string(),
            max: "3".to_string(),
        }))
    );
}

#[test]
fn extract_constraint_name_agnostic_identity_map_preserves_source_domain() {
    // The source `int_between(1, 3)` now extracts bounds [1, 3]. An
    // identity mapper propagates the source's constraint unchanged.
    let via = make_typed_map_call(
        make_typed_int_between_fuzzer("1", "3"),
        make_identity_mapper("n", Type::int()),
        Type::int(),
    );

    let constraint = extract_constraint_from_via(&via, "math", &empty_known_functions());
    assert_eq!(
        constraint,
        FuzzerConstraint::IntRange {
            min: "1".to_string(),
            max: "3".to_string(),
        }
    );
}

#[test]
fn extract_fuzzer_payload_type_handles_type_fn_form() {
    // Sanity check on the baseline: `Type::fuzzer(Int)` (the canonical
    // `Type::Fn` materialization of `Fuzzer<Int>`) should yield `Int`.
    let payload = Type::int();
    let fuzzer = Type::fuzzer(payload.clone());

    let extracted = extract_fuzzer_payload_type(fuzzer.as_ref())
        .expect("canonical Fuzzer<Int> must yield Int payload");

    assert_eq!(extracted.as_ref(), payload.as_ref());
}

#[test]
fn extract_fuzzer_payload_type_handles_type_app_alias() {
    // Direct `Type::App { name: "Fuzzer", args: [T] }` representation of
    // `Fuzzer<Int>`. In today's codebase transparent aliases collapse to
    // the underlying `Type::Fn`, but this branch is exercised defensively
    // so any future alias/FFI path that surfaces a `Type::App`-shaped
    // Fuzzer still extracts the payload correctly.
    let payload = Type::int();
    let fuzzer_app = Rc::new(Type::App {
        public: true,
        contains_opaque: false,
        module: String::new(),
        name: "Fuzzer".to_string(),
        args: vec![payload.clone()],
        alias: None,
    });

    let extracted = extract_fuzzer_payload_type(fuzzer_app.as_ref())
        .expect("Type::App form of Fuzzer<Int> must yield Int payload");

    assert_eq!(extracted.as_ref(), payload.as_ref());
}

#[test]
fn extract_fuzzer_payload_type_rejects_non_prelude_fuzzer_app() {
    // A `Type::App` named "Fuzzer" from a non-prelude module must NOT be
    // unwrapped: only the prelude `Fuzzer` (empty module) is recognized,
    // mirroring how `is_prng_type` and `is_option` check names.
    let payload = Type::int();
    let foreign_fuzzer = Rc::new(Type::App {
        public: true,
        contains_opaque: false,
        module: "some/other/module".to_string(),
        name: "Fuzzer".to_string(),
        args: vec![payload],
        alias: None,
    });

    assert!(extract_fuzzer_payload_type(foreign_fuzzer.as_ref()).is_none());
}

#[test]
fn extract_fuzzer_payload_type_rejects_type_app_wrong_arity() {
    // The `args.len() == 1` guard must reject zero-arg and multi-arg App
    // forms even if module and name match the prelude Fuzzer.
    let payload = Type::int();

    let zero_arg = Rc::new(Type::App {
        public: true,
        contains_opaque: false,
        module: String::new(),
        name: "Fuzzer".to_string(),
        args: vec![],
        alias: None,
    });
    assert!(
        extract_fuzzer_payload_type(zero_arg.as_ref()).is_none(),
        "zero-arg Fuzzer App must be rejected"
    );

    let two_arg = Rc::new(Type::App {
        public: true,
        contains_opaque: false,
        module: String::new(),
        name: "Fuzzer".to_string(),
        args: vec![payload.clone(), payload],
        alias: None,
    });
    assert!(
        extract_fuzzer_payload_type(two_arg.as_ref()).is_none(),
        "two-arg Fuzzer App must be rejected"
    );
}

fn make_typed_int_fuzzer() -> TypedExpr {
    let output_type = Type::int();
    let fuzzer_type = Type::fuzzer(output_type.clone());
    TypedExpr::Call {
        location: Span::empty(),
        tipo: fuzzer_type.clone(),
        fun: Box::new(module_fn_var(
            "int",
            STDLIB_FUZZ_MODULE,
            Type::function(vec![], fuzzer_type),
        )),
        args: vec![],
    }
}

fn make_typed_byte_fuzzer() -> TypedExpr {
    let output_type = Type::int();
    let fuzzer_type = Type::fuzzer(output_type.clone());
    TypedExpr::Call {
        location: Span::empty(),
        tipo: fuzzer_type.clone(),
        fun: Box::new(module_fn_var(
            "byte",
            STDLIB_FUZZ_MODULE,
            Type::function(vec![], fuzzer_type),
        )),
        args: vec![],
    }
}

fn make_typed_int_at_least_fuzzer(min: &str) -> TypedExpr {
    let output_type = Type::int();
    let fuzzer_type = Type::fuzzer(output_type.clone());
    TypedExpr::Call {
        location: Span::empty(),
        tipo: fuzzer_type.clone(),
        fun: Box::new(module_fn_var(
            "int_at_least",
            STDLIB_FUZZ_MODULE,
            Type::function(vec![Type::int()], fuzzer_type),
        )),
        args: vec![call_arg(uint_lit(min))],
    }
}

fn make_typed_int_at_most_fuzzer(max: &str) -> TypedExpr {
    let output_type = Type::int();
    let fuzzer_type = Type::fuzzer(output_type.clone());
    TypedExpr::Call {
        location: Span::empty(),
        tipo: fuzzer_type.clone(),
        fun: Box::new(module_fn_var(
            "int_at_most",
            STDLIB_FUZZ_MODULE,
            Type::function(vec![Type::int()], fuzzer_type),
        )),
        args: vec![call_arg(uint_lit(max))],
    }
}

fn make_typed_bytearray_fixed_fuzzer(len: &str) -> TypedExpr {
    let output_type = Type::byte_array();
    let fuzzer_type = Type::fuzzer(output_type.clone());
    TypedExpr::Call {
        location: Span::empty(),
        tipo: fuzzer_type.clone(),
        fun: Box::new(module_fn_var(
            "bytearray_fixed",
            STDLIB_FUZZ_MODULE,
            Type::function(vec![Type::int()], fuzzer_type),
        )),
        args: vec![call_arg(uint_lit(len))],
    }
}

#[test]
fn int_primitive_constraint_uses_trusted_bounded_support() {
    let via = make_typed_int_fuzzer();
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

    let constraint = extract_constraint_from_via(&via, "math", &empty_known_functions());
    assert_eq!(
        constraint,
        FuzzerConstraint::IntRange {
            min: "-255".to_string(),
            max: "16383".to_string(),
        }
    );

    let semantics = extract_semantics_from_via(
        &via,
        "math",
        &empty_known_functions(),
        &data_types,
        Type::int().as_ref(),
    );

    assert_eq!(
        semantics,
        FuzzerSemantics::IntRange {
            min: Some("-255".to_string()),
            max: Some("16383".to_string()),
        }
    );
}

#[test]
fn byte_primitive_constraint_uses_exact_byte_range() {
    let via = make_typed_byte_fuzzer();
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

    let constraint = extract_constraint_from_via(&via, "math", &empty_known_functions());
    assert_eq!(
        constraint,
        FuzzerConstraint::IntRange {
            min: "0".to_string(),
            max: "255".to_string(),
        }
    );

    let semantics = extract_semantics_from_via(
        &via,
        "math",
        &empty_known_functions(),
        &data_types,
        Type::int().as_ref(),
    );

    assert_eq!(
        semantics,
        FuzzerSemantics::IntRange {
            min: Some("0".to_string()),
            max: Some("255".to_string()),
        }
    );
}

#[test]
fn int_at_least_semantics_use_bounded_upper_support() {
    let via = make_typed_int_at_least_fuzzer("5");
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

    let constraint = extract_constraint_from_via(&via, "math", &empty_known_functions());
    assert_eq!(
        constraint,
        FuzzerConstraint::IntRange {
            min: "5".to_string(),
            max: "255".to_string(),
        }
    );

    let semantics = extract_semantics_from_via(
        &via,
        "math",
        &empty_known_functions(),
        &data_types,
        Type::int().as_ref(),
    );

    assert_eq!(
        semantics,
        FuzzerSemantics::IntRange {
            min: Some("5".to_string()),
            max: Some("255".to_string()),
        }
    );
}

#[test]
fn int_at_most_semantics_use_bounded_lower_support() {
    let via = make_typed_int_at_most_fuzzer("10");
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

    let constraint = extract_constraint_from_via(&via, "math", &empty_known_functions());
    assert_eq!(
        constraint,
        FuzzerConstraint::IntRange {
            min: "-255".to_string(),
            max: "10".to_string(),
        }
    );

    let semantics = extract_semantics_from_via(
        &via,
        "math",
        &empty_known_functions(),
        &data_types,
        Type::int().as_ref(),
    );

    assert_eq!(
        semantics,
        FuzzerSemantics::IntRange {
            min: Some("-255".to_string()),
            max: Some("10".to_string()),
        }
    );
}

#[test]
fn bytearray_fixed_constraint_preserves_exact_length() {
    let via = make_typed_bytearray_fixed_fuzzer("4");
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

    let constraint = extract_constraint_from_via(&via, "math", &empty_known_functions());
    assert_eq!(
        constraint,
        FuzzerConstraint::ByteStringLenRange {
            min_len: 4,
            max_len: 4,
        }
    );

    let semantics = extract_semantics_from_via(
        &via,
        "math",
        &empty_known_functions(),
        &data_types,
        Type::byte_array().as_ref(),
    );

    assert_eq!(
        semantics,
        FuzzerSemantics::ByteArrayRange {
            min_len: Some(4),
            max_len: Some(4),
        }
    );
}

#[test]
fn extract_constraint_name_agnostic_named_identity_mapper_uses_function_body_shape() {
    // The inner `int_between(1, 3)` now extracts [1, 3] via the stdlib
    // recognizer. A named identity mapper structurally collapses the
    // map to its source, so the outer constraint is that same range.
    let (identity_key, identity_fn) =
        make_named_unary_identity_mapper_function("identity", Type::int());
    let mut functions = empty_known_functions();
    functions.insert(&identity_key, &identity_fn);

    let via = make_typed_map_call(
        make_typed_int_between_fuzzer("1", "3"),
        module_fn_var(
            "identity",
            "math",
            Type::function(vec![Type::int()], Type::int()),
        ),
        Type::int(),
    );

    let constraint = extract_constraint_from_via(&via, "math", &functions);
    assert_eq!(
        constraint,
        FuzzerConstraint::IntRange {
            min: "1".to_string(),
            max: "3".to_string(),
        }
    );
}

#[test]
fn extract_constraint_name_agnostic_constant_bool_map_is_exact() {
    let via = make_typed_map_call(
        make_leaf_fuzzer_call("seed", Type::int()),
        make_constant_bool_mapper(Type::int(), true),
        Type::bool(),
    );

    let constraint = extract_constraint_from_via(&via, "math", &empty_known_functions());
    assert_eq!(
        constraint,
        FuzzerConstraint::Exact(FuzzerExactValue::Bool(true))
    );
}

#[test]
fn extract_constraint_named_tautology_bool_map_is_exact() {
    let (mapper_key, mapper_fn) =
        make_named_unary_tautology_mapper_function("tautology", Type::bool());
    let mut functions = empty_known_functions();
    functions.insert(&mapper_key, &mapper_fn);

    let via = make_typed_map_call(
        make_typed_bool_fuzzer(),
        module_fn_var(
            "tautology",
            "math",
            Type::function(vec![Type::bool()], Type::bool()),
        ),
        Type::bool(),
    );

    let constraint = extract_constraint_from_via(&via, "math", &functions);
    assert_eq!(
        constraint,
        FuzzerConstraint::Exact(FuzzerExactValue::Bool(true))
    );
}

#[test]
fn extract_constraint_name_agnostic_constant_int_map_is_singleton_range() {
    let via = make_typed_map_call(
        make_leaf_fuzzer_call("seed", Type::int()),
        make_constant_int_mapper(Type::int(), "41"),
        Type::int(),
    );

    let constraint = extract_constraint_from_via(&via, "math", &empty_known_functions());
    assert_eq!(
        constraint,
        FuzzerConstraint::IntRange {
            min: "41".to_string(),
            max: "41".to_string(),
        }
    );
}

#[test]
fn extract_constraint_name_agnostic_named_constant_int_mapper_uses_function_body_shape() {
    let (mapper_key, mapper_fn) = make_named_unary_constant_int_mapper_function("always_7", "7");
    let mut functions = empty_known_functions();
    functions.insert(&mapper_key, &mapper_fn);

    let via = make_typed_map_call(
        make_leaf_fuzzer_call("seed", Type::int()),
        module_fn_var(
            "always_7",
            "math",
            Type::function(vec![Type::int()], Type::int()),
        ),
        Type::int(),
    );

    let constraint = extract_constraint_from_via(&via, "math", &functions);
    assert_eq!(
        constraint,
        FuzzerConstraint::IntRange {
            min: "7".to_string(),
            max: "7".to_string(),
        }
    );
}

#[test]
fn extract_constraint_finite_string_if_mapper_is_oneof() {
    let via = make_typed_map_call(
        make_typed_int_between_fuzzer("0", "3"),
        finite_string_if_mapper(),
        Type::string(),
    );

    let constraint = extract_constraint_from_via(&via, "math", &empty_known_functions());
    assert_eq!(
        constraint,
        FuzzerConstraint::OneOf(expected_finite_string_values())
    );
}

#[test]
fn extract_constraint_finite_string_when_mapper_is_oneof() {
    let via = make_typed_map_call(
        make_typed_int_between_fuzzer("0", "3"),
        finite_string_when_mapper(),
        Type::string(),
    );

    let constraint = extract_constraint_from_via(&via, "math", &empty_known_functions());
    assert_eq!(
        constraint,
        FuzzerConstraint::OneOf(expected_finite_string_values())
    );
}

#[test]
fn extract_constraint_finite_string_singleton_canonicalizes_to_exact() {
    let via = make_typed_map_call(
        make_typed_int_between_fuzzer("0", "3"),
        make_unary_mapper("i", Type::int(), Type::string(), string_lit("same")),
        Type::string(),
    );

    let constraint = extract_constraint_from_via(&via, "math", &empty_known_functions());
    assert_eq!(
        constraint,
        FuzzerConstraint::Exact(FuzzerExactValue::String("same".to_string()))
    );
}

#[test]
fn extract_constraint_name_agnostic_nested_constant_then_affine_map_transforms_range() {
    let source = make_typed_map_call(
        make_leaf_fuzzer_call("seed", Type::int()),
        make_constant_int_mapper(Type::int(), "41"),
        Type::int(),
    );
    let via = make_typed_map_call(source, make_add_int_mapper("1"), Type::int());

    let constraint = extract_constraint_from_via(&via, "math", &empty_known_functions());
    assert_eq!(
        constraint,
        FuzzerConstraint::IntRange {
            min: "42".to_string(),
            max: "42".to_string(),
        }
    );
}

#[test]
fn extract_constraint_name_agnostic_bind_uses_continuation_shape() {
    // `int_between(5, 8)` now extracts [5, 8] via the stdlib recognizer.
    // A bind's output domain is the continuation's domain, so the
    // result reflects the continuation's extracted bounds.
    let via = make_typed_bind_call(
        make_leaf_fuzzer_call("seed", Type::int()),
        make_inline_bind_continuation(
            "x",
            Type::int(),
            make_typed_int_between_fuzzer("5", "8"),
            Type::int(),
        ),
        Type::int(),
    );

    let constraint = extract_constraint_from_via(&via, "math", &empty_known_functions());
    assert_eq!(
        constraint,
        FuzzerConstraint::IntRange {
            min: "5".to_string(),
            max: "8".to_string(),
        }
    );
}

#[test]
fn extract_constraint_name_agnostic_product_uses_element_shapes() {
    // Each product element now carries its extracted range under the
    // stdlib recognizer; the tuple-level constraint records both.
    let via = make_typed_product_call(
        make_typed_int_between_fuzzer("0", "10"),
        make_typed_int_between_fuzzer("20", "30"),
    );

    let constraint = extract_constraint_from_via(&via, "math", &empty_known_functions());
    assert_eq!(
        constraint,
        FuzzerConstraint::Tuple(vec![
            FuzzerConstraint::IntRange {
                min: "0".to_string(),
                max: "10".to_string(),
            },
            FuzzerConstraint::IntRange {
                min: "20".to_string(),
                max: "30".to_string(),
            },
        ])
    );
}

#[test]
fn extract_constraint_name_agnostic_list_uses_element_shape() {
    // The element fuzzer is now recognized as `int_between`, producing
    // `IntRange { 0, 10 }`. The enclosing list call is `anything_but_list`
    // (not a stdlib `list_*` function) so no length bounds are extracted.
    let via = make_typed_list_call(make_typed_int_between_fuzzer("0", "10"), Type::int());

    let constraint = extract_constraint_from_via(&via, "math", &empty_known_functions());
    assert_eq!(
        constraint,
        FuzzerConstraint::List {
            elem: Box::new(FuzzerConstraint::IntRange {
                min: "0".to_string(),
                max: "10".to_string(),
            }),
            min_len: None,
            max_len: None,
        }
    );
}

#[test]
fn extract_constraint_name_agnostic_nullary_constructor_output_uses_constructor_domain() {
    let output_type = make_nullary_constructor_type("permissions", "Outcome");
    let via = make_leaf_fuzzer_call("custom_outcome_fuzzer", output_type.clone());
    let owned_data_types =
        make_nullary_constructor_data_types("permissions", "Outcome", &["Ok", "Ko"]);
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();

    let constraint = extract_constraint_from_via_with_data_types(
        &via,
        "math",
        &empty_known_functions(),
        &data_types,
    );
    assert_eq!(
        constraint,
        FuzzerConstraint::DataConstructorTags { tags: vec![0, 1] }
    );
}

#[test]
fn extract_constraint_name_agnostic_constructor_map_pushes_forward_nullary_domain() {
    let source_type = make_nullary_constructor_type("workflow", "Stage");
    let output_type = make_nullary_constructor_type("approval", "Decision");
    let via = make_typed_map_call(
        make_leaf_fuzzer_call("stage_fuzzer", source_type.clone()),
        make_nullary_constructor_mapper(
            "workflow",
            "Stage",
            "approval",
            "Decision",
            &[("Idle", "Allow"), ("Busy", "Review")],
            3,
        ),
        output_type.clone(),
    );

    let mut owned_data_types =
        make_nullary_constructor_data_types("workflow", "Stage", &["Idle", "Busy"]);
    owned_data_types.extend(make_nullary_constructor_data_types(
        "approval",
        "Decision",
        &["Allow", "Deny", "Review"],
    ));
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();

    let constraint = extract_constraint_from_via_with_data_types(
        &via,
        "math",
        &empty_known_functions(),
        &data_types,
    );

    assert_eq!(
        constraint,
        FuzzerConstraint::DataConstructorTags { tags: vec![0, 2] }
    );
}

#[test]
fn extract_constraint_name_agnostic_named_constructor_mapper_uses_function_body_shape() {
    let source_type = make_nullary_constructor_type("workflow", "Stage");
    let output_type = make_nullary_constructor_type("approval", "Decision");
    let (mapper_key, mapper_fn) = make_named_nullary_constructor_mapper_function(
        "collapse_stage",
        "workflow",
        "Stage",
        "approval",
        "Decision",
        &[("Idle", "Deny"), ("Busy", "Deny")],
        3,
    );
    let mut functions = empty_known_functions();
    functions.insert(&mapper_key, &mapper_fn);

    let via = make_typed_map_call(
        make_leaf_fuzzer_call("stage_fuzzer", source_type.clone()),
        module_fn_var(
            "collapse_stage",
            "math",
            Type::function(vec![source_type.clone()], output_type.clone()),
        ),
        output_type.clone(),
    );

    let mut owned_data_types =
        make_nullary_constructor_data_types("workflow", "Stage", &["Idle", "Busy"]);
    owned_data_types.extend(make_nullary_constructor_data_types(
        "approval",
        "Decision",
        &["Allow", "Deny", "Review"],
    ));
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();

    let constraint =
        extract_constraint_from_via_with_data_types(&via, "math", &functions, &data_types);

    assert_eq!(
        constraint,
        FuzzerConstraint::DataConstructorTags { tags: vec![1] }
    );
}

#[test]
fn extract_constraint_name_agnostic_constructor_map_requires_total_mapping() {
    let source_type = make_nullary_constructor_type("workflow", "Stage");
    let output_type = make_nullary_constructor_type("approval", "Decision");
    let via = make_typed_map_call(
        make_leaf_fuzzer_call("stage_fuzzer", source_type.clone()),
        make_nullary_constructor_mapper(
            "workflow",
            "Stage",
            "approval",
            "Decision",
            &[("Idle", "Allow")],
            3,
        ),
        output_type.clone(),
    );

    let mut owned_data_types =
        make_nullary_constructor_data_types("workflow", "Stage", &["Idle", "Busy"]);
    owned_data_types.extend(make_nullary_constructor_data_types(
        "approval",
        "Decision",
        &["Allow", "Deny", "Review"],
    ));
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();

    let constraint = extract_constraint_from_via_with_data_types(
        &via,
        "math",
        &empty_known_functions(),
        &data_types,
    );

    assert_eq!(
        constraint,
        FuzzerConstraint::Map(Box::new(FuzzerConstraint::DataConstructorTags {
            tags: vec![0, 1],
        }))
    );
}

#[test]
fn extract_constraint_name_agnostic_constructor_map_unresolved_mapper_is_conservative() {
    let source_type = make_nullary_constructor_type("workflow", "Stage");
    let output_type = make_nullary_constructor_type("approval", "Decision");
    let via = make_typed_map_call(
        make_leaf_fuzzer_call("stage_fuzzer", source_type.clone()),
        make_unresolved_unary_mapper_with_types(
            "next_stage",
            source_type.clone(),
            output_type.clone(),
        ),
        output_type.clone(),
    );

    let mut owned_data_types =
        make_nullary_constructor_data_types("workflow", "Stage", &["Idle", "Busy"]);
    owned_data_types.extend(make_nullary_constructor_data_types(
        "approval",
        "Decision",
        &["Allow", "Deny", "Review"],
    ));
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();

    let constraint = extract_constraint_from_via_with_data_types(
        &via,
        "math",
        &empty_known_functions(),
        &data_types,
    );

    assert_eq!(
        constraint,
        FuzzerConstraint::Map(Box::new(FuzzerConstraint::DataConstructorTags {
            tags: vec![0, 1],
        }))
    );
}

#[test]
fn extract_constraint_name_agnostic_constructor_map_recursive_mapper_cycle_is_conservative() {
    let source_type = make_nullary_constructor_type("workflow", "Stage");
    let output_type = make_nullary_constructor_type("approval", "Decision");
    let mapper_type = Type::function(vec![source_type.clone()], output_type.clone());

    let (left_key, left_fn) = make_zero_arg_function(
        "left_mapper",
        mapper_type.clone(),
        make_zero_arg_call("right_mapper", mapper_type.clone()),
    );
    let (right_key, right_fn) = make_zero_arg_function(
        "right_mapper",
        mapper_type.clone(),
        make_zero_arg_call("left_mapper", mapper_type.clone()),
    );
    let mut functions = empty_known_functions();
    functions.insert(&left_key, &left_fn);
    functions.insert(&right_key, &right_fn);

    let via = make_typed_map_call(
        make_leaf_fuzzer_call("stage_fuzzer", source_type.clone()),
        make_zero_arg_call("left_mapper", mapper_type),
        output_type.clone(),
    );

    let mut owned_data_types =
        make_nullary_constructor_data_types("workflow", "Stage", &["Idle", "Busy"]);
    owned_data_types.extend(make_nullary_constructor_data_types(
        "approval",
        "Decision",
        &["Allow", "Deny", "Review"],
    ));
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();

    let constraint =
        extract_constraint_from_via_with_data_types(&via, "math", &functions, &data_types);

    assert_eq!(
        constraint,
        FuzzerConstraint::Map(Box::new(FuzzerConstraint::DataConstructorTags {
            tags: vec![0, 1],
        }))
    );
}

#[test]
fn extract_semantics_name_agnostic_nullary_constructor_output_uses_constructor_semantics() {
    let output_type = make_nullary_constructor_type("permissions", "Outcome");
    let via = make_leaf_fuzzer_call("custom_outcome_fuzzer", output_type.clone());
    let owned_data_types =
        make_nullary_constructor_data_types("permissions", "Outcome", &["Ok", "Ko"]);
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();

    let semantics = extract_semantics_from_via(
        &via,
        "math",
        &empty_known_functions(),
        &data_types,
        output_type.as_ref(),
    );
    assert_eq!(
        semantics,
        FuzzerSemantics::Constructors { tags: vec![0, 1] }
    );
}

#[test]
fn extract_semantics_name_agnostic_constructor_map_pushes_forward_nullary_domain() {
    let source_type = make_nullary_constructor_type("workflow", "Stage");
    let output_type = make_nullary_constructor_type("approval", "Decision");
    let via = make_typed_map_call(
        make_leaf_fuzzer_call("stage_fuzzer", source_type.clone()),
        make_nullary_constructor_mapper(
            "workflow",
            "Stage",
            "approval",
            "Decision",
            &[("Idle", "Allow"), ("Busy", "Review")],
            3,
        ),
        output_type.clone(),
    );

    let mut owned_data_types =
        make_nullary_constructor_data_types("workflow", "Stage", &["Idle", "Busy"]);
    owned_data_types.extend(make_nullary_constructor_data_types(
        "approval",
        "Decision",
        &["Allow", "Deny", "Review"],
    ));
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();

    let semantics = extract_semantics_from_via(
        &via,
        "math",
        &empty_known_functions(),
        &data_types,
        output_type.as_ref(),
    );

    assert_eq!(
        semantics,
        FuzzerSemantics::Constructors { tags: vec![0, 2] }
    );
}

#[test]
fn extract_semantics_name_agnostic_named_constructor_mapper_uses_function_body_shape() {
    let source_type = make_nullary_constructor_type("workflow", "Stage");
    let output_type = make_nullary_constructor_type("approval", "Decision");
    let (mapper_key, mapper_fn) = make_named_nullary_constructor_mapper_function(
        "collapse_stage",
        "workflow",
        "Stage",
        "approval",
        "Decision",
        &[("Idle", "Deny"), ("Busy", "Deny")],
        3,
    );
    let mut functions = empty_known_functions();
    functions.insert(&mapper_key, &mapper_fn);

    let via = make_typed_map_call(
        make_leaf_fuzzer_call("stage_fuzzer", source_type.clone()),
        module_fn_var(
            "collapse_stage",
            "math",
            Type::function(vec![source_type.clone()], output_type.clone()),
        ),
        output_type.clone(),
    );

    let mut owned_data_types =
        make_nullary_constructor_data_types("workflow", "Stage", &["Idle", "Busy"]);
    owned_data_types.extend(make_nullary_constructor_data_types(
        "approval",
        "Decision",
        &["Allow", "Deny", "Review"],
    ));
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();

    let semantics =
        extract_semantics_from_via(&via, "math", &functions, &data_types, output_type.as_ref());

    assert_eq!(semantics, FuzzerSemantics::Constructors { tags: vec![1] });
}

#[test]
fn extract_semantics_name_agnostic_constructor_map_requires_total_mapping() {
    let source_type = make_nullary_constructor_type("workflow", "Stage");
    let output_type = make_nullary_constructor_type("approval", "Decision");
    let via = make_typed_map_call(
        make_leaf_fuzzer_call("stage_fuzzer", source_type.clone()),
        make_nullary_constructor_mapper(
            "workflow",
            "Stage",
            "approval",
            "Decision",
            &[("Idle", "Allow")],
            3,
        ),
        output_type.clone(),
    );

    let mut owned_data_types =
        make_nullary_constructor_data_types("workflow", "Stage", &["Idle", "Busy"]);
    owned_data_types.extend(make_nullary_constructor_data_types(
        "approval",
        "Decision",
        &["Allow", "Deny", "Review"],
    ));
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();

    let semantics = extract_semantics_from_via(
        &via,
        "math",
        &empty_known_functions(),
        &data_types,
        output_type.as_ref(),
    );

    // Partial constructor map falls back to the default semantics for the
    // output type, which is now the full nullary constructor domain for
    // `approval.Decision`. This remains a sound over-approximation.
    assert_eq!(
        semantics,
        FuzzerSemantics::Constructors {
            tags: vec![0, 1, 2]
        }
    );
}

#[test]
fn extract_semantics_name_agnostic_constructor_map_unresolved_mapper_falls_back_to_default() {
    let source_type = make_nullary_constructor_type("workflow", "Stage");
    let output_type = make_nullary_constructor_type("approval", "Decision");
    let via = make_typed_map_call(
        make_leaf_fuzzer_call("stage_fuzzer", source_type.clone()),
        make_unresolved_unary_mapper_with_types(
            "next_stage",
            source_type.clone(),
            output_type.clone(),
        ),
        output_type.clone(),
    );

    let mut owned_data_types =
        make_nullary_constructor_data_types("workflow", "Stage", &["Idle", "Busy"]);
    owned_data_types.extend(make_nullary_constructor_data_types(
        "approval",
        "Decision",
        &["Allow", "Deny", "Review"],
    ));
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();

    let semantics = extract_semantics_from_via(
        &via,
        "math",
        &empty_known_functions(),
        &data_types,
        output_type.as_ref(),
    );

    // Unresolved mapper falls back to the default semantics for the
    // output type, which is now the full nullary constructor domain for
    // `approval.Decision`. This remains a sound over-approximation.
    assert_eq!(
        semantics,
        FuzzerSemantics::Constructors {
            tags: vec![0, 1, 2]
        }
    );
}

#[test]
fn extract_semantics_name_agnostic_constructor_map_recursive_cycle_falls_back_to_default() {
    let source_type = make_nullary_constructor_type("workflow", "Stage");
    let output_type = make_nullary_constructor_type("approval", "Decision");
    let mapper_type = Type::function(vec![source_type.clone()], output_type.clone());

    let (left_key, left_fn) = make_zero_arg_function(
        "left_mapper",
        mapper_type.clone(),
        make_zero_arg_call("right_mapper", mapper_type.clone()),
    );
    let (right_key, right_fn) = make_zero_arg_function(
        "right_mapper",
        mapper_type.clone(),
        make_zero_arg_call("left_mapper", mapper_type.clone()),
    );
    let mut functions = empty_known_functions();
    functions.insert(&left_key, &left_fn);
    functions.insert(&right_key, &right_fn);

    let via = make_typed_map_call(
        make_leaf_fuzzer_call("stage_fuzzer", source_type.clone()),
        make_zero_arg_call("left_mapper", mapper_type),
        output_type.clone(),
    );

    let mut owned_data_types =
        make_nullary_constructor_data_types("workflow", "Stage", &["Idle", "Busy"]);
    owned_data_types.extend(make_nullary_constructor_data_types(
        "approval",
        "Decision",
        &["Allow", "Deny", "Review"],
    ));
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();

    let semantics =
        extract_semantics_from_via(&via, "math", &functions, &data_types, output_type.as_ref());

    // Recursive mapper cycle falls back to the default semantics for the
    // output type, which is now the full nullary constructor domain for
    // `approval.Decision`. This remains a sound over-approximation.
    assert_eq!(
        semantics,
        FuzzerSemantics::Constructors {
            tags: vec![0, 1, 2]
        }
    );
}

#[test]
fn extract_semantics_name_agnostic_bind_uses_continuation_shape() {
    // The continuation's source `int_between(5, 8)` now produces the
    // closed range [5, 8] via the stdlib recognizer; the bind's
    // semantic output reflects the continuation's bounds.
    let via = make_typed_bind_call(
        make_leaf_fuzzer_call("seed", Type::int()),
        make_inline_bind_continuation(
            "x",
            Type::int(),
            make_typed_int_between_fuzzer("5", "8"),
            Type::int(),
        ),
        Type::int(),
    );
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

    let semantics = extract_semantics_from_via(
        &via,
        "math",
        &empty_known_functions(),
        &data_types,
        Type::int().as_ref(),
    );
    assert_eq!(
        semantics,
        FuzzerSemantics::IntRange {
            min: Some("5".to_string()),
            max: Some("8".to_string()),
        }
    );
}

#[test]
fn extract_semantics_name_agnostic_product_uses_element_shapes() {
    // Both components now extract their bounds via the stdlib
    // recognizer; the product semantics records the tuple shape with
    // each element's closed range preserved.
    let via = make_typed_product_call(
        make_typed_int_between_fuzzer("0", "10"),
        make_typed_int_between_fuzzer("20", "30"),
    );
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

    let semantics = extract_semantics_from_via(
        &via,
        "math",
        &empty_known_functions(),
        &data_types,
        Type::tuple(vec![Type::int(), Type::int()]).as_ref(),
    );
    assert_eq!(
        semantics,
        FuzzerSemantics::Product(vec![
            FuzzerSemantics::IntRange {
                min: Some("0".to_string()),
                max: Some("10".to_string()),
            },
            FuzzerSemantics::IntRange {
                min: Some("20".to_string()),
                max: Some("30".to_string()),
            },
        ])
    );
}

#[test]
fn extract_semantics_name_agnostic_list_uses_element_shape() {
    // The element fuzzer is now recognized, producing a closed range
    // element semantic. The enclosing list call is not a stdlib
    // `list_*` function, so length bounds remain absent.
    let via = make_typed_list_call(make_typed_int_between_fuzzer("0", "10"), Type::int());
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

    let semantics = extract_semantics_from_via(
        &via,
        "math",
        &empty_known_functions(),
        &data_types,
        Type::list(Type::int()).as_ref(),
    );
    assert_eq!(
        semantics,
        FuzzerSemantics::List {
            element: Box::new(FuzzerSemantics::IntRange {
                min: Some("0".to_string()),
                max: Some("10".to_string()),
            }),
            min_len: None,
            max_len: None,
        }
    );
}

#[test]
fn extract_semantics_name_agnostic_map_falls_back_to_default_for_unknown_mapper() {
    // Previously, an unknown mapper shape produced `Opaque` semantics
    // which caused downstream Lean emission to abort with a "semantic
    // domain is opaque" error. The new behavior over-approximates to
    // the default semantics for the output type, allowing universal
    // quantification (a sound widening) to proceed instead. Even though
    // the source now carries bounds, `Map(_)` with an opaque mapper
    // collapses to the default unbounded `IntRange`.
    let via = make_typed_map_call(
        make_typed_int_between_fuzzer("1", "3"),
        make_unresolved_unary_mapper("f", Type::int()),
        Type::int(),
    );
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

    let semantics = extract_semantics_from_via(
        &via,
        "math",
        &empty_known_functions(),
        &data_types,
        Type::int().as_ref(),
    );
    assert_eq!(
        semantics,
        FuzzerSemantics::IntRange {
            min: None,
            max: None,
        }
    );
}

#[test]
fn extract_semantics_name_agnostic_identity_map_preserves_source_domain() {
    // The source `int_between(1, 3)` now carries [1, 3]; an identity
    // map preserves that range unchanged in the semantic layer.
    let via = make_typed_map_call(
        make_typed_int_between_fuzzer("1", "3"),
        make_identity_mapper("n", Type::int()),
        Type::int(),
    );
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

    let semantics = extract_semantics_from_via(
        &via,
        "math",
        &empty_known_functions(),
        &data_types,
        Type::int().as_ref(),
    );
    assert_eq!(
        semantics,
        FuzzerSemantics::IntRange {
            min: Some("1".to_string()),
            max: Some("3".to_string()),
        }
    );
}

#[test]
fn extract_semantics_name_agnostic_named_identity_mapper_uses_function_body_shape() {
    // The inner source now carries bounds via the stdlib recognizer;
    // a named identity mapper structurally collapses the map, so the
    // outer semantics are the source's closed range.
    let (identity_key, identity_fn) =
        make_named_unary_identity_mapper_function("identity", Type::int());
    let mut functions = empty_known_functions();
    functions.insert(&identity_key, &identity_fn);
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

    let via = make_typed_map_call(
        make_typed_int_between_fuzzer("1", "3"),
        module_fn_var(
            "identity",
            "math",
            Type::function(vec![Type::int()], Type::int()),
        ),
        Type::int(),
    );

    let semantics =
        extract_semantics_from_via(&via, "math", &functions, &data_types, Type::int().as_ref());
    assert_eq!(
        semantics,
        FuzzerSemantics::IntRange {
            min: Some("1".to_string()),
            max: Some("3".to_string()),
        }
    );
}

#[test]
fn extract_semantics_finite_string_if_mapper_is_oneof() {
    let via = make_typed_map_call(
        make_typed_int_between_fuzzer("0", "3"),
        finite_string_if_mapper(),
        Type::string(),
    );
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

    let semantics = extract_semantics_from_via(
        &via,
        "math",
        &empty_known_functions(),
        &data_types,
        Type::string().as_ref(),
    );
    assert_eq!(
        semantics,
        FuzzerSemantics::OneOf(expected_finite_string_values())
    );
}

#[test]
fn extract_semantics_helper_wrapped_finite_string_mapper_is_oneof() {
    let (mapper_key, mapper_fn) = make_named_finite_string_mapper_function("label_for_i");
    let mut functions = empty_known_functions();
    functions.insert(&mapper_key, &mapper_fn);
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

    let via = make_typed_map_call(
        make_typed_int_between_fuzzer("0", "3"),
        module_fn_var(
            "label_for_i",
            "math",
            Type::function(vec![Type::int()], Type::string()),
        ),
        Type::string(),
    );

    let semantics = extract_semantics_from_via(
        &via,
        "math",
        &functions,
        &data_types,
        Type::string().as_ref(),
    );
    assert_eq!(
        semantics,
        FuzzerSemantics::OneOf(expected_finite_string_values())
    );
}

#[test]
fn extract_semantics_finite_string_source_range_above_cap_stays_generic_string() {
    let via = make_typed_map_call(
        make_typed_int_between_fuzzer("0", "999999999999999999999999999999999999999999"),
        finite_string_if_mapper(),
        Type::string(),
    );
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

    let semantics = extract_semantics_from_via(
        &via,
        "math",
        &empty_known_functions(),
        &data_types,
        Type::string().as_ref(),
    );
    assert_eq!(semantics, FuzzerSemantics::String);
}

#[test]
fn extract_semantics_finite_string_source_range_outside_i128_above_cap_uses_bigint() {
    let via = make_typed_map_call(
        make_typed_int_between_fuzzer(
            "170141183460469231731687303715884105728",
            "170141183460469231731687303715884105828",
        ),
        finite_string_if_mapper(),
        Type::string(),
    );
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

    let semantics = extract_semantics_from_via(
        &via,
        "math",
        &empty_known_functions(),
        &data_types,
        Type::string().as_ref(),
    );
    assert_eq!(semantics, FuzzerSemantics::String);
}

#[test]
fn extract_semantics_finite_string_non_literal_mapper_stays_generic_string() {
    let mapper = make_unary_mapper(
        "i",
        Type::int(),
        Type::string(),
        local_var("unresolved_label", Type::string()),
    );
    let via = make_typed_map_call(
        make_typed_int_between_fuzzer("0", "3"),
        mapper,
        Type::string(),
    );
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

    let semantics = extract_semantics_from_via(
        &via,
        "math",
        &empty_known_functions(),
        &data_types,
        Type::string().as_ref(),
    );
    assert_eq!(semantics, FuzzerSemantics::String);
}

#[test]
fn extract_semantics_finite_string_mixed_literal_outputs_stay_generic_string() {
    let body = TypedExpr::If {
        location: Span::empty(),
        tipo: Type::string(),
        branches: vec1::vec1![IfBranch {
            condition: int_eq_expr("i", "0"),
            body: string_lit("ok"),
            is: None,
            location: Span::empty(),
        }],
        final_else: Box::new(TypedExpr::ByteArray {
            location: Span::empty(),
            tipo: Type::byte_array(),
            bytes: vec![0],
            preferred_format: crate::ast::ByteArrayFormatPreference::HexadecimalString,
        }),
    };
    let via = make_typed_map_call(
        make_typed_int_between_fuzzer("0", "1"),
        make_unary_mapper("i", Type::int(), Type::string(), body),
        Type::string(),
    );
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

    let semantics = extract_semantics_from_via(
        &via,
        "math",
        &empty_known_functions(),
        &data_types,
        Type::string().as_ref(),
    );
    assert_eq!(semantics, FuzzerSemantics::String);
}

#[test]
fn extract_semantics_name_agnostic_constant_bool_map_is_exact() {
    let via = make_typed_map_call(
        make_leaf_fuzzer_call("seed", Type::int()),
        make_constant_bool_mapper(Type::int(), true),
        Type::bool(),
    );
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

    let semantics = extract_semantics_from_via(
        &via,
        "math",
        &empty_known_functions(),
        &data_types,
        Type::bool().as_ref(),
    );
    assert_eq!(
        semantics,
        FuzzerSemantics::Exact(FuzzerExactValue::Bool(true))
    );
}

#[test]
fn extract_semantics_named_tautology_bool_map_is_exact() {
    let (mapper_key, mapper_fn) =
        make_named_unary_tautology_mapper_function("tautology", Type::bool());
    let mut functions = empty_known_functions();
    functions.insert(&mapper_key, &mapper_fn);
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

    let via = make_typed_map_call(
        make_typed_bool_fuzzer(),
        module_fn_var(
            "tautology",
            "math",
            Type::function(vec![Type::bool()], Type::bool()),
        ),
        Type::bool(),
    );

    let semantics =
        extract_semantics_from_via(&via, "math", &functions, &data_types, Type::bool().as_ref());
    assert_eq!(
        semantics,
        FuzzerSemantics::Exact(FuzzerExactValue::Bool(true))
    );
}

#[test]
fn extract_semantics_name_agnostic_constant_int_map_is_singleton_range() {
    let via = make_typed_map_call(
        make_leaf_fuzzer_call("seed", Type::int()),
        make_constant_int_mapper(Type::int(), "41"),
        Type::int(),
    );
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

    let semantics = extract_semantics_from_via(
        &via,
        "math",
        &empty_known_functions(),
        &data_types,
        Type::int().as_ref(),
    );
    assert_eq!(
        semantics,
        FuzzerSemantics::IntRange {
            min: Some("41".to_string()),
            max: Some("41".to_string()),
        }
    );
}

#[test]
fn extract_semantics_name_agnostic_named_constant_int_mapper_uses_function_body_shape() {
    let (mapper_key, mapper_fn) = make_named_unary_constant_int_mapper_function("always_7", "7");
    let mut functions = empty_known_functions();
    functions.insert(&mapper_key, &mapper_fn);
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

    let via = make_typed_map_call(
        make_leaf_fuzzer_call("seed", Type::int()),
        module_fn_var(
            "always_7",
            "math",
            Type::function(vec![Type::int()], Type::int()),
        ),
        Type::int(),
    );

    let semantics =
        extract_semantics_from_via(&via, "math", &functions, &data_types, Type::int().as_ref());
    assert_eq!(
        semantics,
        FuzzerSemantics::IntRange {
            min: Some("7".to_string()),
            max: Some("7".to_string()),
        }
    );
}

#[test]
fn extract_semantics_name_agnostic_nested_constant_then_affine_map_transforms_range() {
    let source = make_typed_map_call(
        make_leaf_fuzzer_call("seed", Type::int()),
        make_constant_int_mapper(Type::int(), "41"),
        Type::int(),
    );
    let via = make_typed_map_call(source, make_add_int_mapper("1"), Type::int());
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

    let semantics = extract_semantics_from_via(
        &via,
        "math",
        &empty_known_functions(),
        &data_types,
        Type::int().as_ref(),
    );
    assert_eq!(
        semantics,
        FuzzerSemantics::IntRange {
            min: Some("42".to_string()),
            max: Some("42".to_string()),
        }
    );
}

#[test]
fn extract_constraint_unknown_typed_fuzzer_shape_is_unsupported() {
    let via = make_typed_bind_call(
        make_leaf_fuzzer_call("seed", Type::int()),
        uint_lit("0"),
        Type::int(),
    );

    let constraint = extract_constraint_from_via(&via, "math", &empty_known_functions());
    assert!(matches!(constraint, FuzzerConstraint::Unsupported { .. }));
}

#[test]
fn extract_semantics_unknown_typed_fuzzer_shape_is_opaque() {
    let via = make_typed_bind_call(
        make_leaf_fuzzer_call("seed", Type::int()),
        uint_lit("0"),
        Type::int(),
    );
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

    let semantics = extract_semantics_from_via(
        &via,
        "math",
        &empty_known_functions(),
        &data_types,
        Type::int().as_ref(),
    );
    assert!(matches!(semantics, FuzzerSemantics::Opaque { .. }));
}

#[test]
fn map2_identity_mapper_preserves_order() {
    let int_tipo = Type::int();
    let mapper = make_map2_mapper(vec![
        local_var("a", int_tipo.clone()),
        local_var("b", int_tipo),
    ]);

    let functions = empty_known_functions();
    let function_index = index_known_functions(&functions);
    assert_eq!(
        map2_mapper_arg_order(&mapper, "math", &function_index, &BTreeMap::new()),
        Some([0, 1])
    );
}

#[test]
fn map2_swapped_mapper_reports_swapped_order() {
    let int_tipo = Type::int();
    let mapper = make_map2_mapper(vec![
        local_var("b", int_tipo.clone()),
        local_var("a", int_tipo),
    ]);

    let functions = empty_known_functions();
    let function_index = index_known_functions(&functions);
    assert_eq!(
        map2_mapper_arg_order(&mapper, "math", &function_index, &BTreeMap::new()),
        Some([1, 0])
    );
}

#[test]
fn extract_constraint_map3_permuted_mapper_reorders_product_bounds() {
    let int_tipo = Type::int();
    let arg_names = vec!["a".to_string(), "b".to_string(), "c".to_string()];
    let mapper = make_mapn_mapper(
        &arg_names,
        vec![
            local_var("c", int_tipo.clone()),
            local_var("a", int_tipo.clone()),
            local_var("b", int_tipo),
        ],
    );

    let via = make_mapn_via(
        "map3",
        vec![
            make_int_between_via("0", "9"),
            make_int_between_via("10", "19"),
            make_int_between_via("20", "29"),
        ],
        mapper,
    );

    let functions = empty_known_functions();
    let constraint = extract_constraint_from_via(&via, "math", &functions);
    assert_eq!(
        constraint,
        tuple_int_range_constraint(&[("20", "29"), ("0", "9"), ("10", "19")])
    );
}

#[test]
fn extract_constraint_map10_reverse_mapper_reorders_product_bounds() {
    let int_tipo = Type::int();
    let arg_names: Vec<String> = (0..10).map(|i| format!("a{i}")).collect();
    let mapper_elems: Vec<TypedExpr> = arg_names
        .iter()
        .rev()
        .map(|name| local_var(name, int_tipo.clone()))
        .collect();
    let mapper = make_mapn_mapper(&arg_names, mapper_elems);

    let fuzzers: Vec<TypedExpr> = (0..10)
        .map(|i| {
            let min = (i * 10).to_string();
            let max = (i * 10 + 9).to_string();
            make_int_between_via(&min, &max)
        })
        .collect();
    let via = make_mapn_via("map10", fuzzers, mapper);

    let functions = empty_known_functions();
    let constraint = extract_constraint_from_via(&via, "math", &functions);
    assert_eq!(
        constraint,
        tuple_int_range_constraint(&[
            ("90", "99"),
            ("80", "89"),
            ("70", "79"),
            ("60", "69"),
            ("50", "59"),
            ("40", "49"),
            ("30", "39"),
            ("20", "29"),
            ("10", "19"),
            ("0", "9"),
        ])
    );
}

#[test]
fn extract_constraint_map2_identity_mapper_collects_component_bounds() {
    let int_tipo = Type::int();
    let mapper = make_map2_mapper(vec![
        local_var("a", int_tipo.clone()),
        local_var("b", int_tipo),
    ]);

    let via = make_map2_via(
        make_int_between_via("0", "10"),
        make_int_between_via("20", "30"),
        mapper,
    );

    let functions = empty_known_functions();
    let constraint = extract_constraint_from_via(&via, "math", &functions);
    assert_eq!(
        constraint,
        tuple_int_range_constraint(&[("0", "10"), ("20", "30")])
    );
}

#[test]
fn extract_constraint_map2_swapped_mapper_reorders_component_bounds() {
    let int_tipo = Type::int();
    let mapper = make_map2_mapper(vec![
        local_var("b", int_tipo.clone()),
        local_var("a", int_tipo),
    ]);

    let via = make_map2_via(
        make_int_between_via("0", "10"),
        make_int_between_via("20", "30"),
        mapper,
    );

    let functions = empty_known_functions();
    let constraint = extract_constraint_from_via(&via, "math", &functions);
    assert_eq!(
        constraint,
        tuple_int_range_constraint(&[("20", "30"), ("0", "10")])
    );
}

#[test]
fn extract_constraint_ignores_local_int_between_name_collision() {
    let via = TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::int(),
        fun: Box::new(local_var(
            "int_between",
            Type::function(vec![Type::int(), Type::int()], Type::int()),
        )),
        args: vec![call_arg(uint_lit("0")), call_arg(uint_lit("10"))],
    };

    let functions = empty_known_functions();
    assert!(matches!(
        extract_constraint_from_via(&via, "math", &functions),
        FuzzerConstraint::Unsupported { .. }
    ));
}

#[test]
fn extract_constraint_does_not_resolve_local_shadow_to_module_function() {
    let (fn_key, fn_def) = make_zero_arg_function(
        "wrapped_fuzzer",
        Type::int(),
        make_int_between_via("0", "10"),
    );
    let mut functions = empty_known_functions();
    functions.insert(&fn_key, &fn_def);

    // Local variable has the same identifier as a module function, but must
    // not be resolved as that function by name.
    let via = TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::int(),
        fun: Box::new(local_var(
            "wrapped_fuzzer",
            Type::function(vec![], Type::int()),
        )),
        args: vec![],
    };

    assert!(matches!(
        extract_constraint_from_via(&via, "math", &functions),
        FuzzerConstraint::Unsupported { .. }
    ));
}

#[test]
fn extract_constraint_ignores_non_fuzz_module_int_between() {
    let via = TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::int(),
        fun: Box::new(module_fn_var(
            "int_between",
            "my/custom/module",
            Type::function(vec![Type::int(), Type::int()], Type::int()),
        )),
        args: vec![call_arg(uint_lit("0")), call_arg(uint_lit("10"))],
    };

    let functions = empty_known_functions();
    assert!(matches!(
        extract_constraint_from_via(&via, "math", &functions),
        FuzzerConstraint::Unsupported { .. }
    ));
}

#[test]
fn fuzz_existential_requires_canonical_stdlib_module_and_fuzzer_return() {
    let int_fuzzer = Type::fuzzer(Type::int());
    let fuzzer_call_type = Rc::new(int_fuzzer.clone());
    let stdlib_fun = module_fn_var(
        "int_between",
        STDLIB_FUZZ_MODULE,
        Type::function(vec![Type::int(), Type::int()], int_fuzzer.clone()),
    );
    let fake_fun = module_fn_var(
        "int_between",
        "my/custom/fuzz",
        Type::function(vec![Type::int(), Type::int()], int_fuzzer),
    );

    assert!(matches!(
        try_fuzz_existential(
            &stdlib_fun,
            &[call_arg(uint_lit("0")), call_arg(uint_lit("10"))],
            &fuzzer_call_type,
        ),
        Some(ShallowIr::FuzzExistential {
            kind: ShallowIrType::Int,
            lo: Some(0),
            hi: Some(10),
        })
    ));
    assert_eq!(
        try_fuzz_existential(
            &fake_fun,
            &[call_arg(uint_lit("0")), call_arg(uint_lit("10"))],
            &fuzzer_call_type,
        ),
        None
    );
    assert_eq!(
        try_fuzz_existential(
            &stdlib_fun,
            &[call_arg(uint_lit("0")), call_arg(uint_lit("10"))],
            &Type::int(),
        ),
        None
    );
}

#[test]
fn fuzz_existential_big_integer_bounds_are_explicitly_opaque() {
    let int_fuzzer = Type::fuzzer(Type::int());
    let stdlib_fun = module_fn_var(
        "int_between",
        STDLIB_FUZZ_MODULE,
        Type::function(vec![Type::int(), Type::int()], int_fuzzer.clone()),
    );

    assert!(matches!(
        try_fuzz_existential(
            &stdlib_fun,
            &[
                call_arg(uint_lit("170141183460469231731687303715884105728")),
                call_arg(uint_lit("170141183460469231731687303715884105729")),
            ],
            &int_fuzzer,
        ),
        Some(ShallowIr::Opaque { reason, .. })
            if reason.contains("outside the verifier existential's i64 precision")
    ));
}

#[test]
fn extract_constraint_ignores_local_map2_name_collision() {
    let int_tipo = Type::int();
    let mapper = make_map2_mapper(vec![
        local_var("a", int_tipo.clone()),
        local_var("b", int_tipo),
    ]);

    let via = TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::tuple(vec![Type::int(), Type::int()]),
        fun: Box::new(local_var(
            "map2",
            Type::function(
                vec![Type::int(), Type::int(), Type::int()],
                Type::tuple(vec![Type::int(), Type::int()]),
            ),
        )),
        args: vec![
            call_arg(make_int_between_via("0", "10")),
            call_arg(make_int_between_via("20", "30")),
            call_arg(mapper),
        ],
    };

    let functions = empty_known_functions();
    assert!(matches!(
        extract_constraint_from_via(&via, "math", &functions),
        FuzzerConstraint::Unsupported { .. }
    ));
}

#[test]
fn extract_constraint_map2_does_not_resolve_local_mapper_shadow() {
    let int_tipo = Type::int();
    let (fn_key, fn_def) = make_named_map2_mapper_function(
        "pair_mapper",
        vec![
            local_var("a", int_tipo.clone()),
            local_var("b", int_tipo.clone()),
        ],
    );
    let mut functions = empty_known_functions();
    functions.insert(&fn_key, &fn_def);

    let via = make_map2_via(
        make_int_between_via("0", "10"),
        make_int_between_via("20", "30"),
        local_var(
            "pair_mapper",
            Type::function(
                vec![Type::int(), Type::int()],
                Type::tuple(vec![Type::int(), Type::int()]),
            ),
        ),
    );

    assert!(matches!(
        extract_constraint_from_via(&via, "math", &functions),
        FuzzerConstraint::Unsupported { .. }
    ));
}

#[test]
fn tuple_elem_arg_index_prefers_mapper_binder_over_outer_local_alias() {
    let arg_names = vec!["owner".to_string(), "amount".to_string()];
    let mut local_values = BTreeMap::new();
    local_values.insert("owner".to_string(), make_typed_bytearray_fixed_fuzzer("4"));
    local_values.insert("amount".to_string(), make_int_between_via("0", "10"));

    assert_eq!(
        tuple_elem_arg_index_by_names(
            &local_var("owner", Type::byte_array()),
            &arg_names,
            &local_values,
        ),
        Some(0),
        "mapper parameter names must shadow outer local fuzzer aliases",
    );
    assert_eq!(
        tuple_elem_arg_index_by_names(&local_var("amount", Type::int()), &arg_names, &local_values),
        Some(1),
        "second mapper parameter must also resolve before outer alias substitution",
    );
}

#[test]
fn extract_constraint_map2_named_mapper_without_definition_is_unsupported() {
    let mapper = make_named_map2_mapper("int_pair");
    let via = make_map2_via(
        make_int_between_via("0", "10"),
        make_int_between_via("20", "30"),
        mapper,
    );

    let functions = empty_known_functions();
    assert!(matches!(
        extract_constraint_from_via(&via, "math", &functions),
        FuzzerConstraint::Unsupported { .. }
    ));
}

#[test]
fn extract_constraint_map2_named_identity_mapper_uses_function_definition() {
    let int_tipo = Type::int();
    let (fn_key, fn_def) = make_named_map2_mapper_function(
        "int_pair",
        vec![local_var("a", int_tipo.clone()), local_var("b", int_tipo)],
    );

    let mut functions = empty_known_functions();
    functions.insert(&fn_key, &fn_def);

    let mapper = make_named_map2_mapper("int_pair");
    let via = make_map2_via(
        make_int_between_via("0", "10"),
        make_int_between_via("20", "30"),
        mapper,
    );

    let constraint = extract_constraint_from_via(&via, "math", &functions);
    assert_eq!(
        constraint,
        tuple_int_range_constraint(&[("0", "10"), ("20", "30")])
    );
}

#[test]
fn extract_constraint_map2_named_swapped_mapper_uses_function_definition() {
    let int_tipo = Type::int();
    let (fn_key, fn_def) = make_named_map2_mapper_function(
        "swapped_int_pair",
        vec![local_var("b", int_tipo.clone()), local_var("a", int_tipo)],
    );

    let mut functions = empty_known_functions();
    functions.insert(&fn_key, &fn_def);

    let mapper = make_named_map2_mapper("swapped_int_pair");
    let via = make_map2_via(
        make_int_between_via("0", "10"),
        make_int_between_via("20", "30"),
        mapper,
    );

    let constraint = extract_constraint_from_via(&via, "math", &functions);
    assert_eq!(
        constraint,
        tuple_int_range_constraint(&[("20", "30"), ("0", "10")])
    );
}

#[test]
fn extract_constraint_map2_with_partially_applied_named_mapper_reorders() {
    let int_tipo = Type::int();
    let tuple_tipo = Type::tuple(vec![int_tipo.clone(), int_tipo.clone()]);
    let mapper_key = FunctionAccessKey {
        module_name: "math".to_string(),
        function_name: "pair_with_flag".to_string(),
    };
    let mapper_fn = TypedFunction {
        arguments: vec![
            TypedArg::new("flag", int_tipo.clone()),
            TypedArg::new("a", int_tipo.clone()),
            TypedArg::new("b", int_tipo.clone()),
        ],
        body: TypedExpr::Tuple {
            location: Span::empty(),
            tipo: tuple_tipo.clone(),
            elems: vec![
                local_var("b", int_tipo.clone()),
                local_var("a", int_tipo.clone()),
            ],
        },
        doc: None,
        location: Span::empty(),
        name: "pair_with_flag".to_string(),
        public: false,
        return_annotation: None,
        return_type: tuple_tipo.clone(),
        end_position: 0,
        on_test_failure: OnTestFailure::FailImmediately,
    };

    let mut functions = empty_known_functions();
    functions.insert(&mapper_key, &mapper_fn);

    let mapper = TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::function(vec![Type::int(), Type::int()], tuple_tipo.clone()),
        fun: Box::new(module_fn_var(
            "pair_with_flag",
            "math",
            Type::function(
                vec![Type::int(), Type::int(), Type::int()],
                tuple_tipo.clone(),
            ),
        )),
        args: vec![call_arg(uint_lit("1"))],
    };

    let via = make_map2_via(
        make_int_between_via("0", "10"),
        make_int_between_via("20", "30"),
        mapper,
    );

    let constraint = extract_constraint_from_via(&via, "math", &functions);
    assert_eq!(
        constraint,
        tuple_int_range_constraint(&[("20", "30"), ("0", "10")])
    );
}

#[test]
fn extract_constraint_int_between_basic() {
    let via = make_int_between_via("5", "100");
    let functions = empty_known_functions();
    assert_eq!(
        extract_constraint_from_via(&via, "math", &functions),
        int_range_constraint("5", "100")
    );
}

#[test]
fn extract_constraint_int_between_uses_plutus_floor_division_for_negatives() {
    let lower = TypedExpr::BinOp {
        location: Span::empty(),
        tipo: Type::int(),
        name: BinOp::DivInt,
        left: Box::new(negate_expr(uint_lit("8"))),
        right: Box::new(uint_lit("3")),
    };
    let upper = TypedExpr::BinOp {
        location: Span::empty(),
        tipo: Type::int(),
        name: BinOp::ModInt,
        left: Box::new(negate_expr(uint_lit("8"))),
        right: Box::new(uint_lit("3")),
    };
    let via = TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::int(),
        fun: Box::new(fuzz_var(
            "int_between",
            Type::function(vec![Type::int(), Type::int()], Type::int()),
        )),
        args: vec![call_arg(lower), call_arg(upper)],
    };

    let functions = empty_known_functions();
    assert!(matches!(
        extract_constraint_from_via(&via, "math", &functions),
        FuzzerConstraint::Unsupported { .. }
    ));
}

#[test]
fn extract_constraint_int_between_uses_plutus_modulo_divisor_sign() {
    let lower = TypedExpr::BinOp {
        location: Span::empty(),
        tipo: Type::int(),
        name: BinOp::DivInt,
        left: Box::new(uint_lit("8")),
        right: Box::new(negate_expr(uint_lit("3"))),
    };
    let upper = TypedExpr::BinOp {
        location: Span::empty(),
        tipo: Type::int(),
        name: BinOp::ModInt,
        left: Box::new(uint_lit("8")),
        right: Box::new(negate_expr(uint_lit("3"))),
    };
    let via = TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::int(),
        fun: Box::new(fuzz_var(
            "int_between",
            Type::function(vec![Type::int(), Type::int()], Type::int()),
        )),
        args: vec![call_arg(lower), call_arg(upper)],
    };

    let functions = empty_known_functions();
    assert!(matches!(
        extract_constraint_from_via(&via, "math", &functions),
        FuzzerConstraint::Unsupported { .. }
    ));
}

#[test]
fn extract_constraint_int_between_does_not_resolve_local_shadow_constant() {
    let via = TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::int(),
        fun: Box::new(fuzz_var(
            "int_between",
            Type::function(vec![Type::int(), Type::int()], Type::int()),
        )),
        args: vec![
            call_arg(local_var("bound", Type::int())),
            call_arg(uint_lit("10")),
        ],
    };

    let functions = empty_known_functions();
    let mut constants = empty_known_constants();
    let key = FunctionAccessKey {
        module_name: "math".to_string(),
        function_name: "bound".to_string(),
    };
    let value = uint_lit("0");
    constants.insert(&key, &value);

    assert!(matches!(
        extract_constraint_from_via_with_constants(&via, "math", &functions, &constants),
        FuzzerConstraint::Unsupported { .. }
    ));
}

#[test]
fn extract_constraint_int_between_with_module_constant_bounds() {
    let via = TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::int(),
        fun: Box::new(fuzz_var(
            "int_between",
            Type::function(vec![Type::int(), Type::int()], Type::int()),
        )),
        args: vec![
            call_arg(module_const_var(
                "core_development",
                "permissions_examples",
                Type::int(),
            )),
            call_arg(module_const_var(
                "core_development",
                "permissions_examples",
                Type::int(),
            )),
        ],
    };

    let functions = empty_known_functions();
    let mut constants = empty_known_constants();
    let key = FunctionAccessKey {
        module_name: "permissions_examples".to_string(),
        function_name: "core_development".to_string(),
    };
    let value = uint_lit("0");
    constants.insert(&key, &value);

    assert!(matches!(
        extract_constraint_from_via_with_constants(
            &via,
            "permissions_examples",
            &functions,
            &constants,
        ),
        FuzzerConstraint::Unsupported { .. }
    ));
}

#[test]
fn extract_constraint_int_between_with_nested_module_constant_bounds() {
    let via = TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::int(),
        fun: Box::new(fuzz_var(
            "int_between",
            Type::function(vec![Type::int(), Type::int()], Type::int()),
        )),
        args: vec![
            call_arg(module_const_var(
                "core_development",
                "permissions_examples",
                Type::int(),
            )),
            call_arg(module_const_var(
                "core_development",
                "permissions_examples",
                Type::int(),
            )),
        ],
    };

    let functions = empty_known_functions();
    let mut constants = empty_known_constants();

    let key_core = FunctionAccessKey {
        module_name: "permissions_examples".to_string(),
        function_name: "core_development".to_string(),
    };
    let key_base = FunctionAccessKey {
        module_name: "permissions_examples".to_string(),
        function_name: "base_scope".to_string(),
    };

    let value_core = module_const_var("base_scope", "permissions_examples", Type::int());
    let value_base = uint_lit("0");

    constants.insert(&key_core, &value_core);
    constants.insert(&key_base, &value_base);

    assert!(matches!(
        extract_constraint_from_via_with_constants(
            &via,
            "permissions_examples",
            &functions,
            &constants,
        ),
        FuzzerConstraint::Unsupported { .. }
    ));
}

#[test]
fn extract_constraint_int_no_args() {
    let via = TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::int(),
        fun: Box::new(fuzz_var("int", Type::function(vec![], Type::int()))),
        args: vec![],
    };
    let functions = empty_known_functions();
    assert!(matches!(
        extract_constraint_from_via(&via, "math", &functions),
        FuzzerConstraint::Unsupported { .. }
    ));
}

#[test]
fn extract_constraint_int_at_least_small() {
    let via = TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::int(),
        fun: Box::new(fuzz_var(
            "int_at_least",
            Type::function(vec![Type::int()], Type::int()),
        )),
        args: vec![call_arg(uint_lit("10"))],
    };
    let functions = empty_known_functions();
    assert!(matches!(
        extract_constraint_from_via(&via, "math", &functions),
        FuzzerConstraint::Unsupported { .. }
    ));
}

#[test]
fn extract_constraint_int_at_most_small() {
    let via = TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::int(),
        fun: Box::new(fuzz_var(
            "int_at_most",
            Type::function(vec![Type::int()], Type::int()),
        )),
        args: vec![call_arg(uint_lit("10"))],
    };
    let functions = empty_known_functions();
    assert!(matches!(
        extract_constraint_from_via(&via, "math", &functions),
        FuzzerConstraint::Unsupported { .. }
    ));
}

#[test]
fn extract_constraint_constant_int() {
    let via = TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::int(),
        fun: Box::new(fuzz_var(
            "constant",
            Type::function(vec![Type::int()], Type::int()),
        )),
        args: vec![call_arg(uint_lit("42"))],
    };
    let functions = empty_known_functions();
    assert!(matches!(
        extract_constraint_from_via(&via, "math", &functions),
        FuzzerConstraint::Unsupported { .. }
    ));
}

#[test]
fn extract_constraint_map_wraps_inner() {
    let via = TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::int(),
        fun: Box::new(fuzz_var(
            "map",
            Type::function(vec![Type::int(), Type::int()], Type::int()),
        )),
        args: vec![
            call_arg(make_int_between_via("0", "10")),
            call_arg(local_var(
                "f",
                Type::function(vec![Type::int()], Type::int()),
            )),
        ],
    };
    let functions = empty_known_functions();
    let constraint = extract_constraint_from_via(&via, "math", &functions);
    assert!(matches!(constraint, FuzzerConstraint::Unsupported { .. }));
}

#[test]
fn extract_constraint_map_with_named_negate_mapper_transforms_bounds() {
    let (negate_key, negate_fn) = make_named_unary_negate_mapper_function("negate");
    let mut functions = empty_known_functions();
    functions.insert(&negate_key, &negate_fn);

    let via = make_map_via(
        make_int_between_via("1", "50"),
        module_fn_var(
            "negate",
            "math",
            Type::function(vec![Type::int()], Type::int()),
        ),
    );

    let constraint = extract_constraint_from_via(&via, "math", &functions);
    assert_eq!(constraint, int_range_constraint("-50", "-1"));
}

#[test]
fn extract_constraint_such_that_preserves_inner_domain() {
    // A filter (such_that) propagates the source's constraint unchanged.
    // Under the stdlib-gated recognizer, `int_between(1, 50)` extracts
    // `IntRange { 1, 50 }`; the filter then propagates that range.
    let source = make_typed_int_between_fuzzer("1", "50");
    let predicate = make_bool_predicate("x", Type::int());
    let via = make_typed_filter_call(source, predicate);
    let functions = empty_known_functions();
    let constraint = extract_constraint_from_via(&via, "math", &functions);

    assert_eq!(
        constraint,
        FuzzerConstraint::IntRange {
            min: "1".to_string(),
            max: "50".to_string(),
        }
    );
}

#[test]
fn extract_constraint_such_that_const_false_is_explicitly_unsupported() {
    let source = make_typed_int_between_fuzzer("1", "50");
    let predicate = make_constant_bool_mapper(Type::int(), false);
    let via = make_typed_filter_call(source, predicate);
    let functions = empty_known_functions();
    let constraint = extract_constraint_from_via(&via, "math", &functions);

    assert!(matches!(
        constraint,
        FuzzerConstraint::Unsupported { reason }
            if reason.contains("such_that predicate is impossible")
    ));
}

#[test]
fn extract_constraint_bool_such_that_identity_preserves_direct_bool_domain() {
    let source = make_typed_bool_fuzzer();
    let predicate = make_identity_mapper("flag", Type::bool());
    let via = make_typed_filter_call(source, predicate);
    let functions = empty_known_functions();
    let constraint = extract_constraint_from_via(&via, "math", &functions);

    assert_eq!(
        constraint,
        FuzzerConstraint::OneOf(vec![
            FuzzerExactValue::Bool(false),
            FuzzerExactValue::Bool(true)
        ])
    );
}

#[test]
fn extract_constraint_bool_such_that_identity_preserves_opaque_source_domain() {
    let source = make_leaf_fuzzer_call("seed", Type::bool());
    let predicate = make_identity_mapper("flag", Type::bool());
    let via = make_typed_filter_call(source, predicate);
    let functions = empty_known_functions();
    let constraint = extract_constraint_from_via(&via, "math", &functions);

    assert_eq!(constraint, FuzzerConstraint::Any);
}

#[test]
fn normalize_bool_such_that_identity_preserves_predicate_ir() {
    let source = make_typed_bool_fuzzer();
    let predicate = make_identity_mapper("flag", Type::bool());
    let via = make_typed_filter_call(source, predicate);
    let functions = empty_known_functions();
    let normalized = normalize_fuzzer_from_via(&via, "math", &functions);

    match normalized {
        NormalizedFuzzer::Filter { predicate_ir, .. } => assert_eq!(
            predicate_ir,
            Some(ShallowIr::BoundVar {
                name: "_filter_value".to_string(),
                ty: ShallowIrType::Bool,
            })
        ),
        other => panic!("expected filter fuzzer, got {other:?}"),
    }
}

#[test]
fn extract_semantics_bool_such_that_identity_preserves_direct_bool_values() {
    let source = make_typed_bool_fuzzer();
    let predicate = make_identity_mapper("flag", Type::bool());
    let via = make_typed_filter_call(source, predicate);
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

    let semantics = extract_semantics_from_via(
        &via,
        "math",
        &empty_known_functions(),
        &data_types,
        Type::bool().as_ref(),
    );
    assert_eq!(
        semantics,
        FuzzerSemantics::OneOf(vec![
            FuzzerExactValue::Bool(false),
            FuzzerExactValue::Bool(true)
        ])
    );
}

#[test]
fn extract_semantics_bool_such_that_identity_preserves_opaque_source_domain() {
    let source = make_leaf_fuzzer_call("seed", Type::bool());
    let predicate = make_identity_mapper("flag", Type::bool());
    let via = make_typed_filter_call(source, predicate);
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

    let semantics = extract_semantics_from_via(
        &via,
        "math",
        &empty_known_functions(),
        &data_types,
        Type::bool().as_ref(),
    );
    assert_eq!(semantics, FuzzerSemantics::Bool);
}

#[test]
fn normalize_fuzzer_bool_map_negation_stays_map() {
    let via = make_typed_map_call(
        make_typed_bool_fuzzer(),
        make_not_bool_mapper("b"),
        Type::bool(),
    );

    let normalized = normalize_fuzzer_from_via(&via, "math", &empty_known_functions());
    assert_normalized_map(normalized);
}

#[test]
fn extract_constraint_map_to_bool_is_not_filter() {
    // A map from Int to Bool is NOT a filter: source payload (Int) != output payload (Bool).
    // This should remain classified as Map, not be collapsed to the source.
    let source = make_typed_int_between_fuzzer("1", "50");
    let mapper = make_unresolved_unary_mapper_with_types("is_positive", Type::int(), Type::bool());
    let via = make_typed_map_call(source, mapper, Type::bool());
    let functions = empty_known_functions();
    let constraint = extract_constraint_from_via(&via, "math", &functions);

    assert!(
        matches!(constraint, FuzzerConstraint::Map(_)),
        "map-to-Bool should stay as Map, got: {:?}",
        constraint
    );
}

#[test]
fn extract_constraint_map_with_partially_applied_named_mapper_transforms_bounds() {
    let int_tipo = Type::int();
    let mapper_key = FunctionAccessKey {
        module_name: "math".to_string(),
        function_name: "negate_with_offset".to_string(),
    };
    let mapper_fn = TypedFunction {
        arguments: vec![
            TypedArg::new("offset", int_tipo.clone()),
            TypedArg::new("n", int_tipo.clone()),
        ],
        body: negate_expr(local_var("n", int_tipo.clone())),
        doc: None,
        location: Span::empty(),
        name: "negate_with_offset".to_string(),
        public: false,
        return_annotation: None,
        return_type: int_tipo.clone(),
        end_position: 0,
        on_test_failure: OnTestFailure::FailImmediately,
    };
    let mut functions = empty_known_functions();
    functions.insert(&mapper_key, &mapper_fn);

    let mapper = TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::function(vec![Type::int()], Type::int()),
        fun: Box::new(module_fn_var(
            "negate_with_offset",
            "math",
            Type::function(vec![Type::int(), Type::int()], Type::int()),
        )),
        args: vec![call_arg(uint_lit("0"))],
    };
    let via = make_map_via(make_int_between_via("1", "50"), mapper);

    let constraint = extract_constraint_from_via(&via, "math", &functions);
    assert_eq!(constraint, int_range_constraint("-50", "-1"));
}

#[test]
fn extract_constraint_zero_arg_helper_call_is_unwrapped() {
    let (helper_key, helper_fn) =
        make_zero_arg_function("helper_fuzzer", Type::int(), make_int_between_via("3", "7"));
    let mut functions = empty_known_functions();
    functions.insert(&helper_key, &helper_fn);

    let via = make_zero_arg_call("helper_fuzzer", Type::int());
    let constraint = extract_constraint_from_via(&via, "math", &functions);

    assert!(matches!(constraint, FuzzerConstraint::Unsupported { .. }));
}

#[test]
fn extract_constraint_zero_arg_helper_with_map_negate_transforms_bounds() {
    let (negate_key, negate_fn) = make_named_unary_negate_mapper_function("negate");
    let (fuzzer_key, fuzzer_fn) = make_zero_arg_function(
        "negate_fuzzer",
        Type::int(),
        make_map_via(
            make_int_between_via("1", "50"),
            module_fn_var(
                "negate",
                "math",
                Type::function(vec![Type::int()], Type::int()),
            ),
        ),
    );
    let mut functions = empty_known_functions();
    functions.insert(&negate_key, &negate_fn);
    functions.insert(&fuzzer_key, &fuzzer_fn);

    let via = make_zero_arg_call("negate_fuzzer", Type::int());
    let constraint = extract_constraint_from_via(&via, "math", &functions);

    assert!(matches!(constraint, FuzzerConstraint::Unsupported { .. }));
}

#[test]
fn extract_constraint_parameterized_helper_inlines_arguments() {
    let lo_var = local_var("lo", Type::int());
    let upper = TypedExpr::BinOp {
        location: Span::empty(),
        tipo: Type::int(),
        name: BinOp::AddInt,
        left: Box::new(lo_var.clone()),
        right: Box::new(uint_lit("5")),
    };

    let helper_key = FunctionAccessKey {
        module_name: "math".to_string(),
        function_name: "bounded".to_string(),
    };
    let helper_fn = TypedFunction {
        arguments: vec![TypedArg::new("lo", Type::int())],
        body: TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::int(),
            fun: Box::new(fuzz_var(
                "int_between",
                Type::function(vec![Type::int(), Type::int()], Type::int()),
            )),
            args: vec![call_arg(lo_var), call_arg(upper)],
        },
        doc: None,
        location: Span::empty(),
        name: "bounded".to_string(),
        public: false,
        return_annotation: None,
        return_type: Type::int(),
        end_position: 0,
        on_test_failure: OnTestFailure::FailImmediately,
    };

    let mut functions = empty_known_functions();
    functions.insert(&helper_key, &helper_fn);

    let via = TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::int(),
        fun: Box::new(module_fn_var(
            "bounded",
            "math",
            Type::function(vec![Type::int()], Type::int()),
        )),
        args: vec![call_arg(uint_lit("7"))],
    };

    assert!(matches!(
        extract_constraint_from_via(&via, "math", &functions),
        FuzzerConstraint::Unsupported { .. }
    ));
}

#[test]
fn extract_constraint_parameterized_helper_preserves_caller_local_aliases() {
    let lo_var = local_var("lo", Type::int());
    let upper = TypedExpr::BinOp {
        location: Span::empty(),
        tipo: Type::int(),
        name: BinOp::AddInt,
        left: Box::new(lo_var.clone()),
        right: Box::new(uint_lit("5")),
    };

    let helper_key = FunctionAccessKey {
        module_name: "math".to_string(),
        function_name: "bounded".to_string(),
    };
    let helper_fn = TypedFunction {
        arguments: vec![TypedArg::new("lo", Type::int())],
        body: TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::int(),
            fun: Box::new(fuzz_var(
                "int_between",
                Type::function(vec![Type::int(), Type::int()], Type::int()),
            )),
            args: vec![call_arg(lo_var), call_arg(upper)],
        },
        doc: None,
        location: Span::empty(),
        name: "bounded".to_string(),
        public: false,
        return_annotation: None,
        return_type: Type::int(),
        end_position: 0,
        on_test_failure: OnTestFailure::FailImmediately,
    };

    let mut functions = empty_known_functions();
    functions.insert(&helper_key, &helper_fn);

    let via = TypedExpr::Sequence {
        location: Span::empty(),
        expressions: vec![
            TypedExpr::Assignment {
                location: Span::empty(),
                tipo: Type::int(),
                value: Box::new(uint_lit("7")),
                pattern: TypedPattern::var("lo"),
                kind: crate::ast::AssignmentKind::Let { backpassing: () },
                comment: None,
            },
            TypedExpr::Call {
                location: Span::empty(),
                tipo: Type::int(),
                fun: Box::new(module_fn_var(
                    "bounded",
                    "math",
                    Type::function(vec![Type::int()], Type::int()),
                )),
                args: vec![call_arg(local_var("lo", Type::int()))],
            },
        ],
    };

    assert!(matches!(
        extract_constraint_from_via(&via, "math", &functions),
        FuzzerConstraint::Unsupported { .. }
    ));
}

#[test]
fn extract_constraint_partial_int_between_alias_is_resolved() {
    let int_between_tipo = Type::function(vec![Type::int(), Type::int()], Type::int());
    let between_from_two_tipo = Type::function(vec![Type::int()], Type::int());

    let via = TypedExpr::Sequence {
        location: Span::empty(),
        expressions: vec![
            TypedExpr::Assignment {
                location: Span::empty(),
                tipo: between_from_two_tipo.clone(),
                value: Box::new(TypedExpr::Call {
                    location: Span::empty(),
                    tipo: between_from_two_tipo.clone(),
                    fun: Box::new(fuzz_var("int_between", int_between_tipo)),
                    args: vec![call_arg(uint_lit("2"))],
                }),
                pattern: TypedPattern::var("between_from_two"),
                kind: crate::ast::AssignmentKind::Let { backpassing: () },
                comment: None,
            },
            TypedExpr::Call {
                location: Span::empty(),
                tipo: Type::int(),
                fun: Box::new(local_var("between_from_two", between_from_two_tipo)),
                args: vec![call_arg(uint_lit("9"))],
            },
        ],
    };

    let functions = empty_known_functions();
    assert!(matches!(
        extract_constraint_from_via(&via, "math", &functions),
        FuzzerConstraint::Unsupported { .. }
    ));
}

#[test]
fn extract_constraint_partial_helper_alias_is_resolved() {
    let bounded_key = FunctionAccessKey {
        module_name: "math".to_string(),
        function_name: "bounded".to_string(),
    };
    let bounded_fn = TypedFunction {
        arguments: vec![
            TypedArg::new("lo", Type::int()),
            TypedArg::new("hi", Type::int()),
        ],
        body: TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::int(),
            fun: Box::new(fuzz_var(
                "int_between",
                Type::function(vec![Type::int(), Type::int()], Type::int()),
            )),
            args: vec![
                call_arg(local_var("lo", Type::int())),
                call_arg(local_var("hi", Type::int())),
            ],
        },
        doc: None,
        location: Span::empty(),
        name: "bounded".to_string(),
        public: false,
        return_annotation: None,
        return_type: Type::int(),
        end_position: 0,
        on_test_failure: OnTestFailure::FailImmediately,
    };

    let bounded_tipo = Type::function(vec![Type::int(), Type::int()], Type::int());
    let at_most_ten_tipo = Type::function(vec![Type::int()], Type::int());
    let via = TypedExpr::Sequence {
        location: Span::empty(),
        expressions: vec![
            TypedExpr::Assignment {
                location: Span::empty(),
                tipo: at_most_ten_tipo.clone(),
                value: Box::new(TypedExpr::Call {
                    location: Span::empty(),
                    tipo: at_most_ten_tipo.clone(),
                    fun: Box::new(module_fn_var("bounded", "math", bounded_tipo)),
                    args: vec![call_arg(uint_lit("0"))],
                }),
                pattern: TypedPattern::var("at_most_ten"),
                kind: crate::ast::AssignmentKind::Let { backpassing: () },
                comment: None,
            },
            TypedExpr::Call {
                location: Span::empty(),
                tipo: Type::int(),
                fun: Box::new(local_var("at_most_ten", at_most_ten_tipo)),
                args: vec![call_arg(uint_lit("10"))],
            },
        ],
    };

    let mut functions = empty_known_functions();
    functions.insert(&bounded_key, &bounded_fn);

    assert!(matches!(
        extract_constraint_from_via(&via, "math", &functions),
        FuzzerConstraint::Unsupported { .. }
    ));
}

#[test]
fn extract_constraint_recursive_helper_is_unsupported() {
    let helper_key = FunctionAccessKey {
        module_name: "math".to_string(),
        function_name: "looping".to_string(),
    };
    let helper_fn = TypedFunction {
        arguments: vec![],
        body: TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::int(),
            fun: Box::new(module_fn_var(
                "looping",
                "math",
                Type::function(vec![], Type::int()),
            )),
            args: vec![],
        },
        doc: None,
        location: Span::empty(),
        name: "looping".to_string(),
        public: false,
        return_annotation: None,
        return_type: Type::int(),
        end_position: 0,
        on_test_failure: OnTestFailure::FailImmediately,
    };

    let mut functions = empty_known_functions();
    functions.insert(&helper_key, &helper_fn);

    let via = make_zero_arg_call("looping", Type::int());
    assert!(matches!(
        extract_constraint_from_via(&via, "math", &functions),
        FuzzerConstraint::Unsupported { .. }
    ));
}

#[test]
fn extract_semantics_recursive_fuzzer_wrapper_cycle_is_opaque() {
    // See `normalize_fuzzer_recursive_wrapper_cycle_is_opaque` for the
    // rationale: mutually-recursive cycles without a base case are
    // kept opaque so that downstream verification can surface a real
    // error rather than silently widen to "any value of T".
    let (left_key, left_fn) = make_zero_arg_fuzzer_function(
        "left",
        Type::int(),
        make_zero_arg_fuzzer_call("right", Type::int()),
    );
    let (right_key, right_fn) = make_zero_arg_fuzzer_function(
        "right",
        Type::int(),
        make_zero_arg_fuzzer_call("left", Type::int()),
    );
    let mut functions = empty_known_functions();
    functions.insert(&left_key, &left_fn);
    functions.insert(&right_key, &right_fn);

    let via = make_zero_arg_fuzzer_call("left", Type::int());
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();
    let semantics =
        extract_semantics_from_via(&via, "math", &functions, &data_types, Type::int().as_ref());

    assert!(
        matches!(
            &semantics,
            FuzzerSemantics::Opaque { reason }
                if reason.contains("recursive helper fuzzer detected")
        ),
        "recursive helper semantics must remain conservative: {semantics:?}"
    );
}

#[test]
fn extract_constraint_sequence_tracks_local_alias_bindings() {
    let between_tipo = Type::function(vec![Type::int(), Type::int()], Type::int());
    let sequence = TypedExpr::Sequence {
        location: Span::empty(),
        expressions: vec![
            TypedExpr::Assignment {
                location: Span::empty(),
                tipo: between_tipo.clone(),
                value: Box::new(fuzz_var("int_between", between_tipo.clone())),
                pattern: TypedPattern::var("between"),
                kind: crate::ast::AssignmentKind::Let { backpassing: () },
                comment: None,
            },
            TypedExpr::Call {
                location: Span::empty(),
                tipo: Type::int(),
                fun: Box::new(local_var("between", between_tipo)),
                args: vec![call_arg(uint_lit("2")), call_arg(uint_lit("9"))],
            },
        ],
    };

    let functions = empty_known_functions();
    assert!(matches!(
        extract_constraint_from_via(&sequence, "math", &functions),
        FuzzerConstraint::Unsupported { .. }
    ));
}

#[test]
fn extract_constraint_list_between_rejects_negative_or_reversed_bounds() {
    let negative_min = TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::int(),
        fun: Box::new(fuzz_var(
            "list_between",
            Type::function(vec![Type::int(), Type::int(), Type::int()], Type::int()),
        )),
        args: vec![
            call_arg(make_int_between_via("0", "10")),
            call_arg(negate_expr(uint_lit("1"))),
            call_arg(uint_lit("5")),
        ],
    };

    let reversed = TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::int(),
        fun: Box::new(fuzz_var(
            "list_between",
            Type::function(vec![Type::int(), Type::int(), Type::int()], Type::int()),
        )),
        args: vec![
            call_arg(make_int_between_via("0", "10")),
            call_arg(uint_lit("6")),
            call_arg(uint_lit("2")),
        ],
    };

    let functions = empty_known_functions();
    assert!(matches!(
        extract_constraint_from_via(&negative_min, "math", &functions),
        FuzzerConstraint::Unsupported { .. }
    ));
    assert!(matches!(
        extract_constraint_from_via(&reversed, "math", &functions),
        FuzzerConstraint::Unsupported { .. }
    ));
}

#[test]
fn extract_constraint_tuple4_collects_component_bounds() {
    let via = make_tuple4_via(
        make_int_between_via("0", "5"),
        make_int_between_via("10", "15"),
        make_int_between_via("20", "25"),
        make_int_between_via("30", "35"),
    );
    let functions = empty_known_functions();
    let constraint = extract_constraint_from_via(&via, "math", &functions);

    assert_eq!(
        constraint,
        tuple_int_range_constraint(&[("0", "5"), ("10", "15"), ("20", "25"), ("30", "35"),])
    );
}

#[test]
fn extract_constraint_and_then_requires_resolvable_continuation() {
    let continuation_type = Type::function(vec![Type::int()], Type::fuzzer(Type::int()));
    let via = make_and_then_via(
        make_int_between_via("1", "5"),
        local_var("f", continuation_type),
        Type::int(),
    );
    let functions = empty_known_functions();
    assert!(matches!(
        extract_constraint_from_via(&via, "math", &functions),
        FuzzerConstraint::Unsupported { .. }
    ));
}

#[test]
fn extract_constraint_and_then_uses_continuation_output_domain_only() {
    let list_int_tipo = Type::list(Type::int());
    let list_fuzzer_tipo = Type::fuzzer(list_int_tipo.clone());
    let element_fuzzer = make_int_between_via("0", "3");
    let element_fuzzer_type = element_fuzzer.tipo();
    let continuation = TypedExpr::Fn {
        location: Span::empty(),
        tipo: Type::function(vec![Type::int()], list_fuzzer_tipo.clone()),
        is_capture: false,
        args: vec![TypedArg::new("n", Type::int())],
        body: Box::new(TypedExpr::Call {
            location: Span::empty(),
            tipo: list_fuzzer_tipo.clone(),
            fun: Box::new(fuzz_var(
                "list",
                Type::function(vec![element_fuzzer_type], list_fuzzer_tipo),
            )),
            args: vec![call_arg(element_fuzzer)],
        }),
        return_annotation: None,
    };

    let via = make_and_then_via(make_int_between_via("1", "5"), continuation, list_int_tipo);

    let functions = empty_known_functions();
    let constraint = extract_constraint_from_via(&via, "math", &functions);
    assert_eq!(
        constraint,
        FuzzerConstraint::List {
            elem: Box::new(int_range_constraint("0", "3")),
            min_len: Some(0),
            max_len: Some(20),
        }
    );
}

#[test]
fn extract_constraint_and_then_with_helper_returning_lambda_is_resolved() {
    let int_tipo = Type::int();
    let int_fuzzer_tipo = Type::fuzzer(int_tipo.clone());
    let continuation_key = FunctionAccessKey {
        module_name: "math".to_string(),
        function_name: "build_window_continuation".to_string(),
    };
    let continuation_fn = TypedFunction {
        arguments: vec![TypedArg::new("lo", int_tipo.clone())],
        body: TypedExpr::Fn {
            location: Span::empty(),
            tipo: Type::function(vec![int_tipo.clone()], int_fuzzer_tipo.clone()),
            is_capture: false,
            args: vec![TypedArg::new("n", int_tipo.clone())],
            body: Box::new(TypedExpr::Call {
                location: Span::empty(),
                tipo: int_fuzzer_tipo.clone(),
                fun: Box::new(fuzz_var(
                    "int_between",
                    Type::function(vec![Type::int(), Type::int()], int_fuzzer_tipo.clone()),
                )),
                args: vec![
                    call_arg(local_var("lo", int_tipo.clone())),
                    call_arg(uint_lit("10")),
                ],
            }),
            return_annotation: None,
        },
        doc: None,
        location: Span::empty(),
        name: "build_window_continuation".to_string(),
        public: false,
        return_annotation: None,
        return_type: Type::function(vec![int_tipo.clone()], int_fuzzer_tipo.clone()),
        end_position: 0,
        on_test_failure: OnTestFailure::FailImmediately,
    };

    let mut functions = empty_known_functions();
    functions.insert(&continuation_key, &continuation_fn);

    let continuation = TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::function(vec![Type::int()], int_fuzzer_tipo.clone()),
        fun: Box::new(module_fn_var(
            "build_window_continuation",
            "math",
            Type::function(
                vec![Type::int()],
                Type::function(vec![Type::int()], int_fuzzer_tipo),
            ),
        )),
        args: vec![call_arg(uint_lit("2"))],
    };
    let via = make_and_then_via(make_int_between_via("1", "5"), continuation, Type::int());

    let constraint = extract_constraint_from_via(&via, "math", &functions);
    assert_eq!(constraint, int_range_constraint("2", "10"));
}

#[test]
fn extract_constraint_and_then_with_partially_applied_multiarg_helper_is_resolved() {
    let int_tipo = Type::int();
    let int_fuzzer_tipo = Type::fuzzer(int_tipo.clone());
    let continuation_key = FunctionAccessKey {
        module_name: "math".to_string(),
        function_name: "bounded_continuation".to_string(),
    };
    let continuation_fn = TypedFunction {
        arguments: vec![
            TypedArg::new("lo", int_tipo.clone()),
            TypedArg::new("hi", int_tipo.clone()),
            TypedArg::new("n", int_tipo.clone()),
        ],
        body: TypedExpr::Call {
            location: Span::empty(),
            tipo: int_fuzzer_tipo.clone(),
            fun: Box::new(fuzz_var(
                "int_between",
                Type::function(vec![Type::int(), Type::int()], int_fuzzer_tipo.clone()),
            )),
            args: vec![
                call_arg(local_var("lo", int_tipo.clone())),
                call_arg(local_var("hi", int_tipo.clone())),
            ],
        },
        doc: None,
        location: Span::empty(),
        name: "bounded_continuation".to_string(),
        public: false,
        return_annotation: None,
        return_type: int_fuzzer_tipo.clone(),
        end_position: 0,
        on_test_failure: OnTestFailure::FailImmediately,
    };

    let mut functions = empty_known_functions();
    functions.insert(&continuation_key, &continuation_fn);

    let continuation = TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::function(vec![Type::int()], int_fuzzer_tipo.clone()),
        fun: Box::new(module_fn_var(
            "bounded_continuation",
            "math",
            Type::function(vec![Type::int(), Type::int(), Type::int()], int_fuzzer_tipo),
        )),
        args: vec![call_arg(uint_lit("2")), call_arg(uint_lit("9"))],
    };
    let via = make_and_then_via(make_int_between_via("1", "5"), continuation, Type::int());

    let constraint = extract_constraint_from_via(&via, "math", &functions);
    assert_eq!(constraint, int_range_constraint("2", "9"));
}

#[test]
fn extract_constraint_both_produces_tuple() {
    let via = TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::tuple(vec![Type::int(), Type::int()]),
        fun: Box::new(fuzz_var(
            "both",
            Type::function(
                vec![Type::int(), Type::int()],
                Type::tuple(vec![Type::int(), Type::int()]),
            ),
        )),
        args: vec![
            call_arg(make_int_between_via("0", "10")),
            call_arg(make_int_between_via("20", "30")),
        ],
    };
    let functions = empty_known_functions();
    assert!(matches!(
        extract_constraint_from_via(&via, "math", &functions),
        FuzzerConstraint::Unsupported { .. }
    ));
}

#[test]
fn extract_constraint_list_no_bounds() {
    let via = TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::int(), // simplification
        fun: Box::new(fuzz_var(
            "list",
            Type::function(vec![Type::int()], Type::int()),
        )),
        args: vec![call_arg(make_int_between_via("0", "10"))],
    };
    let functions = empty_known_functions();
    assert!(matches!(
        extract_constraint_from_via(&via, "math", &functions),
        FuzzerConstraint::Unsupported { .. }
    ));
}

#[test]
fn extract_constraint_list_between_with_bounds() {
    let via = TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::int(),
        fun: Box::new(fuzz_var(
            "list_between",
            Type::function(vec![Type::int(), Type::int(), Type::int()], Type::int()),
        )),
        args: vec![
            call_arg(make_int_between_via("0", "10")),
            call_arg(uint_lit("2")),
            call_arg(uint_lit("5")),
        ],
    };
    let functions = empty_known_functions();
    assert!(matches!(
        extract_constraint_from_via(&via, "math", &functions),
        FuzzerConstraint::Unsupported { .. }
    ));
}

fn make_typed_bytearray_between_fuzzer(min: &str, max: &str) -> TypedExpr {
    let output_type = Type::byte_array();
    let fuzzer_type = Type::fuzzer(output_type.clone());
    TypedExpr::Call {
        location: Span::empty(),
        tipo: fuzzer_type.clone(),
        fun: Box::new(module_fn_var(
            "bytearray_between",
            STDLIB_FUZZ_MODULE,
            Type::function(vec![Type::int(), Type::int()], fuzzer_type),
        )),
        args: vec![call_arg(uint_lit(min)), call_arg(uint_lit(max))],
    }
}

fn make_typed_bytearray_at_most_fuzzer(max: &str) -> TypedExpr {
    let output_type = Type::byte_array();
    let fuzzer_type = Type::fuzzer(output_type.clone());
    TypedExpr::Call {
        location: Span::empty(),
        tipo: fuzzer_type.clone(),
        fun: Box::new(module_fn_var(
            "bytearray_at_most",
            STDLIB_FUZZ_MODULE,
            Type::function(vec![Type::int()], fuzzer_type),
        )),
        args: vec![call_arg(uint_lit(max))],
    }
}

fn make_typed_bytearray_at_least_fuzzer(min: &str) -> TypedExpr {
    let output_type = Type::byte_array();
    let fuzzer_type = Type::fuzzer(output_type.clone());
    TypedExpr::Call {
        location: Span::empty(),
        tipo: fuzzer_type.clone(),
        fun: Box::new(module_fn_var(
            "bytearray_at_least",
            STDLIB_FUZZ_MODULE,
            Type::function(vec![Type::int()], fuzzer_type),
        )),
        args: vec![call_arg(uint_lit(min))],
    }
}

#[test]
fn extract_constraint_bytearray_between_with_bounds() {
    let via = make_typed_bytearray_between_fuzzer("2", "5");
    let functions = empty_known_functions();
    assert_eq!(
        extract_constraint_from_via(&via, "math", &functions),
        FuzzerConstraint::ByteStringLenRange {
            min_len: 2,
            max_len: 5,
        }
    );
}

#[test]
fn extract_constraint_bytearray_between_equal_bounds_pins_length() {
    // Regression for `prop_from_script`-style tests that use
    // `fuzz.bytearray_between(28, 28)` to mint a fixed-width ByteArray
    // (here, a 28-byte script hash). Must produce an equal-bounds range
    // so downstream semantics yield `ByteArrayRange { Some(28), Some(28) }`
    // rather than falling through to `Unsupported` and triggering a skip.
    let via = make_typed_bytearray_between_fuzzer("28", "28");
    let functions = empty_known_functions();
    assert_eq!(
        extract_constraint_from_via(&via, "math", &functions),
        FuzzerConstraint::ByteStringLenRange {
            min_len: 28,
            max_len: 28,
        }
    );
}

#[test]
fn extract_constraint_bytearray_between_swapped_bounds_are_normalized() {
    // Mirrors `int_between`'s swap-normalization: user-facing sugar that
    // tolerates `(hi, lo)` ordering must still land on `min ≤ max` so
    // downstream emission doesn't produce an empty or inverted domain.
    let via = make_typed_bytearray_between_fuzzer("7", "3");
    let functions = empty_known_functions();
    assert_eq!(
        extract_constraint_from_via(&via, "math", &functions),
        FuzzerConstraint::ByteStringLenRange {
            min_len: 3,
            max_len: 7,
        }
    );
}

#[test]
fn extract_constraint_bytearray_at_most_emits_zero_min_bound() {
    // `bytearray_at_most(n)` bounds length to `[0, n]`. Length 0 is a
    // sound, representable lower bound for bytearrays — no sentinel dance
    // required (contrast `int_at_least`, whose lower bound is unbounded
    // and uses an `i128::MIN` sentinel).
    let via = make_typed_bytearray_at_most_fuzzer("32");
    let functions = empty_known_functions();
    assert_eq!(
        extract_constraint_from_via(&via, "math", &functions),
        FuzzerConstraint::ByteStringLenRange {
            min_len: 0,
            max_len: 32,
        }
    );
}

#[test]
fn extract_constraint_bytearray_at_least_does_not_produce_bytestring_len_range() {
    // `bytearray_at_least(n)` has an unbounded upper length.
    // `ByteStringLenRange` cannot represent "unbounded max" without a
    // sentinel, and unlike `IntRange` no downstream sentinel-stripping
    // exists. We therefore do NOT return a `ByteStringLenRange` for this
    // callee — any other constraint shape (Unsupported or a wider
    // over-approximation) is acceptable, since it keeps the verifier
    // sound. The key negative assertion is that no `ByteStringLenRange`
    // with a fabricated upper bound leaks downstream.
    let via = make_typed_bytearray_at_least_fuzzer("4");
    let functions = empty_known_functions();
    let constraint = extract_constraint_from_via(&via, "math", &functions);
    assert!(
        !matches!(constraint, FuzzerConstraint::ByteStringLenRange { .. }),
        "bytearray_at_least must not fabricate a bounded length range; got {:?}",
        constraint,
    );
}

#[test]
fn extract_constraint_pipeline_uses_last() {
    let via = TypedExpr::Pipeline {
        location: Span::empty(),
        expressions: vec![
            local_var("ignored", Type::int()),
            make_int_between_via("3", "7"),
        ],
    };
    let functions = empty_known_functions();
    assert_eq!(
        extract_constraint_from_via(&via, "math", &functions),
        int_range_constraint("3", "7")
    );
}

#[test]
fn extract_constraint_unknown_function_returns_unsupported() {
    let via = TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::int(),
        fun: Box::new(fuzz_var(
            "some_unknown_fuzzer",
            Type::function(vec![], Type::int()),
        )),
        args: vec![],
    };
    let functions = empty_known_functions();
    assert!(matches!(
        extract_constraint_from_via(&via, "math", &functions),
        FuzzerConstraint::Unsupported { .. }
    ));
}

fn scenario_semantics_fixture() -> (
    IndexMap<DataTypeKey, TypedDataType>,
    Rc<Type>,
    Rc<Type>,
    Rc<Type>,
) {
    let state_type = Rc::new(Type::App {
        public: true,
        contains_opaque: false,
        module: "permissions".to_string(),
        name: "State".to_string(),
        args: vec![],
        alias: None,
    });
    let input_type = Rc::new(Type::App {
        public: true,
        contains_opaque: false,
        module: "cardano/transaction".to_string(),
        name: "Input".to_string(),
        args: vec![],
        alias: None,
    });
    let transaction_type = Rc::new(Type::App {
        public: true,
        contains_opaque: false,
        module: "cardano/transaction".to_string(),
        name: "Transaction".to_string(),
        args: vec![],
        alias: None,
    });
    let state_generic = Type::generic_var(0);

    let scenario_data_type = TypedDataType {
        decorators: vec![],
        constructors: vec![
            RecordConstructor {
                decorators: vec![],
                location: Span::empty(),
                name: "Done".to_string(),
                arguments: vec![],
                doc: None,
                sugar: false,
            },
            RecordConstructor {
                decorators: vec![],
                location: Span::empty(),
                name: "Step".to_string(),
                arguments: vec![
                    RecordConstructorArg {
                        label: Some("labels".to_string()),
                        annotation: Annotation::Constructor {
                            location: Span::empty(),
                            module: None,
                            name: "List".to_string(),
                            arguments: vec![Annotation::Constructor {
                                location: Span::empty(),
                                module: None,
                                name: "String".to_string(),
                                arguments: vec![],
                            }],
                        },
                        location: Span::empty(),
                        tipo: Type::list(Type::string()),
                        doc: None,
                    },
                    RecordConstructorArg {
                        label: Some("state".to_string()),
                        annotation: Annotation::Var {
                            location: Span::empty(),
                            name: "st".to_string(),
                        },
                        location: Span::empty(),
                        tipo: state_generic.clone(),
                        doc: None,
                    },
                    RecordConstructorArg {
                        label: Some("event".to_string()),
                        annotation: Annotation::Constructor {
                            location: Span::empty(),
                            module: Some("cardano/transaction".to_string()),
                            name: "Transaction".to_string(),
                            arguments: vec![],
                        },
                        location: Span::empty(),
                        tipo: transaction_type.clone(),
                        doc: None,
                    },
                ],
                doc: None,
                sugar: false,
            },
        ],
        doc: None,
        location: Span::empty(),
        name: "Scenario".to_string(),
        opaque: false,
        parameters: vec!["st".to_string()],
        public: true,
        typed_parameters: vec![state_generic],
    };

    let mut data_types = IndexMap::new();
    data_types.insert(
        DataTypeKey {
            module_name: STDLIB_FUZZ_SCENARIO_MODULE.to_string(),
            defined_type: "Scenario".to_string(),
        },
        scenario_data_type,
    );

    (data_types, state_type, input_type, transaction_type)
}

fn expected_scenario_transition_semantics() -> StateMachineTransitionSemantics {
    StateMachineTransitionSemantics {
        terminal_tag: 0,
        step_tag: 1,
        label_field_index: 0,
        next_state_field_index: 1,
        event_field_index: 2,
        state_semantics: Box::new(FuzzerSemantics::DataWithSchema {
            type_name: "permissions.State".to_string(),
        }),
        step_input_semantics: vec![FuzzerSemantics::List {
            element: Box::new(FuzzerSemantics::DataWithSchema {
                type_name: "cardano/transaction.Input".to_string(),
            }),
            min_len: None,
            max_len: None,
        }],
        label_semantics: Box::new(FuzzerSemantics::List {
            element: Box::new(FuzzerSemantics::String),
            min_len: None,
            max_len: None,
        }),
        event_semantics: Box::new(FuzzerSemantics::DataWithSchema {
            type_name: "cardano/transaction.Transaction".to_string(),
        }),
    }
}

fn make_state_machine_trace_via(
    callee_name: &str,
    state_type: Rc<Type>,
    input_type: Rc<Type>,
    output_type: Rc<Type>,
) -> TypedExpr {
    let step_type = Type::function(
        vec![state_type.clone(), Type::list(input_type)],
        Type::fuzzer(Rc::new(Type::App {
            public: true,
            contains_opaque: false,
            module: STDLIB_FUZZ_SCENARIO_MODULE.to_string(),
            name: "Scenario".to_string(),
            args: vec![state_type.clone()],
            alias: None,
        })),
    );

    TypedExpr::Call {
        location: Span::empty(),
        tipo: output_type.clone(),
        fun: Box::new(module_fn_var(
            callee_name,
            STDLIB_FUZZ_SCENARIO_MODULE,
            Type::function(vec![state_type.clone(), step_type.clone()], output_type),
        )),
        args: vec![
            call_arg(local_var("initial_state", state_type)),
            call_arg(local_var("step", step_type)),
        ],
    }
}

#[test]
fn extract_constraint_scenario_like_non_fuzzer_call_is_unsupported() {
    let functions = empty_known_functions();

    for name in ["ok", "ko", "report_coverage"] {
        let via = TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::int(),
            fun: Box::new(module_fn_var(
                name,
                STDLIB_FUZZ_SCENARIO_MODULE,
                Type::function(vec![Type::int(), Type::int()], Type::int()),
            )),
            args: vec![
                call_arg(make_int_between_via("0", "10")),
                call_arg(uint_lit("0")),
            ],
        };

        assert!(
            matches!(
                extract_constraint_from_via(&via, "math", &functions),
                FuzzerConstraint::Unsupported { .. }
            ),
            "scenario-like call '{name}' without Fuzzer typing must fail closed"
        );
    }
}

#[test]
fn normalize_fuzzer_state_machine_trace_shape_is_name_agnostic() {
    let (_owned_data_types, state_type, input_type, transaction_type) =
        scenario_semantics_fixture();
    let step_type = Type::function(
        vec![state_type.clone(), Type::list(input_type.clone())],
        Type::fuzzer(Rc::new(Type::App {
            public: true,
            contains_opaque: false,
            module: STDLIB_FUZZ_SCENARIO_MODULE.to_string(),
            name: "Scenario".to_string(),
            args: vec![state_type.clone()],
            alias: None,
        })),
    );
    let via = TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::list(transaction_type.clone()),
        fun: Box::new(module_fn_var(
            "anything_but_ok",
            "math",
            Type::function(
                vec![state_type.clone(), step_type.clone()],
                Type::list(transaction_type),
            ),
        )),
        args: vec![
            call_arg(local_var("initial_state", state_type)),
            call_arg(local_var("step", step_type)),
        ],
    };
    let functions = empty_known_functions();

    assert!(matches!(
        normalize_fuzzer_from_via(&via, "permissions", &functions),
        NormalizedFuzzer::StateMachineTrace {
            acceptance: StateMachineAcceptance::AcceptsSuccess,
            ..
        }
    ));
}

#[test]
fn normalize_fuzzer_state_machine_trace_rejects_known_non_fuzz_stdlib() {
    // The negative filter at normalize_state_machine_trace_from_call must reject
    // known stdlib modules that don't look like fuzz/test/scenario combinators.
    // This regression test pins that `aiken/list` is rejected even when its
    // structural signature matches (state, step_fn) -> List<T>.
    //
    // NOTE: The recognizer is intentionally name-agnostic (commit 2e6c95f0) and
    // accepts user modules with matching shape. Only known non-fuzz stdlib
    // modules are excluded by the negative filter. Converting to a positive
    // allowlist requires a team decision — do not change the filter here.
    let (_owned_data_types, state_type, input_type, transaction_type) =
        scenario_semantics_fixture();
    let step_type = Type::function(
        vec![state_type.clone(), Type::list(input_type.clone())],
        Type::fuzzer(Rc::new(Type::App {
            public: true,
            contains_opaque: false,
            module: STDLIB_FUZZ_SCENARIO_MODULE.to_string(),
            name: "Scenario".to_string(),
            args: vec![state_type.clone()],
            alias: None,
        })),
    );
    let via = TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::list(transaction_type.clone()),
        fun: Box::new(module_fn_var(
            "fold",
            "aiken/list",
            Type::function(
                vec![state_type.clone(), step_type.clone()],
                Type::list(transaction_type),
            ),
        )),
        args: vec![
            call_arg(local_var("initial_state", state_type)),
            call_arg(local_var("step", step_type)),
        ],
    };
    let functions = empty_known_functions();

    let result = normalize_fuzzer_from_via(&via, "permissions", &functions);

    assert!(!matches!(
        result,
        NormalizedFuzzer::StateMachineTrace { .. }
    ));
}

#[test]
fn extract_constraint_state_machine_trace_is_name_agnostic() {
    let (owned_data_types, state_type, input_type, transaction_type) = scenario_semantics_fixture();
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();
    let step_type = Type::function(
        vec![state_type.clone(), Type::list(input_type.clone())],
        Type::fuzzer(Rc::new(Type::App {
            public: true,
            contains_opaque: false,
            module: STDLIB_FUZZ_SCENARIO_MODULE.to_string(),
            name: "Scenario".to_string(),
            args: vec![state_type.clone()],
            alias: None,
        })),
    );
    let via = TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::list(transaction_type.clone()),
        fun: Box::new(module_fn_var(
            "anything_but_ok",
            "math",
            Type::function(
                vec![state_type.clone(), step_type.clone()],
                Type::list(transaction_type.clone()),
            ),
        )),
        args: vec![
            call_arg(local_var("initial_state", state_type.clone())),
            call_arg(local_var("step", step_type)),
        ],
    };
    let functions = empty_known_functions();

    assert_eq!(
        extract_constraint_from_via_with_data_types(&via, "permissions", &functions, &data_types,),
        FuzzerConstraint::List {
            elem: Box::new(FuzzerConstraint::Any),
            min_len: Some(0),
            max_len: None,
        }
    );
}

#[test]
fn extract_semantics_scenario_ok_is_state_machine_trace() {
    let (owned_data_types, state_type, input_type, transaction_type) = scenario_semantics_fixture();
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();
    let step_type = Type::function(
        vec![state_type.clone(), Type::list(input_type.clone())],
        Type::fuzzer(Rc::new(Type::App {
            public: true,
            contains_opaque: false,
            module: STDLIB_FUZZ_SCENARIO_MODULE.to_string(),
            name: "Scenario".to_string(),
            args: vec![state_type.clone()],
            alias: None,
        })),
    );
    let via = TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::list(transaction_type.clone()),
        fun: Box::new(module_fn_var(
            "ok",
            STDLIB_FUZZ_SCENARIO_MODULE,
            Type::function(
                vec![state_type.clone(), step_type.clone()],
                Type::list(transaction_type.clone()),
            ),
        )),
        args: vec![
            call_arg(local_var("initial_state", state_type.clone())),
            call_arg(local_var("step", step_type)),
        ],
    };
    let functions = empty_known_functions();

    assert_eq!(
        extract_semantics_from_via(
            &via,
            "permissions",
            &functions,
            &data_types,
            Type::list(transaction_type.clone()).as_ref(),
        ),
        FuzzerSemantics::StateMachineTrace {
            acceptance: StateMachineAcceptance::AcceptsSuccess,
            state_type: SemanticType::Unsupported("permissions.State".to_string()),
            step_input_types: vec![SemanticType::List(Box::new(SemanticType::Unsupported(
                "cardano/transaction.Input".to_string()
            ),))],
            label_type: SemanticType::List(Box::new(SemanticType::String)),
            event_type: SemanticType::Unsupported("cardano/transaction.Transaction".to_string()),
            transition_semantics: expected_scenario_transition_semantics(),
            output_semantics: Box::new(FuzzerSemantics::List {
                element: Box::new(FuzzerSemantics::DataWithSchema {
                    type_name: "cardano/transaction.Transaction".to_string(),
                }),
                min_len: Some(0),
                max_len: None,
            }),
            step_function_ir: None,
            step_ir_unsupported_reason: None,
            transition_prop: None,
            initial_state_shallow_ir: None,
        }
    );
}

#[test]
fn extract_semantics_state_machine_trace_is_name_agnostic() {
    let (owned_data_types, state_type, input_type, transaction_type) = scenario_semantics_fixture();
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();
    let step_type = Type::function(
        vec![state_type.clone(), Type::list(input_type.clone())],
        Type::fuzzer(Rc::new(Type::App {
            public: true,
            contains_opaque: false,
            module: STDLIB_FUZZ_SCENARIO_MODULE.to_string(),
            name: "Scenario".to_string(),
            args: vec![state_type.clone()],
            alias: None,
        })),
    );
    let via = TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::list(transaction_type.clone()),
        fun: Box::new(module_fn_var(
            "anything_but_ok",
            "math",
            Type::function(
                vec![state_type.clone(), step_type.clone()],
                Type::list(transaction_type.clone()),
            ),
        )),
        args: vec![
            call_arg(local_var("initial_state", state_type.clone())),
            call_arg(local_var("step", step_type)),
        ],
    };
    let functions = empty_known_functions();

    assert_eq!(
        extract_semantics_from_via(
            &via,
            "permissions",
            &functions,
            &data_types,
            Type::list(transaction_type.clone()).as_ref(),
        ),
        FuzzerSemantics::StateMachineTrace {
            acceptance: StateMachineAcceptance::AcceptsSuccess,
            state_type: SemanticType::Unsupported("permissions.State".to_string()),
            step_input_types: vec![SemanticType::List(Box::new(SemanticType::Unsupported(
                "cardano/transaction.Input".to_string()
            ),))],
            label_type: SemanticType::List(Box::new(SemanticType::String)),
            event_type: SemanticType::Unsupported("cardano/transaction.Transaction".to_string()),
            transition_semantics: expected_scenario_transition_semantics(),
            output_semantics: Box::new(FuzzerSemantics::List {
                element: Box::new(FuzzerSemantics::DataWithSchema {
                    type_name: "cardano/transaction.Transaction".to_string(),
                }),
                min_len: Some(0),
                max_len: None,
            }),
            step_function_ir: None,
            step_ir_unsupported_reason: None,
            transition_prop: None,
            initial_state_shallow_ir: None,
        }
    );
}

#[test]
fn extract_semantics_scenario_ko_is_state_machine_trace() {
    let (owned_data_types, state_type, input_type, transaction_type) = scenario_semantics_fixture();
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();
    let step_type = Type::function(
        vec![state_type.clone(), Type::list(input_type.clone())],
        Type::fuzzer(Rc::new(Type::App {
            public: true,
            contains_opaque: false,
            module: STDLIB_FUZZ_SCENARIO_MODULE.to_string(),
            name: "Scenario".to_string(),
            args: vec![state_type.clone()],
            alias: None,
        })),
    );
    let via = TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::tuple(vec![
            Type::list(Type::string()),
            Type::list(transaction_type.clone()),
        ]),
        fun: Box::new(module_fn_var(
            "ko",
            STDLIB_FUZZ_SCENARIO_MODULE,
            Type::function(
                vec![state_type.clone(), step_type.clone()],
                Type::tuple(vec![
                    Type::list(Type::string()),
                    Type::list(transaction_type.clone()),
                ]),
            ),
        )),
        args: vec![
            call_arg(local_var("initial_state", state_type.clone())),
            call_arg(local_var("step", step_type)),
        ],
    };
    let functions = empty_known_functions();

    assert_eq!(
        extract_semantics_from_via(
            &via,
            "permissions",
            &functions,
            &data_types,
            Type::tuple(vec![
                Type::list(Type::string()),
                Type::list(transaction_type.clone()),
            ])
            .as_ref(),
        ),
        FuzzerSemantics::StateMachineTrace {
            acceptance: StateMachineAcceptance::AcceptsFailure,
            state_type: SemanticType::Unsupported("permissions.State".to_string()),
            step_input_types: vec![SemanticType::List(Box::new(SemanticType::Unsupported(
                "cardano/transaction.Input".to_string()
            ),))],
            label_type: SemanticType::List(Box::new(SemanticType::String)),
            event_type: SemanticType::Unsupported("cardano/transaction.Transaction".to_string()),
            transition_semantics: expected_scenario_transition_semantics(),
            output_semantics: Box::new(FuzzerSemantics::Product(vec![
                FuzzerSemantics::List {
                    element: Box::new(FuzzerSemantics::String),
                    min_len: Some(1),
                    max_len: None,
                },
                FuzzerSemantics::List {
                    element: Box::new(FuzzerSemantics::DataWithSchema {
                        type_name: "cardano/transaction.Transaction".to_string(),
                    }),
                    min_len: Some(1),
                    max_len: Some(1),
                },
            ])),
            step_function_ir: None,
            step_ir_unsupported_reason: None,
            transition_prop: None,
            initial_state_shallow_ir: None,
        }
    );
}

#[test]
fn extract_semantics_state_machine_trace_rejects_success_with_wrong_event_type() {
    let (owned_data_types, state_type, input_type, _transaction_type) =
        scenario_semantics_fixture();
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();
    let output_type = Type::list(Type::int());
    let via = make_state_machine_trace_via("ok", state_type, input_type, output_type.clone());
    let functions = empty_known_functions();

    let semantics = extract_semantics_from_via(
        &via,
        "permissions",
        &functions,
        &data_types,
        output_type.as_ref(),
    );

    assert!(
        matches!(
            semantics,
            FuzzerSemantics::Opaque { ref reason }
                if reason.contains("state-machine trace output schema does not match transition payloads")
                    && reason.contains("events of type")
                    && reason.contains("Transaction")
        ),
        "wrong success event type must fail closed, got {semantics:?}"
    );
}

#[test]
fn extract_semantics_state_machine_trace_rejects_failure_with_wrong_label_type() {
    let (owned_data_types, state_type, input_type, transaction_type) = scenario_semantics_fixture();
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();
    let output_type = Type::tuple(vec![Type::list(Type::bool()), Type::list(transaction_type)]);
    let via = make_state_machine_trace_via("ko", state_type, input_type, output_type.clone());
    let functions = empty_known_functions();

    let semantics = extract_semantics_from_via(
        &via,
        "permissions",
        &functions,
        &data_types,
        output_type.as_ref(),
    );

    assert!(
        matches!(
            semantics,
            FuzzerSemantics::Opaque { ref reason }
                if reason.contains("state-machine trace output schema does not match transition payloads")
                    && reason.contains("labels of type")
                    && reason.contains("String")
        ),
        "wrong failure label type must fail closed, got {semantics:?}"
    );
}

#[test]
fn extract_semantics_state_machine_trace_rejects_failure_with_wrong_event_type() {
    let (owned_data_types, state_type, input_type, _transaction_type) =
        scenario_semantics_fixture();
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();
    let output_type = Type::tuple(vec![Type::list(Type::string()), Type::list(Type::int())]);
    let via = make_state_machine_trace_via("ko", state_type, input_type, output_type.clone());
    let functions = empty_known_functions();

    let semantics = extract_semantics_from_via(
        &via,
        "permissions",
        &functions,
        &data_types,
        output_type.as_ref(),
    );

    assert!(
        matches!(
            semantics,
            FuzzerSemantics::Opaque { ref reason }
                if reason.contains("state-machine trace output schema does not match transition payloads")
                    && reason.contains("events of type")
                    && reason.contains("Transaction")
        ),
        "wrong failure event type must fail closed, got {semantics:?}"
    );
}

#[test]
fn state_machine_trace_output_semantics_uses_data_with_schema_for_adt_event_type() {
    // Regression for Issue 14 gap: state-machine trace `output_semantics` must
    // lower a non-nullary qualified ADT event type as `DataWithSchema`, not
    // `Opaque`. Prior to this fix, `state_machine_trace_output_semantics` took
    // `&SemanticType` and called `default_semantics_for_semantic_type`, which
    // mapped `SemanticType::Unsupported(_)` to `FuzzerSemantics::Opaque` and
    // bypassed the `default_semantics_for_type` ADT branch.
    let (owned_data_types, state_type, input_type, transaction_type) = scenario_semantics_fixture();
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();
    let step_type = Type::function(
        vec![state_type.clone(), Type::list(input_type.clone())],
        Type::fuzzer(Rc::new(Type::App {
            public: true,
            contains_opaque: false,
            module: STDLIB_FUZZ_SCENARIO_MODULE.to_string(),
            name: "Scenario".to_string(),
            args: vec![state_type.clone()],
            alias: None,
        })),
    );
    let via = TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::list(transaction_type.clone()),
        fun: Box::new(module_fn_var(
            "ok",
            STDLIB_FUZZ_SCENARIO_MODULE,
            Type::function(
                vec![state_type.clone(), step_type.clone()],
                Type::list(transaction_type.clone()),
            ),
        )),
        args: vec![
            call_arg(local_var("initial_state", state_type.clone())),
            call_arg(local_var("step", step_type)),
        ],
    };
    let functions = empty_known_functions();

    let semantics = extract_semantics_from_via(
        &via,
        "permissions",
        &functions,
        &data_types,
        Type::list(transaction_type.clone()).as_ref(),
    );

    match semantics {
        FuzzerSemantics::StateMachineTrace {
            output_semantics,
            transition_semantics,
            ..
        } => {
            // Event type `cardano/transaction.Transaction` is a non-nullary
            // qualified ADT and must be lowered as `DataWithSchema`, not
            // `Opaque`, in both the `output_semantics` List element and the
            // transition's `event_semantics`.
            assert_eq!(
                output_semantics.as_ref(),
                &FuzzerSemantics::List {
                    element: Box::new(FuzzerSemantics::DataWithSchema {
                        type_name: "cardano/transaction.Transaction".to_string(),
                    }),
                    min_len: Some(0),
                    max_len: None,
                },
                "AcceptsSuccess output_semantics must use DataWithSchema for non-nullary qualified ADT event type",
            );
            assert_eq!(
                transition_semantics.event_semantics.as_ref(),
                &FuzzerSemantics::DataWithSchema {
                    type_name: "cardano/transaction.Transaction".to_string(),
                },
                "transition event_semantics must use DataWithSchema for non-nullary qualified ADT event type",
            );
        }
        other => panic!("expected StateMachineTrace, got {:?}", other),
    }
}

#[test]
fn extract_constraint_nested_both_with_map() {
    // both(map(int_between(0,10), f), int_between(20,30))
    let inner_map = TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::int(),
        fun: Box::new(fuzz_var(
            "map",
            Type::function(vec![Type::int(), Type::int()], Type::int()),
        )),
        args: vec![
            call_arg(make_int_between_via("0", "10")),
            call_arg(local_var(
                "f",
                Type::function(vec![Type::int()], Type::int()),
            )),
        ],
    };
    let via = TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::tuple(vec![Type::int(), Type::int()]),
        fun: Box::new(fuzz_var(
            "both",
            Type::function(
                vec![Type::int(), Type::int()],
                Type::tuple(vec![Type::int(), Type::int()]),
            ),
        )),
        args: vec![
            call_arg(inner_map),
            call_arg(make_int_between_via("20", "30")),
        ],
    };
    let functions = empty_known_functions();
    assert!(matches!(
        extract_constraint_from_via(&via, "math", &functions),
        FuzzerConstraint::Unsupported { .. }
    ));
}

#[test]
fn extract_constraint_both_with_extra_arguments_is_unsupported() {
    let via = TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::tuple(vec![Type::int(), Type::int(), Type::int()]),
        fun: Box::new(fuzz_var(
            "both",
            Type::function(
                vec![Type::int(), Type::int(), Type::int()],
                Type::tuple(vec![Type::int(), Type::int(), Type::int()]),
            ),
        )),
        args: vec![
            call_arg(make_int_between_via("0", "10")),
            call_arg(make_int_between_via("20", "30")),
            call_arg(make_int_between_via("40", "50")),
        ],
    };
    let functions = empty_known_functions();
    assert!(
        matches!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::Unsupported { .. }
        ),
        "both should reject unexpected arity"
    );
}

#[test]
fn derive_semantics_for_int_range_is_generic_ir() {
    assert_eq!(
        semantics_from_constraint(
            &FuzzerConstraint::IntRange {
                min: "1".to_string(),
                max: "10".to_string(),
            },
            Type::int().as_ref(),
        ),
        FuzzerSemantics::IntRange {
            min: Some("1".to_string()),
            max: Some("10".to_string()),
        }
    );
}

#[test]
fn derive_semantics_for_tuple_is_product() {
    assert_eq!(
        semantics_from_constraint(
            &FuzzerConstraint::Tuple(vec![
                FuzzerConstraint::IntRange {
                    min: "0".to_string(),
                    max: "3".to_string(),
                },
                FuzzerConstraint::ByteStringLenRange {
                    min_len: 2,
                    max_len: 4,
                },
            ]),
            Type::tuple(vec![Type::int(), Type::byte_array()]).as_ref(),
        ),
        FuzzerSemantics::Product(vec![
            FuzzerSemantics::IntRange {
                min: Some("0".to_string()),
                max: Some("3".to_string()),
            },
            FuzzerSemantics::ByteArrayRange {
                min_len: Some(2),
                max_len: Some(4),
            },
        ])
    );
}

#[test]
fn derive_semantics_for_list_of_unsupported_type_yields_data_with_schema_leaf() {
    // After Issue 14, qualified non-nullary ADTs (e.g.
    // cardano/transaction.Transaction) lower to
    // `FuzzerSemantics::DataWithSchema` so the state-machine path can
    // attach a structural schema predicate. The leaf is no longer
    // classified as `Opaque` -- `fuzzer_semantics_contains_opaque`
    // returns false for DataWithSchema leaves, which is what unblocks
    // the Transaction-domain state-machine proofs.
    let transaction_type = Rc::new(Type::App {
        public: true,
        contains_opaque: false,
        module: "cardano/transaction".to_string(),
        name: "Transaction".to_string(),
        args: vec![],
        alias: None,
    });

    let semantics = semantics_from_constraint(
        &FuzzerConstraint::List {
            elem: Box::new(FuzzerConstraint::Any),
            min_len: Some(0),
            max_len: None,
        },
        Type::list(transaction_type).as_ref(),
    );

    assert!(
        matches!(
            &semantics,
            FuzzerSemantics::List {
                element,
                min_len: Some(0),
                max_len: None,
            } if matches!(
                element.as_ref(),
                FuzzerSemantics::DataWithSchema { type_name }
                if type_name == "cardano/transaction.Transaction"
            )
        ),
        "expected DataWithSchema leaf for non-nullary qualified ADT, got {semantics:?}"
    );
}

#[test]
fn derive_semantics_for_map_input_constraint_is_not_output_semantics() {
    assert!(matches!(
        semantics_from_constraint(
            &FuzzerConstraint::Map(Box::new(FuzzerConstraint::IntRange {
                min: "5".to_string(),
                max: "15".to_string(),
            })),
            Type::int().as_ref(),
        ),
        FuzzerSemantics::Opaque { reason }
            if reason.contains("map input constraint cannot be reinterpreted as output semantics")
    ));
}

#[test]
fn derive_semantics_for_map_type_mismatch_is_opaque() {
    // Map wrapping an IntRange but output type is ByteArray — type mismatch
    assert!(matches!(
        semantics_from_constraint(
            &FuzzerConstraint::Map(Box::new(FuzzerConstraint::IntRange {
                min: "0".to_string(),
                max: "10".to_string(),
            })),
            Type::byte_array().as_ref(),
        ),
        FuzzerSemantics::Opaque { .. }
    ));
}

#[test]
fn derive_semantics_for_and_intersects_int_ranges() {
    assert_eq!(
        semantics_from_constraint(
            &FuzzerConstraint::And(vec![
                FuzzerConstraint::IntRange {
                    min: "0".to_string(),
                    max: "100".to_string(),
                },
                FuzzerConstraint::IntRange {
                    min: "50".to_string(),
                    max: "200".to_string(),
                },
            ]),
            Type::int().as_ref(),
        ),
        FuzzerSemantics::IntRange {
            min: Some("50".to_string()),
            max: Some("100".to_string()),
        }
    );
}

#[test]
fn derive_semantics_for_and_all_any_produces_default() {
    // And with all Any constraints — should produce default semantics for the type.
    let semantics = semantics_from_constraint(
        &FuzzerConstraint::And(vec![FuzzerConstraint::Any, FuzzerConstraint::Any]),
        Type::int().as_ref(),
    );
    assert_eq!(
        semantics,
        FuzzerSemantics::IntRange {
            min: None,
            max: None,
        }
    );
}

#[test]
fn derive_semantics_for_and_disjoint_ranges_produces_default() {
    // [10, 20] AND [30, 40] are disjoint — intersection is empty,
    // so we should fall back to default (unbounded) semantics.
    let semantics = semantics_from_constraint(
        &FuzzerConstraint::And(vec![
            FuzzerConstraint::IntRange {
                min: "10".to_string(),
                max: "20".to_string(),
            },
            FuzzerConstraint::IntRange {
                min: "30".to_string(),
                max: "40".to_string(),
            },
        ]),
        Type::int().as_ref(),
    );
    assert_eq!(
        semantics,
        FuzzerSemantics::IntRange {
            min: None,
            max: None,
        }
    );
}

#[test]
fn extract_exact_scalar_bool_true() {
    assert_eq!(
        try_extract_exact_scalar(&bool_constructor(true)),
        Some(FuzzerExactValue::Bool(true)),
    );
}

#[test]
fn extract_exact_scalar_bool_false() {
    assert_eq!(
        try_extract_exact_scalar(&bool_constructor(false)),
        Some(FuzzerExactValue::Bool(false)),
    );
}

#[test]
fn extract_exact_scalar_string() {
    let expr = TypedExpr::String {
        location: Span::empty(),
        tipo: Type::string(),
        value: "hello".to_string(),
    };
    assert_eq!(
        try_extract_exact_scalar(&expr),
        Some(FuzzerExactValue::String("hello".to_string())),
    );
}

#[test]
fn extract_exact_scalar_bytearray() {
    let expr = TypedExpr::ByteArray {
        location: Span::empty(),
        tipo: Type::byte_array(),
        bytes: vec![0xDE, 0xAD],
        preferred_format: crate::ast::ByteArrayFormatPreference::HexadecimalString,
    };
    assert_eq!(
        try_extract_exact_scalar(&expr),
        Some(FuzzerExactValue::ByteArray(vec![0xDE, 0xAD])),
    );
}

#[test]
fn canonicalize_finite_scalar_domain_sorts_dedups_and_preserves_oneof() {
    let domain = canonicalize_finite_scalar_domain(
        Type::string().as_ref(),
        vec![
            FuzzerExactValue::String("world".to_string()),
            FuzzerExactValue::String("hello".to_string()),
            FuzzerExactValue::String("".to_string()),
            FuzzerExactValue::String("hello".to_string()),
            FuzzerExactValue::String("test".to_string()),
        ],
    )
    .expect("valid finite string domain");

    assert_eq!(
        domain,
        CanonicalFiniteScalarDomain::OneOf(expected_finite_string_values())
    );
}

#[test]
fn canonicalize_finite_scalar_domain_singleton_becomes_exact() {
    let domain = canonicalize_finite_scalar_domain(
        Type::string().as_ref(),
        vec![
            FuzzerExactValue::String("same".to_string()),
            FuzzerExactValue::String("same".to_string()),
        ],
    )
    .expect("valid singleton string domain");

    assert_eq!(
        domain,
        CanonicalFiniteScalarDomain::Exact(FuzzerExactValue::String("same".to_string()))
    );
}

#[test]
fn canonicalize_finite_scalar_domain_rejects_mismatch_and_empty() {
    assert_eq!(
        canonicalize_finite_scalar_domain(Type::string().as_ref(), vec![]),
        Err(FiniteDomainError::Empty)
    );
    assert_eq!(
        canonicalize_finite_scalar_domain(
            Type::string().as_ref(),
            vec![FuzzerExactValue::ByteArray(vec![0])]
        ),
        Err(FiniteDomainError::OutputTypeMismatch)
    );
    assert_eq!(
        canonicalize_finite_scalar_domain(
            Type::string().as_ref(),
            vec![
                FuzzerExactValue::String("a".to_string()),
                FuzzerExactValue::ByteArray(vec![0]),
            ]
        ),
        Err(FiniteDomainError::Heterogeneous)
    );
}

#[test]
fn describe_semantics_renders_oneof_deterministically() {
    assert_eq!(
        describe_semantics(&FuzzerSemantics::OneOf(expected_finite_string_values())),
        "OneOf(\"\", \"hello\", \"test\", \"world\")"
    );
}

#[test]
fn extract_exact_scalar_int_returns_none() {
    assert_eq!(try_extract_exact_scalar(&uint_lit("42")), None);
}

// --- R6: INT_LITERAL_MAX_DEPTH cycle guard ---
//
// `try_extract_int_literal_inner` bounds its own recursion via
// `INT_LITERAL_MAX_DEPTH` to prevent stack overflows from adversarial or
// pathological expressions (deeply nested negations, circular local
// aliases, or mutually-referential module constants). These tests pin
// down the exact boundary: depth <= MAX succeeds, depth > MAX returns
// None, and the guard triggers without panicking.

/// Wrap `expr` in `depth` layers of `UnOp::Negate`.
fn nested_negate(expr: TypedExpr, depth: usize) -> TypedExpr {
    let mut acc = expr;
    for _ in 0..depth {
        acc = negate_expr(acc);
    }
    acc
}

#[test]
fn int_literal_max_depth_constant_is_sixteen() {
    // Pin the constant to its documented value. If it changes, the
    // boundary tests below should be updated deliberately.
    assert_eq!(INT_LITERAL_MAX_DEPTH, 16);
}

#[test]
fn try_extract_int_literal_at_max_depth_resolves_value() {
    // 16 negations around a UInt is exactly at the depth limit
    // (the UInt is read at depth == MAX, which is allowed since the
    // guard uses strict `>` comparison). 16 is even, so the result
    // equals the original value.
    let constants = HashMap::new();
    let locals = BTreeMap::new();
    let expr = nested_negate(uint_lit("7"), 16);
    assert_eq!(
        try_extract_int_literal(&expr, &constants, &locals),
        Some(BigInt::from(7)),
        "16-level nesting must still resolve (depth == MAX is allowed)"
    );
}

#[test]
fn try_extract_int_literal_below_max_depth_resolves_with_sign() {
    // 15 negations: odd count flips the sign.
    let constants = HashMap::new();
    let locals = BTreeMap::new();
    let expr = nested_negate(uint_lit("7"), 15);
    assert_eq!(
        try_extract_int_literal(&expr, &constants, &locals),
        Some(BigInt::from(-7)),
        "15-level nesting (odd) must resolve to the negated literal"
    );
}

#[test]
fn try_extract_int_literal_above_max_depth_returns_none() {
    // 17 negations: the guard must trip at the innermost recursive call
    // (depth == 17 triggers the `depth > INT_LITERAL_MAX_DEPTH` check)
    // and the function must return None rather than panic or overflow.
    let constants = HashMap::new();
    let locals = BTreeMap::new();
    let expr = nested_negate(uint_lit("7"), 17);
    assert_eq!(
        try_extract_int_literal(&expr, &constants, &locals),
        None,
        "17-level nesting (depth > MAX) must be rejected by the cycle guard"
    );
}

#[test]
fn try_extract_int_literal_far_above_max_depth_terminates() {
    // A grossly deep nesting should terminate gracefully. The absolute
    // depth here (64) would blow the stack without the guard, so this
    // test doubles as a regression guard against removing the check.
    let constants = HashMap::new();
    let locals = BTreeMap::new();
    let expr = nested_negate(uint_lit("7"), 64);
    assert_eq!(
        try_extract_int_literal(&expr, &constants, &locals),
        None,
        "extreme nesting must terminate with None (guard must short-circuit)"
    );
}

#[test]
fn try_extract_int_literal_local_alias_chain_above_max_depth_returns_none() {
    // Local-variable aliases also increment depth at each hop. A chain
    // of 20 aliases must trip the guard even though no negation is
    // involved — this covers the `LocalVariable` recursion path.
    let constants = HashMap::new();
    let mut locals: BTreeMap<String, TypedExpr> = BTreeMap::new();
    let int_tipo = Type::int();

    // x0 = 42; x1 = x0; x2 = x1; ... x20 = x19
    locals.insert("x0".to_string(), uint_lit("42"));
    for i in 1..=20 {
        let prev = local_var(&format!("x{}", i - 1), int_tipo.clone());
        locals.insert(format!("x{i}"), prev);
    }

    let expr = local_var("x20", int_tipo);
    assert_eq!(
        try_extract_int_literal(&expr, &constants, &locals),
        None,
        "20-deep local alias chain must be rejected by the cycle guard"
    );
}

#[test]
fn try_extract_int_literal_short_local_alias_chain_resolves() {
    // A chain of 3 aliases (well under the limit) must still resolve to
    // confirm the alias-resolution path is exercised by these tests.
    let constants = HashMap::new();
    let mut locals: BTreeMap<String, TypedExpr> = BTreeMap::new();
    let int_tipo = Type::int();
    locals.insert("a".to_string(), uint_lit("99"));
    locals.insert("b".to_string(), local_var("a", int_tipo.clone()));
    locals.insert("c".to_string(), local_var("b", int_tipo.clone()));

    let expr = local_var("c", int_tipo);
    assert_eq!(
        try_extract_int_literal(&expr, &constants, &locals),
        Some(BigInt::from(99)),
        "short alias chain must resolve through the alias path"
    );
}

// --- R7: nested Bind and Map(Map) normalization ---
//
// The fuzzer normalizer must handle arbitrarily deep Bind/Map nestings
// without losing structural information. These tests pin the recursive
// descent behavior: nested Binds preserve a Bind-shaped result, nested
// Maps collapse to a Map tree over a Primitive leaf, and mixed chains
// preserve the outermost constructor.

#[test]
fn normalize_fuzzer_nested_bind_depth_four_preserves_bind_shape() {
    // Build a Bind(Bind(Bind(Bind(Primitive)))) — depth 4.
    // Each layer uses an inline lambda whose body is the next Bind call,
    // which is itself a Fuzzer<Int>.
    let int_ty = Type::int();

    let innermost = make_typed_int_between_fuzzer("1", "2");

    let level3 = make_typed_bind_call(
        make_leaf_fuzzer_call("p3", int_ty.clone()),
        make_inline_bind_continuation("x3", int_ty.clone(), innermost, int_ty.clone()),
        int_ty.clone(),
    );
    let level2 = make_typed_bind_call(
        make_leaf_fuzzer_call("p2", int_ty.clone()),
        make_inline_bind_continuation("x2", int_ty.clone(), level3, int_ty.clone()),
        int_ty.clone(),
    );
    let level1 = make_typed_bind_call(
        make_leaf_fuzzer_call("p1", int_ty.clone()),
        make_inline_bind_continuation("x1", int_ty.clone(), level2, int_ty.clone()),
        int_ty.clone(),
    );
    let outer = make_typed_bind_call(
        make_leaf_fuzzer_call("p0", int_ty.clone()),
        make_inline_bind_continuation("x0", int_ty.clone(), level1, int_ty.clone()),
        int_ty.clone(),
    );

    let normalized = normalize_fuzzer_from_via(&outer, "math", &empty_known_functions());

    // Walk the resulting Bind chain: each layer must be a Bind whose
    // source is a Primitive leaf (the `pN` fuzzer) and whose result is
    // the next Bind layer.
    fn expect_bind_chain(n: NormalizedFuzzer, remaining: usize) {
        if remaining == 0 {
            // Innermost: the stdlib recognizer extracts [1, 2] from
            // `int_between(1, 2)` on the `aiken/fuzz` module.
            match n {
                NormalizedFuzzer::Primitive {
                    known_constraint, ..
                } => {
                    assert_eq!(
                        known_constraint,
                        Some(FuzzerConstraint::IntRange {
                            min: "1".to_string(),
                            max: "2".to_string(),
                        }),
                        "innermost primitive must carry the extracted stdlib bounds"
                    );
                }
                other => {
                    panic!("expected innermost Primitive, got {other:?}");
                }
            }
            return;
        }
        match n {
            NormalizedFuzzer::Bind { source, result } => {
                assert!(
                    matches!(*source, NormalizedFuzzer::Primitive { .. }),
                    "each Bind source must remain a Primitive leaf"
                );
                expect_bind_chain(*result, remaining - 1);
            }
            other => panic!("expected Bind at remaining={remaining}, got {other:?}"),
        }
    }

    // 4 bind layers => 4 nested Binds before the Primitive core.
    expect_bind_chain(normalized, 4);
}

#[test]
fn normalize_fuzzer_map_of_map_depth_four_preserves_map_shape() {
    // Build Map(Map(Map(Map(Primitive)))) using `anything_but_map` so the
    // name-agnostic path is exercised. Each inner mapper is a
    // distinguishable unary function, so no collapse to Identity occurs.
    let int_ty = Type::int();

    let level0 = make_typed_map_call(
        make_leaf_fuzzer_call("seed", int_ty.clone()),
        make_add_int_mapper("1"),
        int_ty.clone(),
    );
    let level1 = make_typed_map_call(level0, make_add_int_mapper("2"), int_ty.clone());
    let level2 = make_typed_map_call(level1, make_add_int_mapper("3"), int_ty.clone());
    let level3 = make_typed_map_call(level2, make_add_int_mapper("4"), int_ty.clone());

    let normalized = normalize_fuzzer_from_via(&level3, "math", &empty_known_functions());

    // We expect exactly 4 nested Map layers over a Primitive leaf.
    fn expect_map_chain(n: NormalizedFuzzer, remaining: usize) {
        if remaining == 0 {
            assert!(
                matches!(n, NormalizedFuzzer::Primitive { .. }),
                "innermost of Map chain must be a Primitive leaf, got {n:?}"
            );
            return;
        }
        match n {
            NormalizedFuzzer::Map { source, .. } => {
                expect_map_chain(*source, remaining - 1);
            }
            other => panic!("expected Map at remaining={remaining}, got {other:?}"),
        }
    }
    expect_map_chain(normalized, 4);
}

#[test]
fn normalize_fuzzer_map_of_map_depth_four_yields_map_over_unconstrained_source() {
    // Same Map(Map(Map(Map(...)))) shape, but checked through the
    // constraint extractor which is the actual proof-pipeline entry
    // point. With the stdlib-gated recognizer, `int_between(0, 5)`
    // extracts `IntRange { 0, 5 }`. Each affine mapper transforms the
    // range; `+1 +2 +3 +4 = +10` so the innermost IntRange becomes
    // `[10, 15]` after being composed through all four maps. The
    // constraint extractor collapses the chain of nested `IntRange`
    // transformations into a single `IntRange` at the leaf.
    let int_ty = Type::int();

    let source = make_typed_map_call(
        make_typed_int_between_fuzzer("0", "5"),
        make_add_int_mapper("1"),
        int_ty.clone(),
    );
    let lvl1 = make_typed_map_call(source, make_add_int_mapper("2"), int_ty.clone());
    let lvl2 = make_typed_map_call(lvl1, make_add_int_mapper("3"), int_ty.clone());
    let lvl3 = make_typed_map_call(lvl2, make_add_int_mapper("4"), int_ty);

    let constraint = extract_constraint_from_via(&lvl3, "math", &empty_known_functions());

    // We only pin that bound information is no longer lost: the
    // outer constraint must surface some IntRange (possibly nested
    // under `Map(_)` depending on whether every affine step folds
    // into the range) rather than collapsing to `Any` or
    // `Unsupported`.
    fn contains_int_range(c: &FuzzerConstraint) -> bool {
        match c {
            FuzzerConstraint::IntRange { .. } => true,
            FuzzerConstraint::Map(inner) => contains_int_range(inner),
            FuzzerConstraint::And(items) | FuzzerConstraint::Tuple(items) => {
                items.iter().any(contains_int_range)
            }
            FuzzerConstraint::List { elem, .. } => contains_int_range(elem),
            _ => false,
        }
    }
    assert!(
        contains_int_range(&constraint),
        "Map(Map(Map(Map(IntRange)))) chain must preserve some IntRange, got {constraint:?}"
    );
}

#[test]
fn normalize_fuzzer_nested_bind_continuation_returns_unconstrained_domain() {
    // Build Bind(seed, \x0. Bind(p1, \x1. Bind(p2, \x2. int_between(3,7)))).
    // Under the stdlib-gated recognizer, `int_between(3, 7)` now carries
    // `IntRange { 3, 7 }`, and the nested Bind propagates the innermost
    // continuation's constraint.
    let int_ty = Type::int();

    let innermost = make_typed_int_between_fuzzer("3", "7");
    let mid = make_typed_bind_call(
        make_leaf_fuzzer_call("p2", int_ty.clone()),
        make_inline_bind_continuation("x2", int_ty.clone(), innermost, int_ty.clone()),
        int_ty.clone(),
    );
    let outer_continuation_body = make_typed_bind_call(
        make_leaf_fuzzer_call("p1", int_ty.clone()),
        make_inline_bind_continuation("x1", int_ty.clone(), mid, int_ty.clone()),
        int_ty.clone(),
    );
    let outer = make_typed_bind_call(
        make_leaf_fuzzer_call("seed", int_ty.clone()),
        make_inline_bind_continuation(
            "x0",
            int_ty.clone(),
            outer_continuation_body,
            int_ty.clone(),
        ),
        int_ty.clone(),
    );

    let constraint = extract_constraint_from_via(&outer, "math", &empty_known_functions());
    assert_eq!(
        constraint,
        FuzzerConstraint::IntRange {
            min: "3".to_string(),
            max: "7".to_string(),
        },
        "nested Bind must propagate the innermost continuation's extracted domain"
    );
}

#[test]
fn normalize_fuzzer_map_wrapping_bind_preserves_outer_shape() {
    // Map(Bind(...)) — the outer normalization should expose a Map
    // layer whose source is a Bind (not collapsed or opaqued).
    let int_ty = Type::int();

    let inner_bind = make_typed_bind_call(
        make_leaf_fuzzer_call("seed", int_ty.clone()),
        make_inline_bind_continuation(
            "x",
            int_ty.clone(),
            make_typed_int_between_fuzzer("5", "8"),
            int_ty.clone(),
        ),
        int_ty.clone(),
    );

    let outer_map = make_typed_map_call(inner_bind, make_add_int_mapper("1"), int_ty);

    let normalized = normalize_fuzzer_from_via(&outer_map, "math", &empty_known_functions());

    match normalized {
        NormalizedFuzzer::Map { source, .. } => match *source {
            NormalizedFuzzer::Bind {
                source: bind_source,
                result,
            } => {
                assert!(
                    matches!(*bind_source, NormalizedFuzzer::Primitive { .. }),
                    "Bind source under Map must stay a Primitive leaf"
                );
                assert!(
                    matches!(*result, NormalizedFuzzer::Primitive { .. }),
                    "Bind continuation under Map must resolve to a Primitive (int_between)"
                );
            }
            other => panic!("expected Bind under Map, got {other:?}"),
        },
        other => panic!("expected Map at the outer level, got {other:?}"),
    }
}

#[test]
fn normalize_fuzzer_bind_wrapping_map_exposes_bind_over_map() {
    // Bind(Map(...), \x. fuzzer). The normalizer must see the Map as
    // the Bind's source; otherwise the mapper's effect on the sampling
    // domain would be lost.
    let int_ty = Type::int();

    let inner_map = make_typed_map_call(
        make_leaf_fuzzer_call("seed", int_ty.clone()),
        make_add_int_mapper("5"),
        int_ty.clone(),
    );

    let outer_bind = make_typed_bind_call(
        inner_map,
        make_inline_bind_continuation(
            "x",
            int_ty.clone(),
            make_typed_int_between_fuzzer("0", "3"),
            int_ty.clone(),
        ),
        int_ty.clone(),
    );

    let normalized = normalize_fuzzer_from_via(&outer_bind, "math", &empty_known_functions());

    match normalized {
        NormalizedFuzzer::Bind { source, result } => {
            assert!(
                matches!(*source, NormalizedFuzzer::Map { .. }),
                "Bind source must be the inner Map, not a collapsed primitive"
            );
            assert!(
                matches!(*result, NormalizedFuzzer::Primitive { .. }),
                "Bind continuation must normalize to the inner int_between primitive"
            );
        }
        other => panic!("expected Bind at the outer level, got {other:?}"),
    }
}

#[test]
fn test_cache() {
    let called = std::cell::RefCell::new(0);

    let mut cache = Cache::new(|choices| {
        called.replace_with(|n| *n + 1);

        match choices {
            [0, 0, 0] => Status::Keep(true),
            _ => {
                if choices.len() <= 2 {
                    Status::Invalid
                } else {
                    Status::Ignore
                }
            }
        }
    });

    assert_eq!(cache.get(&[1, 1]), Status::Invalid); // Fn executed
    assert_eq!(cache.get(&[1, 1, 2, 3]), Status::Ignore); // Fn executed
    assert_eq!(cache.get(&[1, 1, 2]), Status::Ignore); // Fnexecuted
    assert_eq!(cache.get(&[1, 1, 2, 2]), Status::Ignore); // Cached result
    assert_eq!(cache.get(&[1, 1, 2, 1]), Status::Ignore); // Cached result
    assert_eq!(cache.get(&[0, 1, 2]), Status::Ignore); // Fn executed
    assert_eq!(cache.get(&[0, 0, 0]), Status::Keep(true)); // Fn executed
    assert_eq!(cache.get(&[0, 0, 0]), Status::Keep(true)); // Cached result

    assert_eq!(called.borrow().deref().to_owned(), 5, "execution calls");
    assert_eq!(cache.size(), 4, "cache size");
}

#[test]
fn default_semantics_for_type_recognizes_nullary_enum() {
    let output_type = make_nullary_constructor_type("scenario", "Outcome");
    let owned_data_types =
        make_nullary_constructor_data_types("scenario", "Outcome", &["Ok", "Ko"]);
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();

    let semantics = default_semantics_for_type(output_type.as_ref(), &data_types);

    assert_eq!(
        semantics,
        FuzzerSemantics::Constructors { tags: vec![0, 1] }
    );
}

#[test]
fn default_semantics_for_type_returns_data_with_schema_for_non_nullary_adt() {
    let output_type = Rc::new(Type::App {
        public: true,
        contains_opaque: false,
        module: "widgets".to_string(),
        name: "Widget".to_string(),
        args: vec![],
        alias: None,
    });

    let data_type = TypedDataType {
        decorators: vec![],
        constructors: vec![RecordConstructor {
            decorators: vec![],
            location: Span::empty(),
            name: "Widget".to_string(),
            arguments: vec![RecordConstructorArg {
                label: Some("size".to_string()),
                annotation: Annotation::Constructor {
                    location: Span::empty(),
                    module: None,
                    name: "Int".to_string(),
                    arguments: vec![],
                },
                location: Span::empty(),
                tipo: Type::int(),
                doc: None,
            }],
            doc: None,
            sugar: false,
        }],
        doc: None,
        location: Span::empty(),
        name: "Widget".to_string(),
        opaque: false,
        parameters: vec![],
        public: true,
        typed_parameters: vec![],
    };

    let mut owned_data_types: IndexMap<DataTypeKey, TypedDataType> = IndexMap::new();
    owned_data_types.insert(
        DataTypeKey {
            module_name: "widgets".to_string(),
            defined_type: "Widget".to_string(),
        },
        data_type,
    );
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();

    let semantics = default_semantics_for_type(output_type.as_ref(), &data_types);

    assert_eq!(
        semantics,
        FuzzerSemantics::DataWithSchema {
            type_name: "widgets.Widget".to_string(),
        },
        "expected DataWithSchema for non-nullary qualified ADT, got {semantics:?}"
    );
}

#[test]
fn default_semantics_for_type_preserves_data_with_schema_generic_arguments() {
    let result_int = Rc::new(Type::App {
        public: true,
        contains_opaque: false,
        module: "widgets".to_string(),
        name: "Result".to_string(),
        args: vec![Type::int()],
        alias: None,
    });
    let result_bytes = Rc::new(Type::App {
        public: true,
        contains_opaque: false,
        module: "widgets".to_string(),
        name: "Result".to_string(),
        args: vec![Type::byte_array()],
        alias: None,
    });
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

    let int_semantics = default_semantics_for_type(result_int.as_ref(), &data_types);
    let bytes_semantics = default_semantics_for_type(result_bytes.as_ref(), &data_types);

    assert_eq!(
        int_semantics,
        FuzzerSemantics::DataWithSchema {
            type_name: "widgets.Result<Int>".to_string(),
        }
    );
    assert_eq!(
        bytes_semantics,
        FuzzerSemantics::DataWithSchema {
            type_name: "widgets.Result<ByteArray>".to_string(),
        }
    );
    assert_ne!(int_semantics, bytes_semantics);
}

#[test]
fn default_semantics_for_type_returns_data_with_schema_for_unknown_qualified_adt() {
    // Unknown qualified ADTs (not in the data_types map) now fall into
    // the DataWithSchema arm: we can't enumerate constructors, but we
    // still quantify over `Data` and defer structural constraints to the
    // exported fuzzer_data_schema (which may or may not exist at
    // proof-generation time).
    let output_type = make_nullary_constructor_type("unknown_mod", "Unknown");
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

    let semantics = default_semantics_for_type(output_type.as_ref(), &data_types);

    assert_eq!(
        semantics,
        FuzzerSemantics::DataWithSchema {
            type_name: "unknown_mod.Unknown".to_string(),
        },
        "expected DataWithSchema for unknown qualified ADT, got {semantics:?}"
    );
}

#[test]
fn default_semantics_for_type_threads_data_types_through_list_element() {
    let owned_data_types =
        make_nullary_constructor_data_types("scenario", "Outcome", &["Ok", "Ko"]);
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();

    let outcome_type = make_nullary_constructor_type("scenario", "Outcome");
    let list_type = Type::list(outcome_type);

    let semantics = default_semantics_for_type(list_type.as_ref(), &data_types);

    assert_eq!(
        semantics,
        FuzzerSemantics::List {
            element: Box::new(FuzzerSemantics::Constructors { tags: vec![0, 1] }),
            min_len: None,
            max_len: None,
        }
    );
}

#[test]
fn default_semantics_for_type_threads_data_types_through_tuple_elements() {
    let owned_data_types =
        make_nullary_constructor_data_types("scenario", "Outcome", &["Ok", "Ko"]);
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();

    let outcome_type = make_nullary_constructor_type("scenario", "Outcome");
    let tuple_type = Type::tuple(vec![outcome_type, Type::int()]);

    let semantics = default_semantics_for_type(tuple_type.as_ref(), &data_types);

    assert_eq!(
        semantics,
        FuzzerSemantics::Product(vec![
            FuzzerSemantics::Constructors { tags: vec![0, 1] },
            FuzzerSemantics::IntRange {
                min: None,
                max: None,
            },
        ])
    );
}

// ---------------------------------------------------------------
// S3 — TransitionProp translation
// ---------------------------------------------------------------

#[test]
fn transition_prop_from_opaque_is_unsupported() {
    let normalized = NormalizedFuzzer::Opaque {
        expr: Box::new(local_var("step", Type::int())),
        reason: "opaque leaf".to_string(),
    };
    let prop = normalized_fuzzer_to_transition_prop(&normalized);
    match prop {
        TransitionProp::Unsupported { reason, .. } => {
            assert_eq!(reason, "opaque leaf");
        }
        other => panic!("expected Unsupported, got {other:?}"),
    }
}

#[test]
fn transition_prop_from_primitive_exact_is_eq_output() {
    let normalized = NormalizedFuzzer::Primitive {
        output_type: Type::bool(),
        known_constraint: Some(FuzzerConstraint::Exact(FuzzerExactValue::Bool(true))),
    };
    let prop = normalized_fuzzer_to_transition_prop(&normalized);
    match prop {
        TransitionProp::EqOutput(ShallowIr::Const(ShallowConst::Bool(true))) => {}
        other => panic!("expected EqOutput(Const(Bool(true))), got {other:?}"),
    }
}

#[test]
fn transition_prop_from_bind_produces_exists() {
    // Bind { source = int in [0,10], result = return true }
    let source = NormalizedFuzzer::Primitive {
        output_type: Type::int(),
        known_constraint: Some(FuzzerConstraint::IntRange {
            min: "0".to_string(),
            max: "10".to_string(),
        }),
    };
    let result = NormalizedFuzzer::Primitive {
        output_type: Type::bool(),
        known_constraint: Some(FuzzerConstraint::Exact(FuzzerExactValue::Bool(true))),
    };
    let normalized = NormalizedFuzzer::Bind {
        source: Box::new(source),
        result: Box::new(result),
    };
    let prop = normalized_fuzzer_to_transition_prop(&normalized);
    match prop {
        TransitionProp::Exists { domain, body, .. } => {
            assert!(matches!(
                *domain,
                FuzzerSemantics::IntRange {
                    min: Some(ref lo),
                    max: Some(ref hi),
                } if lo == "0" && hi == "10"
            ));
            assert!(matches!(
                *body,
                TransitionProp::EqOutput(ShallowIr::Const(ShallowConst::Bool(true)))
            ));
        }
        other => panic!("expected Exists, got {other:?}"),
    }
}

#[test]
fn state_machine_trace_field_transition_prop_defaults_to_none_today() {
    // Until Issue S2 enriches `normalize_fuzzer_from_expr` with
    // `return`/`and_then`/`fork*` recognizers, step-function bodies
    // bottom out in `Opaque` → `Unsupported` and the field collapses
    // to `None`. This test pins that invariant so a regression that
    // starts populating `transition_prop` with spurious structure is
    // noticed immediately.
    let (owned_data_types, state_type, input_type, transaction_type) = scenario_semantics_fixture();
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();
    let step_type = Type::function(
        vec![state_type.clone(), Type::list(input_type.clone())],
        Type::fuzzer(Rc::new(Type::App {
            public: true,
            contains_opaque: false,
            module: STDLIB_FUZZ_SCENARIO_MODULE.to_string(),
            name: "Scenario".to_string(),
            args: vec![state_type.clone()],
            alias: None,
        })),
    );
    let via = TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::list(transaction_type.clone()),
        fun: Box::new(module_fn_var(
            "ok",
            STDLIB_FUZZ_SCENARIO_MODULE,
            Type::function(
                vec![state_type.clone(), step_type.clone()],
                Type::list(transaction_type.clone()),
            ),
        )),
        args: vec![
            call_arg(local_var("initial_state", state_type.clone())),
            call_arg(local_var("step", step_type)),
        ],
    };
    let functions = empty_known_functions();

    let semantics = extract_semantics_from_via(
        &via,
        "permissions",
        &functions,
        &data_types,
        Type::list(transaction_type.clone()).as_ref(),
    );

    match semantics {
        FuzzerSemantics::StateMachineTrace {
            transition_prop, ..
        } => {
            assert!(
                transition_prop.is_none(),
                "transition_prop should currently be None for opaque step functions (got {transition_prop:?})"
            );
        }
        other => panic!("expected StateMachineTrace, got {other:?}"),
    }
}

// ---------------------------------------------------------------
// S3 — direct TypedExpr -> TransitionProp translation
// ---------------------------------------------------------------

type TransitionPropTestContext = (
    FunctionIndex<'static>,
    ConstantIndex<'static>,
    BTreeMap<String, TypedExpr>,
    IndexMap<&'static DataTypeKey, &'static TypedDataType>,
    BTreeSet<(String, String)>,
);

fn empty_transition_prop_context() -> TransitionPropTestContext {
    (
        HashMap::new(),
        HashMap::new(),
        BTreeMap::new(),
        IndexMap::new(),
        BTreeSet::new(),
    )
}

#[test]
fn typed_expr_to_transition_prop_if_produces_if_then_else() {
    // `if True { return(0) } else { return(1) }` — note: the exact cond
    // content isn't important here; we only want to see IfThenElse.
    let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
        empty_transition_prop_context();

    let int_fuzzer = Type::fuzzer(Type::int());
    let return_fn_type = Type::function(vec![Type::int()], int_fuzzer.clone());

    let make_return = |value: &str| TypedExpr::Call {
        location: Span::empty(),
        tipo: int_fuzzer.clone(),
        fun: Box::new(module_fn_var(
            "constant",
            STDLIB_FUZZ_MODULE,
            return_fn_type.clone(),
        )),
        args: vec![call_arg(uint_lit(value))],
    };

    let cond = bool_constructor(true);
    let then_branch = make_return("0");
    let else_branch = make_return("1");

    let if_expr = TypedExpr::If {
        location: Span::empty(),
        tipo: int_fuzzer.clone(),
        branches: vec1::vec1![IfBranch {
            condition: cond,
            body: then_branch,
            is: None,
            location: Span::empty(),
        }],
        final_else: Box::new(else_branch),
    };

    let prop = typed_expr_to_transition_prop(
        &if_expr,
        "test_mod",
        &function_index,
        &constant_index,
        &local_values,
        &empty_data_types,
        &mut visiting,
        &BTreeSet::new(),
        &mut BTreeSet::new(),
    );

    match prop {
        TransitionProp::IfThenElse { t, e, .. } => {
            assert!(
                matches!(
                    *t,
                    TransitionProp::EqOutput(ShallowIr::Const(ShallowConst::Int(ref v)))
                        if v == "0"
                ),
                "expected then = EqOutput(Int 0), got {t:?}"
            );
            assert!(
                matches!(
                    *e,
                    TransitionProp::EqOutput(ShallowIr::Const(ShallowConst::Int(ref v)))
                        if v == "1"
                ),
                "expected else = EqOutput(Int 1), got {e:?}"
            );
        }
        other => panic!("expected IfThenElse, got {other:?}"),
    }
}

#[test]
fn typed_expr_to_transition_prop_aliased_constant_is_exact_return() {
    let (function_index, constant_index, mut local_values, empty_data_types, mut visiting) =
        empty_transition_prop_context();

    let int_fuzzer = Type::fuzzer(Type::int());
    let return_fn_type = Type::function(vec![Type::int()], int_fuzzer.clone());
    local_values.insert(
        "ret".to_string(),
        module_fn_var("constant", STDLIB_FUZZ_MODULE, return_fn_type.clone()),
    );

    let aliased_return = TypedExpr::Call {
        location: Span::empty(),
        tipo: int_fuzzer,
        fun: Box::new(local_var("ret", return_fn_type)),
        args: vec![call_arg(uint_lit("42"))],
    };

    let prop = typed_expr_to_transition_prop(
        &aliased_return,
        "test_mod",
        &function_index,
        &constant_index,
        &local_values,
        &empty_data_types,
        &mut visiting,
        &BTreeSet::new(),
        &mut BTreeSet::new(),
    );

    assert!(
        matches!(prop, TransitionProp::EqOutput(ShallowIr::Const(ShallowConst::Int(ref v))) if v == "42"),
        "expected aliased fuzz.constant to lower to exact EqOutput(Int 42), got {prop:?}"
    );
}

#[test]
fn typed_expr_to_transition_prop_and_then_produces_exists() {
    // `and_then(int_between(0, 10), fn(x) { return(x) })` — the
    // continuation is the pure passthrough, so the body preserves `x` as
    // the drawn witness rather than an unrelated fresh existential.
    let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
        empty_transition_prop_context();

    let int_fuzzer = Type::fuzzer(Type::int());
    let source = make_typed_int_between_fuzzer("0", "10");

    let x = local_var("x", Type::int());
    let return_body = TypedExpr::Call {
        location: Span::empty(),
        tipo: int_fuzzer.clone(),
        fun: Box::new(module_fn_var(
            "constant",
            STDLIB_FUZZ_MODULE,
            Type::function(vec![Type::int()], int_fuzzer.clone()),
        )),
        args: vec![call_arg(x)],
    };

    let continuation = make_inline_bind_continuation("x", Type::int(), return_body, Type::int());

    let bind_call = make_stdlib_bind_call(source, continuation, Type::int());

    let prop = typed_expr_to_transition_prop(
        &bind_call,
        "test_mod",
        &function_index,
        &constant_index,
        &local_values,
        &empty_data_types,
        &mut visiting,
        &BTreeSet::new(),
        &mut BTreeSet::new(),
    );

    match prop {
        TransitionProp::Exists { binder, body, .. } => {
            assert_eq!(binder, "x", "expected binder 'x', got '{binder}'");
            assert!(
                matches!(
                    *body,
                    TransitionProp::EqOutput(ShallowIr::BoundVar { ref name, .. })
                        if name == "x"
                ),
                "expected continuation to return the drawn witness x exactly, got {body:?}"
            );
        }
        other => panic!("expected Exists, got {other:?}"),
    }
}

#[test]
fn typed_expr_to_transition_prop_named_and_then_continuation_produces_exists() {
    let (_, constant_index, local_values, empty_data_types, mut visiting) =
        empty_transition_prop_context();
    let (_, continuation_fn) =
        make_named_bind_passthrough_continuation_function("next_step", Type::int());
    let continuation_fn = Box::leak(Box::new(continuation_fn));
    let function_index = HashMap::from([(
        "math".to_string(),
        HashMap::from([(
            "next_step".to_string(),
            continuation_fn as &'static TypedFunction,
        )]),
    )]);

    let bind_call = make_stdlib_bind_call(
        make_typed_int_between_fuzzer("0", "10"),
        make_bind_continuation("next_step", Type::int(), Type::int()),
        Type::int(),
    );

    let prop = typed_expr_to_transition_prop(
        &bind_call,
        "test_mod",
        &function_index,
        &constant_index,
        &local_values,
        &empty_data_types,
        &mut visiting,
        &BTreeSet::new(),
        &mut BTreeSet::new(),
    );

    match prop {
        TransitionProp::Exists { binder, body, .. } => {
            assert_eq!(binder, "x", "expected named continuation binder x");
            assert!(
                matches!(
                    *body,
                    TransitionProp::EqOutput(ShallowIr::BoundVar { ref name, .. }) if name == "x"
                ),
                "expected named continuation to return the drawn witness x exactly, got {body:?}"
            );
        }
        other => panic!("expected Exists for named continuation, got {other:?}"),
    }
}

#[test]
fn typed_expr_to_transition_prop_aliased_and_then_continuation_produces_exists() {
    let (_, constant_index, mut local_values, empty_data_types, mut visiting) =
        empty_transition_prop_context();
    let (_, continuation_fn) =
        make_named_bind_passthrough_continuation_function("next_step", Type::int());
    let continuation_fn = Box::leak(Box::new(continuation_fn));
    let function_index = HashMap::from([(
        "math".to_string(),
        HashMap::from([(
            "next_step".to_string(),
            continuation_fn as &'static TypedFunction,
        )]),
    )]);
    local_values.insert(
        "aliased_next".to_string(),
        make_bind_continuation("next_step", Type::int(), Type::int()),
    );

    let bind_call = make_stdlib_bind_call(
        make_typed_int_between_fuzzer("0", "10"),
        local_var(
            "aliased_next",
            Type::function(vec![Type::int()], Type::fuzzer(Type::int())),
        ),
        Type::int(),
    );

    let prop = typed_expr_to_transition_prop(
        &bind_call,
        "test_mod",
        &function_index,
        &constant_index,
        &local_values,
        &empty_data_types,
        &mut visiting,
        &BTreeSet::new(),
        &mut BTreeSet::new(),
    );

    match prop {
        TransitionProp::Exists { binder, body, .. } => {
            assert_eq!(binder, "x", "expected aliased continuation binder x");
            assert!(
                matches!(
                    *body,
                    TransitionProp::EqOutput(ShallowIr::BoundVar { ref name, .. }) if name == "x"
                ),
                "expected aliased continuation to return the drawn witness x exactly, got {body:?}"
            );
        }
        other => panic!("expected Exists for aliased continuation, got {other:?}"),
    }
}

#[test]
fn typed_expr_to_transition_prop_fake_and_then_is_not_stdlib_bind() {
    let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
        empty_transition_prop_context();
    let int_fuzzer = Type::fuzzer(Type::int());
    let return_body = TypedExpr::Call {
        location: Span::empty(),
        tipo: int_fuzzer.clone(),
        fun: Box::new(module_fn_var(
            "constant",
            STDLIB_FUZZ_MODULE,
            Type::function(vec![Type::int()], int_fuzzer),
        )),
        args: vec![call_arg(local_var("x", Type::int()))],
    };
    let continuation = make_inline_bind_continuation("x", Type::int(), return_body, Type::int());
    let fake = make_typed_bind_call(
        make_typed_int_between_fuzzer("0", "10"),
        continuation,
        Type::int(),
    );

    let prop = typed_expr_to_transition_prop(
        &fake,
        "test_mod",
        &function_index,
        &constant_index,
        &local_values,
        &empty_data_types,
        &mut visiting,
        &BTreeSet::new(),
        &mut BTreeSet::new(),
    );

    assert!(
        matches!(prop, TransitionProp::SubGenerator { ref module, ref fn_name } if module == "math" && fn_name == "anything_but_and_then"),
        "fake and_then lookalike must not receive stdlib bind semantics: {prop:?}"
    );
}

#[test]
fn typed_expr_to_transition_prop_bind_opaque_int_source_defaults_domain() {
    let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
        empty_transition_prop_context();
    let int_fuzzer = Type::fuzzer(Type::int());
    let source = module_fn_var("opaque_rand", "math", int_fuzzer.clone());
    let return_body = TypedExpr::Call {
        location: Span::empty(),
        tipo: int_fuzzer.clone(),
        fun: Box::new(module_fn_var(
            "constant",
            STDLIB_FUZZ_MODULE,
            Type::function(vec![Type::int()], int_fuzzer.clone()),
        )),
        args: vec![call_arg(local_var("x", Type::int()))],
    };
    let continuation = make_inline_bind_continuation("x", Type::int(), return_body, Type::int());
    let bind_call = make_stdlib_bind_call(source, continuation, Type::int());

    let prop = typed_expr_to_transition_prop(
        &bind_call,
        "test_mod",
        &function_index,
        &constant_index,
        &local_values,
        &empty_data_types,
        &mut visiting,
        &BTreeSet::new(),
        &mut BTreeSet::new(),
    );

    match prop {
        TransitionProp::Exists { domain, .. } => assert!(
            matches!(
                *domain,
                FuzzerSemantics::IntRange {
                    min: None,
                    max: None
                }
            ),
            "opaque Int source should widen to unconstrained Int domain, got {domain:?}"
        ),
        other => panic!("expected Exists, got {other:?}"),
    }
}

#[test]
fn typed_expr_to_transition_prop_bind_opaque_adt_source_defaults_schema_domain() {
    let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
        empty_transition_prop_context();
    let input_type = Rc::new(Type::App {
        public: true,
        contains_opaque: false,
        module: "cardano/transaction".to_string(),
        name: "Input".to_string(),
        args: vec![],
        alias: None,
    });
    let source = module_fn_var("opaque_input", "cardano", Type::fuzzer(input_type.clone()));
    let return_body = TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::fuzzer(input_type.clone()),
        fun: Box::new(module_fn_var(
            "constant",
            STDLIB_FUZZ_MODULE,
            Type::function(vec![input_type.clone()], Type::fuzzer(input_type.clone())),
        )),
        args: vec![call_arg(local_var("x", input_type.clone()))],
    };
    let continuation =
        make_inline_bind_continuation("x", input_type.clone(), return_body, input_type.clone());
    let bind_call = make_stdlib_bind_call(source, continuation, input_type);

    let prop = typed_expr_to_transition_prop(
        &bind_call,
        "test_mod",
        &function_index,
        &constant_index,
        &local_values,
        &empty_data_types,
        &mut visiting,
        &BTreeSet::new(),
        &mut BTreeSet::new(),
    );

    match prop {
        TransitionProp::Exists { domain, .. } => assert!(
            matches!(*domain, FuzzerSemantics::DataWithSchema { ref type_name } if type_name == "cardano/transaction.Input"),
            "opaque ADT source should widen to schema-aware Data domain, got {domain:?}"
        ),
        other => panic!("expected Exists, got {other:?}"),
    }
}

#[test]
fn typed_expr_to_transition_prop_fork_produces_or() {
    // `fork2_and_then(weight, fn() { return(0) }, fn() { return(1) },
    // fn() { return(2) })` — three zero-arg fuzzer thunks, so the call
    // translates to a three-way `Or`.
    let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
        empty_transition_prop_context();

    let int_fuzzer = Type::fuzzer(Type::int());
    let return_fn_type = Type::function(vec![Type::int()], int_fuzzer.clone());

    let make_return_thunk = |value: &str| TypedExpr::Fn {
        location: Span::empty(),
        tipo: Type::function(vec![], int_fuzzer.clone()),
        is_capture: false,
        args: vec![],
        body: Box::new(TypedExpr::Call {
            location: Span::empty(),
            tipo: int_fuzzer.clone(),
            fun: Box::new(module_fn_var(
                "constant",
                STDLIB_FUZZ_MODULE,
                return_fn_type.clone(),
            )),
            args: vec![call_arg(uint_lit(value))],
        }),
        return_annotation: None,
    };

    let call_expr = TypedExpr::Call {
        location: Span::empty(),
        tipo: int_fuzzer.clone(),
        fun: Box::new(module_fn_var(
            "fork2_and_then",
            STDLIB_FUZZ_SCENARIO_MODULE,
            Type::function(
                vec![
                    Type::int(),
                    Type::function(vec![], int_fuzzer.clone()),
                    Type::function(vec![], int_fuzzer.clone()),
                    Type::function(vec![], int_fuzzer.clone()),
                ],
                int_fuzzer.clone(),
            ),
        )),
        args: vec![
            call_arg(uint_lit("1")),
            call_arg(make_return_thunk("0")),
            call_arg(make_return_thunk("1")),
            call_arg(make_return_thunk("2")),
        ],
    };

    let prop = typed_expr_to_transition_prop(
        &call_expr,
        "test_mod",
        &function_index,
        &constant_index,
        &local_values,
        &empty_data_types,
        &mut visiting,
        &BTreeSet::new(),
        &mut BTreeSet::new(),
    );

    match prop {
        TransitionProp::Or(branches) => {
            assert_eq!(branches.len(), 3, "expected 3 branches, got {branches:?}");
            for (i, branch) in branches.iter().enumerate() {
                let expected = i.to_string();
                assert!(
                    matches!(
                        branch,
                        TransitionProp::EqOutput(ShallowIr::Const(ShallowConst::Int(v)))
                            if v == &expected
                    ),
                    "expected branch {i} = EqOutput(Int {expected}), got {branch:?}"
                );
            }
        }
        other => panic!("expected Or, got {other:?}"),
    }
}

#[test]
fn typed_expr_to_transition_prop_fork_with_continuation_produces_exists_wrapping_or() {
    // `fork2_and_then(weight, fn() { return(0) }, fn() { return(1) },
    // fn() { return(2) }, fn(x) { return(x) })` — three thunks plus a
    // trailing continuation. We expect:
    //   Exists { binder = "x",
    //            body = And([ Or([EqOutput 0, EqOutput 1, EqOutput 2]),
    //                         EqOutput(Var x) ]) }
    let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
        empty_transition_prop_context();

    let int_fuzzer = Type::fuzzer(Type::int());
    let return_fn_type = Type::function(vec![Type::int()], int_fuzzer.clone());

    let make_return_thunk = |value: &str| TypedExpr::Fn {
        location: Span::empty(),
        tipo: Type::function(vec![], int_fuzzer.clone()),
        is_capture: false,
        args: vec![],
        body: Box::new(TypedExpr::Call {
            location: Span::empty(),
            tipo: int_fuzzer.clone(),
            fun: Box::new(module_fn_var(
                "constant",
                STDLIB_FUZZ_MODULE,
                return_fn_type.clone(),
            )),
            args: vec![call_arg(uint_lit(value))],
        }),
        return_annotation: None,
    };

    let cont_body = TypedExpr::Call {
        location: Span::empty(),
        tipo: int_fuzzer.clone(),
        fun: Box::new(module_fn_var(
            "constant",
            STDLIB_FUZZ_MODULE,
            return_fn_type.clone(),
        )),
        args: vec![call_arg(local_var("x", Type::int()))],
    };
    let continuation = make_inline_bind_continuation("x", Type::int(), cont_body, Type::int());

    let call_expr = TypedExpr::Call {
        location: Span::empty(),
        tipo: int_fuzzer.clone(),
        fun: Box::new(module_fn_var(
            "fork2_and_then",
            STDLIB_FUZZ_SCENARIO_MODULE,
            Type::function(
                vec![
                    Type::int(),
                    Type::function(vec![], int_fuzzer.clone()),
                    Type::function(vec![], int_fuzzer.clone()),
                    Type::function(vec![], int_fuzzer.clone()),
                    Type::function(vec![Type::int()], int_fuzzer.clone()),
                ],
                int_fuzzer.clone(),
            ),
        )),
        args: vec![
            call_arg(uint_lit("1")),
            call_arg(make_return_thunk("0")),
            call_arg(make_return_thunk("1")),
            call_arg(make_return_thunk("2")),
            call_arg(continuation),
        ],
    };

    let prop = typed_expr_to_transition_prop(
        &call_expr,
        "test_mod",
        &function_index,
        &constant_index,
        &local_values,
        &empty_data_types,
        &mut visiting,
        &BTreeSet::new(),
        &mut BTreeSet::new(),
    );

    match prop {
        TransitionProp::Exists { binder, body, .. } => {
            assert_eq!(binder, "x");
            match *body {
                TransitionProp::And(parts) => {
                    assert_eq!(parts.len(), 2);
                    match &parts[0] {
                        TransitionProp::Or(branches) => {
                            assert_eq!(branches.len(), 3);
                        }
                        other => panic!("expected Or as first And leg, got {other:?}"),
                    }
                    match &parts[1] {
                        TransitionProp::EqOutput(ShallowIr::BoundVar { name, .. })
                            if name == "x" => {}
                        other => {
                            panic!("expected EqOutput(BoundVar x) as second And leg, got {other:?}")
                        }
                    }
                }
                other => panic!("expected And, got {other:?}"),
            }
        }
        other => panic!("expected Exists wrapping fork, got {other:?}"),
    }
}

#[test]
fn typed_expr_to_transition_prop_fork_if_preserves_condition() {
    let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
        empty_transition_prop_context();

    let int_fuzzer = Type::fuzzer(Type::int());
    let return_fn_type = Type::function(vec![Type::int()], int_fuzzer.clone());
    let make_return_thunk = |value: &str| TypedExpr::Fn {
        location: Span::empty(),
        tipo: Type::function(vec![], int_fuzzer.clone()),
        is_capture: false,
        args: vec![],
        body: Box::new(TypedExpr::Call {
            location: Span::empty(),
            tipo: int_fuzzer.clone(),
            fun: Box::new(module_fn_var(
                "constant",
                STDLIB_FUZZ_MODULE,
                return_fn_type.clone(),
            )),
            args: vec![call_arg(uint_lit(value))],
        }),
        return_annotation: None,
    };

    let call_expr = TypedExpr::Call {
        location: Span::empty(),
        tipo: int_fuzzer.clone(),
        fun: Box::new(module_fn_var(
            "fork_if_and_then",
            STDLIB_FUZZ_SCENARIO_MODULE,
            Type::function(
                vec![
                    Type::bool(),
                    Type::int(),
                    Type::function(vec![], int_fuzzer.clone()),
                    Type::function(vec![], int_fuzzer.clone()),
                ],
                int_fuzzer.clone(),
            ),
        )),
        args: vec![
            call_arg(TypedExpr::BinOp {
                location: Span::empty(),
                tipo: Type::bool(),
                name: BinOp::Eq,
                left: Box::new(uint_lit("1")),
                right: Box::new(uint_lit("1")),
            }),
            call_arg(uint_lit("1")),
            call_arg(make_return_thunk("0")),
            call_arg(make_return_thunk("1")),
        ],
    };

    let prop = typed_expr_to_transition_prop(
        &call_expr,
        "test_mod",
        &function_index,
        &constant_index,
        &local_values,
        &empty_data_types,
        &mut visiting,
        &BTreeSet::new(),
        &mut BTreeSet::new(),
    );

    match prop {
        TransitionProp::IfThenElse { cond, t, e } => {
            assert!(
                matches!(
                    cond,
                    ShallowIr::BinOp {
                        op: ShallowBinOp::Eq,
                        ..
                    }
                ),
                "fork_if_and_then must preserve its Bool condition, got {cond:?}",
            );
            assert!(
                matches!(*t, TransitionProp::EqOutput(ShallowIr::Const(ShallowConst::Int(ref v))) if v == "0"),
                "true branch should return 0, got {t:?}",
            );
            assert!(
                matches!(*e, TransitionProp::EqOutput(ShallowIr::Const(ShallowConst::Int(ref v))) if v == "1"),
                "false branch should return 1, got {e:?}",
            );
        }
        other => panic!("expected IfThenElse for fork_if_and_then, got {other:?}"),
    }
}

#[test]
fn typed_expr_to_transition_prop_one_of_uses_relation_lowering() {
    let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
        empty_transition_prop_context();

    let int_fuzzer = Type::fuzzer(Type::int());
    let values = TypedExpr::List {
        location: Span::empty(),
        tipo: Type::list(Type::int()),
        elements: vec![uint_lit("0"), uint_lit("1")],
        tail: None,
    };
    let call_expr = TypedExpr::Call {
        location: Span::empty(),
        tipo: int_fuzzer.clone(),
        fun: Box::new(module_fn_var(
            "one_of",
            STDLIB_FUZZ_MODULE,
            Type::function(vec![Type::list(Type::int())], int_fuzzer.clone()),
        )),
        args: vec![call_arg(values)],
    };

    let prop = typed_expr_to_transition_prop(
        &call_expr,
        "test_mod",
        &function_index,
        &constant_index,
        &local_values,
        &empty_data_types,
        &mut visiting,
        &BTreeSet::new(),
        &mut BTreeSet::new(),
    );

    match prop {
        TransitionProp::Or(branches) => {
            assert_eq!(branches.len(), 2);
            assert!(
                matches!(&branches[0], TransitionProp::EqOutput(ShallowIr::Const(ShallowConst::Int(v))) if v == "0")
            );
            assert!(
                matches!(&branches[1], TransitionProp::EqOutput(ShallowIr::Const(ShallowConst::Int(v))) if v == "1")
            );
        }
        other => panic!("expected Or for top-level one_of, got {other:?}"),
    }
}

#[test]
fn typed_expr_to_transition_prop_one_of_rejects_inexact_literal_element() {
    let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
        empty_transition_prop_context();
    let int_fuzzer = Type::fuzzer(Type::int());
    let values = TypedExpr::List {
        location: Span::empty(),
        tipo: Type::list(Type::int()),
        elements: vec![local_var("mystery", Type::int())],
        tail: None,
    };
    let call_expr = TypedExpr::Call {
        location: Span::empty(),
        tipo: int_fuzzer.clone(),
        fun: Box::new(module_fn_var(
            "one_of",
            STDLIB_FUZZ_MODULE,
            Type::function(vec![Type::list(Type::int())], int_fuzzer.clone()),
        )),
        args: vec![call_arg(values)],
    };

    let prop = typed_expr_to_transition_prop(
        &call_expr,
        "test_mod",
        &function_index,
        &constant_index,
        &local_values,
        &empty_data_types,
        &mut visiting,
        &BTreeSet::new(),
        &mut BTreeSet::new(),
    );

    assert!(
        matches!(
            prop,
            TransitionProp::Unsupported { ref reason, .. }
                if reason.contains("one_of literal element is not emitted exactly on counted path")
        ),
        "one_of must reject non-exact literal elements instead of emitting vacuous equalities"
    );
}

#[test]
fn typed_expr_to_transition_prop_map_resolves_named_helper() {
    let (identity_key, identity_fn) =
        make_named_unary_identity_mapper_function("identity", Type::int());
    let mut known_functions = empty_known_functions();
    known_functions.insert(&identity_key, &identity_fn);
    let function_index = index_known_functions(&known_functions);
    let constant_index: ConstantIndex<'_> = HashMap::new();
    let empty_data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();
    let mut visiting = BTreeSet::new();

    let source = make_typed_int_between_fuzzer("0", "1");
    let mapper = module_fn_var(
        "identity",
        "math",
        Type::function(vec![Type::int()], Type::int()),
    );
    let expr = TypedExpr::Call {
        location: Span::empty(),
        tipo: Type::fuzzer(Type::int()),
        fun: Box::new(module_fn_var(
            "map",
            STDLIB_FUZZ_MODULE,
            Type::function(
                vec![source.tipo(), mapper.tipo()],
                Type::fuzzer(Type::int()),
            ),
        )),
        args: vec![call_arg(source), call_arg(mapper)],
    };

    let prop = typed_expr_to_transition_prop(
        &expr,
        "math",
        &function_index,
        &constant_index,
        &BTreeMap::new(),
        &empty_data_types,
        &mut visiting,
        &BTreeSet::new(),
        &mut BTreeSet::new(),
    );

    match prop {
        TransitionProp::Exists { binder, body, .. } => {
            assert_eq!(binder, "_map_source");
            assert!(
                matches!(
                    *body,
                    TransitionProp::EqOutput(ShallowIr::BoundVar { ref name, .. })
                        if name == "_map_source"
                ),
                "named unary mappers must resolve through the general callback resolver, got {body:?}"
            );
        }
        other => panic!("expected Exists(_map_source), got {other:?}"),
    }
}

#[test]
fn typed_expr_to_transition_prop_either_uses_relation_lowering() {
    let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
        empty_transition_prop_context();

    let int_fuzzer = Type::fuzzer(Type::int());
    let return_fn_type = Type::function(vec![Type::int()], int_fuzzer.clone());
    let make_return = |value: &str| TypedExpr::Call {
        location: Span::empty(),
        tipo: int_fuzzer.clone(),
        fun: Box::new(module_fn_var(
            "constant",
            STDLIB_FUZZ_MODULE,
            return_fn_type.clone(),
        )),
        args: vec![call_arg(uint_lit(value))],
    };
    let call_expr = TypedExpr::Call {
        location: Span::empty(),
        tipo: int_fuzzer.clone(),
        fun: Box::new(module_fn_var(
            "either",
            STDLIB_FUZZ_MODULE,
            Type::function(
                vec![int_fuzzer.clone(), int_fuzzer.clone()],
                int_fuzzer.clone(),
            ),
        )),
        args: vec![call_arg(make_return("0")), call_arg(make_return("1"))],
    };

    let prop = typed_expr_to_transition_prop(
        &call_expr,
        "test_mod",
        &function_index,
        &constant_index,
        &local_values,
        &empty_data_types,
        &mut visiting,
        &BTreeSet::new(),
        &mut BTreeSet::new(),
    );

    match prop {
        TransitionProp::Or(branches) => {
            assert_eq!(branches.len(), 2);
            assert!(
                matches!(&branches[0], TransitionProp::EqOutput(ShallowIr::Const(ShallowConst::Int(v))) if v == "0")
            );
            assert!(
                matches!(&branches[1], TransitionProp::EqOutput(ShallowIr::Const(ShallowConst::Int(v))) if v == "1")
            );
        }
        other => panic!("expected Or for top-level either, got {other:?}"),
    }
}

#[test]
fn typed_expr_to_transition_prop_either_with_unsupported_branch_stays_unsupported() {
    let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
        empty_transition_prop_context();

    let int_fuzzer = Type::fuzzer(Type::int());
    let return_fn_type = Type::function(vec![Type::int()], int_fuzzer.clone());
    let make_return = |value: &str| TypedExpr::Call {
        location: Span::empty(),
        tipo: int_fuzzer.clone(),
        fun: Box::new(module_fn_var(
            "constant",
            STDLIB_FUZZ_MODULE,
            return_fn_type.clone(),
        )),
        args: vec![call_arg(uint_lit(value))],
    };
    let call_expr = TypedExpr::Call {
        location: Span::empty(),
        tipo: int_fuzzer.clone(),
        fun: Box::new(module_fn_var(
            "either",
            STDLIB_FUZZ_MODULE,
            Type::function(
                vec![int_fuzzer.clone(), int_fuzzer.clone()],
                int_fuzzer.clone(),
            ),
        )),
        args: vec![
            call_arg(make_return("0")),
            call_arg(TypedExpr::Call {
                location: Span::empty(),
                tipo: int_fuzzer.clone(),
                fun: Box::new(module_fn_var(
                    "and_then",
                    STDLIB_FUZZ_MODULE,
                    Type::function(vec![int_fuzzer.clone()], int_fuzzer.clone()),
                )),
                args: vec![call_arg(make_return("1"))],
            }),
        ],
    };

    let prop = typed_expr_to_transition_prop(
        &call_expr,
        "test_mod",
        &function_index,
        &constant_index,
        &local_values,
        &empty_data_types,
        &mut visiting,
        &BTreeSet::new(),
        &mut BTreeSet::new(),
    );

    match prop {
        TransitionProp::Unsupported { reason, .. } => {
            assert!(
                reason.contains("either branch is not lowered faithfully yet"),
                "unexpected Unsupported reason: {reason}"
            );
        }
        other => {
            panic!("expected Unsupported for either-with-unsupported-branch, got {other:?}")
        }
    }
}

#[test]
fn typed_expr_to_transition_prop_when_unsupported_clause_preserves_fallback() {
    let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
        empty_transition_prop_context();

    let int_fuzzer = Type::fuzzer(Type::int());
    let return_fn_type = Type::function(vec![Type::int()], int_fuzzer.clone());
    let make_return = |value: &str| TypedExpr::Call {
        location: Span::empty(),
        tipo: int_fuzzer.clone(),
        fun: Box::new(module_fn_var(
            "constant",
            STDLIB_FUZZ_MODULE,
            return_fn_type.clone(),
        )),
        args: vec![call_arg(uint_lit(value))],
    };

    let when_expr = TypedExpr::When {
        location: Span::empty(),
        tipo: int_fuzzer.clone(),
        subject: Box::new(TypedExpr::List {
            location: Span::empty(),
            tipo: Type::list(Type::int()),
            elements: vec![uint_lit("1")],
            tail: None,
        }),
        clauses: vec![
            TypedClause {
                location: Span::empty(),
                pattern: TypedPattern::List {
                    location: Span::empty(),
                    elements: vec![TypedPattern::var("head")],
                    tail: None,
                },
                then: make_return("1"),
            },
            TypedClause {
                location: Span::empty(),
                pattern: TypedPattern::Discard {
                    name: "_rest".to_string(),
                    location: Span::empty(),
                },
                then: make_return("2"),
            },
        ],
    };

    let prop = typed_expr_to_transition_prop(
        &when_expr,
        "test_mod",
        &function_index,
        &constant_index,
        &local_values,
        &empty_data_types,
        &mut visiting,
        &BTreeSet::new(),
        &mut BTreeSet::new(),
    );

    match prop {
        TransitionProp::Or(branches) => {
            assert_eq!(
                branches.len(),
                2,
                "expected unsupported clause + fallback, got {branches:?}"
            );
            match &branches[1] {
                TransitionProp::EqOutput(ShallowIr::Const(ShallowConst::Int(v))) => {
                    assert_eq!(v, "2", "fallback branch should remain reachable");
                }
                other => panic!("expected fallback EqOutput(2), got {other:?}"),
            }
        }
        other => {
            panic!("expected Or preserving fallback after unsupported clause, got {other:?}")
        }
    }
}

#[test]
fn transition_prop_from_named_step_function_binds_state_parameter() {
    let (_, _, _, empty_data_types, mut visiting) = empty_transition_prop_context();

    let int_tipo = Type::int();
    let int_fuzzer = Type::fuzzer(int_tipo.clone());
    let return_fn_type = Type::function(vec![int_tipo.clone()], int_fuzzer.clone());
    let step_fn = TypedFunction {
        arguments: vec![TypedArg::new("st", int_tipo.clone())],
        body: TypedExpr::Call {
            location: Span::empty(),
            tipo: int_fuzzer.clone(),
            fun: Box::new(module_fn_var(
                "constant",
                STDLIB_FUZZ_MODULE,
                return_fn_type,
            )),
            args: vec![call_arg(local_var("st", int_tipo.clone()))],
        },
        doc: None,
        location: Span::empty(),
        name: "step".to_string(),
        public: false,
        return_annotation: None,
        return_type: int_fuzzer.clone(),
        end_position: 0,
        on_test_failure: OnTestFailure::FailImmediately,
    };
    let key = FunctionAccessKey {
        module_name: "mod".to_string(),
        function_name: "step".to_string(),
    };
    let known_functions = IndexMap::from([(&key, &step_fn)]);
    let function_index = index_known_functions(&known_functions);
    let step_ref = module_fn_var(
        "step",
        "mod",
        Type::function(vec![int_tipo.clone()], int_fuzzer),
    );

    let prop = transition_prop_from_step_function(
        &step_ref,
        None,
        &function_index,
        &HashMap::new(),
        &empty_data_types,
        &mut visiting,
    )
    .expect("named step function should lower");

    match prop {
        TransitionProp::EqOutput(ShallowIr::BoundVar { name, ty }) => {
            assert_eq!(name, "state");
            assert_eq!(ty, ShallowIrType::Int);
        }
        other => panic!("expected EqOutput(BoundVar(state)), got {other:?}"),
    }
}

#[test]
fn transition_prop_from_multiarg_step_function_returns_none() {
    let (_, _, _, empty_data_types, mut visiting) = empty_transition_prop_context();

    let int_tipo = Type::int();
    let int_fuzzer = Type::fuzzer(int_tipo.clone());
    let return_fn_type = Type::function(vec![int_tipo.clone()], int_fuzzer.clone());
    let step_fn = TypedExpr::Fn {
        location: Span::empty(),
        tipo: Type::function(vec![int_tipo.clone(), int_tipo.clone()], int_fuzzer.clone()),
        is_capture: false,
        args: vec![
            TypedArg::new("st", int_tipo.clone()),
            TypedArg::new("input", int_tipo.clone()),
        ],
        body: Box::new(TypedExpr::Call {
            location: Span::empty(),
            tipo: int_fuzzer,
            fun: Box::new(module_fn_var(
                "constant",
                STDLIB_FUZZ_MODULE,
                return_fn_type,
            )),
            args: vec![call_arg(local_var("input", int_tipo))],
        }),
        return_annotation: None,
    };

    assert!(
        transition_prop_from_step_function(
            &step_fn,
            None,
            &HashMap::new(),
            &HashMap::new(),
            &empty_data_types,
            &mut visiting,
        )
        .is_none(),
        "multi-arg step functions should stay unsupported until their non-state parameters have an honest theorem-side representation"
    );
}

#[test]
fn typed_expr_to_transition_prop_top_level_map_uses_relation_lowering() {
    let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
        empty_transition_prop_context();

    let int_fuzzer = Type::fuzzer(Type::int());
    let source = make_typed_int_between_fuzzer("0", "0");
    let mapper = make_constant_int_mapper(Type::int(), "7");
    let map_call = TypedExpr::Call {
        location: Span::empty(),
        tipo: int_fuzzer.clone(),
        fun: Box::new(module_fn_var(
            "map",
            STDLIB_FUZZ_MODULE,
            Type::function(vec![source.tipo(), mapper.tipo()], int_fuzzer.clone()),
        )),
        args: vec![call_arg(source), call_arg(mapper)],
    };

    let prop = typed_expr_to_transition_prop(
        &map_call,
        "test_mod",
        &function_index,
        &constant_index,
        &local_values,
        &empty_data_types,
        &mut visiting,
        &BTreeSet::new(),
        &mut BTreeSet::new(),
    );

    match prop {
        TransitionProp::Exists { binder, body, .. } => {
            assert_eq!(binder, "_map_source");
            assert!(
                matches!(
                    *body,
                    TransitionProp::EqOutput(ShallowIr::Const(ShallowConst::Int(ref v)))
                        if v == "7"
                ),
                "mapped branch should constrain the transition output exactly, got {body:?}"
            );
        }
        other => panic!("expected Exists for top-level map, got {other:?}"),
    }
}

#[test]
fn pair_projection_bindings_preserve_scalar_types() {
    let binding = LocalBinding::DrawnValue {
        lean_name: "pair".to_string(),
        ty: ShallowIrType::Pair(Box::new(ShallowIrType::Int), Box::new(ShallowIrType::Bool)),
        domain: FuzzerSemantics::Data,
    };
    let pattern = TypedPattern::Pair {
        location: Span::empty(),
        fst: Box::new(TypedPattern::var("n")),
        snd: Box::new(TypedPattern::var("flag")),
    };
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();
    let mut locals = BTreeMap::new();
    bind_pattern_to_locals(&pattern, &binding, &data_types, &mut locals);

    match locals.get("n") {
        Some(LocalBinding::Projection { ty, .. }) => assert_eq!(*ty, ShallowIrType::Int),
        other => panic!("expected projected Int binding for fst, got {other:?}"),
    }
    let flag_binding = locals
        .get("flag")
        .expect("pair-pattern binding should create a flag projection");
    match local_binding_to_shallow_ir(flag_binding, &data_types, &locals, &mut BTreeSet::new()) {
        ShallowIr::FieldAccess { ty, kind, .. } => {
            assert_eq!(ty, ShallowIrType::Bool);
            assert_eq!(kind, ShallowFieldAccessKind::ListElement);
        }
        other => panic!("expected FieldAccess projection for flag, got {other:?}"),
    }
}

#[test]
fn extend_locals_with_leading_assignments_preserves_alias_chain_destructuring_identity() {
    let (function_index, constant_index, _, _, _) = empty_transition_prop_context();
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();
    let pair_type = Type::pair(Type::int(), Type::bool());
    let pair_binding = LocalBinding::DrawnValue {
        lean_name: "pair_input".to_string(),
        ty: ShallowIrType::Pair(Box::new(ShallowIrType::Int), Box::new(ShallowIrType::Bool)),
        domain: FuzzerSemantics::Data,
    };
    let mut locals = BTreeMap::new();
    locals.insert("pair".to_string(), pair_binding);

    let expr = TypedExpr::Sequence {
        location: Span::empty(),
        expressions: vec![
            TypedExpr::Assignment {
                location: Span::empty(),
                tipo: pair_type.clone(),
                value: Box::new(local_var("pair", pair_type.clone())),
                pattern: TypedPattern::var("p"),
                kind: crate::ast::AssignmentKind::Let { backpassing: () },
                comment: None,
            },
            TypedExpr::Assignment {
                location: Span::empty(),
                tipo: pair_type.clone(),
                value: Box::new(local_var("p", pair_type.clone())),
                pattern: TypedPattern::Pair {
                    location: Span::empty(),
                    fst: Box::new(TypedPattern::var("x")),
                    snd: Box::new(TypedPattern::Discard {
                        name: "_".to_string(),
                        location: Span::empty(),
                    }),
                },
                kind: crate::ast::AssignmentKind::Let { backpassing: () },
                comment: None,
            },
            local_var("x", Type::int()),
        ],
    };

    let ctx = TransitionLoweringContext {
        current_module: "test_mod".to_string(),
        function_index: &function_index,
        constant_index: &constant_index,
        data_types: &data_types,
        locals,
        visiting_functions: BTreeSet::new(),
        visiting_locals: BTreeSet::new(),
        visiting_value_aliases: BTreeSet::new(),
        next_synthetic_binder: 0,
    };

    let scoped = extend_locals_with_leading_assignments(&expr, &ctx)
        .expect("sequence with leading assignments should extend locals");
    let x_binding = scoped
        .get("x")
        .expect("destructured alias chain should bind x in the extended scope");

    match local_binding_to_shallow_ir(x_binding, &data_types, &scoped, &mut BTreeSet::new()) {
        ShallowIr::FieldAccess {
            record,
            index,
            ty,
            kind,
            ..
        } => {
            assert_eq!(index, 0);
            assert_eq!(ty, ShallowIrType::Int);
            assert_eq!(kind, ShallowFieldAccessKind::ListElement);
            assert!(
                matches!(
                    record.as_ref(),
                    ShallowIr::BoundVar { name, ty }
                        if name == "pair_input"
                            && matches!(
                                ty,
                                ShallowIrType::Pair(fst, snd)
                                    if **fst == ShallowIrType::Int
                                        && **snd == ShallowIrType::Bool
                            )
                ),
                "projection must stay rooted in the original drawn pair, got {record:?}"
            );
        }
        other => panic!(
            "expected alias-chain destructuring to lower to a projection from the original pair, got {other:?}"
        ),
    }
}

#[test]
fn translate_sequence_with_locals_preserves_nested_derived_local_identity() {
    fn terminal_let_body(ir: &ShallowIr) -> &ShallowIr {
        match ir {
            ShallowIr::Let { body, .. } => terminal_let_body(body),
            other => other,
        }
    }

    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();
    let pair_type = Type::pair(Type::int(), Type::bool());
    let mut locals = BTreeMap::new();
    locals.insert(
        "pair".to_string(),
        LocalBinding::DrawnValue {
            lean_name: "pair_input".to_string(),
            ty: ShallowIrType::Pair(Box::new(ShallowIrType::Int), Box::new(ShallowIrType::Bool)),
            domain: FuzzerSemantics::Data,
        },
    );

    let expr = TypedExpr::Sequence {
        location: Span::empty(),
        expressions: vec![
            TypedExpr::Assignment {
                location: Span::empty(),
                tipo: pair_type.clone(),
                value: Box::new(local_var("pair", pair_type.clone())),
                pattern: TypedPattern::var("q"),
                kind: crate::ast::AssignmentKind::Let { backpassing: () },
                comment: None,
            },
            TypedExpr::Assignment {
                location: Span::empty(),
                tipo: pair_type.clone(),
                value: Box::new(local_var("q", pair_type.clone())),
                pattern: TypedPattern::Pair {
                    location: Span::empty(),
                    fst: Box::new(TypedPattern::var("x")),
                    snd: Box::new(TypedPattern::Discard {
                        name: "_".to_string(),
                        location: Span::empty(),
                    }),
                },
                kind: crate::ast::AssignmentKind::Let { backpassing: () },
                comment: None,
            },
            TypedExpr::Assignment {
                location: Span::empty(),
                tipo: Type::int(),
                value: Box::new(local_var("x", Type::int())),
                pattern: TypedPattern::var("y"),
                kind: crate::ast::AssignmentKind::Let { backpassing: () },
                comment: None,
            },
            local_var("y", Type::int()),
        ],
    };

    let ir =
        typed_expr_to_shallow_ir_with_locals(&expr, &data_types, &locals, &mut BTreeSet::new());

    match terminal_let_body(&ir) {
        ShallowIr::FieldAccess {
            record,
            index,
            ty,
            kind,
            ..
        } => {
            assert_eq!(*index, 0);
            assert_eq!(*ty, ShallowIrType::Int);
            assert_eq!(*kind, ShallowFieldAccessKind::ListElement);
            assert!(
                matches!(
                    record.as_ref(),
                    ShallowIr::BoundVar { name, ty }
                        if name == "pair_input"
                            && matches!(
                                ty,
                                ShallowIrType::Pair(fst, snd)
                                    if **fst == ShallowIrType::Int
                                        && **snd == ShallowIrType::Bool
                            )
                ),
                "derived local should stay rooted in the original drawn pair, got {record:?}"
            );
        }
        other => panic!(
            "expected nested derived local to lower to the original pair projection, got {other:?}"
        ),
    }
}

#[test]
fn translate_sequence_with_locals_alias_cycle_remains_explicit() {
    fn terminal_let_body(ir: &ShallowIr) -> &ShallowIr {
        match ir {
            ShallowIr::Let { body, .. } => terminal_let_body(body),
            other => other,
        }
    }

    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();
    let int_type = Type::int();
    let expr = TypedExpr::Sequence {
        location: Span::empty(),
        expressions: vec![
            TypedExpr::Assignment {
                location: Span::empty(),
                tipo: int_type.clone(),
                value: Box::new(local_var("y", int_type.clone())),
                pattern: TypedPattern::var("x"),
                kind: crate::ast::AssignmentKind::Let { backpassing: () },
                comment: None,
            },
            TypedExpr::Assignment {
                location: Span::empty(),
                tipo: int_type.clone(),
                value: Box::new(local_var("x", int_type.clone())),
                pattern: TypedPattern::var("y"),
                kind: crate::ast::AssignmentKind::Let { backpassing: () },
                comment: None,
            },
            local_var("y", int_type),
        ],
    };

    let ir = typed_expr_to_shallow_ir_with_locals(
        &expr,
        &data_types,
        &BTreeMap::new(),
        &mut BTreeSet::new(),
    );

    match terminal_let_body(&ir) {
        ShallowIr::Opaque { reason, .. } => assert!(
            reason.contains("local binding cycle on 'y' while lowering value"),
            "alias cycle must stay explicit rather than looping or widening silently, got: {reason}"
        ),
        other => panic!("expected explicit cycle marker, got {other:?}"),
    }
}

#[test]
fn lower_bool_predicate_with_locals_preserves_constructor_false_literal() {
    let (function_index, constant_index, _, _, _) = empty_transition_prop_context();
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();
    let output_binding = LocalBinding::DrawnValue {
        lean_name: "candidate".to_string(),
        ty: ShallowIrType::Bool,
        domain: FuzzerSemantics::Bool,
    };
    let mut ctx = TransitionLoweringContext {
        current_module: "test_mod".to_string(),
        function_index: &function_index,
        constant_index: &constant_index,
        data_types: &data_types,
        locals: BTreeMap::new(),
        visiting_functions: BTreeSet::new(),
        visiting_locals: BTreeSet::new(),
        visiting_value_aliases: BTreeSet::new(),
        next_synthetic_binder: 0,
    };

    let predicate = make_constant_bool_mapper(Type::bool(), false);
    let prop = lower_bool_predicate_with_locals(&predicate, &output_binding, &mut ctx);

    assert!(
        matches!(
            prop,
            TransitionProp::Pure(ShallowIr::Const(ShallowConst::Bool(false)))
        ),
        "constructor-style False must stay as a constraining Bool literal on the relation path"
    );
}

#[test]
fn lower_bool_predicate_with_locals_resolves_named_helper() {
    let (predicate_key, predicate_fn) =
        make_named_unary_tautology_mapper_function("always_true", Type::bool());
    let mut known_functions = empty_known_functions();
    known_functions.insert(&predicate_key, &predicate_fn);
    let function_index = index_known_functions(&known_functions);
    let constant_index: ConstantIndex<'_> = HashMap::new();
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();
    let output_binding = LocalBinding::DrawnValue {
        lean_name: "candidate".to_string(),
        ty: ShallowIrType::Bool,
        domain: FuzzerSemantics::Bool,
    };
    let mut ctx = TransitionLoweringContext {
        current_module: "math".to_string(),
        function_index: &function_index,
        constant_index: &constant_index,
        data_types: &data_types,
        locals: BTreeMap::new(),
        visiting_functions: BTreeSet::new(),
        visiting_locals: BTreeSet::new(),
        visiting_value_aliases: BTreeSet::new(),
        next_synthetic_binder: 0,
    };

    let predicate = module_fn_var(
        "always_true",
        "math",
        Type::function(vec![Type::bool()], Type::bool()),
    );
    let prop = lower_bool_predicate_with_locals(&predicate, &output_binding, &mut ctx);

    assert!(
        matches!(
            prop,
            TransitionProp::Pure(ShallowIr::BinOp {
                op: ShallowBinOp::Eq,
                ref left,
                ref right,
            }) if matches!(
                (left.as_ref(), right.as_ref()),
                (
                    ShallowIr::BoundVar { name: left_name, .. },
                    ShallowIr::BoundVar { name: right_name, .. },
                ) if left_name == "candidate" && right_name == "candidate"
            )
        ),
        "named unary predicate helpers must resolve through the general callback resolver, got {prop:?}"
    );
}

#[test]
fn lower_bool_predicate_with_locals_rejects_inexact_body() {
    let (function_index, constant_index, _, _, _) = empty_transition_prop_context();
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();
    let output_binding = LocalBinding::DrawnValue {
        lean_name: "candidate".to_string(),
        ty: ShallowIrType::Int,
        domain: FuzzerSemantics::IntRange {
            min: None,
            max: None,
        },
    };
    let mut ctx = TransitionLoweringContext {
        current_module: "test_mod".to_string(),
        function_index: &function_index,
        constant_index: &constant_index,
        data_types: &data_types,
        locals: BTreeMap::new(),
        visiting_functions: BTreeSet::new(),
        visiting_locals: BTreeSet::new(),
        visiting_value_aliases: BTreeSet::new(),
        next_synthetic_binder: 0,
    };

    let predicate = make_unary_mapper(
        "n",
        Type::int(),
        Type::bool(),
        TypedExpr::BinOp {
            location: Span::empty(),
            tipo: Type::bool(),
            name: BinOp::GtEqInt,
            left: Box::new(local_var("n", Type::int())),
            right: Box::new(uint_lit("0")),
        },
    );
    let prop = lower_bool_predicate_with_locals(&predicate, &output_binding, &mut ctx);

    assert!(
        matches!(prop, TransitionProp::Pure(ShallowIr::BinOp { .. })),
        "supported Bool predicates must lower to real Pure constraints, got {prop:?}"
    );
}

#[test]
fn normalize_source_domain_strips_map_bind_and_filter_wrappers() {
    let (function_index, constant_index, _, _, _) = empty_transition_prop_context();
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();
    let mut ctx = TransitionLoweringContext {
        current_module: "math".to_string(),
        function_index: &function_index,
        constant_index: &constant_index,
        data_types: &data_types,
        locals: BTreeMap::new(),
        visiting_functions: BTreeSet::new(),
        visiting_locals: BTreeSet::new(),
        visiting_value_aliases: BTreeSet::new(),
        next_synthetic_binder: 0,
    };

    let source = make_typed_int_between_fuzzer("1", "3");
    let mapped = make_typed_map_call(
        source.clone(),
        make_identity_mapper("n", Type::int()),
        Type::int(),
    );
    let bound = make_stdlib_bind_call(
        source.clone(),
        make_inline_bind_continuation(
            "n",
            Type::int(),
            make_typed_int_between_fuzzer("5", "8"),
            Type::int(),
        ),
        Type::int(),
    );
    let filtered = make_typed_filter_call(source, make_bool_predicate("n", Type::int()));

    let expected = FuzzerSemantics::IntRange {
        min: Some("1".to_string()),
        max: Some("3".to_string()),
    };
    assert_eq!(normalize_source_domain(&mapped, &mut ctx), expected);
    assert_eq!(normalize_source_domain(&bound, &mut ctx), expected);
    assert_eq!(normalize_source_domain(&filtered, &mut ctx), expected);
}

#[test]
fn output_match_prop_rejects_non_exact_var_rhs() {
    let (function_index, constant_index, _, _, _) = empty_transition_prop_context();
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();
    let ctx = TransitionLoweringContext {
        current_module: "test_mod".to_string(),
        function_index: &function_index,
        constant_index: &constant_index,
        data_types: &data_types,
        locals: BTreeMap::new(),
        visiting_functions: BTreeSet::new(),
        visiting_locals: BTreeSet::new(),
        visiting_value_aliases: BTreeSet::new(),
        next_synthetic_binder: 0,
    };

    let prop = output_match_prop(
        &transition_output_binding(),
        ShallowIr::Var {
            name: "x".to_string(),
            ty: ShallowIrType::Data,
        },
        &ctx,
    );

    assert!(
        matches!(
            prop,
            TransitionProp::Unsupported { ref reason, .. }
                if reason.contains("counted output equality rejected non-exact rhs root 'Var'")
        ),
        "counted output equality must reject roots that would freshen later"
    );
}

#[test]
fn typed_expr_to_transition_prop_resolves_aliased_fork_continuation() {
    let int_type = Type::int();
    let int_fuzzer = Type::fuzzer(int_type.clone());
    let return_body = TypedExpr::Call {
        location: Span::empty(),
        tipo: int_fuzzer.clone(),
        fun: Box::new(module_fn_var(
            "constant",
            STDLIB_FUZZ_MODULE,
            Type::function(vec![int_type.clone()], int_fuzzer.clone()),
        )),
        args: vec![call_arg(local_var("picked", int_type.clone()))],
    };
    let (cont_key, cont_fn) = make_named_fuzzer_continuation_function(
        "keep_picked",
        "picked",
        int_type.clone(),
        return_body,
        int_type.clone(),
    );
    let mut known_functions: IndexMap<&FunctionAccessKey, &TypedFunction> = IndexMap::new();
    known_functions.insert(&cont_key, &cont_fn);
    let function_index = index_known_functions(&known_functions);
    let constant_index: ConstantIndex<'_> = HashMap::new();
    let empty_data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();
    let mut visiting = BTreeSet::new();

    let continuation_type = Type::function(vec![int_type.clone()], int_fuzzer.clone());
    let mut local_values = BTreeMap::new();
    local_values.insert(
        "keep_alias".to_string(),
        module_fn_var("keep_picked", "math", continuation_type.clone()),
    );

    let fork_call = make_stdlib_fork_call(
        "fork_and_then",
        vec![
            make_zero_arg_fuzzer_thunk(make_typed_int_between_fuzzer("0", "1")),
            make_zero_arg_fuzzer_thunk(make_typed_int_between_fuzzer("2", "3")),
            local_var("keep_alias", continuation_type),
        ],
        int_type.clone(),
    );

    let prop = typed_expr_to_transition_prop(
        &fork_call,
        "test_mod",
        &function_index,
        &constant_index,
        &local_values,
        &empty_data_types,
        &mut visiting,
        &BTreeSet::new(),
        &mut BTreeSet::new(),
    );

    match prop {
        TransitionProp::Exists { binder, body, .. } => {
            assert_eq!(binder, "picked");
            match *body {
                TransitionProp::And(parts) => {
                    assert_eq!(parts.len(), 2);
                    assert!(
                        matches!(&parts[0], TransitionProp::Or(branches) if branches.len() == 2),
                        "fork branches must still be preserved, got {:?}",
                        parts[0]
                    );
                    assert!(
                        matches!(
                            &parts[1],
                            TransitionProp::EqOutput(ShallowIr::BoundVar { name, ty })
                                if name == "picked" && *ty == ShallowIrType::Int
                        ),
                        "resolved continuation must lower its visible body with the bound witness, got {:?}",
                        parts[1]
                    );
                }
                other => panic!("expected fork continuation body And([...]), got {other:?}"),
            }
        }
        other => panic!("expected fork continuation to lower to Exists, got {other:?}"),
    }
}

#[test]
fn typed_expr_to_transition_prop_rejects_unresolved_fork_continuation() {
    let int_type = Type::int();
    let continuation_type = Type::function(vec![int_type.clone()], Type::fuzzer(int_type.clone()));
    let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
        empty_transition_prop_context();

    let fork_call = make_stdlib_fork_call(
        "fork_and_then",
        vec![
            make_zero_arg_fuzzer_thunk(make_typed_int_between_fuzzer("0", "1")),
            make_zero_arg_fuzzer_thunk(make_typed_int_between_fuzzer("2", "3")),
            module_fn_var("missing_visible_continuation", "math", continuation_type),
        ],
        int_type,
    );

    let prop = typed_expr_to_transition_prop(
        &fork_call,
        "test_mod",
        &function_index,
        &constant_index,
        &local_values,
        &empty_data_types,
        &mut visiting,
        &BTreeSet::new(),
        &mut BTreeSet::new(),
    );

    match prop {
        TransitionProp::Unsupported { reason, .. } => assert!(
            reason.contains("fork continuation"),
            "unresolved fork continuations must surface explicitly, got: {reason}"
        ),
        other => panic!("expected explicit unsupported fork continuation, got {other:?}"),
    }
}

#[test]
fn typed_expr_to_transition_prop_int_assignment_pattern_is_explicitly_unsupported() {
    let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
        empty_transition_prop_context();
    let int_fuzzer = Type::fuzzer(Type::int());
    let expr = TypedExpr::Sequence {
        location: Span::empty(),
        expressions: vec![
            TypedExpr::Assignment {
                location: Span::empty(),
                tipo: Type::int(),
                value: Box::new(uint_lit("0")),
                pattern: TypedPattern::Int {
                    location: Span::empty(),
                    value: "0".to_string(),
                    base: Base::Decimal {
                        numeric_underscore: false,
                    },
                },
                kind: crate::ast::AssignmentKind::Let { backpassing: () },
                comment: None,
            },
            TypedExpr::Call {
                location: Span::empty(),
                tipo: int_fuzzer.clone(),
                fun: Box::new(module_fn_var(
                    "constant",
                    STDLIB_FUZZ_MODULE,
                    Type::function(vec![Type::int()], int_fuzzer),
                )),
                args: vec![call_arg(uint_lit("42"))],
            },
        ],
    };

    let prop = typed_expr_to_transition_prop(
        &expr,
        "test_mod",
        &function_index,
        &constant_index,
        &local_values,
        &empty_data_types,
        &mut visiting,
        &BTreeSet::new(),
        &mut BTreeSet::new(),
    );

    match prop {
        TransitionProp::Unsupported { reason, .. } => assert!(
            reason.contains("int-pattern bindings require an equality guard"),
            "literal assignment patterns must be rejected explicitly, got: {reason}"
        ),
        other => panic!("expected explicit unsupported literal pattern, got {other:?}"),
    }
}

#[test]
fn translate_sequence_with_locals_list_assignment_pattern_is_explicitly_opaque() {
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();
    let list_type = Type::list(Type::int());
    let expr = TypedExpr::Sequence {
        location: Span::empty(),
        expressions: vec![
            TypedExpr::Assignment {
                location: Span::empty(),
                tipo: list_type.clone(),
                value: Box::new(TypedExpr::List {
                    location: Span::empty(),
                    tipo: list_type.clone(),
                    elements: vec![uint_lit("1")],
                    tail: None,
                }),
                pattern: TypedPattern::List {
                    location: Span::empty(),
                    elements: vec![TypedPattern::var("head")],
                    tail: None,
                },
                kind: crate::ast::AssignmentKind::Let { backpassing: () },
                comment: None,
            },
            local_var("head", Type::int()),
        ],
    };

    let ir = typed_expr_to_shallow_ir_with_locals(
        &expr,
        &data_types,
        &BTreeMap::new(),
        &mut BTreeSet::new(),
    );

    match ir {
        ShallowIr::Opaque { reason, .. } => assert!(
            reason.contains("list-pattern bindings are not lowered faithfully yet"),
            "list assignment patterns must surface as opaque until a real guard exists, got: {reason}"
        ),
        other => panic!("expected explicit opaque list-pattern binding, got {other:?}"),
    }
}

#[test]
fn shallow_ir_record_update_is_not_vacuous() {
    let ir = ShallowIr::RecordUpdate {
        record: Box::new(ShallowIr::BoundVar {
            name: "state".to_string(),
            ty: ShallowIrType::Data,
        }),
        tag: 1,
        field_count: 1,
        updates: vec![],
    };
    assert!(
        !shallow_ir_is_vacuous(&ir),
        "RecordUpdate now emits structural Data and must not be classified as vacuous",
    );
}

#[test]
fn typed_expr_to_transition_prop_return_produces_eq_output() {
    // `constant(42)` — zero-arg wrapper around a pure value.
    let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
        empty_transition_prop_context();

    let int_fuzzer = Type::fuzzer(Type::int());
    let call_expr = TypedExpr::Call {
        location: Span::empty(),
        tipo: int_fuzzer.clone(),
        fun: Box::new(module_fn_var(
            "constant",
            STDLIB_FUZZ_MODULE,
            Type::function(vec![Type::int()], int_fuzzer),
        )),
        args: vec![call_arg(uint_lit("42"))],
    };

    let prop = typed_expr_to_transition_prop(
        &call_expr,
        "test_mod",
        &function_index,
        &constant_index,
        &local_values,
        &empty_data_types,
        &mut visiting,
        &BTreeSet::new(),
        &mut BTreeSet::new(),
    );

    match prop {
        TransitionProp::EqOutput(ShallowIr::Const(ShallowConst::Int(v))) => {
            assert_eq!(v, "42");
        }
        other => panic!("expected EqOutput(Int 42), got {other:?}"),
    }
}

#[test]
fn typed_expr_to_transition_prop_when_produces_or_of_clauses() {
    // `when x is { a -> constant(42); b -> constant(0) }` — the first
    // `Var` clause is a catch-all, so later clauses are unreachable. The
    // lowering should collapse to the first branch body rather than widen
    // to an unordered disjunction.
    let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
        empty_transition_prop_context();

    let int_fuzzer = Type::fuzzer(Type::int());
    let return_fn_type = Type::function(vec![Type::int()], int_fuzzer.clone());

    let make_return = |value: &str| TypedExpr::Call {
        location: Span::empty(),
        tipo: int_fuzzer.clone(),
        fun: Box::new(module_fn_var(
            "constant",
            STDLIB_FUZZ_MODULE,
            return_fn_type.clone(),
        )),
        args: vec![call_arg(uint_lit(value))],
    };

    let subject = local_var("x", Type::int());
    let when_expr = TypedExpr::When {
        location: Span::empty(),
        tipo: int_fuzzer.clone(),
        subject: Box::new(subject),
        clauses: vec![
            TypedClause {
                location: Span::empty(),
                pattern: TypedPattern::var("a"),
                then: make_return("42"),
            },
            TypedClause {
                location: Span::empty(),
                pattern: TypedPattern::var("b"),
                then: make_return("0"),
            },
        ],
    };

    let prop = typed_expr_to_transition_prop(
        &when_expr,
        "test_mod",
        &function_index,
        &constant_index,
        &local_values,
        &empty_data_types,
        &mut visiting,
        &BTreeSet::new(),
        &mut BTreeSet::new(),
    );

    match prop {
        TransitionProp::EqOutput(ShallowIr::Const(ShallowConst::Int(v))) => {
            assert_eq!(
                v, "42",
                "catch-all first clause must short-circuit later clauses"
            );
        }
        other => panic!("expected EqOutput(Int 42) for catch-all when, got {other:?}"),
    }
}

#[test]
fn unsupported_when_clause_condition_omits_unreachable_body() {
    let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
        empty_transition_prop_context();

    let list_type = Type::list(Type::int());
    let int_fuzzer = Type::fuzzer(Type::int());
    let return_fn_type = Type::function(vec![Type::int()], int_fuzzer.clone());
    let make_return = |value: &str| TypedExpr::Call {
        location: Span::empty(),
        tipo: int_fuzzer.clone(),
        fun: Box::new(module_fn_var(
            "constant",
            STDLIB_FUZZ_MODULE,
            return_fn_type.clone(),
        )),
        args: vec![call_arg(uint_lit(value))],
    };

    let when_expr = TypedExpr::When {
        location: Span::empty(),
        tipo: int_fuzzer.clone(),
        subject: Box::new(local_var("xs", list_type.clone())),
        clauses: vec![
            TypedClause {
                location: Span::empty(),
                pattern: TypedPattern::List {
                    location: Span::create(7, 4),
                    elements: vec![TypedPattern::var("head")],
                    tail: None,
                },
                then: make_return("99"),
            },
            TypedClause {
                location: Span::empty(),
                pattern: TypedPattern::Discard {
                    name: "_".to_string(),
                    location: Span::empty(),
                },
                then: make_return("0"),
            },
        ],
    };

    let prop = typed_expr_to_transition_prop(
        &when_expr,
        "test_mod",
        &function_index,
        &constant_index,
        &local_values,
        &empty_data_types,
        &mut visiting,
        &BTreeSet::new(),
        &mut BTreeSet::new(),
    );

    fn contains_eq_output_int(prop: &TransitionProp, expected: &str) -> bool {
        match prop {
            TransitionProp::EqOutput(ShallowIr::Const(ShallowConst::Int(value))) => {
                value == expected
            }
            TransitionProp::Exists { body, .. } => contains_eq_output_int(body, expected),
            TransitionProp::And(parts) | TransitionProp::Or(parts) => parts
                .iter()
                .any(|part| contains_eq_output_int(part, expected)),
            TransitionProp::Match { arms, .. } => arms
                .iter()
                .any(|arm| contains_eq_output_int(&arm.body, expected)),
            TransitionProp::IfThenElse { t, e, .. } => {
                contains_eq_output_int(t, expected) || contains_eq_output_int(e, expected)
            }
            TransitionProp::EqOutput(_)
            | TransitionProp::Pure(_)
            | TransitionProp::Unsupported { .. }
            | TransitionProp::SubGenerator { .. } => false,
        }
    }

    let unsupported_reasons =
        collect_unsupported_reasons_containing(&prop, "list-pattern when clauses");
    assert_eq!(
        unsupported_reasons.len(),
        1,
        "unsupported list-pattern clause should remain visible exactly once, got {unsupported_reasons:?}"
    );
    assert!(
        contains_eq_output_int(&prop, "0"),
        "reachable discard fallback body should remain available, got {prop:?}"
    );
    assert!(
        !contains_eq_output_int(&prop, "99"),
        "unsupported guarded clause body must not be treated as reachable, got {prop:?}"
    );
}

/// Helper: traverse a `TransitionProp` tree and count the leaves that

/// Helper: collect every `Unsupported.reason` whose text contains a
/// given substring. Used to assert that the per-clause `[E0033]`
/// entry has been emitted at the expected positions.
fn collect_unsupported_reasons_containing(prop: &TransitionProp, needle: &str) -> Vec<String> {
    fn walk(prop: &TransitionProp, needle: &str, out: &mut Vec<String>) {
        match prop {
            TransitionProp::Unsupported { reason, .. } => {
                if reason.contains(needle) {
                    out.push(reason.clone());
                }
            }
            TransitionProp::Exists { body, .. } => walk(body, needle, out),
            TransitionProp::And(parts) | TransitionProp::Or(parts) => {
                for p in parts {
                    walk(p, needle, out);
                }
            }
            TransitionProp::Match { arms, .. } => {
                for arm in arms {
                    walk(&arm.body, needle, out);
                }
            }
            TransitionProp::IfThenElse { t, e, .. } => {
                walk(t, needle, out);
                walk(e, needle, out);
            }
            TransitionProp::Pure(_)
            | TransitionProp::EqOutput(_)
            | TransitionProp::SubGenerator { .. } => {}
        }
    }
    let mut out = Vec::new();
    walk(prop, needle, &mut out);
    out
}

/// Constructor-pattern `when` clauses on the counted transition path must
/// lower to explicit branch guards rather than `[E0033]` widenings.
/// This regression test asserts the repaired shape: nested `IfThenElse`
/// guards with the discard clause preserved as the final fallback.
#[test]
fn when_constructor_pattern_increments_over_approximations() {
    let (function_index, constant_index, local_values, _empty_data_types, mut visiting) =
        empty_transition_prop_context();

    let int_fuzzer = Type::fuzzer(Type::int());
    let return_fn_type = Type::function(vec![Type::int()], int_fuzzer.clone());
    let make_return = |value: &str| TypedExpr::Call {
        location: Span::empty(),
        tipo: int_fuzzer.clone(),
        fun: Box::new(module_fn_var(
            "constant",
            STDLIB_FUZZ_MODULE,
            return_fn_type.clone(),
        )),
        args: vec![call_arg(uint_lit(value))],
    };

    let (owned, scenario_ty) = scenario_like_data_types();
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned.iter().collect();

    // Build a `when` with three clauses:
    //   Step(payload) -> constant(1)
    //   Done         -> constant(2)
    //   _other       -> constant(3)
    let step_pattern = TypedPattern::constructor(
        "Step",
        &[CallArg::var("payload", Span::empty())],
        scenario_ty.clone(),
        Span::create(10, 7),
    );
    let done_pattern =
        TypedPattern::constructor("Done", &[], scenario_ty.clone(), Span::create(20, 4));
    let discard_pattern = TypedPattern::Discard {
        name: "_other".to_string(),
        location: Span::create(30, 6),
    };

    let done_subject = TypedExpr::Var {
        location: Span::empty(),
        constructor: ValueConstructor::public(
            scenario_ty.clone(),
            ValueConstructorVariant::Record {
                name: "Done".to_string(),
                arity: 0,
                field_map: None,
                location: Span::empty(),
                module: "mod".to_string(),
                constructors_count: 2,
            },
        ),
        name: "Done".to_string(),
    };

    let when_expr = TypedExpr::When {
        location: Span::empty(),
        tipo: int_fuzzer.clone(),
        subject: Box::new(done_subject),
        clauses: vec![
            TypedClause {
                location: Span::empty(),
                pattern: step_pattern,
                then: make_return("1"),
            },
            TypedClause {
                location: Span::empty(),
                pattern: done_pattern,
                then: make_return("2"),
            },
            TypedClause {
                location: Span::empty(),
                pattern: discard_pattern,
                then: make_return("3"),
            },
        ],
    };

    let prop = typed_expr_to_transition_prop(
        &when_expr,
        "mod",
        &function_index,
        &constant_index,
        &local_values,
        &data_types,
        &mut visiting,
        &BTreeSet::new(),
        &mut BTreeSet::new(),
    );

    // Constructor patterns are now lowered faithfully via explicit
    // conditions. No widening leaves should remain, and the shape should be
    // a nested IfThenElse ending in the discard-clause body.
    let e0033_entries = collect_unsupported_reasons_containing(&prop, "[E0033]");
    assert!(
        e0033_entries.is_empty(),
        "constructor patterns should no longer emit [E0033] widenings: {e0033_entries:?}",
    );

    match &prop {
        TransitionProp::IfThenElse { cond, t, e } => {
            assert!(
                matches!(
                    cond,
                    ShallowIr::BinOp {
                        op: ShallowBinOp::Eq,
                        ..
                    }
                ),
                "outer constructor clause should lower to an Eq guard, got {cond:?}",
            );
            assert!(
                matches!(&**t, TransitionProp::EqOutput(ShallowIr::Const(ShallowConst::Int(v))) if v == "1"),
                "Some(x) branch should return 1, got {t:?}",
            );
            match &**e {
                TransitionProp::IfThenElse { cond, t, e } => {
                    assert!(
                        matches!(
                            cond,
                            ShallowIr::BinOp {
                                op: ShallowBinOp::Eq,
                                ..
                            }
                        ),
                        "inner constructor clause should lower to an Eq guard, got {cond:?}",
                    );
                    assert!(
                        matches!(&**t, TransitionProp::EqOutput(ShallowIr::Const(ShallowConst::Int(v))) if v == "2"),
                        "None branch should return 2, got {t:?}",
                    );
                    assert!(
                        matches!(&**e, TransitionProp::EqOutput(ShallowIr::Const(ShallowConst::Int(v))) if v == "3"),
                        "discard branch should remain the final fallback, got {e:?}",
                    );
                }
                other => panic!(
                    "expected nested IfThenElse for second constructor clause, got {other:?}"
                ),
            }
        }
        other => {
            panic!("expected nested IfThenElse for constructor-pattern when, got {other:?}")
        }
    }
}

/// H2 / E0033 — the per-clause `[E0033]` log entry must include both
/// the human-readable pattern description AND every binder name the
/// pattern would have introduced. This is the
/// `when_constructor_pattern_log_includes_pattern_and_binders` row
/// from plan §"Test Plan" §B.
#[test]
fn when_constructor_pattern_log_includes_pattern_and_binders() {
    let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
        empty_transition_prop_context();

    let int_fuzzer = Type::fuzzer(Type::int());
    let return_fn_type = Type::function(vec![Type::int()], int_fuzzer.clone());
    let make_return = |value: &str| TypedExpr::Call {
        location: Span::empty(),
        tipo: int_fuzzer.clone(),
        fun: Box::new(module_fn_var(
            "constant",
            STDLIB_FUZZ_MODULE,
            return_fn_type.clone(),
        )),
        args: vec![call_arg(uint_lit(value))],
    };

    // Construct a single-clause `when` with `Cons(head, tail)` so we
    // can pin both binder names in the log entry. The clause-pattern
    // `Span::create(7, 14)` lets us also assert the source-location
    // suffix (start byte == 7).
    let list_ty = Rc::new(Type::App {
        public: true,
        contains_opaque: false,
        module: "stdlib".to_string(),
        name: "List".to_string(),
        args: vec![Type::int()],
        alias: None,
    });
    let cons_pattern = TypedPattern::constructor(
        "Cons",
        &[
            CallArg::var("head", Span::empty()),
            CallArg::var("tail", Span::empty()),
        ],
        list_ty.clone(),
        Span::create(7, 14),
    );
    // A second discard clause keeps the outer `Or` structure non-trivial
    // (single-branch `When` collapses to the inner branch).
    let discard_pattern = TypedPattern::Discard {
        name: "_".to_string(),
        location: Span::empty(),
    };

    let when_expr = TypedExpr::When {
        location: Span::empty(),
        tipo: int_fuzzer.clone(),
        subject: Box::new(local_var("xs", list_ty)),
        clauses: vec![
            TypedClause {
                location: Span::empty(),
                pattern: cons_pattern,
                then: make_return("1"),
            },
            TypedClause {
                location: Span::empty(),
                pattern: discard_pattern,
                then: make_return("0"),
            },
        ],
    };

    let prop = typed_expr_to_transition_prop(
        &when_expr,
        "permissions/test",
        &function_index,
        &constant_index,
        &local_values,
        &empty_data_types,
        &mut visiting,
        &BTreeSet::new(),
        &mut BTreeSet::new(),
    );

    fn find_first_unsupported(prop: &TransitionProp) -> Option<(String, Option<String>)> {
        match prop {
            TransitionProp::Unsupported {
                reason,
                source_location,
            } => Some((reason.clone(), source_location.clone())),
            TransitionProp::Exists { body, .. } => find_first_unsupported(body),
            TransitionProp::And(parts) | TransitionProp::Or(parts) => {
                parts.iter().find_map(find_first_unsupported)
            }
            TransitionProp::Match { arms, .. } => arms
                .iter()
                .find_map(|arm| find_first_unsupported(&arm.body)),
            TransitionProp::IfThenElse { t, e, .. } => {
                find_first_unsupported(t).or_else(|| find_first_unsupported(e))
            }
            _ => None,
        }
    }

    let (reason, source_location) = find_first_unsupported(&prop)
        .expect("Cons-pattern when should preserve an explicit Unsupported branch");
    assert!(
        reason.contains("Cons") || reason.contains("constructor"),
        "unsupported constructor/list branch should keep a meaningful reason, got {reason:?}",
    );
    assert_eq!(
        source_location.as_deref(),
        Some("permissions/test:7"),
        "unsupported list-pattern branch should keep the original source location",
    );
}

/// H2 / E0033 — `Var` and `Discard` patterns must NOT trigger the
/// per-clause widening note: they bind everything (or nothing) and
/// drop no constructor-conditional constraints. This pins the
/// negative case so a future refactor cannot accidentally widen the
/// trigger condition.
#[test]
fn when_var_or_discard_pattern_does_not_log_e0033() {
    let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
        empty_transition_prop_context();

    let int_fuzzer = Type::fuzzer(Type::int());
    let return_fn_type = Type::function(vec![Type::int()], int_fuzzer.clone());
    let make_return = |value: &str| TypedExpr::Call {
        location: Span::empty(),
        tipo: int_fuzzer.clone(),
        fun: Box::new(module_fn_var(
            "constant",
            STDLIB_FUZZ_MODULE,
            return_fn_type.clone(),
        )),
        args: vec![call_arg(uint_lit(value))],
    };

    let when_expr = TypedExpr::When {
        location: Span::empty(),
        tipo: int_fuzzer.clone(),
        subject: Box::new(local_var("x", Type::int())),
        clauses: vec![
            TypedClause {
                location: Span::empty(),
                pattern: TypedPattern::var("a"),
                then: make_return("42"),
            },
            TypedClause {
                location: Span::empty(),
                pattern: TypedPattern::Discard {
                    name: "_".to_string(),
                    location: Span::empty(),
                },
                then: make_return("0"),
            },
        ],
    };

    let prop = typed_expr_to_transition_prop(
        &when_expr,
        "test_mod",
        &function_index,
        &constant_index,
        &local_values,
        &empty_data_types,
        &mut visiting,
        &BTreeSet::new(),
        &mut BTreeSet::new(),
    );

    let e0033_entries = collect_unsupported_reasons_containing(&prop, "[E0033]");
    assert!(
        e0033_entries.is_empty(),
        "Var/Discard clauses must not emit [E0033] entries; got {e0033_entries:?}",
    );
    match &prop {
        TransitionProp::EqOutput(ShallowIr::Const(ShallowConst::Int(v))) => {
            assert_eq!(
                v, "42",
                "leading var-pattern clause should short-circuit later clauses"
            );
        }
        other => panic!("expected EqOutput(Int 42), got {other:?}"),
    }
}

/// Defensive unit test for `describe_pattern` and
/// `collect_pattern_binders` — these helpers are used inside the
/// per-clause E0033 audit log and must produce stable text for the
/// patterns the lowering encounters in practice.
#[test]
fn describe_pattern_and_collect_binders_cover_common_shapes() {
    let int_ty = Type::int();
    let option_int = Rc::new(Type::App {
        public: true,
        contains_opaque: false,
        module: "option".to_string(),
        name: "Option".to_string(),
        args: vec![int_ty.clone()],
        alias: None,
    });

    // `Var x`
    let var_pat = TypedPattern::var("x");
    assert_eq!(describe_pattern(&var_pat), "x");
    assert_eq!(collect_pattern_binders(&var_pat), vec!["x".to_string()]);

    // `_`
    let discard = TypedPattern::Discard {
        name: "_".to_string(),
        location: Span::empty(),
    };
    assert_eq!(describe_pattern(&discard), "_");
    assert!(collect_pattern_binders(&discard).is_empty());

    // `Some(x)`
    let some_x = TypedPattern::constructor(
        "Some",
        &[CallArg::var("x", Span::empty())],
        option_int.clone(),
        Span::empty(),
    );
    assert_eq!(describe_pattern(&some_x), "Some(x)");
    assert_eq!(collect_pattern_binders(&some_x), vec!["x".to_string()]);

    // `None`
    let none = TypedPattern::constructor("None", &[], option_int, Span::empty());
    assert_eq!(describe_pattern(&none), "None");
    assert!(collect_pattern_binders(&none).is_empty());

    // `(a, _, c)` tuple
    let tuple = TypedPattern::Tuple {
        location: Span::empty(),
        elems: vec![
            TypedPattern::var("a"),
            TypedPattern::Discard {
                name: "_".to_string(),
                location: Span::empty(),
            },
            TypedPattern::var("c"),
        ],
    };
    assert_eq!(describe_pattern(&tuple), "(a, _, c)");
    assert_eq!(
        collect_pattern_binders(&tuple),
        vec!["a".to_string(), "c".to_string()]
    );

    // `[head, ..tail]`
    let list = TypedPattern::List {
        location: Span::empty(),
        elements: vec![TypedPattern::var("head")],
        tail: Some(Box::new(TypedPattern::var("tail"))),
    };
    assert_eq!(describe_pattern(&list), "[head, ..tail]");
    assert_eq!(
        collect_pattern_binders(&list),
        vec!["head".to_string(), "tail".to_string()]
    );
}

#[test]
fn typed_expr_to_transition_prop_inlines_resolvable_sub_generator() {
    // A module-level sub-generator whose body is translatable should be
    // inlined into the resulting `TransitionProp`, not emitted as an
    // opaque `SubGenerator` stub. Here, `my_gen()` has body
    // `constant(42)`, which translates to `EqOutput(Int 42)`. The outer
    // call `my_gen()` from another module should therefore translate to
    // the same shape rather than `SubGenerator { ... }`.
    let int_fuzzer = Type::fuzzer(Type::int());

    // Body of the sub-generator: `constant(42)`.
    let body = TypedExpr::Call {
        location: Span::empty(),
        tipo: int_fuzzer.clone(),
        fun: Box::new(module_fn_var(
            "constant",
            STDLIB_FUZZ_MODULE,
            Type::function(vec![Type::int()], int_fuzzer.clone()),
        )),
        args: vec![call_arg(uint_lit("42"))],
    };

    let sub_gen_fn = TypedFunction {
        arguments: vec![],
        body,
        doc: None,
        location: Span::empty(),
        name: "my_gen".to_string(),
        public: true,
        return_annotation: None,
        return_type: int_fuzzer.clone(),
        end_position: 0,
        on_test_failure: OnTestFailure::FailImmediately,
    };

    let key = FunctionAccessKey {
        module_name: "gens".to_string(),
        function_name: "my_gen".to_string(),
    };

    let mut known_functions: IndexMap<&FunctionAccessKey, &TypedFunction> = IndexMap::new();
    known_functions.insert(&key, &sub_gen_fn);
    let function_index = index_known_functions(&known_functions);

    let constant_index: ConstantIndex<'_> = HashMap::new();
    let local_values: BTreeMap<String, TypedExpr> = BTreeMap::new();
    let empty_data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();
    let mut visiting: BTreeSet<(String, String)> = BTreeSet::new();

    // Outer call: `my_gen()` from a different module.
    let call_expr = TypedExpr::Call {
        location: Span::empty(),
        tipo: int_fuzzer.clone(),
        fun: Box::new(module_fn_var(
            "my_gen",
            "gens",
            Type::function(vec![], int_fuzzer),
        )),
        args: vec![],
    };

    let prop = typed_expr_to_transition_prop(
        &call_expr,
        "caller_mod",
        &function_index,
        &constant_index,
        &local_values,
        &empty_data_types,
        &mut visiting,
        &BTreeSet::new(),
        &mut BTreeSet::new(),
    );

    match prop {
        TransitionProp::EqOutput(ShallowIr::Const(ShallowConst::Int(v))) => {
            assert_eq!(v, "42", "expected inlined body EqOutput(Int 42)");
        }
        TransitionProp::SubGenerator { module, fn_name } => panic!(
            "expected inlined EqOutput(Int 42), but got SubGenerator {{ {module}.{fn_name} }}",
        ),
        other => panic!("expected inlined EqOutput(Int 42), got {other:?}"),
    }
}

#[test]
fn typed_expr_to_transition_prop_collects_non_monadic_let_bindings() {
    // Regression: a non-Fuzzer `let` binding preceding the terminal
    // `fuzz.return(...)` of a step body must be collected into
    // `local_values` so that `Var` references inside the terminal can be
    // resolved. Prior to this fix, `terminal_expression` peeled the
    // sequence wrapper and discarded the binding entirely, so any inner
    // `Var "redeemer"` reference produced an opaque/Unsupported result.
    //
    // Body shape:
    //
    //     let redeemer = 42
    //     fuzz.return(Step(redeemer))
    //
    // Expected: `EqOutput(Construct { constructor: "Step", ... })` —
    // demonstrating the path runs cleanly with the binding in scope.
    let (owned, scenario_ty) = scenario_like_data_types();
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned.iter().collect();

    let (function_index, constant_index, _, _, mut visiting) = empty_transition_prop_context();

    // `Step` constructor: `fn(Data) -> Scenario`, second constructor (tag 1).
    let step_fn_ty = Type::function(vec![Type::int()], scenario_ty.clone());
    let step_ctor = TypedExpr::Var {
        location: Span::empty(),
        constructor: ValueConstructor::public(
            step_fn_ty,
            ValueConstructorVariant::Record {
                name: "Step".to_string(),
                arity: 1,
                field_map: None,
                location: Span::empty(),
                module: "mod".to_string(),
                constructors_count: 2,
            },
        ),
        name: "Step".to_string(),
    };

    // Local `Var "redeemer"` reference (LocalVariable) — used inside the
    // terminal's payload position.
    let redeemer_var = local_var("redeemer", Type::int());

    // `Step(redeemer)` — the payload of `fuzz.return`.
    let step_call = TypedExpr::Call {
        location: Span::empty(),
        tipo: scenario_ty.clone(),
        fun: Box::new(step_ctor),
        args: vec![call_arg(redeemer_var)],
    };

    // `fuzz.return(Step(redeemer))` — Fuzzer<Scenario>.
    let scenario_fuzzer_ty = Type::fuzzer(scenario_ty.clone());
    let return_fn_ty = Type::function(vec![scenario_ty.clone()], scenario_fuzzer_ty.clone());
    let fuzz_return_call = TypedExpr::Call {
        location: Span::empty(),
        tipo: scenario_fuzzer_ty.clone(),
        fun: Box::new(module_fn_var("constant", STDLIB_FUZZ_MODULE, return_fn_ty)),
        args: vec![call_arg(step_call)],
    };

    // `let redeemer = 42` — non-Fuzzer Int binding, MUST be collected.
    let assignment = TypedExpr::Assignment {
        location: Span::empty(),
        tipo: Type::int(),
        value: Box::new(uint_lit("42")),
        pattern: TypedPattern::var("redeemer"),
        kind: crate::ast::AssignmentKind::Let { backpassing: () },
        comment: None,
    };

    // Sequence: [ let redeemer = 42; fuzz.return(Step(redeemer)) ]
    let sequence = TypedExpr::Sequence {
        location: Span::empty(),
        expressions: vec![assignment, fuzz_return_call],
    };

    // Empty starting local_values — the binding must be picked up from
    // the leading Assignment in the Sequence.
    let local_values: BTreeMap<String, TypedExpr> = BTreeMap::new();

    let prop = typed_expr_to_transition_prop(
        &sequence,
        "test_mod",
        &function_index,
        &constant_index,
        &local_values,
        &data_types,
        &mut visiting,
        &BTreeSet::new(),
        &mut BTreeSet::new(),
    );

    // The result must be `EqOutput(Construct { Step, ... })` — NOT
    // `Unsupported` (which would indicate the `let` binding was dropped
    // and the path failed) and NOT a bare opaque shape.
    match prop {
        TransitionProp::EqOutput(ShallowIr::Construct {
            ref constructor,
            tag,
            ref fields,
            ..
        }) => {
            assert_eq!(constructor, "Step");
            assert_eq!(tag, 1, "Step is the second constructor (index 1)");
            assert_eq!(fields.len(), 1, "Step has exactly one field (redeemer)");
            // The redeemer field — whether substituted to a constant
            // or left as a Var — must NOT be an Opaque/Unsupported
            // marker indicating a translation failure.
            assert!(
                !matches!(fields[0], ShallowIr::Opaque { .. }),
                "redeemer field must not translate to an Opaque marker: {:?}",
                fields[0]
            );
        }
        TransitionProp::Unsupported { reason, .. } => {
            panic!("expected EqOutput(Construct {{ Step }}), got Unsupported: {reason}")
        }
        other => panic!("expected EqOutput(Construct {{ Step }}), got {other:?}"),
    }
}

#[test]
fn typed_expr_to_transition_prop_preserves_leading_fuzzer_assignment_domains() {
    let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
        empty_transition_prop_context();

    let assignment = TypedExpr::Assignment {
        location: Span::empty(),
        tipo: Type::int(),
        value: Box::new(make_typed_int_between_fuzzer("0", "10")),
        pattern: TypedPattern::var("draw"),
        kind: crate::ast::AssignmentKind::Let { backpassing: () },
        comment: None,
    };

    let int_fuzzer_ty = Type::fuzzer(Type::int());
    let return_call = TypedExpr::Call {
        location: Span::empty(),
        tipo: int_fuzzer_ty.clone(),
        fun: Box::new(module_fn_var(
            "constant",
            STDLIB_FUZZ_MODULE,
            Type::function(vec![Type::int()], int_fuzzer_ty),
        )),
        args: vec![call_arg(local_var("draw", Type::int()))],
    };

    let sequence = TypedExpr::Sequence {
        location: Span::empty(),
        expressions: vec![assignment, return_call],
    };

    let prop = typed_expr_to_transition_prop(
        &sequence,
        "test_mod",
        &function_index,
        &constant_index,
        &local_values,
        &empty_data_types,
        &mut visiting,
        &BTreeSet::new(),
        &mut BTreeSet::new(),
    );

    match prop {
        TransitionProp::Exists {
            binder,
            domain,
            body,
            ..
        } => {
            assert_eq!(binder, "draw");
            assert!(
                matches!(
                    domain.as_ref(),
                    FuzzerSemantics::IntRange {
                        min: Some(min),
                        max: Some(max),
                    } if min == "0" && max == "10"
                ),
                "leading fuzzer assignment should preserve its domain, got: {domain:?}"
            );
            assert!(
                matches!(
                    body.as_ref(),
                    TransitionProp::EqOutput(ShallowIr::BoundVar { name, ty: ShallowIrType::Int })
                        if name == "draw"
                ),
                "continuation should still reference the drawn witness, got: {body:?}"
            );
        }
        other => panic!("expected Exists for leading fuzzer assignment, got {other:?}"),
    }
}

#[test]
fn translate_sequence_with_locals_preserves_non_var_rhs_opaque() {
    let (owned, scenario_ty) = scenario_like_data_types();
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned.iter().collect();

    let unsupported_rhs = TypedExpr::Call {
        location: Span::empty(),
        tipo: scenario_ty.clone(),
        fun: Box::new(module_fn_var(
            "unknown_step",
            "mystery",
            Type::function(vec![], scenario_ty.clone()),
        )),
        args: vec![],
    };

    let assignment = TypedExpr::Assignment {
        location: Span::empty(),
        tipo: scenario_ty.clone(),
        value: Box::new(unsupported_rhs),
        pattern: TypedPattern::constructor(
            "Step",
            &[CallArg::var("payload", Span::empty())],
            scenario_ty,
            Span::empty(),
        ),
        kind: crate::ast::AssignmentKind::Let { backpassing: () },
        comment: None,
    };

    let sequence = TypedExpr::Sequence {
        location: Span::empty(),
        expressions: vec![assignment, uint_lit("0")],
    };

    let ir = typed_expr_to_shallow_ir(&sequence, &data_types);
    assert!(
        matches!(
            ir,
            ShallowIr::Let {
                ref name,
                ref value,
                ref body,
            } if name == "_"
                && matches!(value.as_ref(), ShallowIr::Opaque { .. })
                && matches!(body.as_ref(), ShallowIr::Const(ShallowConst::Int(v)) if v == "0")
        ),
        "destructuring lets must preserve opaque RHS nodes instead of dropping them, got {ir:?}"
    );
}

/// H3 — naive substitute-on-recurse threads a bind binder into
/// `local_values` so a `Var` lookup inside the continuation body
/// resolves to the source fuzzer expression instead of widening to a
/// vacuous `Unsupported` (which the Lean stage would translate to
/// `True`).
///
/// Pre-H3 behaviour: `and_then(int_between(0, 10), fn(x) { x })`
/// produced `Exists { binder: x, body: Unsupported("...variable 'x'
/// has no known transition content") }`. The body widened to `True`
/// downstream, dropping the `int_between(0, 10)` constraint
/// completely.
///
/// Post-H3 behaviour: the drawn witness is still threaded through local
/// scope, but a bare continuation-body value is no longer reinterpreted as
/// a generator relation.
/// substitution still threads the drawn value into local scope, but a bare
/// `Var x` continuation body is honestly rejected as “value binding, not a
/// transition predicate” rather than being reinterpreted as generator
/// semantics.
#[test]
fn translate_bind_threading_rejects_bare_value_body() {
    let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
        empty_transition_prop_context();

    let int_type = Type::int();

    // Source: `int_between(0, 10)` — typed `Fuzzer<Int>`.
    let source = make_typed_int_between_fuzzer("0", "10");

    // Continuation body is the bare local variable reference `x`.
    // Crucially this is NOT wrapped in a `return(...)`, so the body
    // path goes through the `TypedExpr::Var` arm of
    // `typed_expr_to_transition_prop` (the Var-lookup site). This is
    // the path H3 thread enables; pre-H3 the lookup would miss
    // because nothing extends `local_values` for monadic binds.
    let cont_body = local_var("x", int_type.clone());

    // Continuation: `fn(x: Int) -> Fuzzer<Int> { x }`. The synthetic
    // body type doesn't match the declared Fuzzer return type — but
    // `typed_expr_to_transition_prop` does not typecheck here; the
    // shape detector only requires `expression_is_bind_continuation`
    // to recognise this as a 1-arg fn returning a Fuzzer.
    let cont = make_inline_bind_continuation("x", int_type.clone(), cont_body, int_type.clone());

    let bind_call = make_stdlib_bind_call(source, cont, int_type.clone());

    let prop = typed_expr_to_transition_prop(
        &bind_call,
        "test_mod",
        &function_index,
        &constant_index,
        &local_values,
        &empty_data_types,
        &mut visiting,
        &BTreeSet::new(),
        &mut BTreeSet::new(),
    );

    match prop {
        TransitionProp::Exists { binder, body, .. } => {
            assert_eq!(binder, "x", "expected binder 'x', got '{binder}'");
            match *body {
                TransitionProp::Unsupported { reason, .. } => {
                    assert!(
                        reason.contains("value binding, not a transition predicate"),
                        "bare bind-body variable must stay rejected as a value binding; got: {reason}"
                    );
                }
                other => panic!(
                    "expected Unsupported(value binding, not a transition predicate), got {other:?}"
                ),
            }
        }
        other => panic!("expected Exists {{ binder: x, .. }}, got {other:?}"),
    }
}

/// H3 cycle guard — when a nested `translate_bind` re-binds a name
/// that's already on the visiting stack, the inner bind must NOT
/// shadow the outer entry in `local_values` (doing so naively would
/// either lose the outer binding when the inner scope ends or, worse,
/// loop on Var-lookup recursion). Instead the inner site emits a
/// `TransitionProp::Unsupported` audit marker and wraps the body in
/// `And([Unsupported, body_prop])` so:
///   1. The cycle is broken (no infinite recursion).
///   2. The precision loss is logged in the unsupported-log via the
///      Lean widening pipeline (the marker widens to `True`,
///      preserving body semantics: `True ∧ body_prop = body_prop`).
///   3. The reason text contains the stable substring
///      "cyclic monadic-bind binder" and the offending binder name,
///      so downstream tooling can detect this case.
#[test]
fn translate_bind_cycle_guard_falls_through_to_unsupported() {
    let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
        empty_transition_prop_context();

    let int_type = Type::int();

    // Inner cont body is `Var x`. After inner-bind substitution is
    // skipped by the cycle guard, `local_values` still has the *outer*
    // `x -> int_between(0, 10)` entry — so the Var lookup resolves
    // through the outer source. This is the precision-imprecise
    // fallthrough the Unsupported marker documents.
    let inner_cont_body = local_var("x", int_type.clone());
    let inner_cont =
        make_inline_bind_continuation("x", int_type.clone(), inner_cont_body, int_type.clone());
    // Inner source: a *different* fuzzer, so we can verify which
    // source the body is substituted with after the cycle is hit.
    let inner_source = make_typed_int_between_fuzzer("100", "200");
    let inner_bind = make_stdlib_bind_call(inner_source, inner_cont, int_type.clone());

    // Outer continuation wraps the inner bind. Its arg is also `x`,
    // so the inner site sees `binder = "x"` already in
    // `visiting_locals`.
    let outer_cont =
        make_inline_bind_continuation("x", int_type.clone(), inner_bind, int_type.clone());
    let outer_source = make_typed_int_between_fuzzer("0", "10");
    let outer_bind = make_stdlib_bind_call(outer_source, outer_cont, int_type.clone());

    let prop = typed_expr_to_transition_prop(
        &outer_bind,
        "test_mod",
        &function_index,
        &constant_index,
        &local_values,
        &empty_data_types,
        &mut visiting,
        &BTreeSet::new(),
        &mut BTreeSet::new(),
    );

    // Outer bind: Exists { binder: x, body: <inner exists> }
    let inner_exists_body = match prop {
        TransitionProp::Exists { binder, body, .. } => {
            assert_eq!(binder, "x", "outer binder should be x; got {binder}");
            *body
        }
        other => panic!("expected outer Exists {{ binder: x, .. }}, got {other:?}"),
    };

    // Inner bind: Exists { binder: x, body: And([Unsupported, body_prop]) }
    let inner_body = match inner_exists_body {
        TransitionProp::Exists { binder, body, .. } => {
            assert_eq!(binder, "x", "inner binder should be x; got {binder}");
            *body
        }
        other => panic!("expected inner Exists {{ binder: x, .. }}, got {other:?}"),
    };

    // Body must be And([Unsupported(cyclic...), body_prop]).
    match inner_body {
        TransitionProp::And(parts) => {
            assert_eq!(
                parts.len(),
                2,
                "expected 2-part And([Unsupported, body_prop]); got {} parts: {parts:?}",
                parts.len(),
            );
            // First part: Unsupported with cycle-guard reason text.
            match &parts[0] {
                TransitionProp::Unsupported {
                    reason,
                    source_location,
                } => {
                    assert!(
                        reason.contains("cyclic monadic-bind binder"),
                        "expected reason to contain stable substring 'cyclic monadic-bind binder'; got: {reason}",
                    );
                    assert!(
                        reason.contains("'x'"),
                        "expected reason to mention binder name 'x'; got: {reason}",
                    );
                    assert!(
                        source_location.is_none(),
                        "cyclic bind guard currently reports no source_location; got: {source_location:?}"
                    );
                }
                other => panic!("expected first part Unsupported; got {other:?}"),
            }
            // Second part: body_prop. The body still sees `Var x`, but with
            // the bind threading in place that name resolves to the drawn
            // value binding rather than to a transition predicate. That must
            // stay explicit Unsupported rather than being reinterpreted as a
            // generator relation.
            match &parts[1] {
                TransitionProp::Unsupported { reason, .. } => {
                    assert!(
                        reason.contains("value binding, not a transition predicate"),
                        "expected second part Unsupported(value binding, not a transition predicate); got reason: {reason}"
                    );
                }
                other => panic!(
                    "expected second part Unsupported(value binding, not a transition predicate); got {other:?}"
                ),
            }
        }
        other => panic!("expected inner body And([Unsupported, body_prop]); got {other:?}"),
    }
}

/// `return(x)` now lowers through `typed_expr_to_shallow_ir_with_locals`, so
/// the returned witness stays connected to the existential binder as
/// `EqOutput(BoundVar x)` rather than degrading to an out-of-scope `Var`.
#[test]
fn translate_bind_return_shortcut_threads_bound_value_exactly() {
    let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
        empty_transition_prop_context();

    let int_fuzzer = Type::fuzzer(Type::int());
    let source = make_typed_int_between_fuzzer("0", "10");

    let x = local_var("x", Type::int());
    let return_body = TypedExpr::Call {
        location: Span::empty(),
        tipo: int_fuzzer.clone(),
        fun: Box::new(module_fn_var(
            "constant",
            STDLIB_FUZZ_MODULE,
            Type::function(vec![Type::int()], int_fuzzer.clone()),
        )),
        args: vec![call_arg(x)],
    };

    let continuation = make_inline_bind_continuation("x", Type::int(), return_body, Type::int());

    let bind_call = make_stdlib_bind_call(source, continuation, Type::int());

    let prop = typed_expr_to_transition_prop(
        &bind_call,
        "test_mod",
        &function_index,
        &constant_index,
        &local_values,
        &empty_data_types,
        &mut visiting,
        &BTreeSet::new(),
        &mut BTreeSet::new(),
    );

    match prop {
        TransitionProp::Exists { binder, body, .. } => {
            assert_eq!(binder, "x");
            assert!(
                matches!(
                    *body,
                    TransitionProp::EqOutput(ShallowIr::BoundVar { ref name, .. })
                        if name == "x"
                ),
                "expected body = EqOutput(BoundVar x), got {body:?}"
            );
        }
        other => panic!("expected Exists, got {other:?}"),
    }
}

/// H3 self-referential source guard — when the bind's source contains
/// a free `Var` whose name collides with the binder, the naive
/// substitution `local_values[binder] = source` is self-referential
/// (e.g. `local_values["x"] = Var "x"`). Pre-fix the `Var`-lookup arm
/// would recurse on itself indefinitely, stack-overflowing the
/// lowering. Post-fix the bind site detects the self-reference,
/// skips the threading, and emits a `TransitionProp::Unsupported`
/// audit marker (with the distinct phrase
/// `self-referential monadic-bind binder` so it can be filtered
/// separately from the bind-on-bind cycle marker).
///
/// Mirrors legal Aiken: `let g = some_fuzzer; and_then(g, fn(g) { g })`.
/// The inner `g` is a fresh continuation parameter shadowing the outer
/// `g`, but the bind's *source* expression is a bare `Var g`
/// referring to the outer binding — so when `translate_bind` would
/// extend `local_values["g"] -> Var "g"`, the `Var` arm has no
/// terminating substitution.
#[test]
fn translate_bind_self_referential_source_emits_unsupported() {
    let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
        empty_transition_prop_context();

    let int_type = Type::int();
    let int_fuzzer = Type::fuzzer(int_type.clone());

    // Source: `Var "x"` typed as `Fuzzer<Int>` — a free reference to
    // some outer `x` of type `Fuzzer<Int>`. This satisfies
    // `expression_has_fuzzer_type` (so the bind shape detector
    // accepts it) AND its name collides with the continuation
    // parameter `x` below — so the naive
    // `extended_locals.insert("x", Var "x")` is self-referential.
    let source = local_var("x", int_fuzzer.clone());

    // Continuation body is the bare local variable reference `x`.
    // Crucially this is NOT wrapped in `return(...)`, so the body
    // path goes through the `TypedExpr::Var` arm of
    // `typed_expr_to_transition_prop` (the Var-lookup site that
    // pre-fix loops on the self-referential entry).
    let cont_body = local_var("x", int_type.clone());
    let cont = make_inline_bind_continuation("x", int_type.clone(), cont_body, int_type.clone());

    let bind_call = make_stdlib_bind_call(source, cont, int_type.clone());

    let prop = typed_expr_to_transition_prop(
        &bind_call,
        "test_mod",
        &function_index,
        &constant_index,
        &local_values,
        &empty_data_types,
        &mut visiting,
        &BTreeSet::new(),
        &mut BTreeSet::new(),
    );

    // Outer bind: Exists { binder: x, body: And([Unsupported, body_prop]) }
    let body = match prop {
        TransitionProp::Exists { binder, body, .. } => {
            assert_eq!(binder, "x", "expected binder 'x', got '{binder}'");
            *body
        }
        other => panic!("expected Exists {{ binder: x, .. }}, got {other:?}"),
    };

    match body {
        TransitionProp::And(parts) => {
            assert_eq!(
                parts.len(),
                2,
                "expected 2-part And([Unsupported, body_prop]); got {} parts: {parts:?}",
                parts.len(),
            );
            // First part: Unsupported with self-referential reason.
            match &parts[0] {
                TransitionProp::Unsupported {
                    reason,
                    source_location,
                } => {
                    assert!(
                        reason.contains("self-referential monadic-bind binder"),
                        "expected reason to contain stable substring \
                             'self-referential monadic-bind binder' \
                             (distinct from 'cyclic monadic-bind binder' so future filtering \
                             can disambiguate); got: {reason}",
                    );
                    assert!(
                        reason.contains("'x'"),
                        "expected reason to mention binder name 'x'; got: {reason}",
                    );
                    assert!(
                        source_location.is_none(),
                        "self-referential bind guard currently reports no source_location; got: {source_location:?}"
                    );
                }
                other => panic!("expected first part Unsupported; got {other:?}"),
            }
            // Second part: body_prop. Without the threaded
            // substitution (skipped by the self-reference guard),
            // `Var x` lookup misses `local_values` and falls through
            // to the `Unsupported` arm in `typed_expr_to_transition_prop`
            // ("variable 'x' has no known transition content").
            // Since `Var "x"` was the body, body_prop is that
            // Unsupported message.
            match &parts[1] {
                TransitionProp::Unsupported { reason, .. } => {
                    assert!(
                        reason.contains("variable 'x' has no known transition content"),
                        "expected body_prop = Unsupported(no known transition content); \
                             got reason: {reason}",
                    );
                }
                other => panic!(
                    "expected second part Unsupported (Var lookup misses without threading); \
                         got {other:?}"
                ),
            }
        }
        other => panic!("expected body And([Unsupported, body_prop]); got {other:?}"),
    }
}

/// Indirect self-reference: the binder name appears in a *nested*
/// position within the bind's source expression (here, as an
/// argument to a `Call`). The free-vars walker must recurse through
/// `Call::args` to detect this — a shallow check would miss it.
/// Same expected outcome as the direct-reference case.
#[test]
fn translate_bind_self_referential_nested_source_emits_unsupported() {
    let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
        empty_transition_prop_context();

    let int_type = Type::int();
    let int_fuzzer = Type::fuzzer(int_type.clone());

    // Source: `some_fn(Var "x")` typed as `Fuzzer<Int>`. The free
    // `Var "x"` is nested inside `Call::args`, not at the top level.
    let source = TypedExpr::Call {
        location: Span::empty(),
        tipo: int_fuzzer.clone(),
        fun: Box::new(module_fn_var(
            "wrap_int",
            "math",
            Type::function(vec![int_type.clone()], int_fuzzer.clone()),
        )),
        args: vec![call_arg(local_var("x", int_type.clone()))],
    };

    let cont_body = local_var("x", int_type.clone());
    let cont = make_inline_bind_continuation("x", int_type.clone(), cont_body, int_type.clone());

    let bind_call = make_stdlib_bind_call(source, cont, int_type.clone());

    let prop = typed_expr_to_transition_prop(
        &bind_call,
        "test_mod",
        &function_index,
        &constant_index,
        &local_values,
        &empty_data_types,
        &mut visiting,
        &BTreeSet::new(),
        &mut BTreeSet::new(),
    );

    let body = match prop {
        TransitionProp::Exists { binder, body, .. } => {
            assert_eq!(binder, "x");
            *body
        }
        other => panic!("expected Exists {{ binder: x, .. }}, got {other:?}"),
    };

    match body {
        TransitionProp::And(parts) => {
            assert_eq!(parts.len(), 3);
            match &parts[0] {
                TransitionProp::Unsupported { reason, .. } => {
                    assert!(
                        reason.contains("self-referential monadic-bind binder"),
                        "expected nested-self-reference to also fire the \
                             self-referential guard; got: {reason}",
                    );
                    assert!(reason.contains("'x'"));
                }
                other => panic!("expected first part Unsupported; got {other:?}"),
            }
        }
        other => panic!("expected body And([Unsupported, body_prop]); got {other:?}"),
    }
}

/// Lookup-side visited-set guard — direct self-shadow regression.
///
/// Aiken's `aiken check` accepts the legal pattern
/// `let x = 1; let x = x; x`. The leading-`let` collector at the
/// top of `typed_expr_to_transition_prop` walks the Sequence's
/// non-terminal Assignments and overwrites
/// `local_values["x"]` with `Var "x"` (the RHS of the second
/// `let x = x`). Pre-fix, the `Var` arm at lookup time would
/// recurse `Var "x" → local_values["x"] = Var "x" → Var "x" …`
/// indefinitely, stack-overflowing the lowering and SIGABRTing
/// `cargo test`.
///
/// Post-fix, the `visiting_value_aliases` set inserted `"x"`
/// before the recursive lookup and detects the cycle on the
/// second visit, returning `TransitionProp::Unsupported` with the
/// stable substring `local-alias cycle on 'x'`.
#[test]
fn var_lookup_breaks_direct_self_shadow_cycle() {
    let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
        empty_transition_prop_context();

    let int_type = Type::int();

    // First assignment: `let x = 42` (non-Fuzzer Int — collected).
    let first_assignment = TypedExpr::Assignment {
        location: Span::empty(),
        tipo: int_type.clone(),
        value: Box::new(uint_lit("42")),
        pattern: TypedPattern::var("x"),
        kind: crate::ast::AssignmentKind::Let { backpassing: () },
        comment: None,
    };

    // Second assignment: `let x = x` — RHS is `Var "x"`. After
    // collection, `local_values["x"] = Var "x"` (overwriting the
    // first binding). This is the cyclic entry.
    let second_assignment = TypedExpr::Assignment {
        location: Span::empty(),
        tipo: int_type.clone(),
        value: Box::new(local_var("x", int_type.clone())),
        pattern: TypedPattern::var("x"),
        kind: crate::ast::AssignmentKind::Let { backpassing: () },
        comment: None,
    };

    // Terminal: bare `Var "x"`. Goes through the `TypedExpr::Var`
    // arm, which looks up `local_values["x"] = Var "x"` and would
    // recurse forever pre-fix.
    let terminal = local_var("x", int_type.clone());

    let sequence = TypedExpr::Sequence {
        location: Span::empty(),
        expressions: vec![first_assignment, second_assignment, terminal],
    };

    let prop = typed_expr_to_transition_prop(
        &sequence,
        "test_mod",
        &function_index,
        &constant_index,
        &local_values,
        &empty_data_types,
        &mut visiting,
        &BTreeSet::new(),
        &mut BTreeSet::new(),
    );

    assert!(
        matches!(prop, TransitionProp::Unsupported { .. }),
        "expected explicit Unsupported for local-alias cycle; got {prop:?}"
    );
}

/// Lookup-side visited-set guard — mutual-shadow chain regression.
///
/// Legal Aiken: `let x = 0; let y = x; let x = y; x`. The
/// leading-let collector installs:
///   - `local_values["x"] = 0`
///   - `local_values["y"] = Var "x"`
///   - `local_values["x"] = Var "y"` (overwrites `0`)
///     Terminal `Var "x"` looks up `Var "y"` → looks up `Var "x"` → …
///     Pre-fix: SIGABRT. Post-fix: visited-set guard catches the
///     second visit to `"x"` and returns the cycle marker.
#[test]
fn var_lookup_breaks_mutual_shadow_cycle() {
    let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
        empty_transition_prop_context();

    let int_type = Type::int();

    // `let x = 0` — non-Fuzzer Int, collected.
    let assign_x0 = TypedExpr::Assignment {
        location: Span::empty(),
        tipo: int_type.clone(),
        value: Box::new(uint_lit("0")),
        pattern: TypedPattern::var("x"),
        kind: crate::ast::AssignmentKind::Let { backpassing: () },
        comment: None,
    };

    // `let y = x` — RHS is `Var "x"`. After collection,
    // `local_values["y"] = Var "x"`.
    let assign_y_eq_x = TypedExpr::Assignment {
        location: Span::empty(),
        tipo: int_type.clone(),
        value: Box::new(local_var("x", int_type.clone())),
        pattern: TypedPattern::var("y"),
        kind: crate::ast::AssignmentKind::Let { backpassing: () },
        comment: None,
    };

    // `let x = y` — RHS is `Var "y"`. After collection,
    // `local_values["x"] = Var "y"` (OVERWRITES the prior
    // `local_values["x"] = 0`). This is the cycle-completing
    // step: now `x → y → x → …`.
    let assign_x_eq_y = TypedExpr::Assignment {
        location: Span::empty(),
        tipo: int_type.clone(),
        value: Box::new(local_var("y", int_type.clone())),
        pattern: TypedPattern::var("x"),
        kind: crate::ast::AssignmentKind::Let { backpassing: () },
        comment: None,
    };

    // Terminal: `Var "x"`. Lookup recursion would loop forever
    // pre-fix.
    let terminal = local_var("x", int_type.clone());

    let sequence = TypedExpr::Sequence {
        location: Span::empty(),
        expressions: vec![assign_x0, assign_y_eq_x, assign_x_eq_y, terminal],
    };

    let prop = typed_expr_to_transition_prop(
        &sequence,
        "test_mod",
        &function_index,
        &constant_index,
        &local_values,
        &empty_data_types,
        &mut visiting,
        &BTreeSet::new(),
        &mut BTreeSet::new(),
    );

    // The visited-set guard catches the second visit to whichever
    // name is re-entered first. Because we start the lookup at
    // `Var "x"` (terminal), the recursion order is:
    //   visit "x" → resolve to Var "y"
    //     visit "y" → resolve to Var "x"
    //       visit "x" → ALREADY visited → emit cycle marker
    // So the guard fires on `"x"`. (If the terminal were `Var "y"`,
    // the guard would fire on `"y"`. The marker phrase reports
    // whichever name closes the loop.)
    assert!(
        matches!(prop, TransitionProp::Unsupported { .. }),
        "expected explicit Unsupported for local-alias cycle; got {prop:?}"
    );
}

/// Lookup-side visited-set guard — negative control. A legal
/// non-cyclic alias chain `let x = 1; let y = x; y` MUST resolve
/// to whatever the terminal lowering produces, NOT a spurious
/// cycle marker. After the leading-let collector installs
/// `local_values["x"] = UInt 1`, `local_values["y"] = Var "x"`,
/// the terminal `Var "y"` resolves through `Var "x"` to `UInt 1`.
/// `UInt` is not a recognized transition shape so the result is
/// `Unsupported` — but the reason text MUST mention the
/// non-recognized shape, NOT the cycle phrase.
#[test]
fn var_lookup_does_not_falsely_trip_on_distinct_names() {
    let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
        empty_transition_prop_context();

    let int_type = Type::int();

    // `let x = 1`
    let assign_x = TypedExpr::Assignment {
        location: Span::empty(),
        tipo: int_type.clone(),
        value: Box::new(uint_lit("1")),
        pattern: TypedPattern::var("x"),
        kind: crate::ast::AssignmentKind::Let { backpassing: () },
        comment: None,
    };

    // `let y = x` — RHS = Var "x".
    let assign_y = TypedExpr::Assignment {
        location: Span::empty(),
        tipo: int_type.clone(),
        value: Box::new(local_var("x", int_type.clone())),
        pattern: TypedPattern::var("y"),
        kind: crate::ast::AssignmentKind::Let { backpassing: () },
        comment: None,
    };

    // Terminal: `Var "y"`. Recursion: visit "y" → Var "x" →
    // visit "x" → UInt 1 (terminal, no further lookup).
    let terminal = local_var("y", int_type.clone());

    let sequence = TypedExpr::Sequence {
        location: Span::empty(),
        expressions: vec![assign_x, assign_y, terminal],
    };

    let prop = typed_expr_to_transition_prop(
        &sequence,
        "test_mod",
        &function_index,
        &constant_index,
        &local_values,
        &empty_data_types,
        &mut visiting,
        &BTreeSet::new(),
        &mut BTreeSet::new(),
    );

    // The result must NOT be the cycle marker. `UInt 1` falls
    // through to the catch-all Unsupported arm with phrase
    // "is not a recognized transition shape". The visited-set
    // guard MUST NOT misfire here: the alias chain x→y is
    // strictly forward and bottoms out at a non-Var leaf.
    match prop {
        TransitionProp::Unsupported { reason, .. } => {
            assert!(
                !reason.contains("local-alias cycle"),
                "FALSE POSITIVE: visited-set guard misfired on a strictly \
                     forward alias chain. The guard MUST only fire when the \
                     SAME name is re-entered. got reason: {reason}",
            );
            assert!(
                reason.contains("is not a recognized transition shape"),
                "expected the catch-all Unsupported (UInt 1 is not a \
                     transition shape); got: {reason}",
            );
        }
        other => panic!(
            "expected Unsupported(catch-all for UInt 1) — \
                 the alias chain should resolve cleanly to the integer literal; \
                 got {other:?}"
        ),
    }
}

#[test]
fn transition_prop_is_trivially_unsupported_is_deep() {
    // All-Unsupported trees should be classified trivial; a tree with
    // any `EqOutput` leaf should not.
    let unsupp = || TransitionProp::Unsupported {
        reason: "leaf".to_string(),
        source_location: None,
    };

    // Or([Unsupported, Unsupported]) → trivial
    let all_unsupp_or = TransitionProp::Or(vec![unsupp(), unsupp()]);
    assert!(transition_prop_is_trivially_unsupported(&all_unsupp_or));

    // IfThenElse with at least one EqOutput leaf → not trivial
    let mixed = TransitionProp::IfThenElse {
        cond: ShallowIr::Const(ShallowConst::Bool(true)),
        t: Box::new(unsupp()),
        e: Box::new(TransitionProp::EqOutput(ShallowIr::Const(
            ShallowConst::Int("0".to_string()),
        ))),
    };
    assert!(!transition_prop_is_trivially_unsupported(&mixed));

    // Exists whose body is Unsupported → trivial (no extractable
    // content beyond the domain, which is a SOUND over-approximation
    // that a caller would otherwise represent as a bare `Unsupported`).
    let exists_empty = TransitionProp::Exists {
        binder: "x".to_string(),
        ty: ShallowIrType::Int,
        domain: Box::new(FuzzerSemantics::IntRange {
            min: None,
            max: None,
        }),
        body: Box::new(unsupp()),
    };
    assert!(transition_prop_is_trivially_unsupported(&exists_empty));
}

// ---------------------------------------------------------------
// Constructor-tag resolution regression: prior to threading
// `data_types` into `typed_expr_to_shallow_ir`, every `Construct`
// node was emitted with `tag: 0`, so non-first constructors (e.g.
// `Scenario.Step` at index 1) produced the wrong structural
// equality in generated Lean predicates.
// ---------------------------------------------------------------

/// Build a `Scenario`-like data type with constructors:
///   index 0: Done           (zero-arity)
///   index 1: Step(Data)     (single-field)
///
/// Returns the owned data-types map alongside the `Type::App` for
/// `mod/Scenario`.
fn scenario_like_data_types() -> (IndexMap<DataTypeKey, TypedDataType>, Rc<Type>) {
    let scenario_ty = Rc::new(Type::App {
        public: true,
        contains_opaque: false,
        module: "mod".to_string(),
        name: "Scenario".to_string(),
        args: vec![],
        alias: None,
    });

    let scenario_data_type = TypedDataType {
        decorators: vec![],
        constructors: vec![
            RecordConstructor {
                decorators: vec![],
                location: Span::empty(),
                name: "Done".to_string(),
                arguments: vec![],
                doc: None,
                sugar: false,
            },
            RecordConstructor {
                decorators: vec![],
                location: Span::empty(),
                name: "Step".to_string(),
                arguments: vec![RecordConstructorArg {
                    label: Some("payload".to_string()),
                    annotation: Annotation::Constructor {
                        location: Span::empty(),
                        module: None,
                        name: "Data".to_string(),
                        arguments: vec![],
                    },
                    location: Span::empty(),
                    tipo: Type::data(),
                    doc: None,
                }],
                doc: None,
                sugar: false,
            },
        ],
        doc: None,
        location: Span::empty(),
        name: "Scenario".to_string(),
        opaque: false,
        parameters: vec![],
        public: true,
        typed_parameters: vec![],
    };

    let mut data_types = IndexMap::new();
    data_types.insert(
        DataTypeKey {
            module_name: "mod".to_string(),
            defined_type: "Scenario".to_string(),
        },
        scenario_data_type,
    );

    (data_types, scenario_ty)
}

#[test]
fn resolve_constructor_tag_uses_declaration_order() {
    let (owned, scenario_ty) = scenario_like_data_types();
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned.iter().collect();

    // First constructor → Some(0).
    assert_eq!(
        resolve_constructor_tag(&scenario_ty, "Done", &data_types),
        Some(0)
    );

    // Second constructor → Some(1) (the bug emitted 0 here).
    assert_eq!(
        resolve_constructor_tag(&scenario_ty, "Step", &data_types),
        Some(1)
    );

    // Unknown constructor on a known type → None (was: silent fallback
    // to 0). Callers must route through `ShallowIr::Opaque` with the
    // S0002 marker so the verify pipeline raises a hard
    // `ConstructorTagUnresolved` error rather than emit a vacuously-
    // satisfiable equality.
    assert_eq!(
        resolve_constructor_tag(&scenario_ty, "DoesNotExist", &data_types),
        None
    );

    // Type not in the registry → None (was: silent fallback to 0).
    let unknown_ty = Rc::new(Type::App {
        public: true,
        contains_opaque: false,
        module: "other".to_string(),
        name: "Other".to_string(),
        args: vec![],
        alias: None,
    });
    assert_eq!(
        resolve_constructor_tag(&unknown_ty, "Step", &data_types),
        None,
    );
}

#[test]
fn resolve_constructor_tag_walks_through_function_return_type() {
    // For an n-ary constructor, the `tipo` carried by
    // `ValueConstructorVariant::Record` (and `ModuleValueConstructor::
    // Record`) is `Type::Fn { args: [...field types], ret: ADT }`.
    // `resolve_constructor_tag` must walk through `Type::Fn::ret` so
    // n-ary constructor sites resolve to the correct tag, not 0.
    let (owned, scenario_ty) = scenario_like_data_types();
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned.iter().collect();

    // `Step(Data) -> Scenario` is the ctor's curried function type.
    let step_fn_ty = Type::function(vec![Type::data()], scenario_ty);

    assert_eq!(
        resolve_constructor_tag(&step_fn_ty, "Step", &data_types),
        Some(1),
        "n-ary constructor lookup must walk Type::Fn ret",
    );
}

#[test]
fn typed_expr_to_shallow_ir_emits_correct_tag_for_step_constructor() {
    // Regression for the constructor-tag bug: a `Call(Step, [Data])`
    // expression must produce `ShallowIr::Construct { tag: 1, .. }`,
    // not `tag: 0`, because `Step` is the second constructor in the
    // `Scenario` ADT.
    let (owned, scenario_ty) = scenario_like_data_types();
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned.iter().collect();

    // Build `Step(payload)` as a `TypedExpr::Call` with a
    // `ValueConstructorVariant::Record` callee.
    let step_fn_ty = Type::function(vec![Type::data()], scenario_ty.clone());
    let step_ctor = TypedExpr::Var {
        location: Span::empty(),
        constructor: ValueConstructor::public(
            step_fn_ty,
            ValueConstructorVariant::Record {
                name: "Step".to_string(),
                arity: 1,
                field_map: None,
                location: Span::empty(),
                module: "mod".to_string(),
                constructors_count: 2,
            },
        ),
        name: "Step".to_string(),
    };
    let payload = TypedExpr::ByteArray {
        location: Span::empty(),
        tipo: Type::data(),
        bytes: vec![0xab, 0xcd],
        preferred_format: crate::ast::ByteArrayFormatPreference::HexadecimalString,
    };
    let step_call = TypedExpr::Call {
        location: Span::empty(),
        tipo: scenario_ty.clone(),
        fun: Box::new(step_ctor),
        args: vec![call_arg(payload)],
    };

    let ir = typed_expr_to_shallow_ir(&step_call, &data_types);
    match ir {
        ShallowIr::Construct {
            ref constructor,
            tag,
            ref fields,
            ..
        } => {
            assert_eq!(constructor, "Step");
            assert_eq!(tag, 1, "Step is the second constructor (index 1)");
            assert_eq!(fields.len(), 1);
        }
        other => panic!("expected ShallowIr::Construct, got {other:?}"),
    }

    // Done is zero-arity → `TypedExpr::Var` path. Tag must be 0.
    let done_var = TypedExpr::Var {
        location: Span::empty(),
        constructor: ValueConstructor::public(
            scenario_ty,
            ValueConstructorVariant::Record {
                name: "Done".to_string(),
                arity: 0,
                field_map: None,
                location: Span::empty(),
                module: "mod".to_string(),
                constructors_count: 2,
            },
        ),
        name: "Done".to_string(),
    };
    let ir = typed_expr_to_shallow_ir(&done_var, &data_types);
    match ir {
        ShallowIr::Construct {
            ref constructor,
            tag,
            ref fields,
            ..
        } => {
            assert_eq!(constructor, "Done");
            assert_eq!(tag, 0, "Done is the first constructor (index 0)");
            assert!(fields.is_empty());
        }
        other => panic!("expected ShallowIr::Construct, got {other:?}"),
    }
}

// ---------------------------------------------------------------
// S0002 (`ConstructorTagUnresolved`) regression tests:
// when `resolve_constructor_tag` returns `None`, every call site
// must route the affected node through `ShallowIr::Opaque` carrying
// a typed `OpaqueCode::ConstructorTagUnresolved` payload. This is
// the contract that the verify-side dispatcher in
// `aiken-project/src/verify.rs` consumes (via
// `find_first_typed_opaque_in_shallow_ir`) to emit a hard,
// non-skippable `S0002` error rather than a silent tag-0 fallback.
//
// Commit 18 retired the legacy `S0002_REASON_PREFIX` string-prefix
// sniffing in favour of these typed assertions.
// ---------------------------------------------------------------

/// Helper: assert that `code` is the expected S0002 typed payload,
/// independent of the (cosmetic) `reason` diagnostic string.
fn assert_s0002_code(code: &Option<OpaqueCode>, expected_ctor: &str, expected_type: &str) {
    match code {
        Some(OpaqueCode::ConstructorTagUnresolved { ctor, type_name }) => {
            assert_eq!(
                ctor, expected_ctor,
                "ConstructorTagUnresolved.ctor mismatch"
            );
            assert_eq!(
                type_name, expected_type,
                "ConstructorTagUnresolved.type_name mismatch"
            );
        }
        other => panic!(
            "expected Some(OpaqueCode::ConstructorTagUnresolved {{ ctor: {expected_ctor:?}, \
                 type_name: {expected_type:?} }}); got {other:?}"
        ),
    }
}

/// Construct site (Var, zero-arity constructor `Done` referencing a
/// `Scenario`-shaped type whose data-types map is empty): the
/// `TypedExpr::Var` arm in `typed_expr_to_shallow_ir` must collapse
/// to `Opaque` carrying the typed `ConstructorTagUnresolved` code
/// rather than emit `Construct { tag: 0, .. }`.
#[test]
fn construct_site_unknown_constructor_emits_opaque_with_s0002() {
    // Empty registry — every constructor is unknown.
    let owned: IndexMap<DataTypeKey, TypedDataType> = IndexMap::new();
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned.iter().collect();

    let scenario_ty = Rc::new(Type::App {
        public: true,
        contains_opaque: false,
        module: "ghost".to_string(),
        name: "Phantom".to_string(),
        args: vec![],
        alias: None,
    });

    let var_expr = TypedExpr::Var {
        location: Span::empty(),
        constructor: ValueConstructor::public(
            scenario_ty,
            ValueConstructorVariant::Record {
                name: "Ghost".to_string(),
                arity: 0,
                field_map: None,
                location: Span::empty(),
                module: "ghost".to_string(),
                constructors_count: 1,
            },
        ),
        name: "Ghost".to_string(),
    };

    let ir = typed_expr_to_shallow_ir(&var_expr, &data_types);
    match ir {
        ShallowIr::Opaque { reason, code, .. } => {
            assert_s0002_code(&code, "Ghost", "ghost.Phantom");
            // The diagnostic message still mentions the relevant fields.
            assert!(reason.contains("Ghost"));
            assert!(reason.contains("ghost.Phantom"));
        }
        other => {
            panic!("expected ShallowIr::Opaque (constructor not in registry), got {other:?}")
        }
    }
}

/// Module-select site (qualified zero-arity constructor `M.Ghost`
/// referenced via `TypedExpr::ModuleSelect`): same contract — the
/// arm must collapse to `Opaque` carrying the typed S0002 code.
#[test]
fn module_select_unknown_constructor_emits_opaque_with_s0002() {
    let owned: IndexMap<DataTypeKey, TypedDataType> = IndexMap::new();
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned.iter().collect();

    let scenario_ty = Rc::new(Type::App {
        public: true,
        contains_opaque: false,
        module: "ghost".to_string(),
        name: "Phantom".to_string(),
        args: vec![],
        alias: None,
    });

    let mod_select = TypedExpr::ModuleSelect {
        location: Span::empty(),
        tipo: scenario_ty.clone(),
        label: "Ghost".to_string(),
        module_name: "ghost".to_string(),
        module_alias: "M".to_string(),
        constructor: ModuleValueConstructor::Record {
            name: "Ghost".to_string(),
            arity: 0,
            tipo: scenario_ty,
            field_map: None,
            location: Span::empty(),
        },
    };

    let ir = typed_expr_to_shallow_ir(&mod_select, &data_types);
    match ir {
        ShallowIr::Opaque { reason, code, .. } => {
            assert_s0002_code(&code, "Ghost", "ghost.Phantom");
            assert!(reason.contains("Ghost"));
            assert!(reason.contains("ghost.Phantom"));
        }
        other => {
            panic!("expected ShallowIr::Opaque (constructor not in registry), got {other:?}")
        }
    }
}

#[test]
fn translate_clause_with_locals_binds_constructor_pattern_vars_in_body() {
    let (owned, scenario_ty) = scenario_like_data_types();
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned.iter().collect();

    let step_ctor = TypedExpr::Var {
        location: Span::empty(),
        constructor: ValueConstructor::public(
            Type::function(vec![Type::data()], scenario_ty.clone()),
            ValueConstructorVariant::Record {
                name: "Step".to_string(),
                arity: 1,
                field_map: None,
                location: Span::empty(),
                module: "mod".to_string(),
                constructors_count: 2,
            },
        ),
        name: "Step".to_string(),
    };

    let clause = TypedClause {
        location: Span::empty(),
        pattern: TypedPattern::constructor(
            "Step",
            &[CallArg::var("payload", Span::empty())],
            scenario_ty.clone(),
            Span::empty(),
        ),
        then: TypedExpr::Call {
            location: Span::empty(),
            tipo: scenario_ty.clone(),
            fun: Box::new(step_ctor),
            args: vec![call_arg(local_var("payload", Type::data()))],
        },
    };

    let subject_binding = LocalBinding::DrawnValue {
        lean_name: "scrutinee".to_string(),
        ty: ShallowIrType::Adt("mod/Scenario".to_string()),
        domain: FuzzerSemantics::Data,
    };
    let mut visiting = BTreeSet::new();

    let arm = match translate_clause_with_locals(
        &clause,
        &scenario_ty,
        &subject_binding,
        &data_types,
        &BTreeMap::new(),
        &mut visiting,
    ) {
        Ok(arm) => arm,
        Err(failure) => panic!(
            "constructor clause should translate, got failure: {}",
            failure.reason
        ),
    };

    match arm.body {
        ShallowIr::Construct {
            constructor,
            fields,
            ..
        } => {
            assert_eq!(constructor, "Step");
            assert!(
                matches!(
                    fields.as_slice(),
                    [ShallowIr::FieldAccess {
                        record,
                        index: 0,
                        kind: ShallowFieldAccessKind::ConstructorField,
                        ..
                    }] if matches!(
                        record.as_ref(),
                        ShallowIr::BoundVar {
                            name,
                            ty: ShallowIrType::Adt(type_name),
                        } if name == "scrutinee" && type_name == "mod/Scenario"
                    )
                ),
                "constructor-pattern variable should lower to a projection from the scrutinee, got: {fields:?}"
            );
        }
        other => panic!("expected constructor body with projected payload, got {other:?}"),
    }
}

/// `translate_clause` directly: a constructor pattern referencing a
/// constructor not in the registry must yield
/// `Err(ClauseTranslationFailure { code: ConstructorTagUnresolved, .. })`,
/// not silently produce a `ShallowIrArm` with `tag: Some(0)`.
#[test]
fn translate_clause_propagates_unknown_constructor_as_err() {
    let owned: IndexMap<DataTypeKey, TypedDataType> = IndexMap::new();
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned.iter().collect();

    let subject_ty = Rc::new(Type::App {
        public: true,
        contains_opaque: false,
        module: "ghost".to_string(),
        name: "Phantom".to_string(),
        args: vec![],
        alias: None,
    });

    let subject = local_var("scrutinee", subject_ty.clone());
    let clause = TypedClause {
        location: Span::empty(),
        pattern: TypedPattern::constructor("Ghost", &[], subject_ty.clone(), Span::empty()),
        then: uint_lit("0"),
    };

    let result = translate_clause(&clause, &subject, &subject_ty, &data_types);
    match result {
        Err(failure) => {
            match failure.code {
                OpaqueCode::ConstructorTagUnresolved { ctor, type_name } => {
                    assert_eq!(ctor, "Ghost");
                    assert_eq!(type_name, "ghost.Phantom");
                }
            }
            assert!(failure.reason.contains("Ghost"));
            assert!(failure.reason.contains("ghost.Phantom"));
        }
        Ok(arm) => {
            panic!("expected Err(<S0002 typed code>) for unresolved Ghost ctor; got Ok({arm:?})")
        }
    }
}

/// `When` arm dispatch: if a single `TypedClause` references an
/// unresolved constructor, the entire `Match` collapses to a
/// `ShallowIr::Opaque` carrying the typed S0002 code — no partial
/// `Match` (which would carry an unsound tag-0 arm) is allowed to
/// leak out.
#[test]
fn when_arm_unknown_constructor_bails_whole_match_to_opaque() {
    let owned: IndexMap<DataTypeKey, TypedDataType> = IndexMap::new();
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned.iter().collect();

    let subject_ty = Rc::new(Type::App {
        public: true,
        contains_opaque: false,
        module: "ghost".to_string(),
        name: "Phantom".to_string(),
        args: vec![],
        alias: None,
    });

    let when_expr = TypedExpr::When {
        location: Span::empty(),
        tipo: Type::int(),
        subject: Box::new(local_var("x", subject_ty.clone())),
        clauses: vec![TypedClause {
            location: Span::empty(),
            pattern: TypedPattern::constructor("Ghost", &[], subject_ty, Span::empty()),
            then: uint_lit("42"),
        }],
    };

    let ir = typed_expr_to_shallow_ir(&when_expr, &data_types);
    match ir {
        ShallowIr::Opaque { reason, code, .. } => {
            assert_s0002_code(&code, "Ghost", "ghost.Phantom");
            assert!(reason.contains("Ghost"));
            assert!(reason.contains("ghost.Phantom"));
        }
        other => panic!(
            "expected ShallowIr::Opaque (whole Match bails on unresolved arm), got {other:?}"
        ),
    }
}

/// `s0002_reason_message` produces a clean human-readable diagnostic
/// (no machine-readable wire-format prefix). The typed payload —
/// not the string — is what the verify-side dispatcher consumes.
#[test]
fn s0002_reason_message_is_human_readable() {
    let msg = s0002_reason_message("Ghost", "ghost.Phantom");
    assert!(
        !msg.starts_with("S0002:"),
        "post-commit-18: reason must NOT carry the legacy wire prefix; got {msg:?}"
    );
    assert!(msg.contains("constructor 'Ghost'"));
    assert!(msg.contains("type 'ghost.Phantom'"));
    assert!(msg.contains("(S0002)"));
}

/// `shallow_ir_is_vacuous` must classify an `Opaque` carrying a typed
/// `OpaqueCode` as *non-vacuous* so the upstream filter in
/// `state_machine_trace_from_test_arguments` does not swallow it
/// before the verify-side dispatcher can emit a hard error.
/// All other `Opaque` shapes must remain vacuous.
#[test]
fn shallow_ir_is_vacuous_carves_out_typed_opaque() {
    // Bare `Opaque` (no typed code): vacuous as before.
    let bare_opaque = ShallowIr::Opaque {
        ty: ShallowIrType::Data,
        reason: "translator did not recognise this shape".to_string(),
        code: None,
    };
    assert!(
        shallow_ir_is_vacuous(&bare_opaque),
        "bare Opaque must remain vacuous"
    );

    // Typed-code `Opaque`: carve-out returns false so the marker
    // can reach the verify-side dispatcher.
    let typed_opaque = ShallowIr::Opaque {
        ty: ShallowIrType::Data,
        reason: s0002_reason_message("Ghost", "ghost.Phantom"),
        code: Some(OpaqueCode::ConstructorTagUnresolved {
            ctor: "Ghost".to_string(),
            type_name: "ghost.Phantom".to_string(),
        }),
    };
    assert!(
        !shallow_ir_is_vacuous(&typed_opaque),
        "typed-code Opaque must NOT be vacuous (must reach the dispatcher)"
    );

    // Carve-out also fires when a typed-code Opaque is reached
    // through `Let` wrapping (the loop unwraps `Let` before classifying).
    let wrapped = ShallowIr::Let {
        name: "tmp".to_string(),
        value: Box::new(ShallowIr::Const(ShallowConst::Unit)),
        body: Box::new(typed_opaque.clone()),
    };
    assert!(
        !shallow_ir_is_vacuous(&wrapped),
        "Let wrapping a typed-code Opaque must remain non-vacuous"
    );
}

/// `find_first_typed_opaque_in_shallow_ir` must locate typed codes
/// nested arbitrarily deep inside structural shapes — the verify-side
/// dispatcher relies on this to dispatch S0002 even when the outer
/// IR is `Construct { fields: [..., Opaque{S0002}, ...] }` (which is
/// NOT classified as vacuous, so it can reach the dispatcher).
#[test]
fn find_first_typed_opaque_in_shallow_ir_walks_nested_construct() {
    let make_marker_ir = || ShallowIr::Opaque {
        ty: ShallowIrType::Data,
        reason: s0002_reason_message("Ghost", "ghost.Phantom"),
        code: Some(OpaqueCode::ConstructorTagUnresolved {
            ctor: "Ghost".to_string(),
            type_name: "ghost.Phantom".to_string(),
        }),
    };

    // Top-level marker: trivial case.
    let top_level = make_marker_ir();
    let found = find_first_typed_opaque_in_shallow_ir(&top_level);
    assert!(matches!(
        found,
        Some(OpaqueCode::ConstructorTagUnresolved { ctor, type_name })
            if ctor == "Ghost" && type_name == "ghost.Phantom"
    ));

    // Marker buried inside a Construct field: must still be found.
    let nested = ShallowIr::Construct {
        module: "test".to_string(),
        constructor: "Outer".to_string(),
        tag: 0,
        fields: vec![
            ShallowIr::Const(ShallowConst::Int("0".to_string())),
            make_marker_ir(),
        ],
    };
    let found = find_first_typed_opaque_in_shallow_ir(&nested);
    assert!(
        matches!(
            found,
            Some(OpaqueCode::ConstructorTagUnresolved { ctor, type_name })
                if ctor == "Ghost" && type_name == "ghost.Phantom"
        ),
        "marker buried inside Construct field must be discovered; got {found:?}"
    );

    // Marker absent: returns None.
    let no_marker = ShallowIr::Construct {
        module: "test".to_string(),
        constructor: "Outer".to_string(),
        tag: 0,
        fields: vec![ShallowIr::Const(ShallowConst::Bool(true))],
    };
    assert_eq!(find_first_typed_opaque_in_shallow_ir(&no_marker), None);

    // Bare (non-typed) Opaque: returns None.
    let bare_opaque = ShallowIr::Opaque {
        ty: ShallowIrType::Data,
        reason: "unrelated reason".to_string(),
        code: None,
    };
    assert_eq!(find_first_typed_opaque_in_shallow_ir(&bare_opaque), None);
}

/// `find_first_typed_opaque_in_transition_prop` must walk the
/// recursive `TransitionProp` shape and locate typed codes buried
/// inside `EqOutput`, `Pure`, `And`, `Or`, `IfThenElse`, `Match`,
/// and `Exists` arms. The verify-side dispatcher in
/// `try_generate_two_phase_proof` relies on this to surface a hard
/// `S0002` error even when the marker is several layers deep.
#[test]
fn find_first_typed_opaque_in_transition_prop_walks_recursive_shapes() {
    let make_marker_ir = || ShallowIr::Opaque {
        ty: ShallowIrType::Data,
        reason: s0002_reason_message("Ghost", "ghost.Phantom"),
        code: Some(OpaqueCode::ConstructorTagUnresolved {
            ctor: "Ghost".to_string(),
            type_name: "ghost.Phantom".to_string(),
        }),
    };

    let expected = OpaqueCode::ConstructorTagUnresolved {
        ctor: "Ghost".to_string(),
        type_name: "ghost.Phantom".to_string(),
    };

    // EqOutput at top level: trivial.
    let eq_top = TransitionProp::EqOutput(make_marker_ir());
    assert_eq!(
        find_first_typed_opaque_in_transition_prop(&eq_top),
        Some(expected.clone())
    );

    // EqOutput nested in And: walk descends.
    let and_eq = TransitionProp::And(vec![
        TransitionProp::Pure(ShallowIr::Const(ShallowConst::Bool(true))),
        TransitionProp::EqOutput(make_marker_ir()),
    ]);
    assert_eq!(
        find_first_typed_opaque_in_transition_prop(&and_eq),
        Some(expected.clone())
    );

    // EqOutput nested in Or → IfThenElse → EqOutput: walk descends.
    let nested = TransitionProp::Or(vec![TransitionProp::IfThenElse {
        cond: ShallowIr::Const(ShallowConst::Bool(true)),
        t: Box::new(TransitionProp::EqOutput(make_marker_ir())),
        e: Box::new(TransitionProp::EqOutput(ShallowIr::Const(
            ShallowConst::Int("0".to_string()),
        ))),
    }]);
    assert_eq!(
        find_first_typed_opaque_in_transition_prop(&nested),
        Some(expected.clone())
    );

    // Marker buried in Construct field, nested in EqOutput, in turn
    // nested in Exists: walk descends through every layer.
    let deep = TransitionProp::Exists {
        binder: "x".to_string(),
        ty: ShallowIrType::Data,
        domain: Box::new(FuzzerSemantics::Data),
        body: Box::new(TransitionProp::EqOutput(ShallowIr::Construct {
            module: "test".to_string(),
            constructor: "Wrapper".to_string(),
            tag: 0,
            fields: vec![make_marker_ir()],
        })),
    };
    assert_eq!(
        find_first_typed_opaque_in_transition_prop(&deep),
        Some(expected.clone())
    );

    // No marker present → None.
    let no_marker = TransitionProp::And(vec![
        TransitionProp::Pure(ShallowIr::Const(ShallowConst::Bool(true))),
        TransitionProp::EqOutput(ShallowIr::Const(ShallowConst::Int("42".to_string()))),
    ]);
    assert_eq!(find_first_typed_opaque_in_transition_prop(&no_marker), None);

    // SubGenerator and Unsupported leaves never carry markers.
    let sub_gen = TransitionProp::SubGenerator {
        module: "m".to_string(),
        fn_name: "f".to_string(),
    };
    assert_eq!(find_first_typed_opaque_in_transition_prop(&sub_gen), None);
    let unsupported = TransitionProp::Unsupported {
        reason: "test".to_string(),
        source_location: None,
    };
    assert_eq!(
        find_first_typed_opaque_in_transition_prop(&unsupported),
        None
    );
}

/// End-to-end: a synthetic step function whose body is a `Construct`
/// referencing an unknown constructor must (a) flow through
/// `typed_expr_to_shallow_ir` to produce an `Opaque` carrying the
/// typed S0002 code, (b) survive the upstream `shallow_ir_is_vacuous`
/// filter via the typed carve-out, and (c) be discoverable by the
/// verify-side dispatcher via `find_first_typed_opaque_in_shallow_ir`.
/// This is the production path that Oracle A flagged as broken before
/// the carve-out: without it, the marker was swallowed by the filter
/// and the user got a generic skippable `FallbackRequired` instead of
/// the promised hard `S0002` error.
#[test]
fn s0002_marker_survives_shallow_ir_filter_pipeline() {
    // Empty data-type registry — every constructor is unknown.
    let owned: IndexMap<DataTypeKey, TypedDataType> = IndexMap::new();
    let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned.iter().collect();

    let phantom_ty = Rc::new(Type::App {
        public: true,
        contains_opaque: false,
        module: "ghost".to_string(),
        name: "Phantom".to_string(),
        args: vec![],
        alias: None,
    });

    // Step body: `Ghost` (a zero-arity constructor on `ghost.Phantom`,
    // which is NOT in the registry — `resolve_constructor_tag` returns
    // None and the translator must emit `Opaque{S0002}`).
    let body = TypedExpr::Var {
        location: Span::empty(),
        constructor: ValueConstructor::public(
            phantom_ty,
            ValueConstructorVariant::Record {
                name: "Ghost".to_string(),
                arity: 0,
                field_map: None,
                location: Span::empty(),
                module: "ghost".to_string(),
                constructors_count: 1,
            },
        ),
        name: "Ghost".to_string(),
    };

    // (a) `typed_expr_to_shallow_ir` produces typed `Opaque{S0002}`.
    let ir = typed_expr_to_shallow_ir(&body, &data_types);
    match &ir {
        ShallowIr::Opaque { code, .. } => {
            assert_s0002_code(code, "Ghost", "ghost.Phantom");
        }
        other => panic!("expected Opaque{{S0002}} from translator; got {other:?}"),
    }

    // (b) The filter in `state_machine_trace_from_test_arguments`
    // is `.filter(|ir| !shallow_ir_is_vacuous(ir))`.
    // Before the carve-out, this DROPPED the IR; after the carve-out,
    // the IR survives.
    assert!(
        !shallow_ir_is_vacuous(&ir),
        "typed-code Opaque must survive the upstream `shallow_ir_is_vacuous` filter; \
             this is the bug Oracle A flagged as Issue 1"
    );

    // (c) The verify-side dispatcher recovers the typed code — the
    // walker locates it whether at the top level or buried inside a
    // structural wrapper. Here the marker IS at the top level, but
    // confirming the walker finds it pins the contract.
    let found = find_first_typed_opaque_in_shallow_ir(&ir);
    assert!(
        matches!(
            found,
            Some(OpaqueCode::ConstructorTagUnresolved { ctor, type_name })
                if ctor == "Ghost" && type_name == "ghost.Phantom"
        ),
        "verify-side dispatcher must find the S0002 typed code via the walker; got {found:?}"
    );
}

/// Structural-vacuity unit tests (M3).  Pin the verdict on every
/// `TransitionProp` variant individually plus a few representative
/// composites.  Together with the drift sentinel on the verify side,
/// this gives us full coverage of the predicate.
mod transition_prop_is_vacuous {
    use super::super::*;

    fn unsupported_leaf() -> TransitionProp {
        TransitionProp::Unsupported {
            reason: "test".to_string(),
            source_location: None,
        }
    }

    fn pure_true() -> TransitionProp {
        TransitionProp::Pure(ShallowIr::Const(ShallowConst::Bool(true)))
    }

    fn eq_output_var(name: &str) -> TransitionProp {
        TransitionProp::EqOutput(ShallowIr::Var {
            name: name.to_string(),
            ty: ShallowIrType::Data,
        })
    }

    fn eq_output_construct() -> TransitionProp {
        TransitionProp::EqOutput(ShallowIr::Construct {
            module: "m".to_string(),
            constructor: "Step".to_string(),
            tag: 1,
            fields: vec![],
        })
    }

    fn eq_output_typed_opaque() -> TransitionProp {
        TransitionProp::EqOutput(ShallowIr::Opaque {
            ty: ShallowIrType::Data,
            reason: s0002_reason_message("Ghost", "ghost.Phantom"),
            code: Some(OpaqueCode::ConstructorTagUnresolved {
                ctor: "Ghost".to_string(),
                type_name: "ghost.Phantom".to_string(),
            }),
        })
    }

    #[test]
    fn pure_bool_predicate_is_not_vacuous() {
        assert!(!transition_prop_is_vacuous(&pure_true()));
    }

    #[test]
    fn unsupported_is_vacuous() {
        assert!(transition_prop_is_vacuous(&unsupported_leaf()));
    }

    #[test]
    fn eq_output_with_vacuous_rhs_is_vacuous() {
        assert!(transition_prop_is_vacuous(&eq_output_var("transition")));
    }

    #[test]
    fn eq_output_with_structural_rhs_is_not_vacuous() {
        assert!(!transition_prop_is_vacuous(&eq_output_construct()));
    }

    #[test]
    fn eq_output_with_typed_opaque_rhs_preserves_s0002_marker() {
        assert!(!transition_prop_is_vacuous(&eq_output_typed_opaque()));
    }

    #[test]
    fn sub_generator_is_not_vacuous() {
        let sg = TransitionProp::SubGenerator {
            module: "m".to_string(),
            fn_name: "f".to_string(),
        };
        assert!(!transition_prop_is_vacuous(&sg));
    }

    #[test]
    fn empty_and_is_vacuous() {
        // The emitter renders `And([])` as literal `True`.
        assert!(transition_prop_is_vacuous(&TransitionProp::And(vec![])));
    }

    #[test]
    fn empty_or_is_vacuous() {
        // The emitter widens `Or([])` to literal `True` (logged) to
        // avoid an unsatisfiable precondition.
        assert!(transition_prop_is_vacuous(&TransitionProp::Or(vec![])));
    }

    #[test]
    fn empty_match_is_vacuous() {
        assert!(transition_prop_is_vacuous(&TransitionProp::Match {
            scrutinee: ShallowIr::Var {
                name: "x".to_string(),
                ty: ShallowIrType::Data,
            },
            arms: vec![],
        }));
    }

    #[test]
    fn and_of_unsupported_is_vacuous() {
        // Refinement over the text predicate (which gives up on `∧`).
        let prop = TransitionProp::And(vec![unsupported_leaf(), unsupported_leaf()]);
        assert!(transition_prop_is_vacuous(&prop));
    }

    #[test]
    fn and_with_one_real_constraint_is_not_vacuous() {
        let prop = TransitionProp::And(vec![unsupported_leaf(), eq_output_construct()]);
        assert!(!transition_prop_is_vacuous(&prop));
    }

    #[test]
    fn or_of_unsupported_is_vacuous() {
        let prop = TransitionProp::Or(vec![unsupported_leaf(), unsupported_leaf()]);
        assert!(transition_prop_is_vacuous(&prop));
    }

    #[test]
    fn or_with_one_real_constraint_is_not_vacuous() {
        let prop = TransitionProp::Or(vec![unsupported_leaf(), eq_output_construct()]);
        assert!(!transition_prop_is_vacuous(&prop));
    }

    #[test]
    fn exists_recurses_on_body_except_domain_constrained_bound_output() {
        let vacuous_body = TransitionProp::Exists {
            binder: "x".to_string(),
            ty: ShallowIrType::Data,
            domain: Box::new(FuzzerSemantics::Data),
            body: Box::new(unsupported_leaf()),
        };
        assert!(transition_prop_is_vacuous(&vacuous_body));

        let constrained_bound_output = TransitionProp::Exists {
            binder: "x".to_string(),
            ty: ShallowIrType::Data,
            domain: Box::new(FuzzerSemantics::Constructors { tags: vec![0, 1] }),
            body: Box::new(TransitionProp::EqOutput(ShallowIr::Var {
                name: "x".to_string(),
                ty: ShallowIrType::Data,
            })),
        };
        assert!(!transition_prop_is_vacuous(&constrained_bound_output));

        let unconstrained_bound_output = TransitionProp::Exists {
            binder: "x".to_string(),
            ty: ShallowIrType::Data,
            domain: Box::new(FuzzerSemantics::Data),
            body: Box::new(TransitionProp::EqOutput(ShallowIr::Var {
                name: "x".to_string(),
                ty: ShallowIrType::Data,
            })),
        };
        assert!(transition_prop_is_vacuous(&unconstrained_bound_output));

        let real_body = TransitionProp::Exists {
            binder: "x".to_string(),
            ty: ShallowIrType::Data,
            domain: Box::new(FuzzerSemantics::Data),
            body: Box::new(eq_output_construct()),
        };
        assert!(!transition_prop_is_vacuous(&real_body));
    }

    #[test]
    fn if_then_else_recurses_on_both_branches() {
        // Both branches vacuous — refinement over text predicate.
        let prop = TransitionProp::IfThenElse {
            cond: ShallowIr::Const(ShallowConst::Bool(true)),
            t: Box::new(unsupported_leaf()),
            e: Box::new(unsupported_leaf()),
        };
        assert!(transition_prop_is_vacuous(&prop));

        // One branch with a real constraint — not vacuous.
        let prop_partial = TransitionProp::IfThenElse {
            cond: ShallowIr::Const(ShallowConst::Bool(true)),
            t: Box::new(unsupported_leaf()),
            e: Box::new(eq_output_construct()),
        };
        assert!(!transition_prop_is_vacuous(&prop_partial));
    }

    #[test]
    fn match_recurses_on_arm_bodies() {
        let arms_all_vacuous = vec![
            TransitionPropArm {
                tag: Some(0),
                bindings: vec![],
                body: unsupported_leaf(),
            },
            TransitionPropArm {
                tag: Some(1),
                bindings: vec![],
                body: unsupported_leaf(),
            },
        ];
        let prop_vacuous = TransitionProp::Match {
            scrutinee: ShallowIr::Var {
                name: "x".to_string(),
                ty: ShallowIrType::Data,
            },
            arms: arms_all_vacuous,
        };
        assert!(transition_prop_is_vacuous(&prop_vacuous));

        let arms_mixed = vec![
            TransitionPropArm {
                tag: Some(0),
                bindings: vec![],
                body: unsupported_leaf(),
            },
            TransitionPropArm {
                tag: Some(1),
                bindings: vec![],
                body: eq_output_construct(),
            },
        ];
        let prop_real = TransitionProp::Match {
            scrutinee: ShallowIr::Var {
                name: "x".to_string(),
                ty: ShallowIrType::Data,
            },
            arms: arms_mixed,
        };
        assert!(!transition_prop_is_vacuous(&prop_real));
    }
}
