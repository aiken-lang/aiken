use super::cost_model::{CostingFun, ExBudget};

enum RuntimeScheme {
    RuntimeSchemeResult,
    RuntimeSchemeArrow(usize),
    RuntimeSchemeAll(usize),
}

#[derive(std::cmp::PartialEq)]
enum EvalMode {
    Immediate,
    Deferred,
}

struct BuiltinRuntimeOptions<T, G> {
    runtime_scheme: Vec<RuntimeScheme>,
    immediate_eval: T,
    deferred_eval: T,
    budget: fn(CostingFun<G>) -> fn(Vec<u32>) -> ExBudget,
}

struct BuiltinRuntime<T> {
    runtime_scheme: Vec<RuntimeScheme>,
    runtime_denotation: T,
    budget: fn(Vec<u32>) -> ExBudget,
}

impl<T> BuiltinRuntime<T> {
    fn from_builtin_runtime_options<G>(
        eval_mode: EvalMode,
        cost: CostingFun<G>,
        runtime_options: BuiltinRuntimeOptions<T, G>,
    ) -> BuiltinRuntime<T> {
        BuiltinRuntime {
            runtime_scheme: runtime_options.runtime_scheme,
            runtime_denotation: if eval_mode == EvalMode::Immediate {
                runtime_options.immediate_eval
            } else {
                runtime_options.deferred_eval
            },
            budget: (runtime_options.budget)(cost),
        }
    }
}
