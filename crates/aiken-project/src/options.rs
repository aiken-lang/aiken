use aiken_lang::ast::Tracing;

pub struct Options {
    pub code_gen_mode: CodeGenMode,
    pub tracing: Tracing,
    pub env: Option<String>,
    pub json: bool,
}

impl Default for Options {
    fn default() -> Self {
        Self {
            code_gen_mode: CodeGenMode::NoOp,
            tracing: Tracing::silent(),
            env: None,
            json: false,
        }
    }
}

pub enum CodeGenMode {
    Test {
        match_tests: Option<Vec<String>>,
        verbose: bool,
        exact_match: bool,
        seed: u32,
        property_max_success: usize,
    },
    Build(bool),
    NoOp,
}
