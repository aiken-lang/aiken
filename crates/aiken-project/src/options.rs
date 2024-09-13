use std::collections::HashMap;

use aiken_lang::ast::Tracing;

pub struct Options<'a> {
    pub code_gen_mode: CodeGenMode,
    pub tracing: Tracing,
    pub env: Option<String>,
    pub edited: Option<&'a HashMap<String, String>>,
}

impl<'a> Default for Options<'a> {
    fn default() -> Self {
        Self {
            code_gen_mode: CodeGenMode::NoOp,
            tracing: Tracing::silent(),
            env: None,
            edited: None,
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
