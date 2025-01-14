use std::path::PathBuf;

use aiken_lang::ast::Tracing;

pub struct Options {
    pub code_gen_mode: CodeGenMode,
    pub tracing: Tracing,
    pub env: Option<String>,
    pub blueprint_path: PathBuf,
}

impl Default for Options {
    fn default() -> Self {
        Self {
            code_gen_mode: CodeGenMode::NoOp,
            tracing: Tracing::silent(),
            env: None,
            blueprint_path: PathBuf::from("plutus.json"),
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
    Benchmark {
        match_tests: Option<Vec<String>>,
        exact_match: bool,
        seed: u32,
        times_to_run: usize,
    },
    NoOp,
}
