use aiken_lang::ast::Tracing;
use std::path::PathBuf;

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
    NoOp,
}
