pub struct Options {
    pub code_gen_mode: CodeGenMode,
}

pub enum CodeGenMode {
    Test {
        match_tests: Option<String>,
        verbose: bool,
    },
    Build(bool),
    NoOp,
}
