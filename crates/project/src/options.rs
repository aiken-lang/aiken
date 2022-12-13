pub struct Options {
    pub code_gen_mode: CodeGenMode,
}

pub enum CodeGenMode {
    Test(Option<String>),
    Eval(String),
    Build(bool),
    NoOp,
}
