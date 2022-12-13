use crate::script::Script;
use std::path::PathBuf;
use uplc::ast::{NamedDeBruijn, Term};
use uplc::machine::cost_model::ExBudget;

pub trait EventListener: std::fmt::Debug {
    fn handle_event(&self, event: Event);
}

pub enum Event {
    StartingCompilation {
        name: String,
        version: String,
        root: PathBuf,
    },
    ParsingProjectFiles,
    TypeChecking,
    GeneratingUPLC {
        output_path: PathBuf,
    },
    EvaluatingFunction {
        results: Vec<EvalInfo>,
    },
    RunningTests,
    FinishedTests {
        tests: Vec<EvalInfo>,
    },
}

pub struct EvalInfo {
    pub success: bool,
    pub script: Script,
    pub spent_budget: ExBudget,
    pub output: Option<Term<NamedDeBruijn>>,
}
