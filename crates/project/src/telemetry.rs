use crate::script::Script;
use std::path::PathBuf;
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
    RunningTests,
    FinishedTests {
        tests: Vec<TestInfo>,
    },
}

pub struct TestInfo {
    pub is_passing: bool,
    pub test: Script,
    pub spent_budget: ExBudget,
}
