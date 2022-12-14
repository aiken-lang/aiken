use crate::script::EvalInfo;
use std::path::PathBuf;

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
