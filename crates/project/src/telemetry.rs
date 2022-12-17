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
    BuildingDocumentation {
        name: String,
        version: String,
        root: PathBuf,
    },
    ParsingProjectFiles,
    TypeChecking,
    GeneratingDocFiles {
        output_path: PathBuf,
    },
    GeneratingUPLC {
        output_path: PathBuf,
    },
    GeneratingUPLCFor {
        name: String,
        path: PathBuf,
    },
    EvaluatingFunction {
        results: Vec<EvalInfo>,
    },
    RunningTests,
    FinishedTests {
        tests: Vec<EvalInfo>,
    },
}
