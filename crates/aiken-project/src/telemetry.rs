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
    GeneratingDocFiles {
        output_path: PathBuf,
    },
    GeneratingBlueprint {
        path: PathBuf,
    },
    DumpingUPLC {
        path: PathBuf,
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
    WaitingForBuildDirLock,
    DownloadingPackage {
        name: String,
    },
    PackagesDownloaded {
        start: tokio::time::Instant,
        count: usize,
    },
    ResolvingVersions,
}
