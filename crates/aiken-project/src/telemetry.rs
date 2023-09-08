use crate::script::EvalInfo;
use std::{fmt::Display, path::PathBuf};

pub trait EventListener {
    fn handle_event(&self, _event: Event) {}
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
    ResolvingPackages {
        name: String,
    },
    PackageResolveFallback {
        name: String,
    },
    PackagesDownloaded {
        start: tokio::time::Instant,
        count: usize,
        source: DownloadSource,
    },
    ResolvingVersions,
}

pub enum DownloadSource {
    Network,
    Cache,
}

impl Display for DownloadSource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DownloadSource::Network => write!(f, "network"),
            DownloadSource::Cache => write!(f, "cache"),
        }
    }
}
