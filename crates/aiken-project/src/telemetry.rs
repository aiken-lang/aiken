use aiken_lang::{
    expr::UntypedExpr,
    test_framework::{BenchmarkResult, PropertyTestResult, TestResult, UnitTestResult},
};
pub use json::{Json, json_schema};
use std::{
    collections::BTreeMap,
    fmt::Display,
    io::{self, IsTerminal},
    path::PathBuf,
};
pub use terminal::Terminal;
use uplc::machine::cost_model::ExBudget;

mod json;
mod terminal;

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
    CollectingTests {
        matching_module: Option<String>,
        matching_names: Vec<String>,
    },
    RunningTests,
    RunningBenchmarks,
    FinishedTests {
        seed: u32,
        tests: Vec<TestResult<UntypedExpr, UntypedExpr>>,
    },
    FinishedBenchmarks {
        seed: u32,
        benchmarks: Vec<TestResult<UntypedExpr, UntypedExpr>>,
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

pub enum EventTarget {
    Json(Json),
    Terminal(Terminal),
}

impl Default for EventTarget {
    fn default() -> Self {
        if io::stdout().is_terminal() {
            EventTarget::Terminal(Terminal)
        } else {
            EventTarget::Json(Json)
        }
    }
}

impl EventListener for EventTarget {
    fn handle_event(&self, event: Event) {
        match self {
            EventTarget::Terminal(term) => term.handle_event(event),
            EventTarget::Json(json) => json.handle_event(event),
        }
    }
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

pub(crate) fn group_by_module(
    results: &[TestResult<UntypedExpr, UntypedExpr>],
) -> BTreeMap<String, Vec<&TestResult<UntypedExpr, UntypedExpr>>> {
    let mut modules = BTreeMap::new();
    for r in results {
        let xs: &mut Vec<&TestResult<_, _>> = modules.entry(r.module().to_string()).or_default();
        xs.push(r);
    }
    modules
}

pub(crate) fn find_max_execution_units<T>(xs: &[TestResult<T, T>]) -> (usize, usize, usize) {
    fn max_execution_units(max_mem: i64, max_cpu: i64, cost: &ExBudget) -> (i64, i64) {
        if cost.mem >= max_mem && cost.cpu >= max_cpu {
            (cost.mem, cost.cpu)
        } else if cost.mem > max_mem {
            (cost.mem, max_cpu)
        } else if cost.cpu > max_cpu {
            (max_mem, cost.cpu)
        } else {
            (max_mem, max_cpu)
        }
    }

    let (max_mem, max_cpu, max_iter) =
        xs.iter()
            .fold((0, 0, 0), |(max_mem, max_cpu, max_iter), test| match test {
                TestResult::PropertyTestResult(PropertyTestResult { iterations, .. }) => {
                    (max_mem, max_cpu, std::cmp::max(max_iter, *iterations))
                }
                TestResult::UnitTestResult(UnitTestResult { spent_budget, .. }) => {
                    let (max_mem, max_cpu) = max_execution_units(max_mem, max_cpu, spent_budget);
                    (max_mem, max_cpu, max_iter)
                }
                TestResult::BenchmarkResult(BenchmarkResult { measures, .. }) => {
                    let (mut max_mem, mut max_cpu) = (max_mem, max_cpu);
                    for (_, measure) in measures {
                        (max_mem, max_cpu) = max_execution_units(max_mem, max_cpu, measure);
                    }
                    (max_mem, max_cpu, max_iter)
                }
            });

    (
        max_mem.to_string().len(),
        max_cpu.to_string().len(),
        max_iter.to_string().len(),
    )
}
