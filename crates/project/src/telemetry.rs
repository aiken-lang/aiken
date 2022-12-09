use uplc::machine::cost_model::ExBudget;

use crate::script::Script;

pub trait EventListener: std::fmt::Debug {
    fn handle_event(&self, event: Event);
}

pub enum Event {
    CompilingPackage { name: String },
    RunningTests,
    FinishedTests { tests: Vec<TestInfo> },
}

pub struct TestInfo {
    pub is_passing: bool,
    pub test: Script,
    pub spent_budget: ExBudget,
}
