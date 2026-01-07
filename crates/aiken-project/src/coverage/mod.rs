//! Code coverage support for Aiken projects.
//!
//! This module provides functionality to run tests while tracking code coverage,
//! and generate coverage reports in standard formats like LCOV.

mod lcov;
mod tracking;

pub use lcov::{LcovReport, ModuleInfo};
pub use tracking::{
    CoverageData, CoverageResult, collect_all_locations, collect_all_program_locations,
    run_program_with_coverage, run_with_coverage,
};
