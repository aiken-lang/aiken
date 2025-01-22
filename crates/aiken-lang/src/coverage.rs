use crate::line_numbers::LineNumbers;
use std::collections::{HashMap, HashSet};

pub struct CoverageData {
    pub module_name: String,
    pub total_lines: usize,
    pub covered_lines: HashSet<usize>,
}

pub struct CoverageCollector {
    coverage_map: HashMap<String, CoverageData>,
    line_numbers: HashMap<String, LineNumbers>,
    // Store all potential traces from UPLC generation
    potential_traces: HashMap<String, Vec<(usize, usize)>>,
}

impl CoverageCollector {
    pub fn new(module_sources: HashMap<String, (String, LineNumbers)>) -> Self {
        let line_numbers = module_sources
            .into_iter()
            .map(|(k, (_, v))| (k, v))
            .collect();

        Self {
            coverage_map: HashMap::new(),
            line_numbers,
            potential_traces: HashMap::new(),
        }
    }

    pub fn record_trace(&mut self, trace: &str) {
        if let Some(coverage_data) = trace.strip_prefix("COVERAGE:") {
            dbg!("Coverage trace: {}", trace);
            let parts: Vec<&str> = coverage_data.split(':').collect();
            if parts.len() == 4 {
                let (module, start, end, _context) = (
                    parts[0],
                    parts[1].parse().unwrap(),
                    parts[2].parse().unwrap(),
                    parts[3],
                );
                self.record_coverage(module, start, end);
            }
        }
    }

    fn record_coverage(&mut self, module: &str, start: usize, end: usize) {
        let entry = self
            .coverage_map
            .entry(module.to_string())
            .or_insert_with(|| {
                // Find the total number of lines by searching for the last line number
                let total_lines = self
                    .line_numbers
                    .get(module)
                    .map(|ln| {
                        // Keep trying line numbers until we get None
                        let mut i = 1;
                        while ln.line_and_column_number(i).is_some() {
                            i += 1;
                        }
                        i
                    })
                    .unwrap_or(0);

                CoverageData {
                    module_name: module.to_string(),
                    total_lines,
                    covered_lines: HashSet::new(),
                }
            });

        entry.covered_lines.insert(start);
        entry.covered_lines.insert(end);

        if let Some(line_numbers) = self.line_numbers.get(module) {
            let start_line = line_numbers.line_number(start);
            let end_line = line_numbers.line_number(end);

            if let (Some(start), Some(end)) = (start_line, end_line) {
                for line in start..=end {
                    entry.covered_lines.insert(line);
                }
            }
        }
    }

    pub fn generate_report(&self) -> HashMap<String, serde_json::Value> {
        let mut reports = HashMap::new();

        // Process each module that has any traces (either total or executed)
        let all_modules: HashSet<_> = self
            .coverage_map
            .keys()
            .chain(self.potential_traces.keys())
            .collect();

        for module_name in all_modules {
            let coverage_data = self.coverage_map.get(module_name);
            // let total_module_traces = self.potential_traces.get(module_name);

            let total_lines = coverage_data.map(|d| d.total_lines).unwrap_or(0);

            let covered_lines = coverage_data
                .map(|d| d.covered_lines.clone())
                .unwrap_or_default();

            // Calculate coverage percentage and uncovered lines
            let coverage_percentage = if total_lines > 0 {
                (covered_lines.len() as f64 / total_lines as f64) * 100.0
            } else {
                0.0
            };

            let uncovered_lines: Vec<usize> = if total_lines > 0 {
                (1..=total_lines)
                    .filter(|line| !covered_lines.contains(line))
                    .collect()
            } else {
                Vec::new()
            };

            // Create the report for this module
            let report = serde_json::json!({
                "module": module_name,
                "total_lines": total_lines,
                "covered_lines": covered_lines.iter().collect::<Vec<_>>(),
                "uncovered_lines": uncovered_lines,
                "coverage_percentage": coverage_percentage,
                // "total_traces": total_module_traces.unwrap_or(&Vec::new()),
            });

            reports.insert(module_name.clone(), report);
        }

        reports
    }

    // Record a potential trace during UPLC generation
    pub fn record_potential_trace(&mut self, module: &str, start: usize, end: usize) {
        self.potential_traces
            .entry(module.to_string())
            .or_default()
            .push((start, end));
    }
}
