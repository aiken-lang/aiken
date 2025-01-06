use std::{collections::{HashMap, HashSet}, path::Path};
use crate::line_numbers::LineNumbers;

pub struct CoverageData {
    pub module_name: String,
    pub covered_spans: Vec<(usize, usize)>,
    pub total_lines: usize,
    pub covered_lines: HashSet<usize>,
}

pub struct CoverageCollector {
    coverage_map: HashMap<String, CoverageData>,
    line_numbers: HashMap<String, LineNumbers>,
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
                    parts[3]
                );
                self.record_coverage(module, start, end);
            }
        }
    }

    fn record_coverage(&mut self, module: &str, start: usize, end: usize) {
        let entry = self.coverage_map
            .entry(module.to_string())
            .or_insert_with(|| {
                // Find the total number of lines by searching for the last line number
                let total_lines = self.line_numbers
                    .get(module)
                    .and_then(|ln| {
                        // Keep trying line numbers until we get None
                        let mut i = 1;
                        while ln.line_and_column_number(i).is_some() {
                            i += 1;
                        }
                        Some(i)
                    })
                    .unwrap_or(0);
    
                CoverageData {
                    module_name: module.to_string(),
                    covered_spans: Vec::new(),
                    total_lines,
                    covered_lines: HashSet::new(),
                }
            });
    
        entry.covered_spans.push((start, end));
        
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

    pub fn output_report(&self, root: &Path) -> Result<(), std::io::Error> {
        let coverage_dir = root.join("coverage");
        std::fs::create_dir_all(&coverage_dir)?;

        for (module_name, coverage_data) in &self.coverage_map {
            let report = serde_json::json!({
                "module": module_name,
                "covered_spans": coverage_data.covered_spans,
                "total_lines": coverage_data.total_lines,
                "covered_lines": coverage_data.covered_lines.iter().collect::<Vec<_>>(),
            });

            let path = coverage_dir.join(format!("{}.coverage.json", module_name));
            std::fs::write(path, serde_json::to_string_pretty(&report)?)?;
        }

        Ok(())
    }
}