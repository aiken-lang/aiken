//! LCOV format report generation for code coverage.
//!
//! LCOV is a standard coverage data format that can be processed by tools
//! like `genhtml` to generate HTML reports, or uploaded to coverage services.

use aiken_lang::{ast::SourceLocation, line_numbers::LineNumbers};
use indexmap::IndexMap;
use std::{
    collections::HashMap,
    fs::File,
    io::{self, Write},
    path::{Path, PathBuf},
};

/// Metadata about a module needed for coverage reporting.
#[derive(Debug, Clone)]
pub struct ModuleInfo {
    /// The actual file path on disk
    pub file_path: PathBuf,
    /// Whether this module is part of the main project (not a dependency)
    pub is_project_module: bool,
}

/// An LCOV coverage report.
///
/// Aggregates coverage data by file and line number.
#[derive(Debug, Default)]
pub struct LcovReport {
    /// Map from file path to line coverage data.
    /// Each line maps to execution count.
    file_coverage: HashMap<PathBuf, HashMap<usize, u64>>,
}

impl LcovReport {
    pub fn new() -> Self {
        Self {
            file_coverage: HashMap::new(),
        }
    }

    /// Initialize coverage data for all coverable source locations.
    /// Sets all lines to count=0, indicating they haven't been executed yet.
    /// Only includes modules from the project (not dependencies).
    ///
    /// Converts byte spans to line numbers using the provided module sources.
    pub fn init_coverable_locations<'a>(
        &mut self,
        all_locations: impl IntoIterator<Item = &'a SourceLocation>,
        module_sources: &IndexMap<&str, &(String, LineNumbers)>,
        module_info: &HashMap<String, ModuleInfo>,
    ) {
        for loc in all_locations {
            // Skip modules not from the main project (dependencies)
            let info = match module_info.get(&loc.module) {
                Some(info) if info.is_project_module => info,
                _ => continue,
            };

            if let Some((_, line_numbers)) = module_sources.get(loc.module.as_str()) {
                if let Some(line_info) = line_numbers.line_and_column_number(loc.span.start) {
                    // Only insert if not already present (don't overwrite executed counts)
                    self.file_coverage
                        .entry(info.file_path.clone())
                        .or_default()
                        .entry(line_info.line)
                        .or_insert(0);
                }
            }
        }
    }

    /// Add coverage data from executed source locations.
    /// Increments the execution count for each executed line.
    /// Only includes modules from the project (not dependencies).
    ///
    /// Converts byte spans to line numbers using the provided module sources.
    pub fn add_coverage<'a>(
        &mut self,
        executed_locations: impl IntoIterator<Item = &'a SourceLocation>,
        module_sources: &IndexMap<&str, &(String, LineNumbers)>,
        module_info: &HashMap<String, ModuleInfo>,
    ) {
        for loc in executed_locations {
            // Skip modules not from the main project (dependencies)
            let info = match module_info.get(&loc.module) {
                Some(info) if info.is_project_module => info,
                _ => continue,
            };

            if let Some((_, line_numbers)) = module_sources.get(loc.module.as_str()) {
                if let Some(line_info) = line_numbers.line_and_column_number(loc.span.start) {
                    *self
                        .file_coverage
                        .entry(info.file_path.clone())
                        .or_default()
                        .entry(line_info.line)
                        .or_insert(0) += 1;
                }
            }
        }
    }

    /// Get coverage statistics for a file.
    pub fn file_stats(&self, file: &Path) -> Option<(usize, usize)> {
        self.file_coverage.get(file).map(|lines| {
            let total = lines.len();
            let hit = lines.values().filter(|&&count| count > 0).count();
            (hit, total)
        })
    }

    /// Get overall coverage statistics.
    pub fn overall_stats(&self) -> (usize, usize) {
        let mut total = 0;
        let mut hit = 0;
        for lines in self.file_coverage.values() {
            total += lines.len();
            hit += lines.values().filter(|&&count| count > 0).count();
        }
        (hit, total)
    }

    /// Write the coverage report in LCOV format.
    ///
    /// LCOV format:
    /// ```text
    /// TN:test_name
    /// SF:/path/to/source.ak
    /// DA:line_number,execution_count
    /// ...
    /// LF:total_lines
    /// LH:hit_lines
    /// end_of_record
    /// ```
    pub fn write_to_file(&self, path: &Path) -> io::Result<()> {
        let mut file = File::create(path)?;
        self.write(&mut file)
    }

    /// Write the coverage report to a writer.
    pub fn write<W: Write>(&self, writer: &mut W) -> io::Result<()> {
        // Sort files for deterministic output
        let mut files: Vec<_> = self.file_coverage.keys().collect();
        files.sort();

        for source_file in files {
            let line_coverage = &self.file_coverage[source_file];
            let file_str = source_file.display();

            // Test name (using file name)
            writeln!(writer, "TN:{}", file_str)?;

            // Source file (absolute path for genhtml compatibility)
            writeln!(writer, "SF:{}", file_str)?;

            // Sort lines for deterministic output
            let mut lines: Vec<_> = line_coverage.iter().collect();
            lines.sort_by_key(|(line, _)| *line);

            // Data lines: DA:line_number,execution_count
            for (line_num, count) in lines {
                writeln!(writer, "DA:{},{}", line_num, count)?;
            }

            // Lines found (total lines with coverage data)
            writeln!(writer, "LF:{}", line_coverage.len())?;

            // Lines hit (lines with execution count > 0)
            let hit_lines = line_coverage.values().filter(|&&c| c > 0).count();
            writeln!(writer, "LH:{}", hit_lines)?;

            writeln!(writer, "end_of_record")?;
        }

        Ok(())
    }

    /// Render the report as a string.
    pub fn to_string(&self) -> String {
        let mut buffer = Vec::new();
        self.write(&mut buffer)
            .expect("writing to Vec should not fail");
        String::from_utf8(buffer).expect("LCOV output should be valid UTF-8")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use aiken_lang::ast::Span;

    fn make_loc(module: &str, start: usize, end: usize) -> SourceLocation {
        SourceLocation::new(module, Span { start, end })
    }

    fn make_module_info(name: &str) -> (String, ModuleInfo) {
        (
            name.to_string(),
            ModuleInfo {
                file_path: PathBuf::from(format!("{}.ak", name)),
                is_project_module: true,
            },
        )
    }

    #[test]
    fn test_lcov_simple() {
        let mut report = LcovReport::new();

        // Create simple source: "line1\nline2\nline3"
        let src = "line1\nline2\nline3";
        let line_numbers = LineNumbers::new(src);
        let mut module_sources: IndexMap<&str, &(String, LineNumbers)> = IndexMap::new();
        let data = (src.to_string(), line_numbers);
        module_sources.insert("test", &data);

        let module_info: HashMap<String, ModuleInfo> = [make_module_info("test")].into();

        // Execute lines 1 and 3 (byte offsets 0-5 and 12-17)
        let locations = vec![
            make_loc("test", 0, 5),   // line 1
            make_loc("test", 12, 17), // line 3
            make_loc("test", 0, 5),   // line 1 again (should increment count)
        ];

        report.add_coverage(locations.iter(), &module_sources, &module_info);

        let output = report.to_string();
        assert!(output.contains("TN:test.ak"));
        assert!(output.contains("SF:test.ak"));
        assert!(output.contains("DA:1,2")); // line 1 hit twice
        assert!(output.contains("DA:3,1")); // line 3 hit once
        assert!(output.contains("LF:2")); // 2 lines with data
        assert!(output.contains("LH:2")); // 2 lines hit
        assert!(output.contains("end_of_record"));
    }

    #[test]
    fn test_lcov_multiple_files() {
        let mut report = LcovReport::new();

        let src_a = "aaa\nbbb";
        let src_b = "xxx\nyyy\nzzz";
        let line_numbers_a = LineNumbers::new(src_a);
        let line_numbers_b = LineNumbers::new(src_b);
        let data_a = (src_a.to_string(), line_numbers_a);
        let data_b = (src_b.to_string(), line_numbers_b);
        let mut module_sources: IndexMap<&str, &(String, LineNumbers)> = IndexMap::new();
        module_sources.insert("module_a", &data_a);
        module_sources.insert("module_b", &data_b);

        let module_info: HashMap<String, ModuleInfo> =
            [make_module_info("module_a"), make_module_info("module_b")].into();

        let locations = vec![
            make_loc("module_a", 0, 3),  // line 1 in a
            make_loc("module_b", 4, 7),  // line 2 in b
            make_loc("module_b", 8, 11), // line 3 in b
        ];

        report.add_coverage(locations.iter(), &module_sources, &module_info);

        let output = report.to_string();

        // Both files should be present
        assert!(output.contains("SF:module_a.ak"));
        assert!(output.contains("SF:module_b.ak"));

        // Check stats
        let (hit, total) = report.overall_stats();
        assert_eq!(total, 3);
        assert_eq!(hit, 3);
    }

    #[test]
    fn test_lcov_stats() {
        let mut report = LcovReport::new();

        let src = "a\nb\nc\nd\ne";
        let line_numbers = LineNumbers::new(src);
        let data = (src.to_string(), line_numbers);
        let mut module_sources: IndexMap<&str, &(String, LineNumbers)> = IndexMap::new();
        module_sources.insert("test", &data);

        let module_info: HashMap<String, ModuleInfo> = [make_module_info("test")].into();

        // Only execute lines 1, 3, 5
        let locations = vec![
            make_loc("test", 0, 1), // line 1
            make_loc("test", 4, 5), // line 3
            make_loc("test", 8, 9), // line 5
        ];

        report.add_coverage(locations.iter(), &module_sources, &module_info);

        let (hit, total) = report.file_stats(Path::new("test.ak")).unwrap();
        assert_eq!(hit, 3);
        assert_eq!(total, 3);
    }

    #[test]
    fn test_lcov_filters_dependencies() {
        let mut report = LcovReport::new();

        let src = "line1\nline2";
        let line_numbers = LineNumbers::new(src);
        let data = (src.to_string(), line_numbers);
        let mut module_sources: IndexMap<&str, &(String, LineNumbers)> = IndexMap::new();
        module_sources.insert("stdlib/list", &data);

        // stdlib module is not a project module
        let module_info: HashMap<String, ModuleInfo> = [(
            "stdlib/list".to_string(),
            ModuleInfo {
                file_path: PathBuf::from("stdlib/list.ak"),
                is_project_module: false, // dependency
            },
        )]
        .into();

        let locations = vec![make_loc("stdlib/list", 0, 5)];

        report.add_coverage(locations.iter(), &module_sources, &module_info);

        // Should be empty because stdlib is filtered out
        let (hit, total) = report.overall_stats();
        assert_eq!(hit, 0);
        assert_eq!(total, 0);
    }
}
