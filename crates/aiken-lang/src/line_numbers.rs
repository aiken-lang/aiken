use std::fmt::{self, Display};

#[derive(Debug, PartialEq, Eq, Clone, serde::Serialize, serde::Deserialize)]
pub struct LineNumbers {
    line_starts: Vec<usize>,
    length: usize,
    last: Option<usize>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct LineColumn {
    pub line: usize,
    pub column: usize,
}

impl Display for LineColumn {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        f.write_str(&format!("L{};{}", self.line, self.column))
    }
}

impl LineNumbers {
    pub fn new(src: &str) -> Self {
        let line_starts: Vec<usize> = std::iter::once(0)
            .chain(src.match_indices('\n').map(|(i, _)| i + 1))
            .collect();

        let length = src.len();

        Self {
            length,
            last: line_starts.last().cloned(),
            line_starts: if length > 0 { line_starts } else { Vec::new() },
        }
    }

    /// Get the line number for a byte index
    pub fn line_number(&self, byte_index: usize) -> Option<usize> {
        self.line_starts
            .binary_search(&byte_index)
            .map(|l| Some(l + 1))
            .unwrap_or_else(|next_index| {
                if Some(next_index) >= self.last {
                    None
                } else {
                    Some(next_index)
                }
            })
    }

    pub fn line_and_column_number(&self, byte_index: usize) -> Option<LineColumn> {
        let line = self.line_number(byte_index)?;
        let column = byte_index - self.line_starts.get(line - 1).copied().unwrap_or_default() + 1;
        Some(LineColumn { line, column })
    }

    #[allow(dead_code)]
    pub fn byte_index(&self, line: usize, character: usize) -> usize {
        match self.line_starts.get(line) {
            Some(line_index) => *line_index + character,
            None => self.length,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chumsky::text::Character;
    use indoc::indoc;

    fn assert_line_column(src: &str, ix: usize, lcol: Option<LineColumn>) {
        let lines = LineNumbers::new(src);

        println!("{lines:?}");

        let byte = src
            .as_bytes()
            .get(ix)
            .map(|b| {
                if b.is_ascii() {
                    format!("{}", b.to_char())
                } else {
                    format!("{b}")
                }
            })
            .unwrap_or_else(|| "OUT-OF-BOUNDS".to_string());

        assert_eq!(
            lines.line_and_column_number(ix),
            lcol,
            "\n{src}\n--> at index {ix} ({byte})\n",
        );
    }

    #[test]
    fn out_of_range_byte_index() {
        let src = indoc! { r#""# };
        assert_line_column(src, 42, None);
        assert_line_column(src, 0, None);
    }

    #[test]
    fn basic() {
        let src = indoc! { r#"
            foo
            bar
        "# };

        assert_line_column(src, 0, Some(LineColumn { line: 1, column: 1 }));
        assert_line_column(src, 2, Some(LineColumn { line: 1, column: 3 }));
        assert_line_column(src, 4, Some(LineColumn { line: 2, column: 1 }));
    }

    #[test]
    fn unicode() {
        let src = indoc! { r#"
            💩
            foo
        "# };

        assert_line_column(src, 0, Some(LineColumn { line: 1, column: 1 }));
        assert_line_column(src, 2, Some(LineColumn { line: 1, column: 3 }));
        assert_line_column(src, 5, Some(LineColumn { line: 2, column: 1 }));
    }
}
