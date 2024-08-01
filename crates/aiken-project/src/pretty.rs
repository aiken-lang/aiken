use owo_colors::{OwoColorize, Stream};
use std::{self, cmp, fmt::Display};

pub fn say<A>(what: A)
where
    A: Display,
{
    eprintln!("{}", what)
}

pub fn fmt_step<A>(
    f: &mut std::fmt::Formatter,
    title: &str,
    payload: &A,
) -> std::result::Result<(), std::fmt::Error>
where
    A: Display,
{
    write!(
        f,
        "{:>13} {}",
        title
            .if_supports_color(Stream::Stderr, |s| s.bold())
            .if_supports_color(Stream::Stderr, |s| s.purple()),
        payload.if_supports_color(Stream::Stderr, |s| s.bold())
    )
}

pub fn ansi_len(s: &str) -> usize {
    String::from_utf8(strip_ansi_escapes::strip(s).unwrap())
        .unwrap()
        .chars()
        .count()
}

pub fn len_longest_line(zero: usize, s: &str) -> usize {
    s.lines().fold(zero, |max, l| {
        let n = ansi_len(l);
        if n > max {
            n
        } else {
            max
        }
    })
}

pub fn boxed(title: &str, content: &str) -> String {
    boxed_with(title, content, |s| s.to_string())
}

pub fn boxed_with(title: &str, content: &str, border_style: fn(&str) -> String) -> String {
    let n = len_longest_line(ansi_len(title) + 1, content);

    let content = content
        .lines()
        .map(|line| {
            format!(
                "{} {} {}",
                border_style("│"),
                pad_right(line.to_string(), n, " "),
                border_style("│"),
            )
        })
        .collect::<Vec<String>>()
        .join("\n");

    let top = format!(
        "{} {}{}",
        border_style("┍━"),
        pad_right(format!("{title} "), n, &border_style("━")),
        border_style("┑"),
    );

    let bottom = format!(
        "{}{}{}",
        border_style("┕"),
        pad_right(String::new(), n + 2, &border_style("━")),
        border_style("┙")
    );

    format!("{top}\n{content}\n{bottom}")
}

pub fn open_box(
    title: &str,
    content: &str,
    footer: &str,
    border_style: fn(&str) -> String,
) -> String {
    let i = ansi_len(content.lines().collect::<Vec<_>>().first().unwrap());
    let j = len_longest_line(ansi_len(title) + 1, content);
    let k = ansi_len(footer);

    let content = content
        .lines()
        .map(|line| format!("{} {line}", border_style("│"),))
        .collect::<Vec<String>>()
        .join("\n");

    let top = format!(
        "{} {}",
        border_style(if footer.is_empty() {
            "┝━"
        } else {
            "┍━"
        }),
        pad_right(format!("{title} "), i - 1, &border_style("━")),
    );

    let bottom = if footer.is_empty() {
        border_style("╽")
    } else {
        format!(
            "{} {}",
            pad_right(
                border_style("┕"),
                if j < k { 0 } else { j + 1 - k },
                &border_style("━")
            ),
            footer
        )
    };

    format!("{top}\n{content}\n{bottom}")
}

pub fn indent(lines: &str, n: usize) -> String {
    let tab = pad_left(String::new(), n, " ");
    lines
        .lines()
        .map(|line| format!("{tab}{line}"))
        .collect::<Vec<_>>()
        .join("\n")
}

pub fn pad_left(mut text: String, n: usize, delimiter: &str) -> String {
    let diff = n as i32 - ansi_len(&text) as i32;
    if diff.is_positive() {
        for _ in 0..diff {
            text.insert_str(0, delimiter);
        }
    }
    text
}

pub fn pad_right(mut text: String, n: usize, delimiter: &str) -> String {
    let diff = n as i32 - ansi_len(&text) as i32;
    if diff.is_positive() {
        for _ in 0..diff {
            text.push_str(delimiter);
        }
    }
    text
}

pub fn style_if(styled: bool, s: String, apply_style: fn(String) -> String) -> String {
    if styled {
        apply_style(s)
    } else {
        s
    }
}

pub fn multiline(max_len: usize, s: String) -> Vec<String> {
    let mut xs = Vec::new();
    let mut i = 0;
    let len = s.len();
    loop {
        let lo = i * max_len;
        let hi = cmp::min(len, lo + max_len);

        if lo >= len {
            break;
        }

        let chunk = &s[lo..hi];
        xs.push(chunk.to_string());
        i += 1;
    }
    xs
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn multiline_empty_string() {
        assert_eq!(multiline(10, "".to_string()), Vec::<String>::new());
    }

    #[test]
    fn multiline_single_line() {
        assert_eq!(multiline(10, "foo".to_string()), vec!["foo".to_string()]);
    }

    #[test]
    fn multiline_single_line_limit() {
        assert_eq!(multiline(3, "foo".to_string()), vec!["foo".to_string()]);
    }

    #[test]
    fn multiline_many_lines() {
        assert_eq!(
            multiline(3, "foobar".to_string()),
            vec!["foo".to_string(), "bar".to_string()]
        );
    }
}
