pub fn ansi_len(s: &str) -> usize {
    String::from_utf8(strip_ansi_escapes::strip(s).unwrap())
        .unwrap()
        .chars()
        .count()
}

pub fn len_longest_line(s: &str) -> usize {
    s.lines().fold(0, |max, l| {
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
    let n = len_longest_line(content);

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
    let j = len_longest_line(content);
    let k = ansi_len(footer);

    let content = content
        .lines()
        .map(|line| format!("{} {line}", border_style("│"),))
        .collect::<Vec<String>>()
        .join("\n");

    let top = format!(
        "{} {}",
        border_style("┍━"),
        pad_right(format!("{title} "), i - 1, &border_style("━")),
    );

    let bottom = format!(
        "{} {}",
        pad_right(border_style("┕"), j - k + 1, &border_style("━")),
        footer
    );

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
