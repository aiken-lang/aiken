pub fn boxed(title: &str, content: String) -> String {
    let n = content.lines().fold(0, |max, l| {
        let n = l.len();
        if n > max {
            n
        } else {
            max
        }
    });

    let content = content
        .lines()
        .map(|line| format!("│ {} │", pad_right(line.to_string(), n, " ")))
        .collect::<Vec<String>>()
        .join("\n");

    let top = format!("┍━ {}┑", pad_right(format!("{title} "), n, "━"));
    let bottom = format!("┕{}┙", pad_right(String::new(), n + 2, "━"));
    format!("{top}\n{content}\n{bottom}")
}

pub fn pad_left(mut text: String, n: usize, delimiter: &str) -> String {
    let diff = n as i32 - text.len() as i32;
    if diff.is_positive() {
        for _ in 0..diff {
            text.insert_str(0, delimiter);
        }
    }
    text
}

pub fn pad_right(mut text: String, n: usize, delimiter: &str) -> String {
    let diff = n as i32 - text.len() as i32;
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
