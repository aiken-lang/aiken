use std::cmp;

/// Calculate Levenshtein distance for two UTF-8 encoded strings.
///
/// Returns a minimum number of edits to transform from source to target string.
///
/// Levenshtein distance accepts three edit operations: insertion, deletion,
/// and substitution.
///
/// References:
///
/// - [Levenshtein distance in Cargo][1]
/// - [Ilia Schelokov: Optimizing loop heavy Rust code][2]
///
/// [1]: https://github.com/rust-lang/cargo/blob/7d7fe6797ad07f313706380d251796702272b150/src/cargo/util/lev_distance.rs
/// [2]: https://thaumant.me/optimizing-loop-heavy-rust/
pub fn distance(source: &str, target: &str) -> usize {
    if source.is_empty() {
        return target.len();
    }
    if target.is_empty() {
        return source.len();
    }

    let mut distances = (0..=target.chars().count()).collect::<Vec<_>>();

    for (i, ch1) in source.chars().enumerate() {
        let mut sub = i;
        distances[0] = sub + 1;
        for (j, ch2) in target.chars().enumerate() {
            let dist = cmp::min(
                cmp::min(distances[j], distances[j + 1]) + 1,
                sub + (ch1 != ch2) as usize,
            );

            sub = distances[j + 1];
            distances[j + 1] = dist;
        }
    }

    *distances.last().unwrap()
}
