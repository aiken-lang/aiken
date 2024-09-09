use std::collections::HashMap;

use vec1::{vec1, Vec1};

#[derive(Clone)]
pub struct AirInterner {
    identifiers: HashMap<String, Vec1<usize>>,
    current: usize,
}

impl Default for AirInterner {
    fn default() -> Self {
        Self::new()
    }
}

/// Interner that uses previous uniques to prevent future unique collisions
/// when performing optimizations
impl AirInterner {
    pub fn new() -> Self {
        Self {
            identifiers: HashMap::new(),
            current: 0,
        }
    }

    pub fn intern(&mut self, text: String) {
        if let Some(u) = self.identifiers.get_mut(&text) {
            u.push(self.current);

            self.current += 1;
        } else {
            self.identifiers.insert(text, vec1!(self.current));

            self.current += 1;
        }
    }

    pub fn pop_text(&mut self, text: String) {
        if let Some(mut u) = self.identifiers.remove(&text) {
            if u.len() != 1 {
                u.pop().unwrap();
                self.identifiers.insert(text, u);
            }
        } else {
            unreachable!("Looking up a missing text: {}", text);
        }
    }

    pub fn lookup_interned(&self, text: &String) -> String {
        if let Some(u) = self.identifiers.get(text) {
            format!("{}_id_{}", text, *u.last())
        } else {
            unreachable!("Looking up a missing text: {}", text);
        }
    }
}
