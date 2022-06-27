use std::collections::HashMap;

use crate::ast::Unique;

use super::Level;

#[derive(Debug)]

pub struct BiMap {
    left: HashMap<Unique, Level>,
    right: HashMap<Level, Unique>,
}

impl BiMap {
    pub(super) fn new() -> Self {
        BiMap {
            right: HashMap::new(),
            left: HashMap::new(),
        }
    }

    pub(super) fn insert(&mut self, unique: Unique, level: Level) {
        self.left.insert(unique, level);
        self.right.insert(level, unique);
    }

    pub(super) fn remove(&mut self, unique: Unique, level: Level) {
        self.left.remove(&unique);
        self.right.remove(&level);
    }

    pub(super) fn get(&self, unique: &Unique) -> Option<&Level> {
        self.left.get(unique)
    }

    pub(super) fn get_right(&self, level: &Level) -> Option<&Unique> {
        self.right.get(level)
    }
}
