use std::sync::{
    atomic::{AtomicU64, Ordering},
    Arc,
};

pub mod air;
pub mod ast;
pub mod builtins;
pub mod expr;
pub mod format;
pub mod parser;
pub mod pretty;
pub mod tipo;
pub mod uplc;

#[derive(Debug, Default, Clone)]
pub struct IdGenerator {
    id: Arc<AtomicU64>,
}

impl IdGenerator {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn next(&self) -> u64 {
        self.id.fetch_add(1, Ordering::Relaxed)
    }
}

#[cfg(test)]
mod tests;
