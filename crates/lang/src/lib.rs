use std::sync::atomic::{AtomicU64, Ordering};

pub mod ast;
pub mod builtins;
pub mod error;
pub mod expr;
pub mod lexer;
pub mod parser;
pub mod tipo;
pub mod token;

#[derive(Debug, Default)]
pub struct IdGenerator {
    id: AtomicU64,
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
