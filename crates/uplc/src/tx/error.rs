use crate::machine;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("Runtime error")]
    Machine(#[from] machine::Error),
}
