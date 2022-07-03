use thiserror::Error;

use super::ExBudget;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Over budget mem: {} & cpu: {}", .0.mem, .0.cpu)]
    OutOfExError(ExBudget),
}
