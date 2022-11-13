use std::fmt;

use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("{} is not a valid project name. {}", name, reason.to_string())]
    InvalidProjectName {
        name: String,
        reason: InvalidProjectNameReason,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum InvalidProjectNameReason {
    AikenPrefix,
    AikenReservedModule,
    Format,
}

impl fmt::Display for InvalidProjectNameReason {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            InvalidProjectNameReason::AikenPrefix => write!(f, "It is a reserved word in Aiken."),
            InvalidProjectNameReason::AikenReservedModule => {
                write!(f, "It is a reserved module name in Aiken.")
            }
            InvalidProjectNameReason::Format => write!(
                f,
                "It does not have the correct format. Project names \
                must start with a lowercase letter and may only contain lowercase letters, \
                numbers and underscores."
            ),
        }
    }
}
