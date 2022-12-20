use std::fmt;

use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("'{}' is not a valid project name: {}", name, reason.to_string())]
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
            InvalidProjectNameReason::AikenPrefix => write!(f, "it's a reserved word in Aiken."),
            InvalidProjectNameReason::AikenReservedModule => {
                write!(f, "it's a reserved module name in Aiken.")
            }
            InvalidProjectNameReason::Format => write!(
                f,
                "it is malformed.\n\nProjects must be named as:\n\n\t\
                {{repository}}/{{project}}\n\nEach part must start with a lowercase letter \
                and may only contain lowercase letters, numbers, hyphens or underscores.\
                \nFor example,\n\n\taiken-lang/stdlib"
            ),
        }
    }
}
