use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Buffer is not byte aligned")]
    BufferNotByteAligned,
    #[error("{0}")]
    Message(String),
    #[error(transparent)]
    Custom(#[from] anyhow::Error),
}
