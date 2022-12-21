use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Reached end of buffer")]
    EndOfBuffer,
    #[error("Buffer is not byte aligned")]
    BufferNotByteAligned,
    #[error("Incorrect value of num_bits, must be less than 9")]
    IncorrectNumBits,
    #[error("Not enough data available, required {0} bytes")]
    NotEnoughBytes(usize),
    #[error("Not enough data available, required {0} bits")]
    NotEnoughBits(usize),
    #[error(transparent)]
    DecodeUtf8(#[from] std::string::FromUtf8Error),
    #[error("Decoding u32 to char {0}")]
    DecodeChar(u32),
    #[error("{0}")]
    Message(String),
    #[error("Parse error: So far we parsed\n\n{0}\n\nand we ran into error: {1}")]
    ParseError(String, anyhow::Error),
    #[error("Unknown term constructor tag: {0}.\n\nHere are the buffer bytes ({1} preceding) {2}\n\nBuffer position is {3} and buffer length is {4}")]
    UnknownTermConstructor(u8, usize, String, usize, usize),
    #[error(transparent)]
    Custom(#[from] anyhow::Error),
}
