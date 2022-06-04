mod decoder;
mod error;

use crate::filler::Filler;

pub use decoder::Decoder;
pub use error::Error;

pub trait Decode<'b>: Sized {
    fn decode(d: &mut Decoder) -> Result<Self, Error>;
}

impl Decode<'_> for Filler {
    fn decode(d: &mut Decoder) -> Result<Filler, Error> {
        d.filler()?;

        Ok(Filler::FillerEnd)
    }
}

impl Decode<'_> for Vec<u8> {
    fn decode(d: &mut Decoder) -> Result<Self, Error> {
        d.bytes()
    }
}

impl Decode<'_> for u8 {
    fn decode(d: &mut Decoder) -> Result<Self, Error> {
        d.u8()
    }
}

impl Decode<'_> for isize {
    fn decode(d: &mut Decoder) -> Result<Self, Error> {
        d.integer()
    }
}

impl Decode<'_> for usize {
    fn decode(d: &mut Decoder) -> Result<Self, Error> {
        d.word()
    }
}

impl Decode<'_> for char {
    fn decode(d: &mut Decoder) -> Result<Self, Error> {
        d.char()
    }
}

impl Decode<'_> for String {
    fn decode(d: &mut Decoder) -> Result<Self, Error> {
        d.utf8()
    }
}

impl Decode<'_> for bool {
    fn decode(d: &mut Decoder) -> Result<bool, Error> {
        d.bool()
    }
}
