use crate::{decoder::Decoder, filler::Filler};

pub trait Decode<'b>: Sized {
    fn decode(d: &mut Decoder) -> Result<Self, String>;
}

impl Decode<'_> for Filler {
    fn decode(d: &mut Decoder) -> Result<Filler, String> {
        d.filler()?;

        Ok(Filler::FillerEnd)
    }
}

impl Decode<'_> for Vec<u8> {
    fn decode(d: &mut Decoder) -> Result<Self, String> {
        d.bytes()
    }
}

impl Decode<'_> for u8 {
    fn decode(d: &mut Decoder) -> Result<Self, String> {
        d.u8()
    }
}

impl Decode<'_> for isize {
    fn decode(d: &mut Decoder) -> Result<Self, String> {
        d.integer()
    }
}

impl Decode<'_> for usize {
    fn decode(d: &mut Decoder) -> Result<Self, String> {
        d.word()
    }
}

impl Decode<'_> for char {
    fn decode(d: &mut Decoder) -> Result<Self, String> {
        d.char()
    }
}

impl Decode<'_> for String {
    fn decode(d: &mut Decoder) -> Result<Self, String> {
        d.string()
    }
}

impl Decode<'_> for bool {
    fn decode(d: &mut Decoder) -> Result<bool, String> {
        d.bool()
    }
}
