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
        let b = d.bool();
        Ok(b)
    }
}
