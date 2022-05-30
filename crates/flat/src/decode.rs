use crate::{decoder::Decoder, filler::Filler};

pub trait Decode<'b>: Sized {
    fn decode(d: &mut Decoder<'b>) -> Result<Self, String>;
}

impl<'b> Decode<'b> for Filler {
    fn decode(d: &mut Decoder<'b>) -> Result<Self, String> {
        todo!()
    }
}
