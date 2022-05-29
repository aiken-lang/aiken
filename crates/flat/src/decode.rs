use crate::decoder::Decoder;

pub trait Decode<'b>: Sized {
    fn decode(d: &mut Decoder<'b>) -> Result<Self, String>;
}
