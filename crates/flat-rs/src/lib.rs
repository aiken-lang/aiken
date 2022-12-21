mod decode;
mod encode;
pub mod filler;
pub mod zigzag;

pub mod en {
    pub use super::encode::*;
}

pub mod de {
    pub use super::decode::*;
}

pub trait Flat<'b>: en::Encode + de::Decode<'b> {
    fn flat(&self) -> Result<Vec<u8>, en::Error> {
        encode(self)
    }

    fn unflat(bytes: &'b [u8]) -> Result<Self, de::Error> {
        decode(bytes)
    }
}

pub fn encode<T>(value: &T) -> Result<Vec<u8>, en::Error>
where
    T: en::Encode,
{
    let mut e = en::Encoder::new();

    value.encode(&mut e)?;
    e.encode(filler::Filler::FillerEnd)?;

    Ok(e.buffer)
}

pub fn decode<'b, T>(bytes: &'b [u8]) -> Result<T, de::Error>
where
    T: de::Decode<'b>,
{
    let mut d = de::Decoder::new(bytes);

    let value = d.decode()?;

    d.decode::<filler::Filler>()?;

    Ok(value)
}
