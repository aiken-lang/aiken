mod encoder;
mod error;

use crate::filler::Filler;

pub use encoder::Encoder;
pub use error::Error;

pub trait Encode {
    fn encode(&self, e: &mut Encoder) -> Result<(), Error>;
}

impl Encode for bool {
    fn encode(&self, e: &mut Encoder) -> Result<(), Error> {
        e.bool(*self);

        Ok(())
    }
}

impl Encode for u8 {
    fn encode(&self, e: &mut Encoder) -> Result<(), Error> {
        e.u8(*self)?;

        Ok(())
    }
}

impl Encode for i128 {
    fn encode(&self, e: &mut Encoder) -> Result<(), Error> {
        e.big_integer(*self);

        Ok(())
    }
}

impl Encode for isize {
    fn encode(&self, e: &mut Encoder) -> Result<(), Error> {
        e.integer(*self);

        Ok(())
    }
}

impl Encode for usize {
    fn encode(&self, e: &mut Encoder) -> Result<(), Error> {
        e.word(*self);

        Ok(())
    }
}

impl Encode for char {
    fn encode(&self, e: &mut Encoder) -> Result<(), Error> {
        e.char(*self);

        Ok(())
    }
}

impl Encode for &str {
    fn encode(&self, e: &mut Encoder) -> Result<(), Error> {
        e.utf8(self)?;

        Ok(())
    }
}

impl Encode for String {
    fn encode(&self, e: &mut Encoder) -> Result<(), Error> {
        e.utf8(self)?;

        Ok(())
    }
}

impl Encode for Vec<u8> {
    fn encode(&self, e: &mut Encoder) -> Result<(), Error> {
        e.bytes(self)?;

        Ok(())
    }
}

impl Encode for &[u8] {
    fn encode(&self, e: &mut Encoder) -> Result<(), Error> {
        e.bytes(self)?;

        Ok(())
    }
}

impl<T: Encode> Encode for Box<T> {
    fn encode(&self, e: &mut Encoder) -> Result<(), Error> {
        self.as_ref().encode(e)?;

        Ok(())
    }
}

impl Encode for Filler {
    fn encode(&self, e: &mut Encoder) -> Result<(), Error> {
        e.filler();

        Ok(())
    }
}
