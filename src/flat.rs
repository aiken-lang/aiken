pub trait Encode {
    fn encode(&self, e: &mut Encoder) -> Result<(), String>;
}

pub struct Encoder {
    pub bytes: Vec<u8>,
}

impl Default for Encoder {
    fn default() -> Self {
        Self::new()
    }
}

impl Encoder {
    pub fn new() -> Encoder {
        Encoder { bytes: Vec::new() }
    }

    /// Encode any type that implements [`Encode`].
    pub fn encode<T: Encode>(&mut self, x: T) -> Result<&mut Self, String> {
        x.encode(self)?;
        Ok(self)
    }

    pub fn u8(&mut self, x: u8) -> Result<&mut Self, String> {
        todo!()
    }
}
