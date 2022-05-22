use crate::encode::Encode;

pub struct Encoder {
    pub buffer: Vec<u8>,
    // Int
    used_bits: usize,
    // Int
    current_byte: u8,
}

impl Default for Encoder {
    fn default() -> Self {
        Self::new()
    }
}

impl Encoder {
    pub fn new() -> Encoder {
        Encoder {
            buffer: Vec::new(),
            used_bits: 0,
            current_byte: 0,
        }
    }

    /// Encode any type that implements [`Encode`].
    pub fn encode<T: Encode>(&mut self, x: T) -> Result<&mut Self, String> {
        x.encode(self)?;
        Ok(self)
    }

    pub fn u8(&mut self, x: u8) -> Result<&mut Self, String> {
        if self.used_bits == 0 {
            self.current_byte = x;
            self.next_word();
        } else {
            todo!()
        }

        Ok(self)
    }

    /// Encode a `bool` value.
    pub fn bool(&mut self, x: bool) -> &mut Self {
        if x {
            self.one()
        } else {
            self.zero()
        }
    }

    fn zero(&mut self) -> &mut Self {
        if self.used_bits == 7 {
            self.next_word();
        } else {
            self.used_bits += 1;
        }

        self
    }

    fn one(&mut self) -> &mut Self {
        if self.used_bits == 7 {
            self.current_byte |= 1;
            self.next_word();
        } else {
            self.current_byte |= 128 >> self.used_bits;
            self.used_bits += 1;
        }

        self
    }

    fn next_word(&mut self) {
        self.buffer.push(self.current_byte);

        self.current_byte = 0;
        self.used_bits = 0;
    }

    pub(crate) fn filler(&mut self) {
        self.current_byte |= 1;
        self.next_word();
    }
}
