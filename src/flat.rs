pub trait Encode {
    fn encode(&self, e: &mut Encoder) -> Result<(), String>;
}

impl Encode for bool {
    fn encode(&self, e: &mut Encoder) -> Result<(), String> {
        e.bool(*self);

        Ok(())
    }
}

enum Filler {
    FillerStart(Box<Filler>),
    FillerEnd,
}

impl Filler {
    pub fn len(&self) -> usize {
        match self {
            Filler::FillerStart(f) => f.len() + 1,
            Filler::FillerEnd => 1,
        }
    }
}

impl Encode for Filler {
    fn encode(&self, e: &mut Encoder) -> Result<(), String> {
        e.filler();

        Ok(())
    }
}

impl<T, K> Encode for (T, K)
where
    T: Encode,
    K: Encode,
{
    fn encode(&self, e: &mut Encoder) -> Result<(), String> {
        self.0.encode(e)?;
        self.1.encode(e)?;

        Ok(())
    }
}

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
        todo!()
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

    fn filler(&mut self) {
        self.current_byte |= 1;
        self.next_word();
    }
}

pub fn encode<T>(value: T) -> Result<Vec<u8>, String>
where
    T: Encode,
{
    let mut e = Encoder::new();

    e.encode((value, Filler::FillerEnd))?;

    Ok(e.buffer)
}

#[cfg(test)]
mod test {
    #[test]
    fn encode_bool() {
        let bytes = super::encode(true).unwrap();

        assert_eq!(bytes, vec![0b10000001]);

        let bytes = super::encode(false).unwrap();

        assert_eq!(bytes, vec![0b00000001]);
    }
}
