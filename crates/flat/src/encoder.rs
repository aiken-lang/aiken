use crate::encode::Encode;

pub struct Encoder {
    pub buffer: Vec<u8>,
    // Int
    used_bits: i64,
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
            self.byte_unaligned(x);
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

    pub fn bytes(&mut self, arr: &[u8]) -> Result<&mut Self, String> {
        if self.used_bits != 0 {
            return Err("Buffer is not byte aligned".to_string());
        }
        self.write_blk(arr, &mut 0);
        Ok(self)
    }

    pub fn char(&mut self, c: char) -> Result<&mut Self, String> {
        self.word(c as u32);
        Ok(self)
    }

    pub fn string(&mut self, s: &str) -> Result<&mut Self, String> {
        for i in s.chars() {
            self.one();
            self.char(i)?;
        }

        self.zero();

        Ok(self)
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

    fn byte_unaligned(&mut self, x: u8) {
        let x_shift = self.current_byte | (x >> self.used_bits);
        self.buffer.push(x_shift);

        self.current_byte = x << (8 - self.used_bits);
    }

    fn next_word(&mut self) {
        self.buffer.push(self.current_byte);

        self.current_byte = 0;
        self.used_bits = 0;
    }

    fn write_blk(&mut self, arr: &[u8], src_ptr: &mut usize) {
        let src_len = arr.len() - *src_ptr;
        let blk_len = src_len.min(255);
        self.buffer.push(blk_len as u8);
        if blk_len == 0 {
            return;
        }

        self.buffer.extend(&arr[*src_ptr..blk_len]);

        *src_ptr += blk_len;
        self.write_blk(arr, src_ptr);
    }

    fn word(&mut self, c: u32) {
        loop {
            let mut w = (c & 127) as u8;
            let c = c >> 7;

            if c != 0 {
                w |= 128;
            }
            self.bits(8, w);

            if c == 0 {
                break;
            }
        }
    }

    fn bits(&mut self, num_bits: i64, val: u8) {
        self.used_bits += num_bits;
        let unused_bits = 8 - self.used_bits;
        match unused_bits {
            x if x > 0 => {
                self.current_byte |= val << x;
            }
            x if x == 0 => {
                self.current_byte |= val;
                self.next_word();
            }
            x => {
                let used = -x;
                self.current_byte |= val >> used;
                self.next_word();
                self.current_byte = val << (8 - used);
                self.used_bits = used;
            }
        }
    }

    pub(crate) fn filler(&mut self) {
        self.current_byte |= 1;
        self.next_word();
    }
}
