use crate::{decode::Decode, zigzag};

pub struct Decoder<'b> {
    buffer: &'b [u8],
    used_bits: i64,
    pos: usize,
}

impl<'b> Decoder<'b> {
    pub fn new(bytes: &'b [u8]) -> Decoder {
        Decoder {
            buffer: bytes,
            pos: 0,
            used_bits: 0,
        }
    }

    pub fn decode<T: Decode<'b>>(&mut self) -> Result<T, String> {
        T::decode(self)
    }

    pub fn integer(&mut self) -> Result<isize, String> {
        Ok(zigzag::to_isize(self.word()?))
    }

    pub fn bool(&mut self) -> Result<bool, String> {
        let current_byte = self.buffer[self.pos];
        let b = 0 != (current_byte & (128 >> self.used_bits));
        self.increment_buffer_by_bit();
        Ok(b)
    }

    pub fn u8(&mut self) -> Result<u8, String> {
        self.bits8(8)
    }

    pub fn bytes(&mut self) -> Result<Vec<u8>, String> {
        self.filler()?;
        self.byte_array()
    }

    pub fn char(&mut self) -> Result<char, String> {
        Ok(char::from_u32(self.word()? as u32).unwrap())
    }

    pub fn string(&mut self) -> Result<String, String> {
        let mut s = String::new();
        while self.bit()? {
            s += &self.char()?.to_string();
        }
        Ok(s)
    }

    pub fn filler(&mut self) -> Result<(), String> {
        while self.zero()? {}
        Ok(())
    }

    pub fn word(&mut self) -> Result<usize, String> {
        let mut leading_bit = 1;
        let mut final_word: usize = 0;
        let mut shl: usize = 0;
        // continue looping if lead bit is 1 otherwise exit
        while leading_bit == 1 {
            let word8 = self.bits8(8)?;
            let word7 = word8 & 127;
            final_word |= (word7 as usize) << shl;
            shl += 7;
            leading_bit = word8 & 128;
        }
        Ok(final_word)
    }

    pub fn decode_list_with<T: Decode<'b>>(
        &mut self,
        decoder_func: for<'r> fn(&'r mut Decoder) -> Result<T, String>,
    ) -> Result<Vec<T>, String> {
        let mut vec_array: Vec<T> = Vec::new();
        while self.bit()? {
            vec_array.push(decoder_func(self)?)
        }
        Ok(vec_array)
    }

    fn zero(&mut self) -> Result<bool, String> {
        let current_bit = self.bit()?;
        Ok(!current_bit)
    }

    fn bit(&mut self) -> Result<bool, String> {
        if self.pos >= self.buffer.len() {
            return Err("DecoderState: Reached end of buffer".to_string());
        }
        let b = self.buffer[self.pos] & (128 >> self.used_bits) > 0;
        self.increment_buffer_by_bit();
        Ok(b)
    }

    fn byte_array(&mut self) -> Result<Vec<u8>, String> {
        if self.used_bits != 0 {
            return Err("DecoderState.byteArray: Buffer is not byte aligned".to_string());
        }
        self.ensure_bytes(1)?;
        let mut blk_len = self.buffer[self.pos];
        self.pos += 1;
        let mut blk_array: Vec<u8> = Vec::new();
        while blk_len != 0 {
            self.ensure_bytes(blk_len as usize + 1)?;
            let decoded_array = &self.buffer[self.pos..self.pos + blk_len as usize];
            blk_array.extend(decoded_array);
            self.pos += blk_len as usize;
            blk_len = self.buffer[self.pos];
        }
        Ok(blk_array)
    }

    // can decode up to a max of 8 bits
    pub fn bits8(&mut self, num_bits: usize) -> Result<u8, String> {
        if num_bits > 8 {
            return Err(
                "Decoder.bits8: incorrect value of num_bits - must be less than 9".to_string(),
            );
        }
        self.ensure_bits(num_bits)?;
        let unused_bits = 8 - self.used_bits as usize;
        let leading_zeroes = 8 - num_bits;
        let r = (self.buffer[self.pos] << self.used_bits as usize) >> leading_zeroes;

        let x = if num_bits > unused_bits {
            r | (self.buffer[self.pos + 1] >> (unused_bits + leading_zeroes))
        } else {
            r
        };
        self.drop_bits(num_bits);
        Ok(x)
    }

    fn ensure_bytes(&mut self, required_bytes: usize) -> Result<(), String> {
        if required_bytes as isize > self.buffer.len() as isize - self.pos as isize {
            return Err(format!(
                "DecoderState: Not enough data available: {:#?} - required bytes {}",
                self.buffer, required_bytes
            ));
        }
        Ok(())
    }

    fn ensure_bits(&mut self, required_bits: usize) -> Result<(), String> {
        if required_bits as isize
            > (self.buffer.len() as isize - self.pos as isize) * 8 - self.used_bits as isize
        {
            return Err(format!(
                "DecoderState: Not enough data available: {:#?} - required bits {}",
                self.buffer, required_bits
            ));
        }
        Ok(())
    }

    fn drop_bits(&mut self, num_bits: usize) {
        let all_used_bits = num_bits as i64 + self.used_bits;
        self.used_bits = all_used_bits % 8;
        self.pos += all_used_bits as usize / 8;
    }

    fn increment_buffer_by_bit(&mut self) {
        if self.used_bits == 7 {
            self.pos += 1;
            self.used_bits = 0;
        } else {
            self.used_bits += 1;
        }
    }
}
