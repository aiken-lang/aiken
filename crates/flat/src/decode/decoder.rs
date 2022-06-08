use crate::{decode::Decode, zigzag};

use super::Error;

#[derive(Debug)]
pub struct Decoder<'b> {
    pub buffer: &'b [u8],
    pub used_bits: i64,
    pub pos: usize,
}

impl<'b> Decoder<'b> {
    pub fn new(bytes: &'b [u8]) -> Decoder {
        Decoder {
            buffer: bytes,
            pos: 0,
            used_bits: 0,
        }
    }

    /// Encode any type that implements [`Decode`].
    pub fn decode<T: Decode<'b>>(&mut self) -> Result<T, Error> {
        T::decode(self)
    }

    pub fn integer(&mut self) -> Result<isize, Error> {
        Ok(zigzag::to_isize(self.word()?))
    }

    pub fn bool(&mut self) -> Result<bool, Error> {
        let current_byte = self.buffer[self.pos];
        let b = 0 != (current_byte & (128 >> self.used_bits));
        self.increment_buffer_by_bit();
        Ok(b)
    }

    pub fn u8(&mut self) -> Result<u8, Error> {
        self.bits8(8)
    }

    pub fn bytes(&mut self) -> Result<Vec<u8>, Error> {
        self.filler()?;
        self.byte_array()
    }

    pub fn char(&mut self) -> Result<char, Error> {
        let character = self.word()? as u32;

        char::from_u32(character).ok_or(Error::DecodeChar(character))
    }

    pub fn string(&mut self) -> Result<String, Error> {
        let mut s = String::new();
        while self.bit()? {
            s += &self.char()?.to_string();
        }
        Ok(s)
    }

    pub fn utf8(&mut self) -> Result<String, Error> {
        // TODO: Better Error Handling
        String::from_utf8(Vec::<u8>::decode(self)?).map_err(Error::from)
    }

    pub fn filler(&mut self) -> Result<(), Error> {
        while self.zero()? {}
        Ok(())
    }

    pub fn word(&mut self) -> Result<usize, Error> {
        let mut leading_bit = 1;
        let mut final_word: usize = 0;
        let mut shl: usize = 0;
        // continue looping if lead bit is 1 otherwise exit
        while leading_bit > 0 {
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
        decoder_func: for<'r> fn(&'r mut Decoder) -> Result<T, Error>,
    ) -> Result<Vec<T>, Error> {
        let mut vec_array: Vec<T> = Vec::new();
        while self.bit()? {
            vec_array.push(decoder_func(self)?)
        }
        Ok(vec_array)
    }

    fn zero(&mut self) -> Result<bool, Error> {
        let current_bit = self.bit()?;

        Ok(!current_bit)
    }

    fn bit(&mut self) -> Result<bool, Error> {
        if self.pos >= self.buffer.len() {
            return Err(Error::EndOfBuffer);
        }

        let b = self.buffer[self.pos] & (128 >> self.used_bits) > 0;

        self.increment_buffer_by_bit();

        Ok(b)
    }

    fn byte_array(&mut self) -> Result<Vec<u8>, Error> {
        if self.used_bits != 0 {
            return Err(Error::BufferNotByteAligned);
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
        self.pos += 1;

        Ok(blk_array)
    }

    // can decode up to a max of 8 bits
    pub fn bits8(&mut self, num_bits: usize) -> Result<u8, Error> {
        if num_bits > 8 {
            return Err(Error::IncorrectNumBits);
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

    fn ensure_bytes(&mut self, required_bytes: usize) -> Result<(), Error> {
        if required_bytes as isize > self.buffer.len() as isize - self.pos as isize {
            Err(Error::NotEnoughBytes(required_bytes))
        } else {
            Ok(())
        }
    }

    fn ensure_bits(&mut self, required_bits: usize) -> Result<(), Error> {
        if required_bits as isize
            > (self.buffer.len() as isize - self.pos as isize) * 8 - self.used_bits as isize
        {
            Err(Error::NotEnoughBits(required_bits))
        } else {
            Ok(())
        }
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
