mod encode;
mod encoder;
mod filler;
pub mod zigzag;

pub mod en {
    pub use super::encode::*;
    pub use super::encoder::*;
}

pub fn encode<T>(value: T) -> Result<Vec<u8>, String>
where
    T: en::Encode,
{
    let mut e = en::Encoder::new();

    e.encode((value, filler::Filler::FillerEnd))?;

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

    #[test]
    fn encode_u8() {
        let bytes = super::encode(3_u8).unwrap();

        assert_eq!(bytes, vec![0b00000011, 0b00000001]);
    }
}
