use flat_rs::{decode, encode};

#[cfg(test)]
mod test {
    #[test]
    fn encode_bool() {
        let bytes = crate::encode(&true).unwrap();

        assert_eq!(bytes, vec![0b10000001]);

        let decoded: bool = crate::decode(bytes.as_slice()).unwrap();

        assert_eq!(decoded, true);

        let bytes = crate::encode(&false).unwrap();

        assert_eq!(bytes, vec![0b00000001]);

        let decoded: bool = crate::decode(bytes.as_slice()).unwrap();

        assert_eq!(decoded, false);
    }

    #[test]
    fn encode_u8() {
        let bytes = crate::encode(&3_u8).unwrap();

        assert_eq!(bytes, vec![0b00000011, 0b00000001]);

        let decoded: u8 = crate::decode(bytes.as_slice()).unwrap();

        assert_eq!(decoded, 3_u8);
    }
}
