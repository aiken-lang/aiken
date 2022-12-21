#[cfg(test)]
mod test {
    use flat_rs::filler::Filler;
    use flat_rs::{decode, encode};
    use proptest::prelude::*;

    prop_compose! {
        fn arb_big_vec()(size in 255..300, element in any::<u8>()) -> Vec<u8> {
            (0..size).map(|_| element).collect()
        }
    }

    #[test]
    fn encode_bool() {
        let bytes = encode(&true).unwrap();

        assert_eq!(bytes, vec![0b10000001]);

        let decoded: bool = decode(bytes.as_slice()).unwrap();

        assert!(decoded);

        let bytes = encode(&false).unwrap();

        assert_eq!(bytes, vec![0b00000001]);

        let decoded: bool = decode(bytes.as_slice()).unwrap();

        assert!(!decoded);
    }

    #[test]
    fn encode_u8() {
        let bytes = encode(&3_u8).unwrap();

        assert_eq!(bytes, vec![0b00000011, 0b00000001]);

        let decoded: u8 = decode(bytes.as_slice()).unwrap();

        assert_eq!(decoded, 3_u8);
    }

    proptest! {
        #[test]
        fn encode_isize(x: isize) {
            let bytes = encode(&x).unwrap();
            let decoded: isize = decode(&bytes).unwrap();
            assert_eq!(decoded, x);
        }

        #[test]
        fn encode_usize(x: usize) {
            let bytes = encode(&x).unwrap();
            let decoded: usize = decode(&bytes).unwrap();
            assert_eq!(decoded, x);
        }

        #[test]
        fn encode_char(c: char) {
            let bytes = encode(&c).unwrap();
            let decoded: char = decode(&bytes).unwrap();
            assert_eq!(decoded, c);
        }

        #[test]
        fn encode_string(str: String) {
            let bytes = encode(&str).unwrap();
            let decoded: String = decode(&bytes).unwrap();
            assert_eq!(decoded, str);
        }

        #[test]
        fn encode_vec_u8(xs: Vec<u8>) {
            let bytes = encode(&xs).unwrap();
            let decoded: Vec<u8> = decode(&bytes).unwrap();
            assert_eq!(decoded, xs);
        }

        #[test]
        fn encode_big_vec_u8(xs in arb_big_vec()) {
            let bytes = encode(&xs).unwrap();
            let decoded: Vec<u8> = decode(&bytes).unwrap();
            assert_eq!(decoded, xs);
        }

        #[test]
        fn encode_arr_u8(xs: Vec<u8>) {
            let bytes = encode(&xs.as_slice()).unwrap();
            let decoded: Vec<u8> = decode(&bytes).unwrap();
            assert_eq!(decoded, xs);
        }

        #[test]
        fn encode_big_arr_u8(xs in arb_big_vec()) {
            let bytes = encode(&xs.as_slice()).unwrap();
            let decoded: Vec<u8> = decode(&bytes).unwrap();
            assert_eq!(decoded, xs);
        }

        #[test]
        fn encode_boxed(c: char) {
            let boxed = Box::new(c);
            let bytes = encode(&boxed).unwrap();
            let decoded: char = decode(&bytes).unwrap();
            assert_eq!(decoded, c);
        }
    }

    #[test]
    fn encode_filler() {
        let bytes = encode(&Filler::FillerEnd).unwrap();

        assert_eq!(bytes, vec![0b0000001, 0b00000001]);

        let bytes = encode(&Filler::FillerStart(Box::new(Filler::FillerEnd))).unwrap();

        assert_eq!(bytes, vec![0b0000001, 0b00000001]);

        let bytes = encode(&Filler::FillerStart(Box::new(Filler::FillerStart(
            Box::new(Filler::FillerEnd),
        ))))
        .unwrap();

        assert_eq!(bytes, vec![0b0000001, 0b00000001]);
    }
}
