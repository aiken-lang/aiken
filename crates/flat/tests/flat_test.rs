#[cfg(test)]
extern crate quickcheck;

#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;

#[cfg(test)]
mod test {
    use flat_rs::filler::Filler;
    use flat_rs::{decode, encode};
    use quickcheck::Arbitrary;

    #[test]
    fn encode_bool() {
        let bytes = encode(&true).unwrap();

        assert_eq!(bytes, vec![0b10000001]);

        let decoded: bool = decode(bytes.as_slice()).unwrap();

        assert_eq!(decoded, true);

        let bytes = encode(&false).unwrap();

        assert_eq!(bytes, vec![0b00000001]);

        let decoded: bool = decode(bytes.as_slice()).unwrap();

        assert_eq!(decoded, false);
    }

    #[test]
    fn encode_u8() {
        let bytes = encode(&3_u8).unwrap();

        assert_eq!(bytes, vec![0b00000011, 0b00000001]);

        let decoded: u8 = decode(bytes.as_slice()).unwrap();

        assert_eq!(decoded, 3_u8);
    }

    #[quickcheck]
    fn encode_isize(x: isize) -> bool {
        let bytes = encode(&x).unwrap();
        let decoded: isize = decode(&bytes).unwrap();
        decoded == x
    }

    #[quickcheck]
    fn encode_usize(x: usize) -> bool {
        let bytes = encode(&x).unwrap();
        let decoded: usize = decode(&bytes).unwrap();
        decoded == x
    }

    #[quickcheck]
    fn encode_char(c: char) -> bool {
        let bytes = encode(&c).unwrap();
        let decoded: char = decode(&bytes).unwrap();
        decoded == c
    }

    #[quickcheck]
    fn encode_string(str: String) -> bool {
        let bytes = encode(&str).unwrap();
        let decoded: String = decode(&bytes).unwrap();
        decoded == str
    }

    #[quickcheck]
    fn encode_vec_u8(xs: Vec<u8>) -> bool {
        let bytes = encode(&xs).unwrap();
        let decoded: Vec<u8> = decode(&bytes).unwrap();
        decoded == xs
    }

    #[derive(Clone, Debug)]
    struct BigChunk(Vec<u8>);

    impl Arbitrary for BigChunk {
        fn arbitrary(g: &mut quickcheck::Gen) -> Self {
            let num_of_element = g.choose(&[257]).unwrap();

            let vec = (0..*num_of_element).map(|_| u8::arbitrary(g)).collect();

            BigChunk(vec)
        }
    }

    #[quickcheck]
    fn encode_big_vec_u8(xs: BigChunk) -> bool {
        let xs = xs.0;
        let bytes = encode(&xs).unwrap();
        let decoded: Vec<u8> = decode(&bytes).unwrap();
        decoded == xs
    }

    #[quickcheck]
    fn encode_arr_u8(xs: Vec<u8>) -> bool {
        let bytes = encode(&xs.as_slice()).unwrap();
        let decoded: Vec<u8> = decode(&bytes).unwrap();
        decoded == xs
    }

    #[quickcheck]
    fn encode_big_arr_u8(xs: BigChunk) -> bool {
        let xs = xs.0;
        let bytes = encode(&xs.as_slice()).unwrap();
        let decoded: Vec<u8> = decode(&bytes).unwrap();
        decoded == xs
    }

    #[quickcheck]
    fn encode_boxed(c: char) -> bool {
        let boxed = Box::new(c);
        let bytes = encode(&boxed).unwrap();
        let decoded: char = decode(&bytes).unwrap();
        decoded == c
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
