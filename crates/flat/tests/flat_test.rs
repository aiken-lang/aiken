#[cfg(test)]
extern crate quickcheck;

#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;

use flat_rs::{decode, encode};

#[cfg(test)]
mod test {
    use quickcheck::Arbitrary;

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

    #[quickcheck]
    fn encode_vec_u8(xs: Vec<u8>) -> bool {
        let bytes = crate::encode(&xs).unwrap();
        let decoded: Vec<u8> = crate::decode(&bytes).unwrap();
        decoded == xs
    }

    #[derive(Clone, Debug)]
    struct BigChunk(Vec<u8>);

    impl Arbitrary for BigChunk {
        fn arbitrary(g: &mut quickcheck::Gen) -> Self {
            let num_of_element = g.choose(&[255, 256, 244, 100]).unwrap();

            let mut vec = Vec::with_capacity(*num_of_element);
            for _ in 1..*num_of_element {
                vec.push(u8::arbitrary(g));
            }

            BigChunk(vec)
        }
    }

    #[quickcheck]
    fn encode_write_blk(xs: BigChunk) -> bool {
        let xs = xs.0;
        let bytes = crate::encode(&xs).unwrap();
        let decoded: Vec<u8> = crate::decode(&bytes).unwrap();
        decoded == xs
    }
}
