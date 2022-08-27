#[cfg(test)]
mod test {
    use flat_rs::zigzag::{to_isize, to_usize};
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn zigzag(i: isize) {
            let u = to_usize(i);
            let converted_i = to_isize(u);
            assert_eq!(converted_i, i);
        }

        #[test]
        fn zagzig(u: usize) {
            let i = to_isize(u);
            let converted_u = to_usize(i);
            assert_eq!(converted_u, u);
        }
    }
}
