#[cfg(test)]
extern crate quickcheck;

#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;

#[cfg(test)]
mod test {
    use flat_rs::zigzag::{to_isize, to_usize};

    #[quickcheck]
    fn zigzag(i: isize) -> bool {
        let u = to_usize(i);
        let converted_i = to_isize(u);
        converted_i == i
    }

    #[quickcheck]
    fn zagzig(u: usize) -> bool {
        let i = to_isize(u);
        let converted_u = to_usize(i);
        converted_u == u
    }
}
