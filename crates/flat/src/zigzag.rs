pub fn to_usize(x: isize) -> usize {
    let double_x = x << 1;

    if x >= 0 {
        double_x as usize
    } else {
        (-double_x - 1) as usize
    }
}

pub fn to_isize(u: usize) -> isize {
    let s = u as isize;

    (s >> 1) ^ -(s & 1)
}

#[cfg(test)]
mod test {
    #[test]
    fn convert() {
        let n = -12;
        let unsigned = super::to_usize(n);
        let signed = super::to_isize(unsigned);

        assert_eq!(n, signed)
    }
}
