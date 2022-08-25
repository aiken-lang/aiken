pub fn to_usize(x: isize) -> usize {
    let double_x = x << 1;

    if x.is_positive() || x == 0 {
        double_x as usize
    } else {
        (-double_x - 1) as usize
    }
}

pub fn to_isize(u: usize) -> isize {
    ((u >> 1) as isize) ^ (-((u & 1) as isize))
}
