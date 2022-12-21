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

pub fn to_u128(x: i128) -> u128 {
    let double_x = x << 1;

    if x.is_positive() || x == 0 {
        double_x as u128
    } else {
        (-double_x - 1) as u128
    }
}

pub fn to_i128(u: u128) -> i128 {
    ((u >> 1) as i128) ^ (-((u & 1) as i128))
}
