use super::*;
use crate::parser;
use crate::program_builder::constant::WithConstant;
use quickcheck::{Arbitrary, Gen};

// #[derive(Clone, Debug)]
// struct Version {
//     maj: isize,
//     min: isize,
//     patch: isize,
// }
//
// impl Version {
//     fn inner(&self) -> (isize, isize, isize) {
//         (self.maj, self.min, self.patch)
//     }
// }
//
// impl Arbitrary for Version {
//     fn arbitrary(g: &mut Gen) -> Self {
//         let maj = isize::arbitrary(g).unsigned_abs() as isize;
//         let min = isize::arbitrary(g).unsigned_abs() as isize;
//         let patch = isize::arbitrary(g).unsigned_abs() as isize;
//         Version { maj, min, patch }
//     }
// }

#[derive(Clone, Debug)]
struct Version {
    maj: usize,
    min: usize,
    patch: usize,
}

impl Version {
    fn inner(&self) -> (usize, usize, usize) {
        (self.maj, self.min, self.patch)
    }
}

impl Arbitrary for Version {
    fn arbitrary(g: &mut Gen) -> Self {
        let maj = isize::arbitrary(g).unsigned_abs();
        let min = isize::arbitrary(g).unsigned_abs();
        let patch = isize::arbitrary(g).unsigned_abs();
        Version { maj, min, patch }
    }
}

#[quickcheck]
fn build_named__with_version(version: Version) -> bool {
    let (maj, min, patch) = version.inner();
    let code = format!(
        r"(program
             {}.{}.{}
             (con integer 11)
           )",
        maj, min, patch
    );
    let expected = parser::program(&code).unwrap();
    let actual = Builder::start(maj as usize, min as usize, patch as usize)
        .with_int(11)
        .build_named();
    expected == actual
}
