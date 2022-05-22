pub enum Filler {
    FillerStart(Box<Filler>),
    FillerEnd,
}

impl Filler {
    pub fn len(&self) -> usize {
        match self {
            Filler::FillerStart(f) => f.len() + 1,
            Filler::FillerEnd => 1,
        }
    }
}
