pub enum Filler {
    FillerStart(Box<Filler>),
    FillerEnd,
}

impl Filler {
    pub fn length(&self) -> usize {
        match self {
            Filler::FillerStart(f) => f.length() + 1,
            Filler::FillerEnd => 1,
        }
    }
}
