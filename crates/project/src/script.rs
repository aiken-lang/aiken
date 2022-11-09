use uplc::ast::{NamedDeBruijn, Program};

#[derive(Debug)]
pub struct Script {
    pub module: String,
    pub name: String,
    pub program: Program<NamedDeBruijn>,
}

impl Script {
    pub fn new(module: String, name: String, program: Program<NamedDeBruijn>) -> Script {
        Script {
            module,
            name,
            program,
        }
    }
}
