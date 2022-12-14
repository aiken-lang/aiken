use std::path::PathBuf;
use uplc::ast::{NamedDeBruijn, Program};

#[derive(Debug)]
pub struct Script {
    pub input_path: PathBuf,
    pub module: String,
    pub name: String,
    pub program: Program<NamedDeBruijn>,
}

impl Script {
    pub fn new(
        input_path: PathBuf,
        module: String,
        name: String,
        program: Program<NamedDeBruijn>,
    ) -> Script {
        Script {
            input_path,
            module,
            name,
            program,
        }
    }
}
