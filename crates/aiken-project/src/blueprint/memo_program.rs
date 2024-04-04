use aiken_lang::{ast::TypedValidator, gen_uplc::CodeGenerator};
use uplc::ast::{DeBruijn, Program};

pub struct MemoProgram {
    program: Option<Program<DeBruijn>>,
}

impl MemoProgram {
    pub fn new() -> Self {
        Self { program: None }
    }

    pub fn get(
        &mut self,
        generator: &mut CodeGenerator,
        def: &TypedValidator,
        module_name: &str,
    ) -> Program<DeBruijn> {
        match self.program.take() {
            None => {
                let new_program = generator.generate(def, module_name).to_debruijn().unwrap();

                self.program.replace(new_program.clone());

                new_program
            }
            Some(program) => program,
        }
    }
}
