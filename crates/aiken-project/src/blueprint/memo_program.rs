use aiken_lang::{
    ast::TypedValidator,
    gen_uplc::{CodeGenerator, Error},
};
use uplc::ast::{DeBruijn, Program};

#[derive(Default)]
pub struct MemoProgram {
    program: Option<Program<DeBruijn>>,
}

impl MemoProgram {
    pub fn get(
        &mut self,
        generator: &mut CodeGenerator,
        def: &TypedValidator,
        module_name: &str,
    ) -> Result<Program<DeBruijn>, Error> {
        match self.program.take() {
            None => {
                let new_program = generator.generate(def, module_name)?.to_debruijn().unwrap();

                self.program.replace(new_program.clone());

                Ok(new_program)
            }
            Some(program) => Ok(program),
        }
    }
}
