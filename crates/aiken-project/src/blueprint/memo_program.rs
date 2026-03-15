use aiken_lang::{
    ast::{SourceLocation, TypedValidator},
    gen_uplc::CodeGenerator,
};
use uplc::ast::{Name, Program};

#[derive(Default)]
pub struct MemoProgram {
    program: Option<Program<Name, SourceLocation>>,
}

impl MemoProgram {
    pub fn get(
        &mut self,
        generator: &mut CodeGenerator,
        def: &TypedValidator,
        module_name: &str,
    ) -> Program<Name, SourceLocation> {
        match self.program.take() {
            None => {
                let new_program = generator.generate(def, module_name);
                self.program.replace(new_program.clone());
                new_program
            }
            Some(program) => program,
        }
    }
}
