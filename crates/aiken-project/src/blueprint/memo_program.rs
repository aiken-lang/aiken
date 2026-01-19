use aiken_lang::{ast::SourceLocation, ast::TypedValidator, gen_uplc::CodeGenerator};
use uplc::ast::{DeBruijn, Name, Program, Term};

#[derive(Default)]
pub struct MemoProgram {
    program: Option<Program<DeBruijn>>,
    term_with_spans: Option<Term<Name, SourceLocation>>,
}

impl MemoProgram {
    pub fn get(
        &mut self,
        generator: &mut CodeGenerator,
        def: &TypedValidator,
        module_name: &str,
    ) -> Program<DeBruijn> {
        match self.program.take() {
            None => {
                let (program, term_with_spans) = generator.generate_with_term(def, module_name);
                let new_program = program.to_debruijn().unwrap();

                self.program.replace(new_program.clone());
                self.term_with_spans.replace(term_with_spans);

                new_program
            }
            Some(program) => program,
        }
    }

    /// Get the term with source locations, if available.
    /// This is only available after `get()` has been called.
    pub fn get_term_with_spans(&self) -> Option<&Term<Name, SourceLocation>> {
        self.term_with_spans.as_ref()
    }
}
