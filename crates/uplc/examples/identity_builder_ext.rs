use uplc::program_builder::{Builder, WithLambda, WithTerm, WithVar};

trait WithIdentity: WithTerm + WithLambda + WithVar {
    fn with_identity(self, name_str: &str) -> Self::Next {
        self.with_lambda(name_str).with_var(name_str)
    }
}

impl<T: WithTerm> WithIdentity for T {}

fn main() {
    let my_var = "some_var";
    let program = Builder::start(1, 2, 3).with_identity(my_var).build_named();
    println!("{:#?}", program);
}
