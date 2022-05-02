use neptune::cli::Cli;

fn main() {
    let args = Cli::default();

    println!("loading {}", args.input.display());
}
