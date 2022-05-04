use neptune::cli::Cli;
use neptune::parser;

fn main() -> anyhow::Result<()> {
    let args = Cli::default();

    let code = std::fs::read_to_string(&args.input)?;

    let program = parser::program(&code)?;

    println!("{:#?}", program);

    Ok(())
}
