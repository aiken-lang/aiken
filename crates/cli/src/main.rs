use uplc::parser;

use neptune::Cli;

fn main() -> anyhow::Result<()> {
    let args = Cli::default();

    let code = std::fs::read_to_string(&args.input)?;

    let program = parser::program(&code)?;

    println!("{:#?}", program);

    let flat_bytes = program.to_flat()?;

    for byte in flat_bytes {
        print!("{:08b} ", byte);
    }

    println!();

    println!("{}", program.flat_hex()?);

    Ok(())
}
