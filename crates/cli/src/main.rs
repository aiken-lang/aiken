use uplc::{
    ast::{NamedDeBruijn, Program},
    parser,
};

use neptune::Cli;

fn main() -> anyhow::Result<()> {
    let args = Cli::default();

    let code = std::fs::read_to_string(&args.input)?;

    let program = parser::program(&code)?;

    println!("{:#?}", program);

    let flat_bytes = program.to_flat()?;

    print!("flat bits: ");

    for byte in flat_bytes {
        print!("{:08b} ", byte);
    }

    println!();

    let program: Program<NamedDeBruijn> = program.try_into().unwrap();

    println!("{:#?}", program);

    Ok(())
}
