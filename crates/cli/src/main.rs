use uplc::{
    ast::{DeBruijn, NamedDeBruijn, Program},
    parser,
};

use neptune::Cli;

fn main() -> anyhow::Result<()> {
    let args = Cli::default();

    let code = std::fs::read_to_string(&args.input)?;

    let program = parser::program(&code)?;

    println!("\nName:");
    println!("{:#?}", program);

    let flat_bytes = program.to_flat()?;

    print!("\nflat bits:\n");

    for byte in flat_bytes {
        print!("{:08b} ", byte);
    }

    println!();

    let program_nd: Program<NamedDeBruijn> = program.try_into().unwrap();

    println!("\nNamed De Bruijn:");
    println!("{:#?}", program_nd);

    let flat_bytes = program_nd.to_flat()?;

    print!("\nflat bits:\n");

    for byte in flat_bytes {
        print!("{:08b} ", byte);
    }

    println!();

    let program_d: Program<DeBruijn> = program_nd.into();

    println!("\nDe Bruijn:");
    println!("{:#?}", program_d);

    let flat_bytes = program_d.to_flat()?;

    print!("\nflat bits:\n");

    for byte in flat_bytes {
        print!("{:08b} ", byte);
    }

    println!();

    Ok(())
}
