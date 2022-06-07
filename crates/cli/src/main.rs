use uplc::{
    ast::{DeBruijn, FakeNamedDeBruijn, Program},
    parser,
};

use neptune::{Cli, UplcCommand};

fn main() -> anyhow::Result<()> {
    let args = Cli::default();

    match args {
        Cli::Uplc(uplc) => match uplc {
            UplcCommand::Flat { input } => {
                let code = std::fs::read_to_string(&input)?;

                let program = parser::program(&code)?;

                let program = Program::<DeBruijn>::try_from(program)?;

                let bytes = program.to_flat()?;

                for (i, byte) in bytes.iter().enumerate() {
                    print!("{:08b}", byte);

                    if (i + 1) % 4 == 0 {
                        println!();
                    } else {
                        print!(" ");
                    }
                }

                println!();
            }
            UplcCommand::Unflat { input } => {
                let bytes = std::fs::read(&input)?;

                let program = Program::<FakeNamedDeBruijn>::from_flat(&bytes)?;

                println!("{:#?}", program);
            }
        },
    }

    Ok(())
}
