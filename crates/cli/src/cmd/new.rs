use miette::IntoDiagnostic;
use std::fs;
use std::io::Write;
use std::path::PathBuf;

#[derive(clap::Args)]
/// Create a new Aiken project
pub struct Args {
    /// Project name
    name: PathBuf,
}

pub struct Creator {
    root: PathBuf,
    src: PathBuf,
    project_name: String,
}

impl Creator {
    fn new(args: Args) -> Self {
        let root = args.name;
        let src = root.join("src");
        let project_name = root.clone().into_os_string().into_string().unwrap();
        Self{
            root,
            src,
            project_name
        }
    }

    fn run(&self) -> miette::Result<()> {
        fs::create_dir_all(&self.src).into_diagnostic()?;

        self.aiken_toml()?;
        self.always_true_script()?;

        Ok(())
    }

    fn always_true_script(&self) -> miette::Result<()> {
        write(
            self.src.join("always_true.ak"), 
            "
pub fn spend() -> Bool {
    true
}            
"
        )
    }

    fn aiken_toml(&self) -> miette::Result<()> {
        write(
            self.root.join("aiken.toml"),
            &format!(
                r#"name = "{name}"
version = "0.1.0"
licences = ["Apache-2.0"]
description = "Aiken contracts"

[dependencies]
aiken = "~> {aiken}"

"#,
                name = self.project_name,
                aiken = "0.0.24",
            ),
        )
    }
}

fn write(path: PathBuf, contents: &str) -> miette::Result<()> {
    let mut f = fs::File::create(&path).into_diagnostic()?;

    f.write_all(contents.as_bytes()).into_diagnostic()?;
    
    Ok(())
}

pub fn exec(args: Args) -> miette::Result<()> {
    if !args.name.exists() {
        let creator = Creator::new(args);
        creator.run()?;
    } 

    Ok(())
}
