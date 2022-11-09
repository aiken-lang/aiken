use indoc::{formatdoc, indoc};
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
    scripts: PathBuf,
    project: PathBuf,
    project_name: String,
}

impl Creator {
    fn new(args: Args) -> Self {
        let root = args.name;
        let src = root.join("src");
        let scripts = src.join("scripts");
        let project_name = root.clone().into_os_string().into_string().unwrap();
        let project = src.join(&project_name);
        Self {
            root,
            src,
            scripts,
            project,
            project_name,
        }
    }

    fn run(&self) -> miette::Result<()> {
        fs::create_dir_all(&self.src).into_diagnostic()?;
        fs::create_dir_all(&self.scripts).into_diagnostic()?;
        fs::create_dir_all(&self.project).into_diagnostic()?;

        self.aiken_toml()?;
        self.context()?;
        self.project()?;
        self.always_true_script()?;

        Ok(())
    }

    fn always_true_script(&self) -> miette::Result<()> {
        write(
            self.scripts.join("always_true.ak"),
            indoc! {"
                pub fn spend() -> Bool {
                    True
                }            
            "},
        )
    }

    fn project(&self) -> miette::Result<()> {
        write(
            self.src.join(format!("{}.ak", self.project_name)),
            indoc! {"
                pub type Datum {
                    something: String,
                }          
            "},
        )
    }

    fn context(&self) -> miette::Result<()> {
        write(
            self.project.join("context.ak"),
            indoc! {"
                pub type ScriptContext(purpose) {
                    tx_info: TxInfo,
                    script_purpose: purpose,
                }
                
                pub type TxInfo {
                    idk: Int,
                }       
            "},
        )
    }

    fn aiken_toml(&self) -> miette::Result<()> {
        write(
            self.root.join("aiken.toml"),
            &formatdoc! {
                r#"name = "{name}"
                version = "0.1.0"
                licences = ["Apache-2.0"]
                description = "Aiken contracts"

                [dependencies]

                "#,
                name = self.project_name,
            },
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
