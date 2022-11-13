use indoc::{formatdoc, indoc};
use miette::IntoDiagnostic;
use std::fs;
use std::io::Write;
use std::path::PathBuf;

use super::error::{Error, InvalidProjectNameReason};

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
    fn new(args: Args, project_name: String) -> Self {
        let root = args.name;
        let src = root.join("src");
        let scripts = src.join("scripts");
        let project_name = project_name;
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

fn validate_name(name: &str) -> Result<(), Error> {
    if name.starts_with("aiken_") {
        Err(Error::InvalidProjectName {
            name: name.to_string(),
            reason: InvalidProjectNameReason::AikenPrefix,
        })
    } else if name == "aiken" {
        Err(Error::InvalidProjectName {
            name: name.to_string(),
            reason: InvalidProjectNameReason::AikenReservedModule,
        })
    } else if !regex::Regex::new("^[a-z][a-z0-9_]*$")
        .expect("new name regex could not be compiled")
        .is_match(name)
    {
        Err(Error::InvalidProjectName {
            name: name.to_string(),
            reason: InvalidProjectNameReason::Format,
        })
    } else {
        Ok(())
    }
}

pub fn exec(args: Args) -> miette::Result<()> {
    if !args.name.exists() {
        let project_name = args.name.clone().into_os_string().into_string().unwrap();

        validate_name(&project_name).into_diagnostic()?;

        let creator = Creator::new(args, project_name);
        creator.run()?;
    }

    Ok(())
}
