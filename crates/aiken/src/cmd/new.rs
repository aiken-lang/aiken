use aiken_project::config::PackageName;
use indoc::{formatdoc, indoc};
use miette::IntoDiagnostic;
use owo_colors::OwoColorize;
use std::path::PathBuf;
use std::{fs, path::Path};

use super::error::{Error, InvalidProjectNameReason};

#[derive(clap::Args)]
/// Create a new Aiken project
pub struct Args {
    /// Project name
    name: String,
    /// Library only
    lib: bool,
}

pub fn exec(args: Args) -> miette::Result<()> {
    validate_name(&args.name)?;

    let package_name = package_name_from_str(&args.name)?;

    create_project(args, &package_name)?;

    print_success_message(&package_name);

    Ok(())
}

fn create_project(args: Args, package_name: &PackageName) -> miette::Result<()> {
    let root = PathBuf::from(&package_name.repo);

    if root.exists() {
        Err(Error::ProjectExists {
            name: package_name.repo.clone(),
        })?;
    }

    create_lib(&root, package_name)?;

    if !args.lib {
        create_validators(&root, package_name)?;
    }

    aiken_toml(&root, package_name)?;

    readme(&root, &package_name.repo)?;

    gitignore(&root)?;

    Ok(())
}

fn print_success_message(package_name: &PackageName) {
    println!(
        "{}",
        formatdoc! {
            r#"
                Your Aiken project {name} has been {s} created.
                The project can be compiled and tested by running these commands:

                    {cd} {name}
                    {aiken} check
            "#,
            s = "successfully".bold().bright_green(),
            cd = "cd".bold().purple(),
            name = package_name.repo.bright_blue(),
            aiken = "aiken".bold().purple(),
        }
    )
}

fn create_lib(root: &Path, package_name: &PackageName) -> miette::Result<()> {
    let lib = root.join("lib");

    fs::create_dir_all(&lib).into_diagnostic()?;

    let lib_module_path = lib.join(format!("{}.ak", package_name.repo));

    fs::write(
        lib_module_path,
        indoc! {
            r#"
                pub fn hello(_name: String) -> Bool {
                  trace("hello")

                  True
                }

                test hello_1() {
                  hello("Human")
                }
            "#
        },
    )
    .into_diagnostic()?;

    let nested_path = lib.join(&package_name.repo);

    fs::create_dir_all(&nested_path).into_diagnostic()?;

    let nested_module_path = nested_path.join("util.ak");

    fs::write(
        nested_module_path,
        indoc! {
            r#"
                pub fn is_one(a: Int) -> Bool {
                  trace("hi")

                  a == 1
                }

                test is_one_1() {
                  is_one(1)
                }
            "#
        },
    )
    .into_diagnostic()?;

    Ok(())
}

fn create_validators(root: &Path, package_name: &PackageName) -> miette::Result<()> {
    let validators = root.join("validators");

    fs::create_dir_all(&validators).into_diagnostic()?;

    let always_true_path = validators.join("always_true.ak");

    fs::write(
        always_true_path,
        formatdoc! {
            r#"
                use aiken/transaction.{{ScriptContext}}
                use {name}
                use {name}/util

                pub fn spend(_datum: Data, _redeemer: Data, _context: ScriptContext) -> Bool {{
                  {name}.hello("World") && util.is_one(1)
                }}
            "#,
            name = package_name.repo
        },
    )
    .into_diagnostic()?;

    Ok(())
}

fn aiken_toml(root: &Path, package_name: &PackageName) -> miette::Result<()> {
    let aiken_toml_path = root.join("aiken.toml");

    fs::write(
        aiken_toml_path,
        formatdoc! {
            r#"
                name = "{name}"
                version = "0.0.0"
                licences = ["Apache-2.0"]
                description = "Aiken contracts for project '{name}'"

                dependencies = [
                  {{ name = "aiken-lang/stdlib", version = "main", source = "github" }},
                ]
            "#,
            name = package_name.to_string(),
        },
    )
    .into_diagnostic()
}

fn readme(root: &Path, project_name: &str) -> miette::Result<()> {
    fs::write(
        root.join("README.md"),
        formatdoc! {
            r#"
                # {name}

                Write validators in the `validators` folder, and supporting functions in the `lib` folder using `.ak` as a file extension.

                For example, as `validators/always_true.ak`

                ```gleam
                pub fn spend(_datum: Data, _redeemer: Data, _context: Data) -> Bool {{
                  True
                }}
                ```

                Validators are named after their purpose, so one of:

                - `script`
                - `mint`
                - `withdraw`
                - `certify`

                ## Building

                ```sh
                aiken build
                ```

                ## Testing

                You can write tests in any module using the `test` keyword. For example:

                ```gleam
                test foo() {{
                  1 + 1 == 2
                }}
                ```

                To run all tests, simply do:

                ```sh
                aiken check
                ```

                To run only tests matching the string `foo`, do:

                ```sh
                aiken check -m foo
                ```

                ## Documentation

                If you're writing a library, you might want to generate an HTML documentation for it.

                Use:

                ```sh
                aiken docs
                ```

                ## Resources

                Find more on the [Aiken's user manual](https://aiken-lang.org).
            "#,
            name = project_name
        },
    ).into_diagnostic()
}

fn gitignore(root: &Path) -> miette::Result<()> {
    let gitignore_path = root.join(".gitignore");

    fs::write(
        gitignore_path,
        indoc! {
            r#"
                build/
            "#
        },
    )
    .into_diagnostic()?;

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
    } else if !regex::Regex::new("^[a-z0-9_-]+/[a-z0-9_-]+$")
        .expect("regex could not be compiled")
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

fn package_name_from_str(name: &str) -> Result<PackageName, Error> {
    let mut name_split = name.split('/');

    let owner = name_split
        .next()
        .ok_or_else(|| Error::InvalidProjectName {
            name: name.to_string(),
            reason: InvalidProjectNameReason::Format,
        })?
        .to_string();

    let repo = name_split
        .next()
        .ok_or_else(|| Error::InvalidProjectName {
            name: name.to_string(),
            reason: InvalidProjectNameReason::Format,
        })?
        .to_string();

    Ok(PackageName { owner, repo })
}
