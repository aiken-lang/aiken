use indoc::formatdoc;
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
    lib: PathBuf,
    validators: PathBuf,
    project_lib: PathBuf,
    project_name: String,
}

impl Creator {
    fn new(args: Args, project_name: String) -> Self {
        let root = args.name;
        let lib = root.join("lib");
        let validators = root.join("validators");
        let project_name = project_name;
        let project_lib = lib.join(&project_name);
        Self {
            root,
            lib,
            validators,
            project_lib,
            project_name,
        }
    }

    fn run(&self) -> miette::Result<()> {
        fs::create_dir_all(&self.lib).into_diagnostic()?;
        fs::create_dir_all(&self.project_lib).into_diagnostic()?;
        fs::create_dir_all(&self.validators).into_diagnostic()?;
        self.aiken_toml()?;
        self.readme()?;
        Ok(())
    }

    fn readme(&self) -> miette::Result<()> {
        write(
            self.root.join("README.md"),
            &format! {
            r#"# {name}

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

```console
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

```console
aiken check
```

To run only tests matching the string `foo`, do:

```console
aiken check -m foo
```

## Documentation

If you're writing a library, you might want to generate an HTML documentation for it.

Use:

```
aiken docs
```

## Resources

Find more on the [Aiken's user manual](https://aiken-lang.org).
"#,
            name = self.project_name
            },
        )
    }

    fn aiken_toml(&self) -> miette::Result<()> {
        write(
            self.root.join("aiken.toml"),
            &formatdoc! {
                r#"name = "{name}"
                version = "0.0.0"
                licences = ["Apache-2.0"]
                description = "Aiken contracts for project '{name}'"

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

        Creator::new(args, project_name).run()?;
    }

    Ok(())
}
