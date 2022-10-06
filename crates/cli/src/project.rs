use std::{
    fs, io,
    path::{Path, PathBuf},
};

use aiken_lang::ast::ModuleKind;

use crate::config::Config;

#[derive(Debug)]
pub struct Source {
    pub path: PathBuf,
    pub name: String,
    pub code: String,
    pub kind: ModuleKind,
}

pub struct Project {
    config: Config,
    root: PathBuf,
    sources: Vec<Source>,
}

impl Project {
    pub fn new(config: Config, root: PathBuf) -> Project {
        Project {
            config,
            root,
            sources: vec![],
        }
    }

    pub fn build(&mut self) -> io::Result<()> {
        self.read_source_files()?;

        for source in &self.sources {
            println!("{:#?}", source);

            match aiken_lang::parser::script(&source.code) {
                Ok(_) => (),
                Err(errs) => {
                    for err in errs {
                        eprintln!("{:#?}", err);
                    }
                }
            }
        }

        Ok(())
    }

    fn read_source_files(&mut self) -> io::Result<()> {
        let lib = self.root.join("lib");
        let scripts = self.root.join("scripts");

        self.aiken_files(&scripts, ModuleKind::Script)?;
        self.aiken_files(&lib, ModuleKind::Lib)?;

        Ok(())
    }

    fn aiken_files(&mut self, dir: &Path, kind: ModuleKind) -> io::Result<()> {
        let paths = walkdir::WalkDir::new(dir)
            .follow_links(true)
            .into_iter()
            .filter_map(Result::ok)
            .filter(|e| e.file_type().is_file())
            .map(|d| d.into_path())
            .filter(move |d| is_aiken_path(d, dir));

        for path in paths {
            self.add_module(path, dir, kind)?;
        }

        Ok(())
    }

    fn add_module(&mut self, path: PathBuf, dir: &Path, kind: ModuleKind) -> io::Result<()> {
        let name = self.module_name(dir, &path);
        let code = fs::read_to_string(&path)?;

        self.sources.push(Source {
            name,
            code,
            kind,
            path,
        });

        Ok(())
    }

    fn module_name(&self, package_path: &Path, full_module_path: &Path) -> String {
        // ../../lib/module.ak

        // module.ak
        let mut module_path = full_module_path
            .strip_prefix(package_path)
            .expect("Stripping package prefix from module path")
            .to_path_buf();

        // module
        let _ = module_path.set_extension("");

        // Stringify
        let name = module_path
            .to_str()
            .expect("Module name path to str")
            .to_string();

        // normalise windows paths
        let name = name.replace('\\', "/");

        // project_name/module
        format!("{}/{}", self.config.name, name)
    }
}

fn is_aiken_path(path: &Path, dir: impl AsRef<Path>) -> bool {
    use regex::Regex;

    let re = Regex::new(&format!(
        "^({module}{slash})*{module}\\.ak$",
        module = "[a-z][_a-z0-9]*",
        slash = "(/|\\\\)",
    ))
    .expect("is_aiken_path() RE regex");

    re.is_match(
        path.strip_prefix(dir)
            .expect("is_aiken_path(): strip_prefix")
            .to_str()
            .expect("is_aiken_path(): to_str"),
    )
}
