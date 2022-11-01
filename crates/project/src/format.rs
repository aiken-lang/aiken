use std::{
    fs,
    io::Read,
    path::{Path, PathBuf},
    str::FromStr,
};

use aiken_lang::{ast::ModuleKind, parser};

use crate::{
    error::{Error, Unformatted},
    is_aiken_path,
};

pub fn run(stdin: bool, check: bool, files: Vec<String>) -> Result<(), Error> {
    if stdin {
        process_stdin(check)
    } else {
        process_files(check, files)
    }
}

fn process_stdin(check: bool) -> Result<(), Error> {
    let src = read_stdin()?;

    let mut out = String::new();

    let (module, extra) = parser::module(&src, ModuleKind::Lib)
        .map_err(|errs| Error::from_parse_errors(errs, Path::new("<stdin>"), &src))?;

    aiken_lang::format::pretty(&mut out, module, extra, &src);

    if !check {
        print!("{}", out);
        return Ok(());
    }

    if src != out {
        return Err(Error::Format {
            problem_files: vec![Unformatted {
                source: PathBuf::from("<standard input>"),
                destination: PathBuf::from("<standard output>"),
                input: src,
                output: out,
            }],
        });
    }

    Ok(())
}

fn process_files(check: bool, files: Vec<String>) -> Result<(), Error> {
    if check {
        check_files(files)
    } else {
        format_files(files)
    }
}

fn check_files(files: Vec<String>) -> Result<(), Error> {
    let problem_files = unformatted_files(files)?;

    if problem_files.is_empty() {
        Ok(())
    } else {
        Err(Error::Format { problem_files })
    }
}

fn format_files(files: Vec<String>) -> Result<(), Error> {
    for file in unformatted_files(files)? {
        fs::write(file.destination, file.output)?;
    }

    Ok(())
}

fn unformatted_files(files: Vec<String>) -> Result<Vec<Unformatted>, Error> {
    let mut problem_files = Vec::with_capacity(files.len());
    let mut errors = Vec::new();

    for file_path in files {
        let path = PathBuf::from_str(&file_path).unwrap();

        if path.is_dir() {
            for path in aiken_files_excluding_gitignore(&path) {
                if let Err(err) = format_file(&mut problem_files, path) {
                    errors.push(err);
                };
            }
        } else if let Err(err) = format_file(&mut problem_files, path) {
            println!("{:?}", err);
            errors.push(err);
        }
    }

    if errors.is_empty() {
        Ok(problem_files)
    } else {
        Err(Error::List(errors))
    }
}

fn format_file(problem_files: &mut Vec<Unformatted>, path: PathBuf) -> Result<(), Error> {
    let src = fs::read_to_string(&path).map_err(|error| Error::FileIo {
        error,
        path: path.clone(),
    })?;

    let mut output = String::new();

    let (module, extra) = parser::module(&src, ModuleKind::Lib)
        .map_err(|errs| Error::from_parse_errors(errs, &path, &src))?;

    aiken_lang::format::pretty(&mut output, module, extra, &src);

    if src != output {
        problem_files.push(Unformatted {
            source: path.clone(),
            destination: path,
            input: src,
            output,
        });
    }

    Ok(())
}

pub fn read_stdin() -> Result<String, Error> {
    let mut src = String::new();

    std::io::stdin().read_to_string(&mut src)?;

    Ok(src)
}

pub fn aiken_files_excluding_gitignore(dir: &Path) -> impl Iterator<Item = PathBuf> + '_ {
    ignore::WalkBuilder::new(dir)
        .follow_links(true)
        .require_git(false)
        .build()
        .into_iter()
        .filter_map(Result::ok)
        .filter(|e| e.file_type().map(|t| t.is_file()).unwrap_or(false))
        .map(ignore::DirEntry::into_path)
        .filter(move |d| is_aiken_path(d, dir))
}
