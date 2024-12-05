use crate::{cmd::Cmd as MainCmd, pretty};
use clap::{Command, Subcommand};
use clap_complete::{generate, Shell};
use std::{
    fmt::{self, Display},
    fs::{File, OpenOptions},
    io::Write,
    path::{Path, PathBuf},
};

/// Generates shell completion scripts
#[derive(clap::Args)]
pub struct Args {
    /// Install the completion scripts
    #[arg(short, long, default_value_t = false)]
    install: bool,
}

fn generate_wrapper(shell: Shell, buf: &mut dyn Write) {
    let cli = Command::new("aiken").disable_version_flag(true);

    let mut main = MainCmd::augment_subcommands(cli);

    generate(shell, &mut main, "aiken".to_string(), buf);
}

fn zsh() -> miette::Result<()> {
    //if oh-my-zsh
    let prefix_dir = "zsh-completions/site-functions";

    let home = std::env::var("HOME").expect("Environment variable 'HOME' not set but needed.");

    let xdg_dirs = xdg::BaseDirectories::with_prefix(prefix_dir)
        .expect("Could not find completion directory {prefix_dir} in xdg directories.");

    let data_home = xdg_dirs.get_data_home();

    let mut completion_file: File;

    let oh_my_zsh_path = Path::new(&home).join(".oh-my-zsh");

    if oh_my_zsh_path.exists() {
        pretty::say(Log::Detecting("oh-my-zsh!"));
        let completions_path = oh_my_zsh_path.join("completions");

        let aiken_completion_path = completions_path.join("_aiken");

        if !completions_path.exists() {
            std::fs::create_dir(completions_path.as_path()).expect(
                "Cannot create directory: {completions_path.into_os_string().into_string()}",
            );
        }
        pretty::say(Log::Installing(&aiken_completion_path));
        completion_file = File::create(aiken_completion_path)
            .expect("Cannot open file at: {aiken_completion_path.into_os_string().into_string()}");

        generate_wrapper(Shell::Zsh, &mut completion_file);

        return Ok(());
    }

    let home_exists = data_home.exists();

    let completion_path = xdg_dirs
        .place_data_file("_aiken")
        .expect("cannot create directory");
    pretty::say(Log::Installing(&completion_path));
    completion_file = File::create(completion_path)
        .expect("Cannot open file at: {completion_path.into_os_string().into_string()}");

    if home_exists {
        generate_wrapper(Shell::Zsh, &mut completion_file);
        return Ok(());
    }

    let mut zshrc = OpenOptions::new()
        .write(true)
        .append(true)
        .open(format!("{}/.zshrc", home))
        .expect(".zshrc file not found");

    pretty::say(Log::Adjusting(".zshrc"));

    if let Some(home) = data_home.to_str() {
        let fpath: String = format!(r#"fpath=($fpath "{}")"#, home);

        if let Err(e) = writeln!(zshrc, "{}", fpath) {
            eprintln!("Couldn't write to file: {}", e);
        }
    }

    generate_wrapper(Shell::Zsh, &mut completion_file);

    Ok(())
}

fn fish() -> miette::Result<()> {
    // NOTE: Installing completion on ~/.confi/fish/completions
    let prefix_dir = "fish/completions";

    let xdg_dirs = xdg::BaseDirectories::with_prefix(prefix_dir)
        .expect("Could not find completion directory {prefix_dir} in xdg directories.");

    let completion_path = xdg_dirs
        .place_config_file("aiken.fish")
        .expect("Cannot create path");

    pretty::say(Log::Installing(&completion_path));

    let mut completion_file = File::create(completion_path)
        .expect("Cannot open file at: {completion_path.into_os_string().into_string()}");

    generate_wrapper(Shell::Fish, &mut completion_file);

    Ok(())
}

fn bash() -> miette::Result<()> {
    let prefix_dir = "bash-completion/completions";

    let aiken_bash = "aiken.completion.bash";

    let xdg_dirs = xdg::BaseDirectories::with_prefix(prefix_dir)
        .expect("Could not find completion directory {prefix_dir} in xdg directories.");

    let home = std::env::var("HOME").expect("Environment variable 'HOME' not set but needed.");

    let config_home = xdg_dirs.get_config_home();

    let completion_path = xdg_dirs
        .place_config_file(aiken_bash)
        .expect("Cannot create completion file {aiken_bash} under xdg directories");
    pretty::say(Log::Installing(&completion_path));

    let mut bashrc = OpenOptions::new()
        .write(true)
        .append(true)
        .open(format!("{}/.bashrc", home))
        .expect(".bashrc file not found in {home} directory");

    if let Some(config) = config_home.to_str() {
        let path: String = format!("source {config}");

        pretty::say(Log::Adjusting(".bashrc"));

        if let Err(e) = writeln!(bashrc, "{}", path) {
            eprintln!("Couldn't write to file: {}", e);
        }
    }

    let mut completion_file = File::create(completion_path)
        .expect("Cannot open file at: {completion_path.into_os_string().into_string()}");

    generate_wrapper(Shell::Bash, &mut completion_file);

    Ok(())
}

fn completions_to_file(shell: Shell) -> miette::Result<()> {
    match shell {
        Shell::Bash => {
            bash()?;
        }
        Shell::Fish => {
            fish()?;
        }
        Shell::Zsh => {
            zsh()?;
        }
        s => eprintln!("{s} not supported"),
    }

    Ok(())
}

pub fn exec(cmd_args: Args, shell: Shell) -> miette::Result<()> {
    if cmd_args.install {
        completions_to_file(shell)?;
        pretty::say(Log::Done(&shell));
    } else {
        generate_wrapper(shell, &mut std::io::stdout());
    }

    Ok(())
}

enum Log<'a> {
    Detecting(&'a str),
    Installing(&'a PathBuf),
    Adjusting(&'a str),
    Done(&'a Shell),
}

impl Display for Log<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> std::result::Result<(), fmt::Error> {
        match self {
            Log::Detecting(what) => pretty::fmt_step(f, "Detecting", what),
            Log::Installing(path) => pretty::fmt_step(f, "Creating", &path.display()),
            Log::Adjusting(what) => pretty::fmt_step(f, "Adjusting", what),
            Log::Done(shell) => {
                pretty::fmt_step(f, "Done", &format!("installing {} auto-completion", shell))
            }
        }
    }
}
