use std::{fs::File, io::Write, path::Path};

use crate::cmd::Cmd as MainCmd;
use clap::{Command, Subcommand};
use clap_complete::{generate, Shell};
use std::fs::OpenOptions;

/// Generates shell completion scripts

#[derive(clap::Args)]
pub struct Args {
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
    let home = std::env::var("HOME").expect("Cannot find your home directory");
    let xdg_dirs = xdg::BaseDirectories::with_prefix("zsh-completions/site-functions").expect("Cannot load xdg-base directory");
    let data_home = xdg_dirs.get_data_home();
    let mut completion_file: File;

    let oh_my_zsh_path = Path::new(&home).join(".oh-my-zsh");

    if oh_my_zsh_path.exists() {
        let completions_path = oh_my_zsh_path.join("completions");
        let aiken_completion_path = completions_path.join("_aiken");
        if !completions_path.exists() {
            std::fs::create_dir(completions_path.as_path()).expect("Cannot create directory: {completions_path.into_os_string().into_string()}");
        }
        completion_file = File::create(aiken_completion_path).expect("Cannot open file at: {aiken_completion_path.into_os_string().into_string()}");

        generate_wrapper(Shell::Zsh, &mut completion_file);
        return Ok(());
    } 


    if data_home.exists() {
        let completion_path = xdg_dirs
            .place_data_file("_aiken")
            .expect("cannot create directory");
        completion_file = File::create(completion_path).expect("Cannot open file at: {completion_path.into_os_string().into_string()}");
        generate_wrapper(Shell::Zsh, &mut completion_file);
        return Ok(());
    } 

    let completion_path = xdg_dirs
        .place_data_file("_aiken")
        .expect("cannot create directory");
    completion_file = File::create(completion_path).expect("Cannot open file at: {completion_path.into_os_string().into_string()}");

    let mut zshrc = OpenOptions::new()
        .write(true)
        .append(true)
        .open(format!("{}/.zshrc", home))
        .expect(".zshrc file not found");
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
    let xdg_dirs = xdg::BaseDirectories::with_prefix("fish/completions").expect("Cannot find xdg-base directory");
    let completion_path = xdg_dirs
        .place_config_file("aiken.fish").expect("Cannot create path");
    let mut completion_file = File::create(completion_path).expect("Cannot open file at: {completion_path.into_os_string().into_string()}");
    generate_wrapper(Shell::Fish, &mut completion_file);
    Ok(())
}

fn bash() -> miette::Result<()> {
    let xdg_dirs = xdg::BaseDirectories::with_prefix("bash-completion/completions").expect("Cannot find xdg-base directory");
    let home = std::env::var("HOME").expect("Cannot find your home directory");
    let config_home = xdg_dirs.get_config_home();
    let completion_path = xdg_dirs
        .place_config_file("aiken.completion.bash").expect("Cannot create file at: ~/config/bash-completion/completions");

    let mut bashrc = OpenOptions::new()
        .write(true)
        .append(true)
        .open(format!("{}/.bashrc", home))
        .expect(".bashrc file not found");
    if let Some(config) = config_home.to_str() {
        let path: String = format!("source {config}");
        if let Err(e) = writeln!(bashrc, "{}", path) {
            eprintln!("Couldn't write to file: {}", e);
        }
    }
    let mut completion_file = File::create(completion_path).expect("Cannot open file at: {completion_path.into_os_string().into_string()}");
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
        _ => eprintln!("Shell not supported"),
    }

    Ok(())
}

pub fn exec(cmd_args: Args, shell: Shell) -> miette::Result<()> {
    if cmd_args.install {
        completions_to_file(shell)?;
    } else {
        generate_wrapper(shell, &mut std::io::stdout());
    }
    Ok(())
}
