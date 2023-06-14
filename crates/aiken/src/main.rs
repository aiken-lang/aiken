use aiken::{
    built_info,
    cmd::{
        blueprint::{self, address},
        build, check, docs, fmt, lsp, new,
        packages::{self, add},
        tx, uplc,
    },
};
use clap::Parser;
use owo_colors::OwoColorize;

/// Aiken: a smart-contract language and toolchain for Cardano
#[derive(Parser)]
#[clap(version = version(), about, long_about = None)]
#[clap(propagate_version = true)]
pub enum Cmd {
    New(new::Args),
    Fmt(fmt::Args),
    Build(build::Args),
    Address(address::Args),
    Check(check::Args),
    Docs(docs::Args),
    Add(add::Args),

    #[clap(subcommand)]
    Blueprint(blueprint::Cmd),

    #[clap(subcommand)]
    Packages(packages::Cmd),

    #[clap(subcommand)]
    Tx(tx::Cmd),

    #[clap(subcommand)]
    Uplc(uplc::Cmd),

    #[clap(hide = true)]
    Lsp(lsp::Args),
}

impl Default for Cmd {
    fn default() -> Self {
        Self::parse()
    }
}

fn main() -> miette::Result<()> {
    panic_handler();

    match Cmd::default() {
        Cmd::New(args) => new::exec(args),
        Cmd::Fmt(args) => fmt::exec(args),
        Cmd::Build(args) => build::exec(args),
        Cmd::Address(args) => address::exec(args),
        Cmd::Check(args) => check::exec(args),
        Cmd::Docs(args) => docs::exec(args),
        Cmd::Add(args) => add::exec(args),
        Cmd::Blueprint(args) => blueprint::exec(args),
        Cmd::Packages(args) => packages::exec(args),
        Cmd::Lsp(args) => lsp::exec(args),
        Cmd::Tx(sub_cmd) => tx::exec(sub_cmd),
        Cmd::Uplc(sub_cmd) => uplc::exec(sub_cmd),
    }
}

fn version() -> String {
    use std::env;
    let nix_git_rev = env::var("GIT_REVISION").unwrap_or("unknown".to_string());

    format!(
        "v{} {}",
        built_info::PKG_VERSION,
        built_info::GIT_COMMIT_HASH_SHORT.unwrap_or(&nix_git_rev)
    )
}

fn panic_handler() {
    std::panic::set_hook(Box::new(move |info| {
        let message = info
            .payload()
            .downcast_ref::<&str>()
            .map(|s| (*s).to_string())
            .or_else(|| {
                info.payload()
                    .downcast_ref::<String>()
                    .map(|s| s.to_string())
            })
            .unwrap_or_else(|| "unknown error".to_string());

        let location = info.location().map_or_else(
            || "".into(),
            |location| format!("{}:{}\n\n    ", location.file(), location.line()),
        );

        let error_message = indoc::formatdoc! {
            r#"{fatal}

                Whoops! You found a bug in the Aiken compiler.

                Please report this error at https://github.com/aiken-lang/aiken/issues/new.
                In your bug report please provide the information below and if possible the code
                that produced it.

                    {location}{message}"#,
            fatal = "aiken::fatal::error".red().bold(),
            location = location.purple(),
        };

        println!("{error_message}")
    }));
}
