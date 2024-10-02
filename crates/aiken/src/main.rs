use aiken_project::{config, pretty};
#[cfg(not(target_os = "windows"))]
use cmd::completion;
use cmd::{
    blueprint::{self, address},
    build, check, docs, export, fmt, lsp, new,
    packages::{self, add},
    tx, uplc, Cmd,
};
use owo_colors::OwoColorize;

mod cmd;

fn main() -> miette::Result<()> {
    panic_handler();

    #[cfg(target_env = "musl")]
    openssl_probe::init_ssl_cert_env_vars();

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
        #[cfg(not(target_os = "windows"))]
        Cmd::Completion(sub_cmd) => completion::exec(sub_cmd),
        Cmd::Export(args) => export::exec(args),
    }
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
            |location| {
                format!(
                    "{}:{}:{}\n\n    ",
                    location.file(),
                    location.line(),
                    location.column(),
                )
            },
        );

        let error_message = indoc::formatdoc! {
            r#"{fatal}
                Whoops! You found a bug in the Aiken compiler.

                Please report this error at https://github.com/aiken-lang/aiken/issues/new.
                In your bug report please provide the information below and if possible the code
                that produced it.
                {info}

                {location}{message}"#,
            info = config::compiler_info(),
            fatal = "aiken::fatal::error".red().bold(),
            location = location.purple(),
        };

        println!("\n{}", pretty::indent(&error_message, 3));
    }));
}
