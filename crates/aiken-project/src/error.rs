use crate::{blueprint, deps::manifest::Package, package_name::PackageName};
use aiken_lang::{
    ast::{self, Span},
    error::ExtraData,
    parser::error::ParseError,
    test_framework::{BenchmarkResult, PropertyTestResult, TestResult, UnitTestResult},
    tipo,
};
use hex::FromHexError;
use indoc::formatdoc;
use miette::{
    Diagnostic, EyreContext, LabeledSpan, MietteHandler, MietteHandlerOpts, NamedSource, RgbColors,
    SourceCode,
};
use ordinal::Ordinal;
use owo_colors::{
    OwoColorize,
    Stream::{Stderr, Stdout},
};
use pallas_addresses::ScriptHash;
use std::{
    collections::BTreeSet,
    fmt::{self, Debug, Display},
    io,
    path::{Path, PathBuf},
};
use zip::result::ZipError;

pub enum TomlLoadingContext {
    Project,
    Manifest,
    Package,
    Workspace,
}

impl fmt::Display for TomlLoadingContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TomlLoadingContext::Project => write!(f, "project"),
            TomlLoadingContext::Manifest => write!(f, "manifest"),
            TomlLoadingContext::Package => write!(f, "package"),
            TomlLoadingContext::Workspace => write!(f, "workspace"),
        }
    }
}

#[allow(dead_code)]
#[derive(thiserror::Error)]
pub enum Error {
    #[error("I just found two modules with the same name: '{}'", module.if_supports_color(Stderr, |s| s.yellow()))]
    DuplicateModule {
        module: String,
        first: PathBuf,
        second: PathBuf,
    },

    #[error("Some operation on the file-system did fail.")]
    FileIo { error: io::Error, path: PathBuf },

    #[error("I found some files with incorrectly formatted source code.")]
    Format { problem_files: Vec<Unformatted> },

    #[error(transparent)]
    Blueprint(#[from] Box<blueprint::Error>),

    #[error(transparent)]
    StandardIo(#[from] io::Error),

    #[error(transparent)]
    Http(#[from] reqwest::Error),

    #[error(transparent)]
    ZipExtract(#[from] ZipError),

    #[error(transparent)]
    JoinError(#[from] tokio::task::JoinError),

    #[error(transparent)]
    Json(#[from] serde_json::Error),

    #[error(transparent)]
    Module(#[from] ast::Error),

    #[error("I could not load the {ctx} config file.")]
    TomlLoading {
        ctx: TomlLoadingContext,
        path: PathBuf,
        src: String,
        named: Box<NamedSource<String>>,
        location: Option<Span>,
        help: String,
    },

    #[error("I couldn't find any 'aiken.toml' manifest in {path}.")]
    MissingManifest { path: PathBuf },

    #[error("I just found a cycle in module hierarchy!")]
    ImportCycle { modules: Vec<String> },

    #[error("While parsing files...")]
    Parse {
        path: PathBuf,
        src: String,
        named: Box<NamedSource<String>>,
        #[source]
        error: Box<ParseError>,
    },

    #[error("While trying to make sense of your code...")]
    Type {
        path: PathBuf,
        src: String,
        named: NamedSource<String>,
        #[source]
        error: Box<tipo::error::Error>,
    },

    #[error("{name} failed{}", if *verbose { format!("\n{src}") } else { String::new() } )]
    TestFailure {
        name: String,
        path: PathBuf,
        verbose: bool,
        src: String,
    },

    #[error(
        "I was unable to resolve '{}' for {}/{}",
        package.version,
        package.name.owner,
        package.name.repo
    )]
    UnknownPackageVersion { package: Package },

    #[error(
        "I need to resolve a package {}/{}, but couldn't find it.",
        package.name.owner,
        package.name.repo,
    )]
    UnableToResolvePackage { package: Package },

    #[error("I couldn't parse the provided stake address.")]
    MalformedStakeAddress {
        error: Option<pallas_addresses::Error>,
    },

    #[error("I couldn't find any exportable function named '{name}' in module '{module}'.")]
    ExportNotFound { module: String, name: String },

    #[error("No such module '{module}' found in the project.")]
    ModuleNotFound {
        module: String,
        known_modules: Vec<String>,
    },

    #[error("Failed to convert program to DeBruijn form: {error}")]
    DeBruijnConversion { error: String },

    #[error("Test failed during coverage: {name}")]
    CoverageTestFailure { name: String },

    #[error("I located conditional modules under 'env', but no default one!")]
    NoDefaultEnvironment,

    #[error(
        "I couldn't find any script matching {} in your blueprint (plutus.json).",
        script_hash.to_string().if_supports_color(Stdout, |s| s.yellow()),
    )]
    ScriptOverrideNotFound {
        script_hash: ScriptHash,
        known_scripts: BTreeSet<ScriptHash>,
    },

    #[error(
        "I couldn't parse the {} script override argument.",
        Ordinal(*index as u32 + 1).to_string().if_supports_color(Stdout, |s| s.bold()),
    )]
    ScriptOverrideArgumentParseError {
        index: usize,
        #[source]
        error: ScriptOverrideArgumentError,
    },
}

#[derive(thiserror::Error, Debug, Diagnostic)]
pub enum ScriptOverrideArgumentError {
    #[error(
        "I couldn't find the left side of the mapping. Are you sure you provided two colon-separated validator qualifiers?\n"
    )]
    #[diagnostic(help(
        "{}",
        formatdoc!{
            r#"I am expecting a mapping {from} a validator of the transaction, {to} a replacement in the blueprint as such:

                --script-override "122171C7C82348C420A47AFD8F{colon}7730C913CEEC22524B98E1604A"
                                   {underline}
                                    {missing}
            "#,
            from = "from"
                .if_supports_color(Stdout, |s| s.bold()),
            to = "to"
                .if_supports_color(Stdout, |s| s.bold()),
            colon = ":"
                .if_supports_color(Stderr, |s| s.cyan())
                .if_supports_color(Stderr, |s| s.bold()),
            underline = "^^^^^^^^^^^^^^^^^^^^^^^^^^"
                .if_supports_color(Stdout, |s| s.yellow())
                .if_supports_color(Stdout, |s| s.bold()),
            missing = "missing left side (from)"
                .if_supports_color(Stdout, |s| s.yellow())
        },
    ))]
    MissingFrom,

    #[error(
        "I couldn't find the right side of the mapping. Are you sure you provided two colon-separated validator qualifiers?\n"
    )]
    #[diagnostic(help(
        "{}",
        formatdoc!{
            r#"I am expecting a mapping {from} a validator of the transaction, {to} a replacement in the blueprint as such:

                --script-override "122171C7C82348C420A47AFD8F{colon}7730C913CEEC22524B98E1604A"
                                                              {underline}
                                                               {missing}
            "#,
            from = "from"
                .if_supports_color(Stdout, |s| s.bold()),
            to = "to"
                .if_supports_color(Stdout, |s| s.bold()),
            colon = ":"
                .if_supports_color(Stderr, |s| s.cyan())
                .if_supports_color(Stderr, |s| s.bold()),
            underline = "^^^^^^^^^^^^^^^^^^^^^^^^^^"
                .if_supports_color(Stdout, |s| s.yellow())
                .if_supports_color(Stdout, |s| s.bold()),
            missing = "missing right side (to)"
                .if_supports_color(Stdout, |s| s.yellow())
        },
    ))]
    MissingTo,

    #[error(
        "The origin qualifier ({}) isn't a valid hex-encoded hash digest.\n",
        "from".if_supports_color(Stdout, |s| s.bold()),
    )]
    #[diagnostic(help("{0}"))]
    InvalidFromHash(FromHexError),

    #[error(
        "The destination qualifier ({}) isn't a valid hex-encoded hash digest.\n",
        "to".if_supports_color(Stdout, |s| s.bold()),
    )]
    #[diagnostic(help("{0}"))]
    InvalidToHash(FromHexError),

    #[error(
        "The origin qualifier ({}) isn't a valid script hash digest.\n",
        "from".if_supports_color(Stdout, |s| s.bold()),
    )]
    #[diagnostic(help(
        "The size is incorrect. I expected to find {} bytes but found {}.",
        "28".if_supports_color(Stdout, |s| s.green()),
        .0.to_string().if_supports_color(Stdout, |s| s.red()),
    ))]
    InvalidFromSize(usize),

    #[error(
        "The destination qualifier ({}) isn't a valid script hash digest.\n",
        "to".if_supports_color(Stdout, |s| s.bold()),
    )]
    #[diagnostic(help(
        "The size is incorrect. I expected to find {} bytes but found {}.",
        "28".if_supports_color(Stdout, |s| s.green()),
        .0.to_string().if_supports_color(Stdout, |s| s.red()),
    ))]
    InvalidToSize(usize),
}

impl Error {
    pub fn report(&self) {
        if let Error::TestFailure { verbose, .. } = self {
            if !verbose {
                return;
            }
        }

        println!("{self:?}")
    }

    pub fn from_parse_errors(errs: Vec<ParseError>, path: &Path, src: &str) -> Vec<Self> {
        let mut errors = Vec::with_capacity(errs.len());

        for error in errs {
            errors.push(Error::Parse {
                path: path.into(),
                src: src.to_string(),
                named: NamedSource::new(path.display().to_string(), src.to_string()).into(),
                error: error.into(),
            });
        }

        errors
    }

    pub fn from_test_result<U, T>(result: &TestResult<U, T>, verbose: bool) -> Self {
        let (name, path, src) = match result {
            TestResult::UnitTestResult(UnitTestResult { test, .. }) => (
                test.name.to_string(),
                test.input_path.to_path_buf(),
                test.program.to_pretty(),
            ),
            TestResult::PropertyTestResult(PropertyTestResult { test, .. }) => (
                test.name.to_string(),
                test.input_path.to_path_buf(),
                test.program.to_pretty(),
            ),
            TestResult::BenchmarkResult(BenchmarkResult { bench, .. }) => (
                bench.name.to_string(),
                bench.input_path.to_path_buf(),
                bench.program.to_pretty(),
            ),
        };

        Error::TestFailure {
            name,
            path,
            src,
            verbose,
        }
    }
}

impl Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        default_miette_handler(2)
            .debug(self, f)
            // Ignore error to prevent format! panics. This can happen if span points at some
            // inaccessible location, for example by calling `report_error()` with wrong working set.
            .or(Ok(()))
    }
}

impl From<Error> for Vec<Error> {
    fn from(value: Error) -> Self {
        vec![value]
    }
}

impl ExtraData for Error {
    fn extra_data(&self) -> Option<String> {
        match self {
            Error::DuplicateModule { .. }
            | Error::FileIo { .. }
            | Error::Format { .. }
            | Error::StandardIo { .. }
            | Error::Blueprint { .. }
            | Error::MissingManifest { .. }
            | Error::TomlLoading { .. }
            | Error::ImportCycle { .. }
            | Error::Parse { .. }
            | Error::TestFailure { .. }
            | Error::Http { .. }
            | Error::ZipExtract { .. }
            | Error::JoinError { .. }
            | Error::UnknownPackageVersion { .. }
            | Error::UnableToResolvePackage { .. }
            | Error::Json { .. }
            | Error::MalformedStakeAddress { .. }
            | Error::Module { .. }
            | Error::NoDefaultEnvironment
            | Error::ModuleNotFound { .. }
            | Error::ExportNotFound { .. }
            | Error::ScriptOverrideNotFound { .. }
            | Error::ScriptOverrideArgumentParseError { .. }
            | Error::DeBruijnConversion { .. }
            | Error::CoverageTestFailure { .. } => None,
            Error::Type { error, .. } => error.extra_data(),
        }
    }
}

pub trait GetSource {
    fn path(&self) -> Option<PathBuf>;
    fn src(&self) -> Option<String>;
}

impl GetSource for Error {
    fn path(&self) -> Option<PathBuf> {
        match self {
            Error::FileIo { .. }
            | Error::Format { .. }
            | Error::StandardIo(_)
            | Error::Blueprint(_)
            | Error::ImportCycle { .. }
            | Error::Http(_)
            | Error::ZipExtract(_)
            | Error::JoinError(_)
            | Error::UnknownPackageVersion { .. }
            | Error::UnableToResolvePackage { .. }
            | Error::Json { .. }
            | Error::MalformedStakeAddress { .. }
            | Error::ModuleNotFound { .. }
            | Error::ExportNotFound { .. }
            | Error::NoDefaultEnvironment
            | Error::Module { .. }
            | Error::ScriptOverrideNotFound { .. }
            | Error::ScriptOverrideArgumentParseError { .. }
            | Error::DeBruijnConversion { .. }
            | Error::CoverageTestFailure { .. } => None,
            Error::DuplicateModule { second: path, .. }
            | Error::MissingManifest { path }
            | Error::TomlLoading { path, .. }
            | Error::Parse { path, .. }
            | Error::Type { path, .. }
            | Error::TestFailure { path, .. } => Some(path.to_path_buf()),
        }
    }

    fn src(&self) -> Option<String> {
        match self {
            Error::DuplicateModule { .. }
            | Error::FileIo { .. }
            | Error::Format { .. }
            | Error::StandardIo(_)
            | Error::Blueprint(_)
            | Error::MissingManifest { .. }
            | Error::ImportCycle { .. }
            | Error::TestFailure { .. }
            | Error::Http(_)
            | Error::ZipExtract(_)
            | Error::JoinError(_)
            | Error::UnknownPackageVersion { .. }
            | Error::UnableToResolvePackage { .. }
            | Error::Json { .. }
            | Error::MalformedStakeAddress { .. }
            | Error::NoDefaultEnvironment
            | Error::ModuleNotFound { .. }
            | Error::ExportNotFound { .. }
            | Error::Module { .. }
            | Error::ScriptOverrideNotFound { .. }
            | Error::ScriptOverrideArgumentParseError { .. }
            | Error::DeBruijnConversion { .. }
            | Error::CoverageTestFailure { .. } => None,
            Error::TomlLoading { src, .. } | Error::Parse { src, .. } | Error::Type { src, .. } => {
                Some(src.to_string())
            }
        }
    }
}

impl Diagnostic for Error {
    fn severity(&self) -> Option<miette::Severity> {
        Some(miette::Severity::Error)
    }

    fn code<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        fn boxed<'a>(s: Box<dyn Display + 'a>) -> Box<dyn Display + 'a> {
            Box::new(format!(
                "        {} {}",
                "Error"
                    .if_supports_color(Stdout, |s| s.red())
                    .if_supports_color(Stdout, |s| s.bold()),
                format!("{s}").if_supports_color(Stdout, |s| s.red())
            ))
        }

        match self {
            Error::DuplicateModule { .. } => Some(boxed(Box::new("aiken::module::duplicate"))),
            Error::Blueprint(e) => e.code().map(boxed),
            Error::ImportCycle { .. } => Some(boxed(Box::new("aiken::module::cyclical"))),
            Error::Parse { .. } => Some(boxed(Box::new("aiken::parser"))),
            Error::Type { error, .. } => Some(boxed(Box::new(format!(
                "aiken::check{}",
                error.code().map(|s| format!("::{s}")).unwrap_or_default()
            )))),
            Error::TomlLoading { .. } => Some(boxed(Box::new("aiken::loading::toml"))),
            Error::TestFailure { path, .. } => Some(boxed(Box::new(path.to_str().unwrap_or("")))),
            Error::Http(_) => Some(Box::new("aiken::packages::download")),
            Error::UnknownPackageVersion { .. } => {
                Some(boxed(Box::new("aiken::packages::resolve")))
            }
            Error::UnableToResolvePackage { .. } => {
                Some(boxed(Box::new("aiken::package::download")))
            }
            Error::StandardIo(_)
            | Error::MissingManifest { .. }
            | Error::ZipExtract(_)
            | Error::JoinError(_)
            | Error::FileIo { .. }
            | Error::Format { .. }
            | Error::Json { .. }
            | Error::MalformedStakeAddress { .. }
            | Error::ExportNotFound { .. }
            | Error::ModuleNotFound { .. }
            | Error::NoDefaultEnvironment
            | Error::ScriptOverrideNotFound { .. }
            | Error::ScriptOverrideArgumentParseError { .. }
            | Error::DeBruijnConversion { .. }
            | Error::CoverageTestFailure { .. } => None,
            Error::Module(e) => e.code().map(boxed),
        }
    }

    fn help<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        match self {
            Error::DuplicateModule { first, second, .. } => Some(Box::new(format!(
                "Rename either of them:\n- {}\n- {}",
                first.display().if_supports_color(Stderr, |s| s.yellow()),
                second.display().if_supports_color(Stderr, |s| s.yellow()),
            ))),
            Error::FileIo { error, .. } => Some(Box::new(format!("{error}"))),
            Error::Blueprint(e) => e.help(),
            Error::ImportCycle { modules } => Some(Box::new(format!(
                "Try moving the shared code to a separate module that the others can depend on\n- {}",
                modules.join("\n- ")
            ))),
            Error::Parse { error, .. } => error.help(),
            Error::Type { error, .. } => error.help(),
            Error::MissingManifest { .. } => Some(Box::new(
                "Try running `aiken new <REPOSITORY/PROJECT>` to initialise a project with an example manifest.",
            )),
            Error::NoDefaultEnvironment => Some(Box::new(
                "Environment module names are free, but there must be at least one named 'default.ak'.",
            )),
            Error::TomlLoading { help, .. } => Some(Box::new(help)),

            Error::ModuleNotFound { known_modules, .. } => Some(Box::new(format!(
                "I know about the following modules:\n{}",
                known_modules
                    .iter()
                    .map(|s| format!("─▶ {}", s.if_supports_color(Stdout, |s| s.purple())))
                    .collect::<Vec<_>>()
                    .join("\n")
            ))),
            Error::UnknownPackageVersion { .. } => Some(Box::new(
                "Perhaps, double-check the package repository and version?",
            )),
            Error::UnableToResolvePackage { .. } => Some(Box::new(
                "The network is unavailable and the package isn't in the local cache either. Try connecting to the Internet so I can look it up?",
            )),
            Error::Json(error) => Some(Box::new(format!("{error}"))),
            Error::MalformedStakeAddress { error } => Some(Box::new(format!(
                "A stake address must be provided either as a base16-encoded string, or as a bech32-encoded string with the 'stake' or 'stake_test' prefix.{hint}",
                hint = match error {
                    Some(error) => format!("\n\nHere's the error I encountered: {error}"),
                    None => String::new(),
                }
            ))),
            Error::ScriptOverrideNotFound { known_scripts, .. } => Some(Box::new(format!(
                "These are all the scripts found in your blueprint:\n\n{}",
                known_scripts
                    .iter()
                    .map(|s| format!("✓ {s}"))
                    .collect::<Vec<_>>()
                    .join("\n"),
            ))),
            Error::ScriptOverrideArgumentParseError { error, .. } => error.help(),
            Error::Module(e) => e.help(),
            Error::StandardIo(_)
            | Error::Format { .. }
            | Error::TestFailure { .. }
            | Error::Http(_)
            | Error::ZipExtract(_)
            | Error::JoinError(_)
            | Error::ExportNotFound { .. }
            | Error::DeBruijnConversion { .. }
            | Error::CoverageTestFailure { .. } => None,
        }
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        match self {
            Error::Blueprint(e) => e.labels(),
            Error::Parse { error, .. } => error.labels(),
            Error::Type { error, .. } => error.labels(),
            Error::TomlLoading { location, .. } => {
                if let Some(location) = location {
                    Some(Box::new(
                        vec![LabeledSpan::new_with_span(None, *location)].into_iter(),
                    ))
                } else {
                    None
                }
            }
            Error::DuplicateModule { .. }
            | Error::FileIo { .. }
            | Error::ImportCycle { .. }
            | Error::ExportNotFound { .. }
            | Error::StandardIo(_)
            | Error::MissingManifest { .. }
            | Error::Format { .. }
            | Error::TestFailure { .. }
            | Error::Http(_)
            | Error::ZipExtract(_)
            | Error::JoinError(_)
            | Error::UnknownPackageVersion { .. }
            | Error::UnableToResolvePackage { .. }
            | Error::Json { .. }
            | Error::MalformedStakeAddress { .. }
            | Error::NoDefaultEnvironment
            | Error::ModuleNotFound { .. }
            | Error::ScriptOverrideNotFound { .. }
            | Error::ScriptOverrideArgumentParseError { .. }
            | Error::DeBruijnConversion { .. }
            | Error::CoverageTestFailure { .. } => None,

            Error::Module(e) => e.labels(),
        }
    }

    fn source_code(&self) -> Option<&dyn SourceCode> {
        match self {
            Error::Blueprint(e) => e.source_code(),
            Error::Parse { named, .. } => Some(named.as_ref()),
            Error::Type { named, .. } => Some(named),
            Error::TomlLoading { named, .. } => Some(named.as_ref()),
            Error::DuplicateModule { .. }
            | Error::FileIo { .. }
            | Error::ImportCycle { .. }
            | Error::ModuleNotFound { .. }
            | Error::ExportNotFound { .. }
            | Error::NoDefaultEnvironment
            | Error::StandardIo(_)
            | Error::MissingManifest { .. }
            | Error::Format { .. }
            | Error::TestFailure { .. }
            | Error::Http(_)
            | Error::ZipExtract(_)
            | Error::JoinError(_)
            | Error::UnknownPackageVersion { .. }
            | Error::UnableToResolvePackage { .. }
            | Error::Json { .. }
            | Error::MalformedStakeAddress { .. }
            | Error::ScriptOverrideNotFound { .. }
            | Error::ScriptOverrideArgumentParseError { .. }
            | Error::DeBruijnConversion { .. }
            | Error::CoverageTestFailure { .. } => None,
            Error::Module(e) => e.source_code(),
        }
    }

    fn url<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        match self {
            Error::Blueprint(e) => e.url(),
            Error::Type { error, .. } => error.url(),
            Error::DuplicateModule { .. }
            | Error::FileIo { .. }
            | Error::ImportCycle { .. }
            | Error::ModuleNotFound { .. }
            | Error::ExportNotFound { .. }
            | Error::Parse { .. }
            | Error::StandardIo(_)
            | Error::MissingManifest { .. }
            | Error::TomlLoading { .. }
            | Error::Format { .. }
            | Error::TestFailure { .. }
            | Error::Http { .. }
            | Error::ZipExtract { .. }
            | Error::JoinError { .. }
            | Error::UnknownPackageVersion { .. }
            | Error::UnableToResolvePackage { .. }
            | Error::Json { .. }
            | Error::MalformedStakeAddress { .. }
            | Error::NoDefaultEnvironment
            | Error::ScriptOverrideNotFound { .. }
            | Error::ScriptOverrideArgumentParseError { .. }
            | Error::DeBruijnConversion { .. }
            | Error::CoverageTestFailure { .. } => None,

            Error::Module(e) => e.url(),
        }
    }

    fn related<'a>(&'a self) -> Option<Box<dyn Iterator<Item = &'a dyn Diagnostic> + 'a>> {
        match self {
            Error::Blueprint(e) => e.related(),
            Error::Type { error, .. } => error.related(),
            Error::DuplicateModule { .. }
            | Error::FileIo { .. }
            | Error::ModuleNotFound { .. }
            | Error::ExportNotFound { .. }
            | Error::ImportCycle { .. }
            | Error::Parse { .. }
            | Error::StandardIo(_)
            | Error::NoDefaultEnvironment
            | Error::MissingManifest { .. }
            | Error::TomlLoading { .. }
            | Error::Format { .. }
            | Error::TestFailure { .. }
            | Error::Http { .. }
            | Error::ZipExtract { .. }
            | Error::JoinError { .. }
            | Error::UnknownPackageVersion { .. }
            | Error::UnableToResolvePackage { .. }
            | Error::Json { .. }
            | Error::MalformedStakeAddress { .. }
            | Error::ScriptOverrideNotFound { .. }
            | Error::ScriptOverrideArgumentParseError { .. }
            | Error::DeBruijnConversion { .. }
            | Error::CoverageTestFailure { .. } => None,
            Error::Module(e) => e.related(),
        }
    }
}

#[derive(thiserror::Error)]
#[allow(clippy::large_enum_variant)]
pub enum Warning {
    #[error("You do not have any validators to build!")]
    NoValidators,
    #[error("{}", warning)]
    Type {
        path: PathBuf,
        src: String,
        named: NamedSource<String>,
        #[source]
        warning: tipo::error::Warning,
    },
    #[error("{name} is already a dependency.")]
    DependencyAlreadyExists { name: PackageName },
    #[error("Ignoring file with invalid module name at: {path:?}")]
    InvalidModuleName { path: PathBuf },
    #[error("aiken.toml demands compiler version {demanded}, but you are using {current}.")]
    CompilerVersionMismatch { demanded: String, current: String },
    #[error("No configuration found for environment {env}.")]
    NoConfigurationForEnv { env: String },
    #[error("Suspicious test filter (-m) yielding no test scenarios.")]
    SuspiciousTestMatch { test: String },
}

impl ExtraData for Warning {
    fn extra_data(&self) -> Option<String> {
        match self {
            Warning::NoValidators
            | Warning::DependencyAlreadyExists { .. }
            | Warning::InvalidModuleName { .. }
            | Warning::CompilerVersionMismatch { .. }
            | Warning::NoConfigurationForEnv { .. }
            | Warning::SuspiciousTestMatch { .. } => None,
            Warning::Type { warning, .. } => warning.extra_data(),
        }
    }
}

impl GetSource for Warning {
    fn path(&self) -> Option<PathBuf> {
        match self {
            Warning::InvalidModuleName { path } | Warning::Type { path, .. } => Some(path.clone()),
            Warning::NoValidators
            | Warning::DependencyAlreadyExists { .. }
            | Warning::NoConfigurationForEnv { .. }
            | Warning::CompilerVersionMismatch { .. }
            | Warning::SuspiciousTestMatch { .. } => None,
        }
    }

    fn src(&self) -> Option<String> {
        match self {
            Warning::Type { src, .. } => Some(src.clone()),
            Warning::NoValidators
            | Warning::InvalidModuleName { .. }
            | Warning::DependencyAlreadyExists { .. }
            | Warning::NoConfigurationForEnv { .. }
            | Warning::CompilerVersionMismatch { .. }
            | Warning::SuspiciousTestMatch { .. } => None,
        }
    }
}

impl Diagnostic for Warning {
    fn severity(&self) -> Option<miette::Severity> {
        Some(miette::Severity::Warning)
    }

    fn source_code(&self) -> Option<&dyn SourceCode> {
        match self {
            Warning::Type { named, .. } => Some(named),
            Warning::NoValidators
            | Warning::InvalidModuleName { .. }
            | Warning::NoConfigurationForEnv { .. }
            | Warning::DependencyAlreadyExists { .. }
            | Warning::CompilerVersionMismatch { .. }
            | Warning::SuspiciousTestMatch { .. } => None,
        }
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        match self {
            Warning::Type { warning, .. } => warning.labels(),
            Warning::InvalidModuleName { .. }
            | Warning::NoValidators
            | Warning::DependencyAlreadyExists { .. }
            | Warning::NoConfigurationForEnv { .. }
            | Warning::CompilerVersionMismatch { .. }
            | Warning::SuspiciousTestMatch { .. } => None,
        }
    }

    fn code<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        match self {
            Warning::Type { warning, .. } => Some(Box::new(format!(
                "aiken::check{}",
                warning.code().map(|s| format!("::{s}")).unwrap_or_default()
            ))),
            Warning::NoValidators => Some(Box::new("aiken::check")),
            Warning::InvalidModuleName { .. } => Some(Box::new("aiken::project::module_name")),
            Warning::CompilerVersionMismatch { .. } => {
                Some(Box::new("aiken::project::compiler_version_mismatch"))
            }
            Warning::DependencyAlreadyExists { .. } => {
                Some(Box::new("aiken::packages::already_exists"))
            }
            Warning::NoConfigurationForEnv { .. } => {
                Some(Box::new("aiken::project::config::missing::env"))
            }
            Warning::SuspiciousTestMatch { .. } => Some(Box::new("aiken::check::suspicious_match")),
        }
    }

    fn help<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        match self {
            Warning::Type { warning, .. } => warning.help(),
            Warning::NoValidators => None,
            Warning::CompilerVersionMismatch { demanded, .. } => Some(Box::new(format!(
                "You may want to switch to {}",
                demanded.if_supports_color(Stdout, |s| s.purple())
            ))),
            Warning::InvalidModuleName { .. } => Some(Box::new(
                "Module names are lowercase, (ascii) alpha-numeric and may contain dashes or underscores.",
            )),
            Warning::DependencyAlreadyExists { .. } => Some(Box::new(
                "If you need to change the version, try 'aiken packages upgrade' instead.",
            )),
            Warning::NoConfigurationForEnv { .. } => Some(Box::new(
                "When configuration keys are missing for a target environment, no 'config' module will be created. This may lead to issues down the line.",
            )),
            Warning::SuspiciousTestMatch { test } => Some(Box::new(format!(
                "Did you mean to match all tests within a specific module? Like so:\n\n╰─▶ {}",
                format!("-m \"{test}.{{..}}\"").if_supports_color(Stderr, |s| s.bold()),
            ))),
        }
    }
}

impl Warning {
    pub fn from_type_warning(warning: tipo::error::Warning, path: PathBuf, src: String) -> Warning {
        Warning::Type {
            path: path.clone(),
            warning,
            src: src.clone(),
            named: NamedSource::new(path.display().to_string(), src),
        }
    }

    pub fn report(&self) {
        eprintln!("{self:?}")
    }
}

impl Debug for Warning {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        default_miette_handler(1)
            .debug(
                &DisplayWarning {
                    title: &self.to_string(),
                    source_code: self.source_code(),
                    labels: self.labels().map(|ls| ls.collect()),
                    help: self.help().map(|s| s.to_string()),
                },
                f,
            )
            // Ignore error to prevent format! panics. This can happen if span points at some
            // inaccessible location, for example by calling `report_error()` with wrong working set.
            .or(Ok(()))
    }
}

#[derive(thiserror::Error)]
#[error("{}", title.if_supports_color(Stderr, |s| s.yellow()))]
struct DisplayWarning<'a> {
    title: &'a str,
    source_code: Option<&'a dyn miette::SourceCode>,
    labels: Option<Vec<LabeledSpan>>,
    help: Option<String>,
}

impl Diagnostic for DisplayWarning<'_> {
    fn severity(&self) -> Option<miette::Severity> {
        Some(miette::Severity::Warning)
    }

    fn source_code(&self) -> Option<&dyn SourceCode> {
        self.source_code
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        self.labels
            .as_ref()
            .map(|ls| ls.iter().cloned())
            .map(Box::new)
            .map(|b| b as Box<dyn Iterator<Item = LabeledSpan>>)
    }

    fn code<'b>(&'b self) -> Option<Box<dyn Display + 'b>> {
        None
    }

    fn help<'b>(&'b self) -> Option<Box<dyn Display + 'b>> {
        self.help
            .as_ref()
            .map(Box::new)
            .map(|b| b as Box<dyn Display + 'b>)
    }
}

impl Debug for DisplayWarning<'_> {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unreachable!("Display warning are never shown directly.");
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Unformatted {
    pub source: PathBuf,
    pub destination: PathBuf,
    pub input: String,
    pub output: String,
}

fn default_miette_handler(context_lines: usize) -> MietteHandler {
    MietteHandlerOpts::new()
        // For better support of terminal themes use the ANSI coloring
        .rgb_colors(RgbColors::Never)
        // If ansi support is disabled in the config disable the eye-candy
        .unicode(true)
        .terminal_links(true)
        .context_lines(context_lines)
        .build()
}
