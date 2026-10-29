use super::add;
use aiken_project::package_subdir::PackageSubdir;

#[derive(clap::Args)]
#[clap(disable_version_flag(true))]
/// Change the version of an installed dependency
pub struct Args {
    /// Package name, in the form of {owner}/{repository}.
    ///
    /// For example → 'add aiken-lang/stdlib'
    ///
    /// Note that by default, this assumes the package is located
    /// on Github.
    package: String,
    /// The package version, as a git commit hash, a tag or a branch name.
    #[clap(long)]
    version: String,

    /// Select the dependency located in this repository subdirectory.
    #[clap(long)]
    subdir: Option<PackageSubdir>,
}

pub fn exec(args: Args) -> miette::Result<()> {
    add::exec(add::Args {
        package: args.package,
        version: args.version,
        subdir: args.subdir,
        overwrite: true,
    })
}
