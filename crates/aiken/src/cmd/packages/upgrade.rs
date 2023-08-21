use super::add;

#[derive(clap::Args)]
#[clap(disable_version_flag = true)]
/// Change the version of an installed dependency
pub struct Args {
    /// Package name, in the form of {owner}/{repository}.
    ///
    /// For example → 'packages upgrade aiken-lang/stdlib'
    ///
    /// Note that by default, this assumes the package is located
    /// on Github.
    package: String,
    /// The package version, as a git commit hash, a tag or a branch name.
    #[clap(long)]
    version: Option<String>,
}

pub fn exec(args: Args) -> miette::Result<()> {
    add::exec(add::Args {
        package: args.package,
        version: args.version,
        overwrite: true,
    })
}
