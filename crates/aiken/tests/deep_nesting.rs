use aiken_lang::expr::MAX_EXPRESSION_NESTING;
use std::{
    env,
    ffi::OsString,
    fs,
    path::PathBuf,
    process::{Command, Output},
    sync::atomic::{AtomicUsize, Ordering},
};

static NEXT_PROJECT_ID: AtomicUsize = AtomicUsize::new(0);

struct TestProject {
    root: PathBuf,
}

impl TestProject {
    fn new(depth: usize) -> Self {
        Self::from_source(deep_module(depth))
    }

    fn from_source(source: String) -> Self {
        let project_id = NEXT_PROJECT_ID.fetch_add(1, Ordering::Relaxed);
        let root = std::env::temp_dir().join(format!(
            "aiken-deep-nesting-{}-{project_id}",
            std::process::id()
        ));

        fs::create_dir_all(root.join("lib")).expect("create temporary Aiken project");
        fs::write(
            root.join("aiken.toml"),
            "name = \"local/deep_nesting\"\nversion = \"0.0.0\"\nplutusVersion = \"v3\"\n",
        )
        .expect("write temporary project manifest");
        fs::write(root.join("lib/deep.ak"), source).expect("write deeply nested Aiken module");

        Self { root }
    }

    fn check(&self, skip_tests: bool, debug: bool) -> Output {
        let mut command = Command::new(aiken_binary());
        command.arg("check").current_dir(&self.root);
        command.env("NO_COLOR", "1");

        if skip_tests {
            command.arg("--skip-tests");
        }
        if debug {
            command.arg("--debug");
        }

        command.output().expect("run aiken check subprocess")
    }
}

impl Drop for TestProject {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.root);
    }
}

fn aiken_binary() -> OsString {
    env::var_os("AIKEN_BIN").unwrap_or_else(|| env!("CARGO_BIN_EXE_aiken").into())
}

fn deep_module(depth: usize) -> String {
    let mut source = String::with_capacity(160 + depth * 3);
    source.push_str("type Natural {\n  Z\n  S(Natural)\n}\n\ntest deeply_nested_constructor() {\n  let value = ");

    for _ in 0..depth {
        source.push_str("S(");
    }

    source.push('Z');

    for _ in 0..depth {
        source.push(')');
    }

    source.push_str("\n  value == value\n}\n");
    source
}

fn deep_failing_module(depth: usize) -> String {
    let mut source = String::with_capacity(160 + depth * 3);
    source.push_str(
        "type Natural {\n  Z\n  S(Natural)\n}\n\ntest deeply_nested_failure() {\n  let value = ",
    );

    for _ in 0..depth {
        source.push_str("S(");
    }

    source.push('Z');

    for _ in 0..depth {
        source.push(')');
    }

    source.push_str("\n  value == Z\n}\n");
    source
}

fn assert_check_succeeds(depth: usize, skip_tests: bool) {
    let project = TestProject::new(depth);
    let output = project.check(skip_tests, false);

    assert!(
        output.status.success(),
        "aiken check failed at depth {depth} (skip_tests={skip_tests}) with status {}\nstdout:\n{}\nstderr:\n{}",
        output.status,
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
}

fn assert_depth_limit_exits_normally(depth: usize, skip_tests: bool) {
    let project = TestProject::new(depth);
    let output = project.check(skip_tests, false);
    let stderr = String::from_utf8_lossy(&output.stderr);
    let stdout = String::from_utf8_lossy(&output.stdout);

    assert_eq!(
        output.status.code(),
        Some(1),
        "aiken check did not return a normal diagnostic exit at depth {depth} (skip_tests={skip_tests})\nstdout:\n{}\nstderr:\n{stderr}",
        stdout,
    );

    #[cfg(unix)]
    {
        use std::os::unix::process::ExitStatusExt;
        assert_eq!(
            output.status.signal(),
            None,
            "aiken check terminated by signal at depth {depth}"
        );
    }
}

fn assert_failing_test_exits_normally(depth: usize) {
    let project = TestProject::from_source(deep_failing_module(depth));
    let output = project.check(false, true);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert_eq!(
        output.status.code(),
        Some(1),
        "failing deep test did not return a normal diagnostic exit at depth {depth}\nstdout:\n{}\nstderr:\n{stderr}",
        String::from_utf8_lossy(&output.stdout),
    );

    #[cfg(unix)]
    {
        use std::os::unix::process::ExitStatusExt;
        assert_eq!(
            output.status.signal(),
            None,
            "failing deep test terminated by signal at depth {depth}"
        );
    }
}

#[test]
fn check_accepts_deep_constructor_chains() {
    for depth in [2_048, 4_096, 6_144] {
        assert_check_succeeds(depth, false);
    }
}

#[test]
fn skip_tests_still_checks_deep_constructor_chains() {
    assert_check_succeeds(4_096, true);
}

#[test]
fn excessive_nesting_exits_normally() {
    assert_depth_limit_exits_normally(MAX_EXPRESSION_NESTING + 1, false);
    assert_depth_limit_exits_normally(MAX_EXPRESSION_NESTING * 10, true);
}

#[test]
fn failing_deep_test_renders_and_exits_normally() {
    assert_failing_test_exits_normally(4_096);
}
