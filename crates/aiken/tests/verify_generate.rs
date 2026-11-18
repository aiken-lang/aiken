use serde_json::Value;
use std::{
    collections::BTreeMap,
    fs,
    path::{Path, PathBuf},
    process::{Command, Output},
    sync::atomic::{AtomicU64, Ordering},
    time::SystemTime,
};

const CONFIG: &str = include_str!("fixtures/verify_generate/aiken.toml");
const VALIDATOR: &str = include_str!("fixtures/verify_generate/validators/fixtures.ak");
const INVALID_LOWERING: &str = include_str!("fixtures/verify_generate/invalid_lowering.ak");
const ENVIRONMENT: &[u8] = include_bytes!("../../aiken-ual/tests/fixtures/environment.json");
const SENTINEL: &[u8] = b"root blueprint must not be overwritten\n";
static NEXT_PROJECT: AtomicU64 = AtomicU64::new(0);

struct Project(PathBuf);

impl Project {
    fn new() -> Self {
        let root = std::env::temp_dir().join(format!(
            "aiken-verify-generate-{}-{}",
            std::process::id(),
            NEXT_PROJECT.fetch_add(1, Ordering::Relaxed)
        ));
        fs::create_dir(&root).unwrap();
        let project = Self(root);
        for dir in ["validators", "lib", ".home", ".cache", ".config", ".data"] {
            fs::create_dir(project.0.join(dir)).unwrap();
        }
        project.put("aiken.toml", CONFIG);
        project.put("validators/fixtures.ak", VALIDATOR);
        project.put("environment.json", ENVIRONMENT);
        project.put("plutus.json", SENTINEL);
        project
    }

    fn put(&self, path: &str, bytes: impl AsRef<[u8]>) {
        fs::write(self.0.join(path), bytes).unwrap();
    }

    fn run(&self, args: &[&str]) -> Output {
        Command::new(env!("CARGO_BIN_EXE_aiken"))
            .args(args)
            .current_dir(&self.0)
            .env("HOME", self.0.join(".home"))
            .env("XDG_CACHE_HOME", self.0.join(".cache"))
            .env("XDG_CONFIG_HOME", self.0.join(".config"))
            .env("XDG_DATA_HOME", self.0.join(".data"))
            .env("NO_COLOR", "1")
            .output()
            .unwrap()
    }

    fn generate(&self) {
        success(self.run(&["verify", "generate", "--environment", "environment.json"]));
    }

    fn json(&self, path: &str) -> Value {
        serde_json::from_slice(&fs::read(self.0.join(path)).unwrap()).unwrap()
    }

    fn unchanged(&self, args: &[&str], expected_success: bool) -> String {
        let before = snapshot(&self.0);
        let output = self.run(args);
        let message = diagnostic(&output);
        assert_eq!(output.status.success(), expected_success, "{message}");
        assert_eq!(
            snapshot(&self.0),
            before,
            "filesystem changed: {args:?}\n{message}"
        );
        message
    }
}

impl Drop for Project {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}

// Record directories (including the root), file bytes, and modification times.
// Reading atimes is deliberately excluded: opening input files is permitted.
#[derive(Debug, PartialEq, Eq)]
struct Entry {
    directory: bool,
    modified: SystemTime,
    bytes: Vec<u8>,
}

fn snapshot(root: &Path) -> BTreeMap<PathBuf, Entry> {
    fn visit(root: &Path, path: &Path, entries: &mut BTreeMap<PathBuf, Entry>) {
        let metadata = fs::symlink_metadata(path).unwrap();
        assert!(!metadata.file_type().is_symlink());
        entries.insert(
            path.strip_prefix(root).unwrap().to_owned(),
            Entry {
                directory: metadata.is_dir(),
                modified: metadata.modified().unwrap(),
                bytes: if metadata.is_dir() {
                    vec![]
                } else {
                    fs::read(path).unwrap()
                },
            },
        );
        if metadata.is_dir() {
            for child in fs::read_dir(path).unwrap() {
                visit(root, &child.unwrap().path(), entries);
            }
        }
    }
    let mut entries = BTreeMap::new();
    visit(root, root, &mut entries);
    entries
}

fn diagnostic(output: &Output) -> String {
    format!(
        "{}\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    )
}

fn success(output: Output) {
    assert!(output.status.success(), "{}", diagnostic(&output));
}

fn bytes(root: &Path) -> BTreeMap<PathBuf, Vec<u8>> {
    snapshot(root)
        .into_iter()
        .filter_map(|(path, entry)| (!entry.directory).then_some((path, entry.bytes)))
        .collect()
}

#[test]
fn generates_bound_reproducible_bundle_and_removes_only_stale_contexts() {
    let project = Project::new();
    project.generate();
    assert_eq!(fs::read(project.0.join("plutus.json")).unwrap(), SENTINEL);
    let assurance = project.json("assurance/assurance.json");
    let blueprint = project.json("assurance/plutus.json");
    assert_eq!(assurance["blueprint"]["uri"], "plutus.json");
    assert_eq!(assurance["blueprint"]["hash"]["alg"], "sha256");
    assert_eq!(
        assurance["blueprint"]["hash"]["digest"]
            .as_str()
            .unwrap()
            .len(),
        64
    );
    assert_eq!(blueprint["preamble"]["title"], "fixtures/verify_generate");
    assert!(!blueprint["validators"].as_array().unwrap().is_empty());
    let properties = assurance["properties"].as_array().unwrap();
    assert_eq!(properties.len(), 2);
    let contexts = assurance["checkingContexts"].as_object().unwrap();
    assert_eq!(contexts.len(), 2);
    for id in ["positive_minimum", "fallback_rejects"] {
        let property = properties
            .iter()
            .find(|property| property["id"] == id)
            .unwrap();
        assert!(property.get("evidence").is_none());
        assert_eq!(property["checkingContext"], id);
        assert_eq!(property["scope"]["validators"][0], "fixtures__vesting");
        assert!(
            property["statement"]["formal"]["source"]
                .as_str()
                .unwrap()
                .contains("fixtures__vesting")
        );
        let filename = format!("checking-{id}.json");
        assert_eq!(contexts[id]["uri"], filename);
        assert_eq!(contexts[id]["hash"]["alg"], "sha256");
        assert_eq!(contexts[id]["hash"]["digest"].as_str().unwrap().len(), 64);
        let context = project.json(&format!("assurance/{filename}"));
        assert_eq!(context["environment"]["hash"]["alg"], "sha256");
        // Hash of the exact fixture bytes, not of parsed/reformatted JSON.
        assert_eq!(
            context["environment"]["hash"]["digest"],
            "eafb4587f3b2e8bc7bf1fb9175eee3afdf426cf694fe11aa9641146694280d42"
        );
        let environment_uri = context["environment"]["uri"].as_str().unwrap();
        assert_eq!(
            fs::read(project.0.join("assurance").join(environment_uri)).unwrap(),
            ENVIRONMENT
        );
        assert_eq!(context["execution"]["budget"]["steps"], 2500);
        assert_eq!(context["execution"]["semanticsVariant"], "E");
        assert_eq!(context["targets"][0]["validator"], "fixtures__vesting");
        assert_eq!(context["targets"][0]["parameters"]["mode"], "universal");
    }
    let before = bytes(&project.0.join("assurance"));
    project.generate();
    assert_eq!(bytes(&project.0.join("assurance")), before);
    project.put("assurance/checking-obsolete.json", b"stale");
    project.put("assurance/notes.txt", b"keep unrelated output");
    project.generate();
    assert!(!project.0.join("assurance/checking-obsolete.json").exists());
    assert_eq!(
        fs::read(project.0.join("assurance/notes.txt")).unwrap(),
        b"keep unrelated output"
    );
    for (path, expected) in before {
        assert_eq!(
            fs::read(project.0.join("assurance").join(path)).unwrap(),
            expected
        );
    }
    assert_eq!(fs::read(project.0.join("plutus.json")).unwrap(), SENTINEL);
}

const DRY_MODES: &[&[&str]] = &[
    &["verify", "generate", "--no-environment"],
    &[
        "verify",
        "generate",
        "--environment",
        "environment.json",
        "--dry-run",
    ],
];

#[test]
fn dry_runs_leave_fresh_and_existing_projects_completely_unchanged() {
    for existing in [false, true] {
        let project = Project::new();
        if existing {
            project.generate();
            project.put(
                "assurance/checking-obsolete.json",
                "do not remove on dry run",
            );
        }
        for args in DRY_MODES {
            project.unchanged(args, true);
        }
        if !existing {
            assert!(!project.0.join("build").exists());
            assert!(!project.0.join("aiken.lock").exists());
            assert!(!project.0.join("assurance").exists());
        }
    }
}

#[test]
fn dry_run_failures_are_read_only_and_reach_bundle_lowering() {
    for existing in [false, true] {
        for failure in ["annotation", "type", "lowering", "environment"] {
            let project = Project::new();
            if existing {
                project.generate();
            }
            match failure {
                "annotation" => project.put(
                    "validators/fixtures.ak",
                    VALIDATOR.replace("for vesting.spend", "for nonexistent.spend"),
                ),
                "type" => project.put(
                    "validators/fixtures.ak",
                    VALIDATOR.replace("minimum > 0", "minimum > True"),
                ),
                "lowering" => {
                    fs::remove_file(project.0.join("validators/fixtures.ak")).unwrap();
                    project.put("lib/functions.ak", INVALID_LOWERING);
                }
                "environment" => project.put("environment.json", b"{ invalid JSON"),
                _ => unreachable!(),
            }
            for args in DRY_MODES {
                if failure == "environment" && args.contains(&"--no-environment") {
                    project.unchanged(args, true);
                } else {
                    let message = project.unchanged(args, false);
                    if failure == "lowering" {
                        assert!(message.to_lowercase().contains("validator"), "{message}");
                    }
                }
            }
        }
    }
}

#[test]
fn clap_requires_an_environment_choice_and_exposes_common_options() {
    let project = Project::new();
    let missing = project.unchanged(&["verify", "generate"], false);
    assert!(missing.contains("--environment"), "{missing}");
    let conflict = project.unchanged(
        &[
            "verify",
            "generate",
            "--environment",
            "environment.json",
            "--no-environment",
        ],
        false,
    );
    assert!(
        conflict.contains("--environment") && conflict.contains("--no-environment"),
        "{conflict}"
    );
    let help = project.unchanged(&["verify", "generate", "--help"], true);
    for option in [
        "--environment",
        "--no-environment",
        "--dry-run",
        "--trace-filter",
        "--trace-level",
        "--env",
        "DIRECTORY",
    ] {
        assert!(help.contains(option), "missing {option}: {help}");
    }
    project.unchanged(
        &[
            "verify",
            "generate",
            ".",
            "--no-environment",
            "--trace-filter",
            "all",
            "--trace-level",
            "silent",
            "--env",
            "default",
        ],
        true,
    );
}

#[test]
fn ordinary_build_is_unaffected_by_assurance_generation() {
    let project = Project::new();
    success(project.run(&["build"]));
    let before = fs::read(project.0.join("plutus.json")).unwrap();
    project.generate();
    assert_eq!(fs::read(project.0.join("plutus.json")).unwrap(), before);
    success(project.run(&["build"]));
    assert_eq!(fs::read(project.0.join("plutus.json")).unwrap(), before);
}

fn dependency_config(project: &Project) {
    project.put("aiken.toml", format!("{CONFIG}\n[[dependencies]]\nname = \"example/cached\"\nversion = \"v1.0.0\"\nsource = \"github\"\n"));
}

#[test]
fn missing_dependency_fails_without_download_or_lock_writes() {
    let project = Project::new();
    dependency_config(&project);
    for args in DRY_MODES {
        let message = project.unchanged(args, false);
        assert!(
            message.to_lowercase().contains("depend") || message.to_lowercase().contains("cache"),
            "{message}"
        );
    }
}

#[test]
fn pinned_local_dependency_cache_supports_read_only_generation() {
    let project = Project::new();
    dependency_config(&project);
    fs::create_dir_all(project.0.join("build/packages/example-cached/lib")).unwrap();
    project.put(
        "build/packages/example-cached/aiken.toml",
        "name = \"example/cached\"\nversion = \"1.0.0\"\nplutus = \"v3\"\n",
    );
    project.put(
        "build/packages/example-cached/lib/cached.ak",
        "pub fn positive(n: Int) -> Bool { n > 0 }\n",
    );
    project.put(
        "validators/fixtures.ak",
        format!(
            "use cached\n\n{}",
            VALIDATOR.replace("minimum > 0", "cached.positive(minimum)")
        ),
    );
    let pin = "name = \"example/cached\"\nversion = \"v1.0.0\"\nsource = \"github\"\n";
    project.put(
        "aiken.lock",
        format!("[[requirements]]\n{pin}\n[[packages]]\n{pin}requirements = []\n"),
    );
    project.put(
        "build/packages/packages.toml",
        format!("[[packages]]\n{pin}"),
    );
    for args in DRY_MODES {
        project.unchanged(args, true);
    }
}

#[cfg(unix)]
#[test]
fn assurance_symlinks_cannot_redirect_writes_into_the_project() {
    use std::os::unix::fs::symlink;

    let project = Project::new();
    symlink(".", project.0.join("assurance")).unwrap();
    let before = fs::read(project.0.join("plutus.json")).unwrap();
    let output = project.run(&["verify", "generate", "--environment", "environment.json"]);
    assert!(!output.status.success(), "{}", diagnostic(&output));
    assert_eq!(fs::read(project.0.join("plutus.json")).unwrap(), before);
    assert!(!project.0.join("assurance.json").exists());
    assert!(!project.0.join("checking-positive_minimum.json").exists());
    assert!(
        fs::symlink_metadata(project.0.join("assurance"))
            .unwrap()
            .file_type()
            .is_symlink()
    );

    fs::remove_file(project.0.join("assurance")).unwrap();
    fs::create_dir(project.0.join("assurance")).unwrap();
    for name in [
        "plutus.json",
        "assurance.json",
        "checking-positive_minimum.json",
    ] {
        symlink("../plutus.json", project.0.join("assurance").join(name)).unwrap();
    }
    project.generate();
    assert_eq!(fs::read(project.0.join("plutus.json")).unwrap(), before);
    for name in [
        "plutus.json",
        "assurance.json",
        "checking-positive_minimum.json",
    ] {
        let path = project.0.join("assurance").join(name);
        assert!(
            !fs::symlink_metadata(&path)
                .unwrap()
                .file_type()
                .is_symlink()
        );
        serde_json::from_slice::<Value>(&fs::read(path).unwrap()).unwrap();
    }
}
