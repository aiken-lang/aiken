use serde_json::Value;
use std::{
    collections::BTreeMap,
    fs::{self, File, FileTimes},
    path::{Path, PathBuf},
    process::{Command, Output},
    sync::atomic::{AtomicU64, Ordering},
    time::{Duration, UNIX_EPOCH},
};

#[path = "ual_conformance/negative.rs"]
mod negative;
#[path = "ual_conformance/positive.rs"]
mod positive;

static NEXT_PROJECT: AtomicU64 = AtomicU64::new(0);

fn fixtures() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/ual")
}

struct Project(PathBuf);

impl Project {
    fn fixture(relative: &str) -> Self {
        let root = std::env::temp_dir().join(format!(
            "aiken-ual-conformance-{}-{}",
            std::process::id(),
            NEXT_PROJECT.fetch_add(1, Ordering::Relaxed)
        ));
        fs::create_dir(&root).unwrap();
        let project = Self(root);
        for (path, bytes) in files(&fixtures().join(relative)) {
            // Expected output is never an input to the compiler.
            if !path.starts_with("golden") {
                project.put(path.to_str().unwrap(), bytes);
            }
        }
        project.put(
            "environment.json",
            fs::read(fixtures().join("environment.json")).unwrap(),
        );
        // UAL-08 derives the creation date from the project manifest's mtime.
        File::options()
            .write(true)
            .open(project.0.join("aiken.toml"))
            .unwrap()
            .set_times(
                FileTimes::new().set_modified(UNIX_EPOCH + Duration::from_secs(1_793_750_400)),
            )
            .unwrap();
        for directory in [".home", ".cache", ".config", ".data"] {
            fs::create_dir(project.0.join(directory)).unwrap();
        }
        project
    }

    fn put(&self, path: &str, bytes: impl AsRef<[u8]>) {
        let path = self.0.join(path);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, bytes).unwrap();
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
        let output = self.run(&["verify", "generate", "--environment", "environment.json"]);
        assert!(output.status.success(), "{}", diagnostic(&output));
    }

    fn json(&self, relative: &str) -> Value {
        serde_json::from_slice(&fs::read(self.0.join(relative)).unwrap()).unwrap()
    }

    fn bundle(&self) -> BTreeMap<PathBuf, Vec<u8>> {
        files(&self.0.join("assurance"))
    }

    fn assert_golden(&self, relative: &str) {
        let expected = files(&fixtures().join(relative).join("golden/assurance"));
        let mut actual = self.bundle();
        assert_eq!(
            actual.keys().collect::<Vec<_>>(),
            expected.keys().collect::<Vec<_>>(),
            "{relative}: bundle file set changed"
        );
        let environment = fs::read(fixtures().join("environment.json")).unwrap();
        assert_bindings(&expected, &environment);
        assert_bindings(&actual, &environment);

        // Build provenance changes on every commit. Mask only its commit suffix
        // and the consequent blueprint digest; retain all other serialized bytes.
        // Validate both original bundles before masking either binding.
        let blueprint: Value = serde_json::from_slice(&actual[Path::new("plutus.json")]).unwrap();
        let golden_blueprint: Value =
            serde_json::from_slice(&expected[Path::new("plutus.json")]).unwrap();
        let version = blueprint["preamble"]["compiler"]["version"]
            .as_str()
            .unwrap();
        let golden_version = golden_blueprint["preamble"]["compiler"]["version"]
            .as_str()
            .unwrap();
        assert_eq!(
            version.split_once('+').unwrap().0,
            golden_version.split_once('+').unwrap().0,
            "{relative}: compiler release changed"
        );
        let assurance: Value =
            serde_json::from_slice(&actual[Path::new("assurance.json")]).unwrap();
        let golden_assurance: Value =
            serde_json::from_slice(&expected[Path::new("assurance.json")]).unwrap();
        for (file, from, to) in [
            ("plutus.json", version, golden_version),
            (
                "assurance.json",
                assurance["blueprint"]["hash"]["digest"].as_str().unwrap(),
                golden_assurance["blueprint"]["hash"]["digest"]
                    .as_str()
                    .unwrap(),
            ),
        ] {
            let bytes = actual.get_mut(Path::new(file)).unwrap();
            let text = std::str::from_utf8(bytes).unwrap();
            assert_eq!(
                text.matches(from).count(),
                1,
                "{relative}/{file}: ambiguous provenance normalization"
            );
            *bytes = text.replacen(from, to, 1).into_bytes();
        }
        for (path, bytes) in expected {
            assert_eq!(
                std::str::from_utf8(&actual[&path]).unwrap(),
                std::str::from_utf8(&bytes).unwrap(),
                "{relative}/{}: golden bundle differs",
                path.display()
            );
        }
        assert_eq!(
            fs::read(fixtures().join(relative).join("golden/environment.json")).unwrap(),
            fs::read(fixtures().join("environment.json")).unwrap(),
            "{relative}: exported environment differs"
        );
    }
}

impl Drop for Project {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}

fn files(root: &Path) -> BTreeMap<PathBuf, Vec<u8>> {
    fn visit(root: &Path, directory: &Path, result: &mut BTreeMap<PathBuf, Vec<u8>>) {
        for entry in fs::read_dir(directory).unwrap() {
            let entry = entry.unwrap();
            let path = entry.path();
            if entry.file_type().unwrap().is_dir() {
                visit(root, &path, result);
            } else {
                result.insert(
                    path.strip_prefix(root).unwrap().to_owned(),
                    fs::read(path).unwrap(),
                );
            }
        }
    }
    let mut result = BTreeMap::new();
    visit(root, root, &mut result);
    result
}

fn diagnostic(output: &Output) -> String {
    format!(
        "{}\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    )
}

fn assert_bindings(bundle: &BTreeMap<PathBuf, Vec<u8>>, environment: &[u8]) {
    let assurance = serde_json::from_slice(&bundle[Path::new("assurance.json")]).unwrap();
    let contexts: BTreeMap<_, _> = bundle
        .iter()
        .filter_map(|(path, bytes)| {
            path.to_str()
                .unwrap()
                .strip_prefix("checking-")
                .and_then(|name| name.strip_suffix(".json"))
                .map(|id| (id.to_owned(), bytes.as_slice()))
        })
        .collect();
    aiken_ual::validate_bindings(&assurance, &bundle[Path::new("plutus.json")], &contexts).unwrap();
    for bytes in contexts.values() {
        let context = serde_json::from_slice(bytes).unwrap();
        aiken_ual::validate_environment(&context, environment).unwrap();
    }
}

#[test]
fn source_edit_invalidates_function_and_checking_digests() {
    let project = Project::fixture("positive/native_function");
    project.generate();
    let before = project.json("assurance/assurance.json");
    let blueprint = fs::read(project.0.join("assurance/plutus.json")).unwrap();
    let source = fs::read_to_string(project.0.join("validators/fixtures.ak")).unwrap();
    assert_eq!(source.matches("proposed > old").count(), 1);
    project.put(
        "validators/fixtures.ak",
        source.replace("proposed > old", "proposed >= old"),
    );
    project.generate();
    let after = project.json("assurance/assurance.json");
    let target = "fixtures__outbids";
    for assurance in [&before, &after] {
        let function = &assurance["functions"][target];
        let digest =
            aiken_ual::function_digest(function["compiledCode"].as_str().unwrap()).unwrap();
        assert_eq!(function["hash"], serde_json::to_value(digest).unwrap());
    }
    assert_ne!(
        before["functions"][target]["hash"],
        after["functions"][target]["hash"]
    );
    assert_ne!(
        before["checkingContexts"]["outbids"]["hash"],
        after["checkingContexts"]["outbids"]["hash"]
    );
    assert_eq!(
        blueprint,
        fs::read(project.0.join("assurance/plutus.json")).unwrap()
    );
    assert_bindings(
        &project.bundle(),
        &fs::read(fixtures().join("environment.json")).unwrap(),
    );
}
