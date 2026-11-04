use serde_json::Value;
use std::{
    fs,
    path::PathBuf,
    process::{Command, Output},
    sync::atomic::{AtomicU64, Ordering},
};

struct Fixture(PathBuf);

impl Fixture {
    fn new() -> Self {
        static NEXT: AtomicU64 = AtomicU64::new(0);
        let root = std::env::temp_dir().join(format!(
            "aiken-verify-env-test-{}-{}",
            std::process::id(),
            NEXT.fetch_add(1, Ordering::Relaxed)
        ));
        fs::create_dir(&root).unwrap();
        Self(root)
    }

    fn command(&self) -> Command {
        let mut command = Command::new(env!("CARGO_BIN_EXE_aiken"));
        command
            .current_dir(&self.0)
            .env_remove("AIKEN_ASSURANCE_WORKSPACE")
            .env("NO_COLOR", "1")
            .args(["verify", "env"]);
        command
    }
}

impl Drop for Fixture {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}

fn failure(output: Output) -> String {
    assert!(!output.status.success());
    format!(
        "{}\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    )
}

#[test]
fn unconfigured_command_explains_workspace_setup() {
    let fixture = Fixture::new();
    let message = failure(fixture.command().output().unwrap());
    for required in [
        "[assurance]",
        "AIKEN_ASSURANCE_WORKSPACE",
        "CardanoLedgerApiBlaster",
        "scripts/ual/workspace-setup.sh",
        "recfun-finder",
    ] {
        assert!(message.contains(required), "{message}");
    }
    assert!(!fixture.0.join("environment.json").exists());
}

#[test]
fn invalid_config_is_not_hidden_by_environment_fallback() {
    let fixture = Fixture::new();
    fs::write(
        fixture.0.join("aiken.toml"),
        "[assurance]\nworkspace = 42\n",
    )
    .unwrap();
    let message = failure(
        fixture
            .command()
            .env("AIKEN_ASSURANCE_WORKSPACE", &fixture.0)
            .output()
            .unwrap(),
    );
    assert!(message.contains("invalid type"), "{message}");
    assert!(!fixture.0.join("environment.json.run.json").exists());
}

#[test]
fn failed_consumer_run_retains_warnings_without_overwriting_manifest() {
    let fixture = Fixture::new();
    for checkout in [
        "PlutusCoreBlaster",
        "Lean-blaster",
        "CardanoLedgerApiBlaster",
    ] {
        fs::create_dir(fixture.0.join(checkout)).unwrap();
    }
    fs::write(
        fixture.0.join("aiken.toml"),
        "members = []\n[assurance]\nworkspace = 'CardanoLedgerApiBlaster'\n",
    )
    .unwrap();
    let manifest = fixture.0.join("existing environment.json");
    let original = b"previous capture must survive a consumer failure";
    fs::write(&manifest, original).unwrap();
    // No tools available: exercise the real process-spawn failure, not a fake consumer.
    let message = failure(
        fixture
            .command()
            .env("PATH", "")
            .env("AIKEN_ASSURANCE_WORKSPACE", "ignored-missing-workspace")
            .args(["--out", "existing environment.json"])
            .output()
            .unwrap(),
    );
    assert!(message.contains("WARNING:"), "{message}");
    assert!(message.contains("cannot run lake env lean"), "{message}");
    assert_eq!(fs::read(manifest).unwrap(), original);
    let record: Value = serde_json::from_slice(
        &fs::read(fixture.0.join("existing environment.json.run.json")).unwrap(),
    )
    .unwrap();
    assert_eq!(record["status"], "failed");
    assert!(record["environmentHash"].is_null());
    assert!(
        record["error"]
            .as_str()
            .unwrap()
            .contains("cannot run lake env lean")
    );
    assert!(
        record["warnings"]
            .as_array()
            .unwrap()
            .iter()
            .any(|warning| {
                warning
                    .as_str()
                    .unwrap()
                    .contains("cannot determine PlutusCoreBlaster revision")
            })
    );
}
