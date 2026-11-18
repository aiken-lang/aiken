#![cfg(unix)]

use serde_json::Value;
use std::{
    fs,
    os::unix::fs::PermissionsExt,
    path::PathBuf,
    process::{Command, Output},
    sync::atomic::{AtomicU64, Ordering},
    time::{Duration, Instant},
};

const CONFIG: &str = include_str!("fixtures/verify_generate/aiken.toml");
const SOURCE: &str = include_str!("fixtures/verify_generate/validators/fixtures.ak");
const ENVIRONMENT: &[u8] = include_bytes!("../../aiken-ual/tests/fixtures/environment.json");
const VERIFIED: &str = "verified (Blaster SMT; no reconstructed Lean proof)";

struct Fixture(PathBuf);

impl Fixture {
    fn new() -> Self {
        static NEXT: AtomicU64 = AtomicU64::new(0);
        let root = std::env::temp_dir().join(format!(
            "aiken-verify-check-{}-{}",
            std::process::id(),
            NEXT.fetch_add(1, Ordering::Relaxed)
        ));
        fs::create_dir(&root).unwrap();
        let fixture = Self(root);
        for directory in [
            "validators",
            "bin",
            "home",
            "cache",
            "config",
            "data",
            "PlutusCoreBlaster",
            "Lean-blaster",
            "CardanoLedgerApiBlaster",
            "assurance",
        ] {
            fs::create_dir(fixture.0.join(directory)).unwrap();
        }
        fixture.put("aiken.toml", CONFIG);
        fixture.put("validators/fixtures.ak", SOURCE);
        fixture.put("environment.json", ENVIRONMENT);
        fixture.put("plutus.json", b"root blueprint sentinel");
        fixture.put("bin/z3", b"fake solver binary; never executed");
        fixture.put(
            "bin/lake",
            br##"#!/bin/sh
if [ "$1" = env ] && [ "$2" = which ] && [ "$3" = z3 ]; then
  printf '%s\n' "$FAKE_ROOT/bin/z3"
  exit 0
fi
if [ "$1" != env ] || [ "$2" != lean ]; then exit 91; fi
if [ "${FAKE_HANG:-0}" = 1 ]; then sleep 30; fi
cat "$FAKE_ROOT/consumer.log"
exit "$FAKE_EXIT"
"##,
        );
        fs::set_permissions(
            fixture.0.join("bin/lake"),
            fs::Permissions::from_mode(0o755),
        )
        .unwrap();
        fixture
    }

    fn put(&self, path: &str, content: impl AsRef<[u8]>) {
        fs::write(self.0.join(path), content).unwrap();
    }

    fn command(&self, terminal: bool) -> Command {
        let mut command = if terminal {
            // util-linux script gives stdout a real TTY and propagates the child's exit code.
            let mut command = Command::new("script");
            command.args(["-q", "-e", "-c"]);
            command.arg(format!(
                "'{}' verify check --environment environment.json --timeout 2",
                env!("CARGO_BIN_EXE_aiken").replace('\'', "'\\''")
            ));
            command.arg("/dev/null");
            command
        } else {
            let mut command = Command::new(env!("CARGO_BIN_EXE_aiken"));
            command.args([
                "verify",
                "check",
                "--environment",
                "environment.json",
                "--timeout",
                "2",
            ]);
            command
        };
        let mut paths = vec![self.0.join("bin")];
        paths.extend(std::env::split_paths(
            &std::env::var_os("PATH").unwrap_or_default(),
        ));
        command
            .current_dir(&self.0)
            .env("PATH", std::env::join_paths(paths).unwrap())
            .env(
                "AIKEN_ASSURANCE_WORKSPACE",
                self.0.join("CardanoLedgerApiBlaster"),
            )
            .env("HOME", self.0.join("home"))
            .env("XDG_CACHE_HOME", self.0.join("cache"))
            .env("XDG_CONFIG_HOME", self.0.join("config"))
            .env("XDG_DATA_HOME", self.0.join("data"))
            .env("NO_COLOR", "1")
            .env("TERM", "dumb")
            .env("FAKE_ROOT", &self.0)
            .env("FAKE_EXIT", "0");
        command
    }

    fn json(&self, path: &str) -> Value {
        serde_json::from_slice(&fs::read(self.0.join(path)).unwrap()).unwrap()
    }
}

impl Drop for Fixture {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}

fn diagnostic(output: &Output) -> String {
    format!(
        "{}\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    )
}

fn log(first: &str, second: &str, checked: usize, skipped: usize, failed: usize) -> String {
    format!(
        "Property 'positive_minimum': {first}\nProperty 'fallback_rejects': {second}\nAssurance 'fixtures/verify_generate': {checked} checked, {skipped} not checked, {failed} failed/inconclusive\n"
    )
}

#[test]
fn fake_lean_verdicts_fail_closed_in_json_and_terminal() {
    let all_verified = log(VERIFIED, VERIFIED, 2, 0, 0);
    let cases = [
        ("verified", all_verified.clone(), 0, true),
        (
            "falsified",
            log("falsified", "falsified", 2, 0, 2),
            0,
            false,
        ),
        (
            "inconclusive",
            log("inconclusive", "inconclusive", 2, 0, 2),
            0,
            false,
        ),
        (
            "not checked",
            log(
                "not checked (natural-language only)",
                "not checked (unsupported language 'agda')",
                0,
                2,
                0,
            ),
            0,
            false,
        ),
        ("mixed", log(VERIFIED, "inconclusive", 2, 0, 1), 0, false),
        (
            "malformed",
            log("verified-ish", VERIFIED, 2, 0, 0),
            0,
            false,
        ),
        ("mismatch", log(VERIFIED, VERIFIED, 1, 0, 0), 0, false),
        (
            "missing verdict",
            format!(
                "Property 'fallback_rejects': {VERIFIED}\nAssurance 'fixtures/verify_generate': 2 checked, 0 not checked, 0 failed/inconclusive\n"
            ),
            0,
            false,
        ),
        (
            "missing summary",
            format!(
                "Property 'positive_minimum': {VERIFIED}\nProperty 'fallback_rejects': {VERIFIED}\n"
            ),
            0,
            false,
        ),
        (
            "duplicate verdict",
            format!("Property 'positive_minimum': {VERIFIED}\n{all_verified}"),
            0,
            false,
        ),
        (
            "unknown verdict",
            format!("Property 'unexpected': {VERIFIED}\n{all_verified}"),
            0,
            false,
        ),
        (
            "wrong title",
            all_verified.replace("fixtures/verify_generate", "another bundle"),
            0,
            false,
        ),
        ("nonzero", all_verified.clone(), 7, false),
    ];
    for terminal in [false, true] {
        let fixture = Fixture::new();
        for (case, text, exit, success) in &cases {
            fixture.put("consumer.log", text);
            // A failing rerun must never leave stale evidence looking current.
            fixture.put("assurance/verified-assurance.json", b"stale evidence");
            let output = fixture
                .command(terminal)
                .env("FAKE_EXIT", exit.to_string())
                .output()
                .unwrap();
            let message = diagnostic(&output);
            assert_eq!(
                output.status.success(),
                *success,
                "{case}, tty={terminal}: {message}"
            );
            assert_eq!(
                fixture.0.join("assurance/verified-assurance.json").exists(),
                *success,
                "{case}: {message}"
            );
            assert_eq!(
                fs::read(fixture.0.join("plutus.json")).unwrap(),
                b"root blueprint sentinel"
            );
            for trust in [
                "Blaster SMT",
                "no reconstructed Lean proof",
                "z3",
                "PlutusCoreBlaster",
                "CardanoLedgerApi",
                "Blaster",
                "step budget",
            ] {
                assert!(
                    message.contains(trust),
                    "missing {trust}: {case}: {message}"
                );
            }
            if !terminal {
                let report: Value = serde_json::from_slice(&output.stdout).expect(&message);
                assert_eq!(report["success"], *success, "{case}: {message}");
                let properties = report["properties"].as_array().unwrap();
                assert_eq!(properties.len(), 2, "{case}: {message}");
                for property in properties {
                    let expected = match *case {
                        "falsified" => Some("falsified"),
                        "inconclusive" => Some("inconclusive"),
                        "not checked" => Some("not-checked"),
                        "mixed" if property["id"] == "fallback_rejects" => Some("inconclusive"),
                        "malformed" | "missing verdict" if property["id"] == "positive_minimum" => {
                            Some("missing")
                        }
                        _ => None,
                    };
                    if let Some(expected) = expected {
                        assert_eq!(property["verdict"], expected, "{case}: {message}");
                    }
                }
                if *success {
                    assert!(
                        properties
                            .iter()
                            .all(|property| property["verdict"] == "verified")
                    );
                }
            }
            if *success {
                let document = fixture.json("assurance/verified-assurance.json");
                aiken_ual::validate(aiken_ual::DocumentKind::Assurance, &document).unwrap();
                for property in document["properties"].as_array().unwrap() {
                    let context = property["checkingContext"].as_str().unwrap();
                    let evidence = &property["evidence"][0];
                    assert_eq!(evidence["method"], "smt-check");
                    assert_eq!(evidence["outcome"], "verified");
                    assert_eq!(
                        evidence["checkingContextHash"],
                        document["checkingContexts"][context]["hash"]
                    );
                }
            }
        }
    }
}

#[test]
fn falsified_counterexample_survives_both_output_modes() {
    let fixture = Fixture::new();
    fixture.put("consumer.log", format!("Property 'positive_minimum': checking fresh proposition\nCounterexample:\n  minimum := 0\n{}", log("falsified", VERIFIED, 2, 0, 1)));
    for terminal in [false, true] {
        let output = fixture.command(terminal).output().unwrap();
        let message = diagnostic(&output);
        assert!(!output.status.success(), "{message}");
        assert!(message.contains("minimum := 0"), "{message}");
        if !terminal {
            let report: Value = serde_json::from_slice(&output.stdout).unwrap();
            let property = report["properties"]
                .as_array()
                .unwrap()
                .iter()
                .find(|property| property["id"] == "positive_minimum")
                .unwrap();
            assert_eq!(property["verdict"], "falsified");
            assert!(
                property["counterexample"]
                    .as_str()
                    .unwrap()
                    .contains("minimum := 0")
            );
        }
    }
}

#[test]
fn timeout_reaps_consumer_and_removes_stale_evidence() {
    let fixture = Fixture::new();
    fixture.put("consumer.log", log(VERIFIED, VERIFIED, 2, 0, 0));
    fixture.put("assurance/verified-assurance.json", b"stale evidence");
    let start = Instant::now();
    let output = fixture
        .command(false)
        .env("FAKE_HANG", "1")
        .output()
        .unwrap();
    assert!(start.elapsed() < Duration::from_secs(15));
    let message = diagnostic(&output);
    assert!(!output.status.success(), "{message}");
    assert!(
        message.contains("timed out") || message.contains("timeout"),
        "{message}"
    );
    assert!(!fixture.0.join("assurance/verified-assurance.json").exists());
    let report: Value = serde_json::from_slice(&output.stdout).unwrap();
    assert_eq!(report["success"], false);
}

#[test]
fn generation_failure_removes_evidence_and_discloses_trust() {
    let fixture = Fixture::new();
    fixture.put("validators/fixtures.ak", "this is not Aiken");
    fixture.put("assurance/verified-assurance.json", b"stale evidence");
    let output = fixture.command(false).output().unwrap();
    assert!(!output.status.success());
    assert!(!fixture.0.join("assurance/verified-assurance.json").exists());
    let report: Value = serde_json::from_slice(&output.stdout)
        .unwrap_or_else(|error| panic!("{error}: {}", diagnostic(&output)));
    assert_eq!(report["success"], false);
    assert!(
        report["trust"]["method"]
            .as_str()
            .unwrap()
            .contains("no reconstructed Lean proof")
    );
}

#[test]
fn schema_is_available_without_consumer_or_project() {
    let fixture = Fixture::new();
    fs::remove_file(fixture.0.join("aiken.toml")).unwrap();
    let output = Command::new(env!("CARGO_BIN_EXE_aiken"))
        .current_dir(&fixture.0)
        .args(["verify", "check", "--show-json-schema"])
        .env_remove("AIKEN_ASSURANCE_WORKSPACE")
        .output()
        .unwrap();
    assert!(output.status.success(), "{}", diagnostic(&output));
    let schema: Value = serde_json::from_slice(&output.stdout).unwrap();
    assert_eq!(schema["type"], "object");
}

#[test]
fn uncovered_validator_warning_is_present_in_both_outputs() {
    let fixture = Fixture::new();
    fixture.put("validators/fixtures.ak", format!(
        "{}\n@onchain(steps: 2500, semantics: e)\npub fn positive(n: Int) -> Bool {{ n > 0 }}\n//@ PROPERTY helper for positive \"Helper claim.\" True @//\n",
        SOURCE.replace("minimum > 0", "positive(minimum)")
    ));
    // Both existing properties scope vesting, so no advisory warning is warranted.
    fixture.put(
        "consumer.log",
        format!(
            "Property 'helper': {VERIFIED}\n{}",
            log(VERIFIED, VERIFIED, 3, 0, 0)
        ),
    );
    let output = fixture.command(false).output().unwrap();
    assert!(output.status.success(), "{}", diagnostic(&output));
    let report: Value = serde_json::from_slice(&output.stdout).unwrap();
    assert!(
        !report["warnings"]
            .as_array()
            .unwrap()
            .iter()
            .any(|warning| warning.as_str().unwrap().contains("no property scopes"))
    );

    fixture.put(
        "validators/fixtures.ak",
        r#"
        @onchain(steps: 100, semantics: e)
        pub fn positive(n: Int) -> Bool { n > 0 }
        //@ PROPERTY helper for positive "Helper claim." True @//
        fn indirect(n: Int) -> Bool { positive(n) }
        validator unscoped {
            else(_ctx: Data) { indirect(1) }
        }
    "#,
    );
    fixture.put("consumer.log", format!("Property 'helper': {VERIFIED}\nAssurance 'fixtures/verify_generate': 1 checked, 0 not checked, 0 failed/inconclusive\n"));
    for terminal in [false, true] {
        let output = fixture.command(terminal).output().unwrap();
        let message = diagnostic(&output);
        assert!(output.status.success(), "{message}");
        assert!(message.contains("fixtures__positive"), "{message}");
        assert!(message.contains("fixtures.unscoped"), "{message}");
        assert!(message.contains("no property scopes"), "{message}");
    }
}
