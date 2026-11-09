"""Regenerate UAL-09 fixtures without changing sources or claiming consumer success."""

import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import time

from fixture_cases import NEGATIVES


MANIFEST_MTIME = 1_793_750_400


def _save_json(path, value):
    path.write_text(json.dumps(value, indent=2, ensure_ascii=False) + "\n")


def _require(condition, message):
    if not condition:
        raise RuntimeError(message)


def _files(directory):
    _require(directory.is_dir(), f"Missing bundle directory: {directory}")
    return {path.relative_to(directory).as_posix(): path.read_bytes()
            for path in sorted(directory.rglob("*")) if path.is_file()}


def _blueprint_binding(bundle):
    assurance = json.loads(bundle["assurance.json"])
    digest = assurance["blueprint"]["hash"]
    _require(digest["alg"].lower() in ("sha256", "sha-256"),
             "Unsupported blueprint digest algorithm")
    _require(digest["digest"].lower() == hashlib.sha256(bundle["plutus.json"]).hexdigest(),
             "Blueprint byte binding differs before provenance normalization")
    return assurance


def _compare_golden(project, fixture, environment):
    """Match UAL-09: normalize only commit provenance and its blueprint digest.

    Generation has already schema- and binding-validated the original emitted
    bytes with aiken_ual. Equal golden bytes retain those checks except for the
    compiler provenance; check both original blueprint bindings before masking it.
    """
    actual = _files(project / "assurance")
    assurance = _blueprint_binding(actual)
    contexts = sorted(name for name in actual
                      if name.startswith("checking-") and name.endswith(".json"))
    _require(contexts, f"No checking contexts generated for {fixture.name}")
    _require(set(actual) == {"plutus.json", "assurance.json", *contexts},
             f"Unexpected bundle files for {fixture.name}")
    golden = fixture / "golden"
    if not golden.exists():
        return {"status": "skipped", "reason": "No committed golden for this fixture",
                "files": sorted(actual)}
    expected = _files(golden / "assurance")
    _require(actual.keys() == expected.keys(), f"{fixture.name}: bundle file set changed")
    golden_assurance = _blueprint_binding(expected)
    version = json.loads(actual["plutus.json"])["preamble"]["compiler"]["version"]
    golden_version = json.loads(expected["plutus.json"])["preamble"]["compiler"]["version"]
    _require("+" in version and "+" in golden_version,
             f"{fixture.name}: compiler version lacks commit provenance")
    _require(version.split("+", 1)[0] == golden_version.split("+", 1)[0],
             f"{fixture.name}: compiler release changed")
    for filename, before, after in (
        ("plutus.json", version, golden_version),
        ("assurance.json", assurance["blueprint"]["hash"]["digest"],
         golden_assurance["blueprint"]["hash"]["digest"]),
    ):
        before, after = before.encode(), after.encode()
        _require(actual[filename].count(before) == 1,
                 f"{fixture.name}/{filename}: ambiguous provenance normalization")
        actual[filename] = actual[filename].replace(before, after, 1)
    for filename, content in expected.items():
        _require(actual[filename] == content,
                 f"{fixture.name}/{filename}: golden bundle differs")
    _require((golden / "environment.json").read_bytes() == environment,
             f"{fixture.name}: exported synthetic environment differs")
    return {"status": "passed", "files": sorted(actual),
            "normalization": "compiler commit suffix and consequent blueprint digest only"}


class _Commands:
    def __init__(self, output):
        self.output = output
        self.records = []

    def run(self, name, command, cwd, env):
        stdout = self.output / f"{name}.stdout"
        stderr = self.output / f"{name}.stderr"
        record = {"name": name, "command": command, "cwd": str(cwd),
                  "stdout": str(stdout), "stderr": str(stderr),
                  "environment": {key: env[key] for key in
                                  ("HOME", "XDG_CACHE_HOME", "XDG_CONFIG_HOME", "XDG_DATA_HOME",
                                   "NO_COLOR", "CARGO_NET_OFFLINE") if key in env}}
        self.records.append(record)
        _save_json(self.output / "commands.json", self.records)
        started = time.monotonic()
        try:
            result = subprocess.run(command, cwd=cwd, env=env, capture_output=True)
        except OSError as error:
            stdout.write_bytes(b"")
            stderr.write_text(str(error) + "\n")
            record["error"] = str(error)
            raise RuntimeError(f"Cannot execute {name}; see {stderr}: {error}") from error
        else:
            stdout.write_bytes(result.stdout)
            stderr.write_bytes(result.stderr)
            record["returncode"] = result.returncode
            return result
        finally:
            record["seconds"] = round(time.monotonic() - started, 3)
            _save_json(self.output / "commands.json", self.records)


def _copy_fixture(fixture, project, environment):
    shutil.copytree(fixture, project, ignore=shutil.ignore_patterns("golden"))
    (project / "environment.json").write_bytes(environment)
    os.utime(project / "aiken.toml", (MANIFEST_MTIME, MANIFEST_MTIME))
    env = dict(os.environ, NO_COLOR="1", CARGO_NET_OFFLINE="true")
    for variable, directory in (("HOME", ".home"), ("XDG_CACHE_HOME", ".cache"),
                                ("XDG_CONFIG_HOME", ".config"), ("XDG_DATA_HOME", ".data")):
        path = project / directory
        path.mkdir()
        env[variable] = str(path)
    return env


def validate_fixtures(root: Path, output: Path, aiken: Path | None = None) -> dict:
    """Create a new output directory containing regenerated bundles and all logs.

    With no binary, build this checkout using installed Cargo dependencies only.
    Positive fixtures are discovered by their manifests. Negative expectations
    reuse UAL-13's producer-rejection inventory, never its consumer verdicts.
    On failure, retain the partial report and raise RuntimeError. This function
    does not run Lean/Blaster or modify fixtures, goldens, or environment sources.
    """
    root = root.expanduser().resolve()
    output = output.expanduser().resolve()
    fixtures = root / "crates/aiken/tests/fixtures/ual"
    _require(not output.is_relative_to(fixtures.resolve()),
             "Fixture output must not be inside the source fixture tree")
    try:
        output.mkdir(parents=True, exist_ok=False)
    except OSError as error:
        raise RuntimeError(f"Cannot create fresh fixture output {output}: {error}") from error
    commands = _Commands(output)
    report = {
        "status": "running", "output": str(output),
        "environment": {"kind": "synthetic", "source": str(fixtures / "environment.json"),
                        "consumerIdentityValidated": False},
        "validation": "aiken verify generate reuses aiken_ual schema, cross-document, function, "
                      "and environment binding validation before publication",
        "consumer": {"status": "skipped", "reason": "Producer fixtures use a synthetic environment; "
                     "real consumer conformance must be run separately with UAL-13 e2e"},
        "positive": [], "negative": [], "commands": str(output / "commands.json"),
    }
    try:
        environment = (fixtures / "environment.json").read_bytes()
        report["environment"]["sha256"] = hashlib.sha256(environment).hexdigest()
        positives = sorted((fixtures / "positive").rglob("aiken.toml"))
        positives = [path.parent for path in positives if "golden" not in path.relative_to(fixtures).parts]
        _require(positives, "No positive UAL fixture manifests found")
        negatives = {path.parent.relative_to(fixtures / "negative").as_posix()
                     for path in (fixtures / "negative").rglob("aiken.toml")}
        _require(negatives == set(NEGATIVES),
                 "Negative fixture inventory differs from UAL-13 expectations; review the harness")
        if aiken is None:
            env = dict(os.environ, CARGO_NET_OFFLINE="true", NO_COLOR="1")
            built = commands.run("build", ["cargo", "build", "-p", "aiken", "--locked"], root, env)
            _require(built.returncode == 0, f"Aiken build failed; see {output / 'build.stderr'}")
            metadata = commands.run("metadata", ["cargo", "metadata", "--no-deps", "--format-version=1"],
                                    root, env)
            _require(metadata.returncode == 0, f"Cargo metadata failed; see {output / 'metadata.stderr'}")
            aiken = Path(json.loads(metadata.stdout)["target_directory"]) / "debug/aiken"
        aiken = aiken.expanduser().resolve()
        _require(aiken.is_file() and os.access(aiken, os.X_OK), f"Missing executable Aiken CLI: {aiken}")
        report["aiken"] = str(aiken)
        with aiken.open("rb") as binary:
            report["aikenSha256"] = hashlib.file_digest(binary, "sha256").hexdigest()
        for fixture in positives + [fixtures / "negative" / name for name in sorted(NEGATIVES)]:
            relative = fixture.relative_to(fixtures)
            positive = relative.parts[0] == "positive"
            project = output / "projects" / relative
            env = _copy_fixture(fixture, project, environment)
            name = "__".join(relative.parts)
            item = {"fixture": relative.as_posix(), "project": str(project), "status": "running"}
            report["positive" if positive else "negative"].append(item)
            result = commands.run(name, [str(aiken), "verify", "generate", "--environment", "environment.json"],
                                  project, env)
            item["returncode"] = result.returncode
            if positive:
                _require(result.returncode == 0, f"{relative}: generation failed; see {output / (name + '.stderr')}")
                item["golden"] = _compare_golden(project, fixture, environment)
                item["selfValidation"] = "passed via verify generate"
            else:
                diagnostic = NEGATIVES[fixture.relative_to(fixtures / "negative").as_posix()]
                text = (result.stdout + result.stderr).decode(errors="replace")
                _require(result.returncode > 0 and diagnostic in text,
                         f"{relative}: expected nonzero producer rejection with {diagnostic!r}; "
                         f"see {output / (name + '.stderr')}")
                _require(not (project / "assurance").exists(), f"{relative}: rejected source emitted a bundle")
                item["expectedDiagnostic"] = diagnostic
            item["status"] = "passed"
        report["status"] = "passed"
        return report
    except (RuntimeError, OSError, ValueError, KeyError, TypeError, subprocess.SubprocessError) as error:
        report["status"] = "failed"
        report["error"] = str(error)
        for item in report["positive"] + report["negative"]:
            if item["status"] == "running":
                item["status"] = "failed"
                item["error"] = str(error)
        raise RuntimeError(f"Fixture validation failed: {error}; report: {output / 'fixture-validation.json'}") from error
    finally:
        _save_json(output / "fixture-validation.json", report)
