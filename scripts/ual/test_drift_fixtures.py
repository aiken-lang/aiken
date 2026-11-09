"""Run with: python3 -m unittest discover -s scripts/ual -p 'test_*.py'."""

import hashlib
import json
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest
from unittest.mock import patch

from drift_fixtures import MANIFEST_MTIME, _Commands, _compare_golden, validate_fixtures
from e2e import POSITIVES
from fixture_cases import NEGATIVES


ROOT = Path(__file__).resolve().parents[2]
FIXTURES = ROOT / "crates/aiken/tests/fixtures/ual"


class GoldenTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.project = Path(self.temporary.name) / "project"
        self.fixture = FIXTURES / "positive/native_function"
        self.environment = (FIXTURES / "environment.json").read_bytes()
        shutil.copytree(self.fixture / "golden", self.project)

    def compare(self):
        return _compare_golden(self.project, self.fixture, self.environment)

    def change_version(self, release=False, bind=True):
        blueprint = self.project / "assurance/plutus.json"
        assurance = self.project / "assurance/assurance.json"
        version = json.loads(blueprint.read_bytes())["preamble"]["compiler"]["version"]
        replacement = ("99.0.0" if release else version.split("+", 1)[0]) + "+new-commit"
        blueprint.write_bytes(blueprint.read_bytes().replace(version.encode(), replacement.encode()))
        if bind:
            previous = json.loads(assurance.read_bytes())["blueprint"]["hash"]["digest"]
            digest = hashlib.sha256(blueprint.read_bytes()).hexdigest()
            assurance.write_bytes(assurance.read_bytes().replace(previous.encode(), digest.encode()))

    def test_exact_golden_and_commit_only_changes_pass(self):
        self.assertEqual(self.compare()["status"], "passed")
        self.change_version()
        self.assertEqual(self.compare()["status"], "passed")

    def test_release_change_is_not_normalized(self):
        self.change_version(release=True)
        with self.assertRaisesRegex(RuntimeError, "compiler release changed"):
            self.compare()

    def test_stale_blueprint_binding_cannot_be_hidden_by_normalization(self):
        self.change_version(bind=False)
        with self.assertRaisesRegex(RuntimeError, "binding differs"):
            self.compare()

    def test_context_and_extra_file_changes_fail(self):
        context = self.project / "assurance/checking-outbids.json"
        context.write_bytes(context.read_bytes() + b"\n")
        with self.assertRaisesRegex(RuntimeError, "golden bundle differs"):
            self.compare()
        (self.project / "assurance/unexpected.json").write_text("{}")
        with self.assertRaisesRegex(RuntimeError, "Unexpected bundle files"):
            self.compare()

    def test_synthetic_export_must_match(self):
        self.environment += b"\n"
        with self.assertRaisesRegex(RuntimeError, "exported synthetic environment differs"):
            self.compare()


class FixtureRunnerTests(unittest.TestCase):
    def test_all_fixtures_are_isolated_and_no_consumer_success_is_claimed(self):
        before = {path: (path.read_bytes(), path.stat().st_mtime_ns)
                  for path in FIXTURES.rglob("*") if path.is_file()}
        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory) / "fixtures"
            binary = Path(directory) / "aiken"
            binary.write_text("unused: subprocess is mocked in this orchestration unit test")
            binary.chmod(0o755)

            def generated(command, cwd, env, capture_output):
                self.assertEqual(command[1:], ["verify", "generate", "--environment", "environment.json"])
                self.assertEqual(env["HOME"], str(cwd / ".home"))
                self.assertEqual((cwd / "aiken.toml").stat().st_mtime, MANIFEST_MTIME)
                self.assertFalse((cwd / "golden").exists())
                self.assertEqual((cwd / "environment.json").read_bytes(),
                                 (FIXTURES / "environment.json").read_bytes())
                relative = cwd.relative_to(output / "projects")
                if relative.parts[0] == "positive":
                    shutil.copytree(FIXTURES / relative / "golden/assurance", cwd / "assurance")
                    return subprocess.CompletedProcess(command, 0, b"generated", b"")
                diagnostic = NEGATIVES[Path(*relative.parts[1:]).as_posix()]
                return subprocess.CompletedProcess(command, 1, b"", diagnostic.encode())

            with patch("drift_fixtures.subprocess.run", side_effect=generated):
                report = validate_fixtures(ROOT, output, binary)
            self.assertEqual(report["status"], "passed")
            self.assertEqual(len(report["positive"]), len(POSITIVES))
            self.assertEqual(len(report["negative"]), len(NEGATIVES))
            self.assertEqual(report["environment"]["kind"], "synthetic")
            self.assertFalse(report["environment"]["consumerIdentityValidated"])
            self.assertEqual(report["consumer"]["status"], "skipped")
            self.assertEqual(json.loads((output / "fixture-validation.json").read_text()), report)
            commands = json.loads((output / "commands.json").read_text())
            self.assertEqual(len(commands), len(POSITIVES) + len(NEGATIVES))
            for command in commands:
                self.assertTrue(Path(command["stdout"]).is_file())
                self.assertTrue(Path(command["stderr"]).is_file())
        self.assertEqual(before, {path: (path.read_bytes(), path.stat().st_mtime_ns)
                                  for path in FIXTURES.rglob("*") if path.is_file()})

    def test_generation_failure_retains_logs_and_failed_report(self):
        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory) / "fixtures"
            with patch("drift_fixtures.subprocess.run", return_value=subprocess.CompletedProcess(
                    [], 1, b"", b"schema validation failed")):
                with self.assertRaisesRegex(RuntimeError, "generation failed"):
                    validate_fixtures(ROOT, output, Path("/bin/sh"))
            report = json.loads((output / "fixture-validation.json").read_text())
            self.assertEqual(report["status"], "failed")
            self.assertEqual(report["positive"][0]["status"], "failed")
            records = json.loads((output / "commands.json").read_text())
            self.assertEqual(Path(records[0]["stderr"]).read_text(), "schema validation failed")

    def test_command_spawn_failure_is_logged(self):
        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory)
            commands = _Commands(output)
            with patch("drift_fixtures.subprocess.run", side_effect=FileNotFoundError("missing cargo")):
                with self.assertRaisesRegex(RuntimeError, "Cannot execute build"):
                    commands.run("build", ["cargo", "build"], output, {})
            self.assertIn("missing cargo", (output / "build.stderr").read_text())
            self.assertIn("error", json.loads((output / "commands.json").read_text())[0])

    def test_source_tree_output_is_rejected_before_writing(self):
        with self.assertRaisesRegex(RuntimeError, "source fixture tree"):
            validate_fixtures(ROOT, FIXTURES / "must-not-be-created", Path("/bin/sh"))
        self.assertFalse((FIXTURES / "must-not-be-created").exists())


if __name__ == "__main__":
    unittest.main()
