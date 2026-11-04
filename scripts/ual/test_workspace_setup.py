"""Run with: python3 -m unittest discover -s scripts/ual -p 'test_*.py'."""

import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

from workspace_setup import artifacts, check_verdicts, overrides


VERIFIED = "verified (Blaster SMT; no reconstructed Lean proof)"


class WorkspaceSetupTests(unittest.TestCase):
    def test_missing_toolchain_never_invokes_elan_or_other_tools(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            marker = root / "tool-was-invoked"
            for name in ("git", "elan", "z3"):
                tool = root / name
                tool.write_text(f'#!/bin/sh\n: > "{marker}"\nexit 1\n')
                tool.chmod(0o755)
            workspace = root / "workspace"
            result = subprocess.run(
                [sys.executable, str(Path(__file__).with_name("workspace_setup.py")),
                 "--workspace", str(workspace)],
                env=dict(os.environ, PATH=str(root), ELAN_HOME=str(root / "empty-elan")),
                capture_output=True, text=True,
            )
            self.assertEqual(result.returncode, 1)
            self.assertIn("Missing installed Lean", result.stderr)
            self.assertIn("no download attempted", result.stderr)
            self.assertFalse(marker.exists(), result.stderr)
            self.assertFalse(workspace.exists())

    def test_all_expected_properties_must_be_verified_exactly_once(self):
        expected = ["correct_guess", "wrong_guess", "malformed_datum"]
        valid = "\n".join(f"Property '{name}': {VERIFIED}" for name in expected)
        self.assertEqual(check_verdicts(valid, expected), valid.splitlines())
        with_progress = "\n".join(
            f"Property '{name}': checking fresh proposition\nProperty '{name}': {VERIFIED}"
            for name in expected
        )
        self.assertEqual(check_verdicts(with_progress, expected), valid.splitlines())
        for invalid in (
            valid.replace(VERIFIED, "falsified", 1),
            valid.replace(VERIFIED, "inconclusive", 1),
            valid.replace(VERIFIED, "not checked (unsupported)", 1),
            valid.replace(VERIFIED, "verified with an unknown trust base", 1),
            "\n".join(valid.splitlines()[:-1]),
            valid + "\n" + valid.splitlines()[0],
            valid.replace("correct_guess", "unexpected_property"),
            "",
        ):
            with self.subTest(output=invalid), self.assertRaisesRegex(RuntimeError, "did not verify"):
                check_verdicts(invalid, expected)

    def test_existing_overrides_are_never_rewritten(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            packages = [("Blaster", root / "Lean-blaster")]
            overrides(root, packages, False)
            path = root / ".lake/package-overrides.json"
            before = (path.read_bytes(), path.stat().st_mtime_ns)
            overrides(root, packages, True)
            self.assertEqual((path.read_bytes(), path.stat().st_mtime_ns), before)
            with self.assertRaisesRegex(RuntimeError, "refusing to overwrite"):
                overrides(root, [("Blaster", root / "different")], False)
            self.assertEqual((path.read_bytes(), path.stat().st_mtime_ns), before)
            self.assertEqual(json.loads(path.read_text())["packages"][0]["dir"], str(packages[0][1]))

    def test_verify_only_cannot_create_missing_overrides(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            with self.assertRaisesRegex(RuntimeError, "missing"):
                overrides(root, [], True)
            self.assertFalse((root / ".lake").exists())

    def test_artifact_removal_or_modification_invalidates_inventory(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            repo = root / "Consumer"
            build = repo / ".lake/build"
            build.mkdir(parents=True)
            module = build / "Assurance.olean"
            module.write_bytes(b"compiled module")
            before = artifacts(root, [repo])
            self.assertEqual(artifacts(root, [repo]), before)
            module.write_bytes(b"different module")
            self.assertNotEqual(artifacts(root, [repo]), before)
            module.unlink()
            self.assertNotEqual(artifacts(root, [repo]), before)


if __name__ == "__main__":
    unittest.main()
