"""Offline contract/exit classification regressions; no consumer or network."""

import argparse
import json
from pathlib import Path
import subprocess
import tempfile
import unittest

import drift


class DriftTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        self.sources = self.root / "sources"
        self.schemas = self.root / "schemas"
        self.schemas.mkdir()
        self.pins = {}
        for name in drift.REPOS:
            repo = self.sources / name
            repo.mkdir(parents=True)
            self.git(repo, "init", "--quiet")
            self.git(repo, "config", "user.email", "test@example.org")
            self.git(repo, "config", "user.name", "Test")
            (repo / "README.md").write_text("Public source fixture\n")
            if name == "PlutusCoreBlaster":
                (repo / drift.ENCODING).mkdir(parents=True)
                (repo / drift.ASSURANCE).write_text(
                    'logInfo m!"Property \'{p.id}\': verified (Blaster SMT; no reconstructed Lean proof)"\n'
                    'logInfo m!"Assurance \'{doc.title}\': {checked} checked, {skipped} not checked, {failed} failed/inconclusive"\n')
                (repo / (drift.ENCODING + "value.schema.json")).write_text('{"type":"object"}\n')
            if name == "CardanoLedgerApiBlaster":
                (repo / drift.CONTEXTS).parent.mkdir(parents=True)
                (repo / drift.CONTEXTS).write_text(
                    'inductive ScriptInfo where\n  | SpendingScript : Ref → Datum → ScriptInfo\n'
                    '  | MintingScript : Symbol → ScriptInfo\nderiving Repr\n')
            self.commit(repo)
            self.pins[name] = self.git(repo, "rev-parse", "HEAD").strip()
        lock = {"repositories": {"ual": {"commit": self.pins["UniversalAnnotationLanguage"]},
                                  "ledger": {"commit": self.pins["CardanoLedgerApiBlaster"]}}}
        core = self.sources / "PlutusCoreBlaster"
        (core / "scripts").mkdir()
        (core / "scripts/assurance-workspace.json").write_text(json.dumps(lock))
        self.commit(core)
        self.pins["PlutusCoreBlaster"] = self.git(core, "rev-parse", "HEAD").strip()
        (self.schemas / "PIN").write_text(
            f"commit: {self.pins['PlutusCoreBlaster']}\n{drift.ENCODING}value.schema.json\n"
            "scripts/assurance-workspace.json\n")
        (self.schemas / "value.schema.json").write_text('{"type":"object"}\n')
        (self.schemas / "assurance-workspace.json").write_text(json.dumps(lock))
        self.output = self.root / "output"
        (self.output / "diffs").mkdir(parents=True)
        self.args = argparse.Namespace(sources=self.sources, schemas=self.schemas, offline=True,
                                       at_pins=False, pr_state=None, previous=None)
        self.report = {"upstreams": {}, "changes": [], "errors": [], "workItems": []}

    def git(self, repo, *args):
        return subprocess.run(["git", *args], cwd=repo, check=True,
                              capture_output=True, text=True).stdout

    def commit(self, repo):
        self.git(repo, "add", ".")
        self.git(repo, "commit", "--quiet", "-m", "fixture")

    def check(self):
        drift.check_contracts(self.args, self.output, self.report)
        return drift.EXIT_CODES[drift.outcome(self.report)]

    def test_current_pins_have_no_drift(self):
        self.assertEqual(self.check(), 0)
        self.assertEqual(self.report["changes"], [])

    def test_modified_local_schema_needs_work_with_diff(self):
        (self.schemas / "value.schema.json").write_text('{"type":"string"}\n')
        self.assertEqual(self.check(), 20)
        change, = self.report["changes"]
        self.assertEqual(change["kind"], "vendored")
        self.assertIn('+{"type":"string"}', (self.output / change["diff"]).read_text())

    def test_renamed_script_info_constructor_needs_work(self):
        path = self.sources / "CardanoLedgerApiBlaster" / drift.CONTEXTS
        path.write_text(path.read_text().replace("SpendingScript", "SpendScript"))
        self.assertEqual(self.check(), 20)
        change, = self.report["changes"]
        self.assertEqual(change["kind"], "script-info")
        self.assertIn("+  | SpendScript", (self.output / change["diff"]).read_text())

    def test_constructor_argument_change_needs_work(self):
        path = self.sources / "CardanoLedgerApiBlaster" / drift.CONTEXTS
        path.write_text(path.read_text().replace("Ref → Datum", "Ref → Option Datum"))
        self.assertEqual(self.check(), 20)
        self.assertEqual(self.report["changes"][0]["kind"], "script-info")

    def test_changed_verdict_message_needs_work(self):
        path = self.sources / "PlutusCoreBlaster" / drift.ASSURANCE
        path.write_text(path.read_text().replace("verified (Blaster SMT", "valid (Blaster SMT"))
        self.assertEqual(self.check(), 20)
        self.assertIn("verdicts", [c["kind"] for c in self.report["changes"]])

    def test_missing_verdict_source_fails_closed(self):
        (self.sources / "PlutusCoreBlaster" / drift.ASSURANCE).unlink()
        with self.assertRaisesRegex(RuntimeError, "Missing Assurance"):
            self.check()

    def test_unrelated_ledger_commit_is_repin_only(self):
        repo = self.sources / "CardanoLedgerApiBlaster"
        (repo / "README.md").write_text("Documentation update\n")
        self.commit(repo)
        self.assertEqual(self.check(), 10)

    def test_new_core_revision_requires_reader_registration(self):
        repo = self.sources / "PlutusCoreBlaster"
        (repo / "README.md").write_text("Documentation update\n")
        self.commit(repo)
        self.assertEqual(self.check(), 20)
        self.assertIn("PinnedVerdictReader", self.report["workItems"][0])

    def test_pin_mode_ignores_candidate_worktree_not_local_schema(self):
        path = self.sources / "CardanoLedgerApiBlaster" / drift.CONTEXTS
        path.write_text("candidate removed declaration\n")
        self.args.at_pins = True
        self.assertEqual(self.check(), 0)

    def test_pr_transition_is_reported_once_against_previous_report(self):
        states = {f"{repo}#{number}": {"state": "open", "draft": repo == "CardanoLedgerApiBlaster"}
                  for repo, numbers in drift.TRACKED_PRS.items() for number in numbers}
        states["PlutusCoreBlaster#70"] = {"state": "merged", "draft": False}
        self.args.pr_state = self.root / "prs.json"
        self.args.pr_state.write_text(json.dumps(states))
        drift.check_prs(self.args, self.report)
        self.assertEqual(drift.outcome(self.report), "needs-work")
        self.assertTrue(self.report["pullRequests"]["items"]["PlutusCoreBlaster#70"]["changed"])
        self.args.previous = self.root / "previous.json"
        self.args.previous.write_text(json.dumps(self.report))
        self.report["workItems"] = []
        drift.check_prs(self.args, self.report)
        self.assertFalse(self.report["pullRequests"]["items"]["PlutusCoreBlaster#70"]["changed"])
        self.assertEqual(self.report["workItems"], [])

    def test_incomplete_check_overrides_repin(self):
        self.report["upstreams"] = {"ledger": {"pin": "a", "head": "b", "changedFiles": []}}
        self.report["errors"] = [{"check": "fixtures", "message": "generation failed"}]
        self.assertEqual(drift.EXIT_CODES[drift.outcome(self.report)], 20)

    def test_lean_comments_do_not_displace_verdict_literals(self):
        text = '-- A standalone quote: "\nlogInfo m!"Property \'{p.id}\': falsified"\n'
        text += 'logInfo m!"Assurance \'{doc.title}\': {failed} failed/inconclusive"\n'
        self.assertEqual(drift.verdicts(text),
                         "Property '{p.id}': falsified\n"
                         "Assurance '{doc.title}': {failed} failed/inconclusive\n")

    def test_schema_line_endings_are_byte_drift(self):
        (self.schemas / "value.schema.json").write_bytes(b'{"type":"object"}\r\n')
        self.assertEqual(self.check(), 20)
        change, = self.report["changes"]
        self.assertNotEqual(change["beforeSha256"], change["afterSha256"])

    def test_missing_consumer_is_optional_but_invalid_checkout_is_not(self):
        self.args.workspace = self.root / "consumer"
        drift.check_e2e(self.args, self.output, self.report)
        self.assertEqual(self.report["e2e"]["status"], "skipped")
        (self.args.workspace / "CardanoLedgerApiBlaster").mkdir(parents=True)
        with self.assertRaisesRegex(RuntimeError, "has no lakefile"):
            drift.check_e2e(self.args, self.output, self.report)


if __name__ == "__main__":
    unittest.main()
