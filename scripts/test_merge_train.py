#!/usr/bin/env python3
"""Tests for the merge train (scripts/merge_train.py, #2553 item 1):

    python3 -m unittest scripts.test_merge_train -v

The train batches ready pull requests onto one `train/*` branch, runs the full CI tier there once, and merges the batch
only if that exact tree passed, so master never advances to a combination nobody tested. A red batch has its failed jobs
re-run once (owner's flake rule), then is halved until the culprit is found and ejected.
"""

from __future__ import annotations

import subprocess
import tempfile
import unittest
from datetime import datetime, timedelta, timezone
from pathlib import Path

from scripts import merge_train as mt

_NOW = datetime(2026, 11, 3, 16, 0, tzinfo=timezone.utc)
_CI = ".github/workflows/ci.yml"
_XOS = ".github/workflows/cross-os-tests.yml"


def _pr(number: int, head: str | None = None) -> mt.TrainPr:
    return mt.TrainPr(number=number, head=head or f"{number:040x}", title=f"Change {number}")


def _manifest(*numbers: int, base: str = "b" * 40, created: datetime = _NOW) -> mt.Manifest:
    return mt.Manifest(base=base, prs=tuple(_pr(n) for n in numbers), created=created)


def _run(path: str, conclusion: str | None = "success", attempt: int = 1, run_id: int = 1) -> mt.Run:
    status = "completed" if conclusion is not None else "in_progress"
    return mt.Run(id=run_id, path=path, status=status, conclusion=conclusion, attempt=attempt, url=f"https://x/{run_id}")


def _heads(manifest: mt.Manifest) -> dict[int, str | None]:
    return {pr.number: pr.head for pr in manifest.prs}


def _candidate(number: int, **overrides: object) -> mt.Candidate:
    fields: dict[object, object] = dict(
        number=number,
        head=f"{number:040x}",
        title=f"Change {number}",
        draft=False,
        base="master",
        same_repo=True,
        head_ref=f"branch-{number}",
        ci_green=True,
        ejected_heads=frozenset(),
    )
    fields.update(overrides)
    return mt.Candidate(**fields)  # type: ignore[arg-type]


class ManifestTests(unittest.TestCase):
    def test_a_manifest_survives_a_commit_message_round_trip(self) -> None:
        manifest = _manifest(12, 7)
        message = manifest.commit_message()
        self.assertTrue(message.startswith("Merge train: #12 #7\n"))
        self.assertEqual(manifest, mt.Manifest.from_commit_message(message))

    def test_a_commit_without_a_manifest_is_not_a_train(self) -> None:
        self.assertIsNone(mt.Manifest.from_commit_message("Fix a bug (#12)\n\nBody\n"))


class SelectionTests(unittest.TestCase):
    def test_takes_the_oldest_eligible_pull_requests_up_to_the_batch_size(self) -> None:
        batch = mt.select_batch([_candidate(n) for n in (9, 3, 5, 7)], limit=3)
        self.assertEqual([3, 5, 7], [c.number for c in batch])

    def test_skips_drafts_other_bases_forks_and_pull_requests_whose_ci_is_not_green(self) -> None:
        candidates = [
            _candidate(1, draft=True),
            _candidate(2, base="release"),
            _candidate(3, same_repo=False),
            _candidate(4, ci_green=False),
            _candidate(5),
        ]
        self.assertEqual([5], [c.number for c in mt.select_batch(candidates, limit=6)])

    def test_an_ejected_head_stays_out_until_the_author_pushes_again(self) -> None:
        ejected = _candidate(4, ejected_heads=frozenset({f"{4:040x}"}))
        pushed_again = _candidate(5, ejected_heads=frozenset({"0" * 40}))
        self.assertEqual([5], [c.number for c in mt.select_batch([ejected, pushed_again], limit=6)])

    def test_a_bisect_step_is_limited_to_the_pull_requests_it_names(self) -> None:
        batch = mt.select_batch([_candidate(n) for n in (1, 2, 3, 4)], limit=6, only=(3, 1))
        self.assertEqual([1, 3], [c.number for c in batch])


class DecisionTests(unittest.TestCase):
    def _decide(self, manifest: mt.Manifest, runs: list[mt.Run], **kw: object) -> mt.Action:
        heads = kw.pop("heads", _heads(manifest))
        master = kw.pop("master", manifest.base)
        now = kw.pop("now", _NOW)
        return mt.decide(manifest, runs, heads=heads, master=master, now=now)  # type: ignore[arg-type]

    def test_merges_when_every_required_workflow_passed(self) -> None:
        action = self._decide(_manifest(1, 2), [_run(_CI, run_id=1), _run(_XOS, run_id=2)])
        self.assertEqual(mt.Merge(flaky_runs=()), action)

    def test_waits_while_a_required_workflow_is_running_or_has_not_started(self) -> None:
        self.assertIsInstance(self._decide(_manifest(1), [_run(_CI), _run(_XOS, conclusion=None)]), mt.Wait)
        self.assertIsInstance(self._decide(_manifest(1), [_run(_CI)]), mt.Wait)

    def test_the_newest_run_of_a_workflow_is_the_one_that_counts(self) -> None:
        runs = [_run(_CI, "failure", run_id=1), _run(_CI, run_id=5), _run(_XOS, run_id=2)]
        self.assertEqual(mt.Merge(flaky_runs=()), self._decide(_manifest(1), runs))

    def test_a_failed_first_attempt_is_re_run_once(self) -> None:
        runs = [_run(_CI, "failure", run_id=1), _run(_XOS, "cancelled", run_id=2)]
        self.assertEqual(mt.Rerun(run_ids=(1, 2)), self._decide(_manifest(1, 2), runs))

    def test_a_pass_on_the_re_run_merges_and_names_the_run_as_flaky(self) -> None:
        runs = [_run(_CI, attempt=2, run_id=1), _run(_XOS, run_id=2)]
        self.assertEqual(mt.Merge(flaky_runs=(runs[0],)), self._decide(_manifest(1, 2), runs))

    def test_a_batch_still_red_after_the_re_run_is_halved_oldest_half_first(self) -> None:
        runs = [_run(_CI, "failure", attempt=2, run_id=1), _run(_XOS, run_id=2)]
        self.assertEqual(mt.Bisect(numbers=(1, 2, 3)), self._decide(_manifest(1, 2, 3, 4, 5), runs))

    def test_a_single_pull_request_still_red_after_the_re_run_is_ejected(self) -> None:
        failed = _run(_XOS, "failure", attempt=2, run_id=2)
        action = self._decide(_manifest(7), [_run(_CI, run_id=1), failed])
        self.assertEqual(mt.Eject(number=7, failed_runs=(failed,)), action)

    def test_abandons_when_master_moved_under_the_train(self) -> None:
        action = self._decide(_manifest(1), [_run(_CI), _run(_XOS)], master="c" * 40)
        self.assertIsInstance(action, mt.Abandon)

    def test_abandons_when_a_pull_request_got_a_new_head_or_was_closed(self) -> None:
        manifest = _manifest(1, 2)
        for heads in ({1: "f" * 40, 2: _pr(2).head}, {1: _pr(1).head, 2: None}):
            with self.subTest(heads=heads):
                self.assertIsInstance(self._decide(manifest, [_run(_CI), _run(_XOS)], heads=heads), mt.Abandon)

    def test_abandons_a_train_that_never_finished(self) -> None:
        stale = _manifest(1, created=_NOW - timedelta(hours=7))
        self.assertIsInstance(self._decide(stale, [_run(_CI)]), mt.Abandon)


# ----------------------------------------------------------------------------------------------------------------------
# End to end against real git repositories: a bare "origin" holding master and refs/pull/N/head, and a fake GitHub API
# whose merges are real squash merges into that origin, so the tested tree and the merged tree can be compared.


def _git(cwd: Path, *args: str) -> str:
    result = subprocess.run(
        ["git", "-c", "user.name=t", "-c", "user.email=t@t", "-c", "init.defaultBranch=master", *args],
        cwd=cwd,
        capture_output=True,
        text=True,
        check=True,
    )
    return result.stdout.strip()


class Origin:
    def __init__(self, root: Path) -> None:
        self.bare = root / "origin.git"
        self.work = root / "author"
        _git(root, "init", "--bare", str(self.bare))
        _git(root, "clone", str(self.bare), str(self.work))
        (self.work / "README").write_text("base\n")
        _git(self.work, "add", ".")
        _git(self.work, "commit", "-m", "Initial")
        _git(self.work, "push", "origin", "HEAD:refs/heads/master")

    def master(self) -> str:
        return _git(self.bare, "rev-parse", "refs/heads/master")

    def open_pr(self, number: int, path: str, text: str) -> str:
        _git(self.work, "fetch", "origin", "master")
        _git(self.work, "checkout", "-q", "-B", f"pr{number}", "origin/master")
        (self.work / path).write_text(text)
        _git(self.work, "add", ".")
        _git(self.work, "commit", "-m", f"PR {number}")
        _git(self.work, "push", "-f", "origin", f"HEAD:refs/pull/{number}/head", f"HEAD:refs/heads/branch-{number}")
        return _git(self.work, "rev-parse", "HEAD")

    def squash_merge(self, number: int, title: str) -> None:
        _git(self.work, "fetch", "origin", "master", f"refs/pull/{number}/head")
        _git(self.work, "checkout", "-q", "-B", "merging", "origin/master")
        _git(self.work, "merge", "--squash", "FETCH_HEAD")
        _git(self.work, "commit", "-m", title)
        _git(self.work, "push", "origin", "HEAD:refs/heads/master")


    def commit_to_master(self, path: str, text: str) -> None:
        _git(self.work, "fetch", "origin", "master")
        _git(self.work, "checkout", "-q", "-B", "direct", "origin/master")
        (self.work / path).write_text(text)
        _git(self.work, "add", ".")
        _git(self.work, "commit", "-m", "Direct")
        _git(self.work, "push", "origin", "HEAD:refs/heads/master")


class FakeGitHub:
    def __init__(self, origin: Origin) -> None:
        self.origin = origin
        self.candidates: dict[int, mt.Candidate] = {}
        self.runs: dict[str, list[mt.Run]] = {}
        self.dispatched: list[tuple[str, str]] = []
        self.reruns: list[int] = []
        self.cancelled: list[int] = []
        self.merged: list[int] = []
        self.deleted_branches: list[str] = []
        self.comments: list[tuple[int, str]] = []
        self.failed_jobs: dict[int, list[str]] = {}
        self.merge_fails_for: set[int] = set()
        self.extra_on_merge: tuple[str, str] | None = None

    def add_pr(self, number: int, path: str, text: str, **overrides: object) -> None:
        head = self.origin.open_pr(number, path, text)
        self.candidates[number] = _candidate(number, head=head, **overrides)

    def open_candidates(self) -> list[mt.Candidate]:
        return [c for n, c in sorted(self.candidates.items()) if n not in self.merged]

    def head_of(self, number: int) -> str | None:
        return None if number in self.merged else self.candidates[number].head

    def runs_for(self, branch: str) -> list[mt.Run]:
        return self.runs.get(branch, [])

    def dispatch(self, workflow: str, ref: str) -> None:
        self.dispatched.append((workflow, ref))

    def rerun_failed_jobs(self, run_id: int) -> None:
        self.reruns.append(run_id)

    def cancel_run(self, run_id: int) -> None:
        self.cancelled.append(run_id)

    def failed_job_names(self, run_id: int, attempt: int) -> list[str]:
        return self.failed_jobs.get(run_id, [])

    def squash_merge(self, number: int, head: str, title: str) -> None:
        if number in self.merge_fails_for:
            raise mt.MergeRefused(f"#{number} head moved")
        self.origin.squash_merge(number, title)
        self.merged.append(number)
        if self.extra_on_merge is not None:
            self.origin.commit_to_master(*self.extra_on_merge)

    def delete_branch(self, name: str) -> None:
        self.deleted_branches.append(name)

    def comment(self, number: int, body: str) -> None:
        self.comments.append((number, body))


class TrainTests(unittest.TestCase):
    def setUp(self) -> None:
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name)
        self.origin = Origin(self.root)
        self.github = FakeGitHub(self.origin)
        self.clone = self.root / "runner"
        _git(self.root, "clone", "-q", str(self.origin.bare), str(self.clone))
        self.log: list[str] = []
        self.clock = _NOW

    def _train(self, dry_run: bool = False, flake_issue: int | None = 99) -> mt.Train:
        return mt.Train(
            github=self.github,
            git=mt.Git(self.clone),
            batch=6,
            dry_run=dry_run,
            flake_issue=flake_issue,
            now=lambda: self.clock,
            log=self.log.append,
        )

    def _live_branch(self) -> str | None:
        refs = _git(self.origin.bare, "for-each-ref", "--format=%(refname:short)", "refs/heads/train/")
        return refs or None

    def _finish(self, branch: str, ci: str = "success", xos: str = "success", attempt: int = 1) -> None:
        self.github.runs[branch] = [_run(_CI, ci, attempt, run_id=10), _run(_XOS, xos, attempt, run_id=11)]

    def test_starts_one_train_with_every_eligible_pull_request_and_dispatches_the_full_tier(self) -> None:
        self.github.add_pr(1, "a.txt", "a\n")
        self.github.add_pr(2, "b.txt", "b\n")
        self._train().tick()
        branch = self._live_branch()
        self.assertIsNotNone(branch)
        assert branch is not None
        self.assertEqual({("ci.yml", branch), ("cross-os-tests.yml", branch)}, set(self.github.dispatched))
        message = _git(self.origin.bare, "log", "-1", "--format=%B", branch)
        manifest = mt.Manifest.from_commit_message(message)
        assert manifest is not None
        self.assertEqual([1, 2], [p.number for p in manifest.prs])
        self.assertEqual(self.origin.master(), manifest.base)

    def test_a_conflicting_pull_request_is_left_out_of_the_train(self) -> None:
        self.github.add_pr(1, "same.txt", "one\n")
        self.github.add_pr(2, "same.txt", "two\n")
        self._train().tick()
        branch = self._live_branch()
        assert branch is not None
        manifest = mt.Manifest.from_commit_message(_git(self.origin.bare, "log", "-1", "--format=%B", branch))
        assert manifest is not None
        self.assertEqual([1], [p.number for p in manifest.prs])

    def test_a_green_train_merges_its_pull_requests_in_order_and_master_gets_exactly_the_tested_tree(self) -> None:
        self.github.add_pr(1, "a.txt", "a\n")
        self.github.add_pr(2, "b.txt", "b\n")
        self._train().tick()
        branch = self._live_branch()
        assert branch is not None
        tested_tree = _git(self.origin.bare, "rev-parse", f"{branch}^{{tree}}")
        self._finish(branch)
        self._train().tick()
        self.assertEqual([1, 2], self.github.merged)
        self.assertEqual(tested_tree, _git(self.origin.bare, "rev-parse", "master^{tree}"))
        self.assertIn("branch-1", self.github.deleted_branches)
        self.assertIsNone(self._live_branch())

    def test_a_refused_merge_stops_the_train_before_any_untested_combination_lands(self) -> None:
        for n, name in ((1, "a"), (2, "b"), (3, "c")):
            self.github.add_pr(n, f"{name}.txt", f"{name}\n")
        self._train().tick()
        branch = self._live_branch()
        assert branch is not None
        self._finish(branch)
        self.github.merge_fails_for = {2}
        with self.assertRaises(mt.TrainError):
            self._train().tick()
        self.assertEqual([1], self.github.merged)

    def test_master_ending_on_a_tree_the_train_did_not_test_is_an_error(self) -> None:
        self.github.add_pr(1, "a.txt", "a\n")
        self._train().tick()
        branch = self._live_branch()
        assert branch is not None
        self._finish(branch)
        self.github.extra_on_merge = ("late.txt", "landed beside the train\n")
        with self.assertRaises(mt.TrainError):
            self._train().tick()

    def test_a_red_first_attempt_is_re_run_and_the_train_stays(self) -> None:
        self.github.add_pr(1, "a.txt", "a\n")
        self._train().tick()
        branch = self._live_branch()
        assert branch is not None
        self._finish(branch, xos="failure")
        self._train().tick()
        self.assertEqual([11], self.github.reruns)
        self.assertEqual(branch, self._live_branch())

    def test_a_pass_on_re_run_merges_and_records_the_flake_on_the_ledger_issue(self) -> None:
        self.github.add_pr(1, "a.txt", "a\n")
        self._train().tick()
        branch = self._live_branch()
        assert branch is not None
        self.github.runs[branch] = [_run(_CI, run_id=10), _run(_XOS, attempt=2, run_id=11)]
        self.github.failed_jobs[11] = ["shards-macos / Test (macos, shard 1/2)"]
        self._train().tick()
        self.assertEqual([1], self.github.merged)
        ledger = [body for number, body in self.github.comments if number == 99]
        self.assertEqual(1, len(ledger))
        self.assertIn("Test (macos, shard 1/2)", ledger[0])
        self.assertIn("https://x/11", ledger[0])

    def test_a_red_batch_after_the_re_run_is_bisected_onto_a_new_train_of_its_oldest_half(self) -> None:
        for n, name in ((1, "a"), (2, "b"), (3, "c")):
            self.github.add_pr(n, f"{name}.txt", f"{name}\n")
        self._train().tick()
        first = self._live_branch()
        assert first is not None
        self._finish(first, ci="failure", attempt=2)
        self.clock = _NOW + timedelta(minutes=1)
        self._train().tick()
        second = self._live_branch()
        assert second is not None
        self.assertNotEqual(first, second)
        manifest = mt.Manifest.from_commit_message(_git(self.origin.bare, "log", "-1", "--format=%B", second))
        assert manifest is not None
        self.assertEqual([1, 2], [p.number for p in manifest.prs])
        self.assertEqual([], self.github.merged)

    def test_a_lone_red_pull_request_is_ejected_with_a_marker_for_its_head(self) -> None:
        self.github.add_pr(4, "a.txt", "a\n")
        self._train().tick()
        branch = self._live_branch()
        assert branch is not None
        self._finish(branch, ci="failure", attempt=2)
        self._train().tick()
        (number, body), *_ = self.github.comments
        self.assertEqual(4, number)
        self.assertIn(mt.ejection_marker(self.github.candidates[4].head), body)
        self.assertIn("https://x/10", body)
        self.assertEqual([], self.github.merged)

    def test_an_abandoned_train_cancels_its_runs_and_a_fresh_one_is_built(self) -> None:
        self.github.add_pr(1, "a.txt", "a\n")
        self._train().tick()
        first = self._live_branch()
        assert first is not None
        self.github.runs[first] = [_run(_CI, None, run_id=10), _run(_XOS, None, run_id=11)]
        self.github.add_pr(1, "a.txt", "a2\n")
        self.clock = _NOW + timedelta(minutes=1)
        self._train().tick()
        self.assertEqual([10, 11], sorted(self.github.cancelled))
        second = self._live_branch()
        self.assertIsNotNone(second)
        self.assertNotEqual(first, second)

    def test_dry_run_reports_the_batch_and_changes_nothing(self) -> None:
        self.github.add_pr(1, "a.txt", "a\n")
        self._train(dry_run=True).tick()
        self.assertIsNone(self._live_branch())
        self.assertEqual([], self.github.dispatched)
        self.assertTrue(any("#1" in line for line in self.log))

    def test_nothing_to_do_with_no_eligible_pull_requests(self) -> None:
        self._train().tick()
        self.assertIsNone(self._live_branch())
        self.assertEqual([], self.github.dispatched)


class PerfBaseTests(unittest.TestCase):
    """The perf job benchmarks a commit against its base. On a train the base is the master commit the train was built
    on, named in the manifest; anywhere else it stays the first parent."""

    def test_a_train_tip_names_its_manifest_base_and_any_other_commit_its_first_parent(self) -> None:
        self.assertEqual("b" * 40, mt.perf_base(_manifest(1).commit_message()))
        self.assertEqual("HEAD^1", mt.perf_base("Fix a bug (#12)\n"))


_WORKFLOWS = Path(__file__).resolve().parent.parent / ".github" / "workflows"


def _workflow(name: str) -> str:
    return (_WORKFLOWS / name).read_text()


class WorkflowTests(unittest.TestCase):
    """How .github/workflows/merge-train.yml drives the script, and how the existing workflows make room for it."""

    def setUp(self) -> None:
        self.train = _workflow("merge-train.yml")

    def test_steps_are_never_cancelled_so_a_merge_cannot_be_cut_off_half_way(self) -> None:
        self.assertIn("concurrency:\n  group: merge-train\n  cancel-in-progress: false\n", self.train)

    def test_nothing_runs_unless_the_owner_set_the_mode_variable(self) -> None:
        self.assertIn("vars.MERGE_TRAIN_MODE == 'dry-run' || vars.MERGE_TRAIN_MODE == 'live'", self.train)
        self.assertIn("MERGE_TRAIN_MODE: ${{ vars.MERGE_TRAIN_MODE }}", self.train)
        self.assertIn("python3 scripts/merge_train.py tick", self.train)

    def test_a_finished_run_wakes_the_train_only_when_it_ran_on_a_train_branch(self) -> None:
        self.assertIn("workflows: [CI, Cross-OS Tests]", self.train)
        self.assertIn("startsWith(github.event.workflow_run.head_branch, 'train/')", self.train)
        self.assertRegex(self.train, r"schedule:\n    - cron: '[^']+'")

    def test_it_holds_the_permissions_its_steps_use(self) -> None:
        for permission in ("actions: write", "contents: write", "pull-requests: write", "issues: write"):
            self.assertIn(f"  {permission}\n", self.train)

    def test_the_tier_it_dispatches_can_be_dispatched(self) -> None:
        for path in mt.REQUIRED_WORKFLOWS:
            with self.subTest(path=path):
                self.assertIn("\n  workflow_dispatch:", (_WORKFLOWS.parent.parent / path).read_text())

    def test_auto_squash_merge_stands_down_once_the_train_is_live(self) -> None:
        self.assertIn("vars.MERGE_TRAIN_MODE != 'live'", _workflow("auto-squash-merge.yml"))

    def test_the_perf_job_benchmarks_a_train_against_the_master_commit_it_was_built_on(self) -> None:
        ci = _workflow("ci.yml")
        self.assertIn("python3 scripts/merge_train.py perf-base", ci)
        self.assertNotIn('git worktree add --detach "$RUNNER_TEMP/base" HEAD^1', ci)
        self.assertIn("scripts.test_merge_train", ci)


if __name__ == "__main__":
    unittest.main()
