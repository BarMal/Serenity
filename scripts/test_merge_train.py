#!/usr/bin/env python3
"""Tests for the merge train (scripts/merge_train.py, #2553 item 1):

    python3 -m unittest scripts.test_merge_train -v

The train batches ready pull requests onto one `train/*` branch, runs the full CI tier there once, and merges the batch
only if that exact tree passed, so master never advances to a combination nobody tested. A red batch has its failed jobs
re-run once (owner's flake rule), then is halved until the culprit is found and ejected.
"""

from __future__ import annotations

import dataclasses
import os
import re
import subprocess
import tempfile
import unittest
import urllib.error
from datetime import datetime, timedelta, timezone
from pathlib import Path

from scripts import merge_train as mt

_NOW = datetime(2026, 11, 3, 16, 0, tzinfo=timezone.utc)
_MOVED = datetime(2026, 11, 5, 12, 0, tzinfo=timezone.utc)  # when master last moved, in the hold tests
_HAND_MERGE = frozenset({"hand-merge-next"})
_CI = ".github/workflows/ci.yml"
_XOS = ".github/workflows/cross-os-tests.yml"


def _pr(number: int, head: str | None = None) -> mt.TrainPr:
    return mt.TrainPr(number=number, head=head or f"{number:040x}", title=f"Change {number}")


def _manifest(*numbers: int, base: str = "b" * 40, created: datetime = _NOW, bisected: bool = False) -> mt.Manifest:
    return mt.Manifest(base=base, prs=tuple(_pr(n) for n in numbers), created=created, bisected=bisected)


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

    def test_a_bisected_train_is_not_re_run_because_the_top_of_the_bisect_already_ruled_out_a_flake(self) -> None:
        runs = [_run(_CI, "failure", run_id=1), _run(_XOS, run_id=2)]
        self.assertEqual(mt.Bisect(numbers=(1, 2, 3)), self._decide(_manifest(1, 2, 3, 4, 5, bisected=True), runs))

    def test_a_lone_pull_request_of_a_bisected_train_is_ejected_on_its_first_red_run(self) -> None:
        failed = _run(_XOS, "failure", run_id=2)
        action = self._decide(_manifest(7, bisected=True), [_run(_CI, run_id=1), failed])
        self.assertEqual(mt.Eject(number=7, failed_runs=(failed,)), action)

    def test_a_bisected_train_that_passes_merges(self) -> None:
        action = self._decide(_manifest(1, 2, bisected=True), [_run(_CI, run_id=1), _run(_XOS, run_id=2)])
        self.assertEqual(mt.Merge(flaky_runs=()), action)

    def test_the_bisected_flag_survives_the_commit_message_and_older_manifests_read_as_not_bisected(self) -> None:
        marked = _manifest(1, 2, bisected=True)
        self.assertTrue(mt.Manifest.from_commit_message(marked.commit_message()).bisected)  # type: ignore[union-attr]
        plain = mt.Manifest.from_commit_message(_manifest(1, 2).commit_message())
        assert plain is not None
        self.assertFalse(plain.bisected)
        self.assertNotIn("bisected", _manifest(1, 2).commit_message())

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


def _git(cwd: Path, *args: str, env: dict[str, str] | None = None) -> str:
    result = subprocess.run(
        ["git", "-c", "user.name=t", "-c", "user.email=t@t", "-c", "init.defaultBranch=master", *args],
        cwd=cwd,
        capture_output=True,
        text=True,
        check=True,
        env=None if env is None else {**os.environ, **env},
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
        (self.work / path).parent.mkdir(parents=True, exist_ok=True)
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


    def commit_to_master(self, path: str, text: str, when: datetime | None = None) -> None:
        _git(self.work, "fetch", "origin", "master")
        _git(self.work, "checkout", "-q", "-B", "direct", "origin/master")
        (self.work / path).write_text(text)
        _git(self.work, "add", ".")
        dated = {} if when is None else {"GIT_COMMITTER_DATE": when.isoformat(), "GIT_AUTHOR_DATE": when.isoformat()}
        _git(self.work, "commit", "-m", "Direct", env=dated)
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
        self.failed_suites_of_run: dict[int, list[mt.FailedSuite]] = {}
        self.suites_read: list[tuple[int, int]] = []
        self.changed: dict[int, list[str]] = {}
        self.merge_fails_for: set[int] = set()
        self.extra_on_merge: tuple[str, str] | None = None

    def add_pr(self, number: int, path: str, text: str, **overrides: object) -> None:
        head = self.origin.open_pr(number, path, text)
        self.candidates[number] = _candidate(number, head=head, **overrides)
        self.changed[number] = [path]

    def changed_files(self, number: int) -> list[str]:
        return self.changed[number]

    def failed_suites(self, run_id: int, attempt: int) -> list[mt.FailedSuite]:
        self.suites_read.append((run_id, attempt))
        return self.failed_suites_of_run.get(run_id, [])

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
        held = mt.held_heads_in([body])
        ejected = mt.ejected_heads_in([body])
        if (held or ejected) and number in self.candidates:
            current = self.candidates[number]
            self.candidates[number] = dataclasses.replace(
                current, held_heads=current.held_heads | held, ejected_heads=current.ejected_heads | ejected
            )


class TrainRig(unittest.TestCase):
    """A train over a real git origin and a fake GitHub; the test classes below add the scenarios."""

    def setUp(self) -> None:
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name)
        self.origin = Origin(self.root)
        self.github = FakeGitHub(self.origin)
        self.clone = self.root / "runner"
        _git(self.root, "clone", "-q", str(self.origin.bare), str(self.clone))
        self.log: list[str] = []
        self.summary: list[str] = []
        self.clock = _NOW

    def _train(self, dry_run: bool = False, flake_issue: int | None = 99, batch: int = 6) -> mt.Train:
        return mt.Train(
            github=self.github,
            git=mt.Git(self.clone),
            batch=batch,
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

    def _train_numbers(self) -> list[int]:
        branch = self._live_branch()
        assert branch is not None
        manifest = mt.Manifest.from_commit_message(_git(self.origin.bare, "log", "-1", "--format=%B", branch))
        assert manifest is not None
        return [p.number for p in manifest.prs]


class TrainTests(TrainRig):
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

    def test_a_conflicting_pull_request_is_replaced_by_the_next_eligible_one(self) -> None:
        self.github.add_pr(1, "same.txt", "one\n")
        self.github.add_pr(2, "same.txt", "two\n")
        self.github.add_pr(3, "c.txt", "c\n")
        self._train(batch=2).tick()
        self.assertEqual([1, 3], self._train_numbers())
        self.assertIn("Left out (conflict, no change or moved head): #2", self.log)

    def test_a_backfilled_pull_request_is_applied_on_top_of_the_batch_built_so_far(self) -> None:
        self.github.add_pr(1, "x.txt", "one\n")
        self.github.add_pr(2, "x.txt", "two\n")
        self.github.add_pr(3, "x.txt", "three\n")
        self.github.add_pr(4, "y.txt", "y\n")
        self._train(batch=2).tick()
        self.assertEqual([1, 4], self._train_numbers())
        self.assertIn("Left out (conflict, no change or moved head): #2 #3", self.log)

    def test_backfilling_never_takes_the_train_past_the_batch_size(self) -> None:
        for n in (1, 2, 3):
            self.github.add_pr(n, f"f{n}.txt", f"{n}\n")
        self._train(batch=2).tick()
        self.assertEqual([1, 2], self._train_numbers())
        self.assertFalse(any("Left out" in line for line in self.log))

    def test_when_the_batch_cannot_be_filled_it_takes_every_pull_request_that_applies(self) -> None:
        self.github.add_pr(1, "same.txt", "one\n")
        self.github.add_pr(2, "same.txt", "two\n")
        self._train(batch=3).tick()
        self.assertEqual([1], self._train_numbers())

    def test_a_pull_request_that_changes_a_workflow_file_is_held_back_because_the_workflow_token_cannot_push_it(self) -> None:
        # GITHUB_TOKEN may not push a commit that changes .github/workflows/*; the first such train failed at its push.
        self.github.add_pr(1, ".github/workflows/ci.yml", "name: changed\n")
        self.github.add_pr(2, "a.txt", "a\n")
        self.github.add_pr(3, "b.txt", "b\n")
        self._train(batch=2).tick()
        self.assertEqual([2, 3], self._train_numbers())
        self.assertIn("Held back (touches workflows, merged by hand): #1", self.log)

    def test_a_workflow_change_is_held_back_even_when_it_is_deeper_than_the_top_level(self) -> None:
        self.github.add_pr(1, ".github/workflows/nested/other.yml", "name: changed\n")
        self.github.add_pr(2, "a.txt", "a\n")
        self._train().tick()
        self.assertEqual([2], self._train_numbers())

    def test_no_train_is_pushed_when_every_pull_request_changes_a_workflow_file(self) -> None:
        self.github.add_pr(1, ".github/workflows/ci.yml", "name: changed\n")
        self._train().tick()
        self.assertIsNone(self._live_branch())
        self.assertEqual([], self.github.dispatched)
        self.assertIn("Held back (touches workflows, merged by hand): #1", self.log)

    def test_a_held_back_pull_request_gets_one_comment_per_head_saying_it_is_merged_by_hand(self) -> None:
        self.github.add_pr(1, ".github/workflows/ci.yml", "name: changed\n")
        self._train().tick()
        self._train().tick()
        held = [body for number, body in self.github.comments if number == 1]
        self.assertEqual(1, len(held), "a second step must not comment again on the same head")
        self.assertIn(mt.held_marker(self.github.candidates[1].head), held[0])
        self.assertIn(".github/workflows", held[0])
        self.assertIn("Merge it by hand", held[0])

    def test_a_new_push_to_a_held_back_pull_request_is_commented_on_again(self) -> None:
        self.github.add_pr(1, ".github/workflows/ci.yml", "name: one\n")
        self._train().tick()
        self.github.add_pr(1, ".github/workflows/ci.yml", "name: two\n")
        self._train().tick()
        self.assertEqual(2, len([1 for number, _ in self.github.comments if number == 1]))

    def test_held_markers_are_read_back_from_comment_bodies(self) -> None:
        bodies = ["unrelated", f"text\n{mt.held_marker('a' * 40)}\nmore", mt.ejection_marker("b" * 40)]
        self.assertEqual(frozenset({"a" * 40}), mt.held_heads_in(bodies))
        self.assertEqual(frozenset({"b" * 40}), mt.ejected_heads_in(bodies))

    # -- holding the next batch for hand merges (#2618) ----------------------------------------------------------------

    def _hold_train(self, **kwargs: object) -> mt.Train:
        return mt.Train(
            github=self.github,
            git=mt.Git(self.clone),
            batch=6,
            dry_run=False,
            flake_issue=99,
            now=lambda: self.clock,
            log=self.log.append,
            summary=self.summary.append,
            **kwargs,  # type: ignore[arg-type]
        )

    def _master_moved_at(self, when: datetime) -> None:
        self.origin.commit_to_master(f"m{when:%H%M}.txt", "m\n", when=when)

    def test_the_next_train_waits_for_a_labelled_workflow_pull_request_right_after_master_moves(self) -> None:
        self._master_moved_at(_MOVED)
        self.github.add_pr(1, ".github/workflows/ci.yml", "name: changed\n", labels=_HAND_MERGE)
        self.github.add_pr(2, "a.txt", "a\n")
        self.clock = _MOVED + timedelta(minutes=5)
        self._hold_train(hold_minutes=20).tick()
        self.assertIsNone(self._live_branch())
        self.assertEqual([], self.github.dispatched)
        self.assertTrue(any("Holding the next train until 12:20" in line and "#1" in line for line in self.log), self.log)

    def test_the_held_numbers_go_in_the_step_summary_so_the_hold_can_be_seen(self) -> None:
        self._master_moved_at(_MOVED)
        self.github.add_pr(1, ".github/workflows/ci.yml", "name: changed\n", labels=_HAND_MERGE)
        self.clock = _MOVED + timedelta(minutes=5)
        self._hold_train(hold_minutes=20).tick()
        self.assertEqual(1, len(self.summary))
        self.assertIn("#1", self.summary[0])
        self.assertIn("12:20", self.summary[0])

    def test_twenty_minutes_is_the_default_hold(self) -> None:
        self._master_moved_at(_MOVED)
        self.github.add_pr(1, ".github/workflows/ci.yml", "name: changed\n", labels=_HAND_MERGE)
        self.clock = _MOVED + timedelta(minutes=19)
        self._hold_train().tick()
        self.assertIsNone(self._live_branch())

    def test_the_train_is_built_once_the_hold_has_run_out(self) -> None:
        self._master_moved_at(_MOVED)
        self.github.add_pr(1, ".github/workflows/ci.yml", "name: changed\n", labels=_HAND_MERGE)
        self.github.add_pr(2, "a.txt", "a\n")
        self.clock = _MOVED + timedelta(minutes=20)
        self._hold_train(hold_minutes=20).tick()
        self.assertEqual([2], self._train_numbers())
        self.assertIn("Held back (touches workflows, merged by hand): #1", self.log)

    def test_a_hand_merge_inside_the_hold_starts_the_window_again_from_the_new_master_commit(self) -> None:
        self._master_moved_at(_MOVED)
        self._master_moved_at(_MOVED + timedelta(minutes=15))
        self.github.add_pr(1, ".github/workflows/ci.yml", "name: changed\n", labels=_HAND_MERGE)
        self.clock = _MOVED + timedelta(minutes=30)  # 30 min after the first commit, 15 after the last
        self._hold_train(hold_minutes=20).tick()
        self.assertIsNone(self._live_branch())
        self.assertTrue(any("Holding the next train until 12:35" in line for line in self.log), self.log)

    def test_the_train_is_built_as_soon_as_the_labelled_pull_requests_are_merged(self) -> None:
        self._master_moved_at(_MOVED)
        self.github.add_pr(1, ".github/workflows/ci.yml", "name: changed\n", labels=_HAND_MERGE)
        self.github.add_pr(2, "a.txt", "a\n")
        self.clock = _MOVED + timedelta(minutes=5)
        self._hold_train(hold_minutes=20).tick()
        self.assertIsNone(self._live_branch())
        self.github.merged.append(1)
        self._master_moved_at(_MOVED + timedelta(minutes=6))
        self.clock = _MOVED + timedelta(minutes=7)
        self._hold_train(hold_minutes=20).tick()
        self.assertEqual([2], self._train_numbers())

    def test_a_workflow_pull_request_without_the_label_never_holds_the_train(self) -> None:
        self._master_moved_at(_MOVED)
        self.github.add_pr(1, ".github/workflows/ci.yml", "name: changed\n")
        self.github.add_pr(2, "a.txt", "a\n")
        self.clock = _MOVED + timedelta(minutes=5)
        self._hold_train(hold_minutes=20).tick()
        self.assertEqual([2], self._train_numbers())
        self.assertEqual([], self.summary)

    def test_the_label_on_a_pull_request_that_is_not_held_back_holds_nothing(self) -> None:
        self._master_moved_at(_MOVED)
        self.github.add_pr(1, "a.txt", "a\n", labels=_HAND_MERGE)
        self.clock = _MOVED + timedelta(minutes=5)
        self._hold_train(hold_minutes=20).tick()
        self.assertEqual([1], self._train_numbers())

    def test_a_dry_run_does_not_report_a_hold(self) -> None:
        self._master_moved_at(_MOVED)
        self.github.add_pr(1, ".github/workflows/ci.yml", "name: changed\n", labels=_HAND_MERGE)
        self.clock = _MOVED + timedelta(minutes=5)
        train = mt.Train(
            github=self.github, git=mt.Git(self.clone), batch=6, dry_run=True, flake_issue=99,
            now=lambda: self.clock, log=self.log.append, summary=self.summary.append, hold_minutes=20,
        )
        train.tick()
        self.assertFalse(any("Holding" in line for line in self.log), self.log)
        self.assertEqual([], self.summary)

    def test_files_outside_the_workflows_directory_are_not_held_back(self) -> None:
        self.github.add_pr(1, ".github/CODEOWNERS", "* @owner\n")
        self.github.add_pr(2, "scripts/x.txt", "x\n")
        self._train().tick()
        self.assertEqual([1, 2], self._train_numbers())

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

    def _bisect_once(self, count: int) -> str:
        for n in range(1, count + 1):
            self.github.add_pr(n, f"f{n}.txt", f"{n}\n")
        self._train().tick()
        top = self._live_branch()
        assert top is not None
        self._finish(top, ci="failure", attempt=2)
        self.clock = _NOW + timedelta(minutes=1)
        self._train().tick()
        half = self._live_branch()
        assert half is not None and half != top
        return half

    def test_the_train_a_bisect_builds_is_marked_as_bisected(self) -> None:
        half = self._bisect_once(4)
        manifest = mt.Manifest.from_commit_message(_git(self.origin.bare, "log", "-1", "--format=%B", half))
        assert manifest is not None
        self.assertEqual([1, 2], [p.number for p in manifest.prs])
        self.assertTrue(manifest.bisected)

    def test_a_red_half_is_bisected_again_without_a_re_run(self) -> None:
        half = self._bisect_once(4)
        reruns_before = list(self.github.reruns)
        self._finish(half, ci="failure", attempt=1)
        self.clock = _NOW + timedelta(minutes=2)
        self._train().tick()
        self.assertEqual(reruns_before, self.github.reruns)
        quarter = self._live_branch()
        assert quarter is not None and quarter != half
        manifest = mt.Manifest.from_commit_message(_git(self.origin.bare, "log", "-1", "--format=%B", quarter))
        assert manifest is not None
        self.assertEqual([1], [p.number for p in manifest.prs])

    def test_a_green_half_merges_at_once_and_the_other_half_starts_the_next_train_in_the_same_step(self) -> None:
        half = self._bisect_once(4)
        self._finish(half)
        self.clock = _NOW + timedelta(minutes=2)
        self._train().tick()
        self.assertEqual([1, 2], self.github.merged)
        self.assertEqual([3, 4], self._train_numbers())

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


_MAC_JOB = "shards-macos / Test (macos, shard 2/2)"
_LINUX_JOB = "Test"
_PICKER_SPEC = "src/test/scala/com/serenity/CommandPickerSpec.scala"
_WALKER_SPEC = "src/test/scala/com/serenity/io/ProjectFileWalkerParallelSpec.scala"
_HOST_KEY_SPEC = "src/test/scala/com/serenity/host/HostKeySpec.scala"
_GOLDEN_SPEC = "src/test/scala/com/serenity/command/CommandPaletteGoldenRankingSpec.scala"


class AutoEjectTests(TrainRig):
    """A red suite that exactly one pull request of the batch adds or changes is that pull request's to fix: it is
    ejected on the spot, with no re-run and no bisect (owner's decision, #2618)."""

    def _red_batch(self, numbers: tuple[int, ...] = (1, 2, 3), spec_of: dict[int, str] | None = None) -> str:
        for number in numbers:
            path = (spec_of or {}).get(number, f"file{number}.txt")
            self.github.add_pr(number, path, f"{number}\n")
        self._train().tick()
        branch = self._live_branch()
        assert branch is not None
        return branch

    def _fail(self, branch: str, *suites: str, job: str = _MAC_JOB, run_id: int = 11, attempt: int = 1) -> None:
        if run_id == 11:
            self._finish(branch, xos="failure", attempt=attempt)
        else:
            self._finish(branch, ci="failure", attempt=attempt)
        self.github.failed_suites_of_run[run_id] = [mt.FailedSuite(job, suite) for suite in suites]

    def _comments_on(self, number: int) -> list[str]:
        return [body for n, body in self.github.comments if n == number]

    def _assert_owner_ejected(self, owner: int, suite: str, path: str) -> None:
        branch = self._red_batch(spec_of={owner: path})
        self._fail(branch, suite)
        self.clock = _NOW + timedelta(minutes=1)
        self._train().tick()

        self.assertEqual([], self.github.reruns)
        (comment,) = self._comments_on(owner)
        self.assertIn(mt.ejection_marker(self.github.candidates[owner].head), comment)
        self.assertIn(suite, comment)
        self.assertIn(_MAC_JOB, comment)
        self.assertIn("https://x/11", comment)
        self.assertIn(path, comment)
        rebuilt = self._live_branch()
        self.assertNotEqual(branch, rebuilt)
        self.assertEqual([n for n in (1, 2, 3) if n != owner], self._train_numbers())
        self.assertIn(("ci.yml", rebuilt), self.github.dispatched)

    def test_2640_which_adds_the_command_picker_spec_is_ejected_when_that_suite_fails_on_macos(self) -> None:
        self._assert_owner_ejected(3, "com.serenity.CommandPickerSpec", _PICKER_SPEC)

    def test_2527_which_adds_the_walker_spec_is_ejected_when_that_suite_fails(self) -> None:
        self._assert_owner_ejected(2, "com.serenity.io.ProjectFileWalkerParallelSpec", _WALKER_SPEC)

    def test_2541_which_adds_the_host_key_spec_is_ejected_when_that_suite_fails(self) -> None:
        self._assert_owner_ejected(1, "com.serenity.host.HostKeySpec", _HOST_KEY_SPEC)

    def test_2611_which_breaks_a_golden_spec_it_does_not_touch_still_takes_the_re_run_path(self) -> None:
        # #2611 changed AppModeCommands and CommandRegistry, not CommandPaletteGoldenRankingSpec, so no pull request
        # of the batch owns that suite and the rule does not apply.
        branch = self._red_batch(spec_of={2: "src/main/scala/com/serenity/command/AppModeCommands.scala"})
        self._fail(branch, "com.serenity.command.CommandPaletteGoldenRankingSpec", job=_LINUX_JOB, run_id=10)
        self._train().tick()

        self.assertEqual([10], self.github.reruns)
        self.assertEqual([], self.github.comments)
        self.assertEqual(branch, self._live_branch())

    def test_two_pull_requests_touching_the_failing_spec_leave_it_to_the_re_run(self) -> None:
        branch = self._red_batch(spec_of={2: _PICKER_SPEC})
        self.github.changed[3] = [_PICKER_SPEC]
        self._fail(branch, "com.serenity.CommandPickerSpec")
        self._train().tick()

        self.assertEqual([11], self.github.reruns)
        self.assertEqual([], self.github.comments)
        self.assertEqual(branch, self._live_branch())

    def test_a_failing_suite_no_pull_request_touches_takes_the_re_run(self) -> None:
        branch = self._red_batch()
        self._fail(branch, "com.serenity.ui.terminal.SwingWindowResizeSpec", job=_LINUX_JOB, run_id=10)
        self._train().tick()

        self.assertEqual([10], self.github.reruns)
        self.assertEqual([], self.github.comments)

    def test_a_package_that_only_looks_alike_is_not_an_owner(self) -> None:
        branch = self._red_batch(spec_of={2: "src/test/scala/com/serenity/ui/CommandPickerSpec.scala"})
        self._fail(branch, "com.serenity.CommandPickerSpec")
        self._train().tick()

        self.assertEqual([11], self.github.reruns)
        self.assertEqual([], self.github.comments)

    def test_a_red_run_whose_failed_suites_cannot_be_read_takes_the_re_run(self) -> None:
        branch = self._red_batch(spec_of={2: _PICKER_SPEC})
        self._finish(branch, xos="failure")
        self._train().tick()

        self.assertEqual([11], self.github.reruns)
        self.assertEqual([], self.github.comments)

    def test_every_owner_of_a_failing_suite_is_ejected_in_the_same_step(self) -> None:
        branch = self._red_batch(spec_of={1: _HOST_KEY_SPEC, 3: _PICKER_SPEC})
        self._fail(branch, "com.serenity.host.HostKeySpec", "com.serenity.CommandPickerSpec")
        self._train().tick()

        self.assertEqual([], self.github.reruns)
        self.assertEqual(1, len(self._comments_on(1)))
        self.assertEqual(1, len(self._comments_on(3)))
        self.assertEqual([2], self._train_numbers())

    def test_one_pull_request_owning_two_failing_suites_gets_one_comment_naming_both(self) -> None:
        branch = self._red_batch(spec_of={2: _PICKER_SPEC})
        self.github.changed[2] = [_PICKER_SPEC, _HOST_KEY_SPEC]
        self._fail(branch, "com.serenity.CommandPickerSpec", "com.serenity.host.HostKeySpec")
        self._train().tick()

        (comment,) = self._comments_on(2)
        self.assertIn("com.serenity.CommandPickerSpec", comment)
        self.assertIn("com.serenity.host.HostKeySpec", comment)

    def test_an_owned_failure_beside_an_unowned_one_ejects_the_owner_and_rebuilds_without_a_re_run(self) -> None:
        branch = self._red_batch(spec_of={3: _PICKER_SPEC})
        self._fail(branch, "com.serenity.CommandPickerSpec", "com.serenity.ui.terminal.SwingWindowResizeSpec")
        self._train().tick()

        self.assertEqual([], self.github.reruns)
        self.assertEqual(1, len(self._comments_on(3)))
        self.assertEqual([1, 2], self._train_numbers())

    def test_the_failed_suites_are_read_from_the_attempt_that_failed(self) -> None:
        branch = self._red_batch(spec_of={3: _PICKER_SPEC})
        self._fail(branch, "com.serenity.CommandPickerSpec", attempt=2)
        self._train().tick()

        self.assertEqual([(11, 2)], self.github.suites_read)
        self.assertEqual(1, len(self._comments_on(3)))

    def test_a_lone_pull_request_that_owns_its_failing_suite_is_ejected_without_a_re_run(self) -> None:
        branch = self._red_batch(numbers=(3,), spec_of={3: _PICKER_SPEC})
        self._fail(branch, "com.serenity.CommandPickerSpec")
        self._train().tick()

        self.assertEqual([], self.github.reruns)
        self.assertEqual(1, len(self._comments_on(3)))

    def test_a_train_still_red_after_its_re_run_ejects_an_owner_too(self) -> None:
        for number in (1, 2, 3, 4):
            self.github.add_pr(number, _PICKER_SPEC if number == 2 else f"file{number}.txt", f"{number}\n")
        self._train().tick()
        first = self._live_branch()
        assert first is not None
        self._fail(first, "com.serenity.CommandPickerSpec")
        self.github.runs[first] = [_run(_CI, run_id=10), _run(_XOS, "failure", attempt=2, run_id=11)]
        self._train().tick()

        self.assertEqual([], self.github.reruns)
        self.assertEqual(1, len(self._comments_on(2)))
        self.assertEqual([1, 3, 4], self._train_numbers())

    def test_a_dry_run_names_the_owner_and_changes_nothing(self) -> None:
        branch = self._red_batch(spec_of={3: _PICKER_SPEC})
        self._fail(branch, "com.serenity.CommandPickerSpec")
        self._train(dry_run=True).tick()

        self.assertEqual([], self.github.comments)
        self.assertEqual([], self.github.reruns)
        self.assertEqual(branch, self._live_branch())
        self.assertTrue(any("#3" in line and "CommandPickerSpec" in line for line in self.log))

    def test_a_green_train_reads_no_logs(self) -> None:
        branch = self._red_batch()
        self._finish(branch)
        self._train().tick()

        self.assertEqual([], self.github.suites_read)
        self.assertEqual([1, 2, 3], self.github.merged)


class FailedSuiteLogTests(unittest.TestCase):
    """sbt ends a red run with `Failed tests:` and one indented suite per line; the runner's log wraps each line in a
    timestamp and colour codes."""

    @staticmethod
    def _line(text: str) -> str:
        return f"2026-11-05T14:26:31.3698371Z \x1b[0m[\x1b[0m\x1b[31merror\x1b[0m] \x1b[0m\x1b[0m{text}\x1b[0m"

    def test_the_suites_under_failed_tests_are_read_from_a_coloured_timestamped_log(self) -> None:
        log = "\n".join(
            [
                self._line("Failed tests:"),
                self._line("\tcom.serenity.ui.terminal.SwingWindowResizeSpec"),
                self._line("(Test / test) sbt.TestsFailedException: Tests unsuccessful"),
            ]
        )
        self.assertEqual(["com.serenity.ui.terminal.SwingWindowResizeSpec"], mt.failed_suites_in_log(log))

    def test_several_suites_come_back_in_log_order_without_repeats(self) -> None:
        log = "\n".join(
            [
                "[error] Failed tests:",
                "[error] \tcom.serenity.B",
                "[error] \tcom.serenity.A",
                "[error] \tcom.serenity.B",
                "[error] (Test / test) sbt.TestsFailedException: Tests unsuccessful",
            ]
        )
        self.assertEqual(["com.serenity.B", "com.serenity.A"], mt.failed_suites_in_log(log))

    def test_suites_that_aborted_are_listed_under_error_during_tests(self) -> None:
        log = "[error] Error during tests:\n[error] \tcom.serenity.Aborted\n[error] (Test / test) sbt.TestsFailedException"
        self.assertEqual(["com.serenity.Aborted"], mt.failed_suites_in_log(log))

    def test_both_blocks_are_read_when_a_run_has_failures_and_aborts(self) -> None:
        log = "\n".join(
            [
                "[error] Failed tests:",
                "[error] \tcom.serenity.Failed",
                "[error] Error during tests:",
                "[error] \tcom.serenity.Aborted",
            ]
        )
        self.assertEqual(["com.serenity.Failed", "com.serenity.Aborted"], mt.failed_suites_in_log(log))

    def test_a_log_without_either_block_names_no_suite(self) -> None:
        self.assertEqual([], mt.failed_suites_in_log("[info] Tests: succeeded 5, failed 0\n[error] something else"))

    def test_the_block_ends_at_the_first_line_that_is_not_an_indented_suite(self) -> None:
        log = "[error] Failed tests:\n[error] \tcom.serenity.A\n[error] Total time: 3 s\n[error] \tnot.a.Suite"
        self.assertEqual(["com.serenity.A"], mt.failed_suites_in_log(log))


class SpecOwnerTests(unittest.TestCase):
    _CHANGES = {
        1: ["src/main/scala/com/serenity/Foo.scala", "src/test/scala/com/serenity/CommandPickerSpec.scala"],
        2: ["src/test/scala/com/serenity/host/HostKeySpec.scala"],
        3: ["src/test/scala/com/serenity/ui/CommandPickerSpec.scala"],
    }

    def test_the_one_pull_request_whose_files_include_the_spec_owns_it(self) -> None:
        self.assertEqual(
            (2, "src/test/scala/com/serenity/host/HostKeySpec.scala"),
            mt.spec_owner("com.serenity.host.HostKeySpec", self._CHANGES),
        )

    def test_the_package_is_part_of_the_match(self) -> None:
        owner = mt.spec_owner("com.serenity.CommandPickerSpec", self._CHANGES)
        self.assertEqual((1, "src/test/scala/com/serenity/CommandPickerSpec.scala"), owner)

    def test_two_pull_requests_touching_the_spec_leave_it_unowned(self) -> None:
        changes = {**self._CHANGES, 4: ["src/test/scala/com/serenity/host/HostKeySpec.scala"]}
        self.assertIsNone(mt.spec_owner("com.serenity.host.HostKeySpec", changes))

    def test_a_spec_nobody_touches_is_unowned(self) -> None:
        self.assertIsNone(mt.spec_owner("com.serenity.OtherSpec", self._CHANGES))

    def test_a_file_that_only_ends_with_the_same_name_is_not_the_spec(self) -> None:
        self.assertIsNone(mt.spec_owner("com.serenity.PickerSpec", {1: ["src/test/scala/com/serenity/CommandPickerSpec.scala"]}))


class _StubRestJobs(mt.RestGitHub):
    """RestGitHub over canned jobs, logs and pull-request files: no network."""

    def __init__(self, jobs: list[dict], logs: dict[int, str], files: list[list[dict]] | None = None) -> None:
        super().__init__("token", "owner/repo")
        self.jobs, self.logs, self.files = jobs, logs, files or []
        self.paths: list[str] = []

    def _get(self, path: str) -> dict | list:
        self.paths.append(path)
        if "/jobs" in path:
            return {"jobs": self.jobs}
        page = int(path.rsplit("page=", 1)[1])
        return self.files[page - 1] if page <= len(self.files) else []

    def _job_log(self, job_id: int) -> str:
        if job_id not in self.logs:
            raise urllib.error.HTTPError("https://x", 410, "Gone", None, None)  # type: ignore[arg-type]
        return self.logs[job_id]


class RestFailedSuiteTests(unittest.TestCase):
    _RED = "[error] Failed tests:\n[error] \tcom.serenity.CommandPickerSpec\n"

    def _job(self, job_id: int, name: str, conclusion: str) -> dict:
        return {"id": job_id, "name": name, "conclusion": conclusion}

    def test_each_failed_job_of_the_attempt_is_read_and_named(self) -> None:
        rest = _StubRestJobs(
            [self._job(1, "Test (macos)", "failure"), self._job(2, "Test (windows)", "success")],
            {1: self._RED, 2: "[error] Failed tests:\n[error] \tcom.serenity.Wrong\n"},
        )
        self.assertEqual([mt.FailedSuite("Test (macos)", "com.serenity.CommandPickerSpec")], rest.failed_suites(7, 2))
        self.assertIn("actions/runs/7/attempts/2/jobs", rest.paths[0])

    def test_a_skipped_job_is_not_read(self) -> None:
        rest = _StubRestJobs([self._job(1, "Wake", "skipped")], {1: self._RED})
        self.assertEqual([], rest.failed_suites(7, 1))

    def test_a_log_that_cannot_be_fetched_names_no_suite_and_does_not_raise(self) -> None:
        rest = _StubRestJobs([self._job(1, "Test", "failure"), self._job(2, "Test (macos)", "failure")], {2: self._RED})
        self.assertEqual([mt.FailedSuite("Test (macos)", "com.serenity.CommandPickerSpec")], rest.failed_suites(7, 1))

    def test_a_pull_requests_files_are_read_page_by_page_until_a_short_page(self) -> None:
        first = [{"filename": f"f{i}.txt"} for i in range(100)]
        second = [{"filename": "last.txt"}]
        rest = _StubRestJobs([], {}, files=[first, second])
        names = rest.changed_files(5)
        self.assertEqual(101, len(names))
        self.assertEqual("last.txt", names[-1])
        self.assertEqual(2, len(rest.paths))


class _StubRest(mt.RestGitHub):
    """RestGitHub over a canned pulls payload: no network, CI green, no markers."""

    def __init__(self, pulls: list[dict]) -> None:
        super().__init__("token", "owner/repo")
        self.pulls = pulls

    def _get(self, path: str) -> dict | list:
        return self.pulls

    def _ci_green(self, head: str) -> bool:
        return True

    def _marked_heads(self, number: int) -> tuple[frozenset[str], frozenset[str]]:
        return frozenset(), frozenset()


class LabelTests(unittest.TestCase):
    def _pull(self, labels: list[str]) -> dict:
        return {
            "number": 7, "title": "T", "draft": False, "base": {"ref": "master"},
            "head": {"sha": "a" * 40, "ref": "b", "repo": {"full_name": "owner/repo"}},
            "labels": [{"name": name} for name in labels],
        }

    def test_a_candidate_carries_the_names_of_its_labels(self) -> None:
        candidates = _StubRest([self._pull(["hand-merge-next", "perf"])]).open_candidates()
        self.assertEqual(frozenset({"hand-merge-next", "perf"}), candidates[0].labels)

    def test_a_pull_request_with_no_labels_has_none(self) -> None:
        self.assertEqual(frozenset(), _StubRest([self._pull([])]).open_candidates()[0].labels)


class PerfBaseTests(unittest.TestCase):
    """The perf job benchmarks a commit against its base. On a train the base is the master commit the train was built
    on, named in the manifest; anywhere else it stays the first parent."""

    def test_a_train_tip_names_its_manifest_base_and_any_other_commit_its_first_parent(self) -> None:
        self.assertEqual("b" * 40, mt.perf_base(_manifest(1).commit_message()))
        self.assertEqual("HEAD^1", mt.perf_base("Fix a bug (#12)\n"))


_WORKFLOWS = Path(__file__).resolve().parent.parent / ".github" / "workflows"


def _workflow(name: str) -> str:
    return (_WORKFLOWS / name).read_text()


def _job_ids(workflow: str) -> list[str]:
    jobs = workflow.split("\njobs:\n", 1)[1]
    return [m.group(1) for m in re.finditer(r"^  ([A-Za-z0-9_-]+):\s*$", jobs, re.MULTILINE)]


def _job_text(workflow: str, job: str) -> str:
    jobs = workflow.split("\njobs:\n", 1)[1]
    found = re.search(rf"^  {re.escape(job)}:\s*$(.*?)(?=^  [A-Za-z0-9_-]+:\s*$|\Z)", jobs, re.MULTILINE | re.DOTALL)
    return found.group(1) if found else ""


def _run_block(job: str) -> str:
    """The shell text of the job's `run: |` step, dedented."""
    lines = job.splitlines()
    start = next(i for i, line in enumerate(lines) if line.strip() == "run: |")
    indent = len(lines[start]) - len(lines[start].lstrip()) + 2
    body = []
    for line in lines[start + 1 :]:
        if line.strip() and len(line) - len(line.lstrip()) < indent:
            break
        body.append(line[indent:])
    return "\n".join(body) + "\n"


class WakeRetryTests(unittest.TestCase):
    """The wake job's one command can meet a GitHub API error (HTTP 500 on train 17). It retries a few times instead of
    reddening its run, which the train would read as a failed batch (#2618)."""

    def _run(self, workflow: str, failures: int) -> tuple[int, list[str], list[str], str]:
        script = _run_block(_job_text(_workflow(workflow), "wake-train"))
        with tempfile.TemporaryDirectory() as directory:
            bin_dir = Path(directory) / "bin"
            bin_dir.mkdir()
            calls, sleeps = Path(directory) / "calls", Path(directory) / "sleeps"
            (bin_dir / "gh").write_text(
                '#!/bin/sh\necho "$@" >> "$CALLS"\n[ "$(wc -l < "$CALLS")" -gt "$FAILURES" ]\n'
            )
            (bin_dir / "sleep").write_text('#!/bin/sh\necho "$1" >> "$SLEEPS"\n')
            for tool in ("gh", "sleep"):
                (bin_dir / tool).chmod(0o755)
            done = subprocess.run(
                ["bash", "-e", "-c", script],
                capture_output=True,
                text=True,
                env={**os.environ, "PATH": f"{bin_dir}:{os.environ['PATH']}", "CALLS": str(calls), "SLEEPS": str(sleeps), "FAILURES": str(failures)},
            )
            read = lambda path: path.read_text().splitlines() if path.exists() else []
            return done.returncode, read(calls), read(sleeps), done.stdout + done.stderr

    def test_a_dispatch_that_works_first_time_runs_once_and_never_waits(self) -> None:
        for workflow in ("ci.yml", "cross-os-tests.yml"):
            with self.subTest(workflow=workflow):
                code, calls, sleeps, _ = self._run(workflow, failures=0)
                self.assertEqual(0, code)
                self.assertEqual(["workflow run merge-train.yml --ref master"], calls)
                self.assertEqual([], sleeps)

    def test_a_dispatch_that_fails_twice_is_retried_with_a_longer_wait_each_time_and_succeeds(self) -> None:
        for workflow in ("ci.yml", "cross-os-tests.yml"):
            with self.subTest(workflow=workflow):
                code, calls, sleeps, _ = self._run(workflow, failures=2)
                self.assertEqual(0, code)
                self.assertEqual(3, len(calls))
                self.assertEqual(["5", "10"], sleeps)

    def test_a_dispatch_that_never_works_gives_up_after_four_tries_and_fails_the_job(self) -> None:
        for workflow in ("ci.yml", "cross-os-tests.yml"):
            with self.subTest(workflow=workflow):
                code, calls, sleeps, output = self._run(workflow, failures=99)
                self.assertNotEqual(0, code)
                self.assertEqual(4, len(calls))
                self.assertEqual(["5", "10", "15"], sleeps)
                self.assertIn("::warning::", output)

    def test_the_longest_wait_fits_inside_the_job_timeout(self) -> None:
        for workflow in ("ci.yml", "cross-os-tests.yml"):
            with self.subTest(workflow=workflow):
                self.assertIn("    timeout-minutes: 5\n", _job_text(_workflow(workflow), "wake-train"))


class WorkflowTests(unittest.TestCase):
    """How .github/workflows/merge-train.yml drives the script, and how the existing workflows make room for it."""

    def setUp(self) -> None:
        self.train = _workflow("merge-train.yml")

    def test_steps_are_never_cancelled_so_a_merge_cannot_be_cut_off_half_way(self) -> None:
        self.assertIn("  cancel-in-progress: false\n", self.train)
        self.assertNotIn("cancel-in-progress: true", self.train)

    def test_runs_the_job_would_skip_do_not_queue_in_the_trains_concurrency_group(self) -> None:
        # GitHub keeps one pending run per group, so a skipped workflow_run event for a non-train branch would cancel
        # the pending step that has real work (27 dry-run steps were starved this way).
        not_a_train = "github.event_name == 'workflow_run' && !startsWith(github.event.workflow_run.head_branch, 'train/')"
        self.assertIn(
            f"group: ${{{{ ({not_a_train}) && format('merge-train-noop-{{0}}', github.run_id) || 'merge-train' }}}}",
            self.train,
        )
        self.assertIn("(github.event_name != 'workflow_run' || startsWith(github.event.workflow_run.head_branch, 'train/'))", self.train)

    def test_nothing_runs_unless_the_owner_set_the_mode_variable(self) -> None:
        self.assertIn("vars.MERGE_TRAIN_MODE == 'dry-run' || vars.MERGE_TRAIN_MODE == 'live'", self.train)
        self.assertIn("MERGE_TRAIN_MODE: ${{ vars.MERGE_TRAIN_MODE }}", self.train)
        self.assertIn("python3 scripts/merge_train.py tick", self.train)

    def test_a_finished_run_wakes_the_train_only_when_it_ran_on_a_train_branch(self) -> None:
        self.assertIn("workflows: [CI, Cross-OS Tests]", self.train)
        self.assertIn("startsWith(github.event.workflow_run.head_branch, 'train/')", self.train)
        self.assertRegex(self.train, r"schedule:\n    - cron: '[^']+'")

    def test_master_advancing_wakes_the_train_so_the_next_batch_starts_without_waiting_for_a_tick(self) -> None:
        # Scheduled ticks were not firing (none after 18:32Z on 2026-11-04), and nothing else wakes the train between
        # trains: a batch merging moves master, which is the moment the next one can start.
        self.assertRegex(self.train, r"\n  push:\n    branches: \[master\]\n")

    def test_a_train_branchs_runs_wake_the_train_themselves_because_workflow_token_runs_raise_no_workflow_run(self) -> None:
        # GITHUB_TOKEN-created runs do not trigger workflow_run (observed on the first live train), so the last job of
        # each workflow dispatches the next merge train step. workflow_dispatch via the token is allowed.
        for name in ("ci.yml", "cross-os-tests.yml"):
            with self.subTest(workflow=name):
                text = _workflow(name)
                job = _job_text(text, "wake-train")
                self.assertIn("    if: always() && startsWith(github.ref, 'refs/heads/train/')\n", job)
                self.assertIn("    permissions:\n      actions: write\n", job)
                self.assertIn("    timeout-minutes: 5\n", job)
                self.assertIn("GH_REPO: ${{ github.repository }}", job)
                self.assertIn("GH_TOKEN: ${{ github.token }}", job)
                self.assertIn("gh workflow run merge-train.yml --ref master", job)
                others = [j for j in _job_ids(text) if j != "wake-train"]
                self.assertEqual(f"    needs: [{', '.join(others)}]\n", next(l + "\n" for l in job.splitlines() if l.startswith("    needs:")))

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
