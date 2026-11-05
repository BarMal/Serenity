#!/usr/bin/env python3
"""The merge train (#2553 item 1): batches ready pull requests onto one `train/*` branch, runs the full CI tier on it once,
and merges the batch only when that exact tree passed.

    python3 scripts/merge_train.py tick        # one step of the state machine (the Merge Train workflow runs this)
    python3 scripts/merge_train.py perf-base   # the commit the perf job benchmarks HEAD against

Only one train is live at a time, and all of its state lives in git: the `train/<time>` branch, whose tip is an empty
commit carrying the manifest (base, pull requests and their heads). The tip being empty keeps the train's tree equal to
the tree master gets once the batch is merged, which is checked after every merge.

A step looks at the live train and either waits, re-runs its failed jobs once (the owner's flake rule), merges it,
ejects its only pull request, or replaces it with its oldest half (bisecting a red batch). The re-run is for a full
batch only: a train a bisect built is marked `bisected`, because the batch above it already failed twice, and a red half
is red at once. Halving oldest-first and then re-queueing the rest also finds a failure that only two pull requests
cause together: the first half merges, and the second is then tested on top of it.

Pull requests that change workflow files are merged by hand, and a hand merge moves master and so abandons the batch just
built. So when a pull request carrying the `hand-merge-next` label is waiting to be merged by hand, the next batch is
held back until 20 minutes (MERGE_TRAIN_HOLD_MINUTES) after master last moved: each hand merge restarts the window, and
the hold ends as soon as no labelled pull request is waiting.

A red suite that exactly one pull request of the batch adds or changes is that pull request's to fix: it is ejected at
once, with no re-run and no bisect, and the batch is rebuilt without it. The failed suites are read from the failing jobs'
logs (the `Failed tests:` block sbt prints). A suite no pull request, or more than one, touches, a failure outside the
test run, and a log that cannot be read all fall back to the re-run and the bisect.
"""

from __future__ import annotations

import argparse
import json
import math
import os
import re
import subprocess
import sys
import urllib.error
import urllib.request
from dataclasses import dataclass, field
from datetime import datetime, timedelta, timezone
from enum import Enum
from pathlib import Path
from typing import Callable, Iterable, Protocol, Sequence

REQUIRED_WORKFLOWS: tuple[str, ...] = (".github/workflows/ci.yml", ".github/workflows/cross-os-tests.yml")
MAX_TRAIN_AGE = timedelta(hours=6)
HAND_MERGE_LABEL = "hand-merge-next"
DEFAULT_HOLD_MINUTES = 20
_MANIFEST_KEY = "Merge-Train-Manifest: "
_BRANCH_PREFIX = "train/"
_BOT = ("-c", "user.name=merge-train", "-c", "user.email=41898282+github-actions[bot]@users.noreply.github.com")


class TrainError(Exception):
    """Something the train cannot resolve on its own; the workflow run fails so a person looks."""


class MergeRefused(Exception):
    pass


@dataclass(frozen=True)
class TrainPr:
    number: int
    head: str
    title: str
    head_ref: str = ""


@dataclass(frozen=True)
class Manifest:
    base: str
    prs: tuple[TrainPr, ...]
    created: datetime
    # A train built by a bisect: the full batch already failed its re-run, so a flake is ruled out and a red half is
    # red at once instead of being re-run again.
    bisected: bool = False

    def commit_message(self) -> str:
        payload: dict[str, object] = {
            "base": self.base,
            "created": self.created.isoformat(),
            "prs": [{"number": p.number, "head": p.head, "title": p.title, "head_ref": p.head_ref} for p in self.prs],
        }
        if self.bisected:
            payload["bisected"] = True
        numbers = " ".join(f"#{p.number}" for p in self.prs)
        return f"Merge train: {numbers}\n\n{_MANIFEST_KEY}{json.dumps(payload, sort_keys=True)}\n"

    @staticmethod
    def from_commit_message(message: str) -> Manifest | None:
        line = next((l for l in message.splitlines() if l.startswith(_MANIFEST_KEY)), None)
        if line is None:
            return None
        payload = json.loads(line[len(_MANIFEST_KEY) :])
        return Manifest(
            base=payload["base"],
            prs=tuple(TrainPr(p["number"], p["head"], p["title"], p.get("head_ref", "")) for p in payload["prs"]),
            created=datetime.fromisoformat(payload["created"]),
            bisected=bool(payload.get("bisected", False)),
        )


@dataclass(frozen=True)
class Candidate:
    number: int
    head: str
    title: str
    draft: bool
    base: str
    same_repo: bool
    head_ref: str
    ci_green: bool
    ejected_heads: frozenset[str] = field(default_factory=frozenset)
    held_heads: frozenset[str] = field(default_factory=frozenset)
    labels: frozenset[str] = field(default_factory=frozenset)


@dataclass(frozen=True)
class Run:
    id: int
    path: str
    status: str
    conclusion: str | None
    attempt: int
    url: str


@dataclass(frozen=True)
class FailedSuite:
    job: str
    suite: str


@dataclass(frozen=True)
class OwnedFailure:
    suite: FailedSuite
    run: Run
    spec: str


@dataclass(frozen=True)
class Wait:
    reason: str


@dataclass(frozen=True)
class Abandon:
    reason: str


@dataclass(frozen=True)
class Rerun:
    run_ids: tuple[int, ...]


@dataclass(frozen=True)
class Merge:
    flaky_runs: tuple[Run, ...]


@dataclass(frozen=True)
class Eject:
    number: int
    failed_runs: tuple[Run, ...]


@dataclass(frozen=True)
class Bisect:
    numbers: tuple[int, ...]


Action = Wait | Abandon | Rerun | Merge | Eject | Bisect


def ejection_marker(head: str) -> str:
    return f"<!-- merge-train-ejected:{head} -->"


def held_marker(head: str) -> str:
    return f"<!-- merge-train-held:{head} -->"


def _heads_marked(comment_bodies: Iterable[str], kind: str) -> frozenset[str]:
    prefix, suffix = f"<!-- merge-train-{kind}:", " -->"
    found = set()
    for body in comment_bodies:
        for line in body.splitlines():
            if line.startswith(prefix) and line.endswith(suffix):
                found.add(line[len(prefix) : -len(suffix)])
    return frozenset(found)


def ejected_heads_in(comment_bodies: Iterable[str]) -> frozenset[str]:
    return _heads_marked(comment_bodies, "ejected")


def held_heads_in(comment_bodies: Iterable[str]) -> frozenset[str]:
    return _heads_marked(comment_bodies, "held")


def eligible_candidates(candidates: Iterable[Candidate], only: Sequence[int] | None = None) -> list[Candidate]:
    eligible = [
        c
        for c in candidates
        if not c.draft
        and c.base == "master"
        and c.same_repo
        and c.ci_green
        and c.head not in c.ejected_heads
        and (only is None or c.number in only)
    ]
    return sorted(eligible, key=lambda c: c.number)


def select_batch(candidates: Iterable[Candidate], limit: int, only: Sequence[int] | None = None) -> list[Candidate]:
    return eligible_candidates(candidates, only)[:limit]


def decide(
    manifest: Manifest,
    runs: Iterable[Run],
    *,
    heads: dict[int, str | None],
    master: str,
    now: datetime,
    max_age: timedelta = MAX_TRAIN_AGE,
) -> Action:
    if master != manifest.base:
        return Abandon(f"master moved from {manifest.base[:8]} to {master[:8]}")
    changed = [p.number for p in manifest.prs if heads.get(p.number) != p.head]
    if changed:
        return Abandon("pushed to or closed since the train was built: " + " ".join(f"#{n}" for n in changed))
    if now - manifest.created > max_age:
        return Abandon(f"not finished within {max_age}")

    latest: list[Run] = []
    for path in REQUIRED_WORKFLOWS:
        of_path = [r for r in runs if r.path == path]
        if not of_path:
            return Wait(f"{path} has not started")
        latest.append(max(of_path, key=lambda r: r.id))
    running = [r.path for r in latest if r.status != "completed"]
    if running:
        return Wait("running: " + ", ".join(running))

    failed = [r for r in latest if r.conclusion != "success"]
    if not failed:
        return Merge(flaky_runs=tuple(r for r in latest if r.attempt > 1))
    first_attempts = [] if manifest.bisected else [r for r in failed if r.attempt < 2]
    if first_attempts:
        return Rerun(run_ids=tuple(r.id for r in first_attempts))
    if len(manifest.prs) == 1:
        return Eject(number=manifest.prs[0].number, failed_runs=tuple(failed))
    half = math.ceil(len(manifest.prs) / 2)
    return Bisect(numbers=tuple(p.number for p in manifest.prs[:half]))


_ANSI_COLOUR = re.compile(r"\x1b\[[0-9;]*m")
_RUNNER_TIMESTAMP = re.compile(r"^\d{4}-\d\d-\d\dT[\d:.]+Z ")
_SUITE_BLOCK_HEADERS = ("[error] Failed tests:", "[error] Error during tests:")
_SUITE_LINE = re.compile(r"^\[error\] \t(\S+)$")


def failed_suites_in_log(log: str) -> list[str]:
    """The suites sbt names under `Failed tests:` and `Error during tests:` (an aborted suite), in log order."""
    suites: list[str] = []
    in_block = False
    for raw in log.splitlines():
        line = _RUNNER_TIMESTAMP.sub("", _ANSI_COLOUR.sub("", raw)).rstrip()
        if line in _SUITE_BLOCK_HEADERS:
            in_block = True
            continue
        if not in_block:
            continue
        match = _SUITE_LINE.match(line)
        if match is None:
            in_block = False
        elif match.group(1) not in suites:
            suites.append(match.group(1))
    return suites


def spec_owner(suite: str, changes: dict[int, list[str]]) -> tuple[int, str] | None:
    """The pull request whose files include `suite`'s spec, with that file, when exactly one pull request does."""
    ending = "/" + suite.replace(".", "/") + ".scala"
    touching = [(number, path) for number, paths in changes.items() for path in paths if ("/" + path).endswith(ending)]
    return touching[0] if len({number for number, _ in touching}) == 1 else None


def perf_base(head_message: str) -> str:
    manifest = Manifest.from_commit_message(head_message)
    return "HEAD^1" if manifest is None else manifest.base


class GitHub(Protocol):
    def open_candidates(self) -> list[Candidate]: ...
    def head_of(self, number: int) -> str | None: ...
    def runs_for(self, branch: str) -> list[Run]: ...
    def dispatch(self, workflow: str, ref: str) -> None: ...
    def rerun_failed_jobs(self, run_id: int) -> None: ...
    def cancel_run(self, run_id: int) -> None: ...
    def failed_job_names(self, run_id: int, attempt: int) -> list[str]: ...
    def failed_suites(self, run_id: int, attempt: int) -> list[FailedSuite]: ...
    def changed_files(self, number: int) -> list[str]: ...
    def squash_merge(self, number: int, head: str, title: str) -> None: ...
    def delete_branch(self, name: str) -> None: ...
    def comment(self, number: int, body: str) -> None: ...


# GITHUB_TOKEN may not push a commit that changes a workflow file, so a train never carries one.
_WORKFLOWS_DIR = ".github/workflows/"


class Outcome(Enum):
    APPLIED = "applied"
    LEFT_OUT = "left out"
    HELD_BACK = "held back"


@dataclass(frozen=True)
class Built:
    tip: str
    included: list[Candidate]
    left_out: list[Candidate]
    held_back: list[Candidate]


class Git:
    def __init__(self, workdir: Path) -> None:
        self.workdir = workdir

    def _run(self, *args: str, check: bool = True) -> subprocess.CompletedProcess[str]:
        result = subprocess.run(["git", *_BOT, *args], cwd=self.workdir, capture_output=True, text=True)
        if check and result.returncode != 0:
            raise TrainError(f"git {' '.join(args)} failed: {result.stderr.strip()}")
        return result

    def _out(self, *args: str) -> str:
        return self._run(*args).stdout.strip()

    def master(self) -> str:
        self._run("fetch", "-q", "origin", "+refs/heads/master:refs/remotes/origin/master")
        return self._out("rev-parse", "refs/remotes/origin/master")

    def master_time(self) -> datetime:
        return datetime.fromtimestamp(int(self._out("show", "-s", "--format=%ct", self.master())), timezone.utc)

    def live_trains(self) -> list[str]:
        lines = self._out("ls-remote", "--heads", "origin", f"refs/heads/{_BRANCH_PREFIX}*").splitlines()
        return sorted(line.split("\trefs/heads/", 1)[1] for line in lines if "\trefs/heads/" in line)

    def fetch_branch(self, branch: str) -> str:
        self._run("fetch", "-q", "origin", f"+refs/heads/{branch}:refs/remotes/origin/{branch}")
        return self._out("rev-parse", f"refs/remotes/origin/{branch}")

    def message(self, ref: str) -> str:
        return self._out("log", "-1", "--format=%B", ref)

    def tree(self, ref: str) -> str:
        return self._out("rev-parse", f"{ref}^{{tree}}")

    def build(
        self, base: str, manifest_of: Callable[[list[Candidate]], Manifest], prs: list[Candidate], limit: int
    ) -> Built:
        """Squash-applies `prs` in order until `limit` have applied, so a pull request that conflicts, has moved or is
        held back is replaced by the next one. Each is applied on top of those already included, so a later pull
        request is checked against the batch built so far."""
        self._run("checkout", "-q", "--detach", base)
        included: list[Candidate] = []
        left_out: list[Candidate] = []
        held_back: list[Candidate] = []
        for pr in prs:
            if len(included) == limit:
                break
            outcome = self._apply(pr)
            if outcome is Outcome.APPLIED:
                self._run("commit", "-q", "-m", f"{pr.title} (#{pr.number})")
                included.append(pr)
            elif outcome is Outcome.HELD_BACK:
                held_back.append(pr)
            else:
                left_out.append(pr)
        self._run("commit", "-q", "--allow-empty", "-m", manifest_of(included).commit_message())
        return Built(self._out("rev-parse", "HEAD"), included, left_out, held_back)

    def _apply(self, pr: Candidate) -> Outcome:
        self._run("fetch", "-q", "origin", f"refs/pull/{pr.number}/head")
        if self._out("rev-parse", "FETCH_HEAD") != pr.head:
            return Outcome.LEFT_OUT
        if self._run("merge", "--squash", "FETCH_HEAD", check=False).returncode != 0:
            self._run("reset", "-q", "--hard", "HEAD")
            return Outcome.LEFT_OUT
        staged = self._out("diff", "--cached", "--name-only").splitlines()
        if not staged:
            return Outcome.LEFT_OUT
        if any(path.startswith(_WORKFLOWS_DIR) for path in staged):
            self._run("reset", "-q", "--hard", "HEAD")
            return Outcome.HELD_BACK
        return Outcome.APPLIED

    def push(self, sha: str, branch: str) -> None:
        self._run("push", "-q", "origin", f"{sha}:refs/heads/{branch}")

    def delete_remote_branch(self, branch: str) -> None:
        self._run("push", "-q", "origin", "--delete", branch)


class Train:
    def __init__(
        self,
        *,
        github: GitHub,
        git: Git,
        batch: int,
        dry_run: bool,
        flake_issue: int | None,
        now: Callable[[], datetime],
        log: Callable[[str], None],
        hold_minutes: int = DEFAULT_HOLD_MINUTES,
        summary: Callable[[str], None] = lambda _text: None,
    ) -> None:
        self.github, self.git, self.batch, self.dry_run = github, git, batch, dry_run
        self.flake_issue, self.now, self.log = flake_issue, now, log
        self.hold, self.summary = timedelta(minutes=hold_minutes), summary

    def tick(self) -> None:
        live = self.git.live_trains()
        if not live:
            self._start()
            return
        branch, *extra = live
        if extra:
            raise TrainError(f"more than one live train: {', '.join(live)}")
        tip = self.git.fetch_branch(branch)
        manifest = Manifest.from_commit_message(self.git.message(tip))
        if manifest is None:
            raise TrainError(f"{branch} has no manifest at its tip {tip[:8]}")
        runs = self.github.runs_for(branch)
        heads = {p.number: self.github.head_of(p.number) for p in manifest.prs}
        action = decide(manifest, runs, heads=heads, master=self.git.master(), now=self.now())
        self.log(f"{branch} ({_numbers(manifest.prs)}): {action}")
        owners = self._owners_of_failures(manifest, runs) if isinstance(action, (Rerun, Bisect, Eject)) else {}
        for number, hits in owners.items():
            self.log(f"#{number} owns the failing {', '.join(hit.suite.suite for hit in hits)}")
        if self.dry_run or isinstance(action, Wait):
            return
        if owners:
            self._eject_owners(branch, manifest, owners)
            return
        if isinstance(action, Rerun):
            for run_id in action.run_ids:
                self.github.rerun_failed_jobs(run_id)
            return
        if isinstance(action, Abandon):
            for run in runs:
                if run.status != "completed":
                    self.github.cancel_run(run.id)
            self.git.delete_remote_branch(branch)
            self._start()
        elif isinstance(action, Merge):
            self.git.delete_remote_branch(branch)
            self._merge(manifest, tip)
            self._record_flakes(manifest, action.flaky_runs)
            self._start()
        elif isinstance(action, Eject):
            pr = manifest.prs[0]
            self.github.comment(pr.number, _ejection_comment(pr, manifest.base, action.failed_runs))
            self.git.delete_remote_branch(branch)
            self._start()
        elif isinstance(action, Bisect):
            self.git.delete_remote_branch(branch)
            self._start(only=action.numbers)

    def _owners_of_failures(self, manifest: Manifest, runs: list[Run]) -> dict[int, list[OwnedFailure]]:
        latest = [
            max(of_path, key=lambda r: r.id)
            for path in REQUIRED_WORKFLOWS
            if (of_path := [r for r in runs if r.path == path])
        ]
        failed = [r for r in latest if r.status == "completed" and r.conclusion != "success"]
        suites = [(suite, run) for run in failed for suite in self.github.failed_suites(run.id, run.attempt)]
        if not suites:
            return {}
        changes = {pr.number: self.github.changed_files(pr.number) for pr in manifest.prs}
        owned: dict[int, list[OwnedFailure]] = {}
        for suite, run in suites:
            owner = spec_owner(suite.suite, changes)
            if owner is not None:
                owned.setdefault(owner[0], []).append(OwnedFailure(suite, run, owner[1]))
        return owned

    def _eject_owners(self, branch: str, manifest: Manifest, owners: dict[int, list[OwnedFailure]]) -> None:
        for number, hits in owners.items():
            pr = next(p for p in manifest.prs if p.number == number)
            self.github.comment(number, _owner_ejection_comment(pr, manifest, hits))
        self.git.delete_remote_branch(branch)
        self._start()

    def _start(self, only: Sequence[int] | None = None) -> None:
        eligible = eligible_candidates(self.github.open_candidates(), only)
        if not eligible:
            self.log("No pull request is ready for a train.")
            return
        base = self.git.master()
        if self.dry_run:
            self.log(f"Would build a train of {_numbers(eligible[: self.batch])} on master {base[:8]}.")
            return
        created = self.now()

        def manifest_of(included: list[Candidate]) -> Manifest:
            prs = tuple(TrainPr(c.number, c.head, c.title, c.head_ref) for c in included)
            return Manifest(base=base, prs=prs, created=created, bisected=only is not None)

        built = self.git.build(base, manifest_of, eligible, self.batch)
        if built.left_out:
            self.log(f"Left out (conflict, no change or moved head): {_numbers(built.left_out)}")
        if built.held_back:
            self.log(f"Held back (touches workflows, merged by hand): {_numbers(built.held_back)}")
            for pr in built.held_back:
                if pr.head not in pr.held_heads:
                    self.github.comment(pr.number, _held_comment(pr))
            if self._holding_for_hand_merges([pr for pr in built.held_back if HAND_MERGE_LABEL in pr.labels]):
                return
        if not built.included:
            return
        branch = f"{_BRANCH_PREFIX}{created:%Y%m%d-%H%M%S}"
        self.git.push(built.tip, branch)
        for path in REQUIRED_WORKFLOWS:
            self.github.dispatch(Path(path).name, branch)
        self.log(f"Started {branch}: {_numbers(built.included)} on master {base[:8]}.")

    def _holding_for_hand_merges(self, waiting: list[Candidate]) -> bool:
        """A hand merge moves master, which abandons a batch that was just built; so the batch waits for the pull
        requests that are about to be merged by hand. Master's last commit marks the window, so every hand merge
        restarts it without any stored state."""
        if not waiting:
            return False
        until = self.git.master_time() + self.hold
        if self.now() >= until:
            return False
        text = f"Holding the next train until {until:%H:%M}Z for hand merges: {_numbers(waiting)}."
        self.log(text)
        self.summary(text)
        return True

    def _merge(self, manifest: Manifest, tip: str) -> None:
        for pr in manifest.prs:
            try:
                self.github.squash_merge(pr.number, pr.head, f"{pr.title} (#{pr.number})")
            except MergeRefused as refused:
                raise TrainError(f"stopped merging the train at #{pr.number}: {refused}") from refused
            if pr.head_ref:
                self.github.delete_branch(pr.head_ref)
            self.log(f"Merged #{pr.number}.")
        merged = self.git.master()
        if self.git.tree(merged) != self.git.tree(tip):
            raise TrainError(f"master {merged[:8]} does not have the tree the train tested ({tip[:8]})")

    def _record_flakes(self, manifest: Manifest, flaky_runs: tuple[Run, ...]) -> None:
        if not flaky_runs:
            return
        lines = []
        for run in flaky_runs:
            names = self.github.failed_job_names(run.id, 1)
            lines.append(f"- {run.url} (attempt 1 failed, re-run passed): {', '.join(names) or 'job names unavailable'}")
        body = f"Flaky on the merge train of {_numbers(manifest.prs)}, merged after one re-run:\n\n" + "\n".join(lines)
        if self.flake_issue is None:
            self.log(body)
        else:
            self.github.comment(self.flake_issue, body)


def _numbers(prs: Iterable[TrainPr | Candidate]) -> str:
    return " ".join(f"#{p.number}" for p in prs)


def _held_comment(pr: Candidate) -> str:
    return (
        f"The merge train leaves this pull request out: it changes `{_WORKFLOWS_DIR.rstrip('/')}`, and the token the "
        "train pushes with cannot push workflow files. Merge it by hand; a new push is looked at again.\n"
        f"{held_marker(pr.head)}"
    )


def _ejection_comment(pr: TrainPr, base: str, failed_runs: tuple[Run, ...]) -> str:
    runs = "\n".join(f"- {r.url}" for r in failed_runs)
    return (
        f"{ejection_marker(pr.head)}\n"
        f"The merge train tested this pull request alone on master {base[:8]} and it failed, re-run included:\n\n"
        f"{runs}\n\n"
        f"The train skips this head ({pr.head[:8]}); pushing a fix puts the pull request back in line."
    )


def _owner_ejection_comment(pr: TrainPr, manifest: Manifest, hits: list[OwnedFailure]) -> str:
    failures = "\n".join(f"- `{h.suite.suite}` (`{h.spec}`) in {h.suite.job}: {h.run.url}" for h in hits)
    return (
        f"{ejection_marker(pr.head)}\n"
        f"The merge train ejected this pull request from the batch of {_numbers(manifest.prs)} on master "
        f"{manifest.base[:8]}. It adds or changes the spec of a suite that failed, and no other pull request in the "
        f"batch touches that spec:\n\n{failures}\n\n"
        "So there was no re-run and no bisect, and the batch was rebuilt without it. "
        f"The train skips this head ({pr.head[:8]}); pushing a fix puts the pull request back in line."
    )


class _NoRedirect(urllib.request.HTTPRedirectHandler):
    def redirect_request(self, *args: object, **kwargs: object) -> None:
        return None


class RestGitHub:
    """The GitHub REST API as the workflow's GITHUB_TOKEN sees it."""

    def __init__(self, token: str, repository: str, api: str = "https://api.github.com") -> None:
        self.token, self.repository, self.api = token, repository, api

    def _call(self, method: str, path: str, body: object | None = None) -> object:
        data = None if body is None else json.dumps(body).encode()
        request = urllib.request.Request(
            f"{self.api}/repos/{self.repository}/{path}",
            data=data,
            method=method,
            headers={
                "Authorization": f"Bearer {self.token}",
                "Accept": "application/vnd.github+json",
                "X-GitHub-Api-Version": "2022-11-28",
            },
        )
        with urllib.request.urlopen(request, timeout=60) as response:
            text = response.read().decode()
        return json.loads(text) if text else None

    def _get(self, path: str) -> dict | list:
        return self._call("GET", path)  # type: ignore[return-value]

    def open_candidates(self) -> list[Candidate]:
        pulls = self._get("pulls?state=open&base=master&per_page=100&sort=created&direction=asc")
        candidates = []
        for p in pulls:  # type: ignore[union-attr]
            same_repo = (p["head"]["repo"] or {}).get("full_name") == self.repository
            plausible = same_repo and not p["draft"]
            head = p["head"]["sha"]
            ejected, held = self._marked_heads(p["number"]) if plausible else (frozenset(), frozenset())
            candidates.append(
                Candidate(
                    number=p["number"],
                    head=head,
                    title=p["title"],
                    draft=p["draft"],
                    base=p["base"]["ref"],
                    same_repo=same_repo,
                    head_ref=p["head"]["ref"],
                    ci_green=plausible and self._ci_green(head),
                    ejected_heads=ejected,
                    held_heads=held,
                    labels=frozenset(label["name"] for label in p.get("labels", [])),
                )
            )
        return candidates

    def _ci_green(self, head: str) -> bool:
        runs = self._get(f"actions/workflows/ci.yml/runs?head_sha={head}&event=pull_request&per_page=10")["workflow_runs"]  # type: ignore[index]
        latest = max(runs, key=lambda r: r["id"], default=None)
        return latest is not None and latest["conclusion"] == "success"

    def _marked_heads(self, number: int) -> tuple[frozenset[str], frozenset[str]]:
        comments = self._get(f"issues/{number}/comments?per_page=100")
        bodies = [c["body"] or "" for c in comments]  # type: ignore[union-attr]
        return ejected_heads_in(bodies), held_heads_in(bodies)

    def head_of(self, number: int) -> str | None:
        pull = self._get(f"pulls/{number}")
        return pull["head"]["sha"] if pull["state"] == "open" else None  # type: ignore[index]

    def runs_for(self, branch: str) -> list[Run]:
        runs = self._get(f"actions/runs?branch={branch}&event=workflow_dispatch&per_page=50")["workflow_runs"]  # type: ignore[index]
        return [Run(r["id"], r["path"], r["status"], r["conclusion"], r["run_attempt"], r["html_url"]) for r in runs]

    def dispatch(self, workflow: str, ref: str) -> None:
        self._call("POST", f"actions/workflows/{workflow}/dispatches", {"ref": ref})

    def rerun_failed_jobs(self, run_id: int) -> None:
        self._call("POST", f"actions/runs/{run_id}/rerun-failed-jobs")

    def cancel_run(self, run_id: int) -> None:
        try:
            self._call("POST", f"actions/runs/{run_id}/cancel")
        except urllib.error.HTTPError as error:
            if error.code != 409:  # 409: it finished meanwhile
                raise

    def _failed_jobs(self, run_id: int, attempt: int) -> list[dict]:
        jobs = self._get(f"actions/runs/{run_id}/attempts/{attempt}/jobs?per_page=100")["jobs"]  # type: ignore[index]
        return [j for j in jobs if j["conclusion"] not in ("success", "skipped", "neutral", None)]

    def failed_job_names(self, run_id: int, attempt: int) -> list[str]:
        return [j["name"] for j in self._failed_jobs(run_id, attempt)]

    def failed_suites(self, run_id: int, attempt: int) -> list[FailedSuite]:
        found: list[FailedSuite] = []
        for job in self._failed_jobs(run_id, attempt):
            try:
                log = self._job_log(job["id"])
            except urllib.error.URLError:
                continue  # an expired or missing log means the old path, not a failed step
            found.extend(FailedSuite(job["name"], suite) for suite in failed_suites_in_log(log))
        return found

    def _job_log(self, job_id: int) -> str:
        """The log lives behind a redirect to blob storage, which must not be sent the API token."""
        request = urllib.request.Request(
            f"{self.api}/repos/{self.repository}/actions/jobs/{job_id}/logs",
            headers={"Authorization": f"Bearer {self.token}", "X-GitHub-Api-Version": "2022-11-28"},
        )
        try:
            urllib.request.build_opener(_NoRedirect).open(request, timeout=60)
        except urllib.error.HTTPError as redirect:
            location = redirect.headers.get("Location") if redirect.code in (301, 302, 303, 307, 308) else None
            if location is None:
                raise
            with urllib.request.urlopen(location, timeout=120) as response:
                return response.read().decode("utf-8", errors="replace")
        raise urllib.error.URLError(f"job {job_id}: no redirect to the log")

    def changed_files(self, number: int) -> list[str]:
        names: list[str] = []
        page = 1
        while True:
            files = self._get(f"pulls/{number}/files?per_page=100&page={page}")
            names.extend(f["filename"] for f in files)  # type: ignore[union-attr]
            if len(files) < 100:
                return names
            page += 1

    def squash_merge(self, number: int, head: str, title: str) -> None:
        try:
            self._call("PUT", f"pulls/{number}/merge", {"merge_method": "squash", "sha": head, "commit_title": title})
        except urllib.error.HTTPError as error:
            raise MergeRefused(f"HTTP {error.code}: {error.read().decode(errors='replace')}") from error

    def delete_branch(self, name: str) -> None:
        try:
            self._call("DELETE", f"git/refs/heads/{name}")
        except urllib.error.HTTPError as error:
            if error.code not in (404, 422):  # already gone
                raise

    def comment(self, number: int, body: str) -> None:
        self._call("POST", f"issues/{number}/comments", {"body": body})


def _step_summary(text: str) -> None:
    path = os.environ.get("GITHUB_STEP_SUMMARY")
    if path:
        with open(path, "a", encoding="utf-8") as summary:
            summary.write(text + "\n")


def main(argv: Sequence[str]) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("command", choices=("tick", "perf-base"))
    args = parser.parse_args(argv)
    if args.command == "perf-base":
        message = subprocess.run(["git", "log", "-1", "--format=%B", "HEAD"], capture_output=True, text=True, check=True)
        print(perf_base(message.stdout))
        return 0

    mode = os.environ.get("MERGE_TRAIN_MODE", "off")
    if mode not in ("dry-run", "live"):
        print(f"MERGE_TRAIN_MODE is {mode!r}; nothing to do.")
        return 0
    flake_issue = os.environ.get("MERGE_TRAIN_FLAKE_ISSUE", "")
    train = Train(
        github=RestGitHub(os.environ["GITHUB_TOKEN"], os.environ["GITHUB_REPOSITORY"]),
        git=Git(Path.cwd()),
        batch=int(os.environ.get("MERGE_TRAIN_BATCH") or 6),
        dry_run=mode == "dry-run",
        flake_issue=int(flake_issue) if flake_issue else None,
        now=lambda: datetime.now(timezone.utc),
        log=print,
        hold_minutes=int(os.environ.get("MERGE_TRAIN_HOLD_MINUTES") or DEFAULT_HOLD_MINUTES),
        summary=_step_summary,
    )
    try:
        train.tick()
    except TrainError as error:
        print(f"::error::{error}")
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
