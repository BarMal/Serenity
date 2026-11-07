#!/usr/bin/env python3
"""Pulls master's newest nightly bench artifact, runs this machine's benchmarks on it and compares with the last night
this machine benchmarked (#2575, P4). Pull model: nothing here accepts inbound connections, and no GitHub workflow runs
on the machine. Setup guide: docs/nightly-bench-pull.md.

    pull_nightly.py --config ~/.config/serenity-bench/laptop.json [--dry-run] [--force] [--fail-on-regression]

Each run, in order:

1. asks GitHub for the newest successful *scheduled* `bench-artifacts.yml` run on the configured branch, with the token
   in the environment variable the config names (`token_env`; a read-only, Actions: read token);
2. stops if that commit is already in `<results_dir>/state.json` (unless --force);
3. downloads the run's `bench-<sha7>` artifact into a fresh directory under `<results_dir>/bundles` and extracts it,
   refusing any member that would land outside that directory;
4. runs the config's commands with `<results_dir>/runs/<sha7>` as the working directory (never the download), copies
   the files each command collects there and reduces them to one p50 per metric (`metrics.json`);
5. compares with the previous benchmarked commit's metrics, prints a short summary, and, only when the config names an
   issue and the second token variable is set, posts it there as a comment.

Exit codes: 0 done or nothing new, 1 a regression with --fail-on-regression, 2 a failure (nothing is recorded, so the
next run retries the same commit).

Config (JSON): `machine`, `results_dir`, `commands` are required. A command is `name`, `argv` and optionally `cwd`
(`out`, the default, or `bundle`), `stdout_to`, `timeout_s`, `allow_failure` and `collect`: a list of `{kind, path}`
where kind is `perf_csv` (PerformanceBenchmarks output), `render_json` (`--render-benchmark`'s JSON) or `laptop_summary`
(`bench-laptop.sh`'s `<label>-summary.txt`) and path may be a glob. In argv, `cwd`, `stdout_to` and `path`,
`{jar}`, `{perf_jar}`, `{bundle}`, `{out}`, `{sha}` and `{label}` are replaced. Optional: `repo`, `workflow`, `branch`,
`api_url`, `token_env`, `regress_ratio` (1.20), `min_delta_ms` (0.05), `keep_bundle` (false) and
`post: {issue, token_env}`. See bench/nightly-pull.*.example.json.

Stdlib only; runs on Linux and Windows.
"""

from __future__ import annotations

import argparse
import csv
import datetime as dt
import glob
import io
import json
import os
import re
import shutil
import stat
import subprocess
import sys
import urllib.error
import urllib.parse
import urllib.request
import zipfile
from dataclasses import dataclass, field
from pathlib import Path
from typing import Callable, Mapping, Optional, Protocol

DEFAULT_REPO = "BarMal/Serenity"
DEFAULT_WORKFLOW = "bench-artifacts.yml"
DEFAULT_API = "https://api.github.com"
DEFAULT_TOKEN_ENV = "SERENITY_BENCH_TOKEN"
MAX_UNPACKED_BYTES = 2 * 1024**3
COLLECT_KINDS = ("perf_csv", "render_json", "laptop_summary")
PLACEHOLDER = re.compile(r"\{(\w+)\}")


class PullError(Exception):
    """A failure the run reports and exits 2 on."""


# ---------------------------------------------------------------- config (pure)


@dataclass(frozen=True)
class Collect:
    kind: str
    path: str


@dataclass(frozen=True)
class Command:
    name: str
    argv: tuple[str, ...]
    cwd: str = "out"
    stdout_to: str | None = None
    timeout_s: int = 3600
    allow_failure: bool = False
    collect: tuple[Collect, ...] = ()


@dataclass(frozen=True)
class Config:
    machine: str
    results_dir: Path
    commands: tuple[Command, ...]
    repo: str = DEFAULT_REPO
    workflow: str = DEFAULT_WORKFLOW
    branch: str = "master"
    api_url: str = DEFAULT_API
    token_env: str = DEFAULT_TOKEN_ENV
    regress_ratio: float = 1.20
    min_delta_ms: float = 0.05
    keep_bundle: bool = False
    post_issue: int | None = None
    post_token_env: str | None = None


def _command_from(raw: object, index: int) -> Command:
    where = f"commands[{index}]"
    if not isinstance(raw, dict):
        raise PullError(f"{where} must be an object")
    name, argv = raw.get("name"), raw.get("argv")
    if not isinstance(name, str) or not name:
        raise PullError(f"{where}.name must be a non-empty string")
    if not isinstance(argv, list) or not argv or not all(isinstance(a, str) for a in argv):
        raise PullError(f"{where}.argv must be a non-empty list of strings")
    cwd = raw.get("cwd", "out")
    if cwd not in ("out", "bundle"):
        raise PullError(f"{where}.cwd must be 'out' or 'bundle'")
    collect = []
    for item in raw.get("collect", []):
        if not isinstance(item, dict) or item.get("kind") not in COLLECT_KINDS or not isinstance(item.get("path"), str):
            raise PullError(f"{where}.collect entries need kind in {COLLECT_KINDS} and a path")
        collect.append(Collect(item["kind"], item["path"]))
    stdout_to = raw.get("stdout_to")
    if stdout_to is not None and not isinstance(stdout_to, str):
        raise PullError(f"{where}.stdout_to must be a string")
    timeout = raw.get("timeout_s", 3600)
    if not isinstance(timeout, int) or timeout <= 0:
        raise PullError(f"{where}.timeout_s must be a positive integer")
    return Command(name, tuple(argv), cwd, stdout_to, timeout, bool(raw.get("allow_failure", False)), tuple(collect))


def parse_config(raw: object) -> Config:
    if not isinstance(raw, dict):
        raise PullError("the config must be a JSON object")
    machine = raw.get("machine")
    if not isinstance(machine, str) or not re.fullmatch(r"[A-Za-z0-9_-]+", machine):
        raise PullError("machine must be a short name of letters, digits, '-' or '_'")
    results_dir = raw.get("results_dir")
    if not isinstance(results_dir, str) or not results_dir:
        raise PullError("results_dir is required")
    commands = raw.get("commands")
    if not isinstance(commands, list) or not commands:
        raise PullError("commands must be a non-empty list")
    post = raw.get("post") or {}
    issue = post.get("issue")
    if issue is not None and not isinstance(issue, int):
        raise PullError("post.issue must be an issue number")
    return Config(
        machine=machine,
        results_dir=Path(os.path.expanduser(results_dir)),
        commands=tuple(_command_from(c, i) for i, c in enumerate(commands)),
        repo=raw.get("repo", DEFAULT_REPO),
        workflow=raw.get("workflow", DEFAULT_WORKFLOW),
        branch=raw.get("branch", "master"),
        api_url=raw.get("api_url", DEFAULT_API).rstrip("/"),
        token_env=raw.get("token_env", DEFAULT_TOKEN_ENV),
        regress_ratio=float(raw.get("regress_ratio", 1.20)),
        min_delta_ms=float(raw.get("min_delta_ms", 0.05)),
        keep_bundle=bool(raw.get("keep_bundle", False)),
        post_issue=issue,
        post_token_env=post.get("token_env", "SERENITY_BENCH_ISSUES_TOKEN") if issue is not None else None,
    )


def expand(text: str, values: Mapping[str, str]) -> str:
    def replace(match: re.Match[str]) -> str:
        key = match.group(1)
        if key not in values:
            raise PullError(f"unknown placeholder {{{key}}} in '{text}'")
        return values[key]

    return PLACEHOLDER.sub(replace, text)


def child_env(env: Mapping[str, str], secret_names: list[str]) -> dict[str, str]:
    """The environment benchmark processes get: the caller's, minus the token variables."""
    return {k: v for k, v in env.items() if k not in secret_names}


# ---------------------------------------------------------------- GitHub payloads (pure)


@dataclass(frozen=True)
class ScheduledRun:
    run_id: int
    sha: str


@dataclass(frozen=True)
class ArtifactRef:
    name: str
    archive_url: str


def newest_scheduled_run(payload: Mapping, branch: str) -> ScheduledRun | None:
    """The newest successful scheduled run on `branch`; the query filters too, this keeps the rule testable."""
    runs = [
        r
        for r in payload.get("workflow_runs", [])
        if r.get("event") == "schedule" and r.get("conclusion") == "success" and r.get("head_branch") == branch
    ]
    if not runs:
        return None
    newest = max(runs, key=lambda r: r.get("created_at", ""))
    return ScheduledRun(int(newest["id"]), str(newest["head_sha"]))


def pick_artifact(payload: Mapping, sha: str) -> ArtifactRef | None:
    wanted = f"bench-{sha[:7]}"
    for artifact in payload.get("artifacts", []):
        if artifact.get("name") == wanted and not artifact.get("expired", False):
            return ArtifactRef(wanted, str(artifact["archive_download_url"]))
    return None


def redirect_allowed(location: str) -> bool:
    parsed = urllib.parse.urlparse(location)
    return parsed.scheme == "https" or (parsed.scheme == "http" and parsed.hostname in ("127.0.0.1", "localhost"))


# ---------------------------------------------------------------- zip safety (pure + thin extraction)


def unsafe_member(name: str) -> str | None:
    """Why a zip member name may not be extracted, or None."""
    if not name or "\x00" in name:
        return "empty or NUL name"
    if "\\" in name:
        return "backslash in name"
    if name.startswith("/") or re.match(r"^[A-Za-z]:", name):
        return "absolute path"
    if ".." in name.split("/"):
        return "parent-directory component"
    return None


def extract_zip(zip_path: Path, dest: Path) -> list[str]:
    """Extracts into the empty directory `dest`, checking every member before writing any."""
    if dest.exists() and any(dest.iterdir()):
        raise PullError(f"{dest} is not empty")
    dest.mkdir(parents=True, exist_ok=True)
    root = dest.resolve()
    with zipfile.ZipFile(zip_path) as archive:
        infos = archive.infolist()
        total = sum(i.file_size for i in infos)
        if total > MAX_UNPACKED_BYTES:
            raise PullError(f"archive unpacks to {total} bytes, over the {MAX_UNPACKED_BYTES} limit")
        for info in infos:
            problem = unsafe_member(info.filename)
            if problem is None and stat.S_ISLNK(info.external_attr >> 16):
                problem = "symbolic link"
            if problem is None and root not in (root / info.filename).resolve().parents and not info.is_dir():
                problem = "resolves outside the destination"
            if problem is not None:
                raise PullError(f"refusing zip member '{info.filename}': {problem}")
        for info in infos:
            target = root / info.filename
            if info.is_dir():
                target.mkdir(parents=True, exist_ok=True)
                continue
            target.parent.mkdir(parents=True, exist_ok=True)
            with archive.open(info) as source, open(target, "wb") as sink:
                shutil.copyfileobj(source, sink)
    return [i.filename for i in infos]


# ---------------------------------------------------------------- state (pure)


@dataclass(frozen=True)
class State:
    benchmarked: tuple[str, ...] = ()  # oldest first

    def has(self, sha: str) -> bool:
        return sha in self.benchmarked

    def previous(self) -> str | None:
        return self.benchmarked[-1] if self.benchmarked else None

    def with_sha(self, sha: str) -> State:
        return State(tuple(s for s in self.benchmarked if s != sha) + (sha,))

    def to_json(self) -> str:
        return json.dumps({"benchmarked": list(self.benchmarked)}, indent=2) + "\n"

    @staticmethod
    def parse(text: str) -> State:
        if not text.strip():
            return State()
        try:
            shas = json.loads(text)["benchmarked"]
        except (ValueError, KeyError, TypeError) as error:
            raise PullError(f"state file is not readable: {error}") from error
        if not isinstance(shas, list) or not all(isinstance(s, str) for s in shas):
            raise PullError("state file: benchmarked must be a list of commit ids")
        return State(tuple(shas))


# ---------------------------------------------------------------- result parsing (pure)


def parse_perf_csv(text: str) -> dict[str, float]:
    """`PerformanceBenchmarks`' CSV: name in column 0, p50_ms in column 5. Title, context and header lines are skipped
    because their column 5 is not a number (the same rule as scripts/check_perf_regression.py)."""
    metrics: dict[str, float] = {}
    for fields in csv.reader(io.StringIO(text)):
        if len(fields) < 7 or fields[0] in ("context", "name"):
            continue
        try:
            metrics[fields[0]] = float(fields[5])
        except ValueError:
            continue
    return metrics


def parse_render_json(text: str) -> dict[str, float]:
    """`--render-benchmark`'s JSON: scenarios[].{scenario, java2d|skia|raster: {p50Ms}} as `render.<scenario>.<backend>`."""
    try:
        scenarios = json.loads(text)["scenarios"]
    except (ValueError, KeyError, TypeError) as error:
        raise PullError(f"render benchmark JSON is not readable: {error}") from error
    metrics: dict[str, float] = {}
    for scenario in scenarios:
        label = re.sub(r"[^A-Za-z0-9]+", "_", str(scenario.get("scenario", "?"))).strip("_").lower()
        for backend in ("java2d", "skia", "raster"):
            stats = scenario.get(backend)
            if isinstance(stats, dict) and isinstance(stats.get("p50Ms"), (int, float)):
                metrics[f"render.{label}.{backend}"] = float(stats["p50Ms"])
    return metrics


_SUMMARY_ROW = re.compile(r"^([a-z][a-z-]*)\s+(\d+\.\d+)\s+(\d+\.\d+)\s*$")


def parse_laptop_summary(text: str) -> dict[str, float]:
    """`bench-laptop.sh`'s summary table: `<phase> <p50 median> <p95 median>` rows, as `app.<phase>`."""
    metrics: dict[str, float] = {}
    for line in text.splitlines():
        match = _SUMMARY_ROW.match(line.strip())
        if match:
            metrics[f"app.{match.group(1)}"] = float(match.group(2))
    return metrics


PARSERS: dict[str, Callable[[str], dict[str, float]]] = {
    "perf_csv": parse_perf_csv,
    "render_json": parse_render_json,
    "laptop_summary": parse_laptop_summary,
}


# ---------------------------------------------------------------- comparison and summary (pure)


@dataclass(frozen=True)
class Change:
    name: str
    previous: float
    current: float

    @property
    def ratio(self) -> float:
        return self.current / self.previous


@dataclass(frozen=True)
class Comparison:
    regressions: tuple[Change, ...]
    improvements: tuple[Change, ...]
    compared: int
    only_current: int
    only_previous: int


def compare(previous: Mapping[str, float], current: Mapping[str, float], ratio: float, min_delta_ms: float) -> Comparison:
    """Self-relative p50 rule. One run per night is too few for scripts/abba_compare.py's range test, so a metric counts
    when it moved by at least `ratio` and at least `min_delta_ms`; the same machine, one night apart, is the baseline."""
    shared = sorted(set(previous) & set(current))
    regressions, improvements = [], []
    for name in shared:
        before, after = previous[name], current[name]
        if before <= 0.0:
            continue
        change = Change(name, before, after)
        if after >= before * ratio and after - before >= min_delta_ms:
            regressions.append(change)
        elif after * ratio <= before and before - after >= min_delta_ms:
            improvements.append(change)
    return Comparison(
        tuple(sorted(regressions, key=lambda c: -c.ratio)),
        tuple(sorted(improvements, key=lambda c: c.ratio)),
        len(shared),
        len(set(current) - set(previous)),
        len(set(previous) - set(current)),
    )


def format_summary(
    machine: str, sha: str, previous_sha: str | None, metric_count: int, comparison: Comparison | None, limit: int = 10
) -> str:
    lines = [f"Nightly bench on {machine}: {sha[:7]}, {metric_count} metrics."]
    if previous_sha is None or comparison is None:
        lines.append("No earlier run on this machine to compare with.")
        return "\n".join(lines)
    lines.append(
        f"Against {previous_sha[:7]}: {comparison.compared} compared, {len(comparison.regressions)} slower, "
        f"{len(comparison.improvements)} faster, {comparison.only_current} new, {comparison.only_previous} gone."
    )
    for change in comparison.regressions[:limit]:
        lines.append(f"  slower  {change.name}: {change.previous:.3f} -> {change.current:.3f} ms (x{change.ratio:.2f})")
    if len(comparison.regressions) > limit:
        lines.append(f"  ... and {len(comparison.regressions) - limit} more slower")
    for change in comparison.improvements[:limit]:
        lines.append(f"  faster  {change.name}: {change.previous:.3f} -> {change.current:.3f} ms (x{change.ratio:.2f})")
    return "\n".join(lines)


def comment_body(summary: str) -> str:
    return "```\n" + summary + "\n```\n"


# ---------------------------------------------------------------- I/O wrappers


class Api(Protocol):
    def scheduled_runs(self) -> dict: ...
    def artifacts(self, run_id: int) -> dict: ...
    def download(self, archive_url: str, dest: Path) -> None: ...
    def post_comment(self, issue: int, body: str, token: str) -> None: ...


class _NoRedirect(urllib.request.HTTPRedirectHandler):
    def redirect_request(self, req, fp, code, msg, headers, newurl):  # noqa: ANN001
        return None


@dataclass
class GitHubApi:
    """Thin urllib client. The artifact zip is served from a signed storage URL behind a redirect, and urllib would send
    the token along to it, so the redirect is followed by hand without the Authorization header."""

    base_url: str
    repo: str
    workflow: str
    branch: str
    token: str
    timeout_s: int = 60
    _opener: urllib.request.OpenerDirector = field(
        default_factory=lambda: urllib.request.build_opener(_NoRedirect), repr=False
    )

    def _headers(self, token: str | None) -> dict[str, str]:
        headers = {"Accept": "application/vnd.github+json", "X-GitHub-Api-Version": "2022-11-28"}
        if token:
            headers["Authorization"] = f"Bearer {token}"
        return headers

    def _get_json(self, path: str) -> dict:
        request = urllib.request.Request(self.base_url + path, headers=self._headers(self.token))
        try:
            with self._opener.open(request, timeout=self.timeout_s) as response:
                return json.load(response)
        except urllib.error.HTTPError as error:
            raise PullError(f"GET {path}: HTTP {error.code}") from error
        except (urllib.error.URLError, OSError, ValueError) as error:
            raise PullError(f"GET {path}: {error}") from error

    def scheduled_runs(self) -> dict:
        query = urllib.parse.urlencode(
            {"event": "schedule", "status": "success", "branch": self.branch, "per_page": 10}
        )
        return self._get_json(f"/repos/{self.repo}/actions/workflows/{self.workflow}/runs?{query}")

    def artifacts(self, run_id: int) -> dict:
        return self._get_json(f"/repos/{self.repo}/actions/runs/{run_id}/artifacts")

    def download(self, archive_url: str, dest: Path) -> None:
        request = urllib.request.Request(archive_url, headers=self._headers(self.token))
        try:
            try:
                response = self._opener.open(request, timeout=self.timeout_s)
            except urllib.error.HTTPError as error:
                if error.code not in (301, 302, 303, 307, 308) or not error.headers.get("Location"):
                    raise PullError(f"artifact download: HTTP {error.code}") from error
                location = error.headers["Location"]
                if not redirect_allowed(location):
                    raise PullError("artifact download redirected somewhere that is not https")
                response = urllib.request.urlopen(  # noqa: S310 - scheme checked by redirect_allowed
                    urllib.request.Request(location), timeout=600
                )
            with response, open(dest, "wb") as sink:
                shutil.copyfileobj(response, sink)
        except (urllib.error.URLError, OSError) as error:
            raise PullError(f"artifact download: {error}") from error

    def post_comment(self, issue: int, body: str, token: str) -> None:
        request = urllib.request.Request(
            f"{self.base_url}/repos/{self.repo}/issues/{issue}/comments",
            data=json.dumps({"body": body}).encode(),
            headers={**self._headers(token), "Content-Type": "application/json"},
            method="POST",
        )
        try:
            with self._opener.open(request, timeout=self.timeout_s):
                pass
        except (urllib.error.URLError, OSError) as error:
            raise PullError(f"posting to issue {issue}: {error}") from error


Runner = Callable[[list[str], Path, Optional[Path], int, Mapping[str, str]], int]


def run_process(argv: list[str], cwd: Path, stdout_to: Path | None, timeout_s: int, env: Mapping[str, str]) -> int:
    """Runs one command; its exit status, or 124 on timeout and 127 when the program is missing."""
    sink = open(stdout_to, "wb") if stdout_to else None  # noqa: SIM115 - closed below
    try:
        completed = subprocess.run(argv, cwd=cwd, stdout=sink, env=dict(env), timeout=timeout_s, check=False)
        return completed.returncode
    except subprocess.TimeoutExpired:
        return 124
    except FileNotFoundError:
        return 127
    finally:
        if sink:
            sink.close()


# ---------------------------------------------------------------- orchestration


def fresh_dir(base: Path) -> Path:
    """`base`, or `base-2`, `base-3`... : the first path that does not exist yet, created."""
    candidate, n = base, 1
    while candidate.exists():
        n += 1
        candidate = base.with_name(f"{base.name}-{n}")
    candidate.mkdir(parents=True)
    return candidate


def collect_metrics(command: Command, values: Mapping[str, str], out: Path) -> dict[str, float]:
    metrics: dict[str, float] = {}
    for item in command.collect:
        pattern = expand(item.path, values)
        for found in sorted(glob.glob(pattern)):
            source = Path(found)
            text = source.read_text(encoding="utf-8", errors="replace")
            metrics.update(PARSERS[item.kind](text))
            if source.parent.resolve() != out.resolve():
                shutil.copyfile(source, out / source.name)
    return metrics


def run_commands(config: Config, values: Mapping[str, str], bundle: Path, out: Path, runner: Runner, env: Mapping[str, str]) -> dict[str, float]:
    metrics: dict[str, float] = {}
    for command in config.commands:
        argv = [expand(a, values) for a in command.argv]
        cwd = bundle if command.cwd == "bundle" else out
        stdout_to = Path(expand(command.stdout_to, values)) if command.stdout_to else None
        print(f"[{command.name}] {' '.join(argv)}", flush=True)
        status = runner(argv, cwd, stdout_to, command.timeout_s, env)
        if status != 0 and not command.allow_failure:
            raise PullError(f"command '{command.name}' failed with status {status}")
        if status != 0:
            print(f"[{command.name}] status {status}, allowed to fail", flush=True)
        metrics.update(collect_metrics(command, values, out))
    return metrics


def _write_text(path: Path, text: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(path.name + ".tmp")
    temporary.write_text(text, encoding="utf-8")
    os.replace(temporary, path)


def load_metrics(path: Path) -> dict[str, float] | None:
    try:
        return {k: float(v) for k, v in json.loads(path.read_text(encoding="utf-8")).items()}
    except (OSError, ValueError, AttributeError):
        return None


@dataclass(frozen=True)
class Outcome:
    code: int
    summary: str


def pull_and_run(
    config: Config, env: Mapping[str, str], api: Api, runner: Runner = run_process, force: bool = False, dry_run: bool = False
) -> Outcome:
    state_path = config.results_dir / "state.json"
    state = State.parse(state_path.read_text(encoding="utf-8")) if state_path.exists() else State()

    run = newest_scheduled_run(api.scheduled_runs(), config.branch)
    if run is None:
        return Outcome(0, f"No successful scheduled {config.workflow} run on {config.branch} yet.")
    if state.has(run.sha) and not force:
        return Outcome(0, f"{run.sha[:7]} was already benchmarked on {config.machine}; nothing to do.")
    if dry_run:
        return Outcome(0, f"Would benchmark {run.sha[:7]} (run {run.run_id}) on {config.machine}.")

    artifact = pick_artifact(api.artifacts(run.run_id), run.sha)
    if artifact is None:
        raise PullError(f"run {run.run_id} has no unexpired artifact bench-{run.sha[:7]}")

    short = run.sha[:7]
    bundle = fresh_dir(config.results_dir / "bundles" / short)
    out = config.results_dir / "runs" / short
    out.mkdir(parents=True, exist_ok=True)
    try:
        archive = out / f"{artifact.name}.zip"
        api.download(artifact.archive_url, archive)
        extract_zip(archive, bundle)
        archive.unlink()
        values = {
            "jar": str(bundle / "Serenity.jar"),
            "perf_jar": str(bundle / "serenity-perf.jar"),
            "bundle": str(bundle),
            "out": str(out),
            "sha": run.sha,
            "label": f"nightly-{short}",
        }
        secrets = [config.token_env] + ([config.post_token_env] if config.post_token_env else [])
        metrics = run_commands(config, values, bundle, out, runner, child_env(env, secrets))
    finally:
        if not config.keep_bundle:
            shutil.rmtree(bundle, ignore_errors=True)
    if not metrics:
        raise PullError("the commands produced no metrics; not recording this commit")

    previous_sha = state.previous() if state.previous() != run.sha else None
    previous = load_metrics(config.results_dir / "runs" / previous_sha[:7] / "metrics.json") if previous_sha else None
    comparison = compare(previous, metrics, config.regress_ratio, config.min_delta_ms) if previous is not None else None

    _write_text(out / "metrics.json", json.dumps(metrics, indent=2, sort_keys=True) + "\n")
    summary = format_summary(config.machine, run.sha, previous_sha if previous is not None else None, len(metrics), comparison)
    _write_text(out / "summary.txt", summary + "\n")
    _write_text(state_path, state.with_sha(run.sha).to_json())

    post_token = env.get(config.post_token_env or "")
    if config.post_issue is not None and post_token:
        api.post_comment(config.post_issue, comment_body(summary), post_token)
    code = 1 if comparison is not None and comparison.regressions else 0
    return Outcome(code, summary)


def main(argv: list[str] | None = None, env: Mapping[str, str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--config", required=True, help="Path to this machine's JSON config")
    parser.add_argument("--dry-run", action="store_true", help="Say which commit would be benchmarked and stop")
    parser.add_argument("--force", action="store_true", help="Benchmark even if this commit was done before")
    parser.add_argument("--fail-on-regression", action="store_true", help="Exit 1 when a metric got slower")
    args = parser.parse_args(argv)
    environment = os.environ if env is None else env
    try:
        config = parse_config(json.loads(Path(args.config).expanduser().read_text(encoding="utf-8")))
        token = environment.get(config.token_env)
        if not token:
            raise PullError(f"environment variable {config.token_env} is not set")
        api = GitHubApi(config.api_url, config.repo, config.workflow, config.branch, token)
        outcome = pull_and_run(config, environment, api, force=args.force, dry_run=args.dry_run)
    except (PullError, OSError, ValueError) as error:
        print(f"pull_nightly: {error}", file=sys.stderr)
        return 2
    print(outcome.summary)
    return outcome.code if args.fail_on_regression else 0


if __name__ == "__main__":
    sys.exit(main())
