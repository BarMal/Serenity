#!/usr/bin/env python3
"""Tests for bench/pull_nightly.py, the nightly bench-artifact puller (#2575, P4):

    python3 -m unittest scripts.test_pull_nightly -v
"""

from __future__ import annotations

import http.server
import io
import json
import sys
import tempfile
import threading
import unittest
import zipfile
from pathlib import Path

_ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(_ROOT / "bench"))

import pull_nightly as pn  # noqa: E402

SHA = "a1b2c3d4e5f60718293a4b5c6d7e8f9012345678"
OLD_SHA = "0000000fffffffffffffffffffffffffffffffff"

CSV = (
    "Serenity performance benchmarks\n"
    "context,java,25\ncontext,os,linux\ncontext,a,b\ncontext,c,d\n"
    "name,iterations,mean_ms,min_ms,max_ms,p50_ms,p95_ms,stddev_ms\n"
    "laptop.render.a,10,1.0,0.9,1.5,{a},1.2,0.1\n"
    "laptop.present.b,10,2.0,1.9,2.5,{b},2.2,0.1\n"
)

RENDER_JSON = json.dumps(
    {
        "environment": {},
        "scenarios": [
            {"scenario": "Typing at end", "skia": {"p50Ms": 9.5}, "raster": {"p50Ms": 4.5}},
            {"scenario": "Scroll", "skia": {"p50Ms": 5.0}},
        ],
    }
)

SUMMARY = (
    "\n=== Benchmark summary: nightly-a1b2c3d (3 runs) ===\n"
    "Phase                    p50 median   p95 median  (ms, median of runs)\n"
    "------------------------------------------------\n"
    "input-to-paint                 7.50        15.25\n"
    "render                         3.00         6.00\n"
    "\nfull-frame fps (median): 12.0\n"
)


def run_payload(*runs: dict) -> dict:
    return {"workflow_runs": list(runs)}


def run(run_id: int, sha: str, created: str, event: str = "schedule", conclusion: str = "success", branch: str = "master") -> dict:
    return {
        "id": run_id,
        "head_sha": sha,
        "created_at": created,
        "event": event,
        "conclusion": conclusion,
        "head_branch": branch,
    }


def bundle_zip(extra: dict[str, str] | None = None) -> bytes:
    buffer = io.BytesIO()
    with zipfile.ZipFile(buffer, "w") as archive:
        archive.writestr("Serenity.jar", "jar")
        archive.writestr("serenity-perf.jar", "perf")
        archive.writestr("MANIFEST.txt", "manifest")
        archive.writestr("bench/gen-lorem.py", "print('x')")
        for name, text in (extra or {}).items():
            archive.writestr(name, text)
    return buffer.getvalue()


class SelectionTests(unittest.TestCase):
    def test_takes_the_newest_successful_scheduled_run_on_the_branch(self):
        payload = run_payload(
            run(1, OLD_SHA, "2026-11-01T02:00:00Z"),
            run(2, SHA, "2026-11-03T02:00:00Z"),
            run(3, "dispatch", "2026-11-04T02:00:00Z", event="workflow_dispatch"),
            run(4, "failed", "2026-11-04T03:00:00Z", conclusion="failure"),
            run(5, "other", "2026-11-04T04:00:00Z", branch="feature"),
        )
        self.assertEqual(pn.ScheduledRun(2, SHA), pn.newest_scheduled_run(payload, "master"))

    def test_no_qualifying_run_is_none(self):
        self.assertIsNone(pn.newest_scheduled_run(run_payload(), "master"))
        self.assertIsNone(pn.newest_scheduled_run(run_payload(run(3, SHA, "x", event="push")), "master"))

    def test_picks_the_artifact_named_for_the_commit_and_skips_expired_ones(self):
        payload = {
            "artifacts": [
                {"name": "bench-a1b2c3d", "archive_download_url": "https://x/expired", "expired": True},
                {"name": "other", "archive_download_url": "https://x/other"},
                {"name": "bench-a1b2c3d", "archive_download_url": "https://x/ok", "expired": False},
            ]
        }
        self.assertEqual(pn.ArtifactRef("bench-a1b2c3d", "https://x/ok"), pn.pick_artifact(payload, SHA))
        self.assertIsNone(pn.pick_artifact({"artifacts": []}, SHA))

    def test_redirects_must_be_https_or_loopback(self):
        self.assertTrue(pn.redirect_allowed("https://blob.example/x"))
        self.assertTrue(pn.redirect_allowed("http://127.0.0.1:8080/x"))
        self.assertFalse(pn.redirect_allowed("http://evil.example/x"))
        self.assertFalse(pn.redirect_allowed("file:///etc/passwd"))


class StateTests(unittest.TestCase):
    def test_round_trips_and_remembers_the_order(self):
        state = pn.State().with_sha("a").with_sha("b").with_sha("a")
        self.assertEqual(("b", "a"), state.benchmarked)
        self.assertEqual(state, pn.State.parse(state.to_json()))
        self.assertEqual("a", state.previous())
        self.assertTrue(state.has("b"))
        self.assertFalse(pn.State().has("b"))
        self.assertIsNone(pn.State().previous())

    def test_empty_text_is_an_empty_state_and_garbage_is_an_error(self):
        self.assertEqual(pn.State(), pn.State.parse(""))
        with self.assertRaises(pn.PullError):
            pn.State.parse("{not json")
        with self.assertRaises(pn.PullError):
            pn.State.parse('{"benchmarked": [1]}')


class ZipSafetyTests(unittest.TestCase):
    def test_flags_names_that_could_leave_the_directory(self):
        for bad in ("../x", "a/../../x", "/etc/x", "C:/x", "c:x", "a\\b", ""):
            self.assertIsNotNone(pn.unsafe_member(bad), bad)
        for good in ("Serenity.jar", "bench/a.sh", "bench/", "a..b/c"):
            self.assertIsNone(pn.unsafe_member(good), good)

    def test_extracts_a_clean_bundle(self):
        with tempfile.TemporaryDirectory() as tmp:
            archive = Path(tmp) / "a.zip"
            archive.write_bytes(bundle_zip())
            dest = Path(tmp) / "out"
            pn.extract_zip(archive, dest)
            self.assertEqual("jar", (dest / "Serenity.jar").read_text())
            self.assertTrue((dest / "bench" / "gen-lorem.py").exists())

    def test_a_traversal_member_refuses_the_whole_archive_before_writing_anything(self):
        with tempfile.TemporaryDirectory() as tmp:
            archive = Path(tmp) / "a.zip"
            archive.write_bytes(bundle_zip({"../escaped.txt": "x"}))
            dest = Path(tmp) / "out"
            with self.assertRaises(pn.PullError):
                pn.extract_zip(archive, dest)
            self.assertFalse((Path(tmp) / "escaped.txt").exists())
            self.assertEqual([], list(dest.iterdir()))

    def test_a_symlink_member_is_refused(self):
        buffer = io.BytesIO()
        with zipfile.ZipFile(buffer, "w") as archive:
            link = zipfile.ZipInfo("link")
            link.external_attr = (0o120777) << 16
            archive.writestr(link, "/etc/passwd")
        with tempfile.TemporaryDirectory() as tmp:
            archive = Path(tmp) / "a.zip"
            archive.write_bytes(buffer.getvalue())
            with self.assertRaises(pn.PullError):
                pn.extract_zip(archive, Path(tmp) / "out")

    def test_a_directory_that_is_not_empty_is_refused(self):
        with tempfile.TemporaryDirectory() as tmp:
            dest = Path(tmp) / "out"
            dest.mkdir()
            (dest / "stale").write_text("x")
            archive = Path(tmp) / "a.zip"
            archive.write_bytes(bundle_zip())
            with self.assertRaises(pn.PullError):
                pn.extract_zip(archive, dest)

    def test_fresh_dir_never_reuses_a_path(self):
        with tempfile.TemporaryDirectory() as tmp:
            first = pn.fresh_dir(Path(tmp) / "b" / "abc")
            second = pn.fresh_dir(Path(tmp) / "b" / "abc")
            self.assertNotEqual(first, second)
            self.assertTrue(first.is_dir() and second.is_dir())


class ParserTests(unittest.TestCase):
    def test_perf_csv_reads_p50_per_row_and_skips_title_context_and_header(self):
        self.assertEqual(
            {"laptop.render.a": 1.5, "laptop.present.b": 2.5}, pn.parse_perf_csv(CSV.format(a="1.5", b="2.5"))
        )

    def test_render_json_names_each_backend_of_each_scenario(self):
        self.assertEqual(
            {
                "render.typing_at_end.skia": 9.5,
                "render.typing_at_end.raster": 4.5,
                "render.scroll.skia": 5.0,
            },
            pn.parse_render_json(RENDER_JSON),
        )

    def test_render_json_that_is_not_the_report_is_an_error(self):
        with self.assertRaises(pn.PullError):
            pn.parse_render_json("{}")

    def test_laptop_summary_reads_the_phase_rows_only(self):
        self.assertEqual({"app.input-to-paint": 7.5, "app.render": 3.0}, pn.parse_laptop_summary(SUMMARY))


class CompareTests(unittest.TestCase):
    def test_flags_a_slowdown_past_both_the_ratio_and_the_floor(self):
        result = pn.compare({"a": 1.0, "b": 1.0, "c": 0.01}, {"a": 1.3, "b": 1.1, "c": 0.03}, 1.2, 0.05)
        self.assertEqual(["a"], [c.name for c in result.regressions])

    def test_flags_a_speedup_symmetrically(self):
        result = pn.compare({"a": 2.0}, {"a": 1.0}, 1.2, 0.05)
        self.assertEqual(["a"], [c.name for c in result.improvements])
        self.assertEqual((), result.regressions)

    def test_counts_metrics_that_appear_or_disappear_and_ignores_zero_baselines(self):
        result = pn.compare({"a": 1.0, "z": 0.0, "gone": 1.0}, {"a": 1.0, "z": 5.0, "new": 1.0}, 1.2, 0.05)
        self.assertEqual((2, 1, 1), (result.compared, result.only_current, result.only_previous))
        self.assertEqual((), result.regressions)

    def test_summary_lists_the_worst_first_and_caps_the_list(self):
        previous = {f"m{i}": 1.0 for i in range(12)}
        current = {f"m{i}": 2.0 + i for i in range(12)}
        text = pn.format_summary("laptop", SHA, OLD_SHA, 12, pn.compare(previous, current, 1.2, 0.05))
        self.assertIn("Nightly bench on laptop: a1b2c3d, 12 metrics.", text)
        self.assertIn("Against 0000000: 12 compared, 12 slower", text)
        self.assertLess(text.index("m11"), text.index("m10"))
        self.assertIn("... and 2 more slower", text)

    def test_summary_without_a_previous_run_says_so(self):
        self.assertIn("No earlier run", pn.format_summary("laptop", SHA, None, 3, None))


class ConfigTests(unittest.TestCase):
    def test_both_shipped_examples_are_valid(self):
        for name in ("laptop", "windows"):
            path = _ROOT / "bench" / f"nightly-pull.{name}.example.json"
            config = pn.parse_config(json.loads(path.read_text()))
            for command in config.commands:
                for arg in command.argv:
                    pn.expand(arg, {"jar": "j", "perf_jar": "p", "bundle": "b", "out": "o", "sha": "s", "label": "l"})

    def test_rejects_what_would_misbehave(self):
        minimal = {"machine": "m", "results_dir": "r", "commands": [{"name": "n", "argv": ["x"]}]}
        pn.parse_config(minimal)
        for broken in (
            {**minimal, "machine": "../x"},
            {**minimal, "commands": []},
            {**minimal, "commands": [{"name": "n", "argv": []}]},
            {**minimal, "commands": [{"name": "n", "argv": ["x"], "cwd": "elsewhere"}]},
            {**minimal, "commands": [{"name": "n", "argv": ["x"], "collect": [{"kind": "nope", "path": "p"}]}]},
        ):
            with self.assertRaises(pn.PullError, msg=str(broken)):
                pn.parse_config(broken)

    def test_posting_needs_an_issue_and_defaults_its_token_variable(self):
        base = {"machine": "m", "results_dir": "r", "commands": [{"name": "n", "argv": ["x"]}]}
        self.assertIsNone(pn.parse_config(base).post_token_env)
        posting = pn.parse_config({**base, "post": {"issue": 2575}})
        self.assertEqual((2575, "SERENITY_BENCH_ISSUES_TOKEN"), (posting.post_issue, posting.post_token_env))

    def test_unknown_placeholder_is_an_error(self):
        with self.assertRaises(pn.PullError):
            pn.expand("{nope}", {"jar": "j"})

    def test_children_do_not_inherit_the_tokens(self):
        env = pn.child_env({"PATH": "/bin", "T1": "secret", "T2": "secret2"}, ["T1", "T2"])
        self.assertEqual({"PATH": "/bin"}, env)


class FakeApi:
    def __init__(self, runs: dict, artifacts: dict, archive: bytes):
        self.runs, self.artifact_payload, self.archive = runs, artifacts, archive
        self.posted: list[tuple[int, str, str]] = []

    def scheduled_runs(self) -> dict:
        return self.runs

    def artifacts(self, run_id: int) -> dict:
        return self.artifact_payload

    def download(self, archive_url: str, dest: Path) -> None:
        dest.write_bytes(self.archive)

    def post_comment(self, issue: int, body: str, token: str) -> None:
        self.posted.append((issue, body, token))


class FakeRunner:
    """Stands in for the benchmarks: writes a render JSON into the working directory and a CSV where asked."""

    def __init__(self, render_p50: float, status: int = 0):
        self.render_p50, self.status = render_p50, status
        self.calls: list[tuple[list[str], Path, dict]] = []

    def __call__(self, argv, cwd, stdout_to, timeout_s, env) -> int:
        self.calls.append((argv, cwd, dict(env)))
        if "--render-benchmark" in argv:
            report = {"scenarios": [{"scenario": "Scroll", "skia": {"p50Ms": self.render_p50}}]}
            (cwd / "render-benchmark-20261104-020000.json").write_text(json.dumps(report))
        if stdout_to is not None:
            stdout_to.write_text(CSV.format(a="1.0", b="2.0"))
        return self.status


def config_in(results: Path, **overrides) -> pn.Config:
    raw = {
        "machine": "test-machine",
        "results_dir": str(results),
        "commands": [
            {
                "name": "render",
                "argv": ["java", "-jar", "{jar}", "--render-benchmark"],
                "collect": [{"kind": "render_json", "path": "{out}/render-benchmark-*.json"}],
            },
            {
                "name": "perf",
                "argv": ["java", "-cp", "{perf_jar}", "Main", "laptop."],
                "stdout_to": "{out}/perf.csv",
                "collect": [{"kind": "perf_csv", "path": "{out}/perf.csv"}],
            },
        ],
        **overrides,
    }
    return pn.parse_config(raw)


def api_for(sha: str, run_id: int = 7, archive: bytes | None = None) -> FakeApi:
    return FakeApi(
        run_payload(run(run_id, sha, "2026-11-04T02:00:00Z")),
        {"artifacts": [{"name": f"bench-{sha[:7]}", "archive_download_url": "https://x/z", "expired": False}]},
        archive if archive is not None else bundle_zip(),
    )


class PullAndRunTests(unittest.TestCase):
    def test_first_night_runs_in_the_results_directory_and_records_the_commit(self):
        with tempfile.TemporaryDirectory() as tmp:
            results = Path(tmp) / "results"
            runner = FakeRunner(10.0)
            outcome = pn.pull_and_run(config_in(results), {}, api_for(SHA), runner)
            self.assertEqual(0, outcome.code)
            self.assertIn("No earlier run", outcome.summary)
            self.assertEqual(results / "runs" / SHA[:7], runner.calls[0][1])
            self.assertEqual("nightly-a1b2c3d", "nightly-" + SHA[:7])
            self.assertTrue(runner.calls[0][0][2].endswith("Serenity.jar"))
            metrics = json.loads((results / "runs" / SHA[:7] / "metrics.json").read_text())
            self.assertEqual(10.0, metrics["render.scroll.skia"])
            self.assertEqual(1.0, metrics["laptop.render.a"])
            self.assertTrue((results / "state.json").exists())
            self.assertEqual([], list((results / "bundles" / SHA[:7]).parent.iterdir()))

    def test_the_same_commit_is_skipped_until_forced(self):
        with tempfile.TemporaryDirectory() as tmp:
            results = Path(tmp)
            config = config_in(results)
            pn.pull_and_run(config, {}, api_for(SHA), FakeRunner(10.0))
            runner = FakeRunner(10.0)
            outcome = pn.pull_and_run(config, {}, api_for(SHA), runner)
            self.assertIn("already benchmarked", outcome.summary)
            self.assertEqual([], runner.calls)
            pn.pull_and_run(config, {}, api_for(SHA), runner, force=True)
            self.assertTrue(runner.calls)

    def test_second_night_compares_with_the_first_and_reports_the_slowdown(self):
        with tempfile.TemporaryDirectory() as tmp:
            config = config_in(Path(tmp))
            pn.pull_and_run(config, {}, api_for(OLD_SHA), FakeRunner(10.0))
            outcome = pn.pull_and_run(config, {}, api_for(SHA), FakeRunner(14.0))
            self.assertEqual(1, outcome.code)
            self.assertIn("Against 0000000", outcome.summary)
            self.assertIn("slower  render.scroll.skia: 10.000 -> 14.000", outcome.summary)

    def test_a_failing_command_records_nothing_so_the_next_run_retries(self):
        with tempfile.TemporaryDirectory() as tmp:
            results = Path(tmp)
            with self.assertRaises(pn.PullError):
                pn.pull_and_run(config_in(results), {}, api_for(SHA), FakeRunner(10.0, status=3))
            self.assertFalse((results / "state.json").exists())

    def test_an_allowed_failure_does_not_stop_the_run(self):
        with tempfile.TemporaryDirectory() as tmp:
            config = pn.parse_config(
                {
                    "machine": "m",
                    "results_dir": tmp,
                    "commands": [
                        {"name": "float", "argv": ["bash", "f"], "allow_failure": True},
                        {
                            "name": "render",
                            "argv": ["java", "--render-benchmark"],
                            "collect": [{"kind": "render_json", "path": "{out}/render-benchmark-*.json"}],
                        },
                    ],
                }
            )

            class FailsFirst(FakeRunner):
                def __call__(self, argv, cwd, stdout_to, timeout_s, env) -> int:
                    super().__call__(argv, cwd, stdout_to, timeout_s, env)
                    return 1 if argv[0] == "bash" else 0

            self.assertEqual(0, pn.pull_and_run(config, {}, api_for(SHA), FailsFirst(5.0)).code)

    def test_commands_that_produce_no_metrics_are_not_recorded(self):
        with tempfile.TemporaryDirectory() as tmp:
            config = pn.parse_config(
                {"machine": "m", "results_dir": tmp, "commands": [{"name": "noop", "argv": ["true"]}]}
            )
            with self.assertRaises(pn.PullError):
                pn.pull_and_run(config, {}, api_for(SHA), FakeRunner(1.0))
            self.assertFalse((Path(tmp) / "state.json").exists())

    def test_a_hostile_archive_stops_the_run_and_cleans_up_the_bundle(self):
        with tempfile.TemporaryDirectory() as tmp:
            api = api_for(SHA, archive=bundle_zip({"../../escaped": "x"}))
            runner = FakeRunner(1.0)
            with self.assertRaises(pn.PullError):
                pn.pull_and_run(config_in(Path(tmp) / "r"), {}, api, runner)
            self.assertEqual([], runner.calls)
            self.assertFalse((Path(tmp) / "escaped").exists())

    def test_missing_artifact_is_an_error(self):
        with tempfile.TemporaryDirectory() as tmp:
            api = api_for(SHA)
            api.artifact_payload = {"artifacts": []}
            with self.assertRaises(pn.PullError):
                pn.pull_and_run(config_in(Path(tmp)), {}, api, FakeRunner(1.0))

    def test_dry_run_downloads_and_runs_nothing(self):
        with tempfile.TemporaryDirectory() as tmp:
            runner = FakeRunner(1.0)
            outcome = pn.pull_and_run(config_in(Path(tmp)), {}, api_for(SHA), runner, dry_run=True)
            self.assertIn("Would benchmark a1b2c3d", outcome.summary)
            self.assertEqual([], runner.calls)
            self.assertFalse((Path(tmp) / "runs").exists())

    def test_tokens_never_reach_the_benchmark_processes(self):
        with tempfile.TemporaryDirectory() as tmp:
            runner = FakeRunner(1.0)
            env = {"SERENITY_BENCH_TOKEN": "read-secret", "SERENITY_BENCH_ISSUES_TOKEN": "post-secret", "HOME": "/h"}
            config = config_in(Path(tmp), post={"issue": 2575})
            pn.pull_and_run(config, env, api_for(SHA), runner)
            self.assertEqual({"HOME": "/h"}, runner.calls[0][2])

    def test_posts_only_when_an_issue_is_configured_and_its_token_is_set(self):
        with tempfile.TemporaryDirectory() as tmp:
            quiet = api_for(SHA)
            pn.pull_and_run(config_in(Path(tmp) / "a", post={"issue": 2575}), {}, quiet, FakeRunner(1.0))
            self.assertEqual([], quiet.posted)
            no_issue = api_for(SHA)
            pn.pull_and_run(
                config_in(Path(tmp) / "b"), {"SERENITY_BENCH_ISSUES_TOKEN": "t"}, no_issue, FakeRunner(1.0)
            )
            self.assertEqual([], no_issue.posted)
            loud = api_for(SHA)
            pn.pull_and_run(
                config_in(Path(tmp) / "c", post={"issue": 2575}),
                {"SERENITY_BENCH_ISSUES_TOKEN": "t"},
                loud,
                FakeRunner(1.0),
            )
            self.assertEqual(1, len(loud.posted))
            self.assertEqual((2575, "t"), (loud.posted[0][0], loud.posted[0][2]))
            self.assertIn("Nightly bench on test-machine", loud.posted[0][1])


class _Handler(http.server.BaseHTTPRequestHandler):
    seen: list[tuple[str, str | None]] = []
    zip_bytes = b""

    def log_message(self, *args) -> None:  # noqa: D401 - silence the server
        pass

    def do_GET(self) -> None:
        _Handler.seen.append((self.path, self.headers.get("Authorization")))
        if self.path.startswith("/repos/"):
            body = json.dumps({"workflow_runs": []}).encode()
            self.send_response(200)
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)
        elif self.path == "/archive":
            self.send_response(302)
            self.send_header("Location", f"http://127.0.0.1:{self.server.server_port}/blob")
            self.end_headers()
        else:
            self.send_response(200)
            self.send_header("Content-Length", str(len(_Handler.zip_bytes)))
            self.end_headers()
            self.wfile.write(_Handler.zip_bytes)


class GitHubApiTests(unittest.TestCase):
    def setUp(self):
        _Handler.seen = []
        _Handler.zip_bytes = b"PK-payload"
        self.server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), _Handler)
        threading.Thread(target=self.server.serve_forever, daemon=True).start()
        self.addCleanup(self.server.server_close)
        self.addCleanup(self.server.shutdown)
        self.api = pn.GitHubApi(
            f"http://127.0.0.1:{self.server.server_port}", "BarMal/Serenity", "bench-artifacts.yml", "master", "tok"
        )

    def test_asks_for_successful_scheduled_runs_on_the_branch_with_the_token(self):
        self.api.scheduled_runs()
        path, auth = _Handler.seen[0]
        self.assertTrue(path.startswith("/repos/BarMal/Serenity/actions/workflows/bench-artifacts.yml/runs?"))
        for part in ("event=schedule", "status=success", "branch=master"):
            self.assertIn(part, path)
        self.assertEqual("Bearer tok", auth)

    def test_follows_the_download_redirect_without_sending_the_token_on(self):
        with tempfile.TemporaryDirectory() as tmp:
            dest = Path(tmp) / "a.zip"
            self.api.download(f"http://127.0.0.1:{self.server.server_port}/archive", dest)
            self.assertEqual(b"PK-payload", dest.read_bytes())
        self.assertEqual([("/archive", "Bearer tok"), ("/blob", None)], _Handler.seen)


if __name__ == "__main__":
    unittest.main()
