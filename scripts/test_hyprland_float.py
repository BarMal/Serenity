import os
import stat
import subprocess
import tempfile
import unittest

SCRIPT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "bench", "hyprland-float.sh")

# A hyprctl that logs every call and answers "ok" to a keyword unless HYPR_REFUSE names the rule it must refuse.
STUB = """#!/usr/bin/env bash
echo "$*" >> "$HYPR_LOG"
if [[ "$1" == keyword && "$2" == "${HYPR_REFUSE:-none}" ]]; then echo "invalid field"; exit 0; fi
if [[ "$1" == keyword ]]; then echo ok; fi
if [[ "$1" == clients ]]; then printf 'Window 1 -> Serenity render benchmark: Java2D:\\n\\ttitle: Serenity render benchmark: Java2D\\n\\tfloating: 1\\n\\tsize: 1600,1000\\n'; fi
"""


def run(*args, refuse="none", with_stub=True):
    with tempfile.TemporaryDirectory() as tmp:
        stub = os.path.join(tmp, "hyprctl")
        log = os.path.join(tmp, "calls.log")
        if with_stub:
            with open(stub, "w", encoding="utf-8") as handle:
                handle.write(STUB)
            os.chmod(stub, os.stat(stub).st_mode | stat.S_IEXEC)
        env = dict(os.environ, PATH=f"{tmp}:{os.environ['PATH']}" if with_stub else "/usr/bin:/bin",
                   HYPR_LOG=log, HYPR_REFUSE=refuse)
        result = subprocess.run(["bash", SCRIPT, *args], capture_output=True, text=True, env=env, check=False)
        calls = []
        if os.path.exists(log):
            with open(log, encoding="utf-8") as handle:
                calls = handle.read().splitlines()
        return result, calls


class HyprlandFloatTests(unittest.TestCase):
    def test_on_floats_sizes_and_centres_the_window_by_title(self):
        result, calls = run("on", "1600", "1000")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(calls, [
            "keyword windowrulev2 float,title:^Serenity render benchmark",
            "keyword windowrulev2 size 1600 1000,title:^Serenity render benchmark",
            "keyword windowrulev2 center,title:^Serenity render benchmark",
        ])

    def test_on_defaults_to_the_benchmarks_own_size(self):
        _, calls = run("on")
        self.assertIn("keyword windowrulev2 size 1600 1000,title:^Serenity render benchmark", calls)

    def test_on_uses_the_newer_syntax_when_the_older_is_refused(self):
        result, calls = run("on", "1200", "750", refuse="windowrulev2")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("keyword windowrule size 1200 750, match:title ^Serenity render benchmark", calls)

    def test_on_fails_loudly_when_both_syntaxes_are_refused(self):
        stub_refusing_all = STUB.replace('"$2" == "${HYPR_REFUSE:-none}"', "true")
        with tempfile.TemporaryDirectory() as tmp:
            hyprctl = os.path.join(tmp, "hyprctl")
            with open(hyprctl, "w", encoding="utf-8") as handle:
                handle.write(stub_refusing_all)
            os.chmod(hyprctl, 0o755)
            env = dict(os.environ, PATH=f"{tmp}:{os.environ['PATH']}", HYPR_LOG=os.path.join(tmp, "l"))
            result = subprocess.run(["bash", SCRIPT, "on"], capture_output=True, text=True, env=env, check=False)
        self.assertEqual(result.returncode, 1)
        self.assertIn("refused", result.stderr)

    def test_off_reloads_the_config(self):
        result, calls = run("off")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(calls, ["reload"])

    def test_status_shows_the_benchmark_window(self):
        result, _ = run("status")
        self.assertIn("floating: 1", result.stdout)

    def test_without_hyprctl_it_says_so(self):
        result, _ = run("on", with_stub=False)
        self.assertEqual(result.returncode, 2)
        self.assertIn("hyprctl not found", result.stderr)


if __name__ == "__main__":
    unittest.main()
