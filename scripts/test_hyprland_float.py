import os
import shutil
import stat
import subprocess
import tempfile
import unittest

SCRIPT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "bench", "hyprland-float.sh")

# A hyprctl that logs every call and answers "ok" to a keyword unless HYPR_REFUSE names the rule it must refuse.
# With HYPR_PARSER=lua it behaves like Hyprland 0.55 on a Lua config: `keyword` is rejected outright and `eval` works.
# `clients` prints a 0.55-style block whose floating/size/at come well after the title; `clients -j` prints JSON.
CLIENTS_JSON = (
    '[{"address":"0x1","title":"Terminal","floating":false,"size":[10,10],"at":[0,0]},'
    '{"address":"0x2","title":"Serenity render benchmark: Java2D","floating":true,"size":[1600,1000],"at":[160,40]}]'
)
STUB = """#!/usr/bin/env bash
echo "$*" >> "$HYPR_LOG"
if [[ "$1" == keyword && "${HYPR_PARSER:-legacy}" == lua ]]; then
  echo "keyword can't work with non-legacy parsers. Use eval."; exit 0
fi
if [[ "$1" == keyword && "$2" == "${HYPR_REFUSE:-none}" ]]; then echo "invalid field"; exit 0; fi
if [[ "$1" == keyword ]]; then echo ok; fi
if [[ "$1" == eval ]]; then
  if [[ "${HYPR_PARSER:-legacy}" == lua && "$2" != *"${HYPR_EVAL_REFUSE:-@none@}"* ]]; then echo ok; else echo "eval refused"; fi
fi
if [[ "$1" == clients && "$2" == -j ]]; then printf '%s' '@CLIENTS_JSON@'; exit 0; fi
if [[ "$1" == clients ]]; then
  printf 'Window 2 -> Serenity render benchmark: Java2D:\\n\\ttitle: Serenity render benchmark: Java2D\\n'
  for field in initialClass initialTitle pid xwayland pinned fullscreen fullscreenClient overFullscreen grouped tags \\
    swallowing focusHistoryID inhibitingIdle xdgTag xdgDescription contentType stableId; do printf '\\t%s: x\\n' "$field"; done
  printf '\\tfloating: 1\\n\\tsize: 1600,1000\\n\\tat: 160,40\\n'
fi
""".replace("@CLIENTS_JSON@", CLIENTS_JSON)


def run(*args, refuse="none", with_stub=True, parser="legacy", eval_refuse="@none@", path=None):
    with tempfile.TemporaryDirectory() as tmp:
        stub = os.path.join(tmp, "hyprctl")
        log = os.path.join(tmp, "calls.log")
        if with_stub:
            with open(stub, "w", encoding="utf-8") as handle:
                handle.write(STUB)
            os.chmod(stub, os.stat(stub).st_mode | stat.S_IEXEC)
        env = dict(os.environ, PATH=path(tmp) if path else (f"{tmp}:{os.environ['PATH']}" if with_stub else "/usr/bin:/bin"),
                   HYPR_LOG=log, HYPR_REFUSE=refuse, HYPR_PARSER=parser, HYPR_EVAL_REFUSE=eval_refuse)
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

    def test_on_evaluates_a_lua_rule_when_the_config_parser_refuses_keywords(self):
        result, calls = run("on", "1200", "750", parser="lua")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("float rules added (eval)", result.stdout)
        evals = [call for call in calls if call.startswith("eval ")]
        self.assertEqual(len(evals), 1, calls)
        rule = evals[0]
        self.assertIn("hl.window_rule(", rule)
        self.assertIn('title = "^Serenity render benchmark"', rule)
        self.assertIn("float = true", rule)
        self.assertIn("center = true", rule)
        self.assertIn("1200", rule)
        self.assertIn("750", rule)

    def test_on_tries_the_second_size_form_when_the_first_is_refused(self):
        result, calls = run("on", "1200", "750", parser="lua", eval_refuse='size = "1200 750"')
        self.assertEqual(result.returncode, 0, result.stderr)
        evals = [call for call in calls if call.startswith("eval ")]
        self.assertEqual(len(evals), 2, calls)
        self.assertIn("size = {1200, 750}", evals[1])

    def test_on_fails_loudly_when_eval_is_refused_too(self):
        result, _ = run("on", parser="lua", eval_refuse="hl.window_rule")
        self.assertEqual(result.returncode, 1)
        self.assertIn("refused", result.stderr)

    def test_status_shows_the_benchmark_window(self):
        result, _ = run("status")
        self.assertIn("floating: 1", result.stdout)

    def test_status_reads_fields_that_come_far_after_the_title(self):
        result, calls = run("status")
        self.assertEqual(calls, ["clients -j"])
        self.assertEqual(result.stdout.splitlines(), [
            "title: Serenity render benchmark: Java2D", "floating: 1", "size: 1600,1000", "at: 160,40"])

    def test_status_says_so_when_there_is_no_benchmark_window(self):
        with tempfile.TemporaryDirectory() as tmp:
            stub = os.path.join(tmp, "hyprctl")
            with open(stub, "w", encoding="utf-8") as handle:
                handle.write("#!/usr/bin/env bash\\nprintf '[]'\\n")
            os.chmod(stub, 0o755)
            env = dict(os.environ, PATH=f"{tmp}:{os.environ['PATH']}")
            result = subprocess.run(["bash", SCRIPT, "status"], capture_output=True, text=True, env=env, check=False)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("no benchmark window", result.stdout)

    def test_status_without_jq_says_so(self):
        def only_bash_and_hyprctl(tmp):
            os.symlink(shutil.which("bash"), os.path.join(tmp, "bash"))
            os.symlink(shutil.which("env"), os.path.join(tmp, "env"))
            return tmp

        result, _ = run("status", path=only_bash_and_hyprctl)
        self.assertEqual(result.returncode, 2)
        self.assertIn("jq not found", result.stderr)

    def test_without_hyprctl_it_says_so(self):
        result, _ = run("on", with_stub=False)
        self.assertEqual(result.returncode, 2)
        self.assertIn("hyprctl not found", result.stderr)


if __name__ == "__main__":
    unittest.main()
