"""Tests for bench/bench-scaling.sh and what it uses (issue #2478):

    python3 -m unittest scripts.test_bench_scaling -v

The script drives the real app under xdotool, which a test cannot, so what is tested is what does not need the app: the
documents it opens, the keys it sends to put a different buffer in each pane (simulated here with the app's own
NextTab and split rules), its argument handling and that it is valid bash. Running it against the app is done on a
laptop.
"""

import os
import subprocess
import tempfile
import unittest

BENCH = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "bench")
LIB = os.path.join(BENCH, "bench-scaling-lib.sh")
SCRIPT = os.path.join(BENCH, "bench-scaling.sh")
GENERATOR = os.path.join(BENCH, "gen-scaling-docs.py")


def lib(command):
    return subprocess.run(["bash", "-c", f'source "{LIB}"; {command}'], capture_output=True, text=True, check=False)


def generate(directory, count, kind, lines=200):
    return subprocess.run(
        ["python3", GENERATOR, directory, str(count), kind, "--lines", str(lines)], capture_output=True, text=True, check=False
    )


def lines_of(path):
    with open(path, encoding="utf-8") as handle:
        return {line for line in handle.read().splitlines() if line}


class GeneratorTests(unittest.TestCase):
    def test_writes_the_asked_number_of_files_of_distinct_lines(self):
        for kind in ("prose", "code"):
            with self.subTest(kind=kind), tempfile.TemporaryDirectory() as directory:
                self.assertEqual(generate(directory, 4, kind).returncode, 0)
                files = sorted(os.listdir(directory))
                self.assertEqual(len(files), 4)
                sets = [lines_of(os.path.join(directory, name)) for name in files]
                for i, left in enumerate(sets):
                    self.assertGreater(len(left), 10)
                    for right in sets[i + 1 :]:
                        self.assertEqual(left & right, set())

    def test_prose_is_long_lines_and_code_short_ones(self):
        with tempfile.TemporaryDirectory() as prose, tempfile.TemporaryDirectory() as code:
            generate(prose, 1, "prose", 400)
            generate(code, 1, "code", 400)
            longest_code = max(len(l) for l in lines_of(os.path.join(code, os.listdir(code)[0])))
            shortest_prose = min(len(l) for l in lines_of(os.path.join(prose, os.listdir(prose)[0])))
            self.assertLess(longest_code, 120)
            self.assertGreater(shortest_prose, 300)

    def test_the_same_arguments_write_the_same_files(self):
        with tempfile.TemporaryDirectory() as first, tempfile.TemporaryDirectory() as second:
            generate(first, 2, "prose")
            generate(second, 2, "prose")
            for name in os.listdir(first):
                with open(os.path.join(first, name), encoding="utf-8") as a, open(os.path.join(second, name), encoding="utf-8") as b:
                    self.assertEqual(a.read(), b.read())

    def test_a_kind_it_does_not_know_fails(self):
        with tempfile.TemporaryDirectory() as directory:
            self.assertNotEqual(generate(directory, 1, "poetry").returncode, 0)


class SplitPlanTests(unittest.TestCase):
    """Replays the plan with the app's rules: a split opens a pane on its parent's buffer and focuses it; NextTab moves
    the focused pane to the next open buffer in order; a click on a quarter of the window focuses the pane there."""

    @staticmethod
    def replay(panes, buffers):
        plan = lib(f"scaling_split_plan {panes}").stdout.splitlines()
        # Each pane is (column, row); start with one pane showing buffer 0 filling the window.
        shown = {"left-top": 0}
        focus = "left-top"
        # The geometry of the splits the plan makes, as the app lays them out.
        for step in plan:
            action, _, argument = step.partition(" ")
            if action == "key" and argument == "ctrl+alt+v":
                right = "right-top"
                shown[right] = shown[focus]
                focus = right
            elif action == "key" and argument == "ctrl+shift+d":
                column = focus.split("-")[0]
                below = f"{column}-bottom"
                shown[below] = shown[focus]
                focus = below
            elif action == "key" and argument == "ctrl+Tab":
                shown[focus] = (shown[focus] + 1) % buffers
            elif action == "click":
                focus = argument
            else:
                raise AssertionError(f"unknown step: {step}")
        return shown

    def test_two_panes_show_different_buffers(self):
        shown = self.replay(2, buffers=4)
        self.assertEqual(len(shown), 2)
        self.assertEqual(len(set(shown.values())), 2)

    def test_four_panes_are_two_columns_of_two_with_different_buffers(self):
        shown = self.replay(4, buffers=4)
        self.assertEqual(sorted(shown), ["left-bottom", "left-top", "right-bottom", "right-top"])
        self.assertEqual(len(set(shown.values())), 4)

    def test_one_pane_needs_no_keys(self):
        self.assertEqual(lib("scaling_split_plan 1").stdout.strip(), "")

    def test_other_counts_are_refused(self):
        self.assertNotEqual(lib("scaling_split_plan 3").returncode, 0)


class ScriptTests(unittest.TestCase):
    def test_both_scripts_are_valid_bash(self):
        for path in (LIB, SCRIPT):
            self.assertEqual(subprocess.run(["bash", "-n", path], capture_output=True, text=True).returncode, 0, path)

    def run_script(self, *arguments):
        return subprocess.run(["bash", SCRIPT, *arguments], capture_output=True, text=True, check=False)

    def test_a_dry_run_prints_what_each_mode_would_do_without_a_jar_or_a_display(self):
        for mode, expected in (("a", "open_20"), ("b", "split4"), ("c", "heap"), ("d", "not measurable")):
            with self.subTest(mode=mode):
                result = self.run_script("--dry-run", "/no/such.jar", "label", mode)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertIn(expected, result.stdout)

    def test_an_unknown_mode_is_refused_with_the_usage(self):
        result = self.run_script("--dry-run", "/no/such.jar", "label", "z")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("a|b|c|d", result.stderr)

    def test_without_a_dry_run_a_missing_jar_is_refused(self):
        result = self.run_script("/no/such.jar", "label", "a")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("jar not found", result.stderr)


if __name__ == "__main__":
    unittest.main()
