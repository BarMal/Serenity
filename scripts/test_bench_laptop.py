import os
import stat
import subprocess
import tempfile
import unittest

BENCH = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "bench")
LIB = os.path.join(BENCH, "bench-lib.sh")
SCRIPT = os.path.join(BENCH, "bench-laptop.sh")

FRAME = (
    "[FRAME] window=5.0s full=10 (2.0/s) cursor=0 (0.0/s) | render p50=2.00 p95=3.00 max=4.00 n=10 ms | "
    "input-to-paint p50=1.00 p95=2.00 max=3.00 n=10 ms"
)


def lib(command, env=None):
    """Runs `command` in bash with bench-lib.sh sourced."""
    merged = dict(os.environ, **(env or {}))
    return subprocess.run(
        ["bash", "-c", f'source "{LIB}"; {command}'], capture_output=True, text=True, env=merged, check=False
    )


def write(path, text):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w", encoding="utf-8") as handle:
        handle.write(text)


class FindAppLogTests(unittest.TestCase):
    def test_finds_the_log_in_the_state_directory_the_app_writes_to_now(self):
        with tempfile.TemporaryDirectory() as home:
            log = os.path.join(home, ".local", "state", "serenity", "serenity.log")
            write(log, "Initial render completed\n")
            self.assertEqual(lib(f'find_app_log "{home}"').stdout.strip(), log)

    def test_falls_back_to_the_old_location(self):
        with tempfile.TemporaryDirectory() as home:
            log = os.path.join(home, ".serenity", "serenity.log")
            write(log, "Initial render completed\n")
            self.assertEqual(lib(f'find_app_log "{home}"').stdout.strip(), log)

    def test_prefers_the_state_directory_when_both_exist(self):
        with tempfile.TemporaryDirectory() as home:
            new = os.path.join(home, ".local", "state", "serenity", "serenity.log")
            write(new, "new\n")
            write(os.path.join(home, ".serenity", "serenity.log"), "old\n")
            self.assertEqual(lib(f'find_app_log "{home}"').stdout.strip(), new)

    def test_prints_nothing_and_fails_when_there_is_no_log(self):
        with tempfile.TemporaryDirectory() as home:
            result = lib(f'find_app_log "{home}"')
            self.assertNotEqual(result.returncode, 0)
            self.assertEqual(result.stdout.strip(), "")


class AcOnlineTests(unittest.TestCase):
    def test_reads_an_ac_supply_whatever_its_number(self):
        with tempfile.TemporaryDirectory() as supplies:
            write(os.path.join(supplies, "BAT0", "status"), "Full\n")
            write(os.path.join(supplies, "AC0", "online"), "1\n")
            self.assertEqual(lib("ac_online", {"BENCH_POWER_SUPPLY_DIR": supplies}).stdout.strip(), "1")

    def test_reads_a_supply_called_plain_ac(self):
        with tempfile.TemporaryDirectory() as supplies:
            write(os.path.join(supplies, "AC", "online"), "0\n")
            self.assertEqual(lib("ac_online", {"BENCH_POWER_SUPPLY_DIR": supplies}).stdout.strip(), "0")

    def test_says_unknown_when_no_ac_supply_reports(self):
        with tempfile.TemporaryDirectory() as supplies:
            write(os.path.join(supplies, "BAT0", "status"), "Full\n")
            self.assertEqual(lib("ac_online", {"BENCH_POWER_SUPPLY_DIR": supplies}).stdout.strip(), "unknown")


class RunDataTests(unittest.TestCase):
    def test_a_run_with_frame_lines_has_data(self):
        with tempfile.TemporaryDirectory() as results:
            write(os.path.join(results, "x-run1.txt"), f"Launched PID 1\n{FRAME}\n")
            self.assertEqual(lib(f'runs_with_data "{results}" x').stdout.strip(), "1")

    def test_a_run_that_only_timed_out_has_none(self):
        with tempfile.TemporaryDirectory() as results:
            write(os.path.join(results, "x-run1.txt"), "Launched PID 1\nERROR: timed out waiting for initial render\n")
            write(os.path.join(results, "x-run2.txt"), "(none)\n")
            self.assertEqual(lib(f'runs_with_data "{results}" x').stdout.strip(), "0")


class StatsTests(unittest.TestCase):
    def test_stats_fail_when_no_run_produced_a_frame_line(self):
        with tempfile.TemporaryDirectory() as results:
            for n in (1, 2, 3):
                write(os.path.join(results, f"x-run{n}.txt"), "ERROR: timed out waiting for initial render\n")
            result = lib(f'compute_stats "{results}" x')
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("No run produced data", result.stdout + result.stderr)

    def test_stats_pass_when_a_run_produced_frame_lines(self):
        with tempfile.TemporaryDirectory() as results:
            write(os.path.join(results, "x-run1.txt"), f"=== [FRAME] lines ===\n{FRAME}\n")
            result = lib(f'compute_stats "{results}" x')
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertIn("input-to-paint", result.stdout)


class WholeScriptTests(unittest.TestCase):
    def test_exits_non_zero_when_every_launch_fails(self):
        with tempfile.TemporaryDirectory() as work:
            fake_bin = os.path.join(work, "bin")
            os.makedirs(fake_bin)
            java = os.path.join(fake_bin, "java")
            write(java, '#!/usr/bin/env bash\nif [[ "$1" == "-version" ]]; then echo "openjdk 25" >&2; exit 0; fi\nexit 1\n')
            os.chmod(java, os.stat(java).st_mode | stat.S_IXUSR)
            jar = os.path.join(work, "Serenity.jar")
            write(jar, "not a jar\n")
            # A copy of bench/ with a lorem file, so the script's results land in the temp directory.
            bench = os.path.join(work, "bench")
            os.makedirs(bench)
            for name in ("bench-laptop.sh", "bench-lib.sh"):
                with open(os.path.join(BENCH, name), encoding="utf-8") as source:
                    write(os.path.join(bench, name), source.read())
            write(os.path.join(bench, "lorem-bench.txt"), "lorem\n")
            env = dict(os.environ, PATH=fake_bin + os.pathsep + os.environ["PATH"], BENCH_RUN_PAUSE="0")
            result = subprocess.run(
                ["bash", os.path.join(bench, "bench-laptop.sh"), jar, "t"],
                capture_output=True,
                text=True,
                env=env,
                check=False,
            )
            self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertIn("No run produced data", result.stdout + result.stderr)


if __name__ == "__main__":
    unittest.main()
