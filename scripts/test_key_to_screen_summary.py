import os
import subprocess
import sys
import tempfile
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)

import key_to_screen_summary as summary  # noqa: E402


def latency_line(backend, keys, p50, p95, unpainted=0, presented=None):
    presented = presented or ("render-callback-end" if backend == "skia" else "paint-end")
    return (
        f"[LATENCY] summary backend={backend} presented={presented} window=5.0s keys={keys} unpainted={unpainted} | "
        f"when_to_edt p50=1.00 p95=2.00 | total p50={p50:.2f} p95={p95:.2f} ms"
    )


def frame_line(backend, p50, p95):
    return (
        f"[FRAME] backend={backend} presented=paint-end window=5.0s full=10 (2.0/s) cursor=0 (0.0/s) | "
        f"render p50=2.00 p95=3.00 max=4.00 n=10 ms | input-to-paint p50={p50:.2f} p95={p95:.2f} max=9.00 n=10 ms"
    )


class ParseTests(unittest.TestCase):
    def test_reads_the_total_of_a_tagged_latency_summary(self):
        window = summary.parse_latency(latency_line("skia", 12, 8.5, 14.25, unpainted=2))
        self.assertEqual(window.backend, "skia")
        self.assertEqual(window.presented, "render-callback-end")
        self.assertEqual((window.keys, window.unpainted), (12, 2))
        self.assertEqual((window.p50, window.p95), (8.5, 14.25))

    def test_ignores_a_window_without_keystrokes(self):
        line = "[LATENCY] summary backend=skia presented=render-callback-end window=5.0s keys=0 unpainted=0 | no keystrokes"
        self.assertIsNone(summary.parse_latency(line))

    def test_ignores_per_keystroke_lines_and_untagged_summaries(self):
        self.assertIsNone(summary.parse_latency("[LATENCY] seq=7 when_to_edt=4 total=53.00 ms"))
        untagged = "[LATENCY] summary window=5.0s keys=3 unpainted=0 | total p50=1.00 p95=2.00 ms"
        self.assertIsNone(summary.parse_latency(untagged))

    def test_reads_input_to_paint_of_a_tagged_frame_line(self):
        window = summary.parse_frame(frame_line("java2d", 7.5, 11.0))
        self.assertEqual((window.backend, window.p50, window.p95), ("java2d", 7.5, 11.0))

    def test_a_frame_line_without_input_to_paint_is_ignored(self):
        line = "[FRAME] backend=java2d presented=paint-end window=5.0s full=0 (0.0/s) cursor=0 (0.0/s) | no samples"
        self.assertIsNone(summary.parse_frame(line))


class SummaryTests(unittest.TestCase):
    def test_takes_the_median_of_each_runs_windows_then_the_median_of_the_runs_per_backend(self):
        run1 = [latency_line("java2d", 10, 6.0, 9.0), latency_line("java2d", 10, 8.0, 11.0), latency_line("java2d", 10, 7.0, 10.0)]
        run2 = [latency_line("java2d", 10, 9.0, 12.0)]
        run3 = [latency_line("skia", 10, 4.0, 6.0), latency_line("skia", 10, 5.0, 7.0)]
        result = summary.summarise({"a-run1": run1, "a-run2": run2, "b-run1": run3})
        java2d = result["java2d"]
        self.assertEqual((java2d.p50, java2d.p95), (8.0, 11.0))  # runs' medians 7.0 and 9.0; the median of two is 8.0
        self.assertEqual(java2d.runs, 2)
        skia = result["skia"]
        self.assertEqual((skia.p50, skia.p95), (4.5, 6.5))
        self.assertEqual(skia.presented, "render-callback-end")

    def test_counts_the_keystrokes_and_the_unpainted_ones(self):
        result = summary.summarise({"r": [latency_line("skia", 10, 4.0, 6.0, unpainted=1), latency_line("skia", 5, 4.0, 6.0, unpainted=2)]})
        self.assertEqual((result["skia"].keys, result["skia"].unpainted), (15, 3))

    def test_a_backend_with_no_keystroke_windows_is_not_reported(self):
        empty = "[LATENCY] summary backend=skia presented=render-callback-end window=5.0s keys=0 unpainted=0 | no keystrokes"
        self.assertEqual(summary.summarise({"r": [empty]}), {})

    def test_the_table_names_what_presented_means_for_each_backend(self):
        result = summary.summarise({"a": [latency_line("java2d", 10, 6.0, 9.0)], "b": [latency_line("skia", 10, 4.0, 6.0)]})
        table = summary.table(result)
        self.assertIn("java2d", table)
        self.assertIn("paint-end", table)
        self.assertIn("render-callback-end", table)
        self.assertIn("not GPU completion", table)


class CommandLineTests(unittest.TestCase):
    def test_prints_the_table_for_run_files(self):
        with tempfile.TemporaryDirectory() as directory:
            path = os.path.join(directory, "x-run1.txt")
            with open(path, "w") as handle:
                handle.write("\n".join([latency_line("skia", 10, 4.0, 6.0), frame_line("skia", 5.0, 8.0)]) + "\n")
            done = subprocess.run(
                [sys.executable, "-I", os.path.join(HERE, "key_to_screen_summary.py"), path],
                capture_output=True,
                text=True,
            )
        self.assertEqual(done.returncode, 0, done.stderr)
        self.assertIn("skia", done.stdout)
        self.assertIn("4.00", done.stdout)

    def test_fails_when_no_file_has_a_keystroke_window(self):
        with tempfile.TemporaryDirectory() as directory:
            path = os.path.join(directory, "x-run1.txt")
            with open(path, "w") as handle:
                handle.write("nothing here\n")
            done = subprocess.run(
                [sys.executable, "-I", os.path.join(HERE, "key_to_screen_summary.py"), path],
                capture_output=True,
                text=True,
            )
        self.assertNotEqual(done.returncode, 0)


if __name__ == "__main__":
    unittest.main()
