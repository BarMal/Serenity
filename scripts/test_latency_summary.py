import contextlib
import importlib.util
import io
import os
import tempfile
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
SPEC = importlib.util.spec_from_file_location("latency_summary", os.path.join(HERE, "..", "bench", "latency-summary.py"))
latency_summary = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(latency_summary)

STAGES = (
    "when_to_edt=0 edt_to_enqueue=1.00 queue=1.00 dispatch=1.00 apply=6.00 damage=1.00 frame_wait=1.00 "
    "pacing_wait=1.00 pre_render=1.00 render=12.00 publish_to_paint=1.00 paint=1.00 total=27.00"
)
SPLITS = "apply_decode=1.00 apply_typed_step=2.00 apply_typed_commit=3.00 render_layout=4.00 render_draw=5.00 render_flush=2.00"


def summary(lines):
    with tempfile.TemporaryDirectory() as folder:
        path = os.path.join(folder, "serenity.log")
        with open(path, "w", encoding="utf-8") as log:
            for index, line in enumerate(lines):
                log.write(f"[LATENCY] seq={index} {line} ms\n")
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            status = latency_summary.main([path])
        return status, out.getvalue()


class SplitTests(unittest.TestCase):
    def test_lists_the_split_stages_under_the_stages_and_leaves_them_out_of_the_shares(self):
        status, text = summary([f"{STAGES} {SPLITS}"] * 3)
        self.assertEqual(status, 0)
        rows = text.splitlines()
        names = [row.split()[0] for row in rows[2:]]
        self.assertEqual(names[-6:], ["apply_decode", "apply_typed_step", "apply_typed_commit", "render_layout",
                                      "render_draw", "render_flush"])
        shares = [float(row.split()[-1].rstrip("%")) for row in rows[2:] if row.split()[0] in
                  ("edt_to_enqueue", "queue", "dispatch", "apply", "damage", "frame_wait", "pacing_wait",
                   "pre_render", "render", "publish_to_paint", "paint")]
        self.assertAlmostEqual(sum(shares), 100.0, delta=0.5)
        split_row = [row for row in rows if "apply_decode" in row][0]
        self.assertFalse(split_row.rstrip().endswith("%"))

    def test_reads_lines_without_split_fields_as_before(self):
        status, text = summary([STAGES] * 2)
        self.assertEqual(status, 0)
        self.assertNotIn("apply_decode", text)
        self.assertIn("render", text)


if __name__ == "__main__":
    unittest.main()
