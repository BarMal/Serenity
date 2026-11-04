#!/usr/bin/env python3
"""Every workflow job must carry a job-level `timeout-minutes` (#2557, item 17):

    python3 -m unittest scripts.test_workflow_timeouts -v

Without one a job runs until GitHub's 360-minute default; a hung auto-squash-merge job once held a runner for 136
minutes. The limit sits on the job itself, not a step: a step limit does not bound the job's other steps.
"""

from __future__ import annotations

import re
import unittest
from pathlib import Path

_WORKFLOWS = Path(__file__).resolve().parent.parent / ".github" / "workflows"
_JOB_HEADER = re.compile(r"^  ([A-Za-z0-9_-]+):\s*$")
_TIMEOUT = re.compile(r"^    timeout-minutes: [1-9][0-9]*\s*$")
_REUSABLE_CALL = re.compile(r"^    uses: ")

# PR #2555 folds these three into one `checks` job and gives it the timeout, so adding limits here would only conflict.
# Delete this set once #2555 has merged.
_HANDLED_BY_PR_2555 = {("ci.yml", "style"), ("ci.yml", "architecture"), ("ci.yml", "third-party-notices")}


def _jobs(text: str) -> dict[str, list[str]]:
    """Maps each job id to its lines, found between the top-level `jobs:` key and the next top-level key."""
    lines = text.splitlines()
    start = next((i for i, line in enumerate(lines) if line == "jobs:"), None)
    if start is None:
        return {}
    jobs: dict[str, list[str]] = {}
    current: list[str] | None = None
    for line in lines[start + 1 :]:
        if line and not line.startswith((" ", "#")):
            break
        header = _JOB_HEADER.match(line)
        if header:
            current = jobs.setdefault(header.group(1), [])
        elif current is not None:
            current.append(line)
    return jobs


def _lacks_timeout(body: list[str]) -> bool:
    # A job that calls a reusable workflow (`uses:` at job level) cannot take `timeout-minutes`; GitHub rejects the
    # key there. The limit belongs on the called workflow's own jobs, which this test also covers.
    if any(_REUSABLE_CALL.match(line) for line in body):
        return False
    return not any(_TIMEOUT.match(line) for line in body)


class JobParsingTests(unittest.TestCase):
    def test_a_job_level_timeout_is_found_but_a_step_level_one_is_not_mistaken_for_it(self) -> None:
        text = "jobs:\n  a:\n    timeout-minutes: 10\n  b:\n    steps:\n      - run: x\n        timeout-minutes: 5\n"
        found = {name: _lacks_timeout(body) for name, body in _jobs(text).items()}
        self.assertEqual({"a": False, "b": True}, found)

    def test_a_reusable_workflow_call_is_exempt(self) -> None:
        text = "jobs:\n  a:\n    uses: ./.github/workflows/x.yml\n"
        self.assertEqual({"a": False}, {n: _lacks_timeout(b) for n, b in _jobs(text).items()})


class WorkflowTimeoutTests(unittest.TestCase):
    def test_every_job_has_a_job_level_timeout(self) -> None:
        files = sorted(_WORKFLOWS.glob("*.yml"))
        self.assertTrue(files)
        missing = [
            f"{path.name}:{job}"
            for path in files
            for job, body in _jobs(path.read_text()).items()
            if (path.name, job) not in _HANDLED_BY_PR_2555 and _lacks_timeout(body)
        ]
        self.assertEqual([], missing, "jobs without a job-level timeout-minutes")


if __name__ == "__main__":
    unittest.main()
