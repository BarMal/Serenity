#!/usr/bin/env python3
"""Gate a nightly desktop build on the Cross-OS Tests nightly (#2557).

    gh api 'repos/OWNER/REPO/actions/workflows/cross-os-tests.yml/runs?branch=master&event=schedule' \\
        | python3 scripts/cross_os_nightly_gate.py

Looks at the newest completed scheduled run. If it concluded `success`, prints the commit it tested (and nothing else)
on stdout and exits 0, so the caller can build and publish exactly that commit. Otherwise exits 1 with a one-line
reason on stderr.

The commit is the nightly's, not master's current head: GitHub can delay a scheduled run by hours and master moves
meanwhile, so demanding the two match would almost never publish. The nightly desktop build trusts the nightly result
for the commit that result is about. A run that is still in progress is ignored, so the previous completed one decides.
"""

from __future__ import annotations

import argparse
import json
import sys
from dataclasses import dataclass
from typing import Any, Sequence


@dataclass(frozen=True)
class Verdict:
    passed: bool
    reason: str
    sha: str | None = None


def _newest_completed(payload: dict[str, Any]) -> dict[str, Any] | None:
    completed = [run for run in payload.get("workflow_runs", []) if run.get("status") == "completed"]
    return max(completed, key=lambda run: run.get("created_at", ""), default=None)


def check(payload: dict[str, Any]) -> Verdict:
    run = _newest_completed(payload)
    if run is None:
        return Verdict(False, "no completed Cross-OS Tests nightly run found on master")
    tested = run.get("head_sha", "")
    conclusion = run.get("conclusion")
    if conclusion != "success":
        return Verdict(False, f"the newest Cross-OS Tests nightly ({tested[:12]}) concluded {conclusion}, not success")
    return Verdict(True, f"Cross-OS Tests nightly is green on {tested[:12]}", tested)


def main(argv: Sequence[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("runs", nargs="?", help="JSON file of workflow runs; stdin when omitted")
    args = parser.parse_args(argv)
    try:
        if args.runs:
            with open(args.runs, encoding="utf-8") as handle:
                payload = json.load(handle)
        else:
            payload = json.load(sys.stdin)
    except json.JSONDecodeError as error:
        print(f"Cross-OS nightly gate: input is not valid JSON ({error.msg})", file=sys.stderr)
        return 1
    verdict = check(payload)
    if verdict.passed:
        print(verdict.reason, file=sys.stderr)
        print(verdict.sha)
        return 0
    print(f"Cross-OS nightly gate: {verdict.reason}", file=sys.stderr)
    return 1


if __name__ == "__main__":
    sys.exit(main())
