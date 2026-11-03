#!/usr/bin/env python3
"""Gate a nightly desktop build on the Cross-OS Tests nightly (#2557).

    gh api 'repos/OWNER/REPO/actions/workflows/cross-os-tests.yml/runs?branch=master&event=schedule' \\
        | python3 scripts/cross_os_nightly_gate.py --sha "$SHA"

Passes (exit 0) only when the newest completed scheduled run concluded `success` and tested exactly the commit being
built. Otherwise exits 1 with a one-line reason on stderr.

The gate does not wait for a nightly that is still running: Desktop Publish is scheduled 86 minutes after the nightly
and each test shard is capped at 60 minutes, so a nightly of the same commit that is still unfinished is late or stuck,
and failing (then re-running the publish) is clearer than polling behind a timeout.
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


def _newest_completed(payload: dict[str, Any]) -> dict[str, Any] | None:
    completed = [run for run in payload.get("workflow_runs", []) if run.get("status") == "completed"]
    return max(completed, key=lambda run: run.get("created_at", ""), default=None)


def check(payload: dict[str, Any], sha: str) -> Verdict:
    run = _newest_completed(payload)
    if run is None:
        return Verdict(False, "no completed Cross-OS Tests nightly run found on master")
    conclusion = run.get("conclusion")
    tested = run.get("head_sha", "")
    if conclusion != "success":
        return Verdict(False, f"the newest Cross-OS Tests nightly ({tested[:12]}) concluded {conclusion}, not success")
    if tested != sha:
        return Verdict(
            False, f"the newest green Cross-OS Tests nightly tested {tested[:12]}, not the commit being built {sha[:12]}"
        )
    return Verdict(True, f"Cross-OS Tests nightly is green on {sha[:12]}")


def main(argv: Sequence[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--sha", required=True, help="the commit being built")
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
    verdict = check(payload, args.sha)
    if verdict.passed:
        print(verdict.reason)
        return 0
    print(f"Cross-OS nightly gate: {verdict.reason}", file=sys.stderr)
    return 1


if __name__ == "__main__":
    sys.exit(main())
