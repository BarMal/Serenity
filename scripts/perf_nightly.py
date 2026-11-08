#!/usr/bin/env python3
"""Logic for .github/workflows/perf-nightly-abba.yml (issue #2557, item 10): the nightly ABBA that checks the rows the
per-PR gate excludes (scripts/perf-gate-excluded-rows.txt).

    perf_nightly.py prefixes <exclude-file>
        The excluded rows, one per line, as the name prefixes `bench/abba.sh` takes.
    perf_nightly.py previous-sha <runs.json>
        The commit the last successful nightly ran on, from the `actions/workflows/<file>/runs` API response; empty when
        there is none.
    perf_nightly.py verdict <abba-status>
        Turns `bench/abba.sh`'s exit status (abba_compare.py's: 0 clean, 1 blocking, 2 no data) into the job's result.
"""

from __future__ import annotations

import argparse
import json
import sys

from compare_perf_runs import load_excluded

CLEAN, BLOCKING, NO_DATA = 0, 1, 2


def previous_nightly_sha(runs_json: dict) -> str:
    """The head commit of the most recent successful run; a failed night is not a baseline, so a regression stays red
    until it is fixed instead of being compared against once and forgotten."""
    successes = [
        run
        for run in runs_json.get("workflow_runs", [])
        if run.get("status") == "completed" and run.get("conclusion") == "success" and run.get("head_sha")
    ]
    if not successes:
        return ""
    return max(successes, key=lambda run: run.get("created_at", ""))["head_sha"]


def verdict(status: int, prefixes: list[str]) -> tuple[int, str]:
    rows = ", ".join(prefixes)
    if status == CLEAN:
        return 0, f"::notice::No blocking change on the excluded rows ({rows})."
    if status == BLOCKING:
        return 1, f"::error::The nightly ABBA found a blocking change on the excluded rows ({rows}); see the findings above."
    return 1, f"::error::The nightly ABBA produced no comparable data (status {status}); see the raw output in the artifact."


def main_with_args(argv: list[str]) -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    commands = parser.add_subparsers(dest="command", required=True)
    commands.add_parser("prefixes").add_argument("exclude_file")
    commands.add_parser("previous-sha").add_argument("runs_json")
    verdict_parser = commands.add_parser("verdict")
    verdict_parser.add_argument("status", type=int)
    verdict_parser.add_argument("--exclude-file", required=True)
    args = parser.parse_args(argv)

    if args.command == "prefixes":
        print("\n".join(load_excluded(args.exclude_file)))
        return 0
    if args.command == "previous-sha":
        with open(args.runs_json) as handle:
            print(previous_nightly_sha(json.load(handle)))
        return 0
    code, message = verdict(args.status, load_excluded(args.exclude_file))
    print(message)
    return code


def main() -> int:
    return main_with_args(sys.argv[1:])


if __name__ == "__main__":
    sys.exit(main())
