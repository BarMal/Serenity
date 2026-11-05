#!/usr/bin/env python3
"""Growth rule for project/module-graph-baseline.tsv (#2502).

    python3 -m scripts.module_graph_baseline_growth BASE_REF

Exits 1 when the baseline gained an entry compared with BASE_REF and the diff has no `+# paydown:` line.
"""

from __future__ import annotations

import subprocess
import sys

BASELINE = "project/module-graph-baseline.tsv"


def entries(text: str) -> set[str]:
    return {line for line in text.splitlines() if line.strip() and not line.lstrip().startswith("#")}


def needs_justification(base: str | None, head: str, diff: str) -> bool:
    if base is None:
        return False
    added = entries(head) - entries(base)
    return bool(added) and not any(line.startswith("+# paydown:") for line in diff.splitlines())


def _git(*args: str) -> subprocess.CompletedProcess[str]:
    return subprocess.run(["git", *args], capture_output=True, text=True)


def main(base_ref: str) -> int:
    shown = _git("show", f"{base_ref}:{BASELINE}")
    base = shown.stdout if shown.returncode == 0 else None
    with open(BASELINE, encoding="utf-8") as handle:
        head = handle.read()
    diff = _git("diff", base_ref, "HEAD", "--", BASELINE).stdout
    if needs_justification(base, head, diff):
        print(
            f"::error file={BASELINE}::The module-graph baseline gained an entry without a justification. "
            f"Add a '# paydown: <why, issue link>' comment line to {BASELINE} in the same diff, "
            "or remove the upward edge instead."
        )
        return 1
    print("module-graph baseline: no unjustified growth")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1]))
