#!/usr/bin/env bash
# abba-startup.sh <A-launcher> <B-launcher> <out-dir> [runs-per-side-run]
#
# The ABBA protocol of docs/performance-benchmarks.md for start-up (#2420). A and B are the launchers of two trained
# Linux app images (bin/Serenity, with lib/app/serenity.aot beside it), A the baseline and B the candidate. Runs
# bench/startup.py eight times in the order A B B A A B B A, <runs-per-side-run> timed launches per mode each (default 5),
# then applies the blocking thresholds with scripts/abba_compare.py: 5% and ABBA_STARTUP_MIN_DELTA_MS (default 100 ms,
# not the 0.03 ms of in-process rows) over non-overlapping ranges. Basis: the 5% is the ABBA protocol's own (docs/
# performance-benchmarks.md). The 100 ms is a GUESS, a starting point that nothing here has measured on the laptop; on a
# GitHub runner two runs of one commit differed by 77 ms (warm) and 114 ms (cold) (#2420, comment 6480029115), so it is
# at the level of runner noise there. Replace it with a laptop A/A measurement before relying on it as a gate. Without $DISPLAY the runs go through xvfb-run.
#
# Results land in <out-dir>/{a,b}-{1..4}.csv; exits with abba_compare.py's status (0 clean, 1 blocking, 2 no data).
# ABBA_STARTUP_REPORT_ONLY=1 makes it report-only (#2420: start-up is tracked, not gated on CI): findings are ::warning::
# lines and it exits 0 whatever happens. Leave it unset on the laptop, where this is the gate.
set -euo pipefail

A="${1:?Usage: abba-startup.sh <A-launcher> <B-launcher> <out-dir> [runs]}"
B="${2:?Usage: abba-startup.sh <A-launcher> <B-launcher> <out-dir> [runs]}"
OUT="${3:?Usage: abba-startup.sh <A-launcher> <B-launcher> <out-dir> [runs]}"
RUNS="${4:-5}"

report_only="${ABBA_STARTUP_REPORT_ONLY:-}"
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

run_once() {
  local launcher="$1" csv="$2" display=()
  if [[ -z "${DISPLAY:-}" ]]; then display=(xvfb-run -a -s "-screen 0 1280x800x24"); fi
  "${display[@]}" python3 "$HERE/startup.py" "$launcher" "$RUNS" --csv "$csv" > "$csv.log"
  echo "$csv: $(grep -c '^startup\.' "$csv") rows"
}

measure_and_compare() {
  for launcher in "$A" "$B"; do
    [[ -x "$launcher" ]] || { echo "ERROR: launcher not found or not executable: $launcher" >&2; return 1; }
  done
  mkdir -p "$OUT"
  local a=0 b=0 side
  for side in A B B A A B B A; do
    if [[ "$side" == A ]]; then a=$((a + 1)); run_once "$A" "$OUT/a-$a.csv"; else b=$((b + 1)); run_once "$B" "$OUT/b-$b.csv"; fi
  done
  python3 "$HERE/../scripts/abba_compare.py" --a "$OUT"/a-{1..4}.csv --b "$OUT"/b-{1..4}.csv \
    --time-delta-ms "${ABBA_STARTUP_MIN_DELTA_MS:-100}" ${report_only:+--report-only}
}

if [[ -n "$report_only" ]]; then
  measure_and_compare || echo "::warning::abba-startup could not complete (report-only, so this does not fail the job)."
  exit 0
fi
measure_and_compare
