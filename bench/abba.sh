#!/usr/bin/env bash
# abba.sh <A-perf-jar> <B-perf-jar> <out-dir> [name-prefix...]
#
# Runs PerformanceBenchmarks eight times in the order A B B A A B B A, so drift in the machine (thermal throttling,
# a neighbour's load, a warming page cache) falls on both builds alike, then applies the blocking thresholds in
# docs/performance-benchmarks.md ("ABBA protocol") with scripts/abba_compare.py. A is the baseline build, B the
# candidate; both are the `serenity-perf.jar` that bench-artifacts.yml builds for a ref.
#
# Name prefixes limit the run, e.g. `layout.anchor_wrap. layout.cold_font. laptop.input.state_manager.two_runtimes.
# footprint.`. With none, the whole suite runs. Without $DISPLAY the runs go through `xvfb-run`.
#
# Then the same eight runs again with -XX:-DoEscapeAnalysis added to ABBA_JAVA_OPTS: allocation is judged on that pass.
# With escape analysis C2 removes a share of a frame's allocation that varies from one JVM launch to the next (up to about
# 13% on the render rows, #2618), so the default pass cannot hold a 5% threshold; without it the figure is exact. Time
# and footprint are judged on the first pass. ABBA_ALLOCATION_PASS=0 leaves the second pass out, and allocation is then
# judged on the first, as before.
#
# Results land in <out-dir>/{a,b}-{1..4}.csv and <out-dir>/alloc-{a,b}-{1..4}.csv; exits with abba_compare.py's status
# (0 clean, 1 blocking, 2 no data); a missing jar or arguments exit 2 too.

set -euo pipefail

# Exit 2, not 1, for a setup failure: 1 means a benchmark blocks, and a missing jar must not read as one.
[[ $# -ge 3 ]] || { echo "Usage: abba.sh <A-perf-jar> <B-perf-jar> <out-dir> [name-prefix...]" >&2; exit 2; }
A_JAR="$1"
B_JAR="$2"
OUT="$3"
shift 3
PREFIXES=("$@")

for jar in "$A_JAR" "$B_JAR"; do
  [[ -f "$jar" ]] || { echo "ERROR: jar not found: '$jar'" >&2; exit 2; }
done
mkdir -p "$OUT"
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

run_once() {
  local jar="$1" csv="$2" extra_opts="${3:-}" display=()
  if [[ -z "${DISPLAY:-}" ]]; then display=(xvfb-run -a -s "-screen 0 1920x1200x24"); fi
  "${display[@]}" java ${ABBA_JAVA_OPTS:-} ${extra_opts} -cp "$jar" com.serenity.perf.PerformanceBenchmarks "${PREFIXES[@]}" \
    > "$csv.raw" 2> "$csv.err" || true
  sed -n '/^Serenity performance benchmarks$/,$p' "$csv.raw" > "$csv"
  echo "$csv: $(grep -c . "$csv" || true) lines"
}

# One A B B A A B B A pass: files <prefix>a-1..4.csv and <prefix>b-1..4.csv, with extra JVM options for each launch.
pass() {
  local prefix="$1" extra_opts="${2:-}" a=0 b=0 side
  for side in A B B A A B B A; do
    if [[ "$side" == A ]]; then a=$((a + 1)); run_once "$A_JAR" "$OUT/${prefix}a-$a.csv" "$extra_opts"; else b=$((b + 1)); run_once "$B_JAR" "$OUT/${prefix}b-$b.csv" "$extra_opts"; fi
  done
}

pass ""
allocation_args=()
if [[ "${ABBA_ALLOCATION_PASS:-1}" != 0 ]]; then
  pass "alloc-" "-XX:-DoEscapeAnalysis"
  allocation_args=(--allocation-a "$OUT"/alloc-a-{1..4}.csv --allocation-b "$OUT"/alloc-b-{1..4}.csv)
fi

python3 "$HERE/../scripts/abba_compare.py" --a "$OUT"/a-{1..4}.csv --b "$OUT"/b-{1..4}.csv "${allocation_args[@]}"
