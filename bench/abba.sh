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
# Results land in <out-dir>/{a,b}-{1..4}.csv; exits with abba_compare.py's status (0 clean, 1 blocking, 2 no data).

set -euo pipefail

A_JAR="${1:?Usage: abba.sh <A-perf-jar> <B-perf-jar> <out-dir> [name-prefix...]}"
B_JAR="${2:?Usage: abba.sh <A-perf-jar> <B-perf-jar> <out-dir> [name-prefix...]}"
OUT="${3:?Usage: abba.sh <A-perf-jar> <B-perf-jar> <out-dir> [name-prefix...]}"
shift 3
PREFIXES=("$@")

for jar in "$A_JAR" "$B_JAR"; do
  [[ -f "$jar" ]] || { echo "ERROR: jar not found: $jar" >&2; exit 1; }
done
mkdir -p "$OUT"
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

run_once() {
  local jar="$1" csv="$2" display=()
  if [[ -z "${DISPLAY:-}" ]]; then display=(xvfb-run -a -s "-screen 0 1920x1200x24"); fi
  "${display[@]}" java ${ABBA_JAVA_OPTS:-} -cp "$jar" com.serenity.perf.PerformanceBenchmarks "${PREFIXES[@]}" \
    > "$csv.raw" 2> "$csv.err" || true
  sed -n '/^Serenity performance benchmarks$/,$p' "$csv.raw" > "$csv"
  echo "$csv: $(grep -c . "$csv" || true) lines"
}

a=0
b=0
for side in A B B A A B B A; do
  if [[ "$side" == A ]]; then a=$((a + 1)); run_once "$A_JAR" "$OUT/a-$a.csv"; else b=$((b + 1)); run_once "$B_JAR" "$OUT/b-$b.csv"; fi
done

python3 "$HERE/../scripts/abba_compare.py" --a "$OUT"/a-{1..4}.csv --b "$OUT"/b-{1..4}.csv
