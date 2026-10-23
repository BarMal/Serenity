#!/usr/bin/env bash
# Throwaway (Java 25 research, #2380): abba.sh with one jar and two JVMs. Env: JAR OUT A_JAVA B_JAVA A_OPTS B_OPTS.
set -uo pipefail
PREFIXES=("$@")
mkdir -p "$OUT"
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
run_once() {
  local java="$1" opts="$2" csv="$3"
  # shellcheck disable=SC2086
  xvfb-run -a -s "-screen 0 1920x1200x24" "$java" $opts -cp "$JAR" com.serenity.perf.PerformanceBenchmarks "${PREFIXES[@]}" \
    > "$csv.raw" 2> "$csv.err" || true
  sed -n '/^Serenity performance benchmarks$/,$p' "$csv.raw" > "$csv"
  echo "$csv: $(grep -c . "$csv" || true) lines"
}
a=0; b=0
for side in A B B A A B B A; do
  if [[ "$side" == A ]]; then a=$((a + 1)); run_once "$A_JAVA" "$A_OPTS" "$OUT/a-$a.csv"
  else b=$((b + 1)); run_once "$B_JAVA" "$B_OPTS" "$OUT/b-$b.csv"; fi
done
python3 "$HERE/../../scripts/abba_compare.py" --a "$OUT"/a-{1..4}.csv --b "$OUT"/b-{1..4}.csv | tee "$OUT/abba-compare.txt"
