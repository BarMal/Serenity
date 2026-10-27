#!/usr/bin/env bash
# present-abba.sh <out-dir> <config...>
#
# Java2D against the Skia raster path, end to end through the real surface factories (see PresentBenchmarks.scala).
# A config is <logical height>_<scale>, e.g. 1080_2 for a 1920x1080 window at 2x; 1440 means 2560x1440. For each config
# the runs go A B B A A B B A (A = Java2D, B = Skia), one JVM each, so drift falls on both alike. The JVM flags are the
# packaged app's. Needs the Test classpath in $PRESENT_CP (e.g. from `sbt "export Test/fullClasspath"`); without
# $DISPLAY the runs go through xvfb-run. Rows are filtered by $PRESENT_ROWS (a name suffix prefix, default all).
#
# Results land in <out-dir>/<config>-{java2d,skia}-{1..4}.csv, the same layout as the benchmark CSVs; the table is
# `python3 scripts/present_abba_summary.py <out-dir>`.

set -euo pipefail

OUT="${1:?Usage: present-abba.sh <out-dir> <config...>}"
shift
CP="${PRESENT_CP:?set PRESENT_CP to the Test classpath}"
ROWS="${PRESENT_ROWS:-}"
mkdir -p "$OUT"

for config in "$@"; do
  height="${config%_*}"
  scale="${config#*_}"
  width=$((height == 1080 ? 1920 : 2560))
  a=0
  b=0
  for side in A B B A A B B A; do
    if [[ "$side" == A ]]; then a=$((a + 1)); backend=java2d; run=$a; else b=$((b + 1)); backend=skia; run=$b; fi
    csv="$OUT/${config}-${backend}-${run}.csv"
    display=()
    if [[ -z "${DISPLAY:-}" ]]; then display=(xvfb-run -a -s "-screen 0 2600x1500x24"); fi
    "${display[@]}" java -XX:+UseG1GC -XX:MaxRAMPercentage=25 -Xss4m \
      -Dpresent.backend="$backend" -Dpresent.width="$width" -Dpresent.height="$height" -Dpresent.scale="$scale" \
      -cp "$CP" com.serenity.perf.PresentBenchmarks ${ROWS:+"render.present.${height}p_${scale}x.${ROWS}"} 2> "$csv.err" \
      | sed -n '/^Serenity performance benchmarks$/,$p' > "$csv"
    echo "$(date +%T) $csv $(grep -c render.present "$csv" || true) rows"
  done
done
