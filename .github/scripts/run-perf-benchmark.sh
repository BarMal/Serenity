#!/usr/bin/env bash
# Runs PerformanceBenchmarks in <dir> under a virtual display and writes its CSV to <out>. Any further arguments are
# benchmark names (prefixes) to run alone; with none, the whole suite runs.
#   run-perf-benchmark.sh <dir> <out> [benchmark ...]
# Needs SCREEN (e.g. 1920x1200x24), DPI and SCALE (Java2D's uiScale) in the environment. SHARD (index/count, default 1/1)
# runs only that contiguous slice of the suite.
set -uo pipefail

# The option to run only this job's slice of the suite in <dir>: none for the whole suite, and none for a checkout from
# before the option existed (the base commit of the PR that introduced it), which would read "--shard=..." as the name of
# a benchmark, run nothing, and leave the comparison with no pairs. That checkout runs the whole suite instead and the
# comparison only looks at the benchmarks both sides measured.
shard_option_for() {
  local checkout="$1" shard="${SHARD:-1/1}"
  if [ "$shard" != "1/1" ] && [ -f "$checkout/src/test/scala/com/serenity/perf/BenchmarkShard.scala" ]; then
    echo "--shard=${shard}"
  fi
}

if [ "${1:-}" = "--shard-option" ]; then
  shard_option_for "$2"
  exit 0
fi

dir="$1" out="$2"
shift 2
names="$*"
shard_option="$(shard_option_for "$dir")"

# sbt prefixes every line of runMain's stdout with "[info] " and `sbt -v` wraps it in ANSI colour escapes; strip both,
# keeping from the suite's own title line onward so earlier sbt log noise is excluded. A run that crashes leaves an
# empty CSV, which the comparison skips or reports.
# A run that produced no rows measured nothing: dependency resolution failing on the runner's network (seen as a
# missing scala3-sbt-bridge) says nothing about the commit, so it gets one more attempt. A benchmark that runs and is
# slow is never retried -- the paired comparison decides that.
for attempt in 1 2; do
  (cd "$dir" && xvfb-run -a -s "-screen 0 ${SCREEN} -dpi ${DPI}" \
    sbt -v "set Test / run / javaOptions += \"-Dsun.java2d.uiScale=${SCALE}\"" \
    "Test/runMain com.serenity.perf.PerformanceBenchmarks ${shard_option} ${names}") > "$out.raw" 2>&1 || true
  sed -E 's/\x1b\[[0-9;]*[a-zA-Z]//g' "$out.raw" \
    | sed -n '/^\[info\] Serenity performance benchmarks$/,$p' \
    | sed 's/^\[info\] //' > "$out"
  echo "$out (attempt $attempt): $(grep -c . "$out" || true) lines"
  if [ -s "$out" ]; then break; fi
done
