#!/usr/bin/env bash
# startup-variance.sh <app-image> <runs-per-round> <out-dir>
#
# Measures how much time-to-first-frame varies on this machine, with and without the image's JDK 25 AOT cache (#2420).
# Six rounds of bench/ttfw.py run in the order warm cold cold warm warm cold, so drift falls on both modes alike (the
# ABBA idea of docs/performance-benchmarks.md); <runs-per-round> timed runs each, after one discarded run. "Cold" here
# means no AOT cache: the OS file cache is warm for both. Writes <out-dir>/{conditions,round-N,spread}.txt and prints
# the spread. The image is put back as found. Needs a display ($DISPLAY, or run under xvfb-run).
set -euo pipefail

image="${1:?usage: $0 path/to/app-image runs-per-round out-dir}"
runs="${2:?usage: $0 path/to/app-image runs-per-round out-dir}"
out="${3:?usage: $0 path/to/app-image runs-per-round out-dir}"
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
image="$(cd "$image" && pwd)"
cache="$image/lib/app/serenity.aot"

if [[ ! -f "$cache" ]]; then
  echo "No AOT cache at $cache; train the image first (scripts/train-aot-cache.sh)." >&2
  exit 1
fi
mkdir -p "$out"

hidden="$(mktemp -d)"
restore() {
  if [[ -f "$hidden/serenity.aot" ]]; then mv "$hidden/serenity.aot" "$cache"; fi
  rm -rf "$hidden"
}
trap restore EXIT

{
  echo "date: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
  echo "kernel: $(uname -sr)"
  echo "cpus: $(nproc 2>/dev/null || echo unknown)"
  echo "load: $(cut -d' ' -f1-3 /proc/loadavg 2>/dev/null || echo unknown)"
  echo "cpu: $(grep -m1 'model name' /proc/cpuinfo 2>/dev/null | cut -d: -f2- | xargs || echo unknown)"
  echo "launcher: $image/bin/Serenity"
} > "$out/conditions.txt"

round=0
for mode in warm cold cold warm warm cold; do
  round=$((round + 1))
  if [[ "$mode" == cold ]]; then mv "$cache" "$hidden/serenity.aot"; fi
  python3 "$here/ttfw.py" "$image/bin/Serenity" "$runs" "$mode" 1 | tee "$out/round-$round.txt" || true
  if [[ "$mode" == cold ]]; then mv "$hidden/serenity.aot" "$cache"; fi
done

python3 "$here/startup-spread.py" "$out"/round-*.txt | tee "$out/spread.txt"
