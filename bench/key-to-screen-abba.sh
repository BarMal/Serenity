#!/usr/bin/env bash
# key-to-screen-abba.sh <jar> <label>
#
# Key to screen on the Java2D path and on the Skia layer, from the same jar: bench-laptop.sh runs java2d, skia, skia,
# java2d (A B B A, so drift falls on both alike), each its usual 3 runs, then the tagged [LATENCY] summaries are
# reduced per backend by bench/key_to_screen_summary.py. "Presented" is the end of the canvas paint for Java2D and
# the end of Skiko's render callback for the Skia layer, not GPU completion or display scan-out.
#
# Run from inside the unpacked bench-<sha> directory, in the nix-shell bench/README.md names, with the display on AC
# power and the keyboard and mouse left alone.

set -euo pipefail

JAR="${1:?Usage: key-to-screen-abba.sh <jar> <label>}"
LABEL="${2:?Usage: key-to-screen-abba.sh <jar> <label>}"
BENCH_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
RESULTS="$BENCH_DIR/bench-results"

a=0
b=0
for side in A B B A; do
  if [[ "$side" == A ]]; then a=$((a + 1)); backend=java2d; n=$a; else b=$((b + 1)); backend=skia; n=$b; fi
  bash "$BENCH_DIR/bench-laptop.sh" "$JAR" "$LABEL-$backend-$n" "$backend"
done

python3 -I "$BENCH_DIR/key_to_screen_summary.py" "$RESULTS/$LABEL"-*-run*.txt
