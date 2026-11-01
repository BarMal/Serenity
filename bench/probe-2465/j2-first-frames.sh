#!/usr/bin/env bash
# j2-first-frames.sh <artifact-dir> <out-dir>
# Prints the time of each of the first 14 frames of each renderer's window (Java2D, Skia layer, Skia raster), and how long
# after the window was made it ended. Java2D runs first in the process, so its first frame also pays JVM warm-up that the
# later windows share; compare the Skia layer with the Skia raster, which come after it. Run it three times.
set -euo pipefail
ART="${1:?artifact dir}"; OUT="${2:?out dir}"; mkdir -p "$OUT/home"
cd "$OUT"
java -Duser.home="$OUT/home" -Dserenity.probe.firstFrames=true --enable-native-access=ALL-UNNAMED -jar "$ART/Serenity.jar" \
  --render-benchmark --render-benchmark-size 1600x1000 --render-benchmark-frames 20 --render-benchmark-warmup 5 2>&1 \
  | grep -E "^PROBE first-frames|render API"
