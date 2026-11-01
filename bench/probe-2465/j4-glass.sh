#!/usr/bin/env bash
# j4-glass.sh <artifact-dir> <out-dir>
# The #1812 glass recipe on one 600x400 logical panel: on Skiko's GPU layer in a real window (frames as fast as Skiko draws
# them, vsync off; the render API is printed so a software fallback is not read as a GPU result), and on Skia's CPU raster
# at scale 2 with a Java2D-style CPU blur next to it.
set -euo pipefail
ART="${1:?artifact dir}"; OUT="${2:?out dir}"; mkdir -p "$OUT"
CP="$ART/serenity-perf.jar:$ART/Serenity.jar"
java -Dskiko.vsync.enabled=false --enable-native-access=ALL-UNNAMED -cp "$CP" com.serenity.perf.GlassProbe layer 300 2>&1 | grep -E "GLASS-PROBE|^G[0-9]"
java --enable-native-access=ALL-UNNAMED -cp "$CP" com.serenity.perf.GlassProbe 2 150 2>&1 | grep -E "GLASS-PROBE|^V[0-9]|^[0-9a-z]"
