#!/usr/bin/env bash
# typing-paint-flame.sh <out-dir>
#
# Profiles com.serenity.perf.TypingPaintProfile under async-profiler (itimer, collapsed output) and prints the top
# frames. $ASYNC_PROFILER_LIB is the absolute path of libasyncProfiler.so; the collapsed file is written when the JVM
# exits, so the main has to exit normally. The config is $PROFILE_CONFIG, <logical height>_<scale> as in present-abba.sh
# (default 1080_2; 1080 means 1920 wide, anything else 2560). $PROFILE_SECONDS (default 30) is how long it runs and
# $PROFILE_VARIANT (default skia) which path. The JVM flags are the packaged app's. Needs the Test classpath in
# $PRESENT_CP; without $DISPLAY the run goes through xvfb-run.
#
# Result: <out-dir>/<config>-<variant>.collapsed, summarised by scripts/collapsed_top_frames.py.

set -euo pipefail

OUT="${1:?Usage: typing-paint-flame.sh <out-dir>}"
CP="${PRESENT_CP:?set PRESENT_CP to the Test classpath}"
LIB="${ASYNC_PROFILER_LIB:?set ASYNC_PROFILER_LIB to the absolute path of libasyncProfiler.so}"
config="${PROFILE_CONFIG:-1080_2}"
variant="${PROFILE_VARIANT:-skia}"
seconds="${PROFILE_SECONDS:-30}"
height="${config%_*}"
scale="${config#*_}"
width=$((height == 1080 ? 1920 : 2560))
mkdir -p "$OUT"
collapsed="$OUT/${config}-${variant}.collapsed"

display=()
if [[ -z "${DISPLAY:-}" ]]; then display=(xvfb-run -a -s "-screen 0 2600x1500x24"); fi
"${display[@]}" java -XX:+UseG1GC -XX:MaxRAMPercentage=25 -Xss4m \
  -XX:+UnlockDiagnosticVMOptions -XX:+DebugNonSafepoints \
  "-agentpath:${LIB}=start,event=itimer,file=${collapsed},collapsed" \
  -Dprofile.seconds="$seconds" -Dprofile.height="$height" -Dprofile.width="$width" -Dprofile.scale="$scale" \
  -Dprofile.variant="$variant" \
  -cp "$CP" com.serenity.perf.TypingPaintProfile

echo "$(date +%T) $collapsed"
python3 "$(dirname "$0")/../scripts/collapsed_top_frames.py" --top 15 \
  --under Java_org_jetbrains_skia --under blit_mask "$collapsed"
