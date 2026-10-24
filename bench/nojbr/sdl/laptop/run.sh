#!/usr/bin/env bash
# Throwaway (#2380, never merge). Benchmarks SDL3's Wayland software surface on the compositor you are running in.
# Writes only inside SCRATCH (default ./sdl-bench-scratch). Installs nothing, changes no config.
#
# usage: run.sh [SCRATCH]   env: JAVA (default: java from PATH), FRAMES (default 600), DEBUG_FRAMES (default 300)
set -euo pipefail

SCRATCH="$(mkdir -p "${1:-sdl-bench-scratch}" && cd "${1:-sdl-bench-scratch}" && pwd)"
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
JAVA="${JAVA:-$(command -v java)}"
JAVAC="${JAVAC:-$(dirname "$(readlink -f "$JAVA")")/javac}"
FRAMES="${FRAMES:-600}"
DEBUG_FRAMES="${DEBUG_FRAMES:-300}"
M=https://repo1.maven.org/maven2

[ -n "${WAYLAND_DISPLAY:-}" ] || { echo "WAYLAND_DISPLAY is not set: run this inside the Hyprland session" >&2; exit 1; }
"$JAVA" -version 2>&1 | head -2
[ -x "$JAVAC" ] || { echo "no javac next to $JAVA; set JAVAC" >&2; exit 1; }

mkdir -p "$SCRATCH/lib" "$SCRATCH/classes" "$SCRATCH/out"
for path in org/lwjgl/lwjgl/3.4.3/lwjgl-3.4.3.jar org/lwjgl/lwjgl/3.4.3/lwjgl-3.4.3-natives-linux.jar \
            org/lwjgl/lwjgl-sdl/3.4.3/lwjgl-sdl-3.4.3.jar org/lwjgl/lwjgl-sdl/3.4.3/lwjgl-sdl-3.4.3-natives-linux.jar; do
  f="$SCRATCH/lib/$(basename "$path")"
  [ -f "$f" ] || curl -fsSL --retry 3 -o "$f" "$M/$path"
  # Maven Central publishes a .sha1 next to every artifact.
  if want="$(curl -fsSL --retry 3 "$M/$path.sha1" 2>/dev/null | awk '{print $1}')" && [ -n "$want" ]; then
    [ "$(sha1sum "$f" | awk '{print $1}')" = "$want" ] || { echo "checksum mismatch: $f" >&2; exit 1; }
  else
    echo "warning: no .sha1 available for $path, jar not verified" >&2
  fi
done

L="$SCRATCH/lib"
CP="$SCRATCH/classes:$L/lwjgl-3.4.3.jar:$L/lwjgl-3.4.3-natives-linux.jar:$L/lwjgl-sdl-3.4.3.jar:$L/lwjgl-sdl-3.4.3-natives-linux.jar"
"$JAVAC" -cp "$CP" -d "$SCRATCH/classes" "$HERE/SdlBench.java"

run() { # density outdir frames [only-arm]  (stderr = Wayland wire log when WAYLAND_DEBUG=1)
  SDL_VIDEO_DRIVER=wayland "$JAVA" --enable-native-access=ALL-UNNAMED -Djava.awt.headless=true \
    -cp "$CP" SdlBench "$2" "$1" "$3" ${4:+"$4"}
}

{
  echo "=== host ==="
  echo "date: $(date -u +%FT%TZ)"
  echo "kernel: $(uname -sr)"
  echo "compositor: ${XDG_CURRENT_DESKTOP:-unknown} (WAYLAND_DISPLAY=$WAYLAND_DISPLAY)"
  command -v hyprctl >/dev/null && hyprctl monitors 2>/dev/null | grep -E 'Monitor|@|scale' || true
  grep -m1 'model name' /proc/cpuinfo || true
  for g in /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor; do [ -r "$g" ] && echo "cpu governor: $(cat "$g")"; done
  [ -r /sys/class/power_supply/AC/online ] && echo "on AC: $(cat /sys/class/power_supply/AC/online)" || true
} | tee "$SCRATCH/out/host.txt"

echo "Keep the window visible on the active workspace and do not touch the keyboard or mouse while each run goes."
for density in native one; do
  echo "=== pass 1 ($density): timing, no wire logging ==="
  run "$density" "$SCRATCH/out/$density" "$FRAMES" | grep -E '^(META|BENCH|DONE|FATAL)'
done

for density in native one; do
  size="$(grep -m1 '^META surface=' "$SCRATCH/out/$density/bench.txt" | sed -E 's/META surface=([0-9]+)x([0-9]+).*/\1 \2/')"
  for arm in vsync-on/full vsync-on/typing vsync-off/full vsync-off/typing; do
    tag="$density-$(echo "$arm" | tr / -)"
    mkdir -p "$SCRATCH/out/wire-$tag"
    echo "=== pass 2 ($density, $arm): wire log ($DEBUG_FRAMES frames) ==="
    WAYLAND_DEBUG=1 run "$density" "$SCRATCH/out/wire-$tag" "$DEBUG_FRAMES" "$arm" 2> "$SCRATCH/out/wire-$tag/wire.log" | grep -E '^(BENCH|FATAL)' || true
    # shellcheck disable=SC2086
    python3 -I "$HERE/analyze.py" "$SCRATCH/out/wire-$tag/wire.log" $size | tee "$SCRATCH/out/wire-$tag/analysis.txt"
  done
done

cd "$SCRATCH/out"
{
  echo "=== SUMMARY ==="
  cat host.txt
  for d in native one; do grep -E '^(META|BENCH)' "$d/bench.txt" | sed "s/^/[$d] /"; done
  for d in wire-*; do echo "[$d] $(grep -E '^(frame-callback|damage:)' "$d/analysis.txt" | tr '\n' ' ')"; done
  echo "=== END SUMMARY ==="
} | tee summary.txt
echo "Send back: $SCRATCH/out/summary.txt (and, if you like, the per-frame frames-*.csv files in $SCRATCH/out/native and /one)."
