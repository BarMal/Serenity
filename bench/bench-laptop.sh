#!/usr/bin/env nix-shell
#!nix-shell -i bash -p xdotool wmctrl python3 xorg.xwininfo coreutils procps

# bench-laptop.sh <jar> <label> [java2d|skia]
#
# With a backend, the jar is launched with --renderer=<backend> and ui.render.latency_trace is on, so the run file keeps
# the tagged [LATENCY] summaries (key to screen) beside the [FRAME] lines; bench/key-to-screen-abba.sh drives it.
#
# Runs Serenity from <jar> with an isolated config, drives deterministic input,
# collects [FRAME] timing lines, and reports p50/p95 per phase.
# Runs 3 times; reports median across runs.
#
# Usage:
#   ./bench-laptop.sh /path/to/Serenity.jar A
#   ./bench-laptop.sh /path/to/Serenity.jar B
#   ./bench-laptop.sh /path/to/Serenity.jar C
#
# Results written to bench-results/<label>-runN.txt and bench-results/<label>-summary.txt
# Run from inside a nix-shell that provides: xdotool wmctrl python3

set -euo pipefail

JAR="${1:?Usage: bench-laptop.sh <jar> <label>}"
LABEL="${2:?Usage: bench-laptop.sh <jar> <label> [java2d|skia]}"
BACKEND="${3:-}"
case "$BACKEND" in
  ""|java2d|skia) ;;
  *) echo "ERROR: backend must be java2d or skia, not: $BACKEND" >&2; exit 1 ;;
esac
RENDERER_FLAG=()
if [[ -n "$BACKEND" ]]; then RENDERER_FLAG=("--renderer=$BACKEND"); fi
BENCH_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
RESULTS_DIR="$BENCH_DIR/bench-results"
# shellcheck source=bench-lib.sh
source "$BENCH_DIR/bench-lib.sh"
LOREM_FILE="$BENCH_DIR/lorem-bench.txt"
mkdir -p "$RESULTS_DIR"

if [[ ! -f "$JAR" ]]; then
  echo "ERROR: jar not found: $JAR" >&2; exit 1
fi

if [[ ! -f "$LOREM_FILE" ]]; then
  echo "ERROR: lorem ipsum file not found: $LOREM_FILE" >&2
  echo "  Generate it with: python3 $BENCH_DIR/gen-lorem.py > $LOREM_FILE" >&2
  exit 1
fi

# ---------- conditions (recorded once) ----------
record_conditions() {
  local out="$RESULTS_DIR/${LABEL}-conditions.txt"
  echo "=== Benchmark conditions ===" > "$out"
  echo "Label: $LABEL" >> "$out"
  echo "Jar: $JAR" >> "$out"
  echo "Date: $(date -u +%Y-%m-%dT%H:%M:%SZ)" >> "$out"
  echo "" >> "$out"
  java -version 2>&1 | head -3 >> "$out"
  echo "" >> "$out"
  echo "AC online: $(ac_online)" >> "$out"
  echo "CPU governor: $(cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor 2>/dev/null || echo unknown)" >> "$out"
  echo "Load average: $(uptime)" >> "$out"
  echo "" >> "$out"
  if command -v hyprctl &>/dev/null; then
    echo "Hyprland monitors:" >> "$out"
    hyprctl monitors 2>/dev/null | grep -E "Monitor|scale|resolution" >> "$out" || true
  fi
  cat "$out"
  echo ""
}

# ---------- single run ----------
run_once() {
  local run_n="$1"
  local out="$RESULTS_DIR/${LABEL}-run${run_n}.txt"
  local bench_home
  bench_home="$(mktemp -d /tmp/serenity-bench-XXXXXX)"
  trap "rm -rf '$bench_home'" RETURN

  # Seed isolated config
  mkdir -p "$bench_home/.serenity"
  cat > "$bench_home/.serenity/config.conf" <<'CFG'
config.version = 1
ui.render.frame_timing = true
ui.render.latency_trace = true
editor.word_wrap = true
CFG

  local log_file=""

  echo "--- Run $run_n: load average before: $(uptime | awk -F'load average:' '{print $2}')" | tee "$out"

  # Launch Serenity
  java -Duser.home="$bench_home" -jar "$JAR" --gui "${RENDERER_FLAG[@]}" "$LOREM_FILE" \
    > "$bench_home/stdout.log" 2>&1 &
  local serenity_pid=$!
  echo "Launched PID $serenity_pid" | tee -a "$out"

  # Wait for initial render (up to 60s)
  local started=false
  local t0
  t0=$(date +%s)
  for i in $(seq 1 120); do
    sleep 0.5
    log_file="$(find_app_log "$bench_home" || true)"
    if [[ -n "$log_file" ]] && grep -q "Initial render completed" "$log_file" 2>/dev/null; then
      local t1
      t1=$(date +%s)
      echo "Initial render completed in $(( t1 - t0 ))s" | tee -a "$out"
      started=true
      break
    fi
    if ! kill -0 "$serenity_pid" 2>/dev/null; then
      echo "ERROR: Serenity exited before initial render" | tee -a "$out"
      cat "$bench_home/stdout.log" >> "$out" 2>/dev/null || true
      return 1
    fi
  done
  if ! $started; then
    echo "ERROR: timed out waiting for initial render" | tee -a "$out"
    kill "$serenity_pid" 2>/dev/null || true
    return 1
  fi

  # Find the named Serenity window — search by name to avoid XWayland root/internal Java windows
  local win_id=""
  for i in $(seq 1 30); do
    win_id=$(xdotool search --pid "$serenity_pid" --name "Serenity" 2>/dev/null | head -1 || true)
    [[ -n "$win_id" ]] && break
    sleep 0.5
  done
  if [[ -z "$win_id" ]]; then
    echo "ERROR: could not find Serenity window" | tee -a "$out"
    kill "$serenity_pid" 2>/dev/null || true
    return 1
  fi
  local win_hex
  win_hex=$(printf '0x%08x' "$win_id")
  echo "Window ID: $win_id ($win_hex $(xdotool getwindowname "$win_id" 2>/dev/null))" | tee -a "$out"

  # Raise and focus via wmctrl (more reliable under Hyprland/XWayland)
  wmctrl -i -a "$win_hex" 2>/dev/null || true
  sleep 0.5

  # Click the centre of the window to land keyboard focus in the text area
  local geom wx wy ww wh cx cy
  geom=$(xdotool getwindowgeometry "$win_id" 2>/dev/null || true)
  wx=$(echo "$geom" | awk '/Position/{split($2,a,","); print a[1]+0}')
  wy=$(echo "$geom" | awk '/Position/{split($2,a,","); print a[2]+0}')
  ww=$(echo "$geom" | awk '/Geometry/{split($2,a,"x"); print a[1]+0}')
  wh=$(echo "$geom" | awk '/Geometry/{split($2,a,"x"); print a[2]+0}')
  cx=$(( ${wx:-0} + ${ww:-800} / 2 ))
  cy=$(( ${wy:-0} + ${wh:-600} / 2 ))
  echo "  click at ($cx,$cy) geom ${ww}x${wh}+${wx}+${wy}" | tee -a "$out"
  xdotool mousemove "$cx" "$cy"
  sleep 0.1
  xdotool click 1
  sleep 0.5

  # 10s settle
  echo "Settling 10s..." | tee -a "$out"
  sleep 10

  # (a) 400 chars at ~10 chars/sec — type to active window, no --window flag
  echo "[$(date +%T)] Segment a: 400 chars at 10/s" | tee -a "$out"
  local typed400="Lorem ipsum dolor sit amet consectetur adipiscing elit sed do eiusmod tempor incididunt ut labore et dolore magna aliqua Ut enim ad minim veniam quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur Excepteur sint occaecat cupidatat non proident sunt in culpa qui officia deserunt mollit anim id est"
  xdotool type --clearmodifiers --delay 100 "$typed400"

  # (b) 200 chars burst
  echo "[$(date +%T)] Segment b: 200 chars burst" | tee -a "$out"
  local burst200="laboris nisi ut aliquip commodo consequat duis aute irure dolor in reprehenderit voluptate velit esse cillum dolore eu fugiat pariatur sint occaecat cupidatat"
  xdotool type --clearmodifiers --delay 5 "$burst200"

  # (c) 20x PageDown then 20x PageUp
  echo "[$(date +%T)] Segment c: 20x PgDn + 20x PgUp" | tee -a "$out"
  for i in $(seq 1 20); do xdotool key --clearmodifiers Next; sleep 0.1; done
  for i in $(seq 1 20); do xdotool key --clearmodifiers Prior; sleep 0.1; done

  # (d) 30 wheel scroll steps down
  echo "[$(date +%T)] Segment d: 30 wheel down" | tee -a "$out"
  for i in $(seq 1 30); do xdotool click 5; sleep 0.08; done

  # (e) 50x Down/Up arrows
  echo "[$(date +%T)] Segment e: 50x Down/Up" | tee -a "$out"
  for i in $(seq 1 50); do xdotool key --clearmodifiers Down; sleep 0.05; done
  for i in $(seq 1 50); do xdotool key --clearmodifiers Up; sleep 0.05; done

  # Wait for final FRAME window then quit
  echo "[$(date +%T)] Input done; waiting 8s for final FRAME window..." | tee -a "$out"
  sleep 8

  echo "[$(date +%T)] Quitting" | tee -a "$out"
  xdotool key --window "$win_id" ctrl+q 2>/dev/null || true
  sleep 2
  kill "$serenity_pid" 2>/dev/null || true
  wait "$serenity_pid" 2>/dev/null || true

  # Collect FRAME lines
  log_file="$(find_app_log "$bench_home" || echo /dev/null)"
  echo "" >> "$out"
  echo "=== [FRAME] lines ===" >> "$out"
  grep "\[FRAME\]" "$log_file" >> "$out" 2>/dev/null || echo "(none)" >> "$out"

  echo "" >> "$out"
  echo "=== [LATENCY] summaries ===" >> "$out"
  grep "\[LATENCY\] summary" "$log_file" >> "$out" 2>/dev/null || echo "(none)" >> "$out"
  grep "\[RENDER\]" "$log_file" >> "$out" 2>/dev/null || true

  echo "" >> "$out"
  echo "=== Startup log ===" >> "$out"
  grep -E "Initial render|ERROR|WARN|started|startup" "$log_file" 2>/dev/null | head -20 >> "$out" || true

  echo "Run $run_n complete -> $out"
}

# ---------- main ----------
echo "=== bench-laptop.sh: label=$LABEL jar=$JAR ==="
echo ""
record_conditions

for run in 1 2 3; do
  echo ""
  echo "=== Starting run $run/3 ==="
  run_once "$run" || echo "Run $run failed, continuing..."
  # Brief pause between runs to let system settle
  sleep "${BENCH_RUN_PAUSE:-5}"
done

echo ""
echo "=== Computing stats ==="
compute_stats "$RESULTS_DIR" "$LABEL"
