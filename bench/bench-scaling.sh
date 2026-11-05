#!/usr/bin/env nix-shell
#!nix-shell -i bash -p xdotool wmctrl python3 xorg.xwininfo coreutils procps jdk

# bench-scaling.sh [--dry-run] <jar> <label> <a|b|c|d> [java2d|skia]
#
# How the running app scales with many open files and split panes (issue #2478), one mode a row of the harness's
# scaling rows; the harness gives the numbers CI records, this gives the laptop's, from the app's own [FRAME] and
# [LATENCY] lines (ui.render.frame_timing, ui.render.latency_trace), as bench-laptop.sh does.
#
#   a  tab switches: ctrl+Tab through 1 open file and through 20 (`open_1`, `open_20`)
#   b  typing, PageDown and the wheel with 1, 2 and 4 panes, a different file in each (`split1`, `split2`, `split4`)
#   c  heap per open file: the used heap after a GC with 1 file and with 20, each visited once
#   d  not measurable from the app's logs (see below)
#
# Not run against the app where it was written (no xdotool or desktop session there): the documents, the key plans and
# the argument handling are tested by scripts/test_bench_scaling.py, the driving is not. Check a run's first result file
# before trusting a table; --dry-run prints what each mode would do and needs neither a jar nor a display.
#
# Mode d (time to the last of four panes' scrollbars) has no line in the app's logs: the late count ends in a repaint of
# the bar strip, which neither [FRAME] nor [LATENCY] names. The harness row `scrollbar.four_panes.all_bars` measures it.
# Mode c's shape-cache half is not in the logs either: run `sbt "Test / runMain com.serenity.perf.ScalingShapeCacheBenchmarks"`.
#
# Runs 3 times per configuration (BENCH_RUNS), medians across runs. Results: bench-results/<label>-<mode>-<config>-runN.txt.

set -euo pipefail

BENCH_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=bench-lib.sh
source "$BENCH_DIR/bench-lib.sh"
# shellcheck source=bench-scaling-lib.sh
source "$BENCH_DIR/bench-scaling-lib.sh"

usage() { echo "Usage: bench-scaling.sh [--dry-run] <jar> <label> <a|b|c|d> [java2d|skia]" >&2; }

DRY_RUN=false
if [[ "${1:-}" == "--dry-run" ]]; then DRY_RUN=true; shift; fi
JAR="${1:-}"
LABEL="${2:-}"
MODE="${3:-}"
BACKEND="${4:-}"
case "$MODE" in
  a | b | c | d) ;;
  *) usage; exit 2 ;;
esac
case "$BACKEND" in
  "" | java2d | skia) ;;
  *) echo "ERROR: backend must be java2d or skia, not: $BACKEND" >&2; exit 2 ;;
esac
RENDERER_FLAG=()
if [[ -n "$BACKEND" ]]; then RENDERER_FLAG=("--renderer=$BACKEND"); fi
RUNS="${BENCH_RUNS:-3}"
RESULTS_DIR="$BENCH_DIR/bench-results"
FILE_KIND="${BENCH_FILE_KIND:-prose}"

describe() {
  case "$MODE" in
    a) echo "mode a: ctrl+Tab through open_1 (1 file) and open_20 (20 files, 19 switches 0.8 s apart); $RUNS runs each, $FILE_KIND files" ;;
    b)
      echo "mode b: typing, PageDown and the wheel with split1, split2 and split4 (4 files open, a different one in each pane); $RUNS runs each, $FILE_KIND files"
      for panes in 2 4; do
        echo "  split$panes plan:"
        scaling_split_plan "$panes" | sed 's/^/    /'
      done
      ;;
    c) echo "mode c: heap per open file: used heap after a GC with 1 file and with 20, each visited once; $RUNS runs each, $FILE_KIND files" ;;
    d) echo "mode d: time to the last of four scrollbars is not measurable from the app's logs; the harness row scrollbar.four_panes.all_bars measures it" ;;
  esac
}

if $DRY_RUN; then describe; exit 0; fi
if [[ "$MODE" == "d" ]]; then describe; exit 0; fi
if [[ ! -f "$JAR" ]]; then echo "ERROR: jar not found: $JAR" >&2; exit 1; fi
mkdir -p "$RESULTS_DIR"

# ---------- one launch ----------
# run_once <config tag> <file count> <segment function> <run number>
run_once() {
  local tag="$1" count="$2" segment="$3" run_n="$4"
  local out="$RESULTS_DIR/${LABEL}-${MODE}-${tag}-run${run_n}.txt"
  local bench_home docs
  bench_home="$(mktemp -d /tmp/serenity-bench-XXXXXX)"
  docs="$bench_home/docs"
  trap "rm -rf '$bench_home'" RETURN
  python3 "$BENCH_DIR/gen-scaling-docs.py" "$docs" "$count" "$FILE_KIND" > /dev/null
  mkdir -p "$bench_home/.serenity"
  cat > "$bench_home/.serenity/config.conf" <<'CFG'
config.version = 1
ui.render.frame_timing = true
ui.render.latency_trace = true
editor.word_wrap = true
CFG
  echo "--- $tag run $run_n: load average before: $(uptime | awk -F'load average:' '{print $2}')" | tee "$out"
  local files=()
  while IFS= read -r file; do files+=("$file"); done < <(find "$docs" -type f | sort)
  java -Duser.home="$bench_home" -jar "$JAR" --gui "${RENDERER_FLAG[@]}" "${files[@]}" > "$bench_home/stdout.log" 2>&1 &
  local pid=$!

  local started=false log_file=""
  for _ in $(seq 1 240); do
    sleep 0.5
    log_file="$(find_app_log "$bench_home" || true)"
    if [[ -n "$log_file" ]] && grep -q "Initial render completed" "$log_file" 2>/dev/null; then started=true; break; fi
    if ! kill -0 "$pid" 2>/dev/null; then echo "ERROR: Serenity exited before initial render" | tee -a "$out"; return 1; fi
  done
  if ! $started; then echo "ERROR: timed out waiting for initial render" | tee -a "$out"; kill "$pid" 2>/dev/null || true; return 1; fi

  local win_id=""
  for _ in $(seq 1 30); do
    win_id=$(xdotool search --pid "$pid" --name "Serenity" 2>/dev/null | head -1 || true)
    [[ -n "$win_id" ]] && break
    sleep 0.5
  done
  if [[ -z "$win_id" ]]; then echo "ERROR: could not find Serenity window" | tee -a "$out"; kill "$pid" 2>/dev/null || true; return 1; fi
  wmctrl -i -a "$(printf '0x%08x' "$win_id")" 2>/dev/null || true
  sleep 0.5
  WIN_GEOMETRY="$(xdotool getwindowgeometry "$win_id" 2>/dev/null || true)"
  click_quarter "right-bottom"   # lands keyboard focus in the text area, as bench-laptop.sh's centre click does
  sleep 10

  "$segment" "$pid" "$out"

  sleep 8
  xdotool key --window "$win_id" ctrl+q 2>/dev/null || true
  sleep 2
  kill "$pid" 2>/dev/null || true
  wait "$pid" 2>/dev/null || true

  log_file="$(find_app_log "$bench_home" || echo /dev/null)"
  { echo ""; echo "=== [FRAME] lines ==="; grep "\[FRAME\]" "$log_file" 2>/dev/null || echo "(none)"
    echo ""; echo "=== [LATENCY] summaries ==="; grep "\[LATENCY\] summary" "$log_file" 2>/dev/null || echo "(none)"; } >> "$out"
  echo "run $run_n complete -> $out"
}

# click_quarter <left-top|right-top|left-bottom|right-bottom>: a click on that quarter of the window.
click_quarter() {
  local wx wy ww wh
  wx=$(echo "$WIN_GEOMETRY" | awk '/Position/{split($2,a,","); print a[1]+0}')
  wy=$(echo "$WIN_GEOMETRY" | awk '/Position/{split($2,a,","); print a[2]+0}')
  ww=$(echo "$WIN_GEOMETRY" | awk '/Geometry/{split($2,a,"x"); print a[1]+0}')
  wh=$(echo "$WIN_GEOMETRY" | awk '/Geometry/{split($2,a,"x"); print a[2]+0}')
  local x=$(( ${wx:-0} + ${ww:-800} / 4 )) y=$(( ${wy:-0} + ${wh:-600} / 4 ))
  case "$1" in right-*) x=$(( ${wx:-0} + 3 * ${ww:-800} / 4 )) ;; esac
  case "$1" in *-bottom) y=$(( ${wy:-0} + 3 * ${wh:-600} / 4 )) ;; esac
  xdotool mousemove "$x" "$y"
  sleep 0.1
  xdotool click 1
  sleep 0.5
}

# ---------- segments ----------
switch_tabs() { # the 19 switches of open_20; open_1 has none
  local pid="$1" out="$2" files="${SEGMENT_FILES:-1}"
  echo "[$(date +%T)] switching through $files file(s)" | tee -a "$out"
  for _ in $(seq 1 $(( files - 1 ))); do xdotool key --clearmodifiers ctrl+Tab; sleep 0.8; done
}

split_then_edit() { # the split plan, then typing, PageDown/PageUp and the wheel in the focused pane
  local pid="$1" out="$2" step action argument
  while IFS= read -r step; do
    action="${step%% *}"; argument="${step#* }"
    case "$action" in
      key) xdotool key --clearmodifiers "$argument"; sleep 0.8 ;;
      click) click_quarter "$argument" ;;
    esac
  done < <(scaling_split_plan "$PANES")
  echo "[$(date +%T)] typing, 10x PgDn/PgUp, 20 wheel steps with $PANES pane(s)" | tee -a "$out"
  xdotool type --clearmodifiers --delay 100 "the quick brown fox jumps over the lazy dog and keeps on typing for a while"
  for _ in $(seq 1 10); do xdotool key --clearmodifiers Next; sleep 0.1; done
  for _ in $(seq 1 10); do xdotool key --clearmodifiers Prior; sleep 0.1; done
  for _ in $(seq 1 20); do xdotool click 5; sleep 0.08; done
}

visit_then_measure_heap() { # every file once, then the heap in use after a collection
  local pid="$1" out="$2"
  switch_tabs "$pid" "$out"
  jcmd "$pid" GC.run > /dev/null 2>&1 || true
  local used
  used="$(jcmd "$pid" GC.heap_info 2>/dev/null | grep -oE 'used [0-9]+K' | head -1 | grep -oE '[0-9]+' || true)"
  echo "heap_used_kb=${used:-unknown} files=${SEGMENT_FILES:-1}" | tee -a "$out"
}

# ---------- modes ----------
configs() { # tag file-count, one a line
  case "$MODE" in
    a | c) echo "open_1 1"; echo "open_20 20" ;;
    b) echo "split1 4"; echo "split2 4"; echo "split4 4" ;;
  esac
}

segment_for() {
  case "$MODE" in a) echo switch_tabs ;; b) echo split_then_edit ;; c) echo visit_then_measure_heap ;; esac
}

echo "=== bench-scaling.sh: label=$LABEL mode=$MODE kind=$FILE_KIND jar=$JAR ==="
echo "AC online: $(ac_online); load: $(uptime)"
while read -r tag count; do
  for run in $(seq 1 "$RUNS"); do
    export SEGMENT_FILES="$count"
    PANES="${tag#split}"
    export PANES
    run_once "$tag" "$count" "$(segment_for)" "$run" || echo "run $run of $tag failed, continuing..."
    sleep "${BENCH_RUN_PAUSE:-5}"
  done
  if [[ "$MODE" != "c" ]]; then
    compute_stats "$RESULTS_DIR" "${LABEL}-${MODE}-${tag}" || true
  fi
done < <(configs)

if [[ "$MODE" == "c" ]]; then
  python3 - "$RESULTS_DIR" "$LABEL" <<'PYEOF'
import glob, re, statistics, sys
results_dir, label = sys.argv[1], sys.argv[2]
def used(tag):
    values = []
    for path in sorted(glob.glob(f"{results_dir}/{label}-c-{tag}-run*.txt")):
        m = re.search(r"heap_used_kb=(\d+)", open(path).read())
        if m:
            values.append(int(m.group(1)))
    return statistics.median(values) if values else None
one, twenty = used("open_1"), used("open_20")
print(f"heap in use after a GC: 1 file {one} KB, 20 files {twenty} KB")
if one is not None and twenty is not None:
    print(f"per open file: {(twenty - one) / 19:.0f} KB")
PYEOF
fi
