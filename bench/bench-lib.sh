#!/usr/bin/env bash
# Helpers bench-laptop.sh sources. They live apart so scripts/test_bench_laptop.py can source them too.

# The app's log under a home directory: it writes to $HOME/.local/state/serenity now and used to write to
# $HOME/.serenity. Prints the first that exists, and fails when neither does.
find_app_log() {
  local home="$1" candidate
  for candidate in "$home/.local/state/serenity/serenity.log" "$home/.serenity/serenity.log"; do
    if [[ -f "$candidate" ]]; then echo "$candidate"; return 0; fi
  done
  return 1
}

# Whether the machine is on AC power, from whichever AC* supply reports it (AC, AC0, ACAD, ...).
ac_online() {
  local dir="${BENCH_POWER_SUPPLY_DIR:-/sys/class/power_supply}" supply
  for supply in "$dir"/AC*/online; do
    if [[ -r "$supply" ]]; then cat "$supply"; return 0; fi
  done
  echo unknown
}

# How many of <label>-run*.txt in <results dir> kept a [FRAME] timing line. A run that timed out has none.
runs_with_data() {
  local dir="$1" label="$2" count=0 file
  for file in "$dir/$label"-run*.txt; do
    if [[ -f "$file" ]] && grep -qE "\[FRAME\].*p50=" "$file"; then count=$((count + 1)); fi
  done
  echo "$count"
}

# compute_stats <results dir> <label>: p50/p95 per phase, median across runs. Fails when no run produced data, so a
# batch of failed launches cannot pass for an empty table.
compute_stats() {
  local results_dir="$1" label="$2"
  if [[ "$(runs_with_data "$results_dir" "$label")" == 0 ]]; then
    echo "ERROR: No run produced data for $label (no [FRAME] timing lines in $results_dir/$label-run*.txt)" >&2
    return 1
  fi
python3 - "$results_dir" "$label" <<'PYEOF'
import sys, re, os
from statistics import median

results_dir, label = sys.argv[1], sys.argv[2]
phases = ["input-to-paint", "render", "input-queue", "input-apply", "render-wait", "paint"]

all_runs = {}
for n in (1, 2, 3):
    path = os.path.join(results_dir, f"{label}-run{n}.txt")
    if not os.path.exists(path):
        continue
    with open(path) as f:
        content = f.read()
    frame_lines = [l for l in content.splitlines() if "[FRAME]" in l]
    run_data = {}
    for line in frame_lines:
        for phase in phases:
            m = re.search(rf"{phase} p50=(\d+\.\d+) p95=(\d+\.\d+)", line)
            if m:
                run_data.setdefault(phase, {"p50": [], "p95": []})
                run_data[phase]["p50"].append(float(m.group(1)))
                run_data[phase]["p95"].append(float(m.group(2)))
        m = re.search(r"full=(\d+) \(([0-9.]+)/s\)", line)
        if m:
            run_data.setdefault("fps_full", []).append(float(m.group(2)))
        m = re.search(r"cursor=(\d+) \(([0-9.]+)/s\)", line)
        if m:
            run_data.setdefault("fps_cursor", []).append(float(m.group(2)))
    all_runs[n] = run_data
    print(f"  Run {n}: {len(frame_lines)} [FRAME] windows")

if not all_runs:
    print("No run data found.")
    sys.exit(1)

out_path = os.path.join(results_dir, f"{label}-summary.txt")
with open(out_path, "w") as f:
    def w(s): print(s); f.write(s + "\n")
    w(f"\n=== Benchmark summary: {label} ({len(all_runs)} runs) ===")
    w(f"{'Phase':<20} {'p50 median':>12} {'p95 median':>12}  (ms, median of runs)")
    w("-" * 48)
    for phase in phases:
        p50s = [median(all_runs[r][phase]["p50"]) for r in all_runs if phase in all_runs[r]]
        p95s = [median(all_runs[r][phase]["p95"]) for r in all_runs if phase in all_runs[r]]
        if p50s:
            w(f"{phase:<20} {median(p50s):>12.2f} {median(p95s):>12.2f}")
    w("")
    fps_fulls = [median(all_runs[r]["fps_full"]) for r in all_runs if "fps_full" in all_runs[r]]
    fps_cursors = [median(all_runs[r]["fps_cursor"]) for r in all_runs if "fps_cursor" in all_runs[r]]
    if fps_fulls: w(f"full-frame fps (median): {median(fps_fulls):.1f}")
    if fps_cursors: w(f"cursor-only fps (median): {median(fps_cursors):.1f}")
    w(f"\nFull results: {out_path}")
PYEOF
}
