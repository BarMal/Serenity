#!/usr/bin/env bash
# Measures the memory a running Serenity actually holds, with the JVM flags of the packaged desktop app.
#
#   scripts/measure-footprint.sh [options]
#
#   --jar PATH        assembled jar (default: target/scala-*/Serenity.jar, from `sbt assembly`)
#   --mode gui|tui    windowed interface (default; started under xvfb-run when DISPLAY is unset) or terminal interface
#                     (started on a pseudo-terminal from util-linux `script`)
#   --label NAME      value of the `label` column (default: baseline)
#   --java-opt OPT    one more JVM option, after the shipped ones so it wins; repeatable. "-Xmx512m" over
#                     "-XX:MaxRAMPercentage=25", for instance
#   --no-shipped      start from no heap/GC flags at all instead of the shipped ones (for comparison)
#   --no-cds          leave out the AppCDS archive flags
#   --no-nmt          leave out -XX:NativeMemoryTracking=summary (to see what the tracking itself costs)
#   --idle-wait SECS  how long to sit idle before the last checkpoint (default 65: the shipped
#                     G1PeriodicGCInterval is 60000 ms, so the periodic collection has to have had time to fire)
#   --lines N         length of the plain-text fixture (default 15000)
#   --csv FILE        append the rows here (default: stdout); the header is written when the file is new or empty
#   --raw-dir DIR     keep every raw capture (smaps_rollup, GC.heap_info, NMT summary, top mappings by PSS)
#   --jcmd PATH       jcmd to use (default: the one beside the java on PATH)
#
# What it does
#   1. Primes the AppCDS archive once (a --smoke-test run, which writes it on exit), as the packaged app has one.
#   2. Starts the app with a private user.home so the user's own config and session are not touched, and waits until
#      its single-instance socket exists and its CPU use falls quiet (the startup warm-up types through an off-screen
#      editor, which is real work and must not be counted as idle).
#   3. Checkpoint `start`.  Opens a ~15k-line plain-text file and a markdown file, the way a file manager would, by
#      launching a second `Serenity.jar --open FILE` that the running instance receives; waits for quiet after each.
#      Checkpoints `open_text`, `open_markdown`.
#   4. Sits idle for --idle-wait seconds.  Checkpoint `idle`.
#
# Each checkpoint reads /proc/PID/smaps_rollup (RSS, PSS), `jcmd PID GC.heap_info` (heap used/committed, metaspace)
# and `jcmd PID VM.native_memory summary scale=KB` (non-heap by category). Rows go through scripts/footprint_parse.py;
# columns are documented there (cpu_s is the process's cumulative CPU time). `status` is `ok` unless the process died, ran out of heap, or never went quiet.
#
# Caveats
#   - NMT is on for every run so that the breakdown exists; it costs a little memory itself (the "Native Memory
#     Tracking" category) and is a measuring instrument, not a recommended flag.
#   - The first jcmd starts the JVM's attach listener thread, so `start` includes that small cost.
#   - RSS counts pages shared with other processes in full; PSS divides them among the processes sharing them. Quote PSS
#     when saying what the app costs the machine.
#   - MaxRAMPercentage scales with the machine's RAM, so heap numbers from this script are only comparable across
#     machines with the same RAM. The script prints the machine's RAM on stderr.
#   - JAVA_TOOL_OPTIONS and _JAVA_OPTIONS are unset for the app so a CI agent's proxy flags do not leak into it.
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
parser="$repo_root/scripts/footprint_parse.py"

original_args=("$@")
jar=""
mode="gui"
label="baseline"
extra_opts=()
shipped=1
cds=1
nmt=1
idle_wait=65
lines=15000
csv_out=""
raw_dir=""
jcmd_bin=""

while (($#)); do
  case "$1" in
    --jar) jar="$2"; shift 2 ;;
    --mode) mode="$2"; shift 2 ;;
    --label) label="$2"; shift 2 ;;
    --java-opt) extra_opts+=("$2"); shift 2 ;;
    --no-shipped) shipped=0; shift ;;
    --no-cds) cds=0; shift ;;
    --no-nmt) nmt=0; shift ;;
    --idle-wait) idle_wait="$2"; shift 2 ;;
    --lines) lines="$2"; shift 2 ;;
    --csv) csv_out="$2"; shift 2 ;;
    --raw-dir) raw_dir="$2"; shift 2 ;;
    --jcmd) jcmd_bin="$2"; shift 2 ;;
    -h | --help) sed -n '2,/^set -euo/p' "$0" | sed '$d; s/^# \{0,1\}//'; exit 0 ;;
    *) echo "Unknown option: $1" >&2; exit 2 ;;
  esac
done

case "$mode" in gui | tui) ;; *) echo "--mode must be gui or tui" >&2; exit 2 ;; esac

if [[ "$mode" == gui && -z "${DISPLAY:-}" ]]; then
  command -v xvfb-run >/dev/null || { echo "No DISPLAY and no xvfb-run to provide one." >&2; exit 1; }
  exec xvfb-run -a -s "-screen 0 1280x800x24" "$0" "${original_args[@]}"
fi

if [[ -z "$jar" ]]; then
  jar="$(ls -t "$repo_root"/target/scala-*/Serenity.jar 2>/dev/null | head -n 1 || true)"
fi
[[ -f "$jar" ]] || { echo "No jar found; run \`sbt assembly\` or pass --jar." >&2; exit 1; }
jar="$(cd "$(dirname "$jar")" && pwd)/$(basename "$jar")"

java_bin="$(command -v java)"
jcmd_bin="${jcmd_bin:-$(dirname "$(readlink -f "$java_bin")")/jcmd}"
[[ -x "$jcmd_bin" ]] || jcmd_bin="$(command -v jcmd)"

unset JAVA_TOOL_OPTIONS _JAVA_OPTIONS JDK_JAVA_OPTIONS

work="$(mktemp -d)"
home="$work/home"
fixtures="$work/fixtures"
mkdir -p "$home" "$fixtures"
run_id="footprint-$$-$RANDOM"
app_log="$work/app.log"
pid=""
holder_pid=""

cleanup() {
  if [[ -n "$pid" ]] && kill -0 "$pid" 2>/dev/null; then
    kill "$pid" 2>/dev/null || true
    for _ in $(seq 1 20); do kill -0 "$pid" 2>/dev/null || break; sleep 1; done
    kill -9 "$pid" 2>/dev/null || true
  fi
  [[ -n "$holder_pid" ]] && kill "$holder_pid" 2>/dev/null || true
  rm -rf "$work"
}
trap cleanup EXIT

say() { echo "[footprint:$label] $*" >&2; }

# ---------------------------------------------------------------- fixtures
awk -v n="$lines" 'BEGIN {
  srand(42)
  split("the quiet harbour lantern slowly drifted across a pale morning while she wrote letters nobody would read", w, " ")
  for (i = 1; i <= n; i++) {
    len = 4 + int(rand() * 14); line = ""
    for (j = 0; j < len; j++) line = line (j ? " " : "") w[1 + int(rand() * 17)]
    print line "."
  }
}' >"$fixtures/large.txt"

awk 'BEGIN {
  srand(7)
  print "# Footprint fixture\n"
  for (s = 1; s <= 40; s++) {
    print "## Section " s "\n"
    print "A paragraph with **bold**, *italic*, `code` and a [link](https://example.com/" s ") to follow.\n"
    for (i = 1; i <= 6; i++) print "- item " i " of section " s " with some words to wrap around the line"
    print "\n> A quotation for section " s ", long enough to need wrapping on a narrow pane of text.\n"
    print "```scala\nval answer = " s " * 2\n```\n"
  }
}' >"$fixtures/notes.md"

# ---------------------------------------------------------------- JVM flags
jvm_flags=()
if ((shipped)); then
  # Keep in step with .github/workflows/desktop-release.yml.
  jvm_flags+=(-XX:+UseG1GC -XX:MaxRAMPercentage=25 -XX:G1PeriodicGCInterval=60000)
fi
if ((cds)); then
  cds_dir="${XDG_CACHE_HOME:-$HOME/.cache}/serenity-footprint"
  mkdir -p "$cds_dir"
  archive="$cds_dir/$(basename "$jar" .jar)-$(stat -c '%Y-%s' "$jar").jsa"
  jvm_flags+=("-XX:SharedArchiveFile=$archive" -XX:+AutoCreateSharedArchive)
fi
((nmt)) && jvm_flags+=(-XX:NativeMemoryTracking=summary)
jvm_flags+=(${extra_opts[@]+"${extra_opts[@]}"})

if ((cds)) && [[ ! -f "$archive" ]]; then
  say "priming the AppCDS archive at $archive"
  java -XX:SharedArchiveFile="$archive" -XX:+AutoCreateSharedArchive -Duser.home="$work/prime-home" \
    -jar "$jar" --smoke-test --safe-mode >"$work/prime.log" 2>&1 ||
    say "AppCDS priming failed (continuing without a warm archive); see: $(tail -n 3 "$work/prime.log" | tr '\n' ' ')"
fi

say "RAM: $(awk '/MemTotal/ {printf "%d MiB", $2 / 1024}' /proc/meminfo), CPUs: $(nproc), JVM flags: ${jvm_flags[*]:-(none)}"

# ---------------------------------------------------------------- process helpers
clk_tck="$(getconf CLK_TCK)"

cpu_ticks() {
  # utime + stime, fields 14 and 15 of /proc/PID/stat; field 2 (comm) may contain spaces, so cut after its ")".
  local stat
  stat="$(<"/proc/$pid/stat")" || return 1
  stat="${stat##*) }"
  set -- $stat
  echo $((${12} + ${13}))
}

alive() { kill -0 "$pid" 2>/dev/null; }

# Quiet = under 3% of one core for three consecutive 2 s windows. Gives up after $1 seconds and says so.
wait_quiet() {
  local limit="$1" quiet=0 waited=0 before after
  before="$(cpu_ticks)" || return 1
  while ((quiet < 3)); do
    ((waited >= limit)) && return 2
    sleep 2
    waited=$((waited + 2))
    alive || return 1
    after="$(cpu_ticks)" || return 1
    if ((after - before <= clk_tck * 6 / 100)); then quiet=$((quiet + 1)); else quiet=0; fi
    before="$after"
  done
}

app_status() {
  if ! alive; then echo "died"
  elif grep -q "OutOfMemoryError" "$app_log" 2>/dev/null; then echo "oom"
  else echo "ok"; fi
}

top_mappings() {
  awk '/^[0-9a-f]+-[0-9a-f]+ / { name = ($6 == "" ? "[anon]" : $6) }
       /^Pss:/ { pss[name] += $2 }
       END { for (n in pss) printf "%8d kB  %s\n", pss[n], n }' "/proc/$pid/smaps" | sort -rn | head -n 25
}

if [[ -n "$csv_out" ]]; then
  [[ -s "$csv_out" ]] || python3 "$parser" header >"$csv_out"
else
  python3 "$parser" header
fi

capture() {
  local checkpoint="$1" status="$2" cap="$work/cap-$1"
  mkdir -p "$cap"
  cp "/proc/$pid/smaps_rollup" "$cap/smaps_rollup.txt" 2>/dev/null || : >"$cap/smaps_rollup.txt"
  [[ -n "$raw_dir" ]] && top_mappings >"$cap/top_mappings.txt" 2>/dev/null || true
  "$jcmd_bin" "$pid" GC.heap_info >"$cap/heap_info.txt" 2>&1 || : >"$cap/heap_info.txt"
  ((nmt)) && { "$jcmd_bin" "$pid" VM.native_memory summary scale=KB >"$cap/nmt.txt" 2>&1 || : >"$cap/nmt.txt"; }
  local uptime cpu_s ticks
  ticks="$(cpu_ticks 2>/dev/null || true)"
  cpu_s=""
  [[ -n "$ticks" ]] && cpu_s="$(awk -v t="$ticks" -v h="$clk_tck" 'BEGIN { printf "%.2f", t / h }')"
  uptime="$(ps -o etimes= -p "$pid" 2>/dev/null | tr -d ' ' || true)"
  local row
  row="$(python3 "$parser" row "$label" "$mode" "$checkpoint" "${uptime:-0}" "$cpu_s" "$status" \
    "$cap/smaps_rollup.txt" "$cap/heap_info.txt" "$cap/nmt.txt")"
  if [[ -n "$csv_out" ]]; then echo "$row" >>"$csv_out"; else echo "$row"; fi
  if [[ -n "$raw_dir" ]]; then
    mkdir -p "$raw_dir/$label"
    for f in "$cap"/*; do cp "$f" "$raw_dir/$label/$checkpoint.$(basename "$f")"; done
  fi
  say "$checkpoint: $status; RSS/PSS $(sed -n 's/^Rss: *\([0-9]*\).*/\1/p' "$cap/smaps_rollup.txt")/$(sed -n 's/^Pss: *\([0-9]*\).*/\1/p' "$cap/smaps_rollup.txt") kB"
}

open_in_running_app() {
  local file="$1"
  java -Xshare:auto -XX:TieredStopAtLevel=1 -Xmx128m -Duser.home="$home" -Dserenity.log.dir="$work/forward-logs" -jar "$jar" "--$mode" --open "$file" \
    >>"$work/forward.log" 2>&1 </dev/null
}

# ---------------------------------------------------------------- launch
app_cmd=(java "${jvm_flags[@]}" -Duser.home="$home" -Dserenity.log.dir="$work/logs" -Dserenity.footprint.run="$run_id" -jar "$jar" "--$mode")

if [[ "$mode" == gui ]]; then
  "${app_cmd[@]}" >"$app_log" 2>&1 </dev/null &
  pid=$!
else
  # A pty for the terminal interface, held open by a process that never writes.
  mkfifo "$work/stdin.fifo"
  sleep 100000 >"$work/stdin.fifo" &
  holder_pid=$!
  script -qfec "stty cols 120 rows 40; exec ${app_cmd[*]}" /dev/null <"$work/stdin.fifo" >"$app_log" 2>&1 &
  for _ in $(seq 1 50); do
    pid="$(pgrep -f "^([^ ]*/)?java .*-Dserenity.footprint.run=$run_id( |\$)" | head -n 1 || true)"
    [[ -n "$pid" ]] && break
    sleep 0.2
  done
  [[ -n "$pid" ]] || { echo "Could not find the app's JVM." >&2; exit 1; }
fi

socket="$home/.serenity/instance.sock"
for _ in $(seq 1 120); do
  [[ -S "$socket" ]] && break
  alive || { echo "The app exited during startup:" >&2; tail -n 20 "$app_log" >&2; exit 1; }
  sleep 1
done
[[ -S "$socket" ]] || { echo "The app never opened its instance socket." >&2; exit 1; }

quiet_status() {
  local rc=0
  wait_quiet 180 || rc=$?
  case "$rc" in
    0) app_status ;;
    2) [[ "$(app_status)" == ok ]] && echo "not_idle" || app_status ;;
    *) echo "died" ;;
  esac
}

# ---------------------------------------------------------------- checkpoints
status="$(quiet_status)"
capture start "$status"

open_in_running_app "$fixtures/large.txt"
status="$(quiet_status)"
capture open_text "$status"

open_in_running_app "$fixtures/notes.md"
status="$(quiet_status)"
capture open_markdown "$status"

sleep "$idle_wait"
capture idle "$(app_status)"

if [[ -n "$raw_dir" ]]; then
  mkdir -p "$raw_dir/$label"
  cp -r "$work/logs" "$raw_dir/$label/logs" 2>/dev/null || true
  cp "$app_log" "$raw_dir/$label/stdout.log" 2>/dev/null || true
  if [[ "$mode" == gui ]] && command -v import >/dev/null; then
    import -window root "$raw_dir/$label/final.png" 2>/dev/null || true
  fi
fi
if grep -qE "^Exception in thread|OutOfMemoryError" "$app_log"; then
  say "the app logged errors:"
  grep -E "^Exception in thread|OutOfMemoryError" "$app_log" | head -n 5 >&2
fi
if compgen -G "$work/hs_err_pid*.log" >/dev/null || compgen -G "./hs_err_pid*.log" >/dev/null; then
  say "the JVM left a crash log"
fi
