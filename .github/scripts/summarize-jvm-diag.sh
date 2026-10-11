#!/usr/bin/env bash
# Prints a Markdown summary of the CI JVM diagnostics in DIR (default jvm-diag): the longest safepoint and GC pauses
# from gc-safepoint.log (-Xlog:gc*,safepoint with time,uptime,level,tags decorations) and the longest StallSampler wakes
# from stalls.log. Plain POSIX awk/sort so it runs on the Linux, macOS (BSD awk) and Windows (Git Bash) runners; it
# always exits 0 because a diagnostic must not fail the job.
set -uo pipefail

dir="${1:-jvm-diag}"
pause_log="$dir/gc-safepoint.log"
stall_log="$dir/stalls.log"
tab="$(printf '\t')"

echo "## JVM stall diagnostics"
echo

if [ -f "$pause_log" ]; then
  pauses="$(mktemp)"
  awk '
    function stamp(   end) { end = index($0, "]"); return substr($0, 2, end - 2) }
    /\[safepoint *\]/ && index($0, "Safepoint \"") && index($0, "Total: ") {
      rest = substr($0, index($0, "Safepoint \"") + 11)
      cause = substr(rest, 1, index(rest, "\"") - 1)
      total = substr($0, index($0, "Total: ") + 7)
      sub(/ ns.*/, "", total)
      printf "%.3f\tsafepoint\t%s\t%s\n", total / 1000000, cause, stamp()
    }
    /\[gc *\]/ && / Pause / && $NF ~ /ms$/ {
      for (i = 1; i <= NF && $i != "Pause"; i++) {}
      cause = ""
      for (j = i + 1; j < NF && index($j, "->") == 0; j++) cause = cause (cause == "" ? "" : " ") $j
      ms = $NF
      sub(/ms$/, "", ms)
      printf "%.3f\tgc\t%s\t%s\n", ms, cause, stamp()
    }
  ' "$pause_log" | sort -t "$tab" -k1,1nr > "$pauses"

  total="$(wc -l < "$pauses" | tr -d ' ')"
  over_100ms="$(awk -F '\t' '$1 > 100' "$pauses" | wc -l | tr -d ' ')"
  over_1s="$(awk -F '\t' '$1 > 1000' "$pauses" | wc -l | tr -d ' ')"
  echo "Safepoint and GC pauses: $total logged, $over_100ms over 100 ms, $over_1s over 1 s."
  echo
  echo "| Pause ms | Kind | Cause | Timestamp |"
  echo "|---:|---|---|---|"
  head -n 10 "$pauses" | awk -F '\t' '{ printf "| %s | %s | %s | %s |\n", $1, $2, $3, $4 }'
  rm -f "$pauses"
else
  echo "No $pause_log."
fi

echo

if [ -f "$stall_log" ]; then
  stalls="$(mktemp)"
  awk '
    !/^#/ && /late=/ {
      late = ""; uptime = ""
      for (i = 1; i <= NF; i++) {
        if ($i ~ /^late=/) { late = $i; sub(/^late=/, "", late); sub(/ms$/, "", late) }
        if ($i ~ /^uptime=/) uptime = substr($i, 8)
      }
      printf "%d\t%s\t%s\n", late, $1, uptime
    }
  ' "$stall_log" | sort -t "$tab" -k1,1nr > "$stalls"

  echo "StallSampler: $(wc -l < "$stalls" | tr -d ' ') wakes more than 250 ms late (its lines carry no stack frames; match the wake time to a test in the JUnit report)."
  echo
  echo "| Late ms | Woke | JVM uptime |"
  echo "|---:|---|---|"
  head -n 5 "$stalls" | awk -F '\t' '{ printf "| %s | %s | %s |\n", $1, $2, $3 }'
  rm -f "$stalls"
else
  echo "No $stall_log."
fi

exit 0
