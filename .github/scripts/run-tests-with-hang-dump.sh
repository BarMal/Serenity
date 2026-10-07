#!/usr/bin/env bash
# Runs the given test command; if it is still running after HANG_MINUTES (default 40), prints a thread dump of its JVMs,
# stops it and exits 124. A normal suite takes 8-15 minutes, and a hang otherwise holds a runner until the 6-hour job
# limit while telling us nothing about where it stuck. Only the command's own process tree is dumped and stopped.
set -uo pipefail

limit_seconds="${HANG_SECONDS:-$(( ${HANG_MINUTES:-40} * 60 ))}"
hang_marker="$(mktemp)"
rm -f "$hang_marker"

descendants() {
  local child
  for child in $(pgrep -P "$1" 2>/dev/null); do
    echo "$child"
    descendants "$child"
  done
}

"$@" &
test_pid=$!

(
  sleep "$limit_seconds"
  if kill -0 "$test_pid" 2>/dev/null; then
    touch "$hang_marker"
    echo "::error::Tests still running after ${limit_seconds}s; dumping thread stacks and stopping."
    tree="$test_pid $(descendants "$test_pid")"
    dumped=0
    for pid in $tree; do
      if jcmd "$pid" VM.version >/dev/null 2>&1; then
        echo "::group::Thread dump of JVM ${pid}"
        jcmd "$pid" Thread.print -l 2>&1
        echo "::endgroup::"
        dumped=1
      fi
    done
    # Without pgrep (Git Bash on Windows) the tree is just the launcher; dump every JVM instead. Dumping is read-only.
    if [[ "$dumped" == 0 ]]; then
      for pid in $(jps -q 2>/dev/null); do
        echo "::group::Thread dump of JVM ${pid}"
        jcmd "$pid" Thread.print -l 2>&1 || true
        echo "::endgroup::"
      done
    fi
    # Children first, so the launcher cannot respawn or outlive its JVM.
    for pid in $(printf '%s\n' $tree | awk '{ p[NR] = $0 } END { for (i = NR; i > 0; i--) print p[i] }'); do kill "$pid" 2>/dev/null || true; done
  fi
) &
watchdog_pid=$!

wait "$test_pid"
status=$?
kill "$watchdog_pid" 2>/dev/null || true
if [[ -e "$hang_marker" ]]; then
  rm -f "$hang_marker"
  exit 124
fi
exit "$status"
