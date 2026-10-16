#!/usr/bin/env bash
# Usage: sbt-slot.sh <sbt args...>  — runs sbt holding one of 3 machine-wide slots, capped heap.
# Waits (never times out) until a slot is free, so parallel agents don't OOM the 15 GB box.
set -uo pipefail
export PATH=$HOME/tools/sbt/bin:$PATH
export SBT_OPTS="${SBT_OPTS:--Xmx3g -Xss4m}"
LOCKDIR=/tmp/sbt-slots; mkdir -p "$LOCKDIR"
while true; do
  for i in 1 2 3; do
    exec 9>"$LOCKDIR/slot$i"
    if flock -n 9; then
      echo "[sbt-slot] holding slot $i" >&2
      sbt "$@"; rc=$?
      flock -u 9; exit $rc
    fi
  done
  sleep 15
done
