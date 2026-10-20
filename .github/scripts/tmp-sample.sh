#!/usr/bin/env bash
# TEMP profiling sampler for the macOS column-paging investigation.
mkdir -p jvm-diag
for i in $(seq 1 400); do
  sleep 6
  for pid in $(pgrep -x java); do
    jcmd "$pid" Thread.print 2>/dev/null | awk '
      /ScalaTest-running/ { f = 1; top = ""; chain = ""; n = 0; next }
      f && /^[[:space:]]*at / {
        n++
        if (n == 1) top = $0
        if ($0 ~ /com\.serenity/ && ns < 7) { ns++; sub(/^[[:space:]]*at /, "", $0); chain = chain " < " $0 }
        next
      }
      f && /^$/ { print "TOP" top; print "CHAIN" chain; f = 0; ns = 0 }
    '
  done
done > jvm-diag/samples.txt
