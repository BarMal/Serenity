#!/usr/bin/env bash
# TEMP: aggregate the JFR execution samples of the test threads.
ls -la jvm-diag/ || true
jfr print --events jdk.ExecutionSample --stack-depth 40 jvm-diag/rec.jfr > jvm-diag/samples.txt 2>/dev/null || { echo "no jfr"; exit 0; }
awk '
  /jdk.ExecutionSample/ { inEvent = 1; test = 0; n = 0; top = ""; ser = ""; chain = ""; ns = 0; next }
  inEvent && /sampledThread/ { test = ($0 ~ /ScalaTest-running/); next }
  inEvent && /^[[:space:]]+[a-zA-Z_$].*line:/ {
    n++
    line = $0; sub(/^[[:space:]]+/, "", line); sub(/\(.*/, "", line)
    if (n == 1) top = line
    if (line ~ /^com\.serenity/ && ns < 6) { ns++; if (ns == 1) ser = line; chain = chain " < " line }
    next
  }
  inEvent && /^}/ { if (test) { print "TOP " top; print "SER " ser; print "CHAIN" chain }; inEvent = 0 }
' jvm-diag/samples.txt > jvm-diag/agg.txt
echo "test-thread samples: $(grep -c '^TOP' jvm-diag/agg.txt)"
echo "--- top frames";  grep '^TOP' jvm-diag/agg.txt | sort | uniq -c | sort -rn | head -14
echo "--- first serenity frame"; grep '^SER' jvm-diag/agg.txt | sort | uniq -c | sort -rn | head -14
echo "--- chains"; grep '^CHAIN' jvm-diag/agg.txt | sort | uniq -c | sort -rn | head -10
